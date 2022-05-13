/*
 * git-imap-send - drops patches into an imap Drafts folder
 *                 derived from isync/mbsync - mailbox synchronizer
 *
 * Copyright (C) 2000-2002 Michael R. Elkins <me@mutt.org>
 * Copyright (C) 2002-2004 Oswald Buddenhagen <ossi@users.sf.net>
 * Copyright (C) 2004 Theodore Y. Ts'o <tytso@mit.edu>
 * Copyright (C) 2006 Mike McCormack
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include "cache.h"
#include "exec_cmd.h"
#include "run-command.h"
#include "prompt.h"
#ifdef NO_OPENSSL
typedef void *SSL;
#endif

static const char imap_send_usage[] = "git imap-send < <mbox>";

#undef DRV_OK
#define DRV_OK          0
#define DRV_MSG_BAD     -1
#define DRV_BOX_BAD     -2
#define DRV_STORE_BAD   -3

static int Verbose, Quiet;

__attribute__((format (printf, 1, 2)))
static void imap_info(const char *, ...);
__attribute__((format (printf, 1, 2)))
static void imap_warn(const char *, ...);

static char *next_arg(char **);

__attribute__((format (printf, 3, 4)))
static int nfsnprintf(char *buf, int blen, const char *fmt, ...);

static int nfvasprintf(char **strp, const char *fmt, va_list ap)
{
	int len;
	char tmp[8192];

	len = vsnprintf(tmp, sizeof(tmp), fmt, ap);
	if (len < 0)
		die("Fatal: Out of memory");
	if (len >= sizeof(tmp))
		die("imap command overflow!");
	*strp = xmemdupz(tmp, len);
	return len;
}

struct imap_server_conf {
	char *name;
	char *tunnel;
	char *host;
	int port;
	char *user;
	char *pass;
	int use_ssl;
	int ssl_verify;
	int use_html;
	char *auth_method;
};

static struct imap_server_conf server = {
	NULL,	/* name */
	NULL,	/* tunnel */
	NULL,	/* host */
	0,	/* port */
	NULL,	/* user */
	NULL,	/* pass */
	0,   	/* use_ssl */
	1,   	/* ssl_verify */
	0,   	/* use_html */
	NULL,	/* auth_method */
};

struct imap_socket {
	int fd[2];
	SSL *ssl;
};

struct imap_buffer {
	struct imap_socket sock;
	int bytes;
	int offset;
	char buf[1024];
};

struct imap_cmd;

struct imap {
	int uidnext; /* from SELECT responses */
	unsigned caps, rcaps; /* CAPABILITY results */
	/* command queue */
	int nexttag, num_in_progress, literal_pending;
	struct imap_cmd *in_progress, **in_progress_append;
	struct imap_buffer buf; /* this is BIG, so put it last */
};

struct imap_store {
	/* currently open mailbox */
	const char *name; /* foreign! maybe preset? */
	int uidvalidity;
	struct imap *imap;
	const char *prefix;
};

struct imap_cmd_cb {
	int (*cont)(struct imap_store *ctx, struct imap_cmd *cmd, const char *prompt);
	void (*done)(struct imap_store *ctx, struct imap_cmd *cmd, int response);
	void *ctx;
	char *data;
	int dlen;
	int uid;
	unsigned create:1, trycreate:1;
};

struct imap_cmd {
	struct imap_cmd *next;
	struct imap_cmd_cb cb;
	char *cmd;
	int tag;
};

#define CAP(cap) (imap->caps & (1 << (cap)))

enum CAPABILITY {
	NOLOGIN = 0,
	UIDPLUS,
	LITERALPLUS,
	NAMESPACE,
	STARTTLS,
	AUTH_CRAM_MD5
};

static const char *cap_list[] = {
	"LOGINDISABLED",
	"UIDPLUS",
	"LITERAL+",
	"NAMESPACE",
	"STARTTLS",
	"AUTH=CRAM-MD5",
};

#define RESP_OK    0
#define RESP_NO    1
#define RESP_BAD   2

static int get_cmd_result(struct imap_store *ctx, struct imap_cmd *tcmd);


#ifndef NO_OPENSSL
static void ssl_socket_perror(const char *func)
{
	fprintf(stderr, "%s: %s\n", func, ERR_error_string(ERR_get_error(), NULL));
}
#endif

static void socket_perror(const char *func, struct imap_socket *sock, int ret)
{
#ifndef NO_OPENSSL
	if (sock->ssl) {
		int sslerr = SSL_get_error(sock->ssl, ret);
		switch (sslerr) {
		case SSL_ERROR_NONE:
			break;
		case SSL_ERROR_SYSCALL:
			perror("SSL_connect");
			break;
		default:
			ssl_socket_perror("SSL_connect");
			break;
		}
	} else
#endif
	{
		if (ret < 0)
			perror(func);
		else
			fprintf(stderr, "%s: unexpected EOF\n", func);
	}
}

#ifdef NO_OPENSSL
static int ssl_socket_connect(struct imap_socket *sock, int use_tls_only, int verify)
{
	fprintf(stderr, "SSL requested but SSL support not compiled in\n");
	return -1;
}

#else

static int host_matches(const char *host, const char *pattern)
{
	if (pattern[0] == '*' && pattern[1] == '.') {
		pattern += 2;
		if (!(host = strchr(host, '.')))
			return 0;
		host++;
	}

	return *host && *pattern && !strcasecmp(host, pattern);
}

static int verify_hostname(X509 *cert, const char *hostname)
{
	int len;
	X509_NAME *subj;
	char cname[1000];
	int i, found;
	STACK_OF(GENERAL_NAME) *subj_alt_names;

	/* try the DNS subjectAltNames */
	found = 0;
	if ((subj_alt_names = X509_get_ext_d2i(cert, NID_subject_alt_name, NULL, NULL))) {
		int num_subj_alt_names = sk_GENERAL_NAME_num(subj_alt_names);
		for (i = 0; !found && i < num_subj_alt_names; i++) {
			GENERAL_NAME *subj_alt_name = sk_GENERAL_NAME_value(subj_alt_names, i);
			if (subj_alt_name->type == GEN_DNS &&
			    strlen((const char *)subj_alt_name->d.ia5->data) == (size_t)subj_alt_name->d.ia5->length &&
			    host_matches(hostname, (const char *)(subj_alt_name->d.ia5->data)))
				found = 1;
		}
		sk_GENERAL_NAME_pop_free(subj_alt_names, GENERAL_NAME_free);
	}
	if (found)
		return 0;

	/* try the common name */
	if (!(subj = X509_get_subject_name(cert)))
		return error("cannot get certificate subject");
	if ((len = X509_NAME_get_text_by_NID(subj, NID_commonName, cname, sizeof(cname))) < 0)
		return error("cannot get certificate common name");
	if (strlen(cname) == (size_t)len && host_matches(hostname, cname))
		return 0;
	return error("certificate owner '%s' does not match hostname '%s'",
		     cname, hostname);
}

static int ssl_socket_connect(struct imap_socket *sock, int use_tls_only, int verify)
{
#if (OPENSSL_VERSION_NUMBER >= 0x10000000L)
	const SSL_METHOD *meth;
#else
	SSL_METHOD *meth;
#endif
	SSL_CTX *ctx;
	int ret;
	X509 *cert;

	SSL_library_init();
	SSL_load_error_strings();

	if (use_tls_only)
		meth = TLSv1_method();
	else
		meth = SSLv23_method();

	if (!meth) {
		ssl_socket_perror("SSLv23_method");
		return -1;
	}

	ctx = SSL_CTX_new(meth);

	if (verify)
		SSL_CTX_set_verify(ctx, SSL_VERIFY_PEER, NULL);

	if (!SSL_CTX_set_default_verify_paths(ctx)) {
		ssl_socket_perror("SSL_CTX_set_default_verify_paths");
		return -1;
	}
	sock->ssl = SSL_new(ctx);
	if (!sock->ssl) {
		ssl_socket_perror("SSL_new");
		return -1;
	}
	if (!SSL_set_rfd(sock->ssl, sock->fd[0])) {
		ssl_socket_perror("SSL_set_rfd");
		return -1;
	}
	if (!SSL_set_wfd(sock->ssl, sock->fd[1])) {
		ssl_socket_perror("SSL_set_wfd");
		return -1;
	}

#ifdef SSL_CTRL_SET_TLSEXT_HOSTNAME
	/*
	 * SNI (RFC4366)
	 * OpenSSL does not document this function, but the implementation
	 * returns 1 on success, 0 on failure after calling SSLerr().
	 */
	ret = SSL_set_tlsext_host_name(sock->ssl, server.host);
	if (ret != 1)
		warning("SSL_set_tlsext_host_name(%s) failed.", server.host);
#endif

	ret = SSL_connect(sock->ssl);
	if (ret <= 0) {
		socket_perror("SSL_connect", sock, ret);
		return -1;
	}

	if (verify) {
		/* make sure the hostname matches that of the certificate */
		cert = SSL_get_peer_certificate(sock->ssl);
		if (!cert)
			return error("unable to get peer certificate.");
		if (verify_hostname(cert, server.host) < 0)
			return -1;
	}

	return 0;
}
#endif

static int socket_read(struct imap_socket *sock, char *buf, int len)
{
	ssize_t n;
#ifndef NO_OPENSSL
	if (sock->ssl)
		n = SSL_read(sock->ssl, buf, len);
	else
#endif
		n = xread(sock->fd[0], buf, len);
	if (n <= 0) {
		socket_perror("read", sock, n);
		close(sock->fd[0]);
		close(sock->fd[1]);
		sock->fd[0] = sock->fd[1] = -1;
	}
	return n;
}

static int socket_write(struct imap_socket *sock, const char *buf, int len)
{
	int n;
#ifndef NO_OPENSSL
	if (sock->ssl)
		n = SSL_write(sock->ssl, buf, len);
	else
#endif
		n = write_in_full(sock->fd[1], buf, len);
	if (n != len) {
		socket_perror("write", sock, n);
		close(sock->fd[0]);
		close(sock->fd[1]);
		sock->fd[0] = sock->fd[1] = -1;
	}
	return n;
}

static void socket_shutdown(struct imap_socket *sock)
{
#ifndef NO_OPENSSL
	if (sock->ssl) {
		SSL_shutdown(sock->ssl);
		SSL_free(sock->ssl);
	}
#endif
	close(sock->fd[0]);
	close(sock->fd[1]);
}

/* simple line buffering */
static int buffer_gets(struct imap_buffer *b, char **s)
{
	int n;
	int start = b->offset;

	*s = b->buf + start;

	for (;;) {
		/* make sure we have enough data to read the \r\n sequence */
		if (b->offset + 1 >= b->bytes) {
			if (start) {
				/* shift down used bytes */
				*s = b->buf;

				assert(start <= b->bytes);
				n = b->bytes - start;

				if (n)
					memmove(b->buf, b->buf + start, n);
				b->offset -= start;
				b->bytes = n;
				start = 0;
			}

			n = socket_read(&b->sock, b->buf + b->bytes,
					 sizeof(b->buf) - b->bytes);

			if (n <= 0)
				return -1;

			b->bytes += n;
		}

		if (b->buf[b->offset] == '\r') {
			assert(b->offset + 1 < b->bytes);
			if (b->buf[b->offset + 1] == '\n') {
				b->buf[b->offset] = 0;  /* terminate the string */
				b->offset += 2; /* next line */
				if (Verbose)
					puts(*s);
				return 0;
			}
		}

		b->offset++;
	}
	/* not reached */
}

static void imap_info(const char *msg, ...)
{
	va_list va;

	if (!Quiet) {
		va_start(va, msg);
		vprintf(msg, va);
		va_end(va);
		fflush(stdout);
	}
}

static void imap_warn(const char *msg, ...)
{
	va_list va;

	if (Quiet < 2) {
		va_start(va, msg);
		vfprintf(stderr, msg, va);
		va_end(va);
	}
}

static char *next_arg(char **s)
{
	char *ret;

	if (!s || !*s)
		return NULL;
	while (isspace((unsigned char) **s))
		(*s)++;
	if (!**s) {
		*s = NULL;
		return NULL;
	}
	if (**s == '"') {
		++*s;
		ret = *s;
		*s = strchr(*s, '"');
	} else {
		ret = *s;
		while (**s && !isspace((unsigned char) **s))
			(*s)++;
	}
	if (*s) {
		if (**s)
			*(*s)++ = 0;
		if (!**s)
			*s = NULL;
	}
	return ret;
}

static int nfsnprintf(char *buf, int blen, const char *fmt, ...)
{
	int ret;
	va_list va;

	va_start(va, fmt);
	if (blen <= 0 || (unsigned)(ret = vsnprintf(buf, blen, fmt, va)) >= (unsigned)blen)
		die("Fatal: buffer too small. Please report a bug.");
	va_end(va);
	return ret;
}

static struct imap_cmd *v_issue_imap_cmd(struct imap_store *ctx,
					 struct imap_cmd_cb *cb,
					 const char *fmt, va_list ap)
{
	struct imap *imap = ctx->imap;
	struct imap_cmd *cmd;
	int n, bufl;
	char buf[1024];

	cmd = xmalloc(sizeof(struct imap_cmd));
	nfvasprintf(&cmd->cmd, fmt, ap);
	cmd->tag = ++imap->nexttag;

	if (cb)
		cmd->cb = *cb;
	else
		memset(&cmd->cb, 0, sizeof(cmd->cb));

	while (imap->literal_pending)
		get_cmd_result(ctx, NULL);

	if (!cmd->cb.data)
		bufl = nfsnprintf(buf, sizeof(buf), "%d %s\r\n", cmd->tag, cmd->cmd);
	else
		bufl = nfsnprintf(buf, sizeof(buf), "%d %s{%d%s}\r\n",
				  cmd->tag, cmd->cmd, cmd->cb.dlen,
				  CAP(LITERALPLUS) ? "+" : "");

	if (Verbose) {
		if (imap->num_in_progress)
			printf("(%d in progress) ", imap->num_in_progress);
		if (memcmp(cmd->cmd, "LOGIN", 5))
			printf(">>> %s", buf);
		else
			printf(">>> %d LOGIN <user> <pass>\n", cmd->tag);
	}
	if (socket_write(&imap->buf.sock, buf, bufl) != bufl) {
		free(cmd->cmd);
		free(cmd);
		if (cb)
			free(cb->data);
		return NULL;
	}
	if (cmd->cb.data) {
		if (CAP(LITERALPLUS)) {
			n = socket_write(&imap->buf.sock, cmd->cb.data, cmd->cb.dlen);
			free(cmd->cb.data);
			if (n != cmd->cb.dlen ||
			    socket_write(&imap->buf.sock, "\r\n", 2) != 2) {
				free(cmd->cmd);
				free(cmd);
				return NULL;
			}
			cmd->cb.data = NULL;
		} else
			imap->literal_pending = 1;
	} else if (cmd->cb.cont)
		imap->literal_pending = 1;
	cmd->next = NULL;
	*imap->in_progress_append = cmd;
	imap->in_progress_append = &cmd->next;
	imap->num_in_progress++;
	return cmd;
}

__attribute__((format (printf, 3, 4)))
static struct imap_cmd *issue_imap_cmd(struct imap_store *ctx,
				       struct imap_cmd_cb *cb,
				       const char *fmt, ...)
{
	struct imap_cmd *ret;
	va_list ap;

	va_start(ap, fmt);
	ret = v_issue_imap_cmd(ctx, cb, fmt, ap);
	va_end(ap);
	return ret;
}

__attribute__((format (printf, 3, 4)))
static int imap_exec(struct imap_store *ctx, struct imap_cmd_cb *cb,
		     const char *fmt, ...)
{
	va_list ap;
	struct imap_cmd *cmdp;

	va_start(ap, fmt);
	cmdp = v_issue_imap_cmd(ctx, cb, fmt, ap);
	va_end(ap);
	if (!cmdp)
		return RESP_BAD;

	return get_cmd_result(ctx, cmdp);
}

__attribute__((format (printf, 3, 4)))
static int imap_exec_m(struct imap_store *ctx, struct imap_cmd_cb *cb,
		       const char *fmt, ...)
{
	va_list ap;
	struct imap_cmd *cmdp;

	va_start(ap, fmt);
	cmdp = v_issue_imap_cmd(ctx, cb, fmt, ap);
	va_end(ap);
	if (!cmdp)
		return DRV_STORE_BAD;

	switch (get_cmd_result(ctx, cmdp)) {
	case RESP_BAD: return DRV_STORE_BAD;
	case RESP_NO: return DRV_MSG_BAD;
	default: return DRV_OK;
	}
}

static int skip_imap_list_l(char **sp, int level)
{
	char *s = *sp;

	for (;;) {
		while (isspace((unsigned char)*s))
			s++;
		if (level && *s == ')') {
			s++;
			break;
		}
		if (*s == '(') {
			/* sublist */
			s++;
			if (skip_imap_list_l(&s, level + 1))
				goto bail;
		} else if (*s == '"') {
			/* quoted string */
			s++;
			for (; *s != '"'; s++)
				if (!*s)
					goto bail;
			s++;
		} else {
			/* atom */
			for (; *s && !isspace((unsigned char)*s); s++)
				if (level && *s == ')')
					break;
		}

		if (!level)
			break;
		if (!*s)
			goto bail;
	}
	*sp = s;
	return 0;

bail:
	return -1;
}

static void skip_list(char **sp)
{
	skip_imap_list_l(sp, 0);
}

static void parse_capability(struct imap *imap, char *cmd)
{
	char *arg;
	unsigned i;

	imap->caps = 0x80000000;
	while ((arg = next_arg(&cmd)))
		for (i = 0; i < ARRAY_SIZE(cap_list); i++)
			if (!strcmp(cap_list[i], arg))
				imap->caps |= 1 << i;
	imap->rcaps = imap->caps;
}

static int parse_response_code(struct imap_store *ctx, struct imap_cmd_cb *cb,
			       char *s)
{
	struct imap *imap = ctx->imap;
	char *arg, *p;

	if (*s != '[')
		return RESP_OK;		/* no response code */
	s++;
	if (!(p = strchr(s, ']'))) {
		fprintf(stderr, "IMAP error: malformed response code\n");
		return RESP_BAD;
	}
	*p++ = 0;
	arg = next_arg(&s);
	if (!strcmp("UIDVALIDITY", arg)) {
		if (!(arg = next_arg(&s)) || !(ctx->uidvalidity = atoi(arg))) {
			fprintf(stderr, "IMAP error: malformed UIDVALIDITY status\n");
			return RESP_BAD;
		}
	} else if (!strcmp("UIDNEXT", arg)) {
		if (!(arg = next_arg(&s)) || !(imap->uidnext = atoi(arg))) {
			fprintf(stderr, "IMAP error: malformed NEXTUID status\n");
			return RESP_BAD;
		}
	} else if (!strcmp("CAPABILITY", arg)) {
		parse_capability(imap, s);
	} else if (!strcmp("ALERT", arg)) {
		/* RFC2060 says that these messages MUST be displayed
		 * to the user
		 */
		for (; isspace((unsigned char)*p); p++);
		fprintf(stderr, "*** IMAP ALERT *** %s\n", p);
	} else if (cb && cb->ctx && !strcmp("APPENDUID", arg)) {
		if (!(arg = next_arg(&s)) || !(ctx->uidvalidity = atoi(arg)) ||
		    !(arg = next_arg(&s)) || !(*(int *)cb->ctx = atoi(arg))) {
			fprintf(stderr, "IMAP error: malformed APPENDUID status\n");
			return RESP_BAD;
		}
	}
	return RESP_OK;
}

static int get_cmd_result(struct imap_store *ctx, struct imap_cmd *tcmd)
{
	struct imap *imap = ctx->imap;
	struct imap_cmd *cmdp, **pcmdp, *ncmdp;
	char *cmd, *arg, *arg1, *p;
	int n, resp, resp2, tag;

	for (;;) {
		if (buffer_gets(&imap->buf, &cmd))
			return RESP_BAD;

		arg = next_arg(&cmd);
		if (*arg == '*') {
			arg = next_arg(&cmd);
			if (!arg) {
				fprintf(stderr, "IMAP error: unable to parse untagged response\n");
				return RESP_BAD;
			}

			if (!strcmp("NAMESPACE", arg)) {
				/* rfc2342 NAMESPACE response. */
				skip_list(&cmd); /* Personal mailboxes */
				skip_list(&cmd); /* Others' mailboxes */
				skip_list(&cmd); /* Shared mailboxes */
			} else if (!strcmp("OK", arg) || !strcmp("BAD", arg) ||
				   !strcmp("NO", arg) || !strcmp("BYE", arg)) {
				if ((resp = parse_response_code(ctx, NULL, cmd)) != RESP_OK)
					return resp;
			} else if (!strcmp("CAPABILITY", arg)) {
				parse_capability(imap, cmd);
			} else if ((arg1 = next_arg(&cmd))) {
				; /*
				   * Unhandled response-data with at least two words.
				   * Ignore it.
				   *
				   * NEEDSWORK: Previously this case handled '<num> EXISTS'
				   * and '<num> RECENT' but as a probably-unintended side
				   * effect it ignores other unrecognized two-word
				   * responses.  imap-send doesn't ever try to read
				   * messages or mailboxes these days, so consider
				   * eliminating this case.
				   */
			} else {
				fprintf(stderr, "IMAP error: unable to parse untagged response\n");
				return RESP_BAD;
			}
		} else if (!imap->in_progress) {
			fprintf(stderr, "IMAP error: unexpected reply: %s %s\n", arg, cmd ? cmd : "");
			return RESP_BAD;
		} else if (*arg == '+') {
			/* This can happen only with the last command underway, as
			   it enforces a round-trip. */
			cmdp = (struct imap_cmd *)((char *)imap->in_progress_append -
			       offsetof(struct imap_cmd, next));
			if (cmdp->cb.data) {
				n = socket_write(&imap->buf.sock, cmdp->cb.data, cmdp->cb.dlen);
				free(cmdp->cb.data);
				cmdp->cb.data = NULL;
				if (n != (int)cmdp->cb.dlen)
					return RESP_BAD;
			} else if (cmdp->cb.cont) {
				if (cmdp->cb.cont(ctx, cmdp, cmd))
					return RESP_BAD;
			} else {
				fprintf(stderr, "IMAP error: unexpected command continuation request\n");
				return RESP_BAD;
			}
			if (socket_write(&imap->buf.sock, "\r\n", 2) != 2)
				return RESP_BAD;
			if (!cmdp->cb.cont)
				imap->literal_pending = 0;
			if (!tcmd)
				return DRV_OK;
		} else {
			tag = atoi(arg);
			for (pcmdp = &imap->in_progress; (cmdp = *pcmdp); pcmdp = &cmdp->next)
				if (cmdp->tag == tag)
					goto gottag;
			fprintf(stderr, "IMAP error: unexpected tag %s\n", arg);
			return RESP_BAD;
		gottag:
			if (!(*pcmdp = cmdp->next))
				imap->in_progress_append = pcmdp;
			imap->num_in_progress--;
			if (cmdp->cb.cont || cmdp->cb.data)
				imap->literal_pending = 0;
			arg = next_arg(&cmd);
			if (!strcmp("OK", arg))
				resp = DRV_OK;
			else {
				if (!strcmp("NO", arg)) {
					if (cmdp->cb.create && cmd && (cmdp->cb.trycreate || !memcmp(cmd, "[TRYCREATE]", 11))) { /* SELECT, APPEND or UID COPY */
						p = strchr(cmdp->cmd, '"');
						if (!issue_imap_cmd(ctx, NULL, "CREATE \"%.*s\"", (int)(strchr(p + 1, '"') - p + 1), p)) {
							resp = RESP_BAD;
							goto normal;
						}
						/* not waiting here violates the spec, but a server that does not
						   grok this nonetheless violates it too. */
						cmdp->cb.create = 0;
						if (!(ncmdp = issue_imap_cmd(ctx, &cmdp->cb, "%s", cmdp->cmd))) {
							resp = RESP_BAD;
							goto normal;
						}
						free(cmdp->cmd);
						free(cmdp);
						if (!tcmd)
							return 0;	/* ignored */
						if (cmdp == tcmd)
							tcmd = ncmdp;
						continue;
					}
					resp = RESP_NO;
				} else /*if (!strcmp("BAD", arg))*/
					resp = RESP_BAD;
				fprintf(stderr, "IMAP command '%s' returned response (%s) - %s\n",
					 memcmp(cmdp->cmd, "LOGIN", 5) ?
							cmdp->cmd : "LOGIN <user> <pass>",
							arg, cmd ? cmd : "");
			}
			if ((resp2 = parse_response_code(ctx, &cmdp->cb, cmd)) > resp)
				resp = resp2;
		normal:
			if (cmdp->cb.done)
				cmdp->cb.done(ctx, cmdp, resp);
			free(cmdp->cb.data);
			free(cmdp->cmd);
			free(cmdp);
			if (!tcmd || tcmd == cmdp)
				return resp;
		}
	}
	/* not reached */
}

static void imap_close_server(struct imap_store *ictx)
{
	struct imap *imap = ictx->imap;

	if (imap->buf.sock.fd[0] != -1) {
		imap_exec(ictx, NULL, "LOGOUT");
		socket_shutdown(&imap->buf.sock);
	}
	free(imap);
}

static void imap_close_store(struct imap_store *ctx)
{
	imap_close_server(ctx);
	free(ctx);
}

#ifndef NO_OPENSSL

/*
 * hexchar() and cram() functions are based on the code from the isync
 * project (http://isync.sf.net/).
 */
static char hexchar(unsigned int b)
{
	return b < 10 ? '0' + b : 'a' + (b - 10);
}

#define ENCODED_SIZE(n) (4*((n+2)/3))
static char *cram(const char *challenge_64, const char *user, const char *pass)
{
	int i, resp_len, encoded_len, decoded_len;
	HMAC_CTX hmac;
	unsigned char hash[16];
	char hex[33];
	char *response, *response_64, *challenge;

	/*
	 * length of challenge_64 (i.e. base-64 encoded string) is a good
	 * enough upper bound for challenge (decoded result).
	 */
	encoded_len = strlen(challenge_64);
	challenge = xmalloc(encoded_len);
	decoded_len = EVP_DecodeBlock((unsigned char *)challenge,
				      (unsigned char *)challenge_64, encoded_len);
	if (decoded_len < 0)
		die("invalid challenge %s", challenge_64);
	HMAC_Init(&hmac, (unsigned char *)pass, strlen(pass), EVP_md5());
	HMAC_Update(&hmac, (unsigned char *)challenge, decoded_len);
	HMAC_Final(&hmac, hash, NULL);
	HMAC_CTX_cleanup(&hmac);

	hex[32] = 0;
	for (i = 0; i < 16; i++) {
		hex[2 * i] = hexchar((hash[i] >> 4) & 0xf);
		hex[2 * i + 1] = hexchar(hash[i] & 0xf);
	}

	/* response: "<user> <digest in hex>" */
	resp_len = strlen(user) + 1 + strlen(hex) + 1;
	response = xmalloc(resp_len);
	sprintf(response, "%s %s", user, hex);

	response_64 = xmalloc(ENCODED_SIZE(resp_len) + 1);
	encoded_len = EVP_EncodeBlock((unsigned char *)response_64,
				      (unsigned char *)response, resp_len);
	if (encoded_len < 0)
		die("EVP_EncodeBlock error");
	response_64[encoded_len] = '\0';
	return (char *)response_64;
}

#else

static char *cram(const char *challenge_64, const char *user, const char *pass)
{
	die("If you want to use CRAM-MD5 authenticate method, "
	    "you have to build git-imap-send with OpenSSL library.");
}

#endif

static int auth_cram_md5(struct imap_store *ctx, struct imap_cmd *cmd, const char *prompt)
{
	int ret;
	char *response;

	response = cram(prompt, server.user, server.pass);

	ret = socket_write(&ctx->imap->buf.sock, response, strlen(response));
	if (ret != strlen(response))
		return error("IMAP error: sending response failed");

	free(response);

	return 0;
}

static struct imap_store *imap_open_store(struct imap_server_conf *srvc)
{
	struct imap_store *ctx;
	struct imap *imap;
	char *arg, *rsp;
	int s = -1, preauth;

	ctx = xcalloc(sizeof(*ctx), 1);

	ctx->imap = imap = xcalloc(sizeof(*imap), 1);
	imap->buf.sock.fd[0] = imap->buf.sock.fd[1] = -1;
	imap->in_progress_append = &imap->in_progress;

	/* open connection to IMAP server */

	if (srvc->tunnel) {
		const char *argv[] = { srvc->tunnel, NULL };
		struct child_process tunnel = {NULL};

		imap_info("Starting tunnel '%s'... ", srvc->tunnel);

		tunnel.argv = argv;
		tunnel.use_shell = 1;
		tunnel.in = -1;
		tunnel.out = -1;
		if (start_command(&tunnel))
			die("cannot start proxy %s", argv[0]);

		imap->buf.sock.fd[0] = tunnel.out;
		imap->buf.sock.fd[1] = tunnel.in;

		imap_info("ok\n");
	} else {
#ifndef NO_IPV6
		struct addrinfo hints, *ai0, *ai;
		int gai;
		char portstr[6];

		snprintf(portstr, sizeof(portstr), "%d", srvc->port);

		memset(&hints, 0, sizeof(hints));
		hints.ai_socktype = SOCK_STREAM;
		hints.ai_protocol = IPPROTO_TCP;

		imap_info("Resolving %s... ", srvc->host);
		gai = getaddrinfo(srvc->host, portstr, &hints, &ai);
		if (gai) {
			fprintf(stderr, "getaddrinfo: %s\n", gai_strerror(gai));
			goto bail;
		}
		imap_info("ok\n");

		for (ai0 = ai; ai; ai = ai->ai_next) {
			char addr[NI_MAXHOST];

			s = socket(ai->ai_family, ai->ai_socktype,
				   ai->ai_protocol);
			if (s < 0)
				continue;

			getnameinfo(ai->ai_addr, ai->ai_addrlen, addr,
				    sizeof(addr), NULL, 0, NI_NUMERICHOST);
			imap_info("Connecting to [%s]:%s... ", addr, portstr);

			if (connect(s, ai->ai_addr, ai->ai_addrlen) < 0) {
				close(s);
				s = -1;
				perror("connect");
				continue;
			}

			break;
		}
		freeaddrinfo(ai0);
#else /* NO_IPV6 */
		struct hostent *he;
		struct sockaddr_in addr;

		memset(&addr, 0, sizeof(addr));
		addr.sin_port = htons(srvc->port);
		addr.sin_family = AF_INET;

		imap_info("Resolving %s... ", srvc->host);
		he = gethostbyname(srvc->host);
		if (!he) {
			perror("gethostbyname");
			goto bail;
		}
		imap_info("ok\n");

		addr.sin_addr.s_addr = *((int *) he->h_addr_list[0]);

		s = socket(PF_INET, SOCK_STREAM, 0);

		imap_info("Connecting to %s:%hu... ", inet_ntoa(addr.sin_addr), ntohs(addr.sin_port));
		if (connect(s, (struct sockaddr *)&addr, sizeof(addr))) {
			close(s);
			s = -1;
			perror("connect");
		}
#endif
		if (s < 0) {
			fputs("Error: unable to connect to server.\n", stderr);
			goto bail;
		}

		imap->buf.sock.fd[0] = s;
		imap->buf.sock.fd[1] = dup(s);

		if (srvc->use_ssl &&
		    ssl_socket_connect(&imap->buf.sock, 0, srvc->ssl_verify)) {
			close(s);
			goto bail;
		}
		imap_info("ok\n");
	}

	/* read the greeting string */
	if (buffer_gets(&imap->buf, &rsp)) {
		fprintf(stderr, "IMAP error: no greeting response\n");
		goto bail;
	}
	arg = next_arg(&rsp);
	if (!arg || *arg != '*' || (arg = next_arg(&rsp)) == NULL) {
		fprintf(stderr, "IMAP error: invalid greeting response\n");
		goto bail;
	}
	preauth = 0;
	if (!strcmp("PREAUTH", arg))
		preauth = 1;
	else if (strcmp("OK", arg) != 0) {
		fprintf(stderr, "IMAP error: unknown greeting response\n");
		goto bail;
	}
	parse_response_code(ctx, NULL, rsp);
	if (!imap->caps && imap_exec(ctx, NULL, "CAPABILITY") != RESP_OK)
		goto bail;

	if (!preauth) {
#ifndef NO_OPENSSL
		if (!srvc->use_ssl && CAP(STARTTLS)) {
			if (imap_exec(ctx, NULL, "STARTTLS") != RESP_OK)
				goto bail;
			if (ssl_socket_connect(&imap->buf.sock, 1,
					       srvc->ssl_verify))
				goto bail;
			/* capabilities may have changed, so get the new capabilities */
			if (imap_exec(ctx, NULL, "CAPABILITY") != RESP_OK)
				goto bail;
		}
#endif
		imap_info("Logging in...\n");
		if (!srvc->user) {
			fprintf(stderr, "Skipping server %s, no user\n", srvc->host);
			goto bail;
		}
		if (!srvc->pass) {
			struct strbuf prompt = STRBUF_INIT;
			strbuf_addf(&prompt, "Password (%s@%s): ", srvc->user, srvc->host);
			arg = git_getpass(prompt.buf);
			strbuf_release(&prompt);
			if (!*arg) {
				fprintf(stderr, "Skipping account %s@%s, no password\n", srvc->user, srvc->host);
				goto bail;
			}
			/*
			 * getpass() returns a pointer to a static buffer.  make a copy
			 * for long term storage.
			 */
			srvc->pass = xstrdup(arg);
		}
		if (CAP(NOLOGIN)) {
			fprintf(stderr, "Skipping account %s@%s, server forbids LOGIN\n", srvc->user, srvc->host);
			goto bail;
		}

		if (srvc->auth_method) {
			struct imap_cmd_cb cb;

			if (!strcmp(srvc->auth_method, "CRAM-MD5")) {
				if (!CAP(AUTH_CRAM_MD5)) {
					fprintf(stderr, "You specified"
						"CRAM-MD5 as authentication method, "
						"but %s doesn't support it.\n", srvc->host);
					goto bail;
				}
				/* CRAM-MD5 */

				memset(&cb, 0, sizeof(cb));
				cb.cont = auth_cram_md5;
				if (imap_exec(ctx, &cb, "AUTHENTICATE CRAM-MD5") != RESP_OK) {
					fprintf(stderr, "IMAP error: AUTHENTICATE CRAM-MD5 failed\n");
					goto bail;
				}
			} else {
				fprintf(stderr, "Unknown authentication method:%s\n", srvc->host);
				goto bail;
			}
		} else {
			if (!imap->buf.sock.ssl)
				imap_warn("*** IMAP Warning *** Password is being "
					  "sent in the clear\n");
			if (imap_exec(ctx, NULL, "LOGIN \"%s\" \"%s\"", srvc->user, srvc->pass) != RESP_OK) {
				fprintf(stderr, "IMAP error: LOGIN failed\n");
				goto bail;
			}
		}
	} /* !preauth */

	ctx->prefix = "";
	return ctx;

bail:
	imap_close_store(ctx);
	return NULL;
}

/*
 * Insert CR characters as necessary in *msg to ensure that every LF
 * character in *msg is preceded by a CR.
 */
static void lf_to_crlf(struct strbuf *msg)
{
	char *new;
	size_t i, j;
	char lastc;

	/* First pass: tally, in j, the size of the new string: */
	for (i = j = 0, lastc = '\0'; i < msg->len; i++) {
		if (msg->buf[i] == '\n' && lastc != '\r')
			j++; /* a CR will need to be added here */
		lastc = msg->buf[i];
		j++;
	}

	new = xmalloc(j + 1);

	/*
	 * Second pass: write the new string.  Note that this loop is
	 * otherwise identical to the first pass.
	 */
	for (i = j = 0, lastc = '\0'; i < msg->len; i++) {
		if (msg->buf[i] == '\n' && lastc != '\r')
			new[j++] = '\r';
		lastc = new[j++] = msg->buf[i];
	}
	strbuf_attach(msg, new, j, j + 1);
}

/*
 * Store msg to IMAP.  Also detach and free the data from msg->data,
 * leaving msg->data empty.
 */
static int imap_store_msg(struct imap_store *ctx, struct strbuf *msg)
{
	struct imap *imap = ctx->imap;
	struct imap_cmd_cb cb;
	const char *prefix, *box;
	int ret;

	lf_to_crlf(msg);
	memset(&cb, 0, sizeof(cb));

	cb.dlen = msg->len;
	cb.data = strbuf_detach(msg, NULL);

	box = ctx->name;
	prefix = !strcmp(box, "INBOX") ? "" : ctx->prefix;
	cb.create = 0;
	ret = imap_exec_m(ctx, &cb, "APPEND \"%s%s\" ", prefix, box);
	imap->caps = imap->rcaps;
	if (ret != DRV_OK)
		return ret;

	return DRV_OK;
}

static void wrap_in_html(struct strbuf *msg)
{
	struct strbuf buf = STRBUF_INIT;
	static char *content_type = "Content-Type: text/html;\n";
	static char *pre_open = "<pre>\n";
	static char *pre_close = "</pre>\n";
	const char *body = strstr(msg->buf, "\n\n");

	if (!body)
		return; /* Headers but no body; no wrapping needed */

	body += 2;

	strbuf_add(&buf, msg->buf, body - msg->buf - 1);
	strbuf_addstr(&buf, content_type);
	strbuf_addch(&buf, '\n');
	strbuf_addstr(&buf, pre_open);
	strbuf_addstr_xml_quoted(&buf, body);
	strbuf_addstr(&buf, pre_close);

	strbuf_release(msg);
	*msg = buf;
}

#define CHUNKSIZE 0x1000

static int read_message(FILE *f, struct strbuf *all_msgs)
{
	do {
		if (strbuf_fread(all_msgs, CHUNKSIZE, f) <= 0)
			break;
	} while (!feof(f));

	return ferror(f) ? -1 : 0;
}

static int count_messages(struct strbuf *all_msgs)
{
	int count = 0;
	char *p = all_msgs->buf;

	while (1) {
		if (starts_with(p, "From ")) {
			p = strstr(p+5, "\nFrom: ");
			if (!p) break;
			p = strstr(p+7, "\nDate: ");
			if (!p) break;
			p = strstr(p+7, "\nSubject: ");
			if (!p) break;
			p += 10;
			count++;
		}
		p = strstr(p+5, "\nFrom ");
		if (!p)
			break;
		p++;
	}
	return count;
}

/*
 * Copy the next message from all_msgs, starting at offset *ofs, to
 * msg.  Update *ofs to the start of the following message.  Return
 * true iff a message was successfully copied.
 */
static int split_msg(struct strbuf *all_msgs, struct strbuf *msg, int *ofs)
{
	char *p, *data;
	size_t len;

	if (*ofs >= all_msgs->len)
		return 0;

	data = &all_msgs->buf[*ofs];
	len = all_msgs->len - *ofs;

	if (len < 5 || !starts_with(data, "From "))
		return 0;

	p = strchr(data, '\n');
	if (p) {
		p++;
		len -= p - data;
		*ofs += p - data;
		data = p;
	}

	p = strstr(data, "\nFrom ");
	if (p)
		len = &p[1] - data;

	strbuf_add(msg, data, len);
	*ofs += len;
	return 1;
}

static char *imap_folder;

static int git_imap_config(const char *key, const char *val, void *cb)
{
	char imap_key[] = "imap.";

	if (strncmp(key, imap_key, sizeof imap_key - 1))
		return 0;

	key += sizeof imap_key - 1;

	/* check booleans first, and barf on others */
	if (!strcmp("sslverify", key))
		server.ssl_verify = git_config_bool(key, val);
	else if (!strcmp("preformattedhtml", key))
		server.use_html = git_config_bool(key, val);
	else if (!val)
		return config_error_nonbool(key);

	if (!strcmp("folder", key)) {
		imap_folder = xstrdup(val);
	} else if (!strcmp("host", key)) {
		if (starts_with(val, "imap:"))
			val += 5;
		else if (starts_with(val, "imaps:")) {
			val += 6;
			server.use_ssl = 1;
		}
		if (starts_with(val, "//"))
			val += 2;
		server.host = xstrdup(val);
	} else if (!strcmp("user", key))
		server.user = xstrdup(val);
	else if (!strcmp("pass", key))
		server.pass = xstrdup(val);
	else if (!strcmp("port", key))
		server.port = git_config_int(key, val);
	else if (!strcmp("tunnel", key))
		server.tunnel = xstrdup(val);
	else if (!strcmp("authmethod", key))
		server.auth_method = xstrdup(val);

	return 0;
}

int main(int argc, char **argv)
{
	struct strbuf all_msgs = STRBUF_INIT;
	struct strbuf msg = STRBUF_INIT;
	struct imap_store *ctx = NULL;
	int ofs = 0;
	int r;
	int total, n = 0;
	int nongit_ok;

	git_extract_argv0_path(argv[0]);

	git_setup_gettext();

	if (argc != 1)
		usage(imap_send_usage);

	setup_git_directory_gently(&nongit_ok);
	git_config(git_imap_config, NULL);

	if (!server.port)
		server.port = server.use_ssl ? 993 : 143;

	if (!imap_folder) {
		fprintf(stderr, "no imap store specified\n");
		return 1;
	}
	if (!server.host) {
		if (!server.tunnel) {
			fprintf(stderr, "no imap host specified\n");
			return 1;
		}
		server.host = "tunnel";
	}

	/* read the messages */
	if (read_message(stdin, &all_msgs)) {
		fprintf(stderr, "error reading input\n");
		return 1;
	}

	if (all_msgs.len == 0) {
		fprintf(stderr, "nothing to send\n");
		return 1;
	}

	total = count_messages(&all_msgs);
	if (!total) {
		fprintf(stderr, "no messages to send\n");
		return 1;
	}

	/* write it to the imap server */
	ctx = imap_open_store(&server);
	if (!ctx) {
		fprintf(stderr, "failed to open store\n");
		return 1;
	}

	fprintf(stderr, "sending %d message%s\n", total, (total != 1) ? "s" : "");
	ctx->name = imap_folder;
	while (1) {
		unsigned percent = n * 100 / total;

		fprintf(stderr, "%4u%% (%d/%d) done\r", percent, n, total);
		if (!split_msg(&all_msgs, &msg, &ofs))
			break;
		if (server.use_html)
			wrap_in_html(&msg);
		r = imap_store_msg(ctx, &msg);
		if (r != DRV_OK)
			break;
		n++;
	}
	fprintf(stderr, "\n");

	imap_close_store(ctx);

	return 0;
}
