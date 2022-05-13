#ifndef QUOTE_H
#define QUOTE_H

struct strbuf;

/* Help to copy the thing properly quoted for the shell safety.
 * any single quote is replaced with '\'', any exclamation point
 * is replaced with '\!', and the whole thing is enclosed in a
 * single quote pair.
 *
 * For example, if you are passing the result to system() as an
 * argument:
 *
 * sprintf(cmd, "foobar %s %s", sq_quote(arg0), sq_quote(arg1))
 *
 * would be appropriate.  If the system() is going to call ssh to
 * run the command on the other side:
 *
 * sprintf(cmd, "git-diff-tree %s %s", sq_quote(arg0), sq_quote(arg1));
 * sprintf(rcmd, "ssh %s %s", sq_quote(host), sq_quote(cmd));
 *
 * Note that the above examples leak memory!  Remember to free result from
 * sq_quote() in a real application.
 *
 * sq_quote_buf() writes to an existing buffer of specified size; it
 * will return the number of characters that would have been written
 * excluding the final null regardless of the buffer size.
 */

extern void sq_quote_buf(struct strbuf *, const char *src);
extern void sq_quote_argv(struct strbuf *, const char **argv, size_t maxlen);

/* This unwraps what sq_quote() produces in place, but returns
 * NULL if the input does not look like what sq_quote would have
 * produced.
 */
extern char *sq_dequote(char *);

/*
 * Same as the above, but can be used to unwrap many arguments in the
 * same string separated by space. Like sq_quote, it works in place,
 * modifying arg and appending pointers into it to argv.
 */
extern int sq_dequote_to_argv(char *arg, const char ***argv, int *nr, int *alloc);

/*
 * Same as above, but store the unquoted strings in an argv_array. We will
 * still modify arg in place, but unlike sq_dequote_to_argv, the argv_array
 * will duplicate and take ownership of the strings.
 */
struct argv_array;
extern int sq_dequote_to_argv_array(char *arg, struct argv_array *);

extern int unquote_c_style(struct strbuf *, const char *quoted, const char **endp);
extern size_t quote_c_style(const char *name, struct strbuf *, FILE *, int no_dq);
extern void quote_two_c_style(struct strbuf *, const char *, const char *, int);

extern void write_name_quoted(const char *name, FILE *, int terminator);
extern void write_name_quotedpfx(const char *pfx, size_t pfxlen,
                                 const char *name, FILE *, int terminator);
extern void write_name_quoted_relative(const char *name, const char *prefix,
		FILE *fp, int terminator);

/* quote path as relative to the given prefix */
extern char *quote_path_relative(const char *in, const char *prefix,
			  struct strbuf *out);

/* quoting as a string literal for other languages */
extern void perl_quote_buf(struct strbuf *sb, const char *src);
extern void python_quote_buf(struct strbuf *sb, const char *src);
extern void tcl_quote_buf(struct strbuf *sb, const char *src);

#endif
