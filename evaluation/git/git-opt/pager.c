#include "cache.h"
#include "run-command.h"
#include "sigchain.h"

#ifndef DEFAULT_PAGER
#define DEFAULT_PAGER "less"
#endif

struct pager_config {
	const char *cmd;
	int want;
	char *value;
};

/*
 * This is split up from the rest of git so that we can do
 * something different on Windows.
 */

static const char *pager_argv[] = { NULL, NULL };
static struct child_process pager_process;

static void wait_for_pager(void)
{
	fflush(stdout);
	fflush(stderr);
	/* signal EOF to pager */
	close(1);
	close(2);
	finish_command(&pager_process);
}

static void wait_for_pager_signal(int signo)
{
	wait_for_pager();
	sigchain_pop(signo);
	raise(signo);
}

const char *git_pager(int stdout_is_tty)
{
	const char *pager;

	if (!stdout_is_tty)
		return NULL;

	pager = getenv("GIT_PAGER");
	if (!pager) {
		if (!pager_program)
			git_config(git_default_config, NULL);
		pager = pager_program;
	}
	if (!pager)
		pager = getenv("PAGER");
	if (!pager)
		pager = DEFAULT_PAGER;
	if (!*pager || !strcmp(pager, "cat"))
		pager = NULL;

	return pager;
}

void setup_pager(void)
{
	const char *pager = git_pager(isatty(1));

	if (!pager || pager_in_use())
		return;

	/*
	 * force computing the width of the terminal before we redirect
	 * the standard output to the pager.
	 */
	(void) term_columns();

	setenv("GIT_PAGER_IN_USE", "true", 1);

	/* spawn the pager */
	pager_argv[0] = pager;
	pager_process.use_shell = 1;
	pager_process.argv = pager_argv;
	pager_process.in = -1;
	if (!getenv("LESS") || !getenv("LV")) {
		static const char *env[3];
		int i = 0;

		if (!getenv("LESS"))
			env[i++] = "LESS=FRSX";
		if (!getenv("LV"))
			env[i++] = "LV=-c";
		env[i] = NULL;
		pager_process.env = env;
	}
	if (start_command(&pager_process))
		return;

	/* original process continues, but writes to the pipe */
	dup2(pager_process.in, 1);
	if (isatty(2))
		dup2(pager_process.in, 2);
	close(pager_process.in);

	/* this makes sure that the parent terminates after the pager */
	sigchain_push_common(wait_for_pager_signal);
	atexit(wait_for_pager);
}

int pager_in_use(void)
{
	const char *env;
	env = getenv("GIT_PAGER_IN_USE");
	return env ? git_config_bool("GIT_PAGER_IN_USE", env) : 0;
}

/*
 * Return cached value (if set) or $COLUMNS environment variable (if
 * set and positive) or ioctl(1, TIOCGWINSZ).ws_col (if positive),
 * and default to 80 if all else fails.
 */
int term_columns(void)
{
	static int term_columns_at_startup;

	char *col_string;
	int n_cols;

	if (term_columns_at_startup)
		return term_columns_at_startup;

	term_columns_at_startup = 80;

	col_string = getenv("COLUMNS");
	if (col_string && (n_cols = atoi(col_string)) > 0)
		term_columns_at_startup = n_cols;
#ifdef TIOCGWINSZ
	else {
		struct winsize ws;
		if (!ioctl(1, TIOCGWINSZ, &ws) && ws.ws_col)
			term_columns_at_startup = ws.ws_col;
	}
#endif

	return term_columns_at_startup;
}

/*
 * How many columns do we need to show this number in decimal?
 */
int decimal_width(int number)
{
	int i, width;

	for (width = 1, i = 10; i <= number; width++)
		i *= 10;
	return width;
}

static int pager_command_config(const char *var, const char *value, void *data)
{
	struct pager_config *c = data;
	if (starts_with(var, "pager.") && !strcmp(var + 6, c->cmd)) {
		int b = git_config_maybe_bool(var, value);
		if (b >= 0)
			c->want = b;
		else {
			c->want = 1;
			c->value = xstrdup(value);
		}
	}
	return 0;
}

/* returns 0 for "no pager", 1 for "use pager", and -1 for "not specified" */
int check_pager_config(const char *cmd)
{
	struct pager_config c;
	c.cmd = cmd;
	c.want = -1;
	c.value = NULL;
	git_config(pager_command_config, &c);
	if (c.value)
		pager_program = c.value;
	return c.want;
}
