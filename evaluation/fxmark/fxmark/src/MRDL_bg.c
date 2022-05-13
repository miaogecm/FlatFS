/**
 * Nanobenchmark: Read operation with a background inhibitor
 *   RD. PROCESS = {read entries of its private directory}
 */	      
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <dirent.h>
#include <unistd.h>
#include <errno.h>
#include <signal.h>
#define __STDC_FORMAT_MACROS
#include <inttypes.h>
#include "fxmark.h"
#include "util.h"

static int stop_pre_work;

static void set_test_root(struct worker *worker, char *test_root)
{
	struct fx_opt *fx_opt = fx_opt_worker(worker);
	sprintf(test_root, "%s/%d", fx_opt->root, worker->id);
}

static void set_test_file(struct worker *worker, 
			  uint64_t file_id, char *test_file)
{
	struct fx_opt *fx_opt = fx_opt_worker(worker);
	sprintf(test_file, "%s/%d/n_dir_rd_bg-%d-%" PRIu64 ".dat",
		fx_opt->root, worker->id, worker->id, file_id);
}

static void sighandler(int x)
{
	stop_pre_work = 1;
}

static int pre_work(struct worker *worker)
{
	struct bench *bench = worker->bench;
	char path[PATH_MAX];
	int fd, rc = 0;

	/* do nothing if it is a background worker. */
	if (worker->is_bg) goto out;

	/* perform pre_work for bench->duration */
	if (signal(SIGALRM, sighandler) == SIG_ERR) {
		rc = errno;
		goto err_out;
	}
	alarm(bench->duration);

	/* create private directory */
	set_test_root(worker, path);
	rc = mkdir_p(path);
	if (rc) goto err_out;

	/* create files at the private directory */
	for (; !stop_pre_work; ++worker->private[0]) {
		set_test_file(worker, worker->private[0], path);
		if ((fd = open(path, O_CREAT | O_RDWR, S_IRWXU)) == -1) {
			if (errno == ENOSPC)
				goto out;
			goto err_out;
		}
		close(fd);
	}
out:
	return rc; 
err_out:
	rc = errno;
	goto out;
}

static int fg_work(struct worker *worker)
{
	struct bench *bench = worker->bench;
	char dir_path[PATH_MAX];
	DIR *dir;
	struct dirent entry;
	struct dirent *result;
	uint64_t iter = 0;
	int rc = 0;

	set_test_root(worker, dir_path);
	while (!bench->stop) {
		dir = opendir(dir_path);
		if (!dir) goto err_out;
		for (; !bench->stop; ++iter) {
			rc = readdir_r(dir, &entry, &result);
			if (rc) goto err_out;
		}
		closedir(dir);
	}
out:
	bench->stop = 1;
	worker->works = (double)iter;
	return rc;
err_out:
	rc = errno;
	goto out;
}

static int bg_work(struct worker *worker)
{
	struct bench *bench = worker->bench;
	char path[PATH_MAX];
	unsigned int file_ids[bench->ncpu];
	uint64_t iter = 0;
	int fd, rc = 0, i;

	while (!bench->stop) {
		/* delete a randomly selected file in each worker's private directory */
		for (i = 0; i < bench->ncpu && !bench->stop; ++i) {
			struct worker *w = &bench->workers[i];
			if (w->is_bg) continue;

			file_ids[i] = pseudo_random(file_ids[i]);
			set_test_file(w, file_ids[i] % w->private[0], path);
			rc = unlink(path);
			if (rc) goto err_out;
			++iter;
		}

		/* create the deleted file of each worker */
		for (i = 0; i < bench->ncpu && !bench->stop; ++i) {
			struct worker *w = &bench->workers[i];
			if (w->is_bg) continue;
			
			set_test_file(w, file_ids[i] % w->private[0], path);
			if ((fd = open(path, O_CREAT | O_RDWR, S_IRWXU)) == -1) {
				goto err_out;
			}
			close(fd);
			++iter;
		}
	}
out:
	bench->stop = 1;
	worker->works = (double)iter;
	return rc;
err_out:
	rc = errno;
	goto out;
}

static int main_work(struct worker *worker)
{
	if (worker->is_bg)
		return bg_work(worker);
	return fg_work(worker);
}

struct bench_operations n_dir_rd_bg_ops = {
	.pre_work  = pre_work, 
	.main_work = main_work,
};
