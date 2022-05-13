#define _GNU_SOURCE 
#include <fcntl.h>
#include <unistd.h>
#include <dirent.h>
#include <stdio.h>
#include <utime.h>
#include <time.h>
#include <string.h>
#include <sched.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <sys/mount.h>
#define __STDC_FORMAT_MACROS
#include <inttypes.h>
#include <assert.h>

#define PATH_LEN 1024

#define NUM_CPU	48
#define NUM	6

static int max_array[NUM] = {0, 133,1633,16633,166633};
#define MIN	0

static char *paths[NUM] = {
  "/000001/f",
  "/000001/000002/000003/000004/000005/000006/000007/000008/000009/000010/f",
  "/000001/000002/000003/000004/000005/000006/000007/000008/000009/000010/000011/000012/000013/000014/000015/000016/000017/000018/000019/000020/f",
  "/000001/000002/000003/000004/000005/000006/000007/000008/000009/000010/000011/000012/000013/000014/000015/000016/000017/000018/000019/000020/000021/000022/000023/000024/000025/000026/000027/000028/000029/000030/f",
  "/000001/000002/000003/000004/000005/000006/000007/000008/000009/000010/000011/000012/000013/000014/000015/000016/000017/000018/000019/000020/000021/000022/000023/000024/000025/000026/000027/000028/000029/000030/000031/000032/000033/000034/000035/000036/000037/000038/000039/000040/f",
  "/000001/000002/000003/000004/000005/000006/000007/000008/000009/000010/000011/000012/000013/000014/000015/000016/000017/000018/000019/000020/000021/000022/000023/000024/000025/000026/000027/000028/000029/000030/000031/000032/000033/000034/000035/000036/000037/000038/000039/000040/000041/000042/000043/000044/000045/000046/000047/000048/000049/000050/f",
};

static void pw_test(int depth, int width, int wbinvd, char *fs_path, int seq) {
	struct timespec tv_begin,tv_end;
	struct stat statbuf;
	int k, v, ret = 0, fd;
	char path[PATH_LEN];
	double sum = 0;
	int nr = max_array[width];

	if (nr > 1000) nr = 1000;

	if (wbinvd && nr > 10) nr = 10;

	for (k = 0; k < nr; k ++) {
	  v = seq ? k : rand() % max_array[width] + MIN;
	  
		sprintf(path, "%s%s%06d", fs_path, paths[depth], v);

		if (wbinvd) {
		  fd = open("/proc/wbinvd", O_RDONLY);
		  read(fd, NULL, 0);
		  close(fd);
		}

		clock_gettime(CLOCK_REALTIME, &tv_begin);
		ret = stat(path, &statbuf);
		clock_gettime(CLOCK_REALTIME, &tv_end);
		assert(!ret);
		sum += tv_end.tv_nsec - tv_begin.tv_nsec;
	}
	
	printf("LATENCY: %lu\n", (unsigned long) sum/nr);
}

static void path_resolve_test(int depth, int width, int wbinvd, char *fs_path, int seq) {
  pw_test(depth, width, wbinvd, fs_path, seq);
}

int main(int argc, char *argv[]) {
  /* ./test width depth wbinvd fs_path seq */
  
  int width = atoi(argv[1]), depth = atoi(argv[2]), wbinvd = atoi(argv[3]), seq = atoi(argv[5]);
  char *fs_path = argv[4];

	srand((unsigned)time(NULL));

	path_resolve_test(depth, width, wbinvd, fs_path, seq);

	return 0;
}
