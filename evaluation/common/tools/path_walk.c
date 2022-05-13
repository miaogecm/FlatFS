#define _GNU_SOURCE 
#include <fcntl.h>
#include <unistd.h>
#include <dirent.h>
#include <assert.h>
#include <stdio.h>
#include <utime.h>
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

#define NUM_CPU	48

static unsigned long path_walk_time(char *path) {
  struct timeval tv_begin, tv_end;
  struct stat statbuf;
  int ret = 0;

  gettimeofday(&tv_begin,NULL);
  ret = stat(path, &statbuf);
  gettimeofday(&tv_end,NULL);

  assert(!ret);
  
  return tv_end.tv_usec - tv_begin.tv_usec;
}

/* ./path_walk <path> */
int main(int argc, char **argv) {
  char *path = argv[1];
  
  printf("LATENCY: %lu\n", path_walk_time(path));
  
  return 0;
}
