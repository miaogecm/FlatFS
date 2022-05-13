#!/usr/bin/python3

# ./mkfiles.py <fs_path> <width> <nfile_per_cpu>

import sys
import os
import multiprocessing

#nfile_per_cpu = 20000
nfile_per_cpu = int(sys.argv[3])
ncpu = multiprocessing.cpu_count()

max_array = [0, 133, 1633, 16633, 166633]
#nfile_per_dir = 133     # 2
#nfile_per_dir = 1633     # 3
#nfile_per_dir = 16633   # 4
#nfile_per_dir = 166633  # 5
nfile_per_dir = max_array[int(sys.argv[2])]

width = 6
fs_path = sys.argv[1] + '/'
base_dirs = [fs_path + '/'.join(map(lambda x: str(x + 1).zfill(width), range(t))) for t in [1, 10, 20, 30, 40, 50]]

base_dir1 = base_dirs[0]
base_dir2 = base_dirs[1]
base_dir3 = base_dirs[2]
base_dir4 = base_dirs[3]
base_dir5 = base_dirs[4]
base_dir6 = base_dirs[5]

count = 0
cpu = 0

if __name__ == '__main__':
	os.makedirs(base_dir6)

	for i in range(0, nfile_per_dir):
		open(os.path.join(base_dir1, 'f' + str(i).zfill(width)), 'w')
		count += 1
		if count == nfile_per_cpu:
			count = 0
			cpu += 1
			if cpu == ncpu:
				cpu = 0
			os.sched_setaffinity(0, {cpu})
			print('running on cpu [%d]' %(cpu))

	for i in range(0, nfile_per_dir):
		open(os.path.join(base_dir2, 'f' + str(i).zfill(width)), 'w')
		count += 1
		if count == nfile_per_cpu:
			count = 0
			cpu += 1
			if cpu == ncpu:
				cpu = 0
			os.sched_setaffinity(0, {cpu})
			print('running on cpu [%d]' %(cpu))

	for i in range(0, nfile_per_dir):
		open(os.path.join(base_dir3, 'f' + str(i).zfill(width)), 'w')
		count += 1
		if count == nfile_per_cpu:
			count = 0
			cpu += 1
			if cpu == ncpu:
				cpu = 0
			os.sched_setaffinity(0, {cpu})
			print('running on cpu [%d]' %(cpu))

	for i in range(0, nfile_per_dir):
		open(os.path.join(base_dir4, 'f' + str(i).zfill(width)), 'w')
		count += 1
		if count == nfile_per_cpu:
			count = 0
			cpu += 1
			if cpu == ncpu:
				cpu = 0
			os.sched_setaffinity(0, {cpu})
			print('running on cpu [%d]' %(cpu))

	for i in range(0, nfile_per_dir):
		open(os.path.join(base_dir5, 'f' + str(i).zfill(width)), 'w')
		count += 1
		if count == nfile_per_cpu:
			count = 0
			cpu += 1
			if cpu == ncpu:
				cpu = 0
			os.sched_setaffinity(0, {cpu})
			print('running on cpu [%d]' %(cpu))

	for i in range(0, nfile_per_dir):
		open(os.path.join(base_dir6, 'f' + str(i).zfill(width)), 'w')
		count += 1
		if count == nfile_per_cpu:
			count = 0
			cpu += 1
			if cpu == ncpu:
				cpu = 0
			os.sched_setaffinity(0, {cpu})
			print('running on cpu [%d]' %(cpu))

