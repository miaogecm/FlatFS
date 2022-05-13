#!/usr/bin/python2.7

from tabulate import tabulate

all_fs = []

readdir_latencies = []
rmdir_latencies = []
cpdir_latencies = []
mvdir_latencies = []

with open('.data') as f:
    for line in f.readlines():
        fs, readdir_latency, rmdir_latency, cpdir_latency, mvdir_latency = line.strip().split()
        all_fs.append(fs)
        
        readdir_latencies.append(readdir_latency)
        rmdir_latencies.append(rmdir_latency)
        cpdir_latencies.append(cpdir_latency)
        mvdir_latencies.append(mvdir_latency)

table_header = [''] + all_fs
table_data = [
    ['readdir'] + readdir_latencies,
    ['rmdir'] + rmdir_latencies,
    ['cpdir'] + cpdir_latencies,
    ['mvdir'] + mvdir_latencies
]

print(tabulate(table_data, headers=table_header, tablefmt='grid'))
