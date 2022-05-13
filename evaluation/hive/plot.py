#!/usr/bin/python2.7

from tabulate import tabulate

all_fs = []

latencies = []

with open('.data') as f:
    for line in f.readlines():
        fs, latency = line.strip().split()
        all_fs.append(fs)
        
        latencies.append(latency)

table_header = [''] + all_fs
table_data = [
    ['Total'] + latencies,
]

print(tabulate(table_data, headers=table_header, tablefmt='grid'))
