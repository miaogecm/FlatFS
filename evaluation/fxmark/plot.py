#!/usr/bin/python2.7

from tabulate import tabulate

all_fs = []

mwcm_thruputs = []
mwum_thruputs = []

with open('.data') as f:
    for line in f.readlines():
        fs, mwcm_thruput, mwum_thruput = line.strip().split()
        all_fs.append(fs)

        mwcm_thruputs.append(float(mwcm_thruput) / 1000)
        mwum_thruputs.append(float(mwum_thruput) / 1000)

table_header = [''] + all_fs
table_data = [
    ['MWCM'] + mwcm_thruputs,
    ['MWUM'] + mwum_thruputs,
]

print(tabulate(table_data, headers=table_header, tablefmt='grid'))
