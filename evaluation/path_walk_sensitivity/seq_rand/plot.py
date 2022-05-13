#!/usr/bin/python2.7

from tabulate import tabulate

data = {}
with open('.data') as f:
    for line in f.readlines():
        fs, h, depth, seq, latency = line.strip().split()
        data[(fs, int(h), int(depth), int(seq))] = float(latency) / 1000

table_header = ['System', 'VFS(H=3,L=20)', 'FlatFS(H=3,L=20)', 'VFS(H=4,L=20)', 'FlatFS(H=4,L=20)', 'VFS(H=5,L=20)', 'FlatFS(H=5,L=20)']
table_data = [
    ('Seq.', data['vfs', 3, 2, 1], data['flatfs', 3, 2, 1], data['vfs', 4, 2, 1], data['flatfs', 4, 2, 1], data['vfs', 5, 2, 1], data['flatfs', 5, 2, 1]),
    ('Rnd.', data['vfs', 3, 2, 0], data['flatfs', 3, 2, 0], data['vfs', 4, 2, 0], data['flatfs', 4, 2, 0], data['vfs', 5, 2, 0], data['flatfs', 5, 2, 0])
]

print(tabulate(table_data, headers=table_header, tablefmt='grid'))
