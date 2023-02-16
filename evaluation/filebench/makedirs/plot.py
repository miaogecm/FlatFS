#!/usr/bin/python2.7

import numpy as np
import matplotlib.pyplot as plt
import csv

xs = [1,2,3,4,5,6,7]

default = [0, 0, 0, 0, 0, 0, 0]
data = {
    'ext4': [79.811,226.049,245.003,248.248,242.905,244.662,243.999],
    'xfs': [49.120,242.965,305.899,357.101,354.551,362.914,364.203],
    'nova': [54.537,369.452,522.459,422.255,426.062,425.718,426.630],
    'pmfs': [73.431,288.021,380.945,418.236,389.756,382.738,378.760],
    'betrfs': [13.687,29.170,27.559,35.340,34.072,34.609,34.118],
    'vfs_opt': [85.815,319.116,335.359,320.382,310.556,305.175,300.897],
    'flatfs': [102.706,500.184,647.005,668.920,670.710,681.025,680.261],
    'flatfsp': default
}
with open('.data') as f:
    for line in f.readlines():
        fs, nthread, thruput = line.strip().split()
        if data[fs] == default:
            data[fs] = []
        data[fs].append(float(thruput) / 1000)

ext4 = data['ext4']
xfs = data['xfs']
nova = data['nova']
pmfs = data['pmfs']
betrfs = data['betrfs']
vfs_opt = data['vfs_opt']
flatfs = data['flatfs']
flatfsp = data['flatfsp']

plt.plot(xs, ext4, markerfacecolor='none',color='black', marker='s', markersize=10, linestyle='-', linewidth=1, label='Ext4')
plt.plot(xs, xfs, markerfacecolor='none',color='black', marker='^', markersize=10, linestyle='-', linewidth=1, label='XFS')
plt.plot(xs, nova, markerfacecolor='none',color='black', marker='x', markersize=10, linestyle='-', linewidth=1, label='NOVA')
plt.plot(xs, pmfs, markerfacecolor='none',color='black', marker='D', markersize=10, linestyle='-', linewidth=1, label='PMFS')
plt.plot(xs, betrfs, markerfacecolor='none',color='black', marker='<', markersize=10, linestyle='-', linewidth=1, label='BetrFS')
plt.plot(xs, vfs_opt, markerfacecolor='none',color='black', marker='>', markersize=10, linestyle='-', linewidth=1, label='VFS-opt')
plt.plot(xs, flatfs, markerfacecolor='none',color='black', marker='o', markersize=10, linestyle='-', linewidth=1, label='FlatFS')
plt.plot(xs, flatfsp, markerfacecolor='none',color='red', marker='o', markersize=10, linestyle='-', linewidth=1, label='FlatFS+')

font = {'size': '20','fontname': 'Times New Roman'}
plt.xlabel("Thread Number", font)
plt.ylabel("Throughput (K ops/s)", font)

font2 = {'size': '15','fontname': 'Times New Roman'}
ax = plt.gca()
xtick=[1,2,3,4,5,6,7]
ax.set_xticks(xtick)
xlabel=[1,8,16,24,32,40,48]
ax.set_xticklabels(xlabel, font2)

ytick=[0,200,400,600,800,1000]
ax.set_yticks(ytick)
ax.set_yticklabels(ytick, font2)

plt.legend(loc = 2, ncol=3, frameon=False, mode="expand", prop={'size': 14, 'family': 'Times New Roman'},handletextpad=0.2)
plt.show()
