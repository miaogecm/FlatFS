#!/usr/bin/python2.7

import numpy as np
import matplotlib.pyplot as plt
import csv

default = [0, 0, 0, 0, 0]
data = {
    'ext4': default,
    'xfs': default,
    'nova': default,
    'pmfs': default,
    'betrfs': default,
    'vfs_opt': default,
    'flatfs': default
}
with open('.data') as f:
    for line in f.readlines():
        fs, meandirwidth, thruput = line.strip().split()
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

n_groups = 5
ax = plt.gca()
index = np.arange(n_groups)*3.5+0.5
bar_width = 0.4
line_width = 1

bar1 = ax.bar(index+bar_width*1.1, ext4, bar_width, linewidth=line_width, edgecolor='black', fill=False, hatch='..')
bar2 = ax.bar(index+bar_width*2.1, xfs, bar_width, linewidth=line_width, edgecolor='black', fill=False, hatch='xx')
bar3 = ax.bar(index+bar_width*3.1, pmfs, bar_width, linewidth=line_width, edgecolor='black', fill=False, hatch='\\')
bar4 = ax.bar(index+bar_width*4.1, nova, bar_width, linewidth=line_width, edgecolor='black', fill=False, hatch='--')
bar5 = ax.bar(index+bar_width*5.1, betrfs, bar_width, linewidth=line_width, edgecolor='black', fill=False, hatch='')
bar6 = ax.bar(index+bar_width*6.1, vfs_opt, bar_width, linewidth=line_width, edgecolor='black', fill=False, hatch='**')
bar7 = ax.bar(index+bar_width*7.1, flatfs, bar_width, linewidth=line_width, edgecolor='black', fill=False, hatch='//')

font = {'size': '20','fontname': 'Times New Roman'}
plt.xlabel("Directory Depth", font)
plt.ylabel("Throughput (K ops/s)", font)

font2 = {'size': '15','fontname': 'Times New Roman'}
ax = plt.gca()
xtick=[2,5.6,9.2,12.8,16.4]
ax.set_xticks(xtick)
xlabel=[15.6,9.8,7.8,5.2,3.9]
ax.set_xticklabels(xlabel, font2)

ytick=[0,50,100,150,200,250,300,350]
ax.set_yticks(ytick)
ax.set_yticklabels(ytick, font2)

ax.legend(('Ext4', 'XFS', 'PMFS', 'NOVA', 'BetrFS', 'VFS-opt', 'FlatFS'),
	bbox_to_anchor=(0.02, 0.99, 0.97, 0), loc=9, ncol=3, mode="expand", borderaxespad=0.,edgecolor='None',
	prop={'size': 14, 'family': 'Times New Roman'},handletextpad=0.2)

plt.show()
