#!/usr/bin/python

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.ticker import MaxNLocator
from collections import namedtuple
from matplotlib import ticker

n_groups = 2

default = [0, 0]
data = {
    'ext4': default,
    'xfs': default,
    'nova': default,
    'pmfs': default,
    'betrfs': default,
    'vfs_opt': default,
    'flatfs': default,
    'flatfs_opt': default,
}
with open('.data') as f:
    for line in f.readlines():
        fs, status_latency, commit_latency = line.strip().split()
        data[fs] = [float(status_latency), float(commit_latency)] if fs != 'flatfs_opt' else [float(status_latency)]

ext4 =  data['ext4']
xfs =   data['xfs']
nova =  data['nova']
pmfs =  data['pmfs']
betrfs = data['betrfs']
vfs_opt = data['vfs_opt']
flatfs =  data['flatfs']
flatfs_opt =  data['flatfs_opt']

ax = plt.gca()

index = np.arange(n_groups)*3.4+0.1
bar_width = 0.35
line_width = 1

index2 = np.arange(1)*3+0.1

bar1 = ax.bar(index+bar_width*1.1, ext4, bar_width, linewidth=line_width, edgecolor='black', fill=False, hatch='++')
bar2 = ax.bar(index+bar_width*2.1, xfs, bar_width, linewidth=line_width, edgecolor='black', fill=False, hatch='xx')
bar3 = ax.bar(index+bar_width*3.1, pmfs, bar_width, linewidth=line_width, edgecolor='black', fill=False, hatch='\\\\')
bar4 = ax.bar(index+bar_width*4.1, nova, bar_width, linewidth=line_width, edgecolor='black', fill=False, hatch='**')
bar5 = ax.bar(index+bar_width*5.1, betrfs, bar_width, linewidth=line_width, edgecolor='black', fill=False, hatch='---')
bar6 = ax.bar(index+bar_width*6.1, vfs_opt, bar_width, linewidth=line_width, edgecolor='black', fill=False, hatch='///')
bar7 = ax.bar(index+bar_width*7.1, flatfs, bar_width, linewidth=line_width, edgecolor='black', fill=False, hatch='')
bar7 = ax.bar(index2+bar_width*8.1, flatfs_opt, bar_width, linewidth=line_width, edgecolor='black', fill=False, hatch='...')

font1 = {'size': '20','fontname':'Times New Roman'}
ax.set_ylabel('Latency ($\mu$s)', font1)

ytick=[0,1.0,2.0,3.0,4.0]
ax.set_yticks(ytick)
font2 = {'size': '15','fontname':'Times New Roman'}
ax.set_yticklabels(ytick, font2)

xtick=np.arange(n_groups)
ax.set_xticks([1.7,4.9])
ax.set_xticklabels(['git status','git commit'], font2)

plt.xlim(0,6.5)

font3 = {'size': '9','fontname':'Times New Roman'}
x1=0.3

x1=3.7

# bbox_to_anchor (x, y, width, height)
ax.legend(('Ext4', 'XFS', 'PMFS', 'NOVA', 'BetrFS', 'VFS-opt', 'FlatFS', 'Git-opt+FlatFS'),
	bbox_to_anchor=(0.01, 0.99, 0.97, 0), loc=9, ncol=3, mode="expand", borderaxespad=0.,edgecolor='None',
	prop={'size': 14, 'family': 'Times New Roman'},handletextpad=0.2)

plt.show()
