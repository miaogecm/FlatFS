#!/usr/bin/python

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.ticker import MaxNLocator
from collections import namedtuple
from matplotlib import ticker

n_groups = 6

default = [0, 0, 0, 0, 0, 0]
data = {
    'ext4': default,
    'xfs': default,
    'nova': default,
    'pmfs': default,
    'betrfs': default,
    'dcache': default,
    'flatfs': default
}
with open('.data') as f:
    for line in f.readlines():
        fs, cold_latency, hot_latency, dotdot_latency, dot_latency, symlink_latency, mntpoint_latency = line.strip().split()
        data[fs] = list(map(int, [cold_latency, hot_latency, dot_latency, dotdot_latency, symlink_latency, mntpoint_latency]))
        print(data)

# cold-dcache, hot-dcache, dot, dot-dot, symlink, mntpoint
#ext4 = [28.2,6.6,24.8,28.4,34.2,35.0]
#ext4 = [247.5,5.8,244.4,239.6,234.8,14.5]
ext4 = data['ext4']
#xfs = 	[117.8,6.6,118.4,128.2,78.6,12.8]
#xfs  = [117.2,6,119.7,125,121.1,15]
xfs = data['xfs']
#nova =  [20.2,7.8,20,23.4,24.6,18.4]
#nova = [222.9,6,231.3,217.6,232.1,15.6]
nova = data['nova']
#pmfs = 	[19.8,6.4,17.6,19.6,19.6,18.6]
#pmfs = [21.8,5.8,21.5,23.8,23.8,13.4]
pmfs = data['pmfs']
#betrfs = [105,4.8,153,38,33.8,5.2]
betrfs = data['betrfs']
#dcache = [172,3.0,130.6,171,163.4,3.0]
dcache = data['dcache']
#flatfs =  [18,4.8,6.4,5.8,18,13.2]
#flatfs = [8.8,6.7,8.7,8.4,12.7,15.2]
flatfs = data['flatfs']

fig, ax = plt.subplots()
fig.set_figwidth(10)
fig.set_figheight(4)

index = np.arange(n_groups)*0.9+0.1
index2 = np.arange(2)*0.8+0.1
bar_width = 0.1
line_width = 0.8

bar1 = ax.bar(index+bar_width*1.1, ext4, bar_width, linewidth=line_width, edgecolor='black', fill=False, hatch='..')
bar2 = ax.bar(index+bar_width*2.1, xfs, bar_width, linewidth=line_width, edgecolor='black', fill=False, hatch='\\\\')
bar3 = ax.bar(index+bar_width*3.1, nova, bar_width, linewidth=line_width, edgecolor='black', fill=False, hatch='++')
bar4 = ax.bar(index+bar_width*4.1, pmfs, bar_width, linewidth=line_width, edgecolor='black', fill=False, hatch='**')
bar5 = ax.bar(index+bar_width*5.1, betrfs, bar_width, linewidth=line_width, edgecolor='black', fill=False, hatch='---')
bar6 = ax.bar(index+bar_width*6.1, dcache, bar_width, linewidth=line_width, edgecolor='black', fill=False, hatch='xxx')
bar7 = ax.bar(index+bar_width*7.1, flatfs, bar_width, linewidth=line_width, edgecolor='black', fill=False, hatch='//')
#bar8 = ax.bar(index2+bar_width*8.1, flatfs_h, bar_width, linewidth=line_width, edgecolor='black', fill=False, hatch='')

font1 = {'size': '20','fontname':'Times New Roman'}
ax.set_ylabel('Latency ($\mu$s)', font1)
ytick=[0,50,100,150,200,250]
ax.set_yticks(ytick)
font2 = {'size': '14','fontname':'Times New Roman'}
ax.set_yticklabels(ytick, font2)

#formatter = ticker.ScalarFormatter(useMathText=True)
#formatter.set_scientific(True)
#formatter.set_powerlimits((-1,1))
#ax.yaxis.set_major_formatter(formatter)

xtick=np.arange(n_groups)
ax.set_xticks([])

plt.xlim(0,5.5)

font3 = {'size': '16','fontname':'Times New Roman'}
x1=0.5
y1=-15
ax.text(x1-0.4,y1,'cold-dcache',font3)
ax.text(x1+0.6,y1,'hot-dcache',font3)
ax.text(x1+1.7,y1,'dot',font3)
ax.text(x1+2.4,y1,'dot-dot',font3)
ax.text(x1+3.4,y1,'symlink',font3)
ax.text(x1+4.2,y1,'mntpoint',font3)

ax.spines['top'].set_visible(False)
ax.spines['right'].set_visible(False)
ax.spines['bottom'].set_linewidth(1)
ax.spines['left'].set_linewidth(1)
ax.yaxis.grid(True, color='grey', linewidth='0.2', linestyle='--')

# bbox_to_anchor (x, y, width, height)
ax.legend(('Ext4', 'XFS', 'NOVA', 'PMFS', 'BetrFS', 'VFS-opt', 'FlatFS'),
	bbox_to_anchor=(-0.13, 0.11, 1.13, 1), loc=1, ncol=7, mode="expand", borderaxespad=0.,edgecolor='None',
	prop={'size': 16, 'family': 'Times New Roman'},handletextpad=0.2)

#fig.tight_layout()
plt.show()
#plt.savefig('/home/miaogecm/Desktop/pw_effiency.pdf', dpi=fig.dpi)
