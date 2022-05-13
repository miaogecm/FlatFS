#!/usr/bin/python2.7

import numpy as np
import matplotlib.pyplot as plt
import csv

xs = [1,2,3,4,5,6]

default = [0, 0, 0, 0, 0, 0]
data = {
    'ext4': default,
    'xfs': default,
    'nova': default,
    'nova_hot_dcache': default,
    'pmfs': default,
    'betrfs': default,
    'vfs_opt': default,
    'flatfs': default
}
with open('.data') as f:
    for line in f.readlines():
        arr = line.strip().split()
        data[arr[0]] = list(map(int, arr[1:]))

#xfs = [11.4,54.4,98.6,123.8,150.8,171.25]
#xfs = [11.6,61.2,109.1,122.5,152.7,161.4]
xfs = data['xfs']
#ext4 = [8.2,26.4,58.6,93.0,97.0,122.8]
#ext4 = [7.7,27.3,60.3,103.3,148.5,190.5]
ext4 = data['ext4']
#pmfs = [6.2,17.4,29,42.6,51.4,60.6]
#pmfs = [6.6,19.3,30.3,44.9,49.7,69.7]
pmfs = data['pmfs']
#nova = [8.5,34.6,57.2,80.0,110.8,138.6]
#nova = [8.9,30.2,61.6,83.7,113.2,133.3]
nova = data['nova']
#nova_hot_dcache = [2.8,4.6,5.6,7.2,9.6,10.8]
nova_hot_dcache = data['nova_hot_dcache']
#betrfs = [27,70,116,176,273,344]
betrfs = data['betrfs']
#vfs_opt = [2,3,3,3,3,3]
vfs_opt = data['vfs_opt']
#flatfs = [4.6,4.8,4.8,5.0,5.2,6.2]
flatfs = data['flatfs']

marker_size = 18

p1=plt.plot(xs, xfs, markerfacecolor='none',color='black', marker='*', markersize=marker_size, linestyle='-', linewidth=1)
p2=plt.plot(xs, ext4, markerfacecolor='none',color='black', marker='^', markersize=marker_size, linestyle='-', linewidth=1)
p3=plt.plot(xs, pmfs, markerfacecolor='none',color='black', marker='<', markersize=marker_size, linestyle='-', linewidth=1)
p4=plt.plot(xs, nova, markerfacecolor='none',color='black', marker='D', markersize=marker_size, linestyle='-', linewidth=1)
p5=plt.plot(xs, betrfs, markerfacecolor='none',color='black', marker='>', markersize=marker_size, linestyle='-', linewidth=1)
p6=plt.plot(xs, vfs_opt, markerfacecolor='none',color='black', marker='x', markersize=marker_size, linestyle='-', linewidth=1)
p7=plt.plot(xs, nova_hot_dcache, markerfacecolor='none',color='black', marker='s', markersize=marker_size, linestyle='-', linewidth=1)
p8=plt.plot(xs, flatfs, markerfacecolor='none',color='black', marker='o', markersize=marker_size, linestyle='-', linewidth=1)

font = {'size': '20','fontname': 'Times New Roman'}
plt.xlabel("Path Component Number #", font)
plt.ylabel("Syscall Latency ($\mu$s)", font)

font2 = {'size': '20','fontname': 'Times New Roman'}
ax = plt.gca()
ax.set_xlim(0.5,6.5)
xtick=[1,2,3,4,5,6]
ax.set_xticks(xtick)
xlabel=[1,10,20,30,40,50]
ax.set_xticklabels(xlabel, font2)

ytick=[0,50,100,150,200,250,300,350]
ax.set_yticks(ytick)
ax.set_yticklabels(ytick, font2)

plt.legend(['XFS','Ext4','PMFS','NOVA','BetrFS','VFS-opt','Hot-dcache','FlatFS'], bbox_to_anchor=(-0.0, 0.88, 0.78, 0.14), loc = 2, mode="expand", ncol=2, frameon=False, prop={'size': 20, 'family': 'Times New Roman'})

plt.show()

#fig = plt.gcf()
#fig.set_figwidth(7)
#fig.set_figheight(6)
#fig.savefig('/home/miaogecm/Desktop/pw_scalability.pdf', dpi=1000)
