#!/usr/bin/python2.7

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
import matplotlib as mpl

xs = [1,2,3,4,5,6]

data = {}

with open('.data') as f:
    for line in f.readlines():
        cold_cache, h, depth, latency = line.strip().split()
        k = (int(h), int(cold_cache))
        if k not in data:
            data[k] = []
        data[k].append(float(latency) / 1000)

h2_cold = data[(2, 1)]
h3_cold = data[(3, 1)]
h4_cold = data[(4, 1)]
h5_cold = data[(5, 1)]
h2_hot = data[(2, 0)]
h3_hot = data[(3, 0)]
h4_hot = data[(4, 0)]
h5_hot = data[(5, 0)]

fig, ax1 = plt.subplots(figsize=(7, 6))
ax1 = plt.gca()

marker_size=12
p1 = ax1.plot(xs, h2_cold, markerfacecolor='none',color='black', marker='o', markersize=12, linestyle='-', linewidth=1)
p2 = ax1.plot(xs, h3_cold, markerfacecolor='none',color='black', marker='s', markersize=12, linestyle='-', linewidth=1)
p3 = ax1.plot(xs, h4_cold, markerfacecolor='none',color='black', marker='D', markersize=12, linestyle='-', linewidth=1)
p4 = ax1.plot(xs, h5_cold, markerfacecolor='none',color='black', marker='x', markersize=12, linestyle='-', linewidth=1)
p5 = ax1.plot(xs, h2_hot, markerfacecolor='none',color='black', marker='o', markersize=marker_size, linestyle=':', linewidth=1)
p6 = ax1.plot(xs, h3_hot, markerfacecolor='none',color='black', marker='s', markersize=marker_size, linestyle=':', linewidth=1)
p7 = ax1.plot(xs, h4_hot, markerfacecolor='none',color='black', marker='D', markersize=marker_size, linestyle=':', linewidth=1)
p8 = ax1.plot(xs, h5_hot, markerfacecolor='none',color='black', marker='x', markersize=marker_size, linestyle=':', linewidth=1)


font1 = {'size': '18','fontname': 'Times New Roman'}
font2 = {'size': '20','fontname': 'Times New Roman'}

ax1.set_xlabel('Path Component Number #', font2)
ax1.set_xlim(0.5,6.5)
xtick = [1,2,3,4,5,6]
ax1.set_xticks(xtick)
ax1.set_xticklabels([1,10,20,30,40,50], font1)

font = {'size': '20','fontname': 'Times New Roman'}
plt.ylabel("Syscall Latency ($\mu$s)", font)

ytick=[5,10,15,20,25]
ax1.set_yticks(ytick)
ax1.set_yticklabels(ytick, font1)

ax1.legend(['H=2, Cold LLC','H=3, Cold LLC','H=4, Cold LLC','H=5, Cold LLC','H=1, Hot LLC','H=2, Hot LLC','H=3, Hot LLC','H=4, Hot LLC','H=5, Hot LLC'], bbox_to_anchor=(-0.0, 0.88, 0.8, 0.14), loc = 2, mode="expand", ncol=2, frameon=False, prop={'size': 16, 'family': 'Times New Roman'},handletextpad=0.2)

plt.show()

#fig = plt.gcf()
#fig.set_figwidth(7)
#fig.set_figheight(6)
#fig.savefig('/home/miaogecm/Desktop/pw_sensitivity.pdf', dpi=1000)
