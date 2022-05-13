#!/usr/bin/python

import os
import sys

dir=sys.argv[1]

print("generating path...")

f = open('.path', 'w+')

flist = []

for root, dirs, files in os.walk(dir):
    for file in files:
        path = root + '/' + file + '\n'
        flist.append(path)

for path in sorted(flist):
    f.write(path)

f.close()
