#!/bin/sh

umount /mnt/flatfs
rmmod flatfs
insmod flatfs.ko measure_timing=0

sleep 1

mount -t flatfs -o init /dev/pmem0 /mnt/flatfs
