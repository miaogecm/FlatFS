#!/bin/bash

if [ -e /dev/ram1 ]; then
    umount /dev/ram0
    mount -t ext4 /dev/ram0 /mnt/ext4
fi

echo dropping caches...

sync

sleep 1s

echo 3 > /proc/sys/vm/drop_caches

sync

sleep 1s

echo drop caches OK
