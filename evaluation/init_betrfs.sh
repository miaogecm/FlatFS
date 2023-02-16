#!/bin/bash

. common/color.sh

echo -e "${COLOR_GREEN}init FlatFS evaluation${COLOR_CLEAR}"

echo -e "${COLOR_GREEN}creating mountpoint directories...${COLOR_CLEAR}"
mkdir -p /mnt/betrfs

echo -e "${COLOR_GREEN}building tools...${COLOR_CLEAR}"
cd common/tools
make clean
make -j
cd -

echo -e "${COLOR_GREEN}turning off ASLR...${COLOR_CLEAR}"
echo 0 > /proc/sys/kernel/randomize_va_space

echo -e "${COLOR_GREEN}creating ramdisk...${COLOR_CLEAR}"
modprobe brd rd_size=16384000 rd_nr=1 rd_numa_node=1

echo -e "${COLOR_GREEN}building fxmark...${COLOR_CLEAR}"
cd fxmark/fxmark
make clean
make -j
cd -

echo -e "${COLOR_GREEN}mounting BetrFS...${COLOR_CLEAR}"
device=/dev/ram0
mkfs.ext4 $device
mkdir /mnt/toku
mount -t ext4 $device /mnt/toku
rm -rf /mnt/toku/*
mkdir /mnt/toku/db
mkdir /mnt/toku/dev
touch /mnt/toku/dev/null
mkdir /mnt/toku/tmp
chmod 1777 /mnt/toku/tmp
umount /mnt/toku
modprobe zlib
insmod /home/flatfs/Desktop/betrfs-0.4.1/filesystem/ftfs.ko sb_dev=$device sb_fstype=ext4
touch dummy.dev
losetup /dev/loop0 dummy.dev
sleep 1s
mkdir -p /mnt/betrfs
mount -t ftfs /dev/loop0 /mnt/betrfs

echo -e "${COLOR_GREEN}Done.${COLOR_CLEAR}"
