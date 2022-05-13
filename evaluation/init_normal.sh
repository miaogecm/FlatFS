#!/bin/bash

. common/color.sh

echo -e "${COLOR_GREEN}init FlatFS evaluation${COLOR_CLEAR}"

export KERNEL_PATH=$(pwd)/../linux-4.15

echo -e "${COLOR_GREEN}creating mountpoint directories...${COLOR_CLEAR}"
mkdir -p /mnt/ext4
mkdir -p /mnt/pmfs
mkdir -p /mnt/nova
mkdir -p /mnt/xfs
mkdir -p /mnt/betrfs
mkdir -p /mnt/flatfs

pushd ../linux-4.15/fs

echo -e "${COLOR_GREEN}loading PMFS module...${COLOR_CLEAR}"
cd pmfs
make -j
insmod pmfs.ko
cd -

echo -e "${COLOR_GREEN}loading NOVA module...${COLOR_CLEAR}"
cd nova
make -j
insmod nova.ko
cd -

echo -e "${COLOR_GREEN}available FS:${COLOR_CLEAR}"
cat /proc/filesystems

popd

echo -e "${COLOR_GREEN}loading wbinvd module...${COLOR_CLEAR}"
cd common/tools/wbinvd
make -j
insmod wbinvd.ko
cd -

echo -e "${COLOR_GREEN}building tools...${COLOR_CLEAR}"
cd common/tools
make clean
make -j
cd -

echo -e "${COLOR_GREEN}turning off ASLR...${COLOR_CLEAR}"
echo 0 > /proc/sys/kernel/randomize_va_space

echo -e "${COLOR_GREEN}building fxmark...${COLOR_CLEAR}"
cd fxmark/fxmark
make clean
make -j
cd -

echo -e "${COLOR_GREEN}Done.${COLOR_CLEAR}"
