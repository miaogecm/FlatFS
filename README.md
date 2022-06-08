# FlatFS: Flatten Hierarchical File System Namespace on Non-volatile Memories

## Introduction

FlatFS is a metadata-optimized file system built for fast non-volatile memory (NVM) systems. FlatFS features a flat file system namespace design and incorporates three core techniques: coordinated path walk model, range-optimized B^r tree, and write-optimized compressed index key. 

## Building FlatFS

**Hardware requirements:** Intel Optane DC persistent memory

**OS version:** Ubuntu 18.04 or Ubuntu 14.04.6

**Kernel version:** Linux 4.15.0

#### 1. Download FlatFS

`git clone https://github.com/miaogecm/FlatFS.git`. 

This repository contains FlatFS source code and our modified virtual file system to support coordinated path walk. FlatFS cannot be compiled as a kernel module. Its path walk model requires VFS supports.

#### 2. Compile and Install FlatFS

`make localmodconfig`

`make menuconfig` 

configure Linux kernel, make sure these two configurations are enabled:

```
File systems/DAX support
File systems/FlatFS
```

and disable these four configurations:

```
Security options/AppArmor support
Security options/Yama support
Security options/TOMOYO Linux support
Security options/Security hooks for pathname based access control
```

configure `FLATFS_NCPU` in `linux-4.15/include/linux/flatfs_define.h` as maximum CPU number in your system.

Compile the kernel: `make -j`

Install the kernel: `make install`, `make modules_install`

Update grub and reboot the system: `update-grub`, `reboot`

#### 3. Mount FlatFS

FlatFS can be mounted on a real or emulated NVM device.

create a mount directory: `mkdir /mnt/flatfs`

mount FlatFS: `mount -t flatfs -o init /dev/pmem0 /mnt/flatfs`

#### 4. Umount FlatFS

Please run `umount /mnt/flatfs`.

## Publication

Miao Cai, Junru Shen, Bin Tang, Hao Huang, Baoliu Ye. FlatFS: Flatten Hierarchical File System Namespace for Non-volatile Memories, *USENIX Annual Technical Conference (USENIX ATC)*, CARLSBAD, CA, USA. July 11-13, 2022.

## Limitations

Developing a mature file system requires lots of engineering efforts. Currently, FlatFS still has some potential bugs. If you meet an issue when running FlatFS, please contact us.

## Authors

Hohai University

Miao Cai, mcai@hhu.edu.cn

Junru Shen, gnu_emacs@hhu.edu.cn
