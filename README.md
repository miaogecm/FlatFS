# FlatFS: Flatten Hierarchical File System Namespace on Non-volatile Memories

## Introduction

FlatFS is a metadata-optimized file system built for fast non-volatile memory systems. FlatFS features a flat file system namespace design and incorporates three core techniques: coordinated path walk model, range-optimized B^r tree, and write-optimized compressed index key. This artifact provides both FlatFS source code and scripts to reproduce the main experiment results.

## Building FlatFS

FlatFS is implemented based on Linux kernel 4.15.0 and requires Intel Optane DC persistent memory hardware. Following is the instructions of building FlatFS.

**Hardware requirements:** Intel Optane DC persistent memory

**OS version:** Ubuntu 18.04 or Ubuntu 14.04.6

**Kernel version:** Linux 4.15.0

#### 1. Download FlatFS

Please run `git clone https://github.com/miaogecm/FlatFS.git`. 

This repository contains FlatFS source code and our modified virtual file system to support coordinated path walk.

#### 2. Compile and Install FlatFS

Please run following commands as a root user:

`make localmodconfig`

`make menuconfig` to configure Linux kernel, make sure these following configurations are enabled.

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

Compile the kernel: `make -j 48`

Install the kernel: `make install`, `make modules_install`

Update grub and reboot the system: `update-grub`, `reboot`

#### 3. Mount FlatFS

Create a mount directory: `mkdir /mnt/flatfs`

Mount on `/dev/pmem0`: `mount -t flatfs -o init /dev/pmem0 /mnt/flatfs`

#### 4. Run FlatFS

The `evaluation/examples` directory contains several examples to run FlatFS. 

#### 5. Umount FlatFS

please run `umount /mnt/flatfs`.

## Authors

Hohai University

Miao Cai, mcai@hhu.edu.cn

Junru Shen, gnu_emacs@hhu.edu.cn
