# Artifact Evaluation

## Introduction

FlatFS is a metadata-optimized file system built for fast non-volatile memory systems. FlatFS features a flat file system namespace design and incorporates three core techniques: coordinated path walk model, range-optimized B^r tree, and write-optimized compressed index key. This artifact provides both FlatFS source code and scripts to reproduce the main experimental results.

## Getting Started with FlatFS

**Hardware requirements:** Intel Optane DC persistent memory

**OS version:** Ubuntu 18.04 or Ubuntu 14.04.6

**Kernel version:** Linux 4.15.0

#### 1. Download FlatFS

`git clone https://github.com/miaogecm/FlatFS.git`. 

This repository contains FlatFS source code and our modified virtual file system to support coordinated path walk.

#### 2. Compile and Install FlatFS

`make localmodconfig`

`make menuconfig` 

configure Linux kernel, make sure these two configurations are enabled:

```
File systems/DAX support
File systems/FlatFS
```

disable these four configurations:

```
Security options/AppArmor support
Security options/Yama support
Security options/TOMOYO Linux support
Security options/Security hooks for pathname based access control
```

configure `FLATFS_NCPU` in `linux-4.15/include/linux/flatfs_define.h` as maximum CPU number in your system.

compile the kernel: `make -j 48`

install new kernel: `make install`, `make modules_install`

Update grub and reboot the system: `update-grub`, `reboot`

#### 3. Mount FlatFS

FlatFS can be mounted on a real or emulated NVM device.

create a mount directory: `mkdir /mnt/flatfs`

mount FlatFS: `mount -t flatfs -o init /dev/pmem0 /mnt/flatfs`


#### 4. Umount FlatFS

`umount /mnt/flatfs`

## Experiment Reproducibility

**Since different machines deliver different performance, we recommend using our server for the artifact evaluation to reproduce the main experimental results.** 

All experiments are conducted on a Dell PowerEdge R740 server machine. Following are hardware and software configurations:

#### 1. Hardware configuration

**CPU:** Two Intel Xeon Gold 5220R processors.

**Memory:**  Two memory nodes. Each has 96 GB (16 GB x 6) DRAM and 768 GB (128 GB x 6) Intel Optane DC PMM.

**Storage:** 512GB Kingston A400 Solid State Driver.

#### 2. Software configuration

**OS:** Ubuntu 14.04.6. 

**Ext4, XFS, PMFS, NOVA, FlatFS:** These file systems are implemented in Linux kernel 4.15.

#### 3. How to access

**VPN:** You should use a VPN client to access the machine. `evaluation/vpn/README` provides VPN connection instructions.

**Make sure there is one person running experiments at a time on our machine because performance interferences significantly impacts the results**.

#### 4. Environment Setup

We already set up the environment properly in our machine. If you want to build these file systems by yourself, please follow instructions in section 4.1-4.2 and setup Hive in section 4.3. If you just want to reproduce the experiment results, please skip to section 5.

##### 4.1 Update Grub

If you use Ubuntu 14.04.6, please re-install the grub to 2.02 or higher version. The old grub cannot recognize Optane memory correctly. You can download grub at https://ftp.gnu.org/gnu/grub/grub-2.02.tar.gz . The installation guide: https://www.gnu.org/software/grub/manual/grub/grub.html#Installation.

##### 4.2 Build FlatFS, NOVA, PMFS, Ext4, XFS

Download FlatFS repository at `https://github.com/miaogecm/FlatFS.git`. This repository also includes (1) NOVA; (2) PMFS; (3) Ext4; (4) XFS.

Instructions of building system are presented in **Getting Started with FlatFS** section. Also, make sure `CONFIG_PMFS=y, CONFIG_PMFS_XIF=y, CONFIG_NOVA_FS=y, CONFIG_XFS_FS=y`

After building, you should reboot the system and enter into Linux kernel 4.15 to run these file systems.

##### 4.3 Install Hive

Please see `evaluation/hive/hive.md`.

#### 5. Reproducing Experiments

The `/home/flatfs/flat-fs/evaluation/` directory include scripts to reproduce the main experiment results. BetrFS and VFS-opt run on old Linux kernels and have many issues running benchmarks and applications. Please follow instructions below to reproduce the main results:

##### 5.0 Initialization

Please run `evaluation/init_normal.sh`.

##### 5.1 Reproducing Figure 9

Change current directory into `evaluation/path_walk_efficiency.`

Run `./clean` to clean old data.

Run `/run $FS` to collect data for each file system, `$FS={ext4,xfs,pmfs,nova,flatfs}`.

Run `./plot.py` to draw the figure.

##### 5.2 Reproducing Figure 10(a)

Change current directory into `evaluation/path_walk_scalability.`

Run `./clean` to clean old data.

Run `./run $FS` to collect data for each file system, `$FS={ext4,xfs,pmfs,nova,flatfs}`.

Run `./plot.py` to draw the figure.

##### 5.3 Reproducing Figure 10(b)

Change current directory into `evaluation/path_walk_sensitivity/sensitity`.

Run `./clean` to clean old data.

Run `./run` to collect data for FlatFS file system.

Run `./plot.py` to draw the figure.

##### 5.4 Reproducing Table 2

Change current directory into `evaluation/path_walk_sensitivity/seq_rand`.

Run `./clean` to clean old data.

Run `./run $FS` to collect data for each file system, `$FS={vfs,flatfs}`.

Run `./plot.py` to draw the table.

##### 5.5 Reproducing Table 3

Change current directory into `evaluation/directory_range_operation`.

Run `./clean` to clean old data.

Run `./run $FS` to collect data for each file system, `$FS={ext4,xfs,pmfs,nova,flatfs}`.

Run `./plot.py` to draw the table.

##### 5.6 Reproducing Figure 11(a)

Change current directory into `evaluation/filebench/fileserver`.

Run `./clean` to clean old data.

Run `./run $FS` to collect data for each file system, `$FS={ext4,xfs,pmfs,nova,flatfs}`.

Run `./plot.py` to draw the figure.

##### 5.7 Reproducing Figure 11(b)

Change current directory into `evaluation/filebench/varmail`.

Run `./clean` to clean old data.

Run `./run $FS` to collect data for each file system, `$FS={ext4,xfs,pmfs,nova,flatfs}`.

Run `./plot.py` to draw the figure.

##### 5.8 Reproducing Table 4

Change current directory into `evaluation/fxmark`.

Run `./clean` to clean old data.

Run `./run $FS` to collect data for each file system, `$FS={ext4,xfs,pmfs,nova,flatfs}`.

Run `./plot.py` to draw the table.

##### 5.8 Reproducing Figure 12(a)

Change current directory into `evaluation/git`.

Run `./clean` to clean old data.

Run `./run $FS` to collect data for each file system, `$FS={ext4,xfs,pmfs,nova,flatfs,flatfs_opt}`.

Run `./plot.py` to draw the figure.

##### 5.9 Reproducing Figure 12(b)

Change current directory into `evaluation/psearchy`.

Run `./clean` to clean old data.

Run `./run $FS` to collect data for each file system, `$FS={ext4,xfs,pmfs,nova,flatfs}`.

Run `./plot.py` to draw the figure.

##### 5.10 Reproducing Table 5

Run `./clean` to clean old data.

Change current directory `evaluation/hive`.

Run `./run $FS` to collect data for each file system, `$FS={ext4,xfs,pmfs,nova,flatfs}`.

Run `./plot.py` to draw the table.
