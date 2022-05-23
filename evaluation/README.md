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

**OS:** Ubuntu 14.04.6. Because BetrFS and VFS-opt only works on Linux 3.11.10 and 3.14, these old kernels do not support new Ubuntu releases. For a fair comparison, we use Ubuntu 14.04.6 for all file systems in the experiment.

**Ext4, XFS, PMFS, NOVA, FlatFS:** These file systems are implemented in Linux kernel 4.15.

**BetrFS:** We use the latest released version 0.4.1 which works on Linux kernel 3.11.10. 

**VFS-opt:** Linux kernel 3.14. We mount a Ext4 file system on the RAM disk for VFS-opt.

#### 3. How to access

Make sure there is one person running experiments at a time on our machine because performance interferences significantly impacts the results.

#### 4. Environment Setup

We already set up the environment properly in our machine. If you want to build these file systems by yourself, please follow instructions in section 4.1-4.4 and setup Hive in section 4.5. If you just want to reproduce the experiment results, please skip to section 5.

##### 4.1 Update Grub

If you use Ubuntu 14.04.6, please re-install the grub to 2.02 or higher version. The old grub cannot recognize Optane memory correctly. You can download grub at https://ftp.gnu.org/gnu/grub/grub-2.02.tar.gz . The installation guide: https://www.gnu.org/software/grub/manual/grub/grub.html#Installation.

##### 4.2 Build FlatFS, NOVA, PMFS, Ext4, XFS

Download FlatFS repository at `https://github.com/miaogecm/FlatFS.git`. This repository also includes (1) NOVA; (2) PMFS; (3) Ext4; (4) XFS.

Instructions of building system are presented in **Getting Started with FlatFS** section. Also, make sure `CONFIG_PMFS=y, CONFIG_PMFS_XIF=y, CONFIG_NOVA_FS=y, CONFIG_XFS_FS=y`

After building, you should reboot the system and enter into Linux kernel 4.15 to run these file systems.

##### 4.3 Build BetrFS

Download BetrFS 0.4.1 from  `https://github.com/oscarlab/betrfs/archive/refs/tags/0.4.1.tar.gz`. Follow instructions in https://github.com/oscarlab/betrfs to build BetrFS. Enable ramdisk support `device driver --> block devices --> RAM block device support` in `make menuconfig`. After building, you should reboot the system and enter into Linux kernel 3.11.10 to run BetrFS.

We provide a useful script in `evaluation/common/fs/betrfs.sh` for mount BetrFS.

##### 4.4 Build VFS-opt

Please run `git clone https://github.com/oscarlab/dcache.git` to download VFS-opt source code.

Please run `make localmodconfig` , `make menuconfig`, enable ramdisk support `device driver --> block devices --> RAM block device support`.

Compile and install VFS-opt system `make -j 48`, `make modules_install`, `make install`.

After building, you should reboot the system and enter into Linux kernel 3.14 to run VFS-opt system.

##### 4.5 Install Hive

Please see `evaluation/hive/hive.md`.

##### 4.6 Summary

After everything setup, three kernels are installed for seven file systems: (1) Linux 4.15.0 for FlatFS, NOVA, PMFS, Ext4, and XFS; (2) Linux 3.11.10 for BetrFS; (3) Linux 3.14 for VFS-opt. You need to enter into the correct Linux kernel to run these file systems. Please wait until all NVM devices finish initialization during reboot. You can check persistent memory device initialization status in `dmesg`. Example output: `pmem12: detected capacity change from 0 to 135291469824`.

#### 5. Reproducing Experiments

The `/home/flatfs/flat-fs/evaluation/` directory include scripts to reproduce the main experiment results. Please follow instructions below to reproduce the results:

Seven file systems are installed on three different Linux kernel. You need to reboot the system at least two times for each experiment. Rebooting and initializing the server takes around 5-8 minutes. To save your time, we suggest that run each Linux kernel version at one time and collect all data for ten experiments, then reboot the system and switch to another Linux kernel.

For example, five file systems are installed on the kernel 4.15. You can run these five file systems with instructions from section 5.1 to 5.10. Then, reboot the machine, enter into kernel 3.11.10, and run BetrFS with instructions from section 5.1 to 5.10. Next, reboot the system and run VFS-opt system. All data will be saved in a `.data` file in the corresponding directory. Finally, run `./plot.py` scripts in the directory to generate all figures and tables.

VFS-opt and BetrFS running on old Linux kernels.  These two system have issues running benchmarks and applications, which causes kernel crashes or hangs. You may retry several times to obtain the results.

##### 5.0 Initialization

+ if kernel version is 4.15, please run `evaluation/init_normal.sh`.
+ if kernel version is 3.14, please run `evaluation/init_vfs_opt.sh`.
+ if kernel version is 3.11.10, please run `evaluation/init_betrfs.sh`.

##### 5.1 Reproducing Figure 9

Change current directory into `evaluation/path_walk_efficiency.`

Run `./clean` to clean old data.

Run `/run $FS` to collect data for each file system, `$FS={ext4,xfs,pmfs,nova,flatfs,betrfs,dcache}`.

Run `./plot.py` to draw the figure.

##### 5.2 Reproducing Figure 10(a)

Change current directory into `evaluation/path_walk_scalability.`

Run `./clean` to clean old data.

Run `./run $FS` to collect data for each file system, `$FS={ext4,xfs,pmfs,nova,flatfs,betrfs,vfs_opt}`.

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

Run `./run $FS` to collect data for each file system, `$FS={ext4,xfs,pmfs,nova,flatfs,betrfs,vfs_opt}`.

Run `./plot.py` to draw the table.

##### 5.6 Reproducing Figure 11(a)

Change current directory into `evaluation/filebench/fileserver`.

Run `./clean` to clean old data.

Run `./run $FS` to collect data for each file system, `$FS={ext4,xfs,pmfs,nova,flatfs,betrfs,vfs_opt}`.

Run `./plot.py` to draw the figure.

##### 5.7 Reproducing Figure 11(b)

Change current directory into `evaluation/filebench/varmail`.

Run `./clean` to clean old data.

Run `./run $FS` to collect data for each file system, `$FS={ext4,xfs,pmfs,nova,flatfs,betrfs,vfs_opt}`.

Run `./plot.py` to draw the figure.

##### 5.8 Reproducing Table 4

Change current directory into `evaluation/fxmark`.

Run `./clean` to clean old data.

Run `./run $FS` to collect data for each file system, `$FS={ext4,xfs,pmfs,nova,flatfs}`.

Run `./plot.py` to draw the table.

##### 5.8 Reproducing Figure 12(a)

Change current directory into `evaluation/git`.

Run `./setup` to prepare the data.

Run `./clean` to clean old data.

Run `./run $FS` to collect data for each file system, `$FS={ext4,xfs,pmfs,nova,flatfs,betrfs,vfs_opt,flatfs_opt}`.

Run `./plot.py` to draw the figure.

##### 5.9 Reproducing Figure 12(b)

Change current directory into `evaluation/psearchy`.

Run `./clean` to clean old data.

Run `./run $FS` to collect data for each file system, `$FS={ext4,xfs,pmfs,nova,flatfs,betrfs,vfs_opt}`.

Run `./plot.py` to draw the figure.

##### 5.10 Reproducing Table 5

Change current directory `evaluation/hive`.

Run `export TBL_PATH=~/hive/table` and `./mktable` to make tables.

Run `./clean` to clean old data.

Run `./run $FS` to collect data for each file system, `$FS={ext4,xfs,pmfs,nova,flatfs,betrfs,vfs_opt}`. You may encounter error messages like `ij> ERROR X0Y32: Jar file 'COMMONS_LANG' already exists in Schema 'APP'`. It's OK.

Run `./plot.py` to draw the table.
