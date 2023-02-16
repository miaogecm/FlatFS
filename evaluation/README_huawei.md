# Artifact Evaluation

## Introduction

FlatFS is a metadata-optimized file system built for fast non-volatile memory systems. FlatFS features a flat file system namespace design and incorporates three core techniques: coordinated path walk model, range-optimized B^r tree, and write-optimized compressed index key. This artifact provides both FlatFS source code and scripts to reproduce the main experimental results.

## Experiment Reproducibility

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

Make sure there is one person running experiments at a time on our machine because performance interferences significantly impacts the results.

#### 5. Reproducing Experiments

The `/home/flatfs/flat-fs-github/evaluation/` directory include scripts to reproduce the main experiment results. BetrFS and VFS-opt run on old Linux kernels and have many issues running benchmarks and applications. Please follow instructions below to reproduce the main results:

##### 5.0 Initialization

+ Please run `evaluation/init_normal.sh`.

##### 5.1 Path Walk Efficiency

Change current directory into `evaluation/path_walk_efficiency.`

Run `./clean` to clean old data.

Run `/run $FS` to collect data for each file system, `$FS={ext4,xfs,pmfs,nova,flatfs}`.

Run `./plot.py` to draw the figure.

##### 5.2 Filebench

Change current directory into `evaluation/filebench/fileserver`.

Run `./clean` to clean old data.

Run `./run $FS` to collect data for each file system, `$FS={ext4,xfs,pmfs,nova,flatfs}`.

Run `./plot.py` to draw the figure.

##### 5.3 FxMark

Change current directory into `evaluation/fxmark`.

Run `./clean` to clean old data.

Run `./run $FS` to collect data for each file system, `$FS={ext4,xfs,pmfs,nova,flatfs}`.

Run `./plot.py` to draw the table.

##### 5.4 FIO

Change current directory into `evaluation/fio`.

Run `./run $FS` to collect data for each file system, `$FS={ext4,xfs,pmfs,nova,flatfs}`.

##### 5.5 Fs_mark

Change current directory into `evaluation/fs_mark`.

Run `./run $FS` to collect data for each file system, `$FS={ext4,xfs,pmfs,nova,flatfs}`.

