#
# Makefile for the linux pmfs-filesystem routines.
#

obj-m += pmfs.o

pmfs-y := bbuild.o balloc.o dir.o file.o inode.o namei.o super.o symlink.o ioctl.o pmfs_stats.o journal.o xip.o wprotect.o

all:
	make -C $(KERNEL_PATH) M=`pwd`

clean:
	make -C $(KERNEL_PATH) M=`pwd` clean
