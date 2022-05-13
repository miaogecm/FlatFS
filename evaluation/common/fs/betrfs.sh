device=/dev/ram0

fs_mount() {
    export M=/mnt/betrfs
    mkdir -p $M
    echo "mount betrfs in $device at $M"
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
	mount -t ftfs /dev/loop0 /mnt/betrfs
}

fs_umount() {
    echo "umount betrfs in $device at $M"
    umount $M
}
