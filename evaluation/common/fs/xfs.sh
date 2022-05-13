device=/dev/pmem12

fs_mount() {
    export M=/mnt/xfs
    mkdir -p $M
    echo "mount xfs in $device at $M"
    mkfs.xfs -f $device
    mount -t xfs -o dax $device $M
}

fs_umount() {
    echo "umount xfs in $device at $M"
    umount $M
}
