device=/dev/ram0

fs_mount() {
    export M=/mnt/ext4
    mkdir -p $M
    echo "mount ext4 for VFS-opt in $device at $M"
    mkfs.ext4 $device
    mount -t ext4 $device $M
}

fs_umount() {
    echo "umount ext4 in $device at $M"
    umount $M
}
