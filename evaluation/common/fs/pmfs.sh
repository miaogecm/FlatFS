device=/dev/pmem12

fs_mount() {
    export M=/mnt/pmfs
    mkdir -p $M
    echo "mount pmfs in $device at $M"
    mount -t pmfs -o init $device $M
}

fs_umount() {
    echo "umount pmfs in $device at $M"
    umount $M
}
