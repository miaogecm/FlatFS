device=/dev/pmem12

fs_mount() {
    export M=/mnt/nova
    mkdir -p $M
    echo "mount NOVA in $device at $M"
    mount -t NOVA -o init $device $M
}

fs_umount() {
    echo "umount NOVA in $device at $M"
    umount $M
}
