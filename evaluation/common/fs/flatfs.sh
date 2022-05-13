device=/dev/pmem12

fs_mount() {
    export M=/mnt/flatfs
    mkdir -p $M
    echo "mount flatfs in $device at $M"
    echo "Be patient, it's slow due to block allcator initialization."
    mount -t flatfs -o init $device $M
}

fs_umount() {
    echo "umount flatfs in $device at $M"
    umount $M
}
