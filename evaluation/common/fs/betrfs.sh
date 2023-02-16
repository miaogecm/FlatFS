device=/dev/ram0

fs_mount() {
    export M=/mnt/betrfs
    echo "mount betrfs in $device at $M"
    rm -rf $M/*
}

fs_umount() {
    echo "umount betrfs in $device at $M"
}
