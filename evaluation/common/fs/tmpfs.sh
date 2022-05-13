fs_mount() {
    export M=/mnt/tmpfs
    mkdir -p $M
    echo "mount tmpfs at $M"
    mount -t tmpfs -o size=3G tmpfs $M
}

fs_umount() {
    echo "umount tmpfs at $M"
    umount $M
}
