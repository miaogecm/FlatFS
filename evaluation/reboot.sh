#!/bin/bash

if [[ $1 == "normal" ]]; then
    kernel=/boot/vmlinuz-4.15.0
    initrd=/boot/initrd.img-4.15.0
elif [[ $1 == "betrfs" ]]; then
    kernel=/boot/vmlinuz-3.11.10
    initrd=/boot/initrd.img-3.11.10
elif [[ $1 == "vfs_opt" ]]; then
    kernel=/boot/vmlinuz-3.14.0
    initrd=/boot/initrd.img-3.14.0
else
    echo "Wrong option!"
    exit 1
fi

kexec -l $kernel --initrd=$initrd --command-line="$(cat /proc/cmdline)"
echo "Press enter to reboot..."
read
reboot
