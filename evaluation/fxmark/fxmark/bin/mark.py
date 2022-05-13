#!/usr/bin/python3

import subprocess
import os

FXMARK_CORE = (32, 40, 48)
FXMARK_SETTINGS = [
#    ('MRPH', FXMARK_CORE, 3, '/mnt/ext4/test'),
    ('MRDM', FXMARK_CORE, 5, '/mnt/ext4/test'),
#    ('MWCM', FXMARK_CORE, 5, '/mnt/ext4/test'),
#    ('MWUM', FXMARK_CORE, 5, '/mnt/ext4/test'),
]

def exec_cmd(cmd, out=None):
    p = subprocess.Popen(cmd, shell=True, stdout=out, stderr=out)
    p.wait()
    return p

def run_fxmark(typ, ncore, duration, path):
    exec_cmd('sudo mkfs.ext4 /dev/pmem4')
    exec_cmd('sudo mount -t ext4 -o dax /dev/pmem4 /mnt/ext4', None)
    os.makedirs(path)
    cmd = './fxmark --type {} --duration {} --ncore {} --nbg 1 --root {}'.format(typ, duration, ncore, path)
    print(cmd)
    exec_cmd(cmd, None)
    exec_cmd('sudo umount /mnt/ext4', None)
    
def main():
    for typ, ncores, duration, path in FXMARK_SETTINGS:
        for ncore in ncores:
            print('Running...')
            run_fxmark(typ, ncore, duration, path)

if __name__ == '__main__':
    main()
