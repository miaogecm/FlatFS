#!/usr/bin/python3

import os
import sys
import shutil
import multiprocessing

N_PER_CPU = 40000

def get_ncpu():
    return multiprocessing.cpu_count()

def bind_cpu(i):
    try:
        os.sched_setaffinity(0, {i})
        return True
    except OSError:
        return False

def get_total(src):
    return sum([len(f) + len(d) for r, d, f in os.walk(src)])

def cp(src, dst):
    for dirent in os.listdir(src):
        p = os.path.join(src, dirent)
        q = os.path.join(dst, dirent)
        if os.path.isdir(p):
            os.mkdir(q)
            yield
            yield from cp(p, q)
        else:
            shutil.copy(p, q)
            yield

def cp_per_cpu(src, dst):
    fop = cp(src, dst)
    total = get_total(src)
    done = 0
    cpu = -1
    ncpu = get_ncpu()

    print('{} files in total'.format(total))

    while True:
        if done % N_PER_CPU == 0:
            while True:
                cpu = (cpu + 1) % ncpu
                if bind_cpu(cpu):
                    break
                print('** cannot bind, skip cpu[{}]'.format(cpu))
            print('bind cpu[{}]'.format(cpu))

        done += 1
        print('\r%.2f' % (done * 100 / total) + '%', end='', flush=True)
        try:
            next(fop)
        except StopIteration:
            break

    print('\nDone.')

if __name__ == '__main__':
    cp_per_cpu(sys.argv[1], sys.argv[2])
