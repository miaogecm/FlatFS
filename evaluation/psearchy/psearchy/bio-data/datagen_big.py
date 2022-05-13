#!/usr/bin/python3

import os
import multiprocessing

file = open("tiny_data_big.txt")
n = 0
filepath = os.getcwd()


def pre_path(path):
    tmp_ = path.rfind('/')
    return path[0:tmp_]


def next_path(path, add_path):
    return path + "/" + add_path


def get_b(input_line):
    tmp_cnt = 0
    for i_ in input_line:
        if i_ == ' ':
            tmp_cnt += 1
        else:
            return int(tmp_cnt / 2)


def remove_no_a_z(raw_line_):
    tmp_ = raw_line_.replace('/', ' ')
    tmp_ = tmp_.strip()
    for i_ in range(len(tmp_)):
        if not ('a' <= tmp_[i_] <= 'z' or 'A' <= tmp_[i_] <= 'Z' or ' ' == tmp_[i_]):
            if i_ != len(tmp_) - 1:
                tmp_ = tmp_[:i_] + 'a' + tmp_[i_ + 1:]
            else:
                tmp_ = tmp_[:i_] + 'a'
    return tmp_


if __name__ == '__main__':
    tot_cpu = multiprocessing.cpu_count()
    last_cnt = -1
    tmp = 0
    present_cpu = 0
    os.sched_setaffinity(0, {present_cpu})
    for line in file.readlines():
        raw_line = line
        cnt = get_b(line)
        line = remove_no_a_z(line)
        this_line = line[:min(8, line.find(" "))]

        if cnt < last_cnt:
            filepath = pre_path(filepath)
            for i in range(last_cnt - cnt):
                filepath = pre_path(filepath)
            filepath = next_path(filepath, this_line)
        elif cnt > last_cnt:
            filepath = next_path(filepath, this_line)
        else:
            filepath = pre_path(filepath)
            filepath = next_path(filepath, this_line)

        if raw_line[-10:-6] == '[spe':
            tmp_filepath = filepath
            ttmp = 0
            for ss in line.split():
                tmp_filepath += "/" + ss
                ttmp += 1
                if ttmp == 6:
                    break
            if not os.path.exists(tmp_filepath):
                os.makedirs(tmp_filepath)
            with open(tmp_filepath + "/inf.txt", "a") as f:
                f.write("\n" + raw_line.strip())

        last_cnt = cnt
        n += 1
        tmp += 1
        if tmp == 5000:
            tmp = 0
            present_cpu += 1
            if present_cpu == tot_cpu:
                present_cpu = 0
            os.sched_setaffinity(0, {present_cpu})
        print("\r%.2f" % (n / 56537*100) + '%', end='', flush=True)

