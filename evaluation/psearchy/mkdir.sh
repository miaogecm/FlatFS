#!/bin/bash

ncpu=`cat /proc/cpuinfo | grep "processor" | wc -l`
db=$1/db/

mkdir -p ${db}

for((i=0;i<${ncpu};i++))
do
	mkdir ${db}${i}	
done
