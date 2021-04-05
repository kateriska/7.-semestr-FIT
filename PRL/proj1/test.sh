#!/bin/bash

# count of numbers of input sequence is fixed to 16
numbers=16;

if [ "$(hostname)" = "merlin.fit.vutbr.cz" ]; then
    mpic++ --prefix /usr/local/share/OpenMPI -o pms pms.cpp
else
    mpic++ -o pms pms.cpp
fi

# file of random 16 numbers
dd if=/dev/random bs=1 count=$numbers of=numbers status=none

count=5; # right count of processors based on p(n) = log_2(n) + 1 = log_2(16) + 1

# run properly on local Ubuntu or merlin
if [ "$(hostname)" = "merlin.fit.vutbr.cz" ]; then
    mpirun --prefix /usr/local/share/OpenMPI -np $count pms
else
    mpirun -np $count pms
fi

rm -f pms numbers
