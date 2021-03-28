#!/bin/bash

#pocet cisel 16 :)
numbers=16;


#preklad cpp zdrojaku
mpic++ --prefix /usr/local/share/OpenMPI -o pms pms.cpp


#vyrobeni souboru s random cisly
dd if=/dev/random bs=1 count=$numbers of=numbers status=none

count=5;

#spusteni
mpirun --prefix /usr/local/share/OpenMPI -np $count pms

#uklid
rm -f pms numbers
