#!/bin/bash

#pocet cisel bud zadam nebo 16 :)
if [ $# -lt 1 ];then
    numbers=16;
else
    numbers=$1;
fi;

#preklad cpp zdrojaku
mpic++ --prefix /usr/local/share/OpenMPI -o pms pms.cpp


#vyrobeni souboru s random cisly
dd if=/dev/random bs=1 count=$numbers of=numbers
numbers=`echo "scale=2 ; l($numbers) / l(2)" | bc -l`					#vypocet poctu procesoru
echo $numbers
numbers=`echo "(($numbers+0.5)/1)+1" | bc`						      #zaokrouhleni na cela cisla +1
echo $numbers

#spusteni
mpirun --prefix /usr/local/share/OpenMPI -np $numbers pms

#uklid
rm -f pms numbers
