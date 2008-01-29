#!/bin/sh
NAME=$1
EXT=`date +%F`
CWD=`pwd`
i=1
while [ -f ../$NAME-$EXT-$i.tar.gz ]; 
do
		i=`expr $i + 1`
done
(cd ../; tar cvf $NAME-$EXT-$i.tar `basename $CWD` ;gzip -9 $NAME-$EXT-$i.tar)
echo "$NAME-$EXT-$i.tar.gz"
