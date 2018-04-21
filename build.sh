#!/bin/bash
set -e

LPR=$1
if [[ $LPR == "" ]]
then
    LPR=bluetoothtest
fi

CONF=$2
if [[ $CONF == "" ]]
then
    CONF=RPI3
fi
case $CONF in
RPI)
    PROC=RPIB
    ARCH=ARMV6
    KERNEL=kernel.img
    ;;
RPI2)
    PROC=RPI2B
    ARCH=ARMV7a
    KERNEL=kernel7.img
    ;;
RPI3)
    PROC=RPI3B
    ARCH=ARMV7a
    KERNEL=kernel7.img
    ;;
esac

echo build.sh $LPR $CONF

ULTIBO=$HOME/ultibo/core
ULTIBOBIN=$ULTIBO/fpc/bin
REPO=bluetooth-dev
export PATH=$ULTIBOBIN:$PATH
for f in *.lpr

do
    ptop -l 1000 -i 1 -c ptop.cfg $f $f.formatted
    mv $f.formatted $f
done

rm -rf lib/ *.o
fpc -dBUILD_$CONF -B -O2 -Tultibo -Parm -Cp$ARCH -Wp$PROC -Fi$ULTIBO/source/rtl/ultibo/extras -Fi$ULTIBO/source/rtl/ultibo/core @$ULTIBOBIN/$CONF.CFG $LPR.lpr >& errors.log

mv $KERNEL $REPO-$LPR-kernel-$CONF.img
