#!/bin/bash
export OLDPWD="`pwd`"
export NEWPWD="$( cd "$( dirname "${BASH_SOURCE[0]}" )"/ && pwd )"
export LD_LIBRARY_PATH=/usr/lib/jvm/java-7-openjdk-amd64/jre/lib/amd64/server
#export SWIPL=/usr/local/lib/swipl-7.1.11/bin/x86_64-linux/swipl
export RUNFILE="run_debug.pl"
if [ $# -eq 0 ] 
 then
    echo "No arguments supplied"
 else
    export RUNFILE="$1"
fi

echo "You should not see this ever";
reset -w
echo -ne '\033]50;ClearScrollback\a'
echo -en "\ec\e[3J"
echo `pwd`
echo "Hit CTRL+C ${BASH_SOURCE[0]} $RUNFILE ";
sleep 1;
cd $NEWPWD
swipl -L32G -G32G -T32G -f $RUNFILE
cd $OLDPWD
echo exit $?
