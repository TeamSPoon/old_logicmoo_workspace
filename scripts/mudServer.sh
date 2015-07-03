#!/bin/bash

#./clone_externals.sh 2>/dev/null
./pack/hMUD/policyd

export OLDPWD="`pwd`"
export NEWPWD="$( cd "$( dirname "${BASH_SOURCE[0]}" )"/../runtime && pwd )"
#export SWIPL=/usr/local/lib/swipl-7.1.11/bin/x86_64-linux/swipl
export RUNFILE="run_mud_server.pl"
if [ $# -eq 0 ] 
 then
    echo "No arguments supplied"
 else
    export RUNFILE="$1"
fi

while [ 1 ]
do
        echo "You should not see this ever";
	cd $OLDPWD
        reset -w
	echo -ne '\033]50;ClearScrollback\a'
	echo -en "\ec\e[3J"
	echo "Hit CTRL+C ${BASH_SOURCE[0]} ";
        echo ". ./debug_once.sh ${RUNFILE} ";
        cd $NEWPWD
        git pull
        if [[ $EUID -eq 0 ]];
          then
	     sudo su -p -l -s $SHELL -c ". ./debug_once.sh $RUNFILE" prologmud
          else
             . ./debug_once.sh $RUNFILE
        fi
        cd $NEWPWD
#        . ./commit_push.sh 
        sleep 4;
done

