#!/bin/bash
#cls ; swipl -T18G -L18G -T18G -s externals/MUD_ScriptEngines/snark/snark_in_prolog.pl
export OLDPWD="`pwd`"
export NEWPWD="$( cd "$( dirname "${BASH_SOURCE[0]}" )"/src && pwd )"
#export SWIPL=/usr/local/lib/swipl-7.1.11/bin/x86_64-linux/swipl
export RUNFILE="run_debug.pl"
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
        sleep 4;
        cd $NEWPWD
        ./debug_once.sh $RUNFILE
        cd $OLDPWD
done

