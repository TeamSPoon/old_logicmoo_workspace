#!/bin/bash
#cls ; swipl -T18G -L18G -T18G -s externals/MUD_ScriptEngines/snark/snark_in_prolog.pl
#export OLDPWD="`pwd`"
#export NEWPWD="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
#export SWIPL=/usr/local/lib/swipl-7.1.11/bin/x86_64-linux/swipl
#export RUNFILE="run_debug.pl"
if [ $# -eq 0 ] 
 then
    echo "No arguments supplied"
   #gdb -return-child-result -ex set pagination off -ex run --args rlwrap -a -A -r -c -N -r swipl -l init_mud_server.pl
    export RUNFILE="rlwrap -a -A -r -c -N -r sudo -u mud swipl -l init_mud_server.pl"
 else
    export RUNFILE="rlwrap -a -A -r -c -N -r ${*}"
fi

while [ 1 ]
do
        echo "You should not see this ever";
        #cd $OLDPWD
        reset -w
        killall -9 xterm perl
	echo -ne '\033]50;ClearScrollback\a'
	echo -en "\ec\e[3J"
	echo "Hit CTRL+C ${BASH_SOURCE[0]} ";
        killall -9 swipl
        echo gdb -return-child-result -ex "set pagination off" -ex run -ex quit --args $RUNFILE
        sleep 4;
        #cd $NEWPWD
	
	#rlwrap -A -r gdbexec swipl -l daydream.pl
        gdb -return-child-result -ex "set pagination off" -ex run -ex quit --args $RUNFILE
	#cd $OLDPWD
done

