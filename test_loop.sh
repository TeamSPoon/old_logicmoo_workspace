
#!/bin/bash
export OLDPWD="`pwd`"
export NEWPWD="$( cd "$( dirname "${BASH_SOURCE[0]}" )"/src_incoming && pwd )"
#export SWIPL=/usr/local/lib/swipl-7.1.11/bin/x86_64-linux/swipl


while [ 1 ]
do
        echo "Hit CTRL+C ${BASH_SOURCE[0]} ";
        cd $OLDPWD
           reset -w
	   echo -en "\ec\e[3J"
        sleep 3;
        cd $NEWPWD
        swipl -f run_tests.pl
        reset -w
done

