
#!/bin/bash
export OLDPWD="`pwd`"
export NEWPWD="$( cd "$( dirname "${BASH_SOURCE[0]}" )"/src && pwd )"
#export SWIPL=/usr/local/lib/swipl-7.1.11/bin/x86_64-linux/swipl

        echo "Hit CTRL+C ${BASH_SOURCE[0]} ";
	echo -en "\ec\e[3J"
        sleep 3;
        swipl -f run_debug.pl

