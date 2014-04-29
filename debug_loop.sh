
#!/bin/bash
export OLDPWD="`pwd`"
export NEWPWD="$( cd "$( dirname "${BASH_SOURCE[0]}" )"/src && pwd )"
export SWIPL=/usr/local/lib/swipl-7.1.11/bin/x86_64-linux/swipl


while [ 1 ]
do
        echo 'Hit CTRL+C ${BASH_SOURCE[0]} ';
        cd $OLDPWD
           reset
        sleep 2;
        cd $NEWPWD
        $SWIPL -f run_debug.pl
        reset
done

