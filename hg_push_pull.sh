#!/bin/bash
export OLDPWD="`pwd`"
export NEWPWD="$( cd "$( dirname "${BASH_SOURCE[0]}")" && pwd )"


while [ 1 ]
do
        cd $NEWPWD
        git pull
#        git update
        cd $NEWPWD/logicmoo.wiki
           git pull
#           git update
   echo "Hit CTRL+C $NEWPWD ${BASH_SOURCE[0]}";
   cd $OLDPWD
   sleep 20;
done

cd $OLDPWD
