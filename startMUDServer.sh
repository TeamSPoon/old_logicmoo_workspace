#!/bin/bash
if [ $UID -eq 1 ]; then
  exec su --preserve-environment  --login   --session-command "$0 $@" prologmud
  exit 0
fi

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

if [ -z ${STANFORD_JAR+x} ]; then export STANFORD_JAR="${DIR}/pack/logicmoo_nlu/prolog/stanford-corenlp3.5.2-ALL.jar"; fi
if [ -z ${JAVA_HOME+x} ]; then export JAVA_HOME=`find /usr -name java-8-oracle -printf "%p/jre"`; fi

if [ -z `echo $LD_LIBRARY_PATH | grep 'java'` ]; then
  export LD_LIBRARY_PATH="${JAVA_HOME}/lib/amd64/server:${JAVA_HOME}/lib/amd64:${JAVA_HOME}/bin:${PATH}:${LD_LIBRARY_PATH}"
fi

if [! -f $STANFORD_JAR]; then 
  wget http://prologmoo.com/downloads/stanford-corenlp3.5.2-ALL.jar -O $STANFORD_JAR
fi

($DIR/pack/hMUD/policyd)


if [ $# -eq 0 ] 
 then
    echo "No arguments supplied"
    export RUNFILE="runtime/run_mud_server.pl"
 else
    export RUNFILE="$1"
fi

while [ 1 ]
do
  echo "You should not see this ever";
# cd $OLDPWD
   reset -w
	echo -ne '\033]50;ClearScrollback\a'
	echo -en "\ec\e[3J"
   reset -w
   echo "JAVA_HOME='$JAVA_HOME'"
   echo "LD_LIBRARY_PATH='$LD_LIBRARY_PATH'"
   echo "STANFORD_JAR='$STANFORD_JAR'"      
   echo "This ($0 $@) will be run from user $UID"
   echo "Hit CTRL+C ${BASH_SOURCE[0]} ";
   sleep 4;
#        cd $NEWPWD
#        git pull
        if [[ $EUID -eq 1 ]];
          then
             exit 0
	          (sudo su -l -s $SHELL -c "(cd $DIR ; swipl ${RUNFILE})" prologmud )
          else             
             (cd $DIR ; exec swipl $RUNFILE)             
         fi   
#        cd $NEWPWD
if [ $# -eq 0 ]
    then
  exit 0
fi
#        . ./commit_push.sh   
done

