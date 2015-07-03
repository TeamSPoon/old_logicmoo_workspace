#!/bin/bash

if [ -z ${STANFORD_JAR+x} ]; then export STANFORD_JAR="pack/logicmoo_nlu/prolog/stanford-corenlp3.5.2-ALL.jar"; fi
export OLDPWD="`pwd`"
export NEWPWD="$( cd "$( dirname "${BASH_SOURCE[0]}" )"/runtime && pwd )"
export RUNFILE="run_mud_server.pl"

if [ -z ${JAVA_HOME+x} ]; then echo "JAVA_HOME is unset"; else echo "JAVA_HOME is set to '$JAVA_HOME'"; fi
if [ -z ${JAVA_HOME+x} ]; then export JAVA_HOME=`find /usr -name java-8-oracle -printf "%p/jre"`; fi
if [ -z ${JAVA_HOME+x} ]; then 
   echo "JAVA_HOME is STILL unset"; 
   sudo add-apt-repository -y ppa:webupd8team/java
   sudo apt-get update
   sudo apt-get install swi-prolog oracle-java8-installer
   echo select java 8
   sudo update-alternatives --config java
   export JAVA_HOME=`find /usr -name java-8-oracle -printf "%p/jre"`

else
   echo "JAVA_HOME is set to '$JAVA_HOME'";
fi

if [ `echo $LD_LIBRARY_PATH || grep -v 'java' ` ]; then
  export LD_LIBRARY_PATH="${JAVA_HOME}/lib/amd64/server:${JAVA_HOME}/lib/amd64:${JAVA_HOME}/bin:${PATH}:${LD_LIBRARY_PATH}"
fi

if [! -f $STANFORD_JAR]; then 
  wget http://prologmoo.com/downloads/stanford-corenlp3.5.2-ALL.jar -O $STANFORD_JAR
fi

./pack/hMUD/policyd

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
#        git pull
        if [[ $EUID -eq 0 ]];
          then
	          (sudo su -p -l -s $SHELL -c "(source ./debug_once.sh ${RUNFILE} )" prologmud )
          else
             (source ./debug_once.sh $RUNFILE)
        fi
        cd $NEWPWD
#        . ./commit_push.sh 
        sleep 4;
done

