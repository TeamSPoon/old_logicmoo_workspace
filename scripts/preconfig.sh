#!/bin/bash
export OLDPWD="`pwd`"
export NEWPWD="$( cd "$( dirname "${BASH_SOURCE[0]}" )"/.. && pwd )"   
export APTGET="sudo apt-get -o Acquire::ForceIPv4=true "

$APTGET install python-software-properties
sudo apt-add-repository -y ppa:swi-prolog/devel
sudo add-apt-repository -y ppa:webupd8team/java
$APTGET update
$APTGET install swi-prolog oracle-java8-installer

echo select java 8
sudo update-alternatives --config java


# 3,625 inferences, 6.003 CPU in 6.014 seconds (100% CPU, 604 Lips)
# 1,828,987,011 inferences, 316.932 CPU in 319.418 seconds (99% CPU, 5770916 Lips)
echo "Compiling a 1gb file this might take about 5 minutes after this it will only take 6 seconds to load"
swipl -g "time(load_files(['pack/pldata_larkc/prolog/el_holds/el_assertions'],[qcompile(auto),if_needed(true)])),halt."

if [ ! -d /var/www/html/hmud/ ]; then
 sudo ln -s `pwd`/pack/hMUD /var/www/html/hmud/
fi

# safe to run more than once
sudo adduser --gecos "PrologMUD User" --disabled-login --disabled-password prologmud

mkdir -p /opt/PrologMUD/runtime/cache
chmod 777 /opt/PrologMUD/runtime/cache
chmod 777 /opt/PrologMUD/runtime

echo "to start the MUD type: ./startMUDServer.sh

exit 0

if [0]; then

git clone --recurse-submodules
./clone_externals.sh 2>/dev/null

# What the crazy large dev tree might look like 
export GITRC="--recursive"

git submodule update 
git submodule sync 
#Synchronizing submodule url for 'mod'
git submodule update 
#man git-submodule  
git submodule update --rebase 
git submodule update
$ echo $?


if [ $# -eq 0 ] 
 then
    export MUDPACK=.
 else
    export MUDPACK="$1"
fi

# must have at least
git clone $GITRC https://github.com/TeamSPoon/hMUD $MUDPACK/hMUD
if [ ! -d /var/www/html/hmud/ ]; then
 sudo ln -s `pwd`/$MUDPACK/hMUD /var/www/html/hmud/
fi
sudo adduser --gecos "PrologMUD User" --disabled-login --disabled-password prologmud

# very usefull for seeing what is going on
git clone $GITRC https://github.com/TeamSPoon/ClioPatria $MUDPACK/ClioPatria
git clone $GITRC https://github.com/TeamSPoon/swish $MUDPACK/swish
git clone $GITRC https://github.com/TeamSPoon/trill_on_swish $MUDPACK/trill_on_swish
git clone $GITRC https://gitlab.prologmoo.com/prologmud_data/pldata_larkc.git $MUDPACK/pldata_larkc

exit 0

cd $NEWPWD
swipl -f run_tests.pl
echo hopfully some files are between this message and 
find . -name "*.qlf"
echo this message
cd $OLDPWD

fi 
