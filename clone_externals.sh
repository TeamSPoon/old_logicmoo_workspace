#!/bin/bash

# What the crazy large dev tree might look like 
export GITRC="--recursive"

if [ $# -eq 0 ] 
 then
    export MUDPACK=pack
 else
    export MUDPACK="$1"
fi

# must have at least
git clone $GITRC https://github.com/TeamSPoon/MUD_Examples games/
git clone $GITRC https://github.com/TeamSPoon/hMUD $MUDPACK/hMUD
if [ ! -d /var/www/html/hmud/ ]; then
 sudo ln -s `pwd`/$MUDPACK/hMUD /var/www/html/hmud/
fi
sudo adduser --gecos "PrologMUD User" --disabled-login --disabled-password prologmud

# very usefull for seeing what is going on
git clone $GITRC https://github.com/TeamSPoon/ClioPatria $MUDPACK/ClioPatria
git clone $GITRC https://github.com/TeamSPoon/swish $MUDPACK/swish


