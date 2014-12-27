#!/bin/bash

# What the crazy large dev tree might look like 
export GITRC="--recursive"

if [ $# -eq 0 ] 
 then
    export MUDPACK=externals
 else
    export MUDPACK="$1"
fi

# must have at least
git clone $GITRC https://github.com/TeamSPoon/MUD_Examples games/
git clone $GITRC https://github.com/TeamSPoon/hMUD $MUDPACK/hMUD
sudo ln -s `pwd`/$MUDPACK/hMUD /var/www/html/hmud

# very usefull for seeing what is going on
git clone $GITRC https://github.com/TeamSPoon/ClioPatria $MUDPACK/ClioPatria
git clone $GITRC https://github.com/TeamSPoon/swish $MUDPACK/swish


