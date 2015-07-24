#!/bin/bash

command -v git >/dev/null 2>&1 || { echo >&2 "I require git but it's not installed.  Aborting."; exit 1; }

mkdir -p /opt
cd /opt
git clone --recursive  https://github.com/TeamSPoon/PrologMUD 
cd PrologMUD
git pull
git fetch --recurse-submodules
git submodule init
git submodule update
git submodule sync --recursive


( source ./scripts/preconfig.sh )


