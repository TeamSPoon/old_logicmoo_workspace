#!/bin/bash
export OLDPWD="`pwd`"
export NEWPWD="$( cd "$( dirname "${BASH_SOURCE[0]}" )"/src && pwd )"   

cd $NEWPWD
swipl -f run_tests.pl
echo hopfully some files are between this message and 
find . -name "*.qlf"
echo this message
cd $OLDPWD
