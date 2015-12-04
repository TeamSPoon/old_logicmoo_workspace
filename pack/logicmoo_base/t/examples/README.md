#!/bin/bash


for file in sanity/*pfc.pl
do

	swipl -f ${file} -g "halt(0)."
        export status=$?        
        if [ $status -ne 0 ]; then
         echo "command1 borked it ${status}"
	 exit $status
       fi
done
