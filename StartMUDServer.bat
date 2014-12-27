@rem start mud script 

cd runtime

:startMUD

start /WAIT c:/pf/swipl/bin/swipl-win -L32G -G32G -T32G -f run_mud_server.pl

CHOICE  /T 2 /C YN /CS /D Y /M  "RESTART MUD"

IF ERRORLEVEL 1 goto startMUD



