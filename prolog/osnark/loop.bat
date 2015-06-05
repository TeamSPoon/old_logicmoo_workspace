@rem start mud script 

cd runtime

:startMUD

start /MAX /WAIT c:/pf/swipl/bin/swipl-win -L32G -G32G -T32G -f dbase_pfc.pl

CHOICE  /T 0 /C YN /CS /D Y /M  "RESTART MUD"

IF ERRORLEVEL 1 goto startMUD

