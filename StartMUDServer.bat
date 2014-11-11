@rem start mud script 
:startMUD

start /WAIT runtime/run_debug.pl

CHOICE  /T 2 /C YN /CS /D Y /M  "RESTART MUD"

IF ERRORLEVEL 1 goto startMUD



