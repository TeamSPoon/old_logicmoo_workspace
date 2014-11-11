%!swipl -f
/** <module> An Implementation a MUD server in SWI-Prolog

*/


:-redefine_system_predicate(system:halt).
:-abolish(system:halt,0).
system:halt:- format('the halting problem is now solved!').

:-ensure_loaded(launchcliopatria).
:-include(run_common).

:- set_no_debug,set_prolog_flag(report_error,false).

% [Optionaly] load and start sparql server
:- at_start(nodebugx(start_servers)).

% [Optionaly] re-define load_default_game
:- declare_load_game(logicmoo('rooms/startrek.all.plmoo')).
load_default_game:- load_game(logicmoo('rooms/startrek.all.plmoo')).

% [Manditory] This loads the game and intializes
:- at_start(nodebugx(run_setup)).

% [Optionaly] Tell the NPCs to do something every 30 seconds (instead of 90 seconds)
:- register_timer_thread(npc_ticker,30,nodebugx(npc_tick)).

% [Optionaly] Put a telnet client handler on the main console (nothing is executed past the next line)
:- at_start(nodebugx(run)).

% So scripted versions don't just exit
:- at_start(prolog).




