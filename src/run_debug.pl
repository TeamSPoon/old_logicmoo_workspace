%!swipl -f
/** <module> An Implementation a MUD server in SWI-Prolog

*/


:- catch(guitracer,_,true).
:- set_prolog_flag(verbose_load,true).

:- debug.

:-include(run_tests).

% [Optionaly] re-define load_default_game
% load_default_game:- load_game(logicmoo('rooms/startrek.all.pl')).

% [Manditory] This loads the game and intializes
:- at_start(run_setup).

% [Optionaly] Tell the NPCs to do something every 30 seconds (instead of 90 seconds)
:- register_timer_thread(npc_ticker,30,npc_tick).

% [Optionaly] load and start sparql server
%:- at_start(start_servers).

:-repeat, trace, do_player_action('who'),fail.

% [Optionaly] Put a telnet client handler on the main console (nothing is executed past the next line)
:- at_start((debug,must_det(run))).

% So scripted versions don't just exit
:- at_start(prolog).




