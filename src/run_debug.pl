%!swipl -f
/** <module> An Implementation a MUD server in SWI-Prolog

*/

% Was this our startup file?
was_run_dbg_pl:-is_startup_file('run_debug.pl').

:- catch(guitracer,_,true).
:- set_prolog_flag(verbose_load,true).

:- debug.

:-include(run_tests).

% [Optionaly] re-define load_default_game
% load_default_game:- load_game(logicmoo('rooms/startrek.all.pl')).

% [Optionaly] load and start sparql server
% starts in forground
%:- at_start(slow_work).
% starts in thread (the the above was commented out)
%:- at_start(start_servers).
% commented out except on run

start_boxer:-!.
start_boxer:-
   threads,
   ensure_loaded(logicmoo(candc/parser_boxer)),
   % make,
   fmt("Press Ctrl-D to start the mud!"),
   at_start(prolog).

% [Optional] This loads boxer
:- at_start(with_assertions(moo:prevent_transform_moo_preds,within_user(ignore(catch(start_boxer,_,true))))).

% [Manditory] This loads the game and initializes so test can be ran
:- if_flag_true(was_run_dbg_pl, at_start(run_setup)).

% the real tests now (once)
now_run_local_tests:- doall(defined_local_test).
:- if_flag_true(was_run_dbg_pl,at_start(must_det(run_mud_tests))).

% the local tests each reload (once)
:- if_flag_true(was_run_dbg_pl, now_run_local_tests).

% [Optionaly] Tell the NPCs to do something every 30 seconds (instead of 90 seconds)
:- register_timer_thread(npc_ticker,30,npc_tick).

%:-repeat, trace, do_player_action('who'),fail.

% [Optionaly] Put a telnet client handler on the main console (nothing is executed past the next line)
:- if_flag_true(was_run_dbg_pl, at_start(run)).

% So scripted versions don't just exit
:- if_flag_true(was_run_dbg_pl,at_start(prolog)).





