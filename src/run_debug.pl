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

% [Optional] Testing PTTP
:-is_startup_file('run_debug.pl')->doall(do_pttp_tests);true.


% [Manditory] This loads the game and initializes so test can be ran
:- if_flag_true(was_run_dbg_pl, at_start(run_setup)).


% the real tests now (once)
now_run_local_tests_dbg :- doall(defined_local_test).
:- if_flag_true(was_run_dbg_pl,at_start(must_det(run_mud_tests))).

% the local tests each reload (once)
:- if_flag_true(was_run_dbg_pl, now_run_local_tests_dbg).

% [Optionaly] Tell the NPCs to do something every 30 seconds (instead of 90 seconds)
:- register_timer_thread(npc_ticker,30,npc_tick).

% nasty way i debuged parser
%:-repeat, trace, do_player_action('who'),fail.

% [Optionaly] Put a telnet client handler on the main console (nothing is executed past the next line)
:- if_flag_true(was_run_dbg_pl, at_start(run)).

% So scripted versions don't just exit
:- if_flag_true(was_run_dbg_pl,at_start(prolog)).

%:- kill_term_expansion.
%:- prolog.



/*

PTTP input formulas:
  1  firstOrder(motherOf,joe,sue).
  2  not_firstOrder(motherOf,_,A);firstOrder(female,A).
  3  not_firstOrder(sonOf,B,A);firstOrder(motherOf,A,B);firstOrder(fatherOf,A,B).
  4  query:-firstOrder(female,_).
PTTP to Prolog translation time: 0.0028555670000001143 seconds

Prolog compilation time: 0.0004133299999997675 seconds
2.
Proof time: 4.34149999994915e-5 seconds
Proof:
length = 2, depth = 1
Goal#  Wff#  Wff Instance
-----  ----  ------------
  [0]    4   query :- [1].
  [1]    2      firstOrder(female,sue) :- [2].
  [2]    1         firstOrder(motherOf,joe,sue).
Proof end.
%                    succceeded(prove_timed(logicmoo_example1,query))
%                do_pttp_test(logicmoo_example1_holds)

PTTP input formulas:
  1  firstOrder(motherOf,joe,sue).
  2  not_firstOrder(motherOf,_,A);firstOrder(female,A).
  3  not_firstOrder(sonOf,B,A);firstOrder(motherOf,A,B);firstOrder(fatherOf,A,B).
  4  query:-firstOrder(female,_).
PTTP to Prolog translation time: 0.0024834679999994336 seconds

Prolog compilation time: 0.00039567500000003974 seconds
2.
Proof time: 3.7734999999372576e-5 seconds
Proof:
length = 2, depth = 1
Goal#  Wff#  Wff Instance
-----  ----  ------------
  [0]    4   query :- [1].
  [1]    2      firstOrder(female,sue) :- [2].
  [2]    1         firstOrder(motherOf,joe,sue).
Proof end.
%                    succceeded(prove_timed(logicmoo_example1_holds,query))
%                do_pttp_test(logicmoo_example2)


*/

