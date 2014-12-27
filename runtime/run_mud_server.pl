%!swipl -f
/** <module> An Implementation a MUD server in SWI-Prolog

*/

:-redefine_system_predicate(system:halt).
:-abolish(system:halt,0).
system:halt:- format('the halting problem is now solved!').

:- '@'(ensure_loaded('../src_lib/logicmoo_util/logicmoo_util_all'),user).
:- set_prolog_flag(gc,true),set_prolog_flag(debug,false),set_prolog_flag(last_call_optimisation,true),set_prolog_flag(optimise,true).

swi_module(M,E):-dmsg(swi_module(M,E)).
swi_export(_):-!.
swi_export(E):-dmsg(swi_export(E)).

% :- unsetenv('DISPLAY').
:- set_prolog_flag(verbose_load,true).
% :- use_module(library(gui_tracer)).
:- set_prolog_flag(gui_tracer, false).

% :- catch(guitracer,_,true).


:- if_file_exists(ensure_loaded('../externals/swish/logicmoo_run_swish')).
:- if_startup_script(ensure_loaded(run_clio)).

:- with_no_term_expansions(if_file_exists(ensure_loaded('../externals/MUD_KnowRob/knowrob_addons/knowrob_mud/prolog/init'))).

% run_tests includes run_common 
:- include(run_tests).

:- forall(retract(prolog_debug:debugging(http(X), true, O)),show_call(asserta(prolog_debug:debugging(http(X), false, O)))).

:- forall(retract(prolog_debug:debugging((X), true, O)),show_call(asserta(prolog_debug:debugging((X), false, O)))).


% [Optionaly] re-define load_default_game
% load_default_game:- load_game(logicmoo('rooms/startrek.all.plmoo')).

% [Optionaly] load and start sparql server
% starts in forground
% :- at_start((slow_work,threads)).
% starts in thread (the the above was commented out)
% :- at_start(start_servers).
%:- thread_work.
% commented out except on run

:- add_to_search_path(game, '../games/src_game_startrek').

lst:-with_all_dmsg(( forall(enumerate_files(game('**/*.pl'),X),user_ensure_loaded(X)),
      forall(enumerate_files(game('**/*.plmoo'),X),declare_load_game(X)),
      finish_processing_world)).

% [Manditory] This loads the game and initializes so test can be ran
:- if_startup_script( at_start(run_setup)).

% :- ensure_plmoo_loaded(examples(game('rooms/startrek.all.plmoo'))).
:- forall(filematch(logicmoo('*/?*.plmoo'), X),dmsg(X)).
:- ensure_plmoo_loaded(logicmoo('*/?*.plmoo')).
:- forall(filematch(logicmoo('*/*/?*.plmoo'), X),dmsg(X)).
:- ensure_plmoo_loaded(logicmoo('*/*/?*.plmoo')).


:- finish_processing_world.

% the local tests each reload (once)
now_run_local_tests_dbg :- doall(mud_test_local).

% nasty way i debug the parser
% :-repeat, trace, do_player_action('who'),fail.
mud_test_local :- do_player_action('who').

% mud_test_local :-do_player_action("scansrc").

% more tests even
mud_test_local :-do_player_action("look").
mud_test_local :-forall(localityOfObject(O,L),dmsg(localityOfObject(O,L))).

must_test("tests to see if poorly canonicalized code (unrestricted quantification) will not be -too- inneffienct",
   forall(atloc(O,L),fmt(atloc(O,L)))).


% the real tests now (once)
mud_test_local :- at_start(must_det(run_mud_tests)).

% :- if_startup_script( doall(now_run_local_tests_dbg)).


% :-forall(current_prolog_flag(N,V),dmsg(N=V)).
% [Optionaly] Put a telnet client handler on the main console (nothing is executed past the next line)

:-foc_current_player(P),assertz_if_new(thglobal:player_command_stack(P,who)).
:-foc_current_player(P),assertz_if_new(thglobal:player_command_stack(P,look)).
:-foc_current_player(P),assertz_if_new(thglobal:player_command_stack(P,prolog)).


% :-foc_current_player(P),assertz_if_new(thglobal:player_command_stack(P,chat80)).
:- if_startup_script(at_start(run)).

% So scripted versions don't just exit
:- if_startup_script(at_start(prolog)).

