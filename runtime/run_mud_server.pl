%!swipl -f
/** <module> An Implementation a MUD server in SWI-Prolog

*/

% [Manditory] Load the Logicmioo utils
:- '@'(ensure_loaded('../src_lib/logicmoo_util/logicmoo_util_all'),user).

% [Optionaly] Set the Prolog optimize/debug flags
:- set_prolog_flag(gc,true),set_prolog_flag(debug,false),set_prolog_flag(last_call_optimisation,true),set_prolog_flag(optimise,true).
:- set_prolog_flag(verbose_load,true).
:- use_module(library(gui_tracer)).
:- set_prolog_flag(gui_tracer, false).


% [Optionaly] Solve the Halting problem
:-redefine_system_predicate(system:halt).
:-abolish(system:halt,0).
system:halt:- format('the halting problem is now solved!').

% [Manditory] define how we interact with the module system
swi_module(M,E):-dmsg(swi_module(M,E)).
swi_export(_):-!.
swi_export(E):-dmsg(swi_export(E)).


% [Optionaly] register swish server (remote file editing)
:- if_file_exists(ensure_loaded('../externals/swish/logicmoo_run_swish')).

% [Optionaly] register/run Cliopatria sparql server (remote RDF browsing)
% :- if_startup_script(ensure_loaded(run_clio)).

% [Optionaly] register/run KnowRob robot services (we use it for the ontology mainly)
:- with_no_term_expansions(if_file_exists(ensure_loaded('../externals/MUD_KnowRob/knowrob_addons/knowrob_mud/prolog/init'))).

% [Manditory] run_tests (includes run_common)
:- include(run_tests).
% OR
% :- include(run_common).

% [Optionaly] remove debug noises
:- forall(retract(prolog_debug:debugging(http(X), true, O)),show_call(asserta(prolog_debug:debugging(http(X), false, O)))).
:- forall(retract(prolog_debug:debugging((X), true, O)),show_call(asserta(prolog_debug:debugging((X), false, O)))).


% [Manditory] load_default_game
:- add_game_dir('../games/src_game_unknown',prolog_repl).       
	  
% [Manditory] This loads the game and initializes so test can be ran
:- if_startup_script( at_start(finish_processing_world)).

% :- if_startup_script( doall(now_run_local_tests_dbg)).

:-enqueue_player_command(who).
:-enqueue_player_command(look).
% :-enqueue_player_command(prolog).

% [Optionaly] Put a telnet client handler on the main console (nothing is executed past the next line)
:- if_startup_script(at_start(run)).

% So scripted versions don't just exit
:- if_startup_script(at_start(prolog)).

