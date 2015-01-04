%!swipl -f
/** <module> An Implementation a MUD server in SWI-Prolog

*/

% [Optionaly] Set the Prolog optimize/debug flags
:- set_prolog_flag(verbose_load,true).
:- use_module(library(gui_tracer)).
:- set_prolog_flag(gui_tracer, false).

% [Manditory] Load the Logicmioo utils
:- '@'(ensure_loaded('../src_lib/logicmoo_util/logicmoo_util_all'),user).
% :- set_prolog_flag(gc,true),set_prolog_flag(last_call_optimisation,true),set_prolog_flag(optimise,true).
% :- set_prolog_flag(debug,false),

% [Manditory] define how we interact with the module system
swi_module(M,E):-dmsg(swi_module(M,E)).
swi_export(_):-!.
swi_export(E):-dmsg(swi_export(E)).


% [Optionaly] register swish server (remote file editing)
% :- if_file_exists(ensure_loaded('../externals/swish/logicmoo_run_swish')).

% [Optionaly] register/run Cliopatria sparql server (remote RDF browsing)
% :- if_startup_script(ensure_loaded(run_clio)).

% [Optionaly] register/run KnowRob robot services (we use it for the ontology mainly)
% :- with_no_term_expansions(if_file_exists(ensure_loaded('../externals/MUD_KnowRob/knowrob_addons/knowrob_mud/prolog/init.pl'))).

% [Manditory] run_tests (includes run_common)
:- include(run_tests).
% OR
% :- include(run_common).

% [Optionaly] remove debug noises
:- forall(retract(prolog_debug:debugging(http(X), true, O)),show_call(asserta(prolog_debug:debugging(http(X), false, O)))).
:- forall(retract(prolog_debug:debugging((X), true, O)),show_call(asserta(prolog_debug:debugging((X), false, O)))).

:- with_no_term_expansions(if_file_exists(ensure_loaded('../externals/MUD_ircbot/prolog/eggdrop.pl'))).
:- current_predicate(egg_go/0)->egg_go;true.

% [Manditory] load_default_game
% this is what happens when the world is not found
% :- add_game_dir('../games/src_game_unknown',prolog_repl).     

:- onSpawn(pathBetween(tLivingRoom,tOfficeRoom)).

:- declare_load_game('../games/src_game_nani/a_nani_household.plmoo').
:- declare_load_game('../games/src_game_nani/objs_misc_household.plmoo').

% the following 4 worlds are in version control in examples
% :- add_game_dir('../games/src_game_wumpus',prolog_repl).       
% :- add_game_dir('../games/src_game_nani',prolog_repl).       
% :- add_game_dir('../games/src_game_sims',prolog_repl).       
% :- add_game_dir('../games/src_game_startrek',prolog_repl).       


% [Manditory] This loads the game and initializes so test can be ran
:- if_startup_script( at_start(finish_processing_world)).

% :- if_startup_script( doall(now_run_local_tests_dbg)).

:-enqueue_player_command(actWho).
:-enqueue_player_command('look').
:-enqueue_player_command("prolog").

% [Optionaly] Tell the NPCs to do something every 30 seconds (instead of 90 seconds)
:- register_timer_thread(npc_ticker,30,npc_tick).

% [Optionaly] Put a telnet client handler on the main console (nothing is executed past the next line)
:- if_startup_script(at_start(run)).

% So scripted versions don't just exit
:- if_startup_script(at_start(prolog)).
   
