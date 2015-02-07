#!/usr/local/bin/swipl -L8G -G8G -T8G -f
/** <module> MUD server startup script in SWI-Prolog

*/

:- multifile(user:mud_regression_test/0).
:- multifile(user:mudIsa/2).
:- dynamic(user:mudIsa/2).
:-import(user:mudIsa/2).
'$toplevel':mudIsa(X,Y):-user:mudIsa(X,Y).

:- set_prolog_flag(generate_debug_info, true).
:- exists_directory(runtime)->working_directory(_,runtime);(exists_directory('../runtime')->working_directory(_,'../runtime');true).

% [Manditory] Load the Logicmioo utils
:- '@'(ensure_loaded('../src_lib/logicmoo_util/logicmoo_util_all'),user).

:- include(logicmoo(vworld/moo_header)).

:- '@'(if_file_exists(user_ensure_loaded(logicmoo('../externals/MUD_XperiMental/snark/snark_in_prolog'))),'user').
/*
else

% -- CODEBLOCK
:-export(is_ftVar/1).
user:is_ftVar(V):-var(V),!.
user:is_ftVar('$VAR'(_)).
:-export(not_ftVar/1).
not_ftVar(V):-not(is_ftVar(V)).
   

*/

% bugger:action_verb_useable(actWearUnused,wearsClothing,tWearAble,mudPossess).

% [Manditory] define how we interact with the module system
swi_module(M,E):-dmsg(swi_module(M,E)).

% [Optionaly] Set the Prolog optimize/debug flags
:- set_prolog_flag(verbose_load,true).
:- use_module(library(gui_tracer)).
:- set_prolog_flag(gui_tracer, false).
:- set_prolog_flag(answer_write_options, [quoted(true), portray(true), max_depth(1000), spacing(next_argument)]).
:- set_prolog_flag(debug,false).
:- set_mem_opt(false).

:- multifile(user:semweb_startup).
:- export(do_semweb_startup/0).
do_semweb_startup:-forall(clause(user:semweb_startup,Body),must(show_call(Body))).

% [Optionaly] register swish server (remote file editing)
:- if_file_exists(ensure_loaded('../externals/swish/logicmoo_run_swish')).

% [Optionaly] register/run Cliopatria sparql server (remote RDF browsing)
:- if_startup_script(ensure_loaded('run_clio')).

% [Optionaly] register/run KnowRob robot services (we use it for the ontology mainly)
user:semweb_startup :- with_no_term_expansions(if_file_exists(ensure_loaded('../externals/MUD_KnowRob/knowrob_addons/knowrob_mud/prolog/init.pl'))).

% [Optionaly] register/run MILO robot services (we use it for the ontology mainly)
user:semweb_startup :- register_ros_package(milo).

% [Optionaly] register/run EulerSharp robot services (we use it for the ontology mainly)
user:semweb_startup :- register_ros_package(euler).

:- with_no_term_expansions(if_file_exists(ensure_loaded('../externals/MUD_ircbot/prolog/eggdrop.pl'))).
:- current_predicate(egg_go/0)->egg_go;true.

% [Manditory] run_tests (includes run_common)
:- include(run_tests).
% OR
% :- include(run_common).

% [Optionaly] remove debug noises
user:semweb_startup:- forall(retract(prolog_debug:debugging(http(X), true, O)),show_call(asserta(prolog_debug:debugging(http(X), false, O)))).
% user:semweb_startup:- forall(retract(prolog_debug:debugging((X), true, O)),show_call(asserta(prolog_debug:debugging((X), false, O)))).


:-multifile(pre_file_search_path/2).
% user:pre_file_search_path(_,_):-!,fail.
% :- do_semweb_startup.
% :- do_web_startup.

% [Manditory] load_default_game
% this is what happens when the world is not found
% :- add_game_dir('../games/src_game_unknown',prolog_repl).     

:- onSpawn(pathConnects(tLivingRoom,tOfficeRoom)).

% int_firstOrder(some_query, 666, What, C, E, A, J, D, L, B)
% :- forall(clause(user:mud_regression_test,Call),must(Call)).

:- declare_load_game('../games/src_game_nani/a_nani_household.plmoo').
:- declare_load_game('../games/src_game_nani/objs_misc_household.plmoo').

% the following 4 worlds are in version control in examples
% :- add_game_dir('../games/src_game_wumpus',prolog_repl).       
% :- add_game_dir('../games/src_game_nani',prolog_repl).       
% :- add_game_dir('../games/src_game_sims',prolog_repl).

:- add_game_dir('../games/src_game_startrek',prolog_repl).

tAgentGeneric(iCommanderData66).
mudIsa(iCommanderData66,'tMonster').
mudIsa(iCommanderData66,'tExplorer').
wearsClothing(iCommanderData66,'iBoots673').
wearsClothing(iCommanderData66,'iComBadge674').
wearsClothing(iCommanderData66,'iGoldUniform675').
mudStowing(iCommanderData6,'iPhaser676').
pddlSomethingIsa('iBoots673',['tBoots','ProtectiveAttire','PortableObject','SomethingToWear']).
pddlSomethingIsa('iComBadge674',['tComBadge','ProtectiveAttire','PortableObject','tNecklace']).
pddlSomethingIsa('iGoldUniform675',['tGoldUniform','ProtectiveAttire','PortableObject','SomethingToWear']).
pddlSomethingIsa('iPhaser676',['tPhaser','Handgun',tWeapon,'LightingDevice','PortableObject','DeviceSingleUser','SomethingToWear']).
mudDescription(iCommanderData66,txtFormatFn("Very screy looking monster named ~w",[iCommanderData66])).

tAgentGeneric(iExplorer77).
wearsClothing(iExplorer77,'iBoots7773').
wearsClothing(iExplorer77,'iComBadge7774').
wearsClothing(iExplorer77,'iGoldUniform7775').
mudIsa(iExplorer77,'tExplorer').
mudStowing(iExplorer6,'iPhaser7776').
pddlSomethingIsa('iBoots7773',['tBoots','ProtectiveAttire','PortableObject','SomethingToWear']).
pddlSomethingIsa('iComBadge7774',['tComBadge','ProtectiveAttire','PortableObject','tNecklace']).
pddlSomethingIsa('iGoldUniform7775',['tGoldUniform','ProtectiveAttire','PortableObject','SomethingToWear']).
pddlSomethingIsa('iPhaser7776',['tPhaser','Handgun',tWeapon,'LightingDevice','PortableObject','DeviceSingleUser','SomethingToWear']).


% [Manditory] This loads the game and initializes so test can be ran


:- if_startup_script( at_start(finish_processing_world)).

% :- if_startup_script( doall(now_run_local_tests_dbg)).

/*
:-enqueue_player_command(actWho).
:-enqueue_player_command("rez crackers").
:-enqueue_player_command("drop crackers").
:-enqueue_player_command('look').
:-enqueue_player_command("take crackers").
:-enqueue_player_command("eat crackers").
:-enqueue_player_command('look').
:-enqueue_player_command("rez pants").
:-enqueue_player_command("wear pants").
:-enqueue_player_command("tp to closet").
:-enqueue_player_command("take shirt").
:-enqueue_player_command("inventory").
*/

:-enqueue_player_command(prolog).

% [Optionaly] Tell the NPCs to do something every 30 seconds (instead of 90 seconds)
% :- register_timer_thread(npc_ticker,30,npc_tick).

% [Optionaly] Put a telnet client handler on the main console (nothing is executed past the next line)
:- if_startup_script(at_start(run)).

% So scripted versions don't just exit
:- if_startup_script(at_start(prolog)).
   
