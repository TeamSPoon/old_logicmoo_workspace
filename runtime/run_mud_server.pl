#!/usr/local/bin/swipl -L8G -G8G -T8G -f
/** <module> MUD server startup script in SWI-Prolog

*/

:- multifile(user:mud_regression_test/0).
:- multifile user:was_imported_kb_content/2.
:- dynamic user:was_imported_kb_content/2.
:- discontiguous(user:was_imported_kb_content/2).

:- set_prolog_flag(generate_debug_info, true).
:- exists_directory(runtime)->working_directory(_,runtime);(exists_directory('../runtime')->working_directory(_,'../runtime');true).

% [Manditory] Load the Logicmioo utils
:- '@'(ensure_loaded('../src_lib/logicmoo_util/logicmoo_util_all'),user).

%:- include(logicmoo(vworld/moo_header)).


:- '@'(ensure_loaded('../src_mud/dbase/dbase_i_pldoc'),user).

%:- export(with_no_dbase_expansions/1).
%:- meta_predicate(with_no_dbase_expansions(0)).
with_no_dbase_expansions(Goal):-
  with_assertions(user:prolog_mud_disable_term_expansions,Goal).

% bugger:action_verb_useable(actWearUnused,wearsClothing,tWearAble,mudPossess).

% [Manditory] define how we interact with the module system
:-if(not(current_predicate(swi_module/2))).
swi_module(M,E):-dmsg(swi_module(M,E)).
:-endif.

user:file_search_path(weblog, 'C:/docs/Prolog/weblog/development/weblog/prolog').
user:file_search_path(weblog, 'C:/Users/Administrator/AppData/Roaming/SWI-Prolog/pack/weblog').
user:file_search_path(weblog, '/usr/lib/swi-prolog/pack/weblog/prolog'):-current_prolog_flag(unix,true).
user:file_search_path(cliopatria, '../externals/ClioPatria'). % :- current_prolog_flag(unix,true).
user:file_search_path(user, '../externals/ClioPatria/user/').
user:file_search_path(swish, '../externals/swish'):- current_prolog_flag(unix,true).


% [Optionaly] Set the Prolog optimize/debug flags
:- set_prolog_flag(verbose_load,true).
:- use_module(library(gui_tracer)).
:- set_prolog_flag(gui_tracer, false).
:- set_prolog_flag(answer_write_options, [quoted(true), portray(true), max_depth(1000), spacing(next_argument)]).
%:- set_prolog_flag(debug,false).
:- set_mem_opt(false).

:-dynamic(did_ref_job/1).
do_ref_job(_Body,Ref):-did_ref_job(Ref),!.
do_ref_job(Body ,Ref):-asserta(did_ref_job(Ref)),!,show_call(Body).
:- multifile(user:semweb_startup).
:- export(do_semweb_startup/0).
do_semweb_startup:-
   predicate_property(user:semweb_startup,number_of_clauses(N1)),
   forall(clause(user:semweb_startup,Body,Ref),must(do_ref_job(Body,Ref))),
   predicate_property(user:semweb_startup,number_of_clauses(N2)),
   ((N2\=N1) -> do_semweb_startup ; true).

% [Optionaly] register swish server (remote file editing)
:- with_no_dbase_expansions(if_file_exists(ensure_loaded('../externals/swish/logicmoo_run_swish'))).

% [Optionaly] register/run Cliopatria sparql server (remote RDF browsing)
user:semweb_startup:-ensure_loaded('run_clio').

% [Optionaly] register/run KnowRob robot services (we use it for the ontology mainly)
% TODO 

user:semweb_startup :- with_no_term_expansions(if_file_exists(ensure_loaded('../externals/MUD_KnowRob/knowrob_addons/knowrob_mud/prolog/init.pl'))).

% [Optionaly] register/run MILO robot services (we use it for the ontology mainly)
% TODO user:semweb_startup :- register_ros_package(milo).

% [Optionaly] register/run EulerSharp robot services (we use it for the ontology mainly)
% TODO user:semweb_startup :- register_ros_package(euler).

:- ensure_loaded(logicmoo(dbase/dbase_i_pldoc)).
:- do_semweb_startup.

:- with_no_dbase_expansions(if_file_exists(ensure_loaded('../externals/MUD_ircbot/prolog/eggdrop.pl'))).
:- current_predicate(egg_go/0)->egg_go;true.


% [Optionaly] remove debug noises
user:semweb_startup:- forall(retract(prolog_debug:debugging(http(X), true, O)),show_call(asserta(prolog_debug:debugging(http(X), false, O)))).
% user:semweb_startup:- forall(retract(prolog_debug:debugging((X), true, O)),show_call(asserta(prolog_debug:debugging((X), false, O)))).

:-multifile(pre_file_search_path/2).
% user:pre_file_search_path(_,_):-!,fail.


% [Manditory] run_tests (includes run_common)
% :- include(run_tests).
% OR
:- include(run_common).

% [Manditory] load_default_game
% this is what happens when the world is not found
% :- add_game_dir('../games/src_game_unknown',prolog_repl).     

% :- prolog_repl.

tCol(tLivingRoom).
genls(tLivingRoom,tRegion).
genls(tOfficeRoom,tRegion).
:-decl_mpred_hybrid(pathConnects(tRegion,tRegion),symmetric).
:- onSpawn(pathConnects(tLivingRoom,tOfficeRoom)).
% int_firstOrder(some_query, 666, What, C, E, A, J, D, L, B)
% :- forall(clause(user:mud_regression_test,Call),must(Call)).

% :- declare_load_dbase('../games/src_game_nani/a_nani_household.plmoo').
% :- declare_load_dbase('../games/src_game_nani/objs_misc_household.plmoo').

% the following 4 worlds are in version control in examples
% :- add_game_dir('../games/src_game_wumpus',prolog_repl).       
% :- add_game_dir('../games/src_game_nani',prolog_repl).       
% :- add_game_dir('../games/src_game_sims',prolog_repl).


% :- add_game_dir('../games/src_game_startrek',prolog_repl).
% :- declare_load_dbase('../games/src_game_startrek/startrek.all.plmoo').

:- do_ensure_some_pathBetween.

tAgentGeneric(iCommanderData66).
isa(iCommanderData66,'tMonster').
isa(iCommanderData66,'tExplorer').
wearsClothing(iCommanderData66,'iBoots673').
wearsClothing(iCommanderData66,'iComBadge674').
wearsClothing(iCommanderData66,'iGoldUniform675').
mudStowing(iCommanderData66,'iPhaser676').
pddlSomethingIsa('iBoots673',['tBoots','ProtectiveAttire','PortableObject','tWearAble']).
pddlSomethingIsa('iComBadge674',['tComBadge','ProtectiveAttire','PortableObject','tNecklace']).
pddlSomethingIsa('iGoldUniform675',['tGoldUniform','ProtectiveAttire','PortableObject','tWearAble']).
pddlSomethingIsa('iPhaser676',['tPhaser','Handgun',tWeapon,'LightingDevice','PortableObject','DeviceSingleUser','tWearAble']).
mudDescription(iCommanderData66,txtFormatFn("Very screy looking monster named ~w",[iCommanderData66])).

tAgentGeneric(iExplorer1).
wearsClothing(iExplorer1,'iBoots773').
wearsClothing(iExplorer1,'iComBadge774').
wearsClothing(iExplorer1,'iGoldUniform775').
isa(iExplorer1,'tExplorer').
mudStowing(iExplorer1,'iPhaser776').
pddlSomethingIsa('iBoots773',['tBoots','ProtectiveAttire','PortableObject','tWearAble']).
pddlSomethingIsa('iComBadge774',['tComBadge','ProtectiveAttire','PortableObject','tNecklace']).
pddlSomethingIsa('iGoldUniform775',['tGoldUniform','ProtectiveAttire','PortableObject','tWearAble']).
pddlSomethingIsa('iPhaser776',['tPhaser','Handgun',tWeapon,'LightingDevice','PortableObject','DeviceSingleUser','tWearAble']).
isa(iExplorer1,'tExplorer').

:-onSpawn(localityOfObject(iExplorer1,'tLivingRoom')).


% [Manditory] This loads the game and initializes so test can be ran
:- if_startup_script( at_start(finish_processing_world)).

% :- if_startup_script( doall(now_run_local_tests_dbg)).

% :-enqueue_player_command("hide").
:-enqueue_player_command(actWho).
:-enqueue_player_command("rez crackers").
:-enqueue_player_command("drop crackers").
:-enqueue_player_command('look').

% :-enqueue_player_command(prolog).

:-enqueue_player_command("take crackers").
:-enqueue_player_command("eat crackers").
:-enqueue_player_command('look').
:-enqueue_player_command("rez pants").
:-enqueue_player_command("wear pants").
:-enqueue_player_command("tp to closet").
:-enqueue_player_command("take shirt").
:-enqueue_player_command("inventory").

% :-enqueue_player_command(prolog).

% [Optionaly] Tell the NPCs to do something every 30 seconds (instead of 90 seconds)
% :- register_timer_thread(npc_ticker,30,npc_tick).

% [Optionaly] Put a telnet client handler on the main console (nothing is executed past the next line)
:- if_startup_script(at_start(run)).

% So scripted versions don't just exit
:- if_startup_script(at_start(prolog)).
   
