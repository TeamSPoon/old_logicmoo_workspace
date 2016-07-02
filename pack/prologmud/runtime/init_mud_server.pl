#!/usr/bin/env swipl
/** <module> MUD server startup script in SWI-Prolog

*/
:- set_prolog_flag(dialect_pfc,false).
:- set_prolog_stack(global, limit(16*10**9)).
:- set_prolog_stack(local, limit(16*10**9)).
:- set_prolog_stack(trail, limit(16*10**9)).
% ==========================================================
% Sanity tests that first run whenever a person stats the MUD to see if there are regressions in the system
% ==========================================================
:-multifile(lmconf:sanity_test/0).
:-multifile(lmconf:regression_test/0).
:-multifile(lmconf:feature_test/0).
:- dynamic((        
        lmconf:feature_test/0,
        lmconf:mud_test/2,
        lmconf:regression_test/0,
        lmconf:sanity_test/0,
        baseKB:agent_call_command/2,
        lmconf:type_action_info/3)).


:- system:ensure_loaded(setup_paths).
:- system:ensure_loaded(logicmoo_repl).
:- set_prolog_flag(dialect_pfc,false).

:- system:use_module(library(logicmoo/mpred_online/mpred_www)).
%:- ensure_webserver(3020).
%:- initialization(ensure_webserver(3020)).
%:- initialization(ensure_webserver(3020),now).
%:- initialization(ensure_webserver(3020),restore).


% [Mostly Required] Load the Logicmoo Parser/Generator System
:- gripe_time(40,user:ensure_loaded(library(parser_all))).


% [Mostly Required] Load the Logicmoo Plan Generator System
:- with_no_mpred_expansions(if_file_exists(user:ensure_loaded(library(logicmoo/logicmoo_planner)))).


% [Required] Load the CYC Network Client and Logicmoo CycServer Emulator (currently server is disabled)
% :- with_no_mpred_expansions(if_file_exists(user:ensure_loaded(library(logicmoo/logicmoo_u_cyc_api)))).

% [Optional] NOT YET Load the Logicmoo RDF/OWL Browser System
% % :- with_no_mpred_expansions(if_file_exists(user:ensure_loaded(logicmoo(mpred_online/mpred_rdf)))).


% [Debugging] Normarily this set as 'true' can interfere with debugging
% :- set_prolog_flag(gc,true).
% Yet turning it off we cant even startup without crashing
% :- set_prolog_flag(gc,false).

:- doall(printAll(current_prolog_flag(_N,_V))).

% ==========================================================
% Regression tests that first run whenever a person stats the MUD on the public server
% ==========================================================

:- if((gethostname(ubuntu),fail)). % INFO this fail is so we can start faster
:- show_entry(gripe_time(40, doall(lmconf:regression_test))).
:- endif.


% ==============================
% MUD SERVER CODE LOADS
% ==============================


% [Required] load the mud system
:- show_entry(gripe_time(40,user:ensure_loaded(prologmud(mud_loader)))).


:- set_prolog_flag(logicmoo_debug,true).


% ==============================
% MUD SERVER CODE STARTS
% ==============================

:- file_begin(pfc).
:- set_prolog_flag(dialect_pfc,true).



:-assert_isa(iRR7,tRR).
:-ain(genls(tRR,tRRP)).
:-must( isa(iRR7,tRRP) ).
% :-must( tRRP(iRR7) ).

%:- break.


% [Optional] Creates or suppliments a world

tCol(tRegion).
tCol(tLivingRoom).
genls(tLivingRoom,tRegion).
genls(tOfficeRoom,tRegion).


%genlsFwd(tLivingRoom,tRegion).
%genlsFwd(tOfficeRoom,tRegion).

% create some seats
tExplorer(iExplorer1).
tExplorer(iExplorer2).
tExplorer(iExplorer3).
tExplorer(iExplorer4).
tExplorer(iExplorer5).
tExplorer(iExplorer6).

(tHumanBody(skRelationAllExistsFn)==>{trace_or_throw(tHumanBody(skRelationAllExistsFn))}).

genls(tExplorer,tHominid).



tExplorer(iExplorer7).
wearsClothing(iExplorer7,'iBoots773').
wearsClothing(iExplorer7,'iCommBadge774').
wearsClothing(iExplorer7,'iGoldUniform775').
mudStowing(iExplorer7,'iPhaser776').

:- set_prolog_flag(dialect_pfc,true).

pddlSomethingIsa('iBoots773',['tBoots','ProtectiveAttire','PortableObject','tWearAble']).
pddlSomethingIsa('iCommBadge774',['tCommBadge','ProtectiveAttire','PortableObject','tNecklace']).
pddlSomethingIsa('iGoldUniform775',['tGoldUniform','ProtectiveAttire','PortableObject','tWearAble']).
pddlSomethingIsa('iPhaser776',['tPhaser','Handgun',tWeapon,'LightingDevice','PortableObject','DeviceSingleUser','tWearAble']).

tMonster(iCommanderdata66).
tExplorer(iCommanderdata66).
mudDescription(iCommanderdata66,txtFormatFn("Very screy looking monster named ~w",[iCommanderdata66])).
tAgent(iCommanderdata66).
tHominid(iCommanderdata66).
wearsClothing(iCommanderdata66,'iBoots673').
wearsClothing(iCommanderdata66,'iCommBadge674').
wearsClothing(iCommanderdata66,'iGoldUniform675').
mudStowing(iCommanderdata66,'iPhaser676').

pddlSomethingIsa('iBoots673',['tBoots','ProtectiveAttire','PortableObject','tWearAble']).
pddlSomethingIsa('iCommBadge674',['tCommBadge','ProtectiveAttire','PortableObject','tNecklace']).
pddlSomethingIsa('iGoldUniform675',['tGoldUniform','ProtectiveAttire','PortableObject','tWearAble']).
pddlSomethingIsa('iPhaser676',['tPhaser','Handgun',tWeapon,'LightingDevice','PortableObject','DeviceSingleUser','tWearAble']).
  

mpred_argtypes(bordersOn(tRegion,tRegion)).

tRegion(iLivingRoom7).
tRegion(iOfficeRoom7).

:-onSpawn(localityOfObject(tExplorer,tLivingRoom)).
:-onSpawn(localityOfObject(iCommanderdata66,tOfficeRoom)).
:-onSpawn(bordersOn(tLivingRoom,tOfficeRoom)).

:- set_prolog_flag(dialect_pfc,false).

:- file_begin(pl).

% [Optionaly] Start the telent server % iCommanderdata66
start_telnet:- on_x_log_cont(start_mud_telnet_4000).

% :- if_startup_script(initialization(start_telnet)).
:- rl_add_history( 'start_telnet.' ).
:- rl_add_history( 'user:ensure_loaded(run_mud_game).' ).
:- rl_add_history( 'login_and_run.' ).


% :-  statistics(globallimit,G),statistics(locallimit,L),statistics(traillimit,T), qsave_program(run_mud_server,[map('run_mud_server.sav'),global(G),trail(T),local(L)]).

:- write('\n?- user:ensure_loaded(run_mud_game). % to begin loading mud worlds').
% :- user:ensure_loaded(start_mud_server).


end_of_file.

Warning: baseKB:list_to_atomics_list/2, which is referenced by
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/vworld/world_text.pl:130:75: 1-st clause of baseKB:join_for_string/2
Warning: baseKB:logOnFailureIgnore/1, which is referenced by
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/parsing/simple_decl_parser.pl:218:1: 1-st clause of baseKB:assert_text_now/3
Warning: baseKB:member_eq/2, which is referenced by
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/parsing/parser_imperative.pl:176:18: 8-th clause of baseKB:save_fmt_e/2
Warning: baseKB:prevent_transform_moo_preds/0, which is referenced by
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/mud_loader.pl:201:20: 1-st clause of baseKB:slow_work/0
Warning: baseKB:replace_nth_arglist/5, which is referenced by
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/vworld/world_text.pl:68:65: 1-st clause of baseKB:term_anglify_args/6
Warning: baseKB:show_load_call/1, which is referenced by
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/vworld/world_text.pl:363:64: 3-th clause of baseKB:add_description_kv/3
Warning: baseKB:tag_pos/2, which is referenced by
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/parsing/simple_decl_parser.pl:241:106: 1-st clause of baseKB:translation_for/5
Warning: kellerStorage:kellerStorageTestSuite/0, which is referenced by
Warning:        2-nd clause of lmconf:mud_test_local/0: 2-nd clause of lmconf:mud_test_local/0

