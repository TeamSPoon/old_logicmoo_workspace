#!/usr/bin/env swipl
/** <module> MUD server startup script in SWI-Prolog

*/
:- set_prolog_flag(dialect_pfc,false).
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
        lmconf:agent_call_command/2,
        lmconf:type_action_info/3)).


:- ensure_loaded(logicmoo_repl).

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

:- set_prolog_flag(dialect_pfc,false).

:- system:use_module(library(logicmoo/mpred_online/mpred_www)).
%:- initialization(ensure_webserver(3020)).
%:- initialization(ensure_webserver(3020),now).
%:- initialization(ensure_webserver(3020),restore).

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

isa(iCommanderdata66,'tMonster').
mudDescription(iCommanderdata66,txtFormatFn("Very screy looking monster named ~w",[iCommanderdata66])).
tAgent(iCommanderdata66).
tHominid(iCommanderdata66).
isa(iCommanderdata66,'tExplorer').
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

:- if_startup_script(initialization(start_telnet)).
:- rl_add_history( 'start_telnet.' ).
:- rl_add_history( 'user:ensure_loaded(start_mud_server).' ).
:- rl_add_history( 'login_and_run.' ).

oinfo(O):- xlisting((O, - spft, - ( ==> ), - pt , - nt , - bt , - mdefault, - lmcache)).


% :-  statistics(globallimit,G),statistics(locallimit,L),statistics(traillimit,T), qsave_program(run_mud_server,[map('run_mud_server.sav'),global(G),trail(T),local(L)]).

:- write('\n?- user:ensure_loaded(run_mud_game). % to begin loading mud worlds').
% :- user:ensure_loaded(start_mud_server).


end_of_file.

arning:
Warning: baseKB:get_instance_default_props/2, which is referenced by
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/mobs/prey.pl:85:7: 2-nd clause of baseKB:spawn_prey/1
Warning: baseKB:guess_arity/3, which is referenced by
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/vworld/world_agent.pl:279:83: 5-th clause of baseKB:random_instance_no_throw0/3
Warning: baseKB:is_fact_consistent/1, which is referenced by
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/vworld/world_2d.pl:328:143: 3-th clause of baseKB:create_random_fact/1
Warning: baseKB:is_instance_consistent/2, which is referenced by
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_testing.pl:232:29: 2-nd clause of baseKB:check_consistent/2
Warning: baseKB:list_to_atomics_list/2, which is referenced by
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/vworld/world_text.pl:130:75: 1-st clause of baseKB:join_for_string/2
Warning: baseKB:logOnFailureIgnore/1, which is referenced by
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/parsing/simple_decl_parser.pl:215:1: 1-st clause of baseKB:assert_text_now/3
Warning: baseKB:member_eq/2, which is referenced by
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/parsing/parser_imperative.pl:176:18: 8-th clause of baseKB:save_fmt_e/2
Warning: '/root/lib/swipl/pack/prologmud/prolog/prologmud/parsing/parser_imperative.pl':638:27: Illegal UTF-8 start
Warning: baseKB:mudWielding/2, which is referenced by
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/parsing/parser_imperative.pl:638:27: 2-nd clause of baseKB:mudDistance/3
Warning: baseKB:prevent_transform_moo_preds/0, which is referenced by
Warning:        1-st clause of baseKB:slow_work/0: 1-st clause of baseKB:slow_work/0
Warning: baseKB:replace_nth_arglist/5, which is referenced by
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/vworld/world_text.pl:68:65: 1-st clause of baseKB:term_anglify_args/6
Warning: baseKB:show_load_call/1, which is referenced by
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/vworld/world_text.pl:363:64: 3-th clause of baseKB:add_description_kv/3
Warning: baseKB:tag_pos/2, which is referenced by
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/parsing/simple_decl_parser.pl:238:106: 1-st clause of baseKB:translation_for/5
Warning: kellerStorage:kellerStorageTestSuite/0, which is referenced by
Warning:        1-st clause of lmconf:mud_test_local/0: 1-st clause of lmconf:mud_test_local/0
Warning: lmconf:check_consistent/2, which is referenced by
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_testing.pl:242: 16-th clause of lmconf:mud_test_local/0
Warning: lmconf:do_agent_action/1, which is referenced by
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_testing.pl:218:24: 14-th clause of lmconf:mud_test_local/0
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_testing.pl:276:25: 17-th clause of lmconf:mud_test_local/0
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_testing.pl:281:24: 18-th clause of lmconf:mud_test_local/0
Warning: lmconf:foc_current_agent/1, which is referenced by
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_testing.pl:187:3: 7-th clause of lmconf:mud_test_local/0
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_testing.pl:193:3: 8-th clause of lmconf:mud_test_local/0
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_testing.pl:199:6: 9-th clause of lmconf:mud_test_local/0
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_testing.pl:204:6: 10-th clause of lmconf:mud_test_local/0
Warning: lmconf:get_all_templates/1, which is referenced by
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_testing.pl:208:51: 11-th clause of lmconf:mud_test_local/0
Warning: lmconf:req1/1, which is referenced by
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_testing.pl:214:23: 13-th clause of lmconf:mud_test_local/0
Warning: lmconf:run_mud_tests/0, which is referenced by
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_testing.pl:288:43: 20-th clause of lmconf:mud_test_local/0
Warning: lmconf:test_false/1, which is referenced by
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_testing.pl:184:3: 6-th clause of lmconf:mud_test_local/0
Warning: lmconf:test_name/1, which is referenced by
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_testing.pl:171:3: 3-th clause of lmconf:mud_test_local/0
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_testing.pl:175:3: 4-th clause of lmconf:mud_test_local/0
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_testing.pl:179:3: 5-th clause of lmconf:mud_test_local/0
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_testing.pl:183:3: 6-th clause of lmconf:mud_test_local/0
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_testing.pl:188:3: 7-th clause of lmconf:mud_test_local/0
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_testing.pl:192:3: 8-th clause of lmconf:mud_test_local/0
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_testing.pl:198:6: 9-th clause of lmconf:mud_test_local/0
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_testing.pl:203:6: 10-th clause of lmconf:mud_test_local/0
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_testing.pl:208:3: 11-th clause of lmconf:mud_test_local/0
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_testing.pl:211:3: 12-th clause of lmconf:mud_test_local/0
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_testing.pl:242: 16-th clause of lmconf:mud_test_local/0
Warning: lmconf:test_true/1, which is referenced by
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_testing.pl:172:3: 3-th clause of lmconf:mud_test_local/0
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_testing.pl:176:3: 4-th clause of lmconf:mud_test_local/0
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_testing.pl:180:3: 5-th clause of lmconf:mud_test_local/0
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_testing.pl:189:3: 7-th clause of lmconf:mud_test_local/0
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_testing.pl:195:3: 8-th clause of lmconf:mud_test_local/0
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_testing.pl:212:6: 12-th clause of lmconf:mud_test_local/0
Warning: lmconf:test_true_req/1, which is referenced by
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_testing.pl:200:7: 9-th clause of lmconf:mud_test_local/0
Warning:        /root/lib/swipl/pack/prologmud/prolog/prologmud/server/mud_testing.pl:205:7: 10-th clause of lmconf:mud_test_local/0
true.

