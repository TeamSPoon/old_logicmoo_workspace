#!/usr/bin/env swipl
/* * module  MUD server startup script in SWI-Prolog

*/

:- if( \+ current_module(prolog_stack)).
:- system:use_module(library(prolog_stack)).
 prolog_stack:stack_guard(none).
:- endif.

/*
:- set_prolog_flag(access_level,system).
:- use_module(library(prolog_history)).
:- use_module(library(base32)).
:- set_prolog_flag(compile_meta_arguments,false).
*/
:- set_prolog_flag(report_error,true).
:- set_prolog_flag(debug_on_error,true).
:- set_prolog_flag(debugger_write_options,[quoted(true), portray(true), max_depth(1000), attributes(portray)]).
:- set_prolog_flag(generate_debug_info,true).
:- profile(true).

%:- user:ensure_loaded(setup_paths).
%:- if(( system:use_module(library('logicmoo/util/logicmoo_util_clause_expansion.pl')), push_modules)). 
%:- endif.
% :- module(init_mud_server,[]).
% restore entry state
%:- lcme:reset_modules.

:- set_prolog_flag(access_level,system).

:- 
 op(1190,xfx,('::::')),
 op(1180,xfx,('==>')),
 op(1170,xfx,'<==>'),  
 op(1160,xfx,('<-')),
 op(1150,xfx,'=>'),
 op(1140,xfx,'<='),
 op(1130,xfx,'<=>'), 
 op(600,yfx,'&'), 
 op(600,yfx,'v'),
 op(350,xfx,'xor'),
 op(300,fx,'~'),
 op(300,fx,'-'),
 op(1199,fx,('==>')).

:- set_prolog_flag(access_level,user).

:- multifile
        prolog:message//1,
        prolog:message_hook/3.

% prolog:message(ignored_weak_import(Into, From:PI))--> { nonvar(Into),Into \== system,dtrace(dmsg(ignored_weak_import(Into, From:PI))),fail}.
% prolog:message(Into)--> { nonvar(Into),functor(Into,_F,A),A>1,arg(1,Into,N),\+ number(N),dtrace(wdmsg(Into)),fail}.
% prolog:message_hook(T,error,Warn):- dtrace(wdmsg(nessage_hook(T,warning,Warn))),fail.
% prolog:message_hook(T,warning,Warn):- dtrace(wdmsg(nessage_hook(T,warning,Warn))),fail.

:- set_prolog_flag(dialect_pfc,false).
:- set_prolog_stack(global, limit(16*10**9)).
:- set_prolog_stack(local, limit(16*10**9)).
:- set_prolog_stack(trail, limit(16*10**9)).
:- set_prolog_flag(unsafe_speedups,true).
% ==========================================================
% Sanity tests that first run whenever a person stats the MUD to see if there are regressions in the system
% ==========================================================

:- user:ensure_loaded(setup_paths).


:- system:ensure_loaded(library(prolog_server)).
:- prolog_server(4001, [allow(_)]).
:- system:ensure_loaded(library(logicmoo_utils)).

:- set_prolog_flag(access_level,system).

:- shell('./hmud/policyd').

unsafe_preds_init('$syspreds',shell,1).
unsafe_preds_init(M,F,A):-M=files_ex,current_predicate(M:F/A),member(X,[delete,copy]),atom_contains(F,X).
unsafe_preds_init(M,F,A):-M=process,current_predicate(M:F/A),member(X,[kill,create]),atom_contains(F,X).
unsafe_preds_init(M,F,A):-M=system,member(F,[shell,halt]),current_predicate(M:F/A).

:-forall(unsafe_preds_init(M,F,A),bugger:remove_pred(M,F,A)).

% [Optionaly] Solve the Halting problem
:-unlock_predicate(system:halt/0).
:-redefine_system_predicate(system:halt/0).
:-abolish(system:halt,0).
:-asserta((system:halt :- format('the halting problem is now solved!'))).
:-lock_predicate(system:halt/0).

:-redefine_system_predicate(system:halt/1).
:-abolish(system:halt,1).
:-asserta((system:halt(_) :- format('the halting problem is now solved!'))).
:-lock_predicate(system:halt/1).

:- dmsg('the halting problem is now solved!').
:- set_prolog_flag(access_level,user).


:- if(exists_source(library(eggdrop))).
:- ensure_loaded(library(eggdrop)).
:- egg_go.
:- endif.
%:- use_listing_vars.
% :- [run].
:- forall(debugging(X),nodebug(X)).
:- system:ensure_loaded(logicmoo_repl).
:- forall(debugging(X),nodebug(X)).
:- set_prolog_flag(logicmoo_debug,true).
%:- set_prolog_flag(unsafe_speedups,false).

:-baseKB:assert_isa(iRR7,tRR).
:-baseKB:ain(genls(tRR,tRRP)).
:-must( baseKB:isa(iRR7,tRRP) ).
:-must( baseKB:tRRP(iRR7) ).


:-kb_dynamic(baseKB:sanity_test/0).
:-kb_dynamic(baseKB:regression_test/0).
:-kb_dynamic(baseKB:feature_test/0).
:- kb_dynamic((        
        baseKB:feature_test/0,
        baseKB:mud_test/2,
        baseKB:regression_test/0,
        baseKB:sanity_test/0,
        baseKB:agent_call_command/2,
        action_info/2,
        type_action_info/3)).

:- statistics.

%:- ensure_webserver(3020).
:- initialization(ensure_webserver(3020)).
:- initialization(ensure_webserver(3020),now).
:- initialization(ensure_webserver(3020),restore).

:- assert_setting01(lmconf:eachRule_Preconditional(true)).

:- set_prolog_flag(dialect_pfc,false).

:- file_begin(pl).
:- ensure_loaded(logicmoo(logicmoo_engine)).

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


%:- ensure_loaded(pack(logicmoo_base/t/examples/pfc/'sanity_col_as_unary.pfc')).
%:- ensure_loaded(pack(logicmoo_base/t/examples/pfc/'sanity_birdt.pfc')).
%:- ensure_loaded(pack(logicmoo_base/t/examples/pfc/'sanity_sv.pfc')).
%:- ensure_loaded(pack(logicmoo_base/t/examples/pfc/'sanity_foob.pfc')).



:- if((gethostname(ubuntu),fail)). % INFO this fail is so we can start faster
:- show_entry(gripe_time(40, doall(baseKB:regression_test))).
:- endif.


% ==============================
% MUD SERVER CODE LOADS
% ==============================

% :- assert_setting01(lmconf:eachRule_Preconditional(isRuntime)).


%:- push_modules.
% [Required] load the mud system
:- show_entry(gripe_time(40,ensure_loaded(prologmud(mud_loader)))).
%:- lcme:reset_modules.

%:- set_prolog_flag(logicmoo_debug,true).


% ==============================
% MUD SERVER CODE STARTS
% ==============================

:- file_begin(pfc).
:- set_prolog_flag(dialect_pfc,false).


:- set_prolog_flag(dialect_pfc,true).
%:- set_prolog_flag(logicmoo_debug,true).
%:- set_prolog_flag(unsafe_speedups,false).
:- forall(debugging(X),nodebug(X)).

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

:- ain(localityOfObject(P,_)==>{put_in_world(P)}).


tRegion(iLivingRoom7).
tRegion(iOfficeRoom7).

tExplorer(iExplorer7).
wearsClothing(iExplorer7,'iBoots773').
wearsClothing(iExplorer7,'iCommBadge774').
wearsClothing(iExplorer7,'iGoldUniform775').
mudStowing(iExplorer7,'iPhaser776').

:-onSpawn(localityOfObject(iExplorer7,tLivingRoom)).

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
:-onSpawn(localityOfObject(iCommanderdata66,tOfficeRoom)).
  

mpred_argtypes(bordersOn(tRegion,tRegion)).


:- call_u(onSpawn(bordersOn(tLivingRoom,tOfficeRoom))).
:- nortrace,notrace.

:- set_prolog_flag(dialect_pfc,false).

:- file_begin(pl).

:- ensure_loaded(logicmoo(plarkc/logicmoo_i_cyc_kb)).


% [Optionaly] Start the telent server % iCommanderdata66
start_telnet:- on_x_log_cont(start_mud_telnet_4000).

% :- if_startup_script(initialization(start_telnet)).
:- rl_add_history( 'start_telnet.' ).
:- rl_add_history( 'user:ensure_loaded(run_mud_game).' ).
:- rl_add_history( 'login_and_run.' ).


% :-  statistics(globallimit,G),statistics(locallimit,L),statistics(traillimit,T), qsave_program(run_mud_server,[map('run_mud_server.sav'),global(G),trail(T),local(L)]).

:- write('\n?- user:ensure_loaded(run_mud_game). % to begin loading mud worlds').
% :- user:ensure_loaded(start_mud_server).

lar:- login_and_run.

:- initialization(ensure_webserver(3020),now).

% :- assert_setting01(lmconf:eachFact_Preconditional(isRuntime)).

:- set_prolog_flag(unsafe_speedups,true).
% isa(starTrek,mtCycL).
% :- starTrek:force_reload_mpred_file('../games/src_game_startrek/*.pfc.pl').
lst :- force_reload_mpred_file('../games/src_game_startrek/*.pfc.pl').

:- check_clause_counts.

:- must_det(argIsa(genlPreds,2,_)).

%:- ensure_loaded(logicmoo(plarkc/logicmoo_i_cyc_kb)).
%:- initialization(ltkb1,now).

% :- break.

:- assert_setting01(lmconf:eachRule_Preconditional(true)).
:- assert_setting01(lmconf:eachFact_Preconditional(true)).
:- ain(isRuntime).

:- ain(isa(iFoodRez2,tFood)).
:- must(isa(iFoodRez2,tEatAble)).
:- must(parseIsa_Call(tEatAble,O,[food],Rest)).

:- set_prolog_flag(unsafe_speedups,true).

:- gripe_time(3.0,coerce(s,vtDirection,_)).
:- gripe_time(3.0, \+ coerce(l,vtDirection,_)).

:- check_clause_counts.

:- initialization(lar,restore).
% :- initialization(lar).

