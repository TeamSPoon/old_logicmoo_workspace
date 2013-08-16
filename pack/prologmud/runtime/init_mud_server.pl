#!/usr/bin/env swipl
/* * module  MUD server startup script in SWI-Prolog

*/
:- set_prolog_flag(double_quotes,string).
:- set_prolog_flag(autoload_logicmoo,false).
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

:-if((multifile(baseKB:ignore_file_mpreds/1),dynamic(baseKB:ignore_file_mpreds/1), 
   (prolog_load_context(source,F1) -> asserta(baseKB:ignore_file_mpreds(F1)) ; true),
   (prolog_load_context(file,F) -> asserta(baseKB:ignore_file_mpreds(F)) ; true))).
:-endif.

:- user:ensure_loaded(setup_paths).
:- set_prolog_flag(double_quotes,string).
:- user:ensure_loaded(library(logicmoo_utils)).
:- set_prolog_flag(double_quotes,string).

% :- module(init_mud_server,[]).
% restore entry state

:- set_prolog_flag(access_level,system).
:- lmce:current_smt(SM,M),writeln(current_smt(SM,M)).

:- op(300,fx,'-').

:- set_prolog_flag(access_level,user).

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

:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )). 
%:- lmce:reset_modules.
:- endif.

:- ensure_loaded(library(prolog_server)).
:- getenv_safe('LOGICMOO_PORT',Was,3000),
   WebPort is Was + 1023,
   prolog_server(WebPort, [allow(_)]).

:- http_handler('/hmud/', http_reply_from_files(pack(hMUD), []), [prefix]).

:- shell('./hmud/policyd').

unsafe_preds_init(W,shell,2):-predicate_property(shell(_,_),imported_from(W)).
unsafe_preds_init(W,shell,1):-predicate_property(shell(_),imported_from(W)).
unsafe_preds_init(W,shell,0):-predicate_property(shell,imported_from(W)).
unsafe_preds_init(M,F,A):-M=files_ex,current_predicate(M:F/A),member(X,[delete,copy]),atom_contains(F,X).
unsafe_preds_init(M,F,A):-M=process,current_predicate(M:F/A),member(X,[kill,create]),atom_contains(F,X).
unsafe_preds_init(M,F,A):-M=system,member(F,[shell,halt]),current_predicate(M:F/A).

:- if(\+ compiling).
:- set_prolog_flag(access_level,system).

% [Optionaly] Solve the Halting problem
:-unlock_predicate(system:halt/0).
:-redefine_system_predicate(system:halt/0).
:-abolish(system:halt,0).
:-asserta((system:halt :- format('the halting problem is now solved!'))).
:-lock_predicate(system:halt/0).

:-unlock_predicate(system:halt/1).
:-redefine_system_predicate(system:halt/1).
:-abolish(system:halt,1).
:-asserta((system:halt(_) :- format('the halting problem is now solved!'))).
:-lock_predicate(system:halt/1).
kill_unsafe_preds:-(dmsg("kill_unsafe_preds!"),w_tl(set_prolog_flag(access_level,system),forall(unsafe_preds_init(M,F,A),bugger:remove_pred(M,F,A)))).
:- dmsg("the halting problem is now solved!").
:- set_prolog_flag(access_level,user).

:- if(exists_source(library(eggdrop))).
:- ensure_loaded(library(eggdrop)).
:- egg_go.
:- endif.
%:- use_listing_vars.
% :- [run].
:- endif.

% Loaded LogicMOO Code!!!
:- ensure_loaded(logicmoo_repl).

:- set_prolog_flag(unsafe_speedups,true).
%:- mpred_trace_exec.
:- debug.
% :- rtrace.

:- baseKB:ain(isa(iRR7,tRR)).
:- baseKB:ain(genls(tRR,tRRP)).
:- \+ baseKB:tRRP(iRR7) -> (xlisting(iRR7),xlisting(tRRP)) ; true.
:- must( baseKB:isa(iRR7,tRR) ).
:- must( baseKB:isa(iRR7,tRRP) ).
:- must( baseKB:tRRP(iRR7) ).

:- '$set_source_module'(baseKB).
:- '$set_typein_module'(baseKB).

:- listing((kb_dynamic)/1).
:- kb_dynamic(baseKB:sanity_test/0).
:- kb_dynamic(baseKB:regression_test/0).
:- kb_dynamic(baseKB:feature_test/0).
:- kb_dynamic((        
        baseKB:feature_test/0,
        baseKB:mud_test/2,
        baseKB:regression_test/0,
        baseKB:sanity_test/0,
        baseKB:agent_call_command/2,
        action_info/2,
        type_action_info/3)).

:- statistics.
% :- gripe_time(40,baseKB:ensure_loaded(library(parser_e2c))).

:- set_prolog_flag(logicmoo_include,'$set_source_module'(baseKB)).
:- set_prolog_flag(pfc_booted,true).
%:- set_prolog_flag(retry_undefined,true).
:- set_prolog_flag(read_attvars,false).

% :- must((statistics(cputime,X),X<65)).  % was 52

ensure_webserver_3020:- 
   getenv_safe('LOGICMOO_PORT',Was,3000),
   WebPort is Was + 20,
   find_and_call(ensure_webserver(WebPort)).
:- initialization(ensure_webserver_3020).
:- initialization(ensure_webserver_3020,now).
:- initialization(ensure_webserver_3020,restore).

:- assert_setting01(lmconf:eachRule_Preconditional(true)).

:- set_prolog_flag(dialect_pfc,false).

:- file_begin(pl).
:- ensure_loaded(logicmoo(logicmoo_engine)).

% [Mostly Required] Load the Logicmoo Parser/Generator System
:- gripe_time(40,user:ensure_loaded(library(parser_all))).


% [Mostly Required] Load the Logicmoo Plan Generator System
:- with_no_mpred_expansions(if_file_exists(user:ensure_loaded(library(logicmoo/logicmoo_planner)))).


% [Required] Load the CYC Network Client and Logicmoo CycServer Emulator (currently server is disabled)
% :- with_no_mpred_expansions(if_file_exists(ensure_loaded(library(logicmoo/logicmoo_u_cyc_api)))).

% [Optional] NOT YET Load the Logicmoo RDF/OWL Browser System
% % :- with_no_mpred_expansions(if_file_exists(user:ensure_loaded(logicmoo(mpred_online/mpred_rdf)))).


% [Debugging] Normarily this set as 'true' can interfere with debugging
% :- set_prolog_flag(gc,true).
% Yet turning it off we cant even startup without crashing
% :- set_prolog_flag(gc,false).

:- doall(printAll(current_prolog_flag(_N,_V))).
:- initialization(kill_unsafe_preds,restore).
:- if(\+ compiling).
:- initialization(kill_unsafe_preds).
:- endif.
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
% :- with_mpred_trace_exec(show_entry(gripe_time(40,ensure_loaded(prologmud(mud_loader))))).
:- must(show_entry(gripe_time(40,ensure_loaded(prologmud(mud_loader))))).
%:- lmce:reset_modules.

:- set_prolog_flag(logicmoo_debug,true).

:- mpred_trace_exec.
:- ain(isLoaded(iSourceCode7)).
:- mpred_notrace_exec.

% ==============================
% MUD SERVER CODE STARTS
% ==============================


:- set_prolog_flag(dialect_pfc,false).
%:- set_prolog_flag(retry_undefined,true).
:- set_prolog_flag(logicmoo_debug,true).
:- set_prolog_flag(unsafe_speedups,false).

% :- mpred_trace_exec.

:- if(functorDeclares(mobExplorer)).

:- sanity(functorDeclares(tSourceData)).
:- sanity(functorDeclares(mobExplorer)).


% [Optional] Creates or suppliments a world
:- set_prolog_flag(dialect_pfc,true).
:- file_begin(pfc).

:- ain((tCol(tLivingRoom),
tSet(tRegion),
tSet(tLivingRoom),

tSet(mobExplorer),
genls(tLivingRoom,tRegion),
genls(tOfficeRoom,tRegion),


%genlsFwd(tLivingRoom,tRegion).
%genlsFwd(tOfficeRoom,tRegion).

% create some seats
% :- trace.
mobExplorer(iExplorer1),
mobExplorer(iExplorer2),
mobExplorer(iExplorer3),
mobExplorer(iExplorer4),
mobExplorer(iExplorer5),
mobExplorer(iExplorer6),

(tHumanBody(skRelationAllExistsFn)==>{trace_or_throw(tHumanBody(skRelationAllExistsFn))}),

genls(mobExplorer,tHominid))).

:- ain(localityOfObject(P,_)==>{put_in_world(P)}).

:- ain((
tRegion(iLivingRoom7),
tRegion(iOfficeRoom7),

mobExplorer(iExplorer7),
wearsClothing(iExplorer7,'iBoots773'),
wearsClothing(iExplorer7,'iCommBadge774'),
wearsClothing(iExplorer7,'iGoldUniform775'),
mudStowing(iExplorer7,'iPhaser776'))).

:-onSpawn(localityOfObject(iExplorer7,tLivingRoom)).

:- ain((
pddlSomethingIsa('iBoots773',['tBoots','ProtectiveAttire','PortableObject','tWearAble']),
pddlSomethingIsa('iCommBadge774',['tCommBadge','ProtectiveAttire','PortableObject','tNecklace']),
pddlSomethingIsa('iGoldUniform775',['tGoldUniform','ProtectiveAttire','PortableObject','tWearAble']),
pddlSomethingIsa('iPhaser776',['tPhaser','Handgun',tWeapon,'LightingDevice','PortableObject','DeviceSingleUser','tWearAble']),

mobMonster(iCommanderdata66),
mobExplorer(iCommanderdata66),
mudDescription(iCommanderdata66,txtFormatFn("Very scary looking monster named ~w",[iCommanderdata66])),
tAgent(iCommanderdata66),
tHominid(iCommanderdata66),
wearsClothing(iCommanderdata66,'iBoots673'),
wearsClothing(iCommanderdata66,'iCommBadge674'),
wearsClothing(iCommanderdata66,'iGoldUniform675'),
mudStowing(iCommanderdata66,'iPhaser676'),

pddlSomethingIsa('iBoots673',['tBoots','ProtectiveAttire','PortableObject','tWearAble']),
pddlSomethingIsa('iCommBadge674',['tCommBadge','ProtectiveAttire','PortableObject','tNecklace']),
pddlSomethingIsa('iGoldUniform675',['tGoldUniform','ProtectiveAttire','PortableObject','tWearAble']),
pddlSomethingIsa('iPhaser676',['tPhaser','Handgun',tWeapon,'LightingDevice','PortableObject','DeviceSingleUser','tWearAble']))).
:-onSpawn(localityOfObject(iCommanderdata66,tOfficeRoom)).
  
:- ain((mpred_argtypes(mudAreaConnected(tRegion,tRegion)))).


:- call_u(onSpawn(mudAreaConnected(tLivingRoom,tOfficeRoom))).
:- nortrace,notrace.

:- set_prolog_flag(dialect_pfc,false).

:- endif.

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


:- initialization(ensure_webserver_3020,now).

% :- assert_setting01(lmconf:eachFact_Preconditional(isRuntime)).

:- set_prolog_flag(unsafe_speedups,true).
% isa(starTrek,mtCycL).
% :- starTrek:force_reload_mpred_file('../games/src_game_startrek/*.pfc.pl').
lst :- force_reload_mpred_file('../games/src_game_startrek/*.pfc.pl').

:- check_clause_counts.

:- must_det(argIsa(genlPreds,2,_)).

:- expand_file_search_path(pack(logicmoo_nlu/prolog/pldata),X),exists_directory(X),!,assert(user:file_search_path(pldata,X)).

:- ensure_loaded(logicmoo(logicmoo_plarkc)). % ,logicmoo_i_cyc_kb:call(call,ltkb1).

%:- initialization(ltkb1,load_ckb).

:- assert_setting01(lmconf:eachRule_Preconditional(true)).
:- assert_setting01(lmconf:eachFact_Preconditional(true)).

% UNCOMMNET :- ensure_loaded(library(parser_e2c)). 

:- mpred_notrace_exec.


/*
:- ain(isa(iFooRez2,tFood)).
:- must(isa(iFooRez2,tEatAble)).
:- must((parseIsa_Call(tEatAble,O,[food],Rest),O=iFoodRez2,Rest=[])).
*/

:- set_prolog_flag(unsafe_speedups,true).
:- statistics.
%:- gripe_time(1.0,must(coerce(s,vtDirection,_))).
%:- gripe_time(2.0,must( \+ coerce(l,vtDirection,_))).

:- check_clause_counts.

% :- make.
lar:- ain(tSourceData(iWorldData8)),ain(isLoaded(iWorldData8)),ain(isRuntime), login_and_run.
:- initialization(lar,restore).
% :- initialization(lar).


end_of_file.

% :- meta_predicate baseKB:random_instance_no_throw0(*,*,0).
% :- meta_predicate baseKB:fmt_holds_tcall_pred_trans(4,3,?,?,?).
% :- meta_predicate baseKB:dcgParse213(//,//,//,*,?).
% :- meta_predicate baseKB:thread_signal_blocked(*,0).
% :- meta_predicate baseKB:show_call_fmt(0).
% :- meta_predicate baseKB:intersect(*,*,*,*,0,-).
% :- meta_predicate baseKB:run_mud_test_clause(:,0).
% :- meta_predicate baseKB:stringArgUC2(*,*,0).
% :- meta_predicate baseKB:object_print_details0(2,*,*,*,*).
% :- meta_predicate baseKB:call_close_and_detatch(*,*,*,0).
% :- meta_predicate baseKB:get_sorted_instances(?,*,3).
% :- meta_predicate baseKB:telnet_repl_writer(*,*,*,0).
% :- meta_predicate baseKB:hooked_random_instance(*,*,0).
% :- meta_predicate baseKB:punless(0,0).
% :- meta_predicate baseKB:meetsForm80(0,0,*).
% :- meta_predicate baseKB:test_call0(0).
% :- meta_predicate baseKB:agent_coerce_for(2,*,?,?,?).
% :- meta_predicate baseKB:parse_agent_text_command_0(*,0,*,*,*).
% :- meta_predicate baseKB:nonvar_must_be(*,0).
% :- meta_predicate baseKB:pred_contains_term(2,?,?).
% :- meta_predicate baseKB:do_dcg(//,*,*,?,?).
% :- meta_predicate baseKB:service_client_call(0,*,*,*,*,*,*).
% :- meta_predicate baseKB:within_user(0).
% :- meta_predicate baseKB:map_term(2,*,*).
% :- meta_predicate baseKB:must_ac(0).
% :- meta_predicate baseKB:now_try_game_dir(0).
% :- meta_predicate logicmoo_util_dcg:dcgSeq(//,//,*,?).
% Restarting analysis ...
% autoloading logicmoo_util_ctx_frame:list_to_set_safe/2 from t:/devel/LogicmooDeveloperFramework/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_terms
% Found new meta-predicates in iteration 2 (2.647 sec)
% :- meta_predicate baseKB:object_print_details(2,*,*,*,*).
% :- meta_predicate baseKB:stringArgUC(*,*,0).
% :- meta_predicate baseKB:fmt_holds_tcall_pred(4,3,?,?,?).
% :- meta_predicate baseKB:fmt_holds_tcall(4,3,?,?,*).
% :- meta_predicate baseKB:add_game_dir(*,0).
% :- meta_predicate baseKB:merge_list_on_p(4,3,*,?,*,*,*).
% Restarting analysis ...
% Found new meta-predicates in iteration 3 (2.694 sec)
% :- meta_predicate baseKB:show_kb_via_pred_fmt(4,3,*,?,*).
% :- meta_predicate baseKB:show_kb_via_pred_3(4,3,*,?,*,*).
% Restarting analysis ...


        The predicates below are not defined. If these are defined
        at runtime using assert/1, use :- dynamic Name/Arity.

Warning: t:/devel/LogicmooDeveloperFramework/PrologMUD/pack/prologmud/runtime/init_mud_server.pl:380:
        baseKB:tag_pos/2, which is referenced by
                t:/devel/LogicmooDeveloperFramework/PrologMUD/pack/prologmud/prolog/prologmud/parsing/simple_decl_parser.pl:253:106: 1-st clause of baseKB:translation_for/5
Warning: t:/devel/LogicmooDeveloperFramework/PrologMUD/pack/prologmud/runtime/init_mud_server.pl:380:
        common_logic_sexpr:is_quantifier/1, which is referenced by
                t:/devel/LogicmooDeveloperFramework/PrologMUD/pack/logicmoo_base/prolog/logicmoo/snark/common_logic_sexpr.pl:87:54: 3-th clause of common_logic_sexpr:sexpr_sterm_to_pterm/2
Warning: t:/devel/LogicmooDeveloperFramework/PrologMUD/pack/prologmud/runtime/init_mud_server.pl:380:
        common_logic_snark:~/1, which is referenced by
                t:/devel/LogicmooDeveloperFramework/PrologMUD/pack/logicmoo_base/prolog/logicmoo/snark/common_logic_snark.pl:1338:29: 1-st clause of common_logic_snark:why_to_id/3
Warning: t:/devel/LogicmooDeveloperFramework/PrologMUD/pack/prologmud/runtime/init_mud_server.pl:380:
        mpred_pfc:maybe_prepend_mt/3, which is referenced by
                t:/devel/LogicmooDeveloperFramework/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl:410:51: 1-st clause of mpred_pfc:fix_mp/3
Warning: t:/devel/LogicmooDeveloperFramework/PrologMUD/pack/prologmud/runtime/init_mud_server.pl:380:
        mpred_storage:is_known_trew/1, which is referenced by
                t:/devel/LogicmooDeveloperFramework/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_storage.pl:494:23: 3-th clause of mpred_storage:fact_checked/2
Warning: t:/devel/LogicmooDeveloperFramework/PrologMUD/pack/prologmud/runtime/init_mud_server.pl:380:
        mpred_stubs_file_module:t/3, which is referenced by
                t:/devel/LogicmooDeveloperFramework/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_stubs.pl:888:8: 4-th clause of mpred_stubs_file_module:call_for_literal_db2/3
Warning: t:/devel/LogicmooDeveloperFramework/PrologMUD/pack/prologmud/runtime/init_mud_server.pl:380:
        mpred_type_constraints:genls/2, which is referenced by
                t:/devel/LogicmooDeveloperFramework/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_constraints.pl:250:34: 1-st clause of mpred_type_constraints:max_isa_l/2
Warning: t:/devel/LogicmooDeveloperFramework/PrologMUD/pack/prologmud/runtime/init_mud_server.pl:380:
        mpred_type_constraints:lambda/5, which is referenced by
                t:/devel/LogicmooDeveloperFramework/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_constraints.pl:242:34: 1-st clause of mpred_type_constraints:min_isa_l/2
Warning: t:/devel/LogicmooDeveloperFramework/PrologMUD/pack/prologmud/runtime/init_mud_server.pl:380:
        mpred_type_constraints:t/2, which is referenced by
                t:/devel/LogicmooDeveloperFramework/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_constraints.pl:603:56: 1-st clause of mpred_type_constraints:type_size/2
Warning: t:/devel/LogicmooDeveloperFramework/PrologMUD/pack/prologmud/runtime/init_mud_server.pl:380:
        logicmoo_util_shared_dynamic:decl_as/1 is declared as meta_predicate decl_as(+), but has no clauses

