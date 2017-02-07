#!/usr/bin/env swipl
% NEW
/* * module  MUD server startup script in SWI-Prolog

*/
% restore entry state
% :- module(init_mud_server,[]).
:- set_prolog_flag(access_level,system).
:- multifile(prolog:make_hook/2).
:- dynamic(prolog:make_hook/2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DURING/AFTER BOOT HOOKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- meta_predicate(system:during_boot(:)).
system:during_boot(Goal):- initialization(Goal,now),after_boot(Goal). % initialization(Goal,after_load),initialization(Goal,restore)
system:after_boot_call(How):- forall(system:after_boot_goal(Goal),call(How,Goal)).
system:after_boot_call:-system:after_boot_call(must_det).
:- meta_predicate(system:after_boot(:)).
system:after_boot(Goal):- assertz(after_boot_goal(Goal)).
:- meta_predicate(system:after_boot_sanity_test(:)).
system:after_boot_sanity_test(M:Goal):- after_boot(M:sanity(Goal)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DEFUALT PROLOG FLAGS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- '$set_source_module'(user).
:- set_prolog_flag(qcompile,part).
:- set_prolog_flag(do_renames,never).
:- set_prolog_flag(dialect_pfc,false).
:- set_prolog_stack(global, limit(32*10**9)).
:- set_prolog_stack(local, limit(32*10**9)).
:- set_prolog_stack(trail, limit(32*10**9)).
:- set_prolog_flag(double_quotes,string).
:- set_prolog_flag(autoload_logicmoo,false).
:- if( \+ current_module(prolog_stack)).
:- system:use_module(library(prolog_stack)).
 prolog_stack:stack_guard(none).
:- endif.

setup_for_debug :- set_prolog_flag(report_error,true),set_prolog_flag(debug_on_error,true),
   set_prolog_flag(debugger_write_options,[quoted(true), portray(true), max_depth(1000), attributes(portray)]),
   set_prolog_flag(generate_debug_info,true).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LOAD LOGTALK
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- consult('/usr/share/logtalk/integration/logtalk_swi').
:- listing({}/1).

:- during_boot(setup_for_debug).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LOAD PARTS OF SYSTEM EARLY
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- '$set_typein_module'(baseKB).
:- '$set_source_module'(baseKB).
/*
:- set_prolog_flag(access_level,system).
:- set_prolog_flag(compile_meta_arguments,false). % default is false
*/
:- use_module(library(base32)).
:- use_module(library(prolog_history)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(threadutil)).
:- use_module(library(shell)).
:- use_module(library(console_input)).
:- use_module(library(editline)).
:- if(current_predicate(system:mode/1)).
:- system:use_module(library(quintus),except([mode/1])). 
:- else.
:- system:use_module(library(quintus)). 
:- endif.
:- system:use_module(library(dialect/ifprolog),except([op(_,_,_)])).
:- abolish(system:time/1).
:- system:use_module(library(dialect/hprolog)).
:- abolish(hprolog:time/1).
:- system:use_module(library(statistics),[time/1]).
:- system:use_module(library(statistics)).
:- baseKB:use_module(library(statistics),[time/1]).
:- autoload([verbose(false)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LOAD CYC KB LOADER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- consult(baseKB:library('pldata/plkb7166/kb7166')).
:- qcompile_kb7166.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MAKE SURE CLIOPATRIA RUNS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- use_module(pengine_sandbox:library(semweb/rdf_db)).
% :- unsetenv('DISPLAY').
:- user:consult(run_cliopatria).
:-  abolish(rdf_rewrite:arity,2),  % clause(rdf_rewrite:arity(A, B),functor(A, _, B),R),erase(R),
   asserta((rdf_rewrite:arity(A, B) :- (compound(A),functor(A, _, B)))). % AND DOES NOT BREAK LOGICMOO
ensure_webserver_p(Port):- format(atom(A),'httpd@~w_1',[Port]),(thread_property(N,status(V)),N==A)->V=running,!.
ensure_webserver_p(Port) :-catch((thread_httpd:http_server(http_dispatch,[ port(Port), workers(16) ])),E,(writeln(E),fail)).
ensure_webserver_3020:- (getenv('LOGICMOO_PORT',Was);Was=3000),
   WebPort is Was + 20, ensure_webserver_p(WebPort).

:- during_boot(ensure_webserver_3020).

:- autoload([verbose(false)]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ensure hMUD
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- if(exists_directory(hmud)).
:- absolute_file_name('hmud/',O),
   during_boot(http_handler('/hmud/', http_reply_from_files(O, []), [prefix])).
:- during_boot(ignore(catch(shell('killall perl ; ./hmud/policyd'),E,dmsg(E)))).
:- else.
:- during_boot(http_handler('/hmud/', http_reply_from_files(pack(hMUD), []), [prefix])).
:- during_boot(ignore(catch(shell('killall perl ; ../hmud/policyd'),E,dmsg(E)))).
:- endif.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% (X)WINDOWS (DE)BUGGERY
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_x_ide:- \+ current_prolog_flag(logicmoo_headless,true),!.
start_x_ide:- prolog_ide(thread_monitor),prolog_ide(debug_monitor),
   % prolog_ide(open_debug_status),
   guitracer,
   use_module(library(pce_prolog_xref)),
   noguitracer.

:- after_boot(start_x_ide).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SETUP PATHS FOR PROLOGMUD/LOGICMOO 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- during_boot((user:ensure_loaded(setup_paths))).

:- use_module(library('logicmoo/util/logicmoo_util_file_scope')).
:- use_module(library('logicmoo/util/logicmoo_util_clause_expansion')).


% :- during_boot((sanity((lmce:current_smt(SM,M),writeln(current_smt(SM,M)))))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LOAD LOGICMOO UTILS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- user:ensure_loaded(library(logicmoo_utils)).
prolog:make_hook(after, C):- wdmsg(prolog:make_hook(after, C)).
prolog:make_hook(before, C):- wdmsg(prolog:make_hook(before, C)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SETUP LOGICMOO OPERATORS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- w_tl(set_prolog_flag(access_level,system),
 ((op(200,fy,'-'),op(300,fx,'-'),
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
 op(1199,fx,('==>'))))).

:- multifile prolog:message//1, prolog:message_hook/3.
% prolog:message(ignored_weak_import(Into, From:PI))--> { nonvar(Into),Into \== system,dtrace(dmsg(ignored_weak_import(Into, From:PI))),fail}.
% prolog:message(Into)--> { nonvar(Into),functor(Into,_F,A),A>1,arg(1,Into,N),\+ number(N),dtrace(wdmsg(Into)),fail}.
% prolog:message_hook(T,error,Warn):- dtrace(wdmsg(nessage_hook(T,warning,Warn))),fail.
% prolog:message_hook(T,warning,Warn):- dtrace(wdmsg(nessage_hook(T,warning,Warn))),fail.


/*
:- flag_call(unsafe_speedups=true).
:- flag_call(logicmoo_debug=0).
:- flag_call(logicmoo_debug=2).
% ?- flag_call(unsafe_speedups == true) .
:- flag_call(unsafe_speedups=false).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sanity tests that first run whenever a person stats the MUD to see if there are regressions in the system
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )).
%:- lmce:reset_modules.
:- endif.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ensure RPC Telnet
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic(lmcache:prolog_tn_server/1).

prolog_tn_server:- is_thread(prolog_server),thread_property(prolog_server,status(running)),!.
prolog_tn_server:- 
   ensure_loaded(library(prolog_server)),
   getenv_safe('LOGICMOO_PORT',Was,3000),
   WebPort is Was + 1023,
   catch((prolog_server(WebPort, [allow(_)])),_,fail),!,
   asserta(lmcache:prolog_tn_server(WebPort)).
prolog_tn_server:- is_thread(prolog_server),thread_property(prolog_server,status(running)),!.
prolog_tn_server:- catch((prolog_server(4023, [allow(_)])),_,fail).
prolog_tn_server:- catch((prolog_server(5023, [allow(_)])),_,fail),!.

:- during_boot(prolog_tn_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Various RPC Dangers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
unsafe_preds_init(W,shell,2):-predicate_property(shell(_,_),imported_from(W)).
unsafe_preds_init(W,shell,1):-predicate_property(shell(_),imported_from(W)).
unsafe_preds_init(W,shell,0):-predicate_property(shell,imported_from(W)).
%unsafe_preds_init(M,F,A):-M=files_ex,current_predicate(M:F/A),member(X,[delete]),atom_contains(F,X).
unsafe_preds_init(M,F,A):-M=files_ex,current_predicate(M:F/A),member(X,[delete,copy]),atom_contains(F,X).
%unsafe_preds_init(M,F,A):-M=process,current_predicate(M:F/A),member(X,[kill,create]),atom_contains(F,X).
unsafe_preds_init(M,F,A):-M=process,current_predicate(M:F/A),member(X,[kill]),atom_contains(F,X).
unsafe_preds_init(M,F,A):-M=system,member(F,[shell,halt]),current_predicate(M:F/A).

system:kill_unsafe_preds:- 
% (Thus restoring saved state)
   set_prolog_flag(access_level,system),
   
   % [Optionaly] Solve the Halting problem
   unlock_predicate(system:halt/0),
   redefine_system_predicate(system:halt/0),
   abolish(system:halt,0),
   asserta((system:halt :- format('the halting problem is now solved!'))),
   lock_predicate(system:halt/0),   
   unlock_predicate(system:halt/1),
   redefine_system_predicate(system:halt/1),
   abolish(system:halt,1),
   asserta((system:halt(_) :- format('the halting problem was already solved!'))),
   lock_predicate(system:halt/1),
   (dmsg("kill_unsafe_preds!"),w_tl(set_prolog_flag(access_level,system),
     forall(unsafe_preds_init(M,F,A),bugger:remove_pred(M,F,A)))),
   dmsg("the halting problem is now solved!"). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IRC EGGDROP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- if(exists_source(library(eggdrop))).
:- ensure_loaded(user:library(eggdrop)).
:- during_boot((user:egg_go)).
:- endif.

:- during_boot(asserta_if_new(baseKB:load_mud_www)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ?- listing.  (uses varaibles)
% slows the system startup down consideraly
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%:- use_listing_vars.

:- set_prolog_flag(toplevel_print_factorized,true). % default false
:- set_prolog_flag(toplevel_print_anon,true).
:- set_prolog_flag(toplevel_mode,backtracking). % OR recursive 
:- set_prolog_flag(write_attributes,portray).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LOAD BIG CYC KB
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- after_boot(dmsg(qconsult_kb7166)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CYC Alignment util
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- baseKB:consult(logicmoo('plarkc/logicmoo_i_cyc_rewriting')).
:- during_boot(set_prolog_flag(do_renames,restore)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% These are probly loaded by other modules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%:- ensure_loaded(library(multivar)).
%:- ensure_loaded(library(udt)).
%:- ensure_loaded(library(atts)).
%:- use_module(library(persistency)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [Optionaly] Load the EXTRA Logicmoo WWW System
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% :- ensure_loaded(library(logicmoo/mpred_online/mpred_www)).
:- (if_file_exists(ensure_loaded(library(logicmoo/logicmoo_run_pldoc)))).
:- (if_file_exists(ensure_loaded(library(logicmoo/logicmoo_run_swish)))).


:- after_boot((system:kill_unsafe_preds)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BINA43 Code!!!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%:- during_boot(ensure_loaded(daydream)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INIT LOGICMOO (AUTOEXEC)  Load the infernce engine
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- user:consult(library(logicmoo_user)).
% :- baseKB:ensure_loaded(logicmoo(logicmoo_plarkc)).
:- baseKB:ensure_loaded(logicmoo(plarkc/logicmoo_i_cyc_kb)).
% :- gripe_time(40,with_no_mpred_expansions(if_file_exists(ensure_loaded(logicmoo(logicmoo_engine))))).



iRR7_test:-
 baseKB:(
    ain(isa(iRR7,tRR)),
    ain(genls(tRR,tRRP)),
    (\+ tRRP(iRR7) -> (xlisting(iRR7),xlisting(tRRP)) ; true),
    must( isa(iRR7,tRR) ),
    must( isa(iRR7,tRRP) ),
    must( tRRP(iRR7) )).

% :- iRR7_test.

:- after_boot_sanity_test(iRR7_test).

decl_kb_dynamic_tests:-
 sanity(listing((kb_dynamic)/1)),
 kb_dynamic(baseKB:sanity_test/0),
 kb_dynamic(baseKB:regression_test/0),
 kb_dynamic(baseKB:feature_test/0),
 kb_dynamic((
        baseKB:feature_test/0,
        baseKB:mud_test/2,
        baseKB:regression_test/0,
        baseKB:sanity_test/0,
        baseKB:agent_call_command/2,
        action_info/2,
        type_action_info/3)).

:- decl_kb_dynamic_tests.


:- after_boot((set_prolog_flag(pfc_booted,true),flag_call(logicmoo_debug=true),set_prolog_flag(read_attvars,false))).

% :- statistics(cputime,X),dmsg(statistics(cputime,X)),must((X<70)).  % was 52


:- assert_setting01(lmconf:eachRule_Preconditional(true)).

:- set_prolog_flag(dialect_pfc,false).

:- file_begin(pl).

:- baseKB:consult(logicmoo(logicmoo_engine)).


% [Mostly Required] Load the Logicmoo Parser/Generator System
:- gripe_time(40,user:consult(library(parser_all))).


% [Mostly Required] Load the Logicmoo Plan Generator System
:- with_no_mpred_expansions(if_file_exists(user:consult(library(logicmoo/logicmoo_planner)))).


% [Required] Load the CYC Network Client and Logicmoo CycServer Emulator (currently server is disabled)
% :- with_no_mpred_expansions(if_file_exists(consult(library(logicmoo/logicmoo_u_cyc_api)))).

% [Optional] NOT YET Load the Logicmoo RDF/OWL Browser System
% % :- with_no_mpred_expansions(if_file_exists(user:consult(logicmoo(mpred_online/mpred_rdf)))).

% [Debugging] Normarily this set as 'true' can interfere with debugging
% :- set_prolog_flag(gc,true).
% Yet turning it off we cant even startup without crashing
% :- set_prolog_flag(gc,false).

% :- doall(printAll(current_prolog_flag(_N,_V))).
:- initialization(kill_unsafe_preds,restore).
:- if(\+ compiling).
:- initialization(kill_unsafe_preds).
:- endif.
% ==========================================================
% Regression tests that first run whenever a person stats the MUD on the public server
% ==========================================================


%:- consult(pack(logicmoo_base/t/examples/pfc/'sanity_col_as_unary.pfc')).
%:- consult(pack(logicmoo_base/t/examples/pfc/'sanity_birdt.pfc')).
%:- consult(pack(logicmoo_base/t/examples/pfc/'sanity_sv.pfc')).
%:- consult(pack(logicmoo_base/t/examples/pfc/'sanity_foob.pfc')).


:- if((gethostname(ubuntu),fail)). % INFO this fail is so we can start faster
:- show_entry(gripe_time(40, doall(baseKB:regression_test))).
:- endif.


% ==============================
% MUD SERVER CODE LOADS
% ==============================

% :- assert_setting01(lmconf:eachRule_Preconditional(isRuntime)).


%:- push_modules.
% [Required] load the mud system

:- if(\+ exists_source(prologmud(mud_loader))).
:- must((absolute_file_name(logicmoo('../../../prologmud/prolog/prologmud'),Dir),
                                               asserta(user:file_search_path(prologmud,Dir)))),
 must(exists_source(prologmud(mud_loader))).
:- endif.

% :- with_mpred_trace_exec(show_entry(gripe_time(40,consult(prologmud(mud_loader))))).
:- must(show_entry(gripe_time(40,consult(prologmud(mud_loader))))).
%:- lmce:reset_modules.


:- mpred_trace_exec.
:- ain(isLoaded(iSourceCode7)).
:- mpred_notrace_exec.

% ==============================
% MUD SERVER CODE STARTS
% ==============================


:- set_prolog_flag(dialect_pfc,false).
%:- flag_call(logicmoo_debug=true).

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

:- ain((localityOfObject(P,_)==>{put_in_world(P)})).

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
% :- nortrace,notrace.

:- set_prolog_flag(dialect_pfc,false).

:- endif.

:- file_begin(pl).

% [Optionaly] Start the telent server % iCommanderdata66
start_telnet:- on_x_log_cont(start_mud_telnet_4000).
:- after_boot(start_telnet).

add_history(O):- format(atom(A),'~q.',[O]),
      (current_prolog_flag(readline,editline) -> el_add_history(A) ; rl_add_history(A)).

add_history_ideas:- ignore(( current_prolog_flag(readline, true),
        add_history(start_telnet),
        add_history(help(match_regex/2)),
        add_history(list_undefined),
	add_history(user:consult(run_mud_game)),
	add_history(login_and_run))).
%:- after_boot(add_history_ideas).


% :- assert_setting01(lmconf:eachFact_Preconditional(isRuntime)).

% isa(starTrek,mtCycL).
lst :- ensure_loaded('../prologmud_sample/runtime/games/src_game_startrek/?*.pfc.pl').
% [Manditory] This loads the game and initializes so test can be ran
:- declare_load_dbase('../prologmud_sample/runtime/games/src_game_nani/a_nani_household.pfc.pl').

% [Optional] the following game files though can be loaded separate instead
:- declare_load_dbase('../prologmud_sample/runtime/games/src_game_nani/objs_misc_household.pfc.pl').
:- declare_load_dbase('../prologmud_sample/runtime/games/src_game_nani/?*.pfc.pl').

% [Optional] the following worlds are in version control in examples
% :- add_game_dir('../prologmud_sample/runtime/games/src_game_wumpus',prolog_repl).
% :- add_game_dir('../prologmud_sample/runtime/games/src_game_sims',prolog_repl).
% :- add_game_dir('../prologmud_sample/runtime/games/src_game_nani',prolog_repl).
% :- add_game_dir('../prologmud_sample/runtime/games/src_game_startrek',prolog_repl).

%:- check_clause_counts.

:- sanity(argIsa(genlPreds,2,_)).

:- after_boot_sanity_test(argIsa(genlPreds,2,_)).

:- dynamic(lmconf:eachRule_Preconditional/1).
:- dynamic(lmconf:eachFact_Preconditional/1).

:- assert_setting01(lmconf:eachRule_Preconditional(true)).
:- assert_setting01(lmconf:eachFact_Preconditional(true)).

% UNCOMMNET :- consult(library(parser_e2c)).

/*
:- after_boot_sanity_test((
  ignore((mpred_notrace_exec,
     flag_call(logicmoo_debug>true),
     ain(isa(iFoodRez2,tFood)),must(isa(iFoodRez2,tEatAble)))),
    must((parseIsa_Call(tEatAble,O,["food"],Rest),O=iFoodRez2,Rest=[])))).
*/

:- after_boot_sanity_test((gripe_time(1.0,must(coerce("s",vtDirection,_))))).
:- after_boot_sanity_test((gripe_time(2.0,must( \+ coerce(l,vtDirection,_))))).
:- after_boot_sanity_test((statistics)).

:- after_boot(flag_call(logicmoo_debug=false)).
:- during_boot(set_prolog_flag(unsafe_speedups,false)).

% :- after_boot_sanity_test(check_clause_counts).

:- lst,statistics.


lar:- ain(tSourceData(iWorldData8)),ain(isLoaded(iWorldData8)),ain(isRuntime),dmsg("Ctrl-D to start MUD"),prolog,login_and_run.


initialization_after_boot:- listing(system:after_boot_goal/1),dmsg(system:after_boot_call).

qsave_lm(LM):- statistics(globallimit,G),statistics(locallimit,L),statistics(traillimit,T),
  qsave_program(LM,[class(development),autoload(false),stand_alone(false),foreign(no_save),global(G),trail(T),local(L)]).


:- after_boot(lar).
:- initialization(initialization_after_boot,after_load).
:- initialization(initialization_after_boot,restore).

:- if(\+ exists_file(pass00)).
:- initialization(qsave_lm(pass00),after_load).
:- endif.

:- initialization(after_boot_call,after_load).
:- initialization(after_boot_call,restore).

