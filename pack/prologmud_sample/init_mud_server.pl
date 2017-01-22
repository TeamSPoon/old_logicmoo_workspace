#!/usr/bin/env swipl
% NEW
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


:- use_module(library(http/http_files)).
:- use_module(pengine_sandbox:library(semweb/rdf_db)).
:- ensure_loaded(run).

:-  abolish(rdf_rewrite:arity,2),  % clause(rdf_rewrite:arity(A, B),functor(A, _, B),R),erase(R),
  asserta((rdf_rewrite:arity(A, B) :- (compound(A),functor(A, _, B)))).

:- profile(true).

:-if((multifile(baseKB:ignore_file_mpreds/1),dynamic(baseKB:ignore_file_mpreds/1),
   (prolog_load_context(source,F1) -> asserta(baseKB:ignore_file_mpreds(F1)) ; true),
   (prolog_load_context(file,F) -> asserta(baseKB:ignore_file_mpreds(F)) ; true))).
:-endif.


:- notrace((user:ensure_loaded(setup_paths))).
:- set_prolog_flag(double_quotes,string).
:- notrace((user:ensure_loaded(library(logicmoo_utils)))).
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
:- set_prolog_flag(logicmoo_debug,false).

/*
:- set_prolog_flag(logicmoo_debug,true).
:- set_prolog_flag(unsafe_speedups,false).
*/

% ==========================================================
% Sanity tests that first run whenever a person stats the MUD to see if there are regressions in the system
% ==========================================================

:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )).
%:- lmce:reset_modules.
:- endif.
:- dynamic(lmcache:prolog_tn_server/1).

:- ensure_loaded(library(prolog_server)).
:- getenv_safe('LOGICMOO_PORT',Was,3000),
   WebPort is Was + 1023,
   prolog_server(WebPort, [allow(_)]),
   asserta(lmcache:prolog_tn_server(WebPort)).

:- lmcache:prolog_tn_server(_)->true;
  (    getenv_safe('LOGICMOO_PORT',Was,3000),
   WebPort is Was + 2023,
   prolog_server(WebPort, [allow(_)]),
   asserta(lmcache:prolog_tn_server(WebPort))).

:- http_handler('/hmud/', http_reply_from_files(pack(hMUD), []), [prefix]).

:- if(  \+ (is_thread(prolog_server),thread_property(prolog_server,status(running)))).
:- prolog_server(4023, [allow(_)]).
:- endif.

:- ignore(shell('killall perl ; ../hmud/policyd')).

unsafe_preds_init(W,shell,2):-predicate_property(shell(_,_),imported_from(W)).
unsafe_preds_init(W,shell,1):-predicate_property(shell(_),imported_from(W)).
unsafe_preds_init(W,shell,0):-predicate_property(shell,imported_from(W)).
%unsafe_preds_init(M,F,A):-M=files_ex,current_predicate(M:F/A),member(X,[delete]),atom_contains(F,X).
unsafe_preds_init(M,F,A):-M=files_ex,current_predicate(M:F/A),member(X,[delete,copy]),atom_contains(F,X).
%unsafe_preds_init(M,F,A):-M=process,current_predicate(M:F/A),member(X,[kill,create]),atom_contains(F,X).
unsafe_preds_init(M,F,A):-M=process,current_predicate(M:F/A),member(X,[kill]),atom_contains(F,X).
unsafe_preds_init(M,F,A):-M=system,member(F,[shell,halt]),current_predicate(M:F/A).

% end_of_file.

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

% :- [run].
:- endif.

%:- use_listing_vars.

% Loaded LogicMOO Code!!!
%:- profile(ensure_loaded(logicmoo_repl)).
%:- ensure_loaded(daydream).
:- ensure_loaded(multivar).
:- ensure_loaded(udt).
:- ensure_loaded(atts).
:- ensure_loaded(logicmoo_repl).


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


:- must((statistics(cputime,X),X<65)).  % was 52

:- kill_unsafe_preds.

:- asserta((end_of_file:-prolog)).

%:- prolog,trace.

:- include(init_mud_server_run).



