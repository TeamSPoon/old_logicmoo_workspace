#!/usr/bin/env swipl
/*
 Logic Engine startup

*/
:- multifile(prolog:make_hook/2).
:- dynamic(prolog:make_hook/2).

:- if(\+ current_module(baseKB)).
:- threads.
:- set_prolog_flag(logicmoo_qsave,true).
:- [init_cliopatria].
:- else.
:- set_prolog_flag(logicmoo_qsave,false).
% :- set_prolog_flag(lm_expanders,false).
:- statistics.
:- endif.

:- unload_file(logicmoo_repl).

:- meta_predicate(load_library_system(:)).
load_library_system(M:File):- load_library_system(M,File). 
load_library_system(M,File):- during_boot(gripe_time(40,(if_file_exists(ensure_loaded(M:File))))).


:- '$set_source_module'(baseKB).
:- '$set_typein_module'(baseKB).

:- set_prolog_flag(do_renames,restore).
:- baseKB:use_module(library(dif)).

:- baseKB:import(dif:dif/2).
:- baseKB:export(dif:dif/2).
:- use_module(library(prolog_predicate)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CYC Alignment util
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- set_prolog_flag(do_renames,restore).
% :- baseKB:unload_file(logicmoo('plarkc/logicmoo_i_cyc_rewriting')).
:- user:ensure_loaded(logicmoo('plarkc/logicmoo_i_cyc_rewriting')).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LOAD BIG CYC KB ? 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- during_boot(set_prolog_flag(do_renames,restore)).
:- gripe_time(60,user:ensure_loaded(logicmoo(plarkc/'logicmoo_i_cyc_kb_tinykb.pfc'))).
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
:- user:unload_file(library(logicmoo_user)).
:- user:ensure_loaded(library(logicmoo_user)).



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



%:- if(current_prolog_flag(logicmoo_qsave,true)).
:- statistics.
:- baseKB:qsave_lm(lm_repl).
%:- endif.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LOAD CYC KB EXTENSIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- check_clause_counts.

% :-  baseKB:load_library_system(logicmoo(logicmoo_plarkc)).

:-  with_no_mpred_expansions(baseKB:load_library_system(logicmoo(plarkc/logicmoo_i_cyc_kb))).

:- after_boot((set_prolog_flag(pfc_booted,true),flag_call(logicmoo_debug=true),set_prolog_flag(read_attvars,false))).


% :- baseKB:load_library_system(logicmoo(logicmoo_engine)).

% :- loadTinyKB. % 

%{tinyKB(C,MT,STR)},
%  {functor(C,F,_),F\==implies,F\==implies,cycLToMpred(C,C0),fully_expand(=>C0,P)} ==> tiny_clif(P).

{flatTrans(G)}==>{baseKB:listing(G/2)}.

:- ls.

:- baseKB:reconsult(logicmoo('snark/common_logic_sumo.pfc')).

:-ain('==>'({tinyKB(C,_MT,_STR),once((cycLToMpred(C,C0),fully_expand('==>'(C0),C1),
  unnumbervars(C1,P),wdmsg(tiny_clif(P)))),(ground(P)->O=P;O=rule(P))}, O)).


% 6219936200949492 1395.87
