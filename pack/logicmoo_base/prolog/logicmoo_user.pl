/*  LogicMOO User Modules Setup
%
%
% Dec 13, 2035
% Douglas Miles

*/
:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )).
:- if(( ( current_prolog_flag(xref,true)) )).
:- module(logicmoo_user_module,
 [ 
 op(1199,fx,('==>')), 
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
 op(300,fx,'-')  ]).
:- endif.
:- endif.

% restore entry state
:- lmce:current_smt(SM,M),writeln(current_smt1(SM,M)).
:- lmce:reset_modules.
:- lmce:current_smt(SM,M),writeln(current_smt2(SM,M)).

:- if(prolog_load_context(module,system)).
:- set_prolog_flag(access_level,system).
:- endif.
:-
 op(1199,fx,('==>')), 
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
 op(300,fx,'-').
:- if(prolog_load_context(module,system)).
:- set_prolog_flag(access_level,user).
:- endif.

:- set_prolog_flag(pfc_booted,false).
:- current_prolog_flag(unsafe_speedups,_)->true;set_prolog_flag(unsafe_speedups,true).
:- ensure_loaded(library(logicmoo_utils)).
:- ensure_loaded(library(logicmoo_base)).
:- set_prolog_flag(pfc_booted,false).
:-  time((baseKB:ensure_mpred_file_loaded(baseKB:library(logicmoo/pfc/'autoexec.pfc')))).
:- set_prolog_flag(pfc_booted,true).

/*
:- set_prolog_flag(report_error,true).
:- set_prolog_flag(fileerrors,false).
:- set_prolog_flag(access_level,system).
:- set_prolog_flag(debug_on_error,true).
:- set_prolog_flag(debug,true).
:- set_prolog_flag(gc,false).
:- set_prolog_flag(gc,true).
:- set_prolog_flag(optimise,false).
:- set_prolog_flag(last_call_optimisation,false).
:- debug.
*/
:- at_start((Six = 6, set_prolog_stack(global, limit(Six*10**9)),set_prolog_stack(local, limit(Six*10**9)),set_prolog_stack(trail, limit(Six*10**9)))).
:- at_start((logicmoo_util_shared_dynamic:call(asserta_if_new,(ereq(G):- !, baseKB:call_u(G))))).
% :- at_start((ignore((logicmoo_util_shared_dynamic:call(retract,(ereq(G):- find_and_call(G))),fail)))).

:- set_prolog_flag(pfc_booted,true).
:- set_prolog_flag(retry_undefined,true).

