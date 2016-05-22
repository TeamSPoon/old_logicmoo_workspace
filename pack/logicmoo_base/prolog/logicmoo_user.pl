/*  LogicMOO User Modules Setup
%
%
% Dec 13, 2035
% Douglas Miles

*/
:- if(( system:use_module(system:library('logicmoo/util/logicmoo_util_clause_expansion.pl')), push_modules)). 
:- endif.
:- module(logicmoo_user_file,
 [ /*
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
 op(300,fx,'-') */ ]).
% restore entry state
:- reset_modules.


:- use_module(library(logicmoo_utils)).
:- use_module(library(logicmoo_base)).

:- set_prolog_flag(pfc_booted,false).
:-  time((baseKB:ensure_mpred_file_loaded(baseKB:library(logicmoo/pfc/'autoexec.pfc')))).
:- set_prolog_flag(pfc_booted,true).
% :- forall(wsh_w:wrap_shared(F,A,ereq),ain((arity(F,A),pfcControlled(F),prologHybrid(F)))).

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
:- at_start((logicmoo_util_shared_dynamic:asserta_if_new((ereq(G):- !, call_u(G))))).
:- at_start((ignore((logicmoo_util_shared_dynamic:retract((ereq(G):- find_and_call(G))),fail)))).

:- reset_modules.
