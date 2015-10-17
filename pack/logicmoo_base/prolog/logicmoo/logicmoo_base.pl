/** <module> 
% File used as storage place for all predicates which change as
% the world is run.
%
% props(Obj,height(ObjHt)) == k(height,Obj,ObjHt) == rdf(Obj,height,ObjHt) == height(Obj,ObjHt)
% padd(Obj,height(ObjHt)) == padd(height,Obj,ObjHt,...) == add(QueryForm)
% kretract[all](Obj,height(ObjHt)) == kretract[all](Obj,height,ObjHt) == pretract[all](height,Obj,ObjHt) == del[all](QueryForm)
% keraseall(AnyTerm).
%
%
% Dec 13, 2035
% Douglas Miles
*/
:- module(lmbase,[ensure_mpred_system/0,enable_mpred_system/1,disable_mpred_system/1]).

:- multifile '$si$':'$was_imported_kb_content$'/2.
:- dynamic '$si$':'$was_imported_kb_content$'/2.
:- discontiguous('$si$':'$was_imported_kb_content$'/2).

:- multifile(logicmoo_util_help:mpred_is_impl_file/1).
:- dynamic(logicmoo_util_help:mpred_is_impl_file/1).


:- multifile lmconf:startup_option/2. 
:- dynamic lmconf:startup_option/2. 
:- multifile lmconf:mpred_system_status/2.
:- dynamic lmconf:mpred_system_status/2.
:- thread_local t_l:disable_px/0.

lmconf:startup_option(datalog,sanity). %  Run datalog sanity tests while starting
lmconf:startup_option(clif,sanity). %  Run datalog sanity tests while starting

:- ensure_loaded(logicmoo_utils).
:- asserta(lmconf:pfcManageHybrids).

% ========================================
% lmconf:mpred_user_kb/1
% ========================================

:- dynamic lmconf:mpred_user_kb/1.
:- export(lmconf:mpred_user_kb/1).
:- lmconf:mpred_user_kb(_)->true;('$module'(M,M),asserta(lmconf:mpred_user_kb(M))).
:- import(lmconf:mpred_user_kb/1).

% ========================================
% lmconf:mpred_system_kb/1
% ========================================

% TODO uncomment the next line without breaking it all!
% lmconf:use_cyc_database.

:- dynamic lmconf:mpred_system_kb/1.
:- retractall(lmconf:mpred_system_kb(_)).
:- export(lmconf:mpred_system_kb/1).
:- lmconf:mpred_system_kb(_)->true;(context_module(_M),asserta(lmconf:mpred_system_kb(lmbase))).
:- import(lmconf:mpred_system_kb/1).
:- mpred_system_kb(M),dmsg(mpred_system_kb=M).

% ================================================
% DBASE_T System
% ================================================

:- multifile(logicmoo_util_help:mpred_is_impl_file/1).
:- dynamic(logicmoo_util_help:mpred_is_impl_file/1).
logicmoo_util_help:mpred_is_impl_file(mpred/A):-nonvar(A).

load_mpred_system(_Ctx):-  !,use_module(logicmoo(mpred/mpred_loader)).
load_mpred_system(_Ctx):-  lmconf:mpred_system_kb(Sys),
   with_mutex(mpred_system_mutex,forall(logicmoo_util_help:mpred_is_impl_file(File),     
     (( Sys:use_module(logicmoo_utils),trace, call((Sys:w_tl(t_l:disable_px,Sys:ensure_loaded(File)))))))).

:- export(enable_mpred_system/1).
enable_mpred_system(Ctx):- with_mutex(mpred_system_mutex,lmconf:enable_mpred_system0(Ctx)).

:- export(disable_mpred_system/1).
disable_mpred_system(Ctx):- with_mutex(mpred_system_mutex,lmconf:disable_mpred_system0(Ctx)).

:- thread_local t_l:side_effect_ok/0.

lmconf:enable_mpred_system0(Ctx):- lmconf:mpred_system_status(Ctx,enabled),!.
lmconf:enable_mpred_system0(Ctx):-    
   retractall(lmconf:mpred_system_status(Ctx,_)),
   load_mpred_system(Ctx),
   must(current_predicate(_,mred_loader:mpred_expander(_,_,_,_))),
   meta_predicate(mpred_loader:mpred_expander(+,+,+,-)),   
   asserta_if_new((user:term_expansion(I,O):- mpred_expander(term,user,I,O))),
   asserta_if_new((system:goal_expansion(I,O):- mpred_expander(goal,system,I,O))),
   asserta_if_new((Ctx:term_expansion(I,O):- mpred_expander(term,Ctx,I,O))),
   asserta_if_new((Ctx:goal_expansion(I,O):- mpred_expander(goal,Ctx,I,O))),
   asserta(lmconf:mpred_system_status(Ctx,enabled)),
   lmconf:mpred_system_kb(Sys), 
   Sys:w_tl(t_l:side_effect_ok,doall(Ctx:call_no_cuts(_M:lmconf:module_local_init))).

lmconf:disable_mpred_system0(Ctx):- lmconf:mpred_system_status(Ctx,disabled),!.
lmconf:disable_mpred_system0(Ctx):-    
   retractall(lmconf:mpred_system_status(Ctx,_)),
   asserta(lmconf:mpred_system_status(Ctx,disabled)),
   % one day unload_mpred_system(Ctx),
   %retractall((user:term_expansion(I,O):- mpred_expander(term,user,I,O))),
   %retractall((system:goal_expansion(I,O):- mpred_expander(goal,system,I,O))),
   retractall((Ctx:term_expansion(I,O):- mpred_expander(term,Ctx,I,O))),
   retractall((Ctx:goal_expansion(I,O):- mpred_expander(goal,Ctx,I,O))),!.
   
:- module_transparent ensure_mpred_system/0.
ensure_mpred_system:- context_module(M),enable_mpred_system(M).

% :- initialization(add_library_search_path('.',[ './mpred/*.pl','./snark/*.pl'])).
:- add_library_search_path('./mpred/',[ '*.pl']).
:- add_library_search_path('./snark/',[ '*.pl']).
% :- add_library_search_path('./plarkc/',[ '*.pl']).
% :- add_library_search_path('./pttp/',[ 'dbase_i_mpred_*.pl']).


