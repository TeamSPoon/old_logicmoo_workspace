/* <module> LogicMOO Base FOL/PFC Setup
% Dec 13, 2035
% Douglas Miles
*/
:- module(logicmoo_base,[ensure_mpred_system/0,enable_mpred_system/1,disable_mpred_system/1]).

:- multifile '$si$':'$was_imported_kb_content$'/2.
:- dynamic '$si$':'$was_imported_kb_content$'/2.
:- discontiguous('$si$':'$was_imported_kb_content$'/2).

:- multifile(lmconf:mpred_is_impl_file/1).
:- dynamic(lmconf:mpred_is_impl_file/1).

:- source_location(F,_),asserta(lmconf:never_registered_mpred_file(F)).

:- multifile lmconf:startup_option/2. 
:- dynamic lmconf:startup_option/2. 
:- multifile lmconf:mpred_system_status/2.
:- dynamic lmconf:mpred_system_status/2.
:- thread_local t_l:disable_px/0.

:- multifile(lmconf:mpred_system_kb/1).
:- dynamic(lmconf:mpred_system_kb/1).

:- use_module(logicmoo_utils).
% :- initialization(add_library_search_path('.',[ './mpred/*.pl','./snark/*.pl'])).
:-dmsg("Adding logicmoo/[mpred,snark] to autoload path",[]).
:- add_library_search_path('./mpred/',[ '*.pl']).
:- add_library_search_path('./snark/',[ '*.pl']).
% :- add_library_search_path('./plarkc/',[ '*.pl']).
% :- add_library_search_path('./pttp/',[ 'dbase_i_mpred_*.pl']).

% ========================================
% lmconf:mpred_system_kb/1
% ========================================

lmconf:mpred_system_kb(baseKB).


% lmconf:startup_option(datalog,sanity). %  Run datalog sanity tests while starting
% lmconf:startup_option(clif,sanity). %  Run datalog sanity tests while starting

:- ensure_loaded(logicmoo_utils).
:- asserta(lmconf:pfcManageHybrids).


% ================================================
% DBASE_T System
% ================================================

:- multifile(lmconf:mpred_is_impl_file/1).
:- dynamic(lmconf:mpred_is_impl_file/1).
lmconf:mpred_is_impl_file(mpred/A):-nonvar(A).

load_mpred_system(Ctx):-  !,Ctx:use_module(logicmoo(mpred/mpred_userkb)).
load_mpred_system(Ctx):-  lmconf:mpred_system_kb(Sys),
   with_mutex(mpred_system_mutex,forall(lmconf:mpred_is_impl_file(File),     
     (( Ctx:use_module(logicmoo_utils), call((Sys:w_tl(t_l:disable_px,Sys:ensure_loaded(File)))))))).

:- export(enable_mpred_system/1).
%% enable_mpred_system(+Module) is det.
% Begin considering forward and meta programming rules into a Prolog module.
enable_mpred_system(Module):- with_mutex(mpred_system_mutex,lmconf:enable_mpred_system0(Module)).

:- export(disable_mpred_system/1).
%% disable_mpred_system(+Module) is det.
% Disable tasks that considering forward and meta programming rules into a Prolog module.
disable_mpred_system(Module):- with_mutex(mpred_system_mutex,lmconf:disable_mpred_system0(Module)).

:- thread_local t_l:side_effect_ok/0.

lmconf:enable_mpred_system0(Module):- lmconf:mpred_system_status(Module,enabled),!.
lmconf:enable_mpred_system0(Module):- 
   set_user_abox(Module),
   retractall(lmconf:mpred_system_status(Module,_)),
   load_mpred_system(Module),
   must(current_predicate(_,mred_loader:mpred_expander(_,_,_,_))),
   meta_predicate(mpred_loader:mpred_expander(+,+,+,-)),   
   asserta_if_new((user:term_expansion(I,O):- mpred_expander(term,user,I,O))),
   asserta_if_new((system:goal_expansion(I,O):- mpred_expander(goal,system,I,O))),
   asserta_if_new((Module:term_expansion(I,O):- mpred_expander(term,Module,I,O))),
   asserta_if_new((Module:goal_expansion(I,O):- mpred_expander(goal,Module,I,O))),
   asserta(lmconf:mpred_system_status(Module,enabled)),
   Module:w_tl(t_l:side_effect_ok,doall(Module:call_no_cuts(lmconf:module_local_init))).

lmconf:disable_mpred_system0(Module):- lmconf:mpred_system_status(Module,disabled),!.
lmconf:disable_mpred_system0(Module):-    
   retractall(lmconf:mpred_system_status(Module,_)),
   asserta(lmconf:mpred_system_status(Module,disabled)),
   % one day unload_mpred_system(Module),
   %retractall((user:term_expansion(I,O):- mpred_expander(term,user,I,O))),
   %retractall((system:goal_expansion(I,O):- mpred_expander(goal,system,I,O))),
   retractall((Module:term_expansion(I,O):- mpred_expander(term,Module,I,O))),
   retractall((Module:goal_expansion(I,O):- mpred_expander(goal,Module,I,O))),!.
   
:- module_transparent ensure_mpred_system/0.
%% ensure_mpred_system is det.
% Ensure the "managed predicate" system and subsystems are available
ensure_mpred_system:- source_context_module(M),enable_mpred_system(M).


:- lmconf:mpred_system_kb(M),dmsg(system_kb=M).

