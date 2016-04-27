/*   LogicMOO Base FOL/PFC Setup
% Dec 13, 2035
% Douglas Miles
*/
:- module(logicmoo_base,[ensure_mpred_system/0,enable_mpred_system/1,disable_mpred_system/1,fix_ops_for/1]).
:- use_module(library(logicmoo_utils)).

%:- autoload.
%:- set_prolog_IO(user_input,user_output,user_output).

:- multifile '$si$':'$was_imported_kb_content$'/2.
:- dynamic '$si$':'$was_imported_kb_content$'/2.
:- discontiguous('$si$':'$was_imported_kb_content$'/2).
:- multifile(lmconf:mpred_is_impl_file/1).
:- dynamic(lmconf:mpred_is_impl_file/1).

:- source_location(F,_),asserta(lmconf:ignore_file_mpreds(F)).

:- multifile lmconf:startup_option/2. 
:- dynamic lmconf:startup_option/2. 
:- multifile lmconf:mpred_system_status/2.
:- dynamic lmconf:mpred_system_status/2.
:- multifile(t_l:disable_px/0).
:- thread_local(t_l:disable_px/0).

:- multifile(lmconf:mpred_system_kb/1).
:- dynamic(lmconf:mpred_system_kb/1).

:- use_module(logicmoo_utils).
% :- initialization(add_library_search_path('.',[ './mpred/*.pl','./snark/*.pl'])).
:- dmsg("Adding logicmoo/[mpred,snark] to autoload path",[]).
:- add_library_search_path('./logicmoo/mpred/',[ '*.pl']).
:- add_library_search_path('./logicmoo/snark/',[ '*.pl']).
:- add_library_search_path('./logicmoo/',[ '*.pl']).
% :- add_library_search_path('./plarkc/',[ '*.pl']).
% :- add_library_search_path('./pttp/',[ 'dbase_i_mpred_*.pl']).



% ========================================
% lmconf:mpred_system_kb/1
% ========================================


%= 	 	 

%% mpred_system_kb( ?VALUE1) is semidet.
%
% Hook To [lmconf:mpred_system_kb/1] For Module Logicmoo_base.
% Managed Predicate System Knowledge Base.
%
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

%= 	 	 

%% mpred_is_impl_file( ?A) is semidet.
%
% Hook To [lmconf:mpred_is_impl_file/1] For Module Logicmoo_base.
% Managed Predicate If Is A Implimentation File.
%
lmconf:mpred_is_impl_file(mpred/A):-nonvar(A).


%= 	 	 

%% load_mpred_system( ?Ctx) is semidet.
%
% Load Managed Predicate System.
%
load_mpred_system(Ctx):-  !,Ctx:use_module(logicmoo(mpred/mpred_userkb)).
load_mpred_system(Ctx):-  lmconf:mpred_system_kb(Sys),
   with_mutex(mpred_system_mutex,forall(lmconf:mpred_is_impl_file(File),     
     (( Ctx:use_module(logicmoo_utils), call((Sys:w_tl(t_l:disable_px,Sys:ensure_loaded(File)))))))).

:- export(enable_mpred_system/1).
%% enable_mpred_system(+Module) is det.
% Begin considering forward and meta programming rules into a Prolog module.

%= 	 	 

%% enable_mpred_system( ?Module) is semidet.
%
% Enable Managed Predicate System.
%
enable_mpred_system(Module):- with_mutex(mpred_system_mutex,lmconf:enable_mpred_system0(Module)).

:- export(disable_mpred_system/1).
%% disable_mpred_system(+Module) is det.
% Disable tasks that considering forward and meta programming rules into a Prolog module.

%= 	 	 

%% disable_mpred_system( ?Module) is semidet.
%
% Disable Managed Predicate System.
%
disable_mpred_system(Module):- with_mutex(mpred_system_mutex,lmconf:disable_mpred_system0(Module)).

:- thread_local t_l:side_effect_ok/0.


%= 	 	 

%% enable_mpred_system0( ?Module) is semidet.
%
% Hook To [lmconf:enable_mpred_system0/1] For Module Logicmoo_base.
% Enable Managed Predicate System Primary Helper.
%
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


%= 	 	 

%% disable_mpred_system0( ?Module) is semidet.
%
% Hook To [lmconf:disable_mpred_system0/1] For Module Logicmoo_base.
% Disable Managed Predicate System Primary Helper.
%
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

%= 	 	 

%% ensure_mpred_system is semidet.
%
% Ensure Managed Predicate System.
%
ensure_mpred_system:- source_context_module(M),enable_mpred_system(M).


:- lmconf:mpred_system_kb(M),dmsg(system_kb=M).


:-  baseKB:use_module(library(logicmoo/mpred/mpred_userkb)).
:- time((baseKB:ensure_mpred_file_loaded(baseKB:library(logicmoo/pfc/'system_markers.pfc')))).

logicmoo_base_module(mpred_kb_ops).
logicmoo_base_module(mpred_props).
logicmoo_base_module(mpred_pfc).
logicmoo_base_module(mpred_stubs).
logicmoo_base_module(mpred_type_isa).

/*
:-forall(logicmoo_base_module(M),
  (add_import_module(baseKB,M,end),
   add_import_module(logicmoo_user,M,end))).
*/

:- autoload([verbose(false)]).

% 	 	 
%% fix_ops_for( ?VALUE1) is semidet.
%
% Fix Oper.s For.
%
fix_ops_for(CM):-
 op(1199,fx,CM:('==>')), 
 op(1190,xfx,CM:('::::')),
 op(1180,xfx,CM:('==>')),
 op(1170,xfx,CM:('<==>')),  
 op(1160,xfx,CM:('<-')),
 op(1150,xfx,CM:('=>')),
 op(1140,xfx,CM:('<=')),
 op(1130,xfx,CM:('<=>')), 
 op(600,yfx,CM:('&')), 
 op(600,yfx,CM:('v')),
 op(350,xfx,CM:('xor')),
 op(300,fx,CM:('~')),
 op(300,fx,CM:('-')).


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
:- Six = 6, set_prolog_stack(global, limit(Six*10**9)),set_prolog_stack(local, limit(Six*10**9)),set_prolog_stack(trail, limit(Six*10**9)).
*/

:- must(add_library_search_path('./logicmoo/mpred_online/',[ '*.pl'])).


:- sanity( \+predicate_property(baseKB:_,exported)).
% :- sanity( \+predicate_property(logicmoo_user:_,exported)).

:- do_gc.

:- forall(retract(wsh_w:wrap_shared(F,A,ereq)),ain((arity(F,A),pfcControlled(F),prologHybrid(F)))).

:- asserta_if_new((user:term_expansion(I,O):- with_umt_l(mpred_expander(term,user,I,O)))).
:- asserta_if_new((system:goal_expansion(I,O):- hotrace(with_umt_l(mpred_expander(goal,system,I,O))))).

