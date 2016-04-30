/*   
  LogicMOO Base FOL/PFC Setup
% Dec 13, 2035
% Douglas Miles

*/
:- if(('$current_source_module'(SM),'$current_typein_module'(M),
   system:multifile(baseKB:user_module_uses_base/2),
   catch(maybe_add_import_module(SM,baseKB,start),_,true),
   system:dynamic(baseKB:user_module_uses_base/2),
   system:asserta(baseKB:user_module_uses_base(SM,M)))).
:- endif.
:- module(baseKB,
 [
   config_mpred_system/0,
   enable_mpred_system/0,
   config_mpred_system/2,
   enable_mpred_system/2,
   show_kb_structure/0,
  user_module_uses_base/2,
  disable_mpred_system/2,
  fix_ops_for/1]).

:- ensure_loaded('./logicmoo/util/logicmoo_util_filesystem').

:- dynamic(baseKB:'$exported_op'/3).
:- multifile(baseKB:'$exported_op'/3).
:- discontiguous(baseKB:'$exported_op'/3).
% ========================================
% lmconf:mpred_system_kb/1
% ========================================
%% mpred_system_kb( ?VALUE1) is semidet.
%
% Hook To [lmconf:mpred_system_kb/1] For Usr Logicmoo_base.
% Managed Predicate System Knowledge Base.
%
:- multifile(lmconf:tbox_for/2).
:- multifile(lmconf:abox_for/2).
:- dynamic(lmconf:tbox_for/2).
:- dynamic(lmconf:abox_for/2).
lmconf:tbox_for(user,baseKB).
lmconf:abox_for(user,logicmoo_user).

config_mpred_system:- 
  baseKB:user_module_uses_base(SM,M),
  config_mpred_system(SM,M).

config_mpred_system(SM,M):-
  get_abox_for(SM,_),
  get_tbox_for(M,_), 
  show_kb_structure.


show_kb_structure:-
   baseKB:user_module_uses_base(SM,M),
   get_tbox_for(M,Sys),
   Sys:ensure_loaded(library(logicmoo_utils)),   
   get_abox_for(SM,Usr),
   wdmsg((source/typein=SM/M-->abox/tbox=Usr/Sys)),!.

/*
% lmconf:startup_option(datalog,sanity). %  Run datalog sanity tests while starting
% lmconf:startup_option(clif,sanity). %  Run datalog sanity tests while starting
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

% :- autoload([verbose(false)]).
lmconf:mpred_skipped_module(eggdrop).
:- forall(current_module(CM),system:assert(lmconf:mpred_skipped_module(CM))).
:- retractall(lmconf:mpred_skipped_module(pfc)).


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

% ================================================
% DBASE_T System
% ================================================

:- multifile(lmconf:mpred_is_impl_file/1).
:- dynamic(lmconf:mpred_is_impl_file/1).

%% lmconf:mpred_is_impl_file( ?A) is semidet.
%
% Hook To [lmconf:mpred_is_impl_file/1] For Usr Logicmoo_base.
% Managed Predicate If Is A Implimentation File.
%
lmconf:mpred_is_impl_file(mpred/A):-nonvar(A).
lmconf:mpred_is_impl_file(library('logicmoo/mpred/mpred_at_box.pl')).
lmconf:mpred_is_impl_file(library('logicmoo/mpred/mpred_type_args.pl')).
lmconf:mpred_is_impl_file(library('logicmoo/mpred/mpred_type_wff.pl')).
lmconf:mpred_is_impl_file(library('logicmoo/mpred/mpred_type_isa.pl')).
lmconf:mpred_is_impl_file(library('logicmoo/mpred/mpred_expansion.pl')).
lmconf:mpred_is_impl_file(library('logicmoo/mpred/mpred_loader.pl')).
lmconf:mpred_is_impl_file(library('logicmoo/mpred/mpred_pfc.pl')).
lmconf:mpred_is_impl_file(library('logicmoo/mpred/mpred_prolog_file.pl')).
lmconf:mpred_is_impl_file(library('logicmoo/mpred/mpred_stubs.pl')).
lmconf:mpred_is_impl_file(library('logicmoo/mpred/mpred_kb_ops.pl')).
lmconf:mpred_is_impl_file(library('logicmoo/mpred/mpred_userkb.pl')).
lmconf:mpred_is_impl_file(library('logicmoo/mpred/mpred_*.pl')).

:- baseKB:ensure_loaded(baseKB:library('logicmoo/snark/common_logic_at_box.pl')).
:- baseKB:ensure_loaded(baseKB:library('logicmoo/mpred/mpred_expansion.pl')).
:- baseKB:ensure_loaded(baseKB:library('logicmoo/mpred/mpred_kb_ops.pl')).
:- baseKB:ensure_loaded(baseKB:library('logicmoo/mpred/mpred_listing.pl')).
:- baseKB:ensure_loaded(baseKB:library('logicmoo/mpred/mpred_loader.pl')).
:- baseKB:ensure_loaded(baseKB:library('logicmoo/mpred/mpred_pfc.pl')).
:- baseKB:ensure_loaded(baseKB:library('logicmoo/mpred/mpred_prolog_file.pl')).
:- baseKB:ensure_loaded(baseKB:library('logicmoo/mpred/mpred_props.pl')).
:- baseKB:ensure_loaded(baseKB:library('logicmoo/mpred/mpred_storage.pl')).
:- baseKB:ensure_loaded(baseKB:library('logicmoo/mpred/mpred_stubs.pl')).
:- baseKB:ensure_loaded(baseKB:library('logicmoo/mpred/mpred_type_constraints.pl')).
:- baseKB:ensure_loaded(baseKB:library('logicmoo/mpred/mpred_type_isa.pl')).
:- baseKB:ensure_loaded(baseKB:library('logicmoo/mpred/mpred_type_naming.pl')).
:- baseKB:ensure_loaded(baseKB:library('logicmoo/mpred/mpred_type_wff.pl')).
:- baseKB:ensure_loaded(baseKB:library('logicmoo/mpred/mpred_hooks.pl')).
:- baseKB:ensure_loaded(baseKB:library('logicmoo/mpred/mpred_userkb.pl')).
:- baseKB:ensure_loaded(baseKB:library('logicmoo/mpred/mpred_type_args.pl')).
:- baseKB:ensure_loaded(baseKB:library('logicmoo/mpred/mpred_agenda.pl')).

%% assert_setting00( ?X) is semidet.
assert_setting00(M:P):-functor(P,_,A),duplicate_term(P,DP),setarg(A,DP,_),system:retractall(M:DP),system:asserta(M:P).

%% load_mpred_system( ?Usr) is semidet.
%
% Load Managed Predicate System.
%
load_mpred_system(_Sys):-!.
load_mpred_system(Sys):-
 assert_setting00(lmconf:tbox_for(user,Sys)),
 with_mutex(mpred_system_mutex,
   (Sys:ensure_loaded(logicmoo_utils), 
    forall(
      lmconf:mpred_is_impl_file(Match),     
     (( 
        forall(filematch(Sys:Match,File),
          call((w_tl(t_l:disable_px,
               baseKB:ensure_loaded(baseKB:File)))))))))).

current_smt(SM,M):-
 '$current_source_module'(SM),'$current_typein_module'(M).

%% enable_mpred_system is det.
%
% Ensure the "managed predicate" system and subsystems are available
%
enable_mpred_system:-
   current_smt(SM,M),
   get_abox_for(SM,Usr),
   get_tbox_for(M,Sys),
   enable_mpred_system(Usr,Sys).

%% enable_mpred_system(+Usr,+Sys) is det.
%
% Enable Managed Predicate System.
%
% Begin considering forward and meta programming rules into a Prolog module.
:- export(enable_mpred_system/2).
enable_mpred_system(Usr,Sys):- with_mutex(mpred_system_mutex,lmconf:enable_mpred_system0(Usr,Sys)).



%% disable_mpred_system(+Usr,+Sys) is det.
%
% Disable tasks that considering forward and meta programming rules into a Prolog module.
%
:- export(disable_mpred_system/2).
disable_mpred_system(Usr,_Sys):- with_mutex(mpred_system_mutex,lmconf:disable_mpred_system0(Usr)).



:- thread_local t_l:side_effect_ok/0.


%% enable_mpred_system0( ?Usr) is semidet.
%
% Hook To [lmconf:enable_mpred_system0/1] For Usr Logicmoo_base.
% Enable Managed Predicate System Primary Helper.
%
lmconf:enable_mpred_system0(Usr,_):- lmconf:mpred_system_status(Usr,enabled),!.
lmconf:enable_mpred_system0(Usr,_):- lmconf:mpred_system_status(Usr,loading),!.
lmconf:enable_mpred_system0(Usr,Sys):- assert_setting00(lmconf:mpred_system_status(Usr,loading)),
   current_smt(SM,M),
   assert_setting00(lmconf:abox_for(SM,Usr)),
   assert_setting00(lmconf:tbox_for(SM,Sys)),
   assert_setting00(lmconf:tbox_for(M,Sys)),
   load_mpred_system(Sys),
   must(current_predicate(_,Sys:mpred_expander(_,_,_,_,_,_))),
   asserta_if_new((Sys:term_expansion(I,P1,O,P2):- Sys:mpred_expander(term,Sys,I,P1,O,P2))),
   asserta_if_new((Sys:goal_expansion(I,P1,O,P2):- Sys:mpred_expander(goal,Sys,I,P1,O,P2))),
   asserta_if_new((Usr:term_expansion(I,P1,O,P2):- Sys:mpred_expander(term,Usr,I,P1,O,P2))),
   asserta_if_new((Usr:goal_expansion(I,P1,O,P2):- Sys:mpred_expander(goal,Usr,I,P1,O,P2))),
   assert_setting00(lmconf:mpred_system_status(Usr,enabled)),
   w_tl(t_l:side_effect_ok,doall(Usr:call_no_cuts(lmconf:module_local_init(Usr,Sys)))).


%= 	 	 

%% disable_mpred_system0( ?Usr) is semidet.
%
% Hook To [lmconf:disable_mpred_system0/1] For Usr Logicmoo_base.
% Disable Managed Predicate System Primary Helper.
%
lmconf:disable_mpred_system0(Usr):- lmconf:mpred_system_status(Usr,disabled),!.
lmconf:disable_mpred_system0(Usr):-    
   retractall(lmconf:mpred_system_status(Usr,_)),
   asserta(lmconf:mpred_system_status(Usr,disabled)),
   % one day unload_mpred_system(Usr),
   %retractall((user:term_expansion(I,P1,O,P2):- mpred_expander(term,user,I,P1,O,P2))),
   %retractall((system:goal_expansion(I,P1,O,P2):- mpred_expander(goal,system,I,P1,O,P2))),
   retractall((Usr:term_expansion(I,P1,O,P2):- mpred_expander(term,Usr,I,P1,O,P2))),
   retractall((Usr:goal_expansion(I,P1,O,P2):- mpred_expander(goal,Usr,I,P1,O,P2))),!.
   
:- module_transparent config_mpred_system/2.

% :- dmsg("Adding logicmoo/[snark|mpred_online] to autoload path",[]).
:- add_library_search_path('./logicmoo/snark/',[ '*.pl']).

% Config System
:- config_mpred_system.

% Load Utils
:- logicmoo_utils:ensure_loaded(logicmoo_utils:library(logicmoo_utils)).

% Load System
:- user_module_uses_base(_,M),get_tbox_for(M,T),dmsg(system_kb=T),load_mpred_system(baseKB).

%:- autoload([verbose(false)]).
%:- set_prolog_flag(autoload, false).

:- user_module_uses_base(SM,M),fix_ops_for(M),(SM==M->true;fix_ops_for(SM)).

import_2:- user_module_uses_base(SM,M),
   get_abox_for(SM,Usr),
   get_tbox_for(M,Sys),
   maybe_add_import_module(SM,Usr,start),
   maybe_add_import_module(M,Sys,start),
   maybe_add_import_module(M,baseKB,start),
   maybe_add_import_module(M,baseKB,start).

% Enable System
:- enable_mpred_system.


:- asserta_new((logicmoo_util_database:ainz(G):- !, call(mpred_ainz,G))).
:- asserta_new((logicmoo_util_database:ain(G):- !, call(mpred_ain,G))).
:- asserta_new((logicmoo_util_database:aina(G):- !, call(mpred_aina,G))).


:- must(add_library_search_path('./logicmoo/mpred_online/',[ '*.pl'])).
:- add_library_search_path('./logicmoo/',[ '*.pl']).
% :- add_library_search_path('./plarkc/',[ '*.pl']).
% :- add_library_search_path('./pttp/',[ 'dbase_i_mpred_*.pl']).

%:- system:maybe_add_import_module(baseKB,baseKB,start).
%:- autoload([verbose(false)]).

:- forall(current_op(X,Y,baseKB:Z),assert(was_op(baseKB,X,Y,Z))).
:- forall(current_op(X,Y,system:Z),assert(was_op(system,X,Y,Z))).

:- forall(was_op(system,X,Y,Z),op_safe(X,Y,baseKB:Z)).
:- forall(was_op(baseKB,X,Y,Z),op_safe(X,Y,baseKB:Z)).

% :- set_prolog_flag(access_level,system).

import_3:-
   system:maybe_add_import_module(baseKB,system,end),
   system:maybe_add_import_module(logicmoo_user,system,end),
   system:ignore(system:delete_import_module(baseKB,user)),
   system:maybe_add_import_module(user,baseKB,start).
% :-  system:ignore(system:delete_import_module(user,system)).


:- set_prolog_flag(access_level,system).

:- forall(was_op(baseKB,X,Y,Z),system:op(X,Y,baseKB:Z)).
:- forall(was_op(system,X,Y,Z),system:op(X,Y,baseKB:Z)).
:- forall(was_op(baseKB,X,Y,Z),system:op(X,Y,baseKB:Z)).

:- set_prolog_flag(access_level,user).

% :- autoload([verbose(false)]).

%:- asserta_if_new((baseKB:term_expansion(I,P1,O,P2):- mpred_expander(term,baseKB,I,P1,O,P2))).
%:- asserta_if_new((system:goal_expansion(I,P1,O,P2):- mpred_expander(goal,system,I,P1,O,P2))).


:-  time((baseKB:ensure_mpred_file_loaded(baseKB:library(logicmoo/pfc/'system_base.pfc')))).

:- do_gc.

:- forall(retract(wsh_w:wrap_shared(F,A,ereq)),ain((arity(F,A),pfcControlled(F),prologHybrid(F)))).

:- set_prolog_flag(access_level,user).

:- statistics.

