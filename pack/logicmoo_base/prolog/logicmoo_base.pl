/*   
  LogicMOO Base FOL/PFC Setup
% Dec 13, 2035
% Douglas Miles

*/
:- if((
   '$current_source_module'(SM),'$current_typein_module'(M),
   system:multifile(lmconf:source_typein_boxes/3),
   system:dynamic(lmconf:source_typein_boxes/3),

   UTILS = logicmoo_utils,
   TBOX = baseKB,

   ignore((
   SM\==user,

   ignore(catch(system:delete_import_module(TBOX,SM),_,true)),
   (system:default_module(TBOX,SM) -> true;
      catch(system:add_import_module(SM,TBOX,end),_,true)),

   ignore(catch(system:delete_import_module(UTILS,SM),_,true)),
   (system:default_module(UTILS,SM) -> true;
      catch(system:add_import_module(SM,UTILS,end),_,true)))),

   system:asserta(lmconf:source_typein_boxes(SM,M,SM:TBOX)))).

:- endif.
:- module(logicmoo_base_file,
 [
   config_mpred_system/0,
   enable_mpred_system/0,
   config_mpred_system/2,
   enable_mpred_system/2,
   show_kb_structure/0,
  disable_mpred_system/2,
  fix_ops_for/1]).

:- lmconf:source_typein_boxes(SM,_M,SM:_TBOX),'$set_source_module'(SM).

:- ensure_loaded('./logicmoo/util/logicmoo_util_filesystem').

:- dynamic(logicmoo_utils:'$exported_op'/3).

:- multifile(logicmoo_utils:'$exported_op'/3).
:- discontiguous(logicmoo_utils:'$exported_op'/3).
% ========================================
% lmconf:mpred_system_kb/1
% ========================================
%% mpred_system_kb( ?VALUE1) is semidet.
%
% Hook To [lmconf:mpred_system_kb/1] For Usr Logicmoo_base.
% Managed Predicate System Knowledge Base.
%

config_mpred_system:- 
  current_smt(SM,M),
  must(lmconf:source_typein_boxes(SM,M,Usr:Sys)),
  config_mpred_system(Usr,Sys).

config_mpred_system(Usr,Sys):-
  ignore(lmconf:source_typein_boxes(_SM,M,Usr:Sys)),
  must(load_mpred_system(SM)),
  must(baseKB:defaultTBoxMt(Sys)),
  fix_ops_for(SM),
  fix_ops_for(Sys),
  fix_ops_for(Usr),
  assert_setting00(lmconf:source_typein_boxes(SM,M,Usr:Sys)).


show_kb_structure:-
   lmconf:source_typein_boxes(SM,M,Usr:Sys),
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
:- multifile(lmconf:mpred_is_impl_file/2).
:- dynamic(lmconf:mpred_is_impl_file/2).

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

:- multifile(lmconf:mpred_is_impl_file/2).
:- dynamic(lmconf:mpred_is_impl_file/2).

%% lmconf:mpred_is_impl_file(?TBox, ?A) is semidet.
%
% Hook To [lmconf:mpred_is_impl_file/2] For Usr Logicmoo_base.
% Managed Predicate If Is A Implimentation File.
%
lmconf:mpred_is_impl_file(tbox,mpred/A):-nonvar(A).
lmconf:mpred_is_impl_file(tbox,library('logicmoo/snark/common_logic_at_box.pl')).
lmconf:mpred_is_impl_file(tbox,library('logicmoo/mpred/mpred_expansion.pl')).
lmconf:mpred_is_impl_file(tbox,library('logicmoo/mpred/mpred_kb_ops.pl')).
lmconf:mpred_is_impl_file(tbox,library('logicmoo/mpred/mpred_listing.pl')).
lmconf:mpred_is_impl_file(tbox,library('logicmoo/snark/common_logic_sexpr.pl')).
lmconf:mpred_is_impl_file(tbox,library('logicmoo/mpred/mpred_loader.pl')).
lmconf:mpred_is_impl_file(tbox,library('logicmoo/mpred/mpred_pfc.pl')).
lmconf:mpred_is_impl_file(tbox,library('logicmoo/mpred/mpred_prolog_file.pl')).
lmconf:mpred_is_impl_file(tbox,library('logicmoo/mpred/mpred_props.pl')).
lmconf:mpred_is_impl_file(tbox,library('logicmoo/mpred/mpred_storage.pl')).
lmconf:mpred_is_impl_file(tbox,library('logicmoo/mpred/mpred_stubs.pl')).
lmconf:mpred_is_impl_file(tbox,library('logicmoo/mpred/mpred_type_constraints.pl')).
lmconf:mpred_is_impl_file(tbox,library('logicmoo/mpred/mpred_type_isa.pl')).
lmconf:mpred_is_impl_file(tbox,library('logicmoo/mpred/mpred_type_naming.pl')).
lmconf:mpred_is_impl_file(tbox,library('logicmoo/mpred/mpred_type_wff.pl')).
lmconf:mpred_is_impl_file(tbox,library('logicmoo/mpred/mpred_hooks.pl')).
lmconf:mpred_is_impl_file(tbox,library('logicmoo/mpred/mpred_userkb.pl')).
lmconf:mpred_is_impl_file(tbox,library('logicmoo/mpred/mpred_type_args.pl')).
lmconf:mpred_is_impl_file(tbox,library('logicmoo/mpred/mpred_agenda.pl')).
lmconf:mpred_is_impl_file(tbox,library('logicmoo/snark/common_logic_boxlog.pl')).
lmconf:mpred_is_impl_file(tbox,library('logicmoo/snark/common_logic_compiler.pl')).
lmconf:mpred_is_impl_file(tbox,library('logicmoo/snark/common_logic_skolem.pl')).
lmconf:mpred_is_impl_file(tbox,library('logicmoo/snark/common_logic_kb_hooks.pl')).
lmconf:mpred_is_impl_file(tbox,library('logicmoo/snark/common_logic_snark.pl')).
% lmconf:mpred_is_impl_file(tbox,library('logicmoo/mpred/mpred_*.pl')).


% :- reexport(library('logicmoo/snark/common_logic_sexpr.pl')).


%prolog:get_source_ref(R):-logicmoo_utils:get_source_ref(R).
%prolog:mpred_ain(U,R):- logicmoo_utils:mpred_ain(U,R).

%% assert_setting00( ?X) is semidet.
assert_setting00(M:P):-functor(P,_,A),duplicate_term(P,DP),setarg(A,DP,_),system:retractall(M:DP),system:asserta(M:P).

%% load_mpred_system( ?Usr) is semidet.
%
% Load Managed Predicate System.
%
% load_mpred_system(_Sys):-!.
load_mpred_system(Sys):-
 must((current_smt(SM,M),
 lmconf:source_typein_boxes(SM,M,_Usr:Sys))),
 ignore(catch(system:delete_import_module(logicmoo_utils,Sys),_,true)),
 ignore(catch(system:add_import_module(Sys,logicmoo_utils,start),_,true)),
 with_mutex(mpred_system_mutex,
   (forall(
      lmconf:mpred_is_impl_file(tbox,Match),     
     (( 
        forall(filematch(Sys:Match,File),
          call((w_tl(t_l:disable_px,
               logicmoo_utils:ensure_loaded(logicmoo_utils:File)))))))))).

current_smt(SM,M):-
 '$current_source_module'(SM),'$current_typein_module'(M).

%% enable_mpred_system is det.
%
% Ensure the "managed predicate" system and subsystems are available
%
enable_mpred_system:-
   must((current_smt(SM,M),
   lmconf:source_typein_boxes(SM,M,Usr:Sys))),
   enable_mpred_system(Usr,Sys).

%% enable_mpred_system(+Usr,+Sys) is det.
%
% Enable Managed Predicate System.
%
% Begin considering forward and meta programming rules into a Prolog module.
:- export(enable_mpred_system/2).
enable_mpred_system(Usr,Sys):- with_mutex(mpred_system_mutex,enable_mpred_system0(Usr,Sys)).



%% disable_mpred_system(+Usr,+Sys) is det.
%
% Disable tasks that considering forward and meta programming rules into a Prolog module.
%
:- export(disable_mpred_system/2).
disable_mpred_system(Usr,_Sys):- with_mutex(mpred_system_mutex,lmconf:disable_mpred_system0(Usr)).



:- thread_local t_l:side_effect_ok/0.


%% enable_mpred_system0( ?Usr) is semidet.
%
% Hook To [enable_mpred_system0/1] For Usr Logicmoo_base.
% Enable Managed Predicate System Primary Helper.
%
enable_mpred_system0(Usr,_):- lmconf:mpred_system_status(Usr,enabled),!.
enable_mpred_system0(Usr,_):- lmconf:mpred_system_status(Usr,loading),!.
enable_mpred_system0(Usr,Sys):- 
   assert_setting00(lmconf:mpred_system_status(Usr,loading)),
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
:- must(config_mpred_system).

:- set_prolog_flag(gc,false).

%:- autoload([verbose(false)]).
%:- set_prolog_flag(autoload, false).

import_2:- lmconf:source_typein_boxes(SM,M,Usr:Sys),
   maybe_add_import_module(SM,Usr,start),
   maybe_add_import_module(M,Sys,start),
   maybe_add_import_module(M,logicmoo_utils,start),
   maybe_add_import_module(M,logicmoo_utils,start).

% Enable System
:- must(enable_mpred_system).



:- must(add_library_search_path('./logicmoo/mpred_online/',[ '*.pl'])).
:- add_library_search_path('./logicmoo/',[ '*.pl']).
% :- add_library_search_path('./plarkc/',[ '*.pl']).
% :- add_library_search_path('./pttp/',[ 'dbase_i_mpred_*.pl']).

%:- maybe_add_import_module(logicmoo_utils,logicmoo_utils,start).
%:- autoload([verbose(false)]).

:- forall(current_op(X,Y,user:Z),assert(was_op(user,X,Y,Z))).
:- forall(current_op(X,Y,system:Z),assert(was_op(system,X,Y,Z))).

:- forall(was_op(system,X,Y,Z),op_safe(X,Y,logicmoo_utils:Z)).
:- forall(was_op(user,X,Y,Z),op_safe(X,Y,logicmoo_utils:Z)).

:- set_prolog_flag(access_level,system).

:- forall(was_op(system,X,Y,Z),system:op(X,Y,user:Z)).
:- forall(was_op(user,X,Y,Z),system:op(X,Y,user:Z)).
:- forall(was_op(_,X,Y,Z),system:op(X,Y,logicmoo_utils:Z)).
:- forall(was_op(_,X,Y,Z),system:op(X,Y,baseKB:Z)).

:- set_prolog_flag(access_level,user).


:- asserta_new((logicmoo_util_database:ainz(G):- !, call(logicmoo_utils:mpred_ainz,G))).
:- asserta_new((logicmoo_util_database:ain(G):- !, call(logicmoo_utils:mpred_ain,G))).
:- asserta_new((logicmoo_util_database:aina(G):- !, call(logicmoo_utils:mpred_aina,G))).

:- do_gc.


:- set_prolog_flag(access_level,user).

:- add_import_module(logicmoo_utils,prolog_statistics,start).

:- statistics.

/*

:- autoload. % ([verbose(false)]).

:- doall((clause(wsh_w:wrap_shared(F,A,ereq),Body),
    retract(( wsh_w:wrap_shared(F,A,ereq):- Body )), 
      between(0,9,A),ain((arity(F,A),pfcControlled(F),prologHybrid(F))),fail)).
*/

:- doall((current_module(W),import_module(W,system),\+ import_module(W, user),W\==baseKB, add_import_module(logicmoo_utils,W,end))).

:-  time((logicmoo_utils:ensure_mpred_file_loaded(baseKB:library(logicmoo/pfc/'system_base.pfc')))).

:- lmconf:source_typein_boxes(SM,M,_),'$set_source_module'(SM),'$set_typein_module'(M),!.
