/* 
% ===================================================================
% File 'mpred_db_preds.pl'
% Purpose: Emulation of OpenCyc for SWI-Prolog
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'interface.pl' 1.0.0
% Revision:  $Revision: 1.9 $
% Revised At:   $Date: 2002/06/27 14:13:20 $
% ===================================================================
% File used as storage place for all predicates which change as
% the world is run.
%
%
% Dec 13, 2035
% Douglas Miles
*/
% :- if((\+ current_prolog_flag(common_logic_at_box_true,true),set_prolog_flag(common_logic_at_box_true,true))).
%:- if(((current_prolog_flag(xref,true),current_prolog_flag(pldoc_x,true));current_prolog_flag(autoload_logicmoo,true))).
:- module(common_logic_at_box,[
         mtSharedPrologCodeOnly/1,
         assert_setting01/1,
         make_module_name_local/2,
         make_module_name_local0/2,
         
         defaultAssertMt/1,
         set_defaultAssertMt/1,
         mtCanAssertMt/1,

         which_file/1,
         fileAssertMt/1,
         set_fileAssertMt/1,
         defaultTBoxMt/1,
         
         correct_module/3,
         correct_module/4,
         ensure_imports/1,
         import_to_user/1,
         import_to_user0/3,
         in_mpred_kb_module/0,
         
         user_m_check/1,
         which_file/1
    ]).
%:- endif.

user_m_check(_Out).


:- meta_predicate transitive_path(2,*,*).


:- multifile(lmcache:has_pfc_database_preds/1).
:- dynamic(lmcache:has_pfc_database_preds/1).


%% assert_setting01( ?X) is semidet.
assert_setting01(M:P):-functor(P,_,A),duplicate_term(P,DP),setarg(A,DP,_),system:retractall(M:DP),system:asserta(M:P).
            

%% which_file( ?F) is semidet.
%
% Which File.
%
which_file(F):- prolog_load_context(source,F) -> true; once(loading_source_file(F)).

:- module_transparent
 
         assert_setting01/1,
         make_module_name_local/2,
         make_module_name_local0/2,
         
         defaultAssertMt/1,
         set_defaultAssertMt/1,
         
         which_file/1,
         fileAssertMt/1,
         set_fileAssertMt/1,
                          
         correct_module/3,
         correct_module/4,
         ensure_imports/1,
         import_to_user/1,
         import_to_user0/3,
         in_mpred_kb_module/0,
         which_file/1,
         user_m_check/1 .


%% in_mpred_kb_module is semidet.
%
% In Managed Predicate Knowledge Base Module.
%
in_mpred_kb_module:- source_context_module(MT),defaultAssertMt(MT2),!,MT==MT2.


map_inheritance(Child):-forall(import_module(Child,Parent),inherit_into_module(Child,Parent)).


%% box_type( ?F, ?A, ?VALUE3) is semidet.
%
% Datalog Type.
%
box_type(F,A,tbox):-current_predicate(baseKB:F/A).
box_type(_,_,abox).




:- thread_local(t_l:current_defaultAssertMt/1).
:- dynamic(lmconf:file_to_module/2).

:- multifile(defaultTBoxMt/1).
:- dynamic(defaultTBoxMt/1).
defaultTBoxMt(baseKB).


%% defaultAssertMt(-Ctx) is det.
%
% ABox is an "assertion component" Prolog Module
% within a knowledge base.
%
% not just user modules
defaultAssertMt(ABox):- nonvar(ABox),defaultAssertMt(ABoxVar),!,show_failure(ABox=@=ABoxVar).
defaultAssertMt(ABox):- 
    (t_l:current_defaultAssertMt(ABox);
    ((('$current_source_module'(ABox);
    '$current_typein_module'(ABox);
     defaultTBoxMt(ABox))), 
           mtCanAssertMt(ABox))),!.

defaultAssertMt(ABox):- fileAssertMt(ABox).

%% fileAssertMt(-ABox) is det.
%
% Gets ABox is an "assertion component" Prolog Module
% within a knowledge base.
%
% not just user modules
fileAssertMt(ABox):- nonvar(ABox), fileAssertMt(ABoxVar),!,ABox=@=ABoxVar.
fileAssertMt(ABox):- which_file(File)->current_module(ABox),module_property(ABox,file(File)),File\==ABox,!.
fileAssertMt(ABox):-
 (t_l:current_defaultAssertMt(ABox);
    ((('$current_source_module'(ABox))),ABox\==user)),!.
fileAssertMt(ABox):- which_file(File)->make_module_name_local(File,ABox),File\==ABox,!.
fileAssertMt(ABox):-
 (((('$current_typein_module'(ABox);
     defaultTBoxMt(ABox))),mtCanAssertMt(ABox))),!.
fileAssertMt(baseKB).

mtCanAssertMt(ABox):- \+ mtSharedPrologCodeOnly(ABox).

mtSharedPrologCodeOnly(Mt):-
      arg(_,v( 
         common_logic_at_box,
         common_logic_boxlog,
         common_logic_compiler,
         common_logic_kb_hooks,
         common_logic_sexpr,
         common_logic_skolem,
         common_logic_snark,
         lmconf,
         lmcache,
         t_l,
         logicmoo_base,
         logicmoo_base_file,
         logicmoo_util_attvar_reader,
         logicmoo_util_bugger,
         logicmoo_util_catch,
         logicmoo_util_ctx_frame,
         logicmoo_util_database,
         logicmoo_util_dmsg,
         logicmoo_util_dra,
         logicmoo_util_dumpst,
         logicmoo_util_engines,
         logicmoo_util_filestreams,
         logicmoo_util_first,
         logicmoo_util_help,
         logicmoo_util_loop_check,
         logicmoo_util_no_repeats,
         logicmoo_util_preddefs,
         logicmoo_util_prolog_frames,
         logicmoo_util_prolog_streams,
         logicmoo_util_rtrace,
         logicmoo_util_shared_dynamic,
         logicmoo_util_strings,
         logicmoo_util_term_listing,
         logicmoo_util_terms,
         logicmoo_util_varnames,
         logicmoo_util_with_assertions,
         logicmoo_utils_file,
         logicmoo_utils,
         mpred_agenda,
         mpred_expansion,
         mpred_hooks,
         mpred_kb_ops,
         mpred_listing,
         mpred_loader,
         mpred_pfc,
         mpred_props,
         mpred_storage,
         mpred_stubs,
         mpred_stubs_file_module,
         mpred_type_args,
         mpred_type_constraints,
         mpred_type_isa,
         mpred_type_naming,
         mpred_type_wff,
         prolog_statistics,
         system,
         user
       ),Mt).

mtNoPrologCode(mpred_userkb).



%% set_defaultAssertMt( ?ABox) is semidet.
%
% Sets Current Module.
%
set_defaultAssertMt(ABox):- 
    must(mtCanAssertMt(ABox)),
    defaultTBoxMt(TBox),
    (TBox==ABox->true;assert_setting(t_l:current_defaultAssertMt(ABox))),
    '$set_source_module'(ABox),'$set_typein_module'(ABox),                        
    setup_module_ops(ABox), 
    inherit_into_module(ABox,TBox).

%% set_fileAssertMt( ABox) is semidet.
%
% Sets the File''s Module.
%
set_fileAssertMt(ABox):- 
 must_det_l((
   must(mtCanAssertMt(ABox)),
   defaultTBoxMt(TBox),
   TBox:ensure_abox(ABox),
   '$current_typein_module'(CM),
   '$current_source_module'(SM),
   defaultAssertMt(Was),

   set_defaultAssertMt(ABox),
   which_file(File),assert_setting(lmconf:file_to_module(File,ABox)),
   assert_until_eof(lmcache:mpred_directive_value(File,module,ABox)),

   onEndOfFile(set_defaultAssertMt(Was)),
   onEndOfFile('$set_source_module'(SM)),
   onEndOfFile('$set_typein_module'(CM)))).



make_module_name_local(A,B):- make_module_name_local0(A,B), \+ exists_file(B).

make_module_name_local0(Source,KB):- mtGlobal(Source),defaultAssertMt(KB).
make_module_name_local0(Source,SetName):- lmconf:file_to_module(Source,SetName),!.
make_module_name_local0(Source,Source):- lmcache:has_pfc_database_preds(Source).
make_module_name_local0(Source,GetName):- make_module_name(Source,GetName).


ensure_tbox(_ABox).


%% mtCore( ?VALUE1) is semidet.
%
% If Is A System Datalog.
%
:- dynamic(mtCore/1).
mtCore(baseKB).


%% mtGlobal(M,Box).
%
% Boot Modules.
%
%mtGlobal(mpred_loader).

:- dynamic(mtGlobal/1).
mtGlobal(baseKB).
mtGlobal(boot_system).
mtGlobal(system_markers).
mtGlobal(system_singleValued).
mtGlobal(system_genls).
mtGlobal(system_if_missing).
mtGlobal(common_logic_clif).
mtGlobal(system_mdefault).


is_undefaulted(user).





%% import_shared_pred( ?M, ?TBox, ?P) is semidet.
%
% Import Shared Predicate.
%
import_shared_pred(_From,_To,_PI):-!.
import_shared_pred(baseKB,_,_):-!.
import_shared_pred(M,TBox,P):- 
  functor(P,F,A),
  %dynamic(TBox:F/A),
  user:catch(mpred_op_prolog(pain,((M:P:- user:TBox:P))),E,dmsg(import_shared_pred(M:F/A:-TBox:F/A)=E)),
  quietly_must(ignore(show_failure( \+ predicate_property(TBox:P,exported)))),
  import_to_user(M:P).


%% import_to_user( ?P) is semidet.
%
% Import Converted To User.
%

import_to_user(_):-!.
import_to_user(P):- '$current_typein_module'(MM),'$current_source_module'(SM),
   quietly_must(import_to_user0(MM,SM,P)).

import_to_user0(user,user,M:FA):- quietly_must(M\==user),!, call_from_module(M,import_to_user(M:FA)).
import_to_user0(M,SM, user:FA):- M\==SM,dmsg(warn(import_to_user0(M,SM, user:FA))),fail.
import_to_user0(MM,SM,M:F/A):- !,functor(P,F,A),import_to_user_mfa0(MM,SM,P,M:F/A).
import_to_user0(MM,SM,M:P):-!,functor(P,F,A),import_to_user_mfa0(MM,SM,P,M:F/A).
import_to_user0(MM,SM,P):- source_module(SSM),user_abox_or(SSM,M),import_to_user0(MM,SM,M:P),!.


import_to_user_mfa0(_MM,_SM,_P,_M:F/A):- current_predicate(system:F/A),!.
import_to_user_mfa0(_MM,_SM,P,_M:_F/_A):- predicate_property(P,static),!.
import_to_user_mfa0(_MM,_SM,P,_M:_F/_A):- predicate_property(P,exported),!.
import_to_user_mfa0(_MM,_SM,P,_M:_F/_A):- predicate_property(P,imported_from(_)),!.
import_to_user_mfa0(_MM,_SM,_P,M:F/A):- current_predicate(check_never_assert/1),check_never_assert(declared(M:F/A)),fail.
import_to_user_mfa0(_MM,_SM,decl_type(A),baseKB:decl_type/1):-trace_or_throw(basd_import_to_user_mfa0(baseKB:decl_type(A))).



import_to_user_mfa0(_MM,_SM,_,M:F/A):- functor(P,F,A), 
 U=everythingPSC,
 Rule = ((U:P:- U:loop_check_nr(M:P))),
 (clause_asserted(Rule)-> true; 
  must((
   user:catch(mpred_op_prolog(pain,Rule),E,dmsg(import_shared_pred(U:F/A:-M:F/A)=E)),
   user:export(U:F/A),
   catch(user:import(U:F/A),_,true)))).



%% import_module_to_user( ?M) is semidet.
%
% Import Module Converted To User.
%
system:import_module_to_user(M):- (default_module(user,M);import_module(user,M)),!.
system:import_module_to_user(M):- ignore(maybe_delete_import_module(M,user)),
      inherit_into_module(M,system),
      inherit_into_module(user,M),
      % find system thru M
      maybe_delete_import_module(user,system).




%% ensure_imports( ?M) is semidet.
%
% Ensure Imports.
%
ensure_imports(baseKB):-!.
ensure_imports(M):-ensure_imports_tbox(M,baseKB).

:-multifile(lmcache:is_ensured_imports_tbox/2).
:-dynamic(lmcache:is_ensured_imports_tbox/2).


%% skip_user( ?M) is semidet.
%
% Skip over 'user' module and still see 'system'.
%
skip_user(Mt):- import_module(Mt,system), \+ default_module(Mt,user), !.
skip_user(Mt):- !, add_import_module(Mt,system,start),ignore(delete_import_module(Mt,user)),
  forall((import_module(Mt,X),default_module(X,user)),skip_user(X)).
  
inherit_into_module(Child,Parent):- ==(Child,Parent),!.
inherit_into_module(Child,Parent):-ain(baseKB:genlMt(Child,Parent)).

%% ensure_imports_tbox( ?M, ?TBox) is semidet.
%
% Ensure Imports Tbox.
%
ensure_imports_tbox(M,TBox):-M==TBox,!.
ensure_imports_tbox(M,TBox):-
  lmcache:is_ensured_imports_tbox(M,TBox),!.
ensure_imports_tbox(M,TBox):-
  asserta(lmcache:is_ensured_imports_tbox(M,TBox)),
  
  must_det((
   %forall((system:current_module(IM), \+ lmconf:is_box_module(IM,_)),inherit_into_module(M,IM)),
   %forall((system:current_module(IM), \+ lmconf:is_box_module(IM,_)),inherit_into_module(TBox,IM)),
   skip_user(TBox),
   ignore(maybe_delete_import_module(M,TBox)),
   ignore(maybe_delete_import_module(TBox,M)),
   forall((user:current_predicate(_,TBox:P),
      \+ predicate_property(TBox:P,imported_from(_))),
      import_shared_pred(M,TBox,P)),
   inherit_into_module(M,user),
   skip_user(M),
   ignore(maybe_delete_import_module(M,user)),
   inherit_into_module(user,M),
   ignore(maybe_delete_import_module(user,system)), % gets from M now
   !)).



%% correct_module( ?M, ?X, ?T) is semidet.
%
% Correct Module.
%
correct_module(M,X,T):-functor(X,F,A),quietly_must(correct_module(M,F,A,T)),!.

%% correct_module( ?M, ?F, ?A, ?T) is semidet.
%
% Correct Module.
%
correct_module(abox,F,A,T):- !,defaultAssertMt(M),correct_module(M,F,A,T).
correct_module(tbox,F,A,T):- !,defaultTBoxMt(M),correct_module(M,F,A,T).
correct_module(user,F,A,T):- fail,!,defaultAssertMt(M),correct_module(M,F,A,T).
correct_module(M,F,A,T):- functor(G,F,A),guessMtFromGoal(M,G,T),!.
correct_module(MT,_,_,MT):-!.


% :- inherit_into_module(logicmoo_user,logicmoo_base).

fixup_module(system,_).
fixup_module(M,_L):-mtGlobal(M),skip_user(M).
fixup_module(logicmoo_utils,_L):-skip_user(logicmoo_utils).
fixup_module(_,[user]).
fixup_module(M,_L):- skip_user(M).


fixup_modules:- 
   doall((current_module(M),once((findall(I,import_module(M,I),L))),once(fixup_module(M,L)))).

:- set_prolog_flag(retry_undefined,false).
% :- autoload([verbose(false)]).
:- set_prolog_flag(retry_undefined,true).

:- fixup_modules.


% :- endif.

