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
:- if((current_prolog_flag(xref,true),current_prolog_flag(pldoc_x,true))).
:- module(common_logic_at_box,[

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
         
         user_m_check/1,
         which_file/1
    ]).
:- endif.

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


% mtGlobal
% mtCore



makeConstant(_Mt).


%:- (system:trace, rtrace, trace,cls ).
%:- (break,notrace,nortrace).


% ============================================
:-dynamic(baseKB:isRegisteredCycPred/3).

:-module_transparent(baseKB:isRegisteredCycPred/3).

% ?- registerCycPred(baseKB:isa/2). 
registerCycPred(Mt:Pred/Arity):-!,
   registerCycPred(Mt,Pred,Arity).
% ?- registerCycPred(baseKB:isa(_,_)). 
registerCycPred(Mt:Term):-
   functor(Term,Pred,Arity),
   registerCycPred(Mt,Pred,Arity).
registerCycPred(Term):-
   functor(Term,Pred,Arity),
   currentMt(Mt),
   registerCycPred(Mt,Pred,Arity).
   


% ?- registerCycPred(isa(_,_),baseKB). 
registerCycPred(Term,Mt):-
   functor(Term,Pred,Arity),
   registerCycPred(Mt,Pred,Arity).

% ?- registerCycPred(baseKB,isa,2). 
% registerCycPred(Mt,Pred,0):-!,registerCycPred(Mt,Pred,2).

registerCycPred(Mt,Pred,Arity):-
   baseKB:isRegisteredCycPred(Mt,Pred,Arity),!.

registerCycPred(Mt,Pred,Arity):-
   assert_if_new(baseKB:isRegisteredCycPred(Mt,Pred,Arity)),!,
   functor(Goal,Pred,Arity),
   registerCycPred(Mt,Goal,Pred,Arity),!.

guessMtFromGoal(HintMt,Goal,_OtherMt):-
  predicate_property(HintMt:Goal,exported).
guessMtFromGoal(HintMt,Goal,OtherMt):-
  predicate_property(HintMt:Goal,imported_from(OtherMt)).
guessMtFromGoal(_,Goal,OtherMt):-
  predicate_property(Goal,imported_from(OtherMt)).

guessMtFromGoal(_,Goal,OtherMt):- var(OtherMt),!,
  predicate_property(OtherMt:Goal,file(_)).

guessMtFromGoal(_,Goal,OtherMt):-
  mtGlobal(OtherMt),
  predicate_property(OtherMt:Goal,file(_)).


add_import_predicate(Mt,Goal,OtherMt):- fail,
   mtGlobal(Mt),
   mtGlobal(OtherMt),
   \+ import_module(OtherMt,Mt),
   catch(add_import_module(Mt,OtherMt,end),
       error(permission_error(add_import,module,baseKB),
       context(system:add_import_module/3,'would create a cycle')),fail),
   must(predicate_property(Mt:Goal,imported_from(OtherMt))),!.
add_import_predicate(Mt,Goal,OtherMt):- catch(Mt:import(OtherMt:Goal),_,fail),!.
add_import_predicate(Mt,Goal,OtherMt):- 
   functor(Goal,Pred,Arity),
   make_as_dynamic(imported_from(OtherMt),Mt,Pred,Arity),
   assert_if_new(( Mt:Goal :- OtherMt:Goal)).
  

dump_break:- prolog_stack:backtrace(8000),dtrace. % system:break.

make_as_dynamic(Reason,Mt,Pred,Arity):- dynamic( Mt:Pred/Arity),
   functor(Goal,Pred,Arity),
   assert_if_new(( Mt:Goal :- (fail,infoF(Reason)))).


registerCycPred(Mt,Goal,Pred,Arity):-
   guessMtFromGoal(Mt,Goal,OtherMt), 
   sanity(Mt \== OtherMt),
   must(Mt \== OtherMt),
   ain(baseKB:tMicrotheory(Mt)),
   registerCycPred(Mt,Goal,Pred,Arity,OtherMt),!.

registerCycPred(Mt,Goal,Pred,Arity):-
   dynamic(Mt:Pred/Arity), % (1 is random(180)->dump_break;true),
   assert_if_new(( Mt:Goal :- istAbove(Mt,Goal))).


transitive_path(Pred,[Arg1,Arg2],Arg2):-
  dif(Arg1,Arg2),call(Pred,Arg1,Arg2),!.
transitive_path(Pred,[Arg1,SecondNodeMt|REST],Arg2):-
  dif(Arg1,Arg2),dif(Arg1,SecondNodeMt),
  call(Pred,Arg1,SecondNodeMt),stack_check,
  transitive_path(Pred,[SecondNodeMt|REST],Arg2).

registerCycPred(Mt,Goal,_Pred,_Arity,OtherMt):- 
  mtGlobal(OtherMt),
  add_import_predicate(Mt,Goal,OtherMt),!.
  

registerCycPred(Mt,Goal,Pred,Arity,OtherMt):- 
   transitive_path(baseKB:genlMt,[Mt,SecondNodeMt|_],OtherMt),
   make_as_dynamic(genlMt(Mt,OtherMt),Mt,Pred,Arity),
   assert_if_new(( Mt:Goal :- SecondNodeMt:call(Goal))),!.

registerCycPred(Mt,_Goal,Pred,Arity,OtherMt):-
  dump_break,
  make_as_dynamic(need_genlMt(Mt,OtherMt),Mt,Pred,Arity),!.
   


clearKb(KB):- mtCore(KB),!.
clearKb(KB):- forall(import_module(KB,O),
                  ignore(delete_import_module(KB,O))),
     forall(( current_module(X),import_module(X,KB)),
                  ignore(delete_import_module(X,KB))).

autoload_library_index(F,A,M,File):- functor(P,F,A),'$autoload':library_index(P,M,File).

:-dynamic(baseKB:genlMt/2).
istAbove(Mt,Query):- Mt\==baseKB,Mt\==logicmoo_utils,genlMt(Mt,MtAbove),MtAbove:Query.

user:exception(A,B,C):-!,system_exception(A,B,C).
system:exception(A,B,C):-!,system_exception(A,B,C).

system_exception(undefined_predicate,M:debug/1,retry):- use_module(M:library(debug)),!.
system_exception(undefined_predicate,M:debugging/1,retry):- use_module(M:library(debug)),!.
system_exception(undefined_predicate,M:member/2,retry):- use_module(M:library(lists)),!.
system_exception(undefined_predicate,M:directory_file_path/3,retry):- use_module(M:library(filesex)),!.
system_exception(undefined_predicate,M:F/A,retry):- fail,
       autoload_library_index(F,A,_,File),
       load_files(M:File,[if(true),imports([F/A]),register(false),silent(false)]),!.
system_exception(undefined_predicate,M:F/A,retry):-
       autoload_library_index(F,A,_,File),
       ensure_loaded(M:File),!.
system_exception(undefined_predicate,M:F/A,retry):-
      autoload_library_index(F,A,NewMod,File),
      (current_module(NewMod) 
       -> add_import_module(M,NewMod,start) ;
       (NewMod:ensure_loaded(NewMod:File),add_import_module(M,NewMod,start))),!.

system_exception(undefined_predicate,M:'$pldoc'/4,retry):-multifile(M:'$pldoc'/4),dynamic(M:'$pldoc'/4),!.
system_exception(undefined_predicate,lmconf:F/A,retry):-multifile(lmconf:F/A),dynamic(lmconf:F/A),!.
system_exception(undefined_predicate,lmcache:F/A,retry):-multifile(lmcache:F/A),volatile(lmcache:F/A),!.

system_exception(undefined_predicate,M:must/1,retry) :- add_import_module(M,logicmoo_util_catch,start),!.
system_exception(undefined_predicate,M:debugm/2,retry) :- add_import_module(M,logicmoo_util_dmsg,start),!.
system_exception(undefined_predicate, Mt:F/A,retry):-
  current_prolog_flag(retry_undefined,true),
  set_prolog_flag(retry_undefined,false),
   F\=istAbove, % t(tMicrotheory,Mt) -> 
   loop_check_term(registerCycPred(Mt,F,A),
    registerCycPredsIntoMt(Mt,F),dump_break),
  set_prolog_flag(retry_undefined,true).


% ============================================
% Assert Side Effect Prolog to Cyc Predicate Mapping
%
% ?- assert(isa('Fido','Dog')).
% Will assert (isa Fido Dog) into TBox
%
% ?- assert('DogsMt':isa('Fido','Dog')).
% Will assert (isa Fido Dog) into DogsMt
% ============================================
%'$toplevel':assert(X):-ain(Term).

ifHookRedef(_):-!.
%ifHookRedef(C):-C,!.

:-ifHookRedef((redefine_system_predicate(system:assert(_)),assert((system:assert(Term):-nonvar(Term),assertThrough(Term))))).

assertThrough(Mt:CycL):-assertThrough(Mt,CycL).
assertThrough(CycL):-mtForCycL(CycL,Mt),assertThrough(Mt,CycL).

assertThrough(_,ToMt:CycL):-!,assertThrough(ToMt,CycL).
assertThrough(ToMt,CycL):-
      functor(CycL,Pred,Arity),
      (baseKB:isRegisteredCycPred(Mt,Pred,Arity);atom_concat('#$',_,Pred)),!,
      ignore(ToMt=Mt),ain(CycL,ToMt),!.

assertThrough(ToMt,CycL):-
      (predicate_property(Mt:CycL,_);context_module(Mt);Mt=ToMt),!,
      ignore(Mt=ToMt),
      ain(Mt:CycL),!.

% ============================================
% Retract (All) Side Effect Prolog to Cyc Predicate Mapping
%
% ?- retractall(isa('Fido','Dog')).
% Will mpred_rem (isa Fido Dog) from TBox
%
% ?- retractall('DogsMt':isa('Fido','Dog')).
% Will mpred_rem (isa Fido Dog) from DogsMt
% ============================================
:-ifHookRedef((redefine_system_predicate(retractall(_)),asserta((retractall(Term):-nonvar(Term),retractAllThrough(Term))))).

retractAllThrough(Mt:CycL):-
      retractAllThrough(Mt,CycL).

retractAllThrough(CycL):-
      retractAllThrough(_Mt,CycL).

retractAllThrough(ToMt,CycL):-
      functor(CycL,Pred,Arity),
      baseKB:isRegisteredCycPred(Mt,Pred,Arity),!,
      ignore(ToMt=Mt),
      cycRetract(CycL,ToMt),!.

retractAllThrough(ToMt,CycL):-
      (predicate_property(Mt:CycL,_);context_module(Mt);Mt=ToMt),!,
      ignore(Mt=ToMt),
      system:retractall(Mt:CycL),!.
            
% ============================================
% Retract (First) Side Effect Prolog to Cyc Predicate Mapping
%
% ?- retractall(isa('Fido','Dog')).
% Will mpred_rem (isa Fido Dog) from TBox
%
% ?- retractall('DogsMt':isa('Fido','Dog')).
% Will mpred_rem (isa Fido Dog) from DogsMt
% ============================================
% :-ifHookRedef((redefine_system_predicate(retract(_)),asserta((retract(Term):-nonvar(Term),retractOnceThrough(Term))))).

retractOnceThrough(Mt:CycL):-
      retractOnceThrough(Mt,CycL).

retractOnceThrough(CycL):-
      retractOnceThrough(_Mt,CycL).

retractOnceThrough(ToMt,CycL):-
      functor(CycL,Pred,Arity),
      baseKB:isRegisteredCycPred(Mt,Pred,Arity),!,
      ignore(ToMt=Mt),
      cycRetract(CycL,ToMt),!.

retractOnceThrough(ToMt,CycL):-
      (predicate_property(Mt:CycL,_);context_module(Mt);Mt=ToMt),!,
      ignore(Mt=ToMt),
      mpred_remove(Mt:CycL),!.



map_inheritence(Child):-forall(import_module(Child,Parent),inherit_into_module(Child,Parent)).


%% box_type( ?F, ?A, ?VALUE3) is semidet.
%
% Datalog Type.
%
box_type(F,A,tbox):-current_predicate(baseKB:F/A).
box_type(_,_,abox).




:- thread_local(t_l:current_defaultAssertMt/1).
:- dynamic(lmconf:file_to_module/2).
baseKB:defaultTBoxMt(baseKB).


%% defaultAssertMt(-Ctx) is det.
%
% ABox is an "assertion component" Prolog Module
% within a knowledge base.
%
% not just user modules
defaultAssertMt(ABox):- nonvar(ABox),defaultAssertMt(ABoxVar),!,must(ABox=@=ABoxVar).
defaultAssertMt(ABox):- 
    (t_l:current_defaultAssertMt(ABox);
    ((('$current_source_module'(ABox);
    '$current_typein_module'(ABox);
     defaultTBoxMt(ABox))),ABox\==user)),!.

defaultAssertMt(ABox):- fileAssertMt(ABox).

%% fileAssertMt(-ABox) is det.
%
% Gets ABox is an "assertion component" Prolog Module
% within a knowledge base.
%
% not just user modules
fileAssertMt(ABox):- nonvar(ABox),fileAssertMt(ABoxVar),!,ABox=@=ABoxVar.
fileAssertMt(ABox):- which_file(File)->current_module(ABox),module_property(ABox,file(File)),File\==ABox,!.
fileAssertMt(ABox):-
 (t_l:current_defaultAssertMt(ABox);
    ((('$current_source_module'(ABox))),ABox\==user)),!.
fileAssertMt(ABox):- which_file(File)->make_module_name_local(File,ABox),File\==ABox,!.
fileAssertMt(ABox):-
 (((('$current_typein_module'(ABox);
     defaultTBoxMt(ABox))),ABox\==user)),!.
fileAssertMt(baseKB).


%% set_defaultAssertMt( ?ABox) is semidet.
%
% Sets Current Module.
%
set_defaultAssertMt(ABox):- 
    baseKB:defaultTBoxMt(TBox),
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


end_of_file.



[]=system
[baseKB]=logicmoo_util_attvar_reader
[baseKB]=logicmoo_util_bugger
[baseKB,user]=logicmoo_util_catch
[baseKB,user]=logicmoo_util_ctx_frame
[baseKB,user]=logicmoo_util_database
[baseKB,user]=logicmoo_util_dmsg
[baseKB,user]=logicmoo_util_dra
[baseKB,user]=logicmoo_util_dumpst
[baseKB,user]=logicmoo_util_engines
[baseKB,user]=logicmoo_util_filestreams
[baseKB,user]=logicmoo_util_first
[baseKB,user]=logicmoo_util_rtrace
[baseKB,user]=logicmoo_util_varnames
[baseKB,user]=logicmoo_util_with_assertions
[logicmoo_util_filestreams,logicmoo_util_engines,logicmoo_util_dumpst,logicmoo_util_dra,
 logicmoo_util_database,logicmoo_util_ctx_frame,logicmoo_util_with_assertions,
 logicmoo_util_attvar_reader,logicmoo_util_bugger,logicmoo_util_rtrace,logicmoo_util_dmsg,logicmoo_util_varnames,logicmoo_util_catch,
 logicmoo_util_first,user]=logicmoo_utils
[system]= $apply
[system]= $attvar
[system]= $autoload
[system]= $bags
[system]= $dcg
[system]= $dicts
[system]= $dwim
[system]= $expand
[system]= $history
[system]= $messages
[system]= $pack
[system]= $parms
[system]= $predopts
[system]= $qlf
[system]= $rc
[system]= $syspreds
[system]= $toplevel
[system]= $var_info
[system]=aggregate
[system]=ansi_term
[system]=apply
[system]=archive
[system]=arithmetic
[system]=assoc
[system]=backward_compatibility
[system]=base32
[system]=base64
[system]=broadcast
[system]=charsio
[system]=check
[system]=codesio
[system]=crypto_hash
[system]=date
[system]=dcg_basics
[system]=dif
[system]=doc_access
[system]=doc_util
[system]=edinburgh
[system]=editor_buttons
[system]=error
[system]=files_ex
[system]=gensym
[system]=git
[system]=gui_tracer
[system]=help_index
[system]=html_head
[system]=html_quasi_quotations
[system]=html_write
[system]=http_client
[system]=http_dispatch
[system]=http_exception
[system]=http_header
[system]=http_hook
[system]=http_host
[system]=http_multipart_plugin
[system]=http_open
[system]=http_parameters
[system]=http_path
[system]=http_server_files
[system]=http_stream
[system]=httpd_wrapper
[system]=iostream
[system]=jquery
[system]=license
[system]=link_xpce
[system]=lists
[system]=make
[system]=memory_file
[system]=mime_pack
[system]=mimetype
[system]=occurs
[system]=ordsets
[system]=oset
[system]=pairs
[system]=pce
[system]=pce_autoload
[system]=pce_compatibility_layer
[system]=pce_dispatch
[system]=pce_error
[system]=pce_expand
[system]=pce_expansion
[system]=pce_global
[system]=pce_goal_expansion
[system]=pce_host
[system]=pce_keybinding
[system]=pce_messages
[system]=pce_meta
[system]=pce_nedit
[system]=pce_portray
[system]=pce_principal
[system]=pce_realise
[system]=pce_swi_hooks
[system]=pce_util
[system]=pldoc
[system]=pldoc_colours
[system]=pldoc_files
[system]=pldoc_html
[system]=pldoc_htmlsrc
[system]=pldoc_http
[system]=pldoc_index
[system]=pldoc_man
[system]=pldoc_modes
[system]=pldoc_pack
[system]=pldoc_process
[system]=pldoc_register
[system]=pldoc_search
[system]=pldoc_wiki
[system]=predicate_options
[system]=process
[system]=prolog_autoload
[system]=prolog_breakpoints
[system]=prolog_clause
[system]=prolog_codewalk
[system]=prolog_colour
[system]=prolog_debug
[system]=prolog_dialect
[system]=prolog_edit
[system]=prolog_history
[system]=prolog_listing
[system]=prolog_metainference
[system]=prolog_operator
[system]=prolog_pack
[system]=prolog_source
[system]=prolog_stack
[system]=prolog_statistics
[system]=prolog_system_predicate_options
[system]=prolog_xref
[system]=pure_input
[system]=quasi_quotations
[system]=quintus
[system]=random
[system]=rbtrees
[system]=read_util
[system]=record
[system]=settings
[system]=sgml
[system]=sgml_write
[system]=shell
[system]=shlib
[system]=socket
[system]=ssl
[system]=start_emacs
[system]=swi_option
[system]=swi_system_utilities
[system]=terms
[system]=thread_httpd
[system]=thread_pool
[system]=thread_util
[system]=time
[system]=toplevel_variables
[system]=ugraphs
[system]=uri
[system]=url
[system]=user
[system]=utf8
[system]=when
[system]=www_browser
[system]=xpath

[user]=baseKB
[user]=emacs_prolog_colours
[user]=happy1
[user]=http
[user]=listing
[user]=lmcache
[user]=lmconf
[user]=logicmoo_util_filesystem
[user]=logicmoo_util_help
[user]=logicmoo_util_term_listing
[user]=mfree
[user]=mime
[user]=predopts_analysis
[user]=prolog
[user]=rdf_db
[user]=sandbox
[user]=star
[user]=t_l
[user]=t_l_global
[user]=tlbugger
[user]=varname_cache
[user]=vn
happy1:  ?-
true.

 = class(user) ;
MP = exports([]) ;
MP = program_size(0) ;



