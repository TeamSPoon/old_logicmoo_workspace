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
:- module(common_logic_at_box,[

         assert_setting01/1,
         make_module_name_local/2,
         make_module_name_local0/2,
         
         get_user_module/1,
         set_user_module/1,
         
         defaultAssertMt/1,
         set_abox/1,
         get_abox_for/2,
         set_abox_for/2,

         
         which_file/1,
         get_file_abox/1,
         set_file_abox/1,
         
         
         get_current_tbox/1,
         set_tbox/1,
         get_tbox_for/2,
         set_tbox_for/2,
         
         
         best_module/3,
         correct_module/3,
         correct_module/4,
         ensure_imports/1,
         import_to_user/1,
         import_to_user0/3,
         in_mpred_kb_module/0,
         lmconf:is_box_module/2,
         is_default_shared/1,
         is_system_box/1,
         
         best_module/3,
         correct_module/3,
         correct_module/4,
         ensure_imports/1,
         import_to_user/1,
         import_to_user0/3,
         in_mpred_kb_module/0,
         lmconf:is_box_module/2,
         is_default_shared/1,
         is_system_box/1,
         which_file/1,
         user_m_check/1
    ]).

user_m_check(_Out).




:- multifile(lmcache:has_pfc_database_preds/1).
:- multifile(lmconf:tbox_for/2).
:- multifile(lmconf:abox_for/2).
:- dynamic(lmcache:has_pfc_database_preds/1).
:- dynamic(lmconf:tbox_for/2).
:- dynamic(lmconf:abox_for/2).


%% assert_setting01( ?X) is semidet.
assert_setting01(M:P):-functor(P,_,A),duplicate_term(P,DP),setarg(A,DP,_),system:retractall(M:DP),system:asserta(M:P).
            

%% which_file( ?F) is semidet.
%
% Which File.
%
which_file(F):- prolog_load_context(source,F) -> true; once(loading_source_file(F)).

:- module_transparent
   defaultAssertMt/1,
   set_abox/1,
   get_abox_for/2,
   set_abox_for/2,
   %reset_abox/1,

   which_file/1,
   get_file_abox/1,
   set_file_abox/1,

   
   get_current_tbox/1,
   set_tbox/1,
   get_tbox_for/2,
   set_tbox_for/2,
   %reset_tbox/1,
   
   best_module/3,
   correct_module/3,
   correct_module/4,
   ensure_imports/1,
   import_to_user/1,
   import_to_user0/3,
   in_mpred_kb_module/0,
   is_box_module/2,
   is_default_shared/1,
   is_system_box/1   .


%% in_mpred_kb_module is semidet.
%
% In Managed Predicate Knowledge Base Module.
%
in_mpred_kb_module:- source_context_module(MT),defaultAssertMt(MT2),!,MT==MT2.


make_module_name_local(A,B):-make_module_name_local0(A,B),B\==logicmoo_base,\+ exists_file(B).
make_module_name_local0(Source,baseKB):-is_default_shared(Source).
make_module_name_local0(Source,Source):-lmcache:has_pfc_database_preds(Source).
make_module_name_local0(Source,FM):-make_module_name(Source,FM).

:- thread_local(t_l:current_module_override/1).

ensure_tbox(_ABox).

simple_boxes.

%% defaultAssertMt(-Ctx) is det.
%
% ABox is an "assertion component" Prolog Module
% within a knowledge base.
%
% not just user modules

:- thread_local(t_l:user_abox/2).
:- thread_local(t_l:user_tbox/2).
:- dynamic(lmconf:abox_for/2).
:- dynamic(lmconf:tbox_for/2).

get_user_module(M):- t_l:current_module_override(M),!.
get_user_module(M):- '$current_source_module'(M),M\==user,!.
get_user_module(M):- '$current_typein_module'(M),M\==user,!.
% get_user_module(M):- source_module(M),M\==user.
get_user_module(M):- get_file_module(M).


get_file_module(baseKB):- simple_boxes,!.
get_file_module(M):- which_file(File)->make_module_name_local(File,M),File\==M,!.

user:exception(undefined_predicate, Mt:F/A ,retry):- t(tMicrotheory,Mt) -> registerCycPred(Mt,F,A).
   
makeConstant(_Mt).

% ============================================
% Make new Microtheory
% ============================================

ensureMt(Mt):-
   makeConstant(Mt),
   cycAssert(baseKB:isa(Mt,tMicrotheory)).

ensureGenlMt(Sub,Super):-ensureMt(Sub),ensureMt(Super),
   cycAssert(baseKB:'genlMt'(Sub,Super)).


% ============================================
:-dynamic(isRegisterCycPred/3).

:-module_transparent(isRegisterCycPred/3).

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
registerCycPred(Mt,Pred,0):-!,registerCycPred(Mt,Pred,2).
registerCycPred(Mt,Pred,Arity):-isRegisterCycPred(Mt,Pred,Arity),!.
registerCycPred(Mt,Pred,Arity):-
      functor(Term,Pred,Arity),
      ignore(defaultAssertMt(Mt)),
      % asserta(( user:Term :- common_logic_snark:kif_ask(Term,Mt))),
      ain(( Mt:Term :- istAbove(Mt,Term))),
      ain(isRegisterCycPred(Mt,Pred,Arity)),!.


% ============================================
% Assert Side Effect Prolog to Cyc Predicate Mapping
%
% ?- assert(isa('Fido','Dog')).
% Will assert (isa Fido Dog) into BaseKB
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

assertThrough(ToMt,CycL):-
      functor(CycL,Pred,Arity),
      (isRegisterCycPred(Mt,Pred,Arity);atom_concat('#$',_,Pred)),!,
      ignore(ToMt=Mt),cycAssert(CycL,ToMt),!.

assertThrough(ToMt,CycL):-
      (predicate_property(Mt:CycL,_);context_module(Mt);Mt=ToMt),!,
      ignore(Mt=ToMt),
      ain(Mt:CycL),!.

% ============================================
% Retract (All) Side Effect Prolog to Cyc Predicate Mapping
%
% ?- retractall(isa('Fido','Dog')).
% Will mpred_rem (isa Fido Dog) from BaseKB
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
      isRegisterCycPred(Mt,Pred,Arity),!,
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
% Will mpred_rem (isa Fido Dog) from BaseKB
%
% ?- retractall('DogsMt':isa('Fido','Dog')).
% Will mpred_rem (isa Fido Dog) from DogsMt
% ============================================
:-ifHookRedef((redefine_system_predicate(mpred_rem(_)),asserta((mpred_rem(Term):-nonvar(Term),retractOnceThrough(Term))))).

retractOnceThrough(Mt:CycL):-
      retractOnceThrough(Mt,CycL).

retractOnceThrough(CycL):-
      retractOnceThrough(_Mt,CycL).

retractOnceThrough(ToMt,CycL):-
      functor(CycL,Pred,Arity),
      isRegisterCycPred(Mt,Pred,Arity),!,
      ignore(ToMt=Mt),
      cycRetract(CycL,ToMt),!.

retractOnceThrough(ToMt,CycL):-
      (predicate_property(Mt:CycL,_);context_module(Mt);Mt=ToMt),!,
      ignore(Mt=ToMt),
      system:mpred_rem(Mt:CycL),!.


defaultAssertMt(ABox):- get_user_module(M),!,get_abox_for(M,ABox).
defaultAssertMt(baseKB):- simple_boxes,!.

get_current_tbox(TBox):- defaultAssertMt(M),!,get_tbox_for(M,TBox).
get_current_tbox(baseKB):- simple_boxes,!.

%% set_user_module( ?ABox) is semidet.
%
% Set Current Module.
%
set_user_module(M):- assert_setting(t_l:current_module_override(M)),
                        '$set_source_module'(M),'$set_typein_module'(M),
                        get_abox_for(M,ABox),get_tbox_for(ABox,TBox),setup_module_ops(ABox),
                        inherit_into_module(M,TBox,end).

inherit_into_module(Child,Parent):-  ensureGenlMt(Child,Parent),ignore(maybe_remove_import_module(Child,Parent)).
inherit_into_modules(Child):-forall(import_module(Child,Parent) ,inherit_into_module(Child,Parent)).


set_abox(ABox):- '$set_source_module'(ABox), get_user_module(M),set_abox_for(M,ABox),setup_module_ops(ABox).

set_tbox(TBox):- '$set_typein_module'(TBox), get_user_module(M),set_tbox_for(M,TBox),setup_module_ops(TBox).


get_abox_for(M,ABox):- t_l:user_abox(M,ABox),!.
get_abox_for(M,ABox):-lmconf:abox_for(M,ABox),!.
get_abox_for(M,M):- M\==user,current_module(M),!.
get_abox_for(_,baseKB):- simple_boxes,!.

set_abox_for(M,ABox):-assert_setting(lmconf:abox_for(M,ABox)).

get_tbox_for(M,TBox):- t_l:user_abox(M,TBox),!.
get_tbox_for(M,TBox):- lmconf:tbox_for(M,TBox),!.
%get_tbox_for(M,M):- M\==user,!,current_module(M),!.
get_tbox_for(_,baseKB):- simple_boxes,!.

set_tbox_for(M,TBox):-assert_setting(lmconf:tbox_for(M,TBox)).

%% box_type( ?F, ?A, ?VALUE3) is semidet.
%
% Datalog Type.
%
box_type(F,A,tbox):-current_predicate(baseKB:F/A).
box_type(_,_,abox).


%% set_tbox( ?M, ?TBoxM) is semidet.
%
% Set User Tbox.
%
set_tbox(TBoxM):- defaultAssertMt(M),ensure_abox(TBoxM),set_tbox(M,TBoxM).
set_tbox(M,TBoxM):- 
   ( is_system_box(M) -> true ; inherit_into_module(M,TBoxM,end)).

%% set_file_abox( ?M) is semidet.
%
% Set User ABox.
%

get_file_abox(ABox):- which_file(Source),get_abox_for(Source,ABox).
get_file_abox(ABox):- get_file_module(Source),get_abox_for(Source,ABox).

set_file_abox(ABox):- get_file_abox(Was),ABox==Was,!.
set_file_abox(ABox):- 
 must_det_l((
   get_current_tbox(TBox),
   TBox:ensure_abox(ABox),
   user_m_check(ABox), 
   '$current_typein_module'(CM),setup_module_ops(CM),
   '$current_source_module'(SM),setup_module_ops(SM),  
   which_file(Source),set_abox_for(Source,ABox),
   ((t_l:user_abox(SM,Prev),Prev\==ABox)->(assert_until_eof(t_l:user_abox(SM,ABox)))))),
   % onEndOfFile(reset_abox(Prev)));true),   
   onEndOfFile('$set_source_module'(SM)),
   onEndOfFile('$set_typein_module'(CM)),
   set_user_module(ABox),
   !.


%% is_box_module( ?M, ?VALUE2) is semidet.
%
% If Is A Datalog Module.
%
lmconf:is_box_module(M,tbox):- is_system_box(M).
lmconf:is_box_module(user,abox).


:- export(to_box_type0/3).

%% to_box_type( ?M, ?B, ?T) is semidet.
%
% Converted To Datalog Type.
%
to_box_type(M,B,T):-quietly_must(to_box_type0(M,B,TT)),!,T=TT.


%% to_box_type0( ?M, ?VALUE2, ?T) is semidet.
%
% Converted To Datalog Type Primary Helper.
%
to_box_type0(M,abox,T):-!,get_abox_for(M,T).
to_box_type0(M,tbox,T):-get_tbox_for(M,T).


%% is_system_box( ?VALUE1) is semidet.
%
% If Is A System Datalog.
%
is_system_box(baseKB).
is_system_box(logicmoo_user).


%% abox_for(M,Box).
%
% Not Boot Module.
%
%is_default_shared(mpred_loader).
is_default_shared(baseKB).
is_default_shared(boot_system).
is_default_shared(system_markers).
is_default_shared(system_singleValued).
is_default_shared(system_genls).
is_default_shared(system_if_missing).
is_default_shared(common_logic_clif).
is_default_shared(system_mdefault).
is_default_shared(user).

is_undefaulted(user).
is_undefaulted(logicmoo_user).
is_undefaulted(mpred_userkb).



%% best_module( ?List, ?ABox) is semidet.
%
% Best Module.
%
best_module0(List,M,ABox):-member(M,List),get_abox_for(M,ABox).
% best_module0(List,ABox,ABox):-member(M,List),lmcache:has_pfc_database_preds(ABox).

best_module(List,M,ABox):-best_module0(List,M,ABox), \+ is_undefaulted(M), \+ is_system_box(ABox),!.
best_module(List,M,ABox):-best_module0(List,M,ABox), \+ is_default_shared(M), \+ is_system_box(ABox),!.
best_module(List,M,ABox):-best_module0(List,M,ABox), \+ is_undefaulted(M),\+ is_undefaulted(ABox).
best_module(List,M,M):-member(M,List).
best_module(_List,baseKB,baseKB):-!.



%% import_shared_pred( ?M, ?BaseKB, ?P) is semidet.
%
% Import Shared Predicate.
%
import_shared_pred(baseKB,_,_):-!.
import_shared_pred(M,BaseKB,P):- 
  functor(P,F,A),
  %dynamic(BaseKB:F/A),
  user:catch(mpred_op_prolog(pain,((M:P:- user:BaseKB:P))),E,dmsg(import_shared_pred(M:F/A:-BaseKB:F/A)=E)),
  quietly_must(ignore(show_failure( \+ predicate_property(BaseKB:P,exported)))),
  import_to_user(M:P).


%% import_to_user( ?P) is semidet.
%
% Import Converted To User.
%
import_to_user(P):- '$current_typein_module'(MM),'$current_source_module'(SM),
   quietly_must(import_to_user0(MM,SM,P)).

import_to_user0(user,user,M:FA):- quietly_must(M\==user),!, call_from_module(M,import_to_user(M:FA)).
import_to_user0(M,SM, user:FA):- M\==SM,dmsg(warn(import_to_user0(M,SM, user:FA))),fail.
import_to_user0(MM,SM,M:F/A):- !,functor(P,F,A),import_to_user_mfa0(MM,SM,P,M:F/A).
import_to_user0(MM,SM,M:P):-!,functor(P,F,A),import_to_user_mfa0(MM,SM,P,M:F/A).
import_to_user0(MM,SM,P):- source_module(SSM),user_abox_or(SSM,M),import_to_user0(MM,SM,M:P),!.

user_abox_or(SSM,M):- t_l:user_abox(SSM,M),!.
user_abox_or(M,M).

import_to_user_mfa0(_MM,_SM,_P,_M:F/A):- current_predicate(system:F/A),!.
import_to_user_mfa0(_MM,_SM,P,_M:_F/_A):- predicate_property(P,static),!.
import_to_user_mfa0(_MM,_SM,P,_M:_F/_A):- predicate_property(P,exported),!.
import_to_user_mfa0(_MM,_SM,P,_M:_F/_A):- predicate_property(P,imported_from(_)),!.
import_to_user_mfa0(_MM,_SM,_P,M:F/A):- current_predicate(check_never_assert/1),check_never_assert(declared(M:F/A)),fail.
import_to_user_mfa0(_MM,_SM,decl_type(A),baseKB:decl_type/1):-trace_or_throw(basd_import_to_user_mfa0(baseKB:decl_type(A))).



import_to_user_mfa0(_MM,_SM,_,M:F/A):- functor(P,F,A), 
 U=everythingPSC,
 Rule = ((U:P:- user:loop_check_nr(M:P))),
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
                           inherit_into_module(M,system,end),
                           inherit_into_module(user,M,end),
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
% Skip User.
%
skip_user(M):-
  inherit_into_module(M,system,end),  
  ignore(maybe_delete_import_module(M,user)).
  %ignore(maybe_delete_import_module(user,system)).
  %asserta((M:import(P):-system:import(P))),
  



%% ensure_imports_tbox( ?M, ?BaseKB) is semidet.
%
% Ensure Imports Tbox.
%
ensure_imports_tbox(M,BaseKB):-M==BaseKB,!.
ensure_imports_tbox(M,BaseKB):-
  lmcache:is_ensured_imports_tbox(M,BaseKB),!.
ensure_imports_tbox(M,BaseKB):-
  asserta(lmcache:is_ensured_imports_tbox(M,BaseKB)),
  
  must_det((
   %inherit_into_module(BaseKB,mpred_loader,end),
   %inherit_into_module(M,mpred_loader,end),
   forall((system:current_module(IM), \+ lmconf:is_box_module(IM,_)),inherit_into_module(M,IM,end)),
   forall((system:current_module(IM),\+ lmconf:is_box_module(IM,_)),inherit_into_module(BaseKB,IM,end)),
   % skip_user(BaseKB),
   %ignore(maybe_delete_import_module(user,BaseKB)),
   %ignore(maybe_delete_import_module(BaseKB,user)),
   ignore(maybe_delete_import_module(M,BaseKB)),
   ignore(maybe_delete_import_module(BaseKB,M)),
   forall((prolog:current_predicate(_,BaseKB:P),\+predicate_property(BaseKB:P,imported_from(_))),import_shared_pred(M,BaseKB,P)),
   % inherit_into_module(user,BaseKB,end),
   % inherit_into_module(BaseKB,system,end),
   inherit_into_module(M,user,end),
   %inherit_into_module(BaseKB,M,end),
   %skip_user(M),
   ignore(maybe_delete_import_module(M,user)),
   inherit_into_module(user,M,end),
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
correct_module(abox,F,A,T):- !,defaultAssertMt(M),!,correct_module(M,F,A,T).
correct_module(tbox,F,A,T):- !,get_current_tbox(M),!,correct_module(M,F,A,T).
correct_module(sbox,F,A,T):- !,get_current_tbox(M),!,correct_module(M,F,A,T).
correct_module(M,F,A,T):- box_type(F,A,Type),!,to_box_type(M,Type,T).
correct_module(MT,_,_,MT):-!.

% :- inherit_into_module(logicmoo_user,logicmoo_base,start).


