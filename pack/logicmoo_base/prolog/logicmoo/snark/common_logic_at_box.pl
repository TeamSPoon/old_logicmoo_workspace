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
   
          set_current_module/1,
          assert_setting01/1,
make_module_name_local/2,
make_module_name_local0/2,
          get_abox/1,
          get_abox0/1,
          get_abox_for/2,
          guess_abox/2,
          set_abox/1,
          set_abox_for/2,
          set_file_abox/1,
          set_file_abox/1,
          set_guessed_abox/2,
          to_abox/2,
          reset_abox/1,

          get_tbox/1,
          get_tbox0/1,
          get_tbox_for/2,          
          guess_tbox/2,
          set_tbox/1,
          set_tbox_for/2,
          set_file_tbox/1,
          set_file_tbox/1,
          set_guessed_tbox/2,
          to_tbox/2,
          reset_tbox/1,

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
          maybe_add_import_module/3,
          maybe_delete_import_module/2,
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
            which_file/1,
   get_abox/1,
   get_abox0/1,
   get_abox_for/2,
   guess_abox/2,
   set_abox/1,
   set_abox_for/2,
   set_file_abox/1,
   set_guessed_abox/2,
   to_abox/2,
   reset_abox/1,
   
   get_tbox/1,
   get_tbox0/1,
   get_tbox_for/2,
   guess_tbox/2,
   set_tbox/1,
   set_tbox_for/2,
   set_file_tbox/1,
   set_guessed_tbox/2,
   to_tbox/2,
   reset_tbox/1,
   
   best_module/3,
   correct_module/3,
   correct_module/4,
   ensure_imports/1,
   import_to_user/1,
   import_to_user0/3,
   in_mpred_kb_module/0,
   is_box_module/2,
   is_default_shared/1,
   is_system_box/1,
   maybe_add_import_module/3,
   maybe_delete_import_module/2.



%% in_mpred_kb_module is semidet.
%
% In Managed Predicate Knowledge Base Module.
%
in_mpred_kb_module:- source_context_module(MT),get_abox(MT2),!,MT==MT2.



%% to_tbox( ?Box, -TBox) is det.
%
% Converted To Tbox.
%
to_tbox(ABox,T):-sanity((nonvar(ABox),var(T))),is_system_box(ABox),!,T=baseKB.
to_tbox(A,T):-get_abox(TT),A==TT,get_tbox(T),!.
to_tbox(A,A).


%% to_abox( ?Box, -ABox) is det.
%
% Converted To ABox.
%
to_abox(A,T):-sanity((nonvar(A),var(T))),is_system_box(A),!,get_abox(T).
to_abox(A,T):-get_tbox(TT),A==TT,get_abox(T),!.
to_abox(A,A).

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
to_box_type0(M,abox,T):-!,to_abox(M,T).
to_box_type0(M,tbox,T):-to_tbox(M,T).


%% set_file_abox( ?M) is semidet.
%
% Set User ABox.
%

get_file_abox(ABox):- which_file(Source),get_abox_for(Source,ABox).

set_file_abox(ABox):- get_file_abox(Was),ABox==Was,!.
set_file_abox(ABox):- 
 must_det_l((
   get_tbox(TBox),
   TBox:ensure_abox(ABox),
   user_m_check(ABox), 
   '$current_typein_module'(CM),mpred_ops(CM),
   '$current_source_module'(SM),mpred_ops(SM),  
   which_file(Source),set_abox_for(Source,ABox),
   ((t_l:user_abox(SM,Prev),Prev\==ABox)->(assert_until_eof(t_l:user_abox(SM,ABox)),onEndOfFile(reset_abox(Prev)));true),   
   onEndOfFile('$set_source_module'(SM)),
   onEndOfFile('$set_typein_module'(CM)),
   set_current_module(ABox))),
   !. 

%% set_file_tbox( ?M) is semidet.
%
% Set User ABox.
%
set_file_tbox(TBox):- get_file_tbox(Was),TBox==Was,!.
set_file_tbox(TBox):- 
 must_det_l((
   get_file_abox(ABox),
   set_tbox_for(ABox,TBox))).

get_file_tbox(TBox):- 
 must_det_l((
   get_file_abox(ABox),
   get_tbox_for(ABox,TBox))).


make_module_name_local(A,B):-make_module_name_local0(A,B),B\==logicmoo_base,\+ exists_file(B).

make_module_name_local0(Source,baseKB):-is_default_shared(Source).
make_module_name_local0(Source,Source):-lmcache:has_pfc_database_preds(Source).
make_module_name_local0(Source,FM):-make_module_name(Source,FM).

source_module_non_user(From):- source_module(From). 
source_module_non_user(From):- prolog_load_context(source,Source),make_module_name_local(Source,From).
source_module_non_user(From):- which_file(File),make_module_name_local(File,From).
source_module_non_user(From):- '$current_source_module'(From).
source_module_non_user(From):- '$current_typein_module'(From).

guess_abox(From,ABox):- nonvar(From),t_l:user_abox(From,ABox),!.
guess_abox(From,ABox):- source_module_non_user(From),From\==user,t_l:user_abox(From,ABox),!.
guess_abox(From,ABox):- source_module_non_user(From),From\==user,get_abox_for(From,ABox),set_abox_for(From,ABox),!.
guess_abox(From,ABox):- get_abox_for(From,ABox),set_abox_for(From,ABox),!.

guess_tbox(From,TBox):- guess_abox(From,ABox),get_tbox_for(ABox,TBox),!.
guess_tbox(From,ABox):-  
 hide_trace((
  (prolog_load_context(module,Source)->true;prolog_load_context(source,Source)),
  make_module_name_local(Source,FM),
  Source\==user -> (ABox = FM, which_file(File),set_tbox_for(File,ABox),set_tbox_for(Source,ABox),From=ABox,set_file_tbox(ABox),set_tbox(ABox));
  (('$set_source_module'(SM,SM),
   '$current_typein_module'(CM),
   best_module([FM,SM,CM],From,ABox),
   nop(dmsg(best_module([FM,SM,CM],From,ABox))))))),!.

:- thread_local(t_l:user_abox/2).
set_abox(User):-User==user,!,set_abox(baseKB).
set_abox(ABox):-
must_det_l((
 quietly_must(ensure_abox(ABox)),
 quietly_must((which_file(File),set_abox_for(File,ABox))),
 '$current_source_module'(SM),set_abox_for(SM,ABox),
 '$current_typein_module'(CM),set_abox_for(CM,ABox),
 ((t_l:user_abox(SM,Was),ABox\==Was)-> wdmsg(set_abox(Was->ABox));true),
 retractall(t_l:user_abox(SM,_M)),
 asserta(t_l:user_abox(SM,ABox)))).

:- thread_local(t_l:user_abox/2).
set_guessed_abox(ABox,_From):- \+ atom(ABox),!.
set_guessed_abox(_From,_ABox):-!.
set_guessed_abox(From,ABox):-
 must_det_l((
 sanity(atom(ABox)), 
  ((t_l:user_abox(SM,Was),ABox\==Was)-> wdmsg(set_abox(Was-->ABox));true),
 (which_file(File)->(\+ lmconf:abox_for(File,_)->set_abox_for(File,ABox);true);true),
 quietly_must(ensure_abox(ABox)), (\+ is_undefaulted(SM) -> set_abox_for(From,ABox) ; true))).
 %'$set_source_module'(SM,SM),(is_undefaulted(SM)->true;(set_abox_for(SM,ABox),Is_Changed=1)),
 %'$module'(CM,CM),(is_undefaulted(CM)->true;(set_abox_for(CM,ABox),Is_Changed=1)),
 %!.

set_guessed_tbox(_From,TBox):- \+ atom(TBox),!.
set_guessed_tbox(From,TBox):-
 must_det_l((
 sanity(atom(TBox)), 
  ((t_l:user_tbox(SM,Was),TBox\==Was)-> wdmsg(set_tbox(Was-->TBox));true),
 (which_file(File)->(\+ lmconf:tbox_for(File,_)->set_tbox_for(File,TBox);true);true),
 quietly_must(doall(ensure_tbox(TBox))), (\+ is_undefaulted(SM) -> set_tbox_for(From,TBox) ; true))).
 %'$set_source_module'(SM,SM),(is_undefaulted(SM)->true;(set_tbox_for(SM,TBox),Is_Changed=1)),
 %'$module'(CM,CM),(is_undefaulted(CM)->true;(set_tbox_for(CM,TBox),Is_Changed=1)),
 %!.


ensure_tbox(_ABox).

%% get_abox(-Ctx) is det.
%
% ABox is an "assertion component" Prolog Module
% within a knowledge base.
%
% not just user modules

get_abox(A):- nonvar(A),!.
get_abox(A):- get_abox0(A),A\==user,!.
get_abox(logicmoo_user).

get_abox0(A):- which_file(File)->get_abox_for(File,A).
get_abox0(A):-'$current_source_module'(M)->get_abox_for(M,A).
get_abox0(A):- source_module(SM)->get_abox_for(SM,A).
get_abox0(A):-'$current_typein_module'(M)->get_abox_for(M,A).
get_abox0(A):- guess_abox(From,A),set_guessed_abox(From,A).


get_tbox(A):- nonvar(A),!.
get_tbox(A):- quietly_must(quietly(((get_tbox0(A),A\=user);get_tbox0(A)))),!.

get_tbox0(A):-'$current_typein_module'(M),get_tbox_for(M,A).
get_tbox0(A):-'$current_source_module'(M),get_tbox_for(M,A).
get_tbox0(A):- source_module(SM)->get_tbox_for(SM,A).
get_tbox0(A):- which_file(File)->get_tbox_for(File,A).
get_tbox0(A):- guess_tbox(From,A),set_guessed_tbox(From,A).


set_abox_for(SM,ABox):-assert_setting(lmconf:abox_for(SM,ABox)).

get_abox_for(SM,ABox):-lmconf:abox_for(SM,ABox),!.
get_abox_for(SM,ABox):- SM\== user, SM\==baseKB, make_module_name_local(SM,ABox),
  current_module(ABox),\+ exists_file(ABox),
  assert_setting01(lmconf:abox_for(SM,ABox)),!.
get_abox_for(_,logicmoo_user).

:- thread_local(t_l:user_abox/2).
:- thread_local(t_l:user_tbox/2).

set_tbox_for(SM,TBox):-assert_setting(lmconf:tbox_for(SM,TBox)).

get_tbox_for(SM,TBox):- lmconf:tbox_for(SM,TBox),!.
get_tbox_for(M,TBox):- M \== user, M \==logicmoo_user, 
   make_module_name_local(M,TBox),current_module(TBox),
   M\=TBox,
    \+ lmconf:tbox_for(TBox,_),
   assert_setting01(lmconf:tbox_for(M,TBox)),!.

get_tbox_for(_,baseKB).



%% set_current_module( ?ABox) is semidet.
%
% Set Current Module.
%
set_current_module(user):- get_abox(U),set_mpred_module(U),!.
set_current_module(ABox):- '$set_typein_module'(ABox),'$set_source_module'(ABox).


%% chop_box( ?Chop, ?Was) is semidet.
%
% Chop Datalog.
%
chop_box(Chop,Was):-sanity(atom(Chop)),atom_concat(Was,'ABox',Chop),!.
chop_box(Chop,Was):-atom_concat(Was,'TBox',Chop),!.
chop_box(Chop,Was):-atom_concat(Was,'SBox',Chop),!.
chop_box(Chop,Chop).



%% box_type( ?F, ?A, ?VALUE3) is semidet.
%
% Datalog Type.
%
box_type(F,A,tbox):-current_predicate(baseKB:F/A).
box_type(_,_,abox).




% ========================================
% get_abox/1
% ========================================

%% reset_abox( ?M) is semidet.
%
% Reset User ABox.
%
reset_abox(M):- source_module(SM), ignore(show_failure(M\=user)),
   retractall(t_l:user_abox(SM,_Prev)),ensure_abox(M),!,asserta(t_l:user_abox(SM,M)).

reset_tbox(M):- source_module(SM), ignore(show_failure(M\=user)),
   retractall(t_l:user_tbox(SM,_Prev)),ensure_abox(M),!,asserta(t_l:user_tbox(SM,M)).



%% set_tbox( ?M, ?TBoxM) is semidet.
%
% Set User Tbox.
%
set_tbox(TBoxM):- get_abox(M),ensure_abox(TBoxM),set_tbox(M,TBoxM).
set_tbox(M,TBoxM):- 
   ( is_system_box(M) -> true ; maybe_add_import_module(M,TBoxM,end)).



%% is_box_module( ?M, ?VALUE2) is semidet.
%
% If Is A Datalog Module.
%
lmconf:is_box_module(M,tbox):- is_system_box(M).
lmconf:is_box_module(user,abox).



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
 U=logicmoo_user,
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
                           maybe_add_import_module(M,system,end),
                           maybe_add_import_module(user,M,end),
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
  maybe_add_import_module(M,system,end),  
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
   %maybe_add_import_module(BaseKB,mpred_loader,end),
   %maybe_add_import_module(M,mpred_loader,end),
   forall((system:current_module(IM), \+ lmconf:is_box_module(IM,_)),maybe_add_import_module(M,IM,end)),
   forall((system:current_module(IM),\+ lmconf:is_box_module(IM,_)),maybe_add_import_module(BaseKB,IM,end)),
   % skip_user(BaseKB),
   %ignore(maybe_delete_import_module(user,BaseKB)),
   %ignore(maybe_delete_import_module(BaseKB,user)),
   ignore(maybe_delete_import_module(M,BaseKB)),
   ignore(maybe_delete_import_module(BaseKB,M)),
   forall((prolog:current_predicate(_,BaseKB:P),\+predicate_property(BaseKB:P,imported_from(_))),import_shared_pred(M,BaseKB,P)),
   % maybe_add_import_module(user,BaseKB,end),
   % maybe_add_import_module(BaseKB,system,end),
   maybe_add_import_module(M,user,end),
   %maybe_add_import_module(BaseKB,M,end),
   %skip_user(M),
   ignore(maybe_delete_import_module(M,user)),
   maybe_add_import_module(user,M,end),
   ignore(maybe_delete_import_module(user,system)), % gets from M now
   !)).



maybe_hack_import_modules :- fail.

%% maybe_add_import_module( ?A, ?A, ?VALUE3) is semidet.
%
% Maybe Add Import Module.
%
maybe_add_import_module(_,baseKBTBox,end):-!,dumpST,dtrace,fail.
maybe_add_import_module(A,A,_):-!.
maybe_add_import_module(basePFC,_,end):-!.
maybe_add_import_module(baseKB,_,end):-!.
maybe_add_import_module(logicmoo_user, baseKB, end):-!.
maybe_add_import_module(user, baseKB, end):-!.
maybe_add_import_module(_,_,_):- \+ maybe_hack_import_modules,!.
maybe_add_import_module(A,B,C):- catch(add_import_module(A,B,C),_,logicmoo_util_dmsg:dmsg(failed(maybe_add_import_module(A,B,C)))),!.

maybe_delete_import_module(_,_):- \+ maybe_hack_import_modules,!.
maybe_delete_import_module(A,B):- catch(delete_import_module(A,B),_,logicmoo_util_dmsg:dmsg(failed(maybe_delete_import_module(A,B)))),!.


%% correct_module( ?M, ?X, ?T) is semidet.
%
% Correct Module.
%
correct_module(M,X,T):-functor(X,F,A),quietly_must(correct_module(M,F,A,T)),!.




%% correct_module( ?M, ?F, ?A, ?T) is semidet.
%
% Correct Module.
%
correct_module(abox,F,A,T):- !,get_abox(M),!,correct_module(M,F,A,T).
correct_module(tbox,F,A,T):- !,get_tbox(M),!,correct_module(M,F,A,T).
correct_module(sbox,F,A,T):- !,get_tbox(M),!,correct_module(M,F,A,T).
correct_module(M,F,A,T):- box_type(F,A,Type),!,to_box_type(M,Type,T).
correct_module(MT,_,_,MT):-!.

:- add_import_module(logicmoo_user,logicmoo_base,start).


