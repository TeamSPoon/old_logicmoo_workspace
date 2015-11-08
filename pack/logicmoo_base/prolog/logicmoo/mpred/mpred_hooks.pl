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
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl
:- module(mpred_hooks,[

(which_t)/1,
add_arg_parts_of_speech/4,
agent_action_queue/3,
agent_session/2,
agent_text_command/4,
argIsa_call_or_undressed/4,

call_f/3,
call_f/4,
call_f/5,
call_f/6,
call_f/7,
call_f/8,
call_mt_f/10,
call_mt_f/4,
call_mt_f/5,
call_mt_f/6,
call_mt_f/7,
call_mt_f/8,
call_mt_f/9,
call_mt_t/10,
call_mt_t/4,
call_mt_t/5,
call_mt_t/6,
call_mt_t/7,
call_mt_t/8,
call_mt_t/9,
call_t/3,
call_t/4,
call_t/5,
call_t/6,
call_t/7,
call_t/8,
call_which_t/3,
call_which_t/4,
call_which_t/5,
call_which_t/6,
call_which_t/7,
call_which_t/8,
call_which_t/9,
compute_value/2,
compute_value_no_dice/2,
create_random_fact/1,
deduce_facts/2,
fact_always_true/1,
fact_is_false/2,
fact_maybe_deduced/1,
flatten_append/3,
fskel/7,
holds_f/1,
holds_f/2,
holds_f/3,
holds_f/4,
holds_f/5,
holds_f/6,
holds_f/7,
holds_f/8,
holds_plist_t/2,
holds_relaxed_0_t/4,
holds_relaxed_t/3,
holds_t/1,
holds_t/2,
holds_t/3,
holds_t/4,
holds_t/5,
holds_t/6,
holds_t/7,
holds_t/8,
hooked_random_instance/3,
if_result/2,
insert_into/4,
into_plist/2,
into_plist_arities/4,
inverse_args/2,
isCycPredArity_ignoreable/2,
list_update_op/3,
loaded_external_kbs/0,
local_term_anglify/2,
loop_check_mpred/1,
mpred_fact_arity/2,
mpred_module_ready/0,
mpred_fa_call/3,
mpred_plist_t/2,
never_mpred_mpred/1,
prologHybridFact/1,
replace_arg/4,
replace_nth_arglist/4,
replace_nth_ref/5,
same_vars/2,
session_agent/2,
session_io/4,
telnet_fmt_shown/3,
term_anglify_last/2,
term_anglify_np/3,
term_anglify_np_last/3,
tf_result/2,
tms_reject_why/2,
update_value/3,
use_cyc_database/0,
use_kif/2,
verb_after_arg/3,
xcall_f/1,
xcall_f/2,
xcall_f/3,
xcall_f/4,
xcall_f/5,
xcall_f/6,
xcall_f/7,
xcall_t/1,
xcall_t/2,
xcall_t/3,
xcall_t/4,
xcall_t/5,
xcall_t/6,
xcall_t/7,
mpred_hooks_file/0
]).

% XXXXXXXXXXXXXXXXXXXXXXXXXx
% XXXXXXXXXXXXXXXXXXXXXXXXXx
% XXXXXXXXXXXXXXXXXXXXXXXXXx
% XXXXXXXXXXXXXXXXXXXXXXXXXx

:- meta_predicate 
        call_f(?, 1, ?),
        call_f(?, 2, ?, ?),
        call_f(?, 3, ?, ?, ?),
        call_f(?, 4, ?, ?, ?, ?),
        call_f(?, 5, ?, ?, ?, ?, ?),
        call_f(?, 6, ?, ?, ?, ?, ?, ?),
        call_mt_f(?, 2, ?, ?),
        call_mt_f(?, 3, ?, ?, ?),
        call_mt_f(?, 4, ?, ?, ?, ?),
        call_mt_f(?, 5, ?, ?, ?, ?, ?),
        call_mt_f(?, 6, ?, ?, ?, ?, ?, ?),
        call_mt_t(?, 2, ?, ?),
        call_mt_t(?, 3, ?, ?, ?),
        call_mt_t(?, 4, ?, ?, ?, ?),
        call_mt_t(?, 5, ?, ?, ?, ?, ?),
        call_mt_t(?, 6, ?, ?, ?, ?, ?, ?),
        call_which_t(?, 1, ?),
        call_which_t(?, 2, ?, ?),
        call_which_t(?, 3, ?, ?, ?),
        call_which_t(?, 4, ?, ?, ?, ?),
        call_which_t(?, 5, ?, ?, ?, ?, ?),
        call_which_t(?, 6, ?, ?, ?, ?, ?, ?),
        holds_f(5, ?, ?, ?, ?, ?),
        holds_f(6, ?, ?, ?, ?, ?, ?),
        holds_t(5, ?, ?, ?, ?, ?),
        holds_t(6, ?, ?, ?, ?, ?, ?),
        if_result(0, 0),
        loop_check_mpred(?),
        mpred_fa_call(?, ?, 0),
  
        tf_result(0, +),
        xcall_f(0),
        xcall_f(1, ?),
        xcall_f(2, ?, ?),
        xcall_f(3, ?, ?, ?),
        xcall_f(4, ?, ?, ?, ?),
        xcall_f(5, ?, ?, ?, ?, ?),
        xcall_f(6, ?, ?, ?, ?, ?, ?),
        xcall_t(0),
        xcall_t(1, ?),
        xcall_t(2, ?, ?),
        xcall_t(3, ?, ?, ?),
        xcall_t(4, ?, ?, ?, ?),
        xcall_t(5, ?, ?, ?, ?, ?),
        xcall_t(6, ?, ?, ?, ?, ?, ?).

:- dynamic((
(which_t)/1,
add_arg_parts_of_speech/4,
agent_action_queue/3,
agent_session/2,
agent_text_command/4,
argIsa_call_or_undressed/4,
call_f/3,
call_f/4,
call_f/5,
call_f/6,
call_f/7,
call_f/8,
call_mt_f/10,
call_mt_f/4,
call_mt_f/5,
call_mt_f/6,
call_mt_f/7,
call_mt_f/8,
call_mt_f/9,
call_mt_t/10,
call_mt_t/4,
call_mt_t/5,
call_mt_t/6,
call_mt_t/7,
call_mt_t/8,
call_mt_t/9,
call_t/3,
call_t/4,
call_t/5,
call_t/6,
call_t/7,
call_t/8,
call_which_t/3,
call_which_t/4,
call_which_t/5,
call_which_t/6,
call_which_t/7,
call_which_t/8,
call_which_t/9,
compute_value/2,
compute_value_no_dice/2,
create_random_fact/1,
deduce_facts/2,
fact_always_true/1,
fact_is_false/2,
fact_maybe_deduced/1,
flatten_append/3,
fskel/7,
holds_f/1,
holds_f/2,
holds_f/3,
holds_f/4,
holds_f/5,
holds_f/6,
holds_f/7,
holds_f/8,
holds_plist_t/2,
holds_relaxed_0_t/4,
holds_relaxed_t/3,
holds_t/1,
holds_t/2,
holds_t/3,
holds_t/4,
holds_t/5,
holds_t/6,
holds_t/7,
holds_t/8,
hooked_random_instance/3,
if_result/2,
insert_into/4,
into_plist/2,
into_plist_arities/4,
inverse_args/2,
isCycPredArity_ignoreable/2,
list_update_op/3,
loaded_external_kbs/0,
local_term_anglify/2,
loop_check_mpred/1,
mpred_fact_arity/2,
mpred_module_ready/0,
mpred_fa_call/3,
mpred_plist_t/2,
never_mpred_mpred/1,
prologHybridFact/1,
replace_arg/4,
replace_nth_arglist/4,
replace_nth_ref/5,
same_vars/2,
session_agent/2,
session_io/4,
telnet_fmt_shown/3,
term_anglify_last/2,
term_anglify_np/3,
term_anglify_np_last/3,
tf_result/2,
tms_reject_why/2,
update_value/3,
use_cyc_database/0,
use_kif/2,
verb_after_arg/3,
xcall_f/1,
xcall_f/2,
xcall_f/3,
xcall_f/4,
xcall_f/5,
xcall_f/6,
xcall_f/7,
xcall_t/1,
xcall_t/2,
xcall_t/3,
xcall_t/4,
xcall_t/5,
xcall_t/6,
xcall_t/7
)).
% :- registerCycPredMtWhy([genlPreds/4,genlInverse/4,localityOfObject/4]).

/* <module> mpred_mpred_t
% Provides a prolog dabase in these predicates...
%
%  t/N
%  hybridRule/2
%  
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/



% ========================================
% is_holds_true/is_holds_false
% ========================================


:- was_export(into_plist/2).

%= 	 	 

%% into_plist( ?In, ?Out) is semidet.
%
% Converted To Plist.
%
into_plist(In,Out):-into_plist_arities(2,12,In,Out).

:- was_export(into_plist_arities/4).

%= 	 	 

%% into_plist_arities( ?Min, ?Max, ?PLIST, ?PLISTO) is semidet.
%
% Converted To Plist Arities.
%
into_plist_arities(Min,Max,PLIST,PLISTO):- var(PLIST),!,between(Min,Max,X),length(PLIST,X),PLISTO=PLIST.
into_plist_arities(_,_,[P|LIST],[P|LIST]):-var(P),!.
into_plist_arities(_,_,[t|PLIST],PLIST):-!.  % t is our versuion of '$holds' or call/N
into_plist_arities(_,_,plist(P,LIST),[P|LIST]):-!.
into_plist_arities(_,_,Call,PLIST):-Call=..PLIST. % finally the fallthrue



%= 	 	 

%% never_mpred_mpred( ?VALUE1) is semidet.
%
% Never Managed Predicate Managed Predicate.
%
never_mpred_mpred(mpred_isa).
never_mpred_mpred(isa).
never_mpred_mpred(arity).



:- include('mpred_header.pi').

:- shared_multifile agent_action_queue/3.


:- shared_multifile create_random_fact/1.
% :- shared_multifile decl_database_hook/2.
:- shared_multifile deduce_facts/2.
:- shared_multifile default_type_props/3.
:- shared_multifile fact_always_true/1.
:- shared_multifile fact_maybe_deduced/1.
:- shared_multifile tms_reject_why/2.
:- shared_multifile fskel/7.
:- shared_multifile hooked_random_instance/3.

:- shared_multifile now_unused/1.

:- shared_multifile session_io/4, session_agent/2, agent_session/2, telnet_fmt_shown/3, agent_action_queue/3.




:- shared_multifile(lmconf:startup_option/2).
:- shared_multifile(is_edited_clause/3).




:- shared_multifile use_cyc_database/0.
:- shared_multifile agent_session/2.

:- shared_multifile fact_is_false/2.
% :- shared_multifile kbp_t_list_prehook/2.




:- shared_multifile mudKeyword/2.
:- shared_multifile only_if_pttp/0.
:- shared_multifile relationMostInstance/3.


:- shared_multifile tFarthestReachableItem/1.
:- shared_multifile tNearestReachableItem/1.
:- shared_multifile use_kif/2.

:- shared_multifile(agent_text_command/4).

:- shared_multifile(grid_key/1).



:- shared_multifile(use_cyc_database/0).
% :- shared_multifile decl_database_hook/2.


:- shared_multifile(mpred_module_ready).

:- shared_multifile loaded_external_kbs/0.

:- shared_multifile loading_module/1.
:- shared_multifile local_term_anglify/2.


:- shared_multifile term_anglify_last/2.
:- shared_multifile term_anglify_np/3.
:- shared_multifile term_anglify_np_last/3.




:- meta_predicate(tf_result(0,+)).

%= 	 	 

%% tf_result( :GoalCall, +TF) is semidet.
%
% True/false Result.
%
tf_result(Call,TF):-(Call->TF=true;TF=fail).
:- meta_predicate(if_result(0,0)).

%= 	 	 

%% if_result( :GoalTF, :GoalCall) is semidet.
%
% If Result.
%
if_result(TF,Call):-(TF->Call;true).



% ================================================================================
% begin holds_t
% ================================================================================


%= 	 	 

%% mpred_plist_t( ?P, :TermLIST) is semidet.
%
% Managed Predicate Plist True Stucture.
%
mpred_plist_t(P,[]):-!,t(P).
mpred_plist_t(P,LIST):-var(P),!,is_list(LIST),CALL=..[t,P|LIST],on_x_rtrace((CALL)).
mpred_plist_t(t,[P|LIST]):-!, mpred_plist_t(P,LIST).
mpred_plist_t(mpred_isa,[C,_A,I]):-!,ground(I:C),mpred_isa(C,I).
mpred_plist_t(isa,[I,C]):-!,t(C,I).
mpred_plist_t(P,_):-never_mpred_mpred(P),!,fail.
mpred_plist_t(P,[L|IST]):-is_holds_true(P),!,mpred_plist_t(L,IST).
mpred_plist_t(P,LIST):-is_holds_false(P),!,mpred_f(LIST).
mpred_plist_t(P,LIST):- CALL=..[t,P|LIST],on_x_rtrace(CALL).

:- meta_predicate(loop_check_mpred(?)).
% loop_check_mpred(Call):- current_predicate(ireq/1), loop_check_term(ireq(Call),loop_check_mpred(Call),fail).

%= 	 	 

%% loop_check_mpred( ?Call) is semidet.
%
% Loop Check Managed Predicate.
%
loop_check_mpred(Call):- !, fail,not(t_l:infInstanceOnly(_)),loop_check_term(ireq(Call),loop_check_mpred(Call),fail).
% loop_check_mpred(Call):-loop_check(req(t,Call),fail).

:- meta_predicate(mpred_fa_call(?,?,0)).



%= 	 	 

%% mpred_fa_call( ?F, ?UPARAM2, :GoalCall) is semidet.
%
% Managed Predicate Functor-arity Call.
%
mpred_fa_call(F,A,Call):- var(F),!,support_hilog(F,A),\+tNotForUnboundPredicates(F),current_predicate(F,M:_OtherCall),on_x_rtrace(M:Call).
mpred_fa_call(F,_,Call):-current_predicate(F,M:_OtherCall),M:Call.


%= 	 	 

%% mpred_fact_arity( ?VALUE1, ?VALUE2) is semidet.
%
% Managed Predicate Fact Arity.
%
mpred_fact_arity(F,A):-arity(F,A),once(mpred_isa(F,prologHybrid);mpred_isa(F,pfcControlled);mpred_isa(F,prologPTTP);mpred_isa(F,prologKIF)).


%= 	 	 

%% prologHybridFact( ?G) is semidet.
%
% Prolog Hybrid Fact.
%
prologHybridFact(G):- (var(G)->(mpred_fact_arity(F,A),functor(G,F,A));true),into_mpred_form(G,M),!,no_repeats(req(M)).


%= 	 	 

%% isCycPredArity_ignoreable( ?VALUE1, ?VALUE2) is semidet.
%
% If Is A Cyc Predicate Arity Ignoreable.
%
isCycPredArity_ignoreable(F,A):- ignore(mpred_isa(F,cycPred(A))),ignore(arity(F,A)).


%= 	 	 

%% which_t( ?VALUE1) is semidet.
%
% Which True Stucture.
%
which_t(dac(d,a_notnow,c,no_fallback)).


%= 	 	 

%% holds_t( ?P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6, ?A7) is semidet.
%
% Holds True Stucture.
%
holds_t(P,A1,A2,A3,A4,A5,A6,A7):- isCycPredArity_ignoreable(P,7),which_t(DBS),(call_which_t(DBS,P,A1,A2,A3,A4,A5,A6,A7);call_mt_t(DBS,P,A1,A2,A3,A4,A5,A6,A7,_,_);assertion_t([P,A1,A2,A3,A4,A5,A6,A7])).

%= 	 	 

%% holds_t( :PRED6P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6) is semidet.
%
% Holds True Stucture.
%
holds_t(P,A1,A2,A3,A4,A5,A6):- isCycPredArity_ignoreable(P,6),which_t(DBS),(call_which_t(DBS,P,A1,A2,A3,A4,A5,A6);call_mt_t(DBS,P,A1,A2,A3,A4,A5,A6,_,_)).


%= 	 	 

%% holds_t( :PRED5P, ?A1, ?A2, ?A3, ?A4, ?A5) is semidet.
%
% Holds True Stucture.
%
holds_t(P,A1,A2,A3,A4,A5):- isCycPredArity_ignoreable(P,5),which_t(DBS),(call_which_t(DBS,P,A1,A2,A3,A4,A5);call_mt_t(DBS,P,A1,A2,A3,A4,A5,_,_)).


%= 	 	 

%% holds_t( ?P, ?A1, ?A2, ?A3, ?A4) is semidet.
%
% Holds True Stucture.
%
holds_t(P,A1,A2,A3,A4):- isCycPredArity_ignoreable(P,4),which_t(DBS),(call_which_t(DBS,P,A1,A2,A3,A4);call_mt_t(DBS,P,A1,A2,A3,A4,_,_)).

%= 	 	 

%% holds_t( ?P, ?A1, ?A2, ?A3) is semidet.
%
% Holds True Stucture.
%
holds_t(P,A1,A2,A3):- isCycPredArity_ignoreable(P,3),which_t(DBS),(call_which_t(DBS,P,A1,A2,A3);call_mt_t(DBS,P,A1,A2,A3,_,_)).
%holds_t(P,A1,A2):- hotrace(holds_relaxed_t(P,A1,A2)).

%= 	 	 

%% holds_t( ?P, ?A1, ?A2) is semidet.
%
% Holds True Stucture.
%
holds_t(P,A1,A2):- isCycPredArity_ignoreable(P,2),which_t(DBS),(call_which_t(DBS,P,A1,A2);call_mt_t(DBS,P,A1,A2,_,_)).

%= 	 	 

%% holds_t( ?P, ?A1) is semidet.
%
% Holds True Stucture.
%
holds_t(P,A1):- isCycPredArity_ignoreable(P,1),which_t(DBS),(call_which_t(DBS,P,A1);call_mt_t(DBS,P,A1,_,_)).


% holds_relaxed_t(P,A1,A2):-var(A1),var(A2),!,t(P,A1,A2).

%= 	 	 

%% holds_relaxed_t( ?P, ?A1, ?A2) is semidet.
%
% Holds Relaxed True Stucture.
%
holds_relaxed_t(P,A1,A2):-
  isCycPredArity_ignoreable(P,2),which_t(DBS),
      relax_term(P,PR,A1,R1,A2,R2),
         holds_relaxed_0_t(DBS,PR,R1,R2).


%= 	 	 

%% holds_relaxed_0_t( ?DBS, ?P, ?A1, ?A2) is semidet.
%
% holds relaxed  Primary Helper True Stucture.
%
holds_relaxed_0_t(DBS,P,A1,A2):- call_which_t(DBS,P,A1,A2).
holds_relaxed_0_t(DBS,P,A1,A2):- call_mt_t(DBS,P,A1,A2,_,_).

/*
holds_relaxed_0_t(dac(_,a,_,_),P,A1,A2):- assertion_t([P,A1,A2]).
holds_relaxed_0_t(dac(d,_,_,_),P,A1,A2):- t(P,A1,A2).
holds_relaxed_0_t(dac(_,_,_,h),P,A1,A2):- call_which_t(DBS,P,A1,A2).
holds_relaxed_0_t(DBS,P,A1,A2):- call_mt_t(DBS,P,A1,A2,_,_).
holds_relaxed_0_t(_DBS,P,A1,A2):- ground((P,A1)), TEMPL=..[P,T1,_],t(argSingleValueDefault,TEMPL,2,A2),req(isa(A1,T1)),!.
*/


%= 	 	 

%% holds_t( :TermCALL) is semidet.
%
% Holds True Stucture.
%
holds_t([AH,P|LIST]):- is_holds_true(AH),!,holds_plist_t(P,LIST).
holds_t([AH,P|LIST]):- is_holds_false(AH),!,holds_f_p2(P,LIST).
holds_t([P|LIST]):- !,holds_plist_t(P,LIST).
holds_t(not(CALL)):- !, holds_f(CALL).
holds_t(CALL):- '=..'(CALL,PLIST),holds_t(PLIST).


%= 	 	 

%% holds_plist_t( ?P, ?LIST) is semidet.
%
% Holds Plist True Stucture.
%
holds_plist_t(P,LIST):- apply(holds_t,[P|LIST]).




% =======================================================
% term utils
% =======================================================

:- was_export(inverse_args/2).

%= 	 	 

%% inverse_args( ?AR, ?GS) is semidet.
%
% Inverse Arguments.
%
inverse_args([AR,GS],[GS,AR]):-!.
inverse_args([AR,G,S],[S,G,AR]):-!.
inverse_args([A,R,G,S],[S,R,G,A]):-!.
inverse_args([P,A,R,G,S],[S,A,R,G,P]):-!.

:- was_export(same_vars/2).

%= 	 	 

%% same_vars( ?T1, ?T2) is semidet.
%
% Same Variables.
%
same_vars(T1,T2):-term_variables(T1,V1),term_variables(T2,V2),!,V1==V2.


%= 	 	 

%% replace_arg( ?C, :PRED3A, ?VAR, ?CC) is semidet.
%
% Replace Argument.
%
replace_arg(C,0,VAR,CC):-!, C=..[_|ARGS],CC=..[VAR|ARGS].
replace_arg(C,1,VAR,CC):-!, C=..[F,_|ARGS],CC=..[F,VAR|ARGS].
replace_arg(C,2,VAR,CC):-!, C=..[F,A,_|ARGS],CC=..[F,A,VAR|ARGS].
replace_arg(C,3,VAR,CC):-!, C=..[F,A,B,_|ARGS],CC=..[F,A,B,VAR|ARGS].
% replace_arg(C,A,VAR,CO):- duplicate_term(C,CC),setarg(A,CC,VAR),!,CC=CO.
replace_arg(C,A,VAR,CC):- C=..FARGS,replace_nth_arglist(FARGS,A,VAR,FARGO),!,CC=..FARGO.

:- mpred_trace_nochilds(replace_arg/4).

%% replace_nth_arglist(+List, +Index, +Element, -NewList) is det[private]
% Replace the Nth (1-based) element of a list.
:- mpred_trace_nochilds(replace_nth_arglist/4).

%= 	 	 

%% replace_nth_arglist( :TermARG1, ?VALUE2, ?VAR, :TermVAR) is semidet.
%
% Replace Nth Arglist.
%
replace_nth_arglist([],_,_,[]):- !.
replace_nth_arglist([_|ARGO],0,VAR,[VAR|ARGO]):- !.
replace_nth_arglist([T|FARGS],A,VAR,[T|FARGO]):- 
    A2 is A-1,replace_nth_arglist(FARGS,A2,VAR,FARGO).



%= 	 	 

%% replace_nth_ref( :TermARG1, ?N, ?OldVar, ?NewVar, :TermARG5) is semidet.
%
% Replace Nth Ref.
%
replace_nth_ref([],_N,_OldVar,_NewVar,[]):- !,trace_or_throw(missed_the_boat).
replace_nth_ref([OldVar|ARGS],1,OldVar,NewVar,[NewVar|ARGS]):- !.
replace_nth_ref([Carry|ARGS],Which,OldVar,NewVar,[Carry|NEWARGS]):- 
 Which1 is Which-1,
 replace_nth_ref(ARGS,Which1,OldVar,NewVar,NEWARGS),!.


:- mpred_trace_nochilds(update_value/3).

%= 	 	 

%% update_value( ?OLD, ?NEW, ?NEXT) is semidet.
%
% Update Value.
%
update_value(OLD,NEW,NEXT):- var(NEW),!,trace_or_throw(logicmoo_bug(update_value(OLD,NEW,NEXT))).
update_value(OLD,NEW,NEWV):- var(OLD),!,compute_value_no_dice(NEW,NEWV).
update_value(OLD,X,NEW):- is_list(OLD),!,list_update_op(OLD,X,NEW),!.
update_value(OLDI,+X,NEW):- compute_value(OLDI,OLD),number(OLD),catch(NEW is OLD + X,_,fail),!.
update_value(OLDI,-X,NEW):- compute_value(OLDI,OLD),number(OLD),catch(NEW is OLD - X,_,fail),!.
update_value(OLDI,X,NEW):- number(X),X<0,compute_value(OLDI,OLD),number(OLD),catch(NEW is OLD + X,_,fail),!.
update_value(_,NEW,NEWV):- compute_value_no_dice(NEW,NEWV),!.


%= 	 	 

%% flatten_append( ?First, ?Last, ?Out) is semidet.
%
% Flatten Append.
%
flatten_append(First,Last,Out):-flatten([First],FirstF),flatten([Last],LastF),append(FirstF,LastF,Out),!.


%= 	 	 

%% list_update_op( ?OLDI, :TermX, ?NEW) is semidet.
%
% List Update Oper..
%
list_update_op(OLDI,+X,NEW):-flatten_append(OLDI,X,NEW),!.
list_update_op(OLDI,-X,NEW):-flatten([OLDI],OLD),flatten([X],XX),!,list_difference_eq(OLD,XX,NEW),!.


%= 	 	 

%% compute_value_no_dice( ?NEW, ?NEW) is semidet.
%
% Compute Value No Dice.
%
compute_value_no_dice(NEW,NEW):- compound(NEW),functor_catch(NEW,ftDice,_),!.
compute_value_no_dice(NEW,NEWV):-compute_value(NEW,NEWV).


%= 	 	 

%% compute_value( ?NEW, ?NEWV) is semidet.
%
% Compute Value.
%
compute_value(NEW,NEWV):-catch(NEWV is NEW,_,fail),!.
compute_value(NEW,NEWV):-catch(any_to_value(NEW,NEWV),_,fail),!.
compute_value(NEW,NEW).


%= 	 	 

%% insert_into( :TermARGS, ?VALUE2, ?Insert, :TermInsert) is semidet.
%
% Insert Converted To.
%
insert_into(ARGS,0,Insert,[Insert|ARGS]):- !.
insert_into([Carry|ARGS],After,Insert,[Carry|NEWARGS]):- 
   After1 is After - 1,
   insert_into(ARGS,After1,Insert,NEWARGS).



%= 	 	 

%% add_arg_parts_of_speech( ?F, ?N, :TermARG3, :TermARG4) is semidet.
%
% Add Argument Parts Of Speech.
%
add_arg_parts_of_speech(_F,_N,[],[]).
add_arg_parts_of_speech(F,N,[A|ARGS0],[ARG|ARGS]):-argIsa_call_or_undressed(F,N,A,ARG),N1 is N+1, add_arg_parts_of_speech(F,N1,ARGS0,ARGS).


%= 	 	 

%% argIsa_call_or_undressed( ?F, ?N, ?Obj, ?Obj) is semidet.
%
% Argument  (isa/2) call or undressed.
%
argIsa_call_or_undressed(F,N,Obj,fN(Obj,Type)):- argIsa_call_0(F,N,Type),!.
argIsa_call_or_undressed(_F,_N,Obj,Obj).


%= 	 	 

%% verb_after_arg( ?VALUE1, ?VALUE2, :PRED1VALUE3) is semidet.
%
% Verb After Argument.
%
verb_after_arg(_,_,1).

:- source_location(S,_),forall(source_file(H,S),(functor(H,F,A),export(F/A),module_transparent(F/A))).

mpred_hooks_file.
