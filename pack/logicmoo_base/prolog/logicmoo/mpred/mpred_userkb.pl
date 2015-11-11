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

% DW> ... but is there a reason why "Absurdity" is the word used for something that doesn't exist?  SOWA> It's stronger than that.  The absurd type is defined by axioms that are contradictory.  Therefore, by definition, nothing of that type can exist. 
:- module(baseKB, []). % mpred_userkb_file/0

:- include('mpred_header.pi').

:-dynamic(base_kb_pred_list/1).

%= 	 	 

%% base_kb_pred_list( ?VALUE1) is semidet.
%
% Base Knowledge Base Predicate List.
%
base_kb_pred_list([
% prolog_load_file_loop_checked/2,
 (::::)/2,
 (<-)/2,
 (<==)/2,
 (<==>)/2,
 (==>)/1,
 (nesc)/1,
 (~)/1,
% mpred_f/1,
mpred_f/2,mpred_f/3,mpred_f/4,mpred_f/5,mpred_f/6,mpred_f/7,
%add_args/15,
%naf_in_code/1,
%neg_may_naf/1,
%tilda_in_code/1,
addTiny_added/1,
agent_call_command/2,
argGenl/3,
argIsa/3,
argQuotedIsa/3,
argsQuoted/1,
arity/2,
asserted_mpred_f/2,
asserted_mpred_f/3,
asserted_mpred_f/4,
asserted_mpred_f/5,
asserted_mpred_f/6,
asserted_mpred_f/7,
asserted_mpred_t/2,
asserted_mpred_t/3,
asserted_mpred_t/4,
asserted_mpred_t/5,
asserted_mpred_t/6,
asserted_mpred_t/7,
call_OnEachLoad/1,
coerce/3,
completelyAssertedCollection/1,
conflict/1,
constrain_args_pttp/2,
contract_output_proof/2,
current_world/1,
cyc_to_plarkc/2,
%cyckb_t/3,
cycPrepending/2,
decided_not_was_isa/2,
deduceFromArgTypes/1,
default_type_props/3,
defnSufficient/2,
did_learn_from_name/1,
elInverse/2,
feature_test/0,
formatted_resultIsa/2,
function_corisponding_predicate/2,
functorDeclares/1,
genls/2,
grid_key/1,
hybrid_support/2,
if_missing/2, % pfc
is_edited_clause/3,
is_wrapper_pred/1,
isa/2,
isCycAvailable_known/0,
isCycUnavailable_known/1,
lambda/5,
mpred_select/2,
localityOfObject/2,
meta_argtypes/1,
mpred_action/1,
mpred_default/1, % pfc
mpred_do_and_undo_method/2,
mpred_isa/2,
%mpred_manages_unknowns/0,
mpred_mark/4,
mpred_module/2,
mpred_univ/1,
mpred_univ/3,
mudKeyword/2,
never_assert_u/2,
never_assert_u0/2,
never_retract_u/2,
now_unused/1,
only_if_pttp/0,
pddlSomethingIsa/2,
pfcControlled/1,
pfcRHS/1,
predCanHaveSingletons/1,
prologBuiltin/1,
prologDynamic/1,prologDynamic/2,prologHybrid/1,prologHybrid/2,prologKIF/1,prologListValued/1,prologMacroHead/1,prologMultiValued/1,prologNegByFailure/1,prologOrdered/1,
prologPTTP/1,
prologSideEffects/1,
prologSingleValued/1,
props/2,ptReformulatorDirectivePredicate/1,pttp1a_wid/3,pttp_builtin/2,
% pttp_nnf_pre_clean_functor/3,
quasiQuote/1,relationMostInstance/3,resolveConflict/1,
ptSymmetric/1,
resolverConflict_robot/1,
resultIsa/2,
retractall_wid/1,
ruleRewrite/2,
search/7,
skolem/2,skolem/3,
singleValuedInArg/2,
subFormat/2,
support_hilog/2,
t/1,t/10,t/11,t/2,t/3,t/4,t/5,t/6,t/7,t/8,t/9,
tCol/1,
tFarthestReachableItem/1,
tFunction/1,
tNearestReachableItem/1,
tNotForUnboundPredicates/1,
tPred/1,
tRegion/1,
tRelation/1,
tried_guess_types_from_name/1,
tSet/1,
ttFormatType/1,
ttPredType/1,
ttTemporalType/1,
ttUnverifiableType/1,
type_action_info/3,
typeProps/2,
use_ideep_swi/0,
vtUnreifiableFunction/1,
was_chain_rule/1,
wid/3,
prologEquality/1,pfcBcTrigger/1,meta_argtypes/1,pfcDatabaseTerm/1,pfcControlled/1,pfcWatched/1,pfcMustFC/1,predIsFlag/1,tPred/1,prologMultiValued/1,
 prologSingleValued/1,prologMacroHead/1,notAssertable/1,prologBuiltin/1,prologDynamic/1,prologOrdered/1,prologNegByFailure/1,prologPTTP/1,prologKIF/1,prologEquality/1,prologPTTP/1,
 prologSideEffects/1,prologHybrid/1,prologListValued/1]).


% XXXXXXXXXXXXXXXXXXXXXXXXXx
% XXXXXXXXXXXXXXXXXXXXXXXXXx
% XXXXXXXXXXXXXXXXXXXXXXXXXx
% XXXXXXXXXXXXXXXXXXXXXXXXXx

:- meta_predicate

        t(?, ?, ?),
        t(?, ?, ?, ?),
        t(?, ?, ?, ?, ?),
        t(5,?,?,?,?,?),
        t(6,?,?,?,?,?,?),
        t(7,?,?,?,?,?,?,?).   

:- meta_predicate((
      baseKB:resolveConflict((*)),
      baseKB:resolveConflict0((*)),
      baseKB:resolverConflict_robot((*)))).

:- show_call(why,source_context_module(_CM)).
:- base_kb_pred_list(List),forall((member(E,List),E\='$pldoc'/4),baseKB:dynamic(baseKB:E)).
:- base_kb_pred_list(List),forall((member(E,List),E\='$pldoc'/4),decl_shared(E)).
:- base_kb_pred_list(List),forall((member(E,List),E\='$pldoc'/4),import_to_user(baseKB:E)).


:- import_module_to_user(logicmoo_user).
:- initialization(import_module_to_user(logicmoo_user)).


% :- module_property(baseKB, exports(List)),forall(member(E,List),kb_dynamic(E)).

% :- use_module(mpred_pfc).

:- source_location(F,_),asserta(lmconf:never_registered_mpred_file(F)).


%= 	 	 

%% prologNegByFailure( ?VALUE1) is semidet.
%
% Prolog Negated By Failure.
%
prologNegByFailure(prologNegByFailure).

%= 	 	 

%% completelyAssertedCollection( ?VALUE1) is semidet.
%
% Completely Asserted Collection.
%
completelyAssertedCollection(prologNegByFailure).

:- dynamic(t/2).
:- dynamic(t/1).
% t(C,I):- trace_or_throw(t(C,I)),t(C,I). % ,fail,loop_check_term(isa_backchaing(I,C),t(C,I),fail).

%t([P|LIST]):- !,mpred_plist_t(P,LIST).
%t(naf(CALL)):-!,not(t(CALL)).
%t(not(CALL)):-!,mpred_f(CALL).

%= 	 	 

%% t( ?VALUE1, ?VALUE2) is semidet.
%
% True Structure.
%
t(X,Y):- cwc, isa(Y,X).

%= 	 	 

%% t( ?CALL) is semidet.
%
% True Structure.
%
t(CALL):- cwc, call(into_plist_arities(3,10,CALL,[P|LIST])),mpred_plist_t(P,LIST).
:- meta_predicate(t(?,?,?,?,?)).
:- meta_predicate(t(?,?,?,?)).
:- meta_predicate(t(?,?,?)).

:-asserta((~(G):- cwc, neg_in_code(G))).

%= 	 	 

%% ~( ?VALUE1) is semidet.
%
% ~.
%
~(tCol('$VAR')).

% baseKB:import(I):-system:import(baseKB:I).



%= 	 	 

%% skolem( ?X, ?SK) is semidet.
%
% Skolem.
%
skolem(X,SK):-skolem_in_code(X,SK).


%= 	 	 

%% skolem( ?X, ?Vs, ?SK) is semidet.
%
% Skolem.
%
skolem(X,Vs,SK):-skolem_in_code(X,Vs,SK).



%= 	 	 

%% t( ?P, ?A1, ?A2) is semidet.
%
% True Structure.
%
t(P,A1,A2):- mpred_fa_call(P,2,call(P,A1,A2)).
t(P,A1,A2):- loop_check_mpred(t(P,A1,A2)).

%= 	 	 

%% t( ?P, ?A1, ?A2, ?A3) is semidet.
%
% True Structure.
%
t(P,A1,A2,A3):- mpred_fa_call(P,3,call(P,A1,A2,A3)).
t(P,A1,A2,A3):- loop_check_mpred(t(P,A1,A2,A3)).

%= 	 	 

%% t( ?P, ?A1, ?A2, ?A3, ?A4) is semidet.
%
% True Structure.
%
t(P,A1,A2,A3,A4):- mpred_fa_call(P,4,call(P,A1,A2,A3,A4)).
t(P,A1,A2,A3,A4):- loop_check_mpred(t(P,A1,A2,A3,A4)).

%= 	 	 

%% t( :PRED5P, ?A1, ?A2, ?A3, ?A4, ?A5) is semidet.
%
% True Structure.
%
t(P,A1,A2,A3,A4,A5):- mpred_fa_call(P,5,call(P,A1,A2,A3,A4,A5)).
t(P,A1,A2,A3,A4,A5):- loop_check_mpred(t(P,A1,A2,A3,A4,A5)).

%= 	 	 

%% t( :PRED6P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6) is semidet.
%
% True Structure.
%
t(P,A1,A2,A3,A4,A5,A6):- mpred_fa_call(P,6,call(P,A1,A2,A3,A4,A5,A6)).
t(P,A1,A2,A3,A4,A5,A6):- loop_check_mpred(t(P,A1,A2,A3,A4,A5,A6)).

%= 	 	 

%% t( :PRED7P, ?A1, ?A2, ?A3, ?A4, ?A5, ?A6, ?A7) is semidet.
%
% True Structure.
%
t(P,A1,A2,A3,A4,A5,A6,A7):- mpred_fa_call(P,7,call(P,A1,A2,A3,A4,A5,A6,A7)).
t(P,A1,A2,A3,A4,A5,A6,A7):- loop_check_mpred(t(P,A1,A2,A3,A4,A5,A6,A7)).


% :- use_module(logicmoo(mpred/mpred_loader)).
% :- use_module(logicmoo(mpred/mpred_pfc)).




%= 	 	 

%% current_world( ?VALUE1) is semidet.
%
% Current World.
%
baseKB:current_world(current).



%= 	 	 

%% mpred_univ( ?C, ?I, ?Head) is semidet.
%
% Managed Predicate Univ.
%
mpred_univ(C,I,Head):-atom(C),!,Head=..[C,I],predicate_property(Head,number_of_clauses(_)).


/*
:- use_module(logicmoo(mpred/'mpred_stubs.pl')).
:- use_module(logicmoo(mpred/'mpred_*.pl')).

baseKB:resolveConflict(C):- cwc, must((resolveConflict0(C),
  show_if_debug(is_resolved(C)),mpred_rem(conflict(C)))).
baseKB:resolveConflict(C) :- cwc,
  wdmsg("Halting with conflict ~p", [C]),   
  must(mpred_halt(conflict(C))),fail.
*/


%= 	 	 

%% resolveConflict0( ?C) is semidet.
%
% Resolve Conflict Primary Helper.
%
resolveConflict0(C) :- cwc, forall(must(mpred_negation_w_neg(C,N)),ignore(show_failure(why,(nop(baseKB:resolveConflict(C)),pp_why(N))))),
  ignore(show_failure(why,(nop(baseKB:resolveConflict(C)),pp_why(C)))), 
    doall((req(resolverConflict_robot(C)),\+ is_resolved(C),!)),
    is_resolved(C),!.


%= 	 	 

%% resolverConflict_robot( ?N) is semidet.
%
% Resolver Conflict Robot.
%
resolverConflict_robot(N) :- cwc, forall(must(mpred_negation_w_neg(N,C)),forall(compute_resolve(C,N,TODO),on_x_rtrace(show_if_debug(TODO)))).
resolverConflict_robot(C) :- cwc, must((mpred_remove3(C),wdmsg("Rem-3 with conflict ~p", [C]),mpred_run,sanity(\+C))).

% never_assert_u(pt(_,Pre,Post),head_singletons(Pre,Post)):- cwc, head_singletons(Pre,Post).

%= 	 	 

%% never_assert_u( :TermRule, ?Rule) is semidet.
%
% Never Assert For User Code.
%
never_assert_u(Rule,is_var(Rule)):- cwc, is_ftVar(Rule),!.
never_assert_u(Rule,head_singletons(Pre,Post)):- cwc, Rule \= (_:-_), once(mpred_rule_hb(Rule,Post,Pre)), head_singletons(Pre,Post).
never_assert_u(declared(M:F/A),never_declared(M:F/A)):- M:F/A = qrTBox:p/1.
never_assert_u(A,B):-never_assert_u0(A,B),trace,never_assert_u0(A,B).

% never_assert_u(M:arity(_,_),is_support(arity/2)):- M==pqr,dumpST, trace, cwc,!.
never_assert_u(M:Rule,Why):- cwc, atom(M),never_assert_u(Rule,Why).

/*
never_assert_u(pt(_,
       singleValuedInArg(A, _),
       (trace->rhs([{trace}, prologSingleValued(B)]))),singletons):- trace,A\=B,trace.
*/



%= 	 	 

%% never_assert_u0( :TermARG1, ?Why) is semidet.
%
% Never Assert For User Code Primary Helper.
%
never_assert_u0(mpred_mark(pfcPosTrigger,_,F,A),Why):- fail,
  functor(P,F,A),
  ignore(predicate_property(M:P,exported)),
  current_predicate(_,M:P),
  ( \+ predicate_property(M:P,imported_from(_))),  
  is_static_why(M,P,F,A,R),
  Why = static(M:P-F/A,R).


%= 	 	 

%% is_static_why( ?M, ?P, ?VALUE3, ?VALUE4, ?VALUE5) is semidet.
%
% If Is A Static Generation Of Proof.
%
is_static_why(M,P,_,_,_):- predicate_property(M:P,dynamic),!,fail.
is_static_why(M,P,F,A,WHY):- show_success(predicate_property(M:P,static)),!,WHY=static(M:F/A).

  

%  Pred='$VAR'('Pred'),unnumbervars(mpred_eval_lhs(basePFC:pt(UMT,singleValuedInArg(Pred,_G8263654),(trace->rhs([{trace},prologSingleValued(Pred)]))),(singleValuedInArg(Pred,_G8263679),{trace}==>{trace},prologSingleValued(Pred),u)),UN).
%  Pred='$VAR'('Pred'),unnumbervars(mpred_eval_lhs(basePFC:pt(UMT,singleValuedInArg(Pred,_G8263654),(trace->rhs([{trace},prologSingleValued(Pred)]))),(singleValuedInArg(Pred,_G8263679),{trace}==>{trace},prologSingleValued(Pred),u)),UN).


:- source_location(S,_),forall(source_file(M:H,S),(functor(H,F,A),module_transparent(M:F/A))).
:- add_import_module(baseKB,basePFC,end).
:- initialization(add_import_module(baseKB,basePFC,end)).

mpred_userkb_file.




