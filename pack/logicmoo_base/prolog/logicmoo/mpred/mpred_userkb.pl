/* <module> 
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
:- module(kb, [

mpred_default/1, % pfc
if_missing/2, % pfc


registered_module_type/2,
% current_op_alias/2,
% prolog_load_file_loop_checked/2,
current_world/1,
mpred_mark/4,
(==>)/1,
(::::) / 2,
(<-)/2,
(<==>)/2,
(==>)/2,
(neg)/1,
(nesc)/1,
mpred_action/1,
mpred_do_and_undo_method/2,
prologMultiValued/1,
prologOrdered/1,
prologNegByFailure/1,
prologPTTP/1,
prologKIF/1,
pfcControlled/1,
tPredType/1,
prologHybrid/1,
predCanHaveSingletons/1,
prologDynamic/1,
prologBuiltin/1,
prologMacroHead/1,
prologListValued/1,
prologSingleValued/1,
prologDynamic/2,
prologSideEffects/1,
singleValuedInArg/2,
lmconf:mpred_select/2,
agent_action_queue/3,
agent_session/2,
agent_text_command/4,

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
mpred_f/2,
mpred_f/3,
mpred_f/4,
mpred_f/5,
mpred_f/6,
mpred_f/7,
t/1,
t/10,
t/11,
t/2,
t/3,
t/4,
t/5,
t/6,
t/7,
t/8,
t/9,


assertion_f/1,
create_random_fact/1,
deduce_facts/2,
default_type_props/3,
fact_always_true/1,
fact_is_false/2,
fact_maybe_deduced/1,
fskel/7,
grid_key/1,
hooked_random_instance/3,
is_edited_clause/3,
loaded_external_kbs/0,
loading_module_h/1,
local_term_anglify/2,
mpred_module_ready/0,
mudKeyword/2,
never_registered_mpred_file/1,
now_unused/1,
only_if_pttp/0,
registered_mpred_file/1,
relationMostInstance/3,
session_agent/2,
session_io/4,
startup_option/2,
kb:never_assert_u/2,
kb:never_retract_u/2,
tFarthestReachableItem/1,
tNearestReachableItem/1,
/*
telnet_fmt_shown/3,
term_anglify_last/2,
term_anglify_np/3,
term_anglify_np_last/3,
tms_reject_why/2,
use_cyc_database/0,
use_kif/2,
if_result/2,
insert_into/4,
isCycPredArity_ignoreable/2,
list_update_op/3,
loop_check_mpred/1,
mpred_fact_arity/2,
mpred_pa_call/3,
mpred_plist_t/2,
never_mpred_mpred/1,
replace_arg/4,
replace_nth_arglist/4,
replace_nth_ref/5,
tf_result/2,
update_value/3,
% verb_after_arg/3,
which_t/1,
*/
mpred_module/2,
add_args/15,
decl_mpred_multifile/1,
addTiny_added/1,
argGenl/3,
argIsa/3,
argQuotedIsa/3,
argsQuoted/1,
arity/2,
call_mt_t/11,
call_which_t/9,
completelyAssertedCollection/1,
conflict/1,
constrain_args_pttp/2,
contract_output_proof/2,
cyc_to_plarkc/2,
cycPrepending/2,
decided_not_was_isa/2,
defnSufficient/2,
did_learn_from_name/1,
% f_to_mfa/4,
formatted_resultIsa/2,
genls/2,
% get_clause_vars_for_print/2,
holds_f_p2/2,
is_wrapper_pred/1,
isa/2,
%isa_asserted_0/2,
%isa_pred_now_locked/0,
isCycAvailable_known/0,
isCycUnavailable_known/1,
lambda/5,
localityOfObject/2,
logical_functor_pttp/1,
meta_argtypes/1,
mpred_f/1,
mpred_isa/2,
mpred_to_cyc/2,
mpred_univ/1,
mpred_univ/3,
pddlSomethingIsa/2,
pfcRHS/1,
%pp_i2tml_now/1,
%pp_item_html/2,
pttp1a_wid/3,
pttp_builtin/2,
pttp_nnf_pre_clean_functor/3,
quasiQuote/1,
relax_term/6,
resolveConflict/1,
resolverConflict_robot/1,
resultIsa/2,
retractall_wid/1,
ruleRewrite/2,
search/7,
subFormat/2,
support_hilog/2,
tCol/1,
tFunction/1,
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
typeProps/2,
vtUnreifiableFunction/1,
call_OnEachLoad/1,
was_chain_rule/1,
ptReformulatorDirectivePredicate/1,
props/2,
functorDeclares/1,
whymemory/2,
prologHybrid/2,
use_ideep_swi/0,
mpred_manages_unknowns/0,
coerce/3,
current_source_suffix/1,
function_corisponding_predicate/2,
cyckb_t/3,
elInverse/2,
agent_call_command/2,
feature_test/0,
type_action_info/3,
wid/3,
deduceFromArgTypes/1,

kb_dynamic/1

 ]).


:- meta_predicate
        t(?, ?, ?),
        t(?, ?, ?, ?),
        t(?, ?, ?, ?, ?),
        t(5,?,?,?,?,?),
        t(6,?,?,?,?,?,?),
        t(7,?,?,?,?,?,?,?).   


do_kb_export(kb:F/A):-!,do_kb_export(F/A).
do_kb_export(M:F/A):-!, M:multifile(M:F/A),M:dynamic(M:F/A),export(M:F/A),kb:import(M:F/A).
do_kb_export(F/A):-!, kb:multifile(F/A),kb:dynamic(F/A).

:- module_property(kb, exports(List)),maplist(do_kb_export,List).
:- use_module(mpred_pfc).

prologNegByFailure(prologNegByFailure).
completelyAssertedCollection(prologNegByFailure).

:- dynamic(t/2).
:- dynamic(t/1).
% t(C,I):- trace_or_throw(t(C,I)),t(C,I). % ,fail,loop_check_term(isa_backchaing(I,C),t(C,I),fail).

%t([P|LIST]):- !,mpred_plist_t(P,LIST).
%t(naf(CALL)):-!,not(t(CALL)).
%t(not(CALL)):-!,mpred_f(CALL).
t(X,Y):- cwc, isa(Y,X).
t(CALL):- cwc, call(into_plist_arities(3,10,CALL,[P|LIST])),mpred_plist_t(P,LIST).
:- meta_predicate(t(?,?,?,?,?)).
:- meta_predicate(t(?,?,?,?)).
:- meta_predicate(t(?,?,?)).

t(P,A1,A2):- mpred_pa_call(P,2,call(P,A1,A2)).
t(P,A1,A2):- loop_check_mpred(t(P,A1,A2)).
t(P,A1,A2,A3):- mpred_pa_call(P,3,call(P,A1,A2,A3)).
t(P,A1,A2,A3):- loop_check_mpred(t(P,A1,A2,A3)).
t(P,A1,A2,A3,A4):- mpred_pa_call(P,4,call(P,A1,A2,A3,A4)).
t(P,A1,A2,A3,A4):- loop_check_mpred(t(P,A1,A2,A3,A4)).
t(P,A1,A2,A3,A4,A5):- mpred_pa_call(P,5,call(P,A1,A2,A3,A4,A5)).
t(P,A1,A2,A3,A4,A5):- loop_check_mpred(t(P,A1,A2,A3,A4,A5)).
t(P,A1,A2,A3,A4,A5,A6):- mpred_pa_call(P,6,call(P,A1,A2,A3,A4,A5,A6)).
t(P,A1,A2,A3,A4,A5,A6):- loop_check_mpred(t(P,A1,A2,A3,A4,A5,A6)).
t(P,A1,A2,A3,A4,A5,A6,A7):- mpred_pa_call(P,7,call(P,A1,A2,A3,A4,A5,A6,A7)).
t(P,A1,A2,A3,A4,A5,A6,A7):- loop_check_mpred(t(P,A1,A2,A3,A4,A5,A6,A7)).

:- use_module(logicmoo(mpred/mpred_loader)).
:- use_module(logicmoo(mpred/mpred_pfc)).

decl_mpred_multifile(M):- trace_or_throw(depricated(decl_mpred_multifile(M))),
                 multifile(M:('<-')/2),
                    multifile(M:('::::')/2),
                 multifile(M:('<==>'/2)),
                 multifile(M:(('==>')/2)),
                 multifile(M:('nesc')/1),
                 multifile(M:('~')/1),
                 multifile(M:('neg')/1),
                 export(M:('<-')/2),
                    export(M:('::::')/2),
                 export(M:('<==>'/2)),
                 export(M:(('==>')/2)),
                 export(M:('nesc')/1),
                 export(M:('~')/1),
                 export(M:('neg')/1).

:- op(500,fx,kb:'~').
:- op(1050,xfx,(kb:'<-kb:')).
:- op(1050,xfx,kb:'<==>').
:- op(1050,xfx,(kb:'<-kb:')).
:- op(1100,fx,(kb:'nesckb:')).
:- op(1150,xfx,(kb:'::::kb:')).
:- op(500,fx,kb:'~').
:- op(1050,xfx,kb:'<==>').
:- op(1050,xfx,(kb:'<-kb:')).
:- op(1200,fx,(kb:'=>kb:')).
:- op(1200,fx,(kb:'==>kb:')).
:- op(1100,fx,(kb:'nesckb:')).
:- op(1150,xfx,(kb:'::::kb:')).
:- op(300,fx,kb:'-').
:- op(600,yfx,kb:'&').  
:- op(600,yfx,kb:'v').
:- op(1075,xfx,kb:'<-').
:- op(1075,xfx,kb:'<=').
:- op(1070,xfx,kb:'=>').
:- op(1070,xfx,kb:'<=>').
:- op(1100,xfx,(kb:'<==>kb:')).
:- op(1100,xfx,(kb:'==>kb:')).
:- op(350,xfx,kb:'xor').

kb_dynamic(F/A):-!,kb_dynamic(kb:F/A).
kb_dynamic(M:_):- context_module(CM),CM\==kb, must(M==kb),fail.
kb_dynamic(_:M:FA):-atom(M),!,kb_dynamic(FA).
kb_dynamic(M:F/A):-!, M:multifile(F/A),M:module_transparent(F/A),M:dynamic(F/A),M:export(F/A),context_module(CM),show_call(CM:import(M:F/A)),show_call(mpred_loader:import(M:F/A)).
kb_dynamic(M:(FA1,FA2)):-!,kb_dynamic(M:FA1),kb_dynamic(M:FA2).
kb_dynamic((FA1,FA2)):-!,kb_dynamic(FA1),kb_dynamic(FA2).
kb_dynamic(M:P):-functor(P,F,A),!,kb_dynamic(M:F/A).
kb_dynamic(P):-functor(P,F,A),!,kb_dynamic(F/A).


mpred_univ(C,I,Head):-atom(C),!,Head=..[C,I],predicate_property(Head,number_of_clauses(_)).

:- use_module(logicmoo(mpred/'mpred_stubs.pl')).

/*
:- use_module(logicmoo(mpred/'mpred_*.pl')).

kb:resolveConflict(C):- cwc, must((resolveConflict0(C),
  show_if_debug(is_resolved(C)),mpred_rem(conflict(C)))).
kb:resolveConflict(C) :- cwc,
  wdmsg("Halting with conflict ~p", [C]),   
  must(mpred_halt(conflict(C))),fail.
*/

resolveConflict0(C) :- cwc, forall(must(mpred_negation_w_neg(C,N)),ignore(show_call_failure((nop(kb:resolveConflict(C)),pp_why(N))))),
  ignore(show_call_failure((nop(kb:resolveConflict(C)),pp_why(C)))), 
    doall((mpred_call_shared(resolverConflict_robot(C)),\+ is_resolved(C),!)),
    is_resolved(C),!.

resolverConflict_robot(N) :- cwc, forall(must(mpred_negation_w_neg(N,C)),forall(compute_resolve(C,N,TODO),on_x_rtrace(show_if_debug(TODO)))).
resolverConflict_robot(C) :- cwc, must((mpred_remove3(C),wdmsg("Rem-3 with conflict ~p", [C]),mpred_run,sanity(\+C))).

never_assert_u(M:Rule,Why):- cwc, atom(M),never_assert_u(Rule,Why).
% never_assert_u(pt(_,Pre,Post),head_singletons(Pre,Post)):- cwc, head_singletons(Pre,Post).
never_assert_u(Rule,is_var(Rule)):- cwc, is_ftVar(Rule),!.
never_assert_u(Rule,head_singletons(Pre,Post)):- cwc, Rule \= (_:-_), once(mpred_rule_hb(Rule,Post,Pre)), head_singletons(Pre,Post).
never_assert_u(mpred_mark(pfcPosTrigger,_,F,A),static(F/A)):-functor(P,F,A),current_predicate(F,M:P),predicate_property(M:P,static).
/*
never_assert_u(pt(_,
       singleValuedInArg(A, _),
       (trace->rhs([{trace}, prologSingleValued(B)]))),singletons):- trace,A\=B,trace.
*/


%  Pred='$VAR'('Pred'),unnumbervars(mpred_eval_lhs(kbp:pt(umt,singleValuedInArg(Pred,_G8263654),(trace->rhs([{trace},prologSingleValued(Pred)]))),(singleValuedInArg(Pred,_G8263679),{trace}==>{trace},prologSingleValued(Pred),u)),UN).
%  Pred='$VAR'('Pred'),unnumbervars(mpred_eval_lhs(kbp:pt(umt,singleValuedInArg(Pred,_G8263654),(trace->rhs([{trace},prologSingleValued(Pred)]))),(singleValuedInArg(Pred,_G8263679),{trace}==>{trace},prologSingleValued(Pred),u)),UN).


arity(apathFn,2).
arity(isKappaFn,2).
arity(isInstFn,1).
arity(ftListFn,1).
arity(xyzFn,4).
arity(arity,2).
arity(is_never_type,1).
arity(argIsa, 3).
arity(Prop,1):-ttPredType(Prop).
arity(meta_argtypes,1).
arity(arity,2).
arity(is_never_type,1).
arity(prologSingleValued,1).
arity('<=>',2).
arity(F,A):- atom(F), integer(A),current_predicate(F/A),A>1.
arity(F,1):- atom(F), current_predicate(F/1),\+((dif:dif(Z,1), arity(F,Z))).


kb:current_world(current).

:- source_location(S,_),forall(source_file(H,S),(functor(H,F,A),export(F/A),module_transparent(F/A))).

