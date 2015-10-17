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
never_assert_u(Rule,head_singletons(Pre,Post)):- cwc, once(mpred_rule_hb(Rule,Post,Pre)), head_singletons(Pre,Post).
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


end_of_file.

was_was:skip(convert_side_effect_0a/2, kb).
was_was:skip(convert_side_effect_0b/2, kb).
was_was:skip(convert_side_effect_0c/2, kb).
was_was:skip(is_mpred_file0/1, kb).
was_was:skip(load_file_term_to_command_0c/2, kb).
was_was:skip(load_file_term_to_command_1/3, kb).
was_was:skip(load_file_term_to_command_1b/3, kb).
was_was:skip(load_file_term_to_command_2/3, kb).
was_was:skip(mpred_process_input_1/1, kb).
was_was:skip(must_mpred_term_expansion_2/2, kb).
was_was:skip(pl_to_mpred_syntax0/2, kb).
was_was:skip(process_this_script0/1, kb).
was_was:skip(prolog_load_file_loop_checked_0/2, kb).
was_was:skip(prolog_load_file_nlc_0/2, kb).
was_was:skip(transform_opers_0/2, kb).
was_was:skip(transform_opers_1/2, kb).
was_was:skip(xfile_module_term_expansion_pass_3/7, kb).
was_was:skip(etrace/0, kb).
was_was:skip(onEndOfFile/1, kb).
was_was:skip(current_context_module/1, kb).
was_was:skip(module_typed_term_expand/2, kb).
was_was:skip(register_module_type/1, kb).
was_was:skip(end_module_type/1, kb).
was_was:skip(wlmuser/1, kb).
was_was:skip(infoF/1, kb).
was_was:skip(get_source_ref/1, kb).
was_was:skip(mpred_add_minfo_2/2, kb).
was_was:skip(with_no_mpred_expansions/1, kb).
was_was:skip(with_mpred_expansions/1, kb).
was_was:skip(ensure_loaded_no_mpreds/1, kb).
was_was:skip(end_module_type/2, kb).
was_was:skip(declare_load_dbase/1, kb).
was_was:skip(load_mpred_files/0, kb).
was_was:skip(load_mpred_on_file_end/2, kb).
was_was:skip(add/1, kb).
was_was:skip(clr/1, kb).
was_was:skip(ireq/1, kb).
was_was:skip(del/1, kb).
was_was:skip(padd/2, kb).
was_was:skip(padd/3, kb).
was_was:skip(prop/3, kb).
was_was:skip(prop_or/4, kb).
was_was:skip(call_props/2, kb).
was_was:skip(iprops/2, kb).
was_was:skip(upprop/2, kb).
was_was:skip(mreq/1, kb).
was_was:skip(upprop/1, kb).
was_was:skip(req/1, kb).
was_was:skip(world_clear/1, kb).
was_was:skip(with_kb_assertions/2, kb).
was_was:skip(forall_setof/2, kb).
was_was:skip(add_fast/1, kb).
was_was:skip(into_plist/2, kb).
was_was:skip(into_plist_arities/4, kb).
was_was:skip(inverse_args/2, kb).
was_was:skip(same_vars/2, kb).
was_was:skip(rescan_mpred_loaded/0, kb).
was_was:skip(rescan_mpred_loaded_pass2/0, kb).
was_was:skip(agenda_slow_op_restart/0, kb).
was_was:skip(agenda_rescan_mpred_ops/0, kb).
was_was:skip(agenda_slow_op_todo/1, kb).
was_was:skip(do_all_of/1, kb).
was_was:skip(add_later/1, kb).
was_was:skip(agenda_mpred_repropigate/0, kb).
was_was:skip(rescan_duplicated_facts/0, kb).
was_was:skip(rerun_database_hooks/0, kb).
was_was:skip(gather_fact_heads/2, kb).
was_was:skip(onLoad/1, kb).
was_was:skip(onEachLoad/1, lmconf).
was_was:skip(show_all/1, kb).
was_was:skip(alt_calls/1, kb).
was_was:skip(fully_expand/2, kb).
was_was:skip(db_quf/4, kb).
was_was:skip(expanded_different/2, kb).
was_was:skip(into_functor_form/3, kb).
was_was:skip(into_mpred_form/2, kb).
was_was:skip(do_expand_args/3, kb).
was_was:skip(simply_functors/3, kb).
was_was:skip(oncely/1, kb).
was_was:skip(deducedSimply/1, kb).
was_was:skip(whenAnd/2, kb).
was_was:skip(naf/1, kb).
was_was:skip(is_callable/1, kb).
was_was:skip(decl_mpred_mfa/3, kb).
was_was:skip(decl_mpred_prolog/1, kb).
was_was:skip(decl_mpred_prolog/3, kb).
was_was:skip(decl_mpred_prolog/4, kb).
was_was:skip(decl_mpred_hybrid/1, kb).
was_was:skip(decl_mpred_hybrid/3, kb).
was_was:skip(decl_mpred_hybrid/4, kb).
was_was:skip(listprolog/0, kb).
was_was:skip(decl_mpred/1, kb).
was_was:skip(decl_mpred/2, kb).
was_was:skip(glean_pred_props_maybe/1, kb).
was_was:skip(last_arg_ground/1, kb).
was_was:skip(rescan_missing_stubs/0, kb).
was_was:skip(agenda_rescan_mpred_props/0, kb).
was_was:skip(registerCycPredPlus2/1, kb).
was_was:skip(coerce/4, kb).
was_was:skip(argIsa_known/3, kb).
was_was:skip(argIsa_call_0/3, kb).
was_was:skip(correctArgsIsa/2, kb).
was_was:skip(correctArgsIsa/3, kb).
was_was:skip(correctArgsIsa/4, kb).
was_was:skip(correctAnyType/4, kb).
was_was:skip(correctFormatType/4, kb).
was_was:skip(checkAnyType/4, kb).
was_was:skip(any_to_value/2, kb).
was_was:skip(any_to_number/2, kb).
was_was:skip(atom_to_value/2, kb).
was_was:skip(samef/2, kb).
was_was:skip(same/2, kb).
was_was:skip(arg_to_var/3, kb).
was_was:skip(same_arg/3, kb).
was_was:skip(domain/2, kb).
was_was:skip(extend_domain/2, kb).
was_was:skip(extend_dom/2, kb).
was_was:skip(init_dom/2, kb).
was_was:skip(isac/2, kb).
was_was:skip(is_typef/1, kb).
was_was:skip(is_never_type/1, kb).
was_was:skip(was_isa/3, kb).
was_was:skip(asserted_subclass/2, kb).
was_was:skip(transitive_subclass_or_same/2, kb).
was_was:skip(is_known_trew/1, kb).
was_was:skip(is_known_false0/1, kb).
was_was:skip(has_free_args/1, kb).
was_was:skip(not_mud_isa/3, kb).
was_was:skip(isa_backchaing/2, kb).
was_was:skip(isa_backchaing_0/2, kb).
was_was:skip(type_isa/2, kb).
was_was:skip(decl_type_safe/1, kb).
was_was:skip(decl_type/1, kb).
was_was:skip(define_ft/1, kb).
was_was:skip(assert_subclass/2, kb).
was_was:skip(assert_p_safe/3, kb).
was_was:skip(assert_subclass_safe/2, kb).
was_was:skip(assert_isa_safe/2, kb).
was_was:skip(assert_isa_ilc/2, kb).
was_was:skip(create_meta/4, kb).
was_was:skip(i_name_lc/2, kb).
was_was:skip(i_name/2, kb).
was_was:skip(i_name/3, kb).
was_was:skip(typename_to_iname0/3, kb).
was_was:skip(split_name_type/3, kb).
was_was:skip(is_svo_functor/1, kb).
was_was:skip(hilog_functor/1, kb).
was_was:skip(is_holds_true_not_hilog/1, kb).
was_was:skip(is_holds_true/1, kb).
was_was:skip(is_2nd_order_holds/1, kb).
was_was:skip(is_holds_false/1, kb).
was_was:skip(is_log_sent/1, kb).
was_was:skip(is_log_op/1, kb).
was_was:skip(defunctionalize/2, kb).
was_was:skip(infix_op/2, kb).
was_was:skip(comparitiveOp/1, kb).
was_was:skip(additiveOp/1, kb).
was_was:skip(is_kif_rule/1, kb).
was_was:skip(term_slots/2, kb).
was_was:skip(term_singletons/2, kb).
was_was:skip(term_singletons/5, kb).
was_was:skip(fixvars/4, kb).
was_was:skip(input_to_forms/2, kb).
was_was:skip(input_to_forms/3, kb).
was_was:skip(get_input_to_forms/3, kb).
was_was:skip((//)/2, kb).
was_was:skip(to_untyped/2, kb).
was_was:skip(extract_lvars/3, kb).
was_was:skip(svar_fixvarname/2, kb).
was_was:skip(ok_varname/1, kb).
was_was:skip(kbp_t/1, kb).
was_was:skip(kb_f/1, kb).
was_was:skip(get_assertions/2, kb).
was_was:skip(kb_t/1, kb).
was_was:skip(link_to_holds/2, kb).
was_was:skip(link_to_holds_DYNAMIC/2, kb).
was_was:skip(link_to_holds_list/2, kb).
was_was:skip(el_holds_DISABLED_KB/0, kb).
was_was:skip(cyckb_t/1, kb).
was_was:skip(noGenlPreds/1, kb).
was_was:skip(kbp_t_list/1, kb).
was_was:skip(kbp_t_list/2, kb).
was_was:skip(kbp_t_list/3, kb).
was_was:skip(kb_mt/2, kb).
was_was:skip(assert_next/1, kb).
was_was:skip(move_kb_assertions_matching/4, kb).
was_was:skip(kbp_to_mpred_t/0, kb).
was_was:skip(move_implied/0, kb).
was_was:skip(hide_term_rewrites/0, kb).
was_was:skip(hide_empty_strings/0, kb).
was_was:skip(convert_easy_strings/0, kb).
was_was:skip(assertion_t/1, kb).
was_was:skip(subsT_each/3, kb).
was_was:skip(mudEquals/2, kb).
was_was:skip(skolem/3, kb).
was_was:skip(not_mudEquals/2, kb).
was_was:skip(type_of_var/3, kb).
was_was:skip(kif_to_boxlog/2, kb).
was_was:skip(kif_to_boxlog/3, kb).
was_was:skip(kif_to_boxlog/4, kb).
was_was:skip(kif/0, kb).
was_was:skip(kif_io/2, kb).
was_was:skip(why_to_id/3, kb).
was_was:skip(kif_process/1, kb).
was_was:skip(kif_ask_sent/1, kb).
was_was:skip(kif_ask/1, kb).
was_was:skip(kif_ask/2, kb).
was_was:skip(kb_incr/2, kb).
was_was:skip(logical_pos/3, kb).
was_was:skip(logical_neg/3, kb).
was_was:skip(nonegate/3, kb).
was_was:skip(correct_boxlog/4, kb).

was_was:was_was_once(registered_module_type/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_loader.pl', dynamic).
was_was:was_was_once(current_op_alias/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_loader.pl', dynamic).
was_was:was_was_once(mpred_skipped_module/1, lmconf, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_loader.pl', dynamic).
was_was:was_was_once(prolog_load_file_loop_checked/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_loader.pl', dynamic).
was_was:was_was_once(mpred_directive_value/3, lmcache, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_loader.pl', dynamic).
was_was:was_was_once(current_world/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_loader.pl', dynamic).
was_was:was_was_once(loaded_file_world_time/3, lmconf, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_loader.pl', dynamic).
was_was:was_was_once(never_reload_file/1, lmconf, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_loader.pl', dynamic).
was_was:was_was_once(always_expand_on_thread/1, mpred_loader, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_loader.pl', dynamic).
was_was:was_was_once(current_lang/1, mpred_loader, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_loader.pl', dynamic).
was_was:was_was_once(disable_mpred_term_expansions_globally/0, mpred_loader, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_loader.pl', dynamic).
was_was:was_was_once(prolog_load_file/2, user, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_loader.pl', dynamic).
was_was:was_was_once(term_expansion/2, user, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_loader.pl', dynamic).
was_was:was_was_once(into_form_code/0, t_l, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_loader.pl', shared_multifile).
was_was:was_was_once(mpred_module_expansion/1, t_l, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_loader.pl', shared_multifile).
was_was:was_was_once(term_expansion/2, user, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_loader.pl', shared_multifile).
was_was:was_was_once(goal_expansion/2, system, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_loader.pl', shared_multifile).
was_was:was_was_once(mpred_hide_msg/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', dynamic).
was_was:was_was_once(mpred_is_spying/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', dynamic).
was_was:was_was_once(mpred_is_tracing/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', dynamic).
was_was:was_was_once(mpred_warnings/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', dynamic).
was_was:was_was_once(never_assert_u/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', dynamic).
was_was:was_was_once(never_retract_u/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', dynamic).
was_was:was_was_once(mpred_mark/4, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', dynamic).
was_was:was_was_once(mpred_is_tracing_exec/0, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', dynamic).
was_was:was_was_once(use_presently/0, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', dynamic).
was_was:was_was_once((==>)/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', shared_multifile).
was_was:was_was_once(bt/3, kbp, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', shared_multifile).
was_was:was_was_once(nt/4, kbp, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', shared_multifile).
was_was:was_was_once(pk/4, kbp, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', shared_multifile).
was_was:was_was_once(pt/3, kbp, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', shared_multifile).
was_was:was_was_once(spft/5, kbp, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', shared_multifile).
was_was:was_was_once(:::: / 2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', shared_multifile).
was_was:was_was_once((<-)/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', shared_multifile).
was_was:was_was_once((<==>)/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', shared_multifile).
was_was:was_was_once((==>)/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', shared_multifile).
was_was:was_was_once(neg/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', shared_multifile).
was_was:was_was_once(nesc/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', shared_multifile).
was_was:was_was_once(mpred_action/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', shared_multifile).
was_was:was_was_once(tms/1, kbp, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', shared_multifile).
was_was:was_was_once(hs/1, kbp, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', shared_multifile).
was_was:was_was_once(qu/3, kbp, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', shared_multifile).
was_was:was_was_once(sm/1, kbp, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', shared_multifile).
was_was:was_was_once(mpred_do_and_undo_method/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', shared_multifile).
was_was:was_was_once(prologMultiValued/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', shared_multifile).
was_was:was_was_once(prologOrdered/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', shared_multifile).
was_was:was_was_once(prologNegByFailure/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', shared_multifile).
was_was:was_was_once(prologPTTP/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', shared_multifile).
was_was:was_was_once(prologKIF/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', shared_multifile).
was_was:was_was_once(pfcControlled/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', shared_multifile).
was_was:was_was_once(tPredType/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', shared_multifile).
was_was:was_was_once(prologHybrid/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', shared_multifile).
was_was:was_was_once(predCanHaveSingletons/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', shared_multifile).
was_was:was_was_once(prologDynamic/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', shared_multifile).
was_was:was_was_once(prologBuiltin/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', shared_multifile).
was_was:was_was_once(prologMacroHead/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', shared_multifile).
was_was:was_was_once(prologListValued/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', shared_multifile).
was_was:was_was_once(prologSingleValued/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', shared_multifile).
was_was:was_was_once(hs/2, kbp, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', shared_multifile).
was_was:was_was_once(prologDynamic/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', shared_multifile).
was_was:was_was_once(prologSideEffects/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', shared_multifile).
was_was:was_was_once(singleValuedInArg/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', shared_multifile).
was_was:was_was_once(module_local_init/0, lmconf, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', dynamic).
was_was:was_was_once(mpred_hook_rescan_files/0, lmconf, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', shared_multifile).
was_was:was_was_once(mpred_hook_rescan_files/0, lmconf, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', dynamic).
was_was:was_was_once(mpred_select/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', dynamic).
was_was:was_was_once(prologMacroHead/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl', dynamic).
was_was:was_was_once(prolog_load_file/2, user, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_loader.pl', shared_multifile).
was_was:was_was_once(registered_module_type/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_loader.pl', shared_multifile).
was_was:was_was_once(agent_action_queue/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(agent_session/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(agent_text_command/4, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(asserted_mpred_f/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(asserted_mpred_f/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(asserted_mpred_f/4, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(asserted_mpred_f/5, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(asserted_mpred_f/6, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(asserted_mpred_f/7, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(asserted_mpred_t/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(asserted_mpred_t/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(asserted_mpred_t/4, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(asserted_mpred_t/5, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(asserted_mpred_t/6, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(asserted_mpred_t/7, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(assertion_f/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(create_random_fact/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(deduce_facts/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(default_type_props/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(fact_always_true/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(fact_is_false/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(fact_maybe_deduced/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(fskel/7, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(grid_key/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(hooked_random_instance/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(is_edited_clause/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(loaded_external_kbs/0, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(loading_module_h/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(local_term_anglify/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(mpred_f/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(mpred_f/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(mpred_f/4, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(mpred_f/5, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(mpred_f/6, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(mpred_f/7, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(mpred_module_ready/0, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(mudKeyword/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(never_registered_mpred_file/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(now_unused/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(only_if_pttp/0, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(registered_mpred_file/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(relationMostInstance/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(session_agent/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(session_io/4, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(startup_option/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(t/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(t/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(t/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(t/4, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(t/5, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(t/6, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(t/7, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(t/8, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(t/9, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(t/10, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(t/11, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(tFarthestReachableItem/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(tNearestReachableItem/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(telnet_fmt_shown/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(term_anglify_last/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(term_anglify_np/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(term_anglify_np_last/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(tms_reject_why/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(use_cyc_database/0, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(use_kif/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(xcall_f/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(xcall_f/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(xcall_f/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(xcall_f/4, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(xcall_f/5, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(xcall_f/6, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(xcall_f/7, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(xcall_t/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(xcall_t/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(xcall_t/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(xcall_t/4, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(xcall_t/5, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(xcall_t/6, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(xcall_t/7, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', shared_multifile).
was_was:was_was_once(agent_action_queue/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(agent_session/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(agent_text_command/4, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(asserted_mpred_f/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(asserted_mpred_f/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(asserted_mpred_f/4, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(asserted_mpred_f/5, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(asserted_mpred_f/6, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(asserted_mpred_f/7, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(asserted_mpred_t/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(asserted_mpred_t/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(asserted_mpred_t/4, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(asserted_mpred_t/5, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(asserted_mpred_t/6, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(asserted_mpred_t/7, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(assertion_f/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(call_f/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(call_f/4, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(call_f/5, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(call_f/6, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(call_f/7, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(call_f/8, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(call_t/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(call_t/4, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(call_t/5, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(call_t/6, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(call_t/7, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(call_t/8, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(call_mt_t/4, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(call_mt_t/5, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(call_mt_t/6, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(call_mt_t/7, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(call_mt_t/8, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(call_mt_t/9, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(call_mt_t/10, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(call_mt_f/4, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(call_mt_f/5, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(call_mt_f/6, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(call_mt_f/7, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(call_mt_f/8, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(call_mt_f/9, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(call_mt_f/10, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(call_which_t/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(call_which_t/4, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(call_which_t/5, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(call_which_t/6, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(call_which_t/7, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(call_which_t/8, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(compute_value/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(compute_value_no_dice/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(create_random_fact/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(deduce_facts/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(default_type_props/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(fact_always_true/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(fact_is_false/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(fact_maybe_deduced/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(flatten_append/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(fskel/7, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(grid_key/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(holds_f/6, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(holds_f/7, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(holds_plist_t/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(holds_relaxed_0_t/4, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(holds_relaxed_t/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(holds_t/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(holds_t/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(holds_t/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(holds_t/4, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(holds_t/5, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(holds_t/6, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(holds_t/7, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(holds_t/8, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(holds_f/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(holds_f/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(holds_f/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(holds_f/4, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(holds_f/5, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(holds_f/8, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(hooked_random_instance/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(if_result/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(insert_into/4, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(isCycPredArity_ignoreable/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(is_edited_clause/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(list_update_op/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(loaded_external_kbs/0, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(loading_module_h/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(local_term_anglify/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(loop_check_mpred/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(mpred_f/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(mpred_f/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(mpred_f/4, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(mpred_f/5, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(mpred_f/6, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(mpred_f/7, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(mpred_fact_arity/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(mpred_module_ready/0, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(mpred_pa_call/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(mpred_plist_t/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(mudKeyword/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(never_mpred_mpred/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(never_registered_mpred_file/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(now_unused/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(only_if_pttp/0, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(registered_mpred_file/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(replace_arg/4, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(replace_nth_arglist/4, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(replace_nth_ref/5, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(telnet_fmt_shown/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(term_anglify_last/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(term_anglify_np/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(term_anglify_np_last/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(tf_result/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(tms_reject_why/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(update_value/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(use_cyc_database/0, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(use_kif/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(verb_after_arg/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(which_t/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(xcall_f/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(xcall_f/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(xcall_f/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(xcall_f/4, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(xcall_f/5, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(xcall_f/6, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(xcall_f/7, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(xcall_t/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(xcall_t/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(xcall_t/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(xcall_t/4, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(xcall_t/5, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(xcall_t/6, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(xcall_t/7, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(t/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_hooks.pl', dynamic).
was_was:was_was_once(:::: / 2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once((<-)/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once((<==>)/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once((==>)/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once((==>)/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(neg/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(nesc/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(bt/3, kbp, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(hs/1, kbp, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(hs/2, kbp, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(nt/4, kbp, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(pk/4, kbp, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(pt/3, kbp, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(qu/3, kbp, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(sm/1, kbp, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(spft/5, kbp, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(tms/1, kbp, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(mpred_module/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(add_args/15, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(addTiny_added/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(argGenl/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(argIsa/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(argQuotedIsa/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(argsQuoted/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(arity/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(call_mt_t/11, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(call_which_t/9, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(completelyAssertedCollection/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(conflict/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(constrain_args_pttp/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(contract_output_proof/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(cyc_to_plarkc/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(cycPrepending/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(decided_not_was_isa/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(defnSufficient/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(did_learn_from_name/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(f_to_mfa/4, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(formatted_resultIsa/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(genls/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(get_clause_vars_for_print/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(holds_f_p2/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(is_wrapper_pred/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(isa/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(isa_asserted_0/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(isa_pred_now_locked/0, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(isCycAvailable_known/0, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(isCycUnavailable_known/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(lambda/5, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(module_local_init/0, lmconf, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(mpred_provide_storage_clauses/3, lmconf, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(localityOfObject/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(logical_functor_pttp/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(meta_argtypes/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(mpred_action/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(mpred_do_and_undo_method/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(mpred_f/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(mpred_isa/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(mpred_to_cyc/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(mpred_univ/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(pddlSomethingIsa/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(pfcControlled/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(pfcRHS/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(pp_i2tml_now/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(pp_item_html/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(predCanHaveSingletons/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(prologBuiltin/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(prologDynamic/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(prologHybrid/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(prologKIF/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(prologListValued/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(prologMacroHead/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(prologMultiValued/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(prologNegByFailure/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(prologOrdered/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(prologPTTP/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(prologSideEffects/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(prologSingleValued/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(pttp1a_wid/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(pttp_builtin/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(pttp_nnf_pre_clean_functor/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(quasiQuote/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(relax_term/6, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(resolveConflict/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(resolverConflict_robot/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(resultIsa/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(retractall_wid/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(ruleRewrite/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(search/7, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(singleValuedInArg/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(subFormat/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(support_hilog/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(tCol/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(tFunction/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(tNotForUnboundPredicates/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(tPred/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(tRegion/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(tRelation/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(tried_guess_types_from_name/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(tSet/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(ttFormatType/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(ttPredType/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(ttTemporalType/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(ttUnverifiableType/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(typeProps/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(vtUnreifiableFunction/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_userkb.pl', dynamic).
was_was:was_was_once(doing_agenda_slow_op/0, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_agenda.pl', dynamic).
was_was:was_was_once(hook_one_minute_timer_tick/0, lmconf, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_agenda.pl', dynamic).
was_was:was_was_once(hook_one_second_timer_tick/0, lmconf, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_agenda.pl', dynamic).
was_was:was_was_once(loaded_mpred_file/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_agenda.pl', dynamic).
was_was:was_was_once(loading_mpred_file/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_agenda.pl', dynamic).
was_was:was_was_once(suspend_timers/0, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_agenda.pl', dynamic).
was_was:was_was_once(will_call_after/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_agenda.pl', dynamic).
was_was:was_was_once(call_OnEachLoad/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_agenda.pl', dynamic).
was_was:was_was_once(was_chain_rule/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_expansion.pl', shared_multifile).
was_was:was_was_once(ruleRewrite/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_expansion.pl', shared_multifile).
was_was:was_was_once(ptReformulatorDirectivePredicate/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_expansion.pl', shared_multifile).
was_was:was_was_once(props/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_expansion.pl', shared_multifile).
was_was:was_was_once(hook_mpred_listing/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_listing.pl', shared_multifile).
was_was:was_was_once(mpred_list_triggers/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_listing.pl', shared_multifile).
was_was:was_was_once(hook_mpred_listing/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_listing.pl', dynamic).
was_was:was_was_once(mpred_list_triggers/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_listing.pl', dynamic).
was_was:was_was_once(whymemory/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_listing.pl', dynamic).
was_was:was_was_once(portray/1, user, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_listing.pl', shared_multifile).
was_was:was_was_once(prolog_list_goal/1, user, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_listing.pl', shared_multifile).
was_was:was_was_once(prolog_predicate_name/2, user, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_listing.pl', shared_multifile).
was_was:was_was_once(prolog_clause_name/2, user, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_listing.pl', shared_multifile).
was_was:was_was_once(portray/1, user, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_listing.pl', dynamic).
was_was:was_was_once(prologHybrid/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_props.pl', dynamic).
was_was:was_was_once(mpred_provide_setup/4, lmconf, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_stubs.pl', shared_multifile).
was_was:was_was_once(mpred_provide_write_attributes/2, lmconf, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_stubs.pl', shared_multifile).
was_was:was_was_once(mpred_provide_storage_clauses/3, lmconf, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_stubs.pl', shared_multifile).
was_was:was_was_once(mpred_provide_setup/4, lmconf, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_stubs.pl', dynamic).
was_was:was_was_once(mpred_provide_storage_clauses/3, lmconf, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_stubs.pl', dynamic).
was_was:was_was_once(use_ideep_swi/0, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_stubs.pl', dynamic).
was_was:was_was_once(mpred_manages_unknowns/0, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_stubs.pl', shared_multifile).
was_was:was_was_once(mpred_provide_read_attributes/3, lmconf, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_stubs.pl', shared_multifile).
was_was:was_was_once(mpred_provide_storage_op/2, lmconf, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_stubs.pl', shared_multifile).
was_was:was_was_once(pfcManageHybrids/0, lmconf, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_stubs.pl', shared_multifile).
was_was:was_was_once(coerce/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_args.pl', dynamic).
was_was:was_was_once(coerce/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_args.pl', shared_multifile).
was_was:was_was_once(decided_not_was_isa/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_isa.pl', dynamic).
was_was:was_was_once(isa_asserted_0/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_isa.pl', dynamic).
was_was:was_was_once(tried_guess_types_from_name/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_isa.pl', dynamic).
was_was:was_was_once(did_learn_from_name/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_isa.pl', dynamic).
was_was:was_was_once(current_source_suffix/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_naming.pl', dynamic).
was_was:was_was_once(function_corisponding_predicate/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_wff.pl', dynamic).
was_was:was_was_once(leave_as_is_db/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_wff.pl', dynamic).
was_was:was_was_once(ttPredType/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_wff.pl', dynamic).
was_was:was_was_once(non_assertable/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_wff.pl', dynamic).
was_was:was_was_once(function_corisponding_predicate/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_wff.pl', shared_multifile).
was_was:was_was_once(el_holds/4, el_assertions, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/snark/common_logic_kb_hooks.pl', dynamic).
was_was:was_was_once(el_holds/5, el_assertions, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/snark/common_logic_kb_hooks.pl', dynamic).
was_was:was_was_once(el_holds/6, el_assertions, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/snark/common_logic_kb_hooks.pl', dynamic).
was_was:was_was_once(el_holds/7, el_assertions, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/snark/common_logic_kb_hooks.pl', dynamic).
was_was:was_was_once(el_holds/8, el_assertions, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/snark/common_logic_kb_hooks.pl', dynamic).
was_was:was_was_once(el_holds/9, el_assertions, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/snark/common_logic_kb_hooks.pl', dynamic).
was_was:was_was_once(el_holds/10, el_assertions, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/snark/common_logic_kb_hooks.pl', dynamic).
was_was:was_was_once(el_holds/11, el_assertions, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/snark/common_logic_kb_hooks.pl', dynamic).
was_was:was_was_once(el_holds/12, el_assertions, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/snark/common_logic_kb_hooks.pl', dynamic).
was_was:was_was_once(el_holds/13, el_assertions, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/snark/common_logic_kb_hooks.pl', dynamic).
was_was:was_was_once(el_holds/14, el_assertions, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/snark/common_logic_kb_hooks.pl', dynamic).
was_was:was_was_once(el_holds_pred_impl/1, el_assertions, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/snark/common_logic_kb_hooks.pl', shared_multifile).
was_was:was_was_once(is_cyckb_t_pred/2, el_assertions, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/snark/common_logic_kb_hooks.pl', shared_multifile).
was_was:was_was_once(el_holds_pred_impl/1, el_assertions, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/snark/common_logic_kb_hooks.pl', dynamic).
was_was:was_was_once(is_cyckb_t_pred/2, el_assertions, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/snark/common_logic_kb_hooks.pl', dynamic).
was_was:was_was_once(cyckb_t/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/snark/common_logic_kb_hooks.pl', dynamic).
was_was:was_was_once(assertion_f/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/snark/common_logic_kb_hooks.pl', dynamic).
was_was:was_was_once(regression_test/0, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/snark/common_logic_snark.pl', shared_multifile).
was_was:was_was_once(as_prolog/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/snark/common_logic_snark.pl', dynamic).
was_was:was_was_once(elInverse/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/snark/common_logic_snark.pl', dynamic).
was_was:was_was_once(kif_test_string/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/snark/common_logic_snark.pl', dynamic).
was_was:was_was_once(sanity_test/0, lmconf, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/snark/common_logic_snark.pl', shared_multifile).
was_was:was_was_once(agent_call_command/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/snark/common_logic_compiler.pl', shared_multifile).
was_was:was_was_once(feature_test/0, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/snark/common_logic_compiler.pl', shared_multifile).
was_was:was_was_once(mud_test/2, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/snark/common_logic_compiler.pl', shared_multifile).
was_was:was_was_once(regression_test/0, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/snark/common_logic_compiler.pl', shared_multifile).
was_was:was_was_once(type_action_info/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/snark/common_logic_compiler.pl', shared_multifile).
was_was:was_was_once(wid/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/snark/common_logic_compiler.pl', dynamic).
was_was:was_was_once(isa_pred_now_locked/0, lmconf, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/snark/common_logic_compiler.pl', shared_multifile).
was_was:was_was_once(asserted_argIsa_known/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_args.pl', dynamic).
was_was:was_was_once(decl_coerce/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_args.pl', dynamic).
was_was:was_was_once(deduceFromArgTypes/1, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_args.pl', dynamic).
was_was:was_was_once(hook_coerce/3, kb, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_args.pl', dynamic).
was_was:was_was_once(module_local_init/0, lmconf, '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_args.pl', dynamic).


