% File: /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc_utils.pl
:- module(mpred_pfc_utils, [

is_resolved/1,
compute_resolve/3,
mpred_negation_w_neg/2,
  ruleBackward/2, 
  has_cl/1,
basePFC:bt/3,basePFC:hs/1,basePFC:nt/4,basePFC:pk/4,basePFC:pt/3,basePFC:qu/3,basePFC:sm/1,basePFC:spft/5,
  basePFC:tms/1,lmconf:hook_one_minute_timer_tick/0,lmconf:module_local_init/0,lmconf:mpred_hook_rescan_files/0,lmconf:mpred_provide_storage_clauses/4,'$mode'/2,add_reprop/2,add_side_effect/2,
  ain_minfo/1,ain_minfo/2,all_different_head_vals/1,append_as_first_arg/3,assert_eq_quitely/1,assert_i/1,assert_u/1,assert_u/4,
  assertz_mu/2,assertz_u/1,attvar_op/2,bwc/0,call_i/1,call_prologsys/1,
  call_u/1,call_u/2,check_context_module/0,check_never_assert/1,check_never_retract/1,check_real_context_module/0,clause_asserted_i/1,clause_asserted_i/2,
  clause_asserted_i/3,clause_asserted_local/1,clause_i/2,clause_i/3,clause_or_call/2,clause_u/2,clause_u/3,cnstrn/1,
  cnstrn/2,contains_ftVar/1,correctify_support/2,cwc/0,each_in_list/3,erase_w_attvars/2,exact_args/1,f_to_mfa/4,
  fa_to_p/3,fix_negations/2,fixed_negations/2,fwc/0,fwd_ok/1,get_fa/3,get_source_ref/1,get_user_abox_umt/1,
  get_why/4,has_body_atom/2,has_db_clauses/1,has_functor/1,if_missing_mask/3,if_missing_mask/4,is_action_body/1,is_already_supported/3,
  is_bc_body/1,is_disabled_clause/1,is_fc_body/1,is_mpred_action/1,is_nc_as_is/1,is_relative/1,is_retract_first/1,is_side_effect_disabled/0,
  loop_check_nr/1,make_uu_remove/1,map_literals/2,map_literals/3,map_unless/4,meta_wrapper_rule/1,mpred_call_only_facts/1,mpred_call_only_facts/2,
  mpred_clause/3,mpred_clause_is_asserted/2,mpred_clause_is_asserted_hb_nonunify/2,mpred_cleanup/0,mpred_cleanup/2,mpred_conjoin/3,mpred_current_op_support/1,mpred_deep_support/2,
  mpred_each_literal/2,mpred_facts_and_universe/1,mpred_facts_only/1,mpred_freeLastArg/2,mpred_is_builtin/1,mpred_is_info/1,mpred_is_silient/0,mpred_is_taut/1,
  mpred_is_tautology/1,mpred_maptree/2,mpred_maptree/3,mpred_mark_as/4,mpred_mark_as_ml/4,mpred_mark_fa_as/6,mpred_no_chaining/1,mpred_pbody/5,
  mpred_pbody_f/5,mpred_prove_neg/1,mpred_remove_file_support/1,mpred_retry/1,mpred_rewrap_h/2,mpred_rule_hb/3,mpred_scan_tms/1,mpred_test/1,
  mpred_update_literal/4,mreq/1,neg_in_code/1,neg_may_naf/1,no_side_effects/1,nonfact_metawrapper/1,not_cond/2,pfcVerifyMissing/3,
  pfcVersion/1,pfc_provide_storage_op/2,physical_side_effect/1,pos_2_neg/2,pred_all/1,pred_head/2,pred_head_all/1,put_clause_ref/2,
  record_se/0,reduce_clause_from_fwd/2,repropagate/1,repropagate_meta_wrapper_rule/1,req/1,rescan_pfc/0,retract_eq_quitely/1,retract_eq_quitely_f/1,
  retract_i/1,retract_u/1,retractall_i/1,retractall_u/1,rewritten_metawrapper/1,set_prolog_stack_gb/1,should_call_for_facts/1,should_call_for_facts/3,
  show_if_debug/1,sub_term_eq/2,sub_term_v/2,to_addable_form/2,to_addable_form_wte/3,to_predicate_isas/2,to_predicate_isas_each/2,update_single_valued_arg/2,
  user_atom/1,w_get_fa/3,wac/0,which_missing_argnum/2,why_was_true/1,with_mpred_trace_exec/1,with_no_mpred_trace_exec/1,with_search_mode/2,
  with_umt/1,with_umt/2,(({})/1),

  ain_minfo_2/2,all_different_head_vals_2/2,cnstrn0/2,get_source_ref1/1,match_source_ref1/1,mpred_call_0/1,mpred_call_1/3,mpred_cleanup_0/1,
  mpred_deep_support0/2,mpred_file_expansion_0/2,mpred_rule_hb_0/3,pred_r0/1,pred_t0/1,pred_t0/2,pred_u0/1,pred_u1/1,pred_u2/1,
  repropagate_0/1,repropagate_1/1,repropagate_2/1,
    to_predicate_isas0/2  ]).
 :- meta_predicate % cmt :-
        neg_in_code(?),
        neg_may_naf(0),
        show_if_debug(0),
        with_mpred_trace_exec(0).

 :- meta_predicate  pred_head(1,*).
 :- meta_predicate  cnstrn0(0,*).
 :- meta_predicate  mpred_facts_only(0).
 :- meta_predicate  mpred_update_literal(*,*,0,*).
 :- meta_predicate  map_unless(1,?,*,*).
 :- meta_predicate  call_u(0).
 :- meta_predicate  with_no_mpred_trace_exec(0).
 :- meta_predicate  not_cond(*,0).
 :- meta_predicate  cnstrn(0).
 :- meta_predicate  mpred_retry(0).
 :- meta_predicate  loop_check_nr(0).
 :- meta_predicate  each_in_list(2,*,?).

% :- meta_predicate logicmoo_user:t(6,?,?,?,?,?,?).
% :- meta_predicate logicmoo_user:t(5,?,?,?,?,?).
% :- meta_predicate logicmoo_user:t(7,?,?,?,?,?,?,?).
:- meta_predicate physical_side_effect(0).
:- meta_predicate attvar_op(1,*).
:- meta_predicate cnstrn(?,0).
:- meta_predicate ain_minfo(1,*).
:- meta_predicate ain_minfo_2(1,*).


/*
:- multifile(( basePFC:bt/3,basePFC:hs/1,basePFC:nt/4,basePFC:pk/4,basePFC:pt/3,basePFC:qu/3,basePFC:sm/1,basePFC:spft/5,
  basePFC:tms/1,lmconf:mpred_hook_rescan_files/0,'$mode'/2  )).
:- (module_transparent check_context_module/0, clause_i/2, clause_i/3, neg_may_naf/1, repropagate_2/1, show_if_debug/1, with_mpred_trace_exec/1).
:- export(( ain_minfo_2/2,all_different_head_vals_2/2,cnstrn0/2,get_source_ref1/1,match_source_ref1/1,mpred_call_0/1,mpred_call_1/3,mpred_cleanup_0/1,
  mpred_deep_support0/2,mpred_file_expansion_0/2,mpred_rule_hb_0/3,pred_r0/1,pred_t0/1,pred_t0/2,pred_u0/1,pred_u1/1,
  pred_u2/1,repropagate_0/1,repropagate_1/1,repropagate_2/1,to_predicate_isas0/2  )).
:- dynamic(( basePFC:bt/3,basePFC:hs/1,basePFC:nt/4,basePFC:pk/4,basePFC:pt/3,basePFC:qu/3,basePFC:sm/1,basePFC:spft/5,
  basePFC:tms/1,lmconf:mpred_hook_rescan_files/0  )).
:- shared_multifile (( basePFC:bt/3,basePFC:hs/1,basePFC:nt/4,basePFC:pk/4,basePFC:pt/3,basePFC:qu/3,basePFC:sm/1,basePFC:spft/5,
  basePFC:tms/1,lmconf:mpred_hook_rescan_files/0  )).
*/

%  mpred_negation(N,P) is true if N is a negated term and P is the term
%  with the negation operator stripped.

%% mpred_negation( ?P, ?P) is semidet.
%
% PFC Negation.
%
% mpred_negation((-P),P).
mpred_negation((-P),P).
mpred_negation((\+(P)),P).


%% mpred_negated_literal( ?P) is semidet.
%
% PFC Negated Literal.
%
mpred_negated_literal(P):-mpred_negated_literal(P,_).

%% mpred_negated_literal( ?P, ?Q) is semidet.
%
% PFC Negated Literal.
%
mpred_negated_literal(P,Q) :- is_ftNonvar(P),
  mpred_negation(P,Q),
  mpred_literal(Q).


%% mpred_is_assertable( ?X) is semidet.
%
% PFC If Is A Assertable.
%
mpred_is_assertable(X):- mpred_literal_nv(X),\+ functor(X,{},_).

%% mpred_literal_nv( ?X) is semidet.
%
% PFC Literal Nv.
%
mpred_literal_nv(X):-is_ftNonvar(X),mpred_literal(X).

%% mpred_literal( ?X) is semidet.
%
% PFC Literal.
%
mpred_literal(X) :- is_reprop(X),!,fail.
mpred_literal(X) :- cyclic_term(X),!,fail.
mpred_literal(X) :- atom(X),!.
mpred_literal(X) :- mpred_negated_literal(X),!.
mpred_literal(X) :- mpred_positive_literal(X),!.
mpred_literal(X) :- is_ftVar(X),!.


%% is_reprop( ?X) is semidet.
%
% If Is A Reprop.
%
is_reprop(X):- compound(X),is_reprop_0(X).

%% is_reprop_0( ?X) is semidet.
%
% If Is A reprop  Primary Helper.
%
is_reprop_0(~(X)):-!,is_reprop(X).
is_reprop_0(X):-get_functor(X,repropagate,_).


%% mpred_non_neg_literal( ?X) is semidet.
%
% PFC Not Negated Literal.
%
mpred_non_neg_literal(X):-is_reprop(X),!,fail.
mpred_non_neg_literal(X):-atom(X),!.
mpred_non_neg_literal(X):- sanity(stack_check),
    mpred_positive_literal(X), X \= ~(_), X \= mpred_mark(_,_,_,_), X \= conflict(_).

mpred_non_neg_literal(X):-is_reprop(X),!,fail.

%% mpred_positive_literal( ?X) is semidet.
%
% PFC Positive Literal.
%
mpred_positive_literal(X) :- is_ftNonvar(X),
  get_functor(X,F,_),
  \+ mpred_connective(F).

:- module_transparent((check_context_module/0,clause_i/2,
clause_i/3)).

:- use_module(library(logicmoo_utils)).
:- op(500,fx,'-').
:- op(300,fx,'~').
:- op(1050,xfx,('==>')).
:- op(1050,xfx,'<==>').
:- op(1050,xfx,('<-')).
:- op(1050,xfx,('->')).
:- op(1100,fx,('==>')).
:- op(1150,xfx,('::::')).

:- op(1100,fx,('==>')).
:- op(1150,xfx,('::::')).

:- op(1050,xfx,('->')).
:- op(1090,xfx,('=>')).
:- op(1090,xfx,('&')).
:- op(1090,xfx,('v')).
:- op(1050,xfx,'<=>').

/*

?-  ensure_loaded('/home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc_utils.pl').

list_file_preds(ensure_loaded('/home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc_utils.pl')).

list_file_preds('/home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo_utils.pl').

% /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo_utils.pl:204
%
*/


%% maybeSupport( ?P, ?VALUE2) is semidet.
%
% Maybe Support.
%
maybeSupport(P,_):-mpred_ignored(P),!.
maybeSupport(P,S):-( \+ ground(P)-> true;
  (predicate_property(P,dynamic)->mpred_ain(P,S);true)).


%% mpred_ignored( :TermC) is semidet.
%
% PFC Ignored.
%
mpred_ignored(argIsa(F, A, argIsaFn(F, A))).
mpred_ignored(genls(A,A)).
mpred_ignored(isa(tCol,tCol)).
%mpred_ignored(isa(W,tCol)):-mreq(lmconf:hasInstance_dyn(tCol,W)).
mpred_ignored(isa(W,_)):-is_ftCompound(W),isa(W,pred_argtypes).
mpred_ignored(C):-clause_safe(C,true). 
mpred_ignored(isa(_,Atom)):-atom(Atom),atom_concat(ft,_,Atom),!.
mpred_ignored(isa(_,argIsaFn(_, _))).

/*
%% mpred_negation_w_neg( ?P, ?NF) is semidet.
%
% PFC Negation W Negated.
%
mpred_negation_w_neg(~(P),P):-is_ftNonvar(P),!.
mpred_negation_w_neg(P,NF):-mpred_nf1_negation(P,NF).
*/




%% compute_resolve( ?NewerP, ?OlderQ, ?SU, ?SU, ?OlderQ) is semidet.
%
% Compute Resolve.
%
compute_resolve(NewerP,OlderQ,SU,SU,(mpred_remove(OlderQ),mpred_ain(NewerP,S),mpred_unsup(conflict(NewerP)))):-
  must(correctify_support(SU,S)),
  wdmsg(compute_resolve(newer(NewerP-S)>older(OlderQ-S))).
compute_resolve(NewerP,OlderQ,S1,[U],Resolve):-compute_resolve(OlderQ,NewerP,[U2],S1,Resolve),match_source_ref1(U),match_source_ref1(U2),!.
compute_resolve(NewerP,OlderQ,SU,S2,(mpred_remove(OlderQ),mpred_ain(NewerP,S1),mpred_unsup(conflict(NewerP)))):-
  must(correctify_support(SU,S1)),
  wdmsg(compute_resolve((NewerP-S1)>(OlderQ-S2))).



%% compute_resolve( ?NewerP, ?OlderQ, ?Resolve) is semidet.
%
% Compute Resolve.
%
compute_resolve(NewerP,OlderQ,Resolve):-
   supporters_list(NewerP,S1),
   supporters_list(OlderQ,S2),
   compute_resolve(NewerP,OlderQ,S1,S2,Resolve).



%% is_resolved( ?C) is semidet.
%
% If Is A Resolved.
%
is_resolved(C):- Why= is_resolved, mpred_call_only_facts(Why,C),\+mpred_call_only_facts(Why,~(C)).
is_resolved(C):- Why= is_resolved, mpred_call_only_facts(Why,~(C)),\+mpred_call_only_facts(Why,C).


%% mpred_negation_w_neg( ?P, ?NF) is semidet.
%
% PFC Negation W Negated.
%
mpred_negation_w_neg(~(P),P):-is_ftNonvar(P),!.
mpred_negation_w_neg(P,NF):-mpred_nf1_negation(P,NF).


%% has_cl( ?H) is semidet.
%
% Has Clause.
%
has_cl(H):-predicate_property(H,number_of_clauses(_)).




%% each_in_list(+P2,+HT,+S) semidet.
%
% Call P(E,S). each Element in the list.
%
each_in_list(P,[H|T],S) :-
  % mpred_rem1 
  call(P,H,S),
  each_in_list(P,T,S).

%% check_context_module is semidet.
%
% Check Context Module.
%
check_context_module:- is_release,!.
check_context_module:- must((source_context_module(M),M\==mpred_pfc,M\==mpred_loader)).

%% check_real_context_module is semidet.
%
% Check Real Context Module.
%
check_real_context_module:- must((context_module(M),M\==mpred_pfc,M\==mpred_loader)).


:- shared_multifile(basePFC:bt/3).
:- shared_multifile(basePFC:nt/4).
:- shared_multifile(basePFC:pk/4).
:- shared_multifile(basePFC:pt/3).
:- shared_multifile(basePFC:spft/5).
:- shared_multifile(basePFC:tms/1).
:- shared_multifile(basePFC:hs/1).
:- shared_multifile(basePFC:qu/3).
:- shared_multifile(basePFC:sm/1).
%:- shared_multifile(basePFC:sm/1).
:- shared_multifile(mpred_do_and_undo_method/2).
/*
:- shared_multifile(('==>')/1).
:- shared_multifile(('::::')/2).
:- shared_multifile(('<-')/2).
:- shared_multifile(('<==>')/2).
:- shared_multifile(('==>')/2).
:- shared_multifile(('~')/1).
:- shared_multifile(('nesc')/1).
:- shared_multifile((('~'))/1).
*/
/*
:- shared_multifile((mpred_action)/1).
:- foreach(arg(_,isEach(prologMultiValued,prologOrdered,prologNegByFailure,prologPTTP,prologKIF,pfcControlled,ttPredType,
     prologHybrid,predCanHaveSingletons,prologDynamic,prologBuiltin,prologMacroHead,prologListValued,prologSingleValued),P),
   ((shared_multifile(baseKB:P/1)))).
:- shared_multifile(basePFC:hs/2).
:- shared_multifile(pfcControlled/1).
:- shared_multifile(prologDynamic/2).
:- shared_multifile(prologSideEffects/1).
:- shared_multifile(prologSingleValued/1).
:- shared_multifile(singleValuedInArg/2).
:- shared_multifile(prologSideEffects/1).

:- was_dynamic(lmconf:module_local_init/0).
:- discontiguous(lmconf:module_local_init/0).
*/
% :- include('mpred_header.pi').
:- style_check(+singleton).


%% get_user_abox_umt( ?A) is semidet.
%
% Get User Abox Ignore.
%
get_user_abox_umt(A):-!,A=umt.
get_user_abox_umt(A):-ignore(get_user_abox(A)).


% ======================= mpred_file('pfcsyntax').	% operator declarations.
:- was_module_transparent(with_umt/1).
:- was_export(with_umt/1).

%% with_umt( ?ABOX, ?G) is semidet.
%
% Using User Microtheory.
%
with_umt(ABOX,G):- w_tl(t_l:user_abox(ABOX),ABOX:call(ABOX:G)).

%% with_umt( ?G) is semidet.
%
% Using User Microtheory.
%
with_umt(G):- get_user_abox(M),!, M:call(M:G).
with_umt(Goal):- source_context_module(M) -> M:call(Goal).


with_search_mode(Mode,Goal):- w_tl(t_l:mpred_search_mode(Mode),Goal).


%   File   : pfcsyntax.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Purpose: syntactic sugar for Pfc - operator definitions and term expansions.

:-
 op(1199,fx,('==>')), 
 op(1190,xfx,('::::')),
 op(1180,xfx,('==>')),
 op(1170,xfx,'<==>'),  
 op(1160,xfx,('<-')),
 op(1150,xfx,'=>'),
 op(1140,xfx,'<='),
 op(1130,xfx,'<=>'), 
 op(600,yfx,'&'), 
 op(600,yfx,'v'),
 op(350,xfx,'xor'),
 op(300,fx,'~'),
 op(300,fx,'-').

:- op(1100,fx,(shared_multifile)).




%% mreq( ?G) is semidet.
%
% Mreq.
%
mreq(G):- if_defined_else(G,fail).


/*

LogicMOO is mixing Mark Stickel's PTTP (prolog techn theorem prover) to create horn clauses that 
 PFC forwards and helps maintain in visible states )  in prolog knowledge mpred_baseF.. We use basePFC:spft/5 to track deductions
Research~wise LogicMOO has a main purpose is to prove that grounded negations (of contrapostives) are of first class in importance in helping
with Wff checking/TMS 
Also alows an inference engine constrain search.. PFC became important since it helps memoize and close off (terminate) transitive closures

*/


%% is_side_effect_disabled is semidet.
%
% If Is A Side Effect Disabled.
%
is_side_effect_disabled:- t_l:no_physical_side_effects,!.
is_side_effect_disabled:- t_l:side_effect_ok,!,fail.
is_side_effect_disabled:- t_l:noDBaseMODs(_),!.



%% f_to_mfa( ?EF, ?R, ?F, ?A) is semidet.
%
% Functor Converted To Module-functor-arity.
%
f_to_mfa(EF,R,F,A):-w_get_fa(EF,F,A),
              (((current_predicate(F/A),functor(P,F,A),predicate_property(_M:P,imported_from(R)))*->true;
              current_predicate(F/A),functor(P,F,A),source_file(R:P,_SF))),
              current_predicate(R:F/A).


%% w_get_fa( ?PI, ?F, ?A) is semidet.
%
% W Get Functor-arity.
%
w_get_fa(PI,_F,_A):-is_ftVar(PI),!.
w_get_fa(F/A,F,A):- !.
w_get_fa(PI,PI,_A):- atomic(PI),!.
w_get_fa(PI,F,A):- is_ftCompound(PI),!,functor(PI,F,A).
w_get_fa(Mask,F,A):-get_functor(Mask,F,A).


%%  mpred_conjoin(+Conjunct1,+Conjunct2,?Conjunction) is semidet.
%
%  arg3 is a simplified expression representing the conjunction of
%  args 1 and 2.
%
mpred_conjoin(true,X,X):- !.
mpred_conjoin(X,true,X):- !.
mpred_conjoin(C1,C2,(C1,C2)).

%% set_prolog_stack_gb( ?Six) is semidet.
%
% Set Prolog Stack Gb.
%
set_prolog_stack_gb(Six):-set_prolog_stack(global, limit(Six*10**9)),set_prolog_stack(local, limit(Six*10**9)),set_prolog_stack(trail, limit(Six*10**9)).

%% module_local_init is semidet.
%
% Hook To [lmconf:module_local_init/0] For Module Mpred_pfc.
% Module Local Init.
%
lmconf:module_local_init:-set_prolog_stack_gb(16).
:- shared_multifile(lmconf:mpred_hook_rescan_files/0).
:- was_dynamic(lmconf:mpred_hook_rescan_files/0).
:- was_dynamic(use_presently/0).
% used to annotate a predciate to indicate PFC support
:- shared_multifile(infoF/1).
:- was_dynamic(infoF/1).
:- was_export(infoF/1).

% :- set_prolog_flag(access_level,system).


%% is_mpred_action( :TermP) is semidet.
%
% If Is A Managed Predicate Action.
%
is_mpred_action('$VAR'(_)):-!,fail.
is_mpred_action(remove_if_unsupported(_,_)).
is_mpred_action(P):-predicate_property(P,static).

%% mpred_is_builtin( ?P) is semidet.
%
% PFC If Is A Builtin.
%
mpred_is_builtin(P):-predicate_property(P,built_in).

/* UNUSED TODAY

:- use_module(library(mavis)).
:- use_module(library(type_check)).
:- use_module(library(typedef)).
*/

:- use_module(library(lists)).
:- meta_predicate with_mpred_trace_exec(0).

% :- use_module(library(dra/tabling3/swi_toplevel)).

:- discontiguous(mpred_file_expansion_0/2).
/*
compiled(F/A):- was_dynamic(F/A),compile_predicates([F/A]).
:- compiled(('nesc')/1).
:- compiled(('~')/1).
:- compiled(('<-')/2).
:- compiled(('==>')/2).
:- compiled(('::::')/2).
:- compiled(('<==>')/2).
*/

:- thread_local((t_l:use_side_effect_buffer , t_l:verify_side_effect_buffer)).

%% record_se is semidet.
%
% Record Se.
%
record_se:- (t_l:use_side_effect_buffer ; t_l:verify_side_effect_buffer).



%% add_side_effect( ?Op, ?Data) is semidet.
%
% Add Side Effect.
%
add_side_effect(_,_):- ( \+  record_se ),!.
add_side_effect(Op,Data):-current_why(Why),assert(t_l:side_effect_buffer(Op,Data,Why)).











%= 	 	 

%% mpred_retry( ?G) is semidet.
%
% Managed Predicate Retry.
%
mpred_retry(G):- fail;G.











%% attvar_op( +:PRED1, ?Data) is semidet.
%
% Attribute Variable Oper..
%
attvar_op(Op,Data):- strip_module(Op,_,OpA), sanity((atom(OpA))),
   add_side_effect(Op,Data),   
   unnumbervars_and_save(Data,Data0),
   all_different_head_vals(Data0),
   clausify_attributes(Data0,DataA),
   (==(Data0,DataA)->
     physical_side_effect(call(Op,DataA));

   ((atom_concat(asse,_,OpA) -> physical_side_effect(call(Op,DataA)));
   ((
    % nop((expand_to_hb(DataA,H,B),split_attrs(B,BA,G))),
     
    physical_side_effect(call(Op,DataA))

    )))).
    


%% erase_w_attvars( ?Data0, ?Ref) is semidet.
%
% Erase W Attribute Variables.
%
erase_w_attvars(Data0,Ref):- physical_side_effect(erase(Ref)),add_side_effect(erase,Data0).

:- thread_local(t_l:no_physical_side_effects/0).

%% physical_side_effect( ?PSE) is semidet.
%
% Physical Side Effect.
%
physical_side_effect(PSE):- is_side_effect_disabled,!,mpred_warn('no_physical_side_effects ~p',PSE).
physical_side_effect(PSE):- PSE.

%% mpred_no_chaining( ?Goal) is semidet.
%
% PFC No Chaining.
%
mpred_no_chaining(Goal):- w_tl(t_l:no_physical_side_effects,call(Goal)).


% TODO ISSUE https://github.com/TeamSPoon/PrologMUD/issues/7

%% match_source_ref1( :TermARG1) is semidet.
%
% Match Source Ref Secondary Helper.
%
match_source_ref1(u):-!.
match_source_ref1(u(_)).

%% make_uu_remove( :TermU) is semidet.
%
% Make Uu Remove.
%
make_uu_remove((U,U)):-match_source_ref1(U).

% TODO ISSUE https://github.com/TeamSPoon/PrologMUD/issues/7
:- was_export(get_source_ref/1).

%% get_source_ref1( ?Mt) is semidet.
%
% Get Source Ref Secondary Helper.
%
get_source_ref1(_):- check_context_module,fail.
get_source_ref1(u):-!.
get_source_ref1(u(Mt)):-current_why(Mt),!.
get_source_ref1(u(Mt)):-Mt=mt.

%% get_source_ref( :TermU) is semidet.
%
% Get Source Ref.
%
get_source_ref((U,U)):- get_source_ref1(U).


%% has_functor( :TermC) is semidet.
%
% Has Functor.
%
has_functor(_):-!,fail.
has_functor(F/A):-!,atom(F),integer(A),!.
has_functor(C):- (\+ is_ftCompound(C)),!,fail.
has_functor(C):-is_ftCompound(C),\+is_list(C).


%% mpred_each_literal( ?P, ?E) is semidet.
%
% PFC Each Literal.
%
mpred_each_literal(P,E):-is_ftNonvar(P),P=(P1,P2),!,(mpred_each_literal(P1,E);mpred_each_literal(P2,E)).
mpred_each_literal(P,P). %:-conjuncts_to_list(P,List),member(E,List).



is_nc_as_is(P) :- \+ compound(P),!.
is_nc_as_is(P):- is_ftVar(P),!.

fixed_negations(I,O):-notrace((fix_negations(I,O),!,I\=@=O)).
fix_negations(P0,P0):- is_nc_as_is(P0),!.
fix_negations(~(P0),~(P0)):- is_nc_as_is(P0),!.
fix_negations(\+(P0),\+(P0)):- is_nc_as_is(P0),!.
fix_negations(~(~I),O):- !, fix_negations(\+(~I),O).
fix_negations(~(not(I)),O):- !, fix_negations(\+(~I),O).
fix_negations(~~(I),O):- functor(~~(I),~~,1),!, fix_negations(\+(~I),O).
fix_negations(not(I),O):- !, fix_negations(\+(I),O).
fix_negations(~(I),~(O)):- !, fix_negations(I,O).
fix_negations(\+(I),\+(O)):- !, fix_negations(I,O).
% fix_negations(C,C):-exact_args(C),!.
fix_negations([H|T],[HH|TT]):-!,fix_negations(H,HH),fix_negations(T,TT),!.
fix_negations(C,CO):-C=..[F|CL],must_maplist(fix_negations,CL,CLO),!,CO=..[F|CLO].




%% to_addable_form_wte( ?Why, :TermI, :TermO) is semidet.
%
% Converted To Addable Form Wte.
%
to_addable_form_wte(Why,I,O):-nonvar(O),!,to_addable_form_wte(Why,I,M),!,mustvv(M=O).
to_addable_form_wte(Why,I,O):-string(I),must_det_l((input_to_forms(string(I),Wff,Vs),put_variable_names(Vs),!,sexpr_sterm_to_pterm(Wff,PTerm),
  to_addable_form_wte(Why,PTerm,O))).
to_addable_form_wte(Why,I,O):-atom(I),atom_contains(I,'('),must_det_l((input_to_forms(atom(I),Wff,Vs),put_variable_names(Vs),!,sexpr_sterm_to_pterm(Wff,PTerm),
  to_addable_form_wte(Why,PTerm,O))).

to_addable_form_wte(_,X,X):-mreq(as_is_term(X)),!.
to_addable_form_wte(Why,nesc(I),O):-!,to_addable_form_wte(Why,I,O).
to_addable_form_wte(Why,USER:I,O):-USER==user,!,to_addable_form_wte(Why,I,O).
to_addable_form_wte(Why,I,O):- fixed_negations(I,M),to_addable_form_wte(Why,M,O).
to_addable_form_wte(assert,(H:-B),(H:-B)):-B\==true,!.
to_addable_form_wte(Why,(CUT0,P0),(CUT,P)):-to_addable_form_wte(Why,CUT0,CUT),!,to_addable_form_wte(Why,P0,P).
% to_addable_form_wte(Why,(CUT,P0),(CUT,P)):-mpred_is_builtin(CUT),!,to_addable_form_wte(Why,P0,P).
to_addable_form_wte(Why,P0,P):- notrace((
    once(cnotrace(to_addable_form(P0,P));must(to_addable_form(P0,P))),
    ignore((((P0\=@=P,P0\=isa(_,_)),mpred_trace_msg((to_addable_form(Why):-[P0,P]))))))),!.


%% retract_eq_quitely( ?H) is semidet.
%
% Retract Using (==/2) (or =@=/2) ) Quitely.
%
retract_eq_quitely(H):- with_umt(retract_eq_quitely_f(H)).

%% retract_eq_quitely_f( ?H) is semidet.
%
% Retract Using (==/2) (or =@=/2) ) Quitely False.
%
retract_eq_quitely_f((H:-B)):- !,clause_asserted_u(H,B,Ref),erase(Ref).
retract_eq_quitely_f(pfclog(H)):- retract_eq_quitely_f(H),fail.
retract_eq_quitely_f((H)):- clause_asserted_u(H,true,Ref),erase(Ref).


%% assert_eq_quitely( ?H) is semidet.
%
% Assert Using (==/2) (or =@=/2) ) Quitely.
%
assert_eq_quitely(H):- attvar_op(assert_if_new,H).


%% reduce_clause_from_fwd( ?H, ?H) is semidet.
%
% Reduce Clause Converted From Forward Repropigated.
%
reduce_clause_from_fwd(H,H):- (\+is_ftCompound(H)),!.
reduce_clause_from_fwd((H:-B),HH):-B==true,reduce_clause_from_fwd(H,HH).
reduce_clause_from_fwd((B==>H),HH):-B==true,reduce_clause_from_fwd(H,HH).
reduce_clause_from_fwd(I,O):- fixed_negations(I,M),reduce_clause_from_fwd(M,O).
reduce_clause_from_fwd((==>H),HH):-!,reduce_clause_from_fwd(H,HH).
reduce_clause_from_fwd((H<- B),HH):-B==true,reduce_clause_from_fwd(H,HH).
reduce_clause_from_fwd((B<==> H),HH):-B==true,reduce_clause_from_fwd(H,HH).
reduce_clause_from_fwd((H<==> B),HH):-B==true,reduce_clause_from_fwd(H,HH).
reduce_clause_from_fwd((H,B),(HH,BB)):-!,reduce_clause_from_fwd(H,HH),reduce_clause_from_fwd(B,BB).
reduce_clause_from_fwd(H,H).



%% to_addable_form( ?I, ?I) is semidet.
%
% Converted To Addable Form.
%
to_addable_form(I,I):- is_ftVar(I),!.
to_addable_form(I,OOO):-is_list(I),!,must_maplist(to_addable_form,I,O),flatten(O,OO),!,must(reduce_clause_from_fwd(OO,OOO)).

to_addable_form(I,OO):- current_predicate(_:mpred_expansion_file/0),must(fully_expand(pfc,I,II)),!,
 must((into_mpred_form(II,M),to_predicate_isas_each(M,O))),!,reduce_clause_from_fwd(O,OO).

to_addable_form(I,O):- must((bagof(M,do_expand_args(isEachAF,I,M),IM))),list_to_conjuncts(IM,M),to_predicate_isas_each(M,O),!.

:-mpred_expansion_file.
% I =((P,Q)==>(p(P),q(Q))) , findall(O,baseKB:do_expand_args(isEachAF,I,O),L).



%% to_predicate_isas_each( ?I, ?O) is semidet.
%
% Converted To Predicate Isas Each.
%
to_predicate_isas_each(I,O):-to_predicate_isas(I,O).


%% to_predicate_isas( :TermV, :TermV) is semidet.
%
% Converted To Predicate Isas.
%
to_predicate_isas(V,V):- (\+is_ftCompound(V)),!.
to_predicate_isas([H|T],[HH|TT]):-!,to_predicate_isas(H,HH),to_predicate_isas(T,TT),!.
to_predicate_isas((H,T),(HH,TT)):-!,to_predicate_isas(H,HH),to_predicate_isas(T,TT),!.
%to_predicate_isas(I,I):-contains_term(S,I),is_ftNonvar(S),exact_args(S),!.
to_predicate_isas(I,O):-must(to_predicate_isas0(I,O)),!.


%% append_as_first_arg( ?C, ?I, ?V) is semidet.
%
% Append Converted To First Argument.
%
append_as_first_arg(C,I,V):-C=..[F|ARGS],V=..[F,I|ARGS].


%% to_predicate_isas0( :TermV, :TermV) is semidet.
%
% Converted To Predicate Isas Primary Helper.
%
to_predicate_isas0(V,V):- (\+is_ftCompound(V)),!.
to_predicate_isas0({V},{V}):-!.
to_predicate_isas0(eXact(V),V):-!.
to_predicate_isas0(t(C,I),V):-atom(C)->V=..[C,I];(is_ftVar(C)->V=t(C,I);append_as_first_arg(C,I,V)).
to_predicate_isas0(isa(I,C),V):-!,atom(C)->V=..[C,I];(is_ftVar(C)->V=isa(I,C);append_as_first_arg(C,I,V)).
to_predicate_isas0(C,C):-exact_args(C),!.
to_predicate_isas0([H|T],[HH|TT]):-!,to_predicate_isas0(H,HH),to_predicate_isas0(T,TT),!.
to_predicate_isas0(C,CO):-C=..[F|CL],must_maplist(to_predicate_isas0,CL,CLO),!,CO=..[F|CLO].

:- source_location(F,_),asserta(absolute_source_location_pfc(F)).

%% exact_args( ?Q) is semidet.
%
% Exact Arguments.
%
exact_args(Q):-is_ftVar(Q),!,fail.
exact_args(argsQuoted(_)):-!,fail.
exact_args(Q):- req(argsQuoted(Q)).
exact_args(Q):-is_ftCompound(Q),functor(Q,F,_),req(argsQuoted(F)).
exact_args(second_order(_,_)).
exact_args(call(_)).
exact_args(asserted(_)).
exact_args(retract_eq_quitely(_)).
exact_args(asserts_eq_quitely(_)).
exact_args(assertz_if_new(_)).
exact_args((_:-_)).
exact_args((_ =.. _)).
exact_args((:-( _))).
exact_args((A/B)):- (is_ftVar(A);is_ftVar(B)).
exact_args(ain(_)).
exact_args(dynamic(_)).
exact_args(cwc).
exact_args(true).
% exact_args(C):-source_file(C,I),absolute_source_location_pfc(I).


%% mpred_is_tautology( ?Var) is semidet.
%
% PFC If Is A Tautology.
%
mpred_is_tautology(Var):-is_ftVar(Var).
mpred_is_tautology(V):- copy_term_nat(V,VC),numbervars(VC),show_success(mpred_is_taut(VC)).


%% mpred_is_taut( :TermA) is semidet.
%
% PFC If Is A Taut.
%
mpred_is_taut(A):-var(A),!.
mpred_is_taut(A:-B):-!,mpred_is_taut(B==>A).
mpred_is_taut(A<-B):-!,mpred_is_taut(B==>A).
mpred_is_taut(A<==>B):-!,(mpred_is_taut(A==>B);mpred_is_taut(B==>A)).
mpred_is_taut(A==>B):- A==B,!.
mpred_is_taut((B,_)==>A):- mpred_is_assertable(B),mpred_is_taut(A==>B),!.
mpred_is_taut((_,B)==>A):- mpred_is_assertable(B),mpred_is_taut(A==>B),!.
mpred_is_taut(B==>(A,_)):- mpred_is_assertable(A),mpred_is_taut(A==>B),!.
mpred_is_taut(B==>(_,A)):- mpred_is_assertable(A),mpred_is_taut(A==>B),!.


%% loop_check_nr( ?CL) is semidet.
%
% Loop Check Nr.
%
loop_check_nr(CL):- loop_check(no_repeats(CL)).

% lmconf:decl_database_hook(Op,Hook):- loop_check_nr(pfc_provide_storage_op(Op,Hook)).


%% is_retract_first( ?VALUE1) is semidet.
%
% If Is A Retract First.
%
is_retract_first(one).
is_retract_first(a).


%% pfc_provide_storage_op( ?Op, ?I1) is semidet.
%
% Prolog Forward Chaining Provide Storage Oper..
%
pfc_provide_storage_op(Op,(I1,I2)):-!,pfc_provide_storage_op(Op,I1),pfc_provide_storage_op(Op,I2).
pfc_provide_storage_op(Op,(nesc(P))):-!,pfc_provide_storage_op(Op,P).
%pfc_provide_storage_op(change(assert,_AorZ),Fact):- loop_check_nr(ainPreTermExpansion(Fact)).
% pfcRem1 to just get the first
pfc_provide_storage_op(change(retract,OneOrA),FactOrRule):- is_retract_first(OneOrA),!,
            loop_check_nr(mpred_withdraw(FactOrRule)),
  ignore((ground(FactOrRule),mpred_remove(FactOrRule))).
% mpred_remove should be forcefull enough
pfc_provide_storage_op(change(retract,all),FactOrRule):- loop_check_nr(mpred_remove(FactOrRule)),!.
% pfc_provide_storage_op(is_asserted,FactOrRule):- is_ftNonvar(FactOrRule),!,loop_check_nr(clause_u(FactOrRule)).


%% mpred_clause_is_asserted_hb_nonunify( ?H, :TermB) is semidet.
%
% PFC Clause If Is A Asserted Head+body Nonunify.
%
mpred_clause_is_asserted_hb_nonunify(H,B):- clause_true( ==>( B , H) ).
mpred_clause_is_asserted_hb_nonunify(H,B):- clause_true( <-( H , B) ).
mpred_clause_is_asserted_hb_nonunify(_,_):-!,fail.
mpred_clause_is_asserted_hb_nonunify(G, T   ):- T==true,!,hotrace(mpred_rule_hb(G,H,B)),G\=@=H,!,mpred_clause_is_asserted(H,B).
mpred_clause_is_asserted_hb_nonunify(H,(T,B)):- T==true,!,mpred_clause_is_asserted_hb_nonunify(H,B).
mpred_clause_is_asserted_hb_nonunify(H,(B,T)):- T==true,!,mpred_clause_is_asserted_hb_nonunify(H,B).
mpred_clause_is_asserted_hb_nonunify(H,B):- clause_u( <-( H , B) , true).
mpred_clause_is_asserted_hb_nonunify(H,B):- mpred_clause_is_asserted(H,B).


%% mpred_clause_is_asserted( ?H, ?B) is semidet.
%
% PFC Clause If Is A Asserted.
%
mpred_clause_is_asserted(H,B):- is_ftVar(H),is_ftNonvar(B),!,fail.
mpred_clause_is_asserted(H,B):- modulize_head(H,HH), (has_cl(HH) -> clause_u(HH,B) ; mpred_clause_is_asserted_hb_nonunify(H,B)).
%mpred_clause_is_asserted(H,B,Ref):- clause_u(H,B,Ref).


% pfcDatabaseGoal(G):-is_ftCompound(G),get_functor(G,F,A),pfcDatabaseTerm(F/A).


%% mpred_provide_storage_clauses( ?VALUE1, ?H, ?B, ?Proof) is semidet.
%
% Hook To [lmconf:mpred_provide_storage_clauses/4] For Module Mpred_pfc.
% PFC Provide Storage Clauses.
%
lmconf:mpred_provide_storage_clauses(pfc,H,B,Proof):-mpred_clause(H,B,Proof).

%mpred_clause('nesc'(H),B,forward(Proof)):- is_ftNonvar(H),!, lmconf:mpred_provide_storage_clauses(H,B,Proof).
%mpred_clause(H,B,forward(R)):- R=(==>(B,H)),clause_u(R,true).

%% mpred_clause( ?H, ?B, ?Why) is semidet.
%
% PFC Clause.
%
mpred_clause(H,B,Why):-has_cl(H),clause_u(H,CL,R),mpred_pbody(H,CL,R,B,Why).
%mpred_clause(H,B,backward(R)):- R=(<-(H,B)),clause_u(R,true).
%mpred_clause(H,B,equiv(R)):- R=(<==>(LS,RS)),clause_u(R,true),(((LS=H,RS=B));((LS=B,RS=H))).
% mpred_clause(H,true, pfcTypeFull(R,Type)):-is_ftNonvar(H),!,pfcDatabaseTerm(F/A),make_functor(R,F,A),pfcRuleOutcomeHead(R,H),clause(R,true),pfcTypeFull(R,Type),Type\=rule.
% mpred_clause(H,true, pfcTypeFull(R)):-pfcDatabaseTerm(F/A),make_functor(R,F,A),pfcTypeFull(R,Type),Type\=rule,clause(R,true),once(pfcRuleOutcomeHead(R,H)).


%% mpred_pbody( ?H, ?B, ?R, ?BIn, ?WHY) is semidet.
%
% PFC Pbody.
%
mpred_pbody(_H,mpred_bc_only(_BC),_R,fail,deduced(backchains)):-!.
mpred_pbody(H,infoF(INFO),R,B,Why):-!,mpred_pbody_f(H,INFO,R,B,Why).
mpred_pbody(H,B,R,BIn,WHY):- is_true(B),!,BIn=B,get_why(H,H,R,WHY).
mpred_pbody(H,B,R,B,asserted(R,(H:-B))).


%% get_why( ?VALUE1, ?CL, ?R, :TermR) is semidet.
%
% Get Generation Of Proof.
%
get_why(_,CL,R,asserted(R,CL)):- get_user_abox_umt(ABOX),clause_u(basePFC:spft(ABOX,CL, U, U, _Why),true),!.
get_why(H,CL,R,deduced(R,WHY)):- (mpred_get_support(H,WH)*->WHY=(H=WH);(mpred_get_support(CL,WH),WHY=(CL=WH))).



%% mpred_pbody_f( ?H, ?CL, ?R, ?B, ?WHY) is semidet.
%
% PFC Pbody False.
%
mpred_pbody_f(H,CL,R,B,WHY):- CL=(B==>HH),sub_term_eq(H,HH),!,get_why(H,CL,R,WHY).
mpred_pbody_f(H,CL,R,B,WHY):- CL=(HH<-B),sub_term_eq(H,HH),!,get_why(H,CL,R,WHY).
mpred_pbody_f(H,CL,R,B,WHY):- CL=(HH<==>B),sub_term_eq(H,HH),get_why(H,CL,R,WHY).
mpred_pbody_f(H,CL,R,B,WHY):- CL=(B<==>HH),sub_term_eq(H,HH),!,get_why(H,CL,R,WHY).
mpred_pbody_f(H,CL,R,fail,infoF(CL)):- trace_or_throw(mpred_pbody_f(H,CL,R)).


%% sub_term_eq( ?H, ?HH) is semidet.
%
% Sub Term Using (==/2) (or =@=/2) ).
%
sub_term_eq(H,HH):-H==HH,!.
sub_term_eq(H,HH):-each_subterm(HH,ST),ST==H,!.


%% sub_term_v( ?H, ?HH) is semidet.
%
% Sub Term V.
%
sub_term_v(H,HH):-H=@=HH,!.
sub_term_v(H,HH):-each_subterm(HH,ST),ST=@=H,!.

%% all_different_head_vals(+Clause) is det.
%
% Enforces All Different Head Vals.
%
all_different_head_vals(HB):- (\+ compound(HB) ; ground(HB)),!.
all_different_head_vals(HB):- 
  mpred_rule_hb(HB,H,B),
  term_slots(H,Slots),  
  (Slots==[]->
     all_different_head_vals(B);
    (lock_vars(Slots),all_different_head_vals_2(H,Slots),unlock_vars(Slots))),!.
  

all_different_head_vals_2(_H,[]):-!.
all_different_head_vals_2(H,[A,R|EST]):-arg(_,H,E1),E1 ==A,dif(A,E2),arg(_,H,E2),\+ contains_var(A,E2),all_different_vals(dif_matrix,[A,E2,R|EST]),!.
all_different_head_vals_2(_H,[A,B|C]):-all_different_vals(dif_matrix,[A,B|C]),!.
all_different_head_vals_2(HB,_):- \+ compound(HB),!.
all_different_head_vals_2(H,[A]):-arg(_,H,E1),E1 ==A, H=..[_|ARGS], all_different_vals(dif_matrix,ARGS),!.
all_different_head_vals_2(H,[A]):-arg(_,H,E1),E1 ==A,  arg(_,H,E2), A\==E2, \+ contains_var(A,E2), dif(A,E2),!.
all_different_head_vals_2(H,[A]):-arg(_,H,E1),E1\==A, compound(E1), contains_var(A,E1), all_different_head_vals_2(E1,[A]),!.
all_different_head_vals_2(_,_).
   	 

%% mpred_rule_hb( ?Outcome, ?OutcomeO, ?AnteO) is semidet.
%
% PFC Rule Head+body.
%
mpred_rule_hb(Outcome,OutcomeO,AnteO):-hotrace((mpred_rule_hb_0(Outcome,OutcomeO,Ante),mpred_rule_hb_0(Ante,AnteO,_))),!.
:-mpred_trace_nochilds(mpred_rule_hb/3).


%% mpred_rule_hb_0( ?Outcome, ?OutcomeO, ?VALUE3) is semidet.
%
% PFC rule Head+Body  Primary Helper.
%
mpred_rule_hb_0(Outcome,OutcomeO,true):-is_ftVar(Outcome),!,OutcomeO=Outcome.
mpred_rule_hb_0(Outcome,OutcomeO,true):- \+compound(Outcome),!,OutcomeO=Outcome.
mpred_rule_hb_0((Outcome1,Outcome2),OutcomeO,AnteO):-!,mpred_rule_hb(Outcome1,Outcome1O,Ante1),mpred_rule_hb(Outcome2,Outcome2O,Ante2),
                   conjoin(Outcome1O,Outcome2O,OutcomeO),
                   conjoin(Ante1,Ante2,AnteO).
mpred_rule_hb_0((Ante1==>Outcome),OutcomeO,(Ante1,Ante2)):-!,mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0((Ante1=>Outcome),OutcomeO,(Ante1,Ante2)):-!,mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0((Ante1->Outcome),OutcomeO,(Ante1,Ante2)):-!,mpred_rule_hb(Outcome,OutcomeO,Ante2).
% mpred_rule_hb_0((Outcome/Ante1),OutcomeO,(Ante1,Ante2)):-!,mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(rhs(Outcome),OutcomeO,Ante2):-!,mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0({Outcome},OutcomeO,Ante2):-!,mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0((Outcome<-Ante1),OutcomeO,(Ante1,Ante2)):-!,mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0((Ante1 & Outcome),OutcomeO,(Ante1,Ante2)):-!,mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0((Ante1 , Outcome),OutcomeO,(Ante1,Ante2)):-!,mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0((Outcome<==>Ante1),OutcomeO,(Ante1,Ante2)):-mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0((Ante1<==>Outcome),OutcomeO,(Ante1,Ante2)):-!,mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(_::::Outcome,OutcomeO,Ante2):-!,mpred_rule_hb_0(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(basePFC:bt(ABOX,Outcome,Ante1),OutcomeO,(Ante1,Ante2)):-!,get_user_abox_umt(ABOX),mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(basePFC:pt(ABOX,Ante1,Outcome),OutcomeO,(Ante1,Ante2)):-!,get_user_abox_umt(ABOX),mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(basePFC:pk(ABOX,Ante1a,Ante1b,Outcome),OutcomeO,(Ante1a,Ante1b,Ante2)):-!,get_user_abox_umt(ABOX),mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(basePFC:nt(ABOX,Ante1a,Ante1b,Outcome),OutcomeO,(Ante1a,Ante1b,Ante2)):-!,get_user_abox_umt(ABOX),mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(basePFC:spft(ABOX,Outcome,Ante1a,Ante1b,_),OutcomeO,(Ante1a,Ante1b,Ante2)):-!,get_user_abox_umt(ABOX),mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(basePFC:qu(ABOX,Outcome,_),OutcomeO,Ante2):-!,get_user_abox_umt(ABOX),mpred_rule_hb(Outcome,OutcomeO,Ante2).
% mpred_rule_hb_0(pfc Default(Outcome),OutcomeO,Ante2):-!,mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0((Outcome:-Ante),Outcome,Ante):-!.
mpred_rule_hb_0(Outcome,Outcome,true).


%% ain_minfo( ?G) is semidet.
%
% Assert If New Metainformation.
%
ain_minfo(G):-ain_minfo(assertz_if_new,G).

%% ain_minfo( :PRED1How, ?H) is semidet.
%
% Assert If New Metainformation.
%
ain_minfo(How,(H:-True)):-is_true(True),must(is_ftNonvar(H)),!,ain_minfo(How,H).
ain_minfo(How,(H<-B)):- !,ain_minfo(How,(H:-infoF(H<-B))),!,ain_minfo(How,(H:-mpred_bc_only(H))),ain_minfo_2(How,(B:-infoF(H<-B))).
ain_minfo(How,(B==>H)):- !,ain_minfo(How,(H:-infoF(B==>H))),!,ain_minfo_2(How,(B:-infoF(B==>H))).
ain_minfo(How,(B<==>H)):- !,ain_minfo(How,(H:-infoF(B<==>H))),!,ain_minfo(How,(B:-infoF(B<==>H))),!.
ain_minfo(How,((A,B):-INFOC)):-mpred_is_info(INFOC),(is_ftNonvar(A);is_ftNonvar(B)),!,ain_minfo(How,((A):-INFOC)),ain_minfo(How,((B):-INFOC)),!.
ain_minfo(How,((A;B):-INFOC)):-mpred_is_info(INFOC),(is_ftNonvar(A);is_ftNonvar(B)),!,ain_minfo(How,((A):-INFOC)),ain_minfo(How,((B):-INFOC)),!.
ain_minfo(How,(-(A):-infoF(C))):-is_ftNonvar(C),is_ftNonvar(A),!,ain_minfo(How,((A):-infoF((C)))). % attvar_op(How,(-(A):-infoF(C))).
ain_minfo(How,(~(A):-infoF(C))):-is_ftNonvar(C),is_ftNonvar(A),!,ain_minfo(How,((A):-infoF((C)))). % attvar_op(How,(-(A):-infoF(C))).
ain_minfo(How,(A:-INFOC)):-is_ftNonvar(INFOC),INFOC= mpred_bc_only(A),!,attvar_op(How,(A:-INFOC)),!.
ain_minfo(How,basePFC:bt(_ABOX,H,_)):-!,attvar_op(How,(H:-mpred_bc_only(H))).
ain_minfo(How,basePFC:nt(ABOX,H,Test,Body)):-!,attvar_op(How,(H:-fail,basePFC:nt(ABOX,H,Test,Body))).
ain_minfo(How,basePFC:pt(ABOX,H,Body)):-!,attvar_op(How,(H:-fail,basePFC:pt(ABOX,H,Body))).
ain_minfo(How,(A0:-INFOC0)):- mpred_is_info(INFOC0), copy_term_and_varnames((A0:-INFOC0),(A:-INFOC)),!,must((mpred_rewrap_h(A,AA),imploded_copyvars((AA:-INFOC),ALLINFO), attvar_op(How,(ALLINFO)))),!.
%ain_minfo(How,G):-mpred_trace_msg(skipped_add_meta_facts(How,G)).
ain_minfo(_,_).

:- was_export(ain_minfo_2/2).

%% ain_minfo_2( :PRED1How, ?G) is semidet.
%
% Assert If New Metainformation  Extended Helper.
%
ain_minfo_2(How,G):-ain_minfo(How,G).


%% mpred_is_info( :TermC) is semidet.
%
% PFC If Is A Info.
%
mpred_is_info(mpred_bc_only(C)):-is_ftNonvar(C),!.
mpred_is_info(infoF(C)):-is_ftNonvar(C),!.

%:- was_dynamic(not_not/1).

%% mpred_rewrap_h( ?A, ?A) is semidet.
%
% PFC Rewrap Head.
%
mpred_rewrap_h(A,A):-is_ftNonvar(A),\+ is_static_pred(A).
mpred_rewrap_h(A,F):- functor(A,F,_),\+ is_static_pred(F),!.
%mpred_rewrap_h(A,not_not(A)):-!.


%% contains_ftVar( ?Term) is semidet.
%
% Contains Format Type Variable.
%
contains_ftVar(Term):- sub_term(Sub,Term),compound(Sub),Sub='$VAR'(_).
% 

%% cwc is semidet.
%
% Cwc.
%
cwc:-true.

%% fwc is semidet.
%
% Fwc.
%
fwc:-true.

%% bwc is semidet.
%
% Bwc.
%
bwc:-true.

%% wac is semidet.
%
% Wac.
%
wac:-true.

%% is_fc_body( ?P) is semidet.
%
% If Is A Forward Chaining Body.
%
is_fc_body(P):-cwc, has_body_atom(fwc,P).

%% is_bc_body( ?P) is semidet.
%
% If Is A Backchaining Body.
%
is_bc_body(P):-cwc, has_body_atom(bwc,P).

%% is_action_body( ?P) is semidet.
%
% If Is A Action Body.
%
is_action_body(P):-cwc, has_body_atom(wac,P).



%% has_body_atom( ?WAC, ?P) is semidet.
%
% Has Body Atom.
%
has_body_atom(WAC,P):-cwc, call(
   WAC==P -> true ; (is_ftCompound(P),arg(1,P,E),has_body_atom(WAC,E))),!.

/*
has_body_atom(WAC,P,Rest):-cwc, call(WAC==P -> Rest = true ; (is_ftCompound(P),functor(P,F,A),is_atom_body_pfa(WAC,P,F,A,Rest))).
is_atom_body_pfa(WAC,P,F,2,Rest):-arg(1,P,E),E==WAC,arg(2,P,Rest),!.
is_atom_body_pfa(WAC,P,F,2,Rest):-arg(2,P,E),E==WAC,arg(1,P,Rest),!.
*/


:- thread_local(t_l:mpred_debug_local/0).

%% mpred_is_silient is det.
%
% If Is A Silient.
%
mpred_is_silient :- ( \+ t_l:mpred_debug_local, \+ mpred_is_tracing_exec, \+ mpred_is_tracing(_), current_prolog_flag(debug,false), is_release) ,!.


:- meta_predicate(show_if_debug(0)).
% show_if_debug(A):- !,show_call(why,A).

%% show_if_debug( :GoalA) is semidet.
%
% Show If Debug.
%
show_if_debug(A):- get_mpred_is_tracing(A) -> show_call(mpred_is_tracing,A) ; A.

% ======================= 
% user''s program''s database
% ======================= 
% assert_u(arity(prologHybrid,0)):-trace_or_throw(assert_u(arity(prologHybrid,0))).
% assert_u(X):- \+ (is_ftCompound(X)),!,asserta_u(X,X,0).


%% assert_u( ?X) is semidet.
%
% Assert For User Code.
%
assert_u(M:X):- !,functor(X,F,A),assert_u(M,X,F,A).
assert_u(X):- functor(X,F,A),assert_u(abox,X,F,A).


%% assert_u( ?M, ?X, ?F, ?VALUE4) is semidet.
%
% Assert For User Code.
%
assert_u(_M,X,F,_):- req(singleValuedInArg(F,SV)),!,must(update_single_valued_arg(X,SV)),!.
assert_u(_M,X,F,A):- req(prologSingleValued(F)),!,must(update_single_valued_arg(X,A)),!.
% assert_u(M,X,F,A):-must(isa(F,prologAssertAOrdered) -> asserta_u(M,X) ; assertz_u(M,X)).
% assert_u(M,X,F,A):-must(isa(F,prologOrdered)        -> assertz_u(M,X) ; asserta_u(M,X)).
assert_u(M,X,_,_):- assertz_mu(M,X).




%% check_never_assert( ?X) is semidet.
%
% Check Never Assert.
%
check_never_assert(X):- hotrace((( copy_term_and_varnames(X,Y),req(never_assert_u(Y,Why)),X=@=Y,trace_or_throw(never_assert_u(X,Why))))),fail.
check_never_assert(X):- hotrace(ignore(( copy_term_and_varnames(X,Y),req(never_assert_u(Y)),X=@=Y,trace_or_throw(never_assert_u(X))))).

%% check_never_retract( ?X) is semidet.
%
% Check Never Retract.
%
check_never_retract(X):- hotrace(ignore(( copy_term_and_varnames(X,Y),req(never_retract_u(Y,Why)),X=@=Y,trace_or_throw(never_retract_u(X,Why))))).



%% assertz_u( ?X) is semidet.
%
% Assertz For User Code.
%
assertz_u(M:X):-!,assertz_mu(M,X).
assertz_u(X):- assertz_mu(abox,X).


%% assertz_mu( ?M, ?X) is semidet.
%
% Assertz Module Unit.
%
assertz_mu(M,X):- correct_module(M,X,T),T\==M,!,assertz_mu(T,X).
assertz_mu(M,X):- check_never_assert(M:X), clause_asserted_u(M:X),!.
assertz_mu(M,X):- must((expire_tabled_list(M:X),show_call(attvar_op(assertz_if_new,M:X)))).



%% retract_u( :TermX) is semidet.
%
% Retract For User Code.
%
retract_u(X):- check_never_retract(X),fail.
%retract_u(~(X)):-must(is_ftNonvar(X)),!,retract_eq_quitely_f(~(X)),must((expire_tabled_list(~(X)))),must((expire_tabled_list((X)))).
%retract_u(basePFC:hs(X)):-!,retract_eq_quitely_f(basePFC:hs(X)),must((expire_tabled_list(~(X)))),must((expire_tabled_list((X)))).

retract_u(basePFC:qu(ABOX,X,Y)):-!,show_failure(why,retract_eq_quitely_f(basePFC:qu(ABOX,X,Y))),must((expire_tabled_list(~(X)))),must((expire_tabled_list((X)))).
retract_u(~(X)):-!,show_success(why,retract_eq_quitely_f(~(X))),must((expire_tabled_list(~(X)))),must((expire_tabled_list((X)))).
retract_u((X)):-!,show_success(why,retract_eq_quitely_f((X))),must((expire_tabled_list(~(X)))),must((expire_tabled_list((X)))).
retract_u(X):-show_if_debug(attvar_op(retract_eq,X)),!,must((expire_tabled_list(X))).


%% retractall_u( ?X) is semidet.
%
% Retractall For User Code.
%
retractall_u(X):-retractall(X),must((expire_tabled_list(X))).

%% clause_u( ?H, ?B) is semidet.
%
% Clause For User Code.
%
clause_u(H,B):- must(H\==true),catchv(clause_u(H,B),_,fail).

%% clause_u( ?H, ?B, ?Ref) is semidet.
%
% Clause For User Code.
%
clause_u(H,B,Ref):-must(H\==true),catchv(clause_u(H,B,Ref),_,fail).


%% mpred_update_literal( ?P, ?N, ?Q, ?R) is semidet.
%
% PFC Update Literal.
%
mpred_update_literal(P,N,Q,R):-
    arg(N,P,UPDATE),call(replace_arg(P,N,OLD,Q)),
    must(Q),update_value(OLD,UPDATE,NEW), 
    call(replace_arg(Q,N,NEW,R)).


%% update_single_valued_arg( ?P, ?N) is semidet.
%
% Update Single Valued Argument.
%
update_single_valued_arg(P,N):-
 must_det_l((
  get_user_abox_umt(ABOX),
  get_source_ref((U,U)),
  arg(N,P,UPDATE),
  replace_arg(P,N,OLD,Q),
  current_why(Why),
  get_user_abox(M), 
  M:get_source_ref1(U),
  must_det_l((
     attvar_op(assert_if_new,
     basePFC:spft(ABOX,P,U,U,Why)),
     (req(P)->true;(assertz_u(P))),
     doall((
          clause_u(Q,true,E),
          UPDATE \== OLD,
          erase_w_attvars(clause_u(Q,true,E),E),
          mpred_unfwc(Q))))))).

% ======================= 
% prolog system database
% ======================= 
/*
assert_prologsys(X):- attvar_op(assert,X).
asserta_prologsys(X):- attvar_op(asserta,X).
assertz_prologsys(X):-attvar_op(assertz,X).

clause_prologsys(H,B):-clause_u(H,B).
clause_prologsys(H,B,Ref):-clause_u(H,B,Ref).
*/

%% call_prologsys( ?X) is semidet.
%
% Call Prologsys.
%
call_prologsys(X):-with_umt(X).

% ======================= 
% internal bookkeeping
% ======================= 

%% assert_u( ?X) is semidet.
%
% Assert For Internal Interface.
%
assert_u(X):- check_never_assert(X), attvar_op(assert_if_new,X).

%% asserta_u( ?X) is semidet.
%
% Asserta For Internal Interface.
%
asserta_u(X):- check_never_assert(X), attvar_op(asserta_if_new,X).

%% assertz_u( ?X) is semidet.
%
% Assertz For Internal Interface.
%
assertz_u(X):- check_never_assert(X), attvar_op(assertz_if_new,X).

%% retract_u( ?X) is semidet.
%
% Retract For Internal Interface.
%
retract_u(X):- check_never_retract(X),attvar_op(retract,X).

%% clause_u( ?H, ?B) is semidet.
%
% Clause For Internal Interface.
%
clause_u(H,B):- clause_u(H,B,_).

%% clause_u( ?H, ?B, ?Ref) is semidet.
%
% Clause For Internal Interface.
%
clause_u(H,B,Ref):- clause(H,AB,Ref), (must(split_attrs(AB,A,B0)->A),B=B0),term_attvars(H:AB,Vs),maplist(put_clause_ref(Ref),Vs).


%= 	 	 

%% put_clause_ref( ?Ref, ?V) is semidet.
%
% Put Clause Ref.
%
put_clause_ref(Ref,V):- !, nop(put_clause_ref(Ref,V)).
put_clause_ref(Ref,V):-put_attr(V,cref,Ref).
 

%= 	 	 

%% clause_asserted_u( ?HB) is semidet.
%
% Clause Asserted For Internal Interface.
%
clause_asserted_u(HB):- expand_to_hb(HB,H,B),clause_asserted_u(H,B,_).

%= 	 	 

%% clause_asserted_u( ?H, ?B) is semidet.
%
% Clause Asserted For Internal Interface.
%
clause_asserted_u(H,B):- clause_asserted_u(H,B,_).
% clause_asserted_u(H00,B000,Ref):- unnumbervars((H00:B000),(H:B0)), split_attrs(B0,_A,B),!,clause_u(H,B,Ref), (clause_u(HH,BB,Ref),HH=@=H,BB=@=B,A).

%= 	 	 

%% clause_asserted_u( ?H00, ?B000, ?Ref) is semidet.
%
% Clause Asserted For Internal Interface.
%
clause_asserted_u(H00,B000,Ref):- unnumbervars((H00:B000),(H:B0)), split_attrs(B0,A,B),!, clause_u(H,B,Ref), (clause_u(HH,BB,Ref),HH=@=H,BB=@=B,A).

% clause_asserted_u(H,B,Ref):- clause_u(H,B,Ref), (clause_u(HH,BB,Ref),HH=@=H,BB=@=B).


%% call_u( ?X) is semidet.
%
% Call For Internal Interface.
%
call_u(X):-call(X).

%% retractall_u( ?X) is semidet.
%
% Retractall For Internal Interface.
%
retractall_u(X):-attvar_op(retractall,X).


mpred_remove_file_support(File):- 
  forall(filematch(File,Match),
      forall(basePFC:spft(ABOX, W, U, U, Match),forall(retract_u(basePFC:spft(ABOX, W, U, U, Match)),mpred_remove(W)))).


% ======================= 
% utils
% ======================= 

%% map_literals( ?P, ?G) is semidet.
%
% Map Literals.
%
map_literals(P,G):-map_literals(P,G,[]).


%% map_literals( ?VALUE1, :TermH, ?VALUE3) is semidet.
%
% Map Literals.
%
map_literals(_,H,_):-is_ftVar(H),!. % skip over it
map_literals(_,[],_) :- !.
map_literals(Pred,(H,T),S):-!, apply(Pred,[H|S]), map_literals(Pred,T,S).
map_literals(Pred,[H|T],S):-!, apply(Pred,[H|S]), map_literals(Pred,T,S).
map_literals(Pred,H,S):- mpred_literal(H),must(apply(Pred,[H|S])),!.
map_literals(_Pred,H,_S):- \+ is_ftCompound(H),!. % skip over it
map_literals(Pred,H,S):-H=..List,!,map_literals(Pred,List,S),!.



%% map_unless( :PRED1Test, ?Pred, ?H, ?S) is semidet.
%
% Map Unless.
%
map_unless(Test,Pred,H,S):- call(Test,H),ignore(apply(Pred,[H|S])),!.
map_unless(_Test,_,[],_) :- !.
map_unless(_Test,_Pred,H,_S):- \+ is_ftCompound(H),!. % skip over it
map_unless(Test,Pred,(H,T),S):-!, apply(Pred,[H|S]), map_unless(Test,Pred,T,S).
map_unless(Test,Pred,[H|T],S):-!, apply(Pred,[H|S]), map_unless(Test,Pred,T,S).
map_unless(Test,Pred,H,S):-H=..List,!,map_unless(Test,Pred,List,S),!.


%% mpred_maptree( ?Pred, ?List) is semidet.
%
% PFC Maptree.
%
mpred_maptree(Pred,List):-mpred_maptree(Pred,List,[]).

%% mpred_maptree( ?Pred, :TermH, ?S) is semidet.
%
% PFC Maptree.
%
mpred_maptree(Pred,H,S):-is_ftVar(H),!,apply(Pred,[H|S]).
mpred_maptree(_,[],_) :- !.
mpred_maptree(Pred,(H,T),S):-!, mpred_maptree(Pred,H,S), mpred_maptree(Pred,T,S).
mpred_maptree(Pred,(H;T),S):-!, mpred_maptree(Pred,H,S) ; mpred_maptree(Pred,T,S).
mpred_maptree(Pred,[H|T],S):-!, apply(Pred,[H|S]), mpred_maptree(Pred,T,S).
mpred_maptree(Pred,H,S):-apply(Pred,[H|S]). 

% % :- use_module(logicmoo(util/rec_lambda)).

%example pfcVerifyMissing(mpred_isa(I,D), mpred_isa(I,C), ((mpred_isa(I,C), {D==C});-mpred_isa(I,C))). 
%example pfcVerifyMissing(mudColor(I,D), mudColor(I,C), ((mudColor(I,C), {D==C});-mudColor(I,C))). 


%% pfcVerifyMissing( ?GC, ?GO, ?GO) is semidet.
%
% Prolog Forward Chaining Verify Missing.
%
pfcVerifyMissing(GC, GO, ((GO, {D==C});\+ GO) ):-  GC=..[F,A|Args],append(Left,[D],Args),append(Left,[C],NewArgs),GO=..[F,A|NewArgs],!.

%example mpred_freeLastArg(mpred_isa(I,C),~(mpred_isa(I,C))):-is_ftNonvar(C),!.
%example mpred_freeLastArg(mpred_isa(I,C),(mpred_isa(I,F),C\=F)):-!.

%% mpred_freeLastArg( ?G, ?GG) is semidet.
%
% PFC Free Last Argument.
%
mpred_freeLastArg(G,GG):- G=..[F,A|Args],append(Left,[_],Args),append(Left,[_],NewArgs),GG=..[F,A|NewArgs],!.
mpred_freeLastArg(_G,false).


%% mpred_current_op_support( ?VALUE1) is semidet.
%
% PFC Current Oper. Support.
%
mpred_current_op_support((p,p)):-!.



:- must(nop(_)).


%% mpred_prove_neg( ?G) is semidet.
%
% PFC Prove Negated.
%
mpred_prove_neg(G):-nop(trace), \+ mpred_bc_only(G), \+ mpred_fact(G).


%% pred_head( :PRED1Type, ?P) is semidet.
%
% Predicate Head.
%
pred_head(Type,P):- no_repeats_u(P,(call(Type,P),\+ nonfact_metawrapper(P),is_ftCompound(P))).


%% pred_head_all( ?P) is semidet.
%
% Predicate Head All.
%
pred_head_all(P):- pred_head(pred_all,P).


%% nonfact_metawrapper( :TermP) is semidet.
%
% Nonfact Metawrapper.
%
nonfact_metawrapper(~(_)).
nonfact_metawrapper(basePFC:pt(_,_,_)).
nonfact_metawrapper(basePFC:bt(_,_,_)).
nonfact_metawrapper(basePFC:nt(_,_,_,_)).
nonfact_metawrapper(basePFC:spft(_,_,_,_,_)).
nonfact_metawrapper(added(_)).
% we use the arity 1 forms is why 
nonfact_metawrapper(term_expansion(_,_)).
nonfact_metawrapper(P):- \+ current_predicate(_,P).
nonfact_metawrapper(P):- get_functor(P,F,_), 
   (a(prologSideEffects,F);a(tNotForUnboundPredicates,F)).
nonfact_metawrapper(P):-rewritten_metawrapper(P).


%% rewritten_metawrapper( ?C) is semidet.
%
% Rewritten Metawrapper.
%
rewritten_metawrapper(_):-!,fail.
%rewritten_metawrapper(isa(_,_)).
rewritten_metawrapper(C):-is_ftCompound(C),functor(C,t,_).


%% meta_wrapper_rule( :TermARG1) is semidet.
%
% Meta Wrapper Rule.
%
meta_wrapper_rule((_<-_)).
meta_wrapper_rule((_<==>_)).
meta_wrapper_rule((_==>_)).
meta_wrapper_rule((_:-_)).



%% pred_all( ?P) is semidet.
%
% Predicate All.
%
pred_all(P):-pred_u0(P).
pred_all(P):-pred_t0(P).
pred_all(P):-pred_r0(P).


%% pred_u0( ?P) is semidet.
%
% Predicate For User Code Primary Helper.
%
pred_u0(P):-pred_u1(P),has_db_clauses(P).
pred_u0(P):-pred_u2(P).

%% pred_u1( ?VALUE1) is semidet.
%
% Predicate For User Code Secondary Helper.
%
pred_u1(P):-a(pfcControlled,F),arity(F,A),functor(P,F,A).
pred_u1(P):-a(prologHybrid,F),arity(F,A),functor(P,F,A).
pred_u1(P):-a(prologDynamic,F),arity(F,A),functor(P,F,A).

%% pred_u2( ?P) is semidet.
%
% Predicate For User Code Extended Helper.
%
pred_u2(P):-support_hilog(F,A),functor(P,F,A),has_db_clauses(P).
pred_u2(P):-clause_true(arity(F,A)),functor(P,F,A),has_db_clauses(P).



%% has_db_clauses( ?PI) is semidet.
%
% Has Database Clauses.
%
has_db_clauses(PI):-modulize_head(PI,P),predicate_property(P,number_of_clauses(NC)),\+ predicate_property(P,number_of_rules(NC)), \+ \+ clause_u(P,true).


%% pred_t0( ?P) is semidet.
%
% Predicate True Stucture Primary Helper.
%
pred_t0(P):- get_user_abox_umt(ABOX),pred_t0(ABOX,P).
pred_t0(P):- mreq('nesc'(P)).


%% pred_t0( ?ABOX, ?P) is semidet.
%
% Predicate True Stucture Primary Helper.
%
pred_t0(ABOX,P):-mreq(basePFC:pt(ABOX,P,_)).
pred_t0(ABOX,P):-mreq(basePFC:bt(ABOX,P,_)).
pred_t0(ABOX,P):-mreq(basePFC:nt(ABOX,P,_,_)).
pred_t0(ABOX,P):-mreq(basePFC:spft(ABOX,P,_,_,_)).

%pred_r0(-(P)):- req(-(P)).
%pred_r0(~(P)):- mreq(~(P)).


%% pred_r0( :TermP) is semidet.
%
% Predicate R Primary Helper.
%
pred_r0(P==>Q):- mreq(P==>Q).
pred_r0(P<==>Q):- mreq(P<==>Q).
pred_r0(P<-Q):- mreq(P<-Q).


%% cnstrn( ?X) is semidet.
%
% Cnstrn.
%
cnstrn(X):-term_variables(X,Vs),maplist(cnstrn0(X),Vs),!.

%% cnstrn( ?V, ?X) is semidet.
%
% Cnstrn.
%
cnstrn(V,X):-cnstrn0(X,V).

%% cnstrn0( ?X, ?V) is semidet.
%
% Cnstrn Primary Helper.
%
cnstrn0(X,V):-when(is_ftNonvar(V),X).


%% rescan_pfc is semidet.
%
% Rescan Prolog Forward Chaining.
%
rescan_pfc:-forall(clause(lmconf:mpred_hook_rescan_files,Body),show_entry(rescan_pfc,Body)).


%% mpred_facts_and_universe( ?P) is semidet.
%
% PFC Facts And Universe.
%
mpred_facts_and_universe(P):- (is_ftVar(P)->pred_head_all(P);true),req(P). % (meta_wrapper_rule(P)->req(P) ; req(P)).

%add_reprop(_,_):-!.

%% add_reprop( ?Trig, :TermVar) is semidet.
%
% Add Reprop.
%
add_reprop(_Trig,Var):- is_ftVar(Var), !. % trace_or_throw(var_add_reprop(Trig,Var)).
add_reprop(_Trig,~(Var)):- is_ftVar(Var),!.
% CREATES ERROR!!!  add_reprop(_Trig,~(_Var)):-!.
add_reprop(_Trig,~(repropagate(Var))):- \+ is_ftVar(Var),!.
add_reprop(_Trig,repropagate(~(Var))):- \+ is_ftVar(Var),!.
add_reprop(_Trig,repropagate(Var)):- \+ is_ftVar(Var),!.
% add_reprop(_Trig,_):-!.
add_reprop(Trig,(H:-B)):- trace_or_throw(bad_add_reprop(Trig,(H:-B))).

% instant 
add_reprop(Trig ,Trigger):- fail, !, w_tl(t_l:current_why_source(Trig),  repropagate(Trigger)).

% settings
add_reprop( Trig ,Trigger):- fail,
  w_tl(t_l:current_why_source(Trig),
    (
     mpred_fwd(repropagate(Trigger),Trig))),!.

% delayed
add_reprop( Trig ,Trigger):- 
  w_tl(t_l:current_why_source(Trig),
    (get_user_abox_umt(ABOX),
     show_call(attvar_op(assertz_if_new,(basePFC:qu(ABOX,repropagate(Trigger),(Trig,g))))))).


%% repropagate( :TermP) is semidet.
%
% Repropagate.
%
repropagate(_):-  check_context_module,fail.
%repropagate(P):-  check_real_context_module,fail.

repropagate(P):-  is_ftVar(P),!.
repropagate(P):-  meta_wrapper_rule(P),!,with_umt(repropagate_meta_wrapper_rule(P)).
repropagate(P):-  \+ predicate_property(P,_),'$find_predicate'(P,PP),PP\=[],!,forall(member(M:F/A,PP),
                                                          must((functor(Q,F,A),repropagate_1(M:Q)))).
repropagate(F/A):- atom(F),integer(A),!,functor(P,F,A),!,repropagate(P).
repropagate(F/A):- atom(F),is_ftVar(A),!,repropagate(F).

repropagate(P):-  \+ predicate_property(_:P,_),dmsg(undefined_repropagate(P)),dumpST,dtrace,!,fail.
repropagate(P):-  repropagate_0(P).


%% repropagate_0( ?P) is semidet.
%
% repropagate  Primary Helper.
%
repropagate_0(P):- loop_check(with_umt(repropagate_1(P)),true).

:- thread_local t_l:is_repropagating/1.


%% repropagate_1( ?P) is semidet.
%
% repropagate  Secondary Helper.
%
repropagate_1(P):- is_ftVar(P),!.
repropagate_1(USER:P):- USER==user,!,repropagate_1(P).
%repropagate_1((P/_)):-!,repropagate_1(P).

repropagate_1(P):- with_umt(repropagate_2(P)).

:- export(repropagate_2/1).
:- module_transparent(repropagate_2/1).

%% repropagate_2( ?P) is semidet.
%
% repropagate  Extended Helper.
%
repropagate_2(P):-
 doall((no_repeats((mpred_facts_and_universe(P))),
    w_tl(t_l:is_repropagating(P),ignore((once(show_failure(fwd_ok(P))),show_call(mpred_fwd(P))))))).

% repropagate_meta_wrapper_rule(P==>_):- !, repropagate(P).

%% repropagate_meta_wrapper_rule( ?P) is semidet.
%
% Repropagate Meta Wrapper Rule.
%
repropagate_meta_wrapper_rule(P):-repropagate_1(P).


%% fwd_ok( :TermP) is semidet.
%
% Forward Repropigated Ok.
%
fwd_ok(_):-!.
fwd_ok(P):-ground(P),!.
fwd_ok(if_missing(_,_)).
fwd_ok(idForTest(_,_)).
fwd_ok(clif(_)).
fwd_ok(pfclog(_)).
fwd_ok(X):-compound(X),arg(_,X,E),compound(E),functor(E,(:-),_),!.
% fwd_ok(P):-must(ground(P)),!.


%% mpred_facts_only( ?P) is semidet.
%
% PFC Facts Only.
%
mpred_facts_only(P):- (is_ftVar(P)->(pred_head_all(P),\+ meta_wrapper_rule(P));true),no_repeats(P).

:- thread_local(t_l:in_rescan_mpred_hook/0).

%% mpred_hook_rescan_files is semidet.
%
% Hook To [lmconf:mpred_hook_rescan_files/0] For Module Mpred_pfc.
% PFC Hook Rescan Files.
%
lmconf:mpred_hook_rescan_files:- forall(mpred_facts_and_universe(P),w_tl(t_l:in_rescan_mpred_hook,mpred_fwd(P))).
lmconf:mpred_hook_rescan_files:- forall(mpred_facts_and_universe(P),w_tl(t_l:in_rescan_mpred_hook,mpred_scan_tms(P))).
/*
lmconf:mpred_hook_rescan_files:- forall(pred_head(pred_u0,P), 
                          forall(no_repeats(P,call(P)),
                            show_if_debug(mpred_fwd(P)))).
*/


:- retractall(t_l:mpred_debug_local).
:- retractall(mpred_is_tracing_exec).
:- retractall(mpred_is_tracing(_)).

:- logicmoo_util_shared_dynamic:asserta_if_new((ereq(G):- !, req(G))).
:- ignore((logicmoo_util_shared_dynamic:retract((ereq(G):- find_and_call(G))),fail)).
% :- logicmoo_util_shared_dynamic:listing(ereq/1).

:- export(mpred_mark_as_ml/4).

%% mpred_mark_as_ml( ?Sup, ?PosNeg, ?Type, ?P) is semidet.
%
% PFC Mark Converted To Ml.
%
mpred_mark_as_ml(Sup,PosNeg,Type,P):- mpred_mark_as(Sup,PosNeg,P,Type).


%% pos_2_neg( ?P, ?P) is semidet.
%
% pos  Extended Helper Negated.
%
pos_2_neg(p,n):-!.
pos_2_neg(n,p):-!.
pos_2_neg(P,~(P)).


%% mpred_mark_as( ?VALUE1, ?VALUE2, :TermP, ?VALUE4) is semidet.
%
% PFC Mark Converted To.
%
mpred_mark_as(_,_,P,_):- is_ftVar(P),!.
mpred_mark_as(Sup,Pos,\+(P),Type):- pos_2_neg(Pos,Neg),!,mpred_mark_as(Sup,Neg,P,Type).
mpred_mark_as(Sup,Pos,~(P),Type):- pos_2_neg(Pos,Neg),!,mpred_mark_as(Sup,Neg,P,Type).
mpred_mark_as(Sup,Pos,-(P),Type):- pos_2_neg(Pos,Neg),!,mpred_mark_as(Sup,Neg,P,Type).
mpred_mark_as(Sup,Pos,not(P),Type):- pos_2_neg(Pos,Neg),!,mpred_mark_as(Sup,Neg,P,Type).
mpred_mark_as(Sup,PosNeg,[P|PL],Type):- is_list([P|PL]), !,must_maplist(mpred_mark_as_ml(Sup,PosNeg,Type),[P|PL]).
mpred_mark_as(Sup,PosNeg,( P / CC ),Type):- !, mpred_mark_as(Sup,PosNeg,P,Type),mpred_mark_as(Sup,PosNeg,( CC ),pfcCallCode).
mpred_mark_as(Sup,PosNeg,'{}'(  CC ), _Type):- mpred_mark_as(Sup,PosNeg,( CC ),pfcCallCode).
mpred_mark_as(Sup,PosNeg,( A , B), Type):- !, mpred_mark_as(Sup,PosNeg,A, Type),mpred_mark_as(Sup,PosNeg,B, Type).
mpred_mark_as(Sup,PosNeg,( A ; B), Type):- !, mpred_mark_as(Sup,PosNeg,A, Type),mpred_mark_as(Sup,PosNeg,B, Type).
mpred_mark_as(Sup,PosNeg,( A ==> B), Type):- !, mpred_mark_as(Sup,PosNeg,A, Type),mpred_mark_as(Sup,PosNeg,B, pfcRHS).
mpred_mark_as(Sup,PosNeg,( B <- A), Type):- !, mpred_mark_as(Sup,PosNeg,A, Type),mpred_mark_as(Sup,PosNeg,B, pfcRHS).
%mpred_mark_as(_Sup,_PosNeg,( _ :- _ ),_Type):-!.
mpred_mark_as(Sup,PosNeg,( P :- CC ),Type):- !, mpred_mark_as(Sup,PosNeg,P,Type),mpred_mark_as(Sup,PosNeg,( CC ),pfcCallCode).
mpred_mark_as(Sup,PosNeg,P,Type):-get_functor(P,F,A),ignore(mpred_mark_fa_as(Sup,PosNeg,P,F,A,Type)),!.

:- was_dynamic( mpred_mark/4).

% mpred_mark_fa_as(_,_,_,'\=',2,_):- trace.

%% mpred_mark_fa_as( ?Sup, ?PosNeg, ?P, ?F, ?A, ?Type) is semidet.
%
% PFC Mark Functor-arity Converted To.
%
mpred_mark_fa_as(_Sup,_PosNeg,_P,isa,_,_):- !.
mpred_mark_fa_as(_Sup,_PosNeg,_P,t,_,_):- !.
mpred_mark_fa_as(_Sup,_PosNeg,_P,argIsa,N,_):- !,must(N=3).
mpred_mark_fa_as(_Sup,_PosNeg,_P,arity,N,_):- !,must(N=2).
mpred_mark_fa_as(_Sup,_PosNeg,_P,mpred_mark,N,_):- !,must(N=4).
mpred_mark_fa_as(_Sup,_PosNeg,_P,mpred_isa,N,_):- must(N=2).
mpred_mark_fa_as(_Sup,_PosNeg,_P,'[|]',N,_):- trace,must(N=2).
mpred_mark_fa_as(_Sup,_PosNeg,_P,_:mpred_isa,N,_):- must(N=2).
mpred_mark_fa_as(_Sup, PosNeg,_P,F,A,Type):- req(mpred_mark(Type,PosNeg,F,A)),!.
mpred_mark_fa_as(Sup,PosNeg,_P,F,A,Type):- 
  MARK = mpred_mark(Type,PosNeg,F,A),
  check_never_assert(MARK),
  with_no_mpred_trace_exec(mpred_ain(MARK,(s(Sup),g))).
  % with_no_mpred_trace_exec(with_search_mode(direct,mpred_fwd2(MARK,(s(Sup),g)))),!.
   

%% fa_to_p( ?F, ?A, ?P) is semidet.
%
% Functor-arity Converted To Pred.
%
fa_to_p(F,A,P):-integer(A),atom(F),functor(P,F,A),( P \= call_u(_) ),( P \= '$VAR'(_)).


%% hook_one_minute_timer_tick is semidet.
%
% Hook To [lmconf:hook_one_minute_timer_tick/0] For Module Mpred_pfc.
% Hook One Minute Timer Tick.
%
lmconf:hook_one_minute_timer_tick:-mpred_cleanup.


%% mpred_cleanup is semidet.
%
% PFC Cleanup.
%
mpred_cleanup:- forall((no_repeats(F-A,(mpred_mark(pfcRHS,_,F,A),A>1))),mpred_cleanup(F,A)).


%% mpred_cleanup( ?F, ?A) is semidet.
%
% PFC Cleanup.
%
mpred_cleanup(F,A):-functor(P,F,A),predicate_property(P,dynamic)->mpred_cleanup_0(P);true.


%% mpred_cleanup_0( ?P) is semidet.
%
% PFC cleanup  Primary Helper.
%
mpred_cleanup_0(P):- findall(P-B-Ref,clause(P,B,Ref),L),
  forall(member(P-B-Ref,L),erase_w_attvars(clause(P,B,Ref),Ref)),forall(member(P-B-Ref,L),attvar_op(assertz_if_new,((P:-B)))).

% :-debug.
%isInstFn(A):-!,trace_or_throw(isInstFn(A)).




%% get_fa( ?PI, ?F, ?A) is semidet.
%
% Get Functor-arity.
%
get_fa(PI,_F,_A):-is_ftVar(PI),!.
get_fa(F/A,F,A):- !.
get_fa(PI,PI,_A):- atomic(PI),!.
get_fa(PI,F,A):- is_ftCompound(PI),!,functor(PI,F,A).
get_fa(Mask,F,A):-get_functor(Mask,F,A).



%% clause_or_call( ?H, ?B) is semidet. 
%
% Clause Or Call.
%
clause_or_call(M:H,B):-is_ftVar(M),!,no_repeats(M:F/A,(f_to_mfa(H,M,F,A))),M:clause_or_call(H,B).
clause_or_call(isa(I,C),true):-!,req(isa_asserted(I,C)).
clause_or_call(genls(I,C),true):-!,on_x_log_throw(req(genls(I,C))).
clause_or_call(H,B):- clause(src_edit(_Before,H),B).
clause_or_call(H,B):- predicate_property(H,number_of_clauses(C)),predicate_property(H,number_of_rules(R)),((R*2<C) -> (clause_u(H,B)*->!;fail) ; clause_u(H,B)).
clause_or_call(H,true):- req(should_call_for_facts(H)),no_repeats(on_x_log_throw(H)).


% as opposed to simply using clause(H,true).

%% should_call_for_facts( ?H) is semidet.
%
% Should Call For Facts.
%
should_call_for_facts(H):- get_functor(H,F,A),with_umt(should_call_for_facts(H,F,A)).

%% should_call_for_facts( ?VALUE1, ?F, ?VALUE3) is semidet.
%
% Should Call For Facts.
%
should_call_for_facts(_,F,_):- a(prologSideEffects,F),!,fail.
should_call_for_facts(H,_,_):- modulize_head(H,HH), \+ predicate_property(HH,number_of_clauses(_)),!.
should_call_for_facts(_,F,A):- clause_true(mpred_mark(pfcRHS,_,F,A)),!,fail.
should_call_for_facts(_,F,A):- clause_true(mpred_mark(pfcMustFC,_,F,A)),!,fail.
should_call_for_facts(_,F,_):- a(prologDynamic,F),!.
should_call_for_facts(_,F,_):- \+ a(pfcControlled,F),!.



%% no_side_effects( ?P) is semidet.
%
% No Side Effects.
%
no_side_effects(P):-  (\+ is_side_effect_disabled->true;(get_functor(P,F,_),a(prologSideEffects,F))).


%% is_disabled_clause( ?C) is semidet.
%
% If Is A Disabled Clause.
%
is_disabled_clause(C):-req(is_edited_clause(C,_,New)),memberchk((disabled),New).


%% with_mpred_trace_exec( ?P) is semidet.
%
% Using Managed Predicate  Trace exec.
%
with_mpred_trace_exec(P):- w_tl(t_l:mpred_debug_local,w_tl(mpred_is_tracing_exec, must(show_if_debug(P)))).

%% with_mpred_trace_exec( ?P) is semidet.
%
% Without Trace exec.
%
with_no_mpred_trace_exec(P):- wno_tl(t_l:mpred_debug_local,wno_tl(mpred_is_tracing_exec, must(show_if_debug(P)))).


%% mpred_test( ?P) is semidet.
%
% PFC Test.
%
mpred_test(_):- (compiling; current_prolog_flag(xref,true)),!.
mpred_test(P):- mpred_is_silient,!,sanity(req(P)),!.
mpred_test(P):- (show_call(with_mpred_trace_exec(req(P))) -> why_was_true(P) ; ((why_was_true( \+( P )),!,fail))).

why_was_true(P):- mpred_why(P),!.
why_was_true(P):- dmsg(justfied_true(P)),!.



%% clause_asserted_local( :TermABOX) is semidet.
%
% Clause Asserted Local.
%
clause_asserted_local(CL):- must(CL=basePFC:spft(ABOX,P,Fact,Trigger,UOldWhy)),!,
  clause_u(basePFC:spft(ABOX,P,Fact,Trigger,_OldWhy),true,Ref),
  clause_u(basePFC:spft(ABOX,UP,UFact,UTrigger,UOldWhy),true,Ref),
  (((UP=@=P,UFact=@=Fact,UTrigger=@=Trigger))).



%% is_already_supported( ?P, ?S, ?UU) is semidet.
%
% If Is A Already Supported.
%
is_already_supported(P,(S,T),(S,T)):- get_user_abox_umt(ABOX),clause_asserted_local(basePFC:spft(ABOX,P,S,T,_)),!.
is_already_supported(P,_S,UU):- get_user_abox_umt(ABOX),clause_asserted_local(basePFC:spft(ABOX,P,US,UT,_)),must(get_source_ref(UU)),UU=(US,UT).

% TOO UNSAFE 
% is_already_supported(P,_S):- copy_term_and_varnames(P,PC),sp ftY(PC,_,_),P=@=PC,!.



%% if_missing_mask( ?Q, ?R, ?Test) is semidet.
%
% If Missing Mask.
%
if_missing_mask(Q,R,Test):-
   which_missing_argnum(Q,N),
   if_missing_mask(Q,N,R,Test).


%% if_missing_mask( ?Q, ?N, ?R, ?Test) is semidet.
%
% If Missing Mask.
%
if_missing_mask(Q,N,R,Test):-
  arg(N,Q,Was),
  (nonvar(R)-> (which_missing_argnum(R,RN),arg(RN,R,NEW));replace_arg(Q,N,NEW,R)),!,
   Test=dif:dif(Was,NEW).

/*
Old version
if_missing_mask(Q,N,R,dif:dif(Was,NEW)):- 
 must((is_ftNonvar(Q),acyclic_term(Q),acyclic_term(R),functor(Q,F,A),functor(R,F,A))),
  (singleValuedInArg(F,N) -> 
    (arg(N,Q,Was),replace_arg(Q,N,NEW,R));
    ((arg(N,Q,Was),is_ftNonvar(Was)) -> replace_arg(Q,N,NEW,R);
        (N=A,arg(N,Q,Was),replace_arg(Q,N,NEW,R)))).
*/


%% which_missing_argnum( ?VALUE1, ?VALUE2) is semidet.
%
% Which Missing Argnum.
%
which_missing_argnum(Q,N):-
 must((acyclic_term(Q),is_ftCompound(Q),get_functor(Q,F,A))),
 F\=t,
  (singleValuedInArg(F,N) -> true;
    ((arg(N,Q,Was),is_ftNonvar(Was)) -> true; N=A)).


%=
%= call_u(Why,F) is true iff F is a fact is true
%=

%% call_u( ?X) is semidet.
%
% Call For User Code.
%
call_u(X):- mpred_call_only_facts(X).

%% call_u( ?Why, ?X) is semidet.
%
% Call For User Code.
%
call_u(Why,X):- show_call(why,(nop(Why),mpred_call_only_facts(X))).

%=
%= not_cond(Why,F) is true iff F is a fact is not true
%=
% not_cond(_Why,X):- show_success(why,mpred_call_0(~(X))).

%% not_cond( ?Why, ?X) is semidet.
%
% Not Cond.
%
not_cond(_Why,X):- \+ X.



%% is_relative( :TermV) is semidet.
%
% If Is A Relative.
%
is_relative(V):- (\+is_ftCompound(V)),!,fail.
is_relative(update(_)).
is_relative(replace(_)).
is_relative(rel(_)).
is_relative(+(X)):- \+ is_ftVar(X).
is_relative(-(X)):- \+ is_ftVar(X).
is_relative(*(X)):- \+ is_ftVar(X).



%% { ?G} is semidet.
%
% {}.
%
'{}'(G):-req(G).

:- meta_predicate neg_in_code(*).
:- export(neg_in_code/1).

%% neg_in_code( ?G) is semidet.
%
% Negated In Code.
%
neg_in_code(G):-var(G),!,fail.
neg_in_code(req(G)):- !,~G.
neg_in_code(~(G)):- nonvar(G),!, \+ ~G.
neg_in_code(G):-   neg_may_naf(G), \+ with_umt(G).
neg_in_code(G):-  is_ftNonvar(G), prologSingleValued(G),must((if_missing_mask(G,R,Test),nonvar(R))),req(R),with_umt(Test).


:- meta_predicate neg_may_naf(0).
:- export(neg_may_naf/1).

%% neg_may_naf( :GoalP) is semidet.
%
% Negated May Negation-by-faliure.
%
neg_may_naf(P):- mpred_non_neg_literal(P),get_functor(P,F),clause_u(prologNegByFailure(F),true),!.
neg_may_naf(P):- is_ftCompound(P),predicate_property(P,static).

%=
%= mpred_call_only_facts(+Why,:F) is true iff F is a fact available for forward chaining.
%= Note that this has the side effect [maybe] of catching unsupported facts and
%= assigning them support from God. (g,g)
%=

%% req( ?G) is semidet.
%
% Req.
%
req(G):- loop_check(mpred_call_0(G),fail).


%% mpred_call_only_facts( ?Clause) is semidet.
%
% PFC Call Only Facts.
%
mpred_call_only_facts(Clause) :-  strip_module(Clause,_,F), on_x_rtrace(no_repeats(loop_check(mpred_call_0(F),fail))). 

%% mpred_call_only_facts( ?Why, ?F) is semidet.
%
% PFC Call Only Facts.
%
mpred_call_only_facts(_Why,F):- on_x_rtrace(no_repeats(loop_check(mpred_call_0(F),fail))). 




%% mpred_call_0( ?Var) is semidet.
%
% PFC call  Primary Helper.
%
mpred_call_0(Var):-is_ftVar(Var),!,mpred_call_with_no_triggers(Var).
mpred_call_0(M):-fixed_negations(M,O),!,mpred_call_0(O).
mpred_call_0(U:X):-U==user,!,mpred_call_0(X).
mpred_call_0(t(A,B)):-(atom(A)->true;(no_repeats(arity(A,1)),atom(A))),ABC=..[A,B],mpred_call_0(ABC).
mpred_call_0(isa(B,A)):-(atom(A)->true;(no_repeats(tCol(A)),atom(A))),ABC=..[A,B],mpred_call_0(ABC).
%mpred_call_0(t(A,B)):-!,(atom(A)->true;(no_repeats(arity(A,1)),atom(A))),ABC=..[A,B],mpred_call_0(ABC).
mpred_call_0(t(A,B,C)):-!,(atom(A)->true;(no_repeats(arity(A,2)),atom(A))),ABC=..[A,B,C],mpred_call_0(ABC).
mpred_call_0(t(A,B,C,D)):-!,(atom(A)->true;(no_repeats(arity(A,3)),atom(A))),ABC=..[A,B,C,D],mpred_call_0(ABC).
mpred_call_0(t(A,B,C,D,E)):-!,(atom(A)->true;(no_repeats(arity(A,4)),atom(A))),ABC=..[A,B,C,D,E],mpred_call_0(ABC).
mpred_call_0((C1,C2)):-!,mpred_call_0(C1),mpred_call_0(C2).
mpred_call_0((C1;C2)):-!,(mpred_call_0(C1);mpred_call_0(C2)).
mpred_call_0((C1->C2;C3)):-!,(mpred_call_0(C1)->mpred_call_0(C2);mpred_call_0(C3)).
mpred_call_0((C1*->C2;C3)):-!,(mpred_call_0(C1)*->mpred_call_0(C2);mpred_call_0(C3)).
mpred_call_0((C1->C2)):-!,(mpred_call_0(C1)->mpred_call_0(C2)).
mpred_call_0((C1*->C2)):-!,(mpred_call_0(C1)*->mpred_call_0(C2)).
mpred_call_0(call(X)):- !, mpred_call_0(X).
mpred_call_0(req(X)):- !, mpred_call_0(X).
mpred_call_0(\+(X)):- !, \+ mpred_call_0(X).
mpred_call_0(call_u(X)):- !, mpred_call_0(X).
mpred_call_0(asserta(X)):- !, aina(X).
mpred_call_0(assertz(X)):- !, ainz(X).
mpred_call_0(assert(X)):- !, ain(X).
mpred_call_0(retract(X)):- !, mpred_remove(X).

mpred_call_0(M:P):-!,sanity(nonvar(P)),functor(P,F,_),mpred_call_1(M,P,F).
mpred_call_0(G):- strip_module(G,M,P),sanity(nonvar(P)),functor(P,F,_),mpred_call_1(M,P,F).



%% mpred_call_1( ?VALUE1, ?G, ?VALUE3) is semidet.
%
% PFC call  Secondary Helper.
%
mpred_call_1(_,G,_):- is_side_effect_disabled,!,mpred_call_with_no_triggers(G).

mpred_call_1(M,G,F):- sanity(\+  is_side_effect_disabled),
               (ground(G); \+ current_predicate(_,M:G) ; \+ (predicate_property(M:G,number_of_clauses(CC)),CC>1)), 
    
                ignore((loop_check(call_with_bc_triggers(M:G)),maybeSupport(G,(g,g)),fail)),
                 \+ current_predicate(F,M:G),\+ current_predicate(_,_:G),
                 doall(show_call(predicate_property(_UM:G,_PP))),
                 debug(mpred),
                 must(show_call(kb_dynamic(M:G))),import_to_user(M:G),!,fail.
mpred_call_1(_,G,_):- mpred_call_with_no_triggers(G).



%% pfcVersion( ?VALUE1) is semidet.
%
% Prolog Forward Chaining Version.
%
pfcVersion(6.6).



%% correctify_support( ?S, ?S) is semidet.
%
% Correctify Support.
%
correctify_support((S,T),(S,T)):-!.
correctify_support(U,(U,U)):-atom(U),!.
correctify_support([U],S):-correctify_support(U,S).

:- thread_local t_l:infBackChainPrevented/1.



%% mpred_scan_tms( ?P) is semidet.
%
% PFC Scan Truth Maintainence/wff.
%
mpred_scan_tms(P):-mpred_get_support(P,(S,SS)),
  (S==SS-> true;
   once((mpred_deep_support(_How,P)->true;
     (mpred_trace_msg(warn(now_maybe_unsupported(mpred_get_support(P,(S,SS)),fail))))))).


%% user_atom( ?U) is semidet.
%
% User Atom.
%
user_atom(U):-match_source_ref1(U).
user_atom(g).
user_atom(m).
user_atom(d).


%% mpred_deep_support( ?How, ?M) is semidet.
%
% PFC Deep Support.
%
mpred_deep_support(_How,unbound):-!,fail.
mpred_deep_support(How,M):-loop_check(mpred_deep_support0(How,M),fail).


%% mpred_deep_support0( ?U, ?U) is semidet.
%
% PFC Deep Support Primary Helper.
%
mpred_deep_support0(user_atom(U),(U,U)):-user_atom(U),!.
mpred_deep_support0(How,(A==>_)):-!,mpred_deep_support(How,A).
mpred_deep_support0(basePFC:pt(ABOX,HowA,HowB),basePFC:pt(ABOX,A,B)):-!,mpred_deep_support(HowA,A),mpred_deep_support(HowB,B).
mpred_deep_support0(HowA->HowB,(A->B)):-!,mpred_deep_support(HowA,A),mpred_deep_support(HowB,B).
mpred_deep_support0(HowA/HowB,(A/B)):-!,mpred_deep_support(HowA,A),mpred_deep_support(HowB,B).
mpred_deep_support0((HowA,HowB),(A,B)):-!,mpred_deep_support(HowA,A),mpred_deep_support(HowB,B).
mpred_deep_support0(How,rhs(P)):-!,maplist(mpred_deep_support,How,P).
mpred_deep_support0(mpred_call_only_facts(\+ P),\+ call_u(P)):-!,mpred_call_only_facts(\+ P).
mpred_deep_support0(mpred_call_only_facts(P),call_u(P)):-!,mpred_call_only_facts(P).
mpred_deep_support0(mpred_call_only_facts(P),{P}):-!,mpred_call_only_facts(P).
mpred_deep_support0(S==>How,P):-mpred_get_support(P,S),mpred_deep_support(How,S),!.
mpred_deep_support0(mpred_call_only_facts(\+(P)),\+(P)):-!, mpred_call_only_facts(\+(P)).
mpred_deep_support0(user_atom(P),P):-user_atom(P),!.
mpred_deep_support0(mpred_call_only_facts((P)),P):-mpred_call_only_facts(P).


%% call_with_bc_triggers( ?MP) is semidet.
%
% Call Using Backchaining Triggers.
%
call_with_bc_triggers(MP) :- strip_module(MP,_,P), functor(P,F,A), \+ t_l:infBackChainPrevented(F/A), 
  mpred_get_trigger_quick(ABOX,basePFC:bt(ABOX,P,Trigger)),
  no_repeats(mpred_get_support(basePFC:bt(ABOX,P,Trigger),S)),
  once(no_side_effects(P)),
  w_tl(t_l:infBackChainPrevented(F/A),mpred_eval_lhs(Trigger,S)).


%% mpred_call_with_no_triggers( ?Clause) is semidet.
%
% PFC Call Using No Triggers.
%
mpred_call_with_no_triggers(Clause) :-  strip_module(Clause,_,F),
  %  this (is_ftVar(F)) is probably not advisable due to extreme inefficiency.
  (is_ftVar(F)    ->  mpred_facts_and_universe(F) ; mpred_call_with_no_triggers_bound(F)).


%% mpred_call_with_no_triggers_bound( ?F) is semidet.
%
% PFC Call Using No Triggers Bound.
%
mpred_call_with_no_triggers_bound(F):- mpred_call_with_no_triggers_uncaugth(F).

%% mpred_call_with_no_triggers_uncaugth( ?Clause) is semidet.
%
% PFC Call Using No Triggers Uncaugth.
%
mpred_call_with_no_triggers_uncaugth(Clause) :-  strip_module(Clause,_,F),
  show_failure(mpred_call_with_no_triggers_bound,no_side_effects(F)),
  (\+ current_predicate(_,F) -> fail;call_prologsys(F)).
  %  we check for system predicates as well.
  %has_cl(F) -> (clause_u(F,Condition),(Condition==true->true;call_u(Condition)));
  %call_prologsys(F).


%% mpred_bc_only( ?M) is semidet.
%
% PFC Backchaining Only.
%
mpred_bc_only(M:G):- with_umt(M,mpred_bc_only0(G)).

%% mpred_bc_only0( ?G) is semidet.
%
% PFC Backchaining Only Primary Helper.
%
mpred_bc_only0(G):- mpred_negation(G,Pos),!, show_call(why,\+ mpred_bc_only(Pos)).
mpred_bc_only0(G):- loop_check(no_repeats(pfcBC_NoFacts(G))).
mpred_bc_only0(G):- mpred_call_only_facts(G).

%%
%  pfcBC_NoFacts(F) is true iff F is a fact available for backward chaining ONLY.
%  Note that this has the side effect of catching unsupported facts and
%  assigning them support from God.
%  this Predicate should hide Facts from mpred_bc_only/1
%%

%% pfcBC_NoFacts( ?F) is semidet.
%
% Prolog Forward Chaining Backtackable Class No Facts.
%
pfcBC_NoFacts(F):- pfcBC_NoFacts_TRY(F)*-> true ; (mpred_slow_search,pfcBC_Cache(F)).


%% mpred_slow_search is semidet.
%
% PFC Slow Search.
%
mpred_slow_search.



%% ruleBackward( ?R, ?Condition) is semidet.
%
% Rule Backward.
%
ruleBackward(R,Condition):- with_umt(( ruleBackward0(R,Condition),functor(Condition,F,_),\+ arg(_,v(call_prologsys,call_u),F))).
%ruleBackward0(F,Condition):-clause_u(F,Condition),\+ (is_true(Condition);mpred_is_info(Condition)).

%% ruleBackward0( ?F, ?Condition) is semidet.
%
% Rule Backward Primary Helper.
%
ruleBackward0(F,Condition):- with_umt((  '<-'(F,Condition),\+ (is_true(Condition);mpred_is_info(Condition)) )).

%:- was_dynamic('{}'/1).
%{X}:-dmsg(legacy({X})),call_prologsys(X).



%% pfcBC_NoFacts_TRY( ?F) is semidet.
%
% Prolog Forward Chaining Backtackable Class No Facts Try.
%
pfcBC_NoFacts_TRY(F) :- no_repeats(ruleBackward(F,Condition)),
  % neck(F),
  call_prologsys(Condition),
  maybeSupport(F,(g,g)).



%% pfcBC_Cache( ?F) is semidet.
%
% Prolog Forward Chaining Backtackable Class Cache.
%
pfcBC_Cache(F) :- mpred_call_only_facts(pfcBC_Cache,F),
   ignore((ground(F),( (\+is_asserted_1(F)), maybeSupport(F,(g,g))))).


