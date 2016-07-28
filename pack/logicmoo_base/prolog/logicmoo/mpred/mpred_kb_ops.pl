/* 
% File used as storage place for all predicates which change as
% the world is run.
%
% props(Obj,height(ObjHt))  == k(height,Obj,ObjHt) == rdf(Obj,height,ObjHt) == height(Obj,ObjHt)
% padd(Obj,height(ObjHt))  == padd(height,Obj,ObjHt,...) == add(QueryForm)
% kretract[all](Obj,height(ObjHt))  == kretract[all](Obj,height,ObjHt) == pretract[all](height,Obj,ObjHt) == del[all](QueryForm)
% keraseall(AnyTerm).
%
%
% Dec 13, 2035
% Douglas Miles
*/
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_kb_ops.pl
%:- if(((current_prolog_flag(xref,true),current_prolog_flag(pldoc_x,true));current_prolog_flag(autoload_logicmoo,true))).
:- module(mpred_kb_ops,
          [ deducedSimply/1,

how_to_op/2,
is_callable/1,
lookup_inverted_op/3,
mpred_op/2,
naf/1,
oncely/1,
reduce_mpred_op/2,
second_order/2,
whenAnd/2,
if_missing/1,

ain_minfo/2,
cnstrn0/2,
physical_side_effect/1,
mpred_update_literal/4,
cnstrn/1,
predicate_to_goal/2,

map_unless/4,
mpred_update_literal/4,
cnstrn/1,
mpred_facts_only/1,
pred_head/2,
ain_minfo/2,
cnstrn/2,
ain_minfo_2/2,
cnstrn0/2,
physical_side_effect/1,
predicate_to_goal/2,
mpred_kb_ops_file/0,
mpred_wfflist/2,
mpred_wff/3,


attvar_op/2,
no_side_effects/1,
mpred_non_neg_literal/1,
% defaultAssertMt/1,
retract_mu/1,
assert_mu/4,
asserta_mu/2,
asserta_mu/1,
assertz_mu/2,
assertz_mu/1,
ruleBackward0/2,
ruleBackward/2,
fwd_ok/1,
repropagate_meta_wrapper_rule/1,
repropagate_1/1,
repropagate_0/1,
repropagate/1,
mpred_facts_and_universe/1,
rescan_pfc/0,
pred_r0/1,
pred_t0/1,
has_db_clauses/1,
pred_u2/1,
pred_u1/1,
pred_u0/1,
pred_all/1,
meta_wrapper_rule/1,
rewritten_metawrapper/1,
nonfact_metawrapper/1,
pred_head_all/1,
mpred_prove_neg/1,
is_resolved/1,
compute_resolve/3,
compute_resolve/5,
no_side_effects/1,
should_call_for_facts/3,
should_call_for_facts/1,
clause_or_call/2,
is_relative/1,
mpred_non_neg_literal/1,
is_reprop_0/1,
is_reprop/1,
mpred_literal_nv/1,
mpred_is_assertable/1,
mpred_cleanup_0/1,
mpred_cleanup/2,
mpred_cleanup/0,
mpred_negation_w_neg/2,
has_cl/1,
mpred_ignored/1,
maybeSupport/2,
pfcBC_Cache/1,
pfcBC_NoFacts_TRY/1,
mpred_slow_search/0,
pfcBC_NoFacts/1,
mpred_bc_only0/1,
mpred_bc_only/1,
mpred_call_with_no_triggers_uncaugth/1,
mpred_call_with_no_triggers_bound/1,
mpred_call_with_no_triggers/1,
call_with_bc_triggers/1,
mpred_call_1/3,
mpred_call_0/1,
mpred_call_only_facts/2,
mpred_call_only_facts/1,
call_u_req/1,
neg_in_code/1,
neg_in_code0/1,
% {}/1,
trigger_supporters_list/2,
spft_precanonical/3,
mpred_get_support_precanonical/2,
support_ok_via_clause_body/3,
support_ok_via_clause_body/1,
mpred_get_support_via_clause_db/2,
mpred_get_support_via_sentence/2,
mpred_get_support_one/2,
mpred_get_support_precanonical_plus_more/2,
mpred_deep_support0/2,
mpred_deep_support/2,
user_atom/1,
mpred_scan_tms/1,
well_founded/2,
mpred_tms_supported0/3,
mpred_tms_supported/3,
mpred_tms_supported/2,
mpred_remove_file_support/1,
without_running/1,
mpred_run_resume/0,
mpred_run_pause/0,
which_missing_argnum/2,
if_missing_mask/4,
if_missing_mask/3,
is_already_supported/3,
clause_asserted_local/1,
correctify_support/2,
pfcVersion/1,
mpred_current_op_support/1,
mpred_freeLastArg/2,
pfcVerifyMissing/3,
map_first_arg/3,
map_first_arg/2,
map_literals/3,
map_literals/2,
update_single_valued_arg/3,
has_body_atom/2,
is_action_body/1,
is_bc_body/1,
is_fc_body/1,
wac/0,
bwc/0,
fwc/0,
cwc/0,
mpred_rewrap_h/2,
mpred_is_info/1,
ain_minfo/1,
mpred_rule_hb_0/3,
mpred_rule_hb/3,
all_different_head_vals_2/2,
all_different_head_vals/1,
sub_term_v/2,
sub_term_eq/2,
mpred_pbody_f/5,
get_why/4,
mpred_pbody/5,
pfc_provide_storage_op/2,
is_retract_first/1,
mpred_is_taut/1,
mpred_is_tautology/1,
assert_eq_quitely/1,
retract_eq_quitely_f/1,
retract_eq_quitely/1,
mpred_each_literal/2,
has_functor/1,
make_uu_remove/1,
match_source_ref1/1,
mpred_nochaining/1,
erase_w_attvars/2,
call_s2/1,
call_s/1,
clauseq_s/3,
clause_s/3,
assertz_s/1,
asserta_s/1,
lookq_s/2,
lookq_s/1,
lookup_s/2,
lookup_s/1,
retract_s/1,
clause_s/2,
retractall_s/1,
assert_s/1,
listing_s/1,
add_side_effect/2,
record_se/0,
mpred_is_builtin/1,
is_mpred_action/1,
set_prolog_stack_gb/1,
w_get_fa/3,
f_to_mfa/4,
is_side_effect_disabled/0,
mreq/1,
check_real_context_module/0,
check_context_module/0,
lookup_inverted_op/3,
how_to_op/2,
reduce_mpred_op/2,
second_order/2,
assert_mu/1,
assertz_mu/2,
assertz_mu/1,
mpred_op/2,
mpred_retry/1,

has_functor/1,
          call_s/1,call_s2/1,asserta_s/1,assert_s/1,assertz_s/1,retract_s/1,retractall_s/1,
          clause_s/2,clause_s/3,lookup_s/1,lookup_s/2,lookq_s/2,lookq_s/1,



update_single_valued_arg/3,
ruleBackward/2,
retract_eq_quitely_f/1,
neg_in_code/1,
neg_in_code0/1,
mreq/1,
mpred_rule_hb/3,
mpred_remove_file_support/1,
mpred_nochaining/1,
mpred_negation_w_neg/2,          
mpred_negation_w_neg/2,
map_first_arg/2,
mpred_rule_hb/3,mpred_rule_hb_0/3,
is_side_effect_disabled/0,
is_resolved/1,
is_fc_body/1,
is_bc_body/1,
is_action_body/1,
has_cl/1,
has_body_atom/2,
cwc/0,
compute_resolve/3,
clause_or_call/2,          
clause_or_call/2,
check_context_module/0,
call_with_bc_triggers/1,

attvar_op/2,
%supporters_list/2,
%justifications/2,
% baseKB:mpred_provide_storage_clauses/3,
% justification/2,

mpred_facts_and_universe/1
            
          ]).
%:- endif.

:- module_transparent retract_mu/1,
               assert_mu/4,
               assert_mu/1,
               asserta_mu/2,
               asserta_mu/1,
               assertz_mu/2,
               assertz_mu/1,
               physical_side_effect/1.
:- module_transparent(attvar_op/2).
:- module_transparent(attvar_op/2).


:- meta_predicate 
      pred_head(1,*),
      physical_side_effect(+),
      oncely(0),
      naf(0),
      
      mpred_update_literal(*,*,0,*),
      mpred_retry(0),
      mpred_op(?, ?),
      mpred_facts_only(0),
      map_unless(1,:,*,*),      
      is_callable(0),     
      deducedSimply(0),
      cnstrn0(:,+),
      cnstrn(0),
      cnstrn(+,:),
      attvar_op(+,+),
      % clause_u(+,+,-),
      % call_u(+),
      assertz_mu(+),      
      assertz_mu(+,+),
      if_missing(+),
      assert_mu(+),
      assert_mu(+,+,+,+),
      ain_minfo_2(1,*),
      ain_minfo(1,*),
        whenAnd(0,0).

 :- meta_predicate mpred_get_support_one(0,*).
 :- meta_predicate mpred_get_support_precanonical_plus_more(0,*).
 % :- meta_predicate '__aux_maplist/2_cnstrn0+1'(*,0).
 :- meta_predicate repropagate_1(0).
 :- meta_predicate trigger_supporters_list(0,*).
 :- meta_predicate repropagate_meta_wrapper_rule(0).
 :- meta_predicate repropagate_0(0).

:- include('mpred_header.pi').

% :- use_module(logicmoo(util/logicmoo_util_preddefs)).



% oncely later will throw an error if there where choice points left over by call
:- meta_predicate(oncely(0)).
:- was_export(oncely/1).


%% oncely( :GoalCall) is semidet.
%
% Oncely.
%
oncely(:-(Call)):-!,Call,!.
oncely(:-(Call)):-!,call_u(Call).
oncely(Call):-once(Call).
% ================================================
% mpred_op/2
% ================================================

/*
query(t, call_u, G):- call_u(G).
query(_, _, Op, G):- dtrace(call_u(call(Op,G))).
once(A,B,C,D):-trace_or_throw(once(A,B,C,D)).
*/




%% second_order( ?VALUE1, ?VALUE2) is semidet.
%
% Second Order.
%
second_order(_,_):-fail.

:- meta_predicate(deducedSimply(0)).
:- was_export(deducedSimply/1).



%% deducedSimply( :GoalCall) is semidet.
%
% Deduced Simply.
%
deducedSimply(Call):- clause(deduce_facts(Fact,Call),Body),\+ clause_u((Call)),nonvar(Fact),Body,dmsg((deducedSimply2(Call):-Fact)),!,show_call(why,(clause_u(Fact),ground(Call))).

% deducedSimply(Call):- clause(deduce_facts(Fact,Call),Body),nonvar(Fact),Body,ground(Call),dmsg((deducedSimply1(Call):-Fact)),show_call(why,(clause_u(Fact),ground(Call))).

:- meta_predicate(mpred_op(?,+)).



%% mpred_op( ?Op, ?H) is semidet.
%
% Managed Predicate Oper..
%
mpred_op(Op,     H ):- (var(Op);var(H)),!,trace_or_throw(var_database_call(Op,  H )).
mpred_op(clause_u,H):-!,clause_u(H).
mpred_op(Op,     H ):- once(fully_expand(Op,H,HH)),H\=@=HH,!,mpred_op(Op, HH).
mpred_op(~(_,Op),  H ):- !, show_call(why, \+ (mpred_op(Op,  H ))).
mpred_op(change(assert,Op),H):-!,must(mpred_modify(change(assert,Op),H)),!.
mpred_op(change(retract,Op),H):-!,must(mpred_modify(change(retract,Op),H)),!.
mpred_op(query(t,Ireq),  H ):-!, mpred_op(Ireq,H).
mpred_op(query(Dbase_t,_Ireq),  H ):-!, mpred_op(Dbase_t,H).
mpred_op(call(Op),H):-!,mpred_op(Op,H).
mpred_op(Op,((include(A)))):- wno_tl(t_l:already_in_file_term_expansion,mpred_op(Op,((load_data_file(A))))),!.
mpred_op(Op, call(H)):- nonvar(H),!, mpred_op(Op,H).
mpred_op(Op,  not(H)):- nonvar(H),!, mpred_op(~(not,Op),H).
mpred_op(Op,'\\+'(H)):- nonvar(H),!, mpred_op(~(('\\+'),Op),H).
mpred_op(Op,    ~(H)):- nonvar(H),!, mpred_op(~(~,Op),H).
mpred_op(Op,     {H}):- nonvar(H),!, mpred_op(Op,H).

mpred_op(must,Call):- !,must(mpred_op(call_u,Call)).
mpred_op(once,Call):- !,once(mpred_op(call_u,Call)).

mpred_op(assertedOnly,Call):- !,w_tl(t_l:infInstanceOnly(Call),mpred_op(call_u,Call)).
mpred_op(_ , clause(H,B) ):- !, clause_u(H,B).
mpred_op(_ , clause(H,B,Ref) ):- !,  clause_u(H,B,Ref).
mpred_op(_ , (H :- B) ):- !, clause_u(H,B).
mpred_op(clauses(Op),  H):-!,mpred_op((Op),  H).
mpred_op(_,C):- call_u(C).

:- was_export(whenAnd/2).
:- module_transparent(whenAnd/2).



%% whenAnd( :GoalA, :GoalB) is semidet.
%
% When And.
%
whenAnd(A,B):-A,ground(B),once(B).


% =======================================
% Transforming DBASE OPs
% ========================================




%% reduce_mpred_op( ?Op, ?Op2) is semidet.
%
% Reduce Managed Predicate Oper..
%
reduce_mpred_op(Op,Op2):-must(cnotrace(transitive(how_to_op,Op,Op2))),!.
reduce_mpred_op(A,A).




%% how_to_op( ?HowOP, ?HowOP) is semidet.
%
% How Converted To Oper..
%
how_to_op(assert(a),asserta_new).
how_to_op(assert(z),assertz_if_new).
how_to_op(retract(one),retract).
how_to_op(retract(all),retract_all).
how_to_op(retract(all),retractall).
how_to_op(change(assert,z),assertz_if_new).
how_to_op(change(assert,_),asserta_if_new).
how_to_op(change(retract,one),retract).
how_to_op(change(retract,_),retract_all).
how_to_op(asserta,asserta_new).
how_to_op(assertz,assertz_if_new).
how_to_op(call(W),W).
how_to_op(clauses(W),W).
how_to_op(assert,assert_if_new).
how_to_op(assert(_),asserta_new).
how_to_op(retract(_),retract_all).
how_to_op(conjecture,call).
how_to_op(query(t, call_u),call_u).
how_to_op(query(t, Req),Req).
how_to_op(change(Op,HOW),O):- !, O=..[Op,HOW].
how_to_op(HowOP,HowOP).





%% lookup_inverted_op( ?VALUE1, ?VALUE2, +OUT3) is semidet.
%
% Lookup Inverted Oper..
%
lookup_inverted_op(retract,assert,-).
lookup_inverted_op(retractall,assert,-).
lookup_inverted_op(assert_if_new,retract,+).
lookup_inverted_op(assert_new,retract,+).
lookup_inverted_op(assertz_if_new,retract,+).
lookup_inverted_op(assertz_new,retract,+).
lookup_inverted_op(asserta_if_new,retract,+).
lookup_inverted_op(asserta_new,retract,+).


% ================================================
% is_callable/call_u/naf
% ================================================

%:- was_dynamic(naf/1).
:- meta_predicate(naf(0)).
:- was_export(naf/1).



%% naf( :GoalGoal) is semidet.
%
% Negation-By-Faliure.
%
naf(Goal):- (\+ call_u(Goal)).

:- meta_predicate(is_callable(0)).
:- was_export(is_callable/1).



%% is_callable( :GoalC) is semidet.
%
% If Is A Callable.
%
is_callable(C):-current_predicate(_,C),!.

:- module_transparent( (consequent_arg)/3).
consequent_arg(N,P,E):-get_consequent(P,PP),!,arg(N,PP,E).

:- module_transparent( (get_consequent)/2).
get_consequent((_:P),PP):-compound(P),!,get_consequent(P,PP).
get_consequent((P:-_),PP):-compound(P),!,get_consequent(P,PP).
get_consequent(~ P,PP):-compound(P),!,get_consequent(P,PP).
get_consequent(\+ P,PP):-compound(P),!,get_consequent(P,PP).
get_consequent(P,P).


:- include('mpred_header.pi').
:- style_check(+singleton).

% TODO READD
%:- foreach(consequent_arg(_,isEach(prologMultiValued,prologOrdered,prologNegByFailure,prologPTTP,prologKIF,pfcControlled,ttPredType,
%     prologHybrid,predCanHaveSingletons,prologDynamic,prologBuiltin,prologMacroHead,prologListValued,prologSingleValued),P),


% TODO ISSUE https://github.com/TeamSPoon/PrologMUD/issues/7

%% check_context_module is semidet.
%
% Check Context Module. (throws if it turns out wrong)
%
check_context_module:- !.
check_context_module:- is_release,!.
check_context_module:- sanity((source_context_module(M1),defaultAssertMt(M2),must(M1==M2))).

%% check_real_context_module is semidet.
%
% Check Real Context Module (throws if it turns out wrong)
%
check_real_context_module:- is_release,!.
check_real_context_module:-!.
check_real_context_module:- sanity((context_module(M1),defaultAssertMt(M2),must(M1==M2))).


% ======================= mpred_file('pfcsyntax').	% operator declarations.

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




%% mreq( +G) is semidet.
%
% Mreq.
%
mreq(G):- if_defined(call_u(G),fail).

% ======================= mpred_file('pfccore').	% core of Pfc.

%   File   : pfccore.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Updated: 10/11/87, ...
%            4/2/91 by R. McEntire: added calls to valid_dbref as a
%                                   workaround for the Quintus 3.1
%                            bug in the recorded database.
%   Purpose: core Pfc predicates.

/*

LogicMOO is mixing Mark Stickel's PTTP (prolog techn theorem prover) to create horn clauses that 
 PFC forwards and helps maintain in visible states )  in prolog knowledge baseable.. We use spft/3 to track deductions
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



%% f_to_mfa( +EF, ?R, ?F, ?A) is semidet.
%
% Functor Converted To Module-functor-arity.
%
f_to_mfa(EF,R,F,A):-w_get_fa(EF,F,A),
              (((current_predicate(F/A),functor(P,F,A),predicate_property(_M:P,imported_from(R)))*->true;
              current_predicate(F/A),functor(P,F,A),source_file(R:P,_SF))),
              current_predicate(R:F/A).


%% w_get_fa( +PI, ?F, ?A) is semidet.
%
% W Get Functor-arity.
%
w_get_fa(PI,_F,_A):-is_ftVar(PI),!.
w_get_fa(F/A,F,A):- !.
w_get_fa(PI,PI,_A):- atomic(PI),!.
w_get_fa(PI,F,A):- is_ftCompound(PI),!,functor(PI,F,A).
w_get_fa(Mask,F,A):-get_functor(Mask,F,A).



%% set_prolog_stack_gb( +Six) is semidet.
%
% Set Prolog Stack Gb.
%
set_prolog_stack_gb(Six):-set_prolog_stack(global, limit(Six*10**9)),set_prolog_stack(local, limit(Six*10**9)),set_prolog_stack(trail, limit(Six*10**9)).

%% module_local_init() is semidet.
%
% Hook To [baseKB:module_local_init/0] For Module Mpred_pfc.
% Module Local Init.
%
:- multifile(baseKB:mpred_hook_rescan_files/0).
:- dynamic(baseKB:mpred_hook_rescan_files/0).
baseKB:module_local_init:-set_prolog_stack_gb(16).
%:- was_dynamic(use_presently/0).
% used to annotate a predciate to indicate PFC support


%% is_mpred_action( :TermP) is semidet.
%
% If Is A Managed Predicate Action.
%
is_mpred_action('$VAR'(_)):-!,fail.
is_mpred_action(remove_if_unsupported(_,_)).
is_mpred_action(P):-is_static_predicate(P).

%% mpred_is_builtin( +P) is semidet.
%
% PFC If Is A Builtin.
%
mpred_is_builtin(P):- predicate_property(P,built_in), \+ predicate_property(P,dynamic).

/* UNUSED TODAY

:- use_module(library(mavis)).
:- use_module(library(type_check)).
:- use_module(library(typedef)).
*/



:- thread_local((t_l:use_side_effect_buffer , t_l:verify_side_effect_buffer)).

%% record_se is semidet.
%
% Record Se.
%
record_se:- (t_l:use_side_effect_buffer ; t_l:verify_side_effect_buffer).



%% add_side_effect( +Op, ?Data) is semidet.
%
% Add Side Effect.
%
add_side_effect(_,_):- ( \+  record_se ),!.
add_side_effect(Op,Data0):-get_source_ref1(Why),serialize_attvars(Data0,Data),assert(t_l:side_effect_buffer(Op,Data,Why)).


%% attvar_op( +:PRED1, ?Data) is semidet.
%
% Attribute Variable Oper..
%


listing_s(P):-call_s(xlisting(P)).

assert_s(H):- assertz_s(H).
retractall_s(H):- forall(clause_s(H,_,R),erase(R)).
clause_s(H,B):- clause_s(H,B,_).

retract_s(H):- lookup_s(H,R),erase(R).

lookup_s(H):- lookup_s(H,_). 

lookup_s(M:(H:-B),R):- !,clause_s(M:H,B,R).
lookup_s((H:-B),R):-  !,clause_s(H,B,R).
lookup_s(H,R):- clause_s(H,true,R).

lookq_s(X):-lookq_s(X,_Ref).

lookq_s(M:(H:-B),R):- !,clauseq_s(M:H,B,R).
lookq_s((H:-B),R):- !, clauseq_s(H,B,R).
lookq_s(H,R):- clauseq_s(H,true,R).

asserta_s(H):- fix_mp(clause(assert,asserta_s),H,M,H0),asserta_i(M:H0).
assertz_s(H):- fix_mp(clause(assert,assertz_s),H,M,H0),assertz_i(M:H0).
clause_s(H,B,R):- fix_mp(clause(clause,clause_s),H,M,H0),clause_u(M:H0,B,R).
clauseq_s(H,B,R):- fix_mp(clause(clause,clauseq_s),H,M,H0),clause_u(M:H0,B,R),clause(M:HC,BC,R),H0=@=HC,BC=@=B.

call_s(G0):-
  strip_module(G0,_,G),functor(G,F,A),
  (memberchk(F/A,[(',')/2])->
  mpred_METACALL(call_s,G);
  call_s2(G0)).

call_s2(G0):-
  strip_module(G0,WM,G),
  defaultAssertMt(U),  
  must(current_predicate(_,U:G)->(CALL=U:G);(current_predicate(_,WM:G0)->CALL=WM:G0; fail)),
 '$set_source_module'(S,U),'$module'(M,U),
  setup_call_cleanup_each(
    ('$set_source_module'(U),'$set_typein_module'(U)),CALL,
     ('$set_source_module'(S),'$set_typein_module'(M))).

/*
attvar_op(Op,Data):-
   deserialize_attvars(Data,Data0),
   attvar_op(Op,Data0).
*/
:- module_transparent(attvar_op/2).
attvar_op(Op,MData):-
   cnotrace((strip_module(Op,_,OpA), sanity((atom(OpA))),
   fix_mp(clause(assert,OpA),MData,M,Data),
   add_side_effect(OpA,M:Data),
   deserialize_attvars(Data,Data0))),
   (==(Data,Data0)->
     physical_side_effect(call(M:OpA,M:Data0));

   ((atom_concat(assert,_,OpA) -> physical_side_effect(M:call(M:OpA,M:Data0)));
   ((
    % nop((expand_to_hb(Data0,H,B),split_attrs(B,BA,G))),
    dtrace, 
    physical_side_effect(M:call(M:OpA,M:Data0))
    )))).



%% erase_w_attvars( +Data0, ?Ref) is semidet.
%
% Erase W Attribute Variables.
%
erase_w_attvars(Data0,Ref):- physical_side_effect(erase(Ref)),add_side_effect(erase,Data0).

:- thread_local(t_l:no_physical_side_effects/0).

%% physical_side_effect( +PSE) is semidet.
%
% Physical Side Effect.
%
physical_side_effect(PSE):- is_side_effect_disabled,!,mpred_warn('no_physical_side_effects ~p',PSE).
physical_side_effect(PSE):- PSE.

%% mpred_nochaining( +Goal) is semidet.
%
% PFC No Chaining.
%
mpred_nochaining(Goal):- w_tl(t_l:no_physical_side_effects,call(Goal)).

% TODO ISSUE https://github.com/TeamSPoon/PrologMUD/issues/7


%% match_source_ref1( :TermARG1) is semidet.
%
% Match Source Ref Secondary Helper.
%
match_source_ref1(ax):-!.
match_source_ref1(mfl(_,_,_)).

%% make_uu_remove( :TermU) is semidet.
%
% Make Uu Remove.
%
make_uu_remove((_,ax)).


%% has_functor( :TermC) is semidet.
%
% Has Functor.
%
has_functor(_):-!,fail.
has_functor(F/A):-!,is_ftNameArity(F,A),!.
has_functor(C):- (\+ is_ftCompound(C)),!,fail.
has_functor(C):-is_ftCompound(C),\+is_list(C).


%% mpred_each_literal( +P, ?E) is semidet.
%
% PFC Each Literal.
%
mpred_each_literal(P,E):-is_ftNonvar(P),P=(P1,P2),!,(mpred_each_literal(P1,E);mpred_each_literal(P2,E)).
mpred_each_literal(P,P). %:-conjuncts_to_list(P,List),member(E,List).



%% retract_eq_quitely( +H) is semidet.
%
% Retract Using (==/2) (or =@=/2) ) Quitely.
%
retract_eq_quitely(H):- call_u(retract_eq_quitely_f(H)).

%% retract_eq_quitely_f( +H) is semidet.
%
% Retract Using (==/2) (or =@=/2) ) Quitely False.
%
retract_eq_quitely_f((H:-B)):- !,clause_asserted_i(H,B,Ref),erase(Ref).
retract_eq_quitely_f(pfclog(H)):- retract_eq_quitely_f(H),fail.
retract_eq_quitely_f((H)):- clause_asserted_i(H,true,Ref),erase(Ref).


%% assert_eq_quitely( +H) is semidet.
%
% Assert Using (==/2) (or =@=/2) ) Quitely.
%
assert_eq_quitely(H):- attvar_op(assert_if_new,H).


%% mpred_is_tautology( +Var) is semidet.
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


% baseKB:decl_database_hook(Op,Hook):- loop_check_nr(pfc_provide_storage_op(Op,Hook)).


%% is_retract_first( +VALUE1) is semidet.
%
% If Is A Retract First.
%
is_retract_first(one).
is_retract_first(a).


%% pfc_provide_storage_op( +Op, ?I1) is semidet.
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
% pfc_provide_storage_op(clause_u,FactOrRule):- is_ftNonvar(FactOrRule),!,loop_check_nr(clause_u(FactOrRule)).


% pfcDatabaseGoal(G):-is_ftCompound(G),get_functor(G,F,A),pfcDatabaseTerm(F/A).




%% mpred_pbody( +H, ?B, ?R, ?BIn, ?WHY) is semidet.
%
% PFC Pbody.
%
mpred_pbody(_H,mpred_bc_only(_BC),_R,fail,deduced(backchains)):-!.
mpred_pbody(H,infoF(INFO),R,B,Why):-!,mpred_pbody_f(H,INFO,R,B,Why).
mpred_pbody(H,B,R,BIn,WHY):- is_true(B),!,BIn=B,get_why(H,H,R,WHY).
mpred_pbody(H,B,R,B,asserted(R,(H:-B))).


%% get_why( +VALUE1, ?CL, ?R, :TermR) is semidet.
%
% Get Generation Of Proof.
%
get_why(_,CL,R,asserted(R,CL:-U)):- clause_u(spft(CL, U, ax),true),!.
get_why(H,CL,R,deduced(R,WHY)):- (mpred_get_support(H,WH)*->WHY=(H=WH);(mpred_get_support(CL,WH),WHY=(CL=WH))).



%% mpred_pbody_f( +H, ?CL, ?R, ?B, ?WHY) is semidet.
%
% PFC Pbody False.
%
mpred_pbody_f(H,CL,R,B,WHY):- CL=(B==>HH),sub_term_eq(H,HH),!,get_why(H,CL,R,WHY).
mpred_pbody_f(H,CL,R,B,WHY):- CL=(HH<-B),sub_term_eq(H,HH),!,get_why(H,CL,R,WHY).
mpred_pbody_f(H,CL,R,B,WHY):- CL=(HH<==>B),sub_term_eq(H,HH),get_why(H,CL,R,WHY).
mpred_pbody_f(H,CL,R,B,WHY):- CL=(B<==>HH),sub_term_eq(H,HH),!,get_why(H,CL,R,WHY).
mpred_pbody_f(H,CL,R,fail,infoF(CL)):- trace_or_throw(mpred_pbody_f(H,CL,R)).


%% sub_term_eq( +H, ?HH) is semidet.
%
% Sub Term Using (==/2) (or =@=/2) ).
%
sub_term_eq(H,HH):-H==HH,!.
sub_term_eq(H,HH):-each_subterm(HH,ST),ST==H,!.


%% sub_term_v( +H, ?HH) is semidet.
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
all_different_head_vals_2(H,[A,R|EST]):-consequent_arg(_,H,E1),E1 ==A,dif(A,E2),consequent_arg(_,H,E2),\+ contains_var(A,E2),all_different_vals(dif_matrix,[A,E2,R|EST]),!.
all_different_head_vals_2(_H,[A,B|C]):-all_different_vals(dif_matrix,[A,B|C]),!.
all_different_head_vals_2(HB,_):- \+ compound(HB),!.
all_different_head_vals_2(H,[A]):-consequent_arg(_,H,E1),E1 ==A, H=..[_|ARGS], all_different_vals(dif_matrix,ARGS),!.
all_different_head_vals_2(H,[A]):-consequent_arg(_,H,E1),E1 ==A,  consequent_arg(_,H,E2), A\==E2, \+ contains_var(A,E2), dif(A,E2),!.
all_different_head_vals_2(H,[A]):-consequent_arg(_,H,E1),E1\==A, compound(E1), contains_var(A,E1), all_different_head_vals_2(E1,[A]),!.
all_different_head_vals_2(_,_).
   	 

%% mpred_rule_hb( +Outcome, ?OutcomeO, ?AnteO) is semidet.
%
% Calculate PFC Rule Head+body.
%
mpred_rule_hb(Outcome,OutcomeO,Body):- nonvar(OutcomeO),!,mpred_rule_hb(Outcome,OutcomeN,Body),must(OutcomeO=OutcomeN).
mpred_rule_hb(Outcome,OutcomeO,BodyO):- nonvar(BodyO),!,mpred_rule_hb(Outcome,OutcomeO,BodyN),must(BodyN=BodyO).
mpred_rule_hb(Outcome,OutcomeO,AnteO):- 
  quietly((mpred_rule_hb_0(Outcome,OutcomeO,Ante),
  mpred_rule_hb_0(Ante,AnteO,_))).
% :-mpred_trace_nochilds(mpred_rule_hb/3).


%% mpred_rule_hb_0( +Rule, -Head, -Body) is nondet.
%
% Calculate PFC rule Head+Body  Primary Helper.
%


mpred_rule_hb_0(Outcome,OutcomeO,true):-is_ftVar(Outcome),!,OutcomeO=Outcome.
mpred_rule_hb_0(Outcome,OutcomeO,true):- \+compound(Outcome),!,OutcomeO=Outcome.
mpred_rule_hb_0((Outcome1,Outcome2),OutcomeO,AnteO):- mpred_rule_hb(Outcome1,Outcome1O,Ante1),mpred_rule_hb(Outcome2,Outcome2O,Ante2),
                   conjoin(Outcome1O,Outcome2O,OutcomeO),
                   conjoin(Ante1,Ante2,AnteO).
mpred_rule_hb_0((Ante1==>Outcome),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0((Ante1=>Outcome),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0((Ante1->Outcome),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0((Ante1*->Outcome),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
% mpred_rule_hb_0((Outcome/Ante1),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(rhs([Outcome]),OutcomeO,Ante2):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
% mpred_rule_hb_0(rhs([OutcomeH|OutcomeT]),OutcomeO,Ante2):- !, mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0({Outcome},OutcomeO,Ante2):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0((Outcome<-Ante1),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0((Ante1 & Outcome),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0((Ante1 , Outcome),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0((Outcome<==>Ante1),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0((Ante1<==>Outcome),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(_::::Outcome,OutcomeO,Ante2):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb_0(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(bt(Outcome,Ante1),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(pt(Ante1,Outcome),OutcomeO,(Ante1,Ante2)):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(pk(Ante1a,Ante1b,Outcome),OutcomeO,(Ante1a,Ante1b,Ante2)):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(nt(Ante1a,Ante1b,Outcome),OutcomeO,(Ante1a,Ante1b,Ante2)):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(spft(Outcome,Ante1a,Ante1b),OutcomeO,(Ante1a,Ante1b,Ante2)):- (nonvar(Outcome)-> ! ; true),mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(que(Outcome,_),OutcomeO,Ante2):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
% mpred_rule_hb_0(pfc Default(Outcome),OutcomeO,Ante2):- (nonvar(Outcome)-> ! ; true), mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0((Outcome:-Ante),Outcome,Ante):-(nonvar(Outcome)-> ! ; true).
mpred_rule_hb_0(Outcome,Outcome,true).


%% ain_minfo( +G) is semidet.
%
% Assert If New Metainformation.
%
:- module_transparent(ain_minfo/1).
ain_minfo(G):-ain_minfo(assertz_if_new,G).

%% ain_minfo( :PRED1How, ?H) is semidet.
%
% Assert If New Metainformation.
%
:- module_transparent(ain_minfo/2).
ain_minfo(How,(H:-True)):-is_true(True),must(is_ftNonvar(H)),!,ain_minfo(How,H).
ain_minfo(How,(H<-B)):- !,ain_minfo(How,(H:-infoF(H<-B))),!,ain_minfo(How,(H:-mpred_bc_only(H))),ain_minfo_2(How,(B:-infoF(H<-B))).
ain_minfo(How,(B==>H)):- !,ain_minfo(How,(H:-infoF(B==>H))),!,ain_minfo_2(How,(B:-infoF(B==>H))).
ain_minfo(How,(B<==>H)):- !,ain_minfo(How,(H:-infoF(B<==>H))),!,ain_minfo(How,(B:-infoF(B<==>H))),!.
ain_minfo(How,((A,B):-INFOC)):-mpred_is_info(INFOC),(is_ftNonvar(A);is_ftNonvar(B)),!,ain_minfo(How,((A):-INFOC)),ain_minfo(How,((B):-INFOC)),!.
ain_minfo(How,((A;B):-INFOC)):-mpred_is_info(INFOC),(is_ftNonvar(A);is_ftNonvar(B)),!,ain_minfo(How,((A):-INFOC)),ain_minfo(How,((B):-INFOC)),!.
ain_minfo(How,(-(A):-infoF(C))):-is_ftNonvar(C),is_ftNonvar(A),!,ain_minfo(How,((A):-infoF((C)))). % attvar_op(How,(-(A):-infoF(C))).
ain_minfo(How,(~(A):-infoF(C))):-is_ftNonvar(C),is_ftNonvar(A),!,ain_minfo(How,((A):-infoF((C)))). % attvar_op(How,(-(A):-infoF(C))).
ain_minfo(How,(A:-INFOC)):-is_ftNonvar(INFOC),INFOC= mpred_bc_only(A),!,attvar_op(How,(A:-INFOC)),!.
ain_minfo(How,bt(_ABOX,H,_)):-!,attvar_op(How,(H:-mpred_bc_only(H))).
ain_minfo(How,nt(H,Test,Body)):-!,attvar_op(How,(H:-fail,nt(H,Test,Body))).
ain_minfo(How,pt(H,Body)):-!,attvar_op(How,(H:-fail,pt(H,Body))).
ain_minfo(How,(A0:-INFOC0)):- mpred_is_info(INFOC0), copy_term_and_varnames((A0:-INFOC0),(A:-INFOC)),!,must((mpred_rewrap_h(A,AA),imploded_copyvars((AA:-INFOC),ALLINFO), attvar_op(How,(ALLINFO)))),!.
%ain_minfo(How,G):-mpred_trace_msg(skipped_add_meta_facts(How,G)).
ain_minfo(_,_).

:- was_export(ain_minfo_2/2).

%% ain_minfo_2( :PRED1How, ?G) is semidet.
%
% Assert If New Metainformation  Extended Helper.
%
:- module_transparent(ain_minfo_2/2).
ain_minfo_2(How,G):-ain_minfo(How,G).


%% mpred_is_info( :TermC) is semidet.
%
% PFC If Is A Info.
%
mpred_is_info(mpred_bc_only(C)):-is_ftNonvar(C),!.
mpred_is_info(infoF(C)):-is_ftNonvar(C),!.
mpred_is_info((Fail,_)):-Fail==fail.


%:- was_dynamic(not_not/1).

%% mpred_rewrap_h( +A, ?A) is semidet.
%
% PFC Rewrap Head.
%
mpred_rewrap_h(A,A):-is_ftNonvar(A),\+ is_static_pred(A).
mpred_rewrap_h(A,F):- functor(A,F,_),\+ is_static_pred(F),!.
%mpred_rewrap_h(A,not_not(A)):-!.


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

%% is_fc_body( +P) is semidet.
%
% If Is A Forward Chaining Body.
%
is_fc_body(P):-cwc, has_body_atom(fwc,P).

%% is_bc_body( +P) is semidet.
%
% If Is A Backchaining Body.
%
is_bc_body(P):-cwc, has_body_atom(bwc,P).

%% is_action_body( +P) is semidet.
%
% If Is A Action Body.
%
is_action_body(P):-cwc, has_body_atom(wac,P).



%% has_body_atom( +WAC, ?P) is semidet.
%
% Has Body Atom.
%
has_body_atom(WAC,P):-cwc, call(
   WAC==P -> true ; (is_ftCompound(P),consequent_arg(1,P,E),has_body_atom(WAC,E))),!.

/*
has_body_atom(WAC,P,Rest):-cwc, call(WAC==P -> Rest = true ; (is_ftCompound(P),functor(P,F,A),is_atom_body_pfa(WAC,P,F,A,Rest))).
is_atom_body_pfa(WAC,P,F,2,Rest):-consequent_arg(1,P,E),E==WAC,consequent_arg(2,P,Rest),!.
is_atom_body_pfa(WAC,P,F,2,Rest):-consequent_arg(2,P,E),E==WAC,consequent_arg(1,P,Rest),!.
*/


same_functors(Head1,Head2):-must_det(get_consequent_functor(Head1,F1,A1)),must_det(get_consequent_functor(Head2,F2,A2)),!,F1=F2,A1=A2.


%% mpred_update_literal( +P, ?N, ?Q, ?R) is semidet.
%
% PFC Update Literal.
%
mpred_update_literal(P,N,Q,R):-
    consequent_arg(N,P,UPDATE),call(replace_arg(P,N,Q_SLOT,Q)),
    must(call_u(Q)),update_value(Q_SLOT,UPDATE,NEW), 
    replace_arg(Q,N,NEW,R).


% spft(5,5,5).

%% update_single_valued_arg(+Module, +P, ?N) is semidet. 
%
% Update Single Valued Argument.
%
:- module_transparent( (update_single_valued_arg)/3).

update_single_valued_arg(M,M:Pred,N):-!,update_single_valued_arg(M,Pred,N).

update_single_valued_arg(world,P,N):- !, update_single_valued_arg(baseKB,P,N).
update_single_valued_arg(M,P,N):- \+ clause_b(mtCycL(M)), clause_b(mtCycL(M2)),!,update_single_valued_arg(M2,P,N).

update_single_valued_arg(M,P,N):- 
  consequent_arg(N,P,UPDATE),
  is_relative(UPDATE),!,
  dtrace,
  replace_arg(P,N,OLD,Q),
  must_det_l((clause_u(Q),update_value(OLD,UPDATE,NEW),\+ is_relative(NEW), replace_arg(Q,N,NEW,R))),!,
  update_single_valued_arg(M,R,N).


update_single_valued_arg(M,P,N):- 
 call_u((must_det_l((
  call_u(mtCycL(M)),
  mpred_type_args \= M,
  mpred_kb_ops \= M,
  consequent_arg(N,P,UPDATE),
  replace_arg(P,N,Q_SLOT,Q),
  var(Q_SLOT),
  same_functors(P,Q),
  % get_source_ref1(U),
  must_det_l((
     % rtrace(attvar_op(assert_if_new,M:spft(P,U,ax))),
     % (call_u(P)->true;(assertz_mu(P))),
     assertz_mu(M,P),
     doall((
          lookup_u(M:Q,E),
          UPDATE \== Q_SLOT,
          erase(E),
          mpred_unfwc1(M:Q))))))))).

% ======================= 
% utils
% ======================= 

%% map_literals( +P, ?G) is semidet.
%
% Map Literals.
%
map_literals(P,G):-map_literals(P,G,[]).


%% map_literals( +VALUE1, :TermH, ?VALUE3) is semidet.
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


%% map_first_arg( +Pred, ?List) is semidet.
%
% PFC Maptree.
%
map_first_arg(Pred,List):-map_first_arg(Pred,List,[]).

%% map_first_arg( +Pred, :TermH, ?S) is semidet.
%
% PFC Maptree.
%
map_first_arg(Pred,H,S):-is_ftVar(H),!,apply(Pred,[H|S]).
map_first_arg(_,[],_) :- !.
map_first_arg(Pred,(H,T),S):-!, map_first_arg(Pred,H,S), map_first_arg(Pred,T,S).
map_first_arg(Pred,(H;T),S):-!, map_first_arg(Pred,H,S) ; map_first_arg(Pred,T,S).
map_first_arg(Pred,[H|T],S):-!, apply(Pred,[H|S]), map_first_arg(Pred,T,S).
map_first_arg(Pred,H,S):-apply(Pred,[H|S]). 

% % :- use_module(logicmoo(util/rec_lambda)).

%example pfcVerifyMissing(mpred_isa(I,D), mpred_isa(I,C), ((mpred_isa(I,C), {D==C});-mpred_isa(I,C))). 
%example pfcVerifyMissing(mudColor(I,D), mudColor(I,C), ((mudColor(I,C), {D==C});-mudColor(I,C))). 


%% pfcVerifyMissing( +GC, ?GO, ?GO) is semidet.
%
% Prolog Forward Chaining Verify Missing.
%
pfcVerifyMissing(GC, GO, ((GO, {D==C});\+ GO) ):-  GC=..[F,A|Args],append(Left,[D],Args),append(Left,[C],NewArgs),GO=..[F,A|NewArgs],!.

%example mpred_freeLastArg(mpred_isa(I,C),~(mpred_isa(I,C))):-is_ftNonvar(C),!.
%example mpred_freeLastArg(mpred_isa(I,C),(mpred_isa(I,F),C\=F)):-!.

%% mpred_freeLastArg( +G, ?GG) is semidet.
%
% PFC Free Last Argument.
%
mpred_freeLastArg(G,GG):- G=..[F,A|Args],append(Left,[_],Args),append(Left,[_],NewArgs),GG=..[F,A|NewArgs],!.
mpred_freeLastArg(_G,false).


%% mpred_current_op_support( +VALUE1) is semidet.
%
% PFC Current Oper. Support.
%
mpred_current_op_support((p,p)):-!.


%% pfcVersion( +VALUE1) is semidet.
%
% Prolog Forward Chaining Version.
%
pfcVersion(6.6).


% % :- '$set_source_module'(mpred_kb_ops).

%% correctify_support( +S, ?S) is semidet.
%
% Correctify Support.
%
correctify_support(U,(U,ax)):-var(U),!.
correctify_support((U,U),(U,ax)):-!.
correctify_support((S,T),(S,T)):-!.
correctify_support((U,_UU),(U,ax)):-!.
correctify_support([U],S):-correctify_support(U,S).
correctify_support(U,(U,ax)).


%% clause_asserted_local( :TermABOX) is semidet.
%
% Clause Asserted Local. 
%
clause_asserted_local(MCL):-
  strip_module(MCL,_,CL),
  must(CL=spft(P,Fact,Trigger )),!,
  clause_u(spft(P,Fact,Trigger),true,Ref),
  clause_u(spft(UP,UFact,UTrigger),true,Ref),
  (((UP=@=P,UFact=@=Fact,UTrigger=@=Trigger))).



%% is_already_supported( +P, ?S, ?UU) is semidet.
%
% If Is A Already Supported.
%
is_already_supported(P,(S,T),(S,T)):- clause_asserted_local(spft(P,S,T)),!.
is_already_supported(P,_S,UU):- clause_asserted_local(spft(P,US,UT)),must(get_source_ref(UU)),UU=(US,UT).

% TOO UNSAFE 
% is_already_supported(P,_S):- copy_term_and_varnames(P,PC),sp ftY(PC,_,_),P=@=PC,!.


if_missing(Q):- mpred_literal_nv(Q), call_u( \+ ~ Q), if_missing_mask(Q,R,Test),!, lookup_u(R), Test.

%% if_missing_mask( +Q, ?R, ?Test) is semidet.
%
% If Missing Mask.
%

if_missing_mask(_M:Q,R,Test):- nonvar(Q),!,if_missing_mask(Q,R,Test).
if_missing_mask(Q,~Q,\+Q):- \+ is_ftCompound(Q),!.
if_missing_mask(PQ,RO,TestO):- once(mpred_rule_hb(PQ,Q,P)),P\==true,PQ\==Q,!,if_missing_mask(Q,R,TestO),subst(PQ,Q,R,RO).
if_missing_mask(Q,R,Test):-
   which_missing_argnum(Q,N),
   if_missing_mask(Q,N,R,Test),!.
if_missing_mask(ISA, ~ ISA, \+ ISA).

%% if_missing_mask( +Q, ?N, ?R, ?Test) is semidet.
%
% If Missing Mask.
%
if_missing_mask(Q,N,R,Test):-
  consequent_arg(N,Q,Was),
  (nonvar(R)-> (which_missing_argnum(R,RN),consequent_arg(RN,R,NEW));replace_arg(Q,N,NEW,R)),!,
   Test=dif:dif(Was,NEW).

/*
Old version
if_missing_mask(Q,N,R,dif:dif(Was,NEW)):- 
 must((is_ftNonvar(Q),acyclic_term(Q),acyclic_term(R),functor(Q,F,A),functor(R,F,A))),
  (singleValuedInArg(F,N) -> 
    (consequent_arg(N,Q,Was),replace_arg(Q,N,NEW,R));
    ((consequent_arg(N,Q,Was),is_ftNonvar(Was)) -> replace_arg(Q,N,NEW,R);
        (N=A,consequent_arg(N,Q,Was),replace_arg(Q,N,NEW,R)))).
*/


%% which_missing_argnum( +VALUE1, ?VALUE2) is semidet.
%
% Which Missing Argnum.
%
which_missing_argnum(Q,N):-
 must((acyclic_term(Q),is_ftCompound(Q),get_functor(Q,F,A))),
 F\=t,
  (singleValuedInArg(F,N) -> true;
    ((consequent_arg(N,Q,Was),is_ftNonvar(Was)) -> true; N=A)).
mpred_run_pause:- asserta(t_l:mpred_run_paused).
mpred_run_resume:- retractall(t_l:mpred_run_paused).

without_running(G):- (t_l:mpred_run_paused->G;w_tl(t_l:mpred_run_pause,G)).

mpred_remove_file_support(_File):- !.
mpred_remove_file_support(File):- 
  forall((filematch(File,File0),freeze(Match,contains_var(File0,Match))),
      forall(lookup_u(spft( W, Match, ax)),forall(retract_u(spft( W, Match, ax)),mpred_remove(W)))).

/*

%% remove_if_unsupported( +Why, ?P) is semidet.
%
% Remove If Unsupported.
%
remove_if_unsupported(Why,P) :- is_ftVar(P),!,trace_or_throw(warn(var_remove_if_unsupported(Why,P))).
remove_if_unsupported(Why,P) :- ((\+ ground(P), P \= (_:-_) , P \= ~(_) ) -> mpred_trace_msg(warn(nonground_remove_if_unsupported(Why,P))) ;true),
   (((mpred_tms_supported(local,P,How),How\=unknown(_)) -> mpred_trace_msg(still_supported(How,Why,local,P)) ; (  mpred_undo(Why,P)))),!.
   % mpred_run.

*/

%= mpred_tms_supported(+P,-How) succeeds if P is "supported". What "How" means
%= depends on the TMS mode selected.


%% mpred_tms_supported( +P, ?How) is semidet.
%
% PFC Truth Maintainence/wff Supported.
%
mpred_tms_supported(P,How) :-
  lookup_u(tms(Mode)),
  mpred_tms_supported0(Mode,P,How).



%% mpred_tms_supported( +Mode, ?P, ?How) is semidet.
%
% PFC Truth Maintainence/wff Supported.
%
mpred_tms_supported(Mode,P,How) :- is_ftVar(Mode),get_tms_mode(P,tms(Mode)),!,mpred_tms_supported0(Mode,P,How).
mpred_tms_supported(Mode,P,How) :- mpred_tms_supported0(Mode,P,How).
mpred_tms_supported(How,_P,unknown(How)).

:- module_transparent((mpred_wfflist)/2).
:- module_transparent((mpred_wff)/3).


%% mpred_tms_supported0( +TmsMode, ?P, ?How) is semidet.
%
% PFC Truth Maintainence/wff Supported Primary Helper.
%
mpred_tms_supported0(local,P,How) :-  mpred_get_support(P,How). % ,sanity(mpred_deep_support(How,S)).
mpred_tms_supported0(cycles,P,How) :-  well_founded(P,How).
mpred_tms_supported0(deep,P,How) :- mpred_deep_support(How,P).

% baseKB:hook_one_minute_timer_tick:- statistics.

%% well_founded( +Fact, ?How) is semidet.
%
% a fact is well founded if it is supported by the user
% or by a set of facts and a rules, all of which are well founded.
%
well_founded(Fact,How) :- mpred_wff(Fact,[],How).



%% mpred_wff( ?F, ?VALUE2, :TermHow) is semidet.
%
% PFC Well-formed Formula.
%
mpred_wff(F,_,How) :-
  % supported by user (mpred_axiom) or an "absent" fact (assumption).
  ((mpred_axiom(F),How =mpred_axiom(F) ); (mpred_assumption(F),How=mpred_assumption(F))),
  !.

mpred_wff(F,Descendants,wff(Supporters)) :-
  % first make sure we aren''t in a loop.
  (\+ memberchk(F,Descendants)),
  % find a justification.
  supporters_list(F,Supporters),
  % all of whose members are well founded.
  mpred_wfflist(Supporters,[F|Descendants]),
  !.



%% mpred_wfflist(+L1, ?L2) is semidet.
%
%  simply maps mpred_wff over the list.
%
mpred_wfflist([],_).
mpred_wfflist([X|Rest],L) :-
  mpred_wff(X,L,_How),
  mpred_wfflist(Rest,L).


%% mpred_scan_tms( +P) is semidet.
%
% PFC Scan Truth Maintainence/wff.
%
mpred_scan_tms(P):-mpred_get_support(P,(S,SS)),
  (S==SS-> true;
   once((mpred_deep_support(_How,P)->true;
     (mpred_trace_msg(warn(now_maybe_unsupported(mpred_get_support(P,(S,SS)),fail))))))).


%% user_atom( +U) is semidet.
%
% User Atom.
%
user_atom(mfl(_,_,_)):-!.
user_atom(ax).
user_atom(s(_)).


%% mpred_deep_support( +How, ?M) is semidet.
%
% PFC Deep Support.
%
mpred_deep_support(_How,unbound):-!,fail.
mpred_deep_support(How,M):-loop_check(mpred_deep_support0(How,M),fail).


%% mpred_deep_support0( +U, ?U) is semidet.
%
% PFC Deep Support Primary Helper.
%
mpred_deep_support0(user_atom(U),(U,ax)):-user_atom(U),!.
mpred_deep_support0(How,(A==>_)):-!,mpred_deep_support(How,A).
mpred_deep_support0(pt(HowA,HowB),pt(A,B)):-!,mpred_deep_support(HowA,A),mpred_deep_support(HowB,B).
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


%% mpred_get_support_precanonical_plus_more( +P, ?Sup) is semidet.
%
% PFC Get Support Precanonical Plus More.
%
mpred_get_support_precanonical_plus_more(P,Sup):- 
  mpred_get_support_one(P,Sup)*->true;
  ((fully_expand(mpred_get_support_precanonical_plus_more,P,PE),!,
    P\=@=PE,mpred_get_support_one(PE,Sup))).

%% mpred_get_support_one( +P, ?Sup) is semidet.
%
% PFC Get Support One.
%
mpred_get_support_one(P,Sup):- mpred_get_support(P,Sup)*->true;
  (mpred_get_support_via_clause_db(P,Sup)*->true;
     mpred_get_support_via_sentence(P,Sup)).


%% mpred_get_support_via_sentence( +Var, ?VALUE2) is semidet.
%
% PFC Get Support Via Sentence.
%
mpred_get_support_via_sentence(Var,_):-is_ftVar(Var),!,fail.
mpred_get_support_via_sentence((A,B),(FC,TC)):-!, mpred_get_support_precanonical_plus_more(A,(FA,TA)),mpred_get_support_precanonical_plus_more(B,(FB,TB)),conjoin(FA,FB,FC),conjoin(TA,TB,TC).
mpred_get_support_via_sentence(true,g):-!.
mpred_get_support_via_sentence(G,call_u(G)):- call_u(G).



%% mpred_get_support_via_clause_db( :TermP, ?OUT) is semidet.
%
% PFC Get Support Via Clause Database.
%
mpred_get_support_via_clause_db(\+ P,OUT):- mpred_get_support_via_clause_db(~(P),OUT).
mpred_get_support_via_clause_db(\+ P,(naf(g),g)):- !, predicate_property(P,number_of_clauses(_)),\+ clause(P,_Body).
mpred_get_support_via_clause_db(P,OUT):- predicate_property(P,number_of_clauses(N)),N>0,
   clause_u(P,Body),(Body==true->Sup=(g);
    (support_ok_via_clause_body(P),mpred_get_support_precanonical_plus_more(Body,Sup))),
   OUT=(Sup,g).



%% support_ok_via_clause_body( +H) is semidet.
%
% Support Ok Via Clause Body.
%
support_ok_via_clause_body(_H):-!,fail.
support_ok_via_clause_body(H):- get_functor(H,F,A),support_ok_via_clause_body(H,F,A).


%% support_ok_via_clause_body( +VALUE1, ?F, ?VALUE3) is semidet.
%
% Support Ok Via Clause Body.
%
support_ok_via_clause_body(_,(\+),1):-!,fail.
support_ok_via_clause_body(_,F,_):- lookup_u(argsQuoted(F)),!,fail.
support_ok_via_clause_body(H,F,A):- should_call_for_facts(H,F,A).




%% mpred_get_support_precanonical( +F, ?Sup) is semidet.
%
% PFC Get Support Precanonical.
%
mpred_get_support_precanonical(F,Sup):-fully_expand(mpred_get_support_precanonical,F,P),mpred_get_support(P,Sup).

%% spft_precanonical( +F, ?SF, ?ST) is semidet.
%
% Spft Precanonical.
%

spft_precanonical(F,SF,ST):- fully_expand(spft_precanonical,F,P),!,mpred_get_support(P,(SF,ST)).


%% trigger_supporters_list( +U, :TermARG2) is semidet.
%
% Trigger Supports Functor (list Version).
%
trigger_supporters_list(U,[]) :- match_source_ref1(U),!.
trigger_supporters_list(U,[]) :- atom(U),!.

trigger_supporters_list(Trigger,[Fact|MoreFacts]) :-
  mpred_get_support_precanonical_plus_more(Trigger,(Fact,AnotherTrigger)),
  must(trigger_supporters_list(AnotherTrigger,MoreFacts)).

mpred_retry(G):- fail; cnotrace(G).


%% { ?G} is semidet.
%
% {}.
%
'{}'(G):-call_u(G).

:- meta_predicate neg_in_code(*).
:- export(neg_in_code/1).

%% neg_in_code( +G) is semidet.
%
% Negated In Code.
%
neg_in_code(G):-nonvar(G),loop_check(neg_in_code0(G)).
:- meta_predicate neg_in_code0(*).
:- export(neg_in_code0/1).
neg_in_code0(G):- var(G),!,lookup_u(~ G).
neg_in_code0(call_u(G)):- !,call_u(~G).
neg_in_code0(~(G)):- nonvar(G),!,  \+ call_u(~G) ,!.
neg_in_code0(G):-   neg_may_naf(G), \+ call_u(G),!.
neg_in_code0(G):-  is_ftNonvar(G), a(prologSingleValued,G),must((if_missing_mask(G,R,Test),nonvar(R),nonvar(Test))),call_u(R),!,call_u(Test).


:- meta_predicate neg_may_naf(0).
:- module_transparent(neg_may_naf/1).
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
%= assigning them support from God. (g,ax)
%=

%% call_u_req( +G) is semidet.
%
% Req.
%
call_u_req(G):- loop_check(mpred_call_0(G),fail).


%% mpred_call_only_facts( +Clause) is semidet.
%
% PFC Call Only Facts.
%
mpred_call_only_facts(Clause) :-  strip_module(Clause,_,F), 
  on_x_rtrace(no_repeats(loop_check(mpred_call_0(F),fail))). 

%% mpred_call_only_facts( +Why, ?F) is semidet.
%
% PFC Call Only Facts.
%
mpred_call_only_facts(_Why,F):- on_x_rtrace(no_repeats(loop_check(mpred_call_0(F),fail))). 




%% mpred_call_0( +Var) is semidet.
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
mpred_call_0(call_u(X)):- !, mpred_call_0(X).
mpred_call_0(\+(X)):- !, \+ mpred_call_0(X).
mpred_call_0(call_u(X)):- !, mpred_call_0(X).
mpred_call_0(clause(H,B,Ref)):-!,clause_u(H,B,Ref).
mpred_call_0(clause(H,B)):-!,clause_u(H,B).
mpred_call_0(clause(HB)):-expand_to_hb(HB,H,B),!,clause_u(H,B).
mpred_call_0(asserta(X)):- !, aina(X).
mpred_call_0(assertz(X)):- !, ainz(X).
mpred_call_0(assert(X)):- !, mpred_ain(X).
mpred_call_0(retract(X)):- !, mpred_remove(X).
% TODO: test removal
%mpred_call_0(prologHybrid(H)):-get_functor(H,F),!,isa_asserted(F,prologHybrid).
mpred_call_0((H)):- is_static_pred(H),!,show_pred_info(H),dtrace(mpred_call_0((H))).
%mpred_call_0(HB):-hotrace((fully_expand_warn(mpred_call_0,HB,HHBB))),!,mpred_call_0(HHBB).
mpred_call_0(H):- !, w_tl(t_l:infAssertedOnly(H),call_u(H)).
%mpred_call_0(argIsa(mpred_isa,2,mpred_isa/2)):-  trace_or_throw(mpred_call_0(argIsa(mpred_isa,2,mpred_isa/2))),!,fail.
% TODO: test removal
% mpred_call_0(isa(H,B)):-!,isa_asserted(H,B).



mpred_call_0(M:P):-!,sanity(nonvar(P)),functor(P,F,_),mpred_call_1(M,P,F).
mpred_call_0(G):- strip_module(G,M,P),sanity(nonvar(P)),functor(P,F,_),mpred_call_1(M,P,F).



%% mpred_call_1( +VALUE1, ?G, ?VALUE3) is semidet.
%
% PFC call  Secondary Helper.
%
mpred_call_1(_,G,_):- is_side_effect_disabled,!,mpred_call_with_no_triggers(G).

mpred_call_1(M,G,F):- sanity(\+  is_side_effect_disabled),
               (ground(G); \+ current_predicate(_,M:G) ; \+ (predicate_property(M:G,number_of_clauses(CC)),CC>1)), 
    
                ignore((loop_check(call_with_bc_triggers(M:G)),maybeSupport(G,(g,ax)),fail)),
                 \+ current_predicate(F,M:G),\+ current_predicate(_,_:G),
                 doall(show_call(predicate_property(_UM:G,_PP))),
                 debug(mpred),
                 fail,
                 %TODO remove this failure
                 must(show_call(kb_dynamic(M:G))),
                 kb_dynamic(M:G),!,fail.
mpred_call_1(_,G,_):- mpred_call_with_no_triggers(G).


:- thread_local t_l:infBackChainPrevented/1.


%% call_with_bc_triggers( +MP) is semidet.
%
% Call Using Backchaining Triggers.
%
call_with_bc_triggers(MP) :- strip_module(MP,_,P), functor(P,F,A), \+ t_l:infBackChainPrevented(F/A), 
  lookup_u(bt(P,Trigger)),
  no_repeats(mpred_get_support(bt(P,Trigger),S)),
  once(no_side_effects(P)),
  w_tl(t_l:infBackChainPrevented(F/A),mpred_eval_lhs(Trigger,S)).


%% mpred_call_with_no_triggers( +Clause) is semidet.
%
% PFC Call Using No Triggers.
%
mpred_call_with_no_triggers(Clause) :-  strip_module(Clause,_,F),
  %= this (is_ftVar(F)) is probably not advisable due to extreme inefficiency.
  (is_ftVar(F)    ->  mpred_facts_and_universe(F) ;
     mpred_call_with_no_triggers_bound(F)).


%% mpred_call_with_no_triggers_bound( +F) is semidet.
%
% PFC Call Using No Triggers Bound.
%
mpred_call_with_no_triggers_bound(F):- mpred_call_with_no_triggers_uncaugth(F).

%% mpred_call_with_no_triggers_uncaugth( +Clause) is semidet.
%
% PFC Call Using No Triggers Uncaugth.
%
mpred_call_with_no_triggers_uncaugth(Clause) :-  strip_module(Clause,_,F),
  show_failure(mpred_call_with_no_triggers_bound,no_side_effects(F)),
  (\+ current_predicate(_,F) -> fail;call_u(F)).
  %= we check for system predicates as well.
  %has_cl(F) -> (clause_u(F,Condition),(Condition==true->true;call_u(Condition)));
  %call_u(F).


%% mpred_bc_only( +M) is semidet.
%
% PFC Backchaining Only.
%

%mpred_bc_only(G):- !,defaultAssertMt(W), loop_check(mpred_BC_w_cache(W,G)).
%mpred_bc_only(M:G):- !, loop_check(with_umt(M,mpred_bc_only0(G))).
mpred_bc_only(G):- no_repeats(loop_check(mpred_bc_only0(G))).


% % :- '$set_source_module'(mpred_kb_ops).

%% mpred_bc_only0( +G) is semidet.
%
% PFC Backchaining Only Primary Helper.
%
mpred_bc_only0(G):- mpred_unnegate(G,Pos),!, show_call(why,\+ mpred_bc_only(Pos)).
mpred_bc_only0(G):- pfcBC_NoFacts(G).
% mpred_bc_only0(G):- mpred_call_only_facts(G).

%%
%= pfcBC_NoFacts(F) is true iff F is a fact available for backward chaining ONLY.
%= Note that this has the side effect of catching unsupported facts and
%= assigning them support from God.
%= this Predicate should hide Facts from mpred_bc_only/1
%%

%% pfcBC_NoFacts( +F) is semidet.
%
% Prolog Forward Chaining Backtackable Class No Facts.
%
pfcBC_NoFacts(F):- pfcBC_NoFacts_TRY(F)*-> true ; (mpred_slow_search,pfcBC_Cache(F)).


%% mpred_slow_search is semidet.
%
% PFC Slow Search.
%
mpred_slow_search.


/*
%% ruleBackward( +R, ?Condition) is semidet.
%
% Rule Backward.
%
ruleBackward(R,Condition):- call_u(( ruleBackward0(R,Condition),functor(Condition,F,_),\+ consequent_arg(_,v(mpred_call_no_bc,call,call_u),F))).
%ruleBackward0(F,Condition):-clause_u(F,Condition),\+ (is_true(Condition);mpred_is_info(Condition)).

%% ruleBackward0( +F, ?Condition) is semidet.
%
% Rule Backward Primary Helper.
%
ruleBackward0(F,Condition):- call_u((  '<-'(F,Condition),\+ (is_true(Condition);mpred_is_info(Condition)) )).

%:- was_dynamic('{}'/1).
%{X}:-dmsg(legacy({X})),call_u(X).
*/


%% pfcBC_NoFacts_TRY( +F) is semidet.
%
% Prolog Forward Chaining Backtackable Class No Facts Try.
%
pfcBC_NoFacts_TRY(F) :- no_repeats(ruleBackward(F,Condition)),
  % neck(F),
  call_u(Condition),
  maybeSupport(F,(g,ax)).



%% pfcBC_Cache( +F) is semidet.
%
% Prolog Forward Chaining Backtackable Class Cache.
%
pfcBC_Cache(F) :- mpred_call_only_facts(pfcBC_Cache,F),
   ignore((ground(F),( (\+mpred_call_0(F)), maybeSupport(F,(g,ax))))).



%% maybeSupport( +P, ?VALUE2) is semidet.
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
%mpred_ignored(isa(W,tCol)):-mreq(baseKB:hasInstance_dyn(tCol,W)).
mpred_ignored(isa(W,_)):-is_ftCompound(W),call_u(isa(W,meta_argtypes)).
mpred_ignored(C):-clause_safe(C,true). 
mpred_ignored(isa(_,Atom)):-atom(Atom),atom_concat(ft,_,Atom),!.
mpred_ignored(isa(_,argIsaFn(_, _))).



%% has_cl( +H) is semidet.
%
% Has Clause.
%
has_cl(H):-predicate_property(H,number_of_clauses(_)).

% an action is undoable if there exists a method for undoing it.


%% mpred_negation_w_neg( +P, ?NF) is semidet.
%
% PFC Negation W Negated.
%
mpred_negation_w_neg(~(P),P):-is_ftNonvar(P),!.
mpred_negation_w_neg(P,NF):-mpred_nf1_negation(P,NF).


%% hook_one_minute_timer_tick is semidet.
%
% Hook To [baseKB:hook_one_minute_timer_tick/0] For Module Mpred_pfc.
% Hook One Minute Timer Tick.
%
baseKB:hook_one_minute_timer_tick:-mpred_cleanup.


%% mpred_cleanup is semidet.
%
% PFC Cleanup.
%
mpred_cleanup:- forall((no_repeats(F-A,(mpred_mark(pfcRHS,F,A),A>1))),mpred_cleanup(F,A)).


%% mpred_cleanup( +F, ?A) is semidet.
%
% PFC Cleanup.
%
mpred_cleanup(F,A):-functor(P,F,A),predicate_property(P,dynamic)->mpred_cleanup_0(P);true.


%% mpred_cleanup_0( +P) is semidet.
%
% PFC cleanup  Primary Helper.
%
mpred_cleanup_0(P):- findall(P-B-Ref,clause(P,B,Ref),L),
  forall(member(P-B-Ref,L),erase_w_attvars(clause(P,B,Ref),Ref)),forall(member(P-B-Ref,L),attvar_op(assertz_if_new,((P:-B)))).

% :-debug.
%isInstFn(A):-!,trace_or_throw(isInstFn(A)).

%= mpred_unnegate(N,P) is true if N is a negated term and P is the term
%= with the negation operator stripped.

/*
%% mpred_unnegate( +P, ?P) is semidet.
%
% PFC Negation.
%
mpred_unnegate((-P),P).
% mpred_unnegate((~P),P).
mpred_unnegate((\+(P)),P).
*/
/*

%% mpred_negated_literal( +P) is semidet.
%
% PFC Negated Literal.
%
mpred_negated_literal(P):-is_reprop(P),!,fail.
mpred_negated_literal(P):-mpred_negated_literal(P,_).

%% mpred_negated_literal( +P, ?Q) is semidet.
%
% PFC Negated Literal.
%
mpred_negated_literal(P,Q) :- is_ftNonvar(P),
  mpred_unnegate(P,Q),
  mpred_literal(Q).

*/

%% mpred_is_assertable( +X) is semidet.
%
% PFC If Is A Assertable.
%
mpred_is_assertable(X):- mpred_literal_nv(X),\+ functor(X,{},_).

%% mpred_literal_nv( +X) is semidet.
%
% PFC Literal Nv.
%
mpred_literal_nv(X):-is_ftNonvar(X),mpred_literal(X).

/*
%% mpred_literal( +X) is semidet.
%
% PFC Literal.
%
mpred_literal(X) :- is_reprop(X),!,fail.
mpred_literal(X) :- cyclic_term(X),!,fail.
mpred_literal(X) :- atom(X),!.
mpred_literal(X) :- mpred_negated_literal(X),!.
mpred_literal(X) :- mpred_positive_literal(X),!.
mpred_literal(X) :- is_ftVar(X),!.

*/

%% is_reprop( +X) is semidet.
%
% If Is A Reprop.
%
is_reprop(X):- compound(X),is_reprop_0(X).

%% is_reprop_0( +X) is semidet.
%
% If Is A reprop  Primary Helper.
%
is_reprop_0(~(X)):-!,is_reprop(X).
is_reprop_0(X):-get_functor(X,repropagate,_).


%% mpred_non_neg_literal( +X) is semidet.
%
% PFC Not Negated Literal.
%
mpred_non_neg_literal(X):- is_reprop(X),!,fail.
mpred_non_neg_literal(X):- atom(X),!.
mpred_non_neg_literal(X):- sanity(stack_check),
    mpred_positive_literal(X), X \= ~(_), X \= mpred_mark(_,_,_), X \= conflict(_).

% ======================= mpred_file('pfcsupport').	% support maintenance


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

/*
% TODO not called yet
%= mpred_get_trigger_key(+Trigger,-Key)
%=
%= Arg1 is a trigger.  Key is the best term to index it on.

mpred_get_trigger_key(pt(Key,_),Key).
mpred_get_trigger_key(pk(Key,_,_),Key).
mpred_get_trigger_key(nt(Key,_,_),Key).
mpred_get_trigger_key(Key,Key).
*/

/*

the FOL i get from SUMO, CycL, UMBEL and many *non* RDF ontologies out there.. i convert to Datalog..  evidently my conversion process is unique as it preserves semantics most by the book conversions gave up on. 


% TODO not called yet
%=^L
%= Get a key from the trigger that will be used as the first argument of
%= the trigger baseable clause that stores the trigger.
%=
mpred_trigger_key(X,X) :- is_ftVar(X), !.
mpred_trigger_key(chart(word(W),_L),W) :- !.
mpred_trigger_key(chart(stem([Char1|_Rest]),_L),Char1) :- !.
mpred_trigger_key(chart(Concept,_L),Concept) :- !.
mpred_trigger_key(X,X).
*/
% ======================= mpred_file('pfcdb').	% predicates to manipulate database.

%   File   : pfcdb.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Author :  Dan Corpron
%   Updated: 10/11/87, ...
%   Purpose: predicates to manipulate a pfc database (e.g. save,
%	restore, reset, etc).


baseKB:module_local_init:- mpred_set_default(mpred_warnings(_), mpred_warnings(true)).




%% clause_or_call( +H, ?B) is semidet.
%
% Clause Or Call.
%
clause_or_call(M:H,B):-is_ftVar(M),!,no_repeats(M:F/A,(f_to_mfa(H,M,F,A))),M:clause_or_call(H,B).
clause_or_call(isa(I,C),true):-!,call_u(isa_asserted(I,C)).
clause_or_call(genls(I,C),true):-!,on_x_log_throw(call_u(genls(I,C))).
clause_or_call(H,B):- clause(src_edit(_Before,H),B).
clause_or_call(H,B):- predicate_property(H,number_of_clauses(C)),predicate_property(H,number_of_rules(R)),((R*2<C) -> (clause_u(H,B)*->!;fail) ; clause_u(H,B)).
clause_or_call(H,true):- call_u(should_call_for_facts(H)),no_repeats(on_x_log_throw(H)).


% as opposed to simply using clause(H,true).

%% should_call_for_facts( +H) is semidet.
%
% Should Call For Facts.
%
should_call_for_facts(H):- get_functor(H,F,A),call_u(should_call_for_facts(H,F,A)).

%% should_call_for_facts( +VALUE1, ?F, ?VALUE3) is semidet.
%
% Should Call For Facts.
%
should_call_for_facts(_,F,_):- a(prologSideEffects,F),!,fail.
should_call_for_facts(H,_,_):- modulize_head(H,HH), \+ predicate_property(HH,number_of_clauses(_)),!.
should_call_for_facts(_,F,A):- clause_b(mpred_mark(pfcRHS,F,A)),!,fail.
should_call_for_facts(_,F,A):- clause_b(mpred_mark(pfcMustFC,F,A)),!,fail.
should_call_for_facts(_,F,_):- a(prologDynamic,F),!.
should_call_for_facts(_,F,_):- \+ a(pfcControlled,F),!.



%% no_side_effects( +P) is semidet.
%
% No Side Effects.
%
no_side_effects(P):-  (\+ is_side_effect_disabled->true;(get_functor(P,F,_),a(prologSideEffects,F))).


:- was_dynamic(prologMacroHead/1).


%% compute_resolve( +NewerP, ?OlderQ, ?SU, ?SU, ?OlderQ) is semidet.
%
% Compute Resolve.
%
compute_resolve(NewerP,OlderQ,SU,SU,(mpred_blast(OlderQ),mpred_ain(NewerP,S),mpred_withdraw(conflict(NewerP)))):-
  must(correctify_support(SU,S)),
  wdmsg(compute_resolve(newer(NewerP-S)>older(OlderQ-S))).
compute_resolve(NewerP,OlderQ,S1,[U],Resolve):-compute_resolve(OlderQ,NewerP,[U2],S1,Resolve),match_source_ref1(U),match_source_ref1(U2),!.
compute_resolve(NewerP,OlderQ,SU,S2,(mpred_blast(OlderQ),mpred_ain(NewerP,S1),mpred_withdraw(conflict(NewerP)))):-
  must(correctify_support(SU,S1)),
  wdmsg(compute_resolve((NewerP-S1)>(OlderQ-S2))).



%% compute_resolve( +NewerP, ?OlderQ, ?Resolve) is semidet.
%
% Compute Resolve.
%
compute_resolve(NewerP,OlderQ,Resolve):-
   supporters_list(NewerP,S1),
   supporters_list(OlderQ,S2),
   compute_resolve(NewerP,OlderQ,S1,S2,Resolve).



%% is_resolved( +C) is semidet.
%
% If Is A Resolved.
%
is_resolved(C):- Why= is_resolved, mpred_call_only_facts(Why,C),\+mpred_call_only_facts(Why,~(C)).
is_resolved(C):- Why= is_resolved, mpred_call_only_facts(Why,~(C)),\+mpred_call_only_facts(Why,C).

:- must(nop(_)).


%% mpred_prove_neg( +G) is semidet.
%
% PFC Prove Negated.
%
mpred_prove_neg(G):-nop(dtrace), \+ mpred_bc_only(G), \+ mpred_fact(G).


%% pred_head( :PRED1Type, ?P) is semidet.
%
% Predicate Head.
%
pred_head(Type,P):- no_repeats_u(P,(call(Type,P),\+ nonfact_metawrapper(P),is_ftCompound(P))).


%% pred_head_all( +P) is semidet.
%
% Predicate Head All.
%
pred_head_all(P):- pred_head(pred_all,P).


%% nonfact_metawrapper( :TermP) is semidet.
%
% Nonfact Metawrapper.
%
nonfact_metawrapper(~(_)).
nonfact_metawrapper(pt(_,_)).
nonfact_metawrapper(bt(_,_,_)).
nonfact_metawrapper(nt(_,_)).
nonfact_metawrapper(spft(_,_,_)).
nonfact_metawrapper(added(_)).
% we use the arity 1 forms is why 
nonfact_metawrapper(term_expansion(_,_)).
nonfact_metawrapper(P):- \+ current_predicate(_,P).
nonfact_metawrapper(M:P):-atom(M),!,nonfact_metawrapper(P).
nonfact_metawrapper(P):- get_functor(P,F,_), 
   (a(prologSideEffects,F);a(tNotForUnboundPredicates,F)).
nonfact_metawrapper(P):-rewritten_metawrapper(P).


%% rewritten_metawrapper( +C) is semidet.
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



%% pred_all( +P) is semidet.
%
% Predicate All.
%
pred_all(P):-pred_u0(P).
pred_all(P):-pred_t0(P).
pred_all(P):-pred_r0(P).


%% pred_u0( +P) is semidet.
%
% Predicate For User Code Primary Helper.
%
pred_u0(P):-pred_u1(P),has_db_clauses(P).
pred_u0(P):-pred_u2(P).

%% pred_u1( +VALUE1) is semidet.
%
% Predicate For User Code Secondary Helper.
%
pred_u1(P):-a(pfcControlled,F),arity(F,A),functor(P,F,A).
pred_u1(P):-a(prologHybrid,F),arity(F,A),functor(P,F,A).
pred_u1(P):-a(prologDynamic,F),arity(F,A),functor(P,F,A).

%% pred_u2( +P) is semidet.
%
% Predicate For User Code Extended Helper.
%
pred_u2(P):-support_hilog(F,A),functor(P,F,A),has_db_clauses(P).
pred_u2(P):- clause_b(arity(F,A)),functor(P,F,A),has_db_clauses(P).



%% has_db_clauses( +PI) is semidet.
%
% Has Database Clauses.
%
has_db_clauses(PI):-modulize_head(PI,P),
   predicate_property(P,number_of_clauses(NC)),\+ predicate_property(P,number_of_rules(NC)), \+ \+ clause_u(P,true).



%% pred_t0(+ ?P) is semidet.
%
% Predicate True Stucture Primary Helper.
%
pred_t0(P):-mreq('==>'(P)).
pred_t0(P):-mreq(pt(P,_)).
pred_t0(P):-mreq(bt(P,_)).
pred_t0(P):-mreq(nt(P,_,_)).
pred_t0(P):-mreq(spft(P,_,_)).

%pred_r0(-(P)):- call_u(-(P)).
%pred_r0(~(P)):- mreq(~(P)).


%% pred_r0( :TermP) is semidet.
%
% Predicate R Primary Helper.
%
pred_r0(P==>Q):- mreq(P==>Q).
pred_r0(P<==>Q):- mreq(P<==>Q).
pred_r0(P<-Q):- mreq(P<-Q).


%% cnstrn( +X) is semidet.
%
% Cnstrn.
%
:- module_transparent(cnstrn/1).
cnstrn(X):-term_variables(X,Vs),maplist(cnstrn0(X),Vs),!.

%% cnstrn( +V, ?X) is semidet.
%
% Cnstrn.
%
:- module_transparent(cnstrn/2).
cnstrn(V,X):-cnstrn0(X,V).

%% cnstrn0( +X, ?V) is semidet.
%
% Cnstrn Primary Helper.
%
:- module_transparent(cnstrn0/2).
cnstrn0(X,V):-when(is_ftNonvar(V),X).


%% rescan_pfc is semidet.
%
% Rescan Prolog Forward Chaining.
%
rescan_pfc:-forall(clause(baseKB:mpred_hook_rescan_files,Body),show_entry(rescan_pfc,Body)).


%% mpred_facts_and_universe( +P) is semidet.
%
% PFC Facts And Universe.
%
mpred_facts_and_universe(P):- (is_ftVar(P)->pred_head_all(P);true),call_u(P). % (meta_wrapper_rule(P)->call_u(P) ; call_u(P)).


%% repropagate( :TermP) is semidet.
%
% Repropagate.
%
repropagate(_):-  check_context_module,fail.
%repropagate(P):-  check_real_context_module,fail.

repropagate(P):-  is_ftVar(P),!.
repropagate(P):-  meta_wrapper_rule(P),!,call_u(repropagate_meta_wrapper_rule(P)).
repropagate(P):-  \+ predicate_property(P,_),'$find_predicate'(P,PP),PP\=[],!,forall(member(M:F/A,PP),
                                                          must((functor(Q,F,A),repropagate_1(M:Q)))).
repropagate(F/A):- is_ftNameArity(F,A),!,functor(P,F,A),!,repropagate(P).
repropagate(F/A):- atom(F),is_ftVar(A),!,repropagate(F).

repropagate(P):-  \+ predicate_property(_:P,_),dmsg(undefined_repropagate(P)),dumpST,dtrace,!,fail.
repropagate(P):-  repropagate_0(P).


predicate_to_goal(P,Goal):-atom(P),get_arity(P,F,A),functor(Goal,F,A).
predicate_to_goal(PF/A,Goal):-atom(PF),get_arity(PF,F,A),functor(Goal,F,A).
predicate_to_goal(G,G):-compound(G),!.

%% repropagate_0( +P) is semidet.
%
% repropagate  Primary Helper.
%
repropagate_0(P):- loop_check(call_u(repropagate_1(P)),true).

:- thread_local t_l:is_repropagating/1.


%% repropagate_1( +P) is semidet.
%
% repropagate  Secondary Helper.
%
repropagate_1(P):- is_ftVar(P),!.
repropagate_1(USER:P):- USER==user,!,repropagate_1(P).
%repropagate_1((P/_)):-!,repropagate_1(P).

repropagate_1(P):- call_u(repropagate_2(P)).

:- export(repropagate_2/1).
:- module_transparent(repropagate_2/1).

%% repropagate_2( +P) is semidet.
%
% repropagate  Extended Helper.
%
repropagate_2(P):-
 doall((no_repeats((mpred_facts_and_universe(P))),
    w_tl(t_l:is_repropagating(P),ignore((once(show_failure(fwd_ok(P))),show_call(mpred_fwc(P))))))).

% repropagate_meta_wrapper_rule(P==>_):- !, repropagate(P).

%% repropagate_meta_wrapper_rule( +P) is semidet.
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
fwd_ok(X):-compound(X),consequent_arg(_,X,E),compound(E),functor(E,(:-),_),!.
% fwd_ok(P):-must(ground(P)),!.


%% mpred_facts_only( +P) is semidet.
%
% PFC Facts Only.
%
mpred_facts_only(P):- (is_ftVar(P)->(pred_head_all(P),\+ meta_wrapper_rule(P));true),no_repeats(P).



:- thread_local(t_l:user_abox/1).

:- debug(dmiles).

:- was_dynamic(baseKB:module_local_init/0).
:- discontiguous(baseKB:module_local_init/0).
% :- include('mpred_header.pi').
:- style_check(+singleton).

% TODO READD
%:- foreach(consequent_arg(_,isEach(prologMultiValued,prologOrdered,prologNegByFailure,prologPTTP,prologKIF,pfcControlled,ttPredType,
%     prologHybrid,predCanHaveSingletons,prologDynamic,prologBuiltin,prologMacroHead,prologListValued,prologSingleValued),P),)

%% get

% =================================================
% ==============  UTILS BEGIN        ==============
% =================================================

%% ruleBackward(+R, ?Condition) is semidet.
%
% Rule Backward.
%
ruleBackward(R,Condition):- call_u(( ruleBackward0(R,Condition),functor(Condition,F,_),\+ consequent_arg(_,v(call_u,mpred_bc_only),F))).
%ruleBackward0(F,Condition):-clause_u(F,Condition),\+ (is_true(Condition);mpred_is_info(Condition)).

%% ruleBackward0(+F, ?Condition) is semidet.
%
% Rule Backward Primary Helper.
%
ruleBackward0(F,Condition):- call_u((  '<-'(F,Condition),\+ (is_true(Condition);mpred_is_info(Condition)) )).


% ======================= 
% user''s program''s database
% ======================= 


%% assert_mu(+X) is semidet.
%
% Assert For User Code.
%

assert_mu(MH):- fix_mp(clause(assert,assert_u),MH,M,H),get_consequent_functor(H,F,A),assert_mu(M,H,F,A).
asserta_mu(MH):- fix_mp(clause(assert,assert_u),MH,M,H),asserta_mu(M,H).
assertz_mu(MH):- fix_mp(clause(assert,assert_u),MH,M,H),assertz_mu(M,H).


:- dynamic(baseKB:singleValuedInArg/2).

%% assert_mu(+Module, +Pred, ?Functor, ?Arity) is semidet.
%
% Assert For User Code.
%
assert_mu(M,M2:Pred,F,A):- M == M2,!, assert_mu(M,Pred,F,A).
assert_mu(M,_:Pred,F,A):- dtrace,sanity(\+ is_ftVar(Pred)),!, assert_mu(M,Pred,F,A).
assert_mu(M,Pred,F,_):- call_u(singleValuedInArg(F,SV)),!,must(update_single_valued_arg(M,Pred,SV)),!.
assert_mu(M,Pred,F,A):- a(prologSingleValued,F),!,must(update_single_valued_arg(M,Pred,A)),!.
assert_mu(M,Pred,_,_):- !, assertz_mu(M,Pred).
assert_mu(M,Pred,_,1):- assertz_mu(M,Pred),!.
assert_mu(M,Pred,F,_):- a(prologOrdered,F) -> assertz_mu(M,Pred) ; asserta_mu(M,Pred).

%assert_mu(M,Pred,F,_):- a(prologAssertAOrdered,F) -> asserta_mu(M,Pred) ; assertz_mu(M,Pred).
%assert_mu(M,Pred,_,_):- assertz_mu(M,Pred).

:-thread_local(t_l:side_effect_ok/0).


%% assertz_mu(+M, ?X) is semidet.
%
% Assertz Module Unit.
%
%assertz_mu(abox,X):-!,defaultAssertMt(M),!,assertz_mu(M,X).
%assertz_mu(M,X):- check_never_assert(M:X), clause_asserted_u(M:X),!.
% assertz_mu(M,X):- correct_module(M,X,T),T\==M,!,assertz_mu(T,X).
% assertz_mu(_,X):- must(defaultAssertMt(M)),!,must((expire_tabled_list(M:X),show_call(attvar_op(assertz_i,M:X)))).
assertz_mu(M,X):- strip_module(X,_,P), check_never_assert(M:P), 
   (clause_asserted_u(M:P)-> true; must((expire_tabled_list(M:P),show_call(attvar_op(assertz_i,M:P))))).

%% asserta_mu(+M, ?X) is semidet.
%
% Asserta Module Unit.
%
%asserta_mu(abox,X):-!,defaultAssertMt(M),!,asserta_mu(M,X).
% asserta_mu(M,X):- correct_module(M,X,T),T\==M,!,asserta_mu(T,X).
% asserta_mu(_,X):- must(defaultAssertMt(M)),!,must((expire_tabled_list(M:X),show_call(attvar_op(asserta_i,M:X)))).

asserta_mu(M,X):- strip_module(X,_,P), check_never_assert(M:P), 
   (clause_asserted_u(M:P)-> true; must((expire_tabled_list(M:P),show_call(attvar_op(asserta_i,M:P))))).



%% retract_mu( :TermX) is semidet.
%
% Retract For User Code.
%
% retract_mu(que(X,Y)):-!,show_failure(why,retract_eq_quitely_f(que(X,Y))),must((expire_tabled_list(~(X)))),must((expire_tabled_list((X)))).
retract_mu(H0):- throw_depricated, strip_module(H0,_,H),defaultAssertMt(M),show_if_debug(attvar_op(retract_i,M:H)),!,must((expire_tabled_list(H))).
retract_mu(X):- check_never_retract(X),fail.
retract_mu(~(X)):-!,show_success(why,retract_eq_quitely_f(~(X))),must((expire_tabled_list(~(X)))),must((expire_tabled_list((X)))).
retract_mu((X)):-!,show_success(why,retract_eq_quitely_f((X))),must((expire_tabled_list(~(X)))),must((expire_tabled_list((X)))).
retract_mu(M:(H:-B)):- atom(M),!, clause_u(H,B,R),erase(R).
retract_mu((H:-B)):-!, clause_u(H,B,R),erase(R).
%retract_mu(~(X)):-must(is_ftNonvar(X)),!,retract_eq_quitely_f(~(X)),must((expire_tabled_list(~(X)))),must((expire_tabled_list((X)))).
%retract_mu(hs(X)):-!,retract_eq_quitely_f(hs(X)),must((expire_tabled_list(~(X)))),must((expire_tabled_list((X)))).





:- retractall(t_l:mpred_debug_local).

:- thread_local(t_l:in_rescan_mpred_hook/0).

/*

% :- logicmoo_util_shared_dynamic:listing(ereq/1).

*/

% =================================================
% ==============  UTILS END          ==============
% =================================================
:- module_transparent( (retract_mu)/1).
:- module_transparent( (assert_mu)/4).
:- module_transparent( (asserta_mu)/1).
:- module_transparent( (ruleBackward0)/2).
:- module_transparent( (ruleBackward)/2).
:- module_transparent( (fwd_ok)/1).
:- module_transparent( (repropagate_meta_wrapper_rule)/1).
:- module_transparent( (repropagate_1)/1).
:- module_transparent( (repropagate_0)/1).
:- module_transparent( (repropagate)/1).
:- module_transparent( (mpred_facts_and_universe)/1).
:- module_transparent( (rescan_pfc)/0).
:- module_transparent( (pred_r0)/1).
:- module_transparent( (pred_t0)/1).
:- module_transparent( (has_db_clauses)/1).
:- module_transparent( (pred_u2)/1).
:- module_transparent( (pred_u1)/1).
:- module_transparent( (pred_u0)/1).
:- module_transparent( (pred_all)/1).
:- module_transparent( (meta_wrapper_rule)/1).
:- module_transparent( (rewritten_metawrapper)/1).
:- module_transparent( (nonfact_metawrapper)/1).
:- module_transparent( (pred_head_all)/1).
:- module_transparent( (mpred_prove_neg)/1).
:- module_transparent( (is_resolved)/1).
:- module_transparent( (compute_resolve)/3).
:- module_transparent( (compute_resolve)/5).
:- module_transparent( (no_side_effects)/1).
:- module_transparent( (should_call_for_facts)/3).
:- module_transparent( (should_call_for_facts)/1).
:- module_transparent( (clause_or_call)/2).
:- module_transparent( (is_relative)/1).
:- module_transparent( (mpred_non_neg_literal)/1).
:- module_transparent( (is_reprop_0)/1).
:- module_transparent( (is_reprop)/1).
:- module_transparent( (mpred_literal_nv)/1).
:- module_transparent( (mpred_is_assertable)/1).
:- module_transparent( (mpred_cleanup_0)/1).
:- module_transparent( (mpred_cleanup)/2).
:- module_transparent( (mpred_cleanup)/0).
:- module_transparent( (mpred_negation_w_neg)/2).
:- module_transparent( (has_cl)/1).
:- module_transparent( (mpred_ignored)/1).
:- module_transparent( (maybeSupport)/2).
:- module_transparent( (pfcBC_Cache)/1).
:- module_transparent( (pfcBC_NoFacts_TRY)/1).
:- module_transparent( (mpred_slow_search)/0).
:- module_transparent( (pfcBC_NoFacts)/1).
:- module_transparent( (mpred_bc_only0)/1).
:- module_transparent( (mpred_bc_only)/1).
:- module_transparent( (mpred_call_with_no_triggers_uncaugth)/1).
:- module_transparent( (mpred_call_with_no_triggers_bound)/1).
:- module_transparent( (mpred_call_with_no_triggers)/1).
:- module_transparent( (call_with_bc_triggers)/1).
:- module_transparent( (mpred_call_1)/3).
:- module_transparent( (mpred_call_0)/1).
:- module_transparent( (mpred_call_only_facts)/2).
:- module_transparent( (mpred_call_only_facts)/1).
:- module_transparent( (call_u_req)/1).
:- module_transparent( (neg_in_code)/1).
:- module_transparent( ({})/1).
:- module_transparent( (trigger_supporters_list)/2).
:- module_transparent( (spft_precanonical)/3).
:- module_transparent( (mpred_get_support_precanonical)/2).
:- module_transparent( (support_ok_via_clause_body)/3).
:- module_transparent( (support_ok_via_clause_body)/1).
:- module_transparent( (mpred_get_support_via_clause_db)/2).
:- module_transparent( (mpred_get_support_via_sentence)/2).
:- module_transparent( (mpred_get_support_one)/2).
:- module_transparent( (mpred_get_support_precanonical_plus_more)/2).
:- module_transparent( (mpred_deep_support0)/2).
:- module_transparent( (mpred_deep_support)/2).
:- module_transparent( (user_atom)/1).
:- module_transparent( (mpred_scan_tms)/1).
:- module_transparent( (well_founded)/2).
:- module_transparent( (mpred_tms_supported0)/3).
:- module_transparent( (mpred_tms_supported)/3).
:- module_transparent( (mpred_tms_supported)/2).
:- module_transparent( (mpred_remove_file_support)/1).
:- module_transparent( (without_running)/1).
:- module_transparent( (mpred_run_resume)/0).
:- module_transparent( (mpred_run_pause)/0).
:- module_transparent( (which_missing_argnum)/2).
:- module_transparent( (if_missing_mask)/4).
:- module_transparent( (if_missing_mask)/3).
:- module_transparent( (is_already_supported)/3).
:- module_transparent( (clause_asserted_local)/1).
:- module_transparent( (correctify_support)/2).
:- module_transparent( (pfcVersion)/1).
:- module_transparent( (mpred_current_op_support)/1).
:- module_transparent( (mpred_freeLastArg)/2).
:- module_transparent( (pfcVerifyMissing)/3).
:- module_transparent( (map_first_arg)/3).
:- module_transparent( (map_first_arg)/2).
:- module_transparent( (map_literals)/3).
:- module_transparent( (map_literals)/2).
:- module_transparent( (update_single_valued_arg)/3).
:- module_transparent( (has_body_atom)/2).
:- module_transparent( (is_action_body)/1).
:- module_transparent( (is_bc_body)/1).
:- module_transparent( (is_fc_body)/1).
:- module_transparent( (wac)/0).
:- module_transparent( (bwc)/0).
:- module_transparent( (fwc)/0).
:- module_transparent( (cwc)/0).
:- module_transparent( (mpred_rewrap_h)/2).
:- module_transparent( (mpred_is_info)/1).
:- module_transparent( (ain_minfo)/1).
:- module_transparent( (mpred_rule_hb_0)/3).
:- module_transparent( (mpred_rule_hb)/3).
:- module_transparent( (all_different_head_vals_2)/2).
:- module_transparent( (all_different_head_vals)/1).
:- module_transparent( (sub_term_v)/2).
:- module_transparent( (sub_term_eq)/2).
:- module_transparent( (mpred_pbody_f)/5).
:- module_transparent( (get_why)/4).
:- module_transparent( (mpred_pbody)/5).
:- module_transparent( (pfc_provide_storage_op)/2).
:- module_transparent( (is_retract_first)/1).
:- module_transparent( (mpred_is_taut)/1).
:- module_transparent( (mpred_is_tautology)/1).
:- module_transparent( (assert_eq_quitely)/1).
:- module_transparent( (retract_eq_quitely_f)/1).
:- module_transparent( (retract_eq_quitely)/1).




:- module_transparent( (mpred_each_literal)/2).
:- module_transparent( (has_functor)/1).
:- module_transparent( (make_uu_remove)/1).
:- module_transparent( (match_source_ref1)/1).
:- module_transparent( (mpred_nochaining)/1).
:- module_transparent( (erase_w_attvars)/2).
:- module_transparent( (call_s2)/1).
:- module_transparent( (call_s)/1).
:- module_transparent( (clauseq_s)/3).
:- module_transparent( (clause_s)/3).
:- module_transparent( (assertz_s)/1).
:- module_transparent( (asserta_s)/1).
:- module_transparent( (lookq_s)/2).
:- module_transparent( (lookq_s)/1).
:- module_transparent( (lookup_s)/2).
:- module_transparent( (lookup_s)/1).
:- module_transparent( (retract_s)/1).
:- module_transparent( (clause_s)/2).
:- module_transparent( (retractall_s)/1).
:- module_transparent( (assert_s)/1).
:- module_transparent( (listing_s)/1).
:- module_transparent( (add_side_effect)/2).
:- module_transparent( (record_se)/0).
:- module_transparent( (mpred_is_builtin)/1).
:- module_transparent( (is_mpred_action)/1).
:- module_transparent( (set_prolog_stack_gb)/1).
:- module_transparent( (w_get_fa)/3).
:- module_transparent( (f_to_mfa)/4).
:- module_transparent( (is_side_effect_disabled)/0).
:- module_transparent( (mreq)/1).
:- module_transparent( (check_real_context_module)/0).
:- module_transparent( (check_context_module)/0).
:- module_transparent( (lookup_inverted_op)/3).
:- module_transparent( (how_to_op)/2).
:- module_transparent( (reduce_mpred_op)/2).
:- module_transparent( (second_order)/2).
:- module_transparent( (assert_mu)/1).
:- module_transparent( (assertz_mu)/2).
:- module_transparent( (assertz_mu)/1).
:- module_transparent( (mpred_op)/2).
:- module_transparent(deducedSimply/1).
:- module_transparent(is_callable/1).
:- module_transparent(map_unless/4).
:- module_transparent(mpred_facts_only/1).
:- module_transparent(mpred_retry/1).
:- module_transparent(mpred_update_literal/4).
:- module_transparent(naf/1).
:- module_transparent(oncely/1).
:- module_transparent(physical_side_effect/1).
:- module_transparent(pred_head/2).
:- module_transparent(if_missing/1).


 :- meta_predicate update_single_valued_arg(+,+,*).
 :- meta_predicate assert_mu(*,+,*,*).
 :- meta_predicate mpred_facts_and_universe(0).
 :- meta_predicate {0}.
 :- meta_predicate repropagate_2(0).
 :- meta_predicate mpred_get_support_via_sentence(0,*).

% :- shared_multifile(infoF/1).
:- dynamic(system:infoF/1).
:- export(system:infoF/1).


mpred_kb_ops_file.

% % :- '$set_source_module'(mpred_kb_ops).




