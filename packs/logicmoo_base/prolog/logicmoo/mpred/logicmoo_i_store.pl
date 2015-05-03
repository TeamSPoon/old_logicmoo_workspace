/** <module> 
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

:-export(( (add)/1, clr/1,fully_expand/3,ireq/1,del/1,  
  padd/2, padd/3, prop/3, prop_or/4, props/2, iprops/2, upprop/2,add/1, ireq/1, mreq/1, upprop/1, req/1, 
  % use_term_listing/2,  
  world_clear/1,  
   with_kb_assertions/2)).

compound_functor(Compound,F):-compound(Compound),nonvar(Compound),get_functor(Compound,F).

not_variant(G,GG):-
 not(not((
  %numbervars(G,0,_),
  %numbervars(GG,0,_),
  G\=@=GG))).

% ========================================
% Shared Preds
% ========================================
:- include(logicmoo_i_header).

% TODO: canonicalize clauses first!
with_kb_assertions([],Call):- !,Call.
with_kb_assertions([With|MORE],Call):-!,with_kb_assertions(With,with_kb_assertions(MORE,Call)).
with_kb_assertions(With,Call):-
   setup_call_cleanup(asserta(With,Ref),Call,erase(Ref)).


world_clear(Named):-fmt('Clearing world database: ~q.~n',[Named]).



get_pifunctor(Head,PHead):-must(get_pifunctor(Head,PHead,_,_)).
get_pifunctor(Head,PHead,F):-must(get_pifunctor(Head,PHead,F,_)).

get_pifunctor(Head,PHead,F,A):-var(Head),!,sanity(atom(F)),must(ensure_arity(F,A)),functor(PHead,F,A),ignore(PHead=Head).
get_pifunctor(Head,PHead,F,A):-get_functor(Head,F,A),functor(PHead,F,A),ignore(PHead=Head),!.
get_pifunctor(Head,PHead,F,A):-atom(Head),ensure_arity(Head,A),!,get_pifunctor(Head/A,PHead,F,A).



side_effect_prone:- \+ thlocal:noDBaseMODs(_).



:-meta_predicate(with_no_modifications(0)).
with_no_modifications(CALL):-!,CALL.
with_no_modifications(CALL):-with_assertions(thlocal:noDBaseMODs(_),CALL).

:-meta_predicate(with_no_db_hooks(0)).
with_no_db_hooks(CALL):-!,CALL.
with_no_db_hooks(CALL):-with_assertions(thlocal:noDBaseHOOKS(_),CALL).

:-meta_predicate(with_fallbacks(0)).
with_fallbacks(CALL):-with_no_assertions(thlocal:infAssertedOnly(_),CALL).

:-meta_predicate(with_fallbacksg(0)).
with_fallbacksg(CALL):-with_no_assertions(thlocal:noRandomValues(_),CALL).

:-meta_predicate(with_no_fallbacksg(0)).
with_no_fallbacksg(CALL):-with_assertions(thlocal:noRandomValues(_),CALL).

:-meta_predicate(with_no_fallbacks(0)).
with_no_fallbacks(CALL):-with_assertions(thlocal:infAssertedOnly(_),CALL).

:-thread_local(infSecondOrder/0).
infSecondOrder :- not(thlocal:infInstanceOnly(_)).

:-thread_local(infThirdOrder/0).
infThirdOrder :- fail, infSecondOrder, not(thlocal:noRandomValues(_)).


% ================================================
% is_asserted/1/2/3
% ================================================
:- thread_local thlocal:fail_is_asserted/1.

with_fail_is_asserted(Temp,Goal):-ground(Temp),!,Goal.
with_fail_is_asserted(Temp,Goal):-with_assertions(thlocal:fail_is_asserted(Temp),Goal).

not_asserted(X):- !,(\+ clause(X,true)).
not_asserted(X):- not(no_loop_check(is_asserted_ilc(X))).
is_asserted_eq(HB):- ( \+ \+ no_loop_check(is_asserted_ilc(HB))).

is_asserted(X):- no_repeats(loop_check(is_asserted_ilc(X))).
is_asserted(X,Y):- no_repeats(loop_check(is_asserted_ilc(X,Y))).
is_asserted(X,Y,Z):- no_repeats(loop_check(is_asserted_ilc(X,Y,Z))).

is_asserted_ilc(V):-var(V),!,trace_or_throw(var_is_asserted(V)).
% TODO: test removal
%is_asserted_ilc(prologHybrid(H)):-get_functor(H,F),!,isa_asserted(F,prologHybrid).
is_asserted_ilc((H)):- is_static_pred(H),!,show_pred_info(H),dtrace(is_asserted_ilc((H))).
is_asserted_ilc(HB):-hotrace((fully_expand_warn(is_asserted_1,HB,HHBB))),!,is_asserted_1(HHBB).

%is_asserted_1(argIsa(mpred_prop,2,mpred_prop/2)):- dtrace,!,fail.
is_asserted_1(clause(H,B,Ref)):-!,is_asserted_ilc(H,B,Ref).
is_asserted_1(clause(H,B)):-!,is_asserted_ilc(H,B).
is_asserted_1((H1,H2)):-!,is_asserted_1(H1),is_asserted_1(H2).
is_asserted_1((H1;H2)):-!,is_asserted_1(H1);is_asserted_1(H2).
% TODO: test removal
% is_asserted_1(isa(H,B)):-!,isa_asserted(H,B).
is_asserted_1(HB):-expand_to_hb(HB,H,B),!,is_asserted_ilc(H,B).

is_asserted_ilc((H:-BB),B):- is_true(B),!,is_asserted_ilc(H,BB).
is_asserted_ilc(H,B):-hotrace((fully_expand_warn(is_asserted_2,(H:-B),CL),expand_to_hb(CL,HH,BB))),!,is_asserted_2(HH,BB).

is_asserted_2(H,B):-thglobal:pfcManageHybrids,!,pfc_clause_is_asserted(H,B).
is_asserted_2(H,B):-call_no_cuts(user:provide_mpred_storage_clauses(H,B,_Ref)),not(hotrace(special_wrapper_body(B,_))).

is_asserted_ilc((H:-BB),B,Ref):- is_true(B),!,is_asserted_ilc(H,BB,Ref).
is_asserted_ilc(H,B,Ref):-hotrace((fully_expand_warn(is_asserted_3,(H:-B),CL),expand_to_hb(CL,HH,BB))),is_asserted_3(HH,BB,Ref).

% is_asserted_3(H,B,Ref):-thglobal:pfcManageHybrids,!,pfc_clause_is_asserted(H,B,Ref).
is_asserted_3(H,B,Ref):-call_no_cuts(user:provide_mpred_storage_clauses(H,B,Ref)),not(hotrace(special_wrapper_body(B,_))).

is_source_proof(_).


% ================================================
% fact_checked/2, fact_loop_checked/2
% ================================================
:- meta_predicate(fact_checked(?,0)).

fact_checked(Fact,Call):- not(ground(Fact)),!,no_loop_check(call_tabled(Call),is_asserted(Fact)).
fact_checked(Fact,_):- is_known_false0(Fact),!,fail.
fact_checked(Fact,_):- is_known_trew(Fact),!.
fact_checked(Fact,Call):- no_loop_check(call_tabled(Call),is_asserted(Fact)).

:-meta_predicate(fact_loop_checked(+,0)).
fact_loop_checked(Fact,Call):- no_repeats(fact_checked(Fact,Call)).



% ================================================
% hooked_assert/1 hooked_retract/1
% ================================================
ensure_predicate_reachable(_,_):- fast_mud,!.
ensure_predicate_reachable(M,C):-functor(C,F,A),ensure_predicate_reachable(M,C,F,A),fail.
ensure_predicate_reachable(_,_):- is_release,!.
ensure_predicate_reachable(M,C):-once((predicate_property(C,imported_from(Other)),M\=Other,
                                       context_module(CM),
                                       dmsg(wrong_import_module(M,Other:C,from(CM))),
                                       ignore(delete_import_module(CM,Other)),
                                       '@'((M:dynamic(C),M:export(C)),M),user:import(M:C))),fail.
ensure_predicate_reachable(_,_).


singletons_throw_else_fail(C):- fail,not_is_release,contains_singletons(C),!,(test_tl(thlocal:already_in_file_term_expansion) -> (dmsg(contains_singletons(C))); dmsg(trace_or_throw(contains_singletons(C)))),fail.
nonground_throw_else_fail(C):- not_is_release,not(ground(C)),!,( (test_tl(thlocal:already_in_file_term_expansion) ->dmsg(not_ground(C)); trace_or_throw(not_ground(C)))),fail.

% ================================================
% mpred_modify/2
% ================================================

with_logical_functor(not,[G],Call):- !, not(call(Call,G)).
with_logical_functor(_And,[G],Call):- !, call(Call,G).
with_logical_functor(And,[G|T],Call):-
   DO =..[And,call(Call,G),with_logical_functor(And,T,Call)],
   call(DO).


requires_storage((Head :- Body),Why):- nonvar(Head),!, requires_storage(Head,Body,Why).
requires_storage(C,Why):- requires_storage(C,true,Why).

requires_storage(G,_,Why):-get_functor(G,F),!,special_head(G,F,Why),!.
requires_storage(_,_,thlocal:consulting_sources):-thlocal:consulting_sources,pfc_may_expand,!.
% requires_storage(_,_,thlocal:consulting_sources):-thlocal:consulting_sources,in_file_expansion.

special_wrapper_functor(call_mpred_body,direct_to_prolog).
special_wrapper_functor(body_req,direct_to_prolog).
special_wrapper_functor(user:provide_mpred_setup,direct_to_prolog).
special_wrapper_functor(call_provided_mpred_storage_op,direct_to_prolog).
special_wrapper_functor(loop_check,meta).
special_wrapper_functor(loop_check_term,meta).
%special_wrapper_functor(pttp_req).


make_body_clause(_Head,Body,Body):-atomic(Body),!.
make_body_clause(_Head,Body,Body):-special_wrapper_body(Body,_Why),!.
make_body_clause(Head,Body,call_mpred_body(Head,Body)).

special_head(_,F,Why):-special_head0(F,Why),!,show_call_failure(not(isa(F,prologOnly))).
special_head0(F,ttPredType):-ttPredType(F),!.
special_head0(F,functorDeclares):-t(functorDeclares,F),!.
special_head0(F,prologMacroHead):-t(prologMacroHead,F),!.
special_head0(F,pfcControlled):-t(pfcControlled,F),!.
special_head0(isa,isa).
special_head0(F,tCol):-t(tCol,F),!.
special_head0(F,prologHybrid):-t(prologHybrid,F).
special_head0(F,pfcControlled):-t(pfcControlled,F).



special_wrapper_body(W,Why):-get_body_functor(W,F,_),!,special_wrapper_functor(F,Why).

get_body_functor(Var,_,call):-var(Var),!.
get_body_functor((M:BDY),BF,A):-atom(M),!,get_body_functor(BDY,BF,A).
get_body_functor((!,BDY),BF,A):-!,get_body_functor(BDY,BF,A).
get_body_functor(call(BDY),BF,A):-!,get_body_functor(BDY,BF,A).
get_body_functor(once(BDY),BF,A):-!,get_body_functor(BDY,BF,A).
get_body_functor((BDY1;BDY2),BF,A):-!, (get_body_functor(BDY1,BF,A);get_body_functor(BDY2,BF,A)).
get_body_functor((BDY1,BDY2),BF,A):-!, (get_body_functor(BDY1,BF,A);get_body_functor(BDY2,BF,A)).
get_body_functor(BDY,BF,A):-get_functor(BDY,BF,A).


% ================================================
% CHECKED 
% ================================================


% -  del(RetractOne) 
del(C):- fully_expand(change(retract,a),C,C0),pfc_maptree(del0,C0).
del0(C0):- mpred_call(C0),!,clr(C0),!.
del0(C0):- ireq(C0),!,idel(C0),!.
del0(C0):- mreq(C0),!,mdel(C0),!.

idel(C0):- dmsg(idel(C0)),mpred_modify(change( retract,a),C0), verify_sanity(ireq(C0)->(dmsg(warn(incomplete_I_DEL(C0))),fail);true),!.
idel(C0):- dmsg(warn(failed(idel(C0)))),!,fail.

mdel(C0):- dmsg(mdel(C0)),mpred_modify(change( retract,one),C0), verify_sanity(mreq(C0)->(dmsg(warn(incomplete_M_DEL(C0))),fail);true),!.
mdel(C0):- dmsg(warn(failed(mdel(C0)))),!,fail.

% -  clr(Retractall)
% clr(C0):- dmsg(clr(C0)),fail,mpred_modify(change(retract,all),/*to_exp*/(C0)),verify_sanity(ireq(C0)->(dmsg(warn(incomplete_CLR(C0))));true).
clr(P):- agenda_do_prequery,
  fully_expand(change(retract,all),P,PL),pfc_maptree(clr0,PL).

clr0(P):- 
  forall(debugOnError(P), forall( pfc_rem2(P), sanity((not(pfc_tms_supported(local,P)),must(\+(P)))))).


% -  preq(Query) = query with P note
preq(P,C0):- must(not((atom(C0)))),agenda_do_prequery,no_repeats(mpred_op(query(t,P),C0)).

% -  req(Query) = Normal query
req(C0):- nop(dmsg(req(C0))), preq(req,/*to_exp*/(C0)).

% -  mreq(Query) = Forced Full query
mreq(C0):- nop(dmsg(mreq(C0))), agenda_rescan_for_module_ready,
   no_loop_check(with_assertions([-infInstanceOnly(_),-thlocal:infAssertedOnly(_),-thlocal:noRandomValues(_)],
     preq(must,/*to_exp*/(C0)))).

% -  ireq(Query) = Normal query (May not use second order logic) (must be asserted on isntance) (used mainly by 2nd order logic to avoid looping)
ireq(C0):- nop(dmsg(ireq(C0))), 
  agenda_rescan_for_module_ready,
   no_loop_check(with_assertions([+infInstanceOnly(_), +thlocal:infAssertedOnly(_),+thlocal:noRandomValues(_)],preq(ireq,/*to_exp*/(C0)))).

:-dmsg_hide(req).
:-dmsg_hide(ireq).

% -  props(Obj,QueryPropSpecs)
props(Obj,PropSpecs):- req(props(Obj,PropSpecs)).
iprops(Obj,PropSpecs):- ireq(/*to_exp*/(props(Obj,PropSpecs))).



:-export(forall_setof/2).
forall_setof(ForEach,Call):-
   findall(ForEach,ForEach,ForEachAll),
   list_to_set(ForEachAll,Set),!,
   ignore(forall(member(ForEach,Set),Call)).


:-thread_local add_thread_override/1.
% thlocal:add_thread_override(A):-add_from_macropred(A),!.

:-export(((add)/1)).
:-moo_hide_childs((add)/1).
add(A):- var(A),!,trace_or_throw(var_add(A)).
add(end_of_file):-!.
add(grid_key(KW=COL)):- !, add(typeHasGlyph(COL,KW)).
% add(Term):- unnumbervars(Term,TermE), Term \=@= TermE,!,add(TermE).
add(Term):- expands_on(isEach,Term), !,forall(do_expand_args(isEach,Term,O),add(/*to_exp*/(O))),!.
add(TermIn):- fully_expand(change(assert,add),TermIn,Term),add_0(Term).

add_0(A):- is_ftVar(A),!,trace_or_throw(var_add(A)).
add_0(((H1,H2):-B)):-!,add_0((H1:-B)),add_0((H2:-B)).
add_0(((H1,H2))):-!,add_0((H1)),add_0((H2)).
add_0(dynamic(Term)):- !,must(get_arity(Term,F,A)), must(dynamic(F/A)).
add_0(A):- A =(:-(Term)), !, must(add_fast(A)).
% add_0(C0):-check_override(add(C0)),!.
% add_0(Skipped):- ground(Skipped),implied_skipped(Skipped),!. % ,dmsg(implied_skipped(Skipped)).
add_0(C0):- ignore((ground(C0),asserta(user:already_added_this_round(C0)))),must(pfc_add_fast(C0)),!.
add_0(A):-trace_or_throw(fmt('add/1 is failing ~q.',[A])).


implied_skipped(genls(C0,C0)).
implied_skipped(props(_,[])).
implied_skipped(Skipped):-compound(Skipped), not(functor(Skipped,_,1)),fail, (t(Skipped);out_of_mpred_t(Skipped)).
implied_skipped(Skipped):-user:already_added_this_round(Skipped),(is_asserted(Skipped)).


mpred_numbervars_with_names(Term):- term_variables(Term,Vars),mpred_name_variables(Vars),!,numbervars(Vars,91,_,[attvar(skip),singletons(true)]),!.

mpred_name_variables([]).
mpred_name_variables([Var|Vars]):-
   (var_property(Var, name(Name)) -> Var = '$VAR'(Name) ; true),
   mpred_name_variables(Vars).


:-export(pfc_add_fast/1).
% -  add(Assertion)
% pfc_add_fast(C0):- must_det((pfc_add_fast(C0), xtreme_debug(once(ireq(C0);(with_all_dmsg((debug(blackboard),show_call(pfc_add_fast(C0)),rtrace(pfc_add_fast(C0)),dtrace(ireq(C0))))))))),!.
add_fast(Term):-mpred_numbervars_with_names(Term),mpred_modify(change(assert,add), Term),!. % ,xtreme_debug(ireq(C0)->true;dmsg(warn(failed_ireq(C0)))))),!.

% -  upprop(Obj,PropSpecs) update the properties
upprop(Obj,PropSpecs):- upprop(props(Obj,PropSpecs)).
upprop(C0):- add(C0).
% -  padd(Obj,Prop,Value)
padd(Obj,PropSpecs):- add((props(Obj,PropSpecs))).
% -  padd(Obj,Prop,Value)
padd(Obj,Prop,Value):- add((t(Prop,Obj,Value))).
% -  props(Obj,Prop,Value)
prop(Obj,Prop,Value):- req(t(Prop,Obj,Value)).
% -  prop_or(Obj,Prop,Value,OrElse)
prop_or(Obj,Prop,Value,OrElse):- one_must(ireq(t(Prop,Obj,Value)),Value=OrElse).



% ================================================
% db_assert_sv/3
% ================================================

:-dmsg_hide(db_assert_sv).

:-dmsg_hide(mpred_modify).
:-dmsg_hide(add).

:-dmsg_hide(into_mpred_form).



% assert_with to change(CA1,CB2) singlevalue pred
:-export((db_assert_sv/4)).
%db_assert_sv(_Must,C,F,A):- throw_if_true_else_fail(contains_singletons(C),db_assert_sv(C,F,A)).
db_assert_sv(Must,C,F,A):- trace,ex, ignore(( loop_check(db_assert_sv_ilc(Must,C,F,A),true))).

:-export((db_assert_sv_ilc/4)).
db_assert_sv_ilc(Must,C,F,A):- arg(A,C,UPDATE),db_assert_sv_now(Must,C,F,A,UPDATE),!.

:-export(db_assert_sv_now/5).
db_assert_sv_now(Must,C,F,A, UPDATE):- has_free_args(db_assert_sv_now(Must,C,F,A, UPDATE)),!,trace_or_throw(var_db_assert_sv_now(Must,C,F,A, UPDATE)).
db_assert_sv_now(Must,C,F,A, NEGREPLACE):- number(NEGREPLACE),NEGREPLACE<0, !,db_assert_sv_replace(Must,C,F,A,NEGREPLACE).
db_assert_sv_now(Must,C,F,A, -(+(UPDATE))):-!,db_assert_sv_update(Must,C,F,A,-UPDATE).
db_assert_sv_now(Must,C,F,A,    +UPDATE):-!,  db_assert_sv_update(Must,C,F,A,+UPDATE).
db_assert_sv_now(Must,C,F,A, +(-(UPDATE))):-  db_assert_sv_update(Must,C,F,A,-UPDATE).
db_assert_sv_now(Must,C,F,A, REPLACE):- db_assert_sv_replace(Must,C,F,A, REPLACE).

:-export(db_assert_sv_update/5).
db_assert_sv_update(Must,C,F,A,UPDATE):-
   replace_arg(C,A,OLD,COLD),
   % prefer updated values to come from instances but will settle with anything legal
   hotrace(must((once(ireq(COLD);mreq(COLD)),ground(COLD)))),
   update_value(OLD,UPDATE,NEW),!,
   db_assert_sv_replace(Must,C,F,A,NEW),!.

:-export(db_assert_sv_replace/5).

:-style_check(-singleton).
% db_assert_sv_replace_noisey_so_disabled
db_assert_sv_replace(_Must,C,_,A,NEW):- fail,
   replace_arg(C,A,_,CBLANK),
   hooked_retractall(CBLANK),
   replace_arg(C,A,NEW,CNEW),
   db_must_asserta_confirmed_sv(CNEW,A,NEW),!.

db_assert_sv_replace(Must,C,F,A,NEW):-
   replace_arg(C,A,OLD,COLD),
   replace_arg(C,A,NEW,CNEW),
   hotrace(ignore(ireq(COLD))),
   must_det(db_assert_sv_replace_with(Must,C,F,A,COLD,CNEW,OLD,NEW)),!.

db_assert_sv_replace_with(Must,C,F,A,COLD,CNEW,OLD,NEW):- var(OLD),!,   
   dmsg(db_assert_sv(COLD,'__add__',CNEW)),
   % replace_arg(C,A,_,CBLANK),hooked_retractall(CBLANK),
   db_must_asserta_confirmed_sv(CNEW,A,NEW),!.

db_assert_sv_replace_with(Must,C,F,A,COLD,CNEW,OLD,NEW):- OLD =@= NEW,!.
db_assert_sv_replace_with(Must,C,F,A,COLD,CNEW,OLD,NEW):- unify_with_occurs_check(OLD,NEW),!,dmsg(db_assert_sv_same(COLD,'__unify_with_occurs_check__',CNEW)).
db_assert_sv_replace_with(Must,C,F,A,COLD,CNEW,OLD,NEW):- equals_call(OLD,NEW),!,dmsg(db_assert_sv_same(COLD,'__same__',CNEW)),trace_or_throw(dtrace).
db_assert_sv_replace_with(Must,C,F,A,COLD,CNEW,OLD,NEW):-
   dmsg(db_assert_sv(COLD,'__replace__',CNEW)),
   hotrace((ignore(show_call_failure((clr(COLD), not(ireq(COLD))))))),
   %replace_arg(C,A,_,CBLANK),must_det(clr(CBLANK)),hooked_retractall(CBLANK),   
   db_must_asserta_confirmed_sv(CNEW,A,NEW),!.

:-style_check(+singleton).


equals_call(X,Y):-unify_with_occurs_check(X,Y),!.
equals_call(X,Y):-once((any_to_string(X,XX),any_to_string(Y,YY))),unify_with_occurs_check(XX,YY),!.
equals_call(X,Y):-once((to_word_list(X,XX),to_word_list(Y,YY))),unify_with_occurs_check(XX,YY),!.
equals_call(X,Y):-compound(X),compound(Y),once((correctArgsIsa(X,XX),correctArgsIsa(Y,YY))),unify_with_occurs_check(XX,YY),!.

confirm_hook(CNEW:NEW=@=CNOW:NOW):-
   sanity(var(NOW)),               
   hotrace((once(ireq(CNOW)))),
   CNEW:NEW=@=CNOW:NOW,!.

confirm_hook(CNEW:NEW=@=CNOW:NOW):-
   dmsg(warn(failed_i_a_req(CNOW,expected(CNEW)))),   
   dtrace((verify_sanity((mreq(CNOW),(CNEW:NEW=@=CNOW:NOW))))),!.



% Expect CNEW to be what is found
db_must_asserta_confirmed_sv(CNEW,A,NEW):-
   replace_arg(CNEW,A,NOW,CNOW),
   sanity(not(singletons_throw_else_fail(CNEW))),
   mpred_modify(change(assert,sv),CNEW),!,
   verify_sanity(confirm_hook(CNEW:NEW=@=CNOW:NOW)),!.

db_must_asserta_confirmed_sv(CNEW,A,NEW):-dmsg(unconfirmed(db_must_asserta_confirmed_sv(CNEW,A,NEW))).

with_assert_op_override(Op,Call):-with_assertions(thlocal:assert_op_override(Op),Call).

test_expand_units(IN):-fully_expand(query(t,must),IN,OUT),dmsg(test_expand_units((IN->OUT))).



mpred_modify(Op,                 G):- (var(Op);var(G)),!,trace_or_throw(var_database_modify_op(Op,  G )).
mpred_modify(Op,                 G):- G\=meta_argtypes(_),fully_expand(Op,G,GG),not_variant(G,GG),!,mpred_modify(Op, GG ),!.
mpred_modify(_,  (:-include(FILE))):- !,must(load_data_file_now(FILE)).
mpred_modify(Op,  (:-(G))         ):- !,must(with_assert_op_override(Op,debugOnError(G))).
mpred_modify(P,                  G):- thlocal:noDBaseMODs(_),!,dmsg(noDBaseMODs(P,G)).
%mpred_modify(Op,                 G):- mpred_head_expansion(clause,G,GG),not_variant(G,GG),database_modify_0(Op, GG ),!.
mpred_modify(Op,                 G):- database_modify_0(Op,G ),!.
mpred_modify(Op,                 G):- trace_or_throw(unknown_database_modify(Op,G)).


/*
database_modify_0(change(assert, add), G):-predicate_property(M:G,_PP),M==system,!,mpred_call(G).
database_modify_0(change(assert, add), G):-current_predicate(_,G),!,debugOnError(G).
database_modify_0(Op,G):- Op=change(_,_), G=..[And|Stuff], is_logical_functor(And),dtrace(And),loop_check(with_logical_functor(And,Stuff,database_modify_0(Op))).
database_modify_0(Op,G):- once(must(db_quf(Op,G,U,Template))),not(is_true(U)),!, mpred_call(U),database_modify_0(Op,Template).
database_modify_0(change(Assert,AorZ),G):- non_assertable(G,Why),trace_or_throw(non_assertable(Why,change(Assert,AorZ),G)).
database_modify_0(change(retract,all),G):- !, db_quf(change( retract,all),G,U,Template),!,when_debugging(retract,dtrace), doall((mpred_call(U),hooked_retractall(Template))).
database_modify_0(change(retract,A),G):- must(db_quf(change( retract,A),G,U,Template)),!,when_debugging(retract,dtrace), mpred_call(U),!,hooked_retract(Template).
database_modify_0(Op,  G):- Op=..[Assert,How],!,database_modify_0(change(Assert,How),   G).
database_modify_0(Op,  G):- Op=..[Assert],!,database_modify_0(change(Assert,one),   G).
*/

database_modify_0(Op,                       M:G):- atom(M),!, database_modify_0(Op,G).
database_modify_0(Op,                   (C1,C2)):- !, must(database_modify_0(Op,C1)), must(database_modify_0(Op,C2)).
database_modify_0(change(Assert,AorZ),(G:-TRUE)):- is_true(TRUE),!,database_modify_0(change(Assert,AorZ),G).
database_modify_0(change(retract,a),          G):- hooked_retract(G).
database_modify_0(change(retract,one),        G):- hooked_retract(G).
database_modify_0(change(retract,_),          G):- hooked_retractall(G).
database_modify_0(change(assert,AZ),          G):- singletons_throw_else_fail(assert(AZ,G)).
database_modify_0(change(assert,AZ),          G):- copy_term(G,GG),database_modify_assert(change(assert,AZ),G,GG),(must(variant(G,GG))).

database_modify_assert(change(assert,_),         G,GG):- ( \+ \+ is_asserted(GG)),must(variant(G,GG)),!.
database_modify_assert(change(assert,AZ),       _G,GG):- expire_pre_change(AZ,GG),fail.

database_modify_assert(change(assert,_),_, GG):- thglobal:pfcManageHybrids,!,copy_term(GG,GGG),(\+ \+ pfc_add_fast(GGG)),!,show_call_failure(variant(GG,GGG)),!.

database_modify_assert(change(assert,AorZ),      G,GG):- G \= (_:-_), get_functor(G,F,A),
   (isa(F,prologSingleValued) -> (AorZ \== sv -> db_assert_sv(AorZ,G,F,A); fail); 
       isa(F,prologOrdered) -> (AorZ\==z -> database_modify_assert_must(change(assert,z),G,GG);true)).
database_modify_assert(change(assert,AorZ),      G,GG):-database_modify_assert_must(change(assert,AorZ),G,GG).

database_modify_assert_must(Op,G,GG):-must(database_modify_assert_4(Op,G,GG)).

database_modify_assert_4(change(assert,_),_, GG):- thglobal:pfcManageHybrids,!,copy_term(GG,GGG),(\+ \+ pfc_add_fast(GGG)),!,show_call_failure(variant(GG,GGG)),!.
database_modify_assert_4(change(assert,AorZ), _,GG):- Op = change(assert,AorZ),                              
                              database_modify_5(Op,GG),!,
                              database_modify_6(Op,GG),
                              database_modify_7(Op,GG),
                              ignore(show_call_failure(database_modify_8(Op,GG))),!.

database_modify_5(change(assert,_), GG):- thglobal:pfcManageHybrids,!,copy_term(GG,GGG),(\+ \+ pfc_add_fast(GGG)),!,show_call_failure(variant(GG,GGG)),!.
database_modify_5(Op,GG):- copy_term(GG,GGG),must((must_storage_op(Op,GGG), sanity(variant(GG,GGG)))),!.
database_modify_6(Op,GG):- copy_term(GG,GGE),doall(must(call_no_cuts(expire_post_change(Op,GGE)))),sanity(variant(GG,GGE)),!.
database_modify_7(Op,GG):- copy_term(GG,GGH),must((run_database_hooks(Op,GGH),sanity(variant(GG,GGH)))),!.
database_modify_8(_ ,GG):- copy_term(GG,GGA),no_loop_check(is_asserted_eq(GGA)),sanity(variant(GG,GGA)),!.
                               


% ========================================
% only place ever should actual game database be changed from
% ========================================

hooked_asserta(G):- loop_check(mpred_modify(change(assert,a),G),true).

hooked_assertz(G):- loop_check(mpred_modify(change(assert,z),G),true).

hooked_retract(G):-  Op = change(retract,a),
                   ignore(slow_sanity(ignore(show_call_failure((mpred_op(is_asserted,G)))))),
                   slow_sanity(not(singletons_throw_else_fail(retract_cloc(G)))),
                   slow_sanity(ignore(((ground(G), once(show_call_failure((is_asserted(G)))))))),
                   must_storage_op(Op,G),expire_post_change( Op,G),
                   sanity(ignore(show_call_failure(not_asserted((G))))),
                   loop_check(run_database_hooks_depth_1(change(retract,a),G),true).

hooked_retractall(G):- Op = change(retract,all),
                   slow_sanity(ignore(((ground(G), once(show_call_failure((is_asserted(G)))))))),
                   must_storage_op(Op,G),expire_post_change( Op,G),
                   sanity(ignore(show_call_failure(not_asserted((G))))),
                   loop_check(run_database_hooks_depth_1(change(retract,all),G),true).



user:provide_mpred_storage_op(Op,G):- get_functor(G,F,A),user:provide_mpred_storage_op(Op,G,F,A).

user:provide_mpred_storage_op(Op,G, F,_A):- t(pfcControlled,F),!,loop_check(prolog_provide_mpred_storage_op(Op,G)).
user:provide_mpred_storage_op(Op,G, F,_A):- t(prologOnly,F),!,loop_check(pfc_provide_mpred_storage_op(Op,G)).
user:provide_mpred_storage_op(Op,G,_F,_A):- loop_check(prolog_provide_mpred_storage_op(Op,G)).

%user:provide_mpred_storage_op(Op,G):- (loop_check(isa_provide_mpred_storage_op(Op,G))).
%user:provide_mpred_storage_op(Op,G):- Op\=change(_,_), (call_no_cuts(user:provide_mpred_storage_clauses(G,true,_Proof))).

must_storage_op(Op,G):- doall(must(may_storage_op(Op,G))).

may_storage_op(Op,G):-call_no_cuts(user:provide_mpred_storage_op(Op,G)).


:- meta_predicate hooked_asserta(+), hooked_assertz(+), hooked_retract(+), hooked_retractall(+).

:- meta_predicate del(-),clr(-),add(-),req(-), fully_expand(-,-,-).

% Found new meta-predicates in iteration 1 (0.281 sec)
%:- meta_predicate mpred_modify(?,?,?,0).





%retract_all((G:-B)) :-!, forall(clause(G,B,Ref),erase(Ref)).
retract_all(HB) :- ignore((retract(HB),fail)).


is_static_pred(Head:-_):-!,predicate_property(Head,_),not(predicate_property(Head,dynamic)).
is_static_pred(Head):-predicate_property(Head,_),not(predicate_property(Head,dynamic)).

prolog_provide_mpred_storage_op(Op,G):- G\=isa(_,_), get_functor(G,F),user:mpred_prop(F,prologOnly),!, prolog_op(Op,G).
prolog_provide_mpred_storage_op(Op,G):- G\=isa(_,_), get_functor(G,F),not(user:mpred_prop(F,prologHybrid)),!,current_predicate(_,G), prolog_op(Op,G).
use_if_modify_new:- current_predicate(assert_if_new/1).
prolog_op(change(AR,Op), G):-ensure_dynamic(G),!,prolog_modify(change(AR,Op), G).

prolog_op(_,clause(G,B)):-!,clause_asserted(G,B).
prolog_op(_,clause(G,B,Ref)):-!,clause(G,B,Ref).

prolog_op(query(_,Op),G):-!,prolog_op(Op,G).
prolog_op(call(Op),G):-!, prolog_op(Op,G).
prolog_op(clauses(Op),G):-!, prolog_op(Op,G).
prolog_op(is_asserted,(G:-B)):-!,clause_asserted(G,B).
prolog_op(is_asserted,(G)):-!,clause_asserted(G,true).

prolog_op(conjecture,G):-!, mpred_call(G).
prolog_op(call,G):-!, mpred_call(G).
prolog_op(Op,G):- reduce_mpred_op(Op,Op2), debugOnError(call(Op2,G)).



prolog_modify(_Op,(:-(G))):-!, mpred_call(G).
prolog_modify(change(assert,z),G):- use_if_modify_new,!,assertz_if_new(G).
prolog_modify(change(assert,a),G):- use_if_modify_new,!,asserta_if_new(G).
prolog_modify(change(assert,_),G):- use_if_modify_new,!,assert_if_new(G).
prolog_modify(change(assert,z),G):-!,assertz(G).
prolog_modify(change(assert,a),G):-!,asserta(G).
prolog_modify(change(assert,_),G):-!,assert(G).
prolog_modify(change( retract,all),G):-!,retractall(G).
prolog_modify(change(retract,one),(G-B)):-!,retract((G-B)).

prolog_modify(change(retract,_),G):-!,retract(G).
prolog_modify(Op,G):- reduce_mpred_op(Op,Op2), mud_call_store_op(Op2,G).


