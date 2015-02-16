/** <module> 
% ===================================================================
% File 'dbase_db_preds.pl'
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

% ========================================
% Shared Preds
% ========================================
:- include(dbase_i_header).


:- export(( (add)/1, clr/1,database_op_int/2,ireq/1,del/1,  
  padd/2, padd/3, prop/3, prop_or/4, props/2, iprops/2, upprop/2,add/1, ireq/1, mreq/1, upprop/1, req/1, 
  use_term_listing/2,  world_clear/1,  
   with_kb_assertions/2)).
%:- meta_predicate man:with_assertions(:,0).
%:- meta_predicate intersect(?,0,?,0,0,-).
%:- meta_predicate del(0),clr(0),add(0),add0(0),req(0), database_op_int(0,0,0).


world_clear(Named):-fmt('Clearing world database: ~q.~n',[Named]).


:- thread_local thlocal:override_hilog/1.

current_hilog(Dbase_t):- thlocal:override_hilog(Dbase_t),!.
current_hilog(dbase_t).

% ================================================
% fact_checked/2, fact_loop_checked/2
% ================================================
:- meta_predicate_transparent(fact_checked(?,0)).

fact_checked(Fact,Call):- not(ground(Fact)),!,no_loop_check(call_tabled(Call),is_asserted(Fact)).
fact_checked(Fact,_):- is_known_false0(Fact),!,fail.
fact_checked(Fact,_):- is_known_trew(Fact),!.
fact_checked(Fact,Call):- no_loop_check(call_tabled(Call),is_asserted(Fact)).

:-meta_predicate_transparent(fact_loop_checked(+,0)).
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


singletons_throw_else_fail(C):- not_is_release,contains_singletons(C),!,(test_tl(thlocal:adding_from_srcfile) ->dmsg(contains_singletons(C)); dmsg(trace_or_throw(contains_singletons(C)))),fail.
nonground_throw_else_fail(C):- not_is_release,not(ground(C)),!,( (test_tl(thlocal:adding_from_srcfile) ->dmsg(not_ground(C)); trace_or_throw(not_ground(C)))),fail.

% ========================================
% only place ever should actual game database be changed from
% ========================================

asserta_cloc(MP):- not(singletons_throw_else_fail(asserta_cloc(MP))),database_modify(change(assert,a),MP),expire_post_change(assert,MP).
assertz_cloc(MP):- not(singletons_throw_else_fail(assertz_cloc(MP))),database_modify(change(assert,z),MP),expire_post_change(assert,MP).

retract_cloc(MP):- ignore(slow_sanity(ignore(show_call_failure((database_check(is_asserted,MP)))))),
                   slow_sanity(not(singletons_throw_else_fail(assertz_cloc(MP)))),
                   slow_sanity(ignore(((ground(MP), once(show_call_failure((is_asserted(MP)))))))),
                   database_modify(change(retract,one),MP),expire_post_change( retract,MP),sanity(not(is_asserted(MP))).

retractall_cloc(MP):- slow_sanity(ignore(((ground(MP), once(show_call_failure((is_asserted(MP)))))))),
                      database_modify(change(retract,all),MP),expire_post_change( retract,MP),sanity(not(is_asserted(MP))).


hooked_asserta(MP):- database_check(is_asserted,MP),!.
hooked_asserta(MP):- asserta_cloc(MP),run_database_hooks(change(assert,a),MP).

hooked_assertz(MP):- database_check(is_asserted,MP),!.
hooked_assertz(MP):- assertz_cloc(MP),run_database_hooks(change(assert,z),MP).

hooked_retract(MP):- nonground_throw_else_fail(hooked_retract(MP)).
hooked_retract(MP):- retract_cloc(MP),loop_check(run_database_hooks_depth_1(change( retract,one),MP)).

hooked_retractall(MP):- retractall_cloc(MP), loop_check(run_database_hooks_depth_1(change( retract,all),MP)).


:- meta_predicate db_must_asserta(0), db_must_assertz(0), hooked_retract(0), hooked_retractall(0).

:- meta_predicate del(-),clr(-),add(-),req(-), database_op_int(-,-).

% Found new meta-predicates in iteration 1 (0.281 sec)
%:- meta_predicate db_op_exact(?,?,?,0).


% ================================================================================
% ASSERT INTO STORAGE STUBS
% ================================================================================

:-export((assertz_clause/1)).
assertz_to_system(Before):- database_expand_term(Before,Replaced),Before \=@= Replaced,!, assertz_to_system(Replaced).
assertz_to_system((':-'(Body))):-!,must_det(show_call(Body)),!.
assertz_to_system((Head :- Body)):- !,assertz_clause(Head,Body),!.
assertz_to_system(C):- assertz_clause(C,true),!.

assertz_clause(Head,Body):- (var(Head);var(Body)),!,trace_or_throw(var_assertz_clause(Head,Body)).
assertz_clause(Head,Body):- clause_asserted((':-'(Head,Body))),!.
assertz_clause(Head,Body):- ExpIn = (Head:-Body),  database_expand_term(ExpIn,Exp),Exp \=@= ExpIn,!,assertz_to_system(Exp),!.
assertz_clause(Head,Body):- must_det(database_modify(change(assert,z), (Head:-Body))).

% ================================================================================
% DETECT PREDS THAT NEED STORAGE 
% ================================================================================
:-export(is_pred_declarer/1).
is_pred_declarer(Prop):- % vFormatted 
	arg(_,v(predArgTypes,predIsFlag,tPred,
        prologMultiValued,prologSingleValued,prologMacroHead,prologOnly,
		prologOrdered,prologNegByFailure,prologPTTP,prologSNARK,prologHybrid,prologListValued),Prop).


requires_storage((Head :- Body),Why):- nonvar(Head),!, requires_storage(Head,Body,Why).
requires_storage(C,Why):- requires_storage(C,true,Why).

requires_storage(H,_,Why):-get_functor(H,F),!,special_head(H,F,Why),!.

special_wrapper_functor(call_mpred_body,direct_to_prolog).
special_wrapper_functor(body_req,direct_to_prolog).
special_wrapper_functor(user:provide_mpred_setup,direct_to_prolog).
special_wrapper_functor(call_provided_mpred_storage_op,direct_to_prolog).
special_wrapper_functor(loop_check,meta).
special_wrapper_functor(loop_check_term,meta).
%special_wrapper_functor(pttp_req).
%special_wrapper_functor(loop_check_clauses).

make_body_clause(_Head,Body,Body):-atomic(Body),!.
make_body_clause(_Head,Body,Body):-special_wrapper_body(Body),!.
make_body_clause(Head,Body,call_mpred_body(Head,Body)).

special_head(_,F,Why):-special_head0(F,Why),!,show_call_failure(not(mpred_prop(F,prologOnly))).
special_head0(F,is_pred_declarer):-is_pred_declarer(F),!.
special_head0(F,macroDeclarer):-hasInstance(macroDeclarer,F),!.
special_head0(F,tCol):-hasInstance(tCol,F),!.
special_head0(F,prologHybrid):-mpred_prop(F,prologHybrid).



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
del(C0):- ireq(C0),!,idel(C0),!.
del(C0):- mreq(C0),!,mdel(C0),!.

idel(C0):- dmsg(idel(C0)),database_api_entry(change( retract,one),C0), verify_sanity(ireq(C0)->(dmsg(warn(incomplete_I_DEL(C0))),fail);true),!.
idel(C0):- dmsg(warn(failed(idel(C0)))),!,fail.

mdel(C0):- dmsg(mdel(C0)),database_api_entry(change( retract,one),C0), verify_sanity(mreq(C0)->(dmsg(warn(incomplete_M_DEL(C0))),fail);true),!.
mdel(C0):- dmsg(warn(failed(mdel(C0)))),!,fail.

% -  clr(Retractall)
clr(C0):- dmsg(clr(C0)),database_api_entry(change( retract,all),C0),verify_sanity(ireq(C0)->(dmsg(warn(incomplete_CLR(C0))));true).

% -  preq(Query) = query with P note
preq(P,C0):- database_api_entry(query(dbase_t,P),C0).

% -  req(Query) = Normal query
req(C0):- dmsg(req(C0)), preq(req,C0).

% -  mreq(Query) = Forced Full query
mreq(C0):- dmsg(mreq(C0)), rescan_module_ready,no_loop_check(with_assertions([-infInstanceOnly(_),-thlocal:infAssertedOnly(_),-thlocal:noRandomValues(_)],preq(must,C0))).


% -  ireq(Query) = Normal query (May not use second order logic) (must be asserted on isntance) (used mainly by 2nd order logic to avoid looping)
ireq(C0):- dmsg(ireq(C0)), rescan_module_ready,no_loop_check(with_assertions([+infInstanceOnly(_), +thlocal:infAssertedOnly(_),+thlocal:noRandomValues(_)],preq(ireq,C0))).

:-dmsg_hide(req).
:-dmsg_hide(ireq).

% -  props(Obj,QueryPropSpecs)
props(Obj,PropSpecs):- req(props(Obj,PropSpecs)).
iprops(Obj,PropSpecs):- ireq(props(Obj,PropSpecs)).



:-export(forall_setof/2).
forall_setof(ForEach,Call):-
   findall(ForEach,ForEach,ForEachAll),
   list_to_set(ForEachAll,Set),!,
   ignore(forall(member(ForEach,Set),Call)).


:-thread_local add_thread_override/1.
% thlocal:add_thread_override(A):-add_from_macropred(A),!.

:-export(((add)/1)).
:-moo_hide_childs((add)/1).
add(A):- A==end_of_file,!.
add(CO:-BODY):- BODY==true,!,add(CO).
add(M:HB):-atom(M),!, must_det(add(HB)),!.
add(grid_key(KW=COL)):- add(typeProps(COL,[glyphType(KW)])).
add(Term):- expands_on(isEach,Term), !,forall(do_expand_args(isEach,Term,O),add(O)),!.
add(:-(Term)):- must(add_fast(:-(Term))),!.
% add(CO):- thglobal:after_game_load,!,must(add_fast(CO)),!.
add(Skipped):- ground(Skipped),implied_skipped(Skipped),!.
add(CO):- must(add_fast(CO)),!.
add(CO):- ignore((ground(CO),asserta(implied_dont_add(CO)))),!,must_det(add_fast(CO)).
add(A):-trace_or_throw(fmt('add is skipping ~q.',[A])).


implied_skipped(subclass(CO,CO)).
implied_skipped(props(_,[])).
implied_skipped(Skipped):-compound(Skipped), not(functor(Skipped,_,1)), (dbase_t(Skipped);out_of_dbase_t(Skipped)).
implied_skipped(Skipped):-implied_dont_add(Skipped).


:-export(add_fast/1).
% -  add(Assertion)
% add_fast(CO):- must_det((add_fast(CO), xtreme_debug(once(ireq(CO);(with_all_dmsg((debug(blackboard),show_call(add_fast(CO)),rtrace(add_fast(CO)),dtrace(ireq(CO))))))))),!.
add_fast((CO:-BODY)):-BODY==true,!,add_fast(CO).
add_fast((CO:-BODY)):-!,must_det(assertz_clause(CO,BODY)).
add_fast(CO):-must_det((database_api_entry(change(assert,add), CO))). % ,xtreme_debug(ireq(CO)->true;dmsg(warn(failed_ireq(CO)))))),!.

% -  upprop(Obj,PropSpecs) update the properties
upprop(Obj,PropSpecs):- upprop(props(Obj,PropSpecs)).
upprop(CO):- add(CO).
% -  padd(Obj,Prop,Value)
padd(Obj,PropSpecs):- add(props(Obj,PropSpecs)).
% -  padd(Obj,Prop,Value)
padd(Obj,Prop,Value):- add(dbase_t(Prop,Obj,Value)).
% -  prop(Obj,Prop,Value)
prop(Obj,Prop,Value):- req(dbase_t(Prop,Obj,Value)).
% -  prop_or(Obj,Prop,Value,OrElse)
prop_or(Obj,Prop,Value,OrElse):- one_must(ireq(dbase_t(Prop,Obj,Value)),Value=OrElse).


kb_update(New,OldV):- req(New),!,OldV=New.
kb_update(New,OldV):- database_api_entry(change(assert,OldV),New).



% ================================================
% database_api_entry/2
% ================================================
% runs previously required ops fisrt
database_api_entry(change(Assert,Op),Term):- !, database_op_int(change(Assert,Op),Term).
database_api_entry(Op,Term):- do_db_op_hooks,database_op_int(Op,Term),do_db_op_hooks.


% ================================================
% db_assert_[mv|sv]/3
% ================================================

:-dmsg_hide(db_assert_mv).
:-dmsg_hide(db_assert_sv).

:-dmsg_hide(db_op_exact).
:-dmsg_hide(add).

:-dmsg_hide(into_mpred_form).


% assert_with to change(CA1,CB2) mutlivalue pred
:-export((db_assert_mv/4)).
db_assert_mv(_Must,end_of_file,_,_):-!. 
db_assert_mv(OP, argIsa(N, T),F,A):-trace_or_throw(crazy_db_assert_mv(OP, argIsa(N, T),F,A)).
% db_assert_mv(_Must,C,_F,_A):- db_must_assertz(C),!.
db_assert_mv(Must,C,_,_):- test_tl(thlocal:adding_from_srcfile), dmsg(db_assert_mv(Must,C)), db_must_assertz(C).
db_assert_mv(Must,C,F,_):- dmsg(db_assert_mv(Must,C)), must_det((mpred_prop(F,prologOrdered) -> db_must_assertz(C) ; db_must_asserta(C))).


% assert_with to change(CA1,CB2) singlevalue pred
:-export((db_assert_sv/4)).
%db_assert_sv(_Must,C,F,A):- throw_if_true_else_fail(contains_singletons(C),db_assert_sv(C,F,A)).
db_assert_sv(Must,C,F,A):- ex, ignore(( loop_check(db_assert_sv_lc(Must,C,F,A),true))).

:-export((db_assert_sv_lc/4)).
db_assert_sv_lc(Must,C,F,A):- arg(A,C,UPDATE),db_assert_sv_now(Must,C,F,A,UPDATE),!.

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
   notrace(must_det((once(ireq(COLD);mreq(COLD)),ground(COLD)))),
   update_value(OLD,UPDATE,NEW),!,
   db_assert_sv_replace(Must,C,F,A,NEW),!.

:-export(db_assert_sv_replace/5).

:-style_check(-singleton).
% db_assert_sv_replace_noisey_so_disabled
db_assert_sv_replace(_Must,C,_,A,NEW):- fail,
   replace_arg(C,A,_,CBLANK),
   hooked_retractall(CBLANK),
   replace_arg(C,A,NEW,CNEW),
   must_det(db_must_asserta_confirmed(CNEW,A,NEW)),!.

db_assert_sv_replace(Must,C,F,A,NEW):-
   replace_arg(C,A,OLD,COLD),
   replace_arg(C,A,NEW,CNEW),
   notrace(ignore(ireq(COLD))),
   must_det(db_assert_sv_replace_with(Must,C,F,A,COLD,CNEW,OLD,NEW)),!.

db_assert_sv_replace_with(Must,C,F,A,COLD,CNEW,OLD,NEW):- var(OLD),!,   
   dmsg(db_assert_sv(COLD,'__add__',CNEW)),
   % replace_arg(C,A,_,CBLANK),hooked_retractall(CBLANK),
   must_det(db_must_asserta_confirmed(CNEW,A,NEW)),!.

db_assert_sv_replace_with(Must,C,F,A,COLD,CNEW,OLD,NEW):- OLD =@= NEW,!.
db_assert_sv_replace_with(Must,C,F,A,COLD,CNEW,OLD,NEW):- unify_with_occurs_check(OLD,NEW),!,dmsg(db_assert_sv_same(COLD,'__unify_with_occurs_check__',CNEW)).
db_assert_sv_replace_with(Must,C,F,A,COLD,CNEW,OLD,NEW):- equals_call(OLD,NEW),!,dmsg(db_assert_sv_same(COLD,'__same__',CNEW)),trace_or_throw(dtrace).
db_assert_sv_replace_with(Must,C,F,A,COLD,CNEW,OLD,NEW):-
   dmsg(db_assert_sv(COLD,'__replace__',CNEW)),
   notrace((ignore(show_call_failure((clr(COLD), not(ireq(COLD))))))),
   %replace_arg(C,A,_,CBLANK),must_det(clr(CBLANK)),hooked_retractall(CBLANK),   
   must_det(db_must_asserta_confirmed(CNEW,A,NEW)),!.

:-style_check(+singleton).


equals_call(X,Y):-unify_with_occurs_check(X,Y),!.
equals_call(X,Y):-once((any_to_string(X,XX),any_to_string(Y,YY))),unify_with_occurs_check(XX,YY),!.
equals_call(X,Y):-once((to_word_list(X,XX),to_word_list(Y,YY))),unify_with_occurs_check(XX,YY),!.
equals_call(X,Y):-compound(X),compound(Y),once((correctArgsIsa(X,XX),correctArgsIsa(Y,YY))),unify_with_occurs_check(XX,YY),!.

confirm_hook(CNEW:NEW=@=CNOW:NOW):-
   sanity(var(NOW)),               
   notrace((once(ireq(CNOW)))),
   CNEW:NEW=@=CNOW:NOW,!.

confirm_hook(CNEW:NEW=@=CNOW:NOW):-
   dmsg(warn(failed_i_a_req(CNOW,expected(CNEW)))),   
   dtrace((verify_sanity((mreq(CNOW),(CNEW:NEW=@=CNOW:NOW))))),!.



% Expect CNEW to be what is found
db_must_asserta_confirmed(CNEW,A,NEW):-
   replace_arg(CNEW,A,NOW,CNOW),
   sanity(not(singletons_throw_else_fail(CNEW))),
   db_must_asserta(CNEW),!,
   verify_sanity(confirm_hook(CNEW:NEW=@=CNOW:NOW)),!.

db_must_asserta_confirmed(CNEW,A,NEW):-dmsg(unconfirmed(db_must_asserta_confirmed(CNEW,A,NEW))).

db_must_asserta(G):-loop_check(database_modify(change(assert,a),G),trace_or_throw(loop_db_must_asserta(G))).

db_must_assertz(G):-loop_check(database_modify(change(assert,z),G),trace_or_throw(loop_db_must_assertz(G))).


with_assert_op_override(_Op,Call):- call(Call).
% with_assert_op_override(Op,Call):-with_assertions(thlocal:assert_op_override(Op),Call).
% ================================================
% database_op_int/2
% ================================================

:-moo_hide_childs(database_op_int/2).

% ================================================
% database_op_int/2  SIMPLISTIC REWRITE (this is not the PRECANONICALIZER)
% ================================================

any_op_to_call_op(_,call(conjecture)).

database_maplist([E],E,Op,G):- !,database_op_int(Op,G).
database_maplist([E|List],T,Op,G):- copy_term(T+G,CT+CG),E=CT,!,database_op_int(Op,CG),database_maplist(List,T,Op,G).
database_maplist(List,T,Op,G):-forall(member(T,List),database_op_int(Op,G)).

database_op_int(_,Term):-not(compound(Term)),!.
database_op_int(_,NC):- sanity(compound(NC)),fail.
database_op_int(Op,Term):- expands_on(isEach,Term), !,forall(do_expand_args(isEach,Term,O),database_op_int(Op,O)).
database_op_int(Op,MT:Term):- is_kb_module(MT),!,with_assertions(thlocal:caller_module(kb,MT),database_op_int(Op,Term)).
database_op_int(Op,DB:Term):- dbase_mod(DB),!,with_assertions(thlocal:caller_module(db,DB),database_op_int(Op,Term)).
database_op_int(Op,KB:Term):- atom(KB),!,with_assertions(thlocal:caller_module(prolog,KB),database_op_int(Op,Term)).
database_op_int(Op,('prologCall'(A))):-!, logOnFailure(show_call((any_op_to_call_op(Op,CallOp),!,database_op_int_0(CallOp,A)))).
database_op_int(Op,(':-'(A))):-!, logOnFailure(show_call((any_op_to_call_op(Op,CallOp),!,database_op_int_0(CallOp,A)))).
database_op_int(Op,(':-'(A))):- !, logOnFailure(show_call((any_op_to_call_op(Op,CallOp),!,database_op_int_0(CallOp,A)))).

database_op_int(Op,mpred_prop(F,A)):-  !, with_assert_op_override(Op,decl_mpred(F,A)),!.
database_op_int(Op,mpred_arity(F,A)):-  !, with_assert_op_override(Op,decl_mpred(F,A)),!.
database_op_int(Op,pddlSomethingIsa(A,List)):- !,database_maplist(List,E,Op, isa(A,E)).
database_op_int(Op,pddlDescription(A,List)):- !,database_maplist(List,E,Op, mudDescription(A,E)).
database_op_int(Op,pddlObjects(Type,List)):- !,database_maplist(List,I,Op,isa(I,Type)).
database_op_int(Op,pddlSorts(Type,List)):- !,database_maplist(List,I,Op, subclass(I,Type)).
database_op_int(Op,pddlTypes(List)):- !, with_assert_op_override(Op,maplist(decl_type,List)).
database_op_int(Op,pddlPredicates(List)):- !, with_assert_op_override(Op,maplist(decl_mpred,List)).
database_op_int(Op,EACH):- EACH=..[each|List],database_maplist(List,T,Op,T).
database_op_int(Op,EACH):- database_op_int_0(Op,EACH).

database_op_int_0(Op,decl_mpred(F,A)):-  !, with_assert_op_override(Op,decl_mpred(F,A)),!.
database_op_int_0(query(HLDS,Must),props(Obj,Props)):- nonvar(Obj),var(Props),!,gather_props_for(query(HLDS,Must),Obj,Props).
database_op_int_0(Op,Wild):- must(full_transform(Op,Wild,HoldsOut)),!,db_op_exact_units(Op,HoldsOut).


% ================================================
% db_redir_op_if_needed/4
% ================================================
/* TODO UNCOMMENT
db_redir_op_if_needed(Op,C0,Prop,_ARGS):- glean_pred_props_maybe(C0),fail.

% predProxyDatabase/1
db_redir_op_if_needed(Op,C0,Prop,_ARGS):- get_mpred_prop(Prop,predProxyDatabase(Other)),must(nonvar(Other)),!,call(Other,Op,C0).

% predProxyAssert/1
db_redir_op_if_needed(change(assert,A),C0,Prop,_RGS):- get_mpred_prop(Prop,predProxyAssert(How)),must(nonvar(How)),!, once(ignore((call(How,C0), run_database_hooks(change(assert,A),C0)))).

% predProxyRetract/1
db_redir_op_if_needed(change(retract,A),C0,Prop,_RGS):- get_mpred_prop(Prop,predProxyRetract(How)),must(nonvar(How)),!, once(ignore((call(How,C0), run_database_hooks(change( retract,A),C0)))).

% predProxyQuery/1
db_redir_op_if_needed(query(Must,HLDS),C0,Prop,_RGS):- get_mpred_prop(Prop,predProxyQuery(How)),must(nonvar(How)),!, database_call(query(Must,HLDS),call(How,C0)).

% plain prop
db_redir_op_if_needed(Op,_C0,Prop,ARGS):- db_op_exact_units(Op,Unit).


*/


% ================================================
% db_op_exact/2
% ================================================
db_op_exact_units(Op,G) :- no_loop_check(with_assertions(thlocal:infSkipArgIsa,db_op_exact(Op,G))).

with_logical_functor(not,[H],Call):- !, not(call(Call,H)).
with_logical_functor(_And,[H],Call):- !, call(Call,H).
with_logical_functor(And,[H|T],Call):-
   DO =..[And,call(Call,H),with_logical_functor(And,T,Call)],
   call(DO).

db_op_exact(OP,C):- C=..[And|Stuff], is_logical_functor(And),with_logical_functor(And,Stuff,db_op_exact(OP)).
db_op_exact(change(assert,_),isa(F,A)):- atom(A), hasInstance(macroDeclarer,A),!,decl_mpred(F,A).
db_op_exact(change(retract,all),C):- !, db_quf(change( retract,all),C,U,Template),!,when_debugging(retract,dtrace), doall((call_mpred(U),hooked_retractall(Template))).
db_op_exact(change(retract,A),C):- must(db_quf(change( retract,A),C,U,Template)),!,when_debugging(retract,dtrace), call_mpred(U),!,hooked_retract(Template).
db_op_exact(change(Assert,OldV),W):- non_assertable(W,Why),trace_or_throw(todo(database_op_int(change(Assert,OldV), non_assertable(Why,W)))).

db_op_exact(change(assert,Must),C0):- 
   must(db_quf(change(assert,Must),C0,U,C)),!,
   call_mpred(U),
    get_functor(C,F,A),
      must(get_mpred_prop(F,prologSingleValued) -> db_assert_sv(Must,C,F,A) ; db_assert_mv(Must,C,F,A)).

db_op_exact(Op,C):- must(db_quf(Op,C,U,Template)),!, 
        (U==true-> GOAL = Template ; GOAL=(U,Template)),
         loop_check(database_op(Op,GOAL),is_asserted(GOAL)).

% db_op_exact(Op,C):- trace_or_throw(unhandled(db_op_exact(Op,C))).

test_expand_units(IN):-full_transform(query(dbase_t,must),IN,OUT),dmsg(test_expand_units((IN->OUT))).

full_transform(Op,Wild,HoldsOut):-
  with_assertions(thlocal:into_form_code,((transitive(full_transform_each(Op),Wild,HoldsOut),((Wild\=@=HoldsOut -> dmsg(full_transform(Op,Wild->HoldsOut)));true)))).

full_transform_each(Op,Wild,HoldsOut):- 
  transitive(full_transform_0(Op),Wild,W2),full_transform_1(Op,W2,W3),full_transform_2(Op,W3,W4),full_transform_3(Op,W4,HoldsOut).

full_transform_0(change(_,_),Wild,HoldsOut):-user:ruleRewrite(Wild,HoldsOut).

full_transform_1(Op,Wild,HoldsOut):-
             must(into_mpred_form(Wild,Simpler)),must(correctArgsIsa(Op,Simpler,HoldsIn)),
             must((current_hilog(Functor),transform_holds(Functor,HoldsIn,HoldsOut))).
full_transform_2(Op,Wild,HoldsOut):- db_quf(Op,Wild,Pretest,Template),(Pretest==true-> HoldsOut = Template ; HoldsOut = (Pretest,Template)),!.
full_transform_3(A,B,C):- transitive(expand_props(A),B,C),!.

instTypePropsToType(instTypeProps,ttSpatialType).
typePropsToType(typeProps,tCol).
typePropsToType(mpred_prop,tPred).
typePropsToType(props,ftTerm).

into_expand_mpred_form(Op,prop(Obj,Prop),Out):-transitive(expand_props(Op),prop(Obj,Prop),Out).
into_expand_mpred_form(_,In,Out):-transitive(into_mpred_form,In,Out).

expand_props(_,Sent,OUT):-not(compound(Sent)),!,OUT=Sent.
expand_props(Op,(True,Term),OUT):- True==true,!,expand_props(Op,(Term),OUT).
expand_props(Op,Term,OUT):- stack_check,(var(Op);var(Term)),!,trace_or_throw(var_expand_units(Op,Term,OUT)).
expand_props(Op,Sent,OUT):-Sent=..[And|C12],is_logical_functor(And),!,maplist(expand_props(Op),C12,O12),OUT=..[And|O12].
expand_props(Op,props(Obj,Open),OUT):- var(Open),!,trace_or_throw(expand_props(Op,props(Obj,Open)),OUT).
expand_props(Op,props(Obj,List),nop(expand_props(Op,props(Obj,[])))):- List==[],!.
expand_props(Op,props(Obj,[P]),OUT):- nonvar(P),!,expand_props(Op,props(Obj,P),OUT).
expand_props(Op,props(Obj,[P|ROPS]),OUT):- !,expand_props(Op,props(Obj,P),OUT1),expand_props(Op,props(Obj,ROPS),OUT2),conjoin(OUT1,OUT2,OUT).
expand_props(Op,props(Obj,PropVal),OUT):- atom(PropVal),!,Call=..[PropVal,Obj],!,into_expand_mpred_form(Op,Call,OUT).
expand_props(Op,props(Obj,PropVal),OUT):- safe_univ(PropVal,[Prop,NonVar|Val]),Obj==NonVar,!,into_expand_mpred_form(Op,[dbase_t,Prop,Obj|Val],OUT).
expand_props(Op,props(Obj,PropVal),OUT):- PropVal=..[OP,Pred|Val],comparitiveOp(OP),
   not(comparitiveOp(Pred)),!,OPVAL=..[OP|Val],PropVal2=..[Pred,OPVAL],
    expand_props(Op,props(Obj,PropVal2),OUT).
expand_props(Op,props(Obj,PropVal),OUT):- PropVal=..[Prop|Val],not(infix_op(Prop,_)),!,into_expand_mpred_form(Op,[dbase_t,Prop,Obj|Val],OUT).
expand_props(Op,props(Obj,PropVal),OUT):- PropVal=..[Prop|Val],!,trace_or_throw(dtrace),into_expand_mpred_form(Op,[dbase_t,Prop,Obj|Val],OUT).
expand_props(Op,RDF,OUT):- RDF=..[SVO,S,V,O],is_svo_functor(SVO),!,must_det(into_expand_mpred_form(Op,[dbase_t,V,S,O],OUT)).

expand_props(Op,ClassTemplate,OUT):- ClassTemplate=..[TypeProps,Type|Props],
  typePropsToType(TypeProps,TypePropsIsa),
  TypeProps\=props,
  assert_isa(Type,TypePropsIsa),
  create_the_inst_fn(_ALL,Type,TypePropsIsa,NewType),
  NewClassTemplate=..[props,NewType|Props],
  expand_props(Op,NewClassTemplate,OUT),!.

expand_props(Op,ClassTemplate,OUT):- ClassTemplate=..[TypeProps,X,Type|Props],
  instTypePropsToType(TypeProps,TypePropsIsa),
  TypeProps\=props,
  assert_isa(Type,TypePropsIsa),
  create_the_inst_fn(X,Type,TypePropsIsa,NewType),
  NewClassTemplate=..[props,NewType|Props],
  expand_props(Op,NewClassTemplate,OUT),!.


expand_props(Op,ClassTemplate,OUT):- get_functor(ClassTemplate,F,A),not(A==2),
   ClassTemplate=..[TypeProps,Type|Props],
   typePropsToType(TypeProps,TypePropsIsa),F==TypeProps,!,   
   assert_isa(Type,TypePropsIsa),
   flatten(Props,AllProps),
   MID=..[TypeProps,Type,AllProps],
   expand_props(Op,MID,OUT),dmsg(warn(used_type_props(MID))),!.

expand_props(_,Wild,Wild).

create_the_inst_fn(_,Type,PropsIsa,NewType):-PropsIsa==tCol, NewType = isInstFn(Type),!.
create_the_inst_fn(X,Type,PropsIsa,NewType):- NewType = isKappaFn(X,and(isa(X,Type),isa(Type,PropsIsa))),!.

:-export(conjoin/3).
conjoin_op(OP,A,B,C) :-
	A == true ->
		C = B;
	B == true ->
		C = A;
        C =.. [OP,A,B].

db_quf_l(Op,_And,[C],D2,D3):- db_quf(Op,C,D2,D3),!.
db_quf_l(Op, And,[C|C12],PreO,TemplO):-
  db_quf(Op,C,Pre,Next),
  db_quf_l(Op,And,C12,Pre2,Templ2),
  conjoin(Pre,Pre2,PreO),
  conjoin_op(And,Next,Templ2,TemplO).

:-export(db_quf/4).
db_quf(Op,M:C,Pretest,Template):-var(C),!,throw(var(db_quf(Op,M:C,Pretest,Template))).
db_quf(Op,M:C,Pretest,Template):-atom(M),!,must(db_quf(Op,C,Pretest,Template)).
db_quf(Op,Sent,D2,D3):-
 Sent=..[And|C12],is_logical_functor(And),!,
 db_quf_l(Op,And,C12,D2,D3).

db_quf(Op,':-'(C,D),':-'(C2,D2),':-'(C3,D3)):-!,db_quf(Op,C,C2,C3),db_quf(Op,D,D2,D3).
db_quf(Op,','(C,D),','(C2,D2),','(C3,D3)):-!,db_quf(Op,C,C2,C3),db_quf(Op,D,D2,D3).

db_quf(Op,C,Pretest,Template):- C=..[Holds,OBJ|ARGS],is_holds_true(Holds),atom(OBJ),!,C1=..[OBJ|ARGS],must(db_quf(Op,C1,Pretest,Template)).
db_quf(_Op,C,true,C):- C=..[Holds,OBJ|_],is_holds_true(Holds),var(OBJ),!.
db_quf(Op,C,Pretest,Template):- C=..[Prop,OBJ|ARGS],
      functor(C,Prop,A),
      must(translate_args(Op,Prop,A,OBJ,2,ARGS,NEWARGS,true,Pretest)),
      Template =.. [Prop,OBJ|NEWARGS].

translate_args(_O,_Prop,_A,_OBJ,_N,[],[],GIN,GIN).
translate_args(Op,Prop,A,OBJ,N1,[ARG|S],[NEW|ARGS],GIN,GOALS):-
   must(argIsa_call(Op,Prop,N1,Type)),
   translateOneArg(Op,Prop,OBJ,Type,ARG,NEW,GIN,GMID),
   N2 is N1 +1,
   translate_args(Op,Prop,A,OBJ,N2,S,ARGS,GMID,GOALS).

:-export(infix_op/2).
infix_op(Op,_):-comparitiveOp(Op).
infix_op(Op,_):-additiveOp(Op).

:-export(comparitiveOp/1).
comparitiveOp((\=)).
comparitiveOp((\==)).
comparitiveOp((=)).
comparitiveOp((=:=)).
comparitiveOp((==)).
comparitiveOp((<)).
comparitiveOp((>)).
comparitiveOp((=<)).
comparitiveOp((>=)).

:-export(additiveOp/1).
additiveOp((is)).
additiveOp((*)).
additiveOp(+).
additiveOp(-).
additiveOp((/)).

% ftVar
translateOneArg(_Op,_Prop,_Obj,_Type,VAR,VAR,G,G):-var(VAR),!.

% not an expression
translateOneArg(_O,_Prop,_Obj,_Type,ATOMIC,ATOMIC,G,G):-atomic(ATOMIC),!.
% translateOneArg(_O,_Prop,_Obj,Type,ATOMIC,ATOMICUSE,G,(G,same_arg(col(Type),ATOMIC,ATOMICUSE))):-atomic(ATOMIC),!.

% translateOneArg(_O,_Prop,_Obj,Type,VAR,VAR,G,G):-ignore(isa(VAR,Type)),!.

% props(Obj,size < 2).
translateOneArg(_O,Prop,Obj,Type,ARG,OLD,G,(GETTER,COMPARE,G)):-
       functor(ARG,F,2), comparitiveOp(F),!,
       ARG=..[F,Prop,VAL],
       GETTER=..[Prop,Obj,OLD],
       COMPARE= compare_op(Type,F,OLD,VAL),!.

% props(Obj,isOneOf(Sz,[size+1,2])).
translateOneArg(Op,Prop,O,Type,isOneOf(VAL,LIST),VAL,G,(GO,G)):-
   translateListOps(Op,Prop,O,Type,VAL,LIST,G,GO).

% db_op(Op, Obj,size + 2).
translateOneArg(_O,Prop,Obj,_Type,ARG,NEW,G,(GETTER,STORE,G)):-
       functor(ARG,F,2), additiveOp(F),!,
       ARG=..[F,Prop,VAL],
       GETTER=..[Prop,Obj,OLD],
       STORE= update_value(OLD,VAL,NEW),!.

translateOneArg(_O,_Prop,_Obj,_Type,NART,NART,G,G):-!.
translateOneArg(_O,_Prop,_Obj,Type,ATOMIC,ATOMICUSE,G,(G,ignore(same_arg(tCol(Type),ATOMIC,ATOMICUSE)))).

translateListOps(_O,_Prop,_Obj,_Type,_VAL,[],G,G).
translateListOps(Op,Prop,Obj,Type,VAL,[L|LIST],G,GO2):-
   translateOneArg(Op,Prop,Obj,Type,L,VAL,G,GO),
   translateListOps(Op,Prop,Obj,Type,VAL,LIST,GO,GO2).

compare_op(Type,F,OLD,VAL):-nop(Type),show_call((call(F,OLD,VAL))),!.

% start of database
% These will all be deleted at start of run

:-export(inverse_args/2).
inverse_args([AR,GS],[GS,AR]):-!.
inverse_args([AR,G,S],[S,G,AR]):-!.
inverse_args([A,R,G,S],[S,R,G,A]):-!.
inverse_args([P,A,R,G,S],[S,A,R,G,P]):-!.

% =======================================================
% correctArgsIsa/3
% =======================================================

:-export(same_vars/2).
same_vars(T1,T2):-term_variables(T1,V1),term_variables(T2,V2),!,V1==V2.


:-moo_hide_childs(replace_arg/4).
replace_arg(C,A,OLD,CC):- 
   C=..FARGS,
   replace_nth(FARGS,A,OLD,FARGO),!,
   CC=..FARGO.

:-moo_hide_childs(replace_nth/4).
replace_nth([],_,_,[]):- !.
replace_nth([_|ARGO],0,OLD,[OLD|ARGO]):- !.
replace_nth([T|FARGS],A,OLD,[T|FARGO]):- 
    A2 is A-1,replace_nth(FARGS,A2,OLD,FARGO).



member_or_e(E,[L|List]):-!,member(E,[L|List]).
member_or_e(E,E).


replace_nth([],_N,_OldVar,_NewVar,[]):- !,trace_or_throw(missed_the_boat).
replace_nth([OldVar|ARGS],1,OldVar,NewVar,[NewVar|ARGS]):- !.
replace_nth([Carry|ARGS],Which,OldVar,NewVar,[Carry|NEWARGS]):- 
 Which1 is Which-1,
 replace_nth(ARGS,Which1,OldVar,NewVar,NEWARGS),!.

:-moo_hide_childs(update_value/4).
update_value(OLD,NEW,NEXT):- var(NEW),!,trace_or_throw(logicmoo_bug(update_value(OLD,NEW,NEXT))).
update_value(OLD,NEW,NEWV):- var(OLD),!,compute_value_no_dice(NEW,NEWV).
update_value(OLD,X,NEW):- is_list(OLD),!,list_update_op(OLD,X,NEW),!.
update_value(OLDI,+X,NEW):- compute_value(OLDI,OLD),number(OLD),catch(NEW is OLD + X,_,fail),!.
update_value(OLDI,-X,NEW):- compute_value(OLDI,OLD),number(OLD),catch(NEW is OLD - X,_,fail),!.
update_value(OLDI,X,NEW):- number(X),X<0,compute_value(OLDI,OLD),number(OLD),catch(NEW is OLD + X,_,fail),!.
update_value(_,NEW,NEWV):-compute_value_no_dice(NEW,NEWV),!.


list_update_op(OLDI,+X,NEW):-flatten_append(OLDI,X,NEW),!.
list_update_op(OLDI,-X,NEW):-flatten([OLDI],OLD),flatten([X],XX),!,list_difference_eq(OLD,XX,NEW),!.

compute_value_no_dice(NEW,NEW):- compound(NEW),functor_catch(NEW,ftDice,_),!.
compute_value_no_dice(NEW,NEWV):-compute_value(NEW,NEWV).

compute_value(NEW,NEWV):-catch(NEWV is NEW,_,fail),!.
compute_value(NEW,NEWV):-catch(any_to_value(NEW,NEWV),_,fail),!.
compute_value(NEW,NEW).

insert_into(ARGS,0,Insert,[Insert|ARGS]):- !.
insert_into([Carry|ARGS],After,Insert,[Carry|NEWARGS]):- 
   After1 is After - 1,
   insert_into(ARGS,After1,Insert,NEWARGS).


add_arg_parts_of_speech(_F,_N,[],[]).
add_arg_parts_of_speech(F,N,[A|ARGS0],[ARG|ARGS]):-argIsa_call_or_undressed(F,N,A,ARG),N1 is N+1, add_arg_parts_of_speech(F,N1,ARGS0,ARGS).

argIsa_call_or_undressed(F,N,Obj,fN(Obj,Type)):- argIsa_call_0(F,N,Type),!.
argIsa_call_or_undressed(_F,_N,Obj,Obj).

verb_after_arg(_,_,1).


% load_motel:- defrole([],time_state,restr(time,period)).
% :-load_motel.




:- style_check(+discontiguous).
:- style_check(-discontiguous).

:-export(thlocal:in_dynamic_reader/1).

:-export(begin_dynamic_reader/0).
begin_dynamic_reader:- must_det(( prolog_load_context(file,Source),asserta(thlocal:in_dynamic_reader(Source)))).
:-export(end_dynamic_reader/0).
end_dynamic_reader:- must_det(( prolog_load_context(file,Source),retract(thlocal:in_dynamic_reader(Source)))).


inside_dynamic_reader :- prolog_load_context(file,Source),test_tl(thlocal:in_dynamic_reader(Source)),!.
inside_dynamic_reader :- prolog_load_context(source,Source),test_tl(thlocal:in_dynamic_reader(Source)),!.


kb_term_expansion(CL,EXP):- 
 % ==== why we assert
  inside_dynamic_reader,!,
% ==== do it
  WHY = was_imported_kb_content(inside_dynamic_reader,CL),
  dmsg(WHY),
  with_assertions(thlocal:adding_from_srcfile,must(add(CL))),!,
  must(EXP=user:WHY).

kb_term_expansion(CL,WHY):- 
% ==== why we assert
  requires_storage(CL,WhyRS),
% ==== do it
  WHY = was_imported_kb_content(requires_storage(WhyRS),CL),
  dmsg(WHY),
  with_assertions(thlocal:adding_from_srcfile,must(add(CL))),!.


