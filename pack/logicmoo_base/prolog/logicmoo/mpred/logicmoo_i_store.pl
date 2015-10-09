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
   setup_call_cleanup(asserta(With,Ref),Call,erase_safe(With,Ref)).


world_clear(Named):-fmt('Clearing world database: ~q.~n',[Named]).



get_pifunctor(Head,PHead):-must(get_pifunctor(Head,PHead,_,_)).
get_pifunctor(Head,PHead,F):-must(get_pifunctor(Head,PHead,F,_)).

get_pifunctor(Head,PHead,F,A):-var(Head),!,sanity(atom(F)),must(ensure_arity(F,A)),functor(PHead,F,A),ignore(PHead=Head).
get_pifunctor(Head,PHead,F,A):-get_functor(Head,F,A),functor(PHead,F,A),ignore(PHead=Head),!.
get_pifunctor(Head,PHead,F,A):-atom(Head),ensure_arity(Head,A),!,get_pifunctor(Head/A,PHead,F,A).

rescan_meta_argtypes(MT):- functor(MT,F,A),functor(M,F,A),MT=..[F|ARGST],M=..[F|ARGS],forall(clause_asserted(M,_),maplist(deduceEachArg_WithType,ARGS,ARGST)),!.
rescan_argIsa(F,N,Type):- ignore(( arity(F,A), functor(M,F,A),forall((clause_asserted(M,_),arg(N,M,E)),deduceEachArg_WithType(E,Type)))),!.

deduceEachArgType(Var):- \+ compound(Var),!.
deduceEachArgType(meta_argtypes(MT)):- !, rescan_meta_argtypes(MT).
deduceEachArgType(tRelation(M)):-compound(M),functor(M,F,A),add(meta_argtypes(M)),add(tRelation(F)),add(arity(F,A)).
deduceEachArgType(M):-functor(M,F,A),M=..[F|ARGS],deduceEachArgType(F,A,ARGS).
deduceEachArgType(F,_,_):-var(F),!.
deduceEachArgType(argIsa,3,[_F,_N,_Type]):-!.
% deduceEachArgType(argIsa,3,[F,N,Type]):- ttFormatType(Type),add(argQuotedIsa(F,N,Type)),!.
deduceEachArgType(argIsa,3,[F,N,Type]):- rescan_argIsa(F,N,Type),fail.
deduceEachArgType(t,_,[F|_]):-var(F),!.
deduceEachArgType(F,_,[E]):- tCol(F),deduceEachArg_WithType(E,F),!.
deduceEachArgType(t,A,[F|ARGS]):-A2 is A-1, deduceEachArgType(F,A2,ARGS).
deduceEachArgType(F,A,ARGS):-functor(MT,F,A),meta_argtypes(MT),if_main(dmsg(deduceEachArgType(F,ARGS,ARGST))),MT =..[_|ARGST],maplist(deduceEachArg_WithType,ARGS,ARGST).
deduceEachArgType(F,_,ARGS):-deduceEachArg_WithArgIsa(F,1,ARGS).



if_main(G):-(thread_self(M),lmcache:thread_main(_,M))->G ; true.

deduceEachArg_WithArgIsa(_,_,[]).
deduceEachArg_WithArgIsa(F,N,[A|RGS]):- ignore((clause_asserted(argIsa(F,N,Type)),deduceEachArg_WithType(A,Type))),
   N2 is N+1,deduceEachArg_WithArgIsa(F,N2,RGS),!.

deduceEachArg_WithType(M,_):- (var(M);number(M)),!.
deduceEachArg_WithType(M,tSpatialThing):-isa(M,tSpatialThing),!.
deduceEachArg_WithType(M,tTemporalhing):-isa(M,tTemporalhing),!.
deduceEachArg_WithType(M,M):-!.
deduceEachArg_WithType(M,MT):- compound(M),!, (compound(MT)->(( M =..ARGS,MT =..ARGST,maplist(deduceEachArg_WithType,ARGS,ARGST))); true).
deduceEachArg_WithType(_,MT):- (MT=ftTerm;ttFormatType(MT)),!.
deduceEachArg_WithType(M,MT):-isa(M,MT),!.
deduceEachArg_WithType(M,MT):- assert_isa_safe(M,MT),!.

side_effect_prone:- \+ t_l:noDBaseMODs(_), t_l:side_effect_ok.



:-meta_predicate(with_no_modifications(0)).
with_no_modifications(CALL):-!,CALL.
with_no_modifications(CALL):-w_tl(t_l:noDBaseMODs(_),CALL).

:-meta_predicate(with_no_db_hooks(0)).
with_no_db_hooks(CALL):-!,CALL.
with_no_db_hooks(CALL):-w_tl(t_l:noDBaseHOOKS(_),CALL).

:-meta_predicate(with_fallbacks(0)).
with_fallbacks(CALL):-wno_tl(t_l:infAssertedOnly(_),CALL).

:-meta_predicate(with_fallbacksg(0)).
with_fallbacksg(CALL):-wno_tl(t_l:noRandomValues(_),CALL).

:-meta_predicate(with_no_fallbacksg(0)).
with_no_fallbacksg(CALL):-w_tl(t_l:noRandomValues(_),CALL).

:-meta_predicate(with_no_fallbacks(0)).
with_no_fallbacks(CALL):-w_tl(t_l:infAssertedOnly(_),CALL).

infSecondOrder :- not(t_l:infInstanceOnly(_)).

infThirdOrder :- fail, infSecondOrder, not(t_l:noRandomValues(_)).


% ================================================
% is_asserted/1/2/3
% ================================================
:- thread_local t_l:fail_is_asserted/1.

with_fail_is_asserted(Temp,Goal):-ground(Temp),!,Goal.
with_fail_is_asserted(Temp,Goal):-w_tl(t_l:fail_is_asserted(Temp),Goal).

:- meta_predicate is_asserted_1(?).
:- meta_predicate is_asserted_eq(?).
:- meta_predicate not_asserted(?).

not_asserted(X):- !,(\+ clause(X,true)).
not_asserted(X):- not(no_loop_check(is_asserted_1(X))).
is_asserted_eq(HB):- ( \+ \+ no_loop_check(is_asserted_1(HB))).

is_asserted(X):- no_repeats(loop_check(mpred_call(X))).
is_asserted(X,Y):- no_repeats(loop_check(is_asserted_2(X,Y))).
is_asserted(X,Y,Z):- no_repeats(loop_check(is_asserted_3(X,Y,Z))).

is_asserted_1(V):-var(V),!,trace_or_throw(var_is_asserted(V)).
% TODO: test removal
%is_asserted_1(prologHybrid(H)):-get_functor(H,F),!,isa_asserted(F,prologHybrid).
is_asserted_1((H)):- is_static_pred(H),!,show_pred_info(H),dtrace(is_asserted_1((H))).
%is_asserted_1(HB):-hotrace((fully_expand_warn(is_asserted_1,HB,HHBB))),!,is_asserted_1(HHBB).

is_asserted_1(H):- !, w_tl(t_l:infAssertedOnly(H),mpred_call(H)).

%is_asserted_1(argIsa(mpred_prop,2,mpred_prop/2)):- dtrace,!,fail.
is_asserted_1(clause(H,B,Ref)):-!,is_asserted_3(H,B,Ref).
is_asserted_1(clause(H,B)):-!,is_asserted_2(H,B).
is_asserted_1((H1,H2)):-!,is_asserted_1(H1),is_asserted_1(H2).
is_asserted_1((H1;H2)):-!,is_asserted_1(H1);is_asserted_1(H2).
% TODO: test removal
% is_asserted_1(isa(H,B)):-!,isa_asserted(H,B).
is_asserted_1(HB):-expand_to_hb(HB,H,B),!,is_asserted_2(H,B).

skip_is_asserted_expansion(_).


is_asserted_2((H:-BB),B):- is_true(B),!,is_asserted_2(H,BB).
is_asserted_2(H,B):-  skip_is_asserted_expansion(H),!,is_asserted_2a(H,B).
is_asserted_2(H,B):-hotrace((fully_expand_warn(is_asserted_2,(H:-B),CL),expand_to_hb(CL,HH,BB))),!,is_asserted_2a(HH,BB).

is_asserted_2a(H,B):-thglobal:pfcManageHybrids,!,mpred_clause_is_asserted(H,B).
is_asserted_2a(H,B):-call_no_cuts(user:mpred_provide_storage_clauses(H,B,_Ref)),not(hotrace(special_wrapper_body(B,_))).

is_asserted_3((H:-BB),B,Ref):- is_true(B),!,is_asserted_3(H,BB,Ref).
is_asserted_3(H,B,Ref):- skip_is_asserted_expansion(H), !,is_asserted_3a(H,B,Ref).
is_asserted_3(H,B,Ref):-hotrace((fully_expand_warn(is_asserted_3,(H:-B),CL),expand_to_hb(CL,HH,BB))),is_asserted_3a(HH,BB,Ref).

is_asserted_3a(H,B,Ref):-call_no_cuts(user:mpred_provide_storage_clauses(H,B,Ref)),not(hotrace(special_wrapper_body(B,_))).

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
/*
ensure_predicate_reachable(_,_):- fast_mud,!.
%ensure_predicate_reachable(M,C):-functor(C,F,A),ensure_predicate_reachable(M,C,F,A),fail.
ensure_predicate_reachable(_,_):- is_release,!.
ensure_predicate_reachable(M,C):-once((predicate_property(C,imported_from(Other)),M\=Other,
                                       context_module(CM),
                                       dmsg(wrong_import_module(M,Other:C,from(CM))),
                                       ignore(delete_import_module(CM,Other)),
                                       '@'((M:dynamic(C),M:export(C)),M),user:import(M:C))),fail.
ensure_predicate_reachable(_,_).
*/

singletons_throw_else_fail(C):- fail,not_is_release,contains_singletons(C),!,(test_tl(t_l:already_in_file_term_expansion) -> (dmsg(contains_singletons(C))); dmsg(trace_or_throw(contains_singletons(C)))),fail.
nonground_throw_else_fail(C):- not_is_release,not(ground(C)),!,( (test_tl(t_l:already_in_file_term_expansion) ->dmsg(not_ground(C)); trace_or_throw(not_ground(C)))),fail.

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
requires_storage(_,_,t_l:consulting_sources):-t_l:consulting_sources,mpred_may_expand,!.
% requires_storage(_,_,t_l:consulting_sources):-t_l:consulting_sources,in_file_expansion.

special_wrapper_functor(call_mpred_body,direct_to_prolog).
special_wrapper_functor(body_req,direct_to_prolog).
special_wrapper_functor(user:mpred_provide_setup,direct_to_prolog).
special_wrapper_functor(call_provided_mpred_storage_op,direct_to_prolog).
special_wrapper_functor(loop_check,meta).
special_wrapper_functor(loop_check_term,meta).
%special_wrapper_functor(pttp_req).


make_body_clause(_Head,Body,Body):-atomic(Body),!.
make_body_clause(_Head,Body,Body):-special_wrapper_body(Body,_Why),!.
make_body_clause(Head,Body,call_mpred_body(Head,Body)).

special_head(_,F,Why):-special_head0(F,Why),!,show_call_failure(not(isa(F,prologDynamic))).
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
del(C):- fully_expand(change(retract,a),C,C0),mpred_maptree(del0,C0).
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
  fully_expand(change(retract,all),P,PL),mpred_maptree(clr0,PL).

clr0(P):- 
  forall(on_x_rtrace(P), ((forall( mpred_rem2(P), true)),nop((sanity((not(mpred_tms_supported(local,P)),must(\+(P)))))))).


% -  preq(Query) = query with P note
preq(P,C0):- agenda_do_prequery,!,no_repeats(C0,mpred_op(query(t,P),C0)).

% -  req(Query) = Normal query
req(C0):- nop(dmsg(req(C0))), !,preq(req,/*to_exp*/(C0)).

% -  mreq(Query) = Forced Full query
mreq(C0):- nop(dmsg(mreq(C0))), agenda_rescan_for_module_ready,
   no_loop_check(w_tl([-infInstanceOnly(_),-t_l:infAssertedOnly(_),-t_l:noRandomValues(_)],
     preq(must,/*to_exp*/(C0)))).

% -  ireq(Query) = Normal query (May not use second order logic) (must be asserted on isntance) (used mainly by 2nd order logic to avoid looping)
ireq(C0):- nop(dmsg(ireq(C0))), 
  agenda_rescan_for_module_ready,
   no_loop_check(w_tl([+infInstanceOnly(_), +t_l:infAssertedOnly(_),+t_l:noRandomValues(_)],preq(ireq,/*to_exp*/(C0)))).

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
% t_l:add_thread_override(A):-add_from_macropred(A),!.

:-export(((add)/1)).
:-mpred_trace_nochilds((add)/1).
add(A):- var(A),!,trace_or_throw(var_add(A)).
add(end_of_file):-!.
add(grid_key(KW=COL)):- !, add(typeHasGlyph(COL,KW)).
% add(Term):- unnumbervars(Term,TermE), Term \=@= TermE,!,add(TermE).
% add(Term):-  forall(do_expand_args(isEach,Term,O),add_0(O)),!.
add(TermIn):- fully_expand(change(assert,add),TermIn,Term),add_0(Term).

add_0(A):- is_ftVar(A),!,trace_or_throw(var_add(A)).
add_0(((H1,H2):-B)):-!,add_0((H1:-B)),add_0((H2:-B)).
add_0(((H1,H2))):-!,add_0((H1)),add_0((H2)).
add_0(dynamic(Term)):- !,must(get_arity(Term,F,A)), must(dynamic(F/A)).
add_0(A):- A =(:-(_Term)), !, must(add_fast(A)).
% add_0(C0):-check_override(add(C0)),!.
% add_0(Skipped):- ground(Skipped),implied_skipped(Skipped),!. % ,dmsg(implied_skipped(Skipped)).
%add_0(C0):- ignore((ground(C0),asserta(user:already_added_this_round(C0)))),!,must(mpred_add_fast(C0)),!.
add_0(C0):- must(mpred_add_fast(C0)),!.
add_0(A):-trace_or_throw(fmt('add/1 is failing ~q.',[A])).


implied_skipped(genls(C0,C0)).
implied_skipped(props(_,[])).
implied_skipped(Skipped):-compound(Skipped), not(functor(Skipped,_,1)),fail, (t(Skipped);out_of_mpred_t(Skipped)).
%implied_skipped(Skipped):-user:already_added_this_round(Skipped),(is_asserted(Skipped)).


mpred_numbervars_with_names(Term):- term_variables(Term,Vars),mpred_name_variables(Vars),!,numbervars(Vars,91,_,[attvar(skip),singletons(true)]),!.

mpred_name_variables([]).
mpred_name_variables([Var|Vars]):-
   (var_property(Var, name(Name)) -> Var = '$VAR'(Name) ; true),
   mpred_name_variables(Vars).


:-export(mpred_add_fast/1).
% -  add(Assertion)
% mpred_add_fast(C0):- must_det((mpred_add_fast(C0), xtreme_debug(once(ireq(C0);(with_all_dmsg((debug(blackboard),show_call(mpred_add_fast(C0)),rtrace(mpred_add_fast(C0)),dtrace(ireq(C0))))))))),!.
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


/*
update_single_valued_arg(P,N):- arg(N,P,UPDATE),replace_arg(P,N,OLD,Q),
  (is_relative(UPDATE)->
     must_det_l((Q,update_value(OLD,UPDATE,NEW),\+ is_relative(NEW), replace_arg(Q,N,NEW,R),enqueue(\+Q),enqueue(R)));
     forall((Q,UPDATE\=OLD),mpred_enqueue(\+Q))),!.
*/

/*

% assert_with to change(CA1,CB2) singlevalue pred
:-export((db_assert_sv/4)).
%db_assert_sv(_Must,C,F,A):- throw_if_true_else_fail(contains_singletons(C),db_assert_sv(C,F,A)).

db_assert_sv(C):- get_functor(C,F,A), db_assert_sv(must,C,F,A),!.

db_assert_sv(Must,C,F,A):- ex, ignore(( loop_check(db_assert_sv_ilc(Must,C,F,A),true))).

:-export((db_assert_sv_ilc/4)).
db_assert_sv_ilc(Must,C,F,A):- arg(A,C,UPDATE),is_relative(UPDATE),db_assert_sv_now(Must,C,F,A,UPDATE),!.

:-export(db_assert_sv_now/5).

db_assert_sv_now(Must,C,F,A,    +UPDATE):-!,  db_assert_sv_update(Must,C,F,A,+UPDATE).
db_assert_sv_now(Must,C,F,A,    -UPDATE):-!,  db_assert_sv_update(Must,C,F,A,-UPDATE).
db_assert_sv_now(Must,C,F,A,    V):- is_relative(V),!,  db_assert_sv_update(Must,C,F,A,+V).
db_assert_sv_now(Must,C,F,A, UPDATE):- has_free_args(db_assert_sv_now(Must,C,F,A, UPDATE)),!,trace_or_throw(var_db_assert_sv_now(Must,C,F,A, UPDATE)).
db_assert_sv_now(Must,C,F,A, NEGREPLACE):- number(NEGREPLACE),NEGREPLACE<0, !,db_assert_sv_replace(Must,C,F,A,NEGREPLACE).
db_assert_sv_now(Must,C,F,A, -(+(UPDATE))):-!,db_assert_sv_update(Must,C,F,A,-UPDATE).
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

*/

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
   add(CNEW),
   verify_sanity(confirm_hook(CNEW:NEW=@=CNOW:NOW)),!.

db_must_asserta_confirmed_sv(CNEW,A,NEW):-dmsg(unconfirmed(db_must_asserta_confirmed_sv(CNEW,A,NEW))).

with_assert_op_override(Op,Call):-w_tl(t_l:assert_op_override(Op),Call).

test_expand_units(IN):-fully_expand(query(t,must),IN,OUT),dmsg(test_expand_units((IN->OUT))).



mpred_modify(Op,                 G):- (var(Op);var(G)),!,trace_or_throw(var_database_modify_op(Op,  G )).
mpred_modify(Op,                 G):- \+ skip_is_asserted_expansion(G),G\=meta_argtypes(_),fully_expand_warn(Op,G,GG),not_variant(G,GG),!,mpred_modify(Op, GG ),!.
mpred_modify(_,  (:-include(FILE))):- !,must(ensure_mpred_file_loaded(FILE)).
mpred_modify(Op,  (:-(G))         ):- !,must(with_assert_op_override(Op,on_x_rtrace(G))).
mpred_modify(P,                  G):- t_l:noDBaseMODs(_),!,dmsg(noDBaseMODs(P,G)).
%mpred_modify(Op,                 G):- mpred_head_expansion(clause,G,GG),not_variant(G,GG),database_modify_0(Op, GG ),!.
mpred_modify(Op,                 G):- database_modify_0(Op,G ),!.
mpred_modify(Op,                 G):- trace_or_throw(unknown_database_modify(Op,G)).


database_modify_0(Op,                       M:G):- atom(M),!, database_modify_0(Op,G).
database_modify_0(Op,                   (C1,C2)):- !, must(database_modify_0(Op,C1)), must(database_modify_0(Op,C2)).
database_modify_0(change(Assert,AorZ),(G:-TRUE)):- is_true(TRUE),!,database_modify_0(change(Assert,AorZ),G).
database_modify_0(change(retract,a),          G):- hooked_retract(G).
database_modify_0(change(retract,one),        G):- hooked_retract(G).
database_modify_0(change(retract,_),          G):- hooked_retractall(G).
database_modify_0(change(assert,AZ),          G):- singletons_throw_else_fail(assert(AZ,G)).
database_modify_0(change(assert,AZ),          G):- database_modify_assert(change(assert,AZ),G).


% database_modify_assert(change(assert,_),        G):- ( \+ \+ is_asserted(G)),must(variant(G,GG)),!.
% database_modify_assert(change(assert,AZ),       G):- expire_pre_change(AZ,GG),fail.
database_modify_assert(change(assert,_AorZ),       G):- !,mpred_add(G).
database_modify_assert(change(assert,AorZ),       G):- 
 get_functor(G,F,_),!,
   (AorZ == a -> hooked_asserta(G);
    AorZ == z ->  hooked_assertz(G);
    isa(F,prologOrdered) -> database_modify_assert(change(assert,z),G);
    isa(F,prologSingleValued) -> database_modify_assert(change(assert,a),G);
      hooked_asserta(G)).

% ========================================
% only place ever should actual game database be changed from
% ========================================

hooked_asserta(G):- loop_check(mpred_modify(change(assert,a),G),mpred_adda(G)).

hooked_assertz(G):- loop_check(mpred_modify(change(assert,z),G),mpred_addz(G)).

hooked_retract(G):-  Op = change(retract,a),
                   ignore(slow_sanity(ignore(show_call_failure((mpred_op(is_asserted,G)))))),
                   slow_sanity(not(singletons_throw_else_fail(hooked_retract(G)))),
                   slow_sanity(ignore(((ground(G), once(show_call_failure((is_asserted(G)))))))),
                   must_storage_op(Op,G),expire_post_change( Op,G),
                   sanity(ignore(show_call_failure(not_asserted((G))))),
                   loop_check(run_database_hooks_depth_1(change(retract,a),G),true).

hooked_retractall(G):- Op = change(retract,all),
                   slow_sanity(ignore(((ground(G), once(show_call_failure((is_asserted(G)))))))),
                   must_storage_op(Op,G),expire_post_change( Op,G),
                   sanity(ignore(show_call_failure(not_asserted((G))))),
                   loop_check(run_database_hooks_depth_1(change(retract,all),G),true).



user:mpred_provide_storage_op(Op,G):- get_functor(G,F,A),user:mpred_provide_storage_op(Op,G,F,A).

user:mpred_provide_storage_op(Op,G, F,_A):- t(pfcControlled,F),!,loop_check(prolog_mpred_provide_storage_op(Op,G)).
user:mpred_provide_storage_op(Op,G, F,_A):- t(prologDynamic,F),!,loop_check(mpred_mpred_provide_storage_op(Op,G)).
user:mpred_provide_storage_op(Op,G,_F,_A):- loop_check(prolog_mpred_provide_storage_op(Op,G)).

%user:mpred_provide_storage_op(Op,G):- (loop_check(isa_mpred_provide_storage_op(Op,G))).
%user:mpred_provide_storage_op(Op,G):- Op\=change(_,_), (call_no_cuts(user:mpred_provide_storage_clauses(G,true,_Proof))).

must_storage_op(Op,G):- doall(must(may_storage_op(Op,G))).

may_storage_op(Op,G):-call_no_cuts(user:mpred_provide_storage_op(Op,G)).


:- meta_predicate hooked_asserta(+), hooked_assertz(+), hooked_retract(+), hooked_retractall(+).

:- meta_predicate del(-),clr(-),add(-),req(-), fully_expand(-,-,-).

% Found new meta-predicates in iteration 1 (0.281 sec)
%:- meta_predicate mpred_modify(?,?,?,0).





%retract_all((G:-B)) :-!, forall(clause(G,B,Ref),erase(Ref)).
retract_all(HB) :- ignore((retract(HB),fail)).


is_static_pred(Head:-_):-!,predicate_property(Head,_),not(predicate_property(Head,dynamic)).
is_static_pred(Head):-  predicate_property(Head,static),!.
is_static_pred(Head):- predicate_property(Head,_), !, \+ (predicate_property(Head,dynamic)).
is_static_pred(Head):-  predicate_property(Head,meta_predicate),!.

prolog_mpred_provide_storage_op(Op,G):- G\=isa(_,_), get_functor(G,F),user:mpred_prop(F,prologDynamic),!, prolog_op(Op,G).
prolog_mpred_provide_storage_op(Op,G):- G\=isa(_,_), get_functor(G,F),not(user:mpred_prop(F,prologHybrid)),!,current_predicate(_,G), prolog_op(Op,G).
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
prolog_op(Op,G):- reduce_mpred_op(Op,Op2), on_x_rtrace(call(Op2,G)).



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

ensure_dynamic(Var):-var(Var),!,trace_or_throw(var_ensure_dynamic(Var)).
ensure_dynamic(M:H1):-atom(M),!,ensure_dynamic(H1).
ensure_dynamic((H1,H2)):-!,ensure_dynamic(H1),ensure_dynamic(H2).
ensure_dynamic((H1;H2)):-!,ensure_dynamic(H1),ensure_dynamic(H2).
ensure_dynamic((H1:-_)):-!,ensure_dynamic(H1).
ensure_dynamic(':-'(_)):-!.
ensure_dynamic(Head):- Head\=isa(_,_),
   get_functor(Head,F,A),
   functor(PF,F,A),
   (\+ predicate_property(PF,_)->show_call((dynamic(F/A),multifile(F/A),export(F/A)));
   (is_static_pred(PF)-> 
     ((listing(F/A),dmsg(want_to_assert(ensure_dynamic(Head),decl_mpred_prolog(F,A,Head))),nop(dtrace))); true)).

