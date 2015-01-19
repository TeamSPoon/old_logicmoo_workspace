
:- dynamic_multifile_exported(provide_mpred_storage_impl/4).
:- dynamic_multifile_exported(provide_mpred_storage_clauses/3).
:- dynamic_multifile_exported(provide_mpred_read_attributes/3).
:- dynamic_multifile_exported(provide_mpred_write_attributes/2).
:- dynamic_multifile_exported(provide_mpred_storage_ops/3).


tPredStubImpl(prologOnly).
tPredStubImpl(prologHybrid).
tPredStubImpl(prologPTTP).


% ================================================================================
% INSTALL STORAGE STUBS
% ================================================================================


user:decl_database_hook(assert(_),mpred_prop(F,StubType)):- add_storage_stub(StubType,F).
user:decl_database_hook(assert(_),mpred_prop(F,predStubType(StubType))):- add_storage_stub(StubType,F).
user:decl_database_hook(assert(_),mudIsa(F,(StubType))):- add_storage_stub(StubType,F).
user:decl_database_hook(assert(_),mudIsa(F,predStubType(StubType))):- add_storage_stub(StubType,F).

decl_mpred_stubcol(F,StubType):- must(tPredStubImpl(StubType)),decl_mpred(F,predStubType(StubType)),decl_mpred(F,StubType).

add_storage_stub(StubType,F):- mpred_prop(F,hasStub(StubType)),!.
add_storage_stub(StubType,C):- compound(C),get_functor(C,F,A),!,ensure_arity(F,A),add_storage_stub(StubType,F,A).
add_storage_stub(StubType,F):- ensure_arity(F,A),add_storage_stub(StubType,F,A).

add_storage_stub(StubType,F,_):- mpred_prop(F,hasStub(StubType)),must(tPredStubImpl(StubType)),!.
add_storage_stub(StubType,F,A):- tPredStubImpl(StubType),
  must_det_l((          
         ensure_arity(F,A),         
         provide_clauses_list(F,A,HBLIST),
         retractall(mpred_prop(F,hasStub(_))),
         forall(tPredStubImpl(Impl),retractall(mpred_prop(F,Impl))),

         asserta_if_new(mpred_prop(F,hasStub(StubType))),         
         asserta_if_new(mpred_prop(F,StubType)),         
         abolish(F,A),         
         must(get_provide_mpred_storage_impl(StubType,M,F,A)),
         ignore(M=user),
         (functor(P,F,A), not(static_predicate(M,F,A)) -> 
                     assert_if_new((P:-provide_mpred_storage_ops(StubType,call(_),P))) ; dmsg(static_predicate(M,F,A))),
         forall(member((H :- B),HBLIST),add((H:-B))))).


get_provide_mpred_storage_impl(StubType,M,F,A):-call_no_cuts(provide_mpred_storage_impl(StubType,M,F,A)).


add_mpred_universal_stub(StubType,F,A):- functor(P,F,A), ignore((not(static_predicate(user,F,A)), 
  asserta_new((P:-provide_mpred_storage_ops(StubType,call(_),P))),assertz_if_new(mpred_prop(F,hasStub(StubType))))),!.


/*
EVENTUALLY MOVE TO PDDL
  decl_database_hook(assert(_),mpred_prop(F,ENV)):- ((assert_if_new(env_precol(ENV)),mpred_arity(F,A), doall(provide_mpred_write_attributes(F,A,ENV)))).

*/
  

create_stub_really(M,F,A,StubType,REQ,DBASE_T):- mpred_stub(REQ, DBASE_T, F,A,HEAD,BODY), impl_stub_really(M,F,A,StubType,HEAD,BODY).

mpred_stub(REQ, DBASE_T, F,A,HEAD, (!, OUT ) ):- functor(HEAD,F,A),HEAD=..[F|ARGS],HEAD_T=..[DBASE_T,F|ARGS],OUT =.. [REQ,F,A,HEAD,HEAD_T].

impl_stub_really(M,F,_,StubType,HEAD,BODY):- mpred_prop(F,hasStub(StubType)), M:is_asserted_clause((HEAD :- BODY)),!.
impl_stub_really(M,F,A,StubType,HEAD,BODY):-
   must_det_l([
         ensure_arity(F,A),
         provide_clauses_list(F,A,HBLIST),

         retractall(mpred_prop(F,hasStub(_))),
         forall(tPredStubImpl(Impl),retractall(mpred_prop(F,Impl))),
         forall(tPredStubImpl(Impl),retractall(mpred_prop(F,predStubType(Impl)))),

         asserta_if_new(mpred_prop(F,(StubType))),
         asserta_if_new(mpred_prop(F,predStubType(StubType))),
         asserta_if_new(mpred_prop(F,hasStub(StubType))),
         abolish(F,A),
         dynamic_multifile_exported(M:F/A),   
         asserta_if_new((HEAD :- BODY)),
         forall(member((H :- B),HBLIST),add((H:-B))),
         call(compile_predicates([HEAD]))]).



% ================================================================================
% INSTALL MISSING STUBS
% ================================================================================

%:-rescan_mpred_props.

% pass 2

mpred_missing_stubs(F,StubType):-mpred_arity(F,_),mpred_prop(F,predStubType(StubType)),not(mpred_prop(F,hasStub(StubType))).

scan_missing_stubs(F):-
   ignore((forall(mpred_missing_stubs(F,StubType),(mpred_arity(F,A),show_call(add_storage_stub(StubType,F,A)))))).

:-swi_export(rescan_missing_stubs/0).
rescan_missing_stubs:-loop_check_local(time_call(rescan_missing_stubs_lc),true).
rescan_missing_stubs_lc:- once(thglobal:use_cyc_database), once(with_assertions(thlocal:useOnlyExternalDBs,forall((kb_t(arity(F,A)),A>1,good_pred_relation_name(F,A),not(mpred_arity(F,A))),with_no_dmsg(decl_mpred_mfa,decl_mpred_hybrid(F,A))))),fail.
rescan_missing_stubs_lc:- hotrace((doall((mpred_missing_stubs(F,StubType),mpred_arity(F,A),add_storage_stub(StubType,F,A))))).


:-swi_export(rescan_mpred_props/0).

rescan_mpred_props:- loop_check(rescan_mpred_props_lc,true).
rescan_mpred_props_lc:-rescan_duplicated_facts(user,mpred_prop(_,_)),fail.
rescan_mpred_props_lc:-time(forall(mpred_prop_ordered(Pred,Prop),hooked_asserta(mpred_prop(Pred,Prop)))),fail.
rescan_mpred_props_lc:-rescan_missing_stubs.
rescan_mpred_props_lc.


% ================================================================================
% GRABOUT STORAGE STUB CLAUSES
% ================================================================================
provide_clauses_list(F,A,HBLIST):-
   ensure_arity(F,A),functor(H,F,A), 
            findall((H :- B),no_repeats((H:-B),call_no_cuts(provide_mpred_storage_clauses(H,B))),HBLIST).

% ================================================================================
% ASSERT INTO STORAGE STUBS
% ================================================================================

:-swi_export((assertz_clause/1)).
assertz_to_system(Before):- expand_term(Before,Replaced),Before \=@= Replaced,!, assertz_to_system(Replaced).
assertz_to_system((':-'(Body))):-!,must_det(show_call(Body)),!.
assertz_to_system((Head :- Body)):- !,assertz_clause(Head,Body),!.
assertz_to_system(C):- assertz_clause(C,true),!.

assertz_clause(Head,Body):- (var(Head);var(Body)),!,trace_or_throw(var_assertz_clause(Head,Body)).
assertz_clause(Head,Body):- clause_asserted((':-'(Head,Body))),!.
assertz_clause(Head,Body):- ExpIn = (Head:-Body),  expand_term(ExpIn,Exp),Exp \=@= ExpIn,!,assertz_to_system(Exp),!.
assertz_clause(Head,Body):- get_mpred_storage_type(Head,Type),!,must_det(assertz_to_storage(Type,Head,Body)),!.


assertz_to_storage((prologOnly),Head,Body):- must_det(assertz_if_new_clause(Head,Body)),dmsg(used_clause_as_prologOnly(Head,Body)).
assertz_to_storage((prologPTTP),Head,Body):- must_det(pttp_assert((Head:-Body))),dmsg(used_clause_as_prologPTTP(Head,Body)).
assertz_to_storage((static),Head,Body):- must_det(assertz_if_new_clause(Head,Body)),trace_or_throw(eRROR_maybe_used_clause_as_prologOnly(Head,Body)).
assertz_to_storage(_,Head,true):- !,with_assertions(thlocal:adding_from_srcfile,add(Head)),!.
assertz_to_storage(Type,Head,BodyIn):- once(make_body_clause(Head,BodyIn,Body)),must_det(assertz_if_new_clause(Head,Body)),dmsg(used_clause_as_unknown(Type,Head,Body)).
assertz_to_storage(Type,Head,Body):- must_det(assertz_if_new_clause(Head,Body)),dmsg(used_clause_as(Type,Head,Body)).


% ================================================================================
% DETECT STORAGE STUBS
% ================================================================================

:- decl_mpred(argSingleValueDefault, 3).
:- decl_mpred(predModule, 2).

requires_storage((Head :- Body)):-!, requires_storage(Head,Body).
requires_storage(C):- requires_storage(C,true).

requires_storage(M:H,B):-atomic(M),!,requires_storage(H,B).
requires_storage(H,_B):-compound(H),functor_catch(H,F,_),not(get_mpred_prop(F,prologOnly)),!,special_head(H,F),!.
requires_storage(H,M:B):-atomic(M),!,requires_storage(H,B).

special_wrapper_functor(call_mpred_body).
special_wrapper_functor(body_req).
special_wrapper_functor(loop_check).
special_wrapper_functor(loop_check_term).
special_wrapper_functor(loop_check_clauses).

make_body_clause(_Head,Body,Body):-atomic(Body),!.
make_body_clause(_Head,Body,Body):-special_wrapper_body(Body),!.
make_body_clause(Head,Body,call_mpred_body(Head,Body)).


special_head(_,F):-mpred_prop(F,prologOnly),!,fail.
special_head(_,F):-mpred_prop(F,prologHybrid).
special_head(_,F):-mpred_prop(F,prologPTTP).
special_head(_,F):-mpred_prop(F,hasStub(_)).


ensure_clause(HEAD,_,_,_):-functor_safe(HEAD,F,_),mpred_prop(F,prologOnly),!,trace_or_throw(mpred_prop(F,prologOnly)).
ensure_clause(HEAD,_,_,BDY):- clause_asserted(HEAD , BDY),!.
% ensure_clause(HEAD,F,A,_):-pred_prologOnly(F,A), !.
ensure_clause(HEAD,F,_A,BDY):- assertz((HEAD:-BDY)),
   get_body_functor(BDY,BF,_),
   hooked_asserta(mpred_prop(F,prologHybrid)),
   hooked_asserta(mpred_prop(F,hasStub(BF))),
   % this is just to catch asserts at these predicates that are supposed to be contained.. We dont really want them compiled
   nop(((compile_predicates([HEAD])),must_det(static_predicate(HEAD)))).

special_wrapper_body(W):-get_body_functor(W,F,_),!,special_wrapper_functor(F).

get_body_functor(Var,_,call):-var(Var),!.
get_body_functor((M:BDY),BF,A):-atom(M),!,get_body_functor(BDY,BF,A).
get_body_functor((!,BDY),BF,A):-!,get_body_functor(BDY,BF,A).
get_body_functor(call(BDY),BF,A):-!,get_body_functor(BDY,BF,A).
get_body_functor(once(BDY),BF,A):-!,get_body_functor(BDY,BF,A).
get_body_functor((BDY1;BDY2),BF,A):-!, (get_body_functor(BDY1,BF,A);get_body_functor(BDY2,BF,A)).
get_body_functor((BDY1,BDY2),BF,A):-!, (get_body_functor(BDY1,BF,A);get_body_functor(BDY2,BF,A)).
get_body_functor(BDY,BF,A):-functor(BDY,BF,A).



