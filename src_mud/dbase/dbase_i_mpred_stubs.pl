/** <module> dbase_i_mpred_stubs.
% Provides a prolog dabase in these predicates...
%
%  Manages the hybrid_tPredStubImpl/1 in various files
%  
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/

:- include(dbase_i_header).

:- export correctArgsIsa/3.

hybrid_tPredStubImpl(prologHybrid).
hybrid_tPredStubImpl(prologPTTP).
hybrid_tPredStubImpl(prologPfc).
hybrid_tPredStubImpl(prologSNARK).


get_pifunctor(Head,PHead):-must(get_pifunctor(Head,PHead,_,_)).
get_pifunctor(Head,PHead,F):-must(get_pifunctor(Head,PHead,F,_)).

get_pifunctor(Head,PHead,F,A):-var(Head),!,sanity(atom(F)),must(ensure_arity(F,A)),functor(PHead,F,A),ignore(PHead=Head).
get_pifunctor(Head,PHead,F,A):-get_functor(Head,F,A),functor(PHead,F,A),ignore(PHead=Head),!.
get_pifunctor(Head,PHead,F,A):-atom(Head),ensure_arity(Head,A),!,get_pifunctor(Head/A,PHead,F,A).

% ================================================================================
% INSTALL STORAGE STUBS
% ================================================================================

maybe_storage_stub(F,StubType):- hybrid_tPredStubImpl(StubType),not((StubType==prologOnly)),mpred_arity(F,A),must(ensure_universal_stub(F/A)).

% user:decl_database_hook(change(assert,_),user:mpred_prop(F,StubType)):- maybe_storage_stub(F,StubType).
% user:decl_database_hook(change(assert,_),isa(F,StubType)):- maybe_storage_stub(F,StubType).
% user:decl_database_hook(change(assert,_),mpred_arity(F,StubType)):-  hybrid_tPredStubImpl(StubType),user:mpred_prop(F,StubType),must(ensure_universal_stub(F/A)).


% has_storage_stub(Head):- !.
has_storage_stub(Head):-  thglobal:pfcManageHybrids,!,   
      get_pifunctor(Head,PHead,_F),
      create_stub_body(PHead,Body),
      (predicate_property(PHead,dynamic);user:clause_safe(PHead,Body)),!.
      
      

has_storage_stub(Head):-       
      get_pifunctor(Head,PHead,F),
      create_stub_body(PHead,Body),
      user:clause(PHead,Body),
      (((show_call_failure(predicate_property(PHead,number_of_clauses(1)))),(show_call_failure(predicate_property(PHead,number_of_rules(1)))))
        -> true; (listing(PHead),trace)),
      !,
      must((user:mpred_prop(F,predStub(StubType)),
      must(hybrid_tPredStubImpl(StubType)))).
      

% must_have_storage_stub(Head):-!.
must_have_storage_stub(Head):-
 (has_storage_stub(Head)->true;(get_functor(Head,F),listing(F),term_listing(F),ensure_universal_stub(Head))).

missing_stub(Head):-has_storage_stub(Head),!,fail.
missing_stub(Head):-not(predicate_property(Head,public)),listing(Head).

create_stub_body(Head,Stub):-create_stub_body(call(conjecture),Head,Stub).
% return fail to cut and fail
create_stub_body(call(conjecture),Head,Stub):- Stub = (call_provided_mpred_storage_op(call(conjecture),Head,Then),
   ((Then=(!,Whatnot))->
     (!,Whatnot);
      Then)).

erase_mpred_storage_op(Head):-
   create_stub_body(_,Head,Stub),
   doall(retract((Head:- Stub))),
   foreach(clause(Head,Stub,Ref),erase(Ref)).

really_add_mpred_storage_op(Head):-
   create_stub_body(call(conjecture),Head,Stub),
   asserta_if_new((Head:- Stub)).


renumbervarZ((H:-B),GGG):-is_true(B),!,copy_term(H,GG),unnumbervars(GG,GGG),numbervars(GGG,0,_).
renumbervarZ(H,GGG):-copy_term(H,GG),unnumbervars(GG,GGG),numbervars(GGG,0,_).
is_same_clauses(Head,NEWHBLISTN,HBLISTN):-
   maplist(renumbervarZ,HBLISTN,HBLIST),
   maplist(renumbervarZ,NEWHBLISTN,NEWHBLIST),
   sort(NEWHBLIST,NEWHBLISTS),
   sort(HBLIST,HBLISTS),
   length(NEWHBLIST,LN),
   length(HBLIST,LO),
   list_difference_eq(HBLIST,NEWHBLIST,MISSING),length(MISSING,LM),
   list_difference_eq(NEWHBLIST,HBLIST,EXTRA),length(EXTRA,LE),
   (NEWHBLIST=@=HBLIST -> true ;((NEWHBLISTS=@=HBLISTS;LM=0) -> wdmsg(trace_or_throw(must_same_clauses(Head,[extra(LE)|EXTRA],[missing(LM)|MISSING]))) ;  
     ((wdmsg((trace_or_throw(must_same_clauses(Head,[LO|HBLIST],[LN|NEWHBLIST],[extra(LE)|EXTRA],[missing(LM)|MISSING])))),!,fail)))).

must_same_clauses(Head,HBLISTN):-
   provide_clauses_list(Head,NEWHBLISTN),
   must(is_same_clauses(Head,NEWHBLISTN,HBLISTN)).

is_same_clauses(Head,HBLISTN):-
   provide_clauses_list(Head,NEWHBLISTN),
   must(is_same_clauses(Head,NEWHBLISTN,HBLISTN)).

/*

is_asserted  checks using =@=
call(conjecture)
call(once)  % TODO
change(assert,a) asserts first if =@= is not first
change(assert,z) asserts last if no =@=
change( retract,one)  using =
change( retract,all)  using =

same_functors(Head1,Head2):-must_det(get_functor(Head1,F1,A1)),must_det(get_functor(Head2,F2,A2)),!,F1=F2,A1=A2.
good_for_hybrid(H,F):- not(user:mpred_prop(F,_ANY_)),predicate_property(H,number_of_clauses(0)),predicate_property(H,dynamic).
ensure_exists(Head):-get_pifunctor(Head,PHead,F),get_functor(Head,F,A),(predicate_property(PHead,dynamic)->true;(predicate_property(PHead,_)->dmsg(warn(static_pred,F/A));dynamic(F/A))).

*/


% -- CODEBLOCK
is_tCol(V):-is_ftVar(V),!,fail.
is_tCol(tCol).
is_tCol(F):- user:mpred_prop(F,tCol);hasInstance(tCol,F);hasInstance(F,_).

is_proc(V):-is_ftVar(V),!,fail.
is_proc(F):- functor(P,F,1),predicate_property(P,_),must(not(user:mpred_prop(F,tCol))).

% -- CODEBLOCK
is_call_op(Var):-var(Var),!,trace_or_throw(var_is_call_op(Var)).
is_call_op(call(_)):-!.
is_call_op(query(_,_)):-!.
is_call_op(call).

is_non_call_op(Op):-is_mpred_op(Op),not(is_call_op(Op)).

% -- CODEBLOCK
is_mpred_change_op(change(_,_)).

% -- CODEBLOCK
is_mpred_op(Op):-is_mpred_change_op(Op).
is_mpred_op(call(_)).
is_mpred_op(query(_,_)).
is_mpred_op(clauses(_)).

% -- CODEBLOCK
:-export(last_arg_ground/1).
last_arg_ground(HEAD):-compound(HEAD),functor(HEAD,F,A),last_arg_ground(F, A, HEAD),!.
last_arg_ground(mud_test,_,_).
last_arg_ground(_,A,_):-A>2,!.
last_arg_ground(_,A,HEAD):-arg(A,HEAD,Arg),!,ground(Arg).


call_provided_mpred_storage_op(call(_),H,true):-was_isa(H,I,C),!,isa_asserted(I,C).
call_provided_mpred_storage_op(Op,H,true):-!,no_repeats_old(loop_check(may_storage_op(Op,H),is_asserted(H))).


test_call_cut:- X=!,dmsg(testing_call_cut),X.
test_call_cut:- throw(test_failed(testing_call_cut)).
% :-doall(test_call_cut).

%change(X,B):-dbase_op(X,B),!.
%change(X,B,C):-dbase_op(change(X,B),C),!.
%retract(X,Y):-dbase_op(change(retract,X),Y),!.


must_op(Op,     H ):- (var(Op);var(H)),!,trace_or_throw(var_database_op0(Op,  H )).
must_op(Op,     H ):- once(fully_expand(Op,H,HH)),H\=@=HH,!,must_op(Op, HH).
must_op(change(assert,_),Call):-!,must(Call),!.
must_op(change( retract,_),Call):-!,must(Call),!.
must_op(clauses(_),Call):-!,loop_check(debugOnError(Call)).
must_op(call(_),Call):-!,loop_check(debugOnError(Call)).
must_op(_,Call):-!,loop_check(debugOnError(Call)).

call_wdmsg(P,DB):- thlocal:noDBaseMODs(_),!,wdmsg(error(noDBaseMODs(P,DB))).
call_wdmsg(P,DB):- get_functor(DB,F,A), call_wdmsg(P,DB,F,A).

call_wdmsg(P,DB,dbase_t,_A):-!, append_term(P,DB,CALL),dmsg((CALL)),mpred_call(CALL).
call_wdmsg(P,MP,F,A):- user:mpred_prop(F,prologHybrid),must(A>1),into_functor_form(dbase_t,MP,DB),!, append_term(P,DB,CALL),dmsg(info(CALL)),!,mpred_call(CALL).
call_wdmsg(P,MP,F,A):- not(user:mpred_prop(F,prologOnly)),decl_mpred_hybrid(F/A), into_functor_form(dbase_t,MP,DB),!, append_term(P,DB,CALL),dmsg(info(CALL)),!,mpred_call(CALL).
call_wdmsg(P,DB,F,_):- append_term(P,DB,CALL),dmsg(info(CALL)),must(user:mpred_prop(F,prologOnly)),!,mpred_call(CALL).
%call_wdmsg(P,DB,S,_):-  dtrace((append_term(P,DB,CALL),dmsg((CALL)),mpred_call(CALL))).


% ================================================================================
% INSTALL MISSING STUBS
% ================================================================================

%:-agenda_rescan_mpred_props.

% pass 2
scan_missing_stubs(F):-
   ignore((forall(mpred_missing_stubs(F,A),
      (mpred_arity(F,A),show_call(ensure_universal_stub(F/A)))))).

mpred_missing_stubs(F,A):-prologHybrid = StubType, hybrid_tPredStubImpl(StubType),user:mpred_prop(F,StubType),must(mpred_arity(F,A)),not(has_storage_stub(F/A)).


:-assertz_if_new(call_OnEachLoad(rescan_missing_stubs)).

:-export(rescan_missing_stubs/0).
% rescan_missing_stubs:-no_rescans,!.
rescan_missing_stubs:-loop_check_local(time_call(rescan_missing_stubs_ilc),true).
rescan_missing_stubs_ilc:- once(thglobal:use_cyc_database), once(with_assertions(thlocal:useOnlyExternalDBs,forall((kb_t(arity(F,A)),A>1,good_pred_relation_name(F,A),not(mpred_arity(F,A))),with_no_dmsg(decl_mpred_mfa,decl_mpred_hybrid(F,A))))),fail.
rescan_missing_stubs_ilc:- hotrace((doall((mpred_missing_stubs(F,A),mpred_arity(F,A),ensure_universal_stub(F/A))))).

no_rescans.

:-export(agenda_rescan_mpred_props/0).

agenda_rescan_mpred_props:- loop_check(rescan_mpred_props_ilc,true).
rescan_mpred_props_ilc:-no_rescans,!.
rescan_mpred_props_ilc:-rescan_duplicated_facts(user,user:mpred_prop(_,_)),fail.
rescan_mpred_props_ilc:-time(forall(mpred_prop_ordered(Pred,Prop),hooked_asserta(user:mpred_prop(Pred,Prop)))),fail.
rescan_mpred_props_ilc:-rescan_missing_stubs.
rescan_mpred_props_ilc.


% ================================================================================
% GRABOUT STORAGE STUB CLAUSES
% ================================================================================
provide_clauses_list(Head,HBLISTO):- get_pifunctor(Head,PHead,_),  
  findall((PHead :- B),
    no_repeats_old([PHead:B],((call_no_cuts(user:provide_mpred_storage_clauses(_,PHead,B,Proof)),is_source_proof(Proof)))),
   HBLIST),
   create_stub_body(PHead,Stub),
   delete(HBLIST,Stub,HBLISTO),!.

