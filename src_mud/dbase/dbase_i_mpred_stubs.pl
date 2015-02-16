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

:- dynamic_multifile_exported correctArgsIsa/3.

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

% user:decl_database_hook(change(assert,_),mpred_prop(F,StubType)):- maybe_storage_stub(F,StubType).
% user:decl_database_hook(change(assert,_),isa(F,StubType)):- maybe_storage_stub(F,StubType).
% user:decl_database_hook(change(assert,_),mpred_arity(F,StubType)):-  hybrid_tPredStubImpl(StubType),mpred_prop(F,StubType),must(ensure_universal_stub(F/A)).


% has_storage_stub(Head):- !.
has_storage_stub(Head):-       
      get_pifunctor(Head,PHead,F),
      create_stub_body(PHead,Body),
      user:clause(PHead,Body),
      (((show_call_failure(predicate_property(PHead,number_of_clauses(1)))),(show_call_failure(predicate_property(PHead,number_of_rules(1)))))
        -> true; (listing(PHead),trace)),
      !,
      must((mpred_prop(F,predStub(StubType)),
      must(hybrid_tPredStubImpl(StubType)))).
      

% must_have_storage_stub(Head):-!.
must_have_storage_stub(Head):-
 (has_storage_stub(Head)->true;(get_functor(Head,F),listing(F),term_listing(F),ensure_universal_stub(Head))).

missing_stub(Head):-has_storage_stub(Head),!,fail.
missing_stub(Head):-not(predicate_property(Head,public)),listing(Head).

create_stub_body(Head,Stub):-create_stub_body(call(conjecture),Head,Stub).
create_stub_body(call(conjecture),Head,Stub):- Stub = (call_provided_mpred_storage_op(call(conjecture),Head,Then),Then).

erase_mpred_storage_op(Head):-
   create_stub_body(_,Head,Stub),
   doall(retract((Head:- Stub))),
   foreach(clause(Head,Stub,Ref),erase(Ref)).

really_add_mpred_storage_op(Head):-
   create_stub_body(call(conjecture),Head,Stub),
   asserta_if_new((Head:- Stub)).


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

*/

same_functors(Head1,Head2):-must_det(get_functor(Head1,F1,A1)),must_det(get_functor(Head2,F2,A2)),!,F1=F2,A1=A2.
good_for_hybrid(H,F):- not(mpred_prop(F,_ANY_)),predicate_property(H,number_of_clauses(0)),predicate_property(H,dynamic).
ensure_exists(Head):-get_pifunctor(Head,PHead,F),get_functor(Head,F,A),(predicate_property(PHead,dynamic)->true;(predicate_property(PHead,_)->dmsg(warn(static_pred,F/A));dynamic(F/A))).

reduce_clause(H,HHH):-notrace(once(((demodulize(H,HH),H\=@=HH,!,reduce_clause(HH,HHH))))),!.
reduce_clause((C:- B),C):-B==true,!.
reduce_clause(C,C).

demodulize(Var,OUT):-is_ftVar(Var),!,OUT=Var.
demodulize(Var,OUT):-not(compound(Var)),!,OUT=Var.
demodulize(H,HHH):-once(strip_module(H,_,HH)),H\==HH,!,demodulize(HH,HHH).
demodulize(M:H,HH):-atom(M),!,demodulize(H,HH).
demodulize((H:-B),(HH:-BB)):-!,demodulize(H,HH),demodulize(B,BB).
demodulize((H,B),(HH,BB)):-!,demodulize(H,HH),demodulize(B,BB).
demodulize('&'(H,B),'&'(HH,BB)):-!,demodulize(H,HH),demodulize(B,BB).
demodulize('v'(H,B),'v'(HH,BB)):-!,demodulize(H,HH),demodulize(B,BB).
demodulize((H;B),(HH;BB)):-!,demodulize(H,HH),demodulize(B,BB).
demodulize([H|B],[HH|BB]):-!,demodulize(H,HH),demodulize(B,BB).
demodulize({H},{HH}):-!,demodulize(H,HH).
demodulize(H,HH):-compound(H),H=..HL,!,demodulize(HL,HHL),HH=..HHL,!.
demodulize(HB,HB).

correct_negations(How,(~({X})),O):-nonvar(X),wrap_in_neg_functor(How,X,O).
correct_negations(How,(-({X})),O):-nonvar(X),wrap_in_neg_functor(How,X,O).
correct_negations(How,(not({X})),O):-nonvar(X),wrap_in_neg_functor(How,X,O).
correct_negations(How,(notz({X})),O):-nonvar(X),wrap_in_neg_functor(How,X,O).
correct_negations(How,(assertable_not({X})),O):-nonvar(X),wrap_in_neg_functor(How,X,O).
correct_negations(How,(\+({X})),O):-nonvar(X),wrap_in_neg_functor(How,X,O).

wrap_in_neg_functor(clause,X,assertable_neg(X)).
wrap_in_neg_functor(mpred,X,not(X)).
wrap_in_neg_functor(callable,X, (\+(X))).

pfc_head_expansion(_,V,V ):-var(V),!.
pfc_head_expansion(How,H,GG):-correct_negations(How,H,GG),!.
pfc_head_expansion(_,V,V).
database_expand_term(I,O):-with_assertions(thlocal:into_form_code,expand_term(I,O)).



call_provided_mpred_storage_op(Op,H,(!,database_op(Op,H))).

test_call_cut:- X=!,dmsg(testing_call_cut),call(X).
test_call_cut:- X=!,dmsg(testing_call_cut2),(X).
test_call_cut:- throw(test_failed(testing_call_cut)).
:-doall(test_call_cut).

change(X,B):-database_op(X,B),!.
change(X,B,C):-database_op(change(X,B),C),!.
retract(X,Y):-database_op(change(retract,X),Y),!.

database_op(Op,      H ):- (var(Op);var(H)),!,trace_or_throw(var_database_op(Op,  H )).
database_op(Op,     H ):- once(demodulize(H,HH)),H\=@=HH,!,database_op(Op, HH).
%database_op(change(Cmd,How),H):-!,append_term(Cmd,How,Op),database_op(Op,H).
database_op(Op,H):-  must_op(Op, call_no_cuts_loop_checked(provide_mpred_storage_op(Op,H),database_op0(Op,H))).


must_op(Op,     H ):- (var(Op);var(H)),!,trace_or_throw(var_database_op0(Op,  H )).
must_op(Op,     H ):- once(demodulize(H,HH)),H\=@=HH,!,must_op(Op, HH).
must_op(change(assert,_),Call):-!,must(Call),!.
must_op(change( retract,_),Call):-!,must(Call),!.
must_op(clauses(_),Call):-!,loop_check(debugOnError(Call)).
must_op(call(_),Call):-!,loop_check(debugOnError(Call)).
must_op(_,Call):-!,loop_check(debugOnError(Call)).

database_op0(Op,     H ):- (var(Op);var(H)),!,trace_or_throw(var_database_op0(Op,  H )).
database_op0(Op,     H ):- once(demodulize(H,HH)),H\=@=HH,!,database_call(Op, HH).
database_op0(change(assert,Op),(H)):-!,must(database_modify(change(assert,Op),(H))),!.
database_op0(change(retract,Op),(H)):-!,must(database_modify(change( retract,Op),(H))),!.
database_op0(clauses(Op),(H)):-!,database_check(clauses(Op),H).
database_op0(is_asserted,(H)):-!,database_check(clauses(is_asserted),H).
database_op0(call(Op),H):-!,database_call(Op,H).
database_op0(Op,H):-!,trace_or_throw(unknown_database_op0(Op,H)).

database_modify(Op,H):- (var(Op);var(H)),!,trace_or_throw(var_database_modify_op(Op,  H )).
database_modify(Op,     H ):- once(demodulize(H,HH)),H\=@=HH,!,database_modify(Op, HH).
database_modify(P,H):- thlocal:noDBaseMODs(_),!,dmsg(noDBaseMODs(P,H)).
database_modify(Op,  H):- Op=..[Assert,How],!,database_modify(change(Assert,How),   H).
database_modify(Op,  H):- Op=..[Assert],!,database_modify(change(Assert,one),   H).
%database_modify(change(Cmd,How),H):-!,must(( append_term(Cmd,How,Op),!,database_modify(Op,H))).
database_modify(change(assert,_),   H):- database_check(clauses(is_asserted),H),!.
database_modify(change(Type,_),   H):- expire_pre_change(Type,H),fail.
database_modify(Op,          H):- database_expand_term(H,GG),H\=@=GG,!,database_modify(Op, GG ),!.
database_modify(Op,          H):- pfc_head_expansion(clause,H,GG),H\=@=GG,database_modify(Op, GG ),!.
database_modify(Op,          H):- user:must_op(Op,call_no_cuts(provide_mpred_storage_op(Op,H))),!,ignore(call_no_cuts(expire_post_change(Op,H))).
% database_modify(Op, (H:-TRUE) ):-TRUE==true,!,must((database_modify(Op,H))).
database_modify(Op,          H):-!,trace_or_throw(unknown_database_modify(Op,H)).

/*
database_modify(Op,H):- Op=change(assert,a),!, must(loop_check(database_op(Op,H),loop_check(hooked_asserta(H),dtrace(user:provide_mpred_storage_op(Op,H))))),!.
database_modify(Op,H):- Op=change(assert,_),!, must(loop_check(database_op(Op,H),loop_check(hooked_assertz(H),dtrace(user:provide_mpred_storage_op(Op,H))))),!.
database_modify(Op,H):- Op=change( retract,all),!, must(loop_check(hooked_retractall(H),database_op(Op,H))),!,call_no_cuts(expire_post_change( retract,H)).
database_modify(Op,H):- Op=change( retract,_),!, loop_check(hooked_change( retract,H),database_op(Op,H)),call_no_cuts(expire_post_change( retract,H)).
database_modify(Op,H):-!,trace_or_throw(unknown_database_modify(Op,H)).
*/


call_wdmsg(P,DB):- thlocal:noDBaseMODs(_),!,wdmsg(error(noDBaseMODs(P,DB))).
call_wdmsg(P,DB):- get_functor(DB,F,A), call_wdmsg(P,DB,F,A).

call_wdmsg(P,DB,dbase_t,_A):-!, append_term(P,DB,CALL),dmsg((CALL)),prologCall(CALL).
call_wdmsg(P,MP,F,A):- mpred_prop(F,prologHybrid),must(A>1),into_functor_form(dbase_t,MP,DB),!, append_term(P,DB,CALL),dmsg(info(CALL)),!,prologCall(CALL).
call_wdmsg(P,MP,F,A):- not(mpred_prop(F,prologOnly)),decl_mpred_hybrid(F/A), into_functor_form(dbase_t,MP,DB),!, append_term(P,DB,CALL),dmsg(info(CALL)),!,prologCall(CALL).
call_wdmsg(P,DB,F,_):- append_term(P,DB,CALL),dmsg(info(CALL)),must(mpred_prop(F,prologOnly)),!,prologCall(CALL).
%call_wdmsg(P,DB,S,_):-  dtrace((append_term(P,DB,CALL),dmsg((CALL)),prologCall(CALL))).


% ================================================================================
% INSTALL MISSING STUBS
% ================================================================================

%:-rescan_mpred_props.

% pass 2
scan_missing_stubs(F):-
   ignore((forall(mpred_missing_stubs(F,A),
      (mpred_arity(F,A),show_call(ensure_universal_stub(F/A)))))).

mpred_missing_stubs(F,A):-prologHybrid = StubType, hybrid_tPredStubImpl(StubType),mpred_prop(F,StubType),must(mpred_arity(F,A)),not(has_storage_stub(F/A)).


:-assertz_if_new(call_OnEachLoad(rescan_missing_stubs)).

:-dynamic_multifile_exported(rescan_missing_stubs/0).
% rescan_missing_stubs:-no_rescans,!.
rescan_missing_stubs:-loop_check_local(time_call(rescan_missing_stubs_lc),true).
rescan_missing_stubs_lc:- once(thglobal:use_cyc_database), once(with_assertions(thlocal:useOnlyExternalDBs,forall((kb_t(arity(F,A)),A>1,good_pred_relation_name(F,A),not(mpred_arity(F,A))),with_no_dmsg(decl_mpred_mfa,decl_mpred_hybrid(F,A))))),fail.
rescan_missing_stubs_lc:- hotrace((doall((mpred_missing_stubs(F,A),mpred_arity(F,A),ensure_universal_stub(F/A))))).

no_rescans.

:-dynamic_multifile_exported(rescan_mpred_props/0).

rescan_mpred_props:- loop_check(rescan_mpred_props_lc,true).
rescan_mpred_props_lc:-no_rescans,!.
rescan_mpred_props_lc:-rescan_duplicated_facts(user,mpred_prop(_,_)),fail.
rescan_mpred_props_lc:-time(forall(mpred_prop_ordered(Pred,Prop),hooked_asserta(mpred_prop(Pred,Prop)))),fail.
rescan_mpred_props_lc:-rescan_missing_stubs.
rescan_mpred_props_lc.


% ================================================================================
% GRABOUT STORAGE STUB CLAUSES
% ================================================================================
provide_clauses_list(Head,HBLISTO):- get_pifunctor(Head,PHead,_),  
  findall((PHead :- B),no_repeats_old([PHead:B],call_no_cuts(provide_mpred_storage_clauses(_,PHead,B))),HBLIST),
  delete(HBLIST,(PHead:-(call_provided_mpred_storage_op(call(conjecture), PHead,Then),Then)),HBLISTO),!.

is_asserted(PHeadIn,BIn):-demodulize((PHeadIn:-BIn),(PHead:-B)),no_repeats_old([PHead:B],((dif(NotProlog,prolog),
   call_no_cuts(provide_mpred_storage_clauses(NotProlog,PHead,B))))).
is_asserted(PHeadIn,BIn):-demodulize((PHeadIn:-BIn),(PHead:-B)),no_repeats_old([PHead:B],call_no_cuts(provide_mpred_storage_clauses(prolog,PHead,B))),not(notrace(special_wrapper_body(B,_))).

is_asserted(H):- once(demodulize(H,HH)),H\=@=HH,!,is_asserted(HH).
is_asserted(H):- \+ current_predicate(_,H),compound(H),(H=..[C,I] -> hasInstance(C,I);dbase_t(H)).
is_asserted((H:-B)):-!,is_asserted(H,B).
is_asserted((H1,H2)):-!,is_asserted(H1),is_asserted(H2).
is_asserted((H1;H2)):-!,is_asserted(H1);is_asserted(H2).

is_asserted(clause(H,B)):-!,is_asserted(H,B).
is_asserted(clause(H,B,Ref)):-!,clause(H,B,Ref).
is_asserted(M:H):-nonvar(M),!,is_asserted(H).
is_asserted(H):-!,is_asserted(H,true).

database_check(Op, H ):- (var(Op);var(H)),!,trace_or_throw(var_database_check_op(Op,  H )).
database_check(_,H):-debugOnError(is_asserted(H)).


% nart_to_atomic(F,F):-!,atom(F).
nart_to_atomic(F,F).

% ================================================
% is_asserted/1
% ================================================
                                    
had_module(M:X,X):-atom(M).
