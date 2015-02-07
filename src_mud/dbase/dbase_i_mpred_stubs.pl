/** <module> dbase_i_mpred_stubs.
% Provides a prolog dabase in these predicates...
%
%  Managers the tPredStubImpl/1 in various files
%  
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/

:- include(logicmoo('vworld/moo_header.pl')).

:- dynamic_multifile_exported correctArgsIsa/3.

tPredStubImpl(prologOnly).
tPredStubImpl(prologHybrid).
tPredStubImpl(prologPTTP).
tPredStubImpl(prologSNARK).


get_pifunctor(Head,PHead):-must(get_pifunctor(Head,PHead,_,_)).
get_pifunctor(Head,PHead,F):-must(get_pifunctor(Head,PHead,F,_)).

get_pifunctor(Head,PHead,F,A):-var(Head),!,sanity(atom(F)),must(ensure_arity(F,A)),functor(PHead,F,A),ignore(PHead=Head).
get_pifunctor(Head,PHead,F,A):-get_functor(Head,F,A),functor(PHead,F,A),ignore(PHead=Head),!.
get_pifunctor(Head,PHead,F,A):-atom(Head),ensure_arity(Head,A),!,get_pifunctor(Head/A,PHead,F,A).

% ================================================================================
% INSTALL STORAGE STUBS
% ================================================================================

%user:decl_database_hook(assert(_),mpred_prop(F,StubType)):- tPredStubImpl(StubType),must(add_storage_stub(StubType,F)).
%user:decl_database_hook(assert(_),mpred_stubtype(F,StubType)):- add_storage_stub(StubType,F).
%user:decl_database_hook(assert(_),mudIsa(F,(StubType))):- tPredStubImpl(StubType),must(add_storage_stub(StubType,F)).


decl_mpred_stubcol(F,A,StubType):-loop_check(decl_mpred_stubcol_lc(F,A,StubType),dmsg(looping_decl_mpred_stubcol_lc(F,A,StubType))).
decl_mpred_stubcol_lc(Head,_Isa,StubType):-get_functor(Head,F,A),A>0,!,decl_mpred_stubcol(F,A,StubType).
decl_mpred_stubcol_lc(F,A,StubType):- ignore(((number(A),assert_arity(F,A)))),
     functor(Head,F,A),add_storage_stub(StubType,Head),!.
     %must(tPredStubImpl(StubType)),decl_mpred(F,/*predStubType*/(StubType)),decl_mpred(F,StubType).


has_storage_stub(Var,Head):- var(Var),!,tPredStubImpl(Var),has_storage_stub(Var,Head).
has_storage_stub(prologOnly,Head):- 
     get_functor(Head,F),mpred_prop(F,hasStub(StubType)),must(tPredStubImpl(StubType)),!.
has_storage_stub(StubType,Head):-       
      get_pifunctor(Head,PHead,F),mpred_prop(F,hasStub(StubType)),must(tPredStubImpl(StubType)),
      predicate_property(PHead,number_of_rules(_)),
      predicate_property(PHead,number_of_clauses(1)),!,
      user:clause(PHead,(!,call_provided_mpred_storage_op(call(_),PHead,_))).
      


% must_have_storage_stub(StubType,Head):-!.
must_have_storage_stub(StubType,Head):-
 (has_storage_stub(StubType,Head)->true;(get_functor(Head,F),listing(F),term_listing(F),listing(F),add_storage_stub(StubType,Head))).

:-dynamic_multifile_exported(ensure_universal_stub/1).
ensure_universal_stub(F):- must_det(mpred_arity(F,A)),add_storage_stub(prologHybrid,F/A).
:-dynamic_multifile_exported(ensure_universal_stub/2).
ensure_universal_stub(F,A):- dbase_mod(M), M:ensure_universal_stub(M,F,A).
:-dynamic_multifile_exported(ensure_universal_stub/3).
ensure_universal_stub(M,F,AIn):-AIn==0, must((ensure_arity(F,A),A\==0)),!,ensure_universal_stub(M,F,A).
ensure_universal_stub(M,F,AIn):- AIn==0, trace_or_throw(illegal_argument_ensure_universal_stub(M,F,AIn)).
%ensure_universal_stub(M,F,A):- cannot_override(F,A,Why),!,dmsg(todo(cannot_override(F,A,Why))),nop(listing(M:F/A)).
%ensure_universal_stub(M,F,_):- ignore(M=user),mpred_prop(F,prologOnly),dmsg(warn(mpred_prop(F,prologOnly))),!.
ensure_universal_stub(_,F,A):-functor(Head,F,A),add_storage_stub(prologHybrid,Head).

add_storage_stub(StubType,Head):- has_storage_stub(StubType,Head),!.
add_storage_stub(StubType,Head):-get_pifunctor(Head,PHead,F,A),!,add_storage_stub(StubType,Head,PHead,F,A).
add_storage_stub(StubType,Head,PHead,F,0):- dmsg(zero_add_storage_stub(StubType,Head,PHead,F,0)),!.
add_storage_stub(StubType,Head,PHead,F,_):-
 user: must_det_l((          
     OP = call(conjecture),
     must(tPredStubImpl(StubType)),
         provide_clauses_list(Head,HBLIST),
         retractall(mpred_prop(F,hasStub(_))),
         forall(tPredStubImpl(Impl),retractall(mpred_prop(F,Impl))),
         user:asserta_if_new(mpred_prop(F,hasStub(StubType))),       
         asserta_if_new(mpred_prop(F,StubType)),         
         one_must(show_call_failure(call_no_cuts(provide_mpred_setup(OP,PHead,StubType,Result))),
           (show_call_failure(add_mpred_universal_call_stub(StubType,Head)),Result=defined(universal))),
         asserta_if_new(provide_mpred_currently(OP,F,Head,StubType,Result)),
         asserta_if_new(hasStub(F,StubType)), 
         must_have_storage_stub(StubType,Head),
         must_same_clauses(Head,HBLIST))),
         must(must_have_storage_stub(StubType,Head)).


must_same_clauses(Head,HBLIST):-
   provide_clauses_list(Head,NEWHBLIST),
   sort(NEWHBLIST,NEWHBLISTS),
   sort(HBLIST,HBLISTS),
   length(NEWHBLIST,LN),
   length(HBLIST,LO),
   list_difference_eq(HBLIST,NEWHBLIST,MISSING),length(MISSING,LM),
   list_difference_eq(NEWHBLIST,HBLIST,EXTRA),length(EXTRA,LE),
   (NEWHBLIST=@=HBLIST -> true ;((NEWHBLISTS=@=HBLISTS;LM=0) -> wdmsg(trace_or_throw(must_same_clauses(Head,[missing(LM)|MISSING],[extra(LE)|EXTRA]))) ;  
     wdmsg(error(trace_or_throw(must_same_clauses(Head,[LO|HBLIST],[LN|NEWHBLIST],[missing(LM)|MISSING],[extra(LE)|EXTRA])))))).



add_storage_op(OP,Head,StubType):-
  get_functor(Head,F),
     must(tPredStubImpl(StubType)),
     provide_mpred_setup(OP,Head,StubType,Result),
     must_det_l((asserta_if_new(mpred_prop(F,StubType)),
         asserta_if_new(provide_mpred_currently(OP,Head,StubType,Result)))).


add_stub_now(PHead,StubType,OP):- predicate_property(PHead,number_of_clauses(NC)),NC>0,!,asserta_if_new((PHead :- (!,call_provided_mpred_storage_op(OP,PHead,StubType)))).
add_stub_now(PHead,StubType,OP):- asserta_if_new((PHead:- (!,call_provided_mpred_storage_op(OP,PHead,StubType)))).

:-export(call_provided_mpred_storage_op/3).    
call_provided_mpred_storage_op(OP,Head,StubType):-  
  must(get_provided_mpred_storage_oper(OP,Head,StubType,CALL)),!,
  debugOnError(user:call(CALL)).

:-export(get_provided_mpred_storage_op/4).
get_provided_mpred_storage_oper(OP,OrigHead,StubType,CALLO):-
   into_mpred_form(OrigHead,NewHead),
   must(get_mpred_storage_provider(OP,NewHead,StubType)),
   must(provide_mpred_storage_op(OP,NewHead,StubType,CALL)),!,
   CALLO = will_call(StubType,CALL).

will_call(W,user:call(OP,OPRAND)):-!,will_call(W,call(OP,OPRAND)).
will_call(W,user:CALL):-!,will_call(W,CALL).
will_call(_,clause_asserted(mpred_prop(A, B))):-!,mpred_prop(A,B).
will_call(_,call(assertz_if_new, dbase_t(C, I))):-!,assert_hasInstance(C,I).
will_call(_,call(asserta_new, dbase_t(C, I))):-!,assert_hasInstance(C,I).
will_call(_,call(clause_asserted, dbase_t(mudIsa, I, C))):-!,hasInstance(C,I).
will_call(_,call_tabled(call_for_literal(F, A, G))):-!,call_for_literal(F, A, G).
will_call(_StubType,CALL):-!,(CALL).
will_call(_StubType,CALL):-show_call(CALL).

/*
EVENTUALLY MOVE TO PDDL
  user:decl_database_hook(assert(_),mpred_prop(F,ENV)):- ((assert_if_new(env_precol(ENV)),mpred_arity(Head), doall(provide_mpred_write_attributes(Head,ENV)))).

*/
  
%= create_stub_really(Head,StubType,REQ,DBASE_T):- mpred_stub(REQ, DBASE_T, PHead,HEAD,BODY), impl_stub_really(Head,StubType,HEAD,BODY).

/*

clause_asserted  checks using =@=
call(conjecture)
call(once)  % TODO
assert(a) asserts first if =@= is not first
assert(z) asserts last if no =@=
retract(one)  using =
retract(all)  using =

*/

mpred_stubtype(F,StubType) :- nonvar(StubType),must(tPredStubImpl(StubType)),!,
  (mpred_prop(F,hasStub(StubTypeWas));(mpred_prop(F,/*predStubType*/(StubTypeWas)),tPredStubImpl(StubTypeWas))),!,StubType=StubTypeWas.
mpred_stubtype(F,StubType) :- (mpred_prop(F,hasStub(StubType));(mpred_prop(F,StubType),tPredStubImpl(StubType))),!.

get_mpred_storage_provider(OP,PHead,StubType):-get_mpred_storage_provider(OP,PHead,StubType,Will),Will\==wont(_),!.

get_mpred_storage_provider(_OP,Head,StubType,Declared):-get_functor(Head,F),mpred_stubtype(F,StubTypeWas),!,(StubTypeWas=StubType -> declared(_)=Declared ; wont(already(StubTypeWas))=Declared).
get_mpred_storage_provider( OP,Head,StubType,Will):- get_pifunctor(Head,PHead,F,A),
  CALL = (provide_mpred_currently(OP,F,Head,StubType,Will);
   (Head \= PHead,provide_mpred_currently(OP,F,PHead,StubType,Will));
      provide_mpred_currently(OP,F,F/A,StubType,Will)),
  one_must((Will=declared(_),CALL,!),one_must((CALL,Will\==wont(Why)),(Will=wont(Why)))).


same_functors(Head1,Head2):-must_det(get_functor(Head1,F1,A1)),must_det(get_functor(Head2,F2,A2)),!,F1=F2,A1=A2.

ensure_exists(Head):-get_pifunctor(Head,PHead,F),get_functor(Head,F,A),(predicate_property(PHead,dynamic)->true;(predicate_property(PHead,_)->dmsg(warn(static_pred,F/A));dynamic(F/A))).

database_modify(P,G):- thlocal:noDBaseMODs(_),!,dmsg(noDBaseMODs(P,G)).
database_modify(P,M:G):-nonvar(M),!,database_modify(P,G).
database_modify(assert(_),G):- was_isa(G,I,C),!,assert_hasInstance(C,I),!.
database_modify(OP,(HeadBody:-TRUE)):-TRUE==true,!,database_modify(OP,HeadBody).
database_modify(OP,HeadBody):- current_predicate(get_mpred_storage_provider/3),
  show_call_failure(get_mpred_storage_provider(OP,HeadBody,StubType)),
  must(call_provided_mpred_storage_op(OP,HeadBody,StubType)).

database_modify(OP,G):- functor(G,F,_), database_f_modify(OP,F,G).

% database_f_modify(OP,F,G):- once(mpred_prop(F,prologOnly);must((F=dbase_t,wdmsg(database_modify(OP,G))))),fail.
database_f_modify(retract(all), F, G):- mpred_prop(F,prologHybrid),sanity(not(dbase_t=F)), into_functor_form(dbase_t,G,GG),!,doall((retract(GG),expire_post_retract(GG))).
database_f_modify(assert(_),_,G):- is_asserted_dbase_t(G),!.
database_f_modify(assert(_),_,G):- must(not(debugOnError(G))),!.
database_f_modify(assert(a),_,G):-  expire_post_assert(G),!,asserta(G).
database_f_modify(assert(z),_,G):- expire_post_assert(G),!,assertz(G).
database_f_modify(retract(all),_,G):- !,doall((retract(G),expire_post_retract(G))).
database_f_modify(retract(one),_,G):- !,must((retract(G),expire_post_retract(G))).
/*

database_modify(OP,G):- get_functor(G,F,A), database_f_modify(OP,F,A,G).

database_f_modify(OP,F,A,G):- mpred_stubtype(F,prologHybrid),F=dbase_t,!,database_f_modify0(OP,G).
database_f_modify(OP,F,A,G):- mpred_stubtype(F,prologHybrid),into_functor_form(dbase_t,G,DB),!,database_f_modify0(OP,DB).

database_f_modify(OP,F,A,G):- mpred_stubtype(F,prologOnly),F=dbase_t,!,database_f_modify0(OP,G),dmsg(warn(database_f_modify0(OP,G))).
database_f_modify(OP,F,A,G):- mpred_stubtype(F,prologOnly),!,database_f_modify0(OP,G),database_f_modify0(OP,G).

database_f_modify(OP,F,A,HeadBody):- mpred_stubtype(F,_ANY_),!, current_predicate(get_mpred_storage_provider/3),
  show_call_failure(get_mpred_storage_provider(OP,HeadBody,StubType)),!,
  must(call_provided_mpred_storage_op(OP,HeadBody,StubType)).



database_f_modify(OP,F,A,G):- not(mpred_stubtype(F,_ANY_)),good_for_hybrid(G,F),
   functor(PHead,F,A),
  show_call(provide_mpred_setup(OP,PHead,prologHybrid,OUT)),
  into_functor_form(dbase_t,G,DB),must(G\=@=DB),!,database_f_modify0(OP,DB).

database_f_modify(OP,F,A,HeadBody):- mpred_stubtype(F,_ANY_),!, current_predicate(get_mpred_storage_provider/3),
  show_call_failure(get_mpred_storage_provider(OP,HeadBody,StubType)),!,
  must(call_provided_mpred_storage_op(OP,HeadBody,StubType)).



good_for_hybrid(G,F):- not(mpred_stubtype(F,_ANY_)),predicate_property(G,number_of_clauses(0)),predicate_property(G,dynamic).



database_f_modify(OP,F,A,G):- once(mpred_prop(F,prologOnly);must((F=dbase_t,wdmsg(database_modify(OP,G))))),fail.

database_f_modify0(assert(_),G):- is_asserted_dbase_t(G),!.
database_f_modify0(assert(_),G):- predicate_property(G,_), must(not(debugOnError(G))),!.
database_f_modify0(assert(a),G):- expire_post_assert(G),!,asserta(G).
database_f_modify0(assert(z),G):- expire_post_assert(G),!,assertz(G).
database_f_modify0(retract(all),G):- !,doall((retract(G),expire_post_retract(G))).
database_f_modify0(retract(one),G):- !,must((retract(G),expire_post_retract(G)).
*/

database_check(P,M:G):-nonvar(M),!,database_check(P,G).
%database_check(clause_asserted,G):-!,was_isa(G,I,C),moo:hasInstance_dyn(C,I),!.
%database_check(mpred_asserted,G):-!,was_isa(G,I,C),moo:hasInstance_dyn(C,I),!.
database_check(OP,HeadBody):- current_predicate(get_mpred_storage_provider/3),
  one_must(show_call_failure(get_mpred_storage_provider(OP,HeadBody,StubType)),StubType=prologOnly),
  call_provided_mpred_storage_op(OP,HeadBody,StubType).
database_check(OP,HeadBody):- debugOnError(call(OP,HeadBody)).



call_wdmsg(P,DB):- thlocal:noDBaseMODs(_),!,wdmsg(error(noDBaseMODs(P,DB))).
call_wdmsg(P,DB):- get_functor(DB,F,A), call_wdmsg(P,DB,F,A).

call_wdmsg(P,DB,dbase_t,_A):-!, append_term(P,DB,CALL),dmsg((CALL)),call(CALL).
call_wdmsg(P,MP,F,A):- mpred_stubtype(F,prologHybrid),must(A>1),into_functor_form(dbase_t,MP,DB),!, append_term(P,DB,CALL),dmsg(info(CALL)),!,call(CALL).
call_wdmsg(P,MP,F,A):- not(mpred_prop(F,prologOnly)),decl_mpred_hybrid(F/A), into_functor_form(dbase_t,MP,DB),!, append_term(P,DB,CALL),dmsg(info(CALL)),!,call(CALL).
call_wdmsg(P,DB,F,_):- append_term(P,DB,CALL),dmsg(info(CALL)),must(mpred_prop(F,prologOnly)),!,call(CALL).
%call_wdmsg(P,DB,S,_):-  dtrace((append_term(P,DB,CALL),dmsg((CALL)),call(CALL))).

% ================================================================================
% INSTALL MISSING STUBS
% ================================================================================

%:-rescan_mpred_props.

% pass 2
scan_missing_stubs(F):-
   ignore((forall(mpred_missing_stubs(F,A,StubType),
      (mpred_arity(F,A),show_call(add_storage_stub(StubType,F/A)))))).

mpred_missing_stubs(F,A,StubType):-prologHybrid = StubType, tPredStubImpl(StubType),mpred_prop(F,StubType),must(mpred_arity(F,A)),not(has_storage_stub(StubType,F/A)).


:-assertz_if_new(call_OnEachLoad(rescan_missing_stubs)).

:-dynamic_multifile_exported(rescan_missing_stubs/0).
% rescan_missing_stubs:-no_rescans,!.
rescan_missing_stubs:-loop_check_local(time_call(rescan_missing_stubs_lc),true).
rescan_missing_stubs_lc:- once(thglobal:use_cyc_database), once(with_assertions(thlocal:useOnlyExternalDBs,forall((kb_t(arity(F,A)),A>1,good_pred_relation_name(F,A),not(mpred_arity(F,A))),with_no_dmsg(decl_mpred_mfa,decl_mpred_hybrid(F,A))))),fail.
rescan_missing_stubs_lc:- hotrace((doall((mpred_missing_stubs(F,A,StubType),mpred_arity(F,A),add_storage_stub(StubType,F/A))))).

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
  delete(HBLIST,(PHead:-!, call_provided_mpred_storage_op(call(conjecture), PHead, _)),HBLISTO),!.





% ================================================================================
% ASSERT INTO STORAGE STUBS
% ================================================================================

:-dynamic_multifile_exported((assertz_clause/1)).
assertz_to_system(Before):- expand_term(Before,Replaced),Before \=@= Replaced,!, assertz_to_system(Replaced).
assertz_to_system((':-'(Body))):-!,must_det(show_call(Body)),!.
assertz_to_system((Head :- Body)):- !,assertz_clause(Head,Body),!.
assertz_to_system(C):- assertz_clause(C,true),!.

assertz_clause(Head,Body):- (var(Head);var(Body)),!,trace_or_throw(var_assertz_clause(Head,Body)).
assertz_clause(Head,Body):- clause_asserted((':-'(Head,Body))),!.
assertz_clause(Head,Body):- ExpIn = (Head:-Body),  expand_term(ExpIn,Exp),Exp \=@= ExpIn,!,assertz_to_system(Exp),!.
assertz_clause(Head,Body):- must_det(database_modify(assert(z), (Head:-Body))).

% ================================================================================
% DETECT PREDS THAT NEED STORAGE 
% ================================================================================

requires_storage((Head :- Body)):-!, requires_storage(Head,Body).
requires_storage(C):- requires_storage(C,true).

requires_storage(M:H,B):-atomic(M),!,requires_storage(H,B).
requires_storage(H,_):-get_pifunctor(H,F),!,special_head(H,F),!.

special_wrapper_functor(call_mpred_body,direct_to_prolog).
special_wrapper_functor(body_req,direct_to_prolog).
special_wrapper_functor(provide_mpred_storage_op,direct_to_prolog).
special_wrapper_functor(call_provided_mpred_storage_op,direct_to_prolog).
special_wrapper_functor(loop_check,meta).
special_wrapper_functor(loop_check_term,meta).
%special_wrapper_functor(pttp_req).
%special_wrapper_functor(loop_check_clauses).

make_body_clause(_Head,Body,Body):-atomic(Body),!.
make_body_clause(_Head,Body,Body):-special_wrapper_body(Body),!.
make_body_clause(Head,Body,call_mpred_body(Head,Body)).


special_head(_,user:agent_text_command):-!,fail.
special_head(Head,F):- provide_mpred_currently(_OP,F,Head,StubType,Will),Will=declared(_),!,StubType\==prologOnly.
special_head(_,F):-mpred_stubtype(F,prologOnly),!,fail.
special_head(_,F):-mpred_stubtype(F,prologHybrid),!.
% special_head(_,F):-mpred_stubtype(F,prologPTTP),!.
special_head(_,F):-mpred_stubtype(F,UNK),wdmsg(mpred_stubtype(F,UNK)),!.



special_wrapper_body(W,Why):-get_body_functor(W,F,_),!,special_wrapper_functor(F,Why).

get_body_functor(Var,_,call):-var(Var),!.
get_body_functor((M:BDY),BF,A):-atom(M),!,get_body_functor(BDY,BF,A).
get_body_functor((!,BDY),BF,A):-!,get_body_functor(BDY,BF,A).
get_body_functor(call(BDY),BF,A):-!,get_body_functor(BDY,BF,A).
get_body_functor(once(BDY),BF,A):-!,get_body_functor(BDY,BF,A).
get_body_functor((BDY1;BDY2),BF,A):-!, (get_body_functor(BDY1,BF,A);get_body_functor(BDY2,BF,A)).
get_body_functor((BDY1,BDY2),BF,A):-!, (get_body_functor(BDY1,BF,A);get_body_functor(BDY2,BF,A)).
get_body_functor(BDY,BF,A):-get_functor(BDY,BF,A).



