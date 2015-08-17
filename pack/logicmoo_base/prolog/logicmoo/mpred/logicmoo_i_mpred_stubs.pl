/** <module> logicmoo_i_mpred_stubs.
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


:- include(logicmoo_i_header).

hybrid_tPredStubImpl(prologHybrid).
hybrid_tPredStubImpl(prologPTTP).
hybrid_tPredStubImpl(prologDynamic).
hybrid_tPredStubImpl(prologBuiltin).
hybrid_tPredStubImpl(prologKIF).
hybrid_tPredStubImpl(prologEquality).


make_builtin(F/A):- show_call_failure((atom(F),integer(A))),
  with_assertions(set_prolog_flag(access_level,system),lock_predicate(F/A)),pfc_add(prologBuiltin(F)),pfc_add(arity(F,A)).

/*
1 = single value

t = type transitve becoming more specific
T = type transitve becoming more general
tt = type transitve becoming more specific
TT = type transitve becoming more general

i = instance (non col)
p3 = predciate arity
r3 = function arity
a = agent
v = value
o = object
l = literal
f = formula of formulas
 



genls =  ct_cT
relationAllInstance = p2_ct_v
relationMostInstance = p2_ct_sv
relationMostExists = p2_ct_ct
isa X BinaryPredicate = isa_p2_c
isa BinaryPredicate Role = isa_p2_pc
isa 4 number =  isa_v_ft
implies P Q =  





*/
% ================================================
% db_redir_op_if_needed/4
% ================================================
/* TODO RE-INCORPERATE
db_redir_op_if_needed(Op,C0,Prop,_ARGS):- glean_pred_props_maybe(C0),fail.

% predProxyDatabase/1
db_redir_op_if_needed(Op,C0,Prop,_ARGS):- get_mpred_prop(Prop,predProxyDatabase(Other)),must(nonvar(Other)),!,call(Other,Op,C0).

% predProxyAssert/1
db_redir_op_if_needed(change(assert,A),C0,Prop,_RGS):- get_mpred_prop(Prop,predProxyAssert(How)),must(nonvar(How)),!, once(ignore((call(How,C0), run_database_hooks(change(assert,A),C0)))).

% predProxyRetract/1
db_redir_op_if_needed(change(retract,A),C0,Prop,_RGS):- get_mpred_prop(Prop,predProxyRetract(How)),must(nonvar(How)),!, once(ignore((call(How,C0), run_database_hooks(change( retract,A),C0)))).

% predProxyQuery/1
db_redir_op_if_needed(query(Must,HLDS),C0,Prop,_RGS):- get_mpred_prop(Prop,predProxyQuery(How)),must(nonvar(How)),!, mpred_op(query(Must,HLDS),call(How,C0)).

% plain props
db_redir_op_if_needed(Op,_C0,Prop,ARGS):- database_modify_units(Op,Unit).

*/

% ================================================================================
% INSTALL STORAGE STUBS
% ================================================================================

maybe_storage_stub(F,StubType):- hybrid_tPredStubImpl(StubType),not((StubType==prologDynamic)),arity(F,A),must(ensure_universal_stub(F/A)).


%OLD user:decl_database_hook(change(assert,_),user:mpred_prop(F,StubType)):- maybe_storage_stub(F,StubType).

%OLD user:decl_database_hook(change(assert,_),isa(F,StubType)):- maybe_storage_stub(F,StubType).

%OLD user:decl_database_hook(change(assert,_),arity(F,StubType)):-  hybrid_tPredStubImpl(StubType),user:mpred_prop(F,StubType),must(ensure_universal_stub(F/A)).


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
   foreach(clause(Head,Stub,Ref),erase_safe(clause(Head,Stub,Ref),Ref)).

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
is_tCol(F):- user:mpred_prop(F,tCol);t(tCol,F);t(F,_).

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

%change(X,B):-mpred_op(X,B),!.
%change(X,B,C):-mpred_op(change(X,B),C),!.
%retract(X,Y):-mpred_op(change(retract,X),Y),!.


must_op(Op,     H ):- (var(Op);var(H)),!,trace_or_throw(var_database_op0(Op,  H )).
must_op(Op,     H ):- once(fully_expand(Op,H,HH)),H\=@=HH,!,must_op(Op, HH).
must_op(change(assert,_),Call):-!,must(Call),!.
must_op(change( retract,_),Call):-!,must(Call),!.
must_op(clauses(_),Call):-!,loop_check(debugOnError(Call)).
must_op(call(_),Call):-!,loop_check(debugOnError(Call)).
must_op(_,Call):-!,loop_check(debugOnError(Call)).

call_wdmsg(P,DB):- thlocal:noDBaseMODs(_),!,wdmsg(error(noDBaseMODs(P,DB))).
call_wdmsg(P,DB):- get_functor(DB,F,A), call_wdmsg(P,DB,F,A).

call_wdmsg(P,DB,t,_A):-!, append_term(P,DB,CALL),dmsg((CALL)),mpred_call(CALL).
call_wdmsg(P,MP,F,A):- user:mpred_prop(F,prologHybrid),must(A>1),into_functor_form(t,MP,DB),!, append_term(P,DB,CALL),dmsg(info(CALL)),!,mpred_call(CALL).
call_wdmsg(P,MP,F,A):-  (\+ user:mpred_prop(F,prologDynamic)), (\+ user:mpred_prop(F,prologBuiltin)),decl_mpred_hybrid(F/A), into_functor_form(t,MP,DB),!, append_term(P,DB,CALL),dmsg(info(CALL)),!,mpred_call(CALL).
call_wdmsg(P,DB,F,_):- append_term(P,DB,CALL),dmsg(info(CALL)),must(user:mpred_prop(F,prologDynamic);user:mpred_prop(F,prologBuiltin)),!,mpred_call(CALL).
%call_wdmsg(P,DB,S,_):-  dtrace((append_term(P,DB,CALL),dmsg((CALL)),mpred_call(CALL))).


% ================================================================================
% INSTALL MISSING STUBS
% ================================================================================

%:-agenda_rescan_mpred_props.

% pass 2
scan_missing_stubs(F):-
   ignore((forall(mpred_missing_stubs(F,A),
      (arity(F,A),show_call(ensure_universal_stub(F/A)))))).

mpred_missing_stubs(F,A):-prologHybrid = StubType, hybrid_tPredStubImpl(StubType),arity(F,A),user:mpred_prop(F,StubType),must(arity(F,A)),not(has_storage_stub(F/A)).


:-assertz_if_new(call_OnEachLoad(rescan_missing_stubs)).

:-export(rescan_missing_stubs/0).
% rescan_missing_stubs:-no_rescans,!.
rescan_missing_stubs:-loop_check(time_call(rescan_missing_stubs_ilc),true).
rescan_missing_stubs_ilc:- once(thglobal:use_cyc_database), once(with_assertions(thlocal:useOnlyExternalDBs,forall((kb_t(arity(F,A)),A>1,good_pred_relation_name(F,A),not(arity(F,A))),with_no_dmsg(decl_mpred_mfa,decl_mpred_hybrid(F,A))))),fail.
rescan_missing_stubs_ilc:- hotrace((doall((mpred_missing_stubs(F,A),arity(F,A),ensure_universal_stub(F/A))))).

no_rescans.

:-export(agenda_rescan_mpred_props/0).

agenda_rescan_mpred_props:- loop_check(rescan_mpred_props_ilc,true).
rescan_mpred_props_ilc:-no_rescans,!.
rescan_mpred_props_ilc:-rescan_duplicated_facts(user,user:mpred_prop(_,_)),fail.
rescan_mpred_props_ilc:-time(forall(mpred_prop_ordered(Pred,Prop),hooked_asserta(user:mpred_prop(Pred,Prop)))),fail.
rescan_mpred_props_ilc:-rescan_missing_stubs.
rescan_mpred_props_ilc.

first_mpred_props(meta_argtypes(_)).

mpred_prop_ordered(Pred,Prop):-first_mpred_props(Prop),user:mpred_prop(Pred,Prop),\+ (user:mpred_prop(Pred,prologDynamic)).
mpred_prop_ordered(Pred,Prop):-user:mpred_prop(Pred,Prop),not(first_mpred_props(Prop)),not(user:mpred_prop(Pred,prologDynamic)).


% ================================================================================
% GRABOUT STORAGE STUB CLAUSES
% ================================================================================
provide_clauses_list(Head,HBLISTO):- get_pifunctor(Head,PHead,_),  
  findall((PHead :- B),
   no_repeats_old([PHead:B],((call_no_cuts(user:provide_mpred_storage_clauses(_,PHead,B,Proof)),is_source_proof(Proof)))),
   HBLIST),
   create_stub_body(PHead,Stub),
   delete(HBLIST,Stub,HBLISTO),!.


get_cc(PI,NC):-provide_clauses_list(PI,HBLISTO),length(HBLISTO,NC).


% ==============================
% SETUP HYBRID HOOK
% ==============================
user:provide_mpred_setup(Op,HeadIn,StubType,OUT):- StubType \== prologDynamic,
 sanity(var(OUT)),
 must(ensure_universal_stub(HeadIn)),
 must((StubType = prologHybrid)),
 OUT=defined(user:provide_mpred_setup(Op,HeadIn,StubType)).

% ==============================
% SETUP HYBRID STUB 1-5
% ==============================
ensure_universal_stub(HeadIn):- has_storage_stub(HeadIn),!.
ensure_universal_stub(HeadIn):-
  must_det_l(( get_pifunctor(HeadIn,Head,F,A),  
   sanity(not(A==0)),
  ensure_universal_stub4(HeadIn,Head,F,A))).


ensure_universal_stub4(HeadIn,Head,F,A):- thglobal:pfcManageHybrids,!,
   ensure_universal_stub5(HeadIn,Head,F,A,[]).

ensure_universal_stub4(HeadIn,Head,F,A):-
   provide_clauses_list(Head,HBLIST),
   must(ensure_universal_stub5(HeadIn,Head,F,A,HBLIST)).


ensure_universal_stub5(HeadIn,Head,F,A,_HBLIST):- thglobal:pfcManageHybrids,!,
    must((StubType = prologHybrid)),
   tf_result(predicate_property(Head,dynamic),WasDynamic),
   tf_result(predicate_property(Head,multifile),WasMulifile),      
   asserta_if_new(user:mpred_prop(F,StubType)),
   add(isa(F,StubType)),
   dynamic_safe(F/A),
   % abolish(F,A),
   % really_add_mpred_storage_op(Head),
   (predicate_property(Head,dynamic)->true;show_pred_info(Head)),
   must((predicate_property(Head,dynamic))),
   (WasDynamic-> (dynamic_safe(F/A),(if_result(WasMulifile,multifile(F/A)))) ; dynamic_safe(F/A)),   
   % public(F/A),
   % lock_predicate(Head),
   discontiguous(Head),
   retractall(user:mpred_prop(F,prologDynamic)),   
   add(isa(Head,pfcControlled)),
   dmsg(pfcManageHybrids(HeadIn)),!.


ensure_universal_stub5(HeadIn,Head,F,A,[]):-!,
 must((StubType = prologHybrid)),
   tf_result(predicate_property(Head,dynamic),WasDynamic),
   tf_result(predicate_property(Head,multifile),WasMulifile),      
   asserta_if_new(user:mpred_prop(F,StubType)),
   dynamic_safe(F/A),
   call((asserta_if_new(user:mpred_prop(F,predStub(StubType))))),
   abolish(F,A),really_add_mpred_storage_op(Head),
   (WasDynamic-> (dynamic_safe(F/A),(if_result(WasMulifile,multifile(F/A)))) ; compile_predicates([F/A])),   
   % public(F/A),
   lock_predicate(Head),
   retractall(user:mpred_prop(F,prologDynamic)),
   dmsg(compiled_new_predicate(HeadIn)),!.

ensure_universal_stub5(HeadIn,Head,F,A,HBLIST):- user:mpred_prop(F,prologDynamic), must((StubType = prologHybrid)), !,
   forall(member(HB,HBLIST),must(show_call(assert_mpred_t(HB)))),!,
   expire_dont_add,ex,
  (is_same_clauses(Head,HBLIST)
    ->
   (unlock_predicate(Head),
      dmsg(converted_prolog_predicate(HeadIn)),
            tf_result(predicate_property(Head,dynamic),WasDynamic),
            tf_result(predicate_property(Head,multifile),WasMulifile),      
            asserta_if_new(user:mpred_prop(F,StubType)),
            call((asserta_if_new(user:mpred_prop(F,predStub(StubType))))),
            abolish(F,A),really_add_mpred_storage_op(Head),
            (WasDynamic-> (dynamic_safe(F/A),(if_result(WasMulifile,multifile(F/A)))) ; compile_predicates([F/A])),
            public(F/A),
            lock_predicate(Head),
            retractall(user:mpred_prop(F,prologDynamic)),
           % Stop turning GC on/off
           % set_prolog_flag(gc,true), garbage_collect_atoms,
            dmsg(compiled_new_predicate(HeadIn)));
       ((wdmsg(error(cannot_absorb_all_clauses((Head))))),!,fail)).


ensure_universal_stub5(HeadIn,Head,F,A,HBLIST):-  must((StubType = prologHybrid)),
   forall(member(HB,HBLIST),must(show_call(assert_mpred_t(HB)))),!,
   expire_dont_add,ex,
  (is_same_clauses(Head,HBLIST)
    ->
   (unlock_predicate(Head),
    dmsg(merging_prolog_predicate(HeadIn)),
          tf_result(predicate_property(Head,dynamic),WasDynamic),
          tf_result(predicate_property(Head,multifile),WasMulifile),      
          asserta_if_new(user:mpred_prop(F,StubType)),
          call((asserta_if_new(user:mpred_prop(F,predStub(StubType))))),
          abolish(F,A),really_add_mpred_storage_op(Head),
          (WasDynamic-> dynamic_safe(F/A) ; compile_predicates([F/A])),
          if_result(WasMulifile,multifile(F/A)), 
          compile_predicates([F/A]),
          public(F/A),
          lock_predicate(Head),
          retractall(user:mpred_prop(F,prologDynamic)),
          dmsg(merging_prolog_predicate(HeadIn)));
     ((wdmsg(error(cannot_absorb_all_clauses((Head))))),!,fail)).



assert_mpred_t((G:-B)):-is_true(B),!,must(assert_mpred_t(G)).
assert_mpred_t(DB):-once(fully_expand(change(assert,add),DB,MP)),DB\=@=MP,!,must(assert_mpred_t(MP)).
assert_mpred_t((G1,G2)):-!,assert_mpred_t(G1),assert_mpred_t(G2).
assert_mpred_t(G):-add_from_file(G).


:- op(1150,fx,decl_mpred_hybrid).

user:listing_mpred_hook(Match):- fail,
 (( 
  dif:dif(Proof,prologRef(_)),
  no_repeats_old([H,B],((user:provide_mpred_storage_clauses(H,B,Proof)),
                Proof\=prologRef(_))),term_matches_hb(Match,H,B),portray_hb(Proof:H,B))),fail.

      
user:provide_mpred_storage_clauses(H,B,Proof):-mpred_t_mpred_storage_clauses_facts(H,B,Proof).

mpred_t_mpred_storage_clauses_facts(H,true,t(H)):-is_list(H),!,length(H,A),A>2,loop_check(t(H)).
mpred_t_mpred_storage_clauses_facts(H,true,t(H)):-compound(H),!,current_predicate(into_plist_arities/4),functor(H,_,A),A>1,loop_check(t(H)).
% mpred_t_mpred_storage_clauses_facts(H,B,W):-mpred_t_mpred_storage_clauses_rules(H,B,W),H\=isa(_,_).

% TODO USE PFC FOR FOREWARD RULES
% TODO USE PTTP FOR BACKARDS RULES
% mpred_t_mpred_storage_clauses_rules(H,B,ruleForward(B,H)):-ruleForward(B,H).
% mpred_t_mpred_storage_clauses_rules(H,B,ruleBackward(H,B)):-ruleBackward(H,B).
% mpred_t_mpred_storage_clauses_rules(H,B,'<=>'):-'<=>'(HH,B),each_subterm(HH,SubTerm),compound(SubTerm),SubTerm = H.
% mpred_t_mpred_storage_clauses_rules(H,B,'<=>'):-'<=>'(B,HH),each_subterm(HH,SubTerm),compound(SubTerm),SubTerm = H.


mpred_t_provide_mpred_storage_op(Op,HB):-hotrace(demodulize(Op,HB,HeadBody)),get_functor(HeadBody,F),(F==t;user:mpred_prop(F,prologHybrid)), must(is_mpred_op(Op)), 
    with_assertions(thlocal:already_in_file_term_expansion,mpred_t_storage_op(Op,HeadBody)).

% ====================================================
% mpred_t_storage_op/2
% ====================================================
% HOOK Simplification
mpred_t_storage_op(Op,(Head:-Body)):- is_true(Body),!,mpred_t_storage_op(Op,Head).


mpred_t_storage_op(Op,H):- thglobal:pfcManageHybrids,!,pfc_provide_mpred_storage_op(Op,H).

mpred_t_storage_op(Op,(:-(Body))):-!,loop_check(mpred_op(Op,(:-(Body))),true),!.

% HOOK for ISA alt-forms
mpred_t_storage_op(_,isa(_,_)):- !,fail. % <- keeps u out of isa hybrids hairs
mpred_t_storage_op(Op,X):- was_isa(X,I,C),!,mpred_op(Op,isa(I,C)).

% HOOK MOST ALL CALLS
mpred_t_storage_op(Op,HeadBodyI):- hotrace(((expand_term(HeadBodyI,HeadBodyM)),HeadBodyI\=@=HeadBodyM)),!,mpred_t_storage_op(Op,HeadBodyM).
mpred_t_storage_op(Op,X):- not(is_non_call_op(Op)),!,mpred_t_call_op(Op,X).

% RULE HOOK (for prolog special wrapper body stubs)
mpred_t_storage_op(Op,(Head:-Body)):-
 reduce_mpred_op(Op,Op2),
  special_wrapper_body(Body,direct_to_prolog),!,
  wdmsg(direct_to_prolog_special_wrapper_body(Op2,Head,Body)),
   (mud_call_store_op(Op2,(Head:-Body))).  

% OLD RULE HOOK (but we are using it in parallel)
mpred_t_storage_op(Op,(Head:-Body)):- \+ use_kif(Head,Body),
  wdmsg(saved_clause_in_hybridRule(Op,Head,Body)),!,
      (mud_call_store_op(Op,ruleBackward(Head,Body))).  

% PTTP RULE HOOK   
mpred_t_storage_op(Op,(Head:-Body)):- 
   mpred_call(use_kif(Head,Body)),!, 
   reduce_mpred_op(Op,Op2), 
   CALL0 = (call(Op2,ruleBackward(Head,Body))), % remember outside of KIF just in case
   must(((CALL0,mpred_t_tell_kif(Op2,(Head:-Body))))),!.

% KIF RULE HOOK   
mpred_t_storage_op(Op,RULE):- mpred_call(is_kif_rule(RULE)),!,
  reduce_mpred_op(Op,Op2),
  mpred_t_tell_kif(Op2,RULE),!.

% REOP HOOK mpred_t_storage_op(Op1,HeadBody):- reduce_mpred_op(Op1,Op2), Op1\==Op2, mpred_t_storage_op(Op2,HeadBody).
% FACT:-true HOOK   

% FACT DB HOOK
mpred_t_storage_op(Op,HeadBody):-
    into_functor_form(t,HeadBody,DB),
     % wff_check_mpred_t_throw(DB),
     must((mud_call_store_op(Op,DB),sanity(show_call(DB)))),!.

mud_call_store_op(Op,(H:-B)):- is_true(B),!,mud_call_store_op(Op,H).
mud_call_store_op(Op,t('$was_imported_kb_content$', _, OPRAND)):-!,loop_check(mpred_op(Op,OPRAND),true).
mud_call_store_op(Op,OPRAND):- show_call_success(wff_check_failed(Op,OPRAND,_WHY)),!.
mud_call_store_op(Op,OPRAND):- reduce_mpred_op(Op,Op2),show_call(call(Op2,OPRAND)).

wff_check_failed(_,DB,WHY):- DB =  t('$was_imported_kb_content$', WHY, _Assert).
wff_check_mpred_t_throw(DB):- wff_check_failed(_,DB,WHY),trace_or_throw(crazy_mpred_t_was_imported_kb_content(WHY,DB)).
wff_check_mpred_t_throw(_).

% ====================================================
% mpred_t_call_op/2
% ====================================================
% ISA CALL
mpred_t_call_op(_,isa(_,_)):- !,fail.
mpred_t_call_op(Op,X):- was_isa(X,I,C),!,mpred_op(Op,isa(I,C)).

% FACT CALL HOOK
mpred_t_call_op(_,FACT):- get_functor(FACT, F,A), !,
     call_tabled(call_for_literal(F,A,FACT)),!.


:- dynamic(use_ideep_swi/0).

% ====================================================
% call_for_literal/3
% ====================================================
call_for_literal(_,_,HEAD):- use_kif(HEAD,true),!,kif_ask(HEAD).
call_for_literal(_,_,HEAD):- use_ideep_swi,!,  call_for_literal_ideep_ilc(HEAD),!,loop_check_term(cwdl(CALL,7),HEAD,(CALL)).
call_for_literal(F,A,HEAD):- call_for_literal_db(F,A,HEAD).

call_for_literal_db(F,A,HEAD):- P=F, HEAD=..[P|ARGS],
   ((thglobal:after_mpred_load,missing_stub(HEAD))->decl_mpred_hybrid(F,A);true),
   constrain_args(P,ARGS),call_for_literal_db0(F,A,HEAD),constrain_args(P,ARGS).


cwdl(CALL,DEEP7):- call_with_depth_limit(CALL,DEEP7,Result),
   ( Result == depth_limit_exceeded -> (!,fail) ; true).

call_for_literal_ideep_ilc(HEAD):- get_functor(HEAD,F,A),call_for_literal_db(F,A,HEAD).

call_for_literal_db0(F,A,HEAD):-no_repeats(HEAD,call_for_literal_db00(F,A,HEAD)).

:- style_check(-singleton).
call_for_literal_db00(_,_,HEAD):- is_asserted_mpred_t(HEAD).
call_for_literal_db00(F,_,   _):- (isa(F,completelyAssertedCollection);t(completeExtentAsserted,F)),!,fail.
call_for_literal_db00(F,A,HEAD):- loop_check(call_rule_db(F,A,HEAD)).
call_for_literal_db00(F,A,HEAD):- not(use_kif(HEAD,true)),HEAD=..[P1,A1,A2],dif(P2,P1),loop_check_term(is_asserted_mpred_t(genlPreds(P2,P1)),gp(P1),fail),
   call(t,P2,A1,A2).

is_asserted_mpred_t(HEAD):-t(HEAD)*->true;((show_call_success(out_of_mpred_t(HEAD)))).
out_of_mpred_t(HEAD):-clause_safe(HEAD,true)*->true;show_call_success(user:fact_always_true(HEAD)).


call_rule_db(F,A,HEAD):- isa(F,completelyAssertedCollection),!,fail.
call_rule_db(F,A,HEAD):- use_kif(HEAD,_),!,kif_ask(HEAD).
call_rule_db(F,A,HEAD):- ruleBackward(HEAD,BODY),call_mpred_body(HEAD,BODY).

:- style_check(+singleton).
:- style_check(-singleton).

call_mpred_body(_,true):-!.
call_mpred_body(HEAD,and(A,B)):- !,call_mpred_body(HEAD,A),!,call_mpred_body(HEAD,B).
call_mpred_body(HEAD,(A,B)):- !,call_mpred_body(HEAD,A),call_mpred_body(HEAD,B).
call_mpred_body(HEAD,BODY):- no_repeats(loop_check_term(call_mpred_body_ilc(HEAD,BODY),body_of(HEAD),(nop(failure(call_mpred_body_ilc(HEAD,BODY))),!,fail))).

call_mpred_body_ilc(_HEAD,BODY):- debugOnError(BODY).


mustIsa(I,C):-nonvar(I),!,isa(I,C),!.
mustIsa(I,C):-when(nonvar(I),isa(I,C)).

constrain_args(_):-!.
constrain_args(HEAD):-HEAD=..[P|ARGS],constrain_args(P,ARGS).

constrain_args(_,_):-!.
constrain_args(_P,[AR,GS]):-!,dif(AR,GS).
constrain_args(_,[_P,AR,GS]):-!,dif(AR,GS).
constrain_args(A,B):-constrain_args_pttp(A,B).


% call_body_req(HEAD):- functor(HEAD,F,A),HEAD_T=..[F|ARGS],HEAD_T=..[t,F|ARGS],hook_body_req(HEAD,HEAD_T).



% ========================================================================================
% BODY StubType
% ========================================================================================

body_req_isa(I,C):-isa_backchaing(I,C).

body_call_cyckb(HEAD_T):-el_holds_DISABLED_KB, HEAD_T =.. [t|PLIST], thglobal:use_cyc_database,!, no_repeats(kbp_t(PLIST)).

% =====================================
% = body_req
% =====================================
body_req(HEAD,HEAD_T):- (hook_body_req(HEAD,HEAD_T)).

:-export(body_req_normal/4).
%hook_body_req(HEAD,HEAD_T):- user:mpred_prop(F,prologPTTP),!,dmsg(warn(hook_body_req(HEAD,HEAD_T))),fail.
%hook_body_req(HEAD,HEAD_T):- user:mpred_prop(F,prologDynamic),!,dmsg(warn(hook_body_req(HEAD,HEAD_T))),fail.
hook_body_req(_,_,isa(I,C),_):- !, body_req_isa(I,C).
hook_body_req(_,_,_,t(C,I)):- !, body_req_isa(I,C).
hook_body_req(_,_,_,t(C,I)):- !, body_req_isa(I,C).
hook_body_req(_,_,_ ,HEAD_T):- thlocal:useOnlyExternalDBs,!, body_call_cyckb(HEAD_T).
% loop checking is not usefull (why the cut was added)
hook_body_req(HEAD,HEAD_T):-  no_repeats(body_req_normal(HEAD,HEAD_T)).


:-export(body_req_normal/4).
body_req_normal(HEAD,HEAD_T):- not(ground(HEAD)),!,no_repeats(HEAD_T,body_req_1(HEAD,HEAD_T)).
body_req_normal(HEAD,HEAD_T):- body_req_1(HEAD,HEAD_T),!. 

:-export(body_req_1/4).
body_req_1(HEAD,HEAD_T):- get_functor(HEAD,F), user:mpred_prop(F,call_tabled),!, call_tabled(body_req_2(HEAD,HEAD_T)).
body_req_1(HEAD,HEAD_T):- body_req_2(HEAD,HEAD_T).

body_req_2(HEAD,  _):-   get_functor(HEAD,F), user:mpred_prop(F,external(Module)),!,call(Module:HEAD).
body_req_2(HEAD,HEAD_T):- body_req_with_rules(HEAD,HEAD_T).

body_req_with_rules(HEAD,HEAD_T):-body_req_no_rules(HEAD,HEAD_T).
body_req_with_rules(HEAD,HEAD_T):-body_req_only_rules(HEAD,HEAD_T).

body_req_no_rules(HEAD, _):-     clause(HEAD,  true).
body_req_no_rules(_,_,_  , HEAD_T):- clause(HEAD_T,true).
body_req_no_rules(F,_,_,HEAD_T):- body_req_plus_cyc(F,_,_,HEAD_T).

body_req_only_rules(HEAD, _):-  ruleBackward(HEAD,BODY),call_mpred_body(HEAD,BODY).
body_req_only_rules(_,_,_,t(F,Obj,LValue)):-  choose_val(F,Obj,LValue).

body_req_plus_cyc(F,_,_,HEAD_T):-  user:mpred_prop(F,cycPlus2(_)),thlocal:useOnlyExternalDBs,!,with_assertions(thglobal:use_cyc_database,body_call_cyckb(HEAD_T)).
 
foo_b(b1).
foo_b(b2):-!.
foo_b(b3):-!.

:-must_det((findall(R,call_no_cuts(foo_b(R)),List),length(List,3))).



%OLD user:decl_database_hook(AR,C):-smart_decl_database(AR,C).

smart_decl_database(AR,svo(S,V,O)):- !,dbase2pred2svo(DBASE,PRED,svo(S,V,O)),!,smart_db_op(AR,DBASE,PRED,svo(S,V,O)).
smart_decl_database(AR,DBASE):- functor_catch(DBASE,t,_),!,dbase2pred2svo(DBASE,PRED,SVO),!,smart_db_op(AR,DBASE,PRED,SVO).
smart_decl_database(AR,PRED):- dbase2pred2svo(DBASE,PRED,SVO),!,smart_db_op(AR,DBASE,PRED,SVO).

smart_db_op(change( retract,AR),A,B,C):- retract_ar_fact(AR,A), retract_ar_fact(AR,B),  retract_ar_fact(AR,C).

retract_ar_fact(all,What):- predicate_property(What,dynamic), !, doall((retract_ar_fact(one,What),fail)).
retract_ar_fact(all,What):- not(predicate_property(What,_)),!.
retract_ar_fact(all,What):- copy_term(What,WO),ignore(once(WO)),must_det(What=@=WO).

retract_ar_fact(one,What):- predicate_property(What,dynamic),!, clause(What,true),retract(What:-true).
retract_ar_fact(one,What):- predicate_property(What,_),!, clause_safe(What,true),!.
retract_ar_fact(one,What):- dmsg(mssing(retract_ar_fact(one,What))).


make_functorskel(_,_):-!. % currently ununused
make_functorskel(F,_):- fskel(F,_,_,_,_,_,_),!.
make_functorskel(F,N):- arity(F,N),make_functorskel(F,N,SKEL),asserta(SKEL),!.
make_functorskel(F,N):- ignore(arity(F,A)),dmsg(todo(trace_or_throw(illegal_make_functorskel(F,N,A)))).

dbase2pred2svo(DBASE,PRED,svo(A,F,RGS)):-fskel(F,DBASE,PRED,A,RGS,_,_),!.
dbase2pred2svo(DBASE,PRED,svo(A,F,RGS)):-compound(PRED),functor(PRED,F,N),make_functorskel(F,N),!,fskel(F,DBASE,PRED,A,RGS,_,_),!.
dbase2pred2svo(DBASE,PRED,svo(A,F,RGS)):-compound(DBASE),!,arg(1,DBASE,F),must_det(arity(F,N)),make_functorskel(F,N),!,fskel(F,DBASE,PRED,A,RGS,_,_),!.
dbase2pred2svo(DBASE,PRED,svo(A,F,RGS)):-nonvar(F),must(arity(F,N)),make_functorskel(F,N),!,fskel(F,DBASE,PRED,A,RGS,_,_),!.


:-export(registerCycPredPlus2/1).


registerCycPredPlus2_3(_CM,M,PI,F/A2):-
  registerCycPredPlus2_3(M,PI,F/A2).

registerCycPredPlus2_3(M,_PI,F/A2):- 
  ignore((A2==3,assertz_if_new(is_never_type(F)))),
  A is A2 - 2, decl_mpred_mfa(M,F,A),
  decl_mpred(F,cycPlus2(A2)),decl_mpred(F,cycPred(A)).


registerCycPredPlus2(P):-!,user:with_pi(P,registerCycPredPlus2_3).


ensure_universal_stub_plus_minus_2_HIDE(F,AMinus2):-
   decl_mpred_hybrid(F/AMinus2).

ensure_universal_stub_plus_minus_2(F,AMinus2):- decl_mpred(F,arity(AMinus2)), decl_mpred_mfa(user,F,AMinus2).
   
ensure_universal_stub_plus_2(F,A2):- once(( AMinus2 is A2 -2, ensure_universal_stub_plus_minus_2(F,AMinus2))),fail.

%ensure_universal_stub_plus_2(F,A2):- cannot_override(F,A2,Why),!,dmsg(cannot_override_plus_2(F,A2,Why)).

ensure_universal_stub_plus_2(F,A2):- 
   export(F/A2),
   functor(HEAD,F,A2),
   HEAD=..[F|ARGS],
   append(ARGSMinus2,[_,_],ARGS),
   HEADMinus2=..[F|ARGSMinus2],
   AMinus2 is A2 -2,
   assert_if_new((HEAD:-HEADMinus2)),!,
  % compile_predicates([HEAD]),
   user:mpred_mod(M),
   decl_mpred_hybrid(M,F,AMinus2).

