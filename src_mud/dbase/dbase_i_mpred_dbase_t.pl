/** <module> dbase_i_mpred_dbase_t
% Provides a prolog dabase in these predicates...
%
%  dbase_t/N
%  hybridRule/2
%  
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/

:- include(dbase_i_header).


:- meta_predicate(tf_result(0,+)).
tf_result(Call,TF):-(Call->TF=true;TF=fail).
:- meta_predicate(if_result(0,0)).
if_result(TF,Call):-(TF->Call;true).

% ========================================
% is_holds_true/is_holds_false
% ========================================

:- dbase_mod(M),export((
          % M:dbase_t/1,
          M:dbase_t/2,
          M:dbase_t/3,
          M:dbase_t/4,
          M:dbase_t/5,
          M:dbase_t/6,
          M:dbase_t/7,
          M:dbase_t/8,
          M:dbase_t/9,
          M:dbase_t/10,
          M:dbase_t/11)).

:- dbase_mod(M),export((
          % M:holds_t/1,
          M:holds_t/2,
          M:holds_t/3,
          M:holds_t/4,
          M:holds_t/5,
          M:holds_t/6,
          M:holds_t/7,
          M:holds_t/8,
          M:holds_t/9,
          M:holds_t/10,
          M:holds_t/11)).


:-export((dbase_t/1,hasInstance/2)).
:- dynamic((
         % dbase_t/1,
          dbase_t/2,
          dbase_t/3,
          dbase_t/4,
          dbase_t/5,
          dbase_t/6,
          dbase_t/7,
          dbase_t/8,
          dbase_t/9,
          dbase_t/10,
          dbase_t/11,
        %  asserted_dbase_t/1,
          asserted_dbase_t/2,
          asserted_dbase_t/3,
          asserted_dbase_t/4,
          asserted_dbase_t/5,
          asserted_dbase_t/6,
          asserted_dbase_t/7,
          assertion_f/1,
          assertion_t/1,
        %  asserted_dbase_f/1,
          asserted_dbase_f/2,
          asserted_dbase_f/3,
          asserted_dbase_f/4,
          asserted_dbase_f/5,
          asserted_dbase_f/6,
          asserted_dbase_f/7,
         % dbase_f/1,
          dbase_f/2,
          dbase_f/3,
          dbase_f/4,
          dbase_f/5,
          dbase_f/6,
          dbase_f/7)).


:-export(into_plist/2).
into_plist(In,Out):-into_plist_arities(2,12,In,Out).

:-export(into_plist_arities/4).
into_plist_arities(Min,Max,PLIST,PLISTO):- var(PLIST),!,between(Min,Max,X),length(PLIST,X),PLISTO=PLIST.
into_plist_arities(_,_,[P|LIST],[P|LIST]):-var(P),!.
into_plist_arities(_,_,[dbase_t|PLIST],PLIST):-!.  % dbase_t is our versuion of '$holds' or call/N
into_plist_arities(_,_,plist(P,LIST),[P|LIST]):-!.
into_plist_arities(_,_,Call,PLIST):-Call=..PLIST. % finally the fallthrue


never_dbase_mpred(user:mpred_prop).
never_dbase_mpred(isa).
never_dbase_mpred(arity).


% ================================================================================
% begin holds_t
% ================================================================================

:-dynamic dbase_t/2.
% dbase_t(C,I):- trace_or_throw(dbase_t(C,I)),hasInstance(C,I). % ,fail,loop_check_term(isa_backchaing(I,C),hasInstance(C,I),fail).

%dbase_t([P|LIST]):- !,dbase_plist_t(P,LIST).
%dbase_t(naf(CALL)):-!,not(dbase_t(CALL)).
%dbase_t(not(CALL)):-!,dbase_f(CALL).
dbase_t(CALL):- into_plist_arities(3,10,CALL,[P|LIST]),dbase_plist_t(P,LIST).
dbase_plist_t(P,[]):-!,dbase_t(P).
dbase_plist_t(P,LIST):-var(P),!,is_list(LIST),CALL=..[dbase_t,P|LIST],debugOnError((CALL)).
dbase_plist_t(dbase_t,[P|LIST]):-!, dbase_plist_t(P,LIST).
dbase_plist_t(user:mpred_prop,[C,I]):-!,ground(I:C),user:mpred_prop(C,I).
dbase_plist_t(isa,[I,C]):-!,hasInstance(C,I).
dbase_plist_t(P,_):-never_dbase_mpred(P),!,fail.
dbase_plist_t(P,[L|IST]):-is_holds_true(P),!,dbase_plist_t(L,IST).
dbase_plist_t(P,LIST):-is_holds_false(P),!,dbase_f(LIST).
dbase_plist_t(P,LIST):- CALL=..[dbase_t,P|LIST],debugOnError(CALL).

% loop_check_mpred(Call):- current_predicate(ireq/1), loop_check_term(ireq(Call),loop_check_mpred(Call),fail).
loop_check_mpred(Call):- !, fail,not(thlocal:infInstanceOnly(_)),loop_check_local(ireq(Call),loop_check_mpred(Call),fail).
% loop_check_mpred(Call):-loop_check_local(mpred_call(dbase_t,Call),fail).

dbase_t(P,A1,A2):- loop_check_mpred(dbase_t(P,A1,A2)).
dbase_t(P,A1,A2):- mpred_pa_call(P,2,call(P,A1,A2)).
dbase_t(P,A1,A2,A3):- loop_check_mpred(dbase_t(P,A1,A2,A3)).
dbase_t(P,A1,A2,A3):- mpred_pa_call(P,3,call(P,A1,A2,A3)).
dbase_t(P,A1,A2,A3,A4):- loop_check_mpred(dbase_t(P,A1,A2,A3,A4)).
dbase_t(P,A1,A2,A3,A4):- mpred_pa_call(P,4,call(P,A1,A2,A3,A4)).
dbase_t(P,A1,A2,A3,A4,A5):- loop_check_mpred(dbase_t(P,A1,A2,A3,A4,A5)).
dbase_t(P,A1,A2,A3,A4,A5):- mpred_pa_call(P,5,call(P,A1,A2,A3,A4,A5)).
dbase_t(P,A1,A2,A3,A4,A5,A6):- loop_check_mpred(dbase_t(P,A1,A2,A3,A4,A5,A6)).
dbase_t(P,A1,A2,A3,A4,A5,A6):- mpred_pa_call(P,6,call(P,A1,A2,A3,A4,A5,A6)).
dbase_t(P,A1,A2,A3,A4,A5,A6,A7):- loop_check_mpred(dbase_t(P,A1,A2,A3,A4,A5,A6,A7)).
dbase_t(P,A1,A2,A3,A4,A5,A6,A7):- mpred_pa_call(P,7,call(P,A1,A2,A3,A4,A5,A6,A7)).

mpred_pa_call(F,A,Call):-arity(F,A),current_predicate(F/A),call(Call).

mpred_fact_arity(F,A):-arity(F,A),once(mpred_prop(F,prologHybrid);mpred_prop(F,pfcControlled);mpred_prop(F,prologPTTP);mpred_prop(F,prologSNARK)).

prologHybridFact(G):- (var(G)->(mpred_fact_arity(F,A),functor(G,F,A));true),into_mpred_form(G,M),!,no_repeats(mpred_call(M)).

isCycPredArity_ignoreable(F,A):- ignore(user:mpred_prop(F,cycPred(A))),ignore(arity(F,A)).

which_t(dac(d,a_notnow,c,no_fallback)).

holds_t(P,A1,A2,A3,A4,A5,A6,A7):- isCycPredArity_ignoreable(P,7),which_t(DBS),(call_which_t(DBS,P,A1,A2,A3,A4,A5,A6,A7);call_mt_t(DBS,P,A1,A2,A3,A4,A5,A6,A7,_,_);assertion_t([P,A1,A2,A3,A4,A5,A6,A7])).
holds_t(P,A1,A2,A3,A4,A5,A6):- isCycPredArity_ignoreable(P,6),which_t(DBS),(call_which_t(DBS,P,A1,A2,A3,A4,A5,A6);call_mt_t(DBS,P,A1,A2,A3,A4,A5,A6,_,_)).
holds_t(P,A1,A2,A3,A4,A5):- isCycPredArity_ignoreable(P,5),which_t(DBS),(call_which_t(DBS,P,A1,A2,A3,A4,A5);call_mt_t(DBS,P,A1,A2,A3,A4,A5,_,_)).
holds_t(P,A1,A2,A3,A4):- isCycPredArity_ignoreable(P,4),which_t(DBS),(call_which_t(DBS,P,A1,A2,A3,A4);call_mt_t(DBS,P,A1,A2,A3,A4,_,_)).
holds_t(P,A1,A2,A3):- isCycPredArity_ignoreable(P,3),which_t(DBS),(call_which_t(DBS,P,A1,A2,A3);call_mt_t(DBS,P,A1,A2,A3,_,_)).
%holds_t(P,A1,A2):- hotrace(holds_relaxed_t(P,A1,A2)).
holds_t(P,A1,A2):- isCycPredArity_ignoreable(P,2),which_t(DBS),(call_which_t(DBS,P,A1,A2);call_mt_t(DBS,P,A1,A2,_,_)).
holds_t(P,A1):- isCycPredArity_ignoreable(P,1),which_t(DBS),(call_which_t(DBS,P,A1);call_mt_t(DBS,P,A1,_,_)).


% holds_relaxed_t(P,A1,A2):-var(A1),var(A2),!,dbase_t(P,A1,A2).
holds_relaxed_t(P,A1,A2):-
  isCycPredArity_ignoreable(P,2),which_t(DBS),
      relax_term(P,PR,A1,R1,A2,R2),
         holds_relaxed_0_t(DBS,PR,R1,R2).

holds_relaxed_0_t(DBS,P,A1,A2):- call_which_t(DBS,P,A1,A2).
holds_relaxed_0_t(DBS,P,A1,A2):- call_mt_t(DBS,P,A1,A2,_,_).

/*
holds_relaxed_0_t(dac(_,a,_,_),P,A1,A2):- assertion_t([P,A1,A2]).
holds_relaxed_0_t(dac(d,_,_,_),P,A1,A2):- dbase_t(P,A1,A2).
holds_relaxed_0_t(dac(_,_,_,h),P,A1,A2):- call_which_t(DBS,P,A1,A2).
holds_relaxed_0_t(DBS,P,A1,A2):- call_mt_t(DBS,P,A1,A2,_,_).
holds_relaxed_0_t(_DBS,P,A1,A2):- ground((P,A1)), TEMPL=..[P,T1,_],dbase_t(argSingleValueDefault,TEMPL,2,A2),req(isa(A1,T1)),!.
*/

holds_t([AH,P|LIST]):- is_holds_true(AH),!,holds_plist_t(P,LIST).
holds_t([AH,P|LIST]):- is_holds_false(AH),!,holds_f_p2(P,LIST).
holds_t([P|LIST]):- !,holds_plist_t(P,LIST).
holds_t(not(CALL)):- !, holds_f(CALL).
holds_t(CALL):- '=..'(CALL,PLIST),holds_t(PLIST).

holds_plist_t(P,LIST):- apply(holds_t,[P|LIST]).

% ========================================
% decl_mpred_hybrid/1/2/3
% ========================================

:-op(0,fx,((decl_mpred_hybrid))).
:-export((decl_mpred_hybrid)/1).
:-export((decl_mpred_hybrid)/2).
:-export((decl_mpred_hybrid_ilc)/3).
:-export((decl_mpred_hybrid4)/4).

:-op(0,fx,decl_mpred_hybrid).

:-meta_predicate_transparent(decl_mpred_hybrid(+)).
:-meta_predicate_transparent(decl_mpred_hybrid(+,+)).
:-meta_predicate_transparent(decl_mpred_hybrid(+,+,+)).


decl_mpred_hybrid(M):- must(with_pi(M,decl_mpred_hybrid4)).


decl_mpred_hybrid(F,A):- integer(A),!,decl_mpred_hybrid(F/A).
decl_mpred_hybrid(F,Other):- 
     decl_mpred(F,Other),
     get_functor(F,F0),
     must(arity(F0,A)),
     decl_mpred_hybrid(F0/A).

:-export(decl_mpred_hybrid/3).
decl_mpred_hybrid3(M, F, A):- atom(F),integer(A),!,must((functor(PI,F,A),decl_mpred_hybrid3(M,PI,F/A))).
decl_mpred_hybrid3(M,PI,FA):- loop_check(must(decl_mpred_hybrid_ilc(M,PI,FA)),true).


decl_mpred_hybrid_ilc(M,F,F/0):-
    arity(F,A),!,
    must((functor(PI,F,A),
    decl_mpred_hybrid_ilc(M,PI,F/A))).

decl_mpred_hybrid_ilc(M,PI,F/A):-
     must(not(user:mpred_prop(F,prologOnly))),
     assert_arity(F,A),
     must(arity(F,A)),
     must(M=user),
     decl_mpred_mfa(M,F,A),
     decl_mpred_pi(PI),
     must(user:provide_mpred_setup(call(conjecture),F/A,prologHybrid,_OUT)).
     

decl_mpred_hybrid4(_CM,M,PI,F/A):-
   decl_mpred_hybrid3(M,PI,F/A).

:-op(1150,fx,decl_mpred_hybrid).



ensure_universal_stub_plus_minus_2_HIDE(F,AMinus2):-
   decl_mpred(F,arity(AMinus2)),
   decl_mpred_hybrid(F/AMinus2).
   
ensure_universal_stub_plus_2(F,A2):- once(( AMinus2 is A2 -2, ensure_universal_stub_plus_minus_2(F,AMinus2))),fail.

%ensure_universal_stub_plus_2(F,A2):- cannot_override(F,A2,Why),!,dmsg(cannot_override_plus_2(F,A2,Why)).

ensure_universal_stub_plus_2(F,A2):- 
   export(F/A2),
   functor(HEAD,F,A2),
   HEAD=..[F|ARGS],
   append(ARGSMinus2,[_,_],ARGS),
   HEADMinus2=..[F|ARGSMinus2],
   AMinus2 is A2 -2,
   assert_if_new(HEAD:-HEADMinus2),!,
  % compile_predicates([HEAD]),
   dbase_mod(M),
   decl_mpred_hybrid(M,F,AMinus2).

% ==============================
% SETUP HYBRID HOOK
% ==============================
user:provide_mpred_setup(Op,HeadIn,StubType,OUT):- StubType \== prologOnly,
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
   dynamic_safe(F/A),
   call((asserta_if_new(user:mpred_prop(F,predStub(StubType))))),
   % abolish(F,A),
   % really_add_mpred_storage_op(Head),
   must((predicate_property(Head,dynamic))),
   (WasDynamic-> (dynamic_safe(F/A),(if_result(WasMulifile,multifile(F/A)))) ; dynamic_safe(F/A)),   
   % public(F/A),
   % lock_predicate(Head),
   retractall(user:mpred_prop(F,prologOnly)),
   pfcMarkC(Head),
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
   retractall(user:mpred_prop(F,prologOnly)),
   dmsg(compiled_new_predicate(HeadIn)),!.

ensure_universal_stub5(HeadIn,Head,F,A,HBLIST):- user:mpred_prop(F,prologOnly), must((StubType = prologHybrid)), !,
   forall(member(HB,HBLIST),must(show_call(assert_dbase_t(HB)))),!,
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
            retractall(user:mpred_prop(F,prologOnly)),
            set_prolog_flag(gc,true), garbage_collect_atoms,
            dmsg(compiled_new_predicate(HeadIn)));
       ((wdmsg(error(cannot_absorb_all_clauses((Head))))),!,fail)).


ensure_universal_stub5(HeadIn,Head,F,A,HBLIST):-  must((StubType = prologHybrid)),
   forall(member(HB,HBLIST),must(show_call(assert_dbase_t(HB)))),!,
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
          retractall(user:mpred_prop(F,prologOnly)),
          dmsg(merging_prolog_predicate(HeadIn)));
     ((wdmsg(error(cannot_absorb_all_clauses((Head))))),!,fail)).



assert_dbase_t((G:-B)):-is_true(B),!,must(assert_dbase_t(G)).
assert_dbase_t(DB):-once(fully_expand(change(assert,add),DB,MP)),DB\=@=MP,!,must(assert_dbase_t(MP)).
assert_dbase_t((G1,G2)):-!,assert_dbase_t(G1),assert_dbase_t(G2).
assert_dbase_t(G):-add_from_file(G).


:- op(1150,fx,decl_mpred_hybrid).

user:listing_mpred_hook(Match):- 
 (( 
  dif:dif(Proof,prologRef(_)),
  no_repeats_old([H,B],((user:provide_mpred_storage_clauses(H,B,Proof)),
                Proof\=prologRef(_))),slow_term_matches_hb(Match,H,B),portray_hb(Proof:H,B))),fail.

      
user:provide_mpred_storage_clauses(H,B,Proof):-dbase_t_mpred_storage_clauses_facts(H,B,Proof).

dbase_t_mpred_storage_clauses_facts(H,true,dbase_t(H)):-is_list(H),!,length(H,A),A>2,loop_check(dbase_t(H)).
dbase_t_mpred_storage_clauses_facts(H,true,dbase_t(H)):-compound(H),!,current_predicate(into_plist_arities/4),functor(H,_,A),A>1,loop_check(dbase_t(H)).
% dbase_t_mpred_storage_clauses_facts(H,B,W):-dbase_t_mpred_storage_clauses_rules(H,B,W),H\=isa(_,_).

% TODO USE PFC FOR FOREWARD RULES
% TODO USE PTTP FOR BACKARDS RULES
% dbase_t_mpred_storage_clauses_rules(H,B,ruleForward(B,H)):-ruleForward(B,H).
% dbase_t_mpred_storage_clauses_rules(H,B,ruleBackward(H,B)):-ruleBackward(H,B).
% dbase_t_mpred_storage_clauses_rules(H,B,'<=>'):-'<=>'(HH,B),each_subterm(HH,SubTerm),compound(SubTerm),SubTerm = H.
% dbase_t_mpred_storage_clauses_rules(H,B,'<=>'):-'<=>'(B,HH),each_subterm(HH,SubTerm),compound(SubTerm),SubTerm = H.


dbase_t_provide_mpred_storage_op(Op,HB):-notrace(demodulize(Op,HB,HeadBody)),get_functor(HeadBody,F),(F==dbase_t;user:mpred_prop(F,prologHybrid)), must(is_mpred_op(Op)), 
    with_assertions(thlocal:already_in_file_term_expansion,dbase_t_storage_op(Op,HeadBody)).

% ====================================================
% dbase_t_storage_op/2
% ====================================================
% HOOK Simplification
dbase_t_storage_op(Op,(Head:-Body)):- is_true(Body),!,dbase_t_storage_op(Op,Head).


dbase_t_storage_op(Op,H):- thglobal:pfcManageHybrids,!,pfc_provide_mpred_storage_op(Op,H).

dbase_t_storage_op(Op,(:-(Body))):-!,loop_check(dbase_op(Op,(:-(Body))),true),!.

% HOOK for ISA alt-forms
dbase_t_storage_op(_,isa(_,_)):- !,fail. % <- keeps u out of isa hybrids hairs
dbase_t_storage_op(Op,X):- was_isa(X,I,C),!,dbase_op(Op,isa(I,C)).

% HOOK MOST ALL CALLS
dbase_t_storage_op(Op,HeadBodyI):- notrace(((expand_term(HeadBodyI,HeadBodyM)),HeadBodyI\=@=HeadBodyM)),!,dbase_t_storage_op(Op,HeadBodyM).
dbase_t_storage_op(Op,X):- not(is_non_call_op(Op)),!,dbase_t_call_op(Op,X).

% RULE HOOK (for prolog special wrapper body stubs)
dbase_t_storage_op(Op,(Head:-Body)):-
 reduce_dbase_op(Op,Op2),
  special_wrapper_body(Body,direct_to_prolog),!,
  wdmsg(direct_to_prolog_special_wrapper_body(Op2,Head,Body)),
   (mud_call_store_op(Op2,(Head:-Body))).  

% OLD RULE HOOK (but we are using it in parallel)
dbase_t_storage_op(Op,(Head:-Body)):- \+ use_snark(Head,Body),
  wdmsg(saved_clause_in_hybridRule(Op,Head,Body)),!,
      (mud_call_store_op(Op,ruleBackward(Head,Body))).  

% PTTP RULE HOOK   
dbase_t_storage_op(Op,(Head:-Body)):- 
   mpred_call(use_snark(Head,Body)),!, 
   reduce_dbase_op(Op,Op2), 
   CALL0 = (call(Op2,ruleBackward(Head,Body))), % remember outside of SNARK just in case
   must(((CALL0,dbase_t_tell_snark(Op2,(Head:-Body))))),!.

% SNARK RULE HOOK   
dbase_t_storage_op(Op,RULE):- mpred_call(is_snark_rule(RULE)),!,
  reduce_dbase_op(Op,Op2),
  dbase_t_tell_snark(Op2,RULE),!.

% REOP HOOK dbase_t_storage_op(Op1,HeadBody):- reduce_dbase_op(Op1,Op2), Op1\==Op2, dbase_t_storage_op(Op2,HeadBody).
% FACT:-true HOOK   

% FACT DB HOOK
dbase_t_storage_op(Op,HeadBody):-
    into_functor_form(dbase_t,HeadBody,DB),
     % wff_check_dbase_t_throw(DB),
     must((mud_call_store_op(Op,DB),sanity(show_call(DB)))),!.

mud_call_store_op(Op,(H:-B)):- is_true(B),!,mud_call_store_op(Op,H).
mud_call_store_op(Op,dbase_t(was_imported_kb_content, _, OPRAND)):-!,loop_check(dbase_op(Op,OPRAND),true).
mud_call_store_op(Op,OPRAND):- show_call_success(wff_check_failed(Op,OPRAND,_WHY)),!.
mud_call_store_op(Op,OPRAND):- reduce_dbase_op(Op,Op2),show_call(call(Op2,OPRAND)).

wff_check_failed(_,DB,WHY):- DB =  dbase_t(was_imported_kb_content, WHY, _Assert).
wff_check_dbase_t_throw(DB):- wff_check_failed(_,DB,WHY),trace_or_throw(crazy_dbase_t_was_imported_kb_content(WHY,DB)).
wff_check_dbase_t_throw(_).

% ====================================================
% dbase_t_call_op/2
% ====================================================
% ISA CALL
dbase_t_call_op(_,isa(_,_)):- !,fail.
dbase_t_call_op(Op,X):- was_isa(X,I,C),!,dbase_op(Op,isa(I,C)).

% FACT CALL HOOK
dbase_t_call_op(_,FACT):- get_functor(FACT, F,A), !,
     call_tabled(call_for_literal(F,A,FACT)),!.


:- dynamic(use_ideep_swi/0).

% ====================================================
% call_for_literal/3
% ====================================================
call_for_literal(_,_,HEAD):- use_snark(HEAD,true),!,snark_ask(HEAD).
call_for_literal(_,_,HEAD):- use_ideep_swi,!,  call_for_literal_ideep_ilc(HEAD),!,loop_check_term(cwdl(CALL,7),HEAD,(CALL)).
call_for_literal(F,A,HEAD):- call_for_literal_db(F,A,HEAD).

call_for_literal_db(F,A,HEAD):- P=F, HEAD=..[P|ARGS],
   ((thglobal:after_dbase_load,missing_stub(HEAD))->decl_mpred_hybrid(F,A);true),
   constrain_args(P,ARGS),call_for_literal_db0(F,A,HEAD),constrain_args(P,ARGS).


cwdl(CALL,DEEP7):- call_with_depth_limit(CALL,DEEP7,Result),
   ( Result == depth_limit_exceeded -> (!,fail) ; true).

call_for_literal_ideep_ilc(HEAD):- get_functor(HEAD,F,A),call_for_literal_db(F,A,HEAD).

call_for_literal_db0(F,A,HEAD):-no_repeats(HEAD,call_for_literal_db00(F,A,HEAD)).

:- style_check(-singleton).
call_for_literal_db00(_,_,HEAD):- is_asserted_dbase_t(HEAD).
call_for_literal_db00(F,_,   _):- (isa(F,completelyAssertedCollection);hasInstance(completeExtentAsserted,F)),!,fail.
call_for_literal_db00(F,A,HEAD):- loop_check(call_rule_db(F,A,HEAD)).
call_for_literal_db00(F,A,HEAD):- not(use_snark(HEAD,true)),HEAD=..[P1,A1,A2],dif(P2,P1),loop_check_term(is_asserted_dbase_t(genlPreds(P2,P1)),gp(P1),fail),
   call(dbase_t,P2,A1,A2).

is_asserted_dbase_t(HEAD):-dbase_t(HEAD)*->true;((show_call_success(out_of_dbase_t(HEAD)))).
out_of_dbase_t(HEAD):-clause_safe(HEAD,true)*->true;show_call_success(user:fact_always_true(HEAD)).


call_rule_db(F,A,HEAD):- isa(F,completelyAssertedCollection),!,fail.
call_rule_db(F,A,HEAD):- use_snark(HEAD,_),!,snark_ask(HEAD).
call_rule_db(F,A,HEAD):- ruleBackward(HEAD,BODY),call_mpred_body(HEAD,BODY).

:- style_check(+singleton).

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


% call_body_req(HEAD):- functor(HEAD,F,A),HEAD_T=..[F|ARGS],HEAD_T=..[dbase_t,F|ARGS],hook_body_req(HEAD,HEAD_T).



% ========================================================================================
% BODY StubType
% ========================================================================================

body_req_isa(I,C):-isa_backchaing(I,C).

body_call_cyckb(HEAD_T):-el_holds_DISABLED_KB, HEAD_T =.. [dbase_t|PLIST], thglobal:use_cyc_database,!, no_repeats(kbp_t(PLIST)).

% =====================================
% = body_req
% =====================================
body_req(HEAD,HEAD_T):- (hook_body_req(HEAD,HEAD_T)).

:-export(body_req_normal/4).
%hook_body_req(HEAD,HEAD_T):- user:mpred_prop(F,prologPTTP),!,dmsg(warn(hook_body_req(HEAD,HEAD_T))),fail.
%hook_body_req(HEAD,HEAD_T):- user:mpred_prop(F,prologOnly),!,dmsg(warn(hook_body_req(HEAD,HEAD_T))),fail.
hook_body_req(_,_,isa(I,C),_):- !, body_req_isa(I,C).
hook_body_req(_,_,_,dbase_t(C,I)):- !, body_req_isa(I,C).
hook_body_req(_,_,_,hasInstance(C,I)):- !, body_req_isa(I,C).
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
body_req_only_rules(_,_,_,dbase_t(F,Obj,LValue)):-  choose_val(F,Obj,LValue).

body_req_plus_cyc(F,_,_,HEAD_T):-  user:mpred_prop(F,cycPlus2(_)),thlocal:useOnlyExternalDBs,!,with_assertions(thglobal:use_cyc_database,body_call_cyckb(HEAD_T)).
 
foo_b(b1).
foo_b(b2):-!.
foo_b(b3):-!.

:-must_det((findall(R,call_no_cuts(foo_b(R)),List),length(List,3))).



%OLD user:decl_database_hook(AR,C):-smart_decl_database(AR,C).

smart_decl_database(AR,svo(S,V,O)):- !,dbase2pred2svo(DBASE,PRED,svo(S,V,O)),!,smart_db_op(AR,DBASE,PRED,svo(S,V,O)).
smart_decl_database(AR,DBASE):- functor_catch(DBASE,dbase_t,_),!,dbase2pred2svo(DBASE,PRED,SVO),!,smart_db_op(AR,DBASE,PRED,SVO).
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

