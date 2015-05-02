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
% ========================================
% user:mpred_prop/1/2/3
% ========================================

:- export(arity/2).
:- export(mpred_module/2).
:- dynamic(mpred_module/2).
:- export(user:mpred_prop/2).
:- export(is_never_type/1).

pred_type_test(H,F/_):-!,atom(F),THFA=..[H,F],clause(THFA,true).
pred_type_test(H,F):- \+ compound(F), !,atom(F),THFA=..[H,F/_],clause(THFA,true).
pred_type_test(H,P):-functor(P,F,A),!,THFA=..[H,F/A],THF=..[H,F],(clause(THFA,true);clause(HF,true)).


pred_type_test2(T,F):- \+ compound(F),!,arity(F,A),!,pred_type_test(T,F,A).
pred_type_test2(T,F/A):-!,atom(F),arity(F,A),!,pred_type_test(T,F,A).
pred_type_test2(T,P):-functor(P,F,A),!,pred_type_test(T,F,A).

pred_type_test(H,F,A):- THFA=..[H,F/A],THF=..[H,F],(clause(THFA,true);clause(HF,true)).

arity(apathFn,2).
arity(isKappaFn,2).
arity(isInstFn,1).
arity(ftListFn,1).
arity(xyzFn,4).
arity(arity,2).
arity(is_never_type,1).
arity(argIsa, 3).
arity(Prop,1):-ttPredType(Prop).
arity(meta_argtypes,1).
arity(arity,2).
arity(is_never_type,1).
arity(prologSingleValued,1).
arity('<=>',2).
arity(F,A):- atom(F), current_predicate(F/A),A>1.
arity(F,1):- atom(F), current_predicate(F/1),\+((dif:dif(Z,1), arity(F,Z))).


decl_mpred_pi(PI):-ignore((ground(PI),compound(PI),decl_mpred(PI))).
:-export(decl_mpred_mfa/3).
decl_mpred_mfa(_,M:F,A):-atom(M),!,decl_mpred_mfa(M,F,A).
decl_mpred_mfa(M,FF,A):-var(M),!,context_module(M),!,decl_mpred_mfa(M,FF,A).
decl_mpred_mfa(M,FF,A):-
   get_functor(FF,F,_),
   must_det_l([
     ignore((var(M),context_module(M),dmsg(decl_mpred_mfa(M,F,A)))),
     ignore((nonvar(M),asserta_if_new(user:mpred_prop(F,mpred_module(M))))),
     assert_arity(F,A),  
     must_det(nonvar(M)),
    '@'((
     nop((static_predicate(M,F,A)->true; (M:dynamic(F/A),M:discontiguous(F/A)))), 
     nop(M:export(F/A)),
     nop(M:multifile(M:F/A))),M) ]).



% ========================================
% decl_mpred_prolog/1/2/3
% ========================================
:-op(0,fx,(decl_mpred_prolog)).

:-export(decl_mpred_prolog/1).
decl_mpred_prolog(A):-not(compound(A)),!.
decl_mpred_prolog(M):- M=..[isEach|List],!,maplist(decl_mpred_prolog,List).
decl_mpred_prolog(P):- with_pi(P,decl_mpred_prolog).

:-export(decl_mpred_prolog/3).
decl_mpred_prolog(M,F,A):-integer(A),!,must(functor(PI,F,A)),decl_mpred_prolog(M,PI,F/A).
decl_mpred_prolog(M,PI,FA):- must(decl_mpred_prolog(_,M,PI,FA)).
decl_mpred_prolog(F,A):- integer(A),!,decl_mpred_prolog(F/A).
decl_mpred_prolog(F,Other):- decl_mpred(F,Other),
     get_functor(F,F0),
     must(arity(F0,A)),
     decl_mpred_prolog(F0/A).
:-export(decl_mpred_prolog/4).
decl_mpred_prolog(CM,M,PI,FA):- loop_check(must(decl_mpred_prolog_ilc(CM,M,PI,FA)),true).

decl_mpred_prolog_ilc(CM,M,PI,F/A):-atom(PI),A==0,not(current_predicate(F/A)),!,must(arity(F,_)),forall((arity(F,AA),AA\=0),(functor(PIA,F,AA),decl_mpred_prolog_ilc(CM,M,PIA,F/AA))).
decl_mpred_prolog_ilc(CM,M,PI,F/A):-loop_check_term(decl_mpred_prolog_ilc_0(CM,M,PI,F/A),decl_mpred_prolog_ilc(CM,M,F),true).
decl_mpred_prolog_ilc_0(CM,M,PI,F/A):-
      assert_arity(F,A),
      add(mpred_module(PI,M)),
      add(user:mpred_prop(PI,prologOnly)),
      add(user:mpred_prop(PI,predCanHaveSingletons)),!.


% ========================================
% decl_mpred_hybrid/1/2/3
% ========================================
:-op(0,fx,(decl_mpred_hybrid)).

:-export(decl_mpred_hybrid/1).
decl_mpred_hybrid(A):-not(compound(A)),!.
decl_mpred_hybrid(M):-M=..[isEach|List],!,maplist(decl_mpred_hybrid,List).
decl_mpred_hybrid(P):- with_pi(P,decl_mpred_hybrid).

:-export(decl_mpred_hybrid/3).
decl_mpred_hybrid(M,F,A):-integer(A),!,must(functor(PI,F,A)),decl_mpred_hybrid(M,PI,F/A).
decl_mpred_hybrid(M,PI,FA):- must(decl_mpred_hybrid(_,M,PI,FA)).
decl_mpred_hybrid(F,A):- integer(A),!,decl_mpred_hybrid(F/A).
decl_mpred_hybrid(F,Other):- decl_mpred(F,Other),
     get_functor(F,F0),
     must(arity(F0,A)),
     decl_mpred_hybrid(F0/A).

:-export(decl_mpred_hybrid/4).
decl_mpred_hybrid(CM,M,PIN,FA):- unnumbervars(PIN,PI),loop_check(must(decl_mpred_hybrid_ilc(CM,M,PI,FA)),true).

decl_mpred_hybrid_ilc(CM,M,PI,F/A):-atom(PI),A==0,must(arity(F,_)),not(current_predicate(F/A)),!,
   forall((arity(F,AA),AA\=0),(functor(PIA,F,AA),decl_mpred_hybrid_ilc(CM,M,PIA,F/AA))).

decl_mpred_hybrid_ilc(CM,M,PIN,F/A):- unnumbervars(PIN,PI),loop_check_term(decl_mpred_hybrid_ilc_0(CM,M,PI,F/A),decl_mpred_hybrid_ilc(CM,M,F),true).
decl_mpred_hybrid_ilc_0(CM,M,PI,F/A):-
      assert_arity(F,A),
      add(mpred_module(F,M)),
      add(prologHybrid(F)),
      get_cc(PI,NC),
      must(M=user),     
      decl_mpred_mfa(M,F,A),
      decl_mpred_pi(PI),
      must(user:provide_mpred_setup(call(conjecture),F/A,prologHybrid,_OUT)),
      must((get_cc(PI,NCN),NCN>=NC)).



:-op(1120,fx,(decl_mpred_hybrid)).

prologHybrid(X,Y):-dtrace(prologHybrid(X,Y)).
:-lock_predicate(prologHybrid(X,Y)).

% ========================================
% mpred_props database
% ========================================

/*



% user:mpred_prop(F,prologOnly):- not(user:mpred_prop(F,prologHybrid)),(F=ttPredType;(current_predicate(F/1);not(t(F,tCol)))).
user:mpred_prop(G,predProxyAssert(add)):- atom(G),prologMacroHead(G).
user:mpred_prop(G,predProxyQuery(ireq)):- atom(G),prologMacroHead(G).
user:mpred_prop(G,predProxyRetract(del)):- atom(G),prologMacroHead(G).
*/



get_mpred_prop(F,_A,P):-get_mpred_prop(F,P).
get_mpred_prop(F,P):- user:mpred_prop(F,P).

:- export(listprolog/0).
listprolog:-listing(user:mpred_prop(_,prologOnly)).


get_arity(Term,F,A):- atom(Term),F=Term,!,ensure_arity(F,A).
get_arity(F/A,F,A):- atom(F),ensure_arity(F,A),!,(A>0).
get_arity(M:FA,F,A):-atom(M),!,get_arity(FA,F,A).
get_arity(FA,F,A):- get_functor(FA,F,A),must(A>0).

ensure_arity(F,A):- one_must(arity(F,A),one_must((current_predicate(F/A),(A>0),assert_arity(F,A)),(ground(F:A),(A>0),assert_arity(F,A)))),!.

assert_arity(F,A):-not(atom(F)),trace_or_throw(assert_arity(F,A)).
assert_arity(F,A):-not(integer(A)),trace_or_throw(assert_arity(F,A)).
assert_arity(typeProps,0):- trace_or_throw(assert_arity(typeProps,0)).
assert_arity(argsIsa,2):- trace_or_throw(assert_arity_argsIsa(F,2)).
assert_arity(F,A):- must_det(good_pred_relation_name(F,A)),fail.
assert_arity(F,A):- arity(F,A),!.
assert_arity(F,A):- arity(F,AA), A\=AA,dmsg(trace_or_throw(assert_arity_switched(F,AA->A))),fail.
assert_arity(F,A):- pfc_add_fast(arity(F,A)).


good_pred_relation_name(F,A):-not(bad_pred_relation_name0(F,A)).

bad_pred_relation_name0(V,_):-not(atom(V)),!.
bad_pred_relation_name0('[]',_).
bad_pred_relation_name0('',_).
bad_pred_relation_name0('!',_).
bad_pred_relation_name0('{}',_).
bad_pred_relation_name0(',',_).
bad_pred_relation_name0('[|]',_).
bad_pred_relation_name1(X,Y):-bad_pred_relation_name0(X,Y).
bad_pred_relation_name1(F,A):-must_det((atom_codes(F,[C|_]),to_upper(C,U))),!, U == C, A>1.
bad_pred_relation_name1(F,A):-arity(F,AO), A \= AO.

:-at_start(writeq("Seen Mpred_props at start!\n")),!.


% ========================================
% decl_mpred database
% ========================================

:- export((decl_mpred/1)).

decl_mpred((A,B)):-decl_mpred(A),decl_mpred(B).
decl_mpred(M):-loop_check(with_pi(M,decl_mpred_4),true).

decl_mpred_4(user,prologSingleValued(ARGS),prologSingleValued/1):- compound(ARGS),get_functor(ARGS,F,A),!, decl_mpred(F,[prologArity(A),prologSingleValued,meta_argtypes(ARGS)]),!.
decl_mpred_4(_,F,F/0):-!,assert_hasInstance(tPred,F).
decl_mpred_4(M,PI,F/A):-
   decl_mpred(F,A),
   ignore((ground(PI),compound(PI),decl_mpred(F,meta_argtypes(PI)))),
   decl_mpred(F,[mpred_module(M)]).

:-export(decl_mpred/2).
decl_mpred(C,A):- integer(A),!,decl_mpred(C/A).
decl_mpred(C,More):- ignore(loop_check(decl_mpred_0(C,More),true)).

decl_mpred_0(C,More):- (var(C);var(More)), trace_or_throw(var_decl_mpred(C,More)).
decl_mpred_0(F/A,More):-atom(F),integer(A),!,assert_arity(F,A),decl_mpred(F,More),!.
decl_mpred_0(M:FA,More):-atom(M),!,decl_mpred_0(FA,More),decl_mpred_0(FA,mpred_module(M)).
decl_mpred_0(F,A):-atom(F),number(A),!,assert_arity(F,A).
decl_mpred_0(F,tPred):-!,assert_hasInstance(tPred,F).
decl_mpred_0(C,More):-string(C),!,dmsg(trace_or_throw(var_string_decl_mpred(C,More))).
decl_mpred_0(mudDescription, predProxyRetract):-dtrace(decl_mpred_0(mudDescription, predProxyRetract)).
decl_mpred_0(_,meta_argtypes):-!.
decl_mpred_0(F,meta_argtypes(ArgTypes)):-!,decl_mpred_2(F,meta_argtypes(ArgTypes)).
decl_mpred_0(C,More):-compound(C),C=..[F,Arg1|PROPS],ttPredType(F),!,ground(Arg1),decl_mpred(Arg1,[F,PROPS,More]).
decl_mpred_0(C,More):-compound(C),!,functor(C,F,A),assert_arity(F,A),decl_mpred_0(F,More),!,ignore((ground(C),decl_mpred(F,meta_argtypes(C)))),!.
decl_mpred_0(_,[]):-!.
decl_mpred_0(F,[Prop|Types]):-!,decl_mpred_0(F,Prop),!,decl_mpred_0(F,Types),!.

decl_mpred_0(F,T):-doall(( decl_mpred_2(F,T) )).


decl_mpred_2(F,meta_argtypes(FARGS)):- functor(FARGS,_,A),decl_mpred(F,A),fail.
decl_mpred_2(_,meta_argtypes(FARGS)):- functor(FARGS,_,A),arg(A,FARGS,Arg),var(Arg),!.

% decl_mpred_2(F,prologHybrid):- decl_mpred_hybrid(F).
decl_mpred_2(F,cycPlus2(A)):- ensure_universal_stub_plus_2(F,A).

decl_mpred_2(F,A):-once(user:provide_mpred_write_attributes(F,A)).
decl_mpred_2(F,Prop):-add(mpred_prop(F,Prop)).

decl_mpred(Mt,F,A):-decl_mpred(F,A),ignore((nonvar(Mt),decl_mpred(F,mt(Mt)))).
decl_mpred_4(_CM,M,PI,F/A):-
   decl_mpred_4(M,PI,F/A).


functor_check_univ(M:G1,F,List):-atom(M),member(M,[dbase,user]),!,functor_check_univ(G1,F,List),!.
functor_check_univ(G1,F,List):-must_det(compound(G1)),must_det(G1 \= _:_),must_det(G1 \= _/_),G1=..[F|List],!.

:-export(glean_pred_props_maybe/1).
glean_pred_props_maybe(_:G):-!,compound(G),with_assertions(infConfidence(vWeak),forall(glean_pred_props_maybe_some(G),true)).
glean_pred_props_maybe(G):-compound(G),with_assertions(infConfidence(vWeak),forall(glean_pred_props_maybe_some(G),true)).

glean_pred_props_maybe_some(G):-compound(G),G=..[F,Arg1|RGS],ttPredType(F),add_mpred_prop_gleaned(Arg1,[F|RGS]).
% glean_pred_props_maybe_some(G):-arg(_,G,Arg1),compound(Arg1),arg(_,Arg1,Col),t(tCol,Col),with_assertions(infConfidence(vWeak),assert_predArgTypes(Arg1)).

add_mpred_prop_gleaned(M:Arg1,FRGS):-atom(M),!,add_mpred_prop_gleaned(Arg1,FRGS).
add_mpred_prop_gleaned(Arg1,FRGS):-functor_check_univ(Arg1,F,ARGSISA),add_mpred_prop_gleaned_4(Arg1,F,ARGSISA,FRGS).
add_mpred_prop_gleaned_4(Arg1,_F,[ARG|_],FRGS):-nonvar(ARG),!,decl_mpred(Arg1,[meta_argtypes(Arg1)|FRGS]).
add_mpred_prop_gleaned_4(Arg1,_F,_,FRGS):-decl_mpred(Arg1,FRGS).



% user:term_expansion(G,_):- \+ thlocal:disable_mpred_term_expansions_locally, not(thlocal:into_form_code),hotrace((once(glean_pred_props_maybe(G)),fail)).
