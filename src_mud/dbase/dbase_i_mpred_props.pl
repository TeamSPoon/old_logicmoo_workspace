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
% user:mpred_prop/1/2/3
% ========================================

:- export arity/2.
:- export user:mpred_prop/3.
:- export is_never_type/1.

%OLD user:decl_database_hook(change(assert,_),Fact):- ignore((compound(Fact),Fact=..[F,Arg1|PROPS],is_pred_declarer(F),decl_mpred(Arg1,[F|PROPS]))).
%OLD user:decl_database_hook(change(assert,_),isa(F,P)):- is_pred_declarer(P),decl_mpred(F,P).
% this causes too many bugs user:decl_database_hook(change(assert,_),user:mpred_prop(F,arity(A))):- ignore((A==1,atom(F),not(is_never_type(F)),not(user:mpred_prop(F,prologOnly)),decl_type(F))).
%OLD user:decl_database_hook(change(assert,_),user:mpred_prop(F,P)):- decl_mpred(F,P).

asserted_mpred_prop(F,P):-clause(mpred_prop(F,P),true).

arity(apathFn,2).
arity(isKappaFn,2).
arity(isInstFn,1).
arity(ftListFn,1).
arity(xyzFn,4).
arity(arity,2).
arity(mpred_prop,3).
arity(is_never_type,1).
arity(self_call,1).
arity(argIsa, 3).
arity(isa, 2).
%arity(Prop,2):-is_pred_declarer(Prop).
arity(predArgTypes,2).
arity(arity,2).
arity(is_never_type,1).
arity(prologSingleValued,1).
arity(mudTermAnglify,2).
arity('<=>',2).
arity(typeHasGlyph,2).
arity(mudMaxHitPoints,2).
arity(F,A):- atom(F), current_predicate(F/A),A>1.
arity(F,1):- atom(F), current_predicate(F/1).


decl_mpred_pi(PI):-ignore((ground(PI),compound(PI),decl_mpred(PI))).
:-export(decl_mpred_mfa/3).
decl_mpred_mfa(_,M:F,A):-atom(M),!,decl_mpred_mfa(M,F,A).
decl_mpred_mfa(M,FF,A):-var(M),!,context_module(M),!,decl_mpred_mfa(M,FF,A).
decl_mpred_mfa(M,FF,A):-
   get_functor(FF,F,_),
   must_det_l([
     ignore((var(M),context_module(M),dmsg(decl_mpred_mfa(M,F,A)))),
     ignore((nonvar(M),asserta_if_new(user:mpred_prop(F,predModule(M))))),
     assert_arity(F,A),  
     must_det(nonvar(M)),
    '@'((
     nop((static_predicate(M,F,A)->true; M:dynamic(F/A))), 
     nop(M:export(F/A)),
     nop(M:multifile(M:F/A))),M) ]).


ensure_universal_stub_plus_minus_2(F,AMinus2):-
   decl_mpred(F,arity(AMinus2)),
   decl_mpred_mfa(user,F,AMinus2).
   

:-export(registerCycPredPlus2/1).


registerCycPredPlus2_3(_CM,M,PI,F/A2):-
  registerCycPredPlus2_3(M,PI,F/A2).

registerCycPredPlus2_3(M,_PI,F/A2):- 
  ignore((A2==3,assertz_if_new(is_never_type(F)))),
  A is A2 - 2, decl_mpred_mfa(M,F,A),
  decl_mpred(F,cycPlus2(A2)),decl_mpred(F,cycPred(A)).


registerCycPredPlus2(P):-!,user:with_pi(P,registerCycPredPlus2_3).


:-op(0,fx,(decl_mpred_prolog)).

:-export(decl_mpred_prolog/1).
decl_mpred_prolog(P):- with_pi(P,decl_mpred_prolog).

:-export(decl_mpred_prolog/3).
decl_mpred_prolog(M,F,A):-integer(A),!,must(functor(PI,F,A)),decl_mpred_prolog(M,PI,F/A).
decl_mpred_prolog(M,PI,FA):- must(decl_mpred_prolog(_,M,PI,FA)).

decl_mpred_prolog(F,Other):- 
     decl_mpred(F,Other),
     get_functor(F,F0),
     must(arity(F0,A)),
     decl_mpred_prolog(F0/A).

:-export(decl_mpred_prolog/4).
decl_mpred_prolog(CM,M,PI,FA):- loop_check(must(decl_mpred_prolog_ilc(CM,M,PI,FA)),true).

% decl_mpred_prolog_ilc(_,_,_,_):-!.
decl_mpred_prolog_ilc(CM,M,PI,F/A):-
      decl_mpred(F,arity(A)),
      decl_mpred(F,prologOnly),
      decl_mpred(F,predCanHaveSingletons),
      decl_mpred(F,[info(decl_mpred_prolog(CM,M,PI,F/A))]),
      decl_mpred(PI,predModule(M)).   


:-op(1120,fx,decl_mpred_prolog).



% ========================================
% mpred_props database
% ========================================
user:mpred_prop(resolveConflict,predModule(user)).
user:mpred_prop(pfcSelect,predModule(user)).
user:mpred_prop(agent_text_command/4,prologOnly).
user:mpred_prop(dbase_t,prologOnly).
user:mpred_prop(member/2,prologOnly).
user:mpred_prop(arity/2,prologOnly).
user:mpred_prop(mpred_prop/3,prologOnly).
user:mpred_prop(is_never_type/1,prologOnly).
user:mpred_prop(term_expansion/2,prologOnly).
user:mpred_prop(var/1,prologOnly).
user:mpred_prop(F,Prop):- current_predicate(hasInstance/2),  (nonvar(Prop)->(hasInstance(Prop, F));((hasInstance(ttPredType,Prop)),(hasInstance(Prop, F)))).
user:mpred_prop(F,Prop):-nonvar(F),mpred_prop_nvh(F,Prop).
%user:mpred_prop(F,tCol):-current_predicate(tCol/1),tCol(F).

mpred_prop_nvh(H0,mpred_prop(Prop)):-get_arity(H0,F,A),atom(F),integer(A),functor(H,F,A),predicate_property(H,Prop).
mpred_prop_nvh(H0,(Prop)):-nonvar(Prop),get_arity(H0,F,A),atom(F),integer(A),functor(H,F,A),predicate_property(H,Prop).
mpred_prop_nvh(H,Prop):-get_functor(H,F,A), H \=@= F, !,user:mpred_prop(F,Prop).

/*



% user:mpred_prop(F,prologOnly):- not(user:mpred_prop(F,prologHybrid)),(F=is_pred_declarer;(current_predicate(F/1);not(hasInstance(F,tCol)))).
user:mpred_prop(G,predProxyAssert(add)):- atom(G),prologMacroHead(G).
user:mpred_prop(G,predProxyQuery(ireq)):- atom(G),prologMacroHead(G).
user:mpred_prop(G,predProxyRetract(del)):- atom(G),prologMacroHead(G).
*/



get_mpred_prop(F,_A,P):-get_mpred_prop(F,P).
get_mpred_prop(F,P):- user:mpred_prop(F,P).

:- export(listprolog/0).
listprolog:-listing(user:mpred_prop(_,prologOnly)).


get_arity(Term,F,A):- atom(Term),F=Term,!,ensure_arity(F,A).
get_arity(F/A,F,A):- atom(F),ensure_arity(F,A),!.
get_arity(M:FA,F,A):-atom(M),!,get_arity(FA,F,A).
get_arity(FA,F,A):- get_functor(FA,F,A).

ensure_arity(F,A):- one_must(arity(F,A),one_must((current_predicate(F/A),assert_arity(F,A)),(ground(F:A),assert_arity(F,A)))),!.

assert_arity(typeProps,0):- trace_or_throw(assert_arity(typeProps,0)).
assert_arity(F,A):-not(atom(F)),trace_or_throw(assert_arity(F,A)).
assert_arity(F,A):-not(integer(A)),trace_or_throw(assert_arity(F,A)).
assert_arity(F,A):-arity(F,A),assert_if_new(user:mpred_prop(F,arity(A))),!.
assert_arity(F,A):-arity(F,1),dmsg(trace_or_throw(was_one_assert_arity(F,A))),!.
assert_arity(F,2):-F = argsIsa, nottrace_or_throw(assert_arity_argsIsa(F,2)).
assert_arity(F,0):- dmsg(trace_or_throw(assert_arity(F,0))),!.
assert_arity(F,A):-assert_if_new(arity(F,A)),assert_if_new(user:mpred_prop(F,arity(A))),!.
assert_arity(F,A):-dmsg(failed_assert_arity(F,A)).

assert_arity_ilc(F,A):-
  % A2 is A+2,ensure_universal_stub_plus_2(F,A2),
  retractall(user:mpred_prop(F,arity(_))),
  retractall(arity(F,_)),
   must_det(good_pred_relation_name(F,A)),
    hooked_asserta(arity(F,A)),
    hooked_asserta(user:mpred_prop(F,arity(A))),!.



good_pred_relation_name(F,A):-not(bad_pred_relation_name(F,A)).

bad_pred_relation_name(V,_):-not(atom(V)),!.
bad_pred_relation_name('[]',_).
bad_pred_relation_name('[|]',_).
bad_pred_relation_name(F,A):-must_det((atom_codes(F,[C|_]),to_upper(C,U))),!, U == C, A>1.
bad_pred_relation_name(F,A):-arity(F,AO), A \= AO.

:-at_start(writeq("at start!~n")).

first_mpred_props(arity(_)).
first_mpred_props(predArgTypes(_)).

mpred_prop_ordered(Pred,Prop):-first_mpred_props(Prop),user:mpred_prop(Pred,Prop),not(user:mpred_prop(Pred,prologOnly)).
mpred_prop_ordered(Pred,Prop):-user:mpred_prop(Pred,Prop),not(first_mpred_props(Prop)),not(user:mpred_prop(Pred,prologOnly)).

:- export((decl_mpred/1)).

decl_mpred((A,B)):-decl_mpred(A),decl_mpred(B).
decl_mpred(M):-loop_check_local(with_pi(M,decl_mpred_4),true).

decl_mpred_4(user,prologSingleValued(ARGS),prologSingleValued/1):- compound(ARGS),get_functor(ARGS,F,A),!, decl_mpred(F,[prologArity(A),prologSingleValued,predArgTypes(ARGS)]),!.
decl_mpred_4(_,F,F/0):-!,assert_hasInstance(tPred,F).
decl_mpred_4(M,PI,F/A):-
   decl_mpred(F,A),
   ignore((ground(PI),compound(PI),decl_mpred(F,predArgTypes(PI)))),
   decl_mpred(F,[predModule(M)]).

:-export(decl_mpred/2).
decl_mpred(C,More):- ignore(loop_check(decl_mpred_0(C,More),true)).

decl_mpred_0(C,More):- (var(C);var(More)), trace_or_throw(var_decl_mpred(C,More)).
decl_mpred_0(F/A,More):-atom(F),integer(A),!,decl_mpred_2(F,arity(A)),decl_mpred(F,More),!.
decl_mpred_0(M:FA,More):-atom(M),!,decl_mpred_0(M:FA,More),decl_mpred_0(FA,[predModule(M)]).
decl_mpred_0(F,A):-number(A),!,decl_mpred_2(F,arity(A)),!.
decl_mpred_0(F,tPred):-!,assert_hasInstance(tPred,F).
decl_mpred_0(C,More):-string(C),!,dmsg(trace_or_throw(var_string_decl_mpred(C,More))).
decl_mpred_0(mudDescription, predProxyRetract):-dtrace(decl_mpred_0(mudDescription, predProxyRetract)).
decl_mpred_0(_,predArgTypes):-!.
decl_mpred_0(F,predArgTypes(ArgTypes)):-!,decl_mpred_2(F,predArgTypes(ArgTypes)).
decl_mpred_0(C,More):-compound(C),C=..[F,Arg1|PROPS],is_pred_declarer(F),!,ground(Arg1),decl_mpred(Arg1,[F,PROPS,More]).
decl_mpred_0(C,More):-compound(C),!,functor(C,F,A),decl_mpred_2(F,arity(A)),decl_mpred_0(F,More),!,ignore((ground(C),decl_mpred(F,predArgTypes(C)))),!.
decl_mpred_0(_,[]):-!.
decl_mpred_0(F,[Prop|Types]):-!,decl_mpred_0(F,Prop),!,decl_mpred_0(F,Types),!.

decl_mpred_0(F,T):-doall(( decl_mpred_2(F,T) )).


decl_mpred_2(F,predArgTypes(FARGS)):- functor(FARGS,_,A),decl_mpred(F,A),fail.
decl_mpred_2(_,predArgTypes(FARGS)):- functor(FARGS,_,A),arg(A,FARGS,Arg),var(Arg),!.
decl_mpred_2(F,arity(A)):- assert_arity(F,A),fail.

% decl_mpred_2(F,prologHybrid):- decl_mpred_hybrid(F).
decl_mpred_2(F,cycPlus2(A)):- ensure_universal_stub_plus_2(F,A).

decl_mpred_2(F,Prop):-user:mpred_prop(F,Prop),!.
decl_mpred_2(F,Prop):-hooked_asserta(user:mpred_prop(F,Prop)),fail.

decl_mpred_2(F,A):-once(user:provide_mpred_write_attributes(F,A)).

decl_mpred(Mt,F,A):-decl_mpred(F,A),ignore((nonvar(Mt),decl_mpred(F,mt(Mt)))).
decl_mpred_4(_CM,M,PI,F/A):-
   decl_mpred_4(M,PI,F/A).


functor_check_univ(M:G1,F,List):-atom(M),member(M,[dbase,user]),!,functor_check_univ(G1,F,List),!.
functor_check_univ(G1,F,List):-must_det(compound(G1)),must_det(G1 \= _:_),must_det(G1 \= _/_),G1=..[F|List],!.

:-export(glean_pred_props_maybe/1).
glean_pred_props_maybe(_:G):-!,compound(G),with_assertions(infConfidence(vWeak),forall(glean_pred_props_maybe_some(G),true)).
glean_pred_props_maybe(G):-compound(G),with_assertions(infConfidence(vWeak),forall(glean_pred_props_maybe_some(G),true)).

glean_pred_props_maybe_some(G):-compound(G),G=..[F,Arg1|RGS],is_pred_declarer(F),add_mpred_prop_gleaned(Arg1,[F|RGS]).
% glean_pred_props_maybe_some(G):-arg(_,G,Arg1),compound(Arg1),arg(_,Arg1,Col),hasInstance(tCol,Col),with_assertions(infConfidence(vWeak),assert_predArgTypes(Arg1)).

add_mpred_prop_gleaned(M:Arg1,FRGS):-atom(M),!,add_mpred_prop_gleaned(Arg1,FRGS).
add_mpred_prop_gleaned(Arg1,FRGS):-functor_check_univ(Arg1,F,ARGSISA),add_mpred_prop_gleaned_4(Arg1,F,ARGSISA,FRGS).
add_mpred_prop_gleaned_4(Arg1,_F,[ARG|_],FRGS):-nonvar(ARG),!,decl_mpred(Arg1,[predArgTypes(Arg1)|FRGS]).
add_mpred_prop_gleaned_4(Arg1,_F,_,FRGS):-decl_mpred(Arg1,FRGS).

assert_predArgTypes(ArgTs):-not(compound(ArgTs)),!.
assert_predArgTypes(ArgTs):- numbervars(ArgTs,0,_,[functor_name(ftTerm)]),get_functor(ArgTs,F),assert_predArgTypes_fa(F,ArgTs).

assert_predArgTypes_fa(F,ArgTs):- not(is_list(ArgTs)),ArgTs=..[_|ArgsL],!,assert_predArgTypes_fa(F,ArgsL).
assert_predArgTypes_fa(F,ArgsList):- isa(F,ftAction),!,show_call(must(assert_predArgTypes_from_left(F,1,ArgsList))).
assert_predArgTypes_fa(F,ArgsList):- length(ArgsList,L),assert_predArgTypes_l(F,L,ArgsList).

assert_predArgTypes_l(F,L,ArgsList):- arity(F,A),!,must( (A>=L) -> assert_predArgTypes_from_right(F,A,ArgsList);true).
assert_predArgTypes_l(F,L,ArgsList):- must(assert_predArgTypes_from_right(F,L,ArgsList)).


assert_predArgTypes_from_right(_,_,[]):-!.
assert_predArgTypes_from_right(F,A,ArgsList):-append(Left,[Last],ArgsList),assert_argIsa(F,A,Last),!,Am1 is A -1, assert_predArgTypes_from_right(F,Am1,Left).

assert_predArgTypes_from_left(_,_,[]):-!.
assert_predArgTypes_from_left(F,A,[Type|ArgsList]):-assert_argIsa(F,A,Type),!,Ap1 is A + 1,assert_predArgTypes_from_left(F,Ap1,ArgsList).


user:term_expansion(G,_):- not(prolog_mud_disable_term_expansions), not(thlocal:into_form_code),notrace((once(glean_pred_props_maybe(G)),fail)).
