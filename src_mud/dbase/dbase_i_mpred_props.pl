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
% decl_mpred_hybrid/1/2/3
% ========================================

:- dynamic_multifile_exported mpred_arity/2.
:- dynamic_multifile_exported mpred_prop/2.
:- dynamic_multifile_exported never_type/1.

mpred_prop(dbase_t,prologOnly).
mpred_prop(mpred_prop,prologOnly).

decl_database_hook(assert(_),Fact):- ignore((compound(Fact),Fact=..[F,Arg1|PROPS],ttDeclarer(F),decl_mpred(Arg1,[F|PROPS]))).
decl_database_hook(assert(_),mudIsa(F,P)):- ttDeclarer(P),decl_mpred(F,P).
% this causes too many bugs decl_database_hook(assert(_),mpred_prop(F,predArity(A))):- ignore((A==1,atom(F),not(never_type(F)),not(mpred_prop(F,prologOnly)),decl_type(F))).
decl_database_hook(assert(_),mpred_prop(F,P)):- decl_mpred(F,P).

mpred_arity(predArgTypes,1).
mpred_arity(mpred_prop,2).
mpred_arity(mpred_arity,2).
mpred_arity(never_type,1).
mpred_arity(prologSingleValued,1).
mpred_arity(mudTermAnglify,2).
mpred_arity(ruleEquiv,2).

never_type(Var):-var(Var),!,trace_or_throw(var_never_type(Var)).
never_type('Area1000').
never_type(mudSubclass).
never_type(must).
never_type(mpred_prop).
never_type(mudFtInfo).
never_type(C):- compound(C),functor(C,F,1),isa_asserted(F,tCol).
never_type(F):- mpred_arity(F,A),!, A > 1.

decl_mpred_pi(PI):-ignore((ground(PI),compound(PI),decl_mpred(PI))).
:-swi_export(decl_mpred_mfa/3).
decl_mpred_mfa(_,M:F,A):-atom(M),!,decl_mpred_mfa(M,F,A).
decl_mpred_mfa(M,FF,A):-var(M),!,context_module(M),!,decl_mpred_mfa(M,FF,A).
decl_mpred_mfa(M,FF,A):-
   functor_h(FF,F,_),
   must_det_l([
     ignore((var(M),context_module(M),dmsg(decl_mpred_mfa(M,F,A)))),
     ignore((nonvar(M),asserta_if_new(mpred_prop(F,def_module(M))))),
     assert_arity(F,A),     
     must_det(nonvar(M)),
    '@'((
     (static_predicate(M,F,A)->true; M:dynamic(F/A)), 
     M:export(F/A),
     M:multifile(M:F/A)),M),
     scan_missing_stubs(F) ]).


declare_dbase_local_dynamic_plus_minus_2(F,AMinus2):-
   decl_mpred(F,predArity(AMinus2)),
   decl_mpred_mfa(user,F,AMinus2).
   

:-swi_export(registerCycPredPlus2/1).


registerCycPredPlus2_3(_CM,M,PI,F/A2):-
  registerCycPredPlus2_3(M,PI,F/A2).

registerCycPredPlus2_3(M,_PI,F/A2):- 
  ignore((A2==3,assertz_if_new(never_type(F)))),
  A is A2 - 2, decl_mpred_mfa(M,F,A),
  decl_mpred(F,cycPlus2(A2)),decl_mpred(F,cycPred(A)).


registerCycPredPlus2(P):-!,with_pi(P,registerCycPredPlus2_3).

% ========================================
% mpred_props database
% ========================================
:-swi_export(ttDeclarer/1).
ttDeclarer(Prop):- 
	arg(_,v(predArgTypes,vFormatted,tPred,
                prologMultiValued,prologSingleValued,prologMacroHead,prologOnly,
		prologOrdered,prologNegByFailure,prologPTTP,prologHybrid,prologListValued),Prop).

mpred_arity(Prop,1):-ttDeclarer(Prop).
mpred_arity(F,A):- current_predicate(F/A).
mpred_prop(H,PP):-compound(H),predicate_property(H,PP).
mpred_prop(F,PP):-mpred_arity(F,A),functor(H,F,A),predicate_property(H,PP).
mpred_prop(P,Prop):- ttDeclarer(Prop),hasInstance(Prop, P).
mpred_prop(F,Prop):- mpred_arity(F,A),functor(P,F,A),predicate_property(P,Prop).
mpred_prop(F,tCol):- tCol(F).
mpred_prop(H,PP):- nonvar(H),functor_h(H,F), H \=@= F, !,mpred_prop(F,PP).
mpred_prop(F,PP):- hasInstance(PP,F).
mpred_prop(mpred_prop,prologOnly).
mpred_prop(mpred_arity,prologOnly).
mpred_prop(never_type,prologOnly).
mpred_prop(mudSubclass, ttCompleteExtentAsserted).
mpred_prop(mudFtInfo, ttCompleteExtentAsserted).
/*
mpred_prop(G,predProxyAssert(add)):- atom(G),prologMacroHead(G).
mpred_prop(G,predProxyQuery(ireq)):- atom(G),prologMacroHead(G).
mpred_prop(G,predProxyRetract(del)):- atom(G),prologMacroHead(G).
mpred_prop(F,predStubType(Type)):-mpred_prop(F,hasStub(Type)).
*/

:-dynamic_multifile_exported(hasInstance/2).
% hasInstance(col,Prop):-mpred_arity(Prop,1).

:-forall(ttDeclarer(F),dynamic(F/1)).
:-forall(ttDeclarer(F),assert_hasInstance(macroDeclarer,F)).

get_mpred_prop(F,_A,P):-get_mpred_prop(F,P).
get_mpred_prop(F,P):- mpred_prop(F,P).


ensure_arity(F,A):- one_must(mpred_arity(F,A),one_must((current_predicate(F/A),assert_arity(F,A)),(ground(F:A),assert_arity(F,A)))),!.

assert_arity(F,A):-not(atom(F)),trace_or_throw(assert_arity(F,A)).
assert_arity(F,A):-not(integer(A)),trace_or_throw(assert_arity(F,A)).
assert_arity(F,A):-mpred_arity(F,A),assert_if_new(mpred_prop(F,predArity(A))),!.
assert_arity(F,A):-mpred_arity(F,1),dmsg(trace_or_throw(was_one_assert_arity(F,A))),!.
assert_arity(predArgTypes,2):-trace_or_throw(assert_arity_argsIsa(predArgTypes,2)).
assert_arity(ArgsIsa,0):-trace_or_throw(assert_arity(ArgsIsa,0)).
assert_arity(F,A):-loop_check_local(assert_arity_lc(F,A),true),!.
assert_arity(F,A):-asserta(mpred_arity(F,A)),!.
assert_arity(F,A):-dmsg(failed_assert_arity(F,A)).

assert_arity_lc(F,A):-
  % A2 is A+2,declare_dbase_local_dynamic_plus_2(F,A2),
  retractall(mpred_prop(F,predArity(_))),
  retractall(mpred_arity(F,_)),
   must_det(good_pred_relation_name(F,A)),
    hooked_asserta(mpred_arity(F,A)),
    hooked_asserta(mpred_prop(F,predArity(A))),!.



good_pred_relation_name(F,A):-not(bad_pred_relation_name(F,A)).

bad_pred_relation_name(V,_):-not(atom(V)),!.
bad_pred_relation_name('[]',_).
bad_pred_relation_name('[|]',_).
bad_pred_relation_name(F,A):-must_det((atom_codes(F,[C|_]),to_upper(C,U))),!, U == C, A>1.
bad_pred_relation_name(F,A):-mpred_arity(F,AO), A \= AO.

:-at_start(writeq("at start!~n")).

first_mpred_props(predArity(_)).
first_mpred_props(predArgTypes(_)).

mpred_prop_ordered(Pred,Prop):-first_mpred_props(Prop),mpred_prop(Pred,Prop),not(mpred_prop(Pred,prologOnly)).
mpred_prop_ordered(Pred,Prop):-mpred_prop(Pred,Prop),not(first_mpred_props(Prop)),not(mpred_prop(Pred,prologOnly)).

:- dynamic_multifile_exported((decl_mpred/1)).

decl_mpred((A,B)):-decl_mpred(A),decl_mpred(B).
decl_mpred(M):-loop_check_local(with_pi(M,decl_mpred_1),true).
decl_mpred_1(_,F,F/0):-!,assert_hasInstance(tPred,F).
decl_mpred_1(M,PI,F/A):-
   decl_mpred(F,A),
   ignore((ground(PI),compound(PI),decl_mpred(F,predArgTypes(PI)))),
   decl_mpred(F,[predModule(M)]).

:-dynamic_multifile_exported(decl_mpred/2).
decl_mpred(C,More):- ignore(loop_check(decl_mpred_0(C,More),true)).

decl_mpred_0(C,More):- (var(C);var(More)), trace_or_throw(var_decl_mpred(C,More)).
decl_mpred_0(F,tPred):-!, assert_hasInstance(tPred,F).
decl_mpred_0(_,[]):-!.
decl_mpred_0(M:FA,More):-atom(M),!,decl_mpred_0(FA,[predModule(M)|More]).
decl_mpred_0(F/A,More):-atom(F),!,decl_mpred_1(F,predArity(A)),decl_mpred(F,More),!.
decl_mpred_0(C,More):-string(C),!,dmsg(trace_or_throw(var_string_decl_mpred(C,More))).
decl_mpred_0(C,More):-compound(C),C=..[F,Arg1|PROPS],ttDeclarer(F),!,ground(Arg1),decl_mpred(Arg1,[F,PROPS,More]).
decl_mpred_0(C,More):-compound(C),!,functor(C,F,A),decl_mpred_1(F,predArity(A)),decl_mpred_0(F,More),!,ignore((ground(C),decl_mpred(F,predArgTypes(C)))),!.
decl_mpred_0(F,A):-number(A),!,decl_mpred_1(F,predArity(A)),!.
decl_mpred_0(F,[Prop|Types]):-!,decl_mpred_0(F,Prop),!,decl_mpred_0(F,Types),!.

decl_mpred_0(F,T):-doall(( decl_mpred_1(F,T) )).

decl_mpred_1(F,predArgTypes(FARGS)):- functor(FARGS,_,A),decl_mpred(F,A),fail.
decl_mpred_1(_,predArgTypes(FARGS)):- functor(FARGS,_,A),arg(A,FARGS,Arg),var(Arg),!.
decl_mpred_1(F,predArity(A)):- assert_arity(F,A),fail.

decl_mpred_1(F,prologHybrid):- declare_dbase_local_dynamic(F).
decl_mpred_1(F,cycPlus2(A)):- declare_dbase_local_dynamic_plus_2(F,A).

decl_mpred_1(F,Prop):-mpred_prop(F,Prop),!.
decl_mpred_1(F,Prop):-hooked_asserta(mpred_prop(F,Prop)),fail.

decl_mpred_1(F,A):-once(provide_mpred_write_attributes(F,A)).


decl_mpred(Mt,F,A):-decl_mpred(F,A),ignore((nonvar(Mt),decl_mpred(F,mt(Mt)))).
decl_mpred_1(_CM,M,PI,F/A):-
   decl_mpred_1(M,PI,F/A).


functor_check_univ(M:G1,F,List):-atom(M),member(M,[dbase,user]),!,functor_check_univ(G1,F,List),!.
functor_check_univ(G1,F,List):-must_det(compound(G1)),must_det(G1 \= _:_),must_det(G1 \= _/_),G1=..[F|List],!.

:-swi_export(glean_pred_props_maybe/1).
glean_pred_props_maybe(_:G):-!,compound(G),glean_pred_props_maybe(G).
glean_pred_props_maybe(G):-compound(G),G=..[F,Arg1|RGS],ttDeclarer(F),!,add_mpred_prop_gleaned(Arg1,[F|RGS]),!.

add_mpred_prop_gleaned(M:Arg1,FRGS):-atom(M),!,add_mpred_prop_gleaned(Arg1,FRGS).
add_mpred_prop_gleaned(Arg1,FRGS):-functor_check_univ(Arg1,F,ARGSISA),add_mpred_prop_gleaned_4(Arg1,F,ARGSISA,FRGS).
add_mpred_prop_gleaned_4(Arg1,_F,[ARG|_],FRGS):-nonvar(ARG),!,decl_mpred(Arg1,[predArgTypes(Arg1)|FRGS]).
add_mpred_prop_gleaned_4(Arg1,_F,_,FRGS):-decl_mpred(Arg1,FRGS).

user:term_expansion(G,_):- not(thlocal:into_form_code),notrace((once(glean_pred_props_maybe(G)),fail)).
