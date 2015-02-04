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

% ========================================================================================
% DEDUCE RETRACTIONS
% ========================================================================================

:-export((alt_forms/2)).
alt_forms(AR,M:P,NP):- atom(M),!,alt_forms(AR,P,NP).
alt_forms(AR,P,NP):- nonvar(P),into_mpred_form(P,CP),P\=@=CP,!,alt_forms(AR,CP,NP).
alt_forms(AR,P,NP):- no_repeats(( alt_forms0(AR,P,NP), NP\=@=P)).

alt_forms0(AR,P,NP):-alt_forms1(AR,P,M),alt_forms1(AR,M,N),alt_forms1(AR,N,NP).
alt_forms0(AR,P,NP):-alt_forms1(AR,P,M),alt_forms1(AR,M,NP).
alt_forms0(AR,P,NP):-alt_forms1(AR,P,NP).

alt_forms1(none_AR,mudAtLoc(P,L),localityOfObject(P,R)):-ground(mudAtLoc(P,L)),once(locationToRegion(L,R)),nonvar(R).
alt_forms1(none_AR,localityOfObject(P,R),mudAtLoc(P,L)):-ground(localityOfObject(P,R)),is_asserted(mudAtLoc(P,L)),nonvar(L),once(locationToRegion(L,R)).
alt_forms1(AR,P,NP):-compound(P),P=..[F,A,B|R],alt_forms2(AR,F,A,B,R,NP). 

%alt_forms2(r,F,A,B,R,NP):-dbase_t(genlInverse,F,FF),NP=..[FF,B,A|R].
%alt_forms2(r,F,A,B,R,NP):-dbase_t(genlInverse,FF,F),NP=..[FF,B,A|R].
% alt_forms2(r,F,A,B,R,NP):-genlPreds(F,FF),NP=..[FF,A,B|R].
alt_forms2(r,F,A,B,R,NP):-genlPreds(FF,F),NP=..[FF,A,B|R].

user:decl_database_hook(retract(Kind),P):- forall(alt_forms(r,P,NP),ignore(hooked_op(retract(Kind),NP))).




% ========================================================================================
% DEDUCE FACTS
% ========================================================================================


user:decl_database_hook(Type,Fact):- current_predicate(add_deduction/3),run_deduce_facts_from(Type,Fact).


user:decl_database_hook(_,mpred_prop('ArtifactCol1008-VISOR688', flagged_visor)):- trace_or_throw(mpred_prop('ArtifactCol1008-VISOR688', flagged_visor)).

run_deduce_facts_from(Type,M:Fact):-atom(M),!,run_deduce_facts_from(Type,Fact).
run_deduce_facts_from(Type,Fact):-loop_check_local(run_deduce_facts_from_lc(Type,Fact),true).
run_deduce_facts_from_lc(Type,Fact):-doall((call_no_cuts(deduce_facts(Fact,Deduction)),add_deduction(Type,Deduction,Fact))).


user:decl_database_hook(assert(_),BadFact):-bad_fact_why(BadFact,WHY),trace_or_throw(bad_fact_why(BadFact,WHY)).

bad_fact_why(mudAtLoc(iArea1025, _),mudIsa(iArea1025,tRegion)).
bad_fact_why(localityOfObject(iArea1025, iOfficeRoom7),mudIsa(iArea1025,tRegion)).
bad_fact_why(localityOfObject(R,_),mudIsa(R,tRegion)):- mudIsa(R,tRegion).
bad_fact_why(mudFacing(R,_),mudIsa(R,tRegion)):- mudIsa(R,tRegion).
bad_fact_why(mudAtLoc(R,_),mudIsa(R,tRegion)):- mudIsa(R,tRegion).

%deduce_facts(localityOfObject(_,Region),mudIsa(Region,tSpatialThing)).
deduce_facts(localityOfObject(Obj,_),mudIsa(Obj,tObj)).

deduce_facts(Fact,mpred_prop(AF,[predArgTypes(ArgTs)|PROPS])):-compound(Fact),Fact=..[F,ArgTs|PROPS],is_pred_declarer(F),compound(ArgTs),functor(ArgTs,AF,N),N>0,
                ArgTs=..[AF|ARGS],!,must_det(ground(ARGS)).


deduce_facts(predArgTypes(ArgTs),mpred_prop(F,predArgTypes(ArgTs))):-mpred_arity(F,A),functor(ArgTs,F,A).
deduce_facts(mpred_prop(F,predArgTypes(ArgTs)),predArgTypes(ArgTs)):-mpred_arity(F,A),functor(ArgTs,F,A).


deduce_facts(predArgTypes(ArgTs),argIsa(F,A,Type)):-get_functor(ArgTs,F,_),arg(A,ArgTs,Type).
deduce_facts(mpred_prop(F,predArgTypes(ArgTs)),argIsa(F,A,Type)):-arg(A,ArgTs,Type).

deduce_facts(argIsa(F,_A,Type),[mudIsa(Type,tCol),mudIsa(F,tRelation)]):-atom(Type),not(hasInstance(ttFormatType,Type)).

%deduce_facts(B,A):- is_asserted(ruleEquiv(B,A)),not(contains_singletons(A)).
%deduce_facts(B,A):- is_asserted(ruleEquiv(A,B)),not(contains_singletons(A)).
deduce_facts(Term,NewTerm):- current_predicate(good_for_chaining/2),
  hotrace(good_for_chaining(Op,Term)), db_rewrite(Op,Term,NewTerm),not(contains_singletons(NewTerm)).


fix_argIsa(F,N,vtDirection(Val),vtDirection):-add(mpred_prop(F,argSingleValueDefault(N,Val))),!.
fix_argIsa(F,N,ftInt(Val),ftInt):-add(mpred_prop(F,argSingleValueDefault(N,Val))),!.
fix_argIsa(_,_,ftListFn(Type),ftListFn(Type)):-!.
fix_argIsa(_,_,vFormatted(Type),vFormatted(Type)):-!.
fix_argIsa(_,_,Arg,Arg).
fix_argIsa(F,N,Type,F):-compound(Type),Type=..[F,Val],isa_backchaing(Val,F),decl_mpred(F,argSingleValueDefault(N,Val)),!.

fix_argsIsas(_,_,[],[]):-!.
fix_argsIsas(F,N,[Arg|TList],[G|List]):-
   fix_argIsa(F,N,Arg,G),!, N1 is N + 1,fix_argsIsas(F,N1,TList,List),!.

user:decl_database_hook(assert(_),predArgTypes(ArgTs)):-
   ArgTs=..[F|ArgTList],
   fix_argsIsas(F,1,ArgTList,GList),
   Good=..[F|GList],
   Good\=ArgTs,!,del(mpred_prop(F,predArgTypes(ArgTs))),decl_mpred(F,predArgTypes(Good)).

:-dynamic_multifile_exported(add_deduction/3).
quiet_fact(Fact):-functor(Fact,F,A),quiet_fact(F,A).
quiet_fact(mudIsa,_).
quiet_fact(argIsa,_).
quiet_fact(mpred_prop,_).

add_deduction(_Type,[],_How):-!.
add_deduction(Type,[Fact|S],How):-!,add_deduction(Type,Fact,How),add_deduction(Type,S,How),!.
add_deduction(Type,Fact,How):- ignore(loop_check_term(add_deduction_lc(Type,Fact,How),add_deduction_lc(Type,Fact),true)),!.

add_deduction_lc(Type,Fact,_How):-quiet_fact(Fact),!,do_deduction_type(Type,Fact),!.
add_deduction_lc(Type,Fact,How):-dmsg(add_deduction(Type,Fact,'_________from________',How)),do_deduction_type(Type,Fact),!.

do_deduction_type(assert(_),Fact):-add(Fact).
do_deduction_type(retract(_),Fact):-functor(Fact,F,A),(F=mudIsa;F=mpred_prop;A=1),!.
do_deduction_type(retract(_),Fact):-clr(Fact).

/*
a :- b. % a if b
a :- b,c. % a if b and c.
a :- b;c. % a if b or c.
a :- \++ b. % a if b is not provable
a :- \++ b. % a if b is not provable
a :- not b. % a if b fails
a :- b -> c;d. % a if (if b then c else d)
*/
