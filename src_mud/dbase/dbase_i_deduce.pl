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
alt_forms(P,NP):-into_mpred_form(P,CP),P\=@=CP,!,alt_forms(CP,NP).
alt_forms(P,NP):- no_repeats(( alt_forms0(P,NP), NP\=@=P)).

alt_forms0(P,NP):-alt_forms1(P,M),alt_forms1(M,N),alt_forms1(N,NP).
alt_forms0(P,NP):-alt_forms1(P,M),alt_forms1(M,NP).
alt_forms0(P,NP):-alt_forms1(P,NP).


alt_forms1(atloc(P,L),localityOfObject(P,R)):-ground(atloc(P,L)),once(locationToRegion(L,R)),nonvar(R).
alt_forms1(localityOfObject(P,R),atloc(P,L)):-ground(localityOfObject(P,R)),is_asserted(atloc(P,L)),nonvar(L),once(locationToRegion(L,R)).
alt_forms1(P,NP):-P=..[F,A,B|R],alt_forms2(F,A,B,R,NP). 

alt_forms2(F,A,B,R,NP):-dbase_t(genlInverse,F,FF),NP=..[FF,B,A|R].
alt_forms2(F,A,B,R,NP):-dbase_t(genlInverse,FF,F),NP=..[FF,B,A|R].
alt_forms2(F,A,B,R,NP):-dbase_t(genlPreds,F,FF),NP=..[FF,A,B|R].
alt_forms2(F,A,B,R,NP):-dbase_t(genlPreds,FF,F),NP=..[FF,A,B|R].

decl_database_hook(retract(Kind),P):- forall(alt_forms(P,NP),ignore(hooked_op(retract(Kind),NP))).


% ========================================================================================
% DEDUCE FACTS
% ========================================================================================


decl_database_hook(Type,Fact):- predicate_property(add_deduction(_,_),_),run_deduce_facts_from(Type,Fact).


decl_database_hook(_,mpred_prop('ArtifactCol1008-VISOR688', flagged_visor)):- trace_or_throw(mpred_prop('ArtifactCol1008-VISOR688', flagged_visor)).

run_deduce_facts_from(Type,M:Fact):-atom(M),!,run_deduce_facts_from(Type,Fact).
run_deduce_facts_from(Type,Fact):-loop_check_local(run_deduce_facts_from_lc(Type,Fact),true).
run_deduce_facts_from_lc(Type,Fact):-doall((call_no_cuts(deduce_facts(Fact,Deduction)),add_deduction(Type,Deduction,Fact))).


decl_database_hook(assert(_),atloc(R,W)):- isa(R,region),trace_or_throw(atloc(R,W)).


deduce_facts(localityOfObject(_,Region),isa(Region,spatialthing)).
deduce_facts(localityOfObject(Obj,_),isa(Obj,obj)).

deduce_facts(Fact,mpred_prop(AF,[argsIsaInList(ArgTs)|PROPS])):-compound(Fact),Fact=..[F,ArgTs|PROPS],argsIsaProps(F),compound(ArgTs),functor(ArgTs,AF,N),N>0,
                ArgTs=..[AF|ARGS],!,must_det(ground(ARGS)).


deduce_facts(argsIsaInList(ArgTs),mpred_prop(F,argsIsaInList(ArgTs))):-mpred_arity(F,A),functor(ArgTs,F,A).
deduce_facts(mpred_prop(F,argsIsaInList(ArgTs)),argsIsaInList(ArgTs)):-mpred_arity(F,A),functor(ArgTs,F,A).


deduce_facts(argsIsaInList(ArgTs),argIsa(F,A,Type)):-ztrace,functor(ArgTs,F,_),arg(A,ArgTs,Type).
deduce_facts(mpred_prop(F,argsIsaInList(ArgTs)),argIsa(F,A,Type)):-arg(A,ArgTs,Type).

deduce_facts(argIsa(F,_A,Type),[isa(Type,col),isa(F,relation)]):-atom(Type),not(hasInstance(formattype,Type)).

%deduce_facts(B,A):- is_asserted(equivRule(B,A)),not(contains_singletons(A)).
%deduce_facts(B,A):- is_asserted(equivRule(A,B)),not(contains_singletons(A)).
deduce_facts(Term,NewTerm):- hotrace(good_for_chaining(Op,Term)), db_rewrite(Op,Term,NewTerm),not(contains_singletons(NewTerm)).


fix_argIsa(F,N,dir(Val),dir):-add(mpred_prop(F,default_sv(N,Val))),!.
fix_argIsa(F,N,int(Val),int):-add(mpred_prop(F,default_sv(N,Val))),!.
fix_argIsa(_,_,list(Type),list(Type)):-!.
fix_argIsa(_,_,formatted(Type),formatted(Type)):-!.
fix_argIsa(_,_,Arg,Arg).
fix_argIsa(F,N,Type,F):-compound(Type),Type=..[F,Val],isa_backchaing(Val,F),decl_mpred(F,default_sv(N,Val)),!.

fix_argsIsas(_,_,[],[]):-!.
fix_argsIsas(F,N,[Arg|TList],[G|List]):-
   fix_argIsa(F,N,Arg,G),!, N1 is N + 1,fix_argsIsas(F,N1,TList,List),!.

decl_database_hook(assert(_),argsIsaInList(ArgTs)):-
   ArgTs=..[F|ArgTList],
   fix_argsIsas(F,1,ArgTList,GList),
   Good=..[F|GList],
   Good\=ArgTs,!,del(mpred_prop(F,argsIsaInList(ArgTs))),decl_mpred(F,argsIsaInList(Good)).

:-swi_export(add_deduction/3).
quiet_fact(Fact):-functor(Fact,F,A),quiet_fact(F,A).
quiet_fact(isa,_).
quiet_fact(argIsa,_).
quiet_fact(mpred_prop,_).

add_deduction(_Type,[],_How):-!.
add_deduction(Type,[Fact|S],How):-!,add_deduction(Type,Fact,How),add_deduction(Type,S,How),!.
add_deduction(Type,Fact,How):- ignore(loop_check_term(add_deduction_lc(Type,Fact,How),add_deduction_lc(Type,Fact),true)),!.

add_deduction_lc(Type,Fact,_How):-quiet_fact(Fact),!,do_deduction_type(Type,Fact),!.
add_deduction_lc(Type,Fact,How):-dmsg(add_deduction(Type,Fact,'_________from________',How)),do_deduction_type(Type,Fact),!.

do_deduction_type(assert(_),Fact):-add(Fact).
do_deduction_type(retract(_),Fact):-functor(Fact,F,A),(F=isa;F=mpred_prop;A=1),!.
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

