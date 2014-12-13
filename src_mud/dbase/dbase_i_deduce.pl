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

deduce_facts(argIsa(F,_A,Type),[isa(Type,type),isa(F,relation)]):-atom(Type),not(dbase_t(Type,formattype)).

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



:-dynamic_multifile_exported(is_known_trew/1).
:-dynamic_multifile_exported(is_known_true/1).

is_known_true(C):-has_free_args(C),!,trace_or_throw(has_free_args(is_known_trew,C)).
is_known_true(F):-is_known_false0(F),!,fail.
is_known_true(isa(X,spatialthing)):- is_asserted(isa(X,_)),is_known_false0(isa(X,type)),is_known_false0(isa(X,formattype)),is_known_false0(isa(X,mpred)).

is_known_trew(isa(container,completeExtentAsserted)).
% is_known_trew(isa(formattype,metaclass)).
is_known_trew(isa(completeExtentAsserted,completeExtentAsserted)).
is_known_trew(isa(type,completeExtentAsserted)).
is_known_trew(isa(gossup,channel)).
is_known_trew(subclass(region,channel)).
is_known_trew(subclass(agent,channel)).
is_known_trew(isa(agent,createableType)).
is_known_trew(isa(region,createableType)).
is_known_trew(isa(singleValued, completeExtentAsserted)).
is_known_trew(isa(createableType,completeExtentAsserted)).
is_known_trew(isa(formattype,completeExtentAsserted)).
is_known_trew(isa(int,nonCreatableType)).
is_known_trew(isa(type,type)).
is_known_trew(isa(singleValued, type)).
is_known_trew(isa(completeExtentAsserted, type)).
is_known_trew(subclass(completeExtentAsserted, extentDecidable)).
is_known_trew(subclass(singleValued, extentDecidable)).
is_known_trew(subclass('MaleAnimal',agent)).
is_known_trew(subclass(X,X)).
is_known_trew(subclass(formattype,type)).
is_known_trew(isa(type,nonCreatableType)).
is_known_trew(isa(item,createableType)).
is_known_trew(subclass(item,createableType)).
is_known_trew(isa(formattype,nonCreatableType)).
is_known_trew(subclass(formattype,nonCreatableType)).
is_known_trew(isa('TemporallyExistingThing', 'createableType')).
is_known_trew(isa(term,nonCreatableType)).
is_known_trew(subclass(argsIsaInList,relation)).
is_known_trew(subclass(fpred,relation)).
is_known_trew(subclass(F,mpred)):-argsIsaProps(F).
is_known_trew(subclass(F,fpred)):-argsIsaProps(F).
is_known_trew(subclass(F,relation)):-argsIsaProps(F).
is_known_trew(disjointWith(A,B)):-disjointWithT(A,B).

is_known_trew(isa(apath(_,_),areaPath)).
is_known_trew(isa(apath(_,_),apath)).
is_known_trew(isa(_,id)).
is_known_trew(isa(_,term)).


:-dynamic_multifile_exported(is_known_false/1).
% :-dynamic(is_known_false/1).
is_known_false(C):-has_free_args(C),!,fail.
is_known_false(F):-is_known_trew(F),!,fail.
is_known_false(F):-is_known_false0(F),!.

:-dynamic_multifile_exported(is_known_false0/1).
is_known_false0(isa(regiontype,formattype)).
is_known_false0(isa(formattype,formattype)).
is_known_false0(isa(X,spatialthing)):- type(X);formattype(X);mpred(X).
is_known_false0(isa(completeExtentAsserted,createableType)).
is_known_false0(isa(X,Y)):-!,not_mud_isa(X,Y).
is_known_false0(subclass(Type,_)):-arg(_,vv(type,relation,formattype),Type).

:-dynamic_multifile_exported(not_mud_isa/2).
not_mud_isa(agent,formattype).
not_mud_isa(item,formattype).
not_mud_isa(type,formattype).
not_mud_isa(obj, completeExtentAsserted).
not_mud_isa(obj, createableType).
not_mud_isa(assertionMacroHead, formattype).
not_mud_isa(obj, formattype).
not_mud_isa(formattype,formattype).
not_mud_isa(subft,type).
not_mud_isa('TemporallyExistingThing', 'TemporallyExistingThing').
not_mud_isa(createableType,'TemporallyExistingThing').
not_mud_isa(Type,formattype):- \+ (dbase_t(formattype, Type)).
not_mud_isa(Type, assertionMacroHead):- \+ (mpred_prop(Type, assertionMacroHead)).
not_mud_isa(Type, completeExtentAsserted):- \+ (mpred_prop(Type, completeExtentAsserted)).
not_mud_isa(X,type):-never_type(X).

