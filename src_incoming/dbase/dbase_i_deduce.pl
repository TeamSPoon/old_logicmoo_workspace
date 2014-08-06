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

hook:decl_database_hook(Type,Fact):- predicate_property(add_deduction(_,_),_),run_deduce_facts_from(Type,Fact).

run_deduce_facts_from(Type,M:Fact):-atom(M),!,run_deduce_facts_from(Type,Fact).
run_deduce_facts_from(Type,Fact):-loop_check(run_deduce_facts_from_lc(Type,Fact),true).
run_deduce_facts_from_lc(Type,Fact):-doall((call_no_cuts(hook:deduce_facts(Fact,Deduction)),add_deduction(Type,Deduction,Fact))).


hook:decl_database_hook(assert(_),atloc(R,W)):- isa(R,region),trace_or_throw(atloc(R,W)).


hook:deduce_facts(atloc(Obj,LOC),inRegion(Obj,Region)):-locationToRegion(LOC,Region).
hook:deduce_facts(inRegion(Obj,_),atloc(Obj,LOC)):- put_in_world(Obj),must_det(atloc(Obj,LOC)).
hook:deduce_facts(inRegion(_,Region),isa(Region,region)).
hook:deduce_facts(inRegion(Obj,_),isa(Obj,obj)).

hook:deduce_facts(Fact,mpred_prop(AF,[argsIsa(ArgTs)|PROPS])):-compound(Fact),Fact=..[F,ArgTs|PROPS],argsIsaProps(F),compound(ArgTs),functor(ArgTs,AF,N),N>0,
                ArgTs=..[AF|ARGS],!,must_det(ground(ARGS)).

hook:deduce_facts(mpred_prop(F,argsIsa(ArgTs)),argsIsa(ArgTs)):-functor(F,ArgTs,_).

hook:deduce_facts(argsIsa(ArgTs),argIsa(F,A,Type)):-ztrace,functor(ArgTs,F,_),arg(A,ArgTs,Type).
hook:deduce_facts(mpred_prop(F,argsIsa(ArgTs)),argIsa(F,A,Type)):-arg(A,ArgTs,Type).

hook:deduce_facts(argIsa(F,_A,Type),[isa(Type,type),isa(F,relation)]):-atom(Type),not(dbase_t(Type,formattype)).

%hook:deduce_facts(B,A):- is_asserted(equivRule(B,A)),not(contains_singletons(A)).
%hook:deduce_facts(B,A):- is_asserted(equivRule(A,B)),not(contains_singletons(A)).
hook:deduce_facts(Term,NewTerm):- hotrace(good_for_chaining(Op,Term)), db_rewrite(Op,Term,NewTerm),not(contains_singletons(NewTerm)).


fix_argIsa(F,N,dir(Val),dir):-add(mpred_prop(F,default_sv(N,Val))),!.
fix_argIsa(F,N,int(Val),int):-add(mpred_prop(F,default_sv(N,Val))),!.
fix_argIsa(_,_,list(Type),list(Type)):-!.
fix_argIsa(F,N,Type,F):-compound(Type),Type=..[F,Val],get_isa_backchaing(Val,F),decl_mpred(F,default_sv(N,Val)),!.
fix_argIsa(_,_,Arg,Arg).

fix_argsIsas(_,_,[],[]):-!.
fix_argsIsas(F,N,[Arg|TList],[G|List]):-
   fix_argIsa(F,N,Arg,G), N1 is N + 1,fix_argsIsas(F,N1,TList,List),!.

hook:decl_database_hook(assert(_),argsIsa(ArgTs)):-
   ArgTs=..[F|ArgTList],
   fix_argsIsas(F,1,ArgTList,GList),
   Good=..[F|GList],
   Good\=ArgTs,!,del(mpred_prop(F,argsIsa(ArgTs))),decl_mpred(F,argsIsa(Good)).

:-export(add_deduction/3).
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
do_deduction_type(retract(_),Fact):-clr(Fact).



