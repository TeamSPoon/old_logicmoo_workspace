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

end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
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


alt_forms1(AR,P,NP):-compound(P),P=..[F,A,B|R],alt_forms2(AR,F,A,B,R,NP). 

%alt_forms2(r,F,A,B,R,NP):-dbase_t(genlInverse,F,FF),NP=..[FF,B,A|R].
%alt_forms2(r,F,A,B,R,NP):-dbase_t(genlInverse,FF,F),NP=..[FF,B,A|R].
% alt_forms2(r,F,A,B,R,NP):-genlPreds(F,FF),NP=..[FF,A,B|R].
alt_forms2(r,F,A,B,R,NP):-genlPreds(FF,F),NP=..[FF,A,B|R].


%OLD user:decl_database_hook(change( retract,Kind),P):- forall(alt_forms(r,P,NP),ignore(dbase_op(change( retract,Kind),NP))).




% ========================================================================================
% DEDUCE FACTS
% ========================================================================================


%OLD user:decl_database_hook(Type,Fact):- current_predicate(add_deduction/3),current_predicate(add/1),run_deduce_facts_from(Type,Fact).


%OLD user:decl_database_hook(_,user:mpred_prop('ArtifactCol1008-VISOR688', flagged_visor)):- trace_or_throw(user:mpred_prop('ArtifactCol1008-VISOR688',N, flagged_visor)).

run_deduce_facts_from(Type,M:Fact):-atom(M),!,run_deduce_facts_from(Type,Fact).
run_deduce_facts_from(Type,Fact):-loop_check_local(run_deduce_facts_from_ilc(Type,Fact),true).
run_deduce_facts_from_ilc(Type,Fact):-doall((call_no_cuts(deduce_facts_forward(Fact,Deduction)),add_deduction(Type,Deduction,Fact))).


%OLD user:decl_database_hook(change(assert,_),BadFact):-mpred_call(tms_reject_why(BadFact,WHY)),trace_or_throw(tms_reject_why(BadFact,WHY)).

deduceFromArgTypes(_).

deduce_facts_forward(Fact,user:mpred_prop(AF,[predArgTypes(ArgTs)|PROPS])):-compound(Fact),Fact=..[F,ArgTs|PROPS],is_pred_declarer(F),compound(ArgTs),functor(ArgTs,AF,N),N>0,
                ArgTs=..[AF|ARGS],!,sanity(ground(ARGS)).

deduce_facts_forward(predArgTypes(ArgTs),user:mpred_prop(F,predArgTypes(ArgTs))):-get_functor(ArgTs,F),assert_predArgTypes_fa(F,ArgTs).
deduce_facts_forward(user:mpred_prop(F,predArgTypes(ArgTs)),predArgTypes(ArgTs)):-get_functor(ArgTs,F),assert_predArgTypes_fa(F,ArgTs).

deduce_facts_forward(predArgTypes(ArgTs),argIsa(F,A,Type)):-get_functor(ArgTs,F,_),arg(A,ArgTs,Type).
deduce_facts_forward(user:mpred_prop(F,predArgTypes(ArgTs)),argIsa(F,A,Type)):-arg(A,ArgTs,Type).

deduce_facts_forward(argIsa(F,_A,Type),[isa(Type,tCol),isa(F,tRelation)]):-atom(Type),not(hasInstance(ttFormatType,Type)).

%deduce_facts_forward(B,A):- is_asserted('<=>'(B,A)),not(contains_singletons(A)).
%deduce_facts_forward(B,A):- is_asserted('<=>'(A,B)),not(contains_singletons(A)).
deduce_facts_forward(Term,NewTerm):- current_predicate(good_for_chaining/2),
  hotrace(good_for_chaining(Op,Term)), db_rewrite(Op,Term,NewTerm),not(contains_singletons(NewTerm)).

fix_argIsa(F,N,ftInt(Val),ftInt):-add(user:mpred_prop(F,argSingleValueDefault(N,Val))),!.
fix_argIsa(_,_,ftListFn(Type),ftListFn(Type)):-!.
fix_argIsa(_,_,ftFormFn(Type),ftFormFn(Type)):-!.
fix_argIsa(_,_,Arg,Arg).
fix_argIsa(F,N,Type,F):-compound(Type),Type=..[F,Val],isa_backchaing(Val,F),decl_mpred(F,argSingleValueDefault(N,Val)),!.

fix_argsIsas(_,_,[],[]):-!.
fix_argsIsas(F,N,[Arg|TList],[G|List]):-
   fix_argIsa(F,N,Arg,G),!, N1 is N + 1,fix_argsIsas(F,N1,TList,List),!.

%OLD user:decl_database_hook(change(assert,_),predArgTypes(ArgTs)):-
/*   ArgTs=..[F|ArgTList],
   fix_argsIsas(F,1,ArgTList,GList),
   Good=..[F|GList],
   Good\=ArgTs,!,del(user:mpred_prop(F,predArgTypes(ArgTs))),decl_mpred(F,predArgTypes(Good)).
*/

%OLD user:decl_database_hook(change(assert,_),isa(ArgTs,PredArgTypes)):- predArgTypes==PredArgTypes,
/*   ArgTs=..[F|ArgTList],
   fix_argsIsas(F,1,ArgTList,GList),
   Good=..[F|GList],
   Good\=ArgTs,!,del(user:mpred_prop(F,predArgTypes(ArgTs))),decl_mpred(F,predArgTypes(Good)).
*/

:-export(add_deduction/3).
quiet_fact(Fact):-functor(Fact,F,A),quiet_fact(F,A).
quiet_fact(isa,_).
quiet_fact(argIsa,_).
quiet_fact(user:mpred_prop,_).

add_deduction(_Type,[],_How):-!.
add_deduction(Type,[Fact|S],How):-!,add_deduction(Type,Fact,How),add_deduction(Type,S,How),!.
add_deduction(Type,Fact,How):- ignore(loop_check_term(add_deduction_ilc(Type,Fact,How),add_deduction_ilc(Type,Fact),true)),!.

add_deduction_ilc(Type,Fact,_How):-quiet_fact(Fact),!,do_deduction_type(Type,Fact),!.
add_deduction_ilc(Type,Fact,How):-dmsg(add_deduction(Type,Fact,'_________from________',How)),do_deduction_type(Type,Fact),!.

do_deduction_type(change(assert,_),Fact):-add(Fact).
do_deduction_type(change( retract,_),Fact):-functor(Fact,F,A),(F=isa;F=user:mpred_prop;A=1),!.
do_deduction_type(change( retract,_),Fact):-clr(Fact).




learnArgIsa(P,N,_):-argIsa_known(P,N,_),!.
learnArgIsa(P,N,T):-dmsg((skipping(learnArgIsa(P,N,T)))),!.
learnArgIsa(P,N,T):-grtrace, assert_argIsa(P,N,T).

learnArgIsaInst(K,Num,Arg):-integer(Arg),!,learnArgIsa(K,Num,ftInt).
learnArgIsaInst(K,Num,Arg):-number(Arg),!,learnArgIsa(K,Num,ftNumber).
learnArgIsaInst(_,_,_).



deduce_facts(Fact,isa(Arg,Type)):- test_tl(agenda_slow_op_do_prereqs),agenda_slow_op_enqueue(deduce_argIsa_facts(Fact,Arg,Type)).

nonusefull_deduction_type(ftTerm).
nonusefull_deduction_type(ftVoprop).
nonusefull_deduction_type(vtDirection).
nonusefull_deduction_type(Type):-ttSpatialType(Type),!,fail.
nonusefull_deduction_type(tObj).
nonusefull_deduction_type(Type):-is_asserted(ttFormatType(Type)).

assert_deduced_arg_isa_facts(Fact):- !, ignore(((ground(Fact),forall(deduce_argIsa_facts(Fact,Arg,Type),show_call(add(isa(Arg,Type))))))).

assert_deduced_arg_isa_facts(Fact):- agenda_slow_op_enqueue(assert_deduced_arg_isa_facts_0(Fact)),!.
assert_deduced_arg_isa_facts_0(Fact):- ignore(((ground(Fact),forall(deduce_argIsa_facts(Fact,Arg,Type),show_call(add(isa(Arg,Type))))))).


deduce_argIsa_facts(Fact,Arg,Type):- ground(Fact), functor(Fact,F,A),A>1, deduce_from_predicate(F), arg(N,Fact,Arg),ground(Arg),
   call_argIsa_ForAssert(F,N,Type),sanity(atom(Type)),sanity(ground(Arg)).

never_deduce_from_predicate(isa).
never_deduce_from_predicate(user:mpred_prop).
never_deduce_from_predicate(arity).
never_deduce_from_predicate(genls).
never_deduce_from_predicate(typeProps).
never_deduce_from_predicate(P):-arity(P,1).
never_deduce_from_predicate(P):-user:mpred_prop(P,ftCallable).
never_deduce_from_predicate(P):-argIsa_known(P,_,tCol).
never_deduce_from_predicate(P):-argIsa_known(P,_,ftVoprop).

deduce_from_predicate(Never):-never_deduce_from_predicate(Never),!,fail.
deduce_from_predicate(P):-user:mpred_prop(P,_).
deduce_from_predicate(_).


/*
a :- b. % a if b
a :- b,c. % a if b and c.
a :- b;c. % a if b or c.
a :- \++ b. % a if b is not provable
a :- \++ b. % a if b is not provable
a :- not b. % a if b fails
a :- b -> c;d. % a if (if b then c else d)
*/
