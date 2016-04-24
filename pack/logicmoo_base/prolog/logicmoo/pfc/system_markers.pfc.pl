
:- file_begin(pfc).

:- set_mpred_module(baseKB).

% catching of misinterpreations
(mpred_mark(pfcPosTrigger,F,A)/(fa_to_p(F,A,P), P\={_}, predicate_property(P,static))) ==> {break,trace_or_throw(warn(pfcPosTrigger,P,static))}.
(mpred_mark(pfcNegTrigger,F,A)/(fa_to_p(F,A,P),  P\={_}, predicate_property(P,static))) ==> {dmsg(warn(pfcNegTrigger,P,static))}.
(mpred_mark(pfcBcTrigger,F,A)/(fa_to_p(F,A,P), predicate_property(P,static))) ==> {dmsg(warn(pfcNegTrigger,P,static))}.


%(mpred_mark(pfcPosTrigger,F,A)/(fa_to_p(F,A,P), \+ predicate_property(P,_))) ==> {kb_dynamic(tbox:F/A)}.
%(mpred_mark(pfcNegTrigger,F,A)/(fa_to_p(F,A,P), \+ predicate_property(P,_))) ==> {kb_dynamic(tbox:F/A)}.

:- dynamic(marker_supported/2).
:- dynamic(mpred_mark/3).

:- dynamic(mpred_mark_C/1).
:- dynamic(hybrid_support/2).

mpred_mark(S1, F, A)/(ground(S1),is_ftNameArity(F,A))==>(tSet(S1),arity(F,A),isa(F,S1)).
mpred_mark(pfcPosTrigger,F, A)/(is_ftNameArity(F,A))==>marker_supported(F,A).
mpred_mark(pfcNegTrigger,F, A)/(is_ftNameArity(F,A))==>marker_supported(F,A).
mpred_mark(pfcBcTrigger,F, A)/(is_ftNameArity(F,A))==>marker_supported(F,A).
mpred_mark(pfcRHS,F, A)/(is_ftNameArity(F,A))==>marker_supported(F,A).
mpred_mark(pfcCreates,F, A)/(is_ftNameArity(F,A))==>{functor(P,F,A),make_dynamic(P)}.
mpred_mark(pfcCreates,F, A)/(is_ftNameArity(F,A))==>marker_supported(F,A).
mpred_mark(pfcCallCode,F, A)/((is_ftNameArity(F,A)), 
  predicate_is_undefined_fa(F,A))==> marker_supported(F,A).


marker_supported(F,A)/is_ftNameArity(F,A)==>hybrid_support(F,A).


%mpred_mark(pfcPosTrigger,F,A)/(integer(A),functor(P,F,A)) ==> pfcTriggered(F/A),afterAdding(F,lambda(P,mpred_enqueue(P,(m,m)))).
%mpred_mark(pfcNegTrigger,F,A)/(integer(A),functor(P,F,A)) ==> pfcTriggered(F/A), afterRemoving(F,lambda(P,mpred_enqueue(~P,(m,m)))).

/*
mpred_mark(pfcRHSF,1)/(fail,atom(F),functor(Head,F,1), 
 \+ argsQuoted(F),
 \+ prologDynamic(F),
 \+ ~(tCol(F)),
 \+ specialFunctor(F),
 \+ predicate_property(Head,built_in))==>completelyAssertedCollection(F).
*/
% mpred_mark(Type,F,A)/(integer(A),A>1,F\==arity,Assert=..[Type,F])==>arity(F,A),Assert.

mpred_mark_C(G) ==> {map_mpred_mark_C(G)}.
map_mpred_mark_C(G) :-  map_literals(lambda(P,(get_functor(P,F,A),ain([isa(F,pfcControlled),arity(F,A)]))),G).
mpred_mark(pfcRHS,F,A)/(is_ftNameArity(F,A),F\==arity)==>tPred(F),arity(F,A),pfcControlled(F).




%:- meta_predicate(mp_test_agr(?,+,-,*,^,:,0,1,5,9)).
%mp_test_agr(_,_,_,_,_,_,_,_,_,_).
%:- mpred_test(predicate_property(mp_test_agr(_,_,_,_,_,_,_,_,_,_),meta_predicate(_))).
% becomes         mp_test_agr(+,+,-,?,^,:,0,1,0,0)


