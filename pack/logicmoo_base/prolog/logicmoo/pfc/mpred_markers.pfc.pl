
:- file_begin(pfc).

% catching of misinterpreations
(mpred_mark(pfcPosTrigger,_,F,A)/(fa_to_p(F,A,P), predicate_property(P,static))) ==> {trace_or_throw(warn(pfcPosTrigger,P,static))}.
(mpred_mark(pfcNegTrigger,_,F,A)/(fa_to_p(F,A,P), predicate_property(P,static))) ==> {dmsg(warn(pfcNegTrigger,P,static))}.
(mpred_mark(pfcBcTrigger,_,F,A)/(fa_to_p(F,A,P), predicate_property(P,static))) ==> {dmsg(warn(pfcNegTrigger,P,static))}.


%(mpred_mark(pfcPosTrigger,_,F,A)/(fa_to_p(F,A,P), \+ predicate_property(P,_))) ==> {kb_dynamic(tbox:F/A)}.
%(mpred_mark(pfcNegTrigger,_,F,A)/(fa_to_p(F,A,P), \+ predicate_property(P,_))) ==> {kb_dynamic(tbox:F/A)}.


mpred_mark(S1, S2, F, A)/ground(S1:S2)==>arity(F,A).
mpred_mark(pfcPosTrigger, S1, F, A)/ground(S1)==>marker_supported(F,A).
mpred_mark(pfcNegTrigger, S1, F, A)/ground(S1)==>marker_supported(F,A).
mpred_mark(pfcBcTrigger, S1, F, A)/ground(S1)==>marker_supported(F,A).
mpred_mark(pfcRHSR, S1, F, A)/ground(S1)==>marker_supported(F,A).
mpred_mark(pfcCreates, S1, F, A)/ground(S1)==>marker_supported(F,A).


marker_supported(F,A)==>hybrid_support(F,A).
marker_supported(F,A)==>{t_l:user_abox(M)->import_to_user(M:F/A)}.


%mpred_mark(pfcPosTrigger, _, F, A)/(integer(A),functor(P,F,A)) ==> pfcTriggered(F/A),   afterAdding(F,lambda(P,mpred_enqueue(P,(m,m)))).
%mpred_mark(pfcNegTrigger, _, F, A)/(integer(A),functor(P,F,A)) ==> pfcTriggered(F/A), afterRemoving(F,lambda(P,mpred_enqueue(~P,(m,m)))).

/*
mpred_mark(pfcRHS,_,F,1)/(fail,atom(F),functor(Head,F,1), 
 \+ argsQuoted(F),
 \+ prologDynamic(F),
 \+ ~(tCol(F)),
 \+ specialFunctor(F),
 \+ predicate_property(Head,built_in))==>completelyAssertedCollection(F).
*/
% mpred_mark(Type,_,F,A)/(integer(A),A>1,F\==arity,Assert=..[Type,F])==>arity(F,A),Assert.

mpred_mark_C(G) ==> {map_mpred_mark_C(G)}.
map_mpred_mark_C(G) :-  map_literals(lambda(P,(get_functor(P,F,A),ain([isa(F,pfcControlled),arity(F,A)]))),G).
mpred_mark(pfcRHS,_,F,A)/(atom(F),integer(A),F\==arity)==>tPred(F),arity(F,A),pfcControlled(F/A).




%:- meta_predicate(mp_test_agr(?,+,-,*,^,:,0,1,5,9)).
%mp_test_agr(_,_,_,_,_,_,_,_,_,_).
%:- mpred_test(predicate_property(mp_test_agr(_,_,_,_,_,_,_,_,_,_),meta_predicate(_))).
% becomes         mp_test_agr(+,+,-,?,^,:,0,1,0,0)


