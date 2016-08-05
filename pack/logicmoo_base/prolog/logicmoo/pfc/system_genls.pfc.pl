

% to load this files use  ?- ensure_mpred_file_loaded('logicmoo/pfc/system_genls.pfc').
:- dynamic(mudIsa/2).
:- file_begin(pfc).

:- set_fileAssertMt(baseKB).

%TODO ((genls(X,Y),genls(Y,X),{X\==Y}) ==> {mpred_withdraw(genls(Y,X))}).

tCol(C)/atom(C),\+ttExpressionType(C)==>fwdCol(C).
fwdCol(X),genls(X,X) ==> \+ genls(X,X).
fwdCol(S)==> (genls(C,S),genls(S,P) ==> genls(C,P)).
fwdCol(C)==> (isa(I,C) ==> mudIsa(I,C)).
fwdCol(S),genls(C,S) ==> (isa(I,C) ==> isa(I,S)).
% ((genls(Sub,M),genls(M,Super))==> genls(Sub,Super)).

end_of_file.


%:-rtrace.
%genls(X,tPred) <==> ttPredType(X).
%:-nortrace.
% sane_transitivity((genls(I,Sub),genls(Sub, Super)),I,Sub,Super)
/*

(genls(C1,C2),arity(C1,1),arity(C2,1),
  { 
   \+((is_asserted(genls(C1,CM)),CM\=C1,is_asserted(genls(CM,C2)),CM\=C2)),C2\=C1
    
   }) ==>

  {
  get_functor(C1,F1),get_functor(C2,F2),
    nop(dmsg(wishing_to_add(C1 ==> C2)))},
    nearestGenls(F1,F2).

nearestGenls(C1,C2)==>
 {get_functor(C1,F1),get_functor(C2,F2),
   P1 =.. [F1,X],
    P2 =.. [F2,X],
    asserta_if_new((P2:-loop_check(P1)))}.
*/

% (genls(C,SC)==>(tCol(SC),tCol(C),{repropagate(SC)})).

:- sanity(get_lang(pfc)).

% TODO (genls(C,SC)/ground(genls(C,SC))==>(tCol(C),tCol(SC))).


(genls(C,SC)==>(tCol(C),tCol(SC))).



% tAvoidForwardChain(functorDeclares).
tAvoidForwardChain(C):-tCol(C),compound(C).
tAvoidForwardChain(meta_argtypes).
% tAvoidForwardChain(completeIsaAsserted).

ttExpressionType(C)==>tAvoidForwardChain(C).

% TODO ((completeIsaAsserted(I), isa(I,Sub), {dif(Sub, Super)}, genls(Sub,Super),{ground(Sub:Super)}, \+ genls/*Fwd*/(Sub,Super), \+ ttExpressionType(Super))) ==> isa(I,Super).

persistInMudIsa(vtValue).
persistInMudIsa(tSpatialThing).

persistInMudIsa(TCOL) ==> (genls(C,TCOL) ==> (isa(I,C)  ==> mudIsa(I,C))).
% TODO persistInMudIsa(TCOL) ==> (genls(C,TCOL) ==> (isa(I,C) /* /( \+ completeIsaAsserted(I)) */ ==> completeIsaAsserted(I))).

completeIsaAsserted(I) ==> ((isa(I,Sub)/ (\+ tAvoidForwardChain(Sub))) ==> mudIsa(I,Sub)).
completeIsaAsserted(I) ==> ((isa(I,Sub)/ genls(Sub,Sup)) ==> isa(I,Sup)).

% TODO persistInMudIsa(TCOL) ==> (genls(O,TCOL) ==> persistInMudIsa(O)).
% TODO persistInMudIsa(TCOL) ==> (genls(TCOL,O) ==> persistInMudIsa(O)).
persistInMudIsa(TCOL) ==> (isa(I,TCOL) ==> mudIsa(I,TCOL)).

cachePredicate(genls) ==> ((genls(Sub,M),genls(M,Super))==> genls(Sub,Super)).
cachePredicate(genls).


% todo persistInMudIsa(TCOL) ==> (isa(I,TCOL) ==> completeIsaAsserted(I)).

tKnownID(Inst)/atom(Inst),{isa_from_morphology(Inst,Type)} ==> mudIsa(Inst,Type).

/*

% isRuntime ==> 
% (mudIsa(I,Sub)/(ground(mudIsa(I,Sub)), \+ tAvoidForwardChain(Sub))) ==> isa(I,Sub).
((completelyAssertedCollection(Sub) / (\+ tAvoidForwardChain(Sub)))) ==> ttMudIsaCol(Sub).
ttMudIsaCol(Sub) ==> (isa(I,Sub) ==> mudIsa(I,Sub)).
((completeIsaAsserted(I),mudIsa(I,Sub), {dif(Sub, Super)}, genls(Sub,Super),{ground(Sub:Super)}, \+ tAvoidForwardChain(Super))) ==> mudIsa(I,Super).
*/

%(isa(I,Sub), genls(Sub, Super),{ground(Sub:Super)}, 
%  \+ ~(completelyAssertedCollection(Super))) ==> ({dif(Sub, Super)}, isa(I,Super)).

/*
(completelyAssertedCollection(C2),genls(C1,C2))==>
 ({get_functor(C1,F1),get_functor(C2,F2),
   P1 =.. [F1,X],
    P2 =.. [F2,X]},(P1==>P2)).

(genls(C1,C2)/( \+ completelyAssertedCollection(C2))==>
 ({get_functor(C1,F1),get_functor(C2,F2),
   P1 =.. [F1,X],
    P2 =.. [F2,X],
    asserta_if_new(baseKB:((P2:-loop_check(P1))))})).
*/

% TODO (genls(C,P)/(C\=P)), completelyAssertedCollection(P)  ==> genls/*Fwd*/(C,P).
% TODO (genls(C,P)/(C\=P, \+ ttExpressionType(C) , \+ ttExpressionType(P) , \+ tAvoidForwardChain(P) )) ==> genls/*Fwd*/(C,P).

% TODO genls/*Fwd*/(C,P)/(C\=P) ==> (isa(I,C) ==> isa(I,P)).

% genls(C,P)/(C\=P) ==> (isa(I,C) ==> isa(I,P)).

% TODO cachePredicate(mudIsa) ==> (tSet(C) ==> (isa(I,C)==>mudIsa(I,C))).
%cachePredicate(mudIsa).
/*
((genls(C1,C2), ( \+ genls/*Fwd*/(C1,C2)))==>
 ({get_functor(C1,F1),get_functor(C2,F2),
   P1 =.. [F1,X],
    P2 =.. [F2,X],
    asserta_if_new(baseKB:((P2:-loop_check(P1))))})).
*/

