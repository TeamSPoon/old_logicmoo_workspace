

% to load this files use  ?- ensure_mpred_file_loaded('logicmoo/pfc/system_genls.pfc').

:- file_begin(pfc).

:- set_fileAssertMt(baseKB).

((genls(X,Y),genls(Y,X),{X\==Y}) ==> {mpred_withdraw(genls(Y,X))}).

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

(genls(C,SC)/ground(genls(C,SC))==>(tCol(C),tCol(SC))).



% mudIsaSkippedCollection(functorDeclares).
mudIsaSkippedCollection(C):-compound(C).
mudIsaSkippedCollection(meta_argtypes).
% mudIsaSkippedCollection(completeIsaAsserted).

ttExpressionType(C)==>mudIsaSkippedCollection(C).

((completelyAssertedCollection(Sub) / (\+ mudIsaSkippedCollection(Sub)))) ==> ttMudIsaCol(Sub).

ttMudIsaCol(Sub) ==> (isa(I,Sub) ==> mudIsa(I,Sub)).

completeIsaAsserted(I) ==> ((isa(I,Sub)/ (\+ mudIsaSkippedCollection(Sub))) ==> mudIsa(I,Sub)).

% isRuntime ==> 
(mudIsa(I,Sub)/(ground(mudIsa(I,Sub)), \+ mudIsaSkippedCollection(Sub))) ==> isa(I,Sub).

((completeIsaAsserted(I),mudIsa(I,Sub), {dif(Sub, Super)}, genls(Sub,Super),{ground(Sub:Super)}, \+ mudIsaSkippedCollection(Super))) ==> mudIsa(I,Super).
((completeIsaAsserted(I), isa(I,Sub), {dif(Sub, Super)}, genls(Sub,Super),{ground(Sub:Super)}, \+ genlsFwd(Sub,Super), \+ ttExpressionType(Super))) ==> isa(I,Super).



/*

Taxinomic Pair Caching

000 C->I + C->C + I->C     no index
001 C->I + C->C + I->>C      
010 C->I + C->>C + I->C       
011 C->I + C->>C + I->>C     Index2A
100 C->>I + C->C + I->C      
101 C->>I + C->C + I->>C   Index2B

110 C->>I + C->>C + I->C    Index2C

111 C->>I + C->>C + I->>C  Fully indexed


predicate types 101
expression types 010
in world types 101


100 < top down >

011 < bottem up >


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

(genls(C,P)/(C\=P)), completelyAssertedCollection(P)  ==> genlsFwd(C,P).
(genls(C,P)/(C\=P, \+ ttExpressionType(C) , \+ ttExpressionType(P) )) ==> genlsFwd(C,P).

genlsFwd(C,P)/(C\=P) ==> (isa(I,C) ==> isa(I,P)).

((genls(C1,C2), ( \+ genlsFwd(C2)))==>
 ({get_functor(C1,F1),get_functor(C2,F2),
   P1 =.. [F1,X],
    P2 =.. [F2,X],
    asserta_if_new(baseKB:((P2:-loop_check(P1))))})).


