

:- file_begin(pfc).

((genls(X,Y),genls(Y,X),{X\==Y}) ==> {mpred_rem1(genls(X,Y))}).

%:-rtrace.
%genls(X,tPred) <==> ttPredType(X).
%:-nortrace.
% sane_transitivity((genls(I,Sub),genls(Sub, Super)),I,Sub,Super)
(genls(C1,C2),arity(C1,1),arity(C2,1),
  { 
   \+((genls(C1,CM),CM\=C1,genls(CM,C2),CM\=C2)),C2\=C1
    
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

% (genls(C,SC)==>(tCol(SC),tCol(C),{repropagate(SC)})).

((completeIsaAsserted(I), isa(I,Sub), genls(Sub, Super),{ground(Sub:Super)}) ==> ({dif:dif(Sub, Super)}, isa(I,Super))).
% (isa(I,Sub), genls(Sub, Super),{ground(Sub:Super)}, \+ ~(completelyAssertedCollection(Super))) ==> ({dif:dif(Sub, Super)}, isa(I,Super)).


( meta_argtypes(FT), {dif:dif(FT,COL)}, genls(FT, COL),tCol(COL),{not(isa(COL,ttFormatType))}) ==> formatted_resultIsa(FT,COL).

