/*

tCol(tCol).
tCol(tSet).
tCol(C)/( \+ ttExpressionType(C))==>tSet(C).
tCol(mtCycL).
tCol(col_as_isa).
tCol(col_as_unary).
col_as_unary(col_as_unary).
col_as_unary(col_as_isa).
tCol(C)/( \+ col_as_unary(C))==>col_as_isa(C).
mtCycL(baseKB).
tSet(tCol).
tSet(tSet).
genls(tSet,tCol).
genls(ttExpressionType,tCol).
genls(ttExpressionType,col_as_unary).
tSet(tExisting).
tCol(tRR).
genls(tRR,tRRP).
genls(tRRP,tRRP2).
col_as_unary(tRRP2).
tRR(iRR7).
% tAvoidForwardChain(functorDeclares).
tAvoidForwardChain(C):- loop_check(tCol(C)),compound(C).
tAvoidForwardChain(meta_argtypes).
% tAvoidForwardChain(completeIsaAsserted).
ttExpressionType(C)==>tAvoidForwardChain(C).
genls(C,P)==>tCol(C),tCol(P).
(genls(C,P)/(C\=P)), completelyAssertedCollection(P)  ==> genlsFwd(C,P).
(genls(C,P)/(C\=P, \+ ttExpressionType(C) , \+ ttExpressionType(P) , \+ tAvoidForwardChain(P) )) ==> genlsFwd(C,P).
genlsFwd(C,P) ==> (isa(I,C) ==> isa(I,P)).

not_undoable(G):-call_u(G).

ttTypeType(col_as_unary).
genl(ttTypeType,tCol).

tCol(col_as_unary).
col_as_unary(mtCycL).
col_as_unary(completelyAssertedCollection).

tCol(C) ==> {atom(C),not_undoable((CI=..[C,I],assertz_if_new((CI:- (cwc,   loop_check(isa(I,C)))))))}.

col_as_unary(C) ==> {atom(C),not_undoable((CI=..[C,I],forall(retract(isa(I,C):-true),mpred_post1(CI)),retractall(col_as_isa(C))))}.
col_as_isa(C) ==> {atom(C),not_undoable((CI=..[C,I],forall(retract(CI:-true),mpred_post1(isa(I,C))),retractall(col_as_unary(C))))}.

genls(tSet,functorDeclares).
col_as_unary(functorDeclares).
genls(completelyAssertedCollection,tSet).

isa(I,ttRelationType):-I==col_as_unary,!,fail.
isa(I,ttRelationType):-I==col_as_unary,!,fail.
isa(I,ttRelationType):-I==col_as_isa,!,fail.
isa(I,C):- functorDeclares==C, I== (==>) ,!,fail.
isa(I,C):- cwc, atom(C),loop_check((col_as_unary(C);\+col_as_isa(C))),loop_check(isa_w_type_atom(I,C)).

genlsFwd(C,P) ==> (isa(I,C) ==> isa(I,P)).

:- must( baseKB:isa(iRR7,tRR) ).
:- must( baseKB:isa(iRR7,tRRP) ).

:-  tRRP(iRR7) .

% :- break.

%:- defconcept(tCol,[or([ttExpressionType,tSet])]).
%:- defconcept([and([tCol,naf((ttExpressionType))])],tSet).


% tExisting(iRR7).
:- set_prolog_flag(logicmoo_motel,false).





:- if(current_prolog_flag(logicmoo_motel,true)).

isa(I,C)==>{assert_ind(I,C)}.
tSet(C)==> 
  ( tCol(C), % defprimconcept(C),
  (isa(I,C)==>{assert_ind(I,C)}),
  {CI=..[C,I],assertz_if_new((CI:- (cwc,   isa_backchaing(I,C))))}).

genls(Sub,Sup)==>(tCol(Sup),tCol(Sub),{defprimconcept(Sub,Sup),sb_primconcept(Sub,[supers([Sup])])}).
:- show_dag.
:- showEnvironment.
:- printAll(isa(_X,_Y)).
:- must( baseKB:isa(iRR7,tRR) ).
:- must( baseKB:isa(iRR7,tRRP) ).
:- must( baseKB:tRRP(iRR7) ).
%:-defconcept(tPred,[or([prologBuiltin,prologHybrid])]).


% :- break.
% :- \+ baseKB:tRRP(iRR7) -> (xlisting(iRR7),xlisting(tRRP)) ; true.

installedMotelHook.

:- endif.



% :- xlisting(tKnownID).
%?- isa(tKnownID,W).
%tExisting(X):-atom(X).



tSet(tKnownID).


*/

% to load this files use  ?- ensure_mpred_file_loaded('logicmoo/pfc/system_genls.pfc').
:- dynamic(mudIsa/2).
:- file_begin(pfc).


((genls(X,Y),genls(Y,X),{X\==Y}) ==> {mpred_withdraw(genls(Y,X))}).
% (genls(C,SC)==>(tCol(SC),tCol(C),{repropagate(SC)})).

:- sanity(get_lang(pfc)).

% TODO (genls(C,SC)==>(tCol(C),tCol(SC))).

% tAvoidForwardChain(functorDeclares).
tAvoidForwardChain(C):-tCol(C),compound(C).
tAvoidForwardChain(meta_argtypes).
% tAvoidForwardChain(completeIsaAsserted).

ttExpressionType(C)==>tAvoidForwardChain(C).

% TODO ((completeIsaAsserted(I), isa(I,Sub), {dif(Sub, Super)}, genls(Sub,Super),{ground(Sub:Super)}, \+ genls/*Fwd*/(Sub,Super), \+ ttExpressionType(Super))) ==> isa(I,Super).
%    \+ genlsFwd(Sub,Super), \+ ttExpressionType(Super))) ==> isa(I,Super).

completeIsaAsserted(I) ==> ((isa(I,Sub)/ (\+ tAvoidForwardChain(Sub))) ==> mudIsa(I,Sub)).
/*

% isRuntime ==> 
% (mudIsa(I,Sub)/(ground(mudIsa(I,Sub)), \+ tAvoidForwardChain(Sub))) ==> isa(I,Sub).
((completelyAssertedCollection(Sub) / (\+ tAvoidForwardChain(Sub)))) ==> ttMudIsaCol(Sub).
ttMudIsaCol(Sub) ==> (isa(I,Sub) ==> mudIsa(I,Sub)).
((completeIsaAsserted(I),mudIsa(I,Sub), {dif(Sub, Super)}, genls(Sub,Super),{ground(Sub:Super)}, \+ tAvoidForwardChain(Super))) ==> mudIsa(I,Super).
*/


(genls(C,P)/(C\=P)), completelyAssertedCollection(P)  ==> genlsFwd(C,P).
(genls(C,P)/(C\=P, \+ ttExpressionType(C) , \+ ttExpressionType(P) , \+ tAvoidForwardChain(P) )) ==> genlsFwd(C,P).

genlsFwd(C,P)/(C\=P) ==> (isa(I,C) ==> isa(I,P)).

((genls(C1,C2), ( \+ genlsFwd(C1,C2)))==>
 ({get_functor(C1,F1),get_functor(C2,F2),
   P1 =.. [F1,X],
    P2 =.. [F2,X],
    asserta_if_new(baseKB:((P2:-loop_check(P1))))})).


