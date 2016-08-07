

% to load this files use  ?- ensure_mpred_file_loaded('logicmoo/pfc/system_genls.pfc').
:- dynamic(mudIsa/2).
:- file_begin(pfc).

:- set_fileAssertMt(baseKB).

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


