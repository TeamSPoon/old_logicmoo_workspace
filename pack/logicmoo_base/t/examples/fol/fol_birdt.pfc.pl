%
%  PFC is a language extension for prolog.. there is so much that can be done in this language extension to Prolog
%
%
% props(Obj,[height(ObjHt)]) == t(height,Obj,ObjHt) == rdf(Obj,height,ObjHt) == t(height(Obj,ObjHt)).
% padd(Obj,[height(ObjHt)]) == prop_set(height,Obj,ObjHt,...) == ain(height(Obj,ObjHt))
% [pdel/pclr](Obj,[height(ObjHt)]) == [del/clr](height,Obj,ObjHt) == [del/clr]svo(Obj,height,ObjHt) == [del/clr](height(Obj,ObjHt))
% keraseall(AnyTerm). 
%                      ANTECEEDANT                                   CONSEQUENT
%
%         P =         test nesc_true                         assert(P),retract(neg(P))
%       ~ P =         test not_nesc_true                     disable(P), assert(neg(P)),retract(P)
%    neg(P) =         test false/impossible                  make_impossible(P), assert(neg(P))
%   ~neg(P) =         test possible (via not impossible)     enable(P),make_possible(P),retract(neg(P))
%  \+neg(P) =         test impossiblity is unknown           remove_neg(P),retract(neg(P))
%  
%     \+(P) = test naf(P)                            retract(P)
%
% Dec 13, 2035
% Douglas Miles

:- module(sanity_birdt,[]).

:- baseKB:use_module(library(logicmoo/logicmoo_user)).

:- begin_pfc.


tCol(tFly).
tCol(tCanary).
tCol(tPenguin).


tCol(tBird).


:- mpred_test(predicate_property(tBird(_),dynamic)).

genls(tCanary,tBird).
genls(tPenguin,tBird).



:- dmsg("chilly is a penguin.").
tPenguin(iChilly).

:-mpred_test((tBird(iChilly))).



:- dmsg("tweety is a canary.").
tCanary(iTweety).

:-mpred_test((tBird(iTweety))).


:- dmsg("birds fly by default.").
mpred_default(( tBird(X) => tFly(X))).

:- dmsg("make sure chilly can fly").
:-mpred_test((isa(I,tFly),I=iChilly)).

:- dmsg("make sure tweety can fly (and again chilly)").
:-mpred_test((tFly(iTweety))).
:-mpred_test((tFly(iChilly))).


:- dmsg("penguins do not tFly.").
tPenguin(X) => ~tFly(X).

:- dmsg("confirm chilly now cant fly").
:-mpred_test((\+ tFly(iChilly))).
:-mpred_test((neg(tFly(iChilly)))).

tBird(iChilly).

:- dmsg("confirm chilly still cant fly").
:-mpred_test((\+ tFly(iChilly))).
:-mpred_test((neg(tFly(iChilly)))).

/*

% This would be a good TMS test it should throw.. but right now it passes wrongly
tFly(iChilly).

:- dmsg("confirm chilly is flying penguin").
:-mpred_test(( tFly(iChilly))).
:-mpred_test(( tPenguin(iChilly))).
:-mpred_test((\+ neg(tFly(iChilly)))).

\+ tFly(iChilly).

:- dmsg("confirm chilly is a normal penguin who cant fly").
:-mpred_test((\+ tFly(iChilly))).

% fails rightly
:-mpred_test(( tPenguin(iChilly))).

*/

:- dmsg("chilly is no longer a penguin").
\+ tPenguin(iChilly).

:- dmsg("confirm chilly is flying bird").
:-mpred_test(( tFly(iChilly))).
:-mpred_test(( \+ tPenguin(iChilly))).
:-mpred_test((\+ neg(tFly(iChilly)))).


