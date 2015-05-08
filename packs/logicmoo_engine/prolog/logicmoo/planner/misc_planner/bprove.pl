% Computational Intelligence: a logical approach. 
% Prolog Code.
% DEPTH-BOUNDED META-INTERPRETER with negation as failure and inequality
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% This assumes a Prolog implementation with delaying (the when
% predicate), such as Sicstus Prolog

% `<-' is the object-level `if' - it is an infix meta-level predicate
:- op(1150, xfx, <- ).

% `&' is the object level conjunction.
% It is an infix meta-level binary function symbol:
:- op(950,xfy, &).

% `~' is the object level negation as failure.
% It is an prefix meta-level binary function symbol:
:- op(900,fy, ~).

% `\=' is the object level not equal.
% It is an infix meta-level binary function symbol:
:- op(700,xfx, \=).


% bprove(G,D) is true if G can be proven with depth no more than D

bprove(true,_).
bprove(T=T,_).
bprove(T1 \= T2,_) :-
   dif(T1,T2).
bprove((A & B),D) :-
   bprove(A,D),
   bprove(B,D).
bprove((~A),_) :-
   when(ground(A),\+ prove(A)).
bprove(H,D) :-
   D >= 0,
   D1 is D-1,
   (H <- B),
   bprove(B,D1).

prove(true).
prove(T=T).
prove(T1 \= T2) :-
   dif(T1,T2).
prove((A & B)) :-
   prove(A),
   prove(B).
prove((~A)) :-
   when(ground(A),\+ prove(A)).
prove(H) :-
   (H <- B),
   prove(B).
