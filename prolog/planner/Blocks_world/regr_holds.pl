% A SIMPLE REGRESSION PLANNER FOR ACTIONS IN HOLDS NOTATION

% solve(G,NS,P) is true if P is a plan to solve goal G that uses 
% less than NS steps. G is a list of atomic subgoals.

solve(G,_,init) :-
   solved(G).

solve(G,NAs,do(A,Pl)) :-
   NAs > 0,
   useful(G,A),
   wp(G,G1),
%   writeln([' ',NAs,': trying ',A,' to solve ',G]),
%   writeln(['    New subgoals: ',G1]),
   NA1 is NAs-1,
   solve(G1,NA1,Pl).

:- op(1030,xfy,[&]).
:- op(1200,xfx,[<-]).
:- op(700,xfx,[\=]).
% solved(G) is true if goal list G is true initially
solved([]).
solved([H|T]) :-
   (H <- true),
   solved(T).

% useful(G,A) is true if action A is useful to solve a goal in goal list G
useful(G,A) :-
   member(S,G),
   achieves(S,A).
achieves(holds(on(A,B),_),stack(A,B)).
achieves(holds(handempty,_),stack(_,_)).
achieves(holds(clear(Y),_),pickup(_,Y)).
achieves(holds(holding(X),_),pickup(X,_)).

% wp(G,G0) is true if G0 is the weakest precondition that needs to hold
% immediately before action A to ensure that G is true immediately after A
wp(G,G0) :-
   wp(G,[],G0).

% wp(G,C0,C1) true if C1 is the resulting conjuction from solving G given conjunction C0
wp([],C,C).
wp([holds(G,S)|R],C0,C2) :-
   var(S),
   insert(holds(G,S),C0,C1),
   wp(R,C1,C2).
wp([holds(G,S)|R],C0,C2) :-
   \+ var(S),
   (holds(G,S) <- B),
   explode(B,[],BC),
   wp(BC,C0,C1),
   wp(R,C1,C2).

explode((A&B),C0,C2) :-
   explode(A,C0,C1),
   explode(B,C1,C2).
explode(true,C,C).
explode((A \= B),C,C) :- dif(A,B).
explode(holds(P,S),C,[holds(P,S)|C]).

% DEFINITION OF AXIOMS IN HOLDS NOTATION

% stack(X,Y) means to put X onto Y (when holding X and Y is clear)
holds(on(X,Y),do(stack(X,Y),S)) <-
   holds(holding(X),S)&
   holds(clear(Y),S).
holds(handempty,do(stack(X,Y),S)) <-
   holds(holding(X),S)&
   holds(clear(Y),S).
holds(clear(X),do(stack(X,Y),S)) <-
   holds(holding(X),S)&
   holds(clear(Y),S).
holds(on(W,Z),do(stack(X,Y),S)) <-
   holds(holding(X),S)&
   holds(clear(Y),S)&
   holds(on(W,Z),S).
holds(clear(Z),do(stack(X,Y),S)) <-
   Z \= Y&
   holds(holding(X),S)&
   holds(clear(Y),S)&
   holds(clear(Z),S).

% pickup(X,Y) means pick up X from Y (when X is clear and hand is empty)
holds(clear(Y),do(pickup(X,Y),S)) <-
   holds(clear(X),S)&
   holds(on(X,Y),S)&
   holds(handempty,S).
holds(holding(X),do(pickup(X,Y),S)) <-
   holds(clear(X),S)&
   holds(on(X,Y),S)&
   holds(handempty,S).
holds(clear(Z),do(pickup(X,Y),S)) <-
   Z \= X &
   holds(clear(X),S)&
   holds(on(X,Y),S)&
   holds(handempty,S)&
   holds(clear(Z),S).
holds(on(W,Z),do(pickup(X,Y),S)) <-
   W \= X&
   holds(clear(X),S)&
   holds(on(X,Y),S)&
   holds(handempty,S)&
   holds(on(W,Z),S).

% INITIAL STATE
%
%    a
%    b
%    c   d   e
%-------------------

holds(on(a,b),init) <- true.
holds(on(b,c),init) <- true.
holds(on(c,table),init) <- true.
holds(handempty,init) <- true.
holds(clear(a),init) <- true.
holds(on(d,table),init) <- true.
holds(clear(d),init) <- true.
holds(on(e,table),init) <- true.
holds(clear(e),init) <- true.
holds(clear(table),init) <- true.

% Example queries
% solve([holds(on(a,d),S)],2,S).
% solve([holds(on(b,d),S)],4,S).
% solve([holds(on(b,d),S),holds(on(d,a),S)],6,S). % "sussman's anomoly"
% =============================================================================

% member(X,L) is true if X is a member of list L
member(X,[X|_]).
member(X,[_|L]) :-
   member(X,L).

notin(_,[]).
notin(A,[B|C]) :-
   dif(A,B),
   notin(A,C).

% subset(L1,L2) is true if L1 is a subset of list L2
subset([],_).
subset([A|B],L) :-
   member(A,L),
   subset(B,L).

% writeln(L) is true if L is a list of items to be written on a line, followed by a newline.
writeln(L) :- \+ \+ (numbervars(L,0,_), writelnw(L) ).
writelnw([]) :- nl.
writelnw([H|T]) :- write(H), writeln(T).

% insert(E,L0,L1) inserts E into list L0 producing list L1.
% If E is already a member it is not added.
insert(A,[],[A]).
insert(A,[B|L],[A|L]) :- A==B.
insert(A,[B|L],[B|R]) :-
   \+ A == B,
   insert(A,L,R).
grnd(G) :-
   numbervars(G,0,_).

% SIMPLE DEPTH BOUNDED PLANNER USING HOLDS NOTATION
% ss(G,D) simple solve goal G at depth D.
ss((A&B),D) :-
   ss(A,D),
   ss(B,D).
ss(true,_).
ss((A \= B),_) :- dif(A,B).
ss(G,D) :-
   D>0,
   (G <- B),
   D1 is D-1,
   ss(B,D1).
% ss(holds(on(a,d),S),3).
% ss(holds(on(b,d),S),5).
% ss((holds(on(b,d),S)&holds(on(d,a),S)),7).
