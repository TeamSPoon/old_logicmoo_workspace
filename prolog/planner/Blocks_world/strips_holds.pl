% A SIMPLE STRIPS PLANNER FOR ACTIONS IN HOLDS NOTATION

:- op(1030,xfy,[&]).
:- op(1200,xfx,[<-]).
:- op(700,xfx,[\=]).

%achieve(G,S0,S1) is true if G can be achieved going from state S0 to state S1.

achieve(G,S,S) :-
   prove(holds(G,S)),!.

achieve((G1&G2),S0,S2) :- !,
   achieve(G1,S0,S1),
   achieve(G2,S1,S2),
   prove(holds(G1,S2)).
achieve((A \= B),S,S) :- dif(A,B).
achieve(true,S,S) :- !.
achieve(G,S0,do(A,S1)) :-
   achieves(G,A),
   (holds(G,do(A,S1)) <- Body),
   achieve(Body,S0,S1).

prove(true).
prove((A&B)) :-
   prove(A),
   prove(B).
prove(G) :-
   (G <- B),
   prove(B).

achieves(on(A,B),stack(A,B)).
achieves(handempty,stack(_,_)).
achieves(clear(Y),pickup(_,Y)).
achieves(holding(X),pickup(X,_)).

A \= B :- dif(A,B).

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
% achieve(on(a,d),init,S).
% achieve(on(b,d)init,S).
% achieve((on(b,d)&on(d,a)),init,S). % "sussman's anomoly"
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
