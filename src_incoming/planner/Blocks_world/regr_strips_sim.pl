% A SIMPLE REGRESSION PLANNER FOR ACTIONS IN STRIPS NOTATION

% solve(G,NS,P) is true if P is a plan to solve goal G that uses 
% less than NS steps. G is a list of atomic subgoals.

solve(G,P) :-
   solve(G,4,P).

solve(G,_,init) :-
   solved(G).

solve(G,NAs,do(A,Pl)) :-
   NAs > 0,
   useful(G,A),
   wp(G,A,G1),
   writeln(['Trying ',A,' to solve ',G]),
   writeln(['    New subgoals: ',G1]),
   NA1 is NAs-1,
   solve(G1,NA1,Pl).

% solved(G) is true if goal list G is true initially
solved([]).
solved([G|R]) :-
   holds(G,init),
   solved(R).

% useful(G,A) is true if action A is useful to solve a goal in goal list G
useful(G,A) :-
   member(S,G),
   achieves(S,A).

achieves(G,A) :-
   addlist(A,AL),
   member(G,AL).

% wp(G,A,G0) is true if G0 is the weakest precondition that needs to hold
% immediately before action A to ensure that G is true immediately after A
wp([],A,G1) :-
   preconditions(A,G1).
wp([S|R],A,G) :-
   addlist(A,AL),
   member(S,AL),
   wp(R,A,G).
wp([S|R],A,G1) :-
   addlist(A,AL),
   notin(S,AL),
   deletelist(A,DL),
   notin(S,DL),
   wp(R,A,G),
   insert(S,G,G1).

% DEFINITION OF AXIOMS IN STRIPS NOTATION

% stack(X,Y) means to put X onto Y (when holding X and Y is clear)
addlist(stack(X,Y),[on(X,Y),handempty,clear(X)]) :- dif(X,Y).
deletelist(stack(X,Y),[holding(X),clear(Y)]).
preconditions(stack(X,Y),[holding(X),clear(Y)]).

% pickup(X,Y) means pick up X from Y (when X is clear and hand is empty)
addlist(pickup(X,Y),[clear(Y),holding(X)]) :- dif(X,Y).
deletelist(pickup(X,Y),[on(X,Y),handempty,clear(X)]).
preconditions(pickup(X,Y),[clear(X),on(X,Y),handempty]).

% INITIAL STATE
%
%    a
%    b
%    c   d   e
%-------------------

holds(on(a,b),init).
holds(on(b,c),init).
holds(on(c,table),init).
holds(handempty,init).
holds(clear(a),init).
holds(on(d,table),init).
holds(clear(d),init).
holds(on(e,table),init).
holds(clear(e),init).
holds(clear(table),init).

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
