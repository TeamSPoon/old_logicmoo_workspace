% DEFINITION OF BLOCKS-WORLD IN STRIPS NOTATION

% stack(X,Y) means to put X onto Y (when holding X and Y is clear)
achieves(stack(X,Y),on(X,Y)).
achieves(stack(_,_),handempty).
deletes(stack(_,Y),clear(Y)) :- dif(Y,table).
deletes(stack(X,_),holding(X)).
preconditions(stack(X,Y),[holding(X),clear(Y)]) :- dif(X,Y).

% pickup(X,Y) means pick up X from Y (when X is clear and hand is empty)
achieves(pickup(_,Y),clear(Y)) :- dif(table,Y).
achieves(pickup(X,_),holding(X)).
deletes(pickup(X,Y),on(X,Y)).
deletes(pickup(_,_),handempty).
preconditions(pickup(X,Y),[on(X,Y),clear(X),handempty]) :- dif(X,Y).

% INITIAL STATE
%
%    a
%    b   f
%    c   d   e
%-------------------

achieves(init,P) :-
   holds(P,init).
holds(clear(table),init).
holds(on(a,b),init).
holds(on(b,c),init).
holds(on(c,table),init).
holds(handempty,init).
holds(clear(a),init).
holds(on(d,table),init).
holds(on(f,d),init).
holds(clear(f),init).
holds(clear(e),init).
holds(on(e,table),init).

% Example queries
% solve([holds(on(a,f),S)],S,2).
% solve([holds(on(b,f),S)],S,4).
% solve([holds(on(c,d),S)],S,8).
% solve([holds(on(b,f),S),holds(on(f,a),S)],S,6). % "sussman's anomoly"
% solve([holds(on(a,d),S),holds(on(f,b),S)],S,6). % "block swapping"

% FOR PARTIAL ORDER PLANNER
% solve([on(a,f)],P,2), seq(P,S).
% solve([on(b,f)],P,4), seq(P,S).
% solve([on(c,d)],P,8), seq(P,S).
% solve([on(b,f),on(f,a)],P,6), seq(P,S). % "sussman's anomoly"
% solve([on(a,d),on(f,b)],P,6), seq(P,S). % "block swapping"
% solve([on(b,table),on(f,e)],P,6), seq(P,S). % independent problems
