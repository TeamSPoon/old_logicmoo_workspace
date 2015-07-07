constant(X) :- member(X,[c1,c4,r1,r2,r3,r4,d1,d12,d23,d34,d4,robot,corridor,obj1,obj2,obj3,obj4]).

%% Architecture of the room

initially_true(opened(corridor)).

initially_true(connects(X,Y,Z)) :-
	member([X,A,B],[[d1,c1,r1],[d12,r1,r2],
			[d23,r2,r3],[d34,r3,r4],
			[d4,c4,r4],[corridor,c1,c4]]),
	(Y=A,B=Z;Z=A,Y=B).

initially_true(door(X)) :- member(X,[d1,d12,d23,d34,d4]).


%% position of objects/robot

initially_true(object(X)) :- member(X,[o1,o2,o3,o4]).
initially_true(at(o1,r1)).
initially_true(at(o2,r2)).
initially_true(at(o3,r1)).
initially_true(at(o4,r1)).
initially_true(at(robot,c1)).
initially_true(handempty).
initially_true(opened(D)) :- member(D,[d1,d12,d23,d34,d4]).


sequence([X],eventually(X)) :- !.
sequence([X|L],eventually(and(X,next(F)))) :-
	sequence(X,F).

%goal 0
goal(0,eventually(always(at(robot,r4)))).

%goal 1
goal(0,eventually(always(at(o1,r4)))).

%goal 2
goal(eventually(and(at(o1,r2),next(eventually(at(o1,r4)))))).

%goal 3
goal(F) :- sequence([at(o1,r2),at(o1,r4),at(o1,c1)],F).

%goal 4
goal(F) :- sequence([at(o1,r2),at(o1,r4),at(o1,c1),at(o1,r3)],F).

%goal 5
goal(F) :- sequence([at(o1,r2),at(o1,r4),at(o1,c1),at(o1,r3),at(o1,c2)],F).

%goal 6
goal(F) :- sequence([at(o1,r2),at(o1,r4),at(o1,c1),at(o1,r3),at(o1,c2),at(o1,r2)],F).

%goal 7
goal(F) :- sequence([at(o1,r2),at(o1,r4),at(o1,c1),at(o2,r3),at(o2,c2),at(o2,r2)],F).

%goal 8
goal(F) :- sequence([at(o1,r2),at(o2,r4),at(o2,c1),at(o3,r3),at(o4,c2),at(o1,r2)],F).
