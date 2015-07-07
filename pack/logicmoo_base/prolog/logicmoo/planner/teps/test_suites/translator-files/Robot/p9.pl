:- consult(initial1).

% goal 9 everything in each room and then swap!
goal(and(eventually(and(at(obj1,r1),and(at(obj2,r2),and(at(obj3,r3),at(obj4,r4))))),next(eventually(and(at(obj1,r4),and(at(obj2,r3),and(at(obj3,r2),at(obj4,r1)))))))).

