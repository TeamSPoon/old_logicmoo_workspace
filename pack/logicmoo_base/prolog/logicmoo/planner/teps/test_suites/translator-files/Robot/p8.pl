:- consult(initial1).

% goal 8 everything in c4 and then everything in c1
goal(and(eventually(and(at(obj1,c4),and(at(obj2,c4),and(at(obj3,c4),at(obj4,c4))))),next(eventually(and(at(obj1,c1),and(at(obj2,c1),and(at(obj3,c1),at(obj4,c1)))))))).

