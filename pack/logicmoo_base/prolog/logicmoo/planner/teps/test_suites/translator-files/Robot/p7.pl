:- consult(initial1).

% goal 7 o1 eventually in r4, o2 eventually in c1, and finally every object in c2

goal(and(eventually(at(obj1,r4)),
	and(eventually(at(obj2,c1)),
	eventually(always(and(at(obj1,c4),and(at(obj2,c4),and(at(obj3,c4),at(obj4,c4))))))))).

