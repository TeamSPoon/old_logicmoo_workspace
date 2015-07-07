:- consult(robot_init1).

goal(always(and(
	   always(or(not(at(robot,r1)),next(eventually(closed(d1))))),
	   eventually(always(at(obj1,r4)))))).
