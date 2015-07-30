:- consult(robot_init1).

goal(and(eventually(at(obj1,c4)),
	 always(or(not(final),
		   and(at(robot,c1),at(obj1,r2)))))).
