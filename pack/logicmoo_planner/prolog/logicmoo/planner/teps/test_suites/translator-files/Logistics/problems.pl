:- consult(initial1).


all_sizes :- forall(goal(G),(size_fla(G,N),writef("%w\n",[N]))).


goal(eventually(always(at(package6,city2_2)))).

goal(and(all(a,or(not(airport(a)),eventually(exists(p,and(airplane(p),at(p,a)))))),
	eventually(always(at(package6,city3_1))))).

goal(and(eventually(at(package6,city2_2)),and(eventually(at(package6,city1_2)),eventually(at(package6,city3_2))))).

goal(and(eventually(at(package6,city2_1)),and(eventually(at(package6,city1_1)),eventually(at(package6,city3_1))))).

goal(and(eventually(at(package6,city2_1)),and(eventually(at(package6,city1_1)),always(eventually(at(package6,city3_2)))))).

goal(all(l,or(not(location(l)),eventually(at(package6,l))))).

goal(all(l,or(not(in_city(l,city3)),eventually(at(package6,l))))).

goal(all(l,or(not(in_city(l,city4)),eventually(at(package6,l))))).

goal(eventually(and(all(o,or(not(obj(o)),at(o,city3_1))), 
	eventually(all(o,or(not(obj(o)),at(o,city2_1))))))).

goal(and(exists(a,and(airport(a),and(not(equal(a,city4_2)),
	eventually(all(p,or(not(airplane(p)),at(p,a))))))),
   eventually(always(at(package6,city3_2))))).


goal(and(all(a,or(not(airport(a)),eventually(exists(p,and(airplane(p),at(p,a)))))),
	eventually(always(at(package6,city3_1))))).

goal(and(all(a,or(not(airport(a)),eventually(and(exists(p1,and(airplane(p1),at(p1,a))),
	     next(eventually(exists(p2,and(airplane(p2),at(p2,a))))))))),
     eventually(always(at(package6,city3_1))))).


goal(eventually(and(at(package6,city2_2),next(eventually(at(package6,city1_2)))))).


goal(eventually(and(at(package6,city2_2),
	next(and(eventually(at(package6,city1_2)),
		 next(eventually(at(package6,city3_2)))))))).


goal(eventually(and(at(package6,city2_1),next(eventually(at(package6,city1_1)))))).


goal(eventually(and(at(package6,city2_1),
	next(eventually(and(at(package6,city1_1),
	next(eventually(at(package6,city3_2))))))))).


goal(and(until(not(at(package3,city1_2)),at(package6,city1_2)),
	eventually(always(at(package3,city1_2))))).


goal(and(until(not(at(package3,city2_2)),at(package6,city2_2)),eventually(always(at(package3,city2_2))))).


goal(and(until(not(or(at(package3,city2_2),at(package2,city2_2))),at(package6,city2_2)),eventually(always(and(at(package3,city2_2),at(package2,city2_2)))))).



goal(all(a,or(not(airport(a)),eventually(at(plane2,a))))).


goal(and(all(a,or(not(airport(a)),eventually(at(plane2,a)))),eventually(always(at(package6,city1_1))))).


goal(and(all(a,or(not(airport(a)),eventually(at(plane2,a)))),eventually(always(at(plane2,city4_2))))).


goal(all(a,or(not(airport(a)),eventually(exists(p,and(airplane(p),at(p,a))))))).
