:-consult(initial2).


goal(and(eventually(
 		    and(
 			or(  and(at(obj1,r4),
 			       and(at(obj2,r4),
 				   and(at(obj3,r4),
 				       at(obj4,r4)))),
			     and(at(obj1,r4),
 			       and(at(obj2,r3),
 				   and(at(obj3,r3),
 				       at(obj4,r3))))),
 		    next(

 				 and(closed(d23),
 				     and(closed(d34),
 					 closed(d4)))))),
 	 eventually(always(at(obj1,c1))))).




%goal(always(and(
%	   always(or(not(at(robot,r1)),next(eventually(and(at(obj1,r4),and(at(obj2,r4),and(at(obj3,r4),and(at(obj4,r4),closed(d1))))))))),
%	   eventually(always(at(obj1,r4)))))).
