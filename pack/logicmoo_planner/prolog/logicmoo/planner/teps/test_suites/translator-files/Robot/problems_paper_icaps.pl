all_sizes :- forall(goal(G),(size_fla(G,N),writef("%w\n",[N]))).

goal(always(or(not(final),and(at(robot,c1),at(obj1,r2))))).

goal(always(or(not(final),and(at(robot,c1),at(obj1,r2))))).

goal(and(or(next(next(next(next(next(at(obj1,r4)))))),next(next(next(next(next(next(at(obj1,r4)))))))),always(or(not(final),and(at(robot,c1),at(obj1,r2)))))).

goal(always(and(
	   always(or(not(at(robot,r1)),next(eventually(closed(d1))))),
	   eventually(always(at(obj1,r4)))))).


goal(and(or(not(eventually(at(robot,r1))),
	    eventually(and(closed(d1),and(closed(d12),and(closed(d23),and(closed(d34),closed(d4))))))),
	 always(or(not(final),and(at(robot,c1),at(obj2,r4)))))).


goal(and(and(always(or(not(and(closed(d1), next(not(closed(d1))))), next(next(next(closed(d1)))))), and(always(or(not(and(closed(d12), next(not(closed(d12))))), next(next(next(closed(d12)))))), and(always(or(not(and(closed(d23), next(not(closed(d23))))), next(next(next(closed(d23)))))), and(always(or(not(and(closed(d34), next(not(closed(d34))))), next(next(next(closed(d34)))))), always(or(not(and(closed(d4), next(not(closed(d4))))), next(next(next(closed(d4)))))))))),always(or(not(final),and(at(robot,c1),at(obj1,r4)))))).


goal(and(or(not(eventually(at(robot,r1))),
	    eventually(and(closed(d1),and(closed(d12),and(closed(d23),and(closed(d34),closed(d4))))))),
	 always(or(not(final),and(at(robot,c1),at(obj2,r3)))))).
													     

goal(and(eventually(at(obj1,c4)),always(or(not(final),and(at(robot,c1),at(obj1,r2)))))).

goal(and(eventually(and(at(obj1,r4),at(obj2,r3))),always(or(not(final),and(at(robot,c1),at(obj2,r4)))))).

goal(and(and(eventually(at(obj1,r4)),eventually(at(obj1,r2))),always(or(not(final),and(at(robot,c1),at(obj2,r4)))))).


goal(and(eventually(
 		    and(
 			or(

 				   and(at(obj3,r4),
 				       at(obj4,r4)),
			           and(at(obj3,r3),
 				       at(obj4,r3))),
 		    next(
 			 and(closed(d1),
 			     and(closed(d12),
 				 and(closed(d23),
 				     and(closed(d34),
 					 closed(d4)))))))),
 	 eventually(always(at(obj1,c1))))).



goal(and(eventually(
 		    and(
 			or(
 				   and(at(obj3,r4),
 				       at(obj4,r4)),
 			       and(at(obj2,r3),
 				   and(at(obj3,r3),
 				       at(obj4,r3)))),
 		    next(
 			 and(closed(d1),
 			     and(closed(d12),
 				 and(closed(d23),
 				     and(closed(d34),
 					 closed(d4)))))))),
 	 eventually(always(at(obj1,c1))))).




goal(and(eventually(at(obj1,r4)),
	and(eventually(at(obj2,c1)),
	eventually(always(and(at(obj1,c4),and(at(obj2,c4),and(at(obj3,c4),at(obj4,c4))))))))).



goal(and(eventually(
 		    and(
 			or(
 			   and(at(obj1,r4),
 			       and(at(obj2,r4),
 				   and(at(obj3,r4),
 				       at(obj4,r4)))),
 			   and(at(obj1,r3),
 			       and(at(obj2,r3),
 				   and(at(obj3,r3),
 				       at(obj4,r3))))),
 		    next(
 			 and(closed(d1),
 			     and(closed(d12),
 				 and(closed(d23),
 				     and(closed(d34),
 					 closed(d4)))))))),
 	 eventually(always(at(obj1,c1))))).




goal(and(eventually(and(at(obj1,c4),and(at(obj2,c4),and(at(obj3,c4),at(obj4,c4))))),next(eventually(and(at(obj1,c1),and(at(obj2,c1),and(at(obj3,c1),at(obj4,c1)))))))).

















goal(and(always(or(not(final),and(at(obj1, c1), and(at(obj2, c1), and(at(obj3, c1), at(obj4, c1)))))),and(eventually(at(obj1, r4)), and(eventually(at(obj2, r4)), and(eventually(at(obj3, r4)), eventually(at(obj4, r4))))))).






													     






