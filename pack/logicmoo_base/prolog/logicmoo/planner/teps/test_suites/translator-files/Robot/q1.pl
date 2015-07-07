:-consult(initial2).

% goal 0
%goal(always(or(not(final),and(at(robot,c1),at(obj1,r2))))).
% goal 1
% this goal is unsolvable requires to close door immediately after opening it
goal(and(and(always(or(not(and(closed(d1), next(not(closed(d1))))), next(next(closed(d1))))), and(always(or(not(and(closed(d12), next(not(closed(d12))))), next(next(closed(d12))))), and(always(or(not(and(closed(d23), next(not(closed(d23))))), next(next(closed(d23))))), and(always(or(not(and(closed(d34), next(not(closed(d34))))), next(next(closed(d34))))), always(or(not(and(closed(d4), next(not(closed(d4))))), next(next(closed(d4))))))))),always(or(not(final),and(at(robot,c1),at(obj1,r4)))))).
% goal 2
%goal(and(and(always(or(not(and(closed(d1), next(not(closed(d1))))), next(next(next(closed(d1)))))), and(always(or(not(and(closed(d12), next(not(closed(d12))))), next(next(next(closed(d12)))))), and(always(or(not(and(closed(d23), next(not(closed(d23))))), next(next(next(closed(d23)))))), and(always(or(not(and(closed(d34), next(not(closed(d34))))), next(next(next(closed(d34)))))), always(or(not(and(closed(d4), next(not(closed(d4))))), next(next(next(closed(d4)))))))))),always(or(not(final),and(at(robot,c1),at(obj1,r4)))))).
% goal 3
%goal(and(and(always(or(not(and(closed(d1), next(not(closed(d1))))), next(eventually(closed(d1))))), and(always(or(not(and(closed(d12), next(not(closed(d12))))), next(eventually(closed(d12))))), and(always(or(not(and(closed(d23), next(not(closed(d23))))), next(eventually(closed(d23))))), and(always(or(not(and(closed(d34), next(not(closed(d34))))), next(eventually(closed(d34))))), always(or(not(and(closed(d4), next(not(closed(d4))))), next(eventually(closed(d4))))))))),and(always(or(not(final),at(robot,c1))),always(or(not(final),at(obj2,r4)))))).

%goal 4 % all objects in r1, and all eventually in r4
%goal(and(always(or(not(final),and(at(obj1, c1), and(at(obj2, c1), and(at(obj3, c1), at(obj4, c1)))))),and(eventually(at(obj1, r4)), and(eventually(at(obj2, r4)), and(eventually(at(obj3, r4)), eventually(at(obj4, r4))))))).



% goal(and(eventually(at(obj1,c4)),always(or(not(final),and(at(robot,c1),at(obj1,r2)))))).
% goal 3
%goal(and(or(not(eventually(at(robot,r1))),
%	    eventually(and(closed(d1),and(closed(d12),and(closed(d23),and(closed(d34),closed(d4))))))),
%	 always(or(not(final),and(at(robot,c1),at(obj2,r3)))))).
													     
%goal 4
%goal(and(eventually(and(at(obj1,r4),at(obj2,r3))),and(always(or(not(final),at(robot,c1))),always(or(not(final),at(obj2,r4)))))).

%goal 5
%goal(and(and(eventually(at(obj1,r4)),eventually(at(obj1,r2))),and(always(or(not(final),at(robot,c1))),always(or(not(final),at(obj2,r4)))))).

