:- consult(initial1).

% goal 0
goal(always(or(not(final),and(at(robot,c1),at(obj1,r2))))).

% goal 1
% goal(always(and(
%	   always(or(not(at(robot,r1)),next(eventually(closed(d1))))),
%	   eventually(always(at(obj1,r4)))))).

% goal 2
% goal(and(or(next(next(next(next(at(obj1,r4))))),next(next(next(next(next(at(obj1,r4))))))),always(or(not(final),and(at(robot,c1),at(obj1,r2)))))).

% goal 3
% goal(and(eventually(at(obj1,c4)),always(or(not(final),and(at(robot,c1),at(obj1,r2)))))).

% goal 4
% goal(and(or(not(eventually(at(robot,r1))),
%	    eventually(and(closed(d1),and(closed(d12),and(closed(d23),and(closed(d34),closed(d4))))))),
%	 always(or(not(final),and(at(robot,c1),at(obj2,r3)))))).
													     
% goal 5
% goal(and(eventually(and(at(obj1,r4),at(obj2,r3))),always(or(not(final),and(at(robot,c1),at(obj2,r4)))))).

% goal 6
% goal(and(and(eventually(at(obj1,r4)),eventually(at(obj1,r2))),always(or(not(final),and(at(robot,c1),at(obj2,r4)))))).

