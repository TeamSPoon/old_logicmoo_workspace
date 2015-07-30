:- consult(initial1).


%goal 1
%goal(eventually(always(at(package6,city2_2)))).

%goal 2
%goal(eventually(and(at(package6,city2_2),next(eventually(at,package6,city1_2))))).

%goal 3
%goal(eventually(and(at(package6,city2_2),
%	next(and(eventually(at,package6,city1_2)),next(eventually(at,package6,city3_2)))))).

%goal 4
%goal(eventually(and(at(package6,city2_1),next(eventually(at(package6,city1_1)))))).

%goal 5
%goal(eventually(and(at(package6,city2_1),
%	next(and(eventually(at(package6city1_1)),
%	next(eventually(at,package6,city3_2))))))).


%goal 6
% package 3 & 6 have to be delivered at city1_2 but 6 is first

goal(and(until(not(at(package3,city1_2)),at(package6,city1_2)),
	eventually(always(at(package3,city1_2))))).


% package 3 & 6 have to be delivered at city2_2 but 6 is first

%goal 6a
%goal(and(until(not(at(package3,city2_2)),at(package6,city2_2)),eventually(always(at(package3,city2_2))))).


% package 3 & 6 have to be delivered at city2_2 but 6 is first
%goal 6b
%goal(and(until(not(or(at(package3,city2_2),at(package2,city2_2))),at(package6,city2_2)),eventually(always(and(at(package3,city2_2),at(package2,city2_2)))))).


% plane 2 must visit every airport
%goal 7
%goal(forall(a,or(not(airport(a)),eventually(at(plane2,a))))).

  

% plane 2 must visit every airport and package 6 gets delivered at city1_1
% goal 7a
%goal(and(forall(a,or(not(airport(a)),eventually(at(plane2,a)))),eventually(always(at(package6,city1_1))))).

% plane 2 must visit every airport & return (careful with the control!)

% goal 8
%goal(and(forall(a,or(not(airport(a)),eventually(at(plane2,a)))),eventually(always(at(plane2,city4_2))))).


% every airport is visited by at least one airplane (careful with the control!)

% goal 9
%goal(forall(a,or(not(airport(a)),eventually(exists(p,and(airplane(p),at(p,a))))))).



% every airport is visited by at least one airplane (careful with the control!)
% and package 6 gets delivered to city3_1

% goal 10
%goal(and(forall(a,or(not(airport(a)),eventually(exists(p,and(airplane(p),at(p,a)))))),
%	eventually(always(at(package6,city3_1))))).


%% again, touring package6, but now order doesn't matter
% goal 11
%goal(and(eventually(at(package6,city2_2)),and(eventually(at(package6,city1_2)),eventually(at(package6,city3_2))))).

% goal 12
%goal(and(eventually(at(package6,city2_1)),and(eventually(at(package6,city1_1)),eventually(at(package6,city3_1))))).


% goal 12a
%goal(and(eventually(at(package6,city2_1)),and(eventually(at(package6,city1_1)),always(eventually(at(package6,city3_2)))))).


%% package 6 must have eventually been everywhere
% goal13

%goal(forall(l,or(not(location(l)),eventually(at(package6,l)))))


%% package 6 must have eventually been everywhere in city 3

% goal14
%goal(forall(l,or(not(in_city(l,city3)),eventually(at(package6,l))))).


%% package 6 must have eventually been everywhere in city 4

%goal(forall(l,or(not(in_city(l,city4)),eventually(at(package6,l))))).


%% eventually all objects in city3_1 and then eventually all in city2_1

% goal16
%goal(eventually(and(forall(o,or(not(obj(o)),at(o,city3_1))), 
%	eventually(forall(o,or(not(obj(o)),at(o,city2_1))))))).


%% eventually all planes have been at the same airport and
%% package 6 gets delivered to city3_2

% goal17
%goal(and(exists(a,and(airport(a),and(not(equal(am,city4_2)),
%	eventually(forall(p,or(not(airplane(p)),at(p,a))))))),
%   eventually(always(at(package6,city3_2))))).


% every airport gets an airplane
% goal18
%goal(and(forall(a,or(not(airport(a)),eventually(exists(p,and(airplane(p),at(p,a)))))),
%	eventually(always(at(package6,city3_1))))).


% all airports are visited by an airplane at list during two instants of time

% goal19
%goal(and(forall(a,or(not(airport(a)),eventually(and(exists(p1,and(airplane(p1),at(p1,a))),
%	     next(eventually(exists(p2),and(airplane(p2),at(p2,a)))))))),
%     eventually(always(at(package6,city3_1))))).

