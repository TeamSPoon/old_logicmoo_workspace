constant(X):- member(X,[package6,package5,package4,package3,package2,package1,
		     city4,city3,city2,city1,truck4,
		     truck3,truck2,truck1,plane2,plane1,city4_1,
		     city3_1,city2_1,city1_1,city4_2,city3_2,
		      city2_2,city1_2]).

initially_true(obj(X)) :-
	member(X,[package6,package5,package4,package3,package2,package1]).

initially_true(city(X)) :-
	member(X,[city4,city3,city2,city1]).

initially_true(truck(X)) :-
	member(X,[truck4,truck3,truck2,truck1]).

initially_true(airplane(X)) :-
	member(X,[plane1,plane2]).

initially_true(location(X)) :-
	member(X,[city4_1,city3_1,city2_1,
		  city1_1,city4_2,city3_2,
		  city2_2,city1_2]).

initially_true(airport(X)) :-
	member(X,[city4_2,city3_2,city2_2,city1_2]).

initially_true(in_city(X,Y)) :-
	member([X,Y],[[city4_2,city4],[city4_1,city4],
		  [city3_2,city3],[city3_1,city3],[city2_2,city2],
		  [city2_1,city2],[city1_2,city1],[city1_1,city1]]).

initially_true(at(X,Y)) :-
	member([X,Y],[[plane2,city4_2],[plane1,city4_2],
		      [truck4,city4_1],[truck3,city3_1],
		      [truck2,city2_1],[truck1,city1_1],[package6,city3_1],
		      [package5,city4_2],[package4,city1_1],[package3,city1_1],
		      [package2,city1_2],[package1,city2_1]]).

