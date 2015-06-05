%:- module(goalgen,_).
:- module(goalgen,[find_new_goals/1,find_depth_bound/1]).
:- use_module(continuous_pop,[current_perceptions/1]).

:- data goals/1.

%TODO find_new_goals, find_depth_bound
%goals for blocks
%goals([on(c,d),on(d,b)]).

%goals for soccer
%goals([carrying(kula,ball)]).
%goals([waiting_at(ball,oppGoal)]).
goals([waiting_at(kula,cell(20,14))]).
goals([carrying(kula,ball)]).
goals([waiting_at(kula,cell(9,12)),carrying(kula,ball)]).
goals([waiting_at(kula,cell(5,12)),carrying(kula,ball)]).
goals([waiting_at(kula,cell(9,12)),carrying(kula,ball)]).
goals([waiting_at(ball,oppGoal)]).

%goals([waiting_at(kula,cell(1,3))]).

%goals([on(d,a)]).

find_new_goals(Goal):-
	retract_fact(goals(Goal)).
%	goals(Goal).


% find_new_goals(Goal):-
% 	current_perceptions(P),
% 	hasball(P),
% 	retract_fact(goals(Goal)).

% hasball(P) :-
% 	member(carrying(_,_),P).

%TODO
find_depth_bound(6).

	

%find_depth_bound(7).

