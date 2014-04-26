% ===================================================================
% File 'logicmoo_util_strings.pl'
/* <module> Purpose: Common Logicmoo library Functions for Strings */
% Maintainers: Douglas Miles/Annie Ogborn/Kino Coursey
% Contact: $Author: dmiles $@users.sourceforge.net ;
% ===================================================================
%
% This is a *very* simple example of an agent for
% the vacuum cleaner example world.
%

% Declare the module name and the exported (public) predicates.
:- module(vacuum,[]).

% Predicates asserted during run.
% :- dynamic memory/2.

:- include(logicmoo('vworld/vworld_header.pl')).
:- register_module_type(planning).

% Possible agent actions.

moo:world_agent_plan(_World,Agent,Act):-
   mud_isa(Agent,vacuum),
   vacuum_idea(Agent,Act).

vacuum_idea(Agent,take(outlet)) :-
	charge(Agent,Charge),
	Charge < 490,
	look_feet(Agent,What),
	member(outlet,What).
vacuum_idea(Agent,take(dirt)) :-
	look_feet(Agent,What),
	member(dirt,What).
vacuum_idea(Agent,move(Dir)) :-
	charge(Agent,Charge),
	Charge < 200,
	look_percepts(Agent,List),
	list_object_dir_visible(List,outlet,Dir),
	number_to_dir(N,Dir,here),
	nth_member(N,What,List),
	(What == [];
	    What == [dirt];
	    What == [outlet]).
vacuum_idea(Agent,climb(Dir)) :-
	charge(Agent,Charge),
	Charge < 200,
	look_percepts(Agent,List),
	list_object_dir_visible(List,outlet,Dir),
	number_to_dir(N,Dir,here),
	nth_member(N,What,List),
	(What == [low_box];
	    What == [low_wall]).
vacuum_idea(Agent,move(Dir)) :-
	look_percepts(Agent,List),
	list_object_dir_visible(List,dirt,Dir),
	number_to_dir(N,Dir,here),
	nth_member(N,What,List),
	(What == [];
	What == [dirt];
	What == [outlet]).
vacuum_idea(Agent,climb(Dir)) :-
	look_percepts(Agent,List),
	list_object_dir_visible(List,dirt,Dir),
	number_to_dir(N,Dir,here),
	nth_member(N,What,List),
	(What == [low_box];
	    What == [low_wall]).
vacuum_idea(Agent,move(Dir)) :-
	memory(Agent,directions([Dir|_])),
	num_near(Num,Dir,here),
	look_near(Agent,List),
	nth_member(Num,What,List),
	(What == [];
	What == [outlet]).
vacuum_idea(Agent,sit) :-
	del(memory(Agent,directions(Old))),
	random_permutation(Old,New),
	add(memory(Agent,directions(New))).





:- include(logicmoo('vworld/vworld_footer.pl')).


