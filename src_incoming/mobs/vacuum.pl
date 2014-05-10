/** <module> 
% This is a *very* simple example of an agent for
% the vacuum cleaner example world.
%
% Maintainers: Douglas Miles/Annie Ogborn/Kino Coursey
% Contact: $Author: dmiles $@users.sourceforge.net ;
*/

% Declare the module name and the exported (public) predicates.
:- module(vacuum,[]).

% Predicates asserted during run.
% :- dynamic memory/2.

:- include(logicmoo('vworld/moo_header.pl')).
:- register_module_type(planning).

% Possible agent actions.

moo:world_agent_plan(_World,Agent,Act):-
   mud_isa(Agent,vacuum),
   vacuum_idea(Agent,Act).

vacuum_idea(Agent,take(outlet)) :-
	charge(Agent,Charge),
	Charge < 490,
	get_feet(Agent,What),
	member(outlet,What).
vacuum_idea(Agent,take(dirt)) :-
	get_feet(Agent,What),
	member(dirt,What).
vacuum_idea(Agent,move(Dir)) :-
	charge(Agent,Charge),
	Charge < 200,
	get_percepts(Agent,List),
	list_object_dir_sensed(_,List,outlet,Dir),
	number_to_dir(N,Dir,here),
	nth1(N,List,What),
	(What == [];
	    What == [dirt];
	    What == [outlet]).
vacuum_idea(Agent,climb(Dir)) :-
	charge(Agent,Charge),
	Charge < 200,
	get_percepts(Agent,List),
	list_object_dir_sensed(_,List,outlet,Dir),
	number_to_dir(N,Dir,here),
	nth1(N,List,What),
	(What == [low_box];
	    What == [low_wall]).
vacuum_idea(Agent,move(Dir)) :-
	get_percepts(Agent,List),
	list_object_dir_sensed(_,List,dirt,Dir),
	number_to_dir(N,Dir,here),
	nth1(N,List,What),
	(What == [];
	What == [dirt];
	What == [outlet]).
vacuum_idea(Agent,climb(Dir)) :-
	get_percepts(Agent,List),
	list_object_dir_sensed(_,List,dirt,Dir),
	number_to_dir(N,Dir,here),
	nth1(N,List,What),
	(What == [low_box];
	    What == [low_wall]).

vacuum_idea(Agent,Act) :- move_or_sit_memory_idea(Agent,Act,[outlet]).





:- include(logicmoo('vworld/moo_footer.pl')).


