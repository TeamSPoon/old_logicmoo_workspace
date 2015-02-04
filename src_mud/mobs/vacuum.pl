/** <module> 
% This is a *very* simple example of an agent for
% the vacuum cleaner example world.
%
% Maintainers: Douglas Miles/Annie Ogborn/Kino Coursey
% Contact: $Author: dmiles $@users.sourceforge.net ;
*/

% Declare the module name and the exported (public) predicates.
:-swi_module(tVacuum,[]).

% Predicates asserted during run.
% :- dynamic memory/2.

:- include(logicmoo(vworld/moo_header)).
:- register_module_type(planning).

% Possible agent actions.

:-decl_type(tVacuum).
tCol(tVacuum).

user:world_agent_plan(_World,Agent,Act):-
   mudIsa(Agent,tVacuum),
   vacuum_idea(Agent,Act).

vacuum_idea(Agent,actTake(tOutlet)) :-
	mudEnergy(Agent,Charge),
	Charge < 490,
	mudNearFeet(Agent,What),
	member(tOutlet,What).
vacuum_idea(Agent,actTake(tDirt)) :-
	mudNearFeet(Agent,What),
	member(tDirt,What).
vacuum_idea(Agent,actMove(Dir)) :-
	mudEnergy(Agent,Charge),
	Charge < 200,
	mudGetPrecepts(Agent,List),
	list_object_dir_sensed(_,List,tOutlet,Dir),
	number_to_dir(N,Dir,vHere),
	nth1(N,List,What),
	(What == [];
	    What == [tDirt];
	    What == [tOutlet]).
vacuum_idea(Agent,actClimb(Dir)) :-
	mudEnergy(Agent,Charge),
	Charge < 200,
	mudGetPrecepts(Agent,List),
	list_object_dir_sensed(_,List,tOutlet,Dir),
	number_to_dir(N,Dir,vHere),
	nth1(N,List,What),
	(What == [tLowBox];
	    What == [tLowWall]).
vacuum_idea(Agent,actMove(Dir)) :-
	mudGetPrecepts(Agent,List),
	list_object_dir_sensed(_,List,tDirt,Dir),
	number_to_dir(N,Dir,vHere),
	nth1(N,List,What),
	(What == [];
	What == [tDirt];
	What == [tOutlet]).
vacuum_idea(Agent,actClimb(Dir)) :-
	mudGetPrecepts(Agent,List),
	list_object_dir_sensed(_,List,tDirt,Dir),
	number_to_dir(N,Dir,vHere),
	nth1(N,List,What),
	(What == [tLowBox];
	    What == [tLowWall]).

vacuum_idea(Agent,Act) :- move_or_sit_memory_idea(Agent,Act,[tOutlet]).





:- include(logicmoo(vworld/moo_footer)).
