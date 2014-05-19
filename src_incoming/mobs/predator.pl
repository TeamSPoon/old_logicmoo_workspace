/** <module> 
% This is a *very* simple example of an agent for
% the predator example world.
%
% eg.predator.pl
% July 8, 1996
% John Eikenberry
%
% Dec 13, 2035
% Douglas Miles
%
*/

% Declare the module name and the exported (public) predicates.
:- module(predator,[]).

% Predicates asserted during run.
% :- dynamic memory/2.

% Possible agent actions.
:- include(logicmoo('vworld/moo_header.pl')).
:- moo:register_module_type(planning).

moo:world_agent_plan(_World,Agent,Act):-
   mud_isa(Agent,predator),
   predator_idea(Agent,Act).

predator_idea(Agent,eat(Corpse)) :-
	charge(Agent,Charge),
	Charge < 100,
	inventory(Agent, List),                
	obj_memb(Corpse,List),
        mud_isa(Corpse,corpse).
predator_idea(Agent,take(What)) :-
	get_feet(Agent,What),
	mud_isa(What,corpse).
predator_idea(Agent,move(Dir)) :-
	get_percepts(Agent,List),
	list_object_dir_sensed(_,List,corpse(_),Dir).
predator_idea(Agent,attack(Dir)) :-
	get_near(Agent,List),
	list_object_dir_near(List,prey(_),Dir).

% find something near and itnersting and go to it.. or find a dirrection and go that way.. or sit 

predator_idea(Agent,move(Dir)) :-
	get_percepts(Agent,List),
	list_object_dir_sensed(_,List,prey(_),Dir).

predator_idea(Agent,Act) :- 
      move_or_sit_memory_idea(Agent,Act,[nut]).


:- include(logicmoo('vworld/moo_footer.pl')).
