% eg.predator.pl
% July 8, 1996
% John Eikenberry
%
% Dec 13, 2035
% Douglas Miles
%
/** <module> 
% This is a *very* simple example of an agent for
% the predator example world.
%
*/

% Declare the module name and the exported (public) predicates.
:- module(predator,[]).

% Predicates asserted during run.
% :- dynamic memory/2.

% Possible agent actions.
:- include(logicmoo('vworld/vworld_header.pl')).
:- register_module_type(planning).

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
	look_feet(Agent,What),
	mud_isa(What,corpse).
predator_idea(Agent,move(Dir)) :-
	look_percepts(Agent,List),
	list_object_dir_visible(List,corpse(_),Dir).
predator_idea(Agent,attack(Dir)) :-
	look_near(Agent,List),
	list_object_dir_near(List,prey(_),Dir).

% find something near and itnersting and go to it.. or find a dirrection and go that way.. or sit 

predator_idea(Agent,move(Dir)) :-
	look_percepts(Agent,List),
	list_object_dir_visible(List,prey(_),Dir).
predator_idea(Agent,move(Dir)) :-
	memory(Agent,directions([Dir|_])),
	num_near(Num,Dir,here),
	look_near(Agent,List),
	nth_member(Num,What,List),
	(What == [];
	    What == [nut]).
predator_idea(Agent,sit) :-
	del(memory(Agent,directions(Old))),
	random_permutation(Old,New),
	add(memory(Agent,directions(New))).

:- include(logicmoo('vworld/vworld_footer.pl')).
