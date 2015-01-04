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
:-swi_module(tPredator,[]).

% Predicates asserted during run.
% :- dynamic memory/2.

% Possible agent actions.
:- include(logicmoo(vworld/moo_header)).
:- register_module_type(planning).

:-decl_type(tPredator).
tCol(tPredator).

world_agent_plan(_World,Agent,Act):-
   mudIsa(Agent,tPredator),
   predator_idea(Agent,Act).

predator_idea(Agent,actEat(Corpse)) :-
	mudCharge(Agent,Charge),
	Charge < 100,
	actInventory(Agent, List),                
	obj_memb(Corpse,List),
        mudIsa(Corpse,tCorpse).
predator_idea(Agent,actTake(What)) :-
	mudNearFeet(Agent,What),
	mudIsa(What,tCorpse).
predator_idea(Agent,actMove(Dir)) :-
	mudGetPrecepts(Agent,List),
	list_object_dir_sensed(_,List,iCorpseFn(_),Dir).
predator_idea(Agent,actAttack(Dir)) :-
	mudNearReach(Agent,List),
	list_object_dir_near(List,tPrey(_),Dir).

% find something near and itnersting and go to it.. or find a dirrection and go that way.. or sit 

predator_idea(Agent,actMove(Dir)) :-
	mudGetPrecepts(Agent,List),
	list_object_dir_sensed(_,List,tPrey(_),Dir).

predator_idea(Agent,Act) :- 
      move_or_sit_memory_idea(Agent,Act,[tNut]).


:- include(logicmoo(vworld/moo_footer)).
