/** <module> 
% This is optional (simple) monster to prowl the maze world.
%
% monster.pl
% July 11, 1996
% John Eikenberry
%
% Dec 13, 2035
% Douglas Miles
%
% Declare the module name and the exported (public) predicates.
*/
:-swi_module(tMonster,[]).

% Predicates asserted during run.
% :- dynamic memory/2. 
% :- dynamic  agent_list/1.

% Possible agent actions.
:- include(logicmoo(vworld/moo_header)).
:- register_module_type(planning).

:-decl_type(tMonster).

world_agent_plan(_World,Agent,Act):-
   mudIsa(Agent,tMonster),
   monster_idea(Agent,Act).
   
monster_idea(Agent,actEat(Food)) :-
	mudCharge(Agent,Charge),
	Charge < 100,
        mudPossess(Agent, Food),
        isa_any(Food,[tFood,corpse]).
monster_idea(Agent,actTake(Food)) :-
	get_feet(Agent,What),
	isa_any(Food,[tFood,corpse]),
	obj_memb(Food,What).
monster_idea(Agent,actMove(Dir)) :-
	get_percepts(Agent,List),
	isa_any(Food,[tFood,corpse]),
	list_object_dir_sensed(_,List,Food,Dir).
monster_idea(Agent,actAttack(Dir)) :-
	get_near(Agent,List),
	list_agents(Agents),
	isa_any(NearAgt,Agents),
	list_object_dir_near(List,NearAgt,Dir).
monster_idea(Agent,actMove(Dir)) :-
	get_percepts(Agent,List),
	list_agents(Agents),
	isa_any(NearAgt,Agents),
	list_object_dir_sensed(_,List,NearAgt,Dir).

monster_idea(Agent,Act) :- move_or_sit_memory_idea(Agent,Act,[corpse]).

default_inst_props(Instance,tMonster,[mudDescription(fmt("Very screy looking monster named ~w",[Instance])),wearsClothing(tToughHide),mudPossess(tToughHide)]).

:- include(logicmoo(vworld/moo_footer)).
