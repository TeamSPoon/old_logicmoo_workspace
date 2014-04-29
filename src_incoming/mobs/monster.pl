% monster.pl
% July 11, 1996
% John Eikenberry
%
% Dec 13, 2035
% Douglas Miles
%
/** <module> 
% This is optional (simple) monster to prowl the maze world.
%
% Declare the module name and the exported (public) predicates.
*/
:- module(monster,[]).

% Predicates asserted during run.
% :- dynamic memory/2. 
% :- dynamic  agent_list/1.

% Possible agent actions.
:- include(logicmoo('vworld/vworld_header.pl')).
:- register_module_type(planning).

moo:world_agent_plan(_World,Agent,Act):-
   mud_isa(Agent,monster),
   monster_idea(Agent,Act).
   
monster_idea(Agent,eat(Food)) :-
	charge(Agent,Charge),
	Charge < 100,
	inventory(Agent, List),
	isa_any(Food,[food,corpse]),
	obj_memb(Food,List).
monster_idea(Agent,take(Food)) :-
	get_feet(Agent,What),
	isa_any(Food,[food,corpse]),
	obj_memb(Food,What).
monster_idea(Agent,move(Dir)) :-
	get_percepts(Agent,List),
	isa_any(Food,[food,corpse]),
	list_object_dir_sensed(_,List,Food,Dir).
monster_idea(Agent,attack(Dir)) :-
	get_near(Agent,List),
	list_agents(Agents),
	isa_any(NearAgt,Agents),
	list_object_dir_near(List,NearAgt,Dir).
monster_idea(Agent,move(Dir)) :-
	get_percepts(Agent,List),
	list_agents(Agents),
	isa_any(NearAgt,Agents),
	list_object_dir_sensed(_,List,NearAgt,Dir).
monster_idea(Agent,move(Dir)) :-
	memory(Agent,directions([Dir|_])),
	num_near(Num,Dir,here),
	get_near(Agent,List),
	nth_member(Num,What,List),
	What == [].
monster_idea(Agent,sit) :-
	del(memory(Agent,directions(Old))),
	random_permutation(Old,New),
	add(memory(Agent,directions(New))).

moo:type_default_props(Instance,monster, [description(Instance,SFmt),wearing(tough_hide),possess(tough_hide)]):-sformat(SFmt,"Very screy looking monster named ~w",[Instance]).



:- include(logicmoo('vworld/vworld_footer.pl')).



