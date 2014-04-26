% eg.explorer.pl
% July 11, 1996
% John Eikenberry
%
% Dec 13, 2035
% Douglas Miles
%
/** <module> 
% This is simple example explorer for the maze world.
%
*/

% Declare the module name and the exported (public) predicates.
:- module(explorer,[]).

:- include(logicmoo('vworld/vworld_header.pl')).
:- register_module_type(planning).

moo:label_type_props(explorer(_),explorer,[]).

moo:world_agent_plan(_World,Agent,Act):-
   mud_isa(Agent,explorer),
   explorer_idea(Agent,Act).

% Possible agent actions.
explorer_idea(Agent,eat(elixer)) :-
	damage(Agent,Damage),
	Damage < 15,
	inventory(Agent,List),
	obj_memb(elixer,List).
explorer_idea(Agent,eat(food)) :-
	charge(Agent,Charge),
	Charge < 150,
	inventory(Agent,List),
	obj_memb(food,List).
explorer_idea(Agent,take(Good)) :-
	look_feet(Agent,What),
	isa_any(Good,[gold,elixer]),
	obj_memb(Good,What).
explorer_idea(Agent,take(food)) :-
	look_feet(Agent,What),
	obj_memb(food,What).
explorer_idea(Agent,move(Dir)) :-
	look_percepts(Agent,List),
	list_object_dir_visible(List,food,Dir).
explorer_idea(Agent,move(Dir)) :-
	look_percepts(Agent,List),
	list_object_dir_visible(List,gold,Dir).
explorer_idea(Agent,move(Dir)) :-
	look_percepts(Agent,List),
	list_object_dir_visible(List,monster(_),OppDir),
	reverse_dir(OppDir,Dir),
	number_to_dir(N,Dir,here),
	nth_member(N,What,List),
	What == [].
explorer_idea(Agent,move(Dir)) :-
	memory(Agent,directions([Dir|_])),
	num_near(Num,Dir,here),
	look_near(Agent,List),
	nth_member(Num,What,List),
	What == [].
explorer_idea(Agent,attack(Dir)) :-
	look_near(Agent,List),
	list_object_dir_near(List,monster(_),Dir).
explorer_idea(Agent,sit) :-
	del(memory(Agent,directions(Old))),
	random_permutation(Old,New),
	add(memory(Agent,directions(New))).



:- include(logicmoo('vworld/vworld_footer.pl')).


