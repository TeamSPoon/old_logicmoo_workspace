/** <module> 
% This is simple example explorer for the maze world.
%
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



% Declare the module name and the exported (public) predicates.
:- module(explorer,[]).

:- include(logicmoo(vworld/moo_header)).
:- moo:register_module_type(planning).


vette_idea(Agent,Act,Act):-var(Act),!,dmsg(vette_idea(Agent,Act)).
vette_idea(_,sit,sit):-!.
vette_idea(Agent,Act,Act):-dmsg(vette_idea(Agent,Act)).

% dyn:label_type_props(explorer(ID),explorer,[]).

moo:world_agent_plan(_World,Agent,ActV):-
   agent(Agent),
  % mud_isa(Agent,explorer),
   explorer_idea(Agent,Act),
   vette_idea(Agent,Act,ActV).

% Possible agent actions.
explorer_idea(Agent,eat(Elixer)) :-
	damage(Agent,Damage),
	Damage < 15,
   inventory(Agent,List),
   obj_memb(Elixer,List),
   mud_isa(Elixer,elixer).

explorer_idea(Agent,eat(food)) :-
	charge(Agent,Charge),
	Charge < 150,
   inventory(Agent,List),
   obj_memb(Food,List),
   mud_isa(Food,food).

explorer_idea(Agent,take(Good)) :-
	get_feet(Agent,What),
        obj_memb(Good,What),
	isa_any(Good,[gold,elixer]).  

explorer_idea(Agent,take(food)) :-
	get_feet(Agent,What),
        obj_memb(Good,What),
	isa_any(Good,[food]).

explorer_idea(Agent,move(Dir)) :-
	get_percepts(Agent,List),
	list_object_dir_sensed(_,List,food,Dir).

explorer_idea(Agent,move(Dir)) :-
	get_percepts(Agent,List),
	list_object_dir_sensed(_,List,gold,Dir).

explorer_idea(Agent,move(Dir)) :-
	get_percepts(Agent,List),
	list_object_dir_sensed(_,List,monster,OppDir),
	reverse_dir(OppDir,Dir),
	number_to_dir(N,Dir,here),
        nth1(N,List,What),
	What == [].

explorer_idea(Agent,move(Dir)) :-
	memory(Agent,directions([Dir|_])),
	num_near(Num,Dir,here),
	get_near(Agent,List),
	nth1(Num,List,What),
	What == [].

explorer_idea(Agent,attack(Dir)) :-
	get_near(Agent,List),
	list_object_dir_near(List,monster(_),Dir).

explorer_idea(Agent,sit) :-
        req(memory(Agent,directions(Old))),
	del(memory(Agent,directions(Old))),
	random_permutation(Old,New),
	add(memory(Agent,directions(New))).


:- include(logicmoo(vworld/moo_footer)).


