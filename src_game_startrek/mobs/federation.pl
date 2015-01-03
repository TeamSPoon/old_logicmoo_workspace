/** <module> 
% This is simple example federation for the maze world.
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
:-swi_module(federation,[]).

:- include(logicmoo(vworld/moo_header)).
:- register_module_type(planning).

:-decl_type(federation).

vette_fed_idea(Agent,Act,Act):-var(Act),!,dmsg(vette_fed_idea(Agent,Act)).
vette_fed_idea(_,sit,sit):-!.
vette_fed_idea(Agent,Act,Act):-dmsg(vette_fed_idea(Agent,Act)).

label_type_props('Px',federation,[]).

world_agent_plan(_World,Agent,ActV):-
   agent(Agent),
  % isa(Agent,federation),
   federation_idea(Agent,Act),
   vette_fed_idea(Agent,Act,ActV).

% Possible agent actions.
federation_idea(Agent,eat(Elixer)) :-
	health(Agent,Damage),
	Damage < 15,
   inventory(Agent,List),
   obj_memb(Elixer,List),
   isa(Elixer,elixer).

federation_idea(Agent,eat(food)) :-
	charge(Agent,Charge),
	Charge < 150,
   inventory(Agent,List),
   obj_memb(Food,List),
   isa(Food,food).

federation_idea(Agent,take(Good)) :-
	get_feet(Agent,What),
        obj_memb(Good,What),
	isa_any(Good,[gold,elixer,treasure]).  

federation_idea(Agent,take(Good)) :-
	get_feet(Agent,What),
        obj_memb(Good,What),
	isa_any(Good,[food,usefull,item]).

federation_idea(Agent,move(1,Dir)) :-
	get_percepts(Agent,List),
	list_object_dir_sensed(_,List,treasure,Dir).

federation_idea(Agent,move(3,Dir)) :-
	get_percepts(Agent,List),
	list_object_dir_sensed(_,List,monster,OppDir),
	reverse_dir(OppDir,Dir),
	number_to_dir(N,Dir,here),
        nth1(N,List,What),
	What == [].

federation_idea(Agent,move(1,Dir)) :-
	get_percepts(Agent,List),
	list_object_dir_sensed(_,List,usefull,Dir).

federation_idea(Agent,move(1,Dir)) :-
	get_percepts(Agent,List),
	list_object_dir_sensed(_,List,agent,Dir).

federation_idea(Agent,move(5,Dir)) :-
	memory(Agent,directions([Dir|_])),
	num_near(Num,Dir,here),
	get_near(Agent,List),
	nth1(Num,List,What),
	What == [].

federation_idea(Agent,attack(Dir)) :-
	get_near(Agent,List),
	list_object_dir_near(List,monster(_),Dir).

federation_idea(Agent,look) :-
        req(memory(Agent,directions(Old))),
	del(memory(Agent,directions(Old))),
	random_permutation(Old,New),
	add(memory(Agent,directions(New))).


:- include(logicmoo(vworld/moo_footer)).


