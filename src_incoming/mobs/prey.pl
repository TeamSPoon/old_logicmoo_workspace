/** <module> 
% This is a *very* simple example of an agent meant to be 
% used as prey (dead prey turns into food) in simple simulations.
%
% prey.pl
%
% July 8, 1996
% John Eikenberry
%
% Dec 13, 2035
% Douglas Miles
%
*/

% Declare the module name and the exported (public) predicates.
:- module(prey,[]).

:- include(logicmoo(vworld/moo_header)).
:- moo:register_module_type(planning).
:- moo:register_module_type(command).

% Predicates asserted during run.
% :- dynamic memory/2. 
%:- dynamic agent_list/1.

moo:world_agent_plan(_World,Self,Act):-
   mud_isa(Self,prey),
   prey_idea(Self,Act).
   
% Possible agent actions.
prey_idea(Self,move(Dir)) :-
	get_percepts(Self,List),
	list_agents(Agents),
	obj_memb(NearAgnt,Agents),
	list_object_dir_sensed(_,List,NearAgnt,OppDir),
	reverse_dir(OppDir,Dir),
	number_to_dir(Num,Dir,here),
	nth1(Num,List,What),
	What == [].
prey_idea(Self,take(nut)) :-
	get_feet(Self,What),
	member(nut,What).
prey_idea(Self,eat(nut)) :-
	charge(Self,Charge),
	Charge < 120,
	possess(Self,nut).
prey_idea(Self,move(Dir)) :-
	get_percepts(Self,List),
	list_object_dir_sensed(_,List,nut,Dir).
prey_idea(_Agent,_) :-
	spawn.

prey_idea(Agent,Act) :- move_or_sit_memory_idea(Agent,Act,[nut]).



% spawn new prey
% maybe(N) == N chance of each agent spawning a new agent each turn

moo:action_info(spawn(type)).
moo:action_info(rez(type)).

moo:agent_call_command(_Agent,spawn(prey)):-spawn.

spawn :-
	maybe(10),
	spawn_prey(1),
	!,
	fail.

% Doesn't actually create a new prey, but revives a dead one.
% This allows for absolute control of the max number of prey 
% which is important for performance considerations
% (too many prey slows the simulation to a crawl)
% 10 slows my P5-100 down a lot... 
spawn_prey(10) :-
	!.
spawn_prey(N) :-
       Prey = prey(N),
       findall_type_default_props(Prey,prey,Traits),
	\+ act_turn(Prey,_),
         max_charge(NRG),
         max_damage(Dam),
         clr(charge(Prey,_)),
         clr(damage(Prey,_)),
         add(charge(Prey,NRG)),
         add(damage(Prey,Dam)),
         add(act_turn(Prey,1)),
         clr(possess(Prey,_)),
	set_stats(Prey,Traits),
	put_in_world(Prey),
	!.
spawn_prey(N) :-
	Ntemp is N + 1,
	spawn_prey(Ntemp).


moo:agent_call_command(Agent,rez(NewType)):- atloc(Agent,LOC), create_instance(NewType,item,[atloc(LOC)]).


:- include(logicmoo(vworld/moo_footer)).


