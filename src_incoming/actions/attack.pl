% attack.pl
% June 18, 1996
% John Eikenberry
% Dec 13, 2035
% Douglas Miles
%
/** <module> 
% This file defines the agents action of attacking. 
% Comments below document the basic idea.
%
*/

% :- module(user). 
:- module(attack, []).

:- include(logicmoo(vworld/moo_header)).

:- moo:register_module_type(command).

% attack joe ->translates-> attack nw
moo:actiontype(attack(dir)).

% Attack
% Successful Attack
moo:agent_call_command(Agent,attack(Dir)) :-	
	atloc(Agent,LOC),
	move_dir_target(LOC,Dir,XXYY),
	atloc(What,XXYY),
	damage_foe(Agent,What,hit),
	call_update_charge(Agent,attack).

% Destroy small objects (food, etc.)
moo:agent_call_command(Agent,attack(Dir)) :-	
	atloc(Agent,LOC),
	move_dir_target(LOC,Dir,XXYY),
	atloc(What,XXYY),	
	props(What,weight(1)),
	destroy_object(XXYY,What),
	call_update_charge(Agent,attack).

% Hit a big object... causes damage to agent attacking
moo:agent_call_command(Agent,attack(Dir)) :-	
	atloc(Agent,LOC),
	move_dir_target(LOC,Dir,XXYY),
	atloc(What,XXYY),	
	What \== 0,
	props(What,weight(_)),
	call_update_stats(Agent,bash),
	call_update_charge(Agent,attack).

% Hit nothing (empty space)... causes a little damage
moo:agent_call_command(Agent,attack(Dir)) :-	
	atloc(Agent,LOC),
	move_dir_target(LOC,Dir,XXYY),
	not(atloc(_,XXYY)),
	call_update_stats(Agent,wiff),
	call_update_charge(Agent,attack).

% Check to see if agent being attacked is carrying an 
% object which provides defence
check_for_defence(Agent,Def) :-
	findall(Poss,possess(Agent,Poss),Inv),
	member(Obj,Inv),
	props(Obj,act_affect(_,defence(Def))).
check_for_defence(_,0).

% Check to see if attacking agent has a weapon
check_for_weapon(Agent,Wpn) :-
	findall(Poss,possess(Agent,Poss),Inv),
        member(Obj,Inv),
        props(Obj,act_affect(_,attack(Wpn))).

check_for_weapon(_,0).

destroy_object(LOC,What) :-
	del(atloc(What,LOC)).

% Does damage to other agent
damage_foe(Agent,What,hit) :-
	del(damage(What,OldDam)),
	str(Agent,Str),
	check_for_defence(What,Def),
	BaseAtk is Str * 2,
	check_for_weapon(Agent,Wpn),
	Atk is (Wpn + BaseAtk),
	NewDam is (OldDam - (Atk - Def)),
	add(damage(What,NewDam)).

% Record keeping
moo:update_charge(Agent,attack) :- uprop(charge(Agent,-5)).
moo:update_stats(Agent,bash) :-  uprop(damage(Agent,-2)),
	(add_cmdfailure(Agent,bash)).
moo:update_stats(Agent,wiff) :- 
	del(damage(Agent,Old)),
	New is Old - 1,
	add(damage(Agent,New)),
	(add_cmdfailure(Agent,bash)).



:- include(logicmoo(vworld/moo_footer)).


