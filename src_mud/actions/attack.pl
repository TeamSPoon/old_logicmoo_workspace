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

% :-swi_module(user). 
:-swi_module(modAttack, []).

:- include(prologmud(mud_header)).

% :- register_module_type (mtCommand).

% attack joe ->translates-> attack nw
vtActionTemplate(actAttack(vtDirection)).

% Attack
% Successful Attack
user:agent_call_command(Agent,actAttack(Dir)) :-	
	from_dir_target(Agent,Dir,XXYY),
	mudAtLoc(What,XXYY),
	damage_foe(Agent,What,hit),
	call_update_charge(Agent,actAttack).

% Destroy small objects (food, etc.)
user:agent_call_command(Agent,actAttack(Dir)) :-	
	from_dir_target(Agent,Dir,XXYY),
	mudAtLoc(What,XXYY),	
	props(What,mudWeight(1)),
	destroy_object(XXYY,What),
	call_update_charge(Agent,actAttack).

% Hit a big object... causes damage to agent attacking
user:agent_call_command(Agent,actAttack(Dir)) :-	
	from_dir_target(Agent,Dir,XXYY),
	mudAtLoc(What,XXYY),
	props(What,mudWeight(_)),
	call_update_stats(Agent,bash),
	call_update_charge(Agent,actAttack).

% Hit nothing (empty space)... causes a little damage
user:agent_call_command(Agent,actAttack(Dir)) :-
	from_dir_target(Agent,Dir,XXYY),
	not(mudAtLoc(_,XXYY)),
	call_update_stats(Agent,wiff),
	call_update_charge(Agent,actAttack).

% Check to see if agent being attacked is carrying an 
% object which provides defence
check_for_defence(Agent,Def) :-
	findall(Poss,mudPossess(Agent,Poss),Inv),
	member(Obj,Inv),
	props(Obj,mudActAffect(_,mudArmor(Def))).
check_for_defence(_,0).

% Check to see if attacking agent has a weapon
check_for_weapon(Agent,Wpn) :-
	findall(Poss,mudPossess(Agent,Poss),Inv),
        member(Obj,Inv),
        props(Obj,mudActAffect(_,mudAttack(Wpn))).

check_for_weapon(_,0).

destroy_object(LOC,What) :-
	del(mudAtLoc(What,LOC)).

% Does damage to other agent
damage_foe(Agent,What,hit) :-
	del(mudHealth(What,OldDam)),
	mudStr(Agent,Str),
	check_for_defence(What,Def),
	BaseAtk is Str * 2,
	check_for_weapon(Agent,Wpn),
	Atk is (Wpn + BaseAtk),
	NewDam is (OldDam - (Atk - Def)),
	add(mudHealth(What,NewDam)).

% Record keeping
update_charge(Agent,actAttack) :- upprop(Agent,mudEnergy(-5)).
update_stats(Agent,bash) :-  upprop(Agent,mudHealth(-2)),
	(add_cmdfailure(Agent,bash)).
update_stats(Agent,wiff) :- 
	del(mudHealth(Agent,Old)),
	New is Old - 1,
	add(mudHealth(Agent,New)),
	(add_cmdfailure(Agent,bash)).



:- include(prologmud(mud_footer)).
