% move.pl
% May 18, 1996
% John Eikenberry
%
% Dec 13, 2035
% Douglas Miles
%
/** <module> 
% This file defines the predicates for the agent to move
%
*/

:- module(move, []).

:- include(logicmoo('vworld/vworld_header.pl')).

:- register_module_type(command).
   

moo:agent_call_command(Agnt,Cmd):- functor(Cmd,move,_),!,
   must(move_command(Agnt,Cmd)).

moo:decl_action(move(dir)).

% Move thy agent

% cant get anywhere since the map fails it
move_command(Agent,move(Dir)) :-
	    atloc(Agent,LOC),
        not(move_dir_target(LOC,Dir,_)),!,
		add(failure(Agent,move)).

% Run into something big, Ouch...
% damage and charge... plus it doesn't get anywhere
move_command(Agent,move(Dir)) :-
	atloc(Agent,LOC),
        move_dir_target(LOC,Dir,XXYY),
        is_3d(XXYY),
         atloc(Obj,LOC),        
         prop_or(Obj,height,ObjHt,1),
         atloc(Obj2,XXYY),
         prop_or(Obj2,height,ObjHt2,1),
         ObjHt2 > ObjHt,
         ObjHt2 > 1,
	!,
	moo:update_stats(Agent,collide),
	moo:update_charge(Agent,move).

% Another Agent is in the way
move_command(Agent,move(Dir)) :- 
	atloc(Agent,LOC),
	move_dir_target(LOC,Dir,XXYY),
	is_3d(XXYY),
        atloc(Agent2,XXYY),
	mud_isa(Agent2,agent),
	moo:update_stats(Agent,collide),
	moo:update_charge(Agent,move).

%Move successfully
move_command(Agent,move(Dir)) :-
	in_world_move(_,Agent,Dir),
	moo:update_charge(Agent,move).

%Record keeping

moo:decl_update_charge(Agent,move) :- padd(Agent,charge,-4).

moo:decl_update_stats(Agent,collide) :- padd(Agent,damage,-5),add(failure(Agent,collide)).

moo:decl_update_stats(Agent,fall) :- padd(Agent,damage,-10).

% cheating but to test

moo:decl_action(go(dir)).
moo:agent_call_command(Agent,go(Dir)) :-
	atloc(Agent,LOC),
	move_dir_target(LOC,Dir,XXYY),
        del(atloc(Agent,LOC)),
        add(atloc(Agent,XXYY)),
	moo:update_stats(Agent,collide),
	moo:update_charge(Agent,move).


:- include(logicmoo('vworld/vworld_footer.pl')).


