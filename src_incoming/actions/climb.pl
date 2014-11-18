/** <module>
% climb.pl
% June 18, 1996
% John Eikenberry
%
% This file defines the agents action of climbing. 
% Comments below document the basic idea.
%
% Dec 13, 2035
% Douglas Miles
*/
% :- module(user). 
:- module(climb, []).

:- include(logicmoo(vworld/moo_header)).

:- moo:register_module_type(command).

can_move_into(_LOC,XXYY):-var(XXYY),!,fail.
can_move_into(_LOC,XXYY):-not(atloc(_,XXYY)),!.
can_move_into(_LOC,XXYY):-ground(XXYY).

moo:actiontype(climb(dir)).

% Climb
% If there is nothing there to climb, move to location
% plus take some damage and loose charge 
moo:agent_call_command(Agent,climb(Dir)) :-	
	atloc(Agent,LOC),
	move_dir_target(LOC,Dir,XXYY),
	can_move_into(LOC,XXYY),
	in_world_move(_,Agent,Dir),
	call_update_stats(Agent,trip),
	call_update_charge(Agent,climb).
% Object is too high to climb, or it is another agent. 
moo:agent_call_command(Agent,climb(Dir)) :-
	
	\+ climbable(Agent,Dir),
	call_update_stats(Agent,pulled),
	call_update_charge(Agent,climb).
% Successful climb
moo:agent_call_command(Agent,climb(Dir)) :-	
	in_world_move(_,Agent,Dir),
	call_update_charge(Agent,climb).

% Test to see if agent can climb the object
climbable(Agent,Dir) :-
	atloc(Agent,LOC),
	move_dir_target(LOC,Dir,XXYY),
	atloc(Obj,XXYY),
	props(Obj,height(ObjHt)), % If object is an agent, it will fail at this point
	height_on_obj(Agent,AgHt),
	atloc(Obj2,LOC), prop_or(Obj2,height,0,Obj2Ht),
	ObjHt =< (AgHt + Obj2Ht),
	ObjHt > 1.

%Record keeping
moo:update_charge(Agent,climb) :-
	del(charge(Agent,Old)),
	New is Old - 5,
	add(charge(Agent,New)).

moo:update_stats(Agent,trip) :- 
        del(health(Agent,Old)),
	New is Old - 3,
	add(health(Agent,New)).

moo:update_stats(Agent,pulled) :- 
        del(health(Agent,Old)),
	New is Old - 2,
	add(health(Agent,New)),
	(add_cmdfailure(Agent,pulled)).


:- include(logicmoo(vworld/moo_footer)).


