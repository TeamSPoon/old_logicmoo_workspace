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
% :-swi_module(user). 
:-swi_module(modClimb, []).

:- include(logicmoo(vworld/moo_header)).

:- register_module_type(mtCommand).

can_move_into(_LOC,XXYY):-var(XXYY),!,fail.
can_move_into(_LOC,XXYY):-not(mudAtLoc(_,XXYY)),!.
can_move_into(_LOC,XXYY):-ground(XXYY).

vtActionTemplate(actClimb(vtDirection)).

% Climb
% If there is nothing there to climb, move to location
% plus take some damage and loose charge 
agent_call_command(Agent,actClimb(Dir)) :-	
	mudAtLoc(Agent,LOC),
	move_dir_target(LOC,Dir,XXYY),
	can_move_into(LOC,XXYY),
	in_world_move(_,Agent,Dir),
	call_update_stats(Agent,trip),
	call_update_charge(Agent,actClimb).
% Object is too high to climb, or it is another agent. 
agent_call_command(Agent,actClimb(Dir)) :-
	
	\+ climbable(Agent,Dir),
	call_update_stats(Agent,pulled),
	call_update_charge(Agent,actClimb).
% Successful climb
agent_call_command(Agent,actClimb(Dir)) :-	
	in_world_move(_,Agent,Dir),
	call_update_charge(Agent,actClimb).

% Test to see if agent can climb the object
climbable(Agent,Dir) :-
	mudAtLoc(Agent,LOC),
	move_dir_target(LOC,Dir,XXYY),
	mudAtLoc(Obj,XXYY),
	props(Obj,mudHeight(ObjHt)), % If object is an agent, it will fail at this point
	mudHeightOnObj(Agent,AgHt),
	mudAtLoc(Obj2,LOC), prop_or(Obj2,mudHeight,0,Obj2Ht),
	ObjHt =< (AgHt + Obj2Ht),
	ObjHt > 1.

%Record keeping
update_charge(Agent,actClimb) :-
	del(mudCharge(Agent,Old)),
	New is Old - 5,
	add(mudCharge(Agent,New)).

update_stats(Agent,trip) :- 
        del(mudHealth(Agent,Old)),
	New is Old - 3,
	add(mudHealth(Agent,New)).

update_stats(Agent,pulled) :- 
        del(mudHealth(Agent,Old)),
	New is Old - 2,
	add(mudHealth(Agent,New)),
	(add_cmdfailure(Agent,pulled)).


:- include(logicmoo(vworld/moo_footer)).
