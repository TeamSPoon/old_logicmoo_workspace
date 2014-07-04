% push.pl
% July 1, 1996
% John Eikenberry
%
% Dec 13, 2035
% Douglas Miles
%
/** <module> 
% This is meant to be used as a basic template for how the action
% files are formatted.
%
*/
% :- module(user). 
:- module(push, []).

:- include(logicmoo(vworld/moo_header)).

:- moo:register_module_type(command).

moo:action_info(push(dir)).

% Push a box
% Nothing to push... agent moves and takes a little damage.
%Plus it still costs the same charge as if the agent did push something
moo:agent_call_command(Agent,push(Dir)) :-	
	atloc(Agent,LOC),
	move_dir_target(LOC,Dir,XXYY),
	atloc(What,XXYY),
	integer(What),
	in_world_move(_,Agent,Dir),
	call_update_stats(Agent,strain),
	call_update_charge(Agent,push).

% Pushing what cannot be pushed
% Some damage and loss of charge (same as normal push)
moo:agent_call_command(Agent,push(Dir)) :-	
	atloc(Agent,LOC),
	move_dir_target(LOC,Dir,XXYY),
	atloc(What,XXYY),
	\+ pushable(Agent,What,XXYY,Dir),
	call_update_stats(Agent,hernia),
	call_update_charge(Agent,push).

% A successful PUSH
moo:agent_call_command(Agent,push(Dir)) :-	
	atloc(Agent,LOC),
	move_dir_target(LOC,Dir,XXYY),
	atloc(What,XXYY),
	move_object(XXYY,What,Dir),
	in_world_move(_,Agent,Dir),
	call_update_charge(Agent,push).

% Can the Object be pushed?
pushable(Agent,Obj,LOC,Dir) :-
	str(Agent,Str),
	props(Obj,weight(Wt)),
	Wt \== 4,
	Wt =< Str,
	\+ anything_behind(LOC,Dir).
% If the Obj is another agent, compare strenghts to see if the agent can push the other
% An agent can push another if the agents strenght is greater that or equal to
% their opponents strength.
pushable(Agent,Obj,LOC,Dir) :-
	str(Agent,Str),
	str(Obj,OppStr),
	Str >= OppStr,
	(\+ anything_behind(LOC,Dir);
	crashbang(Obj)).

% Is the location behind the pushed object/agent empty (or near empty).
anything_behind(LOC,Dir) :-
	move_dir_target(LOC,Dir,XXYY),
	atloc(What,XXYY),
	props(What,[weight > 1,permanence(or(Pm,0))]),
	Pm < 2.

% Move the object.
move_object(LOC,Obj,Dir) :-
	move_dir_target(LOC,Dir,XXYY),
	squish_behind(XXYY,Obj),
	in_world_move(LOC,Obj,Dir).

% Squish small objects behind what is being pushed.
squish_behind(LOC,Obj) :-
        XY = LOC,
	atloc(What,XY),
	props(What,weight(1)),
	props(Obj,weight(N)),
	N > 1,
	del(atloc(What,XY)).
squish_behind(_,_).

% When one agent pushes another into a wall (or anything big), 
% both the agents take damage. 
% The pusher takes damage as normal (for pushing something
% unpushable), the pushy takes damage below
crashbang(Obj) :- padd(Obj,[damage(-5)]).

% Record keeping
moo:update_charge(Agent,push) :- padd(Agent,[charge(-6)]).
moo:update_stats(Agent,strain) :- padd(Agent,[damage(-2)]).
moo:update_stats(Agent,hernia) :- padd(Agent,[damage(-4),failure(hernia)]).

:- include(logicmoo(vworld/moo_footer)).


