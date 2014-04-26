% take.pl
% May 18, 1996
% John Eikenberry
%
% Dec 13, 2035
% Douglas Miles
%
/** <module>
% This file defines the basic take (pick up) predicate
%
*/
:- module(take, []).

:- include(logicmoo('vworld/vworld_header.pl')).

:- register_module_type(command).

% Take something
% Successfully picking something up
moo:agent_call_command(Agent,take(Obj)) :-
	atloc(Agent,LOC),
	atloc(Obj,LOC),
	props(Obj,weight(1)),
	worth(Agent,take,Obj),
	permanence_take(take,Agent,Obj),
	moo:update_charge(Agent,take).
%Nothing to pick up
moo:agent_call_command(Agent,take(_)) :-
	moo:update_charge(Agent,take),
	add(failure(Agent,take)).

% Is the obect going to stick around after taken, either as is
% or in the agent's possession.
permanence_take(take,Agent,Obj) :-
	atloc(Agent,LOC),
	check_permanence(take,Agent,LOC,Obj).

check_permanence(take,_,LOC,Obj) :-
           props(Obj,permanence(take,0)),
	del(atloc(Obj,LOC)).
check_permanence(take,Agent,LOC,Obj) :-
	props(Obj,permanence(take,1)),
	del(atloc(Obj,LOC)),
	add(possess(Agent,Obj)).
check_permanence(take,_,_,_).

% Record keeping
moo:decl_update_charge(Agent,take) :-
      padd(Agent,[charge(-2)]).






:- include(logicmoo('vworld/vworld_footer.pl')).

