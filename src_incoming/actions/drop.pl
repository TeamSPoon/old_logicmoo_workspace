% drop.pl
% May 18, 1996
% John Eikenberry
% Dec 13, 2035
% Douglas Miles
%
/** <module> 
% This file defines the basic drop predicate
% 
*/
:- module(drop, []).

:- include(logicmoo('vworld/vworld_header.pl')).

:- register_module_type(command).

moo:decl_action(drop(item)).

% Drop something
moo:agent_call_command(Agent,drop(Obj)) :-
	del(possess(Agent,Obj)),
	atloc(Agent,LOC),
	add(atloc(Obj,LOC)),
	moo:update_charge(Agent,drop).
%Nothing to drop
moo:agent_call_command(Agent,drop(_)) :-
	moo:update_charge(Agent,drop),
	add(failure(Agent,drop)).

% Record keeping
moo:decl_update_charge(Agent,drop) :-
	del(charge(Agent,Old)),
	New is Old - 1,
	add(charge(Agent,New)).



:- include(logicmoo('vworld/vworld_footer.pl')).


