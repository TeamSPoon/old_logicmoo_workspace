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

:- include(logicmoo(vworld/moo_header)).

:- moo:register_module_type(command).

moo:action_info(drop(item)).

% Drop something
moo:agent_call_command(Agent,drop(SObj)) :-
	possess(Agent,Obj),
        object_match(SObj,Obj),
	del(possess(Agent,Obj)),
	atloc(Agent,LOC),
	add(atloc(Obj,LOC)),
	call_update_charge(Agent,drop).

%Nothing to drop
moo:agent_call_command(Agent,drop(_)) :-
	call_update_charge(Agent,drop),
	add(failure(Agent,drop)).

% Record keeping
moo:update_charge(Agent,drop) :-
	del(charge(Agent,Old)),
	New is Old - 1,
	add(charge(Agent,New)).



:- include(logicmoo(vworld/moo_footer)).


