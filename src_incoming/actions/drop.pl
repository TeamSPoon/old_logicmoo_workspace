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
% :- module(user). 
:- module(drop, []).

:- include(logicmoo(vworld/moo_header)).

:- moo:register_module_type(command).

moo:actiontype(drop(item)).

% Drop something
moo:agent_call_command(Agent,drop(SObj)) :-
	possess(Agent,Obj),match_object(SObj,Obj),
	possess(Agent,Obj),del(possess(Agent,Obj)),atloc(Agent,LOC),add(atloc(Obj,LOC)),
	must(call_update_charge(Agent,drop)).

%Nothing to drop
moo:agent_call_command(Agent,drop(_)) :-
	call_update_charge(Agent,drop),
	(add_cmdfailure(Agent,drop)).

% Record keeping
moo:update_charge(Agent,drop) :- add(charge(Agent,-1)).

moo:agent_text_command(Agent,[drop,X],Agent,drop(X)).

%:-must_det(show_call(get_agent_text_command(agent1,[drop,item1],_R,_CMD))).

:- include(logicmoo(vworld/moo_footer)).


