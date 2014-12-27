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
% :-swi_module(user). 
:-swi_module(drop, []).

:- include(logicmoo(vworld/moo_header)).

:- register_module_type(command).

actiontype(drop(item)).

% Drop something
agent_call_command(Agent,drop(SObj)) :-
	possess(Agent,Obj),
        match_object(SObj,Obj),
        del(possess(Agent,Obj)),
        must(not((possess(Agent,Obj)))),
        atloc(Agent,LOC),
        add(atloc(Obj,LOC)),
	must(call_update_charge(Agent,drop)).

%Nothing to drop
agent_call_command(Agent,drop(_)) :-
	call_update_charge(Agent,drop),
	(add_cmdfailure(Agent,drop)).

% Record keeping
update_charge(Agent,drop) :- add(charge(Agent,-1)).

agent_text_command(Agent,[drop,X],Agent,drop(X)).

%:-must_det(show_call(get_agent_text_command(agent1,[drop,item1],_R,_CMD))).

:- include(logicmoo(vworld/moo_footer)).


