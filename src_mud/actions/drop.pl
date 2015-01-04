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
:-swi_module(actDrop, []).

:- include(logicmoo(vworld/moo_header)).

:- register_module_type(mtCommand).

tActionType(actDrop(tItem)).

% Drop something
agent_call_command(Agent,actDrop(SObj)) :-
	mudPossess(Agent,Obj),
        match_object(SObj,Obj),
        del(mudPossess(Agent,Obj)),
        must(not((mudPossess(Agent,Obj)))),
        mudAtLoc(Agent,LOC),
        add(mudAtLoc(Obj,LOC)),
	must(call_update_charge(Agent,actDrop)).

%Nothing to drop
agent_call_command(Agent,actDrop(_)) :-
	call_update_charge(Agent,actDrop),
	(add_cmdfailure(Agent,actDrop)).

% Record keeping
update_charge(Agent,actDrop) :- add(mudCharge(Agent,-1)).

agent_text_command(Agent,[actDrop,X],Agent,actDrop(X)).

%:-must_det(show_call(get_agent_text_command(agent1,[drop,item1],_R,_CMD))).

:- include(logicmoo(vworld/moo_footer)).
