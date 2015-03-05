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
:-swi_module(modDrop, []).

:- include(logicmoo(vworld/moo_header)).

% :- register_module_type (mtCommand).

action_info(actDrop(tDropAble)).

% Drop something
user:agent_call_command(Agent,actDrop(Obj)) :-
	mudPossess(Agent,Obj),        
        clr(mudPossess(Agent,Obj)),
        must(not((mudPossess(Agent,Obj)))),
        mudAtLoc(Agent,LOC),
        add(mudAtLoc(Obj,LOC)),
	must(call_update_charge(Agent,actDrop)).

%Nothing to drop
user:agent_call_command(Agent,actDrop(_)) :-
	call_update_charge(Agent,actDrop),
	(add_cmdfailure(Agent,actDrop)).

% Record keeping
update_charge(Agent,actDrop) :- add(mudEnergy(Agent,-1)).

user:agent_text_command(Agent,[actDrop,X],Agent,actDrop(X)).

%:-must_det(show_call(get_agent_text_command(agent1,[drop,item1],_R,_CMD))).

% :- include(logicmoo(vworld/moo_footer)).
