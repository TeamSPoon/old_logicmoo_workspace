% :- module(user). 
:- module(sit, [posture/1]).
/** <module> Agent Postures there and does nothing
% Agent will loose a bit of charge, but heal a bit of damage
% May 18, 1996
% John Eikenberry
% Douglas Miles 2014

*/
:- include(logicmoo(vworld/moo_header)).

:- moo:register_module_type(command).

:- decl_mpred(posture/1,formattype).

moo:posture(sit).
moo:posture(stand).
moo:posture(lay).
moo:posture(kneel).

:-decl_mpred(stance(agent,posture),[singleValued]).

moo:agent_text_command(Agent,[Sit],Agent,Sit):- posture(Sit).
moo:action_info(Posture,text("sets and agent's stance to ",Posture)):-posture(Posture).

% Sit and do nothing.
moo:agent_call_command(Agent,Sit):- posture(Sit),
        fmt('agent ~w is now ~wing ',[Agent,Sit]),
        padd(Agent,stance(Sit)),
	call_update_charge(Agent,Sit).

moo:update_charge(Agent,Sit) :- posture(Sit), padd(Agent,[charge(-1)]).

:- include(logicmoo(vworld/moo_footer)).

