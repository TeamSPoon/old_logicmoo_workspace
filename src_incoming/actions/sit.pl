% :- module(user). 
:- module(sit, []).
/** <module> Agent Postures there and does nothing
% Agent will loose a bit of charge, but heal a bit of damage
% May 18, 1996
% John Eikenberry
% Douglas Miles 2014

*/
:- include(logicmoo(vworld/moo_header)).

:- moo:register_module_type(command).

:-decl_mpred(moo:posture/1).

:-decl_mpred(moo:stance(agent,posture)).


subclass(command,action).

moo:posture(sit).
moo:posture(stand).
moo:posture(lay).
moo:posture(sleep).
moo:posture(kneel).

moo:type(posture).

moo_posture(P):-isa(P,posture).

:-decl_mpred(singleValued(stance(agent,posture))).

moo:action_info(Posture,text("sets and agent's stance to ",Posture)):-moo_posture(PostureV),Posture=..[PostureV,optional(furniture,here)].

% Sit and do nothing.
moo:agent_call_command(Agent,Verb):- compound(Verb), functor(Verb,Sit,1),moo_posture(Sit),arg(1,Verb,Where),moo:agent_call_command(Agent,onto(Where,Sit)).

moo:agent_call_command(Agent,onto(Where,Sit)):-
        fmt('agent ~w is now ~wing on ~w',[Agent,Sit,Where]),
        padd(Agent,stance(Sit)),
        padd(Agent,localityOfObject(Where)),
	call_update_charge(Agent,Sit).

moo:update_charge(Agent,Sit) :- moo_posture(Sit), padd(Agent,[charge(-1)]).

:- include(logicmoo(vworld/moo_footer)).

