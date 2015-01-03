% :-swi_module(user). 
:-swi_module(actSit, []).
/** <module> Agent Postures there and does nothing
% Agent will loose a bit of charge, but heal a bit of damage
% May 18, 1996
% John Eikenberry
% Douglas Miles 2014

*/
:- include(logicmoo(vworld/moo_header)).

:- register_module_type(tCommand).

:-decl_mpred(tPosture/1).

:-decl_mpred(stance(tAgentGeneric,tPosture)).


mudSubclass(tCommand,ftAction).

tPosture(actSit).
tPosture(actStand).
tPosture(actLay).
tPosture(sleep).
tPosture(actKneel).

tCol(tPosture).

moo_posture(P):-mudIsa(P,tPosture).

:-decl_mpred(singleValued(stance(tAgentGeneric,tPosture))).

action_info(Posture,ftText("sets and agent's stance to ",Posture)):-moo_posture(PostureV),Posture=..[PostureV,optional(tFurniture,here)].

% Sit and do nothing.
agent_call_command(Agent,Verb):- compound(Verb), functor(Verb,Sit,1),moo_posture(Sit),arg(1,Verb,Where),agent_call_command(Agent,actOnto(Where,Sit)).

agent_call_command(Agent,actOnto(Where,Sit)):-
        fmt('agent ~w is now ~wing on ~w',[Agent,Sit,Where]),
        padd(Agent,stance(Sit)),
        padd(Agent,localityOfObject(Where)),
	call_update_charge(Agent,Sit).

update_charge(Agent,Sit) :- moo_posture(Sit), padd(Agent,[mudCharge(-1)]).

:- include(logicmoo(vworld/moo_footer)).
