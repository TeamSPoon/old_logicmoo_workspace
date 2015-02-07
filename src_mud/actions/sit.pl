% :-swi_module(user). 
:-swi_module(modSit, []).
/** <module> Agent Postures there and does nothing
% Agent will loose a bit of charge, but heal a bit of damage
% May 18, 1996
% John Eikenberry
% Douglas Miles 2014

*/
:- include(logicmoo(vworld/moo_header)).

:- register_module_type(mtCommand).

:-decl_mpred(vtPosture/1).

:-decl_mpred(stance(tAgentGeneric,vtPosture)).

vtPosture(actSit).
vtPosture(actStand).
vtPosture(actLay).
vtPosture(actSleep).
vtPosture(actKneel).

tCol(vtPosture).

moo_posture(P):-mudIsa(P,vtPosture).

:-decl_mpred(prologSingleValued(stance(tAgentGeneric,vtPosture))).

user:action_info(Posture,ftText("sets and agent's stance to ",Posture)):-moo_posture(PostureV),Posture=..[PostureV,isOptional(tFurniture,isSelfLOC)].

% Sit and do nothing.
user:agent_call_command(Agent,Verb):- compound(Verb), functor(Verb,Sit,1),moo_posture(Sit),arg(1,Verb,Where),user:agent_call_command(Agent,actOnto(Where,Sit)).

user:action_info(actOnto(isOptional(tFurniture,isSelfLOC),vPosture),txtConcatFn("onto tObj do ",Posture)):-moo_posture(Posture).

user:agent_call_command(Agent,actOnto(Where,Sit)):-
        fmt('agent ~w is now ~wing on ~w',[Agent,Sit,Where]),
        padd(Agent,stance(Sit)),
        padd(Agent,localityOfObject(Where)),
	call_update_charge(Agent,Sit).

update_charge(Agent,Sit) :- moo_posture(Sit), padd(Agent,[mudEnergy(-1)]).

:- include(logicmoo(vworld/moo_footer)).
