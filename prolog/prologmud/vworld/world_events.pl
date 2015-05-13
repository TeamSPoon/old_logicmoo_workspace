/** <module> File used to implement in_world_events
% like Talking, Appearing, falling rocks..
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
% :-swi_module(world_events,[]).

/*
% This file is "included" from world.pl 
*/


asInvoked(Cmd,[L|Ist]):-stack_check,once(append([L|Ist],[],Foo)),Foo\==[L|Ist],!,asInvoked(Cmd,Foo).
asInvoked(Cmd,[L|Ist]):-atom(L),not(bad_functor(L)),!, Cmd=..[L|Ist].
asInvoked(Cmd,[L|Ist]):-!,Cmd=..[asInvoked,L|Ist].
asInvoked(Cmd,Cmd):-!.

% :-decl_mpred_prolog(mudObjNearLoc(tObj,tObj)).
mudObjNearLoc(Whom,Where):-nonvar(Where),!,findall(Whom,atlocNear0(Whom,Where),List),list_to_set(List,Set),!,member(Whom,Set).
mudObjNearLoc(Whom,Where):-nonvar(Whom),!,findall(Where,atlocNear0(Whom,Where),List),list_to_set(List,Set),!,member(Where,Set).
mudObjNearLoc(Whom,Where):-findall(Whom+Where,atlocNear0(Whom,Where),List),list_to_set(List,Set),!,member(Whom+Where,Set).

atlocNear0(Whom,Where):-mudNearbyLocs(Where,LOC),is_asserted(mudAtLoc(Whom,LOC)).


:-export(raise_location_event/2).
raise_location_event(Where,Event):- forall(no_repeats(Whom,(tAgent(Whom),mudObjNearLoc(Whom,Where))),doall(call_no_cuts(hooks:deliver_event(Whom,Event)))).


hooks:deliver_event(Whom,Event):-subst(Event,reciever,you,NewEventM),subst(NewEventM,Whom,you,NewEvent),to_agent_io(Whom,NewEvent),!.

to_agent_io(Whom,NewEvent):- 
  once(user:provide_agent_delivery_callback(Whom,_)), % anyone listening?
   forall(user:provide_agent_delivery_callback(Whom,Pred),call(Pred,Who,NewEvent)).
to_agent_io(_Whom,_NewEvent):-!.
%todo allow NPC queuing of ..  
to_agent_io(Whom,NewEvent):- dmsg(could_not(to_agent_io(Whom,NewEvent))).



user:provide_agent_delivery_callback(Whom,Pred):- get_agent_stream(Whom,Session,Output),
    Pred = 
     pfc_lambda([Whom,NewEvent],with_assertions(thlocal:session_agent(Session,Whom),ignoreOnError((with_output_to_stream(Output,fmt(NewEvent)))))).


%%:-export(to_agent_io/2).
get_agent_stream(Whom,Input,Output):- thglobal:agent_message_stream(Whom,_,Input,Output),is_stream(Input),is_stream(Output),!.
get_agent_stream(Whom,_Input,_Output):-ignore(retract(thglobal:agent_message_stream(Whom,_,_,_))),!,fail.

:-export(mudDeliverableLocationEvents/3).
:-dynamic(mudDeliverableLocationEvents/3).
prologHybrid(mudDeliverableLocationEvents(tAgent,tRegion,ftTerm)).

