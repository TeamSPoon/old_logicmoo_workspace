/** <module> File used to implement in_world_events
% like Talking, Appearing, falling rocks..
%
% Project LogicMoo: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/

/*
% This file is "included" from world.pl 
*/

asInvoked(Cmd,[L|Ist]):-append([L|Ist],[],Foo),Foo\==[L|Ist],!,asInvoked(Cmd,Foo).
asInvoked(Cmd,[L|Ist]):-atom(L),!, Cmd=..[L|Ist].
asInvoked(Cmd,[L|Ist]):-!,Cmd=..[asInvoked,L|Ist].
asInvoked(Cmd,Cmd):-!.

atlocNear(Whom,Where):-nonvar(Where),!,findall(Whom,atlocNear0(Whom,Where),List),list_to_set(List,Set),!,member(Whom,Set).
atlocNear(Whom,Where):-nonvar(Whom),!,findall(Where,atlocNear0(Whom,Where),List),list_to_set(List,Set),!,member(Where,Set).
atlocNear(Whom,Where):-findall(Whom+Where,atlocNear0(Whom,Where),List),list_to_set(List,Set),!,member(Whom+Where,Set).

atlocNear0(Whom,Where):-locs_near(Where,LOC),holds_t(atloc,Whom,LOC).

raise_location_event(Where,Event):- forall(atlocNear(Whom,Where),ignore(show_event_to(Whom,Event))).
:-export(raise_location_event/2).


show_event_to(Whom,Event):-subst(Event,reciever,you,NewEventM),subst(NewEventM,Whom,you,NewEvent),direct_to_agent(Whom,NewEvent),!.
direct_to_agent(Whom,NewEvent):- 
      ensure_agent_stream(Whom,Session,Output),
      with_assertions(thlocal:current_agent(Session,Whom),ignoreOnError((with_output_to_stream(Output,fmt(NewEvent))))),!.

direct_to_agent(Whom,NewEvent):- nop(dmsg(could_not(direct_to_agent(Whom,NewEvent)))).
%%:-export(direct_to_agent/2).
ensure_agent_stream(Whom,Input,Output):-agent_message_stream(Whom,Input,Output),is_stream(Input),is_stream(Output),!.
ensure_agent_stream(Whom,_Input,_Output):-ignore(retract(agent_message_stream(Whom,_,_))),!,fail.

:-export(deliverable_location_events/3).

deliverable_location_events(Agent,Loc,tick(Agent,Loc)):-fail.
