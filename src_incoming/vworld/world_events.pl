/** <module> File used to implement in_world_events
% like Talking, Appearing, falling rocks..
%
% Project Logicmoo: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/

/*
% This file is "included" from world.pl 
*/


asInvoked([L|Ist],Cmd):-atom(L),!, Cmd=..[L|Ist].
asInvoked(List,Cmd):-Cmd=..[asInvoked|List].

atlocNear(Whom,Where):-atloc(Whom,LOC),locs_near(LOC,Where).
raise_location_event(Where,Event):-
   forall(atlocNear(Whom,Where),raise_event(Whom,Event)).
:-export(raise_location_event/2).

raise_event(Whom,Event):-subst(Event,reciever,Whom,NewEvent),send_to_agent(Whom,NewEvent),!.
:-export(raise_event/2).

send_to_agent(Whom,NewEvent):- agent_message_stream(Whom,_Input,Output),!,fmt(Output,' ~n Event: ~q. ~n ',[NewEvent]).
send_to_agent(Whom,NewEvent):- nop(could_not(send_to_agent(Whom,NewEvent))).
:-export(send_to_agent/2).

:-export(deliverable_location_events/3).

deliverable_location_events(Agent,Loc,tick(Agent,Loc)):-fail.
