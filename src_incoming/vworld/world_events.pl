% events.pl
% Dec 13, 2035
% Douglas Miles
%
/** <module> % File used to implement in_world_events...
% the world is run.
%
% props(Obj,height(ObjHt))  == k(height,Obj,ObjHt) == rdf(Obj,height,ObjHt) == height(Obj,ObjHt)
% padd(Obj,height(ObjHt))  == padd(height,Obj,ObjHt,...) == add(QueryForm)
% kretract[all](Obj,height(ObjHt))  == kretract[all](Obj,height,ObjHt) == pretract[all](height,Obj,ObjHt) == del[all](QueryForm)
% keraseall(AnyTerm).
%
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

