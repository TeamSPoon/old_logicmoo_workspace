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

% =====================================================================================================================
% call_agent_command/2 -->  call_agent_action/2
% =====================================================================================================================
% execute a prolog command including prolog/0
call_agent_command(Agent,[VERB|ARGS]):-
      debugOnError(parse_agent_text_command(Agent,VERB,ARGS,NewAgent,CMD)),
      call_agent_action(NewAgent,CMD),!.

% lists
call_agent_command(A,Atom):-atom(Atom),atomSplit(Atom,List),call_agent_action(A,List).

% prolog command
call_agent_command(_Gent,Atom):- atom(Atom), catch(((once((read_term_from_atom(Atom,OneCmd,[variables(VARS)]),
      predicate_property(OneCmd,_),!,
      fmt('doing command ~q~n',[OneCmd]))), ignore((OneCmd,fmt('Yes: ~w',[VARS]),fail)))),_,fail).

% remove period at end
call_agent_command(A,PeriodAtEnd):-append(New,[(.)],PeriodAtEnd),!,call_agent_command(A,New).

call_agent_command(A,[L,I|IST]):- atom(L), CMD =.. [L,I|IST],!, call_agent_action(A,CMD).

call_agent_command(A,CMD):- debugOnError(call_agent_action(A,CMD)),!.


% All Actions must be called from here!
call_agent_action(Agent,CMD):-
 thread_self(TS),
 asserta(thlocal:current_agent(TS,Agent)),
 atloc(Agent,Where),
     % start event
     raise_location_event(Where,notice(reciever,do(Agent,CMD))),
    ignore(( once((debugOnError(moo:agent_call_command(Agent,CMD)),
           % complete event
           raise_location_event(Where,notice(reciever,done(Agent,CMD))));
           % fail event
              raise_location_event(Where,notice(reciever,failed(Agent,CMD)))))),
  retract(thlocal:current_agent(TS,Agent)).


asInvoked( [L|Ist],Cmd):-atom(L),!, Cmd=..[L|Ist].
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

