% File 'actr.pl'
% Purpose: A test module
% Maintainer: Douglas Miles
%
%

% command is text or terms (TEXT)
% action is a MUD understood action (GOAL)
:-module(actr, [ call_agent_command/2,  call_agent_action/2 ]).



% =====================================================================================================================
% call_agent_command/2 -->  call_agent_action/2
% =====================================================================================================================
% execute a prolog command including prolog/0
call_agent_command(Agent,[VERB|ARGS]):-
      debugOnError(parse_agent_text_command(Agent,VERB,ARGS,NewAgent,CMD)),
      call_agent_action(NewAgent,CMD),!.

% lists
call_agent_command(A,Atom):-atom(Atom),atomSplit(Atom,List),!,call_agent_command(A,List).

% prolog command
call_agent_command(_Gent,Atom):- atom(Atom), catch((
   (once((read_term_from_atom(Atom,OneCmd,[variables(VARS)]),
      predicate_property(OneCmd,_),
      fmt('doing command ~q~n',[OneCmd]))),!, doall((OneCmd,fmt('Yes: ~w',[VARS]))))),_,fail).

% remove period at end
call_agent_command(A,PeriodAtEnd):-append(New,[(.)],PeriodAtEnd),!,call_agent_command(A,New).
call_agent_command(Ag,[A,B|REST]):- atom(A),atom(B),A=='@',atom_concat(A,B,C),!,call_agent_command(Ag,[C|REST]).

call_agent_command(A,[L,I|IST]):- atom(L), CMD =.. [L,I|IST],!, call_agent_action(A,CMD).

call_agent_command(A,CMD):- call_agent_action(A,CMD),!.

% All Actions must be called from here!
call_agent_action(Agent,CMDI):-
      subst(CMDI,self,Agent,CMD),
      thread_self(TS),
      asserta(thlocal:current_agent(TS,Agent)),
      atloc(Agent,Where),
      % start event
      raise_location_event(Where,notice(reciever,do(Agent,CMD))),
     catch(( ignore(( once((debugOnError(moo:agent_call_command(Agent,CMD)),
           % complete event
           raise_location_event(Where,notice(reciever,done(Agent,CMD))));
           % fail event
              raise_location_event(Where,notice(reciever,failed(Agent,CMD))))))),E,fmt('call_agent_action/2 Error ~q ',[E])),
  retract(thlocal:current_agent(TS,Agent)).




