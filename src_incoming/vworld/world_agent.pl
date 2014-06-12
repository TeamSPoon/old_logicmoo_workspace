/** <module> 
% NPC_Interface for calling actions
%
% "command"s are often text or a description of proposed actions
% "action" is a MUD understood command (GOAL)
%
% Douglas Miles
% Dec 13, 2035
%
*/

/*
% This file is "included" from world.pl 
:-module(actr, [ ]).
*/

% =====================================================================================================================
% call_agent_command/2 -->  call_agent_command_maybe_fail/3
% =====================================================================================================================


% atom with stuff
call_agent_command(Agent,Atom):-once((atom(Atom),atomSplit(Atom,List),length(List,Len))),Len>1,!,call_agent_command(Agent,List),!.

% join up @ verbs 
call_agent_command(Ag,[A,B|REST]):- atom(A),atom(B),member(A,['@']),atom_concat(A,B,C),!,call_agent_command(Ag,[C|REST]),!.

call_agent_command(Agent,CMD):-
   ensure_session_id(Agent,SESSION,Pushed),!,
   must(atloc(Agent,Where)),
      % start event
     % raise_location_event(Where,notice(reciever,do(Agent,CMD))),
     once((catch(( 
       ignore(( 
         once((debugOnError(once((call_agent_command_never_fail(Agent,CMD,Result)))),
           % complete event
           once(raise_location_event(Where,notice(reciever,done(Agent,Result)))));
           % fail event
              raise_location_event(Where,notice(reciever,failed(Agent,CMD))))))),E,fmt('call_agent_action_maybe_fail/2 Error ~q ',[E])))),
    (Pushed -> ignore(retract(thlocal:current_agent(SESSION,Agent)));true),!.


call_agent_command_never_fail(Agent,CMD,Result):- catch(once(call_agent_command_maybe_fail(Agent,CMD,Result)),E,(fmt('call_agent_command_never_fail ~q on ~q ~n',[E,CMD]),grtrace,fail)),!.
call_agent_command_never_fail(Agent,CMD,Result):- bugger:isDebugging(parser),!, dumpST,grtrace, call_agent_command_maybe_fail(Agent,CMD,Result).


% =====================================================================================================================
% call_agent_command_maybe_fail/2 -->  call_agent_command_is_list/2
% =====================================================================================================================

%var
call_agent_command_maybe_fail(Agent,VERB,Result):-var(VERB),Result=failed(var(VERB)),!,trace_or_throw(call_agent_command_maybe_fail(Agent,VERB)),!.


% lists
call_agent_command_maybe_fail(Agent,List,Result):- is_list(List),call_agent_command_is_list(Agent,List,Result),!.
call_agent_command_maybe_fail(Agent,[VERB],Result):-!,call_agent_command_maybe_fail(Agent, VERB,Result),!.


call_agent_command_maybe_fail(A,CMD,Result):- call_agent_action_maybe_fail(A,CMD,Result),!.

% execute a prolog command including prolog/0
call_agent_command_maybe_fail(_Gent,Atom,done(OneCmd)):- atom(Atom), catch((
   (once((read_term_from_atom(Atom,OneCmd,[variables(VARS)]),
      prolog_callable_expanded(OneCmd),
      fmt('doing command ~q~n',[OneCmd]))),!, 
             doall((call_expanded(OneCmd),fmt('Yes: ~w',[VARS]))))),_,fail),!.

% atom with stuff
call_agent_command_maybe_fail(Agent,Atom,Result):-once((atom(Atom),atomSplit(Atom,List),length(List,Len))),Len>1,call_agent_command_maybe_fail(Agent,List,Result),!.

% other stuff
call_agent_command_maybe_fail(Agent,Comp,Result):- compound(Comp),safe_univ(Comp,List),call_agent_command_is_list(Agent,List,Result),!.

% =====================================================================================================================
% call_agent_command_is_list/2 -->  call_agent_action/2
% =====================================================================================================================
% join up @ verbs 
call_agent_command_is_list(Ag,[A,B|REST],Result):- atom(A),atom(B),member(A,['@']),atom_concat(A,B,C),!,call_agent_command_is_list(Ag,[C|REST],Result),!.

call_agent_command_is_list(Agent,[VERB|SENT],Result):-
   once((call_no_cuts(moo:agent_text_command(Agent,[VERB|SENT],AgentR,CMD)),
   safe_univ(CMDOLD ,[VERB|SENT]),
   once(CMDOLD\=CMD;Agent\=AgentR))),
   call_agent_action_maybe_fail(AgentR,CMD,Result),!.

call_agent_command_is_list(Agent,[VERB|ARGS],Result):-
     once(( parser_imperative:parse_agent_text_command(Agent,VERB,ARGS,NewAgent,CMD))),
      must(call_agent_action_maybe_fail(NewAgent,CMD,Result)),!.

call_agent_command_is_list(A,[L,I|IST],Result):- atom(L), safe_univ(CMD , [L,I|IST]), call_agent_action_maybe_fail(A,CMD,Result),!.
call_agent_command_is_list(A,[L|IST],Result):- atom(L), safe_univ(CMD , [L|IST]), call_agent_action_maybe_fail(A,CMD,Result),!.
call_agent_command_is_list(A,[L],Result):- call_agent_action_maybe_fail(A,L,Result),!. 

% remove period at end
call_agent_command_is_list(A,PeriodAtEnd,Result):-append(New,[(.)],PeriodAtEnd),!,call_agent_command_maybe_fail(A,New,Result),!.

% call_agent_command_is_list(A,LIST):- debug_or_throw(call_agent_command_is_list(A,LIST)),!.

% =====================================================================================================================
% call_agent_action_maybe_fail/2 -->  my_call_agent_action_maybe_fail/2
% =====================================================================================================================
% All Actions must be called from here!

call_agent_action_maybe_fail(Agent,CMDI,Result):- is_list(CMDI),!,call_agent_command_is_list(Agent,CMDI,Result),!.
call_agent_action_maybe_fail(Agent,CMDI,Result):- 
    once(( subst(CMDI,self,Agent,CMDI2),      
     (atloc(Agent,Where) -> subst(CMDI2,here,Where,CMD) ; CMD=CMDI2))),
     call_no_cuts(moo:agent_call_command(Agent,CMD)),!,
     Result=did(Agent,CMD),!.



ensure_session_id(Agent,SESSION,fail):- get_session_id(SESSION),once(thlocal:current_agent(SESSION,Agent2)),Agent2==Agent,!.
ensure_session_id(Agent,SESSION,true):- get_session_id(SESSION),asserta(thlocal:current_agent(SESSION,Agent)),!.


get_session_id(IDIn):-current_input(ID),is_stream(ID),!,ID=IDIn.
get_session_id(ID):-thread_self(ID).

current_agent(PIn):-get_session_id(O),thlocal:current_agent(O,P),!,P=PIn.

current_agent_or_var(P):- once(current_agent(PIn)),P=PIn,!.
current_agent_or_var(_).

foc_current_player(P):- current_agent(P),!.
foc_current_player(P):- get_session_id(O),generate_new_player(P),!,asserta(thlocal:current_agent(O,P)).

generate_new_player(P) :-
  gensym(player,N),
   P=explorer(N),
   must(create_agent(P)).


% Lists all the agents in the run. Except for other monsters.
list_agents(Agents) :- agent_list(Agents), !.
list_agents(Agents) :- % build cache
	findall(NearAgent,req(agent(NearAgent)),Agents),
	assert(agent_list(Agents)),!.

:-export((agent_into_corpse/1, display_stats/1)).

% When an agent dies, it turns into a corpse.
% corpse is defined as an object in the *.objects.pl files
agent_into_corpse(Agent) :-
	del(atloc(Agent,LOC)),
	clr(str(Agent,_)),
	clr(height(Agent,_)),
	clr(stm(Agent,_)),
	clr(spd(Agent,_)),
	add(atloc(corpse(Agent),LOC)).

% Displays all the agents stats. Used at end of a run.
display_stats(Agents) :-
	forall(member(Agent,Agents),
	          (charge(Agent,Chg),
		  damage(Agent,Dam),
		  score(Agent,Scr),
		  findall(Obj,possess(Agent,Obj),Inv),
		  write('Agent = '), write(Agent), nl,
		  write('Charge = '), write(Chg), write('  '),
		  write('Dam= ' ), write(Dam), write('  '),
		  write('Score = '), write(Scr), nl,
		  write('Inventory = '), write(Inv), nl)).

