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
% :-swi_module(world_agent,[]).

/*
% This file is "included" from world.pl 
:-swi_module(actr, [ call_agent_command/2,  call_agent_action/2 ]).
*/

:-swi_export(parse_agent_text_command_checked/5).
parse_agent_text_command_checked(Agent,VERB,ARGS,NewAgent,CMD):- 
   catch(( parse_agent_text_command(Agent,VERB,ARGS,NewAgent,CMD),
         nonvar(CMD),must(nonvar(NewAgent))),'$aborted',true),
         ignore((CMD=tick)),ignore((NewAgent=Agent)).

parse_agent_text_command_checked(Agent,VERB,ARGS,NewAgent,CMD):- 
   debugging(parser), trace, parse_agent_text_command(Agent,VERB,ARGS,NewAgent,CMD).

must_ac(G):- show_call(must(G)).
% =====================================================================================================================
% call_agent_command/2 -->  call_agent_action/2
% =====================================================================================================================
% execute a prolog command including prolog/0
call_agent_command(Agent,Var):-var(Var),trace_or_throw(var_call_agent_command(Agent,Var)).

call_agent_command(Agent,Text):-string(Text),atom_string(Atom,Text),!,call_agent_command(Agent,Atom).

call_agent_command(Agent,[VERB|ARGS]):-
      debugOnError(parse_agent_text_command_checked(Agent,VERB,ARGS,NewAgent,CMD)),
      must_ac(call_agent_action(NewAgent,CMD)),!.

% lists
call_agent_command(A,Atom):-atom(Atom),atomSplit(Atom,List),must(is_list(List)),!,call_agent_command(A,List).

% prolog command
call_agent_command(_Gent,Atom):- atom(Atom), catch((
   (once((read_term_from_atom(Atom,OneCmd,[variables(VARS)]),
      predicate_property(OneCmd,_),
      fmt('doing command ~q~n',[OneCmd]))),!, doall((OneCmd,fmt('Yes: ~w',[VARS]))))),E,(dmsg(E),fail)).

% remove period at end
call_agent_command(A,PeriodAtEnd):-append(New,[(.)],PeriodAtEnd),!,call_agent_command(A,New).

% concat the '@'
call_agent_command(Ag,[A,B|REST]):- atom(A),atom(B),A=='@',atom_concat(A,B,C),!,call_agent_command(Ag,[C|REST]).

call_agent_command(A,[L,I|IST]):- atom(L), CMD =.. [L,I|IST],!, must_ac(call_agent_action(A,CMD)).

call_agent_command(A,CMD):- must_ac(call_agent_action(A,CMD)),!.

% All Actions must be called from here!
call_agent_action(Agent,CMDI):-var(CMDI),trace_or_throw(call_agent_action(Agent,CMDI)).
call_agent_action(Agent,CMDI):-
   subst(CMDI,self,Agent,CMD),
   thread_self(TS),
   (TS=main -> Wrapper = call ; Wrapper = notrace),
   with_assertions(thlocal:session_agent(TS,Agent),
     with_assertions(thlocal:agent_current_action(Agent,CMD),
      call(Wrapper, call_agent_action_lc(Agent,CMD)))).

:-swi_export(where_atloc/2).
where_atloc(Agent,Where):-atloc(Agent,Where).
where_atloc(Agent,Where):-localityOfObject(Agent,Where).
where_atloc(Agent,Where):-atloc(Agent,Loc),!,locationToRegion(Loc,Where).
where_atloc(Agent,'OffStage'):-nonvar(Agent).

call_agent_action_lc(Agent,CMD):-
   % start event
 ignore((must_det_l([where_atloc(Agent,Where),
   raise_location_event(Where,notice(reciever,begin(Agent,CMD))),   
   catch(call_agent_where_action_lc(Agent,Where,CMD),E,(fmt('call_agent_action/2 Error ~q ',[E])))]))),!.

:-swi_export(send_command_completed_message/4).
send_command_completed_message(Agent,Where,Done,CMD):-
     ignore((must_det_l([flush_output,renumbervars(CMD,SCMD),Message =..[Done,Agent,SCMD],
                raise_location_event(Where,notice(reciever,Message)),
                padd(Agent,last_command(SCMD)),flush_output]))),!.


% complete event
call_agent_where_action_lc(Agent,Where,CMD):- debugOnError(agent_call_command(Agent,CMD)),send_command_completed_message(Agent,Where,done,CMD),!.
% fail event
call_agent_where_action_lc(Agent,Where,CMD):-  send_command_completed_message(Agent,Where,failed,CMD),!. 

get_session_id(IDIn):-current_input(ID),is_stream(ID),!,ID=IDIn.
get_session_id(ID):-thread_self(ID).

:-swi_export(current_agent/1).
:-decl_mpred_prolog(current_agent/1).
current_agent(PIn):- get_session_id(O),get_agent_session(P,O),!,P=PIn.

current_agent_or_var(P):- once(current_agent(PIn)),P=PIn,!.
current_agent_or_var(_).


get_agent_session(P,O):- (thlocal:session_agent(O,P);thglobal:global_session_agent(O,P)).

foc_current_player(P):- current_agent(P),nonvar(P),!.
foc_current_player(P):- nonvar(P),agent(P),become_player(P),!.
foc_current_player(P):- 
  prolog_must_l([    
             get_session_id(O),
             once(get_dettached_npc(P);generate_new_player(P)),
               retractall(thglobal:global_session_agent(O,P)),
              asserta(thglobal:global_session_agent(O,P)),assert_isa(P,human_player),must_det(create_agent(P))]),!.


get_dettached_npc(P):-random_instance_no_throw(agent,P,true),not(isa(P,human_player)),!.

% generate_new_player(P):- req(agent(P)),!.
generate_new_player(P):- prolog_must_l([gensym(player,N),not((isa_asserted(N,agent))),P=explorer(N),ensure_new_player(P)]),!.

ensure_new_player(P):- prolog_must_l([assert_isa(P,explorer),assert_isa(P,player),assert_isa(P,agent)]),!.

detatch_player(P):- thglobal:global_session_agent(_,P),!,trace_or_throw(detatch_player(P)).
detatch_player(_).

:-swi_export(become_player/1).
become_player(NewName):- once(current_agent(Was)),Was=NewName,!.
become_player(NewName):- get_session_id(O),retractall(thglobal:global_session_agent(O,_)),detatch_player(NewName),asserta(thglobal:global_session_agent(O,NewName)),ensure_player_stream_local(NewName).
:-swi_export(become_player/2).
become_player(_Old,NewName):-become_player(NewName).

% Lists all the agents in the run. Except for other monsters.
list_agents(Agents) :- agent_list(Agents), !.
list_agents(Agents) :- % build cache
	findall(NearAgent,req(agent(NearAgent)),Agents),
	assert(agent_list(Agents)),!.

:-swi_export((agent_into_corpse/1, display_stats/1)).

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
		  health(Agent,Dam),
		  score(Agent,Scr),
		  findall(Obj,possess(Agent,Obj),Inv),
		  write('Agent = '), write(Agent), nl,
		  write('Charge = '), write(Chg), write('  '),
		  write('Dam= ' ), write(Dam), write('  '),
		  write('Score = '), write(Scr), nl,
		  write('Inventory = '), write(Inv), nl)).
