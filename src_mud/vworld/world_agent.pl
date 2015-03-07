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

:- include(logicmoo(vworld/moo_header)).
/*
% This file is "included" from world.pl 
:-swi_module(modr, [ call_agent_command_0/2,  call_agent_action/2 ]).
*/

:-export(parse_agent_text_command_checked/5).
parse_agent_text_command_checked(Agent,VERB,ARGS,NewAgent,CMD):- 
   catch(( parse_agent_text_command(Agent,VERB,ARGS,NewAgent,CMD),
         nonvar(CMD),must(nonvar(NewAgent))),'$aborted',true),
         ignore((CMD=actTick)),ignore((NewAgent=Agent)).

parse_agent_text_command_checked(Agent,VERB,ARGS,NewAgent,CMD):- 
   debugging(parser), trace, parse_agent_text_command(Agent,VERB,ARGS,NewAgent,CMD).

must_ac(G):- show_call(must(G)).


% =====================================================================================================================
% call_agent_command_0/2 -->  call_agent_action/2
% =====================================================================================================================

call_agent_command(A,C):-with_assertions(tlbugger:old_no_repeats, call_agent_command_0(A,C)).

% execute a prolog command including prolog/0
call_agent_command_0(Agent,Var):-var(Var),trace_or_throw(var_call_agent_command(Agent,Var)).

call_agent_command_0(Agent,Text):-string(Text),atom_string(Atom,Text),!,call_agent_command_0(Agent,Atom).

call_agent_command_0(Agent,[VERB|ARGS]):-
      debugOnError(parse_agent_text_command_checked(Agent,VERB,ARGS,NewAgent,CMD)),
      must_ac(call_agent_action(NewAgent,CMD)),!.

% lists
call_agent_command_0(A,Atom):-atom(Atom),atomSplit(Atom,List),must(is_list(List)),!,call_agent_command_0(A,List).

% prolog command
call_agent_command_0(_Gent,Atom):- atom(Atom), catch((
   (once((read_term_from_atom(Atom,OneCmd,[variables(VARS)]),
      predicate_property(OneCmd,_),
      fmt('doing command ~q~n',[OneCmd]))),!, doall((OneCmd,fmt('Yes: ~w',[VARS]))))),E,(dmsg(E),fail)).

% remove period at end
call_agent_command_0(A,PeriodAtEnd):-append(New,[(.)],PeriodAtEnd),!,call_agent_command_0(A,New).

% concat the '@'
call_agent_command_0(Ag,[A,B|REST]):- atom(A),atom(B),A=='@',atom_concat(A,B,C),!,call_agent_command_0(Ag,[C|REST]).

call_agent_command_0(A,[L,I|IST]):- atom(L), CMD =.. [L,I|IST],!, must_ac(call_agent_action(A,CMD)).

call_agent_command_0(A,CMD):- must_ac(call_agent_action(A,CMD)),!.

% All Actions must be called from here!
call_agent_action(Agent,CMDI):-var(CMDI),trace_or_throw(call_agent_action(Agent,CMDI)).
call_agent_action(Agent,CMDI):-
   subst(CMDI,isSelfAgent,Agent,CMD),
   thread_self(TS),
   (TS=main -> Wrapper = call ; Wrapper = notrace),
   with_assertions(thlocal:session_agent(TS,Agent),
     with_assertions(thlocal:agent_current_action(Agent,CMD),
      call(Wrapper, call_agent_action_ilc(Agent,CMD)))).

:-export(where_atloc/2).
where_atloc(Agent,Where):-mudAtLoc(Agent,Where).
where_atloc(Agent,Where):-localityOfObject(Agent,Where).
where_atloc(Agent,Where):-mudAtLoc(Agent,Loc),!,locationToRegion(Loc,Where).
where_atloc(Agent,'OffStage'):-nonvar(Agent).

% call_agent_action_ilc(Agent,CMD):- with_no_fallbacksg(with_no_fallbacks(call_agent_action_ilc0(Agent,CMD))).
call_agent_action_ilc(Agent,CMD):-
   % start event
 ignore((must_det_l([where_atloc(Agent,Where),
   raise_location_event(Where,actNotice(reciever,begin(Agent,CMD))),   
   catch(call_agent_where_action_ilc(Agent,Where,CMD),E,(fmt('call_agent_action/2 Error ~q ',[E])))]))),!.

:-export(send_command_completed_message/4).
send_command_completed_message(Agent,Where,Done,CMD):-
     ignore((must_det_l([flush_output,renumbervars(CMD,SCMD),Message =..[Done,Agent,SCMD],
                raise_location_event(Where,actNotice(reciever,Message)),
                padd(Agent,mudLastCommand(SCMD)),flush_output]))),!.


% complete event
call_agent_where_action_ilc(Agent,Where,CMD):- debugOnError(agent_call_command_now(Agent,CMD)),send_command_completed_message(Agent,Where,done,CMD),!.
% fail event
call_agent_where_action_ilc(Agent,Where,CMD):-  send_command_completed_message(Agent,Where,failed,CMD),!. 

correctCommand(CMD,OUT):-compound(CMD),fail,
   must(current_agent(Who)),
   CMD=..[F|ARGS],   
   functor(CMD,F,A),
   functor(MATCH,F,A),
   vtActionTemplate(MATCH),compound(MATCH),MATCH=..[F|TYPES],!,
   correctEachTypeOrFail(Who,F,query(dbase_t, must),ARGS,TYPES,NEWS),!,
   OUT=..[F|NEWS].

correctCommand(CMD,CMD).

correctEachTypeOrFail( Who, F, Q,ARGS,TYPES,NEWS):- is_list(TYPES),!,maplist(correctEachTypeOrFail(Who,F,Q),ARGS,TYPES,NEWS).
correctEachTypeOrFail(_Who,_F,_Q,Arg,Type,Inst):- fail, not(is_ephemeral(Arg)),not(is_ephemeral(Type)),isa(Arg,Type),!,Inst = Arg.
correctEachTypeOrFail(_Who,_F,_Q,Arg,Type,Inst):- !,acceptableArg(Arg,Type),!,Inst = Arg.
correctEachTypeOrFail(_Who,_F,_Q,Arg,Type,Inst):-not(is_ephemeral(Arg)),not(is_ephemeral(Type)), must(coerce(Arg,Type,Inst)),not(is_ephemeral(Inst)).

is_ephemeral(Var):-var(Var),!,fail.
is_ephemeral(isMissing).
is_ephemeral(isOptional(_,_)).
is_ephemeral(isRandom(_)).
is_ephemeral(isOneOf(_)).


acceptableArg(Arg,Type):-dmsg(acceptableArg(Arg,Type)).


agent_call_command_now(Agent,CMD):-
  with_current_agent(Agent,
      (must(correctCommand(CMD,NewCMD)),
      user:agent_call_command(Agent,NewCMD))).

% need to return http sessions as well
get_session_id(IDIn):-current_input(ID),is_stream(ID),!,ID=IDIn.
get_session_id(ID):-thread_self(ID).

:-export(current_agent/1).
:-export(current_agent/1).
current_agent(PIn):- get_session_id(O),get_agent_session(P,O),!,P=PIn.

:-export(current_agent_or_var/1).
current_agent_or_var(P):- once(current_agent(PIn)),P=PIn,!.
current_agent_or_var(_).

interesting_to_player(Type,Agent,C):- contains_var(C,Agent),dmsg(agent_database_hook(Type,C)),!.
interesting_to_player(Type,Agent,C):-is_asserted(localityOfObject(Agent,Region)),contains_var(C,Region),dmsg(region_database_hook(Type,C)),!.
interesting_to_player(Type,Agent,C):-is_asserted(localityOfObject(Agent,Region)),is_asserted(localityOfObject(Other,Region)),contains_var(C,Other),!,dmsg(other_database_hook(Type,C)),!.

user:decl_database_hook(Type,C):- current_agent(Agent),interesting_to_player(Type,Agent,C).


with_current_agent(Who,Cmd):- get_session_id(ID),with_assertions(thlocal:session_agent(ID,Who),Cmd).

get_agent_session(P,O):- once(thlocal:session_agent(O,P);thglobal:global_session_agent(O,P)).

foc_current_player(P):- current_agent(P),nonvar(P),!.
foc_current_player(P):- nonvar(P),tAgentGeneric(P),become_player(P),!.
foc_current_player(P):- 
  prolog_must_l([    
             get_session_id(O),
             once(get_dettached_npc(P);generate_new_player(P)),
               retractall(thglobal:global_session_agent(O,P)),
              asserta(thglobal:global_session_agent(O,P)),assert_isa(P,tHumanPlayer),must_det(create_agent(P))]),!.


get_dettached_npc(P):-random_instance_no_throw(tAgentGeneric,P,true),not(isa(P,tHumanPlayer)),!.

% generate_new_player(P):- req(agent(P)),!.
generate_new_player(P):- prolog_must_l([gensym(iPlayer,N),not((isa_asserted(N,tAgentGeneric))),P=N,ensure_new_player(P)]),!.

ensure_new_player(P):- prolog_must_l([assert_isa(P,tExplorer),assert_isa(P,tPlayer),assert_isa(P,tAgentGeneric)]),!.

detatch_player(P):- thglobal:global_session_agent(_,P),!,trace_or_throw(detatch_player(P)).
detatch_player(_).

:-export(become_player/1).
become_player(NewName):- once(current_agent(Was)),Was=NewName,!.
become_player(NewName):- get_session_id(O),retractall(thglobal:global_session_agent(O,_)),detatch_player(NewName),asserta(thglobal:global_session_agent(O,NewName)),ensure_player_stream_local(NewName).
:-export(become_player/2).
become_player(_Old,NewName):-become_player(NewName).

% Lists all the agents in the run. Except for other monsters.
list_agents(Agents) :- agent_list(Agents), !.
list_agents(Agents) :- % build cache
	findall(NearAgent,req(tAgentGeneric(NearAgent)),Agents),
	assert(agent_list(Agents)),!.

:-export((agent_into_corpse/1, display_stats/1)).

% When an agent dies, it turns into a tCorpse.
% corpse is defined as an object in the *.objects.pl files
agent_into_corpse(Agent) :-
	del(mudAtLoc(Agent,LOC)),
	clr(mudStr(Agent,_)),
	clr(mudHeight(Agent,_)),
	clr(mudStm(Agent,_)),
	clr(mudSpd(Agent,_)),
        Newthing = iCorpseFn(Agent),
        assert_isa(Newthing,tCorpse),
	add(mudAtLoc(Newthing,LOC)).

% Displays all the agents stats. Used at end of a run.
display_stats(Agents) :-
	forall(member(Agent,Agents),
	          (mudEnergy(Agent,Chg),
		  mudHealth(Agent,Dam),
		  mudScore(Agent,Scr),
		  findall(Obj,mudPossess(Agent,Obj),Inv),
		  write('Agent = '), write(Agent), nl,
		  write('Charge = '), write(Chg), write('  '),
		  write('Dam= ' ), write(Dam), write('  '),
		  write('Score = '), write(Scr), nl,
		  write('Inventory = '), write(Inv), nl)).
