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

:- include(prologmud(mud_header)).
/*
% This file is "included" from world.pl 
:-swi_module(modr, [ agent_call_command_unparsed/1, agent_call_command_unparsed/2,  agent_call_command_now/2 ]).


tAgent - Players and Bot Bodies

Sessions - places players control things
      - irc clients (no threads/consoles)
      - telnet users

agent->nick



*/


immediate_session(_P,_C,_O):-!.

do_agent_action_queue(P):- agent_action_queue(P,C,O),must(with_session(O,agent_call_command_unparsed(P,C))),retract(agent_action_queue(P,C,O)).
do_agent_action_queue(_). % was empty already


:-export(enqueue_agent_action/1).
enqueue_agent_action(C):-enqueue_agent_action(_,C).
:-export(enqueue_agent_action/2).
enqueue_agent_action(P,C):-foc_current_agent(P),get_agent_session(P,O),enqueue_agent_action(P,C,O).
:-export(enqueue_agent_action/3).
enqueue_agent_action(P,C,O):- immediate_session(P,C,O),!, do_agent_action(P,C,O).
enqueue_agent_action(P,C,O):- assertz(agent_action_queue(P,C,O)),must(once(pfc_add(agent_action_queue(P,C,O)))),!.

:-export(do_agent_action/1).
do_agent_action(C):-enqueue_agent_action(C).
:-export(do_agent_action/2).
do_agent_action(P,C):-enqueue_agent_action(P,C).
:-export(do_agent_action/2).
do_agent_action(P,C,O):- \+ immediate_session(P,C,O), !, enqueue_agent_action(P,C,O).
do_agent_action(P,C,_):- var(C),!,fmt('unknown_var_command(~q,~q).',[P,C]).
do_agent_action(_,EOF,_):- end_of_file == EOF, !, tick_tock.
do_agent_action(_,'',_):-!, tick_tock.
do_agent_action(P,C,O):- do_gc,with_session(O,agent_call_command_unparsed(P, C)),!.
do_agent_action(P,C,O):-wdmsg('skipping_unknown_player_action(~q,~q).~n',[P,C]),!.


:-export(parse_agent_text_command_checked/5).
parse_agent_text_command_checked(Agent,VERB,ARGS,NewAgent,CMD):- 
   catch(( parse_agent_text_command(Agent,VERB,ARGS,NewAgent,CMD),
         nonvar(CMD),must(nonvar(NewAgent))),'$aborted',true),
         ignore((CMD=actTick)),ignore((NewAgent=Agent)).

parse_agent_text_command_checked(Agent,VERB,ARGS,NewAgent,CMD):- 
   debugging(parser), parse_agent_text_command(Agent,VERB,ARGS,NewAgent,CMD).

must_ac(G):- show_call_failure(must(G)).

:-  message_queue_property(Queue, alias(waq)) -> true;message_queue_create(_,[alias(waq)]).

thread_signal_blocked(ID,Goal):- thread_self(ID),!,Goal.
thread_signal_blocked(ID,Goal):- message_queue_property(Queue, alias(waq)),thread_self(Waiter), thread_signal(ID,(Goal,thread_send_message(Queue,done(Goal,Waiter),[]))),thread_get_message(Queue,done(Goal,Waiter)).


do_agent_action_queues:- repeat,sleep(0.25),once(logOnError(do_agent_action_queue(_))),fail.

start_agent_action_thread:- 
  (thread_property(T,alias(agent_action_queue_thread)) ->
   (thread_property(T,status(running))->true;(thread_join(agent_action_queue_thread,_);
     thread_create(do_agent_action_queues,_,[alias(agent_action_queue_thread)])));
    thread_create(do_agent_action_queues,_,[alias(agent_action_queue_thread)])).


   

% restarts if it it died
user:one_minute_timer_tick:- start_agent_action_thread.

with_session(ID,CALL):-with_assertions(thlocal:session_id(ID),CALL).


% =====================================================================================================================
% agent_call_command_unparsed_0/2 -->  agent_call_command_now/2
% =====================================================================================================================

agent_call_command_unparsed(C):-foc_current_agent(A),!,agent_call_command_unparsed(A,C).

agent_call_command_unparsed(A,C):-  with_assertions(tlbugger:old_no_repeats, agent_call_command_unparsed_0(A,C)).

% execute a prolog command including prolog/0
agent_call_command_unparsed_0(Agent,Var):-var(Var),trace_or_throw(var_agent_call_command_unparsed(Agent,Var)).

agent_call_command_unparsed_0(Agent,Text):-string(Text),atom_string(Atom,Text),!,agent_call_command_unparsed_0(Agent,Atom).

agent_call_command_unparsed_0(Agent,[VERB|ARGS]):-
      debugOnError(parse_agent_text_command_checked(Agent,VERB,ARGS,NewAgent,CMD)),
      must_ac(agent_call_command_now(NewAgent,CMD)),!.

% lists
agent_call_command_unparsed_0(A,Atom):-atom(Atom),atomSplit(Atom,List),must(is_list(List)),!,agent_call_command_unparsed_0(A,List).

% prolog command
agent_call_command_unparsed_0(_Gent,Atom):- atom(Atom), catch((
   (once((read_term_from_atom(Atom,OneCmd,[variables(VARS)]),
      predicate_property(OneCmd,_),
      fmt('doing command ~q~n',[OneCmd]))),!, doall((OneCmd,fmt('Yes: ~w',[VARS]))))),E,(dmsg(E),fail)).

% remove period at end
agent_call_command_unparsed_0(A,PeriodAtEnd):-append(New,[(.)],PeriodAtEnd),!,agent_call_command_unparsed_0(A,New).

% concat the '@'
agent_call_command_unparsed_0(Ag,[A,B|REST]):- atom(A),atom(B),A=='@',atom_concat(A,B,C),!,agent_call_command_unparsed_0(Ag,[C|REST]).

agent_call_command_unparsed_0(A,[L,I|IST]):- atom(L), CMD =.. [L,I|IST],!, must_ac(agent_call_command_now(A,CMD)).

agent_call_command_unparsed_0(A,CMD):- must_ac(agent_call_command_now(A,CMD)),!.

:-export(where_atloc/2).
where_atloc(Agent,Where):-mudAtLoc(Agent,Where).
where_atloc(Agent,Where):-localityOfObject(Agent,Where).
where_atloc(Agent,Where):-mudAtLoc(Agent,Loc),!,locationToRegion(Loc,Where).
where_atloc(Agent,'OffStage'):-fail,nonvar(Agent).


% All Actions must be called from here!
agent_call_command_now(Agent,CMD):- var(CMD),trace_or_throw(var_agent_call_command_now(Agent,CMD)).
agent_call_command_now(Agent,CMD):- subst(CMD,isSelfAgent,Agent,NewCMD),CMD\=@=NewCMD,!,agent_call_command_now(Agent,NewCMD).
agent_call_command_now(Agent,CMD):- correctCommand(CMD,NewCMD),CMD\=@=NewCMD,!,agent_call_command_now(Agent,NewCMD).
agent_call_command_now(Agent,CMD):- \+ where_atloc(Agent,_),!, agent_call_command_now_2(Agent,CMD),!.
agent_call_command_now(Agent,CMD):- where_atloc(Agent,Where),
   % start event
   must(raise_location_event(Where,actNotice(reciever,begin(Agent,CMD)))),
   (debugOnError(agent_call_command_now_2(Agent,CMD)) ->
   % event done
     send_command_completed_message(Agent,Where,done,CMD);
   % event fail
     send_command_completed_message(Agent,Where,failed,CMD)),!.

agent_call_command_now_2(Agent,CMD):- loop_check(agent_call_command_now_3(Agent,CMD),dmsg(looped(agent_call_command_now_2(Agent,CMD)))).
agent_call_command_now_3(Agent,CMD):-
   with_agent(Agent,
     with_assertions(thlocal:side_effect_ok,
     with_assertions(thlocal:agent_current_action(Agent,CMD),
  (user:agent_call_command(Agent,CMD)*->true;user:agent_call_command_fallback(Agent,CMD))))),
  padd(Agent,mudLastCommand(CMD)).


:-export(send_command_completed_message/4).
send_command_completed_message(Agent,Where,Done,CMD):-
     ignore((must_det_l([flush_output,renumbervars(CMD,SCMD),Message =..[Done,Agent,SCMD],
                raise_location_event(Where,actNotice(reciever,Message)),flush_output]))),!.


correctCommand(CMD,OUT):-compound(CMD),fail,
   must(current_agent(Who)),
   CMD=..[F|ARGS],   
   functor(CMD,F,A),
   functor(MATCH,F,A),
   vtActionTemplate(MATCH),compound(MATCH),MATCH=..[F|TYPES],!,
   correctEachTypeOrFail(Who,F,query(t, must),ARGS,TYPES,NEWS),!,
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


:-export(current_agent/1).
current_agent(PIn):- get_session_id(O),get_agent_session(P,O),!,P=PIn.

:-export(current_agent_or_var/1).
current_agent_or_var(P):- once(current_agent(PIn)),P=PIn,!.
current_agent_or_var(_).

interesting_to_player(Type,Agent,C):- contains_var(C,Agent),dmsg(agent_database_hook(Type,C)),!.
interesting_to_player(Type,Agent,C):-is_asserted(localityOfObject(Agent,Region)),contains_var(C,Region),dmsg(region_database_hook(Type,C)),!.
interesting_to_player(Type,Agent,C):-is_asserted(localityOfObject(Agent,Region)),is_asserted(localityOfObject(Other,Region)),contains_var(C,Other),!,dmsg(other_database_hook(Type,C)),!.

user:decl_database_hook(Type,C):- current_agent(Agent),interesting_to_player(Type,Agent,C).

get_agent_input_stream(P,In):-no_repeats(P-In,(get_agent_session(P,O),thglobal:session_io(O,In,_,_))).

get_agent_input_thread(P,Id):-no_repeats(P-Id,(get_agent_input_stream(P,O),thglobal:session_io(_,In,_,Id))).


with_agent(P,CALL):-
 get_session_id(TS),must(nonvar(TS)),
 thread_self(Self),
 ((get_agent_session(P,O),thglobal:session_io(O,In,Out,Id),Id\=Self)->Wrap=thread_signal_blocked(Id);Wrap=call),!,
  call(Wrap, 
    with_assertions([put_server_no_max,thglobal:session_agent(TS,P),thglobal:agent_session(P,TS)],
      with_output_to_pred(deliver_event(P),CALL))).

has_tty(O):-no_repeats(O,thglobal:session_io(O,_,_,_)).

get_agent_session(P,O):-get_session_id(O),get_agent_sessions(P,O),!.
get_agent_session(P,O):-get_agent_sessions(P,O),has_tty(O).
:-export(get_agent_sessions/2).
get_agent_sessions(P,O):- no_repeats(P-O,(thglobal:session_agent(O,P);thglobal:agent_session(P,O);(irc_user_plays(P,O,C),ground(irc_user_plays(P,O,C))))).

:-export(foc_current_agent/1).
foc_current_agent(P):- current_agent(P),nonvar(P),!.
foc_current_agent(P):- nonvar(P),tAgent(P),!,become_player(P),!.
foc_current_agent(P):- 
  must_det_l([    
             get_session_id(O),
             once((get_dettached_npc(NPC),NPC=P);generate_new_player(P)),
             become_player(P)]),!.
               

:-user:ensure_loaded(library(http/http_session)).

get_session_id(IDIn):-guess_session_ids(ID),nonvar(ID),!,ID=IDIn.

% return any thread locally set session
guess_session_ids(ID):-thlocal:session_id(ID).
% irc session
guess_session_ids(ID):-if_defined(thlocal:default_user(ID)).
% telnet session
guess_session_ids(ID):-thread_self(TID),thglobal:session_io(ID,_,_,TID).
% returns http sessions as well
guess_session_ids(ID):-if_defined(http_in_session(ID)).
% tcp session
guess_session_ids(ID):-thread_self(TID),thread_property(TID,alias(ID)).
% anonymous sessions
guess_session_ids(ID):-thread_self(ID), \+ thread_property(ID,alias(ID)).
% anonymous sessions
guess_session_ids(In):-thread_self(ID),thread_util:has_console(ID,In,Out,Err).


:-export(my_random_member/2).
my_random_member(LOC,LOCS):- must_det((length(LOCS,Len),Len>0)),random_permutation(LOCS,LOCS2),!,member(LOC,LOCS2).

:-export(random_instance/3).
random_instance_no_throw(Type,Value,Test):-var(Test),!,Test=isa(Test,Type),random_instance_no_throw(Type,Value,Test).
random_instance_no_throw(Type,Value,Test):- copy_term(ri(Type,Value,Test),ri(RType,RValue,RTest)),
   hooked_random_instance(RType,RValue,RTest),
   checkAnyType(query(_,_),RValue,Type,Value),
   must_det(Test),!.
random_instance_no_throw(Type,Value,Test):- atom(Type),atom_concat('random_',Type,Pred),Fact=..[Pred,Value],predicate_property(Fact,_),call(Fact),Test,!.
random_instance_no_throw(Type,Value,Test):- compound(Type),get_functor(Type,F),atom_concat('random_',F,Pred),current_predicate(F/1),Fact=..[Pred,Value],predicate_property(Fact,_),Fact,Test,!.
random_instance_no_throw(Type,Value,Test):- compound(Type),get_functor(Type,F,GA),guess_arity(F,GA,A),functor(Formatted,F,A),t(meta_argtypes,Formatted),
                         Formatted=..[F|ArgTypes],functor(Value,F,A),Value=..[F|ValueArgs],must((maplist(random_instance_no_throw,ArgTypes,ValueArgs,_),Test)),!.
random_instance_no_throw(Type,Value,Test):-must(( findall(V,isa(V,Type),Possibles),Possibles\=[])),must_det((my_random_member(Value,Possibles),Test)),!.

random_instance(Type,Value,Test):- must(random_instance_no_throw(Type,Value,Test)).



get_dettached_npc(P):- random_instance_no_throw(tAgent,P,\+ isa(P,tHumanPlayer)).

generate_new_player(P):- var(P),!,must_det_l([gensym(iExplorer,N),not((isa_asserted(N,tAgent))),P=N,ensure_new_player(P)]),!.
generate_new_player(P):- ensure_new_player(P),!.

ensure_new_player(P):- must_det_l([nonvar(P),assert_isa(P,tExplorer),assert_isa(P,tPlayer),assert_isa(P,tAgent)]),!.

detach_player(P):- thglobal:agent_session(P,_),!,trace_or_throw(detach_player(P)).
detach_player(_).

:-export(become_player/1).
become_player(P):- once(current_agent(Was)),Was=P,!.
become_player(P):- get_session_id(O),retractall(thglobal:agent_session(_,O)),
  assert_isa(P,tHumanPlayer),must_det(create_agent(P)),
  detach_player(P),asserta_new(thglobal:agent_session(P,O)).

:-export(become_player/2).
become_player(_Old,NewName):-become_player(NewName).

% Lists all the agents in the run. Except for other monsters.
list_agents(Agents) :- agent_list(Agents), !.
list_agents(Agents) :- % build cache
	findall(NearAgent,req(tAgent(NearAgent)),Agents),
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