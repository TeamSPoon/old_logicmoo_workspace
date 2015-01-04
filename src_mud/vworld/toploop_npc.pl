/** <module> 
% Uses timers to make sure all Agents get a chance to do their things
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
:-swi_module(toploop_npc, [
          move_or_sit_memory_idea/3,
          npc_tick/0,
          join_npcs_long_running/0,npc_tick_tock/0,npc_tick_tock_time/1,
          actTick/1,
          npc_controller/2,   
          warnOnError/1,
          get_world_agent_plan/3,
          tick_controller/2]).

:- meta_predicate warnOnError(0).
:- meta_predicate agent_call_safely(?,?,?).

:- include(logicmoo(vworld/moo_header)).
% :- begin_transform_moo_preds.
:- dynamic(npc_tick_tock_time/1).
npc_tick_tock_time(60).

npc_tick_tock:-
   npc_tick_tock_time(Time),sleep(Time),
   npc_tick.

npc_tick:-
   join_npcs_long_running,   
   forall(npc_controller(What,Who),ignore(in_thread_and_join(debugOnError(tick_controller(What,Who))))).

join_npcs_long_running.

% skip manually controled agents
npc_controller(simple_world_agent_plan,Who):- mudIsa(Who,tAgentGeneric),not(thglobal:agent_message_stream(Who,_,_,_)).

tick_controller(simple_world_agent_plan,Who):- actTick(Who).

%:-ggtrace.

move_or_sit_memory_idea(Agent,actMove(Dir),[Outlet]) :- 
	mudMemory(Agent,directions([Dir|_])),
	number_to_dir(Num,Dir,vHere),
	mudNearReach(Agent,List),
	nth1(Num,List,What),
	(What == [];
	What == [Outlet]).
move_or_sit_memory_idea(Agent,actSit,_) :-
        req(mudMemory(Agent,directions(Old))),
	del(mudMemory(Agent,directions(Old))),
	random_permutation(Old,New),
	add(mudMemory(Agent,directions(New))).


actTick(Who):-
   ignore(current_agent(Who)),
   findall(Idea,get_world_agent_plan(current,Who,Idea),IdeaS),!,IdeaS=[_|_],
   my_random_member(Idea,IdeaS),!,
   do_agent_call_plan_command(Who,Idea).


 
get_world_agent_plan(W,Who,Idea):- tAgentGeneric(Who), world_agent_plan(W,Who,Idea).

do_agent_call_plan_command(A,C):- agent_current_action(A,C),!.
do_agent_call_plan_command(A,C):-   
   with_assertions(agent_current_action(A,C),
    (( call_agent_command(A,C),agent_done(A,C)))).



:- debug .


actIdea(Who,IdeaS):- findall(Idea,(get_world_agent_plan(current,Who,Idea),dmsg(get_world_agent_plan(current,Who,Idea))),IdeaS),(IdeaS==[]->dmsg(noidea(actIdea(Who)));true).

action_info(actNpcTimer(ftInt),"sets how often to let NPCs run").
action_info(actTock,"Makes All NPCs do something brilliant").
action_info(actTick(tAgentGeneric),"Makes some agent do something brilliant").
action_info(actIdea(isOptional(tAgentGeneric,isAgentSelf)),"Makes some agent (or self) think of something brilliant").
action_info(actTick,"Makes *your* agent do something brilliant").
action_info(actProlog(tCallable),"Call a tCallable").

agent_text_command(Agent,["prolog",X],Agent,actProlog(X)):-ignore(X=someCode).
agent_text_command(Agent,["prolog"],Agent,actProlog(user:prolog_repl)).
agent_text_command(Agent,["tlocals"],Agent,actProlog(user:tlocals)).

warnOnError(X):-catch(X,E,dmsg(error(E:X))).

agent_call_command(Agent,actProlog(C)) :- must(nonvar(C)),agent_call_safely(Agent,C).

:-swi_export(agent_call_safely/2).
agent_call_safely(_Agnt,C):- any_to_callable(C,X,Vars), !, gensym(result_count_,RC),flag(RC,_,0),agent_call_safely(RC,X,Vars),flag(RC,CC,CC),fmt(result_count(CC)).
agent_call_safely(RC,X,[]) :- !, '@'(notrace((warnOnError(doall(((X,flag(RC,CC,CC+1),fmt(cmdresult(X,true)))))))),user).
agent_call_safely(RC,X,Vars) :-  '@'(notrace((warnOnError(doall(((X,flag(RC,CC,CC+1),fmt(cmdresult(X,Vars)))))))),user).

atom_to_term_safe(A,T,O):-catch(atom_to_term(A,T,O),_,fail),T\==end_of_file.
any_to_callable(S,X,Vs):-string(C),!,string_to_atom(S,C),atom_to_term_safe(C,X,Vs).
any_to_callable(C,X,Vs):-atom(C),!,atom_to_term_safe(C,X,Vs).
any_to_callable(C,X,Vs):- (expand_goal(C,X)),term_variables((C,X),Vs),!.
% any_to_callable(C,X,Vs):-force_expand(expand_goal(C,X)),term_variables((C,X),Vs),!.

agent_call_command(_Agent,actNpcTimer(Time)):-retractall(npc_tick_tock_time(_)),asserta(npc_tick_tock_time(Time)).
agent_call_command(Who,actTick) :-  debugOnError(actTick(Who)).
agent_call_command(_Agent,actIdea(Who)) :-  catch(actIdea(Who,_),E,(dmsg(actIdea(E, Who)),ggtrace,actIdea(Who,_))).
agent_call_command(_Agent,actTock) :- npc_tick.
agent_call_command(_Agent,actTick(Other)) :- in_thread_and_join(actTick(Other)).

:- include(logicmoo(vworld/moo_footer)).
