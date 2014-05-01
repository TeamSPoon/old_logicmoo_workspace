/** <module> 
% Uses timers to make sure all Agents get a chance to do their things
%
% Project Logicmoo: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
:- module(toploop_npc, [
          move_or_sit_memory_idea/3,
          npc_tick/0,
          join_npcs_long_running/0,npc_tick_tock/0,npc_tick_tock_time/1,
          npc_controller/2,   
          get_world_agent_plan/3,
          tick_controller/2]).

:- include(logicmoo('vworld/moo_header.pl')).

:- dynamic(npc_tick_tock_time/1).
npc_tick_tock_time(30).

npc_tick_tock:-
   npc_tick_tock_time(Time),sleep(Time),
   npc_tick.

npc_tick:-
   join_npcs_long_running,   
   forall(npc_controller(What,Who),in_thread_and_join(debugOnError(tick_controller(What,Who)))).

join_npcs_long_running.

% skip manually controled agents
npc_controller(simple_world_agent_plan,Who):- req(agent(Who)),not(agent_message_stream(Who,_,_)).

tick_controller(simple_world_agent_plan,Who):- tick(Who).

ggtrace:- visible(+all), leash(-exit),leash(-fail),leash(-call),leash(-redo).
%:-ggtrace.

move_or_sit_memory_idea(Agent,move(Dir),[Outlet]) :-
	memory(Agent,directions([Dir|_])),
	num_near(Num,Dir,here),
	get_near(Agent,List),
	nth1(Num,List,What),
	(What == [];
	What == [Outlet]).
move_or_sit_memory_idea(Agent,sit,_) :-
        req(memory(Agent,directions(Old))),
	del(memory(Agent,directions(Old))),
	random_permutation(Old,New),
	add(memory(Agent,directions(New))).

tick(Who):-
   findall(Idea,get_world_agent_plan(current,Who,Idea),IdeaS),!,IdeaS=[_|_],
   random_member(Idea,IdeaS),!,
   do_agent_call_plan_command(Who,Idea).



get_world_agent_plan(W,Who,Idea):- agent(Who), moo:world_agent_plan(W,Who,Idea).

do_agent_call_plan_command(A,C):-agent_doing(A,C),!.
do_agent_call_plan_command(A,C):-   
   with_assertions(agent_doing(A,C),
    (( call_agent_command(A,C),agent_done(A,C)))).



:- debug .

idea(Who,IdeaS):- findall(Idea,(get_world_agent_plan(current,Who,Idea),dmsg(get_world_agent_plan(current,Who,Idea))),IdeaS),(IdeaS==[]->dmsg(noidea(idea(Who)));true).

moo:decl_action(npc_timer(int),"sets how often to let NPCs run").
moo:decl_action(tock,"Makes All NPCs do something brilliant").
moo:decl_action(tick(agent),"Makes some agent do something brilliant").
moo:decl_action(idea(optional(agent,self)),"Makes some agent (or self) think of something brilliant").
moo:decl_action(tick,"Makes *your* agent do something brilliant").
moo:decl_action(prolog,"Call prolog toploop").

moo:agent_call_command(_,prolog) :- prolog.
moo:agent_call_command(_Agent,npc_timer(Time)):-retractall(npc_tick_tock_time(_)),asserta(npc_tick_tock_time(Time)).
moo:agent_call_command(Who,tick) :-  catch(tick(Who),E,(dmsg(tick(E, Who)),debug,ggtrace,trace,tick(Who))).
moo:agent_call_command(_Agent,idea(Who)) :-  catch(idea(Who,_),E,(dmsg(idea(E, Who)),debug,ggtrace,trace,idea(Who,_))).
moo:agent_call_command(_Agent,tock) :- npc_tick.
moo:agent_call_command(_Agent,tick(Other)) :- in_thread_and_join(tick(Other)).

:- include(logicmoo('vworld/moo_footer.pl')).

