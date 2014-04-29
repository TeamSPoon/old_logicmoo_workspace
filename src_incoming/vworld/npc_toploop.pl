%
% Dec 13, 2035
% Douglas Miles
%
/** <module> 
% Loops to make sure all Agents get a chance to do their things
% Comments below document the basic idea.
%
*/

:- module(npc_toploop, [
          npc_tick/0,
          join_npcs_long_running/0,npc_tick_tock/0,npc_tick_tock_time/1,
          npc_controller/2,          
          tick_controller/2]).

:- include(logicmoo('vworld/vworld_header.pl')).

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
npc_controller(simple_world_agent_plan,Who):-dbase:agent(Who),not(agent_message_stream(Who,_,_)).

tick_controller(simple_world_agent_plan,Who):- tick(Who).

tick(Who):- 
   findall(Idea,get_world_agent_plan(current,Who,Idea),IdeaS),!,IdeaS=[_|_],
   random_member(Idea,IdeaS),!,
   do_agent_call_plan_command(Who,Idea).


get_world_agent_plan(W,Who,Idea):- agent(Who),moo:world_agent_plan(W,Who,Idea).

do_agent_call_plan_command(A,C):-agent_doing(A,C),!.
do_agent_call_plan_command(A,C):-   
   with_assertions(agent_doing(A,C),((
   call_agent_command(A,C),agent_done(A,C)))).




moo:decl_action(npc_timer(int),"sets how often to let NPCs run").
moo:decl_action(tock,"Makes All NPCs do something brilliant").
moo:decl_action(tick(agent),"Makes some agent do something brilliant").
moo:decl_action(tick,"Makes *your* agent do something brilliant").

moo:agent_call_command(_Agent,npc_timer(Time)):-retractall(npc_tick_tock_time(_)),asserta(npc_tick_tock_time(Time)).
moo:agent_call_command(Agent,tick) :- tick(Agent).
moo:agent_call_command(_Agent,tock) :- npc_tick.
moo:agent_call_command(_Agent,tick(Other)) :- in_thread_and_join(tick(Other)).

:- include(logicmoo('vworld/vworld_footer.pl')).

