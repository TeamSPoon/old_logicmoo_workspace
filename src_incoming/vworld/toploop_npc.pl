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
          tick/1,
          npc_controller/2,   
          warnOnError/1,
          get_world_agent_plan/3,
          tick_controller/2]).

:- meta_predicate warnOnError(0).
:- meta_predicate agent_call_safely(?,?,?).

:- include(logicmoo(vworld/moo_header)).
% :- moo:begin_transform_moo_preds.
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
npc_controller(simple_world_agent_plan,Who):- isa(Who,agent),not(thglobal:agent_message_stream(Who,_,_,_)).

tick_controller(simple_world_agent_plan,Who):- tick(Who).

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
   ignore(current_agent(Who)),
   findall(Idea,get_world_agent_plan(current,Who,Idea),IdeaS),!,IdeaS=[_|_],
   my_random_member(Idea,IdeaS),!,
   do_agent_call_plan_command(Who,Idea).


 
get_world_agent_plan(W,Who,Idea):- agent(Who), moo:world_agent_plan(W,Who,Idea).

do_agent_call_plan_command(A,C):- agent_current_action(A,C),!.
do_agent_call_plan_command(A,C):-   
   with_assertions(agent_current_action(A,C),
    (( call_agent_command(A,C),agent_done(A,C)))).



:- debug .

idea(Who,IdeaS):- findall(Idea,(get_world_agent_plan(current,Who,Idea),dmsg(get_world_agent_plan(current,Who,Idea))),IdeaS),(IdeaS==[]->dmsg(noidea(idea(Who)));true).

moo:action_info(npc_timer(int),"sets how often to let NPCs run").
moo:action_info(tock,"Makes All NPCs do something brilliant").
moo:action_info(tick(agent),"Makes some agent do something brilliant").
moo:action_info(idea(optional(agent,self)),"Makes some agent (or self) think of something brilliant").
moo:action_info(tick,"Makes *your* agent do something brilliant").
moo:action_info(prolog(prolog),"Call prolog toploop").

moo:agent_text_command(Agent,[prolog,X],Agent,prologCall(X)):-ignore(X=someCode).
moo:agent_text_command(Agent,[prolog],Agent,prologCall(user:prolog_repl)).
moo:agent_text_command(Agent,[tlocals],Agent,prologCall(user:tlocals)).

warnOnError(X):-catch(X,E,dmsg(error(E:X))).

moo:agent_call_command(Agent,prologCall(C)) :-  must(nonvar(C)), agent_call_safely(Agent,C).
moo:agent_call_command(Agent,prolog(C)) :- must(nonvar(C)),agent_call_safely(Agent,C).

:-export(agent_call_safely/2).
agent_call_safely(_Agnt,C):- any_to_callable(C,X,Vars), !, gensym(result_count_,RC),flag(RC,_,0),agent_call_safely(RC,X,Vars),flag(RC,CC,CC),fmt(result_count(CC)).
agent_call_safely(RC,X,[]) :- !, '@'(notrace((warnOnError(doall(((X,flag(RC,CC,CC+1),fmt(cmdresult(X,true)))))))),user).
agent_call_safely(RC,X,Vars) :-  '@'(notrace((warnOnError(doall(((X,flag(RC,CC,CC+1),fmt(cmdresult(X,Vars)))))))),user).

atom_to_term_safe(A,T,O):-catch(atom_to_term(A,T,O),_,fail),T\==end_of_file.
any_to_callable(S,X,Vs):-string(C),!,string_to_atom(S,C),atom_to_term_safe(C,X,Vs).
any_to_callable(C,X,Vs):-atom(C),!,atom_to_term_safe(C,X,Vs).
any_to_callable(C,X,Vs):- (expand_goal(C,X)),term_variables((C,X),Vs),!.
% any_to_callable(C,X,Vs):-force_expand(expand_goal(C,X)),term_variables((C,X),Vs),!.

moo:agent_call_command(_Agent,npc_timer(Time)):-retractall(npc_tick_tock_time(_)),asserta(npc_tick_tock_time(Time)).
moo:agent_call_command(Who,tick) :-  debugOnError(tick(Who)).
moo:agent_call_command(_Agent,idea(Who)) :-  catch(idea(Who,_),E,(dmsg(idea(E, Who)),ggtrace,idea(Who,_))).
moo:agent_call_command(_Agent,tock) :- npc_tick.
moo:agent_call_command(_Agent,tick(Other)) :- in_thread_and_join(tick(Other)).

:- include(logicmoo(vworld/moo_footer)).

