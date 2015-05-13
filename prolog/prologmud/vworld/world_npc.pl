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
          command_actTick/1,
          npc_controller/2,   
          warnOnError/1,
          get_world_agent_plan/3,
          tick_controller/2]).

:- meta_predicate warnOnError(0).
:- meta_predicate agent_call_safely(?,?,?).


:- include(prologmud(mud_header)).
% :- begin_transform_moo_preds.
:- dynamic(npc_tick_tock_time/1).
npc_tick_tock_time(60).

npc_tick_tock:-
   npc_tick_tock_time(Time),sleep(Time),
   npc_tick.

npc_tick:-
   join_npcs_long_running, 
   findall(What-Who,npc_controller(What,Who),List),!,
   my_random_member(What-Who,List),!,
   ignore(in_thread_and_join(debugOnError(tick_controller(What,Who)))).

join_npcs_long_running.

% skip manually controled agents
npc_controller(simple_world_agent_plan,Who):- isa(Who,tAgent),not(thglobal:agent_message_stream(Who,_,_,_)).

tick_controller(simple_world_agent_plan,Who):- command_actTick(Who).

%:-ggtrace.

move_or_sit_memory_idea(Agent,actMove(Dir),[Outlet]) :- 
	mudMemory(Agent,aDirectionsFn([Dir|_])),
	number_to_dir(Num,Dir,vHere),
	mudNearReach(Agent,List),
	nth1(Num,List,What),
	(What == [];
	What == [Outlet]).
move_or_sit_memory_idea(Agent,actSit,_) :-
        req(mudMemory(Agent,aDirectionsFn(Old))),
	del(mudMemory(Agent,aDirectionsFn(Old))),
	random_permutation(Old,New),
	add(mudMemory(Agent,aDirectionsFn(New))).


command_actTick(Who):- (side_effect_prone),
   ignore(current_agent(Who)),
   must(nonvar(Who)),
   with_current_agent(Who,
     must_det_l((
      show_call_failure(current_agent(Who)),
      command_actIdea(Who,IdeaS),
      my_random_member(Idea,IdeaS),!,
      do_agent_call_plan_command(Who,Idea)))).


 
get_world_agent_plan(W,Who,Idea):-with_current_agent(Who,call_no_cuts(world_agent_plan(W,Who,Idea))).

do_agent_call_plan_command(A,C):- thlocal:agent_current_action(A,CC),dmsg(too_busy(CC,agent_call_plan_command(A,C))),!.
do_agent_call_plan_command(A,C):-   
   with_current_agent(A,with_assertions(thlocal:agent_current_action(A,C), call_agent_command(A,C))).


command_actIdea(Who,IdeaS):- nonvar(Who),
  side_effect_prone,
  findall(Idea,
        (get_world_agent_plan(current,Who,Idea),
             dmsg(get_world_agent_plan(current,Who,Idea))),IdeaS),
  (IdeaS==[]->dmsg(noidea(actIdea(Who)));true).

user:action_info(actNpcTimer(ftInt),"sets how often to let NPCs run").

user:action_info(actTock,"Makes All NPCs do something brilliant").
user:action_info(actTick(tAgent),"Makes some agent do something brilliant").
user:action_info(actTick,"Makes *your* agent do something brilliant").

user:action_info(actIdea(isOptional(tAgent,isSelfAgent)),"Makes some agent (or self) think of something brilliant").
user:action_info(actProlog(ftCallable),"Call a ftCallable").

user:agent_text_command(Agent,["prolog",X],Agent,actProlog(X)):-ignore(X=isRandom(ftCallable)).
user:agent_text_command(Agent,["prolog"],Agent,actProlog(prolog_repl)).
% user:agent_text_command(Agent,["tlocals"],Agent,actProlog(tlocals)).

warnOnError(X):-catch(X,E,dmsg(error(E:X))).

user:agent_call_command(Agent,actProlog(C)) :- (side_effect_prone),true,nonvar(C),agent_call_safely(Agent,C).

:-export(agent_call_safely/2).
agent_call_safely(_Agnt,C):- any_to_callable(C,X,Vars), !, gensym(result_count_,RC),flag(RC,_,0),agent_call_safely(RC,X,Vars),flag(RC,CC,CC),fmt(result_count(CC)).
agent_call_safely(RC,X,[]) :- !, '@'(hotrace((warnOnError(doall(((X,flag(RC,CC,CC+1),fmt(cmdresult(X,true)))))))),user).
agent_call_safely(RC,X,Vars) :-  '@'(hotrace((warnOnError(doall(((X,flag(RC,CC,CC+1),fmt(cmdresult(X,Vars)))))))),user).

atom_to_term_safe(A,T,O):-catch(atom_to_term(A,T,O),_,fail),T\==end_of_file.
any_to_callable(S,X,Vs):-string(C),!,string_to_atom(S,C),atom_to_term_safe(C,X,Vs).
any_to_callable(C,X,Vs):-atom(C),!,atom_to_term_safe(C,X,Vs).
any_to_callable(C,X,Vs):- (expand_goal(C,X)),term_variables((C,X),Vs),!.
% any_to_callable(C,X,Vs):-force_expand(expand_goal(C,X)),term_variables((C,X),Vs),!.

user:agent_call_command(_Agent,actNpcTimer(Time)):-retractall(npc_tick_tock_time(_)),asserta(npc_tick_tock_time(Time)).
user:agent_call_command(Who,actTick) :-  debugOnError(command_actTick(Who)).
user:agent_call_command(_Agent,actIdea(Who)) :-  must(command_actIdea(Who,Idea)),fmt(result_actIdea(Who,Idea)).
user:agent_call_command(_Agent,actTock) :- (side_effect_prone), npc_tick.
user:agent_call_command(_Agent,actTick(Other)) :-(side_effect_prone), user:agent_call_command(Other,actTick).

:- include(prologmud(mud_footer)).
