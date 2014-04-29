% File 'actr.pl'
% Purpose: A test module
% Maintainer: Douglas Miles
%
%

:-module(actr, [ ]).

end_of_file.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Commands to run simulation.
%
% run_tests.  -runs world until agents die or stopped (^C a).
% run_tests(turns,Y).  -runs world for Y turns.
% run_tests(score,Y).  -runs world until score Y is reached by any agent.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
run_tests(turns,Turns):-
	add(total_turns(Turns)),
	run_tests.

run_tests(score,Score) :-
	add(end_score(Score)),
	run_tests.

run_tests :-
        old_setup,
        old_tick,
        old_display.

% old_setup/0, old_tick/0,  old_display/0
% Dynamic predicate declaration.
:- dynamic
  % act_turn/2, str/2, spd/2, stm/2, height/2,
  second_act/1,
  stat_total/2,
  total_turns/1, current_turn/1, end_score/1.

:- include(logicmoo('vworld/vworld_header.pl')).

:- register_module_type(utility).

old_setup:-
       
	must(growth),
	must(list_agents(Agents)),
        fmt('Old Setup creating agents.. ~w',[Agents]),
        must(forall_member(Agent,Agents,create_agent(Agent))).


old_tick:-
        list_agents(Agents),
	run_vworld(Agents).

old_display:-
        list_agents(Agents),
        show_world,
	display_grid_labels,
	display_stats(Agents).

:-dynamic(my_act/1).


% end condition 1
run_vworld(_) :-
	total_turns(Trns),
	del(current_turn(Trns)),
	del(total_turns(_)),
	!.
% end condition 2
run_vworld(_) :-
	end_score(Scr),
	dbase:score(_,AgentScr),
	AgentScr >= Scr,
	del(end_score(_)),
	!.

% no agents
run_vworld([]) :-
	!.
run_vworld(Agents) :-
	who_gets_to_act(Agents,LiveAgents),
	(setof(choice(Time,Agent,Decision),
	      (my_act(Agent),deliberate(Agent,Decision,Time)),
	      ActionList); ActionList = []),
	clr(failure(Agent,_)),
	forall(member(choice(_,Agent,Decision),ActionList),
	       act(Agent,Decision)),
	second_actions,
	spread,
        nl,nl,nl,
	show_world,
        sleep(1),
	dec_turns,
	!,
	run_vworld(LiveAgents).

show_world:- 
   show_room_grid(_).

act(Agent,Decision) :-
	moo:agent_call_command(Agent,Decision),
	!.
/*
act(Agent,Decision) :-
	Decision =.. List,
	nth_member(1,Module,List),
	Module:agent_call_command(Agent,Decision),
	!.
*/


% Check system clock
ticks(T) :-
       !,
       get_time(T).


dec_turns :-
	del(current_turn(X)),
	!,
	Y is X + 1,
	add(current_turn(Y)).

deliberate(Agent,Decision,Time) :-
	add(thinking(Agent)),
	ticks(StartClock),
	once(moo:world_agent_plan(_World,Agent,Decision)),
	ticks(StopClock),
	del(thinking(_)),
	Diff is StopClock - StartClock,
	Time is abs(Diff).

get_world_agent_action_plan(W,Agent,Decision) :-
       get_world_agent_plan(W,Agent,Decision).

get_world_agent_action_plan(_,_,Decision) :-
	Decision = sit.

who_gets_to_act(Agents,LiveAgents) :-
	clr(my_act(_)),
	retractall(second_act(_)),
	still_alive(Agents,LiveAgents),
	forall(member(Agent1,LiveAgents),
	        update_act_turn(Agent1)),
	findall(Agent2,
	       act_turn(Agent2,0),
	       Actlist),
	forall(member(Agent3,Actlist),
	        (asserta(my_act(Agent3)),reset_turn(Agent3))),
	forall(member(Agent4,LiveAgents),
	       (spd(Agent4,Spd),
	        (Spd == 3,
	           current_turn(Trn),
		   even(Trn);
		  Spd == 4),
		asserta(second_act(Agent4)))).

second_actions :-
	(setof(choice(Time,Agent,Decision),
	      (second_act(Agent),deliberate(Agent,Decision,Time)),
	      ActionList); ActionList = []),
	clr(failure(_,_)),
	forall(member(choice(_,Agent,Decision),ActionList),
	       act(Agent,Decision)).

still_alive(Agents,LiveAgents) :-
	forall(member(Agent,Agents),
	       (charge(Agent,Charge),
	       damage(Agent,Damage),
	       (\+ Charge > 0; \+ Damage > 0),
	       agent_into_corpse(Agent),
	       del(act_turn(Agent,_)))),
	findall(Agent1,
	       act_turn(Agent1,_),
	       LiveAgents).

update_act_turn(Agent) :-
	del(act_turn(Agent,Y)),
	X is Y - 1,
	add(act_turn(Agent,X)).

reset_turn(Agent) :-
	spd(Agent,Spd),
	transform_speed(Spd,Wait),
	del(act_turn(Agent,_)),
	add(act_turn(Agent,Wait)).

transform_speed(0,3).
transform_speed(1,2).
transform_speed(_,1).

even(Num) :-
	Mod is (Num mod 2),
	Mod == 0.

:- include(logicmoo('vworld/vworld_footer.pl')).

