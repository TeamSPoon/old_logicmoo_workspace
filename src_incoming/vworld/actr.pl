% ===================================================================
% File 'run.pl'
% Purpose: An Implementation a MUD server in SWI-Prolog
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'run.pl' 1.0.0
% Revision:  $Revision: 1.7 $
% Revised At:   $Date: 2002/07/11 21:57:28 $
% ===================================================================
%
% This file DECLAREs the agent_action/2 and implement/2 as MULTIFILE
% Very simple... but kept separate to maintain modularity
%

:-module(actr, [ 
 %memory/2,
         % str/2, spd/2, stm/2, height/2,
         run_tests/0, run_tests/2,
         set_stats/2,
         process_stats/2,
         init/0,
         check_stat_total/1,
         old_setup/0, old_tick/0,  old_display/0,
         worth/3]).

% Dynamic predicate declaration.
:- dynamic
  % act_turn/2, str/2, spd/2, stm/2, height/2,
  second_act/1,
  stat_total/2,
  total_turns/1, current_turn/1, end_score/1.

:- include(logicmoo('vworld/vworld_header.pl')).

:- register_module_type(utility).

% Used by eat.pl and take.pl
% Is the object worth anything (either scored points or charge)
% Score any points?
worth(Agent,Action,Obj) :-
	props(Obj,act(Action,score(S))),
	del(score(Agent,Y)),
	X is Y + S,
	add(score(Agent,X)),
	fail. % fail to check for charge too
% Charge up those batteries
worth(Agent,Action,Obj) :-
           props(Obj,act(Action,charge(NRG))),
	charge(Agent,Chg),
	stm(Agent,Stm),
	max_charge(Max),
	(Chg + NRG) < (((Stm * 10) -20) + Max),
	del(charge(Agent,Y)),
	X is Y + NRG,
	add(charge(Agent,X)),
	fail. % fail to check for healing
% Heal
worth(Agent,Action,Obj) :-
           props(Obj,act(Action,heal(Hl))),
	damage(Agent,Dam),
	stm(Agent,Stm),
	str(Agent,Str),
	max_damage(Max),
	(Dam + Hl) < ((((Stm * 10) -20) + ((Str * 5) - 10)) + Max),
	del(charge(Agent,Y)),
	X is Y + Hl,
	add(charge(Agent,X)),
	!.
worth(_,_,_).



% Initialize world.
% This keeps the old databases messing with new runs.
           

:-dynamic(spawn_objects/1).

% Check to see if any of the objects should be placed in the world as it runs.

init :- world_clear(current),retractall(spawn_objects(_)).

growth :-
	findall(([Obj,Chance]),
	    props(Obj,spawn_rate(Chance)),
	    Objects),
	assert(spawn_objects(Objects)).

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

old_setup:-
	must(init),
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
       moo:world_agent_plan(W,Agent,Decision).

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

% For Quintus alter 'Y is random(LLL)' to random(0,LLL,Y)
% defined pred maybe.... be sure to comment this pred out before going
% back to quitus (located below).
spread :-
	spawn_objects(Objects),
	forall(prop_memb([Object,Chance],Objects),
	spread(Object,Chance)).
spread(Object,Chance) :-        
	maybe(Chance),!,
        gensym(Object,Named),
        create_instance(Named,Object,[]),!.

spread(_,_).

set_stats(Agent,[]) :-
	add(str(Agent,2)),
	add(height(Agent,2)),
	add(stm(Agent,2)),
	add(spd(Agent,2)).

set_stats(Agent,Traits) :-
        clr(stat_total(Agent,0)),
	add(stat_total(Agent,0)),
	forall(member(Trait,Traits),
	       ignore(catch(process_stats(Agent,Trait),_,true))),
	       catch(check_stat_total(Agent),_,true).

process_stats(Agent,str(Y)) :-
	add(str(Agent,Y)),
	must(del(damage(Agent,Dam))),
	NewDam is (Dam + ((Y * 5) - 10)),
	add(damage(Agent,NewDam)),
	del(stat_total(Agent,T)),
	NT is T + Y,
	add(stat_total(Agent,NT)).

process_stats(Agent,height(Ht)) :-
	add(height(Agent,Ht)),
	del(stat_total(Agent,T)),
	NewT is T + Ht,
	add(stat_total(Agent,NewT)).

process_stats(Agent,stm(Stm)) :-
	add(stm(Agent,Stm)),
	del(damage(Agent,Dam)),
	NewDam is (((Stm * 10) - 20) + Dam),
	add(damage(Agent,NewDam)),
	del(charge(Agent,NRG)),
	Charge is (((Stm * 10) - 20) + NRG),
	add(charge(Agent,Charge)),
	del(stat_total(Agent,Total)),
	NewT is Total + Stm,
	add(stat_total(Agent,NewT)).

process_stats(Agent,spd(Spd)) :-
	add(spd(Agent,Spd)),
	del(stat_total(Agent,T)),
	NewT is T + Spd,
	add(stat_total(Agent,NewT)).

check_stat_total(Agent) :-
	del(stat_total(Agent,Total)),
	Total > 12,
	nl,
	write('Agent '),
	write(Agent),
	write(' has more than 12 points in their triats.'),
	nl,
	write('Exiting....'),
	nl,
	abort.
check_stat_total(_).

even(Num) :-
	Mod is (Num mod 2),
	Mod == 0.


:- include(logicmoo('vworld/vworld_footer.pl')).
