/** <module> 
% This file gives a common place where world effects 
% (such as carrying  shield or being drunk) are implemented
%
% Project LogicMoo: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
% :- module(world_effects,[]).
/*
% This file is "included" from world.pl 
*/




% Used by eat.pl and take.pl
% Is the object worth anything (either scored points or charge)
% Score any points?
worth(Agent,Action,Obj) :-
	props(Obj,act_affect(Action,score(S))),
	add(score(Agent,+S)),
	fail. % fail to check for charge too
% Charge up those batteries
worth(Agent,Action,Obj) :-
           props(Obj,act_affect(Action,charge(NRG))),
	req(charge(Agent,Chg)),
	req(stm(Agent,Stm)),
	moo:max_charge(Agent,Max),
	(Chg + NRG) < (((Stm * 10) -20) + Max),
	add(charge(Agent,+NRG)),
	fail. % fail to check for healing
% Heal
worth(Agent,Action,Obj) :-
           props(Obj,act_affect(Action,heal(Hl))),
	req((damage(Agent,Dam),
             stm(Agent,Stm),
             str(Agent,Str))),
	req(max_damage(Agent,Max)),
	(Dam + Hl) < ((((Stm * 10) -20) + ((Str * 5) - 10)) + Max),
	add(charge(Agent,+Hl)),
	!.
worth(_,_,_).


% Check to see if last action was successful or not
:-export(success/2).
success(Agent,no) :- cmdfailure(Agent,_),!.
success(_,yes).

:-export(add_cmdfailure/2).
add_cmdfailure(Agent,What):-add(cmdfailure(Agent,What)).

hook:decl_database_hook(assert(_),cmdfailure(Agent,What)):- once(del(cmdsuccess(Agent,What));clr(cmdsuccess(Agent,_))).

% Initialize world.
% This keeps the old databases messing with new runs.
           

:-dynamic(spawn_objects/1).

% Check to see if any of the objects should be placed in the world as it runs.

:-export(call_update_charge/2).
call_update_charge(Agent,What):- padd(Agent,cmdsuccess(What)), doall(must(moo:update_charge(Agent,What))),!.

:-export(call_update_stats/2).
call_update_stats(Agent,What):- padd(Agent,cmdsuccess(What)), doall(must(moo:update_stats(Agent,What))),!.

set_stats(Agent,[]) :- set_stats(Agent,[str(2),height(2),stm(2),spd(2)]).

set_stats(Agent,Traits) :-
        clr(stat_total(Agent,_)),
	add(stat_total(Agent,0)),
	forall(member(Trait,Traits),
	       ignore(catch(process_stats(Agent,Trait),E,dmsg(E:process_stats(Agent,Trait))))),
               ignore(catch(check_stat_total(Agent),E2,dmsg(E2:check_stat_total(Agent)))).
set_stats(Agent,Traits):-dmsg(warn(failed(set_stats(Agent,Traits)))).

process_stats(Agent,str(Y)) :-
	add(str(Agent,Y)),
	must_det((damage(Agent,Dam),number(Dam))),
	NewDam is (Dam + ((Y * 5) - 10)),
	add(damage(Agent,NewDam)),
	add(stat_total(Agent,+Y)).

process_stats(Agent,height(Ht)) :-
	add(height(Agent,Ht)),
	add(stat_total(Agent,+Ht)).

process_stats(Agent,stm(Stm)) :-
	add(stm(Agent,Stm)),
	req(damage(Agent,Dam)),
	NewDam is (((Stm * 10) - 20) + Dam),
	add(damage(Agent,NewDam)),
	req(charge(Agent,NRG)),
	Charge is (((Stm * 10) - 20) + NRG),
	add(charge(Agent,Charge)),
	add(stat_total(Agent,+Stm)).

process_stats(Agent,spd(Spd)) :-
	add(spd(Agent,Spd)),
	add(stat_total(Agent,+Spd)).

process_stats(Agent,Stat) :- add(props(Agent,[Stat])).

check_stat_total(Agent) :-
	req(stat_total(Agent,Total)),
	Total > 12,!,
	nl,
	write('Agent '),
	write(Agent),
	write(' has more than 12 points in their triats.'),
	nl,
	write('Exiting....'),
	nl,
	abort.
check_stat_total(_).


