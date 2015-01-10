/** <module> 
% This file gives a common place where world effects 
% (such as carrying  shield or being drunk) are implemented
%
% Project LogicMoo: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
% :-swi_module(world_effects,[]).
/*
% This file is "included" from world.pl 
*/


:-decl_mpred_hybrid(mudActAffect/3).

% Used by eat.pl and take.pl
% Is the object worth anything (either scored points or charge)
% Score any points?


do_act_affect(Agent,Action,Obj) :-
	props(Obj,mudActAffect(Action,mudScore(S))),
	add(mudScore(Agent,+S)),
	fail. % fail to check for charge too
% Charge up those batteries
do_act_affect(Agent,Action,Obj) :-
          props(Obj,mudActAffect(Action,mudCharge(NRG))),
	req(mudCharge(Agent,Chg)),
	req(mudStm(Agent,Stm)),
	predInstMax(mudCharge,Agent,Max),
	(Chg + NRG) < (((Stm * 10) -20) + Max),
	add(mudCharge(Agent,+NRG)),
	fail. % fail to check for healing
% Heal
do_act_affect(Agent,Action,Obj) :-
           props(Obj,mudActAffect(Action,heal(Hl))),
	req((mudHealth(Agent,Dam),
             mudStm(Agent,Stm),
             mudStr(Agent,Str))),
	req(predInstMax(mudHealth,Agent,Max)),
	(Dam + Hl) < ((((Stm * 10) -20) + ((Str * 5) - 10)) + Max),
	add(mudCharge(Agent,+Hl)),
	!.
do_act_affect(_,_,_).


% Check to see if last action was successful or not
:-swi_export(success/2).
success(Agent,YN) :- mudCmdfailure(Agent,_)-> YN=no ; YN=yes.

:-swi_export(add_cmdfailure/2).
add_cmdfailure(Agent,What):-add(mudCmdfailure(Agent,What)).

decl_database_hook(assert(_),mudCmdfailure(Agent,What)):- once(idel(cmdsuccess(Agent,What));clr(cmdsuccess(Agent,_))).

% Initialize world.
% This keeps the old databases messing with new runs.
           

:-dynamic(spawn_objects/1).

% Check to see if any of the objects should be placed in the world as it runs.

:-swi_export(call_update_charge/2).
call_update_charge(Agent,What):- padd(Agent,cmdsuccess(What)), doall(must(update_charge(Agent,What))),!.

:-swi_export(call_update_stats/2).
call_update_stats(Agent,What):- padd(Agent,cmdsuccess(What)), doall(must(update_stats(Agent,What))),!.

set_stats(Agent,[]) :- set_stats(Agent,[mudStr(2),mudHeight(2),mudStm(2),mudSpd(2)]).

set_stats(Agent,Traits) :-
        clr(stat_total(Agent,_)),
        add(stat_total(Agent,0)),	
	forall(member(Trait,Traits),
	       ignore(catch(process_stats(Agent,Trait),E,dmsg(E:process_stats(Agent,Trait))))),
               ignore(catch(check_stat_total(Agent),E2,dmsg(E2:check_stat_total(Agent)))).
set_stats(Agent,Traits):-dmsg(warn(failed(set_stats(Agent,Traits)))).

process_stats(Agent,mudStr(Y)) :-
	add(mudStr(Agent,Y)),
	must_det((mudHealth(Agent,Dam),number(Dam))),
	NewDam is (Dam + ((Y * 5) - 10)),
	add(mudHealth(Agent,NewDam)),
	add(stat_total(Agent,+Y)).

process_stats(Agent,mudHeight(Ht)) :-
	add(mudHeight(Agent,Ht)),
	add(stat_total(Agent,+Ht)).

process_stats(Agent,mudStm(Stm)) :-
	add(mudStm(Agent,Stm)),
	req(mudHealth(Agent,Dam)),
	NewDam is (((Stm * 10) - 20) + Dam),
	add(mudHealth(Agent,NewDam)),
	req(mudCharge(Agent,NRG)),
	Charge is (((Stm * 10) - 20) + NRG),
	add(mudCharge(Agent,Charge)),
	add(stat_total(Agent,+Stm)).

process_stats(Agent,mudSpd(Spd)) :-
	add(mudSpd(Agent,Spd)),
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
