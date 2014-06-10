/** <module> 
% This file gives a common place where world effects 
% (such as carrying  shield or being drunk) are implemented
%
% Project LogicMoo: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/

/*
% This file is "included" from world.pl 
*/




% Used by eat.pl and take.pl
% Is the object worth anything (either scored points or charge)
% Score any points?
worth(Agent,Action,Obj) :-
	props(Obj,act_affect(Action,score(S))),
	del(score(Agent,Y)),
	X is Y + S,
	add(score(Agent,X)),
	fail. % fail to check for charge too
% Charge up those batteries
worth(Agent,Action,Obj) :-
           props(Obj,act_affect(Action,charge(NRG))),
	req(charge(Agent,Chg)),
	req(stm(Agent,Stm)),
	max_charge(Max),
	(Chg + NRG) < (((Stm * 10) -20) + Max),
	del(charge(Agent,Y)),
	X is Y + NRG,
	add(charge(Agent,X)),
	fail. % fail to check for healing
% Heal
worth(Agent,Action,Obj) :-
           props(Obj,act_affect(Action,heal(Hl))),
	req((damage(Agent,Dam),
             stm(Agent,Stm),
             str(Agent,Str))),
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


