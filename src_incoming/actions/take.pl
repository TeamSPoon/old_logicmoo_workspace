% take.pl
% May 18, 1996
% John Eikenberry
%
% Dec 13, 2035
% Douglas Miles
%
/** <module>
% This file defines the basic take (pick up) predicate
%
*/
% :- module(user). 
:- module(take, []).

:- include(logicmoo(vworld/moo_header)).

:- moo:register_module_type(command).

moo:actiontype(take(item)).

% Take something
% Successfully picking something up
moo:agent_call_command(Agent,take(SObj)) :- 
	hotrace(once((inRegion(Agent,LOC),
              inRegion(Obj,LOC),
              not(possess(Agent,Obj)),
              object_match(SObj,Obj)))),
	nop((ignore(props(Obj,weight<2)),
	ignore(worth(Agent,take,Obj)))),
	permanence_take(take,Agent,Obj),
	call_update_charge(Agent,take).

%Nothing to pick up
moo:agent_call_command(Agent,take(_)) :-
	call_update_charge(Agent,take),
	add(failure(Agent,take)).

% Is the obect going to stick around after taken, either as is
% or in the agent's possession.
permanence_take(take,Agent,Obj) :-
	atloc(Agent,LOC),
	check_permanence(take,Agent,LOC,Obj),!.
        %term_listing(Obj).

moo:check_permanence(take,_,_,Obj):-
        props(Obj,permanence(take,Dissapears)), 
		member(Dissapears,[0,dissapears]),
        atloc(Obj,LOC),
	clr(atloc(Obj,LOC)).
moo:check_permanence(take,Agent,_,Obj) :-
	props(Obj,permanence(take,1)),
        atloc(Obj,LOC),
	ignore(clr(atloc(Obj,LOC))),
	add(possess(Agent,Obj)),
        (req(possess(Agent,Obj)) -> true; throw(req(possess(Agent,Obj)))).
moo:check_permanence(take,Agent,_,Obj) :-
        atloc(Obj,LOC),
	ignore(clr(atloc(Obj,LOC))),
	add(possess(Agent,Obj)),
        (req(possess(Agent,Obj)) -> true; throw(req(possess(Agent,Obj)))).

moo:check_permanence(take,_,_,_).

% Record keeping
moo:update_charge(Agent,take) :-
      padd(Agent,charge(-2)).






:- include(logicmoo(vworld/moo_footer)).

