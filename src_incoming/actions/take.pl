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
% :-swi_module(user). 
:-swi_module(take, []).

:- include(logicmoo(vworld/moo_header)).

:- register_module_type(command).

actiontype(take(item)).

% Take something
% Successfully picking something up
agent_call_command(Agent,take(SObj)) :-
	(once((localityOfObject(Agent,LOC),
                       localityOfObject(Obj,LOC),nonvar(Obj),
                       not(possess(Agent,Obj)),

                       match_object(SObj,Obj)))),
	nop((ignore(props(Obj,weight<2)),
	ignore(do_act_affect(Agent,take,Obj)))),
	permanence_take(take,Agent,Obj),
	call_update_charge(Agent,take).

%Nothing to pick up
agent_call_command(Agent,take(_)) :-
	call_update_charge(Agent,take),
	(add_cmdfailure(Agent,take)).

% Is the obect going to stick around after taken, either as is
% or in the agent's possession.
permanence_take(take,Agent,Obj) :-
	atloc(Agent,LOC),
	check_permanence(take,Agent,LOC,Obj),!.
        %term_listing(Obj).

check_permanence(take,_,_,Obj):-
        props(Obj,permanence(take,Dissapears)), 
		member(Dissapears,[0,dissapears]),
        atloc(Obj,LOC),
	clr(atloc(Obj,LOC)).
check_permanence(take,Agent,_,Obj) :-
	props(Obj,permanence(take,Held)),
           member(Held,[1,held]),
        atloc(Obj,LOC),
	ignore(clr(atloc(Obj,LOC))),
	add(possess(Agent,Obj)),
        (req(possess(Agent,Obj)) -> true; throw(req(possess(Agent,Obj)))).
check_permanence(take,Agent,_,Source) :-
	props(Source,permanence(take,copy(What))),
        create_new_object([What],Obj),
         add(possess(Agent,Obj)),
        (req(possess(Agent,Obj)) -> true; throw(req(possess(Agent,Obj)))).
check_permanence(take,_Agent,_,Obj) :-
        props(Obj,permanence(take,stays)),!.

check_permanence(take,_,_,_).

% Record keeping
update_charge(Agent,take) :-
      padd(Agent,charge(-2)).






:- include(logicmoo(vworld/moo_footer)).

