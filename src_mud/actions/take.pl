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
:-swi_module(actTake, []).

:- include(logicmoo(vworld/moo_header)).

:- register_module_type(tCommand).

tActionType(actTake(tItem)).

:-decl_mpred(mudPossess,notAssertible).

reachable_object(Agent,Obj):-
  dif(Agent,Obj),
  localityOfObject(Agent,LOC),
  localityOfObject(Obj,LOC),
  nonvar(Obj),
  not(mudPossess(Agent,Obj)).

% Take something
% Successfully picking something up
agent_call_command(Agent,actTake(SObj)) :-
	once((reachable_object(Agent,Obj),match_object(SObj,Obj))),
	nop((ignore(props(Obj,mudWeight<2)),
	ignore(do_act_affect(Agent,actTake,Obj)))),
	permanence_take(actTake,Agent,Obj),
	call_update_charge(Agent,actTake).

%Nothing to pick up
agent_call_command(Agent,actTake(_)) :-
	call_update_charge(Agent,actTake),
	add_cmdfailure(Agent,actTake).

% Is the obect going to stick around after taken, either as is
% or in the agent's possession.
permanence_take(actTake,Agent,Obj) :-
	mudAtLoc(Agent,LOC),
	do_permanence_change(actTake,Agent,LOC,Obj),!.
        %term_listing(Obj).

remove_object_loc(Obj):- 
  (mudAtLoc(Obj,LOC)-> clr(mudAtLoc(Obj,LOC));true),
  (localityOfObject(Obj,R)-> clr(localityOfObject(Obj,R));true).

do_permanence_change(actTake,_,_,Obj):-
        props(Obj,mudPermanence(actTake,Dissapears)), 
		member(Dissapears,[0,dissapears]),
        remove_object_loc(Obj).
do_permanence_change(actTake,Agent,_,Obj) :-
	props(Obj,mudPermanence(actTake,Held)),
           member(Held,[1,held]),
        remove_object_loc(Obj),
	add(mudStowed(Agent,Obj)),
        must_posses(Agent,Obj).
do_permanence_change(actTake,Agent,_,Source) :-
	props(Source,mudPermanence(actTake,copy(What))),
        create_new_object([What],Obj),
        add(mudStowed(Agent,Obj)),
        must_posses(Agent,Obj).
do_permanence_change(actTake,_Agent,_,Obj) :-
        props(Obj,mudPermanence(actTake,stays)),!.
do_permanence_change(actTake,Agent,_,Obj) :-
        remove_object_loc(Obj),
	add(mudStowed(Agent,Obj)),
        must_posses(Agent,Obj).

must_posses(Agent,Obj):-
         (req(mudPossess(Agent,Obj)) -> true; trace_or_throw(req(mudPossess(Agent,Obj)))).


% Record keeping
update_charge(Agent,actTake) :-
      padd(Agent,mudCharge(-2)).






:- include(logicmoo(vworld/moo_footer)).
