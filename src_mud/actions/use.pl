% use.pl
% Dec 13, 2035
% Douglas Miles
%
% This file defines the basic use (pick up) predicate
%

% :-swi_module(user). 
:-swi_module(actUse, []).

:- include(logicmoo(vworld/moo_header)).

:- register_module_type(mtCommand).

argsIsaInList(action_verb_useable(vtVerb,ftTerm(tPred),tCol,ftTerm(tPred))).


mudSubclass(isEach('PortableObject','ProtectiveAttire',tStowable),tWieldable).
mudSubclass('FluidReservoir',tDrinkable).
mudSubclass(tWeapon,tWieldable).
mudSubclass(tContolDevice,tUsable).

action_verb_useable(actWear,wearsClothing,tWearable,mudStowed).
action_verb_useable(actWield,wielding,tWieldable,mudStowed).
action_verb_useable(actUse,using,tUsable,mudStowed).
action_verb_useable(actDrink,drinking,tDrinkable,holding).
action_verb_useable(actStow,mudStowed,tStowable,holding).

action_info(Syntax,String):-action_verb_useable(Stow,Stowed,Stowable,Holding),Syntax=..[Stow,Stowable],
   sformat(String,'~w a ~w that you are/have ~w so it will be ~w.',[Stow,Stowable,Holding,Stowed]).

get_use_verbs(USE,USING,USABLE,STOWED):-action_verb_useable(USE,USING,USABLE,STOWED).

% Use something
% Successfully picking something up
agent_call_command(Agent,SENT) :-
  get_use_verbs(USE,_USING,USABLE,STOWED),
    SENT=..[USE,Obj],
	mudPossess(Agent,Obj),
        prop(Agent,STOWED,Obj),
	mudIsa(Obj,USABLE),
	props(Obj, mudWeight =< 1),
	do_act_affect(Agent,USE,Obj),
	do_permanence(USE,Agent,Obj),
	call_update_charge(Agent,USE).
%Nothing to use
agent_call_command(Agent,SENT) :-
  get_use_verbs(USE,_USING,_USABLE,_STOWED),
    SENT=..[USE,_Obj],
	call_update_charge(Agent,USE),
	(add_cmdfailure(Agent,USE)).

% Is the obect going to stick around after usen, either as is
% or in the agent's possession.
do_permanence(USE,Agent,Obj) :-
  get_use_verbs(USE,_USING,_USABLE,_STOWED),
	mudAtLoc(Obj,LOC),
	check_permanence(USE,Agent,LOC,Obj).

check_permanence(USE,_Agent,LOC,Obj) :-
     get_use_verbs(USE,_USING,_USABLE,_STOWED),
	props(Obj,mudPermanence(USE,Dissapears)),
	member(Dissapears,[dissapears,0]),
	del(mudAtLoc(Obj,LOC)).
check_permanence(USE,Agent,LOC,Obj) :-
    get_use_verbs(USE,USING,_USABLE,_STOWED),
        props(Obj,mudPermanence(USE,Held)),
        member(Held,[1,held]),
	del(mudAtLoc(Obj,LOC)),
	padd(Agent,USING,Obj).
check_permanence(USE,_,_,_):-get_use_verbs(USE,_USING,_USABLE,_STOWED),!.

% Record keeping
update_charge(Agent,USE) :-
    get_use_verbs(USE,_USING,_USABLE,_STOWED),
      padd(Agent,[mudCharge(-2)]).






:- include(logicmoo(vworld/moo_footer)).
