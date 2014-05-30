% use.pl
% Dec 13, 2035
% Douglas Miles
%
% This file defines the basic use (pick up) predicate
%

:- module(use, []).

:- include(logicmoo(vworld/moo_header)).

:- moodb:register_module_type(command).



moo:use_usable(wear,wearing,wearable,stowed).
moo:use_usable(hold,holding,wieldable,stowed).
moo:use_usable(use,using,usable,stowed).
moo:use_usable(drink,drinking,drinkable,holding).
moo:use_usable(stow,stowed,stowable,holding).

moo:action_help(Syntax,String):-moo:use_usable(Stow,Stowed,Stowable,Holding),Syntax=..[Stow,Stowable],
   sformat(String,'~w a ~w that you are/have ~w so it will be ~w.',[Stow,Stowable,Holding,Stowed]).

use_verbs(USE,USING,USABLE,STOWED):-moo:use_usable(USE,USING,USABLE,STOWED).

% Use something
% Successfully picking something up
moo:agent_call_command(Agent,SENT) :-
  use_verbs(USE,_USING,USABLE,STOWED),
    SENT=..[USE,Obj],
	possess(Agent,Obj),
        prop(Agent,STOWED,Obj),
	mud_isa(Obj,USABLE),
	props(Obj, weight =< 1),
	worth(Agent,USE,Obj),
	do_permanence(USE,Agent,Obj),
	moo:update_charge(Agent,USE).
%Nothing to use
moo:agent_call_command(Agent,SENT) :-
  use_verbs(USE,_USING,_USABLE,_STOWED),
    SENT=..[USE,_Obj],
	moo:update_charge(Agent,USE),
	add(failure(Agent,USE)).

% Is the obect going to stick around after usen, either as is
% or in the agent's possession.
do_permanence(USE,Agent,Obj) :-
  use_verbs(USE,_USING,_USABLE,_STOWED),
	atloc(Obj,LOC),
	check_permanence(USE,Agent,LOC,Obj).

check_permanence(USE,_Agent,LOC,Obj) :-
     use_verbs(USE,_USING,_USABLE,_STOWED),
	props(Obj,permanence(USE,dissapears)),
	del(atloc(Obj,LOC)).
check_permanence(USE,Agent,LOC,Obj) :-
    use_verbs(USE,USING,_USABLE,_STOWED),
        props(Obj,permanence(USE,1)),
	del(atloc(Obj,LOC)),
	padd(Agent,USING,Obj).
check_permanence(_,_,_,_).

% Record keeping
moo:update_charge(Agent,USE) :-
    use_verbs(USE,_USING,_USABLE,_STOWED),
      padd(Agent,[charge(-2)]).






:- include(logicmoo(vworld/moo_footer)).

