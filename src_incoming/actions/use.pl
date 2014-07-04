% use.pl
% Dec 13, 2035
% Douglas Miles
%
% This file defines the basic use (pick up) predicate
%

% :- module(user). 
:- module(use, []).

:- include(logicmoo(vworld/moo_header)).

:- moo:register_module_type(command).

moo:argsIsa(action_verb_useable(verb,term(mpred),type,term(mpred))).

moo:action_verb_useable(wear,wearing,wearable,stowed).
moo:action_verb_useable(hold,holding,wieldable,stowed).
moo:action_verb_useable(use,using,usable,stowed).
moo:action_verb_useable(drink,drinking,drinkable,holding).
moo:action_verb_useable(stow,stowed,stowable,holding).

moo:action_info(Syntax,String):-moo:action_verb_useable(Stow,Stowed,Stowable,Holding),Syntax=..[Stow,Stowable],
   sformat(String,'~w a ~w that you are/have ~w so it will be ~w.',[Stow,Stowable,Holding,Stowed]).

get_use_verbs(USE,USING,USABLE,STOWED):-moo:action_verb_useable(USE,USING,USABLE,STOWED).

% Use something
% Successfully picking something up
moo:agent_call_command(Agent,SENT) :-
  get_use_verbs(USE,_USING,USABLE,STOWED),
    SENT=..[USE,Obj],
	possess(Agent,Obj),
        prop(Agent,STOWED,Obj),
	mud_isa(Obj,USABLE),
	props(Obj, weight =< 1),
	worth(Agent,USE,Obj),
	do_permanence(USE,Agent,Obj),
	call_update_charge(Agent,USE).
%Nothing to use
moo:agent_call_command(Agent,SENT) :-
  get_use_verbs(USE,_USING,_USABLE,_STOWED),
    SENT=..[USE,_Obj],
	call_update_charge(Agent,USE),
	add(failure(Agent,USE)).

% Is the obect going to stick around after usen, either as is
% or in the agent's possession.
do_permanence(USE,Agent,Obj) :-
  get_use_verbs(USE,_USING,_USABLE,_STOWED),
	atloc(Obj,LOC),
	check_permanence(USE,Agent,LOC,Obj).

moo:check_permanence(USE,_Agent,LOC,Obj) :-
     get_use_verbs(USE,_USING,_USABLE,_STOWED),
	props(Obj,permanence(USE,Dissapears)),
	member(Dissapears,[dissapears,0]),
	del(atloc(Obj,LOC)).
moo:check_permanence(USE,Agent,LOC,Obj) :-
    get_use_verbs(USE,USING,_USABLE,_STOWED),
        props(Obj,permanence(USE,1)),
	del(atloc(Obj,LOC)),
	padd(Agent,USING,Obj).
moo:check_permanence(USE,_,_,_):-get_use_verbs(USE,_USING,_USABLE,_STOWED),!.

% Record keeping
moo:update_charge(Agent,USE) :-
    get_use_verbs(USE,_USING,_USABLE,_STOWED),
      padd(Agent,[charge(-2)]).






:- include(logicmoo(vworld/moo_footer)).

