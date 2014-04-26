% any.pl
% Dec 13, 2035
% Douglas Miles
%
% This file defines the basic ANY verb that has the four elements of 
% 
%  action_requires_states(Agent,StateRules,REQS),
%  action_removes_states(Agent,StateRules,REMS),
%  states_types(Agent,StateRules,TYPES),
%  action_adds_states(Agent,StateRules,ADDS),
% 

:- module(any, []).

action_adds_states(_Agent,List,Adds):-findall(A,member(_ -> A,List),Adds).
action_removes_states(_Agent,List,Dels):-findall(A,member(A -> _,List),Dels).
action_requires_states(_Agent,List,Preconds):-findall(A,(member(A,List),\+ functor(A,(->),_)),Preconds).


:- include(logicmoo('vworld/vworld_header.pl')).

:- register_module_type(command).
moo:action_rules(_,_,_,_):-fail.

moo:action_rules(Agent,use,[Obj],[possess(Agent,Obj),mud_isa(Obj,useable),stowed(Agent,Obj)->using(Agent,Obj)]).
moo:action_rules(Agent,stow,[Obj],[possess(Agent,Obj),mud_isa(Obj,stowable),genlPreds(Using,controling),k(Using,Agent,Obj)->stowed(Agent,Obj)]).

% Use something
moo:agent_call_command(Agent,ACT) :-
   ACT =.. [VERB|SENT],
   moo:action_rules(Agent,VERB,SENT,StateRules),
      action_requires_states(Agent,StateRules,REQS),
      action_removes_states(Agent,StateRules,REMS),
      action_adds_states(Agent,StateRules,ADDS),
     moo:update_charge(Agent,VERB),
     ((
         req(REQS)) ->
         ((clr(REMS),
         add(ADDS),
         moo:update_charge(Agent,VERB)));	
%Nothing to use
      add(failure(Agent,SENT))).

:- include(logicmoo('vworld/vworld_footer.pl')).


