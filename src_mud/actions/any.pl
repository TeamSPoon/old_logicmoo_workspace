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

% :-swi_module(user). 
:-swi_module(any, []).

action_adds_states(_Agent,List,Adds):-findall(A,member(_ -> A,List),Adds).
action_removes_states(_Agent,List,Dels):-findall(A,member(A -> _,List),Dels).
action_requires_states(_Agent,List,Preconds):-findall(A,(member(A,List),\+ functor(A,(->),_)),Preconds).


:- include(logicmoo(vworld/moo_header)).

:- register_module_type(tCommand).
action_rules(_,_,_,_):-fail.

action_rules(Agent,actUse,[Obj],[mudPossess(Agent,Obj),mudIsa(Obj,tUseable),mudStowed(Agent,Obj)->using(Agent,Obj)]).
action_rules(Agent,actStow,[Obj],[mudPossess(Agent,Obj),mudIsa(Obj,tStowable),genlPreds(Using,'controls'),[Using,Agent,Obj]]->[mudStowed,Agent,Obj]).

% Use something
agent_call_command(Agent,ACT) :-
   safe_univ(ACT,[VERB|SENT]),
   action_rules(Agent,VERB,SENT,StateRules),
      action_requires_states(Agent,StateRules,REQS),
      action_removes_states(Agent,StateRules,REMS),
      action_adds_states(Agent,StateRules,ADDS),
     call_update_charge(Agent,VERB),
     ((
         req(REQS)) ->
         ((clr(REMS),
         add(ADDS),
         call_update_charge(Agent,VERB)));	
%Nothing to use
      (add_cmdfailure(Agent,SENT))).

:- include(logicmoo(vworld/moo_footer)).
