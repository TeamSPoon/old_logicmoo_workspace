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



:- include(prologmud(mud_header)).

% :- register_module_type (mtCommand).
user:action_rules(_,_,_,_):-fail.

end_of_file.

action_adds_states(_Agent,List,Adds):-findall(A,member(_ -> A,List),Adds).
action_removes_states(_Agent,List,Dels):-findall(A,member(A -> _,List),Dels).
action_requires_states(_Agent,List,Preconds):-findall(A,(member(A,List),\+ functor(A,(->),_)),Preconds).


user:action_rules(Agent,actUse,[Obj],[mudPossess(Agent,Obj),isa(Obj,tUseAble),mudStowing(Agent,Obj)->using(Agent,Obj)]).
user:action_rules(Agent,actStow,[Obj],[mudPossess(Agent,Obj),isa(Obj,tStowAble),genlPreds(Using,'mudControls'),[Using,Agent,Obj]]->[mudStowing,Agent,Obj]).

% Use something
user:agent_call_command(Agent,ACT) :-
   call((user:action_rules(Agent,VERB,SENT,StateRules),safe_univ(ACT,[VERB|SENT]))),
   
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

% :- include(prologmud(mud_footer)).
