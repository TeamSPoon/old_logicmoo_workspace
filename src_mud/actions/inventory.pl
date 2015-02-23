% :-swi_module(user). 
:-swi_module(modInventory, [mudInventoryLocation/3,show_inventory/2]).
/** <module> A command to  ...
% Douglas Miles 2014
% inventory(Agt,Inv) = inventory (anything the agent has taken)
*/
:- include(logicmoo(vworld/moo_header)).

% :- register_module_type (mtCommand).


% ====================================================
% the entire inventory system
% ====================================================
:- decl_type(tNearestReachableItem).
tNearestReachableItem(Obj):-
  current_agent(Agent),
  nearest_reachable_object(Agent,Obj).

:- decl_type(tFarthestReachableItem).
tFarthestReachableItem(Obj):-
  current_agent(Agent),
  farthest_reachable_object(Agent,Obj).

nearest_reachable_object(Agent,Obj):- 
  with_no_modifications((findall(Obj,farthest_reachable_object(Agent,Obj),List),reverse(List,Reverse),!,member(Obj,Reverse))).

farthest_reachable_object(Agent,Obj):-with_no_modifications((farthest_reachable_object0(Agent,Obj))).
farthest_reachable_object0(Agent,Obj):-
  test_exists(Obj),
  dif(Agent,Obj),
  localityOfObject(Agent,LOC),
  localityOfObject(Obj,LOC).
farthest_reachable_object0(Agent,Obj):-
  test_exists(Obj),
  dif(Agent,Obj),
  mudAtLoc(Agent,LOC),
  mudAtLoc(Obj,LOC).
farthest_reachable_object0(Agent,Obj):-
  test_exists(Obj),
  dif(Agent,Obj),
  localityOfObject(Obj,Agent).
farthest_reachable_object0(Agent,Obj):-
  test_exists(Obj),
  mudPossess(Agent,Obj).
  

detatch_object(Obj):-  
  (mudPossess(Agent,Obj)->clr(mudPossess(Agent,Obj));true),
  (with_no_modifications(mudAtLoc(Obj,LOC))-> clr(mudAtLoc(Obj,LOC));true),
  (with_no_modifications(localityOfObject(Obj,R))-> clr(localityOfObject(Obj,R));true).

user:action_info(actInventory(isOptional(tAgentGeneric,isSelfAgent)), "Examine an inventory").

user:agent_call_command(Agent,actInventory(Who)):- show_inventory(Agent,Who).

show_inventory(Agent,Who):-
        show_kb_preds(Agent,[                                                  
                        % listof(mudInventoryLocation(Who, value, _)),
                       % listof(mudContains(Who,value)),                 
                       % listof(mudPossess(Who,value)),
                        listof(mudStowing(Who,value)),                       
                        listof(mudWielding(Who,value)),
                        listof(wearsClothing(Who,value))]).


mudInventoryLocation(Who,Obj,Loc):- 
         findall(prop(Obj,PRED),
                  (member(dbase_t(PRED,A,B), [
                        dbase_t(mudPossess,Who,Obj),
                        dbase_t(mudStowing,Who,Obj),
                        dbase_t(mudContains,Who,Obj),
                        dbase_t(mudWielding,Who,Obj),
                        dbase_t(wearsClothing,Who,Obj)]),
                     ireq(dbase_t(PRED,A,B))),
                  RESULTS),
         setof(Obj,member(prop(Obj,PRED),RESULTS),OBJLIST),!,
         member(Obj,OBJLIST),once((member(PRED2,[mudAtLoc,localityOfObject]),ireq(dbase_t(PRED2,Obj,Loc)))).

test_exists(O):- tItem(O).
test_exists(O):- tAgentGeneric(O).
test_exists(O):- tRegion(O).
test_anyInst(O):- tCol(O).
test_anyInst(O):- test_exists(O).

% helps for testings
% :- listing(inventory:_).

% :- include(logicmoo(vworld/moo_footer)).
