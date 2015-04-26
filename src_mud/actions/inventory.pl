% :-swi_module(user). 
:-swi_module(modInventory, [mudInventoryLocation/3,show_inventory/2]).
/** <module> A command to  ...
% Douglas Miles 2014
% inventory(Agt,Inv) = inventory (anything the agent has taken)
*/
:- include(prologmud(mud_header)).

% :- register_module_type (mtCommand).

:-decl_mpred_prolog(nearest_reachable_object(tAgentGeneric,tObj)).
:-decl_mpred_prolog(farthest_reachable_object(tAgentGeneric,tObj)).

% ====================================================
% the entire inventory system
% ====================================================
tCol(tNearestReachableItem).
tNearestReachableItem(Obj):-
  current_agent(Agent),
  nearest_reachable_object(Agent,Obj).

tCol(tFarthestReachableItem).
tFarthestReachableItem(Obj):-
  current_agent(Agent),
  farthest_reachable_object(Agent,Obj).



nearest_reachable_object(Agent,Obj):- 
  with_no_modifications((findall(Obj,farthest_reachable_object(Agent,Obj),List),reverse(List,Reverse),!,member(Obj,Reverse))).

:-decl_mpred_prolog(farthest_reachable_object(tAgentGeneric,tObj)).
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
  (is_asserted(mudPossess(Agent,Obj))->clr(mudPossess(Agent,Obj));true),
  (is_asserted(mudAtLoc(Obj,LOC))-> clr(mudAtLoc(Obj,LOC));true),
  (is_asserted(localityOfObject(Obj,R))-> clr(localityOfObject(Obj,R));true),
  (clr(inRegion(Obj,_))),!.
   

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
                  (member(t(PRED,A,B), [
                        t(mudPossess,Who,Obj),
                        t(mudStowing,Who,Obj),
                        t(mudContains,Who,Obj),
                        t(mudWielding,Who,Obj),
                        t(wearsClothing,Who,Obj)]),
                     ireq(t(PRED,A,B))),
                  RESULTS),
         setof(Obj,member(prop(Obj,PRED),RESULTS),OBJLIST),!,
         member(Obj,OBJLIST),once((member(PRED2,[mudAtLoc,localityOfObject]),ireq(t(PRED2,Obj,Loc)))).

test_exists(O):- tItem(O).
test_exists(O):- tAgentGeneric(O).
test_exists(O):- tRegion(O).
test_anyInst(O):- tCol(O).
test_anyInst(O):- test_exists(O).

% helps for testings
% :- listing(inventory:_).

:- include(prologmud(mud_footer)).
