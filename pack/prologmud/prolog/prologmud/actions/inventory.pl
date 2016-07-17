% :-swi_module(user). 
:-swi_module(modInventory, [mudInventoryLocation/3,show_inventory/2]).
/** <module> A command to  ...
% Douglas Miles 2014
% inventory(Agt,Inv) = inventory (anything the agent has taken)
*/
:- include(prologmud(mud_header)).

% :- register_module_type (mtCommand).

:-decl_mpred_prolog(nearest_reachable_object(tAgent,tObj)).
:-decl_mpred_prolog(farthest_reachable_object(tAgent,tObj)).

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

:-decl_mpred_prolog(farthest_reachable_object(tAgent,tObj)).
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

% detatch from world
detatch_object(Obj):-  
  (req1(mudPossess(Agent,Obj))->clr(mudPossess(Agent,Obj));true),
  (req1(mudAtLoc(Obj,LOC))-> clr(mudAtLoc(Obj,LOC));true),
  (req1(localityOfObject(Obj,R))-> clr(localityOfObject(Obj,R));true),
  (clr(inRegion(Obj,_))),!.
   
% destroy from ontology
destroy_instance(Obj):- % forall(isa(Obj,Col),mpred_remove(isa(Obj,Col))),
                        xlisting_inner(destroy_clause,Obj,[]),!.

destroy_clause(H,B,R):- nonvar(R),catch(clause_property(R,_),_,fail),erase(R),wdmsg(destroy_clause(H,B,R)),!,mpred_undo((H:-B)).
destroy_clause(H,B,R):- nop(wdmsg(misssed_destroy_clause(H,B,R))),!,mpred_undo((H:-B)).
   

action_info(actInventory(isOptional(tAgent,isSelfAgent)), "Examine an inventory").

agent_call_command(Agent,actInventory(Who)):- show_inventory(Agent,Who).
agent_call_command(Agent,actInventory):- show_inventory(Agent,Agent).

show_inventory(Agent,Who):-
        show_kb_preds(Agent,[                                                  
                        % listof(mudInventoryLocation(Who, value, _)),
                        listof(mudContains(Who,value)),                 
                        listof(mudPossess(Who,value)),
                        listof(mudStowing(Who,value)),                       
                        listof(mudWielding(Who,value)),
                        listof(wearsClothing(Who,value))]).


mudInventoryLocation(Who,Obj,Loc):- 
         findall(props(Obj,PRED),
                  (member(t(PRED,A,B), [
                        t(mudPossess,Who,Obj),
                        t(mudStowing,Who,Obj),
                        t(mudContains,Who,Obj),
                        t(mudWielding,Who,Obj),
                        t(wearsClothing,Who,Obj)]),
                     ireq(t(PRED,A,B))),
                  RESULTS),
         setof(Obj,member(props(Obj,PRED),RESULTS),OBJLIST),!,
         member(Obj,OBJLIST),once((member(PRED2,[mudAtLoc,localityOfObject]),ireq(t(PRED2,Obj,Loc)))).

test_exists(O):- tItem(O).
test_exists(O):- tAgent(O).
test_exists(O):- tRegion(O).
test_anyInst(O):- tCol(O).
test_anyInst(O):- test_exists(O).

% helps for testings
% :- listing(inventory:_).

:- include(prologmud(mud_footer)).
