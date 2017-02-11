% use.pl
% Dec 13, 2035
% Douglas Miles
%
% This file defines the basic use (pick up) predicate
%

% :-swi_module(user). 
:-swi_module(moduleUse, [do_act_use/3]).

:- include(prologmud(mud_header)).

% :- register_module_type (mtCommand).

meta_argtypes(action_verb_useable(vtVerb,tCol,ftTerm(tPred),ftTerm(tPred),ftTerm(tPred))).


==>genls(isEach('PortableObject','ProtectiveAttire',tStowAble),tWieldAble).
genls('FluidReservoir',tDrinkAble).
genls(tWeapon,tWieldAble).
genls(tContolDevice,tUseAble).
genls(tWieldAble,tUseAble).
genls(tCarryAble,tStowAble).


prologHybrid(mudWielding/2).
prologHybrid(mudStowing/2).
prologHybrid(mudPossess/2).

:-ain((mudStowing(A,O) ==> mudPossess(A,O))).
mudWielding(A,O) ==> mudPossess(A,O).
wearsClothing(A,O) ==> mudPossess(A,O).


prologMultiValued(wearsClothing(tAgent,tWearAble)).
prologMultiValued(mudWielding(tAgent,tWieldAble)).
prologMultiValued(mudStowing(tAgent,tStowAble)).
prologMultiValued(mudContains(tContainer,tObj)).


prologHybrid(genlPreds/2).
genlPreds(wearsClothing,mudPossess).
genlPreds(mudWielding,mudPossess).
genlPreds(mudStowing,mudPossess).


prologHybrid(mudKnowing(tAgent,ftTerm)).
genlPreds(mudKnowing,mudPossess).
% genlPreds(mudPossess,mudContains).
genlInverse(mudContains,mudInsideOf).
%genlInverse(mudStowing,mudInsideOf).
%genlInverse(mudInsideOf,mudPossess).


prologHybrid(action_verb_useable/5).

% action_verb_useable(Actionn,RequiredArg,AddedProp,PrecondProp,RemovedProp).
action_verb_useable(actWear,tWearAble,wearsClothing,mudPossess,mudStowing).
action_verb_useable(actWield,tWieldAble,mudWielding,mudPossess,mudStowing).
action_verb_useable(actStow,tStowAble,mudStowing,mudPossess,mudWielding).
% action_verb_useable(actUse,mudUsing,tUseAble,mudPossess,mudPossess).

:- baseKB:import(logicmoo_util_strings:convert_to_cycString/2).


==>(action_info(Syntax,String):-
 no_repeats([Syntax],(
  call_u(action_verb_useable(ActUse,Wieldable,NowWielding,Possessing,Unstowed)),
  Syntax=..[ActUse,isAnd([tNearestReachableItem,call(Possessing,isSelfAgent,isThis),Wieldable])])),
   must_maplist(name_text_now,[ActUse,Wieldable,Possessing,NowWielding,Unstowed],List),
   sformat(String,'~w a ~w that you ~w so it will be ~w and not be ~w afterwards.',List)).

agent_call_command(Agent,Syntax) :- 
    call_u((action_verb_useable(ActUse,_Wieldable,_NowWielding,_Possessing,_Unstowed),Syntax=..[ActUse,Obj])),
    agent_call_command_use(Agent,ActUse,Obj),!.

 
% Successfully use something
agent_call_command_use(Agent,ActUse,Obj) :- 
  must_det_l([
	once((nearest_reachable_object(Agent,Obj))),
	nop((ignore(props(Obj,mudWeight<2)),
	ignore(do_act_affect(Agent,ActUse,Obj)))),
	do_act_use(ActUse,Agent,Obj),
	call_update_charge(Agent,ActUse)]).

% Unsuccessfully use something
agent_call_command_use(Agent,ActUse,_Obj) :- 
	call_update_charge(Agent,ActUse),
	add_cmdfailure(Agent,ActUse).

get_use_perminance(Obj,ActUse,TakeableType):-
 ignore(props(Obj,mudPermanence(ActUse,TakeableType))), 
 ignore(TakeableType=vTakenMoves).

do_act_use(ActUse,Agent,Obj) :-
   must_det_l([  
        get_use_perminance(Obj,ActUse,TakeableType),
	do_change_use(ActUse,Agent,Obj,TakeableType)]),!.


get_add_remove_use(ActUse,Agent,NowWielding,Obj,Unstowed):-     
 must_det_l([
     action_verb_useable(ActUse,Wieldable,NowWielding,Possessing,Unstowed),
      show_failure(isa(Obj,Wieldable)),
  %   show_failure(ireq(t(Unstowed,Agent,Obj))),
  %   show_failure(not(ireq(t(NowWielding,Agent,Obj)))),
     show_failure(ireq(t(Possessing,Agent,Obj)))]).

% Is the obect going to stick around after use-ing, either as is or in the agent's possession.
do_change_use(ActUse,Agent,Obj,vTakenDeletes):-
        get_add_remove_use(ActUse,Agent,NowWielding,Obj,Unstowed),
        detatch_object(Obj),
        clr(t(Unstowed,Agent,Obj)),
        ain(t(NowWielding,Agent,Obj)),    
        must_post_use(ActUse,Agent,Obj),
        detatch_object(Obj).
do_change_use(ActUse,Agent,_Source,vTakenCopyFn(What)) :-
        get_add_remove_use(ActUse,Agent,NowWielding,Obj,Unstowed),
        create_new_object([What],Obj),
        detatch_object(Obj),
        clr(t(Unstowed,Agent,Obj)),
        ain(t(NowWielding,Agent,Obj)),        
        must_post_use(ActUse,Agent,Obj).
do_change_use(ActUse,Agent,Obj,vTakenStays) :-        
        get_add_remove_use(ActUse,Agent,NowWielding,Obj,Unstowed),
        mudAtLoc(Obj,Was),
        detatch_object(Obj),
        clr(t(Unstowed,Agent,Obj)),
        ain(t(NowWielding,Agent,Obj)),
        must_post_use(ActUse,Agent,Obj),
        detatch_object(Obj),
        ain(mudAtLoc(Obj,Was)).
% default is same as vTakenMoves
do_change_use(ActUse,Agent,Obj,vTakenMoves) :-
 must_det_l((
        get_add_remove_use(ActUse,Agent,NowWielding,Obj,Unstowed),        
        detatch_object(Obj),
        clr(t(Unstowed,Agent,Obj)),
	ain(t(NowWielding,Agent,Obj)),
        must_post_use(ActUse,Agent,Obj))).

must_post_use(ActUse,Agent,Obj):-
      must_det_l((
       get_add_remove_use(ActUse,Agent,NowWielding,Obj,Unstowed),
       fmt([Agent,ActUse,Obj]),       
       REQ = t(NowWielding,Agent,Obj),
       CLR = t(Unstowed,Agent,Obj),
       (ireq(REQ) -> true; trace_or_throw(ireq(REQ))),
       (ireq(CLR) -> trace_or_throw(not(ireq(REQ))); true))),!.

% Record keeping
update_charge(Agent,_ActWield) :- 
        padd(Agent,mudEnergy(+ -2)).

:- include(prologmud(mud_footer)).



:- include(prologmud(mud_footer)).
