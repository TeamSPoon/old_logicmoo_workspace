% look.pl
% June 18, 1996
% John Eikenberry
%
% Dec 13, 2035
% Douglas Miles
%
/** <module>
% This file defines the basic look action
% Agents will use the predicate:
% mudGetPrecepts(Agent,Percepts) = list of lists of objects in agents location plus 2 locations in each direction
% mudNearReach(Agent,Percepts) = list of lists of objects in agents atloc plus 1 atloc in each dir
% mudNearFeet(Agent,Percepts) = list of objects in agents location
%

%
% props(Obj,height(ObjHt))  == svo(Obj,height,ObjHt) == p(height,Obj,ObjHt) == height(Obj,ObjHt)
% padd(Obj,height(ObjHt))  == add(p(height,Obj,ObjHt)) == add(p(height,Obj,ObjHt)) == add(height(Obj,ObjHt))
*/

% :-swi_module(user). 
:-swi_module(actLook, [ mudGetPrecepts/2,  mudNearReach/2, mudNearFeet/2, height_on_obj/2, can_sense/5 , call_look/2]).

:- include(logicmoo(vworld/moo_header)).

:- register_module_type(mtCommand).

:- dynamic blocks/1.



% can_sense(Agent,Sense,InList,CanDetect,CantDetect).
can_sense(_Agent,visual,InList,InList,[]).

action_info(actExamine(tItem), "view details of item (see also @list)").
agent_call_command(_Gent,actExamine(SObj)):- term_listing(SObj).

visibleTo(Agent,Agent).
visibleTo(Agent,Obj):-mudPossess(Agent,Obj).
visibleTo(Agent,Obj):-same_regions(Agent,Obj).

:- decl_type(txtPrepOf).
:- decl_type(txtPrepSpatial).
hook_coerce(Prep,txtPrepSpatial,Str):-member(Prep,[in,on,north_of,inside,onto,ontop]),name_text(Prep,Str).
hook_coerce(Prep,txtPrepSpatial,Inst):-hook_coerce(Prep,txtPrepOf,Inst).
hook_coerce([SDir,of],txtPrepOf,vDirFn(Dir)):-hook_coerce(SDir,vtDirection,Dir).

action_info(actLook, "generalized look in region").
action_info(actLook(isOptionalStr("in"),isOptionalStr("here")), "generalized look in region").
action_info(actLook(txtPrepOf,isOptionalStr("self")), "Look in a direction (TODO: look north of isSelfAgent)").
action_info(actLook(isOptional(txtPrepSpatial,"at"),tObj),"look [in|at|on|under|at] somewhere").
%action_info(look(obj), "Look at a speficific item").
%action_info(look_at(isOptional(call(visibleTo(isSelfAgent,value)),call(visibleTo(isSelfAgent,value)))), "Look at a speficific item").

agent_call_command(Agent,actLook):- look_as(Agent),!.
agent_call_command(Agent,actLook("here")):- look_as(Agent),!.
agent_call_command(Agent,actLook(_,"here")):- look_as(Agent),!.
agent_call_command(Agent,actLook(DirS,"self")):- coerce(DirS,vtDirection,Dir),!,
   view_dirs(Agent,[[Dir,vHere],[Dir,Dir],[Dir,Dir,vAdjacent]],Percepts),
   forall_member(P,Percepts,call_agent_action(Agent,actExamine(P))).
agent_call_command(Agent,actLook(_Dir,SObj)):-
   objects_match_for_agent(Agent,SObj,tObj,Percepts),
   forall_member(P,Percepts,call_agent_action(Agent,actExamine(P))).

:-swi_export(look_as/1).
look_as(Agent):-
   get_session_id(O),
   with_assertions(thlocal:session_agent(O,Agent),
        ((mudAtLoc(Agent,LOC),call_look(Agent,LOC)))).


:-swi_export(call_look/2).
call_look(Agent,LOC):-  mmake, call(call_look_proc,Agent,LOC).

:-decl_mpred_prolog(call_look_proc/3).
call_look_proc(Agent,LOC):- 
   with_no_modifications(with_assertions(mpred_prop(nameStrings,prologListValued),call_look_proc_0(Agent,LOC))).
call_look_proc_0(Agent,LOC):-
   clr(props(Agent,mudNeedsLook(true))),
   add(props(Agent,mudNeedsLook(false))),    
    toploop_output:show_kb_preds(Agent,LOC,
         [
         location= (value=LOC),
      % TODO make this work
         %  why does this this work on Prolog REPL?
         %   with_output_to(string(Str),show_room_grid('Area1000'))
         %  but yet this doent?
       %   show_room_grid = once(with_output_to(string(value),show_room_grid(region))),
         % for now workarround is 
         call(show_room_grid(isSelfRegion)),
         mudAtLoc(Agent,value),
         nameStringsList(isSelfRegion,value),
         forEach(mudDescription(isSelfRegion,Value),fmt(mudDescription(Value))),
         events=deliverable_location_events(Agent,LOC,value),
         path(D) = pathBetween(isSelfRegion,D,value),
         pathName(D) = pathName(isSelfRegion,D,value),
         % value = localityOfObject(value,isSelfRegion),
         localityOfObject(value,isSelfRegion),
         mudFacing(Agent,value),
         mudNearFeet(Agent,value),
         mudNearReach(Agent,value),
         mudGetPrecepts(Agent,value),         
         mudMoveDist(Agent,value),
         height_on_obj(Agent,value),
         success=success(Agent,value)
       ]),
    show_inventory(Agent,Agent).

nameStringsList(Region,ValueList):-findall(Value,nameStrings(Region,Value),ValueList).

looking(Agent):- current_agent(Agent),!.
looking(Agent):- tAgentGeneric(Agent),not(tDeleted(Agent)).

% ********** TOP LEVEL PREDICATE: this is the predicate agents use to look
% Look, reports everything not blocked up to two locations away
% plus the agents score, damage, charge, and if they succeeded at their last action.
% To make this action take a turn, change the first line to:
% Impliment(get_all(Agent,Vit,Dam,Suc,Scr,Percepts,Inv)) :-
get_all(Agent,Vit,Dam,Suc,Scr,Percepts,Inv) :-
  call((
	looking(Agent),
	mudCharge(Agent,Vit),
        mudHealth(Agent,Dam),
	success(Agent,Suc),
	mudScore(Agent,Scr),
	mudPossess(Agent,Inv),
	mudGetPrecepts(Agent,Percepts))),!.


% Get only the Percepts

% :-decl_mpred(mudGetPrecepts(agent,list(tSpatialThing)),[predModule(user)]).
mudGetPrecepts(Agent,Percepts) :- mudGetPrecepts0(Agent,Percepts0),!,flatten_set(Percepts0,Percepts).
mudGetPrecepts0(Agent,Percepts) :-
  call((
	looking(Agent),
	view_vectors(Dirs),
	check_for_blocks(Agent),
	view_dirs(Agent,Dirs,Tmp_percepts),
	alter_view(Agent,Dirs,Tmp_percepts,Percepts))),
	!.

% Look at locations immediately around argent
% :-decl_mpred(mudNearReach(agent,list(tSpatialThing)),[predModule(user)]).
mudNearReach(Agent,PerceptsO):- get_near0(Agent,Percepts0),!,flatten_set(Percepts0,Percepts),delete(Percepts,Agent,PerceptsO).
   
get_near0(Agent,Percepts) :-
  call((
	looking(Agent),
	near_vectors(Dirs),
	view_dirs(Agent,Dirs,Percepts))),!.

% Look only at location agent is currently in.
% :-decl_mpred(mudNearFeet(agent,list(tSpatialThing)),[predModule(user)]).
mudNearFeet(Agent,PerceptsO) :-  get_feet0(Agent,Percepts0),!,flatten_set(Percepts0,Percepts),delete(Percepts,Agent,PerceptsO).

get_feet0(Agent,Percepts):-
  call((
	looking(Agent),
	mudAtLoc(Agent,LOC),
        mudFacing(Agent,Facing),
        reverse_dir(Facing,Rev),
	get_mdir_u(Agent,[Facing,Rev],LOC,Percepts))),
	!.



%View list starting at vac'vSouth position and moving out in a clockwise spiral
%old_view_list([[vEast,vWest],[vNorth,vHere],[vNE,vHere],[vEast,vHere],[vSE,vHere],[vSouth,vHere],[vSW,vHere],
%	   [vWest,vHere],[vNW,vHere],[vNorth,vNorth],[vNorth,vNE],[vNE,vNE],[vEast,vNE],[vEast,vEast],[vEast,vSE],
%	   [vSE,vSE],[vSouth,vSE],[vSouth,vSouth],[vSouth,vSW],[vSW,vSW],[vWest,vSW],[vWest,vWest],[vWest,vNW],
%	   [vNW,vNW],[vNorth,vNW]]).

%grid of view, upper left (vNW) to lower right (vSE)
%This is the order the agents will receive their Percepts returned from get_all(Agent,) in
view_vectors([[vNW,vNW],[vNorth,vNW],[vNorth,vNorth],[vNorth,vNE],[vNE,vNE],
	    [vWest,vNW],[vNW,vHere],[vNorth,vHere],[vNE,vHere],[vEast,vNE],
	    [vWest,vWest],[vWest,vHere],[vDown,vUp],[vEast,vHere],[vEast,vEast],
	    [vWest,vSW],[vSW,vHere],[vSouth,vHere],[vSE,vHere],[vEast,vSE],
	    [vSW,vSW],[vSouth,vSW],[vSouth,vSouth],[vSouth,vSE],[vSE,vSE]]).

% A view list of only the locations immediately surrounding the agent.
near_vectors([[vNW,vHere],[vNorth,vHere],[vNE,vHere],
	[vWest,vHere],[vDown,vUp],[vEast,vHere],
	[vSW,vHere],[vSouth,vHere],[vSE,vHere]]).

:-dynamic(visually_blocked/2).
:-decl_mpred_prolog(visually_blocked(tAgentGeneric,ftListFn(vtDirection))).

:-listing(visually_blocked).

% Series of predicates to modify agents vision so return 'dar(k)' for locations
% which are blocked from view
% check_for_blocks(_Agent) :-!.
check_for_blocks(Agent) :-
	height_on_obj(Agent,Ht),
	clr(visually_blocked(Agent,_)),
	Dirs = [[vNorth,vHere],[vSouth,vHere],[vEast,vHere],[vWest,vHere],
	[vNE,vHere],[vNW,vHere],[vSE,vHere],[vSW,vHere]],
	view_dirs(Agent,Dirs,Percepts),
	blocked_percepts(Ht,Dirs,Percepts,[],Blocked_Percepts),
	add(visually_blocked(Agent,Blocked_Percepts)).
check_for_blocks(_).

prologSingleValued(height_on_obj(tSpatialThing,ftInt)).
prologSingleValued(mudSize(tSpatialThing,ftTerm)).
prologSingleValued(mudShape(tSpatialThing,ftTerm)).
% prologSingleValued(texture(tSpatialThing,term)).

% High enough to see over obstacles??
% Check to see how tall the agent is and if they are standing on an item
height_on_obj(Agent,Ht) :-
	mudAtLoc(Agent,LOC),
	report(LOC,Objs),
	member(Obj,Objs),
	props(Obj,mudHeight(ObjHt)),
	mudHeight(Agent,AgHt),
	Ht = (AgHt + ObjHt) - 1,!.
height_on_obj(Agent,Ht) :-
	mudHeight(Agent,Ht),!.


% Figure out if any obstacles are blocking vision...
blocked_percepts(_,[],[],Blocked_Percepts,Blocked_Percepts).
blocked_percepts(AgHt,[[D1,_]|Drest],[[P1|_]|Prest],Blocked_sofar,Blocked_Percepts) :-
	props(P1,mudHeight(ObjHt)),
	ObjHt > AgHt,
	block_coverage(D1,D1,Hidden),
	append(Hidden,Blocked_sofar,Blocked_sofar_tmp),
	!,
	blocked_percepts(AgHt,Drest,Prest,Blocked_sofar_tmp,Blocked_Percepts).
blocked_percepts(AgHt,[_|Drest],[_|Prest],Blocked_sofar,Blocked_Percepts) :-
	!,
	blocked_percepts(AgHt,Drest,Prest,Blocked_sofar,Blocked_Percepts).

% Blocks view for inbetween locations (eg.[vNorth,vHere] would block [vNorth,vNorth],[vNorth,vNE],[vNorth,vNW]).
block_coverage(vNorth,vNorth,[[vNorth,vNorth],[vNorth,vNE],[vNorth,vNW]]).
block_coverage(vSouth,vSouth,[[vSouth,vSouth],[vSouth,vSE],[vSouth,vSW]]).
block_coverage(vWest,vWest,[[vWest,vWest],[vWest,vNW],[vWest,vSW]]).
block_coverage(vEast,vEast,[[vEast,vEast],[vEast,vNE],[vEast,vSE]]).
block_coverage(D1,D2,[[D1,D2]]).

% These three predicates modifies Percepts so that blocked locations return 'dark'
alter_view(_Agent,[],[],[]).
alter_view(Agent,[[D1,D2]|Drest],[TP|TPrest],[P|Prest]) :-
	mem_test(Agent,D1,D2,YorN),
	alter_view(Agent,Drest,TPrest,Prest),
	dark_if_yes(YorN,[TP],P).

mem_test(Agent,D1,D2,YorN) :-
	visually_blocked(Agent,Bdirs),
	prop_memb([D1,D2],Bdirs),
	YorN = yes.
mem_test(_Agent,_,_,no).

dark_if_yes(yes,_,[vDark]).
%dark_if_yes(no,[[]],[]).
dark_if_yes(no,[P],P).

% Builds the Percepts list. (everything located up to 2 locations away from agent).
view_dirs(_,[],[]).
view_dirs(Agent,[[D1|D2]|Rest],Percepts) :-
      looking(Agent),
	view_dirs(Agent,Rest,Psofar),
	mudAtLoc(Agent,LOC),
	get_mdir_u(Agent,[D1|D2],LOC,What),
	append([What],Psofar,Percepts).

% The look loop (look at one location)
get_mdir(_Gent,[],LOC,What) :-
	report(LOC,What).
get_mdir(_Gent,[vHere],LOC,What) :-
	report(LOC,What).
get_mdir(Agent,[Dir|D],LOC,What) :-
	move_dir_target(LOC,Dir,XXYY),
	get_mdir(Agent,D,XXYY,What).

% The look loop (look at one location)
get_mdir_u(_Gent,[],LOC,What) :-
	report(LOC,What).
get_mdir_u(_Gent,[vHere],LOC,What) :-
	report(LOC,What).
get_mdir_u(Agent,[Dir|D],LOC,What) :-
	move_dir_target(LOC,Dir,XXYY),
	get_mdir_u(Agent,D,XXYY,What).
get_mdir_u(Agent,[_|D],LOC,What) :- 
   get_mdir_u(Agent,D,LOC,What).

% Reports everything at a location.
report(LOC,List) :-
	findall(Z,mudAtLoc(Z,LOC),List).

% Converts the objects seen... basically to weed out the 0'vSouth the empty locations report
mask([],What,What).
mask([K|Tail],SoFar,What) :-
	(K)=nil,
	!,
	mask(Tail, SoFar,What).
mask([Head|Tail],SoFar,What) :-
	mask(Tail,[Head|SoFar],What).

:- include(logicmoo(vworld/moo_footer)).
