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
% get_percepts(Agent,Percepts) = list of lists of objects in agents location plus 2 locations in each direction
% get_near(Agent,Percepts) = list of lists of objects in agents atloc plus 1 atloc in each dir
% get_feet(Agent,Percepts) = list of objects in agents location
%

%
% props(Obj,height(ObjHt))  == svo(Obj,height,ObjHt) == p(height,Obj,ObjHt) == height(Obj,ObjHt)
% padd(Obj,height(ObjHt))  == add(p(height,Obj,ObjHt)) == add(p(height,Obj,ObjHt)) == add(height(Obj,ObjHt))
*/

% :-swi_module(user). 
:-swi_module(look, [ get_percepts/2,  get_near/2, get_feet/2, height_on_obj/2, can_sense/5 , call_look/2]).

:- include(logicmoo(vworld/moo_header)).

:- register_module_type(command).

:- dynamic blocks/1.



% can_sense(Agent,Sense,InList,CanDetect,CantDetect).
can_sense(_Agent,visual,InList,InList,[]).

action_info(examine(item), "view details of item (see also @list)").
agent_call_command(_Gent,examine(SObj)):- term_listing(SObj).

visibleTo(Agent,Agent).
visibleTo(Agent,Obj):-possess(Agent,Obj).
visibleTo(Agent,Obj):-same_regions(Agent,Obj).

term_specifier_text(Prep,prepstr_spatial):-member(Prep,[in,on,north_of,inside,onto,ontop]).
term_specifier_text(Prep,prepstr_spatial):-term_specifier_text(Prep,prepstr_dir_of).
term_specifier_text([Dir,of],prepstr_dir_of):-term_specifier_text(Dir,dir).

action_info(look, "generalized look in region").
action_info(look(optionalStr("in"),optionalStr("here")), "generalized look in region").
action_info(look(prepstr_dir_of,optionalStr("self")), "Look in a direction (TODO: look north of self)").
action_info(look(optional(prepstr_spatial,"at"),obj),"look [in|at|on|under|at] somewhere").
%action_info(look(obj), "Look at a speficific item").
%action_info(look_at(optional(call(visibleTo(self,value)),call(visibleTo(self,value)))), "Look at a speficific item").

agent_call_command(Agent,look):- look_as(Agent),!.
agent_call_command(Agent,look("here")):- look_as(Agent),!.
agent_call_command(Agent,look(_,"here")):- look_as(Agent),!.
agent_call_command(Agent,look(Dir,"self")):- get_term_specifier_text(Dir,dir),!,
   view_dirs(Agent,[[Dir,here],[Dir,Dir],[Dir,Dir,adjacent]],Percepts),
   forall_member(P,Percepts,call_agent_action(Agent,examine(P))).
agent_call_command(Agent,look(_Dir,SObj)):-
   objects_match_for_agent(Agent,SObj,obj,Percepts),
   forall_member(P,Percepts,call_agent_action(Agent,examine(P))).

:-swi_export(look_as/1).
look_as(Agent):-
   get_session_id(O),
   with_assertions(thlocal:session_agent(O,Agent),
        ((atloc(Agent,LOC),call_look(Agent,LOC)))).


:-swi_export(call_look/2).
call_look(Agent,LOC):-  mmake, call(call_look_proc,Agent,LOC).

:-decl_mpred_prolog(call_look_proc/3).
call_look_proc(Agent,LOC):-
   clr(props(Agent,needs_look(true))),
   add(props(Agent,needs_look(false))),    
    toploop_output:show_kb_preds(Agent,LOC,
         [
      % TODO make this work
         %  why does this this work on Prolog REPL?
         %   with_output_to(string(Str),show_room_grid('Area1000'))
         %  but yet this doent?
       %   show_room_grid = once(with_output_to(string(value),show_room_grid(region))),
         % for now workarround is 
         call(show_room_grid(region)),
         atloc(Agent,value),
         nameStrings(region,value),
         forEach(description(region,Value),fmt(region_desc(Value))),
         events=deliverable_location_events(Agent,LOC,value),
         path(D) = pathBetween_call(region,D,value),
         pathName(D) = pathName(region,D,value),
         value = is_asserted(localityOfObject(value,region)),       
         facing(Agent,value),
         get_feet(Agent,value),
         get_near(Agent,value),
         get_percepts(Agent,value),         
         movedist(Agent,value),
         height_on_obj(Agent,value),
         listof(possess(Agent,value)),
         inventory(Agent,value),         
         success=success(Agent,value)
       ]).


looking(Agent):- current_agent(Agent),!.
looking(Agent):- agent(Agent). % ,thinking(Agent).

% ********** TOP LEVEL PREDICATE: this is the predicate agents use to look
% Look, reports everything not blocked up to two locations away
% plus the agents score, damage, charge, and if they succeeded at their last action.
% To make this action take a turn, change the first line to:
% Impliment(get_all(Agent,Vit,Dam,Suc,Scr,Percepts,Inv)) :-
get_all(Agent,Vit,Dam,Suc,Scr,Percepts,Inv) :-
  call((
	looking(Agent),
	charge(Agent,Vit),
        health(Agent,Dam),
	success(Agent,Suc),
	score(Agent,Scr),
	inventory(Agent,Inv),
	get_percepts(Agent,Percepts))),!.


% Get only the Percepts

% :-decl_mpred(get_percepts(agent,list(spatialthing)),[ask_module(user)]).
get_percepts(Agent,Percepts) :- get_percepts0(Agent,Percepts0),!,flatten_set(Percepts0,Percepts).
get_percepts0(Agent,Percepts) :-
  call((
	looking(Agent),
	view_vectors(Dirs),
	check_for_blocks(Agent),
	view_dirs(Agent,Dirs,Tmp_percepts),
	alter_view(Agent,Dirs,Tmp_percepts,Percepts))),
	!.

% Look at locations immediately around argent
% :-decl_mpred(get_near(agent,list(spatialthing)),[ask_module(user)]).
get_near(Agent,PerceptsO):- get_near0(Agent,Percepts0),!,flatten_set(Percepts0,Percepts),delete(Percepts,Agent,PerceptsO).
   
get_near0(Agent,Percepts) :-
  call((
	looking(Agent),
	near_vectors(Dirs),
	view_dirs(Agent,Dirs,Percepts))),!.

% Look only at location agent is currently in.
% :-decl_mpred(get_feet(agent,list(spatialthing)),[ask_module(user)]).
get_feet(Agent,PerceptsO) :-  get_feet0(Agent,Percepts0),!,flatten_set(Percepts0,Percepts),delete(Percepts,Agent,PerceptsO).

get_feet0(Agent,Percepts):-
  call((
	looking(Agent),
	atloc(Agent,LOC),
        facing(Agent,Facing),
        reverse_dir(Facing,Rev),
	get_mdir_u(Agent,[Facing,Rev],LOC,Percepts))),
	!.



%View list starting at vac's position and moving out in a clockwise spiral
%old_view_list([[e,w],[n,here],[ne,here],[e,here],[se,here],[s,here],[sw,here],
%	   [w,here],[nw,here],[n,n],[n,ne],[ne,ne],[e,ne],[e,e],[e,se],
%	   [se,se],[s,se],[s,s],[s,sw],[sw,sw],[w,sw],[w,w],[w,nw],
%	   [nw,nw],[n,nw]]).

%grid of view, upper left (nw) to lower right (se)
%This is the order the agents will receive their Percepts returned from get_all(Agent,) in
view_vectors([[nw,nw],[n,nw],[n,n],[n,ne],[ne,ne],
	    [w,nw],[nw,here],[n,here],[ne,here],[e,ne],
	    [w,w],[w,here],[d,u],[e,here],[e,e],
	    [w,sw],[sw,here],[s,here],[se,here],[e,se],
	    [sw,sw],[s,sw],[s,s],[s,se],[se,se]]).

% A view list of only the locations immediately surrounding the agent.
near_vectors([[nw,here],[n,here],[ne,here],
	[w,here],[d,u],[e,here],
	[sw,here],[s,here],[se,here]]).

:-dynamic(visually_blocked/2).
:-decl_mpred_prolog(visually_blocked(agent,list)).

:-listing(visually_blocked).

% Series of predicates to modify agents vision so return 'dar(k)' for locations
% which are blocked from view
% check_for_blocks(_Agent) :-!.
check_for_blocks(Agent) :-
	height_on_obj(Agent,Ht),
	clr(visually_blocked(Agent,_)),
	Dirs = [[n,here],[s,here],[e,here],[w,here],
	[ne,here],[nw,here],[se,here],[sw,here]],
	view_dirs(Agent,Dirs,Percepts),
	blocked_percepts(Ht,Dirs,Percepts,[],Blocked_Percepts),
	add(visually_blocked(Agent,Blocked_Percepts)).
check_for_blocks(_).

singleValued(height_on_obj(spatialthing,int)).
singleValued(size(spatialthing,term)).
singleValued(shape(spatialthing,term)).
% singleValued(texture(spatialthing,term)).

% High enough to see over obstacles??
% Check to see how tall the agent is and if they are standing on an item
height_on_obj(Agent,Ht) :-
	atloc(Agent,LOC),
	report(LOC,Objs),
	member(Obj,Objs),
	props(Obj,height(ObjHt)),
	height(Agent,AgHt),
	Ht = (AgHt + ObjHt) - 1,!.
height_on_obj(Agent,Ht) :-
	height(Agent,Ht),!.


% Figure out if any obstacles are blocking vision...
blocked_percepts(_,[],[],Blocked_Percepts,Blocked_Percepts).
blocked_percepts(AgHt,[[D1,_]|Drest],[[P1|_]|Prest],Blocked_sofar,Blocked_Percepts) :-
	props(P1,height(ObjHt)),
	ObjHt > AgHt,
	block_coverage(D1,D1,Hidden),
	append(Hidden,Blocked_sofar,Blocked_sofar_tmp),
	!,
	blocked_percepts(AgHt,Drest,Prest,Blocked_sofar_tmp,Blocked_Percepts).
blocked_percepts(AgHt,[_|Drest],[_|Prest],Blocked_sofar,Blocked_Percepts) :-
	!,
	blocked_percepts(AgHt,Drest,Prest,Blocked_sofar,Blocked_Percepts).

% Blocks view for inbetween locations (eg.[n,here] would block [n,n],[n,ne],[n,nw]).
block_coverage(n,n,[[n,n],[n,ne],[n,nw]]).
block_coverage(s,s,[[s,s],[s,se],[s,sw]]).
block_coverage(w,w,[[w,w],[w,nw],[w,sw]]).
block_coverage(e,e,[[e,e],[e,ne],[e,se]]).
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

dark_if_yes(yes,_,[dark]).
%dark_if_yes(no,[[]],[]).
dark_if_yes(no,[P],P).

% Builds the Percepts list. (everything located up to 2 locations away from agent).
view_dirs(_,[],[]).
view_dirs(Agent,[[D1|D2]|Rest],Percepts) :-
      looking(Agent),
	view_dirs(Agent,Rest,Psofar),
	atloc(Agent,LOC),
	get_mdir_u(Agent,[D1|D2],LOC,What),
	append([What],Psofar,Percepts).

% The look loop (look at one location)
get_mdir(_Gent,[],LOC,What) :-
	report(LOC,What).
get_mdir(_Gent,[here],LOC,What) :-
	report(LOC,What).
get_mdir(Agent,[Dir|D],LOC,What) :-
	move_dir_target(LOC,Dir,XXYY),
	get_mdir(Agent,D,XXYY,What).

% The look loop (look at one location)
get_mdir_u(_Gent,[],LOC,What) :-
	report(LOC,What).
get_mdir_u(_Gent,[here],LOC,What) :-
	report(LOC,What).
get_mdir_u(Agent,[Dir|D],LOC,What) :-
	move_dir_target(LOC,Dir,XXYY),
	get_mdir_u(Agent,D,XXYY,What).
get_mdir_u(Agent,[_|D],LOC,What) :- 
   get_mdir_u(Agent,D,LOC,What).

% Reports everything at a location.
report(LOC,List) :-
	findall(Z,atloc(Z,LOC),List).

% Converts the objects seen... basically to weed out the 0's the empty locations report
mask([],What,What).
mask([K|Tail],SoFar,What) :-
	(K)=nil,
	!,
	mask(Tail, SoFar,What).
mask([Head|Tail],SoFar,What) :-
	mask(Tail,[Head|SoFar],What).

:- include(logicmoo(vworld/moo_footer)).


