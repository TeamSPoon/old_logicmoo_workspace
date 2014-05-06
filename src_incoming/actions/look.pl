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
% get_all(Agent,Chg,Dam,Fail,Scr,Percepts,Inv)
% instead of get_all(Agent,), these can be called separately
% charge(Agent,Chg) = charge (amount of charge agent has)
% damage(Agent,Dam) = damage
% success(Agent,Suc) = checks success of last action (actually checks the failure predicate)
% score(Agent,Scr) = score
% get_percepts(Agent,Percepts) = list of lists of objects in agents location plus 2 locations in each direction
% get_near(Agent,Percepts) = list of lists of objects in agents atloc plus 1 atloc in each dir
% get_feet(Agent,Percepts) = list of objects in agents location
% inventory(Agt,Inv) = inventory (anything the agent has taken
% to do this.
%

%
% props(Obj,height(ObjHt))  == k(Obj,height,ObjHt) == p(height,Obj,ObjHt) == height(Obj,ObjHt)
% padd(Obj,height(ObjHt))  == add(p(height,Obj,ObjHt)) == add(p(height,Obj,ObjHt)) == add(height(Obj,ObjHt))
*/

:- module(look, [get_all/7, get_percepts/2,  get_near/2, get_feet/2, height_on_obj/2, inventory/2, can_sense/5 , success/2, 
          remove_dupes/2,
          look_via_pred/3,
          call_look/2,
          default_repl_obj_to_string/3,
          default_repl_writer/4,
          flatten_dedupe/2]).

:- include(logicmoo('vworld/moo_header.pl')).

:- register_module_type(command).

:- dynamic blocks/1.

% can_sense(Agent,Sense,InList,CanDetect,CantDetect).
can_sense(_Agent,visual,InList,InList,[]).

moo:decl_action(examine(item), "view details of item (see also @list)").
moo:agent_call_command(_Gent,examine(SObj)):- term_listing(SObj).

looking(Agent):- get_session_id(O), thlocal:current_agent(O,Agent),!.
looking(Agent):- isa(Agent,agent).
% looking(Agent):- thinking(Agent).

% ********** TOP LEVEL PREDICATE: this is the predicate agents use to look
% Look, reports everything not blocked up to two locations away
% plus the agents score, damage, charge, and if they succeeded at their last action.
% To make this action take a turn, change the first line to:
% Impliment(get_all(Agent,Vit,Dam,Suc,Scr,Percepts,Inv)) :-
get_all(Agent,Vit,Dam,Suc,Scr,Percepts,Inv) :-
  call((
	looking(Agent),
	charge(Agent,Vit),
        damage(Agent,Dam),
	success(Agent,Suc),
	score(Agent,Scr),
	inventory(Agent,Inv),
	get_percepts(Agent,Percepts))),!.


flatten_dedupe(Percepts0,Percepts):-
   flatten([Percepts0],Percepts1),remove_dupes(Percepts1,Percepts).

% Get only the Percepts
get_percepts(Agent,Percepts) :- get_percepts0(Agent,Percepts0),!,flatten_dedupe(Percepts0,Percepts).
get_percepts0(Agent,Percepts) :-
  call((
	looking(Agent),
	view_vectors(Dirs),
	check_for_blocks(Agent),
	view_dirs(Agent,Dirs,Tmp_percepts),
	alter_view(Dirs,Tmp_percepts,Percepts))),
	!.

% Look at locations immediately around argent
get_near(Agent,Percepts):- get_near0(Agent,Percepts0),!,flatten_dedupe(Percepts0,Percepts).
   
get_near0(Agent,Percepts) :-
  call((
	looking(Agent),
	near_vectors(Dirs),
	view_dirs(Agent,Dirs,Percepts))),!.

% Look only at location agent is currently in.
get_feet(Agent,Percepts) :-  get_feet0(Agent,Percepts0),!,flatten_dedupe(Percepts0,Percepts).

get_feet0(Agent,Percepts):-
  call((
	looking(Agent),
	atloc(Agent,LOC),
        facing(Agent,Facing),
        reverse_dir(Facing,Rev),
	get_mdir_u(Agent,[Facing,Rev],LOC,Percepts))),
	!.

% Get only the Inv (inventory)
inventory(Agent,Percepts) :-  inventory0(Agent,Percepts0),!,flatten_dedupe(Percepts0,Percepts).
inventory0(Agent, Inv) :-
	findall(Poss,possess(Agent,Poss),Inv).

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

% Series of predicates to modify agents vision so return 'dar' for locations
% which are blocked from view
check_for_blocks(_Agent) :-!.
check_for_blocks(Agent) :-
	height_on_obj(Agent,Ht),
	clr(blocks(_)),
	Dirs = [[n,here],[s,here],[e,here],[w,here],
	[ne,here],[nw,here],[se,here],[sw,here]],
	view_dirs(Agent,Dirs,Percepts),
	blocked_percepts(Ht,Dirs,Percepts,[],Blocked_Percepts),
	add(blocks(Blocked_Percepts)).
check_for_blocks(_).

% High enough to see over obstacles??
% Check to see how tall the agent is and if they are standing on an object
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
alter_view([],[],[]).
alter_view([[D1,D2]|Drest],[TP|TPrest],[P|Prest]) :-
	mem_test(D1,D2,YorN),
	alter_view(Drest,TPrest,Prest),
	dark_if_yes(YorN,[TP],P).

mem_test(D1,D2,YorN) :-
	blocks(Bdirs),
	prop_memb([D1,D2],Bdirs),
	YorN = yes.
mem_test(_,_,no).

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
	integer(K),
	!,
	mask(Tail, SoFar,What).
mask([Head|Tail],SoFar,What) :-
	mask(Tail,[Head|SoFar],What).

% Check to see if last action was successful or not
success(Agent,no) :-
	failure(Agent,_),!.
success(_,yes).


moo:decl_action(look, "generalized look in region").
moo:decl_action(look(dir), "Look in a direction").
moo:decl_action(look(item), "Look at a speficific item").

moo:agent_call_command(Agent,look(Dir)):-
   view_dirs(Agent,[[Dir,here],[Dir,Dir],[Dir,Dir,adjacent]],Percepts),
   forall_member(P,Percepts,call_agent_action(Agent,examine(P))).

moo:agent_call_command(Agent,look(SObj)):-
   objects_match(Agent,SObj,Percepts),
   forall_member(P,Percepts,call_agent_action(Agent,examine(P))).

moo:agent_call_command(Agent,look):- 
   get_session_id(O),
   with_assertions(thlocal:current_agent(O,Agent),
        ((atloc(Agent,LOC),call_look(Agent,LOC)))).

moo:decl_db_prop(repl_writer(agent,term),[singleValued,default(default_repl_writer)]).
moo:decl_db_prop(repl_to_string(agent,term),[singleValued,default(default_repl_obj_to_string)]).

default_repl_writer(_TL,N,Type,V):-copy_term(Type,TypeO),ignore(TypeO=o),fmt('~q=(~w)~q.~n',[N,TypeO,V]).
default_repl_obj_to_string(O,Type,toString(TypeO,O)):-copy_term(Type,TypeO),ignore(TypeO=o).

call_wp(WP,TL,N,VT,V):-call(WP,TL,N,VT,V).

forall_call_wp(Call,WP,TL,N,VT,V):-forall(Call,call_wp(WP,TL,N,VT,V)).

call_look(Agent,LOC):-
       must( dbase:repl_writer(Agent,WPred  )),
        must((dbase:repl_to_string(Agent,ToSTR))),
        locationToRegion(LOC,Region),
        gensym(call_look,TL),
         call_wp(WPred  ,TL,call,term,show_room_grid(Region)),
         forall_call_wp(nameStrings(Region,E),WPred  ,TL,nameStrings,string,E),
         forall_call_wp(description(Region,E),WPred  ,TL,description,string,E),
         forall_call_wp(pathBetween_call(Region,D,E),WPred  ,TL,path(D),region,E),
         forall_call_wp(dbase:pathName(Region,D,E),WPred  ,TL,path(D),string,E),
         forall_call_wp(deliverable_location_events(Agent,LOC,Event),WPred  ,TL,event,term,Event),
         look_via_pred(WPred  ,ToSTR,
         [
         charge(Agent,value),
         movedist(Agent,value),
         damage(Agent,value),
         success=look:success(Agent,value),
         score(Agent,value),
         inventory(Agent,value),
         all(get_feet(Agent,value)),
         get_near(Agent,value),
         height(Agent,value),
         facing(Agent,value),
         height_on_obj(Agent,value),
         get_percepts(Agent,value),
         inRegion(value(ToSTR),Region)
         ]),
      retractall(telnet_fmt_shown(TL,_,_)).


look_via_pred(_,_,[]).
look_via_pred(WPred ,ToSTR,[L|List]):-!,
   catch((ignore(look_via_pred_0(WPred ,ToSTR,L);dmsg(failed(look_via_pred_0(WPred ,L))))),E,dmsg(error_failed(E,look_via_pred_0(WPred ,L)))),
   look_via_pred(WPred ,ToSTR,List).

look_via_pred_0(WPred ,ToSTR,F=Call):- !,look_via_pred_1(WPred ,ToSTR,F,Call).
look_via_pred_0(WPred ,ToSTR,once(Call)):- !,functor(Call,F,_), look_via_pred_1(WPred ,ToSTR,F,once(Call)).
look_via_pred_0(WPred ,ToSTR,all(Call)):- !,functor(Call,F,_), look_via_pred_1(WPred ,ToSTR,F,all(Call)).
look_via_pred_0(WPred ,ToSTR,Call):- functor(Call,F,_), look_via_pred_1(WPred ,ToSTR,F,Call).

look_via_pred_1(WPred ,ToSTR,F,all(Call)):-!,look_via_pred_2(WPred ,ToSTR,F,Call).
look_via_pred_1(WPred ,ToSTR,F,once(Call)):-!,look_via_pred_2(WPred ,ToSTR,F,once(Call)).
look_via_pred_1(WPred ,ToSTR,F,Call):-look_via_pred_2(WPred ,ToSTR,F,Call).

look_via_pred_2(WPred ,ToSTRIn,F,Call0):-
      wsubst(Call0,value(ToSTR),value,Call),
      ignore( ToSTR = (ToSTRIn) ),
      wsubst(Call,value,NewValue,GCall),
      % call_argIsa(F,.,.),
      look_via_pred_3(WPred ,ToSTR,F,_UnkType,GCall,NewValue).

look_via_pred_3(WPred ,ToSTR,F,Type,GCall,NewValue):-
  % dmsg(look_via_pred_3(WPred ,ToSTR,F,GCall,NewValue)),
      doall((catch(call(GCall),Error, NewValue=Error), 
             fmt_call(WPred ,ToSTR,F,Type,NewValue))).


fmt_call(WPred ,ToSTR,F,Type,NewValue):-flatten([NewValue],ValueList), NewValue\=ValueList,fmt_call(WPred ,ToSTR,F,Type,ValueList),!.
fmt_call(WPred ,ToSTR,N,Type,[V]):-fmt_call_pred(WPred ,ToSTR,N,Type,V),!.
fmt_call(WPred ,ToSTR,N,Type,[V|VV]):-remove_dupes([V|VV],RVs),reverse(RVs,Vs),fmt_call_pred(WPred ,ToSTR,N,Type,Vs),!.
fmt_call(WPred ,ToSTR,N,Type,V):-fmt_call_pred(WPred ,ToSTR,N,Type,V),!.

fmt_call_pred(WPred ,ToSTR,N,Type,[L|List]):-!, doall((member(V,[L|List]),fmt_call_pred_trans(WPred ,ToSTR,N,Type,V))).
fmt_call_pred(WPred ,ToSTR,N,Type,V0):-fmt_call_pred_trans(WPred ,ToSTR,N,Type,V0).

fmt_call_pred_trans(WPred ,ToSTR,N,Type,V0):-must((debugOnError(call(ToSTR,V0,Type,V)),!,debugOnError(call(WPred,_Tn,N,Type,V)))).



remove_dupes(In,Out):-remove_dupes(In,Out,[]).

remove_dupes([],[],_):-!.
remove_dupes([I|In],Out,Shown):-member(I,Shown),!,remove_dupes(In,Out,Shown).
remove_dupes([I|In],[I|Out],Shown):-remove_dupes(In,Out,[I|Shown]).



:- include(logicmoo('vworld/moo_footer.pl')).


