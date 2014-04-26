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
% look_all(Agent,Chg,Dam,Fail,Scr,Percepts,Inv)
% instead of look_all(Agent,), these can be called separately
% charge(Agent,Chg) = charge (amount of charge agent has)
% damage(Agent,Dam) = damage
% success(Agent,Suc) = checks success of last action (actually checks the failure predicate)
% score(Agent,Scr) = score
% look_percepts(Agent,Percepts) = list of lists of objects in agents location plus 2 locations in each direction
% look_near(Agent,Percepts) = list of lists of objects in agents atloc plus 1 atloc in each dir
% look_feet(Agent,Percept) = list of objects in agents location
% inventory(Agt,Inv) = inventory (anything the agent has taken
% to do this.
%

%
% props(Obj,height(ObjHt))  == k(Obj,height,ObjHt) == p(height,Obj,ObjHt) == height(Obj,ObjHt)
% padd(Obj,height(ObjHt))  == add(p(height,Obj,ObjHt)) == add(p(height,Obj,ObjHt)) == add(height(Obj,ObjHt))
*/

:- module(look, [look_all/7, look_percepts/2,look_brief/1, look_to_fmt/1, look_near/2, look_feet/2,height_on_obj/2, inventory/2]).

:- include(logicmoo('vworld/vworld_header.pl')).

:- register_module_type(command).

:- dynamic blocks/1.


look_brief(Agent):- prop(Agent,last_command,X),functor(X,look,_),!.
look_brief(Agent):- look_to_fmt(Agent).

looking(Agent):- thlocal:current_agent(Agent),!.
looking(Agent):- thinking(Agent).

% ********** TOP LEVEL PREDICATE: this is the predicate agents use to look
% Look, reports everything not blocked up to two locations away
% plus the agents score, damage, charge, and if they succeeded at their last action.
% To make this action take a turn, change the first line to:
% Impliment(look_all(Agent,Vit,Dam,Suc,Scr,Percepts,Inv)) :-
look_all(Agent,Vit,Dam,Suc,Scr,Percepts,Inv) :-
	looking(Agent),
	ignore(charge(Agent,Vit)),
        ignore(damage(Agent,Dam)),
	ignore(success(Agent,Suc)),
	ignore(score(Agent,Scr)),
	ignore(inventory(Agent,Inv)),
	ignore((view_list(Dirs),
	check_for_blocks(Agent),
	view_dirs(Agent,Dirs,Tmp_percepts),
	alter_view(Dirs,Tmp_percepts,Percepts))),
	!.

% Get only the Percepts
look_percepts(Agent,Percepts) :-
	looking(Agent),
	view_list(Dirs),
	check_for_blocks(Agent),
	view_dirs(Agent,Dirs,Tmp_percepts),
	alter_view(Dirs,Tmp_percepts,Percepts),
	!.

% Look at locations immediately around argent
look_near(Agent,Percepts) :-
	looking(Agent),
	view_near_list(Dirs),
	view_dirs(Agent,Dirs,Percepts),
	!.

% Look only at location agent is currently in.
look_feet(Agent,Percept) :-
	looking(Agent),
	atloc(Agent,LOC),
        facing(Agent,Facing),
        reverse_dir(Facing,Rev),
	look_mdir(Agent,[Facing,Rev],LOC,Percept),
	!.

% Get only the Inv (inventory)
inventory(Agent, Inv) :-
	findall(Poss,possess(Agent,Poss),Inv).

%View list starting at vac's position and moving out in a clockwise spiral
%old_view_list([[e,w],[n,here],[ne,here],[e,here],[se,here],[s,here],[sw,here],
%	   [w,here],[nw,here],[n,n],[n,ne],[ne,ne],[e,ne],[e,e],[e,se],
%	   [se,se],[s,se],[s,s],[s,sw],[sw,sw],[w,sw],[w,w],[w,nw],
%	   [nw,nw],[n,nw]]).

%grid of view, upper left (nw) to lower right (se)
%This is the order the agents will receive their Percepts returned from look_all(Agent,) in
view_list([[nw,nw],[n,nw],[n,n],[n,ne],[ne,ne],
	    [w,nw],[nw,here],[n,here],[ne,here],[e,ne],
	    [w,w],[w,here],[d,u],[e,here],[e,e],
	    [w,sw],[sw,here],[s,here],[se,here],[e,se],
	    [sw,sw],[s,sw],[s,s],[s,se],[se,se]]).

% A view list of only the locations immediately surrounding the agent.
view_near_list([[nw,here],[n,here],[ne,here],
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
	Ht = (AgHt + ObjHt) - 1.
height_on_obj(Agent,Ht) :-
	height(Agent,Ht).
height_on_obj(Agent,Ht) :-
	height(Agent,Ht).

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
view_dirs(Agent,[[D1,D2]|Rest],Percepts) :-
	view_dirs(Agent,Rest,Psofar),
	atloc(Agent,LOC),
	look_mdir(Agent,[D1,D2],LOC,What),
	append([What],Psofar,Percepts).

% The look loop (look at one location)
look_mdir(_Gent,[],LOC,What) :-
	report(LOC,What).
look_mdir(_Gent,[here],LOC,What) :-
	report(LOC,What).
look_mdir(Agent,[Dir|D],LOC,What) :-
	move_dir_target(LOC,Dir,XXYY),
	look_mdir(Agent,D,XXYY,What).

% Reports everything at a location.
report(LOC,What) :-
	findall(Z,atloc(Z,LOC),List),
	mask(List,[],What).

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
	failure(Agent,_).
success(_,yes).


moo:decl_action(look).
moo:decl_action(look(dir)).
moo:decl_action(look(item)).

moo:agent_call_command(Agent,look):- with_assertions(thlocal:current_agent(Agent),look_to_fmt(Agent)).

scan_updates:-ignore(catch(make,_,true)).

look_to_fmt(Agent):-
        scan_updates,!,
        must(atloc(Agent,LOC)),!,
        locationToRegion(LOC,Region),
        must(look_location_fmt(Agent,Region)),!,
        ignore(look_exits_fmt(Agent,LOC)),!,
        must(look_loc_objects_fmt(Agent,LOC)),!,
        must(deliver_location_events(Agent,LOC)),!,
        ignore(look_all(Agent,Chg,Dam,Suc,Scr,Percepts,Inv)),
        ignore(height(Agent,Ht)),
        ignore(facing(Agent,Facing)),
        fmt('Agent=~w Ht=~w Facing=~w LOC=~w Charge=~w Dam=~w Success=~w Score=~w Inventory=~q ~n', [Agent,Ht,Facing,LOC,Chg,Dam,Suc,Scr,Inv]),
	ignore((nonvar(Percepts),write_pretty(Percepts))),
        must(ignore(((look_near(Agent,Stuff),
        fmt('STUFF=~q',[Stuff]))))),!.

look_exits_fmt(_Agent,LOC):-
  locationToRegion(LOC,Region),
   setof(D-E,pathBetween_call(Region,D,E),Set),
   forall_member(D-E,Set,once(describe_path(Region,D,E))).

describe_path(Region,D,E):-
   req(pathName(Region,D,S)) -> fmt('~w.',[S]) ;
   nameStrings(E,NS) -> fmt('~w is ~w',[D,NS]) ;
   fmt('~w ~w',[D,E]).   

look_location_fmt(Agent,Region):-
      must(show_room_grid(Region)),
      must(look_o_fmt(Agent,Region,4,6,'the name of this place',99)),!.

look_o_fmt(_Agent,O,LOW,_GOOD,WhatString,_Max):-
   order_descriptions(O,LOW-199,[Region|IST]) -> forall_member(M,[Region|IST],fmt0('~w.  ',[M])) ;
   setof(S,nameStrings(O,S),[Region|IST]) ->   forall_member(M,[Region|IST],fmt0('~w is ~w. ',[M,WhatString])) ;
   setof(S,mud_isa(O,S),List), fmt('~q is ~w',[mud_isa(O,List),WhatString]).

look_loc_objects_fmt(Agent,Region):- !,ignore((
     setof(O,inRegion(O,Region),Set),
       forall_member(O,Set,look_object_fmt(Agent,O)))).

look_loc_objects_fmt(Agent,Region):-
       props(Agent,[id(X)]),
       look_object_expect_for_fmt(Agent,Region,[same(X),classof(invisible)]).

look_object_expect_for_fmt(Agent,Region,ExceptFor):-
 setof(O,inRegion(O,Region),Set),
 forall(member(O,Set),(divide_match(O,ExceptFor,_,[]),look_object_fmt(Agent,O))).


look_object_fmt(Agent,O):-
 look_o_fmt(Agent,O,4,6,'is here.',1).

%% divide_match(-O,-InList,+Matches,+NotMatches)
divide_match(O,InList,Matches,NotMatches):-
   divide_match0(O,InList,MatchesI,NotMatchesI),!,
   NotMatches=NotMatchesI,Matches=MatchesI.

divide_match0(_,More,[],[]):-More==[],!.
divide_match0(O,[Test|For],True,False):-
   props(O,Test ) ->
   divide_match(O,For,[Test|True],False);
   divide_match(O,For,True,[Test|False]).

deliver_location_events(_Agent,_LOC):-true.

order_descriptions(O,DescSpecs,ListO):-findall(S,(description(O,S),meets_desc_spec(S,DescSpecs)),Rev),reverse(Rev,List),delete_repeats(List,ListO).

delete_repeats([],[]):-!.
delete_repeats([Region|List],[Region|ListO]):-delete(List,Region,ListM), delete_repeats(ListM,ListO),!.

meets_desc_spec(S0,_L):- string_to_atom(S0,A),atomic_list_concat([_,_|_],'mudBareHandDa',A),!,fail.
meets_desc_spec(S,From-To):- desc_len(S,Region),!, Region =< To, Region >= From.
meets_desc_spec(_,_).

desc_len(S0,Region):-string_to_atom(S0,S),
   atomic_list_concat(Words,' ',S),length(Words,Ws),atomic_list_concat(Sents,'.',S),length(Sents,Ss),Region is Ss+Ws,!.

% Display what the agent sees in a form which
% makes sense to me
write_pretty([]).
write_pretty(Percepts) :-
	write_pretty_aux(Percepts, Rest, 0),
	nl,
	write_pretty(Rest).

write_pretty_aux(Rest,Rest,5).
write_pretty_aux([[]|Tail],Return,Column) :-
	Ctemp is Column + 1,
	label_type(Obj,0),
	write(Obj), write(' '),
	write_pretty_aux(Tail,Return,Ctemp).
write_pretty_aux([[dark]|Tail],Return,Column) :-
	Ctemp is Column + 1,
	write('dk '),
	write_pretty_aux(Tail,Return,Ctemp).
write_pretty_aux([[Head]|Tail], Return, Column) :-
	Ctemp is Column + 1,
	label_type(Map,Head),
	write(Map), write(' '),
	write_pretty_aux(Tail, Return, Ctemp).
write_pretty_aux([[Agent]|Tail],Return,Column) :-
	Ctemp is Column + 1,
	mud_isa(Agent,agent),
	write('Ag'), write(' '),
	write_pretty_aux(Tail,Return,Ctemp).
write_pretty_aux([[_|_]|Tail],Return,Column) :-
	Ntemp is Column + 1,
	write('A+'), write(' '),
	write_pretty_aux(Tail,Return,Ntemp).

:- include(logicmoo('vworld/vworld_footer.pl')).


