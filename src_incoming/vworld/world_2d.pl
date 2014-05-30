/** <module> 
% This module defines the way we lay out 2-D grids into room
%
% Project LogicMoo: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/

/*
% This file is "included" from world.pl 
*/


% :- include(logicmoo(vworld/moo_header)).

:- moodb:register_module_type(utility).

grid_dist(L1,L2,Dist):- to_3d(L1,L13D),to_3d(L2,L23D),dist(L13D,L23D,Dist),!.

dist(_,_,5).

pathBetween_call(From,Dir,To):-any_to_dir(Dir,Dir2),holds_t(pathBetween,From,Dir2,To),same(Dir,Dir2).
   
% 5x5 rooms are average
%% to_3d(L1,L13D):-compound(L1)->L13D=L1; room_center(L1,X,Y,Z), L13D = xyz(L1,X,Y,Z).
to_3d(xyz(L1,X,Y,Z),xyz(L1,X,Y,Z)):- nonvar(L1),!.
to_3d(L1,xyz(L1,X,Y,Z)):-room_center(L1,X,Y,Z),!.


center_xyz(MaxX,MidX):- MidX is MaxX div 2 + MaxX mod 2.

room_center(Room,X,Y,Z):-
      grid_size(Room,MaxX,MaxY,MaxZ),
      center_xyz(MaxX,X),
      center_xyz(MaxY,Y),
      center_xyz(MaxZ,Z),!.
      % doing it ! dmsg(todo("get room size and calc center ",Room)).


locationToRegion(xyz(Room,_,_,_),Region2):-!,locationToRegion(Room,Region2).
locationToRegion(Obj,Obj):-mud_isa(Obj,region),!. % inRegion(Obj,Room).

loc_to_xy(LOC,X,Y,xyz(Room,X,Y,1)):- locationToRegion(LOC,Room),!.
loc_to_xy(Room,X,Y,xyz(Room,X,Y,1)).

is_3d(LOC):- compound(LOC).

% Quintus random(1,MaxX,X) and random(1,MaxY,Y)
grid_size(Room,MaxX,MaxY,MaxZ):- fail,
   moo:type_grid(What,1,L),
   props(Room,mud_isa(What)),!,
   maxZ(MaxZ),
	length(L,MaxX),
	findall(1,moo:type_grid(What,_,_),LL),
	length(LL,MaxY),!.

grid_size(_Room,MaxX,MaxY,MaxZ):- MaxX = 5 , MaxY = 5 , maxZ(MaxZ) ,!.

maxZ(2).

in_grid(LocName,xyz(LocName,X,Y,1)) :-
   grid_size(LocName,MaxX,MaxY, _MaxZ),!,
   between(1,MaxX,X),
   between(1,MaxY,Y).

in_grid_rnd(LocName,xyz(LocName,X,Y,1)) :-
   grid_size(LocName,MaxX,MaxY, _MaxZ),!,
   repeat,
	X is (1 + random(MaxX-2)),
	Y is (1 + random(MaxY-2)).	

% for now not useing grids
in_grid_rnd(LocName,xyz(LocName,1,1,1)).


init_location_grid(LocName):-
        mud_isa(LocName,LocType),
        init_location_grid(LocName,LocType),!.

init_location_grid(LocName,LocType):-
        mud_isa(LocName,LocType),
        init2(LocName,LocType,1,1).

% process map file (world.map.pl)
init2(LocName,LocType,Y,1) :-
	grid(LocName,1,Y,L),
	!,
	init3(LocName,LocType,xyz(LocName,1,Y,_),L).
init2(_LocName,_LocType,_,_).

init3(LocName,LocType,xyz(LocName,_,Y,1),[]) :-
	!,
	X is Y + 1,
	init2(LocName,LocType,X,1).

init3(LocName,LocType,xyz(LocName,X,Y,1),[O|T]) :-
	label_type(O,Type),
           rez_object(xyz(LocName,X,Y,1),Type),
	K is X + 1,
	init3(LocName,LocType,xyz(LocName,K,Y,1),T).



nearby(X,Y):-atloc(X,L1),atloc(Y,L2),locs_near(L1,L2).

:-export(locs_near/2).
locs_near(L1,L2):- var(L1),nonvar(L2),!,locs_near(L2,L1).
locs_near(L1,L2):- nonvar(L1),nonvar(L2),L2=xyz(_,_,_,_),locationToRegion(L1,R),!,call_tabled(locs_near_i(R,L2)).
locs_near(L1,L2):- nonvar(L1),nonvar(L2),locationToRegion(L1,R1),locationToRegion(L2,R2),!,region_near(R1,R2).
locs_near(L1,L2):-region_near(R1,R2),in_grid(R1,L1),in_grid(R2,L2).

:-export(locs_near_i/2).
locs_near_i(L1,L2):- locationToRegion(L1,R),in_grid(R,L2).
locs_near_i(L1,L2):- locationToRegion(L1,R),pathBetween_call(R,_,R2),in_grid(R2,L2).

region_near(R1,R2):-pathBetween_call(R1,_,R2).
region_near(R1,R1).

moo:type_default_props(OfAgent,agent,[facing(F),atloc(L)]):-create_someval(facing,OfAgent,F),create_someval(atloc,OfAgent,L).

put_in_world(Agent):-ensure_some(facing,Agent),!,ensure_some(atloc,Agent),!.

ensure_some(Property,OfAgent):- prop(OfAgent, Property,_),!.
ensure_some(Property,OfAgent):- create_someval(Property,OfAgent,Value),padd(OfAgent,Property,Value).

create_someval(facing,_Agent,Dir) :- my_random_member(Dir,[n,s,e,w,ne,nw,se,sw]).
create_someval(atloc,Agent,Where) :- 
   defaultRegion(Agent,Region),
   in_grid(Region,Where),
   unoccupied(Where),!.
create_someval(atloc,_Agent,Loc) :- find_unoccupied(Loc).

defaultRegion(Agent,Region):- inRegion(Agent,Region),!.
defaultRegion(_Agent,Region):- inRegion(_,Region),!.
defaultRegion(_Agent,Region):- Region = 'Area1000'.


decide_region(LOC):- findall(O,region(O),LOCS),my_random_member(LOC,LOCS).

:-export(my_random_member/2).

my_random_member(LOC,LOCS):- length(LOCS,Len),Len>0, X is random(Len),nth0(X,LOCS,LOC).
find_unoccupied(Where):-
   must(decide_region(LOC)),
   in_grid_rnd(LOC,Where),
   unoccupied(Where),!.
find_unoccupied('Area1000'):- trace, throw(game_not_loaded).

unoccupied(_X):-!. %%not(atloc(_,X)).


% Used all over the place
% Transforms location based on cardinal direction given

calc_xyz(Region1,Dir,force(X1,Y1,Z1),X2,Y2,Z2):-
   to_3d(Region1,xyz(_,X,Y,Z)),
   dir_offset(Dir,1,OX,OY,OZ),
   X2 is X+ (OX*X1), Y2 is Y+OY*Y1, Z2 is Z+OZ*Z1.

move_dir_target(RegionXYZ,Dir,XXYY):-
   move_dir_target(RegionXYZ,Dir,1,XXYY).
move_dir_target(RegionXYZ,Dir,Force,XXYY):-
   hotrace(calc_xyz(RegionXYZ,Dir,force(Force,Force,Force),X,Y,Z)),
   hotrace(locationToRegion(RegionXYZ,Region1)),
   hotrace(round_loc_target(Region1,X,Y,Z,Region2,X2,Y2,Z2)),
   XXYY = xyz(Region2,X2,Y2,Z2),!,
   must(ground(XXYY)),
   check_ahead_for_ground(XXYY).


round_loc_target(Region1,X,Y,Z,Region3,X3,Y3,Z3):-
   round_loc(Region1,X,Y,Z,Region2,X2,Y2,Z2),!,
   Region2=Region3,X2=X3,Y2=Y3,Z2=Z3.

round_loc(Region1,X,Y,Z,Region2,X2,Y2,Z2):-
   compute_dir(Region1,X,Y,Z,Dir),!,
   round_loc_dir(Region1,X,Y,Z,Dir,Region2,X2,Y2,Z2),!.

round_loc_dir(Region1,X,Y,Z,'',Region2,X2,Y2,Z2):-!,
   X2=X,Y2=Y,Z2=Z,Region2=Region1.

round_loc_dir(Region1,X,Y,Z,Dir,Region2,X2,Y2,Z2):- 
   any_to_dir(Dir,DirLong),
   pathBetween_call(Region1,DirLong,Region2),!,
   grid_size(Region1,X1,Y1,Z1),
   calc_xyz(xyz(Region2,X,Y,Z),Dir,force(-X1,-Y1,-Z1),X2,Y2,Z2),!.

round_loc_dir(Region1,X,Y,Z,_Dir,Region2,X2,Y2,Z2):-Region2=Region1,X2=X,Y2=Y,Z2=Z.

compute_dir(Region1,X,Y,Z,Dir):-
  grid_size(Region1,MaxX,MaxY,MaxZ),
   ((X<1 -> EW=w ; X > MaxX -> EW=e ; EW= ''),
   (Y<1 -> NS=n ; Y > MaxY -> NS=s ; NS= ''),
   (Z<1 -> UD=d ; Z > MaxZ -> UD=u ; UD= '')),
   atomic_list_concat([NS,EW,UD],'',Dir),!.



dir_offset(u,F,0,0,F).
dir_offset(d,F,0,0,-F).
dir_offset(n,F,0,-F,0).
dir_offset(s,F,0,F,0).
dir_offset(e,F,F,0,0).
dir_offset(w,F,-F,0,0).
dir_offset(ne,F,F,-F,0).
dir_offset(sw,F,-F,-F,0).
dir_offset(se,F,F,F,0).
dir_offset(nw,F,-F,-F,0).
dir_offset(here,_,0,0,0).



% dir_mult(X,Y,Z,X1,Y1,Z1,X2,Y2,Z2):- X2 is X * X1,Y2 is Y * Y1,Z2 is Z * Z1.


% Used in move.pl, push.pl and climb.pl
% Move agent (usually). Used to relocate agent's location.
in_world_move(LOC,Agent,DirS) :-
        string_to_atom(DirS,Dir),
        ignore(atloc(Agent,LOC)),
        in_world_move0(LOC,Agent,Dir),
        atloc(Agent,LOC2),
        must(LOC2 \== LOC),
        padd(Agent,[needs_look(true)]).
  
in_world_move0(LOC,Agent,Dir) :-
        padd(Agent,facing(Dir)),
   ignore(atloc(Agent,LOC)),
        check_behind_for_ground(LOC),
	move_dir_target(LOC,Dir,XXYY),!,
        dmsg(move_dir_target(LOC,Dir,XXYY)),
        locationToRegion(LOC,Region1),
        locationToRegion(XXYY,Region2),
        clr(atloc(Agent,_)),
        add(atloc(Agent,XXYY)),
   ifThen(( Region1\==Region2) , raise_location_event(LOC,notice(reciever,leave(Agent,Region1,to(Dir))))),
        reverse_dir(Dir,Rev),
   ifThen(( Region1\==Region2) , raise_location_event(XXYY,notice(reciever,enter(Agent,Region2,from(Rev))))),!,
	check_for_fall(LOC,XXYY,Agent).

check_behind_for_ground(LOC):-nonvar(LOC).
check_ahead_for_ground(XXYY):-nonvar(XXYY),
   to_3d(XXYY,xyz(L1,X,Y,Z)),
   grid_size(L1,MX,MY,MZ),
   inside_grid(L1,X,Y,Z,MX,MY,MZ).

inside_grid(_L1,X,Y,Z,MX,MY,MZ):-is_between(1,MX,X),is_between(1,MY,Y),is_between(1,MZ,Z).

is_between(L,H,V):- H >= V,L =< V.

% Used for every move
% Does the agent take a header off a high object?
check_for_fall(LOC,XXYY,Agent) :-
	atloc(HighObj,LOC),
	props(HighObj,height(Hh)),
        % if nothing is there pretend it is 1
	(not(atloc(_,XXYY)) -> Hl = 1; atloc(LowObj,XXYY)),
	props(LowObj,height(Hl)),
	Hd is Hh - Hl,
	Hd > 1,
	moo:update_stats(Agent,fall).
check_for_fall(_,_,_).


% Reverses the direction returned by number_to_direction
% Used for fleeing
reverse_dir(d,u).
reverse_dir(u,d).
reverse_dir(n,s).
reverse_dir(s,n).
reverse_dir(e,w).
reverse_dir(w,e).
reverse_dir(nw,se).
reverse_dir(ne,sw).
reverse_dir(sw,ne).
reverse_dir(se,nw).
reverse_dir(Was,reverseOf(Was)):-nonvar(Was).

% Yet another hash table to covert numbers into directions (or the reverse).
num_near(1,nw,here).
num_near(2,n,here).
num_near(3,ne,here).
num_near(4,w,here).
num_near(6,e,here).
num_near(7,sw,here).
num_near(8,s,here).
num_near(9,se,here).

num_near(0,u,here).
num_near(5,d,here).

% Translates numbers returned from scan_lists_aux/3 (the number of the location)
% into thier relative directions.
number_to_dir(1,nw,nw).
number_to_dir(2,n,nw).
number_to_dir(3,n,n).
number_to_dir(4,n,ne).
number_to_dir(5,ne,ne).
number_to_dir(6,w,nw).
number_to_dir(7,nw,here).
number_to_dir(8,n,here).
number_to_dir(9,ne,here).
number_to_dir(10,e,ne).
number_to_dir(11,w,w).
number_to_dir(12,w,here).
number_to_dir(14,e,here).
number_to_dir(15,e,e).
number_to_dir(16,w,sw).
number_to_dir(17,sw,here).
number_to_dir(18,s,here).
number_to_dir(19,se,here).
number_to_dir(20,e,se).
number_to_dir(21,sw,sw).
number_to_dir(22,s,sw).
number_to_dir(23,s,s).
number_to_dir(24,s,se).
number_to_dir(25,se,se).


% Scans through list of perceptions (as returned by look_percepts(Agent,L) or look_all(NearAgt,_,_,_,L,_))
% for an object, returns the direction in which the object lies.
list_object_dir_sensed(_,List,Type,Dir) :-
	!,
	scan_lists_aux(List,Type,1,N),
	number_to_dir(N,Dir,_).


list_object_dir_near(List,Type,Dir) :-
	!,
	scan_lists_aux(List,Type,1,N),
	num_near(N,Dir,_).

scan_lists_aux([Loc|_],Type,N,N) :-
	member(Obj,Loc),
        mud_isa(Obj,Type),
	!.
scan_lists_aux([_|Rest],Type,M,N) :-
	Mtemp is M + 1,
	!,
	scan_lists_aux(Rest,Type,Mtemp,N).

rez_object(_,0):-!.
rez_object(XY,Type):-
           gensym(Type,Name2),
           Name =..[Type,xyN(XY,Name2)],
           !,
           add(atloc(Name,XY)),!.


doorLocation(_Room,3,0,_Z,n).
doorLocation(_Room,2,0,_Z,n).
doorLocation(_Room,4,0,_Z,n).
doorLocation(_Room,3,6,_Z,s).
doorLocation(_Room,2,6,_Z,s).
doorLocation(_Room,4,6,_Z,s).
doorLocation(_Room,0,2,_Z,w).
doorLocation(_Room,0,3,_Z,w).
doorLocation(_Room,0,4,_Z,w).
doorLocation(_Room,6,2,_Z,e).
doorLocation(_Room,6,3,_Z,e).
doorLocation(_Room,6,4,_Z,e).
doorLocation(_Room,6,0,_Z,ne).
doorLocation(_Room,6,6,_Z,se).
doorLocation(_Room,0,0,_Z,nw).
doorLocation(_Room,6,0,_Z,sw).
doorLocation(_Room,_X,_Y,_Z,_Dir):-!,fail.

% :- include(logicmoo(vworld/moo_footer)).

