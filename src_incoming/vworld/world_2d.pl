/** <module> 
% This module defines the way we lay out 2-D grids into room
%
% Project Logicmoo: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
% :- module(world_2d,[]).

:- export(((
         check_for_fall/3,
         dir_offset/5,
         doorLocation/5,
         grid_size/4,
         in_grid/2,
         in_grid_rnd/2,
         in_world_move/3, 
         is_3d/1,
         loc_to_xy/4,
         move_dir_target/3,
         number_to_dir/3,  
         reverse_dir/2,
         round_loc/8,
         round_loc_target/8,
         to_3d/2))).

/*
% This file is "included" from world.pl 
*/


grid_dist(L1,L2,Dist):- to_3d(L1,L13D),to_3d(L2,L23D),dist(L13D,L23D,Dist),!.

dist(_,_,5).

% pathBetween_call(From,DirS,To):-string(DirS),!,atom_string(Dir,DirS),!,any_to_dir(Dir,Dir2),pathBetween(From,Dir2,To),same(Dir,Dir2).
pathBetween_call_0(From,Dir,To):-any_to_dir(Dir,Dir2),asserted_mpred_clause(pathBetween(From,Dir2,To)),same(Dir,Dir2).
pathBetween_call(From,Dir,To):-pathBetween_call_0(From,DirS,To), same(Dir,DirS).
   
% 5x5 rooms are average
%% to_3d(L1,L13D):-compound(L1)->L13D=L1; room_center(L1,X,Y,Z), L13D = xyz(L1,X,Y,Z).
to_3d(xyz(L1,X,Y,Z),xyz(L1,X,Y,Z)):- nonvar(L1),!.
to_3d(L1,xyz(L1,X,Y,Z)):-room_center(L1,X,Y,Z),!.


center_xyz(MaxX,MidX):- MidX is MaxX div 2 + MaxX mod 2.

room_center(Room,X,Y,Z):-
      grid_size(Room,MaxX,MaxY,MaxZ),
      center_xyz(MaxX,X),
      center_xyz(MaxY,Y),
      center_xyz(MaxZ,Z),!,
      dmsg(todo("get room size and calc center ",Room)).

locationToRegion(Obj,RegionIn):-locationToRegion_0(Obj,Region),!,RegionIn=Region.
locationToRegion_0(Obj,Obj):-var(Obj),dmsg(warn(var_locationToRegion(Obj,Obj))),!.
locationToRegion_0(xyz(Room,_,_,_),Region2):-nonvar(Room),!,locationToRegion_0(Room,Region2).
locationToRegion_0(Obj,Obj):-nonvar(Obj),isa(Obj,region),!. % inRegion(Obj,Room).
locationToRegion_0(Obj,Obj):-dmsg(warn(locationToRegion(Obj,Obj))),!.

loc_to_xy(LOC,X,Y,xyz(Room,X,Y,1)):- locationToRegion(LOC,Room),!.
loc_to_xy(Room,X,Y,xyz(Room,X,Y,1)).

is_3d(LOC):- compound(LOC).

% Quintus random(1,MaxX,X) and random(1,MaxY,Y)
grid_size(Room,MaxX,MaxY,MaxZ):- fail,
    tbox:type_grid(What,1,L),
   isa(Room,What),!,
   maxZ(MaxZ),
	length(L,MaxX),
	findall(1, tbox:type_grid(What,_,_),LL),
	length(LL,MaxY),!.

grid_size(_Room,MaxX,MaxY,MaxZ):- MaxX = 5 , MaxY = 5 , maxZ(MaxZ) ,!.

maxZ(2).

in_grid(LocName,Var):-var(Var),!,in_grid_rnd(LocName,Var).
in_grid(LocName,Var):-in_grid_no_rnd(LocName,Var).

in_grid_no_rnd(LocName,xyz(LocName,X,Y,Z)) :-
   grid_size(LocName,MaxX,MaxY, MaxZ),!,between(1,MaxX,X),between(1,MaxY,Y),between(1,MaxZ,Z).

in_grid_rnd(LocName,xyz(LocName,X,Y,1)) :-
   grid_size(LocName,MaxX,MaxY, _MaxZ),!,
   repeat,
	X is (1 + random(MaxX-2)),
	Y is (1 + random(MaxY-2)).	

% for now not useing grids
in_grid_rnd(LocName,xyz(LocName,1,1,1)).


init_location_grid(LocName):-
        isa(LocName,LocType),
        init_location_grid(LocName,LocType),!.

init_location_grid(LocName,LocType):-
        isa(LocName,LocType),
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
	moo:label_type(O,Type),
           rez_loc_object(xyz(LocName,X,Y,1),Type),
	K is X + 1,
	init3(LocName,LocType,xyz(LocName,K,Y,1),T).


rez_loc_object(_,0):-!.
rez_loc_object(XY,Type):-
           gensym(Type,Name2),
           Name = xyN(XY,Name2),           
           assert_isa(Name,Type),
           add(atloc(Name,XY)),!,
           add_missing_instance_defaults(Name).

nearby(X,Y):-atloc(X,L1),atloc(Y,L2),locs_near(L1,L2).

:-export(locs_near/2).
locs_near(L1,L2):- var(L1),nonvar(L2),!,locs_near(L2,L1).
locs_near(L1,L2):- nonvar(L1),nonvar(L2),L2=xyz(_,_,_,_),locationToRegion(L1,R),!,call_tabled(locs_near_i(R,L2)).
locs_near(L1,L2):- nonvar(L1),nonvar(L2),locationToRegion(L1,R1),locationToRegion(L2,R2),!,region_near(R1,R2).
locs_near(L1,L2):-region_near(R1,R2),in_grid_no_rnd(R1,L1),in_grid_no_rnd(R2,L2).

% :- decl_not_mpred(locs_near_i,2).
:-export(locs_near_i/2).
locs_near_i(L1,L2):- locationToRegion(L1,R),in_grid_no_rnd(R,L2).
locs_near_i(L1,L2):- locationToRegion(L1,R),pathBetween_call(R,_,R2),in_grid_no_rnd(R2,L2).

region_near(R1,R2):-pathBetween_call(R1,_,R2).
region_near(R1,R1).

moo:default_inst_props(OfAgent,agent,[facing(F),atloc(L)]):-ignore((nonvar(OfAgent),create_someval(facing,OfAgent,F),create_someval(atloc,OfAgent,L))).

moo:transitive_other(atloc,1,Obj,What):-inside_of(Obj,What).

:-export(inside_of/2).
inside_of(Obj,What):-is_asserted(stowed(What,Obj)).
inside_of(Obj,What):-is_asserted(wearing(What,Obj)).
inside_of(Obj,What):-is_asserted(contains(What,Obj)).

genlInverse(inside_of,possess).
genlInverse(wearing,inside_of).
genlInverse(contains,inside_of).
genlInverse(stowed,inside_of).

put_in_world(self):-!.
put_in_world(Agent):-loop_check(put_in_world_lc(Agent),true),!.

put_in_world_lc(Obj):-is_asserted(atloc(Obj,_)),!.
put_in_world_lc(Obj):-inside_of(Obj,What),ensure_in_world(What),!.
put_in_world_lc(Obj):-with_fallbacksg(with_fallbacks(put_in_world_lc_gen(Obj))),!.

put_in_world_lc_gen(Obj):-choose_for(facing,Obj,_),!,must_det((choose_for(atloc,Obj,LOC),nonvar(LOC))).


ensure_in_world(What):-must_det(put_in_world(What)).


:- dynamic_multifile_exported hook:decl_database_hook/2.
:- dynamic_multifile_exported hook:deduce_facts/2.
:- dynamic_multifile_exported hook:create_random_fact/1.
:- dynamic_multifile_exported hook:create_random_instance/3.
:- dynamic_multifile_exported hook:fact_maybe_deduced/1.

%  suggest a deducable fact that is probably not already asserted
hook:fact_maybe_deduced(inRegion(Obj,Region)):- is_asserted(atloc(Obj,LOC)),locationToRegion(LOC,Region),!.
hook:fact_maybe_deduced(inRegion(apath(Region,Dir),Region)):-is_asserted(pathBetween(Region,Dir,_)).

%  suggest a random fact that is probably not already true
hook:create_random_fact(atloc(Obj,LOC)) :- nonvar(Obj),!,asserted_or_deduced(inRegion(Obj,Region)),must_det((in_grid(Region,LOC),unoccupied(Obj,LOC))),!.
hook:create_random_fact(inRegion(Obj,Region)) :- nonvar(Obj),!,asserted_or_deduced(inRegion(Obj,Region)).

%  suggest random values
hook:create_random_instance(dir,Dir,Test) :- my_random_member(Dir,[n,s,e,w,ne,nw,se,sw]),Test,!.
hook:create_random_instance(int,3,Test):-call(Test),dmsg(create_random(int,3,Test)),dtrace,!,fail.

%  give required forward deductions
hook:deduce_facts(atloc(Obj,LOC),inRegion(Obj,Region)):- nonvar(LOC), locationToRegion(LOC,Region).
hook:deduce_facts(inRegion(Obj,_Region),atloc(Obj,LOC)):- nonvar(Obj), put_in_world(Obj),must_det(atloc(Obj,LOC)).


% random_region(LOC):- findall(O,isa(O,region),LOCS),my_random_member(LOC,LOCS).


random_xyz(LOC):-
   must_det(create_random(region,Region,true)),
   in_grid_rnd(Region,LOC),!.

random_xyz(xyz('Area1000',1,1,1)):-  trace_or_throw(game_not_loaded).

unoccupied(_,Loc):- not(is_asserted(atloc(_,Loc))),!.
unoccupied(_,_):-!.
unoccupied(Obj,Loc):- loop_check(unoccupied_lc(Obj,Loc),not(is_asserted(atloc(_,Loc)))),!.

unoccupied_lc(Obj,Loc):- is_occupied(Loc,What),!,What=Obj.
unoccupied_lc(_,_).

is_occupied(Loc,What):- is_asserted(atloc(What,Loc)),!.
is_occupied(Loc,What):- locationToRegion(Loc,Region),inRegion(What,Region),ensure_in_world(What),atloc(What,Loc),!.

% Used all over the place
% Transforms location based on cardinal direction given

calc_xyz(Region1,Dir,force(X1,Y1,Z1),X2,Y2,Z2):-
   to_3d(Region1,xyz(_,X,Y,Z)),
   dir_offset(Dir,1,OX,OY,OZ),
   X2 is X+ (OX*X1), Y2 is Y+OY*Y1, Z2 is Z+OZ*Z1.

move_dir_target(RegionXYZ,Dir,XXYY):-
   move_dir_target(RegionXYZ,Dir,1,XXYY).

move_dir_target(RegionXYZ,Dir,Force,XXYY):-
   (calc_xyz(RegionXYZ,Dir,force(Force,Force,Force),X,Y,Z)),
   (locationToRegion(RegionXYZ,Region1)),
   (round_loc_target(Region1,X,Y,Z,Region2,X2,Y2,Z2)),
   XXYY = xyz(Region2,X2,Y2,Z2),!,
   must_det(ground(XXYY)),
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
   atomic_list_concat_catch([NS,EW,UD],'',Dir),!.


:-export(dir_offset/5).

% :-decl_mpred_hybrid(dir_offset(string,int,int,int,int)).
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

hook:decl_database_hook(retract(_),atloc(Agent,_)):-padd(Agent,needs_look(true)).

% dir_mult(X,Y,Z,X1,Y1,Z1,X2,Y2,Z2):- X2 is X * X1,Y2 is Y * Y1,Z2 is Z * Z1.


% Used in move.pl, push.pl and climb.pl
% Move agent (usually). Used to relocate agent's location.
in_world_move(LOC,Agent,DirS) :-
        string_to_atom(DirS,Dir),
        ignore(is_asserted(atloc(Agent,LOC))),
        must_det((with_assertions(thlocal:noDefaultValues(atloc),in_world_move0(LOC,Agent,Dir)),        
         is_asserted(atloc(Agent,LOC2)),
         LOC2 \== LOC)),!.

can_world_move(LOC,_Agent,Dir) :- check_behind_for_ground(LOC), move_dir_target(LOC,Dir,_).

in_world_move0(LOC,Agent,Dir) :-
        padd(Agent,facing(Dir)),   
        check_behind_for_ground(LOC),
	move_dir_target(LOC,Dir,XXYY),!,
   must_det_l([
        dmsg(move_dir_target(LOC,Dir,XXYY)),
        locationToRegion(LOC,Region1),
        locationToRegion(XXYY,Region2),
        ((add(atloc(Agent,XXYY)),
        is_asserted(atloc(Agent,LOC2)),
         LOC2 \== LOC)),
   ifThen(( Region1\==Region2) , raise_location_event(LOC,notice(reciever,leave(Agent,Region1,to(Dir))))),
        reverse_dir(Dir,Rev),
   ifThen(( Region1\==Region2) , raise_location_event(XXYY,notice(reciever,enter(Agent,Region2,from(Rev))))),!,
	check_for_fall(LOC,XXYY,Agent)]).

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
	call_update_stats(Agent,fall).
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

num_near(0,d,here).
num_near(5,u,here).

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
        isa(Obj,Type),
	!.
scan_lists_aux([_|Rest],Type,M,N) :-
	Mtemp is M + 1,
	!,
	scan_lists_aux(Rest,Type,Mtemp,N).


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


