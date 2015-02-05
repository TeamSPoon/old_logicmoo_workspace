/** <module> 
% This module defines the way we lay out 2-D grids into room
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13,2035
%
*/
% :-swi_module(world_2d,[]).

:- dynamic_multifile_exported(((
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
:- leash(+all).


grid_dist(L1,L2,Dist):- to_3d(L1,L13D),to_3d(L2,L23D),dist(L13D,L23D,Dist),!.

dist(_,_,5).

:-decl_mpred_prolog(pathBetween_call(tRegion,vtDirection,tRegion)).

% pathBetween_call(From,DirS,To):-string(DirS),!,atom_string(Dir,DirS),!,any_to_dir(Dir,Dir2),pathBetween(From,Dir2,To),same(Dir,Dir2).
pathBetween_call_0(From,Dir,To):-any_to_dir(Dir,Dir2),is_asserted(pathBetween(From,Dir2,To)),same(Dir,Dir2).
pathBetween_call(From,Dir,To):-pathBetween_call_0(From,DirS,To),same(Dir,DirS).
   
% 5x5 rooms are average
%% to_3d(L1,L13D):-compound(L1)->L13D=L1; room_center(L1,X,Y,Z),L13D = xyz(L1,X,Y,Z).
to_3d(xyzFn(L1,X,Y,Z),xyzFn(L1,X,Y,Z)):- nonvar(L1),!.
to_3d(L1,xyzFn(L1,X,Y,Z)):-room_center(L1,X,Y,Z),!.


center_xyz(MaxX,MidX):- MidX is MaxX div 2 + MaxX mod 2.

room_center(Region,X,Y,Z):-
      grid_size(Region,MaxX,MaxY,MaxZ),
      center_xyz(MaxX,X),
      center_xyz(MaxY,Y),
      center_xyz(MaxZ,Z),!,
      dmsg(todo("get room size and calc center ",Region)).

loc_to_xy(LOC,X,Y,xyzFn(Region,X,Y,1)):- locationToRegion(LOC,Region),!.
loc_to_xy(Region,X,Y,xyzFn(Region,X,Y,1)).

is_3d(LOC):- compound(LOC).

% Quintus random(1,MaxX,X) and random(1,MaxY,Y)
grid_size(Region,MaxX,MaxY,MaxZ):- fail,
    typeGrid(What,1,L),
   mudIsa(Region,What),!,
   maxZ(MaxZ),
	length(L,MaxX),
	findall(1,typeGrid(What,_,_),LL),
	length(LL,MaxY),!.

grid_size(_Room,MaxX,MaxY,MaxZ):- MaxX = 5 ,MaxY = 5 ,maxZ(MaxZ) ,!.

maxZ(2).

in_grid(LocName,Var):-var(Var),!,in_grid_rnd(LocName,Var).
in_grid(LocName,Var):-in_grid_no_rnd(LocName,Var).

in_grid_no_rnd(LocName,xyzFn(LocName,X,Y,Z)) :- !,
   grid_size(LocName,MaxX,MaxY,MaxZ),!,between(1,MaxX,X),between(1,MaxY,Y),between(1,MaxZ,Z).
in_grid_no_rnd(LocName,LocName).

in_grid_rnd(LocName,xyzFn(LocName,X,Y,1)) :-
   grid_size(LocName,MaxX,MaxY,_MaxZ),!,
   repeat,
	X is (1 + random(MaxX-2)),
	Y is (1 + random(MaxY-2)).	

% for now not useing grids
in_grid_rnd(LocName,xyzFn(LocName,1,1,1)).


init_location_grid(LocName):-
        mudIsa(LocName,LocType),
        init_location_grid(LocName,LocType),!.

init_location_grid(LocName,LocType):-
        mudIsa(LocName,LocType),
        init2(LocName,LocType,1,1).

% process map file (world.map.pl)
init2(LocName,LocType,Y,1) :-
	gridValue(LocName,1,Y,L),
	!,
	init3(LocName,LocType,xyzFn(LocName,1,Y,_),L).
init2(_LocName,_LocType,_,_).

init3(LocName,LocType,xyzFn(LocName,_,Y,1),[]) :-
	!,
	X is Y + 1,
	init2(LocName,LocType,X,1).

init3(LocName,LocType,xyzFn(LocName,X,Y,1),[O|T]) :-
	label_type(O,Type),
           rez_loc_object(xyzFn(LocName,X,Y,1),Type),
	K is X + 1,
	init3(LocName,LocType,xyzFn(LocName,K,Y,1),T).


% rez_loc_object(_,0):-!.
rez_loc_object(XY,Type):-
           gensym(Type,Name2),
           Name = xyN(XY,Name2),          
           assert_isa(Name,Type),
           add(mudAtLoc(Name,XY)),!,
           add_missing_instance_defaults(Name).


%prologOnly(mudNearbyObjs(tObj,tObj)).
%prologOnly(mudNearbyObjs(tObj,tObj)).
%predModule(mudNearbyObjs(tObj,tObj),user).
mudNearbyObjs(X,Y):-mudAtLoc(X,L1),mudAtLoc(Y,L2),mudNearbyLocs(L1,L2).

locationToRegion(Obj,RegionIn):-var(Obj),!,dmsg(warn(var_locationToRegion(Obj,RegionIn))),mudIsa(RegionIn,tRegion).
locationToRegion(Obj,RegionIn):-locationToRegion_0(Obj,Region),sanity((nonvar(Region),mudIsa(Region,tRegion))),!,RegionIn=Region.
locationToRegion_0(Obj,Obj):-var(Obj),dmsg(warn(var_locationToRegion(Obj,Obj))),!.
locationToRegion_0(xyzFn(Region,_,_,_),Region2):-nonvar(Region),!,locationToRegion_0(Region,Region2).
locationToRegion_0(Obj,Obj):-nonvar(Obj),!,mudIsa(Obj,tRegion),!.
locationToRegion_0(Obj,Region):-nonvar(Obj),must(localityOfObject(Obj,Location)),!,locationToRegion_0(Location,Region).
locationToRegion_0(Obj,Obj):-dmsg(warn(locationToRegion(Obj,Obj))),!.

:-dynamic_multifile_exported(mudNearbyLocs/2).
mudNearbyLocs(L1,L2):- var(L1),nonvar(L2),!,mudNearbyLocs(L2,L1).
mudNearbyLocs(L1,L2):- nonvar(L1),nonvar(L2),L2=xyzFn(_,_,_,_),locationToRegion(L1,R),!,call_tabled(locs_near_i(R,L2)).
mudNearbyLocs(L1,L2):- nonvar(L1),nonvar(L2),locationToRegion(L1,R1),locationToRegion(L2,R2),!,mudNearbyRegions(R1,R2).
mudNearbyLocs(L1,L2):- must((hotrace(mudNearbyRegions(R1,R2)),in_grid_no_rnd(R1,L1),in_grid_no_rnd(R2,L2))).

% :- decl_not_mpred(locs_near_i,2).
:-dynamic_multifile_exported(locs_near_i/2).
locs_near_i(L1,L2):- locationToRegion(L1,R),in_grid_no_rnd(R,L2).
locs_near_i(L1,L2):- locationToRegion(L1,R),pathBetween_call(R,_,R2),in_grid_no_rnd(R2,L2).

mudNearbyRegions(R1,R2):-pathBetween_call(R1,_,R2).
mudNearbyRegions(R1,R1).

% 345345  instTypeProps(OfAgent,agent,[facing(F),atloc(L)]):-  dfsdfd ignore((nonvar(OfAgent),create_someval(facing,OfAgent,F),create_someval(atloc,OfAgent,L))).

transitive_other(mudAtLoc,1,Obj,What):-mudInsideOf(Obj,What).
% transitive_other(localityOfObject,1,Obj,What):-mudInsideOf(Obj,What).

:-decl_mpred_hybrid(mudInsideOf/2).
:-dynamic_multifile_exported(mudInsideOf/2).
mudInsideOf(Inner,Outer):-is_asserted(mudStowing(Outer,Inner)).
mudInsideOf(Inner,Outer):-is_asserted(mudContains(Outer,Inner)).


moves_with(Obj1,Obj2):-nonvar(Obj2),!,moves_with(Obj2,Where),localityOfObject(Where,Obj1).
moves_with(Obj1,Obj2):-moves_with_sym(Obj1,Obj2).
moves_with(Obj1,Obj2):-moves_with_sym(Obj2,Obj1).
moves_with_sym(Obj1,Obj2):-localityOfObject(Where,Obj1),moves_with(Obj2,Where).

mudLocOnSurface(Clothes,Agent):-loop_check(wearsClothing(Agent,Clothes),fail).

:-export(same_regions/2).
same_regions(Agent,Obj):-must(inRegion(Agent,Where1)),dif_safe(Agent,Obj),inRegion(Obj,Where2),Where1=Where2.

%:-decl_mpred(prologPTTP(inRegion(tObj,tRegion))).
%prologPTTP(localityOfObject(tObj,tSpatialthing)).

%:- add_storage_stub(prologPTTP,inRegion/2).
%:- add_storage_stub(prologPTTP,mudTestAgentWearing/2).

% inRegion(Agent,Region):- nonvar(Agent),!, loop_check(( (is_asserted(mudAtLoc(Agent,Where));localityOfObject(Agent,Where)), locationToRegion(Where,Region)),fail).
%inRegion(Agent,Region):- nonvar(Region),!, loop_check(( (is_asserted(atloc(Agent,Where));localityOfObject(Agent,Where)), locationToRegion(Where,Region)),fail).

/*
:- must(show_call(get_mpred_storage_provider(assert(_),inRegion,O))).
:- must(show_call(get_mpred_storage_provider(assert(_),inRegion/2,O))).
:- must(show_call(get_mpred_storage_provider(assert(_),inRegion(_,_),O))).
*/

inRegion(Obj,Where):- localityOfObject(Obj,Where), tRegion(Where).

% :-snark_tell(localityOfObject(A,B) &  localityOfObject(B,C) => localityOfObject(A,C)).


%localityOfObject(fo_T__T_T_T_TTTT_________TT__To,fo_T__T_T_T_TTTT_________TT__To_R).

localityOfObject(Inner,Container):-mudInsideOf(Inner,Container).
localityOfObject(Above,HasSurface):-mudLocOnSurface(Above,HasSurface).
localityOfObject(Clothes,Agent):-mudSubPart(Agent,Clothes).
localityOfObject(Inner,Outer):-use_pttp,localityOfObject(Inner,Container),localityOfObject(Container,Outer).
localityOfObject(Obj,Region):-loop_check(inRegion(Obj,Region),fail).


mudSubPart(Outer,Inner):-is_asserted(mudInsideOf(Inner,Outer)).
mudSubPart(Agent,Clothes):-wearsClothing(Agent,Clothes).

mudSubclass(tPlayer,tHominid).
mudSubclass(tHumanBody,tBodyPart).

predInnerArgIsa(mudSubPart(tBodyPart,tBodyPart)).

predRelationAllExists(mudSubPart,tHominid,tHumanBody).
predRelationAllExists(mudSubPart,tHumanBody,tBodyPart).
predRelationAllExists(mudSubPart,tHumanBody,isEach(tHumanHead,tHumanNeck,tHumanUpperTorso,tHumanLowerTorso,tHumanPelvis,tHumanArms,tHumanLegs)).
predRelationAllExists(mudSubPart,tHumanHead,isEach(tHumanFace,tHumanHair)).

predPredicateToFunction(Pred,SubjT,ObjT,FullNameFnO):- 
  is_asserted(predPredicateToFunction(Pred,SubjT,ObjT,FullNameFn)) *-> FullNameFnO=FullNameFn ; 
  (i_name('i',ObjT,Obj),i_name(Obj,Pred,ObjPred),i_name('Of',SubjT,OfSubj),concat_atom([ObjPred,OfSubj,'Fn'],FullNameFn)),simplifyFullName(FullNameFn,FullNameFnO).

simplifyFullName(FullNameFn,FullNameFn).

mudSubPart(Subj,Obj):- thlocal:infThirdOrder, find_instance_of(mudSubPart,Subj,Obj).

find_instance_of(Pred,Subj,Obj):- predRelationAllExists(Pred,SubjT,ObjT), mudIsa(Subj,SubjT), 
 (is_asserted(dbase_t(Pred,Subj,Obj),mudIsa(Obj,ObjT)) *-> true ; (predPredicateToFunction(Pred,SubjT,ObjT,PredFn), Obj =.. [PredFn,Subj])).

% mudSubPart(face,isEach(eyes,nose,mouth)).
% mudSubPart([upper_torso,arms,left_arm,left_hand,left_digits]).
% mudSubPart([upper_torso,arms,right_arm,right_hand,right_digits]).
% mudSubPart([pelvis,legs,left_leg,left_foot,left_toes]).
% mudSubPart([pelvis,legs,right_leg,right_foot,right_toes]).



put_in_world(isSelf):-!.
put_in_world(Agent):-loop_check(put_in_world_lc(Agent),true),!.

put_in_world_lc(Obj):-isa_asserted(Obj,tRegion),!.
put_in_world_lc(Obj):-is_asserted(mudAtLoc(Obj,_)),!.
put_in_world_lc(Obj):-localityOfObject(Obj,What),not(tRegion(What)),!,ensure_in_world(What),!.
put_in_world_lc(Obj):-with_fallbacksg(with_fallbacks(put_in_world_lc_gen(Obj))),!.

put_in_world_lc_gen(Obj):-choose_for(mudFacing,Obj,_),!,must_det((choose_for(mudAtLoc,Obj,LOC),nonvar(LOC))).


ensure_in_world(What):-must_det(put_in_world(What)).


:- dynamic_multifile_exported user:decl_database_hook/2.
:- dynamic_multifile_exported deduce_facts/2.
:- dynamic_multifile_exported create_random_fact/1.
:- dynamic_multifile_exported hooked_random_instance/3.
:- dynamic_multifile_exported fact_always_true/1.
:- dynamic_multifile_exported fact_maybe_deduced/1.
:- dynamic_multifile_exported fact_is_false/2.

:-decl_mpred_hybrid(mudInsideOf(tObj,tObj)).

% facts that cant be true

%fact_is_false(mudAtLoc(Obj,_LOC),mudInsideOf(Obj,What)) :- nonvar(Obj),is_asserted(mudInsideOf(Obj,What)),not(mudIsa(What,tRegion)).
%fact_is_false(mudAtLoc(Obj,LOC),mudInsideOf(Obj,What)) :- nonvar(Obj),(mudInsideOf(Obj,What)),not(mudAtLoc(What,LOC)).
%fact_is_false(localityOfObject(Obj,_LOC),mudInsideOf(Obj,What)) :- nonvar(Obj),(mudInsideOf(Obj,What)),!.

% facts that must be true 
%  suggest a deducable fact that is always defiantely true but not maybe asserted
fact_always_true(localityOfObject(apathFn(Region,Dir),Region)):-is_asserted(pathBetween(Region,Dir,_)).
fact_always_true(localityOfObject(Obj,Region)):- is_asserted(mudAtLoc(Obj,LOC)),locationToRegion(LOC,Region),!.

%  suggest a deducable fact that is probably true but not already asserted
fact_maybe_deduced(localityOfObject(Obj,Region)):- is_asserted(mudAtLoc(Obj,LOC)),locationToRegion(LOC,Region),!.
fact_maybe_deduced(localityOfObject(apathFn(Region,Dir),Region)):-is_asserted(pathBetween(Region,Dir,_)).

%  suggest a random fact that is probably is not already true
create_random_fact(mudAtLoc(Obj,LOC)) :- nonvar(Obj),asserted_or_deduced(localityOfObject(Obj,Region)),!,((in_grid(Region,LOC),unoccupied(Obj,LOC),is_fact_consistent(mudAtLoc(Obj,LOC)))).
create_random_fact(localityOfObject(Obj,Region)) :- nonvar(Obj),not(is_asserted(localityOfObject(Obj,_))),asserted_or_deduced(localityOfObject(Obj,Region)).

%  suggest random values
hooked_random_instance(vtDirection,Dir,Test) :- my_random_member(Dir,[vNorth,vSouth,vEast,vWest,vNE,vNW,vSE,vSW]),Test,!.
hooked_random_instance(ftInt,3,Test):-call(Test),dmsg(random_instance(ftInt,3,Test)),dtrace,!,fail.

%  give required forward deductions
deduce_facts(mudAtLoc(Obj,LOC),localityOfObject(Obj,Region)):- nonvar(LOC),locationToRegion(LOC,Region).
deduce_facts(localityOfObject(Obj,Region),mudAtLoc(Obj,LOC)):- tRegion(Region),not(is_asserted(mudAtLoc(Obj,_))), nonvar(Obj),
  show_call(put_in_world(Obj)),
  must_det(mudAtLoc(Obj,LOC)).


% random_region(LOC):- findall(O,isa(O,region),LOCS),my_random_member(LOC,LOCS).


random_xyzFn(LOC):-
   must_det(random_instance(tRegion,Region,true)),
   in_grid_rnd(Region,LOC),!.

random_xyzFn(xyzFn('Area1000',1,1,1)):-  trace_or_throw(game_not_loaded).

unoccupied(_,Loc):- not(is_asserted(mudAtLoc(_,Loc))),!.
unoccupied(_,_):-!.
unoccupied(Obj,Loc):- loop_check(unoccupied_lc(Obj,Loc),not(is_asserted(mudAtLoc(_,Loc)))),!.

unoccupied_lc(Obj,Loc):- is_occupied(Loc,What),!,What=Obj.
unoccupied_lc(_,_).

is_occupied(Loc,What):- is_asserted(mudAtLoc(What,Loc)),!.
is_occupied(Loc,What):- locationToRegion(Loc,Region),localityOfObject(What,Region),ensure_in_world(What),mudAtLoc(What,Loc),!.

% Used all over the place
% Transforms location based on cardinal direction given

calc_xyz(Region1,Dir,force(X1,Y1,Z1),X2,Y2,Z2):-
   to_3d(Region1,xyzFn(_,X,Y,Z)),
   get_dir_offset(Dir,1,OX,OY,OZ),
   X2 is X+ (OX*X1),Y2 is Y+OY*Y1,Z2 is Z+OZ*Z1.

move_dir_target(RegionXYZ,Dir,XXYY):-
   move_dir_target(RegionXYZ,Dir,1,XXYY).


move_dir_target(RegionXYZ,DirS,Force,XXYY):-
   any_to_atom(DirS,Dir),
   once(((calc_xyz(RegionXYZ,Dir,force(Force,Force,Force),X,Y,Z)),
   (locationToRegion(RegionXYZ,Region1)),
   (round_loc_target(Region1,X,Y,Z,Region2,X2,Y2,Z2)),
   XXYY = xyzFn(Region2,X2,Y2,Z2),
   sanity(ground(XXYY)))),
   check_ahead_for_ground(XXYY),!.

move_dir_target(RegionXYZ,Dir,_Force,XXYY):-
   any_to_string(Dir,DirS),
   locationToRegion(RegionXYZ,Region1),
   pathBetween_call(Region1,DirS,Region2),
   in_grid_rnd(Region2,XXYY),
   XXYY = xyzFn(Region2,_X2,_Y2,_Z2),
   sanity(ground(XXYY)),
   check_ahead_for_ground(XXYY),!.


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
   calc_xyz(xyzFn(Region2,X,Y,Z),Dir,force(-X1,-Y1,-Z1),X2,Y2,Z2),!.

round_loc_dir(Region1,X,Y,Z,_Dir,Region2,X2,Y2,Z2):-Region2=Region1,X2=X,Y2=Y,Z2=Z.

compute_dir(Region1,X,Y,Z,Dir):-
  grid_size(Region1,MaxX,MaxY,MaxZ),
   ((X<1 -> EW=vWest ; X > MaxX -> EW=vEast ; EW= ''),
   (Y<1 -> NS=vNorth ; Y > MaxY -> NS=vSouth ; NS= ''),
   (Z<1 -> UD=vDown ; Z > MaxZ -> UD=vUp ; UD= '')),
   atomic_list_concat_catch([NS,EW,UD],'',Dir),!.


get_dir_offset(Dir,F,OX,OY,OZ):-
  dir_offset(Dir,F,OX,OY,OZ),!.
get_dir_offset(Dir,F,OX,OY,OZ):- any_to_atom(Dir,DirA),
  dir_offset(DirA,F,OX,OY,OZ),!.
get_dir_offset(Dir,F,OX,OY,OZ):- any_to_string(Dir,DirS),
  dir_offset(DirS,F,OX,OY,OZ),!.



p2c_dir2('s','vSouth').
p2c_dir2('w','vWest').
p2c_dir2('u','vUp').
p2c_dir2('d','vDown').
p2c_dir2('e','vEast').
p2c_dir2('n','vNorth').

:-dynamic_multifile_exported(is_any_dir/1).
is_any_dir(Dir):-var(Dir),!,fail.
is_any_dir(Dir):-any_to_dir(Dir,_).
:-dynamic_multifile_exported(any_to_dir/2).

any_to_dir(D,D):-var(D),!.
any_to_dir(S,D):-string(S),string_to_atom(S,A),any_to_dir(A,D),!.
any_to_dir(D,D):-dir_offset(D,_,_,_,_),!.
any_to_dir(A,D):-p2c_dir2(D,A),!.
any_to_dir(D,O):-atom(D),sub_atom(D, 0, 1, _, S),toLowercase(S,L),p2c_dir2(L,O),!.
any_to_dir(D,D):-pathBetween(_,D,_),!.

:-dynamic_multifile_exported(dir_offset/5).

% :-decl_mpred_hybrid(dir_offset(term,int,int,int,int)).


dir_offset(vUp,F,0,0,F).
dir_offset(vDown,F,0,0,-F).
dir_offset(vNorth,F,0,-F,0).
dir_offset(vSouth,F,0,F,0).
dir_offset(vEast,F,F,0,0).
dir_offset(vWest,F,-F,0,0).
dir_offset(vNE,F,F,-F,0).
dir_offset(vSW,F,-F,F,0).
dir_offset(vSE,F,F,F,0).
dir_offset(vNW,F,-F,-F,0).
dir_offset(vHere,_,0,0,0).

% MergedNess -1,0,1 = contacting_at,inside,outside_near_on
with_offset(detatched,F,X,Y,Z):-dir_offset(vHere,F,X,Y,Z).
with_offset(absolute_with,F,X,Y,Z):-dir_offset(vUp,F,X,Y,Z).
with_offset(relative_from,F,X,Y,Z):-dir_offset(vDown,F,X,Y,Z).
with_offset(surrounding,F,X,Y,Z):-dir_offset(vNorth,F,X,Y,Z).
with_offset(mudInsideOf,F,X,Y,Z):-dir_offset(vSouth,F,X,Y,Z).
with_offset(on,F,X,Y,Z):-dir_offset(vEast,F,X,Y,Z).
with_offset(tPartofObj,F,X,Y,Z):-dir_offset(vWest,F,X,Y,Z).

facing_offset(at,F,X,Y,Z):-dir_offset(vHere,F,X,Y,Z).
facing_offset(above,F,X,Y,Z):-dir_offset(vUp,F,X,Y,Z).
facing_offset(below,F,X,Y,Z):-dir_offset(vDown,F,X,Y,Z).
facing_offset(left,F,X,Y,Z):-dir_offset(vWest,F,X,Y,Z).
facing_offset(right,F,X,Y,Z):-dir_offset(vEast,F,X,Y,Z).
facing_offset(behind,F,X,Y,Z):-dir_offset(vSouth,F,X,Y,Z).
facing_offset(front,F,X,Y,Z):-dir_offset(vNorth,F,X,Y,Z).



user:decl_database_hook(retract(_),mudAtLoc(Agent,_)):-padd(Agent,mudNeedsLook(vTrue)).

% dir_mult(X,Y,Z,X1,Y1,Z1,X2,Y2,Z2):- X2 is X * X1,Y2 is Y * Y1,Z2 is Z * Z1.


% Used in move.pl,push.pl and climb.pl
% Move agent (usually). Used to relocate agent'vSouth location.
in_world_move(LOC,Agent,DirS) :-
        string_to_atom(DirS,Dir),
        ignore(is_asserted(mudAtLoc(Agent,LOC))),
        must_det((with_assertions(thlocal:infAssertedOnly(mudAtLoc),in_world_move0(LOC,Agent,Dir)),       
         is_asserted(mudAtLoc(Agent,LOC2)),
         LOC2 \== LOC)),!.

can_world_move(LOC,_Agent,Dir) :- check_behind_for_ground(LOC),move_dir_target(LOC,Dir,_).

in_world_move0(LOC,Agent,Dir) :-
      any_to_dir(Dir,DirS),
        padd(Agent,mudFacing(DirS)),  
        check_behind_for_ground(LOC),
	move_dir_target(LOC,Dir,XXYY),!,
   must_det_l([
        dmsg(move_dir_target(LOC,DirS,XXYY)),
        locationToRegion(LOC,Region1),
        locationToRegion(XXYY,Region2),
              
        ((expire_dont_add, add(mudAtLoc(Agent,XXYY)),
        sanity((is_asserted(mudAtLoc(Agent,LOC2)),LOC2 \== LOC)))),         
   ifThen(( Region1\==Region2) ,raise_location_event(LOC,actNotice(reciever,actLeave(Agent,Region1,to(Dir))))),
        reverse_dir(Dir,Rev),
   ifThen(( Region1\==Region2) ,raise_location_event(XXYY,actNotice(reciever,actEnter(Agent,Region2,from(Rev))))),!,
	check_for_fall(LOC,XXYY,Agent)]).

check_behind_for_ground(LOC):-nonvar(LOC).
check_ahead_for_ground(XXYY):-nonvar(XXYY),
   to_3d(XXYY,xyzFn(L1,X,Y,Z)),
   grid_size(L1,MX,MY,MZ),
   inside_grid(L1,X,Y,Z,MX,MY,MZ).

inside_grid(_L1,X,Y,Z,MX,MY,MZ):-is_between(1,MX,X),is_between(1,MY,Y),is_between(1,MZ,Z).

is_between(L,H,V):- H >= V,L =< V.

% Used for every move
% Does the agent take a header off a high object?
check_for_fall(LOC,XXYY,Agent) :-
	mudAtLoc(HighObj,LOC),
	props(HighObj,mudHeight(Hh)),
        % if nothing is there pretend it is 1
	(not(mudAtLoc(_,XXYY)) -> Hl = 1; mudAtLoc(LowObj,XXYY)),
	props(LowObj,mudHeight(Hl)),
	Hd is Hh - Hl,
	Hd > 1,
	call_update_stats(Agent,fall).
check_for_fall(_,_,_).


% Reverses the direction returned by number_to_direction
% Used for fleeing
reverse_dir(W,R):-string(W),atom_string(A,W),!,reverse_dir0(A,RA),atom_string(RA,R),!.
reverse_dir(A,R):-reverse_dir0(A,R),!.
reverse_dir(Was,reverseOf(Was)):-nonvar(Was).

reverse_dir0(vDown,vUp).
reverse_dir0(vUp,vDown).
reverse_dir0(vNorth,vSouth).
reverse_dir0(vSouth,vNorth).
reverse_dir0(vEast,vWest).
reverse_dir0(vWest,vEast).
reverse_dir0(vNW,vSE).
reverse_dir0(vNE,vSW).
reverse_dir0(vSW,vNE).
reverse_dir0(vSE,vNW).


% Yet another hash table to covert numbers into aDirectionsFn (or the reverse).
num_near_reverse(1,vNW,vHere).
num_near_reverse(2,vNorth,vHere).
num_near_reverse(3,vNE,vHere).
num_near_reverse(4,vWest,vHere).
num_near_reverse(6,vEast,vHere).
num_near_reverse(7,vSW,vHere).
num_near_reverse(8,vSouth,vHere).
num_near_reverse(9,vSE,vHere).

num_near_reverse(0,vDown,vHere).
num_near_reverse(5,vUp,vHere).

% Translates numbers returned from scan_lists_aux/3 (the number of the location)
% into thier relative aDirectionsFn.
number_to_dir(1,vNW,vNW).
number_to_dir(2,vNorth,vNW).
number_to_dir(3,vNorth,vNorth).
number_to_dir(4,vNorth,vNE).
number_to_dir(5,vNE,vNE).
number_to_dir(6,vWest,vNW).
number_to_dir(7,vNW,vHere).
number_to_dir(8,vNorth,vHere).
number_to_dir(9,vNE,vHere).
number_to_dir(10,vEast,vNE).
number_to_dir(11,vWest,vWest).
number_to_dir(12,vWest,vHere).
number_to_dir(14,vEast,vHere).
number_to_dir(15,vEast,vEast).
number_to_dir(16,vWest,vSW).
number_to_dir(17,vSW,vHere).
number_to_dir(18,vSouth,vHere).
number_to_dir(19,vSE,vHere).
number_to_dir(20,vEast,vSE).
number_to_dir(21,vSW,vSW).
number_to_dir(22,vSouth,vSW).
number_to_dir(23,vSouth,vSouth).
number_to_dir(24,vSouth,vSE).
number_to_dir(25,vSE,vSE).


% Scans through list of perceptions (as returned by look_percepts(Agent,L) or look_all(NearAgt,_,_,_,L,_))
% for an object,returns the direction in which the object lies.
list_object_dir_sensed(_,List,Type,Dir) :-
	!,
	scan_lists_aux(List,Type,1,N),
	number_to_dir(N,Dir,_).


list_object_dir_near(List,Type,Dir) :-
	!,
	scan_lists_aux(List,Type,1,N),
	num_near_reverse(N,Dir,_).

scan_lists_aux([Loc|_],Type,N,N) :-
	member(Obj,Loc),
        mudIsa(Obj,Type),
	!.
scan_lists_aux([_|Rest],Type,M,N) :-
	Mtemp is M + 1,
	!,
	scan_lists_aux(Rest,Type,Mtemp,N).


doorLocation(_Room,3,0,_Z,vNorth).
doorLocation(_Room,2,0,_Z,vNorth).
doorLocation(_Room,4,0,_Z,vNorth).
doorLocation(_Room,3,6,_Z,vSouth).
doorLocation(_Room,2,6,_Z,vSouth).
doorLocation(_Room,4,6,_Z,vSouth).
doorLocation(_Room,0,2,_Z,vWest).
doorLocation(_Room,0,3,_Z,vWest).
doorLocation(_Room,0,4,_Z,vWest).
doorLocation(_Room,6,2,_Z,vEast).
doorLocation(_Room,6,3,_Z,vEast).
doorLocation(_Room,6,4,_Z,vEast).
doorLocation(_Room,6,0,_Z,vNE).
doorLocation(_Room,6,6,_Z,vSE).
doorLocation(_Room,0,0,_Z,vNW).
doorLocation(_Room,0,6,_Z,vSW).
doorLocation(_Room,_X,_Y,_Z,_Dir):-!,fail.
