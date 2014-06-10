% move.pl
% May 18, 1996
% John Eikenberry
%
% Dec 13, 2035
% Douglas Miles
%
/** <module> 
% This file defines the predicates for the agent to move
%
*/

:- module(move, []).

:- include(logicmoo(vworld/moo_header)).

:- moo:register_module_type(command).

:- moo:begin_transform_moo_preds.

moo:agent_text_command(Agent,[DirSS],Agent,move(Dir)):- catch(((string_to_atom(DirSS,Dir),dyn:specifier_text(Dir,dir))),_,fail),!.

moo:agent_text_command(Agent,[DirSS],Agent,move(DirS)):- 
 catch(((string_to_atom(DirSS,DirS),dyn:specifier_text(Dir,dir),
       catch((atom_concat(Dir,N,DirS),(atom_number(N,_))),_,fail))),_,fail).

moo:agent_call_command(Agnt,Cmd):- functor(Cmd,move,_),!,
   must(move_command(Agnt,Cmd)).

moo:action_info(move(dir)).

% dir###
move_command(Agent,move(DirSS)) :- catch((string_to_atom(DirSS,DirS),
	    atom_concat(Dir,N,DirS),atom_number(N,Dist)),_,fail),!,
            move_command(Agent,Dir,Dist).
% dir
move_command(Agent,move(Dir)) :-
	    get_move_dist(Agent,Dist),
            move_command(Agent,Dir,Dist).

get_move_dist(Agent,Dist):-req(movedist(Agent,Dist)),!.
get_move_dist(_Gent,1).

% Move thy agent
move_command(Agent,DirS,DistS) :- 
   string_to_atom(DirS,Dir),
   any_to_number(DistS,Dist),
   catch(doall((between(1,Dist,_),move_command_1(Agent,Dir))),giveup(_),true).



% cant get anywhere since the map fails it
move_command_1(Agent,Dir) :-
	atloc(Agent,LOC),
        not(move_dir_target(LOC,Dir,_)),!,
		add(failure(Agent,move)),
      throw(giveup(nopath(Agent,move))).

% Run into something big, Ouch...
% damage and charge... plus it doesn't get anywhere
move_command_1(Agent,Dir) :-
	atloc(Agent,LOC),
        move_dir_target(LOC,Dir,XXYY),
        is_3d(XXYY),
         atloc(Obj,LOC),        
         prop_or(Obj,height,ObjHt,1),
         atloc(Obj2,XXYY),
         prop_or(Obj2,height,ObjHt2,1),
         ObjHt2 > ObjHt,
         ObjHt2 > 1,
	!,
	dyn:update_stats(Agent,collide),
	dyn:update_charge(Agent,move),
        raise_location_event(XXYY,collide(Agent,Obj2)),
   throw(giveup(collide(Agent,Obj2))).


% Another Agent is in the way
move_command_1(Agent,Dir):- 
	atloc(Agent,LOC),
	move_dir_target(LOC,Dir,XXYY),
	is_3d(XXYY),
        atloc(Agent2,XXYY),
	mud_isa(Agent2,agent),
	dyn:update_stats(Agent,collide),
	dyn:update_charge(Agent,move),
        raise_location_event(XXYY,collide(Agent,Agent2)),
   throw(giveup(collide(Agent,Agent2))).


%Move successfully
move_command_1(Agent,Dir) :-
	in_world_move(_,Agent,Dir),
	dyn:update_charge(Agent,move).

%Record keeping

dyn:update_charge(Agent,move) :- padd(Agent,charge,-4).

dyn:update_stats(Agent,collide) :- padd(Agent,damage,-5),add(failure(Agent,collide)).

dyn:update_stats(Agent,fall) :- padd(Agent,damage,-10).

% cheating but to test

moo:action_info(go(dir)).
moo:agent_call_command(Agent,go(Dir)) :-
	atloc(Agent,LOC),
        in_world_move(LOC,Agent,Dir),
	dyn:update_charge(Agent,move).


:- include(logicmoo(vworld/moo_footer)).


