% ===================================================================
% File 'spawning.pl'
% Purpose: An Implementation a MUD server in SWI-Prolog
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'run.pl' 1.0.0
% Revision:  $Revision: 1.7 $
% Revised At:   $Date: 2002/07/11 21:57:28 $
% ===================================================================
%


moo:on_world_load :- retractall(spawn_objects(_)).

growth :-
	findall(([Obj,Chance]),
	    props(Obj,spawn_rate(Chance)),
	    Objects),
	assert(spawn_objects(Objects)).



% For Quintus alter 'Y is random(LLL)' to random(0,LLL,Y)
% defined pred maybe.... be sure to comment this pred out before going
% back to quitus (located below).
spread :-
	spawn_objects(Objects),
	forall(prop_memb([Object,Chance],Objects),
	spread(Object,Chance)).

spread(Object,Chance) :-        
	maybe(Chance),!,
        gensym(Object,Named),
        create_instance(Named,Object,[]),!.

spread(_,_).

