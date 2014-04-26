% vacuum.objects.pl
% July 7, 1996
% John Eikenberry
%
%
% Dec 13, 2035
% Douglas Miles
%
/** <module> 
% This file contains the definitions for the objects in the world
% To create a new world, simply change the object definitions as
% described below (or in manual)
%
% *******Object definitions*******
%.
% Use the fmt moo:label_type_props(label,typecode,[property1,property2,etc.]]).
% label is whats used to refer to the object in the running world
% typecode is whats used to refer to the object for initialization (see world.pl)
%
% **All objects require height() and weight() properties
% **Even locations you want empty require height(0) to be defined
% **If permanence(take,) is not defined for an object that can be picked
%   up (ie take()), it is assumed to be permanence(take,2) (ie never goes away).
%
% The properties are as follows:
%
% # height(N) where N ranges from 1-5 & 10.
% ---> 1  = very small, can be steped over.
% ---> 2  = low, blocks movement, but can be climbed onto.
% ---> 3+ = high, blocks sight and movement, can be climbed on from objects of
%           one less height. Eg. an agent on object height(2) can climb onto an object
%           with height(3).
% ---> 10 = Edge of world. Can't ever be seen over or climbed onto.
% **** Negative numbers can be used for N, this is good for pits, etc.
%
% # weight(N) where N ranges from 1-4.
% ---> 1 = light, can be picked up.
% ---> 2 = hefty, can be pushed.
% ---> 3 = heavy, normally immobile, can be pushed by high strength agents
% ---> 4 = structure, cannot be moved. 
%
% # act_energy(Action,N) where N is any positive or negative integer (not 0)
% ---> N is the amount of charge agent receives or loses 
%      from doing that Action on the object defined.
%
% # act_score(Action,N) where N is any positive or negative integer (not 0)
% ---> N is the score the agent recieves from doing Action on the
%      obect defined.
%
% # act_heal(Action,N) where N is any positive or negative integer
% --->N is the amount of damage the agent heals (or takes) from
%        doing Action on object defined.
%
% # permanence(take,N) where N ranges from 0-2.
% ---> 0 = object dissapears when taken.
% ---> 1 = object is held when taken, can be eaten/destroyed (see eat.pl)
% ---> 2 = object stays where it is but something else (charge,score)
%          is usually accumulated.
%
% # attack(N) where N is some positive or negative interger.
% ---> when held by agent-1 and agnet-1 attacks agent-2. agent-2 takes N
%      amount of damage.(negative damage or healing is possible)
%
% # defence(N) where N is some positive or negative integer.
% ---> N points of damage are subtracted (added) to an incoming attack.
%
% # spawn_rate(N) 
% ---> N% chance per turn of this object being placed in the world.
%	
*/
:- module(objects, [ max_charge/1, max_damage/1,label_type/2]).

:- include(logicmoo('vworld/vworld_header.pl')).

:-ignore(register_module_type([world_types,parser])).

label_type(Label,Type):-moo:label_type_props(Label,Type,_).

% :-register_module_type(dynamic).

max_charge(500).
max_damage(120).

% :-end_module_type(dynamic).

moo:type_default_props(Instance,Type,[named(Instance),id(Instance)|SP]):- moo:label_type_props(_,Type,SomeProps),flatten(SomeProps,SP).

% Vacuum World example objects........
moo:label_type_props(wl,wall,[height(3),weight(4)]).
moo:label_type_props(tr,tree,[height(3),weight(4)]).
moo:label_type_props(rk,rock,[height(2),weight(4)]).
moo:label_type_props(pt,pit,[height(-1),weight(4)]).
moo:label_type_props(ot,outlet,[height(1),weight(1),permanence(take,2),act(take,charge(50))]).
moo:label_type_props(nt,nut,[height(1),weight(1),permanence(take,1),act(eat,charge(40)),spawn_rate(10)]).
moo:label_type_props(lw,low_wall,[height(2),weight(4)]).
moo:label_type_props(lg,ledge,[height(2),weight(4)]).
moo:label_type_props(lb,low_box,[height(2),weight(2)]).
moo:label_type_props(hw,high_wall,[height(3),weight(4)]).
moo:label_type_props(hb,high_box,[height(3),weight(3)]).
moo:label_type_props(gd,gold,[height(1),weight(1),permanence(take,0),act(take,score(10))]).
moo:label_type_props(fd,food,[height(1),weight(1),permanence(take,1),act(eat,charge(80))]).
moo:label_type_props(el,elixer,[height(1),weight(1),permanence(take,1),act(eat,damage(-10))]).
moo:label_type_props(ed,edge,[height(10),weight(4)]).
moo:label_type_props(dt,dirt,[height(1),weight(1),permanence(take,0),act(take,score(1)),spawn_rate(5)]).
moo:label_type_props(dr,door,[height(3),weight(2)]).

%Empty Location
% You *have* to use 0 as the id of the empty location.
moo:label_type_props(--,0,[]).

% What an agent turns into upon death.
% Must be named corpse (or edit abent_to_corpse/1 in lib.pl.
moo:label_type_props(cp,corpsea,[height(1),weight(1),permanence(take,1),act(eat,charge(80)),act(take,score(10))]).
moo:label_type_props(cp,corpseb,[height(1),weight(1),permanence(take,1),act(eat,charge(120))]).
moo:label_type_props(da,corpsec,[height(2),weight(2)]).
% This is used to make the monster roaming the maze a bit tougher
% It doesn't require any of the usual traits since the monster will never 
% leave the monster's possession.
moo:label_type_props(th,nasty_knife,[act(held,attack(2))]).
moo:label_type_props(th,tough_hide,[act(wear,defence(2))]).


% Define the maximum charge and maximum damage an agent can have


:- include(logicmoo('vworld/vworld_footer.pl')).
