/** <module> 
% This is simple example federation for the maze world.
%
%
% monster.pl
% July 11, 1996
% John Eikenberry
%
% Dec 13, 2035
% Douglas Miles
%
% Declare the module name and the exported (public) predicates.
*/



% Declare the module name and the exported (public) predicates.
:-module(modFederation,[]).

:- include(logicmoo(vworld/moo_header)).
:- register_module_type(planning).

:-decl_type(tFederation).

vette_federation_idea(Agent,Act,Act):-var(Act),!,dmsg(vette_federation_idea(Agent,Act)).
vette_federation_idea(_,actSit,actSit):-!.
vette_federation_idea(Agent,Act,Act):-dmsg(vette_federation_idea(Agent,Act)).

mudLabelTypeProps('Px',tFederation,[]).

user:world_agent_plan(_World,Agent,ActV):-
   tAgentGeneric(Agent),
  % mudIsa(Agent,federation),
   federation_idea(Agent,Act),
   vette_federation_idea(Agent,Act,ActV).

% Possible tAgentGeneric actions.
federation_idea(Agent,actEat(Elixer)) :-
	mudHealth(Agent,Damage),
	Damage < 15,
   mudPossess(Agent,Elixer),   
   mudIsa(Elixer,tElixer).

federation_idea(Agent,actEat(tFood)) :-
	mudEnergy(Agent,Charge),
	Charge < 150,
   mudPossess(Agent,Food),
   mudIsa(Food,tFood).

federation_idea(Agent,actTake(What)) :-
	mudNearFeet(Agent,What),
	isa_any(What,[tGold,tElixer,tTreasure]).  

federation_idea(Agent,actTake(What)) :-
	mudNearFeet(Agent,What),
	isa_any(What,[tFood,tUsefull,tItem]).

federation_idea(Agent,actMove(1,Dir)) :-
	mudGetPrecepts(Agent,List),
	list_object_dir_sensed(_,List,tTreasure,Dir).

federation_idea(Agent,actMove(3,Dir)) :-
	mudGetPrecepts(Agent,List),
	list_object_dir_sensed(_,List,tMonster,OppDir),
	reverse_dir(OppDir,Dir),
	number_to_dir(N,Dir,vHere),
        nth1(N,List,What),
	What == [].

federation_idea(Agent,actMove(1,Dir)) :-
	mudGetPrecepts(Agent,List),
	list_object_dir_sensed(_,List,tUsefull,Dir).

federation_idea(Agent,actMove(1,Dir)) :-
	mudGetPrecepts(Agent,List),
	list_object_dir_sensed(_,List,tAgentGeneric,Dir).

federation_idea(Agent,actMove(5,Dir)) :-
	mudMemory(Agent,aDirectionsFn([Dir|_])),
	number_to_dir(Num,Dir,vHere),
	mudNearReach(Agent,List),
	nth1(Num,List,What),
	What == [].

federation_idea(Agent,actAttack(Dir)) :-
	mudNearReach(Agent,List),
	list_object_dir_near(List,tMonster,Dir).

federation_idea(Agent,actLook) :-
        req(mudMemory(Agent,aDirectionsFn(Old))),
	del(mudMemory(Agent,aDirectionsFn(Old))),
	random_permutation(Old,New),
	add(mudMemory(Agent,aDirectionsFn(New))).


:- include(logicmoo(vworld/moo_footer)).
