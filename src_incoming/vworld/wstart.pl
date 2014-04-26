% wstart.pl
% July 10,1996
% John Eikenberry
%
% Dec 13, 2035
% Douglas Miles
%
/** <module> 
% This file loads the world (world.pl), the map of the world, 
% the agents and their definitions.
% This file is used as a configuation file and a startup script.
*/

% standard header used in all files that all modules are loaded (therefore useful for when(?) the day comes that modules *can*only*see their explicitly imported modules)
:- include(logicmoo('vworld/vworld_header.pl')).

% These contain the definition of the object types.
:- ensure_loaded(logicmoo('objs/vacuum.objects.pl')). 

% Load the map file (*.map.pl) appropriate for the world being used.
:- ensure_loaded(logicmoo('rooms/vacuum.map.pl')).

% puts world into running state
:- must(old_setup).

% standard footer to clean up any header defined states
:- include(logicmoo('vworld/vworld_footer.pl')).


% Define the agents traits, both for your agent and the world inhabitants. 
% agent name and stats ([] = defaults).
% Agents with numbered names (eg. prey(1)) are able to be used multiple times.
% Just copy their line and increment the number to make more of them.
/*
:-create_agent(predator(1),[str(4),stm(2),height(2),spd(3)]).
:-create_agent(prey(1),[str(0),stm(-8),spd(1),height(1)]).
:-create_agent(prey(2),[str(0),stm(-8),spd(1),height(1)]).
:-create_agent(prey(3),[str(0),stm(-8),spd(1),height(1)]).
%:-create_agent(prey(4),[str(0),stm(-8),spd(1),height(1)]).
:-create_agent(monster(1),[str(6),stm(2),height(2),spd(1)]).
:-create_agent(monster(2),[str(6),stm(2),height(2),spd(1)]).
:-create_agent(explorer(1),[str(2),spd(4),stm(3),height(2)]).
:-create_agent(vacuum(1),[]).
:-create_agent(explorer(2),[]).
*/

% [Optionaly] Start the telent server
:-at_start(telnet_server(4040, [allow(_ALL),call_pred(login_and_run)])).






