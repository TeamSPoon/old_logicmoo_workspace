/** <module> 
% This file loads the world (world.pl), the map of the world, 
% the agents and their definitions.
% This file is used as a configuation file and a startup script.
%
% July 10,1996
% John Eikenberry
%
% Project LogicMoo: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
:- set_prolog_flag(verbose_load,true).

% logicmoo utils shared with other systems

:- ensure_loaded(logicmoo(logicmoo_util/logicmoo_util_bugger)).
:- ensure_loaded(logicmoo(logicmoo_util/logicmoo_util_library)).
:- ensure_loaded(logicmoo(logicmoo_util/logicmoo_util_ctx_frame)).
:- ensure_loaded(logicmoo(logicmoo_util/logicmoo_util_strings)).
:- ensure_loaded(logicmoo(logicmoo_util/logicmoo_util_terms)).
:- ensure_loaded(logicmoo(logicmoo_util/logicmoo_util_dcg)).
:- set_prolog_flag(verbose_load,true).
% these do not get defined!?
% :-dynamic user_db:assert_user/2, user_db:grant_openid_server/2, user_db:retractall_grant_openid_server/2, user_db:retractall_user/2, user_db:assert_grant_openid_server/2.

% These three are for use with Quintus
%:- prolog_flag(unknown,error,fail). % Not sure if this is needed for Quintus
%:- ensure_loaded(library(random)).
%:- ensure_loaded(library(date)).

% This one is for use with SWI
:- ensure_loaded(library(quintus)).

:- ensure_loaded(logicmoo(vworld/moo)).
:- ensure_loaded(logicmoo(vworld/dbase)).
:- ensure_moo_loaded(logicmoo(vworld/world)).

:- include(moo_loadall).


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


% standard header used in all files that all modules are loaded (therefore useful for when(?) the day comes that modules *can*only*see their explicitly imported modules)
% :- ensure_loaded(logicmoo(vworld/moo_header)).

% These contain the definition of the object types.
:- ensure_moo_loaded(logicmoo('objs/objs_misc_monster.pl')). 

% Load the map file (*.map.pl) appropriate for the world being used.
:- ensure_moo_loaded(logicmoo('rooms/vacuum.map.pl')).

% puts world into running state
% :- must(old_setup).

% [Optionaly] Start the telnet server

:-at_start(toploop_telnet:start_mud_telnet(4000)).


