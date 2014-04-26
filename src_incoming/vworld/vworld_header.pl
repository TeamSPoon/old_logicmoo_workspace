% All modules are declared here so that this next lines dont have to be pasted int every file.
% Since this list will need at least 160 entries to cover the obj classes rooms and commands, 
% we add the modules here to not waste 160^2 lines of text and having to not 
% update 160+ files whenever a new module is used

% :-set_prolog_flag(unknown,fail).
:- set_prolog_flag(double_quotes, atom).
:- set_prolog_flag(double_quotes, string). 

%  very very first import
:- ensure_loaded(logicmoo('vworld/moo.pl')).

% :-context_module(Ctx),writeq(context_module(Ctx)),nl.

:-context_module(Ctx),asserta(loading_module_h(Ctx)).

% These three are for use with Quintus
%:- prolog_flag(unknown,error,fail). % Not sure if this is needed for Quintus
%:- ensure_loaded(library(random)).
%:- ensure_loaded(library(date)).
% This one is for use with SWI
:- ensure_loaded(library(quintus)).

% logicmoo utils shared with other systems
:- ensure_loaded(logicmoo('logicmoo_util/logicmoo_util_bugger.pl')).
:- ensure_loaded(logicmoo('logicmoo_util/logicmoo_util_library.pl')).
:- ensure_loaded(logicmoo('logicmoo_util/logicmoo_util_ctx_frame.pl')).
:- ensure_loaded(logicmoo('logicmoo_util/logicmoo_util_strings.pl')).
:- ensure_loaded(logicmoo('logicmoo_util/logicmoo_util_terms.pl')).
:- ensure_loaded(logicmoo('logicmoo_util/logicmoo_util_dcg.pl')).

% make sure these get in early
:- ensure_loaded(logicmoo('vworld/formattypes.pl')).
:- ensure_loaded(logicmoo('vworld/dbase.pl')).
:- ensure_loaded(logicmoo('vworld/kernel.pl')).

% logicmoo vworld mud server
:-ensure_loaded(logicmoo('vworld/world.pl')).
% :-ensure_loaded(logicmoo('vworld/room_grids.pl')).
:-ensure_loaded(logicmoo('vworld/lib.pl')).
:-ensure_loaded(logicmoo('vworld/actr.pl')).
:-ensure_loaded(logicmoo('vworld/parsem.pl')).
:-ensure_loaded(logicmoo('vworld/telnet_server.pl')).
:-ensure_loaded(logicmoo('vworld/npc_toploop.pl')).
:-ensure_loaded(logicmoo('vworld/game_loader.pl')).
:-ensure_loaded(logicmoo('vworld/mud_tests.pl')).

% NPC planners
:-ensure_loaded(logicmoo('mobs/monster.pl')).
:-ensure_loaded(logicmoo('mobs/predator.pl')).
:-ensure_loaded(logicmoo('mobs/explorer.pl')).
:-ensure_loaded(logicmoo('mobs/prey.pl')).
:-ensure_loaded(logicmoo('mobs/vacuum.pl')).

% Action/Commands implementation
:-ensure_loaded(logicmoo('actions/any.pl')).
:-ensure_loaded(logicmoo('actions/drink.pl')).
:-ensure_loaded(logicmoo('actions/use.pl')).
:-ensure_loaded(logicmoo('actions/attack.pl')).
:-ensure_loaded(logicmoo('actions/push.pl')).
:-ensure_loaded(logicmoo('actions/climb.pl')).
:-ensure_loaded(logicmoo('actions/eat.pl')).
:-ensure_loaded(logicmoo('actions/move.pl')).
:-ensure_loaded(logicmoo('actions/drop.pl')).
:-ensure_loaded(logicmoo('actions/sit.pl')).
:-ensure_loaded(logicmoo('actions/look.pl')).
:-ensure_loaded(logicmoo('actions/take.pl')).
:-ensure_loaded(logicmoo('actions/logon.pl')).
:-ensure_loaded(logicmoo('actions/teleport.pl')).
:-ensure_loaded(logicmoo('actions/chat.pl')).
:-ensure_loaded(logicmoo('actions/help.pl')).

