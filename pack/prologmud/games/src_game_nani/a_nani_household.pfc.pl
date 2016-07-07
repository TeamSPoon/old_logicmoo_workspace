/** <module> a_nani_household
% This file contains the definitions for the room in a household
% To create a new world, simply change the room definitions as
% described below (or in manual)
%

use this file with...

:- declare_load_dbase('../games/src_game_nani/a_nani_household.pfc.pl').

*/

:- style_check(-singleton).
:- style_check(-discontiguous).
% :- style_check(-atom).

:- op(600,fx,onSpawn).

:- file_begin(pfc).

% ==================================================
% Rooms
% ==================================================

/* technically the following are not needed due the bordersOn/2s below */
:-onSpawn tRegion(tKitchen).
:-onSpawn tRegion(tHall).
:-onSpawn tRegion(tCellar).
:-onSpawn tRegion(tOfficeRoom).
:-onSpawn tRegion(tLivingRoom).
:-onSpawn tRegion(tDiningRoom).
:-onSpawn tRegion(tBedRoom).
:-onSpawn tRegion(tBathRoom).
:-onSpawn tRegion(tClosetRoom).
:-onSpawn tRegion(tBackYard).


% ==================================================
% Doors
% ==================================================

:-decl_mpred_hybrid(bordersOn(tRegion,tRegion),symmetric).

:-onSpawn bordersOn(tLivingRoom,tOfficeRoom).
:-onSpawn bordersOn(tHall,tDiningRoom).
:-onSpawn bordersOn(tHall,tBedRoom).
:-onSpawn bordersOn(tHall,tLivingRoom).
:-onSpawn bordersOn(tHall,tBathRoom).
:-onSpawn bordersOn(tKitchen, tCellar).
:-onSpawn bordersOn(tDiningRoom, tKitchen).
:-onSpawn bordersOn(tBedRoom, tClosetRoom).
:-onSpawn bordersOn(tKitchen, tBackYard).
