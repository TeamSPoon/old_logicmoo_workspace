/*************************************************************************

    File: frolog.pl
    Copyright (C) 2007 

    Programmer: Luciana Benotti

    This file is part of Frolog, version 0.1 (October 2007).

    Frolog is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Frolog is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Frolog; if not, write to the Free Software Foundation, Inc., 
    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*************************************************************************/

:- module(frolog,[load/1,execute/2, resolveReso/2]).  

:- use_module('KBInterfaces/RacerInterface/racer',[load_scenario/5]).
:- use_module('NLPModules/Actions/execute',[executeCommand/4]).
:- use_module('NLPModules/ContentDetermination/describe',[describe/2,describe/1]).
:- use_module('NLPModules/ReferenceResolution/resolve',[resolveResolutionMain/3]).
:- use_module('GameScenarios/FairyTaleCastle/actionDatabase').

% Don't load the scenario right away after starting racer because racer takes a while to start. 
startRacer :- shell('/users/led/benottil/Sync/Programming/Racer/RacerPro/RacerPro-1-9-2-beta/RacerPro -silent > racer_log.txt &').


load(Scenario) :- 
	retractall(racer:worldKB(_,_)), 
	retractall(racer:playerKB(_,_)),
	load_scenario(Scenario,_,TBoxName,GameAboxName,PlayerAboxName),
	format('The Name of the TBox is: ~w ~n',TBoxName), 
	format('The Name of the Game ABox is: ~w ~n',GameAboxName), 
	format('The Name of the Player ABox is: ~w ~n',PlayerAboxName).

end :- shell('killall RacerPro').

execute(Command,Executed,Failed,Decision) :- executeCommand(Command,Executed,Failed,Decision).

resolveReso(Concept, I):- resolveResolutionMain(Concept,I,DM).



	

