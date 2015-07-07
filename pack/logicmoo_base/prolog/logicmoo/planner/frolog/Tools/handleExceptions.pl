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

:- module(handleExceptions,[handle/2,handle/3]).  

% Parsing errors
handle(parser,'no parse') :-
	format("\nSorry? I couldn't parse that."). 
handle(parser,'unknown word',Word) :-
	format("\nI don't know the word ~w.",[Word]). 

% Resolution errors
handle(resolution,'no_ref_for'(Concept)) :-
	format("\nYou can't see any ~w.",[Concept]).
handle(resolution,'no_ref_for_pronoun'(Concept)) :-
	format("\nI don't understand what you mean by `it'.").
handle(resolution,'ref_ambiguous'(Concept)) :-
	format("\nThere is more than one ~w.",[Concept]).

% Action errors
handle(actions,'Missing entry in action-database!',Action) :-
	format("\nSorry! I don't know how to do that.").
handle(actions,'ambiguousCommand'(Executable)) :-
	format("\nThere is more than one way to do that: ~w",[Executable]).
handle(actions,'precondFailure'(Failures)) :-
	format("\nYou can't do this! ~w",[Failures]).
% TODO: Verbalize the failed precondition. Study the content
% determination module. 
% This is how this error is handled in Froz:
/*
      [] error(module:actions type:precondFailure failures:F) then
	 CD RefGen Real
      in
	 {Tools.inspectIfDebug e(Exc) Win}
	 {Win addLine("\nYou can't do this!")} 
	 {W setProgress(content)}
	 try
	    CD = {Content.determineContent failure(F) {Access PlayerABox} {Access WorldABox}}
	    {Tools.inspectIfDebug c(CD) Win}
	    {W setProgress(refgen)}
	    RefGen = {GenRefExpr.generateReferringExpressions CD {Access PlayerABox} {Access WorldABox}}
	    {Tools.inspectIfDebug refg(RefGen) Win}
	    {W setProgress(realization)}
	    if Args.debug then {System.showError 'now: realization'} end
	    Real = {Realize.realize RefGen {Access PlayerABox} W}
	    {List.forAll Real
	     proc{$ S}
		{W addLine({Tools.prepareOutput S.1})}
	     end}
	 end
*/

% Racer error 
handle(racer,'racer error',RacerAnswer) :-
	format("\nA RACER error occurred: ~w",[RacerAnswer]).

