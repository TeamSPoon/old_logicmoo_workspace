% load.pl : Load Chat-80, for Quintus Prolog

/*
 _________________________________________________________________________
|	Copyright (C) 1982						  |
|									  |
|	David Warren,							  |
|		SRI International, 333 Ravenswood Ave., Menlo Park,	  |
|		California 94025, USA;					  |
|									  |
|	Fernando Pereira,						  |
|		Dept. of Architecture, University of Edinburgh,		  |
|		20 Chambers St., Edinburgh EH1 1JZ, Scotland		  |
|									  |
|	This program may be used, copied, altered or included in other	  |
|	programs only for academic purposes and provided that the	  |
|	authorship of the initial program is aknowledged.		  |
|	Use for commercial purposes without the previous written 	  |
|	agreement of the authors is forbidden.				  |
|_________________________________________________________________________|

*/

:- ensure_loaded(tlxgproc).	% XG generator


:- load_plus_xg_file(parser_chat80,'clone.xg').
:- load_plus_xg_file(parser_chat80,'lex.xg').
:- compile_xg_clauses.

% :- list('newg.pl').
:- include(xgrun).	% XG runtimes
% :- include(newg).		% clone + lex
:- include(clotab).	% attachment tables
:- include(newdict).	% syntactic dictionary
:- include(slots).	% fits arguments into predicates
:- include(scopes).	% quantification and scoping
:- include(templa).	% semantic dictionary
:- include(qplan).	% query planning
:- include(talkr).	% query evaluation
:- include(ndtabl).	% relation info.
:- include(readin).	% sentence input
:- include(ptree).	% print trees
:- include(aggreg).	% aggregation operators
:- include(world0).     	% data base
:- include(rivers).
:- include(cities).
:- include(countries).
:- include(contain).
:- include(borders).
:- include(newtop).	% top level




bad_chat80 :-
  told,
  told,
   repeat,
   prompt(_,'Question: '),
   trace,read_sent(P),
      control80(report,P),
      end(user).

