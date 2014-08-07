/* <module>
% Imperitive Sentence Parser (using DCG)
%
% Project Logicmoo: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
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

:- module(parser_chat80, [chat80/0]).

:- include(logicmoo('vworld/moo_header.pl')).

:- register_module_type(utility).

:- asserta(thlocal:into_form_code).   

:- ensure_loaded(logicmoo(parsing/chat80/xgproc)).	% XG generator

:- load_plus_xg_file(logicmoo(parsing/chat80/'clone.xg')).
:- load_plus_xg_file(logicmoo(parsing/chat80/'lex.xg')).
:- compile_xg_clauses.
% :- list('newg.pl').
:- ensure_loaded(logicmoo(parsing/chat80/xgrun)).	% XG runtimes
% :- ensure_loaded(logicmoo(parsing/chat80/newg)).		% clone + lex


:- ensure_loaded(logicmoo(parsing/chat80/clotab)).	% attachment tables
:- ensure_loaded(logicmoo(parsing/chat80/newdict)).	% syntactic dictionary
:- ensure_loaded(logicmoo(parsing/chat80/slots)).	% fits arguments into predicates
:- ensure_loaded(logicmoo(parsing/chat80/scopes)).	% quantification and scoping
:- ensure_loaded(logicmoo(parsing/chat80/templa)).	% semantic dictionary
:- ensure_loaded(logicmoo(parsing/chat80/qplan)).	% query planning
:- ensure_loaded(logicmoo(parsing/chat80/talkr)).	% query evaluation
:- ensure_loaded(logicmoo(parsing/chat80/ndtabl)).	% relation info.
:- ensure_loaded(logicmoo(parsing/chat80/readin)).	% sentence input
:- ensure_loaded(logicmoo(parsing/chat80/ptree)).	% print trees
:- ensure_loaded(logicmoo(parsing/chat80/aggreg)).	% aggregation operators

:- ensure_loaded(logicmoo(parsing/chat80/world0)).     	% data base
:- ensure_loaded(logicmoo(parsing/chat80/rivers)).
:- ensure_loaded(logicmoo(parsing/chat80/cities)).
:- ensure_loaded(logicmoo(parsing/chat80/countries)).
:- ensure_loaded(logicmoo(parsing/chat80/contain)).
:- ensure_loaded(logicmoo(parsing/chat80/borders)).

% testing
:- ensure_loaded(logicmoo(parsing/chat80/newtop)).	% top level

:- time(hi(logicmoo(parsing/chat80/demo))).

chat80 :-
  told,
   repeat,
   prompt_read('CHAT80 Question: ',Atom),
   atomSplit(Atom,P),   
      control(report,P).

:- retract(thlocal:into_form_code).

:- include(logicmoo('vworld/moo_footer.pl')).


