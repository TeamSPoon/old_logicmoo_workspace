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

% :- module(parser_chat80, [chat80/0,test_chat80_regressions/0]).

% :- include(logicmoo('vworld/moo_header.pl')).

% :- register_module_type(utility).

:- dynamic_multifile_exported moo:contains/2.
:- dynamic_multifile_exported moo:longitude/2.
:- dynamic_multifile_exported moo:latitude/2.
:- dynamic_multifile_exported moo:region/1.
:- dynamic_multifile_exported moo:trans/9.

% ===========================================================
% CHAT80 command
% ===========================================================
moo:type_action_info(human_player,chat80(list(term)),"Development test CHAT-80 Text for a human.  Usage: CHAT80 Cant i see the blue backpack?").

moo:agent_call_command(_Gent,chat80([])):- chat80.
moo:agent_call_command(_Gent,chat80(StringM)):- do_chat80(StringM).  


% ===========================================================
% CHAT80 REPL
% ===========================================================
chat80 :- told, repeat, prompt_read('CHAT80> ',U),  to_word_list(U,WL),do_chat80(WL).

:- asserta(thlocal:into_form_code).   

:- include(logicmoo(parsing/chat80/xgproc)).	% XG generator

:- load_plus_xg_file(logicmoo(parsing/chat80/'clone.xg')).
:- load_plus_xg_file(logicmoo(parsing/chat80/'lex.xg')).

:- compile_xg_clauses.

% :- list('newg.pl').
:- include(logicmoo(parsing/chat80/xgrun)).	% XG runtimes
% :- include(logicmoo(parsing/chat80/newg)).		% clone + lex


:- include(logicmoo(parsing/chat80/clotab)).	% attachment tables
:- include(logicmoo(parsing/chat80/newdict)).	% syntactic dictionary
:- include(logicmoo(parsing/chat80/slots)).	% fits arguments into predicates
:- include(logicmoo(parsing/chat80/scopes)).	% quantification and scoping
:- include(logicmoo(parsing/chat80/templa)).	% semantic dictionary
:- include(logicmoo(parsing/chat80/qplan)).	% query planning
:- include(logicmoo(parsing/chat80/talkr)).	% query evaluation
:- include(logicmoo(parsing/chat80/ndtabl)).	% relation info.
:- include(logicmoo(parsing/chat80/readin)).	% sentence input
:- include(logicmoo(parsing/chat80/ptree)).	% print trees
:- include(logicmoo(parsing/chat80/aggreg)).	% aggregation operators

:- include(logicmoo(parsing/chat80/world0)).     	% data base
:- include(logicmoo(parsing/chat80/rivers)).
:- include(logicmoo(parsing/chat80/cities)).
:- include(logicmoo(parsing/chat80/countries)).
:- include(logicmoo(parsing/chat80/contain)).
:- include(logicmoo(parsing/chat80/borders)).

% testing
:- include(logicmoo(parsing/chat80/newtop)).	% top level


:-export(test_chat80_regressions/0).
test_chat80_regressions:- time(hi(logicmoo(parsing/chat80/demo))).

:- retract(thlocal:into_form_code).


moo:mud_test(chat80_regressions,test_chat80_regressions).

% :- module_predicates_are_exported.

% :- module_meta_predicates_are_transparent(chat).

% :- include(logicmoo('vworld/moo_footer.pl')).


