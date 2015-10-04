

:- multifile
        user:'$pldoc'/4,
	user:portray/1,
	user:prolog_list_goal/1,
	user:prolog_predicate_name/2,
	user:prolog_clause_name/2.
:- dynamic
	user:portray/1.


% :- style_check(-discontiguous).
% :- style_check(-singleton).
% % :- style_check(-atom).
/*

:- dynamic   user:file_search_path/2.
:- multifile user:file_search_path/2.
user:file_search_path(pack, '../../../../').
:- attach_packs.
:- initialization(attach_packs).
:- user:ensure_loaded(library(logicmoo/logicmoo_utils)).
*/
