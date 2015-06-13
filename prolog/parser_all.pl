% ===================================================================
% File 'parser_all.pl'
% Purpose: English to KIF conversions from SWI-Prolog  
% This implementation is incomplete
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'parser_all.pl' 1.0.0
% Revision:  $Revision: 1.3 $
% Revised At:   $Date: 2002/06/06 15:43:15 $
% ===================================================================


% ==============================================================================

:- user:ensure_loaded(library(logicmoo/util/logicmoo_util_all)).
:- user:ensure_loaded(library(logicmoo/logicmoo_base)).
:- asserta_if_new(thlocal:disable_mpred_term_expansions_locally/0).

:-multifile(user:type_action_info/3).
:-multifile(user:agent_call_command/2).
:-multifile(user:mud_test/2).

:-meta_predicate(install_converter(0)).
:-export(install_converter/1).
install_converter(CNV):- must(pfc_add(installed_converter(CNV))).

:- user:ignore(( Z = ('/'),current_op(X,Y,Z),display(:-(op(X,Y,Z))),nl,fail)).
:- user:ignore((Z = (':'),current_op(X,Y,Z),display(:-(op(X,Y,Z))),nl,fail)).
:- user:ignore((Z = ('-'),current_op(X,Y,Z),display(:-(op(X,Y,Z))),nl,fail)).
:- writeq(parser_all_start).

%:- user:ensure_loaded(parser_chat80).
:- user:ensure_loaded(parser_ape).
%:- user:ensure_loaded(parser_candc).
%:- user:ensure_loaded(parser_talk).
%:- if_file_exists(user:ensure_loaded(stanford_parser)).
% :- get_pos_tagger(I),jpl_set(I,is_DEBUG,'@'(false)).
%:- user:ensure_loaded(parser_chart89).

/*
:- user:ensure_loaded(parser_CURT).

% Not yet started 
:- user:ensure_loaded(parser_regulus).
:- user:ensure_loaded(parser_SUPPLE).
:- user:ensure_loaded(parser_SIRIDUS).
:- user:ensure_loaded(parser_ProNTo).
*/
