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

/*
% Find all "install_converter", Subfolders, "T:\devel\LogicmooDeveloperFramework\PrologMUD\pack\MUD_PDDL\prolog\ape\", "*.*"

File T:\devel\LogicmooDeveloperFramework\PrologMUD\pack\MUD_PDDL\prolog\ape\parser\ace_to_drs.pl
  64 4::- install_converter(acetext_to_drs(+acetext, -sentences, -syntaxTrees, -drs, -messages)).
  167 4::- install_converter(call_tokenizer(+acetext, +(guess,on), -sentences, -sentencesToParse)).
  192 4::- install_converter(call_parser(+sentences, +(startID,99), -syntaxtrees, -(drs,reversed))).
  198 4::- install_converter(paragraphs_to_drs(paragraphs, +(guess,on), +(catch,on), +(startID,99), -sentences, -syntaxTrees, -drs, -messages, -time)).
File T:\devel\LogicmooDeveloperFramework\PrologMUD\pack\MUD_PDDL\prolog\ape\parser\tokenizer.pl
  69 4::- install_converter(tokenize(+acetext, -tokens:list)).
File T:\devel\LogicmooDeveloperFramework\PrologMUD\pack\MUD_PDDL\prolog\ape\parser\tokens_to_sentences.pl
  54 4::- install_converter(tokens_to_sentences(+tokens, -sentences:list)).
  100 4::- install_converter(tokens_to_paragraphs(+tokens, -sentences:list)).
File T:\devel\LogicmooDeveloperFramework\PrologMUD\pack\MUD_PDDL\prolog\ape\utils\drs_to_sdrs.pl
  37 4::- install_converter(drs_to_sdrs(+drs, -sdrs)).
File T:\devel\LogicmooDeveloperFramework\PrologMUD\pack\MUD_PDDL\prolog\ape\utils\drs_to_drslist.pl
  38 4::- install_converter(drs_to_drslist(+drs, -drs:list)).
File T:\devel\LogicmooDeveloperFramework\PrologMUD\pack\MUD_PDDL\prolog\ape\utils\drs_to_ace.pl
  35 4::- install_converter(drs_to_ace(+drs, -acetext:list)).
  54 4::- install_converter(drs_to_ace(+drs:list, -acetext:list)).
File T:\devel\LogicmooDeveloperFramework\PrologMUD\pack\MUD_PDDL\prolog\ape\utils\drs_to_fol_to_prenex.pl
  71 4::- install_converter(drs_fol(+drs, -ke)).
  93 4::- install_converter(drs_pnf(+drs, -ke)).

 */

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
