/* 
< module > Adds autoloading of LogicMOO Utilities predicates
% ===================================================================
    File:         'logicmoo_utils).'
    Purpose:       To load the logicmoo libraries as needed
    Contact:       $Author: dmiles $@users.sourceforge.net ;
    Version:       'logicmoo_utils).' 1.0.0
    Revision:      $Revision: 1.7 $
    Revised At:    $Date: 2002/07/11 21:57:28 $
    Author:        Douglas R. Miles
    Maintainers:   TeamSPoon
    E-mail:        logicmoo@gmail.com
    WWW:           http://www.prologmoo.com
    SCM:           https://github.com/TeamSPoon/PrologMUD/tree/master/pack/logicmoo_base
    Copyleft:      1999-2015, LogicMOO Prolog Extensions
    License:       Lesser GNU Public License
% ===================================================================
*/

:- if((prolog_load_context(directory,Dir),
   multifile(user:file_search_path/2),
   dynamic(user:file_search_path/2),
   (( \+ user:file_search_path(library,Dir)) ->asserta(user:file_search_path(library,Dir));true))).
:- endif.
:- if((set_prolog_flag(logicmoo_utils_file,(exists_source(library('logicmoo/util/logicmoo_util_clause_expansion.pl')), 
       ensure_loaded(library('logicmoo/util/logicmoo_util_clause_expansion.pl')))))).
:- endif.
:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )). 
:- module(logicmoo_utils_file,[logicmoo_utils_test_msg/0]).
:- endif.
% restore entry state
:- current_predicate(lmce:reset_modules/0)->lmce:reset_modules;true.

:- set_prolog_flag(lm_expanders,false).

% ======================================================
% Add Extra file_search_paths
% ======================================================
:- dynamic(user:file_search_path/2).
:- multifile(user:file_search_path/2).


add_file_search_path_local(Name,Path):-  resolve_dir_local(Path,Dir),
   is_absolute_file_name(Dir), (( \+ user:file_search_path(Name,Dir)) ->asserta(user:file_search_path(Name,Dir));true).

resolve_dir_local(Dir,Dir):- is_absolute_file_name(Dir),exists_directory(Dir),!.
resolve_dir_local(Dir,ABS):- absolute_file_name(Dir,ABS),exists_directory(ABS),!.
resolve_dir_local(Dir,ABS):- absolute_file_name(library(Dir),ABS),exists_directory(ABS),!.

% ======================================================
% Add Extra pack-ages directory
% ======================================================
% :- initialization(attach_packs,now).
:- if( \+ exists_source(pack(logicmoo_base/prolog/logicmoo/logicmoo_utils))).
:- add_file_search_path_local(pack,'../../').
% :- initialization(attach_packs,now).
:- endif.
% ======================================================
% Save a directory of *this* file into logicmoo(..)
% And adds the local directories to file search path of logicmoo(..)
% ======================================================
:- if( \+ exists_source(logicmoo(logicmoo_engine))).
:- add_file_search_path_local(logicmoo,'./logicmoo/').
:- exists_source(logicmoo(logicmoo_engine)).
:- endif.



:- if( \+ current_predicate(system:setup_call_cleanup_each/3)).
:- ensure_loaded(library('logicmoo/util/logicmoo_util_supp.pl')).
:- endif.

% ======================================================
% Included separated logicmoo util files
% ======================================================

:- set_prolog_flag(system:generate_debug_info, true).
:- set_prolog_flag(generate_debug_info, true).
:- set_prolog_flag(lm_expanders,false).

:- ensure_loaded(library('logicmoo/util/logicmoo_util_database.pl')).
:- ensure_loaded(library('logicmoo/util/logicmoo_util_first.pl')).
:- ensure_loaded(library('logicmoo/util/logicmoo_util_catch.pl')).
:- ensure_loaded(library('logicmoo/util/logicmoo_util_dmsg.pl')).
:- ensure_loaded(library('logicmoo/util/logicmoo_util_rtrace.pl')).
:- ensure_loaded(library('logicmoo/util/logicmoo_util_varnames.pl')).
:- ensure_loaded(library('logicmoo/util/logicmoo_util_bugger.pl')).
:- ensure_loaded(library('logicmoo/util/logicmoo_util_loop_check.pl')).
:- ensure_loaded(library('logicmoo/util/logicmoo_util_no_repeats.pl')).
:- ensure_loaded(library('logicmoo/util/logicmoo_util_scce.pl')).
:- ensure_loaded(library('logicmoo/util/logicmoo_util_terms.pl')).
:- ensure_loaded(library('logicmoo/util/logicmoo_util_dumpst.pl')).
:- ensure_loaded(library('logicmoo/util/logicmoo_util_with_assertions.pl')).
:- ensure_loaded(library('logicmoo/util/logicmoo_util_preddefs.pl')).

:- ensure_loaded(library('logicmoo/util/logicmoo_util_term_listing.pl')).
:- ensure_loaded(library('logicmoo/util/logicmoo_util_strings.pl')).
:- ensure_loaded(library('logicmoo/util/logicmoo_util_filestreams.pl')).
:- ensure_loaded(library('logicmoo/util/logicmoo_util_filesystem.pl')).
:- ensure_loaded(library('logicmoo/util/logicmoo_util_prolog_frames.pl')).
:- ensure_loaded(library('logicmoo/util/logicmoo_util_prolog_streams.pl')).
:- ensure_loaded(library('logicmoo/util/logicmoo_util_engines.pl')).
:- ensure_loaded(library('logicmoo/util/logicmoo_util_help.pl')).

:- user:ensure_loaded(library(logicmoo_swilib)).
:- system:ensure_loaded(library('logicmoo/util/logicmoo_util_shared_dynamic.pl')).

:- forall((current_module(M),M\==baseKB),assert_if_new(baseKB:mtProlog(M))).


% ======================================================
% Add Utils files to autoloads
% ======================================================
:- add_library_search_path('./logicmoo/util/',[ 'logicmoo_util_*.pl']).

% ======================================================
% Pre-release Sanity tests
% ======================================================
:- dynamic(baseKB:logicmoo_scan_autoloads/0).
:- dynamic(baseKB:logicmoo_pre_release/0).
 	 	 

%  logicmoo_pre_release() is semidet.
%
% Hook To [baseKB:logicmoo_pre_release/0] For Module Logicmoo_utils.
% Logicmoo Pre Release.
%
baseKB:logicmoo_pre_release.

:- if(baseKB:logicmoo_pre_release).
/*
:- set_prolog_flag(debugger_write_options,[quoted(true), portray(true), max_depth(1000)]).
:- set_prolog_flag(verbose_load,true).
:- set_prolog_flag(verbose_autoload, true).
*/
:- set_prolog_flag(debug, true).
:- set_prolog_flag(report_error,true),set_prolog_flag(debug_on_error,true).
:- set_prolog_flag(backtrace_show_lines, true).
:- set_prolog_flag(debugger_show_context,true).

:- if(current_prolog_flag(gui,true)).
:- guitracer.
:- notrace(trace).
:- notrace.
:- endif.


% baseKB:logicmoo_scan_autoloads() is semidet.
%
% Hook To [baseKB:logicmoo_scan_autoloads/0] For Module Logicmoo_utils.
% Logicmoo Scan Autoloads.
%
baseKB:logicmoo_scan_autoloads:-false.

:- endif.
% ======================================================
% Pre-release Should check if autoloading messes up anything
% ======================================================

:- if(baseKB:logicmoo_scan_autoloads).
%:- set_prolog_flag(verbose_autoload, false).
%:- autoload([verbose(false)]).
%:- set_prolog_flag(verbose_autoload, true).
:- endif.

% ======================================================
% Create one big logicmoo_utils module
% ======================================================
:- multifile((term_expansion/2,user:term_expansion/2,system:term_expansion/2)).
:- dynamic((term_expansion/2,user:term_expansion/2,system:term_expansion/2)).
%user:term_expansion((:-module(Name,List)), :-maplist(export,List)):- atom(Name),atom_concat(logicmoo_util_,_,Name).
%user:term_expansion((:-use_module(Name)), :-true):- atom(Name),atom_concat(logicmoo_util_,_,Name).
%user:term_expansion((:-use_module(Name)), :-true):- atom(Name),atom_concat(logicmoo_util_,_,Name).


:- thread_local logicmoo_utils_test_tl/0.
logicmoo_utils_test_msg:- w_tl((
 logicmoo_utils_test_tl:-dmsg("Adding logicmoo/utils to autoload path",[])),logicmoo_utils_test_tl).
:- export(logicmoo_utils_test_msg/0).
/*
% the next are loaded idomaticaly later (if needed)
% :- ensure_loaded(library('logicmoo/util/logicmoo_util_clause_expansion.pl')).
% :- ensure_loaded(library('logicmoo/util/logicmoo_util_attvar_reader.pl')).
% :- ensure_loaded(library('logicmoo/util/logicmoo_util_ctx_frame.pl')).
% :- ensure_loaded(library('logicmoo/util/logicmoo_util_dra.pl')).
% :- ensure_loaded(library('logicmoo/util/logicmoo_util_bb_gvar.pl')).
% :- ensure_loaded(library('logicmoo/util/logicmoo_util_bb_env.pl')).
% :- ensure_loaded(library('logicmoo/util/logicmoo_util_dcg.pl')).
% :- ensure_loaded(library('logicmoo/util/logicmoo_util_varfunctors.pl')).
% :- ensure_loaded(library('logicmoo/util/logicmoo_util_structs.pl')).
% :- ensure_loaded(library('logicmoo/util/logicmoo_util_supp.pl')).
*/
:- multifile baseKB:prologBuiltin/1.
:- discontiguous baseKB:prologBuiltin/1.
:- dynamic baseKB:prologBuiltin/1.
:- logicmoo_utils_test_msg.
:- set_prolog_flag(lm_expanders,true).
:- set_prolog_flag(logicmoo_virtualize,true).
% :- lmce:reset_modules.


