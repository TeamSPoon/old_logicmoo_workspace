/* <module> Adds autoloading of LogicMOO Utilities predicates
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
:- if(( system:system:use_module(library('logicmoo/util/logicmoo_util_filesystem.pl')), push_modules)). 
:- endif.
:- module(logicmoo_utils_file,[]).
% restore entry state
:- reset_modules.

:- if( \+ current_predicate(system:setup_call_cleanup_each/3)).
:- system:use_module(library('logicmoo/util/logicmoo_util_supp.pl')).
:- endif.

% ======================================================
% Included separated logicmoo util files
% ======================================================
/*
:- module_transparent(user:term_expansion/1).
user:term_expansion(EOF,POS,O,POS2):- 
 is_file_based_expansion(term,EOF,POS,O,POS2),
 nonvar(EOF),
 (EOF=end_of_file;EOF=(:-(module(_,_)))),
 prolog_load_context(module,M),
 M\==user, 
 ignore((
    source_location(S,_),
    '$current_typein_module'(TM),
    glean_prolog_impl_file(EOF,S,M,TM))),fail.

*/
:- set_prolog_flag(system:generate_debug_info, true).
:- set_prolog_flag(generate_debug_info, true).

:- system:use_module(library('logicmoo/util/logicmoo_util_database.pl')).
:- system:use_module(library('logicmoo/util/logicmoo_util_first.pl')).
:- system:use_module(library('logicmoo/util/logicmoo_util_catch.pl')).
:- system:use_module(library('logicmoo/util/logicmoo_util_dmsg.pl')).
:- system:use_module(library('logicmoo/util/logicmoo_util_rtrace.pl')).
:- system:use_module(library('logicmoo/util/logicmoo_util_varnames.pl')).
:- system:use_module(library('logicmoo/util/logicmoo_util_bugger.pl')).
:- system:use_module(library('logicmoo/util/logicmoo_util_loop_check.pl')).
:- system:use_module(library('logicmoo/util/logicmoo_util_no_repeats.pl')).
:- system:use_module(library('logicmoo/util/logicmoo_util_scce.pl')).
:- system:use_module(library('logicmoo/util/logicmoo_util_terms.pl')).
:- system:use_module(library('logicmoo/util/logicmoo_util_dumpst.pl')).
:- system:use_module(library('logicmoo/util/logicmoo_util_with_assertions.pl')).
:- system:use_module(library('logicmoo/util/logicmoo_util_shared_dynamic.pl')).
:- system:use_module(library('logicmoo/util/logicmoo_util_preddefs.pl')).
:- system:use_module(library('logicmoo/util/logicmoo_util_attvar_reader.pl')).
:- system:use_module(library('logicmoo/util/logicmoo_util_term_listing.pl')).
:- system:use_module(library('logicmoo/util/logicmoo_util_strings.pl')).
:- system:use_module(library('logicmoo/util/logicmoo_util_filestreams.pl')).
:- system:use_module(library('logicmoo/util/logicmoo_util_prolog_frames.pl')).
:- system:use_module(library('logicmoo/util/logicmoo_util_prolog_streams.pl')).
:- system:use_module(library('logicmoo/util/logicmoo_util_engines.pl')).
:- system:use_module(library('logicmoo/util/logicmoo_util_help.pl')).

:- system:use_module(library(logicmoo_swilib)).

% ======================================================
% Add Extra file_search_paths
% ======================================================
:- dynamic(user:file_search_path/2).
:- multifile(user:file_search_path/2).

% ======================================================
% Add Extra pack-ages directory
% ======================================================
% :- initialization(attach_packs,now).
:- if( \+ exists_source(pack(logicmoo_base/prolog/logicmoo/logicmoo_utils))).
:- add_file_search_path(pack,'../../').
% :- initialization(attach_packs,now).
:- endif.
% ======================================================
% Save a directory of *this* file into logicmoo(..)
% And adds the local directories to file search path of logicmoo(..)
% ======================================================
:- if( \+ exists_source(logicmoo(logicmoo_engine))).
:- add_file_search_path(logicmoo,'./logicmoo/').
:- exists_source(logicmoo(logicmoo_engine)).
:- endif.


% ======================================================
% Add Utils files to autoloads
% ======================================================
:- add_library_search_path('./logicmoo/util/',[ 'logicmoo_util_*.pl']).

% ======================================================
% Pre-release Sanity tests
% ======================================================
:- dynamic(lmconf:logicmoo_scan_autoloads/0).
:- dynamic(lmconf:logicmoo_pre_release/0).

%= 	 	 

%% logicmoo_pre_release is semidet.
%
% Hook To [lmconf:logicmoo_pre_release/0] For Module Logicmoo_utils.
% Logicmoo Pre Release.
%
lmconf:logicmoo_pre_release.

:- if(lmconf:logicmoo_pre_release).
/*
:- set_prolog_flag(report_error,true),set_prolog_flag(debug_on_error,true),set_prolog_flag(debug, true).
:- set_prolog_flag(debugger_write_options,[quoted(true), portray(true), max_depth(100000)]).
:- set_prolog_flag(verbose_load,true).
*/
:- set_prolog_flag(backtrace_show_lines, true).
:- set_prolog_flag(debugger_show_context,true).

:- if(current_prolog_flag(gui,true)).
% :- guitracer.
% :- cnotrace(trace).
:- cnotrace.
:- endif.


%% logicmoo_scan_autoloads is semidet.
%
% Hook To [lmconf:logicmoo_scan_autoloads/0] For Module Logicmoo_utils.
% Logicmoo Scan Autoloads.
%
lmconf:logicmoo_scan_autoloads:-false.

:- endif.
% ======================================================
% Pre-release Should check if autoloading messes up anything
% ======================================================

:- if(lmconf:logicmoo_scan_autoloads).
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
:- w_tl((logicmoo_utils_test_tl:-dmsg("Adding logicmoo/utils to autoload path",[])),logicmoo_utils_test_tl).

/*
% the next are loaded idomaticalyl later (if needed)
% :- system:use_module(library('logicmoo/util/logicmoo_util_ctx_frame.pl')).
% :- system:use_module(library('logicmoo/util/logicmoo_util_dra.pl')).
% :- system:use_module(library('logicmoo/util/logicmoo_util_bb_gvar.pl')).
% :- system:use_module(library('logicmoo/util/logicmoo_util_bb_env.pl')).
% :- system:use_module(library('logicmoo/util/logicmoo_util_dcg.pl')).
% :- system:use_module(library('logicmoo/util/logicmoo_util_varfunctors.pl')).
% :- system:use_module(library('logicmoo/util/logicmoo_util_structs.pl')).
% :- system:use_module(library('logicmoo/util/logicmoo_util_supp.pl')).
*/



