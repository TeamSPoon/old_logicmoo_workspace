/** <module> Adds autoloading of LogicMOO Utilities predicates
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
:- module(logicmoo_utils,[add_file_search_path/2,add_library_search_path/2]).

:- dynamic(lmconf:logicmoo_utils_separate/0).
:- retractall(lmconf:logicmoo_utils_separate).
:- set_prolog_flag(generate_debug_info, true).


% ======================================================
% Add Extra file_search_paths
% ======================================================
:- dynamic(user:file_search_path/2).
:- multifile(user:file_search_path/2).

resolve_dir(Dir,Dir):- is_absolute_file_name(Dir),!,exists_directory(Dir),!.
resolve_dir(Path,Dir):- (prolog_load_context(directory,SDir);(prolog_load_context(file,File),file_directory_name(File,SDir)),working_directory(SDir,SDir)),
   absolute_file_name(Path,Dir,[relative_to(SDir),file_type(directory)]),exists_directory(Dir).


%%	add_file_search_path(+Alias, +WildCard) is det.
%
%	Create an alias when it is missing
%
%	  ==
%	  :- add_file_search_path(all_utils, '../*/util/').
%	  ==
%
add_file_search_path(Name,Path):-  resolve_dir(Path,Dir),
   is_absolute_file_name(Dir), (( \+ user:file_search_path(Name,Dir)) ->asserta(user:file_search_path(Name,Dir));true).
   
% ======================================================
% Add Extra pack-ages directory
% ======================================================
:- initialization(attach_packs,now).
:- if( \+ exists_source(pack(logicmoo_base/prolog/logicmoo/logicmoo_utils))).
:- add_file_search_path(pack,'../../../').
:- initialization(attach_packs,now).
:- endif.
% ======================================================
% Save a directory of *this* file into logicmoo(..)
% And adds the local directories to file search path of logicmoo(..)
% ======================================================
:- if( \+ exists_source(logicmoo(logicmoo_utils))).
:- add_file_search_path(logicmoo,'.').
:- endif.

%%	add_library_search_path(+Dir, +Patterns:list(atom)) is det.
%
%	Create an autoload index INDEX.pl for  Dir by scanning all files
%	that match any of the file-patterns in Patterns. Typically, this
%	appears as a directive in MKINDEX.pl.  For example:
%
%	  ==
%	  :- add_library_search_path('../*/util/',[ 'logicmoo_util_*.pl']).
%	  ==
%
add_library_search_path(Path,Masks):- 
      forall(resolve_dir(Path,Dir), 
      (make_library_index(Dir, Masks), 
      (user:library_directory(Dir) -> true ; (asserta(user:library_directory(Dir)), reload_library_index)))).

% ======================================================
% Add Utils files to autoloads
% ======================================================
:- add_library_search_path('./util/',[ 'logicmoo_util_*.pl']).

% ======================================================
% Pre-release Sanity tests
% ======================================================
:- dynamic(lmconf:logicmoo_scan_autoloads/0).
:- dynamic(lmconf:logicmoo_pre_release/0).
lmconf:logicmoo_pre_release.

:- if(lmconf:logicmoo_pre_release).
/*
:- set_prolog_flag(report_error,true),set_prolog_flag(debug_on_error,true),set_prolog_flag(debug, true).
:- set_prolog_flag(debugger_write_options,[quoted(true), portray(true), max_depth(100000)]).
:- set_prolog_flag(backtrace_show_lines, true).
:- set_prolog_flag(debugger_show_context,true).
:- set_prolog_flag(verbose_load,true).
*/

:- if(current_prolog_flag(gui,true)).
% :- guitracer.
:- notrace(trace).
:- notrace.
:- endif.

lmconf:logicmoo_scan_autoloads:-false.

:- endif.
% ======================================================
% Pre-release Should check if autoloading messes up anything
% ======================================================

:- if(lmconf:logicmoo_scan_autoloads).
:- set_prolog_flag(verbose_autoload, false).
:- autoload.
:- set_prolog_flag(verbose_autoload, true).
:- endif.

% ======================================================
% Create one big logicmoo_utils module
% ======================================================
:- multifile((term_expansion/2,user:term_expansion/2,system:term_expansion/2)).
:- dynamic((term_expansion/2,user:term_expansion/2,system:term_expansion/2)).
%user:term_expansion((:-module(Name,List)), :-maplist(export,List)):- atom(Name),atom_concat(logicmoo_util_,_,Name).
%user:term_expansion((:-use_module(Name)), :-true):- atom(Name),atom_concat(logicmoo_util_,_,Name).
%user:term_expansion((:-ensure_loaded(Name)), :-true):- atom(Name),atom_concat(logicmoo_util_,_,Name).

% ======================================================
% Included separated logicmoo util files
% ======================================================
%:- export(use_libraries/1). 
%use_libraries(M):- F= (util/_),foreach(lmconf:mpred_is_impl_file(F),(writeln(M:use_module(F)),M:ensure_loaded(F))).

%:- export(use_libraries/0). 
%use_libraries:- source_context_module(M),use_libraries(M).

:- multifile(lmconf:mpred_is_impl_file/1).
:- dynamic(lmconf:mpred_is_impl_file/1).
 lmconf:mpred_is_impl_file(util/logicmoo_util_first).
 lmconf:mpred_is_impl_file(util/logicmoo_util_database).
 lmconf:mpred_is_impl_file(util/logicmoo_util_catch).
 lmconf:mpred_is_impl_file(util/logicmoo_util_with_assertions).
 lmconf:mpred_is_impl_file(util/logicmoo_util_loop_check).
 lmconf:mpred_is_impl_file(util/logicmoo_util_dmsg).
 lmconf:mpred_is_impl_file(util/logicmoo_util_bugger).
 lmconf:mpred_is_impl_file(util/logicmoo_util_ctx_frame).
 lmconf:mpred_is_impl_file(util/logicmoo_util_filestreams).
 lmconf:mpred_is_impl_file(util/logicmoo_util_filesystem).
 lmconf:mpred_is_impl_file(util/logicmoo_util_multivar).
 lmconf:mpred_is_impl_file(util/logicmoo_util_no_repeats).
 lmconf:mpred_is_impl_file(util/logicmoo_util_preddefs).
 lmconf:mpred_is_impl_file(util/logicmoo_util_prolog_frames).
 lmconf:mpred_is_impl_file(util/logicmoo_util_prolog_streams).
 lmconf:mpred_is_impl_file(util/logicmoo_util_term_listing).
 lmconf:mpred_is_impl_file(util/logicmoo_util_terms).
 lmconf:mpred_is_impl_file(util/logicmoo_util_varnames). 
 lmconf:mpred_is_impl_file(util/logicmoo_util_strings). 

:- thread_local logicmoo_utils_test_tl/0.
:- w_tl((logicmoo_utils_test_tl:-dmsg("Adding logicmoo/utils to autoload path",[])),logicmoo_utils_test_tl).

% ?- logicmoo_util_term_listing:xlisting(get_gtime).

% ?- list_undefined.


/*
:- 'mpred_trace_none'(tlbugger:dont_skip_bugger/0).
:- 'mpred_trace_none'(tlbugger:skip_bugger/0).
:- 'mpred_trace_none'(tlbugger:rtracing/0).
*/

