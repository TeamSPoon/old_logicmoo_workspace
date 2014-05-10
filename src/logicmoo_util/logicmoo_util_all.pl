/** <module> Logicmoo Path Setups
*/
:-module(logicmoo_util_all,[if_flag_true/2]).

:- meta_predicate if_flag_true(0,0).
:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.

if_flag_true(Flag,Goal):- catch(Flag,_,fail)->catch(Goal,E,throw(if_flag_true(E)));true.
join_path33(A,B,C):-exists_directory(B)->B=C;directory_file_path(A,B,C).

:-include('logicmoo_util_header.pl').

:-dynamic(logicmoo_runtime_dir/1).

% ======================================================
% Save a location of *this* file into logicmoo_runtime_dir/1
% And adds the local directories to file search path of logicmoo(..)
% ======================================================
:- source_location(File,_Line),
    file_directory_name(File, RunDir),
    retractall(logicmoo_runtime_dir(RunDir)),
    asserta(logicmoo_runtime_dir(RunDir)).

/*
:- 
    source_location(File,_Line),
    file_directory_name(File, RunDir),
    atom_concat(RunDir,'/../library',RelDir),
    absolute_file_name(RelDir,A),
   'format'(' ~q. ~n',[user:file_search_path(library, A)]),
   asserta(user:file_search_path(library, A)).
*/

% Add the locations that the MUD source files will be picked up by the system
local_directory_search('../..').
local_directory_search('..'). 
local_directory_search('.'). 
local_directory_search('../../src_game').  % for non uploadables
local_directory_search('../../src_incoming').  % for user uploads
local_directory_search('../../src').  % for user uploads
local_directory_search('../../src_modules').  % for big modules
local_directory_search('../../src_data'). 
local_directory_search('../../src_natlang_data').  
local_directory_search('~logicmoo-mud/cynd/startrek'). % home dir CynD world

:- current_prolog_flag(windows,true)->
   setenv('PATH_INDIGOLOG','t:/devel/logicmoo/src_modules/indigolog');
   setenv('PATH_INDIGOLOG','/devel/logicmoo/src_modules/indigolog').

% register search path hook
user:file_search_path(library,ATLIB):-getenv('PATH_INDIGOLOG',AT),atom_concat(AT,'/lib',ATLIB).
user:file_search_path(indigolog,AT):-getenv('PATH_INDIGOLOG',AT).
user:file_search_path(logicmoo,Dir):- logicmoo_runtime_dir(RunDir), local_directory_search(Locally), join_path33(RunDir,Locally,Directory),absolute_file_name(Directory,Dir).

:-forall(user:file_search_path(logicmoo,Dir),(writeq(Dir),nl)).

:- use_module(logicmoo('logicmoo_util/logicmoo_util_bugger.pl')).
:- use_module(logicmoo('logicmoo_util/logicmoo_util_library.pl')).
:- use_module(logicmoo('logicmoo_util/logicmoo_util_ctx_frame.pl')).
:- use_module(logicmoo('logicmoo_util/logicmoo_util_strings.pl')).
:- use_module(logicmoo('logicmoo_util/logicmoo_util_terms.pl')).
:- use_module(logicmoo('logicmoo_util/logicmoo_util_dcg.pl')).



/*
win_fork(G,SERVIO,PID):-atom_concat('swipl-win.exe ',G,AC),writeq(win_fork(AC,SERVIO)),nl,
      win_exec(AC,showdefault),PID = 0.

:- current_prolog_flag(windows,true)->
   asserta((fork(G):-win_fork(G)));
   use_module(library(unix)).

% fork(G):-writeq(fork(G)),nl.
*/


