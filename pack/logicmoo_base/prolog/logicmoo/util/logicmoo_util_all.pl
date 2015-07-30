/** <module> Logicmoo Path Setups
*/
:-module(logicmoo_util_all,[if_flag_true/2,add_to_search_path/2,add_to_search_path_first/2,prolog_file_dir/2,if_startup_script/1,if_startup_script/0]).
:- set_prolog_flag(generate_debug_info, true).

:- '@'( ensure_loaded((logicmoo_util_filestreams)), 'user').

:- dynamic(double_quotes_was/1).
:- multifile(double_quotes_was/1).
:- current_prolog_flag(double_quotes,WAS),asserta(double_quotes_was(WAS)).
:- retract(double_quotes_was(WAS)),set_prolog_flag(double_quotes,WAS).
:- current_prolog_flag(double_quotes,WAS),asserta(double_quotes_was(WAS)).
:- set_prolog_flag(double_quotes,string).

% have to load this module here so we dont take ownership of prolog_exception_hook/4.
:- set_prolog_flag(access_level,system).
:- set_prolog_flag(verbose_autoload, true).
:- set_prolog_flag(generate_debug_info, true).
% have to load this module here so we dont take ownership of prolog_exception_hook/4.
:- user:ensure_loaded(library(ansi_term)).
:- user:ensure_loaded(library(check)).
:- user:ensure_loaded(library(debug)).
:- user:ensure_loaded(library(listing)).
:- user:ensure_loaded(library(lists)).
:- user:ensure_loaded(library(make)).
:- user:ensure_loaded(library(prolog_stack)).
:- user:ensure_loaded(library(system)).

:- meta_predicate if_flag_true(0,0).
:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.

%logicmoo_util_all:if_flag_true(Flag,Goal):- catch(Flag,_,fail)->catch(Goal,E,throw(if_flag_true(E)));true.
logicmoo_util_all:if_flag_true(Flag,Goal):- catch(Flag,E,(dmsg(E:Flag),fail)) -> must(Goal); true.

join_path33(A,B,C):-exists_directory(B)->B=C;directory_file_path(A,B,C).

:-export(with_vars/2).
:-module_transparent(with_vars/2). 
:- meta_predicate with_vars(*,0).
with_vars([],Stuff):- !, Stuff.
with_vars([V|Vs],Stuff):- !,
  b_getval('$variable_names', VsOrig),
  append([V|Vs],VsOrig,Temp),
  b_setval('$variable_names', Temp),!,Stuff.
with_vars(_,Stuff):- Stuff.


% this is a backwards compatablity block for SWI-Prolog 6.6.6


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
    my_absolute_file_name(RelDir,A),
   'format'(' ~q. ~n',[user:file_search_path(library, A)]),
   asserta(user:file_search_path(library, A)).
*/

:-multifile(local_directory_search/1).
:-dynamic(local_directory_search/1).
% Add the locations that the MUD source files will be picked up by the system
%local_directory_search('../..').
%local_directory_search('~logicmoo-mud/cynd/startrek'). % home vtDirection CynD world
% local_directory_search('.').
%local_directory_search('..'). 
%local_directory_search('../runtime'). 
%local_directory_search('../src_game'). % for user overrides and uploads
%local_directory_search('../src_assets').  % for non uploadables (downloadables)
%local_directory_search('../src_modules'). % for big modules
%local_directory_search('../src_webui').  % for web UI modules
local_directory_search('../src'). % shared_library preds
local_directory_search('../src_lib').
local_directory_search('../src_mud').  % for vetted src of the MUD
%local_directory_search('../externals/XperiMental/src_incoming').  % areeba underlay

:- current_prolog_flag(windows,true)->
   setenv('PATH_INDIGOLOG','../../indigolog');
   setenv('PATH_INDIGOLOG','../../indigolog').

my_absolute_file_name(F,A):-catch(expand_file_name(F,[A]),_,fail),F\=A,!.
my_absolute_file_name(F,A):-catch(absolute_file_name(F,A),_,fail),!.

% register search path hook
user:file_search_path(library,ATLIB):-getenv('PATH_INDIGOLOG',AT),atom_concat(AT,'/lib',ATLIB).
user:file_search_path(indigolog,AT):-getenv('PATH_INDIGOLOG',AT).
user:file_search_path(logicmoo,Dir):- 
   local_directory_search(Locally),
   locally_to_dir(Locally,Dir).

locally_to_dir(Locally,Dir):-logicmoo_runtime_dir(RunDir), join_path33(RunDir,Locally,Directory),my_absolute_file_name(Directory,Dir),exists_directory(Dir),!.
locally_to_dir(Directory,Dir):-my_absolute_file_name(Directory,Dir),exists_directory(Dir),!.


:- '@'( ensure_loaded((logicmoo_util_bugger_new)), 'user').
:- '@'( ensure_loaded((logicmoo_util_bugger_catch)), 'user').
:- '@'( ensure_loaded((logicmoo_util_bugger)), 'user').
:- '@'( ensure_loaded((logicmoo_util_strings)), 'user').
:- '@'( ensure_loaded((logicmoo_util_library)), 'user').
:- '@'( use_module((logicmoo_util_ctx_frame)), 'user').
:- '@'( use_module((logicmoo_util_terms)), 'user').
:- '@'( use_module((logicmoo_util_dcg)), 'user').
:- '@'( use_module((logicmoo_util_coroutining_was)), 'user').
:- '@'( use_module((logicmoo_util_coroutining_iz)), 'user').


/*
win_fork(G,SERVIO,PID):-atom_concat('swipl-win.exe ',G,AC),writeq(win_fork(AC,SERVIO)),nl,
      win_exec(AC,showdefault),PID = 0.

:- current_prolog_flag(windows,true)->
   asserta((fork(G):-win_fork(G)));
   use_module(library(unix)).

% fork(G):-writeq(fork(G)),nl.
*/


show_file_search_path:- % 'format'('% ~q.~n',[forall(user:file_search_path(_,_))]), 
  forall(user:file_search_path(A,B),'format'('% ~q.~n',[user:file_search_path(A,B)])).

:- show_file_search_path.

% :- list_undefined.

:- logicmoo_util_dcg:call(do_dcg_util_tests).


% this is a backwards compatablity block for SWI-Prolog 6.6.6
:- retract(double_quotes_was(WAS)),set_prolog_flag(double_quotes,WAS).


