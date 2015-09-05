/** <module> Logicmoo Path Setups
*/
:- if(current_prolog_flag(dialect,yap)).
swi_export(_P):-!.
:- else.
:-module_transparent(swi_export/1).
user:swi_export(P):-export(P).
:-module_transparent(swi_module/2).
user:swi_module(_,_).
:- endif.

:-swi_module(logicmoo_util_all,[if_flag_true/2]).
:- set_prolog_flag(generate_debug_info, true).
:- set_prolog_flag(access_level,system).



:- user:ensure_loaded((logicmoo_util_filestreams)).
:- user:ensure_loaded((logicmoo_util_filesystem)).
:- user:ensure_loaded((logicmoo_util_term_listing)).

:- dynamic(double_quotes_was/1).
:- multifile(double_quotes_was/1).
:- current_prolog_flag(double_quotes,WAS),asserta(double_quotes_was(WAS)).
:- retract(double_quotes_was(WAS)),set_prolog_flag(double_quotes,WAS).
:- current_prolog_flag(double_quotes,WAS),asserta(double_quotes_was(WAS)).
:- set_prolog_flag(double_quotes,string).

% have to load this module here so we dont take ownership of prolog_exception_hook/4.
:- set_prolog_flag(access_level,system).
%:- set_prolog_flag(verbose_autoload, true).
%:- set_prolog_flag(generate_debug_info, true).
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

:-export(if_flag_true/2).
if_flag_true(Flag,Goal):- catch(Flag,E,(dmsg(E:Flag),fail)) -> must(Goal); true.

join_path33(A,B,C):-exists_directory(B)->B=C;directory_file_path(A,B,C).

:-swi_export(with_vars/2).
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


:- current_prolog_flag(windows,true)->
   setenv('PATH_INDIGOLOG','../../indigolog');
   setenv('PATH_INDIGOLOG','../../indigolog').



:- user:ensure_loaded((logicmoo_util_bugger_new)).
:- user:ensure_loaded((logicmoo_util_bugger_catch)).
:- user:ensure_loaded((logicmoo_util_bugger)).
:- user:ensure_loaded((logicmoo_util_strings)).
:- user:ensure_loaded((logicmoo_util_library)).
:- user:use_module((logicmoo_util_ctx_frame)).
:- user:use_module((logicmoo_util_terms)).
:- user:use_module((logicmoo_util_dcg)).
:- user:use_module((logicmoo_util_coroutining_was)).
:- user:use_module((logicmoo_util_coroutining_iz)).
:- user:ensure_loaded(logicmoo_util_prolog_streams).



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
:- set_prolog_flag(access_level,user).


