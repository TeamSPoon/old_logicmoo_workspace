#!/usr/bin/env swipl
/*  
  LogicMOO initial paths

*/

:- if(current_prolog_flag(dialect,yap)).
:-  expects_dialect(swi).
@(C,M) :- M:call(C).
user:file_search_path(library, '../../../pack/logicmoo_base/prolog').
user:file_search_path(library, '../../../pack/logicmoo_nlu/prolog').
user:file_search_path(library, '../../../pack/logicmoo_packages/prolog').
user:file_search_path(library, '../../../pack/logicmoo_planner/prolog').

:- else.


:- multifile(mpred_online:semweb_startup/0).

in_logicmoo_repl_source_file.

% ==============================
% Link up to our local packs
% ==============================
:- attach_packs.
:- initialization(attach_packs).

:- if((exists_source(library(logicmoo_utils)))).
:- system:ensure_loaded(library(logicmoo_utils)).
%:- ensure_loaded(logicmoo(mpred/mpred_pfc)).
%USER :- ensure_loaded(library(logicmoo_user)).

:- else.

fix_pwd :- (exists_directory(runtime) -> working_directory(_,runtime);(exists_directory('../runtime')->working_directory(_,'../runtime');true)), pwd.
:- fix_pwd.
:- initialization(fix_pwd).

:- dynamic   user:file_search_path/2.
:- multifile user:file_search_path/2.

:- dynamic   user:library_directory/1.
:- multifile user:library_directory/1.

:- working_directory(X,X),asserta(lmcache:restore_working_directory(X)).

:- source_location(F,_),file_directory_name(F, D),asserta(lmcache:pmrt(D)),cd(D).


user:file_search_path(weblog, '/usr/lib/swi-prolog/pack/weblog/prolog'):-current_prolog_flag(unix,true).
user:file_search_path(weblog, 'C:/docs/Prolog/weblog/development/weblog/prolog').
user:file_search_path(weblog, 'C:/Users/Administrator/AppData/Roaming/SWI-Prolog/pack/weblog').

system:pmrt_file_search_path(pack, '../../../pack').
system:pmrt_file_search_path(cliopatria,pack('ClioPatria')). % :- current_prolog_flag(unix,true).
system:pmrt_file_search_path(user, pack('ClioPatria/user')).
system:pmrt_file_search_path(user, pack('swish')).
system:pmrt_file_search_path(prologmud, library(prologmud)).


system:pmrt_file_search_path(games, '../games').
system:pmrt_file_search_path(library, LIB):- system:pmrt_file_search_path_library(LIB).

system:pmrt_file_search_path_library(pack('logicmoo_base/prolog')).
system:pmrt_file_search_path_library(pack('logicmoo_nlu/prolog')):-exists_source(pack('logicmoo_nlu/pack.pl')).
system:pmrt_file_search_path_library(pack('logicmoo_packages/prolog')):-exists_source(pack('logicmoo_packages/pack.pl')).
system:pmrt_file_search_path_library(pack('logicmoo_planner/prolog')):-exists_source(pack('logicmoo_planner/pack.pl')).
% system:pmrt_file_search_path_library(pack('MUD_ircbot/prolog')).


system:pmrt_expand_file_search_path(T,O):- system:pmrt_file_search_path(T,A), once((lmcache:pmrt(D),absolute_file_name(A,R,[relative_to(D),file_type(directory),access(exist)]))),R=O.

:-asserta((user:file_search_path(T,R):- system:pmrt_expand_file_search_path(T,R))).
:-asserta((user:library_directory(R):- lmcache:pmrt(R))).
:-asserta((user:library_directory(R):- system:pmrt_expand_file_search_path(library,R))).

:- attach_packs.
:- system:ensure_loaded(library(logicmoo_utils)).
:- endif.

:- attach_packs.
:- initialization(attach_packs).

:- endif.   % SWI-Prolog

:- retract(lmcache:restore_working_directory(X)),working_directory(_,X).
