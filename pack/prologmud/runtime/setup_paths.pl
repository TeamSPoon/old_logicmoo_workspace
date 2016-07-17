#!/usr/bin/env swipl
/** <module> LogicMOO initial paths

*/

system:'$set_source_module'(X):-'$set_source_module'(_,X).
system:'$set_typein_module'(X):-'$set_typein_module'(_,X).
system:'$current_source_module'(X):-'$set_source_module'(X,X).
system:'$current_typein_module'(X):-'$set_typein_module'(X,X).
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


:-source_file(in_logicmoo_repl_source_file,F),file_directory_name(F, D),
  asserta(pmrt(D)),cd(D).


user:file_search_path(weblog, '/usr/lib/swi-prolog/pack/weblog/prolog'):-current_prolog_flag(unix,true).
user:file_search_path(weblog, 'C:/docs/Prolog/weblog/development/weblog/prolog').
user:file_search_path(weblog, 'C:/Users/Administrator/AppData/Roaming/SWI-Prolog/pack/weblog').

pmrt_file_search_path(pack, '../../../pack').
pmrt_file_search_path(cliopatria,pack('ClioPatria')). % :- current_prolog_flag(unix,true).
pmrt_file_search_path(user, pack('ClioPatria/user')).
pmrt_file_search_path(user, pack('swish')).
pmrt_file_search_path(prologmud, library(prologmud)).


pmrt_file_search_path(games, '../games').
pmrt_file_search_path(library, LIB):- pmrt_file_search_path_library(LIB).

pmrt_file_search_path_library(pack('logicmoo_base/prolog')).
pmrt_file_search_path_library(pack('logicmoo_nlu/prolog')).
pmrt_file_search_path_library(pack('logicmoo_packages/prolog')).
pmrt_file_search_path_library(pack('logicmoo_planner/prolog')).
% pmrt_file_search_path_library(pack('MUD_ircbot/prolog')).

:-asserta((user:library_directory(R):- pmrt(R))).
:-asserta((user:library_directory(R):- pmrt_expand_file_search_path(library,R))).

pmrt_expand_file_search_path(T,O):- pmrt_file_search_path(T,A), once((pmrt(D),absolute_file_name(A,R,[relative_to(D),file_type(directory),access(exist)]))),R=O.

user:file_search_path(T,R):- pmrt_expand_file_search_path(T,R).

:- attach_packs.
:- system:ensure_loaded(library(logicmoo_utils)).
:- endif.

:- attach_packs.
:- initialization(attach_packs).

:- endif.   % SWI-Prolog


