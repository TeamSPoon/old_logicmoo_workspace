% #! swipl -L8G -G8G -T8G -f
/** <module> MUD server startup script in SWI-Prolog

*/


:- dynamic   user:file_search_path/2.
:- multifile user:file_search_path/2.

:- dynamic   user:library_directory/1.
:- multifile user:library_directory/1.

:- multifile(mpred_online:semweb_startup/0).

in_logicmoo_repl_source_file.

:-source_file(in_logicmoo_repl_source_file,F),file_directory_name(F, D),
  asserta(user:pmrt(D)),cd(D).


fix_pwd:- exists_directory(runtime)->working_directory(_,runtime);(exists_directory('../runtime')->working_directory(_,'../runtime');true),pwd.
:-fix_pwd.
:-initialization(fix_pwd).

user:file_search_path(weblog, '/usr/lib/swi-prolog/pack/weblog/prolog'):-current_prolog_flag(unix,true).
user:file_search_path(weblog, 'C:/docs/Prolog/weblog/development/weblog/prolog').
user:file_search_path(weblog, 'C:/Users/Administrator/AppData/Roaming/SWI-Prolog/pack/weblog').

pmrt_file_search_path(cliopatria, '../pack/ClioPatria'). % :- current_prolog_flag(unix,true).
pmrt_file_search_path(user, '../pack/ClioPatria/user').
pmrt_file_search_path(swish, '../pack/swish'). % :- current_prolog_flag(unix,true).
pmrt_file_search_path(pack, '../pack').
pmrt_file_search_path(games, '../games').
pmrt_file_search_path(library, LIB):- pmrt_file_search_path_library(LIB).

pmrt_file_search_path_library('../pack/logicmoo_base/prolog').
pmrt_file_search_path_library('../pack/logicmoo_nlu/prolog').
pmrt_file_search_path_library('../pack/logicmoo_packages/prolog').
pmrt_file_search_path_library('../pack/logicmoo_planner/prolog').
pmrt_file_search_path_library('../pack/MUD_ircbot/prolog').

:-asserta((user:library_directory(R):- user:pmrt(R))).
:-asserta((user:library_directory(R):- pmrt_expand_file_search_path(library,R))).

pmrt_expand_file_search_path(T,O):-pmrt_file_search_path(T,A), once((user:pmrt(D),absolute_file_name(A,R,[relative_to(D),file_type(directory),access(exist)]))),R=O.

user:file_search_path(T,R):-pmrt_expand_file_search_path(T,R).



:- if(current_prolog_flag(dialect,yap)).
:-  expects_dialect(swi).
@(C,M) :- M:call(C).
user:file_search_path(library, '../pack/logicmoo_base/prolog').
user:file_search_path(library, '../pack/logicmoo_nlu/prolog').
user:file_search_path(library, '../pack/logicmoo_packages/prolog').
user:file_search_path(library, '../pack/logicmoo_planner/prolog').

:- else.

:-set_prolog_stack(global, limit(16*10**9)).
:-set_prolog_stack(local, limit(16*10**9)).
:-set_prolog_stack(trail, limit(16*10**9)).
:- statistics.
:- attach_packs.
:- initialization(attach_packs).
user:file_search_path(prologmud, library(prologmud)).
:- user:use_module(library(persistency)).
setup_rl_read_history:-
  ((current_prolog_flag(readline, true))->expand_file_name("~/.pl-history", [File|_]),(exists_file(File) -> rl_read_history(File); true),at_halt(rl_write_history(File));true).




machine_config_file(Y):- gethostname(X),concat_atom([machine_,X,.,pl],Y).
load_machine_config:- ignore((machine_config_file(Y),exists_file(Y),ensure_loaded(Y))).

% :- multifile sandbox:safe_primitive/1.
% :-asserta((sandbox:safe_primitive(Z):-wdmsg(Z))).

%%% ON :- initialization( profiler(_,walltime) ).
%%% ON :- initialization(user:use_module(library(swi/pce_profile))).

% [Required] Load the Logicmoo Library Utils
:- user:ensure_loaded(library(logicmoo/util/logicmoo_util_all)).
% :- qcompile_libraries.



% [Optionaly] Load an Eggdrop (Expects you have  Eggdrop runinng with PROLOG.TCL scripts @ https://github.com/TeamSPoon/MUD_ircbot/)
:- if_file_exists(user:ensure_loaded(library(eggdrop))).
:- eggdrop:egg_go.
:- initialization((current_predicate(egg_go/0)->egg_go;true),now).

:- asserta(user:load_mud_www).

:- setup_rl_read_history.
:- initialization(setup_rl_read_history,now).


% [Mostly Required] Load the UPV Curry System
%:- time(user:ensure_loaded(library(upv_curry/main))).


% [Required] Load the Logicmoo WWW System
:- (if_file_exists(user:ensure_loaded(library(logicmoo/logicmoo_run_pldoc)))).
:- (if_file_exists(user:ensure_loaded(library(logicmoo/logicmoo_run_swish)))).
:- (if_file_exists(user:ensure_loaded(library(logicmoo/logicmoo_run_clio)))).

:- ensure_webserver.



:- endif.


% [Required] Load the Logicmoo Base System
:- time(user:ensure_loaded(logicmoo(logicmoo_base))).
:- gripe_time(40,user:ensure_loaded(logicmoo(mpred_online/logicmoo_i_www))).

:- asserta(thlocal:disable_mpred_term_expansions_locally).

:- multifile(user:push_env_ctx/0).
:- dynamic(user:push_env_ctx/0).

push_env_ctx:-!,fail.
push_env_ctx:-!.

:- load_machine_config.
:- initialization(load_machine_config,now).



% [Required] Load the Logicmoo Backchaining Inference System
:- gripe_time(40,with_no_mpred_expansions(if_file_exists(user:ensure_loaded(logicmoo(logicmoo_engine))))).

% :- statistics(globallimit,G),statistics(locallimit,L),statistics(traillimit,T),qsave_program(logicmoo_repl,[map('logicmoo_repl.sav'),global(G),trail(T),local(L)]).
