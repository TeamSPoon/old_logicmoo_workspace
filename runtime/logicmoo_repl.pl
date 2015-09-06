% #! swipl -L8G -G8G -T8G -f
/** <module> MUD server startup script in SWI-Prolog

*/


:- dynamic   user:file_search_path/2.
:- multifile user:file_search_path/2.

:- multifile(mpred_online:semweb_startup/0).

:- exists_directory(runtime)->working_directory(_,runtime);(exists_directory('../runtime')->working_directory(_,'../runtime');true).
user:file_search_path(weblog, 'C:/docs/Prolog/weblog/development/weblog/prolog').
user:file_search_path(weblog, 'C:/Users/Administrator/AppData/Roaming/SWI-Prolog/pack/weblog').
user:file_search_path(weblog, '/usr/lib/swi-prolog/pack/weblog/prolog'):-current_prolog_flag(unix,true).
user:file_search_path(cliopatria, '../pack/ClioPatria'). % :- current_prolog_flag(unix,true).
user:file_search_path(user, '../pack/ClioPatria/user/').
user:file_search_path(swish, '../pack/swish'):- current_prolog_flag(unix,true).
user:file_search_path(pack, '../pack/').
user:file_search_path(games, '../games').



:- if(current_prolog_flag(dialect,yap)).
:-  expects_dialect(swi).
@(C,M) :- M:call(C).
user:file_search_path(library, '/devel/LogicmooDeveloperFramework/PrologMUD/pack/logicmoo_base/prolog').
user:file_search_path(library, '/devel/LogicmooDeveloperFramework/PrologMUD/pack/logicmoo_nlu/prolog').
user:file_search_path(library, '/devel/LogicmooDeveloperFramework/PrologMUD/pack/logicmoo_packages/prolog').
user:file_search_path(library, '/devel/LogicmooDeveloperFramework/PrologMUD/pack/logicmoo_planner/prolog').

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


:- endif.


machine_config_file(Y):- gethostname(X),concat_atom([machine_,X,.,pl],Y).
load_machine_config:-machine_config_file(Y),exists_file(Y),ensure_loaded(Y).

% :- multifile sandbox:safe_primitive/1.
% :-asserta((sandbox:safe_primitive(Z):-wdmsg(Z))).

%%% ON :- initialization( profiler(_,walltime) ).
%%% ON :- initialization(user:use_module(library(swi/pce_profile))).

% [Required] Load the Logicmoo Library Utils
:- user:ensure_loaded(library(logicmoo/util/logicmoo_util_all)).
% :- qcompile_libraries.



% [Optionaly] Load an Eggdrop (Expects you have  Eggdrop runinng with PROLOG.TCL scripts @ https://github.com/TeamSPoon/MUD_ircbot/)
:- if_file_exists(user:ensure_loaded(library(eggdrop))).
:-eggdrop:egg_go.
:- initialization((current_predicate(egg_go/0)->egg_go;true),now).

:-asserta(user:load_mud_www).

:- setup_rl_read_history.
:- initialization(setup_rl_read_history,now).


% [Mostly Required] Load the UPV Curry System
%:- time(user:ensure_loaded(library(upv_curry/main))).


% [Required] Load the Logicmoo WWW System
:- (if_file_exists(user:ensure_loaded(library(logicmoo/logicmoo_run_pldoc)))).
:- (if_file_exists(user:ensure_loaded(library(logicmoo/logicmoo_run_swish)))).
:- (if_file_exists(user:ensure_loaded(library(logicmoo/logicmoo_run_clio)))).

:-dynamic(user:startup_option/2).

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


