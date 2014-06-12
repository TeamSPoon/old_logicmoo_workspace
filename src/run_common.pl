%!swipl -f
/** <module> An Implementation a MUD server in SWI-Prolog

*/

:- use_module(library(settings)).
% :- use_module(library(check)).
% :- make.
:- portray_text(true).

:-context_module(CM),assert(startup_mod:loading_from_cm(CM)).
:-module(startup_mod).

:-export(within_user/1).

within_user(Call):- context_module(CM),setup_call_cleanup(true,(module(user),'@'(Call,'user')),module(CM)).
% ======================================================
% Configure the logicmoo utilities into the file path
% :- include('logicmoo_util/logicmoo_util_header').
% :- user_use_module('logicmoo_util/logicmoo_util_all.pl').
% And adds the local directories to file search path of logicmoo(..)
% ======================================================
:- within_user(consult('logicmoo_util/logicmoo_util_all')).

% one more case of not clear what's the good way to do this.
% Add your own path to weblog for now
user:file_search_path(weblog, 'C:/docs/Prolog/weblog/development/weblog/prolog').
user:file_search_path(weblog, 'C:/Users/Administrator/AppData/Roaming/SWI-Prolog/pack/weblog').

user:file_search_path(weblog, '/usr/local/lib/swipl-7.1.11/pack/weblog/prolog'):-current_prolog_flag(unix,true).
user:file_search_path(cliopatria, '/devel/ClioPatria'). %  current_prolog_flag(unix,true).
%user:file_search_path(cliopatria, 't:/devel/ClioPatria'):- not( current_prolog_flag(unix,true)).

:- user_use_module(library(settings)).

:- user:file_search_path(cliopatria,SP),
   exists_directory(SP),
   writeq(user:file_search_path(cliopatria,SP)),nl.
   %set_setting_default(cliopatria_binding:path, SP).
   %save_settings('moo_settings.db').
   %%setting(cliopatria_binding:path, atom, SP, 'Path to root of cliopatria install'),!.

:- user_use_module(logicmoo('http/user_page')).

:- meta_predicate(startup_mod:if_version_greater(?,0)).

startup_mod:if_version_greater(V,Goal):- current_prolog_flag(version,F), ((F > V) -> call(Goal) ; true).

% set to false because we don't want to use the mudconsole
:- if_flag_true(false, startup_mod:if_version_greater(70109,user_use_module(logicmoo('mudconsole/mudconsolestart')))).

% [Optionaly 1st run] tell where ClioPatria is located and restart for the 2nd run
%:- set_setting(cliopatria_binding:path, '/devel/ClioPatria'), save_settings('moo_settings.db').

% [Optionaly] load and start sparql server
% if we don't start cliopatria we have to manually start
%
% :- use_module('t:/devel/cliopatria/rdfql/sparql_runtime.pl').
hard_work:- within_user(ensure_loaded(logicmoo(launchcliopatria))).
slow_work:- with_assertions(moo:prevent_transform_moo_preds,within_user(hard_work)),retractall(prevent_transform_moo_preds).
start_servers :- startup_mod:if_version_greater(70111,thread_create(_,slow_work,[alias(loading_code)])).

% startup_mod:start_servers
% this is evil. Starts the old mudconsole, the experiment with Jan's
% webconsole. We're not using that
% :- startup_mod:if_version_greater(70109,http_mud_server).

% [Required] load and start mud
:- within_user(ensure_loaded(logicmoo(vworld/moo_startup))).

startup_mod:run_setup_now:-
   within_user((
   load_default_game,
   register_timer_thread(npc_ticker,90,npc_tick))).

run_setup:- within_user(at_start(startup_mod:run_setup_now)).

run:- within_user(at_start(login_and_run)).


