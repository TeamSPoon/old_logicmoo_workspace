%!swipl -f
/** <module> An Implementation a MUD server in SWI-Prolog

*/

:- use_module(library(settings)).
% :- use_module(library(check)).
% :- make.
:- portray_text(true).

:-module(user).

:- multifile( entailment:rdf /3 ).


:-context_module(CM),assert(startup_mod:loading_from_cm(CM)).
create_module(M):-context_module(CM),module(M),asserta(M:this_is_a_module(M)),writeq(switching_back_to_module(CM)),module(CM).
:-create_module(user).
:-create_module(hook).
:-create_module(thlocal).
:-create_module(thglobal).
:-create_module(moo).


:-module_transparent moo:parser_chat80_module/1.
:-multifile moo:parser_chat80_module/1.
:-export((moo:parser_chat80_module/1)).
moo:parser_chat80_module(moo).


:-export(prolog_repl/0).
prolog_repl:- nl,fmt("Press Ctrl-D to start the mud!"),nl,catch(tlocals,E,dmsg(tlocals==E)),prolog.

:- set_prolog_flag(gui,false).
:- set_prolog_flag(history,1000).
:- set_prolog_flag(history,1000).

:-export(within_user/1).
:-export(is_startup_file/1).

is_startup_file(Name):- current_prolog_flag(os_argv,ArgV),member(Named,ArgV),atom(Named),atom_concat(Name,_,Named),!.

within_user(Call):- '@'(Call,'user').

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

start_boxer:-
   threads,
   ensure_loaded(logicmoo(candc/parser_boxer)),
   % make,   
   at_start(prolog_repl).


% We don't start cliopatria we here. We have to manually start
%  with  ?- start_servers.
hard_work:- 
   ensure_loaded(logicmoo(launchcliopatria)),
   % use_module('t:/devel/cliopatria/rdfql/sparql_runtime.pl'),
   ensure_loaded(logicmoo(testwebconsole)),
   !.

slow_work:- with_assertions( moo:prevent_transform_moo_preds , within_user(at_start(hard_work))).
thread_work:- thread_property(X, status(running)),X=loading_code,!.
thread_work:- thread_create(slow_work,_,[alias(loading_code)]).

start_servers :- startup_mod:if_version_greater(70111,thread_work).


% [Required] load and start mud
:- within_user(ensure_loaded(logicmoo(vworld/moo_startup))).

startup_mod:run_setup_now:-
   within_user((
   load_game(logicmoo('rooms/startrek.all.plmoo'))
   % TO UNDO register_timer_thread(npc_ticker,90,npc_tick)
   )).

run_setup:- within_user(at_start(startup_mod:run_setup_now)).

run:- within_user(at_start(login_and_run)).


