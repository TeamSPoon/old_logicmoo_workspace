%!swipl -f
/** <module> An Implementation a MUD server in SWI-Prolog

*/
:- debug.
:- use_module(library(settings)).
% :- use_module(library(check)).
% :- make.
:- portray_text(true).

:- dynamic(run_debug:fullStart/0).

:-context_module(CM),assert(run_debug:loading_from_cm(CM)).
:-module(run_debug).
:-run_debug:loading_from_cm(CM),module(CM).

% :- guitracer.
:- set_prolog_flag(verbose_load,true).

% ======================================================
% Configure the logicmoo utilities into the file path
% :- include('logicmoo_util/logicmoo_util_header').
% :- user_use_module('logicmoo_util/logicmoo_util_all.pl').
% And adds the local directories to file search path of logicmoo(..)
% ======================================================
:- consult('logicmoo_util/logicmoo_util_all').

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
hard_work:-ensure_loaded(logicmoo(launchcliopatria)).
slow_work:-with_assertions(moo:prevent_transform_moo_preds,hard_work),retractall(prevent_transform_moo_preds).
start_servers :- startup_mod:if_version_greater(70111,thread_create(slow_work,_,[alias(loading_code)])).

% run_debug:start_servers
% this is evil. Starts the old mudconsole, the experiment with Jan's
% webconsole. We're not using that
% :- startup_mod:if_version_greater(70109,http_mud_server).

:- if_flag_true(fullStart, run_debug:start_servers).

% [Required] load and start mud
:- ensure_loaded(logicmoo('vworld/moo_startup')).



:-run_debug:loading_from_cm(CM),module(CM).
% :-module(user).
% :-prolog.
% end_of_file.


% [Optionaly] Put a telnet client handler on the main console
% :- at_start(login_and_run).

load_default_game :- load_game(logicmoo('rooms/startrek.all.pl')).

run_setup:-
   nodebug,
   debug,  
   at_start(load_default_game),!,
   register_timer_thread(npc_ticker,1,npc_tick_tock),!.

run:-
   login_and_run.

% LOGICMOO LOGICSERVER DATA (Defaut uncommented)
:- if_flag_true(fullStart, user_use_module(logicmoo('data/mworld0.pldata'))).

% :- if_flag_true(fullStart, load_game(logicmoo('rooms/startrek.all.pl'))).

%:- register_timer_thread(npc_ticker,30,npc_tick).

% do some sanity testing
ht:- do_player_action('s'),
   do_player_action(look),
   do_player_action('s'),
   do_player_action('s'),
   do_player_action('e'),
   do_player_action('e'),
   do_player_action(look),
   do_player_action('s'),
   do_player_action('s').

:- catch(noguitracer,_,true).

% :- make.

% :- at_start(run_debug:start_servers).


% do some sanity testing
ht:- do_player_action('s'),
   do_player_action(look),
   do_player_action('s'),
   do_player_action('s'),
   do_player_action('e'),
   do_player_action('e'),
   do_player_action(look),
   do_player_action('s'),
   do_player_action('s').

% :- at_start(start_servers).

dyn:mud_test(test_movedist,
 (
  foc_current_player(P),
   test_name("teleport to main enginering"),
   do_player_action('tp Area1000'),
  test_true(req(inRegion(P,'Area1000'))),
  % test_true(req(atloc(P,'Area1000'))),
   test_name("set the move dist to 5 meters"),
   do_player_action('@set movedist 5'),
   test_name("going 5 meters"),
   do_player_action('n'),
   test_name("must be now be in corridor"),
   test_true(req(inRegion(P,'Area1001'))),
   do_player_action('@set movedist 1'),
   call_n_times(5, do_player_action('s')),
   do_player_action('s'),
   test_name("must be now be back in engineering"),
   test_true(req(inRegion(P,'Area1000'))))).

dyn:mud_test(drop_take,
 (
  do_player_action('drop food'),
  do_player_action('take food')
)).

:- at_start(must_det(run_setup)).

:-must_det((foc_current_player(Agent),dmsg(foc_current_player(Agent)))).
:-foc_current_player(Agent),must_det((atloc(Agent,Where))),dmsg(atloc(Agent,Where)).

:- at_start((debug,must_det(run_mud_tests))).

:- at_start((debug,must_det(run))).

end_of_file.

% So scripted versions don't just exit
:- at_start(prolog).


:- doc_server(4088).

:- use_module(library(pldoc/doc_library)).
:- doc_load_library.

