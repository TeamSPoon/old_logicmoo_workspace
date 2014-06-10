%!swipl -f
/** <module> An Implementation a MUD server in SWI-Prolog

*/
:- debug.
:- use_module(library(settings)).

:- dynamic(fullStart/0).
% :- guitracer.


% ======================================================
% Configure the logicmoo utilities into the file path
% :- include('logicmoo_util/logicmoo_util_header').
% :- ensure_loaded('logicmoo_util/logicmoo_util_all.pl').
% And adds the local directories to file search path of logicmoo(..)
% ======================================================
:- consult('logicmoo_util/logicmoo_util_all').

% one more case of not clear what's the good way to do this.
% Add your own path to weblog for now
user:file_search_path(weblog, 'C:/docs/Prolog/weblog/development/weblog/prolog').
user:file_search_path(weblog, 'C:/Users/Administrator/AppData/Roaming/SWI-Prolog/pack/weblog').
user:file_search_path(weblog, '/usr/local/lib/swipl-7.1.11/pack/weblog/prolog').
user:file_search_path(cliopatria, '/devel/ClioPatria').
user:file_search_path(cliopatria, 't:/devel/ClioPatria').


:- use_module(library(settings)).

:- user:file_search_path(cliopatria,SP),
   exists_directory(SP),
   writeq(user:file_search_path(cliopatria,SP)),nl.
   %set_setting_default(cliopatria_binding:path, SP).
   %save_settings('moo_settings.db').
   %%setting(cliopatria_binding:path, atom, SP, 'Path to root of cliopatria install'),!.

:- ensure_loaded(logicmoo('http/user_page')).

:- meta_predicate(if_version_greater(?,0)).

if_version_greater(V,Goal):- current_prolog_flag(version,F), ((F > V) -> call(Goal) ; true).

% set to false because we don't want to use the mudconsole
:- if_flag_true(false, if_version_greater(70109,ensure_loaded(logicmoo('mudconsole/mudconsolestart')))).

% [Optionaly 1st run] tell where ClioPatria is located and restart for the 2nd run
%:- set_setting(cliopatria_binding:path, '/devel/ClioPatria'), save_settings('moo_settings.db').

% [Optionaly] load and start sparql server
% if we don't start cliopatria we have to manually start
%
start_servers :- !.
start_servers :- if_version_greater(70111,thread_create(ensure_loaded(logicmoo(launchcliopatria)),_,[alias(loading_code)])).

% start_servers
% this is evil. Starts the old mudconsole, the experiment with Jan's
% webconsole. We're not using that
% :- if_version_greater(70109,http_mud_server).

:- if_flag_true(fullStart, start_servers).

% [Required] load and start mud
:- ensure_loaded(logicmoo('vworld/moo_startup')).

/*
% Load datalog
:- if_flag_true(fullStart, ((ensure_loaded(logicmoo('des/des.pl')),
  flush_output,
  init_des,
  display_status,
 %  des,
   !))).

*/

moo:agent_text_command(Agent,[run,Term], Agent,prologCall(Term)).


:- use_module(library(check)).
% :- at_start(check:list_undefined).

% GOLOG SYSTEM WITHOUT FLUX (Default Commented Out)
%:- if_flag_true(fullStart,ensure_loaded(logicmoo('indigolog/indigolog_main_swi.pl'))).

% FLUX AGENT SYSTEM WITHOUT GOLOG (Default Commented Out)
%:- if_flag_true(fullStart,ensure_loaded(logicmoo('indigolog/flux_main_swi.pl'))).

% FLUX AGENT SYSTEM WITH GOLOG
% :- if_flag_true(true,ensure_loaded(logicmoo('indigolog/indigolog_main_swi_flux.pl'))).

% LOGICMOO DATABASE LOGIC ENGINE SERVER
%:- if_flag_true(true,ensure_loaded(logicmoo('database/logicmoo.swi'))).

% when we import new and awefull code base (the previous )this can be helpfull
% we redfine list_undefined/1 .. this is the old version
lundef :- A = [],
       check:( merge_options(A, [module_class([user])], B),
        prolog_walk_code([undefined(trace), on_trace(found_undef)|B]),
        findall(C-D, retract(undef(C, D)), E),
        (   E==[]
        ->  true
        ;   print_message(warning, check(undefined_predicates)),
            keysort(E, F),
            group_pairs_by_key(F, G),
            maplist(report_undefined, G)
        )).

:- if_flag_true(fullStart,remove_undef_search).


/*
  ==
  ?- [library(mudconsole)].
  ?- mc_start.				% opens browser

   or else http_mud_server

  ?- mc_format('Hello ~w', [world]).
  ?- mc_html(p(['Hello ', b(world)])).
  ?- mc_ask([age(Age)], [p('How old are you'), input([name(age)])]).
  Age = 24.				% type 24 <enter>
  ==

*/

% [Optionaly] Put a telnet client handler on the main console
% :- at_start(login_and_run).
run_setup:-
   nodebug,
   debug,
   scan_db_prop,
   at_start(load_game(logicmoo('rooms/startrek.all.pl'))),
   register_timer_thread(npc_ticker,1,npc_tick_tock).

run:-
   login_and_run.

% LOGICMOO LOGICSERVER DATA (Defaut uncommented)
:- if_flag_true(fullStart, ensure_loaded(logicmoo('data/mworld0.pldata'))).

% :- if_flag_true(fullStart, load_game(logicmoo('rooms/startrek.all.pl'))).

%:- register_timer_thread(npc_ticker,30,npc_tick).

:- noguitracer.

% :- at_start(run).
%:- run_setup.

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

:- at_start(start_servers).

dyn:mud_test(test_movedist,
 (
  foc_current_player(P),
   test_name("teleport to main enginering"),
   do_player_action('tp Area1000'),
   test_true(req(atloc(P,'Area1000'))),
   test_name("set the move dist to 5 meters"),
   do_player_action('@set movedist 5'),
   test_name("going 5 meters"),
   do_player_action('n'),
   test_name("must be now be in corridor"),
   test_true(req(atloc(P,'Area1001'))),
   do_player_action('@set movedist 1'),
   call_n_times(5, do_player_action('s')),
   do_player_action('s'),
   test_name("must be now be back in engineering"),
   test_true(req(atloc(P,'Area1000'))))).

dyn:mud_test(drop_take,
 (
  do_player_action('drop food'),
  do_player_action('take food')
)).

%:- run_mud_tests.

:- at_start((debug,run_mud_tests)).

:- at_start((debug,run)).

% so scripted versions don't just exit
% :- at_start(prolog).

