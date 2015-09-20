% #! swipl -L8G -G8G -T8G -f
/** <module> Logicmoo_base sanity test script to be ran before a release

*/

:- initialization(attach_packs).

:-if(\+(exists_source(logicmoo(logicmoo_base)))).
:- dynamic   user:file_search_path/2.
:- multifile user:file_search_path/2.

:- if(exists_directory(runtime)).
user:file_search_path(pack, './pack').
:- else.
user:file_search_path(pack, '../pack').
:- endif.

:- attach_packs.
:- user:ensure_loaded(library(logicmoo/util/logicmoo_util_all)).
:- endif.

:- initialization(attach_packs).

:- asserta(thlocal:verify_side_effect_buffer).

:- asserta(user:load_mud_www).
:- user:ensure_loaded(library(logicmoo/logicmoo_base)).
:- gripe_time(40,user:ensure_loaded(logicmoo(mpred_online/logicmoo_i_www))).
:- ensure_webserver.

% [Optionaly] Load an Eggdrop (Expects you have  Eggdrop runinng with PROLOG.TCL scripts @ https://github.com/TeamSPoon/MUD_ircbot/)
:- if_file_exists(user:ensure_loaded(library(eggdrop))).
:- eggdrop:egg_go.
:- initialization((current_predicate(egg_go/0)->egg_go;true),now).

/*
% [Required] Load the Logicmoo WWW System
:- (if_file_exists(user:ensure_loaded(library(logicmoo/logicmoo_run_pldoc)))).
:- (if_file_exists(user:ensure_loaded(library(logicmoo/logicmoo_run_clio)))).
:- (if_file_exists(user:ensure_loaded(library(logicmoo/logicmoo_run_swish)))).
*/






% :- statistics(globallimit,G),statistics(locallimit,L),statistics(traillimit,T),qsave_program(logicmoo_repl,[map('logicmoo_repl.sav'),global(G),trail(T),local(L)]).

:- initialization(list_undefined).

:- tell(blalla).

:-listing(side_effect_buffer/3).

:-forall(actual_side_effect(H,B),(nl,portray_clause(H:-B))).

:-listing(side_effect_buffer/3).

:- told.

:- initialization(list_undefined).

% [Required] Load the Logicmoo Backchaining Inference System
:- gripe_time(40,with_no_mpred_expansions(if_file_exists(user:ensure_loaded(logicmoo(logicmoo_engine))))).



:- dynamic  el_assertions:el_holds/4.
:- dynamic  el_assertions:el_holds/10.
:- dynamic  el_assertions:el_holds/11.
:- dynamic  el_assertions:el_holds/12.
:- dynamic  el_assertions:el_holds/13.
:- dynamic  el_assertions:el_holds/14.

:- rl_add_history('help(match_regex/2).').
:- rl_add_history('list_undefined.').

:- user:ensure_loaded(library(logicmoo/logicmoo_plarkc)).
:- initialization(list_undefined).
:- prolog.

:- user:ensure_loaded(library(logicmoo/logicmoo_planner)).
:- initialization(list_undefined).
:- prolog.

:- user:ensure_loaded(run_mud_server).
:- initialization(list_undefined).
:- prolog.


:- user:ensure_loaded(init_mud_server).


% [Never] saves about a 3 minute compilation time (for when not runing mud)
:- if((fail,gethostname(titan),fail)).
:- if_startup_script( finish_processing_world).
:- enqueue_agent_action("rez crackers").
%:- prolog.
:- endif.


%:- set_prolog_flag(trace_gc,false).
%:- set_prolog_flag(backtrace_depth,400).


% [Manditory] This loads the game and initializes so test can be ran
:- if_startup_script(finish_processing_world).

% user:sanity_test:- rescan_pfc.

%:- rescan_pfc. 
:-dmsg("About to run Sanity").
%:- prolog.

:- show_call_entry(gripe_time(40,if_startup_script(doall(user:sanity_test)))).

%:- prolog.

feature_testp1:- forall(parserTest(Where,String),assert_text(Where,String)).

:- if((fail,gethostname(titan))).

% :-feature_testp1.

% [Optionaly] Run a battery of tests
% :- if_startup_script( doall(now_run_local_tests_dbg)).

% [Optionaly] Run a battery of tests
% :- if_startup_script( doall(user:regression_test)).


sanity_test0a:- enqueue_agent_action("hide").

sanity_test0b:- enqueue_agent_action(actWho).
:-sanity_test0b.

sanity_test1:-   
   enqueue_agent_action("rez crackers"),
   enqueue_agent_action("drop crackers"),
   enqueue_agent_action('look'),
   enqueue_agent_action("take crackers"),
   enqueue_agent_action("eat crackers"),
   enqueue_agent_action('look').
:-sanity_test1.

sanity_test2:- enqueue_agent_action("rez pants"),
   enqueue_agent_action("wear pants"),
   enqueue_agent_action("tp to closet"),
   enqueue_agent_action("take shirt"),
   enqueue_agent_action("inventory").

:-sanity_test2.


% [Optionaly] Tell the NPCs to do something every 60 seconds (instead of 90 seconds)
% :- register_timer_thread(npc_ticker,60,npc_tick).

:- show_call_entry(gripe_time(40,user:ensure_loaded(prologmud(mud_startup)))).


:- pce_show_profile.

:-endif.  % MUD TESTS
% :- enqueue_agent_action(prolog).

:- initialization(list_undefined).
:- prolog.

:- user:ensure_loaded(library(logicmoo/parser_all)).
:- initialization(list_undefined).


:- user:ensure_loaded(run_mud_game).

