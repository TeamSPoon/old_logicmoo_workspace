% #! swipl -L8G -G8G -T8G -f
/** <module> MUD server startup script in SWI-Prolog

*/


:- user:ensure_loaded(run_mud_server).

started_mud_server.

% ==============================
% MUD GAME CODE LOADS
% ==============================

% [Manditory] This loads the game and initializes so test can be ran
:- declare_load_dbase('../games/src_game_nani/a_nani_household.plmoo').


% [Never] saves about a 3 minute compilation time (for when not runing mud)
:- if((fail,gethostname(titan),fail)).
:- if_startup_script( finish_processing_world).
:- enqueue_agent_action("rez crackers").
%:- prolog.
:- endif.


% [Optional] the following game files though can be loaded separate instead
:- declare_load_dbase('../games/src_game_nani/objs_misc_household.plmoo').
:- declare_load_dbase('../games/src_game_nani/?*.plmoo').
% [Optional] the following worlds are in version control in examples
% :- add_game_dir('../games/src_game_wumpus',prolog_repl).       
% :- add_game_dir('../games/src_game_sims',prolog_repl).
% :- add_game_dir('../games/src_game_nani',prolog_repl).       
:- add_game_dir('../games/src_game_startrek',prolog_repl).

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

% ==============================
% MUD GAME REPL 
% ==============================
% [Optionaly] Put a telnet client handler on the main console (nothing is executed past the next line)
:- if_startup_script(at_start(login_and_run)).
:- initialization(login_and_run).

% So scripted versions don't just exit
:- if_startup_script(at_start(prolog)).



