:- catch(noguitracer,_,true).

:-include(run_common).

% define tests locally
moo:mud_test(drop_take,
 (
  do_player_action('drop food'),
  do_player_action('take food')
)).

moo:mud_test(test_movedist,
 (
  foc_current_player(P),
   test_name("teleport to main enginering"),
   do_player_action('tp Area1000'),
   test_true(req(atloc(P,_Somewhere))),
   test_true(req(inRegion(P,'Area1000'))),
   test_name("set the move dist to 5 meters"),
   do_player_action('@set movedist 5'),
   test_name("going 5 meters"),
   do_player_action('move n'),
   test_name("must be now be in corridor"),
   test_true(req(inRegion(P,'Area1001'))),
   do_player_action('@set movedist 1'),
   call_n_times(5, do_player_action('s')),
   do_player_action('move s'),
   test_name("must be now be back in engineering"),
   test_true(req(inRegion(P,'Area1000'))))).


% Was this our startup file?
was_runs_tests_pl:-is_startup_file('run_tests.pl').

% [Optionally] load and start sparql server
%:- at_start(start_servers)

% [Optionaly] Add some game content
:- if_flag_true(was_runs_tests_pl, add_game_content(logicmoo('rooms/startrek.all.pl'))).

defined_local_test:-
   test_name("tests to see if we have: player1"),
   test_true(must_det((foc_current_player(Agent),dmsg(foc_current_player(Agent))))).

defined_local_test:-
   test_name("tests to see if we have: atloc"),
   test_true((foc_current_player(Agent),must_det((atloc(Agent,Where)),dmsg(atloc(Agent,Where))))).

defined_local_test:- 
   test_name("tests to see if we have: singleValued on movedist"),
   must((
   add(movedist(explorer(player1),3))
   ,
   findall(X,movedist(explorer(player1),X),L),length(L,1))).

defined_local_test_failing:-
   test_name("tests to see if we have: inRegion"),
   test_true((foc_current_player(Agent),must_det((inRegion(Agent,Where))),dmsg(inRegion(Agent,Where)))).

moo:mud_test("local sanity tests", doall(defined_local_test)).

:- moo_hide_childs(dbase:record_on_thread/2).

% [Manditory] This loads the game and initializes so test can be ran
:- if_flag_true(was_runs_tests_pl, at_start(run_setup)).

% the real tests now (once)
now_run_local_tests:- doall(defined_local_test).
:- if_flag_true(was_runs_tests_pl,at_start(must_det(run_mud_tests))).

% the local tests each reload (once)
:- if_flag_true(was_runs_tests_pl, now_run_local_tests).

% [Optionaly] Put a telnet client handler on the main console (nothing is executed past the next line)
:- if_flag_true(was_runs_tests_pl, at_start(run)).

% So scripted versions don't just exit
:- if_flag_true(was_runs_tests_pl,at_start(prolog)).



