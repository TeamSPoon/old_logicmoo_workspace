:- catch(noguitracer,_,true).

:-include(run_common).

% [Optionally] load and start sparql server
%:- at_start(start_servers)

% [Optionaly] re-define load_default_game
% load_default_game:- load_game(logicmoo('rooms/startrek.all.pl')).

% [Manditory] This loads the game and intializes
:- at_start(run_setup).

% tests to see if we have: "player1"
:-must_det((foc_current_player(Agent),dmsg(foc_current_player(Agent)))).

% tests to see if we have: "atloc"
:-foc_current_player(Agent),must_det((atloc(Agent,Where))),dmsg(atloc(Agent,Where)).

% tests to see if we have: "inRegion"
:-foc_current_player(Agent),must_det((inRegion(Agent,Where))),dmsg(inRegion(Agent,Where)).

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


:- at_start((debug,must_det(run_mud_tests))).


svTest :- must((
   trace,add(movedist(explorer(player1),4))
   ,
   findall(X,movedist(explorer(player1),X),L),length(L,1))).

:-moo_hide_childs(dbase:record_on_thread/2).

% [Optionaly] Put a telnet client handler on the main console (nothing is executed past the next line)
% :- at_start(run).

% So scripted versions don't just exit
% :- at_start(prolog).



