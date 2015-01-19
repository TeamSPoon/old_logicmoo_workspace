:- catch(noguitracer,_,true).

:-include(run_common).

% define tests locally

mud_test(test_movedist,
 (
  foc_current_player(P),
   test_name("teleport to main enginering"),
   do_player_action('tp self to Area1000'),
  test_name("now we are really somewhere"),
   test_true(req(atloc(P,_Somewhere))),
  test_name("in main engineering?"),
   test_true(req(localityOfObject(P,'Area1000'))),
   test_name("set the move dist to 5 meters"),
   do_player_action('@set mudMoveDist 5'),
   test_name("going 5 meters"),
   % gets use out of othre NPC path
   do_player_action('move e'),
   do_player_action('move n'),
   test_name("must be now be in corridor"),
   test_true(req(localityOfObject(P,'Area1002'))),
   do_player_action('@set mudMoveDist 1'),
   call_n_times(5, do_player_action('s')),
   do_player_action('move s'),
   test_name("must be now be back in engineering"),
   test_true(req(localityOfObject(P,'Area1000'))))).

mud_test_level2(create_gensym_named,
  with_all_dmsg(((do_player_action('create food999'),
  foc_current_player(P),
  must(( req(( possess(P,Item),isa(Item,food))))))))) .

mud_test_level2(drop_take,
  with_all_dmsg(((do_player_action('create food'),
  do_player_action('drop food'),
  do_player_action('take food'),
  do_player_action('eat food'))))).


% Was this our startup file?
was_runs_tests_pl:-is_startup_file('run_tests.pl').

:- catch(noguitracer,_,true).

% [Optionally] load and start sparql server
%:- at_start(start_servers)

% [Optionaly] Add some game content
:- if_flag_true(was_runs_tests_pl, declare_load_game(logicmoo('rooms/startrek.all.plmoo'))).

mud_test_local:-
   test_name("tests to see if we have: player1"),
   test_true(show_call(foc_current_player(_Agent))).

mud_test_local:-
   test_name("tests to see if we have: atloc"),
   test_true((foc_current_player(Agent),show_call(atloc(Agent,_Where)))).

mud_test_local:- 
   test_name("tests to see if we have: localityOfObject"),
   test_true((foc_current_player(Agent),show_call(localityOfObject(Agent,_Where)))).

mud_test_local:- 
   test_name("tests to see if our clothing doesnt: atloc"),
   test_false(atloc('ArtifactCol1003-Gold-Uniform775',_X)).
    
mud_test_local:- 
   foc_current_player(Agent),
   test_name("tests to see if we have: argIsas on charge"),
   test_true(correctArgsIsa(charge(Agent,_),_)).

mud_test_local:- 
   test_name("tests to see if we have: singleValued on mudMoveDist"),
   must(add(mudMoveDist(explorer(player1),3))),
   test_true(must((findall(X,mudMoveDist(explorer(player1),X),L),length(L,1)))).

mud_test_local:- 
      test_name("nudity test"), 
       test_true_req(wearsClothing(explorer(player1), 'ArtifactCol1003-Gold-Uniform775')).

mud_test_local:- 
      test_name("genlInverse test"), 
       test_true_req(possess(explorer(player1), 'ArtifactCol1003-Gold-Uniform775')).

mud_test_local:- 
   test_name("Tests our action templates"), doall((get_type_action_templates(Templates),dmsg(get_type_action_templates(Templates)))).

mud_test_local:-
   test_name("tests to see if 'food' can be an item"),
      test_true(parseIsa0(item, _, [food], [])).

mud_test_local:-call_mpred(cmdShowRoomGrid('Area1000')).


% more tests even
mud_test_local :-do_player_action("look").
mud_test_local :-forall(localityOfObject(O,L),dmsg(localityOfObject(O,L))).

% ---------------------------------------------------------------------------------------------
mud_test_local:-
  test_name("Tests our types to populate bad_instance/2 at level 5"),
  retractall(is_instance_consistent(_,_)),
  retractall(bad_instance(_,_)),
  forall(subclass(T,spatialthing),check_consistent(T,1000)),
  listing(bad_instance/2).

:-thread_local thlocal:is_checking_instance/1.

:-decl_mpred_prolog(user:check_consistent(term,int)).
:-decl_mpred_prolog(user:is_instance_consistent(term,int)).
:-decl_mpred_prolog(user:bad_instance(term,why)).
:-decl_mpred_prolog(user:is_checking_instance(term)).

check_consistent(Obj,Scope):-var(Scope),!,check_consistent(Obj,0).
check_consistent(Obj,Scope):-is_instance_consistent(Obj,Was),!,Was>=Scope.
check_consistent(Obj,_):- thlocal:is_checking_instance(Obj),!.
check_consistent(Obj,Scope):- with_assertions(thlocal:is_checking_instance(Obj),doall(check_consistent_0(Obj,Scope))).
check_consistent_0(Obj,Scope):- once((catchv((doall(((clause(hook:hooked_check_consistent(Obj,AvScope),Body),once(var(AvScope); (AvScope =< Scope) ),Body))),assert_if_new(is_instance_consistent(Obj,Scope))),E,assert_if_new(bad_instance(Obj,E))))),fail.
check_consistent_0(Type,Scope):- once(type(Type)),
 catchv((forall(isa(Obj,Type),check_consistent(Obj,Scope)),
                                                   assert_if_new(is_instance_consistent(Type,Scope))),E,assert_if_new(bad_instance(Type,E))),fail.

hook:hooked_check_consistent(Obj,20):-must(object_string(_,Obj,0-5,String)),dmsg(checked_consistent(object_string(_,Obj,0-5,String))).
% ---------------------------------------------------------------------------------------------


% mud_test("local sanity tests", doall(mud_test_local)).


:- moo_hide_childs(dbase:record_on_thread/2).

% [Manditory] This loads the game and initializes so test can be ran
:- if_flag_true(was_runs_tests_pl, at_start(run_setup)).

% the real tests now (once)
now_run_local_tests:- doall(mud_test_local).
:- if_flag_true(was_runs_tests_pl,at_start(must_det(run_mud_tests))).

% the local tests each reload (once)
:- if_flag_true(was_runs_tests_pl, now_run_local_tests).

% halt if this was the script file
:- if_flag_true(was_runs_tests_pl, halt).



% the local tests each reload (once)
now_run_local_tests_dbg :- doall(mud_test_local).

% nasty way i debug the parser
% :-repeat, trace, do_player_action('who'),fail.
mud_test_local :- do_player_action('who').

% mud_test_local :-do_player_action("scansrc").

% more tests even
mud_test_local :-do_player_action("look").
mud_test_local :-forall(localityOfObject(O,L),dmsg(localityOfObject(O,L))).

must_test("tests to see if poorly canonicalized code (unrestricted quantification) will not be -too- inneffienct",
   forall(atloc(O,L),fmt(atloc(O,L)))).

% the real tests now (once)
mud_test_local :- at_start(must_det(run_mud_tests)).


end_of_file.




40 thousand line codebase

100 * 360+180 


I am not some great prolog proggrammer.. butt the opposite and needed to code like i was


so i've continue to use these cetain practices
At the time I also developed a web based testing based framework as well (this was back before swi proliog had a built in webserver) 
But it is just natural events.. later on I'll expantin its usage as well

must/1  - code must succeed semi deterministally

must_det/1 - must leave no coice points behind after the body completes


dumpST


rtrace/1 tracceing must_det/1

ftrace/1 tracing must/1

warnOnErrors/1  keep going but put recogniable garbage into variables 


even in i parser library .. i am seeing people have to awakwardly pass arround state that is not releated to the DCG  they are parsing .. and that state cannot always be created in the same order as the sentence
