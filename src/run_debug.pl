%!swipl -f
/** <module> An Implementation a MUD server in SWI-Prolog

*/

% Was this our startup file?
was_run_dbg_pl:-is_startup_file('run_debug.pl').

% :- catch(guitracer,_,true).
:- set_prolog_flag(verbose_load,true).

:- debug.

% run_tests includes run_common 
:-include(run_tests).

:- module(moo).

% [Optionaly] re-define load_default_game
% load_default_game:- load_game(logicmoo('rooms/startrek.all.plmoo')).

% [Optionaly] load and start sparql server
% starts in forground
%:- at_start(slow_work).
% starts in thread (the the above was commented out)
%:- at_start(start_servers).
% commented out except on run

start_boxer:-
   threads,
   ensure_loaded(logicmoo(candc/parser_boxer)),
   % make,   
   at_start(prolog_repl).

:-thread_local thlocal:test1234_TRUE/1.

thlocal:test1234_TRUE(5).

:-thread_local thlocal:test1234_OFF/1.

thlocal:test1234_OFF(_):-!,fail.

:-thread_local thlocal:test1234_FALSE/1.



:- export(tlocals/0).
:- module_transparent(tlocals/0).
tlocals:- 
   tlocals(false),
   tlocals(all).

tlocals(SHOW):- 
   doall((current_module(M),M\==thglobal,M\==thlocal,current_predicate(M:F/A),functor(P,F,A),predicate_property(M:P,thread_local),tlocal(M,F,A,SHOW))),
   doall(tlocal(thglobal,SHOW)),
   doall(tlocal(thlocal,SHOW)),!.

:- meta_predicate_transparent(tlocal/2).
tlocal(M,ON):- forall(current_predicate(M:F/A), tlocal(M,F,A,ON)).

:- meta_predicate_transparent(tlocal/4).
tlocal(M,F,A,ON):- functor(P,F,A), not(predicate_property(M:P,imported_from(_))), once((tlocal_0(M,P,ON,TF),must_det(tlocal_show(M,F,A,P,ON,TF)))).


:- meta_predicate_transparent(tlocal_0/4).
tlocal_0(M,P,ON,TF):- ccatch(tlocal_1(M,P,ON,TF),ERROR,(dmsg(ERROR),
       ((contains_var(error,ON);contains_var(all,ON)), 
         TF=' .???. '(M:P,ERROR)))),!.     

:- meta_predicate_transparent(tlocal_1/4).
tlocal_1(M,P,ON,TF):- '@'(M:call(M:P),M),!,
        (contains_var(on,ON),contains_var(true,ON);contains_var(all,ON)),!,
         TF=' .XXX. '(M:P),!.
tlocal_1(M,P,ON,TF):-  predicate_property(M:P,number_of_clauses(N)),N>0,!, 
        (contains_var(off,ON);contains_var(all,ON)),!,
         nth_clause(P, 1, Ref),clause(Head, Body, Ref),shrink_clause(Head,Body,FCL),
         TF=' .OFF. '(M:FCL),!.

:- meta_predicate_transparent(tlocal_2/4).
tlocal_2(M,P,ON,TF):- tlocal_1(M,P,ON,TF),!.
tlocal_2(M,P,ON,TF):-
        (contains_var(false,ON);contains_var(all,ON)),!,
         TF=' . - . '(M:P),!.

:- meta_predicate_transparent(shrink_clause/3).
shrink_clause(P,Body,Prop):- (Body==true-> Prop=P ; (Prop= (P:-Body))).
   
:- meta_predicate_transparent(tlocal_show/6).
tlocal_show(M,F,A,P,_ON,TF):-
   copy_term(P,PL),
   must_det((predicate_property(M:P,number_of_clauses(_)) -> findall(Prop,(clause(M:PL,Body),shrink_clause(PL,Body,Prop)),Props1);Props1=[no_clause_Access])),
   findall(' ++'(Prop),mpred_prop(F,Prop),Props2),
   findall(' -'(yes(Prop)),(predicate_property(M:P,Prop),not(member(Prop,[number_of_rules(0),number_of_clauses(0),/*thread_local,*/volatile,dynamic,visible,interpreted]))),Props3),
   findall(' -'(not(Prop)),(member(Prop,[number_of_clauses(_),thread_local,volatile,dynamic,visible,exported,interpreted]),not(predicate_property(M:P,Prop))),Props4),   
   flatten([[Props1],[Props2],[Props3],[Props4],[TF/A]],PropsR),
   numbervars(PropsR,0,_,[singletons(true),ignore(skip)]),
   reverse(PropsR,Props),
   fmt(Props),!.



:-export(prolog_repl/0).
prolog_repl:- nl,fmt("Press Ctrl-D to start the mud!"),nl,catch(tlocals,E,dmsg(tlocals==E)),prolog.


debug_repl(Module,CallFirst):- !,         
          with_assertions(thlocal:useOnlyExternalDBs,
            with_assertions(thglobal:use_cyc_database,
                ((user_ensure_loaded(logicmoo(parsing/parser_talk)),
                user_ensure_loaded(logicmoo(parsing/parser_chat80)),
                decl_type(person),          
                ensure_plmoo_loaded(logicmoo('rooms/startrek.all.plmoo')),
                module(Module),
                show_call(CallFirst), prolog_repl)))).

%  bug.. swi does not maintain context_module(CM) outside
%  of the current caller (so we have no idea what the real context module is!?!
debug_repl_m(Module,CallFirst):- 
        context_module(CM),
          call_cleanup(
            (module(Module),
              debug_repl(Module,CallFirst)),
            module(CM)).

% [Required] Defines debug80
debug80:- debug_repl(parser_chat80,t1).


% [Required] Defines debug_e2c
debug_e2c:- debug_repl(parser_e2c,cache_the_posms).


% [Required] Defines debug_talk
debug_talk:- debug_repl(parser_talk,t3).


% [Optional] This loads boxer
% :- at_start(with_assertions(moo:prevent_transform_moo_preds,within_user(ignore(catch(start_boxer,_,true))))).

% [Optional] Testing PTTP
% :-is_startup_file('run_debug.pl')->doall(do_pttp_test(_));true.


% [Manditory] This loads the game and initializes so test can be ran
:- if_flag_true(was_run_dbg_pl, at_start(run_setup)).

% [Optional] Interactively debug E2C
% :- debug_e2c.

% the local tests each reload (once)
now_run_local_tests_dbg :- doall(defined_local_test).
:- if_flag_true(was_run_dbg_pl, now_run_local_tests_dbg).

:-must_det(show_call((atloc('NpcCol1012-Ensign728',X),nonvar(X)))).

% nasty way i debug the parser
:- do_player_action('who').
% :-repeat, trace, do_player_action('who'),fail.


% :-do_player_action("scansrc").

% :-trace.



% [Optionaly] Tell the NPCs to do something every 30 seconds (instead of 90 seconds)
% :- register_timer_thread(npc_ticker,30,npc_tick).


% the real tests now (once)
:- if_flag_true(was_run_dbg_pl,at_start(must_det(run_mud_tests))).


% [Optionaly] Put a telnet client handler on the main console (nothing is executed past the next line)
:-do_player_action("look").

:-forall(localityOfObject(O,L),dmsg(localityOfObject(O,L))).

% :-forall(atloc(O,L),dmsg(atloc(O,L))).

% [Optionaly] Put a telnet client handler on the main console (nothing is executed past the next line)
:- if_flag_true(was_run_dbg_pl, at_start(run)).


% [Optionaly] Allows testing/debug of the chat80 system (withouyt loading the servers)
% :- debug80.

% So scripted versions don't just exit
:- if_flag_true(was_run_dbg_pl,at_start(prolog)).

%:- kill_term_expansion.
%:- prolog.


/*

PTTP input formulas:
  1  firstOrder(motherOf,joe,sue).
  2  not_firstOrder(motherOf,_,A);firstOrder(female,A).
  3  not_firstOrder(sonOf,B,A);firstOrder(motherOf,A,B);firstOrder(fatherOf,A,B).
  4  query:-firstOrder(female,_).
PTTP to Prolog translation time: 0.0028555670000001143 seconds

Prolog compilation time: 0.0004133299999997675 seconds
2.
Proof time: 4.34149999994915e-5 seconds
Proof:
length = 2, depth = 1
Goal#  Wff#  Wff Instance
-----  ----  ------------
  [0]    4   query :- [1].
  [1]    2      firstOrder(female,sue) :- [2].
  [2]    1         firstOrder(motherOf,joe,sue).
Proof end.
%                    succceeded(prove_timed(logicmoo_example1,query))
%                do_pttp_test(logicmoo_example1_holds)

PTTP input formulas:
  1  firstOrder(motherOf,joe,sue).
  2  not_firstOrder(motherOf,_,A);firstOrder(female,A).
  3  not_firstOrder(sonOf,B,A);firstOrder(motherOf,A,B);firstOrder(fatherOf,A,B).
  4  query:-firstOrder(female,_).
PTTP to Prolog translation time: 0.0024834679999994336 seconds

Prolog compilation time: 0.00039567500000003974 seconds
2.
Proof time: 3.7734999999372576e-5 seconds
Proof:
length = 2, depth = 1
Goal#  Wff#  Wff Instance
-----  ----  ------------
  [0]    4   query :- [1].
  [1]    2      firstOrder(female,sue) :- [2].
  [2]    1         firstOrder(motherOf,joe,sue).
Proof end.
%                    succceeded(prove_timed(logicmoo_example1_holds,query))
%                do_pttp_test(logicmoo_example2)


*/

