%!swipl -f
/** <module> An Implementation a MUD server in SWI-Prolog

*/

end_of_file.


% [Optionaly] load and start sparql server
% starts in forground
%:- at_start(slow_work).
% starts in thread (the the above was commented out)
%:- at_start(start_servers).
% commented out except on run

debug_repl_w_cyc(Module,CallFirst):- !,         
          with_assertions(thlocal:useOnlyExternalDBs,
            with_assertions(thglobal:use_cyc_database,
               ((decl_type(person),          
                ensure_plmoo_loaded(logicmoo('rooms/startrek.all.plmoo')),
                module(Module),
                show_call(CallFirst), 
                prolog_repl)))).

debug_repl_wo_cyc(Module,CallFirst):- !,         
          with_no_assertions(thlocal:useOnlyExternalDBs,
            with_assertions(thglobal:use_cyc_database,
               ((decl_type(person),          
                ensure_plmoo_loaded(logicmoo('rooms/startrek.all.plmoo')),
                module(Module),
                show_call(CallFirst), 
                prolog_repl)))).

%  bug.. swi does not maintain context_module(CM) outside
%  of the current caller (so we have no idea what the real context module is!?!
debug_repl_m(Module,CallFirst):- 
        context_module(CM),
          call_cleanup(
            (module(Module),
              debug_repl_wo_cyc(Module,CallFirst)),
            module(CM)).

% [Required] Defines debug80
debug80:- parser_chat80_module(M),debug_repl_wo_cyc(M,M:t1).

% [Optionaly] Allows testing/debug of the chat80 system (withouyt loading the servers)
% :- parser_chat80:t1.

% [Required] Defines debug_e2c
debug_e2c:- debug_repl_wo_cyc(parser_e2c,cache_the_posms).


% [Required] Defines debug_talk
debug_talk:- debug_repl_wo_cyc(parser_talk,t3).


% [Optional] This loads boxer
% :- at_start(with_assertions(prevent_transform_moo_preds,within_user(ignore(catch(start_boxer,_,true))))).

% [Optional] Testing PTTP
% :-is_startup_file('run_debug.pl')->doall(do_pttp_test(_));true.


% [Optional] Interactively debug E2C
% :- debug_e2c.


mud_test_local :- current_predicate(kellerStorage:kellerStorageTestSuite/0) -> kellerStorage:kellerStorageTestSuite ; true.

% :-curt80.


% the real tests now (once)
mud_test_local :- if_flag_true(was_run_dbg_pl,at_start(must_det(run_mud_tests))).

 % :- if_flag_true(was_run_dbg_pl, doall(now_run_local_tests_dbg)).


% [Optionaly] Allows testing/debug of the chat80 system (withouyt loading the servers)
% :- debug80.
/*

explorer(player1)> prolog statistics
notice(you,begin(you,mpred_call(statistics)))
statistics.
188.523 seconds cpu time for 282,024,744 inferences
1,004,265 atoms, 14,959 functors, 11,578 predicates, 176 modules, 268,104,937 VM-codes

                       Limit    Allocated       In use
Local  stack :137,438,953,472      126,976       41,032 Bytes
Global stack :137,438,953,472  805,302,256  669,634,856 Bytes
Trail  stack :137,438,953,472      129,016        2,448 Bytes

1 garbage collections gained 41,528 bytes in 0.000 seconds.
2 atom garbage collections gained 19,741 atoms in 1.360 seconds.
Stack shifts: 4 local, 22 global, 20 trail in 0.038 seconds.
2 threads, 0 finished threads used 0.000 seconds.
true.

cmdresult(statistics,true)

*/

% :- kill_term_expansion.
% :- slow_work.
% :- prolog.
% :- now_run_local_tests_dbg.
% :- prolog.

% :-foc_current_player(P),assertz_if_new(thglobal:player_command_stack(P,chat80)).
:- if_flag_true(was_run_dbg_pl, at_start(run)).


% So scripted versions don't just exit
%:- if_flag_true(was_run_dbg_pl,at_start(prolog)).

%:- kill_term_expansion.
%:- prolog.

% :-proccess_command_line.

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

