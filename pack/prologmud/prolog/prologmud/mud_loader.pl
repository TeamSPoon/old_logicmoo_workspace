/** <module> 
% This file loads the world (world.pl), the map of the world, 
% the agents and their definitions.
% This file is used as a configuation file and a startup script.
%
% July 10,1996
% John Eikenberry
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/


:- dynamic   user:file_search_path/2.
:- multifile user:file_search_path/2.

:- op(200,fy,(-)).

:- set_prolog_flag(verbose_load,true).


:- include(mud_header).

:- retractall(t_l:disable_px).

:- set_prolog_flag(generate_debug_info, true).
% [Optionaly] Set the Prolog optimize/debug flags
%:- set_prolog_flag(verbose_load,true).
%:- use_module(library(gui_tracer)).
%:- set_prolog_flag(gui_tracer, false).
%:- set_prolog_flag(answer_write_options, [quoted(true), portray(true), max_depth(1000), spacing(next_argument)]).
%:- catch(noguitracer,_,true).


% [Optionaly] Set up the Prolog optimize/debug flags
%:- set_prolog_flag(debug,false).
%:- set_optimize(true).


% [Optionaly] load the mpred_online system
% :- if_file_exists(user:ensure_loaded(library(logicmoo/mpred_online))).

:- prolog_load_context(directory,Dir),asserta(user:file_search_path(prologmud,Dir)).

% xyzFn(R,X,Y,Z):-dmsg(xyzFn(R,X,Y,Z)),trace_or_throw(xyzFn(R,X,Y,Z)).

% :- multifile prolog:message/3.
% prolog:message(git(update_versions),A,A):-!.

:- use_module(library(settings)).
% :- use_module(library(check)).
% :- make.
%:- portray_text(true).

:- asserta(t_l:disable_px).

:-set_prolog_stack(global, limit(32*10**9)).
:-set_prolog_stack(local, limit(32*10**9)).
:-set_prolog_stack(trail, limit(32*10**9)).


:- multifile( entailment:rdf /3 ).

% [Optionaly] Solve the Halting problem
:-use_module(library(process)).
% :-use_module(library(pce)).
:- has_gui_debug -> true ; remove_pred(pce_principal,send,2).
:- has_gui_debug -> true ; remove_pred(pce_principal,new,2).


unsafe_preds(M,F,A):-M=files_ex,current_predicate(M:F/A),member(X,[delete,copy]),atom_contains(F,X).
unsafe_preds(M,F,A):-M=process,current_predicate(M:F/A),member(X,[kill,create]),atom_contains(F,X).
unsafe_preds(M,F,A):-M=system,member(F,[shell,halt]),current_predicate(M:F/A).

:-forall(unsafe_preds(M,F,A),bugger:remove_pred(M,F,A)).

% [Optionaly] Solve the Halting problem
:-unlock_predicate(system:halt/0).
:-redefine_system_predicate(system:halt/0).
:-abolish(system:halt,0).
:-asserta((system:halt :- format('the halting problem is now solved!'))).
:-lock_predicate(system:halt/0).

:- dmsg('the halting problem is now solved!').

:- file_begin(prolog).

:- '$set_source_module'('user').

:- asserta(t_l:disable_px).

add_game_dir(GAMEDIR,Else):- add_to_search_path_first(game, GAMEDIR),now_try_game_dir(Else).


now_try_game_dir(Else):-  
 enumerate_files(game('.'), GAMEDIR) *-> 
  ((exists_directory(GAMEDIR) -> 
    with_all_dmsg(( 
      % forall(enumerate_files(game('**/*.pl'),X),user:ensure_loaded(X)),
      forall(no_repeats_old(X,enumerate_files(game('**/*.plmoo'),X)),declare_load_dbase(X)))); (fmt(missing(GAMEDIR)),Else)));  (fmt(no_game_dir),Else).


:-context_module(CM),assert(loading_from_cm(CM)).
create_module(M):-context_module(CM),module(M),asserta(M:this_is_a_module(M)),writeq(switching_back_to_module(M,CM)),module(CM).
:-create_module(user).
:-create_module(hook).
:-create_module(t_l).
:-create_module(thglobal).
%:-create_module(moo).


:-module_transparent parser_chat80_module/1.
:-multifile parser_chat80_module/1.
:-export((parser_chat80_module/1)).
parser_chat80_module(moo).



:-export(prolog_repl/0).
prolog_repl:- with_all_dmsg((nl,fmt("Press Ctrl-D to start the mud!"),nl,'@'(prolog,'user'))).

%:- set_prolog_flag(gui,false).
%:- set_prolog_flag(history,1000).

% :- prolog_ide(debug_monitor),prolog_ide(open_debug_status). % ,prolog_ide(xref).

%:- debug(wotp).


:-export(within_user/1).
:-export(is_startup_file/1).

is_startup_file(Name):- current_prolog_flag(os_argv,ArgV),member(Named,ArgV),atom(Named),atom_concat(Name,_,Named),!.

within_user(Call):- '@'(Call,'user').

% ======================================================
% Configure the logicmoo utilities into the file path
% :- include('logicmoo_util/logicmoo_util_header').
% :- user:use_module('logicmoo_util/logicmoo_util_all.pl').
% And adds the local directories to file search path of logicmoo(..)
% ======================================================


:- dynamic   user:file_search_path/2.
:- multifile user:file_search_path/2.

:- user:use_module(library(settings)).

:- ignore((user:file_search_path(cliopatria,SP),
   exists_directory(SP),!,
   writeq(user:file_search_path(cliopatria,SP)),nl)).
   %set_setting_default(cliopatria_binding:path, SP).
   %save_settings('moo_settings.db').
   %%setting(cliopatria_binding:path, atom, SP, 'Path to root of cliopatria install'),!.

% :- user:use_module(logicmoo('http/user_page')).

:- meta_predicate(if_version_greater(?,0)).

if_version_greater(V,Goal):- current_prolog_flag(version,F), ((F > V) -> call(Goal) ; true).

% set to false because we don't want to use the mudconsole
:- if_flag_true(false, if_version_greater(70109,user:use_module(logicmoo('mudconsole/mudconsolestart')))).

% [Optionaly 1st run] tell where ClioPatria is located and restart for the 2nd run
%:- set_setting(cliopatria_binding:path, '/devel/ClioPatria'), save_settings('moo_settings.db').

start_boxer:-
   threads,
   ensure_loaded(logicmoo(candc/parser_boxer)),
   % make,   
   at_start(prolog_repl).


% We don't start cliopatria we here. We have to manually start
%  with  ?- start_servers.
hard_work:-!.
hard_work:-
   with_no_mpred_expansions(wno_tl(op(200,fy,'@'),
   ((
 %  use_module('t:/devel/cliopatria/rdfql/sparql_runtime.pl'),
  % ensure_loaded(logicmoo(launchcliopatria)),
  % ensure_loaded(logicmoo(testwebconsole)),
  % kill_term_expansion, 
   ensure_loaded(swish(logicmoo_run_swish))
   )))),!.

% [Required] load the mud PFCs
:- show_entry(gripe_time(40,user:ensure_loaded(prologmud(server/mud_builtin)))).


slow_work:- wno_tl( prevent_transform_moo_preds , within_user(at_start(hard_work))).

thread_work:- thread_property(X, status(running)),X=loading_code,!.
thread_work:- thread_create(slow_work,_,[alias(loading_code)]).

% start_servers :- if_version_greater(70111,thread_work).
start_servers :- if_version_greater(70111,slow_work).

run_setup_now:-
   within_user((
      finish_processing_world      
   % TO UNDO register_timer_thread(npc_ticker,90,npc_tick)
   )).

run_setup:- within_user(at_start(run_setup_now)).




% [Optionaly] load and start sparql server
% starts in forground
%:- at_start(slow_work).
% starts in thread (the the above was commented out)
%:- at_start(start_servers).
% commented out except on run

debug_repl_w_cyc(Module,CallFirst):- !,         
          wno_tl(t_l:useOnlyExternalDBs,
            w_tl(thglobal:use_cyc_database,
               ((decl_type(person),          
                ensure_mpred_file_loaded(logicmoo('rooms/startrek.all.plmoo')),
                module(Module),
                show_call(CallFirst), 
                prolog_repl)))).

debug_repl_wo_cyc(Module,CallFirst):- !,         
          w_tl(t_l:useOnlyExternalDBs,
            wno_tl(thglobal:use_cyc_database,
               ((decl_type(person),          
                ensure_mpred_file_loaded(logicmoo('rooms/startrek.all.plmoo')),
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
% :- at_start(w_tl(prevent_transform_moo_preds,within_user(ignore(catch(start_boxer,_,true))))).

% [Optional] Testing PTTP
% :-is_startup_file('run_debug.pl')->doall(do_pttp_test(_));true.

% Was this our startup file?
was_runs_tests_pl:-is_startup_file('run_tests.pl').

% [Optional] Interactively debug E2C
% :- debug_e2c.


mud_test_local :- current_predicate(kellerStorage:kellerStorageTestSuite/0) -> kellerStorage:kellerStorageTestSuite ; true.

% :-curt80.


% the real tests now (once)
mud_test_local :- if_flag_true(was_runs_tests_pl,at_start(must_det(run_mud_tests))).

 % :- if_flag_true(was_runs_tests_pl, doall(now_run_local_tests_dbg)).


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

% :-foc_current_agent(P),assertz_if_new(agent_action_queue(P,chat80)).
:- if_flag_true(was_runs_tests_pl, at_start(login_and_run)).


% So scripted versions don't just exit
%:- if_flag_true(was_runs_tests_pl,at_start(prolog)).

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


% standard header used in all files that all modules are loaded (therefore useful for when(?) the day comes that modules *can*only*see their explicitly imported modules)
%:- prolog_flag(unknown,error,fail). % Not sure if this is needed for Quintus
%:- use_module(library(random)).
%:- use_module(library(date)).
% This one is for use with SWI
:- use_module(library(quintus)).


% logicmoo utils shared with other systems
:- set_prolog_flag(double_quotes, atom).
:- set_prolog_flag(double_quotes, string).


% logicmoo vworld mud server

:- include(mud_loader_pt2).
