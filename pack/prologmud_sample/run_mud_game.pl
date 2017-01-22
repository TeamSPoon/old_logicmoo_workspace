#!/usr/bin/env swipl
/* * module  MUD server startup script in SWI-Prolog

*/
:- set_prolog_flag(autoload_logicmoo,false).
:- if( \+ current_module(prolog_stack)).
:- system:use_module(library(prolog_stack)).
 prolog_stack:stack_guard(none).
:- endif.

:- set_prolog_flag(report_error,true).
:- set_prolog_flag(debug_on_error,true).
:- set_prolog_flag(debugger_write_options,[quoted(true), portray(true), max_depth(1000), attributes(portray)]).
:- set_prolog_flag(generate_debug_info,true).

:-if((multifile(baseKB:ignore_file_mpreds/1),dynamic(baseKB:ignore_file_mpreds/1),
   (prolog_load_context(source,F1) -> asserta(baseKB:ignore_file_mpreds(F1)) ; true),
   (prolog_load_context(file,F) -> asserta(baseKB:ignore_file_mpreds(F)) ; true))).
:-endif.

:- user:ensure_loaded(setup_paths).
:- user:ensure_loaded(library(logicmoo_utils)).

:- set_prolog_flag(access_level,system).
:- lmce:current_smt(SM,M),writeln(current_smt(SM,M)).

:- op(300,fx,'-').

:- set_prolog_flag(access_level,user).

:-
 op(1190,xfx,('::::')),
 op(1180,xfx,('==>')),
 op(1170,xfx,'<==>'),
 op(1160,xfx,('<-')),
 op(1150,xfx,'=>'),
 op(1140,xfx,'<='),
 op(1130,xfx,'<=>'),
 op(600,yfx,'&'),
 op(600,yfx,'v'),
 op(350,xfx,'xor'),
 op(300,fx,'~'),
 op(300,fx,'-'),
 op(1199,fx,('==>')).


:- set_prolog_flag(dialect_pfc,false).
:- set_prolog_stack(global, limit(16*10**9)).
:- set_prolog_stack(local, limit(16*10**9)).
:- set_prolog_stack(trail, limit(16*10**9)).
:- set_prolog_flag(unsafe_speedups,true).
% ==========================================================
% Sanity tests that first run whenever a person stats the MUD to see if there are regressions in the system
% ==========================================================

:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )).
%:- lmce:reset_modules.
:- endif.


:- ignore(shell('./hmud/policyd')).

unsafe_preds_init(W,shell,2):-predicate_property(shell(_,_),imported_from(W)).
unsafe_preds_init(W,shell,1):-predicate_property(shell(_),imported_from(W)).
unsafe_preds_init(W,shell,0):-predicate_property(shell,imported_from(W)).
unsafe_preds_init(M,F,A):-M=files_ex,current_predicate(M:F/A),member(X,[delete,copy]),atom_contains(F,X).
unsafe_preds_init(M,F,A):-M=process,current_predicate(M:F/A),member(X,[kill,create]),atom_contains(F,X).
unsafe_preds_init(M,F,A):-M=system,member(F,[shell,halt]),current_predicate(M:F/A).

:- if(\+ compiling).
:- set_prolog_flag(access_level,system).

% [Optionaly] Solve the Halting problem
:-unlock_predicate(system:halt/0).
:-redefine_system_predicate(system:halt/0).
:-abolish(system:halt,0).
:-asserta((system:halt :- format('the halting problem is now solved!'))).
:-lock_predicate(system:halt/0).

:-unlock_predicate(system:halt/1).
:-redefine_system_predicate(system:halt/1).
:-abolish(system:halt,1).
:-asserta((system:halt(_) :- format('the halting problem is now solved!'))).
:-lock_predicate(system:halt/1).
kill_unsafe_preds:-(dmsg("kill_unsafe_preds!"),w_tl(set_prolog_flag(access_level,system),forall(unsafe_preds_init(M,F,A),bugger:remove_pred(M,F,A)))).
:- dmsg("the halting problem is now solved!").
:- set_prolog_flag(access_level,user).

:- if(exists_source(library(eggdrop))).
:- ensure_loaded(library(eggdrop)).
%:- egg_go.
:- endif.
:- use_listing_vars.
% :- [run].
:- endif.

% Loaded LogicMOO Code!!!
:- ensure_loaded(logicmoo_repl).

:- set_prolog_flag(unsafe_speedups,true).
%:- mpred_trace_exec.
:- debug.
% :- rtrace.

:- baseKB:ain(isa(iRR7,tRR)).
:- baseKB:ain(genls(tRR,tRRP)).
:- \+ baseKB:tRRP(iRR7) -> (xlisting(iRR7),xlisting(tRRP)) ; true.
:- must( baseKB:isa(iRR7,tRR) ).
:- must( baseKB:isa(iRR7,tRRP) ).
:- must( baseKB:tRRP(iRR7) ).

:- '$set_source_module'(baseKB).
:- '$set_typein_module'(baseKB).

:- listing((kb_dynamic)/1).
:- kb_dynamic(baseKB:sanity_test/0).
:- kb_dynamic(baseKB:regression_test/0).
:- kb_dynamic(baseKB:feature_test/0).
:- kb_dynamic((
        baseKB:feature_test/0,
        baseKB:mud_test/2,
        baseKB:regression_test/0,
        baseKB:sanity_test/0,
        baseKB:agent_call_command/2,
        action_info/2,
        type_action_info/3)).

:- statistics.
% :- gripe_time(40,baseKB:ensure_loaded(library(parser_e2c))).

:- set_prolog_flag(logicmoo_include,'$set_source_module'(baseKB)).
:- set_prolog_flag(pfc_booted,true).
%:- set_prolog_flag(retry_undefined,true).
:- set_prolog_flag(read_attvars,false).

% :- must((statistics(cputime,X),X<65)).  % was 52

ensure_webserver_3020:- find_and_call(ensure_webserver(3020)).
:- initialization(ensure_webserver_3020).
:- initialization(ensure_webserver_3020,now).
:- initialization(ensure_webserver_3020,restore).

:- assert_setting01(lmconf:eachRule_Preconditional(true)).

:- set_prolog_flag(dialect_pfc,false).

:- file_begin(pl).
:- ensure_loaded(logicmoo(logicmoo_engine)).



% :- user:ensure_loaded(load_).
started_mud_server.

:- push_modules.
% :- '$current_source_module'(user)->'$set_source_module'('baseKB');true.

% :- make.

% ==============================
% MUD SERVER CODE LOADS
% ==============================

% :- assert_setting01(lmconf:eachRule_Preconditional(isRuntime)).


:- push_modules.
% [Required] load the mud system
:- with_mpred_trace_exec(show_entry(gripe_time(40,ensure_loaded(prologmud(mud_loader))))).
%:- must(show_entry(gripe_time(40,ensure_loaded(prologmud(mud_loader))))).
:- lmce:reset_modules.

:- set_prolog_flag(logicmoo_debug,true).

% :- mpred_trace_exec.
:- ain(isLoaded(iSourceCode7)).
:- mpred_notrace_exec.

% ==============================
% MUD SERVER CODE STARTS
% ==============================

% ==============================
% MUD GAME CODE LOADS
% ==============================

% [Manditory] This loads the game and initializes so test can be ran
:- declare_load_dbase('../games/src_game_nani/a_nani_household.pfc.pl').

% [Optional] the following game files though can be loaded separate instead
:- declare_load_dbase('../games/src_game_nani/objs_misc_household.pfc.pl').
:- declare_load_dbase('../games/src_game_nani/?*.pfc.pl').

% [Optional] the following worlds are in version control in examples
% :- add_game_dir('../games/src_game_wumpus',prolog_repl).
% :- add_game_dir('../games/src_game_sims',prolog_repl).
% :- add_game_dir('../games/src_game_nani',prolog_repl).
% :- add_game_dir('../games/src_game_startrek',prolog_repl).
lst :- force_reload_mpred_file('../games/src_game_startrek/*.pfc.pl').

% ==============================
% MUD GAME REPL
% ==============================
% [Optionaly] Put a telnet client handler on the main console (nothing is executed past the next line)
:- if_startup_script(at_start(login_and_run)).
:- if_startup_script(initialization(login_and_run,restore)).

% So scripted versions don't just exit
:- if_startup_script(at_start(prolog)).

:- listing(is_in_world/1).

:- pop_modules.


