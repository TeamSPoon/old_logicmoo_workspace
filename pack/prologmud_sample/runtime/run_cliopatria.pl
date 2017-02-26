#!/usr/bin/swipl 


:- multifile(prolog:make_hook/2).
:- dynamic(prolog:make_hook/2).

:- if(\+ current_module(baseKB)).
:- threads.
:- set_prolog_flag(logicmoo_qsave,true).
:- else.
:- set_prolog_flag(logicmoo_qsave,false).
:- set_prolog_flag(lm_expanders,false).
:- statistics.
:- endif.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DEFAULT PROLOG FLAGS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- '$set_source_module'(user).
:- set_prolog_flag(lm_expanders,false).
:- set_prolog_flag(qcompile,part).
:- set_prolog_flag(do_renames,never).
:- set_prolog_flag(dialect_pfc,false).
:- set_prolog_stack(global, limit(32*10**9)).
:- set_prolog_stack(local, limit(32*10**9)).
:- set_prolog_stack(trail, limit(32*10**9)).
:- set_prolog_flag(double_quotes,string).
:- if( \+ current_module(prolog_stack)).
:- system:use_module(library(prolog_stack)).
 prolog_stack:stack_guard(none).
:- endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LOAD LOGTALK
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- user:ensure_loaded('/usr/share/logtalk/integration/logtalk_swi').
:- listing('$lgt_default_flag'/2).

:- unload_file(logicmoo_repl).
:- set_prolog_flag(lm_expanders,false).
%:- use_module(library('logicmoo/util/logicmoo_util_first')).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LOAD PARTS OF SYSTEM EARLY
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- '$set_typein_module'(baseKB).
:- '$set_source_module'(baseKB).
/*
:- set_prolog_flag(access_level,system).
:- set_prolog_flag(compile_meta_arguments,false). % default is false
*/
:- use_module(library(base32)).
% :- use_module(library(prolog_history)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/thread_httpd)).
:- use_module(thread_httpd:library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(threadutil)).
:- use_module(library(shell)).
:- use_module(library(console_input)).
:- use_module(library(editline)).
:- if(current_predicate(system:mode/1)).
:- system:use_module(library(quintus),except([mode/1])). 
:- else.
:- system:use_module(library(quintus)). 
:- endif.
:- system:use_module(library(dialect/ifprolog),except([op(_,_,_)])).
:- abolish(system:time/1).
:- system:use_module(library(dialect/hprolog)).
:- abolish(hprolog:time/1).
:- system:use_module(library(statistics),[time/1]).
:- system:use_module(library(statistics)).
:- baseKB:use_module(library(statistics),[time/1]).
:- autoload([verbose(false)]).
add_history(O):- format(atom(A),'~q.',[O]),
      (current_prolog_flag(readline,editline) -> el_add_history(user_input,A) ; rl_add_history(A)).

add_history_ideas:- 
        add_history(start_telnet),
        add_history(help(match_regex/2)),
        add_history(list_undefined),
        add_history(listing(after_boot_goal/1)),
	add_history(ensure_loaded(run_mud_game)),
	add_history(statistics),
        add_history(threads),
        add_history(qsave_lm(lm_repl)),        
        add_history(after_boot_call(must_det)),
        add_history(after_boot_call),
        add_history(make),        
        add_history(mmake),
        add_history(login_and_run),
        add_history([logicmoo_repl]),
        forall(system:after_boot_goal(G),add_history(G)),
        add_history([init_mud_server]),
        !.
        

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DURING/AFTER BOOT HOOKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic(system:after_boot_goal/1).
:- meta_predicate(system:during_boot(:)).
system:during_boot(Goal):- initialization(Goal,now),after_boot(Goal). % initialization(Goal,after_load),initialization(Goal,restore)
system:after_boot_call(How):- forall(system:after_boot_goal(Goal),call(How,Goal)).
system:after_boot_call:-system:after_boot_call(must_det).
:- meta_predicate(system:after_boot(:)).
system:after_boot(Goal):- add_history(Goal),system:assertz(after_boot_goal(Goal)).
:- meta_predicate(system:after_boot_sanity_test(:)).
system:after_boot_sanity_test(M:Goal):- after_boot(M:sanity(Goal)).

:- during_boot(add_history_ideas->!;(trace,add_history_ideas)).

qsave_lm(LM):- statistics(globallimit,G),statistics(locallimit,L),statistics(traillimit,T),
  qsave_program(LM,[class(development),autoload(false),toplevel(logicmoo_toplevel),goal(logicmoo_goal),op(save),
   stand_alone(false),foreign(no_save),global(G),trail(T),local(L)]).
logicmoo_toplevel:- add_history_ideas,threads, module(baseKB),listing(after_boot_goal/1), dmsg("logicmoo_toplevel"),prolog.
logicmoo_goal:- logicmoo_toplevel.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DEFAULT LOGICMOO FLAGS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- set_prolog_flag(double_quotes,string).
:- set_prolog_flag(autoload_logicmoo,false).
:- if( \+ current_module(prolog_stack)).
:- use_module(library(prolog_stack)).
 prolog_stack:stack_guard(none).
:- endif.

setup_for_debug :- set_prolog_flag(report_error,true),set_prolog_flag(debug_on_error,true),
   set_prolog_flag(debugger_write_options,[quoted(true), portray(true), max_depth(1000), attributes(portray)]),
   set_prolog_flag(generate_debug_info,true).



:- during_boot(setup_for_debug).


/*
:- user:ensure_loaded(system:library(logicmoo_utils)).
:- user:ensure_loaded(library(pfc)).
:- user:ensure_loaded(library(logicmoo_user)).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LOAD CYC KB LOADER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- ensure_loaded(baseKB:library('pldata/plkb7166/kb7166')).
:- qcompile_kb7166.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MAKE SURE CLIOPATRIA RUNS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- use_module(pengine_sandbox:library(semweb/rdf_db)).
% :- unsetenv('DISPLAY').

% :- user:ensure_loaded(run_cliopatria).


:- initialization cp_server.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This file provides a skeleton startup file.  It can be localized by running

    % ./configure			(Unix)
    % Double-clicking win-config.exe	(Windows)

After  that,  the  system  may  be  customized  by  copying  or  linking
customization  files  from  config-available    to  config-enabled.  See
config-enabled/README.txt for details.

To run the system, do one of the following:

    * Running for development
      Run ./run.pl (Unix) or open run.pl by double clicking it (Windows)

    * Running as Unix daemon (service)
      See daemon.pl
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

% Setup search path for cliopatria. We add  both a relative and absolute
% path. The absolute path allow us to  start in any directory, while the
% relative one ensures that the system remains working when installed on
% a device that may be mounted on a different location.

add_relative_search_path(Alias, Abs) :-
	is_absolute_file_name(Abs), !,
	prolog_load_context(file, Here),
	relative_file_name(Abs, Here, Rel),
	assertz(user:file_search_path(Alias, Rel)).
add_relative_search_path(Alias, Rel) :-
	assertz(user:file_search_path(Alias, Rel)).

file_search_path(cliopatria, '/mnt/dddd/workspace/ClioPatria').
:- add_relative_search_path(cliopatria, '/mnt/dddd/workspace/ClioPatria').

% Make loading files silent. Comment if you want verbose loading.

:- current_prolog_flag(verbose, Verbose),
   asserta(saved_verbose(Verbose)),
   set_prolog_flag(verbose, silent).


		 /*******************************
		 *	      LOAD CODE		*
		 *******************************/

% Use the ClioPatria help system.  May   be  commented to disable online
% help on the source-code.

:- use_module(cliopatria('applications/help/load')).

% Load ClioPatria itself.  Better keep this line.

:- use_module(cliopatria(cliopatria)).

% Get back normal verbosity of the toplevel.

:- (   retract(saved_verbose(Verbose))
   ->  set_prolog_flag(verbose, Verbose)
   ;   true
   ).





:-  abolish(rdf_rewrite:arity,2),  % clause(rdf_rewrite:arity(A, B),functor(A, _, B),R),erase(R),
   asserta((rdf_rewrite:arity(A, B) :- (compound(A),functor(A, _, B)))). % AND DOES NOT BREAK LOGICMOO
ensure_webserver_p(Port):- format(atom(A),'httpd@~w',[Port]),thread_property(N,status(V)),V=running,atom(N),atom_concat(A,_,N),!.
ensure_webserver_p(Port) :-catch((thread_httpd:http_server(http_dispatch,[ port(Port), workers(16) ])),E,(writeln(E),fail)).
ensure_webserver_3020:- (getenv('LOGICMOO_PORT',Was);Was=3000),
   WebPort is Was + 20, ensure_webserver_p(WebPort).
:- volatile(swish_trace:installed/1).

:- during_boot(ensure_webserver_3020).

:- autoload([verbose(false)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ensure hMUD
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- if(exists_directory(hmud)).
:- absolute_file_name('hmud/',O),
   during_boot(http_handler('/hmud/', http_reply_from_files(O, []), [prefix])).
:- during_boot(ignore(catch(shell('killall perl ; ./hmud/policyd'),E,dmsg(E)))).
:- else.
:- during_boot(http_handler('/hmud/', http_reply_from_files(pack(hMUD), []), [prefix])).
:- during_boot(ignore(catch(shell('killall perl ; ../hmud/policyd'),E,dmsg(E)))).
:- endif.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% (X)WINDOWS (DE)BUGGERY
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_x_ide:- \+ current_prolog_flag(logicmoo_headless,true),!.
start_x_ide:- prolog_ide(thread_monitor),prolog_ide(debug_monitor),
   % prolog_ide(open_debug_status),
   guitracer,
   use_module(library(pce_prolog_xref)),
   noguitracer.

:- after_boot(start_x_ide).


