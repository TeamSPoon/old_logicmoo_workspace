%!swipl -f
/** <module> An Implementation a MUD server in SWI-Prolog

*/

:- multifile
	prolog:message/3.

prolog:message(git(update_versions),A,A):-!.

:- use_module(library(settings)).
% :- use_module(library(check)).
% :- make.
:- portray_text(true).

:- ((current_prolog_flag(readline, true))->expand_file_name("~/.pl-history", [File|_]),(exists_file(File) -> rl_read_history(File); true),at_halt(rl_write_history(File));true).

:-module(user).

:- multifile( entailment:rdf /3 ).

% [Optionaly] Solve the Halting problem
:-use_module(library(process)).
:-use_module(library(pce)).
:- has_gui_debug -> true ; remove_pred(pce_principal,send,2).
:- has_gui_debug -> true ; remove_pred(pce_principal,new,2).


unsafe_preds(M,F,A):-M=files_ex,current_predicate(M:F/A),member(X,[delete,copy]),atom_contains(F,X).
unsafe_preds(M,F,A):-M=process,current_predicate(M:F/A),member(X,[kill,create]),atom_contains(F,X).
unsafe_preds(M,F,A):-M=system,member(F,[shell,halt]),current_predicate(M:F/A).

:-forall(unsafe_preds(M,F,A),bugger:remove_pred(M,F,A)).

% [Optionaly] Solve the Halting problem
:-redefine_system_predicate(system:halt).
:-abolish(system:halt,0).
system:halt:- format('the halting problem is now solved!').

:- dmsg('the halting problem is now solved!').

add_game_dir(GAMEDIR,Else):- add_to_search_path_first(game, GAMEDIR),now_try_game_dir(Else).

now_try_game_dir(Else):-  enumerate_files(game('.'), GAMEDIR) *-> 
  ((exists_directory(GAMEDIR) -> 
    with_all_dmsg(( forall(enumerate_files(game('**/*.pl'),X),user_ensure_loaded(X)),
      forall(enumerate_files(game('**/*.plmoo'),X),declare_load_game(X)))); (fmt(missing(GAMEDIR)),Else)));  (fmt(no_game_dir),Else).


:-context_module(CM),assert(startup_mod:loading_from_cm(CM)).
create_module(M):-context_module(CM),module(M),asserta(M:this_is_a_module(M)),writeq(switching_back_to_module(CM)),module(CM).
:-create_module(user).
:-create_module(hook).
:-create_module(thlocal).
:-create_module(thglobal).
:-create_module(moo).


:-module_transparent parser_chat80_module/1.
:-multifile parser_chat80_module/1.
:-export((parser_chat80_module/1)).
parser_chat80_module(moo).


:-export(prolog_repl/0).
prolog_repl:- with_all_dmsg((nl,fmt("Press Ctrl-D to start the mud!"),nl,catch(tlocals,E,dmsg(tlocals==E)),prolog)).

:- set_prolog_flag(gui,false).
:- set_prolog_flag(history,1000).


:-export(within_user/1).
:-export(is_startup_file/1).

is_startup_file(Name):- current_prolog_flag(os_argv,ArgV),member(Named,ArgV),atom(Named),atom_concat(Name,_,Named),!.

within_user(Call):- '@'(Call,'user').

% ======================================================
% Configure the logicmoo utilities into the file path
% :- include('logicmoo_util/logicmoo_util_header').
% :- user_use_module('logicmoo_util/logicmoo_util_all.pl').
% And adds the local directories to file search path of logicmoo(..)
% ======================================================
:- within_user(consult('../src_lib/logicmoo_util/logicmoo_util_all')).

% one more case of not clear what's the good way to do this.
% Add your own path to weblog for now
user:file_search_path(weblog, 'C:/docs/Prolog/weblog/development/weblog/prolog').
user:file_search_path(weblog, 'C:/Users/Administrator/AppData/Roaming/SWI-Prolog/pack/weblog').
user:file_search_path(weblog, '/usr/lib/swi-prolog/pack/weblog/prolog'):-current_prolog_flag(unix,true).
user:file_search_path(cliopatria, '../externals/ClioPatria'). % :- current_prolog_flag(unix,true).
user:file_search_path(swish, '../externals/swish'):- current_prolog_flag(unix,true).

:- user_use_module(library(settings)).

:- user:file_search_path(cliopatria,SP),
   exists_directory(SP),
   writeq(user:file_search_path(cliopatria,SP)),nl.
   %set_setting_default(cliopatria_binding:path, SP).
   %save_settings('moo_settings.db').
   %%setting(cliopatria_binding:path, atom, SP, 'Path to root of cliopatria install'),!.

:- user_use_module(logicmoo('http/user_page')).

:- meta_predicate(startup_mod:if_version_greater(?,0)).

startup_mod:if_version_greater(V,Goal):- current_prolog_flag(version,F), ((F > V) -> call(Goal) ; true).

% set to false because we don't want to use the mudconsole
:- if_flag_true(false, startup_mod:if_version_greater(70109,user_use_module(logicmoo('mudconsole/mudconsolestart')))).

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
   with_no_term_expansions(with_assertions(op(200,fy,'@'),
   ((
 %  use_module('t:/devel/cliopatria/rdfql/sparql_runtime.pl'),
  % ensure_loaded(logicmoo(launchcliopatria)),
  % ensure_loaded(logicmoo(testwebconsole)),
  % kill_term_expansion, 
   ensure_loaded(swish(logicmoo_run_swish))
   )))),!.



slow_work:- with_assertions( prevent_transform_moo_preds , within_user(at_start(hard_work))).

thread_work:- thread_property(X, status(running)),X=loading_code,!.
thread_work:- thread_create(slow_work,_,[alias(loading_code)]).

% start_servers :- startup_mod:if_version_greater(70111,thread_work).
start_servers :- startup_mod:if_version_greater(70111,slow_work).

enqueue_player_command(C):-enqueue_player_command(_,C).
enqueue_player_command(P,C):-foc_current_player(P),assertz_if_new(thglobal:player_command_stack(P,C)).


% [Required] load and start mud
:- within_user(ensure_loaded(logicmoo(vworld/moo_startup))).

startup_mod:run_setup_now:-
   within_user((
      finish_processing_world      
   % TO UNDO register_timer_thread(npc_ticker,90,npc_tick)
   )).

run_setup:- within_user(at_start(startup_mod:run_setup_now)).

run:- within_user(at_start(login_and_run)).


