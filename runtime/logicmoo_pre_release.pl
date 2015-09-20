% #! swipl -L8G -G8G -T8G -f
/** <module> Logicmoo_base sanity test script to be ran before a release

*/

:- initialization(attach_packs).

:-if(\+(exists_source(logicmoo(logicmoo_base)))).
:- dynamic   user:file_search_path/2.
:- multifile user:file_search_path/2.

:- if(exists_directory(runtime)).
user:file_search_path(pack, './pack').
:- else.
user:file_search_path(pack, '../pack').
:- endif.

:- attach_packs.
:- user:ensure_loaded(library(logicmoo/util/logicmoo_util_all)).
:- endif.

:- initialization(attach_packs).

:- asserta(thlocal:verify_side_effect_buffer).

:- user:ensure_loaded(library(logicmoo/logicmoo_base)).

% [Optionaly] Load an Eggdrop (Expects you have  Eggdrop runinng with PROLOG.TCL scripts @ https://github.com/TeamSPoon/MUD_ircbot/)
:- if_file_exists(user:ensure_loaded(library(eggdrop))).
:- eggdrop:egg_go.
:- initialization((current_predicate(egg_go/0)->egg_go;true),now).


/*
% [Required] Load the Logicmoo WWW System
:- (if_file_exists(user:ensure_loaded(library(logicmoo/logicmoo_run_pldoc)))).
:- (if_file_exists(user:ensure_loaded(library(logicmoo/logicmoo_run_clio)))).
:- (if_file_exists(user:ensure_loaded(library(logicmoo/logicmoo_run_swish)))).
*/

:- asserta(user:load_mud_www).
:- gripe_time(40,user:ensure_loaded(logicmoo(mpred_online/logicmoo_i_www))).
:- ensure_webserver.


% [Required] Load the Logicmoo Backchaining Inference System
%:- gripe_time(40,with_no_mpred_expansions(if_file_exists(user:ensure_loaded(logicmoo(logicmoo_engine))))).

% :- statistics(globallimit,G),statistics(locallimit,L),statistics(traillimit,T),qsave_program(logicmoo_repl,[map('logicmoo_repl.sav'),global(G),trail(T),local(L)]).

:- initialization(list_undefined).


% :-  doc_server(8887). % , [allow(localhost), workers(1)]).
:- rl_add_history('help(match_regex/2).').

:- tell(blalla).

:-listing(side_effect_buffer/3).

:-forall(actual_side_effect(H,B),(nl,portray_clause(H:-B))).

:-listing(side_effect_buffer/3).

:- told.

