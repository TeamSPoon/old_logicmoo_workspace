% #! swipl -L8G -G8G -T8G -f
/** <module> Example Logicmoo_base startup script in SWI-Prolog

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

:- user:ensure_loaded(library(logicmoo/logicmoo_base)).

% [Optionaly] Load an Eggdrop (Expects you have  Eggdrop runinng with PROLOG.TCL scripts @ https://github.com/TeamSPoon/MUD_ircbot/)
:- if_file_exists(user:ensure_loaded(library(eggdrop))).
:- eggdrop:egg_go.
:- initialization((current_predicate(egg_go/0)->egg_go;true),now).

:- asserta(user:load_mud_www).
:- gripe_time(40,user:ensure_loaded(logicmoo(mpred_online/logicmoo_i_www))).
:- ensure_webserver.


:- user:ensure_loaded(logicmoo_repl).
