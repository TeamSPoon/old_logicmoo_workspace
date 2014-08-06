/** <module> 
% All modules are declared here so that this next lines dont have to be pasted into every file.
% Since this list will need at least 160 entries to cover the obj classes rooms and commands, 
% we add the modules here to not waste 160^2 lines of text and having to not 
% update 160+ files whenever a new module is used
%
% Project Logicmoo: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/


% :-set_prolog_flag(unknown,fail).
:- set_prolog_flag(double_quotes, atom).
:- set_prolog_flag(double_quotes, string). 

% these do not get defined!?
% :-dynamic user_db:assert_user/2, user_db:grant_openid_server/2, user_db:retractall_grant_openid_server/2, user_db:retractall_user/2, user_db:assert_grant_openid_server/2.

:- dynamic_multifile_exported hook:decl_database_hook/2.
:- dynamic_multifile_exported hook:deduce_facts/2.

end_of_file.


:- dynamic_multifile_exported moo:ft_info/2.
:- dynamic_multifile_exported moo:subft/2.
:- dynamic_multifile_exported moo:type/1.


%  very very first import
:- within_user(ensure_loaded(logicmoo('vworld/moo.pl'))).

% :-context_module(Ctx),writeq(context_module(Ctx)),nl.
:-context_module(Ctx),asserta(moo:context_module_h(Ctx)).

% These three are for use with Quintus
%:- prolog_flag(unknown,error,fail). % Not sure if this is needed for Quintus
:- within_user(ensure_loaded(library(random))).
%:- within_user(ensure_loaded(library(date))).
% This one is for use with SWI
:- within_user(ensure_loaded(library(quintus))).

% logicmoo utils shared with other systems
:- within_user(ensure_loaded(logicmoo('logicmoo_util/logicmoo_util_bugger.pl'))).
:- within_user(ensure_loaded(logicmoo('logicmoo_util/logicmoo_util_library.pl'))).
:- within_user(ensure_loaded(logicmoo('logicmoo_util/logicmoo_util_ctx_frame.pl'))).
:- within_user(ensure_loaded(logicmoo('logicmoo_util/logicmoo_util_strings.pl'))).
:- within_user(ensure_loaded(logicmoo('logicmoo_util/logicmoo_util_terms.pl'))).
:- within_user(ensure_loaded(logicmoo('logicmoo_util/logicmoo_util_dcg.pl'))).

% make sure these get in early
% :- within_user(ensure_loaded(logicmoo('dbase/dbase_formattypes.pl'))).

% logicmoo vworld mud server
:- within_user(ensure_loaded(logicmoo('vworld/world.pl'))).
% :- within_user(ensure_loaded(logicmoo('vworld/world_action.pl'))).
:- within_user(ensure_loaded(logicmoo('vworld/moo_loader.pl'))).
:- within_user(ensure_loaded(logicmoo('vworld/toploop_telnet.pl'))).
:- within_user(ensure_loaded(logicmoo('vworld/toploop_npc.pl'))).
:- within_user(ensure_loaded(logicmoo('parsing/parser_imperative.pl'))).
:- within_user(ensure_loaded(logicmoo('vworld/moo_testing.pl'))).

% NPC planners
:- within_user(ensure_loaded(logicmoo('mobs/monster.pl'))).
:- within_user(ensure_loaded(logicmoo('mobs/predator.pl'))).
:- within_user(ensure_loaded(logicmoo('mobs/explorer.pl'))).
:- within_user(ensure_loaded(logicmoo('mobs/prey.pl'))).
:- within_user(ensure_loaded(logicmoo('mobs/vacuum.pl'))).

% Action/Commands implementation

:- expand_file_name('../src_incoming/actions/*.pl',X),
     forall(member(E,X),within_user(ensure_loaded(E))).
/*
:- within_user(ensure_loaded(logicmoo('actions/any.pl'))).
:- within_user(ensure_loaded(logicmoo('actions/drink.pl'))).
:- within_user(ensure_loaded(logicmoo('actions/use.pl'))).
:- within_user(ensure_loaded(logicmoo('actions/attack.pl'))).
:- within_user(ensure_loaded(logicmoo('actions/push.pl'))).
:- within_user(ensure_loaded(logicmoo('actions/climb.pl'))).
:- within_user(ensure_loaded(logicmoo('actions/eat.pl'))).
:- within_user(ensure_loaded(logicmoo('actions/move.pl'))).
:- within_user(ensure_loaded(logicmoo('actions/drop.pl'))).
:- within_user(ensure_loaded(logicmoo('actions/sit.pl'))).
:- within_user(ensure_loaded(logicmoo('actions/look.pl'))).
:- within_user(ensure_loaded(logicmoo('actions/take.pl'))).
:- within_user(ensure_loaded(logicmoo('actions/logon.pl'))).
:- within_user(ensure_loaded(logicmoo('actions/teleport.pl'))).
:- within_user(ensure_loaded(logicmoo('actions/chat.pl'))).
:- within_user(ensure_loaded(logicmoo('actions/help.pl'))).
:- within_user(ensure_loaded(logicmoo('actions/get_set.pl'))).
*/
