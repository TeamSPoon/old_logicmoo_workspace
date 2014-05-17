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

%  very very first import
:- debug.

:- ensure_loaded(logicmoo('vworld/dbase.pl')).
:- ensure_loaded(logicmoo('vworld/moo.pl')).

:- end_transform_cyc_preds.

% :-context_module(Ctx),writeq(context_module(Ctx)),nl.

:-context_module(Ctx),asserta(loading_module_h(Ctx)).

% These three are for use with Quintus
%:- prolog_flag(unknown,error,fail). % Not sure if this is needed for Quintus
%:- ensure_loaded(library(random)).
%:- ensure_loaded(library(date)).
% This one is for use with SWI
:- ensure_loaded(library(quintus)).

% logicmoo utils shared with other systems

:- ensure_loaded(logicmoo('logicmoo_util/logicmoo_util_bugger.pl')).
:- ensure_loaded(logicmoo('logicmoo_util/logicmoo_util_library.pl')).
:- ensure_loaded(logicmoo('logicmoo_util/logicmoo_util_ctx_frame.pl')).
:- ensure_loaded(logicmoo('logicmoo_util/logicmoo_util_strings.pl')).
:- ensure_loaded(logicmoo('logicmoo_util/logicmoo_util_terms.pl')).
:- ensure_loaded(logicmoo('logicmoo_util/logicmoo_util_dcg.pl')).

% make sure these get in early
:- ensure_loaded(logicmoo('vworld/dbase_formattypes.pl')).

% logicmoo vworld mud server
:- ensure_loaded(logicmoo('vworld/world.pl')).
% :- ensure_loaded(logicmoo('vworld/world_action.pl')).
:- ensure_loaded(logicmoo('vworld/moo_loader.pl')).
:- ensure_loaded(logicmoo('vworld/toploop_telnet.pl')).
:- ensure_loaded(logicmoo('vworld/toploop_npc.pl')).
:- ensure_loaded(logicmoo('vworld/parser_e2c.pl')).
:- ensure_loaded(logicmoo('vworld/parser_imperative.pl')).

:- ensure_loaded(logicmoo('vworld/moo_testing.pl')).

% NPC planners
:- ensure_loaded(logicmoo('mobs/monster.pl')).
:- ensure_loaded(logicmoo('mobs/predator.pl')).
:- ensure_loaded(logicmoo('mobs/explorer.pl')).
:- ensure_loaded(logicmoo('mobs/prey.pl')).
:- ensure_loaded(logicmoo('mobs/vacuum.pl')).

% Action/Commands implementation

:- expand_file_name('../src_incoming/actions/*pl',X),
     forall(member(E,X),ensure_loaded(E)).
/*
:- ensure_loaded(logicmoo('actions/any.pl')).
:- ensure_loaded(logicmoo('actions/drink.pl')).
:- ensure_loaded(logicmoo('actions/use.pl')).
:- ensure_loaded(logicmoo('actions/attack.pl')).
:- ensure_loaded(logicmoo('actions/push.pl')).
:- ensure_loaded(logicmoo('actions/climb.pl')).
:- ensure_loaded(logicmoo('actions/eat.pl')).
:- ensure_loaded(logicmoo('actions/move.pl')).
:- ensure_loaded(logicmoo('actions/drop.pl')).
:- ensure_loaded(logicmoo('actions/sit.pl')).
:- ensure_loaded(logicmoo('actions/look.pl')).
:- ensure_loaded(logicmoo('actions/take.pl')).
:- ensure_loaded(logicmoo('actions/logon.pl')).
:- ensure_loaded(logicmoo('actions/teleport.pl')).
:- ensure_loaded(logicmoo('actions/chat.pl')).
:- ensure_loaded(logicmoo('actions/help.pl')).
:- ensure_loaded(logicmoo('actions/get_set.pl')).
*/



