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

:- ensure_loaded(logicmoo('vworld/dbase')).
:- ensure_loaded(logicmoo('vworld/moo')).

:- dbase:end_transform_cyc_preds.

% :-context_module(Ctx),writeq(context_module(Ctx)),nl.

:-context_module(Ctx),asserta(loading_module_h(Ctx)).


% These three are for use with Quintus
%:- prolog_flag(unknown,error,fail). % Not sure if this is needed for Quintus
%:- ensure_loaded(library(random)).
%:- ensure_loaded(library(date)).
% This one is for use with SWI
:- ensure_loaded(library(quintus)).

% logicmoo utils shared with other systems

:- ensure_loaded(logicmoo('logicmoo_util/logicmoo_util_bugger')).
:- ensure_loaded(logicmoo('logicmoo_util/logicmoo_util_library')).
:- ensure_loaded(logicmoo('logicmoo_util/logicmoo_util_ctx_frame')).
:- ensure_loaded(logicmoo('logicmoo_util/logicmoo_util_strings')).
:- ensure_loaded(logicmoo('logicmoo_util/logicmoo_util_terms')).
:- ensure_loaded(logicmoo('logicmoo_util/logicmoo_util_dcg')).

% make sure these get in early
:- ensure_loaded(logicmoo('vworld/dbase_formattypes')).

% logicmoo vworld mud server
:- ensure_loaded(logicmoo('vworld/world')).
% :- ensure_loaded(logicmoo('vworld/world_action')).
:- ensure_loaded(logicmoo('vworld/moo_loader')).
:- ensure_loaded(logicmoo('vworld/toploop_telnet')).
:- ensure_loaded(logicmoo('vworld/toploop_npc')).
:- ensure_loaded(logicmoo('vworld/parser_e2c')).
:- ensure_loaded(logicmoo('vworld/parser_imperative')).

:- ensure_loaded(logicmoo('vworld/moo_testing')).

% NPC planners
:- ensure_loaded(logicmoo('mobs/monster')).
:- ensure_loaded(logicmoo('mobs/predator')).
:- ensure_loaded(logicmoo('mobs/explorer')).
:- ensure_loaded(logicmoo('mobs/prey')).
:- ensure_loaded(logicmoo('mobs/vacuum')).

% Action/Commands implementation

:- ensure_loaded(logicmoo('pldata/tiny_kb')).
:- ensure_loaded(logicmoo('pldata/nldata_freq_pdat')).
:- ensure_loaded(logicmoo('pldata/nldata_BRN_WSJ_LEXICON')).
:- ensure_loaded(logicmoo('pldata/nldata_colloc_pdat')).
:- ensure_loaded(logicmoo('pldata/nldata_cycl_pos0')).
:- ensure_loaded(logicmoo('pldata/nldata_dictionary_some01')).
:- ensure_loaded(logicmoo('pldata/tt0_00022_cycl')).
:- ensure_loaded(logicmoo('pldata/hl_holds')).
:- ensure_loaded(logicmoo('pldata/mworld0')).
:- catch(ensure_loaded(logicmoo('pldata/withvars_988')),_,true).

:- expand_file_name('../src_incoming/actions/*pl',X),
     forall(member(E,X),ensure_loaded(E)).


/*
:- ensure_loaded(logicmoo('actions/any')).
:- ensure_loaded(logicmoo('actions/drink')).
:- ensure_loaded(logicmoo('actions/use')).
:- ensure_loaded(logicmoo('actions/attack')).
:- ensure_loaded(logicmoo('actions/push')).
:- ensure_loaded(logicmoo('actions/climb')).
:- ensure_loaded(logicmoo('actions/eat')).
:- ensure_loaded(logicmoo('actions/move')).
:- ensure_loaded(logicmoo('actions/drop')).
:- ensure_loaded(logicmoo('actions/sit')).
:- ensure_loaded(logicmoo('actions/look')).
:- ensure_loaded(logicmoo('actions/take')).
:- ensure_loaded(logicmoo('actions/logon')).
:- ensure_loaded(logicmoo('actions/teleport')).
:- ensure_loaded(logicmoo('actions/chat')).
:- ensure_loaded(logicmoo('actions/help')).
:- ensure_loaded(logicmoo('actions/get_set')).

% done in 'user' to avoid reloading when we reload dbase

:- expand_file_name('../src_data/pldata/  *pl',X),
     forall(member(E,X),ensure_loaded(E)).

*/

