:- set_prolog_flag(verbose_load,true).

% logicmoo utils shared with other systems

:- ensure_loaded(logicmoo(logicmoo_util/logicmoo_util_bugger)).
:- ensure_loaded(logicmoo(logicmoo_util/logicmoo_util_library)).
:- ensure_loaded(logicmoo(logicmoo_util/logicmoo_util_ctx_frame)).
:- ensure_loaded(logicmoo(logicmoo_util/logicmoo_util_strings)).
:- ensure_loaded(logicmoo(logicmoo_util/logicmoo_util_terms)).
:- ensure_loaded(logicmoo(logicmoo_util/logicmoo_util_dcg)).
:- set_prolog_flag(verbose_load,true).
% these do not get defined!?
% :-dynamic user_db:assert_user/2, user_db:grant_openid_server/2, user_db:retractall_grant_openid_server/2, user_db:retractall_user/2, user_db:assert_grant_openid_server/2.

% These three are for use with Quintus
%:- prolog_flag(unknown,error,fail). % Not sure if this is needed for Quintus
%:- ensure_loaded(library(random)).
%:- ensure_loaded(library(date)).

% This one is for use with SWI
:- ensure_loaded(library(quintus)).

:- ensure_loaded(logicmoo(vworld/moo)).
:- ensure_loaded(logicmoo(vworld/dbase)).


:- ensure_moo_loaded(logicmoo(vworld/dbase_formattypes)).

:- set_prolog_flag(verbose_load,true).
% these do not get defined!?
% :-dynamic user_db:assert_user/2, user_db:grant_openid_server/2, user_db:retractall_grant_openid_server/2, user_db:retractall_user/2, user_db:assert_grant_openid_server/2.

% logicmoo vworld mud server
:- ensure_moo_loaded(logicmoo(vworld/world)).
:- ensure_moo_loaded(logicmoo(vworld/toploop_telnet)).
:- ensure_moo_loaded(logicmoo(vworld/toploop_npc)).
:- ensure_moo_loaded(logicmoo(vworld/parser_e2c)).
:- ensure_moo_loaded(logicmoo(vworld/parser_imperative)).

:- ensure_loaded(logicmoo(vworld/moo_loader)).
:- ensure_moo_loaded(logicmoo(vworld/moo_testing)).



% NPC planners
%:- include_moo_files('../src_incoming/mobs/?*.pl').
:- ensure_moo_loaded(logicmoo('mobs/monster.pl')).
:- ensure_moo_loaded(logicmoo('mobs/predator.pl')).
:- ensure_moo_loaded(logicmoo('mobs/explorer.pl')).
:- ensure_moo_loaded(logicmoo('mobs/prey.pl')).
:- ensure_moo_loaded(logicmoo('mobs/mobs_conf.pl')).
:- ensure_moo_loaded(logicmoo('mobs/vacuum.pl')).


/*

% done in 'user' to avoid reloading when we reload dbase

:- include_moo_files('../src_data/pldata/?*.pl').

*/

:- use_module(logicmoo(pldata/tiny_kb)).
:- use_module(logicmoo(pldata/nldata_freq_pdat)).
:- use_module(logicmoo(pldata/nldata_BRN_WSJ_LEXICON)).
:- use_module(logicmoo(pldata/nldata_colloc_pdat)).
:- use_module(logicmoo(pldata/nldata_cycl_pos0)).
:- use_module(logicmoo(pldata/nldata_dictionary_some01)).
:- use_module(logicmoo(pldata/tt0_00022_cycl)).
:- use_module(logicmoo(pldata/hl_holds)).
:- use_module(logicmoo(pldata/mworld0)).
:- catch(use_module(logicmoo(pldata/withvars_988)),_,true).

% Action/Commands implementation
% :- include_moo_files('../src_incoming/actions/?*.pl').
:- ensure_moo_loaded(logicmoo('actions/drink.pl')).
:- ensure_moo_loaded(logicmoo('actions/actions_db.pl')).
:- ensure_moo_loaded(logicmoo('actions/attack.pl')).
:- ensure_moo_loaded(logicmoo('actions/inventory.pl')).
:- ensure_moo_loaded(logicmoo('actions/push.pl')).
:- ensure_moo_loaded(logicmoo('actions/where.pl')).
:- ensure_moo_loaded(logicmoo('actions/climb.pl')).
:- ensure_moo_loaded(logicmoo('actions/move.pl')).
:- ensure_moo_loaded(logicmoo('actions/any.pl')).
:- ensure_moo_loaded(logicmoo('actions/drop.pl')).
:- ensure_moo_loaded(logicmoo('actions/help.pl')).
:- ensure_moo_loaded(logicmoo('actions/logon.pl')).
:- ensure_moo_loaded(logicmoo('actions/get_set.pl')).
:- ensure_moo_loaded(logicmoo('actions/sit.pl')).
:- ensure_moo_loaded(logicmoo('actions/look.pl')).
:- ensure_moo_loaded(logicmoo('actions/actions_conf.pl')).
:- ensure_moo_loaded(logicmoo('actions/take.pl')).
:- ensure_moo_loaded(logicmoo('actions/chat.pl')).
:- ensure_moo_loaded(logicmoo('actions/use.pl')).
:- ensure_moo_loaded(logicmoo('actions/stats.pl')).
:- ensure_moo_loaded(logicmoo('actions/eat.pl')).
:- ensure_moo_loaded(logicmoo('actions/teleport.pl')).



% standard footer to clean up any header defined states
%:- ensure_moo_loaded(logicmoo(vworld/moo_footer)).


