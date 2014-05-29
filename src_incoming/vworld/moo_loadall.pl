
% these do not get defined!?
% :-dynamic user_db:assert_user/2, user_db:grant_openid_server/2, user_db:retractall_grant_openid_server/2, user_db:retractall_user/2, user_db:assert_grant_openid_server/2.

% These three are for use with Quintus
%:- prolog_flag(unknown,error,fail). % Not sure if this is needed for Quintus
%:- ensure_loaded(library(random)).
%:- ensure_loaded(library(date)).

% This one is for use with SWI
:- ensure_loaded(library(quintus)).

:- set_prolog_flag(verbose_load,true).

load_mud_core_file(F0):-
   expand_file_name(F0,[F]),
  % 'format'('%~q.~n',[load_mud_core_file(F)]),
   load_files([F],[ redefine_module(false),if(not_loaded),register(true),silent(false),must_be_module(false)]).

% logicmoo utils shared with other systems

:- ensure_loaded(logicmoo('logicmoo_util/logicmoo_util_bugger')).
:- ensure_loaded(logicmoo('logicmoo_util/logicmoo_util_library')).
:- ensure_loaded(logicmoo('logicmoo_util/logicmoo_util_ctx_frame')).
:- ensure_loaded(logicmoo('logicmoo_util/logicmoo_util_strings')).
:- ensure_loaded(logicmoo('logicmoo_util/logicmoo_util_terms')).
:- ensure_loaded(logicmoo('logicmoo_util/logicmoo_util_dcg')).

% make sure these get in earliest

:- ensure_loaded(logicmoo(vworld/dbase)).
:- ensure_loaded(logicmoo(vworld/moo)).

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
:- begin_transform_moo_preds.
/*
:- ensure_loaded(logicmoo('mobs/monster')).
:- ensure_loaded(logicmoo('mobs/predator')).
:- ensure_loaded(logicmoo('mobs/explorer')).
:- ensure_loaded(logicmoo('mobs/prey')).
:- ensure_loaded(logicmoo('mobs/vacuum')).
*/
:- expand_file_name('../src_incoming/mobs/*.pl',X),
     forall(member(E,X),load_mud_core_file(E)).
:- end_transform_moo_preds.

:- set_prolog_flag(verbose_load,true).

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

:- begin_transform_moo_preds.
:- expand_file_name('../src_incoming/actions/*.pl',X),
     forall(member(E,X),load_mud_core_file(E)).
:- end_transform_moo_preds.


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

:- expand_file_name('../src_data/pldata/?*.pl,X),
     forall(member(E,X),load_mud_core_file(E)).

*/


