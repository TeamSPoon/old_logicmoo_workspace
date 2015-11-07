/*   LogicMOO Base FOL/PFC Setup
% Dec 13, 2035
% Douglas Miles
*/
:- module(logicmoo_base,[ensure_mpred_system/0,enable_mpred_system/1,disable_mpred_system/1]).

:- multifile '$si$':'$was_imported_kb_content$'/2.
:- dynamic '$si$':'$was_imported_kb_content$'/2.
:- discontiguous('$si$':'$was_imported_kb_content$'/2).

:- multifile(lmconf:mpred_is_impl_file/1).
:- dynamic(lmconf:mpred_is_impl_file/1).

:- source_location(F,_),asserta(lmconf:never_registered_mpred_file(F)).

:- multifile lmconf:startup_option/2. 
:- dynamic lmconf:startup_option/2. 
:- multifile lmconf:mpred_system_status/2.
:- dynamic lmconf:mpred_system_status/2.
:- multifile(t_l:disable_px/0).
:- thread_local(t_l:disable_px/0).

:- multifile(lmconf:mpred_system_kb/1).
:- dynamic(lmconf:mpred_system_kb/1).

:- use_module(logicmoo_utils).
% :- initialization(add_library_search_path('.',[ './mpred/*.pl','./snark/*.pl'])).
:-dmsg("Adding logicmoo/[mpred,snark] to autoload path",[]).
:- add_library_search_path('./mpred/',[ '*.pl']).
:- add_library_search_path('./snark/',[ '*.pl']).
% :- add_library_search_path('./plarkc/',[ '*.pl']).
% :- add_library_search_path('./pttp/',[ 'dbase_i_mpred_*.pl']).


% ========================================
% lmconf:mpred_system_kb/1
% ========================================


%= 	 	 

%% mpred_system_kb( ?VALUE1) is semidet.
%
% Hook To [lmconf:mpred_system_kb/1] For Module Logicmoo_base.
% Managed Predicate System Knowledge Base.
%
lmconf:mpred_system_kb(baseKB).


% lmconf:startup_option(datalog,sanity). %  Run datalog sanity tests while starting
% lmconf:startup_option(clif,sanity). %  Run datalog sanity tests while starting

:- ensure_loaded(logicmoo_utils).
:- asserta(lmconf:pfcManageHybrids).


% ================================================
% DBASE_T System
% ================================================

:- multifile(lmconf:mpred_is_impl_file/1).
:- dynamic(lmconf:mpred_is_impl_file/1).

%= 	 	 

%% mpred_is_impl_file( ?A) is semidet.
%
% Hook To [lmconf:mpred_is_impl_file/1] For Module Logicmoo_base.
% Managed Predicate If Is A Implimentation File.
%
lmconf:mpred_is_impl_file(mpred/A):-nonvar(A).


%= 	 	 

%% load_mpred_system( ?Ctx) is semidet.
%
% Load Managed Predicate System.
%
load_mpred_system(Ctx):-  !,Ctx:use_module(logicmoo(mpred/mpred_userkb)).
load_mpred_system(Ctx):-  lmconf:mpred_system_kb(Sys),
   with_mutex(mpred_system_mutex,forall(lmconf:mpred_is_impl_file(File),     
     (( Ctx:use_module(logicmoo_utils), call((Sys:w_tl(t_l:disable_px,Sys:ensure_loaded(File)))))))).

:- export(enable_mpred_system/1).
%% enable_mpred_system(+Module) is det.
% Begin considering forward and meta programming rules into a Prolog module.

%= 	 	 

%% enable_mpred_system( ?Module) is semidet.
%
% Enable Managed Predicate System.
%
enable_mpred_system(Module):- with_mutex(mpred_system_mutex,lmconf:enable_mpred_system0(Module)).

:- export(disable_mpred_system/1).
%% disable_mpred_system(+Module) is det.
% Disable tasks that considering forward and meta programming rules into a Prolog module.

%= 	 	 

%% disable_mpred_system( ?Module) is semidet.
%
% Disable Managed Predicate System.
%
disable_mpred_system(Module):- with_mutex(mpred_system_mutex,lmconf:disable_mpred_system0(Module)).

:- thread_local t_l:side_effect_ok/0.


%= 	 	 

%% enable_mpred_system0( ?Module) is semidet.
%
% Hook To [lmconf:enable_mpred_system0/1] For Module Logicmoo_base.
% Enable Managed Predicate System Primary Helper.
%
lmconf:enable_mpred_system0(Module):- lmconf:mpred_system_status(Module,enabled),!.
lmconf:enable_mpred_system0(Module):- 
   set_user_abox(Module),
   retractall(lmconf:mpred_system_status(Module,_)),
   load_mpred_system(Module),
   must(current_predicate(_,mred_loader:mpred_expander(_,_,_,_))),
   meta_predicate(mpred_loader:mpred_expander(+,+,+,-)),   
   asserta_if_new((user:term_expansion(I,O):- mpred_expander(term,user,I,O))),
   asserta_if_new((system:goal_expansion(I,O):- mpred_expander(goal,system,I,O))),
   asserta_if_new((Module:term_expansion(I,O):- mpred_expander(term,Module,I,O))),
   asserta_if_new((Module:goal_expansion(I,O):- mpred_expander(goal,Module,I,O))),
   asserta(lmconf:mpred_system_status(Module,enabled)),
   Module:w_tl(t_l:side_effect_ok,doall(Module:call_no_cuts(lmconf:module_local_init))).


%= 	 	 

%% disable_mpred_system0( ?Module) is semidet.
%
% Hook To [lmconf:disable_mpred_system0/1] For Module Logicmoo_base.
% Disable Managed Predicate System Primary Helper.
%
lmconf:disable_mpred_system0(Module):- lmconf:mpred_system_status(Module,disabled),!.
lmconf:disable_mpred_system0(Module):-    
   retractall(lmconf:mpred_system_status(Module,_)),
   asserta(lmconf:mpred_system_status(Module,disabled)),
   % one day unload_mpred_system(Module),
   %retractall((user:term_expansion(I,O):- mpred_expander(term,user,I,O))),
   %retractall((system:goal_expansion(I,O):- mpred_expander(goal,system,I,O))),
   retractall((Module:term_expansion(I,O):- mpred_expander(term,Module,I,O))),
   retractall((Module:goal_expansion(I,O):- mpred_expander(goal,Module,I,O))),!.
   
:- module_transparent ensure_mpred_system/0.
%% ensure_mpred_system is det.
% Ensure the "managed predicate" system and subsystems are available

%= 	 	 

%% ensure_mpred_system is semidet.
%
% Ensure Managed Predicate System.
%
ensure_mpred_system:- source_context_module(M),enable_mpred_system(M).


:- lmconf:mpred_system_kb(M),dmsg(system_kb=M).
:- baseKB:use_module(library(logicmoo/mpred/mpred_userkb)).

end_of_file.

Warning: at runtime using assert/1, use :- dynamic Name/Arity.
Warning:
Warning: common_logic_boxlog:pfcRHS/1, which is referenced by
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/snark/common_logic_boxlog.pl:170:35: 26-th clause of common_logic_boxlog:boxlog_to_compile/3
Warning: common_logic_compiler:pttp1a_wid/3, which is referenced by
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/snark/common_logic_compiler.pl:802:5: 1-st clause of common_logic_compiler:cf/4
Warning: common_logic_compiler:pttp_builtin/2, which is referenced by
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/snark/common_logic_compiler.pl:1019:43: 2-nd clause of common_logic_compiler:unbuiltin_negate/4
Warning: common_logic_snark:is_wrapper_pred/1, which is referenced by
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/snark/common_logic_snark.pl:429:32: 14-th clause of common_logic_snark:adjust_kif0/4
Warning: common_logic_snark:retractall_wid/1, which is referenced by
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/snark/common_logic_snark.pl:819:5: 1-st clause of common_logic_snark:kif_ask_sent/1
Warning: lmconf:contract_output_proof/2, which is referenced by
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/snark/common_logic_snark.pl:837:8: 1-st clause of common_logic_snark:kif_ask/2
Warning: lmconf:search/7, which is referenced by
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/snark/common_logic_snark.pl:830:8: 5-th clause of common_logic_snark:kif_ask/1
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/snark/common_logic_snark.pl:836:8: 1-st clause of common_logic_snark:kif_ask/2
Warning: mpred_pfc:mpred_mark/4, which is referenced by
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl:2495:40: 1-st clause of mpred_pfc:mpred_cleanup/0
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl:2939:26: 3-th clause of mpred_pfc:should_call_for_facts/3
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl:2940:26: 4-th clause of mpred_pfc:should_call_for_facts/3
Warning: mpred_pfc:support_hilog/2, which is referenced by
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl:3326:7: 1-st clause of mpred_pfc:pred_u2/1
Warning: mpred_pfc:tCol/1, which is referenced by
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl:2210:50: 4-th clause of mpred_pfc:mpred_call_0/1
Warning: mpred_pfc:~/1, which is referenced by
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl:2182:24: 2-nd clause of mpred_pfc:neg_in_code/1
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_pfc.pl:2183:36: 3-th clause of mpred_pfc:neg_in_code/1
Warning: mpred_storage:mpred_isa/2, which is referenced by
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_storage.pl:715:70: 1-st clause of mpred_storage:prolog_mpred_provide_storage_op/2
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_storage.pl:716:74: 2-nd clause of mpred_storage:prolog_mpred_provide_storage_op/2
Warning: mpred_storage:tCol/1, which is referenced by
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_storage.pl:214:29: 5-th clause of mpred_storage:deduceEachArgType/3
Warning: mpred_storage:ttFormatType/1, which is referenced by
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_storage.pl:232:42: 6-th clause of mpred_storage:deduceEachArg_WithType/2
Warning: mpred_type_args:genls/2, which is referenced by
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_args.pl:331:76: 1-st clause of mpred_type_args:argIsa_call_7/3
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_args.pl:339:43: 1-st clause of mpred_type_args:argIsa_call_9/3
Warning: mpred_type_args:isa/2, which is referenced by
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_args.pl:137: 2-nd clause of mpred_type_args:term_is_ft_how/2
Warning: mpred_type_args:resultIsa/2, which is referenced by
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_args.pl:186:34: 4-th clause of mpred_type_args:is_ftText/1
Warning: mpred_type_args:tCol/1, which is referenced by
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_args.pl:300:74: 51-th clause of mpred_type_args:argIsa_call_0/3
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_args.pl:198:36: 1-st clause of mpred_type_args:as_one_of/2
Warning: mpred_type_args:tRelation/1, which is referenced by
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_args.pl:216:43: 3-th clause of mpred_type_args:asserted_argIsa_known/3
Warning: mpred_type_args:ttFormatType/1, which is referenced by
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_args.pl:128:61: 4-th clause of mpred_type_args:term_is_ft/2
Warning: mpred_type_constraints:argIsa/3, which is referenced by
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_constraints.pl:176:39: 1-st clause of mpred_type_constraints:attempt_attribute_one_arg/4
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_constraints.pl:178:39: 3-th clause of mpred_type_constraints:attempt_attribute_one_arg/4
Warning: mpred_type_constraints:argQuotedIsa/3, which is referenced by
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_constraints.pl:177:39: 2-nd clause of mpred_type_constraints:attempt_attribute_one_arg/4
Warning: mpred_type_constraints:genls/2, which is referenced by
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_constraints.pl:141:29: 2-nd clause of mpred_type_constraints:max_isa/3
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_constraints.pl:142:29: 3-th clause of mpred_type_constraints:max_isa/3
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_constraints.pl:125:34: 1-st clause of mpred_type_constraints:max_isa_l/2
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_constraints.pl:135:29: 2-nd clause of mpred_type_constraints:min_isa/3
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_constraints.pl:136:29: 3-th clause of mpred_type_constraints:min_isa/3
Warning: mpred_type_constraints:lambda/5, which is referenced by
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_constraints.pl:124:34: 1-st clause of mpred_type_constraints:min_isa_l/2
Warning: mpred_type_constraints:tCol/1, which is referenced by
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_constraints.pl:167:55: 11-th clause of mpred_type_constraints:attempt_attribute_args/3
Warning: mpred_type_isa:decided_not_was_isa/2, which is referenced by
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_isa.pl:191:44: 7-th clause of mpred_type_isa:never_type_why/2
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_isa.pl:246:48: 1-st clause of mpred_type_isa:was_isa/3
Warning: mpred_type_isa:genls/2, which is referenced by
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_isa.pl:502:96: 6-th clause of mpred_type_isa:isa_asserted_1/2
Warning: mpred_type_isa:tCol/1, which is referenced by
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_isa.pl:601:28: 1-st clause of mpred_type_isa:guess_types_0/2
Warning: mpred_type_isa:tPred/1, which is referenced by
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_isa.pl:410:64: 3-th clause of mpred_type_isa:not_mud_isa/3
Warning: mpred_type_naming:argIsa/3, which is referenced by
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_naming.pl:200:9: 1-st clause of mpred_type_naming:spawnOneSpawnArg/4
Warning: mpred_type_naming:genls/2, which is referenced by
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_naming.pl:205:41: 3-th clause of mpred_type_naming:convertToInstance/3
Warning: mpred_type_naming:mudKeyword/2, which is referenced by
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_naming.pl:122:31: 7-th clause of mpred_type_naming:createByNameMangle0/3
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_naming.pl:108:42: 3-th clause of mpred_type_naming:to_atomic_name/3
Warning: mpred_type_naming:tCol/1, which is referenced by
Warning:        /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_naming.pl:169:1: 2-nd clause of mpred_type_naming:onSpawn_0/2

