%   File   : pfc
%   Author : Tim Finin, finin@umbc.edu
%   Updated: 10/11/87, ...
%   Purpose: consult system file for ensure
:- module(mpred_pfc,
          [ add_reprop/2,
            add_side_effect/2,
            all_closed/1,
            append_as_first_arg/3,
            assert_eq_quitely/1,
            assert_i/1,
            assert_u/1,
            assert_u/3,
            % mpred_select/2,
            asserta_i/1,
            assertz_i/1,
            assertz_u/1,
            assertz_u/3,
            assumption/1,
            assumptions/2,
            assumptions1/2,
            attvar_op/2,
            baseable/2,
            baseable_list/2,
            brake/1,
            build_code_test/3,
            build_consequent/3,
            build_neg_test/4,
            build_rhs/3,
            build_rule/3,
            build_trigger/4,
            bwc/0,
            call_i/1,
            call_prologsys/1,
            call_u/1,
            call_u/2,
            call_with_bc_triggers/1,
            clause_asserted_local/1,
            clause_i/2,
            clause_i/3,
            clause_or_call/2,
            clause_u/2,
            clause_u/3,
            cnstrn/1,
            cnstrn/2,
            cnstrn0/2,
            code_sentence_op/1,
            compute_resolve/3,
            compute_resolve/5,
            correctify_support/2,
            cyclic_break/1,
            cwc/0,
            defaultmpred_select/2,
            different_literal/4,
            erase_w_attvars/2,
            exact_args/1,
            fc_eval_action/2,
            fcnt/2,
            fcnt0/2,
            fcpt/2,
          f_to_mfa/4,
          w_get_fa/3,
            fcpt0/2,
            foreachl_do/2,
            fwc/0,
            fwd_ok/1,
            get_fa/3,
            get_next_fact/2,
            get_source_ref/1,
            get_source_ref1/1,
            get_why/4,
            has_body_atom/2,
            has_cl/1,
            has_db_clauses/1,
            has_functor/1,
            is_action_body/1,
            is_already_supported/3,
            is_bc_body/1,
            is_disabled_clause/1,
            is_fc_body/1,
            is_mpred_action/1,
            is_relative/1,
            is_reprop/1,
            is_reprop_0/1,
            is_resolved/1,
            is_retract_first/1,
            is_side_effect_disabled/0,
            justification/2,
            justifications/2,
            loop_check_nr/1,
            make_uu_remove/1,
            map_literals/2,
            map_literals/3,
            map_unless/4,
            match_source_ref1/1,
            maybeSupport/2,
            meta_wrapper_rule/1,
            lmconf:module_local_init/0,
            mpred_add/1,
            mpred_add/2,
            mpred_add_actiontrace/2,
            mpred_add_db_to_head/2,
            mpred_add_fast/1,
            mpred_add_fast/2,
            mpred_add_fast_sp/2,
            mpred_add_fast_sp0/2,
            mpred_add_fast_timed/2,
            mpred_add_minfo/1,
            mpred_add_minfo/2,
            mpred_add_minfo_2/2,
            mpred_add_rule0/1,
            mpred_add_rule_if_rule/1,
            mpred_add_support/2,
            mpred_add_t/2,
            mpred_add_trigger/2,
            mpred_add_trigger_0/3,
            mpred_adda/1,
            mpred_adda_i/2,
            mpred_addz/1,
            mpred_addz_i/2,
            mpred_axiom/1,
            mpred_bc_only/1,
            mpred_bt_pt_combine/3,
            mpred_call/1,
            mpred_call_0/1,
            mpred_call_0/3,
            mpred_call_only_facts/1,
            mpred_call_only_facts/2,
            mpred_call_shared/1,
            mpred_call_t_exact/1,
            mpred_call_with_no_triggers/1,
            mpred_call_with_no_triggers_bound/1,
            mpred_child/2,
            mpred_children/2,
            mpred_clause/3,
            mpred_clause_i/1,
            mpred_clause_is_asserted/2,
            mpred_clause_is_asserted_hb_nonunify/2,
            mpred_cleanup/0,
            mpred_cleanup/2,
            mpred_cleanup_0/1,
            mpred_compile_rhsTerm/3,
            mpred_connective/1,
            mpred_current/0,
            mpred_current_db/1,
            mpred_current_op_support/1,
            mpred_database_item/1,
            mpred_database_term/1,
            mpred_db_type/2,
            mpred_deep_support/2,
            mpred_deep_support0/2,
            mpred_define_bc_rule/3,
            mpred_descendant/2,
            mpred_descendant1/3,
            mpred_descendants/2,
            mpred_each_literal/2,
            mpred_enqueue/2,
            mpred_error/1,
            mpred_error/2,
            mpred_eval_lhs/2,
            mpred_eval_lhs0/2,
            mpred_eval_rhs1/2,
            mpred_eval_rhs_0/2,
            mpred_fact/1,
            mpred_fact/2,
            mpred_facts/1,
            mpred_facts/2,
            mpred_facts/3,
            mpred_facts_and_universe/1,
            mpred_facts_only/1,
            mpred_file_expansion_0/2,
            mpred_freeLastArg/2,
            mpred_fwd/1,
            mpred_fwd/2,
            mpred_fwd1/2,
            mpred_fwd2/2,
            mpred_get_support/2,
            mpred_get_support_neg/2,
            mpred_get_support_one/2,
            mpred_get_support_precanonical/2,
            mpred_get_support_precanonical_plus_more/2,
            mpred_get_support_via_clause_db/2,
            mpred_get_support_via_sentence/2,
            mpred_get_trigger_quick/1,
            mpred_had_support/2,
            mpred_halt/0,
            mpred_halt/1,
            mpred_halt/2,
            mpred_hide_msg/1,
            mpred_ignored/1,
            mpred_init_i/2,
            mpred_is_builtin/1,
            mpred_is_info/1,
            mpred_is_silient/0,
            mpred_is_taut/1,
            mpred_is_tautology/1,
            mpred_is_tracing_exec/0,
            mpred_literal/1,
            mpred_literal_nv/1,
            mpred_maptree/2,
            mpred_maptree/3,
            mpred_mark_as/4,
            mpred_mark_fa_as/6,
            mpred_mpred_file/0,
            mpred_negated_literal/1,
            mpred_negated_literal/2,
            mpred_negation/2,
            mpred_negation_w_neg/2,
            mpred_nf/2,
            mpred_nf1/2,
            mpred_nf1_negation/2,
            mpred_nf_negation/2,
            mpred_nf_negations/2,
            mpred_no_chaining/1,
            mpred_no_spy/0,
            mpred_no_spy/1,
            mpred_no_spy/3,
            mpred_no_spy_all/0,
            mpred_no_trace/0,
            mpred_no_trace_all/0,
            mpred_no_warnings/0,
            mpred_no_watch/0,
            mpred_non_neg_literal/1,
            mpred_pbody/5,
            mpred_pbody_f/5,
            mpred_positive_literal/1,
            mpred_post/2,
            mpred_post1/2,
            mpred_post1_sp_0/2,
            mpred_post1_sp_1/2,
            mpred_post_sp/2,
            mpred_post_sp_zzz/2,
            mpred_post_sp_zzzz/2,
            mpred_prove_neg/1,
            
            pfc_provide_storage_op/2,
            mpred_rem/1,
            mpred_rem1/1,
            mpred_rem1/2,
            mpred_rem2/1,
            mpred_rem2a/1,
            mpred_rem2a/2,
            mpred_rem_actiontrace/2,
            mpred_rem_support/3,
            mpred_remove3/1,
            mpred_remove_old_version/1,
            mpred_remove_supports_f_l/2,
            mpred_remove_supports_quietly/1,
            mpred_reset/0,
            mpred_retract_db_type/1,
            mpred_retract_db_type/2,
            mpred_retract_or_warn_i/1,
            mpred_retract_support_relations/2,
            mpred_rewrap_h/2,
            mpred_rule_hb/3,
            mpred_rule_hb_0/3,
            mpred_run/0,
            mpred_run0/0,
            mpred_run_maybe/0,
            mpred_scan_tms/1,
            mpred_slow_search/0,
            mpred_is_spying/2,
            mpred_spy/1,
            mpred_spy/2,
            mpred_spy/3,
            mpred_spy1/3,
            mpred_spy_all/0,
            mpred_step/0,
            mpred_step0/0,
            mpred_test/1,
            mpred_tms_supported/2,
            mpred_tms_supported/3,
            mpred_tms_supported0/3,
            mpred_trace/0,
            mpred_trace/1,
            mpred_trace/2,
            mpred_trace_add/2,
            mpred_trace_addPrint/2,
            mpred_trace_addPrint_0/2,
            mpred_trace_exec/0,
            mpred_trace_break/2,
            mpred_trace_msg/1,
            mpred_trace_msg/2,
            mpred_trace_rem/2,
            mpred_is_tracing/1,
            mpred_undo/2,
            mpred_undo_e/2,
            mpred_undo_u/2,
            mpred_unfwc/1,
            mpred_unfwc1/1,
            mpred_unfwc_check_triggers/2,
            mpred_union/3,
            mpred_unique_i/1,
            mpred_unique_u/1,
            mpred_untrace/0,
            mpred_untrace/1,
            mpred_update_literal/4,
            mpred_user_fact/1,
            mpred_warn/0,
            mpred_warn/1,
            mpred_warn/2,
            mpred_warnings/0,
            mpred_warnings/1,
            mpred_watch/0,
            mpred_wff/3,
            mpred_wfflist/2,
            never_assert_u/1,
            never_retract_u/1,
            no_side_effects/1,
            nompred_warn/0,
            nonfact_metawrapper/1,
            not_cond/2,
            pfcBC_Cache/1,
            pfcBC_NoFacts/1,
            pfcBC_NoFacts_TRY/1,
            pfcVerifyMissing/3,
            pfcVersion/1,
            pfcl_do/1,
            physical_side_effect/1,
            pred_all/1,
            pred_head/2,
            pred_head_all/1,
            pred_r0/1,
            pred_t0/1,
            pred_u0/1,
            pred_u1/1,
            pred_u2/1,
            process_rule/3,
            record_se/0,
            reduce_clause_from_fwd/2,
            rem_list/1,
            remove_if_unsupported/2,
            remove_if_unsupported_verbose/3,
            remove_selection/2,
            repropagate/1,
            repropagate_0/1,
            repropagate_1/1,
            repropagate_meta_wrapper_rule/1,
            rescan_pfc/0,
            kb:resolveConflict0/1,
            retract_eq_quitely/1,
            retract_eq_quitely_f/1,
            retract_i/1,
            retract_t/1,
            retract_u/1,
            retractall_i/1,
            retractall_u/1,
            rewritten_metawrapper/1,
            ruleBackward/2,
            ruleBackward0/2,
            select_next_fact/2,
            set_prolog_stack_gb/1,
            should_call_for_facts/1,
            should_call_for_facts/3,
            show_if_debug/1,
            spft_precanonical/3,
            sub_term_eq/2,
            sub_term_v/2,
            support_ok_via_clause_body/1,
            support_ok_via_clause_body/3,
            supports_f_l/2,
            to_addable_form/2,
            to_addable_form_wte/3,
            to_predicate_isas/2,
            to_predicate_isas0/2,
            to_predicate_isas_each/2,
            trigger_supports_f_l/2,
            undoable/1,
            update_single_valued_arg/2,
            use_presently/0,
            user_atom/1,
            wac/0,
            wellFounded/2,
            with_mpred_trace_exec/1,
            wlmuser/1
        
          ]).
/*
:- was_shared_multifile    
        lmconf:hook_one_minute_timer_tick/0,
        lmconf:infoF/1,
        lmconf:mpred_hook_rescan_files/0,
        kb:resolveConflict/1,
        kb:resolverConflict_robot/1.
*/
:- meta_predicate 
      attvar_op(1,*),
      brake(0),
      call_i(0),
      call_prologsys(0),
      call_u(0),
      cnstrn(0),
      cnstrn(?,0),
      cnstrn0(0,*),
      fc_eval_action(0,*),
      foreachl_do(0,*),
      is_resolved(0),
      kb:resolveConflict(0),
      kb:resolveConflict0(0),
      kb:resolverConflict_robot(0),
      loop_check_nr(0),
      map_unless(1,:,*,*),
      mpred_add_fast_sp(?,0),
      mpred_add_fast_sp0(?,0),
      mpred_add_minfo(1,*),
      mpred_add_minfo_2(1,*),
      mpred_add_rule_if_rule(0),
      mpred_bc_only(0),
      mpred_call(0),
      mpred_call_0(0),
      mpred_call_only_facts(*,0),
      mpred_call_only_facts(0),
      mpred_call_with_no_triggers(0),
      mpred_deep_support(*,0),
      mpred_deep_support0(*,0),
      mpred_enqueue(0,?),
      mpred_fact(*,0),
      mpred_facts_and_universe(0),
      mpred_facts_only(0),
      mpred_fwd(0),
      mpred_fwd(0,?),
      mpred_fwd1(0,*),
      mpred_fwd2(0,*),
      mpred_post1_sp_0(?,0),
      mpred_post1_sp_1(?,0),
      mpred_post_sp(?,0),
      mpred_post_sp_zzz(?,0),
      mpred_post_sp_zzzz(?,0),
      mpred_prove_neg(0),
      mpred_rem(0),
      mpred_rem1(0),
      mpred_rem1(0,*),
      mpred_rem2(0),
      mpred_rem2a(0),
      mpred_rem2a(0,?),
      mpred_rem_support(*,0,*),
      mpred_remove_supports_f_l(*,0),
      mpred_remove_supports_quietly(0),
      mpred_scan_tms(0),
      mpred_tms_supported(*,0,?),
      mpred_tms_supported(0,?),
      mpred_tms_supported0(*,0,?),
      mpred_update_literal(*,*,0,*),
      not_cond(*,0),
      pfc_provide_storage_op(*,0),
      pfcBC_Cache(0),
      pfcBC_NoFacts(0),
      pfcl_do(0),
      physical_side_effect(0),
      pred_head(1,*),
      remove_if_unsupported(*,0),
      remove_if_unsupported_verbose(*,*,0),
      repropagate_0(0),
      repropagate_1(0),
      repropagate_meta_wrapper_rule(0),
      update_single_valued_arg(0,*),
      with_mpred_trace_exec(0).

:- meta_predicate((
      attvar_op(1,*),
      brake((*)),
      call_i((*)),
      call_prologsys((*)),
      call_u((*)),
      cnstrn((*)),
      cnstrn(?,(*)),
      cnstrn0((*),*),
      fc_eval_action((*),*),
      foreachl_do((*),*),
      is_resolved((*)),
      kb:resolveConflict((*)),
      kb:resolveConflict0((*)),
      kb:resolverConflict_robot((*)),
      loop_check_nr((*)),
      map_unless(1,:,*,*),
      mpred_add_fast_sp(?,(*)),
      mpred_add_fast_sp0(?,(*)),
      mpred_add_minfo(1,*),
      mpred_add_minfo_2(1,*),
      mpred_add_rule_if_rule((*)),
      mpred_bc_only((*)),
      mpred_call((*)),
      mpred_call_0((*)),
      mpred_call_only_facts(*,(*)),
      mpred_call_only_facts((*)),
      mpred_call_with_no_triggers((*)),
      mpred_deep_support(*,(*)),
      mpred_deep_support0(*,(*)),
      mpred_enqueue((*),?),
      mpred_fact(*,(*)),
      mpred_facts_and_universe((*)),
      mpred_facts_only((*)),
      mpred_fwd((*)),
      mpred_fwd((*),?),
      mpred_fwd1((*),*),
      mpred_fwd2((*),*),
      mpred_post1_sp_0(?,(*)),
      mpred_post1_sp_1(?,(*)),
      mpred_post_sp(?,(*)),
      mpred_post_sp_zzz(?,(*)),
      mpred_post_sp_zzzz(?,(*)),
      mpred_prove_neg((*)),
      mpred_rem((*)),
      mpred_rem1((*)),
      mpred_rem1((*),*),
      mpred_rem2((*)),
      mpred_rem2a((*)),
      mpred_rem2a((*),?),
      mpred_rem_support(*,(*),*),
      mpred_remove_supports_f_l(*,(*)),
      mpred_remove_supports_quietly((*)),
      mpred_scan_tms((*)),
      mpred_tms_supported(*,(*),?),
      mpred_tms_supported((*),?),
      mpred_tms_supported0(*,(*),?),
      mpred_update_literal(*,*,(*),*),
      not_cond(*,(*)),
      pfc_provide_storage_op(*,(*)),
      pfcBC_Cache((*)),
      pfcBC_NoFacts((*)),
      pfcl_do((*)),
      physical_side_effect((*)),
      pred_head(1,*),
      remove_if_unsupported(*,(*)),
      remove_if_unsupported_verbose(*,*,(*)),
      repropagate_0((*)),
      repropagate_1((*)),
      repropagate_meta_wrapper_rule((*)),
      update_single_valued_arg((*),*),
      with_mpred_trace_exec((*)))).

:- use_module(logicmoo(mpred/mpred_pfc)).
:- dynamic((
        mpred_hide_msg/1,
        mpred_is_spying/2,
        mpred_is_tracing/1,
        mpred_warnings/1,
        never_assert_u/1,
        never_retract_u/1,
        %mpred_select/2,
        mpred_is_tracing_exec/0,
        use_presently/0)).


:- was_shared_multifile(('==>')/1).
:- was_shared_multifile(kbp:bt/3).
:- was_shared_multifile(kbp:nt/4).
:- was_shared_multifile(kbp:pk/4).
:- was_shared_multifile(kbp:pt/3).
:- was_shared_multifile(kbp:spft/5).
:- was_shared_multifile(('::::')/2).
:- was_shared_multifile(('<-')/2).
:- was_shared_multifile(('<==>')/2).
:- was_shared_multifile(('==>')/2).
:- was_shared_multifile(('neg')/1).
:- was_shared_multifile(('nesc')/1).
:- was_shared_multifile((('neg'))/1).
:- was_shared_multifile((mpred_action)/1).
:- was_shared_multifile(kbp:tms/1).
:- was_shared_multifile(kbp:hs/1).
:- was_shared_multifile(kbp:qu/3).
:- was_shared_multifile(kbp:sm/1).
:- was_shared_multifile(kbp:sm/1).
:- was_shared_multifile(mpred_do_and_undo_method/2).
:- foreach(arg(_,isEach(prologMultiValued,prologOrdered,prologNegByFailure,prologPTTP,prologKIF,pfcControlled,tPredType,
     prologHybrid,predCanHaveSingletons,prologDynamic,prologBuiltin,prologMacroHead,prologListValued,prologSingleValued),P),
   ((was_shared_multifile(kb:P/1)))).
:- was_shared_multifile(kbp:hs/2).
:- was_shared_multifile(pfcControlled/1).
:- was_shared_multifile(prologDynamic/2).
:- was_shared_multifile(prologSideEffects/1).
:- was_shared_multifile(prologSingleValued/1).
:- was_shared_multifile(singleValuedInArg/2).
:- was_shared_multifile(prologSideEffects/1).

:- source_location(F,_),asserta(kb:never_registered_mpred_file(F)).
:- was_dynamic(lmconf:module_local_init/0).
:- discontiguous(lmconf:module_local_init/0).

:- include('mpred_header.pi').

% ======================= mpred_file('pfcsyntax').	% operator declarations.
:- was_module_transparent(wlmuser/1).
:- was_export(wlmuser/1).
wlmuser(G):- lmconf:mpred_user_kb(M), M:call(G).

%   File   : pfcsyntax.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Purpose: syntactic sugar for Pfc - operator definitions and term expansions.

:- op(500,fx,'~').
:- op(1050,xfx,('<-')).
:- op(1050,xfx,'<==>').
:- op(1050,xfx,('<-')).
:- op(1100,fx,('nesc')).
:- op(1150,xfx,('::::')).

:- op(500,fx,'~').
:- op(1050,xfx,'<==>').
:- op(1050,xfx,('<-')).
:- op(1200,fx,('=>')).
:- op(1200,fx,('==>')).
:- op(1100,fx,('nesc')).
:- op(1150,xfx,('::::')).
:- op(300,fx,'-').
:- op(600,yfx,'&').  
:- op(600,yfx,'v').
:- op(1075,xfx,'<-').
:- op(1075,xfx,'<=').
:- op(1070,xfx,'=>').
:- op(1070,xfx,'<=>').
:- op(1100,xfx,('<==>')).
:- op(1100,xfx,('==>')).
:- op(350,xfx,'xor').
:- op(300,fx,user:'~').
:- op(300,fx,user:'-').
:- op(400,yfx,user:'&').  
:- op(500,yfx,user:'v').
:- op(1075,xfx,user:'<-').
:- op(1075,xfx,user:'<==>').
:- op(350,xfx,user:'xor').



%mpred_call_shared(G):- kb:if_defined(G).
mpred_call_shared(G):- if_defined(G).

% ======================= mpred_file('pfccore').	% core of Pfc.

%   File   : pfccore.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Updated: 10/11/87, ...
%            4/2/91 by R. McEntire: added calls to valid_dbref as a
%                                   workaround for the Quintus 3.1
%                            bug in the recorded database.
%   Purpose: core Pfc predicates.

/*

LogicMOO is mixing Mark Stickel's PTTP (prolog techn theorem prover) to create horn clauses that 
 PFC forwards and helps maintain in visible states )  in prolog knowledge baseable.. We use kbp:spft/5 to track deductions
Research-wise LogicMOO has a main purpose is to prove that grounded negations (of contrapostives) are of first class in importance in helping
with Wff checking/TMS 
Also alows an inference engine constrain search.. PFC became important since it helps memoize and close off (terminate) transitive closures

*/

is_side_effect_disabled:- t_l:no_physical_side_effects,!.
is_side_effect_disabled:- t_l:side_effect_ok,!,fail.
is_side_effect_disabled:- t_l:noDBaseMODs(_),!.


          f_to_mfa(EF,R,F,A):-w_get_fa(EF,F,A),
              (((current_predicate(_:F/A),functor(P,F,A),predicate_property(_M:P,imported_from(R)))*->true;
              current_predicate(_:F/A),functor(P,F,A),source_file(R:P,_SF))),
              current_predicate(R:F/A).

w_get_fa(PI,_F,_A):-is_ftVar(PI),!.
w_get_fa(F/A,F,A):- !.
w_get_fa(PI,PI,_A):- atomic(PI),!.
w_get_fa(PI,F,A):- is_ftCompound(PI),!,functor(PI,F,A).
w_get_fa(Mask,F,A):-get_functor(Mask,F,A).


set_prolog_stack_gb(Six):-set_prolog_stack(global, limit(Six*10**9)),set_prolog_stack(local, limit(Six*10**9)),set_prolog_stack(trail, limit(Six*10**9)).
lmconf:module_local_init:-set_prolog_stack_gb(16).
:- was_shared_multifile(lmconf:mpred_hook_rescan_files/0).
:- was_dynamic(lmconf:mpred_hook_rescan_files/0).
:- was_dynamic(use_presently/0).
% used to annotate a predciate to indicate PFC support
:- was_shared_multifile(infoF/1).
:- was_dynamic(infoF/1).
:- was_export(infoF/1).

% :- set_prolog_flag(access_level,system).

is_mpred_action('$VAR'(_)):-!,fail.
is_mpred_action(remove_if_unsupported(_,_)).
is_mpred_action(P):-predicate_property(P,static).
mpred_is_builtin(P):-predicate_property(P,built_in).

/* UNUSED TODAY

:- use_module(library(mavis)).
:- use_module(library(type_check)).
:- use_module(library(typedef)).
*/

:- use_module(library(lists)).
:- meta_predicate with_mpred_trace_exec(*).

% :- use_module(library(dra/tabling3/swi_toplevel)).

:- discontiguous(mpred_file_expansion_0/2).
/*
compiled(F/A):- was_dynamic(F/A),compile_predicates([F/A]).
:- compiled(('nesc')/1).
:- compiled(('neg')/1).
:- compiled(('<-')/2).
:- compiled(('==>')/2).
:- compiled(('::::')/2).
:- compiled(('<==>')/2).
*/

:- thread_local((t_l:use_side_effect_buffer , t_l:verify_side_effect_buffer)).
record_se:- (t_l:use_side_effect_buffer ; t_l:verify_side_effect_buffer).


add_side_effect(_,_):- ( \+  record_se ),!.
add_side_effect(Op,Data):-current_why(Why),assert(t_l:side_effect_buffer(Op,Data,Why)).

attvar_op(Op,Data0):- add_side_effect(Op,Data0),unnumbervars_and_save(Data0,Data),physical_side_effect(call(Op,Data)).
erase_w_attvars(Data0,Ref):- physical_side_effect(erase(Ref)),add_side_effect(erase,Data0).



:- thread_local(t_l:no_physical_side_effects/0).
physical_side_effect(PSE):- is_side_effect_disabled,!,mpred_warn('no_physical_side_effects ~p',PSE).
physical_side_effect(PSE):- PSE.
mpred_no_chaining(Goal):- w_tl(t_l:no_physical_side_effects,call(Goal)).


% TODO ISSUE https://github.com/TeamSPoon/PrologMUD/issues/7
match_source_ref1(u):-!.
match_source_ref1(u(_)).
make_uu_remove((U,U)):-match_source_ref1(U).

% TODO ISSUE https://github.com/TeamSPoon/PrologMUD/issues/7
:- was_export(get_source_ref/1).
get_source_ref1(u):-!.
get_source_ref1(u(Mt)):-current_why(Mt),!.
get_source_ref1(u(Mt)):-Mt=mt.
get_source_ref((U,U)):- get_source_ref1(U).

has_functor(_):-!,fail.
has_functor(F/A):-!,atom(F),integer(A),!.
has_functor(C):- (\+ is_ftCompound(C)),!,fail.
has_functor(C):-is_ftCompound(C),\+is_list(C).

mpred_each_literal(P,E):-is_ftNonvar(P),P=(P1,P2),!,(mpred_each_literal(P1,E);mpred_each_literal(P2,E)).
mpred_each_literal(P,P). %:-conjuncts_to_list(P,List),member(E,List).


%:- ensure_loaded(logicmoo(snark/common_logic_sexpr)).

:- ensure_loaded(logicmoo(mpred/mpred_loader)).

to_addable_form_wte(Why,I,O):-string(I),must_det_l((input_to_forms(string(I),Wff,Vs),b_setval('$variable_names',Vs),!,sexpr_sterm_to_pterm(Wff,PTerm),
  to_addable_form_wte(Why,PTerm,O))).
to_addable_form_wte(Why,I,O):-atom(I),atom_contains(I,'('),must_det_l((input_to_forms(atom(I),Wff,Vs),b_setval('$variable_names',Vs),!,sexpr_sterm_to_pterm(Wff,PTerm),
  to_addable_form_wte(Why,PTerm,O))).

to_addable_form_wte(_,X,X):-mpred_call_shared(as_is_term(X)),!.
to_addable_form_wte(Why,nesc(I),O):-!,to_addable_form_wte(Why,I,O).
to_addable_form_wte(Why,USER:I,O):-USER==user,!,to_addable_form_wte(Why,I,O).
to_addable_form_wte(_Why,neg(USER:P0),neg(P0)):-USER==user,!.
to_addable_form_wte(_Why,neg(P0),neg(P0)):-!.
to_addable_form_wte(assert,(H:-B),(H:-B)):-B\==true,!.
to_addable_form_wte(Why,(CUT,P0),(CUT,P)):-mpred_is_builtin(CUT),!,to_addable_form_wte(Why,P0,P).
to_addable_form_wte(Why,P0,P):-
    once(cnotrace(to_addable_form(P0,P));must(to_addable_form(P0,P))),
    ignore((((P0\=@=P,P0\=isa(_,_)),mpred_trace_msg((to_addable_form(Why):-[P0,P]))))),!.

retract_eq_quitely(H):-ignore(retract_eq_quitely_f(H)).
retract_eq_quitely_f((H:-B)):- ((clause(H,B,Ref),clause(HH,BB,Ref),H=@=HH,B=@=BB,!,erase_w_attvars(clause(HH,BB,Ref),Ref))).
retract_eq_quitely_f((H)):- ((clause(H,true,Ref),clause(HH,BB,Ref),H=@=HH,BB==true,!,erase_w_attvars(clause(HH,BB,Ref),Ref))).

assert_eq_quitely(H):- attvar_op(assert_if_new,H).

reduce_clause_from_fwd(H,H):- (\+is_ftCompound(H)),!.
reduce_clause_from_fwd((H:-B),HH):-B==true,reduce_clause_from_fwd(H,HH).
reduce_clause_from_fwd((B==>H),HH):-B==true,reduce_clause_from_fwd(H,HH).
reduce_clause_from_fwd((==>H),HH):-!,reduce_clause_from_fwd(H,HH).
reduce_clause_from_fwd((H<- B),HH):-B==true,reduce_clause_from_fwd(H,HH).
reduce_clause_from_fwd((B<==> H),HH):-B==true,reduce_clause_from_fwd(H,HH).
reduce_clause_from_fwd((H<==> B),HH):-B==true,reduce_clause_from_fwd(H,HH).
reduce_clause_from_fwd((H,B),(HH,BB)):-!,reduce_clause_from_fwd(H,HH),reduce_clause_from_fwd(B,BB).
reduce_clause_from_fwd(H,H).


to_addable_form(I,I):-is_ftVar(I),!.
to_addable_form(I,OOO):-is_list(I),!,must_maplist(to_addable_form,I,O),flatten(O,OO),!,reduce_clause_from_fwd(OO,OOO).
to_addable_form(I,OO):- current_predicate(mpred_term_expansion_file/0),once(fully_expand(_,I,II)),!,
 once((into_mpred_form(II,M),to_predicate_isas_each(M,O))),!,reduce_clause_from_fwd(O,OO).
to_addable_form(I,O):- findall(M,do_expand_args(isEach,I,M),IM),list_to_conjuncts(IM,M),to_predicate_isas_each(M,O),!.

to_predicate_isas_each(I,O):-to_predicate_isas(I,O).

to_predicate_isas(V,V):- (\+is_ftCompound(V)),!.
to_predicate_isas([H|T],[HH|TT]):-!,to_predicate_isas(H,HH),to_predicate_isas(T,TT),!.
to_predicate_isas((H,T),(HH,TT)):-!,to_predicate_isas(H,HH),to_predicate_isas(T,TT),!.
%to_predicate_isas(I,I):-contains_term(S,I),is_ftNonvar(S),exact_args(S),!.
to_predicate_isas(I,O):-must(to_predicate_isas0(I,O)),!.

append_as_first_arg(C,I,V):-C=..[F|ARGS],V=..[F,I|ARGS].

to_predicate_isas0(V,V):- (\+is_ftCompound(V)),!.
to_predicate_isas0({V},{V}):-!.
to_predicate_isas0(eXact(V),V):-!.
to_predicate_isas0(t(C,I),V):-atom(C)->V=..[C,I];(is_ftVar(C)->V=t(C,I);append_as_first_arg(C,I,V)).
to_predicate_isas0(isa(I,C),V):-!,atom(C)->V=..[C,I];(is_ftVar(C)->V=isa(I,C);append_as_first_arg(C,I,V)).
to_predicate_isas0(C,C):-exact_args(C),!.
to_predicate_isas0([H|T],[HH|TT]):-!,to_predicate_isas0(H,HH),to_predicate_isas0(T,TT),!.
to_predicate_isas0(C,CO):-C=..[F|CL],must_maplist(to_predicate_isas0,CL,CLO),!,CO=..[F|CLO].

:- source_location(F,_),asserta(absolute_source_location_pfc(F)).
exact_args(Q):-is_ftVar(Q),!,fail.
exact_args(Q):- mpred_call_shared(argsQuoted(Q)).
exact_args(Q):-is_ftCompound(Q),functor(Q,F,_),mpred_call_shared(argsQuoted(F)).
exact_args(second_order(_,_)).
exact_args(call(_)).
exact_args(asserted(_)).
exact_args(retract_eq_quitely(_)).
exact_args(asserts_eq_quitely(_)).
exact_args(assertz_if_new(_)).
exact_args((_:-_)).
exact_args((_ =.. _)).
exact_args((:-( _))).
exact_args((A/B)):- (is_ftVar(A);is_ftVar(B)).
exact_args(mpred_add(_)).
exact_args(dynamic(_)).
exact_args(cwc).
exact_args(true).
% exact_args(C):-source_file(C,I),absolute_source_location_pfc(I).

mpred_is_tautology(Var):-is_ftVar(Var).
mpred_is_tautology(V):- \+ \+ call((copy_term_nat(V,VC),snumbervars(VC),mpred_is_taut(VC))).

mpred_is_taut(A:-B):-!,mpred_is_taut(B==>A).
mpred_is_taut(A<-B):-!,mpred_is_taut(B==>A).
mpred_is_taut(A<==>B):-!,(mpred_is_taut(A==>B);mpred_is_taut(B==>A)).
mpred_is_taut(A==>B):- A==B,!.
mpred_is_taut((B,_)==>A):-is_ftNonvar(B),mpred_is_taut(A==>B),!.
mpred_is_taut((_,B)==>A):-is_ftNonvar(B),mpred_is_taut(A==>B),!.
mpred_is_taut(B==>(A,_)):-is_ftNonvar(A),mpred_is_taut(A==>B),!.
mpred_is_taut(B==>(_,A)):-is_ftNonvar(A),mpred_is_taut(A==>B),!.

loop_check_nr(CL):- loop_check(no_repeats(CL)).

% lmconf:decl_database_hook(Op,Hook):- loop_check_nr(pfc_provide_storage_op(Op,Hook)).

is_retract_first(one).
is_retract_first(a).

pfc_provide_storage_op(Op,(I1,I2)):-!,pfc_provide_storage_op(Op,I1),pfc_provide_storage_op(Op,I2).
pfc_provide_storage_op(Op,(nesc (P))):-!,pfc_provide_storage_op(Op,P).
%pfc_provide_storage_op(change(assert,_AorZ),Fact):- loop_check_nr(mpred_addPreTermExpansion(Fact)).
% pfcRem1 to just get the first
pfc_provide_storage_op(change(retract,OneOrA),FactOrRule):- is_retract_first(OneOrA),!,
            loop_check_nr(mpred_rem1(FactOrRule)),
  ignore((ground(FactOrRule),mpred_rem2(FactOrRule))).
% mpred_rem2 should be forcefull enough
pfc_provide_storage_op(change(retract,all),FactOrRule):- loop_check_nr(mpred_rem2(FactOrRule)),!.
% pfc_provide_storage_op(is_asserted,FactOrRule):- is_ftNonvar(FactOrRule),!,loop_check_nr(clause_i(FactOrRule)).

mpred_clause_is_asserted_hb_nonunify(H,B):- clause( ==>( B , H) , true).
mpred_clause_is_asserted_hb_nonunify(H,B):- clause( <-( H , B) , true).
mpred_clause_is_asserted_hb_nonunify(_,_):-!,fail.
mpred_clause_is_asserted_hb_nonunify(G, T   ):- T==true,!,hotrace(mpred_rule_hb(G,H,B)),G\=@=H,!,mpred_clause_is_asserted(H,B).
mpred_clause_is_asserted_hb_nonunify(H,(T,B)):- T==true,!,mpred_clause_is_asserted_hb_nonunify(H,B).
mpred_clause_is_asserted_hb_nonunify(H,(B,T)):- T==true,!,mpred_clause_is_asserted_hb_nonunify(H,B).
mpred_clause_is_asserted_hb_nonunify(H,B):- clause_u( <-( H , B) , true).
mpred_clause_is_asserted_hb_nonunify(H,B):- mpred_clause_is_asserted(H,B).

mpred_clause_is_asserted(H,B):- is_ftVar(H),is_ftNonvar(B),!,fail.
mpred_clause_is_asserted(H,B):- has_cl(H) -> clause(H,B) ; mpred_clause_is_asserted_hb_nonunify(H,B).
%mpred_clause_is_asserted(H,B,Ref):- clause_u(H,B,Ref).


% pfcDatabaseGoal(G):-is_ftCompound(G),get_functor(G,F,A),pfcDatabaseTerm(F/A).

lmconf:mpred_provide_storage_clauses(pfc,H,B,Proof):-mpred_clause(H,B,Proof).

%mpred_clause('nesc'(H),B,forward(Proof)):- is_ftNonvar(H),!, lmconf:mpred_provide_storage_clauses(H,B,Proof).
%mpred_clause(H,B,forward(R)):- R=(==>(B,H)),clause(R,true).
mpred_clause(H,B,Why):-has_cl(H),clause(H,CL,R),mpred_pbody(H,CL,R,B,Why).
%mpred_clause(H,B,backward(R)):- R=(<-(H,B)),clause(R,true).
%mpred_clause(H,B,equiv(R)):- R=(<==>(LS,RS)),clause(R,true),(((LS=H,RS=B));((LS=B,RS=H))).
% mpred_clause(H,true, pfcTypeFull(R,Type)):-is_ftNonvar(H),!,pfcDatabaseTerm(F/A),make_functor(R,F,A),pfcRuleOutcomeHead(R,H),clause(R,true),pfcTypeFull(R,Type),Type\=rule.
% mpred_clause(H,true, pfcTypeFull(R)):-pfcDatabaseTerm(F/A),make_functor(R,F,A),pfcTypeFull(R,Type),Type\=rule,clause(R,true),once(pfcRuleOutcomeHead(R,H)).

mpred_pbody(_H,mpred_bc_only(_BC),_R,fail,deduced(backchains)):-!.
mpred_pbody(H,infoF(INFO),R,B,Why):-!,mpred_pbody_f(H,INFO,R,B,Why).
mpred_pbody(H,B,R,BIn,WHY):- is_true(B),!,BIn=B,get_why(H,H,R,WHY).
mpred_pbody(H,B,R,B,asserted(R,(H:-B))).

get_why(_,CL,R,asserted(R,CL)):- clause(kbp:spft(ukb,CL, U, U, _Why),true),!.
get_why(H,CL,R,deduced(R,WHY)):-mpred_get_support(H,WH)*->WHY=(H=WH);(mpred_get_support(CL,WH),WHY=(CL=WH)).


mpred_pbody_f(H,CL,R,B,WHY):- CL=(B==>HH),sub_term_eq(H,HH),!,get_why(H,CL,R,WHY).
mpred_pbody_f(H,CL,R,B,WHY):- CL=(HH<-B),sub_term_eq(H,HH),!,get_why(H,CL,R,WHY).
mpred_pbody_f(H,CL,R,B,WHY):- CL=(HH<==>B),sub_term_eq(H,HH),get_why(H,CL,R,WHY).
mpred_pbody_f(H,CL,R,B,WHY):- CL=(B<==>HH),sub_term_eq(H,HH),!,get_why(H,CL,R,WHY).
mpred_pbody_f(H,CL,R,fail,infoF(CL)):- trace_or_throw(mpred_pbody_f(H,CL,R)).

sub_term_eq(H,HH):-H==HH,!.
sub_term_eq(H,HH):-each_subterm(HH,ST),ST==H,!.
sub_term_v(H,HH):-H=@=HH,!.
sub_term_v(H,HH):-each_subterm(HH,ST),ST=@=H,!.


mpred_rule_hb(Outcome,OutcomeO,AnteO):-mpred_rule_hb_0(Outcome,OutcomeO,Ante),mpred_rule_hb_0(Ante,AnteO,_).

mpred_rule_hb_0(Outcome,OutcomeO,true):-is_ftVar(Outcome),!,OutcomeO=Outcome.
mpred_rule_hb_0((Outcome1,Outcome2),OutcomeO,AnteO):-!,mpred_rule_hb(Outcome1,Outcome1O,Ante1),mpred_rule_hb(Outcome2,Outcome2O,Ante2),
                   conjoin(Outcome1O,Outcome2O,OutcomeO),
                   conjoin(Ante1,Ante2,AnteO).
mpred_rule_hb_0(Ante1==>Outcome,OutcomeO,(Ante1,Ante2)):-!,mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(Outcome<-Ante1,OutcomeO,(Ante1,Ante2)):-!,mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(Outcome<==>Ante1,OutcomeO,(Ante1,Ante2)):-mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(Ante1<==>Outcome,OutcomeO,(Ante1,Ante2)):-!,mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(_::::Outcome,OutcomeO,Ante2):-!,mpred_rule_hb_0(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(kbp:bt(umt,Outcome,Ante1),OutcomeO,(Ante1,Ante2)):-!,mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(kbp:pt(umt,Ante1,Outcome),OutcomeO,(Ante1,Ante2)):-!,mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(kbp:pk(umt,Ante1a,Ante1b,Outcome),OutcomeO,(Ante1a,Ante1b,Ante2)):-!,mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(kbp:nt(umt,Ante1a,Ante1b,Outcome),OutcomeO,(Ante1a,Ante1b,Ante2)):-!,mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(kbp:spft(ukb,Outcome,Ante1a,Ante1b,_),OutcomeO,(Ante1a,Ante1b,Ante2)):-!,mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(kbp:qu(umt,Outcome,_),OutcomeO,Ante2):-!,mpred_rule_hb(Outcome,OutcomeO,Ante2).
% mpred_rule_hb_0(pfc Default(Outcome),OutcomeO,Ante2):-!,mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0((Outcome:-Ante),Outcome,Ante):-!.
mpred_rule_hb_0(Outcome,Outcome,true).

mpred_add_minfo(G):-mpred_add_minfo(assertz_if_new,G).
mpred_add_minfo(How,(H:-True)):-is_true(True),must(is_ftNonvar(H)),!,mpred_add_minfo(How,H).
mpred_add_minfo(How,(H<-B)):- !,mpred_add_minfo(How,(H:-infoF(H<-B))),!,mpred_add_minfo(How,(H:-mpred_bc_only(H))),mpred_add_minfo_2(How,(B:-infoF(H<-B))).
mpred_add_minfo(How,(B==>H)):- !,mpred_add_minfo(How,(H:-infoF(B==>H))),!,mpred_add_minfo_2(How,(B:-infoF(B==>H))).
mpred_add_minfo(How,(B<==>H)):- !,mpred_add_minfo(How,(H:-infoF(B<==>H))),!,mpred_add_minfo(How,(B:-infoF(B<==>H))),!.
mpred_add_minfo(How,((A,B):-INFOC)):-mpred_is_info(INFOC),(is_ftNonvar(A);is_ftNonvar(B)),!,mpred_add_minfo(How,((A):-INFOC)),mpred_add_minfo(How,((B):-INFOC)),!.
mpred_add_minfo(How,((A;B):-INFOC)):-mpred_is_info(INFOC),(is_ftNonvar(A);is_ftNonvar(B)),!,mpred_add_minfo(How,((A):-INFOC)),mpred_add_minfo(How,((B):-INFOC)),!.
mpred_add_minfo(How,(~(A):-infoF(C))):-is_ftNonvar(C),is_ftNonvar(A),!,mpred_add_minfo(How,((A):-infoF((C)))). % attvar_op(How,(~(A):-infoF(C))).
mpred_add_minfo(How,(neg(A):-infoF(C))):-is_ftNonvar(C),is_ftNonvar(A),!,mpred_add_minfo(How,((A):-infoF((C)))). % attvar_op(How,(~(A):-infoF(C))).
mpred_add_minfo(How,(A:-INFOC)):-is_ftNonvar(INFOC),INFOC= mpred_bc_only(A),!,attvar_op(How,(A:-INFOC)),!.
mpred_add_minfo(How,kbp:bt(umt,H,_)):-!,attvar_op(How,(H:-mpred_bc_only(H))).
mpred_add_minfo(How,kbp:nt(umt,H,Test,Body)):-!,attvar_op(How,(H:-fail,kbp:nt(umt,H,Test,Body))).
mpred_add_minfo(How,kbp:pt(umt,H,Body)):-!,attvar_op(How,(H:-fail,kbp:pt(umt,H,Body))).
mpred_add_minfo(How,(A0:-INFOC0)):- mpred_is_info(INFOC0), copy_term((A0:-INFOC0),(A:-INFOC)),!,must((mpred_rewrap_h(A,AA),imploded_copyvars((AA:-INFOC),ALLINFO), attvar_op(How,(ALLINFO)))),!.
%mpred_add_minfo(How,G):-mpred_trace_msg(skipped_add_meta_facts(How,G)).
mpred_add_minfo(_,_).

:- was_export(mpred_add_minfo_2/2).
mpred_add_minfo_2(How,G):-mpred_add_minfo(How,G).

mpred_is_info(mpred_bc_only(C)):-is_ftNonvar(C),!.
mpred_is_info(infoF(C)):-is_ftNonvar(C),!.



%:- was_dynamic(not_not/1).
mpred_rewrap_h(A,A):-is_ftNonvar(A),\+ is_static_pred(A).
mpred_rewrap_h(A,F):- functor(A,F,_),\+ is_static_pred(F),!.
%mpred_rewrap_h(A,not_not(A)):-!.

cwc:-true.
fwc:-true.
bwc:-true.
wac:-true.
is_fc_body(P):-cwc, has_body_atom(fwc,P).
is_bc_body(P):-cwc, has_body_atom(bwc,P).
is_action_body(P):-cwc, has_body_atom(wac,P).


has_body_atom(WAC,P):-cwc, call(
   WAC==P -> true ; (is_ftCompound(P),arg(1,P,E),has_body_atom(WAC,E))),!.

/*
has_body_atom(WAC,P,Rest):-cwc, call(WAC==P -> Rest = true ; (is_ftCompound(P),functor(P,F,A),is_atom_body_pfa(WAC,P,F,A,Rest))).
is_atom_body_pfa(WAC,P,F,2,Rest):-arg(1,P,E),E==WAC,arg(2,P,Rest),!.
is_atom_body_pfa(WAC,P,F,2,Rest):-arg(2,P,E),E==WAC,arg(1,P,Rest),!.
*/


:- thread_local(t_l:mpred_debug_local/0).
mpred_is_silient :- ( \+ t_l:mpred_debug_local, \+ mpred_is_tracing_exec, \+ mpred_is_tracing(_)) ,!.

:- meta_predicate(show_if_debug(0)).
show_if_debug(A):- !,show_call(A).
show_if_debug(A):- mpred_is_tracing(A) ->show_call(A) ; A.

% ======================= 
% user''s program''s database
% ======================= 
% assert_u(arity(prologHybrid,0)):-trace_or_throw(assert_u(arity(prologHybrid,0))).
% assert_u(X):- \+ (is_ftCompound(X)),!,asserta_u(X,X,0).

assert_u(X):- copy_term(X,Y),never_assert_u(Y),X=@=Y,trace_or_throw(never_assert_u(Y)).
assert_u(X):- functor(X,F,A),assert_u(X,F,A).

assert_u(X,F,_):-mpred_call_shared(singleValuedInArg(F,SV)),!,must(update_single_valued_arg(X,SV)),!.
assert_u(X,F,A):-mpred_call_shared(prologSingleValued(F)),!,must(update_single_valued_arg(X,A)),!.
% assert_u(X,F,A):-must(isa(F,prologAssertAOrdered) -> asserta_u(X,F,A) ; assertz_u(X,F,A)).
assert_u(X,F,A):- assertz_u(X,F,A).
% assert_u(X,F,A):-must(isa(F,prologOrdered) -> assertz_u(X,F,A) ; asserta_u(X,F,A)).


% asserta_u(X):- never_assert_u(Y),X=@=Y,trace_or_throw(never_assert_u(Y)).
% asserta_u(X):- functor(X,F,A),asserta_u(X,F,A).

% asserta_u(X,_,_):- show_call_success(clause_asserted(X)),!.
% asserta_u(X,_,_):- must((expire_tabled_list(X),show_if_debug(attvar_op(asserta,X)))).

assertz_u(X):- functor(X,F,A),assertz_u(X,F,A).

assertz_u(X,_,_):- never_assert_u(Y),X=@=Y,trace_or_throw(never_assert_u(Y)).
assertz_u(X,_,_):- clause_asserted(X),!.
assertz_u(X,_,_):- must((expire_tabled_list(X),show_if_debug(attvar_op(assertz,X)))).

:- was_dynamic(never_assert_u/1).
:- was_dynamic(never_retract_u/1).

retract_u(X):- never_retract_u(Y),X=@=Y,trace_or_throw(never_retract_u(Y)).
%retract_u(neg(X)):-must(is_ftNonvar(X)),!,retract_eq_quitely_f(neg(X)),must((expire_tabled_list(neg(X)))),must((expire_tabled_list((X)))).
%retract_u(kbp:hs(X)):-!,retract_eq_quitely_f(kbp:hs(X)),must((expire_tabled_list(neg(X)))),must((expire_tabled_list((X)))).
retract_u(kbp:qu(umt,X,Y)):-!,show_call_failure(retract_eq_quitely_f(kbp:qu(umt,X,Y))),must((expire_tabled_list(neg(X)))),must((expire_tabled_list((X)))).
retract_u(neg(X)):-!,show_call_success(retract_eq_quitely_f(neg(X))),must((expire_tabled_list(neg(X)))),must((expire_tabled_list((X)))).
retract_u((X)):-!,show_call_success(retract_eq_quitely_f((X))),must((expire_tabled_list(neg(X)))),must((expire_tabled_list((X)))).
retract_u(X):-show_if_debug(attvar_op(retract_eq,X)),!,must((expire_tabled_list(X))).

retractall_u(X):-retractall(X),must((expire_tabled_list(X))).
clause_u(H,B):- must(H\==true),catchvv(clause(H,B),_,fail).
clause_u(H,B,Ref):-must(H\==true),catchvv(clause(H,B,Ref),_,fail).

mpred_update_literal(P,N,Q,R):-
    arg(N,P,UPDATE),call(replace_arg(P,N,OLD,Q)),
    must(Q),update_value(OLD,UPDATE,NEW), 
    call(replace_arg(Q,N,NEW,R)).

update_single_valued_arg(P,N):-
   must(get_source_ref((U,U))),
  arg(N,P,UPDATE),call(replace_arg(P,N,OLD,Q)),
  current_why(Why),
  lmconf:mpred_system_kb(M), M:get_source_ref1(U),
  must_det_l((attvar_op(assert_if_new,kbp:spft(ukb,P,U,U,Why)),(mpred_call(P)->true;(assertz_u(P))),
     doall((clause(Q,true,E),UPDATE \== OLD,erase_w_attvars(clause(Q,true,E),E),mpred_unfwc1(Q))))).



% ======================= 
% prolog system database
% ======================= 
/*
assert_prologsys(X):- attvar_op(assert,X).
asserta_prologsys(X):- attvar_op(asserta,X).
assertz_prologsys(X):-attvar_op(assertz,X).

clause_prologsys(H,B):-clause(H,B).
clause_prologsys(H,B,Ref):-clause(H,B,Ref).
*/
call_prologsys(X):-call(X).

% ======================= 
% internal bookkeeping
% ======================= 
assert_i(X):- attvar_op(assert_if_new,X).
asserta_i(X):-attvar_op(asserta_if_new,X).
assertz_i(X):-attvar_op(assertz_if_new,X).
retract_i(X):-attvar_op(retract,X).
clause_i(H,B):-clause(H,B).
clause_i(H,B,Ref):-clause(H,B,Ref).
call_i(X):-call(X).
retractall_i(X):-attvar_op(retractall,X).

% ======================= 
% utils
% ======================= 
map_literals(P,G):-map_literals(P,G,[]).

map_literals(_,H,_):-is_ftVar(H),!. % skip over it
map_literals(_,[],_) :- !.
map_literals(Pred,(H,T),S):-!, apply(Pred,[H|S]), map_literals(Pred,T,S).
map_literals(Pred,[H|T],S):-!, apply(Pred,[H|S]), map_literals(Pred,T,S).
map_literals(Pred,H,S):- mpred_literal(H),must(apply(Pred,[H|S])),!.
map_literals(_Pred,H,_S):- \+ is_ftCompound(H),!. % skip over it
map_literals(Pred,H,S):-H=..List,!,map_literals(Pred,List,S),!.


map_unless(Test,Pred,H,S):- call(Test,H),ignore(apply(Pred,[H|S])),!.
map_unless(_Test,_,[],_) :- !.
map_unless(_Test,_Pred,H,_S):- \+ is_ftCompound(H),!. % skip over it
map_unless(Test,Pred,(H,T),S):-!, apply(Pred,[H|S]), map_unless(Test,Pred,T,S).
map_unless(Test,Pred,[H|T],S):-!, apply(Pred,[H|S]), map_unless(Test,Pred,T,S).
map_unless(Test,Pred,H,S):-H=..List,!,map_unless(Test,Pred,List,S),!.

mpred_maptree(Pred,List):-mpred_maptree(Pred,List,[]).
mpred_maptree(Pred,H,S):-is_ftVar(H),!,apply(Pred,[H|S]).
mpred_maptree(_,[],_) :- !.
mpred_maptree(Pred,(H,T),S):-!, mpred_maptree(Pred,H,S), mpred_maptree(Pred,T,S).
mpred_maptree(Pred,(H;T),S):-!, mpred_maptree(Pred,H,S) ; mpred_maptree(Pred,T,S).
mpred_maptree(Pred,[H|T],S):-!, apply(Pred,[H|S]), mpred_maptree(Pred,T,S).
mpred_maptree(Pred,H,S):-apply(Pred,[H|S]). 

% % :- use_module(logicmoo(util/rec_lambda)).



%example pfcVerifyMissing(mpred_isa(I,D), mpred_isa(I,C), ((mpred_isa(I,C), {D==C});~mpred_isa(I,C))). 
%example pfcVerifyMissing(mudColor(I,D), mudColor(I,C), ((mudColor(I,C), {D==C});~mudColor(I,C))). 

pfcVerifyMissing(GC, GO, ((GO, {D==C});\+ GO) ):-  GC=..[F,A|Args],append(Left,[D],Args),append(Left,[C],NewArgs),GO=..[F,A|NewArgs],!.

%example mpred_freeLastArg(mpred_isa(I,C),neg(mpred_isa(I,C))):-is_ftNonvar(C),!.
%example mpred_freeLastArg(mpred_isa(I,C),(mpred_isa(I,F),C\=F)):-!.
mpred_freeLastArg(G,GG):- G=..[F,A|Args],append(Left,[_],Args),append(Left,[_],NewArgs),GG=..[F,A|NewArgs],!.
mpred_freeLastArg(_G,false).

mpred_current_op_support((p,p)):-!.

pfcVersion(6.6).


correctify_support((S,T),(S,T)):-!.
correctify_support(U,(U,U)):-atom(U),!.
correctify_support([U],S):-correctify_support(U,S).

%=% initialization of global assertons

%= lmconf:mpred_init_i/1 initialized a global assertion.
%=  lmconf:mpred_init_i(P,Q) - if there is any fact unifying with P, then do
%=  nothing, else assert_db Q.

mpred_init_i(GeneralTerm,Default) :- ((
  clause_i(GeneralTerm,true) -> true ; assert_i(Default))).

%= kbp:tms is one of {none,local,cycles} and controles the tms alg.
lmconf:module_local_init:- must((mpred_init_i(kbp:tms(_), kbp:tms(cycles)))).

% Pfc Search strategy. kbp:sm(X) where X is one of {direct,depth,breadth}
lmconf:module_local_init:- mpred_init_i(kbp:sm(_), kbp:sm(direct)).

% aliases
mpred_adda(G):-mpred_add(G).
% a really common example is people want unbound predicate backchaining .. that is to query the predicates witha  varaible where the predciate is 
mpred_addz(G):-mpred_add(G).

%= mpred_add/2 and mpred_post/2 are the main ways to assert_db new clauses into the
%= database and have forward reasoning done.

%= mpred_add(P,S) asserts P into the user''s dataBase with support from S.
mpred_add(P) :- 
  mpred_add_fast(P).

mpred_add(P,S) :- 
  mpred_add_fast(P,S).


mpred_add_fast('$si$':'$was_imported_kb_content$'(_, _)<-THIS):-is_ftNonvar(THIS),!.
mpred_add_fast(P0):-
  must(get_source_ref(S)), mpred_add_fast(P0,S).

mpred_add_fast((nesc P),S) :- is_ftNonvar(P),!,
  mpred_add_fast(P,S).

mpred_add_fast(P0,S):- gripe_time(0.6,mpred_add_fast_timed(P0,S)).

mpred_add_fast_timed(P0,S):-
  must(to_addable_form_wte(assert,P0,P)),
      (is_list(P)
        ->must_maplist(mpred_add_fast_sp(S),P);
       mpred_add_fast_sp(S,P)).


mpred_add_fast_sp(S,P):- 
   mpred_add_fast_sp0(S,P).

% mpred_add_fast_sp(S,P->Q) :-!,mpred_add_fast_sp(S,P==>Q).
mpred_add_fast_sp0(S,P) :-
   mpred_rule_hb(P,OutcomeO,_),!,
     loop_check_term((mpred_post_sp_zzz(S,P),mpred_run_maybe),
     mpred_adding(OutcomeO),
     (mpred_post_sp_zzz(S,P),mpred_trace_msg(looped_outcome((P))))),!.
%mpred_add_fast_sp(_,_).
mpred_add_fast_sp0(P,S) :- mpred_error("mpred_add_fast(~p,~p) failed",[P,S]).



% mpred_post(+Ps,+S) tries to assert a fact or set of fact to the database.  For
% each fact (or the singelton) mpred_post1 is called. It always succeeds.

mpred_post([H|T],S) :-
  !,
  mpred_post1(H,S),
  mpred_post(T,S).
mpred_post([],_) :- !.
mpred_post(P,S) :-   
  mpred_post1(P,S).

% mpred_post1(+P,+S) tries to assert a fact to the database, and, if it succeeded,
% adds an entry to the pfc queue for subsequent forward chaining.
% It always succeeds.
mpred_post1(mpred_add(P0),S):- must(is_ftNonvar(P0)), !,mpred_add(P0,S).
mpred_post1(P0,S):-
  to_addable_form_wte(assert,P0,P),
      (is_list(P)
        ->maplist(mpred_post_sp_zzz(S),P);
       mpred_post_sp_zzz(S,P)).

mpred_post_sp(S,P):- mpred_post_sp_zzz(S,P).


mpred_post_sp_zzz(S,P):- ground(S:P),!,mpred_post_sp_zzzz(S,P),!.
mpred_post_sp_zzz(S,P):- \+ is_main_thread,!,
   (ensure_vars_labled(S:P,S0:P0)-> 
     mpred_post_sp_zzzz(S0,P0);mpred_post_sp_zzzz(S,P)),!.

mpred_post_sp_zzz(S,P):-  is_main_thread,!,
   (ensure_vars_labled(S:P,S0:P0)-> 
     mpred_post_sp_zzzz(S0,P0);mpred_post_sp_zzzz(S,P)),!.

mpred_post_sp_zzz(S,P):- 
    show_call_when(ensure_vars_labled,S:P,S0:P0),!,
    mpred_post_sp_zzzz(S0,P0),!.

mpred_post_sp_zzz(S,P):-mpred_post_sp_zzzz(S,P),!.

mpred_post_sp_zzzz(S,(P1,P2)) :- !,mpred_post_sp_zzzz(S,(P1)),mpred_post_sp_zzzz(S,(P2)).
mpred_post_sp_zzzz(S,[P1]) :- !,mpred_post_sp_zzzz(S,(P1)).
mpred_post_sp_zzzz(S,[P1|P2]) :- !,mpred_post_sp_zzzz(S,(P1)),mpred_post_sp_zzzz(S,(P2)).
mpred_post_sp_zzzz(S, \+ P) :-!,doall(mpred_rem2a(P,S)),!,mpred_undo((\+),P).
mpred_post_sp_zzzz(S, ~(P)) :-!,mpred_post_sp_zzzz(S, neg( P)).
mpred_post_sp_zzzz(S, not(P)) :-!,mpred_post_sp_zzzz(S, neg( P)).
mpred_post_sp_zzzz(S, neg(P)) :-doall(mpred_rem2a(P,S)),mpred_undo((\+),P),fail.
mpred_post_sp_zzzz(_S,P) :- once((mpred_is_tautology(P),wdmsg(trace_or_throw(todo(error(mpred_is_tautology(P))))))),show_load_context,fail.

% only do loop check if it's already supported

mpred_post_sp_zzzz(S,P) :- is_ftCompound(P), arg(SV,P,V),is_relative(V),must((mpred_update_literal(P,SV,Q,R),mpred_post_sp_zzzz(S,R))),(Q=R->true;mpred_undo(update,Q)).
mpred_post_sp_zzzz(S,P) :- is_already_supported(P,S,_How),must(loop_check(mpred_post1_sp_0(S,P),mpred_post1_sp_1(S,P))),!. % ,mpred_enqueue(P,S).

mpred_post_sp_zzzz(S,neg(P)) :-!, mpred_post1_sp_0(S,neg(P)),mpred_run,!,assert_u(neg(P)).

mpred_post_sp_zzzz(S,P) :- mpred_post1_sp_0(S,P).

mpred_post1_sp_0(S,P) :-
  %= db mpred_add_db_to_head(P,P2),
  % mpred_remove_old_version(P),
  must(once(mpred_add_support(P,S))),
  mpred_post1_sp_1(S,P).

mpred_post1_sp_1(S,P):- P\==true,
  mpred_unique_u(P),
  must(assert_u(P)),!,
  must(mpred_trace_add(P,S)),
  !,
  must(mpred_enqueue(P,S)),
  !.

mpred_post1_sp_1(_,_). % already added
mpred_post1_sp_1(S,P) :-  mpred_warn("mpred_post1(~p,~p) failed",[P,S]).


with_mpred_trace_exec(P):- w_tl(t_l:mpred_debug_local,w_tl(mpred_is_tracing_exec, must(show_if_debug(P)))).
% mpred_test(P):- mpred_is_silient,!,show_if_debug(must(P)),!.
mpred_test(P):- show_call(with_mpred_trace_exec(P)),!.

clause_asserted_local(kbp:spft(ukb,P,Fact,Trigger,UOldWhy)):-
  clause(kbp:spft(ukb,P,Fact,Trigger,_OldWhy),true,Ref),
  clause(kbp:spft(ukb,UP,UFact,UTrigger,UOldWhy),true,Ref),
  (((UP=@=P,UFact=@=Fact,UTrigger=@=Trigger))).


is_already_supported(P,(S,T),(S,T)):- clause_asserted_local(kbp:spft(ukb,P,S,T,_)),!.
is_already_supported(P,_S,UU):- clause_asserted_local(kbp:spft(ukb,P,US,UT,_)),must(get_source_ref(UU)),UU=(US,UT).

% TOO UNSAFE 
% is_already_supported(P,_S):- copy_term(P,PC),sp ftY(PC,_,_),P=@=PC,!.



different_literal(Q,N,R,Test):- 
 is_ftNonvar(Q),acyclic_term(Q),acyclic_term(R),functor(Q,F,A),functor(R,F,A),
  (singleValuedInArg(F,N) -> 
    (arg(N,Q,Was),Test=dif(Was,NEW),replace_arg(Q,N,NEW,R));
    ((arg(N,Q,Was),is_ftNonvar(Q)) -> (Test=dif(Was,NEW),replace_arg(Q,N,NEW,R));
        (N=A,arg(N,Q,Was),Test=dif(Was,NEW),replace_arg(Q,N,NEW,R)))).



% was nothing  mpred_current_db/1.
mpred_current_db(U):-get_source_ref1(U).
mpred_current.


%=
%= mpred_add_db_to_head(+P,-NewP) talkes a fact P or a conditioned fact
%= (P:-C) and adds the Db context.
%=

mpred_add_db_to_head(P,NewP) :-
  mpred_current_db(Db),
  (Db=true        -> NewP = P;
   P=(Head:-Body) -> NewP = (Head :- (Db,Body));
   otherwise      -> NewP = (P :- Db)).


% mpred_unique_u(X) is true if there is no assertion X in the prolog db.

mpred_unique_u((Head:-Tail)) :-
  !,
  \+ clause_u(Head,Tail).
mpred_unique_u(P) :-
  !,
  \+ clause_u(P,true).


mpred_unique_i((Head:-Tail)) :-
  !,
  \+ clause_i(Head,Tail).
mpred_unique_i(P) :-
  !,
  \+ clause_i(P,true).


mpred_enqueue(P,S) :-
  must(mpred_call_shared(kbp:sm(Mode));Mode=direct)
    -> (Mode=direct  -> must(mpred_fwd(P,S)) ;
	Mode=depth   -> mpred_adda_i(kbp:qu(umt,P,S),S) ;
	Mode=breadth -> mpred_addz_i(kbp:qu(umt,P,S),S) ;
	% else
          otherwise           -> mpred_warn("Unrecognized kbp:sm mode: ~p", Mode))
     ; mpred_warn("No kbp:sm mode").


% if there is a rule of the form Identifier ::: Rule then delete it.

mpred_remove_old_version((Identifier::::Body)) :-
  % this should never happen.
  is_ftVar(identifier),
  !,
  mpred_warn("variable used as an  rule name in ~p :::: ~p",
          [Identifier,Body]).


mpred_remove_old_version((Identifier::::Body)) :-
  is_ftNonvar(Identifier),
  clause_u((Identifier::::OldBody),_),
  \+(Body=OldBody),
  mpred_rem1((Identifier::::OldBody)),
  !.
mpred_remove_old_version(_).

% mpred_run compute the deductive closure of the current database.
% How this is done depends on the searching mode:
%    direct -  fc has already done the job.
%    depth or breadth - use the kbp:qu mechanism.

mpred_run_maybe:-!, mpred_run.
mpred_run_maybe:-!, mpred_run.
mpred_run_maybe :- (X is random(5)),X<4,!.
mpred_run_maybe :-
  (\+ kbp:sm(direct)),
  mpred_step,
  mpred_run_maybe.
mpred_run_maybe.

mpred_run:-loop_check(mpred_run0,true).

mpred_run0 :-
  mpred_step,
  mpred_run0.
mpred_run0.


%mpred_run_queued:- repeat,sleep(1.0),mpred_run,fail.
%:-thread_property(_,alias(mpred_running_queue))-> true ; thread_create(mpred_run_queued,_,[alias(mpred_running_queue)]).


% mpred_step removes one entry from the kbp:qu and reasons from it.

mpred_step :-
  % if kbp:hs(Signal) is true, reset it and fail, thereby stopping inferencing.
  mpred_retract_db_type(kbp:hs(Signal)),
  !,
  mpred_warn("Stopping on signal ~p",[Signal]),
  fail.
mpred_step:-loop_check(mpred_step0,true).

mpred_step0 :-
  % draw immediate conclusions from the next fact to be considered.
  % fails iff the queue is empty.
  get_next_fact(P,S),
  pfcl_do(mpred_fwd(P,S)),
  !.

get_next_fact(P,WS) :-
  %identifies the nect fact to fc from and removes it from the queue.
  select_next_fact(P,WS),
  remove_selection(P,WS).

remove_selection(P,S) :-
  mpred_retract_db_type(kbp:qu(umt,P,S)),
  mpred_remove_supports_quietly(kbp:qu(umt,P,S)),
  !.
remove_selection(P,S) :-
  brake(wdmsg("pfc:get_next_fact - selected fact not on Queue: ~p (~p)",
               [P,S])).


% select_next_fact(P) identifies the next fact to reason from.
% It tries the user defined predicate first and, failing that,
%  the mpred_default mechanism.
select_next_fact(P,S) :-
  lmconf:mpred_select(P,S),
  !.
select_next_fact(P,S) :-
  defaultmpred_select(P,S),
  !.

% the mpred_default selection predicate takes the item at the froint of the queue.
defaultmpred_select(P,S) :- kbp:qu(umt,P,S),!.

:- was_shared_multifile(kbp:hs/1).

% mpred_halt stops the forward chaining.
mpred_halt :-  mpred_halt("",[]).

mpred_halt(Format) :- mpred_halt(Format,[]).

mpred_halt(Format,Args) :-
  sformat(S,Format,Args),
  !,
  in_cmt((wdmsg('~s',[S]))),
  (kbp:hs(Signal) ->
       mpred_warn("mpred_halt finds kbp:hs(Signal) already set to ~p",[Signal])
     ; assert_i(kbp:hs(S))).


%=
%=
%= predicates for manipulating triggers
%=
mpred_add_trigger(Trig,Support) :- arg(1,Trig,Term),
   copy_term(Trig,Int), 
   loop_check_term(mpred_add_trigger_0(Int,Trig,Support),trig(Term),true).


mpred_add_trigger_0(_Trig,TriggerBody,Support) :- mpred_had_support(TriggerBody,Support),!,mpred_trace_msg('Had Support',TriggerBody).

mpred_add_trigger_0(Trig,kbp:pt(umt,Trigger,Body),Support) :-
  !,  
  mpred_trace_msg('Adding For Later',kbp:pt(umt,Trigger,Body)),
  (clause_asserted(kbp:pt(umt,Trigger,Body)) -> ! ;   
     (( mpred_add_t(kbp:pt(umt,Trigger,Body),Support),
        (must(mpred_mark_as(Support,p,Trigger,pfcPosTrigger))),
        add_reprop(Trig,Trigger)))).

mpred_add_trigger_0(_Trig,kbp:nt(umt,Trigger,Test,Body),Support) :- !,
  mpred_trace_msg('Adding For Later',kbp:nt(umt,Trigger,Test,Body)),
   must(mpred_mark_as(Support,n,Trigger,pfcNegTrigger)),
        copy_term(Trigger,TriggerCopy),!,
        mpred_add_t(kbp:nt(umt,TriggerCopy,Test,Body),Support),
        ignore((
          (not_cond(kbp:nt,Test)),!, 
           doall(( mpred_eval_lhs(Body,((\+Trigger),kbp:nt(umt,TriggerCopy,Test,Body))))))),!.


mpred_add_trigger_0(_Trig,kbp:bt(umt,Trigger,Body),Support) :- !,
  mpred_trace_msg('Adding For Later',kbp:bt(umt,Trigger,Body)),
   must(mpred_add_t(kbp:bt(umt,Trigger,Body),Support)),
      attvar_op(assertz_if_new,((Trigger:-mpred_bc_only(Trigger)))),!,
      must(mpred_mark_as(Support,b,Trigger,pfcBcTrigger)),
     % WAS mpred_bt_pt_combine(Trigger,Body).
  must(mpred_bt_pt_combine(Trigger,Body,Support)),!.

mpred_add_trigger_0(Trig,X,Support) :- mpred_warn("Unrecognized trigger to mpred_addtrigger: ~p for ~p",[mpred_add_trigger(X,Support),Trig]).


mpred_bt_pt_combine(Head,Body,Support) :-
  %= a backward trigger (kbp:bt) was just added with head and Body and support Support
  %= find any kbp:pt''s with unifying heads and assert the instantied kbp:bt body.
  mpred_get_trigger_quick(kbp:pt(umt,Head,_PtBody)),
  mpred_eval_lhs(Body,Support),
  fail.
mpred_bt_pt_combine(_,_,_) :- !.


mpred_get_trigger_quick(Trigger) :- !, mpred_call_shared(Trigger).
mpred_get_trigger_quick(Trigger) :- clause_i(Trigger,true)*->true;clause(kbp:spft(ukb,Trigger,_,_,_),true).

%=
%=
%= predicates for manipulating action traces.
%=

mpred_add_actiontrace(Action,Support) :-
  % adds an action trace and it''s support.
  mpred_add_support(mpred_action(Action),Support).

mpred_rem_actiontrace(_,mpred_action(A)) :-
  mpred_do_and_undo_method(A,M),
  M,
  !.


%=
%= predicates to remove pfc facts, triggers, action traces, and queue items
%= from the database.
%=
%= was simply:  mpred_retract
mpred_retract_db_type(X) :-
  %= retract an arbitrary thing.
  mpred_db_type(X,Type),
  mpred_retract_db_type(Type,X),
  !.


mpred_retract_db_type(_,kbp:qu(umt,P,S)) :-
  doall(retract_u(kbp:qu(umt,P,_))),
  ignore(mpred_unfwc(kbp:qu(umt,P,S))).


mpred_retract_db_type(fact,X) :-
  %= db mpred_add_db_to_head(X,X2), retract(X2).
  retract_u(X),
  ignore(mpred_unfwc(X)).

mpred_retract_db_type(rule,X) :-
  %= db  mpred_add_db_to_head(X,X2),  retract(X2).
  retract_u(X).

mpred_retract_db_type(trigger,X) :-
  retract_t(X)
    -> mpred_unfwc(X)
     ; mpred_warn("Trigger not found to mpred_retract_db_type: ~p",[X]).

mpred_retract_db_type(action,X) :- mpred_rem_actiontrace(mpred_retract_db_type,X).


/* UNUSED TODAY
%= mpred_add_db_type(X) adds item X to some database
%= was simply:  mpred_Add
mpred_add_db_type(X) :-
  % what type of X do we have?
  mpred_db_type(X,Type),
  % call the appropriate predicate.
  mpred_add_db_type(Type,X).

mpred_add_db_type(fact,X) :-
  mpred_unique_u(X),
  must(assert_u(X)),!.
mpred_add_db_type(rule,X) :-
  mpred_unique_i(X),
  assert_i(X),!.
mpred_add_db_type(trigger,X) :-
  assert_t(X).
mpred_add_db_type(action,_Action) :- !.
*/

%= mpred_rem1(P,S) removes support S from P and checks to see if P is still supported.
%= If it is not, then the fact is retracted from the database and any support
%= relationships it participated in removed.

mpred_rem1(List) :-
  % iterate down the list of facts to be mpred_rem1'ed.
  is_ftNonvar(List),
  List=[_|_],
  rem_list(List).

mpred_rem1(P) :-
  % mpred_rem1/1 is the user''s interface - it withdraws user support for P.
  make_uu_remove(UU),
  mpred_rem1(P,UU).

rem_list([H|T]) :-
  % mpred_rem1 each element in the list.
  make_uu_remove(UU),
  mpred_rem1(H,UU),
  rem_list(T).

mpred_rem1(P,S) :- copy_term(mpred_rem1(P,S),Why),
  mpred_trace_msg('Removing support',mpred_rem1(P,S)),
  mpred_rem_support(Why,P,S)
     -> (remove_if_unsupported(Why,P))
      ; mpred_warn("mpred_rem1/2 Could not find support ~p to remove from fact ~p",
                [S,P]).

%=
%= mpred_rem2 is like mpred_rem1, but if P is still in the DB after removing the
%= user''s support, it is retracted by more forceful means (e.g. remove).
%=

mpred_rem2a(P) :- mpred_run,
  % mpred_rem2/1 is the user''s interface - it withdraws user support for P.
  make_uu_remove(UU),
  mpred_rem2a(P,UU).

mpred_rem2a(P,S) :-
  mpred_rem1(P,S),
  % used to say mpred_call_only_facts(Why,P) but that meant it was 
  % was no_repeats(( mpred_call_with_triggers(P);mpred_call_with_no_triggers(Why,P)))
  (( mpred_call_only_facts(mpred_rem2a,P) )  
     -> (mpred_remove3(P))
      ; true).

% prev way
% mpred_rem2(P):-!,mpred_rem2a(P).
% new way
mpred_rem2(P):-mpred_rem2a(P),mpred_unfwc(P).

% help us choose
mpred_rem(P) :-mpred_rem2a(P),mpred_unfwc(P).


%=
%= mpred_remove3(+F) retracts fact F from the DB and removes any dependent facts */
%=

mpred_remove3(F) :-
  show_if_debug(mpred_remove_supports_f_l(mpred_remove3(F),F)),
  mpred_undo(mpred_remove3(F),F).


% removes any remaining supports for fact F, complaining as it goes.

mpred_remove_supports_f_l(Why,F) :-
  mpred_rem_support(Why,F,S),
  (S=(z,z)->true;mpred_trace_msg("~p was supported by ~p",[F,S])),
  fail.
mpred_remove_supports_f_l(Why,F) :- fail,
  mpred_rem_support(Why,F,S),nonvar(S),
  (S=(z,z)->true;mpred_warn("WARN: ~p was still supported by ~p",[F,S])),
  fail.
mpred_remove_supports_f_l(_,_).

mpred_remove_supports_quietly(F) :-
  mpred_rem_support(mpred_remove_supports_quietly,F,_),
  fail.
mpred_remove_supports_quietly(_).

% mpred_undo(Why,X) undoes X.


mpred_undo(Why,mpred_action(A)) :-
  % undo an action by finding a method and successfully executing it.
  !,
  mpred_rem_actiontrace(Why,mpred_action(A)).

mpred_undo(Why,kbp:pk(umt,Key,Head,Body)) :-
  % undo a positive trigger.
  %
  !,
  (retract_i(kbp:pk(umt,Key,Head,Body))
    -> mpred_unfwc(kbp:pt(umt,Head,Body))
     ; mpred_warn("for ~p \nTrigger not found to retract kbp:pk=~p: ~p",[Why,Key,kbp:pt(umt,Head,Body)])).

mpred_undo(Why,kbp:pt(umt,Head,Body)) :- 
  % undo a positive trigger.
  %
  !,
  (retract_i(kbp:pt(umt,Head,Body))
    -> mpred_unfwc(kbp:pt(umt,Head,Body))
     ; mpred_warn("for ~p:\nTrigger not found to retract: ~p",[Why,kbp:pt(umt,Head,Body)])).


mpred_undo(Why,kbp:bt(umt,Head,Body)) :- 
  % undo a backchaining trigger.
  %
  !,
  dtrace(attvar_op(retractall,(Head:-mpred_bc_only(Head)))),
  (retract_i(kbp:bt(umt,Head,Body))
    -> mpred_unfwc(kbp:bt(umt,Head,Body))
     ; mpred_warn("for ~p:\nTrigger not found to retract: ~p",[Why,kbp:bt(umt,Head,Body)])).


mpred_undo(Why,kbp:nt(umt,Head,Condition,Body)) :-
  % undo a negative trigger.
  !,
  (retract_i(kbp:nt(umt,Head,Condition,Body))
    -> mpred_unfwc(kbp:nt(umt,Head,Condition,Body))
     ; mpred_trace_msg("for ~p:\nTrigger not found to retract: ~p",[Why,kbp:nt(umt,Head,Condition,Body)])).

mpred_undo(Why,Fact):- mpred_undo_u(Why,Fact)*->true;mpred_undo_e(Why,Fact).

mpred_undo_u(Why,Fact) :-
  % undo a random fact, printing out the trace, if relevant.
  retract_u(Fact),
     must(mpred_trace_rem(Why,Fact)),
     mpred_unfwc1(Fact).

mpred_undo_e(Why,Fact) :- 
     (Fact\=neg(_)->cnotrace(mpred_trace_msg("mpred_undo_e ; Fact not found in user db: ~p",[Fact]));true),
     (Fact\=neg(_)->mpred_trace_rem(Why,Fact);true),
     mpred_unfwc(Fact).


%= mpred_unfwc(P) "un-forward-chains" from fact f.  That is, fact F has just
%= been removed from the database, so remove all support relations it
%= participates in and check the things that they support to see if they
%= should stay in the database or should also be removed.

mpred_unfwc(F) :-
  mpred_retract_support_relations(mpred_unfwc(F),F),
  mpred_unfwc1(F).

mpred_unfwc1(F) :-
  mpred_unfwc_check_triggers(_Sup,F),
  % is this really the right place for mpred_run<?
  mpred_run_maybe.

mpred_unfwc_check_triggers(_Sup,F) :-
  mpred_db_type(F,fact),
  copy_term(F,Fcopy),
  mpred_get_trigger_quick(kbp:nt(umt,Fcopy,Condition,Action)),
  (not_cond(kbp:nt,Condition)),
  G = mpred_eval_lhs(Action,((\+F),kbp:nt(umt,F,Condition,Action))),
  loop_check(G,mpred_trace_msg(unfwc_caught_loop(G))),
  fail.
mpred_unfwc_check_triggers(_Sup,_).

mpred_retract_support_relations(Why,Fact) :-
  mpred_db_type(Fact,Type),
  (Type=trigger -> mpred_rem_support(Why,P,(_,Fact)) ;
    % non trigger
    mpred_rem_support(Why,P,(Fact,_))),
  remove_if_unsupported(Why,P),
  fail.
mpred_retract_support_relations(_,_).

%= remove_if_unsupported(Why,+P) checks to see if P is supported and removes
%= it from the DB if it is not.


remove_if_unsupported_verbose(Why,TMS,P) :- is_ftVar(P),!,trace_or_throw(warn(var_remove_if_unsupported_verbose(Why,TMS,P))).
remove_if_unsupported_verbose(Why,TMS,P) :- (\+ ground(P) -> mpred_trace_msg(warn(ng_remove_if_unsupported_verbose(Why,TMS,P))) ;true),
   (((mpred_tms_supported(TMS,P,How),How\=unknown(_)) -> mpred_trace_msg(v_still_supported(How,Why,TMS,P)) ; (  mpred_undo(Why,P)))),
   mpred_run.


remove_if_unsupported(Why,P) :- is_ftVar(P),!,trace_or_throw(warn(var_remove_if_unsupported(Why,P))).
remove_if_unsupported(Why,P) :- ((\+ ground(P), P \= (_:-_) , P \= neg(_) ) -> mpred_trace_msg(warn(nonground_remove_if_unsupported(Why,P))) ;true),
   (((mpred_tms_supported(local,P,How),How\=unknown(_)) -> mpred_trace_msg(still_supported(How,Why,local,P)) ; (  mpred_undo(Why,P)))),
   mpred_run.


%= mpred_tms_supported(+P,-How) succeeds if P is "supported". What "How" means
%= depends on the TMS mode selected.

mpred_tms_supported(P,How) :-
  kbp:tms(Mode),
  mpred_tms_supported0(Mode,P,How).


mpred_tms_supported(Mode,P,How) :- is_ftVar(Mode),kbp:tms(Mode),!,mpred_tms_supported0(Mode,P,How).
mpred_tms_supported(Mode,P,How) :- mpred_tms_supported0(Mode,P,How).
mpred_tms_supported(How,_P,unknown(How)).

mpred_tms_supported0(local,P,How) :-  mpred_get_support(P,How). % ,sanity(mpred_deep_support(How,S)).
mpred_tms_supported0(cycles,P,How) :-  wellFounded(P,How).
mpred_tms_supported0(deep,P,How) :- mpred_deep_support(How,P).

% lmconf:hook_one_minute_timer_tick:- statistics.


mpred_scan_tms(P):-mpred_get_support(P,(S,SS)),
  (S==SS-> true;
   once((mpred_deep_support(_How,P)->true;
     (mpred_trace_msg(warn(now_maybe_unsupported(mpred_get_support(P,(S,SS)),fail))))))).

user_atom(U):-match_source_ref1(U).
user_atom(g).
user_atom(m).
user_atom(d).

mpred_deep_support(_How,unbound):-!,fail.
mpred_deep_support(How,M):-loop_check(mpred_deep_support0(How,M),fail).

mpred_deep_support0(user_atom(U),(U,U)):-user_atom(U),!.
mpred_deep_support0(How,(A==>_)):-!,mpred_deep_support(How,A).
mpred_deep_support0(kbp:pt(umt,HowA,HowB),kbp:pt(umt,A,B)):-!,mpred_deep_support(HowA,A),mpred_deep_support(HowB,B).
mpred_deep_support0(HowA->HowB,(A->B)):-!,mpred_deep_support(HowA,A),mpred_deep_support(HowB,B).
mpred_deep_support0(HowA/HowB,(A/B)):-!,mpred_deep_support(HowA,A),mpred_deep_support(HowB,B).
mpred_deep_support0((HowA,HowB),(A,B)):-!,mpred_deep_support(HowA,A),mpred_deep_support(HowB,B).
mpred_deep_support0(How,rhs(P)):-!,maplist(mpred_deep_support,How,P).
mpred_deep_support0(mpred_call_only_facts(\+ P),\+ call_u(P)):-!,mpred_call_only_facts(\+ P).
mpred_deep_support0(mpred_call_only_facts(P),call_u(P)):-!,mpred_call_only_facts(P).
mpred_deep_support0(mpred_call_only_facts(P),{P}):-!,mpred_call_only_facts(P).
mpred_deep_support0(S==>How,P):-mpred_get_support(P,S),mpred_deep_support(How,S),!.
mpred_deep_support0(mpred_call_only_facts(\+(P)),\+(P)):-!, mpred_call_only_facts(\+(P)).
mpred_deep_support0(user_atom(P),P):-user_atom(P),!.
mpred_deep_support0(mpred_call_only_facts((P)),P):-mpred_call_only_facts(P).


%=
%= a fact is well founded if it is supported by the user
%= or by a set of facts and a rules, all of which are well founded.
%=

wellFounded(Fact,How) :- mpred_wff(Fact,[],How).

mpred_wff(F,_,How) :-
  % supported by user (mpred_axiom) or an "absent" fact (assumption).
  ((mpred_axiom(F),How =mpred_axiom(F) ); (assumption(F),How=assumption(F))),
  !.

mpred_wff(F,Descendants,wff(Supporters)) :-
  % first make sure we aren't in a loop.
  (\+ memberchk(F,Descendants)),
  % find a justification.
  supports_f_l(F,Supporters),
  % all of whose members are well founded.
  mpred_wfflist(Supporters,[F|Descendants]),
  !.

%= mpred_wfflist(L) simply maps mpred_wff over the list.

mpred_wfflist([],_).
mpred_wfflist([X|Rest],L) :-
  mpred_wff(X,L,_How),
  mpred_wfflist(Rest,L).

% supports_f_l(+F,-ListofSupporters) where ListOfSupports is a list of the
% supports for one justification for fact F -- i.e. a list of facts which,
% together allow one to deduce F.  One of the facts will typically be a rule.
% The supports for a user-defined fact are: [u].

supports_f_l(F,[Fact|MoreFacts]) :-
  mpred_get_support_precanonical_plus_more(F,(Fact,Trigger)),
  trigger_supports_f_l(Trigger,MoreFacts).

mpred_get_support_precanonical_plus_more(P,Sup):-mpred_get_support_one(P,Sup)*->true;((to_addable_form_wte(mpred_get_support_precanonical_plus_more,P,PE),P\=@=PE,mpred_get_support_one(PE,Sup))).
mpred_get_support_one(P,Sup):- mpred_get_support(P,Sup)*->true;
  (mpred_get_support_via_clause_db(P,Sup)*->true;
     mpred_get_support_via_sentence(P,Sup)).

mpred_get_support_via_sentence(Var,_):-is_ftVar(Var),!,fail.
mpred_get_support_via_sentence((A,B),(FC,TC)):-!, mpred_get_support_precanonical_plus_more(A,(FA,TA)),mpred_get_support_precanonical_plus_more(B,(FB,TB)),conjoin(FA,FB,FC),conjoin(TA,TB,TC).
mpred_get_support_via_sentence(true,g):-!.

mpred_get_support_via_clause_db(\+ P,OUT):- mpred_get_support_via_clause_db(neg(P),OUT).
mpred_get_support_via_clause_db(\+ P,(naf(g),g)):- !, predicate_property(P,number_of_clauses(_)),\+ clause(P,_Body).
mpred_get_support_via_clause_db(P,OUT):- predicate_property(P,number_of_clauses(N)),N>0,
   clause(P,Body),(Body==true->Sup=(g);
    (support_ok_via_clause_body(P),mpred_get_support_precanonical_plus_more(Body,Sup))),
   OUT=(Sup,g).

support_ok_via_clause_body(_H):-!,fail.
support_ok_via_clause_body(H):- get_functor(H,F,A),support_ok_via_clause_body(H,F,A).
support_ok_via_clause_body(_,(\+),1):-!,fail.
support_ok_via_clause_body(_,F,_):- mpred_call_shared(prologSideEffects(F)),!,fail.
support_ok_via_clause_body(H,_,_):- \+ predicate_property(H,number_of_clauses(_)),!,fail.
support_ok_via_clause_body(_,F,A):- mpred_call_shared(pfcMark(pfcRHS,_,F,A)),!,fail.
support_ok_via_clause_body(_,F,A):- mpred_call_shared(pfcMark(pfcMustFC,_,F,A)),!,fail.
support_ok_via_clause_body(_,F,_):- mpred_call_shared(argsQuoted(F)),!,fail.
support_ok_via_clause_body(_,F,_):- mpred_call_shared(prologDynamic(F)),!.
support_ok_via_clause_body(_,F,_):- \+ mpred_call_shared(pfcControlled(F)),!.


mpred_get_support_precanonical(F,Sup):-to_addable_form_wte(mpred_get_support_precanonical,F,P),mpred_get_support(P,Sup).
spft_precanonical(F,SF,ST):-to_addable_form_wte(spft_precanonical,F,P),!,kbp:spft(ukb,P,SF,ST,_).

trigger_supports_f_l(U,[]) :- match_source_ref1(U),!.
trigger_supports_f_l(Trigger,[Fact|MoreFacts]) :-
  mpred_get_support_precanonical_plus_more(Trigger,(Fact,AnotherTrigger)),
  trigger_supports_f_l(AnotherTrigger,MoreFacts).


%=
%=
%= mpred_fwd(X) forward chains from a fact or a list of facts X.
%=

mpred_fwd(P):- get_source_ref(UU), mpred_fwd(P,UU).
mpred_fwd([H|T],S) :- !, mpred_fwd1(H,S), mpred_fwd(T,S).
mpred_fwd([],_) :- !.
mpred_fwd(P,S) :- mpred_fwd1(P,S),!.

% mpred_fwd1(+P) forward chains for a single fact.



mpred_fwd1(Fact,Sup) :- gripe_time(0.80,mpred_fwd2(Fact,Sup)),!.

mpred_fwd2(Fact,Sup) :- cyclic_term(Fact;Sup),writeq(mpred_fwd2_cyclic_term(Fact;Sup)),!.
mpred_fwd2(Fact,_Sup) :-
  once(must(mpred_add_rule_if_rule(Fact))),
  copy_term(Fact,F),
  % check positive triggers
  once(must(fcpt(Fact,F))),
  % check negative triggers
  once(must(fcnt(Fact,F))).


%=
%= mpred_add_rule_if_rule(P) does some special, built in forward chaining if P is
%= a rule.
%=

% mpred_add_rule_if_rule(Fact) :- cyclic_break(Fact),is_mpred_action(Fact),(ground(Fact)->must(once(Fact));doall(show_if_debug(must(Fact)))),fail.
mpred_add_rule_if_rule(Fact) :- cyclic_break(Fact),is_mpred_action(Fact),(ground(Fact)->must(once(Fact));doall(show_if_debug(must(Fact)))),!.
mpred_add_rule_if_rule(Fact):- must(mpred_add_rule0(Fact)),!.

mpred_add_rule0((P==>Q)) :-
  !,
  process_rule(P,Q,(P==>Q)).

mpred_add_rule0((Name::::P==>Q)) :-
  !,
  process_rule(P,Q,(Name::::P==>Q)).

mpred_add_rule0((P<==>Q)) :-
  !,
  process_rule(P,Q,(P<==>Q)),
  process_rule(Q,P,(P<==>Q)).

mpred_add_rule0((Name::::P<==>Q)) :-
  !,
  process_rule(P,Q,((Name::::P<==>Q))),
  process_rule(Q,P,((Name::::P<==>Q))).

mpred_add_rule0(('<-'(P,Q))) :-
  !,
  mpred_define_bc_rule(P,Q,('<-'(P,Q))).

mpred_add_rule0(_).

fcpt(Fact,F):- fcpt0(Fact,F)*->fail;nop(mpred_trace_msg(no_pt(Fact,F))).
fcpt(_,_).

fcpt0(Fact,F) :- 
  mpred_get_trigger_quick(kbp:pt(umt,F,Body)),
  (mpred_eval_lhs(Body,(Fact,kbp:pt(umt,F,Body)))
    *-> mpred_trace_msg('Using Trigger',kbp:pt(umt,F,Body));
      (mpred_trace_msg('Skipped Trigger',kbp:pt(umt,F,Body)),fail)).
  

fcpt0(Fact,F) :- use_presently,
  mpred_get_trigger_quick(kbp:pt(umt,presently(F),Body)),
  pp_item('Found presently ',kbp:pt(umt,F,Body)),
  mpred_eval_lhs(Body,(presently(Fact),kbp:pt(umt,presently(F),Body))).

fcnt(Fact,F):- fcnt0(Fact,F)*->fail;nop(mpred_trace_msg(no_spft_nt(Fact,F))).
fcnt(_,_).

fcnt0(_Fact,F) :- 
  kbp:spft(ukb,X,_,kbp:nt(umt,F,Condition,Body),_Why),
  (call_u(Condition) *-> 
   (mpred_trace_msg('Using Trigger'(X),kbp:nt(umt,F,Condition,Body)),
      mpred_rem1(X,(_,kbp:nt(umt,F,Condition,Body))),fail);
      (mpred_trace_msg('Unused Trigger'(X),kbp:nt(umt,F,Condition,Body)),fail)).



%=
%= mpred_define_bc_rule(+Head,+Body,+Parent_rule) - defines a backward
%= chaining rule and adds the corresponding kbp:bt triggers to the database.
%=

mpred_define_bc_rule(Head,Body,Parent_rule) :-
  (\+ mpred_literal(Head)),
  mpred_warn("Malformed backward chaining rule.  ~p not atomic.",[(Head:-Body)]),
  mpred_warn("rule: ~p",[Parent_rule]),
 % !,
  dtrace(mpred_define_bc_rule(Head,Body,Parent_rule)),
  fail.

mpred_define_bc_rule(Head,Body,Parent_rule) :- 
  copy_term(Parent_rule,Parent_ruleCopy),
  attvar_op(assert_if_new,(Head:-mpred_bc_only(Head))),
  build_rhs(Head,Head,Rhs),
  foreachl_do(mpred_nf(Body,Lhs),
       (build_trigger(Parent_ruleCopy,Lhs,rhs(Rhs),Trigger),
       % can be mpred_post_sp(kbp:bt(umt,Head,Trigger),(Parent_ruleCopy,U))
         ((get_source_ref1(U),mpred_add_fast(kbp:bt(umt,Head,Trigger),(Parent_ruleCopy,U)))))).
        

%=
%=
%= eval something on the LHS of a rule.
%=

mpred_eval_lhs(P,S):-no_repeats(loop_check(mpred_eval_lhs0(P,S))).

mpred_eval_lhs0((Test->Body),Support) :-
  !,
  % (call_prologsys(Test) -> mpred_eval_lhs(Body,Support)),
   ((no_repeats(call_prologsys(Test)) , no_repeats(mpred_eval_lhs(Body,Support))) *-> true ; (!,fail)).

mpred_eval_lhs0(rhs(X),Support) :-
   cyclic_break(X),
  !,
  on_x_rtrace(mpred_eval_rhs_0(X,Support)),
  !.

mpred_eval_lhs0(X,Support) :-
  cyclic_break((X)),
  is_ftCompound(X),
  on_x_rtrace(mpred_db_type(X,trigger)),
  !,
  on_x_rtrace(mpred_add_trigger(X,Support)),
  !.

%mpred_eval_lhs0(snip(X),Support) :-
%  snip(Support),
%  mpred_eval_lhs(X,Support).

mpred_eval_lhs0(X,Why) :-
  mpred_warn("Unrecognized item found in trigger body, namely ~p.",[mpred_eval_lhs0(X,Why)]),!.


%=
%= eval something on the RHS of a rule.
%=

mpred_eval_rhs_0([],_) :- !.
mpred_eval_rhs_0([Head|Tail],Support) :-
  mpred_eval_rhs1(Head,Support),
  mpred_eval_rhs_0(Tail,Support).


mpred_eval_rhs1({Action},Support) :-
 % evaluable Prolog code.
 !,
 fc_eval_action(Action,Support),!.

mpred_eval_rhs1(mpred_action(Action),Support) :-
 % evaluable Prolog code.
 !,
 fc_eval_action(Action,Support),!.

mpred_eval_rhs1(P,_Support) :-
 % predicate to remove.
 mpred_negation(P,N),
 !,
 doall(mpred_rem1(N)),!.

mpred_eval_rhs1([X|Xrest],Support) :-
 % embedded sublist.
 !,
 mpred_eval_rhs_0([X|Xrest],Support),!.

mpred_eval_rhs1(added(Assertion),Support) :-
 % an assertion to be added.
 mpred_add(Assertion,Support),!.

mpred_eval_rhs1(Assertion,Support) :-
 % an assertion to be added.
 mpred_post1(Assertion,Support),!.

mpred_eval_rhs1(X,_) :-
  mpred_warn("Malformed rhs of a rule: ~p",[X]).


%=
%= evaluate an action found on the rhs of a rule.
%=

fc_eval_action(Action,Support) :-
  call_prologsys(Action),
  (undoable(Action)
     -> mpred_add_actiontrace(Action,Support)
      ; true).


%=
%=
%=
/*
trigger_trigger(Trigger,Body,_Support) :-
 trigger_trigger1(Trigger,Body).
trigger_trigger(_,_,_).


trigger_trigger1(presently(Trigger),Body) :- use_presently,
  is_ftNonvar(Trigger),!,
  copy_term(Trigger,TriggerCopy),
  no_repeats(call_u(Trigger)),
  mpred_eval_lhs(Body,(presently(Trigger),kbp:pt(umt,presently(TriggerCopy),Body))),
  fail.

trigger_trigger1(Trigger,Body) :-
  copy_term(Trigger,TriggerCopy),
  no_repeats(mpred_call_only_facts(Trigger)),
  mpred_eval_lhs(Body,(Trigger,kbp:pt(umt,TriggerCopy,Body))),
  fail.
*/

%=
%= call_u(Why,F) is true iff F is a fact is true
%=
call_u(X):- mpred_call_only_facts(X).
call_u(Why,X):- show_call((nop(Why),mpred_call_only_facts(X))).

%=
%= not_cond(Why,F) is true iff F is a fact is not true
%=
% not_cond(_Why,X):- show_call_success(mpred_call_0(neg(X))).
not_cond(_Why,X):- \+ X.


%=
%= mpred_call_only_facts(+Why,:F) is true iff F is a fact available for forward chaining.
%= Note that this has the side effect [maybe] of catching unsupported facts and
%= assigning them support from God. (g,g)
%=
mpred_call(G):-  on_x_rtrace(no_repeats(loop_check(mpred_call_0(G),fail))).
mpred_call_only_facts(F):- on_x_rtrace(no_repeats(loop_check(mpred_call_0(F),fail))). 
mpred_call_only_facts(_Why,F):- on_x_rtrace(no_repeats(loop_check(mpred_call_0(F),fail))). 



mpred_call_0(Var):-is_ftVar(Var),!,mpred_call_with_no_triggers(Var).
mpred_call_0(U:X):-U==user,!,mpred_call_0(X).
mpred_call_0(t(A,B)):-(atom(A)->true;no_repeats(arity(A,1))),ABC=..[A,B],mpred_call_0(ABC).
mpred_call_0(isa(B,A)):-(atom(A)->true;no_repeats(tCol(A))),ABC=..[A,B],mpred_call_0(ABC).
%mpred_call_0(t(A,B)):-!,(atom(A)->true;no_repeats(arity(A,1))),ABC=..[A,B],mpred_call_0(ABC).
mpred_call_0(t(A,B,C)):-!,(atom(A)->true;no_repeats(arity(A,2))),ABC=..[A,B,C],mpred_call_0(ABC).
mpred_call_0(t(A,B,C,D)):-!,(atom(A)->true;no_repeats(arity(A,3))),ABC=..[A,B,C,D],mpred_call_0(ABC).
mpred_call_0(t(A,B,C,D,E)):-!,(atom(A)->true;no_repeats(arity(A,4))),ABC=..[A,B,C,D,E],mpred_call_0(ABC).
mpred_call_0((C1,C2)):-!,mpred_call_0(C1),mpred_call_0(C2).
mpred_call_0(call(X)):- !, mpred_call_0(X).
mpred_call_0(\+(X)):- !, \+ mpred_call_0(X).
mpred_call_0(call_u(X)):- !, mpred_call_0(X).
mpred_call_0(G):-functor(G,F,A),mpred_call_0(G,F,A).


mpred_call_0(G,_,_):- is_side_effect_disabled,!,mpred_call_with_no_triggers(G).

mpred_call_0(G,F,A):-  (ground(G); \+ current_predicate(F/A) ; \+ (predicate_property(G,clause_count(CC)),CC>1)), (\+  is_side_effect_disabled),
                ignore((loop_check(call_with_bc_triggers(G)),maybeSupport(G,(g,g)),fail)),
                (\+ current_predicate(F/A),dynamic(F/A),multifile(F/A),!,fail).
mpred_call_0(G,_,_):- mpred_call_with_no_triggers(G).


:- thread_local t_l:infBackChainPrevented/1.

call_with_bc_triggers(P) :- functor(P,F,A), \+t_l:infBackChainPrevented(F/A), 
  mpred_get_trigger_quick(kbp:bt(umt,P,Trigger)),
  no_repeats(mpred_get_support(kbp:bt(umt,P,Trigger),S)),
  once(no_side_effects(P)),
  wno_tl(t_l:infBackChainPrevented(F/A),mpred_eval_lhs(Trigger,S)).


mpred_call_with_no_triggers(F) :- 
  %= this (is_ftVar(F)) is probably not advisable due to extreme inefficiency.
  (is_ftVar(F)    ->  mpred_facts_and_universe(F) ; mpred_call_with_no_triggers_bound(F)).

mpred_call_with_no_triggers_bound(F) :- 
  show_call_failure(no_side_effects(F)),
  (\+ current_predicate(_,F) -> fail;call_prologsys(F)).
  %= we check for system predicates as well.
  %has_cl(F) -> (clause_u(F,Condition),(Condition==true->true;call_u(Condition)));
  %call_prologsys(F).


mpred_bc_only(G):- mpred_negation(G,Pos),!, show_call(\+ mpred_bc_only(Pos)).
mpred_bc_only(G):- !,mpred_call_only_facts(G).
%mpred_bc_only(G):- loop_check(no_repeats(pfcBC_NoFacts(G))).

%%
%= pfcBC_NoFacts(F) is true iff F is a fact available for backward chaining ONLY.
%= Note that this has the side effect of catching unsupported facts and
%= assigning them support from God.
%= this Predicate should hide Facts from mpred_bc_only/1
%%
pfcBC_NoFacts(F):- pfcBC_NoFacts_TRY(F)*-> true ; (mpred_slow_search,pfcBC_Cache(F)).

mpred_slow_search.


ruleBackward(F,Condition):-ruleBackward0(F,Condition),functor(Condition,F,_),\+ arg(_,F,v(call_prologsys,call_u)).
%ruleBackward0(F,Condition):-clause_u(F,Condition),\+ (is_true(Condition);mpred_is_info(Condition)).
ruleBackward0(F,Condition):-'<-'(F,Condition),\+ (is_true(Condition);mpred_is_info(Condition)).

%:- was_dynamic('{}'/1).
%{X}:-dmsg(legacy({X})),call_prologsys(X).


pfcBC_NoFacts_TRY(F) :- no_repeats(ruleBackward(F,Condition)),
  % neck(F),
  call_prologsys(Condition),
  maybeSupport(F,(g,g)).


pfcBC_Cache(F) :- mpred_call_only_facts(pfcBC_Cache,F),
   ignore((ground(F),( (\+is_asserted_1(F)), maybeSupport(F,(g,g))))).


maybeSupport(P,_):-mpred_ignored(P),!.
maybeSupport(P,S):-( \+ ground(P)-> true;
  (predicate_property(P,dynamic)->mpred_add(P,S);true)).

mpred_ignored(argIsa(F, A, argIsaFn(F, A))).
mpred_ignored(genls(A,A)).
mpred_ignored(isa(tCol,tCol)).
%mpred_ignored(isa(W,tCol)):-mpred_call_shared(lmconf:hasInstance_dyn(tCol,W)).
mpred_ignored(isa(W,_)):-is_ftCompound(W),isa(W,pred_argtypes).
mpred_ignored(C):-clause_safe(C,true). 
mpred_ignored(isa(_,Atom)):-atom(Atom),atom_concat(ft,_,Atom),!.
mpred_ignored(isa(_,argIsaFn(_, _))).


has_cl(H):-predicate_property(H,number_of_clauses(_)).

% an action is undoable if there exists a method for undoing it.
undoable(A) :- mpred_do_and_undo_method(A,_).

%=
%=
%= defining fc rules
%=

%= mpred_nf(+In,-Out) maps the LHR of a pfc rule In to one normal form
%= Out.  It also does certain optimizations.  Backtracking into this
%= predicate will produce additional clauses.


mpred_nf(LHS,List) :-
  must(mpred_nf1(LHS,List2)),
  mpred_nf_negations(List2,List).


%= mpred_nf1(+In,-Out) maps the LHR of a pfc rule In to one normal form
%= Out.  Backtracking into this predicate will produce additional clauses.

% handle a variable.

mpred_nf1(P,[P]) :- is_ftVar(P), !.

% these next three rules are here for upward compatibility and will go
% away eventually when the P/Condition form is no longer used anywhere.

mpred_nf1(P/Cond,[(\+Q)/Cond]) :- fail, mpred_negated_literal(P,Q), !.  % DMILES does not undersand why this is wron gand the next is correct here
mpred_nf1(P/Cond,[(\+P)/Cond]) :- mpred_negated_literal(P,_), !.

mpred_nf1(P/Cond,[P/Cond]) :-  mpred_literal(P), !.

mpred_nf1((P/Cond),O) :-!,mpred_nf1((P,{Cond}),O).

mpred_nf1({P},[{P}]) :-!.

%= handle a negated form

mpred_nf1(NegTerm,NF) :-
  mpred_negation(NegTerm,Term),
  !,
  mpred_nf1_negation(Term,NF).

mpred_nf1(~((P,Q)),NF) :-
 mpred_nf1(~P,NP),
 mpred_nf1(~Q,NQ),
 !,
 mpred_nf1(((NP/Q);(NQ/P)),NF).

%= disjunction.

mpred_nf1((P;Q),NF) :-
  !,
  (mpred_nf1(P,NF) ;   mpred_nf1(Q,NF)).


%= conjunction.

mpred_nf1((P,Q),NF) :-
  !,
  mpred_nf1(P,NF1),
  mpred_nf1(Q,NF2),
  append(NF1,NF2,NF).

%= handle a random atom.

mpred_nf1(P,[P]) :-
  mpred_literal(P),
  !.

%=% shouln't we have something to catch the rest as errors?
mpred_nf1(Term,[Term]) :-
  mpred_warn("mpred_nf doesn't know how to normalize ~p",[Term]),!,fail.

mpred_negation_w_neg(neg(P),P):-is_ftNonvar(P),!.
mpred_negation_w_neg(P,NF):-mpred_nf1_negation(P,NF).

%= mpred_nf1_negation(P,NF) is true if NF is the normal form of \+P.
mpred_nf1_negation((P/Cond),[(\+(P))/Cond]) :- !.

mpred_nf1_negation((P;Q),NF) :-
  !,
  mpred_nf1_negation(P,NFp),
  mpred_nf1_negation(Q,NFq),
  append(NFp,NFq,NF).

mpred_nf1_negation((P,Q),NF) :-
  % this code is not correct! tmpred_wff.
  !,
  mpred_nf1_negation(P,NF)
  ;
  (mpred_nf1(P,Pnf),
   mpred_nf1_negation(Q,Qnf),
   append(Pnf,Qnf,NF)).

mpred_nf1_negation(P,[\+P]).


%= mpred_nf_negations(List2,List) sweeps through List2 to produce List,
%= changing ~{...} to {\+...}
%=% ? is this still needed? tmpred_wff 3/16/90

mpred_nf_negations(X,X) :- !.  % I think not! tmpred_wff 3/27/90

mpred_nf_negations([],[]).

mpred_nf_negations([H1|T1],[H2|T2]) :-
  mpred_nf_negation(H1,H2),
  mpred_nf_negations(T1,T2).

mpred_nf_negation(Form,{\+ X}) :-
  is_ftNonvar(Form),
  Form=(~({X})),
  !.
mpred_nf_negation(Form,{\+ X}) :-
  is_ftNonvar(Form),
  Form=(neg({X})),
  !.
mpred_nf_negation(X,X).


%=
%= build_rhs(Sup,+Conjunction,-Rhs)
%=

build_rhs(_Sup,X,[X]) :-
  is_ftVar(X),
  !.
build_rhs(_Sup,neg(X),[neg(X)]) :-
  is_ftVar(X),
  !.

build_rhs(Sup,call(A),Rest) :- !, build_rhs(Sup,(A),Rest).
build_rhs(Sup,call_u(A),Rest) :- !, build_rhs(Sup,(A),Rest).

build_rhs(Sup,(A,B),[A2|Rest]) :-
  !,
  mpred_compile_rhsTerm(Sup,A,A2),
  build_rhs(Sup,B,Rest).

build_rhs(Sup,X,[X2]) :-
   mpred_compile_rhsTerm(Sup,X,X2).


mpred_compile_rhsTerm(_Sup,P,P):-is_ftVar(P),!.
mpred_compile_rhsTerm(Sup,(P/C),((P0:-C0))) :- !,mpred_compile_rhsTerm(Sup,P,P0),build_code_test(Sup,C,C0),!.
mpred_compile_rhsTerm(Sup,I,O):-to_addable_form_wte(mpred_compile_rhsTerm,I,O), must(\+ \+ mpred_mark_as(Sup,r,O,pfcRHSR)),!.



mpred_mark_as(_,_,P,_):- is_ftVar(P),!.

mpred_mark_as(Sup,_PosNeg,neg(P),Type):-!,mpred_mark_as(Sup,neg,P,Type).
mpred_mark_as(Sup,_PosNeg,\+(P),Type):-!,mpred_mark_as(Sup,neg,P,Type).
mpred_mark_as(Sup,_PosNeg,-(P),Type):-!,mpred_mark_as(Sup,neg,P,Type).
mpred_mark_as(Sup,_PosNeg,~(P),Type):-!,mpred_mark_as(Sup,neg,P,Type).
mpred_mark_as(Sup,PosNeg,( P / _ ),Type):- !, mpred_mark_as(Sup,PosNeg,P,Type).
mpred_mark_as(_Sup,_PosNeg,'{}'(  _P ), _Type):- !. % , mpred_mark_as(Sup,PosNeg,P,Type).
mpred_mark_as(_Sup,_PosNeg,( _ :- _ ),_Type):-!.
mpred_mark_as(Sup,PosNeg,( A , B), Type):- !, mpred_mark_as(Sup,PosNeg,A, Type),mpred_mark_as(Sup,PosNeg,B, Type).
mpred_mark_as(Sup,PosNeg,( A ; B), Type):- !, mpred_mark_as(Sup,PosNeg,A, Type),mpred_mark_as(Sup,PosNeg,B, Type).
mpred_mark_as(Sup,PosNeg,( A ==> B), Type):- !, mpred_mark_as(Sup,PosNeg,A, Type),mpred_mark_as(Sup,PosNeg,B, pfcRHS).
mpred_mark_as(Sup,PosNeg,P,Type):-get_functor(P,F,A),ignore(mpred_mark_fa_as(Sup,PosNeg,P,F,A,Type)),!.

:- was_dynamic( pfcMark/4).

% mpred_mark_fa_as(_,_,_,'\=',2,_):- trace.
mpred_mark_fa_as(_Sup, PosNeg,_P,F,A,Type):- pfcMark(Type,PosNeg,F,A),!.
mpred_mark_fa_as(_Sup,_PosNeg,_P,isa,_,_):- !.
mpred_mark_fa_as(_Sup,_PosNeg,_P,t,_,_):- !.
mpred_mark_fa_as(_Sup,_PosNeg,_P,argIsa,N,_):- !,must(N=3).
mpred_mark_fa_as(_Sup,_PosNeg,_P,arity,N,_):- !,must(N=2).
mpred_mark_fa_as(_Sup,_PosNeg,_P,pfcMark,N,_):- !,must(N=4).
mpred_mark_fa_as(_Sup,_PosNeg,_P,mpred_isa,N,_):- must(N=2).
mpred_mark_fa_as(_Sup,_PosNeg,_P,_:mpred_isa,N,_):- must(N=2).
mpred_mark_fa_as(Sup,PosNeg,_P,F,A,Type):- mpred_post_sp_zzz((s(Sup),g),pfcMark(Type,PosNeg,F,A)),!.


lmconf:hook_one_minute_timer_tick:-mpred_cleanup.

mpred_cleanup:- forall((no_repeats(F-A,(pfcMark(pfcRHS,_,F,A),A>1))),mpred_cleanup(F,A)).

mpred_cleanup(F,A):-functor(P,F,A),predicate_property(P,dynamic)->mpred_cleanup_0(P);true.

mpred_cleanup_0(P):- findall(P-B-Ref,clause(P,B,Ref),L),forall(member(P-B-Ref,L),erase_w_attvars(clause(P,B,Ref),Ref)),forall(member(P-B-Ref,L),attvar_op(assertz_if_new,((P:-B)))).

% :-debug.
%isInstFn(A):-!,trace_or_throw(isInstFn(A)).

%= mpred_negation(N,P) is true if N is a negated term and P is the term
%= with the negation operator stripped.

mpred_negation((~P),P).
mpred_negation((-P),P).
mpred_negation((\+(P)),P).

mpred_negated_literal(P):-mpred_negated_literal(P,_).
mpred_negated_literal(P,Q) :- is_ftNonvar(P),
  mpred_negation(P,Q),
  mpred_literal(Q).

mpred_literal_nv(X):-is_ftNonvar(X),mpred_literal(X).
mpred_literal(X) :- is_reprop(X),!,fail.
mpred_literal(X) :- cyclic_term(X),!,fail.
mpred_literal(X) :- atom(X),!.
mpred_literal(X) :- mpred_negated_literal(X),!.
mpred_literal(X) :- mpred_positive_literal(X),!.
mpred_literal(X) :- is_ftVar(X),!.

is_reprop(X):- compound(X),is_reprop_0(X).
is_reprop_0(neg(X)):-!,is_reprop(X).
is_reprop_0(X):-functor(X,repropagate,_).

mpred_non_neg_literal(X):-is_reprop(X),!,fail.
mpred_non_neg_literal(X):-atom(X),!.
mpred_non_neg_literal(X):-mpred_positive_literal(X), X \= neg(_), X \= pfcMark(_,_,_,_), X \= conflict(_).

mpred_non_neg_literal(X):-is_reprop(X),!,fail.
mpred_positive_literal(X) :- is_ftNonvar(X),
  functor(X,F,_),
  \+ mpred_connective(F).

mpred_connective(';').
mpred_connective(',').
mpred_connective('/').
mpred_connective('|').
mpred_connective(('==>')).
mpred_connective(('<-')).
mpred_connective('<==>').

mpred_connective('-').
mpred_connective('~').
mpred_connective('\\+').

cyclic_break(Cyclic):-cyclic_term(Cyclic)->(writeq(cyclic_break(Cyclic)),nl,prolog);true.

process_rule(Lhs,Rhs,Parent_rule) :- 
  copy_term(Parent_rule,Parent_ruleCopy),
  build_rhs((Parent_ruleCopy),Rhs,Rhs2),
   cyclic_break((Lhs,Rhs,Rhs2,Parent_ruleCopy)),
  foreachl_do(mpred_nf(Lhs,Lhs2),
   doall(build_rule(Lhs2,rhs(Rhs2),(Parent_ruleCopy,u)))).

build_rule(Lhs,Rhs,Support) :-
  build_trigger(Support,Lhs,Rhs,Trigger),
   cyclic_break((Lhs,Rhs,Support,Trigger)),
  mpred_eval_lhs(Trigger,Support).

build_trigger(Support,[],Consequent,ConsequentO):- build_consequent(Support,Consequent,ConsequentO).

build_trigger(Support,[V|Triggers],Consequent,kbp:pt(umt,V,X)) :-
  is_ftVar(V),
  !,
  build_trigger(Support,Triggers,Consequent,X).

build_trigger(Support,[added(T)|Triggers],Consequent,kbp:pt(umt,T,X)) :-
  !,
  build_code_test(Support,ground(T),Test2),
  build_trigger(Support,[{Test2}|Triggers],Consequent,X).



build_trigger(Support,[(T1/Test)|Triggers],Consequent,kbp:nt(umt,T2,Test2,X)) :-
  is_ftNonvar(T1),mpred_negation(T1,T2),
  !,
  build_neg_test(Support,T2,Test,Test2),
  build_trigger(Support,Triggers,Consequent,X).

build_trigger(Support,[(T1)|Triggers],Consequent,kbp:nt(umt,T2,Test,X)) :-
  mpred_negation(T1,T2),
  !,
  build_neg_test(Support,T2,true,Test),
  build_trigger(Support,Triggers,Consequent,X).

build_trigger(Support,[{Test}|Triggers],Consequent,(Test->X)) :-
  !,
  build_trigger(Support,Triggers,Consequent,X).

build_trigger(Support,[T/Test|Triggers],Consequent,kbp:pt(umt,T,X)) :-
  !,
  build_code_test(Support,Test,Test2),
  build_trigger(Support,[{Test2}|Triggers],Consequent,X).


%build_trigger(Support,[snip|Triggers],Consequent,snip(X)) :-
%  !,
%  build_trigger(Support,Triggers,Consequent,X).

build_trigger(Support,[T|Triggers],Consequent,kbp:pt(umt,T,X)) :-
  !,
  build_trigger(Support,Triggers,Consequent,X).

%=
%= build_neg_test(Support,+,+,-).
%=
%= builds the test used in a negative trigger (kbp:nt/4).  This test is a
%= conjunction of the check than no matching facts are in the db and any
%= additional test specified in the rule attached to this ~ term.
%=

build_neg_test(Support,T,Testin,Testout) :- must(is_ftNonvar(T)),
  build_code_test(Support,Testin,Testmid),
  conjoin((call_u(T)),Testmid,Testout).


%= this just strips away any currly brackets.

build_code_test(_Support,Test,TestO):-is_ftVar(Test),!,must(is_ftNonvar(Test)),TestO=call_u(Test).
build_code_test(Support,{Test},TestO) :- !,build_code_test(Support,Test,TestO).
build_code_test(Support,Test,TestO):- code_sentence_op(Test),Test=..[F|TestL],must_maplist(build_code_test(Support),TestL,TestLO),TestO=..[F|TestLO],!.
build_code_test(Support,Test,Test):- must(mpred_mark_as(Support,p,Test,pfcCallCode)),!.
build_code_test(_,Test,Test).

code_sentence_op(Var):-is_ftVar(Var),!,fail.
code_sentence_op(rhs(_)).
code_sentence_op(neg(_)).
code_sentence_op(-(_)).
code_sentence_op(~(_)).
code_sentence_op(\+(_)).
code_sentence_op(call_u(_)).
code_sentence_op(call_u(_,_)).
code_sentence_op(Test):-predicate_property(Test,meta_predicate(PP)),predicate_property(Test,built_in),  \+ (( arg(_,PP,N), N\=0)).

all_closed(C):- \+is_ftCompound(C)->true;(functor(C,_,A),A>1,\+((arg(_,C,Arg),is_ftVar(Arg)))),!.

%=

build_consequent(_      ,Test,Test):-is_ftVar(Test),!.
build_consequent(_      ,Test,TestO):-is_ftVar(Test),!,TestO=added(Test).
build_consequent(Support,rhs(Test),rhs(TestO)) :- !,build_consequent(Support,Test,TestO).
build_consequent(Support,Test,TestO):- code_sentence_op(Test),Test=..[F|TestL],maplist(build_consequent(Support),TestL,TestLO),TestO=..[F|TestLO],!.
build_consequent(Support,Test,Test):-must(mpred_mark_as(Support,p,Test,pfcCreates)),!.
build_consequent(_ ,Test,Test).

%= simple typeing for pfc objects

mpred_db_type(('<-'(_,_)),Type) :- !, Type=rule.
mpred_db_type(('<==>'(_,_)),Type) :- !, Type=rule.
mpred_db_type(('<-'(_,_)),Type) :- !, Type=rule.
mpred_db_type((':-'(_,_)),Type) :- !, Type=rule.
mpred_db_type(kbp:pk(umt,_,_,_),Type) :- !, Type=trigger.
mpred_db_type(kbp:pt(umt,_,_),Type) :- !, Type=trigger.
mpred_db_type(kbp:nt(umt,_,_,_),Type) :- !,  Type=trigger.
mpred_db_type(kbp:bt(umt,_,_),Type) :- !,  Type=trigger.
mpred_db_type(mpred_action(_),Type) :- !, Type=action.
mpred_db_type((('::::'(_,X))),Type) :- is_ftNonvar(X),!, mpred_db_type(X,Type).
mpred_db_type(_,fact) :-
  %= if it''s not one of the above, it must be a fact!
  !.



mpred_call_t_exact(Trigger) :- copy_term(Trigger,Copy),mpred_get_trigger_quick(Trigger),Trigger=@=Copy.

retract_t(Trigger) :-  retract_i(kbp:spft(ukb,Trigger,_,_,_)),ignore(retract_i(Trigger)).


mpred_add_t(P,Support) :- 
  (mpred_clause_i(P) ; (assert_i(P),mpred_add_trigger(P,Support))),
  !,
  mpred_add_support(P,Support).



mpred_adda_i(P,Support) :-
  (mpred_clause_i(P) ; asserta_i(P)),
  !,
  mpred_add_support(P,Support).


mpred_addz_i(P,Support) :-  
  (mpred_clause_i(P) ; assertz_i(P)),
  !,
  mpred_add_support(P,Support).


mpred_clause_i((Head :- Body)) :-
  !,
  copy_term(Head,Head_copy),
  copy_term(Body,Body_copy),
  clause_i(Head,Body),
  variant(Head,Head_copy),
  variant(Body,Body_copy).

mpred_clause_i(Head) :-
  % find a unit clause_db identical to Head by finding one which unifies,
  % and then checking to see if it is identical
  copy_term(Head,Head_copy),
  clause_i(Head_copy,true),
  variant(Head,Head_copy).


foreachl_do(Binder,Body) :- Binder,pfcl_do(Body),fail.
foreachl_do(_,_).

% pfcl_do(X) executesf X once and always succeeds.
pfcl_do(X) :- X,!.
pfcl_do(_).


%= mpred_union(L1,L2,L3) - true if set L3 is the result of appending sets
%= L1 and L2 where sets are represented as simple lists.

mpred_union([],L,L).
mpred_union([Head|Tail],L,Tail2) :-
  memberchk(Head,L),
  !,
  mpred_union(Tail,L,Tail2).
mpred_union([Head|Tail],L,[Head|Tail2]) :-
  mpred_union(Tail,L,Tail2).



% ======================= mpred_file('pfcsupport').	% support maintenance

%=
%=
%= predicates for manipulating support relationships
%=


% user:portray(C):-is_ftCompound(C),C=kbp:spft(ukb,_,_,_,_),pp_item('',C).

%= mpred_had_support(+Fact,+Support)
mpred_had_support(P,(Fact,Trigger)) :- 
 ( clause_asserted_local(kbp:spft(ukb,P,Fact,Trigger,_OldWhy)) -> 
    true ; fail).


%= mpred_add_support(+Fact,+Support)

mpred_add_support(P,(Fact,Trigger)) :- 
 ( clause_asserted_local(kbp:spft(ukb,P,Fact,Trigger,_OldWhy)) ->
    true ; 
    (current_why(Why), 
      ( % get_clause_vars(P),get_clause_vars(Fact),get_clause_vars(Trigger),
        get_clause_vars(kbp:spft(ukb,P,Fact,Trigger,Why)),
      attvar_op(assertz,(kbp:spft(ukb,P,Fact,Trigger,Why)))))),!.  % was assert_i

/*
mpred_add_support(P,(Fact,Trigger)) :-
  NEWSUPPORT = kbp:spft(ukb,NewP,NewFact,NewTrigger,NewWhy),
 copy_term(kbp:spft(ukb,P,Fact,Trigger,Why,OldWhy),NEWSUPPORT),
 ( clause_asserted_local(kbp:spft(ukb,P,Fact,Trigger,OldWhy)) ->
    true ; 
    (current_why(NewWhy),attvar_op(assertz,(NEWSUPPORT)))),!. 
*/

mpred_add_support(P,FT) :- trace_or_throw(failed_mpred_add_support(P,FT)).


mpred_get_support(not(P),(Fact,Trigger)) :- is_ftNonvar(P),!, mpred_get_support(neg(P),(Fact,Trigger)).
mpred_get_support(P,(Fact,Trigger)) :- kbp:spft(ukb,P,Fact,Trigger,_)*->true;(is_ftNonvar(P),mpred_get_support_neg(P,(Fact,Trigger))).

% dont mpred_get_support_neg(\+ neg(P),(Fact,Trigger)) :- sp ftY((P),Fact,Trigger).
mpred_get_support_neg(\+ (P),S) :- !, is_ftNonvar(P), mpred_get_support(neg(P),S).
mpred_get_support_neg(~ (P),S) :- !, is_ftNonvar(P), mpred_get_support(neg(P),S).


% There are three of these to try to efficiently handle the cases
% where some of the arguments are not bound but at least one is.

mpred_rem_support(WhyIn,P,(Fact,Trigger)) :- is_ftVar(P),!,copy_term(mpred_rem_support(mpred_rem_support,P,(Fact,Trigger)) ,TheWhy),
  SPFC = kbp:spft(ukb,RP,RFact,RTrigger,_RWhy),
  clause(kbp:spft(ukb,P,Fact,Trigger,_),true,Ref),
  ((clause(SPFC,true,Ref),
     ( spftV(RP,RFact,RTrigger) =@= spftV(P,Fact,Trigger) -> 
        erase_w_attvars(clause(SPFC,true,Ref),Ref); 
       (mpred_trace_msg(<=(TheWhy,~SPFC)),nop(mpred_retract_or_warn_i(spftVVVVVVV(P,Fact,Trigger))),nop(trace))),
   (is_ftVar(P)->trace_or_throw(is_ftVar(P));remove_if_unsupported_verbose(WhyIn,local,P)))).
mpred_rem_support(Why,(\+ N) , S):- mpred_rem_support(Why,neg(N),S).
mpred_rem_support(_Why,P,(Fact,Trigger)):-mpred_retract_or_warn_i(kbp:spft(ukb,P,Fact,Trigger,_)).

/*
% TODO not called yet
mpred_collect_supports_f_l(Tripples) :-
  bagof(Tripple, mpred_support_relation(Tripple), Tripples),
  !.
mpred_collect_supports_f_l([]).
*/
/* UNUSED TODAY
% TODO not called yet
mpred_support_relation((P,F,T)) :- kbp:spft(ukb,P,F,T,_).

% TODO not called yet
mpred_make_supports_f_l((P,S1,S2)) :-
  % was mpred_add_support(P,(S1,S2),_),
  mpred_add_support(P,(S1,S2)),
  (mpred_add_db_type(P); true),
  !.
*/

is_relative(V):- (\+is_ftCompound(V)),!,fail.
is_relative(update(_)).
is_relative(replace(_)).
is_relative(rel(_)).
is_relative(+(X)):- \+ is_ftVar(X).
is_relative(-(X)):- \+ is_ftVar(X).
is_relative(*(X)):- \+ is_ftVar(X).

/*
% TODO not called yet
%= mpred_get_trigger_key(+Trigger,-Key)
%=
%= Arg1 is a trigger.  Key is the best term to index it on.

mpred_get_trigger_key(kbp:pt(umt,Key,_),Key).
mpred_get_trigger_key(kbp:pk(umt,Key,_,_),Key).
mpred_get_trigger_key(kbp:nt(umt,Key,_,_),Key).
mpred_get_trigger_key(Key,Key).
*/

/*
% TODO not called yet
%=^L
%= Get a key from the trigger that will be used as the first argument of
%= the trigger baseable clause that stores the trigger.
%=
mpred_trigger_key(X,X) :- is_ftVar(X), !.
mpred_trigger_key(chart(word(W),_L),W) :- !.
mpred_trigger_key(chart(stem([Char1|_Rest]),_L),Char1) :- !.
mpred_trigger_key(chart(Concept,_L),Concept) :- !.
mpred_trigger_key(X,X).
*/
% ======================= mpred_file('pfcdb').	% predicates to manipulate database.

%   File   : pfcdb.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Author :  Dan Corpron
%   Updated: 10/11/87, ...
%   Purpose: predicates to manipulate a pfc database (e.g. save,
%=	restore, reset, etc).

% mpred_database_term(P/A) is true iff P/A is something that pfc adds to
% the database and should not be present in an empty pfc database

mpred_database_term(kbp:spft/5).
mpred_database_term(kbp:pk/4).
mpred_database_term(kbp:bt/3).  % was 3
mpred_database_term(kbp:nt/4). % was 4
mpred_database_term('<-'/2).
mpred_database_term('==>'/2).
mpred_database_term('<==>'/2).
mpred_database_term(kbp:qu/3).

:- forall(mpred_database_term(T),was_shared_multifile(T)).


% removes all forward chaining rules and justifications from db.

mpred_reset :-
  clause_i(kbp:spft(ukb,P,F,Trigger,Why),true),
  mpred_retract_or_warn_i(P),
  mpred_retract_or_warn_i(kbp:spft(ukb,P,F,Trigger,Why)),
  fail.
mpred_reset :-
  mpred_database_item(T),
  mpred_error("Pfc database not empty after mpred_reset, e.g., ~p.",[T]).
mpred_reset.

% true if there is some pfc crud still in the database.
mpred_database_item(Term) :-
  mpred_database_term(P/A),
  functor(Term,P,A),
  clause_u(Term,_).

mpred_retract_or_warn_i(X) :- retract_i(X),mpred_trace_msg("Success retract: ~p.",[X]),!.
mpred_retract_or_warn_i(X) :- \+ \+ X =kbp:spft(ukb,neg(_),_,_,_),!.
mpred_retract_or_warn_i(X) :- ground(X),mpred_trace_msg("Couldn't retract ~p.",[X]),!.
mpred_retract_or_warn_i(_).

% ======================= mpred_file('pfcdebug').	% debugging aids (e.g. tracing).
%   File   : pfcdebug.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Updated:
%   Purpose: provides predicates for examining the database and debugginh
%   for Pfc.

:- dynamic mpred_is_tracing/1.
:- dynamic mpred_is_spying/2.
:- thread_local mpred_is_tracing/1.
:- dynamic mpred_warnings/1.

mpred_is_tracing(_):- mpred_is_tracing_exec ; t_l:mpred_debug_local.

lmconf:module_local_init:- lmconf:mpred_init_i(mpred_warnings(_), mpred_warnings(true)).



get_fa(PI,_F,_A):-is_ftVar(PI),!.
get_fa(F/A,F,A):- !.
get_fa(PI,PI,_A):- atomic(PI),!.
get_fa(PI,F,A):- is_ftCompound(PI),!,functor(PI,F,A).
get_fa(Mask,F,A):-get_functor(Mask,F,A).


clause_or_call(M:H,B):-is_ftVar(M),!,no_repeats(M:F/A,(f_to_mfa(H,M,F,A))),M:clause_or_call(H,B).
clause_or_call(isa(I,C),true):-!,req(isa_asserted(I,C)).
clause_or_call(genls(I,C),true):-!,on_x_log_throw(req(genls(I,C))).
clause_or_call(H,B):- clause(src_edit(_Before,H),B).
clause_or_call(H,B):- predicate_property(H,number_of_clauses(C)),predicate_property(H,number_of_rules(R)),((R*2<C) -> (clause(H,B)*->!;fail) ; clause(H,B)).
clause_or_call(H,true):- should_call_for_facts(H),no_repeats(on_x_log_throw(H)).


% as opposed to simply using clause(H,true).
should_call_for_facts(H):- get_functor(H,F,A),should_call_for_facts(H,F,A).
should_call_for_facts(_,F,_):- prologSideEffects(F),!,fail.
should_call_for_facts(H,_,_):- \+ predicate_property(H,number_of_clauses(_)),!.
should_call_for_facts(_,F,A):- pfcMark(pfcRHS,_,F,A),!,fail.
should_call_for_facts(_,F,A):- pfcMark(pfcMustFC,_,F,A),!,fail.
should_call_for_facts(_,F,_):- prologDynamic(F),!.
should_call_for_facts(_,F,_):- \+ pfcControlled(F),!.

no_side_effects(S):- get_functor(S,F), (prologSideEffects(F)-> (\+ is_side_effect_disabled);true).

is_disabled_clause(C):-is_edited_clause(C,_,New),memberchk((disabled),New).

%= mpred_fact(P) is true if fact P was asserted into the database via mpred_add.

mpred_fact(P) :- mpred_fact(P,true).

%= mpred_fact(P,C) is true if fact P was asserted into the database via
%= assert and condition C is satisfied.  For example, we might do:
%=
%=  mpred_fact(X,mpred_user_fact(X))
%=

mpred_user_fact(X):-no_repeats(kbp:spft(ukb,X,U,U,_)).

mpred_fact(P,PrologCond) :-
  mpred_get_support(P,_),is_ftNonvar(P),
  once(mpred_db_type(P,F)),F=fact,
  call_prologsys(PrologCond).

%= mpred_facts(-ListofPfcFacts) returns a list of facts added.

mpred_facts(L) :- mpred_facts(_,true,L).

mpred_facts(P,L) :- mpred_facts(P,true,L).

%= mpred_facts(Pattern,Condition,-ListofPfcFacts) returns a list of facts added.

mpred_facts(P,C,L) :- setof(P,mpred_fact(P,C),L).

brake(X) :-  X, break.

%=
%=
%= predicates providing a simple tracing facility
%=
/*
NOT NEEDED ANYMORE
mpred_trace_add(P) :-
  % this is here for upward compat. - should go away eventually.
  mpred_trace_add(P,(o,o)).
*/
/*
mpred_trace_add(kbp:bt(umt,Head,Body)) :-
  % hack for now - never trace triggers.
  !.
mpred_trace_add(kbp:pt(umt,Head,Body)) :-
  % hack for now - never trace triggers.
  !.
mpred_trace_add(kbp:nt(umt,Head,Condition,Body)) :-
  % hack for now - never trace triggers.
  !.
*/
mpred_trace_add(P,S) :-
   mpred_trace_addPrint(P,S),
   mpred_trace_break(P,S).


mpred_trace_addPrint(P,S):- (\+ \+ mpred_trace_addPrint_0(P,S)).
mpred_trace_addPrint_0(P,S) :-
  mpred_is_tracing(P),
  !,
  must(S=(F,T)),
  (F==T
       -> mpred_trace_msg("Adding (~p) ~p ",[F,P])
        ; (((mpred_trace_msg("Adding (:) ~p    <-------- ~n (~p <-TF-> ~p)",[P,(T),(F)]))))).

mpred_trace_addPrint_0(_,_).


mpred_trace_break(P,_S) :-
  mpred_is_spying(P,add) ->
   ((\+ \+ wdmsg("Breaking on mpred_add(~p)",[P])),
    break)
   ; true.

/*
mpred_trace_rem(Why,kbp:bt(umt,Head,Body)) :-
  % hack for now - never trace triggers.
  !.
mpred_trace_rem(Why,kbp:pt(umt,Head,Body)) :-
  % hack for now - never trace triggers.
  !.
mpred_trace_rem(Why,kbp:nt(umt,Head,Condition,Body)) :-
  % hack for now - never trace triggers.
  !.
*/

mpred_trace_rem(Why,P) :-
  ((mpred_is_tracing(P);mpred_is_tracing(Why))
     -> (mpred_trace_msg('Removing (~p) ~p.',[Why,P]))
      ; true),
  ((mpred_is_spying(P,rem);mpred_is_spying(P,Why))
     -> (in_cmt(wdmsg("Breaking on remove(~p,~p)",[Why,P])), break)
   ; true),!.


mpred_no_spy_all :- mpred_no_spy, retractall_i(mpred_is_tracing_exec).
mpred_no_trace_all :-  retractall_i(mpred_is_tracing_exec).
mpred_spy_all :- assert_i(mpred_is_tracing_exec).
mpred_trace_exec :- assert_i(mpred_is_tracing_exec).
mpred_trace :- mpred_trace(_).
lmconf:module_local_init:-mpred_no_trace_all.

mpred_trace(Form) :-
  assert_i(mpred_is_tracing(Form)).

mpred_trace(Form,Condition) :-
  assert_i((mpred_is_tracing(Form) :- Condition)).

mpred_spy(Form) :- mpred_spy(Form,[add,rem],true).

mpred_spy(Form,Modes) :- mpred_spy(Form,Modes,true).

mpred_spy(Form,[add,rem],Condition) :-
  !,
  mpred_spy1(Form,add,Condition),
  mpred_spy1(Form,rem,Condition).

mpred_spy(Form,Mode,Condition) :-
  mpred_spy1(Form,Mode,Condition).

mpred_spy1(Form,Mode,Condition) :-
  assert_i((mpred_is_spying(Form,Mode) :- Condition)).

mpred_no_spy :- mpred_no_spy(_,_,_).

mpred_no_spy(Form) :- mpred_no_spy(Form,_,_).

mpred_no_spy(Form,Mode,Condition) :-
  clause_i(mpred_is_spying(Form,Mode), Condition, Ref),
  erase_w_attvars(clause_i(mpred_is_spying(Form,Mode), Condition, Ref),Ref),
  fail.
mpred_no_spy(_,_,_).

mpred_no_trace :- mpred_untrace.
mpred_untrace :- mpred_untrace(_).
mpred_untrace(Form) :- retractall_i(mpred_is_tracing(Form)).

% needed:  mpred_trace_rule(Name)  ...


% if the correct flag is set, trace exection of Pfc
mpred_trace_msg(Msg) :- mpred_trace_msg('~p.',[Msg]),!.

mmsg(Msg,Args):- is_list(Args) -> wdmsg(Msg, Args) ; mpred_trace_item(Msg, Args).

:- was_dynamic(mpred_hide_msg/1).
mpred_hide_msg('Adding For Later').
mpred_hide_msg('Skipped Trigger').
mpred_hide_msg('Had Support').

mpred_trace_msg(Msg,Args) :- !, mmsg(Msg,Args).
% mpred_trace_msg(Msg,_Args) :- mpred_hide_msg(Msg),!.
% mpred_trace_msg(Msg,Args) :- ignore((mpred_is_tracing_exec,!,\+ mpred_is_silient, !, mmsg(Msg,Args),!.




mpred_watch :- assert_i(mpred_is_tracing_exec).

mpred_no_watch :-  retractall_i(mpred_is_tracing_exec).

mpred_error(Msg) :-  mpred_error(Msg,[]).

mpred_error(Msg,Args) :-
 ignore((in_cmt(( wdmsg("ERROR/Pfc: ",[]),wdmsg(Msg,Args))))),!.


%=
%= These control whether or not warnings are printed at all.
%=   mpred_warn.
%=   nompred_warn.
%=
%= These print a warning message if the flag mpred_warnings is set.
%=   mpred_warn(+Message)
%=   mpred_warn(+Message,+ListOfArguments)
%=

mpred_warn :-
  retractall_i(mpred_warnings(_)),
  assert_i(mpred_warnings(true)).

nompred_warn :-
  retractall_i(mpred_warnings(_)),
  assert_i(mpred_warnings(false)).

mpred_warn(Msg) :-  mpred_warn(Msg,[]).

lmconf:module_local_init:-mpred_warn.

mpred_warn(Msg,Args) :-
  gethostname(ubuntu),!,
 ignore((
  sformat(S, Msg,Args),
  show_source_location,
  wdmsg(pfc(warn(S))))),!,trace.

mpred_warn(Msg,Args) :-
 ignore((
 (mpred_warnings(true); \+ mpred_is_silient),
  !,
  sformat(S, Msg,Args),
  show_source_location,
  wdmsg(pfc(warn(S))))),!.


%=
%= mpred_warnings/0 sets flag to cause pfc warning messages to print.
%= mpred_no_warnings/0 sets flag to cause pfc warning messages not to print.
%=

mpred_warnings :-
  retractall_i(mpred_warnings(_)),
  assert_i(mpred_warnings(true)).

mpred_no_warnings :-
  retractall_i(mpred_warnings(_)).


% ======================= mpred_file('pfcjust').	% predicates to manipulate justifications.


%   File   : pfcjust.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Updated:
%   Purpose: predicates for accessing Pfc justifications.
%   Status: more or less working.
%   Bugs:

%= *** predicates for exploring supports of a fact *****


:- use_module(library(lists)).

justification(F,J) :- supports_f_l(F,J).

justifications(F,Js) :- bagof(J,justification(F,J),Js).

%= baseable(P,L) - is true iff L is a list of "baseable" facts which, taken
%= together, allows us to deduce P.  A baseable fact is an mpred_axiom (a fact
%= added by the user or a raw Prolog fact (i.e. one w/o any support))
%= or an assumption.

baseable(F,[F]) :- (mpred_axiom(F) ; assumption(F)),!.
baseable(F,L) :-
  % i.e. (reduce 'append (map 'baseable (justification f)))
  justification(F,Js),
  baseable_list(Js,L).


%= baseable_list(L1,L2) is true if list L2 represents the union of all of the
%= facts on which some conclusion in list L1 is based.

baseable_list([],[]).
baseable_list([X|Rest],L) :-
  baseable(X,Bx),
  baseable_list(Rest,Br),  
  mpred_union(Bx,Br,L).
	

mpred_axiom(F) :-
  %mpred_get_support(F,UU);
  %mpred_get_support(F,(g,g));
  mpred_get_support(F,(OTHER,OTHER)).

% mpred_axiom(F) :-  mpred_get_support(F,(U,U)).

%= an assumption is a failed action, i.e. were assuming that our failure to
%= prove P is a proof of not(P)

assumption(P) :- is_ftNonvar(P),mpred_negation(P,_).

%= assumptions(X,As) if As is a set of assumptions which underly X.

assumptions(X,[X]) :- assumption(X).
assumptions(X,[]) :- mpred_axiom(X).
assumptions(X,L) :-
  justification(X,Js),
  assumptions1(Js,L).

assumptions1([],[]).
assumptions1([X|Rest],L) :-
  assumptions(X,Bx),
  assumptions1(Rest,Br),
  mpred_union(Bx,Br,L).


%= pfcProofTree(P,T) the proof tree for P is T where a proof tree is
%= of the form
%=
%=     [P , J1, J2, ;;; Jn]         each Ji is an independent P justifier.
%=          ^                         and has the form of
%=          [J11, J12,... J1n]      a list of proof trees.


% mpred_child(P,Q) is true iff P is an immediate justifier for Q.
% mode: mpred_child(+,?)

mpred_child(P,Q) :-
  mpred_get_support(Q,(P,_)).

mpred_child(P,Q) :-
  mpred_get_support(Q,(_,Trig)),
  mpred_db_type(Trig,trigger),
  mpred_child(P,Trig).

mpred_children(P,L) :- bagof(C,mpred_child(P,C),L).

% mpred_descendant(P,Q) is true iff P is a justifier for Q.

mpred_descendant(P,Q) :-
   mpred_descendant1(P,Q,[]).

mpred_descendant1(P,Q,Seen) :-
  mpred_child(X,Q),
  (\+ member(X,Seen)),
  (P=X ; mpred_descendant1(P,X,[X|Seen])).

mpred_descendants(P,L) :-
  bagof(Q,mpred_descendant1(P,Q,[]),L).


:- was_dynamic(kb:prologMacroHead/1).

compute_resolve(NewerP,OlderQ,SU,SU,(mpred_remove3(OlderQ),mpred_add(NewerP,S),mpred_rem1(conflict(NewerP)))):-
  must(correctify_support(SU,S)),
  wdmsg(compute_resolve(newer(NewerP-S)>older(OlderQ-S))).
compute_resolve(NewerP,OlderQ,S1,[U],Resolve):-compute_resolve(OlderQ,NewerP,[U2],S1,Resolve),match_source_ref1(U),match_source_ref1(U2),!.
compute_resolve(NewerP,OlderQ,SU,S2,(mpred_remove3(OlderQ),mpred_add(NewerP,S1),mpred_rem1(conflict(NewerP)))):-
  must(correctify_support(SU,S1)),
  wdmsg(compute_resolve((NewerP-S1)>(OlderQ-S2))).


compute_resolve(NewerP,OlderQ,Resolve):-
   supports_f_l(NewerP,S1),
   supports_f_l(OlderQ,S2),
   compute_resolve(NewerP,OlderQ,S1,S2,Resolve).


is_resolved(C):- Why= is_resolved, mpred_call_only_facts(Why,C),\+mpred_call_only_facts(Why,neg(C)).
is_resolved(C):- Why= is_resolved, mpred_call_only_facts(Why,neg(C)),\+mpred_call_only_facts(Why,C).

:- must(nop(_)).

mpred_prove_neg(G):-nop(trace), \+ mpred_bc_only(G), \+ mpred_fact(G).

pred_head(Type,P):- no_repeats_u(P,(call(Type,P),\+ nonfact_metawrapper(P),is_ftCompound(P))).

pred_head_all(P):- pred_head(pred_all,P).

nonfact_metawrapper(neg(_)).
nonfact_metawrapper(kbp:pt(umt,_,_)).
nonfact_metawrapper(kbp:bt(umt,_,_)).
nonfact_metawrapper(kbp:nt(umt,_,_,_)).
nonfact_metawrapper(kbp:spft(ukb,_,_,_,_)).
nonfact_metawrapper(added(_)).
% we use the arity 1 forms is why 
nonfact_metawrapper(term_expansion(_,_)).
nonfact_metawrapper(P):- \+ current_predicate(_,P).
nonfact_metawrapper(P):- functor(P,F,_), 
   (mpred_call_shared(prologSideEffects(F));mpred_call_shared(tNotForUnboundPredicates(F))).
nonfact_metawrapper(P):-rewritten_metawrapper(P).

rewritten_metawrapper(_):-!,fail.
%rewritten_metawrapper(isa(_,_)).
rewritten_metawrapper(C):-is_ftCompound(C),functor(C,t,_).

meta_wrapper_rule((_<-_)).
meta_wrapper_rule((_<==>_)).
meta_wrapper_rule((_==>_)).
meta_wrapper_rule((_:-_)).


pred_all(P):-pred_u0(P).
pred_all(P):-pred_t0(P).
pred_all(P):-pred_r0(P).

pred_u0(P):-pred_u1(P),has_db_clauses(P).
pred_u0(P):-pred_u2(P).
pred_u1(P):-pfcControlled(F),arity(F,A),functor(P,F,A).
pred_u1(P):-prologHybrid(F),arity(F,A),functor(P,F,A).
pred_u1(P):-prologDynamic(F),arity(F,A),functor(P,F,A).
pred_u2(P):-support_hilog(F,A),functor(P,F,A),has_db_clauses(P).
pred_u2(P):-clause(arity(F,A),true),functor(P,F,A),has_db_clauses(P).

has_db_clauses(P):- predicate_property(P,number_of_clauses(NC)),\+ predicate_property(P,number_of_rules(NC)), \+ \+ clause(P,true).

pred_t0(P):-mpred_call_shared(kbp:pt(umt,P,_)).
pred_t0(P):-mpred_call_shared(kbp:bt(umt,P,_)).
pred_t0(P):-mpred_call_shared(kbp:nt(umt,P,_,_)).
pred_t0(P):-mpred_call_shared(kbp:spft(ukb,P,_,_,_)).
pred_t0(P):- mpred_call_shared('nesc'(P)).
%pred_r0(~(P)):- mpred_call_shared(~(P)).
%pred_r0(neg(P)):- mpred_call_shared(neg(P)).

pred_r0(P==>Q):- mpred_call_shared(P==>Q).
pred_r0(P<==>Q):- mpred_call_shared(P<==>Q).
pred_r0(P<-Q):- mpred_call_shared(P<-Q).

cnstrn(X):-term_variables(X,Vs),maplist(cnstrn0(X),Vs),!.
cnstrn(V,X):-cnstrn0(X,V).
cnstrn0(X,V):-when(is_ftNonvar(V),X).

rescan_pfc:-forall(clause(lmconf:mpred_hook_rescan_files,Body),show_call_entry(Body)).

mpred_facts_and_universe(P):- (is_ftVar(P)->pred_head_all(P);true),(meta_wrapper_rule(P)->(no_repeats(on_x_rtrace(P))) ; (no_repeats(on_x_rtrace(P)))).

add_reprop(_Trig,Var):- is_ftVar(Var), !. % trace_or_throw(add_reprop(Trig,Var)).
add_reprop(_Trig,neg(Var)):- is_ftVar(Var),!.
% CREATES ERROR!!!  add_reprop(_Trig,neg(_Var)):-!.
add_reprop(_Trig,neg(repropagate(Var))):- \+ is_ftVar(Var),!.
add_reprop(_Trig,repropagate(neg(Var))):- \+ is_ftVar(Var),!.
add_reprop(_Trig,repropagate(Var)):- \+ is_ftVar(Var),!.
% add_reprop(_Trig,_):-!.
add_reprop(Trig,(H:-B)):- trace_or_throw(add_reprop(Trig,(H:-B))).


add_reprop(Trig ,Trigger):-
  w_tl(t_l:current_why_source(Trig),
    attvar_op(assertz_if_new,(kbp:qu(umt,repropagate(Trigger),(g,g))))).

 
repropagate(P):- is_ftVar(P),!.
repropagate(P):-  (meta_wrapper_rule(P))->repropagate_meta_wrapper_rule(P);fail.
repropagate(P):-  \+ predicate_property(P,_),'$find_predicate'(P,PP),PP\=[],!,forall(member(M:F/A,PP),
                                                          must((functor(Q,F,A),repropagate_1(M:Q)))).
repropagate(F/A):- atom(F),integer(A),!,functor(P,F,A),!,repropagate(P).
repropagate(F/A):- atom(F),is_ftVar(A),!,repropagate(F).
repropagate(P):-  must(repropagate_0(P)).

repropagate_0(P):- loop_check(repropagate_1(P),true).

:- thread_local t_l:is_repropagating/1.

repropagate_1(P):- is_ftVar(P),!.
repropagate_1(USER:P):- USER==user,!,repropagate_1(P).
repropagate_1((P/_)):-!,repropagate_1(P).
repropagate_1(P):-
 forall(mpred_facts_and_universe(P),
  w_tl(t_l:is_repropagating(P),
  ignore((
   once(fwd_ok(P)),
   mpred_fwd(P))))).

% repropagate_meta_wrapper_rule(P==>_):- !, repropagate(P).
repropagate_meta_wrapper_rule(P):-repropagate_1(P).

fwd_ok(P):-ground(P),!.
fwd_ok(if_missing(_,_)).
fwd_ok(idForTest(_,_)).
fwd_ok(clif(_)).
% fwd_ok(_).
% fwd_ok(P):-must(ground(P)),!.

mpred_facts_only(P):- (is_ftVar(P)->(pred_head_all(P),\+ meta_wrapper_rule(P));true),(no_repeats(on_x_rtrace(P))).

:- thread_local(t_l:in_rescan_mpred_hook/0).
lmconf:mpred_hook_rescan_files:- forall(mpred_facts_and_universe(P),w_tl(t_l:in_rescan_mpred_hook,mpred_fwd(P))).
lmconf:mpred_hook_rescan_files:- forall(mpred_facts_and_universe(P),w_tl(t_l:in_rescan_mpred_hook,mpred_scan_tms(P))).
/*
lmconf:mpred_hook_rescan_files:- forall(pred_head(pred_u0,P), 
                          forall(no_repeats(P,call(P)),
                            show_if_debug(mpred_fwd(P)))).
*/


:- retractall(t_l:mpred_debug_local).
:- retractall(mpred_is_tracing_exec).
:- retractall(mpred_is_tracing(_)).

mpred_mpred_file. 

:- source_location(S,_),forall(source_file(H,S),(functor(H,F,A),export(F/A),module_transparent(F/A))).

% :- doall(lmconf:module_local_init).
