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
            assert_u/4,
          assertz_u/1,
          check_never_assert/1,
          check_never_retract/1,
          assertz_mu/2,
            % mpred_select/2,
            asserta_i/1,
            assertz_i/1,
            check_context_module/0,
            repropagate_2/1,
            pfc_add/1,
            assumption/1,
            assumptions/2,
            assumptions1/2,
            {}/1,
            neg_in_code/1,
            attvar_op/2,
            with_umt/1,
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
            cwc/0,
            defaultmpred_select/2,            
            if_missing_mask/3,
            if_missing_mask/4,
            which_missing_argnum/2,
            erase_w_attvars/2,
            exact_args/1,
            fc_eval_action/2,
            fcnt/2,
            mmsg/2,
            fcnt0/2,
            fcpt/2,
          f_to_mfa/4,
          fa_to_p/3,
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
            ain/1,
            ain/2,
            mpred_ain/1,
            mpred_aina/1,
            mpred_ainz/1,
            mpred_ainz/2,
            mpred_ain/2,
            ain_actiontrace/2,
            ain_db_to_head/2,
            ain_fast/1,
            ain_fast/2,
            ain_fast_sp/2,
            ain_fast_sp0/2,
            ain_fast_timed/2,
            ain_minfo/1,
            ain_minfo/2,
            ain_minfo_2/2,
            ain_rule0/1,
            ain_rule_if_rule/1,
            ain_support/2,
            ain_t/2,
            ain_trigger/2,
            ain_trigger_0/3,
            aina_i/2,            
            ainz_i/2,
            mpred_axiom/1,
            mpred_bc_only/1,
            mpred_call_with_no_triggers_uncaugth/1,
            mpred_bt_pt_combine/3,
            req/1,
            mpred_call_0/1,
            mpred_call_1/3,
            mpred_call_only_facts/1,
            mpred_call_only_facts/2,
            mreq/1,
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
            mpred_mark_as_ml/4,
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
            mpred_notrace_exec/0,
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
            with_mpred_trace_exec/1
        
          ]).
/*
:- shared_multifile    
        lmconf:hook_one_minute_timer_tick/0,
        lmconf:infoF/1,
        lmconf:mpred_hook_rescan_files/0,
        baseKB:resolveConflict/1,
        baseKB:resolverConflict_robot/1.
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
      % baseKB:resolveConflict(0),baseKB:resolveConflict0(0),baseKB:resolverConflict_robot(0),
      loop_check_nr(0),
      map_unless(1,:,*,*),
      ain_fast_sp(?,0),
      ain_fast_sp0(?,0),
      ain_minfo(1,*),
      ain_minfo_2(1,*),
      ain_rule_if_rule(0),
      mpred_bc_only(0),
      req(0),
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
      mpred_test(*),
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
      loop_check_nr((*)),
      map_unless(1,:,*,*),
      ain_fast_sp(?,(*)),
      ain_fast_sp0(?,(*)),
      ain_minfo(1,*),
      ain_minfo_2(1,*),
      ain_rule_if_rule((*)),
      mpred_bc_only((*)),
      req((*)),
      with_umt((*)),
      mpred_call_0((*)),
      mpred_call_only_facts(*,(*)),
      mpred_call_only_facts((*)),
      mpred_call_with_no_triggers((*)),
      mpred_call_with_no_triggers_uncaugth((*)),
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
        %mpred_select/2,
        mpred_is_tracing_exec/0,
        use_presently/0)).

:- module_transparent 
            add_reprop/2,
            add_side_effect/2,
            all_closed/1,
            append_as_first_arg/3,
            assert_eq_quitely/1,
            assert_i/1,
            assert_u/1,
            assert_u/4,
          assertz_u/1,
          assertz_mu/2,
            % mpred_select/2,
            asserta_i/1,
            assertz_i/1,
            check_context_module/0,
            repropagate_2/1,
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
            cwc/0,
            defaultmpred_select/2,            
            if_missing_mask/3,
            if_missing_mask/4,
            which_missing_argnum/2,
            erase_w_attvars/2,
            exact_args/1,
            fc_eval_action/2,
            fcnt/2,
            mmsg/2,
            fcnt0/2,
            fcpt/2,
          f_to_mfa/4,
          fa_to_p/3,
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
            ain/1,
            ain/2,
            mpred_ain/1,
            mpred_aina/1,
            mpred_ainz/1,
            mpred_ainz/2,
            mpred_ain/2,
            ain_actiontrace/2,
            ain_db_to_head/2,
            ain_fast/1,
            ain_fast/2,
            ain_fast_sp/2,
            ain_fast_sp0/2,
            ain_fast_timed/2,
            ain_minfo/1,
            ain_minfo/2,
            ain_minfo_2/2,
            ain_rule0/1,
            ain_rule_if_rule/1,
            ain_support/2,
            ain_t/2,
            ain_trigger/2,
            ain_trigger_0/3,
            aina_i/2,            
            ainz_i/2,
            mpred_axiom/1,
            mpred_bc_only/1,
            mpred_call_with_no_triggers_uncaugth/1,
            mpred_bt_pt_combine/3,
            req/1,
            mpred_call_0/1,
            mpred_call_1/3,
            mpred_call_only_facts/1,
            mpred_call_only_facts/2,
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
            mpred_notrace_exec/0,
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
            with_umt/1.
:- module_transparent(check_context_module/0).
check_context_module:- must((source_context_module(M),M\==mpred_pfc,M\==mpred_loader)).
check_real_context_module:- must((context_module(M),M\==mpred_pfc,M\==mpred_loader)).


:- shared_multifile(basePFC:bt/3).
:- shared_multifile(basePFC:nt/4).
:- shared_multifile(basePFC:pk/4).
:- shared_multifile(basePFC:pt/3).
:- shared_multifile(basePFC:spft/5).
:- shared_multifile(basePFC:tms/1).
:- shared_multifile(basePFC:hs/1).
:- shared_multifile(basePFC:qu/3).
:- shared_multifile(basePFC:sm/1).
:- shared_multifile(basePFC:sm/1).
:- shared_multifile(mpred_do_and_undo_method/2).
/*
:- shared_multifile(('==>')/1).
:- shared_multifile(('::::')/2).
:- shared_multifile(('<-')/2).
:- shared_multifile(('<==>')/2).
:- shared_multifile(('==>')/2).
:- shared_multifile(('~')/1).
:- shared_multifile(('nesc')/1).
:- shared_multifile((('~'))/1).
*/
:- shared_multifile((mpred_action)/1).
:- foreach(arg(_,isEach(prologMultiValued,prologOrdered,prologNegByFailure,prologPTTP,prologKIF,pfcControlled,ttPredType,
     prologHybrid,predCanHaveSingletons,prologDynamic,prologBuiltin,prologMacroHead,prologListValued,prologSingleValued),P),
   ((shared_multifile(baseKB:P/1)))).
:- shared_multifile(basePFC:hs/2).
:- shared_multifile(pfcControlled/1).
:- shared_multifile(prologDynamic/2).
:- shared_multifile(prologSideEffects/1).
:- shared_multifile(prologSingleValued/1).
:- shared_multifile(singleValuedInArg/2).
:- shared_multifile(prologSideEffects/1).

:- was_dynamic(lmconf:module_local_init/0).
:- discontiguous(lmconf:module_local_init/0).

:- include('mpred_header.pi').

% ======================= mpred_file('pfcsyntax').	% operator declarations.
:- was_module_transparent(with_umt/1).
:- was_export(with_umt/1).
with_umt(G):- get_user_abox(M),!, M:call(G).
with_umt(Goal):- source_context_module(M) -> M:call(Goal).

%   File   : pfcsyntax.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Purpose: syntactic sugar for Pfc - operator definitions and term expansions.

:-
 op(1199,fx,('==>')), 
 op(1190,xfx,('::::')),
 op(1180,xfx,('==>')),
 op(1170,xfx,'<==>'),  
 op(1160,xfx,('<-')),
 op(1150,xfx,'=>'),
 op(1140,xfx,'<='),
 op(1130,xfx,'<=>'), 
 op(600,yfx,'&'), 
 op(600,yfx,'v'),
 op(350,xfx,'xor'),
 op(300,fx,'~'),
 op(300,fx,'-').

:- op(1100,fx,(shared_multifile)).



mreq(G):- if_defined_else(G,fail).

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
 PFC forwards and helps maintain in visible states )  in prolog knowledge baseable.. We use basePFC:spft/5 to track deductions
Research~wise LogicMOO has a main purpose is to prove that grounded negations (of contrapostives) are of first class in importance in helping
with Wff checking/TMS 
Also alows an inference engine constrain search.. PFC became important since it helps memoize and close off (terminate) transitive closures

*/

is_side_effect_disabled:- t_l:no_physical_side_effects,!.
is_side_effect_disabled:- t_l:side_effect_ok,!,fail.
is_side_effect_disabled:- t_l:noDBaseMODs(_),!.


f_to_mfa(EF,R,F,A):-w_get_fa(EF,F,A),
              (((current_predicate(F/A),functor(P,F,A),predicate_property(_M:P,imported_from(R)))*->true;
              current_predicate(F/A),functor(P,F,A),source_file(R:P,_SF))),
              current_predicate(R:F/A).

w_get_fa(PI,_F,_A):-is_ftVar(PI),!.
w_get_fa(F/A,F,A):- !.
w_get_fa(PI,PI,_A):- atomic(PI),!.
w_get_fa(PI,F,A):- is_ftCompound(PI),!,functor(PI,F,A).
w_get_fa(Mask,F,A):-get_functor(Mask,F,A).


set_prolog_stack_gb(Six):-set_prolog_stack(global, limit(Six*10**9)),set_prolog_stack(local, limit(Six*10**9)),set_prolog_stack(trail, limit(Six*10**9)).
lmconf:module_local_init:-set_prolog_stack_gb(16).
:- shared_multifile(lmconf:mpred_hook_rescan_files/0).
:- was_dynamic(lmconf:mpred_hook_rescan_files/0).
:- was_dynamic(use_presently/0).
% used to annotate a predciate to indicate PFC support
:- shared_multifile(infoF/1).
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
:- compiled(('~')/1).
:- compiled(('<-')/2).
:- compiled(('==>')/2).
:- compiled(('::::')/2).
:- compiled(('<==>')/2).
*/

:- thread_local((t_l:use_side_effect_buffer , t_l:verify_side_effect_buffer)).
record_se:- (t_l:use_side_effect_buffer ; t_l:verify_side_effect_buffer).


add_side_effect(_,_):- ( \+  record_se ),!.
add_side_effect(Op,Data):-current_why(Why),assert(t_l:side_effect_buffer(Op,Data,Why)).

attvar_op(Op,Data):- add_side_effect(Op,Data),
   must(nonvar(Op)),
   unnumbervars_and_save(Data,Data0),
   physical_side_effect(call(Op,Data0)).

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
get_source_ref1(_):- check_context_module,fail.
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

to_addable_form_wte(Why,I,O):-nonvar(O),!,to_addable_form_wte(Why,I,M),!,mustvv(M=O).
to_addable_form_wte(Why,I,O):-string(I),must_det_l((input_to_forms(string(I),Wff,Vs),b_setval('$variable_names',Vs),!,sexpr_sterm_to_pterm(Wff,PTerm),
  to_addable_form_wte(Why,PTerm,O))).
to_addable_form_wte(Why,I,O):-atom(I),atom_contains(I,'('),must_det_l((input_to_forms(atom(I),Wff,Vs),b_setval('$variable_names',Vs),!,sexpr_sterm_to_pterm(Wff,PTerm),
  to_addable_form_wte(Why,PTerm,O))).

to_addable_form_wte(_,X,X):-mreq(as_is_term(X)),!.
to_addable_form_wte(Why,nesc(I),O):-!,to_addable_form_wte(Why,I,O).
to_addable_form_wte(Why,USER:I,O):-USER==user,!,to_addable_form_wte(Why,I,O).
to_addable_form_wte(_Why,~(USER:P0),~(P0)):-USER==user,!.
to_addable_form_wte(_Why,~(P0),~(P0)):-!.
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


to_addable_form(I,I):- is_ftVar(I),!.
to_addable_form(I,OOO):-is_list(I),!,must_maplist(to_addable_form,I,O),flatten(O,OO),!,must(reduce_clause_from_fwd(OO,OOO)).
to_addable_form(I,OO):- current_predicate(_:mpred_term_expansion_file/0),must(fully_expand(pfc,I,II)),!,
 must((into_mpred_form(II,M),to_predicate_isas_each(M,O))),!,reduce_clause_from_fwd(O,OO).
to_addable_form(I,O):- must((bagof(M,do_expand_args(isEachAF,I,M),IM))),list_to_conjuncts(IM,M),to_predicate_isas_each(M,O),!.


% I =((P,Q)==>(p(P),q(Q))) , findall(O,baseKB:do_expand_args(isEachAF,I,O),L).


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
exact_args(Q):- req(argsQuoted(Q)).
exact_args(Q):-is_ftCompound(Q),functor(Q,F,_),req(argsQuoted(F)).
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
exact_args(ain(_)).
exact_args(dynamic(_)).
exact_args(cwc).
exact_args(true).
% exact_args(C):-source_file(C,I),absolute_source_location_pfc(I).

mpred_is_tautology(Var):-is_ftVar(Var).
mpred_is_tautology(V):- \+ \+ call((copy_term_nat(V,VC),numbervars(VC),show_success(why,mpred_is_taut(VC)))).

mpred_is_taut(A:-B):-!,mpred_is_taut(B==>A).
mpred_is_taut(A<-B):-!,mpred_is_taut(B==>A).
mpred_is_taut(A<==>B):-!,(mpred_is_taut(A==>B);mpred_is_taut(B==>A)).
mpred_is_taut(A==>B):- A==B,!.
mpred_is_taut((B,_)==>A):- mpred_is_assertable(B),mpred_is_taut(A==>B),!.
mpred_is_taut((_,B)==>A):- mpred_is_assertable(B),mpred_is_taut(A==>B),!.
mpred_is_taut(B==>(A,_)):- mpred_is_assertable(A),mpred_is_taut(A==>B),!.
mpred_is_taut(B==>(_,A)):- mpred_is_assertable(A),mpred_is_taut(A==>B),!.

loop_check_nr(CL):- loop_check(no_repeats(CL)).

% lmconf:decl_database_hook(Op,Hook):- loop_check_nr(pfc_provide_storage_op(Op,Hook)).

is_retract_first(one).
is_retract_first(a).

pfc_provide_storage_op(Op,(I1,I2)):-!,pfc_provide_storage_op(Op,I1),pfc_provide_storage_op(Op,I2).
pfc_provide_storage_op(Op,(nesc(P))):-!,pfc_provide_storage_op(Op,P).
%pfc_provide_storage_op(change(assert,_AorZ),Fact):- loop_check_nr(ainPreTermExpansion(Fact)).
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

get_why(_,CL,R,asserted(R,CL)):- clause(basePFC:spft(ukb,CL, U, U, _Why),true),!.
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


mpred_rule_hb(Outcome,OutcomeO,AnteO):-hotrace((mpred_rule_hb_0(Outcome,OutcomeO,Ante),mpred_rule_hb_0(Ante,AnteO,_))),!.
:-mpred_trace_nochilds(mpred_rule_hb/3).

mpred_rule_hb_0(Outcome,OutcomeO,true):-is_ftVar(Outcome),!,OutcomeO=Outcome.
mpred_rule_hb_0(Outcome,OutcomeO,true):- \+compound(Outcome),!,OutcomeO=Outcome.
mpred_rule_hb_0((Outcome1,Outcome2),OutcomeO,AnteO):-!,mpred_rule_hb(Outcome1,Outcome1O,Ante1),mpred_rule_hb(Outcome2,Outcome2O,Ante2),
                   conjoin(Outcome1O,Outcome2O,OutcomeO),
                   conjoin(Ante1,Ante2,AnteO).
mpred_rule_hb_0((Ante1==>Outcome),OutcomeO,(Ante1,Ante2)):-!,mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0((Outcome<-Ante1),OutcomeO,(Ante1,Ante2)):-!,mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0((Outcome<==>Ante1),OutcomeO,(Ante1,Ante2)):-mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0((Ante1<==>Outcome),OutcomeO,(Ante1,Ante2)):-!,mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(_::::Outcome,OutcomeO,Ante2):-!,mpred_rule_hb_0(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(basePFC:bt('$ABOX',Outcome,Ante1),OutcomeO,(Ante1,Ante2)):-!,mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(basePFC:pt('$ABOX',Ante1,Outcome),OutcomeO,(Ante1,Ante2)):-!,mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(basePFC:pk('$ABOX',Ante1a,Ante1b,Outcome),OutcomeO,(Ante1a,Ante1b,Ante2)):-!,mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(basePFC:nt('$ABOX',Ante1a,Ante1b,Outcome),OutcomeO,(Ante1a,Ante1b,Ante2)):-!,mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(basePFC:spft(ukb,Outcome,Ante1a,Ante1b,_),OutcomeO,(Ante1a,Ante1b,Ante2)):-!,mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0(basePFC:qu('$ABOX',Outcome,_),OutcomeO,Ante2):-!,mpred_rule_hb(Outcome,OutcomeO,Ante2).
% mpred_rule_hb_0(pfc Default(Outcome),OutcomeO,Ante2):-!,mpred_rule_hb(Outcome,OutcomeO,Ante2).
mpred_rule_hb_0((Outcome:-Ante),Outcome,Ante):-!.
mpred_rule_hb_0(Outcome,Outcome,true).

ain_minfo(G):-ain_minfo(assertz_if_new,G).
ain_minfo(How,(H:-True)):-is_true(True),must(is_ftNonvar(H)),!,ain_minfo(How,H).
ain_minfo(How,(H<-B)):- !,ain_minfo(How,(H:-infoF(H<-B))),!,ain_minfo(How,(H:-mpred_bc_only(H))),ain_minfo_2(How,(B:-infoF(H<-B))).
ain_minfo(How,(B==>H)):- !,ain_minfo(How,(H:-infoF(B==>H))),!,ain_minfo_2(How,(B:-infoF(B==>H))).
ain_minfo(How,(B<==>H)):- !,ain_minfo(How,(H:-infoF(B<==>H))),!,ain_minfo(How,(B:-infoF(B<==>H))),!.
ain_minfo(How,((A,B):-INFOC)):-mpred_is_info(INFOC),(is_ftNonvar(A);is_ftNonvar(B)),!,ain_minfo(How,((A):-INFOC)),ain_minfo(How,((B):-INFOC)),!.
ain_minfo(How,((A;B):-INFOC)):-mpred_is_info(INFOC),(is_ftNonvar(A);is_ftNonvar(B)),!,ain_minfo(How,((A):-INFOC)),ain_minfo(How,((B):-INFOC)),!.
ain_minfo(How,(-(A):-infoF(C))):-is_ftNonvar(C),is_ftNonvar(A),!,ain_minfo(How,((A):-infoF((C)))). % attvar_op(How,(-(A):-infoF(C))).
ain_minfo(How,(~(A):-infoF(C))):-is_ftNonvar(C),is_ftNonvar(A),!,ain_minfo(How,((A):-infoF((C)))). % attvar_op(How,(-(A):-infoF(C))).
ain_minfo(How,(A:-INFOC)):-is_ftNonvar(INFOC),INFOC= mpred_bc_only(A),!,attvar_op(How,(A:-INFOC)),!.
ain_minfo(How,basePFC:bt('$ABOX',H,_)):-!,attvar_op(How,(H:-mpred_bc_only(H))).
ain_minfo(How,basePFC:nt('$ABOX',H,Test,Body)):-!,attvar_op(How,(H:-fail,basePFC:nt('$ABOX',H,Test,Body))).
ain_minfo(How,basePFC:pt('$ABOX',H,Body)):-!,attvar_op(How,(H:-fail,basePFC:pt('$ABOX',H,Body))).
ain_minfo(How,(A0:-INFOC0)):- mpred_is_info(INFOC0), copy_term_and_varnames((A0:-INFOC0),(A:-INFOC)),!,must((mpred_rewrap_h(A,AA),imploded_copyvars((AA:-INFOC),ALLINFO), attvar_op(How,(ALLINFO)))),!.
%ain_minfo(How,G):-mpred_trace_msg(skipped_add_meta_facts(How,G)).
ain_minfo(_,_).

:- was_export(ain_minfo_2/2).
ain_minfo_2(How,G):-ain_minfo(How,G).

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
% show_if_debug(A):- !,show_call(why,A).
show_if_debug(A):- mpred_is_tracing(A) -> show_call(mpred_is_tracing,A) ; A.

% ======================= 
% user''s program''s database
% ======================= 
% assert_u(arity(prologHybrid,0)):-trace_or_throw(assert_u(arity(prologHybrid,0))).
% assert_u(X):- \+ (is_ftCompound(X)),!,asserta_u(X,X,0).

assert_u(M:X):- !,functor(X,F,A),assert_u(M,X,F,A).
assert_u(X):- functor(X,F,A),assert_u(abox,X,F,A).

assert_u(_M,X,F,_):-mreq(singleValuedInArg(F,SV)),!,must(update_single_valued_arg(X,SV)),!.
assert_u(_M,X,F,A):-mreq(prologSingleValued(F)),!,must(update_single_valued_arg(X,A)),!.
% assert_u(M,X,F,A):-must(isa(F,prologAssertAOrdered) -> asserta_u(M,X) ; assertz_u(M,X)).
% assert_u(M,X,F,A):-must(isa(F,prologOrdered)        -> assertz_u(M,X) ; asserta_u(M,X)).
assert_u(M,X,_,_):- assertz_mu(M,X).



check_never_assert(X):- ignore(( copy_term_and_varnames(X,Y),req(never_assert_u(Y,Why)),X=@=Y,snumbervars(X),trace_or_throw(never_assert_u(X,Why)))).
check_never_retract(X):- ignore(( copy_term_and_varnames(X,Y),req(never_retract_u(Y,Why)),X=@=Y,snumbervars(X),trace_or_throw(never_retract_u(X,Why)))).


assertz_u(M:X):-!,assertz_mu(M,X).
assertz_u(X):- assertz_mu(abox,X).

assertz_mu(M,X):- correct_module(M,X,T),T\==M,!,assertz_mu(T,X).
assertz_mu(M,X):- check_never_assert(M:X), clause_asserted(M:X),!.
assertz_mu(M,X):- must((expire_tabled_list(M:X),show_call(attvar_op(assertz,M:X)))).


retract_u(X):- check_never_retract(X),fail.
%retract_u(~(X)):-must(is_ftNonvar(X)),!,retract_eq_quitely_f(~(X)),must((expire_tabled_list(~(X)))),must((expire_tabled_list((X)))).
%retract_u(basePFC:hs(X)):-!,retract_eq_quitely_f(basePFC:hs(X)),must((expire_tabled_list(~(X)))),must((expire_tabled_list((X)))).
retract_u(basePFC:qu('$ABOX',X,Y)):-!,show_failure(why,retract_eq_quitely_f(basePFC:qu('$ABOX',X,Y))),must((expire_tabled_list(~(X)))),must((expire_tabled_list((X)))).
retract_u(~(X)):-!,show_success(why,retract_eq_quitely_f(~(X))),must((expire_tabled_list(~(X)))),must((expire_tabled_list((X)))).
retract_u((X)):-!,show_success(why,retract_eq_quitely_f((X))),must((expire_tabled_list(~(X)))),must((expire_tabled_list((X)))).
retract_u(X):-show_if_debug(attvar_op(retract_eq,X)),!,must((expire_tabled_list(X))).

retractall_u(X):-retractall(X),must((expire_tabled_list(X))).
clause_u(H,B):- must(H\==true),catchv(clause(H,B),_,fail).
clause_u(H,B,Ref):-must(H\==true),catchv(clause(H,B,Ref),_,fail).

mpred_update_literal(P,N,Q,R):-
    arg(N,P,UPDATE),call(replace_arg(P,N,OLD,Q)),
    must(Q),update_value(OLD,UPDATE,NEW), 
    call(replace_arg(Q,N,NEW,R)).

update_single_valued_arg(P,N):-
 must_det_l((
  get_source_ref((U,U)),
  arg(N,P,UPDATE),
  replace_arg(P,N,OLD,Q),
  current_why(Why),
  get_user_abox(M), 
  M:get_source_ref1(U),
  must_det_l((
     attvar_op(assert_if_new,
     basePFC:spft(ukb,P,U,U,Why)),
     (req(P)->true;(assertz_u(P))),
     doall((
          clause(Q,true,E),
          UPDATE \== OLD,
          erase_w_attvars(clause(Q,true,E),E),
          mpred_unfwc1(Q))))))).



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
call_prologsys(X):-with_umt(X).

% ======================= 
% internal bookkeeping
% ======================= 
assert_i(X):- check_never_assert(X), attvar_op(assert_if_new,X).
asserta_i(X):- check_never_assert(X), attvar_op(asserta_if_new,X).
assertz_i(X):- check_never_assert(X), attvar_op(assertz_if_new,X).
retract_i(X):- check_never_retract(X), attvar_op(retract,X).
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



%example pfcVerifyMissing(mpred_isa(I,D), mpred_isa(I,C), ((mpred_isa(I,C), {D==C});-mpred_isa(I,C))). 
%example pfcVerifyMissing(mudColor(I,D), mudColor(I,C), ((mudColor(I,C), {D==C});-mudColor(I,C))). 

pfcVerifyMissing(GC, GO, ((GO, {D==C});\+ GO) ):-  GC=..[F,A|Args],append(Left,[D],Args),append(Left,[C],NewArgs),GO=..[F,A|NewArgs],!.

%example mpred_freeLastArg(mpred_isa(I,C),~(mpred_isa(I,C))):-is_ftNonvar(C),!.
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

%= basePFC:tms is one of {none,local,cycles} and controles the tms alg.
lmconf:module_local_init:- must((mpred_init_i(basePFC:tms(_), basePFC:tms(cycles)))).

% Pfc Search strategy. basePFC:sm(X) where X is one of {direct,depth,breadth}
lmconf:module_local_init:- mpred_init_i(basePFC:sm(_), basePFC:sm(direct)).

% aliases

mpred_ain(G):-pfc_add(G).
mpred_ainz(G):-pfc_add(G).
mpred_aina(G):-pfc_add(G).

mpred_ain(G,S):- ain(G,S).
mpred_ainz(G,S):-ain(G,S).
mpred_aina(G,S):-ain(G,S).

%= ain/2 and mpred_post/2 are the main ways to assert_db new clauses into the
%= database and have forward reasoning done.

%= ain(P,S) asserts P into the user''s dataBase with support from S.
pfc_add(P) :- 
  ain_fast(P).

ain(P,S) :- 
  ain_fast(P,S).


ain_fast('$si$':'$was_imported_kb_content$'(_, _)<-THIS):-is_ftNonvar(THIS),!.
ain_fast(P0):-
  must(get_source_ref(S)), ain_fast(P0,S).


ain_fast(nesc(P),S) :- is_ftNonvar(P),!,ain_fast(P,S).
ain_fast(P0,S):- gripe_time(0.6,ain_fast_timed(P0,S)).

ain_fast_timed(P0,S):- '$module'(user,user),'$set_source_module'(user,user),!,
  '$module'(WM,baseKB),'$set_source_module'(WS,baseKB),
   call_cleanup(ain_fast_timed(P0,S),('$module'(_,WM),'$set_source_module'(_,WS))).

ain_fast_timed(P0,S):- check_context_module,
  must(to_addable_form_wte(assert,P0,P)),
      (is_list(P)
        ->must_maplist(ain_fast_sp(S),P);
       ain_fast_sp(S,P)).


% a really common example is people want unbound predicate backchaining .. that is to query the predicates witha  varaible where the predciate is 
ain_fast_sp(S,P):- ensure_vars_labled(P,P0),fully_expand(change(assert,add),P0,P1),ain_fast_sp0(S,P1).

  

% ain_fast_sp(S,P->Q) :-!,ain_fast_sp(S,P==>Q).
ain_fast_sp0(S,P) :-
   mpred_rule_hb(P,OutcomeO,_),!,
     loop_check_term((mpred_post_sp_zzz(S,P),mpred_run_maybe),
     aining(OutcomeO),
     (mpred_post_sp_zzz(S,P),mpred_trace_msg(looped_outcome((P))))),!.
%ain_fast_sp(_,_).
ain_fast_sp0(P,S) :- mpred_error("ain_fast(~p,~p) failed",[P,S]).

:-module_transparent(mpred_ain/1).
:-module_transparent(mpred_aina/1).
:-module_transparent(mpred_ainz/1).
:-module_transparent(logicmoo_util_database:ain/1).
:-module_transparent(logicmoo_util_database:aina/1).
:-module_transparent(logicmoo_util_database:ainz/1).
:-multifile(logicmoo_util_database:ain/1).
:-multifile(logicmoo_util_database:aina/1).
:-multifile(logicmoo_util_database:ainz/1).
:-asserta((logicmoo_util_database:ainz(G):- !, with_umt(mpred_ainz(G)))).
:-asserta((logicmoo_util_database:ain(G):- !, with_umt(mpred_ain(G)))).
:-asserta((logicmoo_util_database:aina(G):- !, with_umt(mpred_aina(G)))).

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
mpred_post1(ain(P0),S):- must(is_ftNonvar(P0)), !,ain(P0,S).
mpred_post1(P0,S):-
  to_addable_form_wte(assert,P0,P),
      (is_list(P)
        ->maplist(mpred_post_sp_zzz(S),P);
       mpred_post_sp_zzz(S,P)).

mpred_post_sp(S,P):- mpred_post_sp_zzz(S,P).


mpred_post_sp_zzz(S,P):- ground(S:P),!,mpred_post_sp_zzzz(S,P),!.
mpred_post_sp_zzz(S,P):- \+ is_main_thread,!,
   (hotrace(ensure_vars_labled(S:P,S0:P0))-> 
     mpred_post_sp_zzzz(S0,P0);mpred_post_sp_zzzz(S,P)),!.

mpred_post_sp_zzz(S,P):-  is_main_thread,!,
   (hotrace(ensure_vars_labled(S:P,S0:P0))-> 
     mpred_post_sp_zzzz(S0,P0);mpred_post_sp_zzzz(S,P)),!.

mpred_post_sp_zzz(S,P):- 
    hotrace(dcall_when(ensure_vars_labled,S:P,S0:P0)),!,
    mpred_post_sp_zzzz(S0,P0),!.

mpred_post_sp_zzz(S,P):-mpred_post_sp_zzzz(S,P),!.

mpred_post_sp_zzzz(S,(P1,P2)) :- !,mpred_post_sp_zzzz(S,(P1)),mpred_post_sp_zzzz(S,(P2)).
mpred_post_sp_zzzz(S,[P1]) :- !,mpred_post_sp_zzzz(S,(P1)).
mpred_post_sp_zzzz(S,[P1|P2]) :- !,mpred_post_sp_zzzz(S,(P1)),mpred_post_sp_zzzz(S,(P2)).
mpred_post_sp_zzzz(S, \+ P) :-!,doall(mpred_rem2a(P,S)),!,mpred_undo((\+),P).
mpred_post_sp_zzzz(S, -(P)) :-!,mpred_post_sp_zzzz(S, ~( P)).
mpred_post_sp_zzzz(S, not(P)) :-!,mpred_post_sp_zzzz(S, ~( P)).
mpred_post_sp_zzzz(S, ~(P)) :-doall(mpred_rem2a(P,S)),mpred_undo((\+),P),fail.
mpred_post_sp_zzzz(_S,P) :- once((notrace(mpred_is_tautology(P)),wdmsg(trace_or_throw(todo(error(mpred_is_tautology(P))))))),show_load_context,fail.

% only do loop check if it's already supported

mpred_post_sp_zzzz(S,P) :- is_ftCompound(P), arg(SV,P,V),is_relative(V),must((mpred_update_literal(P,SV,Q,R),mpred_post_sp_zzzz(S,R))),(Q=R->true;mpred_undo(update,Q)).
mpred_post_sp_zzzz(S,P) :- is_already_supported(P,S,_How),must(loop_check(mpred_post1_sp_0(S,P),mpred_post1_sp_1(S,P))),!. % ,mpred_enqueue(P,S).

mpred_post_sp_zzzz(S,~(P)) :-!, mpred_post1_sp_0(S,~(P)),mpred_run,!,assert_u(~(P)).

mpred_post_sp_zzzz(S,P) :- mpred_post1_sp_0(S,P).

mpred_post1_sp_0(S,P) :-
  %= db ain_db_to_head(P,P2),
  % mpred_remove_old_version(P),
  must(once(ain_support(P,S))),
  mpred_post1_sp_1(S,P).

mpred_post1_sp_1(S,P):- P\==true,
  mpred_unique_u(P),
  must(import_to_user(P)),
  must(assert_u(P)),!,
  must(mpred_trace_add(P,S)),
  !,
  must(mpred_enqueue(P,S)),
  !.

mpred_post1_sp_1(_,_). % already added
mpred_post1_sp_1(S,P) :-  mpred_warn("mpred_post1(~p,~p) failed",[P,S]).

with_mpred_trace_exec(P):- w_tl(t_l:mpred_debug_local,w_tl(mpred_is_tracing_exec, must(show_if_debug(P)))).

mpred_test(P):- (mpred_is_silient;compiling),!,sanity(req(P)),!.
mpred_test(P):- show_call(with_mpred_trace_exec(req(P))),!.

clause_asserted_local(basePFC:spft(ukb,P,Fact,Trigger,UOldWhy)):-
  clause(basePFC:spft(ukb,P,Fact,Trigger,_OldWhy),true,Ref),
  clause(basePFC:spft(ukb,UP,UFact,UTrigger,UOldWhy),true,Ref),
  (((UP=@=P,UFact=@=Fact,UTrigger=@=Trigger))).


is_already_supported(P,(S,T),(S,T)):- clause_asserted_local(basePFC:spft(ukb,P,S,T,_)),!.
is_already_supported(P,_S,UU):- clause_asserted_local(basePFC:spft(ukb,P,US,UT,_)),must(get_source_ref(UU)),UU=(US,UT).

% TOO UNSAFE 
% is_already_supported(P,_S):- copy_term_and_varnames(P,PC),sp ftY(PC,_,_),P=@=PC,!.


if_missing_mask(Q,R,Test):-
   which_missing_argnum(Q,N),
   if_missing_mask(Q,N,R,Test).

if_missing_mask(Q,N,R,Test):-
  arg(N,Q,Was),
  (nonvar(R)-> (which_missing_argnum(R,RN),arg(RN,R,NEW));replace_arg(Q,N,NEW,R)),!,
   Test=dif:dif(Was,NEW).

/*
Old version
if_missing_mask(Q,N,R,dif:dif(Was,NEW)):- 
 must((is_ftNonvar(Q),acyclic_term(Q),acyclic_term(R),functor(Q,F,A),functor(R,F,A))),
  (singleValuedInArg(F,N) -> 
    (arg(N,Q,Was),replace_arg(Q,N,NEW,R));
    ((arg(N,Q,Was),is_ftNonvar(Was)) -> replace_arg(Q,N,NEW,R);
        (N=A,arg(N,Q,Was),replace_arg(Q,N,NEW,R)))).
*/

which_missing_argnum(Q,N):-
 must((acyclic_term(Q),is_ftCompound(Q),get_functor(Q,F,A))),
 F\=t,
  (singleValuedInArg(F,N) -> true;
    ((arg(N,Q,Was),is_ftNonvar(Was)) -> true; N=A)).



% was nothing  mpred_current_db/1.
mpred_current_db(U):-get_source_ref1(U).
mpred_current.


%=
%= ain_db_to_head(+P,-NewP) talkes a fact P or a conditioned fact
%= (P:-C) and adds the Db context.
%=

ain_db_to_head(P,NewP) :-
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
  must(mreq(basePFC:sm(Mode));Mode=direct)
    -> (Mode=direct  -> must(mpred_fwd(P,S)) ;
	Mode=depth   -> aina_i(basePFC:qu('$ABOX',P,S),S) ;
	Mode=breadth -> ainz_i(basePFC:qu('$ABOX',P,S),S) ;
	% else
          otherwise           -> mpred_warn("Unrecognized basePFC:sm mode: ~p", Mode))
     ; mpred_warn("No basePFC:sm mode").


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
%    depth or breadth - use the basePFC:qu mechanism.

mpred_run_maybe:-!, mpred_run.
mpred_run_maybe :- (X is random(5)),X<4,!.
mpred_run_maybe :-
  (\+ basePFC:sm(direct)),
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


% mpred_step removes one entry from the basePFC:qu and reasons from it.

mpred_step :-
  % if basePFC:hs(Signal) is true, reset it and fail, thereby stopping inferencing.
  basePFC:hs(Signal),!,
  mpred_retract_db_type(basePFC:hs(Signal)),
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
  mpred_retract_db_type(basePFC:qu('$ABOX',P,S)),
  mpred_remove_supports_quietly(basePFC:qu('$ABOX',P,S)),
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
defaultmpred_select(P,S) :- basePFC:qu('$ABOX',P,S),!.

:- shared_multifile(basePFC:hs/1).

% mpred_halt stops the forward chaining.
mpred_halt :-  mpred_halt("",[]).

mpred_halt(Format) :- mpred_halt(Format,[]).

mpred_halt(Format,Args) :-
  sformat(S,Format,Args),
  !,
  in_cmt((wdmsg('-s',[S]))),
  (basePFC:hs(Signal) ->
       mpred_warn("mpred_halt finds basePFC:hs(Signal) already set to ~p",[Signal])
     ; assert_i(basePFC:hs(S))).


%=
%=
%= predicates for manipulating triggers
%=
ain_trigger(Trig,Support) :- arg(1,Trig,Term),
   copy_term_and_varnames(Trig,Int), 
   loop_check_term(ain_trigger_0(Int,Trig,Support),trig(Term),true).


ain_trigger_0(_Trig,TriggerBody,Support) :- mpred_had_support(TriggerBody,Support),!,mpred_trace_msg('Had Support',TriggerBody).

ain_trigger_0(Trig,basePFC:pt('$ABOX',Trigger,Body),Support) :-
  !,  
  mpred_trace_msg('Adding For Later',basePFC:pt('$ABOX',Trigger,Body)),
  (clause_asserted(basePFC:pt('$ABOX',Trigger,Body)) -> ! ;   
     (( ain_t(basePFC:pt('$ABOX',Trigger,Body),Support),
        (must(mpred_mark_as(Support,p,Trigger,pfcPosTrigger))),
        add_reprop(Trig,Trigger)))).

ain_trigger_0(_Trig,basePFC:nt('$ABOX',Trigger,Test,Body),Support) :- !,
  mpred_trace_msg('Adding For Later',basePFC:nt('$ABOX',Trigger,Test,Body)),
   must(mpred_mark_as(Support,n,Trigger,pfcNegTrigger)),
        copy_term_and_varnames(Trigger,TriggerCopy),!,
        ain_t(basePFC:nt('$ABOX',TriggerCopy,Test,Body),Support),
          (not_cond(basePFC:nt,Test)),           
             mpred_eval_lhs(Body,
             ((\+Trigger),basePFC:nt('$ABOX',TriggerCopy,Test,Body))).

ain_trigger_0(_Trig,basePFC:bt('$ABOX',Trigger,Body),Support) :- 
 must((
  mpred_trace_msg('Adding For Later',basePFC:bt('$ABOX',Trigger,Body)),
   must(ain_t(basePFC:bt('$ABOX',Trigger,Body),Support)),
      attvar_op(assertz_if_new,((Trigger:-mpred_bc_only(Trigger)))),!,
      import_to_user(Trigger),
      must(mpred_mark_as(Support,p,Trigger,pfcBcTrigger)),
     % WAS mpred_bt_pt_combine(Trigger,Body).
   mpred_bt_pt_combine(Trigger,Body,Support))),!.

ain_trigger_0(Trig,X,Support) :- mpred_warn("Unrecognized trigger to aintrigger: ~p for ~p",[ain_trigger(X,Support),Trig]).


mpred_bt_pt_combine(Head,Body,Support) :-
  %= a backward trigger (basePFC:bt) was just added with head and Body and support Support
  %= find any basePFC:pt''s with unifying heads and assert the instantied basePFC:bt body.
  mpred_get_trigger_quick(basePFC:pt('$ABOX',Head,_PtBody)),
  mpred_eval_lhs(Body,Support),
  fail.
mpred_bt_pt_combine(_,_,_) :- !.


mpred_get_trigger_quick(Trigger) :- !, mreq(Trigger).
mpred_get_trigger_quick(Trigger) :- clause_i(Trigger,true)*->true;clause(basePFC:spft(ukb,Trigger,_,_,_),true).

%=
%=
%= predicates for manipulating action traces.
%=

ain_actiontrace(Action,Support) :-
  % adds an action trace and it''s support.
  ain_support(mpred_action(Action),Support).

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


mpred_retract_db_type(_,basePFC:qu('$ABOX',P,S)) :-
  doall(retract_u(basePFC:qu('$ABOX',P,_))),
  ignore(mpred_unfwc(basePFC:qu('$ABOX',P,S))).


mpred_retract_db_type(fact,X) :-
  %= db ain_db_to_head(X,X2), retract(X2).
  retract_u(X),
  ignore(mpred_unfwc(X)).

mpred_retract_db_type(rule,X) :-
  %= db  ain_db_to_head(X,X2),  retract(X2).
  retract_u(X).

mpred_retract_db_type(trigger,X) :-
  retract_t(X)
    -> mpred_unfwc(X)
     ; mpred_warn("Trigger not found to mpred_retract_db_type: ~p",[X]).

mpred_retract_db_type(action,X) :- mpred_rem_actiontrace(mpred_retract_db_type,X).


/* UNUSED TODAY
%= ain_db_type(X) adds item X to some database
%= was simply:  mpred_Add
ain_db_type(X) :-
  % what type of X do we have?
  mpred_db_type(X,Type),
  % call the appropriate predicate.
  ain_db_type(Type,X).

ain_db_type(fact,X) :-
  mpred_unique_u(X),
  import_to_user(X),
  must(assert_u(X)),!.
ain_db_type(rule,X) :-
  mpred_unique_i(X),
  assert_i(X),!.
ain_db_type(trigger,X) :-
  assert_t(X).
ain_db_type(action,_Action) :- !.
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

mpred_rem1(P,S) :- copy_term_and_varnames(mpred_rem1(P,S),Why),   
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

mpred_undo(Why,basePFC:pk('$ABOX',Key,Head,Body)) :-
  % undo a positive trigger.
  %
  !,
  (retract_i(basePFC:pk('$ABOX',Key,Head,Body))
    -> mpred_unfwc(basePFC:pt('$ABOX',Head,Body))
     ; mpred_warn("for ~p \nTrigger not found to retract basePFC:pk= ~p: ~p",[Why,Key,basePFC:pt('$ABOX',Head,Body)])).

mpred_undo(Why,basePFC:pt('$ABOX',Head,Body)) :- 
  % undo a positive trigger.
  %
  !,
  (retract_i(basePFC:pt('$ABOX',Head,Body))
    -> mpred_unfwc(basePFC:pt('$ABOX',Head,Body))
     ; mpred_warn("for ~p:\nTrigger not found to retract: ~p",[Why,basePFC:pt('$ABOX',Head,Body)])).


mpred_undo(Why,basePFC:bt('$ABOX',Head,Body)) :- 
  % undo a backchaining trigger.
  %
  !,
  dtrace(attvar_op(retractall,(Head:-mpred_bc_only(Head)))),
  (retract_i(basePFC:bt('$ABOX',Head,Body))
    -> mpred_unfwc(basePFC:bt('$ABOX',Head,Body))
     ; mpred_warn("for ~p:\nTrigger not found to retract: ~p",[Why,basePFC:bt('$ABOX',Head,Body)])).


mpred_undo(Why,basePFC:nt('$ABOX',Head,Condition,Body)) :-
  % undo a negative trigger.
  !,
  (retract_i(basePFC:nt('$ABOX',Head,Condition,Body))
    -> mpred_unfwc(basePFC:nt('$ABOX',Head,Condition,Body))
     ; mpred_trace_msg("for ~p:\nTrigger not found to retract: ~p",[Why,basePFC:nt('$ABOX',Head,Condition,Body)])).

mpred_undo(Why,( \+ ~Fact)):- mpred_undo(Why, Fact),fail.
mpred_undo(Why,   ~(~Fact)):- mpred_undo(Why, Fact),fail.

mpred_undo(Why,Fact):- mpred_undo_u(Why,Fact)*->true;mpred_undo_e(Why,Fact).

mpred_undo_u(Why,Fact) :-
  % undo a random fact, printing out the trace, if relevant.
  retract_u(Fact),
     must(mpred_trace_rem(Why,Fact)),
     mpred_unfwc1(Fact).

mpred_undo_e(Why,Fact) :- 
    % (Fact\= ~(_)->cnotrace(mpred_trace_msg("mpred_undo_e ; Fact not found in user db: ~p",[Fact]));true),
     (Fact\= ~(_)->mpred_trace_rem(Why,Fact);true),
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
  copy_term_and_varnames(F,Fcopy),
  mpred_get_trigger_quick(basePFC:nt('$ABOX',Fcopy,Condition,Action)),
  (not_cond(basePFC:nt,Condition)),
  G = mpred_eval_lhs(Action,((\+F),basePFC:nt('$ABOX',F,Condition,Action))),
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
remove_if_unsupported(Why,P) :- ((\+ ground(P), P \= (_:-_) , P \= ~(_) ) -> mpred_trace_msg(warn(nonground_remove_if_unsupported(Why,P))) ;true),
   (((mpred_tms_supported(local,P,How),How\=unknown(_)) -> mpred_trace_msg(still_supported(How,Why,local,P)) ; (  mpred_undo(Why,P)))),
   mpred_run.


%= mpred_tms_supported(+P,-How) succeeds if P is "supported". What "How" means
%= depends on the TMS mode selected.

mpred_tms_supported(P,How) :-
  basePFC:tms(Mode),
  mpred_tms_supported0(Mode,P,How).


mpred_tms_supported(Mode,P,How) :- is_ftVar(Mode),basePFC:tms(Mode),!,mpred_tms_supported0(Mode,P,How).
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
mpred_deep_support0(basePFC:pt('$ABOX',HowA,HowB),basePFC:pt('$ABOX',A,B)):-!,mpred_deep_support(HowA,A),mpred_deep_support(HowB,B).
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
  % first make sure we aren''t in a loop.
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


mpred_get_support_via_clause_db(\+ P,OUT):- mpred_get_support_via_clause_db(~(P),OUT).
mpred_get_support_via_clause_db(\+ P,(naf(g),g)):- !, predicate_property(P,number_of_clauses(_)),\+ clause(P,_Body).
mpred_get_support_via_clause_db(P,OUT):- predicate_property(P,number_of_clauses(N)),N>0,
   clause(P,Body),(Body==true->Sup=(g);
    (support_ok_via_clause_body(P),mpred_get_support_precanonical_plus_more(Body,Sup))),
   OUT=(Sup,g).


support_ok_via_clause_body(_H):-!,fail.
support_ok_via_clause_body(H):- get_functor(H,F,A),support_ok_via_clause_body(H,F,A).

support_ok_via_clause_body(_,(\+),1):-!,fail.
support_ok_via_clause_body(_,F,_):- req(prologSideEffects(F)),!,fail.
support_ok_via_clause_body(H,_,_):- \+ predicate_property(H,number_of_clauses(_)),!,fail.
support_ok_via_clause_body(_,F,A):- req(mpred_mark(pfcRHS,_,F,A)),!,fail.
support_ok_via_clause_body(_,F,A):- req(mpred_mark(pfcMustFC,_,F,A)),!,fail.
support_ok_via_clause_body(_,F,_):- req(argsQuoted(F)),!,fail.
support_ok_via_clause_body(_,F,_):- req(prologDynamic(F)),!.
support_ok_via_clause_body(_,F,_):- \+ req(pfcControlled(F)),!.


mpred_get_support_precanonical(F,Sup):-to_addable_form_wte(mpred_get_support_precanonical,F,P),mpred_get_support(P,Sup).
spft_precanonical(F,SF,ST):-to_addable_form_wte(spft_precanonical,F,P),!,basePFC:spft(ukb,P,SF,ST,_).

trigger_supports_f_l(U,[]) :- match_source_ref1(U),!.

trigger_supports_f_l(Trigger,[Fact|MoreFacts]) :-
  mpred_get_support_precanonical_plus_more(Trigger,(Fact,AnotherTrigger)),
  trigger_supports_f_l(AnotherTrigger,MoreFacts).


%=
%=
%= mpred_fwd(X) forward chains from a fact or a list of facts X.
%=
% mpred_fwd(+P) forward chains for a multiple facts.

mpred_fwd(P):- get_source_ref(UU), mpred_fwd(P,UU).
mpred_fwd([H|T],S) :- !, mpred_fwd1(H,S), mpred_fwd(T,S).
mpred_fwd([],_) :- !.
mpred_fwd(P,S) :- mpred_fwd1(P,S),!.

%=
% mpred_fwd1(+P) forward chains for a single fact.
mpred_fwd1(Fact,Sup) :- gripe_time(0.80,mpred_fwd2(Fact,Sup)),!.

mpred_fwd2(Fact,Sup) :- cyclic_term(Fact;Sup),writeq(mpred_fwd2_cyclic_term(Fact;Sup)),!.
mpred_fwd2(Fact0,_Sup):-
  once(must(ain_rule_if_rule(Fact0))),
  unnumbervars(Fact0,Fact),
  copy_term(Fact,F),
  % check positive triggers
  once(must(fcpt(Fact,F))),
  % check negative triggers
  once(must(fcnt(Fact,F))).


%=
%= ain_rule_if_rule(P) does some special, built in forward chaining if P is
%= a rule.
%=

% ain_rule_if_rule(Fact) :- cyclic_break(Fact),is_mpred_action(Fact),(ground(Fact)->must(once(Fact));doall(show_if_debug(must(Fact)))),fail.
% ain_rule_if_rule(Fact) :- cyclic_break(Fact),is_mpred_action(Fact),(ground(Fact)->must(once(Fact));doall(show_if_debug(must(Fact)))),!.
ain_rule_if_rule(Fact) :- cyclic_break(Fact),is_mpred_action(Fact),
    doall(show_if_debug(with_umt(req(Fact)))),!.
ain_rule_if_rule(Fact):- must(ain_rule0(Fact)),!.

ain_rule0((P==>Q)) :-
  !,
  process_rule(P,Q,(P==>Q)).

ain_rule0((Name::::P==>Q)) :-
  !,
  process_rule(P,Q,(Name::::P==>Q)).

ain_rule0((P<==>Q)) :-
  !,
  process_rule(P,Q,(P<==>Q)),
  process_rule(Q,P,(P<==>Q)).

ain_rule0((Name::::P<==>Q)) :-
  !,
  process_rule(P,Q,((Name::::P<==>Q))),
  process_rule(Q,P,((Name::::P<==>Q))).

ain_rule0(('<-'(P,Q))) :-
  !,
  mpred_define_bc_rule(P,Q,('<-'(P,Q))).

ain_rule0(_).

fcpt(Fact,F):- fcpt0(Fact,F)*->fail;nop(mpred_trace_msg(no_pt(Fact,F))).
fcpt(_,_).

fcpt0(Fact,F) :- 
  mpred_get_trigger_quick(basePFC:pt('$ABOX',F,Body)),
  (mpred_eval_lhs(Body,(Fact,basePFC:pt('$ABOX',F,Body)))
    *-> mpred_trace_msg('Using Trigger',basePFC:pt('$ABOX',F,Body));
      (mpred_trace_msg('Skipped Trigger',basePFC:pt('$ABOX',F,Body)),fail)).
  

fcpt0(Fact,F) :- use_presently,
  mpred_get_trigger_quick(basePFC:pt('$ABOX',presently(F),Body)),
  pp_item('Found presently ',basePFC:pt('$ABOX',F,Body)),
  mpred_eval_lhs(Body,(presently(Fact),basePFC:pt('$ABOX',presently(F),Body))).

fcnt(Fact,F):- fcnt0(Fact,F)*->fail;nop(mpred_trace_msg(no_spft_nt(Fact,F))).
fcnt(_,_).

fcnt0(_Fact,F) :- 
  basePFC:spft(ukb,X,_,basePFC:nt('$ABOX',F,Condition,Body),_Why),
  (call_u(Condition) *-> 
   (mpred_trace_msg('Using Trigger'(X),basePFC:nt('$ABOX',F,Condition,Body)),
      mpred_rem1(X,(_,basePFC:nt('$ABOX',F,Condition,Body))),fail);
      (mpred_trace_msg('Unused Trigger'(X),basePFC:nt('$ABOX',F,Condition,Body)),fail)).



%=
%= mpred_define_bc_rule(+Head,+Body,+Parent_rule) - defines a backward
%= chaining rule and adds the corresponding basePFC:bt triggers to the database.
%=

mpred_define_bc_rule(Head,Body,Parent_rule) :-
  (\+ mpred_literal(Head)),
  mpred_warn("Malformed backward chaining rule.  ~p not atomic.",[(Head:-Body)]),
  mpred_warn("rule: ~p",[Parent_rule]),
 % !,
  dtrace(mpred_define_bc_rule(Head,Body,Parent_rule)),
  fail.

mpred_define_bc_rule(Head,Body,Parent_rule) :- 
  copy_term_and_varnames(Parent_rule,Parent_ruleCopy),
  attvar_op(assert_if_new,(Head:-mpred_bc_only(Head))),
  must(import_to_user(Head)),
  build_rhs(Head,Head,Rhs),
  foreachl_do(mpred_nf(Body,Lhs),
       (build_trigger(Parent_ruleCopy,Lhs,rhs(Rhs),Trigger),
       % can be mpred_post_sp(basePFC:bt('$ABOX',Head,Trigger),(Parent_ruleCopy,U))
         ((get_source_ref1(U),ain_fast(basePFC:bt('$ABOX',Head,Trigger),(Parent_ruleCopy,U)))))).
        

contains_ftVar(Term):- sub_term(Sub,Term),compound(Sub),Sub='$VAR'(_).
%=
%=
%= eval something on the LHS of a rule.
%=
%mpred_eval_lhs(P,S):- contains_ftVar(P),unnumbervars(mpred_eval_lhs(P,S),mpred_eval_lhs(P0,S0)),P\=@=P0,!,mpred_eval_lhs(P0,S0).
%mpred_eval_lhs(P,S):- contains_ftVar(P),trace_or_throw(contains_ftVar_mpred_eval_lhs(P,S)).
mpred_eval_lhs(P,S):-
  unnumbervars(mpred_eval_lhs(P,S),mpred_eval_lhs(P0,S0)),
    loop_check(mpred_eval_lhs0(P0,S0)).

mpred_eval_lhs0((Test->Body),Support) :-
  !,
  % (call_prologsys(Test) -> mpred_eval_lhs(Body,Support)),
   ((no_repeats(call_prologsys(Test)) , 
       (mpred_eval_lhs(Body,Support))) *-> true ; (!,fail)).

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
  on_x_rtrace(ain_trigger(X,Support)),
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
 ain(Assertion,Support),!.

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
     -> ain_actiontrace(Action,Support)
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
  copy_term_and_varnames(Trigger,TriggerCopy),
  no_repeats(call_u(Trigger)),
  mpred_eval_lhs(Body,(presently(Trigger),basePFC:pt('$ABOX',presently(TriggerCopy),Body))),
  fail.

trigger_trigger1(Trigger,Body) :-
  copy_term_and_varnames(Trigger,TriggerCopy),
  no_repeats(mpred_call_only_facts(Trigger)),
  mpred_eval_lhs(Body,(Trigger,basePFC:pt('$ABOX',TriggerCopy,Body))),
  fail.
*/

%=
%= call_u(Why,F) is true iff F is a fact is true
%=
call_u(X):- mpred_call_only_facts(X).
call_u(Why,X):- show_call(why,(nop(Why),mpred_call_only_facts(X))).

%=
%= not_cond(Why,F) is true iff F is a fact is not true
%=
% not_cond(_Why,X):- show_success(why,mpred_call_0(~(X))).
not_cond(_Why,X):- \+ X.


'{}'(G):-req(G).

:- meta_predicate neg_in_code(*).
:- export(neg_in_code/1).
neg_in_code(G):-var(G),!,fail.
neg_in_code(req(G)):- !,~G.
neg_in_code(~(G)):- nonvar(G),!, \+ ~G.
neg_in_code(G):-   neg_may_naf(G), \+ with_umt(G).
neg_in_code(G):-  is_ftNonvar(G), prologSingleValued(G),must((if_missing_mask(G,R,Test),nonvar(R))),req(R),with_umt(Test).


:- meta_predicate neg_may_naf(0).
:- export(neg_may_naf/1).
neg_may_naf(P):- mpred_non_neg_literal(P),get_functor(P,F),clause(prologNegByFailure(F),true),!.
neg_may_naf(P):- is_ftCompound(P),predicate_property(P,static).



%=
%= mpred_call_only_facts(+Why,:F) is true iff F is a fact available for forward chaining.
%= Note that this has the side effect [maybe] of catching unsupported facts and
%= assigning them support from God. (g,g)
%=
req(G):- loop_check(mpred_call_0(G),fail).

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
mpred_call_0(G):- strip_module(G,M,P),must(nonvar(P)),functor(P,F,_),mpred_call_1(M,P,F).


mpred_call_1(_,G,_):- is_side_effect_disabled,!,mpred_call_with_no_triggers(G).

mpred_call_1(M,G,F):-  (ground(G); \+ current_predicate(_,M:G) ; \+ (predicate_property(M:G,clause_count(CC)),CC>1)), (\+  is_side_effect_disabled),
                ignore((loop_check(call_with_bc_triggers(M:G)),maybeSupport(G,(g,g)),fail)),
                 \+ current_predicate(F,M:G),\+ current_predicate(_,_:G),
                 doall(show_call(predicate_property(_UM:G,_PP))),
                 debug(mpred),
                 must(show_call(kb_dynamic(M:G))),import_to_user(M:G),!,fail.
mpred_call_1(_,G,_):- mpred_call_with_no_triggers(G).


:- thread_local t_l:infBackChainPrevented/1.

call_with_bc_triggers(P) :- functor(P,F,A), \+ t_l:infBackChainPrevented(F/A), 
  mpred_get_trigger_quick(basePFC:bt('$ABOX',P,Trigger)),
  no_repeats(mpred_get_support(basePFC:bt('$ABOX',P,Trigger),S)),
  once(no_side_effects(P)),
  wno_tl(t_l:infBackChainPrevented(F/A),mpred_eval_lhs(Trigger,S)).

mpred_call_with_no_triggers(F) :- 
  %= this (is_ftVar(F)) is probably not advisable due to extreme inefficiency.
  (is_ftVar(F)    ->  mpred_facts_and_universe(F) ; mpred_call_with_no_triggers_bound(F)).

mpred_call_with_no_triggers_bound(F):- mpred_call_with_no_triggers_uncaugth(F).

mpred_call_with_no_triggers_uncaugth(F) :- 
  show_failure(mpred_call_with_no_triggers_bound,no_side_effects(F)),
  (\+ current_predicate(_,F) -> fail;call_prologsys(F)).
  %= we check for system predicates as well.
  %has_cl(F) -> (clause_u(F,Condition),(Condition==true->true;call_u(Condition)));
  %call_prologsys(F).


mpred_bc_only(G):- mpred_negation(G,Pos),!, show_call(why,\+ mpred_bc_only(Pos)).
mpred_bc_only(G):- loop_check(no_repeats(pfcBC_NoFacts(G))).
mpred_bc_only(G):- mpred_call_only_facts(G).

%%
%= pfcBC_NoFacts(F) is true iff F is a fact available for backward chaining ONLY.
%= Note that this has the side effect of catching unsupported facts and
%= assigning them support from God.
%= this Predicate should hide Facts from mpred_bc_only/1
%%
pfcBC_NoFacts(F):- pfcBC_NoFacts_TRY(F)*-> true ; (mpred_slow_search,pfcBC_Cache(F)).

mpred_slow_search.


ruleBackward(R,Condition):- with_umt(( ruleBackward0(R,Condition),functor(Condition,F,_),\+ arg(_,v(call_prologsys,call_u),F))).
%ruleBackward0(F,Condition):-clause_u(F,Condition),\+ (is_true(Condition);mpred_is_info(Condition)).
ruleBackward0(F,Condition):- with_umt((  '<-'(F,Condition),\+ (is_true(Condition);mpred_is_info(Condition)) )).

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
  (predicate_property(P,dynamic)->ain(P,S);true)).

mpred_ignored(argIsa(F, A, argIsaFn(F, A))).
mpred_ignored(genls(A,A)).
mpred_ignored(isa(tCol,tCol)).
%mpred_ignored(isa(W,tCol)):-mreq(lmconf:hasInstance_dyn(tCol,W)).
mpred_ignored(isa(W,_)):-is_ftCompound(W),isa(W,pred_argtypes).
mpred_ignored(C):-clause_safe(C,true). 
mpred_ignored(isa(_,Atom)):-atom(Atom),atom_concat(ft,_,Atom),!.
mpred_ignored(isa(_,argIsaFn(_, _))).


has_cl(H):-predicate_property(H,number_of_clauses(_)).

% an action is undoable if there exists a method for undoing it.
undoable(A) :- req(mpred_do_and_undo_method(A,_)).

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

mpred_nf1(-((P,Q)),NF) :-
 mpred_nf1(-P,NP),
 mpred_nf1(-Q,NQ),
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

mpred_negation_w_neg(~(P),P):-is_ftNonvar(P),!.
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
%= changing -{...} to {\+...}
%=% ? is this still needed? tmpred_wff 3/16/90

mpred_nf_negations(X,X) :- !.  % I think not! tmpred_wff 3/27/90

mpred_nf_negations([],[]).

mpred_nf_negations([H1|T1],[H2|T2]) :-
  mpred_nf_negation(H1,H2),
  mpred_nf_negations(T1,T2).

mpred_nf_negation(Form,{\+ X}) :-
  is_ftNonvar(Form),
  Form=(-({X})),
  !.
mpred_nf_negation(Form,{\+ X}) :-
  is_ftNonvar(Form),
  Form=(~({X})),
  !.
mpred_nf_negation(X,X).


%=
%= build_rhs(Sup,+Conjunction,-Rhs)
%=

build_rhs(_Sup,X,[X]) :-
  is_ftVar(X),
  !.
build_rhs(_Sup,~(X),[~(X)]) :-
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
mpred_compile_rhsTerm(Sup,I,O):-to_addable_form_wte(mpred_compile_rhsTerm,I,O), must(\+ \+ mpred_mark_as(Sup,p,O,pfcRHSR)),!.

:- export(mpred_mark_as_ml/4).
mpred_mark_as_ml(Sup,PosNeg,Type,P):- mpred_mark_as(Sup,PosNeg,P,Type).

pos_2_neg(p,n):-!.
pos_2_neg(n,p):-!.
pos_2_neg(P,~(P)).

mpred_mark_as(_,_,P,_):- is_ftVar(P),!.
mpred_mark_as(Sup,PosNeg,\+(P),Type):-!,mpred_mark_as(Sup,PosNeg,P,Type).
mpred_mark_as(Sup,Pos,~(P),Type):- pos_2_neg(Pos,Neg),!,mpred_mark_as(Sup,Neg,P,Type).
mpred_mark_as(Sup,Pos,-(P),Type):- pos_2_neg(Pos,Neg),!,mpred_mark_as(Sup,Neg,P,Type).
mpred_mark_as(Sup,PosNeg,[P|PL],Type):- is_list([P|PL]), !,must_maplist(mpred_mark_as_ml(Sup,PosNeg,Type),[P|PL]).
mpred_mark_as(Sup,PosNeg,( P / CC ),Type):- !, mpred_mark_as(Sup,PosNeg,P,Type),mpred_mark_as(Sup,PosNeg,( CC ),pfcCallCode).
mpred_mark_as(Sup,PosNeg,'{}'(  CC ), _Type):- mpred_mark_as(Sup,PosNeg,( CC ),pfcCallCode).
mpred_mark_as(Sup,PosNeg,( A , B), Type):- !, mpred_mark_as(Sup,PosNeg,A, Type),mpred_mark_as(Sup,PosNeg,B, Type).
mpred_mark_as(Sup,PosNeg,( A ; B), Type):- !, mpred_mark_as(Sup,PosNeg,A, Type),mpred_mark_as(Sup,PosNeg,B, Type).
mpred_mark_as(Sup,PosNeg,( A ==> B), Type):- !, mpred_mark_as(Sup,PosNeg,A, Type),mpred_mark_as(Sup,PosNeg,B, pfcRHS).
%mpred_mark_as(_Sup,_PosNeg,( _ :- _ ),_Type):-!.
mpred_mark_as(Sup,PosNeg,( P :- CC ),Type):- !, mpred_mark_as(Sup,PosNeg,P,Type),mpred_mark_as(Sup,PosNeg,( CC ),pfcCallCode).
mpred_mark_as(Sup,PosNeg,P,Type):-get_functor(P,F,A),ignore(mpred_mark_fa_as(Sup,PosNeg,P,F,A,Type)),!.

:- was_dynamic( mpred_mark/4).

% mpred_mark_fa_as(_,_,_,'\=',2,_):- trace.
mpred_mark_fa_as(_Sup,_PosNeg,_P,isa,_,_):- !.
mpred_mark_fa_as(_Sup,_PosNeg,_P,t,_,_):- !.
mpred_mark_fa_as(_Sup,_PosNeg,_P,argIsa,N,_):- !,must(N=3).
mpred_mark_fa_as(_Sup,_PosNeg,_P,arity,N,_):- !,must(N=2).
mpred_mark_fa_as(_Sup,_PosNeg,_P,mpred_mark,N,_):- !,must(N=4).
mpred_mark_fa_as(_Sup,_PosNeg,_P,mpred_isa,N,_):- must(N=2).
mpred_mark_fa_as(_Sup,_PosNeg,_P,'[|]',N,_):- trace,must(N=2).
mpred_mark_fa_as(_Sup,_PosNeg,_P,_:mpred_isa,N,_):- must(N=2).
mpred_mark_fa_as(_Sup, PosNeg,_P,F,A,Type):- req(mpred_mark(Type,PosNeg,F,A)),!.
mpred_mark_fa_as(Sup,PosNeg,_P,F,A,Type):- 
  MARK = mpred_mark(Type,PosNeg,F,A),
  check_never_assert(MARK),
  mpred_post_sp_zzz((s(Sup),g),MARK),!.
   
fa_to_p(F,A,P):-integer(A),atom(F),functor(P,F,A),( P \= call_u(_) ),( P \= '$VAR'(_)).

lmconf:hook_one_minute_timer_tick:-mpred_cleanup.

mpred_cleanup:- forall((no_repeats(F-A,(mpred_mark(pfcRHS,_,F,A),A>1))),mpred_cleanup(F,A)).

mpred_cleanup(F,A):-functor(P,F,A),predicate_property(P,dynamic)->mpred_cleanup_0(P);true.

mpred_cleanup_0(P):- findall(P-B-Ref,clause(P,B,Ref),L),forall(member(P-B-Ref,L),erase_w_attvars(clause(P,B,Ref),Ref)),forall(member(P-B-Ref,L),attvar_op(assertz_if_new,((P:-B)))).

% :-debug.
%isInstFn(A):-!,trace_or_throw(isInstFn(A)).

%= mpred_negation(N,P) is true if N is a negated term and P is the term
%= with the negation operator stripped.

mpred_negation((-P),P).
mpred_negation((-P),P).
mpred_negation((\+(P)),P).

mpred_negated_literal(P):-mpred_negated_literal(P,_).
mpred_negated_literal(P,Q) :- is_ftNonvar(P),
  mpred_negation(P,Q),
  mpred_literal(Q).

mpred_is_assertable(X):- mpred_literal_nv(X),\+ functor(X,{},_).
mpred_literal_nv(X):-is_ftNonvar(X),mpred_literal(X).
mpred_literal(X) :- is_reprop(X),!,fail.
mpred_literal(X) :- cyclic_term(X),!,fail.
mpred_literal(X) :- atom(X),!.
mpred_literal(X) :- mpred_negated_literal(X),!.
mpred_literal(X) :- mpred_positive_literal(X),!.
mpred_literal(X) :- is_ftVar(X),!.

is_reprop(X):- compound(X),is_reprop_0(X).
is_reprop_0(~(X)):-!,is_reprop(X).
is_reprop_0(X):-functor(X,repropagate,_).

mpred_non_neg_literal(X):-is_reprop(X),!,fail.
mpred_non_neg_literal(X):-atom(X),!.
mpred_non_neg_literal(X):- sanity(stack_check),
    mpred_positive_literal(X), X \= ~(_), X \= mpred_mark(_,_,_,_), X \= conflict(_).

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
mpred_connective('-').
mpred_connective('\\+').

process_rule(Lhs,Rhs,Parent_rule) :- 
  copy_term_and_varnames(Parent_rule,Parent_ruleCopy),
  build_rhs((Parent_ruleCopy),Rhs,Rhs2),
   cyclic_break((Lhs,Rhs,Rhs2,Parent_ruleCopy)),
  foreachl_do(mpred_nf(Lhs,Lhs2),
   build_rule(Lhs2,rhs(Rhs2),(Parent_ruleCopy,u))).

build_rule(Lhs,Rhs,Support) :-
  build_trigger(Support,Lhs,Rhs,Trigger),
   cyclic_break((Lhs,Rhs,Support,Trigger)),
  mpred_eval_lhs(Trigger,Support).

build_trigger(Support,[],Consequent,ConsequentO):- 
      build_consequent(Support,Consequent,ConsequentO).

build_trigger(Support,[V|Triggers],Consequent,basePFC:pt('$ABOX',V,X)) :-
  is_ftVar(V),
  !,
  build_trigger(Support,Triggers,Consequent,X).

build_trigger(Support,[added(T)|Triggers],Consequent,basePFC:pt('$ABOX',T,X)) :-
  !,
  build_code_test(Support,ground(T),Test2),
  build_trigger(Support,[{Test2}|Triggers],Consequent,X).



build_trigger(Support,[(T1/Test)|Triggers],Consequent,basePFC:nt('$ABOX',T2,Test2,X)) :-
  is_ftNonvar(T1),mpred_negation(T1,T2),
  !,
  build_neg_test(Support,T2,Test,Test2),
  build_trigger(Support,Triggers,Consequent,X).

build_trigger(Support,[(T1)|Triggers],Consequent,basePFC:nt('$ABOX',T2,Test,X)) :-
  mpred_negation(T1,T2),
  !,
  build_neg_test(Support,T2,true,Test),
  build_trigger(Support,Triggers,Consequent,X).

build_trigger(Support,[{Test}|Triggers],Consequent,(Test->X)) :-
  !,
  build_trigger(Support,Triggers,Consequent,X).

build_trigger(Support,[T/Test|Triggers],Consequent,basePFC:pt('$ABOX',T,X)) :-
  !,
  build_code_test(Support,Test,Test2),
  build_trigger(Support,[{Test2}|Triggers],Consequent,X).


%build_trigger(Support,[snip|Triggers],Consequent,snip(X)) :-
%  !,
%  build_trigger(Support,Triggers,Consequent,X).

build_trigger(Support,[T|Triggers],Consequent,basePFC:pt('$ABOX',T,X)) :-
  !,
  build_trigger(Support,Triggers,Consequent,X).

%=
%= build_neg_test(Support,+,+,-).
%=
%= builds the test used in a negative trigger (basePFC:nt/4).  This test is a
%= conjunction of the check than no matching facts are in the db and any
%= additional test specified in the rule attached to this - term.
%=

build_neg_test(Support,T,Testin,Testout) :-  % must(sanity(is_ftNonvar(T))),
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
code_sentence_op(~(_)).
code_sentence_op(-(_)).
code_sentence_op(-(_)).
code_sentence_op(\+(_)).
code_sentence_op(call_u(_)).
code_sentence_op(call_u(_,_)).
code_sentence_op(Test):-predicate_property(Test,meta_predicate(PP)),predicate_property(Test,built_in),  \+ (( arg(_,PP,N), N\=0)).

all_closed(C):- \+is_ftCompound(C)->true;(functor(C,_,A),A>1,\+((arg(_,C,Arg),is_ftVar(Arg)))),!.

%=

build_consequent(_      ,Test,Test):- is_ftVar(Test),!.
build_consequent(_      ,Test,TestO):-is_ftVar(Test),!,TestO=added(Test).
build_consequent(Support,rhs(Test),rhs(TestO)) :- !,build_consequent(Support,Test,TestO).
build_consequent(Support,Test,TestO):- code_sentence_op(Test),Test=..[F|TestL],
   maplist(build_consequent(Support),TestL,TestLO),TestO=..[F|TestLO],!.
build_consequent(Support,Test,Test):-must(mpred_mark_as(Support,p,Test,pfcCreates)),!.
build_consequent(_ ,Test,Test).

%= simple typeing for pfc objects

mpred_db_type(('<-'(_,_)),Type) :- !, Type=rule.
mpred_db_type(('<==>'(_,_)),Type) :- !, Type=rule.
mpred_db_type(('==>'(_,_)),Type) :- !, Type=rule.
mpred_db_type((':-'(_,_)),Type) :- !, Type=rule.
mpred_db_type(basePFC:pk('$ABOX',_,_,_),Type) :- !, Type=trigger.
mpred_db_type(basePFC:pt('$ABOX',_,_),Type) :- !, Type=trigger.
mpred_db_type(basePFC:nt('$ABOX',_,_,_),Type) :- !,  Type=trigger.
mpred_db_type(basePFC:bt('$ABOX',_,_),Type) :- !,  Type=trigger.
mpred_db_type(mpred_action(_),Type) :- !, Type=action.
mpred_db_type((('::::'(_,X))),Type) :- is_ftNonvar(X),!, mpred_db_type(X,Type).
mpred_db_type(_,fact) :-
  %= if it''s not one of the above, it must be a fact!
  !.



mpred_call_t_exact(Trigger) :- copy_term_and_varnames(Trigger,Copy),mpred_get_trigger_quick(Trigger),Trigger=@=Copy.

retract_t(Trigger) :-  retract_i(basePFC:spft(ukb,Trigger,_,_,_)),ignore(retract_i(Trigger)).


ain_t(P,Support) :- 
  (mpred_clause_i(P) ; (assert_i(P),ain_trigger(P,Support))),
  !,
  ain_support(P,Support).



aina_i(P,Support) :-
  (mpred_clause_i(P) ; asserta_i(P)),
  !,
  ain_support(P,Support).


ainz_i(P,Support) :-  
  (mpred_clause_i(P) ; assertz_i(P)),
  !,
  ain_support(P,Support).


mpred_clause_i((Head :- Body)) :-
  !,
  copy_term_and_varnames(Head,Head_copy),
  copy_term_and_varnames(Body,Body_copy),
  clause_i(Head,Body),
  variant(Head,Head_copy),
  variant(Body,Body_copy).

mpred_clause_i(Head) :-
  % find a unit clause_db identical to Head by finding one which unifies,
  % and then checking to see if it is identical
  copy_term_and_varnames(Head,Head_copy),
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


% user:portray(C):-is_ftCompound(C),C=basePFC:spft(ukb,_,_,_,_),pp_item('',C).

%= mpred_had_support(+Fact,+Support)
mpred_had_support(P,(Fact,Trigger)) :- 
 ( clause_asserted_local(basePFC:spft(ukb,P,Fact,Trigger,_OldWhy)) -> 
    true ; fail).


%= ain_support(+Fact,+Support)

ain_support(P,(Fact,Trigger)) :- 
 ( clause_asserted_local(basePFC:spft(ukb,P,Fact,Trigger,_OldWhy)) ->
    true ; 
    (current_why(Why), 
      ( % get_clause_vars(P),get_clause_vars(Fact),get_clause_vars(Trigger),
        get_clause_vars(basePFC:spft(ukb,P,Fact,Trigger,Why)),
      attvar_op(assertz,(basePFC:spft(ukb,P,Fact,Trigger,Why)))))),!.  % was assert_i

/*
ain_support(P,(Fact,Trigger)) :-
  NEWSUPPORT = basePFC:spft(ukb,NewP,NewFact,NewTrigger,NewWhy),
 copy_term_and_varnames(basePFC:spft(ukb,P,Fact,Trigger,Why,OldWhy),NEWSUPPORT),
 ( clause_asserted_local(basePFC:spft(ukb,P,Fact,Trigger,OldWhy)) ->
    true ; 
    (current_why(NewWhy),attvar_op(assertz,(NEWSUPPORT)))),!. 
*/

ain_support(P,FT) :- trace_or_throw(failed_ain_support(P,FT)).


mpred_get_support(not(P),(Fact,Trigger)) :- is_ftNonvar(P),!, mpred_get_support(~(P),(Fact,Trigger)).
mpred_get_support(P,(Fact,Trigger)) :- basePFC:spft(ukb,P,Fact,Trigger,_)*->true;(is_ftNonvar(P),mpred_get_support_neg(P,(Fact,Trigger))).

% dont mpred_get_support_neg(\+ ~(P),(Fact,Trigger)) :- sp ftY((P),Fact,Trigger).
mpred_get_support_neg(\+ (P),S) :- !, is_ftNonvar(P), mpred_get_support(~(P),S).
mpred_get_support_neg(- (P),S) :- !, is_ftNonvar(P), mpred_get_support(~(P),S).


% There are three of these to try to efficiently handle the cases
% where some of the arguments are not bound but at least one is.

mpred_rem_support(WhyIn,P,S):- P \= ~(_), mpred_trace_msg('Removing support',mpred_rem_support(WhyIn,P,S)),fail.
mpred_rem_support(WhyIn,P,(Fact,Trigger)) :- is_ftVar(P),!,copy_term_and_varnames(mpred_rem_support(mpred_rem_support,P,(Fact,Trigger)) ,TheWhy),
  SPFC = basePFC:spft(ukb,RP,RFact,RTrigger,_RWhy),
  clause(basePFC:spft(ukb,P,Fact,Trigger,_),true,Ref),
  ((clause(SPFC,true,Ref),
     ( spftV(RP,RFact,RTrigger) =@= spftV(P,Fact,Trigger) -> 
        erase_w_attvars(clause(SPFC,true,Ref),Ref); 
       (mpred_trace_msg(<=(TheWhy,-SPFC)),nop(mpred_retract_or_warn_i(spftVVVVVVV(P,Fact,Trigger))),nop(trace))),
   (is_ftVar(P)->trace_or_throw(is_ftVar(P));remove_if_unsupported_verbose(WhyIn,local,P)))).
mpred_rem_support(Why,(\+ N) , S):- mpred_rem_support(Why,~(N),S).
mpred_rem_support(_Why,P,(Fact,Trigger)):-mpred_retract_or_warn_i(basePFC:spft(ukb,P,Fact,Trigger,_)).

/*
% TODO not called yet
mpred_collect_supports_f_l(Tripples) :-
  bagof(Tripple, mpred_support_relation(Tripple), Tripples),
  !.
mpred_collect_supports_f_l([]).
*/
/* UNUSED TODAY
% TODO not called yet
mpred_support_relation((P,F,T)) :- basePFC:spft(ukb,P,F,T,_).

% TODO not called yet
mpred_make_supports_f_l((P,S1,S2)) :-
  % was ain_support(P,(S1,S2),_),
  ain_support(P,(S1,S2)),
  (ain_db_type(P); true),
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

mpred_get_trigger_key(basePFC:pt('$ABOX',Key,_),Key).
mpred_get_trigger_key(basePFC:pk('$ABOX',Key,_,_),Key).
mpred_get_trigger_key(basePFC:nt('$ABOX',Key,_,_),Key).
mpred_get_trigger_key(Key,Key).
*/

/*

the FOL i get from SUMO, CycL, UMBEL and many *non* RDF ontologies out there.. i convert to Datalog..  evidently my conversion process is unique as it preserves semantics most by the book conversions gave up on. 


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

mpred_database_term(basePFC:spft/5).
mpred_database_term(basePFC:pk/4).
mpred_database_term(basePFC:pt/3).  % was 3
mpred_database_term(basePFC:bt/3).  % was 3
mpred_database_term(basePFC:nt/4). % was 4
mpred_database_term('<-'/2).
mpred_database_term('==>'/2).
mpred_database_term('<==>'/2).
mpred_database_term(basePFC:qu/3).

:- forall(mpred_database_term(T),shared_multifile(T)).


% removes all forward chaining rules and justifications from db.

mpred_reset :-
  clause_i(basePFC:spft(ukb,P,F,Trigger,Why),true),
  mpred_retract_or_warn_i(P),
  mpred_retract_or_warn_i(basePFC:spft(ukb,P,F,Trigger,Why)),
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
mpred_retract_or_warn_i(X) :- \+ \+ X =basePFC:spft(ukb,~(_),_,_,_),!.
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
clause_or_call(H,true):- req(should_call_for_facts(H)),no_repeats(on_x_log_throw(H)).


% as opposed to simply using clause(H,true).
should_call_for_facts(H):- get_functor(H,F,A),with_umt(should_call_for_facts(H,F,A)).
should_call_for_facts(_,F,_):- prologSideEffects(F),!,fail.
should_call_for_facts(H,_,_):- \+ predicate_property(H,number_of_clauses(_)),!.
should_call_for_facts(_,F,A):- mpred_mark(pfcRHS,_,F,A),!,fail.
should_call_for_facts(_,F,A):- mpred_mark(pfcMustFC,_,F,A),!,fail.
should_call_for_facts(_,F,_):- prologDynamic(F),!.
should_call_for_facts(_,F,_):- \+ pfcControlled(F),!.

no_side_effects(S):- get_functor(S,F), (req(prologSideEffects(F))-> (\+ is_side_effect_disabled);true).

is_disabled_clause(C):-is_edited_clause(C,_,New),memberchk((disabled),New).

%= mpred_fact(P) is true if fact P was asserted into the database via ain.

mpred_fact(P) :- mpred_fact(P,true).

%= mpred_fact(P,C) is true if fact P was asserted into the database via
%= assert and condition C is satisfied.  For example, we might do:
%=
%=  mpred_fact(X,mpred_user_fact(X))
%=

mpred_user_fact(X):-no_repeats(basePFC:spft(ukb,X,U,U,_)).

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
mpred_trace_add(basePFC:bt('$ABOX',Head,Body)) :-
  % hack for now - never trace triggers.
  !.
mpred_trace_add(basePFC:pt('$ABOX',Head,Body)) :-
  % hack for now - never trace triggers.
  !.
mpred_trace_add(basePFC:nt('$ABOX',Head,Condition,Body)) :-
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
        ; (((mpred_trace_msg("Adding (:) ~p    <-------- -n (~p <-TF-> ~p)",[P,(T),(F)]))))).

mpred_trace_addPrint_0(_,_).


mpred_trace_break(P,_S) :-
  mpred_is_spying(P,add) ->
   ((\+ \+ wdmsg("Breaking on ain(~p)",[P])),
    break)
   ; true.

/*
mpred_trace_rem(Why,basePFC:bt('$ABOX',Head,Body)) :-
  % hack for now - never trace triggers.
  !.
mpred_trace_rem(Why,basePFC:pt('$ABOX',Head,Body)) :-
  % hack for now - never trace triggers.
  !.
mpred_trace_rem(Why,basePFC:nt('$ABOX',Head,Condition,Body)) :-
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
mpred_notrace_exec :- retractall_i(mpred_is_tracing_exec).
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

mmsg(Msg,Args):- is_list(Args) -> wdmsg(Msg, Args) ; pp_item(Msg, Args).

:- was_dynamic(mpred_hide_msg/1).
mpred_hide_msg('Adding For Later').
mpred_hide_msg('Skipped Trigger').
mpred_hide_msg('Had Support').

% mpred_trace_msg(Msg,Args) :- !, mmsg(Msg,Args).
% mpred_trace_msg(Msg,_Args) :- mpred_hide_msg(Msg),!.
mpred_trace_msg(Msg,Args) :- ignore((mpred_is_tracing_exec,!,\+ mpred_is_silient, !, mmsg(Msg,Args))),!.



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

%% baseable(P,L) - is true iff L is a list of "baseable" facts which, taken
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


:- was_dynamic(baseKB:prologMacroHead/1).

compute_resolve(NewerP,OlderQ,SU,SU,(mpred_remove3(OlderQ),ain(NewerP,S),mpred_rem1(conflict(NewerP)))):-
  must(correctify_support(SU,S)),
  wdmsg(compute_resolve(newer(NewerP-S)>older(OlderQ-S))).
compute_resolve(NewerP,OlderQ,S1,[U],Resolve):-compute_resolve(OlderQ,NewerP,[U2],S1,Resolve),match_source_ref1(U),match_source_ref1(U2),!.
compute_resolve(NewerP,OlderQ,SU,S2,(mpred_remove3(OlderQ),ain(NewerP,S1),mpred_rem1(conflict(NewerP)))):-
  must(correctify_support(SU,S1)),
  wdmsg(compute_resolve((NewerP-S1)>(OlderQ-S2))).


compute_resolve(NewerP,OlderQ,Resolve):-
   supports_f_l(NewerP,S1),
   supports_f_l(OlderQ,S2),
   compute_resolve(NewerP,OlderQ,S1,S2,Resolve).


is_resolved(C):- Why= is_resolved, mpred_call_only_facts(Why,C),\+mpred_call_only_facts(Why,~(C)).
is_resolved(C):- Why= is_resolved, mpred_call_only_facts(Why,~(C)),\+mpred_call_only_facts(Why,C).

:- must(nop(_)).

mpred_prove_neg(G):-nop(trace), \+ mpred_bc_only(G), \+ mpred_fact(G).

pred_head(Type,P):- no_repeats_u(P,(call(Type,P),\+ nonfact_metawrapper(P),is_ftCompound(P))).

pred_head_all(P):- pred_head(pred_all,P).

nonfact_metawrapper(~(_)).
nonfact_metawrapper(basePFC:pt(_,_,_)).
nonfact_metawrapper(basePFC:bt(_,_,_)).
nonfact_metawrapper(basePFC:nt(_,_,_,_)).
nonfact_metawrapper(basePFC:spft(_,_,_,_,_)).
nonfact_metawrapper(added(_)).
% we use the arity 1 forms is why 
nonfact_metawrapper(term_expansion(_,_)).
nonfact_metawrapper(P):- \+ current_predicate(_,P).
nonfact_metawrapper(P):- functor(P,F,_), 
   (req(prologSideEffects(F));req(tNotForUnboundPredicates(F))).
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
pred_u1(P):-a(pfcControlled,F),arity(F,A),functor(P,F,A).
pred_u1(P):-a(prologHybrid,F),arity(F,A),functor(P,F,A).
pred_u1(P):-a(prologDynamic,F),arity(F,A),functor(P,F,A).
pred_u2(P):-support_hilog(F,A),functor(P,F,A),has_db_clauses(P).
pred_u2(P):-clause(arity(F,A),true),functor(P,F,A),has_db_clauses(P).

has_db_clauses(P):- predicate_property(P,number_of_clauses(NC)),\+ predicate_property(P,number_of_rules(NC)), \+ \+ clause(P,true).

pred_t0(P):-mreq(basePFC:pt('$ABOX',P,_)).
pred_t0(P):-mreq(basePFC:bt('$ABOX',P,_)).
pred_t0(P):-mreq(basePFC:nt('$ABOX',P,_,_)).
pred_t0(P):-mreq(basePFC:spft(ukb,P,_,_,_)).
pred_t0(P):- mreq('nesc'(P)).
%pred_r0(-(P)):- mreq(-(P)).
%pred_r0(~(P)):- mreq(~(P)).

pred_r0(P==>Q):- mreq(P==>Q).
pred_r0(P<==>Q):- mreq(P<==>Q).
pred_r0(P<-Q):- mreq(P<-Q).

cnstrn(X):-term_variables(X,Vs),maplist(cnstrn0(X),Vs),!.
cnstrn(V,X):-cnstrn0(X,V).
cnstrn0(X,V):-when(is_ftNonvar(V),X).

rescan_pfc:-forall(clause(lmconf:mpred_hook_rescan_files,Body),show_entry(rescan_pfc,Body)).

mpred_facts_and_universe(P):- (is_ftVar(P)->pred_head_all(P);true),req(P). % (meta_wrapper_rule(P)->req(P) ; req(P)).

add_reprop(_Trig,Var):- is_ftVar(Var), !. % trace_or_throw(add_reprop(Trig,Var)).
add_reprop(_Trig,~(Var)):- is_ftVar(Var),!.
% CREATES ERROR!!!  add_reprop(_Trig,~(_Var)):-!.
add_reprop(_Trig,~(repropagate(Var))):- \+ is_ftVar(Var),!.
add_reprop(_Trig,repropagate(~(Var))):- \+ is_ftVar(Var),!.
add_reprop(_Trig,repropagate(Var)):- \+ is_ftVar(Var),!.
% add_reprop(_Trig,_):-!.
add_reprop(Trig,(H:-B)):- trace_or_throw(add_reprop(Trig,(H:-B))).


add_reprop(Trig ,Trigger):-
  w_tl(t_l:current_why_source(Trig),
    attvar_op(assertz_if_new,(basePFC:qu('$ABOX',repropagate(Trigger),(g,g))))).


repropagate(_):-  check_context_module,fail.
%repropagate(P):-  check_real_context_module,fail.

repropagate(P):-  is_ftVar(P),!.
repropagate(P):-  meta_wrapper_rule(P),!,with_umt(repropagate_meta_wrapper_rule(P)).
repropagate(P):-  \+ predicate_property(P,_),'$find_predicate'(P,PP),PP\=[],!,forall(member(M:F/A,PP),
                                                          must((functor(Q,F,A),repropagate_1(M:Q)))).
repropagate(F/A):- atom(F),integer(A),!,functor(P,F,A),!,repropagate(P).
repropagate(F/A):- atom(F),is_ftVar(A),!,repropagate(F).

repropagate(P):-  \+ predicate_property(_:P,_),dmsg(undefined_repropagate(P)),dumpST,dtrace,!,fail.
repropagate(P):-  repropagate_0(P).

repropagate_0(P):- loop_check(with_umt(repropagate_1(P)),true).

:- thread_local t_l:is_repropagating/1.

repropagate_1(P):- is_ftVar(P),!.
repropagate_1(USER:P):- USER==user,!,repropagate_1(P).
repropagate_1((P/_)):-!,repropagate_1(P).

repropagate_1(P):- with_umt(repropagate_2(P)).

:- export(repropagate_2/1).
:- module_transparent(repropagate_2/1).
repropagate_2(P):-
 doall((mpred_facts_and_universe(P),
    w_tl(t_l:is_repropagating(P),ignore((once(fwd_ok(P)),mpred_fwd(P)))))).

% repropagate_meta_wrapper_rule(P==>_):- !, repropagate(P).
repropagate_meta_wrapper_rule(P):-repropagate_1(P).

fwd_ok(P):-ground(P),!.
fwd_ok(if_missing(_,_)).
fwd_ok(idForTest(_,_)).
fwd_ok(clif(_)).
% fwd_ok(_).
% fwd_ok(P):-must(ground(P)),!.

mpred_facts_only(P):- (is_ftVar(P)->(pred_head_all(P),\+ meta_wrapper_rule(P));true),no_repeats(P).

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

:- source_location(S,_),prolog_load_context(module,M),forall(source_file(M:H,S),(functor(H,F,A),M:module_transparent(M:F/A),M:export(M:F/A))).

% :- doall(lmconf:module_local_init).
