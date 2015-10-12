/* <module> 
% ===================================================================
% File 'mpred_db_preds.pl'
% Purpose: Emulation of OpenCyc for SWI-Prolog
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'interface.pl' 1.0.0
% Revision:  $Revision: 1.9 $
% Revised At:   $Date: 2002/06/27 14:13:20 $
% ===================================================================
% File used as storage place for all predicates which change as
% the world is run.
%
%
% Dec 13, 2035
% Douglas Miles
*/
:- module(kb,
          [ 

            %will_call_after/2,
            (::::) / 2,
            (<-)/2,
            (<==>)/2,
            (==>)/1,
            (==>)/2,
            add_args/15,
            argGenls/3,
            argIsa/3,
            argIsa_known/3,
            argQuotedIsa/3,
            argsQuoted/1,
            arity/2,
            bt/2,
            call_mt_t/11,
            call_which_t/9,
            completelyAssertedCollection/1,
            constrain_args_pttp/2,
            contract_output_proof/2,
            defnSufficient/2,
            disjointWith/2,
            f_to_mfa/4,
            fcTmsMode/1,
            formatted_resultIsa/2,
            functorDeclares/1,
            genls/2,
            get_clause_vars_for_print/2,
            holds_f_p2/2,
            input_to_forms/2,
            is_wrapper_pred/1,
            isa/2,
            lambda/5,
            logical_functor_pttp/1,
            meta_argtypes/1,
            mpred_action/1,
            mpred_f/1,
            mpred_halt_signal/1,
            mpred_halt_signal/2,
            mpred_isa/2,
            mpred_module/2,
            mpred_queue/2,
            mpred_search/1,
            mpred_search_mode/1,
            mpred_select/2,
            mpred_do_and_undo_method/2,
            conflict/1,
            mpred_univ/1,
            natFunction/2,
            neg/1,
            (nesc)/1,
            nt/3,
            pfcControlled/1,
            pfcRHS/1,
            pk/3,
            pp_i2tml_now/1,
            pp_item_html/2,
            predCanHaveSingletons/1,
            prologBuiltin/1,
            prologDynamic/1,
            prologDynamic/2,
            prologHybrid/1,
            prologHybridFact/1,
            prologKIF/1,
            prologListValued/1,
            prologMacroHead/1,
            prologMultiValued/1,
            prologNegByFailure/1,
            prologOrdered/1,
            prologPTTP/1,
            prologSideEffects/1,
            prologSingleValued/1,
            props/2,
            pt/2,
            ptReformulatorDirectivePredicate/1,
            pttp1a_wid/3,
            pttp_builtin/2,
            pttp_nnf_pre_clean_functor/3,
            quasiQuote/1,
            relationMostInstance/3,
            relax_term/6,
            resolveConflict/1,
            resolverConflict_robot/1,
            resultGenls/2,
            resultIsa/2,
            retractall_wid/1,
            ruleRewrite/2,
            search/7,
            session_agent/2,
            session_io/4,
            singleValuedInArg/2,
            spftY/4,
            startup_option/2,
            support_hilog/2,
            svar_fixvarname/2,
            t/1,
            t/10,
            t/11,
            t/2,
            t/3,
            t/4,
            t/5,
            t/6,
            t/7,
            t/8,
            t/9,
            tCol/1,
            tFarthestReachableItem/1,
            tFunction/1,
            tNearestReachableItem/1,
            tNotForUnboundPredicates/1,
            tPred/1,
            tPredType/1,
            tRelation/1,
            tSet/1,
            ttFormatType/1,
            ttTemporalType/1,
            ttUnverifiableType/1,
            was_chain_rule/1
          ]).


:- 
 With = [export,multifile,dynamic,discontiguous],
 with_pfa(With,((logical_functor_pttp/1, pfcControlled/1, pfcRHS/1,  conflict/1, kb:argsQuoted/1,       add_args/15,argIsa_known/3,call_mt_t/11))),
 with_pfa(With,(((call_which_t/9,constrain_args_pttp/2,contract_output_proof/2,f_to_mfa/4,get_clause_vars_for_print/2,holds_f_p2/2,input_to_forms/2,is_wrapper_pred/1,lambda/5,mpred_f/1,
          pp_i2tml_now/1,pp_item_html/2,pttp1a_wid/3,pttp_builtin/2,pttp_nnf_pre_clean_functor/3,quasiQuote/1,relax_term/6,retractall_wid/1,ruleRewrite/2,search/7,support_hilog/2,svar_fixvarname/2)))),
 with_pfa(With,((pfcControlled/1,pfcRHS/1,logical_functor_pttp/1,          add_args/15,argIsa_known/3,call_mt_t/11,call_which_t/9,constrain_args_pttp/2,contract_output_proof/2,f_to_mfa/4,get_clause_vars_for_print/2,holds_f_p2/2,input_to_forms/2,is_wrapper_pred/1,lambda/5,mpred_f/1,pp_i2tml_now/1,pp_item_html/2,pttp1a_wid/3,pttp_builtin/2,pttp_nnf_pre_clean_functor/3,
          quasiQuote/1,relax_term/6,retractall_wid/1,ruleRewrite/2,search/7,support_hilog/2,svar_fixvarname/2,tNotForUnboundPredicates/1))),
 with_pfa(With,(((('==>')/1),('bt'/2),('nt'/3),('pk'/3),('pt'/2),('spftY'/4),
          (('::::')/2),(('<-')/2),(('<==>')/2),(('==>')/2),(('neg')/1),(('nesc')/1),         ((mpred_action)/1),(fcTmsMode/1),(mpred_halt_signal/1),
          (mpred_queue/2),(mpred_search/1),(mpred_search_mode/1),(mpred_select/2),(mpred_do_and_undo_method/2),

	  prologMultiValued/1,prologOrdered/1,prologNegByFailure/1,prologPTTP/1,prologKIF/1,pfcControlled/1,tPredType/1,
           prologHybrid/1,predCanHaveSingletons/1,prologDynamic/1,prologBuiltin/1,prologMacroHead/1,prologListValued/1,prologSingleValued/1,
          (mpred_halt_signal/2),(pfcControlled/1),(prologDynamic/2),(prologSideEffects/1),(prologSingleValued/1),(singleValuedInArg/2),(prologSideEffects/1,prologMacroHead/1,pfcControlled/1,
           resolveConflict/1,resolverConflict_robot/1)))),
 with_pfa(With,((mpred_isa/2,arity/2,mpred_module/2))).



arity(apathFn,2).
arity(isKappaFn,2).
arity(isInstFn,1).
arity(ftListFn,1).
arity(xyzFn,4).
arity(arity,2).
arity(is_never_type,1).
arity(argIsa, 3).
arity(Prop,1):-ttPredType(Prop).
arity(meta_argtypes,1).
arity(arity,2).
arity(is_never_type,1).
arity(prologSingleValued,1).
arity('<=>',2).
arity(F,A):- atom(F), integer(A),current_predicate(F/A),A>1.
arity(F,1):- atom(F), current_predicate(F/1),\+((dif:dif(Z,1), arity(F,Z))).


% mpred_expansion
:- dynamic(props/2, ptReformulatorDirectivePredicate/1, ruleRewrite/2, was_chain_rule/1).

% mpred_hooks
:- dynamic((
prologHybridFact/1,
relationMostInstance/3,
session_agent/2,
session_io/4,
startup_option/2,
tFarthestReachableItem/1,
tNearestReachableItem/1,
t/1,t/2,t/3,t/4,t/5,t/6,t/7,t/8,t/9,t/10,t/11)).

% mpred_kb_ops

% mpred_type_args
:- dynamic 
   argGenls/3,
   argIsa/3,
        argsQuoted/1,
        argQuotedIsa/3,
        formatted_resultIsa/2,
        meta_argtypes/1,
        resultGenls/2,
        resultIsa/2,
        ttFormatType/1.

% mpred_type_constraints.

% mpred_type_isa
:- dynamic 
         tSet/1,
         isa/2,
         tCol/1,
         tFunction/1,
         functorDeclares/1,
         completelyAssertedCollection/1,
         disjointWith/2,
         ttTemporalType/1,
         defnSufficient/2,
         natFunction/2,
         tPred/1,
         mpred_univ/1,
         tRelation/1,
         genls/2,         
         ttUnverifiableType/1.

% mpred_type_naming

