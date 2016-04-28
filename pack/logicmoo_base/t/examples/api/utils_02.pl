
% TODO
% :- guitracer.


:- use_module(library(logicmoo_user)).
% :- autoload.

% :- meta_predicate logicmoo_util_terms:'__aux_maplist/3_maptree+1'(*,*,2).
% :- meta_predicate logicmoo_util_strings:'__aux_maplist/3_toCaseSplit+2'(*,*,?,2).
% :- meta_predicate logicmoo_util_strings:convert_members(2,?,?).
% :- meta_predicate logicmoo_util_strings:'__aux_maplist/3_toCase+1'(*,*,2).
% :- meta_predicate logicmoo_util_preddefs:with_pi_selected(+,+,*,0).
% :- meta_predicate logicmoo_util_preddefs:dynamic_safe(0).
% :- meta_predicate mpred_pfc:foreachl_do(0,*).
% :- meta_predicate mpred_pfc:pfcl_do(0).
% :- meta_predicate logicmoo_util_loop_check:call_tabled1(*,?,0,-).
% :- meta_predicate logicmoo_util_loop_check:call_tabled0(*,?,0,-).
% :- meta_predicate vn:set_varname(2,*,*).
% :- meta_predicate vn:set_varname(2,*).
% :- meta_predicate baseKB:t(3,?,?,?).
% :- meta_predicate baseKB:t(4,?,?,?,?).
% :- meta_predicate baseKB:t(2,?,?).
% :- meta_predicate baseKB:resolverConflict_robot(0).
% :- meta_predicate logicmoo_util_dumpst:dtrace(*,0).
% :- meta_predicate logicmoo_util_dumpst:dumptrace_ret(0).
% :- meta_predicate logicmoo_util_dumpst:dumptrace(0).
% :- meta_predicate logicmoo_util_rtrace:hotrace_prev(0).
% :- meta_predicate with_umt_l(0).
% :- meta_predicate logicmoo_util_bugger:bugger_t_expansion(:,0,*).
% :- meta_predicate logicmoo_util_bugger:prolog_must_l(0).
% :- meta_predicate logicmoo_util_bugger:bugger_goal_expansion(:,0,*).
% :- meta_predicate logicmoo_util_bugger:dtrace_msg(0).
% :- meta_predicate logicmoo_util_bugger:test_tl(0,*,*).
 :- meta_predicate logicmoo_user:t(3,?,?,?).
% :- meta_predicate logicmoo_user:t(4,?,?,?,?).
% :- meta_predicate logicmoo_user:t(2,?,?).
% :- meta_predicate logicmoo_user:resolverConflict_robot(0).
% :- meta_predicate logicmoo_util_bugger:bugger_term_expansion(:,0,*).
% :- meta_predicate mpred_props:decl_mpred_prolog(0).
% :- meta_predicate mpred_props:decl_mpred(0).
% :- meta_predicate mpred_props:decl_mpred_pi(0).
% :- meta_predicate mpred_props:decl_mpred_hybrid(0).
% :- meta_predicate mpred_props:decl_mpred_prolog(?,0).
% :- meta_predicate mpred_props:decl_mpred_hybrid(?,0).
% :- meta_predicate mpred_stubs:call_wdmsg(*,*,?,0).
% :- meta_predicate mpred_stubs:call_for_literal_db(?,0,*).
% :- meta_predicate mpred_stubs:call_for_literal(?,0,*).
% :- meta_predicate mpred_stubs:ensure_universal_stub_2(*,:,?,0,*).
% :- meta_predicate mpred_type_constraints:'__aux_maplist/3_map_subterms+1'(*,*,2).
% :- meta_predicate mpred_type_constraints:thaw(0).
% :- meta_predicate mpred_type_constraints:boxlog_goal_expansion(*,0).
% :- meta_predicate vn:renumbervars(2,*,*).
% :- meta_predicate mpred_type_isa:assert_isa_hooked(0,*).
% :- meta_predicate mpred_kb_ops:neg_in_code(0).
% :- meta_predicate mpred_kb_ops:'__aux_maplist/2_cnstrn0+1'(*,0).
% Restarting analysis ...
% Found new meta-predicates in iteration 2 (3.608 sec)
% :- meta_predicate mpred_stubs:ensure_universal_stub_1(*,:,?,0).
% :- meta_predicate mpred_stubs:ensure_universal_stub_plus_HIDE(?,0).
% :- meta_predicate mpred_props:decl_mpred_hybrid_ilc_0(*,*,0,*).
% :- meta_predicate mpred_props:decl_mpred(?,0).
% :- meta_predicate mpred_props:decl_mpred_0(*,0).
% :- meta_predicate mpred_props:decl_mpred(*,?,0).
% :- meta_predicate baseKB: ~0.
% Restarting analysis ...
% Found new meta-predicates in iteration 3 (3.628 sec)
% :- meta_predicate mpred_props:add_mpred_prop_gleaned_4(?,*,*,0).
% :- meta_predicate mpred_type_isa:assert_isa_hooked_after(?,0).
% Restarting analysis ...
% Found new meta-predicates in iteration 4 (3.645 sec)
% :- meta_predicate mpred_props:add_mpred_prop_gleaned(?,0).
% Restarting analysis ...
% Autoloader: iteration 1 resolved 53 predicates and loaded 2,435 files in 57.143 seconds.  Restarting ...
% Autoloader: loaded 53 files in 2 iterations in 60.742 seconds

no_profile_meta_preds:- 
 ignore(((current_module(M),current_predicate(_,M:P), \+ predicate_property(M:P,imported_from(_)),
   predicate_property(M:P,meta_predicate(P)), 
   ((arg(_,P,E),number(E)) ->(functor(P,F,A),noprofile(M:F/A)) ; true)),fail)).

% :- set_prolog_flag(bad_idea,true).

:- profiler(_Old, cputime).
:- no_profile_meta_preds.
:- reset_profiler.
:- profile(ensure_mpred_file_loaded('logicmoo/pfc/autoexec.pfc'),[]).


end_of_file.

% samples sez:

^CAction (h for help) ? goals
[405] [logicmoo_util_rtrace] notrace((skipWrapper;tracing;tlbugger:rtracing))
[404] [logicmoo_util_rtrace] logicmoo_util_rtrace:on_x_rtrace(baseKB:is_list([arity(tPred,_G51465),rhs([hybrid_support(tPred,_G51465)]),(prologHybrid(tPred),pt(prologHybrid(tPred),pt(arity(tPred,_G51465),rhs([hybrid_support(tPred,_G51465)]))))]))
[403] [logicmoo_util_catch] '<meta-call>'((on_x_rtrace(baseKB:is_list([arity(tPred,_G51465),rhs([hybrid_support(tPred,_G51465)]),(prologHybrid(tPred),pt(prologHybrid(tPred),pt(arity(tPred,_G51465),rhs([hybrid_support(tPred,_G51465)]))))]))*->true;debugCallWhy(failed(on_f_debug(baseKB:is_list([arity(tPred,_G51465),rhs([hybrid_support(tPred,_G51465)]),(prologHybrid(tPred),pt(prologHybrid(tPred),pt(arity(tPred,_G51465),rhs([hybrid_support(tPred,_G51465)]))))]))),baseKB:is_list([arity(tPred,_G51465),rhs([hybrid_support(tPred,_G51465)]),(prologHybrid(tPred),pt(prologHybrid(tPred),pt(arity(tPred,_G51465),rhs([hybrid_support(tPred,_G51465)]))))]))))
[402] [logicmoo_util_catch] logicmoo_util_catch:must(baseKB:is_list([arity(tPred,_G51465),rhs([hybrid_support(tPred,_G51465)]),(prologHybrid(tPred),pt(prologHybrid(tPred),pt(arity(tPred,_G51465),rhs([hybrid_support(tPred,_G51465)]))))]))
[401] [baseKB] logicmoo_util_dmsg:format_to_message('~N~n\tAdding positive~n\t\ttrigger: ~p~n\t\tbody: ~p~n\t Support: ~p~n',[arity(tPred,_G51465),rhs([hybrid_support(tPred,_G51465)]),(prologHybrid(tPred),pt(prologHybrid(tPred),pt(arity(tPred,_G51465),rhs([hybrid_support(tPred,_G51465)]))))],_G51858)
Action (h for help) ?
continue
^CAction (h for help) ? goals
[477] [logicmoo_util_loop_check] notrace(make_key(baseKB:as_is_term1(hybrid_support(prologSingleValued,1)),_G48836))
[476] [logicmoo_util_loop_check] logicmoo_util_loop_check:loop_check_term_key(baseKB:as_is_term1(hybrid_support(prologSingleValued,1)),baseKB:as_is_term1(hybrid_support(prologSingleValued,1)),logicmoo_util_loop_check:fail)
[475] [logicmoo_util_loop_check] logicmoo_util_loop_check:loop_check_early(baseKB:as_is_term1(hybrid_support(prologSingleValued,1)),logicmoo_util_loop_check:fail)
[474] [logicmoo_util_loop_check] logicmoo_util_loop_check:loop_check(baseKB:as_is_term1(hybrid_support(prologSingleValued,1)),logicmoo_util_loop_check:fail)
[473] [logicmoo_util_loop_check] logicmoo_util_loop_check:loop_check(baseKB:as_is_term1(hybrid_support(prologSingleValued,1)))
Action (h for help) ?
continue
^CAction (h for help) ? goals
[401] [system] compound(true)
[400] [baseKB] logicmoo_util_database:clause_asserted_i5(spft_mod,spft(pt(arity(prologPTTP,_G24608),rhs([hybrid_support(prologPTTP,_G24608)])),prologHybrid(prologPTTP),pt(prologHybrid(prologPTTP),pt(arity(prologPTTP,_G24608),rhs([hybrid_support(prologPTTP,_G24608)])))),true,_G24723,<clause>(0x2822400))
[399] [baseKB] logicmoo_util_database:clause_asserted_i4(spft_mod,spft(pt(arity(prologPTTP,_G24124),rhs([hybrid_support(prologPTTP,_G24124)])),prologHybrid(prologPTTP),pt(prologHybrid(prologPTTP),pt(arity(prologPTTP,_G24124),rhs([hybrid_support(prologPTTP,_G24124)])))),true,<clause>(0x2822400))
[398] [baseKB] logicmoo_util_database:clause_asserted_i(spft(pt(arity(prologPTTP,_G24124),rhs([hybrid_support(prologPTTP,_G24124)])),prologHybrid(prologPTTP),pt(prologHybrid(prologPTTP),pt(arity(prologPTTP,_G24124),rhs([hybrid_support(prologPTTP,_G24124)])))),true,<clause>(0x2822400))
[397] [baseKB] logicmoo_util_database:clause_asserted_i(spft(pt(arity(prologPTTP,_G24124),rhs([hybrid_support(prologPTTP,_G24124)])),prologHybrid(prologPTTP),pt(prologHybrid(prologPTTP),pt(arity(prologPTTP,_G24124),rhs([hybrid_support(prologPTTP,_G24124)])))))
Action (h for help) ?
continue
^CAction (h for help) ? goals
[599] [logicmoo_util_rtrace] notrace((skipWrapper;tracing;tlbugger:rtracing))
[598] [logicmoo_util_rtrace] logicmoo_util_rtrace:on_x_rtrace(baseKB: (current_predicate(_G52367,baseKB:functorDeclares(hybrid_support))->_G52373=baseKB:functorDeclares(hybrid_support);current_predicate(_G52385,baseKB:functorDeclares(hybrid_support))->_G52373=baseKB:functorDeclares(hybrid_support);fail))
[597] [logicmoo_util_catch] '<meta-call>'((on_x_rtrace(baseKB: (current_predicate(_G52367,baseKB:functorDeclares(hybrid_support))->_G52373=baseKB:functorDeclares(hybrid_support);current_predicate(_G52385,baseKB:functorDeclares(hybrid_support))->_G52373=baseKB:functorDeclares(hybrid_support);fail))*->true;debugCallWhy(failed(on_f_debug(baseKB: (current_predicate(_G52367,baseKB:functorDeclares(hybrid_support))->_G52373=baseKB:functorDeclares(hybrid_support);current_predicate(_G52385,baseKB:functorDeclares(hybrid_support))->_G52373=baseKB:functorDeclares(hybrid_support);fail))),baseKB: (current_predicate(_G52367,baseKB:functorDeclares(hybrid_support))->_G52373=baseKB:functorDeclares(hybrid_support);current_predicate(_G52385,baseKB:functorDeclares(hybrid_support))->_G52373=baseKB:functorDeclares(hybrid_support);fail))))
[596] [logicmoo_util_catch] logicmoo_util_catch:must(baseKB: (current_predicate(_G52367,baseKB:functorDeclares(hybrid_support))->_G52373=baseKB:functorDeclares(hybrid_support);current_predicate(_G52385,baseKB:functorDeclares(hybrid_support))->_G52373=baseKB:functorDeclares(hybrid_support);fail))
[595] [baseKB] with_umt(functorDeclares(hybrid_support))
Action (h for help) ?
continue
^CAction (h for help) ? goals
[446] [baseKB] as_is_term0(hybrid_support(prologHybrid,1))
[445] [logicmoo_util_rtrace] logicmoo_util_rtrace:cnotrace(baseKB:as_is_term0(hybrid_support(prologHybrid,1)))
[444] [baseKB] as_is_term(hybrid_support(prologHybrid,1))
[443] [baseKB] to_addable_form(hybrid_support(prologHybrid,1),_G50450)
[442] [logicmoo_util_rtrace] logicmoo_util_rtrace:hotrace(baseKB:to_addable_form(hybrid_support(prologHybrid,1),_G50450))
Action (h for help) ?
continue
^CAction (h for help) ? goals
[306] [logicmoo_util_rtrace] logicmoo_util_catch:catchv(mpred_pfc:cnotrace(fix_mp(baseKB:mpred_is_tracing_pred((ftInt(_G52182)==>{ignore(retract(isa(_G52182,ftInt)))})),_G53158:_G53159)),_G53202,(wdmsg(on_x_rtrace(_G53202)),catchv(rtrace(with_skip_bugger(mpred_pfc:cnotrace(fix_mp(baseKB:mpred_is_tracing_pred((ftInt(_G52182)==>{ignore(retract(isa(_G52182,ftInt)))})),_G53158:_G53159)))),_G53202,wdmsg(_G53202)),dumptrace(mpred_pfc:cnotrace(fix_mp(baseKB:mpred_is_tracing_pred((ftInt(_G52182)==>{ignore(retract(isa(_G52182,ftInt)))})),_G53158:_G53159)))))
[305] [logicmoo_util_rtrace] logicmoo_util_rtrace:on_x_rtrace(mpred_pfc:cnotrace(fix_mp(baseKB:mpred_is_tracing_pred((ftInt(_G52182)==>{ignore(retract(isa(_G52182,ftInt)))})),_G53158:_G53159)))
[304] [logicmoo_util_catch] '<meta-call>'((on_x_rtrace(mpred_pfc:cnotrace(fix_mp(baseKB:mpred_is_tracing_pred((ftInt(_G52182)==>{ignore(retract(isa(_G52182,ftInt)))})),_G53158:_G53159)))*->true;debugCallWhy(failed(on_f_debug(mpred_pfc:cnotrace(fix_mp(baseKB:mpred_is_tracing_pred((ftInt(_G52182)==>{ignore(retract(isa(_G52182,ftInt)))})),_G53158:_G53159)))),mpred_pfc:cnotrace(fix_mp(baseKB:mpred_is_tracing_pred((ftInt(_G52182)==>{ignore(retract(isa(_G52182,ftInt)))})),_G53158:_G53159)))))
[303] [logicmoo_util_catch] logicmoo_util_catch:must(mpred_pfc:cnotrace(fix_mp(baseKB:mpred_is_tracing_pred((ftInt(_G52182)==>{ignore(retract(isa(_G52182,ftInt)))})),_G53158:_G53159)))
[302] [mpred_pfc] lookup_u(baseKB:mpred_is_tracing_pred((ftInt(_G52182)==>{ignore(retract(isa(_G52182,ftInt)))})),_G53246)
Action (h for help) ?
continue


