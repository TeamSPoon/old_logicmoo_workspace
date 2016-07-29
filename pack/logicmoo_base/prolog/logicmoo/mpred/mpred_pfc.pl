/* Part of LogicMOO Base Logicmoo Debug Tools
% ===================================================================
% File '$FILENAME.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: '$FILENAME.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% Licience: LGPL
% ===================================================================
*/

%:- if(((current_prolog_flag(xref,true),current_prolog_flag(pldoc_x,true));current_prolog_flag(autoload_logicmoo,true))).
:- module(mpred_pfc, [
  ensure_abox/1,
  mpred_call_no_bc/1,%fix_mp/3,
  fix_mp/4, %fix_mp/3,
  mpred_fwc/1,
  get_mpred_is_tracing/1,
  show_if_debug/1,
  full_transform/3,
  maybe_mpred_break/1,
  each_E/3,
  call_m_g/3,
  same_modules/2,
  throw_depricated/0,
  lookup_m_g/3,
  head_to_functor_name/2,
  mpred_post1_rem/2,
  mpred_post1_rem1/2,
  fwc1s_post1s/2,
  mpred_mark_as_ml/3,
  mpred_mark_fa_as/5,
  %mpred_te/0,
  %mpred_te/2,
  '__aux_maplist/2_call+0'/1,
  log_failure/1,
  code_sentence_op/1,
  mnotrace/1,
  build_consequent/3,
  with_fc_mode/2,
  filter_buffer_n_test/3,
  filter_buffer_get_n/3,
  filter_buffer_trim/2,
  plus_fwc/0,
  plus_fwc/1,
  cut_c/0,
  to_u/2,
  fresh_mode/0,
  mpred_mark_as/3,
  get_first_user_reason/2,
  assert_u_confirm_if_missing/1,
  assert_u_confirmed_was_missing/1,
  mpred_notrace_exec/0,
  remove_negative_version/1,
  listing_u/1,
  

  '=@@='/2,
  op(700,xfx,'=@@='),
  get_source_ref/1,
  get_source_ref1/1,
  get_source_ref10/1,
  ensure_abox/1, 
  is_source_ref1/1,
  get_source_ref_stack/1,
  set_fc_mode/1,
  
  with_no_mpred_breaks/1,
  mpred_remove1/2,
  check_never_assert/1,check_never_retract/1,
  oinfo/1,
  why_was_true/1,
  mpred_fwc0/1,
  with_no_mpred_trace_exec/1,
  mpred_set_default/2,
  mpred_ain/1,mpred_ain/1,mpred_ain/2,
  action_is_undoable/1,
  mpred_assumption/1,mpred_assumptions/2,mpred_axiom/1,bagof_or_nil/3,bases_union/2,brake/1,build_rhs/3,
  mpred_BC_CACHE0/2,
  build_neg_test/4,build_rule/3,build_code_test/3,
  build_trigger/4,
  defaultmpred_select/1,fc_eval_action/2,
  foreachl_do/2,get_next_fact/1,
  justification/2,justifications/2,
  call_u/1,
  variant_u/2,
  mpred_BC_CACHE/2,
  mpred_call_no_bc/1,mpred_METACALL/2,mpred_METACALL/3,mpred_METACALL/3,
  mpred_halt/0,mpred_halt/1,mpred_halt/2,
  mpred_ain_db_to_head/2,mpred_ain_actiontrace/2,mpred_trace_op/2,mpred_add_support/2,mpred_ain_trigger_reprop/2,
  mpred_ain_by_type/2,
  mpred_prompt_ask/2,
  mpred_METACALL/3,mpred_BC_w_cache/2,
  ain_fast/1,
  ain_fast/2,
  setup_mpred_ops/0,
  mpred_assert_w_support/2,mpred_asserta_w_support/2,mpred_assertz_w_support/2,mpred_basis_list/2,mpred_bt_pt_combine/3,mpred_child/2,mpred_children/2,
  mpred_classifyFacts/4,mpred_collect_supports/1,mpred_unhandled_command/3,mpred_compile_rhs_term/3,mpred_conjoin/3,mpred_neg_connective/1,
  mpred_database_item/1,mpred_database_term/3,
  mpred_db_type/2,mpred_set_default/2,mpred_define_bc_rule/3,mpred_descendant/2,
  mpred_descendants/2,mpred_enqueue/2,mpred_error/1,mpred_error/2,mpred_eval_lhs/2,mpred_eval_lhs_0/2,mpred_eval_rhs/2,mpred_fact/1,
  mpred_fact/2,mpred_facts/1,mpred_facts/2,mpred_facts/3,mpred_fwc/1,mpred_get_support/2,lookup_u/1,lookup_u/2,
  mpred_literal/1,mpred_load/1,mpred_make_supports/1,mpred_ain_object/1,mpred_aina/2,mpred_ainz/2,mpred_aina/1,mpred_ainz/1,
  mpred_negated_literal/1,mpred_unnegate/2,mpred_nf/2,mpred_nf1_negation/2,mpred_nf_negation/2,mpred_nf_negations/2,mpred_notrace/0,mpred_nowatch/0,
  mpred_nospy/0,mpred_nospy/1,mpred_nospy/3,mpred_positive_literal/1,mpred_post/2,pp_qu/0,mpred_undo_action/1,
  mpred_rem_support/2,mpred_remove_old_version/1,mpred_remove_supports/1,mpred_remove_supports_quietly/1,mpred_reset/0,mpred_retract/1,mpred_retract_i_or_warn/1,mpred_retract_supported_relations/1,
  mpred_retract_type/2,mpred_select_justification_node/3,mpred_set_warnings/1,mpred_pp_db_justifications/2,
  mpred_spy/1,mpred_spy/2,mpred_spy/3,mpred_step/0,mpred_support_relation/1,mpred_supported/1,mpred_supported/2,
  mpred_trace/0,mpred_trace/1,mpred_trace/2,mpred_trace_maybe_print/3,mpred_trace_maybe_break/3,mpred_trace_exec/0,mpred_trace_op/3,
  mpred_trace_op/2,mpred_trace_msg/1,mpred_trace_msg/2,mpred_trigger_key/2,mpred_trigger_key/2,mpred_undo/1,mpred_unfwc/1,
  mpred_unfwc_check_triggers/1,mpred_union/3,mpred_unique_u/1,mpred_untrace/0,mpred_untrace/1,mpred_warn/0,mpred_warn/1,
  mpred_warn/2,mpred_watch/0,well_founded_0/2,mpred_why/0,mpred_why/1,mpred_whyBrouse/2,mpred_handle_why_command/3,
  nompred_warn/0,pfcl_do/1,pp_DB/0,pp_db_facts/0,pp_db_facts/1,pp_db_facts/2,pp_db_items/1,
  pp_db_rules/0,pp_db_supports/0,pp_db_triggers/0,mpred_load/1,process_rule/3,
  remove_if_unsupported/1,remove_selection/1,mpred_withdraw1/2,

  mpred_post1/2,get_mpred_assertion_status/3,mpred_post_update4/4,get_mpred_support_status/5,same_file_facts/2,clause_asserted_u/1,
  

  mpred_run/0,mpred_test/1,mpred_test_fok/1,
  fa_to_p/3,
  mpred_call_no_bc/1,
  with_umt/2,
          asserta_u/1,assert_u/1,assertz_u/1,retract_u/1,retractall_u/1,
          retract_u0/1,retractall_u0/1,
  clause_u/1,clause_u/2,clause_u/3,
  clause_i/3,

  lookup_u/1,

mpred_load_term/1,
pos_2_neg/2,
not_not_ignore_mnotrace/1,
mpred_trace_all/0,
really_mpred_mark/4,


unassertable/1,
log_failure_red/0,
convention_to_symbolic_mt/4,
attvar_op_fully/2,
closest_u/2,
pred_check/1,
pp_why/0,
get_consequent_functor/3,
is_user_fact/1,
mpred_retract_i_or_warn_1/1,
mpred_is_silient/0,
pp_why/1,
bad_head_pred/1,
get_mpred_current_db/1,
mpred_call_no_bc0/1,
to_real_mt/3,
all_closed/1,
convention_to_mt/4,

copy_term_vn/2,
remove_meta_wrapper/2,
mpred_undo1/1,
convention_to_symbolic_mt_ec/4,

mpred_retract_i_or_warn_0/1,
push_current_choice/1,



          get_fc_mode/3,mpred_rem_support_if_exists/2,get_tms_mode/2,

  stop_trace/1,with_mpred_trace_exec/1,
  select_next_fact/1,supporters_list/2,triggerSupports/2,well_founded/1,well_founded_list/2,
  do_assumpts/2,mpred_do_fcnt/2,mpred_do_fcpt/2,mpred_fwc1/1,mpred_do_rule/1,mpred_descendant1/3,mpred_eval_rhs1/2,mpred_nf1/2,
  mpred_post1/2,mpred_withdraw/1,mpred_withdraw/2,mpred_remove/1,
  mpred_remove/2,mpred_post1/2,
  mpred_pp_db_justification1/2,mpred_pp_db_justifications2/3,mpred_spy1/3,
  mpred_unfwc_check_triggers0/1,mpred_unfwc1/1,mpred_why1/1,mpred_blast/1
  % trigger_trigger1/2  , trigger_trigger/3,
  ]).
%:- endif.

% :- '$set_source_module'(logicmoo_utils).

:- include('mpred_header.pi').

:- meta_predicate 
      each_E(+,+,+),
      pfcl_do(0),
      pfcl_do(+), % not all arg1s are callable
      lookup_u(+),
      lookup_u(?,?),
      mnotrace(0),
      fix_mp(+,+,-,-),
      clause_asserted_u(+),
      mpred_get_support(+,-),
      mpred_fact(?,0),
      mpred_test(+),
      mpred_test_fok(+),
      mpred_METACALL(1,-,+),
      mpred_METACALL(1,-,+),
      mpred_METACALL(1,+),
      mpred_call_no_bc(+),
      mpred_call_no_bc0(+),
      call_u(+),
      mpred_BC_CACHE(+,+),
      mpred_BC_CACHE0(+,+),
      foreachl_do(0,?), 
      foreachl_do(+,?), % not all arg1s are callable
      with_no_mpred_breaks(0),
      fc_eval_action(0,-),
      clause_u(+,+,-),
      
      clause_u(+,-),
      clause_u(+),      
      with_umt(+,+),
      brake(0),
      with_no_mpred_trace_exec(0),
      with_mpred_trace_exec(0),
      with_fc_mode(+,0),
      bagof_or_nil(?,^,-).

:- current_prolog_flag(unsafe_speedups,_)->true;set_prolog_flag(unsafe_speedups,true).
:- meta_predicate mpred_retract_i_or_warn(+).
:- meta_predicate mpred_retract_i_or_warn_0(+).
:- meta_predicate mpred_retract_i_or_warn_1(+).
:- meta_predicate not_not_ignore_mnotrace(+).

:- module_transparent lookup_u/1,lookup_u/2,mpred_unfwc_check_triggers0/1,mpred_unfwc1/1,mpred_why1/1,mpred_blast/1.


system:must_notrace_pfc(G):- must(quietly(G)).

/*

  ?- dynamic(f2/2),gensym(nnn,N),sanity_attvar_08:attr_bind([name_variable(A, 'ExIn'), form_sk(A, 'SKF-66')], true),
   IN=f2(N,A),OUT=f2(N,B),copy_term_vn(IN,OUT),
  asserta_u(IN),clause_asserted_u(OUT),!. % ,nl,writeq(A=@=B).
*/

%% mpred_database_term(:PI, -TYPE) is nondet.
%
% is true iff F/A is something that Pfc adds to
% the database and should not be present in an empty Pfc database
%


:- dynamic(mpred_database_term/3).
% mined from program database      


mpred_database_term(do_and_undo,2,rule).
mpred_database_term(('::::'),2,rule).
mpred_database_term((<-),2,rule).
mpred_database_term((<==>),2,rule).
mpred_database_term((==>),2,rule).

mpred_database_term((==>),1,fact(_)).
mpred_database_term((~),1,fact(_)).

% forward,backward chaining database
mpred_database_term(spft,3,support).
mpred_database_term(nt,3,trigger).
mpred_database_term(pt,2,trigger).
mpred_database_term(bt,2,trigger).

% transient state
mpred_database_term(actn,1,state).
mpred_database_term(que,1,state).
mpred_database_term(hs,1,state).

% forward,backward settings
mpred_database_term(mpred_current_db,1,setting).
mpred_database_term(mpred_select_hook,1,setting).
mpred_database_term(tms,1,setting).
mpred_database_term(pm,1,setting). 

% debug settings
mpred_database_term(mpred_is_tracing_exec,0,debug).
mpred_database_term(mpred_is_spying_pred,2,debug).
mpred_database_term(mpred_warnings,1,debug).
mpred_database_term(why_buffer,2,debug).

get_head_term(Form,Form):-var(Form),!.
get_head_term(F/A,Form):- integer(A),functor(Form0,F,A),!,get_consequent(Form0,Form).
get_head_term(Form0,Form):- get_consequent(Form0,Form).

% % :- '$set_source_module'(mpred_pfc).

:- op(700,xfx,'=@@=').

'=@@='(A,B):-variant_u(A,B).

% :- use_module(library(logicmoo_utils)).

:- module_transparent((assert_u_confirmed_was_missing/1,mpred_trace_exec/0,pfcl_do/1,
  mpred_post1/2,get_mpred_assertion_status/3,mpred_post_update4/4,get_mpred_support_status/5,same_file_facts/2,foreachl_do/2,
                       asserta_u/1,assert_u/1,assertz_u/1,retract_u/1,retractall_u/1,
                       
                       retract_u0/1,retractall_u0/1,
  mpred_trace_op/3)).

:- thread_local(t_l:no_mpred_breaks/0).


user:current_abox(M):-prolog_load_context(module,user:M).
system:current_abox(M):-prolog_load_context(module,system:M).

:- module_transparent((ensure_abox)/1).
:- multifile(lmcache:has_pfc_database_preds/1).
:- volatile(lmcache:has_pfc_database_preds/1).
:- dynamic(lmcache:has_pfc_database_preds/1).
ensure_abox(M):- sanity(atom(M)), lmcache:has_pfc_database_preds(M),!.
ensure_abox(user):- setup_module_ops(user),!,ensure_abox(baseKB),!.
ensure_abox(M):- 
 must_det_l((
   asserta(lmcache:has_pfc_database_preds(M)),
   assert_if_new(baseKB:mtCycL(M)),
   retractall(baseKB:mtProlog(M)),
   assert_if_new(M:current_abox(M)),
   setup_module_ops(M),
   set_prolog_flag(M:unknown,error),
   forall(mpred_database_term(F,A,_),
       must_det_l(((
        M:multifile(M:F/A),
        M:dynamic(M:F/A),
        M:discontiguous(M:F/A),
        create_predicate_istAbove(M,F,A),        
        M:module_transparent(M:F/A))))))),!.


mnotrace(G):- (no_trace(G)),!.

% =================================================
% ==============  UTILS BEGIN        ==============
% =================================================
% copy_term_vn(A,A):- current_prolog_flag(unsafe_speedups,true),!.
copy_term_vn(B,A):- ground(B),!,A=B.
copy_term_vn(B,A):- !,copy_term(B,A).
copy_term_vn(B,A):- need_speed,!,copy_term(B,A).
copy_term_vn(B,A):- get_varname_list(Vs),length(Vs,L),L<30, shared_vars(B,Vs,Shared),Shared\==[],!,copy_term(B+Vs,A+Vs2),append(Vs,Vs2,Vs3),set_varname_list(Vs3),!.
copy_term_vn(B,A):- nb_current('$old_variable_names',Vs),length(Vs,L),L<30, shared_vars(B,Vs,Shared),Shared\==[],!,copy_term(B+Vs,A+Vs2),append(Vs,Vs2,Vs3),b_setval('$old_variable_names',Vs3),!.
copy_term_vn(B,A):- copy_term(B,A).

setup_mpred_ops:-
          op(500,fx,'-'),
          op(300,fx,'~'),
          op(1050,xfx,('==>')),
          op(1050,xfx,'<==>'),
          op(1050,xfx,('<-')),
          op(1100,fx,('==>')),
          op(1150,xfx,('::::')),
          op(500,fx,user:'-'),
          op(300,fx,user:'~'),
          op(1050,xfx,(user:'==>')),
          op(1050,xfx,user:'<==>'),
          op(1050,xfx,(user:'<-')),
          op(1100,fx,(user:'==>')),
          op(1150,xfx,(user:'::::')).
:- setup_mpred_ops.


%% get_source_ref( :TermU) is det.
%
% Get Source Ref (Current file or User)
%
:- module_transparent((get_source_ref)/1).
get_source_ref(O):- cnotrace((current_why(U),(U=(_,_)->O=U;O=(U,ax)))),!.
get_source_ref(O):- cnotrace((get_source_ref1(U),(U=(_,_)->O=U;O=(U,ax)))),!.

get_source_ref_stack(O):- findall(U,current_why(U),Whys),Whys\==[],!, U=(_,_),(Whys=[U]->O=U;O=(Whys,ax)),!.
get_source_ref_stack(O):- get_source_ref1(U),(U=(_,_)->O=U;O=(U,ax)),!.

is_user_fact((_,U)):-atomic(U).

get_first_user_reason(P,(F,T)):-
  UU=(F,T),
  (((lookup_u(spft(P,F,T))),is_user_fact(UU))*-> true;
    (((lookup_u(spft(P,F,T))), \+ is_user_fact(UU))*-> true ; 
       (clause_asserted_u(P),get_source_ref(UU),is_user_fact(UU)))),!.
get_first_user_reason(P,UU):-get_source_ref_stack(UU),is_user_fact(UU),!.
get_first_user_reason(P,UU):-must(get_source_ref_stack(UU)),!.
%get_first_user_reason(_,UU):- get_source_ref(UU),\+is_user_fact(UU). % ignore(get_source_ref(UU)).

%% get_source_ref1(+Mt) is semidet.
%
% Get Source Ref Secondary Helper.
%
:- module_transparent((get_source_ref1)/1).
:- module_transparent((get_source_ref10)/1).
% get_source_ref1(M):- atom(M),must((get_source_ref10(N),atom(N))),!,M=N.
get_source_ref1(M):- ground(M),!.
get_source_ref1(M):- get_source_ref10(M),!.
get_source_ref1(_).

get_source_ref10(M):- current_why(M), nonvar(M) , M =mfl(_,_,_).
get_source_ref10(mfl(M,F,L)):- defaultAssertMt(M), source_location(F,L).

get_source_ref10(mfl(M,F,L)):- defaultAssertMt(M), current_source_file(F:L).
get_source_ref10(mfl(M,F,_L)):- defaultAssertMt(M), current_source_file(F).
get_source_ref10(mfl(M,_F,_L)):- defaultAssertMt(M).
%get_source_ref10(M):- (defaultAssertMt(M)->true;(atom(M)->(module_property(M,class(_)),!);(var(M),module_property(M,class(_))))).
get_source_ref10(M):- fail,dtrace, 
 ((defaultAssertMt(M) -> !;
 (atom(M)->(module_property(M,class(_)),!);
    mpred_error(no_source_ref(M))))).

is_source_ref1(_).

unassertable(Var):-var(Var),!.
unassertable((M:V)):-nonvar(M),!,unassertable(V).
unassertable((_;_)).
unassertable((_,_)).

:- style_check(+discontiguous).

to_real_mt(_Why,abox,ABOX):- defaultAssertMt(ABOX),!.
to_real_mt(_Why,tbox,TBOX):- get_current_default_tbox(TBOX),!.
to_real_mt(_Why,BOX,BOX).

%% fix_mp(+Why,+I,-O) is det.
%
% Ensure modules are correct when asserting/calling information into the correct MTs
%
%fix_mp(Why,I,UO):- compound(UO),dtrace,UO=(U:O),!,quietly_must(fix_mp(Why,I,U,O)).
% fix_mp(Why,I,MT:UO):- current_prolog_flag(unsafe_speedups,true), !, strip_module(I,_,UO),defaultAssertMt(MT).
fix_mp(Why,I,UO):- quietly_must(fix_mp(Why,I,U,O)),maybe_prepend_mt(U,O,UO).


fix_mp(Why,G,M,GOO):-
  fix_mp0(Why,G,M,GO),strip_module(GO,_,GOO).


fix_mp0(Nonvar,Var,ABox,VarO):- sanity(nonvar(Nonvar)), is_ftVar(Var),!,Var=VarO,defaultAssertMt(ABox),!.
fix_mp0(Why, '~'(G0), M, '~'(CALL)):-nonvar(G0),!,fix_mp0(Why,G0,M,CALL).
fix_mp0(Why,'?-'(G0),M, '?-'(CALL)):-nonvar(G0),!,fix_mp0(Why,G0,M,CALL).
fix_mp0(Why,':-'(G0),M, ':-'(CALL)):-nonvar(G0),!,fix_mp0(Why,G0,M,CALL).
fix_mp0(Why,(G :- B),M,( GO :- B)):- !, fix_mp0(Why,G,M,GO).
fix_mp0(Why,_:(G :- B),M,( GO :- B)):- !, fix_mp0(Why,G,M,GO).
fix_mp0(_Why,Mt:P,Mt,P):- clause_b(mtCycL(Mt)),!.
fix_mp0(_Why,Mt:P,Mt,P):- clause_b(mtExact(Mt)),!.
fix_mp0(_Why,P,S,GO):- predicate_property(P,imported_from(S)),!,strip_module(P,_,GO).
fix_mp0(Why,M:P,MT,P):- to_real_mt(Why,M,MT)->M\==MT,!.
fix_mp0(Why,G,M,GO):- strip_module(G,_,GO),get_consequent_functor(GO,F,A),loop_check(convention_to_mt(Why,F,A,M),fail),!.

fix_mp0(_Why,I,ABox,I):- defaultAssertMt(ABox),!.

/*
fix_mp(Why,Junct,ABox,Result):- fail, (mpred_db_type(Junct,rule);(functor(Junct,F,_),bad_head_pred(F))),!,
   must((mpred_rule_hb(Junct,HC,BC),nonvar(HC))),
   Junct=..[F|List],
   must_maplist(fix_mp(call(hb(HC,BC,Op))),List,ListO),
   Result=..[F|ListO],
   defaultAssertMt(ABox),!.

%fix_mp(call(hb(HC,_BC,Op)),H,M,HH):- contains_var(H,HC),!,
%   fix_mp(clause(assert,Op),H,M,HH).

fix_mp(call(hb(_HC,BC,Op)),B,M,BB):- contains_var(B,BC),B\=@=BC,!,
   fix_mp(call(Op),B,M,BB).



% fix_mp(Why,Unassertable,_,_):- Why = clause(_,_), unassertable(Unassertable),!,trace_or_throw(unassertable_fix_mp(Why,Unassertable)).

*/



convention_to_mt(Why,F,A,RealMt):-convention_to_symbolic_mt_ec(Why,F,A,Mt),to_real_mt(Why,Mt,RealMt).


get_consequent_functor(G,F,A):- strip_module(G,_,GO),remove_meta_wrapper(GO,Unwrap),nonvar(Unwrap),functor(Unwrap,F,A),!.

remove_meta_wrapper(Head,Unwrap):- is_ftVar(Head),!,Head=Unwrap.
remove_meta_wrapper( Head,UnwrapO):- fail, mpred_rule_hb(Head,Unwrap,_),nonvar(Unwrap),
  Head \=@= Unwrap,!,remove_meta_wrapper2(Unwrap,UnwrapO).
remove_meta_wrapper( ( Head :- _ ),Unwrap):- nonvar(Head), !, remove_meta_wrapper2(Head,Unwrap).
remove_meta_wrapper(Head,Unwrap):- strip_module(Head,_,HeadM),Head\=@=HeadM,!,remove_meta_wrapper(HeadM,Unwrap).
remove_meta_wrapper(Head,Unwrap):- remove_meta_wrapper2(Head,Unwrap).

remove_meta_wrapper2(Head,Unwrap):- strip_module(Head,_,HeadM),Head\=@=HeadM,!,remove_meta_wrapper2(HeadM,Unwrap).
remove_meta_wrapper2(~ Head,Unwrap):- nonvar(Head),!, remove_meta_wrapper(Head,Unwrap).
remove_meta_wrapper2( \+ Head,Unwrap):- nonvar(Head),!, remove_meta_wrapper(Head,Unwrap).
remove_meta_wrapper2( ( _,Head),Unwrap):-nonvar(Head),!, remove_meta_wrapper(Head,Unwrap).
remove_meta_wrapper2(Unwrapped,Unwrapped).

bad_head_pred([]).
bad_head_pred('[]').
bad_head_pred('{}').
bad_head_pred('[|]').
bad_head_pred(',').
bad_head_pred(':').
bad_head_pred(':-').
bad_head_pred(';').
bad_head_pred('~').

% the next line transforms to mpred_pfc:convention_to_symbolic_mt(_Why,A, _, B) :- call(ereq, predicateConventionMt(A, B)), !.

% convention_to_symbolic_mt_ec(Why,F,A,Error):- bad_head_pred(F),!,trace_or_throw(error_convention_to_symbolic_mt(Why,F,A,Error)).
convention_to_symbolic_mt_ec(Why,F,A,Mt):-convention_to_symbolic_mt(Why,F,A,Mt).

convention_to_symbolic_mt(_Why,predicateConventionMt,2,baseKB):-!.
convention_to_symbolic_mt(_Why,genlMt,2,baseKB):-!.
convention_to_symbolic_mt(_Why,mtCycL,1,baseKB):-!.
convention_to_symbolic_mt(_Why,mtProlog,1,baseKB):-!.
convention_to_symbolic_mt(_Why,functorDeclares,1,baseKB):-!.
convention_to_symbolic_mt(_Why,prologMacroHead,1,baseKB):-!.
convention_to_symbolic_mt(_Why,F,A,abox):- mpred_database_term(F,A,_).
convention_to_symbolic_mt(_Why,F,_,Mt):-  call_u(predicateConventionMt(F,Mt)),!.
convention_to_symbolic_mt(_Why,F,A,abox):- baseKB:wrap_shared(F,A,ereq).
% convention_to_symbolic_mt(_Why,_,_,M):- atom(M),!.

full_transform(Why,MH,MHH):-
   must(fully_expand(clause(Why,full_transform),MH,MHH)),!,
   sanity(same_modules(MH,MHH)).

same_modules(MH,MHH):- strip_module(MH,HM,_),strip_module(MHH,HHM,_),!,
   HM==HHM.

%:- if(\+ current_prolog_flag(umt_local,false)).

listing_u(P):-mpred_call_no_bc(listing(P)).

attvar_op_fully(Why,MH):- !, attvar_op(Why,MH).
attvar_op_fully(Why,M:H):- must_notrace_pfc(fully_expand(change(Why,attvar_op_fully),H,HH)),!,each_E(attvar_op(Why),M:HH,[]).
attvar_op_fully(Why,MH):- full_transform(Why, MH,MHH),each_E(attvar_op(Why),MHH,[]).

throw_depricated:- trace_or_throw(throw_depricated).

assert_u(MH):- assert_u_no_dep(MH).
assert_u_no_dep(MH):- fix_mp(clause(assert,assert_u),MH,MHA),
    attvar_op_fully(assert_i, MHA),expire_tabled_list(H).
asserta_u(MH):- throw_depricated, fix_mp(clause(assert,asserta_u),MH,MHA),attvar_op_fully(asserta_i,MHA).
assertz_u(MH):- throw_depricated, fix_mp(clause(assert,assertz_u),MH,MHA),attvar_op_fully(assertz_i,MHA).
retract_u(H):- retract_u0(H) *-> true; attvar_op_fully(retract_u0,H).

retract_u0(H0):- strip_module(H0,_,H),(H = ( \+ _ )),!,trace_or_throw(mpred_warn(retract_u(H0))),expire_tabled_list(H).
retract_u0(M:(H:-B)):- atom(M),!, clause_u(H,B,R),erase(R),expire_tabled_list(H).
retract_u0((H:-B)):-!,clause_u(H,B,R),erase(R),expire_tabled_list(H).
retract_u0(H):- clause_u(H,true,R),erase(R),expire_tabled_list(H).


retractall_u(H):- attvar_op_fully(retractall_u0,H).
retractall_u0(H):- forall(clause_u(H,_,R),erase(R)),expire_tabled_list(H).



clause_u(C):- expand_to_hb(C,H,B),!,clause_u(H,B).


%% clause_u( ?H, ?B) is semidet.
clause_u(H,B):-  current_prolog_flag(unsafe_speedups,true), ground(H:B),!,clause(H,B).
clause_u(H,B):- clause_u(H,B,_).
%clause_u(H,B):- clause_true( ==>( B , H) ).
%clause_u(H,B):- clause_true( <-( H , B) ).

%% clause_u( +H, ?B, ?Why) is semidet.
%
% PFC Clause.
%
clause_u(MH,B,R):- nonvar(R),!,must(clause_i(M:H,B,R)),must((MH=(M:H);MH=(H))),!.
clause_u(H,B,Ref):-var(H),!,trace_or_throw(var_clause_u(H,B,Ref)).
clause_u((H:-BB),B,Ref):- is_true(B),!,clause_u(H,BB,Ref).
clause_u((H:-B),BB,Ref):- is_true(B),!,clause_u(H,BB,Ref).
clause_u(MH,B,R):- Why = clause(clause,clause_u),
 ((mnotrace(fix_mp(Why,MH,M,H)),
  clause(M:H,B,R))*->true;
           (fix_mp(Why,MH,M,CALL)->clause_i(M:CALL,B,R))).
% clause_u(H,B,Why):- has_cl(H),clause_u(H,CL,R),mpred_pbody(H,CL,R,B,Why).
%clause_u(H,B,backward(R)):- R=(<-(H,B)),clause_u(R,true).
%clause_u(H,B,equiv(R)):- R=(<==>(LS,RS)),clause_u(R,true),(((LS=H,RS=B));((LS=B,RS=H))).
%clause_u(H,true, pfcTypeFull(R,Type)):-is_ftNonvar(H),!,pfcDatabaseTerm(F/A),make_functor(R,F,A),pfcRuleOutcomeHead(R,H),clause(R,true),pfcTypeFull(R,Type),Type\=rule.
%clause_u(H,true, pfcTypeFull(R)):-pfcDatabaseTerm(F/A),make_functor(R,F,A),pfcTypeFull(R,Type),Type\=rule,clause(R,true),once(pfcRuleOutcomeHead(R,H)).
%clause_u('nesc'(H),B,forward(Proof)):- is_ftNonvar(H),!, clause_u(H,B,Proof).
%clause_u(H,B,forward(R)):- R=(==>(B,H)),clause_u(R,true).

%% clause_u( +VALUE1, ?H, ?B, ?Proof) is semidet.
%
% Hook To [baseKB:clause_u/4] For Module Mpred_pfc.
% PFC Provide Storage Clauses.
%
%clause_u(pfc,H,B,Proof):-clause_u(H,B,Proof).

lookup_u(SPFT):- current_prolog_flag(unsafe_speedups,true), !,baseKB:mtCycL(MT),call(MT:SPFT).
lookup_u(H):-lookup_u(H,_).


lookup_u(MH,Ref):- nonvar(Ref),!,
                   must(clause(H,B,Ref)),
                   must(hb_to_clause(H,B,MHI)),!,
                   MH=MHI.

lookup_u((MH,H),Ref):- nonvar(MH),!,lookup_u(MH),lookup_u(H,Ref).
lookup_u(MH,Ref):- clause_u(MH,true,Ref).

/*
lookup_u(MH,Ref):- must(mnotrace(fix_mp(Why,MH,M,H))), 
                    on_x_debug(clause_u(M:H,B,Ref)),
                        (var(B)->rtrace(clause_u(M:H,_,Ref));true),
                        on_x_debug(B).
*/


:- was_module_transparent(with_umt/2).
:- was_export(with_umt/2).
%% with_umt( +ABOX, ?G) is semidet.
%
% Using User Microtheory.
%

with_umt(mud_telnet,P):- !,with_umt(baseKB,P).
with_umt(U,G):- sanity(stack_check(5000)), 
  (t_l:current_defaultAssertMt(W)->W=U,!,call_from_module(U,G)).
with_umt(user,P):- !,with_umt(baseKB,P).
with_umt(M,P):- 
  (clause_b(mtCycL(M))-> W=M;defaultAssertMt(W)),!,
   w_tl(t_l:current_defaultAssertMt(W),
     call_from_module(W,P)).


%:- else
/*
listing_u(P):- (listing(P)).
assert_u(A):- assert(A).
asserta_u(A):- asserta(A).
assertz_u(A):- assertz(A).
retract_u((H:-B)):-!, clause_u(H,B,R),erase(R).
retract_u(H):-!, clause_u(H,true,R),erase(R).
retractall_u(H):- forall(clause_u(H,_,R),erase(R)).
clause_u(H,B):- clause_u(H,B,_).
clause_u(H,B,R):- clause_i(H,B,R).
mpred_call_no_bc(G):- G.
*/
%:- endif.

%% each_E(+P2,+HT,+S) semidet.
%
% Call P(E,S). each Element in the list.
%
each_E(P,HV,S):- var(HV),!,apply(P,[HV|S]).
each_E(P,M:(H,T),S) :- must_be(atom,M),!,each_E(P,M:H,S), each_E(P,M:T,S).
each_E(P,M:[H],S) :- must_be(atom,M),!,each_E(P,M:H,S).
each_E(P,M:[H|T],S) :- must_be(atom,M),!,each_E(P,M:H,S), each_E(P,M:T,S).
each_E(P,M:HT,S) :- M=='$si$',!,apply(P,[M:HT|S]).
each_E(P,M:HT,S) :- !, must_be(atom,M),M:each_E(P,HT,S).
each_E(P,[H],S) :- !, each_E(P,H,S).
each_E(P,[H|T],S) :- !, each_E(P,H,S), each_E(P,T,S).
each_E(P,(H,T),S) :- !, each_E(P,H,S), each_E(P,T,S).
each_E(P,H,S) :- apply(P,[H|S]).


% =================================================
% ==============  UTILS END          ==============
% =================================================

:- style_check(+singleton).
%   File   : mpred_syntax.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Purpose: syntactic sugar for Pfc - operator definitions and term expansions.

:- op(500,fx,'-').
:- op(300,fx,'~').
:- op(1050,xfx,('==>')).
:- op(1050,xfx,'<==>').
:- op(1050,xfx,('<-')).
:- op(1100,fx,('==>')).
:- op(1150,xfx,('::::')).

:- export('__aux_maplist/2_call+0'/1).
:- meta_predicate('__aux_maplist/2_call+0'(0)).
'__aux_maplist/2_call+0'([]).
'__aux_maplist/2_call+0'([A|B]) :-!,
        call(A),
        '__aux_maplist/2_call+0'(B).
'__aux_maplist/2_call+0'(_:[]).
'__aux_maplist/2_call+0'(M:[A|B]) :-
        M:call(A),
        '__aux_maplist/2_call+0'(M:B).


:- use_module(library(lists)).



%:- dynamic(mpred_te/0).
:- multifile(user:term_expansion/2).
:- dynamic(user:term_expansion/2).
% user:term_expansion(I,O):-mpred_te->mpred_te(I,O).

/*
mpred_te((P==>Q),(:- mpred_ain((P==>Q)))).
%mpred_te((P==>Q),(:- mpred_ain(('<-'(Q,P))))).  % speed-up attempt
mpred_te(('<-'(P,Q)),(:- mpred_ain(('<-'(P,Q))))). 
mpred_te((P<==>Q),(:- mpred_ain((P<==>Q)))).
mpred_te((RuleName :::: Rule),(:- mpred_ain((RuleName :::: Rule)))).
mpred_te((==>P),(:- mpred_ain(P))).
*/

%  predicates to examine the state of mpred_


pp_qu:- mpred_call_no_bc(listing(que/1)).

%   File   : mpred_core.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Updated: 10/11/87, ...
%            4/2/91 by R. McEntire: added calls to valid_dbref as a
%                                   workaround for the Quintus 3.1
%                                   bug in the recorded database.
%   Purpose: core Pfc predicates.


% ============================================
% % initialization of global assertons 
% ============================================

%%   mpred_set_default(P,Q) is det.
%
%  if there is any fact unifying with P, 
% via lookup_u/1 then do 
%  nothing, else assert_u Q.
%
mpred_set_default(GeneralTerm,Default):-
  clause_u(GeneralTerm,true) -> true ; assert_u_no_dep(Default).

%  tms is one of {none,local,cycles} and controles the tms alg.
% :- mpred_set_default(tms(_),tms(cycles)).

% Pfc Propagation strategy. pm(X) where P is one of {direct,depth,breadth}
% :- must(mpred_set_default(pm(_), pm(direct))).



%% mpred_ainz(+G, ?S) is semidet.
%
% PFC Ainz.
%
mpred_ainz(G):-mpred_ain(G).
mpred_ainz(G,S):-mpred_ain(G,S).

%% mpred_aina(+G, ?S) is semidet.
%
% PFC Aina.
%
mpred_aina(G):- mpred_ain(G).
mpred_aina(G,S):-mpred_ain(G,S).

%%  mpred_ain(P,S) 
%
%  asserts P into the dataBase with support from S.
%
%  mpred_ain/2 and mpred_post/2 are the proper ways to add new clauses into the
%  database and have forward reasoning done.
%
mpred_ain(_:P):- P==end_of_file,!.
mpred_ain(P):- get_source_ref(UU),mpred_ain(P,UU).

%%  mpred_ain(P,S) 
%
%  asserts P into the dataBase with support from S.
%

mpred_ain(MTP,S):- is_ftVar(MTP),!,trace_or_throw(var_mpred_ain(MTP,S)).
mpred_ain(user:MTP,S):- !, must(mpred_ain(MTP,S)).
mpred_ain(user:MTP :-B,S):- !, must(mpred_ain(MTP:-B,S)).

mpred_ain(ToMt:P :- B,(mfl(FromMt,File,Lineno),UserWhy)):- ToMt \== FromMt,
 defaultAssertMt(ABox), ToMt \== ABox,!,
  with_umt(ToMt,(mpred_ain(ToMt:P :- B,(mfl(ToMt,File,Lineno),UserWhy)))).

mpred_ain(ToMt:P,(mfl(FromMt,File,Lineno),UserWhy)):- ToMt \== FromMt,
 defaultAssertMt(ABox), ToMt \== ABox,!,
  with_umt(ToMt,(mpred_ain(ToMt:P,(mfl(ToMt,File,Lineno),UserWhy)))).

mpred_ain(MTP,S):- sanity(stack_check), strip_module(MTP,MT,P),P\==MTP,!,
  with_umt(MT,mpred_ain(P,S)),!.

mpred_ain(MTP :- B,S):- strip_module(MTP,MT,P),P\==MTP,!,
  with_umt(MT,mpred_ain(P :- B,S)),!.

mpred_ain(PIn,S):- 
  must(add_eachRulePreconditional(PIn,P)),
  must(full_transform(ain,P,P0)),!,
  must(ain_fast(P0,S)),!.

mpred_ain(P,S):- mpred_warn("mpred_ain(~p,~p) failed",[P,S]),!.



ain_fast(P):- call_u((( get_source_ref(UU), ain_fast(P,UU)))).

ain_fast(P,S):- maybe_updated_value(P,RP,OLD),subst(S,P,RP,RS),!,ain_fast(RP,RS),ignore(mpred_retract(OLD)).

ain_fast(P,S):- 
  fwc1s_post1s(One,Two),
  filter_buffer_trim('$last_mpred_fwc1s',One),
  filter_buffer_trim('$last_mpred_post1s',Two),
  each_E(mpred_post1,P,[S]),
  mpred_run.

:- dynamic(lmconf:eachRule_Preconditional/1).
lmconf:eachRule_Preconditional(true).
:- dynamic(lmconf:eachFact_Preconditional/1).
lmconf:eachFact_Preconditional(true).

add_eachRulePreconditional(A,A):-var(A),!.
add_eachRulePreconditional(B::::A,B::::AA):-add_eachRulePreconditional(A,AA).
add_eachRulePreconditional(A==>B,AA==>B):-!,add_eachRulePreconditional_now(A,AA).
add_eachRulePreconditional(A<==>B, ('==>'(AA , B) , (BB ==> A)) ):-!,add_eachRulePreconditional_now(A,AA),add_eachRulePreconditional_now(B,BB).
add_eachRulePreconditional((B <- A), (B <- AA)) :-!,add_eachRulePreconditional_now(A,AA).
add_eachRulePreconditional(A,AA):-add_eachFactPreconditional_now(A,AA).

add_eachFactPreconditional_now(A,A):- lmconf:eachFact_Preconditional(true),!.
add_eachFactPreconditional_now(A,(Was==>A)):- lmconf:eachFact_Preconditional(Was),!.

add_eachRulePreconditional_now(A,A):- lmconf:eachRule_Preconditional(true),!.
add_eachRulePreconditional_now(A,(Was,A)):- lmconf:eachRule_Preconditional(Was),!.




remove_negative_version(_P):- current_prolog_flag(unsafe_speedups,true),!.
remove_negative_version((H:-B)):- !,
  % TODO extract_predciates((H:-B),Preds),trust(Preds),
  with_no_mpred_trace_exec((
  once((get_source_ref_stack(S),!,
  must(mpred_ain(\+ (~(H) :- B), S)))))),!.
remove_negative_version(P) :- \+ mpred_non_neg_literal(P),!.

remove_negative_version(P):-
  % TODO extract_predciates(P,Preds),trust(Preds),
  with_no_mpred_trace_exec((
  once((get_source_ref_stack(S),!,
  must(mpred_ain(\+ (~(P)), S)))))),!.

     
fwc1s_post1s(1,2):-!.     
fwc1s_post1s(1,2):- current_prolog_flag(unsafe_speedups,false),!.
    
fwc1s_post1s(100,200):- fresh_mode,!.
fwc1s_post1s(1,2):- current_prolog_flag(pfc_booted,true),!.
% fwc1s_post1s(10,20):- defaultAssertMt(Mt)->Mt==baseKB,!.
fwc1s_post1s(1,2).

fresh_mode :- \+ current_prolog_flag(pfc_booted,true), \+ current_prolog_flag(unsafe_speedups,false).
plus_fwc :- \+ fresh_mode.

plus_fwc(P):- is_ftVar(P),!,trace_or_throw(var_plus_fwc(P)).
plus_fwc(support_hilog(_,_)):-!.
plus_fwc('==>'(_,_)):-!.
plus_fwc(P):- gripe_time(0.6,
  (plus_fwc
    ->
      loop_check_term(must(mpred_fwc(P)),plus_fwc(P),true);true)),!.


maybe_updated_value(UP,R,OLD):-
    compound(UP),
    get_consequent(UP,P),
    compound(P),
    once((arg(N,P,UPDATE),is_relative(UPDATE))),
    replace_arg(P,N,Q_SLOT,Q),
    must(call_u(Q)), update_value(Q_SLOT,UPDATE,NEW), must( \+ is_relative(NEW)),
    replace_arg(Q,N,NEW,R),!,R\=@=UP,subst(UP,P,Q,OLD).




%% mpred_post(+Ps,+S) 
%
% tries to assert a fact or set of fact to the database.  For
% each fact (or the singleton) mpred_post1 is called. It always succeeds.
%
mpred_post(P, S):- fully_expand_now(post,P,P0),each_E(mpred_post1,P0,[S]).


%% mpred_post1(+P,+S) is det.
%
% tries to add a fact to the database, and, if it succeeded,
% adds an entry to the Pfc queue for subsequent forward chaining.
% It always succeeds.
%
mpred_post1( isa(_,_,_),   _):- dumpST,dtrace.
mpred_post1( P,   S):- sanity(nonvar(P)),fixed_negations(P,P0),!, mpred_post1( P0,   S).

mpred_post1(Fact, _):- current_prolog_flag(unsafe_speedups,true), ground(Fact),fwc1s_post1s(One,_Two),Three is One * 3, filter_buffer_n_test('$last_mpred_post1s',Three,Fact),!.

mpred_post1(P,S):- gripe_time(0.6,mpred_post12(P,S)).


:- module_transparent(mpred_post1/2).
:- module_transparent(mpred_post12/2).
:- export(mpred_post12/2).

mpred_post12(P, S):- compound(P), ( \+ P = argsQuoted(_) ), functor(P,F,N), ( F \==  action_rules, F \== (==>) ), (arg(N,P,A);arg(1,P,A)), member(A,['$VAR'('????????????'),'$VAR'(_)])->trace_or_throw(mpred_post1(P, S)).

mpred_post12( \+ P,   S):- nonvar(P), !, must(mpred_post1_rem(P,S)).

% TODO - FIGURE OUT WHY THIS IS NEEDED
mpred_post12( ~ P,   S):- 
   with_current_why(S,with_no_mpred_breaks((nonvar(P),doall(mpred_remove(P,S)),must(mpred_undo(P))))),fail.

mpred_post12(P,S):- maybe_updated_value(P,RP,OLD),!,subst(S,P,RP,RS),mpred_post12(RP,RS),ignore(mpred_retract(OLD)).

% Two versions exists of this function one expects for a clean database (fresh_mode) and adds new information.
% tries to assert a fact or set of fact to the database.
% The other version is if the program is been running before loading this module.
%
mpred_post12(P,S):- fail,
  fresh_mode,!,
  % db mpred_ain_db_to_head(P,P2),
  % mpred_remove_old_version(P),  
 \+ \+ mpred_add_support(P,S),
  ( (\+ mpred_unique_u(P)) -> true ;
  ( assert_u_confirm_if_missing(P),
     !,
     mpred_trace_op(add,P,S),
     !,
     mpred_enqueue(P,S),
     !)),
  plus_fwc(P),!.
  

% this would be for complete repropagation
mpred_post12(P,S):- t_l:is_repropagating(_),!,
 ignore(( %  db mpred_ain_db_to_head(P,P2),
  % mpred_remove_old_version(P),  
  mpred_add_support(P,S),
  (mpred_unique_u(P)->
     assert_u_confirmed_was_missing(P);
     assert_u_confirm_if_missing(P)), 
  mpred_trace_op(add,P,S),
  !,
  mpred_enqueue(P,S))),
  !.

% this would be the very inital by Tim Finnin...
mpred_post12(P,S):- fail, fresh_mode,
 ignore(( %  db mpred_ain_db_to_head(P,P2),
  % mpred_remove_old_version(P),  
  mpred_add_support(P,S),
  mpred_unique_u(P),
  assert_u_confirm_if_missing(P),
  mpred_trace_op(add,P,S),
  !,
  mpred_enqueue(P,S))),
  !.


/*
% Expects a clean database and adds new information.
mpred_post12(P,S):-  fail,!,  
  % db mpred_ain_db_to_head(P,P2),
  % mpred_remove_old_version(P),  
  must( \+ \+ mpred_add_support(P,S)),
  ( \+ mpred_unique_u(P) 
    -> clause_asserted_u(P) 
    ; ( assert_u_confirmed_was_missing(P),
        !,
        mpred_trace_op(add,P,S),
        !,
        mpred_enqueue(P,S),
        !)).
*/
 
% Expects a *UN*clean database and adds new information.
% (running the program is been running before loading this module)
%
%  (gets the status in Support and in Database)
mpred_post12(P,S):- !,

% set_varname_list([]),!,

  copy_term_vn((P,S),(PP,SS)),
  
% checks to see if we have forward chain the knowledge yet or 
 gripe_time(0.6, must(get_mpred_support_status(P,S,PP,SS,Was))),!,
% if we''ve asserted what we''ve compiled      
  gripe_time(0.6, must(get_mpred_assertion_status(P,PP,WasA))),!,
  gripe_time(0.6, must(mpred_post_update4(WasA,P,S,Was))),!.
  

get_mpred_assertion_status(P,PP,WasO):-
  (clause_asserted_u(P)-> Was=identical;
    (lookup_u(PP,Ref)*-> must((lookup_u(HB,Ref),Was= partial(HB)));Was= unique)) ,
  !,WasO=Was.

% The cyclic_break is when we have regressions arouind ~ ~ ~ ~ ~
get_mpred_support_status(P,_S, PP,(FF,TT),Was):- 
  Simular=simular(none),
  dont_make_cyclic((((lookup_u(spft(PP,F,T)),P=@@=PP)) *-> 
     ((TT=@@=T,same_file_facts(F,FF)) -> (Was = exact , ! ) ; (nb_setarg(1,Simular,(FF,TT)),fail))
    ; Was = none) -> true ; ignore(Was=Simular)).


same_file_facts(mfl(M,F,_),mfl(M,FF,_)):-nonvar(M),!, FF=@=F.
same_file_facts(F,FF):- FF=@=F,!.


%% mpred_post_update4(++AssertionStatus, +Ps, +S, ++SupportStatus) is det.
%
% Physically assert the Knowledge+Support Data based on statuses
%
mpred_post_update4(Was,P,S,What):- 
  not_not_ignore_mnotrace(( (get_mpred_is_tracing(P);get_mpred_is_tracing(S)),
  fix_mp(clause(assert,post),P,M,PP),
  must(S=(F,T)),wdmsg(call_mpred_post4:- (Was,post1=M:PP,fact=F,trig=T,What)))),
  fail.

mpred_post_update4(identical,_P,_S,exact):-!.

mpred_post_update4(unique,P,S,none):-!,
  mpred_add_support_fast(P,S),
  assert_u_confirmed_was_missing(P),
  mpred_trace_op(add,P,S),
  mpred_enqueue(P,S).

mpred_post_update4(identical,P,S,none):-!,mpred_add_support_fast(P,S),
    mpred_enqueue(P,S).

mpred_post_update4(identical,P,S,simular(_)):- !,mpred_add_support_fast(P,S).

mpred_post_update4(Was,P,S,What):- 
  not_not_ignore_mnotrace(( \+ (get_mpred_is_tracing(P);get_mpred_is_tracing(S)),
  fix_mp(clause(assert,post),P,M,PP),
  must(S=(F,T)),wdmsg(mpred_post_update4:- (Was,post1=M:PP,fact=F,trig=T,What)))),
  fail.

mpred_post_update4(partial(_Other),P,S,none):-!,
  mpred_add_support_fast(P,S),
  assert_u_confirmed_was_missing(P),
  mpred_trace_op(add,P,S),
  mpred_enqueue(P,S).
  
mpred_post_update4(partial(_Other),P,S,exact):-!,
  assert_u_confirmed_was_missing(P),
  mpred_trace_op(add,P,S),
  mpred_enqueue(P,S).

mpred_post_update4(unique,P,S,exact):-!,
  assert_u_confirmed_was_missing(P),
  mpred_trace_op(add,P,S).


mpred_post_update4(partial(_),P,S,exact):- !,
  assert_u_confirmed_was_missing(P),
  mpred_trace_op(add,P,S).


mpred_post_update4(partial(_),P,S,simular(_)):- !,
  mpred_add_support_fast(P,S),
  ignore((mpred_unique_u(P),assert_u_confirmed_was_missing(P),mpred_trace_op(add,P,S))),
  mpred_enqueue(P,S).

mpred_post_update4(unique,P,S,simular(_)):-!,
  mpred_add_support_fast(P,S),
  assert_u_confirmed_was_missing(P),
  mpred_trace_op(add,P,S),
  mpred_enqueue(P,S).
  

mpred_post_update4(Was,P,S,What):-dmsg(mpred_post_update4(Was,P,S,What)),dtrace,fail.

mpred_post_update4(Was,P,S,What):-!,trace_or_throw(mpred_post_update4(Was,P,S,What)).

/*
assert_u_confirmed_was_missing(P):- once((get_consequent_functor(P,F,_),get_functor(P,FF,_))),
 F==FF,
 call_u(prologSingleValued(F)),!, 
 \+ \+ must((db_assert_sv(P))),
 \+ \+ sanity((clause_asserted_u(P))),!.
*/
assert_u_confirmed_was_missing(P):-
 \+ \+ must(assert_mu(P)),!.
 % sanity( (\+ clause_asserted_u(P)) -> (rtrace(assert_mu(P)),dbreak) ; true),!.

assert_u_confirmed_was_missing(P):- 
 copy_term_vn(P,PP),
 dtrace,must(assert_u_no_dep(P)),!,
(nonvar(PP) -> true ; must((P=@=PP,clause_asserted_u(PP),P=@=PP))),!.


assert_u_confirm_if_missing(P):-
 must(clause_asserted_u(P)-> true ; assert_u_confirmed_was_missing(P)).

%% get_mpred_current_db(-Db) is semidet.
%
% PFC Current Database.
%
% (was nothing)
%
get_mpred_current_db(Db):-lookup_u(mpred_current_db(Db)),!.
get_mpred_current_db(true).
 
%%  mpred_ain_db_to_head(+P,-NewP) is semidet.
%
% takes a fact P or a conditioned fact
%  (P:-C) and adds the Db context.
% 
mpred_ain_db_to_head(P,NewP):-
  lookup_u(mpred_current_db(Db)),
  (Db=true        -> NewP = P;
   P=(Head:-Body) -> NewP = (Head:- (Db,Body));
   otherwise      -> NewP = (P:- Db)).


%% mpred_unique_u(+P) is semidet.
%
% is true if there is no assertion P in the prolog db.
%
mpred_unique_u((Head:-Tail)):- !, \+ clause_u(Head,Tail).
mpred_unique_u(P):- !, \+ clause_u(P,true).


%% get_fc_mode(+P,+S,-Mode) is semidet.
%
% return Mode to forward assertion P in the prolog db.
%
get_fc_mode(_P,_S,Mode):- t_l:mpred_fc_mode(Mode),!.
get_fc_mode(_P,_S,Mode):- lookup_u(pm(Mode)),!.
get_fc_mode(_P,_S,Mode):- must(Mode=direct),!.


:- thread_local(t_l:mpred_fc_mode/1).

%% with_fc_mode(+Mode,:Goal) is semidet.
%
% Temporariliy changes to forward chaining propagation mode while running the Goal
%
with_fc_mode(Mode,Goal):- w_tl(t_l:mpred_fc_mode(Mode),((Goal))).

set_fc_mode(Mode):- asserta(t_l:mpred_fc_mode(Mode)).

%% mpred_enqueue(+P,+S) is det.
%
% PFC Enqueue P for forward chaining
%

% mpred_enqueue(P,S):- get_fc_mode(P,S,Mode), must(Mode=direct),fail.
mpred_enqueue(P,S):-
 ( (must(get_fc_mode(P,S,Mode))
    -> (Mode=direct  -> loop_check_term(mpred_fwc(P),mpred_enqueueing(P),true) ;
	Mode=depth   -> mpred_asserta_w_support(que(P),S) ;
	Mode=breadth -> mpred_assert_w_support(que(P),S) ;
	true         -> mpred_error("Unrecognized pm mode: ~p", Mode))
     ; mpred_error("No pm mode"))),!.


%% mpred_remove_old_version( :TermIdentifier) is semidet.
%
% if there is a rule of the form Identifier ::: Rule then delete it.
%
mpred_remove_old_version((Identifier::::Body)):-
  % this should never happen.
  var(identifier),
  !,
  mpred_warn("variable used as an  rule name in ~p :::: ~p",
          [Identifier,Body]).

  
mpred_remove_old_version((Identifier::::Body)):-
  nonvar(Identifier),
  clause_u((Identifier::::OldBody),_),
  \+(Body=OldBody),
  mpred_withdraw((Identifier::::OldBody)),
  !.
mpred_remove_old_version(_).



% mpred_run compute the deductive closure of the current database. 
% How this is done depends on the propagation mode:
%    direct -  mpred_fwc has already done the job.
%    depth or breadth - use the queue mechanism.

mpred_run :- lookup_u(pm(Mode)),Mode=direct,!.
% mpred_run :- repeat, \+ mpred_step, !.
mpred_run:-
%  (\+ lookup_u(pm(direct))),
  mpred_step,
  mpred_run.
mpred_run.


% mpred_step removes one entry from the queue and reasons from it.


mpred_step:-  
  % if hs/1 is true, reset it and fail, thereby stopping inferencing. (hs=halt_signal)
  mnotrace((lookup_u(hs(Was)))),
  mpred_retract(hs(Was)),
  mpred_trace_msg('Stopping on: ~p',[hs(Was)]),
  !, 
  fail.

mpred_step:-
  % draw immediate conclusions from the next fact to be considered.
  % fails iff the queue is empty.
  mnotrace(get_next_fact(P)),
  pfcl_do(mpred_fwc(P)),
  !.

get_next_fact(P):-
  %identifies the nect fact to mpred_fwc from and removes it from the queue.
  select_next_fact(P),
  remove_selection(P).

remove_selection(P):-
  lookup_u(que(P),Ref),
  erase(Ref),
  % must(mpred_retract(que(P))),
  mpred_remove_supports_quietly(que(P)),
  !.
remove_selection(P):-
  brake(format("~Nmpred_:get_next_fact - selected fact not on Queue: ~p",
               [P])).


% select_next_fact(P) identifies the next fact to reason from.  
% It tries the user defined predicate first and, failing that, 
%  the default mechanism.

select_next_fact(P):- 
  lookup_u(mpred_select_hook(P)),
  !.  
select_next_fact(P):- 
  defaultmpred_select(P),
  !.  

% the default selection predicate takes the item at the froint of the queue.
defaultmpred_select(P):- lookup_u(que(P)),!.

% mpred_halt stops the forward chaining.
mpred_halt:-  mpred_halt(anonymous(mpred_halt)).

mpred_halt(Format,Args):- format_to_message(Format,Args,Info), mpred_halt(Info).

mpred_halt(Now):-
  mpred_trace_msg("New halt signal ",[Now]),
  (lookup_u(hs(Was)) -> 
       mpred_warn("mpred_halt finds halt signal already set to: ~p ",[Was])
     ; assert_u_no_dep(hs(Now))).


% stop_trace(Info):- mnotrace((tracing,leash(+all),dtrace(dmsg(Info)))),!,rtrace.
stop_trace(Info):- dtrace(dmsg(Info)).

with_no_mpred_breaks(G):-
  w_tl(t_l:no_mpred_breaks,G).

%% mpred_ain_trigger_reprop(+Trigger,+Support) is nondet.
%
%  Assert New Trigger and Propigate
% 
mpred_ain_trigger_reprop(PT,Support):-
  PT = pt(Trigger,Body),!,
  mpred_mark_as(Support,Trigger,pfcPosTrigger),
  mpred_trace_msg('~N~n\tAdding positive~n\t\ttrigger: ~p~n\t\tbody: ~p~n\t Support: ~p~n',[Trigger,Body,Support]),
  sanity(\+ string(Support)),
  sanity(\+ string(Trigger)),
  sanity(\+ string(Body)),
  %  (debugging(foo)->dtrace;true),
  mpred_assert_w_support(PT,Support),
  copy_term(PT,Tcopy),
  mpred_call_no_bc(Trigger),
  mpred_eval_lhs(Body,(Trigger,Tcopy)),
  fail.

mpred_ain_trigger_reprop(nt(Trigger,Test,Body),Support):-
  copy_term_vn(Trigger,TriggerCopy),
  NT = nt(TriggerCopy,Test,Body),!,
  mpred_mark_as(Support,Trigger,pfcNegTrigger),
  mpred_trace_msg('~N~n\tAdding negative~n\t\ttrigger: ~p~n\t\ttest: ~p~n\t\tbody: ~p~n\t Support: ~p~n',[Trigger,Test,Body,Support]),  
  mpred_assert_w_support(NT,Support),
  %stop_trace(mpred_assert_w_support(NT,Support)),
  \+ mpred_call_no_bc(Test),
  mpred_eval_lhs(Body,((\+Trigger),NT)).

mpred_ain_trigger_reprop(BT,Support):- 
  BT = bt(Trigger,Body),!,
  ain_fast((Trigger:-mpred_bc_only(Trigger))),
  mpred_mark_as(Support,Trigger,pfcBcTrigger),
  % if_defined(kb_dynamic(Trigger),true), 
  mpred_trace_msg('~N~n\tAdding backwards~n\t\ttrigger: ~p~n\t\tbody: ~p~n\t Support: ~p~n',[Trigger,Body,Support]),
  mpred_assert_w_support(BT,Support),
  mpred_bt_pt_combine(Trigger,Body,Support).

mpred_ain_trigger_reprop(X,Support):-
  mpred_warn("Unrecognized trigger to mpred_ain_trigger_reprop: ~p\n~~p~n",[X,Support]).


mpred_bt_pt_combine(Head,Body,Support):- 
  %  a backward trigger (bt) was just added with head and Body and support Support
  %  find any pt''s with unifying heads and add the instantied bt body.
  lookup_u(pt(Head,Body)),
  mpred_eval_lhs(Body,Support),
  fail.
mpred_bt_pt_combine(_,_,_):- !.



% 
%  predicates for manipulating action traces.
% 

mpred_ain_actiontrace(Action,Support):- 
  % adds an action dtrace and it''s support.
  mpred_add_support(actn(Action),Support).

mpred_undo_action(actn(A)):-
  lookup_u(do_and_undo(A,M)),
  mpred_call_no_bc(M),
  !.


%%  mpred_retract(X) is det.
%
%  predicates to remove Pfc facts, triggers, action traces, and queue items
%  from the database.
%
mpred_retract(X):- 
  %  retract an arbitrary thing.
  mpred_db_type(X,Type),!,
  mpred_retract_type(Type,X),
  !.

mpred_retract_type(fact(_FT),X):-   
  %  db mpred_ain_db_to_head(X,X2), retract_u(X2). 
  % stop_trace(mpred_retract_type(fact(FT),X)),
  (retract_u(X) 
   *-> nop(mpred_unfwc(X)) ; (mpred_unfwc(X),!,fail)).

mpred_retract_type(rule,X):- 
  %  db  mpred_ain_db_to_head(X,X2),  retract_u(X2).
  (retract_u(X)
      *-> mpred_unfwc(X) ; (mpred_unfwc(X),!,fail)).

mpred_retract_type(trigger,X):- 
  retract_u(X)
    -> mpred_unfwc(X)
     ; mpred_warn("Trigger not found to retract_u: ~p",[X]).

mpred_retract_type(action,X):- mpred_undo_action(X).
  

%%  mpred_ain_object(X) 
%
% adds item P to some database
%
mpred_ain_object(X):-
  % what type of P do we have?
  mpred_db_type(X,Type),
  % call the appropriate predicate.
  mpred_ain_by_type(Type,X).

mpred_ain_by_type(fact(_FT),X):- 
  mpred_unique_u(X), 
  assert_u_confirmed_was_missing(X),!.
mpred_ain_by_type(rule,X):- 
  mpred_unique_u(X), 
  assert_u_confirmed_was_missing(X),!.
mpred_ain_by_type(trigger,X):- 
  assert_u_confirmed_was_missing(X).
mpred_ain_by_type(action,_ZAction):- !.


%%  mpred_withdraw(P).
%  removes support S from P and checks to see if P is still supported.
%  If it is not, then the fact is retracted from the database and any support
%  relationships it participated in removed.

mpred_withdraw(Ps):- 
  % TODO this is for idiomatic withdrawls: get_source_ref_stack(UU),
   mpred_withdraw(Ps,_UU).

%%  mpred_withdraw(+P,+S) is det.
% removes support S from P and checks to see if P is still supported.
%  If it is not, then the fact is retreacted from the database and any support
%  relationships it participated in removed.
mpred_withdraw(P,S):-
  sanity(is_ftNonvar(P)),
  each_E(mpred_withdraw1,P,[S]).

mpred_withdraw1(P,S):-
  % TODO this is for idiomatic withdrawls sanity(is_ftNonvar(S)),
  (get_first_user_reason(P,S)*->true;true),
  sanity(is_ftNonvar(P)),
  mpred_trace_msg('~N~n\tRemoving~n\t\tterm: ~p~n\t\tsupport (was): ~p~n',[P,S]),
  must((
   mpred_rem_support(P,S)
     -> with_current_why(S,must(remove_if_unsupported(P)))
      ; mpred_trace_msg("mpred_withdraw/2 Could not find support ~p to remove from fact ~p",
                [S,P]))).

%%  mpred_remove(+P) is det.
% 
%  mpred_remove is like mpred_withdraw, but if P is still in the DB after removing the
%  user''s support, it is retracted by more forceful means (e.ax. remove).
% 
mpred_remove(P):- get_first_user_reason(P,UU), mpred_remove(P,UU).
mpred_remove(P,S):- each_E(mpred_remove1,P,[S]).

mpred_remove1(P,S):-
  mpred_withdraw(P,S),
  mpred_call_no_bc(P)
     -> mpred_blast(P) 
      ; true.


mpred_post1_rem(P,S):- 
  clause_asserted_u(P),
  must((mpred_post1_rem1(P,S), \+ clause_asserted_u(P))).

mpred_post1_rem(P,S):- mpred_post1_rem1(P,S),!.

mpred_post1_rem1(P,S):- 
   must(mpred_withdraw(P, S)),
   must(mpred_remove(P,S)),
   doall(mpred_undo(P)),
   ignore(retract_u(P)).



% 
%  mpred_blast(+F) retracts fact F from the DB and removes any dependent facts
% 

mpred_blast(F):- 
  must(mpred_remove_supports(F)),
  must(mpred_undo(F)).


% removes any remaining supports for fact F, complaining as it goes.

mpred_remove_supports(F):- 
  mpred_rem_support(F,S),
  mpred_warn("~p was still supported by ~p",[F,S]),
  fail.
mpred_remove_supports(_).

mpred_remove_supports_quietly(F):- 
  mpred_rem_support(F,_),
  fail.
mpred_remove_supports_quietly(_).

%% mpred_undo(X) undoes X.
%
% - a positive or negative trigger.
% - an action by finding a method and successfully executing it.
% - or a random fact, printing out the dtrace, if relevant.
%

mpred_undo(X):- mpred_undo1(X),!.
% maybe still un-forward chain?
mpred_undo(Fact):-
  % undo a random fact, printing out the dtrace, if relevant.  
  show_call(mpred_unfwc(Fact)).
% mpred_undo(X):- doall(mpred_undo1(X)).

mpred_undo1((H:-B)):- reduce_clause(unpost,(H:-B),HB), HB\=@= (H:-B),!,mpred_undo1((HB)).
mpred_undo1(actn(A)):-  
  % undo an action by finding a method and successfully executing it.
  !,
  mpred_undo_action(actn(A)).

mpred_undo1(pt(Key,Head,Body)):-  
  % undo a positive trigger.
  %
  !,
  (show_success(retract_u(pt(Key,Head,Body)))
    -> mpred_unfwc(pt(Head,Body))
     ; mpred_warn("Trigger not found to undo: ~p",[pt(Head,Body)])).

mpred_undo1(pt(Head,Body)):- fail,
  % undo a positive trigger.
  %
  !,
  (show_success(retract_u(pt(Head,Body)))
    -> mpred_unfwc(pt(Head,Body))
     ; mpred_warn("Trigger not found to undo: ~p",[pt(Head,Body)])).

mpred_undo1(nt(Head,Condition,Body)):-  
  % undo a negative trigger.
  !,
  (show_success(retract_u(nt(Head,Condition,Body)))
    -> mpred_unfwc(nt(Head,Condition,Body))
     ; mpred_warn("Trigger not found to undo: ~p",[nt(Head,Condition,Body)])).

mpred_undo1(Fact):-
  % undo a random fact, printing out the dtrace, if relevant.
  retract_u(Fact),
  mpred_trace_op(rem,Fact),
  mpred_unfwc(Fact).



:- forall(debugging(X),nodebug(X)).

%%  mpred_unfwc(+P) 
%
% "un-forward-chains" from fact P.  That is, fact P has just
%  been removed from the database, so remove all support relations it
%  participates in and check the things that they support to see if they
%  should stay in the database or should also be removed.
%
mpred_unfwc(F):- 
  show_call(mpred_retract_supported_relations(F)),
  mpred_unfwc1(F).

mpred_unfwc1(F):-
  mpred_unfwc_check_triggers(F),
  % is this really the right place for mpred_run<?
  quietly(mpred_run),!.


mpred_unfwc_check_triggers(F):- loop_check(mpred_unfwc_check_triggers0(F),
  (mpred_warn(looped_mpred_unfwc_check_triggers0(F)), mpred_run)).

mpred_unfwc_check_triggers0(F):-
  mpred_db_type(F,fact(_FT)),
  copy_term_vn(F,Fcopy),
  lookup_u(nt(Fcopy,Condition,Action)),
  \+ mpred_call_no_bc(Condition),
  mpred_eval_lhs(Action,((\+F),nt(F,Condition,Action))),
  fail.
mpred_unfwc_check_triggers0(_).


mpred_retract_supported_relations(Fact):-
  mpred_db_type(Fact,Type),
  (Type=trigger -> mpred_rem_support_if_exists(P,(_,Fact))
                ; mpred_rem_support_if_exists(P,(Fact,_))),
  must(nonvar(P)),
  remove_if_unsupported(P),
  fail.
mpred_retract_supported_relations(_).



%  remove_if_unsupported(+Ps) checks to see if all Ps are supported and removes
%  it from the DB if they are not.
remove_if_unsupported(P):- 
   mpred_supported(P) -> mpred_trace_msg('~p',[still_supported(P)]) ;  must(mpred_undo(P)).




%%  mpred_fwc(+X) is det.
%
% forward chains from a fact or a list of facts X.
%
mpred_fwc(Ps):- each_E(mpred_fwc0,Ps,[]).
:- module_transparent((mpred_fwc0)/1).

%%  mpred_fwc0(+X) is det.
%
%  Avoid loop while calling mpred_fwc1(P)
% 
% this line filters sequential (and secondary) dupes
mpred_fwc0(Fact):- fail, ground(Fact),fwc1s_post1s(_One,Two),Six is Two * 3,filter_buffer_n_test('$last_mpred_fwc1s',Six,Fact),!.
mpred_fwc0(Fact):- copy_term_vn(Fact,FactC),
      mpred_fwc1(FactC).


filter_buffer_trim(Name,N):-
  filter_buffer_get_n(Name,List,N),
  nb_setval(Name,List).

filter_buffer_get_n(Name,FactS,N):-
  nb_current(Name,Fact1s),
  length(Fact1s,PFs),!,
  ((PFs =< N) 
    -> FactS=Fact1s;
   (length(FactS,N),append(FactS,_,Fact1s))).
filter_buffer_get_n(_,[],_).


% filter_buffer_n_test(_Name,_,_Fact):- \+ need_speed, !,fail.
filter_buffer_n_test(Name,N,Fact):- filter_buffer_get_n(Name,FactS,N),
   (memberchk(Fact,FactS)-> true ; (nb_setval(Name,[Fact|FactS]),fail)).


%% mpred_fwc1(+P) is det.
%
% forward chains for a single fact.
%  Avoids loop while calling mpred_fwc1(P)
mpred_fwc1(clause_asserted_u(Fact)):-!,sanity(clause_asserted_u(Fact)).
mpred_fwc1((Fact:- BODY)):- compound(Body),arg(1,Body,Cwc),Cwc==fwc,ground(BODY),!, mpred_fwc1({BODY}==>Fact).
mpred_fwc1(support_hilog(_,_)):-!.
% mpred_fwc1(singleValuedInArg(_, _)):-!.
% this line filters sequential (and secondary) dupes
% mpred_fwc1(Fact):- current_prolog_flag(unsafe_speedups,true), ground(Fact),fwc1s_post1s(_One,Two),Six is Two * 3,filter_buffer_n_test('$last_mpred_fwc1s',Six,Fact),!.

mpred_fwc1(Fact):- 
  mpred_trace_msg(mpred_fwc1(Fact)),
  %ignore((mpred_non_neg_literal(Fact),remove_negative_version(Fact))),
  mpred_do_rule(Fact),!.


%% mpred_do_rule(P)
% does some special, built in forward chaining if P is
%  a rule.

% mpred_do_rule((H:-attr_bind(B,_))):- get_functor(H,F,A),lookup_u(mpred_mark(pfcLHS,F,A)), sanity(nonvar(B)), repropagate(H),!. 
mpred_do_rule((H:-B)):- var(H),sanity(nonvar(B)),forall(call_u(B),mpred_ain(H)),!.
mpred_do_rule((H:-B)):- get_functor(H,F,A),lookup_u(mpred_mark(pfcLHS,F,A)), sanity(nonvar(B)),forall(call_u(B),mpred_fwc(H)),!.
%   !,dtrace,ignore((lookup_u(H),mpred_fwc1(H),fail)).

% mpred_do_rule((H:-B)):- !,ignore((call_u(B),mpred_fwc1(H),fail)).

mpred_do_rule((P==>Q)):-  
  !,  
  process_rule(P,Q,(P==>Q)).
mpred_do_rule((Name::::P==>Q)):- 
  !,  
  process_rule(P,Q,(Name::::P==>Q)).
mpred_do_rule((P<==>Q)):- 
  !, 
  process_rule(P,Q,(P<==>Q)), 
  process_rule(Q,P,(P<==>Q)).
mpred_do_rule((Name::::P<==>Q)):- 
  !, 
  process_rule(P,Q,((Name::::P<==>Q))), 
  process_rule(Q,P,((Name::::P<==>Q))).

mpred_do_rule(('<-'(P,Q))):-
  !,
  mpred_define_bc_rule(P,Q,('<-'(P,Q))).

mpred_do_rule(('<=='(P,Q))):-
  !,
  mpred_define_bc_rule(P,Q,('<-'(P,Q))).

mpred_do_rule(Fact):-
  copy_term_vn(Fact,F),
  % check positive triggers
  loop_check(mpred_do_fcpt(Fact,F),true),
  % check negative triggers
  mpred_do_fcnt(Fact,F).


mpred_do_fcpt(Fact,F):- 
  lookup_u(pt(F,Body)),  
  mpred_trace_msg('~N~n\tFound positive trigger: ~p~n\t\tbody: ~p~n',
		[F,Body]),  
  mpred_eval_lhs(Body,(Fact,pt(F,Body))),
  fail.

%mpred_do_fcpt(Fact,F):- 
%  lookup_u(pt(presently(F),Body)),
%  mpred_eval_lhs(Body,(presently(Fact),pt(presently(F),Body))),
%  fail.

mpred_do_fcpt(_,_).

mpred_do_fcnt(_ZFact,F):-
  NT = nt(F,Condition,Body),
  SPFT = spft(X,F1,NT),
  lookup_u(SPFT),
  %clause(SPFT,true),
  mpred_trace_msg('~N~n\tFound negative trigger: ~p~n\t\tcond: ~p~n\t\tbody: ~p~n\tSupport: ~p~n',
                 [F,Condition,Body,SPFT]),
  mpred_call_no_bc(Condition),
  mpred_withdraw(X,(F2,NT)),
  must(F1=F2),
  fail.
mpred_do_fcnt(_,_).


%% mpred_define_bc_rule(+Head,+Body,+Parent_rule) 
%  
% defines a backward chaining rule and adds the 
% corresponding bt triggers to the database.
% 
mpred_define_bc_rule(Head,_ZBody,Parent_rule):-
  (\+ mpred_literal(Head)),
  mpred_warn("Malformed backward chaining rule.  ~p not atomic.",[Head]),
  mpred_error("rule: ~p",[Parent_rule]),
  !,
  fail.

mpred_define_bc_rule(Head,Body,Parent_rule):-
  must_notrace_pfc(get_source_ref1(U)),
  copy_term(Parent_rule,Parent_ruleCopy),
  build_rhs(U,Head,Rhs),
  ain_fast((Head:-mpred_bc_only(Head))),
  foreachl_do(mpred_nf(Body,Lhs),
          (build_trigger(Parent_ruleCopy,Lhs,rhs(Rhs),Trigger),
           ain_fast(bt(Head,Trigger),(Parent_ruleCopy,U)))).
 
:-nb_setval('$pfc_current_choice',[]).

push_current_choice(CP):- (nb_current('$pfc_current_choice',Was);Was=[]), b_setval('$pfc_current_choice',[CP|Was]),!.


cut_c:-
  must(nb_current('$pfc_current_choice',[CP|_WAS])),prolog_cut_to(CP).


%% mpred_eval_lhs(X,Support) is nondet.
%
%  eval something on the LHS of a rule.
% 
mpred_eval_lhs(X,S):-
   prolog_current_choice(CP),push_current_choice(CP),
   
   with_current_why(S,mpred_eval_lhs_0(X,S)).


%% mpred_eval_lhs_0(X,Support) is det.
%
%  Helper of evaling something on the LHS of a rule.
% 
mpred_eval_lhs_0(Var,Support):- var(Var),!,trace_or_throw(var_mpred_eval_lhs_0(Var,Support)).
mpred_eval_lhs_0((Test *-> Body),Support):-  % Noncutted ->
  !,
  mpred_call_no_bc(Test),
   mpred_eval_lhs_0(Body,Support).

mpred_eval_lhs_0((Test -> Body),Support):- !,  % cutted ->
  mpred_call_no_bc(Test) -> mpred_eval_lhs_0(Body,Support).

mpred_eval_lhs_0(rhs(X),Support):- !,
   mpred_eval_rhs(X,Support).

%mpred_eval_lhs_0(snip(X),Support):- 
%  snip(Support),
%  mpred_eval_lhs_0(X,Support).

mpred_eval_lhs_0(X,Support):- mpred_db_type(X,trigger), !, mpred_ain_trigger_reprop(X,Support).
mpred_eval_lhs_0(X,_):- mpred_warn("Unrecognized item found in trigger body, namely ~p.",[X]).



%%  mpred_eval_rhs1(What,Support) is nondet.
%
%  eval something on the RHS of a rule.
% 
mpred_eval_rhs([],_):- !.
mpred_eval_rhs([Head|Tail],Support):- 
  mpred_eval_rhs1(Head,Support),
  mpred_eval_rhs(Tail,Support).

mpred_eval_rhs1({Action},Support):-
 % evaluable Prolog code.
 !,
 fc_eval_action(Action,Support).

% Dmiles replaced with this
mpred_eval_rhs1( P,Support):-
 % predicate to remove.
  mpred_unnegate( P , PN),
  %TODO Shouldn''t we be mpred_withdrawing the Positive version?  (We are)
  % perhaps we aready negated here from mpred_nf1_negation?!
  mpred_trace_msg('~n\t\tWithdrawing Negation: ~p \n\tSupport: ~p~n',[P,Support]),
  !,
  mpred_withdraw(PN).

mpred_eval_rhs1( P,Support):-
 % predicate to remove.
  \+ \+ mpred_negated_literal( P),
  %TODO Shouldn''t we be mpred_withdrawing the Positive version?  
  % perhaps we aready negated here dirrent nf1_*
  mpred_trace_msg('~N~n =pred_eval_rhs1= ~n\t\tWithdrawing: ~p \n\tSupport: ~p~n',[P,Support]),
  !,
  mpred_withdraw(P).

mpred_eval_rhs1([X|Xrest],Support):-
 % embedded sublist.
 !,
 mpred_eval_rhs([X|Xrest],Support).

mpred_eval_rhs1(Assertion,Support):- !,
 % an assertion to be added.
 mpred_trace_msg('~N~n =pred_eval_rhs1= ~n~n\tPost1: ~p \n\tSupport: ~p~n',[Assertion,Support]),!,
 (must_det(mpred_post(Assertion,Support)) *->
    true;
    mpred_warn("\n\t\t\n\t\tMalformed rhs of a rule (mpred_post1 failed)\n\t\tPost1: ~p\n\t\tSupport=~p.",[Assertion,Support])).

% mpred_eval_rhs1(X,_):-  mpred_warn("Malformed rhs of a rule: ~p",[X]).


%% fc_eval_action(+Action,+Support)
%
%  evaluate an action found on the rhs of a rule.
% 

fc_eval_action(Action,Support):-
  mpred_call_no_bc(Action), 
  (action_is_undoable(Action) 
     -> mpred_ain_actiontrace(Action,Support) 
      ; true).

/*
% 
%  
% 

trigger_trigger(Trigger,Body,_ZSupport):-
 trigger_trigger1(Trigger,Body).
trigger_trigger(_,_,_).


%trigger_trigger1(presently(Trigger),Body):-
%  !,
%  copy_term_vn(Trigger,TriggerCopy),
%  call_u(Trigger),
%  mpred_eval_lhs(Body,(presently(Trigger),pt(presently(TriggerCopy),Body))),
%  fail.

trigger_trigger1(Trigger,Body):-
  copy_term_vn(Trigger,TriggerCopy),
  call_u(Trigger),
  mpred_eval_lhs(Body,(Trigger,pt(TriggerCopy,Body))),
  fail.
*/


call_m_g(To,_M,G):- To:call(G).
lookup_m_g(To,_M,G):- clause(To:G,true).

%%  call_u(F) is det.
% 
%  is true iff F is a fact available *for* forward chaining 
%  (or *from* the backchaining rules)
%  Note: a bug almost fixed is that this sometimes the side effect of catching 
%  facts and not assigning the correct justifications
% 
% call_u(P):- predicate_property(P,number_of_rules(N)),N=0,!,lookup_u(P).

call_u(G):- var(G),!,dtrace,defaultAssertMt(W),with_umt(W,mpred_fact_mp(W,G)).
call_u(M:G):- var(M),!,trace_or_throw(var_call_u(M:G)).
call_u(M:G):- nonvar(M),var(G),!,sanity(mtCycL(M)),with_umt(M,mpred_fact_mp(M,G)).
call_u(G):- current_prolog_flag(unsafe_speedups,true),!,baseKB:call(G).
call_u(M:G):- clause_b(mtProlog(M)),predicate_property(M:G,defined),!,call(M:G).
call_u(G):- strip_module(G,M,P),
  (clause_b(mtCycL(M))-> W=M;defaultAssertMt(W)),!,
   (var(P)->CP=mpred_fact_mp(W,P);
   % CP=mpred_BC_w_cache(W,P)
   CP=P
      ),
   with_umt(W, CP).


/*
call_u(G):- strip_module(G,M,P),
  set_prolog_flag(retry_undefined, true),
  (clause_b(mtCycL(M))-> W=M;defaultAssertMt(W)),
    with_umt(W, mpred_BC_w_cache(W,P)).
*/

mpred_BC_w_cache(W,P):- must(mpred_BC_CACHE(W,P)),!,mpred_call_no_bc(P).

mpred_BC_CACHE(M,P0):-  ignore( \+ loop_check_early(mpred_BC_CACHE0(M,P0),true)).

mpred_BC_CACHE0(_,P00):- var(P00),!.
mpred_BC_CACHE0(M,must(P00)):-!,mpred_BC_CACHE0(M,P00).
mpred_BC_CACHE0(M,P):- predicate_property(M:P,static),!.
mpred_BC_CACHE0(M,P):- predicate_property(M:P,built_in),!.
mpred_BC_CACHE0(_, :-(_,_)):-!.
mpred_BC_CACHE0(_,bt(_,_)):-!.
mpred_BC_CACHE0(_,clause(_,_)):-!.
mpred_BC_CACHE0(_,spft(_,_,_)):-!.
mpred_BC_CACHE0(_,P):- 
 ignore((
  cyclic_break(P),
 % trigger any bc rules.
  lookup_u(bt(P,Trigger)),
  copy_term_vn(bt(P,Trigger),bt(CP,CTrigger)),
  must(lookup_u(spft(bt(CP,_Trigger),F,T))),
  mpred_eval_lhs(CTrigger,(F,T)),
  fail)).



% I''d like to remove this soon
mpred_call_no_bc(P0):- strip_module(P0,_,P), sanity(stack_check),var(P),!, mpred_fact(P).
mpred_call_no_bc(baseKB:true):-!.

mpred_call_no_bc(P):- no_repeats(loop_check(mpred_call_no_bc0(P),mpred_METACALL(call, P))).

% mpred_call_no_bc0(P):- lookup_u(P).
% mpred_call_no_bc0(P):-  defaultAssertMt(Mt), Mt:lookup_u(P).
% mpred_call_no_bc0((A,B)):-!, mpred_call_no_bc0(A),mpred_call_no_bc0(B).
%mpred_call_no_bc0(P):-  defaultAssertMt(Mt),current_predicate(_,Mt:P),!,Mt:call(P).
%mpred_call_no_bc0(P):-  defaultAssertMt(Mt),rtrace(Mt:call(P)).
% TODO .. mpred_call_no_bc0(P):-  defaultAssertMt(Mt), clause_b(genlMt(Mt,SuperMt)), call_umt(SuperMt,P).
%mpred_call_no_bc0(P):- mpred_call_with_no_triggers(P).
% mpred_call_no_bc0(P):- nonvar(P),predicate_property(P,defined),!, P.
mpred_call_no_bc0(P):- current_prolog_flag(unsafe_speedups,true),!,baseKB:call(P).
mpred_call_no_bc0(P):- loop_check(mpred_METACALL(ereq, P)).

pred_check(A):- var(A),!.
% catch module prefix issues
pred_check(A):- nonvar(A),must(atom(A)).

mpred_METACALL(How,P):- current_prolog_flag(unsafe_speedups,true),!,baseKB:call(How,P).
mpred_METACALL(How,P):- mpred_METACALL(How, Cut, P), (var(Cut)->true;(Cut=cut(CutCall)->(!,CutCall);mpred_call_no_bc(Cut))).

mpred_METACALL(How, Cut, Var):- var(Var),!,trace_or_throw(var_mpred_METACALL_MI(How,Cut,Var)).
mpred_METACALL(How, Cut, (H:-B)):-!,mpred_METACALL(How, Cut, clause_asserted_call(H,B)).
%  this is probably not advisable due to extreme inefficiency.
mpred_METACALL(_How,_Cut, Var):-is_ftVar(Var),!,mpred_call_with_no_triggers(Var).
mpred_METACALL(How, Cut, mpred_call_no_bc(G0)):- !,mpred_METACALL(How, Cut, (G0)).
mpred_METACALL(_How, Cut, mpred_METACALL(How2, G0)):- !,mpred_METACALL(How2, Cut, (G0)).
mpred_METACALL(How, Cut, mpred_METACALL(G0)):- !,mpred_METACALL(How, Cut, (G0)).
mpred_METACALL(_How, cut(true), !):- !.

mpred_METACALL(How, Cut, (C1->C2;C3)):-!,(mpred_METACALL(How, Cut, C1)->mpred_METACALL(How, Cut, C2);mpred_METACALL(How, Cut, C3)).
mpred_METACALL(How, Cut, (C1*->C2;C3)):-!,(mpred_METACALL(How, Cut, C1)*->mpred_METACALL(How, Cut, C2);mpred_METACALL(How, Cut, C3)).

mpred_METACALL(How, Cut, (C1->C2)):-!,(mpred_METACALL(How, Cut, C1)->mpred_METACALL(How, Cut, C2)).
mpred_METACALL(How, Cut, (C1*->C2)):-!,(mpred_METACALL(How, Cut, C1)*->mpred_METACALL(How, Cut, C2)).
mpred_METACALL(How, Cut, (C1,C2)):-!,mpred_METACALL(How, Cut, C1),mpred_METACALL(How, Cut, C2).
mpred_METACALL(How, Cut, (C1;C2)):-!,(mpred_METACALL(How, Cut, C1);mpred_METACALL(How, Cut, C2)).
%  check for system predicates first
% mpred_METACALL(_How, _SCut, P):- predicate_property(P,built_in),!, call(P).


mpred_METACALL(How, Cut, M):- mpred_expansion:fixed_negations(M,O),!,mpred_METACALL(How, Cut, O).
mpred_METACALL(How, Cut, U:X):-U==user,!,mpred_METACALL(How, Cut, X).
mpred_METACALL(How, Cut, t(A,B)):-(atom(A)->true;(no_repeats(arity(A,1)),atom(A))),ABC=..[A,B],mpred_METACALL(How, Cut, ABC).
mpred_METACALL(How, Cut, isa(B,A)):-(atom(A)->true;(no_repeats(tCol(A)),atom(A))),ABC=..[A,B],mpred_METACALL(How, Cut, ABC).
%mpred_METACALL(How, Cut, t(A,B)):-!,(atom(A)->true;(no_repeats(arity(A,1)),atom(A))),ABC=..[A,B],mpred_METACALL(How, Cut, ABC).
mpred_METACALL(How, Cut, t(A,B,C)):-!,(atom(A)->true;(no_repeats(arity(A,2)),atom(A))),ABC=..[A,B,C],mpred_METACALL(How, Cut, ABC).
mpred_METACALL(How, Cut, t(A,B,C,D)):-!,(atom(A)->true;(no_repeats(arity(A,3)),atom(A))),ABC=..[A,B,C,D],mpred_METACALL(How, Cut, ABC).
mpred_METACALL(How, Cut, t(A,B,C,D,E)):-!,(atom(A)->true;(no_repeats(arity(A,4)),atom(A))),ABC=..[A,B,C,D,E],mpred_METACALL(How, Cut, ABC).
mpred_METACALL(How, Cut, call(X)):- !, mpred_METACALL(How, Cut, X).
mpred_METACALL(How, Cut, call_u(X)):- !, mpred_METACALL(How, Cut, X).
mpred_METACALL(How, Cut, \+(X)):- !, \+ mpred_METACALL(How, Cut, X).
mpred_METACALL(How, Cut, call_u(X)):- !, mpred_METACALL(How, Cut, X).
mpred_METACALL(_How, _Cut, clause(H,B,Ref)):-!,clause_u(H,B,Ref).
mpred_METACALL(_How, _Cut, clause(H,B)):-!,clause_u(H,B).
mpred_METACALL(_How, _Cut, clause(HB)):-expand_to_hb(HB,H,B),!,clause_u(H,B).
mpred_METACALL(_How, _Cut, asserta(X)):- !, aina(X).
mpred_METACALL(_How, _Cut, assertz(X)):- !, ainz(X).
mpred_METACALL(_How, _Cut, assert(X)):- !, mpred_ain(X).
mpred_METACALL(_How, _Cut, retract(X)):- !, mpred_remove(X).
% TODO: test removal
%mpred_METACALL(How, Cut, prologHybrid(H)):-get_functor(H,F),!,isa_asserted(F,prologHybrid).
%mpred_METACALL(How, Cut, HB):-hotrace((fully_expand_warn(mpred_call_0,HB,HHBB))),!,mpred_METACALL(How, Cut, HHBB).
%mpred_METACALL(How, Cut, argIsa(mpred_isa,2,mpred_isa/2)):-  trace_or_throw(mpred_METACALL(How, Cut, argIsa(mpred_isa,2,mpred_isa/2))),!,fail.
% TODO: test removal
% mpred_METACALL(How, Cut, isa(H,B)):-!,isa_asserted(H,B).
mpred_METACALL(_How, _Cut, (H:-B)):- !, clause_u((H :- B)).
mpred_METACALL(_How, _Cut, M:(H:-B)):- !, clause_u((M:H :- B)).
% TODO: mpred_METACALL(_How, _Cut, M:HB):- current_prolog_flag(unsafe_speedups,true),!, call(M:HB).
mpred_METACALL(_How, _SCut, P):- predicate_property(P,built_in),!, call(P).
%mpred_METACALL(How, Cut, (H)):- is_static_pred(H),!,show_pred_info(H),dtrace(mpred_METACALL(How, Cut, (H))).
mpred_METACALL( How,   Cut, P) :- fail, predicate_property(P,number_of_clauses(_)),!,
     clause_u(P,Condition),
     mpred_METACALL(How,Cut,Condition),
       (var(Cut)->true;(Cut=cut(CutCall)->(!,CutCall);mpred_call_no_bc(Cut))).

% mpred_METACALL(_How,_SCut, P):- must(current_predicate(_,M:P)),!, call_u(M:P).
%mpred_METACALL(How, Cut, H):- !, w_tl(t_l:infAssertedOnly(H),call_u(H)).
mpred_METACALL(How, _SCut, P):- call(How,P).




%% action_is_undoable(?A) 
%
% an action is action_is_undoable if there exists a method for undoing it.
%
action_is_undoable(A):- lookup_u(do_and_undo(A,_)).



%% mpred_nf(+In,-Out)
% 
% maps the LHR of a Pfc rule In to one normal form 
%  Out.  It also does certain optimizations.  Backtracking into this
%  predicate will produce additional clauses.
%

mpred_nf({LHS},List):- !,
  mpred_nf((nondet,{LHS}),List).

mpred_nf(LHS,List):-
  mpred_nf1(LHS,List2),
  mpred_nf_negations(List2,List).


%%  mpred_nf1(+In,-Out) 
%
% maps the LHR of a Pfc rule In to one normal form
%  Out.  Backtracking into this predicate will produce additional clauses.

% handle a variable.

mpred_nf1(P,[P]):- is_ftVar(P), !.

% these next two rules are here for upward compatibility and will go 
% away eventually when the P/Condition form is no longer used anywhere.

mpred_nf1(P/Cond,[(\+P)/Cond]):- mpred_negated_literal(P), !.

mpred_nf1(P/Cond,[P/Cond]):- !, must( mpred_literal(P)), !.

%  handle a negated form

mpred_nf1(NegTerm,NF):-
  mpred_unnegate(NegTerm,Term),
  !,
  mpred_nf1_negation(Term,NF).

%  disjunction.

mpred_nf1((P;Q),NF):- 
  !,
  (mpred_nf1(P,NF) ;   mpred_nf1(Q,NF)).


%  conjunction.

mpred_nf1((P,Q),NF):-
  !,
  mpred_nf1(P,NF1),
  mpred_nf1(Q,NF2),
  append(NF1,NF2,NF).

mpred_nf1([P|Q],NF):-
  !,
  mpred_nf1(P,NF1),
  mpred_nf1(Q,NF2),
  append(NF1,NF2,NF).


%  handle a random literal.

mpred_nf1(P,[P]):- 
  mpred_literal(P), 
  !.

% mpred_nf1(Term,[Term]):- mpred_warn("mpred_nf Accepting ~p",[Term]),!.


%=% shouldn''t we have something to catch the rest as errors?
mpred_nf1(Term,[Term]):-
  mpred_warn("mpred_nf doesn't know how to normalize ~p",[Term]),dtrace,!,fail.

notiffy_p(P,\+(P)):- var(P),!. % prevents next line from binding
notiffy_p(\+(P),P):- dmsg(notiffy_p(\+(P),P)), !.
notiffy_p(P,\+(P)).

%% mpred_nf1_negation(+P, ?NF) is semidet.
%
% is true if NF is the normal form of \+P.
%
mpred_nf1_negation((P/Cond),[NOTP/Cond]):- notiffy_p(P,NOTP), !.

mpred_nf1_negation((P;Q),NF):-
  !,
  mpred_nf1_negation(P,NFp),
  mpred_nf1_negation(Q,NFq),
  append(NFp,NFq,NF).

mpred_nf1_negation((P,Q),NF):- 
  % this code is not correct! twf.
  !,
  mpred_nf1_negation(P,NF) 
  ;
  (mpred_nf1(P,Pnf),
   mpred_nf1_negation(Q,Qnf),
   append(Pnf,Qnf,NF)).

mpred_nf1_negation(P,[\+P]).


%%  mpred_nf_negations(List2,List) is det.
%
% sweeps through List2 to produce List,
%  changing -{...} to {\+...}
% % ? is this still needed? twf 3/16/90

%% mpred_nf_negations( :TermX, :TermX) is semidet.
%
% PFC Normal Form Negations.
%
mpred_nf_negations(X,X) :- !.  % I think not! twf 3/27/90

mpred_nf_negations([],[]).

mpred_nf_negations([H1|T1],[H2|T2]):-
  mpred_nf_negation(H1,H2),
  mpred_nf_negations(T1,T2).


%% mpred_nf_negation(+X, ?X) is semidet.
%
% PFC Normal Form Negation.
%
mpred_nf_negation(Form,{\+ X}):- 
  nonvar(Form),
  Form=(-({X})),
  !.
mpred_nf_negation(X,X).


 
%%  build_rhs(+Sup,+Conjunction,-Rhs)
% 

build_rhs(_Sup,X,[X]):- 
  var(X),
  !.

build_rhs(Sup,(A,B),[A2|Rest]):- 
  !, 
  mpred_compile_rhs_term(Sup,A,A2),
  build_rhs(Sup,B,Rest).

build_rhs(Sup,X,[X2]):-
   mpred_compile_rhs_term(Sup,X,X2).


mpred_compile_rhs_term(_Sup,P,P):-is_ftVar(P),!.

% TODO confirm this is not reversed (mostly confirmed this is correct now)
mpred_compile_rhs_term(Sup, \+ ( P / C), COMPILED) :- nonvar(C), !,
  mpred_compile_rhs_term(Sup, ( \+ P ) / C , COMPILED).

% dmiles added this to get PFC style lazy BCs
mpred_compile_rhs_term(Sup,(P/C),((P0 <- C0))) :- !,mpred_compile_rhs_term(Sup,P,P0),
   build_code_test(Sup,C,C0),!.

mpred_compile_rhs_term(Sup,(P/C),((P0 :- C0))) :- !,mpred_compile_rhs_term(Sup,P,P0),
   build_code_test(Sup,C,C0),!.

mpred_compile_rhs_term(Sup,I,OO):- 
  fully_expand(compile_rhs,I,O),
  must(\+ \+ mpred_mark_as(Sup,O,pfcRHS)),!,build_consequent(Sup,O,OO).

mpred_compile_rhs_term(Sup,I,O):- build_consequent(Sup,I,O).



%% mpred_unnegate(+N, ?P) is semidet.
%
%  is true if N is a negated term and P is the term
%  with the negation operator stripped.
%
mpred_unnegate(P,_):- is_ftVar(P),!,fail.
mpred_unnegate((\+(P)),P).
mpred_unnegate((-P),P).



%% mpred_negated_literal(+P) is semidet.
%
% PFC Negated Literal.
%
mpred_negated_literal(P):- 
  mpred_unnegate(P,Q),
  mpred_positive_literal(Q).

mpred_literal(X):- is_ftVar(X),!.
mpred_literal(X):- mpred_negated_literal(X),!.
mpred_literal(X):- mpred_positive_literal(X),!.

mpred_positive_literal(X):-  
  is_ftNonvar(X),
  \+ mpred_db_type(X,rule),
  get_functor(X,F,_), 
  \+ mpred_neg_connective(F),
  !.


mpred_connective(Var):-var(Var),!,fail.
mpred_connective(';').
mpred_connective(',').
mpred_connective('/').
mpred_connective('|').
mpred_connective(('==>')).
mpred_connective(('<-')).
mpred_connective('<==>').
mpred_connective('-').
% mpred_connective('~').
mpred_connective(('\\+')).


mpred_neg_connective('-').
% mpred_neg_connective('~').
mpred_neg_connective('\\+').


%% process_rule(+Lhs, ?Rhs, ?Parent_rule) is semidet.
%
% Process Rule.
%
process_rule(Lhs,Rhs,Parent_rule):-
  must_notrace_pfc(get_source_ref1(U)),
  copy_term(Parent_rule,Parent_ruleCopy),
  build_rhs(U,Rhs,Rhs2),
  foreachl_do(mpred_nf(Lhs,Lhs2), 
          build_rule(Lhs2,rhs(Rhs2),(Parent_ruleCopy,U))).


%% build_rule(+Lhs, ?Rhs, ?Support) is semidet.
%
% Build Rule.
%
build_rule(Lhs,Rhs,Support):-
  copy_term_vn(Support,WS),
  mpred_mark_as(WS,Lhs,pfcLHS),
  build_trigger(WS,Lhs,Rhs,Trigger),
  cyclic_break((Lhs,Rhs,WS,Trigger)),
  doall(mpred_eval_lhs(Trigger,Support)).

build_trigger(WS,[],Consequent,ConsequentO):-
   build_consequent(WS,Consequent,ConsequentO).

build_trigger(WS,[V|Triggers],Consequent,pt(V,X)):-
  var(V),
  !, 
  build_trigger(WS,Triggers,Consequent,X).

% T1 is a negation in the next two clauses
build_trigger(WS,[(T1/Test)|Triggers],Consequent,nt(T2,Test2,X)):-
  mpred_unnegate(T1,T2),
  !, 
  build_neg_test(WS,T2,Test,Test2),
  build_trigger(WS,Triggers,Consequent,X).

build_trigger(WS,[(T1)|Triggers],Consequent,nt(T2,Test,X)):-
  mpred_unnegate(T1,T2),
  !,
  build_neg_test(WS,T2,true,Test),
  build_trigger(WS,Triggers,Consequent,X).

build_trigger(WS,[{Test}|Triggers],Consequent,(Test*->Body)):- % Noncutted ->
  !,
  build_trigger(WS,Triggers,Consequent,Body).

build_trigger(WS,[T/Test|Triggers],Consequent,pt(T,X)):-
  !, 
  build_code_test(WS, Test,Test2),
  build_trigger(WS,[{Test2}|Triggers],Consequent,X).


%build_trigger(WS,[snip|Triggers],Consequent,snip(X)):-
%  !,
%  build_trigger(WS,Triggers,Consequent,X).

build_trigger(WS,[T|Triggers],Consequent,pt(T,X)):-
  !, 
  build_trigger(WS,Triggers,Consequent,X).


%%  build_neg_test(+WhyBuild,+Test,+Testin,-Testout).
% 
%  builds the test used in a negative trigger (nt/3).  This test is a
%  conjunction of the check than no matching facts are in the db and any
%  additional test specified in the rule attached to this - term.
% 

build_neg_test(WS,T,Testin,Testout):-
  build_code_test(WS,Testin,Testmid),
   
  mpred_conjoin((mpred_call_no_bc(T)),Testmid,Testout).



%% check_never_assert(+Pred) is semidet.
%
% Check Never Assert.
%
check_never_assert(_Pred):-!.
check_never_assert(Pred):- fail,mnotrace((( copy_term_and_varnames(Pred,Pred_2),mpred_call_no_bc(never_assert_u(Pred_2,Why)), Pred=@@=Pred_2,trace_or_throw(never_assert_u(Pred,Why))))),fail.
check_never_assert(Pred):- fail,mnotrace(ignore(( copy_term_and_varnames(Pred,Pred_2),mpred_call_no_bc(never_assert_u(Pred_2)),Pred=@@=Pred_2,trace_or_throw(never_assert_u(Pred))))).

%% check_never_retract(+Pred) is semidet.
%
% Check Never Retract.
%
check_never_retract(_Pred):-!.
check_never_retract(Pred):- notrace(ignore(( copy_term_and_varnames(Pred,Pred_2),mpred_call_no_bc(never_retract_u(Pred_2,Why)),Pred=@@=Pred_2,trace_or_throw(never_retract_u(Pred,Why))))).


:- export(mpred_mark_as_ml/3).

%% mpred_mark_as_ml(+Sup, ?Type, ?P) is semidet.
%
% PFC Mark Converted To Ml.
%
mpred_mark_as_ml(Sup,Type,P):- mpred_mark_as(Sup,P,Type).


%% pos_2_neg(+P, ?P) is semidet.
%
% pos  Extended Helper Negated.
%
pos_2_neg(p,n):-!.
pos_2_neg(n,p):-!.
pos_2_neg(P,~(P)):- (var(P); P \= '~'(_)),!.
% pos_2_neg(P,~(P)).


%% mpred_mark_as(+VALUE1, ?VALUE2, :TermP, ?VALUE4) is semidet.
%
% PFC Mark Converted To.
%
mpred_mark_as(_,P,_):- is_ftVar(P),!.
mpred_mark_as(Sup,\+(P),Type):- !,mpred_mark_as(Sup,P,Type).
mpred_mark_as(Sup,~(P),Type):- !,mpred_mark_as(Sup,P,Type).
mpred_mark_as(Sup,-(P),Type):- !,mpred_mark_as(Sup,P,Type).
mpred_mark_as(Sup,not(P),Type):- !,mpred_mark_as(Sup,P,Type).
mpred_mark_as(Sup,[P|PL],Type):- is_list(PL), !,must_maplist(mpred_mark_as_ml(Sup,Type),[P|PL]).
mpred_mark_as(Sup,( P / CC ),Type):- !, mpred_mark_as(Sup,P,Type),mpred_mark_as(Sup,( CC ),pfcCallCodePreCond).
mpred_mark_as(Sup,'{}'(  CC ), _Type):- mpred_mark_as(Sup,( CC ),pfcCallCodeBody).
mpred_mark_as(Sup,( A , B), Type):- !, mpred_mark_as(Sup,A, Type),mpred_mark_as(Sup,B, Type).
mpred_mark_as(Sup,( A ; B), Type):- !, mpred_mark_as(Sup,A, Type),mpred_mark_as(Sup,B, Type).
mpred_mark_as(Sup,( A ==> B), Type):- !, mpred_mark_as(Sup,A, Type),mpred_mark_as(Sup,B, pfcRHS).
mpred_mark_as(Sup,( B <- A), Type):- !, mpred_mark_as(Sup,A, Type),mpred_mark_as(Sup,B, pfcRHS).
%mpred_mark_as(_Sup,( _ :- _ ),_Type):-!.
mpred_mark_as(Sup,( P :- CC ),Type):- !, mpred_mark_as(Sup,P,Type),mpred_mark_as(Sup,( CC ),pfcCallCodeAnte).
mpred_mark_as(Sup,P,Type):-get_functor(P,F,A),ignore(mpred_mark_fa_as(Sup,P,F,A,Type)),!.


%% mpred_mark_fa_as(+Sup, ?P, ?F, ?A, ?Type) is semidet.
%
% PFC Mark Functor-arity Converted To.
%

% mpred_mark_fa_as(_Sup,_P,'\=',2,_):- dtrace.
mpred_mark_fa_as(_Sup,_P,_,_,Type):- Type \== pfcLHS, current_prolog_flag(unsafe_speedups,true),!.
mpred_mark_fa_as(_Sup,_P,isa,_,_):- !.
mpred_mark_fa_as(_Sup,_P,_,_,pfcCallCodeBody):- !.
mpred_mark_fa_as(_Sup,_P,_,_,pfcCallCodeTst):- !.
mpred_mark_fa_as(_Sup,_P,t,_,_):- !.
mpred_mark_fa_as(_Sup,_P,argIsa,N,_):- !,must(N=3).
mpred_mark_fa_as(_Sup,_P,arity,N,_):- !,must(N=2).
mpred_mark_fa_as(_Sup,_P,mpred_mark,N,_):- !,must(N=3).
mpred_mark_fa_as(_Sup,_P,mpred_isa,N,_):- must(N=2).
mpred_mark_fa_as(_Sup,_P,'[|]',N,_):- dtrace,must(N=2).
mpred_mark_fa_as(_Sup,_P,_:mpred_isa,N,_):- must(N=2).
mpred_mark_fa_as(Sup, _P,F,A,Type):- really_mpred_mark(Sup,Type,F,A),!.

really_mpred_mark(_  ,Type,F,A):- mpred_call_no_bc(mpred_mark(Type,F,A)),!.
really_mpred_mark(Sup,Type,F,A):- 
  MARK = mpred_mark(Type,F,A),
  check_never_assert(MARK),
  with_no_mpred_trace_exec(with_fc_mode(direct,mpred_ain(MARK,(s(Sup),ax)))).
  % with_no_mpred_trace_exec(with_fc_mode(direct,mpred_fwc1(MARK,(s(Sup),ax)))),!.
   

%% fa_to_p(+F, ?A, ?P) is semidet.
%
% Functor-arity Converted To Pred.
%
fa_to_p(F,A,P):-is_ftNameArity(F,A),functor(P,F,A),
  ( P \= mpred_call_no_bc(_) ),( P \= '$VAR'(_)).
  

%% build_code_test(+WS, ?Test, ?TestO) is semidet.
%
% Build Code Test.
%
% what this does...
%
%   strips away any currly brackets
%   converts cuts to cut_c/0
%   converts variable Ps to mpred_call_no_bc(P)
%
build_code_test(_Support,Test,TestO):- is_ftVar(Test),!,must(TestO=mpred_call_no_bc(Test)).
build_code_test(WS,{Test},TestO) :- !,build_code_test(WS,Test,TestO).
build_code_test(_Sup,!,cut_c):-!.
build_code_test(WS,rhs(Test),rhs(TestO)) :- !,build_code_test(WS,Test,TestO).
build_code_test(WS,Test,TestO):- is_list(Test),must_maplist(build_code_test(WS),Test,TestO).
build_code_test(_WS,(H:-B),clause_asserted_u(H,B)):- !.
build_code_test(_WS,M:(H:-B),clause_asserted_u(M:H,B)):- !.
build_code_test(WS,Test,TestO):- code_sentence_op(Test),Test=..[F|TestL],must_maplist(build_code_test(WS),TestL,TestLO),TestO=..[F|TestLO],!.
build_code_test(WS,Test,Test):- must(mpred_mark_as(WS,Test,pfcCallCodeTst)),!.
build_code_test(_,Test,Test).


%% build_consequent(+Support, +TestIn, -TestOut) is semidet.
%
% Build Consequent.
%
build_consequent(_      ,Test,Test):- is_ftVar(Test),!.
build_consequent(_      ,Test,TestO):-is_ftVar(Test),!,TestO=added(Test).
build_consequent(_Sup,!,{cut_c}):-!.
build_consequent(WS,'{}'(Test),'{}'(TestO)) :- !,build_code_test(WS,Test,TestO).
build_consequent(WS,rhs(Test),rhs(TestO)) :- !,build_consequent(WS,Test,TestO).
build_consequent(WS,Test,TestO):- is_list(Test),must_maplist(build_consequent(WS),Test,TestO).
build_consequent(WS,Test,TestO):- code_sentence_op(Test),Test=..[F|TestL],
   must_maplist(build_consequent(WS),TestL,TestLO),TestO=..[F|TestLO],!.
build_consequent(WS,Test,Test):-must(mpred_mark_as(WS,Test,pfcCreates)),!.
build_consequent(_ ,Test,Test).


%% code_sentence_op( :TermVar) is semidet.
%
% Code Sentence Oper..
%
code_sentence_op(Var):-is_ftVar(Var),!,fail.
code_sentence_op(rhs(_)).
code_sentence_op(~(_)).
code_sentence_op(-(_)).
code_sentence_op(-(_)).
code_sentence_op((_,_)).
code_sentence_op((_;_)).
code_sentence_op(\+(_)).
code_sentence_op(call(_)).
code_sentence_op(call_u(_)).
code_sentence_op(mpred_call_no_bc(_,_)).
code_sentence_op(Test:-_):-!,code_sentence_op(Test).
code_sentence_op(Test):- 
  predicate_property(Test,built_in),
  predicate_property(Test,meta_predicate(PP)), \+ (( arg(_,PP,N), N \= 0)).


%% all_closed(+C) is semidet.
%
% All Closed.
%
all_closed(C):- \+is_ftCompound(C)->true;(functor(C,_,A),A>1,\+((arg(_,C,Arg),is_ftVar(Arg)))),!.


%head_to_functor_name(I,F):- is_ftCompound(I),get_head(I,H),is_ftCompound(I),get_functor_name(I,F).
head_to_functor_name(I,F):- is_ftCompound(I),get_functor(I,F).


%% mpred_db_type(+VALUE1, ?Type) is semidet.
%
% PFC Database Type.
%
%  simple typeing for Pfc objects
%
mpred_db_type(Var,Type):- var(Var),!, Type=fact(_FT).
mpred_db_type(~_,Type):- !, Type=fact(_FT).
mpred_db_type(('==>'(_,_)),Type):- !, Type=rule.
mpred_db_type(('<==>'(_,_)),Type):- !, Type=rule.
mpred_db_type(('<-'(_,_)),Type):- !, Type=rule.
mpred_db_type(pt(_,_,_),Type):- !, Type=trigger.
mpred_db_type(pt(_,_),Type):- !, Type=trigger.
mpred_db_type(nt(_,_,_),Type):- !,  Type=trigger.
mpred_db_type(bt(_,_),Type):- !,  Type=trigger.
mpred_db_type(actn(_),Type):- !, Type=action.
mpred_db_type((('::::'(_,X))),Type):- !, mpred_db_type(X,Type).
mpred_db_type(((':'(_,X))),Type):- !, mpred_db_type(X,Type).
mpred_db_type(_,fact(_FT)):-
  %  if it''s not one of the above, it must be a fact!
  !.

mpred_assert_w_support(P,Support):- 
  (clause_asserted_u(P) ; assert_u_confirmed_was_missing(P)),
  !,
  mpred_add_support(P,Support).

mpred_asserta_w_support(P,Support):-
  (clause_asserted_u(P) ; asserta_u(P)),
  !,
  mpred_add_support(P,Support).

mpred_assertz_w_support(P,Support):-
  (clause_asserted_u(P) ; assertz_u(P)),
  !,
  mpred_add_support(P,Support).



%% clause_asserted_u(+Head) is semidet.
%
% PFC Clause For User Interface.
%

:- module_transparent(clause_asserted_call/2).
clause_asserted_call(H,B):-clause_asserted(H,B).


clause_asserted_u(MH):- sanity((nonvar(MH), ignore(show_failure(\+ is_static_predicate(MH))))),fail.
%clause_asserted_u(MH):- \+ ground(MH),must_notrace_pfc(fully_expand(change(assert,assert_u),MH,MA)),MA\=@=MH,!,clause_asserted_u(MA).
clause_asserted_u((MH:-B)):- must(nonvar(MH)), !, must(mnotrace(fix_mp(clause(clause,clause_asserted_u),MH,M,H))),!,
              (current_prolog_flag(unsafe_speedups,true) -> 
                 (clause_asserted_ii((M:H , B )))
                 /*; clause_asserted_u((M:H :- B ))*/
                 ; clause_asserted_i((M:H :- B ))).

clause_asserted_u(MH):- current_prolog_flag(unsafe_speedups,true), !,clause_asserted_ii(MH).
clause_asserted_u(MH):- must(mnotrace(fix_mp(clause(clause,clause_asserted_u),MH,M,H))),clause_asserted_ii(M:H).

clause_asserted_ii(H,B):- HB=(H:-B),copy_term(HB,HHBB),clause(H,B),variant(HHBB,HB),!.
clause_asserted_ii(H):-copy_term(H,HH),clause(H,true),variant(HH,H),!.

variant_m(_:H,_:HH):-!,H=@=HH.
variant_m(H,_:HH):-!,H=@=HH.
variant_m(_:H,HH):-!,H=@=HH.
variant_m(H,HH):-!,H=@=HH.

variant_u(HeadC,Head_copy):-variant_i(HeadC,Head_copy).


%% foreachl_do(+Binder, ?Body) is det.
%
% Foreachl Do.
%
foreachl_do(Binder,Body):- Binder,pfcl_do(Body),fail.
foreachl_do(_,_).


%% pfcl_do(+X) is semidet.
%
% executes P once and always succeeds.
%
pfcl_do(X):- X,!.
pfcl_do(_).


%% mpred_union(L1,L2,L3) is semidet.
%
%  true if set L3 is the result of appending sets
%  L1 and L2 where sets are represented as simple lists.
%
mpred_union([],L,L).
mpred_union([Head|Tail],L,Tail2):-  
  memberchk(Head,L),
  !,
  mpred_union(Tail,L,Tail2).
mpred_union([Head|Tail],L,[Head|Tail2]):-  
  mpred_union(Tail,L,Tail2).


%  mpred_conjoin(+Conjunct1,+Conjunct2,?Conjunction).
%  arg3 is a simplified expression representing the conjunction of
%  args 1 and 2.

mpred_conjoin(True,X,X):- True==true, !.
mpred_conjoin(X,True,X):- True==true, !.
mpred_conjoin(C1,C2,(C1,C2)).



%   File   : pfcdb.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Author :  Dan Corpron
%   Updated: 10/11/87, ...
%   Purpose: predicates to manipulate a Pfc database (e.ax. save,
% 	restore, reset, etc.)

%% mpred_reset() is det.
%
% removes all forward chaining rules and justifications from db.
%
mpred_reset:- 
  SPFT = spft(P,_ZF,_ZTrigger),
  lookup_u(SPFT),
  mpred_retract_i_or_warn(P),
  mpred_retract_i_or_warn(SPFT),
  fail.
mpred_reset:-
  mpred_database_item(T),!,
  mpred_warn("Couldn't full mpred_reset: ~p.~n",[T]), must(pp_DB),!,
  mpred_error("Pfc database not empty after mpred_reset, e.ax., ~p.~n",[T]),!.
mpred_reset:- mpred_trace_msg("Reset DB complete").

% true if there is some Pfc crud still in the database.
mpred_database_item(P):-
  mpred_database_term(F,A,Type),
  Type\=debug,
  P \= ~(_),
  Type\=setting,
  functor(H,F,A),
  clause_u(H,B), 
  B\= _:loop_check_nr(_),
  \+ (B=@@=(cwc, neg_in_code(_G))),
  \+ ( B= H),
  ((B== true)-> P=H; P=(H:B)).


mpred_retract_i_or_warn_1(X):- sanity(is_ftNonvar(X)), call_u(X), retract_u(X), !, mpred_trace_msg('~NSUCCESS: ~p~n',[retract_u(X)]).


mpred_retract_i_or_warn_0(X):- mpred_retract_i_or_warn_1(X).
mpred_retract_i_or_warn_0(spft(P,T,mfl(M,F,A))):- nonvar(A),mpred_retract_i_or_warn_1(spft(P,T,mfl(M,F,_))).
mpred_retract_i_or_warn_0(spft(P,mfl(M,F,A),T)):- nonvar(A),mpred_retract_i_or_warn_1(spft(P,mfl(M,F,_),T)).
mpred_retract_i_or_warn_0(spft(P,T,mfl(M,A,F))):- nonvar(A),mpred_retract_i_or_warn_1(spft(P,T,mfl(M,_,F))).
mpred_retract_i_or_warn_0(spft(P,mfl(M,A,F),T)):- nonvar(A),mpred_retract_i_or_warn_1(spft(P,mfl(M,_,F),T)).
mpred_retract_i_or_warn_0(spft(P,F,A)):- nonvar(A),mpred_retract_i_or_warn_1(spft(P,F,_)).
mpred_retract_i_or_warn_0(spft(P,A,T)):- nonvar(A),mpred_retract_i_or_warn_1(spft(P,_,T)).
mpred_retract_i_or_warn_0(spft(P,_,_)):- mpred_retract_i_or_warn_1(spft(P,_,_)).


mpred_retract_i_or_warn(X):- mpred_retract_i_or_warn_0(X).
mpred_retract_i_or_warn(SPFT):- \+ \+ SPFT = spft(_,a,a),!,fail.
mpred_retract_i_or_warn(X):- fail,
  mpred_warn("Couldn't retract_u ~p.~n",[X]),
  (debugging(dmiles)->rtrace(retract_u(X));true),!.
mpred_retract_i_or_warn(X):- 
  mpred_warn("Couldn't retract_u ~p.~n",[X]),!.




%   File   : pfcdebug.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Updated:
%   Purpose: provides predicates for examining the database and debugginh 
%   for Pfc.

%:- mpred_set_default(baseKB:mpred_warnings(_), baseKB:mpred_warnings(true)).
%  tms is one of {none,local,cycles} and controles the tms alg.
%:- baseKB:mpred_set_default(tms(_), tms(cycles)).
:-dynamic(baseKB:spft/3).
:-asserta(baseKB:spft(a,b,c)).
:-retractall(baseKB:spft(a,b,c)).

%  mpred_fact(P) is true if fact P was asserted into the database via add.


mpred_fact_mp(M,G):- current_predicate(_,M:G),\+ predicate_property(M:G,imported_from(_)),
  mpred_fact(G),ignore((lookup_u(G,Ref),clause_property(Ref,module(MW)))),MW=M.

mpred_fact(P):- mpred_fact(P,true).

%  mpred_fact(P,C) is true if fact P was asserted into the database via
%  add and contdition C is satisfied.  For example, we might do:
%  
%   mpred_fact(X,mpred_userFact(X))
% 

mpred_fact(P,C):- 
  mpred_get_support(P,_),
  mpred_db_type(P,fact(_FT)),
  mpred_call_no_bc(C).

%  mpred_facts(-ListofPmpred_facts) returns a list of facts added.

mpred_facts(L):- mpred_facts(_,true,L).

mpred_facts(P,L):- mpred_facts(P,true,L).

%  mpred_facts(Pattern,Condition,-ListofPmpred_facts) returns a list of facts added.

%% mpred_facts(+P, ?C, ?L) is semidet.
%
% PFC Facts.
%
mpred_facts(P,C,L):- setof(P,mpred_fact(P,C),L).


%% brake(+X) is semidet.
%
% Brake.
%
brake(X):-  X, dbreak.


% 
%  predicates providing a simple tracing facility
% 

% this is here for upward compat. - should go away eventually.
mpred_trace_op(Add,P):- not_not_ignore_mnotrace((get_source_ref_stack(Why), !, mpred_trace_op(Add,P,Why))).


mpred_trace_op(Add,P,S):-  
   not_not_ignore_mnotrace((mpred_trace_maybe_print(Add,P,S),
      mpred_trace_maybe_break(Add,P,S))).
   

mpred_trace_maybe_print(Add,P,S):-
  not_not_ignore_mnotrace((
  \+ get_mpred_is_tracing(P) -> true;
  (
   ((to_u(S,U),atom(U))
       -> wdmsg("~NOP: ~p (~p) ~p",[Add,U,P])
        ; wdmsg("~NOP: ~p (:) ~p~N\tSupported By: ~q",[Add,P,S]))))),!.

to_u(S,U):-S=(U,ax),!.
to_u(S,U):-S=(U,_),!.
to_u(S,U):-S=(U),!.

mpred_trace_maybe_break(Add,P0,_ZS):-
  get_head_term(P0,P),
   (
  \+ call_u(mpred_is_spying_pred(P,Add)) -> true;
   (wdmsg("~NBreaking on ~p(~p)",[Add,P]),
    dbreak)).
   



mpred_trace:- mpred_trace(_).

mpred_trace(Form0):-  get_head_term(Form0,Form),
  assert_u_no_dep(mpred_is_spying_pred(Form,print)).

%% get_mpred_is_tracing(:PRED) is semidet.
%
% PFC If Is A Tracing.
%
get_mpred_is_tracing(_):-!,fail.
get_mpred_is_tracing(Form0):- get_head_term(Form0,Form), t_l:hide_mpred_trace_exec,!,
  \+ \+ ((mnotrace(call_u(mpred_is_spying_pred(Form,print))))).
get_mpred_is_tracing(Form0):- get_head_term(Form0,Form),
  once(t_l:mpred_debug_local ; tracing ; clause_asserted_u(mpred_is_tracing_exec) ;
     call_u(mpred_is_spying_pred(Form,print))).


%% mpred_trace(+Form, ?Condition) is semidet.
%
% PFC Trace.
%
mpred_trace(Form0,Condition):- get_head_term(Form0,Form),  
  assert_u_no_dep((mpred_is_spying_pred(Form,print):- Condition)).

mpred_spy(Form):- mpred_spy(Form,[add,rem],true).

mpred_spy(Form,Modes):- mpred_spy(Form,Modes,true).

mpred_spy(Form0,List,Condition):- is_list(List),!,get_head_term(Form0,Form),  
  !,must_maplist(mpred_spy1(Condition,Form),List).
  
mpred_spy(Form0,Mode,Condition):- get_head_term(Form0,Form), 
  mpred_spy1(Condition,Form,Mode).

mpred_spy1(Condition,Form0,Mode):- get_head_term(Form0,Form), 
  assert_u_no_dep((mpred_is_spying_pred(Form,Mode):- Condition)).

mpred_nospy:- mpred_nospy(_,_,_).

mpred_nospy(Form):- mpred_nospy(Form,_,_).

mpred_nospy(Form0,Mode,Condition):- get_head_term(Form0,Form), 
  clause_u(mpred_is_spying_pred(Form,Mode), Condition, Ref),
  erase(Ref),
  fail.
mpred_nospy(_,_,_).

mpred_notrace:- mpred_untrace.
mpred_untrace:- mpred_untrace(_).
mpred_untrace(Form0):- get_head_term(Form0,Form), retractall_u(mpred_is_spying_pred(Form,print)).


not_not_ignore_mnotrace(G):- ignore(mnotrace(\+ \+ G)).

% needed:  mpred_trace_rule(Name)  ...

log_failure(ALL):- cnotrace((log_failure_red,maybe_mpred_break(ALL),log_failure_red)).
log_failure_red:- cnotrace(doall((between(1,3,_),wdmsg(color(red,"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")),fail))).

maybe_mpred_break(Info):- (t_l:no_mpred_breaks->true;(debugging(mpred)->dtrace(dmsg(Info));(dmsg(Info)))).

% if the correct flag is set, dtrace exection of Pfc
mpred_trace_msg(Info):- not_not_ignore_mnotrace(((((clause_asserted_u(mpred_is_tracing_exec);tracing)->in_cmt(wdmsg(Info));true)))).
mpred_trace_msg(Format,Args):- not_not_ignore_mnotrace((((clause_asserted_u(mpred_is_tracing_exec);tracing)-> wdmsg(Format,Args)))),!.
% mpred_trace_msg(Format,Args):- not_not_ignore_mnotrace((((format_to_message(Format,Args,Info),mpred_trace_msg(Info))))).

mpred_warn(Info):- not_not_ignore_mnotrace((((lookup_u(mpred_warnings(true));tracing) -> 
  wdmsg(warn(mpred,Info)) ; mpred_trace_msg('WARNING/PFC:  ~p ',[Info])),
  maybe_mpred_break(Info))).

mpred_warn(Format,Args):- not_not_ignore_mnotrace((((format_to_message(Format,Args,Info),mpred_warn(Info))))).

mpred_error(Info):- not_not_ignore_mnotrace(((tracing -> wdmsg(error(pfc,Info)) ; mpred_warn(error(Info))))).
mpred_error(Format,Args):- not_not_ignore_mnotrace((((format_to_message(Format,Args,Info),mpred_error(Info))))).

mpred_trace_exec:- assert_u_no_dep(mpred_is_tracing_exec).
mpred_watch:- mpred_trace_exec.
mpred_trace_all:- mpred_trace_exec,mpred_trace,mpred_set_warnings(true).
mpred_notrace_exec:- retractall_u(mpred_is_tracing_exec).

mpred_nowatch:-  retractall_u(mpred_is_tracing_exec).

:- thread_local(t_l:hide_mpred_trace_exec/0).

%% with_mpred_trace_exec( +P) is semidet.
%
% Using Trace exec.
%

% with_mpred_trace_exec(P):- wno_tl_e(t_l:hide_mpred_trace_exec,w_tl_e(t_l:mpred_debug_local, must(show_if_debug(P)))).
with_mpred_trace_exec(P):- 
 ((\+ lookup_u(mpred_is_tracing_exec),mpred_trace_exec)
      -> Exit = mpred_notrace_exec; Exit = true),
   wno_tl_e(t_l:hide_mpred_trace_exec,
       w_tl_e(t_l:mpred_debug_local, 
           must(show_if_debug(P)))),
        call(Exit).

%% with_mpred_trace_exec( +P) is semidet.
%
% Without Trace exec.
%
with_no_mpred_trace_exec(P):- 
   wno_tl_e(t_l:mpred_debug_local,w_tl_e(t_l:hide_mpred_trace_exec, must(/*show_if_debug*/(P)))).

%% show_if_debug( :GoalA) is semidet.
%
% Show If Debug.
%
:- meta_predicate(show_if_debug(0)).
% show_if_debug(A):- !,show_call(why,A).
show_if_debug(A):-  get_mpred_is_tracing(A) -> show_call(A) ; A.

:- thread_local(t_l:mpred_debug_local/0).

%% mpred_is_silient is det.
%
% If Is A Silient.
%
mpred_is_silient :- t_l:hide_mpred_trace_exec,!, \+ tracing.
mpred_is_silient :- notrace(( \+ t_l:mpred_debug_local, \+ lookup_u(mpred_is_tracing_exec), \+ lookup_u(mpred_is_spying_pred(_)), 
  current_prolog_flag(debug,false), is_release)) ,!.


%% mpred_test(+P) is semidet.
%
% PFC Test.
%
mpred_test(_):- (compiling; current_prolog_flag(xref,true)),!.
mpred_test(G):- mpred_is_silient,!, with_no_mpred_trace_exec(must(mpred_test_fok(G))).
mpred_test(G):- with_mpred_trace_exec(must(mpred_test_fok(G))).

oinfo(O):- xlisting((O, - spft, - ( ==> ), - pt , - nt , - bt , - mdefault, - lmcache)).

why_was_true(P):- mpred_why(P),!.
why_was_true(P):- dmsg(justfied_true(P)),!.

mpred_test_fok(\+ G):-!, ( \+ call_u(G) -> wdmsg(passed_mpred_test(\+ G)) ; (log_failure(failed_mpred_test(\+ G)),!,ignore(why_was_true(G)),!,fail)).
mpred_test_fok(G):- (call_u(G) -> ignore(sanity(why_was_true(G))) ; (log_failure(failed_mpred_test(G))),!,fail).


mpred_load_term(:- module(_,L)):-!, mpred_call_no_bc(maplist(export,L)).
mpred_load_term(:- TermO):- mpred_call_no_bc(TermO).
mpred_load_term(TermO):-mpred_ain_object(TermO).

mpred_load(In):- is_stream(In),!,
   repeat,
   line_count(In,_Lineno),
   % double_quotes(_DQBool)
   Options = [variables(_Vars),variable_names(VarNames),singletons(_Singletons),comment(_Comment)],
   catchv((read_term(In,Term,[syntax_errors(error)|Options])),E,(dmsg(E),fail)),
   set_varname_list(VarNames),expand_term(Term,TermO),mpred_load_term(TermO),
   Term==end_of_file,
   close(In).

mpred_load(PLNAME):- % unload_file(PLNAME),
   open(PLNAME, read, In, []),
   mpred_load(In).

% 
%  These control whether or not warnings are printed at all.
%    mpred_warn.
%    nompred_warn.
% 
%  These print a warning message if the flag mpred_warnings is set.
%    mpred_warn(+Message)
%    mpred_warn(+Message,+ListOfArguments)
% 

mpred_warn:- 
  retractall_u(mpred_warnings(_)),
  assert_u_no_dep(mpred_warnings(true)).

nompred_warn:-
  retractall_u(mpred_warnings(_)),
  assert_u_no_dep(mpred_warnings(false)).
 

%%  mpred_set_warnings(+TF) is det.
%   true = sets flag to cause Pfc warning messages to print.
%   false = sets flag to cause Pfc warning messages not to print.
% 
mpred_set_warnings(True):- 
  retractall_u(mpred_warnings(_)),
  assert_u_no_dep(mpred_warnings(True)).
mpred_set_warnings(false):- 
  retractall_u(mpred_warnings(_)).

%   File   : pfcjust.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Updated:
%   Purpose: predicates for accessing Pfc justifications.
%   Status: more or less working.
%   Bugs:

%  *** predicates for exploring supports of a fact *****

justification(F,J):- supporters_list(F,J).

justifications(F,Js):- bagof_nr(J,justification(F,J),Js).

mpred_why(F,Js):- bagof_nr(J,justification(F,J),Js).



%%  mpred_basis_list(+P,-L)
%
%  is true iff L is a list of "base" facts which, taken
%  together, allows us to deduce P.  A mpred "based on" list fact is an axiom (a fact 
%  added by the user or a raw Prolog fact (i.e. one w/o any support))
%  or an assumption.
%
mpred_basis_list(F,[F]):- (mpred_axiom(F) ; mpred_assumption(F)),!.

mpred_basis_list(F,L):-
  % i.e. (reduce 'append (map 'mpred_basis_list (justification f)))
  justification(F,Js),
  bases_union(Js,L).


%%  bases_union(+L1,+L2).
%
%  is true if list L2 represents the union of all of the 
%  facts on which some conclusion in list L1 is based.
%
bases_union([],[]).
bases_union([X|Rest],L):-
  mpred_basis_list(X,Bx),
  bases_union(Rest,Br),
  mpred_union(Bx,Br,L).
	
mpred_axiom(F):- 
  mpred_get_support(F,(_,ax)).

%% mpred_assumption(P)
% 
%  an mpred_assumption is a failed goal, i.e. were assuming that our failure to 
%  prove P is a proof of not(P)
%
mpred_assumption(P):- nonvar(P), mpred_unnegate(P,_).
   

%% mpred_assumptions( +X, +AsSet) is semidet.
%
% true if AsSet is a set of assumptions which underly X.
%
mpred_assumptions(X,[X]):- mpred_assumption(X).
mpred_assumptions(X,[]):- mpred_axiom(X).
mpred_assumptions(X,L):-
  justification(X,Js),
  do_assumpts(Js,L).


%% do_assumpts(+Set1,?Set2) is semidet.
%
% Assumptions Secondary Helper.
%
do_assumpts([],[]).
do_assumpts([X|Rest],L):-
  mpred_assumptions(X,Bx),
  do_assumpts(Rest,Br),
  mpred_union(Bx,Br,L).  


%  mpred_proofTree(P,T) the proof tree for P is T where a proof tree is
%  of the form
% 
%      [P , J1, J2, ;;; Jn]         each Ji is an independent P justifier.
%           ^                         and has the form of
%           [J11, J12,... J1n]      a list of proof trees.


%% mpred_child(+P,?Q) is semidet.
%
% is true iff P is an immediate justifier for Q.
%
mpred_child(P,Q):-
  mpred_get_support(Q,(P,_)).

mpred_child(P,Q):-
  mpred_get_support(Q,(_,Trig)),
  mpred_db_type(Trig,trigger),
  mpred_child(P,Trig).


%% mpred_children(+P, ?L) is semidet.
%
% PFC Children.
%
mpred_children(P,L):- bagof_nr(C,mpred_child(P,C),L).



%% mpred_descendant(+P, ?Q) is semidet.
%
% mpred_descendant(P,Q) is true iff P is a justifier for Q.
%
mpred_descendant(P,Q):- 
   mpred_descendant1(P,Q,[]).


%% mpred_descendant1(+P, ?Q, ?Seen) is semidet.
%
% PFC Descendant Secondary Helper.
%
mpred_descendant1(P,Q,Seen):-
  mpred_child(X,Q),
  (\+ member(X,Seen)),
  (P=X ; mpred_descendant1(P,X,[X|Seen])).
  

%% mpred_descendants(+P, ?L) is semidet.
%
% PFC Descendants.
%
mpred_descendants(P,L):- 
  bagof_nr(Q,mpred_descendant1(P,Q,[]),L).


bagof_nr(T,G,B):- no_repeats(B,(bagof(T,G,B))).

bagof_or_nil(T,G,B):- (bagof_nr(T,G,B) *-> true; B=[]).

% 
%  predicates for manipulating support relationships
% 

%  mpred_add_support(+Fact,+Support)
mpred_add_support(P,(Fact,Trigger)):-
  (Trigger= nt(F,Condition,Action) -> 
    (mpred_trace_msg('~N~n\tAdding NEG mpred_do_fcnt via support~n\t\ttrigger: ~p~n\t\tcond: ~p~n\t\taction: ~p~n\t from: ~p~N',
      [F,Condition,Action,mpred_add_support(P,(Fact,Trigger))]));true),
  assert_u_confirm_if_missing(spft(P,Fact,Trigger)).

%  mpred_add_support(+Fact,+Support)
mpred_add_support_fast(P,(Fact,Trigger)):-
  (Trigger= nt(F,Condition,Action) -> 
    (mpred_trace_msg('~N~n\tAdding NEG mpred_do_fcnt via support~n\t\ttrigger: ~p~n\t\tcond: ~p~n\t\taction: ~p~n\t from: ~p~N',
      [F,Condition,Action,mpred_add_support_fast(P,(Fact,Trigger))]));true),
  assert_mu(spft(P,Fact,Trigger)).


mpred_get_support(P,(Fact,Trigger)):-
      lookup_u(spft(P,Fact,Trigger)).


mpred_rem_support_if_exists(P,(Fact,Trigger)):-
 SPFT = spft(P,Fact,Trigger),
  lookup_u(SPFT),
  once(mpred_retract_i_or_warn(SPFT)).


mpred_rem_support(P,(Fact,Trigger)):-
  closest_u(spft(P,Fact,Trigger),spft(P,FactO,TriggerO)),
  mpred_retract_i_or_warn_1(spft(P,FactO,TriggerO)).
mpred_rem_support(P,S):-
  mpred_retract_i_or_warn(spft(P,Fact,Trigger)),
  ignore((Fact,Trigger)=S).


closest_u(Was,WasO):-clause_asserted_u(Was),!,Was=WasO.
closest_u(Was,WasO):-lookup_u(Was),Was=WasO,!.
closest_u(Was,WasO):-lookup_u(WasO),ignore(Was=WasO),!.

mpred_collect_supports(Tripples):-
  bagof_or_nil(Tripple, mpred_support_relation(Tripple), Tripples).

mpred_support_relation((P,F,T)):-
  lookup_u(spft(P,F,T)).

mpred_make_supports((P,S1,S2)):- 
  mpred_add_support(P,(S1,S2)),
  (mpred_ain_object(P); true),
  !.

%%  mpred_trigger_key(+Trigger,-Key) 
% 
%  Arg1 is a trigger.  Key is the best term to index it on.
% 
%  Get a key from the trigger that will be used as the first argument of
%  the trigger base clause that stores the trigger.

mpred_trigger_key(X,X):- var(X), !.
mpred_trigger_key(pt(Key,_),Key).
mpred_trigger_key(pk(Key,_,_),Key).
mpred_trigger_key(nt(Key,_,_),Key).
mpred_trigger_key(Key,Key).

% For chart parser
mpred_trigger_key(chart(word(W),_ZL),W):- !.
mpred_trigger_key(chart(stem([Char1|_ZRest]),_ZL),Char1):- !.
mpred_trigger_key(chart(Concept,_ZL),Concept):- !.
mpred_trigger_key(X,X).



%% pp_DB is semidet.
%
% Pretty Print All.
%
pp_DB:-
 must_det_l((
  pp_db_facts,
  pp_db_rules,
  pp_db_triggers,
  pp_db_supports)).

%  pp_db_facts ...

pp_db_facts:- ignore(pp_db_facts(_,true)).

pp_db_facts(Pattern):- pp_db_facts(Pattern,true).

pp_db_facts(P,C):-
  mpred_facts(P,C,L),
  mpred_classifyFacts(L,User,Pfc,_ZRule),
  format("~N~nUser added facts:",[]),
  pp_db_items(User),
  format("~N~nPfc added facts:",[]),
  pp_db_items(Pfc).

%  printitems clobbers it''s arguments - beware!


pp_db_items(Var):-var(Var),!,format("~N  ~p",[Var]).
pp_db_items([]):-!.
pp_db_items([H|T]):- !,
  % numbervars(H,0,_),
  format("~N  ~p",[H]),
  nonvar(T),pp_db_items(T).
pp_db_items(Var):-format("~N  ~p",[Var]).

mpred_classifyFacts([],[],[],[]).

mpred_classifyFacts([H|T],User,Pfc,[H|Rule]):-
  mpred_db_type(H,rule),
  !,
  mpred_classifyFacts(T,User,Pfc,Rule).

mpred_classifyFacts([H|T],[H|User],Pfc,Rule):-
  % get_source_ref(UU),
  get_first_user_reason(H,_UU),
  !,
  mpred_classifyFacts(T,User,Pfc,Rule).

mpred_classifyFacts([H|T],User,[H|Pfc],Rule):-
  mpred_classifyFacts(T,User,Pfc,Rule).

pp_db_rules:-
 format("~NRules...~n",[]),
  bagof_or_nil((P==>Q),clause_u((P==>Q),true),R1),
  pp_db_items(R1),
  bagof_or_nil((P<==>Q),clause_u((P<==>Q),true),R2),
  pp_db_items(R2),
  bagof_or_nil((P<-Q),clause_u((P<-Q),true),R3),
  pp_db_items(R3).

pp_db_triggers:-
  format("~NPositive triggers...~n",[]),
  bagof_or_nil(pt(T,B),lookup_u(pt(T,B)),Pts),
  pp_db_items(Pts),
  format("~NNegative triggers...~n",[]),
  bagof_or_nil(nt(A,B,C),lookup_u(nt(A,B,C)),Nts),
  pp_db_items(Nts),
  format("~NGoal triggers...~n",[]),
  bagof_or_nil(bt(A,B),lookup_u(bt(A,B)),Bts),
  pp_db_items(Bts).

pp_db_supports:- 
  % temporary hack.
  format("~NSupports...~n",[]),
  setof((P >= S), mpred_get_support(P,S),L),
  pp_db_items(L),!.
pp_db_supports:- bagof_or_nil((P =< S),mpred_get_support(P,S),Bts),pp_db_items(Bts),!.

%   File   : mpred_why.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Updated:
%   Purpose: predicates for interactively exploring Pfc justifications.

% ***** predicates for brousing justifications *****

:- use_module(library(lists)).

pp_why:-mpred_why.

mpred_why:- 
  lookup_u(why_buffer(P,_)),
  mpred_why(P).

pp_why(A):-mpred_why(A).

mpred_why(N):-
  number(N),
  !,
  lookup_u(why_buffer(P,Js)),
  mpred_handle_why_command(N,P,Js).

mpred_why(M:P):-atom(M),!,call_from_module(M,mpred_why(P)).
mpred_why(P):-
  justifications(P,Js),
  retractall_u(why_buffer(_,_)),
  assert_u_no_dep(why_buffer(P,Js)),
  mpred_whyBrouse(P,Js).

mpred_why1(P):-
  justifications(P,Js),
  mpred_whyBrouse(P,Js).

% non-interactive
mpred_whyBrouse(P,Js):- 
   must(mnotrace(in_cmt((mpred_pp_db_justifications(P,Js))))), !.

% Interactive
mpred_whyBrouse(P,Js):-
  mpred_pp_db_justifications(P,Js),
  mpred_prompt_ask(' >> ',Answer),
  mpred_handle_why_command(Answer,P,Js).

mpred_handle_why_command(q,_,_):- !.
mpred_handle_why_command(h,_,_):- 
  !,
  format("~N
Justification Brouser Commands:
 q   quit.
 N   focus on Nth justification.
 N.M brouse step M of the Nth justification
 user   up a level ~n",
  []).

mpred_handle_why_command(N,_ZP,Js):-
  float(N),
  !,
  mpred_select_justification_node(Js,N,Node),
  mpred_why1(Node).

mpred_handle_why_command(u,_,_):-
  % u=up
  !.

mpred_unhandled_command(N,_,_):-
  integer(N),
  !,
  format("~N~p is a yet unimplemented command.",[N]),
  fail.

mpred_unhandled_command(X,_,_):-
 format("~N~p is an unrecognized command, enter h. for help.",[X]),
 fail.
  
mpred_pp_db_justifications(P,Js):-
 must(mnotrace(( format("~NJustifications for ~p:",[P]),
  mpred_pp_db_justification1(Js,1)))).

mpred_pp_db_justification1([],_).

mpred_pp_db_justification1([J|Js],N):-
  % show one justification and recurse.
  nl,
  mpred_pp_db_justifications2(J,N,1),
  N2 is N+1,
  mpred_pp_db_justification1(Js,N2).

mpred_pp_db_justifications2([],_,_).

mpred_pp_db_justifications2([C|Rest],JustNo,StepNo):- 
 (StepNo==1->fmt('~N~n',[]);true),
  format("~N    ~p.~p ~p",[JustNo,StepNo,C]),
  StepNext is 1+StepNo,
  mpred_pp_db_justifications2(Rest,JustNo,StepNext).

mpred_prompt_ask(Info,Ans):-
  format("~N~p",[Info]),
  read(Ans).

mpred_select_justification_node(Js,Index,Step):-
  JustNo is integer(Index),
  nth1(JustNo,Js,Justification),
  StepNo is 1+ integer(Index*10 - JustNo*10),
  nth1(StepNo,Justification,Step).


%%  mpred_supported(+P) is semidet.
%
%  succeeds if P is "supported". What this means
%  depends on the TMS mode selected.
%
mpred_supported(P):- 
  get_tms_mode(P,Mode),
  mpred_supported(Mode,P).

get_tms_mode(_P,Mode):- lookup_u(tms(Mode)),!.
get_tms_mode(_P,Mode):- must(Mode=local).

%%  mpred_supported(+TMS,+P) is semidet.
%
%  succeeds if P is "supported". What this means
%  depends on the TMS mode supplied.
%
mpred_supported(local,P):- !, mpred_get_support(P,_).
mpred_supported(cycles,P):-  !, well_founded(P).
mpred_supported(_,_):- true.


%% well_founded(+Fact) is semidet.
%
% a fact is well founded if it is supported by the user
%  or by a set of facts and a rules, all of which are well founded.
% 
well_founded(Fact):- each_E(well_founded_0,Fact,[_]).

well_founded_0(F,_):-
  % supported by user (mpred_axiom) or an "absent" fact (mpred_assumption).
  (mpred_axiom(F) ; mpred_assumption(F)),
  !.

well_founded_0(F,Descendants):-
  % first make sure we aren't in a loop.
  (\+ memberchk(F,Descendants)),
  % find a justification.
  supporters_list(F,Supporters),
  % all of whose members are well founded.
  well_founded_list(Supporters,[F|Descendants]),
  !.

%%  well_founded_list(+List,-Decendants) is det.
%
% simply maps well_founded over the list.
%
well_founded_list([],_).
well_founded_list([X|Rest],L):-
  well_founded_0(X,L),
  well_founded_list(Rest,L).

%% supporters_list(+F,-ListofSupporters) is det.
%
% where ListOfSupports is a list of the
% supports for one justification for fact F -- i.e. a list of facts which,
% together allow one to deduce F.  One of the facts will typically be a rule.
% The supports for a user-defined fact are: [u].
%
supporters_list(F,[Fact|MoreFacts]):-
  mpred_get_support(F,(Fact,Trigger)),
  triggerSupports(Trigger,MoreFacts).

is_file_ref(A):-compound(A),A=mfl(_,_,_).

triggerSupports(uWas(_),[]):-!.
triggerSupports(ax,[]):-!.
triggerSupports(U,[(U)]):- is_file_ref(U),!.
triggerSupports(U,[uWas(U)]):- get_source_ref((U1,U2))->member(U12,[U1,U2]),U12=@=U.
triggerSupports(Trigger,[Fact|MoreFacts]):-
  mpred_get_support(Trigger,(Fact,AnotherTrigger)),
  triggerSupports(AnotherTrigger,MoreFacts).

:-module_transparent(mpred_ain/1).
:-module_transparent(mpred_aina/1).
:-module_transparent(mpred_ainz/1).




:- '$current_source_module'(M),forall(mpred_database_term(F,A,_),(abolish(mpred_pfc:F/A),abolish(user:F/A),abolish(M:F/A))).
% :- initialization(ensure_abox(baseKB)).


:- set_prolog_flag(mpred_pfc_file,true).
% local_testing

end_of_file.



:- must(mpred_reset).

:- defaultAssertMt(M),dynamic((M:current_ooZz/1,M:default_ooZz/1,M:if_mooZz/2)).

:- mpred_trace.
:- mpred_watch.


% this should have been ok
% (if_mooZz(Missing,Create) ==> ((\+ Missing/(Missing\==Create), \+ Create , \+ ~(Create)) ==> Create)).
:- ((mpred_ain((if_mooZz(Missing,Create) ==> 
 ( ( \+ Missing/ \+ (Missing=@@=Create)) ==> Create))))).

:- mpred_ain((default_ooZz(X) ==> if_mooZz(current_ooZz(_),current_ooZz(X)))).

:- mpred_ain(default_ooZz(booZz)).

:- mpred_test(current_ooZz(booZz)).

% :- pp_DB.

:- (mpred_ain(current_ooZz(fooZz))).

:- mpred_test(\+current_ooZz(booZz)).

:- (mpred_ain(\+ current_ooZz(fooZz))).

:- mpred_test(current_ooZz(booZz)).

:- (mpred_withdraw( default_ooZz(booZz) )).

:- listing([current_ooZz,default_ooZz]).

:- mpred_test( \+current_ooZz(booZz)).

:- mpred_ain(~ current_ooZz(fooZz)).

% :- pp_DB.

:- mpred_test(~current_ooZz(fooZz)).

:- mpred_ain(default_ooZz(booZz)).

:- mpred_test(current_ooZz(booZz)).

:- mpred_reset.

