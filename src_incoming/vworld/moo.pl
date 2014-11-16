/** <module> 
% This module defines the module types that we use:
% utility,planner,parser,action,database,effects,spawning_loading,connection
% It is the basic module system of the Logicmoo MUD
%
% Project Logicmoo: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
:- module(moo,[current_context_module/1,
    term_expansion_local/2,
         register_module_type/1,          
         end_module_type/1,
         op(1120,fx,export),
         register_timer_thread/3]).

:-dynamic_multifile_exported loading_module_h/1.
:-dynamic_multifile_exported(mpred_prop/2).
:-dynamic_multifile_exported(mpred_arity/2).
:-dynamic_multifile_exported(never_type/1).
mpred_prop(mpred_prop,prologOnly).
mpred_prop(mpred_arity,prologOnly).
mpred_prop(never_type,prologOnly).
mpred_arity(mpred_prop,2).
mpred_arity(mpred_arity,2).
mpred_arity(never_type,1).

:-export(is_stable/0).
is_stable:-fail.

:-export(is_release/0).
is_release :- fail,1 is random(3).
:-export(not_is_release/0).
not_is_release :- true. % 1 is random(3).
simple_code :- fail.
save_in_dbase_t:-true.
not_simple_code :- \+ simple_code.
type_error_checking:-false.

:- set_prolog_flag(double_quotes, atom).
:- set_prolog_flag(double_quotes, string).

:- '@'((use_module(logicmoo(logicmoo_util/logicmoo_util_bugger)),
         use_module(logicmoo(logicmoo_util/logicmoo_util_library)),
         use_module(logicmoo(logicmoo_util/logicmoo_util_ctx_frame)),
         use_module(logicmoo(logicmoo_util/logicmoo_util_strings)),
         use_module(logicmoo(logicmoo_util/logicmoo_util_terms)),
         use_module(logicmoo(logicmoo_util/logicmoo_util_dcg))),'user').

% ========================================
% include_moo_files(MASK)
% ========================================

include_moo_files(Mask):- expand_file_name(Mask,X),
     forall(member(E,X),ensure_moo_loaded(E)).
/*
module(M,Preds):-
    'format'(user_error,'% visting module ~w.~n',[M]),
    forall(member(P,Preds),dynamic_multifile_exported(P)).
*/
scan_updates:-thread_property(X,alias(loading_code)),thread_property(X,status(running)),!.
scan_updates:-!.
scan_updates:-ignore(catch(make,_,true)).


do_term_expansions:- context_module(CM), notrace(do_term_expansions(CM)).

do_term_expansions(_):- thread_self(ID),always_expand_on_thread(ID),!.
do_term_expansions(_):- always_transform_heads,not(prevent_transform_moo_preds),!.
do_term_expansions(_):- is_compiling_clause.
do_term_expansions(CM):- may_moo_term_expand(CM),!, not(ended_transform_moo_preds), not(prevent_transform_moo_preds).

check_term_expansions:- not(do_term_expansions).


:- meta_predicate locate_moo_file(:,-).

locate_moo_file(I,O):-locate_moo_file0(I,O),!.
locate_moo_file(_:O,O):-!.
locate_moo_file(O,O).
locate_moo_file0(user:SpecPre, Path):-!,locate_moo_file0(SpecPre, Path).
locate_moo_file0(_:SpecPre, Path):-!,locate_moo_file0(SpecPre, Path).
locate_moo_file0(SpecPre, Path) :-
        catch((expand_file_search_path(SpecPre,Spec)),_,fail),
        catch(absolute_file_name(Spec,
                           [ file_type(prolog),
                             access(read)
                           ],
                           Path),_,fail),
        exists_file(Path),!.

:- meta_predicate ensure_moo_loaded(:).

% the once/1s here arte just for dmiles institional memory
ensure_moo_loaded(A) :-
   setup_call_cleanup(once(asserta(may_moo_term_expand(_))),
        load_moo_files(A),
        once(retract(may_moo_term_expand(asdasdasd)))).

:- meta_predicate load_moo_files(:,+).

load_moo_files(F0):-!,user_use_module(F0).
% load_moo_files(F0):-use_module(F0).

load_moo_files(M:F0,List):-!,
  locate_moo_file(M:F0,F),  % scope_settings  expand(true),register(false),
  % 'format'(user_error,'%  ~q + ~q -> ~q.~n',[M,F0,F]),
  load_files(F,[if(not_loaded), must_be_module(true)|List]).
   %load_files(F,[redefine_module(false),if(not_loaded),silent(false),reexport(true),must_be_module(true)|List]).   
load_moo_files(M:F0,List):-
  locate_moo_file(M:F0,F),  % scope_settings
  'format'(user_error,'% load_moo_files_M ~q.~n',[M=locate_moo_file(F0,F)]),
   load_files(F,[redefine_module(false),module(M),expand(true),if(not_loaded),reexport(true),register(false),silent(false),must_be_module(true)|List]).



register_timer_thread(Name,_Seconds,_OnTick):-current_thread(Name,_Status).
register_timer_thread(Name,Seconds,OnTick):-
   thread_create(tick_every(Name,Seconds,OnTick),_ID,[alias(Name)]). 

tick_every(Name,Seconds,OnTick):-repeat,sleep(Seconds),catch(OnTick,E,dmsg(caused(Name,OnTick,E))),fail.

hdr_debug(_,_):-!.
hdr_debug(F,A):-'format'(F,A).
:-meta_predicate term_expansion_local(?,?),term_expansion_local0(?,?).
% :-meta_predicate user:term_expansion(?,?).


term_expansion_local(X,_):-not(compound(X)),!,fail.
term_expansion_local( ((':-'(_))) , _ ):-!,fail.

term_expansion_local(_:B1,B2):-!,term_expansion_local(B1,B2),!.

term_expansion_local(((H1:-B1)),H2B2):- !,
   nonvar(B1),  current_context_module(CM),!,        
      functor_catch(H1,F,A), atom_concat(P,'_hook',F),!,atomic_list_concat_catch([_,_|_],'_',P),
      H1=..[F|ARGS], H2=..[P|ARGS],
      B2 = '@'((nop(CM), B1), CM ),
      module_transparent((P/A)),
      % copy_term(H2,META), meta_predicate(META),
      dynamic(P/A),
      dynamic_multifile_exported(P/A),
      multifile(P/A),
      ignore(H2B2 = ((H2 :- B2))),!.


term_expansion_local(X,Y):- compound(X),loading_module_h(CM),functor_catch(X,F,A),term_expand_local(CM,X,F,A,Y).



term_expand_local(CM,X,F,A,Y):-findall(Y,term_expand_local_each(CM,X,F,A,Y),Ys), Ys == [],!,fail.  

term_expand_local_each(_,_,F,A,_):- member(F / A,[never_expand]),!,fail.
term_expand_local_each(CM,X,F,A,X):-registered_module_type(CM,utility),dynamic_multifile_exported(F/A).
term_expand_local_each(CM,X,F,A,X):-registered_module_type(CM,dynamic),dynamic(F/A).

term_expansion_local0(A,B):- compound(A),term_expansion_local(A,B),!.

% user:term_expansion(X,Y):- term_expansion_local0(X,Y).



current_context_module(Ctx):-loading_module_h(Ctx),!.
current_context_module(Ctx):-context_module(Ctx).

% ========================================
% begin/end_transform_moo_preds
% ========================================

:-thread_local is_compiling_clause/0.
is_compiling:-is_compiling_clause;compiling.
:-thread_local ended_transform_moo_preds/0, always_expand_on_thread/1, prevent_transform_moo_preds/0, may_moo_term_expand/1, always_transform_heads/0.
:-module_transparent begin_transform_moo_preds/0, end_transform_moo_preds/0.
:-export(((begin_transform_moo_preds/0,end_transform_moo_preds/0))).
begin_transform_moo_preds:- retractall(ended_transform_moo_preds),context_module(CM),asserta(may_moo_term_expand(CM)).
end_transform_moo_preds:- retractall(ended_transform_moo_preds),asserta(ended_transform_moo_preds).

% ========================================
% register_module_type/end_module_type
% ========================================
:- module_transparent register_module_type/1.
:- dynamic_multifile_exported registered_module_type/2.

register_module_type(Type):-current_context_module(CM),register_module_type(CM,Type).
register_module_type(CM,Types):-is_list(Types),!,forall(member(T,Types),register_module_type(CM,T)).
register_module_type(CM,Type):-asserta_new(registered_module_type(CM,Type)).

:-export(end_module_type/2).
end_module_type(Type):-current_context_module(CM),end_module_type(CM,Type).
end_module_type(CM,Type):-retractall(registered_module_type(CM,Type)).

% :- include(logicmoo('vworld/moo_header.pl')).

:- register_module_type(utility).

:- include(logicmoo('dbase/dbase.pl')).

% :- include(logicmoo('vworld/moo_footer.pl')).


end_of_file.


moo:  ?- make.
% Found new meta-predicates in iteration 1 (14.128 sec)
% :- meta_predicate logicmoo_util_library:nd_predsubst2(2,*,*).
% :- meta_predicate logicmoo_util_library:call_n_times(*,0).
% :- meta_predicate logicmoo_util_library:throw_if_true_else_fail(^,*).
% :- meta_predicate logicmoo_util_library:nd_predsubst(?,2,?).
% :- meta_predicate logicmoo_util_library:pred_term_parts(1,?,*).
% :- meta_predicate logicmoo_util_library:predsubst(?,2,?).
% :- meta_predicate logicmoo_util_library:list_retain(*,1,*).
% :- meta_predicate moo_loader:myDebugOnError(0).
% :- meta_predicate logicmoo_util_strings:convert_members(2,?,?).
% :- meta_predicate dbase_rules_pttp:timed_call(0,*).
% :- meta_predicate dbase_rules_pttp:search1(0,*,*,*,*,*,*).
% :- meta_predicate dbase_rules_pttp:call_proof(0,*).
% :- meta_predicate dbase_rules_pttp:call_print_tf(0).
% :- meta_predicate user:in_user_startup(0).
% :- meta_predicate user:within_user(0).
% :- meta_predicate moo_testing:test_call0(0).
% :- meta_predicate moo:call_after_next(0,*).
% :- meta_predicate moo:xcall_t(1,?).
% :- meta_predicate moo:oncely(0).
% :- meta_predicate moo:xcall_t(2,?,?).
% :- meta_predicate moo:cached(0).
% :- meta_predicate moo:simply_functors(2,*,*).
% :- meta_predicate moo:hooked_asserta_confirmed(^,*,*).
% :- meta_predicate moo:xcall_t(0).
% :- meta_predicate moo:trigger_first(*,0).
% :- meta_predicate moo:with_kb_assertions(:,0).
% :- meta_predicate moo:show_cgoal(0).
% :- meta_predicate moo:forall_setof(0,0).
% :- meta_predicate moo:trigger_determined(*,*,0).
% :- meta_predicate moo:check_disj(*,*,0).
% :- meta_predicate moo:trigger_ground(*,0).
% :- meta_predicate moo:trigger_pred(?,1,0).
% :- meta_predicate moo:trigger_nonvar(*,0).
% :- meta_predicate moo:parse_sentence(*,4,?,?,?,?).
% :- meta_predicate moo:xcall_t(5,?,?,?,?,?).
% :- meta_predicate moo:intersect(*,*,*,*,0,-).
% :- meta_predicate moo:fact_checked0(*,0).
% :- meta_predicate moo:xcall_t(4,?,?,?,?).
% :- meta_predicate moo:compare_op(*,2,?,?).
% :- meta_predicate moo:must_ac(^).
% :- meta_predicate moo:xcall_f(2,?,?).
% :- meta_predicate moo:xcall_f(1,?).
% :- meta_predicate moo:hooked_asserta(^).
% :- meta_predicate moo:xcall_t(3,?,?,?).
% :- meta_predicate moo:random_instance(*,*,0).
% :- meta_predicate moo:xcall_t(6,?,?,?,?,?,?).
% :- meta_predicate moo:db_op_loop(*,*,0).
% :- meta_predicate moo:punless(0,0).
% :- meta_predicate moo:call_mpred_body(*,^).
% :- meta_predicate moo:exception(0).
% :- meta_predicate moo:object_print_details(2,*,*,*,*).
% :- meta_predicate moo:xcall_f(0).
:- meta_predicate moo:call_after(0,^).
% :- meta_predicate moo:mud_pred_expansion_2(4,?,?,*,*,*).
% :- meta_predicate moo:xcall_f(5,?,?,?,?,?).
% :- meta_predicate moo:xcall_f(6,?,?,?,?,?,?).
% :- meta_predicate moo:call_mpred_g(*,^).
% :- meta_predicate moo:xcall_f(4,?,?,?,?).
% :- meta_predicate moo:call_mpred_w_results(^,*).
% :- meta_predicate bugger:must_flag(*,0,0).
% :- meta_predicate bugger:dtrace(0).
% :- meta_predicate bugger:show_and_do(0).
% :- meta_predicate bugger:with_skip_bugger(0).
% :- meta_predicate bugger:one_must(^,*,*).
% :- meta_predicate bugger:restore_trace(0).
% :- meta_predicate bugger:trace_or(0).
% :- meta_predicate bugger:ggtrace(0).
% :- meta_predicate bugger:call_no_cuts_0(0).
% :- meta_predicate bugger:with_preds(*,*,*,*,*,0).
% :- meta_predicate bugger:outside_loop_check_term(0,*,0).
% :- meta_predicate bugger:grtrace(0).
% :- meta_predicate bugger:once_if_ground(0).
% :- meta_predicate bugger:gftrace(0).
% :- meta_predicate bugger:logOnFailureIgnore(0).
% :- meta_predicate bugger:outside_loop_check_thread(0).
% :- meta_predicate bugger:must_findall(*,0).
% :- meta_predicate toploop_telnet:telnet_repl_writer(*,*,*,^).
% Restarting analysis ...
% Found new meta-predicates in iteration 2 (13.698 sec)
% :- meta_predicate logicmoo_util_library:pred_term_parts_l(1,*,*).
% :- meta_predicate logicmoo_util_library:nd_predsubst1(2,*,*,*).
% :- meta_predicate dbase_rules_pttp:search0(0,*,*,*,*,*,*).
% :- meta_predicate moo:call_t(*,3,?,?,?).
% :- meta_predicate moo:call_mt_f(*,2,?,?).
% :- meta_predicate moo:call_mt_t(*,3,?,?,?).
% :- meta_predicate moo:xcall_f(3,?,?,?).
% :- meta_predicate moo:call_t(*,6,?,?,?,?,?,?).
% :- meta_predicate moo:call_mt_t(*,5,?,?,?,?,?).
% :- meta_predicate moo:call_t(*,2,?,?).
% :- meta_predicate moo:call_list_t(*,0,*).
% :- meta_predicate moo:call_mt_t(*,2,?,?).
% :- meta_predicate moo:call_mt_f(*,6,?,?,?,?,?,?).
% :- meta_predicate moo:call_t(*,1,?).
% :- meta_predicate moo:call_mt_t(*,4,?,?,?,?).
% :- meta_predicate moo:call_f(*,6,?,?,?,?,?,?).
% :- meta_predicate moo:call_f(*,5,?,?,?,?,?).
% :- meta_predicate moo:call_f(*,4,?,?,?,?).
% :- meta_predicate moo:mud_pred_expansion_1(4,*,*,*).
% :- meta_predicate moo:call_f(*,1,?).
% :- meta_predicate moo:call_t(*,5,?,?,?,?,?).
% :- meta_predicate moo:call_mt_t(*,6,?,?,?,?,?,?).
% :- meta_predicate moo:call_t(*,4,?,?,?,?).
% :- meta_predicate moo:call_mt_f(*,4,?,?,?,?).
% :- meta_predicate moo:call_mt_f(*,5,?,?,?,?,?).
% :- meta_predicate moo:call_f(*,2,?,?).
% :- meta_predicate bugger:traceafter_call(0).
% :- meta_predicate bugger:outside_loop_check(0,0).
% :- meta_predicate bugger:debugCallWhy2(*,0).
% :- meta_predicate bugger:ftrace(0).
% Restarting analysis ...
% Found new meta-predicates in iteration 3 (13.777 sec)
% :- meta_predicate dbase_rules_pttp:search(0,*,*,*,*,*,*).
% :- meta_predicate moo:holds_t(6,?,?,?,?,?,?).
% :- meta_predicate moo:holds_t(5,?,?,?,?,?).
% :- meta_predicate moo:mud_pred_expansion_0(4,*,*,*).
% :- meta_predicate moo:holds_f(5,?,?,?,?,?).
% :- meta_predicate moo:holds_f(1,?).
% :- meta_predicate moo:holds_f(6,?,?,?,?,?,?).
% :- meta_predicate moo:call_f(*,3,?,?,?).
% :- meta_predicate moo:call_mt_f(*,3,?,?,?).
% :- meta_predicate bugger:debugCallWhy(*,0).
% :- meta_predicate moo:dbase_t(5,?,?,?,?,?).
% :- meta_predicate moo:dbase_t(6,?,?,?,?,?,?).
% Restarting analysis ...
Warning: The predicates below are not defined. If these are defined
Warning: at runtime using assert/1, use :- dynamic Name/Arity.
Warning:
Warning: moo:argIsa_ft/3, which is referenced by
Warning:        /devel/logicmoo/src_incoming/dbase/dbase.pl:423:56: 1-st clause of moo:transform_functor_holds/5
Warning: moo:holds_f/1, which is referenced by
Warning:        /devel/logicmoo/src_incoming/dbase/dbase_i_call.pl:192:21: 4-th clause of moo:holds_t/1
Warning: moo:functor_member/2, which is referenced by
Warning:        2-nd clause of moo:get_property/3: 2-nd clause of moo:get_property/3
Warning: moo:isCycPredArity_Check/2, which is referenced by
Warning:        /devel/logicmoo/src_incoming/dbase/dbase_c_term_expansion.pl:49:84: 2-nd clause of moo:using_holds_db/4
Warning:        /devel/logicmoo/src_incoming/dbase/dbase_c_term_expansion.pl:51:45: 4-th clause of moo:using_holds_db/4
Warning: moo:isRegisteredCycPred/3, which is referenced by
Warning:        /devel/logicmoo/src_incoming/dbase/dbase_i_cyc.pl:512:6: 1-st clause of moo:assertThrough/2
Warning:        /devel/logicmoo/src_incoming/dbase/dbase_i_cyc.pl:487:3: 1-st clause of moo:mtForPred/2
Warning:        /devel/logicmoo/src_incoming/dbase/dbase_i_cyc.pl:540:6: 1-st clause of moo:retractAllThrough/2
Warning: moo:list_difference_eq/3, which is referenced by
Warning:        /devel/logicmoo/src_incoming/dbase/dbase.pl:896:67: 2-nd clause of moo:list_update_op/3
Warning: moo:object/3, which is referenced by
Warning:        2-nd clause of moo:get_property/3: 2-nd clause of moo:get_property/3
Warning: moo:pttp_expansions/2, which is referenced by
Warning:        /devel/logicmoo/src_incoming/dbase/dbase_c_term_expansion.pl:142:29: 2-nd clause of moo:mud_rule_expansion/3
Warning: moo:pttp_term_expansion/2, which is referenced by
Warning:        /devel/logicmoo/src_incoming/dbase/dbase_c_term_expansion.pl:142:50: 2-nd clause of moo:mud_rule_expansion/3
Warning: moo:trace_or_throw/2, which is referenced by
Warning:        /devel/logicmoo/src_incoming/dbase/dbase.pl:992:27: 9-th clause of moo:add_handler_lc/1
Warning: nldata_dictionary_some01:cycAssert/2, which is referenced by
Warning:        /devel/logicmoo/src_data/pldata/nldata_dictionary_some01.pl:390:0: 2-nd clause of nldata_dictionary_some01:rememberDictionary2/3
Warning:        /devel/logicmoo/src_data/pldata/nldata_dictionary_some01.pl:392:12: 2-nd clause of nldata_dictionary_some01:rememberDictionary2/3
Warning:        /devel/logicmoo/src_data/pldata/nldata_dictionary_some01.pl:397:8: 2-nd clause of nldata_dictionary_some01:rememberDictionary2/3
Warning: nldata_dictionary_some01:makeConstant/1, which is referenced by
Warning:        /devel/logicmoo/src_data/pldata/nldata_dictionary_some01.pl:386:32: 2-nd clause of nldata_dictionary_some01:rememberDictionary2/3
true.

