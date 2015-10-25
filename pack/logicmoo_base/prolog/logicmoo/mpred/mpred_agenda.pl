% =======================================================
/** <module> 
% This Agenda System is mainly used by the mpred_loader but also needed everywhere
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
% =======================================================
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_agenda.pl
:- module(mpred_agenda,
          [ will_call_after/2,
            add_later/1,
            
            after_mpred_load_pass2/0,
            agenda_do_prequery/0,
            agenda_mpred_repropigate/0,
            agenda_rescan_for_module_ready/0,
            agenda_rescan_mpred_ops/0,
            agenda_slow_op_enqueue/1,
            agenda_slow_op_restart/0,
            agenda_slow_op_todo/1,
            assertOnLoad/1,
            assert_next/2,
            call_after/2,
            call_after_mpred_load/1,
            call_after_mpred_load_slow/1,
            call_after_next/2,
            do_all_of/1,
            do_all_of_ilc/1,
            do_all_of_when/1,
            do_call_OnEachLoad/0,
            do_stuff_of_ilc/1,
            doing_agenda_slow_op/0,
            englishServerInterface/1,
            ensure_at_least_one_region/0,
            expire_post_change/2,
            expire_pre_change/2,
            finish_processing_dbase/0,
            gather_fact_heads/2,
            kb_db_op/2,
            more_to_do/1,
            mpred_one_minute_timer/0,
            mpred_one_second_timer/0,
            onEachLoad/1,
            onLoad/1,
            reduce_fact_heads/4,
            register_timer_thread/3,
            rerun_database_hooks/0,
            rescan_all/0,
            rescan_duplicated_facts/0,
            rescan_duplicated_facts/2,
            rescan_duplicated_facts/3,
            rescan_mpred_facts_local/0,
            rescan_mpred_loaded/0,
            rescan_mpred_loaded_pass2/0,
            rescandb/0,
            run_database_hooks/2,
            run_database_hooks_0/2,
            run_database_hooks_depth_1/2,
            setTemplate/1,
            show_cgoal/1,
            start_one_minute_timer/0,
            start_one_second_timer/0,
            suspend_timers/0,
            tick_every/3,
            time_tick/2,
            wfAssert/1            
          ]).
:- meta_predicate 
   agenda_slow_op_enqueue(0),
        call_after_mpred_load(0),
        register_timer_thread(?, ?, 0),
        % mpred_agenda
call_after(0,0),
% mpred_agenda
call_after_next(0,*),
% mpred_agenda
do_all_of_when(0),
% mpred_agenda
onLoad(0),
% mpred_agenda
show_cgoal(0),
% mpred_agenda
time_tick(*,0),
        tick_every(?, ?, 0).

:- dynamic
        agenda_slow_op_todo/1,
        suspend_timers/0,
        will_call_after/2.

/*
:- was_dynamic((
        doing_agenda_slow_op/0,
        loaded_mpred_file/2)).
*/


register_timer_thread(Name,_Seconds,_OnTick):-current_thread(Name,_Status).
register_timer_thread(Name,Seconds,OnTick):-
   thread_create(tick_every(Name,Seconds,OnTick),_ID,[alias(Name)]). 

tick_every(Name,Seconds,OnTick):-repeat,sleep(Seconds),catch(OnTick,E,dmsg(caused(Name,OnTick,E))),fail.


% ================================================
% Agenda system - source file loading
% ================================================

after_mpred_load:- \+(t_l:loading_mpred_file(_,_)),lmconf:loaded_mpred_file(_,_),!.

% when all previous tasks have completed
after_mpred_load_pass2:- not(lmconf:will_call_after(lmconf:after_mpred_load,_)).
:- meta_predicate(call_after_mpred_load(0)).
% call_after_mpred_load(Code):- lmconf:after_mpred_load,!, call_after_next(after_mpred_load_pass2,Code).
call_after_mpred_load(Code):- call_after_next(lmconf:after_mpred_load,Code).

:- was_export(rescan_mpred_loaded/0).
rescan_mpred_loaded:- ignore((lmconf:after_mpred_load, loop_check(call_after(lmconf:after_mpred_load, true ),true))).

:- was_export(rescan_mpred_loaded_pass2/0).
rescan_mpred_loaded_pass2:- ignore((lmconf:after_mpred_load, loop_check(call_after(after_mpred_load_pass2,  dmsg(rescan_mpred_loaded_pass2_comlpete)),true))).

% ================================================
% Agenda system - standard database
% ================================================
:- was_dynamic(suspend_timers/0).
time_tick(Time,Pred):- repeat,sleep(Time), (suspend_timers->true;(once(doall(on_x_log_throw(call_no_cuts(Pred)))))),fail.

lmconf:hook_one_second_timer_tick.

mpred_one_second_timer:- repeat,time_tick(1.0,lmconf:hook_one_second_timer_tick),fail.
start_one_second_timer:-thread_property(_,alias(mpred_one_second_timer))-> true ; thread_create(mpred_one_second_timer,_,[alias(mpred_one_second_timer)]).

% :-initialization(start_one_second_timer).

lmconf:hook_one_minute_timer_tick.

mpred_one_minute_timer:- repeat,sleep(60.0),time_tick(60.0,lmconf:hook_one_minute_timer_tick),fail.
start_one_minute_timer:-thread_property(_,alias(mpred_one_minute_timer))-> true ; thread_create(mpred_one_minute_timer,_,[alias(mpred_one_minute_timer)]).

% :-initialization(start_one_minute_timer).


agenda_do_prequery:-!.
agenda_do_prequery:- loop_check(agenda_rescan_mpred_ops,true),!.
:- '$hide'(agenda_rescan_mpred_ops/0).
:- '$hide'(agenda_do_prequery/0).
%:- rescan_missing_stubs.
%:- agenda_rescan_mpred_props.


:- was_export(agenda_slow_op_restart/0).
:- dynamic(doing_agenda_slow_op/0).

% agenda_slow_op_restart:-!.
agenda_slow_op_restart:-doing_agenda_slow_op,!.
agenda_slow_op_restart:-
 w_tl(doing_agenda_slow_op,
  forall(lmconf:agenda_slow_op_todo(Slow),
    wno_tl(t_l:side_effect_ok,
      ((copy_term(Slow,CopySlow),
          must((is_callable(Slow),must(Slow),ignore(retract(lmconf:agenda_slow_op_todo(CopySlow)))))))))).

:- was_export(agenda_rescan_mpred_ops/0).
agenda_rescan_mpred_ops:- test_tl(agenda_suspend_scans),!.
agenda_rescan_mpred_ops:- agenda_rescan_for_module_ready,!.

:- thread_local t_l:in_agenda_rescan_for_module_ready/0.
agenda_rescan_for_module_ready:- t_l:in_agenda_rescan_for_module_ready,!.
agenda_rescan_for_module_ready:- w_tl(t_l:in_agenda_rescan_for_module_ready,loop_check(do_all_of(mpred_module_ready),true)).

:- was_export(agenda_slow_op_todo/1).
:- was_dynamic(agenda_slow_op_todo/1).
agenda_slow_op_enqueue(_):-!.
agenda_slow_op_enqueue(Slow):- test_tl(agenda_slow_op_do_prereqs),!,on_x_rtrace(Slow).
agenda_slow_op_enqueue(Slow):- assertz_if_new(agenda_slow_op_todo(Slow)),!.


expire_pre_change(change(assert,_),_):-expire_tabled_list(all),!. 
expire_pre_change(_,_).
expire_post_change(change(retract,_),_):-expire_dont_add,!.
expire_post_change(_,_).

% ============================================
% Prolog will_call_after/do_all_of
% ============================================


call_after(When,C):- When,!,do_all_of(When),must_det(C),!.
call_after(When,C):- assert_next(When,C),!.

assert_next(_,_:true):-!.
assert_next(_,true):-!.
assert_next(When,C):- clause_asserted(lmconf:will_call_after(When,logOnFailure(C))),!.
% assert_next(When,C):- nonground_throw_else_fail(C).
assert_next(When,C):- retractall(lmconf:will_call_after(When,logOnFailure(C))),!, assertz_if_new(lmconf:will_call_after(When,logOnFailure(C))).

call_after_next(When,C):- ignore((When,!,do_all_of(When))),assert_next(When,C).


do_all_of_when(When):- ignore((more_to_do(When),When,do_all_of(When))).

:- was_export(do_all_of/1).
do_all_of(When):- ignore(loop_check(do_all_of_ilc(When),true)),!.
do_all_of_ilc(When):- not(lmconf:will_call_after(When,_)),!.
do_all_of_ilc(When):-  repeat,do_stuff_of_ilc(When), not(more_to_do(When)).

more_to_do(When):-predicate_property(lmconf:will_call_after(When,_),number_of_clauses(N)),!,N>0.

do_stuff_of_ilc(When):-not(more_to_do(When)),!.
do_stuff_of_ilc(When):- lmconf:will_call_after(When,A),!,retract(lmconf:will_call_after(When,A)),!,call(A),!.



show_cgoal(G):- slow_sanity((stack_check(9600,dmsg(warning(maybe_overflow(stack_lvl)))))),call(G).


:- was_export(add_later/1).
add_later(Fact):- call_after_mpred_load(ain(Fact)).

% ========================================
% run_database_hooks(Type,Hook)
%
%     assert/retract hooks
% ========================================
/*
:- was_dynamic(lmconf:decl_database_hook/2).
:- was_shared_multifile(lmconf:decl_database_hook/2).
:- was_export(lmconf:decl_database_hook/2).
:- meta_predicate lmconf:decl_database_hook(?,0).
*/
% hooks are declared as
%        lmconf:decl_database_hook(change(assert,A_or_Z),Fact):- ...
%        lmconf:decl_database_hook(change( retract,One_or_All),Fact):- ...

run_database_hooks(Type,Hook):- trace_or_throw(use_pfc(run_database_hooks(Type,Hook))),t_l:noDBaseHOOKS(_),dmsg(noDBaseHOOKS(Type,Hook)),!.
run_database_hooks(Type,HookIn):-run_database_hooks_0(Type,HookIn).

% non recusive of the above
run_database_hooks_depth_1(Type,Hook):- trace_or_throw(use_pfc(run_database_hooks(Type,Hook))), t_l:noDBaseHOOKS(_),dmsg(noDBaseHOOKS(Type,Hook)),!.
run_database_hooks_depth_1(Type,Hook):- w_tl(t_l:noDBaseHOOKS(_),run_database_hooks_0(Type,Hook)).

kb_db_op(assert(How),change(assert,How)):-!.
kb_db_op(retract(How),change(retract,How)):-!.
kb_db_op(KBDB,KBDB).

% next line exists because mpred_form allows modules (maybe will change that soon)
run_database_hooks_0(Type,M:Hook):- trace_or_throw(use_pfc(run_database_hooks(Type,Hook))), atom(M),!,run_database_hooks_0(Type,Hook).
run_database_hooks_0(TypeIn,HookIn):-  trace_or_throw(use_pfc(run_database_hooks(TypeIn,HookIn))),!.
/*
   kb_db_op(TypeIn,Type),
   into_mpred_form(HookIn,Hook),
   copy_term(Hook,HookCopy),
   loop_check_term(doall(call_no_cuts(lmconf:decl_database_hook(Type,HookCopy))),run_database_hooks(Hook),true).
*/

% ========================================
% Rescan for consistency
% ========================================

%:-meta_predicate(rescan_all/0).
rescan_all:- doall_and_fail(agenda_rescan_mpred_ops).
rescan_all:- doall_and_fail(agenda_mpred_repropigate).
rescan_all:- doall_and_fail(rescan_mpred_loaded).
rescan_all:- doall_and_fail(agenda_rescan_mpred_ops).
% rescan_all:- doall_and_fail(agenda_rescan_sim_objects).
rescan_all:- doall_and_fail(agenda_slow_op_restart).
rescan_all:- doall_and_fail(agenda_rescan_mpred_props).
rescan_all.

ensure_at_least_one_region:- (isa(_,tRegion)->true;ain(isa(iRegion1,tRegion))),!.

% :-meta_predicate(finish_processing_dbase).
finish_processing_dbase:- do_gc,dmsginfo(begin_finish_processing_dbase),fail.
finish_processing_dbase:- (doall_and_fail(rescan_all)).
finish_processing_dbase:- doall_and_fail(ensure_at_least_one_region).
finish_processing_dbase:- (doall_and_fail(do_call_OnEachLoad)).
finish_processing_dbase:- dmsginfo(saving_finish_processing_dbase),fail.
finish_processing_dbase:- savedb,fail.
finish_processing_dbase:- do_gc,dmsginfo(end_finish_processing_dbase),fail.
finish_processing_dbase.

lmconf:hook_one_minute_timer_tick:-agenda_slow_op_restart.


%:-meta_predicate(rescandb/0).
% rescandb:- forall(get_user_abox(World),(findall(File,lmconf:loaded_file_world_time(File,World,_),Files),forall(member(File,Files),ensure_plmoo_loaded_each(File)),req(finish_processing_world))).
rescandb:- req(finish_processing_world).



:- was_export((agenda_mpred_repropigate/0, rescan_duplicated_facts/0, rerun_database_hooks/0 , gather_fact_heads/2)).

agenda_mpred_repropigate:-  loop_check(rescan_mpred_facts_local).

rescan_mpred_facts_local:-wno_tl(lmconf:use_cyc_database,(must_det(rescan_duplicated_facts),must_det(rerun_database_hooks))).

rescan_duplicated_facts:- !, hotrace( forall(member(M,[moo,user,world,hook]), forall((predicate_property(M:H,dynamic),arity(F,A),functor(H,F,A)), rescan_duplicated_facts(M,H)))).
rescan_duplicated_facts(_M,_H):-!.
rescan_duplicated_facts(M,H):-!,rescan_duplicated_facts(M,H,true).
rescan_duplicated_facts(M,H):-findall(H,(clause_safe(M:H,B),B==true),CF1), once((list_to_set(CF1,CF2),reduce_fact_heads(M,H,CF1,CF2))).
rescan_duplicated_facts(M,H,BB):-hotrace(doall((gather_fact_heads(M,H),BB=true,once((findall(C,(clause_safe(H,B),B=@=BB,reduce_clause(is_asserted,(H:-B),C)),CF1),
                                                                     list_to_set(CF1,CF2),once(reduce_fact_heads(M,H,CF1,CF2))))))).
rerun_database_hooks:-!.
rerun_database_hooks:-time_call(doall((gather_fact_heads(_M,H),forall(is_asserted(H),run_database_hooks(change(assert,z),H))))),fail.
rerun_database_hooks:-time_call(doall((is_asserted(genls(I,C)),run_database_hooks(change(assert,z),genls(I,C))))),fail.
rerun_database_hooks:-time_call(doall((isa_asserted(I,C),run_database_hooks(change(assert,z),isa(I,C))))),fail.

reduce_fact_heads(_M,_H,CF1,CF1):-!. % no change
reduce_fact_heads(M,H,CF1,CF2):- 
 ignore((
   predicate_property(M:H,dynamic),
   length(CF1,F1),length(CF2,F2),
   dmsg(reduce_fact_heads(M,H,from(F1,F2))),
   retractall(M:H),
   % forall(member(C,CF1),retractall(M:C)),
   forall(member(C,CF2),assertz(M:C)))).

gather_fact_heads(M,H):- (nonvar(M)->true; member(M,[dbase,moo,world,user,hook])), current_predicate(M:F/A), arity(F,A),
  once((once((A>0,atom(F),F\=(:),var(H), on_x_rtrace(functor_catch(H,F,A)))),compound(H),predicate_property(M:H,number_of_clauses(_)),
  not((arg(_,vv(system,bugger,logicmoo_util_dcg,user),M);predicate_property(M:H,imported_from(_)))))).


/*
:- was_export(begin_prolog_source/0).
:- was_export(end_prolog_source/0).
begin_prolog_source:- must_det(asserta(t_l:in_prolog_source_code)).
end_prolog_source:- mpred_modify(change( retract,_),t_l:in_prolog_source_code).
*/


assertOnLoad(X):-add_later(X).

setTemplate(X):-ain(X).

englishServerInterface(SomeEnglish):-dmsg(todo(englishServerInterface(SomeEnglish))).


:- was_export(onLoad/1).
onLoad(C):-call_after_mpred_load(C).
:- was_export(lmconf:onEachLoad/1).
onEachLoad(C):-assert_if_new(lmconf:call_OnEachLoad(C)).


call_after_mpred_load_slow(A):-dmsg(call_after_mpred_load_slow(A)).

do_call_OnEachLoad:-forall(call_OnEachLoad(C),doall(C)).


wfAssert(X):-ain(X). % add_later(X).

:- source_location(S,_),forall(source_file(H,S),(functor(H,F,A),export(F/A),module_transparent(F/A))).

