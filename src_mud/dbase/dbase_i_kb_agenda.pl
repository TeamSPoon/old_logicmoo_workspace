
:- meta_predicate tick_every(*,*,0).
:- meta_predicate register_timer_thread(*,*,0).

register_timer_thread(Name,_Seconds,_OnTick):-current_thread(Name,_Status).
register_timer_thread(Name,Seconds,OnTick):-
   thread_create(tick_every(Name,Seconds,OnTick),_ID,[alias(Name)]). 

tick_every(Name,Seconds,OnTick):-repeat,sleep(Seconds),catch(OnTick,E,dmsg(caused(Name,OnTick,E))),fail.


% ================================================
% Agenda system - source file loading
% ================================================

:- dynamic((thglobal:loading_dbase_file/2, thglobal:loaded_dbase_file/2)).

thglobal:after_dbase_load:- not(thglobal:loading_dbase_file(_,_)),thglobal:loaded_dbase_file(_,_),!.

% when all previous tasks have completed
after_dbase_load_pass2:- not(thglobal:will_call_after(thglobal:after_dbase_load,_)).
:- meta_predicate(call_after_dbase_load(0)).
% call_after_dbase_load(Code):- thglobal:after_dbase_load,!, call_after_next(after_dbase_load_pass2,Code).
call_after_dbase_load(Code):- call_after_next(thglobal:after_dbase_load,Code).

:-export(rescan_dbase_loaded/0).
rescan_dbase_loaded:- ignore((thglobal:after_dbase_load, loop_check(call_after(thglobal:after_dbase_load, true ),true))).

:-export(rescan_dbase_loaded_pass2/0).
rescan_dbase_loaded_pass2:- ignore((thglobal:after_dbase_load, loop_check(call_after(after_dbase_load_pass2,  dmsg(rescan_dbase_loaded_pass2_comlpete)),true))).

% ================================================
% Agenda system - standard database
% ================================================

% agenda_do_prequery:-!.
agenda_do_prequery:- loop_check_local(agenda_rescan_dbase_ops,true).
:-'$hide'(agenda_rescan_dbase_ops/0).
:-'$hide'(agenda_do_prequery/0).
%:- rescan_missing_stubs.
%:- agenda_rescan_mpred_props.

:- sanity(dbase_mod(user)).

:-export(agenda_slow_op_restart/0).

% agenda_slow_op_restart:-!.
agenda_slow_op_restart:- loop_check(forall(retract(agenda_slow_op_todo(Slow)),(must(is_callable(Slow)),must_det(Slow))),true).

:-export(agenda_rescan_dbase_ops/0).
agenda_rescan_dbase_ops:- test_tl(agenda_suspend_scans),!.
agenda_rescan_dbase_ops:- agenda_rescan_for_module_ready.

:-thread_local thlocal:in_agenda_rescan_for_module_ready/0.
agenda_rescan_for_module_ready:- thlocal:in_agenda_rescan_for_module_ready,!.
agenda_rescan_for_module_ready:- with_assertions(thlocal:in_agenda_rescan_for_module_ready,loop_check_local(do_all_of(dbase_module_ready),true)).

:-export agenda_slow_op_todo/1.
agenda_slow_op_enqueue(Slow):- test_tl(agenda_slow_op_do_prereqs),!,debugOnError(Slow).
agenda_slow_op_enqueue(Slow):- assertz_if_new(agenda_slow_op_todo(Slow)),!.

:-dynamic(already_added_this_round/1).
expire_dont_add:-retractall(already_added_this_round(_)),expire_tabled_list(all),nop(dmsg(expire_dont_add)).

expire_pre_change(change(assert,_),_):-expire_tabled_list(all),!. 
expire_pre_change(_,_).
expire_post_change(change(retract,_),_):-expire_dont_add,!.
expire_post_change(_,_).

% ============================================
% Prolog will_call_after/do_all_of
% ============================================

:-dynamic(thglobal:will_call_after/2).

call_after(When,C):- When,!,do_all_of(When),must_det(C),!.
call_after(When,C):- assert_next(When,C),!.

assert_next(_,_:true):-!.
assert_next(_,true):-!.
assert_next(When,C):- clause_asserted(thglobal:will_call_after(When,logOnFailure(C))),!.
% assert_next(When,C):- nonground_throw_else_fail(C).
assert_next(When,C):- retractall(thglobal:will_call_after(When,logOnFailure(C))),!, assertz_if_new(thglobal:will_call_after(When,logOnFailure(C))).

call_after_next(When,C):- ignore((When,!,do_all_of(When))),assert_next(When,C).


do_all_of_when(When):- ignore((more_to_do(When),When,do_all_of(When))).

:-export(do_all_of/1).
do_all_of(When):- ignore(loop_check_local(do_all_of_ilc(When),true)),!.
do_all_of_ilc(When):- not(thglobal:will_call_after(When,_)),!.
do_all_of_ilc(When):-  repeat,do_stuff_of_ilc(When), not(more_to_do(When)).

more_to_do(When):-predicate_property(thglobal:will_call_after(When,_),number_of_clauses(N)),!,N>0.

do_stuff_of_ilc(When):-not(more_to_do(When)),!.
do_stuff_of_ilc(When):- thglobal:will_call_after(When,A),!,retract(thglobal:will_call_after(When,A)),!,call(A),!.



show_cgoal(G):- slow_sanity((stack_check(9600,dmsg(warning(maybe_overflow(stack_lvl)))))),call(G).


:-export(add_later/1).
add_later(Fact):- call_after_dbase_load(add(Fact)).

% ========================================
% run_database_hooks(Type,Hook)
%
%     assert/retract hooks
% ========================================
:- export user:decl_database_hook/2.
% hooks are declared as
%        user:decl_database_hook(change(assert,A_or_Z),Fact):- ...
%        user:decl_database_hook(change( retract,One_or_All),Fact):- ...

run_database_hooks(Type,Hook):- thlocal:noDBaseHOOKS(_),dmsg(noDBaseHOOKS(Type,Hook)),!.
run_database_hooks(Type,HookIn):-run_database_hooks_0(Type,HookIn).

% non recusive of the above
run_database_hooks_depth_1(Type,Hook):- thlocal:noDBaseHOOKS(_),dmsg(noDBaseHOOKS(Type,Hook)),!.
run_database_hooks_depth_1(Type,Hook):- with_assertions(thlocal:noDBaseHOOKS(_),run_database_hooks_0(Type,Hook)).

kb_db_op(assert(How),change(assert,How)):-!.
kb_db_op(retract(How),change(retract,How)):-!.
kb_db_op(KBDB,KBDB).

% next line exists because mpred_form allows modules (maybe will change that soon)
run_database_hooks_0(Type,M:Hook):-atom(M),!,run_database_hooks_0(Type,Hook).
run_database_hooks_0(TypeIn,HookIn):- 
   kb_db_op(TypeIn,Type),
   into_mpred_form(HookIn,Hook),
   copy_term(Hook,HookCopy),
   loop_check_term(doall(call_no_cuts(user:decl_database_hook(Type,HookCopy))),run_database_hooks(Hook),true).

% ========================================
% Spawn new instances
% ========================================

onSpawn(ClassFact):- ClassFact=..[Funct,InstA],
 createByNameMangle(InstA,Inst,Type2),
 assert_isa(Type2,tCol),
 assert_isa(Inst,Funct),assert_isa(Inst,Type2),!.
onSpawn(ClassFact):- ClassFact=..[Funct|InstADeclB],must_det(onSpawn_f_args(Funct,InstADeclB)).

onSpawn_f_args(Funct,List):-
 with_assertions(deduceArgTypes(Funct),
  (convertSpawnArgs(Funct,1,List,NewList),
   Later =.. [dbase_t,Funct|NewList],
   add(Later),
  call_after_dbase_load_slow(with_assertions(deduceArgTypes(Funct), add(Later))))),!.

convertSpawnArgs(_,_,[],[]).
convertSpawnArgs(Funct,N,[A|List],[O|NewList]):-
 convertOneSpawnArg(Funct,N,A,O),!,
 N2 is N + 1,
 convertSpawnArgs(Funct,N2,List,NewList).

convertOneSpawnArg(_,_,O,O):-string(O),!.
convertOneSpawnArg(_,_,O,O):-number(O),!.
convertOneSpawnArg(_,_,nospawn(O),O):-!.
convertOneSpawnArg(Funct,N,isInstFn(A),O):-spawnOneSpawnArg(Funct,N,A,O).
convertOneSpawnArg(Funct,N,A,O):-spawnOneSpawnArg(Funct,N,A,O).

spawnOneSpawnArg(Funct,N,A,O):-
 createByNameMangle(A,O,TypeA),assert_isa(TypeA,tCol),
 assert_subclass_on_argIsa(Funct,N,TypeA).
 

convertOneTypedSpawnArg(Type,A,O):-
 createByNameMangle(A,O,TypeA),assert_isa(TypeA,tCol),
 assert_subclass(TypeA,Type).


% ========================================
% Rescan for consistency
% ========================================

%:-meta_predicate_transparent(rescan_all/0).
rescan_all:- doall_and_fail(agenda_rescan_dbase_ops).
rescan_all:- doall_and_fail(agenda_dbase_repropigate).
rescan_all:- doall_and_fail(rescan_dbase_loaded).
rescan_all:- doall_and_fail(agenda_rescan_dbase_ops).
rescan_all:- doall_and_fail(agenda_rescan_sim_objects).
rescan_all:- doall_and_fail(agenda_slow_op_restart).
rescan_all:- doall_and_fail(agenda_rescan_mpred_props).
rescan_all.

ensure_at_least_one_region:- (isa(_,tRegion)->true;create_instance(oneRegion1,tRegion)),!.

% :-meta_predicate_transparent(finish_processing_dbase).
finish_processing_dbase:- do_gc,dmsginfo(begin_finish_processing_dbase),fail.
finish_processing_dbase:- doall_and_fail(rescan_all).
finish_processing_dbase:- doall_and_fail(ensure_at_least_one_region).
finish_processing_dbase:- doall_and_fail(call_OnEachLoad).
finish_processing_dbase:- dmsginfo(saving_finish_processing_dbase),fail.
finish_processing_dbase:- savedb,fail.
finish_processing_dbase:- do_gc,dmsginfo(end_finish_processing_dbase),fail.
finish_processing_dbase.


%:-meta_predicate_transparent(rescandb/0).
% rescandb:- forall(thglobal:current_world(World),(findall(File,thglobal:loaded_file_world_time(File,World,_),Files),forall(member(File,Files),ensure_plmoo_loaded_each(File)),mpred_call(finish_processing_world))).
rescandb:- mpred_call(finish_processing_world).



:-export((agenda_dbase_repropigate/0, rescan_duplicated_facts/0, rerun_database_hooks/0 , gather_fact_heads/2)).

agenda_dbase_repropigate:-  loop_check_local(rescan_dbase_facts_local).

rescan_dbase_facts_local:-with_no_assertions(thglobal:use_cyc_database,(must_det(rescan_duplicated_facts),must_det(rerun_database_hooks))).

rescan_duplicated_facts:- !, notrace( forall(member(M,[moo,user,world,hook]), forall((predicate_property(M:H,dynamic),mpred_arity(F,A),functor(H,F,A)), rescan_duplicated_facts(M,H)))).
rescan_duplicated_facts(_M,_H):-!.
rescan_duplicated_facts(M,H):-!,rescan_duplicated_facts(M,H,true).
rescan_duplicated_facts(M,H):-findall(H,(clause_safe(M:H,B),B==true),CF1), once((list_to_set(CF1,CF2),reduce_fact_heads(M,H,CF1,CF2))).
rescan_duplicated_facts(M,H,BB):-notrace(doall((gather_fact_heads(M,H),BB=true,once((findall(C,(clause_safe(H,B),B=@=BB,reduce_clause(is_asserted,(H:-B),C)),CF1),
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

gather_fact_heads(M,H):- (nonvar(M)->true; member(M,[dbase,moo,world,user,hook])), current_predicate(M:F/A), mpred_arity(F,A),
  once((once((A>0,atom(F),F\=(:),var(H), debugOnError(functor_catch(H,F,A)))),compound(H),predicate_property(M:H,number_of_clauses(_)),
  not((arg(_,vv(system,bugger,logicmoo_util_dcg,user),M);predicate_property(M:H,imported_from(_)))))).




:-export(begin_prolog_source/0).
:-export(end_prolog_source/0).
begin_prolog_source:- must_det(asserta(thlocal:in_prolog_source_code)).
end_prolog_source:- must_det(change( retract,thlocal:in_prolog_source_code)).



assertOnLoad(X):-add_later(X).

setTemplate(X):-add(X).

englishServerInterface(SomeEnglish):-dmsg(todo(englishServerInterface(SomeEnglish))).

:-multifile(user:call_OnEachLoad/1).
:-export(user:call_OnEachLoad/1).
:-dynamic(user:call_OnEachLoad/1).

:-export(onLoad/1).
onLoad(C):-call_after_dbase_load(C).
:-export(user:onEachLoad/1).
onEachLoad(C):-assert_if_new(user:call_OnEachLoad(C)).


call_after_dbase_load_slow(A):-dmsg(call_after_dbase_load_slow(A)).

call_OnEachLoad:-forall(call_OnEachLoad(C),doall(C)).


createByNameMangle(InstA,IDA,InstAO):-must(createByNameMangle0(InstA,IDA,InstAO)),!.

createByNameMangle0(InstA,InstA,Type):-compound(InstA),InstA=..[Type|Props],assert_isa(InstA,Type),with_assertions(deduceArgTypes(_),padd(InstA,Props)).
createByNameMangle0(InstA,Inst,Type):- compound(InstA),!,functor_catch(InstA,Type,A),must(A==1),assert_isa(InstA,Type),InstA=Inst.
createByNameMangle0(InstA,_,_Type):- not(atom(InstA)),!,trace_or_throw(todo(not_atom_createByNameMangle(InstA))).
createByNameMangle0(OType,InstA,Type):-isa_asserted(OType,tCol),!,create_from_type(OType,InstA,Type).
createByNameMangle0(Suggest,InstA,Type):- once(split_name_type(Suggest,InstA,Type)),Suggest==InstA,assert_isa(InstA,Type).
createByNameMangle0(OType,InstA,Type):- create_from_type(OType,InstA,Type),!.
createByNameMangle0(InstA,IDA,InstA):- gensym(InstA,IDA), englishServerInterface([actCreate,InstA,IDA]).


create_from_type(OType,InstA,Type):-sanity(var(InstA)),i_name(t,OType,Type),atom_concat(Type,'7',InstA7),i_name(i,InstA7,InstA),must_det(assert_isa(InstA,Type)), 
 call_after_dbase_load_slow(create_instance(InstA,Type)).

wfAssert(X):-add(X). % add_later(X).


