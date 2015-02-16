
% ================================================
% Agenda system - source file loading
% ================================================

:- dynamic_multifile_exported((thglobal:loading_game_file/2, thglobal:loaded_game_file/2)).

thglobal:after_game_load:- not(thglobal:loading_game_file(_,_)),thglobal:loaded_game_file(_,_),!.

% when all previous tasks have completed
after_game_load_pass2:- not(thglobal:will_call_after(thglobal:after_game_load,_)).
:- meta_predicate(call_after_game_load(0)).
% call_after_game_load(Code):- thglobal:after_game_load,!, call_after_next(after_game_load_pass2,Code).
call_after_game_load(Code):- call_after_next(thglobal:after_game_load,Code).

:-export(rescan_game_loaded/0).
rescan_game_loaded:- ignore((thglobal:after_game_load, loop_check(call_after(thglobal:after_game_load, true ),true))).

:-export(rescan_game_loaded_pass2/0).
rescan_game_loaded_pass2:- ignore((thglobal:after_game_load, loop_check(call_after(after_game_load_pass2,  dmsg(rescan_game_loaded_pass2_comlpete)),true))).

% ================================================
% Agenda system - standard database
% ================================================

% do_db_op_hooks:-!.
do_db_op_hooks:- loop_check_local(rescan_dbase_ops,true).

:-export(rescan_slow_kb_ops/0).

% rescan_slow_kb_ops:-!.
rescan_slow_kb_ops:- loop_check(forall(retract(do_slow_kb_op_later(Slow)),(must(is_callable(Slow)),must_det(Slow))),true).

:-export(rescan_dbase_ops/0).
rescan_dbase_ops:- test_tl(skip_db_op_hooks),!.
rescan_dbase_ops:- rescan_module_ready.

:-thread_local thlocal:in_rescan_module_ready/0.
rescan_module_ready:- thlocal:in_rescan_module_ready,!.
rescan_module_ready:- with_assertions(thlocal:in_rescan_module_ready,loop_check_local(do_all_of(dbase_module_ready),true)).

:- export do_slow_kb_op_later/1.
slow_kb_op(Slow):- test_tl(do_slow_kb_op_now),!,debugOnError(Slow).
slow_kb_op(Slow):- asserta_if_new(do_slow_kb_op_later(Slow)),!.

:-dynamic(implied_dont_add/1).
expire_dont_add:-retractall(implied_dont_add(_)),expire_tabled_list(all),nop(dmsg(expire_dont_add)).

expire_pre_change(assert,_):-expire_tabled_list(all),!. 
expire_pre_change(_,_).
expire_post_change( retract,_):-expire_dont_add,!.
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

:-dynamic_multifile_exported(do_all_of/1).
do_all_of(When):- ignore(loop_check_local(do_all_of_lc(When),true)),!.
do_all_of_lc(When):- not(thglobal:will_call_after(When,_)),!.
do_all_of_lc(When):-  repeat,do_stuff_of_lc(When), not(more_to_do(When)).

more_to_do(When):-predicate_property(thglobal:will_call_after(When,_),number_of_clauses(N)),!,N>0.

do_stuff_of_lc(When):-not(more_to_do(When)),!.
do_stuff_of_lc(When):- thglobal:will_call_after(When,A),!,retract(thglobal:will_call_after(When,A)),!,call(A),!.



show_cgoal(G):- slow_sanity((stack_check(9600,dmsg(warning(maybe_overflow(stack_lvl)))))),call(G).


:-dynamic_multifile_exported(add_later/1).
add_later(Fact):- call_after_game_load(add(Fact)).



user:decl_database_hook(change(assert,_),mudFtInfo(FT,_)):- define_ft(FT).
user:decl_database_hook(change(assert,_),subFormat(FT,OFT)):- define_ft(OFT),define_ft(FT).
% user:decl_database_hook(change(assert,_),subclass(FT,OFT)):- formattype(OFT),dmsg(warning(subclass_of_define_ft(FT))).


user:decl_database_hook(AR,C):- record_on_thread(dbase_change,changing(AR,C)).

record_on_thread(Dbase_change,O):- thread_self(ID),thlocal:dbase_capture(ID,Dbase_change),!,Z=..[Dbase_change,ID,O],assertz(Z).


% ========================================
% run_database_hooks(Type,Hook)
%
%     assert/retract hooks
% ========================================
:- dynamic_multifile_exported user:decl_database_hook/2.
% hooks are declared as
%        user:decl_database_hook(change(assert,A_or_Z),Fact):- ...
%        user:decl_database_hook(change( retract,One_or_All),Fact):- ...

run_database_hooks(Type,Hook):- thlocal:noDBaseHOOKS(_),dmsg(noDBaseHOOKS(Type,Hook)),!.
run_database_hooks(Type,HookIn):-run_database_hooks_0(Type,HookIn).

% non recusive of the above
run_database_hooks_depth_1(Type,Hook):- thlocal:noDBaseHOOKS(_),dmsg(noDBaseHOOKS(Type,Hook)),!.
run_database_hooks_depth_1(Type,Hook):- with_assertions(thlocal:noDBaseHOOKS(_),run_database_hooks_0(Type,Hook)).

kb_db_op(change(Verb,How),DB):-atom(Verb),!,DB=..[Verb,How].
kb_db_op(KBDB,KBDB).

% next line exists because mpred_form allows modules (maybe will change that soon)
run_database_hooks_0(Type,M:Hook):-atom(M),!,run_database_hooks_0(Type,Hook).
run_database_hooks_0(TypeIn,HookIn):- 
   kb_db_op(TypeIn,Type),
   into_mpred_form(HookIn,Hook),
   copy_term(Hook,HookCopy),
   loop_check_term(doall(call_no_cuts(user:decl_database_hook(Type,HookCopy))),run_database_hooks(Hook),true).


% ========================================
% Rescan for consistency
% ========================================
:-dynamic_multifile_exported((rescan_dbase_facts/0, rescan_duplicated_facts/0, rerun_database_hooks/0 , gather_fact_heads/2)).

rescan_dbase_facts:-!.
rescan_dbase_facts:-  loop_check_local(rescan_dbase_facts_local).

rescan_dbase_facts_local:-with_no_assertions(thglobal:use_cyc_database,(must_det(rescan_duplicated_facts),must_det(rerun_database_hooks))).

rescan_duplicated_facts:- !, notrace( forall(member(M,[moo,user,world,hook]), forall((predicate_property(M:H,dynamic),mpred_arity(F,A),functor(H,F,A)), rescan_duplicated_facts(M,H)))).
rescan_duplicated_facts(_M,_H):-!.
rescan_duplicated_facts(M,H):-!,rescan_duplicated_facts(M,H,true).
rescan_duplicated_facts(M,H):-findall(H,(clause_safe(M:H,B),B==true),CF1), once((list_to_set(CF1,CF2),reduce_fact_heads(M,H,CF1,CF2))).
rescan_duplicated_facts(M,H,BB):-notrace(doall((gather_fact_heads(M,H),BB=true,once((findall(C,(clause_safe(H,B),B=@=BB,reduce_clause((H:-B),C)),CF1),
                                                                     list_to_set(CF1,CF2),once(reduce_fact_heads(M,H,CF1,CF2))))))).
rerun_database_hooks:-!.
rerun_database_hooks:-time_call(doall((gather_fact_heads(_M,H),forall(is_asserted(H),run_database_hooks(change(assert,z),H))))),fail.
rerun_database_hooks:-time_call(doall((is_asserted(subclass(I,C)),run_database_hooks(change(assert,z),subclass(I,C))))),fail.
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




:-dynamic_multifile_exported(begin_prolog_source/0).
:-dynamic_multifile_exported(end_prolog_source/0).
begin_prolog_source:- must_det(asserta(thlocal:in_prolog_source_code)).
end_prolog_source:- must_det(change( retract,thlocal:in_prolog_source_code)).



assertOnLoad(X):-add_later(X).

setTemplate(X):-add(X).

englishServerInterface(SomeEnglish):-dmsg(todo(englishServerInterface(SomeEnglish))).

:-multifile(user:call_OnEachLoad/1).
:-export(user:call_OnEachLoad/1).
:-dynamic(user:call_OnEachLoad/1).

:-dynamic_multifile_exported(onLoad/1).
onLoad(C):-call_after_game_load(C).
:-dynamic_multifile_exported(user:onEachLoad/1).
onEachLoad(C):-assert_if_new(user:call_OnEachLoad(C)).


call_after_game_load_slow(A):-dmsg(call_after_game_load_slow(A)).

call_OnEachLoad:-forall(call_OnEachLoad(C),doall(C)).


onSpawn(ClassFact):- ClassFact=..[Funct,InstA],createByNameMangle(InstA,Inst,Type2),assert_isa(Type2,tCol),assert_isa(Inst,Funct),assert_isa(Inst,Type2),!.
onSpawn(ClassFact):- ClassFact=..[Funct|InstADeclB],must_det(onSpawn_f_args(Funct,InstADeclB)).

onSpawn_f_args(Funct,List):-
 with_assertions(deduceArgTypes(Funct),
  (convertSpawnArgs(Funct,1,List,NewList),
   Later =.. [dbase_t,Funct|NewList],
   add(Later),
  call_after_game_load_slow(with_assertions(deduceArgTypes(Funct), add(Later))))),!.

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


createByNameMangle(InstA,IDA,InstAO):-must(createByNameMangle0(InstA,IDA,InstAO)),!.

createByNameMangle0(InstA,InstA,Type):-compound(InstA),InstA=..[Type|Props],assert_isa(InstA,Type),with_assertions(deduceArgTypes(_),padd(InstA,Props)).
createByNameMangle0(InstA,Inst,Type):- compound(InstA),!,functor_catch(InstA,Type,A),must(A==1),assert_isa(InstA,Type),InstA=Inst.
createByNameMangle0(InstA,_,_Type):- not(atom(InstA)),!,trace_or_throw(todo(not_atom_createByNameMangle(InstA))).
createByNameMangle0(Suggest,InstA,Type):- once(split_name_type(Suggest,InstA,Type)),Suggest==InstA,assert_isa(InstA,Type).
createByNameMangle0(OType,InstA,Type):- sanity(var(InstA)),i_name(t,OType,Type),atom_concat(Type,'7',InstA7),i_name(i,InstA7,InstA),must_det(assert_isa(InstA,Type)), 
 call_after_game_load_slow(create_instance(InstA,Type)).
createByNameMangle0(InstA,IDA,InstA):- gensym(InstA,IDA), englishServerInterface([actCreate,InstA,IDA]).

wfAssert(X):-add(X). % add_later(X).

