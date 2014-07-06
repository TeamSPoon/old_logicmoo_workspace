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
% :- module(user). 
:- module(moo,[coerce/3, current_context_module/1,
    rescan_dbase_t_once/0,
    rescan_dbase_t/0,
    term_expansion_local/2,
         run_database_hooks/2,
         register_module_type/1, 
         registered_module_type/2, 
         end_module_type/1,
         enter_term_anglify/2,
         agent_text_command/4,         
         register_timer_thread/3,
         end_module_type/2         ]).



simple_code :- fail.
not_simple_code :- \+ simple_code.

:- set_prolog_flag(double_quotes, atom).
:- set_prolog_flag(double_quotes, string).

:- '@'((use_module(logicmoo(logicmoo_util/logicmoo_util_bugger)),
         use_module(logicmoo(logicmoo_util/logicmoo_util_library)),
         use_module(logicmoo(logicmoo_util/logicmoo_util_ctx_frame)),
         use_module(logicmoo(logicmoo_util/logicmoo_util_strings)),
         use_module(logicmoo(logicmoo_util/logicmoo_util_terms)),
         use_module(logicmoo(logicmoo_util/logicmoo_util_dcg))),'user').

% ========================================
% dbase_mod/1
% ========================================

:- dynamic_multifile_exported dbase_mod/1.
dbase_mod(moo).

% ========================================
% assert/retract hooks
% ========================================
% :- include(logicmoo(dbase/dbase_rules_nnf)).


% hooks are declared as
% hook:decl_database_hook(assert(A_or_Z),Fact).
% hook:decl_database_hook(retract(One_or_All),Fact).
:- dynamic_multifile_exported hook:decl_database_hook/2.

run_database_hooks(Type,M:Hook):-atom(M),!,moo:run_database_hooks(Type,Hook).
run_database_hooks(Type,Hook):- copy_term(Hook,HCopy),doall(call_no_cuts(hook:decl_database_hook(Type,HCopy))).

rescan_dbase_t:-  loop_check(rescan_dbase_t_once,true).

reduce_clause((C:-B),C):-B==true,!.
reduce_clause(C,C).

:-export((rescan_dbase_t_once/0, rescan_dbase_t/0, remove_duplicated_facts/0, rerun_database_hooks/0 , gather_fact_heads/2)).
:-dynamic_multifile_exported((nameStrings/2,grid_key/1,on_world_load/0,label_type/2,createableType/2)).

rescan_dbase_t_once:- must(remove_duplicated_facts),must(rerun_database_hooks).


remove_duplicated_facts:- !,
                   forall(member(M,[moo,world,hook]),
                     forall(predicate_property(M:H,dynamic),
                      (findall(H,(clause(M:H,B),B==true),CL1),
                         once((list_to_set(CL1,CL2),                      
                      reduce_fact_heads(M,H,CL1,CL2)))))).


remove_duplicated_facts:-doall((gather_fact_heads(M,H),BB=true,once((findall(C,(clause(H,B),B=@=BB,reduce_clause((H:-B),C)),CL1),list_to_set(CL1,CL2),once(reduce_fact_heads(M,H,CL1,CL2)))))).
rerun_database_hooks:-doall((gather_fact_heads(_M,H),forall(clause(H,true),run_database_hooks(assert(z),H)))).



reduce_fact_heads(_M,_H,CL1,CL1):-!. % no change
reduce_fact_heads(M,H,CL1,CL2):- 
 ignore((
   predicate_property(M:H,dynamic),
   length(CL1,L1),length(CL2,L2),
   dmsg(reduce_fact_heads(M,H,from(L1,L2))),
   retractall(M:H),
   % forall(member(C,CL1),retractall(M:C)),
   forall(member(C,CL2),assertz(M:C)))).

gather_fact_heads(M,H):- (nonvar(M)->true; member(M,[dbase,moo,world,user,hook])), current_predicate(M:F/A), 
  once((once((A>0,atom(F),F\=(:),var(H), debugOnError(functor(H,F,A)))),compound(H),predicate_property(M:H,number_of_clauses(_)),
  not((arg(_,vv(system,bugger,logicmoo_util_dcg,user),M);predicate_property(M:H,imported_from(_)))))).



hook:decl_database_hook(assert(_),Fact):-
            ignore((compound(Fact),Fact=..[F,Arg1|PROPS],argsIsaProps(F),!,            
            decl_mpred(Arg1,[F|PROPS]))).



% ========================================
% mpred_props database
% ========================================

:-export(argsIsaProps/1).
argsIsaProps(Prop):- arg(_,v(argsIsa,multiValued,singleValued,negationByFailure,formatted,mpred,listValued),Prop).



% pass 2

declare_dbase_local(F):- mpred_prop(F,hasStub),!.
declare_dbase_local(F):- assert_if_new(mpred_prop(F,hasStub)),fail.
declare_dbase_local(F):- mpred_prop(F,isStatic),!.
declare_dbase_local(F):- not(mpred_arity(F,A)),trace_or_throw(not(mpred_arity(F,A))).
declare_dbase_local(F):- must(mpred_arity(F,A)), dynamic(F/A), user_export(F/A),
   dynamic_multifile_exported(F/A),   
   functor(HEAD,F,A),HEAD=..[F|_ARGS],ensure_clause(HEAD,F,A,body_req(F,A,HEAD)),
   assert_if_new(mpred_prop(F,hasStub)).


body_req(F,_A,HEAD):- mpred_prop(F,external(Module)),!,call(Module:HEAD).
%body_req(isa,2,_):-!,fail.
%body_req(_,_,HEAD):- req(HEAD).
body_req(F,A,HEAD):-mpred_prop(F,default(V)),arg(A,HEAD,V).


user_export(_):- dbase_mod(user),!.
user_export(Prop/Arity):- 
   dbase_mod(M), '@'( M:export(Prop/Arity) , M).

% ============================================
% Prolog to Cyc Predicate Mapping
%
%  the following will all do the same things:
%
% :- decl_mpred('BaseKB':isa/2). 
% :- decl_mpred('BaseKB':isa(_,_)). 
% :- decl_mpred(isa(_,_),'BaseKB'). 
% :- decl_mpred('BaseKB',isa,2). 
%
%  Will make calls 
% :- isa(X,Y)
%  Query into #$BaseKB for (#$isa ?X ?Y) 
%
% decl_mpred/N
%
% ============================================

:-dynamic_multifile_exported((loading_module_h/1, loading_game_file/2, loaded_game_file/2)).
not_loading_game_file:-not(loading_game_file(_,_)),loaded_game_file(_,_).


:- dynamic_multifile_exported((decl_mpred/1)).
decl_mpred([]):-!.
decl_mpred(M:FA):-atom(M),!,'@'(decl_mpred(FA),M).
decl_mpred([H|T]):-!,decl_mpred(H),decl_mpred(T).
decl_mpred((H,T)):-!,decl_mpred(H),decl_mpred(T).
decl_mpred(F/A):-!,decl_mpred(F,A).
decl_mpred(C):-decl_mpred(C,[]).

:-dynamic_multifile_exported(decl_mpred/2).
decl_mpred(M:FA,More):-!,decl_mpred(FA,[decl_mpred(M)|More]).
decl_mpred(F/A,More):-!,decl_mpred(F,arity(A)),decl_mpred(F,More),!.
decl_mpred(C,More):-compound(C),!,functor(C,F,A),decl_mpred(F,arity(A)),decl_mpred(F,More),!,decl_mpred(F,argsIsa(C)),!.
decl_mpred(F,A):-number(A),!,decl_mpred(F,arity(A)),!.
decl_mpred(_,[]):-!.
decl_mpred(F,[Prop|Types]):-!,decl_mpred(F,Prop),!,decl_mpred(F,Types),!.
decl_mpred(F,Prop):-assert_if_new(mpred_prop(F,Prop)),fail.
decl_mpred(F,arity(A)):-assert_if_new(mpred_arity(F,A)),!.
decl_mpred(F,external(Module)):- not(dbase_mod(Module)),mpred_arity(F,A),functor(HEAD,F,A),must(predicate_property(Module:HEAD,_)),!.
decl_mpred(F,interArgIsa):- not((mpred_prop(F,external(Module)),not(dbase_mod(Module)))),declare_dbase_local(F),!.
decl_mpred(_,_).

      
functor_check_univ(M:G1,F,List):-atom(M),member(M,[dbase]),!,functor_check_univ(G1,F,List),!.
functor_check_univ(G1,F,List):-must_det(compound(G1)),must_det(G1 \= _:_),must_det(G1 \= _/_),G1=..[F|List],!.

:-export(glean_pred_props_maybe/1).
glean_pred_props_maybe(_:G):-!,compound(G),glean_pred_props_maybe(G).
glean_pred_props_maybe(G):-compound(G),functor(G,F,_),argsIsaProps(F),G=..[F,Arg1|RGS],!,add_mpred_prop_gleaned(Arg1,[F|RGS]),!.

add_mpred_prop_gleaned(Arg1,FRGS):-functor_check_univ(Arg1,F,ARGSISA),add_mpred_prop_gleaned_4(Arg1,F,ARGSISA,FRGS).
add_mpred_prop_gleaned_4(Arg1,_F,[ARG|_],FRGS):-nonvar(ARG),!,decl_mpred(Arg1,[argsIsa(Arg1)|FRGS]).
add_mpred_prop_gleaned_4(Arg1,_F,_,FRGS):-add_mpred_prop(Arg1,FRGS).

user:term_expansion(G,_):- notrace((once(glean_pred_props_maybe(G)),fail)).

% ========================================
% is_holds_true/is_holds_false
% ========================================


:- dbase_mod(M),dynamic_multifile_exported((
          M:dbase_t/1,
          M:dbase_t/2,
          M:dbase_t/3,
          M:dbase_t/4,
          M:dbase_t/5,
          M:dbase_t/6,
          M:dbase_t/7)).

:-export(hilog_functor/1).
hilog_functor(dbase_t).

:-export(is_holds_true_not_hilog/1).
is_holds_true_not_hilog(HOLDS):-is_holds_true(HOLDS),\+ hilog_functor(HOLDS).

:-export(is_holds_true/1).
is_holds_true(Prop):- notrace((atom(Prop),is_holds_true0(Prop))),!.

% k,p,..
is_holds_true0(Prop):-arg(_,vvv(holds,holds_t,dbase_t,asserted_dbase_t,assertion_t,assertion,secondOrder,firstOrder),Prop).

:-export(is_2nd_order_holds/1).
is_2nd_order_holds(Prop):- is_holds_true(Prop) ; is_holds_false(Prop).

:-export(is_holds_false/1).
% is_holds_false(Prop):-notrace((atom(Prop),once((is_holds_false0(Prop,Stem),is_holds_true0(Stem))))).
is_holds_false(Prop):-member(Prop,[not,nholds,holds_f,dbase_f,aint,assertion_f,asserted_dbase_f,retraction,not_secondOrder,not_firstOrder]).

is_holds_false0(Prop,Stem):-atom_concat('not_',Stem,Prop).
is_holds_false0(Prop,Stem):-atom_concat(Stem,'_not',Prop).
is_holds_false0(Prop,Stem):-atom_concat(Stem,'_false',Prop).
is_holds_false0(Prop,Stem):-atom_concat(Stem,'_f',Prop).

:- dynamic_multifile_exported (decl_coerce)/3.

:-decl_mpred(singleValued,1).

% ========================================
% dynamic_multifile_exported database
% ========================================

:- discontiguous(singleValued/1).

:- dynamic_multifile_exported moo:ft_info/2.
:- dynamic_multifile_exported moo:subft/2.

:- dynamic_multifile_exported action_info/1.
:- dynamic_multifile_exported action_info/2.
:- dynamic_multifile_exported action_info/3.
:- dynamic_multifile_exported mud_test/2.
:- dynamic_multifile_exported agent_call_command/2.

:- dynamic_multifile_exported registered_module_type/2.
:- dynamic_multifile_exported action_rules/4.
:- dynamic_multifile_exported agent_text_command/4.
:- dynamic_multifile_exported check_permanence/4.
:- dynamic_multifile_exported call_after_load/1.
:- dynamic_multifile_exported decl_mud_test/2.
:- dynamic_multifile_exported default_type_props/3.
:- dynamic_multifile_exported label_type_props/3.
:- dynamic_multifile_exported loading_module_h/1.
:- dynamic_multifile_exported mpred_arity/2.
:- dynamic_multifile_exported mpred_prop/2.
:- dynamic_multifile_exported now_unused/1.
:- dynamic_multifile_exported subclass/2.
:- dynamic_multifile_exported term_specifier_text/2.
:- dynamic_multifile_exported type_grid/3.
:- dynamic_multifile_exported update_charge/2.
:- dynamic_multifile_exported update_stats/2.
:- dynamic_multifile_exported use_usable/4, last_command/2.
:- dynamic_multifile_exported str/4. 
:- dynamic_multifile_exported verb_alias/2.
:- dynamic_multifile_exported named/2, spd/2.
:- dynamic_multifile_exported world_agent_plan/3.
:- dynamic_multifile_exported((action_info/1,action_info/2,action_rules/4,action_info/3,term_specifier_text/2,action_verb_useable/4)).
:- dynamic_multifile_exported((term_anglify/2,term_anglify_last/2, term_anglify_np/3,term_anglify_np_last/3)).
:- dynamic_multifile_exported((update_charge/2,update_stats/2,default_type_props/3)).

:- module_transparent register_module_type/1.

:-dynamic_multifile_exported(thlocal:session_agent/2).
:-thread_local thlocal:dbase_change/2.
:-thread_local thlocal:dbase_opcall/2.
:-thread_local thlocal:repl_to_string/2.
:-thread_local thlocal:repl_writer/2.
:-thread_local thlocal:session_agent/2.


current_context_module(Ctx):-loading_module_h(Ctx),!.
current_context_module(Ctx):-context_module(Ctx).

% ========================================
% begin/end_transform_moo_preds
% ========================================

:-thread_local is_compiling_clause/0.
is_compiling:-is_compiling_clause;compiling.
:-thread_local ended_transform_moo_preds/0, always_expand_on_thread/1, prevent_transform_moo_preds/0, may_moo_term_expand/1, always_transform_heads/0.
:-module_transparent begin_transform_moo_preds/0, end_transform_moo_preds/0.
:-export((begin_transform_moo_preds/0,end_transform_moo_preds/0)).
begin_transform_moo_preds:- retractall(ended_transform_moo_preds),context_module(CM),asserta(may_moo_term_expand(CM)).
end_transform_moo_preds:- retractall(ended_transform_moo_preds),asserta(ended_transform_moo_preds).


% ========================================
% register_module_type/end_module_type
% ========================================
register_module_type(Type):-current_context_module(CM),register_module_type(CM,Type).
register_module_type(CM,Types):-is_list(Types),!,forall(member(T,Types),register_module_type(CM,T)).
register_module_type(CM,Type):-asserta_new(registered_module_type(CM,Type)).
end_module_type(Type):-current_context_module(CM),end_module_type(CM,Type).
end_module_type(CM,Type):-retractall(registered_module_type(CM,Type)).


% ========================================
% into_asserted_form/into_mpred_form
% ========================================

:-'$hide'(expanded_different/2).
:-export(expanded_different/2).

expanded_different(G0,G1):-G0==G1,!,fail.
expanded_different(G0,G1):-expanded_different_1(G0,G1),!.
expanded_different(G0,G1):- G0\==G1.

expanded_different_1(NV:G0,G1):-nonvar(NV),!,expanded_different_1(G0,G1).
expanded_different_1(G0,NV:G1):-nonvar(NV),!,expanded_different_1(G0,G1).
expanded_different_1(G0,G1):- (var(G0);var(G1)),!,trace_or_throw(expanded_different(G0,G1)).
expanded_different_1(G0,G1):- G0 \= G1,!.


:-export(into_asserted_form/2).
into_asserted_form(M:X,O):- atom(M),!,into_asserted_form(X,O).
into_asserted_form(X,O):- X=..[F|A],into_asserted_form(X,F,A,O).

% TODO finish negations
into_asserted_form(X,dbase_t,_A,X).
into_asserted_form(X,holds_t,_A,X).
into_asserted_form(X,cholds_t,_A,X).
into_asserted_form(_X,F,A,Call):-Call=..[dbase_t,F|A].

:- meta_predicate(call_after_game_load(-)).
call_after_game_load(Code):- call_after_next(moo:not_loading_game_file,Code).

hook:into_assertion_form_trans_hook(G,was_asserted_gaf(G)):- functor(G,F,_A),mpred_prop(F,isStatic),!.

:-export(into_assertion_form/2).
into_assertion_form(M:H,G):-atom(M),!,into_assertion_form(H,G).
into_assertion_form(H,G):- call_no_cuts((hook:into_assertion_form_trans_hook(H,G))),expanded_different(H,G),!.
into_assertion_form(H,GO):-expand_term( (H :- true) , C ), reduce_clause(C,G),expanded_different(H,G),!,into_assertion_form(G,GO),!.
into_assertion_form(X,O):- functor(X,F,A),into_assertion_form_via_mpred(X,F,A,O),!.
into_assertion_form(X,O):- into_assertion_form(dbase_t,X,O),!.

into_assertion_form_via_mpred(X,F,_A,O):- mpred_prop(F,dynamic_in_module),!,X=O.
into_assertion_form_via_mpred(X,F,_A,O):- mpred_prop(F,as_is(_)),!,X=O.
into_assertion_form_via_mpred(X,F,_A,O):- not(mpred_prop(F,dbase_t)),!,X=O.

:-export(into_assertion_form/3).
into_assertion_form(HLDS,M:X,O):- atom(M),!,into_assertion_form(HLDS,X,O),!.
into_assertion_form(HLDS,X,O):- X=..[F|A],into_assertion_form(HLDS, X,F,A,O),!.

% TODO finish negations
into_assertion_form(Dbase_t,X,Dbase_t,_A,X):-!.
into_assertion_form(Dbase_t,_X,holds_t,A,Call):-Call=..[Dbase_t|A].
into_assertion_form(Dbase_t,_X,cholds_t,A,Call):-Call=..[Dbase_t|A].
into_assertion_form(Dbase_t,_X,HLDS,A,Call):- is_holds_true(HLDS), Call=..[Dbase_t|A].
into_assertion_form(Dbase_t,_X,F,A,Call):-Call=..[Dbase_t,F|A].

:-export(into_mpred_form/2).

into_mpred_form(M:X,O):- atom(M),!,into_mpred_form(X,O),!.
into_mpred_form(was_asserted_gaf(H),G):-!,into_mpred_form(H,G),!.
into_mpred_form(H,GO):- once((expand_term( (H :- true) , C ), reduce_clause(C,G))),expanded_different(H,G),!,into_mpred_form(G,GO),!.
into_mpred_form(X,O):- X=..[F,P|A],into_mpred_form(X,F,P,A,O),!.

% TODO confirm negations
into_mpred_form(_X,H,P,A,O):-is_holds_true(H),(atom(P)->O=..[P|A];O=..[cholds_t,P|A]).
into_mpred_form(_X,H,P,A,O):-is_holds_false(H),(atom(P)->(G=..[P|A],O=not(G));O=..[cholds_f,P|A]).
into_mpred_form(X,_H,_P,_A,X).




create_queryPred(H,B):-functor(H,HF,HA),functor(B,BF,BA),
      dynamic_multifile_exported((HF / HA)), 
      dynamic_multifile_exported((BF / BA)),
      asserta((H :- call_no_cuts(B))).


% ========================================
% include_moo_files(MASK)
% ========================================

include_moo_files(Mask):- expand_file_name(Mask,X),
     forall(member(E,X),ensure_moo_loaded(E)).
/*
module(M,Preds):-
    'format'(user_error,'% visting module ~w.~n',[M]),
    forall(member(P,Preds),export(P)).
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

:- dynamic registered_module_type/2.

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


% ========================================
% enter_term_anglify(MASK)
% ========================================

enter_term_anglify(X,Y):-findall(X-Y-Body,clause( term_anglify(X,Y),Body),List),!,member(X-Y-Body,List),call(Body).
enter_term_anglify(X,Y):-findall(X-Y-Body,clause( term_anglify_np(X,Y),Body),List),!,member(X-Y-Body,List),call(Body).
enter_term_anglify(X,Y):-findall(X-Y-Body,clause( term_anglify_last(X,Y),Body),List),!,member(X-Y-Body,List),call(Body).
enter_term_anglify(X,Y):-findall(X-Y-Body,clause( term_anglify_np_last(X,Y),Body),List),!,member(X-Y-Body,List),call(Body).


/*
fix_fa(FA,F,A):-var(FA),!,mpred_arity(F,A),functor(FA,F,A).
fix_fa(_:FA0,F,A):-!,fix_fa(FA0,F,A),!.

fix_fa(F,F,A):-atom(F),!,mpred_arity(F,A).

fix_fa(FA,F0,A0):- fix_fa0(FA,F,A), fix_fa1(F, A,F0,A0).

fix_fa0(_:FA,F,A):-!,fix_fa0(FA,F,A).
fix_fa0(F/A,F,A):-!.
fix_fa0(F,F,_):-atom(F),!.
fix_fa0(FA,F,A):-functor(FA,F,A).

fix_fa1(F,A,F0,A0):-not(atom(F)),trace_or_throw(fix_fa1(F,A,F0,A0)).
fix_fa1(F,A,F,A0):- number(A),A0=A.
fix_fa1(F,A,F,A):- mpred_arity(F,A).
*/
/*
fix_fa1(FA,F,AR):- compound(FA),functor(FA,F,A), (mpred_arity(F,AA) -> (AA=A -> ignore(AR=A)  ; arg(_,vv(A,AA),AR) ) ; ignore(AR=A) ).
fix_fa1(FA,F,A):- mpred_arity(F,A),functor(FA,F,A).
fix_fa1(FA,F,AR):- atom(FA),!,FA=F, A=0, (mpred_arity(F,AA) -> (AA=A -> ignore(AR=A)  ; arg(_,vv(A,AA),AR) ) ; ignore(AR=A) ).
fix_fa1(FA,F,A):-get_functor(FA,FA2),!,fix_fa(FA2,F,A).
*/

% ================================================
% is_asserted/1
% ================================================

had_module(M:X,X):-atom(M).

is_asserted(X):- loop_check_throw(is_asserted_lc(X)).

is_asserted_lc(M:X):- atom(M),!,is_asserted_lc(X).
is_asserted_lc(X):- ground(X),!,is_asserted_lc_1(X),!.
% is_asserted_lc(X):-  clause_present(X),!.
is_asserted_lc(X):- is_asserted_lc_1(X).

is_asserted_lc_1(X):- into_asserted_form(X,O),clause(O,true).


% ============================================
% Prolog will_call_after/do_all_of
% ============================================

:-dynamic(moo:will_call_after/2).

call_after(When,C):- When,!,do_all_of(When),must_det(C),!.
call_after(When,C):- assert_next(When,C),!.


assert_next(_,_:true):-!.
assert_next(_,true):-!.
assert_next(When,C):- clause_asserted(moo:will_call_after(When,show_call(C))),!.
assert_next(When,C):- assertz_if_new(moo:will_call_after(When,show_call(C))).

call_after_next(When,C):- ignore((When,!,do_all_of(When))),assert_next(When,C).

do_all_of(When):- ignore(( once(moo:will_call_after(When,_)), repeat, retract(moo:will_call_after(When,A)),
                           % dmsg(doingNow(When,A)),
                           call(A), not(moo:will_call_after(When,_)))),!.


%:- create_queryPred(createableType(A) , createableType(A)).
%:- create_queryPred(createableType(A,B) , createableType(A,B)).

:- dynamic_multifile_exported(subclass/2).
%:- create_queryPred(subclass(A,B) , subclass(A,B)).


:- create_queryPred(qqcall_update_charge(A,B) , update_charge(A,B)).
:- create_queryPred(qqcall_update_charge(A,B) , update_stats(A,B)).


:- create_queryPred(coerce0(A,B,C) , decl_coerce(A,B,C)).

decl_coerce(_,_,_):-fail.
coerce(What,Type,NewThing):-decl_coerce(What,Type,NewThing),!.
coerce(What,_Type,NewThing):-NewThing = What.



define_argType(F,N,ArgType):-decl_mpred(F,argIsa(N,ArgType)).




:- dynamic_multifile_exported do_expand_args/3.

do_expand_args(Exp,Term,Out):- compound(Term),!,do_expand_args_c(Exp,Term,Out).
do_expand_args(_,Term,Term).

do_expand_args_c(Exp,[L|IST],Out):- !,do_expand_args_l(Exp,[L|IST],Out).
do_expand_args_c(Exp,Term,Out):- Term=..[P|ARGS],do_expand_args_pa(Exp,P,ARGS,Out).

do_expand_args_pa(Exp,Exp,ARGS,Out):- !,member(Out,ARGS).
do_expand_args_pa(Exp,P,ARGS,Out):- do_expand_args_l(Exp,ARGS,EARGS), Out=..[P|EARGS].

:- dynamic_multifile_exported is_type/1.
:- dynamic_multifile_exported define_type/1.

define_type(Var):-var(Var),!,trace_or_throw(define_type(Var)).
define_type(M:F):-!, '@'(define_type(F), M).
define_type([]):-!.
define_type([A]):-!,define_type(A).
define_type([A|L]):-!,define_type(A),define_type(L).
define_type((A,L)):-!,define_type(A),define_type(L).
define_type(Spec):-is_type(Spec),!.

define_type(Spec):- asserta_if_new(is_type(Spec)), assertz(dbase_t(isa,Spec,type)),
      run_database_hooks(assert(z),isa(Spec,type)),decl_mpred(Spec,1).

%is_type(Spec):- is_asserted(isa(Spec,type)).

define_type_if_atom(T):- compound(T),!.
define_type_if_atom(T):- ignore((atom(T),not(number(T)),define_type(T))).

hook:decl_database_hook(assert(_A_or_Z),label_type_props(Lbl,T,Props)):- hooked_assertz(default_type_props(self,T,[label(self,Lbl)|Props])).
hook:decl_database_hook(assert(_A_or_Z),default_type_props(_,T,_)):- define_type_if_atom(T).
hook:decl_database_hook(assert(_A_or_Z),subclass(S,C)):-define_type_if_atom(S),define_type_if_atom(C).

is_creatable_type(Type):- arg(_,vv(agent,item,region,concept),Type).
is_creatable_type(Type):- atom(Type),is_asserted(isa(Type,creatable_type)).

hook:decl_database_hook(assert(_A_or_Z),is_mpred_prop(F,A,P)):- ignore((A==1,define_type(F))) , ignore((atom(P),define_type(P))).

do_expand_args_l(_,A,A):- var(A),!.
do_expand_args_l(_,[],[]):- !.
do_expand_args_l(Exp,[A|RGS],[E|ARGS]):- do_expand_args(Exp,A,E),do_expand_args_l(Exp,RGS,ARGS).



:- meta_predicate tick_every(*,*,0).
:- meta_predicate register_timer_thread(*,*,0).

:- dynamic_multifile_exported((subclass/2, argsIsa/1 ,createableType/1, createableSubclassType/2)).

/*
:- dynamic_multifile_exported((createableSubclassType(type,type))).
:- dynamic_multifile_exported((createableType(type))).
:- dynamic_multifile_exported subclass/2.
:- dynamic_multifile_exported formatted/1.
% :- dynamic_multifile_exported moo:label_type/2.
:- dynamic_multifile_exported type_grid/3.
*/
:- dynamic_multifile_exported(formatted/1).


register_timer_thread(Name,_Seconds,_OnTick):-current_thread(Name,_Status).
register_timer_thread(Name,Seconds,OnTick):-
   thread_create(tick_every(Name,Seconds,OnTick),_ID,[alias(Name)]). 

tick_every(Name,Seconds,OnTick):-repeat,sleep(Seconds),catch(OnTick,E,dmsg(caused(Name,OnTick,E))),fail.

hdr_debug(_,_):-!.
hdr_debug(F,A):-'format'(F,A).

:- dynamic_multifile_exported registered_module_type/2.




:-meta_predicate term_expansion_local(?,?),term_expansion_local0(?,?).
% :-meta_predicate user:term_expansion(?,?).

term_expansion_local(X,_):-not(compound(X)),!,fail.
term_expansion_local( ((':-'(_))) , _ ):-!,fail.

term_expansion_local(_:B1,B2):-!,term_expansion_local(B1,B2),!.

term_expansion_local(((H1:-B1)),H2B2):- !,
   nonvar(B1),  current_context_module(CM),!,        
      functor(H1,F,A), atom_concat(P,'_hook',F),!,atomic_list_concat([_,_|_],'_',P),
      H1=..[F|ARGS], H2=..[P|ARGS],
      B2 = '@'((nop(CM), B1), CM ),
      module_transparent((P/A)),
      % copy_term(H2,META), meta_predicate(META),
      dynamic_multifile_exported(P/A),
      export(P/A),
      ignore(H2B2 = ((H2 :- B2))),!.

term_expansion_local(X,Y):- compound(X),loading_module_h(CM),functor(X,F,A),term_expand_local(CM,X,F,A,Y).



term_expand_local(CM,X,F,A,Y):-findall(Y,term_expand_local_each(CM,X,F,A,Y),Ys), Ys == [],!,fail.  

term_expand_local_each(_,_,F,A,_):- member(F / A,[never_expand]),!,fail.
term_expand_local_each(CM,X,F,A,X):-registered_module_type(CM,utility),export(F/A).
term_expand_local_each(CM,X,F,A,X):-registered_module_type(CM,dynamic),dynamic(F/A).

term_expansion_local0(A,B):- compound(A),term_expansion_local(A,B),!.

% user:term_expansion(X,Y):- term_expansion_local0(X,Y).

%:- include(logicmoo('vworld/moo_header.pl')).

:- register_module_type(utility).

agent_text_command(_Agent,_Text,_AgentTarget,_Cmd):-fail.

:- include(logicmoo('dbase/dbase.pl')).

%:- include(logicmoo('vworld/moo_footer.pl')).

