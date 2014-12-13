/** <module> 
% File used as storage place for all predicates which change as
% the world is run.
%
% props(Obj,height(ObjHt))  == k(height,Obj,ObjHt) == rdf(Obj,height,ObjHt) == height(Obj,ObjHt)
% padd(Obj,height(ObjHt))  == padd(height,Obj,ObjHt,...) == add(QueryForm)
% kretract[all](Obj,height(ObjHt))  == kretract[all](Obj,height,ObjHt) == pretract[all](height,Obj,ObjHt) == del[all](QueryForm)
% keraseall(AnyTerm).
%
%
% Dec 13, 2035
% Douglas Miles
*/

:- dynamic_multifile_exported fact_always_true/1.

assert_isa_motel(I,T):- assert_if_new(dbase_t(T,I)).

subclass_asserted_motel(T,ST):- dbase_t(subclass,T,ST).




% is_known_false(subclass(A,B)):-disjointWith(A,B).



disjointWith(A,B):- A=B,!,fail.
disjointWith(A,B):-disjointWithT(A,B).
disjointWith(A,B):-disjointWithT(AS,BS),transitive_subclass_or_same(A,AS),transitive_subclass_or_same(B,BS).
disjointWith(A,B):- once((type_isa(A,AT),type_isa(B,BT))),AT \= BT.

disjointWithT(A,B):-disjointWith_asserted(A,B).
disjointWithT(B,A):-disjointWith_asserted(A,B).


disjointWith_asserted(agent,item).
disjointWith_asserted(region,obj).
disjointWith_asserted(formattype,item).
disjointWith_asserted(formattype,obj).
disjointWith_asserted(formattype,region).
disjointWith_asserted(createableType,nonCreatableType).


:- decl_mpred_hybrid(typeDeclarer/1).


decl_type_motel(Spec):- assert_isa(Spec,type).


:-swi_export(assert_subclass_motel/2).
assert_subclass_motel(O,T):- ignore((nonvar(O),decl_type_safe(O),nonvar(T),decl_type_safe(T),nonvar(O),not(formattype(O)),not(formattype(T)),add(subclass(O,T)))).

subclass_backchaing_motel(A,T):- fact_loop_checked(subclass(A,T),transitive_P_l_r(dbase_t,subclass,A,T)).
      
transitive_subclass_or_same(A,A).
% transitive_subclass_or_same(A,AA):-compound(A),into_single_class(A,AA).
transitive_subclass_or_same(A,B):-nonvar(A),!,build_genls_inst_list_cache(A,subclass,B,Call),Call.
transitive_subclass_or_same(A,B):-subclass_backchaing(A,B).

transitive_P_l_r(DB,P,FT,Sub):-call(DB,P,FT,Sub).
transitive_P_l_r(DB,P,FT,Sub):-var(FT),!,transitive_P_r_l(DB,P,FT,Sub).
transitive_P_l_r(DB,P,FT,Sub):-call(DB,P,FT,A),call(DB,P,A,Sub).
transitive_P_l_r(DB,P,FT,Sub):-call(DB,P,FT,A),call(DB,P,A,B),call(DB,P,B,Sub).
transitive_P_l_r(_,_P,_FT,Sub):-var(Sub),!,fail.
transitive_P_l_r(DB,P,FT,Sub):-call(DB,P,FT,A),call(DB,P,A,B),call(DB,P,B,C),call(DB,P,C,Sub).

transitive_P_r_l(DB,P,FT,Sub):-call(DB,P,A,Sub),call(DB,P,FT,A).
transitive_P_r_l(DB,P,FT,Sub):-call(DB,P,B,Sub),call(DB,P,A,B),call(DB,P,FT,A).
transitive_P_r_l(_,_P,_FT,Sub):-var(Sub),!,fail.
transitive_P_r_l(DB,P,FT,Sub):-call(DB,P,C,Sub),call(DB,P,B,C),call(DB,P,A,B),call(DB,P,FT,A).


% ================================================
% assert_isa HOOKS
% ================================================
decl_database_hook(_,subclass(_,_)):-retractall(dbase_t(_,isa,_,_)),retractall(dbase_t(_,subclass,_,_)).
decl_database_hook(assert(_),DATA):-into_mpred_form(DATA,O),!,O=isa(I,T),hotrace(doall(assert_isa_hooked(I,T))).
% decl_database_hook(retract(_),isa(I,T)):-doall(db_retract_isa_hooked(I,T)).

assert_isa_hooked(A,_):-retractall(dbase_t(cache_I_L,isa,A,_)),fail.
assert_isa_hooked(F,T):-argsIsaProps(T),decl_mpred(F,T),fail.
assert_isa_hooked(I,T):- not(ground(assert_isa(I,T))),!, trace_or_throw(not(ground(assert_isa(I,T)))).
assert_isa_hooked(I,T):- asserta_if_new(dbase_t(T,I)),fail.

assert_isa_hooked(T,type):-!,decl_type(T),!.
assert_isa_hooked(T,formattype):-!,define_ft(T),!.
assert_isa_hooked(Term,mpred):-!,decl_mpred(Term).
assert_isa_hooked(Term,prologHybrid):-!,decl_mpred_hybrid(Term).
assert_isa_hooked(Term,prologOnly):-!,decl_mpred_prolog(Term).
assert_isa_hooked(I,_):- glean_pred_props_maybe(I),fail.
assert_isa_hooked(I,T):- argsIsaProps(T),decl_mpred(I,T),fail.
assert_isa_hooked(food5,'Weapon'):-trace_or_throw(assert_isa(food5,'Weapon')).

% assert_isa_hooked(I,T):- motel:defconcept(I,and([lexicon,T])).
% assert_isa_hooked(I,T):- motel:defprimconcept(I,T).
% assert_isa_hooked(I,T):-dmsg((told(assert_isa(I,T)))).


decl_database_hook(assert(_),isa(I,T)):- doall(assert_isa_hooked_after(I,T)).

assert_isa_hooked_after(F,T):-argsIsaProps(T),!,decl_mpred(F,T).
assert_isa_hooked_after(_,type):-!.
assert_isa_hooked_after(_,formattype):-!.
assert_isa_hooked_after(I,T):- createableType(T),!,assert_isa_hooked_creation(I,T).
assert_isa_hooked_after(I,T):- not(completeExtentAsserted(T)),subclass_backchaing(T,ST),completeExtentAsserted(ST),assert_isa(I,ST).
%assert_isa_hooked_after(I,T):- assert_isa_hooked_creation(I,T).

completeExtentAsserted(Ext):- arg(_,vv(type,dir,formattype,string),Ext).
completeExtentAsserted(F):- createableType(F).
%completeExtentAsserted(F):- argsIsaProps(F).
completeExtentAsserted(F):- is_asserted(isa(F,completeExtentAsserted)).




% one of 4 special types
assert_isa_hooked_creation(I,T):- createableType(T),!,call_after_game_load((create_instance(I,T,[]))).
% sublass of 4 special types
assert_isa_hooked_creation(I,T):- doall((createableType(ST),subclass_backchaing(T,ST),call_after_game_load((create_instance(I,ST,[isa(T)]))))).

:-swi_export( transitive_subclass_tst/2).
transitive_subclass_tst(_,_):-!,fail.


% isa_backchaing(A,T):- stack_depth(Level),Level>650,trace_or_throw(skip_dmsg_nope(failing_stack_overflow(isa_backchaing(A,T)))),!,fail.

:-decl_mpred_prolog(isa_backchaing/2).
isa_backchaing(A,T):- fact_loop_checked(isa(A,T),no_repeats(isa_backchaing_0(A,T))).

isa_backchaing_v_nv(A,term):-nonvar(A),!.
isa_backchaing_v_nv(_,var):-!.
isa_backchaing_v_nv(A,T):-no_repeats([A],(transitive_subclass_or_same(AT,T),isa_asserted(A,AT))).

:-swi_export(isa_backchaing_0/2).
isa_backchaing_0(A,T):- T==var,!,var(A).
isa_backchaing_0(A,T):-  var(A),nonvar(T),!,isa_backchaing_v_nv(A,T).
isa_backchaing_0(A,T):-  var(T),!,setof(TT,AT^(isa_asserted(A,AT),transitive_subclass_or_same(AT,TT)),List),!,member(T,List).
isa_backchaing_0(A,T):-  nonvar(A),isa_backchaing_nv_nv(A,T),!.
isa_backchaing_0(A,T):-  transitive_subclass_or_same(AT,T),isa_asserted(A,AT).

% ==========================
% taxonomicPair(isa,subclass)
% ==========================

% A->TL = (isa_asserted_0(A,AT),transitive_subclass_or_same(AT,TL))
build_isa_inst_list_cache(A,Isa,B,(!,dbase_t(cache_I_I,Isa,A,B))):- Isa=isa, nonvar(A),once(dbase_t(cache_I_I,Isa,A,_)),!.
build_isa_inst_list_cache(A,Isa,B,(!,dbase_t(cache_I_I,Isa,A,B))):- Isa=isa, nonvar(A),
      forall(((isa_asserted_0(A,AT),transitive_subclass_or_same(AT,T))),assertz_if_new(dbase_t(cache_I_I,Isa,A,T))).
      

% A->TL = subclass_backchaing(T,TL).
build_genls_inst_list_cache(A,Subclass,B,dbase_t(cache_I_I,Subclass,A,B)):- Subclass=subclass, nonvar(A),once(dbase_t(cache_I_I,Subclass,A,_)),!.
build_genls_inst_list_cache(A,Subclass,B,dbase_t(cache_I_I,Subclass,A,B)):- Subclass=subclass, nonvar(A),forall(subclass_backchaing(A,T),assertz_if_new(dbase_t(cache_I_I,Subclass,A,T))).




isa_backchaing_nv_nv(A,argsIsaInList):-!,compound(A).
isa_backchaing_nv_nv(I,T):-compound(I),functor(I,F,_),isa_backchaing(F,T),!.
isa_backchaing_nv_nv(I,T):-atom(T),!,catch(call(T,I),_,fail).

not_ft(T):-transitive_subclass_or_same(T,spatialthing).

:-swi_export(isa_asserted/2).
isa_asserted_motel(A,T):-no_repeats(isa_asserted_nr(A,T)).
isa_asserted_nr(A,T):- nonvar(A),fail,build_isa_inst_list_cache(A,_,T,CALL),CALL.
isa_asserted_nr(A,T):- stack_check,fact_loop_checked(isa(A,T),isa_asserted_0(A,T)).

isa_asserted_0(I,T):-atom(I),isa_w_inst_atom(I,T).
isa_asserted_0(I,T):-fact_always_true(isa(I,T)).



isa_asserted_0(I,T):- ((thlocal:useOnlyExternalDBs,!);thglobal:use_cyc_database),(kbp_t([isa,I,T]);kbp_t([T,I])).
isa_asserted_0(I,T):-clause(dbase_t(T,I),true).
isa_asserted_0(I,T):-clause(isa(I,T),true).

isa_asserted_0(I,T):-mpred_prop(I,T),T\=mped_type(_).
isa_asserted_0(_,T):-var(T),!,fail.
isa_asserted_0(I,'&'(T1 , T2)):-nonvar(T1),var(T2),!,dif:dif(T1,T2),isa_backchaing(I,T1),subclass_backchaing(T1,T2),isa_backchaing(I,T2).
isa_asserted_0(I,'&'(T1 , T2)):-nonvar(T1),!,dif:dif(T1,T2),isa_backchaing(I,T1),isa_backchaing(I,T2).
isa_asserted_0(I,(T1 ; T2)):-nonvar(T1),!,dif:dif(T1,T2),isa_backchaing(I,T1),isa_backchaing(I,T2).
isa_asserted_0(I,formattype):-!,isa_w_type_atom(I,formattype).
isa_asserted_0(I,type):-!,isa_w_type_atom(I,type).
isa_asserted_0(I,T):-atom(T),isa_w_type_atom(I,T).
isa_asserted_0(I,T):-nonvar(I),isa_asserted(T,formattype),!,term_is_ft(I,T).

% isa_asserted_0(I,T):- compound(I),functor(I,F,_),!,isa_backchaing_1(F,T).

%isa_w_inst_atom(O,T):- atomic_list_concat_catch([T,_|_],'-',O),!.
%isa_w_inst_atom(O,T):- atom_concat(T,Int,O),catch(atom_number(Int,_),_,fail),!.
isa_w_inst_atom(_,T):- T==atom. 

isa_w_type_atom(I,formattype):- clause(ft_info(I,_),true).
isa_w_type_atom(I,formattype):-!, clause(subft(I,_),true).
isa_w_type_atom(I,T):- argsIsaProps(T),!,mpred_prop(I,T).
isa_w_type_atom(I,T):- clause(mpred_prop(I,T),true).
isa_w_type_atom(I,T):- G=..[T,I],once_if_ground(isa_atom_call(T,G),_).

dont_call_type_arity_one(type).
dont_call_type_arity_one(formattype).
dont_call_type_arity_one(agenttype).
dont_call_type_arity_one(F):-mpred_prop(F,stubType(prologHybrid)),!.

isa_atom_call(T,G):-loop_check(isa_atom_call_lc(T,G),fail).

isa_atom_call_lc(_,G):- predicate_property(G,builtin),!,G.
isa_atom_call_lc(Type,_):-dont_call_type_arity_one(Type),!,fail.
isa_atom_call_lc(_,G):- predicate_property(G,number_of_clauses(_)),!,clause(G,B),call_mpred_body(G,B).
isa_atom_call_lc(_,G):- predicate_property(G,number_of_rules(R)),R>0,!,G.

