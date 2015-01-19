
provide_mpred_write_attributes(F,A,multifile):- multifile(F/A).
provide_mpred_write_attributes(F,A,thread_local):- decl_thlocal(F/A).
provide_mpred_write_attributes(F,A,dynamic):- dynamic(F/A).


provide_mpred_storage_clauses(prolog,H,B):-predicate_property(H,number_of_clauses(_)),clause(H,B).




provide_mpred_write_attributes(F,external(Module)):- dmsg(decl_mpred(F,external(Module))),not(dbase_mod(Module)),must_det(mpred_arity(F,A)),functor(HEAD,F,A),must_det(predicate_property(Module:HEAD,_)),!.



% HOOK
provide_mpred_storage_impl(prologOnly,M,F,A) :- declare_dbase_local_dynamic(M,F,A).


:-op(0,fx,decl_mpred_prolog).

:-swi_export(decl_mpred_prolog/1).
decl_mpred_prolog(P):- with_pi(P,decl_mpred_prolog).

:-swi_export(decl_mpred_prolog/3).
decl_mpred_prolog(M,PI,F/A):- 
 decl_mpred_prolog(_,M,PI,F/A).

:-swi_export(decl_mpred_prolog/4).
decl_mpred_prolog(CM,M,PI,F/A):-
      dynamic_multifile_exported(CM,M,PI,F/A),
      debugOnError(assert_if_new(mpred_prop(F,prologOnly))),
      debugOnError(assert_if_new(mpred_arity(F,A))),
      assert_arity(F,A),
      swi_export(F/A),
      % retractall(mpred_prop(F,_)),   
      decl_mpred(F,prologOnly),   
      decl_mpred(F,info(decl_mpred_prolog(M:F/A))),
      decl_mpred(F,predModule(M)),
      module_transparent(F/A),
      ignore((ground(PI),decl_mpred(PI))),
      decl_mpred(F,A).


:-op(1120,fx,decl_mpred_prolog).

 

user_swi_export(_):- dbase_mod(user),!.
user_swi_export(Prop/Arity):- 
   dbase_mod(M), '@'( M:swi_export(Prop/Arity) , M).


provide_mpred_read_attributes(Obj,PropVal):- fail, safe_univ(PropVal,[Prop,NonVar|Val]),safe_univ(CallVal,[Prop,Obj,NonVar|Val]),
     predicate_property(CallVal,_),!,call_mpred(CallVal).


provide_mpred_read_attributes(F,_,_):-mpred_prop(F,prologHybrid),!,fail.
provide_mpred_read_attributes(F,A,prologOnly(F,A)):-mpred_prop(F,prologOnly).
provide_mpred_read_attributes(F,A,Why):-functor_safe(P,F,A),provide_mpred_read_attributes(P,F,A,Why).

provide_mpred_read_attributes(P,F,A,static_predicate(P)):-static_predicate(user,F,A).
provide_mpred_read_attributes(P,_,_,predicate_property(P,foreign)):-predicate_property(P,foreign),!.
provide_mpred_read_attributes(P,_,_,predicate_property(P,builtin)):-predicate_property(P,builtin),!.
provide_mpred_read_attributes(P,_,_,predicate_property(P,imported_from(system))):-predicate_property(P,imported_from(system)).



get_mpred_storage_type(Head,Type):-functor_h(Head,F,A),!,get_mpred_storage_type(Head,F,A,Type).
get_mpred_storage_type(F,A,Type):-functor(P,F,A),get_mpred_storage_type(P,F,A,Type).

get_mpred_storage_type(Head,F,A,Type):-atom(Head),mpred_arity(Head,A),!,dmsg(get_mpred_storage_type(Head,F,A,Type)),get_mpred_storage_type(Head,A,Type).
get_mpred_storage_type(Head,_,_,Type):-compound(Head),!,functor_h(Head,F,A),get_mpred_type4(Head,F,A,Type).
get_mpred_storage_type(_,F,A,Type):-atom(F),number(A),!,functor(Head,F,A),get_mpred_type4(Head,F,A,Type).
get_mpred_storage_type(Head,F,A,Type):-must(mpred_arity(F,A)),functor(Head,F,A),get_mpred_type4(Head,F,A,Type).

get_mpred_type4(P,F,A,T):-get_mpred_type5(P,F,A,T),!.

get_mpred_type5(_,F,_,(Type)):-tPredStubImpl(Type),mpred_prop(F,Type).
get_mpred_type5(_,F,_,(Type)):-member(Type,[tCol]),mpred_prop(F,Type).
get_mpred_type5(P,_,_,W):-compound(P),!,pp_has(P,W).
get_mpred_type5(_,F,A,W):-atom(F),current_predicate(F/A),functor(P,F,A),!,pp_has(P,W).
get_mpred_type5(F,_,A,W):-atom(F),current_predicate(F/A),functor(P,F,A),!,pp_has(P,W).
get_mpred_type5(_P,_F,_A,funknown):-!. % dmsg(warn_pp(not(predicate_property(P,F,A)))).

pp_has(P,callable(dynamic)):-predicate_property(P,dynamic),!.
pp_has(P,callable(static)):-predicate_property(P,dynamic),!.
pp_has(_,unknown).

:- op(1120,fx,decl_mpred_prolog).
  
  

  
  
  
  

  
