/** <module> dbase_i_mpred_dbase_t
% Provides a prolog dabase in these predicates...
%
%  dbase_t/N
%  hybridRule/2
%  
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/

:- include(logicmoo('vworld/moo_header.pl')).


provide_mpred_write_attributes(F,A,multifile):- multifile(F/A).
provide_mpred_write_attributes(F,A,thread_local):- decl_thlocal(F/A).
provide_mpred_write_attributes(F,A,dynamic):- dynamic(F/A).


provide_mpred_currently(_OP,Head,prologOnly,declared(_)):- get_functor(Head,F),mpred_prop(F,prologOnly).
provide_mpred_currently(_OP,Head,prologOnly, Will):- get_functor(Head,F,A), current_predicate(F/A) -> Will= will(_) ; Will = wont(_).


provide_mpred_storage_clauses(prolog,H,B):-predicate_property(H,number_of_clauses(_)),clause(H,B).




provide_mpred_write_attributes(F,external(Module)):- dmsg(decl_mpred(F,external(Module))),not(dbase_mod(Module)),must_det(mpred_arity(F,A)),functor(HEAD,F,A),must_det(predicate_property(Module:HEAD,_)),!.



% HOOK
provide_mpred_setup(OP,Head,StubType,OUT):-  StubType = prologOnly, 
  must_det_l(( get_pifunctor(Head,PHead,F,A),   
   doall((clause(PHead,use_provided_mpred_storage_op(call(_),PHead,_),Ref),erase(Ref))),
   show_call(provide_clauses_list(PHead,HBLIST)),
   user:abolish(F,A),dynamic_multifile_exported(user:F/A),
   asserta_if_new(mpred_prop(F,hasStub(StubType))),         
   asserta_if_new(mpred_prop(F,StubType)), 
   forall(member(HB,HBLIST),must(add(HB))),!,   
   doall(retract((user:PHead:-use_provided_mpred_storage_op(call(_),PHead,_)))),
   must_same_clauses(PHead,HBLIST))),
   must(OUT=defined(provide_mpred_setup(OP,StubType))).

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

%retract_all((H:-B)) :-!, forall(clause(H,B,Ref),erase(Ref)).
retract_all(HB) :- ignore((retract(HB),fail)).


is_static_pred(Head:-_):-!,predicate_property(Head,_),not(predicate_property(Head,dynamic)).
is_static_pred(Head):-predicate_property(Head,_),not(predicate_property(Head,dynamic)).

% get_mpred_storage_provider(assert(a), mpred_prop(agent_call_command, info((decl_mpred_prolog user:agent_call_command/2))),S,W)

provide_mpred_storage_op(clause_asserted,Head,prologOnly,CALL):- CALL = clause_asserted(Head).

provide_mpred_storage_op(assert(How),Head,prologOnly,CALL):-   
   get_functor(Head,F,A),
   functor(PF,F,A),
   (predicate_property(PF,_)->true;show_call((dynamic(F/A),multifile(F/A),export(F/A)))),
   (is_static_pred(PF)-> 
     (listing(F/A),dmsg(want_to_assert(How,decl_mpred_hybrid(F,A,Head))),decl_mpred_hybrid(F/A),CALL=add(Head)); 
      (transitive(how_to_op,assert(How),OP),CALL = call_wdmsg(OP,Head))),!.   
   


provide_mpred_storage_op(assert(How),Head,prologOnly,CALL):-   
   get_functor(Head,F,A),
   functor(PF,F,A),
   (predicate_property(PF,_)->true;show_call((dynamic(F/A),multifile(F/A),export(F/A)))),
   (is_static_pred(PF)-> (listing(F/A),trace_or_throw(want_to_assert(How,Head))); true),   
   transitive(how_to_op,assert(How),OP),
   CALL = call_wdmsg(OP,Head).

provide_mpred_storage_op(How,Head,prologOnly,CALL):- 
 transitive(how_to_op,How,OP),
 wdmsg(provide_mpred_storage_op(How,Head,prologOnly,defined(OP))),
 CALL = call_wdmsg(OP,Head).

transitive(X,A,B):- (call(X,A,R) -> ( R\=@=A -> transitive(X,R,B) ; B=R); B=A).


how_to_op(assert(a),asserta_new).
how_to_op(assert(z),assertz_if_new).
how_to_op(retract(one),retract).
how_to_op(retract(all),retract_all).
how_to_op(assert(z),assertz_if_new).
how_to_op(asserta,asserta_new).
how_to_op(assertz,assertz_if_new).
how_to_op(call(_),call).
how_to_op(assert,assert_if_new).
how_to_op(HowOP,HowOP).


:- op(1120,fx,decl_mpred_prolog).
  
  

  
  
  
  

  
