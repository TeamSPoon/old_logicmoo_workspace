% ----------
:- export(with_pi/2).
:- module_transparent(with_pi/2).
:- meta_predicate(with_pi(0,4)).
with_pi(_:[],_):-!.
with_pi(M:(P1,P2),Pred3):-!,'@'(( with_pi(M:P1,Pred3),with_pi(M:P2,Pred3) ),M).
with_pi(M:P,Pred3):-context_module(CM),!,with_pi_selected(CM,M,P,Pred3),!.
with_pi([],_):-!.
with_pi((P1,P2),Pred3):-!, with_pi(P1,Pred3),with_pi(P2,Pred3).
with_pi(P,Pred3):-context_module(CM),with_pi_selected(CM,CM,P,Pred3),!.

:- export(with_pi_selected/4).
:- meta_predicate(with_pi_selected(+,+,+,+)).
with_pi_selected(CM, M,[P|L],Pred3):-!,with_pi_selected(CM,M,P,  Pred3),with_pi_selected(CM,M,L,Pred3).
with_pi_selected(CM,M,(P,L),Pred3):-!,with_pi_selected(CM,M,P,  Pred3),with_pi_selected(CM,M,L,Pred3).
with_pi_selected(_CM,_M,[]  ,_Pred3):-!.
with_pi_selected(CM,M,[P]  ,Pred3):-!,with_pi_selected(CM,M,P,  Pred3).
with_pi_selected(CM,_, M:F//A,Pred3):-Ap2 is A+2, !,with_pi_selected(CM,M,F/Ap2,Pred3).
with_pi_selected(CM,_, M:F/A,Pred3):-!,with_pi_selected(CM,M,F/A,Pred3).
with_pi_selected(CM,_, M:P ,Pred3):-!,with_pi_selected(CM,M,P,  Pred3).
with_pi_selected(CM,M, F//A ,Pred3):-Ap2 is A+2,!,functor_safe(P,F,A),  with_pi_stub(CM,M,P,F/Ap2,Pred3).
with_pi_selected(CM,M, F/A ,Pred3):-!,functor_safe(P,F,A),  with_pi_stub(CM,M,P,F/A,Pred3).
with_pi_selected(CM,M, P ,Pred3):-  functor_safe(P,F,A), with_pi_stub(CM,M,P,F/A,Pred3).


% ----------



:- export(with_pi_stub/5).
:- meta_predicate(with_pi_stub(+,+,+,+,3)).
with_pi_stub(CM, M,P, F//A , PM:Pred3):- ((integer(A),atom(M),atom(F),Ap2 is A+2, functor_safe(P,F,Ap2))),
  must(PM:call(Pred3,CM, M,P,F/Ap2)),!.
with_pi_stub(CM, M,P, F/A , PM:Pred3):- ((integer(A),atom(M),atom(F),functor_safe(P,F,A))),
  must(PM:call(Pred3,CM, M,P,F/A)),!.
with_pi_stub(CM, M,P, F//A , Pred3):- ((integer(A),atom(M),atom(F),Ap2 is A+2,functor_safe(P,F,Ap2))),
   must(call(Pred3,CM, M,P,F/Ap2)),!.
with_pi_stub(CM, M,P, F/A , Pred3):- ((integer(A),atom(M),atom(F),functor_safe(P,F,A))),
   must(call(Pred3,CM, M,P,F/A)),!.
%with_pi_stub(CM, M,P, F/A ,_: Pred3):- ((integer(A),atom(M),atom(F),functor_safe(P,F,A))),  must(call(Pred3,CM, M,P,F/A)),!.
%with_pi_stub(CM, M,P, F/A ,CP: Pred3):-!, must(CP:call(Pred3,CM, M,P,F/A)),!.
%with_pi_stub(CM, M,P, F/A , Pred3):-!, must(call(Pred3,CM, M,P,F/A)),!.

with_pi_stub(CM, M,P,FA,Pred3):- trace_or_throw(invalide_args(CM, M,P,FA,Pred3)).
% ----------

:- export(with_mfa/2).
:- module_transparent(with_mfa/2).
:- meta_predicate(with_mfa(0,3)).
with_mfa(P  ,Pred3):- with_pi(P,with_mfa_of(Pred3)).

:- module_transparent(with_mfa_of/5).
:- meta_predicate(with_mfa_of(3,+,+,+,+)).
with_mfa_of(Pred3,_CM,M,_P,F//A):- Ap2 is A+2, M:call(Pred3,M,F,Ap2).
with_mfa_of(Pred3,_CM,M,_P,F/A):-M:call(Pred3,M,F,A).

% ----------


:-module_transparent(make_transparent/4).
:- export(make_transparent/4).

make_transparent(_CM,M,_PI,F/0):-!, compound_name_arity(C,F,0), M:meta_predicate(C).
make_transparent(CM,_,PI,M:F/A):-!,make_transparent(CM,M,PI,F/A).
make_transparent(_CM,M,PI,F/A):-
   motrace(((var(PI)->functor_safe(PI,F,A);true),
   M:module_transparent(F/A),
   fill_args(PI,('?')),!,
   dbgsubst(PI, (^),(^),PI1),
   dbgsubst(PI1,(0),(0),PI2),
   dbgsubst(PI2,(:),(:),PI3),
   (compound(PI3) -> M:meta_predicate(PI3) ; true))).

% ----------

:- export((dynamic_multifile_exported)/1).
:- export((dynamic_multifile_exported)/4).
:- meta_predicate(( dynamic_multifile_exported(:), dynamic_multifile_exported(+,+,+,+))).
:- module_transparent((dynamic_multifile_exported)/1).
dynamic_multifile_exported( (M:F)/A ):- !,context_module(CM),
'@'((dynamic_safe(M,F,A), 
   (static_predicate(M,F,A) -> true ; ((M:multifile(F/A),CM:multifile(F/A)))),
   % make_transparent(CM,M,PI,F/A),
   M:export(F/A)),M). %,dmsg(dynamic_multifile_exported(CALL)).
dynamic_multifile_exported( FA ):- with_pi(FA,(dynamic_multifile_exported)).
dynamic_multifile_exported(CM, M, _PI, F/A):-
'@'((dynamic_safe(M,F,A), 
   (static_predicate(M,F,A) -> true ; ((M:multifile(F/A),CM:multifile(F/A)))),
   % make_transparent(CM,M,PI,F/A),
   M:export(F/A)),M). %,dmsg(dynamic_multifile_exported(CALL)).

% ----------


:- export(fill_args/2).
fill_args([Arg|More],With):-!,ignore(With=Arg),fill_args(More,With).
fill_args([],_).
fill_args(PI,With):-compound_name_arguments(PI,_,ARGS),fill_args(ARGS,With).

:- meta_predicate(meta_predicate(0)).


:- export(def_meta_predicate/3).
:- meta_predicate((def_meta_predicate(0,+,+))).

def_meta_predicate(M:F,S,E):-!,M:doall(((between(S,E,N),make_list('?',N,List),compound_name_arguments(CALL,F,List),'@'(meta_predicate(CALL),M)))).
def_meta_predicate(F,S,E):- trace_or_throw(def_meta_predicate(F,S,E)).



:- export(remove_pred/3).
remove_pred(_,_,_):-!.
remove_pred(_,F,A):-member(_:F/A,[_:delete_common_prefix/4]),!.
remove_pred(M,F,A):- functor(P,F,A),
  (current_predicate(M:F/A) -> ignore((catchvv(redefine_system_predicate(M:P),_,true),abolish(M:F,A)));true),
  M:asserta((P:-user:wdmsg(error(P)),throw(permission_error(M:F/A)))).



:- meta_predicate(call_if_defined(0)).
:- export(call_if_defined/1).
call_if_defined(G):-current_predicate(_,G),G.


:-module_transparent(p_predicate_property/2).
p_predicate_property(P,PP):-predicate_property(P,PP),!.
p_predicate_property(_:P,PP):-predicate_property(P,PP).
%current_bugger_predicate(M:FF/FA):-nonvar(FF),!,current_predicate(M:FF,FA).
%current_bugger_predicate(FF/FA):-nonvar(FF),!,!,current_predicate(FF/FA).
:-module_transparent(current_predicate_module/2).
current_predicate_module(P,M):-var(P),!,current_predicate(F/A),functor_safe(P,F,A),(nonvar(M)->true;p_predicate_property(P,imported_from(M))).
current_predicate_module(OM:F/A,M):-!,functor_safe(P,F,A),(current_predicate(M:F/A);(current_predicate(OM:F/A),M=OM);current_predicate(F/A)),(nonvar(M)->true;p_predicate_property(P,imported_from(M))).
current_predicate_module(OM:P,M):-!,functor_safe(P,F,A),(current_predicate(M:F/A);(current_predicate(OM:F/A),M=OM);current_predicate(F/A)),(nonvar(M)->true;p_predicate_property(P,imported_from(M))).
current_predicate_module(F/A,M):-!,functor_safe(P,F,A),(current_predicate(M:F/A);(current_predicate(OM:F/A),M=OM);current_predicate(F/A)),(nonvar(M)->true;p_predicate_property(P,imported_from(M))).
current_predicate_module(P,M):-!,functor_safe(P,F,A),(current_predicate(M:F/A);current_predicate(F/A)),(nonvar(M)->true;p_predicate_property(P,imported_from(M))).


dynamic_multifile(Pred/N):-
   dynamic(Pred/N),
   multifile(Pred/N),
   module_transparent(Pred/N).

dynamic_transparent([]):-!.
dynamic_transparent([X]):-dynamic_transparent(X),!.
dynamic_transparent([X|Xs]):-!,dynamic_transparent(X),dynamic_transparent(Xs),!.
dynamic_transparent(M:F/A):-!, module_transparent(M:F/A),dynamic(M:F/A).
dynamic_transparent(F/A):-!,multi_transparent(user:F/A).
dynamic_transparent(X):-functor_catch(X,F,A),dynamic_transparent(F/A),!.

multi_transparent([]):-!.
multi_transparent([X]):-multi_transparent(X),!.
multi_transparent([X|Xs]):-!,multi_transparent(X),multi_transparent(Xs),!.
multi_transparent(M:F/A):-!, module_transparent(M:F/A),dynamic(M:F/A),multifile(M:F/A).
multi_transparent(F/A):-!,multi_transparent(user:F/A).
multi_transparent(X):-functor_catch(X,F,A),multi_transparent(F/A),!.



dynamic_if_missing(F/A):-functor_safe(X,F,A),predicate_property(X,_),!.
dynamic_if_missing(F/A):-dynamic([F/A]).

get_pi(PI,PI):-var(PI),!.
get_pi(F/A,PI):-!,functor(PI,F,A).
get_pi(PI,PI):- atomic(PI),!.
get_pi(PI,PI):- compound(PI),!.
get_pi(Mask,PI):-get_functor(Mask,F,A),functor(PI,F,A),!.


:- meta_predicate get_module_of_4(0,+,+,-).
get_module_of_4(_P,F,A,ModuleName):- current_module(ModuleName),module_property(ModuleName, exports(List)),member(F/A,List),!.
get_module_of_4(_P,F,A,M):- current_predicate(M0:F0/A0),F0=F,A0=A,!,M=M0.
get_module_of_4(P,F,A,M):-trace_or_throw((get_module_of_4(P,F,A,M))).

/*
get_module_of_4(_P,F,A,M):- current_predicate(F0/A0),F0=F,A0=A,!,user:mpred_mod(M).
get_module_of_4(_P,F,A,_M):-trace, isCycPredArity(F,A),!,fail.
get_module_of_4(P,F,A,M):- trace, debugCall(get_module_of_4(P,F,A,M)).
*/

:- meta_predicate get_module_of(0,-).
get_module_of(V,M):-var(V),!,current_module(M).
get_module_of(F/A,M):-!,functor_catch(P,F,A),!,get_module_of(P,M).
get_module_of(P,M):-predicate_property(P,imported_from(M)),!.
get_module_of(P,M):-predicate_property(_:P,imported_from(M)),!.
get_module_of(MM:_,M):-!,MM=M.
get_module_of(P,M):-functor_catch(P,F,A),get_module_of_4(P,F,A,M).



% ----------
:- export(static_predicate/3).
:- meta_predicate(static_predicate(+,+,+)).
static_predicate(M,F,A):- functor_safe(FA,F,A),  once(M:predicate_property(FA,_)),not(M:predicate_property(FA,dynamic)),not((M:predicate_property(FA,imported_from(Where)),Where \== M)).

static_predicate(A):-atom(F),!,current_predicate(F/A),!,functor(FA,F,A),static_predicate(FA).
static_predicate(F/A):-!,atom(F),current_predicate(F/A),!,functor(FA,F,A),static_predicate(FA).
% static_predicate(FA):-predicate_property(FA,built_in),!.
static_predicate(FA):-predicate_property(FA,static),!.
static_predicate(FA):-once(predicate_property(FA,_)),not(predicate_property(FA,dynamic)).



:- export((((dynamic_safe)/1))).
:- meta_predicate(dynamic_safe(+)).
:- module_transparent((((dynamic_safe)/1))).
dynamic_safe(MFA):- with_mfa(MFA,dynamic_safe).

:- export((((dynamic_safe)/3))).
:- meta_predicate(dynamic_safe(+,+,+)).
:- module_transparent((((dynamic_safe)/3))).

convert_to_dynamic(M:FA):- !, get_functor(FA,F,A),convert_to_dynamic(M,F,A).
convert_to_dynamic(FA):- get_functor(FA,F,A), convert_to_dynamic(user,F,A).

convert_to_dynamic(M,F,A):-  functor(C,F,A), M:predicate_property(C,dynamic),!.
convert_to_dynamic(M,F,A):-  M:functor(C,F,A),\+ predicate_property(C,_),!,M:((dynamic(F/A),multifile(F/A),export(F/A))),!.
convert_to_dynamic(M,F,A):-  functor(C,F,A),findall((C:-B),clause(C,B),List),rebuild_as_dyn(M,C,F,A),maplist(assertz,List),!.

rebuild_as_dyn(M,C,_,_):- predicate_property(M:C,dynamic),!.
rebuild_as_dyn(M,C,F,A):- redefine_system_predicate(M:C),M:abolish(F,A),dynamic(M:F/A),multifile(M:F/A),export(F/A),!.

dynamic_safe(M,F,A):- functor(C,F,A),predicate_property(C,imported_from(system)),!,dmsg(warn(predicate_property(M:C,imported_from(system)))).
dynamic_safe(M,F,A):- (static_predicate(M,F,A) -> show_call(convert_to_dynamic(M,F,A)) ; logOnErrorIgnore((dynamic(M:F/A),multifile(M:F/A)))). % , warn_module_dupes(M,F,A).
:-op(1150,fx,user:dynamic_safe).


% pred_prop(Spec,DO,TEST,DONT)
pred_prop((M:F/A),DO,TEST,true):-pred_prop(M:F/A,DO,TEST).
pred_prop(M:F/A,(lock_predicate(M:F/A)),(built_in),unlock_predicate(M:F/A)).
pred_prop(M:F/A, (dynamic(M:F/A)) ,(dynamic), show_call(compile_predicates([F/A]))).

pred_prop(_,(meta_predicate Spec),(meta_predicate Spec)).
pred_prop(M:F/A,multifile(M:F/A)	       ,(multifile)).
pred_prop(M:F/A,module_transparent(M:F/A) ,(transparent)).
pred_prop(M:F/A,discontiguous(M:F/A) ,(discontiguous)).
pred_prop(M:F/A,volatile(M:F/A)	  ,(volatile)).
pred_prop(M:F/A,public(M:F/A)     ,(public)).
pred_prop(M:F/A,thread_local(M:F/A),(thread_local)).
pred_prop(M:F/A,noprofile(M:F/A)	    , (noprofile)).
pred_prop(M:F/A,'$iso'(M:F/A) ,(iso)).


:-dynamic(mpred_impl/2).
:-multifile(mpred_impl/2).

%%  rebuild_pred_into(OMC,NMC,AssertZ,[+dynamic,-built_in,+volatile, etc]).

:-meta_predicate(rebuild_pred_into(0,1,?)).
rebuild_pred_into(C,AssertZ,OtherTraits):-rebuild_pred_into(C,C,AssertZ,OtherTraits).

:-meta_predicate(rebuild_pred_into(0,0,1,?)).
rebuild_pred_into(_,NMC,AssertZ,_):-mpred_impl(NMC,AssertZ),!.
rebuild_pred_into(OMC,NMC,AssertZ,OtherTraits):-
  lsting(OMC),
  asserta(mpred_impl(NMC,AssertZ)),
  show_call((predicate_property(OMC,number_of_clauses(_)))),
  strip_module(OMC, OM, OC),
  strip_module(NMC, NM, NC),
   must_det_l((
      '$set_source_module'(Before, OM),  
      functor(NC,NF,A), functor(OC,OF,A), 
      (show_call(predicate_property(OMC,number_of_clauses(_)))),
      must(show_call_failure(predicate_property(OMC,number_of_clauses(_)))),
      forall(predicate_property(OC,PP),asserta(pp_temp(NC,PP))),
      findall((OC:-B),((clause(OC,B),assertz(pp_clauses((OC:-B))))),List),
      '$set_source_module'(_, NM),
      forall(member(-PP,OtherTraits),retractall(pp_temp(NC,PP))),
      forall(member(+PP,OtherTraits),asserta(pp_temp(NC,PP))),      
      once(pp_temp(NC,(built_in))->(redefine_system_predicate(NF/A),unlock_predicate(NF/A));true),      
      show_call(must(abolish(NF/A))),
      show_call(must(abolish(NF/A))),
      garbage_collect_clauses,
      ignore(convert_to_dynamic(NM,NF,A)),
      garbage_collect_clauses,
      %must(\+ predicate_property(NMC,_)),
      %once(memberchk(CC,List)->true;(CC=((NC:-fail,1234)))),
      %convert_to_dynamic(NM,NF,A),
      %ignore(logOnError(pp_temp(NC,(dynamic))->dynamic(NF/A);true)),
      %ignore(once(pp_temp(NC,(multifile))->multifile(NF/A);true)),
      must(((pp_temp(NC,file(File)),pp_temp(NC,line_count(_Line))))
        -> 
            must(('$compile_aux_clauses'(CC, File),retractall(CC)));
            must(dmsg(noFileFor(NC)))),
      forall(pred_prop(NM:NF/A,TODO,PP,ELSE),(pp_temp(NC,PP)->must(TODO);must(ELSE))),
      (pp_temp(NC,meta_predicate(NC))->meta_predicate(NC);true),
      dbgsubst(List,OF,NF,ListO),maplist(AssertZ,ListO),!,
      
      retractall(mpred_impl(NMC,_)),
      asserta(mpred_impl(NMC,AssertZ)),
      '$set_source_module'(_, Before),
      lsting(NMC),      
      retractall(pp_temp(NC,_))
      )).




 %:-interactor.

export_all_preds:-source_location(File,_Line),module_property(M,file(File)),!,export_all_preds(M).

export_all_preds(ModuleName):-forall(current_predicate(ModuleName:F/A),
                   ((export(F/A),functor_safe(P,F,A),moo_hide_childs(ModuleName:P)))).






module_predicate(ModuleName,F,A):-current_predicate(ModuleName:F/A),functor_safe(P,F,A),
   not((( predicate_property(ModuleName:P,imported_from(IM)),IM\==ModuleName ))).

:-module_transparent(module_predicates_are_exported/0).
:-module_transparent(module_predicates_are_exported/1).
:-module_transparent(module_predicates_are_exported0/1).

module_predicates_are_exported:- context_module(CM),module_predicates_are_exported(CM).

module_predicates_are_exported(user):-!,context_module(CM),module_predicates_are_exported0(CM).
module_predicates_are_exported(Ctx):- show_call(module_predicates_are_exported0(Ctx)).

module_predicates_are_exported0(user):- !. % dmsg(warn(module_predicates_are_exported(user))).
module_predicates_are_exported0(ModuleName):-
   module_property(ModuleName, exports(List)),
    findall(F/A,
    (module_predicate(ModuleName,F,A),
      not(member(F/A,List))), Private),
   module_predicates_are_not_exported_list(ModuleName,Private).

:-export(export_if_noconflict/2).
:-module_transparent(export_if_noconflict/2).
export_if_noconflict(M,F/A):- current_module(M2),M2\=M,module_property(M2,exports(X)),member(F/A,X),dmsg(skipping_export(M2=M:F/A)),!.
export_if_noconflict(M,F/A):-M:export(F/A).

% module_predicates_are_not_exported_list(ModuleName,Private):- once((length(Private,Len),dmsg(module_predicates_are_not_exported_list(ModuleName,Len)))),fail.
module_predicates_are_not_exported_list(ModuleName,Private):- forall(member(F/A,Private),export_if_noconflict(ModuleName,F/A)).





arg_is_transparent(Arg):- member(Arg,[':','0']).
arg_is_transparent(0).
arg_is_transparent(Arg):- number(Arg).

% make meta_predicate's module_transparent
module_meta_predicates_are_transparent(_):-!.
module_meta_predicates_are_transparent(ModuleName):-
    forall((module_predicate(ModuleName,F,A),functor_safe(P,F,A)), 
      ignore(((predicate_property(ModuleName:P,(meta_predicate( P ))),
            not(predicate_property(ModuleName:P,(transparent))), (compound(P),arg(_,P,Arg),arg_is_transparent(Arg))),
                   (dmsg(todo(module_transparent(ModuleName:F/A))),
                   (module_transparent(ModuleName:F/A)))))).

:- export(all_module_predicates_are_transparent/1).
% all_module_predicates_are_transparent(_):-!.
all_module_predicates_are_transparent(ModuleName):-
    forall((module_predicate(ModuleName,F,A),functor_safe(P,F,A)), 
      ignore((
            not(predicate_property(ModuleName:P,(transparent))),
                   ( dmsg(todo(module_transparent(ModuleName:F/A)))),
                   (module_transparent(ModuleName:F/A))))).

quiet_all_module_predicates_are_transparent(_):-!.
quiet_all_module_predicates_are_transparent(ModuleName):-
    forall((module_predicate(ModuleName,F,A),functor_safe(P,F,A)), 
      ignore((
            not(predicate_property(ModuleName:P,(transparent))),
                   nop(dmsg(todo(module_transparent(ModuleName:F/A)))),
                   (module_transparent(ModuleName:F/A))))).



