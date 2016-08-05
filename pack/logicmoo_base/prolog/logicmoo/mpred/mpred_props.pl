/* 
% ===================================================================
% File 'mpred_db_preds.pl'
% Purpose: Emulation of OpenCyc for SWI-Prolog
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'interface.pl' 1.0.0
% Revision:  $Revision: 1.9 $
% Revised At:   $Date: 2002/06/27 14:13:20 $
% ===================================================================
% File used as storage place for all predicates which change as
% the world is run.
%
%
% Dec 13, 2035
% Douglas Miles
*/
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_props.pl
%:- if(((current_prolog_flag(xref,true),current_prolog_flag(pldoc_x,true));current_prolog_flag(autoload_logicmoo,true))).
:- module(mpred_props,
          [ add_mpred_prop_gleaned/2,
            add_mpred_prop_gleaned_4/4,
            assert_arity/2,
            bad_pred_relation_name0/2,
            bad_pred_relation_name1/2,
            (decl_mpred)/1,
            (decl_mpred)/2,
            arity_no_bc/2,
            (decl_mpred)/3,
            decl_mpred_0/2,
            decl_mpred_2/2,
            decl_mpred_3/3,
            decl_mpred_4/4,
            define_maybe_exact/2,
          (kb_dynamic)/1,
          (kb_dynamic)/2,
          (kb_dynamic)/3,
          (kb_dynamic)/4,
          (kb_dynamic)/5,
            decl_mpred_mfa/3,
            decl_mpred_pi/1,
          (decl_mpred_prolog)/1,
          (decl_mpred_prolog)/2,
          (decl_mpred_prolog)/3,
          (decl_mpred_prolog)/4,
          (decl_mpred_prolog)/5,
            ensure_arity/2,
            functor_check_univ/3,
            get_arity/3,
            get_mpred_prop/2,
            get_mpred_prop/3,
            glean_pred_props_maybe/1,
            glean_pred_props_maybe_some/1,
            good_pred_relation_name/2,
            listprolog/0,
            pred_type_test/2,
            pred_type_test/3,
            pred_type_test2/2,
            mpred_props_file/0
          ]).
%:- endif.

% :- use_module(logicmoo(util/logicmoo_util_preddefs)).
:- meta_predicate(kb_dynamic(:,+,+,+,+)).
:- meta_predicate(decl_mpred_prolog(:,+,+,+,+)).

:- module_transparent((
   (kb_dynamic)/1,
   (kb_dynamic)/2,
   (kb_dynamic)/3,
   (kb_dynamic)/4,
   (kb_dynamic)/5)).


:- include('mpred_header.pi').
/*
 :- meta_predicate decl_mpred_prolog(?,1).
 :- meta_predicate kb_dynamic(?,1).
 :- meta_predicate decl_mpred(?,1).
 :- meta_predicate decl_mpred_0(?,1).
 :- meta_predicate decl_mpred(+,+,+).
*/
:- meta_predicate decl_mpred_0(?,+).
:- meta_predicate kb_dynamic(?,+).
:- meta_predicate add_mpred_prop_gleaned(?,1).
:- meta_predicate add_mpred_prop_gleaned_4(?,*,*,1).
:- meta_predicate decl_mpred(*,?,+).
:- meta_predicate decl_mpred(?,+).
:- meta_predicate decl_mpred_prolog(?,+).
%: mpred_props:meta_argtypes/2, which is referenced by
% Warning: mpred_type_constraints:completeExtentEnumerable/1, which is referenced by
% Warning: mpred_type_constraints:gather_goals/2, which is referenced by

% ========================================
% mpred_isa/1/2/3
% ========================================



%= 	 	 

%% pred_type_test( ?H, ?F) is semidet.
%
% Predicate Type Test.
%
pred_type_test(H,F/_):-!,atom(F),THFA=..[H,F],clause(THFA,true).
pred_type_test(H,F):- \+ compound(F), !,atom(F),THFA=..[H,F/_],clause(THFA,true).
pred_type_test(H,P):-functor(P,F,A),!,THFA=..[H,F/A],HF=..[H,F],(clause(THFA,true);clause(HF,true)).


%= 	 	 

%% pred_type_test2( ?T, :TermP) is semidet.
%
% Predicate Type Test Extended Helper.
%
pred_type_test2(T,F):- \+ compound(F),!,arity_no_bc(F,A),!,pred_type_test(T,F,A).
pred_type_test2(T,F/A):-!,atom(F),arity_no_bc(F,A),!,pred_type_test(T,F,A).
pred_type_test2(T,P):-functor(P,F,A),!,pred_type_test(T,F,A).


%= 	 	 

%% pred_type_test( ?H, ?F, ?A) is semidet.
%
% Predicate Type Test.
%
pred_type_test(H,F,A):- THFA=..[H,F/A],HF=..[H,F],(clause(THFA,true);clause(HF,true)).




%= 	 	 

%% decl_mpred_pi( ?PI) is semidet.
%
% Declare Managed Predicate Predicate Indicator.
%
decl_mpred_pi(PI):-ignore((ground(PI),compound(PI),decl_mpred(PI))).
:- was_export(decl_mpred_mfa/3).

%= 	 	 

%% decl_mpred_mfa( ?M, ?FF, ?A) is semidet.
%
% Declare Managed Predicate Module-functor-arity_no_bc.
%
decl_mpred_mfa(_,M:F,A):-atom(M),!,decl_mpred_mfa(M,F,A).
decl_mpred_mfa(M,FF,A):-var(M),!,source_context_module(M),!,decl_mpred_mfa(M,FF,A).
decl_mpred_mfa(M,FF,A):-
   get_functor(FF,F,_),
   must_det_l([
     ignore((var(M),source_context_module(M),dmsg(decl_mpred_mfa(M,F,A)))),
     ignore((nonvar(M),asserta_if_new(mpred_isa(F,predicateConventionMt(M))))),
     assert_arity(F,A),  
     must_det(nonvar(M)),
    nop(dmsg(('@'((
     nop((is_static_predicate(M,F,A)->true; (M:dynamic(F/A),M:discontiguous(F/A)))), 
     nop(M:export(F/A)),
     nop(M:multifile(M:F/A))),M)))) ]).




% ========================================
% (decl_mpred_prolog)/1/2/3
% ========================================
:- op(0,fx,(decl_mpred_prolog)).

:- was_export((decl_mpred_prolog)/1).

:- meta_predicate(decl_mpred_prolog(?)).

%= 	 	 

%% decl_mpred_prolog( ?A) is semidet.
%
% Declare Managed Predicate Hybrid.
%

decl_mpred_prolog(A):- \+ compound(A),!,ain00(prologBuiltin(A)).
decl_mpred_prolog(A):-!,ain_expanded(prologBuiltin(A)).

% ain(love(isEach(a/1,b/2,c/1,d),mother)).
% ain(loves(isElement(a/1,b/2,c/1,d),mother)).
decl_mpred_prolog(M):- M =.. [isEach|List],!,must_maplist(decl_mpred_prolog,List).
decl_mpred_prolog(List):-is_list(List),!,must_maplist(decl_mpred_prolog,List).
decl_mpred_prolog(MPI):- must((with_pfa(m_fa_to_m_p_fa(decl_mpred_prolog),MPI))),!.


decl_mpred_prolog(F/A):- var(A),atom(F),
 call_u((must(current_smt(SM,CM)),!,
   forall(between(1,11,A),must((functor(PI,F,A),decl_mpred_prolog(CM,SM,PI,F,A)))))).
decl_mpred_prolog(P):- must(call_u(with_pi(P,decl_mpred_prolog))).


:- was_export((decl_mpred_prolog)/3).

%= 	 	 

%% decl_mpred_prolog( ?M, ?F, ?A) is semidet.
%
% Declare Managed Predicate Hybrid.
%
decl_mpred_prolog(M,F,A):-integer(A),!,must(functor(PI,F,A)),decl_mpred_prolog(M,PI,F/A).
decl_mpred_prolog(M,PI,FA):- must(decl_mpred_prolog(_,M,PI,FA)).


%= 	 	 

%% decl_mpred_prolog( ?F, ?A) is semidet.
%
% Declare Managed Predicate Hybrid.
%
decl_mpred_prolog(F,A):- integer(A),!,functor(FA,F,A),decl_mpred_prolog(FA).
decl_mpred_prolog(F,Other):- 
     decl_mpred(F,Other),     
     get_functor(F,F0),
     must(arity_no_bc(F0,A)),
     functor(F0A,F0,A),
     decl_mpred_prolog(F0A).

:- was_export((decl_mpred_prolog)/4).


%% decl_mpred_prolog( ?CM, ?M, ?PIN, :TermF) is semidet.
%
% Declare Managed Predicate Hybrid Inside Of Loop Checking.
%

decl_mpred_prolog(Any,M,PI,MFAIn):-
  must_det_l(( 
    pi_to_head_l(MFAIn,MFA),
    strip_module(MFA,_,FA),
    functor(FA,F,A),
    decl_mpred_prolog(Any,M,PI,F,A))).


decl_mpred_prolog(_:CM,M,PI,F,A):-var(A),!,
   forall(between(1,11,A),decl_mpred_prolog(CM,M,PI,F,A)),!.
decl_mpred_prolog(CM:M,baseKB,PI,F,A):- M\==baseKB, must(decl_mpred_prolog(CM:baseKB,baseKB,PI,F,A)).





decl_mpred_prolog(CM:baseKB,M,PI,F,A):- defaultAssertMt(Mt)-> M\==Mt,!,must(decl_mpred_prolog(CM:baseKB,Mt,PI,F,A)).
decl_mpred_prolog(_:CM,    M,PI,F,A):- atom(PI),A==0,get_arity(PI,F,A),
   \+(is_static_predicate(F/A)),!,
   must((forall((arity_no_bc(F,AA),AA\=0),
   (functor(PIA,F,AA),decl_mpred_prolog(CM,M,PIA,F,AA))))).
decl_mpred_prolog(_:CM,M,PI,F,A):-
   must_det_l((    
      ((var(CM),nonvar(M))->CM=M;true),
      ((var(PI),integer(A))->functor(PI,F,A);true),
      define_maybe_prolog(M,PI,F,A),
      ain_expanded(prologBuiltin(F)),
      (integer(A)->assert_arity(F,A);true))).

define_maybe_prolog(M,PI,F,_A):- predicate_property(M:PI,imported_from(system)),ain(prologBuiltin(F)).

define_maybe_prolog(M,PI,F,A):- 
    (\+ predicate_property(M:PI,_); predicate_property(M:PI,imported_from(OM))),
    ((OM=system;current_module(OM)),predicate_property(OM:PI,_),\+ predicate_property(OM:PI,imported_from(_))),!,
    decl_mpred_prolog(M,OM,PI,F,A).

define_maybe_prolog(M,PI,F,A):-
      assert_arity(F,A),
   %sanity(\+ mtCycL(M)),
   %sanity(\+ M == baseKB),
      ain(predicateConventionMt(F,M)),
      ain(~prologHybrid(F)),
      ain(prologBuiltin(F)),
      (\+ is_static_predicate(M:PI)->ain(prologDynamic(F));true),
      ain(mpred_isa(PI,predCanHaveSingletons)),!.


:- op(1120,fx,(decl_mpred_prolog)).

%prologHybrid(X,Y):-dtrace(prologHybrid(X,Y)).
%:- was_dynamic(prologHybrid(_,_)).
%:- lock_predicate(prologHybrid(_,_)).

% ========================================
% mpred_props database
% ========================================


% ========================================
% (kb_dynamic)/1/2/3
% ========================================
:- op(0,fx,(kb_dynamic)).

:- was_export((kb_dynamic)/1).

:- meta_predicate(kb_dynamic(?)).

%= 	 	 

%% kb_dynamic( ?A) is semidet.
%
% Declare Managed Predicate Hybrid.
%

% kb_dynamic(A):- \+(compound(A)),!,ain00(prologHybrid(A)).
% ain_expanded(love(isEach(a/1,b/2,c/1,d),mother)).
% ain_expanded(loves(isElement(a/1,b/2,c/1,d),mother)).
kb_dynamic(M):- var(M),!,trace_or_throw(var_kb_dynamic(M)).
kb_dynamic(M):- M =.. [isEach|List],!,must_maplist(kb_dynamic,List).
kb_dynamic(F/A):- var(F),!,trace_or_throw(var_kb_dynamic(F/A)).
kb_dynamic([H|List]):- is_list(List),!,kb_dynamic(H),must_maplist(kb_dynamic,List).

% kb_dynamic(MPI):- must(decl_shared(MPI)),must(dynamic(MPI)),!.
kb_dynamic(MPI):- must(decl_shared(MPI)),must((with_pfa(m_fa_to_m_p_fa(kb_dynamic),MPI))),!.

kb_dynamic(F/A):- var(A),atom(F),!,
 must(call_u((must(current_smt(SM,CM)),!,
   forall(between(1,11,A),must((functor(PI,F,A),kb_dynamic(CM,SM,PI,F,A))))))),!.
kb_dynamic(P):- must(call_u(with_pi(P,kb_dynamic))).


:- was_export((kb_dynamic)/3).

%= 	 	 

%% kb_dynamic( ?M, ?F, ?A) is semidet.
%
% Declare Managed Predicate Hybrid.
%
kb_dynamic(M,F,A):-integer(A),!,must(functor(PI,F,A)),kb_dynamic(M,PI,F/A).
kb_dynamic(M,PI,FA):- prolog_load_context(module,CM),must(kb_dynamic(CM,M,PI,FA)).


%= 	 	 

%% kb_dynamic( ?F, ?A) is semidet.
%
% Declare Managed Predicate Hybrid.
%
kb_dynamic(F,A):- integer(A),!,functor(FA,F,A),kb_dynamic(FA).
kb_dynamic(F,Other):- 
     decl_mpred(F,Other),     
     get_functor(F,F0),
     must(arity_no_bc(F0,A)),
     functor(F0A,F0,A),
     kb_dynamic(F0A).

:- was_export((kb_dynamic)/4).


no_need_to_import(baseKB).
no_need_to_import(lmcache).
no_need_to_import(t_l).
no_need_to_import(system).
no_need_to_import(baseKB).


%% kb_dynamic( ?CM, ?M, ?PIN, :TermF) is semidet.
%
% Declare Managed Predicate Hybrid Inside Of Loop Checking.
%
kb_dynamic(Any,M,PI,MFAIn):-
  must_det_l(( 
    pi_to_head_l(MFAIn,MFA),
    strip_module(MFA,_,FA),
    functor(FA,F,A),
    kb_dynamic(Any,M,PI,F,A))).


kb_dynamic(_:CM,M,PI,F,A):-var(A),!,
   forall(between(1,11,A),kb_dynamic(CM,M,PI,F,A)),!.

kb_dynamic(CM:OM,M,PI,F,A):-M==OM,kb_dynamic(CM,M,PI,F,A).

kb_dynamic(CM:Imp,M,PI,F,A):-M==CM,
   kb_dynamic(CM,M,PI,F,A),
   (CM==baseKB->true;((   CM:export(CM:F/A),dmsg(Imp:import(CM:F/A)), Imp:import(CM:F/A)))).

% kb_dynamic(CM,M,PI,F,A):- dmsg(kb_dynamic(CM,M,PI,F,A)),fail.

% kb_dynamic(CM:M,baseKB,PI,F,A):- M\==baseKB, must(kb_dynamic(CM:baseKB,baseKB,PI,F,A)).

kb_dynamic(CM:baseKB,M,PI,F,A):- M==abox, defaultAssertMt(Mt)-> M\==Mt,!,must(kb_dynamic(CM:baseKB,Mt,PI,F,A)).

kb_dynamic(CM:baseKB,M,PI,F,A):- defaultAssertMt(Mt)-> M\==Mt,!,must(kb_dynamic(CM:baseKB,Mt,PI,F,A)).
kb_dynamic(_:CM,    M,PI,F,A):- atom(PI),A==0,get_arity(PI,F,A),
   \+(is_static_predicate(F/A)),!,
   must((forall((arity_no_bc(F,AA),AA\=0),
   (functor(PIA,F,AA),kb_dynamic(CM,M,PIA,F,AA))))).
kb_dynamic(_:CM,M,PI,F,A):-
   must_det_l((    
      ((var(CM),nonvar(M))->CM=M;true),
      ((var(PI),integer(A))->functor(PI,F,A);true),
      define_maybe_exact(M,PI),
      (integer(A)->assert_arity(F,A);true))),!.

define_maybe_exact(system,PI):- !,must((defaultAssertMt(Mt),define_maybe_exact(Mt,PI))),!.
define_maybe_exact(M,PI):- % a(mtExact,M),!, 
   must_det_l((    functor(PI,F,A),
     M:multifile(M:F/A),
     once((M==baseKB->true;ain(baseKB:predicateConventionMt(F,M)))),
     asserta_if_new(baseKB:wrap_shared(F,A,ereq)),
     decl_shared(M:PI),     
     sanity(\+is_static_predicate(M:PI)),
     maybe_define_if_not_static(M,PI))),!.
define_maybe_exact(_,PI):-
     maybe_define_if_not_static(baseKB,PI).

maybe_define_if_not_static(M,PI):- 
  must_det_l((              
              functor_h(PI,F,A),
              asserta_if_new(baseKB:wrap_shared(F,A,ereq)),
              M:multifile(M:F/A),
              M:public(M:F/A),
              on_f_throw( (M:F/A)\== (baseKB:loaded_external_kbs/1)),
              M:discontiguous(M:F/A),
              M:module_transparent(M:F/A),      
      (is_static_predicate(M:PI) -> true ;
       (predicate_property(M:PI,dynamic) -> true ; icatch(M:dynamic(M:PI)))))),!.


:- op(1120,fx,(kb_dynamic)).

%prologHybrid(X,Y):-dtrace(prologHybrid(X,Y)).
%:- was_dynamic(prologHybrid(_,_)).
%:- lock_predicate(prologHybrid(_,_)).

% ========================================
% mpred_props database
% ========================================

/*



% mpred_isa(F,prologDynamic):- \+ (mpred_isa(F,prologHybrid)),(F=ttPredType;(current_predicate(F/1);not(t(F,tCol)))).
mpred_isa(G,predProxyAssert(ain)):- atom(G),prologMacroHead(G).
mpred_isa(G,predProxyQuery(ireq)):- atom(G),prologMacroHead(G).
mpred_isa(G,predProxyRetract(del)):- atom(G),prologMacroHead(G).
*/




%= 	 	 

%% get_mpred_prop( ?F, ?A, ?P) is semidet.
%
% Get Managed Predicate Prop.
%
get_mpred_prop(F,_A,P):-get_mpred_prop(F,P).

%= 	 	 

%% get_mpred_prop( ?F, ?P) is semidet.
%
% Get Managed Predicate Prop.
%
get_mpred_prop(F,P):- mreq(mpred_isa(F,P)).

:- was_export(listprolog/0).

%= 	 	 

%% listprolog is semidet.
%
% Listprolog.
%
listprolog:-listing(mpred_isa(_,prologDynamic)).



%= 	 	 

%% get_arity( :TermTerm, ?F, ?A) is semidet.
%
% Get Arity.
%
get_arity(Term,F,A):- atom(Term),F=Term,!,ensure_arity(F,A).
get_arity(F/A,F,A):- atom(F),ensure_arity(F,A),!,(A>0).
get_arity(M:FA,F,A):-atom(M),!,get_arity(FA,F,A).
get_arity(FA,F,A):- get_functor(FA,F,A),must(A>0).

% arity_no_bc(F,A):- call_u(arity(F,A)).
arity_no_bc(F,A):- clause_b(arity(F,A)).
%= 	 	 

%% ensure_arity( ?VALUE1, ?VALUE2) is semidet.
%
% Ensure Arity.
%
ensure_arity(F,A):- one_must(arity_no_bc(F,A),one_must((current_predicate(F/A),
    (A>0),assert_arity(F,A)),(ground(F:A),(A>0),assert_arity(F,A)))),!.


%= 	 	 

%% assert_arity( ?F, :PRED2A) is semidet.
%
% Assert Arity.
%

assert_arity(F,A):- sanity(\+ ((bad_arity(F,A), trace_or_throw(assert_arity(F,A))))), arity_no_bc(F,A),!.
assert_arity(F,A):- arity_no_bc(F,AA), A\=AA,dmsg(assert_additional_arity(F,AA->A)),!,ain_fast(arity(F,A)).
assert_arity(F,A):- ain_fast(arity(F,A)),!.

bad_arity(F,_):- \+ atom(F).
bad_arity(_,A):- \+ integer(A).
bad_arity(typeProps,0).
bad_arity(argsIsa,2).
bad_arity(prologDynamic,2).
bad_arity(F,A):- \+ good_pred_relation_name(F,A).


%= 	 	 

%% good_pred_relation_name( ?F, ?A) is semidet.
%
% Good Predicate Relation Name.
%
good_pred_relation_name(F,A):- \+ bad_pred_relation_name0(F,A).


%= 	 	 

%% bad_pred_relation_name0( ?V, ?VALUE2) is semidet.
%
% Bad Predicate Relation Name Primary Helper.
%
bad_pred_relation_name0(V,_):- \+ atom(V),!.
bad_pred_relation_name0('[]',_).
bad_pred_relation_name0('',_).
bad_pred_relation_name0('!',_).
bad_pred_relation_name0('{}',_).
bad_pred_relation_name0(',',_).
bad_pred_relation_name0('[|]',_).

%= 	 	 

%% bad_pred_relation_name1( ?X, ?Y) is semidet.
%
% Bad Predicate Relation Name Secondary Helper.
%
bad_pred_relation_name1(X,Y):-bad_pred_relation_name0(X,Y).
bad_pred_relation_name1(F,A):-must_det((atom_codes(F,[C|_]),to_upper(C,U))),!, U == C, A>1.
bad_pred_relation_name1(F,A):-arity_no_bc(F,AO), A \= AO.

% :-at_start(writeq("Seen Mpred_props at start!\n")),!.


% ========================================
% decl_mpred database
% ========================================

:- was_export(((decl_mpred)/1)).

:- meta_predicate(decl_mpred(?)).

%= 	 	 

%% decl_mpred( ?M) is semidet.
%
% Declare Managed Predicate.
%
decl_mpred((A,B)):-decl_mpred(A),decl_mpred(B).
decl_mpred(M):-!,kb_dynamic(M).
decl_mpred(M):-loop_check(with_pi(M,decl_mpred_4),true).


%= 	 	 

%% decl_mpred_3( ?VALUE1, ?ARGS, :TermARG3) is semidet.
%
% Declare Managed Predicate Helper Number 4..
%
decl_mpred_3(user,prologSingleValued(ARGS),prologSingleValued/1):- compound(ARGS),get_functor(ARGS,F,A),!, 
   ain(arity(F,A)),ain(prologSingleValued(F)),ain_expanded(meta_argtypes(ARGS)),!.
decl_mpred_3(_,F,F/0):-!,assert_hasInstance(tPred,F).
decl_mpred_3(M,PI,F/A):-
   decl_mpred(F,A),
   ignore((ground(PI),compound(PI),call(call,GG=meta_argtypes(PI)),decl_mpred(F,GG))),
   nop(ain(predicateConventionMt(F,M))).

:- was_export((decl_mpred)/2).

%= 	 	 

%% decl_mpred( ?C, ?A) is semidet.
%
% Declare Managed Predicate.
%
decl_mpred(C,A):- integer(A),!,functor(FA,C,A),decl_mpred(FA).
decl_mpred(C,More):- ignore(loop_check(decl_mpred_0(C,More),true)).


%= 	 	 

%% decl_mpred_0( ?C, :TermMore) is semidet.
%
% Declare Managed Predicate  Primary Helper.
%
decl_mpred_0(C,More):- (var(C);var(More)), trace_or_throw(var_decl_mpred(C,More)).
decl_mpred_0(F/A,More):-atom(F),integer(A),!,assert_arity(F,A),decl_mpred(F,More),!.
decl_mpred_0(M:FA,More):-atom(M),!,decl_mpred_0(FA,More),decl_mpred_0(FA,predicateConventionMt(M)).
decl_mpred_0(F,A):-atom(F),number(A),!,assert_arity(F,A).
decl_mpred_0(F,tPred):-!,assert_hasInstance(tPred,F).
decl_mpred_0(C,More):-string(C),!,dmsg(trace_or_throw(var_string_decl_mpred(C,More))).
decl_mpred_0(mudDescription, predProxyRetract):- trace_or_throw(decl_mpred_0(mudDescription, predProxyRetract)).
decl_mpred_0(_,meta_argtypes):-!.
decl_mpred_0(F,GG):- call(call,GG=meta_argtypes(ArgTypes)),!,decl_mpred_2(F,meta_argtypes(ArgTypes)).
decl_mpred_0(C,More):-compound(C),C=..[F,Arg1|PROPS],ttPredType(F),!,ground(Arg1),call(call,decl_mpred(Arg1,[F,PROPS,More])).
decl_mpred_0(C,More):-compound(C),!,functor(C,F,A),assert_arity(F,A),decl_mpred_0(F,More),!,ignore((ground(C),
  call(call,GG=meta_argtypes(C)),decl_mpred(F,GG))),!.
decl_mpred_0(_,[]):-!.
decl_mpred_0(F,[Prop|Types]):-!,decl_mpred_0(F,Prop),!,decl_mpred_0(F,Types),!.

decl_mpred_0(F,T):-doall(( decl_mpred_2(F,T) )).



%= 	 	 

%% decl_mpred_2( ?F, ?A) is semidet.
%
% Declare Managed Predicate  Extended Helper.
%
decl_mpred_2(F,meta_argtypes(FARGS)):- functor(FARGS,_,A),decl_mpred(F,A),fail.
decl_mpred_2(_,meta_argtypes(FARGS)):- functor(FARGS,_,A),arg(A,FARGS,Arg),var(Arg),!.

% decl_mpred_2(F,prologHybrid):- kb_dynamic(F).
decl_mpred_2(F,cycPlus2(A)):- ensure_universal_stub_plus_mt_why(F,A).

decl_mpred_2(F,A):-once(baseKB:mpred_provide_write_attributes(F,A)).
decl_mpred_2(F,Prop):-ain_expanded(mpred_isa(F,Prop)).


%= 	 	 

%% decl_mpred( ?Mt, ?F, ?A) is semidet.
%
% Declare Managed Predicate.
%
decl_mpred(Mt,F,A):-decl_mpred(F,A),ignore((nonvar(Mt),     
      (\+ baseKB:mtCycL(Mt)),
      (\+ Mt == baseKB), 
      decl_mpred(F,predicateConventionMt(Mt)))).

%= 	 	 

%% decl_mpred_4( ?CM, ?M, ?PI, :TermF) is semidet.
%
% Declare Managed Predicate Helper Number 4..
%
decl_mpred_4(_CM,M,PI,FA):- decl_mpred_3(M,PI,FA).



%= 	 	 

%% functor_check_univ( ?G1, ?F, ?List) is semidet.
%
% Functor Check Univ.
%
functor_check_univ(M:G1,F,List):-atom(M),member(M,[dbase,user]),!,functor_check_univ(G1,F,List),!.
functor_check_univ(G1,F,List):-must_det(compound(G1)),must_det(G1 \= _:_),must_det(G1 \= _/_),G1=..[F|List],!.

:- was_export(glean_pred_props_maybe/1).

%= 	 	 

%% glean_pred_props_maybe( ?G) is semidet.
%
% Glean Predicate Props Maybe.
%
glean_pred_props_maybe(_:G):-!,compound(G),w_tl(infConfidence(vWeak),forall(glean_pred_props_maybe_some(G),true)).
glean_pred_props_maybe(G):-compound(G),w_tl(infConfidence(vWeak),forall(glean_pred_props_maybe_some(G),true)).


%= 	 	 

%% glean_pred_props_maybe_some( ?VALUE1) is semidet.
%
% Glean Predicate Props Maybe Some.
%
glean_pred_props_maybe_some(G):-compound(G),G=..[F,Arg1|RGS],ttPredType(F),call(call,add_mpred_prop_gleaned(Arg1,[F|RGS])).
% glean_pred_props_maybe_some(G):-arg(_,G,Arg1),compound(Arg1),arg(_,Arg1,Col),t(tCol,Col),w_tl(infConfidence(vWeak),assert_predArgTypes(Arg1)).


%= 	 	 

%% add_mpred_prop_gleaned( ?Arg1, ?FRGS) is semidet.
%
% Add Managed Predicate Prop Gleaned.
%
add_mpred_prop_gleaned(M:Arg1,FRGS):-atom(M),!,add_mpred_prop_gleaned(Arg1,FRGS).
add_mpred_prop_gleaned(Arg1,FRGS):-functor_check_univ(Arg1,F,ARGSISA),add_mpred_prop_gleaned_4(Arg1,F,ARGSISA,FRGS).

%= 	 	 

%% add_mpred_prop_gleaned_4( ?Arg1, ?F, ?ARG, ?FRGS) is semidet.
%
% Add Managed Predicate Prop Gleaned Helper Number 4..
%
add_mpred_prop_gleaned_4(Arg1,_F,[ARG|_],FRGS):-nonvar(ARG),!,ain_expanded(meta_argtypes(Arg1)),decl_mpred(Arg1,FRGS).
add_mpred_prop_gleaned_4(Arg1,_F,_,FRGS):-decl_mpred(Arg1,FRGS).



% user:term_expansion(G,_):- current_predicate(logicmoo_bugger_loaded/0),\+ t_l:disable_px, not(t_l:into_form_code),hotrace((once(glean_pred_props_maybe(G)),fail)).

mpred_props_file.
