/*  
% ===================================================================
% File 'dbase_c_term_expansion'
% Purpose: Emulation of OpenCyc for SWI-Prolog
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'interface' 1.0.0
% Revision:  $Revision: 1.9 $
% Revised At:   $Date: 2002/06/27 14:13:20 $
% ===================================================================
% File used as storage place for all predicates which change as
% the world is run.
%
% props(Obj,height(ObjHt))  == holds(height,Obj,ObjHt) == rdf(Obj,height,ObjHt) == height(Obj,ObjHt)
% padd(Obj,height(ObjHt))  == padd(height,Obj,ObjHt,...) == add(QueryForm)
% kretract[all](Obj,height(ObjHt))  == kretract[all](Obj,height,ObjHt) == pretract[all](height,Obj,ObjHt) == del[all](QueryForm)
% keraseall(AnyTerm).
%
% when deciding the setting for a pred in file foof.pl
%
%  foom:foo(1):-bar(2).
%
%      we search in this order:  SOURCE:LOADTYPE:PRED
%
% SOURCETYPE
%                  source_file('/dir/foof.pl')
%                  source_module(foom)
%                  source_user(ax)
%                  source_filetype(pl)
%                  source_caller(user)   % module it's being loaded for
%                  (missing)*
% LOADTYPE
%                  consult
%                  assert
%                  (missing)*
%                  
% CLAUSETYPE
%                  rule
%                  fact
%                  directive
%                  (missing)*
%                  
% PRED INDICATOR
%                  
%                  foo(int),
%                  foo/1
%                  foo,
%                  (:-)/2  % neck used
%                  (missing)*
%
%
%
% clause types: (:-)/1, (:-)/2, (=>)/1,  (=>)/2,  (==>)/1,  (==>)/2, (<-)/1,  (<-)/2, (<==>)/2, fact/1
%
*/
%:- if(((current_prolog_flag(xref,true),current_prolog_flag(pldoc_x,true));current_prolog_flag(autoload_logicmoo,true))).
:- module(mpred_expansion,
          [ a/2,
            acceptable_xform/2,
            additiveOp/1,
            alt_calls/1,
            any_op_to_call_op/2,
            as_is_term/1,as_is_term/1,
            as_list/2,
            cheaply_u/1,
            cheaply_u/1,
            compare_op/4,
            comparitiveOp/1,
            compound_all_open/1,
            conjoin_l/3,
            try_expand_head/3,
            db_expand_0/3,
            db_expand_chain/3,
            db_expand_final/3,
            db_expand_maplist/5,
            db_expand_up/4,
            db_op_sentence/4,
            db_op_simpler/3,
            db_quf/4,
            db_quf_l/5,
            db_quf_l_0/5,
            default_te/3,
            demodulize/3,
            remodulize/3,
            replaced_module/3,
            do_expand_args/3,
            ain_expanded/1,
            do_expand_args_l/3,
            do_expand_args_pa/4,
            ex_argIsa/3,
            exact_args/1,
            exact_args0/1,
            expand_isEach_or_fail/2,
            expand_goal_correct_argIsa/2,
            expand_props/3,
            expand_props/4,
            expanded_different/2,
            expanded_different_1/2,
            expanded_different_ic/2,
            %expands_on/2,
            fix_negations/2,
            fixed_negations/2,
            foreach_arg/7,
            from_univ/4,
            fully_expand/2,            
            fully_expand/3,
            fully_expand_clause/3,
            fully_expand_goal/3,
            fully_expand_head/3,
            fully_expand_head_throw_if_loop/3,         
            fully_expand_now/3,
            fully_expand_warn/3,
            functor_declares_collectiontype/2,
            functor_declares_instance/2,
            functor_declares_instance_0/2,
            holds_args/2,
            %if_expands_on/3,
            infix_op/2,
            instTypePropsToType/2,
            into_functor_form/3,
            into_functor_form/5,
            into_mpred_form/2,
            into_mpred_form6/6,
            into_mpred_form_ilc/2,
            is_arity_pred/1,
            is_meta_functor/3,
            is_pred_declarer/1,
            is_relation_type/1,
            is_stripped_module/1,
            is_unit/1,
			is_unit_like/1,
            is_unit_functor/1,
            listToE/2,
            map_f/2,
            mpred_expand_rule/2,
            must_expand/1,
            must_remodulize/3,
            recommify/2,
            recommify/3,
            reduce_clause/3,
            reduce_clause_from_fwd/3,
            same_terms/2,
            show_doall/1,
            simply_functors/3,
            to_reduced_hb/4,
            transform_functor_holds/5,
            transform_holds/3,
            transform_holds_3/3,
            transitive_lc_nr/3,
            translate_args/9,
            translateListOps/8,
            translateOneArg/8,
            was_mpred_isa/3,
          mpred_expansion_file/0,
          expand_kif_string/2,
         temp_comp/4,
         get_ruleRewrite/2,
         expand_kif_string_or_fail/3,
         to_predicate_isas/2,
         append_as_first_arg/3,
         try_expand_head/3,
         is_elist_functor/1
          ]).
%:- endif.

:- meta_predicate 
   % mpred_expansion
   cheaply_u(+),
   ain_expanded(+),
   cheaply_u(+),
   db_expand_maplist(2,*,*,*,*),
   % mpred_expansion
   transitive_lc_nr(2,*,*),
   simply_functors(2,*,*).
          
:- include('mpred_header.pi').

:- use_module(library(apply)).
%= :- shared_multifile(was_chain_rule/1).
%= :- shared_multifile(baseKB:ptReformulatorDirectivePredicate/1).
%= :- shared_multifile(props/2).

:- dynamic(baseKB:col_as_isa/1).
:- dynamic(baseKB:col_as_unary/1).

:- style_check(+singleton).

%= 	 	 

%% default_te( ?IF, ?VAR, ?VAL) is semidet.
%
% Default Te.
%
default_te(IF,VAR,VAL):-assertz(te_setting(IF,VAR,VAL)).

:- default_te([source_filetype(pl) ],use_te,file_prolog).
:- default_te([source_filetype(pfc) ],use_te,file_pfc).
:- default_te([source_filetype(console) ],use_te,file_prolog).

:- default_te(file_prolog,proccess_directive, proccess_directive).
:- default_te(file_prolog,compile_clause, compile_clause).
:- default_te(file_prolog,rule_neck, (head :- body)).
:- default_te(file_prolog,fact_neck, (head :- true)).

:- default_te(file_pfc, compile_clause, ain).
:- default_te(file_pfc, expand_clause, fully_expand_clause).
:- default_te(file_pfc, proccess_directive, proccess_directive).
:- default_te(file_pfc, fact_neck, (clause <- true)).
:- default_te(file_pfc, rule_neck, (head :- body)).

:- default_te(file_syspreds,isa_detector, always_fail(i,c)).
:- default_te(file_syspreds,isa_holder, c(i)).
:- default_te(file_syspreds,isa_varholder, t(c,i)).  % was isa(i,c).
:- default_te(file_syspreds,pred_holder, head).  % was isa(i,c).
:- default_te(file_syspreds,pred_varholder, newhead=..[t,pred|args]).
:- default_te(file_syspreds,proccess_directive, proccess_directive).
:- default_te(file_syspreds,compile_clause, compile_clause).
:- default_te(file_syspreds,rule_neck, (head :- body)).
:- default_te(file_syspreds,fact_neck, (clause :- true)).
:- default_te(file_syspreds, expand_clause, (=)).

:- default_te(file_syspreds:pred(*), neck_override, (cwc)).
:- default_te(file_pfc:pred(*), neck_override, (hybrid)).
:- default_te(file_prolog:pred(*), neck_override, (hybrid)).

:- default_te((:-)/1, compile_clause, proccess_directive).
:- default_te((:-)/2, rule_neck, clause).
:- default_te((=>),use_te, file_pfc).
:- default_te((<==>),use_te, file_pfc).
:- default_te((<-),use_te, file_pfc).

/*
% :- directive:  process_directive, call
% fact:  fwc(pfc), bwc(pfc), *cwc(prolog), bwc(pttp), implies(kif), other
% :- rule:  fwc(pfc), bwc(pfc), *cwc(prolog), bwc(pttp), implies(kif), other
% <- rule:   fwc(pfc), *bwc(pfc), cwc(prolog), bwc(pttp), implies(kif), other
% <= rule:   *fwc(pfc), bwc(pfc), cwc(prolog), bwc(pttp), implies(kif), other
% <- fact:   fwc(pfc), *bwc(pfc), cwc(prolog), bwc(pttp), implies(kif), other
% => fact:   *fwc(pfc), bwc(pfc), cwc(prolog), bwc(pttp), implies(kif), other
% loading:  compile_clause, process_directive, assertz, 
% head types: code, *hybrid, functor(outer), holds(outer)
% body types: code, *hybrid, functor(outer), holds(outer)
% isa holder:   isa(i,c), t(c,i),  *c(i).
% isa holder is_ftVar c:   isa(i,c), *t(c,i).
% varpred_head:  *t(P,A,B).
% varpred_body:  *t(P,A,B).
% body types: code, *hybrid, functor(outer), holds(outer)

Interestingly there are three main components I have finally admit to needing despite the fact that using Prolog was to provide these exact components.  
First of all a defaulting system using to shadow (hidden) behind assertions
Axiomatic semantics define the meaning of a command in a program by describing its effect on assertions about the program state.
The assertions are logical statements - predicates with variables, where the variables define the state of the program.
Predicate transformer semantics to combine programming concepts in a compact way, before logic is realized.   
This simplicity makes proving the correctness of programs easier, using Hoare logic.

Axiomatic semantics
Writing in Prolog is actually really easy for a MUD is when X is chosen

%
% Dec 13, 2035
% Douglas Miles
*/



% ============================================
% inital a/2 database
% ============================================

% baseKB:hasInstance_dyn(W,SS):-nonvar(W),nonvar(SS),SS=isKappaFn(_,S),nonvar(S),!.


/*
disabled a(T,I):- not(current_predicate(deduce_M/1)),!,baseKB:hasInstance_dyn(T,I).
disabled a(T,I):- !, (mudIsa_motel(I,T) *-> true ; (((atom(I),must(not(baseKB:hasInstance_dyn(T,I)))),fail))).
disabled a(T,I):- rdf_x(I,rdf:type,T).
*/

%% a( ?C, ?I) is nondet.
%
% A.
%
:- meta_predicate a(+,?).
% WANT (but will loop) a(C,I):- !, quietly((atom(C),G=..[C,I], no_repeats_old(call_u(G)))).
a(C,I):- quietly((atom(C),G=..[C,I], no_repeats_old(lookup_u(G)))).


%=  :- was_export(alt_calls/1).

%= 	 	 

%% alt_calls( +Op) is semidet.
%
% Alt Calls.
%
alt_calls(call).
alt_calls(call_u).
alt_calls(clause_u).
alt_calls(clause_asserted_i).
alt_calls(t).
alt_calls(is_entailed_u).
alt_calls(call_u).
alt_calls(ireq).



:- meta_predicate compare_op(*,2,?,?).


:- meta_predicate show_doall(0).

%= 	 	 

%% show_doall( :GoalCall) is semidet.
%
% Show Doall.
%
show_doall(Call):- doall(show_call(why,Call)).


/*
Name               Meaning                            

speed              speed of the runtime code
safety             run-time error checking            

correct         run-time error correction            

compilation-speed  speed of the compilation process   
debug              ease of debugging     


cheaply_u(G):- quickly(quietly(Goal)).

*/

cheaply_u(argsQuoted(G)):- !,lookup_u(argsQuoted(G)).
cheaply_u(call(ereq,G)):- must(callable(G)),!,cheaply_u(G).
cheaply_u(G):- quietly(call_u(G)).
%cheaply_u(G):- need_speed,!, (ground(G)->(quietly(baseKB:G),!);quietly(lookup_u(G))).
%cheaply_u(G):- loop_check(cheaply_u(G),loop_check_term(cheaply_u(G),ilc2(G),fail)).
%cheaply_u(G):- predicate_property(G,number_of_rules(N)),N=0,!,lookup_u(G).
%cheaply_u(G):- strip_module(G,_,C),G\==C,!,cheaply_u(C).


%= 	 	 

%% is_pred_declarer( ?P) is semidet.
%
% If Is A Predicate Declarer.
%
is_pred_declarer(P):-functor_declares_instance(P,tPred).

%= 	 	 

%% is_relation_type( ?P) is semidet.
%
% If Is A Relation Type.
%
is_relation_type(tRelation).
is_relation_type(tFunction).
is_relation_type(tPred).
is_relation_type(P):-is_pred_declarer(P).


%= 	 	 

%% functor_declares_instance( ?F, ?C) is semidet.
%
% Functor Declares Instance.
%
functor_declares_instance(F,C):-functor_declares_instance_0(F,C0),!,C=C0. % ,F\=C.

%= 	 	 

%% functor_declares_instance_0( ?P, ?P) is semidet.
%
% functor declares instance  Primary Helper.
%
functor_declares_instance_0(decl_mpred,tPred).
functor_declares_instance_0(kb_dynamic,prologHybrid).
functor_declares_instance_0(shared_multifile,prologHybrid).
functor_declares_instance_0(decl_mpred_prolog,prologBuiltin).
functor_declares_instance_0(decl_mpred_prolog,prologDynamic).

% functor_declares_instance_0(prologSideEffects,tPred).
% functor_declares_instance_0(tPred,tPred).
functor_declares_instance_0(meta_argtypes,tRelation).
functor_declares_instance_0(prologMacroHead,tRelation).
functor_declares_instance_0(tFunction,tFunction).
functor_declares_instance_0(P,tPred):- arg(_,s(tPred,prologMultiValued,mpred_isa,mpred_isa,
       prologOrdered,prologNegByFailure,prologHybrid,prologPTTP,prologSideEffects,
       predCanHaveSingletons,prologBuiltin,prologKIF,prologDynamic,prologMacroHead,prologListValued,prologSingleValued),P).
functor_declares_instance_0(P,tPred):-isa_from_morphology(P,ttPredType).
functor_declares_instance_0(P,tFunction):-isa_from_morphology(P,ftFunctional).
functor_declares_instance_0(P,tFunction):-isa_from_morphology(P,O)->O=tFunction.

functor_declares_instance_0(P,tCol):- arg(_,s(tCol,tSpec,ttExpressionType),P).
functor_declares_instance_0(P,ttModule):- arg(_,s(tCol,tModule),P).
%functor_declares_instance_0(P,tPred):-isa_asserted(P,ttPredType),!.
%functor_declares_instance_0(P,tCol):-isa_asserted(P,functorDeclares),\+functor_declares_instance_0(P,tPred).

functor_declares_instance_0(P,P):- cheaply_u(functorDeclares(P)). % arity(P,1),\+((arity(P,N),N>1)).


%= 	 	 

%% functor_declares_collectiontype( +Op, ?VALUE2) is semidet.
%
% Functor Declares Collectiontype.
%
functor_declares_collectiontype(typeProps,ttTemporalType).


%= 	 	 

%% instTypePropsToType( +Op, ?VALUE2) is semidet.
%
% Inst Type Props Converted To Type.
%
instTypePropsToType(instTypeProps,ttSpatialType222).


%= 	 	 

%% reduce_clause( ?Op, ?C, ?HB) is semidet.
%
% Reduce Clause.
%
reduce_clause(Op,C,HB):-must(nonvar(C)),quietly_must(demodulize(Op,C,CB)),CB\=@=C,!,reduce_clause(Op,CB,HB).
reduce_clause(Op,clause(C, B),HB):-!,reduce_clause(Op,(C :- B),HB).
reduce_clause(Op,(C:- B),HB):- is_true(B),!,reduce_clause(Op,C,HB).
reduce_clause(_,C,C).


%% demodulize( ?Op, ?H, ?HH) is semidet.
%
% Demodulize.
%


demodulize(_Op,H,HH):- as_is_term(H),!,HH=H.
demodulize(Op,H,HHH):-once(strip_module(H,M,HH)),H\==HH,old_is_stripped_module(M),!,demodulize(Op,HH,HHH).
demodulize(Op,H,HH):-is_list(H),!,must_maplist(demodulize(Op),H,HH),!.
demodulize(Op,H,HH):-is_ftCompound(H),H=..[F|HL],!,must_maplist(demodulize(Op),HL,HHL),HH=..[F|HHL],!.
demodulize(_ ,HB,HB).

old_is_stripped_module(user).
old_is_stripped_module(baseKB).
%= 	 	 

%% to_reduced_hb( ?Op, ?HB, ?HH, ?BB) is semidet.
%
% Converted To Reduced Head+body.
%
to_reduced_hb(Op,HB,HH,BB):-reduce_clause(Op,HB,HHBB),expand_to_hb(HHBB,HH,BB).


/*
dbase_head_expansion(_,V,V ):-is_ftVar(V),!.
dbase_head_expansion(Op,H,GG):-correct_negations(Op,H,GG),!.
dbase_head_expansion(_,V,V).
*/

% ================================================
% db_expand_maplist/3
% ================================================


%= 	 	 

%% any_op_to_call_op( +Op, ?VALUE2) is semidet.
%
% Any Oper. Converted To Call Oper..
%
any_op_to_call_op(_,call(conjecture)).


%= 	 	 

%% db_expand_maplist( :PRED2FE, ?List, ?T, ?G, ?O) is semidet.
%
% Database Expand Maplist.
%
db_expand_maplist(FE,[E],E,G,O):- !,call(FE,G,O).
db_expand_maplist(FE,[E|List],T,G,O):- copy_term(T+G,CT+CG),E=CT,!,call(FE,CG,O1),db_expand_maplist(FE,List,T,G,O2),conjoin_l(O1,O2,O).
db_expand_maplist(FE,List,T,G,O):-bagof(M, (member(T,List),call(FE,G,M)), ML),list_to_conjuncts(ML,O).


% ================================================
% fully_expand/3
%   SIMPLISTIC REWRITE (this is not the PRECANONICALIZER)
% ================================================


%= 	 	 

%% must_expand( :TermG) is semidet.
%
% Must Be Successfull Expand.
%
must_expand(/*to_exp*/(_)).
must_expand(props(_,_)).
must_expand(iprops(_,_)).
must_expand(upprop(_,_)).
must_expand(typeProps(_,_)).
must_expand(G):-functor(G,_,A),!,A==1.

% fully_expand_warn(_,C,C):-!.

%= 	 	 

%% fully_expand_warn( ?A, ?B, ?O) is semidet.
%
% Fully Expand Warn.
%
fully_expand_warn(A,B,O):-
  must(fully_expand(A,B,C)),!,
  sanity(ignore(show_failure(why,same_terms(B,C)))),(O=C;must(sanity(ignore(show_failure(why,same_terms(O,C)))))),!.


%= 	 	 

%% same_terms( ?A, :TermB) is semidet.
%
% Same Terms.
%
same_terms(A,B):-A=@=B,!.
same_terms(A,A):-!,fail.
same_terms((A:-AA),(B:-BB)):-!,same_terms(A,B),same_terms(AA,BB).
same_terms(M:A,B):-atom(M),!,same_terms(A,B).
same_terms(A,M:B):-atom(M),!,same_terms(A,B).

:- export(fully_expand/3).


:- export(fully_expand/2).

%= 	 	 

%% fully_expand( ^X, --Y) is det.
%
% Fully Expand.
%
fully_expand(X,Y):- must((fully_expand(clause(unknown,cuz),X,Y))).

%:- mpred_trace_nochilds(fully_expand/3).



ain_expanded(III):- fully_expand_now(change(assert,ain_expanded),III,OOO),ain(OOO).

%% fully_expand( ++Op, ^Sent, --SentO) is det.
%
% Fully Expand.
%
%  Op = the type of operation we are expanding for.. currently there are
%  change(_,_) - for inclusion and testing of present in plain prolog
%  query(_,_) - for call/ask that is dirrectly runnable
%  pfc(_,_) - for salient language based analysis at a human level
%

in_dialect_pfc:- 
  (current_prolog_flag(dialect_pfc,true); 
  (source_location(F,_W),( atom_concat(_,'.pfc.pl',F);atom_concat(_,'.plmoo',F);atom_concat(_,'.pfc',F)))),!.

fully_expand(Op,Sent,SentO):-
 % must(/*hotrace*/( / nop(deserialize_attvars(Sent,SentI)))),
 gripe_time(0.2,w_tl_e(t_l:no_kif_var_coroutines(true),fully_expand_now(Op,Sent,SentO))).

%% fully_expand_now( ++Op, ^Sent, --SentO) is det.
%
% Fully Expand Now.

fully_expand_now(Op,Sent,SentO):- 
 must((dont_make_cyclic((w_tl(t_l:disable_px,must(fully_expand_clause_now(Op,Sent,SentO))))))),!.
 /*
    SentO=BO,
    sanity(Sent=@=NoVary),
   ignore(((fail,cnotrace((Sent\=@=SentO, (Sent\=isa(_,_)->SentO\=isa(_,_);true), 
    (Sent \=@= baseKB:SentO), dmsg(fully_expand_now(Op,(Sent --> SentO)))))))))))))),!.
*/
     
:- decl_shared(mtExact/1).

%% is_stripped_module( +Op) is semidet.
%
%  Is a stripped Module (Meaning it will be found via inheritance)
%
is_stripped_module(A):-var(A),!,fail.
is_stripped_module(Mt):- call_u(mtExact(Mt)),!,fail.
%is_stripped_module(Inherited):-'$current_source_module'(E), default_module(E,Inherited).
%is_stripped_module(Inherited):-'$current_typein_module'(E), default_module(E,Inherited).
is_stripped_module(abox).
% is_stripped_module(_):-!,fail.
% is_stripped_module(baseKB).
% is_stripped_module(A):- defaultAssertMt(AB),!,AB=A.



%% expand_isEach_or_fail(^Sent, --SentO) is semidet.
%
% Expand isEach/Ns.

/*
expand_isEach_or_fail(Sent,SentO):- get_lang(pfc),quietly_must(demodulize(clause(_,_),
   Sent,SentM)),Sent\=@=SentM,!,bagof(O,do_expand_args(isEach,SentM,O),SentO),!.
*/
expand_isEach_or_fail(Sent,SentO):-
    notrace((bagof(O,do_expand_args(isEach,Sent,O),L),!,L\=@=[Sent],SentO=L)).

%% expand_kif_string_or_fail( ++Op, ++Sent, --SentO) is semidet.
%
% Expand if String of KIF.
expand_kif_string_or_fail(_Why,I,O):- string(I), % atom_contains(I,'('),
   input_to_forms(string(I),Wff,Vs)->
   put_variable_names(Vs) ->
   must(sexpr_sterm_to_pterm(Wff,PTerm))->
   PTerm\=@=I -> 
   O=PTerm.


expand_kif_string(I,O):- any_to_string(I,S),
  input_to_forms(string(S),O,Vs)->
  put_variable_names(Vs).
  

%% fully_expand_clause( ++Op, :TermSent, -- SentO) is det.
%
% Fully Expand Clause.
%
fully_expand_clause_now(Op,Sent,SentO):- expand_isEach_or_fail(Sent,SentM),SentM\=@=Sent,!,must(fully_expand_clause(Op,SentM,SentO)).
fully_expand_clause_now(Op,Sent,SentO):- fully_expand_clause(Op,Sent,SentO),!.
fully_expand_clause_now(Op,Sent,SentO):-memoize_on(fully_expand,Sent->SentO,fully_expand_clause(Op,Sent,SentO)).

fully_expand_clause(Op,Sent,SentO):- sanity(is_ftNonvar(Op)),sanity(var(SentO)),var(Sent),!,Sent=SentO.
fully_expand_clause(Op,Sent,SentO):- expand_kif_string_or_fail(Op,Sent,SentM),SentM\=@=Sent,!,must(fully_expand_clause(Op,SentM,SentO)).
fully_expand_clause(_,(:-(Sent)),(:-(Sent))):-!.


fully_expand_clause(Op,Sent,SentO):- is_list(Sent),!,must_maplist(fully_expand_clause(Op),Sent,SentO).
fully_expand_clause(Op,(B,H),Out):- !,must((fully_expand_clause(Op,H,HH),fully_expand_clause(Op,B,BB))),!,must(Out=(BB,HH)).
fully_expand_clause(Op,':-'(Sent),Out):-!,fully_expand_goal(Op,Sent,SentO),!,must(Out=':-'(SentO)).

fully_expand_clause(_,Sent,SentO):- t_l:infSkipFullExpand,!,must(Sent=SentO).


fully_expand_clause(Op,Sent,SentO):- (not_ftCompound(Sent)),!,fully_expand_head(Op,Sent,SentO).

fully_expand_clause(Op,'==>'(Sent),(SentO)):-!,fully_expand_clause(Op,Sent,SentO),!.
fully_expand_clause(Op,'=>'(Sent),(SentO)):-!,fully_expand_clause(Op,Sent,SentO),!.
fully_expand_clause(Op,M:Sent,SentO):- is_stripped_module(M),!,fully_expand_clause(Op,Sent,SentO).

fully_expand_clause(Op,(B/H),Out):- !,fully_expand_head(Op,H,HH),fully_expand_goal(Op,B,BB),!,must(Out=(BB/HH)).
fully_expand_clause(Op ,NC,NCO):- db_expand_final(Op,NC,NCO),!.
fully_expand_clause(Op, HB,HHBB):- 
  to_reduced_hb(Op,HB,H,B)-> 
  \+ is_true(B) ->
  must((fully_expand_head(Op,H,HH),fully_expand_goal(Op,B,BB))) ->
  reduce_clause(Op,(HH:-BB),HHBB) ->
  HB \=@= HHBB,!.
fully_expand_clause(Op,Sent,SentO):- must(fully_expand_head(Op,Sent,SentO)).


%% fully_expand_goal( ?Op, ^ Sent, -- SentO) is det.
%
% Fully Expand Goal.
%
fully_expand_goal(Op,Sent,SentO):- % must(functor(Op,_,2)),
 must((
  w_tl(t_l:into_form_code,fully_expand_head(Op,Sent,SentM)),
    recommify(SentM,SentO))).

/*

?- recommify((a,{((b,c),d)},e),O).
O =  (a, {b, c, d}, e).

?- recommify((a,{((b,c),d)},e),O).
O =  (a, {b, c, d}, e).

?- recommify((a,(b,c,d),e),O).
O =  (a, b, c, d, e).

?- recommify((a,(b,c),(d,e)),O).
O =  (a, b, c, d, e).

?- recommify((a,(b,c),(true,e)),O).
O =  (a, b, c, e).

?- recommify((((a0,a),(b,c)),(true,d,e)),O),portray_clause((h:-O)).
O =  (a0, a, b, c, d, e).

?- recommify((a,(b,c),call((true,e)),true),O).
O =  (a, b, c, call(e)).

*/

recommify(A,AA):- \+ compound(A),!,AA=A.
% recommify(A,A):-!.
recommify(A,B):- recommify(true,A,B),!.

recommify(A,B,C):- \+ compound(B),!,conjoin(A,B,C).
recommify(A,(B,C),D):- \+ compound(B),!, conjoin(A,B,AB), recommify(AB,C,D).
recommify(A,((X,B),C),D):- !, recommify(A,X,AX),recommify(AX,(B,C),D).
recommify(A,(B,C),D):- !, conjoin(A,B,AB), recommify(AB,C,D).
recommify(A,PredArgs,C):- PredArgs=..[P|Args],maplist(recommify,Args,AArgs),B=..[P|AArgs],conjoin(A,B,C),!.





:- export(as_is_term/1).

%% as_is_term( :TermNC) is semidet.
%
% Converted To If Is A Term Primary Helper.
%
as_is_term(NC):- cyclic_break(NC), var(NC),!.
as_is_term(A):-atomic(A),!.
as_is_term(argsQuoted(Atom)):- !, \+ compound(Atom).
as_is_term(prologMacroHead(Atom)):- !, \+ compound(Atom).
as_is_term(functorDeclares(Atom)):- !, \+ compound(Atom).
as_is_term('$VAR'(_)).
as_is_term(PARSE):-is_parse_type(PARSE),!,fail.
as_is_term('$was_imported_kb_content$'(_,_)):-dtrace.
as_is_term('wid'(_,_,_)).
as_is_term('call'(_)).
% as_is_term0('{}'(_)).
as_is_term('ignore'(_)).
as_is_term(_:NC):-!,as_is_term(NC). 
as_is_term(NC):-exact_args(NC),!.
as_is_term(NC):-functor(NC,Op,2),infix_op(Op,_).
as_is_term(NC):-loop_check(is_unit(NC)),!.
:- mpred_trace_none(as_is_term(_)).
:- '$set_predicate_attribute'(as_is_term(_), hide_childs, 1).
:- lock_predicate(as_is_term(_)).

%=  :- was_export(infix_op/2).

%= 	 	 

%% infix_op( ?Op, ?VALUE2) is semidet.
%
% Infix Oper..
%
infix_op(Op,_):-comparitiveOp(Op).
infix_op(Op,_):-additiveOp(Op).

%=  :- was_export(comparitiveOp/1).

%= 	 	 

%% comparitiveOp( +Op) is semidet.
%
% Comparitive Oper..
%
comparitiveOp((\=)).
comparitiveOp((\==)).
comparitiveOp((=)).
comparitiveOp((=:=)).
comparitiveOp((==)).
comparitiveOp((<)).
comparitiveOp((>)).
comparitiveOp((=<)).
comparitiveOp((>=)).

%=  :- was_export(additiveOp/1).

%= 	 	 

%% additiveOp( +Op) is semidet.
%
% Additive Oper..
%
additiveOp((is)).
additiveOp((*)).
additiveOp(+).
additiveOp(-).
additiveOp((/)).



%= 	 	 

%% is_unit( ?C) is semidet.
%
% If Is A Unit.
%
is_unit(A):-notrace(is_unit_like(A)).
is_unit_like(A):- atomic(A),!.
is_unit_like(C):- mpred_get_attr(C,sk,_),!.
is_unit_like(C):- var(C),!,fail.
is_unit_like(C):- \+ compound(C),!.
is_unit_like(C):- C\='VAR'(_),C\='$VAR'(_),C\=(_:-_),C\=ftRest(_),C\=ftListFn(_),get_functor(C,F),is_unit_functor(F).



%= 	 	 

%% is_unit_functor( ?F) is semidet.
%
% If Is A Unit Functor.
%
is_unit_functor(F):- (\+ atom(F)),!,fail.
is_unit_functor(F):-atom_concat('sk',_,F).
is_unit_functor(F):-atom_concat(_,'Fn',F).


%= 	 	 

%% get_ruleRewrite( ^ Sent, ?SentM) is semidet.
%
% Get Rule Rewrite.
%
% TODO - remove the fail (added just to speed up testing and initial debugging)
get_ruleRewrite(Sent,SentM):- fail, cheaply_u(ruleRewrite(Sent,SentM)).



transitive_lc_nr(P,A,B):- call(P,A,B),!.
transitive_lc_nr(_,A,A).
%= 	 	 


%% mpred_expand_rule( ?PfcRule, ?Out) is det.
%
% Managed Predicate Expand.
%
mpred_expand_rule(PfcRule,Out):-is_ftCompound(PfcRule),functor(PfcRule,F,A),
   mpred_database_term(F,A,_),
   PfcRule=[F|Args],maplist(fully_expand_goal(assert),Args,ArgsO),!,Out=..[F|ArgsO].

is_parse_type(Var):-var(Var),!,fail.
is_parse_type('kif'(NV)):-nonvar(NV).
is_parse_type('pkif'(NV)):-nonvar(NV).

%% db_expand_final( +Op, +TermNC, ?NC) is semidet.
%
% Database Expand Final.
%
% db_expand_final(Op,Sent,Sent):- Sent=..[_,A],atom(A),!.
db_expand_final(_ ,NC,NC):-as_is_term(NC),!.
db_expand_final(_,PARSE,_):- is_parse_type(PARSE),!,fail.
%db_expand_final(_,PARSE,ISA):- PARSE=..[t,C,I],atom(C),atom(I),ISA=..[C,I],!.
db_expand_final(_ ,NC,NC):-functor(NC,_,1),arg(1,NC,T),(not_ftCompound(T)),!.
db_expand_final(_, Sent,true):-is_true(Sent).
db_expand_final(Op,M:Sent,SentO):- atom(M),is_stripped_module(M),!,db_expand_final(Op,Sent,SentO).
db_expand_final(_,Term,Term):- is_ftCompound(Term),functor(Term,F,_),cheaply_u(argsQuoted(F)),!.
db_expand_final(_, arity(F,A),arity(F,A)):- not_ftCompound(F),not_ftCompound(A),!.
db_expand_final(_, tPred(V),tPred(V)):-!,fail, not_ftCompound(V),!.
%db_expand_final(_ ,NC,NC):-functor(NC,_,1),arg(1,NC,T),db_expand_final(_,T,_),!.
db_expand_final(_ ,isa(Atom,PredArgTypes), tRelation(Atom)):-PredArgTypes==meta_argtypes,atom(Atom),!.
db_expand_final(Op, meta_argtypes(Args),    O  ):-is_ftCompound(Args),functor(Args,Pred,A),
    (Pred=t->  (fully_expand_head(Op, Args,ArgsO),O=meta_argtypes(ArgsO)) ; (assert_arity(Pred,A),O=meta_argtypes(Args))).
db_expand_final(_ ,meta_argtypes(F,Args),    meta_argtypes(Args)):-atom(F),!,functor(Args,Pred,A),assert_arity(Pred,A).
db_expand_final(_ ,meta_argtypes(Args),      meta_argtypes(Args)):-!.
db_expand_final(_ ,isa(Args,Meta_argtypes),  meta_argtypes(Args)):-Meta_argtypes==meta_argtypes,!,is_ftCompound(Args),!,functor(Args,Pred,A),assert_arity(Pred,A).
db_expand_final(Op,(A,B),(AA,BB)):-  !,db_expand_final(Op,A,AA),db_expand_final(Op,B,BB).
db_expand_final(Op,props(A,B),PROPS):- (is_ftNonvar(A);is_ftNonvar(B)),!,expand_props(_,Op,props(A,B),Props),!,Props\=props(_,_),fully_expand_head(Op,Props,PROPS).
/*
db_expand_final(_, MArg1User, NewMArg1User):- is_ftCompound(MArg1User), fail,
   MArg1User=..[M,Arg1,Arg2|User],
   compound_all_open(Arg1),
   get_functor(Arg1,F,A),F\==(t),F\==(/),
   member(F,[arity,predicateConventionMt]),
   NewMArg1User=..[M,F/A,Arg2|User],!.
*/



%= 	 	 

%% is_elist_functor( +Op) is semidet.
%
% If Is A Elist Functor.
%
is_elist_functor(isList).
is_elist_functor(ftListfn).
is_elist_functor(isEach).
is_elist_functor(isAnd).


%= 	 	 

%% as_list( ?EC, ?AL) is det.
%
% Converted To List.
%
as_list(ftListFn(Atom),[Atom]):- atom(Atom),!.
as_list(Atom,[]):-is_elist_functor(Atom),!.
as_list(EC,AL):-compound(EC),EC=..[IsEach,A|List],is_elist_functor(IsEach),!,((List==[],is_list(A))->AL=A;AL=[A|List]).
as_list(List,AL):-sanity(is_list(List)),AL=List.



%= 	 	 

%% listToE( ?EL, ?E) is det.
%
% List Converted To E.
%
listToE(EL,E):-is_ftNonvar(EL),must((ground(EL),as_list(EL,List))),E=..[isEach|List].



%= 	 	 

%% db_expand_chain( +Op, ?M, ?PO) is det.
%
% Database Expand Chain.
%
db_expand_chain(_,V,_):-var(V),!,fail.
db_expand_chain(_,M:PO,PO) :- atom(M),!.
db_expand_chain(_,(P:-B),P) :-is_true(B),!.
db_expand_chain(_,B=>P,P) :-is_true(B),!.
db_expand_chain(_,<=(P,B),P) :-is_true(B),!.
db_expand_chain(_,P<==>B,P) :-is_true(B),!.
db_expand_chain(_,B<==>P,P) :-is_true(B),!.
db_expand_chain(_,P<-B,P) :-is_true(B),!.
db_expand_chain(_,isa(I,Not),INot):-Not==not,!,INot =.. [Not,I].
%db_expand_chain(_,P,PE):-fail,cyc_to_clif_entry(P,PE).
db_expand_chain(_,('nesc'(P)),P) :- !.


%= 	 	 

%% fully_expand_head_throw_if_loop( ?A, ?B, ?C) is semidet.
%
% Database Expand A.
%
fully_expand_head_throw_if_loop(A1,B1,C1):- must(loop_check_term(fully_expand_head(A1,B1,C1),fully_expand_head(A1,B1,C1),trace_or_throw(loop_check(fully_expand_head(A1,B1,C1))))),!.

%= 	 	 

%% fully_expand_head( ?A, ?B, ?C) is semidet.
%
% Database Expand A Noloop.
%
fully_expand_head(Op,Sent,SentO):-
  memoize_on(fully_expand,Sent->SentO,fully_expand_head_now(Op,Sent,SentO)).

fully_expand_head_now(Why,Before,After):-
   subst(Before,mpred_isa,isa,Before1),
   into_mpred_form(Before1,Before2),
   must(
    loop_check_term(
        transitive_lc(try_expand_head(Why),Before2,After),
        fully_expand_head_loop(Why,Before), 
        Before2=After)),!.


%db_expand_0(Op,Sent,SentO):- is_meta_functor(Sent,F,List),F\=t,!,must_maplist(fully_expand_goal(Op),List,ListO),List\=@=ListO,SentO=..[F|ListO].
%db_expand_0(_ ,NC,OUT):-mpred_expand(NC,OUT),NC\=@=OUT,!.
try_expand_head(_,A,B):- t_l:infSkipFullExpand,!,A=B.
%==SKIPPED==% try_expand_head(Op,Sent,SentO):-db_quf(Op,Sent,Pretest,Template),(Pretest==true-> SentO = Template ; SentO = (Pretest,Template)),!.
%==SKIPPED==% try_expand_head(_Op,Sent,SentO):-once(transform_holds(t,Sent,SentO)).
%==COVERED==% try_expand_head(_Op,Sent,SentO):-to_predicate_isas(Sent,SentO).
try_expand_head(Op,Sent,SentO):- transitive_lc(db_expand_0(Op),Sent,OO),!,SentO=OO.
%==SKIPPED==% try_expand_head(Op,Sent,SentO):-loop_check(expand_term(Sent,SentO)).



:- meta_predicate temp_comp(*,*,2,?).

temp_comp(H,B,PRED,OUT):- nonvar(H),term_variables(B,Vs1),Vs1\==[], term_attvars(B,AVs1), AVs1==[],   
   must_atomic((asserta(('$temp_comp123'(H,B):- B),Ref),clause('$temp_comp123'(H,_),BO,Ref),erase(Ref))),
   B\=@=BO,!,
   must((term_variables(BO,Vs2),!,must_maplist(=,Vs1,Vs2),call(PRED,(H:-BO),OUT))).


%= 	 	 

%% db_expand_0( ?Op, ^ Sent, -- SentO) is semidet.
%
% Database expand  Primary Helper.
%

%% db_expand_0( ++Op, +:Sent, -SentO) is det.
%
% Database Expand Term Primary Helper.
%
% :- meta_predicate(term_expansion(':'(:-),(:-))).
% :- mode(term_expansion(+,--)).
db_expand_0(Op,Sent,SentO):- sanity(is_ftNonvar(Op)),sanity(var(SentO)),var(Sent),!,Sent=SentO.
db_expand_0(Op,pkif(SentI),SentO):- nonvar(SentI),!,must((any_to_string(SentI,Sent),must(expand_kif_string_or_fail(Op,Sent,SentM)),SentM\=@=Sent,!,
  fully_expand_clause_now(Op,SentM,SentO))).
db_expand_0(_Op,kif(Sent),SentO):- nonvar(Sent),!, must(expand_kif_string(Sent,SentO)).

db_expand_0(Op,Sent,SentO):- expand_kif_string_or_fail(Op,Sent,SentM),SentM\=@=Sent,!,fully_expand_clause_now(Op,SentM,SentO).
db_expand_0(Op,M:Sent,SentO):- is_stripped_module(M),!,db_expand_0(Op,Sent,SentO).
db_expand_0(Op,Sent,SentO):- cyclic_break(Sent),db_expand_final(Op ,Sent,SentO),!.


%% db_expand_2( +Op, ^ Sent, -- SentO) is semidet.
%
% Database expand  Extended Helper.
%
db_expand_0(_,Sent,SentO):-is_ftNonvar(Sent),get_ruleRewrite(Sent,SentO),!.
db_expand_0(Op,Sent,SentO):- arg(2,Sent,Arg),is_ftNonvar(Arg),get_functor(Sent,F),fail,
  asserted_argIsa_known(F,2,_),!,
  correctArgsIsa(Op,Sent,SentO),!.

db_expand_0(Op ,NC,NCO):- db_expand_final(Op,NC,NCO),!.

db_expand_0(Op,t(Sent),SentO):- is_ftNonvar(Sent),!,fully_expand_head(Op,Sent,SentO).
db_expand_0(Op,{Sent},{SentO}):-!, fully_expand_goal(Op,Sent,SentO).
%==SKIPPED==%  db_expand_0(_ ,NC,NC):- as_is_term(NC),!.

db_expand_0(_,Sent,SentO):- is_ftNonvar(Sent),copy_term(Sent,NoVary),get_ruleRewrite(Sent,SentO),must(Sent\=@=NoVary),SentO \=@= Sent.
db_expand_0(call(Op),Sent,SentO):-  mreq(quasiQuote(QQuote)),subst(Sent,QQuote,isEach,MID),Sent\=@=MID,!,db_expand_0(call(Op),MID,SentO).

db_expand_0(_,I,O):- \+ compound(I),!,I=O.

db_expand_0(Op,Sent,SentO):- transitive_lc(db_expand_chain(Op),Sent,SentO)-> SentO \=@= Sent.

db_expand_0(Op,EL,O):- is_list(EL),!,must_maplist(db_expand_0(Op),EL,O).


db_expand_0(_Op,P,PO):- 
  compound(P),
  P=..[ARE,FF,AA],
  atom_concat('arg',REST,ARE),
  member(E,['Genl','Isa','SometimesIsa','Format','QuotedIsa']),atom_concat(N,E,REST),
  atom_number(N,NN),
  atom_concat('arg',E,AE),
  PO=..[AE,FF,NN,AA],!.


db_expand_0(Op,(H:-B),OUT):- temp_comp(H,B,db_expand_0(Op),OUT).
db_expand_0(Op,(:-(CALL)),(:-(CALLO))):-with_assert_op_override(Op,db_expand_0(Op,CALL,CALLO)).
db_expand_0(Op,isa(I,O),INot):-Not==not,!,INot =.. [Not,I],!,db_expand_0(Op,INot,O).
db_expand_0(Op,isa(I,O),INot):-Not== ( \+ ) ,!,INot =.. [Not,I],!,db_expand_0(Op,INot,O).
%db_expand_0(_,THOLDS,ISA):- was_mpred_isa(THOLDS,I,C),!,to_isa(I,C,ISA).
db_expand_0(Op,THOLDS,OUT):- THOLDS=..[t,P|ARGS],atom(P),!,HOLDS=..[P|ARGS],db_expand_0(Op,HOLDS,OUT).
db_expand_0(Op,RDF,OUT):- RDF=..[SVO,S,V,O],is_svo_functor(SVO),!,must_det(from_univ(_,Op,[V,S,O],OUT)).
db_expand_0(Op,G,OUT):- G=..[Pred,InstFn,VO],InstFn=isInstFn(Type),is_ftNonvar(Type),from_univ(relationMostInstance,Op,[Pred,Type,VO],OUT).
db_expand_0(Op,G,OUT):- G=..[Pred,InstFn|VO],InstFn=isInstFn(Type),is_ftNonvar(Type),GO=..[Pred,Type|VO],db_expand_0(Op,GO,OUT).

db_expand_0(Op,(call_u(CALL)),(call_u(CALLO))):-with_assert_op_override(Op,db_expand_0(Op,CALL,CALLO)).
db_expand_0(_ ,include(CALL),(load_data_file_now(CALL))):- dtrace, !.

db_expand_0(Op,=>(G),(GG)):-!,db_expand_0(Op,(G),(GG)).
db_expand_0(Op,(G,B),(GGBB)):-!,db_expand_0(Op,G,GG),db_expand_0(Op,B,BB),conjoin_l(GG,BB,GGBB).

db_expand_0(Op,(G==>B),(GG==>BB)):-!,db_expand_0(Op,G,GG),db_expand_0(Op,B,BB).


db_expand_0(Op,(G;B),(GG;BB)):-!,db_expand_0(Op,G,GG),db_expand_0(Op,B,BB).
db_expand_0(Op,(G:-B),(GG:-BB)):-!,db_expand_0(Op,G,GG),fully_expand_goal(Op,B,BB).
% db_expand_0(_,Term,CL):- bagof(O,do_expand_args(isEach,Term,O),L),L\=@=[Term],!,list_to_conjuncts(L,CL).

db_expand_0(Op,M:Sent,SentO):- atom(M),is_stripped_module(M),!,db_expand_0(Op,Sent,SentO).
db_expand_0(Op,M:Sent,R:SentO):- replaced_module(Op,M,R),!,db_expand_0(Op,Sent,SentO).


db_expand_0(Op,pddlSomethingIsa(I,EL),O):- listToE(EL,E),fully_expand_clause_now(Op,isa(I,E),O).
db_expand_0(Op,pddlDescription(I,EL),O):- listToE(EL,E),fully_expand_clause_now(Op,mudDescription(I,E),O).
db_expand_0(Op,pddlObjects(I,EL),O):- listToE(EL,E),fully_expand_clause_now(Op,isa(E,I),O).
db_expand_0(Op,pddlSorts(I,EL),O):- listToE(EL,E),fully_expand_clause_now(Op,genls(E,I),O).
db_expand_0(Op,pddlTypes(EL),O):- listToE(EL,E),fully_expand_clause_now(Op,isa(E,tCol),O).
db_expand_0(Op,pddlPredicates(EL),O):- listToE(EL,E),fully_expand_clause_now(Op,prologHybrid(E),O).

db_expand_0(Op,DECL,O):- arg(_,DECL,S),string(S),DECL=..[F|Args],maplist(destringify,Args,ArgsO),
  ArgsO\=@=Args,!,DECLM=..[F|ArgsO],db_expand_0(Op,DECLM,O).

% db_expand_0(Op,EACH,O):- EACH=..[each|List],db_expand_maplist(fully_expand_now(Op),List,T,T,O).
db_expand_0(Op,DECL,(arity(F,A),O)):-DECL=..[D,F/A|Args],is_ftNameArity(F,A),functor_declares_instance(D,TPRED),
  is_ftNonvar(TPRED),is_relation_type(TPRED),expand_props(_Prefix,Op,props(F,[D,TPRED|Args]),O),!.


db_expand_0(Op,DECL,(arity(F,A),O)):-DECL=..[D,F,A|Args],is_ftNameArity(F,A),functor_declares_instance(D,TPRED),
  arity_zor(D,1),
  is_ftNonvar(TPRED),is_relation_type(TPRED),expand_props(_Prefix,Op,props(F,[D,TPRED|Args]),O),!.

db_expand_0(Op,DECL,(arity(F,A),O)):-DECL=..  [D,C|Args],is_ftCompound(C),functor_declares_instance(D,TPRED),
  \+ is_ftVar(C),is_non_unit(C),!,get_functor(C,F,A),  
  arity_zor(D,1),
  is_ftNonvar(TPRED),expand_props(_Prefix,Op,props(F,[D,TPRED|Args]),M),!,
  (\+((arg(_,C,Arg),is_ftVar(Arg))) -> O = (meta_argtypes(C),M) ; (O= (M))).

db_expand_0(Op,DECL,O):-DECL=..[D,F,A1|Args], functor_declares_instance(D,DType),
   arity_zor(D,1),
   %\+ is_relation_type(DType),
   expand_props(_Prefix,Op,props(F,[DType,D,A1|Args]),O),!.

db_expand_0(Op,DECL,O):-DECL=..[D,F|Args],functor_declares_instance(D,DType),
   %\+ is_relation_type(DType),
   arity_zor(D,1),!,
   expand_props(_Prefix,Op,props(F,[DType,D|Args]),O),!.

% shift/1 reset/3
%  room_template(iLivingRoom7,.....).
db_expand_0(Op,ClassTemplate,(tCol(PropsIsa),isa(Inst,PropsIsa),OUT)):- 
   ClassTemplate=..[TypePropsFunctor,Inst|Props],
   functor_declares_instance(TypePropsFunctor,PropsIsa),
   arity_zor(TypePropsFunctor,1),!,
   \+ compound_all_open(ClassTemplate),
   %ain(isa(PropsIsa,tCol)),
   %ain(isa(Inst,PropsIsa)),
   expand_props(t,Op,props(Inst,[PropsIsa|Props]),OUT),!.

% typeProps(tCrackers,.....).
db_expand_0(Op,ClassTemplate,(tCol(PropsIsa),isa(Type,PropsIsa),OUT)):-
   ClassTemplate=..[TypeTypePropsFunctor,Type|Props],
   functor_declares_collectiontype(TypeTypePropsFunctor,PropsIsa),
   arity_zor(TypeTypePropsFunctor,1),
   \+ compound_all_open(ClassTemplate),
   %ain(isa(Type,tCol)),
   %ain(isa(Type,PropsIsa)),
   expand_props(relationMostInstance,Op,props(Type,Props),OUT),!.

% tRegion_inst_template(X, tLivingRoom,.....).
db_expand_0(Op,ClassTemplate,(isa(TypePropsIsa,Type),(isa(NewInst,Type)=>OUT))):- 
  ClassTemplate=..[FunctorTypePropsIsa,NewInst,Type|Props],
  instTypePropsToType(FunctorTypePropsIsa,TypePropsIsa),
  arity_zor(FunctorTypePropsIsa,2),
   \+ compound_all_open(ClassTemplate),
  expand_props(Op,props(NewInst,Props),OUT),!.

/*

% tRegion_template(tLivingRoom,.....).
db_expand_0(Op,typeProps(C,Props),(isa(I,C)=>OOUT)):- (is_ftNonvar(C);is_ftNonvar(Props)), expand_props(Prefix,Op,props(I,Props),OUT),dtrace,list_to_conjuncts(OUT,OUTC),conjuncts_to_list(OUTC,OUTL),
   ISEACH=..[isEach|OUTL],
  db_expand_0(Op,mdefault(ISEACH),OOUT).

*/

% db_expand_0(Op,C,F/A):-compound_all_open(C),get_functor(C,F,A).
db_expand_0(Op,ClassTemplate,OUT):- ClassTemplate=..[props,Inst,Second,Third|Props],!,
   expand_props(_Prefix,Op,props(Inst,[Second,Third|Props]),OUT),!.

db_expand_0(Op,IN,OUT):- IN=..[F|Args],F==t,!,must(from_univ(_,Op,Args,OUT)).
db_expand_0(Op,isa(A,F),OO):-atom(F),O=..[F,A],!,db_expand_0(Op,O,OO).
db_expand_0(Op,isa(A,F),OO):-is_ftNonvar(A),is_ftNonvar(F),expand_props(_Prefix,Op,props(A,F),OO),!.
db_expand_0(_Op,isa(A,F),isa(A,F)):-!.
db_expand_0(Op,props(A,F),OO):-expand_props(_Prefix,Op,props(A,F),OO),!.

db_expand_0(_,arity(F,A),arity(F,A)):-atom(F),!.
db_expand_0(Op,arity(F,A),O):-expand_props(_Prefix,Op,props(F,arity(A)),O),!.
db_expand_0(Op,IN,OUT):- 
   IN=..[F|Args],
   % wdmsg(db_expand_0(Op,IN)),
   sanity(F \== isa),
   must_maplist(db_expand_0(Op),Args,ArgsO),
   map_f(F,FO),OUT=..[FO|ArgsO].
   

%= 	 	 

%% is_arity_pred( +Op) is semidet.
%
% If Is A Arity Predicate.
%
is_arity_pred(argIsa).
is_arity_pred(arity).

arity_zor(D,ZOR) :- atom(D),D\==isa, \+ (arity(D,N),!,N>ZOR).

%= 	 	 

%% map_f( ?F, ?F) is semidet.
%
% Map False.
%
map_f(M:F,M:FO):-atom(M),map_f(F,FO).
map_f(mpred_isa,isa).
map_f(props,isa).
map_f(F,F):-!.


%= 	 	 

%% ex_argIsa( ?P, ?N, ?C) is semidet.
%
% ex Argument  (isa/2).
%
ex_argIsa(P,N,C):- clause(_:argIsa(P,N,C),true).


%= 	 	 

%% compound_all_open( ?C) is semidet.
%
% Compound All Open.
%
compound_all_open(C):-compound(C),functor(C,_,A),A>1,\+((arg(_,C,Arg),is_ftNonvar(Arg))),!.

/*
db_expand_0(Op,Mt:Term,Mt:O):- is_kb_module(Mt),!,w_tl(t_l:caller_module(baseKB,Mt),db_expand_0(Op,Term,O)).
db_expand_0(Op,DB:Term,DB:O):- defaultAssertMt(DB),!,w_tl(t_l:caller_module(db,DB),db_expand_0(Op,Term,O)).
db_expand_0(Op,KB:Term,KB:O):- atom(KB),!,w_tl(t_l:caller_module(prolog,KB),db_expand_0(Op,Term,O)).
*/

% db_expand_0(query(HLDS,Must),props(Obj,Props)):- is_ftNonvar(Obj),is_ftVar(Props),!,gather_props_for(query(HLDS,Must),Obj,Props).

replaced_module(_,V,_):- \+ atom(V),!,fail.
replaced_module(_,umt,ABox):-defaultAssertMt(ABox).
replaced_module(_,abox,ABox):-defaultAssertMt(ABox).
replaced_module(_,tbox,TBox):-get_current_default_tbox(TBox).


maybe_prepend_mt(MT,I,O):- t_l:current_defaultAssertMt(ABOX)->ABOX==MT,!,maybe_prepend_mt(abox,I,O).
maybe_prepend_mt(abox,H,HH):-nonvar(HH),dtrace,maybe_prepend_mt(abox,H,HHH),must(HHH=HH),!.
maybe_prepend_mt(abox,H,HH):-var(H),must(HH=H),!.
maybe_prepend_mt(_,CL,CL):- compound(CL),CL=(_,_),!.
maybe_prepend_mt(_,H,HH):-predicateSystemCode(H,HH),!.
maybe_prepend_mt(abox,_:HH,HH):-!.
maybe_prepend_mt(abox,HH,HH):-!.
maybe_prepend_mt(Mt,Mt:HH,Mt:HH):-!.
maybe_prepend_mt(_,Mt:HH,Mt:HH):-!.
maybe_prepend_mt(Mt,HH,Mt:HH):-!.

predicateSystemCode(P,PP):-strip_module(P,_,PP),predicate_property(system:PP,defined),
  \+ predicate_property(system:PP,imported_from(baseKB)).

%% remodulize( ?Why, ?H, ?HH) is det.
%
% Re-Modulize.
%
remodulize(_, H,H):- is_ftVar(H),!.
remodulize(_, H,H):- \+ compound(H),!. % this disables the two next rules
remodulize(Why, H,HH):- atom(H),convention_to_symbolic_mt(Why,H,0,M),maybe_prepend_mt(M,H,HH).
remodulize(call(Why),M,R):-atom(M),replaced_module(Why,M,R),!.
remodulize(Why,M:H,M:HHH):-is_ftVar(M),!,must_remodulize(mvar(Why),H,HHH).
remodulize(Why,H,HH):-is_list(H),!,must_maplist(remodulize(Why),H,HH),!.
remodulize(Why,':-'(G),':-'(GG)):-!,must_remodulize(call(Why),G,GG).
remodulize(Why,(H:-G),(HH:-GG)):-!,must_remodulize(clause(Why,(':-')),H,HH),must_remodulize(call(Why),G,GG).
remodulize(Why,(H,G),(HH,GG)):-!,must_remodulize(call(Why),H,HH),must_remodulize(call(Why),G,GG).
remodulize(Why,(H;G),(HH;GG)):-!,must_remodulize(call(Why),H,HH),must_remodulize(call(Why),G,GG).

remodulize(Why,M:H,R:HHH):- replaced_module(Why,M,R),!,must_remodulize(Why,H,HHH).
remodulize(Why,M:H,HHH):- is_stripped_module(M),!,must_remodulize(Why,H,HHH).

remodulize(Why,Mt:H,HHHH):- is_ftCompound(H),H=..[F|HL],!,must_maplist(remodulize(Why),HL,HHL),HH=..[F|HHL],!,
  must((remodulize_pass2(Why,HH,HHH),maybe_prepend_mt(Mt,HHH,HHHH))).

remodulize(Why,H,HHH):-is_ftCompound(H),H=..[F|HL],!,must_maplist(remodulize(Why),HL,HHL),HH=..[F|HHL],!,
  must(remodulize_pass2(Why,HH,HHH)).

remodulize_pass2(Why,MHH,HHH):- strip_module(MHH,_,HH),functor(HH,F,A),convention_to_symbolic_mt(Why,F,A,Mt),maybe_prepend_mt(Mt,HH,HHH).
% remodulize_pass2(Why,HH,HHH):- fix_mp(Why,HH,HHH),!. % this is overzealous
remodulize_pass2(_Why,HH,HH):- !.

must_remodulize(Why,H,HHH):-must(demodulize(Why,H,HHH)),!.
%= 	 	 

%% is_meta_functor( ^ Sent, ?F, ?List) is semidet.
%
% If Is A Meta Functor.
%
is_meta_functor(Sent,F,List):-is_ftCompound(Sent),Sent=..[F|List],(predicate_property(Sent,meta_predicate(_));is_sentence_functor(F);F==pfcDefault),!.
 


%= 	 	 

%% from_univ( ?Prefix, ?Op, :TermMORE, ?Out) is semidet.
%
% Converted From Univ.
%
from_univ(Prefix,Op,[T|MORE],Out):-T==t,!,from_univ(Prefix,Op,MORE,Out).
from_univ(Prefix,Op,[PROP,Obj|MORE],Out):-PROP==props,!,expand_props(Prefix,Op,props(Obj,MORE),Out).
from_univ(Prefix,Op,MORE,Out):-atom(Prefix),!,from_univ(_,Op,[Prefix|MORE],Out).
from_univ(Prefix,Op,[PROP|MORE],Out):-atom(PROP),!,Mid=..[PROP|MORE],db_expand_up(Prefix,Op,Mid,Out).
from_univ(Prefix,Op,In,Out):-Mid=..[t|In],!,db_expand_up(Prefix,Op,Mid,Out).


%= 	 	 

%% db_expand_up( ?Prefix, ?Op, ?Mid, ?OOUT) is semidet.
%
% Database Expand Up.
%
db_expand_up(Prefix,Op,Mid,OOUT):- fully_expand_head(Op,Mid,Out), 
  is_ftCompound(Prefix),subst(Prefix,value,Out,OOUT).
db_expand_up(_,Op,Mid,Out):- fully_expand_head(Op,Mid,Out).


%= 	 	 

%% expand_props( ?Op, ?Term, ?OUT) is semidet.
%
% Expand Props.
%
expand_props(Op,Term,OUT):-expand_props(_,Op,Term,OUT).


%= 	 	 

%% expand_props( ?Prefix, ?VALUE2, ^ Sent, ^ Sent) is semidet.
%
% Expand Props.
%
expand_props(_Prefix,_,Sent,OUT):- (not_ftCompound(Sent)),!,OUT=Sent.
%expand_props(Prefix,Op,Term,OUT):- stack_check,(is_ftVar(OpeR);is_ftVar(Term)),!,trace_or_throw(var_expand_units(OpeR,Term,OUT)).
expand_props(Prefix,Op,Sent,OUT):-Sent=..[And|C12],is_sentence_functor(And),!,maplist(expand_props(Prefix,Op),C12,O12),OUT=..[And|O12].
expand_props(_Prefix,_ ,props(Obj,Open),props(Obj,Open)):- is_ftVar(Open),!. % ,trace_or_throw(expand_props(Prefix,Op,props(Obj,Open))->OUT).
expand_props(_Prefix,_ ,props(Obj,List),ftID(Obj)):- List==[],!.
expand_props(Prefix,Op,props(Obj,[P]),OUT):- is_ftNonvar(P),!,expand_props(Prefix,Op,props(Obj,P),OUT).
expand_props(Prefix,Op,props(Obj,[P|ROPS]),OUT):- !,expand_props(Prefix,Op,props(Obj,P),OUT1),expand_props(Prefix,Op,props(Obj,ROPS),OUT2),conjoin_l(OUT1,OUT2,OUT).
expand_props(Prefix,Op,props(Obj,PropVal),OUT):- atom(PropVal),!,from_univ(Prefix,Op,[PropVal,Obj],OUT).

expand_props(_Prefix,_Op,props(Obj,PropVal),(PropVal2,{OPVAL})):- PropVal=..[OpeR,Pred|Val],comparitiveOp(OpeR),
   not(comparitiveOp(Pred)),!,OPVAL=..[OpeR,NewVar|Val],PropVal2=..[Pred,Obj,NewVar],!.    


expand_props(Prefix,Op,props(Obj,PropVal),OUT):- safe_univ(PropVal,[Prop,NonVar|Val]),Obj==NonVar,!,from_univ(Prefix,Op,[Prop,Obj|Val],OUT).
expand_props(Prefix,Op,props(Obj,PropVal),OUT):- 
   PropVal=..[OpeR,Pred|Val],comparitiveOp(OpeR),
   not(comparitiveOp(Pred)),!,OPVAL=..[OpeR|Val],PropVal2=..[Pred,OPVAL],
    expand_props(Prefix,Op,props(Obj,PropVal2),OUT),!.
expand_props(Prefix,Op,props(Obj,PropVal),OUT):- PropVal=..[Prop|Val],not(infix_op(Prop,_)),!,from_univ(Prefix,Op,[Prop,Obj|Val],OUT).
expand_props(Prefix,Op,props(Obj,PropVal),OUT):- PropVal=..[Prop|Val],!,dtrace(from_univ(Prefix,Op,[Prop,Obj|Val],OUT)).
expand_props(Prefix,Op,props(Obj,Open),props(Obj,Open)):- trace_or_throw(unk_expand_props(Prefix,Op,props(Obj,Open))).

expand_props(Prefix,OpeR,ClassTemplate,OUT):- ClassTemplate=..[props,Inst,Second,Third|Props],!,
   expand_props(Prefix,OpeR,props(Inst,[Second,Third|Props]),OUT),!.

expand_props(_Prefix,_,Sent,Sent).


%= 	 	 

%% conjoin_l( ?A, :TermAA, ?C) is semidet.
%
% Conjoin (list Version).
%
conjoin_l(A,AA,C):-A==AA,!,C=A.
conjoin_l(A,(AA,B),C):-A==AA,!,conjoin_l(A,B,C).
conjoin_l(A,B,C):-conjoin(A,B,C).


%= 	 	 

%% db_quf_l( ?Op, ?And, ?C12, ?Pre2, ?Templ2) is semidet.
%
% Database Quf (list Version).
%
db_quf_l(Op,And,[C],D2,D4):- !, db_quf(Op,C,D2,D3),!,D4=..[And,D3].
db_quf_l(Op,And,C12,Pre2,Templ2):-db_quf_l_0(Op,And,C12,Pre2,Templ2).


%= 	 	 

%% db_quf_l_0( ?Op, ?And, :TermC, ?D2, ?D3) is semidet.
%
% Database quf (List version)  Primary Helper.
%
db_quf_l_0(Op,_And,[C],D2,D3):- db_quf(Op,C,D2,D3),!.
db_quf_l_0(Op, And,[C|C12],PreO,TemplO):-
  db_quf(Op,C,Pre,Next),
  db_quf_l_0(Op,And,C12,Pre2,Templ2),
  conjoin_l(Pre,Pre2,PreO),
  conjoin_op(And,Next,Templ2,TemplO).

%=  :- was_export(db_quf/4).

%= 	 	 

%% db_quf( +Op, ?C, ?Pretest, ?Template) is semidet.
%
% Database Quf.
%
db_quf(_ ,C,Pretest,Template):- (not_ftCompound(C)),!,must(Pretest=true),must(Template=C).
db_quf(Op,C,Pretest,Template):-is_ftVar(C),!,trace_or_throw(var_db_quf(Op,C,Pretest,Template)).
db_quf(_ ,C,Pretest,Template):-as_is_term(C),!,must(Pretest=true),must(Template=C),!.

db_quf(Op,M:C,Pretest,M:Template):-atom(M),!,must(db_quf(Op,C,Pretest,Template)).

db_quf(Op,C,Pretest,Template):- C=..[Holds,OBJ|ARGS],is_holds_true(Holds),atom(OBJ),!,C1=..[OBJ|ARGS],must(db_quf(Op,C1,Pretest,Template)).
db_quf(_Op,C,true,C):- C=..[Holds,OBJ|_],is_holds_true(Holds),is_ftVar(OBJ),!.
db_quf(Op,Sent,D2,D3):- Sent=..[And|C12],C12=[_|_],is_sentence_functor(And),!, db_quf_l(Op,And,C12,D2,D3).
db_quf(Op,C,Pretest,Template):- C=..[Prop,OBJ|ARGS],
      functor(C,Prop,A),
      show_failure(why,translate_args(Op,Prop,A,OBJ,2,ARGS,NEWARGS,true,Pretest)),
      Template =.. [Prop,OBJ|NEWARGS],!.
db_quf(_Op,C,true,C).


%= 	 	 

%% translate_args( ?O, ?Prop, ?A, ?OBJ, ?N, :TermARG6, :TermARG7, ?GIN, ?GIN) is semidet.
%
% Translate Arguments.
%
translate_args(_O,_Prop,_A,_OBJ,_N,[],[],GIN,GIN).
translate_args(Op,Prop,A,OBJ,N1,[ARG|S],[NEW|ARGS],GIN,GOALS):-
   Type = argIsaFn(Prop,N1),
   translateOneArg(Op,Prop,OBJ,Type,ARG,NEW,GIN,GMID),
   N2 is N1 +1,
   translate_args(Op,Prop,A,OBJ,N2,S,ARGS,GMID,GOALS).


% ftVar

%= 	 	 

%% translateOneArg( ?Op, ?Prop, ?Obj, ?Type, ?VAR, ?VAR, ?G, ?G) is semidet.
%
% Translate One Argument.
%
translateOneArg(_Op,_Prop,_Obj,_Type,VAR,VAR,G,G):-is_ftVar(VAR),!.

% not an expression
translateOneArg(_O,_Prop,_Obj,_Type,ATOMIC,ATOMIC,G,G):-atomic(ATOMIC),!.
% translateOneArg(_O,_Prop,_Obj,Type,ATOMIC,ATOMICUSE,G,(G,same_arg(tCol(Type),ATOMIC,ATOMICUSE))):-atomic(ATOMIC),!.

% translateOneArg(_O,_Prop,_Obj,Type,VAR,VAR,G,G):-ignore(isa(VAR,Type)),!.

% props(Obj,size < 2).
translateOneArg(_O,Prop,Obj,Type,ARG,OLD,G,(GETTER,COMPARE,G)):-
       functor(ARG,F,2), comparitiveOp(F),!,
       ARG=..[F,Prop,VAL],
       GETTER=..[Prop,Obj,OLD],
       COMPARE= compare_op(Type,F,OLD,VAL),!.

% props(Obj,isOneOf(Sz,[size+1,2])).
translateOneArg(Op,Prop,O,Type,isOneOf(VAL,LIST),VAL,G,(G0,G)):-
   translateListOps(Op,Prop,O,Type,VAL,LIST,G,G0).

% db_op(Op, Obj,size + 2).
translateOneArg(_O,Prop,Obj,_Type,ARG,NEW,G,(GETTER,STORE,G)):-
       ground(ARG),
       functor(ARG,F,2), additiveOp(F),!,
       ARG=..[F,Prop,VAL],
       GETTER=..[Prop,Obj,OLD],
       STORE= update_value(OLD,VAL,NEW),!.

translateOneArg(_O,_Prop,_Obj,_Type,NART,NART,G,G):-!. % <- makes us skip the next bit of code
translateOneArg(_O,_Prop,_Obj,Type,ATOMIC,ATOMICUSE,G,(G,ignore(same_arg(tCol(Type),ATOMIC,ATOMICUSE)))).


%= 	 	 

%% translateListOps( ?O, ?Prop, ?Obj, ?Type, ?VAL, :TermARG6, ?G, ?G) is semidet.
%
% Translate List Oper.s.
%
translateListOps(_O,_Prop,_Obj,_Type,_VAL,[],G,G).
translateListOps(Op,Prop,Obj,Type,VAL,[L|LIST],G,GO2):-
   translateOneArg(Op,Prop,Obj,Type,L,VAL,G,G0),
   translateListOps(Op,Prop,Obj,Type,VAL,LIST,G0,GO2).


%= 	 	 

%% compare_op( ?Type, :PRED2F, ?OLD, ?VAL) is semidet.
%
% Compare Oper..
%
compare_op(Type,F,OLD,VAL):-nop(Type),show_call(why,(call(F,OLD,VAL))),!.


% load_motel:- defrole([],time_state,restr(time,period)).
% :-load_motel.

% ========================================
% expanded_different compares fact terms to see if they are different
% ========================================

:- '$hide'(expanded_different/2).
%=  :- was_export(expanded_different/2).


%= 	 	 

%% expanded_different( ?G0, ?G1) is semidet.
%
% Expanded Different.
%
expanded_different(G0,G1):-call(expanded_different_ic(G0,G1)).


%= 	 	 

%% expanded_different_ic( ?G0, ?G1) is semidet.
%
% Expanded Different Ic.
%
expanded_different_ic(G0,G1):-G0==G1,!,fail.
expanded_different_ic(G0,G1):-expanded_different_1(G0,G1),!.
expanded_different_ic(G0,G1):- G0\==G1.


%= 	 	 

%% expanded_different_1( ?G0, :TermG1) is semidet.
%
% expanded different  Secondary Helper.
%
expanded_different_1(NV:G0,G1):-is_ftNonvar(NV),!,expanded_different_1(G0,G1).
expanded_different_1(G0,NV:G1):-is_ftNonvar(NV),!,expanded_different_1(G0,G1).
expanded_different_1(G0,G1):- (is_ftVar(G0);is_ftVar(G1)),!,trace_or_throw(expanded_different(G0,G1)).
expanded_different_1(G0,G1):- G0 \= G1,!.


% ========================================
% into_functor_form/3 (adds a second order functor onto most predicates)
% ========================================
%=  :- was_export(into_functor_form/3).

%= 	 	 

%% into_functor_form( ?HFDS, ?X, ?O) is semidet.
%
% Converted To Functor Form.
%
into_functor_form(HFDS,M:X,M:O):- atom(M),!,into_functor_form(HFDS,X,O),!.
into_functor_form(HFDS,X,O):-call((( X=..[F|A],into_functor_form(HFDS, X,F,A,O)))),!.

% TODO finish negations

%= 	 	 

%% into_functor_form( ?Dbase_t, ?X, ?Dbase_t, ?A, ?X) is semidet.
%
% Converted To Functor Form.
%
into_functor_form(Dbase_t,X,Dbase_t,_A,X):-!.
into_functor_form(Dbase_t,_X,holds_t,A,Call):-Call=..[Dbase_t|A].
into_functor_form(Dbase_t,_X,t,A,Call):-Call=..[Dbase_t|A].
into_functor_form(Dbase_t,_X,HFDS,A,Call):- is_holds_true(HFDS), Call=..[Dbase_t|A].
into_functor_form(Dbase_t,_X,F,A,Call):-Call=..[Dbase_t,F|A].

% ========================================
% into_mpred_form/2 (removes a second order functors until the common mpred form is left)
% ========================================
%=  :- was_export(into_mpred_form/2).

%= 	 	 

%% into_mpred_form( :TermV, ?VO) is semidet.
%
% Converted To Managed Predicate Form.
%

% into_mpred_form(Var,MPRED):- is_ftVar(Var), trace_or_throw(var_into_mpred_form(Var,MPRED)).
into_mpred_form(V,VO):- (not_ftCompound(V)),!,VO=V.
into_mpred_form(M:X,M:O):- atom(M),!,into_mpred_form(X,O),!.
into_mpred_form(Sent,SentO):-is_ftNonvar(Sent),get_ruleRewrite(Sent,SentM),!,into_mpred_form(SentM,SentO).
into_mpred_form((H:-B),(HH:-BB)):-!,into_mpred_form(H,HH),into_mpred_form(B,BB).
into_mpred_form((H:-B),(HH:-BB)):-!,into_mpred_form(H,HH),into_mpred_form(B,BB).
into_mpred_form((H,B),(HH,BB)):-!,into_mpred_form(H,HH),into_mpred_form(B,BB).
into_mpred_form((H;B),(HH;BB)):-!,into_mpred_form(H,HH),into_mpred_form(B,BB).
into_mpred_form((H/B),(HH/BB)):-!,into_mpred_form(H,HH),into_mpred_form(B,BB).
into_mpred_form(WAS,isa(I,C)):-was_mpred_isa(WAS,I,C),!.
into_mpred_form(t(P,A),O):-atom(P),!,O=..[P,A].
into_mpred_form(t(P,A,B),O):-atom(P),!,O=..[P,A,B].
into_mpred_form(t(P,A,B,C),O):-atom(P),!,O=..[P,A,B,C].
into_mpred_form(IN,OUT):- 
   IN=..[F|Args],
   must_maplist(into_mpred_form,Args,ArgsO),!,
   map_f(F,FO),OUT=..[FO|ArgsO].


% into_mpred_form(I,O):- /*hotrace*/(loop_check(into_mpred_form_ilc(I,O),O=I)). % trace_or_throw(into_mpred_form(I,O).

%:- mpred_trace_nochilds(into_mpred_form/2).


%= 	 	 

%% into_mpred_form_ilc( ?G, ?O) is semidet.
%
% Converted To Managed Predicate Form Inside Of Loop Checking.
%
into_mpred_form_ilc([F|Fist],O):- is_list([F|Fist]),!,G=..[t|[F|Fist]], into_mpred_form(G,O).
into_mpred_form_ilc(G,O):- functor(G,F,A),G=..[F,P|ARGS],!,into_mpred_form6(G,F,P,A,ARGS,O),!.

% TODO confirm negations

:- expire_tabled_list(all).

%= 	 	 

%% was_mpred_isa( ?G, ?VALUE2, ?VALUE3) is semidet.
%
% was  (isa/2) syntax.
%
was_mpred_isa(G,_,_):-is_ftVar(G),!,fail.
was_mpred_isa(isa(I,C),I,C):-!.
was_mpred_isa(t(C,I),I,C):-!.
was_mpred_isa(a(C,I),I,C):-!.
was_mpred_isa(G,I,C):-was_isa(G,I,C).


%= 	 	 

%% into_mpred_form6( ?X, ?H, ?P, ?N, ?A, ?O) is semidet.
%
% Converted To Managed Predicate Form6.
%
into_mpred_form6(C,_,_,2,_,C):-!.
% into_mpred_form6(H,_,_,_,_,G0):- once(w_tl(t_l:into_form_code,(expand_term( (H :- true) , C ), reduce_clause(assert,C,G)))),expanded_different(H,G),!,into_mpred_form(G,G0),!.
into_mpred_form6(_,F,_,1,[C],O):-alt_calls(F),!,into_mpred_form(C,O),!.
into_mpred_form6(_,':-',C,1,_,':-'(O)):-!,into_mpred_form_ilc(C,O).
into_mpred_form6(_,not,C,1,_,not(O)):-into_mpred_form(C,O),!.
into_mpred_form6(C,isa,_,2,_,C):-!.
into_mpred_form6(C,_,_,_,_,isa(I,T)):-was_mpred_isa(C,I,T),!.
into_mpred_form6(_X,t,P,_N,A,O):-!,(atom(P)->O=..[P|A];O=..[t,P|A]).
into_mpred_form6(G,_,_,1,_,G):-predicate_property(G,number_of_rules(N)),N >0, !.
into_mpred_form6(G,F,C,1,_,O):-real_builtin_predicate(G),!,into_mpred_form(C,OO),O=..[F,OO].
into_mpred_form6(_X,H,P,_N,A,O):-is_holds_false(H),(atom(P)->(G=..[P|A],O=not(G));O=..[holds_f,P|A]).
into_mpred_form6(_X,H,P,_N,A,O):-is_holds_true(H),(atom(P)->O=..[P|A];O=..[t,P|A]).
into_mpred_form6(G,F,_,_,_,G):-a(prologHybrid,F),!.
into_mpred_form6(G,F,_,_,_,G):-a(prologDynamic,F),!.
into_mpred_form6(G,F,_,_,_,G):-nop(dmsg(warn(unknown_mpred_type(F,G)))).

% ========================================
% acceptable_xform/2 (when the form is a isa/2, do a validity check)
% ========================================

%= 	 	 

%% acceptable_xform( ?From, ?To) is semidet.
%
% Acceptable Xform.
%
acceptable_xform(From,To):- From \=@= To,  (To = isa(I,C) -> was_mpred_isa(From,I,C); true).

% ========================================
% transform_holds(Functor,In,Out)
% ========================================

%= 	 	 

%% transform_holds( ?H, ?In, ?Out) is semidet.
%
% Transform Holds.
%
transform_holds(H,In,Out):- once(transform_holds_3(H,In,Out)),!,ignore((In\=Out,fail,dmsg(transform_holds(H,In,Out)))).


% foreach_arg/7 
%  is a maping predicate

%= 	 	 

%% foreach_arg( :TermARGS, ?N, ?ArgIn, ?ArgN, ?ArgOut, ?Call, :TermARGS) is semidet.
%
% Foreach Argument.
%
foreach_arg(ARGS,_N,_ArgIn,_ArgN,_ArgOut,_Call,ARGS):- (not_ftCompound(ARGS)),!.
foreach_arg([ArgIn1|ARGS],ArgN1,ArgIn,ArgN,ArgOut,Call1,[ArgOut1|ARGSO]):-
     copy_term( a(ArgIn1,ArgOut1,ArgN1,Call1), a(ArgIn,ArgOut,ArgN,Call) ),
      call(Call),
      ArgN2 is ArgN + 1,
      foreach_arg(ARGS,ArgN2,ArgIn,ArgN,ArgOut,Call,ARGSO).


%= 	 	 

%% transform_functor_holds( +Op, ?F, ?ArgInOut, ?N, ?ArgInOut) is semidet.
%
% Transform Functor Holds.
%
transform_functor_holds(_,F,ArgInOut,N,ArgInOut):- once(argIsa_ft(F,N,FT)),FT=ftTerm,!.
transform_functor_holds(Op,_,ArgIn,_,ArgOut):- transform_holds(Op,ArgIn,ArgOut),!.


%= 	 	 

%% transform_holds_3( +Op, :TermA, ?A) is semidet.
%
% Transform Holds Helper Number 3..
%
transform_holds_3(_,A,A):- (not_ftCompound(A)),!.
transform_holds_3(_,props(Obj,Props),props(Obj,Props)):-!.
%transform_holds_3(Op,Sent,OUT):-Sent=..[And|C12],is_sentence_functor(And),!,maplist(transform_holds_3(Op),C12,O12),OUT=..[And|O12].
transform_holds_3(_,A,A):-compound(A),functor(A,F,N), predicate_property(A,_),arity(F,N),!.
transform_holds_3(HFDS,M:Term,M:OUT):-atom(M),!,transform_holds_3(HFDS,Term,OUT).
transform_holds_3(HFDS,[P,A|ARGS],DBASE):- is_ftVar(P),!,DBASE=..[HFDS,P,A|ARGS].
transform_holds_3(HFDS, ['[|]'|ARGS],DBASE):- trace_or_throw(list_transform_holds_3(HFDS,['[|]'|ARGS],DBASE)).
transform_holds_3(Op,[SVOFunctor,Obj,Prop|ARGS],OUT):- is_svo_functor(SVOFunctor),!,transform_holds_3(Op,[Prop,Obj|ARGS],OUT).
transform_holds_3(Op,[P|ARGS],[P|ARGS]):- not(atom(P)),!,dmsg(transform_holds_3),trace_or_throw(transform_holds_3(Op,[P|ARGS],[P|ARGS])).
transform_holds_3(HFDS,[HOFDS,P,A|ARGS],OUT):- is_holds_true(HOFDS),!,transform_holds_3(HFDS,[P,A|ARGS],OUT).
transform_holds_3(HFDS,[HOFDS,P,A|ARGS],OUT):- HFDS==HOFDS, !, transform_holds_3(HFDS,[P,A|ARGS],OUT).
transform_holds_3(_,HOFDS,isa(I,C)) :- was_mpred_isa(HOFDS,I,C),!.
transform_holds_3(_,[Type,Inst],isa(Inst,Type)):-is_ftNonvar(Type),a(tCol,Type),!.
transform_holds_3(_,HOFDS,isa(I,C)):- holds_args(HOFDS,[ISA,I,C]),ISA==isa,!.

transform_holds_3(Op,[Fogical|ARGS],OUT):-  
         call(call,is_sentence_functor(Fogical)),!,sanity(not(is_svo_functor(Fogical))),
         must_det(foreach_arg(ARGS,1,ArgIn,ArgN,ArgOut,transform_functor_holds(Op,Fogical,ArgIn,ArgN,ArgOut),FARGS)),
         OUT=..[Fogical|FARGS].

transform_holds_3(_,[props,Obj,Props],props(Obj,Props)).
transform_holds_3(_,[Type,Inst|PROPS],props(Inst,[isa(Type)|PROPS])):- 
                  is_ftNonvar(Inst), not(Type=props), cheaply_u(tCol(Type)), must_det(\+(is_never_type(Type))),!.
transform_holds_3(_,[Type,Inst|PROPS],props(Inst,[isa(Type)|PROPS])):- 
                  is_ftNonvar(Inst), not(Type=props), t(functorDeclares,Type), must_det(\+(is_never_type(Type))),!.

transform_holds_3(_,[P,A|ARGS],DBASE):- atom(P),!,DBASE=..[P,A|ARGS].
transform_holds_3(Op,[P,A|ARGS],DBASE):- !, is_ftNonvar(P),dumpST,trace_or_throw(transform_holds_3(Op,[P,A|ARGS],DBASE)), DBASE=..[P,A|ARGS].
transform_holds_3(Op,DBASE_T,OUT):- DBASE_T=..[P,A|ARGS],!,transform_holds_3(Op,[P,A|ARGS],OUT).



%= 	 	 

%% holds_args( ?HOFDS, ?FIST) is semidet.
%
% Holds Arguments.
%
holds_args([H|FIST],FISTO):- !, is_holds_true(H),!,FIST=FISTO.
holds_args(HOFDS,FIST):- is_ftCompound(HOFDS),HOFDS=..[H|FIST],is_holds_true(H),!.


%% do_expand_args( ?Op, ?Term, ?Term) is semidet.
%
% Do Expand Arguments.
%
do_expand_args(_,Term,TermO):- \+ compound(Term),!,must(Term=TermO).
do_expand_args(Exp,M:Sent,M:SentO):- atom(M),!,do_expand_args(Exp,Sent,SentO).
do_expand_args(_,Term,Term):- functor(Term,F,_),cheaply_u(argsQuoted(F)),!.
do_expand_args(Exp,[L|IST],Out):- !,must(do_expand_args_l(Exp,[L|IST],Out)).
do_expand_args(Exp,Term,Out):- Term=..[P|ARGS],do_expand_args_pa(Exp,P,ARGS,Out).


%= 	 	 

%% do_expand_args_pa( ?Exp, ?P, ?ARGS, ?Out) is semidet.
%
% Do Expand Arguments Pa.
%

% allows ?- fully_expand(arity(isEach([X,TY,OO]),4),O).
do_expand_args_pa(Exp,Exp,[ARGS|Some],Out):- (Some==[]),is_list(ARGS),!,member(Out,ARGS).
% allows ?- fully_expand(arity(isEach(X,TY,OO),4),O).
do_expand_args_pa(Exp,Exp,ARGS,Out):- !,member(Out,ARGS).
do_expand_args_pa(Exp,P,ARGS,Out):- do_expand_args_l(Exp,ARGS,EARGS), Out=..[P|EARGS].


%= 	 	 

%% do_expand_args_l( +Op, :TermA, :TermA) is semidet.
%
% Do Expand Arguments (list Version).
%

% do_expand_args_l(Exp,ARGS,EARGS):- do_expand_args(Exp,A,E),do_expand_args_l(Exp,RGS,ARGS).

do_expand_args_l(_,A,A):- is_ftVar(A),!.
do_expand_args_l(Exp,[A|RGS],[E|ARGS]):- is_list(RGS),!,do_expand_args(Exp,A,E),do_expand_args_l(Exp,RGS,ARGS).
do_expand_args_l(_,A,A).



% :- mpred_trace_nochilds(functor_safe/2).
% :- mpred_trace_nochilds(functor_safe/3).


% ================================================
%  expand_goal_correct_argIsa/2
% ================================================

%= 	 	 

%% expands_on( ?EachOf, ?Term) is semidet.
%
% Expands Whenever.
%
%expands_on(EachOf,Term):-subst(Term,EachOf,foooz,Term2),!,Term2\=Term, \+ ((do_expand_args(EachOf,Term,O),O = Term)).

%= 	 	 

%% if_expands_on( ?EachOf, ?Term, ?Call) is semidet.
%
% If Expands Whenever.
%
%if_expands_on(EachOf,Term,Call):- expands_on(EachOf,Term),subst(Call,Term,O,OCall),!, forall(do_expand_args(EachOf,Term,O),OCall).

/*
%db_reop(WhatNot,Call) :- into_mpred_form(Call,NewCall),NewCall\=@=Call,!,db_reop(WhatNot,NewCall).
db_reop(Op,Term):- expands_on(isEach,Term), !,forall(do_expand_args(isEach,Term,O),db_reop_l(Op,O)).
db_reop(Op,Term):-db_reop_l(Op,Term).

db_reop_l(query(_HLDS,Must),Call) :- !,preq(Must,Call).
db_reop_l(Op,DATA):-no_loop_check(db_op0(Op,DATA)).

 dm sg_hook(transform_holds(t,_What,props(ttSpatialType,[isa(isa),isa]))):-trace_or_throw(dtrace).

*/


% expand_goal_correct_argIsa(A,A):-simple_code,!.

%= 	 	 

%% expand_goal_correct_argIsa( ?A, ?B) is semidet.
%
% expand goal correct Argument  (isa/2).
%
expand_goal_correct_argIsa(A,B):- expand_goal(A,B).

% db_op_simpler(query(HLDS,_),MODULE:C0,call_u(call,MODULE:C0)):- atom(MODULE), is_ftNonvar(C0),not(not(predicate_property(C0,_PP))),!. % , functor_catch(C0,F,A), dmsg(todo(unmodulize(F/A))), %trace_or_throw(module_form(MODULE:C0)), %   db_op(Op,C0).

%= 	 	 

%% db_op_simpler( +Op, ?VALUE2, :TermARG3) is semidet.
%
% Database Oper. Simpler.
%
db_op_simpler(Op,Sent,SentO):- call_last_is_var(db_op_simpler(Op,Sent,SentO)).

db_op_simpler(_,TypeTerm,props(Inst,[isa(Type)|PROPS])):- TypeTerm=..[Type,Inst|PROPS],is_ftNonvar(Inst),t(functorDeclares,Type),!.



%= 	 	 

%% db_op_sentence( ?Op, ?Prop, ?ARGS, ?C0) is semidet.
%
% Database Oper. Sentence.
%
db_op_sentence(_Op,Prop,ARGS,C0):- atom(Prop),!, C0=..[Prop|ARGS].
db_op_sentence(_Op,Prop,ARGS,C0):- C0=..[t,Prop|ARGS].


%=  :- was_export(simply_functors/3).

%= 	 	 

%% simply_functors( :PRED2Db_pred, ?Op, ?Wild) is semidet.
%
% Simply Functors.
%
simply_functors(Db_pred,query(HLDS,Must),Wild):- once(into_mpred_form(Wild,Simpler)),Wild\=@=Simpler,!,call(Db_pred,query(HLDS,Must),Simpler).
simply_functors(Db_pred,Op,Wild):- once(into_mpred_form(Wild,Simpler)),Wild\=@=Simpler,!,call(Db_pred,Op,Simpler).


% -  dmsg_hook(db_op(query(HLDS,call),holds_t(ft_info,tCol,'$VAR'(_)))):-trace_or_throw(dtrace).

fixed_negations(I,O):- fix_negations(I,O)->I\=@=O.
fix_negations(P0,P0):- not_ftCompound(P0),!.
fix_negations(~(P0),~(P0)):- not_ftCompound(P0),!.
fix_negations(\+(P0),\+(P0)):- not_ftCompound(P0),!.
fix_negations(~(~I),O):- !, fix_negations(\+(~I),O).
fix_negations(~not(I),O):- !, fix_negations(\+(~I),O).
fix_negations(~~(I),O):- functor(~~(I),~~,1),!, fix_negations(\+(~I),O).
fix_negations(not(I),O):- !, fix_negations(\+(I),O).
fix_negations(~(I),~(O)):- !, fix_negations(I,O).
fix_negations(\+(I),\+(O)):- !, fix_negations(I,O).
fix_negations(C,C):- if_defined(exact_args(C),fail),!.
fix_negations([H|T],[HH|TT]):-!,fix_negations(H,HH),fix_negations(T,TT),!.
fix_negations(C,CO):-C=..[F|CL],must_maplist(fix_negations,CL,CLO),!,CO=..[F|CLO].


%% reduce_clause_from_fwd(Op, +H, ?H) is semidet.
%
% Reduce Clause Converted From Forward Repropigated.
%
reduce_clause_from_fwd(_Op,H,H):- (\+is_ftCompound(H)),!.
reduce_clause_from_fwd(Op,(H:-B),HH):-B==true,reduce_clause_from_fwd(Op,H,HH).
reduce_clause_from_fwd(Op,(B==>H),HH):-B==true,reduce_clause_from_fwd(Op,H,HH).
reduce_clause_from_fwd(Op,I,O):- fixed_negations(I,M),reduce_clause_from_fwd(Op,M,O).
reduce_clause_from_fwd(Op,(==>H),HH):-!,reduce_clause_from_fwd(Op,H,HH).
reduce_clause_from_fwd(Op,(H<- B),HH):-B==true,reduce_clause_from_fwd(Op,H,HH).
reduce_clause_from_fwd(Op,(B<==> H),HH):-B==true,reduce_clause_from_fwd(Op,'==>'(H),HH).
reduce_clause_from_fwd(Op,(H<==> B),HH):-B==true,reduce_clause_from_fwd(Op,H,HH).
reduce_clause_from_fwd(Op,(H,B),(HH,BB)):-!,reduce_clause_from_fwd(Op,H,HH),reduce_clause_from_fwd(Op,B,BB).
reduce_clause_from_fwd(_Op,H,H).



%% append_as_first_arg( +C, ?I, ?V) is semidet.
%
% Append Converted To First Argument.
%
append_as_first_arg(C,I,V):-C=..[F|ARGS],V=..[F,I|ARGS].



%% to_predicate_isas( :TermV, :TermV) is semidet.
%
% Converted To Predicate Isas.
%
to_predicate_isas(V,V):- (\+is_ftCompound(V)),!.
to_predicate_isas({V},{V}):-!.
% to_predicate_isas(eXact(V),V):-!.
to_predicate_isas([H|T],[HH|TT]):-!,to_predicate_isas(H,HH),to_predicate_isas(T,TT),!.
to_predicate_isas((H,T),(HH,TT)):-!,to_predicate_isas(H,HH),to_predicate_isas(T,TT),!.
%to_predicate_isas(I,I):-contains_term(S,I),is_ftNonvar(S),exact_args(S),!.
to_predicate_isas(t(C,I),V):-atom(C)->V=..[C,I];(is_ftVar(C)->V=t(C,I);append_as_first_arg(C,I,V)).
to_predicate_isas(isa(I,C),V):-!,(atom(C)->V=..[C,I];(is_ftVar(C)->V=isa(I,C);append_as_first_arg(C,I,V))).
to_predicate_isas(C,C):- exact_args(C),!.
to_predicate_isas(C,CO):-C=..[F|CL],must_maplist(to_predicate_isas,CL,CLO),!,CO=..[F|CLO].

:- source_location(F,_),asserta(absolute_source_location_pfc(F)).

%% exact_args( +Q) is semidet.
%
% Exact Arguments.
%
exact_args(Q):-is_ftVar(Q),!,fail.
exact_args(isEach):-!,fail.
exact_args(Q):- \+ compound(Q), !.
exact_args(_:Q):- !,loop_check(exact_args0(Q),fail).
exact_args(Q):- exact_args0(Q),!.


%exact_args0(cwc).
%exact_args0(true).
exact_args0(Q):- \+ compound(Q), !.
exact_args0(Q):- Q=..[_,A],atomic(A),!.
exact_args0(argsQuoted(_)):-!,fail.
exact_args0(==>(_,_)):-!,fail.
exact_args0(mtExact(_)).
exact_args0(second_order(_,_)).
exact_args0(call(_)).
exact_args0(asserted(_)).
exact_args0(retract_eq_quitely(_)).
exact_args0(asserts_eq_quitely(_)).
exact_args0(assertz_if_new(_)).
exact_args0(mpred_ain(_)).
exact_args0(dynamic(_)).
exact_args0((A/B)):- (is_ftVar(A);is_ftVar(B)),!.
exact_args0((_ =.. _)).
exact_args0(Q):- is_ftCompound(Q),functor(Q,F,_),!,lookup_u(argsQuoted(F)),!.
exact_args0(Q):- clause(argsQuoted(Q),Body),compound(Body),call(Body),!.

% exact_args((_:-_)).
% exact_args((:-( _))).
% exact_args(C):-source_file(C,I),absolute_source_location_pfc(I).


:- module_transparent(is_stripped_module/1).

% these do not get defined!?%= :- shared_multifile user_db:assert_user/2, user_db:grant_openid_server/2, user_db:retractall_grant_openid_server/2, user_db:retractall_user/2, user_db:assert_grant_openid_server/2.

:- export(mpred_expansion_file/0).
mpred_expansion_file.


