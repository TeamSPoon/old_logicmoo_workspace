/** <module> mpred_type_wff
% Provides a common set of operators in translation between the several logical languages
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_wff.pl
:- module(mpred_type_wff,
          [ 
            head_singletons/2, head_singles0/2,head_singles01/2,
            append_termlist/3,            
            call_last_is_var/1,

            contains_negs/1,
            contains_no_negs/1,
            contains_t_var/3,
            contains_type_lits/3,
            contains_var_lits/3,
            correct_negations/3,
            current_hilog/1,
            defunctionalize/2,
            defunctionalize/3,
            ensure_quantifiers/2,
            function_to_predicate/3,
            get_isa/3,
            get_isa0/3,
            get_kv/3,
            get_pred/2,
            hilog_functor/1,

            isBodyConnective/1,
            isEntityFunction/3,
            isEntitySlot/1,
            isEntityref/2,
            isHiddenSlot/1,
            isLiteralTerm/1,
            isLiteralTerm_util/1,
            isNonCompound/1,
            isNonVar/1,
            isObject/2,
            isQualifiableAs/3,
            isQualifiableAsClass/1,
            isQualifiedAndKnownAs/3,
            isQualifiedAndVarAndUnifiable/3,
            isQualifiedAndVarAs/3,
            isQualifiedAs/3,
            isQualifiedAs/4,
            isSlot/1,
            isSlot/2,
            isVarObject/1,
            isVarObject/2,
            isVarProlog/1,
            is_2nd_order_holds/1,
            is_colection_name/3,
            is_ftEquality/1,
            is_function/1,
            is_function/3,
            is_gaf/1,
            is_holds_false/1,
            is_holds_false0/1,
            is_holds_true/1,
            is_holds_true0/1,
            is_holds_true_not_hilog/1,
            is_kif_rule/1,
            is_log_op/1,
            is_log_sent/1,
            is_logical_functor/1,
            is_logical_functor0/1,
            is_modal/2,
            is_neg/1,
            is_pos/1,
            is_sentence_functor/1,
            is_svo_functor/1,
            kb_nlit/2,
            lastImproperMember/3,
            leave_as_is/1,
            leave_as_is_db/1,
            leave_as_is_functor/1,
            logical_functor_ft/1,            
            non_assertable/2,
            non_compound/1,
            not_log_op/1,
            prequent/1,
            put_singles/4,
            set_is_lit/1,
            subst_except/4,
            term_singletons/2,
            term_singletons/3,
            term_singletons/5,
            term_slots/2,
            wrap_in_neg_functor/3            
          ]).
:- meta_predicate 
        call_last_is_var(0).


:- use_module(logicmoo(mpred/mpred_expansion)).
:- include('mpred_header.pi').



subst_except(  Var, VarS,SUB,SUB ) :- Var==VarS,!.
subst_except(  Var, _,_,Var ) :- \+compound(Var),!.
subst_except(  Var, _,_,Var ) :- leave_as_is(Var),!.
subst_except([H|T],B,A,[HH|TT]):- !,
   subst_except(H,B,A,HH),
   subst_except(T,B,A,TT).
subst_except(HT,B,A,HHTT):- HT=..FARGS,subst_except(FARGS,B,A,[FM|MARGS]),
   (atom(FM)->HHTT=..[FM|MARGS];append_termlist(FM,MARGS,HHTT)).

append_termlist(Call,EList,CallE):-must((compound(Call),is_list(EList))), Call=..LeftSide, append(LeftSide,EList,ListE), CallE=..ListE.


% ========================================
% Logic Preds Shared
% ========================================

:- was_export(is_svo_functor/1).
is_svo_functor(Prop):- hotrace((atom(Prop),arg(_,svo(svo,prop,valueOf,rdf),Prop))).

:- was_export(hilog_functor/1).
hilog_functor(true_t).

:- was_export(is_holds_true_not_hilog/1).
is_holds_true_not_hilog(HOFDS):-is_holds_true(HOFDS),\+ hilog_functor(HOFDS).

:- was_export(is_holds_true/1).
is_holds_true(Prop):- hotrace((atom(Prop),is_holds_true0(Prop))),!.

% k,p,..
is_holds_true0(Prop):-arg(_,vvv(holds,holds_t,t,t,asserted_mpred_t,assertion_t,true_t,assertion,secondOrder,firstOrder),Prop).
% is_holds_true0(Prop):-atom_concat(_,'_t',Prop).

:- was_export(is_2nd_order_holds/1).
is_2nd_order_holds(Prop):- is_holds_true(Prop) ; is_holds_false(Prop).

:- was_export(is_holds_false/1).
is_holds_false(Prop):-hotrace((atom(Prop),is_holds_false0(Prop))).

is_holds_false0(Prop):-member(Prop,[not,nholds,holds_f,mpred_f,aint,assertion_f,not_true_t,asserted_mpred_f,retraction,not_secondOrder,not_firstOrder]).
%is_holds_false0(Prop,Stem):-atom_concat('not_',Stem,Prop).
%is_holds_false0(Prop,Stem):-atom_concat('int_not_',Stem,Prop).
%is_holds_false0(Prop,Stem):-atom_concat(Stem,'_f',Prop).
%is_holds_false0(Prop):-is_holds_false0(Prop,Stem),is_holds_true0(Stem).
%is_holds_false0(Prop,Stem):-atom_concat(Stem,'_not',Prop).
%is_holds_false0(Prop,Stem):-atom_concat(Stem,'_false',Prop).


:- thread_local t_l:override_hilog/1.

current_hilog(Dbase_t):- t_l:override_hilog(Dbase_t),!.
current_hilog(t).


% ===================================================================
% EXPORTS
% ===================================================================
isNonVar(Denotation):-not(isSlot(Denotation)).

% ===============================================================================================
% ===============================================================================================

:- if(\+ current_predicate(isSlot/1)).
isSlot(Denotation):-((isVarProlog(Denotation);isVarObject(Denotation))),!.
:- endif.

isSlot(Denotation,Denotation):- isVarProlog(Denotation),!.
isSlot(Denotation,PrologVar):- isVarObject(Denotation,PrologVar),!.

% ===============================================================================================
% ===============================================================================================

isHiddenSlot(_Term):-fail.

% ===============================================================================================
% ===============================================================================================

isVarProlog(A):-((var(A);A='$VAR'(_))).

% ===============================================================================================
% ===============================================================================================

isVarObject(Denotation):-((
		  isObject(Denotation,_BaseType),
		  arg(1,Denotation,Value),!,isSlot(Value))).

isVarObject(Denotation,Value):-((
		  isObject(Denotation,_BaseType),
		  arg(1,Denotation,Value),!,isSlot(Value))).

% ===============================================================================================
% ===============================================================================================
	
isObject(Denotation,BaseType):-
	(((atom(BaseType) ->
		  (atom_concat('$',BaseType,F),functor(Denotation,F,2));
		  (functor(Denotation,F,2),atom_concat('$',BaseType,F))
		 ),!)).

% ===============================================================================================
% ===============================================================================================

isQualifiableAsClass(Atom):-atom(Atom),!.
isQualifiableAsClass('$Class'(Atom,_)):-atom(Atom),!.

isQualifiableAs(Denotation,BaseType,Value):-
		  isObject(Denotation,BaseType),
		  arg(1,Denotation,Value).

% ===============================================================================================
% ===============================================================================================

isQualifiedAs(Denotation,_,_):-not(compound(Denotation)),!,fail.
isQualifiedAs(Denotation,BaseType,Value):-
		  isQualifiedAs(Denotation,BaseType,Value,_SubType).
isQualifiedAs(Denotation,BaseType,Value,SubType):-
		  isObject(Denotation,BaseType),
		  arg(1,Denotation,Value),
		  arg(2,Denotation,List),
		  lastImproperMember(BaseType,SubType,List).

% ===============================================================================================
% ===============================================================================================
:- style_check(-singleton).

lastImproperMember(Default,Default,List):-isVarProlog(List),!.
lastImproperMember(Default,Default,[]):-!.
lastImproperMember(Default,SubType,List):-proper_list(List),last(SubType,List).
lastImproperMember(Default,SubType,[SubType|End]):-isVarProlog(End),!.
lastImproperMember(Default,SubType,[_|Rest]):-
	lastImproperMember(Default,SubType,Rest),!.
	
% ===============================================================================================
% ===============================================================================================

isQualifiedAndKnownAs(Denotation,BaseType,Value):-
		  isQualifiedAs(Denotation,BaseType,Value),!,
		  not(isVarProlog(Value)).

% ===============================================================================================
% ===============================================================================================

isQualifiedAndVarAs(Denotation,BaseType,Value):-
		  isQualifiedAs(Denotation,BaseType,Value),!,
		  isVarProlog(Value).

% ===============================================================================================
% ===============================================================================================

isQualifiedAndVarAndUnifiable(Denotation,BaseType,NValue):-
		  isQualifiedAs(Denotation,BaseType,Value),!,
		  (isVarProlog(Value);
		  (\+ \+ NValue=Value)),!.

% ===============================================================================================
% ===============================================================================================

isBodyConnective(Funct):-atom_concat(_,'_',Funct),!.
isBodyConnective(Funct):-atom_concat('t~',_,Funct),!.
isBodyConnective(Funct):-atom_concat('f~',_,Funct),!.
isBodyConnective(Funct):-member(Funct,[and,or,until,',',';',':-',unless,xor,holdsDuring]). % Other Propositional Wrhtml_appers

isEntityref(Var,Var):-isSlot(Var),!.
isEntityref(Term,A):-Term=..[F,A,B],!,atom_concat('$',_,F),!.


% ===============================================================================================
% ===============================================================================================

isLiteralTerm(A):-isLiteralTerm_util(A),!.
isLiteralTerm(not(A)):-isLiteralTerm_util(A),!.

isLiteralTerm_util(A):-var(A),!.
isLiteralTerm_util('$VAR'(_)):-!.
isLiteralTerm_util(string(_)):-!.
isLiteralTerm_util(A):-not(compound(A)),!.
isLiteralTerm_util(A):-string(A).

% ===============================================================================================
% ===============================================================================================

isEntitySlot(Term):-isSlot(Term),!.
isEntitySlot(Term):-not(compound(Term)),!.
isEntitySlot(Term):-isEntityFunction(Term,FnT,ArgsT),!.

% ===============================================================================================
% ===============================================================================================

isEntityFunction(Term,FnT,ArgsT):-isSlot(Term),!,fail.
isEntityFunction(Term,FnT,ArgsT):-atomic(Term),!,fail.
isEntityFunction(Term,FnT,ArgsT):-Term=..[FnT|ArgsT],isa(FnT,'Function'),!.

% ===============================================================================================
% ===============================================================================================

isNonCompound(Var):-isSlot(Var),!.
isNonCompound(Var):-not(compound(Var)),!.
isNonCompound(svar(_,_)):-!.
isNonCompound(Var):-is_ftText(Var),!.
isNonCompound(string(Var)):-!.

% ===============================================================================================
% ===============================================================================================

logical_functor_ft(F):-is_logical_functor(F).
logical_functor_ft((':-')).
logical_functor_ft((',')).

% ===============================================================================================
% ===============================================================================================

non_assertable(WW,isVar(WW)):- var(WW),!.
non_assertable(_:WW,Why):- !,non_assertable(WW,Why).
non_assertable(WW,notAssertable(Why)):- compound(WW),get_functor(WW,F),mpred_isa(F,notAssertable(Why)),!.
% non_assertable(WW,Why):- db_prop_add

% ===============================================================================================
% ===============================================================================================

is_logical_functor(And):-hotrace(is_logical_functor0(And)).
is_logical_functor0(X):-atom(X),member(X,[',',';',xor,'\\+',~]).
is_logical_functor0(X):-call_if_defined(logical_functor_pttp(X)).
is_logical_functor0(And):-member(And,[(,),(;),('<-'),('=>'),('<=>'),(':-'),(and),nop]).

% ===============================================================================================
% ===============================================================================================

is_neg(not(_)).
is_pos(One):- get_functor(One,F),!,not(is_log_op(F)).

:- was_export(is_log_sent/1).
is_log_sent(S):- get_functor(S,F,_),is_log_op(F).

not_log_op(OP):- not(is_log_op(OP)).
:- was_export(is_log_op/1).
is_log_op(OP):- atomic(OP),to_dlog_ops(OPS),!,(member(OP=_,OPS);member(_=OP,OPS)).

% % :- use_module(logicmoo(plarkc/mpred_kif)).

put_singles(Wff,_,[],Wff).
put_singles(Wff,Exists,[S|Singles],NewWff):-   
   (((each_subterm(Wff,SubTerm),compound(SubTerm),
    SubTerm=..[OtherExists,SO,_],same_var(SO,S),
     member(OtherExists,[all,exists])))
 -> WffM = Wff ; WffM =..[Exists,S,Wff]),
   put_singles(WffM,Exists,Singles,NewWff),!.


:- meta_predicate(call_last_is_var(0)).
call_last_is_var(MCall):- strip_module(MCall,M,Call),
   must((compound(Call),functor(Call,_,A))),
   arg(A,Call,Last),nonvar(Last),Call=..FArgs,
   append(Left,[Last],FArgs),append(Left,[IsVar],NFArgs),NewCall=..NFArgs,!,M:NewCall*->IsVar=Last;fail.

   


:- was_export(defunctionalize/2).
defunctionalize(Wff,WffO):- w_tl(t_l:dont_use_mudEquals,defunctionalize(',',Wff,WffO)).
defunctionalize(OP,Wff,WffO):- call_last_is_var(defunctionalize(OP,Wff,WffO)).
defunctionalize(_ ,Wff,Wff):- \+ compound(Wff),!.
defunctionalize(_ ,Wff,Wff):- non_compound(Wff),!.
defunctionalize(_ ,Wff,Wff):- leave_as_is(Wff),!.

defunctionalize(OP,(H:-B),WffO):- t_l:dont_use_mudEquals,!,
 wno_tl(t_l:dont_use_mudEquals,defunctionalize(OP,(H:-B),WffO)).
defunctionalize(OP,(H:-B),WffO):- !,
  defunctionalize(',',(B=>H),HH),
  (HH=(PreC,(NewBody=>NEWH))-> 
     defunctionalize(OP,(NEWH:not(PreC,NewBody)),WffO);
  (defunctionalize(OP,B,NewBody),WffO=(H:-NewBody))).
  
defunctionalize(OP,Wff,WffO):- 
  each_subterm(Wff,SubTerm),
  compound(SubTerm),
  \+ (is_ftEquality(SubTerm)),
  \+ (leave_as_is(SubTerm)),
  arg(_,SubTerm,Function),
  is_function(Function),
  \+ (leave_as_is(Function)),
  subst_except(SubTerm,Function,NewVar,NewSubTerm),
  function_to_predicate(Function,NewVar,PredifiedFunction),
  subst_except(Wff,SubTerm,NewSubTerm,NextWff),!,
  defunctionalize(OP,NextWff,WffM),!,
  WffO=..[OP,PredifiedFunction,WffM].

defunctionalize(OP,Wff,WffO):-
  each_subterm(Wff,SubTerm),
  compound(SubTerm),
  not(is_ftEquality(SubTerm)),
  not(leave_as_is(SubTerm)),
  arg(_,SubTerm,Function),is_function(Function),
  subst_except(SubTerm,Function,NewVar,NewSubTerm),
  function_to_predicate(Function,NewVar,PredifiedFunction),
  NEW =..[OP,PredifiedFunction,NewSubTerm],
  subst_except(Wff,SubTerm,NEW,NextWff),!,
  defunctionalize(OP,NextWff,WffO),!.
defunctionalize(_,Wff,Wff).


correct_negations(Op,(~({X})),O):-nonvar(X),wrap_in_neg_functor(Op,X,O).
correct_negations(Op,(-({X})),O):-nonvar(X),wrap_in_neg_functor(Op,X,O).
correct_negations(Op,(not({X})),O):-nonvar(X),wrap_in_neg_functor(Op,X,O).
correct_negations(Op,(notz({X})),O):-nonvar(X),wrap_in_neg_functor(Op,X,O).
correct_negations(Op,(assertable_not({X})),O):-nonvar(X),wrap_in_neg_functor(Op,X,O).
correct_negations(Op,(\+({X})),O):-nonvar(X),wrap_in_neg_functor(Op,X,O).

wrap_in_neg_functor(clause,X,assertable_neg(X)).
wrap_in_neg_functor(mpred,X,not(X)).
wrap_in_neg_functor(callable,X, (\+(X))).



contains_no_negs(X):- \+ contains_negs(X).

contains_negs(X):-sub_term(Sub, X),compound(Sub),Sub=not(_).


is_modal(MODAL,_):- \+ compound(MODAL),!,fail.
is_modal(MODAL,BDT):- (MODAL = nesc(BDT,_) ; MODAL = poss(BDT,_)),!,nonvar(BDT).
is_modal(MODAL,BDT):- arg(_,MODAL,ARG),is_modal(ARG,BDT).

contains_var_lits(Fml,Var,Lits):- findall(Lit,contains_t_var(Fml,Var,Lit),Lits).

contains_type_lits(Fml,Var,Lits):- findall(T,(contains_t_var(Fml,Var,Lit),get_isa(Lit,O,T),same_var(O,Var)),Lits).
contains_t_var(Fml,Var,Term):- each_subterm(Fml,Term),compound(Term),arg(_,Term,O),same_var(O,Var).


get_isa(Lit,I,TT):- compound(Lit),get_isa0(Lit,I,TT).
get_isa0(isa(I,T),I,TT):- to_iname(T,TT),!.
get_isa0(IT,I,TT):- IT=..[T,I],is_colection_name(IT,T,TT),!.

is_colection_name(_,-,_):- !,fail.
is_colection_name(IT,T,TT):- atom_length(T,TL),TL>2,not(atom_contains(T,'_')),not(predicate_property(IT,_)),to_iname(T,TT).


is_sentence_functor(&).
is_sentence_functor(v).
is_sentence_functor(exists).
is_sentence_functor(all).


leave_as_is(V):- \+ compound(V),!.
leave_as_is((_ :-_ )):-!,fail.
leave_as_is((_;_)):-!,fail.
leave_as_is((_/_)):-!,fail.
leave_as_is(V):-compound(V),leave_as_is_db(V),!.

leave_as_is_db('$VAR'(_)).
leave_as_is_db('aNARTFn'(_)).
leave_as_is_db('comment'(_,_)).

leave_as_is_db(C):-get_functor(C,F),leave_as_is_functor(F).
leave_as_is_db(infer_by(_)).
leave_as_is_db(b_d(_,_,_)).
leave_as_is_db(ct(_,_)).
% leave_as_is_db('CollectionSubsetFn'(_,_)).
leave_as_is_db(ignore(_)).
leave_as_is_db(isa(_,_)).
leave_as_is_db(P):-prequent(P).


leave_as_is_functor(Atom):- \+ atom(Atom),!,fail.
leave_as_is_functor('TINYKB-ASSERTION').
leave_as_is_functor('skolem').
leave_as_is_functor('$VAR').
leave_as_is_functor('kbMark').
leave_as_is_functor('z_unused').
leave_as_is_functor('genlMt').
leave_as_is_functor('{}').
leave_as_is_functor(F):-if_defined_else(ptReformulatorDirectivePredicate(F),fail).

prequent(original(_)).
prequent(mudEquals(_,_)).
prequent(skolem(_,_)).
prequent(different(_,_)).
prequent(argInst(_,_,_)).
prequent(G):-functor(G,call_builtin,_).
prequent(G):-functor(G,not_call_builtin,_).

kb_nlit(_KB,Neg):-member(Neg,[(not),(~),(-),(~)]).

set_is_lit(A):-when(nonvar(A),\+ is_ftVar(A)),!.

non_compound(InOut):- once( \+ (compound(InOut));is_ftVar(InOut)).

is_gaf(Gaf):-when(nonvar(Gaf), \+ (is_kif_rule(Gaf))).

:- was_export(is_kif_rule/1).
is_kif_rule(Var):- is_ftVar(Var),!,fail.
is_kif_rule(R):- kif_hook(R),!.


:- was_export(term_slots/2).
term_slots(Term,Slots):-term_singletons(Term, [],NS, [],S),append(NS,S,Slots).


:- export(head_singletons/2).
%head_singletons(Pre,Post):-  !, hotrace((\+ ignore(must( \+ head_singles0(Pre,Post))))).
head_singletons(Pre,Post):-   hotrace((\+ ignore(show_failure(why, \+ head_singles0(Pre,Post))))),!,dumpST.
:- export(head_singles0/2).
:- export(head_singles01/2).
% TODO how to adderess head_singles0(true, if_missing(foob(_G754993), foob(a)))?
% also should fix  (head_singles0(true, nt(umt, foob(_G764659),  (call_u(foob(_G764666)), foob(_G764666)\==foob(a)), rhs([foob(a)]))))).
head_singles0(Pre,Post):-is_ftVar(Post),!,head_singles01(Pre,Post).
head_singles0(_,Post):- \+ compound(Post),!,fail.
head_singles0(Pre,M:Post):-atom(M),!,head_singles0(Pre,Post).
head_singles0(Pre,[Post|More]):-nonvar(Post),!,head_singles0(Pre,Post),head_singles0((Pre,Post),More).
head_singles0(Pre,'if_missing'(_,Post)):-nonvar(Post),!,head_singles0((Pre,Pre2),Post).
head_singles0(Pre,'->'(Pre2,Post)):-nonvar(Post),!,head_singles0((Pre,Pre2),Post).
head_singles0(Pre,'/'(Post,Pre2)):-nonvar(Post),!,head_singles0((Pre,Pre2),Post).
head_singles0(Pre,rhs(Post)):- nonvar(Post),mpred_rule_hb(Post,Post2,Pre2), !,head_singles0((Pre,Pre2),Post2).
head_singles0(Pre,mpred_default(Post)):- nonvar(Post),mpred_rule_hb(Post,Post2,Pre2), !,head_singles0((Pre,Pre2),Post2).
head_singles0(Pre,nt(_,Pre2,Pre3,Post)):-nonvar(Post),!,head_singles0((Pre,Pre2,Pre3),Post).
head_singles0(Pre,pt(_,Pre2,Post)):-nonvar(Post),!,head_singles0((Pre,Pre2),Post).
head_singles0(Pre,Post):- nonvar(Post),mpred_rule_hb(Post,Post2,Pre2),Post2\=@=Post,!,head_singles0((Pre,Pre2),Post2).
head_singles0(Pre,Post):-head_singles01(Pre,Post).

head_singles01(Pre,Post):-
    term_singletons(Post,_,CSingles),
    term_slots(Pre,PreVars),!,
    subtract_eq(CSingles,PreVars,Bad),!,Bad\==[].
    

:- was_export(term_singletons/2).
term_singletons(A,Vs):- notrace(term_singletons(A,[],_,[],Vs)).
:- was_export(term_singletons/3).
term_singletons(Term,NonSingle,Singles):- notrace(term_singletons(Term,[],NonSingle,[],Singles)).
:- was_export(term_singletons/5).
term_singletons(Fml, NS,NS, S,S):- atomic(Fml),!.
term_singletons(Fml, NS,NS, S,S):- identical_member(Fml,NS),!.
term_singletons(Fml, NS, [Fml|NS], S, NSV):- is_ftVar(Fml),identical_member(Fml,S),!,delete_eq(S,Fml,NSV),!.
term_singletons(Fml, NS, NS, S, [Fml|S]):- is_ftVar(Fml),!.
term_singletons([H|T],NS,NSO,S,NSV):- !, term_singletons(H,NS,NSM,S,M),term_singletons(T,NSM,NSO,M,NSV).
term_singletons(Fml, NS,NSO, S,NSV):- compound(Fml),Fml=..[_|T],!, term_singletons(T, NS,NSO, S,NSV).

get_kv(X=Y,X,Y):- !.
get_kv(X-Y,X,Y):- !.
get_kv(KV,X,Y):- functor(KV,_,1),KV=..[X,Y],!.
get_kv(KV,X,Y):- arg(1,KV,X),arg(2,KV,Y),!.


is_function(F):- is_ftVar(F),!,fail.
is_function(Function):- compound(Function),get_functor(Function,F,A),is_function(Function,F,A).



is_function(_,'SubLQuoteFn',_):- !,fail.
is_function(_,'aQuoteFn',_):- !,fail.
is_function(_,'aNARTFn',_):- !,fail.
is_function(_,'CollectionSubsetFn',_).
is_function(_,'aCollectionSubsetFn',_).
is_function(_,F,_):- atom_concat('sk',_Was,F),!,fail.
is_function(P,_,_):- leave_as_is(P),!,fail.
is_function(_,F,_):- is_log_op(F),!,fail.
is_function(_,F,_):- atom_concat(_Was,'Fn',F).
is_function(_,F,_):- tFunction(F).
% is_function(_,F,A):- A2 is A+1, current_predicate(F/A2), \+ current_predicate(F/A).

%:- ain(isa(I,C)<=(ttPredType(C),lmconf:isa(I,C))).

is_ftEquality(Term):- is_ftVar(Term),!,fail.
%is_ftEquality(Term):- get_pred(Term,Pred),is),!,(Pred==mudEquals;genlPreds(Pred,equals);clause_asserted(prologEquality(Pred))),!.
is_ftEquality(mudEquals(_,_)).
is_ftEquality(skolem(_,_)).
is_ftEquality(equals(_,_)).
is_ftEquality(termOfUnit(_,_)).

:- thread_local(t_l:dont_use_mudEquals/0).


ensure_quantifiers(Wff:- B,WffO):- B== true,!, ensure_quantifiers(Wff,WffO).
ensure_quantifiers(Wff:- B,Wff:- B):- !.
% ensure_quantifiers(Wff,Wff):-!.
ensure_quantifiers(Wff,WffO):-
 must_det_l((show_failure(why,term_singletons(Wff,[],NS,[],Singles)),
  put_singles(Wff,'all',Singles,WffM),put_singles(WffM,'all',NS,WffO))).

:- was_shared_multifile(function_corisponding_predicate/2).
:- was_dynamic(function_corisponding_predicate/2).

get_pred(Pred,F):- get_functor(Pred,F).




function_to_predicate(Function,NewVar,PredifiedFunction):- 
 Function = 'CollectionSubsetFn'(Col,'TheSetOf'(NewVar,Formulas)), 
 must(is_ftVar(NewVar)), % \+ is_ftVar(Col),!,
 PredifiedFunction = (isa(NewVar,Col) & Formulas).

function_to_predicate(Function,NewVar,PredifiedFunction):- 
  Function=..[F|ARGS],
  function_corisponding_predicate(F,P),
  fresh_varname(Function,NewVar),
  PredifiedFunction=..[P,NewVar|ARGS],!.

function_to_predicate(Function,NewVar,mudEquals(NewVar,Function)):- \+ t_l:dont_use_mudEquals, fresh_varname(Function,NewVar),!.


:- source_location(S,_),forall(source_file(H,S),(functor(H,F,A),export(F/A),module_transparent(F/A))).

