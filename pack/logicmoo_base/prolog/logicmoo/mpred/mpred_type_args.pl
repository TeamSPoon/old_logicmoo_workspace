% =======================================================
/* 
% This is mainly used by the moo_loader but also needed everywhere
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
% =======================================================
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_args.pl
%:- if(((current_prolog_flag(xref,true),current_prolog_flag(pldoc_x,true));current_prolog_flag(autoload_logicmoo,true))).
:- module(mpred_type_args,
          [ any_to_number/2,
            any_to_relation/2,
            any_to_value/2,
            argIsa_call_0/3,
            argIsa_call_3/2,
            argIsa_call_6/3,
            argIsa_call_7/3,
            argIsa_call_9/3,
            argIsa_ft/3,
            argIsa_known/3,
            argIsa_op_call/4,
            argisa_nodebug/0,
            as_one_of/2,
            assert_argIsa/3,
            assert_predArgTypes/1,
            assert_predArgTypes_fa/2,
            assert_predArgTypes_from_left/3,
            assert_predArgTypes_from_right/3,
            assert_predArgTypes_l/3,
            asserted_argIsa_known/3,
            atom_to_value/2,
            checkAnyType/4,
            % coerce/4,
            correctAnyType/4,
            correctAnyTypeOrFail/4,
            correctArgsIsa/2,
            correctArgsIsa/3,
            correctArgsIsa/4,
            correctArgsIsa0/3,
            correctArgsIsa00/3,
            correctFormatType/4,
            correctType/4,
            correctType0/4,
            correctTypeArg/4,
            correctType_gripe/4,
            % decl_coerce/3,
            %deduceFromArgTypes/1,
            deduced_is_tCol/1,
            discoverAndCorrectArgsIsa/5,
            discoverAndCorrectArgsIsa_from_left/5,
            discoverAndCorrectArgsIsa_from_right/5,
            evaluatableArg/2,
            evaluatableFunctor/1,
            grab_argsIsa/2,
            grab_argsIsa_6/1,
            % hook_coerce/3,
            is_boolean/1,
            is_declarations/1,
            is_ephemeral/1,
            is_ftText/1,
            is_id/1,
            is_list_of/2,
            is_renamed_to/2,
            is_rest/1,
            is_rest_of/2,
            is_spec/1,
            is_valuespec/1,
            list_to_callform/3,
            maybe_argtypes/1,
            mpred_arity_pred/1,
            mpred_full_arity/2,
            must_equals/2,
            must_equals_correct/3,
            pl_arg_type/2,
            roll_dice/4,
            term_is_ft/2,
            term_is_ft_how/2,
            to_format_type/2,
            trans_subft/2,
            mpred_type_args_file/0
          ]).
%:- endif.
% autoloading user:portray_clause_pi/2 from /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_first
% % :- '$set_source_module'(mpred_type_args).

:- include('mpred_header.pi').

/*
:- dynamic((
        argIsa_known/3,
        asserted_argIsa_known/3,
        coerce/3,
        decl_coerce/3,
        deduceFromArgTypes/1,
   hook_coerce/3,
        baseKB:module_local_init/0)).
*/


%= 	 	 

%% assert_argIsa( ?Prop, ?N, ?Type) is semidet.
%
% assert Argument  (isa/2).
%
% % :- '$set_source_module'(mpred_type_args).
assert_argIsa(Prop,N,Type):-show_failure(why,ain_fast(argIsa(Prop,N,Type))).



% % :- '$set_source_module'(mpred_type_args).

%% assert_predArgTypes( ?ArgTs) is semidet.
%
% assert Predicate Argument  Types.
%
assert_predArgTypes(ArgTs):-not(compound(ArgTs)),!.
assert_predArgTypes(ArgTs):- numbervars(ArgTs,0,_,[functor_name(ftTerm),attvar(skip)]),get_functor(ArgTs,F),assert_predArgTypes_fa(F,ArgTs).


%= 	 	 

%% assert_predArgTypes_fa( ?VALUE1, ?ArgTs) is semidet.
%
% assert Predicate Argument  Types Functor-Arity.
%
assert_predArgTypes_fa(_,ArgTs):- nonvar(ArgTs),ArgTs=(_/_),!.
assert_predArgTypes_fa(F,ArgTs):- not(is_list(ArgTs)),ArgTs=..[_|ArgsL],!,assert_predArgTypes_fa(F,ArgsL).
%assert_predArgTypes_fa(F,ArgsList):- clause_b(ftAction(F),true),!,show_call(why,must(assert_predArgTypes_from_left(F,1,ArgsList))).
assert_predArgTypes_fa(F,ArgsList):- length(ArgsList,L),assert_predArgTypes_l(F,L,ArgsList).

%assert_predArgTypes_l(F,L,ArgsList):- arity(F,A),!,must( (A>=L) -> assert_predArgTypes_from_right(F,A,ArgsList);true).

% % :- '$set_source_module'(mpred_type_args).


%% assert_predArgTypes_l( ?F, ?L, ?ArgsList) is semidet.
%
% assert Predicate Argument  Types (List version).
%
assert_predArgTypes_l(F,L,ArgsList):- must(assert_predArgTypes_from_right(F,L,ArgsList)).



%= 	 	 

%% assert_predArgTypes_from_right( ?F, ?A, :TermArgsList) is semidet.
%
% assert Predicate Argument  Types Converted From right.
%
assert_predArgTypes_from_right(_,_,[]):-!.
assert_predArgTypes_from_right(_,_,(_/_)):-!.
assert_predArgTypes_from_right(F,A,ArgsList):-append(Left,[Last],ArgsList),assert_argIsa(F,A,Last),!,Am1 is A -1, assert_predArgTypes_from_right(F,Am1,Left).


%= 	 	 

%% assert_predArgTypes_from_left( ?F, ?A, :TermType) is semidet.
%
% assert Predicate Argument  Types Converted From left.
%
assert_predArgTypes_from_left(_,_,[]):-!.
assert_predArgTypes_from_left(F,A,[Type|ArgsList]):-assert_argIsa(F,A,Type),!,Ap1 is A + 1,assert_predArgTypes_from_left(F,Ap1,ArgsList).



%= 	 	 

%% term_is_ft( :TermTerm, :TermType) is semidet.
%
% Term If Is A Format Type.
%
term_is_ft(Term,Type):- is_ftVar(Term),!,member(Type,[ftVar,ftProlog]).
term_is_ft(_ANY,Type):- Type==ftVar,!,fail.
term_is_ft([T|Term],ftListFn(Type)):-is_list_of(Type,[T|Term]).
term_is_ft(_ANY,Type):- nonvar(Type),(ttExpressionType==Type;(\+ ttExpressionType(Type))),!,fail.
term_is_ft(Term,Type):- nonvar(Type), term_is_ft_how(Term,Type),!.
term_is_ft(Term,Type):- no_repeats_old(Type,(term_is_ft_how(Term,Was),trans_subft(Was,Type))).



%= 	 	 

%% term_is_ft_how( ?Term, ?Type) is semidet.
%
% Term If Is A Format Type How.
%
term_is_ft_how(Term,Type):- clause_b(quotedDefnIff(Type,Info)),nonvar(Info),
   (show_success(why,(Info='SubLQuoteFn'(LISPSYMBOL),nop(dmsg(Term+Type+LISPSYMBOL))))-> 
                 fail;
                 (append_term(Info,Term,CALL),call_u(CALL))),!.

term_is_ft_how(Term,Type):- compound(Term),functor(Term,F,A),functor(Type,F,A),
  once((a(meta_argtypes,Type),Type=..[_|Types],Term=..[_|Args],
     maplist(call_as_t,Types,Args))).

call_as_t(T,A):-append_term(T,A,Call),call_u(Call).

%= 	 	 

%% trans_subft( ?FT, ?FT) is semidet.
%
% Trans Subft.
%
trans_subft(FT,FT).
trans_subft(FT,Sub):-clause_b(subFormat(FT,Sub)).
trans_subft(FT,Sub):-clause_b(subFormat(FT,A)),clause_b(subFormat(A,Sub)).
trans_subft(FT,Sub):-clause_b(subFormat(FT,A)),clause_b(subFormat(A,B)),clause_b(subFormat(B,Sub)).


%= 	 	 

%% is_id( ?ID) is semidet.
%
% If Is A Id.
%
is_id(ID):-atom(ID)->true;(compound(ID),arg(1,ID,A),is_id(A)).

%= 	 	 

%% is_boolean( ?VALUE1) is semidet.
%
% If Is A Boolean.
%
is_boolean(isMissing):-!,fail.
is_boolean(vTrue).
is_boolean(vFalse).


%= 	 	 

%% is_declarations( ?C) is semidet.
%
% If Is A Declarations.
%
is_declarations(C):-compound(C),ground(C),!, (\+ (arg(_,C,T), \+ is_spec(T))).


%= 	 	 

%% is_spec( ?T) is semidet.
%
% If Is A Spec.
%
is_spec(T):- call_u(tCol(T))->true;is_declarations(T).


% % :- '$set_source_module'(mpred_type_args).

%% is_rest( :TermARG1) is semidet.
%
% If Is A Rest.
%
is_rest([_|Term]):-not(is_list(Term)).

%= 	 	 

%% is_rest_of( ?Type, :TermARG2) is semidet.
%
% If Is A Rest Of.
%
is_rest_of(_Type,[_|Term]):-not(is_list(Term)).

%= 	 	 

%% is_list_of( ?Type, :TermTerm) is semidet.
%
% If Is A List Of.
%
is_list_of(Type,Term):- is_rest(Term),!,Type=ftRest.
is_list_of(Type,[T|Term]):-term_is_ft(T,Type),maplist(is_list_of(Type),Term).

/*
pl_arg_type_or_functor(Arg,Type):- pl_arg_type(Arg,T) , 
 (T==ftCompound -> functor(Arg,Type,_); 
  ( (T==ftListFn(_),Arg=[_|_])-> T=[Type|_] ;
         Type=T)) .

sameArgTypes(A,C):-same(A,C);(pl_arg_type(C,CT),pl_arg_type(A,AT),!,colsOverlap(AT,CT)).
colsOverlap(AT,AT).

*/


%= 	 	 

%% pl_arg_type( ?Arg, ?Type) is semidet.
%
% Pl Argument Type.
%
pl_arg_type(Arg,Type):- 
      var(Arg) -> Type =ftVar;
      integer(Arg) -> Type =ftInteger;
      number(Arg) -> Type =ftFloat;
      string(Arg) -> Type =ftString;
      is_ftText(Arg) -> Type =ftText;
      is_list(Arg) -> Type =ftListFn(_);
      atom(Arg) -> Type =ftAtom;
      atomic(Arg) -> Type =ftAtomic;
      compound(Arg) -> Type =ftCompound;
         Arg = Type.



% % :- '$set_source_module'(mpred_type_args).

%% is_ftText( ?Arg) is semidet.
%
% If Is A Format Type Text.
%
is_ftText(Arg):-string(Arg),!.
is_ftText(Arg):- \+ compound(Arg),!,fail.
is_ftText(Arg):- text_to_string_safe(Arg,_),!.
is_ftText(Arg):- functor(Arg,S,_),resultIsa(S,ftText).

% :- was_dynamic(coerce/3).
:- was_export(coerce/4).

%= 	 	 

%% coerce( ?VALUE1, ?VALUE2, ?NewThing, ?Else) is semidet.
%
% Coerce.
%
coerce(What,Type,NewThing,_Else):- call_u(coerce(What,Type,NewThing)),!.
coerce(_ ,_,     NewThing,Else):- NewThing = Else.

coerce(A,B,C):-no_repeats(call_u(coerce_hook(A,B,C))),(sanity(show_failure(call_u(isa(C,B))))->!;true).


%= 	 	 

%% mpred_arity_pred( ?P) is semidet.
%
% Managed Predicate Arity Predicate.
%
mpred_arity_pred(P):- nonvar(P),arg(_,a(arity,arity,arityMax,arityMin),P).
mpred_arity_pred(arity).


%= 	 	 

%% as_one_of( ?Type, ?TypeO) is semidet.
%
% Converted To One Of.
%
as_one_of(Types,Type):-nonvar(Type),tCol(Type),!,member(Type,Types).
as_one_of([Type],TypeO):-!,same_arg(same_or(genls),Type,TypeO).
as_one_of(Types,isOneOf(Types)).



%= 	 	 

%% argIsa_op_call( ?Op, :TermFunc, ?N, ?Type) is semidet.
%
% Argument  (isa/2) Oper. call.
%
argIsa_op_call(Op,_:F,N,Type):-!,argIsa_op_call(Op,F,N,Type),!.
argIsa_op_call(Op,F/_,N,Type):- !,argIsa_op_call(Op,F,N,Type),!.
argIsa_op_call(Op,Func,N,Type):- compound(Func),!,functor(Func,F,_),argIsa_op_call(Op,F,N,Type),!.
argIsa_op_call(_,F,N,Type):-hotrace((loop_check((argIsa_known(F,N,Type),!),Type=ftTerm),must(nonvar(Type)))).


:- was_export(argIsa_known/3).

%= 	 	 

%% argIsa_known( ?F, ?N, ?Type) is semidet.
%
% Argument  (isa/2) known.
%
argIsa_known(F/_,N,Type):-nonvar(F),!,argIsa_known(F,N,Type).
argIsa_known(F,N,Type):-  one_must(asserted_argIsa_known(F,N,Type),argIsa_call_7(F,N,Type)).



%= 	 	 

%% asserted_argIsa_known( ?F, ?N, ?Type) is semidet.
%
% asserted Argument  (isa/2) known.
%
asserted_argIsa_known(F/_,N,Type):-nonvar(F),!,asserted_argIsa_known(F,N,Type).
asserted_argIsa_known(F,N,Type):- argIsa_call_0(F,N,Type).
asserted_argIsa_known(F,N,Type):- var(F),!,tRelation(F),asserted_argIsa_known(F,N,Type).
asserted_argIsa_known(F,N,Type):- arity(F,1),!,N=1,Type=F.
asserted_argIsa_known(F,N,Type):- var(N),arity(F,A),!,between(1,A,N),asserted_argIsa_known(F,N,Type).
asserted_argIsa_known(F,N,Type):- argIsa_call_6(F,N,Type),!.


%= 	 	 

%% to_format_type( ?COL, ?FT) is semidet.
%
% Converted To Format Type.
%
to_format_type(FT,FT):-a(ttExpressionType,FT),!.
to_format_type(COL,FT):- clause_b(formatted_resultIsa(FT,COL)),!.
to_format_type(COL,FT):- clause_b(resultIsa(FT,COL)),a(ttExpressionType,FT),!.
to_format_type(COL,ftTerm(COL)).


%= 	 	 

%% argIsa_ft( ?F, ?N, ?FTO) is semidet.
%
% Argument  (isa/2) Format Type.
%
argIsa_ft(F/_,N,Type):-nonvar(F),!,argIsa_ft(F,N,Type).
argIsa_ft(F,N,FTO):-must((argIsa_known(F,N,FT),to_format_type(FT,FTO))),!.


:- was_export(argIsa_call_0/3).

%= 	 	 

%% argIsa_call_0( ?F, ?N, ?Type) is semidet.
%
% Argument  (isa/2) call  Primary Helper.
%
argIsa_call_0(F/_,N,Type):-nonvar(F),!,argIsa_call_0(F,N,Type).
argIsa_call_0(F,N,Type):- clause(t(argIsa,F,N,Type),true).
argIsa_call_0(F,N,Type):- clause(argIsa(F,N,Type),true).
argIsa_call_0(argIsa,1,tRelation).
argIsa_call_0(argIsa,2,ftInt).
argIsa_call_0(argIsa,3,tCol).  
argIsa_call_0(comment,2,ftString).
argIsa_call_0(isKappaFn,1,ftVar).
argIsa_call_0(isKappaFn,2,ftAskable).
%argIsa_call_0(isInstFn,1,tCol).
argIsa_call_0(Col,1,Col):-a(tCol,Col).
argIsa_call_0(Col,2,ftVoprop):-a(tCol,Col).
argIsa_call_0(quotedDefnIff,1,ttExpressionType).
argIsa_call_0(quotedDefnIff,2,ftCallable).
argIsa_call_0(meta_argtypes,1,ttExpressionType).


argIsa_call_0(isa,1,ftID).
argIsa_call_0(export,_,ftTerm).
argIsa_call_0(shared_multifile,_,ftTerm).


argIsa_call_0(isa,2,tCol).
argIsa_call_0(mpred_isa,1,tPred).
argIsa_call_0(mpred_isa,2,ftVoprop).
% argIsa_call_0(mpred_isa,3,ftVoprop).

argIsa_call_0(formatted_resultIsa,1,ttExpressionType).
argIsa_call_0(formatted_resultIsa,2,tCol).

argIsa_call_0(predicates,1,ftListFn(ftTerm)).
argIsa_call_0(resultIsa,2,tCol).

argIsa_call_0(predTypeMax,1,tPred).
argIsa_call_0(predTypeMax,2,tCol).
argIsa_call_0(predTypeMax,3,ftInt).

argIsa_call_0(predInstMax,1,tObj).
argIsa_call_0(predInstMax,2,tPred).
argIsa_call_0(predInstMax,3,ftInt).

argIsa_call_0(props,1,ftID).
argIsa_call_0(props,N,ftVoprop):- between(2,31,N).

argIsa_call_0(apathFn,1,tRegion).
argIsa_call_0(apathFn,2,vtDirection).
argIsa_call_0(localityOfObject,1,tObj).
argIsa_call_0(localityOfObject,2,tSpatialThing).

argIsa_call_0(typeProps,1,tCol).
argIsa_call_0(typeProps,N,ftVoprop):-between(2,31,N).

argIsa_call_0(instTypeProps,1,ftProlog).
argIsa_call_0(instTypeProps,2,tCol).
argIsa_call_0(instTypeProps,N,ftVoprop):-between(3,31,N).


argIsa_call_0(must,1,ftAskable).

argIsa_call_0(predicateConventionMt,1,tPred).
argIsa_call_0(predicateConventionMt,2,ftAtom).
% argIsa_call_0(baseKB:agent_text_command,_,ftTerm).
argIsa_call_0('<=>',_,ftTerm).
argIsa_call_0(class_template,N,Type):- (N=1 -> Type=tCol;Type=ftListFn(ftVoprop)).
argIsa_call_0(Arity,N,T):-mpred_arity_pred(Arity),arity(Arity,A),number(A),number(N),N=<A,arg(N,vv(tPred,ftInt,tCol),T).
argIsa_call_0(F,2,ftString):-member(F,[descriptionHere,mudDescription,nameStrings,mudKeyword]),!.

argIsa_call_0(F,N,Type):-a(functorDeclares,F),!,(N=1 -> Type=F ; Type=ftTerm(ftVoprop)).
argIsa_call_0(F,N,Type):-a(tCol,F),!,(N=1 -> Type=F ; Type=ftTerm(ftVoprop)).
argIsa_call_0(Compound,N,Type):-compound(Compound),!,arg(N,Compound,Type),tCol(Type).
argIsa_call_0(F,N,Type):-between(1,2,N),argIsa_call_3(F,Type).
argIsa_call_0(F,N,ftTerm):- N = 1, atom(F), current_predicate(F/N).
argIsa_call_0(F,N,ftAskable):- atom(F), current_predicate(F/A),between(1,A,N),functor(P,F,A), predicate_property(P,meta_predicate(P)),arg(N,P,Number),number(Number),!.
% argIsa_call_0(HILOG,_,term):-hilog_functor(HILOG).



%= 	 	 

%% argIsa_call_3( ?WP, ?VALUE2) is semidet.
%
% Argument  (isa/2) call Helper number 3..
%
argIsa_call_3(WP,tPred):-member(WP,[predProxyRetract,predProxyAssert,predProxyQuery,genlInverse]).
argIsa_call_3(disjointWith,tCol).
argIsa_call_3(ftFormFn,ftTerm).
argIsa_call_3(mudTermAnglify,ftTerm).
argIsa_call_3(genls,tCol).
argIsa_call_3(subFormat,ttExpressionType).


%= 	 	 

%% argisa_nodebug is semidet.
%
% Argument (isa/2) Nodebug.
%
argisa_nodebug:-!.


%= 	 	 

%% grab_argsIsa( ?F, ?Types) is semidet.
%
% grab Arguments  (isa/2).
%
grab_argsIsa(resultIsa,resultIsa(tFunction,tCol)).
%grab_argsIsa(P, A):-P=='$si$':'$was_imported_kb_content$',trace_or_throw(crazy_grab_argsIsa('$si$':'$was_imported_kb_content$', A)).
%grab_argsIsa(P, A):-P=={}, trace_or_throw(crazy_grab_argsIsa({}, A)).
grab_argsIsa(F,Types):- grab_argsIsa_6(Types),get_functor(Types,F0),F0==F,!,assert_predArgTypes_fa(F,Types).


%= 	 	 

%% grab_argsIsa_6( ?Types) is semidet.
%
% grab Arguments  (isa/2) Helper number 6..
%
grab_argsIsa_6(Types):- meta_argtypes(Types).
grab_argsIsa_6(mudColor(tSpatialThing, vtColor)).
grab_argsIsa_6(Types):- clause_b(quotedDefnIff(Types,_)),maybe_argtypes(Types).
% grab_argsIsa_6(Types):- current_predicate(get_all_templates/1),get_all_templates(Types),maybe_argtypes(Types).

%argIsa_call_6(F,N,Type):- isa(F,argIsa(N,Type)),nonvar(Type),assert_argIsa(F,N,Type),!.

%= 	 	 

%% argIsa_call_6( ?F, ?N, ?Type) is semidet.
%
% Argument  (isa/2) call Helper number 6..
%
argIsa_call_6(F,N,Type):- grab_argsIsa(F,Types),maybe_argtypes(Types),arg(N,Types,Type),show_failure(why,(nonvar(Type),assert_argIsa(F,N,Type))),!.


%= 	 	 

%% maybe_argtypes( ?Types) is semidet.
%
% Maybe Argument Types.
%
maybe_argtypes(Types):- compound(Types), ground(Types), Types\=(_/_), Types\=(_:_/_), Types\='$VAR'(_).


%= 	 	 

%% argIsa_call_7( ?Prop, ?N1, ?Type) is semidet.
%
% Argument  (isa/2) call Helper number 7..
%
argIsa_call_7(Prop,N1,Type):- nonvar(Type),!,argIsa_call_7(Prop,N1,WType),!,genls(WType,Type).
argIsa_call_7(Pred,N,ftVoprop):-number(N),arity(Pred,A),N>A,!.
argIsa_call_7(_,_,ftTerm):- argisa_nodebug,!.
argIsa_call_7(F,_,ftTerm):-member(F/_, [argIsa/3,predProxyAssert/2,negate_wrapper0/2,mudFacing/_,registered_module_type/2,       
                                ruleBackward/2,formatted_resultIsa/2,
                                pt/_,rhs/_,nt/_,bt/_,bracket/3]),!.
argIsa_call_7(Prop,N1,Type):- is_2nd_order_holds(Prop),dmsg(todo(define(argIsa(Prop,N1,'Second_Order_TYPE')))),dumpST,dtrace,Type=argIsaFn(Prop,N1),!.
argIsa_call_7(Prop,N1,Type):- argIsa_call_9(Prop,N1,Type).

%= 	 	 

%% argIsa_call_9( ?Prop, ?N1, ?Type) is semidet.
%
% Argument  (isa/2) call Helper number 9..
%
argIsa_call_9(_,_,Type):- argisa_nodebug,!,genls(ftTerm,Type).
argIsa_call_9(Prop,N1,Type):- arity(Prop,Arity),dmsg(todo(define(argIsa_known_a(Prop,N1,'_TYPE')))),number(Arity),number(N1),must(N1=<Arity),Type=argIsaFn(Prop,N1),!.
argIsa_call_9(Prop,N1,Type):- dmsg(todo(define(argIsa_known_b(Prop,N1,'_TYPE')))),dtrace,Type=argIsaFn(Prop,N1),!.
argIsa_call_9(_,_,ftTerm).


:- was_export(correctArgsIsa/2).

%= 	 	 

%% correctArgsIsa( ?In, ?Out) is semidet.
%
% correct Arguments  (isa/2).
%
correctArgsIsa(In,Out):- correctArgsIsa(query(must,t),In,Out),!.

:- was_export(correctArgsIsa/3).

%= 	 	 

%% correctArgsIsa( ?VALUE1, :TermNC, :TermNC) is semidet.
%
% correct Arguments  (isa/2).
%
correctArgsIsa(_,NC,NC):-not(compound(NC)),!.
correctArgsIsa(_,NC,NC):-as_is_term(NC),!.
correctArgsIsa(_,G,G):- (\+ t_l:infMustArgIsa), (is_release; bad_idea; skipWrapper;  t_l:infSkipArgIsa),!.
correctArgsIsa(Op,M:G,MAA):- nonvar(M),!,correctArgsIsa(Op,G,GG),M:GG=MAA.
correctArgsIsa(_,(A,B),(AA,BB)):-!,correctArgsIsa(Op,A,AA),correctArgsIsa(Op,B,BB).
correctArgsIsa(_,isa(Args,PredArgTypes),isa(Args,PredArgTypes)):- PredArgTypes==meta_argtypes,!.
correctArgsIsa(_,G,GG):- get_functor(G,F,A),
  arg(_,vv('{}'/_,  genls/_,mpred_isa/_,
    t/2,arity/_,genls/_,'<=>'/_,pt/_,rhs/_,nt/_,bt/_,
    formatted_resultIsa/_,resultIsa/_,quotedDefnIff/_),F/A),!,must_equals(G,GG).
correctArgsIsa(_,G,GG):- get_functor(G,F),lookup_u(functorDeclares(F)),!,must_equals(G,GG).
correctArgsIsa(_,G,GG):- t_l:infSkipArgIsa, !,must_equals(G,GG).
correctArgsIsa(Op,G,GG):- correctArgsIsa0(Op,G,GG),nonvar(GG),!.
correctArgsIsa(Op,G,GG):- grtrace,correctArgsIsa0(Op,G,GG).

:- was_export(correctArgsIsa/4).

%= 	 	 

%% correctArgsIsa( ?Op, ?A, ?Type, ?AA) is semidet.
%
% correct Arguments  (isa/2).
%
correctArgsIsa(Op,A,Type,AA):- trace_or_throw(warn(not(correctArgsIsa(Op,A,Type,AA)))).


%= 	 	 

%% list_to_callform( ?ARGS, ?Functor, ?CALL) is semidet.
%
% List Converted To Callform.
%
list_to_callform([P|ARGS],_,CALL):-atom(P),!,CALL=..[P|ARGS].
list_to_callform(ARGS,Functor,CALL):-CALL=..[Functor|ARGS].


%= 	 	 

%% correctArgsIsa0( ?Op, ?A, ?RESULTC) is semidet.
%
% correct Arguments  (isa/2) Primary Helper.
%
correctArgsIsa0(Op,[PRED|ARGS],RESULT):-!,correctArgsIsa00(Op,[PRED|ARGS],RESULT).
correctArgsIsa0(Op,A,RESULTC):-A=..[PRED|ARGS],!,correctArgsIsa00(Op,[PRED|ARGS],RESULT), list_to_callform(RESULT,t,RESULTC).


%= 	 	 

%% correctArgsIsa00( ?VALUE1, :TermProp, :TermAA) is semidet.
%
% correct Arguments  (isa/2) Primary Helper Primary Helper.
%
correctArgsIsa00(_ ,[Prop|Args],AA):-stack_check(1000), var(Prop),!,AA=[Prop|Args].
correctArgsIsa00(Op,[KP,Prop|Args],AA):-is_holds_true(KP),!,correctArgsIsa00(Op,[Prop|Args],AA).
correctArgsIsa00(Op,[KP,Prop|Args],[KP|AArgs]):-logical_functor_ft(KP),!,correctAnyType(Op,[Prop|Args],ftListFn(ftAskable),AArgs).
correctArgsIsa00(Op,[KP,Prop|Args],[KP|AA]):-is_holds_false(KP),!,correctArgsIsa00(Op,[KP,Prop|Args],AA).
%correctArgsIsa00(_ ,[Prop,Arg],[Prop,Arg]):- !.
correctArgsIsa00(Op,[Prop,ArgI],[Prop,ArgO]):- a(tCol,Prop),!, correctAnyType(query(ftID,Op),ArgI,Prop,ArgO).
correctArgsIsa00(Op,[Prop|Args],[Prop|AArgs]):- discoverAndCorrectArgsIsa(Op,Prop,1,Args,AArgs).


%= 	 	 

%% discoverAndCorrectArgsIsa( ?Op, ?Prop, ?VALUE3, ?ArgsIn, ?ArgsOut) is semidet.
%
% discover and correct Arguments  (isa/2).
%
discoverAndCorrectArgsIsa(Op,Prop,_,ArgsIn,ArgsOut):- length(ArgsIn,ArgUsed),show_failure(why,(mpred_full_arity(Prop,MaxArity),(number(ArgUsed),number(MaxArity),ArgUsed=<MaxArity))),
    discoverAndCorrectArgsIsa_from_right(Op,Prop,MaxArity,ArgsIn,ArgsOut),!.
discoverAndCorrectArgsIsa(Op,Prop,N,ArgsIn,ArgsOut):-discoverAndCorrectArgsIsa_from_left(Op,Prop,N,ArgsIn,ArgsOut),!.


%= 	 	 

%% discoverAndCorrectArgsIsa_from_right( ?Op, ?Prop, ?N1, ?In, ?Out) is semidet.
%
% discover and correct Arguments  (isa/2) Converted From right.
%
discoverAndCorrectArgsIsa_from_right(_O,_Prop,_N1,[],[]):-!.
discoverAndCorrectArgsIsa_from_right(Op,Prop,N1,In,Out):- append(Args,[A],In),
   must((argIsa_op_call(Op,Prop,N1,Type),correctAnyType(Op,A,Type,AA))),
   N2 is N1-1,
   discoverAndCorrectArgsIsa_from_right(Op,Prop,N2,Args,AArgs),
   append(AArgs,[AA],Out).


%= 	 	 

%% discoverAndCorrectArgsIsa_from_left( ?O, ?Prop, ?N1, :TermARG4, ?VALUE5) is semidet.
%
% discover and correct Arguments  (isa/2) Converted From left.
%
discoverAndCorrectArgsIsa_from_left(_O,_Prop,_N1,[],[]):-!.
discoverAndCorrectArgsIsa_from_left(Op,Prop,N1,[A|Args],Out):-
   must((argIsa_op_call(Op,Prop,N1,Type),correctAnyType(Op,A,Type,AA))),
   N2 is N1+1,
   discoverAndCorrectArgsIsa_from_left(Op,Prop,N2,Args,AArgs),
    Out = [AA|AArgs].



%= 	 	 

%% mpred_full_arity( ?F, ?A) is semidet.
%
% Managed Predicate Full Arity.
%
mpred_full_arity({},A):-trace_or_throw(crazy_mpred_full_arity({}, A)).
mpred_full_arity(F,A):-arity(F,A),!.
mpred_full_arity(F,A):-grab_argsIsa(F,Types),maybe_argtypes(Types),show_call(why,(functor(Types,F,A),assert_arity(F,A))),!.




%= 	 	 

%% is_ephemeral( :TermVar) is semidet.
%
% If Is A Ephemeral.
%
is_ephemeral(Var):-var(Var),!,fail.
is_ephemeral(isMissing).
is_ephemeral(isOptional(_,_)).
is_ephemeral(isRandom(_)).
is_ephemeral(isOneOf(_)).

:- was_export(correctAnyType/4).


%= 	 	 

%% is_valuespec( ?G) is semidet.
%
% If Is A Valuespec.
%
is_valuespec(G):-is_ephemeral(G).
is_valuespec(G):-a(tCol,G).
is_valuespec(FT):-a(ttExpressionType,FT).
is_valuespec(G):-evaluatableArg(G,_).


%= 	 	 

%% evaluatableArg( ?AA, ?VALUE2) is semidet.
%
% Evaluatable Argument.
%
evaluatableArg(AA,_):-compound(AA),get_functor(AA,F),!,evaluatableFunctor(F).

%= 	 	 

%% evaluatableFunctor( ?VALUE1) is semidet.
%
% Evaluatable Functor.
%
evaluatableFunctor(isRandom).
evaluatableFunctor(isOptional).


%= 	 	 

%% correctAnyType( ?VALUE1, ?A, ?VALUE3, ?A) is semidet.
%
% Correct Any Type.
%
correctAnyType(_,A,_,A):-is_release.

correctAnyType(_, A,_Type,AA):- is_ftVar(A),sanity(var(AA)),must_det(A=AA),!.
correctAnyType(Op,A,Type,AA):-  var(A),correctType(Op,A,Type,AA),sanity(var(AA)),must_det(A==AA),!.
correctAnyType(_, A,Type,AA):-  evaluatableArg(A,Type),dmsg(evaluatableArg(A,Type)),must_det(A=AA),!.
correctAnyType(Op,A,Type,AA):- var(Type),trace_or_throw(correctAnyType(Op,A,Type,AA)).
% TODO snags on new tpyes correctAnyType(Op,A,Type,AA):- correctType(Op,A,Type,AA),nonvar(AA),!.
correctAnyType(Op,A,Type,AA):- one_must(correctType(Op,A,Type,AA),A=AA).
correctAnyType(Op,A,Type,A):- dtrace(nop(dmsg(warn(not(correctAnyType(Op,A,Type)))))).



%  @set mudMoveDist 4

:- was_export(correctFormatType/4).

%= 	 	 

%% correctFormatType( ?Op, ?A, ?Type, ?AA) is semidet.
%
% Correct Format Type.
%
correctFormatType(Op,A,Type,AA):- var(A),correctType(Op,A,Type,AA),sanity(var(AA)),must_det(A==AA),!.
correctFormatType(Op,A,Type,AA):- var(Type),trace_or_throw(correctFormatType(Op,A,Type,AA)).
correctFormatType(Op,A,Type,AA):- correctType(Op,A,Type,AA),sanity(nonvar(AA)),!.
correctFormatType(Op,A,Type,AA):- tracing, correctType(Op,A,Type,AA).
correctFormatType(Op,A,Type,A):- dmsg(todo(not(correctFormatType(Op,A,Type)))),fail.

:- was_export(checkAnyType/4).


%= 	 	 

%% checkAnyType( ?Op, ?A, ?Type, ?AA) is semidet.
%
% Check Any Type.
%
checkAnyType(Op,A,Type,AA):- var(A),correctType(Op,A,Type,AA),sanity(var(AA)),must_det(A==AA),!.
checkAnyType(Op,A,Type,AA):- correctType(Op,A,Type,AA),nonvar(AA),!.


%= 	 	 

%% correctAnyTypeOrFail( ?Op, ?A, ?Type, ?AA) is semidet.
%
% Correct Any Type Or Fail.
%
correctAnyTypeOrFail(Op,A,Type,AA):- w_tl(tlbugger:skipMust,checkAnyType(Op,A,Type,AA)).



:- thread_local t_l:can_coerce/1.

%= 	 	 

%% correctType_gripe( ?Op, ?A, ?Fmt, ?AA) is semidet.
%
% Correct Type Gripe.
%
correctType_gripe(Op,A,Fmt,AA):- a(ttExpressionType,Fmt),!,trace_or_throw(correctType(is_ft_correctFormatType(Op,A,Fmt,AA))).
correctType_gripe(Op,A,Type,AA):- fail,atom(Type),must_equals(A,AA),
      dmsg(todo(isa_assert_type(Type))),
      % decl_type(Type),
      t_l:can_coerce(Op),
      dmsg(warning(ain(isa(A,Type)))),
      dtrace(ain(isa(A,Type))),!.

correctType_gripe(Op,A,C,A):-sanity(ground(A)),dmsg(todo(define(correctType(Op,A,C,'ConvertedArg')))),throw(retry(_)).
correctType_gripe(Op,A,Type,NewArg):-trace_or_throw(failure(correctType(Op,A,Type,NewArg))).

:- style_check(+singleton).


%= 	 	 

%% is_renamed_to( ?A, ?AA) is semidet.
%
% If Is A Renamed Converted To.
%
is_renamed_to(A,AA):- fail,atomic(A),not(A=[];A='';A=""),not(atom_concat(_,'Table',A)),not(atom_concat(_,'table',A)),
    atom_concat(Base,'able',A),atom_length(Base,AL),AL>2,!,atom_concat(Base,'Able',AA).


%= 	 	 

%% correctType( ?Op, ?A, ?Type, ?AA) is semidet.
%
% Correct Type.
%
correctType(Op,A,Type,AA):-correctType0(Op,A,Type,AA).


%= 	 	 

%% correctType0( ?Op, :TermA, :TermType, :TermAA) is semidet.
%
% Correct Type Primary Helper.
%
correctType0(change(_,_),A,T,AA):- A==T,!,must_equals(A,AA).
correctType0(_ ,A,T,AA):- A==T,!,must_equals(A,AA).
correctType0(Op,A,Type,AA):- var(Type),trace_or_throw(correctType0(Op,A,Type,AA)).
correctType0(_ ,A,Type,AA):- (var(A);var(Type)),!, must(must_equals(A,AA)).

correctType0(Op,A,'&'(Type1,Type2),AA):-var(Type2),!,correctType0(Op,A,Type1,AA).
correctType0(Op,A,'&'(Type1,Type2),AAA):-!,correctType0(Op,A,Type1,AA),correctType0(Op,AA,Type2,AAA).


correctType0(_ ,A,ftCallable,AA):- must_equals(A,AA).
correctType0(Op,A,ftID,AA):- must_equals_correct(query(ftID,Op),A,AA),!.
correctType0(_ ,A,Type,AAA):-A==Type,!,A=AAA.
correctType0(query(ftID,Op),A,ftAction,AA):- must_equals_correct(Op,A,AA),!.
correctType0(Op,A,Type,AAA):-is_renamed_to(A,AA),!,must(correctType0(Op,AA,Type,AAA)).
correctType0(Op,+A,Type,+AA):-nonvar(A),!,correctType0(Op,A,Type,AA).
correctType0(Op,-A,Type,-AA):-nonvar(A),!,correctType0(Op,A,Type,AA).
correctType0(Op,A,ftInteger,AA):-!,correctType0(Op,A,ftInt,AA).
correctType0(Op,A,ftAskable,AA):-!,must_equals_correct(query(ftAskable,Op),A,AA).
correctType0(_ ,A,ftInt,AA):- any_to_number(A,AA).
correctType0(_ ,A,ftNumber,AA):- any_to_number(A,AA).
correctType0(_ ,A,ftProlog,AA):- must_equals(A,AA).
correctType0(_ ,A,ftString,AA):- must(any_to_string(A,AA)).
correctType0(Op,A,ftTerm(_),AA):- must_equals_correct(Op,A,AA).
correctType0(_ ,A,ftVoprop,AA):- !, must(A=AA).
correctType0(Op,A,ftVoprop,AA):- is_list(A),!,maplist(correctTypeArg(Op,ftAskable),A,AA).
correctType0(Op,A,ftVoprop,AA):- !,w_tl(t_l:inVoprop,correctType0(Op,A,ftAskable,AA)).

correctType0(_ ,Obj,argIsaFn(_Prop,N),AA):-must_equals(Obj,AA),
   ignore((t_l:deduceArgTypes(_),
     sanity(N\=0),
      findall(OT,call_u(isa_asserted(Obj,OT)),_OType),
         ! /* must(nop(deduce_argN(Prop,N,Obj,OType,argIsaFn(Prop,N)))) */ )),!.


correctType0(_ ,A,ftTerm,AA):- must_equals(A,AA).
correctType0(_ ,A,ftText,AA):- must_equals(A,AA).

correctType0(_ ,A,tPred,AA):- any_to_relation(A,AA).
correctType0(_ ,A,tFunction,AA):- any_to_relation(A,AA).
correctType0(_ ,A,tRelation,AA):- any_to_relation(A,AA).
correctType0(_ ,A,ftAtom,AA):- any_to_atom(A,AA).
correctType0(change(_,_),A,tCol,AA):- atom(A),deduced_is_tCol(A),must_equals(A,AA).
correctType0(change(_,_),A,tCol,AA):- compound(A),deduced_is_tCol(A),must_equals(A,AA).
correctType0(_ ,A,vtVerb,AA):- must_equals(A,AA).
correctType0(_ ,A,Type,AA):- compound(A),( \+ is_list(A)),atom(Type),functor_safe(A,Type,_), must_equals(A,AA).

correctType0(_ ,A,Type,AA):- compound(Type),contains_var(Type,isThis),
   subst(Type,isThis,A,Call1),subst(Call1,value,AA,Call2),!,
      show_call(why,(Call2)),ignore(AA=A).

correctType0(_ ,A,Type,AA):- functor(Type,F,A),
   (A2 is A+2,current_predicate(F/A2)->show_call(why,clause_u(t(Type,A,AA)));
   (A1 is A+1,current_predicate(F/A1)->show_call(why,clause_u(t(Type,A))));
   fail),ignore(AA=A).


correctType0(_ ,What,Type,NewThing):- call_u(coerce(What,Type,NewThing)),!.
%TODO Confirm vtDirection is coerce/3'd correctType0(_ ,A,vtDirection,AA):- call((current_predicate(any_to_dir/2),!,call(any_to_dir,A,AA))),!.
%TODO Confirm vtDirection is coerce/3'd correctType0(_ ,A,vtDirection,AA):- must_equals(A,AA).

correctType0(query(HLDS,Must),A,xyzFn(Region, ftInt, ftInt, ftInt),xyzFn(AA, _, _, _)):-atom(A),correctAnyType(query(HLDS,Must),A,Region,AA).
correctType0(_ ,A,ftListFn(_),AA):- A == [],!,A=AA.
correctType0(Op,[A|AA],ftListFn(T),[L|LIST]):-!, correctType0(Op,A,T,L), correctType0(Op,AA,ftListFn(T),LIST).
correctType0(Op,A,ftListFn(T),[OT]):-!,correctAnyType(Op,A,T,OT).
correctType0(_ ,A,same(T),AA):-must_equals(T,AA),must_equals(A,AA).
correctType0(Op,A,isOneOf(List),AA):-!,member(Type,List),correctType0(Op,A,Type,AA).

correctType0(_ ,[],ftFormFn([]),[]):-!.
correctType0(Op,[H|T],ftFormFn([H2|T2]),[H3|T3]):-
   correctType0(Op,H,H2,H3),
   correctType0(Op,T,ftFormFn(T2),T3).

correctType0(Op,Args,ftFormFn(Types),NewArgs):- compound(Args),compound(Types),
   functor(Args,F,N),functor(Types,F,N),functor(NewArgs,F,N),
   Args=..[F|ArgsL],
   Types=..[F|TypesL],
   NewArgs=..[F|NewArgsL],!,   
   correctType0(Op,ArgsL,TypesL,NewArgsL).

correctType0(Op,[A|AA],ftListFn(T),LIST):-!,findall(OT,((member(O,[A|AA]),correctAnyType(Op,O,T,OT))),LIST).
correctType0(Op,A,ftListFn(T),[OT]):-!,correctAnyType(Op,A,T,OT).
correctType0(_ ,[],[],[]):-!.
correctType0(Op,[H|T],[H2|T2],[H3|T3]):-!, correctAnyType(Op,H,H2,H3),correctType0(Op,T,T2,T3).

correctType0(Op,Args,Types,NewArgs):-compound(Args), compound(Types),
   functor(Args,F,N),functor(Types,F,N),functor(NewArgs,F,N),
   Args=..[F|ArgsL],
   Types=..[F|TypesL],
   NewArgs=..[F|NewArgsL],
   correctAnyType(Op,ArgsL,TypesL,NewArgsL).

correctType0(Op,A,Fmt,AA):- trans_subft(Fmt,Code),Fmt\=Code,loop_check(correctType0(Op,A,Code,AA)).
correctType0(Op,A,Super,AA):- a(ttExpressionType,Super),call_u(genls(Sub,Super)),Sub\=Super,loop_check(correctType0(Op,A,Sub,AA)).

correctType0(Op,Arg,Props,NewArg):- compound(Props),
   Props=..[F|TypesL],
   functor(Props,F,A),
   A2 is A+1,
   clause_b(arity(F,A2)),
   C=..[F,Arg|TypesL],
   correctArgsIsa(Op,C,CC),
   CC=..[F,NewArg|_].

correctType0(_ ,A,Type,AA):- \+ (a(ttExpressionType,Type)),a(tCol,Type),isa_asserted(A,Type),!,must_equals(A,AA).
correctType0(_,A,_,_):- not(compound(A)),!,fail.
correctType0(Op,A,T,AAA):- once(correctArgsIsa(Op,A,AA)),A\=AA,!,correctType0(Op,AA,T,AAA).
correctType0(_ ,A,T,AA):- get_functor(A,F),clause_b(resultIsa(F,T)),must_det(A=AA),!.
correctType0(_ ,A,T,AA):- get_functor(A,F),clause_b(formatted_resultIsa(F,T)),must_det(A=AA),!.



%= 	 	 

%% correctTypeArg( ?Op, ?Type, ?A, ?AA) is semidet.
%
% Correct Type Argument.
%
correctTypeArg(Op,Type,A,AA):-correctType(Op,A,Type,AA).


%= 	 	 

%% must_equals_correct( ?Op, ?A, ?AA) is semidet.
%
% Must Be Successfull Equals Correct.
%
must_equals_correct(Op,A,AA):-must(correctArgsIsa(Op,A,AA)).

% :- style_check(+singleton).


%= 	 	 

%% must_equals( ?A, ?AA) is semidet.
%
% Must Be Successfull Equals.
%
must_equals(A,AA):-must_det(A=AA).


%= 	 	 

%% deduced_is_tCol( ?VALUE1) is semidet.
%
% Deduced If Is A True Structure Col.
%
deduced_is_tCol(A):- (t_l:infSkipArgIsa->true; (a(tCol,A)->true;(fail,ain(isa(A,tCol))))),!.
:- style_check(+singleton).

:- was_export(any_to_value/2).

%= 	 	 

%% any_to_value( ?Var, ?Var) is semidet.
%
% Any Converted To Value.
%
any_to_value(Var,Var):-var(Var),!.
any_to_value(V,Term):-atom(V),!,atom_to_value(V,Term).
any_to_value(A,V):-any_to_number(A,V).
any_to_value(A,A).

:- was_export(correctArgsIsa/3).

:- was_export(any_to_number/2).

%= 	 	 

%% any_to_number( :TermN, ?N) is semidet.
%
% Any Converted To Number.
%
any_to_number(N,N):- number(N),!.
any_to_number(ftDice(A,B,C),N):- ground(A),roll_dice(A,B,C,N),!.
any_to_number(A,N):-atom(A),atom_to_value(A,V),A\=V,any_to_number(V,N).
any_to_number(A,N):- catch(number_string(N,A),_,fail).

:- was_export(atom_to_value/2).

%= 	 	 

%% atom_to_value( ?V, :TermTerm) is semidet.
%
% Atom Converted To Value.
%
atom_to_value(V,Term):-not(atom(V)),!,any_to_value(V,Term).
% 56
atom_to_value(V,Term):- catch((read_term_from_atom(V,Term,[variable_names([])])),_,fail),!.
% 18d18+4000
atom_to_value(V,ftDice(T1,T2,+T3)):- atomic_list_concat_safe([D1,'d',D2,'+',D3],V), atom_to_value(D1,T1),atom_to_value(D2,T2),atom_to_value(D3,T3),!.
atom_to_value(V,ftDice(T1,T2,-T3)):- atomic_list_concat_safe([D1,'d',D2,'-',D3],V), atom_to_value(D1,T1),atom_to_value(D2,T2),atom_to_value(D3,T3),!.




%= 	 	 

%% any_to_relation( ?A, ?F) is det.
%
% Any Converted To Relation.
%
any_to_relation(A,F):-atomic(A),!,any_to_atom(A,F).
any_to_relation(A,F):-functor_h(A,F),!.


%= 	 	 

%% roll_dice( ?Rolls, ?VALUE2, ?Bonus, ?Result) is semidet.
%
% Roll Dice.
%
roll_dice(Rolls,_,Bonus,Result):- Rolls < 0, !, Result is Bonus.
roll_dice(Rolls,Sided,Bonus,Result):- LessRolls is Rolls-1, roll_dice(LessRolls,Sided, Bonus + random(Sided) +1, Result).

% call_argIsa_ForAssert(F,N,Type):-argIsa_known(F,N,Type),atom(Type),!,not(nonusefull_deduction_type(Type)),tCol(Type).

%:-ain_fast(<=( argIsa(F,N,Isa), asserted_argIsa_known(F,N,Isa))).
%:-ain_fast(<=( argIsa(F,N,Isa), argIsa_known(F,N,Isa))).

%= 	 	 

% module_local_init() is semidet.
%
% Hook To [baseKB:module_local_init/0] For Module Mpred_type_args.
% Module Local Init.
%
baseKB:module_local_init:- ain_fast(prologHybrid(formatted_resultIsa/2)).
baseKB:module_local_init:- ain_fast(prologHybrid(resultIsa/2)).

mpred_type_args_file.
