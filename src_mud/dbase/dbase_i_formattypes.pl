% =======================================================
/** <module> 
% This is mainly used by the moo_loader but also needed everywhere
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
% =======================================================

% :-swi_module(dbase_formattypes, []).

:-export((          
          any_to_number/2,
          any_to_value/2,
          argIsa_op_call/4,
          argIsa_call_0/3,
          atom_to_value/2,
         % formattype/1,
          term_is_ft/2,
          pl_arg_type/2,
          p2c_dir2/2
          )).


:- include(dbase_i_header).
:-export correctArgsIsa/3.



term_is_ft(Term,Type):- var(Term),!,member(Type,[ftVar,ftProlog]).
term_is_ft(_ANY,Type):- Type==ftVar,!,fail.
term_is_ft([T|Term],ftListFn(Type)):-!,is_list_of(Type,[T|Term]).
term_is_ft(Term,Type):- no_repeats_av(Type,(term_is_ft_how(Term,Was),trans_subft(Was,Type))).


term_is_ft_how(Term,Type):- is_asserted(defnSufficient(Type,Info)),(show_call_success((Info='SubLQuoteFn'(LISPSYMBOL),nop(Term+Type+LISPSYMBOL)))->fail;call(Info,Term)).
term_is_ft_how(Term,Type):- compound(Term),functor(Term,F,A),functor(Type,F,A),hasInstance(ttFormatted,Type),
   Type=..[_|Types],Term=..[_|Args],!,maplist(isa,Args,Types).

trans_subft(FT,FT).
trans_subft(FT,Sub):-dbase_t(subFormat(FT,Sub)).
trans_subft(FT,Sub):-dbase_t(subFormat(FT,A)),dbase_t(subFormat(A,Sub)).
trans_subft(FT,Sub):-dbase_t(subFormat(FT,A)),dbase_t(subFormat(A,B)),dbase_t(subFormat(B,Sub)).

is_id(ID):-atom(ID)->true;(compound(ID),arg(1,ID,A),is_id(A)).
is_boolean(isMissing):-!,fail.
is_boolean(vTrue).
is_boolean(vFalse).

is_declarations(C):-compound(C),!,ground(C),!.

is_rest([_|Term]):-not(is_list(Term)).
is_rest_of(_Type,[_|Term]):-not(is_list(Term)).
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

pl_arg_type(Arg,Type):- 
      var(Arg) -> Type =ftVar;
      integer(Arg) -> Type =ftInteger;
      number(Arg) -> Type =ftFloat;
      string(Arg) -> Type =ftString;
      is_string(Arg) -> Type =ftText;
      is_list(Arg) -> Type =ftListFn(_);
      atom(Arg) -> Type =ftAtom;
      atomic(Arg) -> Type =ftAtomic;
      compound(Arg) -> Type =ftCompound;
         Arg = Type.


:-export(coerce/4).
coerce(What,Type,NewThing,_Else):-coerce(What,Type,NewThing),!.
coerce(_ ,_,     NewThing,Else):- NewThing = Else.


mpred_arity_pred(P):- nonvar(P),arg(_,a(arity,arity,arityMax,arityMin),P).
mpred_arity_pred(arity).

as_one_of(Types,Type):-nonvar(Type),tCol(Type),!,member(Type,Types).
as_one_of([Type],TypeO):-!,same_arg(same_or(genls),Type,TypeO).
as_one_of(Types,isOneOf(Types)).


argIsa_op_call(Op,_:F,N,Type):-!,argIsa_op_call(Op,F,N,Type),!.
argIsa_op_call(Op,F/_,N,Type):- !,argIsa_op_call(Op,F,N,Type),!.
argIsa_op_call(Op,Func,N,Type):- compound(Func),!,functor(Func,F,_),argIsa_op_call(Op,F,N,Type),!.
argIsa_op_call(_,F,N,Type):-hotrace((loop_check((argIsa_known(F,N,Type),!),Type=ftTerm),must(nonvar(Type)))).


:-export(argIsa_known/3).
argIsa_known(F,N,Type):- one_must(asserted_argIsa_known(F,N,Type),argIsa_known0(F,N,Type)).
argIsa_known0(F,N,Type):- not(number(N)),trace_or_throw(type_error(not(number(N)),argIsa_known(F,N,Type))).
argIsa_known0(F,N,Type):- argIsa_call_7(F,N,Type),!.
argIsa_known0(F,N,Type):- argIsa_call_9(F,N,Type),!.
argIsa_known0(F,N,Type):- findall(T,argIsa_call_0(F,N,Type),T),Types=[_|_],!,as_one_of(Types,Type),!.


asserted_argIsa_known(F,N,Type):- argIsa_call_0(F,N,Type).
asserted_argIsa_known(F,N,Type):- var(F),!,tPred(F),argIsa(F,N,Type).
asserted_argIsa_known(F,N,Type):- var(N),arity(F,A),!,between(1,A,N),argIsa(F,N,Type).
asserted_argIsa_known(F,N,Type):- argIsa_call_6(F,N,Type),!.

to_format_type(FT,FT):-hasInstance(ttFormatType,FT),!.
to_format_type(COL,FT):- is_asserted(formatted_resultIsa(FT,COL)),!.
to_format_type(COL,FT):- is_asserted(resultIsa(FT,COL)),hasInstance(ttFormatType,FT),!.
to_format_type(COL,ftTerm(COL)).

argIsa_ft(F,N,FTO):-must((argIsa_known(F,N,FT),to_format_type(FT,FTO))),!.


:-export(argIsa_call_0/3).
argIsa_call_0(F,N,Type):- clause(dbase_t(argIsa,F,N,Type),true).
argIsa_call_0(F,N,Type):- clause(argIsa(F,N,Type),true).
argIsa_call_0(argIsa,1,tRelation).
argIsa_call_0(argIsa,2,ftInt).
argIsa_call_0(argIsa,3,tCol).  
argIsa_call_0(comment,2,ftString).
argIsa_call_0(isKappaFn,1,ftVar).
argIsa_call_0(isKappaFn,2,ftAskable).
argIsa_call_0(isInstFn,1,tCol).
argIsa_call_0(Col,1,Col):-hasInstance(tCol,Col).
argIsa_call_0(Col,2,ftVoprop):-hasInstance(tCol,Col).
argIsa_call_0(defnSufficient,1,ttFormatType).
argIsa_call_0(defnSufficient,2,ftCallable).
argIsa_call_0(ttFormatted,1,ttFormatType).


argIsa_call_0(isa,1,ftID).
argIsa_call_0(export,_,ftTerm).
argIsa_call_0(decl_mpred_hybrid,_,ftTerm).


argIsa_call_0(isa,2,tCol).
argIsa_call_0(mpred_prop,1,tPred).
argIsa_call_0(mpred_prop,2,ftVoprop).

argIsa_call_0(formatted_resultIsa,1,ttFormatType).
argIsa_call_0(formatted_resultIsa,2,tCol).

argIsa_call_0(predicates,1,ftListFn(ftTerm)).
argIsa_call_0(resultIsa,2,tCol).

argIsa_call_0(predTypeMax,1,tPred).
argIsa_call_0(predTypeMax,2,tCol).
argIsa_call_0(predTypeMax,3,ftInt).

argIsa_call_0(predInstMax,1,tObj).
argIsa_call_0(predInstMax,2,tPred).
argIsa_call_0(predInstMax,3,ftInt).

argIsa_call_0(prop,1,ftID).
argIsa_call_0(prop,N,ftVoprop):- N>1.

argIsa_call_0(apathFn,1,tRegion).
argIsa_call_0(apathFn,2,vtDirection).
argIsa_call_0(localityOfObject,1,tObj).
argIsa_call_0(localityOfObject,2,tSpatialThing).

argIsa_call_0(typeProps,1,tCol).
argIsa_call_0(typeProps,N,ftVoprop):- N>1.

argIsa_call_0(instTypeProps,1,ftProlog).
argIsa_call_0(instTypeProps,2,tCol).
argIsa_call_0(instTypeProps,N,ftVoprop):- N> 2.


argIsa_call_0(must,1,ftAskable).

argIsa_call_0(predModule,1,tPred).
argIsa_call_0(predModule,2,ftAtom).
% argIsa_call_0(user:agent_text_command,_,ftTerm).
argIsa_call_0('<=>',_,ftTerm).
argIsa_call_0(F,N,Type):-between(1,2,N),argIsa_call_3(F,Type).
argIsa_call_0(class_template,N,Type):- (N=1 -> Type=tCol;Type=ftListFn(ftVoprop)).
argIsa_call_0(Arity,N,T):-mpred_arity_pred(Arity),!,arg(N,vv(tPred,ftInt,tCol),T).
argIsa_call_0(F,2,ftString):-member(F,[descriptionHere,mudDescription,nameStrings,mudKeyword]),!.

argIsa_call_0(F,N,Type):-hasInstance(functorDeclares,F),!,(N=1 -> Type=F ; Type=ftTerm(ftVoprop)).
argIsa_call_0(F,N,Type):-hasInstance(tCol,F),!,(N=1 -> Type=F ; Type=ftTerm(ftVoprop)).
argIsa_call_0(F,N,ftTerm):-N = 1, current_predicate(F/N).
argIsa_call_0(F,N,ftAskable):- current_predicate(F/A),N =< A, functor(P,F,A), predicate_property(P,meta_predicate(P)),arg(N,P,Number),number(Number),!.
% argIsa_call_0(HILOG,_,term):-hilog_functor(HILOG).


argIsa_call_3(WP,tPred):-member(WP,[predProxyRetract,predProxyAssert,predProxyQuery,genlInverse]).
argIsa_call_3(disjointWith,tCol).
argIsa_call_3(ftFormFn,ftTerm).
argIsa_call_3(mudTermAnglify,ftTerm).
argIsa_call_3(genls,tCol).
argIsa_call_3(subFormat,ttFormatType).


grab_argsIsa(resultIsa,resultIsa(tFunction,tCol)).
grab_argsIsa(P, A):-P==was_imported_kb_content,trace_or_throw(crazy_grab_argsIsa(was_imported_kb_content, A)).
grab_argsIsa(P, A):-P=={}, trace_or_throw(crazy_grab_argsIsa({}, A)).
grab_argsIsa(F,Types):- grab_argsIsa_6(Types),compound(Types),get_functor(Types,F),assert_predArgTypes_fa(F,Types).

grab_argsIsa_6(Types):- hasInstance(ttFormatted,Types).
grab_argsIsa_6(mudColor(tSpatialThing, vtColor)).
grab_argsIsa_6(Types):- hasInstance(predArgTypes,Types).
grab_argsIsa_6(Types):- is_asserted(defnSufficient(Types,_)).
grab_argsIsa_6(Types):- hasInstance(_,Types),compound(Types),ground(Types).
grab_argsIsa_6(Types):- asserted_mpred_prop(_,predArgTypes(Types)).
grab_argsIsa_6(Types):- is_asserted(get_all_templates(Types)).
grab_argsIsa_6(Types):- current_predicate(get_all_templates/1),get_all_templates(Types),ground(Types).

argIsa_call_6(F,N,Type):- asserted_mpred_prop(F,argIsa(N,Type)),nonvar(Type),assert_argIsa(F,N,Type),!.
argIsa_call_6(F,N,Type):- grab_argsIsa(F,Types),arg(N,Types,Type),show_call_failure((nonvar(Type),assert_argIsa(F,N,Type))),!.

argIsa_call_7(F,N,_):- asserted_mpred_prop(F,prologHybrid),trace_or_throw(cant_grab_argsIsa(F,N,_)),fail.

argIsa_call_7(F,_,ftTerm(ftProlog)):-member(F/_,
                                [
                                argIsa/3,
                                predProxyAssert/2,
                                 negate_wrapper0/2,
                                registered_module_type/2,       
                                ruleBackward/2,
                                formatted_resultIsa/2,
                                bracket/3]).

argIsa_call_7(mudFacing,_,ftTerm).
% argIsa_call_7(Var,2,ftTerm):-not(must(not(is_asserted(isa(Var,tCol))))),dmsg(trace_or_throw( argIsa_call_9(Var,2,ftTerm))),fail.
argIsa_call_7(Prop,N1,Type):- is_2nd_order_holds(Prop),dmsg(todo(define(argIsa(Prop,N1,'Second_Order_TYPE')))),dumpST,dtrace,
   Type=argIsaFn(Prop,N1).
argIsa_call_7(F,N,Type):- debugOnError(dbase_t(argIsa,F,N,Type)).

argIsa_call_9(Prop,N1,Type):- arity(Prop,Arity),dmsg(todo(define(argIsa_known(Prop,N1,'_TYPE')))),must(N1=<Arity),Type=argIsaFn(Prop,N1).
argIsa_call_9(Prop,N1,Type):- dmsg(todo(define(argIsa_known(Prop,N1,'_TYPE')))),Type=argIsaFn(Prop,N1).
argIsa_call_9(_,_,ftTerm).


:-export(correctArgsIsa/2).
correctArgsIsa(In,Out):- correctArgsIsa(query(must,dbase_t),In,Out),!.

:-export(correctArgsIsa/3).
correctArgsIsa(_,NC,NC):-not(compound(NC)),!.
correctArgsIsa(_,NC,NC):-as_is_term(NC),!.
correctArgsIsa(_,G,G):- (is_release; bad_idea;  thlocal:infSkipArgIsa),!.
correctArgsIsa(Op,M:G,MAA):- nonvar(M),!,correctArgsIsa(Op,G,GG),M:GG=MAA.
correctArgsIsa(_,(A,B),(AA,BB)):-!,correctArgsIsa(Op,A,AA),correctArgsIsa(Op,B,BB).
correctArgsIsa(_,isa(Args,PredArgTypes),isa(Args,PredArgTypes)):- predArgTypes==predArgTypes,!.
correctArgsIsa(_,G,GG):- get_functor(G,F,A),
  arg(_,vv('{}'/_,  genls/_,user:mpred_prop/_,
    hasInstance/2,arity/_,genls/_,'<=>'/_,
    formatted_resultIsa/_,resultIsa/_,defnSufficient/_),F/A),!,must_equals(G,GG).
correctArgsIsa(_,G,GG):- get_functor(G,F),hasInstance(functorDeclares,F),!,must_equals(G,GG).
correctArgsIsa(_,G,GG):- thlocal:infSkipArgIsa, !,must_equals(G,GG).
correctArgsIsa(Op,G,GG):- correctArgsIsa0(Op,G,GG),nonvar(GG),!.
correctArgsIsa(Op,G,GG):- grtrace,correctArgsIsa0(Op,G,GG).

:-export(correctArgsIsa/4).
correctArgsIsa(Op,A,Type,AA):- trace_or_throw(warn(not(correctArgsIsa(Op,A,Type,AA)))).

list_to_callform([P|ARGS],_,CALL):-atom(P),!,CALL=..[P|ARGS].
list_to_callform(ARGS,Functor,CALL):-CALL=..[Functor|ARGS].

correctArgsIsa0(Op,[PRED|ARGS],RESULT):-!,correctArgsIsa00(Op,[PRED|ARGS],RESULT).
correctArgsIsa0(Op,A,RESULTC):-A=..[PRED|ARGS],!,correctArgsIsa00(Op,[PRED|ARGS],RESULT), list_to_callform(RESULT,dbase_t,RESULTC).

correctArgsIsa00(_ ,[Prop|Args],AA):-stack_check(1000), var(Prop),!,AA=[Prop|Args].
correctArgsIsa00(Op,[KP,Prop|Args],AA):-is_holds_true(KP),!,correctArgsIsa00(Op,[Prop|Args],AA).
correctArgsIsa00(Op,[KP,Prop|Args],[KP|AArgs]):-logical_functor_ft(KP),!,correctAnyType(Op,[Prop|Args],ftListFn(ftAskable),AArgs).
correctArgsIsa00(Op,[KP,Prop|Args],[KP|AA]):-is_holds_false(KP),!,correctArgsIsa00(Op,[KP,Prop|Args],AA).
%correctArgsIsa00(_ ,[Prop,Arg],[Prop,Arg]):- !.
correctArgsIsa00(Op,[Prop,ArgI],[Prop,ArgO]):- asserted_mpred_prop(Prop,tCol),!, correctAnyType(query(ftID,Op),ArgI,Prop,ArgO).
correctArgsIsa00(Op,[Prop|Args],[Prop|AArgs]):- discoverAndCorrectArgsIsa(Op,Prop,1,Args,AArgs).

discoverAndCorrectArgsIsa(Op,Prop,_,ArgsIn,ArgsOut):- length(ArgsIn,ArgUsed),show_call_failure((mpred_full_arity(Prop,MaxArity),(ArgUsed=<MaxArity))),
    discoverAndCorrectArgsIsa_from_right(Op,Prop,MaxArity,ArgsIn,ArgsOut),!.
discoverAndCorrectArgsIsa(Op,Prop,N,ArgsIn,ArgsOut):-discoverAndCorrectArgsIsa_from_left(Op,Prop,N,ArgsIn,ArgsOut),!.

discoverAndCorrectArgsIsa_from_right(_O,_Prop,_N1,[],[]):-!.
discoverAndCorrectArgsIsa_from_right(Op,Prop,N1,In,Out):- append(Args,[A],In),
   must((argIsa_op_call(Op,Prop,N1,Type),correctAnyType(Op,A,Type,AA))),
   N2 is N1-1,
   discoverAndCorrectArgsIsa_from_right(Op,Prop,N2,Args,AArgs),
   append(AArgs,[AA],Out).

discoverAndCorrectArgsIsa_from_left(_O,_Prop,_N1,[],[]):-!.
discoverAndCorrectArgsIsa_from_left(Op,Prop,N1,[A|Args],Out):-
   must((argIsa_op_call(Op,Prop,N1,Type),correctAnyType(Op,A,Type,AA))),
   N2 is N1+1,
   discoverAndCorrectArgsIsa_from_left(Op,Prop,N2,Args,AArgs),
    Out = [AA|AArgs].


mpred_full_arity({},A):-trace_or_throw(crazy_mpred_full_arity({}, A)).
mpred_full_arity(F,A):-arity(F,A),!.
mpred_full_arity(F,A):-grab_argsIsa(F,Types),show_call((functor(Types,F,A),assert_arity(F,A))),!.

:-export(correctAnyType/4).

is_valuespec(G):-is_ephemeral(G).
is_valuespec(G):-hasInstance(tCol,G).
is_valuespec(FT):-hasInstance(ttFormatType,FT).
is_valuespec(G):-evaluatableArg(G,_).

evaluatableArg(AA,_):-compound(AA),get_functor(AA,F),!,evaluatableFunctor(F).
evaluatableFunctor(isRandom).
evaluatableFunctor(isOptional).

% correctAnyType(_,A,_,A):-is_release.

correctAnyType(_, A,_Type,AA):- var(A),sanity(var(AA)),must_det(A=AA),!.
correctAnyType(Op,A,Type,AA):-  var(A),correctType(Op,A,Type,AA),sanity(var(AA)),must_det(A==AA),!.
correctAnyType(_, A,Type,AA):-  evaluatableArg(A,Type),dmsg(evaluatableArg(A,Type)),must_det(A=AA),!.
correctAnyType(Op,A,Type,AA):- var(Type),trace_or_throw(correctAnyType(Op,A,Type,AA)).
% TODO snags on new tpyes correctAnyType(Op,A,Type,AA):- correctType(Op,A,Type,AA),nonvar(AA),!.
correctAnyType(Op,A,Type,AA):- one_must(correctType(Op,A,Type,AA),A=AA).
correctAnyType(Op,A,Type,A):- dtrace,dmsg(warn(not(correctAnyType(Op,A,Type)))).



%  @set mudMoveDist 4

:-export(correctFormatType/4).
correctFormatType(Op,A,Type,AA):- var(A),correctType(Op,A,Type,AA),sanity(var(AA)),must_det(A==AA),!.
correctFormatType(Op,A,Type,AA):- var(Type),trace_or_throw(correctFormatType(Op,A,Type,AA)).
correctFormatType(Op,A,Type,AA):- correctType(Op,A,Type,AA),sanity(nonvar(AA)),!.
correctFormatType(Op,A,Type,AA):- tracing, correctType(Op,A,Type,AA).
correctFormatType(Op,A,Type,A):- dmsg(todo(not(correctFormatType(Op,A,Type)))),fail.

:-export(checkAnyType/4).

checkAnyType(Op,A,Type,AA):- var(A),correctType(Op,A,Type,AA),sanity(var(AA)),must_det(A==AA),!.
checkAnyType(Op,A,Type,AA):- correctType(Op,A,Type,AA),nonvar(AA),!.

correctAnyTypeOrFail(Op,A,Type,AA):- with_assertions(tlbugger:skipMust,checkAnyType(Op,A,Type,AA)).



:-thread_local thlocal:can_coerce/1.
correctType_gripe(Op,A,Fmt,AA):- hasInstance(ttFormatType,Fmt),!,trace_or_throw(correctType(is_ft_correctFormatType(Op,A,Fmt,AA))).
correctType_gripe(Op,A,Type,AA):- fail,atom(Type),must_equals(A,AA),
      dmsg(todo(isa_assert_type(Type))),
      % decl_type(Type),
      thlocal:can_coerce(Op),
      dmsg(warning(add(isa(A,Type)))),
      dtrace(add(isa(A,Type))),!.

correctType_gripe(Op,A,C,A):-sanity(ground(A)),dtrace, dmsg(todo(define(correctType(Op,A,C,'ConvertedArg')))),throw(retry(_)).
correctType_gripe(Op,A,Type,NewArg):-trace_or_throw(failure(correctType(Op,A,Type,NewArg))).

:- style_check(+singleton).

is_renamed_to(A,AA):- fail,atomic(A),not(A=[];A='';A=""),not(atom_concat(_,'Table',A)),not(atom_concat(_,'table',A)),
    atom_concat(Base,'able',A),atom_length(Base,AL),AL>2,!,atom_concat(Base,'Able',AA).

correctType(Op,A,Type,AA):-correctType0(Op,A,Type,AA).

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
correctType0(_ ,A,vtDirection,AA):- current_predicate(any_to_dir/2),!,any_to_dir(A,AA).
correctType0(_ ,A,vtDirection,AA):- must_equals(A,AA).
correctType0(Op,A,ftInteger,AA):-!,correctType0(Op,A,ftInt,AA).
correctType0(Op,A,ftAskable,AA):-!,must_equals_correct(query(ftAskable,Op),A,AA).
correctType0(_ ,A,ftInt,AA):- any_to_number(A,AA).
correctType0(_ ,A,ftNumber,AA):- any_to_number(A,AA).
correctType0(_ ,A,ftProlog,AA):- must_equals(A,AA).
correctType0(_ ,A,ftString,AA):- must(any_to_string(A,AA)).
correctType0(Op,A,ftTerm(_),AA):- must_equals_correct(Op,A,AA).
correctType0(_ ,A,ftVoprop,AA):- !, must(A=AA).
correctType0(Op,A,ftVoprop,AA):- is_list(A),!,maplist(correctTypeArg(Op,ftAskable),A,AA).
correctType0(Op,A,ftVoprop,AA):- !,with_assertions(thlocal:inVoprop,correctType0(Op,A,ftAskable,AA)).

correctType0(_ ,Obj,argIsaFn(Prop,N),AA):-must_equals(Obj,AA),
   ignore((thlocal:deduceArgTypes(_),
     sanity(N\=0),
      findall(OT,isa_asserted(Obj,OT),OType),
         show_call(deduce_argN(Prop,N,Obj,OType,argIsaFn(Prop,N))))),!.
correctType0(_ ,A,ftTerm,AA):- must_equals(A,AA).
correctType0(_ ,A,ftText,AA):- must_equals(A,AA).

correctType0(_ ,A,tPred,AA):- any_to_relation(A,AA).
correctType0(_ ,A,tFunction,AA):- any_to_relation(A,AA).
correctType0(_ ,A,tRelation,AA):- any_to_relation(A,AA).
correctType0(_ ,A,ftAtom,AA):- any_to_atom(A,AA).
correctType0(change(_,_),A,tCol,AA):- atom(A),deduced_is_tCol(A),must_equals(A,AA).
correctType0(change(_,_),A,tCol,AA):- compound(A),deduced_is_tCol(A),must_equals(A,AA).
correctType0(_ ,A,vtVerb,AA):- must_equals(A,AA).
correctType0(_ ,A,Type,AA):- compound(A),not(is_list(A)),atom(Type),functor_safe(A,Type,_), must_equals(A,AA).

correctType0(_ ,A,Type,AA):- compound(Type),contains_var(Type,isSelf),
   subst(Type,isSelf,A,Call1),subst(Call1,value,AA,Call2),!,
      show_call(is_asserted(Call2)),ignore(AA=A).

correctType0(_ ,A,Type,AA):- functor(Type,F,A),
   (A2 is A+2,current_predicate(F/A2)->show_call(is_asserted(call(Type,A,AA)));
   (A1 is A+1,current_predicate(F/A1)->show_call(is_asserted(call(Type,A))));
   fail),ignore(AA=A).

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
correctType0(Op,A,Super,AA):- hasInstance(ttFormatType,Super),req(genls(Sub,Super)),Sub\=Super,loop_check(correctType0(Op,A,Sub,AA)).

correctType0(Op,Arg,Props,NewArg):- compound(Props),
   Props=..[F|TypesL],
   functor(Props,F,A),
   A2 is A+1,
   is_asserted(arity(F,A2)),
   C=..[F,Arg|TypesL],
   correctArgsIsa(Op,C,CC),
   CC=..[F,NewArg|_].

correctType0(_ ,A,Type,AA):- not(hasInstance(ttFormatType,Type)),hasInstance(tCol,Type),isa_asserted(A,Type),!,must_equals(A,AA).
correctType0(_,A,_,_):- not(compound(A)),!,fail.
correctType0(Op,A,T,AAA):- once(correctArgsIsa(Op,A,AA)),A\=AA,!,correctType0(Op,AA,T,AAA).
correctType0(_ ,A,T,AA):- get_functor(A,F),is_asserted(resultIsa(F,T)),must_det(A=AA),!.
correctType0(_ ,A,T,AA):- get_functor(A,F),is_asserted(formatted_resultIsa(F,T)),must_det(A=AA),!.


correctTypeArg(Op,Type,A,AA):-correctType(Op,A,Type,AA).

must_equals_correct(Op,A,AA):-must(correctArgsIsa(Op,A,AA)).

% :- style_check(+singleton).

must_equals(A,AA):-must_det(A=AA).

deduced_is_tCol(A):- (thlocal:infSkipArgIsa->true; (hasInstance(tCol,A)->true;(fail,pfcAdd(isa(A,tCol))))),!.
:- style_check(+singleton).

:-export(any_to_value/2).
any_to_value(Var,Var):-var(Var),!.
any_to_value(V,Term):-atom(V),!,atom_to_value(V,Term).
any_to_value(A,V):-any_to_number(A,V).
any_to_value(A,A).


:-export(any_to_number/2).
any_to_number(N,N):- number(N),!.
any_to_number(ftDice(A,B,C),N):- ground(A),roll_dice(A,B,C,N),!.
any_to_number(A,N):-atom(A),atom_to_value(A,V),A\=V,any_to_number(V,N).
any_to_number(A,N):- catchv(number_string(N,A),_,fail).

:-export(atom_to_value/2).
atom_to_value(V,Term):-not(atom(V)),!,any_to_value(V,Term).
% 56
atom_to_value(V,Term):- catchv((read_term_from_atom(V,Term,[variable_names([])])),_,fail),!.
% 18d18+4000
atom_to_value(V,ftDice(T1,T2,+T3)):- atomic_list_concat_safe([D1,'d',D2,'+',D3],V), atom_to_value(D1,T1),atom_to_value(D2,T2),atom_to_value(D3,T3),!.
atom_to_value(V,ftDice(T1,T2,-T3)):- atomic_list_concat_safe([D1,'d',D2,'-',D3],V), atom_to_value(D1,T1),atom_to_value(D2,T2),atom_to_value(D3,T3),!.



any_to_relation(A,F):-atomic(A),!,any_to_atom(A,F).
any_to_relation(A,F):-functor_h(A,F).

roll_dice(Rolls,_,Bonus,Result):- Rolls < 0, !, Result is Bonus.
roll_dice(Rolls,Sided,Bonus,Result):- LessRolls is Rolls-1, roll_dice(LessRolls,Sided, Bonus + random(Sided) +1, Result).

call_argIsa_ForAssert(F,N,Type):-argIsa_known(F,N,Type),atom(Type),!,not(nonusefull_deduction_type(Type)),tCol(Type).


