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
          argIsa_call/4,
          argIsa_call_0/3,
          atom_to_value/2,
         % formattype/1,
          term_is_ft/2,
          pl_arg_type/2,
          p2c_dir2/2
          )).


:- include(dbase_i_header).
:- export correctArgsIsa/3.


:-export(split_name_type/3).
:- '$hide'(split_name_type/3).
split_name_type(Suggest,InstName,Type):- must_det(split_name_type_0(Suggest,NewInstName,NewType)),!,must((NewInstName=InstName,NewType=Type)),!.

split_name_type_0(S,P,C):- string(S),!,atom_string(A,S),split_name_type_0(A,P,C),!.
split_name_type_0(FT,FT,ttFormatType):-ttFormatType(FT),dmsg(trace_or_throw(ttFormatType(FT))),fail.
split_name_type_0(T,T,C):- compound(T),functor(T,C,_),!.
split_name_type_0(T,T,C):- notrace((once(atomic_list_concat_safe([CO,'-'|_],T)),atom_string(C,CO))).
split_name_type_0(T,T,C):- notrace((atom(T),atom_codes(T,AC),last(AC,LC),is_digit(LC),append(Type,Digits,AC),catchv(number_codes(_,Digits),_,fail),atom_codes(CC,Type),!,i_name(t,CC,C))).
split_name_type_0(C,P,C):- var(P),atom(C),i_name(i,C,I),gensym(I,P),!.

formattype_guessable(S):- is_asserted(mudFtInfo(S,_)).
formattype_guessable(S):- is_asserted(isa(S,ttFormatted)).


term_is_ft(Term,Type):- var(Term),!,member(Type,[ftVar,ftProlog]).
term_is_ft(_ANY,Type):- Type==ftVar,!,fail.
term_is_ft(Term,Type):- formattype_guessable(Type),trans_subft_info(Type,How),   
   with_no_assertions(thlocal:infSkipArgIsa, ( correctFormatType(query(_HLDS,_OldV),Term,How,NewTerm),!,
   sameArgTypes(NewTerm,Term))).

ft_info_how(FT,Info):- must(nonvar(FT)),  is_asserted(mudFtInfo(FT,Info)).
ft_info_how(FT,ftFormFn(FT)):- compound(FT), ttFormatted(FT).

trans_subft_info(FT,Info):-ft_info_how(FT,Info).
trans_subft_info(FT,Info):-ttFormatType(FT),trans_subft(FT,Sub),ft_info_how(Sub,Info).
trans_subft(FT,Sub):-subFormat(FT,Sub).
trans_subft(FT,Sub):-subFormat(FT,A),subFormat(A,Sub).
trans_subft(FT,Sub):-subFormat(FT,A),subFormat(A,B),subFormat(B,Sub).

sameArgTypes(A,C):-same(A,C);(pl_arg_type(C,CT),pl_arg_type(A,AT),!,colsOverlap(AT,CT)).
colsOverlap(AT,AT).


/*
pl_arg_type_or_functor(Arg,Type):- pl_arg_type(Arg,T) , 
 (T==ftCompound -> functor(Arg,Type,_); 
  ( (T==ftListFn(_),Arg=[_|_])-> T=[Type|_] ;
         Type=T)) .
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


mpred_arity_pred(P):- nonvar(P),arg(_,a(arity,predArity,arityMax,arityMin),P).
mpred_arity_pred(mpred_arity).

as_one_of(Types,Type):-nonvar(Type),tCol(Type),!,member(Type,Types).
as_one_of([Type],TypeO):-!,same_arg(same_or(subclass),Type,TypeO).
as_one_of(Types,isOneOf(Types)).


argIsa_call(Op,_:F,N,Type):-!,argIsa_call(Op,F,N,Type),!.
argIsa_call(Op,F/_,N,Type):- !,argIsa_call(Op,F,N,Type),!.
argIsa_call(Op,Func,N,Type):- compound(Func),!,functor(Func,F,_),argIsa_call(Op,F,N,Type),!.
argIsa_call(Op,F,N,Type):-hotrace((loop_check((argIsa_call_nt(Op,F,N,Type),!),Type=ftTerm),must(nonvar(Type)))).

argIsa_call_nt(_O,F,N,Type):-argIsa_call_nt(F,N,Type).


:- decl_mpred_hybrid(argIsa/3).
user:ruleBackward(argIsa(F,N,Isa),argIsa_call(F,N,Isa)).

argIsa_call(F,N,Type):- argIsa_known(F,N,Type),!.
argIsa_call(F,N,Type):- argIsa_call_1(F,N,Type),!.

argIsa_known(F,N,Type):- argIsa_call_0(F,N,Type),!.
argIsa_known(F,N,Type):- argIsa_asserted(F,N,Type),!.

to_format_type(FT,FT):-ttFormatType(FT),!.
to_format_type(COL,FT):-formatted_resultIsa(FT,COL),!.
to_format_type(COL,FT):-resultIsa(FT,COL),ttFormatType(FT),!.
to_format_type(COL,ftTerm(COL)).

argIsa_ft(F,N,FTO):-must((argIsa_known(F,N,FT),to_format_type(FT,FTO))),!.


argIsa_call_nt(F,N,Type):- once(var(F);not(number(N))),dtrace,once(var(F);not(number(N))),trace_or_throw(once(var(F);not(number(N)))->argIsa_call(F,N,Type)).
argIsa_call_nt(F,N,Type):- argIsa_call(F,N,Type),!.
argIsa_call_nt(F,N,Type):- findall(T,argIsa_call_0(F,N,Type),T),Types=[_|_],!,as_one_of(Types,Type),!.

:-export(argIsa_call_0/3).
argIsa_call_0(argIsa,1,tRelation).
argIsa_call_0(argIsa,2,ftInt).
argIsa_call_0(argIsa,3,tCol).
argIsa_call_0(comment,2,ftString).
argIsa_call_0(isKappaFn,1,ftVar).
argIsa_call_0(isKappaFn,2,ftAskable).
argIsa_call_0(isInstFn,1,tCol).
argIsa_call_0(Col,1,Col):-isa(Col,tCol).
argIsa_call_0(mudFtInfo,1,ttFormatType).
argIsa_call_0(mudFtInfo,2,ftCallable).
argIsa_call_0(ttFormatted,1,ttFormatType).
argIsa_call_0(isa,1,ftID).
argIsa_call_0(export,_,ftTerm).
argIsa_call_0(decl_mpred_hybrid,_,ftTerm).

argIsa_call_0(prop,2,ftVoprop).
argIsa_call_0(isa,2,tCol).

argIsa_call_0(mpred_prop,1,tPred).
argIsa_call_0(mpred_prop,2,ftVoprop).
argIsa_call_0(predicates,1,ftListFn(ftTerm)).
argIsa_call_0(resultIsa,2,tCol).
argIsa_call_0(predTypeMax,1,tPred).
argIsa_call_0(predTypeMax,2,tCol).
argIsa_call_0(predTypeMax,3,ftInt).

argIsa_call_0(predInstMax,1,tObj).
argIsa_call_0(predInstMax,2,tPred).
argIsa_call_0(predInstMax,3,ftInt).
argIsa_call_0(must,1,ftAskable).

argIsa_call_0(predModule,1,tPred).
argIsa_call_0(predModule,2,ftAtom).
% argIsa_call_0(user:agent_text_command,_,ftTerm).
argIsa_call_0(ruleEquiv,_,ftTerm).
argIsa_call_0(F,N,Type):-between(1,2,N),argIsa_call_3(F,Type).
argIsa_call_0(class_template,N,Type):- (N=1 -> Type=tCol;Type=ftListFn(ftVoprop)).
argIsa_call_0(Arity,N,T):-mpred_arity_pred(Arity),!,arg(N,vv(tPred,ftInt,tCol),T).
argIsa_call_0(F,2,ftString):-member(F,[descriptionHere,nameftStrings,mudKeyword]).

argIsa_call_0(F,N,Type):-hasInstance(macroDeclarer,F),!,(N=1 -> Type=F ; Type=ftTerm(ftVoprop)).
argIsa_call_0(F,N,Type):-hasInstance(tCol,F),!,(N=1 -> Type=F ; Type=ftTerm(ftVoprop)).

argIsa_call_0(F,N,ftTerm):-N = 1, current_predicate(F/N).
argIsa_call_0(F,N,ftAskable):- current_predicate(F/A),N =< A, functor(P,F,A), predicate_property(P,meta_predicate(P)),arg(N,P,Number),number(Number),!.



argIsa_call_3(WP,tPred(predArity(2))):-member(WP,[predProxyRetract,predProxyAssert,predProxyQuery,genlPreds,genlInverse]).
argIsa_call_3(disjointWith,tCol).
argIsa_call_3(ttFormatted,ftTerm).
argIsa_call_3(ftFormFn,ftTerm).
argIsa_call_3(mudTermAnglify,ftTerm).
argIsa_call_3(subclass,tCol).
% argIsa_call_3(subclass,ttFormatType).

% argIsa_call_0(HILOG,_,term):-hilog_functor(HILOG).

argIsa_asserted(F,N,Type):- dbase_t(argIsa,F,N,Type),!.
argIsa_asserted(F,N,Type):- argIsa_call_0(F,N,Type),!.
argIsa_asserted(F,N,Type):- get_mpred_prop(F,argIsa(N,Type)),nonvar(Type),add(argIsa(F,N,Type)),!.
argIsa_asserted(F,N,Type):- show_call_failure((grab_argsIsa(F,Types),arg(N,Types,Type),nonvar(Type),add(argIsa(F,N,Type)))),!.
argIsa_asserted(F,N,Type):- grab_argsIsa2(F,Types),must((arg(N,Types,Type),nonvar(Type),add(argIsa(F,N,Type)))),!.
argIsa_asserted(F,N,argIsaFn(F,N)).

grab_argsIsa(resultIsa,resultIsa(tFunction,tCol)).
% grab_argsIsa(F,Types):- call_typelect([flag(+firstValue),+debugOnError,+deducedSimply],mpred_prop(F,predArgTypes(Types))).
grab_argsIsa(F,Types):- mpred_prop(F,predArgTypes(Types)),get_functor(Types,F),assert_predArgTypes_fa(F,Types),!.

grab_argsIsa(was_imported_kb_content, A):-trace_or_throw(crazy_grab_argsIsa(was_imported_kb_content, A)).
grab_argsIsa(F,Types):- is_asserted(mudFtInfo(Types,_)),compound(Types),get_functor(Types,F),assert_predArgTypes_fa(F,Types).
grab_argsIsa(F,Types):- is_asserted(ttFormatted(Types)),compound(Types),get_functor(Types,F),assert_predArgTypes_fa(F,Types).
grab_argsIsa(F,Types):- is_asserted(vtActionTemplate(Types)),compound(Types),get_functor(Types,F),assert_predArgTypes_fa(F,Types).
grab_argsIsa(F,Types):- current_predicate(get_all_templates/1),get_all_templates(Types),ground(Types),get_functor(Types,F),assert_predArgTypes_fa(F,Types).
grab_argsIsa(F,Types):- hasInstance(_,Types),compound(Types),ground(Types),get_functor(Types,F),assert_predArgTypes_fa(F,Types).
grab_argsIsa2(F,Types):- fail,deducedSimply(mpred_prop(F,predArgTypes(Types))).


argIsa_call_1(Var,2,ftTerm):-not(must(not(is_asserted(tCol(Var))))),dmsg(trace_or_throw( argIsa_call_1(Var,2,ftTerm))),fail.
argIsa_call_1(Prop,N1,Type):- is_2nd_order_holds(Prop),dmsg(todo(define(argIsa_call(Prop,N1,'Second_Order_TYPE')))),dumpST,dtrace,
   Type=argIsaFn(Prop,N1).
argIsa_call_1(F,_,ftTerm(ftProlog)):-member(F/_,
                                [
                                argIsa/3,
                                predProxyAssert/2,
                                 negate_wrapper0/2,
                                registered_module_type/2,       
                                ruleBackward/2,
                                formatted_resultIsa/2,
                                bracket/3]).

argIsa_call_1(Prop,N1,Type):- mpred_arity(Prop,Arity),dmsg(todo(define(argIsa_call(Prop,N1,'_TYPE')))),must(N1=<Arity),must( Type=argIsaFn(Prop,N1)).

argIsa_call_1(Prop,N1,Type):- dmsg(todo(define(argIsa_call(Prop,N1,'_TYPE')))),must( Type=argIsaFn(Prop,N1)).
argIsa_call_1(_,_,ftTerm).
argIsa_call_1(mudFacing,_,ftTerm).


:-export(correctArgsIsa/2).
correctArgsIsa(In,Out):- correctArgsIsa(query(must,dbase_t),In,Out),!.

:-export(correctArgsIsa/3).
correctArgsIsa(_,NC,NC):-not(compound(NC)),!.
correctArgsIsa(_,G,G):- (is_release; bad_idea;  thlocal:infSkipArgIsa),!.
correctArgsIsa(Op,M:G,MAA):- nonvar(M),!,correctArgsIsa(Op,G,GG),M:GG=MAA.
correctArgsIsa(_,G,GG):- get_functor(G,F,A),arg(_,vv(subclass/_,mpred_prop/_,mpred_arity/_,subclass/_,mudDescription/_,ruleEquiv/_,formatted_resultIsa/_,resultIsa/_,mudFtInfo/_),F/A),!,must_equals(G,GG).
correctArgsIsa(_,G,GG):- get_functor(G,F),hasInstance(macroDeclarer,F),!,must_equals(G,GG).
correctArgsIsa(_,G,GG):- thlocal:trust_argIsas, !,must_equals(G,GG).
correctArgsIsa(Op,G,GG):- correctArgsIsa0(Op,G,GG),nonvar(GG),!.
correctArgsIsa(Op,G,GG):- grtrace,correctArgsIsa0(Op,G,GG).

:-export(correctArgsIsa/4).
correctArgsIsa(Op,A,Type,AA):- trace_or_throw(warn(not(correctArgsIsa(Op,A,Type,AA)))).


logical_functor_ft(F):-is_logical_functor(F).
logical_functor_ft((':-')).
logical_functor_ft((',')).

list_to_callform([P|ARGS],_,CALL):-atom(P),!,CALL=..[P|ARGS].
list_to_callform(ARGS,Functor,CALL):-CALL=..[Functor|ARGS].

correctArgsIsa0(Op,[PRED|ARGS],RESULT):-!,correctArgsIsa00(Op,[PRED|ARGS],RESULT).
correctArgsIsa0(Op,A,RESULTC):-A=..[PRED|ARGS],!,correctArgsIsa00(Op,[PRED|ARGS],RESULT), list_to_callform(RESULT,dbase_t,RESULTC).

correctArgsIsa00(_ ,[Prop|Args],AA):-stack_check(1000), var(Prop),!,AA=[Prop|Args].
correctArgsIsa00(Op,[KP,Prop|Args],AA):-is_holds_true(KP),!,correctArgsIsa00(Op,[Prop|Args],AA).
correctArgsIsa00(Op,[KP,Prop|Args],[KP|AArgs]):-logical_functor_ft(KP),!,correctAnyType(Op,[Prop|Args],ftListFn(ftAskable),AArgs).
correctArgsIsa00(Op,[KP,Prop|Args],[KP|AA]):-is_holds_false(KP),!,correctArgsIsa00(Op,[KP,Prop|Args],AA).
%correctArgsIsa00(_ ,[Prop,Arg],[Prop,Arg]):- !.
correctArgsIsa00(Op,[Prop,ArgI],[Prop,ArgO]):- mpred_prop(Prop,tCol),!, correctAnyType(query(ftID,Op),ArgI,Prop,ArgO).
correctArgsIsa00(Op,[Prop|Args],[Prop|AArgs]):- discoverAndCorrectArgsIsa(Op,Prop,1,Args,AArgs).

discoverAndCorrectArgsIsa(Op,Prop,_,ArgsIn,ArgsOut):- length(ArgsIn,ArgUsed),show_call_failure((mpred_full_arity(Prop,MaxArity),(ArgUsed=<MaxArity))),
    discoverAndCorrectArgsIsa_from_right(Op,Prop,MaxArity,ArgsIn,ArgsOut),!.
discoverAndCorrectArgsIsa(Op,Prop,N,ArgsIn,ArgsOut):-discoverAndCorrectArgsIsa_from_left(Op,Prop,N,ArgsIn,ArgsOut),!.

discoverAndCorrectArgsIsa_from_right(_O,_Prop,_N1,[],[]):-!.
discoverAndCorrectArgsIsa_from_right(Op,Prop,N1,In,Out):- append(Args,[A],In),
   must((argIsa_call(Op,Prop,N1,Type),correctAnyType(Op,A,Type,AA))),
   N2 is N1-1,
   discoverAndCorrectArgsIsa_from_right(Op,Prop,N2,Args,AArgs),
   append(AArgs,[AA],Out).

discoverAndCorrectArgsIsa_from_left(_O,_Prop,_N1,[],[]):-!.
discoverAndCorrectArgsIsa_from_left(Op,Prop,N1,[A|Args],Out):-
   must((argIsa_call(Op,Prop,N1,Type),correctAnyType(Op,A,Type,AA))),
   N2 is N1+1,
   discoverAndCorrectArgsIsa_from_left(Op,Prop,N2,Args,AArgs),
    Out = [AA|AArgs].


mpred_full_arity(F,A):-mpred_arity(F,A),!.
mpred_full_arity(F,A):-grab_argsIsa(F,Types),show_call(functor(Types,F,A)),assert_arity(F,A),!.


:-export(correctAnyType/4).


unknowableArg(AA,_):-compound(AA),get_functor(AA,F),!,unknowableArgF(F).
unknowableArgF(isRandom).

% correctAnyType(_,A,_,A):-is_release.

correctAnyType(_, A,_Type,AA):- var(A),sanity(var(AA)),must_det(A=AA),!.
correctAnyType(Op,A,Type,AA):-  var(A),correctType(Op,A,Type,AA),sanity(var(AA)),must_det(A==AA),!.
correctAnyType(_, A,Type,AA):-  unknowableArg(A,Type),dmsg(unknowableArg(A,Type)),must_det(A=AA),!.
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



:-decl_thlocal thlocal:can_coerce/1.
correctType_gripe(Op,A,Fmt,AA):- ttFormatType(Fmt),!,trace_or_throw(correctType(is_ft_correctFormatType(Op,A,Fmt,AA))).
correctType_gripe(Op,A,Type,AA):- fail,atom(Type),must_equals(A,AA),
      dmsg(todo(isa_assert_type(Type))),
      % decl_type(Type),
      thlocal:can_coerce(Op),dmsg(warning(add(isa(A,Type)))),dtrace,
      add(isa(A,Type)),!.

correctType_gripe(Op,A,C,A):-sanity(ground(A)),dtrace, dmsg(todo(define(correctType(Op,A,C,'ConvertedArg')))),throw(retry(_)).
correctType_gripe(Op,A,Type,NewArg):-trace_or_throw(failure(correctType(Op,A,Type,NewArg))).

:- style_check(+singleton).

is_renamed_to(A,AA):- fail,atomic(A),not(A=[];A='';A=""),not(atom_concat(_,'Table',A)),not(atom_concat(_,'table',A)),
    atom_concat(Base,'able',A),atom_length(Base,AL),AL>2,!,atom_concat(Base,'Able',AA).

correctType(Op,A,Type,AA):-correctType0(Op,A,Type,AA).

correctType0(change(_,_),A,T,AA):- A==T,!,must_equals(A,AA).
correctType0(_O,A,T,AA):- A==T,!,must_equals(A,AA).
correctType0(Op,A,Type,AA):- var(Type),trace_or_throw(correctType0(Op,A,Type,AA)).
correctType0(_O,A,Type,AA):- (var(A);var(Type)),!, must(must_equals(A,AA)).

correctType0(Op,A,'&'(Type1,Type2),AA):-var(Type2),!,correctType0(Op,A,Type1,AA).
correctType0(Op,A,'&'(Type1,Type2),AAA):-!,correctType0(Op,A,Type1,AA),correctType0(Op,AA,Type2,AAA).


correctType0(_O,A,ftCallable,AA):- must_equals(A,AA).
correctType0(Op,A,ftID,AA):- must_equals_correct(query(ftID,Op),A,AA),!.
correctType0(query(ftID,Op),A,ftAction,AA):- must_equals_correct(Op,A,AA),!.

correctType0(Op,A,Type,AAA):-is_renamed_to(A,AA),!,must(correctType0(Op,AA,Type,AAA)).
correctType0(Op,+A,Type,+AA):-nonvar(A),!,correctType0(Op,A,Type,AA).
correctType0(Op,-A,Type,-AA):-nonvar(A),!,correctType0(Op,A,Type,AA).
correctType0(_O,A,vtDirection,AA):- current_predicate(any_to_dir/2),!,any_to_dir(A,AA).
correctType0(_O,A,vtDirection,AA):- must_equals(A,AA).
correctType0(Op,A,ftInteger,AA):-!,correctType0(Op,A,ftInt,AA).
correctType0(Op,A,ftAskable,AA):-!,must_equals_correct(query(ftAskable,Op),A,AA).
correctType0(_O,A,ftInt,AA):- any_to_number(A,AA).
correctType0(_O,A,ftNumber,AA):- any_to_number(A,AA).
correctType0(_O,A,ftProlog,AA):- must_equals(A,AA).
correctType0(_O,A,ftString,AA):- must(any_to_string(A,AA)).
correctType0(Op,A,ftTerm(_),AA):- must_equals_correct(Op,A,AA).
correctType0(Op,A,ftVoprop,AA):- is_list(A),!,maplist(correctTypeArg(Op,ftAskable),A,AA).
correctType0(Op,A,ftVoprop,AA):- !,with_assertions(thlocal:inVoprop,correctType0(Op,A,ftAskable,AA)).

correctType0(_O,Obj,argIsaFn(Prop,N),AA):-must_equals(Obj,AA),
   ignore((thlocal:deduceArgTypes(_),
     sanity(N\=0),
      findall(OT,isa(Obj,OT),OType),
         show_call(deduce_argN(Prop,N,Obj,OType,argIsaFn(Prop,N))))),!.
correctType0(_O,A,ftTerm,AA):- must_equals(A,AA).
correctType0(_O,A,ftText,AA):- must_equals(A,AA).

correctType0(_O,A,tPred,AA):- any_to_relation(A,AA).
correctType0(_O,A,tFunction,AA):- any_to_relation(A,AA).
correctType0(_O,A,tRelation,AA):- any_to_relation(A,AA).
correctType0(_O,A,ftAtom,AA):- any_to_atom(A,AA).
correctType0(change(_,_),A,tCol,AA):- atom(A),decl_type(A),must_equals(A,AA).
correctType0(change(_,_),A,tCol,AA):- compound(A),decl_type(A),must_equals(A,AA).
correctType0(_O,A,vtVerb,AA):- must_equals(A,AA).
correctType0(_O,A,Type,AA):- compound(A),not(is_list(A)),atom(Type),functor_safe(A,Type,_), must_equals(A,AA).

correctType0(_O,A,prologCall(Code),A):- !,subst(Code,isSelf,A,Call),with_assertions(thlocal:no_arg_type_error_checking,req(Call)).   
correctType0(_O,A,req(Code),A):- !,subst(Code,isSelf,A,Call),with_assertions(thlocal:no_arg_type_error_checking,req(Call)).   
correctType0(_O,A,Type,AA):- compound(Type),contains_var(Type,isSelf),predicate_property(Type,_),!,
   subst(Type,isSelf,A,Call1),
   subst(Call1,value,AA,Call2),!,
      show_call(Call2),ignore(AA=A).

correctType0(query(HLDS,Must),A,xyzFn(Region, ftInt, ftInt, ftInt),xyzFn(AA, _, _, _)):-atom(A),correctAnyType(query(HLDS,Must),A,Region,AA).
correctType0(_Op,A,ftListFn(_),AA):- A == [],!,A=AA.
correctType0(Op,[A|AA],ftListFn(T),[L|LIST]):-!, correctType0(Op,A,T,L), correctType0(Op,AA,ftListFn(T),LIST).
correctType0(Op,A,ftListFn(T),[OT]):-!,correctAnyType(Op,A,T,OT).
correctType0(_O,A,same(T),AA):-must_equals(T,AA),must_equals(A,AA).
correctType0(Op,A,isOneOf(List),AA):-!,member(Type,List),correctType0(Op,A,Type,AA).

correctType0(_O,[],ftFormFn([]),[]):-!.
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
correctType0(_O,[],[],[]):-!.
correctType0(Op,[H|T],[H2|T2],[H3|T3]):-!, correctAnyType(Op,H,H2,H3),correctType0(Op,T,T2,T3).

correctType0(Op,Args,Types,NewArgs):-compound(Args), compound(Types),
   functor(Args,F,N),functor(Types,F,N),functor(NewArgs,F,N),
   Args=..[F|ArgsL],
   Types=..[F|TypesL],
   NewArgs=..[F|NewArgsL],
   correctAnyType(Op,ArgsL,TypesL,NewArgsL).

correctType0(Op,A,Fmt,AA):- trans_subft_info(Fmt,Code),!,correctType0(Op,A,Code,AA).
correctType0(Op,A,Super,AA):- ttFormatType(Super),req(subclass(Sub,Super)),Sub\=Super,correctType0(Op,A,Sub,AA).

correctType0(Op,Arg,Props,NewArg):- compound(Props),
   Props=..[F|TypesL],
   C=..[F,Arg|TypesL],
   correctArgsIsa(Op,C,CC),
   CC=..[F,NewArg|_].

correctType0(Op,A,T,AAA):-  compound(A),once(correctArgsIsa(Op,A,AA)),A\=AA,!,correctType0(Op,AA,T,AAA).

correctType0(_O,A,Type,AA):-not(ttFormatType(Type)),is_asserted(tCol(Type)),is_asserted(isa(A,Type)),!,must_equals(A,AA).

correctType0(_Op,A,T,AA):- get_functor(A,F),resultIsa(F,T),must_det(A=AA),!.
correctType0(_Op,A,T,AA):- get_functor(A,F),formatted_resultIsa(F,T),must_det(A=AA),!.


correctTypeArg(Op,Type,A,AA):-correctType(Op,A,Type,AA).

must_equals_correct(Op,A,AA):-must(correctArgsIsa(Op,A,AA)).

% :- style_check(+singleton).

must_equals(A,AA):-must_det(A=AA).

  
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



learnArgIsa(P,N,_):-argIsa_asserted(P,N,_),!.
learnArgIsa(P,N,T):-dmsg((skipping(learnArgIsa(P,N,T)))),!.
learnArgIsa(P,N,T):-grtrace, add(argIsa(P,N,T)).

learnArgIsaInst(K,Num,Arg):-integer(Arg),!,learnArgIsa(K,Num,ftInt).
learnArgIsaInst(K,Num,Arg):-number(Arg),!,learnArgIsa(K,Num,ftNumber).
learnArgIsaInst(_,_,_).



deduce_facts(Fact,isa(Arg,Type)):- test_tl(do_slow_kb_op_now),slow_kb_op(deduce_argIsa_facts(Fact,Arg,Type)).

call_argIsa_ForAssert(F,N,Type):-argIsa_call(F,N,Type),atom(Type),!,not(nonusefull_deduction_type(Type)),tCol(Type).

nonusefull_deduction_type(ftTerm).
nonusefull_deduction_type(ftVoprop).
nonusefull_deduction_type(vtDirection).
nonusefull_deduction_type(Type):-ttSpatialType(Type),!,fail.
nonusefull_deduction_type(tObj).
nonusefull_deduction_type(Type):-is_asserted(ttFormatType(Type)).

assert_deduced_arg_isa_facts(Fact):- !, ignore(((ground(Fact),forall(deduce_argIsa_facts(Fact,Arg,Type),add(isa(Arg,Type)))))).

assert_deduced_arg_isa_facts(Fact):- slow_kb_op(assert_deduced_arg_isa_facts_0(Fact)),!.
assert_deduced_arg_isa_facts_0(Fact):- ignore(((ground(Fact),forall(deduce_argIsa_facts(Fact,Arg,Type),add(isa(Arg,Type)))))).


deduce_argIsa_facts(Fact,Arg,Type):- ground(Fact), functor(Fact,F,A),A>1, deduce_from_predicate(F), arg(N,Fact,Arg),ground(Arg),
   call_argIsa_ForAssert(F,N,Type),sanity(atom(Type)),sanity(ground(Arg)).

never_deduce_from_predicate(isa).
never_deduce_from_predicate(mpred_prop).
never_deduce_from_predicate(mpred_arity).
never_deduce_from_predicate(subclass).
never_deduce_from_predicate(typeProps).
never_deduce_from_predicate(P):-mpred_arity(P,1).
never_deduce_from_predicate(P):-mpred_prop(P,ftCallable).
never_deduce_from_predicate(P):-argIsa_asserted(P,_,tCol).
never_deduce_from_predicate(P):-argIsa_asserted(P,_,ftVoprop).

deduce_from_predicate(Never):-never_deduce_from_predicate(Never),!,fail.
deduce_from_predicate(P):-mpred_prop(P,_).
deduce_from_predicate(_).


