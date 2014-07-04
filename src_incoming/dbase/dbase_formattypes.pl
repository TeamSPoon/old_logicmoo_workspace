/** <module> 
% This is mainly used by the moo_loader but also needed everywhere
%
% Project Logicmoo: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
% =======================================================
:- module(dbase_formattypes, [
          term_is_ft/2,
          formattype/1,
          format_complies/3,
	  %        argIsa_call_0/3,
          any_to_value/2,
          any_to_number/2,
          atom_to_value/2,
          any_to_dir/2]).

:- include(logicmoo('vworld/moo_header.pl')).

term_is_ft(Term,Type):-
   moo:ft_info(Type,How),
   format_complies(Term,How,NewTerm),
   ignore(NewTerm=Term).


formattype(S):- moo:ft_info(S,_).
formattype(S):- moo:subft(S,_).
% formattype(S):- moo:subclass(S,formattype).
% formattype(S):-mud_isa(S,formattype).

/*
is_decl_ft_except(S,List):-
   moo:ft_info(S,_);
   not((member(S,List), 
      ((moo:subft(S,S2) ,
        is_decl_ft_except(S2,[S|List]) ;
             ((moo:subft(S3,S) , is_decl_ft_except(S3,[S,S2|List]))))))).
*/

moo:ft_info(atom,atom(self)).
moo:ft_info(apath(region,dir),formatted).
moo:ft_info(string,string(self)).
moo:ft_info(number,number(self)).
moo:ft_info(type,type(self)).
moo:ft_info(dir,any_to_dir(self,_)).
moo:ft_info(dice(int,int,int),formatted).
moo:ft_info(xyz(region,int,int,int),formatted).
moo:ft_info(list(type),formatted).
moo:ft_info(term,nonvar(self)).
moo:ft_info(id,nonvar(self)).
moo:ft_info(prolog,true).
moo:ft_info(rest,true).
moo:ft_info(var,var(self)).
moo:ft_info(action(prolog),formatted).

moo:subft(var,prolog).
moo:subft(term,prolog).
moo:subft(atom,term).
moo:subft(string,term).
% moo:subft(number,term).
moo:subft(id,term).

moo:subft(int,integer).
moo:subft(integer,number).
moo:subft(dice,int).

format_complies(A,Type,A):- var(Type),!,trace,throw(failure(format_complies(A,Type))).
format_complies(A,string,AA):- ignoreOnError(text_to_string(A,AA)).
format_complies(A,int,AA):- any_to_number(A,AA).
format_complies(A,number,AA):- any_to_number(A,AA).
format_complies(A,integer,AA):- any_to_number(A,AA).
format_complies(A,Fmt,AA):- moo:ft_info(Fmt,formatted),!,format_complies(A,formatted(Fmt),AA).
format_complies(A,Fmt,A):- moo:ft_info(Fmt,Code),!,subst(Code,self,A,Call),Call.   
format_complies(A,number,AA):- must(any_to_number(A,AA)).
format_complies(A,dir,AA):- any_to_dir(A,AA).
format_complies([A|AA],list(T),LIST):-!,findall(OT,((member(O,[A|AA]),format_complies(O,T,OT))),LIST).
format_complies(A,list(T),[OT]):-!,format_complies(A,T,OT).
format_complies(A,same(A),A):-!.
format_complies([],formatted([]),[]):-!.
format_complies([H|T],formatted([H2|T2]),[H3|T3]):-
   format_complies(H,H2,H3),
   format_complies(T,formatted(T2),T3).
format_complies(Args,formatted(Types),NewArgs):- compound(Args),compound(Types),
   functor(Args,F,N),functor(Types,F,N),functor(NewArgs,F,N),
   Args=..[F|ArgsL],
   Types=..[F|TypesL],
   NewArgs=..[F|NewArgsL],!,   
   format_complies(ArgsL,TypesL,NewArgsL).
format_complies(A,Super,AA):- moo:subft(Sub,Super),format_complies(A,Sub,AA).
  


any_to_value(V,Term):-atom(V),!,atom_to_value(V,Term).
any_to_value(A,A).


any_to_number(N,N):- number(N),!.
any_to_number(dice(A,B,C),N):- ground(A),roll_dice(A,B,C,N),!.
any_to_number(A,N):-atom(A),atom_to_value(A,V),A\=V,any_to_number(V,N).
any_to_number(A,N):- catch(number_string(N,A),_,fail).


roll_dice(Rolls,_,Bonus,Result):- Rolls < 0, !, Result is Bonus.
roll_dice(Rolls,Sided,Bonus,Result):- LessRolls is Rolls-1, roll_dice(LessRolls,Sided, Bonus + random(Sided) +1, Result).


atom_to_value(V,Term):-not(atom(V)),!,any_to_value(V,Term).
% 56
atom_to_value(V,Term):- catch((read_term_from_atom(V,Term,[variable_names([])])),_,fail),!.
% 18d18+4000
atom_to_value(V,dice(T1,T2,+T3)):- atomic_list_concat_safe([D1,'d',D2,'+',D3],V), atom_to_value(D1,T1),atom_to_value(D2,T2),atom_to_value(D3,T3),!.
atom_to_value(V,dice(T1,T2,-T3)):- atomic_list_concat_safe([D1,'d',D2,'-',D3],V), atom_to_value(D1,T1),atom_to_value(D2,T2),atom_to_value(D3,T3),!.

any_to_dir(D,D):-var(D),!.
any_to_dir(D,D):-dir_offset(D,_,_,_,_),!.
any_to_dir(A,D):-p2c_dir2(D,A),!.
any_to_dir(S,D):-string(S),string_to_atom(S,A),any_to_dir(A,D),!.
any_to_dir(D,O):-atom(D),sub_atom(D, 0, 1, _, S),toLowercase(S,L),p2c_dir2(L,O),!.

p2c_dir2('s','South-Directly').
p2c_dir2('w','West-Directly').
p2c_dir2('u','Up-Directly').
p2c_dir2('d','Down-Directly').
p2c_dir2('e','East-Directly').
p2c_dir2('n','North-Directly').

end_of_file.

/** <module> 
% This is mainly used by the moo_loader but also needed everywhere
%
% Project LogicMoo: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
% =======================================================

:-decl_mpred(subft,2).

as_one_of(Types,Type):-nonvar(Type),is_type(Type),!,member(Type,Types).
as_one_of([Type],TypeO):-!,same_arg(same_or(subclass),Type,TypeO).
as_one_of(Types,oneOf(Types)).

argIsa_call(Op,F,N,Type):-hotrace(loop_check(argIsa_call_nt(Op,F,N,Type),Type=term)),must(nonvar(Type)).

argIsa_call_nt(_O,F,N,Type):-argIsa_call_nt(F,N,Type).

argIsa_call_nt(F,N,Type):- once(var(F);not(number(N))),dtrace,once(var(F);not(number(N))),trace_or_throw(once(var(F);not(number(N)))->argIsa_call(F,N,Type)).
argIsa_call_nt(_:F,N,Type):-!,argIsa_call_nt(F,N,Type),!.
argIsa_call_nt(F/_,N,Type):- !,argIsa_call_nt(F,N,Type),!.
argIsa_call_nt(F,N,Type):- argIsa_call_0(F,N,Type),!.
argIsa_call_nt(F,N,Type):- argIsa_asserted(F,N,Type),!.
argIsa_call_nt(F,N,Type):- is_ArgsIsa(F,_,Templ),arg(N,Templ,Type),nonvar(Type),!,trace_or_throw((is_ArgsIsa(F,_,Templ),arg(N,Templ,Type),nonvar(Type))).
argIsa_call_nt(F,N,Type):- argIsa_call_1(F,N,Type),!.
argIsa_call_nt(F,N,Type):- findall(T,argIsa_call_0(F,N,Type),T),Types=[_|_],!,as_one_of(Types,Type),!.

argIsa_call_0(F/_,N1,Type):-!,argIsa_call_0(F,N1,Type).
argIsa_call_0(agent_text_command,_,term).
argIsa_call_0(comment,2,string).
argIsa_call_0(isa,1,argIsaFn(isa,1)).
argIsa_call_0(isa,2,type).
argIsa_call_0(directions,2,list(dir)).
argIsa_call_0(equivRule,_,term).
argIsa_call_0(formatted,_,term).
argIsa_call_0(F,N,Type):-is_type(F),!,(N=1 -> Type=F;Type=term(props)).

argIsa_call_0(memory,2,term).
argIsa_call_0(subclass,_,type).
argIsa_call_0(term_anglify,_,term).
argIsa_call_0(type_max_charge,1,type).
argIsa_call_0(type_max_charge,2,int).
argIsa_call_0(type_max_damage,1,type).
argIsa_call_0(type_max_damage,2,int).
argIsa_call_0(Arity,N,T):-arity_pred(Arity),!,arg(N,vv(term,int,int),T).
argIsa_call_0(ask_module,_,term).
argIsa_call_0(Func,N,Type):- get_functor(Func,F,_),F \= Func,argIsa_call_0(F,N,Type).
argIsa_call_0(ask_module,_,term).
argIsa_call_0(HILOG,_,term):-hilog_functor(HILOG).



argIsa_asserted(F,N,Type):- dbase_t(argIsa,F,N,Type),!.
argIsa_asserted(F,N,Type):- is_ArgsIsa(F,_,Templ),arg(N,Templ,Type),nonvar(Type).
argIsa_asserted(F,N,Type):- mpred_prop(F,argIsa(N,Type)),!.
argIsa_asserted(F/_,N,Type):- nonvar(F), argIsa_asserted(F,N,Type).

is_ArgsIsa(F,A,Templ):- mpred_prop(F,argsIsa(Templ)).
is_ArgsIsa(F,A,Templ):- moo:ft_info(Templ,formatted),functor(Templ,F,A).
is_ArgsIsa(F,_,Templ):- dbase:dbase_t(_, Templ),compound(Templ),functor(F,_,Templ).
is_ArgsIsa(F,_,Templ):- moo:argsIsaProps(Prop),  dbase:dbase_t(Prop, Templ),compound(Templ),functor(F,_,Templ).


argIsa_call_1(Prop,N1,Type):- is_2nd_order_holds(Prop),dmsg(todo(define(argIsa_call(Prop,N1,'Second_Order_TYPE')))),dumpST,dtrace,
   Type=argIsaFn(Prop,N1).
argIsa_call_1(Prop,N1,Type):- dmsg(todo(define(argIsa_call(Prop,N1,'_TYPE')))),
   Type=argIsaFn(Prop,N1).

db_quf(Op,M:C,Pretest,Template):-var(C),!,throw(var(db_quf(Op,M:C,Pretest,Template))).
db_quf(Op,_:C,Pretest,Template):-nonvar(C),!,db_quf(Op,C,Pretest,Template).
db_quf(Op,','(C,D),','(C2,D2),','(C3,D3)):-!,db_quf(Op,C,C2,C3),db_quf(Op,D,D2,D3).
db_quf(Op,C,Pretest,Template):- C=..[Holds,OBJ|ARGS],is_holds_true(Holds),atom(OBJ),!,C1=..[OBJ|ARGS],db_quf(Op,C1,Pretest,Template).
db_quf(_Op,C,true,C):- C=..[Holds,OBJ|_],is_holds_true(Holds),var(OBJ),!.
db_quf(Op,C,Pretest,Template):- C=..[Prop,OBJ|ARGS],
      functor(C,Prop,A),
      translate_args(Op,Prop,A,OBJ,2,ARGS,NEWARGS,true,Pretest),
      Template =.. [Prop,OBJ|NEWARGS].

translate_args(_O,_Prop,_A,_OBJ,_N,[],[],GIN,GIN).
translate_args(Op,Prop,A,OBJ,N1,[ARG|S],[NEW|ARGS],GIN,GOALS):-
   argIsa_call(Op,Prop/A,N1,Type),
   translateOneArg(Op,Prop,OBJ,Type,ARG,NEW,GIN,GMID),
   N2 is N1 +1,
   translate_args(Op,Prop,A,OBJ,N2,S,ARGS,GMID,GOALS).

infix_op(OP,_):-comparitiveOp(OP).
infix_op(OP,_):-additiveOp(OP).

comparitiveOp((\=)).
comparitiveOp((\==)).
comparitiveOp((=)).
comparitiveOp((=:=)).
comparitiveOp((==)).
comparitiveOp((<)).
comparitiveOp((>)).
comparitiveOp((=<)).
comparitiveOp((>=)).

additiveOp((is)).
additiveOp((*)).
additiveOp(+).
additiveOp(-).
additiveOp((/)).

% var
translateOneArg(_Op,_Prop,_Obj,_Type,VAR,VAR,G,G):-var(VAR),!.

% not an expression
translateOneArg(_O,_Prop,_Obj,_Type,ATOMIC,ATOMIC,G,G):-atomic(ATOMIC),!.
% translateOneArg(_O,_Prop,_Obj,Type,ATOMIC,ATOMICUSE,G,(G,same_arg(type(Type),ATOMIC,ATOMICUSE))):-atomic(ATOMIC),!.

% translateOneArg(_O,_Prop,_Obj,Type,VAR,VAR,G,G):-ignore(isa(VAR,Type)),!.

% props(Obj,size < 2).
translateOneArg(_O,Prop,Obj,Type,ARG,OLD,G,(GETTER,COMPARE,G)):-
       functor(ARG,F,2), comparitiveOp(F),!,
       ARG=..[F,Prop,VAL],
       GETTER=..[Prop,Obj,OLD],
       COMPARE= compare_op(Type,F,OLD,VAL),!.

% props(Obj,oneOf(Sz,[size+1,2])).
translateOneArg(Op,Prop,O,Type,oneOf(VAL,LIST),VAL,G,(GO,G)):-
   translateListOps(Op,Prop,O,Type,VAL,LIST,G,GO).

% db_op(Op, Obj,size + 2).
translateOneArg(_O,Prop,Obj,_Type,ARG,NEW,G,(GETTER,STORE,G)):-
       functor(ARG,F,2), additiveOp(F),!,
       ARG=..[F,Prop,VAL],
       GETTER=..[Prop,Obj,OLD],
       STORE= update_value(OLD,VAL,NEW),!.

translateOneArg(_O,_Prop,_Obj,_Type,NART,NART,G,G):-!.
translateOneArg(_O,_Prop,_Obj,Type,ATOMIC,ATOMICUSE,G,(G,ignore(same_arg(type(Type),ATOMIC,ATOMICUSE)))).

translateListOps(_O,_Prop,_Obj,_Type,_VAL,[],G,G).
translateListOps(Op,Prop,Obj,Type,VAL,[L|LIST],G,GO2):-
   translateOneArg(Op,Prop,Obj,Type,L,VAL,G,GO),
   translateListOps(Op,Prop,Obj,Type,VAL,LIST,GO,GO2).

compare_op(Type,F,OLD,VAL):-nop(Type),call((dtrace,call(F,OLD,VAL))),!.

% start of database
% These will all be deleted at start of run

inverse_args([AR,GS],[GS,AR]):-!.
inverse_args([AR,G,S],[S,G,AR]):-!.
inverse_args([A,R,G,S],[S,R,G,A]):-!.
inverse_args([P,A,R,G,S],[S,A,R,G,P]):-!.

term_is_ft(Term,Type):-
   moo:ft_info(Type,How),
   correctFormatType(query(_HLDS,_OldV),Term,How,NewTerm),!,
   sameArgTypes(NewTerm,Term).

sameArgTypes(A,C):-same(A,C);(pl_arg_type(C,CT),pl_arg_type(A,AT),!,typesOverlap(AT,CT)).
typesOverlap(AT,AT).

pl_arg_type_or_functor(Arg,Type):- pl_arg_type(Arg,T) , (T==compound -> functor(Arg,Type,_); ( (T==list,T=[_|_])-> T=[Type|_] ;Type=T)) .

pl_arg_type(Arg,Type):- 
      var(Arg) -> Type =var;
      integer(Arg) -> Type =integer;
      number(Arg) -> Type =float;
      string(Arg) -> Type =string;
      atom(Arg) -> Type =atom;
      is_list(Arg) -> Type =list;
      compound(Arg) -> Type =compound;
         Arg = Type.


% =======================================================
% correctArgsIsa/3
% =======================================================


moo:decl_coerce(A,Type,AA):- correctAnyType(tell(_),A,Type,AA).

same_vars(T1,T2):-term_variables(T1,V1),term_variables(T2,V2),!,V1==V2.

correctArgsIsa(_,NC,NC):-not(compound(NC)),!.
correctArgsIsa(Op,M:A,MAA):- nonvar(M),!,correctArgsIsa(Op,A,AA),M:AA=MAA.
correctArgsIsa(Op,A,AA):- correctArgsIsa0(Op,A,AA),nonvar(AA),!.
correctArgsIsa(Op,A,AA):- grtrace,correctArgsIsa0(Op,A,AA).
correctArgsIsa(Op,A,Type,A):- dtrace,dmsg(warn(not(correctArgsIsa(Op,A,Type)))).

correctArgsIsa0(Op,A,AA):-A =..[KP,Prop|Args],atom(Prop),is_holds_true(KP),!,
   discoverAndCorrectArgsIsa(Op,Prop,1,Args,AArgs),
   AA =..[KP,Prop|AArgs].

correctArgsIsa0(Op,A,not(AA)):-A =..[KP,Prop|Args],atom(Prop),is_holds_false(KP),!,
   discoverAndCorrectArgsIsa(Op,Prop,1,Args,AArgs),
   AA =..[KP,Prop|AArgs].

correctArgsIsa0(Op,A,AA):-A =..[Prop|Args],
   discoverAndCorrectArgsIsa(Op,Prop,1,Args,AArgs),
   AA =..[Prop|AArgs].

discoverAndCorrectArgsIsa(_O,_Prop,_N1,[],[]):-!.
discoverAndCorrectArgsIsa(Op,Prop,N1,[A|Args],Out):-
   must((argIsa_call(Op,Prop,N1,Type),correctAnyType(Op,A,Type,AA))),
   N2 is N1+1,
   discoverAndCorrectArgsIsa(Op,Prop,N2,Args,AArgs),
    Out = [AA|AArgs].

correctAnyType(_Op,A,_Type,AA):- var(A),!,must_det(var(AA)),must_det(A=AA),!.
correctAnyType(Op,A,Type,AA):- var(A),correctType(Op,A,Type,AA),must_det(var(AA)),must_det(A==AA),!.
correctAnyType(Op,A,Type,AA):- var(Type),trace_or_throw(correctAnyType(Op,A,Type,AA)).
% TODO snags on new tpyes correctAnyType(Op,A,Type,AA):- correctType(Op,A,Type,AA),nonvar(AA),!.
correctAnyType(Op,A,Type,AA):- one_must(correctType(Op,A,Type,AA),A=AA).
correctAnyType(Op,A,Type,A):- dtrace,dmsg(warn(not(correctAnyType(Op,A,Type)))).

%  @set movedist 4
:-export((correctFormatType/4)).
correctFormatType(Op,A,Type,AA):- var(A),correctType(Op,A,Type,AA),must_det(var(AA)),must_det(A==AA),!.
correctFormatType(Op,A,Type,AA):- var(Type),trace_or_throw(correctFormatType(Op,A,Type,AA)).
correctFormatType(Op,A,Type,AA):- correctType(Op,A,Type,AA),nonvar(AA),!.
correctFormatType(Op,A,Type,AA):- grtrace,correctType(Op,A,Type,AA).
correctFormatType(Op,A,Type,A):- dtrace,dmsg(warn(not(correctFormatType(Op,A,Type)))).

:-export(checkAnyType/4).

checkAnyType(Op,A,Type,AA):- var(A),correctType(Op,A,Type,AA),must_det(var(AA)),must_det(A==AA),!.
checkAnyType(Op,A,Type,AA):- correctType(Op,A,Type,AA),nonvar(AA),!.


correctType_gripe(Op,A,Fmt,AA):- formattype(Fmt),!,trace_or_throw(correctType(is_ft_correctFormatType(Op,A,Fmt,AA))).
correctType_gripe(Op,A,Type,AA):- fail,atom(Type),must_equals(A,AA),
      dmsg(todo(isa_assert_type(Type))),
      define_type(Type),can_coerce(Op),dtrace,
      add(isa(A,Type)),!.

correctType_gripe(Op,A,C,A):-must(ground(A)),dtrace, dmsg(todo(define(correctType(Op,A,C,'ConvertedArg')))),throw(retry(_)).
correctType_gripe(Op,A,Type,NewArg):-trace_or_throw(failure(correctType(Op,A,Type,NewArg))).

can_coerce(notta).

:- style_check(+singleton).

correctType(Op,A,Type,AA):- var(Type),trace_or_throw(correctType(Op,A,Type,AA)).
correctType(_O,A,Type,AA):- (var(A);var(Type)),!, must(must_equals(A,AA)).
correctType(_O,A,argIsaFn(_,_),AA):-must_equals(A,AA). % !. %any_to_value(O,V).  %missing
correctType(_O,A,dir,AA):- any_to_dir(A,AA).
correctType(Op,+A,Type,+AA):-correctType(Op,A,Type,AA).
correctType(Op,-A,Type,-AA):-correctType(Op,A,Type,AA).
correctType(Op,A,integer,AA):-!,correctType(Op,A,int,AA).
correctType(_O,A,int,AA):- any_to_number(A,AA).
correctType(_O,A,number,AA):- must(any_to_number(A,AA)).
correctType(_O,A,prolog,AA):- must_equals(A,AA).
correctType(_O,A,string,AA):- must(text_to_string(A,AA)).
correctType(_O,A,term(_),AA):- must_equals(A,AA).
correctType(_O,A,Type,AA):- compound(A),atom(Type),functor_safe(A,Type,_), must_equals(A,AA).
correctType(_O,A,term,AA):- must_equals(A,AA).
correctType(_O,A,text,AA):- must_equals(A,AA).
correctType(_O,A,pred,AA):- any_to_atom(A,AA).
correctType(_O,A,atom,AA):- any_to_atom(A,AA).
correctType(_O,A,type,AA):- atom(A),define_type(A),must_equals(A,AA).
correctType(_O,A,verb,AA):- must_equals(A,AA).

correctType(query(HLDS,Must),A,xyz(Region, int, int, int),xyz(AA, _, _, _)):-atom(A),correctAnyType(query(HLDS,Must),A,Region,AA).
correctType(_Op,A,list(_),AA):- A == [],!,A=AA.
correctType(Op,[A|AA],list(T),[L|LIST]):-!, correctType(Op,A,T,L), correctType(Op,AA,list(T),LIST).
correctType(Op,A,list(T),[OT]):-!,correctAnyType(Op,A,T,OT).
correctType(_O,A,same(T),AA):-must_equals(T,AA),must_equals(A,AA).
correctType(Op,A,oneOf(List),AA):-!,member(Type,List),correctType(Op,A,Type,AA).

correctType(_O,[],formatted([]),[]):-!.
correctType(Op,[H|T],formatted([H2|T2]),[H3|T3]):-
   correctType(Op,H,H2,H3),
   correctType(Op,T,formatted(T2),T3).

correctType(Op,Args,formatted(Types),NewArgs):- compound(Args),compound(Types),
   functor(Args,F,N),functor(Types,F,N),functor(NewArgs,F,N),
   Args=..[F|ArgsL],
   Types=..[F|TypesL],
   NewArgs=..[F|NewArgsL],!,   
   correctType(Op,ArgsL,TypesL,NewArgsL).

correctType(Op,[A|AA],list(T),LIST):-!,findall(OT,((member(O,[A|AA]),correctAnyType(Op,O,T,OT))),LIST).
correctType(Op,A,list(T),[OT]):-!,correctAnyType(Op,A,T,OT).
correctType(_O,[],[],[]):-!.
correctType(Op,[H|T],[H2|T2],[H3|T3]):-!, correctAnyType(Op,H,H2,H3),correctType(Op,T,T2,T3).

correctType(Op,Args,Types,NewArgs):-compound(Args), compound(Types),
   functor(Args,F,N),functor(Types,F,N),functor(NewArgs,F,N),
   Args=..[F|ArgsL],
   Types=..[F|TypesL],
   NewArgs=..[F|NewArgsL],
   correctAnyType(Op,ArgsL,TypesL,NewArgsL).



correctType(Op,A,Fmt,AA):- moo:ft_info(Fmt,formatted),!,correctFormatType(Op,A,formatted(Fmt),AA).
correctType(_O,A,Fmt,A):- moo:ft_info(Fmt,Code),!,subst(Code,self,A,Call),debugOnError(req(Call)).   
correctType(Op,A,Super,AA):- formattype(Super),req(subft(Sub,Super)),Sub\=Super,correctType(Op,A,Sub,AA).

correctType(Op,Arg,Props,NewArg):- compound(Props),
   Props=..[F|TypesL],
   C=..[F,Arg|TypesL],
   correctArgsIsa(Op,C,CC),
   CC=..[F,NewArg|_].

correctType(_O,A,Type,AA):-not(formattype(Type)),is_type(Type),get_isa_backchaing(A,Type),!,must_equals(A,AA).



% :- style_check(+singleton).

must_equals(A,AA):-must_det(A=AA).

  
:- style_check(+singleton).


any_to_value(V,Term):-atom(V),!,atom_to_value(V,Term).
any_to_value(A,V):-any_to_number(A,V).
any_to_value(A,A).


any_to_number(N,N):- number(N),!.
any_to_number(dice(A,B,C),N):- ground(A),roll_dice(A,B,C,N),!.
any_to_number(A,N):-atom(A),atom_to_value(A,V),A\=V,any_to_number(V,N).
any_to_number(A,N):- catch(number_string(N,A),_,fail).


roll_dice(Rolls,_,Bonus,Result):- Rolls < 0, !, Result is Bonus.
roll_dice(Rolls,Sided,Bonus,Result):- LessRolls is Rolls-1, roll_dice(LessRolls,Sided, Bonus + random(Sided) +1, Result).


atom_to_value(V,Term):-not(atom(V)),!,any_to_value(V,Term).
% 56
atom_to_value(V,Term):- catch((read_term_from_atom(V,Term,[variable_names([])])),_,fail),!.
% 18d18+4000
atom_to_value(V,dice(T1,T2,+T3)):- atomic_list_concat_safe([D1,'d',D2,'+',D3],V), atom_to_value(D1,T1),atom_to_value(D2,T2),atom_to_value(D3,T3),!.
atom_to_value(V,dice(T1,T2,-T3)):- atomic_list_concat_safe([D1,'d',D2,'-',D3],V), atom_to_value(D1,T1),atom_to_value(D2,T2),atom_to_value(D3,T3),!.

:-export(any_to_dir/2).
any_to_dir(D,D):-var(D),!.
any_to_dir(D,D):-world:dir_offset(D,_,_,_,_),!.
any_to_dir(A,D):-p2c_dir2(D,A),!.
any_to_dir(S,D):-string(S),string_to_atom(S,A),any_to_dir(A,D),!.
any_to_dir(D,O):-atom(D),sub_atom(D, 0, 1, _, S),toLowercase(S,L),p2c_dir2(L,O),!.


detWithSpace(WithSpace,String):-ddeterminer0(String),atom_concat(String,' ',WithSpace).
detWithSpace(WithSpace,String):-ddeterminer1(String),atom_concat(String,' ',WithSpace).
determinerRemoved(S0,Det,S):- nonvar(S0),detWithSpace(WithSpace,String),string_concat(WithSpace,S,S0),string_lower(String,Det).

assert_description(description(A,S0)):-assert_description(A,S0).

assert_description(A,S0):-nonvar(S0),string_concat('#$PunchingSomething ',S,S0),!,assert_description(A,S).

assert_description(A,S0):-determinerRemoved(S0,String,S),!,assert_description(A,S),add(determinerString(A,String)).
assert_description(A,S0):-
   string_to_atom(S0,S),
   atomic_list_concat(Words,' ',S),
   atomic_list_concat(Sents,'.',S),!,
   length(Words,Ws),
   must(assert_description(A,S,S0,Ws,Sents,Words)).
% "NOBACKSTAB","ACT_STAY_ZONE","MEMORY"
assert_description(A,S,_S0,Ws,_Sents,_Words):- Ws<3,  
   atomic_list_concat([K,V],': ',S),!,add_description_kv(A,K,V).
assert_description(A,S,_S0,Ws,_Sents,_Words):- Ws<3,
   atomic_list_concat([K,V],'=',S),!,add_description_kv(A,K,V).
%#$PunchingSomething mudBareHandDamage: 10d10+75

assert_description(A,_S,_S0,1,_,[Word]):-add_description_word(A,Word).

assert_description(A,S,S0,Ws,Sents,['#$PunchingSomething',B|C]):-assert_description(A,S,S0,Ws,Sents,[B|C]).
assert_description(A,S,S0,Ws,Sents,[Det,B|C]):-ddeterminer(Det,L),assert_description(A,S,S0,Ws,Sents,[B|C]),add(determinerString(A,L)).
assert_description(A,S,S0,Ws,_Sents,_Words):-Ws>3,is_here_String(S),!,show_call(add(descriptionHere(A,S0))).
assert_description(A,_S,S0,_Ws,_Sents,_Words):-show_call(add(comment(A,S0))).

is_here_String(S):- atomic_list_concat_safe([_,is,_,here,_],S).
is_here_String(S):- atomic_list_concat_safe([_,here],S).
is_here_String(S):- atomic_list_concat_safe([_,is,here,_],S).


ddeterminer1('A').
ddeterminer1('An').
ddeterminer1('The').
ddeterminer0(a).
ddeterminer0(an).
ddeterminer0(the).
ddeterminer(L,L):-ddeterminer0(L).
ddeterminer(U,L):-string_lower(U,L),U\=L,!,ddeterminer0(L).

add_description_word(A,Word):- string_upper(Word,Word),string_lower(Word,Flag),string_to_atom(Flag,Atom),show_call(add(isa(A,Atom))).
add_description_word(A,Word):- string_lower(Word,Word),show_call(add(keyword(A,Word))).
add_description_word(A,Word):- string_lower(Word,Lower),show_call(add(keyword(A,Lower))).


add_description_kv(A,K,V):- atom_concat('#$PunchingSomething ',Key,K),!,add_description_kv(A,Key,V).
add_description_kv(A,K,V):- atom_concat('+',Key,K),!,add_description_kv(A,Key,V).
add_description_kv(A,K,V):-atom_to_value(V,Term),C=..[K,A,Term],
   learnArgIsaInst(K,2,V),
   decl_mpred(K,2),show_call(add(C)).

learnArgIsa(P,N,_):-argIsa_asserted(P,N,_),!.
learnArgIsa(P,N,T):-dmsg((skipping(learnArgIsa(P,N,T)))),!.
learnArgIsa(P,N,T):-grtrace, add(argIsa(P,N,T)).

learnArgIsaInst(K,Num,Arg):-integer(Arg),!,learnArgIsa(K,Num,int).
learnArgIsaInst(K,Num,Arg):-number(Arg),!,learnArgIsa(K,Num,number).
learnArgIsaInst(_,_,_).



:- include(logicmoo('vworld/moo_footer.pl')).

