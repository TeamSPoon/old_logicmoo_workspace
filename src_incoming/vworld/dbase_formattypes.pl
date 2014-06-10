/** <module> 
% This is mainly used by the moo_loader but also needed everywhere
%
% Project LogicMoo: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
% =======================================================



argIsa_call(Prop,N1,_):-once((must(nonvar(Prop)),must(number(N1)))),fail.
argIsa_call(Prop,N1,Type):-argIsa_call_0(Prop,N1,Type),!.
argIsa_call(_:Prop,N1,Type):-!,argIsa_call(Prop,N1,Type).
argIsa_call(Prop/_,N1,Type):-!,argIsa_call(Prop,N1,Type).

argIsa_call(Prop,N1,Type):-argIsa_call_1(Prop,N1,Type),!.

argIsa_call_0(Prop,N1,Type):-get_mpred_prop(Prop,argIsa(N1,Type)),!.
argIsa_call_0(isa,1,argIsaFn(isa,1)):-!.

argIsa_call_0(isa,2,type):-!.
argIsa_call_0(subclass,_,type).
argIsa_call_0(type_max_damage,1,object).
argIsa_call_0(type_max_damage,2,int).
argIsa_call_0(type_max_charge,1,object).
argIsa_call_0(type_max_charge,2,int).


argIsa_call_0(term_anglify,_,term).

argIsa_call_0(memory,2,term):-!.
argIsa_call_0(Pred,N,Type):- holds_t(argIsa,Pred,N,Type),!.
argIsa_call_0(Pred,N,Type):- holds_t(isa,Templ,mpred),functor(Templ,Pred,A),A>0,!,arg(N,Templ,Type).
argIsa_call_0(Pred,N,Type):- get_mpred_prop(Pred,argsIsa(Templ)),!,arg(N,Templ,Type).
argIsa_call_0(Func,N,Type):- get_functor(Func,F,_),F \= Func,argIsa_call_0(F,N,Type).
argIsa_call_0(Pred,N,Type):- moo:ft_info(Templ,formatted),functor(Templ,Pred,A),A>0,!,arg(N,Templ,Type).

argIsa_call_1(Prop,N1,Type):- is_2nd_order_holds(Prop),trace,dmsg(todo(define(argIsa_call(Prop,N1,'Second_Order_TYPE')))),
   Type=argIsaFn(Prop,N1).
argIsa_call_1(Prop,N1,Type):- trace, dmsg(todo(define(argIsa_call(Prop,N1,'_TYPE')))),
   Type=argIsaFn(Prop,N1).

db_quf(C,Pretest,Template):-var(C),!,throw(var(db_quf(C,Pretest,Template))).
db_quf(_:C,Pretest,Template):-nonvar(C),!,db_quf(C,Pretest,Template).
db_quf(','(C,D),','(C2,D2),','(C3,D3)):-!,db_quf(C,C2,C3),db_quf(D,D2,D3).
db_quf(C,Pretest,Template):- C=..[Holds,OBJ|ARGS],is_holds_true(Holds),nonvar(OBJ),!,C1=..[OBJ|ARGS],db_quf(C1,Pretest,Template).

db_quf(C,Pretest,Template):- C=..[Prop,OBJ|ARGS],
      functor(C,Prop,A),
      translate_args(Prop,A,OBJ,2,ARGS,NEWARGS,true,Pretest),
      Template =.. [Prop,OBJ|NEWARGS].

translate_args(_Prop,_A,_OBJ,_N,[],[],GIN,GIN).
translate_args(Prop,A,OBJ,N1,[ARG|S],[NEW|ARGS],GIN,GOALS):-
   argIsa_call(Prop/A,N1,Type),
   translateOneArg(Prop,OBJ,Type,ARG,NEW,GIN,GMID),
   N2 is N1 +1,
   translate_args(Prop,A,OBJ,N2,S,ARGS,GMID,GOALS).

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
translateOneArg(_Prop,_O,_Type,VAR,VAR,G,G):-var(VAR),!.

% not an expression
translateOneArg(_Prop,_O,_Type,VAR,VAR,G,G):-atomic(VAR),!.

% translateOneArg(_Prop,_O,Type,VAR,VAR,G,G):-ignore(isa(VAR,Type)),!.

% props(Obj,size < 2).
translateOneArg(Prop,O,Type,ARG,OLD,G,(GETTER,COMPARE,G)):-
       functor(ARG,F,2), comparitiveOp(F),!,
       ARG=..[F,Prop,VAL],
       GETTER=..[Prop,O,OLD],
       COMPARE= compare_op(Type,F,OLD,VAL),!.

% props(Obj,oneOf(Sz,[size+1,2])).
translateOneArg(Prop,O,Type,oneOf(VAL,LIST),VAL,G,(GO,G)):-
   translateListOps(Prop,O,Type,VAL,LIST,G,GO).

% pdb_op(Op, Obj,size + 2).
translateOneArg(Prop,O,_Type,ARG,NEW,G,(GETTER,STORE,G)):-
       functor(ARG,F,2), additiveOp(F),!,
       ARG=..[F,Prop,VAL],
       GETTER=..[Prop,O,OLD],
       STORE= update_value(OLD,VAL,NEW),!.

translateListOps(_Prop,_O,_Type,_VAL,[],G,G).
translateListOps(Prop,O,Type,VAL,[L|LIST],G,GO2):-
   translateOneArg(Prop,O,Type,L,VAL,G,GO),
   translateListOps(Prop,O,Type,VAL,LIST,GO,GO2).

compare_op(Type,F,OLD,VAL):-nop(Type),call((trace,call(F,OLD,VAL))),!.

% start of database
% These will all be deleted at start of run

inverse_args([AR,GS],[GS,AR]):-!.
inverse_args([AR,G,S],[S,G,AR]):-!.
inverse_args([A,R,G,S],[S,R,G,A]):-!.
inverse_args([P,A,R,G,S],[S,A,R,G,P]):-!.

term_is_ft(Term,Type):-
   moo:ft_info(Type,How),
   correctFormatType(tell(_OldV),Term,How,NewTerm),
   ignore(NewTerm=Term).


% =======================================================
% correctArgsIsa/3
% =======================================================

same_vars(T1,T2):-term_variables(T1,V1),term_variables(T2,V2),!,V1==V2.

correctArgsIsa(_,NC,NC):-not(compound(NC)),!.
correctArgsIsa(Op,M:A,MAA):-nonvar(M),!,correctArgsIsa(Op,A,AA),M:AA=MAA.
correctArgsIsa(Op,A,AA):- correctArgsIsa0(Op,A,AA),nonvar(AA),!.
correctArgsIsa(Op,A,AA):- grtrace,correctArgsIsa0(Op,A,AA).
correctArgsIsa(Op,A,Type,A):- trace,dmsg(warn(not(correctArgsIsa(Op,A,Type)))).

correctArgsIsa0(Op,A,AA):-A =..[KP,Prop|Args],atom(Prop),is_holds_true(KP),!,
   discoverAndCorrectArgsIsa(Op,Prop,1,Args,AArgs),
   AA =..[KP,Prop|AArgs].

correctArgsIsa0(Op,A,not(AA)):-A =..[KP,Prop|Args],atom(Prop),is_holds_false(KP),!,
   discoverAndCorrectArgsIsa(Op,Prop,1,Args,AArgs),
   AA =..[KP,Prop|AArgs].

correctArgsIsa0(Op,A,AA):-A =..[Prop|Args],
   discoverAndCorrectArgsIsa(Op,Prop,1,Args,AArgs),
   AA =..[Prop|AArgs].

discoverAndCorrectArgsIsa(_Op,_Prop,_N1,[],[]):-!.
discoverAndCorrectArgsIsa(Op,Prop,N1,[A|Args],[AA|AArgs]):-
   must(argIsa_call(Prop,N1,Type)),
   must(correctAnyType(Op,A,Type,AA)),
   N2 is N1+1,
   discoverAndCorrectArgsIsa(Op,Prop,N2,Args,AArgs).

correctAnyType(Op,A,Type,AA):- var(A),correctType(Op,A,Type,AA),!.
correctAnyType(Op,A,Type,AA):- correctType(Op,A,Type,AA),nonvar(AA),!.
correctAnyType(Op,A,Type,AA):- grtrace,correctType(Op,A,Type,AA).
correctAnyType(Op,A,Type,A):- trace,dmsg(warn(not(correctAnyType(Op,A,Type)))).

%  @set movedist 4


:- style_check(+singleton).

correctType(_Op,A,Type,A):- (var(A);var(Type)),!. % ,trace,throw(failure(correctType(Op,A,Type))).
correctType(_Op,O,argIsaFn(_,_),O):-!. %any_to_value(O,V).  %missing
correctType(_Op,A,type,A):-atom(A),define_type(A).
correctType(_Op,A,term,A):-!. % must(ground(A)).
correctType(Op,[A|AA],list(T),LIST):-!,findall(OT,((member(O,[A|AA]),correctAnyType(Op,O,T,OT))),LIST).
correctType(Op,A,list(T),[OT]):-!,correctAnyType(Op,A,T,OT).
correctType(_Op,[],[],[]):-!.
correctType(ask(Must),A,xyz(Region, int, int, int),xyz(AA, _, _, _)):-atom(A),correctType(ask(Must),A,Region,AA).
correctType(Op,A,Type,AA):-correctFormatType(Op,A,Type,AAA),AA=AAA,!.
correctType(_Op,A,Type,A):-atom(Type),
      dmsg(todo(isa_assert_type(Type))),
      define_type(Type),add(isa(A,Type)),!.
correctType(Op,[H|T],[H2|T2],[H3|T3]):-!,
   correctAnyType(Op,H,H2,H3),correctType(Op,T,T2,T3).
correctType(Op,Args,Types,NewArgs):-compound(Args), compound(Types),
   functor(Args,F,N),functor(Types,F,N),functor(NewArgs,F,N),
   Args=..[F|ArgsL],
   Types=..[F|TypesL],
   NewArgs=..[F|NewArgsL],
   correctType(Op,ArgsL,TypesL,NewArgsL).
correctType(Op,Arg,Props,NewArg):- compound(Props),
   Props=..[F|TypesL],
   C=..[F,Arg|TypesL],
   correctArgsIsa(Op,C,CC),
   CC=..[F,NewArg|_].
correctType(Op,A,C,A):-must(ground(A)),trace, dmsg(todo(define(correctType(Op,A,C,'ConvertedArg')))),throw(retry(_)).
correctType(Op,A,Type,_NewArg):-throw(failure(correctType(Op,A,Type))).

:-decl_mpred(subft/2).

% :- style_check(+singleton).

correctFormatType(Op,A,Type,A):- var(Type),!,trace,throw(failure(correctFormatType(Op,A,Type))).
correctFormatType(_O,A,dir,AA):- must(string_to_atom(A,AA)).
correctFormatType(Op,A,Fmt,AA):- holds_t(ft_info,Fmt,formatted),!,correctFormatType(Op,A,formatted(Fmt),AA).
correctFormatType(_O,A,Fmt,A):- holds_t(ft_info,Fmt,Code),!,subst(Code,self,A,Call),debugOnError(req(Call)).   
correctFormatType(_O,A,int,AA):- any_to_number(A,AA).
correctFormatType(Op,A,integer,AA):-!,correctFormatType(Op,A,int,AA).
correctFormatType(_O,A,number,AA):- must(any_to_number(A,AA)).
correctFormatType(_O,A,string,AA):- must(text_to_string(A,AA)).
correctFormatType(_O,A,text,AA):- A=AA.
correctFormatType(_O,A,verb,AA):- A=AA.
correctFormatType(_O,A,term(_),AA):- A=AA.
correctFormatType(_O,A,dir,AA):- any_to_dir(A,AA).
correctFormatType(Op,[A|AA],list(T),LIST):-!,findall(OT,((member(O,[A|AA]),correctFormatType(Op,O,T,OT))),LIST).
correctFormatType(Op,A,list(T),[OT]):-!,correctFormatType(Op,A,T,OT).
correctFormatType(_O,A,same(A),A):-!.
correctFormatType(_O,[],formatted([]),[]):-!.
correctFormatType(Op,[H|T],formatted([H2|T2]),[H3|T3]):-
   correctFormatType(Op,H,H2,H3),
   correctFormatType(Op,T,formatted(T2),T3).
correctFormatType(Op,Args,formatted(Types),NewArgs):- compound(Args),compound(Types),
   functor(Args,F,N),functor(Types,F,N),functor(NewArgs,F,N),
   Args=..[F|ArgsL],
   Types=..[F|TypesL],
   NewArgs=..[F|NewArgsL],!,   
   correctFormatType(Op,ArgsL,TypesL,NewArgsL).
correctFormatType(Op,A,Super,AA):- holds_t(subft,Sub,Super),Sub\=Super,correctFormatType(Op,A,Sub,AA).
  
:- style_check(+singleton).


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
any_to_dir(D,D):-world:dir_offset(D,_,_,_,_),!.
any_to_dir(A,D):-p2c_dir2(D,A),!.
any_to_dir(S,D):-string(S),string_to_atom(S,A),any_to_dir(A,D),!.
any_to_dir(D,O):-atom(D),sub_atom(D, 0, 1, _, S),toLowercase(S,L),p2c_dir2(L,O),!.


detWithSpace(WithSpace,String):-ddeterminer0(String),atom_concat(String,' ',WithSpace).
detWithSpace(WithSpace,String):-ddeterminer1(String),atom_concat(String,' ',WithSpace).
determinerRemoved(S0,Det,S):- nonvar(S0),detWithSpace(WithSpace,String),string_concat(WithSpace,S,S0),string_lower(String,Det).

assert_description(moo:description(A,S0)):-assert_description(A,S0).

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

learnArgIsa(P,N,T):-add(argIsa(P,N,T)).

learnArgIsaInst(K,Num,Arg):-integer(Arg),!,learnArgIsa(K,Num,int).
learnArgIsaInst(K,Num,Arg):-number(Arg),!,learnArgIsa(K,Num,number).
learnArgIsaInst(_,_,_).


show_call(add(A)):-
   correctArgsIsa(tell(_OldV),A,AA),
   show_call0(add(AA)),!.
show_call(C):-show_call0(C).

show_call0(C):-debugOnError0(C). % dmsg(show_call(C)),C.      


% =======================================================
% correctArgsIsa/3
% =======================================================





