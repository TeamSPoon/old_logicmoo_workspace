% Dec 13, 2035
% Douglas Miles
%
/** <module>
% This is mainly used by the game loader but also needed everywhere
%
*/
% =======================================================
:- module(formattypes, [
          term_is_ft/2,
          is_decl_ft/1,
          format_complies/3,
          any_to_value/2,
          any_to_number/2,
          atom_to_value/2,
          any_to_dir/2]).

term_is_ft(Term,Type):-
   moo:decl_ft(Type,How),
   format_complies(Term,How,NewTerm),
   ignore(NewTerm=Term).


is_decl_ft(S):- is_decl_ft_except(S,[]).

is_decl_ft_except(S,List):-
   moo:decl_ft(S,_);
   not((member(S,List), 
      ((moo:decl_subft(S,S2) ,
        is_decl_ft_except(S2,[S|List]) ;
             ((moo:decl_subft(S3,S) , is_decl_ft_except(S3,[S,S2|List]))))))).


moo:decl_ft(atom,atom(self)).
moo:decl_ft(apath(region,dir),formatted).
moo:decl_ft(string,string(self)).
moo:decl_ft(number,number(self)).
moo:decl_ft(type,type(self)).
moo:decl_ft(dir,any_to_dir(self,_)).
moo:decl_ft(dice(int,int,int),formatted).
moo:decl_ft(xyz(region,int,int,int),formatted).
moo:decl_ft(list(type),formatted).
moo:decl_ft(term,nonvar(self)).
moo:decl_ft(id,nonvar(self)).
moo:decl_ft(prolog,true).
moo:decl_ft(rest,true).
moo:decl_ft(var,var(self)).
moo:decl_ft(action(prolog),formatted).

moo:decl_subft(var,prolog).
moo:decl_subft(term,prolog).
moo:decl_subft(atom,term).
moo:decl_subft(string,term).
moo:decl_subft(number,term).
moo:decl_subft(id,term).

moo:decl_subft(int,integer).
moo:decl_subft(integer,number).
moo:decl_subft(dice,int).

format_complies(A,Type,A):- var(Type),!,trace,throw(failure(format_complies(A,Type))).
format_complies(A,string,AA):- ignoreOnError(text_to_string(A,AA)).
format_complies(A,int,AA):- any_to_number(A,AA).
format_complies(A,Fmt,AA):- moo:decl_ft(Fmt,formatted),!,format_complies(A,formatted(Fmt),AA).
format_complies(A,Fmt,A):- moo:decl_ft(Fmt,Code),!,subst(Code,self,A,Call),Call.   
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
format_complies(A,Super,AA):- moo:decl_subft(Sub,Super),format_complies(A,Sub,AA).
  


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




