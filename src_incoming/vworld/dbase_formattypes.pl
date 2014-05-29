/** <module> 
% This is mainly used by the moo_loader but also needed everywhere
%
% Project LogicMoo: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
% =======================================================
:- module(dbase_formattypes, [
          term_is_ft/2,
          is_ft/1,
          format_complies/3,
          any_to_value/2,
          any_to_number/2,
          atom_to_value/2,
          any_to_dir/2]).

:- include(logicmoo(vworld/moo_header)).

:- registerCycPred((ft_info/2,subft/2)).

term_is_ft(Term,Type):-
   moo:ft_info(Type,How),
   format_complies(Term,How,NewTerm),
   ignore(NewTerm=Term).



is_ft(S):-   moo:ft_info(S,_).
is_ft(S):-   moo:subft(S,_).

/*
is_ft_except(S,List):-
   moo:ft_info(S,_);
   not((member(S,List), 
      ((moo:subft(S,S2) ,
        is_ft_except(S2,[S|List]) ;
             ((moo:subft(S3,S) , is_ft_except(S3,[S,S2|List]))))))).
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
format_complies(A,Fmt,AA):- ft_info(Fmt,formatted),!,format_complies(A,formatted(Fmt),AA).
format_complies(A,Fmt,A):- ft_info(Fmt,Code),!,subst(Code,self,A,Call),Call.   
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
format_complies(A,Super,AA):- subft(Sub,Super),format_complies(A,Sub,AA).
  


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


:- include(logicmoo(vworld/moo_footer)).


