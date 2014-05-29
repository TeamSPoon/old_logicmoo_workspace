% ===================================================================
% File 'logicmoo_util_strings.pl'
% Purpose: Common Logicmoo library Functions for Strings
% Maintainers: Douglas Miles/Annie Ogborn/Kino Coursey
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_util_strings.pl' 1.0.0
% Revision:  $Revision: 1.7 $
% Revised At:   $Date: 2002/07/11 21:57:28 $
% ===================================================================
:-module(logicmoo_util_strings,[
         quoteAtomString/2,
         unquoteAtom/2,
         /*
         atom_to_number/2,
         */
         toUppercase/2,
         toLowercase/2,
         toPropercase/2,
         toCamelcase/2,

         is_string/1,
         is_codelist/1,
         is_charlist/1,

         stringToCodelist/2,
         trim/2,
         clean_codes/2,
         all_upper_atom/1,
         convert_to_string/2,
         atom_contains/2,
         atomsSameCI/2,
         isWhitespace/1,
         atomSplit/2,
         atomic_list_concat_safe/2,
         any_to_string/2,
         atom_contains/2
         %string_chars/2
         % text_to_string/2,
         %string_upper/2,
         %string_lower/2

   ]).

/*
string_chars(S,C):-atom_chars(S,C).
text_to_string(T,S):-string_to_atom(S,T).
string_upper(M,U):-toUppercase(M,U).
string_lower(M,U):-toLowercase(M,U).
*/

:-use_module(logicmoo('logicmoo_util/logicmoo_util_library.pl')).
:-use_module(logicmoo('logicmoo_util/logicmoo_util_bugger.pl')).

ib_multi_transparent(MT):-multifile(MT),dynamic(MT),module_transparent(MT).

:- ib_multi_transparent(camelSplitters/1).

:- ib_multi_transparent(to_string_hook/3).

camelSplitters(V):-member(V,[' ','-','_',':','mt','doom','Mt','Doom']).

%================================================================
% Atom / String functions
%================================================================
atomsSameCI(Name1,Name1):-!.
atomsSameCI(Name1,Name2):-atom(Name1),atom(Name2),downcase_atom(Name1,D1),downcase_atom(Name2,D2),!,D1=D2.

clean_codes(X,Y):-trim(X,Y),!.  % actually cyc:trim/2
clean_codes(X,X).

%clean_out_atom(X,Y):-atomSplit(X,C),delete(C,'',O),concat_atom_safe(C,' ',Y).
clean_out_atom(X,Y):-atom_codes(X,C),clean_codes(C,D),!,atom_codes(X,D),!,Y=X.

%%atomSplit(A,B):-token_stream_of(A,AA),findall(B0,arg(1,AA,B),B).

all_upper_atom(X):-toUppercase(X,N),!,N=X.

atom_contains(F0,C0):- notrace((any_to_atom(F0,F),any_to_atom(C0,C),sub_atom(F,_,_,_,C))).

any_to_atom(A,A):-atom(A),!.
any_to_atom(T,A):-sformat(S,'~w',[T]),atom_string(A,S).
          
atomic_list_concat_safe(O,''):- O=[],!.
atomic_list_concat_safe([Atom|Bonus],V):-atomic(Atom),atom_concat(Atom,NV,V),!,atomic_list_concat_safe(Bonus,NV).
atomic_list_concat_safe([D1,Atom|Bonus],V):-var(D1),atomic(Atom),sub_atom(V, NBefore, _Len, _NumAfter, Atom),
      sub_atom(V, 0, NBefore, _, D1), !, atomic_list_concat_safe([D1,Atom|Bonus],V).
atomic_list_concat_safe([V],V):-!.




% convert any term to 'atom' string
convert_to_string(I,ISO):-
                term_to_string(I,IS),!,
		string_to_list(IS,LIST),!,
		list_replace(LIST,92,[92,92],LISTM),
		list_replace(LISTM,34,[92,34],LISTO),!,
		string_to_atom_safe(ISO,LISTO),!.

list_replace(List,Char,Replace,NewList):-
	append(Left,[Char|Right],List),
	append(Left,Replace,NewLeft),
	list_replace(Right,Char,Replace,NewRight),
	append(NewLeft,NewRight,NewList),!.
list_replace(List,_Char,_Replace,List):-!.

term_to_string(I,IS):- catch(string_to_atom(IS,I),_,fail),!.
term_to_string(I,IS):- term_to_atom(I,A),string_to_atom(IS,A),!.

% ===========================================================
% CASE CHANGE
% ===========================================================

noCaseChange(NC):-noCaseChange(NC,_),!.
noCaseChange([],[]):-!.
noCaseChange(VAR,VAR):-var(VAR),!.
noCaseChange(MiXed,MiXed):-atom(MiXed),atom_concat('#$',_,MiXed),!.
noCaseChange(c(VAR),c(VAR)):-!.

toUppercase(MiXed,MiXed):-noCaseChange(MiXed),!.
toUppercase(V,V2):-string(V),!,atom_codes(V,VC),toUppercase(VC,CVC),string_to_atom(V2,CVC),!.
toUppercase(95,45):-!.
toUppercase(I,O):-integer(I),!,to_upper(I,O).
toUppercase([A|L],[AO|LO]):-
   toUppercase(A,AO),!,
   toUppercase(L,LO),!.
toUppercase(MiXed,CASED):-atom(MiXed),upcase_atom(MiXed,CASED),!.
toUppercase(MiXed,CASED):-atom(MiXed),!,
   atom_codes(MiXed,Codes),
   toUppercase(Codes,UCodes),
   atom_codes(CASED,UCodes),!.
toUppercase(MiXed,CASED):-compound(MiXed),MiXed=..MList,toUppercase(MList,UList),!,CASED=..UList.
toUppercase(A,A).

toLowercase(MiXed,MiXed):-noCaseChange(MiXed),!.
toLowercase(V,V2):-string(V),!,atom_codes(V,VC),toLowercase(VC,CVC),string_to_atom(V2,CVC),!.
toLowercase(95,45):-!.
toLowercase(I,O):-integer(I),!,to_lower(I,O).
toLowercase([A|L],[AO|LO]):-
   toLowercase(A,AO),!,
   toLowercase(L,LO),!.
toLowercase(MiXed,CASED):-atom(MiXed),downcase_atom(MiXed,CASED),!.
toLowercase(MiXed,CASED):-atom(MiXed),!,
   atom_codes(MiXed,Codes),
   toLowercase(Codes,UCodes),
   atom_codes(CASED,UCodes),!.
toLowercase(MiXed,CASED):-compound(MiXed),MiXed=..MList,toLowercase(MList,UList),!,CASED=..UList.
toLowercase(A,A).

toPropercase(VAR,VAR):-var(VAR),!.
toPropercase([],[]):-!.
toPropercase([CX|Y],[D3|YY]):-!,toPropercase(CX,D3),toPropercase(Y,YY).
toPropercase(D3,DD3):-atom(D3),camelSplitters(V),concat_atom([L,I|ST],V,D3),toPropercase([L,I|ST],LIST2),toPropercase(V,VV),concat_atom(LIST2,VV,DD3).
toPropercase(CX,Y):-atom(CX),name(CX,[S|SS]),char_type(S,to_lower(NA)),name(NA,[N]),name(Y,[N|SS]),!.
toPropercase(MiXed,UPPER):-compound(MiXed),MiXed=..MList,toPropercase(MList,UList),!,UPPER=..UList.
toPropercase(A,A).


toCamelcase(VAR,VAR):-var(VAR),!.
toCamelcase([],[]):-!.
toCamelcase([CX|Y],[D3|YY]):-!,toCamelcase(CX,D3),toCamelcase(Y,YY).
toCamelcase(D3,DD3):-atom(D3),camelSplitters(V),concat_atom([L,I|ST],V,D3),toCamelcase([L,I|ST],LIST2),toCamelcase(V,VV),concat_atom(LIST2,VV,DD3).
toCamelcase(CX,Y):-atom(CX),name(CX,[S|SS]),char_type(S,to_upper(NA)),name(NA,[N]),name(Y,[N|SS]),!.
toCamelcase(MiXed,UPPER):-compound(MiXed),MiXed=..MList,toCamelcase(MList,UList),!,UPPER=..UList.
toCamelcase(A,A).

      
% ===========================================================
% Quote-Unquote
% ===========================================================

quoteAtomString([34|T],Out):-name(Out,[34|T]),!.
quoteAtomString([H|T],Out):-!,append([34,H|T],[34],Quote),name(Out,Quote).
quoteAtomString(QU,QU):-concat_atom(['"'|_],QU),!.
quoteAtomString(UQ,QU):-concat_atom(['"',UQ,'"'],QU),!.

unquoteAtom(Atom,New):-concat_atom(LIST,'"',Atom),concat_atom(LIST,'',New),!.

% ===========================================================
% string/chars/codes
% ===========================================================

is_charlist([X]):-atom(X),not(number(X)),atom_length(X,1).
is_charlist([X|T]):-atom(X),not(number(X)),atom_length(X,1),is_charlist(T),!.

is_codelist([A]):-integer(A),!,A>8,A<129,!.
is_codelist([A|L]):-integer(A),!,A>8,A<129,is_codelist(L).

is_string(X):-atom(X),!,atom_length(X,L),L>1,atom_concat('"',_,X),atom_concat(_,'"',X),!.
is_string(X):-var(X),!,fail.
is_string(string(_)):-!.
is_string("").
is_string(X):-string(X),!.
is_string(L):-is_charlist(L),!.
is_string(L):-is_codelist(L),!.


isWhitespace(32).
isWhitespace(N):-N<33;N>128.


% ===========================================================
% escapeString/Codes/Chars
% ===========================================================

escapeString(R,RS):- (string(R);is_list(R)) ,string_to_atom(R,A),atom_codes(A,Codes),escapeCodes([34,92],92,Codes,RS),!.

escapeCodes(_Escaped,_EscapeChar,[],[]):-!.
escapeCodes(Escaped,EscapeChar,[EscapeChar,Skip|Source],[EscapeChar,Skip|New]):-!,
   escapeCodes(Escaped,EscapeChar,Source,New),!.
escapeCodes(Escaped,EscapeChar,[Char|Source],[EscapeChar,Char|New]):-member(Char,Escaped),!,
   escapeCodes(Escaped,EscapeChar,Source,New),!.
escapeCodes(Escaped,EscapeChar,[Skipped|Source],[Skipped|New]):-
   escapeCodes(Escaped,EscapeChar,Source,New),!.


% ===========================================================
% [d|r]estringify/Codes/Chars
% ===========================================================

destringify(X,X):-var(X);number(X),!.
destringify(X,S):-is_string(X),stringToCodelist(X,CL),name(S,CL),!.
destringify([],[]):-!.
destringify([H|T],[HH|TT]):-!,destringify(H,HH),destringify(T,TT),!.
destringify(X,P):-compound(X),X=..LIST,destringify(LIST,DL),P=..DL,!.
destringify(X,X):-not(atom(X)),!.
destringify(B,A):-atom_concat('#$',A,B),!.
destringify(B,B):-!.

%stringToList(X,Y):-writeq(string_to_list(X,Y)),nl,fail.
stringToList(X,Y):-var(X),!,string_to_list(X,Y).
stringToList([],[]).
stringToList("",[]).
stringToList(X,Y):-atom(X),atom_codes(X,Codes),!,stringToList(Codes,Y),!.
stringToList(X,Y):-string(X),string_to_atom(X,M),!,stringToList(M,Y).
stringToList(X,Y):-string(X),!,string_to_list(X,Y).
stringToList(X,Y):-is_string(X),!,string_to_list(X,Y).
stringToList([X|XX],Y):-concat_atom([X|XX],' ',XXX),!,string_to_list(XXX,Y).
%prologPredToCyc(Predicate):-arity(PredicateHead)

stringToCodelist(S,CL):-stringToCodelist2(S,SL),!,escapeString(SL,CS),!,stringToList(CL,CS),!.

stringToCodelist2(string(S),Codes):-!,stringToCodelist2(S,Codes).
stringToCodelist2([],[]):-!.
stringToCodelist2([[]],[]):-!.
stringToCodelist2([''],[]):-!.
stringToCodelist2([X|T],[X|T]):-is_codelist([X|T]),!.
stringToCodelist2([X|T],Codes):-atom(X),is_charlist([X|T]),!,stringToList([X|T],Codes),!.
stringToCodelist2(String,Codes):-string(String),!,string_to_atom(String,Atom),atom_codes(Atom,Codes),!.
stringToCodelist2(Atom,Codes):-atom(Atom),atom_codes(Atom,Codes),!.
stringToCodelist2(A,Codes):- to_string_hook(A,_,L),atom_codes(L,Codes),!.
stringToCodelist2(Term,Codes):-sformat(Codes,'~q',[Term]),true.


%===================================================================
% Removes Leading and Trailing whitespaces and non ANSI charsets.
%====================================================================
:-assert(show_this_hide(trim,2)).
:-current_prolog_flag(double_quotes,X),asserta(double_quotes(X)).
:-set_prolog_flag(double_quotes,codes).

trim(S,Y):-flatten(S,S2),trim2(S2,Y).

trim2(S,Y):-
      ground(S),%true,
      stringToList(S,X),
      ltrim(X,R),lists:reverse(R,Rvs), 
      addSpaceBeforeSym(Rvs,Rv),      
      ltrim(Rv,RY),lists:reverse(RY,Y),!.
     
addSpaceBeforeSym([H|T],[H,32|T]):-member(H,"?.!"),!.
addSpaceBeforeSym(H,H).

:-retract(double_quotes(X)),set_prolog_flag(double_quotes,X).
:-set_prolog_flag(double_quotes,string).

ltrim([],[]):-!.
ltrim([32,32,32,32,32,32,32|String],Out) :-trim(String,Out),!.
ltrim([32,32,32,32,32|String],Out) :-trim(String,Out),!.
ltrim([32,32,32|String],Out) :- trim(String,Out),!.
ltrim([32,32|String],Out) :- trim(String,Out),!.
ltrim([P|X],Y):- (isWhitespace(P);not(number(P));P<33;P>128),trim(X,Y),!.
ltrim(X,X).

any_to_string(Atom,String):- hotrace(once(any_to_string0(Atom,String))).

any_to_string0(Atom,String):-var(Atom),!,term_to_atom(Atom,AAtom),!,any_to_string0(AAtom,String),!.
any_to_string0(Atom,String):-string(Atom),Atom=String,!.
any_to_string0(Atom,String):-atom(Atom),atom_string(Atom,String),!.
any_to_string0([Atom],String):-atom(Atom),atom_string(Atom,String),!.
any_to_string0(A,""):-nonvar(A),member(A,[[],'',""]),!.
any_to_string0(List,String):-catch(text_to_string(List,String),_,fail),!.
any_to_string0(List,String):-is_list(List), (catch(atomics_to_string(List, ' ', String),_,fail);((list_to_atomics_list0(List,AList),catch(atomics_to_string(AList, ' ', String),_,fail)))),!.
any_to_string0(List,String):-sformat(String,'~q',[List]).

list_to_atomics_list0(Var,A):-var(Var),!,any_to_string(Var,A),!.
list_to_atomics_list0([E|EnglishF],[A|EnglishA]):-
   any_to_string(E,A),
   list_to_atomics_list0(EnglishF,EnglishA),!.
list_to_atomics_list0([],[]):-!.


catch_read_term_from_atom(Sub,Term,NewOnes):-catch(read_term_from_atom(Sub,Term,[module(user),variable_names(NewOnes)]),_,fail),Term\==end_of_file.

splt_words(Atom,Terms,Var):- catch((hotrace(once(splt_words_0(Atom,Terms,Var)))),_,fail),!.
splt_words(Atom,Words1,[]):- catch(atomic_list_concat(Words1,' ',Atom),_,fail),!.
splt_words_0('',[],[]):-!.
splt_words_0(Atom,[Term|List],Vars):- atom_length(Atom,To),between(0,To,X), 
      sub_atom(Atom,0,Len,X,Sub),Len>0,
      catch_read_term_from_atom(Sub,Term,NewOnes),
      (compound(Term)->sub_atom(Sub,_,1,0,')');true),
      sub_atom(Atom,Len,_,0,Next),
      splt_words_0(Next,List,NewVars),
      merge_vars(NewVars,NewOnes,Vars),!.
splt_words_0(Atom,[L0|ListO],Vars):-atomic_list_concat([L0,L1|List],' ',Atom),atomic_list_concat([L1|List],' ',Atom2),!,
      splt_words_0(Atom2,ListO,Vars),!.


merge_vars(NewVars,[],NewVars).
merge_vars([],NewVars,NewVars).
merge_vars([X=Y|More],OldVars,NewVars):-member(X=Y,OldVars),!,
   merge_vars(More,OldVars,NewVars).
merge_vars([X=Y|More],OldVars,[X=Y|NewVars]):-
   merge_vars(More,OldVars,NewVars).

vars_to_ucase(_,List):-ground(List),!.
vars_to_ucase(Vars,[L|List]):- var(L),!,vars_to_ucase_0(Vars,[L|List]),!.
vars_to_ucase(Vars,[_|List]):- vars_to_ucase(Vars,List).

vars_to_ucase_0([],_).
vars_to_ucase_0([N=V|Vars],List):-
   ignore(N=V),
   vars_to_ucase_0(Vars,List).

atomSplit(In,List):- hotrace(( ground(In),
 any_to_string(In,String),atom_string(Atom,String),splt_words(Atom,List,Vars),vars_to_ucase(Vars,List))),!.
atomSplit(Atom,WordsO):- 
   hotrace((atomSplit(Atom,WordsO,[' ','\t','\n','\v','\f','\r',' ','!','"','#','$','%','&','\'',
    '(',')','*','+',',','-','.','/',':',';','<',
    '=','>','?','@','[',\,']','^','_',
    '`','{','|','}','~']
    ))).
   
%%atomSplit(Atom,WordsO):- atomSplit(Atom,WordsO,[' ','\'',';',',','"','`',':','?','!','.','\n','\t','\r','\\','*','%','(',')','#']),!.

atomSplit(S,WordsO,List):- hotrace(( string(S),string_to_atom(S,Atom), atomic_list_concat(Words1,' ',Atom),!, atomSplit2(Words1,Words,List),!, Words=WordsO )).
atomSplit(Atom,WordsO,List):- hotrace(( atom(Atom), atomic_list_concat(Words1,' ',Atom),!, atomSplit2(Words1,Words,List),!, Words=WordsO )).
atomSplit(Atom,Words,[Space|AtomO]):-hotrace((var(Atom),ground(Words),!,atomic_list_concat(Words,Space,AtomO),!,Atom=AtomO)).


atomSplit2([],[],_List):-!.
atomSplit2([Mark|S],[Mark|Words],List):- member(Mark,List),!,atomSplit2(S,Words,List),!.
atomSplit2([W|S],[A,Mark|Words],List):- member(Mark,List),atom_concat(A,Mark,W),!,atomSplit2(S,Words,List),!.
atomSplit2([W|S],[Mark,A|Words],List):- member(Mark,List),atom_concat(Mark,A,W),!,atomSplit2(S,Words,List),!.
atomSplit2([Word|S],Words,List):- member(Space,List),Atoms=[_,_|_],atomic_list_concat(Atoms,Space,Word),!,
                  interleave(Atoms,Space,Left),
                  atomSplit2(S,Right,List),append(Left,Right,WordsM),!,atomSplit2(WordsM,Words,List),!.
atomSplit2([W|S],[W|Words],List):-atomSplit2(S,Words,List),!.

interleave([''],Space,[Space]):-!.
interleave([Atom],_Space,[Atom]):-!.
interleave([''|More],Space,[Space|Result]):-interleave(More,Space,Result),!.
interleave([Atom|More],Space,[Atom,Space|Result]):-interleave(More,Space,Result),!.


