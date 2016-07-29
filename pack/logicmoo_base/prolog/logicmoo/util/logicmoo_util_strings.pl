% ===================================================================
% File 'logicmoo_util_strings.pl'
% Purpose: Common Logicmoo library Functions for Strings
% Maintainers: Douglas Miles/Annie Ogborn/Kino Coursey
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_util_strings.pl' 1.0.0
% Revision:  $Revision: 1.7 $
% Revised At:   $Date: 2002/07/11 21:57:28 $
% ===================================================================

% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_strings.pl
:- module(logicmoo_util_strings,
          [ addSpaceBeforeSym/2,
            all_upper_atom/1,
            isSlot/1,
            any_to_atom/2,
            any_to_string/2,
            any_to_string1/2,
            any_to_string_or_var/2,
            append_ci/3,
            append_ci0/3,
            as_atom/2,
            as_nc_str/2,
            atomSplit/2,
            atomSplit/3,
            atomSplit2/3,
            atomSplitEasy/2,
            atom_contains/2,
            atom_subst/4,
            atomic_concat/4,
            atomic_list_concat_catch/3,
            atomic_list_concat_safe/2,
            atomic_list_concat_safe/3,
            atomsSameCI/2,
            atoms_of/2,
            both_empty/2,
            breaked_codes/2,
            camelSplitters/1,
            capitalized/1,
            catch_read_term_from_atom/3,
            char_type_this/2,
            clean_codes/2,
            clean_out_atom/2,
            concat_atom_safe/2,
            concat_atom_safe/3,
            convert_members/3,
            convert_to_string/2,
            ctype_continue/2,
            ctype_switch/2,
            ctype_switcher/1,
            destringify/2,
            divide_list/3,
            either_empty/2,
            either_starts_with_icase/2,
            empty_str/1,
            empty_string/1,
            ends_with_icase/2,
            equals_icase/2,
            escapeCodes/4,
            escapeString/2,
            first_char_to_lower/2,
            first_char_to_upper/2,
            hide_char_type/1,
            interleave/3,
            isWhitespace/1,
            is_charlist/1,
            is_codelist/1,
            is_ending/1,
            is_ftString2/1,
            is_simple_split/1,
            list_replace/4,
            longest_string/3,
            ltrim/2,
            map_tree_to_list/3,
          must_assign/1,
          must_assign/2,
            member_ci/2,
            merge_vars/3,
            must_nonvar/1,
            noCaseChange/1,
            non_empty/1,
            ommitable/1,
            quoteAtomString/2,
            read_stream_to_arglist/2,
            rejoined/3,
            remove_predupes/2,
            replace_in_string/4,
            replace_in_string/5,
            replace_periods/2,
            replace_periods_string_list/2,
            sort_by_strlen/2,
            splt_words/3,
            splt_words_0/3,
            splt_words_0_atom/3,
            starts_or_ends_with_icase/2,
            starts_with_icase/2,
            str_contains_all/2,
            str_contains_all0/2,
            stringToCodelist/2,
            stringToCodelist2/2,
            stringToList/2,
            string_ci/2,
            string_dedupe/2,
            string_equal_ci/2,
            string_equal_ci0/2,
            string_equal_ci1/2,
            string_to_atom_safe/2,
            text_to_string_safe/2,
            toCamelcase/2,
            toCase/3,
            toCaseSplit/4,
            toLowercase/2,
            toPropercase/2,
            toUppercase/2,
            to_case_breaks/2,
            to_case_breaks/5,
            to_first_break/2,
            to_first_break/5,
            to_first_break_w/6,
            to_list_of_sents/2,
            to_titlecase/2,
            to_word_list/2,
            to_word_list_0/2,
            to_word_list_2/2,
            trim/2,
            trim2/2,
            unCamelcase/2,
            unquoteAtom/2,
            vars_to_ucase/2,
            vars_to_ucase_0/2
          ]).
% autoloading user:portray_clause_pi/2 from /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_first
:- multifile % (multifile) :-
        
        
        double_quotes_was_strings/1,
        string_predicate/1,
        to_string_hook/3.
:- meta_predicate 
        map_tree_to_list(2, ?, ?),
        toCase(2, ?, ?),
        toCaseSplit(?, 2, ?, ?).
:- module_transparent % (module_transparent) :-
        addSpaceBeforeSym/2,
        all_upper_atom/1,
        any_to_atom/2,
        any_to_string/2,
        any_to_string1/2,
        any_to_string_or_var/2,
        append_ci/3,
        append_ci0/3,
        as_atom/2,
        as_nc_str/2,
        atomSplit/2,
        atomSplit/3,
        atomSplit2/3,
        atomSplitEasy/2,
        atom_contains/2,
        atom_subst/4,
        atomic_concat/4,
        atomic_list_concat_catch/3,
        atomic_list_concat_safe/2,
        atomic_list_concat_safe/3,
        atomsSameCI/2,
        atoms_of/2,
        both_empty/2,
        breaked_codes/2,
        camelSplitters/1,
        capitalized/1,
        catch_read_term_from_atom/3,
        char_type_this/2,
        clean_codes/2,
        clean_out_atom/2,
        concat_atom_safe/2,
        concat_atom_safe/3,
        convert_to_string/2,
        ctype_continue/2,
        ctype_switch/2,
        ctype_switcher/1,
        destringify/2,
        divide_list/3,
        double_quotes_was_strings/1,
        either_empty/2,
        either_starts_with_icase/2,
        empty_str/1,
        empty_string/1,
        ends_with_icase/2,
        equals_icase/2,
        escapeCodes/4,
        escapeString/2,
        first_char_to_lower/2,
        first_char_to_upper/2,
        hide_char_type/1,
        interleave/3,
        isWhitespace/1,
        is_charlist/1,
        is_codelist/1,
        is_ending/1,
        is_ftString2/1,
        is_simple_split/1,
        list_replace/4,
        longest_string/3,
        ltrim/2,
        member_ci/2,
        merge_vars/3,
        must_nonvar/1,
        noCaseChange/1,
        non_empty/1,
        ommitable/1,
        quoteAtomString/2,
        read_stream_to_arglist/2,
        rejoined/3,
        remove_predupes/2,
        replace_in_string/4,
        replace_in_string/5,
        replace_periods/2,
        replace_periods_string_list/2,
        sort_by_strlen/2,
        splt_words/3,
        splt_words_0/3,
        splt_words_0_atom/3,
        starts_or_ends_with_icase/2,
        starts_with_icase/2,
        str_contains_all/2,
        str_contains_all0/2,
        stringToCodelist/2,
        stringToCodelist2/2,
        stringToList/2,
        string_ci/2,
        string_dedupe/2,
        string_equal_ci/2,
        string_equal_ci0/2,
        string_equal_ci1/2,
        string_to_atom_safe/2,
        text_to_string_safe/2,
        toCamelcase/2,
        toLowercase/2,
        toPropercase/2,
        toUppercase/2,
        to_case_breaks/2,
        to_case_breaks/5,
        to_first_break/2,
        to_first_break/5,
        to_first_break_w/6,
        to_list_of_sents/2,
        to_string_hook/3,
        to_titlecase/2,
        to_word_list/2,
        to_word_list_0/2,
        to_word_list_2/2,
        trim/2,
        trim2/2,
        unCamelcase/2,
        unquoteAtom/2,
        vars_to_ucase/2,
        vars_to_ucase_0/2.

:- dynamic % (dynamic) :-
        
        
        double_quotes_was_strings/1,
        to_string_hook/3.

:- include('logicmoo_util_header.pi').


:- meta_predicate toCase(2,?,?).
:- meta_predicate toCaseSplit(?,2,?,?).

:- use_module(library(check)).
% :- use_module(library(check),[check:string_predicate/1]).
:- multifile(check:string_predicate/1).
:- dynamic(check:string_predicate/1).

% this is a backwards compatablity block for SWI-Prolog 6.6.6
:- multifile(double_quotes_was_strings/1).
:- dynamic(double_quotes_was_strings/1).
:- current_prolog_flag(double_quotes,WAS),asserta(double_quotes_was_strings(WAS)).
:- retract(double_quotes_was_strings(WAS)),set_prolog_flag(double_quotes,WAS).
:- current_prolog_flag(double_quotes,WAS),asserta(double_quotes_was_strings(WAS)).
:- set_prolog_flag(double_quotes,string).

% :-import(must/1).


%= 	 	 

%% string_to_atom_safe( ?ISO, ?LISTO) is semidet.
%
% String Converted To Atom Safely Paying Attention To Corner Cases.
%
string_to_atom_safe(ISO,LISTO):-LISTO==[],!,string_to_atom(ISO,'').
string_to_atom_safe(ISO,LISTO):- string_to_atom(ISO,LISTO).

/*
string_chars(S,C):-atom_chars(S,C).
text_to_string(T,S):- string_to_atom(S,T).
string_upper(M,U):-toUppercase(M,U).
string_lower(M,U):-toLowercase(M,U).
*/

% :- ensure_loaded(logicmoo_util_bugger).
:- meta_predicate map_tree_to_list(2,?,*).


% = :- meta_predicate(camelSplitters(+)).

% = :- meta_predicate(to_string_hook(-,-,+)).
:- multifile(to_string_hook/3).
:- dynamic(to_string_hook/3).


%= 	 	 

%% camelSplitters( ?V) is semidet.
%
% Camel Splitters.
%
camelSplitters(V):-arg(_,v(' ','-','_',':' /*,'mt','doom','Mt','Doom'*/ ),V).



%= 	 	 

%% concat_atom_safe( ?I, ?O) is semidet.
%
% Concat Atom Safely Paying Attention To Corner Cases.
%
concat_atom_safe(I,O):-concat_atom_safe(I,'',O).


%= 	 	 

%% concat_atom_safe( ?A, ?B, ?C) is semidet.
%
% Concat Atom Safely Paying Attention To Corner Cases.
%
concat_atom_safe(A,B,C):-!,atomic_list_concat_safe(A,B,C).

concat_atom_safe([],_,O):-nonvar(O),!,O==''.
concat_atom_safe(L,_,''):-nonvar(L),!,L==[].
concat_atom_safe(A,B,C):-concat_atom(A,B,C).

%================================================================
% Atom / String functions
%================================================================

%= 	 	 

%% atomsSameCI( ?Name1, ?Name1) is semidet.
%
% Atoms Same Class For Internal Interface.
%
atomsSameCI(Name1,Name1):-!.
atomsSameCI(Name1,Name2):-atom(Name1),atom(Name2),downcase_atom(Name1,D1),downcase_atom(Name2,D2),!,D1=D2.


%= 	 	 

%% clean_codes( ?X, ?Y) is semidet.
%
% Clean Codes.
%
clean_codes(X,Y):-trim(X,Y),!.  % actually cyc:trim/2
clean_codes(X,X).

%clean_out_atom(X,Y):-atomSplit(X,C),delete(C,'',O),concat_atom_safe(C,' ',Y).

%= 	 	 

%% clean_out_atom( ?X, ?Y) is semidet.
%
% Clean Out Atom.
%
clean_out_atom(X,Y):-atom_codes(X,C),clean_codes(C,D),!,atom_codes(X,D),!,Y=X.

%%atomSplit(A,B):-token_stream_of(A,AA),findall(B0,arg(1,AA,B),B).


%= 	 	 

%% all_upper_atom( ?X) is semidet.
%
% All Upper Atom.
%
all_upper_atom(X):-toUppercase(X,N),!,N=X.


%= 	 	 

%% atom_contains( ?F, ?X) is semidet.
%
% Atom Contains.
%
atom_contains(F,X):-sub_string(F,_,_,_,X).
% atom_contains(F0,C0):- must((any_to_atom(F0,F),!,any_to_atom(C0,C))),!,sub_string(F,_,_,_,C).


%= 	 	 

%% any_to_atom( ?A, ?A) is semidet.
%
% Any Converted To Atom.
%
any_to_atom(A,A):-atom(A),!.
any_to_atom(T,A):-sformat(S,'~w',[T]),atom_string(A,S).


%= 	 	 

%% any_to_string_or_var( ?StringO, ?String) is semidet.
%
% Any Converted To String Or Variable.
%
any_to_string_or_var(StringO,String):- (var(StringO);var(String)),!,String=StringO.
any_to_string_or_var(StringO,String):- any_to_string(StringO,StringOS1),any_to_string(String,StringOS2),!,StringOS1=StringOS2.


%= 	 	 

%% atomic_list_concat_safe( ?List, ?StringO) is semidet.
%
% Atomic List Concat Safely Paying Attention To Corner Cases.
%
atomic_list_concat_safe(List,StringO):- ground(List),!,atomics_to_string(List,String),any_to_string_or_var(StringO,String).
atomic_list_concat_safe(List,V):- (V=='';V==""),!,List=[].

atomic_list_concat_safe([Atom,A2|Bonus],V):-atomic(Atom),atomic(A2),string_concat(Atom,A2,A3),!,atomic_list_concat_safe([A3|Bonus],V).
atomic_list_concat_safe([Atom|Bonus],V):-atomic(Atom),atomic(V),string_concat(Atom,NV,V),!,atomic_list_concat_safe(Bonus,NV).
atomic_list_concat_safe([D1,Atom|Bonus],V):-var(D1),atomic(Atom),sub_string(V, NBefore, _Len, _NumAfter, Atom),
      sub_string(V, 0, NBefore, _, D1), atomic_list_concat_safe([D1,Atom|Bonus],V).
atomic_list_concat_safe([V],V):-!.


%= 	 	 

%% atomic_list_concat_safe( ?List, ?Sep, ?StringO) is semidet.
%
% Atomic List Concat Safely Paying Attention To Corner Cases.
%
atomic_list_concat_safe(List,Sep,StringO):- (Sep==[];Sep=='';Sep==""),!,atomic_list_concat_safe(List,StringO).
atomic_list_concat_safe(List,Sep,Str):-ground(Sep:Str),not(atom_contains(Str,Sep)),!,List=[Str0],any_to_string_or_var(Str,Str0).
atomic_list_concat_safe(List,Sep,StringO):- ground(List:Sep),!,atomics_to_string(List,Sep,String),any_to_string_or_var(StringO,String).
atomic_list_concat_safe(List,_,V):- (V=='';V==""),!,List=[].
atomic_list_concat_safe(List,Sep,StringO):-ground(StringO),ground(Sep),not(atom_contains(StringO,Sep)),!,List=[D1O],any_to_string_or_var(StringO,D1O).
atomic_list_concat_safe([Atom,A2|Bonus],Sep,V):-atomic(Atom),atomic(A2),atomic_list_concat_safe([Atom,Sep,A2],A3),atomic_list_concat_safe([A3|Bonus],Sep,V),!.
atomic_list_concat_safe([Atom|Bonus],Sep,V):-atomic(Atom),atomic(V),atomic_list_concat_safe([Atom,Sep,NV],V),!,atomic_list_concat_safe(Bonus,NV).
atomic_list_concat_safe([D1,PostAtom|Bonus],Sep,V):-var(D1),atomic(Atom),atomic(Sep),string_concat(Sep,PostAtom,Atom),
  % We calc D1
  sub_string(V, NBefore, _Len, NumAfter, Atom),sub_string(V, 0, NBefore, _, D1O),
  sub_string(V,_,NumAfter,0,NewV),atomic_list_concat_safe(Bonus,Sep,NewV),!,
  any_to_string_or_var(D1,D1O).
atomic_list_concat_safe([D1|Bonus],AtomSep,V):-var(D1),atomic(AtomSep),
  % We calc D1
  sub_string(V, NBefore, _Len, NumAfter, AtomSep),sub_string(V, 0, NBefore, _, D1O),!,
  sub_string(V,_,NumAfter,0,NewV),atomic_list_concat_safe(Bonus,AtomSep,NewV),!,
  any_to_string_or_var(D1,D1O).
atomic_list_concat_safe([V],_Sep,V):-!.




% convert any ftTerm to 'atom' string

%= 	 	 

%% convert_to_string( ?I, ?ISO) is semidet.
%
% Convert Converted To String.
%
convert_to_string(I,ISO):-
                logicmoo_util_first:term_to_string(I,IS),!,
		string_to_list(IS,LIST),!,
		list_replace(LIST,92,[92,92],LISTM),
		list_replace(LISTM,34,[92,34],LISTO),!,
		text_to_string(LISTO,ISO). % string_to_atom_safe(ISO,LISTO),!.


%= 	 	 

%% list_replace( ?List, ?Char, ?Replace, ?NewList) is semidet.
%
% List Replace.
%
list_replace(List,Char,Replace,NewList):-
	append(Left,[Char|Right],List),
	append(Left,Replace,NewLeft),
	list_replace(Right,Char,Replace,NewRight),
	append(NewLeft,NewRight,NewList),!.
list_replace(List,_Char,_Replace,List):-!.


:- export(atomic_concat/3).

%= 	 	 

%% atomic_concat( ?A, ?B, ?C, ?Out) is semidet.
%
% Atomic Concat.
%
atomic_concat(A,B,C,Out):-atomic_list_concat_safe([A,B,C],Out).

% :-atomic_list_concat_safe([A,'/',C],'','foo/bar/baz').
% ===========================================================
% CASE CHANGE
% ===========================================================

isSlot(V):- is_ftVar(V).


%= 	 	 

%% noCaseChange( ?VAR) is semidet.
%
% No Case Change.
%
noCaseChange(VAR):- is_ftVar(VAR),!.
% noCaseChange(VAR):- if_defined(isSlot(VAR)),!.
noCaseChange([]):-!.
noCaseChange(MiXed):-atom(MiXed),atom_concat('#$',_,MiXed),!.
noCaseChange(c(_)):-!.


%= 	 	 

%% first_char_to_upper( ?CX, ?Y) is semidet.
%
% First Char Converted To Upper.
%
first_char_to_upper(CX,Y):- name(CX,[S|SS]),char_type(S,to_lower(NA)),name(NA,[N]),name(Y,[N|SS]),!.

%= 	 	 

%% first_char_to_lower( ?CX, ?Y) is semidet.
%
% First Char Converted To Lower.
%
first_char_to_lower(CX,Y):- name(CX,[S|SS]),char_type(S,to_upper(NA)),name(NA,[N]),name(Y,[N|SS]),!.

%= 	 	 

%% to_titlecase( ?CX, ?Y) is semidet.
%
% Converted To Titlecase.
%
to_titlecase(CX,Y):- sub_string(CX,1,_,0,Z),string_lower(Z,L), name(CX,[S|_]),char_type(S,to_lower(NA)),atom_concat(NA,L,Y).


%= 	 	 

%% text_to_string_safe( ?Expr, ?Forms) is semidet.
%
% Text Converted To String Safely Paying Attention To Corner Cases.
%
text_to_string_safe(Expr,Forms):-on_x_fail(text_to_string(Expr,Forms)).



%= 	 	 

%% toLowercase( ?I, ?O) is semidet.
%
% Converted To Lowercase.
%
toLowercase(I,O):-integer(I),!,to_lower(I,O).
toLowercase(I,O):-toCase(downcase_atom,I,O).


%= 	 	 

%% toUppercase( ?I, ?O) is semidet.
%
% Converted To Uppercase.
%
toUppercase(I,O):-integer(I),!,to_upper(I,O).
toUppercase(I,O):-toCase(upcase_atom,I,O).


%= 	 	 

%% toCamelcase( ?I, ?O) is semidet.
%
% Converted To Camelcase.
%
toCamelcase(I,O):-toCaseSplit('',first_char_to_upper,I,O).


%= 	 	 

%% unCamelcase( ?I, ?O) is semidet.
%
% Un Camelcase.
%
unCamelcase(I,O):-toCaseSplit('_',first_char_to_lower,I,O).


%= 	 	 

%% toPropercase( ?I, ?O) is semidet.
%
% Converted To Propercase.
%
toPropercase(I,O):-toCaseSplit(_Same,to_titlecase,I,O).



%= 	 	 

%% toCase( :PRED2Pred, ?MiXed, ?MiXed) is semidet.
%
% Converted To Case.
%
toCase(_Pred,MiXed,MiXed):-noCaseChange(MiXed),!.
toCase(_Pred,95,45):-!.
toCase( Pred,MiXed,CASED):-atom(MiXed),!,call(Pred,MiXed,CASED),!.
toCase( Pred,D3,DD3):- text_to_string_safe(D3,S),!,string_to_atom(S,A3),toCase(Pred,A3,DD3).
toCase( Pred,D3,DD3):- is_list(D3),atomic_list_concat(D3," ",Str),maplist(toCase( Pred),Str,DD3).
toCase( Pred,D3,DD3):- is_list(D3),!,maplist(toCase( Pred),D3,DD3).
toCase( Pred,MiXed,CASED):-compound(MiXed),MiXed=..MList,maplist(toCase(Pred),MList,UList),!,CASED=..UList.




%= 	 	 

%% toCaseSplit( ?UPARAM1, :PRED2VALUE2, ?MiXed, ?MiXed) is semidet.
%
% Converted To Case Split.
%
toCaseSplit(_,_,[Empty],[]):-nonvar(Empty), (empty_str(Empty);camelSplitters(Empty)),!.
toCaseSplit(_,_,Empty,''):- (empty_str(Empty);camelSplitters(Empty)),!.
toCaseSplit(_,_,MiXed,MiXed):-noCaseChange(MiXed),!.
toCaseSplit(Rejoin,Pred,D3,DD3):-atom(D3),!,
  ((camelSplitters(V),concat_atom([L,I|ST],V,D3))->
   (maplist(Pred,[L,I|ST],LIST2),rejoined(Rejoin,V,VV),concat_atom(LIST2,VV,DD3));
   toCase(Pred,D3,DD3)).
toCaseSplit(Rejoin,Pred,D3,DD3):-text_to_string_safe(D3,S),!,string_to_atom(S,A3),toCaseSplit(Rejoin,Pred,A3,DD3).
toCaseSplit(Rejoin,Pred,LI,OUT):-is_list(LI),!,maplist(toCaseSplit(Rejoin,Pred),LI,LO),ignore(VV=Rejoin),ignore(VV=''),concat_atom(LO,VV,OUT).
toCaseSplit(Rejoin,Pred,[CX|Y],[D3|YY]):-!,toCaseSplit(Rejoin,Pred,CX,D3),toCaseSplit(Rejoin,Pred,Y,YY).
toCaseSplit(_     ,Pred,MiXed,UPPER):-must((compound(MiXed),MiXed=..MList,toCaseSplit(' ',Pred,MList,UList),!,UPPER=..UList)).


%= 	 	 

%% rejoined( ?Rejoin, ?V, ?VV) is semidet.
%
% Rejoined.
%
rejoined(Rejoin,V,VV):-ignore(Rejoin=VV),ignore(V=VV),!.



%= 	 	 

%% empty_str( ?E) is semidet.
%
% Empty Str.
%
empty_str(E):-ground(E),memberchk(E,[``,[],"",'']).



% ===========================================================
% CHECK CASE
% ===========================================================
:- export(capitalized/1).


%= 	 	 

%% capitalized( ?Type) is semidet.
%
% Capitalized.
%
capitalized(Type):- string_codes(Type,[S|_]),char_type(S,upper),must(char_type(S,alpha)).

% ===========================================================
% BREAKING ON CASE CHANGE
% ===========================================================
:- export(to_case_breaks/2).

%= 	 	 

%% to_case_breaks( ?Text, ?New) is semidet.
%
% Converted To Case Breaks.
%
to_case_breaks(Text,New):- string_codes(Text,[C|Codes]), char_type_this(C,WillBe),!,
  to_case_breaks(Codes,WillBe,[C],WillBe,New).


%= 	 	 

%% char_type_this( ?C, ?Lower) is semidet.
%
% Char Type This.
%
char_type_this(C,Lower):-ctype_switcher(Lower),char_type(C,Lower),!.


%= 	 	 

%% ctype_switcher( ?VALUE1) is semidet.
%
% Ctype Switcher.
%
ctype_switcher(lower).
ctype_switcher(upper).
ctype_switcher(digit).
ctype_switcher(punct).
ctype_switcher(white).


%= 	 	 

%% breaked_codes( ?S, ?C0) is semidet.
%
% Breaked Codes.
%
breaked_codes(S,C0):- nonvar(S),on_x_fail(write_to_codes(S,C)),!,C=C0.
breaked_codes(S,C):- catch(number_codes(S,C),_,
  string_codes(S,C)->true;(atom_codes(S,C)->true;string_equal_ci(S,C))).


%= 	 	 

%% ctype_continue( ?X, ?X) is semidet.
%
% Ctype Continue.
%
ctype_continue(upper,lower).
ctype_continue(X,X):-ctype_switcher(X).


%= 	 	 

%% ctype_switch( ?T1, ?T2) is semidet.
%
% Ctype Switch.
%
ctype_switch(upper,upper).
ctype_switch(T1,T2):-ctype_switcher(T1),ctype_switcher(T2),T1\=T2.



%= 	 	 

%% hide_char_type( ?VALUE1) is semidet.
%
% Hide Char Type.
%
hide_char_type(white).
hide_char_type(punct).

% to_case_breaks(+Codes,+SoFarC,+Lower,List)

%= 	 	 

%% to_case_breaks( :TermC___Codes, ?WillBe, ?SoFar, ?Upper, ?OUT) is semidet.
%
% Converted To Case Breaks.
%
to_case_breaks([      ],_WillBe,   [],_,     []):-!.
to_case_breaks([       ],WillBe,SoFar,_NewType,[t(Left,WillBe)]):-breaked_codes(Left,SoFar),!.
to_case_breaks([C|Codes],WillBe,SoFar,Upper,New):- ctype_continue(Upper,Lower), char_type(C,Lower),append(SoFar,[C],SoFarC),!,to_case_breaks(Codes,WillBe,SoFarC,Lower,New).
to_case_breaks(C___Codes,WillBe,SoFar,Upper,OUT):- is_list(OUT),OUT=[t(Left,WillBe),t(New,RType)],!,to_first_break_w(C___Codes,SoFar,Upper,Left,New,RType).
to_case_breaks([C|Codes],WillBe,SoFar,Upper,OUT):- ctype_switch(Upper,Digit), char_type(C,Digit),!, breaked_codes(Left,SoFar), to_case_breaks(Codes,Digit,[C],Digit,New),!,(hide_char_type(WillBe)->OUT=New;OUT=[t(Left,WillBe)|New]).
to_case_breaks([C|Codes],WillBe,SoFar,Upper,New):- append(SoFar,[C],SoFarC),to_case_breaks(Codes,WillBe,SoFarC,Upper,New).

:- export(to_first_break/2).

%= 	 	 

%% to_first_break( ?Text, ?Left) is semidet.
%
% Converted To First Break.
%
to_first_break(Text,Left):-to_first_break(Text,_LeftType,Left,_Right,_NextType).
:- export(to_first_break/5).

%= 	 	 

%% to_first_break( ?Text, ?LType, ?Left, ?Right, ?RType) is semidet.
%
% Converted To First Break.
%
to_first_break(Text,LType,Left,Right,RType):- string_codes(Text,[C|Codes]), char_type_this(C,LType),!,to_first_break_w(Codes,[C],LType,Left,Right,RType).
to_first_break(Text,LType,Left,Right,RType):- string_codes(Text,[C|Codes]), !,to_first_break_w(Codes,[C],LType,Left,Right,RType).


%= 	 	 

%% to_first_break_w( :TermARG1, ?SoFar, ?Some, ?Left, ?VALUE5, ?VALUE6) is semidet.
%
% Converted To First Break W.
%
to_first_break_w([],       []   ,empty,'',[],empty):-!.
to_first_break_w([],       SoFar,_Some,Left,[],empty):-breaked_codes(Left,SoFar),!.
to_first_break_w([C|Codes],SoFar,Upper,Left,Rest,RType):- ctype_continue(Upper,Lower), char_type(C,Lower),append(SoFar,[C],SoFarC),!,to_first_break_w(Codes,SoFarC,Lower,Left,Rest,RType).
to_first_break_w([C|Codes],SoFar,Lower,Left,Rest,Upper):- ctype_switch(Lower,Upper), char_type(C,Upper), breaked_codes(Left,SoFar),!,breaked_codes(Rest,[C|Codes]).
to_first_break_w([C|Codes],SoFar,Lower,Left,Rest,RType):- append(SoFar,[C],SoFarC),to_first_break_w(Codes,SoFarC,Lower,Left,Rest,RType).

% ===========================================================
% Quote-Unquote
% ===========================================================


%= 	 	 

%% quoteAtomString( ?QU, ?QU) is semidet.
%
% Quote Atom String.
%
quoteAtomString([34|T],Out):-name(Out,[34|T]),!.
quoteAtomString([H|T],Out):-!,append([34,H|T],[34],Quote),name(Out,Quote).
quoteAtomString(QU,QU):-concat_atom_safe(['"'|_],QU),!.
quoteAtomString(UQ,QU):-concat_atom_safe(['"',UQ,'"'],QU),!.


%= 	 	 

%% unquoteAtom( ?Atom, ?New) is semidet.
%
% Unquote Atom.
%
unquoteAtom(Atom,New):-concat_atom_safe(LIST,'"',Atom),concat_atom_safe(LIST,'',New),!.

% ===========================================================
% string/chars/codes
% ===========================================================


%= 	 	 

%% is_charlist( :TermX) is semidet.
%
% If Is A Charlist.
%
is_charlist([X]):-atom(X),not(number(X)),atom_length(X,1).
is_charlist([X|T]):-atom(X),not(number(X)),atom_length(X,1),is_charlist(T),!.


%= 	 	 

%% is_codelist( :TermA) is semidet.
%
% If Is A Codelist.
%
is_codelist([A]):-integer(A),!,A>8,A<129,!.
is_codelist([A|L]):-integer(A),!,A>8,A<129,is_codelist(L).


%= 	 	 

%% is_ftString2( ?X) is semidet.
%
% If Is A Format Type String Extended Helper.
%
is_ftString2(X):- atom(X),!,atom_length(X,L),L>1,atom_concat('"',_,X),atom_concat(_,'"',X),!.
is_ftString2(X):- var(X),!,fail.
is_ftString2(string(_)):-!.
is_ftString2("").
is_ftString2(X):- string(X),!.
is_ftString2(L):-is_charlist(L),!.
is_ftString2(L):-is_codelist(L),!.



%= 	 	 

%% isWhitespace( ?N) is semidet.
%
% If Is A Whitespace.
%
isWhitespace(32).
isWhitespace(N):-N<33;N>128.


% ===========================================================
% escapeString/Codes/Chars
% ===========================================================


%= 	 	 

%% escapeString( ?R, ?RS) is semidet.
%
% Escape String.
%
escapeString(R,RS):- (string(R);is_list(R)) ,string_to_atom(R,A),atom_codes(A,Codes),escapeCodes([34,92],92,Codes,RS),!.


%= 	 	 

%% escapeCodes( ?Escaped, ?EscapeChar, :TermARG3, :TermARG4) is semidet.
%
% Escape Codes.
%
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



%= 	 	 

%% destringify( :TermX, :TermX) is semidet.
%
% Destringify.
%
destringify(X,X):-(var(X);number(X)),!.
destringify('$VAR'(S),'$VAR'(S)):-!.
destringify(string(S),string(S)):-is_ftVar(S),!.
destringify([],[]):-!.
destringify('[]','[]'):-!.
destringify(T,A):- on_x_fail(call((text_to_string(T,S),!,atom_string(A,S)))),!.
destringify(X,S):-is_ftString2(X),stringToCodelist(X,CL),name(S,CL),!.
destringify([H|T],[HH|TT]):-!,destringify(H,HH),destringify(T,TT),!.
destringify(X,P):-compound(X),X=..LIST,maplist(destringify,LIST,DL),P=..DL,!.
destringify(B,A):- (atom(A),atom_concat('#$',A,B))->true;A=B.

%stringToList(X,Y):-writeq(string_to_list(X,Y)),nl,fail.

%= 	 	 

%% stringToList( :TermX, ?Y) is semidet.
%
% String Converted To List.
%
stringToList(X,Y):-var(X),!,string_to_list(X,Y).
stringToList([],[]).
stringToList("",[]).
stringToList(X,Y):-atom(X),atom_codes(X,Codes),!,stringToList(Codes,Y),!.
stringToList(X,Y):- string(X),string_to_atom(X,M),!,stringToList(M,Y).
stringToList(X,Y):- string(X),!,string_to_list(X,Y).
stringToList(X,Y):-is_ftString2(X),!,string_to_list(X,Y).
stringToList([X|XX],Y):-concat_atom_safe([X|XX],' ',XXX),!,string_to_list(XXX,Y).
%prologPredToCyc(Predicate):-arity(PredicateHead)


%= 	 	 

%% stringToCodelist( ?S, ?CL) is semidet.
%
% String Converted To Codelist.
%
stringToCodelist(S,CL):- stringToCodelist2(S,SL),!,escapeString(SL,CS),!,stringToList(CL,CS),!.


%= 	 	 

%% stringToCodelist2( :TermS, ?Codes) is semidet.
%
% String Converted To Codelist Extended Helper.
%
stringToCodelist2(S,_Codes):-var(S),!,fail.
stringToCodelist2(string(S),Codes):-!,stringToCodelist2(S,Codes).
stringToCodelist2([],[]):-!.
stringToCodelist2([[]],[]):-!.
stringToCodelist2([''],[]):-!.
stringToCodelist2([X|T],[X|T]):-is_codelist([X|T]),!.
stringToCodelist2([X|T],Codes):-atom(X),is_charlist([X|T]),!,stringToList([X|T],Codes),!.
stringToCodelist2(String,Codes):- string(String),!,string_to_atom(String,Atom),atom_codes(Atom,Codes),!.
stringToCodelist2(Atom,Codes):-atom(Atom),atom_codes(Atom,Codes),!.
stringToCodelist2(A,Codes):- to_string_hook(A,_,L),atom_codes(L,Codes),!.
stringToCodelist2(Term,Codes):-sformat(Codes,'~q',[Term]),true.


%===================================================================
% Removes Leading and Trailing whitespaces and non ANSI charsets.
%====================================================================
:- assert(show_this_hide(trim,2)).
:- current_prolog_flag(double_quotes,X),asserta(double_quotes_string_was(X)).
:- set_prolog_flag(double_quotes,codes).


%= 	 	 

%% trim( ?S, ?Y) is semidet.
%
% Trim.
%
trim(S,Y):-flatten(S,S2),trim2(S2,Y).


%= 	 	 

%% trim2( ?S, ?Y) is semidet.
%
% Trim Extended Helper.
%
trim2(S,Y):-
      ground(S),%true,
      stringToList(S,X),
      ltrim(X,R),lists:reverse(R,Rvs),
      addSpaceBeforeSym(Rvs,Rv),
      ltrim(Rv,RY),lists:reverse(RY,Y),!.


%= 	 	 

%% addSpaceBeforeSym( ?H, ?H) is semidet.
%
% Add Space Before Sym.
%
addSpaceBeforeSym([H|T],[H,32|T]):-member(H,"?.!"),!.
addSpaceBeforeSym(H,H).

:- retract(double_quotes_string_was(X)),set_prolog_flag(double_quotes,X).
:- set_prolog_flag(double_quotes,string).


%= 	 	 

%% ltrim( ?X, ?X) is semidet.
%
% Ltrim.
%
ltrim([],[]):-!.
ltrim([32,32,32,32,32,32,32|String],Out) :-trim(String,Out),!.
ltrim([32,32,32,32,32|String],Out) :-trim(String,Out),!.
ltrim([32,32,32|String],Out) :- trim(String,Out),!.
ltrim([32,32|String],Out) :- trim(String,Out),!.
ltrim([P|X],Y):- (isWhitespace(P);not(number(P));P<33;P>128),trim(X,Y),!.
ltrim(X,X).



%= 	 	 

%% any_to_string( ?Atom, ?String) is semidet.
%
% Any Converted To String.
%
any_to_string(Atom,String):- with_err_to_pred(nop, (must(any_to_string1(Atom,StringS)),!,StringS=String)),!.


%= 	 	 

%% any_to_string1( ?Atom, ?String) is semidet.
%
% Any Converted To String Secondary Helper.
%
any_to_string1(Atom,String):- string(Atom),!,Atom=String.
any_to_string1(O,O):-stack_depth(X),X>2000,!,sanity(fail).
any_to_string1(Atom,String):-var(Atom),!,term_string(Atom,String).
any_to_string1(Atom,String):- var(Atom),!,=(Atom,String).
any_to_string1(Atom,String):- number(Atom),!,number_string(Atom,String).
any_to_string1("",String):- !,""=String.
any_to_string1(``,String):- !,""=String.
any_to_string1('',String):- !,""=String.
any_to_string1([],String):- !,""=String.
any_to_string1('[]',String):- !,""=String.
any_to_string1(fmt(Fmt,Args),String):-!,sformat(String,Fmt,Args).
any_to_string1(txtFormatFn(Fmt,Args),String):-!,sformat(String,Fmt,Args).
any_to_string1(Atom,String):-atom(Atom),!,atom_string(Atom,String).
any_to_string1([Atom],String):-nonvar(Atom),!,any_to_string1(Atom,String).
any_to_string1(A,""):-nonvar(A),member(A,[[],'',"",``]),!.
any_to_string1(List,String):- text_to_string_safe(List,String),!.
%any_to_string1(List,String):- fail, dtrace, is_list(List), (on_x_fail(atomics_to_string(List, ' ', String)); ((list_to_atomics_list0(List,AList),on_x_fail(atomics_to_string(AList, ' ', String))))),!.
any_to_string1(List,String):- on_x_fail(format(string(String),'~s',[List])).
any_to_string1(Term,String):- on_x_fail(term_string(Term,String)).
any_to_string1(List,String):- on_x_fail(format(string(String),'~p',[List])).
any_to_string1(List,String):- format(string(String),'~q',[List]).
/*
list_to_atomics_list0(Var,A):-var(Var),!,any_to_string(Var,A),!.
list_to_atomics_list0([E|EnglishF],[A|EnglishA]):-
   any_to_string(E,A),
   list_to_atomics_list0(EnglishF,EnglishA),!.
list_to_atomics_list0([],[]):-!.
*/

:- export(atomic_list_concat_catch/3).

%= 	 	 

%% atomic_list_concat_catch( ?List, ?Sep, ?Atom) is semidet.
%
% Atomic List Concat Catch.
%
atomic_list_concat_catch(List,Sep,Atom):-catch(atomic_list_concat_safe(List,Sep,Atom),E,(dumpST,dmsg(E:atomic_list_concat_safe(List,Sep,Atom)),!,fail)).



%= 	 	 

%% catch_read_term_from_atom( ?Sub, ?Term, ?NewOnes) is semidet.
%
% Catch Read Term Converted From Atom.
%
catch_read_term_from_atom(Sub,Term,NewOnes):-
  on_x_fail(read_term_from_atom(Sub,Term,[module(user),variable_names(NewOnes)])),Term\==end_of_file.

:- export(splt_words/3).

%= 	 	 

%% splt_words( ?Atom, ?Terms, ?Var) is semidet.
%
% Splt Words.
%
splt_words(Atom,Terms,Var):- on_x_fail((hotrace(once(splt_words_0(Atom,Terms,Var))))),!.
splt_words(Atom,Words1,[]):- on_x_fail(atomic_list_concat_safe(Words1,' ',Atom)),!.


%= 	 	 

%% splt_words_0( ?S, ?Terms, ?Var) is semidet.
%
% splt words  Primary Helper.
%
splt_words_0(S,Terms,Var):-any_to_atom(S,Atom),!,splt_words_0_atom(Atom,Terms,Var),!.


%= 	 	 

%% splt_words_0_atom( ?Atom, :TermTerm, ?Vars) is semidet.
%
% splt words  Primary Helper atom.
%
splt_words_0_atom('',[],[]):-!.
splt_words_0_atom(Atom,[Term|List],Vars):- atom(Atom),atom_length(Atom,To),between(0,To,X),
      sub_atom(Atom,0,Len,X,Sub),Len>0,
      catch_read_term_from_atom(Sub,Term,NewOnes),
      (compound(Term)->sub_atom(Sub,_,1,0,')');true),
      sub_atom(Atom,Len,_,0,Next),
      splt_words_0_atom(Next,List,NewVars),
      merge_vars(NewVars,NewOnes,Vars),!.
splt_words_0_atom(Atom,[L0|ListO],Vars):-atomic_list_concat([L0,L1|List],' ',Atom),atomic_list_concat([L1|List],' ',Atom2),!,
      splt_words_0_atom(Atom2,ListO,Vars),!.




%= 	 	 

%% merge_vars( :TermNewVars, ?VALUE2, :TermNewVars) is semidet.
%
% Merge Variables.
%
merge_vars(NewVars,[],NewVars).
merge_vars([],NewVars,NewVars).
merge_vars([X=Y|More],OldVars,NewVars):-member(X=Y,OldVars),!,
   merge_vars(More,OldVars,NewVars).
merge_vars([X=Y|More],OldVars,[X=Y|NewVars]):-
   merge_vars(More,OldVars,NewVars).


%= 	 	 

%% vars_to_ucase( ?VALUE1, :TermList) is semidet.
%
% Variables Converted To Ucase.
%
vars_to_ucase(_,List):-ground(List),!.
vars_to_ucase(Vars,[L|List]):- var(L),!,vars_to_ucase_0(Vars,[L|List]),!.
vars_to_ucase(Vars,[_|List]):- vars_to_ucase(Vars,List).


%= 	 	 

%% vars_to_ucase_0( :TermN, ?List) is semidet.
%
% Variables Converted To ucase  Primary Helper.
%
vars_to_ucase_0([],_).
vars_to_ucase_0([N=V|Vars],List):-
   ignore(N=V),
   vars_to_ucase_0(Vars,List).


%= 	 	 

%% atomSplit( ?In, ?List) is semidet.
%
% Atom Split.
%
atomSplit(In,List):- hotrace(( ground(In),
 any_to_string(In,String),
    splt_words(String,List,Vars),vars_to_ucase(Vars,List))),!.

atomSplit(Atom,WordsO):-atomSplitEasy(Atom,WordsO),!.

:- export(atomSplitEasy/2).

%= 	 	 

%% atomSplitEasy( ?Atom, ?WordsO) is semidet.
%
% Atom Split Easy.
%
atomSplitEasy(Atom,WordsO):-
   hotrace((atomSplit(Atom,WordsO,[' ','\t','\n','\v','\f','\r',' ','!','"','#','$','%','&','\'',
    '(',')','*','+',',','-','.','/',':',';','<',
    '=','>','?','@','[',\,']','^','_',
    '`','{','|','}','~']
    ))).

%%atomSplit(Atom,WordsO):- atomSplit(Atom,WordsO,[' ','\'',';',',','"','`',':','?','!','.','\n','\t','\r','\\','*','%','(',')','#']),!.


%= 	 	 

%% atomSplit( ?S, ?WordsO, :TermList) is semidet.
%
% Atom Split.
%
atomSplit(S,WordsO,List):- hotrace(( atomic(S),atomic_list_concat_safe(Words1," ",S),!, atomSplit2(Words1,Words,List),!, Words=WordsO )).
atomSplit(Atom,WordsO,List):- hotrace(( atom(Atom), atomic_list_concat_safe(Words1,' ',Atom),!, atomSplit2(Words1,Words,List),!, Words=WordsO )).
atomSplit(Atom,Words,[Space|AtomO]):-hotrace((var(Atom),ground(Words),!,atomic_list_concat_safe(Words,Space,AtomO),!,Atom=AtomO)).



%= 	 	 

%% atomSplit2( :TermARG1, :TermARG2, ?List) is semidet.
%
% Atom Split Extended Helper.
%
atomSplit2([],[],_List):-!.
atomSplit2([Mark|S],[Mark|Words],List):- member(Mark,List),!,atomSplit2(S,Words,List),!.
atomSplit2([W|S],[A,Mark|Words],List):- member(Mark,List),atom_concat(A,Mark,W),!,atomSplit2(S,Words,List),!.
atomSplit2([W|S],[Mark,A|Words],List):- member(Mark,List),atom_concat(Mark,A,W),!,atomSplit2(S,Words,List),!.
atomSplit2([Word|S],Words,List):- member(Space,List),Atoms=[_,_|_],atomic_list_concat_safe(Atoms,Space,Word),!,
                  interleave(Atoms,Space,Left),
                  atomSplit2(S,Right,List),append(Left,Right,WordsM),!,atomSplit2(WordsM,Words,List),!.
atomSplit2([W|S],[W|Words],List):-atomSplit2(S,Words,List),!.


%= 	 	 

%% interleave( :TermARG1, ?Space, :TermSpace) is semidet.
%
% Interleave.
%
interleave([''],Space,[Space]):-!.
interleave([Atom],_Space,[Atom]):-!.
interleave([''|More],Space,[Space|Result]):-interleave(More,Space,Result),!.
interleave([Atom|More],Space,[Atom,Space|Result]):-interleave(More,Space,Result),!.



%================================================================
% decends tree
%================================================================



%% must_assign( :TermFrom) is semidet.
%
% Must Be Successfull Assign.
%
must_assign(From=To):-must_assign(From,To).

%= 	 	 

%% must_assign( ?From, ?To) is semidet.
%
% Must Be Successfull Assign.
%
must_assign(From,To):-To=From,!.
must_assign(From,To):- tlbugger:skipMust,!,ignore(To=From),!.
must_assign(From,To):-dmsg(From),dmsg(=),dmsg(From),dmsg(must_assign),!,dtrace,To=From.


	 	 

%% map_tree_to_list( :PRED2VALUE1, ?PATTERN, ?Output) is semidet.
%
% Map Tree Converted To List.
%
map_tree_to_list(_,PATTERN,Output):- (var(PATTERN);number(PATTERN)),!,must_assign([PATTERN],Output).
map_tree_to_list(_,[],OUT):-!,must_assign([],OUT).
map_tree_to_list(Pred,IN,Output):- once(call(Pred,IN,MID)),must((MID=IN -> flatten([MID],OUT) ; map_tree_to_list(Pred,MID,OUT))),!,must_assign(OUT=Output).
map_tree_to_list(Pred,[I|IN],Output):-!,with_each(on_f_debug((map_tree_to_list(Pred,I,O1),map_tree_to_list(Pred,IN,O2),!,append(O1,O2,OUT)))),!,must_assign(OUT=Output).
map_tree_to_list(Pred,IN,Output):-atom(IN),!,must((atomSplit(IN,MID),!,map_tree_to_list(Pred,MID,OUT))),!,must_assign(OUT=Output).
map_tree_to_list(Pred,IN,Output):-
 must((compound(IN), IN=..INP, append(Left,[Last],INP), map_tree_to_list(Pred,Last,UT),!,
  append(Left,[UT],OUTP),!, OUT =.. OUTP)),must_assign([OUT],Output).
map_tree_to_list(_,IN,IN):- dtrace,must_assign([IN],IN).


%= 	 	 

%% non_empty( ?A) is semidet.
%
% Not Empty.
%
non_empty(A):-must_det(not(empty_string(A))).

%= 	 	 

%% must_nonvar( ?A) is semidet.
%
% Must Be Successfull Nonvar.
%
must_nonvar(A):-one_must(nonvar(A),trace_or_throw(must_nonvar(A))).



%= 	 	 

%% both_empty( ?A, ?B) is semidet.
%
% Both Empty.
%
both_empty(A,B):-empty_string(A),!,empty_string(B),nop(dmsg(both_empty(A,B))).

%= 	 	 

%% either_empty( ?A, ?B) is semidet.
%
% Either Empty.
%
either_empty(A,B):- (empty_string(B);empty_string(A)),!,nop(dmsg(either_empty(A,B))).


%= 	 	 

%% equals_icase( ?A, ?B) is semidet.
%
% Equals Icase.
%
equals_icase(A,B):-either_empty(A,B),!,fail.
equals_icase(A,B):- string_ci(A,U),string_ci(B,U).

%= 	 	 

%% starts_with_icase( ?A, ?B) is semidet.
%
% Starts Using Icase.
%
starts_with_icase(A,B):-either_empty(A,B),!,fail.
starts_with_icase(A,B):- string_ci(A,UA),string_ci(B,UB),non_empty(UB),atom_concat(UB,_,UA).
starts_with_icase(A,B):-both_empty(A,B),dmsg(warn(equals_icase(A,B))).

%= 	 	 

%% either_starts_with_icase( ?A, ?B) is semidet.
%
% Either Starts Using Icase.
%
either_starts_with_icase(A,B):-either_empty(A,B),!,fail.
either_starts_with_icase(A,B):- string_ci(A,UA),string_ci(B,UB),non_empty(UA),non_empty(UB),(atom_concat(UB,_,UA);atom_concat(UA,_,UB)).

%= 	 	 

%% starts_or_ends_with_icase( ?A, ?B) is semidet.
%
% Starts Or Ends Using Icase.
%
starts_or_ends_with_icase(A,B):-either_empty(A,B),!,fail.
starts_or_ends_with_icase(A,B):- string_ci(A,UA),string_ci(B,UB),non_empty(UA),non_empty(UB),(atom_concat(UB,_,UA);atom_concat(_,UA,UB)).

%= 	 	 

%% ends_with_icase( ?A, ?B) is semidet.
%
% Ends Using Icase.
%
ends_with_icase(A,B):-either_empty(A,B),!,fail.
ends_with_icase(A,B):- string_ci(A,UA),string_ci(B,UB),non_empty(UB),atom_concat(_,UB,UA).


%= 	 	 

%% string_dedupe( ?StringI, ?StringO) is semidet.
%
% String Dedupe.
%
string_dedupe(StringI,StringO):- to_word_list(StringI,Words),remove_predupes(Words,StringO).

:- multifile(check:string_predicate/1).

%= 	 	 

%% string_predicate( :PRED1VALUE1) is semidet.
%
% Hook To [check:string_predicate/1] For Module Logicmoo_util_strings.
% String Predicate.
%
:- asserta(check:string_predicate(remove_predupes/2)).


%= 	 	 

%% remove_predupes( :TermListI, ?ListO) is semidet.
%
% Remove Predupes.
%
remove_predupes([],[]).
remove_predupes(ListI,ListO):- member(L0,["",[],"",'',[32],' ']),member(L0,ListI),delete(ListI,L0,ListM),!,remove_predupes(ListM,ListO),!.
remove_predupes([L|ListI], ListO):- (member_ci(L,ListI) -> remove_predupes(ListI,ListO) ; (remove_predupes(ListI,ListM),[L|ListM]=ListO)),!.


%= 	 	 

%% member_ci( ?W, ?WL) is semidet.
%
% Member Ci.
%
member_ci(L,[List|I]):-!,member(LL2,[List|I]),string_equal_ci(LL2,L).
member_ci(W,WL):-to_word_list(WL,ListI),member(LL2,ListI),string_equal_ci(LL2,W).


%= 	 	 

%% string_ci( ?A, ?LIC) is semidet.
%
% String Ci.
%
string_ci(A,LIC):-hotrace((must(nonvar(A)),non_empty(A),any_to_string(A,S),!,text_to_string(S,SS),string_lower(SS,SL),atomics_to_string(SLIC,"_",SL),
   atomics_to_string(SLIC," ",LIC))),!.

:- export(append_ci/3).

%= 	 	 

%% append_ci( ?A1, ?A2, ?A3) is semidet.
%
% Append Ci.
%
append_ci(A1,A2,A3):-to_word_list(A1,L1),to_word_list(A2,L2),to_word_list(A3,L3),!, append_ci0(L1,L2,L3),!.


%= 	 	 

%% append_ci0( ?L, ?L2, ?R) is semidet.
%
% Append Ci Primary Helper.
%
append_ci0([],L1,L2):- string_equal_ci(L1,L2),!.
append_ci0(L,L2,R):-divide_list(L,H1,L1),divide_list(R,H2,L3),string_equal_ci(H1,H2),!,append_ci0(L1,L2,L3).


%= 	 	 

%% divide_list( ?L, ?L0, ?LT) is semidet.
%
% Divide List.
%
divide_list(L,L0,LT):-is_list(L),!,length(L,X),X1 is X-1,between(1,X1,RS),length(LT,RS),append(L0,LT,L).
divide_list(L,L0,LT):-append(L0,LT,L).


%= 	 	 

%% string_equal_ci( ?A0, ?A0) is semidet.
%
% String Equal Ci.
% 
string_equal_ci(L0,L1):- 
 to_word_list(L0,WL0),WL0\==[],to_word_list(L1,WL1)->WL1\==[],!,
   string_equal_ci0(WL0,WL1),!.
    % (string_equal_ci0(WL0,WL1);(var(L1),freeze(L1,string_equal_ci0(WL0,WL1)))).


%= 	 	 

%% string_equal_ci0( ?L0, ?R0) is semidet.
%
% String Equal Ci Primary Helper.
%
string_equal_ci0([],_):-!,fail.
%string_equal_ci0(_,[]):-!,fail.
string_equal_ci0(L0,R0):- string_equal_ci1(L0,R0),!.
string_equal_ci0(L,R):-divide_list(L,L0,LT),divide_list(R,R0,RT),string_equal_ci1(L0,R0),!,string_equal_ci0(LT,RT).


%= 	 	 

%% string_equal_ci1( ?A0, ?A0) is semidet.
%
% String Equal Ci Secondary Helper.
%
string_equal_ci1(A0,A0):-!.
string_equal_ci1([],_):-!,fail.
string_equal_ci1(_,[]):-!,fail.
string_equal_ci1(A0,B0):-as_nc_str(A0,AR),as_nc_str(B0,BR),!, AR = BR.



%= 	 	 

%% as_nc_str( ?A0, ?ASL) is semidet.
%
% Converted To Nc Str.
%
as_nc_str([A0,'\'',B0],AS):-atomic_list_concat_safe([A0,'\'',B0],'',AO),as_nc_str(AO,AS).
as_nc_str([A0,'\'',B0],AS):-as_nc_str([A0,B0],AS).
as_nc_str([A0,Ommitable,B0],AS):- once(ommitable(Ommitable)), as_nc_str([A0,B0],AS).
as_nc_str([Ommitable,B0],AS):- once(ommitable(Ommitable)), as_nc_str([B0],AS).
as_nc_str([A0,B0],AS):-atom_concat(A0,B0,AO),as_nc_str(AO,AS).
as_nc_str(A0,ASL):-any_to_string(A0,AS),string_lower(AS,ASL).
% as_nc_str(A0,A0).


%= 	 	 

%% ommitable( ?O) is semidet.
%
% Ommitable.
%
ommitable(O):-empty_string(O).
ommitable(O):- string_to_atom(O,A),atom_length(A,L),!,L<2.


%= 	 	 

%% atom_subst( ?A, ?F, ?R, ?K) is semidet.
%
% Atom Subst.
%
atom_subst(A,F,R,K):-replace_in_string(F,R,A,K),!.



%= 	 	 

%% empty_string( ?A) is semidet.
%
% Empty String.
%
empty_string(A):-var(A),!.
empty_string([]).
empty_string('').
empty_string("").



% Meta-Interp that appends the arguments to the calls
:- meta_predicate convert_members(?,?,?).

%= 	 	 

%% convert_members( ?Call, ?In, ?Out) is semidet.
%
% Convert Members.
%
convert_members([], InOut,InOut).
convert_members([A,!|B], In,Out):- !, convert_members(A,In,M),!,convert_members(B,M,Out).
convert_members([A|B], In,Out):- !, convert_members(A,In,M),convert_members(B,M,Out).
convert_members(once(Call), In,Out):- !, convert_members(Call, In,Out),!.
convert_members(ht(Call), In,Out):- !, Call=..[P|MID],CallOut=..[P,In|MID],call(CallOut,Out).
convert_members(ico(In,Call,Out), In,Out):-  Call=..[P|MID],CallOut=..[P,In|MID],call(CallOut,Out).
convert_members(cio(In,Call,Out), In,Out):-  call(Call,In,Out).
convert_members(ic(InOut,Call), InOut,InOut):-  call(Call).
convert_members(call(Call), InOut,InOut):-  call(Call).
convert_members(Call, In,Out):- call(call,Call,In,Out).


%= 	 	 

%% replace_in_string( ?SepChars, ?PadChars, ?Repl, ?A, ?C) is semidet.
%
% Replace In String.
%
replace_in_string(SepChars, PadChars,Repl, A,C):- split_string(A,SepChars,PadChars,B),atomics_to_string(B,Repl,C).


%= 	 	 

%% replace_in_string( ?F, ?R, ?A, ?K) is semidet.
%
% Replace In String.
%
replace_in_string(F,R,OriginalString,K):-atom(OriginalString),!,atom_string(OriginalString,S),replace_in_string(F,R,S,ModifiedString),atom_string(K,ModifiedString).
replace_in_string(Find,Repl,OriginalString,ModifiedString):-  atomic_list_concat(List,Find,OriginalString),atomics_to_string(List,Repl,ModifiedString).


%= 	 	 

%% replace_periods( ?A, ?S) is semidet.
%
% Replace Periods.
%
replace_periods(A,S):-
 convert_members([
   %  white space
    replace_in_string(`\r`,` `),
    replace_in_string(`\n`,` `),
    % replace_in_string(`\s`,` `),
    replace_in_string(`'`,` apostraphyMARK `),
    replace_in_string(`\t`,` `),
    % only remove leading and trailing white space
   % at(split_string(``, `\s\t\n`)),
    % respace the spaces
    replace_in_string(` `, ` `, ` `),
    % add a space on the end
    ht(string_concat(` `)),
    replace_in_string(`?`,` ? `),
    replace_in_string(`!`,` ! `),
    replace_in_string(`.`,` . `),
    % replace periods
   replace_in_string(`. `,` periodMARK `)
   ],A,S).

% ?- replace_periods_string_list("hi there bub. how are you.",X),to_list_of_sents(X,L).
% X = [hi, there, bub, '.', how, are, you, '.'],
% L = [[hi, there, bub, '.'], [how, are, you, '.']] .
%
% ?- replace_periods_string_list("hi there bub! how are you?",X),to_list_of_sents(X,L).
% X = [hi, there, bub, !, how, are, you, ?],
% L = [[hi, there, bub, !], [how, are, you, ?]] .


%= 	 	 

%% to_list_of_sents( ?WList, ?FirstSent) is semidet.
%
% Converted To List Of Sentences.
%
to_list_of_sents([],[]).
to_list_of_sents(WList,[sent(FirstSent)|Groups]):-append(Left,[Last|Rest],WList),member(Last,['.','?','!']),!,append(Left,[Last],FirstSent),!,to_list_of_sents(Rest,Groups).
to_list_of_sents(WList,[sent(WList)]).


%= 	 	 

%% replace_periods_string_list( ?A, ?S) is semidet.
%
% Replace Periods String List.
%
replace_periods_string_list(A,S):-replace_periods(A,AR),to_word_list(AR,WL),subst(WL,periodMARK,'.',WLS),subst(WLS,apostraphyMARK,'\'',S).


%= 	 	 

%% to_word_list( ?A, ?SL) is semidet.
%
% Converted To Word List.
%
to_word_list(A,SL):-var(A),!,A=SL.
to_word_list(A,SL):-once(hotrace((to_word_list_0(A,S0),(is_list(S0)->delete(S0,'',S);S=S0)))),
   maplist(as_atom,S,SSL),!,
   must(SSL=SL),!.

% as_atom(A,A):-atom(A),!.
% 

%= 	 	 

%% as_atom( ?A, ?A) is semidet.
%
% Converted To Atom.
%
as_atom(A,A):-atom(A),!.
as_atom(T,A):- compound(T), \+ is_list(T),arg(_,T,M),atomic(M),as_atom(M,A),!.
as_atom(S,A):- string_to_atom(S,A),!.
as_atom(T,A):- term_to_atom(T,A).

:- export(to_word_list_0/2).

%= 	 	 

%% to_word_list_0( ?V, ?V) is semidet.
%
% Converted To word list  Primary Helper.
%
to_word_list_0(V,V):-var(V),!.
to_word_list_0([A],[A]):-number(A),!.
to_word_list_0(E,[]):-empty_str(E),!.
to_word_list_0([A|C],[A|C]):- string(A),!.
%to_word_list_0(A,WList):- string(A),Final=" (period) ",replace_periods(A,Final,S),not(A=S),!,to_word_list_0(S,WList),!.
%to_word_list_0([A|C],[A|C]):- (atomic(A);catch((text_to_string([A|C],_),fail),_,true)),!.
to_word_list_0(A,WList):- must(any_to_string(A,String)),!,must(text_to_string(String,Atom)),to_word_list_2(Atom,WList),!.



%= 	 	 

%% read_stream_to_arglist( ?Input, :TermARG2) is semidet.
%
% Read Stream Converted To Arglist.
%
read_stream_to_arglist(Input,[]):- at_end_of_stream(Input),!.
read_stream_to_arglist(Input,[]):- on_x_fail((once(wait_for_input([Input], Inputs, 0.01)),Inputs=[])),!.
read_stream_to_arglist(Input,[H|T]):-show_call(why,if_defined(lisp_read_from_input(Input,H))),!,(is_ending(H)->T=[];read_stream_to_arglist(Input,T)),!.


%= 	 	 

%% is_ending( ?List) is semidet.
%
% If Is A Ending.
%
is_ending(List):-nonvar(List),(is_list(List)->last(List,whitepace("\n"));List==whitepace("\n")).


:- asserta(check:string_predicate(is_simple_split/1)).

%= 	 	 

%% is_simple_split( ?S) is semidet.
%
% If Is A Simple Split.
%
is_simple_split(S):-text_to_string(S,SS),split_string(SS,"().!\"\'","()",O),!,O=[SS].


%= 	 	 

%% to_word_list_2( ?Input, ?WList) is semidet.
%
% Converted To word list  Extended Helper.
%
to_word_list_2(Input,WList):-is_simple_split(Input),split_string(Input," "," ",WList),!.
to_word_list_2(A,S):-atomSplit(A,S),!.
to_word_list_2(Atom,WList):- atom_to_memory_file(Atom,File),open_memory_file(File,read,Stream),read_stream_to_arglist(Stream,WList).
to_word_list_2(Input,WList):- open_string(Input,Stream),read_stream_to_arglist(Stream,WList).
to_word_list_2(Input,Input).


%= 	 	 

%% str_contains_all( ?A, ?String) is semidet.
%
% Str Contains All.
%
str_contains_all([],String):- dtrace(wdmsg(str_contains_all([],String))).
str_contains_all(A,String):- empty_string(String), dtrace(str_contains_all(A,String)).
str_contains_all(A,SL):- string_ci(SL,SLIC),SL\=SLIC,!,str_contains_all(A,SLIC).
str_contains_all(List,String):-str_contains_all0(List,String).


%= 	 	 

%% str_contains_all0( :TermA, ?String) is semidet.
%
% Str Contains All Primary Helper.
%
str_contains_all0([],_).
str_contains_all0([A|Atoms],String):-
      string_ci(A,L),
      sub_string(String,_,_,Aft,L),
      sub_string(String,Aft,_,0,SubString),!,
      str_contains_all0(Atoms,SubString).



%= 	 	 

%% atoms_of( :TermC, ?L) is semidet.
%
% Atoms Of.
%
atoms_of(Var,[]):- (var(Var);Var==[]),!.
atoms_of(':',[]).
atoms_of('moo',[]).
atoms_of('t',[]).
atoms_of(',',[]).
atoms_of(':-',[]).
atoms_of('$VAR',[]):-!.
atoms_of(Atom,[]):-number(Atom),!.
atoms_of(Atom,[Atom]):-atomic(Atom),!.
atoms_of([H|T],L):-!,atoms_of(H,HL),atoms_of(T,TL),append(HL,TL,L),!.
atoms_of(C,L):-C=..CL,atoms_of(CL,L),!.


%= 	 	 

%% sort_by_strlen( ?List, ?Sorted) is semidet.
%
% Sort By Strlen.
%
sort_by_strlen(List,Sorted):-predsort(longest_string,List,Sorted).

% longest_string(?Order, @Term1, @Term2)

%= 	 	 

%% longest_string( ?Order, ?TStr1, ?TStr2) is semidet.
%
% Longest String.
%
longest_string(Order,TStr1,TStr2):-
   text_to_string(TStr1,Str1),string_length(Str1,L1),
   text_to_string(TStr2,Str2),string_length(Str2,L2),
   compare(Order,L2-Str2,L1-Str1).


% this is a backwards compatablity block for SWI-Prolog 6.6.6
:- retract(double_quotes_was_strings(WAS)),set_prolog_flag(double_quotes,WAS).


