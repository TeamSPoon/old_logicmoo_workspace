

% ===================================================
% Make Args
% ===================================================

make_args_out(bach,[],Vars,''):-!.
make_args_out(bach,[C],Vars,ArgsOut):-
	 flag(indent,X,X),indent_nbsp(X,PreOut),!,
	 toMarkupFormula(bach,C,Vars,Chars1),
	 sformat(ArgsOut,'~w~w',[PreOut,Chars1]).

make_args_out(bach,[C|GS],Vars,ArgsOut):-
	 toMarkupFormula(bach,C,Vars,Chars1),
	 make_args_out(bach,GS,Vars,Chars2),!,
	 flag(indent,X,X),indent_nbsp(X,PreOut),!,
	 sformat(ArgsOut,'~w~w,~w',[PreOut,Chars1,Chars2]).


make_args_out(L,[],Vars,''):-!.
make_args_out(L,[C],Vars,ArgsOut):-
                toMarkupFormula(L,C,Vars,ArgsOut).
make_args_out(L,[C|GS],Vars,ArgsOut):-
                toMarkupFormula(L,C,Vars,Chars1),
                make_args_out(L,GS,Vars,Chars2),!,
                sformat(ArgsOut,'~w ~w',[Chars1,Chars2]).


% ===============================================================
% toMarkup(-Markup,-Prolog,-PrologVarableList, +Output)
% example: toMarkup(html, explaination(('Military':996:subclass('IntransitiveRelation', 'BinaryRelation')^B)* ('Military':836:subclass('BinaryRelation', 'Relation')^C)*forall('IntransitiveRelation', forall(D, forall('Relation', holds(subclass, 'IntransitiveRelation', D)and holds(subclass, D, 'Relation')=>holds(subclass, 'IntransitiveRelation', 'Relation'))))*sfind(instance(subclass, 'PartialOrderingRelation'))*sfind(subclass('PartialOrderingRelation', 'TransitiveRelation'))* ('Military':2756:instance(on, 'IntransitiveRelation')^E)), ['X'=on|A],O),write_ln(O).
%
% Markup := [html,kif,pml,leml] (Expandable)
% Prolog := any prolog term
% PrologVaraibles list is the equal list as produced by read/3  [=(Name,Val)|...]
% Output is an CharicterAtom (the difference is this atom is not added the the symbol table)
% ===================================================================

	

% ===================================================================
% term_to_leml(-Prolog, +Output)
%
% arity 2 version (note html) is sufficient for printing values
% ===================================================================

% ===================================================================
% This File is the bootstrap for the Moo Infence engine one first calls "[belief_module]"
% So first is loads the proper files and then starts up the system
% There are no predicates defined in this file (it just uses other files' predicates)
% ===================================================================



% ===================================================================
% getPrologVars/4. 
% ===================================================================

getPrologVars(Term, Vars, Singletons, Multiples) :-
    ((getPrologVars(Term, VarList, []),
    close_lst(VarList),
    keysort(VarList, KeyList),
    split_key_lst(KeyList, Vars, Singletons, Multiples))).

getPrologVars(Term,  [Term - x|V], V) :-isVarProlog(Term),!.
getPrologVars(Term, V, V) :-not(compound(Term)),!.
getPrologVars(Term,  V0, V) :-
	isQualifiedAs(Term,Type,PrologVar),
	Type \= existential,!,
	(isVarProlog(PrologVar) -> V0=[PrologVar - x|V]; V0=V),!.
	
getPrologVars(Term, V0, V) :- 
	 functor(Term, F, N),
	 getPrologVars(1, N, Term, V0, V).
getPrologVars(I, N, Term, V0, V) :-
    (  (I > N) -> V=V0
    ;   arg(I, Term, Arg),
	getPrologVars(Arg, V0, V1),
	J is I + 1,
	getPrologVars(J, N, Term, V1, V)
    ).

% ===================================================================
% getPrologVars/4. 
% ===================================================================

getAllPrologVars(Term, Vars, Singletons, Multiples) :-
    ((getAllPrologVars(Term, VarList, []),
    close_lst(VarList),
    keysort(VarList, KeyList),
    split_key_lst(KeyList, Vars, Singletons, Multiples))).

getAllPrologVars(Term,  [Term - x|V], V) :-isVarProlog(Term),!.
getAllPrologVars(Term, V, V) :-not(compound(Term)),!.
getAllPrologVars(Term, V0, V) :- 
	 functor(Term, F, N),
	 getAllPrologVars(1, N, Term, V0, V).
getAllPrologVars(I, N, Term, V0, V) :-
    (  (I > N) -> V=V0
    ;   arg(I, Term, Arg),
	getAllPrologVars(Arg, V0, V1),
	J is I + 1,
	getAllPrologVars(J, N, Term, V1, V)
    ).

% ===================================================================
% getSlots/4. Returns no Existential Body Vars
% ===================================================================

getSlots(Term, Vars, Singletons, Multiples) :-
    ((getSlots(Term, VarList, []),
    close_lst(VarList),
    keysort(VarList, KeyList),
    split_key_lst(KeyList, Vars, Singletons, Multiples))).

getSlots(Term,  [Term - x|V], V) :-isVarProlog(Term),!.
getSlots(Term, V, V) :-not(compound(Term)),!.
getSlots(Term, V, V) :-isHiddenSlot(Term),!.
getSlots(Term,  VO, V) :-
	isQualifiedAs(Term,existential,EVar),!,
	getSlots(EVar,  VO, V).
getSlots(Term,  V0, V) :-
	isQualifiedAs(Term,Type,PrologVar),!,
	(isVarProlog(PrologVar) -> V0=[Term - x|V]; V0=V),!.
getSlots(Term, V0, V) :- 
	 functor(Term, F, N),
	 getSlots(1, N, Term, V0, V).
getSlots(I, N, Term, V0, V) :-
    (  (I > N) -> V=V0
    ;   arg(I, Term, Arg),
	getSlots(Arg, V0, V1),
	J is I + 1,
	getSlots(J, N, Term, V1, V)
    ).


% ===================================================================
% ===================================================================

rnd(N):-number(N),X is random(100),!,X<N.

% ===================================================================
% ===================================================================
sameString(X,Y):-same_str(X,Y).

same_str(X,X):-!.
same_str(S,Y):-to_codes(S,SS),to_codes(Y,YS),!,SS=YS.

close_str(S,Y):-to_codes(S,SS),to_codes(Y,YS),!,toLowercase(SS,LSS),toLowercase(YS,LYS),!,LSS=LYS.

% ===================================================================
% ===================================================================

to_codes(A,O):-atom(A),!,atom_codes(A,O).
to_codes(S,O):-string(S),!,string_to_atom(S,A),to_codes(A,O).
to_codes(C,C).

% ===================================================================
% getConstants/4. 
% ===================================================================

getConstants(Types,Term, Vars, Singletons, Multiples) :-
    ((getConstants(Types,Term, VarList, []),
    close_lst(VarList),
    keysort(VarList, KeyList),
    split_key_lst(KeyList, Vars, Singletons, Multiples))).

getConstants(Types,Term, [Term - x|V], V) :- getConstants(Types,Term),!.
getConstants(Types,Term, V, V) :- var(Term),!.
getConstants(Types,Term,  FOUND, V) :-
            Term=..[L,I|ST],
            getConstants(Types,L, VL, []),
            consts_l(Types,[I|ST], FLIST),
            html_append(V,FLIST,UND),
            html_append(VL,UND,FOUND),!.

getConstants(Types,Term, V, V) :- !.
    
consts_l(Types,[],[]).
consts_l(Types,[L|IST], FLIST):-
         getConstants(Types,L, FOUND,[]), 
         consts_l(Types,IST, FOUNDMore), !,
         html_append(FOUND,FOUNDMore,FLIST).

    
getConstants(_,('.')):-!,fail.
getConstants(_,'[]'):-!,fail.
getConstants(_,'$VAR'(_)):-!,fail.
getConstants(atomic,A):-atomic(A).
getConstants(atom,A):-atom(A).
getConstants(skolems,'zzskFn'(_)).
getConstants(funct,'AssignmentFn'(_,_)).



toMarkup(chat,Var,VS,Chars):-!,catch(toMarkup(bach,Var,VS,Chars),_,true),!.
toMarkup(java,Var,VS,Chars):-!,catch(toMarkup(html,Var,VS,Chars),_,true),!.

% VARIABLES
toMarkup(L,C,Vars,Chars):-isSlot(C),!,toMarkupSlot(L,C,Vars,Chars).

% COMPOUND
toMarkup(L,C,Vars,Chars):-compound(C),!,toMarkupFormula(L,C,Vars,Chars).


toMarkup(html,'$spacer',Vars,'\n<hr>\n').
toMarkup(_,'$spacer',Vars,'\n;; ------------------------------------------------------------------------------\n\n').

toMarkup(L,formula(C),Vars,Chars):-!,toMarkupFormula(L,C,Vars,Chars).

%Terminal Control
toMarkup(html,lparen,Vars,'('):-!.
toMarkup(html,rparen,Vars,')'):-!.
toMarkup(kif,lparen,Vars,'('):-!.
toMarkup(kif,rparen,Vars,')'):-!.
toMarkup(html,nl,Vars,'<br>'):-!.
toMarkup(html,tab,Vars,'<li>'):-!.
toMarkup(kif,nl,Vars,'\n'):-!.
toMarkup(kif,tab,Vars,'\t'):-!.

toMarkup(_,surf,Vars,''):-!.
toMarkup(_,end_of_file,Vars,''):-!.

toMarkup(_,',',Vars,'and'):-!.
toMarkup(_,';',Vars,'or'):-!.
toMarkup(_,'=',Vars,'equal'):-!.
toMarkup(_,'deduced',Vars,' ').


%LISTS
%toMarkup(LANG,[COMP],Vars,Atom)

toMarkup(L,[],Vars,Atom):-toMarkup(L,'NullSet',Vars,Atom).
%toMarkup(html,[Su|Bj],Vars,Chars):-toMarkupList(html,[Su|Bj],Vars,Chars1),sformat(Chars,'<div>(<ul>~w </ul>)</div>',[Chars1]).

close_varlst([]):-!.
close_varlst('$VAR'(_)):-!.
close_varlst([V|VV]):-close_varlst(VV),!.


toMarkup(html,option(Option),Vars,Chars):-sformat(Chars,'<option value="~w">~w</option>',[Option,Option]).

% Numbers
toMarkup(_,Atom,_VS,Chars):-float(Atom),!,sformat(Chars,'~f',[Atom]).
toMarkup(_,Atom,_VS,Chars):-number(Atom),!,sformat(Chars,'~w',[Atom]).

toMarkup(L,Value,Vars,Chars):-
        fail,mooCache(PredR, skolem, Value = x(Name,Expression),SKVARS,Context, Ctx, TN, Auth, State),!,
            toMarkup(kif,Name,Vars,NameQ),  prependQuestionMark(NameQ,NameQM),
            subst(x(Sk,Expression),Sk,NameQM,x(NSk,NExpression)),!,
            toMarkup(L,exists([NSk],NExpression),SKVARS,Chars).

% PRETTYNESS
toMarkup(_,';',Vars,'or ').
toMarkup(_,',',Vars,'and ').
toMarkup(_,'neg',Vars,'neg ').
%toMarkup(_,entails,Vars,'modus-ponens ').
%toMarkup(_,entails,Vars,'modus-tollens ').

%toMarkup(L,Char,Vars,Char):-L==kif.

toMarkup(html,Atom,Vars,Char):-!,
      my2_www_form_encode(Atom,E),
      sformat(Char,'<a href="browse.moo?find=~w">~w<a>',[E,Atom]).

toMarkup(bach,Atom,Vars,Chars):-atom(Atom),atom_codes(Atom,Codes),member(32,Codes),sformat(Chars,'~w',[Atom]).
%TODO toMarkup(bach,Atom,Vars,Chars):-atom(Atom),sformat(Chars,'"~w"',[Atom]).
toMarkup(bach,Atom,Vars,Chars):-atom(Atom),sformat(Chars,'~w',[Atom]).
      

toMarkup(_,Atom,Vars,Atom):-!.

my2_www_form_encode(H,E):-
	 sformat(S,'~q',[H]),
	 string_to_atom(S,A),
	 www_form_encode(A,E),!.


%TODO Number?

% ================================================
%      toMarkupList
% ================================================

toMarkupList(L,Var,VS,Chars):-isSlot(Var),!,toMarkupSlot(L,Var,VS,Chars).
toMarkupList(_,[],VS,''):-!.
toMarkupList(LANG,[H],VS,Chars):-!,
        toMarkup(LANG,H,VS,Chars).
toMarkupList(LANG,[H|T],VS,Chars):-!,
        toMarkup(LANG,H,VS,Chars1),
        toMarkupList(LANG,T,VS,Chars2),
        sformat(Chars,'~w ~w',[Chars1,Chars2]).

markUpVARLIST(L,[],Vars,''):-!.
markUpVARLIST(L,'$VAR'(_),Vars,''):-!.

markUpVARLIST(L,[VV|Varnames],Vars,Chars):-
                  VV=..[_,Name,Value],!,
                  toMarkupVarEquals(L,'$VAR'(Name),Value,Vars,Chars1),
                  markUpVARLIST(L,Varnames,Vars,Chars2),
                  sformat(Chars,'~w\n~w',[Chars1,Chars2]).

toMarkupVarEquals(_,Name,Value,Vars,Chars):-
            toMarkup(kif,Name,Vars,NameQ),
            toMarkup(L,Value,Vars,ValChars),
            sformat(Chars,'~w = ~w',[NameQ,ValChars]).

% Real Prolog Var
toMarkupSlot(L,Slot,VarList,Chars):- isVarProlog(Slot),!,
        toMarkup_makeNamePrologVar(L,VarList,Slot,Name),
        atom_concat('?',Name,Chars),!.
% Slot 'Typed'
toMarkupSlot(L,Slot,VarList,Chars):-isQualifiedAs(Slot,BaseType,Value,Subtype), !,
        toMarkup_makeName(L,VarList,Slot,Subtype,Value,Name),
        close_freeVars(VarList,NVarList),
        html_append(NVarList,[Name=Value],NV),
        toMarkup(L,Value,NV,VChars),
        sformat(Chars,'<div title="~w">~w</div>',[Subtype,VChars]).

toMarkup_makeNamePrologVar(L,VarList,Value,Name):-member(Name=Var,VarList),Var==Value,!.
toMarkup_makeNamePrologVar(L,VarList,Value,Name):-getVarAtom(Value,NUame),atom_concat('',NUame,Name).

getVarAtom(Value,Name):-var(Value),!,term_to_atom(Value,Vname),atom_codes(Vname,[95,_|CODES]),atom_codes(Name,CODES),!.
getVarAtom('$VAR'(VNUM),Name):-sformat(Name,'~w',['$VAR'(VNUM)]),!.
getVarAtom('$VAR'(VNUM),Name):-concat_atom([VNUM],Name),!.



toMarkup_makeName(L,VarList,Slot,BaseType,Value,Name):-
        member(Name=Var,VarList),Var==Slot,!.
toMarkup_makeName(L,VarList,Slot,BaseType,Value,Name):-
        member(Name=Var,VarList),Var==Value,!.
toMarkup_makeName(L,VarList,Slot,BaseType,Value,Name):-atom_concat('?',BaseType,Name).



close_freeVars(V,V):-proper_lst(V),!.
close_freeVars(V,[]):-isSlot(V),!. %Closing List if there are no free getPrologVars
close_freeVars([X|XX],[X|More]):- close_freeVars(XX,More).





toMarkup_varProlog(kif,Var,_VS,NameQ):- _VS=[VV|_],nonvar(VV),VV=..[_,Name,VarRef],number(Name),Var==VarRef,!,sformat(NameQ,'?~d',[Name]).
toMarkup_varProlog(kif,Var,_VS,NameQ):- _VS=[VV|_],nonvar(VV),VV=..[_,Name,VarRef],Var==VarRef,!,sformat(NameQ,'?~w',[Name]).

toMarkup_varProlog(html,Var,_VS,NameQ):- _VS=[VV|_],nonvar(VV),VV=..[_,Name,VarRef],number(Name),Var==VarRef,!,sformat(NameQ,'?~d',[Name]).
toMarkup_varProlog(html,Var,_VS,NameQ):- _VS=[VV|_],nonvar(VV),VV=..[_,Name,VarRef],Var==VarRef,!,sformat(NameQ,'?~w',[Name]).

toMarkup_varProlog(T,Var,[_|Rest],Name):-nonvar(Rest),toMarkup_varProlog(T,Var,Rest,Name).
toMarkup_varProlog(kif,VAR,_,VarName):-term_to_atom(VAR,AVAR),atom_codes(AVAR,[95|CODES]),!,catch(sformat(VarName,'?HYP-~s',[CODES]),_,VarName='?HYP-AVAR').
toMarkup_varProlog(kif,VAR,_,VarName):-term_to_atom(VAR,AVAR),atom_codes(AVAR,CODES),!,catch(sformat(VarName,'?HYP-~s',[CODES]),_,VarName='?HYP-AVAR').
toMarkup_varProlog(html,VAR,VS,VarName):-toMarkup_varProlog(kif,VAR,VS,VarName).

prependQuestionMark(Name,NameQ):-atom_concat('?',Name,NameQ).


% ===================================================
% Special Objects
% ===================================================
%toMarkupFormula(L,C,Vars,Chars):-       writeq( C=Vars),nl,fail.


toMarkupFormula(L,C,Vars,Chars):-isSlot(C),!,toMarkupSlot(L,C,Vars,Chars).
toMarkupFormula(L,C,Vars,Chars):-not(compound(C)),!,toMarkup(L,C,Vars,Chars).


% QUOTED STRING FORMAT
toMarkupFormula(L,string([]),Vars,'""'):-!.
toMarkupFormula(L,string(''),Vars,'""'):-!.
toMarkupFormula(L,string(Atom),Vars,Chars):-!,sformat(Chars,'"~w"',[Atom]).
toMarkupFormula(L,'$stringCodes'(Atom),Vars,Chars):-!,sformat(Chars,'"~w"',[Atom]).
toMarkupFormula(L,'$stringSplit'(List),Vars,Chars):-
      toMarkupList(L,List,Vars,Chars1),sformat(Chars,'"~w"',[Chars1]).
toMarkupFormula(L,Atom,_VS,Chars):-((isCharCodelst(Atom);string(Atom);is_string(Atom))),!,
%        catch(sformat(Chars,'"~s"',[Atom]),_,sformat(Chars,'"~w"',[Atom])).
        catch(sformat(Chars,'~s',[Atom]),_,sformat(Chars,'~w',[Atom])).

isCharCodelst([]).  isCharCodelst([A|T]):-integer(A),A>9,A<128,isCharCodelst(T).


toMarkupFormula(bach,float(Atom),_VS,Chars):-float(Atom) -> sformat(Chars,'~f',[Atom]);sformat(Chars,'~w',[Atom]).
toMarkupFormula(bach,int(Atom),_VS,Chars):-sformat(Chars,'~w',[Atom]).



toMarkupFormula(kif,'.'(Su,Bj),Vars,Chars):- 
      Bj \==[],
      atom(Su),not(is_list(Bj)),!,
      toMarkupFormula(kif,Su,Vars,Chars1),
      toMarkupFormula(kif,Bj,Vars,Chars2),!,
      sformat(Chars,'(~w . ~w)',[Chars1,Chars2]).


% SPECIAL FORMATS


toMarkupFormula(Form,saved(Data,Mt,Vars,Lits),_VS,Chars):-
      clause(saved(Data,Mt,Vars,Lits),true,ID),!,
      toMarkupFormula(Form,Data,Vars,Chars1),
      sformat(Chars,'<a href="editform.moo?id=~w" title="Edit EL ~w"><img border=0 src="http://www.cyc.com/cycdoc/img/cb/white.gif"></a>~w in ~w',[ID,ID,Chars1,Mt]),!.

toMarkupFormula(L,saved(Data,Mt,Vars,Lits),_,O):-!,toMarkupFormula(L,Data,_,O).

toMarkupFormula(L,':-'(C,true),Vars,Chars):-toMarkup(L,C,Vars,Chars).
toMarkupFormula(L,(T^V),Vars,Chars):-var_merge(Vars,V,TVars),!,toMarkup(L,T,TVars,Chars).

% No parens (nv = no vector)
toMarkupFormula(L,nv(Subj),Vars,Chars):-is_list(Subj),!,toMarkupList(L,Subj,Vars,Chars).
toMarkupFormula(L,nv(Subj),Vars,Chars):-!,toMarkup(L,Subj,Vars,Chars).

toMarkupFormula(_,writeFmt(F,A),Vars,Chars):-sformat(Chars,F,A),!.

toMarkupFormula(kif,[Su|Bj],Vars,Chars):-toMarkupList(kif,[Su|Bj],Vars,Chars1),sformat(Chars,'(~w)',[Chars1]).

toMarkupFormula(_,writeq(Term),Vars,Atom):-!,sformat(Atom,'~q',[Term]).
toMarkupFormula(kif,maillink(Title,Address,Subject),Vars,Address):-!.
toMarkupFormula(kif,weblink(Title,URL),Vars,Title):-!.
toMarkupFormula(kif,helplink(Title,URL),Vars,Title):-!.
toMarkupFormula(L,explaination(PB),Vars,Atom):-
        flag(explaination_linenumber,_,1),
        toMarkupFormula(L,PB,Vars,AtomS),!,
        sformat(Atom,'\nExplaination:\n~w\n',[AtomS]).

toMarkupFormula(LANG,krlog(COMP),Vars,Atom):-!,prolog_to_krlog(COMP,KR),toMarkup(LANG,KR,Vars,Atom).

toMarkupFormula(LANG,kif(COMP),Vars,Atom):-!,toMarkup(kif,COMP,Vars,Atom).
toMarkupFormula(LANG,html(COMP),Vars,Atom):-!,toMarkup(html,COMP,Vars,Atom).

toMarkupFormula(html,select(Name,OptionList),Vars,Chars):-toMarkup(html,options(OptionList),Vars,Options),sformat(Chars,'<select sort name="~w" id="~w" size="1">~w</select>',[Name,Name,Options]).
toMarkupFormula(html,chectheoryox(Name,on),Vars,Chars):-
                sformat(Chars,'<input type=chectheoryox name="~w" id="~w" checked>',[Name,Name]),!.
toMarkupFormula(html,chectheoryox(Name,_),Vars,Chars):-
                sformat(Chars,'<input type=chectheoryox name="~w" id="~w">',[Name,Name]),!.
toMarkupFormula(html,options([]),Vars,'').

toMarkupFormula(L,getPrologVars(Form),Vars,Chars):-markUpVARLIST(L,Form,Vars,SChars),sformat(Chars,'~w',[SChars]),!.

toMarkupFormula(L,getPrologVars(Form),Vars,Chars):-!,sformat(Chars,'; var_post_err (~q). ',[Form]).


toMarkupFormula(html,qresult(Res),Vars,Chars):-!,sformat(Chars,'Result ',[Res]).

toMarkupFormula(kif,qresult(Res),Vars,''):-!. %,sformat(Chars,'res="~w"\n',[Res]).

% Back into Standard Terms

format_o(Format,Stuff):-
        toMarkup(html,Stuff,_,Chars),writeFmt(Format,[Chars]).


toMarkupFormula(html,options([Option|List]),Vars,Chars):-
               toMarkup(html,option(Option),Vars,Chars2),
               toMarkup(html,options(List),Vars,Out3),
               atom_concat(Chars2,Out3,Chars).

toMarkupFormula(Form,entails(A,C),Vars,Chars):-!,
      toMarkupFormula(Agent,'#$codeEntailment'(C,A),Vars,Chars1),
      sformat(Chars,'<img src="http://www.cyc.com/cycdoc/img/cb/purple.gif">~w in CODE',[Chars1]),!.


toMarkupFormula(L,hidden(F,Args),Vars,''):-!.

toMarkupFormula(L,'$nartFixed'(C),Vars,O):-!,toMarkupFormula(L,(C),Vars,O),!.

toMarkupFormula(html,colourize(Color,Thing),Vars,Chars):-!,
        toMarkupFormula(html,Thing,Vars,Chars1),!,
        sformat(Chars,'<font color="~w">~w</font>\n',[Color,Chars1]).

toMarkupFormula(L,colourize(Color,Thing),Vars,Chars):-!,
        toMarkupFormula(L,Thing,Vars,Chars),!.

/*
toMarkupFormula(L,','(A,B),Vars,Chars):-!,
        prolog_to_krlog(','(A,B),KR),
        toMarkupFormula(L,KR,Vars,Chars),!.
*/


toMarkupFormula(L,write_dollar('$v',[A|Args]),Vars,Chars):-!,
                Flag=..[getPrologVars,A|Args],!,
                toMarkupFormula(L,Flag,Vars,Chars).

toMarkupFormula(L,table_(Goal,Lits),Vars,Chars):-!,
                toMarkupFormula(L,table_p(Lits,Goal),Vars,Chars).


toMarkupFormula(L,write_dollar(F,[A|Args]),Vars,Chars):-!,
        toMarkupFormula(L,A,Vars,Chars1),
        toMarkupFormula(L,hidden(F,Args),Vars,Chars2),!,
        sformat(Chars,'~w~w',[Chars1,Chars2]).

toMarkupFormula(L,'$existential'(VarName,Name,Literal),Vars,O):-!,
        toMarkupFormula(L,'existential'(VarName),Vars,O).

toMarkupFormula(L,'$eval'(Function),Vars,O):-!,
        toMarkupFormula(L,' eval'(Function),Vars,O).


toMarkupFormula(L,functional(VarName,Domains,Literal),Vars,O):-
        toMarkup(L,Literal,Vars,O),!.

close_lst_var(M,[]):-isSlot(M),!.
close_lst_var([[M]|Ms],[M|Ls]):-!,
        close_lst_var(Ms,Ls).
close_lst_var([M|Ms],[M|Ls]):-!,
        close_lst_var(Ms,Ls).


      

toMarkupFormula(L,unused(C,P),Vars,O):-!,
        toMarkupFormula(L,notused(C,writeq(P)),Vars,O).

toMarkupFormula(L,ff([]),Vars,'[]'):-!.

toMarkupFormula(L,ff([Flag|Flags]),Vars,Chars):-!,
        toMarkupFormula(L,flag(Flag),Vars,Chars1),
        toMarkupFormula(L,ff(Flags),Vars,Chars2),
        sformat(Chars,'~w, ~w',[Chars1, Chars2]).

toMarkupFormula(L,domargs([]),Vars,''):-!.

toMarkupFormula(L,domargs([(P:N)]),Vars,Chars):-!,
        toMarkupFormula(L,P,Vars,Chars1),
        sformat(Chars,'~w:~w',[Chars1,N]).

toMarkupFormula(L,domargs([(P:N)|Flags]),Vars,Chars):-!,
        toMarkupFormula(L,P,Vars,Chars1),
        toMarkupFormula(L,domargs(Flags),Vars,Chars2),
        sformat(Chars,'~s:~w,~w',[Chars1,N,Chars2]).

toMarkupFormula(L,flag(Flag),Vars,Chars):-
        Flag=..[domainV,Var,DomArgs],!,
        toMarkupFormula(L,Var,Vars,VarChars),
        toMarkupFormula(L,domargs(DomArgs),Vars,ArgChars),
        sformat(Chars,'~w(~w,[~w])',[domainV,VarChars,ArgChars]).

toMarkupFormula(L,flag(Flag),Vars,Chars):-
        Flag=..[Name,Var,Args],!,
        toMarkupFormula(L,Var,Vars,VarChars),
        sformat(Chars,'~w(~w, ~q)',[Name,VarChars,Args]).
toMarkupFormula(L,flag(Flag),Vars,Chars):-!,
        toMarkupFormula(L,writeq(Flag),Vars,Chars).


toMarkupFormula(L,writeq(Atom),_VS,Chars):-!,sformat(Chars,'~q',[Atom]).

toMarkupFormula(L,[],Vars,''):-!.

toMarkupFormula(bach,':'(Bj,list([])),Vars,Chars):-!,sformat(Chars,'~w:[]',[Bj]).
toMarkupFormula(bach,':'(Bj,map([])),Vars,Chars):-!,sformat(Chars,'~w:{}',[Bj]).
toMarkupFormula(bach,':'(Bj,More),Vars,Chars):-!,
	 toMarkupFormula(bach,More,Vars,Chars2),
        sformat(Chars,'~w:~w',[Bj,Chars2]).

toMarkupFormula(bach,[Bj],Vars,Chars):-!,toMarkupFormula(bach,Bj,Vars,Chars).
toMarkupFormula(bach,[Bj|More],Vars,Chars):-!,
	 toMarkupFormula(bach,Bj,Vars,Chars1),
	 toMarkupFormula(bach,More,Vars,Chars2),
        sformat(Chars,'~w,~w',[Chars1,Chars2]).

%toMarkupFormula(L,[A | B],Vars,Chars):-proper_lst([A | B]),html_append(['('|[A | B],[')'],TRY),toMarkupList(L,[Su|Bj],Vars,Chars).
%toMarkupFormula(L,[A | B],Vars,Chars):-catch(TRY=..['',A | B],_,fail),toMarkupFormula(L,TRY,Varsr,Chars),!.
%toMarkupFormula(L,[A | B],Vars,Chars):-catch(TRY=..[A | B],_,fail),toMarkupFormula(L,TRY,Vars,Chars),!.
%toMarkupFormula(L,[A | B],Vars,Chars):-catch(TRY=..[A | B],_,fail),toMarkupFormula(L,TRY,Vars,Chars),!.
toMarkupFormula(L,[Su|Bj],Vars,Chars):-
        toMarkupList(L,[Su|Bj],Vars,Chars1),
        sformat(Chars,'(~w)',[Chars1]).



/*
toMarkupFormula(L,Term,Vars,O):-
        Term=..[holds,F|Args],isNonVar(F),not_a_function(F),!,
        NTerm=..[F|Args],
        toMarkupFormula(L,NTerm,Vars,O).
*/
toMarkupFormula(L,'$VAR'(_)* X ,Vars,Chars):-!,toMarkupFormula(L, X ,Vars,Chars).
toMarkupFormula(L, X * '$VAR'(_) ,Vars,Chars):-!,toMarkupFormula(L, X ,Vars,Chars).
toMarkupFormula(L,(A * []),Vars,Chars):-!,toMarkupFormula(L,A ,Vars,Chars).
toMarkupFormula(L,([] * A),Vars,Chars):-!,toMarkupFormula(L,A ,Vars,Chars).
toMarkupFormula(L,deduced* X ,Vars,Chars):-!,toMarkupFormula(L, X ,Vars,Chars).
toMarkupFormula(L, X * deduced ,Vars,Chars):-!,toMarkupFormula(L, X ,Vars,Chars).


toMarkupFormula(L,domainV(Var,ReqsL),Vars,Chars):-
        toMarkupFormula(L,' domainV'(Var,writeq(ReqsL)),Vars,Chars).
toMarkupFormula(L,domainC(Var,ReqsL),Vars,Chars):-
        toMarkupFormula(L,' domainC'(Var,writeq(ReqsL)),Vars,Chars).
toMarkupFormula(L,domainA(Var,ReqsL),Vars,Chars):-
        toMarkupFormula(L,' domainA'(Var,writeq(ReqsL)),Vars,Chars).
toMarkupFormula(L,existsC(Var,ReqsL),Vars,Chars):-
        toMarkupFormula(L,' existsC'(Var,writeq(ReqsL)),Vars,Chars).
toMarkupFormula(L,existsA(Var,ReqsL),Vars,Chars):-
        toMarkupFormula(L,' existsA'(Var,writeq(ReqsL)),Vars,Chars).

toMarkupFormula(L,(A * B),Vars,Chars):-!,
        toMarkupFormula(L,B,Vars,Chars2),
        toMarkupFormula(L,A,Vars,Chars1),
        sformat(Chars,'~w\n~w',[Chars2, Chars1]).

toMarkupFormula(L,formula(C),Vars,Chars):-!,
        toMarkupFormula(L,C,Vars,Chars).


toMarkupFormula(html,undefined_constants(UnDefinedList),_,O):-
        toMarkupFormula(kif,nv(UnDefinedList),_,I),
        sformat(O,'\n<font color=red>Warning Undefined constants: <font color=black size=+1>~w</font></font>',[I]).

toMarkupFormula(kif,undefined_constants(UnDefinedList),_,O):-
        toMarkupFormula(kif,(UnDefinedList),_,I),
        sformat(O,'\nWarning Undefined constants ~w',[I]).



toMarkupFormula(L,C,Vars,Chars):-is_list(C),!,make_args_out(L,C,Vars,Chars1),sformat(Chars,'(~w)',[Chars1]).

% ==================================================
% Unest And/Or
% ==================================================

toMarkupFormula(L,and(and(and(and(and(F,E),D),C),B),A),VS,Chars):-!, toMarkupFormula(L,and(F,E,D,C,B,A),VS,Chars).
toMarkupFormula(L,and(and(and(and(E,D),C),B),A),VS,Chars):-!, toMarkupFormula(L,and(E,D,C,B,A),VS,Chars).
toMarkupFormula(L,and(and(and(D,C),B),A),VS,Chars):-!, toMarkupFormula(L,and(D,C,B,A),VS,Chars).
toMarkupFormula(L,and(and(B,C),A),VS,Chars):-!, toMarkupFormula(L,and(C,B,A),VS,Chars).
toMarkupFormula(L,and(A,and(B,and(C,and(D,and(E,F))))),VS,Chars):-!, toMarkupFormula(L,'and'(A,B,C,D,E,F),VS,Chars).
toMarkupFormula(L,and(A,and(B,and(C,and(D,E)))),VS,Chars):-!, toMarkupFormula(L,'and'(A,B,C,D,E),VS,Chars).
toMarkupFormula(L,and(A,and(B,and(C,D))),VS,Chars):-!, toMarkupFormula(L,'and'(A,B,C,D),VS,Chars).
toMarkupFormula(L,and(A,and(B,C)),VS,Chars):-!, toMarkupFormula(L,'and'(A,B,C),VS,Chars).
toMarkupFormula(L,or(or(or(or(D,E),D),B),A),VS,Chars):-!, toMarkupFormula(L,or(E,D,C,B,A),VS,Chars).
toMarkupFormula(L,or(or(or(C,D),B),A),VS,Chars):-!, toMarkupFormula(L,or(D,C,B,A),VS,Chars).
toMarkupFormula(L,or(or(B,C),A),VS,Chars):-!, toMarkupFormula(L,or(C,B,A),VS,Chars).
toMarkupFormula(L,or(A,or(B,or(C,or(D,E)))),VS,Chars):-!, toMarkupFormula(L,'or'(A,B,C,D,E),VS,Chars).
toMarkupFormula(L,or(A,or(B,or(C,D))),VS,Chars):-!, toMarkupFormula(L,'or'(A,B,C,D),VS,Chars).
toMarkupFormula(L,or(A,or(B,C)),VS,Chars):-!, toMarkupFormula(L,'or'(A,B,C),VS,Chars).

% ==================================================
% Mark terms as implemented in code
% ==================================================

toMarkupFormula(html,incode(X),Vars,HAtom):-!,
        toMarkupFormula(L,bullet(X),Vars,Atom),
        sformat(HAtom,'<table border=0><tr><td><pre>~w</pre></td><td><pre>Implemented in code.</pre></td></tr></table>',[Atom]).

toMarkupFormula(kif,incode(X),Vars,HAtom):-!,
        toMarkupFormula(L,bullet(X),Vars,Atom),
        sformat(HAtom,'~w\nImplemented in code.\n',[Atom]).


toMarkupFormula(html,incode(X,M),Vars,HAtom):-!,
        toMarkupFormula(L,bullet(X),Vars,Atom),
        sformat(HAtom,'<table border=0><tr><td><pre>~w</pre></td><td><pre>Implemented in code.\n~w</pre></td></tr></table>',[Atom,M]).

toMarkupFormula(kif,incode(X,M),Vars,HAtom):-!,
        toMarkupFormula(L,bullet(X),Vars,Atom),
        sformat(HAtom,'~w\nImplemented in code.\n (~w)\n',[Atom,M]).




toMarkupFormula(L,cfind(entails(Pre,Post)),Vars,Chars):-
        fail,mooCache(PredR,Post,Pre,T,true,Context,Explaination),
        toMarkupFormula(L,Explaination,Vars,Chars),!.

% ==================================================
% Show explaination of cross reference optimization
% ==================================================
toMarkupFormula(L,g_h(_),Vars,''):-!.
toMarkupFormula(L,tid(_),Vars,''):-!.

toMarkupFormula(L,crossref(X,Y),Vars,Atom):-!,
        crossref_to_explaination(crossref(X,Y),P),
        toMarkupFormula(L,P,Vars,Atom).

toMarkupFormula(L,crossref(X),Vars,Atom):-!,
        crossref_to_explaination(crossref(X),P),
        toMarkupFormula(L,P,Vars,Atom).


% ==========================
% Slolem  rewriting
% ==========================

toMarkupFormula(L,(X),Vars,Chars):- nonvar(X),X=..['E',Sk|ArgS],!,
        Y=..[Sk|ArgS],!,
        toMarkupFormula(L,Y,Vars,Chars).

% =====================
% remove_nonvars
% =====================

remove_nonvars(V,V):-isSlot(V),!.
remove_nonvars([],[]):-!.
remove_nonvars([V|L],LL):-isNonVar(V),!,remove_nonvars(L,LL).
remove_nonvars([V|L],[V|LL]):-remove_nonvars(L,LL).



% =====================
% Forall
% =====================

toMarkupFormula(L,forall(V,F),Vars,Chars):-not(is_list(V)),!,
        group_forall(forall(V,F),Next),!,
        cleanQuantifierConversionForWrite_forall(Next,O),
        toMarkupFormula(L,O,Vars,Chars).

cleanQuantifierConversionForWrite_forall(forall(VL,F),O):-
        remove_nonvars(VL,NL),
        ((NL=[],!,O=F);(!,O=forall(NL,F))).

toMarkupFormula(L,forall(V,F),Vars,Chars):- not(is_list(V)),!,
        toMarkupFormula(L,forall([V],F),Vars,Chars).

group_forall(forall(V1,forall(V2,forall(V3,forall(V4,forall(V5,F))))),forall([V1,V2,V3,V4,V5],F)):-!.
group_forall(forall(V1,forall(V2,forall(V3,forall(V4,F)))),forall([V1,V2,V3,V4],F)):-!.
group_forall(forall(V1,forall(V2,forall(V3,F))),forall([V1,V2,V3],F)):-!.
group_forall(forall(V1,forall(V2,F)),forall([V1,V2],F)):-!.
group_forall(forall(V1,F),forall([V1],F)):-!.

% =====================
% Exists
% =====================



toMarkupFormula(L,exists(V,F),Vars,Chars):-not(is_list(V)),!,
        group_exists(exists(V,F),Next),!,
        cleanQuantifierConversionForWrite_exists(Next,O),
        toMarkupFormula(L,O,Vars,Chars).

cleanQuantifierConversionForWrite_exists(exists(VL,F),O):-
        remove_nonvars(VL,NL),
        ((NL=[],!,O=F);(!,O=exists(NL,F))).

toMarkupFormula(L,exists(V,F),Vars,Chars):- not(is_list(V)),!,
        toMarkupFormula(L,exists([V],F),Vars,Chars).

group_exists(exists(V1,exists(V2,exists(V3,exists(V4,exists(V5,F))))),exists([V1,V2,V3,V4,V5],F)):-!.
group_exists(exists(V1,exists(V2,exists(V3,exists(V4,F)))),exists([V1,V2,V3,V4],F)):-!.
group_exists(exists(V1,exists(V2,exists(V3,F))),exists([V1,V2,V3],F)):-!.
group_exists(exists(V1,exists(V2,F)),exists([V1,V2],F)):-!.
group_exists(exists(V1,F),exists([V1],F)):-!.
% =====================
% Exists
% =====================

toMarkupFormula(L,exists(V,F),Vars,Chars):-not(is_list(V)),!,
        group_exists(exists(V,F),Next),!,
        cleanQuantifierConversionForWrite_exists(Next,O),
        toMarkupFormula(L,O,Vars,Chars).

cleanQuantifierConversionForWrite_exists(exists(VL,F),O):-
        remove_nonvars(VL,NL),
        ((NL=[],!,O=F);(!,O=exists(NL,F))).

toMarkupFormula(L,exists(V,F),Vars,Chars):- not(is_list(V)),!,
        toMarkupFormula(L,exists([V],F),Vars,Chars).

group_exists(exists(V1,exists(V2,exists(V3,exists(V4,exists(V5,F))))),exists([V1,V2,V3,V4,V5],F)):-!.
group_exists(exists(V1,exists(V2,exists(V3,exists(V4,F)))),exists([V1,V2,V3,V4],F)):-!.
group_exists(exists(V1,exists(V2,exists(V3,F))),exists([V1,V2,V3],F)):-!.
group_exists(exists(V1,exists(V2,F)),exists([V1,V2],F)):-!.
group_exists(exists(V1,F),exists([V1],F)):-!.

% ==================================================
% Finds the clausification then displays the explaination
% ==================================================
toMarkupFormula(L,map(ARGS),Vars,Chars):-
                flag(indent,X,X+1),
                indent_nbsp(X,PreOut),!,
                make_args_out(L,ARGS,Vars,ArgsOut),!,
                sformat(Chars,'~w{~w}',[PreOut,ArgsOut]), !,
                flag(indent,NX,NX-1).

toMarkupFormula(L,list(ARGS),Vars,Chars):-
                flag(indent,X,X+1),
                indent_nbsp(X,PreOut),!,
                make_args_out(L,ARGS,Vars,ArgsOut),!,
                sformat(Chars,'~w[~w]',[PreOut,ArgsOut]), !,
                flag(indent,NX,NX-1).














%   File   : CLAUSE
%   Author : R.A.O'Keefe
%   Updated: 10 March 1984
%   Purpose: Convert a formula in FOPC to clausal form.
%   Needs  : ord_union/3 from UTIL:ORDSET.PL.

/*----------------------------------------------------------------------

    This module has three entry points:
	clausal_form(Formula, Clauses)
	clausal_form_of_negation(Formula, Clauses)
	units_separated(Clauses, PosUnits, NegUnits, NonUnits)

    The Formula is an <expr>, where
	<expr> ::= all(<variable>, <expr>)
		|  some(<variable>, <expr>)
		|  <expr> => <expr>
		|  <expr> <=> <expr>
		|  if(<expr>,<expr>,<expr>)
		|  <expr> and <expr>
		|  <expr> or <expr>
		|  ~ <expr>
		|  <atom>

	<atom> ::= <predicate>(<term>,...,<term>)

	<term> ::= <variable>
		|  <constant>
		|  <functor>(<term>,...,<term>)

    The Clauses are a sentence, where
	<sentence> ::= []			(true)
		|  <clause> . <sentence>	(and)

	<clause> ::= clause(<atoms>, <atoms>)
	<atoms> ::= [] | <atom> . <atoms>

    Note that this representation of a clause is not quite the
    usual one.  clause([a,b,c], [d,e,f]) represents
	a v b v c <- d & e & f
    or, if you don't like "Kowalski form",
	a v b v c v ~d v ~e v ~f

    The reason for the two entry points is that the formula may
    contain free variables, these are to be understood as being
    universally quantified, and the negation of the universal
    closure of a formula is not at all the same thing as the
    universal closure of the negation!

    units_separated takes a list of clauses such as the other two predicates
    might produce, and separates them into a list of positive unit clauses
    (represented just by <atom>s), a list of negative unit clauses (also
    represented by their single <atom>s), and a list of non-unit clauses.
    Some theorem provers might find this separation advantageous, but it is
    not buillt into clausal_form becauses some provers would not benefit.

----------------------------------------------------------------------*/
/*
:- public
	clausal_form/2,
	clausal_form_of_negation/2,
	units_seaparated/4.

:- mode
	clausal_form(+, -),
	clausal_form_of_negation(+, -),
	    pass_one(+, -),
		pass_one(+, +, -),
		pass_one(+, -, +, +, -),
		term_one(+, +, +, -),
		    term_one(+, +, +, +, -),
	    pass_two(+, -),
		pass_two_pos(+, -),
		pass_two_pos(+, -, +, +),
		    term_two(+, -, +),
			term_var(+, +, -),
			term_two(+, +, +, +),
		pass_two_neg(+, -, +, +),
		    sent_and(+, +, -),
		    sent_or(+, +, -),
	units_separated(+, -, -, -),
	containsCF(+, ?),
	literally_contains(+, +),
	does_not_literally_contain(+, ?).
*/

:- op(700, xfx, [containsCF,literally_contains,does_not_literally_contain]).
:- op(910,  fy, ~).
:- op(920, xfy, and).
:- op(930, xfy, or).
:- op(940, xfx, [=>, <=>]).


units_separated([], [], [], []).
units_separated([clause([],[Neg])|Clauses], PosL, [Neg|NegL], NonL) :- !,
	units_separated(Clauses, PosL, NegL, NonL).
units_separated([clause([Pos],[])|Clauses], [Pos|PosL], NegL, NonL) :- !,
	units_separated(Clauses, PosL, NegL, NonL).
units_separated([Clause|Clauses], PosL, NegL, [Clause|NonL]) :-
	units_separated(Clauses, PosL, NegL, NonL).


clausal_form(Formula, Clauses) :-
	pass_one(Formula, ClosedAndImplicationFree),
	pass_two(ClosedAndImplicationFree, Clauses).


clausal_form_of_negation(Formula, Clauses) :-
	pass_one(Formula, ClosedAndImplicationFree),
	pass_two(~ClosedAndImplicationFree, Clauses).


/*----------------------------------------------------------------------

    The first pass over the formula does two things.
    1a. It locates the free variables of the formula.
    2.  It applies the rules
	    A => B	--> B v ~A
	    A <=> B	--> (B v ~A) /\ (A v ~B)
	    if(A,B,C)	--> (B v ~A) /\ (A v C)
	to eliminate implications.  Even in a non-clausal
	theorem prover this can be a good idea, eliminating
	<=> and if is essential if each subformula is to
	have a definite parity, and that in turn is vital
	if we are going to replace existential quantifiers
	by Skolem functions.
    1b. It adds explicit quantifiers for the free variables.
    The predicate which does all this is pass_one/5:
	pass_one(+Formula,		% The original formula
		 -Translation,		% its implication-free equivalent
		 +Bound,		% The binding environment
		 +Free0,		% The variables known to be free
		 -Free)			% Free0 union Formula's free variables
    The binding environment just tells us which variables occur in quantifiers
    dominating this subformula, it doesn't matter yet whether they're
    universal or existential.

    The translated formula is still an <expr>, although there are practical
    advantages to be gained by adopting a slightly different representation,
    but the neatness of being able to say that
	pass_one(F, G) --> pass_one(G, G)
    outweighs them.

----------------------------------------------------------------------*/

pass_one(Formula, ClosedAndImplicationFree) :-
	pass_one(Formula, ImplicationFree, [], [], FreeVariables),
	pass_one(FreeVariables, ImplicationFree, ClosedAndImplicationFree).


pass_one([], Formula, Formula).
pass_one([Var|Vars], Formula, all(Var,Closure)) :-
	pass_one(Vars, Formula, Closure).


pass_one(all(Var,B), all(Var,D), Bound, Free0, Free) :- !,
	pass_one(B, D, [Var|Bound], Free0, Free).
pass_one(some(Var,B), some(Var,D), Bound, Free0, Free) :- !,
	pass_one(B, D, [Var|Bound], Free0, Free).
pass_one(A and B, C and D, Bound, Free0, Free) :- !,
	pass_one(A, C, Bound, Free0, Free1),
	pass_one(B, D, Bound, Free1, Free).
pass_one(A or B, C or D, Bound, Free0, Free) :- !,
	pass_one(A, C, Bound, Free0, Free1),
	pass_one(B, D, Bound, Free1, Free).
pass_one(A => B, D or ~C, Bound, Free0, Free) :- !,
	pass_one(A, C, Bound, Free0, Free1),
	pass_one(B, D, Bound, Free1, Free).
pass_one(A <=> B, (D or ~C) and (C or ~D), Bound, Free0, Free) :- !,
	pass_one(A, C, Bound, Free0, Free1),
	pass_one(B, D, Bound, Free1, Free).
pass_one(if(T,A,B), (C or ~U) and (D or U), Bound, Free0, Free) :- !,
	pass_one(T, U, Bound, Free0, Free1),
	pass_one(A, C, Bound, Free1, Free2),
	pass_one(B, D, Bound, Free2, Free).
pass_one(~A, ~C, Bound, Free0, Free) :- !,
	pass_one(A, C, Bound, Free0, Free).
pass_one(Atom, Atom, Bound, Free0, Free) :-
	%   An Atom is "anything else".  If Atoms were explicitly flagged,
	%   say by being written as +Atom, we wouldn't need those wretched
	%   cuts all over the place.  The same is true of pass_two.
	term_one(Atom, Bound, Free0, Free).


%   term_one/4 scans a term which occurs in a context where some
%   variables are Bound by quantifiers and some free variables (Free0)
%   have already been discovered.  Free is returned as the union of the
%   free variables in this term with Free0.  Note that though we call
%   does_not_literally_contain twice, it is doing two different things.
%   The first call determines that the variable is free.  The second
%   call is part of adding an element to a set, which could perhaps have
%   been a binary tree or some other data structure.

term_one(Term, Bound, Free0, Free) :-
	nonvar(Term),
	functor(Term, _, Arity),
	!,
	term_one(Arity, Term, Bound, Free0, Free).
term_one(Var, Bound, Free0, [Var|Free0]) :-
	Bound does_not_literally_contain Var,
	Free0 does_not_literally_contain Var,
	!.
term_one(_, _, Free0, Free0).

term_one(0, _, _, Free0, Free0) :- !.
term_one(N, Term, Bound, Free0, Free) :-
	arg(N, Term, Arg),
	term_one(Arg, Bound, Free0, Free1),
	M is N-1, !,
	term_one(M, Term, Bound, Free1, Free).


/*----------------------------------------------------------------------

    pass_two does the following in one grand sweep:
    1.  The original formula might have used the same variable in any
	number of quantifiers.  In the output, each quantifier gets a
	different variable.
    2.  But existentally quantified variables are replaced by new Skolem
	functions, not by new variables.  As a result, we can simply drop
	all the quantifiers, every remaining variable is universally
	quantified.
    3.  The rules
	~ all(V, F)	--> some(V, ~F)
	~ some(V, F)	--> all(V, ~F)
	~ (A and B)	--> ~A or ~B
	~ (A or B)	--> ~A and ~B
	~ ~ A		--> A
	are applied to move negations down in front of atoms.
    4.  The rules
	A or A		--> A
	A or ~A		--> true
	A or true	--> true
	A or false	--> A
	(A or B) or C	--> A or (B or C)
	(A and B) or C	--> (A or C) and (B or C)
	A or (B and C)	--> (A or B) and (A or C)
	A and true	--> A
	A and false	--> false
	(A and B) and C	--> A and (B and C)
	are applied to the clauses which we build as we work our
	way back up the tree.  The rules
	A and A		--> A
	A and ~A	--> false
	A and (~A or B)	--> A and B
	are NOT applied.  This is best done, if at all, after all the
	clauses have been generated.  The last two rules are special
	cases of resolution, so it is doubtful whether it is worth
	doing them at all.

    The main predicate is pass_two_pos/4:
	pass_two_pos(+Formula,		% The formula to translate
		     -Translation,	% its translation
		     +Univ,		% universal quantifiers in scope
		     +Rename)		% how to rename variables
    Rename is var | var(Old,New,Rename), where Old is a source variable,
    and New is either a new variable (for universal quantifiers) or a
    Skolem function applied to the preceding new variables (for existential
    quantifiers).  Univ is those New elements of the Rename argument which
    are variables.  pass_two_neg produces the translation of its Formula's
    *negation*, this saves building the negation and then handling it.

----------------------------------------------------------------------*/

pass_two(ClosedAndImplicationFree, ClausalForm) :-
	pass_two_pos(ClosedAndImplicationFree, PreClausalForm, [], var),
	pass_two_pos(PreClausalForm, ClausalForm).


%   pass_two_pos/2 does two things.  First, if there was only one clause,
%   pass_two_pos/4 wouldn't have wrapped it up in a list.  This we do here.
%   Second, if one of the clauses is "false", we return that as the only
%   clause.  This would be the place to apply A & A --> A.

pass_two_pos(clause(P,N), [clause(P,N)]) :- !.
pass_two_pos(Sentence, [clause([],[])]) :-
	Sentence containsCF clause([],[]),
	!.
pass_two_pos(Sentence, Sentence).


pass_two_pos(all(Var,B), Translation, Univ, Rename) :- !,
	pass_two_pos(B, Translation, [New|Univ], var(Var,New,Rename)).
pass_two_pos(some(Var,B), Translation, Univ, Rename) :- !,
	gensym('f-', SkolemFunction),
	SkolemTerm =.. [SkolemFunction|Univ],
	pass_two_pos(B, Translation, Univ, var(Var,SkolemTerm,Rename)).
pass_two_pos(A and B, Translation, Univ, Rename) :- !,
	pass_two_pos(A, C, Univ, Rename),
	pass_two_pos(B, D, Univ, Rename),
	sent_and(C, D, Translation).
pass_two_pos(A or B, Translation, Univ, Rename) :- !,
	pass_two_pos(A, C, Univ, Rename),
	pass_two_pos(B, D, Univ, Rename),
	sent_or(C, D, Translation).
pass_two_pos(~A, Translation, Univ, Rename) :- !,
	pass_two_neg(A, Translation, Univ, Rename).
pass_two_pos(true, [], _, _) :- !.
pass_two_pos(false, clause([],[]), _, _) :- !.
pass_two_pos(Atom, clause([Renamed],[]), _, Rename) :-
	%   An Atom is "anything else", hence the cuts above.
	term_two(Atom, Renamed, Rename).


pass_two_neg(all(Var,B), Translation, Univ, Rename) :- !,
	gensym('g-', SkolemFunction),
	SkolemTerm =.. [SkolemFunction|Univ],
	pass_two_neg(B, Translation, Univ, var(Var,SkolemTerm,Rename)).
pass_two_neg(some(Var,B), Translation, Univ, Rename) :- !,
	pass_two_neg(B, Translation, [New|Univ], var(Var,New,Rename)).
pass_two_neg(A and B, Translation, Univ, Rename) :- !,
	pass_two_neg(A, C, Univ, Rename),
	pass_two_neg(B, D, Univ, Rename),
	sent_or(C, D, Translation).
pass_two_neg(A or B, Translation, Univ, Rename) :- !,
	pass_two_neg(A, C, Univ, Rename),
	pass_two_neg(B, D, Univ, Rename),
	sent_and(C, D, Translation).
pass_two_neg(~A, Translation, Univ, Rename) :- !,
	pass_two_pos(A, Translation, Univ, Rename).
pass_two_neg(true, clause([],[]), _, _) :- !.
pass_two_neg(false, [], _, _) :- !.
pass_two_neg(Atom, clause([],[Renamed]), _, Rename) :-
	%   An Atom is "anything else", hence the cuts above.
	term_two(Atom, Renamed, Rename).



term_two(OldTerm, NewTerm, Rename) :-
	nonvar(OldTerm),
	functor(OldTerm, FunctionSymbol, Arity),
	functor(NewTerm, FunctionSymbol, Arity),
	!,
	term_two(Arity, OldTerm, NewTerm, Rename).
term_two(OldVar, NewTerm, Rename) :-
	term_var(Rename, OldVar, NewTerm).


term_var(var(Old,New,_), Var, New) :-
	Old == Var,
	!.
term_var(var(_,_,Rest), Var, New) :-
	term_var(Rest, Var, New).


term_two(0, _, _, _) :- !.
term_two(N, OldTerm, NewTerm, Rename) :-
	arg(N, OldTerm, OldArg),
	term_two(OldArg, NewArg, Rename),
	arg(N, NewTerm, NewArg),
	M is N-1, !,
	term_two(M, OldTerm, NewTerm, Rename).


/*----------------------------------------------------------------------

	sent_and(S1, S2, "S1 and S2")
	sent_or(S1, S2, "S1 or S2")
    perform the indicated logical operations on clauses or sets of
    clauses (sentences), using a fair bit of propositional reasoning
    (hence our use of "literally" to avoid binding variables) to try
    to keep the results simple.  There are several rules concerning
    conjunction which are *not* applied, but even checking for
	A and A --> A
    would require us to recognise alphabetic variants of A rather
    than literal identity.  So far the naivety abount conjunction
    has not proved to be a practical problem.

----------------------------------------------------------------------*/

sent_or(clause(P1,_), clause(_,N2), []) :-
	P1 containsCF Atom,
	N2 literally_contains Atom,
	!.
sent_or(clause(_,N1), clause(P2,_), []) :-
	N1 containsCF Atom,
	P2 literally_contains Atom,
	!.
sent_or(clause(P1,N1), clause(P2,N2), clause(P3,N3)) :- !,
	ord_union(P1, P2, P3),
	ord_union(N1, N2, N3).
sent_or([], _, []) :- !.
sent_or(_, [], []) :- !.
sent_or([Clause|Clauses], Sentence, Answer) :- !,
	sent_or(Sentence, Clause, X),
	sent_or(Clauses, Sentence, Y),
	sent_and(X, Y, Answer).
sent_or(Sentence, [Clause|Clauses], Answer) :- !,
	sent_or(Sentence, Clause, X),
	sent_or(Clauses, Sentence, Y),
	sent_and(X, Y, Answer).


sent_and([], Sentence, Sentence) :- !.
sent_and(Sentence, [], Sentence) :- !.
sent_and([H1|T1], [H2|T2], [H1,H2|T3]) :- !,
	sent_and(T1, T2, T3).
sent_and([H1|T1], Clause, [Clause,H1|T1]) :- !.
sent_and(Clause, [H2|T2], [Clause,H2|T2]) :- !.
sent_and(Clause1, Clause2, [Clause1,Clause2]).


[Head|_] containsCF Head.
[_|Tail] containsCF Something :-
	Tail containsCF Something.


[Head|_] literally_contains Something :-
	Head == Something,
	!.
[_|Tail] literally_contains Something :-
	Tail literally_contains Something.


[] does_not_literally_contain Anything.
[Head|Tail] does_not_literally_contain Something :-
	Head \== Something,
	Tail does_not_literally_contain Something.


/*----------------------------------------------------------------------
    Debugging kit.
	portray_sentence(ListOfClauses)
	    displays a list of clauses, one per line.
	rok_portray_clause(Clause)
	    displays a single clause in "Kowalski notation"
	t(Formula)
	    translates a formula and prints the result.

:- public
	t1/0,t9/0,t/1.


portray_sentence([Clause]) :- !,
	rok_portray_clause(Clause),
	nl.
portray_sentence([Clause|Clauses]) :-
	rok_portray_clause(Clause),
	write(' AND'), nl,
	portray_sentence(Clauses).
portray_sentence([]) :-
	write('TRUE'), nl.


rok_portray_clause(clause(PosAtoms, NegAtoms)) :-
	numbervars(PosAtoms, 0, N),
	numbervars(NegAtoms, N, _),
	rok_portray_clause(PosAtoms, ' v '),
	write(' <- '),
	rok_portray_clause(NegAtoms, ' & '),
	fail.
rok_portray_clause(_).


rok_portray_clause([Atom], _) :- !,
	print(Atom).
rok_portray_clause([Atom|Atoms], Separator) :-
	print(Atom), write(Separator),
	rok_portray_clause(Atoms, Separator).
rok_portray_clause([], _) :-
	write([]).


t(X) :-
	clausal_form(X, Y),
	portray_sentence(Y).

t1 :- t((a=>b) and (b=>c) and (c=>d) and (d=>a) => (a<=>d)).

t2 :- t(continuous(F,X) <=> all(Epsilon, Epsilon > 0 =>
	    some(Delta, Delta > 0 and all(Y,
		abs(Y-X) < Delta => abs(val(F,Y)-val(F,X)) < Epsilon
	)))).

t3 :- clausal_form_of_negation(
	( subset(S1,S2) <=> all(X, member(X,S1) => member(X,S2) )) =>
	( subset(T1,T2) and subset(T2,T3) => subset(T1,T3) )	,Y),
	portray_sentence(Y).

t4 :- t(subset(T1,T2) and subset(T2,T3) => subset(T1,T3)).



t5 :- t((a=>b) and (b=>c)).

t6 :- t(~(a and b)).

t7 :- t((a and b) or c).

t8 :- t((a and b) or (a and ~b) or (~a and b) or (~a and ~b)).

t9 :- t(
	(true(P) <=> t(w0,P)) and
	(t(W1,P1 and P2) <=> t(W1,P1) and t(W1,P2)) and
	(t(W1,P1 or P2) <=> t(W1,P1) or t(W1,P2)) and
	(t(W1,P1 => P2) <=> (t(W1,P1) => t(W1,P2))) and
	(t(W1,P1 <=> P2) <=> (t(W1,P1) <=> t(W1,P2))) and
	(t(W1,~P1) <=> ~t(W1,P1)) and
	(t(W1,know(A1,P1)) <=> all(W2,k(A1,W1,W2)=>t(W2,P1))) and
	k(A1,W1,W1) and
	(k(A1,W1,W2) => (k(A1,W2,W3) => k(A1,W1,W3))) and
	(k(A1,W1,W2) => (k(A1,W1,W3) => k(A1,W2,W3))) and
	(t(W1,know(A,P)) <=> all(W2,k(A,W1,W2) => t(W2,P)))
	).

----------------------------------------------------------------------*/


