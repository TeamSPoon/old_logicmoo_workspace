

:- multifile(t/3).
:- multifile(t/4).
:- multifile(t/5).
:- multifile(t/6).
:- multifile(t/7).
:- multifile(t/8).

coreMt(X):-forward(_BaseKB,'CycInternalAnthropacity',isa,X,ID), \+ disabledAssertion(ID).
% coreMt(X):-forward('BaseKB','CycInternalAnthropacity',isa,X,ID), \+ disabledAssertion(ID).

:- meta_predicate freeze_pvars(*,0).
freeze_pvars( _ ,Goal):-!,call(Goal). 
freeze_pvars([ ],Goal):-!,call(Goal).
freeze_pvars([V],Goal):-!,freeze(V,Goal).
freeze_pvars([V|Vs],Goal):-freeze(V,freeze_pvars(Vs,Goal)).

unt_ify(TGaf,PGaf):- \+ current_prolog_flag(logicmoo_untify,true),!,TGaf=PGaf.
unt_ify(TGaf,PGaf):- compound(TGaf),functor(TGaf,UR,_),arg(_,v(uU,nartR),UR),!,TGaf=PGaf.
unt_ify(TGaf,PGaf):- term_variables(TGaf,TVars),
  freeze_pvars(TVars,unt_ify0(TGaf,PGaf)).
unt_ify0(TGaf,PGaf):- 
 \+compound(TGaf)->PGaf=TGaf ;
    ((TGaf=..[t,P|TARGS]) -> (maplist(unt_ify,TARGS,ARGS)-> apply_term(P,ARGS,PGaf) ; PGaf=TGaf)).

% t_ify(PGaf,TGaf):- \+ current_prolog_flag(logicmoo_untify,true),!,TGaf=PGaf.
t_ify(PGaf,TGaf):- term_variables(PGaf,PVars),
  freeze_pvars(PVars,t_ify0(PGaf,TGaf)).
t_ify0(PGaf,TGaf):- 
 (\+ compound(PGaf)->PGaf=TGaf;
    (PGaf=..[P|ARGS], (P==t -> PGaf=TGaf ; (maplist(t_ify,ARGS,TARGS)->TGaf=..[t,P|TARGS])))).

isT(X):- isT(_,X).

isAskableT(Gaf):- var(Gaf),!.
isAskableT(isT(_)):-!,fail.
isAskableT(call_u(_)):-!,fail.
isAskableT(Gaf):-compound(Gaf).

isT(Mt,Gaf):- isAskableT(Gaf), 
 ( \+ compound(Gaf)->
    (isTT(Mt,TGaf)*->(unt_ify(TGaf,Gaf)->true);fail) ;
    (t_ify(Gaf,TGaf)->isTT(Mt,TGaf))).

:- dynamic(disabledAssertion/1).

disabledAssertion(ID):-fbc(_,_,randomChars,_,ID).
disabledAssertion(ID):-fbc(_,_,_,randomChars,ID).
%disabledAssertion(ID):-fbc(_,isa,randomChars,_,ID).
%disabledAssertion(ID):-fbc(_,genls,randomChars,_,ID).


extra_tcol(Mt,A,ID):- fbc(Mt,isa,A,'tCollection',ID).
extra_tcol(Mt,A,ID):- fbc(Mt,isa,A,'tFirstOrderCollection',ID).
extra_tcol(Mt,A,ID):- fbc(Mt,isa,A,'ttSecondOrderCollection',ID).
extra_tcol(Mt,A,ID):- fbc(Mt,gens,B,C,ID),(A=B;A=C).
extra_tcol(Mt,A,ID):- fbc(Mt,isa,A,'ttVariedOrderCollection',ID).
extra_tcol(Mt,A,ID):- fbc(Mt,isa,A,'tSetOrCollection',ID).

extra_tcol(Mt,A,ID):- fbc(Mt,isa,A,'ttCollectionType',ID).
extra_tcol(Mt,A,ID):- fbc(Mt,isa,A,'ttObjectType',ID).
extra_tcol(Mt,A,ID):- fbc(Mt,isa,A,'ttStuffType',ID).
extra_tcol(Mt,A,ID):- fbc(Mt,isa,A,'ttExistingStuffType',ID).
extra_tcol(Mt,A,ID):- fbc(Mt,isa,A,'ttAtemporalNecessarilyEssentialCollectionType',ID).

specFn(X,Y):-  relToOf(_,Y,X).
superFn(X,Y):-  relToOf(_,X,Y).


relToOf(P,X,Y):-nonvar(X),!,relToOf_v_U(P,X,Y). 
relToOf(P,X,Y):-nonvar(Y),!,chkRelToOf(P,X,Y).
relToOf(P,X,Y):-freeze(Y,relToOf(P,X,Y)).
relToOf_v_U(P,X,Y):-var(Y),!,freeze(X,relToOf(P,X,Y)),freeze(Y,relToOf(P,X,Y)).
relToOf_v_U(P,X,Y):-freeze(X,relToOf(P,X,Y)).

pat(P,A,B):-fbc(_Mt,P,A,B,ID),\+ notrace(disabledAssertion(ID)).

transbin(genlMt).
transbin(genls).

chkRelToOf(P,X,Y):-dif(X,Y),transbin(P),pat(P,X,RelTo2),(Y=RelTo2;(pat(P,RelTo2,RelTo3),(Y=RelTo3;pat(P,RelTo3,Y)))).

% extra_tcol(Mt,A,ID):- is(TT,(Mt,t(genls,A,Other),ID),atom(Other),Other\=A,'Thing'\=Other.
% extra_tcol(Mt,A,ID):- is(TT,(Mt,t(genls,Other,A),ID),atom(Other),Other\=A,'Thing'\=Other.

isTT(Mt,TGaf):- no_repeats(TGaf,((isTT(Mt,TGaf,ID), \+ disabledAssertion(ID)))).

isTT(Mt,t(P,A),ID):-fbc(Mt,P,A,ID).
isTT(Mt,t(tCol,A),ID):- extra_tcol(Mt,A,ID).
isTT(Mt,t(P,A,B),ID):-fbc(Mt,P,A,B,ID).
isTT(Mt,t(P,A,B,C),ID):-fbc(Mt,P,A,B,C,ID).
isTT(Mt,t(P,A,B,C,D),ID):-fbc(Mt,P,A,B,C,D,ID).
isTT(Mt,t(P,A,B,C,D,E),ID):-fbc(Mt,P,A,B,C,D,E,ID).
isTT(Mt,t(P,A,B,C,D,E,F),ID):-fbc(Mt,P,A,B,C,D,E,F,ID).
isTT(_,t(ist,Mt,PAB),ID):- nonvar(Mt),!,isTT(Mt,PAB,ID), \+ arg(1,PAB,ist).




fbc(Mt,P,A,B,ID):-forward(Mt,B,P,A,ID).
fbc(Mt,P,A,B,ID):-backward(Mt,B,P,A,ID).
fbc(Mt,P,A,B,ID):-code(Mt,B,P,A,ID).

fbc(Mt,P,A,ID):-forward(Mt,P,A,ID).
fbc(Mt,P,A,ID):-backward(Mt,P,A,ID).
fbc(Mt,P,A,ID):-code(Mt,P,A,ID).


fbc(Mt,P,A,B,C,ID):-forward(Mt,A,P,B,C,ID).
fbc(Mt,P,A,B,C,ID):-backward(Mt,A,P,B,C,ID).
fbc(Mt,P,A,B,C,ID):-code(Mt,A,P,B,C,ID).


fbc(Mt,P,A,B,C,D,ID):-forward(Mt,A,P,B,C,D,ID).
fbc(Mt,P,A,B,C,D,ID):-backward(Mt,A,P,B,C,D,ID).
fbc(Mt,P,A,B,C,D,ID):-code(Mt,A,P,B,C,D,ID).

fbc(Mt,P,A,B,C,D,E,ID):-forward(Mt,A,P,B,C,D,E,ID).
fbc(Mt,P,A,B,C,D,E,ID):-backward(Mt,A,P,B,C,D,E,ID).
fbc(Mt,P,A,B,C,D,E,ID):-code(Mt,A,P,B,C,D,E,ID).

fbc(Mt,P,A,B,C,D,E,F,ID):-forward(Mt,A,P,B,C,D,E,F,ID).
fbc(Mt,P,A,B,C,D,E,F,ID):-backward(Mt,A,P,B,C,D,E,F,ID).
fbc(Mt,P,A,B,C,D,E,F,ID):-code(Mt,A,P,B,C,D,E,F,ID).

makeRenames:- tell('renames.lisp'), forall(rename(A,B),makeRenameForCyc(B,A)),told.

makeRenameForCyc(B,A):- format('(csetq *const* (find-constant "~w")) (pwhen *const* (rename-constant *const* "~w"))~n',[B,A]).

end_of_file.

~coExtensional(C, A) :-
        ~differenceFactsForTerms(B, A, genls(B, D)&genls(A, C)),
        coExtensional(D, B),
        different(D, B),
        disjointWith(D, C),
        different(C, A).
~coExtensional(B, A) :-
        ~differenceFactsForTerms(A, C, genls(A, B)&genls(C, D)),
        different(B, A),
        disjointWith(B, D),
        coExtensional(D, C),
        different(D, C).
~different(C, A) :-
        ~differenceFactsForTerms(B, A, genls(B, D)&genls(A, C)),
        coExtensional(D, B),
        different(D, B),
        disjointWith(D, C),
        coExtensional(C, A).
~different(B, A) :-
        ~differenceFactsForTerms(A, C, genls(A, B)&genls(C, D)),
        coExtensional(B, A),
        disjointWith(B, D),
        coExtensional(D, C),
        different(D, C).
~disjointWith(B, D) :-
        ~differenceFactsForTerms(A, C, genls(A, B)&genls(C, D)),
        coExtensional(B, A),
        different(B, A),
        coExtensional(D, C),
        different(D, C).
differenceFactsForTerms(A, B, genls(A, C)&genls(B, D)) :-
        coExtensional(C, A),
        different(C, A),
        disjointWith(C, D),
        coExtensional(D, B),
        different(D, B).

~coExtensional(A, C) :-
        isa(B, A),
        ~isa(B, C).
~isa(B, A) :-
        coExtensional(A, C),
        ~isa(B, C).
isa(A, C) :-
        isa(A, B),
        coExtensional(B, C).

