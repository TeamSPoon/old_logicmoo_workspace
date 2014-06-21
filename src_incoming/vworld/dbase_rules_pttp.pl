:-module(dbase_rules_pttp,[ nnf/2, dnf/2, def_nnf/5, make_matrix/3,pttp1/2,
        op(400,fy,-),    % negation
	op(500,xfy,&),   % conjunction
	op(600,xfy,v),   % disjunction
	op(650,xfy,=>),  % implication
	op(680,xfy,<=>), % equivalence
        op( 500, fy, ~),    % negation
       op( 500, fy, all),  % universal quantifier
       op( 500, fy, ex),   % existential quantifier
       op( 500,xfy, :),
       nnf/4,
       pttp_test/2,
       do_pttp_test/1
        ]).

:- thread_local int_query/6.
:- thread_local int_query/7.

unimplemented:-throw(unimplemented).

%%% ****f* PTTP_Examples/chang_lee_example1
%%% DESCRIPTION
%%%   Prove that in an associative system with left and right
%%%   solutions, there is a right identity element.
%%% NOTES
%%%   this is problem GRP028-4 in TPTP
%%%
%%%   this and the other chang_lee examples are taken from
%%%   C.L. Chang and R.C.T. Lee,
%%%   Symbolic Logic and Mechanical Theorem Proving,
%%%   Academic Press, New York, 1973, pp. 298-305.
%%%
%%%   the result of executing these examples
%%%   can be seen in the file pttp-examples.typescript
%%%
%%%   this problem contains only Horn clauses
%%%   (clauses with at most one positive literal)
%%%   so neither contrapositives nor more than one
%%%   instance of an all negative query are required
%%%   for completeness
%%%
%%%   so clauses are written in implication form to
%%%   suppress generation of unnecessary contrapositives
%%%   and the negation of the query is not included
%%% SEE ALSO
%%%   chang_lee_example7, chang_lee_example8
%%% SOURCE

do_pttp_test(TestName):- forall(pttp_test(TestName,Data),(retractall(int_query(_,_,_,_,_,_,_)),retractall(int_query(_,_,_,_,_,_)),once(pttp(Data)),prove(query))).

pttp_test(logicmoo_example1,
	((
          motherOf(joe,sue),
          (motherOf(X,Y) => female(Y)),
          (sonOf(Y,X) => (motherOf(X,Y);fatherOf(X,Y))),          
          (query:-female(Y))
	))).

pttp_test(logicmoo_example1_holds,
	((
          firstOrder(motherOf,joe,sue),
          (firstOrder(motherOf,X,Y) => firstOrder(female,Y)),
          (firstOrder(sonOf,Y,X) => (firstOrder(motherOf,X,Y);firstOrder(fatherOf,X,Y))),          
          (query:-firstOrder(female,Y))
	))).

%  int_kbholds(sonOf,gun,phil,A,B,C,C,D,E,F):-D=[G,[1,F,A,B]|H],E=[G|H].
%  int_not_kbholds(female,gun,A,B,C,C,D,E,F):-D=[G,[-2,F,A,B]|H],E=[G|H].
%  int_query(A,B,C,D,E,F,G):- (E=[H,[3,query,A,B]|I],J=[H|I]),firstOrder(K,phil,gun,A,B,C,D,J,F).
%  firstOrder(A,B,C,D,E,F,G,H,I):-J=firstOrder(A,B,C), (identical_member(J,D)->fail; (identical_member(J,E),!;unifiable_member(J,E)),G=F,H=[K,[red,J,D,E]|L],I=[K|L];int_kbholds(A,B,C,D,E,F,G,H,I,J)).
%  not_kbholds(A,B,C,D,E,F,G,H):-I=firstOrder(A,B), (identical_member(I,D)->fail; (identical_member(I,C),!;unifiable_member(I,C)),F=E,G=[J,[redn,I,C,D]|K],H=[J|K];int_not_kbholds(A,B,C,D,E,F,G,H,I)).
pttp_test(logicmoo_example2,
	((
          firstOrder(sonOf,gun,phil),
          not(firstOrder(female,gun)),
          (query:-firstOrder(What,phil,gun))
          % What = fatherOf
	))).

pttp_test(logicmoo_example3,
	((
          (secondOrder(genls,SubClass,SuperClass) & firstOrder(SubClass,Instance) => firstOrder(SuperClass,Instance)),
          (secondOrder(genlPreds,P1,P2) & firstOrder(P1,A) => firstOrder(P2,A)),
          (secondOrder(genlPreds,P1,P2) & firstOrder(P1,A,B) => firstOrder(P2,A,B)),
          (secondOrder(genlPreds,P1,P2) & firstOrder(P1,A,B,C) => firstOrder(P2,A,B,C)),
          (secondOrder(genlPreds,P1,P2) & firstOrder(P1,A,B,C,D) => firstOrder(P2,A,B,C,D)),
          (secondOrder(genlInverse,P1,P2) & firstOrder(P1,A,B) => firstOrder(P2,B,A)),

          (secondOrder(irreflexive,P) & firstOrder(P,A,B) => ~ firstOrder(P,B,A)),

           (secondOrder(P,A,B)<=>firstOrder(P,A,B)),
           secondOrder(genlInverse,parentOf,sonOf),
           secondOrder(genlPreds,motherOf,parentOf),
           
           secondOrder(irreflexive,sonOf),
           

          (query:- not(firstOrder(sonOf,gun,phil)))
          % Expected true
	))):- fail.

          % (all X:( all Y : (motherOf(X,Y)) => (bellyButton(X) , older(X,Y) , female(Y)) )),

make_matrix_pttp(In,Out):-  % trace,
   nnf(In,[],MOut,_),
  % make_matrix(In,MOut,[def]),
   delist_junts(MOut,Out),   
   % 'format'('Tranforms: ~q ~n -> ~q ~n -> ~q ~n~n',[In,MOut,Out]),
   !.

delist_junts(A,A):-isVar(A),!.
delist_junts(-A,AA):-!, negated_literal(A,AA).
delist_junts((A:-B),(C:-D)):- delist_junts(A,C), delist_junts(B,D).
delist_junts((A,B),(C,D)):- delist_junts(A,C), delist_junts(B,D).
delist_junts((A;B),(C;D)):- delist_junts(A,C), delist_junts(B,D).
delist_junts(A,A).

delist_junts([[In]],Out):-!,delist_lit(In,Out).
delist_junts(InList,Out):-apply_to_elements(InList,delist_lit,OutList),!,disjoin_list(OutList,Out).
delist_junts(Out,Out).

disjoin_list([Out],Out):-!.
disjoin_list((A,B),(C;D)):- disjoin_list(A,C), disjoin_list(B,D).
disjoin_list([Out|List],(Out ; ListO)):-disjoin_list(List,ListO).
disjoin_list(Out,Out):-!.
conjoin_list([Out],Out):-!.
conjoin_list([Out|List],(Out,ListO)):-conjoin_list(List,ListO).
conjoin_list(Out,Out):-!.

delist_lit(In,Out):- delist_lit0(In,Out).
delist_lit0(-In,Out):- !,negated_literal(In,Out).
delist_lit0([In],Out):- !,delist_lit0(In,Out).
delist_lit0([],truef).
delist_lit0([A|B],Out):-apply_to_elements([A|B],delist_lit,OutList),conjoin_list(OutList,Out).
delist_lit0(Out,Out).


pttp_test(chang_lee_example1,
	((
		p(g(X,Y),X,Y),
		p(X,h(X,Y),Y),
		(p(U,Z,W) :- p(X,Y,U) , p(Y,Z,V) , p(X,V,W)),
		(p(X,V,W) :- p(X,Y,U) , p(Y,Z,V) , p(U,Z,W)),
		(query :- p(k(X),X,k(X)))
	))).

%%% ***
%%% ****f* PTTP_Examples/chang_lee_example2
%%% DESCRIPTION
%%%   In an associative system with an identity element,
%%%   if the square of every element is the identity,
%%%   the system is commutative.
%%% NOTES
%%%   this is problem GRP001-5 in TPTP
%%% SOURCE

pttp_test(chang_lee_example2,
	((
		p(e,X,X),
		p(X,e,X),
		p(X,X,e),
		p(a,b,c),
		(p(U,Z,W) :- p(X,Y,U) , p(Y,Z,V) , p(X,V,W)),
		(p(X,V,W) :- p(X,Y,U) , p(Y,Z,V) , p(U,Z,W)),
		(query :- p(b,a,c))
	))).

%%% ***
%%% ****f* PTTP_Examples/chang_lee_example3
%%% DESCRIPTION
%%%   In a group the left identity is also a right identity.
%%% NOTES
%%%   this is problem GRP003-1 in TPTP
%%% SOURCE

pttp_test(chang_lee_example3,
	((
          p(e,X,X),
          p(i(X),X,e),
          (p(U,Z,W) :- p(X,Y,U) , p(Y,Z,V) , p(X,V,W)),
          (p(X,V,W) :- p(X,Y,U) , p(Y,Z,V) , p(U,Z,W)),
          (query :- p(a,e,a))
	))).


%%% ***
%%% ****f* PTTP_Examples/chang_lee_example4
%%% DESCRIPTION
%%%   In a group with left inverse and left identity
%%%   every element has a right inverse.
%%% NOTES
%%%   this is problem GRP004-1 in TPTP
%%% SOURCE
pttp_test(chang_lee_example4,
	((
          p(e,X,X),
          p(i(X),X,e),
          (p(U,Z,W) :- p(X,Y,U) , p(Y,Z,V) , p(X,V,W)),
          (p(X,V,W) :- p(X,Y,U) , p(Y,Z,V) , p(U,Z,W)),
          (query :- p(a,X,e))
	))).


%%% ***
%%% ****f* PTTP_Examples/chang_lee_example5
%%% DESCRIPTION
%%%   If S is a nonempty subset of a group such that
%%%   if x,y belong to S, then x*inv(y) belongs to S,
%%%   then the identity e belongs to S.
%%% NOTES
%%%   this is problem GRP005-1 in TPTP
%%% SOURCE

pttp_test(chang_lee_example5,
	pttp((
		p(e,X,X),
		p(X,e,X),
		p(X,i(X),e),
		p(i(X),X,e),
		s(a),
		(s(Z) :- s(X) , s(Y) , p(X,i(Y),Z)),
		(p(U,Z,W) :- p(X,Y,U) , p(Y,Z,V) , p(X,V,W)),
		(p(X,V,W) :- p(X,Y,U) , p(Y,Z,V) , p(U,Z,W)),
		(query :- s(e))
	))).

%%% ***
%%% ****f* PTTP_Examples/chang_lee_example6
%%% DESCRIPTION
%%%   If S is a nonempty subset of a group such that
%%%   if x,y belong to S, then x*inv(y) belongs to S,
%%%   then S contains inv(x) whenever it contains x.
%%% NOTES
%%%   this is problem GRP006-1 in TPTP
%%% SOURCE

chang_lee_example6 :-
	pttp((
		p(e,X,X),
		p(X,e,X),
		p(X,i(X),e),
		p(i(X),X,e),
		s(b),
		(s(Z) :- s(X) , s(Y) , p(X,i(Y),Z)),
		(p(U,Z,W) :- p(X,Y,U) , p(Y,Z,V) , p(X,V,W)),
		(p(X,V,W) :- p(X,Y,U) , p(Y,Z,V) , p(U,Z,W)),
		(query :- s(i(b)))
	)),
	fail.
chang_lee_example6 :-
	prove(query).
%%% ***
%%% ****f* PTTP_Examples/chang_lee_example7
%%% DESCRIPTION
%%%   If a is a prime and a = b*b/c*c then a divides b.
%%% NOTES
%%%   this is problem NUM014-1 in TPTP
%%%
%%%   this problem is non-Horn
%%%   so clauses are written in disjunction form to
%%%   result in generation of all contrapositives
%%%
%%%   because the query is ground, it is unnecessary
%%%   for its negation to be included
%%% SEE ALSO
%%%   chang_lee_example1, chang_lee_example8
%%% SOURCE

chang_lee_example7 :-
	pttp((
		p(a),
		m(a,s(c),s(b)),
		m(X,X,s(X)),
		(not_m(X,Y,Z) ; m(Y,X,Z)),
		(not_m(X,Y,Z) ; d(X,Z)),
		(not_p(X) ; not_m(Y,Z,U) ; not_d(X,U) ; d(X,Y) ; d(X,Z)),
		(query :- d(a,b))
	)),
	fail.
chang_lee_example7 :-
	prove(query).
%%% ***
%%% ****f* PTTP_Examples/chang_lee_example8
%%% DESCRIPTION
%%%    Any number greater than one has a prime divisor.
%%% NOTES
%%%   this is problem NUM015-1 in TPTP
%%%
%%%   this problem is non-Horn
%%%   so clauses are written in disjunction form to
%%%   result in generation of all contrapositives
%%%
%%%   the negation of the query is included
%%%   to allow multiple instances to be used in
%%%   the proof (and yield an indefinite answer)
%%% SEE ALSO
%%%   chang_lee_example1, chang_lee_example7
%%% SOURCE

chang_lee_example8 :-
	pttp((
		l(1,a),
		d(X,X),
		(p(X) ; d(g(X),X)),
		(p(X) ; l(1,g(X))),
		(p(X) ; l(g(X),X)),
		(not_p(X) ; not_d(X,a)),		% negation of query
		(not_d(X,Y) ; not_d(Y,Z) ; d(X,Z)),
		(not_l(1,X) ; not_l(X,a) ; p(f(X))),
		(not_l(1,X) ; not_l(X,a) ; d(f(X),X)),
		(query :- (p(X) , d(X,a)))
	)),
	fail.
chang_lee_example8 :-
	prove(query).
%%% ***
%%% ****f* PTTP_Examples/chang_lee_example9
%%% DESCRIPTION
%%%   There exist infinitely many primes.
%%% NOTES
%%%   this is problem NUM016-2 in TPTP
%%% SOURCE

chang_lee_example9 :-
	pttp((
		l(X,f(X)),
		not_l(X,X),
		(not_l(X,Y) ; not_l(Y,X)),
		(not_d(X,f(Y)) ; l(Y,X)),
		(p(X) ; d(h(X),X)),
		(p(X) ; p(h(X))),
		(p(X) ; l(h(X),X)),
		(not_p(X) ; not_l(a,X) ; l(f(a),X)),	% negation of query
		(query :- p(X) , l(a,X) , not_l(f(a),X))
	)),
	fail.
chang_lee_example9 :-
	prove(query).
%%% ***
%%% ****f* PTTP_Examples/overbeek_example4
%%% DESCRIPTION
%%%   Show that Kalman's shortest single axiom for the
%%%   equivalential calculus, XGK, can be derived from the
%%%   Meredith single axiom PYO.
%%% NOTES
%%%   a harder problem than the Chang and Lee examples
%%%   from Overbeek's competition problems
%%%
%%%   this is problem LCL024-1 in TPTP
%%% SOURCE

overbeek_example4 :-
	pttp((
		p(e(X,e(e(Y,e(Z,X)),e(Z,Y)))),
		(p(Y) :- p(e(X,Y)), p(X)),
		(query :- p(e(e(e(a,e(b,c)),c),e(b,a))))
	)),
	fail.
overbeek_example4 :-
	prove(query,100,0,2).	% cost 30 proof
%%% ***


%% File: def_mm.pl  -  Version: 1.01  -  Date: 07 June 2007
%%
%% Purpose: Transform first-order formulae into clausal form
%%
%% Author:  Jens Otten
%% Web:     www.leancop.de
%%
%% Usage:   make_matrix(F,M,S).  % where F is a first-order formula,
%%                               % S is a list of settings, and M is
%%                               % the (definitional) clausal form
%%
%% Example: make_matrix(ex Y: (all X: ((p(Y) => p(X))) ),Matrix,[]).
%%          Matrix = [[-(p(X1))], [p(1 ^ [X1])]]
%%
%% Copyright: (c) 1999-2007 by Jens Otten
%% License:   GNU General Public License


% definitions of logical connectives and quantifiers

:- op(1130, xfy, <=>). % equivalence
:- op(1110, xfy, =>).  % implication
%                      % disjunction (;)
%                      % conjunction (,)
:- op( 500, fy, ~).    % negation
:- op( 500, fy, all).  % universal quantifier
:- op( 500, fy, ex).   % existential quantifier
:- op( 500,xfy, :).


% ------------------------------------------------------------------
%  make_matrix(+Fml,-Matrix,+Settings)
%    -  transform first-order formula into set of clauses (matrix)
%
%  Fml, Matrix: first-order formula and matrix
%
%  Settings: list of settings, which can contain def, nodef and conj;
%            if it contains nodef/def, no definitional transformation
%            or a complete definitional transformation is done,
%            otherwise a definitional transformation is done for
%            the conjecture and the standard transformation is done
%            for the axioms; conjecture is marked if conj is given
%
%  Syntax of Fml: negation '~', disjunction ';', conjunction ',',
%      implication '=>', equivalence '<=>', universal/existential
%      quantifier 'all X:<Formula>'/'ex X:<Formula>' where 'X' is a
%      Prolog variable, and atomic formulae are Prolog atoms.
%
%  Example: make_matrix(ex Y:(all X:((p(Y) => p(X)))),Matrix,[]).
%           Matrix = [[-(p(X1))], [p(1 ^ [X1])]]

make_matrix(Fml,Matrix,Set) :-
    univar(Fml,[],F1),
    ( member(conj,Set), F1=(A=>C) -> F2=((A,#)=>(#,C)) ; F2=F1 ),
    ( member(nodef,Set) ->
       def_nnf(F2,NNF,1,_,nnf), dnf(NNF,DNF)
       ;
       \+member(def,Set), F2=(B=>D) ->
        def_nnf(~(B),NNF,1,I,nnf), dnf(NNF,DNF1),
        def_nnf(D,DNF2,I,_,def), DNF=(DNF2;DNF1)
        ;
        def_nnf(F2,DNF,1,_,def)
    ),
    mat(DNF,M),
    ( member(reo(I),Set) -> mreorder(M,Matrix,I) ; Matrix=M ).

% ------------------------------------------------------------------
%  def_nnf(+Fml,-DEF)  -  transform formula into a definitional
%                         Skolemized negation normal form (DEF)
%  Fml, DEF: first-order formula and formula in DEF
%
%  make_matrix( 
%
%  def_nnf(all X: ((bellyButton(X) , older(X,Y) , female(Y))  => ex Y : (motherOf(X,Y))),T,1,DEF,D).
% T = ((~bellyButton(1^[]);~older(1^[], _G814);~female(_G814));motherOf(1^[], _G814)),
% DEF = 2.
%
%  Example: def_nnf(ex Y:(all X:((p(Y) => p(X)))),T,1,DEF,D).
%           DEF = ~ p(X1) ; p(1 ^ [X1])

def_nnf(Fml,DEF,I,I1,Set) :-
    def(Fml,[],NNF,DEF1,_,I,I1,Set), def(DEF1,NNF,DEF).

def([],Fml,Fml).
def([(A,(B;C))|DefL],DEF,Fml) :- !, def([(A,B),(A,C)|DefL],DEF,Fml).
def([A|DefL],DEF,Fml) :- def(DefL,(A;DEF),Fml).

def(Fml,FreeV,NNF,DEF,Paths,I,I1,Set) :-
    ( Fml = ~(~A)      -> Fml1 = A;
      Fml = ~(all X:F) -> Fml1 = (ex X: ~F);
      Fml = ~(ex X:F)  -> Fml1 = (all X: ~F);
      Fml = ~((A ; B)) -> Fml1 = ((~A , ~B));
      Fml = ~((A , B)) -> Fml1 = (~A ; ~B);
      Fml = (A => B)   -> Fml1 = (~A ; B);
      Fml = ~((A => B))-> Fml1 = ((A , ~B));
      Fml = (A <=> B)  ->
      ( Set=def        -> Fml1 = ((A => B) , (B => A));
                          Fml1 = ((A , B) ; (~A , ~B)) );
      Fml = ~((A<=>B)) -> Fml1 = ((A , ~B) ; (~A , B)) ), !,
    def(Fml1,FreeV,NNF,DEF,Paths,I,I1,Set).

def((ex X:F),FreeV,NNF,DEF,Paths,I,I1,Set) :- !,
    def(F,[X|FreeV],NNF,DEF,Paths,I,I1,Set).

def((all X:Fml),FreeV,NNF,DEF,Paths,I,I1,Set) :- !,
    copy_term((X,Fml,FreeV),((I^FreeV),Fml1,FreeV)), I2 is I+1,
    def(Fml1,FreeV,NNF,DEF,Paths,I2,I1,Set).

def((A ; B),FreeV,NNF,DEF,Paths,I,I1,Set) :- !,
    def(A,FreeV,NNF1,DEF1,Paths1,I,I2,Set),
    def(B,FreeV,NNF2,DEF2,Paths2,I2,I1,Set),
    append(DEF1,DEF2,DEF), Paths is Paths1 * Paths2,
    (Paths1 > Paths2 -> NNF = (NNF2;NNF1);
                        NNF = (NNF1;NNF2)).

def((A , B),FreeV,NNF,DEF,Paths,I,I1,Set) :- !,
    def(A,FreeV,NNF3,DEF3,Paths1,I,I2,Set),
    ( NNF3=(_;_), Set=def -> append([(~I2^FreeV,NNF3)],DEF3,DEF1),
                             NNF1=I2^FreeV, I3 is I2+1 ;
                             DEF1=DEF3, NNF1=NNF3, I3 is I2 ),
    def(B,FreeV,NNF4,DEF4,Paths2,I3,I4,Set),
    ( NNF4=(_;_), Set=def -> append([(~I4^FreeV,NNF4)],DEF4,DEF2),
                             NNF2=I4^FreeV, I1 is I4+1 ;
                             DEF2=DEF4, NNF2=NNF4, I1 is I4 ),
    append(DEF1,DEF2,DEF), Paths is Paths1 + Paths2,
    (Paths1 > Paths2 -> NNF = (NNF2,NNF1);
                        NNF = (NNF1,NNF2)).

def(Lit,_,Lit,[],1,I,I,_).

% ------------------------------------------------------------------
%  dnf(+NNF,-DNF)  -  transform formula in NNF into formula in DNF
%  NNF, DNF: formulae in NNF and DNF
%
%  Example: dnf(((p;~p),(q;~q)),DNF).
%           DNF = (p, q ; p, ~ q) ; ~ p, q ; ~ p, ~ q

dnf(((A;B),C),(F1;F2)) :- !, dnf((A,C),F1), dnf((B,C),F2).
dnf((A,(B;C)),(F1;F2)) :- !, dnf((A,B),F1), dnf((A,C),F2).
dnf((A,B),F) :- !, dnf(A,A1), dnf(B,B1),
    ( (A1=(C;D);B1=(C;D)) -> dnf((A1,B1),F) ; F=(A1,B1) ).
dnf((A;B),(A1;B1)) :- !, dnf(A,A1), dnf(B,B1).
dnf(Lit,Lit).

% ------------------------------------------------------------------
%  mat(+DNF,-Matrix)  -  transform formula in DNF into matrix
%  DNF, Matrix: formula in DNF, matrix
%
%  Example: mat(((p, q ; p, ~ q) ; ~ p, q ; ~ p, ~ q),Matrix).
%           Matrix = [[p, q], [p, -(q)], [-(p), q], [-(p), -(q)]]

mat((A;B),M) :- !, mat(A,MA), mat(B,MB), append(MA,MB,M).
mat((A,B),M) :- !, (mat(A,[CA]),mat(B,[CB]) -> union2(CA,CB,M);M=[]).
mat(~Lit,[[-Lit]]) :- !.
mat(Lit,[[Lit]]).

% ------------------------------------------------------------------
%  univar(+Fml,[],-Fml1)  -  rename variables
%  Fml, Fml1: first-order formulae
%
%  Example: univar((all X:(p(X) => (ex X:p(X)))),[],F1).
%           F1 = all Y : (p(Y) => ex Z : p(Z))

univar(X,_,X)  :- (atomic(X);var(X);X==[[]]), !.
univar(F,Q,F1) :-
    F=..[A,B|T], ( (A=ex;A=all) -> B=(X:C), delete2(Q,X,Q1),
    copy_term((X,C,Q1),(Y,D,Q1)), univar(D,[Y|Q],D1), F1=..[A,Y:D1] ;
    univar(B,Q,B1), univar(T,Q,T1), F1=..[A,B1|T1] ).

% ------------------------------------------------------------------
%  union2/member2 - union and member for lists without unification

union2([],L,[L]).
union2([X|L1],L2,M) :- member2(X,L2), !, union2(L1,L2,M).
union2([X|_],L2,M)  :- (-Xn=X;-X=Xn) -> member2(Xn,L2), !, M=[].
union2([X|L1],L2,M) :- union2(L1,[X|L2],M).

member2(X,[Y|_]) :- X==Y, !.
member2(X,[_|T]) :- member2(X,T).

% ------------------------------------------------------------------
%  delete2 - delete variable from list

delete2([],_,[]).
delete2([X|T],Y,T1) :- X==Y, !, delete2(T,Y,T1).
delete2([X|T],Y,[X|T1]) :- delete2(T,Y,T1).

% ------------------------------------------------------------------
%  mreorder - reorder clauses

mreorder(M,M,0) :- !.
mreorder(M,M1,I) :-
    length(M,L), K is L//3, append(A,D,M), length(A,K),
    append(B,C,D), length(C,K), mreorder2(C,A,B,M2), I1 is I-1,
    mreorder(M2,M1,I1).

mreorder2([],[],C,C).
mreorder2([A|A1],[B|B1],[C|C1],[A,B,C|M1]) :- mreorder2(A1,B1,C1,M1).



 
% ?- make_matrix(all X:((bellyButton(X) , older(X,Y) , female(Y))  => ex Y : (motherOf(X,Y))),D,[]).

pnf2pl(PNF,Prolog):- make_matrix(PNF,NNF,SET),Prolog=nnf(NNF,SET).


   

%%% ****h* PTTP/PTTP
%%% COPYRIGHT
%%%   Copyright (c) 1988-2003 Mark E. Stickel, SRI International, Menlo Park, CA 94025  USA
%%% 
%%%   Permission is hereby granted, free of charge, to any person obtaining a
%%%   copy of this software and associated documentation files (the "Software"),
%%%   to deal in the Software without restriction, including without limitation
%%%   the rights to use, copy, modify, merge, publish, distribute, sublicense,
%%%   and/or sell copies of the Software, and to permit persons to whom the
%%%   Software is furnished to do so, subject to the following conditions:
%%% 
%%%   The above copyright notice and this permission notice shall be included
%%%   in all copies or substantial portions of the Software.
%%% 
%%%   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%%   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%%%   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
%%%   IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
%%%   CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
%%%   TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
%%%   SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%% DESCRIPTION
%%%  
%%%      A Prolog Technology Theorem Prover
%%%  
%%%               Mark E. Stickel
%%%  
%%%   Prolog is not a full theorem prover
%%%   for three main reasons:
%%%  
%%%     It uses an unsound unification algorithm without
%%%     the occurs check.
%%%  
%%%     Its inference system is complete for Horn
%%%     clauses, but not for more general formulas.
%%%  
%%%     Its unbounded depth-first search strategy
%%%     is incomplete.
%%%  
%%%   Also, it cannot display the proofs it finds.
%%%  
%%%   The Prolog Technology Theorem Prover (PTTP)
%%%   overcomes these limitations by
%%%  
%%%     transforming clauses so that head literals have
%%%     no repeated variables and unification without the
%%%     occurs check is valid; remaining unification
%%%     is done using complete unification with the
%%%     occurs check in the body;
%%%  
%%%     adding contrapositives of clauses (so that
%%%     any literal, not just a distinguished head
%%%     literal, can be resolved on) and the model-
%%%     elimination procedure reduction rule that
%%%     matches goals with the negations of their
%%%     ancestor goals;
%%%  
%%%     using a sequence of bounded depth-first searches
%%%     to prove a theorem;
%%%  
%%%     retaining information on what formulas are
%%%     used for each inference so that the proof
%%%     can be printed.
%%%  
%%%   This version of PTTP translates first-order
%%%   predicate calculus formulas written in a Prolog-like
%%%   notation into Prolog code.  The resulting code
%%%   is then compiled and executed.
%%%  
%%%   PTTP commands:
%%%  
%%%     pttp(formula) - translates the first-order formula
%%%       (normally a conjunction of formulas) into Prolog
%%%       and compiles it
%%%
%%%     prove(formula) - tries to prove formula
%%%
%%%   Look at the description of these functions
%%%   and the examples for more details on how
%%%   pttp and prove should be used.
%%%   For more information on PTTP, consult
%%%     Stickel, M.E.  A Prolog technology theorem prover:
%%%     implementation by an extended Prolog compiler.
%%%     Journal of Automated Reasoning 4, 4 (1988), 353-380.
%%%     and
%%%     Stickel, M.E.  A Prolog technology theorem prover:
%%%     a new exposition and implementation in Prolog.
%%%     Technical Note 464, Artificial Intelligence Center,
%%%     SRI International, Menlo Park, California, June 1989.
%%%
%%%
%%%
%%%   Several arguments are added to each predicate:
%%%   PosAncestors is list of positive ancestor goals
%%%   NegAncestors is list of negative ancestor goals
%%%   DepthIn is depth bound before goal is solved
%%%   DepthOut will be set to remaining depth bound after goal is solved
%%%   ProofIn is dotted-pair difference list of proof so far
%%%   ProofOut will be set to list of steps of proof so far after goal is solved
%%%  
%%%
%%%
%%%   Depth-first iterative-deepening search.
%%%
%%%   PTTP adds arguments DepthIn and DepthOut
%%%   to each PTTP literal to control bounded depth-first
%%%   search.  When a literal is called,
%%%   DepthIn is the current depth bound.  When
%%%   the literal exits, DepthOut is the new number
%%%   of levels remaining after the solution of
%%%   the literal (DepthIn - DepthOut is the number
%%%   of levels used in the solution of the goal.)
%%%
%%%   For clauses with empty bodies or bodies
%%%   composed only of builtin functions,
%%%   DepthIn = DepthOut.
%%%
%%%   For other clauses, the depth bound is
%%%   compared to the cost of the body.  If the
%%%   depth bound is exceeded, the clause fails.
%%%   Otherwise the depth bound is reduced by
%%%   the cost of the body.
%%%
%%%   p :- q , r.
%%%   is transformed into
%%%   p(DepthIn,DepthOut) :-
%%%       DepthIn >= 2, Depth1 is DepthIn - 2,
%%%       q(Depth1,Depth2),
%%%       r(Depth2,DepthOut).
%%%
%%%   p :- q ; r.
%%%   is transformed into
%%%   p(DepthIn,DepthOut) :-
%%%       DepthIn >= 1, Depth1 is DepthIn - 1,
%%%       (q(Depth1,DepthOut) ; r(Depth1,DepthOut)).
%%%
%%%
%%%
%%%   Complete inference.
%%%
%%%   Model elimination reduction operation and
%%%   identical ancestor goal pruning.
%%%
%%%   Two arguments are added to each literal, one
%%%   for all the positive ancestors, one for all
%%%   the negative ancestors.
%%%
%%%   Unifiable membership is checked in the list 
%%%   of opposite polarity to the goal
%%%   for performing the reduction operation.
%%%
%%%   Identity membership is checked in the list
%%%   of same polarity as the goal
%%%   for performing the ancestor goal pruning operation.
%%%   This is not necessary for soundness or completeness,
%%%   but is often effective at substantially reducing the
%%%   number of inferences.
%%%
%%%   The current head goal is added to the front
%%%   of the appropriate ancestor list during the
%%%   call on subgoals in bodies of nonunit clauses.
%%%
%%%
%%%
%%%   Proof Printing.
%%%
%%%   Add extra arguments to each goal so that information
%%%   on what inferences were made in the proof can be printed
%%%   at the end.
%%% ***
%%% ****f* PTTP/pttp
%%% DESCRIPTION
%%%   pttp is the PTTP compiler top-level predicate.
%%%   Its argument is a conjunction of formulas to be compiled.
%%% SOURCE


%:-      module(nnf,[nnf/2]).

:-      op(400,fy,-),    % negation
	op(500,xfy,&),   % conjunction
	op(600,xfy,v),   % disjunction
	op(650,xfy,=>),  % implication
	op(680,xfy,<=>). % equivalence

% -----------------------------------------------------------------
%  nnf(+Fml,?NNF)
%
% Fml is a first-order formula and NNF its Skolemized negation 
% normal form.
%
% Syntax of Fml:
%  negation: '-', disj: 'v', conj: '&', impl: '=>', eqv: '<=>',
%  quant. 'all(X,<Formula>)', where 'X' is a prolog variable.
%
% Syntax of NNF: negation: '-', disj: ';', conj: ',', quant.:
%  'all(X,<Formula>)', where 'X' is a prolog variable.
%
% Example:  nnf(ex(Y, all(X, (f(Y) => f(X)))),NNF).
%           NNF =  all(_A,(-(f(all(X,f(ex)=>f(X))));f(_A)))) ?

nnf(Fml,NNF) :- nnf(Fml,[],NNF,_).

% -----------------------------------------------------------------
%  nnf(+Fml,+FreeV,-NNF,-Paths)
%
% Fml,NNF:    See above.
% FreeV:      List of free variables in Fml.
% Paths:      Number of disjunctive paths in Fml.

nnf_pre_clean(Atomic,Atomic,[]):-atomic(Atomic),!.
nnf_pre_clean(Atomic,Atomic,[]):-isVar(Atomic),!.
nnf_pre_clean(pttp(A),AA,Vars):- !,nnf_pre_clean(A,AA,Vars).
nnf_pre_clean([A|B],[AA|BB],Vars):-
   nnf_pre_clean(A,AA,Vars1),
   nnf_pre_clean(B,BB,Vars2),
   append(Vars1,Vars2,Vars).
nnf_pre_clean(C,CC,Vars):-
   C=..[A|B],
   nnf_pre_clean_functor(A,AA,Vars1),
   nnf_pre_clean(B,BB,Vars2),
   append(Vars1,Vars2,Vars),
   CC=..[AA|BB],!.

nnf_pre_clean_functor(and,(,),[]).
nnf_pre_clean_functor(or,(;),[]).
nnf_pre_clean_functor(~,(-),[]).
nnf_pre_clean_functor(not,(-),[]).
nnf_pre_clean_functor(implies,(->),[]).
nnf_pre_clean_functor(imp,(->),[]).
nnf_pre_clean_functor(forall,(all),[]).
nnf_pre_clean_functor(exists,(ex),[]).
nnf_pre_clean_functor(A,A,[]).

nnf(Fml,FreeV,NNF,Paths):-
   nnf_pre_clean(Fml,Clean,FreeV),
   nnf_clean(Clean,FreeV,NNF,Paths).

nnf_clean(Fml,FreeV,NNF,Paths) :-   
	(Fml = -(-A)      -> Fml1 = A;
	 Fml = -all(X,F)  -> Fml1 = ex(X,-F);
	 Fml = -ex(X,F)   -> Fml1 = all(X,-F);
	 Fml = -(A v B)   -> Fml1 = (-A & -B);
	 Fml = -(A & B)   -> Fml1 = (-A v -B);
	 Fml = (A => B)   -> Fml1 = (-A v B);
	 Fml = -(A => B)  -> Fml1 = A & -B;
	 Fml = (A <=> B)  -> Fml1 = (A & B) v (-A & -B);
	 Fml = -(A <=> B) -> Fml1 = (A & -B) v (-A & B)),!,
	nnf_clean(Fml1,FreeV,NNF,Paths).

nnf_clean(all(X,F),FreeV,all(X,NNF),Paths) :- !,
	nnf_clean(F,[X|FreeV],NNF,Paths).

nnf_clean(ex(X,Fml),FreeV,NNF,Paths) :- !,
	copy_term((X,Fml,FreeV),(sk(X,Fml),Fml1,FreeV)),
	nnf_clean(Fml1,FreeV,NNF,Paths).

nnf_clean((A & B),FreeV,(NNF1,NNF2),Paths) :- !,
	nnf_clean(A,FreeV,NNF1,Paths1),
	nnf_clean(B,FreeV,NNF2,Paths2),
	Paths is Paths1 * Paths2.

nnf_clean((A v B),FreeV,NNF,Paths) :- !,
	nnf_clean(A,FreeV,NNF1,Paths1),
	nnf_clean(B,FreeV,NNF2,Paths2),
	Paths is Paths1 + Paths2,
	(Paths1 > Paths2 -> NNF = (NNF2;NNF1);
		            NNF = (NNF1;NNF2)).

nnf_clean(Lit,_,Lit,1).


pttp(X) :-
	timed_call(pttp1(X,Y),'PTTP to Prolog translation'),
	!,
	timed_call(pttp2(Y),'Prolog compilation'),
	!.
%%% ***
%%% ****f* PTTP/prove
%%% DESCRIPTION
%%%   prove(Goal) can be used to prove Goal using code compiled by PTTP.
%%%   It uses depth-first iterative-deepening search and prints the proof.
%%%
%%%   Depth-first iterative-deepening search can be controlled
%%%   by extra paramaters of the prove predicate:
%%%      prove(Goal,Max,Min,Inc,ProofIn)
%%%   Max is the maximum depth to search (defaults to a big number),
%%%   Min is the minimum depth to search (defaults to 0),
%%%   Inc is the amount to increment the bound each time (defaults to 1).
%%%   ProofIn specifies initial steps of proof to retrace (defaults to []).
%%%
%%%   A query can be compiled along with the axioms by including the
%%%   clause 'query :- ...'.  The query can then be proved by 'prove(query)'.
%%% SOURCE

prove(Goal,Max,Min,Inc,ProofIn,ProofOut) :-
	expand_input_proof(ProofIn,PrfEnd),
	PrevInc is Min + 1,
	add_args(Goal,_,_,[],_,_,[],[],DepthIn,DepthOut,[PrfEnd|PrfEnd],ProofOut1,Goal1,_),
	!,
	timed_call(search(Goal1,Max,Min,Inc,PrevInc,DepthIn,DepthOut),'Proof'),
	contract_output_proof(ProofOut1,ProofOut),
	write_proof(ProofOut1),
	nl.

prove(Goal,Max,Min,Inc,ProofIn) :-
	prove(Goal,Max,Min,Inc,ProofIn,_).

prove(Goal,Max,Min,Inc) :-
	prove(Goal,Max,Min,Inc,[],_).

prove(Goal,Max,Min) :-
	prove(Goal,Max,Min,1,[],_).

prove(Goal,Max) :-
	prove(Goal,Max,0,1,[],_).

prove(Goal) :-!,
	prove(Goal,100,0,1,[],_).
prove(Goal) :-
	prove(Goal,1000000,0,1,[],_).

%%% ***

%%% ****if* PTTP/linearize
%%% DESCRIPTION
%%%   Prolog's unification operation is unsound for first-order
%%%   reasoning because it lacks the occurs check that would
%%%   block binding a variable to a term that contains the
%%%   variable and creating a circular term.  However, Prolog's
%%%   unification algorithm is sound and the occurs check is
%%%   unnecessary provided the terms being unified have no
%%%   variables in common and at least one of the terms has
%%%   no repeated variables.  A Prolog fact or rule head will
%%%   not have variables in common with the goal.  The linearize
%%%   transformation rewrites a fact or rule so that the fact
%%%   or rule head has no repeated variables and Prolog unification
%%%   can be used safely.  The rest of the unification can then
%%%   be done in the body of the transformed clause, using
%%%   the sound unify predicate.
%%%
%%%   For example,
%%%      p(X,Y,f(X,Y)) :- true.
%%%   is transformed into
%%%      p(X,Y,f(X1,Y1)) :- unify(X,X1), unify(Y,Y1).
%%% SOURCE

linearize(TermIn,TermOut,VarsIn,VarsOut,MatchesIn,MatchesOut) :-
	nonvar(TermIn) ->
		functor(TermIn,F,N),
		pttp_functor(TermOut,F,N),
		linearize_args(TermIn,TermOut,VarsIn,VarsOut,
		               MatchesIn,MatchesOut,1,N);
	identical_member(TermIn,VarsIn) ->
		VarsOut = VarsIn,
		conjoin(MatchesIn,unify(TermIn,TermOut),MatchesOut);
	%true ->
		TermOut = TermIn,
		VarsOut = [TermIn|VarsIn],
		MatchesOut = MatchesIn.

linearize_args(TermIn,TermOut,VarsIn,VarsOut,MatchesIn,MatchesOut,I,N) :-
	I > N ->
		VarsOut = VarsIn,
		MatchesOut = MatchesIn;
	%true ->
		arg(I,TermIn,ArgI),
		linearize(ArgI,NewArgI,VarsIn,Vars1,MatchesIn,Matches1),
		arg(I,TermOut,NewArgI),
		I1 is I + 1,
		linearize_args(TermIn,TermOut,Vars1,VarsOut,Matches1,MatchesOut,I1,N).
%%% ***
%%% ****if* PTTP/unify
%%% DESCRIPTION
%%%   Prolog's unification operation is unsound for first-order
%%%   reasoning because it lacks the occurs check that would
%%%   block binding a variable to a term that contains the
%%%   variable and creating a circular term.  Thus, PTTP
%%%   must provide a sound unfication algorithm with the occurs
%%%   check.
%%%
%%%   unify(X,Y) is similar to Prolog's X=Y, except that operations
%%%   like unify(X,f(X)) fail rather than create circular terms.
%%% SOURCE

unify(X,Y) :-!, unify_with_occurs_check(X,Y).

unify(X,Y) :-
	var(X) ->
		(var(Y) ->
			X = Y;
		%true ->
			functor(Y,_,N),
			(N = 0 ->
				true;
			N = 1 ->
				arg(1,Y,Y1), not_occurs_in(X,Y1);
			%true ->
				not_occurs_in_args(X,Y,N)),
			X = Y);
	var(Y) ->
		functor(X,_,N),
		(N = 0 ->
			true;
		N = 1 ->
			arg(1,X,X1), not_occurs_in(Y,X1);
		%true ->
			not_occurs_in_args(Y,X,N)),
		X = Y;
	%true ->
		functor(X,F,N),
		functor(Y,F,N),
		(N = 0 ->
			true;
		N = 1 ->
			arg(1,X,X1), arg(1,Y,Y1), unify(X1,Y1);
		%true ->
			unify_args(X,Y,N)).

unify_args(X,Y,N) :-
	N = 2 ->
		arg(2,X,X2), arg(2,Y,Y2), unify(X2,Y2),
		arg(1,X,X1), arg(1,Y,Y1), unify(X1,Y1);
	%true ->
		arg(N,X,Xn), arg(N,Y,Yn), unify(Xn,Yn),
		N1 is N - 1, unify_args(X,Y,N1).
%%% ***
%%% ****if* PTTP/not_occurs_in
%%% DESCRIPTION
%%%   not_occurs_in(Var,Term) fails if variable Var occurs inside
%%%   Term and succeeds otherwise.
%%% SOURCE

not_occurs_in(Var,Term) :-
	Var == Term ->
		fail;
	var(Term) ->
		true;
	%true ->
		functor(Term,_,N),
		(N = 0 ->
			true;
		N = 1 ->
			arg(1,Term,Arg1), not_occurs_in(Var,Arg1);
		%true ->
			not_occurs_in_args(Var,Term,N)).

not_occurs_in_args(Var,Term,N) :-
	N = 2 ->
		arg(2,Term,Arg2), not_occurs_in(Var,Arg2),
		arg(1,Term,Arg1), not_occurs_in(Var,Arg1);
	%true ->
		arg(N,Term,ArgN), not_occurs_in(Var,ArgN),
		N1 is N - 1, not_occurs_in_args(Var,Term,N1).
%%% ***

%%% ****if* PTTP/test_and_decrement_search_cost_expr
%%% SOURCE

test_and_decrement_search_cost_expr(DepthIn,Cost,Depth1,Expr) :-
	Cost == 0 ->
		Depth1 = DepthIn,
		Expr = true;
	%true ->
		Expr = (DepthIn >= Cost , Depth1 is DepthIn - Cost).
%%% ***
%%% ****if* PTTP/search_cost
%%% DESCRIPTION
%%%   Search cost is ordinarily computed by counting literals in the body.
%%%   It can be given explicitly instead by including a number, as in
%%%     p :- search_cost(3).     (ordinarily, cost would be 0)
%%%     p :- search_cost(1),q,r. (ordinarily, cost would be 2)
%%%     p :- search_cost(0),s.   (ordinarily, cost would be 1)
%%% SOURCE

search_cost(Body,HeadArgs,N) :-
	Body = search_cost(M) ->
		N = M;
	Body = (A , B) ->
		(A = search_cost(M) ->	% if first conjunct is search_cost(M),
			N = M;		% search cost of conjunction is M
		%true ->
			search_cost(A,HeadArgs,N1),
			search_cost(B,HeadArgs,N2),
			N is N1 + N2);
	Body = (A ; B) ->
		search_cost(A,HeadArgs,N1),
		search_cost(B,HeadArgs,N2),
		min(N1,N2,N);
	builtin(Body) ->
		N = 0;
	%true ->
		N = 1.
%%% ***
%%% ****if* PTTP/search
%%% DESCRIPTION
%%%   Search driver predicate.
%%%   Note that depth-bounded execution of Goal is enabled by
%%%   the fact that the DepthIn and DepthOut arguments of
%%%   search are also the DepthIn and DepthOut arguments of Goal.
%%% SOURCE

search(_Goal,Max,Min,_Inc,_PrevInc,_DepthIn,_DepthOut) :-
	Min > Max,
	!,
	fail.
search(Goal,_Max,Min,_Inc,PrevInc,DepthIn,DepthOut) :-
        write_search_progress(Min),
	DepthIn = Min,
	call(Goal),
	DepthOut < PrevInc.	% fail if solution found previously
search(Goal,Max,Min,Inc,_PrevInc,DepthIn,DepthOut) :-
	Min1 is Min + Inc,
	search(Goal,Max,Min1,Inc,Inc,DepthIn,DepthOut).
%%% ***

%%% ****if* PTTP/make_wrapper
%%% SOURCE

make_wrapper(_DefinedPreds,[query,0],true) :-
	!.
make_wrapper(DefinedPreds,[P,N],Result) :-
	functor(Goal,P,N),
	Goal =.. [P|Args],
	ExtraArgs = [PosAncestors,NegAncestors,DepthIn,DepthOut,ProofIn,ProofOut],
	list_append(Args,ExtraArgs,Args1),
	Head =.. [P|Args1],
	internal_functor(P,IntP),
	list_length(ExtraArgs,NExtraArgs),
	NN is N + NExtraArgs + 1,
	(identical_member([IntP,NN],DefinedPreds) ->
	        list_append(ExtraArgs,[GoalAtom],ExtraArgs2),
		list_append(Args,ExtraArgs2,Args2),
		IntHead =.. [IntP|Args2];
	%true ->
		IntHead = fail),
	(negative_functor(P) ->
		negated_literal(Goal,PosGoal),
		Red = redn,  % atom in proof is negation of actual literal
		C1Ancestors = NegAncestors,
		C2Ancestors = PosAncestors;
	%true ->
		PosGoal = Goal,
		Red = red,
		C1Ancestors = PosAncestors,
		C2Ancestors = NegAncestors),
	(N = 0 ->	% special case for propositional calculus
		V1 = (identical_member(GoalAtom,C2Ancestors) , !);
	%true ->
		V1 = ((identical_member(GoalAtom,C2Ancestors) , !);
		       unifiable_member(GoalAtom,C2Ancestors))),
	V2 = (DepthOut = DepthIn,
		 ProofIn = [Prf,[Red,GoalAtom,PosAncestors,NegAncestors]|PrfEnd],
		 ProofOut = [Prf|PrfEnd]),
	conjoin(V1,V2,Reduce),
	Result = (Head :- GoalAtom = PosGoal,
		  	  (identical_member(GoalAtom,C1Ancestors) ->
			   	fail;
			  %true ->
			   	(Reduce;
				 IntHead))).
%%% ***
%%% ****if* PTTP/query
%%% DESCRIPTION
%%%   query uses the following definition instead of the more complex one
%%%   that would be created by make_wrapper
%%% SOURCE

query(PosAncestors,NegAncestors,DepthIn,DepthOut,ProofIn,ProofOut) :-
	int_query(PosAncestors,NegAncestors,DepthIn,DepthOut,ProofIn,ProofOut,query).
%%% ***
%%% ****if* PTTP/unifiable_member
%%% DESCRIPTION
%%%   unifiable_member(X,L) succeeds each time X is unifiable with an
%%%   element of the list L
%%% SOURCE

unifiable_member(X,[Y|_]) :-
	unify(X,Y).
unifiable_member(X,[_|L]) :-
	unifiable_member(X,L).
%%% ***
%%% ****if* PTTP/identical_member
%%% DESCRIPTION
%%%   identical_member(X,L) succeeds iff X is an element of the list L
%%%   it does not use unification during element comparisons
%%% SOURCE

identical_member(X,[Y|_])  :-
	X == Y,
	!.
identical_member(X,[_|L]) :-
	identical_member(X,L).
%%% ***

%%% ****if* PTTP/write_proof
%%% DESCRIPTION
%%%   write_proof prints the proof that PTTP finds
%%% SOURCE

write_proof(Proof) :-
	write('Proof:'),
	nl,
	proof_length(Proof,Len),
	write('length = '),
	write(Len),
	write(', '),
	proof_depth(Proof,Depth),
	write('depth = '),
	write(Depth),
	nl,
	write('Goal#  Wff#  Wff Instance'),
	nl,
	write('-----  ----  ------------'),
	add_proof_query_line(Proof,Proof2),
	process_proof(Proof2,0,Proof1),
	write_proof1(Proof1),
	nl,
	write('Proof end.').

write_proof1([]).
write_proof1([[LineNum,X,Head,Depth,Subgoals]|Y]) :-
	nl,
	write_indent_for_number(LineNum),
	write('['),
	write(LineNum),
	write(']  '),
	write_indent_for_number(X),
	write(X),
	write('   '),
	write_proof_indent(Depth),
	write(Head),
	(Subgoals = [] ->
		true;
	%true ->
		write(' :- '),
		write_proof_subgoals(Subgoals)),
	write(.),
	write_proof1(Y).	

write_proof_subgoals([X,Y|Z]) :-
	write('['),
	write(X),
	write('] , '),
	write_proof_subgoals([Y|Z]).
write_proof_subgoals([X]) :-
	write('['),
	write(X),
	write(']').

write_proof_indent(N) :-
	N > 0,
	write('   '),
	N1 is N - 1,
	write_proof_indent(N1).
write_proof_indent(0).

process_proof([Prf|PrfEnd],_LineNum,Result) :-
	Prf == PrfEnd,
	!,
	Result = [].
process_proof([[[X,Head,PosAncestors,NegAncestors]|Y]|PrfEnd],LineNum,Result) :-
	LineNum1 is LineNum + 1,
	process_proof([Y|PrfEnd],LineNum1,P),
	(Head == query ->
		Depth is 0;
	%true ->
		list_length(PosAncestors,N1),	% compute indentation to show
		list_length(NegAncestors,N2),	% level of goal nesting from
		Depth is N1 + N2 + 1),		% lengths of ancestor lists
	Depth1 is Depth + 1,
	collect_proof_subgoals(Depth1,P,Subgoals),
	(X = redn ->
		X1 = red,
		negated_literal(Head,Head1);
	 (number(X) , X < 0) ->
		X1 is - X,
		negated_literal(Head,Head1);
	%true ->
		X1 = X,
		Head1 = Head),
	Result = [[LineNum,X1,Head1,Depth,Subgoals]|P].

collect_proof_subgoals(_Depth1,[],Result) :-
	Result = [].
collect_proof_subgoals(Depth1,[[LineNum,_,_,Depth,_]|P],Result) :-
	Depth = Depth1,
	collect_proof_subgoals(Depth1,P,R),
	Result = [LineNum|R].
collect_proof_subgoals(Depth1,[[_,_,_,Depth,_]|P],Result) :-
	Depth > Depth1,
	collect_proof_subgoals(Depth1,P,Result).
collect_proof_subgoals(Depth1,[[_,_,_,Depth,_]|_],Result) :-
	Depth < Depth1,
	Result = [].

add_proof_query_line(Proof,Proof2) :-
	Proof = [Prf|_PrfEnd],
	nonvar(Prf),
	Prf = [[_,query,_,_]|_],
	!,
	Proof2 = Proof.
add_proof_query_line(Proof,Proof2) :-
	Proof = [Prf|PrfEnd],
	Proof2 = [[[0,query,[],[]]|Prf]|PrfEnd].
%%% ***

%%% ****if* PTTP/clauses
%%% DESCRIPTION
%%%   Negation normal form to Prolog clause translation.
%%%   Include a literal in the body of each clause to
%%%   indicate the number of the formula the clause came from.
%%% SOURCE

clauses((A , B),L,WffNum1,WffNum2) :-
	!,
	clauses(A,L1,WffNum1,W),
	clauses(B,L2,W,WffNum2),
	conjoin(L1,L2,L).

clauses(PNF,L,WffNum1,WffNum2):- once(make_matrix_pttp(PNF,Out)),clauses1(Out,L,WffNum1,WffNum2).

clauses1(A,L,WffNum1,WffNum2) :-
	write_clause_with_number(A,WffNum1),
	head_literals(A,Lits),
	clauses2(A,Lits,L,WffNum1),
	WffNum2 is WffNum1 + 1.

clauses2(A,[Lit|Lits],L,WffNum) :-
	body_for_head_literal(Lit,A,Body1),
	(Body1 == false ->
		L = true;
	%true ->
		conjoin(infer_by(WffNum),Body1,Body),
		clauses2(A,Lits,L1,WffNum),
		conjoin((Lit :- Body),L1,L)).
clauses2(_,[],true,_).

head_literals(Wff,L) :-
	Wff = (A :- B) ->	% contrapositives not made for A :- ... inputs
		head_literals(A,L);
	Wff = (A , B) ->
		head_literals(A,L1),
		head_literals(B,L2),
		list_union(L1,L2,L);
	Wff = (A ; B) ->
		head_literals(A,L1),
		head_literals(B,L2),
		list_union(L1,L2,L);
	%true ->
		L = [Wff].

body_for_head_literal(Head,Wff,Body) :-
	Wff = (A :- B) ->
		body_for_head_literal(Head,A,A1),
		conjoin(A1,B,Body);
	Wff = (A , B) ->
		body_for_head_literal(Head,A,A1),
		body_for_head_literal(Head,B,B1),
		disjoin(A1,B1,Body);
	Wff = (A ; B) ->
		body_for_head_literal(Head,A,A1),
		body_for_head_literal(Head,B,B1),
		conjoin(A1,B1,Body);
	Wff == Head ->
		Body = true;
	negated_literal(Wff,Head) ->
		Body = false;
	%true ->
		negated_literal(Wff,Body).
%%% ***
%%% ****if* PTTP/predicates
%%% DESCRIPTION
%%%   predicates returns a list of the predicates appearing in a formula.
%%% SOURCE

predicates(Wff,L) :-
	Wff = (A :- B) ->
		predicates(A,L1),
		predicates(B,L2),
		list_union(L2,L1,L);
	Wff = (A , B) ->
		predicates(A,L1),
		predicates(B,L2),
		list_union(L2,L1,L);
	Wff = (A ; B) ->
		predicates(A,L1),
		predicates(B,L2),
		list_union(L2,L1,L);
	builtin(Wff) ->
		L = [];
	%true ->
		functor(Wff,F,N),
		L = [[F,N]].
%%% ***
%%% ****if* PTTP/procedure
%%% DESCRIPTION
%%%   procedure returns a conjunction of the clauses
%%%   with head predicate P/N.
%%% SOURCE

procedure(P,N,Clauses,Proc) :-
	Clauses = (A , B) ->
		procedure(P,N,A,ProcA),
		procedure(P,N,B,ProcB),
		conjoin(ProcA,ProcB,Proc);
	(Clauses = (A :- B) , functor(A,P,N)) ->
		Proc = Clauses;
	%true ->
		Proc = true.

procedures([[P,N]|Preds],Clauses,Procs) :-
	procedure(P,N,Clauses,Proc),
	procedures(Preds,Clauses,Procs2),
	conjoin(Proc,Procs2,Procs).
procedures([],_Clauses,true).
%%% ***

%%% ****if* PTTP/add_features
%%% SOURCE

add_features((Head :- Body),(Head1 :- Body1)) :-
	(functor(Head,query,_) ->
		Head2 = Head,
		add_args(Body,yes,query,[],
		         PosAncestors,NegAncestors,
			 PosAncestors,NegAncestors,
		         DepthIn,DepthOut,
			 ProofIn,ProofOut,
			 Body1,_);
	%true ->
		linearize(Head,Head2,[],_,true,Matches),
		(negative_literal(Head) ->
			PosGoal = no;
		%true ->
			PosGoal = yes),
		Head =.. [_|HeadArgs],
		add_args(Body,PosGoal,GoalAtom,HeadArgs,
                         PosAncestors,NegAncestors,
			 NewPosAncestors,NewNegAncestors,
		         Depth1,DepthOut,
			 ProofIn,ProofOut,
			 Body2,New),
		(var(New) ->
			PushAnc = true;
		PosGoal = yes ->
			NewNegAncestors = NegAncestors,
			PushAnc = (NewPosAncestors = [GoalAtom|PosAncestors]);
		%true ->
			NewPosAncestors = PosAncestors,
			PushAnc = (NewNegAncestors = [GoalAtom|NegAncestors])),
		search_cost(Body,HeadArgs,Cost),
		test_and_decrement_search_cost_expr(DepthIn,Cost,Depth1,TestExp),
		conjoin(PushAnc,Body2,Body4),
		conjoin(Matches,Body4,Body5),
		conjoin(TestExp,Body5,Body1)),
    	Head2 =.. [P|L],
	internal_functor(P,IntP),
	list_append(L,[PosAncestors,NegAncestors,
		       DepthIn,DepthOut,
		       ProofIn,ProofOut,
		       GoalAtom],
		    L1),
	Head1 =.. [IntP|L1].
%%% ***
%%% ****if* PTTP/add_args
%%% SOURCE

add_args(Body,PosGoal,GoalAtom,HeadArgs,
         PosAncestors,NegAncestors,
	 NewPosAncestors,NewNegAncestors,
	 DepthIn,DepthOut,
	 ProofIn,ProofOut,
	 Body1,New) :-
	Body = (A , B) ->
		add_args(A,PosGoal,GoalAtom,HeadArgs,
                         PosAncestors,NegAncestors,
			 NewPosAncestors,NewNegAncestors,
		         DepthIn,Depth1,
			 ProofIn,Proof1,
		         A1,New),
		add_args(B,PosGoal,GoalAtom,HeadArgs,
		         PosAncestors,NegAncestors,
			 NewPosAncestors,NewNegAncestors,
		         Depth1,DepthOut,
			 Proof1,ProofOut,
                         B1,New),
		conjoin(A1,B1,Body1);
	Body = (A ; B) ->
		add_args(A,PosGoal,GoalAtom,HeadArgs,
		         PosAncestors,NegAncestors,
			 NewPosAncestors,NewNegAncestors,
		         DepthA,DepthOut,
			 ProofIn,ProofOut,
			 A2,New),
		add_args(B,PosGoal,GoalAtom,HeadArgs,
		         PosAncestors,NegAncestors,
			 NewPosAncestors,NewNegAncestors,
		         DepthB,DepthOut,
			 ProofIn,ProofOut,
			 B2,New),
		search_cost(A,HeadArgs,CostA),
		search_cost(B,HeadArgs,CostB),
		(CostA < CostB ->
			DepthA = DepthIn,
			Cost is CostB - CostA,
			test_and_decrement_search_cost_expr(DepthIn,Cost,DepthB,TestExp),
			A1 = A2,
			conjoin(TestExp,B2,B1);
		CostA > CostB ->
			DepthB = DepthIn,
			Cost is CostA - CostB,
			test_and_decrement_search_cost_expr(DepthIn,Cost,DepthA,TestExp),
			B1 = B2,
			conjoin(TestExp,A2,A1);
		%true ->
		        DepthA = DepthIn,
			DepthB = DepthIn,
			A1 = A2,
			B1 = B2),
		disjoin(A1,B1,Body1);
	functor(Body,search_cost,_) ->
		DepthOut = DepthIn,
		ProofOut = ProofIn,
		Body1 = true;
	Body = infer_by(N) ->
		DepthOut = DepthIn,
		(PosGoal = yes -> 
			N1 = N;
		%true ->  % atom in proof is negation of actual literal
			N1 is - N),
		Body1 = (ProofIn = [Prf,[N1,GoalAtom,PosAncestors,NegAncestors]|PrfEnd],
			 ProofOut = [Prf|PrfEnd]);
	Body = search_cost(N) ->
		DepthOut = DepthIn,
		ProofOut = ProofIn,
		Body1 = true;
	builtin(Body) ->
		DepthOut = DepthIn,
		ProofOut = ProofIn,
		Body1 = Body;
	%true ->
		Body =.. L,
		list_append(L,
                       [NewPosAncestors,NewNegAncestors,
		        DepthIn,DepthOut,
			ProofIn,ProofOut],
		       L1),
		Body1 =.. L1,
		New = yes.
%%% ***

%%% ****if* PTTP/pttp1
%%% SOURCE

pttp1(X,Y) :-
	nl,
	write('PTTP input formulas:'),
	clauses(X,X0,1,_),		% prints and transforms input clauses
	nl,
	apply_to_conjuncts(X0,add_features,X8),
	predicates(X8,IntPreds0),
	list_reverse(IntPreds0,IntPreds1),
	procedures(IntPreds1,X8,IntProcs),
	predicates(X0,Preds0),
	list_reverse(Preds0,Preds),
	apply_to_elements(Preds,make_wrapper(IntPreds1),Procs),
	conjoin(IntProcs,Procs,Y),
	!.
%%% ***
%%% ****if* PTTP/pttp2
%%% SOURCE

pttp_assert(Y):-assert(Y),copy_term(Y,YY),numbervars(YY),dmsg(pttp_assert(YY)).

pttp_assert(A,_) :-				% 2-ary predicate for use as
	pttp_assert(A).			% apply_to_conjuncts argument

pttp2(Y) :- 
	apply_to_conjuncts(Y,pttp_assert,_),!.

pttp2(Y) :-
%	nl,
%	write('PTTP output formulas:'),
%	apply_to_conjuncts(Y,write_clause,_),
%	nl,
	nl,
	tell('pttp_temp.pl'),
	apply_to_conjuncts(Y,write_clause,_),
	nl,
	told,
	compile('pttp_temp.pl'),
	nl,
	!.
%%% ***
%%% ****if* PTTP/expand_input_proof
%%% SOURCE

expand_input_proof([],_Proof).
expand_input_proof([N|L],[[N|_]|L1]) :-
	expand_input_proof(L,L1).
%%% ***
%%% ****if* PTTP/contract_output_proof
%%% SOURCE

contract_output_proof([Prf|PrfEnd],Proof) :-
	Prf == PrfEnd,
	!,
	Proof = [].
contract_output_proof([[[N,_,_,_]|L]|PrfEnd],[N|L1]) :-
	contract_output_proof([L|PrfEnd],L1).
%%% ***
%%% ****if* PTTP/proof_length
%%% SOURCE

proof_length([Prf|PrfEnd],N) :-
	Prf == PrfEnd,
	!,
	N = 0.
proof_length([[[_,X,_,_]|L]|PrfEnd],N) :-
	proof_length([L|PrfEnd],N1),
	(X == query -> N is N1; N is N1 + 1).
%%% ***
%%% ****if* PTTP/proof_depth
%%% SOURCE

proof_depth([Prf|PrfEnd],N) :-
	Prf == PrfEnd,
	!,
	N = 0.
proof_depth([[[_,_,PosAnc,NegAnc]|L]|PrfEnd],N) :-
	proof_depth([L|PrfEnd],N1),
	list_length(PosAnc,N2),
	list_length(NegAnc,N3),
	N4 is N2 + N3,
	max(N1,N4,N).
%%% ***

%%% ****if* PTTP/pttp_functor
%%% DESCRIPTION
%%%   Sometimes the `functor' predicate doesn't work as expected and
%%%   a more comprehensive predicate is needed.  The pttp_functor'
%%%   predicate overcomes the problem of functor(X,13,0) causing
%%%   an error in Symbolics Prolog.  You may need to use it if
%%%   `functor' in your Prolog system fails to construct or decompose
%%%   terms that are numbers or constants.
%%% SOURCE

pttp_functor(Term,F,N) :-
	nonvar(F),
	atomic(F),
	N == 0,
	!,
	Term = F.
pttp_functor(Term,F,N) :-
	nonvar(Term),
	atomic(Term),
	!,
	F = Term,
	N = 0.
pttp_functor(Term,F,N) :-
	functor(Term,F,N).
%%% ***
%%% ****if* PTTP/list_append
%%% SOURCE

list_append([X|L1],L2,[X|L3]) :-
	list_append(L1,L2,L3).
list_append([],L,L).
%%% ***
%%% ****if* PTTP/list_reverse
%%% SOURCE

list_reverse(L1,L2) :-
	revappend(L1,[],L2).

revappend([X|L1],L2,L3) :-
	revappend(L1,[X|L2],L3).
revappend([],L,L).
%%% ***
%%% ****if* PTTP/list_union
%%% SOURCE

list_union([X|L1],L2,L3) :-
	identical_member(X,L2),
	!,
	list_union(L1,L2,L3).
list_union([X|L1],L2,[X|L3]) :-
	list_union(L1,L2,L3).
list_union([],L,L).
%%% ***
%%% ****if* PTTP/list_length
%%% SOURCE

list_length([_X|L],N) :-
	list_length(L,N1),
	N is N1 + 1.
list_length([],0).
%%% ***
%%% ****if* PTTP/min
%%% SOURCE

min(X,Y,Min) :-
	X =< Y ->
		Min = X;
	%true ->
		Min = Y.
%%% ***
%%% ****if* PTTP/max
%%% SOURCE

max(X,Y,Max) :-
	X =< Y ->
		Max = Y;
	%true ->
		Max = X.
%%% ***
%%% ****if* PTTP/conjoin
%%% SOURCE

conjoin(A,B,C) :-
	A == true ->
		C = B;
	B == true ->
		C = A;
	A == false ->
		C = false;
	B == false ->
		C = false;
	%true ->
		C = (A , B).
%%% ***
%%% ****if* PTTP/disjoin
%%% SOURCE

disjoin(A,B,C) :-
	A == true ->
		C = true;
	B == true ->
		C = true;
	A == false ->
		C = B;
	B == false ->
		C = A;
	%true ->
		C = (A ; B).
%%% ***
%%% ****if* PTTP/negated_functor
%%% SOURCE
negated_functor(-,_):-!,trace,fail.
negated_functor(~,_):-!,trace,fail.
negated_functor(F,NotF) :-
	name(F,L),
	name(not_,L1),
	(list_append(L1,L2,L) ->
		true;
	%true ->
		list_append(L1,L,L2)),
	name(NotF,L2).
%%% ***
%%% ****if* PTTP/negated_literal
%%% SOURCE

negated_literal(-A,B):-negated_literal(A,AA),!,negated_literal(AA,B).
negated_literal(A,B):-var(B),!,negated_literal_0(A,B).
negated_literal(A,-B):-negated_literal(B,BB),!,negated_literal_0(A,B).
negated_literal(A,B):-negated_literal_0(A,B).
negated_literal_0(Lit,NotLit) :-
	Lit =.. [F1|L1],
	negated_functor(F1,F2),
	(var(NotLit) ->
		NotLit =.. [F2|L1];
	%true ->
	       
               ( NotLit =.. [F2|L2],
		L1 == L2) ).
%%% ***
%%% ****if* PTTP/negative_functor
%%% SOURCE

negative_functor(F) :-
	name(F,L),
	name(not_,L1),
	list_append(L1,_,L).
%%% ***
%%% ****if* PTTP/negative_literal
%%% SOURCE

negative_literal(Lit) :-
	functor(Lit,F,_),
	negative_functor(F).
%%% ***
%%% ****if* PTTP/internal_functor
%%% SOURCE

internal_functor(P) :-
	name(P,L),
	name(int_,L1),
	list_append(L1,_,L).

internal_functor(P,IntP) :-
	name(P,L),
	name(int_,L1),
	list_append(L1,L,L2),
	name(IntP,L2).
%%% ***
%%% ****if* PTTP/apply_to_conjuncts
%%% SOURCE

apply_to_conjuncts(Wff,P,Wff1) :-
	Wff = (A , B) ->
		apply_to_conjuncts(A,P,A1),
		apply_to_conjuncts(B,P,B1),
		conjoin(A1,B1,Wff1);
	%true ->
		P =.. G,
		list_append(G,[Wff,Wff1],G1),
		T1 =.. G1,
		call(T1).
%%% ***
%%% ****if* PTTP/apply_to_elements
%%% SOURCE

apply_to_elements([X|L],P,Result) :-
	P =.. G,
	list_append(G,[X,X1],G1),
	T1 =.. G1,
	call(T1),
	apply_to_elements(L,P,L1),
	conjoin(X1,L1,Result).
apply_to_elements([],_,true).

%%% ***
%%% ****if* PTTP/write_clause
%%% SOURCE

write_clause(A) :-
	nl,
	write(A),
	write(.).

write_clause(A,_) :-				% 2-ary predicate for use as
	write_clause(A).			% apply_to_conjuncts argument
%%% ***
%%% ****if* PTTP/write_clause_with_number
%%% SOURCE

write_clause_with_number(A,WffNum) :-
	nl,
	write_indent_for_number(WffNum),
	write(WffNum),
	write('  '),
	write(A),
	write(.).

write_indent_for_number(N) :-
	((number(N) , N <  100) -> write(' ') ; true),
	((number(N) , N <   10) -> write(' ') ; true).
%%% ***

%%% ****if* PTTP/timed_call
%%% DESCRIPTION
%%%   A query can be timed by timed_call(query,'Proof').
%%% NOTES
%%%   assumes that statistics(cputime,T) binds T to run-time in seconds
%%%   different Prolog systems have different ways to get this information
%%% SOURCE

timed_call(X,Type) :-
	statistics(cputime,T1),			%SWI Prolog
	(call(X) -> V = success ; V = failure),	%SWI Prolog
	statistics(cputime,T2),			%SWI Prolog
	Secs is T2 - T1,			%SWI Prolog
%	statistics(runtime,[T1,_]),		%Quintus/SICStus Prolog
%	(call(X) -> V = success ; V = failure),	%Quintus/SICStus Prolog
%	statistics(runtime,[T2,_]),		%Quintus/SICStus Prolog
%	Secs is (T2 - T1) / 1000.0,		%Quintus/SICStus Prolog
	nl,
	write(Type),
	write(' time: '),
	write(Secs),
	write(' seconds'),
	nl,
	V = success.
%%% ***
%%% ****if* PTTP/write_search_progress
%%% SOURCE

write_search_progress(Level) :-
	nl,
	write('search for cost '),
	write(Level),
	write(' proof...  '),
	current_output(S),
	flush_output(S).
%%% ***

%%% ****if* PTTP/builtin
%%% DESCRIPTION
%%%   List of builtin predicates that can appear in clause bodies.
%%%   No extra arguments are added for ancestor goals or depth-first
%%%   iterative-deepening search.  Also, if a clause body is
%%%   composed entirely of builtin goals, the head is not saved
%%%   as an ancestor for use in reduction or pruning.
%%%   This list can be added to as required.
%%% SOURCE

builtin(T) :-
	functor(T,F,N),
	builtin(F,N).

builtin(!,0).
builtin(true,0).
builtin(false,0).
builtin(fail,0).
builtin(succeed,0).
builtin(trace,0).
builtin(atom,1).
builtin(integer,1).
builtin(number,1).
builtin(atomic,1).
builtin(constant,1).
builtin(functor,3).
builtin(arg,3).
builtin(var,1).
builtin(nonvar,1).
builtin(call,1).
builtin(=,2).
builtin(\=,2).
builtin(==,2).
builtin(\==,2).
builtin(=\=,2).
builtin(>,2).
builtin(<,2).
builtin(>=,2).
builtin(=<,2).
builtin(is,2).
builtin(display,1).
builtin(write,1).
builtin(nl,0).
builtin(infer_by,_).
builtin(search_cost,_).
builtin(test_and_decrement_search_cost,_).
builtin(unify,_).
builtin(identical_member,_).
builtin(unifiable_member,_).
%%% ***



%%% ****h* PTTP/PTTP-dalit
%%% COPYRIGHT
%%%   Copyright (c) 1988-2003 Mark E. Stickel, SRI International, Menlo Park, CA 94025  USA
%%% 
%%%   Permission is hereby granted, free of charge, to any person obtaining a
%%%   copy of this software and associated documentation files (the "Software"),
%%%   to deal in the Software without restriction, including without limitation
%%%   the rights to use, copy, modify, merge, publish, distribute, sublicense,
%%%   and/or sell copies of the Software, and to permit persons to whom the
%%%   Software is furnished to do so, subject to the following conditions:
%%% 
%%%   The above copyright notice and this permission notice shall be included
%%%   in all copies or substantial portions of the Software.
%%% 
%%%   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%%   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%%%   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
%%%   IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
%%%   CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
%%%   TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
%%%   SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%% DESCRIPTION
%%%  
%%%      A Prolog Technology Theorem Prover
%%%  
%%%               Mark E. Stickel
%%%  
%%%   This file contains changes to PTTP to use
%%%   depth-first iterative deepening dalit_search with bound on
%%%   D_Alit (maximum number of A-literals on a branch)
%%%   instead of
%%%   D_Inf (total number of subgoals).
%%%
%%%   To use, load pttp and then load this file
%%%   to replace changed definitions.
%%% SOURCE

dalit_prove(Goal,Max,Min,Inc,ProofIn,ProofOut) :-
	expand_input_proof(ProofIn,PrfEnd),
	PrevInc is Min + 1,
	dalit_add_args(Goal,_,_,[],_,_,[],[],DepthIn,[PrfEnd|PrfEnd],ProofOut1,Goal1,_),
	!,
	timed_call(dalit_search(Goal1,Max,Min,Inc,PrevInc,DepthIn),'Proof'),
	contract_output_proof(ProofOut1,ProofOut),
	write_proof(ProofOut1),
	nl.

dalit_prove(Goal,Max,Min,Inc,ProofIn) :-
	dalit_prove(Goal,Max,Min,Inc,ProofIn,_).

dalit_prove(Goal,Max,Min,Inc) :-
	dalit_prove(Goal,Max,Min,Inc,[],_).

dalit_prove(Goal,Max,Min) :-
	dalit_prove(Goal,Max,Min,1,[],_).

dalit_prove(Goal,Max) :-
	dalit_prove(Goal,Max,0,1,[],_).

dalit_prove(Goal) :-
	dalit_prove(Goal,10000,0,1,[],_).

dalit_search_cost(Body,HeadArgs,N) :-
	Body = dalit_search_cost(M) ->
		N = M;
	Body = (A , B) ->
		(A = dalit_search_cost(M) ->	% if first conjunct is dalit_search_cost(M),
			N = M;		% dalit_search cost of conjunction is M
		%true ->
			dalit_search_cost(A,HeadArgs,N1),
			dalit_search_cost(B,HeadArgs,N2),
			max(N1,N2,N));
	Body = (A ; B) ->
		dalit_search_cost(A,HeadArgs,N1),
		dalit_search_cost(B,HeadArgs,N2),
		min(N1,N2,N);
	builtin(Body) ->
		N = 0;
	%true ->
		N = 1.

dalit_search(_Goal,Max,Min,_Inc,_PrevInc,_DepthIn) :-
	Min > Max,
	!,
	fail.
dalit_search(Goal,_Max,Min,_Inc,PrevInc,DepthIn) :-
        write_search_progress(Min),
	DepthIn = Min,
	call(Goal),
	true.			% should fail if solution found previously
dalit_search(Goal,Max,Min,Inc,_PrevInc,DepthIn) :-
	Min1 is Min + Inc,
	dalit_search(Goal,Max,Min1,Inc,Inc,DepthIn).

dalit_make_wrapper(_DefinedPreds,[dalit_query,0],true) :-
	!.
dalit_make_wrapper(DefinedPreds,[P,N],Result) :-
	functor(Goal,P,N),
	Goal =.. [P|Args],
	ExtraArgs = [PosAncestors,NegAncestors,DepthIn,ProofIn,ProofOut],
	list_append(Args,ExtraArgs,Args1),
	Head =.. [P|Args1],
	internal_functor(P,IntP),
	list_length(ExtraArgs,NExtraArgs),
	NN is N + NExtraArgs + 1,
	(identical_member([IntP,NN],DefinedPreds) ->
	        list_append(ExtraArgs,[GoalAtom],ExtraArgs2),
		list_append(Args,ExtraArgs2,Args2),
		IntHead =.. [IntP|Args2];
	%true ->
		IntHead = fail),
	(negative_functor(P) ->
		negated_literal(Goal,PosGoal),
		Red = redn,  % atom in proof is negation of actual literal
		C1Ancestors = NegAncestors,
		C2Ancestors = PosAncestors;
	%true ->
		PosGoal = Goal,
		Red = red,
		C1Ancestors = PosAncestors,
		C2Ancestors = NegAncestors),
	(N = 0 ->	% special case for propositional calculus
		V1 = (identical_member(GoalAtom,C2Ancestors) , !);
	%true ->
		V1 = ((identical_member(GoalAtom,C2Ancestors) , !);
		       unifiable_member(GoalAtom,C2Ancestors))),
	V2 = (
		 ProofIn = [Prf,[Red,GoalAtom,PosAncestors,NegAncestors]|PrfEnd],
		 ProofOut = [Prf|PrfEnd]),
	conjoin(V1,V2,Reduce),
	Result = (Head :- GoalAtom = PosGoal,
		  	  (identical_member(GoalAtom,C1Ancestors) ->
			   	fail;
			  %true ->
			   	(Reduce;
				 IntHead))).

dalit_query(PosAncestors,NegAncestors,DepthIn,ProofIn,ProofOut) :-
	int_query(PosAncestors,NegAncestors,DepthIn,ProofIn,ProofOut,dalit_query).

dalit_add_features((Head :- Body),(Head1 :- Body1)) :-
	(functor(Head,dalit_query,_) ->
		Head2 = Head,
		dalit_add_args(Body,yes,dalit_query,[],
		         PosAncestors,NegAncestors,
			 PosAncestors,NegAncestors,
		         DepthIn,
			 ProofIn,ProofOut,
			 Body1,_);
	%true ->
		linearize(Head,Head2,[],_,true,Matches),
		(negative_literal(Head) ->
			PosGoal = no;
		%true ->
			PosGoal = yes),
		Head =.. [_|HeadArgs],
		dalit_add_args(Body,PosGoal,GoalAtom,HeadArgs,
                         PosAncestors,NegAncestors,
			 NewPosAncestors,NewNegAncestors,
		         Depth1,
			 ProofIn,ProofOut,
			 Body2,New),
		(var(New) ->
			PushAnc = true;
		PosGoal = yes ->
			NewNegAncestors = NegAncestors,
			PushAnc = (NewPosAncestors = [GoalAtom|PosAncestors]);
		%true ->
			NewPosAncestors = PosAncestors,
			PushAnc = (NewNegAncestors = [GoalAtom|NegAncestors])),
		dalit_search_cost(Body,HeadArgs,Cost),
		test_and_decrement_search_cost_expr(DepthIn,Cost,Depth1,TestExp),
		conjoin(PushAnc,Body2,Body4),
		conjoin(Matches,Body4,Body5),
		conjoin(TestExp,Body5,Body1)),
    	Head2 =.. [P|L],
	internal_functor(P,IntP),
	list_append(L,[PosAncestors,NegAncestors,
		       DepthIn,
		       ProofIn,ProofOut,
		       GoalAtom],
		    L1),
	Head1 =.. [IntP|L1].

dalit_add_args(Body,PosGoal,GoalAtom,HeadArgs,
         PosAncestors,NegAncestors,
	 NewPosAncestors,NewNegAncestors,
	 DepthIn,
	 ProofIn,ProofOut,
	 Body1,New) :-
	Body = (A , B) ->
		dalit_add_args(A,PosGoal,GoalAtom,HeadArgs,
                         PosAncestors,NegAncestors,
			 NewPosAncestors,NewNegAncestors,
		         DepthIn,
			 ProofIn,Proof1,
		         A1,New),
		dalit_add_args(B,PosGoal,GoalAtom,HeadArgs,
		         PosAncestors,NegAncestors,
			 NewPosAncestors,NewNegAncestors,
			 DepthIn,
			 Proof1,ProofOut,
                         B1,New),
		conjoin(A1,B1,Body1);
	Body = (A ; B) ->
		unimplemented;
	functor(Body,dalit_search_cost,_) ->
		ProofOut = ProofIn,
		Body1 = true;
	Body = infer_by(N) ->
		(PosGoal = yes -> 
			N1 = N;
		%true ->  % atom in proof is negation of actual literal
			N1 is - N),
		Body1 = (ProofIn = [Prf,[N1,GoalAtom,PosAncestors,NegAncestors]|PrfEnd],
			 ProofOut = [Prf|PrfEnd]);
	Body = dalit_search_cost(N) ->
		ProofOut = ProofIn,
		Body1 = true;
	builtin(Body) ->
		ProofOut = ProofIn,
		Body1 = Body;
	%true ->
		Body =.. L,
		list_append(L,
                       [NewPosAncestors,NewNegAncestors,
			DepthIn,
			ProofIn,ProofOut],
		       L1),
		Body1 =.. L1,
		New = yes.
%%% ***
:-do_pttp_test(_X).
:-prolog.

