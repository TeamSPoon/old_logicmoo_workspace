%%% ****h* PTTP/PTTP TESTING INTERFACE
%%% 
%%% 
%%% 
%%% 
%%% 
%%% 
%%% 
%%% 

:- '@'(ensure_loaded('../../src_lib/logicmoo_util/logicmoo_util_all'),user).
:- include(dbase_i_header).
:- ensure_loaded(dbase_i_mpred_pttp).

:- multifile(pttp_test/2).
:- discontiguous(pttp_test/2).
:- multifile(pttp_logic/2).
:- discontiguous(pttp_logic/2).

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

:-export(chang_lee_example3/0).
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
%%%   In a group with left genlInverse and left identity
%%%   every element has a right genlInverse.
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
	((
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

pttp_test(chang_lee_example6,
	((
		p(e,X,X),
		p(X,e,X),
		p(X,i(X),e),
		p(i(X),X,e),
		s(b),
		(s(Z) :- s(X) , s(Y) , p(X,i(Y),Z)),
		(p(U,Z,W) :- p(X,Y,U) , p(Y,Z,V) , p(X,V,W)),
		(p(X,V,W) :- p(X,Y,U) , p(Y,Z,V) , p(U,Z,W)),
		(query :- s(i(b)))
	))).

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

pttp_test(chang_lee_example7,
	((
		p(a),
		m(a,s(c),s(b)),
		m(X,X,s(X)),
		(not_m(X,Y,Z) ; m(Y,X,Z)),
		(not_m(X,Y,Z) ; d(X,Z)),
		(not_p(X) ; not_m(Y,Z,U) ; not_d(X,U) ; d(X,Y) ; d(X,Z)),
		(query :- d(a,b))
	))).

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

% pttp_test_fails_is_ok(chang_lee_example8).
pttp_test(chang_lee_example8,
	((
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
	))).

%%% ***
%%% ****f* PTTP_Examples/chang_lee_example9
%%% DESCRIPTION
%%%   There exist infinitely many primes.
%%% NOTES
%%%   this is problem NUM016-2 in TPTP
%%% SOURCE

pttp_test(chang_lee_example9,
	((
		l(X,f(X)),
		not_l(X,X),
		(not_l(X,Y) ; not_l(Y,X)),
		(not_d(X,f(Y)) ; l(Y,X)),
		(p(X) ; d(h(X),X)),
		(p(X) ; p(h(X))),
		(p(X) ; l(h(X),X)),
		(not_p(X) ; not_l(a,X) ; l(f(a),X)),	% negation of query
		(query :- p(X) , l(a,X) , not_l(f(a),X))
	))).

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

% pttp_test(_,_):- !,fail.

pttp_test_skipped(overbeek_example4a,     
	((
		(p(e(X,e(e(Y,e(Z,X)),e(Z,Y))))),
		((p(Y) :- p(e(X,Y)), p(X))),
		((queryXXX :- p(e(e(e(a,e(b,c)),c),e(b,a))))),
                ((query:- call(prove(queryXXX,100,0,2))))
	))).


pttp_test_fails_is_ok(overbeek_example4).
pttp_test(overbeek_example4,     
	((
		(p(e(X,e(e(Y,e(Z,X)),e(Z,Y))))),
		((p(Y) :- p(e(X,Y)), p(X))),
		((query :- p(e(e(e(a,e(b,c)),c),e(b,a)))))
	))).

%%% ***
pttp_test_query(overbeek_example4,prove(query,100,0,2)).	% cost 30 proof
%%% ***



pttp_test(logicmoo_example1,
	((
          mudMother(iJoe,iSue),
          (mudMother(X,Y) => isa(Y,tFemale)),
          (mudChild(Y,X) => (mudMother(X,Y);mudFather(X,Y))),          
          (query:- isa(Y,tFemale))
	))).



pttp_test(logicmoo_example1_holds,
	((
          asserted_t(mudMother,iJoe,iSue),
          (asserted_t(mudMother,X,Y) => isa(Y,tFemale)),
          (asserted_t(mudChild,Y,X) => (proven_t(mudMother,X,Y);proven_t(mudFather,X,Y))),          
          (query:- isa(Y,tFemale))
	))).

pttp_logic(logicmoo_prules,
        ((      
         uses_logic(logicmoo_kb_refution),
           (mudMother(X,Y) => isa(Y,tFemale)),
           (mudChild(Y,X) => (mudMother(X,Y);mudFather(X,Y))),  
           (asserted_t(mudMother,X,Y) => isa(Y,tFemale)),
           (asserted_t(mudChild,Y,X) => (proven_t(mudMother,X,Y);proven_t(mudFather,X,Y)))))).


%  kbholds(mudChild,iGun,iSonOfGun,A,B,C,C,D,E,F):- D=[G,[1,F,A,B]|H],E=[G|H].
%  not_kbholds(tFemale,iGun,A,B,C,C,D,E,F):- D=[G,[-2,F,A,B]|H],E=[G|H].
%  query(A,B,C,D,E,F,G):- (E=[H,[3,query,A,B]|I],J=[H|I]),asserted_t(K,iSonOfGun,iGun,A,B,C,D,J,F).
%  asserted_t(A,B,C,D,E,F,G,H,I):- J=asserted_t(A,B,C), (identical_member(J,D)->fail; (identical_member(J,E),!;unifiable_member(J,E)),G=F,H=[K,[red,J,D,E]|L],I=[K|L];kbholds(A,B,C,D,E,F,G,H,I,J)).
%  not_kbholds(A,B,C,D,E,F,G,H):- I=asserted_t(A,B), (identical_member(I,D)->fail; (identical_member(I,C),!;unifiable_member(I,C)),F=E,G=[J,[redn,I,C,D]|K],H=[J|K];not_kbholds(A,B,C,D,E,F,G,H,I)).
pttp_test(logicmoo_example2,
	((         
          uses_logic(logicmoo_prules),
          mudMother(iJoe,iSue),
           asserted_t(mudChild,iGun,iSonOfGun),
          (-isa(iGun,tFemale)),
          (query:- proven_t(What,iSonOfGun,iGun))
          % What = mudFather
	))):-What='$VAR'('?MUD-FATHER').

pttp_test(logicmoo_example22,
	((          
          uses_logic(logicmoo_prules),
           asserted_t(mudChild,iGun,iSonOfGun),
          (-isa(iGun,tFemale)),
          (query:- proven_t(What,iSonOfGun,iGun))
          % What = mudFather
	))):-What='$VAR'('?MUD-FATHER').


% not_firstOrder(tFemale, iGun,E,F,A,B,C,G,D) :- test_and_decrement_search_cost(A, 0, B), C=[H, [-4, D, E, F]|I], G=[H|I].
% firstOrder(Pred,Arg1,Arg2,E,F,A,B,C,G,D) :- call_prop_val2(Pred,Arg1,Arg2), test_and_decrement_search_cost(A, 0, B), C=[H, [3, D, E, F]|I], G=[H|I].


% pttp_test_fails_is_ok(logicmoo_example3).
pttp_test(logicmoo_example3,
	((
          uses_logic(logicmoo_kb_logic),
          pred_t(genlInverse,mudParent,mudChild),
          pred_t(genlPreds,mudMother,mudParent),
          pred_isa_t(predIrreflexive,mudChild),         
          asserted_t(mudParent, iSon1, iFather1),
        (query:- (refuted_t(mudChild, iSon1, iFather1)))
          
          % Expected true
	))).




% pttp_test_fails_is_ok(logicmoo_example4).
pttp_test(logicmoo_example4,
	((
          uses_logic(logicmoo_kb_logic),
          pred_t(genlInverse,mudParent,mudChild),
          pred_t(genlPreds,mudMother,mudParent),
          pred_isa_t(predIrreflexive,mudChild),         
          asserted_t(mudParent, iSon1, iFather1),
          (query:- -(assertable_t(mudChild, iSon1, iFather1)))
          % Expected true
	))).


pttp_logic(Name,Data):- pttp_test(Name,Data).

pttp_logic(logicmoo_kb_logic,
          ((
           uses_logic(logicmoo_kb_refution),

          (( pred_t(genls,C1,C2) & pred_isa_t(C1,P) => pred_isa_t(C2,P) )),
          (( pred_t(genls,C1,C2) & isa(I,C1) => isa(I,C2) )),

          (( pred_t(disjointWith,C1,C2) =>  pred_isa_t(C1,P) v pred_isa_t(C2,P) )),
          (( pred_t(disjointWith,C1,C2) =>  isa(I,C1) v isa(I,C2) )),

          (( pred_t(genlPreds,P,PSuper) & proven_t(P,A,B) => proven_t(PSuper,A,B) )),
          (( pred_t(genlPreds,P,PSuper) & refuted_t(PSuper,A,B) => refuted_t(P,A,B) )),
          (( pred_t(genlInverse,P,PSuper) & proven_t(P,A,B) => proven_t(PSuper,B,A) )),
          (( pred_t(negationPreds,P,PSuper) & proven_t(P,A,B) => refuted_t(PSuper,A,B) )),
          (( pred_t(negationInverse,P,PSuper) & proven_t(P,A,B) => refuted_t(PSuper,B,A) )),

      %    (( pred_isa_t(predTransitive,P) & proven_t(P,A,B) & proven_t(P,B,C) => proven_t(P,A,C) )),
      %    (( pred_isa_t(predReflexive,P) & proven_t(P,A,B) => proven_t(P,A,A) & proven_t(P,B,B) )),
      %    (( pred_isa_t(predSymmetric,P) & proven_t(P,A,B) => proven_t(P,B,A)  ))
          (( pred_isa_t(predIrreflexive,P) &  proven_t(P,A,B) => refuted_t(P,B,A) ))
       %   (( pred_isa_t(predIrreflexive,PSuper) & pred_t(genlInverse,PSuper,P) => pred_isa_t(predIrreflexive,P) )),
       %   (( pred_isa_t(predIrreflexive,PSuper) & pred_t(genlPreds,P,PSuper) => pred_isa_t(predIrreflexive,P) ))
       ))).


% all questions (askable_t) to the KB are 

pttp_logic(logicmoo_kb_refution,
          ((
           
           % TODO define askable_t
           (( asserted_t(P,A,B) => proven_t(P,A,B) )),
           (( proven_t(P,A,B) => assumed_t(P,A,B) )),
           (( assumed_t(P,A,B) => -refuted_t(P,A,B) & -fallacy_t(P,A,B)  )),
           (( assertable_t(P,A,B) => -refuted_t(P,A,B) & -fallacy_t(P,A,B)  )),            

           (( proven_t(P,A,B) & refuted_t(P,A,B) => fallacy_t(P,A,B) )),
           
           (( proven_t(P,A,B) =>  -refuted_t(P,A,B) & assertable_t(P,A,B) & -unknowable_t(P,A,B) )),
           (( refuted_t(P,A,B) => -proven_t(P,A,B) & -assertable_t(P,A,B) & -unknowable_t(P,A,B) )),
           (( askable_t(P,A,B) => proven_t(P,A,B) v unknowable_t(P,A,B) v refuted_t(P,A,B)  )),
           (( answerable_t(P,A,B) <=> -unknowable_t(P,A,B) )),
           (( askable_t(P,A,B) <=> -fallacy_t(P,A,B) )),
           (( answerable_t(P,A,B) => proven_t(P,A,B) v refuted_t(P,A,B)  )),
           (( proven_t(P,A,B) v unknowable_t(P,A,B) v refuted_t(P,A,B)  ))

/*
         (( fallacy_t(P,A,B) => refuted_t(P,A,B) & proven_t(P,A,B) & -unknowable_t(P,A,B) & -assertable_t(P,A,B) )),   
         
           (( unknowable_t(P,A,B) =>  -proven_t(P,A,B) & assertable_t(P,A,B) & -asserted_t(P,A,B) & -refuted_t(P,A,B) )),
         
            (( -unknowable_t(P,A,B) => proven_t(P,A,B) v refuted_t(P,A,B)  )),
            (( -asserted_t(P,A,B) => assertable_t(P,A,B) v refuted_t(P,A,B) v fallacy_t(P,A,B) )),
            (( -proven_t(P,A,B) =>  refuted_t(P,A,B) v fallacy_t(P,A,B) v assertable_t(P,A,B) )),
            (( -assertable_t(P,A,B) => refuted_t(P,A,B) v fallacy_t(P,A,B) )),
            (( -refuted_t(P,A,B) => fallacy_t(P,A,B) v unknowable_t(P,A,B) v proven_t(P,A,B) )),
            (( -fallacy_t(P,A,B) =>  unknowable_t(P,A,B) v refuted_t(P,A,B) v proven_t(P,A,B) ))

            */

          %  (( askable_t(P,A,B) v fallacy_t(P,A,B) )),

       ))).

:- foreach(pttp_logic(N,_),(asserta_if_new(N:- pttp_load_wid(N)),export(N/0))).
:- foreach(pttp_test(N,_),(asserta_if_new(N:- do_pttp_test(N)),export(N/0))).

:- if_startup_script(do_pttp_tests).

end_of_file.

%%% Examples

e :- e1,e2,e3,e4,e5,e6,e7,e8,e9,
	chang_lee_example1, chang_lee_example2.

e1 :-
	clear,
	axiom((a or b) and ~(a)),
	prove_goal(b),
	\+ prove_goal(a),
	prove(a or b).

e2 :- clear,
	axioms(
	[rev([],[]),
	(rev([X|Xs],Zs) <= rev(Xs,Ys) and append(Ys,[X],Zs)),
	append([],Xs,Xs),
	(append([X|Xs],Ys,[X|Zs]) <= append(Xs,Ys,Zs))
    ]),
	prove(rev([a,b,c],[c,b,a])),
	\+ prove(rev([a,b,c],[c,b,a,z])),
	e2(30,497).

%nrev([],[]).
%nrev([X|Xs],Zs) :- nrev(Xs,Ys),appn(Ys,[X],Zs).
%appn([],Xs,Xs).
%appn([X|Xs],Ys,[X|Zs]) :- appn(Xs,Ys,Zs).

%% Naive reverse.  The recursion gets quite deep, which means that
%% checking the ancestor list will be  expensive.  On such
%% problems, it would almost certainly pay out to use an indexed
%% ancestor list.  Remember that nrev(30)
%% takes 496 inferences, add one for query, and we get 497.  Starting
%% with a bound of 497 results in an answer in .27 sec (about 10*
%% slower than interpreted prolog).  Each Iterative deepening step
%% increases the bound by only 2 or 3.  This means that a bound of 496
%% takes .52 sec (.25+.27) and 494 takes .76sec.  It is the last
%% portion of the tree that is really expensive, so even a bound of
%% 400 takes 23.7sec.  The moral: set the initial bound right the
%% first time!

e2(N,B) :-
	length(X,N),
	numbervars(X,0,_),
	timed(prove_goal(rev(X,Y),B,100000)),
	reverse(X,Y).

%% the assert's are done directly because the clausal form reorders goals.
e3 :-
	clear,
	assertz(rule(len([],0),[],0)),
	assertz(rule(len([_A|B],N),[len(B,N1),N is N1 + 1],2)),
	prove(X^len([a,b],X)),
	X == 2.

e4 :- 
	clear,
	axioms(
	[q(X) => p(X),
	q(2),
	q(1)]),
	bagof(X,prove(X^p(X)),L),
	(L = [1,2] ; L = [2,1]).

e5_1(0).
e5_1(N) :- N > 0, axiom(p(N)),N1 is N-1, e5_1(N1).

%% This is much faster than I expected.  e5(100) takes about 1
%% second (ss10-3) including converting to clausal form and asserting
%% the rules.

%% Almost all of the time here is in the bagof or findall.  If we just
%% fail, then it is almost immediate for N=100.  For QP,findall is

e5 :- e5(5).
e5(N) :-
    clear, e5_1(N),
    axiom(q(X) <= p(X)),
    axiom(r(X,Y) <= p(X) and q(Y)),
    %%bagof(r(X,Y),prove([X,Y]^r(X,Y)),L),
    bagof(r(X,Y),prove_goal(r(X,Y)),L),
    length(L,Len),
    Len =:= N*N.

e6 :- clear,
	axioms(
	[p(X) <= apply(format,['APPLY: ~p~n',[X]]) and q(X),
	q(1),
	q(2)]),
	bagof(X,prove(X^p(X)),L),
	format('Should have done APPLY on ~p.~n',[L]).

e7 :- clear,
	axiom(a or b or c),
	axiom(~c),
	prove(a or b),
	\+ prove(a or c).

e8 :- clear,
	axiom(a or b or c),
	prove(a or b or c),
	\+ prove(a or b),
	\+ prove(b or c),
	\+ prove(a or c).

e9 :- clear,
	prove(~(a and b) <=> ~a or ~b).

e10 :- clear,
	axiom(p(X) or q(X) <= r(X)),
	axiom(r(a) and r(b)),
	prove(X^(p(X) or q(X))),
	\+ prove(p(a)),
	\+ prove(p(b)),
	\+ prove(q(a)),
	\+ prove(q(b)),
	\+ prove(p(a) or q(b)),
	prove(p(a) or q(a)),
	prove(p(b) or q(b)).


:- int_listing_wid.

end_of_file.

?- listing_wid.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   logicmoo_kb_refution:0
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% pttp_in: asserted_t(M29, N29, O29)=>proven_t(M29, N29, O29).

not_asserted_t(A, B, C) :-
        not_proven_t(A, B, C).

proven_t(A, B, C) :-
        asserted_t(A, B, C).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   logicmoo_kb_refution:1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% pttp_in: assertable_t(M29, N29, O29)=> -refuted_t(M29, N29, O29)& -fallacy_t(M29, N29, O29).

not_assertable_t(_, _, _).

not_refuted_t(A, B, C) :-
        assertable_t(A, B, C).

not_fallacy_t(A, B, C) :-
        assertable_t(A, B, C).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   logicmoo_kb_refution:2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% pttp_in: proven_t(M29, N29, O29)&refuted_t(M29, N29, O29)=>fallacy_t(M29, N29, O29).

not_both_t(proven_t(A, B, C), refuted_t(_, _, _)) :-
        not_fallacy_t(A, B, C).

fallacy_t(A, B, C) :-
        proven_t(A, B, C),
        refuted_t(A, B, C).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   logicmoo_kb_refution:3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% pttp_in: proven_t(M29, N29, O29)=> -refuted_t(M29, N29, O29)& (assertable_t(M29, N29, O29)& -unknowable_t(M29, N29, O29)).

not_proven_t(_, _, _).

not_refuted_t(A, B, C) :-
        proven_t(A, B, C).

assertable_t(A, B, C) :-
        proven_t(A, B, C).

not_unknowable_t(A, B, C) :-
        proven_t(A, B, C).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   logicmoo_kb_refution:4
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% pttp_in: refuted_t(M29, N29, O29)=> -proven_t(M29, N29, O29)& (-assertable_t(M29, N29, O29)& -unknowable_t(M29, N29, O29)).

not_refuted_t(_, _, _).

not_proven_t(A, B, C) :-
        refuted_t(A, B, C).

not_assertable_t(A, B, C) :-
        refuted_t(A, B, C).

not_unknowable_t(A, B, C) :-
        refuted_t(A, B, C).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   logicmoo_kb_refution:5
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% pttp_in: askable_t(M29, N29, O29)v fallacy_t(M29, N29, O29).

askable_t(A, B, C) :-
        not_fallacy_t(A, B, C).

fallacy_t(A, B, C) :-
        not_askable_t(A, B, C).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   logicmoo_kb_refution:6
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% pttp_in: askable_t(M29, N29, O29)=>proven_t(M29, N29, O29)v (unknowable_t(M29, N29, O29)v refuted_t(M29, N29, O29)).

not_askable_t(A, B, C) :-
        not_proven_t(A, B, C),
        not_unknowable_t(A, B, C),
        not_refuted_t(A, B, C).

proven_t(A, B, C) :-
        askable_t(A, B, C),
        not_unknowable_t(A, B, C),
        not_refuted_t(A, B, C).

unknowable_t(A, B, C) :-
        askable_t(A, B, C),
        not_proven_t(A, B, C),
        not_refuted_t(A, B, C).

refuted_t(A, B, C) :-
        askable_t(A, B, C),
        not_proven_t(A, B, C),
        not_unknowable_t(A, B, C).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   logicmoo_kb_refution:7
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% pttp_in: answerable_t(M29, N29, O29)<=> -unknowable_t(M29, N29, O29).

answerable_t(A, B, C) :-
        not_unknowable_t(A, B, C).

not_unknowable_t(A, B, C) :-
        answerable_t(A, B, C).

not_answerable_t(A, B, C) :-
        unknowable_t(A, B, C).

unknowable_t(A, B, C) :-
        not_answerable_t(A, B, C).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   logicmoo_kb_refution:8
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% pttp_in: askable_t(M29, N29, O29)<=> -fallacy_t(M29, N29, O29).

askable_t(A, B, C) :-
        not_fallacy_t(A, B, C).

not_fallacy_t(A, B, C) :-
        askable_t(A, B, C).

not_askable_t(A, B, C) :-
        fallacy_t(A, B, C).

fallacy_t(A, B, C) :-
        not_askable_t(A, B, C).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   logicmoo_kb_refution:9
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% pttp_in: answerable_t(M29, N29, O29)=>proven_t(M29, N29, O29)v refuted_t(M29, N29, O29).

not_answerable_t(A, B, C) :-
        not_proven_t(A, B, C),
        not_refuted_t(A, B, C).

proven_t(A, B, C) :-
        answerable_t(A, B, C),
        not_refuted_t(A, B, C).

refuted_t(A, B, C) :-
        answerable_t(A, B, C),
        not_proven_t(A, B, C).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   logicmoo_kb_refution:10
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% pttp_in: proven_t(M29, N29, O29)v (unknowable_t(M29, N29, O29)v refuted_t(M29, N29, O29)).

proven_t(A, B, C) :-
        not_unknowable_t(A, B, C),
        not_refuted_t(A, B, C).

unknowable_t(A, B, C) :-
        not_proven_t(A, B, C),
        not_refuted_t(A, B, C).

refuted_t(A, B, C) :-
        not_proven_t(A, B, C),
        not_unknowable_t(A, B, C).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   logicmoo_kb_logic:1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% pttp_in: pred_t(typeSubclass, M29, N29)&pred_isa_t(M29, O29)=>pred_isa_t(N29, O29).

not_both_t(pred_t(typeSubclass, _, A), pred_isa_t(_, B)) :-
        not_pred_isa_t(A, B).

pred_isa_t(A, C) :-
        pred_t(typeSubclass, B, A),
        pred_isa_t(B, C).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   logicmoo_kb_logic:2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% pttp_in: pred_t(typeSubclass, M29, N29)&isa(O29, M29)=>isa(O29, N29).

not_both_t(pred_t(typeSubclass, _, B), isa(A, _)) :-
        not_mudIsa(A, B).

isa(B, A) :-
        pred_t(typeSubclass, C, A),
        isa(B, C).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   logicmoo_kb_logic:3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% pttp_in: pred_t(disjointWith, M29, N29)=>pred_isa_t(M29, O29)v pred_isa_t(N29, O29).

not_pred_t(disjointWith, A, B) :-
        not_pred_isa_t(A, C),
        not_pred_isa_t(B, C).

pred_isa_t(A, C) :-
        pred_t(disjointWith, A, B),
        not_pred_isa_t(B, C).

pred_isa_t(A, C) :-
        pred_t(disjointWith, B, A),
        not_pred_isa_t(B, C).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   logicmoo_kb_logic:4
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% pttp_in: pred_t(disjointWith, M29, N29)=>isa(O29, M29)v isa(O29, N29).

not_pred_t(disjointWith, A, C) :-
        not_mudIsa(B, A),
        not_mudIsa(B, C).

isa(B, A) :-
        pred_t(disjointWith, A, C),
        not_mudIsa(B, C).

isa(B, A) :-
        pred_t(disjointWith, C, A),
        not_mudIsa(B, C).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   logicmoo_kb_logic:5
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% pttp_in: pred_t(genlPreds, M29, N29)&proven_t(M29, O29, P29)=>proven_t(N29, O29, P29).

not_both_t(pred_t(genlPreds, _, A), proven_t(_, B, C)) :-
        not_proven_t(A, B, C).

proven_t(A, C, D) :-
        pred_t(genlPreds, B, A),
        proven_t(B, C, D).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   logicmoo_kb_logic:6
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% pttp_in: pred_t(genlInverse, M29, N29)&proven_t(M29, O29, P29)=>proven_t(N29, P29, O29).

not_both_t(pred_t(genlInverse, _, A), proven_t(_, C, B)) :-
        not_proven_t(A, B, C).

proven_t(A, D, C) :-
        pred_t(genlInverse, B, A),
        proven_t(B, C, D).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   logicmoo_kb_logic:7
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% pttp_in: pred_t(negationPreds, M29, N29)&proven_t(M29, O29, P29)=>refuted_t(N29, O29, P29).

not_both_t(pred_t(negationPreds, _, A), proven_t(_, B, C)) :-
        not_refuted_t(A, B, C).

refuted_t(A, C, D) :-
        pred_t(negationPreds, B, A),
        proven_t(B, C, D).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   logicmoo_kb_logic:8
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% pttp_in: pred_t(negationInverse, M29, N29)&proven_t(M29, O29, P29)=>refuted_t(N29, P29, O29).

not_both_t(pred_t(negationInverse, _, A), proven_t(_, C, B)) :-
        not_refuted_t(A, B, C).

refuted_t(A, D, C) :-
        pred_t(negationInverse, B, A),
        proven_t(B, C, D).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   logicmoo_kb_logic:9
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% pttp_in: pred_isa_t(predIrreflexive, M29)&proven_t(M29, N29, O29)=>refuted_t(M29, O29, N29).

not_both_t(pred_isa_t(predIrreflexive, A), proven_t(_, C, B)) :-
        not_refuted_t(A, B, C).

refuted_t(A, C, B) :-
        pred_isa_t(predIrreflexive, A),
        proven_t(A, B, C).

true.

?-

