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

nnf((A,B),(C,D)):- nonvar(A), !, nnf(A,C), nnf(B,D).
nnf(Fml,NNFOUT) :- nnf(Fml,[],NNF,_),NNFOUT=NNF.

:-      op(400,fy,-),    % negation
	op(500,xfy,&),   % conjunction
	op(600,xfy,v),   % disjunction
	op(650,xfy,=>),  % implication
	op(680,xfy,<=>). % equivalence


% -----------------------------------------------------------------
%  nnf(+Fml,+FreeV,-NNF,-Paths)
%
% Fml,NNF:    See above.
% FreeV:      List of free variables in Fml.
% Paths:      Number of disjunctive paths in Fml.

nnf_pre_clean(_Type,Atomic,Atomic,[]):-atomic(Atomic),!.
nnf_pre_clean(_Type,Atomic,Atomic,[]):-isVar(Atomic),!.
nnf_pre_clean(Type,pttp(A),AA,Vars):- !,nnf_pre_clean(Type,A,AA,Vars).
nnf_pre_clean(Type,[A|B],[AA|BB],Vars):-!,
   nnf_pre_clean(Type,A,AA,Vars1),
   nnf_pre_clean(Type,B,BB,Vars2),
   append(Vars1,Vars2,Vars).

nnf_pre_clean(_Type,C,CC,Vars):-
   C=..[A|B],
   logical_functor_pttp(A),!,
   nnf_pre_clean_functor(A,AA,Vars1),!,
   nnf_pre_clean(sent,B,BB,Vars2),
   append(Vars1,Vars2,Vars),
   CC=..[AA|BB],!.

nnf_pre_clean(Type,CIN,CC,Vars):-
   Type == sent,
   correct_lit(CIN,C),
   C=..[A|B],
   nnf_pre_clean_functor(A,AA,Vars1),!,
   nnf_pre_clean(arg,B,BB,Vars2),
   append(Vars1,Vars2,Vars),
   CC=..[AA|BB],!.

nnf_pre_clean(Type,C,CC,Vars):-
   C=..[A|B],
   nnf_pre_clean_functor(A,AA,Vars1),!,
   nnf_pre_clean(Type,B,BB,Vars2),
   append(Vars1,Vars2,Vars),
   CC=..[AA|BB],!.


nnf_post_clean(Atomic,Atomic,[]):-atomic(Atomic),!.
nnf_post_clean(Atomic,Atomic,[]):-isVar(Atomic),!.
nnf_post_clean(pttp(A),AA,Vars):- !,nnf_post_clean(A,AA,Vars).
nnf_post_clean(-(A),NN,Vars):- !,nnf_post_clean(A,AA,Vars),negated_literal(AA,NN).
nnf_post_clean([A|B],[AA|BB],Vars):-
   nnf_post_clean(A,AA,Vars1),
   nnf_post_clean(B,BB,Vars2),
   append(Vars1,Vars2,Vars).
nnf_post_clean(C,CC,Vars):-
   C=..[A|B],
   A=AA,
   nnf_post_clean(B,BB,Vars),
   CC=..[AA|BB],!.

:-export(logical_functor_pttp/1).

logical_functor_pttp(X):-not(atom(X)),!,fail.
logical_functor_pttp(X):-nnf_pre_clean_functor(A,B,_),(X==A;X==B),!.
logical_functor_pttp(&).
logical_functor_pttp(~).
logical_functor_pttp(<=>).
logical_functor_pttp(=>).
logical_functor_pttp(v).

nnf_pre_clean_functor(and,(,),[]).
nnf_pre_clean_functor(or,(;),[]).
nnf_pre_clean_functor('::',(:),[]).
nnf_pre_clean_functor(~,(-),[]).
nnf_pre_clean_functor(not,(-),[]).
nnf_pre_clean_functor(implies,(=>),[]).
nnf_pre_clean_functor(imp,(=>),[]).
nnf_pre_clean_functor(equiv,(<=>),[]).
%nnf_pre_clean_functor(->,(=>),[]).
nnf_pre_clean_functor(entailed_from,(:-),[]).
nnf_pre_clean_functor(implied_by,(:-),[]).
nnf_pre_clean_functor(forAll,(all),[]).
nnf_pre_clean_functor(thereExists,(ex),[]).
nnf_pre_clean_functor(forall,(all),[]).
nnf_pre_clean_functor(exists,(ex),[]).
nnf_pre_clean_functor(A,A,[]):-atom(A).
nnf_pre_clean_functor(A,A,[]).

nnf(Fml,FreeV,CleanNNF,Paths):-
   nnf_pre_clean(sent,Fml,Clean,FreeV),
   nnf_clean(Clean,FreeV,NNF,Paths),
   nnf_post_clean(NNF,CleanNNF,FreeV).

nnf_clean(Atomic,_,Atomic,1):-atomic(Atomic),!.
nnf_clean(Atomic,_,Atomic,1):-isVar(Atomic),!.
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

