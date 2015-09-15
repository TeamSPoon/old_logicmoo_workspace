/******************************************************************/
/* foil.pl            Last modification: Thu May 26 11:04:38 1994 */
/* Quinlan's First-Order Inductive Learning of relational concepts*/
/******************************************************************/
%
%    Copyright 1991 by John M. Zelle and Raymond J. Mooney
%
%    Permission to use this software is granted subject to the
%    following restrictions and understandings:
%
%    1. This material is for educational and research purposes only.
%
%    2. Raymond J. Mooney has provided this software AS IS. Raymond
%       J. Mooney has made no warranty or representation that the
%       operation of this software will be error-free, and he is
%       under no obligation to provide any services, by way of
%       maintenance, update, or otherwise.  
%
%    3. Any user of such software agrees to indemnify and hold
%       harmless Raymond J. Mooney and The University of Texas from
%	all claims arising out of the use or misuse of this
%	software, or arising out of any accident, injury, or damage
%	whatsoever, and from all costs, counsel fees and liabilities
%       incurred in or about any such claim, action, or proceeding
%       brought thereon. 
%
%    4. Users are requested, but not required, to inform Raymond J.
%	Mooney of any noteworthy uses of this software.
%
%    5. All materials and reports developed as a consequence of the
%       use of this software shall duly acknowledge such use, in
%	accordance with the usual standards of acknowledging credit
%	in academic research.
%
/******************************************************************/
/*                                                                */
/*  impl. by    : John M. Zelle                                   */
/*                1992                                            */
/*                                                                */
/*  reference   : Learning Logical Definitions from Relations,    */
/*                Quinlan, J. R., Machine Learning, 5, 1990.      */
/*                                                                */
/*                Determinate Literals in Inductive Logic         */
/*                Programming, Quinlan, J. R., Proceedings of the */
/*                Eighth International Workshop in Machine        */
/*                Learning, 1991                                  */
/*                                                                */
/*  call        : foil(Name/Arity),                               */
/*                foil(Predicate),                                */
/*                                                                */
/*  parameter   : foil_predicates/1, foil_use_negations/1,        */
/* 	          foil_det_lit_bound/1, foil_cwa/1                */
/*                                                                */
/******************************************************************/
/* The version presented here is somewhat simplified in that it   */
/* uses a much weaker test to constrain recursive predicates (a   */
/* recursive call must contain vars not found in the head of a    */
/* clause, and may not introduce any unbound vars), and it does   */
/* not incorporate encoding length restrictions to handle noisy   */
/* data. There is also no post-processing of clauses to simplify  */ 
/* learned definitions, although this would be relatively easy to */
/* add.                                                           */
/*                                                                */
/* This is a very simple implementation which recomputes tuple    */
/* sets "on the fly".  Don't expect it to run like the wind.      */
/*                                                                */
/* The background knowledge for predicate induction is represented*/
/* as "existential" predicates. By existential it is meant that   */
/* the definitions there must be fully constructive to avoid      */ 
/* instantiation errors when running FOIL. The input data file    */
/* needs to be preceeded by the following header (see also the    */
/* example files):                                                */
/*                                                                */
/* 	foil_predicates(<FunctorList>).                           */
/* 	foil_cwa(<Boolean>).                                      */
/* 	foil_use_negations(<Boolean>).                            */
/* 	foil_det_lit_bound(<Integer>).                            */
/*                                                                */
/* were foil_predicates(<FunctorList>) declares all predicates    */
/* occuring in FunctorList, which are used in the example facts.  */
/* If foil should use the closed world assumption, foil_cwa must  */
/* be set to true. Otherwise, explicitly asserted negative facts  */
/* (for the relation in question) are used if they exist. Whether */
/* Foil should use negated literals in the body of generated      */
/* clauses can be switched on or off with the fact                */
/* foil_use_negations(<Boolean>), its argument can be either true */
/* or false. And foil_det_lit_bound(<Integer>) is used as depth   */
/* limit on the search for determinate literals.                  */
/******************************************************************/
% TH: Sat Jan 15 16:05:29 1994  Corrected a variable binding bug 
%                               in generate_possible_extensions/2
%                               Modified output slightly and made
%                               foil Quintus independent.
%     Fri Apr  8 20:04:14 1994  Added switch for cwa/'explicite
% 				negative examples' 
%     Thu May 26 17:41:30 1994  Modified choose_tie_clause, so that 
%				'foil_4.pl' will be processed
%				correctly. 
%
% TH: Note, it seems that the member example used by Quinlan 90
%     'foil_3.pl' is erroneous. This implementation cannot produce the
%     correct member definition from it. However, if the example is
%     reformulated (as in 'foil_4.pl'), the correct member definition
%     can be learned. 

% If you use Quintus-Prolog you can use these libs, but than you
% need to comment out the predicates at the end of the file.
% :- ensure_loaded(library(occurs)).
% :- ensure_loaded(library(basics)).
% :- ensure_loaded(library(lists)).
% :- ensure_loaded(library(between)).
% :- ensure_loaded(library(math)).
% :- use_module(library(ordsets),[ord_add_element/3]).

% This is Quintus-Prolog specific
% time(X) :-
%	statistics(runtime,[T|_]).

% This is YAP- and SWI-Prolog specific

log(X,Y) :- 
	Y is log(X).


/******************************************************************/
/*                                                                */
/*  call        : foil (+PREDICATE)                               */
/*                                                                */
/*  arguments   : PREDICATE = Either a most general predicate or  */
/*                            a Prolog functor                    */
/*                            e.g. predicate(_,_) or predicate/2  */
/*                                                                */
/******************************************************************/
/* Run FOIL to attempt finding a definition for PREDICATE and     */
/* then print out the resulting clauses.                          */
/******************************************************************/
foil(Name/Arity) :-
	functor(Predicate,Name,Arity),
	!,
	foil(Predicate).
foil(Goal) :-
	foil(Goal,Clauses),
	nl, write('Found definition:'), nl, 
	portray_clauses(Clauses).

portray_clauses([]) :- 
	nl.
portray_clauses([H|T]):-
	portray_clause(H),
	portray_clauses(T).

% Clauses is the set of clauses defining Goal found by FOIL. Negative
% examples are provided by either explicitly or by closed world
% assumption on the Herbrand base, depending in the switch foil_cwa/1. 
foil(Goal, Clauses) :-
	get_examples(Goal, Positives, Negatives),
	foil_loop(Positives, Goal, Negatives, [], Clauses).

% Find the positive and negative examples of Goal. Negative examples
% are constructed using the closed world assumption, if foil_cwa is
% set to true, otherwise explicitly given negative examples for Goal
% are used.
get_examples(Goal, Pos, Neg) :-
	findall(Goal, clause(Goal,true), Pos),
	( foil_cwa(true) ->
	      create_negatives(Pos, Neg)
	; findall(Goal, clause(not(Goal),true), Neg) 
	).	      

/******************************************************************/
/*                                                                */
/*  call        : foil_loop (+POSITIVE,+GOAL,+NEGATIVE,+ACCU,     */
/*			     -CLAUSES)                            */
/*                                                                */
/*  arguments   : POSTITIVE = Positive examples left to be covered*/
/*                GOAL      = The predicate which should be       */
/*                            defined.                            */
/*                NEGATIVE  = Negative examples of GOAL           */
/*                ACCU      = Clauses found in previous iterations*/
/*                CLAUSES   = Resulting clauses defining GOAL     */
/*                                                                */
/******************************************************************/
/* This predicate corresponds to the "outer loop" in Quinlan 90.  */
/* Each iteration of the outer loop attempts to construct a       */ 
/* clause, printsit and determines the remaining set of positive  */
/* examples for the next iteration. If no positive examples are   */
/* left, the outer loop terminates, and the set of clauses        */
/* defining GOAL is given back as result.                         */
/******************************************************************/
foil_loop(Pos, Goal, Neg, Clauses0, Clauses) :-
	( Pos = [] ->
	      Clauses = Clauses0
	; nl, write('Uncovered positives:'), nl, 
	  write(Pos), nl, 
	  nl, write('Adding a clause ...'), nl, nl,
	  extend_clause_loop(Neg, Pos, (Goal :- true), Clause),
	  nl, write('Clause found:'), nl,
	  portray_clause(Clause),
	  uncovered_examples(Clause, Pos, Pos1),
	  foil_loop(Pos1, Goal, Neg, [Clause|Clauses0], Clauses)
	).

/******************************************************************/
/*                                                                */
/*  call        : extend_clause_loop (+NEGATIVE,+POSITIVE,+SEED,  */
/*			              -CLAUSE)                    */
/*                                                                */
/*  arguments   : NEGATIVE  = Negative examples of GOAL           */
/*                POSTITIVE = Positive examples left to be covered*/
/*                SEED      = The most general clause defining    */
/*                            the predicate.                      */
/*                CLAUSE    = The extended clause which covers no */
/*                            negative examples, or which cannot  */
/*                            be improved.                        */
/*                                                                */
/******************************************************************/
/* This predicate corresponds to the "inner loop" in Quinlan 90   */
/* and in a general to specific manner. At each iteration a       */ 
/* premises is determined and added to SEED, until it covers no   */  
/* negative examples, or until the information gain does not      */ 
/* improve. If the latter happens, determinate literals may be    */
/* added to the clause (see Quinlan 91), depending on the value   */
/* of the switch foil_det_lit_bound/1. This switch determines the */
/* maximum number of determinate literals which can be added to   */
/* the clause.                                                    */
/******************************************************************/
extend_clause_loop(Nxs0, Pxs0, Clause0, Clause) :-
	( Nxs0 = [] ->
	      Clause = Clause0
	; write('Specializing current clause: '), nl,
	  portray_clause(Clause0), 
	  nl, write('Covered negatives:'), nl, write(Nxs0), nl,
	  nl, write('Covered positives:'), nl, write(Pxs0), nl, nl,
	  generate_possible_extensions(Clause0, Ls),
	  info_value(Clause0, Pxs0, Nxs0, Info),
	  best_next_clause(Ls, Nxs0, Pxs0, Clause0, Info, 0, Clause0, Clause1),
	  ( Clause0 == Clause1 ->
		foil_det_lit_bound(DLB),
		nl,
		write('No improvement -- trying determinate literals ...'), nl,
		bounded_determinate_literals(DLB, Ls, Clause0, Pxs0, Nxs0, Ds),
		( Ds = [] ->
		      write('No determinate literals found.'), nl,
		      covered_examples(Clause1, Nxs0, Nxs1),
		      write('WARNING -- clause covers negatives:'), nl,
		      write(Nxs1), nl,
		      Clause = Clause1
		; write('Adding determinate literals: '), write(Ds), nl, 
		  add_literals(Ds, Clause0, Clause2),
		  covered_examples(Clause2, Nxs0, Nxs1),
		  extend_clause_loop(Nxs1, Pxs0, Clause2, Clause)
		)
	  ; covered_examples(Clause1, Pxs0, Pxs1),
	    covered_examples(Clause1, Nxs0, Nxs1),
	    extend_clause_loop(Nxs1, Pxs1, Clause1, Clause)
	  )
	).
	
% Compute the information matric for the set of positive and negative
% tuples which result from applying Clause to the examples Pxs and
% NXs. 
info_value(Clause, Pxs, Nxs, Value) :-
	tuples(Clause, Pxs, Ptuples),
	length(Ptuples, P),
	( P =:= 0 ->
	      Value = 0
	; tuples(Clause, Nxs, Ntuples),
	  length(Ntuples, N),
	  Temp is P / (P + N),
	  log(Temp, Temp1),
	  Value is Temp1 * -1.442695
	).

% Determines the clause which is an extension of Clause by a single
% literal and provides maximum information gain over the original
% clause. 
best_next_clause([], _, _, _, _, _, Clause, Clause).
best_next_clause([L|Ls], Nxs, Pxs, Clause, Info, Gain0, Best0, Best) :-
	add_literal(L, Clause, Best1),
	compute_gain(Nxs, Pxs, Info, Best1, Gain1),
%% TH: For debugging purposes 
%	\+ \+ ( numbervars(Best1,0,_), 
%		write('Gain: '), write(Gain1), write('	Clause: '), 
%		print(Best1), nl ),
	( Gain1 > Gain0 ->
	      best_next_clause(Ls, Nxs, Pxs, Clause, Info, Gain1, Best1, Best)
	; Gain1 =:= Gain0 ->
	      choose_tie_clause(Best0, Best1, Best2),
	      best_next_clause(Ls, Nxs, Pxs, Clause, Info, Gain0, Best2, Best)
	; best_next_clause(Ls, Nxs, Pxs, Clause, Info, Gain0, Best0, Best)
	).

% In the case of an information tie, the clause with the viewest
% number of variables is choosen. If both have the same number of
% variables this design causes problems !
choose_tie_clause((A1:-B1), (A2:-B2), C) :-
	variables_in(B1, V1),
	length(V1, N1),
	variables_in(B2, V2),
	length(V2, N2),
% TH: This was the initial formulation, which means: in case of a
%     variable tie ignore the refinement and take the first clause.
%     In this formulation neither the membertest example 'foil_3.pl'
%     nor 'foil_4.pl' will be processed correctly !
%	( N2 < N1 -> 
%	      C = (A2:-B2)  
%	;  C = (A1:-B1) 
%	).
% TH: In this formulation, which means: in case of a variable tie use
%     the refinement and ignore the previous possible clauses,
%     the membertest example 'foil_4.pl' will be processed correctly,
%     but 'foil_3.pl' will not terminate !
	( N2 =< N1 -> 
	      C = (A2:-B2)  
	;  C = (A1:-B1) 
	).
% TH: Obviously, this implies that chosing arbitrarily a clause in the
%     case of a variable tie is the wrong solution. It would be
%     better to specialize those clauses further and to decide one
%     step later, which branch to follow. Unfortunately, Quinlan 90
%     seems to give no answer to this problem !
	      
% For a set of positive and negative examples Pxs and Nxs, compute the
% information gain of Clause over a clause which produces a split
% having Info, as it's "information value" on these examples.
compute_gain(Nxs, Pxs, Info, Clause, Gain) :-
	covered_examples(Clause, Pxs, Retained),
	length(Retained, R),
	( R =:= 0 ->
	      Gain = 0
	; info_value(Clause, Pxs, Nxs, Info1),
	  Gain is R * (Info - Info1)
	).
	
% Add a literal to the right end of a clause
add_literal(L, (A :- B), (A :- B1)) :-
	( B = true ->
	      B1 = L
	; B1 = (B,L)
	).

add_literals(Ls, Clause0, Clause) :-
	( Ls = [] ->
	      Clause = Clause0
	; Ls = [L|Ls1],
	  add_literal(L, Clause0, Clause1),
	  add_literals(Ls1, Clause1, Clause)
	).
	
% Construct a list representing the set of variables in Term.
variables_in(A, Vs) :- 
	variables_in(A, [], Vs).
	
variables_in(A, V0, V) :-
	var(A), !, 
	ord_add_element(V0, A, V).
variables_in(A, V0, V) :-
	ground(A), !, V = V0. 
variables_in(Term, V0, V) :-
	functor(Term, _, N),
	variables_in_args(N, Term, V0, V).

variables_in_args(N, Term, V0, V) :-
	( N =:= 0 ->
	      V = V0
	; arg(N, Term, Arg),
	  variables_in(Arg, V0, V1),
	  N1 is N-1,
	  variables_in_args(N1, Term, V1, V)
	).

% Given a clause and a list of examples, construct the list of tuples
% for the clause.  A tuple is the binding of values to variables such
% that the clause can be used to prove the example.
tuples((A :- B), Xs, Tuples) :-
	variables_in((A :- B), Vars),
	variables_in(A, HeadVars),
	length(HeadVars, N1),
	length(Vars, N2),
	( N1 =:= N2 ->
	      %% shortcut - only need 1 proof if no new variables.
	      findall(Vars, (member(A, Xs), \+(\+ B)), Tuples)
	; findall(Vars, (member(A,Xs), call(B)), Tuples)
	).

% Xs1 are the examples from Xs that can be proved with the clause
covered_examples((A :- B), Xs, Xs1) :-
	findall(A, ( member(A,Xs), \+( \+ B ) ), Xs1).

% Xs1 are the examples from Xs that cannot be proved with the clause.
uncovered_examples((A:-B), Xs, Xs1) :-
	findall(A, ( member(A, Xs), \+ B ), Xs1 ).

% Generate possible literals, which can be used to extend the clause
generate_possible_extensions((A :- B), Extensions) :-
	variables_in((A :- B), OldVars),
        %% TH: This differs from the original implementation, which
	%%     was not correct, since findall usually loses the
	%%     variable bindings.  
	findall((OldVars :- L), candidate_literal(A, OldVars, L), Extension1),
	rmhead(Extension1,OldVars,Extensions).

% Compute a candidate literal. If the switch foil_use_negations/1 is
% set also negated literals are generated.
candidate_literal(Goal, OldVars, Lit) :-
	foil_predicates(Preds),
	member(Pred/Arity, Preds),
	functor(L, Pred, Arity),
	recursion_check(Goal, Pred, Arity, RecursionFlag),
	MaxNewVars is Arity - 1, 
	possible_new_vars(RecursionFlag, MaxNewVars, NewVars),
	length(NewVars, NewVarPositions),
	OldVarPositions is Arity - NewVarPositions,
	list_of_n_from(OldVars, OldVarPositions, [], OldVarSeq),
	recursion_safe(RecursionFlag, Goal, OldVarSeq),
	possible_unification(NewVars, NewVarSeq, _),
	subseq(VarSeq, OldVarSeq, NewVarSeq),
	bind_vars(L, VarSeq, 1),
	( Lit = L 
	; foil_use_negations(true), 
	  Lit = (\+ L) 
	).

recursion_check(G, Pred, Arity, Flag) :-
	( functor(G, Pred, Arity) ->
	      Flag = true
	; Flag = false
	).

possible_new_vars(true,_,[]).
possible_new_vars(false, N, L) :-
	length(L,N).
possible_new_vars(false, N, L) :-
	N > 0,
	N1 is N - 1,
	possible_new_vars(false, N1, L).

list_of_n_from(Elements, N, List0, List) :-
	( N is 0 ->
	      List = List0
	; N1 is N - 1,
	  member(E, Elements),
	  list_of_n_from(Elements, N1, [E|List0], List)
	).

recursion_safe(true, Goal, OldVarSeq) :-
	!,
	\+ (numbervars(Goal, 0, _), ground(OldVarSeq)).
recursion_safe(false, _, _).

possible_unification([], [], []).
possible_unification([H|T], [H|Result], [H|Vars]) :-
	possible_unification(T,Result,Vars).
possible_unification([H|T], [H|T1], Vs) :-
	possible_unification(T, T1, Vs),
	member(V,Vs),
	H = V.

bind_vars(Lit, Vars, Index) :-
	( Vars = [] ->
	      true
	; Vars = [H|T],
 	  arg(Index, Lit, H),
	  Index1 is Index + 1,
	  bind_vars(Lit, T, Index1)
	).

rmhead([],_,[]).
rmhead([(Vars :- B)|Rest],Vars,[B|Result]) :-
	rmhead(Rest,Vars,Result).

%---------------------------------------------------------------------------
% Closed World Assumption on the Herbrand base

create_universe(Universe) :-
	setof(Term, term_of_ext_def(Term), Universe).

term_of_ext_def(Term) :-
	foil_predicates(PredSpecs),
	member(Pred/Arity, PredSpecs),
	functor(Goal, Pred, Arity),
	call(Goal),
	between(1, Arity, ArgPos),
	arg(ArgPos, Goal, Term).

create_negatives([P|Ps], Negatives) :-
	functor(P, F, N),
	functor(Template, F, N),
	create_universe(Universe),
	setof(Template,
	      (  arguments_are_members(Template, N, Universe),
	         \+ member(Template, [P|Ps]) ),
	      Negatives).
	      
arguments_are_members(Term, N, Universe) :-
	( N > 0 ->
	      arg(N, Term, Arg),
	      member(Arg, Universe),
	      N1 is N-1,
	      arguments_are_members(Term, N1, Universe)
	; true
	).
	
%---------------------------------------------------------------------------
% Determinate Literals

% determinate(+Lit, +Vars, +PTuples, +NTuples) -- holds if Lit is a
% determinate literal wrt the bindings for Vars as represented in
% PTuples and NTuples. 
determinate(L, Vars, PTuples, NTuples) :-
	binds_new_var(L, Vars),
	determ_cover(PTuples, L, Vars),
	determ_partial_cover(NTuples, L, Vars).

binds_new_var((\+ _),_) :- 
	!, fail.
binds_new_var(L, Vars) :-
	variables_in(L, LVars),
	member(V,LVars),
	\+ contains_var(V, Vars),
	!.

determ_cover([], _, _).
determ_cover([T|Ts], Lit, Vars) :-
	findall(Lit, (Vars = T, call(Lit)), [_]),
	determ_cover(Ts, Lit, Vars).

determ_partial_cover([], _, _).
determ_partial_cover([T|Ts], Lit, Vars) :-
	findall(Lit, (Vars=T, call(Lit)), Xs),
	(Xs = [] ; Xs = [_]),
	determ_partial_cover(Ts, Lit, Vars).

bounded_determinate_literals(0, _, _, _, _, []) :- !.
bounded_determinate_literals(Bound, Cands, (A:-B), Pxs, Nxs, DLits) :-
	determinate_literals(Cands, (A:-B), Pxs, Nxs, DLits0),
	reachable_antes(Bound, A, DLits0, DLits).

determinate_literals(Cands, Clause, Pxs, Nxs, DLits) :-
	variables_in(Clause, Vars),
	tuples(Clause, Pxs, PTuples),
	tuples(Clause, Nxs, NTuples),
	Clause = (_:-Body),
	determinate_literals1(Cands, Body, Vars, PTuples, NTuples, DLits).

determinate_literals1(Cands, Body, Vars, PTuples, NTuples, DLits) :-
	bagof(X, ( member(X, Cands), 
	           determinate(X, Vars, PTuples, NTuples),
		   \+( (numbervars(Vars,0,_), ante_memberchk(X,Body)) )
		 ), 
	      DLits).

ante_memberchk(A,A) :- !.
ante_memberchk(A, (B,C)) :-
	( ante_memberchk(A,B) ->
	      true
	; ante_memberchk(A,C)
	).

% reachable_antes(+Bound, +H, +Cands, -Antes) -- Antes is the list of
% literals from Cands which can be "connected" to H by some chain of
% variables of length <= Bound. 
reachable_antes(Bound, H, Cands, Antes) :-
	variables_in(H, Vs),
	expand_by_var_chain(Bound, Cands, Vs, [], Antes).

expand_by_var_chain(Bound, Cands, Vars, As0, As) :-
	( Bound =:= 0 ->
	      As = As0
	; partition_on_vars(Cands, Vars, Haves, Havenots),
	  ( Haves = [] ->
		As = As0
	  ; append(As0, Haves, As1),
	    variables_in(As1, Vars1),
	    Bound1 is Bound - 1,
	    expand_by_var_chain(Bound1, Havenots, Vars1, As1, As)
	  )
	).

partition_on_vars([], _, [], []).
partition_on_vars([C|Cs], Vars, Hs, Hnots) :-
	( member(V, Vars), contains_var(V, C) ->
	      Hs = [C|Hs1],
	      Hnots = Hnots1
	; Hs = Hs1,
	  Hnots = [C|Hnots1]
	),
	partition_on_vars(Cs, Vars, Hs1, Hnots1).

% If the Quintus libraries are available the following can be removed
between(X,_,X).
between(X,Max,Z) :-
	X < Max,
	Y is X + 1,
	!,
	between(Y,Max,Z).

%   ord_add_element(+Set1, +Element, ?Set2)
%   is the equivalent of add_element for ordered sets.  It should give
%   exactly the same result as merge(Set1, [Element], Set2), but a bit
%   faster, and certainly more clearly.

ord_add_element([], Element, [Element]).
ord_add_element([Head|Tail], Element, Set) :-
	compare(Order, Head, Element),
	ord_add_element(Order, Head, Tail, Element, Set).

ord_add_element(<, Head, Tail, Element, [Head|Set]) :-
	ord_add_element(Tail, Element, Set).
ord_add_element(=, Head, Tail, _, [Head|Tail]).
ord_add_element(>, Head, Tail, Element, [Element,Head|Tail]).

%   contains_var(+Variable, +Term)
%   is true when the given Term contains at least one sub-term which
%   is identical to the given Variable.  We use '=='/2 to check for
%   the variable (contains_term/2 uses '=') so it can be used to check
%   for arbitrary terms, not just variables.
contains_var(Variable, Term) :-
	\+ free_of_var(Variable, Term).

%   free_of_var(+Variable, +Term)
%   is true when the given Term contains no sub-term identical to the
%   given Variable (which may actually be any term, not just a var).
%   For variables, this is precisely the "occurs check" which is
%   needed for sound unification.
free_of_var(Variable, Term) :-
	Term == Variable,
	!,
	fail.
free_of_var(Variable, Term) :-
	compound(Term),
	!,
	functor(Term, _, Arity),
	free_of_var(Arity, Term, Variable).
free_of_var(_, _).

free_of_var(1, Term, Variable) :- !,
	arg(1, Term, Argument),
	free_of_var(Variable, Argument).
free_of_var(N, Term, Variable) :-
	arg(N, Term, Argument),
	free_of_var(Variable, Argument),
	M is N - 1, !,
	free_of_var(M, Term, Variable).

%   subseq(Sequence, SubSequence, Complement)
%   is true when SubSequence and Complement are both subsequences of
%   the list Sequence (the order of corresponding elements being
%   preserved) and every element of Sequence which is not in
%   SubSequence is in the Complement and vice versa.  That is,
%   length(Sequence) = length(SubSequence)+length(Complement), e.g.
%   subseq([1,2,3,4], [1,3,4], [2]).  This was written to generate
%   subsets and their complements together, but can also be used to
%   interleave two lists in all possible ways.  Note that if S1 is a
%   subset of S2, it will be generated *before S2 as a SubSequence and
%   *after it as a Complement.  
subseq([], [], []).
subseq([Head|Tail], Sbsq, [Head|Cmpl]) :-
	subseq(Tail, Sbsq, Cmpl).
subseq([Head|Tail], [Head|Sbsq], Cmpl) :-
	subseq(Tail, Sbsq, Cmpl).

member(X,[X|_]).
member(X,[_|T]) :- 
        member(X,T).

/*
foil_1.pl

foil_predicates([ can_reach/2, linked_to/2]).
foil_cwa(true).
foil_use_negations(false).  % Don't use negations of foil_predicates
foil_det_lit_bound(0).    % Don't add any determinate literals
                          % In general, this is a depth limit on
       			  %   the search for determinate literals

% Definitions of background predicates

can_reach(0,1).
can_reach(0,2).
can_reach(0,3).
can_reach(0,4).
can_reach(0,5).
can_reach(0,6).
can_reach(0,8).
can_reach(1,2).
can_reach(3,2).
can_reach(3,4).
can_reach(3,5).
can_reach(3,6).
can_reach(3,8).
can_reach(4,5).
can_reach(4,6).
can_reach(4,8).
can_reach(6,8).
can_reach(7,6).
can_reach(7,8).

linked_to(0,1).
linked_to(0,3).
linked_to(1,2).
linked_to(3,2).
linked_to(3,4).
linked_to(4,5).
linked_to(4,6).
linked_to(6,8).
linked_to(7,6).
linked_to(7,8).


*/

/*
% foil_3.pl

foil_predicates([ null/1, components/3, membertest/2]).
foil_cwa(true).
foil_use_negations(false).        % Don't use negations of foil_predicates
foil_det_lit_bound(0).            % Don't add any determinate literals
                                  % In general, this is a depth limit on
			          %   the search for determinate literals

% Note that this example is due to Quinlan 90, but it cannot be
% processed correctly ! On the other hand the example foil_4.pl works
% as expected !

% Definitions of background predicates
null([]).

components([[a]], [a], []).
components([b, [a], d], b, [[a], d]).
components([[a],d], [a], [d]).
components([d], d, []).
components([e|f], e, f).

membertest(a,[a, b, [c]]).
membertest(b,[a, b, [c]]).
membertest([c],[a, b, [c]]).
membertest(b,[b, [c]]).
membertest([c],[b, [c]]).
membertest([c],[[c]]).

*/
