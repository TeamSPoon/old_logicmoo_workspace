/** <module> special module hooks into the logicmoo engine allow
%   clif syntax to be recocogized via our CycL/KIF handlers 
% 
% Logicmoo Project: A LarKC Server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/

% we are in "prolog" consultation mode

:- ensure_loaded(logicmoo(logicmoo_engine)).
:- enable_mpred_expansion.
:- user:ensure_loaded(logicmoo_i_compiler).

are_clauses_entailed([C|L]):-!,maplist(clause_asserted,[C|L]).
are_clauses_entailed((C,L)):-!,are_clauses_entailed(C),are_clauses_entailed(L).
are_clauses_entailed(CL):-clause_asserted(CL).

:- thlocal:disable_mpred_term_expansions_locally->throw(thlocal:disable_mpred_term_expansions_locally);true.

% cwc "code-wise chaining" is always true in Prolog but will throw programming error if evalled in LogicMOO Prover.
% Use this to mark code and not axiomatic prolog
clif_to_prolog(HORN,Prolog):- cwc, compound(HORN), HORN=(_:-_),!,boxlog_to_pfc(HORN,Prolog),!.
clif_to_prolog(CLIF,Prolog):- cwc,
  % somehow integrate why_to_id(tell,Wff,Why),
     must_det_l((kif_to_boxlog(CLIF,HORN),
     boxlog_to_pfc(HORN,Prolog))).


% Sanity Test for expected side-effect entailments
clif_must(CLIF):- cwc,sanity((clif_to_prolog(CLIF,Prolog),!,show_call(are_clauses_entailed(Prolog)))),!.

% Sanity Test for required absence of specific side-effect entailments
clif_must_not(CLIF):- cwc, sanity((clif_to_prolog(CLIF,Prolog),show_call(\+ are_clauses_entailed(Prolog)))),!.


:-op(1190,xfx,(:-)).
:-op(1200,fy,(clif_must)).

% this defines a recogniser for clif syntax
is_clif(all(_,X)):-cwc,compound(X),!,is_clif(X).
is_clif(forall(_,X)):-cwc,compound(X),!.
is_clif(CLIF):-cwc,
  VVs = v(if,iff,clif_forall,all,exists), % implies,equiv,forall
   (var(CLIF)-> (arg(_,VVs,F),functor(CLIF,F,2));
     compound(CLIF),functor(CLIF,F,2),arg(_,VVs,F)).

% we ensure we are in "pfc" consultation mode

:- wdmsg(pfc_trace).
:- pfc_trace.
:- file_begin(pfc).

% make sure op alias for '=>' is not overriden
:- op_alias( (=>),  (=>)).

% whenever we know about clif we'll use the prolog forward chainging system
(clif(CLIF) => 
   ({ clif_to_prolog(CLIF,PROLOG) },
      % this consequent asserts the new rules
      PROLOG)).

% we create syntax listeners for [if,iff,clif_forall,all,exists]/2s
({is_clif(CLIF)} =>
  (CLIF/is_clif(CLIF) => clif(CLIF))).

:- if(if_defined(pfc_examples,true)).

:- wdmsg(pfc_trace).

:- pfc_trace.

% see logicmoo_i_compiler.pl for more info
:- set_clause_compile(fwc).

% if two like each other then they are love compatable
clif(
 forall(a,forall(b,
    if( (likes(a,b)  & likes(b,a)), 
     love_compatable(a,b))))).


% will have the side effects... 

% if A is not love compatable with B .. yet B likes A.. we must conclude A must not like B back.
:- clif_must((not(likes(A, B)):-not(love_compatable(A, B)), likes(B, A))).
% if A is not love compatable with B .. yet A likes B.. we must conclude B must not like A back.
:- clif_must((not(likes(B, A)):-not(love_compatable(A, B)), likes(A, B))).
% if A and B like each other both ways then they are love compatable
:- clif_must((love_compatable(A, B):-likes(A, B), likes(B, A))).


% if people are love compatable then they must like each other
clif(
  forall(a,forall(b,
   if(love_compatable(a,b), 
    (likes(a,b)  & likes(b,a)))))).

 
% will have the side effects... 

% if A and B must not be love compatable since A does not like B
:- clif_must((not(love_compatable(A, B)):-not(likes(A, B)))).
% if A and B must not be love compatable since B does not like A
:- clif_must((not(love_compatable(A, B)):-not(likes(B, A)))).
% obviously A likes B .. since they are love compatable
:- clif_must((likes(A, B):-love_compatable(A, B))).
% obviously B likes A .. after all they are love compatable
:- clif_must((likes(B, A):-love_compatable(A, B))).


% this uses biconditional implicatature 
clif(
 forall(a,forall(b,
  iff(might_altercate(a,b),
    (dislikes(a,b)  & dislikes(b,a)))))).

%  canonicalizes to..

% A and B will not scrap becasue it takes two to tango and A doesnt dislike B
:- clif_must((not(might_altercate(A, B)):-not(dislikes(A, B)))).
% A and B will not scrap becasue it takes B doent dislike A (like above)
:- clif_must((not(might_altercate(A, B)):-not(dislikes(B, A)))).
% Since we can prove A and B  dislike each other we can prove they are scrap compatable
:- clif_must((might_altercate(A, B):-dislikes(A, B), dislikes(B, A))).
%  A dislikes B  when we prove A and B are scrap compatable somehow  (this was due to the biconditional implicatature)
:- clif_must((dislikes(A, B):-might_altercate(A, B))).
%  B dislikes A  when we prove A and B are scrap compatable
:- clif_must((dislikes(B, A):-might_altercate(A, B))).


% alice likes bill
clif(likes(alice,bill)).

% also she likes ted
clif(likes(alice,ted)).

% we take as a given that bill does not like alice (why?)
clif(not(likes(bill,alice))).

% we take as a given that bill and ted dislike each other (dont blame the woman!)
clif((dislikes(bill,ted) & dislikes(ted,bill))).

% we support also SUMO language 
:- file_begin(kif).
% (yes we are reading and atom.. which contains parens so we read it as sexprs
% thank you triska for showing off this neat hack for term reading)

% treat the (normally PFC) operators as clif
:- op_alias((<=>), iff).
:- op_alias( (=>),  if).

clif('

(<=>
  (dislikes ?A ?B)
  (not
      (likes ?A ?B)))

'
).


% we support also CycL language 
clif('

(equiv
  (dislikes ?A ?B)
  (not
      (likes ?A ?B)))

'
).

% interestingly this canonicallizes to ... 
% A does not dislike B when A like B
:- clif_must((not(dislikes(A, B)):-likes(A, B))).
% A does not like B when A dislikes B
:- clif_must((not(likes(A, B)):-dislikes(A, B))).
% A does dislikes B when we can somehow prove A not liking B
:- clif_must((dislikes(A, B):-not(likes(A, B)))).

% NOTE: somehow we avoided this trap! 
:- clif_must_not((likes(A,B) :- not(dislikes(A,B)))).
/*
 Though it a bit of a red herring due the the fact not(disliking(..)) in our system means 
  you have "proven by some means that not disliking is true"
    .. such means would include likes(N3, O3) .. and it does above.
   but still not sure if this is by crook.

*/

% The above assertions forward chain to these side-effects... 
:- clif_must((not(love_compatable(bill, alice)))).
:- clif_must((not(love_compatable(alice, bill)))).
:- clif_must((might_altercate(ted, bill))).
:- clif_must((might_altercate(bill, ted))).

% get proof
:- sanity(call(pfc_get_support(not(love_compatable(bill, alice))   ,Why))).
% O = (not(likes(bill, alice)), pt(not(likes(bill, alice)), rhs([not(love_compatable(bill, alice))]))) ;
% TODO fix this error O = (u, u). 

% logic tests...


:- file_begin(kif).
% treat the (normally PFC) operators as clif
:- op_alias((<=>), iff).
:- op_alias( (=>),  if).


% save compiled clauses using forward chaining storage
% we are using forward chaining just so any logical errors, performance and program bugs manefest
% immediately
:- set_clause_compile(fwc).
:- file_begin(kif).

otherGender(male,female).
otherGender(female,male).
% :-start_rtrace.
spouse(X,Y) <=> spouse(Y,X).
:-prolog.
(spouse(X,Y),gender(X,G1), otherGender(G1,G2))
     => gender(Y,G2).



gender(P,male) <=> male(P).
gender(P,female) <=> female(P).

male(P) <=> ~female(P).

% human(P) => (female(P) v male(P)).
clif(if(human(P), (female(P) v male(P)))).

all(P,exists([M,F],
  (human(P) => mother(M,P) & father(F,P)))).

parent(X,Y),female(X) <=> mother(X,Y).
parent(X,Y),parent(Y,Z) => grandparent(X,Z).
grandparent(X,Y),male(X) <=> grandfather(X,Y).
grandparent(X,Y),female(X) <=> grandmother(X,Y).
mother(Ma,Kid),parent(Kid,GrandKid)
      =>grandmother(Ma,GrandKid).
grandparent(X,Y),female(X) <=> grandmother(X,Y).
parent(X,Y),male(X) <=> father(X,Y).
parent(Ma,X),parent(Ma,Y),{dif(X,Y)} =>sibling(X,Y).
parent(P1,P2) => ancestor(P1,P2).
parent(P1,P2), ancestor(P2,P3) => ancestor(P1,P3).


((human(P1),ancestor(P1,P2))=>human(P2)).

mother(eileen,douglas).
mother(trudy,eileen).
mother(trudy,robby).
mother(trudy,liana).
mother(liana,matt).
mother(liana,liz).
mother(trudy,pam).

% this fact sets off the anscesteral rule that her decendants are humans to
human(trudy).

% therefore
:-clif_must(human(douglas)).

% so far no males in the KB
% :- sanity( ~(~(male(_)))). 

% we should report the presence on non male though...
%    the ~/1 is our negatinon hook into the inference engine 
:-doall(show_call(~ male(Who ))).
% we expect to see at least there mothers here


% unaliasing
:- op_alias( (<=>),  <=>).
:- op_alias(  (=>),   =>).


% break to the debugger
:- wdmsg("press Ctrl-D to resume.").
:- break.

:- endif.


