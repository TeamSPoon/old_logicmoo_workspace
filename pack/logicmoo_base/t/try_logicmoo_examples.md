#!/usr/bin/env swipl

/**
````
*/

:- if(gethostname(ubuntu)).
:- user:ensure_loaded(logicmoo_repl).
:- else.
:- user:ensure_loaded(logicmoo_repl).
% :- load_files(logicmoo_repl, [if(not_loaded),qcompile(auto)]).
:- endif.

show_call_test(G):-must(show_call(G)).

%= define the example language
example_known_is_success(G):-  G.
example_impossible_is_success(G):- neg(G).
example_known_is_failure(G):-  \+ G.
example_impossible_is_failure(G):- \+ neg(G).

%= define the four truth values
example_proven_true(G):- example_known_is_success(G),example_impossible_is_failure(G).
example_proven_false(G):- example_impossible_is_success(G),example_known_is_failure(G).
example_inconsistent(G):- example_known_is_success(G),example_impossible_is_success(G).
example_unknown(G):- example_known_is_failure(G),example_impossible_is_failure(G).
  
:-multifile shared_hide_data/1.
%= shared_hide_data(hideMeta):-is_main_thread.
%= shared_hide_data(hideTriggers):-is_main_thread.

:- shell(cls).
:- process_this_script.


%=  setup pfc
:- file_begin(pfc).

%=  see logicmoo_i_compiler.pl for more info
:- set_clause_compile(fwc).


isa(Pred, ptSymmetric)==> (t(Pred, X, Y)==>t(Pred, Y, X)), (neg(t(Pred, X, Y))==>neg(t(Pred, Y, X))).

isa(love_compatible, ptSymmetric).


%= if two thing do like  each other then they are "love compatible"
clif(
 forall(a,forall(b,
    if( (likes(a,b)  & likes(b,a)), 
     love_compatible(a,b))))).


%= will have the side effects... 

%= if A is not love compatible with B .. yet B likes A.. we show_call conclude A show_call not like B back.
:- is_entailed((not(likes(A, B)):-not(love_compatible(A, B)), likes(B, A))).
%= if A is not love compatible with B .. yet A likes B.. we show_call conclude B show_call not like A back.
:- is_entailed((not(likes(B, A)):-not(love_compatible(A, B)), likes(A, B))).
%= if A and B like each other both ways then they are love compatible
:- is_entailed((love_compatible(A, B):-likes(A, B), likes(B, A))).


%= if people are love compatible then they show_call like each other
clif(
  forall(a,forall(b,
   if(love_compatible(a,b), 
    (likes(a,b)  & likes(b,a)))))).


 
%= will have the side effects... 

%= if A and B show_call not be love compatible since A does not like B
:- is_entailed((not(love_compatible(A, B)):-not(likes(A, B)))).
%= if A and B show_call not be love compatible since B does not like A
:- is_entailed((not(love_compatible(A, B)):-not(likes(B, A)))).
%= obviously A likes B .. since they are love compatible
:- is_entailed((likes(A, B):-love_compatible(A, B))).
%= obviously B likes A .. after all they are love compatible
:- is_entailed((likes(B, A):-love_compatible(A, B))).


%= this uses biconditional implicatature 
clif(
 forall(a,forall(b,
  iff(might_altercate(a,b),
    (dislikes(a,b)  & dislikes(b,a)))))).

%=  canonicalizes to..

%= A and B will not scrap becasue it takes two to tango and A doesnt dislike B
:- is_entailed((not(might_altercate(A, B)):-not(dislikes(A, B)))).
%= A and B will not scrap becasue it takes B doent dislike A (like above)
:- is_entailed((not(might_altercate(A, B)):-not(dislikes(B, A)))).
%= Since we can prove A and B  dislike each other we can prove they are scrap compatible
:- is_entailed((might_altercate(A, B):-dislikes(A, B), dislikes(B, A))).
%=  A dislikes B  when we prove A and B are scrap compatible somehow  (this was due to the biconditional implicatature)
:- is_entailed((dislikes(A, B):-might_altercate(A, B))).
%=  B dislikes A  when we prove A and B are scrap compatible
:- is_entailed((dislikes(B, A):-might_altercate(A, B))).


%= alice likes bill
clif(likes(alice,bill)). 

%= dumbo does not exists
%= TODO clif(not(isa(dumbo,_))).

%= evertyting that exists is an instance of Thing
%= TODO clif(isa(_,tThing))).

%= also she likes ted
clif(likes(alice,ted)).

%= we take as a given that bill does not like alice (why?)
clif(not(likes(bill,alice))).

%= we take as a given that bill and ted dislike each other (dont blame the woman!)
clif((dislikes(bill,ted) & dislikes(ted,bill))).

%= we support also SUMO language 
:- file_begin(kif).
%= (yes we are reading and atom.. which contains parens so we read it as sexprs
%= thank you triska for showing off this neat hack for term reading)

%= treat the (normally PFC) operators as clif
:- op_alias((<=>), iff).
:- op_alias( (=>),  if).

%= we of course supprt KIF
clif('

(<=>
  (dislikes ?A ?B)
  (not
      (likes ?A ?B)))

'
).

%= we of course supprt CLIF!
clif('

(forall (a b)
 (iif
  (dislikes a b)
  (not
      (likes a b))))

'
).

%= we support also CycL language 
clif('

(equiv
  (dislikes ?A ?B)
  (not
      (likes ?A ?B)))

'
).

%= interestingly this canonicallizes to ... 
%= A does not dislike B when A like B
:- is_entailed((not(dislikes(A, B)):-likes(A, B))).
%= A does not like B when A dislikes B
:- is_entailed((not(likes(A, B)):-dislikes(A, B))).
%= A does dislikes B when we can somehow prove A not liking B
:- is_entailed((dislikes(A, B):-not(likes(A, B)))).
%= A does likes B when we can somehow prove A not disliking B
:- is_entailed((likes(A,B) => not(dislikes(A,B)))).

%= The above assertions forward chain to these side-effects... 
:- is_entailed((not(love_compatible(bill, alice)))).
:- is_entailed((not(love_compatible(alice, bill)))).
:- is_entailed((might_altercate(ted, bill))).
:- is_entailed((might_altercate(bill, ted))).

%= get proof
:- sanity(call(pfc_get_support(not(love_compatible(bill, alice))   ,Why))).
%= O = (not(likes(bill, alice)), pt(not(likes(bill, alice)), rhs([not(love_compatible(bill, alice))]))) ;
%= TODO fix this error O = (u, u). 

%= logic tests...


:- file_begin(kif).
%= treat the (normally PFC) operators as clif
:- op_alias((<=>), iff).
:- op_alias( (=>),  if).


%= save compiled clauses using forward chaining storage
%= we are using forward chaining just so any logical errors, performance and program bugs manefest
%= immediately
:- set_clause_compile(fwc).
:- file_begin(kif).
:- pfc_trace.

prologBuiltin(otherGender/2).
otherGender(male,female).
otherGender(female,male).

:- pfc_trace.

breeder(X,Y) <=> breeder(Y,X).

(breeder(X,Y),gender(X,G1), otherGender(G1,G2))
     => gender(Y,G2).


gender(P,male) <=> male(P).
gender(P,female) <=> female(P).

male(P) <=> ~female(P).



((parent(X,Y) & female(X)) <=> mother(X,Y)).

:- is_entailed(((parent(X,Y) & female(X)) <=> mother(X,Y))).
:- is_entailed(((parent(X,Y) & female(X)) => mother(X,Y))).
:- is_entailed((mother(X,Y)=>(parent(X,Y) & female(X)))).
:- is_entailed((parent(A,B):-mother(A,B))).
:- is_entailed(not(mother(A,B)):-not(parent(A,B))).

((parent(X,Y) & female(X)) => mother(X,Y)).
((mother(X,Y) => parent(X,Y) & female(X))).

:- is_entailed((parent(A,B):-mother(A,B))).




parent(GRAND,PARENT),parent(PARENT,CHILD) => grandparent(GRAND,CHILD).

grandparent(X,Y),male(X) <=> grandfather(X,Y).
grandparent(X,Y),female(X) <=> grandmother(X,Y).
mother(Ma,Kid),parent(Kid,GrandKid)
      =>grandmother(Ma,GrandKid).
grandparent(X,Y),female(X) <=> grandmother(X,Y).
parent(X,Y),male(X) <=> father(X,Y).
(parent(Ma,X),parent(Ma,Y),different(X,Y) =>siblings(X,Y)).
parent(P1,P2) => ancestor(P1,P2).
(parent(P1,P2), ancestor(P2,P3)) => ancestor(P1,P3).
(ancestor(P1,P2), ancestor(P2,P3)) => ancestor(P1,P3).


mother(eileen,douglas).


human(trudy).

never_retract_u(human(trudy)).

clif(forall(p,exists([m,f], if(human(p), (mother(m,p) & father(f,p)))))).


:-show_call(show_call(father(_,trudy))).


mother(trudy,eileen).
((human(P1),ancestor(P1,P2))=>human(P2)).
% :- listing([ancestor,human,parent]).
%=:- wdmsg("press Ctrl-D to resume.").
%=:- prolog.

:-is_entailed(grandmother(trudy,douglas)).

mother(trudy,robby).
mother(trudy,liana).
mother(liana,matt).
mother(liana,liz).
mother(trudy,pam).


%= this fact sets off the anscesteral rule that her decendants are humans to
%= human(trudy).

%= therefore
:-is_entailed(human(douglas)).

/*

?- pfc_why(human(douglas)).

Justifications for human(douglas):
    1.1 ancestor(trudy,douglas)
    1.2 human(trudy)
    1.3 human(trudy),ancestor(trudy,douglas),{vg(s(douglas))}==>human(douglas)
    2.1 ancestor(eileen,douglas)
    2.2 human(eileen)
    2.3 human(eileen),ancestor(eileen,douglas),{vg(s(douglas))}==>human(douglas)


:- pfc_why(grandparent(trudy,douglas)).

Justifications for grandparent(trudy,douglas):
    1.1 grandmother(trudy,douglas)
    1.2 grandmother(trudy,douglas),{vg(s(douglas,trudy))}==>grandparent(trudy,douglas)
    2.1 parent(eileen,douglas)
    2.2 parent(trudy,eileen)
    2.3 parent(trudy,eilee.),parent(eileen,douglas),{vg(s(douglas,trudy))}==>grandparent(trudy,douglas)

*/

%=:- wdmsg("press Ctrl-D to resume.").


:- style_check(-singleton).


%= so far no males "asserted" in the KB
:-doall(show_call(male(Who ))).
/*
OUTPUT WAS.. 
male(skArg1ofFatherFn(pam)).
male(skArg1ofFatherFn(liz)).
male(skArg1ofFatherFn(matt)).
male(skArg1ofFatherFn(liana)).
male(skArg1ofFatherFn(robby)).
male(skArg1ofFatherFn(eileen)).
male(skArg1ofFatherFn(douglas)).
male(skArg1ofFatherFn(trudy)).
*/

%= we can report the presence on non male though...
%=    the ~/1 is our negation hook into the inference engine 
% :-prolog.
:- no_varnaming( pfc_no_chaining(doall((trace,show_call(~male(Who )))))).
:-prolog.
%= we expect to see at least there mothers here
%=  succeed(user: ~male(liana)).
%=  succeed(user: ~male(trudy)).
%=            succeed(user: ~male(skArg1ofMotherFn(trudy))).
%=  succeed(user: ~male(eileen)).

%= thus ~/1 is tnot/1 of XSB ?!?

%= there ar explicly non females
:-doall(show_call(~ female(Who ))).

%= ensure skolems are made or destroyed

father(robert,eileen).
siblings(douglas,cassiopea).
father(douglas,sophiaWebb).
father(douglas,skylar).
father(douglas,sophiaWisdom).
father(douglas,zaltana).

:-is_entailed(human(douglas)).

:- pfc_why(human(douglas)).

:- pfc_why(grandparent(trudy,douglas)).

:-doall(show_call_test(mother(Female,Who))).

:-doall(show_call_test(father(Male,Who))).

% :-doall(show_call_test(male(Who))).

% :-doall(show_call_test(female(Who))).

:-doall(show_call_test(siblings(Who,AndWho))).

:-prolog.

%= human(P) => (female(P) v male(P)).
clif(if(gendered_human(P), (female(P) v male(P)))).

%= ````


