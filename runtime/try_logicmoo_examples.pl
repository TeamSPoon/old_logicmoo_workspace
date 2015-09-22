#!/usr/bin/env swipl
/** <module> Example Logicmoo_base startup script in SWI-Prolog

*/

:- if(gethostname(ubuntu)).
:- user:ensure_loaded(logicmoo_repl).
:- else.
:- load_files(logicmoo_repl, [if(not_loaded),qcompile(auto)]).
:- endif.



:- file_begin(pfc).
:- pfc_trace.
:- pfc_watch.

% see logicmoo_i_compiler.pl for more info
:- set_clause_compile(fwc).


==> clif(male(P)  => ~ female(P)).

==> male(joe).


:-prolog.

% if two thing do like  each other then they are "love compatible"
clif(
 forall(a,forall(b,
    if( (likes(a,b)  & likes(b,a)), 
     love_compatible(a,b))))).

:-prolog.

% will have the side effects... 

% if A is not love compatible with B .. yet B likes A.. we must conclude A must not like B back.
:- horn_true((not(likes(A, B)):-not(love_compatible(A, B)), likes(B, A))).
% if A is not love compatible with B .. yet A likes B.. we must conclude B must not like A back.
:- horn_true((not(likes(B, A)):-not(love_compatible(A, B)), likes(A, B))).
% if A and B like each other both ways then they are love compatible
:- horn_true((love_compatible(A, B):-likes(A, B), likes(B, A))).


% if people are love compatible then they must like each other
clif(
  forall(a,forall(b,
   if(love_compatible(a,b), 
    (likes(a,b)  & likes(b,a)))))).


 
% will have the side effects... 

% if A and B must not be love compatible since A does not like B
:- horn_true((not(love_compatible(A, B)):-not(likes(A, B)))).
% if A and B must not be love compatible since B does not like A
:- horn_true((not(love_compatible(A, B)):-not(likes(B, A)))).
% obviously A likes B .. since they are love compatible
:- horn_true((likes(A, B):-love_compatible(A, B))).
% obviously B likes A .. after all they are love compatible
:- horn_true((likes(B, A):-love_compatible(A, B))).


% this uses biconditional implicatature 
clif(
 forall(a,forall(b,
  iff(might_altercate(a,b),
    (dislikes(a,b)  & dislikes(b,a)))))).

%  canonicalizes to..

% A and B will not scrap becasue it takes two to tango and A doesnt dislike B
:- horn_true((not(might_altercate(A, B)):-not(dislikes(A, B)))).
% A and B will not scrap becasue it takes B doent dislike A (like above)
:- horn_true((not(might_altercate(A, B)):-not(dislikes(B, A)))).
% Since we can prove A and B  dislike each other we can prove they are scrap compatible
:- horn_true((might_altercate(A, B):-dislikes(A, B), dislikes(B, A))).
%  A dislikes B  when we prove A and B are scrap compatible somehow  (this was due to the biconditional implicatature)
:- horn_true((dislikes(A, B):-might_altercate(A, B))).
%  B dislikes A  when we prove A and B are scrap compatible
:- horn_true((dislikes(B, A):-might_altercate(A, B))).


% alice likes bill
clif(likes(alice,bill)). 

% dumbo does not exists
% TODO clif(not(isa(dumbo,_))).

% evertyting that exists is an instance of Thing
% TODO clif(isa(_,tThing))).

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

% we of course supprt KIF
clif('

(<=>
  (dislikes ?A ?B)
  (not
      (likes ?A ?B)))

'
).

% we of course supprt CLIF!
clif('

(forall (a b)
 (iif
  (dislikes a b)
  (not
      (likes a b))))

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
:- horn_true((not(dislikes(A, B)):-likes(A, B))).
% A does not like B when A dislikes B
:- horn_true((not(likes(A, B)):-dislikes(A, B))).
% A does dislikes B when we can somehow prove A not liking B
:- horn_true((dislikes(A, B):-not(likes(A, B)))).
% A does likes B when we can somehow prove A not disliking B
:- horn_true((likes(A,B) :- not(dislikes(A,B)))).

% The above assertions forward chain to these side-effects... 
:- horn_true((not(love_compatible(bill, alice)))).
:- horn_true((not(love_compatible(alice, bill)))).
:- horn_true((might_altercate(ted, bill))).
:- horn_true((might_altercate(bill, ted))).

% get proof
:- sanity(call(pfc_get_support(not(love_compatible(bill, alice))   ,Why))).
% O = (not(likes(bill, alice)), pt(not(likes(bill, alice)), rhs([not(love_compatible(bill, alice))]))) ;
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

% human(P) => (female(P) v male(P)).
clif(if(human(P), (female(P) v male(P)))).


((parent(X,Y) & female(X)) <=> mother(X,Y)).

:- horn_true(((parent(X,Y) & female(X)) <=> mother(X,Y))).
:- horn_true(((parent(X,Y) & female(X)) => mother(X,Y))).
:- horn_true((mother(X,Y)=>(parent(X,Y) & female(X)))).
:- horn_true((parent(A,B):-mother(A,B))).
:- horn_true(not(mother(A,B)):-not(parent(A,B))).

((parent(X,Y) & female(X)) => mother(X,Y)).
((mother(X,Y) => parent(X,Y) & female(X))).

:- horn_true((parent(A,B):-mother(A,B))).




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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  % /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/plarkc/logicmoo_i_clif.pl:309
%  kif :-
%          all(M,
%              all(F, all(P, exists([M, F], (human(P)=>mother(M, P)&father(F, P)))))).
%  %
%  % /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/plarkc/logicmoo_i_clif.pl:309
%  pkif :-
%          all(M,
%              all(F,
%                  all(P,
%                      exists(M, exists(F, (human(P)=>mother(M, P)&father(F, P))))))).
%  %
%  % /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/plarkc/logicmoo_i_clif.pl:309
%  cnf :-
%          (not(skolem(M, skArg1ofMotherFn(P)))v (not(skolem(F, skArg1ofFatherFn(P)))v (not(human(P))v mother(M, P))))& (not(skolem(M, skArg1ofMotherFn(P)))v (not(skolem(F, skArg1ofFatherFn(P)))v (not(human(P))v father(F, P)))).
%  %
%  % /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/plarkc/logicmoo_i_clif.pl:309
%  horn :-
%  
%          [ (not(human(P)):-skolem(M, skArg1ofMotherFn(P)), skolem(F, skArg1ofFatherFn(P)), not(father(F, P))),
%            (not(human(P)):-skolem(M, skArg1ofMotherFn(P)), skolem(F, skArg1ofFatherFn(P)), not(mother(M, P))),
%            (not(skolem(F, skArg1ofFatherFn(P))):-skolem(M, skArg1ofMotherFn(P)), human(P), not(father(F, P))),
%            (not(skolem(F, skArg1ofFatherFn(P))):-skolem(M, skArg1ofMotherFn(P)), human(P), not(mother(M, P))),
%            (not(skolem(M, skArg1ofMotherFn(P))):-skolem(F, skArg1ofFatherFn(P)), human(P), not(father(F, P))),
%            (not(skolem(M, skArg1ofMotherFn(P))):-skolem(F, skArg1ofFatherFn(P)), human(P), not(mother(M, P))),
%            (father(F, P):-skolem(M, skArg1ofMotherFn(P)), skolem(F, skArg1ofFatherFn(P)), human(P)),
%            (mother(M, P):-skolem(M, skArg1ofMotherFn(P)), skolem(F, skArg1ofFatherFn(P)), human(P))
%          ].
%  %
%  % /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/plarkc/logicmoo_i_clif.pl:309
%  pfc :-
%  
%          [ (neg(human(P))<-neg(father(F, P)), {vg(s(P))}),
%            (neg(human(P))<-neg(mother(M, P)), {vg(s(P))}),
%            true,
%            true,
%            true,
%            true,
%            (if_missing(father(F, P), father(skArg1ofFatherFn(P), P))<={ignore(M=skArg1ofMotherFn(P))}, human(P), {vg(s(P))}),
%            (if_missing(mother(M, P), mother(skArg1ofMotherFn(P), P))<={ignore(F=skArg1ofFatherFn(P))}, human(P), {vg(s(P))})
%          ].
%  %
%  % /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/plarkc/logicmoo_i_clif.pl:309
%  %                succeed(user:must(repropagate(arity(if_missing, _)))).
%  %
%  
%  % Removing (pfc_rem1(neg(arity(if_missing,2)), (u,u))) neg(arity(if_missing,2)).
%  
%  % /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/plarkc/logicmoo_i_clif.pl:309
%  %     succeed(user:must(repropagate(arity(if_missing, _)))).
%  %
%  % /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/plarkc/logicmoo_i_clif.pl:309
%  %      succeed(user:must(repropagate(argIsa(if_missing, 2, ftPercent)))).
%  %
%  % /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/plarkc/logicmoo_i_clif.pl:309
%  %       succeed(user:must(repropagate(argIsa(if_missing, 2, ftInt)))).
%  %


:-show_call(must(father(_,trudy))).


mother(trudy,eileen).
((human(P1),ancestor(P1,P2))=>human(P2)).
:- listing([ancestor,human,parent]).
%:- wdmsg("press Ctrl-D to resume.").
%:- prolog.

:-horn_true(grandmother(trudy,douglas)).

mother(trudy,robby).
mother(trudy,liana).
mother(liana,matt).
mother(liana,liz).
mother(trudy,pam).


% this fact sets off the anscesteral rule that her decendants are humans to
% human(trudy).

% therefore
:-horn_true(human(douglas)).

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

%:- wdmsg("press Ctrl-D to resume.").


:- style_check(-singleton).


% so far no males "asserted" in the KB
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

% we can report the presence on non male though...
%    the ~/1 is our negation hook into the inference engine 
:-doall(show_call(~male(Who ))).
% we expect to see at least there mothers here
%  succeed(user: ~male(liana)).
%  succeed(user: ~male(trudy)).
%            succeed(user: ~male(skArg1ofMotherFn(trudy))).
%  succeed(user: ~male(eileen)).

% thus ~/1 is tnot/1 of XSB ?!?

% there ar explicly non females
:-doall(show_call(~ female(Who ))).

% ensure skolems are made or destroyed

father(robert,eileen).
siblings(douglas,cassiopea).
father(douglas,sophiaWebb).
father(douglas,skylar).
father(douglas,sophiaWisdom).
father(douglas,zaltana).

:-doall(show_call(mother(Female,Who))).
%  succeed(user:mother(eileen, douglas)).
%  succeed(user:mother(liana, liz)).
%  succeed(user:mother(liana, matt)).
%  succeed(user:mother(skArg1ofMotherFn(skylar), skylar)).
%  succeed(user:mother(skArg1ofMotherFn(sophiaWebb), sophiaWebb)).
%  succeed(user:mother(skArg1ofMotherFn(sophiaWisdom), sophiaWisdom)).
%  succeed(user:mother(skArg1ofMotherFn(trudy), trudy)).
%  succeed(user:mother(skArg1ofMotherFn(zaltana), zaltana)).
%  succeed(user:mother(trudy, eileen)).
%  succeed(user:mother(trudy, liana)).
%  succeed(user:mother(trudy, pam)).
%  succeed(user:mother(trudy, robby)).


:-doall(show_call(father(Female,Who))).
%  succeed(user:father(douglas, skylar)).
%  succeed(user:father(douglas, sophiaWebb)).
%  succeed(user:father(douglas, sophiaWisdom)).
%  succeed(user:father(douglas, zaltana)).
%  succeed(user:father(robert, eileen)).
%  succeed(user:father(skArg1ofFatherFn(douglas), douglas)).
%  succeed(user:father(skArg1ofFatherFn(eileen), eileen)).
%  succeed(user:father(skArg1ofFatherFn(liana), liana)).
%  succeed(user:father(skArg1ofFatherFn(liz), liz)).
%  succeed(user:father(skArg1ofFatherFn(matt), matt)).
%  succeed(user:father(skArg1ofFatherFn(pam), pam)).
%  succeed(user:father(skArg1ofFatherFn(robby), robby)).
%  succeed(user:father(skArg1ofFatherFn(trudy), trudy)).


:-doall(show_call(male(Who))).
%  succeed(user:male(douglas)).
%  succeed(user:male(robert)).
%  succeed(user:male(skArg1ofFatherFn(douglas))).
%  succeed(user:male(skArg1ofFatherFn(eileen))).
%  succeed(user:male(skArg1ofFatherFn(liana))).
%  succeed(user:male(skArg1ofFatherFn(liz))).
%  succeed(user:male(skArg1ofFatherFn(matt))).
%  succeed(user:male(skArg1ofFatherFn(pam))).
%  succeed(user:male(skArg1ofFatherFn(robby))).
%  succeed(user:male(skArg1ofFatherFn(trudy))).

:-doall(show_call(female(Who))).
%  succeed(user:female(eileen)).
%  succeed(user:female(liana)).
%  succeed(user:female(skArg1ofMotherFn(skylar))).
%  succeed(user:female(skArg1ofMotherFn(sophiaWebb))).
%  succeed(user:female(skArg1ofMotherFn(sophiaWisdom))).
%  succeed(user:female(skArg1ofMotherFn(trudy))).
%  succeed(user:female(skArg1ofMotherFn(zaltana))).
%  succeed(user:female(trudy)).

:-doall(show_call(siblings(Who,AndWho))).
%  succeed(user:siblings(douglas, cassiopea)).
%  succeed(user:siblings(eileen, liana)).
%  succeed(user:siblings(eileen, pam)).
%  succeed(user:siblings(eileen, robby)).
%  succeed(user:siblings(liana, eileen)).
%  succeed(user:siblings(liana, pam)).
%  succeed(user:siblings(liana, robby)).
%  succeed(user:siblings(liz, matt)).
%  succeed(user:siblings(matt, liz)).
%  succeed(user:siblings(pam, eileen)).
%  succeed(user:siblings(pam, liana)).
%  succeed(user:siblings(pam, robby)).
%  succeed(user:siblings(robby, eileen)).
%  succeed(user:siblings(robby, liana)).
%  succeed(user:siblings(robby, pam)).
%  succeed(user:siblings(skylar, sophiaWebb)).
%  succeed(user:siblings(skylar, sophiaWisdom)).
%  succeed(user:siblings(skylar, zaltana)).
%  succeed(user:siblings(sophiaWebb, skylar)).
%  succeed(user:siblings(sophiaWebb, sophiaWisdom)).
%  succeed(user:siblings(sophiaWebb, zaltana)).
%  succeed(user:siblings(sophiaWisdom, skylar)).
%  succeed(user:siblings(sophiaWisdom, sophiaWebb)).
%  succeed(user:siblings(sophiaWisdom, zaltana)).
%  succeed(user:siblings(zaltana, skylar)).
%  succeed(user:siblings(zaltana, sophiaWebb)).
%  succeed(user:siblings(zaltana, sophiaWisdom)).

