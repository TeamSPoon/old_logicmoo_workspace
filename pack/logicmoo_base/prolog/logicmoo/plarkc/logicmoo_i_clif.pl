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
:- ensure_loaded(logicmoo_i_compiler).
:- enable_mpred_expansion.

are_clauses_entailed(E):-not(compound(E)),!,must(true==E).
are_clauses_entailed([E|List]):-is_list([E|List]),!,maplist(are_clauses_entailed,[E|List]).
are_clauses_entailed((C,L)):-!,are_clauses_entailed(C),are_clauses_entailed(L).
are_clauses_entailed(CL):- unnumbervars(CL,UCL),  !, \+ \+ show_call_failure(is_entailed(UCL)),!.

is_entailed(UCL):-clause_asserted(UCL),!.
is_entailed(UCL):-pfc_call(UCL),!.
 
member_ele(E,E):- (\+ (compound(E))),!.
member_ele([L|List],E):- is_list([L|List]),!,member(EE,[L|List]),member_ele(EE,E).
member_ele((H,T),E):- nonvar(H),nonvar(T),!, (member_ele(H,E);member_ele(T,E)).
member_ele(E,E).

delistify_last_arg(Arg,Pred,Last):-is_list(Arg),!,member(E,Arg),delistify_last_arg(E,Pred,Last).
delistify_last_arg(Arg,Pred,Last):- Pred=..[F|ARGS],append([Arg|ARGS],[NEW],NARGS),NEWCALL=..[F|NARGS],show_call(NEWCALL),!,member_ele(NEW,Last).

% sanity that mpreds (manage prolog prodicate) are abily to transform
:- thlocal:disable_mpred_term_expansions_locally->throw(thlocal:disable_mpred_term_expansions_locally);true.

% cwc "code-wise chaining" is always true in Prolog but will throw programming error if evalled in LogicMOO Prover.
% Use this to mark code and not axiomatic prolog

clif_to_prolog(CLIF,Prolog):-cwc,is_list(CLIF),!,must_maplist(clif_to_prolog,CLIF,Prolog).
clif_to_prolog((H,CLIF),(T,Prolog)):-cwc,sanity(must(nonvar(H))),!,trace,clif_to_prolog(H,T),clif_to_prolog(CLIF,Prolog).
clif_to_prolog((H<-B),(H<-B)):- cwc,!.
clif_to_prolog((P==>Q),(P==>Q)):- cwc,!.
clif_to_prolog((H:-B),PrologO):- cwc,!,must((show_call_failure(boxlog_to_pfc((H:-B),Prolog)),!,=(Prolog,PrologO))),!.
clif_to_prolog(CLIF,PrologO):- cwc,
  % somehow integrate why_to_id(tell,Wff,Why),
     must_det_l((
      kif_to_boxlog(CLIF,HORN),
      boxlog_to_pfc(HORN,Prolog),
      dmsg(pfc:-Prolog),
      =(Prolog,PrologO))),!.


% Sanity Test for expected side-effect entailments
% why does renumbervars work but not copy_term? 
clif_must(CLIF):- cwc, sanity((clif_to_prolog(CLIF,Prolog),!,sanity(( \+ \+ (show_call_failure(are_clauses_entailed(Prolog))))))),!.

% Sanity Test for required absence of specific side-effect entailments
clif_must_not(CLIF):- cwc, sanity((clif_to_prolog(CLIF,Prolog),show_call_failure(\+ are_clauses_entailed(Prolog)))),!.


:-op(1190,xfx,(:-)).
:-op(1200,fy,(clif_must)).

% this defines a recogniser for clif syntax (well stuff that might be safe to send in thru kif_to_boxlog)
is_clif(all(_,X)):-cwc,compound(X),!,is_clif(X).
is_clif(forall(_,X)):-cwc,compound(X),!.
is_clif(CLIF):-cwc,
  VVs = v(if,iff,clif_forall,all,exists), % implies,equiv,forall
   (var(CLIF)-> (arg(_,VVs,F),functor(CLIF,F,2));
     compound(CLIF),functor(CLIF,F,2),arg(_,VVs,F)).

% we ensure we are in "pfc" consultation mode (so the syntax rules will define correctly)

% :- pfc_trace.
:- file_begin(pfc).

% make sure op alias for '=>' is not overriden
:- op_alias( (=>),  (=>)).

==> hybrid_support(clif,1).

% whenever we know about clif we'll use the prolog forward chainging system

(clif(CLIF) ==> 
   ({ clif_to_prolog(CLIF,PROLOG)},
      % this consequent asserts the new rules
      PROLOG,{sanity(clif_must(CLIF))})).

% :- prolog.

% pfc_examples.

arity(clif,1).
arity(boxlog,1).
arity(pfclog,1).

(clif(CLIF),{delistify_last_arg(CLIF,kif_to_boxlog,PROLOG)}) ==> boxlog(PROLOG).
(boxlog(CLIF),{delistify_last_arg(CLIF,boxlog_to_pfc,PROLOG)}) ==> pfclog(PROLOG).
% :- pfc_trace.
(pfclog(PROLOG)==>(PROLOG,{slow_sanity(clif_must(PROLOG))})).


/*
(clif(CLIF),{member_ele(CLIF,E)}) ==> clif1(E).
(clif1(CLIF),{kif_to_boxlog(CLIF,PROLOG)}) ==> boxlog(PROLOG).

(boxlog(CLIF),{member_ele(CLIF,E)}) ==> boxlog1(E).
(boxlog1(CLIF),{boxlog_to_pfc(CLIF,PROLOG)}) ==> pfclog(PROLOG).

(pfclog(CLIF),{member_ele(CLIF,E)}) ==> pfclog1(E).
(pfclog1(PROLOG)==>(PROLOG,{clif_must(PROLOG)})).
*/

% we create code syntax listeners for [if,iff,clif_forall,all,exists]/2s
({is_clif(CLIF)} ==>
  (CLIF/is_clif(CLIF) ==> clif(CLIF))).


end_of_file.



:- if( if_defined(pfc_examples,user:startup_option(clif,sanity_tests))).

:- wdmsg(pfc_trace).

:- pfc_trace.

% see logicmoo_i_compiler.pl for more info
:- set_clause_compile(fwc).


% if two like each other then they are love compatible
clif(
 forall(a,forall(b,
    if( (likes(a,b)  & likes(b,a)), 
     love_compatible(a,b))))).
:- pfc_no_trace.


% will have the side effects... 

% if A is not love compatible with B .. yet B likes A.. we must conclude A must not like B back.
:- clif_must((not(likes(A, B)):-not(love_compatible(A, B)), likes(B, A))).
% if A is not love compatible with B .. yet A likes B.. we must conclude B must not like A back.
:- clif_must((not(likes(B, A)):-not(love_compatible(A, B)), likes(A, B))).
% if A and B like each other both ways then they are love compatible
:- clif_must((love_compatible(A, B):-likes(A, B), likes(B, A))).


% if people are love compatible then they must like each other
clif(
  forall(a,forall(b,
   if(love_compatible(a,b), 
    (likes(a,b)  & likes(b,a)))))).

 
% will have the side effects... 

% if A and B must not be love compatible since A does not like B
:- clif_must((not(love_compatible(A, B)):-not(likes(A, B)))).
% if A and B must not be love compatible since B does not like A
:- clif_must((not(love_compatible(A, B)):-not(likes(B, A)))).
% obviously A likes B .. since they are love compatible
:- clif_must((likes(A, B):-love_compatible(A, B))).
% obviously B likes A .. after all they are love compatible
:- clif_must((likes(B, A):-love_compatible(A, B))).


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
% Since we can prove A and B  dislike each other we can prove they are scrap compatible
:- clif_must((might_altercate(A, B):-dislikes(A, B), dislikes(B, A))).
%  A dislikes B  when we prove A and B are scrap compatible somehow  (this was due to the biconditional implicatature)
:- clif_must((dislikes(A, B):-might_altercate(A, B))).
%  B dislikes A  when we prove A and B are scrap compatible
:- clif_must((dislikes(B, A):-might_altercate(A, B))).


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
:- clif_must((not(dislikes(A, B)):-likes(A, B))).
% A does not like B when A dislikes B
:- clif_must((not(likes(A, B)):-dislikes(A, B))).
% A does dislikes B when we can somehow prove A not liking B
:- clif_must((dislikes(A, B):-not(likes(A, B)))).
% A does likes B when we can somehow prove A not disliking B
:- clif_must((likes(A,B) :- not(dislikes(A,B)))).

% The above assertions forward chain to these side-effects... 
:- clif_must((not(love_compatible(bill, alice)))).
:- clif_must((not(love_compatible(alice, bill)))).
:- clif_must((might_altercate(ted, bill))).
:- clif_must((might_altercate(bill, ted))).

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

:- clif_must(((parent(X,Y) & female(X)) <=> mother(X,Y))).
:- clif_must(((parent(X,Y) & female(X)) => mother(X,Y))).
:- clif_must((mother(X,Y)=>(parent(X,Y) & female(X)))).
:- clif_must((parent(A,B):-mother(A,B))).
:- clif_must(not(mother(A,B)):-not(parent(A,B))).

((parent(X,Y) & female(X)) => mother(X,Y)).
((mother(X,Y) => parent(X,Y) & female(X))).

:- clif_must((parent(A,B):-mother(A,B))).




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

:-clif_must(grandmother(trudy,douglas)).

mother(trudy,robby).
mother(trudy,liana).
mother(liana,matt).
mother(liana,liz).
mother(trudy,pam).


% this fact sets off the anscesteral rule that her decendants are humans to
% human(trudy).

% therefore
:-clif_must(human(douglas)).

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


% unaliasing
:- op_alias( (<=>),  <=>).
:- op_alias(  (=>),   =>).


% break to the debugger
%:- wdmsg("press Ctrl-D to resume.").
%:- break.

:- endif.

