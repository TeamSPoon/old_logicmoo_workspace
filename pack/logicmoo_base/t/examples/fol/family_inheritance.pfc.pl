#!/usr/bin/env swipl

:- module(fi,[]).


:-
 op(1199,fx,('==>')), 
 op(1190,xfx,('::::')),
 op(1180,xfx,('==>')),
 op(1170,xfx,'<==>'),  
 op(1160,xfx,('<-')),
 op(1150,xfx,'=>'),
 op(1140,xfx,'<='),
 op(1130,xfx,'<=>'), 
 op(600,yfx,'&'), 
 op(600,yfx,'v'),
 op(350,xfx,'xor'),
 op(300,fx,'~'),
 op(300,fx,'-').

:- op(1100,fx,(shared_multifile)).

% :- include('test_header.pfc').
:- use_module(library(logicmoo/logicmoo_user)).

:- baseKB:with_ukb(baseKB,baseKB:ensure_mpred_file_loaded(logicmoo(snark/'common_logic_clif.pfc'))).

:- begin_pfc.

:- process_this_script.

%=  setup pfc
:- file_begin(pfc).

%= save compiled clauses using forward chaining storage (by default)
%= we are using forward chaining just so any logical errors, performance and program bugs manefest
%= immediately
:- set_clause_compile(fwc).

must_is_entailed(G):- must(is_entailed(G)).


show_test(G):- get_user_abox(KB),printAll(must(KB:G)).

%= ````
%= logic tests...
%= ````
%:- debug(mpred).

prologBuiltin(otherGender/2).
otherGender(male,female).
otherGender(female,male).

tCol(male).
:- dynamic((bore_offspring/2, gender/2)).
bore_offspring(X,Y) <=> bore_offspring(Y,X).

(bore_offspring(X,Y),gender(X,G1), otherGender(G1,G2))
     => gender(Y,G2).


gender(P,male) <=> male(P).
gender(P,female) <=> female(P).

male(P) <=> ~female(P).




% seven rules
(((parent(M,C) & female(M)) <=> mother(M,C))).



% ((parent(M,C) & female(M)) <=> mother(M,C)).
:- must_is_entailed(((parent(M,C) & female(M)) <=> mother(M,C))).


:- must_is_entailed(((parent(M,C) & female(M)) => mother(M,C))).
%       [ (not(female(M)):-not(mother(M, C)), parent(M, C)),
%         (not(parent(M, C)):-not(mother(M, C)), female(M)),
%         (mother(M, C):-parent(M, C), female(M))
%       ].

:- must_is_entailed((mother(M,C) => (parent(M,C) & female(M)))).
%       [ (female(M):-mother(M, _C)),
%         (not(mother(M, _C)):-not(female(M))),
%         (not(mother(M, C)):-not(parent(M, C))),
%         (parent(M, C):-mother(M, C))
%       ].


:- must_is_entailed(not(mother(M,C)):- not(parent(M,C))).
:- must_is_entailed(not(mother(M,_Anyone)):- not(female(M))).
:- must_is_entailed((parent(M,C):- mother(M,C))).
:- must_is_entailed((female(M):- mother(M,_))).




parent(GRAND,PARENT),parent(PARENT,CHILD) => grandparent(GRAND,CHILD).

grandparent(X,Y),male(X) <=> grandfather(X,Y).
grandparent(X,Y),female(X) <=> grandmother(X,Y).
mother(Ma,Kid),parent(Kid,GrandKid)
      =>grandmother(Ma,GrandKid).
grandparent(X,Y),female(X) <=> grandmother(X,Y).


parent(X,Y),male(X) <=> father(X,Y).
parent(Ma,X),parent(Ma,Y),different(X,Y) =>siblings(X,Y).
parent(P1,P2) => ancestor(P1,P2).
(parent(P1,P2), ancestor(P2,P3)) => ancestor(P1,P3).
(ancestor(P1,P2), ancestor(P2,P3)) => ancestor(P1,P3).

mother(eileen,douglas).

%= trudy is human
human(trudy).

%= catch a regression bug that may couse trudy to lose human assertion
never_retract_u(human(trudy)).


% :- kif_add(forall(p,exists([m,f], if(human(p), (mother(m,p) & father(f,p)))))).
forall(p,exists([m,f], if(human(p), (mother(m,p) & father(f,p))))).

:- printAll(must(father(_,trudy))).


mother(trudy,eileen).
((human(P1),ancestor(P1,P2))=>human(P2)).
% :- listing([ancestor,human,parent]).
%=:- wdmsg("press Ctrl-D to resume.").
%=:- prolog.

grandmother(trudy,douglas).

mother(trudy,robby).
mother(trudy,liana).
mother(liana,matt).
mother(liana,liz).
mother(trudy,pam).


%= human(trudy) supports anscesteral rule that her decendants are humans as well .. therefore ..
:- must_is_entailed(human(douglas)).


?- mpred_why(human(douglas)).
/*


Justifications for human(douglas):
    1.1 ancestor(trudy,douglas)
    1.2 human(trudy)
    1.3 human(trudy),ancestor(trudy,douglas),{vg(s(douglas))}==>human(douglas)
    2.1 ancestor(eileen,douglas)
    2.2 human(eileen)
    2.3 human(eileen),ancestor(eileen,douglas),{vg(s(douglas))}==>human(douglas)


:- mpred_why(grandparent(trudy,douglas)).

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

:- show_test(male(Who)).

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
:- no_varnaming( mpred_no_chaining(doall((show_call(~male(Who )))))).


%= we expect to see at least there mothers here
%=  succeed(user: ~male(liana)).
%=  succeed(user: ~male(trudy)).
%=            succeed(user: ~male(skArg1ofMotherFn(trudy))).
%=  succeed(user: ~male(eileen)).

%= thus ~/1 is tnot/1 of XSB ?!?

%= there are explicly non females
:- show_test(~(female(Who))).

%= ensure skolems are made or destroyed

father(robert,eileen).
siblings(douglas,cassiopea).
father(douglas,sophiaWebb).
father(douglas,skylar).
father(douglas,sophiaWisdom).
father(douglas,zaltana).

:- must_is_entailed(human(douglas)).

:- mpred_why(human(douglas)).

:- mpred_why(grandparent(trudy,douglas)).

% :- mpred_trace_exec.
:- show_test((mother(Female,Who))).

:- show_test(father(Male,Who)).

% :- show_test((male(Who))).

% :- show_test((female(Who))).

:- show_test((siblings(Who,AndWho))).

%= human(P) => (female(P) v male(P)).
if(gendered_human(P), (female(P) v male(P))).



