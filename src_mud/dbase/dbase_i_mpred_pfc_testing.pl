/** <module> dbase_i_mpred_pfc_testing
% Tests a prolog database replacent that uses PFC
%  
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/

:- include(dbase_i_header).

user:term_expansion(A,B):- once(pfc_file_expansion(A,B)),A\=@=B.

:- pfcTrace.
%:- pfcWatch.
:- pfcWarn.
next_test :- sleep(1),pfcReset.

:-dynamic((disjointWith/2,genls/2)).

(disjointWith(P1,P2) , genls(C1,P1)) =>    disjointWith(C1,P2).
disjointWith(Sub, Super) => disjointWith( Super, Sub).
disjointWith(tObj,tRegion).
disjointWith(ttSpatialType,ttAbstractType).


tCol(Col) <=> isa(Col,tCol).

(isa(I,Sub), genls(Sub, Super)) => isa(I,Super).



(isa(I,Sub), disjointWith(Sub, Super)) => not(isa(I,Super)).

genls(tPartOfobj,tItem).

% dividesBetween(tItem,tPathways).
dividesBetween(tItem,tMassfull,tMassless).
dividesBetween(tObj,tItem,tAgentGeneric).
dividesBetween(tObj,tMassfull,tMassless).
dividesBetween(tSpatialThing,tObj,tRegion).
dividesBetween(tAgentGeneric,tPlayer,tNpcPlayer).

dividesBetween(S,C1,C2) => (disjointWith(C1,C2) , genls(C1,S) ,genls(C2,S)).

disjointWith(P1,P2) => (not(isa(C,P1)) <=> isa(C,P2)).

isa(Col1, ttObjectType) => ~isa(Col1, ttFormatType).

=> tCol(tCol).
=> tCol(tPred).
=> tCol(tFunction).
=> tCol(tRelation).
=> tCol(ttSpatialType).
=> tCol(ttFormatType).
=> tCol(functorDeclares).
% tCol(ArgsIsa):-is_pred_declarer(ArgsIsa).
% TODO decide if OK
%tCol(F):-hasInstance(functorDeclares,F).
=> tCol(ttFormatType).
=> tCol(vtActionTemplate).
=> tCol(tRegion).
=> tCol(tContainer).

isa(tRegion,ttSpatialType).
isa(tRelation,ttAbstractType).



% a conflict triggers a Prolog action to resolve it.
conflict(C) => {resolveConflict(C)}.

% this isn't written yet.
resolveConflict(C) :-
  format("~NHalting with conflict ~w", [C]),
  pfcHalt.

% meta rules to schedule inferencing.

% resolve conflicts asap
pfcSelect(conflict(X)) :- pfcQueue(conflict(X)).
  
% a pretty basic conflict.
not(P), P => conflict(P).








  % -*-Prolog-*-
% here is an example which defines pfcDefault facts and rules.  Will it work?

(pfcDefault(P)/pfcAtom(P))  =>  (~not(P) => P).

pfcDefault((P => Q))/pfcAtom(Q) => (P, ~not(Q) => Q).

% birds fly by pfcDefault.
=> pfcDefault((bird(X) => fly(X))).

% here's one way to do an isa hierarchy.
% isa = genls.

isa(C1,C2) =>
  {P1 =.. [C1,X],
    P2 =.. [C2,X]},
  (P1 => P2).

=> isa(canary,bird).
=> isa(penguin,bird).

% penguins do not fly.
penguin(X) => not(fly(X)).

% chilly is a penguin.
:-(pfcAdd(=> penguin(chilly))).

% rtrace(Goal):- Goal. % (notrace((visible(+all),visible(+unify),visible(+exception),leash(-all),leash(+exception))),(trace,Goal),leash(+all)).

% :- gutracer.


:-prolog.
end_of_file.

:-next_test.
:-debug.


end_of_file.



































% dcg_pfc: translation of dcg-like grammar rules into pfc rules.

:- op(1200,xfx,'-->>').
:- op(1200,xfx,'--*>>').
% :- op(1200,xfx,'<<--').
:- op(400,yfx,'\').

% :- use_module(library(strings)), use_module(library(lists)).

term_expansion((P -->> Q),(:- fcAdd(Rule))) :-
  pfc_translate_rule((P -->> Q), Rule).
term_expansion((P --*>> Q),(:- fcAdd(Rule))) :-
  pfc_translate_rule((P --*>> Q), Rule).

pfc_translate_rule((LP-->>[]),H) :- !, pfc_t_lp(LP,Id,S,S,H).
pfc_translate_rule((LP-->>RP),(H <= B)):-
   pfc_t_lp(LP,Id,S,SR,H),
   pfc_t_rp(RP,Id,S,SR,B1),
   pfc_tidy(B1,B).


pfc_translate_rule((LP--*>>[]),H) :- !, pfc_t_lp(LP,Id,S,S,H).
pfc_translate_rule((LP--*>>RP),(B => H)):-
   pfc_t_lp(LP,Id,S,SR,H),
   pfc_t_rp(RP,Id,S,SR,B1),
   pfc_tidy(B1,B).

pfc_t_lp(X,Id,S,SR,ss(X,Id,(S\SR))) :- var(X),!.

pfc_t_lp((LP,List),Id,S,SR,ss(LP,Id,(S\List2))):- 
   !,
   pfcAppend(List,SR,List2).

pfc_t_lp(LP,Id,S,SR,ss(LP,Id,(S\SR))).

pfc_t_rp(!,Id,S,S,!) :- !.
pfc_t_rp([],Id,S,S1,S=S1) :- !.
pfc_t_rp([X],Id,S,SR,ss(word(X),Id,(S\SR))) :- !.
pfc_t_rp([X|R],Id,S,SR,(ss(word(X),Id,(S\SR1)),RB)) :- 
  !, 
  pfc_t_rp(R,Id,SR1,SR,RB).
pfc_t_rp({T},Id,S,S,{T}) :- !.
pfc_t_rp((T,R),Id,S,SR,(Tt,Rt)) :- !,
   pfc_t_rp(T,Id,S,SR1,Tt),
   pfc_t_rp(R,Id,SR1,SR,Rt).
pfc_t_rp((T;R),Id,S,SR,(Tt;Rt)) :- !,
   pfc_t_or(T,Id,S,SR,Tt),
   pfc_t_or(R,Id,S,SR,Rt).
pfc_t_rp(T,Id,S,SR,ss(T,Id,(S\SR))).

pfc_t_or(X,Id,S0,S,P) :-
   pfc_t_rp(X,Id,S0a,S,Pa),
 ( var(S0a), S0a \== S, !, S0=S0a, P=Pa;
   P=(S0=S0a,Pa) ).

pfc_tidy((P1;P2),(Q1;Q2)) :-
   !,
   pfc_tidy(P1,Q1),
   pfc_tidy(P2,Q2).
pfc_tidy(((P1,P2),P3),Q) :- 
   pfc_tidy((P1,(P2,P3)),Q).
pfc_tidy((P1,P2),(Q1,Q2)) :- 
   !,
   pfc_tidy(P1,Q1),
   pfc_tidy(P2,Q2).
pfc_tidy(A,A) :- !.

compile_pfcg :-
  ((retract((L -->> R)), pfc_translate_rule((L -->> R), PfcRule));
    (retract((L --*>> R)), pfc_translate_rule((L --*>> R), PfcRule))),
  fcAdd(PfcRule),
  fail.
compile_pfcg.

parse(Words) :- 
  parse(Words,Id),
  format("~Nsentence id = ~w",Id),
  show(Id,sentence(X)).

parse(Words,Id) :- 
  gen_s_tag(Id),
  parse1(Words,Id),
  fcAdd(sentence(Id,Words)).

parse1([],_) :- !.
parse1([H|T],Id) :-
 do(fcAdd(ss(word(H),Id,([H|T]\T)))),
 parse1(T,Id).


showSentences(Id) :- showSentences(Id,_).

showSentences(Id,Words) :-
  sentence(Id,Words),
  pfc(ss(s(S),Id,(Words\[]))),
  nl,write(S),
  fail.
showSentences(_,_).

do(X) :- call(X) -> true;true.

show(Id,C) :-
  pfc(ss(C,Id,A\B)),
  append(Words,B,A),
  format("~n ~w    :   ~w",[C,Words]),
  fail.

gen_s_tag(s(N2)) :-
  var(V),
  (retract(s_tag(N)); N=0),
  N2 is N+1,
  assert(s_tag(N2)).

make_term(ss(Constituent,Id,String),Term) :-
   Constituent =.. [Name|Args],
   name(Name,Name_string),
   name(Name2,[36|Name_string]),
   append([Name2|Args],[Id,String],Term_string),
   Term =.. Term_string.
append([],X,X).
append([H|T],L2,[H|L3]) :- append(T,L2,L3).



% -*-Prolog-*-

:- dynamic ('-->>')/2.
:- dynamic ('--*>>')/2.

% a simple pfc dcg grammar.  requires dcg_pfc.pl

% backward grammar rules.
s(s(Np,Vp)) -->> np(Np), vp(Vp).

vp(vp(V,Np)) -->> verb(V), np(Np).
vp(vp(V)) -->> verb(V).
vp(vp(VP,X)) -->> vp(VP), pp(X).

np(np(N,D)) -->> det(D), noun(N).
np(np(N)) -->> noun(N).
np(np(Np,pp(Pp))) -->> np(Np), pp(Pp).

pp(pp(P,Np)) -->> prep(P), np(Np).

% forward grammar rules.
P --*>>  [W],{cat(W,Cat),P =.. [Cat,W]}.

% simple facts.
cat(the,det).
cat(a,det).
cat(man,noun).
cat(fish,noun).
cat(eats,verb).
cat(catches,verb).
cat(in,prep).
cat(on,prep).
cat(house,noun).
cat(table,noun).



















% tweety is a canary.
=> canary(tweety).

%% some simple tests to see if Pfc is working properly

time(Call,Time) :-
  statistics(runtime,_),
  db_call(Call),
  statistics(runtime,[_,Time]).


%test0 
:- 
  add([(p(X) => q),
       p(1),
       (p(X), ~r(X) => s(X)),
       (t(X), {X>0} => r(X)),
       (t(X), {X<0} => minusr(X)),
       t(-2),
       t(1)]).

%test1 
:-
  consult('pfc/kinship.pfc'),
  consult('pfc/finin.pfc').

:-dynamic((a/2,b/2,found/1)).

%test2 
:-
  add([(a(X),~b(Y)/(Y>X) => biggest(a)),
       (b(X),~a(Y)/(Y>X) => biggest(b)),
        a(5)]).


test3 :-
  add([(a(X),\+(b(Y))/(Y>X) => biggest(a)),
       (b(X),\+a((Y))/(Y>X) => biggest(b)),
        a(5)]).


%test4 
:-
    add([(foo(X), bar(Y)/{X=:=Y} => foobar(X)),
         (foobar(X), go => found(X)),
	 (found(X), {X>=100} => big(X)),
	 (found(X), {X>=10,X<100} => medium(X)),
	 (found(X), {X<10} => little(X)),
	 foo(1),
	 bar(2),
	 bar(1),
	 foo(100),
	 goAhead,
	 bar(100)
	]).


%test5 
:-
    add([(faz(X), ~baz(Y)/{X=:=Y} => fazbaz(X)),
         (fazbaz(X), go => found(X)),
	 (found(X), {X>=100} => big(X)),
	 (found(X), {X>=10,X<100} => medium(X)),
	 (found(X), {X<10} => little(X)),
	 faz(1),
	 goAhead,
	 baz(2),
	 baz(1)
	]).


%test6 
:-
    add([(d(X), ~f(Y)/{X=:=Y} => justD(X)),
         (justD(X), go => dGo(X)),
	 d(1),
	 go,
	 f(1)
	]).


%test7 
:-
    add([(g(X), h(Y)/{X=:=Y} => justG(X)),
         (justG(X), go => gGo(X)),
	 g(1),
	 go,
	 h(1)
	]).


test8 :-
    add([(j(X), k(Y) => bothJK(X,Y)),
         (bothJK(X,Y), go => jkGo(X,Y)),
	 j(1),
	 go,
	 k(2)
	]).


test9 :-
    add([(j(X), k(Y) => bothJK(X,Y)),
         (bothJK(X,Y) => jkGo(X,Y)),
	 j(1),
	 k(2)
	]).

test10 :-
  add([
	(j(X), k(Y) => bothJK(X,Y)),
	(bothJK(X,Y), go => jkGo(X,Y)),
	j(1),
	go,
	k(2)
       ]).


:-next_test. % ==


% if we learn that someone has a full name, then we know they are a user.
full_name(U,_) => user(U).


% if we learn that someone has a host name, then we know they are a user.
host_name(U,_) => user(U).


% when we know a user's full name and host name, make a user/3 assertion.
user(User),
full_name(User,Name),
host_name(User,Host) 
  =>
  user(User,Name,Host).


% the pfcDefault full_name for a user is 'unknown'.
user(User),
~full_name(User,X)/(X\==unknown)
  =>
full_name(User,unknown).
  


% the pfcDefault host_name for a user is 'unknown'.
user(User),
~host_name(User,X)/(X\==unknown)
  =>
host_name(User,unknown).




=> full_name(finin,'Tim Finin').

=> host_name(finin,antares).

=> full_name(robin,'Robin,McEntire').

=> host_name(fritzson,hamlet).




:-next_test. % ==

skCheck(eq(_,_),[]) :- !.

skCheck(P,Rules) :-
  sk(P,L),
  bagof(Rule,
        S^(member(S,L),
           skNoticer(P,S,Rule)),
        Rules).
         
% L is a list of the skolem constants found in the term P.
sk(P,L) :- sk1(P,[],L).

sk1(P,L,[P|L]) :- 
  skolemConstant(P),
  !,
  \+member(P,L),
  !.

sk1(P,L,L) :- 
  skolemConstant(P),
  !.

sk1(P,L,L) :- atomic(P),!.

sk1([Head|Tail], Lin, Lout) :-
  !,
  sk1(Head,Lin,Ltemp),
  sk1(Tail,Ltemp,Lout).

sk1(P,Lin,Lout) :-
  P =.. Plist,
  sk1(Plist,Lin,Lout).

% a skolem constant is any term sk/1.
skolemConstant(sk(_)).


% make a Pfc rule to add new facts based on equality info about skolem terms.
skNoticer(P,Sk,(eq(Sk,X)=>P2)) :- termSubst(Sk,X,P,P2).
  

% list Lisp's subst, but for terms.

termSubst(Old,New,Old,New) :- !.

termSubst(_,_,Term,Term) :- atomic(Term),!.

termSubst(Old,New,[Head|Tail],[Head2|Tail2]) :- 
  !,
  termSubst(Old,New,Head,Head2),
  termSubst(Old,New,Tail,Tail2).

termSubst(Old,New,Term,Term2) :-
  Term =.. TermList,
  termSubst(Old,New,TermList,TermList2),
  Term2 =.. TermList2.

%:- add((P/(\+P=eq(_,_)) => {skCheck(P,Rules)}, Rules)).
:- add((P => {skCheck(P,Rules)}, Rules)).

:- add((eq(X,Y) <=> eq(Y,X))).




:-next_test. % ==
%% a simple Knowledge Representation Language:
%%   class(Class)
%%   isa(Individual,Class)
%%   genls(SuperClass,SubClass)
%%   role(Class,Role)
%%   type(Class,Role,Type)
%%   range(Class,Role,Range)


% roles are inherited.
role(Super,R), genls(Super,Sub) => role(Sub,R).

% types are inherited.
type(Super,Role,Type), genls(Super,Sub) => type(Sub,Role,Type).

% classification rule
genls(Super,Sub),
      genls(Super,SubSub),
      {Sub \== SubSub},
      \+ not(subsumes(Sub,SubSub)),
      \+ not(primitive(SubSub))
      =>
      genls(Sub,SubSub).

disjoint(C1,C2) => disjoint(C2,C1).

not(subsume(C1,C2)) <= genls(C2,C1).

not(subsumes(C1,C2)) <= disjoint(C1,C2).

not(subsumes(C1,C2)) <=
  % we can't infer that C1 subsumes C2 if C1 has a role that C2 doen't.
  role(C1,R),
  \+ role(C2,R).

not(subsumes(C1,C2)) <=
  % we can't infer that C1 subsumes C2 if C1 has a role a type that...
  type(C1,R,T1),
  type(C2,R,T2),
  not(subsume(T1,T2)).

:-export otherGender/2.
:-next_test. % ==

% kinship domain example.

spouse(P1,P2) <=> spouse(P2,P1).

spouse(P1,P2), gender(P1,G1), {otherGender(G1,G2)} => gender(P2,G2).

=>otherGender(male,female).
=>otherGender(female,male).

gender(P,male) <=> male(P).

gender(P,female) <=> female(P).

parent(X,Y), female(X) <=> mother(X,Y).

parent(P1,P2), parent(P2,P3) => grandParent(P1,P3).

grandParent(P1,P2), male(P1) <=> grandFather(P1,P2).

grandParent(P1,P2), female(P1) <=> grandMother(P1,P2).

mother(Ma,Kid), parent(Kid,GrandKid) => grandMother(Ma,GrandKid).

parent(X,Y), male(X) <=> father(X,Y).

parent(Ma,P1), parent(Ma,P2), {P1\==P2} =>  sibling(P1,P2).

spouse(P1,P2), spouse(P1,P3), {P2\==P3} => 
   bigamist(P1), 
   {format("~N~w is a bigamist, married to both ~w and ~w~n",[P1,P2,P3])}.

% here is an example of a pfcDefault rule

parent(P1,X), 
  parent(P2,X)/(P1\==P2),
  \+ spouse(P1,P3)/(P3\==P2),
  \+ spouse(P2,P4)/(P4\==P1)
  =>
  spouse(P1,P2).

uncle(U,P1), parent(U,P2) => cousin(P1,P2).

aunt(U,P1), parent(U,P2) => cousin(P1,P2).

parent(P,K), sibling(P,P2)

   =>

   (female(P2) 
     => 
     aunt(P2,K),
     (spouse(P2,P3) => uncle(P3,K))),

   (male(P2) 
     => 
     uncle(P2,K),
     (spouse(P2,P3) => aunt(P3,K))).


:-next_test. % ==

%% equality axiomm

equal(A,B) => equal(B,A).

equal(A,B),{\+A=B},equal(B,C),{\+A=C} => equal(A,C).

notequal(A,B) => notequal(B,A).

notequal(A,B),equal(A,C) => notequal(C,B).


show_pfc_fact(P) :- send_editor(['(show-assertion "',P,'")']).

hide_pfc_fact(P) :- send_editor(['(hide-assertion "',P,'")']).

demons(P, WhenAdded, WhenRemoved) =>
  (P => {WhenAdded}),
  fcUndoMethod(WhenAdded,WhenRemoved).

show(P) => demons(P,show_pfc_fact(P),hide_pfc_fact(P)).


:-next_test. % ==

:- op(1050,xfx,('==>')).

(P ==> Q) => 
  (P => Q),
  (not(Q) => not(P)).


or(P,Q) => 
  (not(P) => Q),
  (not(Q) => P).
		
prove_by_contradiction(P) :- P.
prove_by_contradiction(P) :-
  \+ (not(P) ; P),
  add(not(P)),
  P -> pfcRem(not(P))
    ; (pfcRem(not(P)),fail).

=> or(p,q).
=> (p ==> x).
=> (q ==> x).


% try :- prove_by_contradiction(x).

:-prolog.

:-next_test. % ==
% here is an example which defines pfcDefault facts and rules.  Will it work?

(pfcDefault(P)/pfcAtom(P))  =>  (~not(P) => P).

pfcDefault((P => Q))/pfcAtom(Q) => (P, ~not(Q) => Q).

% birds fly by pfcDefault.
=> pfcDefault((bird(X) => fly(X))).

% here's one way to do an isa hierarchy.
% isa = genls.

isa(C1,C2) =>
  {P1 =.. [C1,X],
    P2 =.. [C2,X]},
  (P1 => P2).

=> isa(canary,bird).
=> isa(penguin,bird).

% penguins do not fly.
penguin(X) => not(fly(X)).

% chilly is a penguin.
=> penguin(chilly).

% tweety is a canary.
=> canary(tweety).



% is this how to define constraints?

either(P,Q) => (not(P) => Q), (not(Q) => P).

(P,Q => false) => (P => not(Q)), (Q => not(P)).


:-next_test. % ==
% here is an interesting rule!

not(P), P => contradiction(P).

contradiction(P) => 
  {format('~n% contradiction - both ~w and not(~w) added.~n',[P,P])}.

% this means that both P and Q can't be true.
disjoint(P,Q)
  =>
  (P => not(Q)),
  (Q => not(P)).

=> disjoint(male(P), female(P)).

=> male(shirley).

=> mother(shirley,mary).

mother(X,_Y) => female(X).






bel(A1,desire(A2,know(A2,bel(A1,P)))), self(A1), bel(A1,P) => tell(A1,A2,P).


bel(A1,desire(A2,knowif(A2,P))),
self(A1),
bel(A1,not(P))
=>
tell(A1,A2,not(P)).


=> fact(0,1).
=> fact(1,1).
=> fact(2,2).
fact(N,M) <= {N>0,N1 is N-1}, fact(N1,M1), {M is N*M1}.

 
=> fib(1,1).
=> fib(2,1).
fib(N,M) <= 
  {N>2,N1 is N-1,N2 is N-2},
  fib(N1,M1),
  fib(N2,M2),
  {M is M1+M2}.



:-next_test. % ==

at(Obj,NewLoc), 
{(at(Obj,OldLoc), OldLoc\==NewLoc)}
  =>
  ~at(Obj,OldLoc).

function(P) =>
  {P1 =.. [P,X,Y],
   P2 =.. [P,X,Z]},
  (P1,{(P2,Y\==Z)} => ~P2).
  
=> function(age).

function(Name,Arity) =>
  {functor(P1,Name,Arity),
   functor(P2,Name,Arity),
   arg(Arity,P1,PV1),
   arg(Arity,P2,PV2),
   N is Arity-1,
   merge(P1,P2,N)},
  (P1,{(P2,PV1\==PV2)} => ~P2).


merge(_,_,N) :- N<1.
merge(T1,T2,N) :-
  N>0,
  arg(N,T1,X),
  arg(N,T2,X),
  N1 is N-1,
  merge(T1,T2,N1).



not(P),P => contrradiction.

bird(X), ~not(fly(X)) => fly(X).

penguin(X) => bird(X).

penguin(X) => not(fly(X)).

bird(X), injured(X) => not(fly(X)).

bird(X), dead(X) => not(fly(X)).

:-pfcPrintDB.

:-next_test.


% dcg_pfc: translation of dcg-like grammar rules into pfc rules.

:- op(1200,xfx,'-->>').
:- op(1200,xfx,'--*>>').
% :- op(1200,xfx,'<<--').
:- op(400,yfx,'^^').

% :- use_module(library(strings)), use_module(library(lists)).

term_expansion((P -->> Q),(:- add(Rule))) :-
  pfc_translate_rule((P -->> Q), Rule).
term_expansion((P --*>> Q),(:- add(Rule))) :-
  pfc_translate_rule((P --*>> Q), Rule).

pfc_translate_rule((LP-->>[]),H) :- !, pfc_t_lp(LP,Id,S,S,H).
pfc_translate_rule((LP-->>RP),(H <= B)):-
   pfc_t_lp(LP,Id,S,SR,H),
   pfc_t_rp(RP,Id,S,SR,B1),
   pfc_tidy(B1,B).


pfc_translate_rule((LP--*>>[]),H) :- !, pfc_t_lp(LP,Id,S,S,H).
pfc_translate_rule((LP--*>>RP),(B => H)):-
   pfc_t_lp(LP,Id,S,SR,H),
   pfc_t_rp(RP,Id,S,SR,B1),
   pfc_tidy(B1,B).

pfc_t_lp(X,Id,S,SR,ss(X,Id,(S ^^ SR))) :- var(X),!.

pfc_t_lp((LP,List),Id,S,SR,ss(LP,Id,(S ^^ List2))):- 
   !,
   pfcAppend(List,SR,List2).

pfc_t_lp(LP,Id,S,SR,ss(LP,Id,(S ^^ SR))).

pfc_t_rp(!,Id,S,S,!) :- !.
pfc_t_rp([],Id,S,S1,S=S1) :- !.
pfc_t_rp([X],Id,S,SR,ss(word(X),Id,(S ^^ SR))) :- !.
pfc_t_rp([X|R],Id,S,SR,(ss(word(X),Id,(S ^^ SR1)),RB)) :- 
  !, 
  pfc_t_rp(R,Id,SR1,SR,RB).
pfc_t_rp({T},Id,S,S,{T}) :- !.
pfc_t_rp((T,R),Id,S,SR,(Tt,Rt)) :- !,
   pfc_t_rp(T,Id,S,SR1,Tt),
   pfc_t_rp(R,Id,SR1,SR,Rt).
pfc_t_rp((T;R),Id,S,SR,(Tt;Rt)) :- !,
   pfc_t_or(T,Id,S,SR,Tt),
   pfc_t_or(R,Id,S,SR,Rt).
pfc_t_rp(T,Id,S,SR,ss(T,Id,(S ^^ SR))).

pfc_t_or(X,Id,S0,S,P) :-
   pfc_t_rp(X,Id,S0a,S,Pa),
 ( var(S0a), S0a \== S, !, S0=S0a, P=Pa;
   P=(S0=S0a,Pa) ).

pfc_tidy((P1;P2),(Q1;Q2)) :-
   !,
   pfc_tidy(P1,Q1),
   pfc_tidy(P2,Q2).
pfc_tidy(((P1,P2),P3),Q) :- 
   pfc_tidy((P1,(P2,P3)),Q).
pfc_tidy((P1,P2),(Q1,Q2)) :- 
   !,
   pfc_tidy(P1,Q1),
   pfc_tidy(P2,Q2).
pfc_tidy(A,A) :- !.

compile_pfcg :-
  ((retract((L -->> R)), pfc_translate_rule((L -->> R), PfcRule));
    (retract((L --*>> R)), pfc_translate_rule((L --*>> R), PfcRule))),
  add(PfcRule),
  fail.
compile_pfcg.

parse(Words) :- 
  parse(Words,Id),
  format("~Nsentence id = ~w",Id),
  show(Id,sentence(X)).


parse(Words,Id) :- 
  gen_s_tag(Id),
  parse1(Words,Id),
  add(sentence(Id,Words)).

parse1([],_) :- !.
parse1([H|T],Id) :-
 do_or_ignore(add(ss(word(H),Id,([H|T] ^^ T)))),
 parse1(T,Id).


showSentences(Id) :- showSentences(Id,_).

showSentences(Id,Words) :-
  sentence(Id,Words),
  pfc(ss(s(S),Id,(Words ^^ []))),
  nl,write(S),
  fail.
showSentences(_,_).

do_or_ignore(X) :- db_call(X) -> true;true.

show(Id,C) :-
  pfc(ss(C,Id,A ^^ B)),
  append(Words,B,A),
  format("~n ~w    :   ~w",[C,Words]),
  fail.

gen_s_tag(s(N2)) :-
  var(V),
  (retract(s_tag(N)); N=0),
  N2 is N+1,
  assert(s_tag(N2)).

make_term(ss(Constituent,Id,String),Term) :-
   Constituent =.. [Name|Args],
   name(Name,Name_string),
   name(Name2,[36|Name_string]),
   append([Name2|Args],[Id,String],Term_string),
   Term =.. Term_string.
%append([],X,X).
%append([H|T],L2,[H|L3]) :- append(T,L2,L3).



:-next_test. % ==

:- dynamic ('-->>')/2.
:- dynamic ('--*>>')/2.

% a simple pfc dcg grammar.  requires dcg_pfc.pl

% backward grammar rules.
s(s(Np,Vp)) -->> np(Np), vp(Vp).

vp(vp(V,Np)) -->> verb(V), np(Np).
vp(vp(V)) -->> verb(V).
vp(vp(VP,X)) -->> vp(VP), pp(X).

np(np(N,D)) -->> det(D), noun(N).
np(np(N)) -->> noun(N).
np(np(Np,pp(Pp))) -->> np(Np), pp(Pp).

pp(pp(P,Np)) -->> prep(P), np(Np).

% forward grammar rules.
P --*>>  [W],{cat(W,Cat),P =.. [Cat,W]}.

% simple facts.
cat(the,det).
cat(a,det).
cat(man,noun).
cat(fish,noun).
cat(eats,verb).
cat(catches,verb).
cat(in,prep).
cat(on,prep).
cat(house,noun).
cat(table,noun).






end_of_file.


:-next_test. % ==

%% a simple Pfc example - the three bulb problem (see DeKleer and
%% Williams, IJCAI89)
%%
%% Tim Finin, finin@prc.unisys.com, 8/89

% Devices behave as intended unless they are faulty.
isa(X,Class), ~faulty(X) => behave(X,Class).

% connecting two terminals means their voltages are equal.
connect(T1,T2) => (voltage(T1,V) <=> voltage(T2,V)).

equal(voltage(T1),voltage(T2)) <= connect(T1,T2).
  
% a wire behaves by connecting its two terminals.
behave(X,wire) => connect(t1(X),t2(X)).

% a battery's behaviour
behave(X,battery), rating(X,V)
  =>
 voltage(t1(X),V),
 voltage(t2(X),0).

% a bulb's behaviour.
behave(X,bulb) =>
 (voltage(t1(X),V1),voltage(t2(X),V2), {V1\==V2} => lit(X)),
 (notequal(voltage(t1(X)),voltage(t2(X))) => lit(X)).

lit(X) => notequal(voltage(t1(X)),voltage(t2(X))).



% a pretty basic conflict.
not(P), P => conflict(P).

% this doesn't work anyomore. twf.
% voltage(T,V) => (not(voltage(T,V2)) <= {\+V=:=V2}).

% It is a conflict if a terminal has two different voltages.
voltage(T,V1), voltage(T,V2)/(\+V1=:=V2) => conflict(two_voltages(T,V1,V2)).

% assume an observation is true.
observed(P), ~false_observation(P) => P.

% a conflict triggers a Prolog action to resolve it.
conflict(C) => {resolveConflict(C)}.

% this isn't written yet.
resolveConflict(C) :-
  format("~NHalting with conflict ~w", [C]),
  pfcHalt.

% meta rules to schedule inferencing.

% resolve conflicts asap
pfcSelect(conflict(X)) :- pfcQueue(conflict(X)).
  

%% ***** here is a particular test case. *****


% here is a particular circuit - a gizmo.

isa(X,gizmo) =>
  isa(battery(X),battery),
  rating(battery(X),6),

  isa(b1(X),bulb),
  isa(b2(X),bulb),
  isa(b3(X),bulb),

  isa(w1(X),wire),
  isa(w2(X),wire),
  isa(w3(X),wire),
  isa(w4(X),wire),
  isa(w5(X),wire),
  isa(w6(X),wire),
 
  connect(t1(battery(X)),t1(w1(X))),
  connect(t2(w1(X)),t1(b1(X))),
  connect(t2(w1(X)),t1(w2(X))),
  connect(t2(w2(X)),t1(b2(X))),
  connect(t2(w2(X)),t1(w3(X))),
  connect(t2(w3(X)),t1(b3(X))),

  connect(t2(battery(X)),t1(w4(X))),
  connect(t2(w4(X)),t2(b2(X))),
  connect(t2(w4(X)),t1(w5(X))),
  connect(t2(w5(X)),t2(b2(X))),
  connect(t2(w5(X)),t1(w6(X))),
  connect(t2(w6(X)),t2(b3(X))).


%% here is a diagnostic problem for a gizmo.

test_bs(X) :- 
  add([isa(X,gizmo),
       observed(not(lit(b1(X)))),
       observed(not(lit(b2(X)))),
       observed(lit(b3(X)))]).


:-next_test. % ==

%% a simple Pfc example - the one bulb problem (see DeKleer and
%% Williams, IJCAI89)
%%
%% Tim Finin, finin@prc.unisys.com, 8/89



% Devices behave as intended unless they are faulty.
isa(X,Class), ~faulty(X) => behave(X,Class).

% assume an observation is true.
observed(P), ~false_observation(P) => P.

% connecting two terminals means their voltages are equal.
con(T1,T2) => (volt(T1,V) <=> volt(T2,V)).

% a wire behaves by connecting its two terminals.
behave(X,wire) => con(t1(X),t2(X)).

% a battery's behaviour
behave(X,battery)
  =>
 volt(t1(X),1.5),
 volt(t2(X),0).

% a bulb's behaviour.
behave(X,bulb), 
 volt(t1(X),V1),
 volt(t2(X),V2), 
 {V1\==V2} 
=> lit(X).

% It is a conflict if a terminal has two different voltages.
% volt(T,V1), volt(T,V2)/(\+V1=:=V2) => conflict(two_voltages(T,V1,V2)).

%% ***** here is a particular test case. *****


% here is a particular circuit - a gizmo.

isa(X,gizmo) =>
  isa(battery(X),battery),
  isa(bulb(X),bulb),

  isa(w1(X),wire),
  isa(w2(X),wire),
 
  con(t1(battery(X)),t1(w1(X))),
  con(t2(battery(X)),t1(w2(X))),
  con(t2(w1(X)),t1(bulb(X))),
  con(t2(bulb(X)),t2(w2(X))).

%% here is a diagnostic problem for a gizmo.

test_b1(X) :- 
  add([isa(X,gizmo),
       observed(not(lit(bulb(X))))]).



:-next_test. % ==

%% a simple Pfc example - the standard circuit diagnosis problem.
%%
%% Tim Finin, finin@prc.unisys.com, 9/29/88

% Devices behave as intended unless they are faulty.
isa(X,Class), ~faulty(X) => behave(X,Class).

% a wire equates the values at each end.
wire(T1,T2) => (value(T1,V) <=> value(T2,V)).

% It is a conflict if a terminal has two different values.
value(T,V1), value(T,V2)/(\+V1=:=V2) => conflict(two_values(T,V1,V2)).

% assume an observation is true.
observed(P), ~false_observation(P) => P.

% a conflict triggers a Prolog action to resolve it.
conflict(C) => {resolveConflict(C)}.

% this isn't written yet.
resolveConflict(C) :-
  format("~NHalting with conflict ~w", [C]),
  pfcHalt.

% an adder's behaviour
behave(X,adder) =>
 (value(in(1,X),I1), value(in(2,X),I2) => {O is I1+I2}, value(out(X),O)),
 (value(in(2,X),I2) <= value(in(1,X),I1), value(out(X),O), {I2 is O-I1}),
 ( value(in(1,X),I1) <= value(in(2,X),I2), value(out(X),O), {I1 is O-I2}).

% a multiplier's behaviour.
behave(X,multiplier) =>
 (value(in(1,X),I1), value(in(2,X),I2) => {O is I1*I2}, value(out(X),O)),
 (value(in(2,X),I2) <= value(in(1,X),I1), value(out(X),O), {I2 is O/I1}),
 ( value(in(1,X),I1) <= value(in(2,X),I2), value(out(X),O), {I1 is O/I2}).


% meta rules to schedule inferencing.

% resolve conflicts asap
pfcSelect(conflict(X)) :- pfcQueue(conflict(X)).



%% ***** here is a particular test case. *****


% here is a particular circuit - a gizmo.

isa(X,gizmo) =>
  isa(m1(X),multiplier),
  isa(m2(X),multiplier),
  isa(m3(X),multiplier),
  isa(a1(X),adder),
  isa(a2(X),adder),
  wire(out(m1(X)),in(1,a1(X))),
  wire(out(m2(X)),in(2,a1(X))),
  wire(out(m2(X)),in(1,a2(X))),
  wire(out(m3(X)),in(2,a2(X))).


%% here is a diagnostic problem for a gizmo.

test(X) :- 
  add(isa(X,gizmo)),
  add(value(in(1,m1(X)),3.0)),
  add(value(in(2,m1(X)),2.0)),
  add(value(in(1,m2(X)),3.0)),
  add(value(in(2,m2(X)),2.0)),
  add(value(in(1,m3(X)),2.0)),
  add(value(in(2,m3(X)),3.0)),
  add(observed(value(out(a1(X)),10.0))),
  add(observed(value(out(a2(X)),12.0))).


