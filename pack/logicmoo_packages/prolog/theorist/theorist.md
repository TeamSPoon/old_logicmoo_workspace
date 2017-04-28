A Theorist to Prolog Compiler (th2.tex)∗

David Poole

Department of Computer Science,

University of British Columbia,

Vancouver, B.C. Canada V6T 1W5 (604) 228-6254

poole@cs.ubc.ca

April 28, 2017

Abstract

Artiﬁcial intelligence researchers have been designing representa-

tion systems for default and abductive reasoning. Logic Programming

researchers have been working on techniques to improve the eﬃciency

of Horn Clause deduction systems. This paper describes how Theo-

rist can be translated into Quintus Prolog. The verbatim code is the

actual running code.

This should not be seen as The Theorist System, but rather as one

implementation of the Theorist framework. Theorist should be seen

as the idea that much reasoning can be done by theory formation from

a ﬁxed set of possible hypotheses. This implementation is based on

a complete linear resolution theorem prover which does not multiply

out subterms.

It also carries out incremental consistency checking.

Note that there is nothing in this document which forces the control

strategy of Prolog. This is a compiler from Theorist to a Horn-clause

reasoner with negation as failure; nothing precludes any other search

strategy (e.g., dependency directed backtracking, constraint propaga-

tion). This is intended to be a runnable speciﬁcation, which runs fast

(e.g., for the common intersection between Theorist and Prolog (i.e.,

∗Copyright c(cid:13)1990 David Poole. All rights reserved.

![139778829892304](images/139778829892304)

Theorist to Prolog (th2.tex)

Horn clauses) Theorist code runs about half the speed of compiled

Quintus Prolog code).

This code is available electronically from the author.

Contents

Introduction

Many people in Artiﬁcial Intelligence have been working on default reasoning

and abductive diagnosis systems [?, ?, ?, ?]. The systems implemented so far

(eg., [?, ?, ?, ?]) are only prototypes or have been developed in ways that can-

not take full advantage in the advances of logic programming implementation

technology.

Many people are working on making logic programming systems more ef-

ﬁcient. These systems, however, usually assume that the input is in the form

of Horn clauses with negation as failure. This paper shows how to implement

the default reasoning system Theorist [?, ?] by compiling its input into Horn

clauses with negation as failure, thereby allowing direct use the advances in

logic programming implementation technology. Both the compiler and the

compiled code can take advantage of these improvements.

We have been running this implementation on standard Prolog compilers

(in particular Quintus Prolog) and it outperforms all other default reasoning

systems that the author is aware of. It is, however, not restricted to the con-

trol structure of Prolog. There is nothing in the compiled code which forces

it to use Prolog’s search strategy. Logic programming and other researchers

are working on alternate control structures which seem very appropriate for

default and abductive reasoning. Advances in parallel inference (e.g., [?]),

constraint satisfaction [?, ?] and dependency directed backtracking [?, ?, ?]

should be able to be directly applicable to the code produced by this com-

piler.

We are thus eﬀecting a clear distinction between the control and logic of

our default reasoning systems [?]. We can let the control people concentrate

on eﬃciency of Horn clause systems, and these will then be directly applicable

to those of us building richer representation systems. The Theorist system

has been designed to allow maximum ﬂexibility in control strategies while

Theorist to Prolog (th2.tex)

still giving us the power of assumption-based reasoning that are required for

default and abductive reasoning.

This is a step towards having representation and reasoning systems which

are designed for correctness being able to use the most eﬃcient of control

strategies, so we can have the best of expressibility and eﬃciency.

2 Theorist Framework

Theorist [?, ?] is a logical reasoning system for default reasoning and diag-

nosis. It is based on the idea of theory formation from a ﬁxed set of possible

hypotheses.

This implementation is of the version of Theorist described in [?]. The

user provides three sets of ﬁrst order formulae

F is a set of closed formulae called the facts. These are intended to be

true in the world being modelled.

∆ is a set of formulae which act as possible hypotheses, any ground in-

stance of which can be used in an explanation if consistent.

C is a set of closed formulae taken as constraints. The constraints restrict

what can be hypothesised.

We assume that F ∪ C is consistent.

Deﬁnition 1 a scenario of F , ∆ is a set D ∪ F where D is a set of ground

instances of elements of ∆ such that D ∪ F ∪ C is consistent.

Deﬁnition 2 If g is a closed formula then an explanation of g from F , ∆

is a scenario of F , ∆ which implies g.

That is, g is explainable from F , ∆ if there is a set D of ground instances of

elements of ∆ such that

F ∪ D |= g and

F ∪ D ∪ C is consistent

Theorist to Prolog (th2.tex)

F ∪ D is an explanation of g.

In other papers we have described how this can be the basis of default and

abductive reasoning systems [?, ?, ?, ?]. If we are using this for prediction

then possible hypotheses can be seen as defaults.

[?] describes how this

formalism can account for default reasoning. This is also a framework for

abductive reasoning where the possible hypotheses are the base causes we

are prepared to accept as to why some observation was made [?]. We will

refer to possible hypotheses as defaults.

One restriction that can be made with no loss of expressive power is to

restrict possible hypotheses to just atomic forms with no structure [?]. This

is done by naming the defaults.

2.1 Syntax

The syntax of Theorist is designed to be of maximum ﬂexibility. Virtually

any syntax is appropriate for wﬀs; the formulae are translated into Prolog

clauses without mapping out subterms. The theorem prover implemented in

the Compiler can be seen as a non-clausal theorem prover [?].

A wﬀ is a well formed formula made up of arbitrary combination of

equivalence (“==”, “equiv”), implication (“=>”, “< −”), disjunction (“or”,

“;”), conjunction (“and”, “&”, “,”) and negation (“not”, “˜”) of atomic

symbols. Variables follow the Prolog convention of being in upper case.

There is no explicit quantiﬁcation.

A name is an atomic symbol with only free variables as arguments.

The following gives the syntax of the Theorist code:

fact w.

where w is a wﬀ, means that (∀w) ∈ F ; i.e., the universal closure of w

(all variables universally quantiﬁed) is a fact.

default d.

where d is a name, means that d ∈ ∆; i.e., d is a default (a possible

hypothesis).

default d : w.

where d is a name and w is a wﬀ means w, with name d can be used in a

scenario if it is consistent. Formally it means d ∈ ∆ and (∀d ⇒ w) ∈ F .

Theorist to Prolog (th2.tex)

constraint w.

where w is a wﬀ means ∀w ∈ C.

prolog p.

where p is an atomic symbol means any Theorist call to p should be

proven in Prolog. This allows us to use built-in predicates of pure

Prolog. One should not expect Prolog’s control predicates to work.

explain w.

where w is an arbitrary wﬀ, gives all explanations of ∃w.

predict w.

where w is a arbitrary ground wﬀ, returns “yes” if w is in every ex-

tension of the defaults and “no” otherwise. If it returns “yes”, a set of

explanations is returned, if it returns “no” then a scenario from which

g cannot be explained is returned (this follows the framework of [?]).

In this compiler these are interpreted as commands to Prolog. The inter-

face will thus be the Prolog interface with some predeﬁned commands.

2.2 Compiler Directives

The following are compiler directives:

thconsult ﬁlename.

reads commands from ﬁlename, and asserts and/or executes them.

thtrans ﬁlename.

reads commands from ﬁlename and translates them into Prolog code

in the ﬁle ﬁlename.pl.

thcompile ﬁlename.

reads commands from ﬁlename, translates them into the ﬁle ﬁlename.pl

and then compiles this ﬁle. “explain” commands in the ﬁle are not

interpreted.

dyn atom.

should be in a ﬁle and declares that anything matching the atom is

allowed to be asked or added to. This should appear before any use of

Theorist to Prolog (th2.tex)

the atom. This corresponds to the “dynamic” declaration of Quintus

Prolog. This is ignored except when compiling a ﬁle.

There are some other commands which allow one to set ﬂags. See section ??

for more detail on setting checking and resetting ﬂags.

3 Overview of Implementation

In this section we assume that we have a deduction system (denoted ⊢) which

has the following properties (such a deduction system will be deﬁned in the

next section):

1. It is sound (i.e., if A ⊢ g then A |= g).

2. It is complete in the sense that if g follows from a consistent set of

formulae, then g can be proven. I.e., if A is consistent and A |= g then

A ⊢ g.

3. If A ⊢ g then A ∪ B ⊢ g; i.e., adding in extra facts will not prevent the

system from ﬁnding a proof which previously existed.

4. It can return instances of predicates which were used in the proof.

The basic idea of the implementation follows the deﬁnition on explain-

ability:

Algorithm 1 to explain g

1. try to prove g from F ∪ ∆. If no proof exists, then g is not explainable.

If there is a proof, let D be the set of instances of elements of ∆ used

in the proof. We then know

F ∪ D |= g

by the soundness of our proof procedure.

2. show D is consistent with F ∪ C by failing to prove it is inconsistent.

Theorist to Prolog (th2.tex)

3.1 Consistency Checking

The following two theorems are important for implementing the consistency

check:

Lemma 1 If A is a consistent set of formulae and D is a ﬁnite set of ground

instances of possible hypotheses, then if we impose arbitrary ordering on the

elements of D = {d1, ..., dn}

A ∪ D is inconsistent

if and only if

there is some i, 1 ≤ i ≤ n such that A ∪ {d1, ..., di−1} is consistent and

A ∪ {d1, ..., di−1} |= ¬di.

If A ∪ D is inconsistent there is some least i such that

Proof:

A∪{d1, ..., di} is inconsistent. Then we must have A∪{d1, ..., di−1}

is consistent (as i is minimal) and A ∪ {d1, ..., di−1} |= ¬di (by

inconsistency). ✷

This lemma says that we can show that F ∪ C ∪ {d1, ..., dn} is consistent

if we can show that for all i, 1 ≤ i ≤ n, F ∪ C ∪ {d1, ..., di−1} 6⊢ ¬di. If our

theorem prover can show there is no proof of all of the ¬di, then we have

consistency.

This lemma indicates that we can implement Theorist by incrementally

failing to prove inconsistency. We need to try to prove the negation of the

default in the context of all previously assumed defaults. Note that this

ordering is arbitrary.

The following theorem expands on how explainability can be computed:

Theorem 2 If F ∪ C is consistent, g is explainable from F , ∆ if and only

if there is a ground proof of g from F ∪ D where D = {d1, ..., dn} is a set of

ground instances of elements of ∆ such that F ∧ C ∧ {d1, ..., di−1} 6⊢ ¬di for

all i, 1 ≤ i ≤ n.

If g is explainable from F , ∆, there is a set D of ground

Proof:

instances of elements of ∆ such that F ∪ D |= g and F ∪ D ∪ C

is consistent. So there is a ground proof of g from F ∪ D. By

the preceding lemma F ∪ D ∪ C is consistent so there can be no

sound proof of inconsistency. That is, we cannot prove F ∧ C ∧

{d1, ..., di−1} ⊢ ¬di for any i. ✷

Theorist to Prolog (th2.tex)

This leads us to the reﬁnement of algorithm ??:

Algorithm 2 to explain g from F , ∆

1. Build a ground proof of g from F ∪ ∆. Make D the set of instances of

elements of ∆ used in the proof.

2. For each i, try to prove ¬di from F ∧ C ∧ {d1, ..., di−1}.

If all such

proofs fail, D is an explanation for g.

Note that the ordering imposed on the D is arbitrary. A sensible one

is the order in which the elements of D were generated. Thus when a new

hypothesis is used in the proof, we try to prove its negation from the facts

and the previously used hypotheses. These proofs are independent of the

original proof and can be done as they are generated as in negation as failure

(see section ??), or can be done concurrently.

3.2 Variables

Notice that theorem ?? says that g is explainable if there is a ground proof.

There is a problem that arises to translate the preceding algorithm (which

assumes ground proofs) into an algorithm which does not build ground proofs

(eg., a standard resolution theorem prover), as we may have variables in the

forms we are trying to prove the negation of.

A problem arises when there are variables in the D generated. Consider

the following example:

Example 1 Let ∆ = {p(X)}. That is, any instance of p can be used if it is

consistent. Let F = {∀Y (p(Y ) ⇒ g), ¬p(a)}. That is, g is true if there is a

Y such that p(Y ).

According to our semantics, g is explainable with the explanation {p(b)},

which is consistent with F (consider the interpretation I = {¬p(a), p(b)} on

the domain {a, b}), and implies g.

However, if we try to prove g, we generate D = {p(Y )} where Y is free

(implicitly a universally quantiﬁed variable). The existence of the fact ¬p(a)

should not make it inconsistent, as we want g to be explainable.

Theorem 3 It is not adequate to only consider interpretations in the Her-

brand universe of F ∪ ∆ ∪ C to determine explainability.

Theorist to Prolog (th2.tex)

consider the example above; the Herbrand universe

Proof:

is just the set {a}. Within this domain there is no consistent

explanation to explain g. ✷

This shows that Herbrand’s theorem is not applicable to the whole system.

It is, however, applicable to each of the deduction steps [?].

So we need to generate a ground proof of g. This leads us to:

Algorithm 3 To determine if g is explainable from F , ∆

1. generate a proof of g using elements of F and ∆ as axioms. Make D0

the set of instances of ∆ used in the proof;

2. form D1 by replacing free variables in D0 with unique constants;

3. add D1 to F and try to prove an inconsistency (as in the previous case).

If a complete search for a proof fails, g is explainable.

This algorithm can now be directly implemented by a resolution theorem

prover.

Example 2 Consider F and ∆ as in example 1 above. If we try to prove

g, we use the hypothesis instance p(Y ). This means that g is provable from

any instance of p(Y ). To show g cannot be explained, we must show that

all of the instances are inconsistent. The above algorithm says we replace Y

with a constant β. p(β) is consistent with the facts. Thus we can show g is

explainable.

3.3

Incremental Consistency Checking

Algorithm ?? assumed that we only check consistency at the end. We cannot

replace free variables by a unique constant until the end of the computation.

This algorithm can be further reﬁned by considering cases where we can

check consistency at the time the hypothesis is generated.

Theorem ?? shows that we can check consistency incrementally in what-

ever order we like. The problem is to determine whether we have generated

the ﬁnal version of a set of hypotheses. If there are no variables in our set of

hypotheses, then we can check consistency as soon as they are generated. If

there are variables in a hypothesis, then we cannot guarantee that the form

generated will be the ﬁnal form of the hypothesis.

Theorist to Prolog (th2.tex)

Example 3 Consider the two alternate set of facts:

∆ = { p(X) }

F1 = { ∀X p(X) ∧ q(X) ⇒ g,

¬p(a),

q(b) }

F2 = { ∀X p(X) ∧ q(X) ⇒ g,

¬p(a),

q(a) }

Suppose we try to explain g by ﬁrst explaining p and then explaining q.

Once we have generated the hypothesis p(X), we have not enough informa-

tion to determine whether the consistency check should succeed or fail. The

consistency check for F1 should succeed (i.e, we should conclude with a con-

sistent instance, namely X = b), but the consistency check for F2 should fail

(there is no consistent value for the X which satisﬁes p and q). We can only

determine the consistency after we have proven q.

There seems to be two obvious solutions to this problem, the ﬁrst is to

allow the consistency check to return constraints on the values (eg., [?]). The

alternate (and simpler) solution is to delay the evaluation of the consistency

check until all of the variables are bound (as in [?]), or until we know that the

variables cannot be bound any more. In particular we know that a variable

cannot be bound any more at the end of the computation.

The implementation described in this paper does the simplest form of in-

cremental consistency checking, namely it computes consistency immediately

for those hypotheses with no variables and delays consistency checking until

the end for hypotheses containing variables at the time they are generated.

4 The Deduction System

This implementation is based on linear resolution [?, ?]. This is complete

in the sense that if g logically follows from some consistent set of clauses A,

then there is a linear resolution proof of g from A.

SLD resolution of Prolog [?] can be seen as linear resolution with the

contrapositive and ancestor search removed.

Theorist to Prolog (th2.tex)

To implement linear resolution in Prolog, we add two things

1. we use the contrapositive of our clauses. If we have the clause

L1 ∨ L2 ∨ ... ∨ Ln

then we create the n rules

L1 ← ¬L2 ∧ ... ∧ ¬Ln

L2 ← ¬L1 ∧ ¬L3 ∧ ... ∧ ¬Ln

...

Ln ← ¬L1 ∧ ... ∧ ¬Ln−1

as rules. Each of these can then be used to prove the left hand literal if

we know the other literals are false. Here we are treating the negation

of an atom as a diﬀerent Prolog atom (i.e., we treat ¬p(X) as an atom

notp(X)).

2. the ancestor cancellation rule. While trying to prove L we can assume

¬L. We have a subgoal proven if it uniﬁes with the negation of an

ancestor in the proof tree. This is an instance of proof by contradiction.

We can see this as assuming ¬L and then when we have proven L we

can discharge the assumption.

One property of the deduction system that we want is the ability to

implement deﬁnite clauses with a constant factor overhead over using Prolog.

One way to do this is to keep two lists of ancestors of any node: P the

positive (non negated atoms) ancestors and N the negated ancestors. Thus

for a positive subgoal we only need to search for membership in N and for a

negated subgoal we only need to search P . When we have deﬁnite clauses,

there are no negated subgoals, and so N is always empty. Thus the ancestor

search always consists of checking for membership in an empty list. The

alternate contrapositive form of the clauses are never used.

Alternate, more complicated ways to do ancestor search have been inves-

tigated [?], but this implementation uses the very simple form given above.

Another tempting possibility is to use the near-Horn resolution of [?]. More

work needs to be done on this area.

![139778821230672](images/139778821230672)

![139778820903184](images/139778820903184)

Theorist to Prolog (th2.tex)

4.1 Disjunctive Answers

For the compiler to work properly we need to be able to return disjunctive

answers. We need disjunctive answers for the case that we can prove only a

disjunctive form of the query.

For example, if we can prove p(a) ∨ p(b) for the query ?p(X), then we

want the answer X = a or b. This can be seen as “if the answer is not a then

the answer is b”.

To have the answer a1 ∨ ... ∨ an, we need to have a proof of “If the answer

is not a1 and not a2 and ... and not an−1 then the answer is an”. We collect

up conditions on the proof of alternate answers that we are assuming are not

true in order to have the disjunctive answer.

This is implemented by being able to assume the negation of the top level

goal as long as we add it to the set of answers. To do this we carry a list of

the alternate disjuncts that we are assuming in proving the top level goal.

4.2 Conversion to Clausal Form

It is desirable that we can convert an arbitrary well formed formula into

clausal (or rule) form without mapping out subterms. Instead of distributing,

we do this by creating a new term to refer to the disjunct.

Once a formula is in negation normal form, then the normal way to con-

vert to clausal form [?] is to convert something of the form

by distribution into

α ∨ (β ∧ γ)

(α ∨ β) ∧ (α ∨ γ)

and so mapping out subterms.

The alternate [?] is to create a new relation p parameterised with the

variables in common with α and β ∧ γ. We can then replace β ∧ γ by p and

then add

(¬p ∨ β) ∧ (¬p ∨ γ)

to the set of formulae.

This can be embedded into the compiler by using Prolog “or” instead

of actually building the p.

(Alternatively we can deﬁne “or” by deﬁning

the clause (p; q) ← p and (p; q) ← q.) We build up the clauses so that

Theorist to Prolog (th2.tex)

the computation runs without any multiplying out of subterms. This is an

instance of the general procedure of making clausal theorem provers into

non-clausal theorem provers [?].

5 Details of the Compiler

In this section we give actual code which converts Theorist code into Prolog

code. The compiler is described here in a bottom up fashion; from the

construction of the atoms to compilation of general formulae.

The compiler is written in Prolog and the target code for the compiler

is Prolog code (in particular Horn clauses with negation as failure). There

are no “cuts” or other non-logical “features” of Prolog which depend on

Prolog’s search strategy in the compiled code. Each Theorist wﬀ gets locally

translated into a set of Prolog clauses.

5.1 Target Atoms

For each Theorist predicate symbol r there are 4 target predicate symbols,

with the following informal meanings:

prove r meaning r can be proven from the facts and the constraints.

prove not r meaning ¬r can be proven from the facts and the constraints.

ex r meaning r can be explained from F , ∆.

ex not r meaning ¬r can be explained from F , ∆.

The arguments to these built predicate symbols will contain all of the

information that we need to prove or explain instances of the source predi-

cates.

5.1.1 Proving

For relation r(−args−) in the source code we want to produce object code

which says that r(−args−) (or its negation) can be proven from the facts

and constraints and the current set of assumed hypotheses.

![139778819419664](images/139778819419664)

![139778818798224](images/139778818798224)

![139778818834768](images/139778818834768)

![139778818835152](images/139778818835152)

![139778818871568](images/139778818871568)

![139778818873872](images/139778818873872)

Theorist to Prolog (th2.tex)

For the source relation

r(−args−)

(which is to mean that r is a relation with −args− being the sequence of its

arguments), we have the corresponding target relations

prove r(−args−, T hs, Anc)

prove not r(−args−, T hs, Anc)

which are to mean that r (or ¬r) can be proven ¿from the facts and ground

hypotheses T hs with ancestor structure Anc. These extra arguments are:

T hs is a list of ground defaults. These are the defaults we have already

assumed and so deﬁne the context in which to prove r(−args−).

Anc is a structure of the form anc(P, N) where P and N are lists of source

atoms. Interpreted procedurally, P is the list of positive (not negated)

ancestors of the goal in a proof and N is the list of negated ancestors in

a proof. As described in section ?? we conclude some goal if it uniﬁes

with its negated ancestors.

Declaratively,

means

5.1.2 Explaining

prove r(−args−, T hs, anc(P, N))

F ∪ T hs ∪ ¬P ∪ N |= r(−args−)

There are two target relations for explaining associated with each source

relation r. These are ex r and ex not r.

For the source relation:

r(−args−)

we have two target new relations for explaining r:

ex r(−args−, T hs, Anc, Ans)

ex not r(−args−, T hs, Anc, Ans)

These mean that r(−args−) (or ¬r(−args−)) can be explained, with

![139778818696848](images/139778818696848)

![139778818257488](images/139778818257488)

![139778818259088](images/139778818259088)

![139778818259472](images/139778818259472)

![139778818052176](images/139778818052176)

![139778818119184](images/139778818119184)

![139778818119760](images/139778818119760)

![139778818120144](images/139778818120144)

![139778818158032](images/139778818158032)

![139778818188432](images/139778818188432)

Theorist to Prolog (th2.tex)

T hs is the structure of the incrementally built hypotheses used in explaining

r. There are two statuses of hypotheses we use; one the defaults that

are ground and so can be proven consistent at the time of generation;

the other the hypotheses with free variables at the time they are needed

in the proof, for which we defer consistency checking (in case the free

variables get instantiated later in the proof). T hs is essentially two

diﬀerence lists, one of the ground defaults already proven consistent

and one of the deferred defaults. T hs is of the form

ths(T1, T2, D1, D2)

which is to mean that T1 is the consistent hypotheses before we try to

explain r, and and T2 is the list of consistent hypotheses which includes

T1 and those hypotheses assumed to explain r. Similarly, D1 is the list

of deferred hypotheses before we consider the goal and D2 is the list of

resulting deferred hypotheses used in explaining r.

Anc contains the ancestors of the goal. As in the previous case, this is a pair

of the form anc(P, N) where P is the list of positive ancestors of the

goal, and N is the list of negated ancestors of the goal.

Ans contains the answers we are considering in diﬀerence list form ans(A1, A2),where A1 is the answers before proving the goal, and A2 is the answers

after proving the goal.

The semantics of

ex r(−args−, ths(T1, T2, D1, D2), anc(P, N), ans(A1, A2))

is deﬁned by

F ∪ T2 ∪ D2 ∪ ¬P ∪ N ∪ A2 |= r(−args−)

where T1 ⊆ T2, D1 ⊆ D2 and A1 ⊆ A2, and such that

F ∪ T2 is consistent

![139778817862608](images/139778817862608)

Theorist to Prolog (th2.tex)

5.1.3 Building Atoms

The procedure new lit(Preﬁx, Reln, Newargs, Newreln) constructs a new

atom, N ewreln, with predicate symbol made up of P ref ix prepended to

the predicate symbol of Reln, and taking as arguments the arguments of

Reln together with N ewargs. For example,

?– new lit(”ex ”,reln(a,b,c),[T,A,B],N).

yields

N = ex reln(a,b,c,T,A,B)

The procedure is deﬁned as follows1:

*/

new_lit(Prefix, Reln, NewArgs, NewReln) :-

Reln =.. [Pred | Args],

add_prefix(Prefix,Pred,NewPred),

append(Args, NewArgs, AllArgs),

NewReln =.. [NewPred | AllArgs].

add_prefix(Prefix,Pred,NewPred) :-

name(Pred,PredName),

append(Prefix, PredName, NewPredName),

name(NewPred,NewPredName).

/*

5.2 Compiling Rules

The next simplest compilation form we consider is the intermediate form

called a “rule”. Rules are statements of how to conclude the value of some

relation. Each Theorist fact corresponds to a number of rules (one for each

literal in the fact). Each rule gets translated into Prolog rules to explain and

prove the head of the rule.

Rules use the intermediate form called a “literal”. A literal is either an

atomic symbol or of the form n(A) where A is an atomic symbol. A rules is

1The verbatim code is the actual implementation code given in standard Edinburgh

notation. I assume that the reader is familiar with such notation.

![139778817111760](images/139778817111760)

![139778817144528](images/139778817144528)

![139778816723600](images/139778816723600)

![139778816724240](images/139778816724240)

![139778816759504](images/139778816759504)

Theorist to Prolog (th2.tex)

either a literal or of the form H ← Body (written “if(H,Body)”) where H is

a literal and Body is a conjunction and disjunction of literals.

We translate rules of the form

h(−x−) ← b1(−x1−), b2(−x2−), ..., bN (−xn−);

into the internal form (assuming that h is not negated)

ex h(−x−, ths(T0, Tn, D0, Dn), anc(P, N), ans(A0, An)) : −

ex b1(−x1−, ths(T0, T1, D0, D1), anc([h(−x−)|P ], N), ans(A0, A1)),

ex b2(−x2−, ths(T1, T2, D1, D2), anc([h(−x−)|P ], N), ans(A1, A2)),

...,

ex bn(−xn−, ths(Tn−1, Tn, Dn−1, Dn), anc([h(−x−)|P ], N), ans(An−1, An)).That is, we explain h if we explain each of the bi, accumulating the ex-

planations and the answers. Note that if h is negated, then the head of

the clause will be of the form ex not h, and the ancestor form will be

anc(P, [h(−x−)|N]).

If any of the bi are negated, then the corresponding

predicate will be ex not bi.

Example 4 the rule

gr(X, Y ) ← f (X, Z), p(Z, Y )

gets translated into

ex gr(X, Y, ths(D, E, F, G), anc(H, I), ans(J, K)) : −

ex f (X, Z, ths(D, M, F, N), anc([gr(X, Y )|H], I), ans(J, O)),

ex p(Z, Y, ths(M, E, N, G), anc([gr(X, Y )|H], I), ans(O, K)).

To explain gr we explain both f and p. The initial assumptions for f should

be the initial assumptions for gr, and the initial assumptions for p should be

the initial assumptions plus those made to explain f . The resulting assump-

tions after proving p are are the assumptions made in explaining gr.

Example 5 the fact

f ather(randy, jodi)

gets translated into

![139778816317904](images/139778816317904)

![139778817862416](images/139778817862416)

![139778815948560](images/139778815948560)

![139778815981136](images/139778815981136)

![139778816005776](images/139778816005776)

![139778816121552](images/139778816121552)

![139778816121936](images/139778816121936)

![139778815661968](images/139778815661968)

![139778815662352](images/139778815662352)

![139778815703248](images/139778815703248)

![139778816017488](images/139778816017488)

Theorist to Prolog (th2.tex)

ex f ather(randy, jodi, ths(T, T, D, D), , ans(A, A)).

We can explain f ather(randy, jodi) independently of the ancestors; we need

no extra assumptions, and we create no extra answers.

Similarly we translate rules of the form

h(−x−) ← b1(−x1−), b2(−x2−), ..., bN (−xn−);

into

prove h(−x−, T, anc(P, N)) : −

prove b1(−x1−, T, anc([h(−x−)|P ], N)),

...,

prove bn(−xn−, T, anc([h(−x−)|P ], N)).

Example 6 the rule

gr(X, Y ) ← f (X, Z), p(Z, Y )

gets translated into

prove gr(X, Y, D, anc(H, I)) : −

prove f (X, Z, D, anc([gr(X, Y )|H], I)),

prove p(Z, Y, D, anc([gr(X, Y )|H], I)).

That is, we can prove gr if we can prove f and p. Having gr(X, Y ) in the

ancestors means that we can prove gr(X, Y ) by assuming that ¬gr(X, Y )

and then proving gr(X, Y ).

Example 7 the fact

f ather(randy, jodi)

gets translated into

prove f ather(randy, jodi, , ).

Thus we can prove f ather(randy, jodi) for any explanation and for any an-

cestors.

![139778815587920](images/139778815587920)

![139778815589968](images/139778815589968)

![139778815620880](images/139778815620880)

![139778815191760](images/139778815191760)

![139778815230416](images/139778815230416)

![139778815233040](images/139778815233040)

![139778815296336](images/139778815296336)

![139778815298128](images/139778815298128)

![139778815337360](images/139778815337360)

![139778814917520](images/139778814917520)

![139778814947600](images/139778814947600)

Theorist to Prolog (th2.tex)

Disjuncts in the source body (;) get mapped into Prolog’s disjunction.

The answers and assumed hypotheses should be accumulated from whichever

branch was taken. This is then executed without mapping out subterms.

Example 8 The rule

p(A) ← q(A), (r(A), s(A); t(A)), m(A).

gets translated into

prove p(A, B, anc(C, D)) : −

prove q(A, B, anc([p(A)|C], D)),

(

prove r(A, B, anc([p(A)|C], D)),

prove s(A, B, anc([p(A)|C], D))

prove t(A, B, anc([p(A)|C], D))),

;

prove m(A, B, anc([p(A)|C], D)).

ex p(A, ths(B, C, D, E), anc(F, G), ans(H, I)) : −

ex q(A, ths(B, J, D, K), anc([p(A)|F ], G), ans(H, L)),

(

ex r(A, ths(J, M, K, N), anc([p(A)|F ], G), ans(L, O)),

ex s(A, ths(M, P, N, Q), anc([p(A)|F ], G), ans(O, R))

ex t(A, ths(J, P, K, Q), anc([p(A)|F ], G), ans(L, R))),

;

ex m(A, ths(P, C, Q, E), anc([p(A)|F ], G), ans(R, I))

Note that P is the resulting explanation from either executing r and s or

executing t from the explanation J.

5.2.1 The Code to Compile Rules

The following relation builds the desired structure for the bodies:

make bodies(B, T, [T hs, Anc, Ans], P roveB, ExB)

where B is a disjunct/conjunct of literals (the body of the rule), T is a

theory for the proving, T hs is a theory structure for explaining, Anc is an

ancestor structure (of form anc(P, N)), Ans is an answer structure (of form

ans(A0, A1)). This procedure makes P roveB the body of forms prove bi

(and prove not bi), and ExB a body of the forms ex bi.

![139778814580496](images/139778814580496)

![139778814701136](images/139778814701136)

![139778814739664](images/139778814739664)

![139778814741712](images/139778814741712)

![139778814772432](images/139778814772432)

![139778814774416](images/139778814774416)

![139778814805200](images/139778814805200)

![139778814806992](images/139778814806992)

![139778814314128](images/139778814314128)

![139778814354192](images/139778814354192)

![139778814357264](images/139778814357264)

![139778814384976](images/139778814384976)

![139778814425040](images/139778814425040)

![139778814533776](images/139778814533776)

![139778814174992](images/139778814174992)

![139778814175888](images/139778814175888)

![139778814176272](images/139778814176272)

Theorist to Prolog (th2.tex)

*/

make_bodies((H,B), T, [ths(T1,T3,D1,D3), Anc, ans(A1,A3)],

(ProveH,ProveB), (ExH,ExB)) :-

!,

make_bodies(H,T,[ths(T1,T2,D1,D2),Anc,ans(A1,A2)],ProveH,ExH),make_bodies(B,T,[ths(T2,T3,D2,D3),Anc,ans(A2,A3)],ProveB,ExB).make_bodies((H;B),T,Ths,(ProveH;ProveB),(ExH;ExB)) :-

!,

make_bodies(H,T,Ths,ProveH,ExH),

make_bodies(B,T,Ths,ProveB,ExB).

make_bodies(n(A), T, [Ths,Anc,Ans], ProveA, ExA) :-

!,

new_lit("prove_not_", A, [T,Anc], ProveA),

new_lit("ex_not_", A, [Ths,Anc,Ans], ExA).

make_bodies(true,_,[ths(T,T,D,D),_,ans(A,A)],true,true) :- !.

make_bodies(unif(X,Y),_,[ths(T,T,D,D),_,ans(A,A)],

unif(X,Y),unif(X,Y)) :-!.

make_bodies(A, T, [Ths,Anc,Ans], ProveA, ExA) :-

!,

new_lit("prove_", A, [T,Anc], ProveA),

new_lit("ex_", A, [Ths,Anc,Ans], ExA).

/*

The procedure rule(F, R) declares R to be a fact or constraint rule (de-

pending on the value of F ). Constraints can only be used for proving; facts

can be used for explaining as well as proving. R is either a literal or of the

form if (H, B) where H is a literal and B is a body.

This rule ﬁrst checks to see whether we want sound uniﬁcation and then

uses drule(F, R) to decare the rule.

prolog cl(C) is a way of asserting to Prolog the clause C. This can either

be asserted or written to a ﬁle to be consulted or compiled. The simplest

form is to just assert C.

make anc(H) is a procedure which ensures that the ancestor search is set

up properly for H. It is described in section ??, and can be ignored on ﬁrst

![139778813840912](images/139778813840912)

![139778813356176](images/139778813356176)

Theorist to Prolog (th2.tex)

reading.

*/

rule(F,R) :-

flag((sound_unification,on)),!,

make_sound(R,S),

drule(F,S).

rule(F,R) :-

drule(F,R).

drule(F,if(H,B)) :-

!,

make_anc(H),

make_bodies(H,T,[Ths,Anc,Ans],ProveH,ExH),

form_anc(H,Anc,Newanc),

make_bodies(B,T,[Ths,Newanc,Ans],ProveB,ExB),

prolog_cl((ProveH:-ProveB)),

( F=fact,

prolog_cl((ExH:-ExB))

; F=constraint).

drule(F,H) :-

make_anc(H),

make_bodies(H,T,[ths(T,T,D,D),_,ans(A,A)],ProveH,ExH),

prolog_cl(ProveH),

( F=fact,

prolog_cl(ExH)

; F=constraint).

/*

f orm anc(L, A1, A2) means that A2 is the ancestor form for subgoal L

with previous ancestor form A1.

*/

form_anc(n(G), anc(P,N), anc(P,[G|N])) :- !.

form_anc(G, anc(P,N), anc([G|P],N)).

/*

![139778813121424](images/139778813121424)

Theorist to Prolog (th2.tex)

5.3 Forming Contrapositives

For both facts and constraints we convert the user syntax into negation nor-

mal form (section ??), form the contrapositives, and declare these as rules.

Note that here we choose an arbitrary ordering for the clauses in the

bodies of the contrapositive forms of the facts. No attempt has been made

to optimise this, although it is noted that some orderings are more eﬃcient

than others (see for example [?] for a discussion of such issues).

The declarations are as follows:

*/

declare_fact(F) :-

nnf(F,even,N),

rulify(fact,N).

declare_constraint(C) :-

nnf(C,even,N),

rulify(constraint,N).

/*

nnf (Wﬀ,Parity,Nnf ) (section ??) means that Nnf is the negation normal

form of Wﬀ if Parity=even and of ¬Wﬀ if Parity=odd. Note that we rulify

the normal form of the negation of the formula.

rulify(H, N) where H is either “fact” or “constraint” and N is the nega-

tion of a fact or constraint in negation normal form (see section ??), means

that all rules which can be formed from N (by allowing each atom in N being

the head of some rule) should be declared as such.

*/

rulify(H,(A,B)) :- !,

contrapos(H,B,A),

contrapos(H,A,B).

rulify(H,(A;B)) :- !,

rulify(H,A),

rulify(H,B).

rulify(H,n(A)) :- !,

Theorist to Prolog (th2.tex)

rule(H,A).

rulify(H,A) :-

rule(H,n(A)).

/*

contrapos(H, D, T ) where H is either “fact” or “constraint”, and (D, T )

is (the negation of) a formula in negation normal form means that all rules

which can be formed from (D, T ) with head of the rule coming from T should

be formed. Think of D as the literals for which the rules with them as heads

have been formed, and T as those which remain to be as the head of some

rule.

*/

contrapos(H,D, (L,R)) :- !,

contrapos(H,(R,D),L),

contrapos(H,(L,D),R).

contrapos(H,D,(L;R)) :- !,

contrapos(H,D,L),

contrapos(H,D,R).

contrapos(H,D,n(A)) :- !,

rule(H,if(A,D)).

contrapos(H,D,A) :-

rule(H,if(n(A),D)).

/*

Example 9 if we are to rulify the negation normal form

n(p(A)), q(A), (r(A), s(A); t(A)), m(A)

we generate the following rule forms, which can then be given to rule

p(A) ← q(A), (r(A), s(A); t(A)), m(A)

n(q(A)) ← (r(A), s(A); t(A)), m(A), n(p(A))

n(r(A)) ← s(A), m(A), q(A), n(p(A))

n(s(A)) ← r(A), m(A), q(A), n(p(A))

n(t(A)) ← m(A), q(A), n(p(A))

n(m(A)) ← (r(A), s(A); t(A)), q(A), n(p(A))

Theorist to Prolog (th2.tex)

5.4 Sound Uniﬁcation

Sound uniﬁcation works, by checking for repeated variables in the left hand

side of a rule, and then uniﬁes them by hand. This idea was stolen from

Stickel’s implementation.

*/

make_sound(if(H,B),if(NH,NB)) :- !,

rms(H,NH,[],_,B,NB).

make_sound(H,NR) :-

rms(H,NH,[],_,true,NB),

(NB=true,NR=H;

NR=if(NH,NB)),!.

rms(V,V1,L,L,B,(unif(V,V1),B)) :-

var(V),

id_member(V,L),!.

rms(V,V,L,[V|L],B,B) :-

var(V),!.

rms([H|T],[H1|T1],L1,L3,B1,B3) :- !,

rms(H,H1,L1,L2,B1,B2),

rms(T,T1,L2,L3,B2,B3).

rms(A,A,L,L,B,B) :-

atomic(A),!.

rms(S,S2,L1,L2,B1,B2) :-

S =.. L,

rms(L,LR,L1,L2,B1,B2),

S2 =.. LR.

/*

*/

unif(X,Y) :-

var(X), var(Y), X=Y,!.

unif(X,Y) :-

var(X),!,

\+ appears_in(X,Y),

Theorist to Prolog (th2.tex)

X=Y.

unif(X,Y) :-

var(Y),!,

\+ appears_in(Y,X),

X=Y.

unif(X,Y) :-

atomic(X),!,X=Y.

unif([H1|T1],[H2|T2]) :- !,

unif(H1,H2),

unif(T1,T2).

unif(X,Y) :-

\+ atomic(Y),

X=..XS,

Y=..YS,

unif(XS,YS).

appears_in(X,Y) :-

var(Y),!,X==Y.

appears_in(X,[H|T]) :- !,

(appears_in(X,H); appears_in(X,T)).

appears_in(X,S) :-

\+ atomic(S),

S =.. L,

appears_in(X,L).

/*

5.5 Possible Hypotheses

The other class of things we have to worry about is the class of possible

hypotheses. As described in [?] and outlined in section ??, we only need

worry about atomic possible hypotheses.

If d(−args−) is a possible hypothesis (default), then we want to form the

target code as follows:

1. We can only prove a hypothesis if we have already assumed it:

prove d(−args−, T hs, Anc) : −

member(d(−args−), T hs).

![139778811095056](images/139778811095056)

Theorist to Prolog (th2.tex)

2. We can explain a default if we have already assumed it:

ex d(−args−, ths(T, T, D, D), Anc, ans(A, A)) : −

member(d(−args−), T ).

3. We can explain a hypothesis by assuming it, if it has no free variables,

we have not already assumed it and it is consistent with everything

assumed before:

ex d(−args−, ths(T, [d(−args−)|T ], D, D), Anc, ans(A, A)) : −

variable free(d(−args−)),

\ + (member(d(−args−), T )),

\ + (prove not d(−args−, [d(−args−)|T ], anc([], []))).

4. If a hypothesis has free variables, it can be explained by adding it to

the deferred defaults list (making no assumptions about its consistency;

this will be checked at the end of the explanation phase):

ex d(−args−, ths(T, T, D, [d(−args−)|D], Anc, ans(A, A)) : −

\ + (variable free(d(−args−))).

The following compiles directly into such code:

*/

declare_default(D) :-

make_anc(D),

new_lit("prove_",D,[T,_],Pr_D),

prolog_cl((Pr_D :- member(D,T))),

new_lit("ex_",D, [ths(T,T,Defer,Defer), _, ans(A,A)], ExD),

prolog_cl((ExD :- member(D, T))),

new_lit("ex_",D, [ths(T,[D|T],Defer,Defer), _, ans(A,A)], ExDass),new_lit("prove_not_",D, [[D|T],anc([],[])],Pr_not_D),

prolog_cl((ExDass :- variable_free(D), \+member(D,T),

\+Pr_not_D)),

new_lit("ex_",D, [ths(T,T,Defer,[D|Defer]), _, ans(A,A)], ExDefer),prolog_cl((ExDefer :- \+ variable_free(D))).

/*

![139778810515152](images/139778810515152)

![139778810557200](images/139778810557200)

![139778810131472](images/139778810131472)

![139778810164048](images/139778810164048)

![139778810204240](images/139778810204240)

![139778810204624](images/139778810204624)

![139778810304400](images/139778810304400)

Theorist to Prolog (th2.tex)

Example 10 The default

birdsﬂy(A)

gets translated into

prove birdsﬂy(A, B, C) : −

member(birdsﬂy(A), B)

ex birdsﬂy(A, ths(B, B, C, C), D, ans(E, E)) : −

member(birdsﬂy(A), B)

ex birdsﬂy(A, ths(B, [birdsﬂy(A)|B], C, C), D, ans(E, E)) : −

variable free(birdsﬂy(A)),

\+member(birdsﬂy(A), B),

\+prove not birdsﬂy(A, [birdsﬂy(A)|B], anc([], []))

ex birdsﬂy(A, ths(B, B, C, [birdsﬂy(A)|C]), D, ans(E, E)) : −

\+variable free(birdsﬂy(A))

5.6 Relations deﬁned in Prolog

We can deﬁne some relations to be executed in Prolog. This means that we

can prove the prove and ex forms by calling the appropriate Prolog deﬁnition.

*/

declare_prolog(G) :-

new_lit("ex_",G, [ths(T,T,D,D), _, ans(A,A)], ExG),

prolog_cl((ExG :- G)),

new_lit("prove_",G,[_,_],PrG),

prolog_cl((PrG :- G)).

/*

5.7 Explaining Observations

expl(G, T 0, T 1, A) means that T 1 is an explanation of G or A (A being the

alternate answers) from the facts given T 0 is already assumed. G is an

arbitrary wﬀ.

*/

expl(G,T0,T1,Ans) :-

![139778809696080](images/139778809696080)

![139778809746512](images/139778809746512)

![139778809749136](images/139778809749136)

![139778809781776](images/139778809781776)

![139778809814352](images/139778809814352)

![139778809850384](images/139778809850384)

![139778809850768](images/139778809850768)

![139778809353744](images/139778809353744)

Theorist to Prolog (th2.tex)

make_ground(N),

declare_fact(’<-’(newans(N,G) , G)),

ex_newans(N,G,ths(T0,T,[],D),anc([],[]),ans([],Ans)),

make_ground(D),

check_consis(D,T,T1).

/*

*/

check_consis([],T,T).

check_consis([H|D],T1,T) :-

new_lit("prove_not_",H, [T1,anc([],[])], Pr_n_H),

\+ Pr_n_H,

check_consis(D,[H|T1],T).

/*

To obtain disjunctive answers we have to know if the negation of the top

level goal is called. This is done by declaring the fact newans(G) ← G, and

if we ever try to prove the negation of a top level goal, we add that instance

to the list of alternate answers. In this implementation we also check that

G is not identical to a higher level goal. This removes most cases where we

have a redundant assumption (i.e., when we are not gaining a new answer,

but only adding redundant information).

*/

:- dynamic ex_not_newans/5.

:- dynamic prove_not_newans/4.

ex_not_newans(N,G,ths(T,T,D,D),anc(Pos,Neg),ans(A,[G|A])) :-

member(newans(N,_),Pos),

\+ id_anc(G,anc(Pos,Neg)).

id_anc(n(G),anc(_,N)) :- !, id_member(G,N).

id_anc(G,anc(P,_)) :- id_member(G,P).

/*

5.8 Ancestor Search

Our linear resolution theorem prover must recognise that a goal has been

proven if it uniﬁes with an ancestor in the search tree. To do this, it keeps

Theorist to Prolog (th2.tex)

two lists of ancestors, one containing the positive (non negated) ancestors

and the other the negated ancestors. When the ancestor search rules for

predicate p are deﬁned, we assert ancestor recorded(p), so that we do not

attempt to redeﬁne the ancestor search rules.

*/

:- dynamic ancestor_recorded/1.

make_anc(_) :-

flag((ancestor_search,off)),

flag((loop_check,off)),

flag((depth_bound,off)),

!.

make_anc(Name) :-

ancestor_recorded(Name),

!.

make_anc(n(Goal)) :-

!,

make_anc(Goal).

make_anc(Goal) :-

Goal =.. [Pred|Args],

same_length(Args,Nargs),

NG =.. [Pred|Nargs],

make_bodies(NG,_,[ths(T,T,D,D),anc(P,N),ans(A,A)],ProveG,ExG),make_bodies(n(NG),_,[ths(T,T,D,D),anc(P,N),ans(A,A)],ProvenG,ExnG),( flag((loop_check,off))

;

prolog_cl((ProveG :- id_member(NG,P),!,fail)),

prolog_cl((ProvenG :- id_member(NG,N),!,fail)),

prolog_cl((ExG :- id_member(NG,P),!,fail)),

prolog_cl((ExnG :- id_member(NG,N),!,fail))

),

( flag((ancestor_search,off))

;

prolog_cl((ProveG :- member(NG,N))),

prolog_cl((ProvenG :- member(NG,P))),

prolog_cl((ExG :- member(NG,N))),

prolog_cl((ExnG :- member(NG,P)))

![139778808364432](images/139778808364432)

Theorist to Prolog (th2.tex)

),

( flag((depth_bound,off)), !

;

prolog_cl((ProveG :- (flag((depth_bound,MD))),

number(MD),length(P,LP),length(N,LN),LP+LN>MD,!,fail)),prolog_cl((ProvenG :- (flag((depth_bound,MD))),

number(MD),length(P,LP),length(N,LN),LP+LN>MD,!,fail)),prolog_cl((ExG :- (flag((depth_bound,MD))),

number(MD),length(P,LP),length(N,LN),LP+LN>MD,!,fail)),prolog_cl((ExnG :- (flag((depth_bound,MD))),

number(MD),length(P,LP),length(N,LN),LP+LN>MD,!,fail))),

assert(ancestor_recorded(NG)),

!.

/*

Example 11 If we do a call

make anc(gr(A,B))

we create the Prolog clauses

prove gr(A,B,C,anc(D,E)) : −

member(gr(A,B),E).

prove not gr(A,B,C,anc(D,E)) : −

member(gr(A,B),D).

ex gr(A,B,ths(C,C,D,D),anc(E,F),ans(G,G)) : −

member(gr(A,B),F).

ex not gr(A,B,ths(C,C,D,D),anc(E,F),ans(G,G)) : −

member(gr(A,B),E).

This is only done once for the gr relation.

Interface

In this section a minimal interface is given. We try to give enough so that

we can understand the conversion of the wﬀ form into negation normal form

and the parsing of facts and defaults. There is, of course, much more in any

usable interface than described here.

![139778807959504](images/139778807959504)

![139778807722320](images/139778807722320)

![139778807725136](images/139778807725136)

![139778807240720](images/139778807240720)

![139778807241104](images/139778807241104)

![139778807276688](images/139778807276688)

![139778807305360](images/139778807305360)

Theorist to Prolog (th2.tex)

6.1 Syntax Declarations

All of the declarations we use will be deﬁned as operators. This will allow us

to use inﬁx forms of our wﬀs, for extra readability. Here we use the standard

Edinburgh operator declarations which are given in the spirit of being enough

to make the rest of the description self contained.

*/

:- dynamic flag/1.

:- op(1150,fx,default).

:- op(1150,fx,fact).

:- op(1150,fx,constraint).

:- op(1150,fx,prolog).

:- op(1150,fx,explain).

:- op(1150,fx,predict).

:- op(1150,fx,define).

:- op(1150,fx,set).

:- op(1150,fx,flag).

:- op(1150,fx,reset).

:- op(1150,fy,h).

:- op(1150,fx,thconsult).

:- op(1150,fx,thtrans).

:- op(1150,fx,thcompile).

:- op(1130,xfx,:).

:- op(1120,xfx,==).

:- op(1120,xfx,equiv).

:- op(1110,xfx,<-).

:- op(1110,xfx,=>).

:- op(1100,xfy,or).

:- op(1000,xfy,and).

:- op(1000,xfy,&).

:- op(950,fy,~).

:- op(950,fy,not).

/*

Theorist to Prolog (th2.tex)

6.2 Converting to Negation Normal Form

We want to convert an arbitrarily complex formula into a standard form

called negation normal form. Negation normal form of a formula is an equiv-

alent formula consisting of conjunctions and disjunctions of literals (either an

atom or of the form n(A) where A is an atom). The relation deﬁned here puts

formulae into negation normal form without mapping out subterms. Usually

we want to ﬁnd the negation normal form of the negation of the formula, as

this is the form suitable for use in the body of a rule.

The predicate used is of the form

nnf (F la, P arity, Body)

where

F la is a formula with input syntax

P arity is either odd or even and denotes whether F la is in the context of

an odd or even number of negations.

Body is a tuple which represents the negation normal form of the negation

of F la if parity is even and the negation normal form of F la if parity

is odd.

*/

nnf((X equiv Y), P,B) :- !,

nnf(((Y or not X) and (X or not Y)),P,B).

nnf((X == Y), P,B) :- !,

nnf(((Y or not X) and (X or not Y)),P,B).

nnf((X => Y), P,B) :- !,

nnf((Y or not X),P,B).

nnf((Y <- X), P,B) :- !,

nnf((Y or not X),P,B).

nnf((X & Y), P,B) :- !,

nnf((X and Y),P,B).

nnf((X , Y), P,B) :- !,

nnf((X and Y),P,B).

nnf((X ; Y), P,B) :- !,

Theorist to Prolog (th2.tex)

nnf((X or Y),P,B).

nnf((X and Y),P,B) :- !,

opposite_parity(P,OP),

nnf((not X or not Y),OP,B).

nnf((X or Y),even,(XB,YB)) :- !,

nnf(X,even,XB),

nnf(Y,even,YB).

nnf((X or Y),odd,(XB;YB)) :- !,

nnf(X,odd,XB),

nnf(Y,odd,YB).

nnf((~ X),P,B) :- !,

nnf((not X),P,B).

nnf((not X),P,B) :- !,

opposite_parity(P,OP),

nnf(X,OP,B).

nnf(F,odd,F).

nnf(n(F),even,F) :- !.

nnf(F,even,n(F)).

/*

*/

opposite_parity(even,odd).

opposite_parity(odd,even).

/*

Example 12 the wﬀ

(a or not b) and c ⇒ d and (not e or f )

with parity odd gets translated into

d, (e; f ); n(a), b; n(c)

the same wﬀ with parity even (i.e., the negation of the wﬀ) has negation

normal form:

(n(d); e, n(f )), (a; n(b)), c

Theorist to Prolog (th2.tex)

6.3 Theorist Declarations

The following deﬁne the Theorist declarations. Essentially these operators

just call the appropriate compiler instruction. These commands cannot be

undone by doing a retry to them; the compiler assertions will be undone on

backtracking.

*/

fact F :- declare_fact(F),!.

/*

The def ault declaration makes the appropriate equivalences between the

named defaults and the unnamed defaults.

*/

default N : H :-

!,

declare_default(N),

declare_fact((H <-N)),

!.

default N :-

declare_default(N),

!.

/*

*/

constraint C :-

declare_constraint(C),

!.

/*

The prolog command says that the atom G should be proven in the Prolog

system. The argument of the def ine statement is a Prolog deﬁnition which

should be asserted (N.B. that it should be in parentheses if it contains a

“:-”.

*/

prolog G :-

declare_prolog(G).

/*

Theorist to Prolog (th2.tex)

*/

define G :-

prolog_cl(G).

/*

The explain command keeps writing out all of the explanations found.

This is done by ﬁnding one, writing the answer, and then retrying so that

the next answer is found. This is done so that the computation is left in an

appropriate state at the end of the computation.

*/

% :- dynamic statistics/2.

explain G :-

(flag timing,on),

statistics(cputime,_),

expl(G,[],D,A),

statistics(cputime,[_,Time]),

writeans(G,D,A),

format(’took ~3d sec.~n~n’,[Time]),

fail

expl(G,[],D,A),

writeans(G,D,A),

fail.

;

/*

*/

writeans(G,D,A) :-

format(’~nAnswer is ~w’, [G]),

writedisj(A),

format(’~nTheory is ~w~n’, [D]),

!.

writedisj([]).

writedisj([H|T]) :-

writedisj(T),

Theorist to Prolog (th2.tex)

format(’ or ~w’,[H]).

/*

6.4 Prediction

In [?] we present a sceptical view of prediction argue that one should predict

what is in every extension. The following theorem proved in [?] gives us a

hint as to how it should be implemented.

Theorem 4 g is not in every extension iﬀ there exists a scenario S such

that g is not explainable from S.

The intuition is that if g is not in every extension then there is no reason

to rule out S (based on the information given) and so we should not predict

g.

We can use theorem ?? to consider another way to view membership in

every extension. Consider two antagonistic agents Y and N trying to deter-

mine whether g should be predicted or not. Y comes up with explanations

of g, and N tries to ﬁnd where these explanations fall down (i.e., tries to

ﬁnd a scenario S which is inconsistent with all of Y ’s explanations). Y then

tries to ﬁnd an explanation of g given S. If N cannot ﬁnd a defeater for Y ’s

explanations then g is in every extension, and if Y cannot ﬁnd an explanation

from some S constructed by N then g is not in every extension.

The following code implements this, but (as we cannot implement corou-

tines as needed above in Prolog), it may generate more explanations of the

goal than is needed. What we really want is for the ﬁrst “bagof” to gen-

erate the explanations in a demand-driven fashion, and then just print the

explanations needed.

*/

predict G :-

bagof(E,expl(G,[],E,[]),Es),

predct(G,Es).

predct(G,Es) :-

simplify_expls(Es,SEs),

( find_counter(SEs,[],S),

!,

Theorist to Prolog (th2.tex)

format(’No, ~q is not explainable from ~q.~n’,[G,S])

; format(’Yes, ~q is in all extensions.~nExplanations are:~n’,[G]),list_scens(1,SEs)).

/*

*/

find_counter([],S,S).

find_counter([E|R],S0,S2) :-

member(D,E),

expl2not(D,S0,S1),

find_counter(R,S1,S2).

/*

*/

list_scens(_,[]).

list_scens(N,[H|T]) :-

format(’~q: ~q.~n’,[N,H]),

N1 is N+1,

list_scens(N1,T).

/*

expl2not(G, T 0, T 1) is true if ground ¬G is explainable starting from sce-

nario T 0, with resulting explanation T 1. No disjunctive answers are formed.

*/

expl2not(G,T0,T1) :-

new_lit("ex_not_",G,[ths(T0,T,[],D),anc([],[]),ans([],[])],ExG),ExG,

make_ground(D),

check_consis(D,T,T1).

/*

Theorist to Prolog (th2.tex)

6.5 Simplifying Explanations

*/

simplify_expls([S],[S]).

simplify_expls([H|T], S) :-

simplify_expls(T, TS),

mergeinto(H,TS,S).

/*

*/

mergeinto(L,[],[L]).

mergeinto(L,[A|R],[A|R]) :-

instance_of(A,L),

!.

mergeinto(L,[A|R],N) :-

instance_of(L,A),

!,

mergeinto(L,R,N).

mergeinto(L,[A|R],[A|N]) :-

mergeinto(L,R,N).

/*

*/

instance_of(D,S) :-

remove_all(D,S,_).

/*

6.6 File Handling

To consult a Theorist ﬁle, you should do a,

thconsult ﬁlename.

Theorist to Prolog (th2.tex)

The following is the deﬁnition of thconsult. Basicly we just keep reading the

ﬁle and executing the commands in it until we stop.

*/

thconsult File :-

current_input(OldFile),

open(File,read,Input),

set_input(Input),

read(T),

read_all(T),

set_input(OldFile).

/*

*/

read_all(end_of_file) :- !.

read_all(T) :-

((flag asserting,on),!; format(’~n% ~w.~n’,[T])),

(call(T);format(’Warning: ~w failed~n’,[T])),

read(T2),

read_all(T2).

/*

thtrans is like the previous version, but the generated code is written to

a ﬁle. This code is neither loaded or compiled.

*/

thtrans File :-

current_input(Oldinput),

open(File,read,Input),

set_input(Input),

name(File, Fname),

append(Fname,".pl",Plfname),

name(Plfile, Plfname),

current_output(Oldoutput),

open(Plfile,write,Output),

set_output(Output),

Theorist to Prolog (th2.tex)

format(’:- dynamic contrapos_recorded/1.~n’,[]),

(set asserting,off),

read(T),

read_all(T),

close(Output),

set_input(Oldinput),

set_output(Oldoutput),

(reset asserting).

/*

To compile a Theorist ﬁle, you should do a,

thconsult ﬁlename.

This translates the code to Prolog and then compiles the prolog code.

thcompile translates the ﬁle to Prolog which is then compiled using the

Prolog compiler.

*/

thcompile File :-

(thtrans File),

%

no_style_check(all),

compile(File).

/*

6.7 Flag Setting

There are a number of Theorist options which can be set by ﬂag declarations.

Flags, by default, are on. To set the ﬂag f to value v you can issue the

command

set f, v.

To ﬁnd out the value of the ﬂag f issue the command

ﬂag f, V.

You can reset the value of ﬂag f to its old value by

reset f.

Theorist to Prolog (th2.tex)

The list of all ﬂags is given by the command

ﬂags.

The following is the deﬁnition of these

*/

set F,V :-

prolog_decl((flag F,V1 :- !,V=V1)).

/*

*/

flag _,on.

/*

*/

reset F :-

retract((flag F,_ :- !,_=_)).

/*

*/

flags :- list_flags([asserting,ancestor_search,loop_check,

depth_bound,sound_unification,timing]).

list_flags([]).

list_flags([H|T]) :-

(flag H,V),

format(’flag ~w,~w.~n’,[H,V]),

list_flags(T).

/*

6.8 Compiler Directives

There are some compiler directives which need to be added to Theorist code

so that the Prolog to assembly language compiler can work (these are trans-

lated into the appropriate Quintus compiler directives).

So that the Quintus compiler can correctly compile the code, we should

declare that all calls for which we can assert the goal or the negative are

dynamic, this is done by the command

Theorist to Prolog (th2.tex)

dyn n.

This need only be given in ﬁles, and should be given before the atomic symbol

n is ever used.

The following gives the appropriate translation. Essentially we then must

say that the appropriate Prolog code is dynamic.

*/

:- op(1150,fx,dyn).

dyn _ :-

(flag asserting, on),

!.

dyn n(G) :-

!,

(dyn G).

dyn G :-

G =.. [R|Args],

add_prefix("ex_not_",R,ExNR),

add_prefix("ex_",R,ExR),

length(Args,NA),

ExL is NA + 3,

format(’:- dynamic ~a/~d.~n’,[ExNR,ExL]),

format(’:- dynamic ~a/~d.~n’,[ExR,ExL]),

add_prefix("prove_not_",R,PrNR),

add_prefix("prove_",R,PrR),

PrL is NA + 2,

format(’:- dynamic ~a/~d.~n’,[PrNR,PrL]),

format(’:- dynamic ~a/~d.~n’,[PrR,PrL]).

/*

6.9 Using the Compiled Rules

The use of conditional asserting (prolog cl) is twofold. The ﬁrst is to write

the condition to a ﬁle if that is desired. The second is to be a backtrackable

assert otherwise.

*/

prolog_cl(C) :-

![139778803552528](images/139778803552528)

Theorist to Prolog (th2.tex)

flag((asserting,off)),

!,

\+ \+ (

numbervars(C,0,_),

writeq(C),

write(’.’),

nl).

prolog_cl(C) :-

assertz(C).

prolog_cl(C) :-

retract(C),

fail.

/*

prolog decl is like the above predicate, but is both written to the ﬁle and

asserted.

*/

prolog_decl(C) :-

flag((asserting,off)),

numbervars(C,0,_),

writeq(C),

write(’.’),

nl,

fail.

prolog_decl(C) :-

asserta(C).

prolog_decl(C) :-

retract(C),

fail.

/*

6.10 Saving Theorist

The command “save” automagically saves the state of the Theorist code as

the command ”theorist”. This is normally done straight after compiling this

ﬁle.

![139778801078160](images/139778801078160)

Theorist to Prolog (th2.tex)

*/

save :-

save_program(th,

format(’~nWelcome to THEORIST 1.1.1

(4 December 89 version)

For help type ‘‘h.’’.

Any Problems see David Poole (poole@cs.ubc.ca)~n’,[])).

/*

7 Utility Functions

7.1 List Predicates

append(X, Y, Z) is the normal append function

append([],L,L).

append([H|X],Y,[H|Z]) :-

append(X,Y,Z).

*/

member(A,[A|_]).

member(A,[_|R]) :-

member(A,R).

/*

id member(X, L) is true if X is identical to some member of list L.

*/

id_member(A,[B|_]) :-

A==B.

id_member(A,[_|R]) :-

id_member(A,R).

/*

*/

same_length([],[]).

![139778800922576](images/139778800922576)

Theorist to Prolog (th2.tex)

same_length([_|L1],[_|L2]) :-

same_length(L1,L2).

/*

*/

remove(A,[A|B],B).

remove(A,[H|T],[H|R]) :-

remove(A,T,R).

/*

*/

remove_all([],L,L).

remove_all([H|T],L,L2) :-

remove(H,L,L1),

remove_all(T,L1,L2).

/*

7.2 Looking at Terms

*/

variable_free(X) :-

atomic(X),

!.

variable_free(X) :-

var(X),

!,

fail.

variable_free([H|T]) :-

!,

variable_free(H),

variable_free(T).

variable_free(X) :-

X =.. Y,

variable_free(Y).

/*

Theorist to Prolog (th2.tex)

*/

make_ground(X) :-

retract(groundseed(N)),

numbervars(X,N,NN),

asserta(groundseed(NN)).

:- dynamic groundseed/1.

groundseed(26).

/*

*/

reverse([],T,T).

reverse([H|T],A,B) :-

reverse(T,A,[H|B]).

/*

7.3 Help Commands

*/

(h) :- format(’This is Theorist 1.1 (all complaints to David Poole)For more details issue the command:

h H.

where H is one of:~n’,[]),

unix(system(’ls /faculty/poole/theorist/help’)).

(h H) :- !,

add_prefix("more /faculty/poole/theorist/help/",H,Cmd),

unix(system(Cmd)).

/*

7.4 Runtime Considerations

What is given here is the core part of our current implementation of Theorist.

This code has been used with Waterloo Unix Prolog, Quintus Prolog, C-

prolog and Mac-Prolog. For those Prologs with compilers we can actually

Theorist to Prolog (th2.tex)

compile the resulting code from this translater as we could any other Prolog

code; this make it very fast indeed.

The resulting code when the Theorist code is of the form of deﬁnite clauses

(the only case where a comparison makes sense, as it is what the two systems

have in common), runs at about a quarter the speed of the corresponding

interpreted or compiled code of the underlying Prolog system. About half of

this extra cost is for the extra arguments to unify, and the other factor is for

one membership of an empty list for each procedure call. For each procedure

call we do one extra Prolog call which immediately fails. For the deﬁnite

clause case, the contrapositive of the clauses are never used.

8 Conclusion

This paper has described in detail how we can translate Theorist code into

prolog so that we can use the advances in Prolog implementation Technology.

As far as this compiler is concerned there are a few issues which arise:

• Is there a more eﬃcient way to determine that a goal can succeed

because it uniﬁes with an ancestor [?, ?]?

• Can we incorporate a cycle check that has a low overhead? A simple,

but expensive, version is implemented in some versions of our compiler

which checks for identical ancestors.

• Are there optimal ordering which we can put the compiled clauses in so

that we get answer most quickly [?]? At the moment the compiler just

puts the elements of the bodies in an arbitrary ordering. The optimal

ordering depends, of course, on the underlying control structure.

• Are there better ways to do the consistency checking when there are

variables in the hypotheses?

We are currently working on many applications of default and abductive

reasoning. Hopefully with compilers based on the ideas presented in this

paper we will be able to take full advantage of advances in Prolog imple-

mentation technology while still allowing ﬂexibility in speciﬁcation of the

problems to be solved.

Theorist to Prolog (th2.tex)

Acknowledgements

This work could not have been done without the ideas, criticism and feed-

back from Randy Goebel, Eric Neufeld, Paul Van Arragon, Scott Goodwin

and Denis Gagn´e. Thanks to Brenda Parsons and Amar Shan for valuable

comments on a previous version of this paper. This research was supported

under NSERC grant A6260.

References

[Brewka86] G. Brewka, “Tweety – Still Flying: Some Remarks on Abnormal

Birds, Applicable Rules and a Default Prover”, Proc. AAAI-86,

pp. 8-12.

[Chang73] C-L. Chang and R. C-T. Lee, Symbolic Logic and Mechanical

Theorem Proving, Academic Press, 1973.

[Cox82]

P. T. Cox, Dependency-directed backtracking for Prolog Programs.

[Cox87]

P. T. Cox and T. Pietrzykowski, General Diagnosis by Abductive

Inference, Technical report CS8701, School of Computer Science,

Technical University of Nova Scotia, April 1987.

[Dincbas87] M. Dincbas, H. Simonis and P. Van Hentenryck, Solving Large

Combinatorial Problems in Logic Programming, ECRC Technical

Report, TR-LP-21, June 1987.

[Doyle79] J. Doyle, “A Truth Maintenance System”, Artiﬁcial Intelligence,

Vol. 12, pp 231-273.

[de Kleer86] J. de Kleer, “An Assumption-based TMS”, Artiﬁcial Intelli-

gence, Vol. 28, No. 2, pp. 127-162.

[Edmonson87] R. Edmonson, ????

[Enderton72] H. B. Enderton, A Mathematical Introduction to Logic, Aca-

demic Press, Orlando.

[Genesereth87] M. Genesereth and N. Nilsson, Logical Foundations of Arti-

ﬁcial Intelligence, Morgan-Kaufmann, Los Altos, California.

Theorist to Prolog (th2.tex)

[Ginsberg87] M. L. Ginsberg, Computing Circumscription, Stanford Logic

Group Report Logic-87-8, June 1987.

[Goebel87] R. G. Goebel and S. D. Goodwin, “Applying theory formation to

the planning problem” in F. M. Brown (Ed.), Proceedings of the

1987 Workshop on The Frame Problem in Artiﬁcial Intelligence,

Morgan Kaufmann, pp. 207-232.

[Kowalski79] R. Kowalski, “Algorithm = Logic + Control”, Comm. A.C.M.

Vol 22, No 7, pp. 424-436.

[Lifschitz85] V. Lifschitz, “Computing Circumscription”, Proc. IJCAI85,

pp. 121-127.

[Lloyd87] J. Lloyd, Foundations of Logic Programming, Springer-Verlag,

2nd Edition.

[Loveland78] D. W. Loveland, Automated Theorem Proving: a logical basis,

North-Holland, Amsterdam.

[Loveland87] D. W. Loveland, “Near-Horn Logic Programming”, Proc. 6th

International Logic Programming Conference.

[McCarthy86] J. McCarthy, “Applications of Circumscription to Formalising

Common Sense Knowledge”, Artiﬁcial Intelligence, Vol. 28, No.

1, pp. 89-116.

[Moto-Oka84] T. Moto-Oka, H. Tanaka, H. Aida,

k. Hirata and

T. Maruyama, “The Architecture of a Parallel Inference Engine

— PIE”, Proc. Int. Conf. on Fifth Generation Computing Sys-

tems, pp. 479-488.

[Naish86] L. Naish, “Negation and Quantiﬁers in NU-PROLOG”, Proc. 3rd

Int. Conf. on Logic Programming, Springer-Verlag, pp. 624-634.

[Neufeld87] E. M. Neufeld and D. Poole, “Towards solving the multiple ex-

tension problem: combining defaults and probabilities”, Proc.

Third AAAI Workshop on Reasoning with Uncertainty, Seattle,

pp. 305-312.

Theorist to Prolog (th2.tex)

[Poole84] D. L. Poole, “Making Clausal theorem provers Non-clausal”,

Proc. CSCSI-84. pp. 124-125.

[Poole86] D. L. Poole, “Gracefully adding Negation to Prolog”, Proc. Fifth

International Logic Programming Conference, London, pp. 635-

641.

[Poole86] D. L. Poole, “Default Reasoning and Diagnosis as Theory For-

mation”, Technical Report, CS-86-08, Department of Computer

Science, University of Waterloo, March 1986.

[Poole87a] D. L. Poole, “Variables in Hypotheses”, Proc. IJCAI-87, pp. 905-

908.

[Poole87b] D. L. Poole, Defaults and Conjectures: Hypothetical Reasoning

for Explanation and Prediction, Research Report CS-87-54, De-

partment of Computer Science, University of Waterloo, October

1987, 49 pages.

[Poole88] D. L. Poole, A Logical Framework for Default Reasoning, to ap-

pear Artiﬁcial Intelligence, Spring 1987.

[PGA87] D. L. Poole, R. G. Goebel and R. Aleliunas, “Theorist: A Logical

Reasoning System for Defaults and Diagnosis”, in N. Cercone

and G. McCalla (Eds.) The Knowledge Frontier: Essays in the

Representation of Knowledge, Springer Varlag, New York, 1987,

pp. 331-352.

[Reiter80] R. Reiter, “A Logic for Default Reasoning”, Artiﬁcial Intelli-

gence, Vol. 13, pp 81-132.

[Smith86] D. Smith and M. Genesereth, “Ordering Conjunctive Queries”,

Artiﬁcial Intelligence.

[Van Hentenryck87] P. Van Hentenryck, “A Framework for consistency tech-

niques in Logic Programming” IJCAI-87, Milan, Italy.

