# logicmoo_base

Is probably 50 packages...  (do not try these before 0.0.11)

# Part A - Utilities

````
?- ensure_loaded(library(logicmoo/logicmoo_utils)).
````

# 1) Variable names in listing!

````
dmiles@gitlab:~$ swipl
Welcome to SWI-Prolog (Multi-threaded, 64 bits, Version 7.3.9)
Copyright (c) 1990-2015 University of Amsterdam, VU Amsterdam
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software,
and you are welcome to redistribute it under certain conditions.
Please visit http://www.swi-prolog.org for details.
For help, use ?- help(Topic). or ?- apropos(Word).
?- ensure_loaded(library(logicmoo/logicmoo_utils)).
% /home/dmiles/lib/swipl/pack/logicmoo_base/prolog/logicmoo/logicmoo_utils.pl:157
% Adding logicmoo/utils to autoload path
true.

?- listing(ls).
shell:ls :-
        ls('.').
shell:prolog_listing:ls(A) :-
        name_to_files(A, B),
        ls_(B).

true.
?- use_listing_vars.
true.
?- listing(ls).
shell:ls :-
        ls('.').
shell:ls(Spec) :-
        name_to_files(Spec, Matches),
        ls_(Matches).
true.
````

# 2) Less trace button pushing!


````
?- rtrace(member(X,[1,2,3])).
   Call: (11) [lists] lists:member(_G3260,[1,2,3])
   Unify: (11) [lists] lists:member(_G3260,[1,2,3])
   Exit: (11) [lists] lists:member(1,[1,2,3])
X = 1 ;
   Redo: (11) [lists] lists:member(_G3260,[1,2,3])
   Exit: (11) [lists] lists:member(2,[1,2,3])
X = 2 ;
   Redo: (11) [lists] lists:member(_G3260,[1,2,3])
   Exit: (11) [lists] lists:member(3,[1,2,3])
X = 3.

````


# 3) hotrace/1 fixes notrace/1 determinancy!


````
?- notrace(member(X,[1,2,3])).
X = 1.
````

Oops it had an implicit once/1

Dont worry..

````
?- trace,hotrace(member(X,[1,2,3])).
X = 1 ;
X = 2 ;
X = 3.
[trace]  ?-

````

(Of course it hides the trace just like trace/1)



# 4) special assertion/1 predicates  must/1, sanity/1, on_x_rtrace/1 for code you are in the middle of writing 

````
?- must(true).
true.

?- must(fail).
% failed(debugOnFailure0('_catch':on_x_debug(user:fail))).
in_dumptrace(logicmoo_util_bugger_catch:on_x_debug(user:fail))
% '_catch':on_x_debug(user:fail).
(0)$[system] '$c_call_prolog'.  no(clause)
(1)$[plevel] plevel:'$toplevel'.   %  toplevel.pl:515:
(2)$[plevel] plevel:'$runtoplevel'.   %  toplevel.pl:529:
(3)$[plevel] plevel:'$query_loop'.   %  toplevel.pl:574:
(5)*$[plevel] plevel:'$execute_goal2'(user:must(fail),[]).   %  toplevel.pl:811:
(6)$[plevel] plevel:residue_vars(user:must(fail),[]).   %  toplevel.pl:831:
(7)[_catch] '_catch':must(user:fail).   %  logicmoo_util_bugger_catch.pl:922:
(9)$[bugger] bugger:with_each(0,bugger:debugOnFailure0,'_catch':on_x_debug(user:fail)).   %  logicmoo_util_bugger.pl:1270:
(11)[_catch] '_catch':one_must(bugger:rtraceOnError('_catch':on_x_debug(user:fail)),bugger:debugCallWhy(failed(debugOnFailure0('_catch':on_x_debug(user:fail))),'_catch':on_x_debug(user:fail))).   %  logicmoo_util_bugger_catch.pl:840:
(14)[dumpst] dumptrace('_catch':on_x_debug(user:fail)).   %  logicmoo_util_dumpst.pl:241:
(15)[l_dmsg] l_dmsg:with_all_dmsg(dumpst:dumptrace('_catch':on_x_debug(user:fail),103)).   %  logicmoo_util_dmsg.pl:231:
(16)[rtions] rtions:w_tl(bugger:tl_always_show_dmsg,l_dmsg:w_tl(set_prolog_flag(opt_debug,true),w_tl(bugger:dmsg_match(show,_G38117),dumpst:dumptrace('_catch':on_x_debug(user:fail),103)))).   %  logicmoo_util_with_assertions.pl:70:
(17)[system] scc(rtions:asserta(bugger:tl_always_show_dmsg,<clause>(0x1768a40)),l_dmsg:w_tl(set_prolog_flag(opt_debug,true),w_tl(bugger:dmsg_match(show,_G38233),dumpst:dumptrace('_catch':on_x_debug(user:fail),103))),rtions:erase(<clause>(0x1768a40))).   %  init.pl:314:
(18)$[system] sccc(rtions:asserta(bugger:tl_always_show_dmsg,<clause>(0x1768a40)),l_dmsg:w_tl(set_prolog_flag(opt_debug,true),w_tl(bugger:dmsg_match(show,_G38337),dumpst:dumptrace('_catch':on_x_debug(user:fail),103))),_G38309,rtions:erase(<clause>(0x1768a40))).   %  init.pl:310:
(19)[rtions] rtions:w_tl(l_dmsg:set_prolog_flag(opt_debug,true),l_dmsg:w_tl(bugger:dmsg_match(show,_G38423),dumpst:dumptrace('_catch':on_x_debug(user:fail),103))).   %  logicmoo_util_with_assertions.pl:61:
(20)[system] scc(rtions:set_prolog_flag(opt_debug,true),rtions: @(w_tl(bugger:dmsg_match(show,_G38518),dumpst:dumptrace('_catch':on_x_debug(user:fail),103)),logicmoo_util_dmsg),rtions: (filter=unUSED->true;set_prolog_flag(opt_debug,filter))).   %  init.pl:314:
(21)$[system] sccc(rtions:set_prolog_flag(opt_debug,true),rtions: @(w_tl(bugger:dmsg_match(show,_G38604),dumpst:dumptrace('_catch':on_x_debug(user:fail),103)),logicmoo_util_dmsg),_G38582,rtions: (filter=unUSED->true;set_prolog_flag(opt_debug,filter))).   %  init.pl:310:
(22)$[rtions] rtions: @(w_tl(bugger:dmsg_match(show,_G38667),dumpst:dumptrace('_catch':on_x_debug(user:fail),103)),logicmoo_util_dmsg).  no(clause)
(23)[rtions] rtions:w_tl(bugger:dmsg_match(show,_G38739),dumpst:dumptrace('_catch':on_x_debug(user:fail),103)).   %  logicmoo_util_with_assertions.pl:70:
(24)[system] scc(rtions:asserta(bugger:dmsg_match(show,_G38813),<clause>(0x1768a80)),dumpst:dumptrace('_catch':on_x_debug(user:fail),103),rtions:erase(<clause>(0x1768a80))).   %  init.pl:314:
(25)$[system] sccc(rtions:asserta(bugger:dmsg_match(show,_G38875),<clause>(0x1768a80)),dumpst:dumptrace('_catch':on_x_debug(user:fail),103),_G38862,rtions:erase(<clause>(0x1768a80))).   %  init.pl:310:
(26)*[dumpst] dumptrace('_catch':on_x_debug(user:fail),103).   %  logicmoo_util_dumpst.pl:253:
(27)$[_catch] '_catch':hotrace(dumpst:dumpST(500000000)).   %  logicmoo_util_bugger_catch.pl:706:
(28)[dumpst] dumpST(500000000).   %  logicmoo_util_dumpst.pl:71:
(29)[_check] '_check':loop_check_early(dumpst:dumpST9(500000000),dumpst:dumpST0(500000000)).   %  logicmoo_util_loop_check.pl:181:
(30)[_check] '_check':loop_check_term_key(dumpst:dumpST9(500000000),dumpst:dumpST9(500000000),dumpst:dumpST0(500000000)).   %  logicmoo_util_loop_check.pl:189:
(31)[_check] '_check':loop_check_term(dumpst:dumpST9(500000000),dumpST9(500000000),dumpst:dumpST0(500000000)).   %  logicmoo_util_loop_check.pl:199:
(32)[system] scc('_check':asserta(mcache:ilc(dumpST9(500000000)),<clause>(0x1768ad0)),dumpst:dumpST9(500000000),'_check':erase(<clause>(0x1768ad0))).   %  init.pl:314:
(33)$[system] sccc('_check':asserta(mcache:ilc(dumpST9(500000000)),<clause>(0x1768ad0)),dumpst:dumpST9(500000000),_G39134,'_check':erase(<clause>(0x1768ad0))).   %  init.pl:310:
(34)[dumpst] dumpST9(500000000).   %  logicmoo_util_dumpst.pl:77:
in_dumptrace(logicmoo_util_bugger_catch:on_x_debug(user:fail))
% '_catch':on_x_debug(user:fail).

$    = frames normally hidden
*    = choice points
scc? = setup_call_cleanup
r    = run again unleashed

````


# 5) wt_l/2 With Thread Local

````
?- w_tl((doit:-dmsg("hi")),doit).
% "hi".
true.
````

Even changes and restores non locals like..

````
?- w_tl(prolog_flag(xref,true),...).

?- w_tl(op(0,xfx,'@'), consult(file_that_dont_like_op)).

````

# 6) Comment your output with in_cmt/1 macro

````
?- in_cmt(listing(in_cmt)).
% :- meta_predicate logicmoo_util_dmsg:in_cmt(0).
%
% logicmoo_util_dmsg:in_cmt(Call) :-
%       call_cleanup(prepend_each_line('% ', Call), format('~N', [])).
true.
````

# 7) Use Jan's newly add prolog streams 

````

?- with_output_to_pred(print_as_html_pre,
    (writeln("hi there"),writeln("how are you?"))).

<pre>hi there
</pre>
<pre>how are you?
</pre>


````
# (8)-(45) .....



# Part B - Predicate Calc

````
?- ensure_loaded(library(logicmoo/logicmoo_base)).
````


# (46) Defining new Forward chaining/backchaining rules..

````
spouse(X,Y) ==> spouse(Y,X).
gender(P,male) <==> male(P).
gender(P,female) <==> female(P).
````

# (47) Defining new Genral Inference rules..

````
parent(X,Y),female(X) <=> mother(X,Y).
parent(X,Y),parent(Y,Z) => grandparent(X,Z).
grandparent(X,Y),male(X) <=> grandfather(X,Y).
grandparent(X,Y),female(X) <=> grandmother(X,Y).
mother(Ma,Kid),parent(Kid,GrandKid)
      =>grandmother(Ma,GrandKid).
grandparent(X,Y),female(X) <=> grandmother(X,Y).
parent(X,Y),male(X) <=> father(X,Y).
mother(Ma,X),mother(Ma,Y),{X\==Y}
     =>sibling(X,Y).

````


# First Order Logic (FOL) declarations in Prolog source code. 


Despite Prolog's logic heritage it does not qualify as a full general-purpose theorem-proving system. There are three main reasons: (1) Prolog is a programming language and not an inference engine so if we used the unification algorithm of Prolog for FOL, it is unsound,  (2) Prolog's unbounded depth-first search strategy is inefficient when it is doing complete search, and (3) Prolog's inference system is not complete for non-Horn clauses. Nevertheless, Prolog is quite interesting from a theorem-proving standpoint because of its very high inference rate as compared to conventional theorem-proving programs. 

Logicmoo use of the Prolog Technology Theorem Prover (PTTP) was to overcome the deficiencies while retaining as fully as possible the high performance of well-engineered Prolog systems.

The Prolog Technology Theorem Prover (PTTP) is an extension of Prolog that is complete for the full first order predicate calculus (Stickel, 1988).   
It is invoked whenever the facts and rule are described in NNF or CNF put into the knowledge base.  And optionally for Horn clauses built by other modules.
However, when the rules are in Prenix Normal Form (PNF) (thus have quantifiers) they are converted to NNF, SNF and finally CNF and handed back over to PTTP.
Also a formula whose leading quantifier is existential, the formula obtained by removing that quantifier via Closed Skolemization may be generated. 

kif_add/1: the file has a rule or fact, in the form of a predicate of FOPC (First Order Predicate Calculus).  The LogicMOO invokes the PTTP compiler (discussed later) to assert the form to the knowledge base. The
knowledge base represents the user''s beliefs. Thus, asserting the logical form to the knowledge base amounts to applying the Declarative rule and the Distributivity rule (Axiom B2).

kif_ask/1: the user types in a question, in the form of a predicate of FOPC (First Order Predicate Calculus). The PTTP inference system is then invoked to attempt to  prove the predicate, 
using the axioms and facts of the knowledge base. This amounts toassuming that the user''s beliefs are closed under logical consequence, i.e., the Closure rule (Axiom B1) is implicitly applied over and over.

LogicMOO, because of PTTP, is unlike all other theorem provers today  (Perhaps except SNARK and maybe CYC) 
Here is how:: If the proof succeeds, LogicMOO answers ``yes'', and prints out the predicate, instantiating all variables. If there are multiple answers, then it prints all of them. 
If the proof fails, LogicMOO invokes PTTP to prove the negation of the queried predicate.  If that NEGATED proof succeeds, then LogicMOO answers ``no''; otherwise, LogicMOO answers ``cannot tell, not enough information''.

LogicMOO, therefore, has the capability for reasoning about negation, being able to distinguish between real negation (``P is false'') from negation by failure (``P is not provable'').
This allows the system to distinguish beliefs that are provably false from beliefs that cannot be proven because of insufficient information. 
This is an important feature that overcomes the supposed limitations of Prolog.   For example, without this added capability, if a user were to
ask whether LogicMOO believes that John intended to let the cat out, then LogicMOO would answer ``no''. 
This answer is misleading because LogicMOO would also answer ``no'' if it were asked if John did not intend to let the cat out.  This is why the system automaically Re-asks the negation.

Sadly all theorem provers since PTTP (include theorem provers said to be based on it) have been simplified to absurdium and cannot do this simple analysis.  The reason? According to classically trained
logicians horn clauses *cannot* start with a negated literals.   So to not offend them (entirely)  PTTP can store "( ~a :- ~b )" as "( not_a :- not_b )" 
If we obeyed the classical limitations set forth upon Horn clauses to only being "positive" that would remove the unique ability for LogicMOO to deduce the difference between false and unknown. 
We are no longer restricted to CWA and the limitations imposed by modern theorem provers and sematic web tools.  I must assume the reason programmers made these sacrifices is they can still solve problems like circuit verifcation without disrupting the post 1980s maintsteam thinking.

Since LogicMOO can infer the limits it's theoretical knowledge, so it can help guide the user to understand what types of knowledge it is missing.  That also provides a portal through which
other modules (e.g., a plan recognition system or a modules for NL reference resolution) can enter information. When such modules are not available, the user may simulate this capacity. ("ask the user")

is_entailed/1: Detects if an Horn Clause (or fact) is holograpically existing. Example: assert a=>b. this will result in the following two clauses:   is_entailed(( ~a :- ~b )) and   is_entailed((  b :- a ) ).


# Backward-Chaining Rules
## PFC
Pfc includes a special kind of backward chaining rule which is used to generate all possible solutions to a goal that is sought in the process of forward chaining.     


# Forward chaining macros create smaller self-maintaining codebase.

Prolog, like most logic programming languages, offers depth first backward chaining as the only reasoning scheme. It is well known that sound and complete reasoning systems can be built using either exclusive backward chaining or exclusive forward chaining. Thus, this is not a theoretical problem. It is also well understood how to ``implement'' forward reasoning using an exclusively backward chaining system and vice versa. Thus, this need not be a practical problem. In fact, many of the logic-based languages developed for AI applications allow one to build systems with both forward and backward chaining rules.
There are, however, some interesting and important issues which need to be addresses in order to provide the Prolog programmer with a practical, efficient, and well integrated facility for forward chaining.

This module uses such a facility, written by Tim Finin called PFC, which he has implemented in standard Prolog. The PFC system is a package that provides a forward reasoning capability to be used together with conventional Prolog programs. The PFC inference rules are Prolog terms which are asserted as facts into the regular Prolog database.

The PFC system package provides a forward reasoning capability to be used together with conventional Prolog programs. The PFC inference rules are Prolog terms which are asserted as clauses into the regular Prolog database. When new facts or forward reasoning rules are added to the Prolog database (via a special predicate pfc_add/1, forward reasoning is triggered and additional facts that can be deduced via the application of the forward chaining rules are also added to the database. A simple justification-based truth-maintenance system is provided as well as simple predicates to explore the resulting proof trees.   Additionally this module provides the user with various methods for trying out new techniques of backwards chaining without rewriting their code.



The =logicmoo_base= module allows one to use optimal First Order Logic declarations in Prolog code.
During *development*, these declarations log informative information when values don't match
expectations.  A typical development environment converts this into a helpful
stack trace which assists in locating the programing error.

FOL declarations can be give manually by calling pfc_assert/1.  `logicmoo_base` also inserts
predicate Mode declarations for you based on your PlDoc structured comments.  

## Inference Engines can:

  * offer documentation to those reading our code
  * help find errors during development
  * structure our thinking during development
  * provide a place to persist content tests 


````

% this means that both P and Q can't be true.
disjoint(P,Q), {current_predciate(_,P),current_predciate(_,Q)}
  ==>
  (P ==> not(Q)),
  (Q ==> not(P)).

````

You may have noticed that Logicmoo defines {}/1 as a escape construct for bypassing the FOL's salient body goals. 


As exemplified above, this is the same control construct that is used in grammar rules for bypassing the expansion of rule body goals when a rule is converted into a clause. 
Both control constructs can be combined in order to call a goal from a grammar rule body, while bypassing at the same time the Prolog compiler. Consider the following example:





# Runtime Loop checking.  @loop_check(+:Goal)

````
````


# Lexical @with_assertions(+:ThreadLocal,+:Goal).


````
````


# Online Prolog current state cross referencer.  
````
?- start_logicmoo_www(11111).

````

# S-Expr reader utilities

The abiliity to use CLIF/KIF/CycL Etc


# External predicate proxies 

````
````


# Changes in this Version

  * Fix packaging error

# Installation

Using SWI-Prolog 6.3.16 or later:

    $ swipl
    1 ?- pack_install(logicmoo_base).

Source code available and pull requests accepted on GitHub:
https://github.com/TeamSPoon/PrologMUD/tree/master/pack/logicmoo_base
