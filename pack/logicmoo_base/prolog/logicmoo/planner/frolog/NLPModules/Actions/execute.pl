/*************************************************************************

    File: execute.pl
    Copyright (C) 2007 

    Programmer: Luciana Benotti

    This file is part of Frolog, version 0.1 (October 2007).

    Frolog is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Frolog is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Frolog; if not, write to the Free Software Foundation, Inc., 
    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*************************************************************************/

:- module(execute,[k/1,notk/1,pk/1,del/1,add/1,executeCommand/4]).   

:- dynamic
      failed/1, failed/2, current_clone/2.  

:- use_module(library(lists)).
:- use_module('KBInterfaces/RacerInterface/racer',
	[individual_instance/4,
	 individuals_related/5,
	 forget_concept_assertion/4,
	 forget_role_assertion/5,
	 add_concept_assertion/4,
	 add_role_assertion/5,
	 worldKB/2, 
	 playerKB/2,
	 clone_abox/3, 
	 delete_abox/2,
	 concept_instances/4,   %(+Link,+ABox,+Concept,-Individuals)
	 individual_fillers/5,   %(+Link,+ABox,+Individual,+Role,-Individuals) 
	 individual_fillers_inv/5,   %(+Link,+ABox,+Individual,+Role,-Individuals) 
	 log/1
	]).

% TODO: This import should be dynamic, according to the selected scenario.
:- use_module('GameScenarios/FairyTaleCastle/actionDatabase',[
			  take/3,
			  eat/2,
			  drop/3,
			  throw/3,
			  put/3,
			  kiss/2,
			  kill/3,
			  look/2,
			  standup/3,
			  move/4,
			  unlock/3,
			  open/2,
			  lock/3,
			  shut/2,
			  wear/2
			  ]).
:- use_module('Tools/handleExceptions',[handle/2]).  

/**************************************************************************
   Meta-interpreter that implements the core of the actions module. 

*************************************************************************/

/*========================================================================
executeCommand(+Command,-Executed)
 Tries to execute all the readings, then filters all readings and
 keeps the ones that could be executed, finally it evaluates whether the
 command is executable and not ambiguos
========================================================================*/

executeCommand(Command,Executed,Failed,Decision) :-
	log(Log),format(Log,'------------------------Command: ~w ~n',[Command]),
	maplistflat(lookupConjunction,Command,Disjunction),
	maplist(executeConjunction,Disjunction,Execution), %SWI Prolog built-in
	evaluate(Execution,Executed,Failed,Decision).


/*========================================================================
maplistflat(+Pred, ?List1, ?List2)
 The difference with maplist/3 is that
 the elements in List2 are appended in order to obtain one big list. 
========================================================================*/

maplistflat(_, [], []).
maplistflat(Goal, [Elem1|Tail1],List2) :-
        apply(Goal, [Elem1, Elem2]),
        maplistflat(Goal, Tail1, Tail2),
	append(Elem2,Tail2,List2).

/*========================================================================
NestedConjunction is a conjunction of disjunctions of entries. Where
   the inner most list corresponds to ambiguities in the actions
   database (several actions with the same name; they should be
   disambiguated by their preconditions). Multiply these ambiguities
   out so that we again have a conjunction of entries.
========================================================================*/

lookupConjunction(Conjunction,Product) :-
	maplist(lookupAction,Conjunction,NestedConjunction), %SWI Prolog built-in
	cross_product(NestedConjunction,Product).

/*========================================================================
Get all actions definitions from the action database. If
   one action name corresponds to several definitions in the action
   database, all of these are returned in ActionMeanings.
========================================================================*/

lookupAction(ActionName,ActionMeanings) :-
	findall(action(ActionName,body(ActionMeaning)),
                actionDatabase:clause(ActionName,ActionMeaning),ActionMeanings).

/*========================================================================
cross_product(+LList,-List) 
  List is the cartesian
  product of the lists in LList, that is, the list of
  lists formed with one element of each list in LList, in
  the same order.
========================================================================*/

cross_product([], [[]]).
cross_product([L1|Ls], Lds) :-
        cross_product(Ls, Lda),
        add_each_elem(L1, Lda, Lds).

add_each_elem([], _, []).
add_each_elem([X|Xs], Lda, Lds) :-
        add_elem(Lda, X, Lds, Lds_),
        add_each_elem(Xs, Lda, Lds_).

add_elem([], _X, Lds, Lds).
add_elem([L|Ls], X, [[X|L]|XLs], Lds_) :-
        add_elem(Ls, X, XLs, Lds_).


/*========================================================================
 Retrieves the Game ABox and clones it. Asserts this clone as the 
 current clone (so it can be retrieved by the add and del predicates).
 Tries to execute the input reading on the clone. Before finishing, 
 retracts the current clone. 
========================================================================*/

executeConjunction(Conjunction,reading(Conjunction,CloneABox,Result)) :-
	racer: worldKB(Link,GameABox),
	racer: clone_abox(Link,GameABox,CloneABox),
	assert(current_clone(Link,CloneABox)),
	catch((maplist(executeAction,Conjunction),Result = executed),nil(ActionName,Precondition),failedReading(ActionName,Precondition,Result)),
	retract(current_clone(Link,CloneABox)).

failedReading(ActionName,Precondition,failed(ActionName,Precondition)). 
% TODO: When two readings are already executable, it's not necessary to 
% continue checking the rest of the readings because the command will be
% considered ambiguos anyway. Now, we are checking all the readings.  

/*========================================================================
 Executes an action of the form take(myself,apple1,chest1) if its
 preconditions hold. Otherwise it asserts the action and the failed
 precondition and the predicate fails. 
=========================================================================*/

executeAction(action(ActionName,body(ActionMeaning))) :- 
	catch(ActionMeaning,nil(Precondition),failedAction(ActionName,Precondition)). 
		       %This is a meta-call. ActionMeaning is conjunction of 
		       %the predicates that constitute the definition of the 
		       %action _ActionName

failedAction(ActionName,Precondition) :- throw(nil(ActionName,Precondition)).

/*========================================================================
 Fails if no reading is executable.
 Succeeds if only one reading is executable, and commits to the 
 corresponding clone. 
 Fails if the command is ambiguous, i.e. if more than one reading is 
 executable. Then delete all the cloned ABoxes. 
=========================================================================*/

evaluate(Execution,Executed,Failed,Decision) :-
	sublist(executable,Execution,Executed),            %SWI Prolog built-in
	sublist(failing,Execution,Failed),                  %SWI Prolog built-in
	evaluateExecution(Executed,Failed,Decision).

evaluateExecution([],Failed,failed) :- % No reading is executable
	maplist(deleteClones,Failed).
evaluateExecution([reading(_Reading,Clone,executed)],_Failed,executed) :- % One is executable 
	racer:retract(worldKB(Link,_)),
	racer:assert(worldKB(Link,Clone)). 
evaluateExecution(Executed,Failed,ambiguous) :-  % Many reaadings are executable
	maplist(deleteClones,Executed),
	maplist(deleteClones,Failed). 

executable(reading(_,_,executed)). 
failing(reading(_,_,failed(_,_))).

deleteClones(reading(_,CloneABox,_)) :-
	racer: worldKB(Link,_),
	racer: delete_abox(Link,CloneABox).

 

/*========================================================================
   Functions for checking if an assertion holds in the world KB
   k(X) verifies if the assertion X holds
   notk(X) verifies if the assertion not(X) holds
   If the assertion does not hold, the assertion X is registered as 
   failed_precond(X), and the function fails. 

========================================================================*/

k(X) :- ground(X),
	current_clone(Link,ABox),
	(X =.. [Concept,Individual] -> racer: individual_instance(Link,ABox,Individual,Concept),!;
	X =.. [Role,Individual1,Individual2] -> racer: individuals_related(Link,ABox,Individual1,Individual2,Role),!).

% The following makes many simplifying assumptions:
% 1. The concept queried will contain only one individual
% 2. The not ground element of the role assertion is the Individual2 -- I think I dropped this assumption 
% 3. The Individual1 is related by the Role to only one Individual2
% I'd better respect this assumptions in the design of the actions schemas and KBs 
k(X) :- not(ground(X)),
	    X =.. [Concept,Individual],
	    current_clone(Link,ABox),
	    racer: concept_instances(Link,ABox,Concept,Individualaux), 
	    Individualaux = [Individual],!.
k(X) :- not(ground(X)),
        X =.. [Role,Individual1,Individual2],
	    not(ground(Individual1)), 
	    current_clone(Link,ABox),
	    racer: individual_fillers_inv(Link,ABox,Individual2,Role,Individual), 
	    Individual = [Individual1],!.
k(X) :- not(ground(X)),
        X =.. [Role,Individual1,Individual2],
	    not(ground(Individual2)), 
	    current_clone(Link,ABox),
	    racer: individual_fillers(Link,ABox,Individual1,Role,Individual), 
	    Individual = [Individual2],!.
k(X) :- throw(nil(k(X))).

pk(X) :- ground(X), k(X).
pk(X) :- not(ground(X)),
	    X =.. [Concept,Individual],
	    playerKB(Link,ABox),
	    racer: concept_instances(Link,ABox,Concept,Individualaux), 
	    Individualaux = [Individual],!.
pk(X) :- not(ground(X)),
        X =.. [Role,Individual1,Individual2],
	    not(ground(Individual1)), 
	    playerKB(Link,ABox),
	    racer: individual_fillers_inv(Link,ABox,Individual2,Role,Individual), 
	    Individual = [Individual1],!.
pk(X) :- not(ground(X)),
        X =.. [Role,Individual1,Individual2],
	    not(ground(Individual2)), 
	    playerKB(Link,ABox),
	    racer: individual_fillers(Link,ABox,Individual1,Role,Individual), 
	    Individual = [Individual2],!.
pk(X) :- throw(nil(k(X))).
	 

notk(X) :- ground(X),
	current_clone(Link,ABox),
	(X =.. [Concept,Individual] -> not(racer: individual_instance(Link,ABox,Individual,Concept)),!;
	X =.. [Role,Individual1,Individual2] -> not(racer: individuals_related(Link,ABox,Individual1,Individual2,Role)),!).
notk(X) :- throw(nil(notk(X))).


/*========================================================================
   Functions for asserting and retracting from the world KB
   del(X) retracts X from the world KB
   add(X) asserts X in the world KB

========================================================================*/


del(X) :- current_clone(Link,ABox),
	      X =.. [Concept,Individual],
	      racer: forget_concept_assertion(Link,ABox,Individual,Concept), 
	      not(racer: individual_instance(Link,ABox,Individual,Concept)).

del(X) :- current_clone(Link,ABox),
	      X =.. [Role,Individual1,Individual2],
	      racer: forget_role_assertion(Link,ABox,Individual1,Individual2,Role),
	      not(racer: individuals_related(Link,ABox,Individual1,Individual2,Role)).

add(describe(X)).
add(describe(X,Y)).

add(X) :- current_clone(Link,ABox),
	      X =.. [Concept,Individual], 
	      racer: add_concept_assertion(Link,ABox,Individual,Concept), 
	      racer: individual_instance(Link,ABox,Individual,Concept).
	 
add(X) :- current_clone(Link,ABox),
	      X =.. [Role,Individual1,Individual2],
	      racer: add_role_assertion(Link,ABox,Individual1,Individual2,Role), 
	      racer: individuals_related(Link,ABox,Individual1,Individual2,Role).


/*************************************************************************
    
    Input: a list of lists of action names. E.g.:

      [[take(patient:[priesemut])]] (for the command "take the green frog")

      [[drop(patient:[priesemut])
        take(patient:[priesemut2])]]
                          (for the command "drop it and take the brown frog)

    So, the outer list collects different "interpretations" of the
    command. If the command had referential or syntactic ambiguities
    which could not be resolved by the module for referent
    resolution. "Interpretations" are again lists, representing
    sequences of actions. 

    This input is the output of the interpretation modules (i.e., parsing
    and resolution of referring expressions).


    Output: A list of readings corresponding to the different
    interpretations that came in. Each interpretation is a sequence of
    actions. The output shows which of these actions can be executed,
    which of these actions fail (because its preconditions are not
    satisfied), and which of these actions were not tested (as soon as
    the first failing action is found the rest of the sequence of
    actions is not evaluated any more).

    This is what the output looks like for an interpretatation that
    consists of only one action which can be executed.

    [reading(executable:[conjunct(action:NAME
                                  failPre:FAILED_PRECONDITIONS
                                  succPre:SUCCESSFULL_PRECONDITIONS
                                  untestedPre:UNTESTED_PRECONDITIONS
                                  uk:UPDATES_ON_PLAYER_KNOWLEDGE)]
             failed:nil
             untested:nil)]

    This output is then passed on to the content determination module.


    Function: 

    Input is a list of lists of actions, or as they call it a
    disjunction of conjunctions of actions. The idea behind this
    naming is that the outer list corresponds to alternative
    interpretations while the inner list of actions is a sequence of
    actions that all have to be carried out.

    1) Np-conjunctions are being expanded. See preprocessing.

    2) Get all actions from the action database. If
   one action name corresponds to several entries in the action
   database all of these are returned as a list.

    3) So, what we have then is a list of lists of lists of entries. Where
   the inner most list corresponds to ambiguities in the actions
   database (several actions with the same name; they should be
   disambiguated by their preconditions). Multiply these ambiguities
   out so that we again have a list of lists of entries.

    4) For each disjunct (element of the outer most list), test whether it
   can be executed in a clone of the current game A-Box. This is done
   by function executableDisjunct.

    5) The result is a list of alternative action sequences annotated for
   whether they are executable or not.

    6) If there is only one executable sequence of actions, execute it.
   If there is no executable sequence of actions, raise an error. The
   generation module will generate an error message. If there are
   several executable sequences, pass the information on to the
   content determination. (THIS is actually a BUG. An error should be
   raised in this last case as well.)


    Technical: 

    Wishlist: 

/* If the command is not ground, such as 'take the apple' = [take(myself,apple1,Source)], 
the command can be grounded using the preconditions of the action, such as 
k(haslocation(apple1,Source)). The location of the apple is found querying RACER with:
individual_fillers(Link,ABox,Concept,apple1,haslocation,Individual2) and the 
preconditions suceeds if the apple1 is located somewhere. I think that this query
should be made wrt the player KB but I will do it using the current clone of the game KB 
for now. Because, if we use the player KB then we need also to clone the player KB 
for each Conjunction. Also, we need to consider the case in which the apple 
has no location. Moreover, when shall frolog commit to the updated player KB?*/

% I need negated roles, can I do that with Racer? NO, I have to do it on top of RACER:
% Do the cross product of all individuals and eliminate those  in the relation, 
% I should consider the domain of the relation, too much ambiguity! Don't want to 
% deal with that right now.
notk(X) :- not(ground(X)),
    current_clone(Link,ABox),
	(X =.. [Concept,Individual] -> swritef(NegatedConcept,'(not ~w)',[Concept]), concept_instances(Link,PlayerABox,NegatedConcept,Individual),!;
	X =.. [Role,Individual1,Individual2] -> individual_fillers(Link,PlayerABox,Concept,Individual1,Role,Individual2),!).

*************************************************************************/
