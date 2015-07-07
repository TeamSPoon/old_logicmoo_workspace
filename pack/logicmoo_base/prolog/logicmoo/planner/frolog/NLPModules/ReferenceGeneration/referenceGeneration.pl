/*************************************************************************

    File: referenceGeneration.pl
    Copyright (C) 2007 

    Programmer: Laura Perez

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
    59 Temple PlingleSet([I|[]]).ace, Suite 330, Boston, MA  02111-1307  USA

*************************************************************************/

:- module(referenceGeneration,[generateReferringExpressions/3, %generateReferringExpressions(+Individual,+PreferedOrdering,-ReferringExpression)
							   filterConcepts/2  %filterConcepts(+BaseConcepts,-Concepts)  
]).

/*
    Function: This module implements Frolog's reference generation. Assembling the semantic content for realization.

    Technical: .

    Wishlist: .

TODO:
1- Distractor set build from the PlayerABox and with the visible objects. 
*/

:- dynamic
        preferedPropertyOrder/1.

:- use_module('KBInterfaces/RacerInterface/racer',
	[worldKB/2,
	playerKB/2,
	all_individuals/3,
	concept_instances/4,
	individual_direct_types/4,
	all_role_assertions_for_individual_in_domain/4,
	individual_instance/4,
	individual/3,
	individuals_related/5,
	individual_fillers/5,
	concept_subsumes/3
	]).

generateReferringExpressions(myself,_,[you(myself)]).

/* generateReferringExpressions(+Individual,+PreferedOrdering,-ReferringExpression).
Based on whether the individual Individual is known in the player Abox this predicate will generate a definite or indefinite description. Newness is determined by a query to Racer checking whether the individual is mentioned in the player PalyerAbox or not. If the individual is known to the player, a Definite Description. On the other hand, if it is not in the player knowledge, it will generate an Indefinit Description. 
The input parameter +PreferedOrdering is asserted because it will be used in each time a related object should be described.
*/
generateReferringExpressions(Individual,PreferedOrdering,ReferringExpression):-
			racer:playerKB(Link,PlayerABoxName),
			racer:individual(Link,PlayerABoxName,Individual),!, %ABox queries
			assert(preferedPropertyOrder(PreferedOrdering)),
			makeDefiniteDescription(Individual,PreferedOrdering,ReferringExpression).
generateReferringExpressions(Individual,PreferedOrdering,ReferringExpression):-
			makeIndefiniteDescription(Individual,PreferedOrdering,ReferringExpression).

/* makeDefiniteDescription(+Target,+PreferedOrdering,-DescriptionList).
This predicate implements the Dale & Reiter's base incremental algorithm, for generating define descriptions. It starts from an empty description and incrementally adds properties to it until all distractors are ruled out. If a property is chosen which is not unary but involves other objects then the description must be extended until it uniquely identifies these other objects as well.
The intended input for Target is the target entity t to be described.
The intended output for DescriptionList is a set of facts that uniquely identifies t (the distinguishing description). It fails in case such a description cannot be built.
*/
makeDefiniteDescription(Target,PreferedOrdering,BaseTypeDescription):-
			getBaseType(Target,[BaseType]),%the ontology is assumed to have only one base type per individual.
			racer:playerKB(Link,PlayerABoxName),
			racer:concept_instances(Link,PlayerABoxName,BaseType,Distractors), %ABox retrieval
			singleSet(Distractors),!,
			BTDescription=..[BaseType,Target],
			Def=..[def,Target],
			append([BTDescription],[Def],BaseTypeDescription).
makeDefiniteDescription(Target,PreferedOrdering,DistinguishingList):-
			getBaseType(Target,[BaseType]),
			racer:playerKB(Link,PlayerABoxName),
			racer:concept_instances(Link,PlayerABoxName,BaseType,Distractors), %ABox retrival
			racer:individual_direct_types(Link,PlayerABoxName,Target,DirectConcepts),%ABox retrieval
			filterConcepts(DirectConcepts,Concepts),
			racer:all_role_assertions_for_individual_in_domain(Link,PlayerABoxName,Target,DirectRoles),%Abox retrieval
			filterRoles(DirectRoles,Roles),
			append(Concepts,Roles,Properties),
			AssertionBaseType=..[BaseType,Target],
			Def=..[def,Target],
			chooseProperties(PreferedOrdering,Target,Distractors,Properties,[AssertionBaseType,Def],DistinguishingList).

/* makeIndefiniteDescription(+Target,+PreferedOrdering,-DescriptionList).
In this case we are referring to objects that are new to the player. So, and indefinit NP containing the type and, if this has one, the color will be generated.
*/
makeIndefiniteDescription(Individual,PreferedOrdering,DescriptionList):-
			racer:worldKB(Link,GameABoxName),
			racer:individual_direct_types(Link,GameABoxName,Individual,DirectConcepts),%ABox retrieval
			directTypeColorConcepts(Individual,DirectConcepts,DescriptionList).


/* chooseProperties(+PreferedList,+Target,+Distractors,+Properties,+L,-DescriptionList).
According to the PreferedList (the prefered order of properties evaluation) the properties from the Properties list are evaluated. This predicate implements part of the select applicable property step from the Dale&Reiter incremental algorithm, collaborating with the preferredApplicableProperties and checkRulesOut predicates.
Requiriments to be an applicable property p:
- p mentions an object that is already part of the description. (All properties in +Properties are the properties for the Target object.)
- p is new to the description.(yes because they are taken according to the PreferedList.)
- adding p reduces the distractor set of at least one object mentioned in the description. (This is checked with the checkRulesOut predicate).
*/
chooseProperties(PreferedList,Target,[Target],Propertries,L,L).
chooseProperties([],Target,Distractors,Propertries,L,[]):-!. %throw exception, in this case no def description was found.
chooseProperties([P|PreferedList],Target,Distractors,Properties,L,DescriptionList):-
			preferredApplicableProperties(P,Properties,ApplicableProperties,RestProperties),
			checkRulesOut(ApplicableProperties,Target,Distractors,DistinguishingList,NewDistractorSet),
			chooseProperties(PreferedList,Target,NewDistractorSet,RestProperties,L,NewDistinguishingDesc),
			append(DistinguishingList,NewDistinguishingDesc,DescriptionList).

/* checkRulesOut(+Properties,+Target,+Distractors,-DescriptionList,-NewDistractorSet).
In the distractor set of an object o there are all the objects that fit the description of o given by L, a list of properties. Each of the properties in the list +SelectedProperties are checked one by one, in order to see which objects in the distractor set +Distractors fit them. If there is an object of the set +Distractors that does no fit a property P of +SelectedProperties then this property P is added to the description list -DescriptionList because it means that the property rules out an object.
A query to RACER is used to verify which are the objects that fit the description given by a selected property. 
This predicate is defined for the case that property P is unary (concept) as well as for the case that P is binary (role). In the case that P is concept the predicate possibleSubstitutions is used in order to try all the possible substitutions o' for the target object o. In the case of P being a role, it is neccessary to try the possible substitutions of o by o' and the possible substitutions for role related objects, so the predicate possibleRoleSubstitutions is called. Both predicates aim to build a new distracors set, to check whether adding property P to the description list reduces it.
*/
checkRulesOut(SelectedProperties,Target,[Target],[],[]). %recursion base case. The target referent was identify.
checkRulesOut([],Target,Distractors,[],Distractors). %recursion base case. The target referent was not identify.
checkRulesOut([P|SelectedProperties],Target,Distractors,NewDescriptionList,NewDistractorSet):-
			%cheking for a role property that rules out.
			P=..[_,_,RelObject],
			once(getRelObjectDistractors(Distractors,P,RelObjDistractors)),
			once(describeRelObject(RelObject,RelObjDistractors,RelObjectDescription,RestRelDistractors)),
			once(possibleRoleSubstitutions(Distractors,RestRelDistractors,P,NewDistractorSet)),
			different(Distractors,NewDistractorSet),
			checkRulesOut(SelectedProperties,Target,NewDistractorSet,DescriptionList,NewDistractorSet),
			append([P|DescriptionList],RelObjectDescription,NewDescriptionList).
checkRulesOut([P|SelectedProperties],Target,Distractors,DescriptionList,NewDistractorSet):-
			%cheking for a role property that does not rule out.
			P=..[_,_,_],!,
			checkRulesOut(SelectedProperties,Target,Distractors,DescriptionList,NewDistractorSet).
checkRulesOut([P|SelectedProperties],Target,Distractors,[PAss|DescriptionList],NewDistractorSet):-
			%cheking for a concept property that rules out.
			once(PAss=.. [P,Target]),
			once(possibleSubstitutions(P,Distractors,NewDistractorSet)),
			different(Distractors,NewDistractorSet),
			checkRulesOut(SelectedProperties,Target,NewDistractorSet,DescriptionList,NewDistractorSet).
checkRulesOut([P|SelectedProperties],Target,Distractors,DescriptionList,NewDistractorSet):-
			%cheking for a role property that does not rule out.
			checkRulesOut(SelectedProperties,Target,Distractors,DescriptionList,NewDistractorSet).

/*possibleSubstitutions(+P,+Distractors,-NewDistractors).
Which are the objects o' that may also fit the concept property P for describing o. The resulting set is the distractors of o given by adding P. Checks whether adding P reduces the distractors set of at least one object, that is, if there is no possible substitution of concept property P and objects in the distractor set that is in the knowledge base.*/
possibleSubstitutions(P,[],[]).
possibleSubstitutions(P,[D|Distractors],[D|NewDistractors]):-
			assertionToConcept(P,Concept),
			racer:playerKB(Link,PlayerABoxName),
			racer:individual_instance(Link,PlayerABoxName,D,Concept),%ABox queries
			possibleSubstitutions(P,Distractors,NewDistractors).
possibleSubstitutions(P,[D|Distractors], NewDistractors):-
			possibleSubstitutions(P,Distractors,NewDistractors).

/*possibleRoleSubstitutions(+Distractors,+RestRelDistractors,+P,NewDistractorSet).
Which are the objects o' that may also fit P, the role property, and d or d', the objects directly related via P, for describing o. Checks whether adding P reduces the distractors set of at least one object, that is, if there is no possible substitution of role property P and objects in the distractor set that is in the knowledge base.
*/
possibleRoleSubstitutions([],RestRelDistractors,P,[]).
possibleRoleSubstitutions([D|Distractors],RestRelDistractors,P,[D|NewDistractorSet]):-
			someRelatedTo(D,P,RestRelDistractors),
			possibleRoleSubstitutions(Distractors,RestRelDistractors,P,NewDistractorSet).
possibleRoleSubstitutions([D|Distractors],RestRelDistractors,P,NewDistractorSet):-
			possibleRoleSubstitutions(Distractors,RestRelDistractors,P,NewDistractorSet).

/*someRelatedTo(+D,+Role,+RestRelDistractors).*/
someRelatedTo(D,Role,[RelObject|RestRelDistractors]):-
			Role=..[R|_],
			racer:playerKB(Link,PlayerABoxName),
			racer:individuals_related(Link,PlayerABoxName,D,RelObject,R),!. %ABox queries
someRelatedTo(D,Role,[_|RestRelDistractors]):-
			someRelatedTo(D,Role,RestRelDistractors).

/* getRelObjectDistractors(+Distractors,+P,-RelDistractors).
This predicate obtains all the individuals that are in relation with objects in the distractor set +Distractors through role +P. The set RelDistractors is the distractor set of the related object of the target object.*/
getRelObjectDistractors([],P,[]).
getRelObjectDistractors([D|Distractors],P,RelDistractors):-
			%in this case there are NO Individual fillers to append.
			P=..[Role|_],
			racer:playerKB(Link,PlayerABoxName),
			racer:individual_fillers(Link,PlayerABoxName,D,Role,Individuals),%ABox retrieval
			member(nil,Individuals),!,
			getRelObjectDistractors(Distractors,P,RelDistractors).
getRelObjectDistractors([D|Distractors],P,NewRelDistractors):-
			%in this case there are Individual fillers to append.
			P=..[Role|_],
			racer:playerKB(Link,PlayerABoxName),
			racer:individual_fillers(Link,PlayerABoxName,D,Role,Individuals),!,%ABox retrieval
			getRelObjectDistractors(Distractors,P,RelDistractors),
			append(RelDistractors,Individuals,NewRelDistractors).


/*describeRelObject(+RelObject,+RelObjDistractors,-RelObjectDescription,-RestRelDistractors).*/
describeRelObject(RelObject,RelObjDistractors,RelObjectDescription,BaseTypeRelObjDistractors):-
			getBaseType(RelObject,[BaseType]),
			racer:playerKB(Link,PlayerABoxName),
			racer:concept_instances(Link,PlayerABoxName,BaseType,BaseObjDistractors),%ABox retrieval
			getBaseTypeRelObjDistractors(BaseObjDistractors,RelObjDistractors,BaseTypeRelObjDistractors),
			singleSet(BaseTypeRelObjDistractors),!,
			Base=..[BaseType,RelObject], Def=..[def,RelObject],
			append([Base],[Def],RelObjectDescription).
describeRelObject(RelObject,RelObjDistractors,RelObjectDescription,NewDistractorSet):-
			getBaseType(RelObject,[BaseType]),
			racer:playerKB(Link,PlayerABoxName),
			racer:concept_instances(Link,PlayerABoxName,BaseType,BaseObjDistractors),%ABox retrieval
			getBaseTypeRelObjDistractors(BaseObjDistractors,RelObjDistractors,BaseTypeRelObjDistractors),
			racer:playerKB(Link,PlayerABoxName),
			racer:concept_instances(Link,PlayerABoxName,BaseType,Distractors),%ABox retrieval
			racer:individual_direct_types(Link,PlayerABoxName,Target,DirectConcepts),%ABox retrieval
			filterConcepts(DirectConcepts,Concepts),
			racer:all_role_assertions_for_individual_in_domain(Link,PlayerABoxName,Target,DirectRoles),%ABox retrieval
			filterRoles(DirectRoles,Roles),
			append(Concepts,Roles,Properties),
			AssertionBaseType=..[BaseType,Target],
			preferedPropertyOrder(PreferedOrdering),
			chooseProperties(PreferedOrdering,Target,Distractors,Properties,[AssertionBaseType],DistinguishingList),
			Def=..[def,RelObject],
			append([Def],DistinguishingList,RelObjectDescription).

/*getBaseTypeRelObjDistractors(BaseObjDistractors,RelObjDistractors,BaseTypeRelObjDistractors).*/
getBaseTypeRelObjDistractors([],RelObjDistractors,[]).
getBaseTypeRelObjDistractors([B|BaseObjDistractors],RelObjDistractors,[B|BaseTypeRelObjDistractors]):-
			member(B,RelObjDistractors),
			getBaseTypeRelObjDistractors(BaseObjDistractors,RelObjDistractors,BaseTypeRelObjDistractors).
getBaseTypeRelObjDistractors([B|BaseObjDistractors],RelObjDistractors,BaseTypeRelObjDistractors):-
			getBaseTypeRelObjDistractors(BaseObjDistractors,RelObjDistractors,BaseTypeRelObjDistractors).


/*getBaseType(+Target,-Concepts)*/
getBaseType(Target,Concepts):-
			racer:playerKB(Link,PlayerABoxName),
			racer:individual_direct_types(Link,PlayerABoxName,Target,AllConcepts),%ABox retrieval
			filterBaseConcepts(AllConcepts,Concepts).

/*filterBaseConcepts(+AllConcepts,-BaseConcepts).*/
filterBaseConcepts([],[]).
filterBaseConcepts([C|AllConcepts],BaseConcepts):-
			racer:worldKB(Link,_),
			not(racer:concept_subsumes(Link,C,'special')),%Queries for Concept Terms
			racer:concept_subsumes(Link,'(or object room exit)',C),%Queries for Concept Terms
			filterBaseConcepts(AllConcepts,NewBaseConcepts),
			append([C],NewBaseConcepts,BaseConcepts).
filterBaseConcepts([C|AllConcepts],BaseConcepts):-
			filterBaseConcepts(AllConcepts,BaseConcepts).

/*filterConcepts(+BaseConcepts,-Concepts).*/
filterConcepts([],[]).
filterConcepts([C|BaseConcepts],Concepts):-
			assertionToConcept(C,CDesc),
			racer:worldKB(Link,_),
			racer:concept_subsumes(Link,CDesc,'special'),%Queries for Concept Terms
			filterConcepts(BaseConcepts,Concepts).
filterConcepts([C|BaseConcepts],[C|Concepts]):-
			filterConcepts(BaseConcepts,Concepts).

/*filterRoles(+DirectRoles,-Roles).*/
filterRoles([],[]).
filterRoles([R|DirectRoles],Roles):-
			assertionToConcept(R,RDesc),
			member(RDesc,['fitsin','hascounterpart']),
			filterRoles(DirectRoles,Roles).
filterRoles([R|DirectRoles],[R|Roles]):-
			filterRoles(DirectRoles,Roles).

/* preferredApplicableProperties(+P,+Properties,-ApplicableProperties,-RestProperties).
Extracts from the Properties list only the properties that are subsumed by concept P in the ontology. The result is a list with all these properties.
*/
preferredApplicableProperties(P,[],[],[]).
preferredApplicableProperties(P,[Prop|Properties],[Prop|ApplicableProperties],RestProperties):-
			assertionToConcept(Prop,CDesc),
			racer:worldKB(Link,_),
			racer:concept_subsumes(Link,P,CDesc),%Queries for Concept Terms
			preferredApplicableProperties(P,Properties,ApplicableProperties,RestProperties).
preferredApplicableProperties(P,[Prop|Properties],ApplicableProperties,[Prop|RestProperties]):-
			preferredApplicableProperties(P,Properties,ApplicableProperties,RestProperties).

/* directTypeColorConcepts(+Individual,+ConceptList,-FilteredConceptList).
This predicate eliminates those concepts direct concepts that are neither subsumed by the object concept nor by the colour concept.
*/
directTypeColorConcepts(Individual,[],[indef(Individual)]).
directTypeColorConcepts(Individual,[Concept|ConceptList],[Ass|FilteredConceptList]):-
			racer:worldKB(Link,_),
			not(racer:concept_subsumes(Link,Concept,'special')),
			racer:concept_subsumes(Link,'(or object room exit)',Concept),%Queries for Concept Terms
			Ass=..[Concept,Individual],
			directTypeColorConcepts(Individual,ConceptList,FilteredConceptList).
directTypeColorConcepts(Individual,[Concept|ConceptList],[Ass|FilteredConceptList]):-
			racer:worldKB(Link,_),
			racer:concept_subsumes(Link,'colour',Concept),%Queries for Concept Terms
			Ass=..[Concept,Individual],
			directTypeColorConcepts(Individual,ConceptList,FilteredConceptList).
directTypeColorConcepts(Individual,[Concept|ConceptList],FilteredConceptList):-
			directTypeColorConcepts(Individual,ConceptList,FilteredConceptList).

/*Auxiliary predicates*/

assertionToConcept(As,CDesc):- As=..[C,I1,I2],!, concat('(some ',C,FPart),concat(FPart,' *top*)',CDesc).
assertionToConcept(As,CDesc):- As=..[CDesc|_].

singleSet([I|[]]).

displayConcepts([]):- nl.
displayConcepts([X|L]):-
			write(X), tab(1), displayConcepts(L).

different(X,Y):-igual(X,Y),!,fail.
different(X,Y).

igual([],[]).
igual([X],[X]).
igual([X|Y],[X|Z]):-igual(Y,Z).