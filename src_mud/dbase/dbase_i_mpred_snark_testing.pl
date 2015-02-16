% :- load_kif("; This is the IDE's built-in-editor, where you create and edit lisp source code. You could use some other editor instead, though the IDE's menu-bar commands would not be applicable there. This editor has a tab for each file that it's editing. You can create a new editor buffer at any time with the File | New command. Other commands such as Search | Find Definitions will create editor buffers automatically for existing code. You can use the File | Compile and Load command to compile and load an entire file, or compile an individual definition by placing the text cursor inside it and using Tools | Incremental Compile. You can similarly evaluate test expressions in the editor by using Tools | Incremental Evaluation the returned values and any printed output will appear in a lisp listener in the Debug Window. For a brief introduction to other IDE tools, try the Help | Interactive IDE Intro command. And be sure to explore the other facilities on the Help menu.").
% :- load_kif("; ================================================ SUMO (Suggested Upper Merged Ontology) ================================================").
% :- load_kif("; The original versions of SUMO incorporated elements from many public sources which are documented at http://www.ontologyportal.org/SUMOhistory/").
% :- load_kif("; The SUMO is freely available, subject to the following IEEE license.").
% :- load_kif("; ---------------------- Copyright © 2004 by the Institute of Electrical and Electronics Engineers, Inc. Three Park Avenue New York, NY 10016-5997, USA All rights reserved.").
% :- load_kif("; 1. COPYRIGHT The Institute of Electrical and Electronics Engineers, Inc., (\"IEEE\") owns the copyright to this Document in all forms of media. Copyright in the text retrieved, displayed or output from this Document is owned by IEEE and is protected by the copyright laws of the United States and by international treaties. The IEEE reserves all rights not expressly granted herein.").
% :- load_kif("; 2. ROYALTIES The IEEE is providing the Document at no charge. However, the Document is not to be considered \"Public Domain,\" as the IEEE is, and at all times shall remain, the sole copyright holder in the Document. The IEEE intends to revise the Document from time to time the latest version shall be available at http://standards.ieee.org/catalog/").
% :- load_kif("; 3. TERMS OF USE The IEEE hereby grants Licensee a perpetual, non-exclusive, royalty-free, world-wide right and license to copy, publish and distribute the Document in any way, and to prepare derivative works that are based on or incorporate all or part of the Document provided that the IEEE is appropriately acknowledged as the source and copyright owner in each and every use.").
% :- load_kif("; 4. LIMITED WARRANTIES & LIMITATIONS OF REMEDIES LICENSOR Does not warrant or represent the accuracy or content of the document and Expressly Disclaims any Express or Implied Warranty, including any Implied Warranty of Merchantability or Fitness for a Specific Purpose or that the use of the document is free from patent infringement. The document is supplied ONLY \"AS IS.\" ----------------------").
% :- load_kif("; The SUMO was initially developed at Teknowledge Corp.").
% :- load_kif("; Any questions or comments about this ontology can be directed to the Technical editor, Adam Pease, apease [at] articulatesoftware [dot] com").
% :- load_kif(";      ").
% :- load_kif("; The knowledge representation language in which the SUMO is expressed is SUO-KIF, which stands for \"Standard Upper Ontology - Knowledge Interchange Format\". SUO-KIF is a simplified form of the popular KIF knowledge representation language. A specification of SUO-KIF can be found at: http://www.ontologyportal.org").
% :- load_kif("; The SUMO is a modular ontology. That is, the ontology is divided into self-contained subontologies. Each subontology is indicated by a section header, and the dependencies between the subontologies are specified with statements of the form \" INCLUDES '<SUBONTOLOGY>'\". These statements are found at the beginning of each section.").
% :- load_kif("; We ask the people using or referencing SUMO cite our primary paper:").
% :- load_kif("; Niles, I., and Pease, A. 2001. Towards a Standard Upper Ontology. In Proceedings of the 2nd International Conference on Formal Ontology in Information Systems (FOIS-2001), Chris Welty and Barry Smith, eds, Ogunquit, Maine, October 17-19, 2001. Also see http://www.ontologyportal.org").
% :- load_kif("; BEGIN FILE").
% :- load_kif(";  STRUCTURAL ONTOLOGY  ").
% :- load_kif("; INCLUDES 'BASE ONTOLOGY'").
% :- load_kif("; The Structural Ontology consists of definitions of certain syntactic abbreviations that can be both heuristically useful and computationally advantageous.").
:- load_kif("(instance instance BinaryPredicate)").
:- load_kif("(domain instance 1 Entity)").
:- load_kif("(domain instance 2 SetOrClass)").
:- load_kif("(documentation instance EnglishLanguage \"An object is an &%instance of a &%SetOrClass if it is included in that &%SetOrClass. An individual may be an instance of many classes, some of which may be subclasses of others. Thus, there is no assumption in the meaning of &%instance about specificity or uniqueness.\")").
:- load_kif("(subrelation immediateInstance instance)").
:- load_kif("(instance immediateInstance AsymmetricRelation)").
:- load_kif("(instance immediateInstance IntransitiveRelation)").
:- load_kif("(documentation immediateInstance EnglishLanguage \"An object is an &%immediateInstance of a &%SetOrClass if it is an instance of the &%SetOrClass and it is not an instance of a proper subclass of &%SetOrClass.\")").
:- load_kif("(=> (immediateInstance ?ENTITY ?CLASS) (not (exists (?SUBCLASS) (and (subclass ?SUBCLASS ?CLASS) (not (equal ?SUBCLASS ?CLASS)) (instance ?ENTITY ?SUBCLASS)))))").
:- load_kif("(instance inverse BinaryPredicate)").
:- load_kif("(instance inverse IrreflexiveRelation)").
:- load_kif("(instance inverse IntransitiveRelation)").
:- load_kif("(instance inverse SymmetricRelation)").
:- load_kif("(domain inverse 1 BinaryRelation)").
:- load_kif("(domain inverse 2 BinaryRelation)").
:- load_kif("(documentation inverse EnglishLanguage \"The inverse of a &%BinaryRelation is a relation in which all the tuples of the original relation are reversed. In other words, one &%BinaryRelation is the inverse of another if they are equivalent when their arguments are swapped.\")").
% :- load_kif("; causes a predicate variable expansion explosion ").
% :- load_kif(" (=> (and (inverse ?REL1 ?REL2) (instance ?REL1 Predicate) (instance ?REL2 Predicate)) (forall (?INST1 ?INST2) (<=> (?REL1 ?INST1 ?INST2) (?REL2 ?INST2 ?INST1))))").
:- load_kif("(instance subclass BinaryPredicate)").
:- load_kif("(instance subclass PartialOrderingRelation)").
:- load_kif("(domain subclass 1 SetOrClass)").
:- load_kif("(domain subclass 2 SetOrClass)").
:- load_kif("(documentation subclass EnglishLanguage \"(&%subclass ?CLASS1 ?CLASS2) means that ?CLASS1 is a subclass of ?CLASS2, i.e. every instance of ?CLASS1 is also an instance of ?CLASS2. A class may have multiple superclasses and subclasses.\")").
:- load_kif("(=> (subclass ?X ?Y) (and (instance ?X SetOrClass) (instance ?Y SetOrClass)))").
:- load_kif("(=> (and (subclass ?X ?Y) (instance ?Z ?X)) (instance ?Z ?Y))").
:- load_kif("(subrelation immediateSubclass subclass)").
:- load_kif("(instance immediateSubclass AsymmetricRelation)").
:- load_kif("(instance immediateSubclass IntransitiveRelation)").
:- load_kif("(documentation immediateSubclass EnglishLanguage \"A &%SetOrClass ?CLASS1 is an &%immediateSubclass of another &%SetOrClass ?CLASS2 just in case ?CLASS1 is a subclass of ?CLASS2 and there is no other subclass of ?CLASS2 such that ?CLASS1 is also a subclass of it.\")").
:- load_kif("(=> (immediateSubclass ?CLASS1 ?CLASS2) (not (exists (?CLASS3) (and (subclass ?CLASS3 ?CLASS2) (subclass ?CLASS1 ?CLASS3) (not (equal ?CLASS2 ?CLASS3)) (not (equal ?CLASS1 ?CLASS3))))))").
:- load_kif("(instance subrelation BinaryPredicate)").
:- load_kif("(instance subrelation PartialOrderingRelation)").
:- load_kif("(domain subrelation 1 Relation)").
:- load_kif("(domain subrelation 2 Relation)").
:- load_kif("(documentation subrelation EnglishLanguage \"(&%subrelation ?REL1 ?REL2) means that every tuple of ?REL1 is also a tuple of ?REL2. In other words, if the &%Relation ?REL1 holds for some arguments arg_1, arg_2, ... arg_n, then the &%Relation ?REL2 holds for the same arguments. A consequence of this is that a &%Relation and its subrelations must have the same &%valence.\")").
:- load_kif("(=> (and (subrelation ?PRED1 ?PRED2) (valence ?PRED1 ?NUMBER)) (valence ?PRED2 ?NUMBER))").
:- load_kif("(=> (and (subrelation ?PRED1 ?PRED2) (domain ?PRED2 ?NUMBER ?CLASS1)) (domain ?PRED1 ?NUMBER ?CLASS1))").
:- load_kif("(=> (and (subrelation ?REL1 ?REL2) (instance ?REL1 Predicate) (instance ?REL2 Predicate) (?REL1 @ROW)) (?REL2 @ROW))").
:- load_kif("(=> (and (subrelation ?PRED1 ?PRED2) (instance ?PRED2 ?CLASS) (instance ?CLASS InheritableRelation)) (instance ?PRED1 ?CLASS))").
:- load_kif("(instance domain TernaryPredicate)").
:- load_kif("(domain domain 1 Relation)").
:- load_kif("(domain domain 2 PositiveInteger)").
:- load_kif("(domain domain 3 SetOrClass)").
:- load_kif("(documentation domain EnglishLanguage \"Provides a computationally and heuristically convenient mechanism for declaring the argument types of a given relation. The formula (&%domain ?REL ?INT ?CLASS) means that the ?INT'th element of each tuple in the relation ?REL must be an instance of ?CLASS. Specifying argument types is very helpful in maintaining ontologies. Representation systems can use these specifications to classify terms and check integrity constraints. If the restriction on the argument type of a &%Relation is not captured by a &%SetOrClass already defined in the ontology, one can specify a &%SetOrClass compositionally with the functions &%UnionFn, &%IntersectionFn, etc.\")").
:- load_kif("(=> (and (domain ?REL ?NUMBER ?CLASS1) (domain ?REL ?NUMBER ?CLASS2)) (or (subclass ?CLASS1 ?CLASS2) (subclass ?CLASS2 ?CLASS1)))").
:- load_kif("(instance domainSubclass TernaryPredicate)").
:- load_kif("(domain domainSubclass 1 Relation)").
:- load_kif("(domain domainSubclass 2 PositiveInteger)").
:- load_kif("(domain domainSubclass 3 SetOrClass)").
:- load_kif("(documentation domainSubclass EnglishLanguage \"&%Predicate used to specify argument type restrictions of &%Predicates. The formula (&%domainSubclass ?REL ?INT ?CLASS) means that the ?INT'th element of each tuple in the relation ?REL must be a subclass of ?CLASS.\")").
:- load_kif("(=> (and (subrelation ?REL1 ?REL2) (domainSubclass ?REL2 ?NUMBER ?CLASS1)) (domainSubclass ?REL1 ?NUMBER ?CLASS1))").
:- load_kif("(=> (and (domainSubclass ?REL ?NUMBER ?CLASS1) (domainSubclass ?REL ?NUMBER ?CLASS2)) (or (subclass ?CLASS1 ?CLASS2) (subclass ?CLASS2 ?CLASS1)))").
:- load_kif("(instance equal BinaryPredicate)").
:- load_kif("(instance equal EquivalenceRelation)").
:- load_kif("(instance equal RelationExtendedToQuantities)").
:- load_kif("(domain equal 1 Entity)").
:- load_kif("(domain equal 2 Entity)").
:- load_kif("(documentation equal EnglishLanguage \"(equal ?ENTITY1 ?ENTITY2) is true just in case ?ENTITY1 is identical with ?ENTITY2.\")").
:- load_kif("(=> (equal ?THING1 ?THING2) (forall (?ATTR) (<=> (property ?THING1 ?ATTR) (property ?THING2 ?ATTR))))").
:- load_kif("(=> (equal ?ATTR1 ?ATTR2) (forall (?THING) (<=> (property ?THING ?ATTR1) (property ?THING ?ATTR2))))").
:- load_kif("(=> (equal ?THING1 ?THING2) (forall (?CLASS) (<=> (instance ?THING1 ?CLASS) (instance ?THING2 ?CLASS))))").
:- load_kif("(=> (equal ?CLASS1 ?CLASS2) (forall (?THING) (<=> (instance ?THING ?CLASS1) (instance ?THING ?CLASS2))))").
% :- load_kif(" (=> (equal ?REL1 ?REL2) (forall (@ROW) (<=> (?REL1 @ROW) (?REL2 @ROW))))").
:- load_kif("(=> (equal ?LIST1 ?LIST2) (=> (and (equal ?LIST1 (ListFn @ROW1)) (equal ?LIST2 (ListFn @ROW2))) (forall (?NUMBER) (equal (ListOrderFn (ListFn @ROW1) ?NUMBER) (ListOrderFn (ListFn @ROW2) ?NUMBER)))))").
:- load_kif("(instance range BinaryPredicate)").
:- load_kif("(instance range AsymmetricRelation)").
:- load_kif("(domain range 1 Function)").
:- load_kif("(domain range 2 SetOrClass)").
:- load_kif("(documentation range EnglishLanguage \"Gives the range of a function. In other words, &%range ?FUNCTION ?CLASS) means that all of the values assigned by ?FUNCTION are &%instances of ?CLASS.\")").
:- load_kif("(=> (and (range ?FUNCTION ?CLASS) (equal (AssignmentFn ?FUNCTION @ROW) ?VALUE)) (instance ?VALUE ?CLASS))").
:- load_kif("(=> (and (subrelation ?REL1 ?REL2) (range ?REL2 ?CLASS1)) (range ?REL1 ?CLASS1))").
:- load_kif("(=> (and (range ?REL ?CLASS1) (range ?REL ?CLASS2)) (or (subclass ?CLASS1 ?CLASS2) (subclass ?CLASS2 ?CLASS1)))").
:- load_kif("(instance rangeSubclass BinaryPredicate)").
:- load_kif("(instance rangeSubclass AsymmetricRelation)").
:- load_kif("(domain rangeSubclass 1 Function)").
:- load_kif("(domainSubclass rangeSubclass 2 SetOrClass)").
:- load_kif("(documentation rangeSubclass EnglishLanguage \"(&%rangeSubclass ?FUNCTION ?CLASS) means that all of the values assigned by ?FUNCTION are &%subclasses of ?CLASS.\")").
:- load_kif("(=> (and (rangeSubclass ?FUNCTION ?CLASS) (equal (AssignmentFn ?FUNCTION @ROW) ?VALUE)) (subclass ?VALUE ?CLASS))").
:- load_kif("(=> (and (subrelation ?REL1 ?REL2) (rangeSubclass ?REL2 ?CLASS1)) (rangeSubclass ?REL1 ?CLASS1))").
:- load_kif("(=> (and (rangeSubclass ?REL ?CLASS1) (rangeSubclass ?REL ?CLASS2)) (or (subclass ?CLASS1 ?CLASS2) (subclass ?CLASS2 ?CLASS1)))").
:- load_kif("(instance valence BinaryPredicate)").
:- load_kif("(instance valence AsymmetricRelation)").
:- load_kif("(instance valence SingleValuedRelation)").
:- load_kif("(domain valence 1 Relation)").
:- load_kif("(domain valence 2 PositiveInteger)").
:- load_kif("(documentation valence EnglishLanguage \"Specifies the number of arguments that a relation can take. If a relation does not have a fixed number of arguments, it does not have a valence and it is an instance of &%VariableArityRelation.\")").
:- load_kif("(instance documentation TernaryPredicate)").
:- load_kif("(domain documentation 1 Entity)").
:- load_kif("(domain documentation 2 HumanLanguage)").
:- load_kif("(domain documentation 3 SymbolicString)").
:- load_kif("(documentation documentation EnglishLanguage \"A relation between objects in the domain of discourse and strings of natural language text stated in a particular &%HumanLanguage. The domain of &%documentation is not constants (names), but the objects themselves. This means that one does not quote the names when associating them with their documentation.\")").
:- load_kif("(instance format TernaryPredicate)").
:- load_kif("(domain format 1 Language)").
:- load_kif("(domain format 2 Entity)").
:- load_kif("(domain format 3 SymbolicString)").
:- load_kif("(documentation format EnglishLanguage \"A relation that specifies how to present an expression in a natural language format.\")").
:- load_kif("(instance termFormat TernaryPredicate)").
:- load_kif("(domain termFormat 1 Language)").
:- load_kif("(domain termFormat 2 Entity)").
:- load_kif("(domain termFormat 3 SymbolicString)").
:- load_kif("(documentation termFormat EnglishLanguage \"A relation that specifies how to present a term in a natural language format.\")").
:- load_kif("(instance disjoint BinaryPredicate)").
:- load_kif("(instance disjoint SymmetricRelation)").
:- load_kif("(domain disjoint 1 SetOrClass)").
:- load_kif("(domain disjoint 2 SetOrClass)").
:- load_kif("(documentation disjoint EnglishLanguage \"&%Classes are &%disjoint only if they share no instances, i.e. just in case the result of applying &%IntersectionFn to them is empty.\")").
:- load_kif("(<=> (disjoint ?CLASS1 ?CLASS2) (forall (?INST) (not (and (instance ?INST ?CLASS1) (instance ?INST ?CLASS2)))))").
:- load_kif("(instance disjointRelation BinaryPredicate)").
:- load_kif("(instance disjointRelation IrreflexiveRelation)").
:- load_kif("(domain disjointRelation 1 Relation)").
:- load_kif("(domain disjointRelation 2 Relation)").
:- load_kif("(relatedInternalConcept disjointRelation disjoint)").
:- load_kif("(documentation disjointRelation EnglishLanguage \"This predicate relates two &%Relations.  (&%disjointRelation ?REL1 ?REL2) means that the two relations have no tuples in common.\")").
:- load_kif("(=> (and (domain ?REL1 ?NUMBER ?CLASS1) (domain ?REL2 ?NUMBER ?CLASS2) (disjoint ?CLASS1 ?CLASS2)) (disjointRelation ?REL1 ?REL2))").
:- load_kif("(=> (and (domainSubclass ?REL1 ?NUMBER ?CLASS1) (domainSubclass ?REL2 ?NUMBER ?CLASS2) (disjoint ?CLASS1 ?CLASS2)) (disjointRelation ?REL1 ?REL2))").
:- load_kif("(=> (and (range ?REL1 ?CLASS1) (range ?REL2 ?CLASS2) (disjoint ?CLASS1 ?CLASS2)) (disjointRelation ?REL1 ?REL2))").
:- load_kif("(=> (and (rangeSubclass ?REL1 ?CLASS1) (rangeSubclass ?REL2 ?CLASS2) (disjoint ?CLASS1 ?CLASS2)) (disjointRelation ?REL1 ?REL2))").
:- load_kif("(=> (and (instance ?REL1 Predicate) (instance ?REL2 Predicate) (disjointRelation ?REL1 ?REL2) (not (equal ?REL1 ?REL2)) (?REL1 @ROW2)) (not (?REL2 @ROW2)))").
:- load_kif("(instance contraryAttribute Predicate)").
:- load_kif("(instance contraryAttribute VariableArityRelation)").
:- load_kif("(documentation contraryAttribute EnglishLanguage \"A &%contraryAttribute is a set of &%Attributes such that something can not simultaneously have more than one of these &%Attributes. For example, (&%contraryAttribute &%Pliable &%Rigid) means that nothing can be both &%Pliable and &%Rigid.\")").
:- load_kif("(=> (contraryAttribute @ROW) (=> (inList ?ELEMENT (ListFn @ROW)) (instance ?ELEMENT Attribute)))").
:- load_kif("(=> (and (contraryAttribute @ROW1) (identicalListItems (ListFn @ROW1) (ListFn @ROW2))) (contraryAttribute @ROW2))").
:- load_kif("(=> (contraryAttribute @ROW) (forall (?ATTR1 ?ATTR2) (=> (and (equal ?ATTR1 (ListOrderFn (ListFn @ROW) ?NUMBER1)) (equal ?ATTR2 (ListOrderFn (ListFn @ROW) ?NUMBER2)) (not (equal ?NUMBER1 ?NUMBER2))) (=> (property ?OBJ ?ATTR1) (not (property ?OBJ ?ATTR2))))))").
:- load_kif("(instance exhaustiveAttribute Predicate)").
:- load_kif("(instance exhaustiveAttribute VariableArityRelation)").
:- load_kif("(domainSubclass exhaustiveAttribute 1 Attribute)").
:- load_kif("(documentation exhaustiveAttribute EnglishLanguage \"This predicate relates a &%Class to a set of &%Attributes, and it means that the elements of this set exhaust the instances of the &%Class. For example, (&%exhaustiveAttribute &%PhysicalState &%Solid &%Fluid &%Liquid &%Gas &%Plasma) means that there are only five instances of the class &%PhysicalState, viz. &%Solid, &%Fluid, &%Liquid, &%Gas and &%Plasma.\")").
:- load_kif("(=> (exhaustiveAttribute ?CLASS @ROW) (=> (inList ?ATTR (ListFn @ROW)) (instance ?ATTR Attribute)))").
:- load_kif("(=> (exhaustiveAttribute ?CLASS @ROW) (forall (?ATTR1) (=> (instance ?ATTR1 ?CLASS) (exists (?ATTR2) (and (inList ?ATTR2 (ListFn @ROW)) (equal ?ATTR1 ?ATTR2))))))").
:- load_kif("(=> (exhaustiveAttribute ?ATTRCLASS @ROW) (not (exists (?EL) (and (instance ?EL ?ATTRCLASS) (not (exists (?ATTR ?NUMBER) (and (equal ?EL ?ATTR) (equal ?ATTR (ListOrderFn (ListFn @ROW) ?NUMBER)))))))))").
:- load_kif("(instance exhaustiveDecomposition Predicate)").
:- load_kif("(instance exhaustiveDecomposition VariableArityRelation)").
:- load_kif("(domain exhaustiveDecomposition 1 Class)").
:- load_kif("(relatedInternalConcept exhaustiveDecomposition partition)").
:- load_kif("(documentation exhaustiveDecomposition EnglishLanguage \"An &%exhaustiveDecomposition of a &%Class C is a set of subclasses of C such that every instance of C is an instance of one of the subclasses in the set. Note: this does not necessarily mean that the elements of the set are disjoint (see &%partition - a &%partition is a disjoint exhaustive decomposition).\")").
:- load_kif("(=> (exhaustiveDecomposition @ROW) (=> (inList ?ELEMENT (ListFn @ROW)) (instance ?ELEMENT Class)))").
:- load_kif("(instance disjointDecomposition Predicate)").
:- load_kif("(instance disjointDecomposition VariableArityRelation)").
:- load_kif("(domain disjointDecomposition 1 Class)").
:- load_kif("(relatedInternalConcept disjointDecomposition exhaustiveDecomposition)").
:- load_kif("(relatedInternalConcept disjointDecomposition disjoint)").
:- load_kif("(documentation disjointDecomposition EnglishLanguage \"A &%disjointDecomposition of a &%Class C is a set of subclasses of C that are mutually &%disjoint.\")").
:- load_kif("(=> (disjointDecomposition @ROW) (=> (inList ?ELEMENT (ListFn @ROW)) (instance ?ELEMENT Class)))").
:- load_kif("(instance partition Predicate)").
:- load_kif("(instance partition VariableArityRelation)").
:- load_kif("(domain partition 1 Class)").
:- load_kif("(documentation partition EnglishLanguage \"A &%partition of a class C is a set of mutually &%disjoint classes (a subclass partition) which covers C. Every instance of C is an instance of exactly one of the subclasses in the partition.\")").
:- load_kif("(<=> (partition @ROW) (and (exhaustiveDecomposition @ROW) (disjointDecomposition @ROW)))").
:- load_kif("(instance relatedInternalConcept BinaryPredicate)").
:- load_kif("(instance relatedInternalConcept EquivalenceRelation)").
:- load_kif("(domain relatedInternalConcept 1 Entity)").
:- load_kif("(domain relatedInternalConcept 2 Entity)").
:- load_kif("(documentation relatedInternalConcept EnglishLanguage \"Means that the two arguments are related concepts within the SUMO, i.e. there is a significant similarity of meaning between them. To indicate a meaning relation between a SUMO concept and a concept from another source, use the Predicate &%relatedExternalConcept.\")").
:- load_kif("(instance relatedExternalConcept TernaryPredicate)").
:- load_kif("(domain relatedExternalConcept 1 SymbolicString)").
:- load_kif("(domain relatedExternalConcept 2 Entity)").
:- load_kif("(domain relatedExternalConcept 3 Language)").
:- load_kif("(relatedInternalConcept relatedExternalConcept relatedInternalConcept)").
:- load_kif("(documentation relatedExternalConcept EnglishLanguage \"Used to signify a three-place relation between a concept in an external knowledge source, a concept in the SUMO, and the name of the other knowledge source.\")").
:- load_kif("(subrelation synonymousExternalConcept relatedExternalConcept)").
:- load_kif("(disjointRelation synonymousExternalConcept subsumedExternalConcept)").
:- load_kif("(disjointRelation synonymousExternalConcept subsumingExternalConcept)").
:- load_kif("(disjointRelation subsumedExternalConcept subsumingExternalConcept)").
:- load_kif("(documentation synonymousExternalConcept EnglishLanguage \"(&%synonymousExternalConcept ?STRING ?THING ?LANGUAGE) means that the SUMO concept ?THING has the same meaning as ?STRING in ?LANGUAGE.\")").
:- load_kif("(subrelation subsumingExternalConcept relatedExternalConcept)").
:- load_kif("(documentation subsumingExternalConcept EnglishLanguage \"(&%subsumingExternalConcept ?STRING ?THING ?LANGUAGE) means that the SUMO concept ?THING subsumes the meaning of ?STRING in ?LANGUAGE, i.e. the concept ?THING is broader in meaning than ?STRING.\")").
:- load_kif("(subrelation subsumedExternalConcept relatedExternalConcept)").
:- load_kif("(documentation subsumedExternalConcept EnglishLanguage \"(&%subsumedExternalConcept ?STRING ?THING ?LANGUAGE) means that the SUMO concept ?THING is subsumed by the meaning of ?STRING in ?LANGUAGE, i.e. the concept ?THING is narrower in meaning than ?STRING.\")").
:- load_kif("(instance externalImage BinaryPredicate)").
:- load_kif("(documentation externalImage EnglishLanguage \"A link between an Entity and a URL that represents or exemplifies the term in some way.\")").
:- load_kif("(domain externalImage 1 Entity)").
:- load_kif("(domain externalImage 2 SymbolicString)").
:- load_kif("(instance subAttribute BinaryPredicate)").
:- load_kif("(instance subAttribute PartialOrderingRelation)").
:- load_kif("(domain subAttribute 1 Attribute)").
:- load_kif("(domain subAttribute 2 Attribute)").
:- load_kif("(disjointRelation subAttribute successorAttribute)").
:- load_kif("(documentation subAttribute EnglishLanguage \"Means that the second argument can be ascribed to everything which has the first argument ascribed to it.\")").
:- load_kif("(=> (subAttribute ?ATTR1 ?ATTR2) (forall (?OBJ) (=> (property ?OBJ ?ATTR1) (property ?OBJ ?ATTR2))))").
:- load_kif("(=> (and (subAttribute ?ATTR1 ?ATTR2) (instance ?ATTR2 ?CLASS)) (instance ?ATTR1 ?CLASS))").
:- load_kif("(instance successorAttribute BinaryPredicate)").
:- load_kif("(instance successorAttribute AsymmetricRelation)").
:- load_kif("(domain successorAttribute 1 Attribute)").
:- load_kif("(domain successorAttribute 2 Attribute)").
:- load_kif("(documentation successorAttribute EnglishLanguage \"(&%successorAttribute ?ATTR1 ?ATTR2) means that ?ATTR2 is the &%Attribute that comes immediately after ?ATTR1 on the scale that they share.\")").
:- load_kif("(=> (and (successorAttribute ?ATTR1 ?ATTR2) (holdsDuring ?TIME1 (property ?ENTITY ?ATTR2))) (exists (?TIME2) (and (temporalPart ?TIME2 (PastFn ?TIME1)) (holdsDuring ?TIME2 (property ?ENTITY ?ATTR1)))))").
:- load_kif("(instance successorAttributeClosure BinaryPredicate)").
:- load_kif("(instance successorAttributeClosure TransitiveRelation)").
:- load_kif("(instance successorAttributeClosure IrreflexiveRelation)").
:- load_kif("(domain successorAttributeClosure 1 Attribute)").
:- load_kif("(domain successorAttributeClosure 2 Attribute)").
:- load_kif("(relatedInternalConcept successorAttributeClosure successorAttribute)").
:- load_kif("(documentation successorAttributeClosure EnglishLanguage \"The transitive closure of &%successorAttribute. (&%successorAttributeClosure ?ATTR1 ?ATTR2) means that there is a chain of &%successorAttribute assertions connecting ?ATTR1 and ?ATTR2.\")").
:- load_kif("(=> (successorAttribute ?ATTR1 ?ATTR2) (successorAttributeClosure ?ATTR1 ?ATTR2))").
:- load_kif("(instance greaterThanByQuality TernaryPredicate)").
:- load_kif("(documentation greaterThanByQuality EnglishLanguage \"(greaterThanByQuality ?ENTITY1 ?ENTITY2 ?ATT) means that ?ENTITY1 has more of the given quality ?ATT than ?ENTITY2)\")").
:- load_kif("(domain greaterThanByQuality 1 Entity)").
:- load_kif("(domain greaterThanByQuality 2 Entity)").
:- load_kif("(domain greaterThanByQuality 3 Attribute)").
:- load_kif("(=> (and (greaterThanByQuality ?E1 ?E2 ?ATT) (greaterThanByQuality ?E2 ?E3 ?ATT)) (greaterThanByQuality ?E1 ?E3 ?ATT))").
:- load_kif("(=> (greaterThanByQuality ?E1 ?E2 ?ATT) (not (greaterThanByQuality ?E2 ?E1 ?ATT)))").
:- load_kif("(=> (greaterThanByQuality ?E1 ?E2 ?ATT) (not (equal ?E2 ?E1)))").
:- load_kif("(instance entails BinaryPredicate)").
:- load_kif("(domain entails 1 Formula)").
:- load_kif("(domain entails 2 Formula)").
:- load_kif("(documentation entails EnglishLanguage \"The operator of logical entailment. (&%entails ?FORMULA1 ?FORMULA2) means that ?FORMULA2 can be derived from ?FORMULA1 by means of the proof theory of SUO-KIF.\")").
% :- load_kif("; The following axiom is commented out, because it is rejected by the inference engine parser.").
% :- load_kif(" (=> (entails ?FORMULA1 ?FORMULA2) (=> ?FORMULA1 ?FORMULA2))").
:- load_kif("(instance AssignmentFn Function)").
:- load_kif("(instance AssignmentFn VariableArityRelation)").
:- load_kif("(domain AssignmentFn 1 Function)").
:- load_kif("(range AssignmentFn Entity)").
:- load_kif("(documentation AssignmentFn EnglishLanguage \"If F is a &%Function with a value for the objects denoted by N1,..., NK, then (&%AssignmentFn F N1 ... NK) is the value of applying F to the objects denoted by N1,..., NK. Otherwise, the value is undefined.\")").
:- load_kif("(instance PowerSetFn UnaryFunction)").
:- load_kif("(instance PowerSetFn TotalValuedRelation)").
:- load_kif("(domain PowerSetFn 1 SetOrClass)").
:- load_kif("(rangeSubclass PowerSetFn SetOrClass)").
:- load_kif("(documentation PowerSetFn EnglishLanguage \"(&%PowerSetFn ?CLASS) maps the &%SetOrClass ?CLASS to the &%SetOrClass of all &%subclasses of ?CLASS.\")").
% :- load_kif("; END FILE").
% :- load_kif("; BEGIN FILE").
% :- load_kif(";  BASE ONTOLOGY  ").
% :- load_kif("; INCLUDES 'STRUCTURAL ONTOLOGY'").
% :- load_kif("; The following hierarchy incorporates content from Sowa, Russell & Norvig, and the top-level ontology from ITBM-CNR.").
:- load_kif("(partition Entity Physical Abstract)").
:- load_kif("(documentation Entity EnglishLanguage \"The universal class of individuals. This is the root node of the ontology.\")").
:- load_kif("(forall (?THING) (instance ?THING Entity))").
:- load_kif("(exists (?THING) (instance ?THING Entity))").
:- load_kif("(<=> (instance ?CLASS Class) (subclass ?CLASS Entity))").
:- load_kif("(subclass Physical Entity)").
:- load_kif("(partition Physical Object Process)").
:- load_kif("(documentation Physical EnglishLanguage \"An entity that has a location in space-time. Note that locations are themselves understood to have a location in space-time.\")").
:- load_kif("(<=> (instance ?PHYS Physical) (exists (?LOC ?TIME) (and (located ?PHYS ?LOC) (time ?PHYS ?TIME))))").
:- load_kif("(subclass Object Physical)").
:- load_kif("(documentation Object EnglishLanguage \"Corresponds roughly to the class of ordinary objects. Examples include normal physical objects, geographical regions, and locations of &%Processes, the complement of &%Objects in the &%Physical class. In a 4D ontology, an &%Object is something whose spatiotemporal extent is thought of as dividing into spatial parts roughly parallel to the time-axis.\")").
:- load_kif("(subclass SelfConnectedObject Object)").
:- load_kif("(documentation SelfConnectedObject EnglishLanguage \"A &%SelfConnectedObject is any &%Object that does not consist of two or more disconnected parts.\")").
:- load_kif("(subclass OrganicThing SelfConnectedObject)").
:- load_kif("(documentation OrganicThing EnglishLanguage \"A &%SelfConnectedObject that is produced by a non-intentional process from an &%Organism. Note that this refers only to the primary cause. That is, a &%PlantAgriculturalProduct is firstly produced by a &%Plant, and only secondarily by a &%Human that is tending the plant.\")").
:- load_kif("(instance FrontFn SpatialRelation)").
:- load_kif("(instance FrontFn PartialValuedRelation)").
:- load_kif("(instance FrontFn UnaryFunction)").
:- load_kif("(instance FrontFn AsymmetricRelation)").
:- load_kif("(instance FrontFn IrreflexiveRelation)").
:- load_kif("(domain FrontFn 1 SelfConnectedObject)").
:- load_kif("(range FrontFn SelfConnectedObject)").
:- load_kif("(documentation FrontFn EnglishLanguage \"A &%Function that maps an &%Object to the side that generally receives the most attention or that typically faces the direction in which the &%Object moves. Note that this is a partial function, since some &%Objects do not have sides, e.g. apples and spheres. Note too that the &%range of this &%Function is indefinite in much the way that &%ImmediateFutureFn and &%ImmediatePastFn are indefinite. Although this indefiniteness is undesirable from a theoretical standpoint, it does not have significant practical implications, since there is widespread intersubjective agreement about the most common cases.\")").
:- load_kif("(=> (instance ?OBJ SelfConnectedObject) (side (FrontFn ?OBJ) ?OBJ))").
:- load_kif("(instance BackFn SpatialRelation)").
:- load_kif("(instance BackFn PartialValuedRelation)").
:- load_kif("(instance BackFn UnaryFunction)").
:- load_kif("(instance BackFn AsymmetricRelation)").
:- load_kif("(instance BackFn IrreflexiveRelation)").
:- load_kif("(domain BackFn 1 SelfConnectedObject)").
:- load_kif("(range BackFn SelfConnectedObject)").
:- load_kif("(documentation BackFn EnglishLanguage \"A &%Function that maps an &%Object to the side that is opposite the &%FrontFn of the &%Object. Note that this is a partial function, since some &%Objects do not have sides, e.g. apples and spheres. Note too that the &%range of this &%Function is indefinite in much the way that &%ImmediateFutureFn and &%ImmediatePastFn are indefinite. Although this indefiniteness is undesirable from a theoretical standpoint, it does not have significant practical implications, since there is widespread intersubjective agreement about the most common cases.\")").
:- load_kif("(=> (instance ?OBJ SelfConnectedObject) (side (BackFn ?OBJ) ?OBJ))").
:- load_kif("(instance part SpatialRelation)").
:- load_kif("(instance part PartialOrderingRelation)").
:- load_kif("(domain part 1 Object)").
:- load_kif("(domain part 2 Object)").
:- load_kif("(documentation part EnglishLanguage \"The basic mereological relation. All other mereological relations are defined in terms of this one.  (&%part ?PART ?WHOLE) simply means that the &%Object ?PART is part of the &%Object ?WHOLE. Note that, since &%part is a &%ReflexiveRelation, every &%Object is a part of itself.\")").
:- load_kif("(instance properPart AsymmetricRelation)").
:- load_kif("(instance properPart TransitiveRelation)").
:- load_kif("(subrelation properPart part)").
:- load_kif("(documentation properPart EnglishLanguage \"(&%properPart ?OBJ1 ?OBJ2) means that ?OBJ1 is a part of ?OBJ2 other than ?OBJ2 itself. This is a &%TransitiveRelation and &%AsymmetricRelation (hence an &%IrreflexiveRelation).\")").
:- load_kif("(<=> (properPart ?OBJ1 ?OBJ2) (and (part ?OBJ1 ?OBJ2) (not (part ?OBJ2 ?OBJ1))))").
:- load_kif("(subrelation piece part)").
:- load_kif("(domain piece 1 Substance)").
:- load_kif("(domain piece 2 Substance)").
:- load_kif("(documentation piece EnglishLanguage \"A specialized common sense notion of part for arbitrary parts of &%Substances. Quasi-synonyms are: chunk, hunk, bit, etc. Compare &%component, another subrelation of &%part.\")").
:- load_kif("(=> (piece ?SUBSTANCE1 ?SUBSTANCE2) (forall (?CLASS) (=> (instance ?SUBSTANCE1 ?CLASS) (instance ?SUBSTANCE2 ?CLASS))))").
:- load_kif("(subrelation component part)").
:- load_kif("(domain component 1 CorpuscularObject)").
:- load_kif("(domain component 2 CorpuscularObject)").
:- load_kif("(documentation component EnglishLanguage \"A specialized common sense notion of part for heterogeneous parts of complexes. (&%component ?COMPONENT ?WHOLE) means that ?COMPONENT is a component of ?WHOLE. Examples of component include the doors and walls of a house, the states or provinces of a country, or the limbs and organs of an animal. Compare &%piece, which is also a subrelation of &%part.\")").
:- load_kif("(instance material BinaryPredicate)").
:- load_kif("(domainSubclass material 1 Substance)").
:- load_kif("(domain material 2 CorpuscularObject)").
:- load_kif("(documentation material EnglishLanguage \"(&%material ?SUBSTANCE ?OBJECT) means that ?OBJECT is structurally made up in part of ?SUBSTANCE. This relation encompasses the concepts of 'composed of', 'made of', and 'formed of'. For example, plastic is a &%material of my computer monitor. Compare &%part and its subrelations, viz &%component and &%piece.\")").
:- load_kif("(subrelation contains partlyLocated)").
:- load_kif("(instance contains SpatialRelation)").
:- load_kif("(instance contains AsymmetricRelation)").
:- load_kif("(disjointRelation contains part)").
:- load_kif("(domain contains 1 SelfConnectedObject)").
:- load_kif("(domain contains 2 Object)").
:- load_kif("(documentation contains EnglishLanguage \"The relation of spatial containment for two separable objects. When the two objects are not separable (e.g. an automobile and one of its seats), the relation of &%part should be used.  (&%contains ?OBJ1 ?OBJ2) means that the &%SelfConnectedObject ?OBJ1 has a space (i.e. a &%Hole) which is at least partially filled by ?OBJ2.\")").
:- load_kif("(<=> (contains ?OBJ1 ?OBJ2) (exists (?HOLE) (and (hole ?HOLE ?OBJ1) (properlyFills ?OBJ2 ?HOLE))))").
:- load_kif("(subclass Substance SelfConnectedObject)").
:- load_kif("(partition Substance PureSubstance Mixture)").
:- load_kif("(partition Substance SyntheticSubstance NaturalSubstance)").
:- load_kif("(documentation Substance EnglishLanguage \"An &%Object in which every part is similar to every other in every relevant respect. More precisely, something is a &%Substance when it has only arbitrary pieces as parts - any parts have properties which are similar to those of the whole. Note that a &%Substance may nonetheless have physical properties that vary. For example, the temperature, chemical constitution, density, etc. may change from one part to another. An example would be a body of water.\")").
:- load_kif("(=> (and (subclass ?OBJECTTYPE Substance) (instance ?OBJECT ?OBJECTTYPE) (part ?PART ?OBJECT)) (instance ?PART ?OBJECTTYPE))").
:- load_kif("(=> (and (instance ?OBJ Substance) (attribute ?OBJ ?ATTR) (part ?PART ?OBJ)) (attribute ?PART ?ATTR))").
:- load_kif("(subclass SyntheticSubstance Substance)").
:- load_kif("(documentation SyntheticSubstance EnglishLanguage \"Any &%Substance that is the result of an &%IntentionalProcess, i.e. any substance that is created by &%Humans.\")").
:- load_kif("(<=> (instance ?SUBSTANCE SyntheticSubstance) (exists (?PROCESS) (and (instance ?PROCESS IntentionalProcess) (result ?PROCESS ?SUBSTANCE) (instance ?SUBSTANCE Substance))))").
:- load_kif("(subclass NaturalSubstance Substance)").
:- load_kif("(documentation NaturalSubstance EnglishLanguage \"Any &%Substance that is not the result of an &%IntentionalProcess, i.e. any substance that occurs naturally.\")").
:- load_kif("(subclass PureSubstance Substance)").
:- load_kif("(partition PureSubstance CompoundSubstance ElementalSubstance)").
:- load_kif("(documentation PureSubstance EnglishLanguage \"The &%Class of &%Substances with constant composition. A &%PureSubstance can be either an element (&%ElementalSubstance) or a compound of elements (&%CompoundSubstance). Examples: Table salt").
:- load_kif("(sodium chloride, NaCl), sugar (sucrose, C_{12}H_{22}O_{11}), water (H_2O), iron (Fe), copper (Cu), and oxygen (O_2).\")").
:- load_kif("(subclass ElementalSubstance PureSubstance)").
:- load_kif("(documentation ElementalSubstance EnglishLanguage \"The &%Class of &%PureSubstances that cannot be separated into two or more &%Substances by ordinary chemical").
:- load_kif("(or physical) means. This excludes nuclear reactions. &%ElementalSubstances are composed of only one kind of atom. Examples: Iron (Fe), copper (Cu), and oxygen (O_2). &%ElementalSubstances are the simplest &%PureSubstances.\")").
:- load_kif("(subclass Metal ElementalSubstance)").
:- load_kif("(documentation Metal EnglishLanguage \"A &%Metal is an &%ElementalSubstance that conducts heat and electricity, is shiny and reflects many colors of light, and can be hammered into sheets or drawn into wire. About 80% of the known chemical elements"). (&%ElementalSubstances) are metals.\")").
:- load_kif("(subclass Atom ElementalSubstance)").
:- load_kif("(documentation Atom EnglishLanguage \"An extremely small unit of matter that retains its identity in Chemical reactions. It consists of an &%AtomicNucleus and &%Electrons surrounding the &%AtomicNucleus.\")").
:- load_kif("(=> (instance ?ATOM Atom) (exists (?PROTON ?ELECTRON) (and (component ?PROTON ?ATOM) (component ?ELECTRON ?ATOM) (instance ?PROTON Proton) (instance ?ELECTRON Electron))))").
:- load_kif("(=> (instance ?ATOM Atom) (forall (?NUCLEUS1 ?NUCLEUS2) (=> (and (component ?NUCLEUS1 ?ATOM) (component ?NUCLEUS2 ?ATOM) (instance ?NUCLEUS1 AtomicNucleus) (instance ?NUCLEUS2 AtomicNucleus)) (equal ?NUCLEUS1 ?NUCLEUS2))))").
:- load_kif("(subclass SubatomicParticle ElementalSubstance)").
:- load_kif("(documentation SubatomicParticle EnglishLanguage \"The class of &%ElementalSubstances that are smaller than &%Atoms and compose &%Atoms.\")").
:- load_kif("(=> (instance ?PARTICLE SubatomicParticle) (exists (?ATOM) (and (instance ?ATOM Atom) (part ?PARTICLE ?ATOM))))").
:- load_kif("(subclass AtomicNucleus SubatomicParticle)").
:- load_kif("(documentation AtomicNucleus EnglishLanguage \"The core of the &%Atom. It is composed of &%Protons and &%Neutrons.\")").
:- load_kif("(=> (instance ?NUCLEUS AtomicNucleus) (exists (?NEUTRON ?PROTON) (and (component ?NEUTRON ?NUCLEUS) (component ?PROTON ?NUCLEUS) (instance ?NEUTRON Neutron) (instance ?PROTON Proton))))").
:- load_kif("(subclass Electron SubatomicParticle)").
:- load_kif("(documentation Electron EnglishLanguage \"&%SubatomicParticles that surround the &%AtomicNucleus. They have a negative charge.\")").
:- load_kif("(subclass Proton SubatomicParticle)").
:- load_kif("(documentation Proton EnglishLanguage \"Components of the &%AtomicNucleus. They have a positive charge.\")").
:- load_kif("(subclass Neutron SubatomicParticle)").
:- load_kif("(documentation Neutron EnglishLanguage \"Components of the &%AtomicNucleus. They have no charge.\")").
:- load_kif("(subclass CompoundSubstance PureSubstance)").
:- load_kif("(documentation CompoundSubstance EnglishLanguage \"The &%Class of &%Substances that contain two or more elements (&%ElementalSubstances), in definite proportion by weight. The composition of a pure compound will be invariant, regardless of the method of preparation. Compounds are composed of more than one kind of atom (element). The term molecule is often used for the smallest unit of a compound that still retains all of the properties of the compound. Examples: Table salt (sodium chloride, NaCl), sugar (sucrose, C_{12}H_{22}O_{11}), and water (H_2O). \")").
:- load_kif("(subclass Mixture Substance)").
:- load_kif("(documentation Mixture EnglishLanguage \"A &%Mixture is two or more &%PureSubstances, combined in varying proportions - each retaining its own specific properties. The components of a &%Mixture can be separated by physical means, i.e. without the making and breaking of chemical bonds. Examples: Air, table salt thoroughly dissolved in water, milk, wood, and concrete. \")").
:- load_kif("(=> (instance ?MIXTURE Mixture) (exists (?PURE1 ?PURE2) (and (instance ?PURE1 PureSubstance) (instance ?PURE2 PureSubstance) (not (equal ?PURE1 ?PURE2)) (part ?PURE1 ?MIXTURE) (part ?PURE2 ?MIXTURE))))").
:- load_kif("(=> (and (instance ?MIXTURE Mixture) (part ?SUBSTANCE ?MIXTURE) (not (instance ?SUBSTANCE Mixture))) (instance ?SUBSTANCE PureSubstance))").
:- load_kif("(subclass CorpuscularObject SelfConnectedObject)").
:- load_kif("(disjoint CorpuscularObject Substance)").
:- load_kif("(documentation CorpuscularObject EnglishLanguage \"A &%SelfConnectedObject whose parts have properties that are not shared by the whole.\")").
:- load_kif("(=> (instance ?OBJ CorpuscularObject) (exists (?SUBSTANCE1 ?SUBSTANCE2) (and (subclass ?SUBSTANCE1 Substance) (subclass ?SUBSTANCE2 Substance) (material ?SUBSTANCE1 ?OBJ) (material ?SUBSTANCE2 ?OBJ) (not (equal ?SUBSTANCE1 ?SUBSTANCE2)))))").
:- load_kif("(subclass Region Object)").
:- load_kif("(documentation Region EnglishLanguage \"A topographic location. &%Regions encompass surfaces of &%Objects, imaginary places, and &%GeographicAreas. Note that a &%Region is the only kind of &%Object which can be located at itself. Note too that &%Region is not a subclass of &%SelfConnectedObject, because some &%Regions, e.g. archipelagos, have &%parts which are not &%connected with one another.\")").
:- load_kif("(=> (instance ?REGION Region) (exists (?PHYS) (located ?PHYS ?REGION)))").
:- load_kif("(subclass Collection Object)").
:- load_kif("(disjoint Collection SelfConnectedObject)").
:- load_kif("(documentation Collection EnglishLanguage \"Collections have &%members like &%Classes, but, unlike &%Classes, they have a position in space-time and &%members can be added and subtracted without thereby changing the identity of the &%Collection. Some examples are toolkits, football teams, and flocks of sheep.\")").
:- load_kif("(=> (instance ?COLL Collection) (exists (?OBJ) (member ?OBJ ?COLL)))").
:- load_kif("(subrelation member part)").
:- load_kif("(instance member AsymmetricRelation)").
:- load_kif("(instance member IntransitiveRelation)").
:- load_kif("(domain member 1 SelfConnectedObject)").
:- load_kif("(domain member 2 Collection)").
:- load_kif("(relatedInternalConcept member instance)").
:- load_kif("(relatedInternalConcept member element)").
:- load_kif("(documentation member EnglishLanguage \"A specialized common sense notion of part for uniform parts of &%Collections. For example, each sheep in a flock of sheep would have the relationship of member to the flock.\")").
:- load_kif("(instance subCollection BinaryPredicate)").
:- load_kif("(instance subCollection PartialOrderingRelation)").
:- load_kif("(domain subCollection 1 Collection)").
:- load_kif("(domain subCollection 2 Collection)").
:- load_kif("(documentation subCollection EnglishLanguage \"(&%subCollection ?COLL1 ?COLL2) means that the &%Collection ?COLL1 is a proper part of the &%Collection ?COLL2.\")").
:- load_kif("(subrelation subCollection part)").
:- load_kif("(<=> (subCollection ?COLL1 ?COLL2) (forall (?MEMBER) (=> (member ?MEMBER ?COLL1) (member ?MEMBER ?COLL2))))").
:- load_kif("(subclass ContentBearingPhysical Physical)").
:- load_kif("(documentation ContentBearingPhysical EnglishLanguage \"Any &%Object or &%Process that expresses content. This covers &%Objects that contain a &%Proposition, such as a book, as well as &%ManualSignLanguage, which may similarly contain a &%Proposition.\")").
:- load_kif("(=> (instance ?OBJ ContentBearingPhysical) (exists (?THING) (represents ?OBJ ?THING)))").
:- load_kif("(subclass ContentBearingProcess ContentBearingPhysical)").
% :- load_kif("; NS: add. Missing!").
:- load_kif("(subclass ContentBearingProcess Process)").
% :- load_kif("; NS: cf. Note that &%ManualHumanLanguage is not a subclass of &%Process, despite the documentation statement below.").
:- load_kif("(documentation ContentBearingProcess EnglishLanguage \"Any &%Process, for example &%ManualHumanLanguage, which may contain a &%Proposition.\")").
:- load_kif("(subclass ContentBearingObject CorpuscularObject)").
:- load_kif("(subclass ContentBearingObject ContentBearingPhysical)").
:- load_kif("(relatedInternalConcept ContentBearingObject containsInformation)").
:- load_kif("(documentation ContentBearingObject EnglishLanguage \"Any &%SelfConnectedObject that expresses content. This content may be a &%Proposition, e.g. when the &%ContentBearingObject is a &%Sentence or &%Text, or it may be a representation of an abstract or physical object, as with an &%Icon, a &%Word or a &%Phrase.\")").
:- load_kif("(subclass SymbolicString ContentBearingObject)").
:- load_kif("(documentation SymbolicString EnglishLanguage \"The &%Class of alphanumeric sequences.\")").
:- load_kif("(subclass Character SymbolicString)").
:- load_kif("(documentation Character EnglishLanguage \"An element of an alphabet, a set of numerals, etc. Note that a &%Character may or may not be part of a &%Language. &%Character is a subclass of &%SymbolicString, because every instance of &%Character is an alphanumeric sequence consisting of a single element.\")").
:- load_kif("(=> (instance ?STRING SymbolicString) (exists (?PART) (and (part ?PART ?STRING) (instance ?PART Character))))").
:- load_kif("(instance containsInformation BinaryPredicate)").
:- load_kif("(instance containsInformation AsymmetricRelation)").
:- load_kif("(subrelation containsInformation represents)").
:- load_kif("(domain containsInformation 1 ContentBearingPhysical)").
:- load_kif("(domain containsInformation 2 Proposition)").
:- load_kif("(documentation containsInformation EnglishLanguage \"A subrelation of &%represents. This predicate relates a &%ContentBearingPhysical to the &%Proposition that is expressed by the &%ContentBearingPhysical. Examples include the relationships between a physical novel and its story and between a printed score and its musical content.\")").
:- load_kif("(subclass Icon ContentBearingPhysical)").
:- load_kif("(documentation Icon EnglishLanguage \"This is the subclass of &%ContentBearingPhysical which are not part of a &%Language and which have some sort of similarity with the &%Objects that they represent. This &%Class would include symbolic roadway signs, representational art works, photographs, etc.\")").
:- load_kif("(subclass MotionPicture Text)").
:- load_kif("(documentation MotionPicture EnglishLanguage \"A &%ContentBearingObject which depicts motion").
:- load_kif("(and which may have an audio or text component as well). This &%Class covers films, videos, etc.\")").
:- load_kif("(subclass LinguisticExpression ContentBearingPhysical)").
:- load_kif("(disjoint LinguisticExpression Icon)").
:- load_kif("(documentation LinguisticExpression EnglishLanguage \"This is the subclass of &%ContentBearingPhysical which are language-related. Note that this &%Class encompasses both &%Language and the the elements of &%Languages, e.g. &%Words.\")").
:- load_kif("(subclass Language LinguisticExpression)").
:- load_kif("(disjointDecomposition Language AnimalLanguage HumanLanguage ComputerLanguage)").
:- load_kif("(documentation Language EnglishLanguage \"A system of signs for expressing thought. The system can be either natural or artificial, i.e. something that emerges gradually as a cultural artifact or something that is intentionally created by a person or group of people.\")").
:- load_kif("(subclass AnimalLanguage Language)").
:- load_kif("(documentation AnimalLanguage EnglishLanguage \"The &%subclass of &%Languages used by &%Animals other than &%Humans.\")").
:- load_kif("(=> (and (instance ?LANG AnimalLanguage) (agent ?PROC ?AGENT) (instrument ?PROC ?LANG)) (and (instance ?AGENT Animal) (not (instance ?AGENT Human))))").
:- load_kif("(subclass ArtificialLanguage Language)").
:- load_kif("(documentation ArtificialLanguage EnglishLanguage \"The &%subclass of &%Languages that are designed by &%Humans.\")").
:- load_kif("(subclass ComputerLanguage ArtificialLanguage)").
:- load_kif("(documentation ComputerLanguage EnglishLanguage \"The class of &%Languages designed for and interpreted by a computer.\")").
:- load_kif("(=> (and (instance ?LANG ComputerLanguage) (agent ?PROC ?AGENT) (instrument ?PROC ?LANG)) (instance ?AGENT Machine))").
:- load_kif("(subclass HumanLanguage Language)").
:- load_kif("(partition HumanLanguage NaturalLanguage ConstructedLanguage)").
:- load_kif("(partition HumanLanguage SpokenHumanLanguage ManualHumanLanguage)").
:- load_kif("(documentation HumanLanguage EnglishLanguage \"The &%subclass of &%Languages used by &%Humans.\")").
:- load_kif("(=> (and (instance ?LANG HumanLanguage) (agent ?PROC ?AGENT) (instrument ?PROC ?LANG)) (instance ?AGENT Human))").
:- load_kif("(subclass ConstructedLanguage HumanLanguage)").
:- load_kif("(subclass ConstructedLanguage ArtificialLanguage)").
:- load_kif("(documentation ConstructedLanguage EnglishLanguage \"An &%ConstructedLanguage is a &%HumanLanguage that did not evolve spontaneously within a language community, but rather had its core grammar and vocabulary invented by one or more language experts, often with an aim to produce a more grammatically regular language than any language that has evolved naturally. This &%Class includes languages like Esperanto that were created to facilitate international communication\")").
:- load_kif("(=> (instance ?LANG ConstructedLanguage) (exists (?PLAN) (and (instance ?PLAN Planning) (result ?PLAN ?LANG))))").
:- load_kif("(subclass NaturalLanguage HumanLanguage)").
:- load_kif("(documentation NaturalLanguage EnglishLanguage \"The &%subclass of &%HumanLanguages which are not designed and which evolve from generation to generation. This &%Class includes all of the national languages, e.g. English, Spanish, Japanese, etc. Note that this class includes dialects of natural languages.\")").
:- load_kif("(subclass ManualHumanLanguage HumanLanguage)").
:- load_kif("(documentation ManualHumanLanguage EnglishLanguage \"A &%ManualHumanLanguage is a &%HumanLanguage which has as its medium gestures and movement, such as the shape, position, and movement of the hands.\")").
:- load_kif("(subclass SpokenHumanLanguage HumanLanguage)").
:- load_kif("(documentation SpokenHumanLanguage EnglishLanguage \"A &%SpokenHumanLanguage is a &%HumanLanguage which has as its medium the human voice. It can also berepresented visually through writing, although not all &%SpokenHumanLanguages have a codified written form.\")").
:- load_kif("(instance EnglishLanguage NaturalLanguage)").
:- load_kif("(instance EnglishLanguage SpokenHumanLanguage)").
:- load_kif("(documentation EnglishLanguage EnglishLanguage \"A Germanic language that incorporates many roots from the Romance languages. It is the official language of the &%UnitedStates, the &%UnitedKingdom, and many other countries.\")").
:- load_kif("(subclass Word LinguisticExpression)").
:- load_kif("(documentation Word EnglishLanguage \"A term of a &%Language that represents a concept.\")").
:- load_kif("(subclass Formula Sentence)").
:- load_kif("(documentation Formula EnglishLanguage \"A syntactically well-formed formula in the SUO-KIF knowledge representation language.\")").
% :- load_kif("; The following ground facts incorporate the 'Agent' hierarchy from the corresponding ontology on the Ontolingua server. It also includes predicates defined in the ITBM-CNR ontology \"Actors\".").
:- load_kif("(subclass Agent Object)").
:- load_kif("(documentation Agent EnglishLanguage \"Something or someone that can act on its own and produce changes in the world.\")").
:- load_kif("(<=> (instance ?AGENT Agent) (exists (?PROC) (agent ?PROC ?AGENT)))").
:- load_kif("(subclass SentientAgent Agent)").
% :- load_kif("; NS: delete. (documentation SentientAgent EnglishLanguage \"An &%Agent that has rights but may or may not have responsibilities and the ability to reason. If the latter are present, then the &%Agent is also an instance of &%CognitiveAgent. Domesticated animals are an example of &%SentientAgents that are not also &%CognitiveAgents.\")").
% :- load_kif("; NS: add.").
:- load_kif("(documentation SentientAgent EnglishLanguage \"A &%SentientAgent is an &%Agent that is capable of &%Perception and experiences some level of consciousness (see &%ConsciousnessAttribute). If the &%Agent is able to reason at a comparatively high level (evinced by the ability to learn, plan, and feel emotions), then it is also an instance of &%CognitiveAgent. A &%DomesticAnimal is a &%SentientAgent, but may or may not be a &%CognitiveAgent, depending on the type of &%Animal.\")").
:- load_kif("(subclass CognitiveAgent SentientAgent)").
% :- load_kif("; NS: delete. (documentation CognitiveAgent EnglishLanguage \"A &%SentientAgent with responsibilities and the ability to reason, deliberate, make plans, etc. This is essentially the legal/ethical notion of a person. Note that, although &%Human is a subclass of &%CognitiveAgent, there may be instances of &%CognitiveAgent which are not also instances of &%Human. For example, chimpanzees, gorillas, dolphins, whales, and some extraterrestrials (if they exist) may be &%CognitiveAgents.\")").
% :- load_kif("; NS: add.").
:- load_kif("(documentation CognitiveAgent EnglishLanguage \"A &%CognitiveAgent is an &%Agent that has the ability to reason, deliberate, make plans, and experience emotions. Although &%Human is a subclass of &%CognitiveAgent, there may be instances of &%CognitiveAgent which are not also instances of &%Human. For example, &%Primates, dolphins, whales, and some extraterrestrials (if they exist) might be considered &%CognitiveAgents.\")").
% :- load_kif("; NS: new. Since an Entity might or might not be considered a LegalAgent for its entire existence, depending on the circumstances, it might be better to represent &%LegalAgent as a &%RelationalAttribute.").
:- load_kif("(subclass LegalAgent Agent)").
% :- load_kif("; NS: add.").
:- load_kif("(documentation LegalAgent EnglishLanguage \"A &%LegalAgent is an &%Agent that is allowed by law to to act and be treated as a legal person for certain purposes, such as being a party to a lawsuit, owning property, and entering into a contract. Typically, a &%LegalAgent is either an adult &%Human or some type of &%Organization. Depending on the prevailing legal system in a given time and location, &%Humans in general, as well as other &%CognitiveAgents, typically will have additional legal rights and obligations beyond those accorded to &%LegalAgents. See the Wikipedia description of <a href=http://en.wikipedia.org/wiki/Juristic_person>Juristic person</a>.\")").
% :- load_kif("; NS: add.").
:- load_kif("(<=> (holdsDuring ?TIME (instance ?AGENT LegalAgent)) (holdsDuring ?TIME (or (capability LegalAction agent ?AGENT) (capability LegalAction patient ?AGENT))))").
:- load_kif("(instance leader BinaryPredicate)").
:- load_kif("(instance leader AsymmetricRelation)").
:- load_kif("(instance leader SingleValuedRelation)").
:- load_kif("(domain leader 1 Agent)").
:- load_kif("(domain leader 2 Human)").
:- load_kif("(documentation leader EnglishLanguage \"(&%leader ?INSTITUTION ?PERSON) means that the leader of ?INSTITUTION is ?PERSON.\")").
:- load_kif("(=> (holdsDuring ?TIME (leader ?X ?Y)) (holdsDuring ?TIME (attribute ?Y Living)))").
:- load_kif("(subclass Process Physical)").
:- load_kif("(documentation Process EnglishLanguage \"The class of things that happen and have temporal parts or stages. Examples include extended events like a football match or a race, actions like &%Pursuing and &%Reading, and biological processes. The formal definition is: anything that occurs in time but is not an &%Object. Note that a &%Process may have participants 'inside' it which are &%Objects, such as the players in a football match. In a 4D ontology, a &%Process is something whose spatiotemporal extent is thought of as dividing into temporal stages roughly perpendicular to the time-axis.\")").
:- load_kif("(subclass DualObjectProcess Process)").
:- load_kif("(documentation DualObjectProcess EnglishLanguage \"Any &%Process that requires two, nonidentical &%patients.\")").
:- load_kif("(=> (instance ?PROCESS DualObjectProcess) (exists (?OBJ1 ?OBJ2) (and (patient ?PROCESS ?OBJ1) (patient ?PROCESS ?OBJ2) (not (equal ?OBJ1 ?OBJ2)))))").
:- load_kif("(subclass SingleAgentProcess Process)").
:- load_kif("(documentation SingleAgentProcess EnglishLanguage \"&%SingleAgentProcess is the &%Class of all &%Processes that require exactly one &%agent in order to occur.\")").
:- load_kif("(=> (instance ?PROC SingleAgentProcess) (exists (?AGENT) (agent ?PROC ?AGENT)))").
:- load_kif("(=> (and (instance ?PROC SingleAgentProcess) (agent ?PROC ?AGENT_1) (agent ?PROC ?AGENT_2)) (equal ?AGENT_1 ?AGENT_2))").
:- load_kif("(subclass Abstract Entity)").
:- load_kif("(disjointDecomposition Abstract Quantity Attribute SetOrClass Relation Proposition)").
:- load_kif("(documentation Abstract EnglishLanguage \"Properties or qualities as distinguished from any particular embodiment of the properties/qualities in a physical medium. Instances of Abstract can be said to exist in the same sense as mathematical objects such as sets and relations, but they cannot exist at a particular place and time without some physical encoding or embodiment.\")").
% :- load_kif("; Something is Abstract just in case it has neither a spatial nor temporal location.").
:- load_kif("(<=> (instance ?ABS Abstract) (not (exists (?POINT) (or (located ?ABS ?POINT) (time ?ABS ?POINT)))))").
:- load_kif("(subclass Quantity Abstract)").
:- load_kif("(documentation Quantity EnglishLanguage \"Any specification of how many or how much of something there is. Accordingly, there are two subclasses of &%Quantity: &%Number (how many) and &%PhysicalQuantity (how much).\")").
:- load_kif("(subclass Attribute Abstract)").
:- load_kif("(partition Attribute InternalAttribute RelationalAttribute PerceptualAttribute)").
:- load_kif("(documentation Attribute EnglishLanguage \"Qualities which we cannot or choose not to reify into subclasses of &%Object.\")").
:- load_kif("(instance property BinaryPredicate)").
:- load_kif("(domain property 1 Entity)").
:- load_kif("(domain property 2 Attribute)").
:- load_kif("(documentation property EnglishLanguage \"This &%Predicate holds between an instance of &%Entity and an instance of &%Attribute. (&%property ?ENTITY ?ATTR) means that ?ENTITY has the &%Attribute ?ATTR.\")").
:- load_kif("(instance attribute AsymmetricRelation)").
:- load_kif("(instance attribute IrreflexiveRelation)").
:- load_kif("(subrelation attribute property)").
:- load_kif("(domain attribute 1 Object)").
:- load_kif("(documentation attribute EnglishLanguage \"(&%attribute ?OBJECT ?PROPERTY) means that ?PROPERTY is a &%Attribute of ?OBJECT. For example,"). (&%attribute &%MyLittleRedWagon &%Red).\")").
:- load_kif("(instance manner AsymmetricRelation)").
:- load_kif("(instance manner IrreflexiveRelation)").
:- load_kif("(subrelation manner property)").
:- load_kif("(domain manner 1 Process)").
:- load_kif("(disjointRelation manner attribute)").
:- load_kif("(documentation manner EnglishLanguage \"(&%manner ?PROCESS ?MANNER) means that the &%Process ?PROCESS is qualified by the &%Attribute ?MANNER. The &%Attributes of &%Processes are usually denoted by adverbs and include things like the speed of the wind, the style of a dance, or the intensity of a sports competition.\")").
% :- load_kif("; Seldom used function that appears to cause a very complex contradiction").
% :- load_kif(" (instance AbstractionFn UnaryFunction) (instance AbstractionFn PartialValuedRelation) (domain AbstractionFn 1 Class) (range AbstractionFn Attribute) (documentation AbstractionFn EnglishLanguage \"A &%UnaryFunction that maps a &%Class into the instance of &%Attribute that specifies the condition(s) for membership in the &%Class.\")").
% :- load_kif(" (<=> (equal (AbstractionFn ?CLASS) ?ATTR) (forall (?INST) (<=> (instance ?INST ?CLASS) (property ?INST ?ATTR))))").
:- load_kif("(instance ExtensionFn UnaryFunction)").
:- load_kif("(instance ExtensionFn PartialValuedRelation)").
:- load_kif("(domain ExtensionFn 1 Attribute)").
:- load_kif("(range ExtensionFn Class)").
:- load_kif("(documentation ExtensionFn EnglishLanguage \"A &%UnaryFunction that maps an &%Attribute into the &%Class whose condition for membership is the &%Attribute.\")").
% :- load_kif(" (<=> (equal (ExtensionFn ?ATTRIBUTE) ?CLASS) (equal (AbstractionFn ?CLASS) ?ATTRIBUTE))").
:- load_kif("(subclass InternalAttribute Attribute)").
:- load_kif("(documentation InternalAttribute EnglishLanguage \"Any &%Attribute of an &%Entity that is an internal property of the &%Entity, e.g. its shape, its color, its fragility, etc.\")").
:- load_kif("(documentation PhysicalAttribute EnglishLanguage \"An &%InternalAttribute given by physical properties of the object.\")").
:- load_kif("(subclass PhysicalAttribute InternalAttribute)").
:- load_kif("(subclass RelationalAttribute Attribute)").
:- load_kif("(documentation RelationalAttribute EnglishLanguage \"Any &%Attribute that an &%Entity has by virtue of a relationship that it bears to another &%Entity or set of &%Entities, e.g. &%SocialRoles and &%PositionalAttributes.\")").
% :- load_kif("; The following formulas incorporate the Number hierarchy from the ontology 'kif-numbers' on the Ontolingua server.").
:- load_kif("(subclass Number Quantity)").
:- load_kif("(partition Number RealNumber ImaginaryNumber ComplexNumber)").
:- load_kif("(documentation Number EnglishLanguage \"A measure of how many things there are, or how much there is, of a certain kind. &%Numbers are subclassed into &%RealNumber, &%ComplexNumber, and &%ImaginaryNumber.\")").
:- load_kif("(instance lessThan BinaryPredicate)").
:- load_kif("(instance lessThan TransitiveRelation)").
:- load_kif("(instance lessThan IrreflexiveRelation)").
:- load_kif("(instance lessThan RelationExtendedToQuantities)").
:- load_kif("(trichotomizingOn lessThan RealNumber)").
:- load_kif("(domain lessThan 1 Quantity)").
:- load_kif("(domain lessThan 2 Quantity)").
:- load_kif("(documentation lessThan EnglishLanguage \"(&%lessThan ?NUMBER1 ?NUMBER2) is true just in case the &%Quantity ?NUMBER1 is less than the &%Quantity ?NUMBER2.\")").
:- load_kif("(instance greaterThan BinaryPredicate)").
:- load_kif("(instance greaterThan TransitiveRelation)").
:- load_kif("(instance greaterThan IrreflexiveRelation)").
:- load_kif("(instance greaterThan RelationExtendedToQuantities)").
:- load_kif("(trichotomizingOn greaterThan RealNumber)").
:- load_kif("(domain greaterThan 1 Quantity)").
:- load_kif("(domain greaterThan 2 Quantity)").
:- load_kif("(inverse greaterThan lessThan)").
:- load_kif("(documentation greaterThan EnglishLanguage \"(&%greaterThan ?NUMBER1 ?NUMBER2) is true just in case the &%Quantity ?NUMBER1 is greater than the &%Quantity ?NUMBER2.\")").
:- load_kif("(instance lessThanOrEqualTo BinaryPredicate)").
:- load_kif("(instance lessThanOrEqualTo PartialOrderingRelation)").
:- load_kif("(instance lessThanOrEqualTo RelationExtendedToQuantities)").
:- load_kif("(trichotomizingOn lessThanOrEqualTo RealNumber)").
:- load_kif("(domain lessThanOrEqualTo 1 Quantity)").
:- load_kif("(domain lessThanOrEqualTo 2 Quantity)").
:- load_kif("(documentation lessThanOrEqualTo EnglishLanguage \"(&%lessThanOrEqualTo ?NUMBER1 ?NUMBER2) is true just in case the &%Quantity ?NUMBER1 is less than or equal to the &%Quantity ?NUMBER2.\")").
:- load_kif("(<=> (lessThanOrEqualTo ?NUMBER1 ?NUMBER2) (or (equal ?NUMBER1 ?NUMBER2) (lessThan ?NUMBER1 ?NUMBER2)))").
:- load_kif("(instance greaterThanOrEqualTo BinaryPredicate)").
:- load_kif("(instance greaterThanOrEqualTo PartialOrderingRelation)").
:- load_kif("(instance greaterThanOrEqualTo RelationExtendedToQuantities)").
:- load_kif("(trichotomizingOn greaterThanOrEqualTo RealNumber)").
:- load_kif("(domain greaterThanOrEqualTo 1 Quantity)").
:- load_kif("(domain greaterThanOrEqualTo 2 Quantity)").
:- load_kif("(inverse greaterThanOrEqualTo lessThanOrEqualTo)").
:- load_kif("(documentation greaterThanOrEqualTo EnglishLanguage \"(&%greaterThanOrEqualTo ?NUMBER1 ?NUMBER2) is true just in case the &%Quantity ?NUMBER1 is greater than the &%Quantity ?NUMBER2.\")").
:- load_kif("(<=> (greaterThanOrEqualTo ?NUMBER1 ?NUMBER2) (or (equal ?NUMBER1 ?NUMBER2) (greaterThan ?NUMBER1 ?NUMBER2)))").
:- load_kif("(subclass RealNumber Number)").
:- load_kif("(partition RealNumber NegativeRealNumber NonnegativeRealNumber)").
:- load_kif("(partition RealNumber RationalNumber IrrationalNumber)").
:- load_kif("(documentation RealNumber EnglishLanguage \"Any &%Number that can be expressed as a").
:- load_kif("(possibly infinite) decimal, i.e. any &%Number that has a position on the number line.\")").
:- load_kif("(subclass ImaginaryNumber Number)").
:- load_kif("(documentation ImaginaryNumber EnglishLanguage \"Any &%Number that is the result of multiplying a &%RealNumber by the square root of -1.\")").
:- load_kif("(=> (instance ?NUMBER ImaginaryNumber) (exists (?REAL) (and (instance ?REAL RealNumber) (equal ?NUMBER (MultiplicationFn ?REAL (SquareRootFn -1))))))").
:- load_kif("(subclass RationalNumber RealNumber)").
:- load_kif("(documentation RationalNumber EnglishLanguage \"Any &%RealNumber that is the product of dividing two &%Integers.\")").
:- load_kif("(subclass IrrationalNumber RealNumber)").
:- load_kif("(documentation IrrationalNumber EnglishLanguage \"Any &%RealNumber that is not also a &%RationalNumber.\")").
:- load_kif("(subclass NonnegativeRealNumber RealNumber)").
:- load_kif("(documentation NonnegativeRealNumber EnglishLanguage \"A &%RealNumber that is greater than or equal to zero.\")").
:- load_kif("(<=> (instance ?NUMBER NonnegativeRealNumber) (and (greaterThanOrEqualTo ?NUMBER 0) (instance ?NUMBER RealNumber)))").
:- load_kif("(subclass PositiveRealNumber NonnegativeRealNumber)").
:- load_kif("(documentation PositiveRealNumber EnglishLanguage \"A &%RealNumber that is greater than zero.\")").
:- load_kif("(<=> (instance ?NUMBER PositiveRealNumber) (and (greaterThan ?NUMBER 0) (instance ?NUMBER RealNumber)))").
:- load_kif("(subclass NegativeRealNumber RealNumber)").
:- load_kif("(documentation NegativeRealNumber EnglishLanguage \"A &%RealNumber that is less than zero.\")").
:- load_kif("(<=> (instance ?NUMBER NegativeRealNumber) (and (lessThan ?NUMBER 0) (instance ?NUMBER RealNumber)))").
:- load_kif("(subclass Integer RationalNumber)").
:- load_kif("(partition Integer OddInteger EvenInteger)").
:- load_kif("(partition Integer NegativeInteger NonnegativeInteger)").
:- load_kif("(documentation Integer EnglishLanguage \"A negative or nonnegative whole number.\")").
:- load_kif("(subclass EvenInteger Integer)").
:- load_kif("(documentation EvenInteger EnglishLanguage \"An &%Integer that is evenly divisible by 2.\")").
:- load_kif("(subclass OddInteger Integer)").
:- load_kif("(documentation OddInteger EnglishLanguage \"An &%Integer that is not evenly divisible by 2.\")").
:- load_kif("(subclass PrimeNumber Integer)").
:- load_kif("(documentation PrimeNumber EnglishLanguage \"An &%Integer that is evenly divisible only by itself and 1.\")").
:- load_kif("(subclass NonnegativeInteger Integer)").
:- load_kif("(subclass NonnegativeInteger NonnegativeRealNumber)").
:- load_kif("(documentation NonnegativeInteger EnglishLanguage \"An &%Integer that is greater than or equal to zero.\")").
:- load_kif("(=> (instance ?X NonnegativeInteger) (greaterThan ?X -1))").
:- load_kif("(subclass NegativeInteger Integer)").
:- load_kif("(subclass NegativeInteger NegativeRealNumber)").
:- load_kif("(documentation NegativeInteger EnglishLanguage \"An &%Integer that is less than zero.\")").
:- load_kif("(=> (instance ?X NegativeInteger) (greaterThan 0 ?X))").
:- load_kif("(subclass PositiveInteger NonnegativeInteger)").
:- load_kif("(subclass PositiveInteger PositiveRealNumber)").
:- load_kif("(documentation PositiveInteger EnglishLanguage \"An &%Integer that is greater than zero.\")").
:- load_kif("(=> (instance ?X PositiveInteger) (greaterThan ?X 0))").
:- load_kif("(subclass BinaryNumber RealNumber)").
:- load_kif("(documentation BinaryNumber EnglishLanguage \"Elements from the number system with base 2. Every &%BinaryNumber is expressed as a sequence of the digits 1 and 0.\")").
:- load_kif("(subclass ComplexNumber Number)").
:- load_kif("(disjoint ComplexNumber RealNumber)").
:- load_kif("(documentation ComplexNumber EnglishLanguage \"A &%Number that has the form: x + yi, where x and y are &%RealNumbers and i is the square root of -1.\")").
:- load_kif("(=> (instance ?NUMBER ComplexNumber) (exists (?REAL1 ?REAL2) (and (instance ?REAL1 RealNumber) (instance ?REAL2 RealNumber) (equal ?NUMBER (AdditionFn ?REAL1 (MultiplicationFn ?REAL2 (SquareRootFn -1)))))))").
:- load_kif("(subclass PhysicalQuantity Quantity)").
:- load_kif("(partition PhysicalQuantity ConstantQuantity FunctionQuantity)").
:- load_kif("(documentation PhysicalQuantity EnglishLanguage \"A &%PhysicalQuantity is a measure of some quantifiable aspect of the modeled world, such as 'the earth's diameter' (a constant length) and 'the stress in a loaded deformable solid' (a measure of stress, which is a function of three spatial coordinates). Every &%PhysicalQuantity is either a &%ConstantQuantity or &%FunctionQuantity. Instances of &%ConstantQuantity are dependent on a &%UnitOfMeasure, while instances of &%FunctionQuantity are &%Functions that map instances of &%ConstantQuantity to other instances of &%ConstantQuantity (e.g., a &%TimeDependentQuantity is a &%FunctionQuantity). Although the name and definition of &%PhysicalQuantity is borrowed from physics, a &%PhysicalQuantity need not be material. Aside from the dimensions of length, time, velocity, etc., nonphysical dimensions such as currency are also possible. Accordingly, amounts of money would be instances of &%PhysicalQuantity. A &%PhysicalQuantity is distinguished from a pure &%Number by the fact that the former is associated with a dimension of measurement.\")").
:- load_kif("(subclass ConstantQuantity PhysicalQuantity)").
:- load_kif("(documentation ConstantQuantity EnglishLanguage \"A &%ConstantQuantity is a &%PhysicalQuantity that has a constant value, e.g. 3 &%Meters and 5 &%HourDurations. The magnitude (see &%MagnitudeFn) of every &%ConstantQuantity is a &%RealNumber. &%ConstantQuantity is distinguished from &%FunctionQuantity, in that each instance of the latter is formed through the mapping of one &%PhysicalQuantity to another &%PhysicalQuantity. Each instance of &%ConstantQuantity is expressed with the &%BinaryFunction &%MeasureFn, which takes a &%Number and a &%UnitOfMeasure as arguments. For example, 3 &%Meters is expressed as (&%MeasureFn 3 &%Meter). Instances of &%ConstantQuantity form a partial order (see &%PartialOrderingRelation) with the &%lessThan relation, since &%lessThan is a &%RelationExtendedToQuantities and &%lessThan is defined over the &%RealNumbers. The &%lessThan relation is not a total order (see &%TotalOrderingRelation) over the class &%ConstantQuantity since elements of some subclasses of &%ConstantQuantity (such as length quantities) are incomparable to elements of other subclasses of &%ConstantQuantity").
:- load_kif("(such as mass quantities).\")").
:- load_kif("(subclass TimeMeasure ConstantQuantity)").
:- load_kif("(documentation TimeMeasure EnglishLanguage \"The class of temporal durations (instances of &%TimeDuration) and positions of &%TimePoints and &%TimeIntervals along the universal timeline (instances of &%TimePosition).\")").
:- load_kif("(subclass TimeDuration TimeMeasure)").
:- load_kif("(documentation TimeDuration EnglishLanguage \"Any measure of length of time, with or without respect to the universal timeline.\")").
:- load_kif("(subclass TimePosition TimeMeasure)").
:- load_kif("(partition TimePosition TimeInterval TimePoint)").
:- load_kif("(documentation TimePosition EnglishLanguage \"Any &%TimePoint or &%TimeInterval along the universal timeline from &%NegativeInfinity to &%PositiveInfinity.\")").
:- load_kif("(subclass TimeInterval TimePosition)").
:- load_kif("(documentation TimeInterval EnglishLanguage \"An interval of time. Note that a &%TimeInterval has both an extent and a location on the universal timeline. Note too that a &%TimeInterval has no gaps, i.e. this class contains only convex time intervals.\")").
:- load_kif("(subclass TimePoint TimePosition)").
:- load_kif("(documentation TimePoint EnglishLanguage \"An extensionless point on the universal timeline. The &%TimePoints at which &%Processes occur can be known with various degrees of precision and approximation, but conceptually &%TimePoints are point-like and not interval-like. That is, it doesn't make sense to talk about how long a &%TimePoint lasts.\")").
:- load_kif("(subclass FunctionQuantity PhysicalQuantity)").
:- load_kif("(documentation FunctionQuantity EnglishLanguage \"A &%FunctionQuantity is a &%PhysicalQuantity that is returned by a &%Function that maps from one or more instances of &%ConstantQuantity to another instance of &%ConstantQuantity. For example, the velocity of a particle would be represented by a &%FunctionQuantity relating values of time (which are instances of &%ConstantQuantity) to values of distance").
:- load_kif("(also instances of &%ConstantQuantity). Note that all elements of the range of the &%Function corresponding to a &%FunctionQuantity have the same physical dimension as the &%FunctionQuantity itself.\")").
:- load_kif("(subclass UnaryConstantFunctionQuantity FunctionQuantity)").
:- load_kif("(documentation UnaryConstantFunctionQuantity EnglishLanguage \"A &%subclass of &%FunctionQuantity, instances of which are returned by &%UnaryFunctions that map from one instance of the &%Class &%ConstantQuantity to another instance of the &%Class &%ConstantQuantity.\")").
:- load_kif("(subclass TimeDependentQuantity UnaryConstantFunctionQuantity)").
:- load_kif("(documentation TimeDependentQuantity EnglishLanguage \"A &%UnaryConstantFunctionQuantity of continuous time. All instances of this &%Class are returned by &%Functions that map a time quantity into another &%ConstantQuantity such as temperature. For example, 'the temperature at the top of the Empire State Building' is a &%TimeDependentQuantity, since its value depends on the time.\")").
:- load_kif("(subclass SetOrClass Abstract)").
:- load_kif("(partition SetOrClass Set Class)").
:- load_kif("(documentation SetOrClass EnglishLanguage \"The &%SetOrClass of &%Sets and &%Classes, i.e. any instance of &%Abstract that has &%elements or &%instances.\")").
:- load_kif("(subclass Class SetOrClass)").
:- load_kif("(documentation Class EnglishLanguage \"&%Classes differ from &%Sets in three important respects. First, &%Classes are not assumed to be extensional. That is, distinct &%Classes might well have exactly the same instances. Second, &%Classes typically have an associated `condition' that determines the instances of the &%Class. So, for example, the condition `human' determines the &%Class of &%Humans. Note that some &%Classes might satisfy their own condition (e.g., the &%Class of &%Abstract things is &%Abstract) and hence be instances of themselves. Third, the instances of a class may occur only once within the class, i.e. a class cannot contain duplicate instances.\")").
:- load_kif("(subclass Set SetOrClass)").
:- load_kif("(documentation Set EnglishLanguage \"A &%SetOrClass that satisfies extensionality as well as other constraints specified by some choice of set theory. &%Sets differ from &%Classes in two important respects. First, &%Sets are extensional - two &%Sets with the same &%elements are identical. Second, a &%Set can be an arbitrary stock of objects. That is, there is no requirement that &%Sets have an associated condition that determines their membership. Note that &%Sets are not assumed to be unique sets, i.e. &%elements of a &%Set may occur more than once in the &%Set.\")").
:- load_kif("(subclass Relation Abstract)").
:- load_kif("(disjointDecomposition Relation BinaryRelation TernaryRelation QuaternaryRelation QuintaryRelation VariableArityRelation)").
:- load_kif("(partition Relation Predicate Function List)").
:- load_kif("(partition Relation TotalValuedRelation PartialValuedRelation)").
:- load_kif("(documentation Relation EnglishLanguage \"The &%Class of relations. There are three kinds of &%Relation: &%Predicate, &%Function, and &%List. &%Predicates and &%Functions both denote sets of ordered n-tuples. The difference between these two &%Classes is that &%Predicates cover formula-forming operators, while &%Functions cover term-forming operators. A &%List, on the other hand, is a particular ordered n-tuple.\")").
% :- load_kif("; The following part of the ontology covers the various classes under 'Relation'. Most of the content here is taken from frame-ontology, abstract-algebra, kif-relations, and kif-extensions (ontologies available on the Ontolingua server).").
:- load_kif("(subclass SingleValuedRelation Relation)").
:- load_kif("(subclass SingleValuedRelation InheritableRelation)").
:- load_kif("(documentation SingleValuedRelation EnglishLanguage \"A &%Relation is a &%SingleValuedRelation just in case an assignment of values to every argument position except the last one determines at most one assignment for the last argument position. Note that not all &%SingleValuedRelations are &%TotalValuedRelations.\")").
:- load_kif("(=> (instance ?REL SingleValuedRelation) (forall (@ROW ?ITEM1 ?ITEM2) (=> (and (?REL @ROW ?ITEM1) (?REL @ROW ?ITEM2)) (equal ?ITEM1 ?ITEM2))))").
:- load_kif("(subclass TotalValuedRelation Relation)").
:- load_kif("(subclass TotalValuedRelation InheritableRelation)").
:- load_kif("(documentation TotalValuedRelation EnglishLanguage \"A &%Relation is a &%TotalValuedRelation just in case there exists an assignment for the last argument position of the &%Relation given any assignment of values to every argument position except the last one. Note that declaring a &%Relation to be both a &%TotalValuedRelation and a &%SingleValuedRelation means that it is a total function.\")").
:- load_kif("(<=> (and (instance ?REL TotalValuedRelation) (instance ?REL Predicate)) (exists (?VALENCE) (and (instance ?REL Relation) (valence ?REL ?VALENCE) (=> (forall (?NUMBER ?ELEMENT ?CLASS) (=> (and (lessThan ?NUMBER ?VALENCE) (domain ?REL ?NUMBER ?CLASS) (equal ?ELEMENT (ListOrderFn (ListFn @ROW) ?NUMBER))) (instance ?ELEMENT ?CLASS))) (exists (?ITEM) (?REL @ROW ?ITEM))))))").
:- load_kif("(subclass PartialValuedRelation Relation)").
:- load_kif("(documentation PartialValuedRelation EnglishLanguage \"A &%Relation is a &%PartialValuedRelation just in case it is not a &%TotalValuedRelation, i.e. just in case assigning values to every argument position except the last one does not necessarily mean that there is a value assignment for the last argument position. Note that, if a &%Relation is both a &%PartialValuedRelation and a &%SingleValuedRelation, then it is a partial function.\")").
:- load_kif("(subclass BinaryRelation Relation)").
:- load_kif("(subclass BinaryRelation InheritableRelation)").
:- load_kif("(documentation BinaryRelation EnglishLanguage \"&%BinaryRelations are relations that are true only of pairs of things. &%BinaryRelations are represented as slots in frame systems.\")").
:- load_kif("(subclass ReflexiveRelation BinaryRelation)").
:- load_kif("(documentation ReflexiveRelation EnglishLanguage \"&%Relation ?REL is reflexive iff").
:- load_kif("(?REL ?INST ?INST) for all ?INST.\")").
:- load_kif("(<=> (instance ?REL ReflexiveRelation) (?REL ?INST ?INST))").
:- load_kif("(subclass IrreflexiveRelation BinaryRelation)").
:- load_kif("(documentation IrreflexiveRelation EnglishLanguage \"&%Relation ?REL is irreflexive iff (?REL ?INST ?INST) holds for no value of ?INST.\")").
:- load_kif("(<=> (instance ?REL IrreflexiveRelation) (forall (?INST) (not (?REL ?INST ?INST))))").
:- load_kif("(subclass SymmetricRelation BinaryRelation)").
:- load_kif("(documentation SymmetricRelation EnglishLanguage \"A &%BinaryRelation ?REL is symmetric just iff (?REL ?INST1 ?INST2) imples (?REL ?INST2 ?INST1), for all ?INST1 and ?INST2.\")").
:- load_kif("(<=> (instance ?REL SymmetricRelation) (forall (?INST1 ?INST2) (=> (?REL ?INST1 ?INST2) (?REL ?INST2 ?INST1))))").
:- load_kif("(subclass AsymmetricRelation IrreflexiveRelation)").
:- load_kif("(subclass AsymmetricRelation AntisymmetricRelation)").
:- load_kif("(documentation AsymmetricRelation EnglishLanguage \"A &%BinaryRelation is asymmetric if and only if it is both an &%AntisymmetricRelation and an &%IrreflexiveRelation.\")").
:- load_kif("(<=> (instance ?REL AsymmetricRelation) (and (instance ?REL AntisymmetricRelation) (instance ?REL IrreflexiveRelation)))").
:- load_kif("(subclass AntisymmetricRelation BinaryRelation)").
:- load_kif("(documentation AntisymmetricRelation EnglishLanguage \"&%BinaryRelation ?REL is an &%AntisymmetricRelation if for distinct ?INST1 and ?INST2, (?REL ?INST1 ?INST2) implies not (?REL ?INST2 ?INST1). In other words, for all ?INST1 and ?INST2, (?REL ?INST1 ?INST2) and (?REL ?INST2 ?INST1) imply that ?INST1 and ?INST2 are identical. Note that it is possible for an &%AntisymmetricRelation to be a &%ReflexiveRelation.\")").
:- load_kif("(<=> (instance ?REL AntisymmetricRelation) (forall (?INST1 ?INST2) (=> (and (?REL ?INST1 ?INST2) (?REL ?INST2 ?INST1)) (equal ?INST1 ?INST2))))").
:- load_kif("(subclass TrichotomizingRelation BinaryRelation)").
:- load_kif("(documentation TrichotomizingRelation EnglishLanguage \"A &%BinaryRelation ?REL is a &%TrichotomizingRelation just in case all ordered pairs consisting of distinct individuals are elements of ?REL.\")").
:- load_kif("(<=> (instance ?REL TrichotomizingRelation) (forall (?INST1 ?INST2) (or (and (?REL ?INST1 ?INST2) (not (equal ?INST1 ?INST2)) (not (?REL ?INST2 ?INST1))) (and (not (?REL ?INST1 ?INST2)) (equal ?INST1 ?INST2) (not (?REL ?INST2 ?INST1))) (and (not (?REL ?INST1 ?INST2)) (not (equal ?INST1 ?INST2)) (?REL ?INST2 ?INST1)))))").
:- load_kif("(subclass TransitiveRelation BinaryRelation)").
:- load_kif("(documentation TransitiveRelation EnglishLanguage \"A &%BinaryRelation ?REL is transitive if (?REL ?INST1 ?INST2) and (?REL ?INST2 ?INST3) imply (?REL ?INST1 ?INST3), for all ?INST1, ?INST2, and ?INST3.\")").
:- load_kif("(<=> (instance ?REL TransitiveRelation) (forall (?INST1 ?INST2 ?INST3) (=> (and (?REL ?INST1 ?INST2) (?REL ?INST2 ?INST3)) (?REL ?INST1 ?INST3))))").
:- load_kif("(subclass IntransitiveRelation BinaryRelation)").
:- load_kif("(documentation IntransitiveRelation EnglishLanguage \"A &%BinaryRelation ?REL is intransitive only if (?REL ?INST1 ?INST2) and (?REL ?INST2 ?INST3) imply not").
:- load_kif("(?REL ?INST1 ?INST3), for all ?INST1, ?INST2, and ?INST3.\")").
:- load_kif("(<=> (instance ?REL IntransitiveRelation) (forall (?INST1 ?INST2 ?INST3) (=> (and (?REL ?INST1 ?INST2) (?REL ?INST2 ?INST3)) (not (?REL ?INST1 ?INST3)))))").
:- load_kif("(subclass PartialOrderingRelation TransitiveRelation)").
:- load_kif("(subclass PartialOrderingRelation AntisymmetricRelation)").
:- load_kif("(subclass PartialOrderingRelation ReflexiveRelation)").
:- load_kif("(documentation PartialOrderingRelation EnglishLanguage \"A &%BinaryRelation is a partial ordering if it is a &%ReflexiveRelation, an &%AntisymmetricRelation, and a &%TransitiveRelation.\")").
:- load_kif("(subclass TotalOrderingRelation PartialOrderingRelation)").
:- load_kif("(subclass TotalOrderingRelation TrichotomizingRelation)").
:- load_kif("(documentation TotalOrderingRelation EnglishLanguage \"A &%BinaryRelation is a &%TotalOrderingRelation if it is a &%PartialOrderingRelation and a &%TrichotomizingRelation.\")").
:- load_kif("(<=> (instance ?REL TotalOrderingRelation) (forall (?INST1 ?INST2) (and (or (?REL ?INST1 ?INST2) (?REL ?INST2 ?INST1)) (or (not (?REL ?INST1 ?INST2)) (not (?REL ?INST2 ?INST1))))))").
:- load_kif("(subclass EquivalenceRelation TransitiveRelation)").
:- load_kif("(subclass EquivalenceRelation SymmetricRelation)").
:- load_kif("(subclass EquivalenceRelation ReflexiveRelation)").
:- load_kif("(documentation EquivalenceRelation EnglishLanguage \"A &%BinaryRelation is an equivalence relation if it is a &%ReflexiveRelation, a &%SymmetricRelation, and a &%TransitiveRelation.\")").
:- load_kif("(subclass CaseRole BinaryPredicate)").
:- load_kif("(subclass CaseRole InheritableRelation)").
:- load_kif("(subclass CaseRole AsymmetricRelation)").
:- load_kif("(documentation CaseRole EnglishLanguage \"The &%Class of &%Predicates relating the spatially distinguished parts of a &%Process. &%CaseRoles include, for example, the &%agent, &%patient or &%destination of an action, the flammable substance in a burning process, or the water that falls in rain.\")").
:- load_kif("(documentation involvedInEvent EnglishLanguage \"(involvedInEvent ?EVENT ?THING) means that in the &%Process ?EVENT, the &%Entity ?THING plays some &%CaseRole.\")").
:- load_kif("(instance involvedInEvent BinaryPredicate)").
:- load_kif("(instance involvedInEvent AsymmetricRelation)").
:- load_kif("(domain involvedInEvent 1 Process)").
:- load_kif("(domain involvedInEvent 2 Entity)").
% :- load_kif("; AP - this axiom below seems questionable, what about relations we just haven't \"discovered\" yet. ").
% :- load_kif("(=> (involvedInEvent ?E ?T) (exists (?R) (and (instance ?R CaseRole) (subrelation ?R involvedInEvent) (?R ?E ?T))))").
:- load_kif("(=> (instance ?R CaseRole) (subrelation ?R involvedInEvent))").
:- load_kif("(instance agent CaseRole)").
:- load_kif("(domain agent 1 Process)").
:- load_kif("(domain agent 2 Agent)").
:- load_kif("(subrelation agent involvedInEvent)").
:- load_kif("(documentation agent EnglishLanguage \"(&%agent ?PROCESS ?AGENT) means that ?AGENT is an active determinant, either animate or inanimate, of the &%Process ?PROCESS, with or without voluntary intention. For example, Eve is an &%agent in the following proposition: Eve bit an apple.\")").
:- load_kif("(instance destination CaseRole)").
:- load_kif("(domain destination 1 Process)").
:- load_kif("(domain destination 2 Entity)").
:- load_kif("(subrelation destination involvedInEvent)").
:- load_kif("(documentation destination EnglishLanguage \"(destination ?PROCESS ?GOAL) means that ?GOAL is the target or goal of the Process ?PROCESS. For example, Danbury would be the destination in the following proposition: Bob went to Danbury. Note that this is a very general &%CaseRole and, in particular, that it covers the concepts of 'recipient' and 'beneficiary'. Thus, John would be the &%destination in the following proposition: Tom gave a book to John.\")").
:- load_kif("(instance experiencer CaseRole)").
:- load_kif("(domain experiencer 1 Process)").
:- load_kif("(domain experiencer 2 Agent)").
:- load_kif("(subrelation experiencer involvedInEvent)").
:- load_kif("(documentation experiencer EnglishLanguage \"(&%experiencer ?PROCESS ?AGENT) means that ?AGENT experiences the &%Process ?PROCESS. For example, Yojo is the &%experiencer of seeing in the following proposition: Yojo sees the fish. Note that &%experiencer, unlike &%agent, does not entail a causal relation between its arguments.\")").
:- load_kif("(subrelation instrument patient)").
:- load_kif("(domain instrument 1 Process)").
:- load_kif("(domain instrument 2 Object)").
:- load_kif("(documentation instrument EnglishLanguage \"(instrument ?EVENT ?TOOL) means that ?TOOL is used by an agent in bringing about ?EVENT and that ?TOOL is not changed by ?EVENT. For example, the key is an &%instrument in the following proposition: The key opened the door. Note that &%instrument and &%resource cannot be satisfied by the same ordered pair.\")").
:- load_kif("(instance origin CaseRole)").
:- load_kif("(domain origin 1 Process)").
:- load_kif("(domain origin 2 Object)").
:- load_kif("(subrelation origin involvedInEvent)").
:- load_kif("(documentation origin EnglishLanguage \"(&%origin ?PROCESS ?SOURCE) means that ?SOURCE indicates where the ?Process began. Note that this relation implies that ?SOURCE is present at the beginning of the process, but need not participate throughout the process. For example, the submarine is the &%origin in the following proposition: the missile was launched from a submarine.\")").
:- load_kif("(instance patient CaseRole)").
:- load_kif("(domain patient 1 Process)").
:- load_kif("(domain patient 2 Entity)").
:- load_kif("(subrelation patient involvedInEvent)").
:- load_kif("(documentation patient EnglishLanguage \"(&%patient ?PROCESS ?ENTITY) means that ?ENTITY is a participant in ?PROCESS that may be moved, said, experienced, etc. For example, the direct objects in the sentences 'The cat swallowed the canary' and 'Billy likes the beer' would be examples of &%patients. Note that the &%patient of a &%Process may or may not undergo structural change as a result of the &%Process. The &%CaseRole of &%patient is used when one wants to specify as broadly as possible the object of a &%Process.\")").
:- load_kif("(subrelation resource patient)").
:- load_kif("(domain resource 1 Process)").
:- load_kif("(domain resource 2 Object)").
:- load_kif("(disjointRelation resource result)").
:- load_kif("(disjointRelation resource instrument)").
:- load_kif("(disjointRelation result instrument)").
:- load_kif("(documentation resource EnglishLanguage \"(&%resource ?PROCESS ?RESOURCE) means that ?RESOURCE is present at the beginning of ?PROCESS, is used by ?PROCESS, and as a consequence is changed by ?PROCESS. For example, soap is a &%resource in the following proposition: the gun was carved out of soap. Note that &%resource differs from &%instrument, another subrelation of &%patient, in that its internal or physical properties are altered in some way by the &%Process.\")").
:- load_kif("(subrelation result patient)").
:- load_kif("(domain result 1 Process)").
:- load_kif("(domain result 2 Entity)").
:- load_kif("(documentation result EnglishLanguage \"(result ?ACTION ?OUTPUT) means that ?OUTPUT is a product of ?ACTION. For example, house is a &%result in the following proposition: Eric built a house.\")").
:- load_kif("(subclass InheritableRelation Relation)").
:- load_kif("(documentation InheritableRelation EnglishLanguage \"The class of &%Relations whose properties can be inherited downward in the class hierarchy via the &%subrelation &%Predicate.\")").
:- load_kif("(subclass ProbabilityRelation Relation)").
:- load_kif("(subclass ProbabilityRelation InheritableRelation)").
:- load_kif("(documentation ProbabilityRelation EnglishLanguage \"The &%Class of &%Relations that permit assessment of the probability of an event or situation.\")").
:- load_kif("(instance ProbabilityFn ProbabilityRelation)").
:- load_kif("(instance ProbabilityFn TotalValuedRelation)").
:- load_kif("(instance ProbabilityFn UnaryFunction)").
:- load_kif("(domain ProbabilityFn 1 Formula)").
:- load_kif("(range ProbabilityFn RealNumber)").
:- load_kif("(instance ProbabilityFn AsymmetricRelation)").
:- load_kif("(documentation ProbabilityFn EnglishLanguage \"One of the basic &%ProbabilityRelations, &%ProbabilityFn is used to state the a priori probability of a state of affairs. (&%ProbabilityFn ?FORMULA) denotes the a priori probability of ?FORMULA.\")").
:- load_kif("(instance conditionalProbability ProbabilityRelation)").
:- load_kif("(instance conditionalProbability TernaryPredicate)").
:- load_kif("(domain conditionalProbability 1 Formula)").
:- load_kif("(domain conditionalProbability 2 Formula)").
:- load_kif("(domain conditionalProbability 3 RealNumber)").
:- load_kif("(documentation conditionalProbability EnglishLanguage \"One of the basic &%ProbabilityRelations. &%conditionalProbability is used to state the numeric value of a conditional probability. (&%conditionalProbability ?FORMULA1 ?FORMULA2 ?NUMBER) means that the probability of ?FORMULA2 being true given that ?FORMULA1 is true is ?NUMBER.\")").
:- load_kif("(instance increasesLikelihood ProbabilityRelation)").
:- load_kif("(instance increasesLikelihood BinaryPredicate)").
:- load_kif("(instance increasesLikelihood IrreflexiveRelation)").
:- load_kif("(domain increasesLikelihood 1 Formula)").
:- load_kif("(domain increasesLikelihood 2 Formula)").
:- load_kif("(disjointRelation increasesLikelihood decreasesLikelihood)").
:- load_kif("(disjointRelation increasesLikelihood independentProbability)").
:- load_kif("(disjointRelation decreasesLikelihood independentProbability)").
:- load_kif("(documentation increasesLikelihood EnglishLanguage \"One of the basic &%ProbabilityRelations.  (&%increasesLikelihood ?FORMULA1 ?FORMULA2) means that ?FORMULA2 is more likely to be true if ?FORMULA1 is true.\")").
:- load_kif("(=> (and (increasesLikelihood ?FORMULA1 ?FORMULA2) (equal (ProbabilityFn ?FORMULA2) ?NUMBER1) (conditionalProbability ?FORMULA1 ?FORMULA2 ?NUMBER2)) (greaterThan ?NUMBER2 ?NUMBER1))").
:- load_kif("(instance decreasesLikelihood ProbabilityRelation)").
:- load_kif("(instance decreasesLikelihood BinaryPredicate)").
:- load_kif("(instance decreasesLikelihood IrreflexiveRelation)").
:- load_kif("(domain decreasesLikelihood 1 Formula)").
:- load_kif("(domain decreasesLikelihood 2 Formula)").
:- load_kif("(documentation decreasesLikelihood EnglishLanguage \"One of the basic &%ProbabilityRelations.  (&%decreasesLikelihood ?FORMULA1 ?FORMULA2) means that ?FORMULA2 is less likely to be true if ?FORMULA1 is true.\")").
:- load_kif("(=> (and (decreasesLikelihood ?FORMULA1 ?FORMULA2) (equal (ProbabilityFn ?FORMULA2) ?NUMBER1) (conditionalProbability ?FORMULA1 ?FORMULA2 ?NUMBER2)) (lessThan ?NUMBER2 ?NUMBER1))").
:- load_kif("(instance independentProbability ProbabilityRelation)").
:- load_kif("(instance independentProbability BinaryPredicate)").
:- load_kif("(instance independentProbability SymmetricRelation)").
:- load_kif("(domain independentProbability 1 Formula)").
:- load_kif("(domain independentProbability 2 Formula)").
:- load_kif("(documentation independentProbability EnglishLanguage \"One of the basic &%ProbabilityRelations.  (&%independentProbability ?FORMULA1 ?FORMULA2) means that the probabilities of ?FORMULA1 and ?FORMULA2 being true are independent.\")").
:- load_kif("(=> (and (independentProbability ?FORMULA1 ?FORMULA2) (equal (ProbabilityFn ?FORMULA2) ?NUMBER1) (conditionalProbability ?FORMULA1 ?FORMULA2 ?NUMBER2)) (equal ?NUMBER2 ?NUMBER1))").
:- load_kif("(=> (and (instance ?FORMULA1 Formula) (instance ?FORMULA2 Formula)) (or (increasesLikelihood ?FORMULA1 ?FORMULA2) (decreasesLikelihood ?FORMULA1 ?FORMULA2) (independentProbability ?FORMULA1 ?FORMULA2)))").
:- load_kif("(subclass SpatialRelation Relation)").
:- load_kif("(subclass SpatialRelation InheritableRelation)").
:- load_kif("(documentation SpatialRelation EnglishLanguage \"The &%Class of &%Relations that are spatial in a wide sense. This &%Class includes mereological relations and topological relations.\")").
:- load_kif("(subclass TemporalRelation Relation)").
:- load_kif("(subclass TemporalRelation InheritableRelation)").
:- load_kif("(documentation TemporalRelation EnglishLanguage \"The &%Class of temporal &%Relations. This &%Class includes notions of (temporal) topology of intervals,").
:- load_kif("(temporal) schemata, and (temporal) extension.\")").
:- load_kif("(subclass IntentionalRelation InheritableRelation)").
:- load_kif("(subclass IntentionalRelation Relation)").
:- load_kif("(documentation IntentionalRelation EnglishLanguage \"The &%Class of &%Relations between an &%Agent and one or more &%Entities, where the &%Relation requires that the &%Agent have awareness of the &%Entity.\")").
:- load_kif("(=> (and (instance ?REL IntentionalRelation) (?REL ?AGENT @ROW) (inList ?OBJ (ListFn @ROW))) (inScopeOfInterest ?AGENT ?OBJ))").
:- load_kif("(instance prefers TernaryPredicate)").
:- load_kif("(instance prefers IntentionalRelation)").
:- load_kif("(domain prefers 1 CognitiveAgent)").
:- load_kif("(domain prefers 2 Formula)").
:- load_kif("(domain prefers 3 Formula)").
:- load_kif("(documentation prefers EnglishLanguage \"(&%prefers ?AGENT ?FORMULA1 ?FORMULA2) means that &%CognitiveAgent ?AGENT prefers the state of affairs expressed by ?FORMULA1 over the state of affairs expressed by ?FORMULA2 all things being equal.\")").
:- load_kif("(subclass PropositionalAttitude IntentionalRelation)").
:- load_kif("(subclass PropositionalAttitude AsymmetricRelation)").
:- load_kif("(subclass PropositionalAttitude InheritableRelation)").
:- load_kif("(documentation PropositionalAttitude EnglishLanguage \"The &%Class of &%IntentionalRelations where the &%Agent has awareness of a &%Proposition.\")").
:- load_kif("(=> (and (instance ?REL PropositionalAttitude) (?REL ?AGENT ?FORMULA)) (instance ?FORMULA Formula))").
:- load_kif("(subclass ObjectAttitude IntentionalRelation)").
:- load_kif("(subclass ObjectAttitude InheritableRelation)").
:- load_kif("(disjoint ObjectAttitude PropositionalAttitude)").
:- load_kif("(documentation ObjectAttitude EnglishLanguage \"The &%Class of &%IntentionalRelations where the &%Agent has awareness of an instance of &%Physical.\")").
:- load_kif("(=> (and (instance ?REL ObjectAttitude) (?REL ?AGENT ?THING)) (instance ?THING Physical))").
:- load_kif("(instance inScopeOfInterest BinaryPredicate)").
:- load_kif("(instance inScopeOfInterest IntentionalRelation)").
:- load_kif("(domain inScopeOfInterest 1 CognitiveAgent)").
:- load_kif("(domain inScopeOfInterest 2 Entity)").
:- load_kif("(documentation inScopeOfInterest EnglishLanguage \"A very general &%Predicate.  (&%inScopeOfInterest ?AGENT ?ENTITY) means that ?ENTITY is within the scope of interest of ?AGENT. Note that the interest indicated can be either positive or negative, i.e. the ?AGENT can have an interest in avoiding or promoting ?ENTITY.\")").
:- load_kif("(<=> (exists (?PROCESS) (and (instance ?PROCESS IntentionalProcess) (agent ?PROCESS ?AGENT) (patient ?PROCESS ?OBJECT))) (inScopeOfInterest ?AGENT ?OBJECT))").
:- load_kif("(instance needs ObjectAttitude)").
:- load_kif("(instance needs BinaryPredicate)").
:- load_kif("(subrelation needs inScopeOfInterest)").
:- load_kif("(domain needs 1 CognitiveAgent)").
:- load_kif("(domain needs 2 Physical)").
:- load_kif("(documentation needs EnglishLanguage \"(&%needs ?AGENT ?OBJECT) means that ?OBJECT is physically required for the continued existence of ?AGENT.\")").
:- load_kif("(=> (needs ?AGENT ?OBJECT) (wants ?AGENT ?OBJECT))").
:- load_kif("(instance wants ObjectAttitude)").
:- load_kif("(instance wants BinaryPredicate)").
:- load_kif("(subrelation wants inScopeOfInterest)").
:- load_kif("(relatedInternalConcept wants desires)").
:- load_kif("(domain wants 1 CognitiveAgent)").
:- load_kif("(domain wants 2 Physical)").
:- load_kif("(documentation wants EnglishLanguage \"(&%wants ?AGENT ?OBJECT) means that ?OBJECT is desired by ?AGENT, i.e. ?AGENT believes that ?OBJECT will satisfy one of its goals. Note that there is no implication that what is wanted by an agent is not already possessed by the agent.\")").
:- load_kif("(=> (wants ?AGENT ?OBJ) (exists (?PURP) (hasPurposeForAgent ?OBJ ?PURP ?AGENT)))").
:- load_kif("(=> (and (wants ?AGENT ?OBJ) (instance ?OBJ Object)) (desires ?AGENT (possesses ?AGENT ?OBJ)))").
:- load_kif("(instance desires PropositionalAttitude)").
:- load_kif("(instance desires BinaryPredicate)").
:- load_kif("(subrelation desires inScopeOfInterest)").
:- load_kif("(relatedInternalConcept desires wants)").
:- load_kif("(domain desires 1 CognitiveAgent)").
:- load_kif("(domain desires 2 Formula)").
:- load_kif("(documentation desires EnglishLanguage \"(&%desires ?AGENT ?FORMULA) means that ?AGENT wants to bring about the state of affairs expressed by ?FORMULA. Note that there is no implication that what is desired by the agent is not already true. Note too that &%desires is distinguished from &%wants only in that the former is a &%PropositionalAttitude, while &%wants is an &%ObjectAttitude.\")").
:- load_kif("(instance considers PropositionalAttitude)").
:- load_kif("(instance considers BinaryPredicate)").
:- load_kif("(subrelation considers inScopeOfInterest)").
:- load_kif("(domain considers 1 CognitiveAgent)").
:- load_kif("(domain considers 2 Formula)").
:- load_kif("(documentation considers EnglishLanguage \"(&%considers ?AGENT ?FORMULA) means that ?AGENT considers or wonders about the truth of the proposition expressed by ?FORMULA.\")").
:- load_kif("(instance believes PropositionalAttitude)").
:- load_kif("(instance believes BinaryPredicate)").
:- load_kif("(subrelation believes inScopeOfInterest)").
:- load_kif("(domain believes 1 CognitiveAgent)").
:- load_kif("(domain believes 2 Formula)").
:- load_kif("(documentation believes EnglishLanguage \"The epistemic predicate of belief.  (&%believes ?AGENT ?FORMULA) means that ?AGENT believes the proposition expressed by ?FORMULA.\")").
:- load_kif("(=> (believes ?AGENT ?FORMULA) (exists (?TIME) (holdsDuring ?TIME (considers ?AGENT ?FORMULA))))").
:- load_kif("(instance knows PropositionalAttitude)").
:- load_kif("(instance knows BinaryPredicate)").
:- load_kif("(subrelation knows inScopeOfInterest)").
:- load_kif("(domain knows 1 CognitiveAgent)").
:- load_kif("(domain knows 2 Formula)").
:- load_kif("(documentation knows EnglishLanguage \"The epistemic predicate of knowing. (&%knows ?AGENT ?FORMULA) means that ?AGENT knows the proposition expressed by ?FORMULA. Note that &%knows entails conscious awareness, so this &%Predicate cannot be used to express tacit or subconscious or unconscious knowledge.\")").
:- load_kif("(=> (knows ?AGENT ?FORMULA) (believes ?AGENT ?FORMULA))").
:- load_kif("(=> (knows ?AGENT ?FORMULA) (truth ?FORMULA True))").
:- load_kif("(subclass TernaryRelation Relation)").
:- load_kif("(subclass TernaryRelation InheritableRelation)").
:- load_kif("(documentation TernaryRelation EnglishLanguage \"&%TernaryRelations relate three items. The two &%subclasses of &%TernaryRelation are &%TernaryPredicate and &%BinaryFunction.\")").
:- load_kif("(subclass QuaternaryRelation Relation)").
:- load_kif("(subclass QuaternaryRelation InheritableRelation)").
:- load_kif("(documentation QuaternaryRelation EnglishLanguage \"&%QuaternaryRelations relate four items. The two &%subclasses of &%QuaternaryRelation are &%QuaternaryPredicate and &%TernaryFunction.\")").
:- load_kif("(subclass QuintaryRelation Relation)").
:- load_kif("(subclass QuintaryRelation InheritableRelation)").
:- load_kif("(documentation QuintaryRelation EnglishLanguage \"&%QuintaryRelations relate five items. The two &%subclasses of &%QuintaryRelation are &%QuintaryPredicate and &%QuaternaryFunction.\")").
:- load_kif("(subclass List Relation)").
:- load_kif("(documentation List EnglishLanguage \"Every &%List is a particular ordered n-tuple of items. Generally speaking, &%Lists are created by means of the &%ListFn &%Function, which takes any number of items as arguments and returns a &%List with the items in the same order. Anything, including other &%Lists, may be an item in a &%List. Note too that &%Lists are extensional - two lists that have the same items in the same order are identical. Note too that a &%List may contain no items. In that case, the &%List is the &%NullList.\")").
:- load_kif(" (=> (and (instance ?LIST List) (not (instance ?LIST NullList))) (exists (?NUMBER1 ?ITEM1) (and (not (equal (ListOrderFn ?LIST ?NUMBER1) ?ITEM1)) (forall (?NUMBER2) (=> (and (instance ?NUMBER2 PositiveInteger) (lessThan ?NUMBER2 ?NUMBER1)) (exists (?ITEM2) (equal (ListOrderFn ?LIST ?NUMBER2) ?ITEM2)))))))").
:- load_kif("(subclass UniqueList List)").
:- load_kif("(documentation UniqueList EnglishLanguage \"A &%List in which no item appears more than once, i.e. a &%List for which there are no distinct numbers ?NUMBER1 and ?NUMBER2 such that (&%ListOrderFn ?LIST ?NUMBER1) and (&%ListOrderFn ?LIST ?NUMBER2) return the same value.\")").
:- load_kif("(=> (instance ?LIST UniqueList) (forall (?NUMBER1 ?NUMBER2) (=> (equal (ListOrderFn ?LIST ?NUMBER1) (ListOrderFn ?LIST ?NUMBER2)) (equal ?NUMBER1 ?NUMBER2))))").
:- load_kif("(instance NullList List)").
:- load_kif("(documentation NullList EnglishLanguage \"The &%List that has no items. The uniqueness of &%NullList follows from the extensionality of &%Lists, i.e. the fact that two &%Lists with the same items in the same order are identical.\")").
:- load_kif(" (<=> (equal ?LIST NullList) (and (instance ?LIST List) (not (exists (?ITEM) (inList ?ITEM ?LIST)))))").
:- load_kif("(instance ListFn Function)").
:- load_kif("(instance ListFn VariableArityRelation) (instance ListFn TotalValuedRelation) - appears to be a contradiction with VariableArityRelation").
:- load_kif("(range ListFn List)").
:- load_kif("(documentation ListFn EnglishLanguage \"A &%Function that takes any number of arguments and returns the &%List containing those arguments in exactly the same order.\")").
:- load_kif("(=> (exhaustiveDecomposition ?CLASS @ROW) (forall (?OBJ) (=> (instance ?OBJ ?CLASS) (exists (?ITEM) (and (inList ?ITEM (ListFn @ROW)) (instance ?OBJ ?ITEM))))))").
:- load_kif("(=> (disjointDecomposition ?CLASS @ROW) (forall (?ITEM) (=> (inList ?ITEM (ListFn @ROW)) (subclass ?ITEM ?CLASS))))").
:- load_kif("(=> (disjointDecomposition ?CLASS @ROW) (forall (?ITEM1 ?ITEM2) (=> (and (inList ?ITEM1 (ListFn @ROW)) (inList ?ITEM2 (ListFn @ROW)) (not (equal ?ITEM1 ?ITEM2))) (disjoint ?ITEM1 ?ITEM2))))").
:- load_kif("(<=> (disjointDecomposition ?CLASS ?ROW1 ?ROW2 ?ROW3) (and (disjoint ?ROW1 ?ROW2) (disjoint ?ROW2 ?ROW3) (disjoint ?ROW3 ?ROW1)))").
:- load_kif("(instance ListOrderFn BinaryFunction)").
:- load_kif("(instance ListOrderFn PartialValuedRelation)").
:- load_kif("(domain ListOrderFn 1 List)").
:- load_kif("(domain ListOrderFn 2 PositiveInteger)").
:- load_kif("(range ListOrderFn Entity)").
:- load_kif("(documentation ListOrderFn EnglishLanguage \"(&%ListOrderFn ?LIST ?NUMBER) denotes the item that is in the ?NUMBER position in the &%List ?LIST. For example,"). (&%ListOrderFn (&%ListFn &%Monday &%Tuesday &%Wednesday) 2) would return the value &%Tuesday.\")").
:- load_kif(" (=> (and (instance ?LIST1 List) (not (equal ?LIST1 NullList)) (not (equal ?LIST2 NullList)) (instance ?LIST2 List) (forall (?NUMBER) (equal (ListOrderFn ?LIST1 ?NUMBER) (ListOrderFn ?LIST2 ?NUMBER)))) (equal ?LIST1 ?LIST2))").
:- load_kif("(=> (and (domain ?REL ?NUMBER ?CLASS) (instance ?REL Predicate) (?REL @ROW)) (instance (ListOrderFn (ListFn @ROW) ?NUMBER) ?CLASS))").
:- load_kif("(=> (and (domainSubclass ?REL ?NUMBER ?CLASS) (instance ?REL Predicate) (?REL @ROW)) (subclass (ListOrderFn (ListFn @ROW) ?NUMBER) ?CLASS))").
:- load_kif("(instance ListLengthFn UnaryFunction)").
:- load_kif("(instance ListLengthFn TotalValuedRelation)").
:- load_kif("(domain ListLengthFn 1 List)").
:- load_kif("(range ListLengthFn NonnegativeInteger)").
:- load_kif("(documentation ListLengthFn EnglishLanguage \"A &%Function that takes a &%List as its sole argument and returns the number of items in the &%List. For example,"). (&%ListLengthFn (&%ListFn &%Monday &%Tuesday &%Wednesday)) would return the value 3.\")").
:- load_kif(" (=> (and (equal (ListLengthFn ?LIST) ?NUMBER1) (instance ?LIST List) (not (equal ?LIST NullList)) (instance ?NUMBER1 PositiveInteger)) (forall (?NUMBER2) (<=> (exists (?ITEM) (and (equal (ListOrderFn ?LIST ?NUMBER2) ?ITEM) (inList ?ITEM ?LIST))) (lessThanOrEqualTo ?NUMBER2 ?NUMBER1))))").
:- load_kif("(forall (@ROW ?ITEM) (equal (ListLengthFn (ListFn @ROW ?ITEM)) (SuccessorFn (ListLengthFn (ListFn @ROW)))))").
:- load_kif("(forall (@ROW ?ITEM) (equal (ListOrderFn (ListFn @ROW ?ITEM) (ListLengthFn (ListFn @ROW ?ITEM))) ?ITEM))").
:- load_kif("(=> (and (valence ?REL ?NUMBER) (instance ?REL Predicate)) (forall (@ROW) (=> (?REL @ROW) (equal (ListLengthFn (ListFn @ROW)) ?NUMBER))))").
:- load_kif(" (=> (and (equal (ListLengthFn ?LIST1) ?NUMBER) (instance ?LIST List) (not (equal ?LIST NullList)) (instance ?NUMBER1 PositiveInteger)) (exists (?LIST2 ?ITEM) (and (initialList ?LIST1 ?LIST2) (equal (SuccessorFn ?NUMBER) (ListLengthFn ?LIST2)) (equal (ListOrderFn ?LIST2 (SuccessorFn ?NUMBER)) ?ITEM))))").
:- load_kif("(instance ListConcatenateFn BinaryFunction)").
:- load_kif("(instance ListConcatenateFn TotalValuedRelation)").
:- load_kif("(domain ListConcatenateFn 1 List)").
:- load_kif("(domain ListConcatenateFn 2 List)").
:- load_kif("(range ListConcatenateFn List)").
:- load_kif("(documentation ListConcatenateFn EnglishLanguage \"A &%Function that returns the concatenation of the two &%Lists that are given as arguments. For example, the value of (&%ListConcatenateFn (&%ListFn &%Monday &%Tuesday) (&%ListFn &%Wednesday &%Thursday)) would be (&%ListFn &%Monday &%Tuesday &%Wednesday &%Thursday).\")").
% :- load_kif("; removed to solve an obscure contradiction, but needs to be reexamined ").
% :- load_kif(" (<=> (and (equal ?LIST3 (ListConcatenateFn ?LIST1 ?LIST2)) (not (equal ?LIST1 NullList)) (not (equal ?LIST2 NullList))) (forall (?NUMBER1 ?NUMBER2) (=> (and (lessThanOrEqualTo ?NUMBER1 (ListLengthFn ?LIST1)) (lessThanOrEqualTo ?NUMBER2 (ListLengthFn ?LIST2)) (instance ?NUMBER1 PositiveInteger) (instance ?NUMBER2 PositiveInteger)) (and (equal (ListOrderFn ?LIST3 ?NUMBER1) (ListOrderFn ?LIST1 ?NUMBER1)) (equal (ListOrderFn ?LIST3 (AdditionFn (ListLengthFn ?LIST1) ?NUMBER2)) (ListOrderFn ?LIST2 ?NUMBER2))))))").
:- load_kif("(instance inList BinaryPredicate)").
:- load_kif("(instance inList IrreflexiveRelation)").
:- load_kif("(instance inList AsymmetricRelation)").
:- load_kif("(domain inList 1 Entity)").
:- load_kif("(domain inList 2 List)").
:- load_kif("(documentation inList EnglishLanguage \"The analog of &%element and &%instance for &%Lists.  (&%inList ?OBJ ?LIST) means that ?OBJ is in the &%List ?LIST. For example,"). (&%inList &%Tuesday (&%ListFn &%Monday &%Tuesday &%Wednesday)) would be true.\")").
:- load_kif("(=> (inList ?ITEM ?LIST) (exists (?NUMBER) (equal (ListOrderFn ?LIST ?NUMBER) ?ITEM)))").
:- load_kif("(instance subList BinaryPredicate)").
:- load_kif("(instance subList PartialOrderingRelation)").
:- load_kif("(domain subList 1 List)").
:- load_kif("(domain subList 2 List)").
:- load_kif("(documentation subList EnglishLanguage \"(&%subList ?LIST1 ?LIST2) means that ?LIST1 is a sublist of ?LIST2, i.e. every element of ?LIST1 is an element of ?LIST2 and the elements that are common to both &%Lists have the same order in both &%Lists. Elements that are common to both Lists and are consecutive in one list must also be consecutive in the other list. (Therefore - the list of prime numbers smaller than 10 [1 2 3 5 7] is not a subList of the natural numbers smaller than 10 [1 2 3 4 5 6 7 8 9]).\")").
:- load_kif(" (=> (and (subList ?LIST1 ?LIST2) (not (equal ?LIST1 NullList)) (not (equal ?LIST2 NullList))) (forall (?ITEM) (=> (inList ?ITEM ?LIST1) (inList ?ITEM ?LIST2))))").
:- load_kif(" (=> (and (subList ?LIST1 ?LIST2) (not (equal ?LIST1 NullList)) (not (equal ?LIST2 NullList))) (exists (?NUMBER3) (forall (?ITEM) (=> (inList ?ITEM ?LIST1) (exists (?NUMBER1 ?NUMBER2) (and (equal (ListOrderFn ?LIST1 ?NUMBER1) ?ITEM) (equal (ListOrderFn ?LIST2 ?NUMBER2) ?ITEM) (equal ?NUMBER2 (AdditionFn ?NUMBER1 ?NUMBER3))))))))").
:- load_kif("(instance initialList BinaryPredicate)").
:- load_kif("(instance initialList PartialOrderingRelation)").
:- load_kif("(subrelation initialList subList)").
:- load_kif("(documentation initialList EnglishLanguage \"(&%initialList ?LIST1 ?LIST2) means that ?LIST1 is a &%subList of ?LIST2 and (&%ListOrderFn ?LIST1 ?NUMBER) returns the same value as (&%ListOrderFn ?LIST2 ?NUMBER) for all of the values of ?NUMBER over which (&%ListOrderFn ?LIST1 ?NUMBER) is defined.\")").
:- load_kif(" (=> (and (initialList ?LIST1 ?LIST2) (not (equal ?LIST1 NullList)) (not (equal ?LIST2 NullList))) (forall (?NUMBER1 ?NUMBER2) (=> (and (equal (ListLengthFn ?LIST1) ?NUMBER1) (lessThanOrEqualTo ?NUMBER2 ?NUMBER1)) (equal (ListOrderFn ?LIST1 ?NUMBER2) (ListOrderFn ?LIST2 ?NUMBER2)))))").
:- load_kif("(forall (@ROW ?ITEM) (initialList (ListFn @ROW) (ListFn @ROW ?ITEM)))").
:- load_kif("(instance identicalListItems BinaryPredicate)").
:- load_kif("(instance identicalListItems EquivalenceRelation)").
:- load_kif("(domain identicalListItems 1 List)").
:- load_kif("(domain identicalListItems 2 List)").
:- load_kif("(documentation identicalListItems EnglishLanguage \"(&%identicalListItems ?LIST1 ?LIST2) means that ?LIST1 and ?LIST2 have exactly the same items in their respective lists. Although ?LIST1 and ?LIST2 are required to share exactly the same items, they may order these items differently.\")").
:- load_kif(" (=> (and (identicalListItems ?LIST1 ?LIST2) (not (equal ?LIST1 NullList)) (not (equal ?LIST2 NullList))) (<=> (inList ?ITEM ?LIST1) (inList ?ITEM ?LIST2)))").
:- load_kif("(subclass Predicate Relation)").
:- load_kif("(subclass Predicate InheritableRelation)").
:- load_kif("(documentation Predicate EnglishLanguage \"A &%Predicate is a sentence-forming &%Relation. Each tuple in the &%Relation is a finite, ordered sequence of objects. The fact that a particular tuple is an element of a &%Predicate is denoted by '(*predicate* arg_1 arg_2 .. arg_n)', where the arg_i are the objects so related. In the case of &%BinaryPredicates, the fact can be read as `arg_1 is *predicate* arg_2' or `a *predicate* of arg_1 is arg_2'.\")").
:- load_kif("(subclass Function SingleValuedRelation)").
:- load_kif("(subclass Function InheritableRelation)").
:- load_kif("(documentation Function EnglishLanguage \"A &%Function is a term-forming &%Relation that maps from a n-tuple of arguments to a range and that associates this n-tuple with at most one range element. Note that the range is a &%SetOrClass, and each element of the range is an instance of the &%SetOrClass.\")").
:- load_kif("(subclass UnaryFunction Function)").
:- load_kif("(subclass UnaryFunction BinaryRelation)").
:- load_kif("(subclass UnaryFunction InheritableRelation)").
:- load_kif("(documentation UnaryFunction EnglishLanguage \"The &%Class of &%Functions that require a single argument.\")").
:- load_kif("(=> (instance ?FUNCTION UnaryFunction) (valence ?FUNCTION 1))").
:- load_kif("(subclass OneToOneFunction UnaryFunction)").
:- load_kif("(documentation OneToOneFunction EnglishLanguage \"The &%Class of &%UnaryFunctions which are one to one. A function F is one to one just in case for all X, Y in the domain of F, if X is not identical to Y, then F(X) is not identical to F(Y).\")").
:- load_kif("(=> (instance ?FUN OneToOneFunction) (forall (?ARG1 ?ARG2) (=> (and (domain ?FUN 1 ?CLASS) (instance ?ARG1 ?CLASS) (instance ?ARG2 ?CLASS) (not (equal ?ARG1 ?ARG2))) (not (equal (AssignmentFn ?FUN ?ARG1) (AssignmentFn ?FUN ?ARG2))))))").
:- load_kif("(subclass SequenceFunction OneToOneFunction)").
:- load_kif("(documentation SequenceFunction EnglishLanguage \"The &%Class of &%OneToOneFunctions whose range is a subclass of the &%PositiveIntegers.\")").
:- load_kif("(=> (and (instance ?SEQ SequenceFunction) (range ?SEQ ?CLASS)) (subclass ?CLASS Integer))").
:- load_kif("(subclass BinaryFunction Function)").
:- load_kif("(subclass BinaryFunction TernaryRelation)").
:- load_kif("(subclass BinaryFunction InheritableRelation)").
:- load_kif("(documentation BinaryFunction EnglishLanguage \"The &%Class of &%Functions that require two arguments.\")").
:- load_kif("(=> (instance ?FUNCTION BinaryFunction) (valence ?FUNCTION 2))").
:- load_kif("(subclass AssociativeFunction BinaryFunction)").
:- load_kif("(documentation AssociativeFunction EnglishLanguage \"A &%BinaryFunction is associative if bracketing has no effect on the value returned by the &%Function. More precisely, a &%Function ?FUNCTION is associative just in case").
:- load_kif("(?FUNCTION ?INST1 (?FUNCTION ?INST2 ?INST3)) is equal to").
:- load_kif("(?FUNCTION (?FUNCTION ?INST1 ?INST2) ?INST3), for all ?INST1, ?INST2, and ?INST3.\")").
:- load_kif("(=> (instance ?FUNCTION AssociativeFunction) (forall (?INST1 ?INST2 ?INST3) (=> (and (domain ?FUNCTION 1 ?CLASS) (instance ?INST1 ?CLASS) (instance ?INST2 ?CLASS) (instance ?INST3 ?CLASS)) (equal (AssignmentFn ?FUNCTION ?INST1 (AssignmentFn ?FUNCTION ?INST2 ?INST3)) (AssignmentFn ?FUNCTION (AssignmentFn ?FUNCTION ?INST1 ?INST2) ?INST3)))))").
:- load_kif("(subclass CommutativeFunction BinaryFunction)").
:- load_kif("(documentation CommutativeFunction EnglishLanguage \"A &%BinaryFunction is commutative if the ordering of the arguments of the function has no effect on the value returned by the function. More precisely, a function ?FUNCTION is commutative just in case (?FUNCTION ?INST1 ?INST2) is equal to (?FUNCTION ?INST2 ?INST1), for all ?INST1 and ?INST2.\")").
:- load_kif("(=> (instance ?FUNCTION CommutativeFunction) (forall (?INST1 ?INST2) (=> (and (domain ?FUNCTION 1 ?CLASS) (instance ?INST1 ?CLASS) (instance ?INST2 ?CLASS)) (equal (AssignmentFn ?FUNCTION ?INST1 ?INST2) (AssignmentFn ?FUNCTION ?INST2 ?INST1)))))").
:- load_kif("(subclass TernaryFunction Function)").
:- load_kif("(subclass TernaryFunction QuaternaryRelation)").
:- load_kif("(subclass TernaryFunction InheritableRelation)").
:- load_kif("(documentation TernaryFunction EnglishLanguage \"The &%Class of &%Functions that require exactly three arguments.\")").
:- load_kif("(=> (instance ?FUNCTION TernaryFunction) (valence ?FUNCTION 3))").
:- load_kif("(subclass QuaternaryFunction Function)").
:- load_kif("(subclass QuaternaryFunction QuintaryRelation)").
:- load_kif("(subclass QuaternaryFunction InheritableRelation)").
:- load_kif("(documentation QuaternaryFunction EnglishLanguage \"The &%Class of &%Functions that require exactly four arguments.\")").
:- load_kif("(=> (instance ?FUNCTION QuaternaryFunction) (valence ?FUNCTION 4))").
:- load_kif("(subclass ContinuousFunction Function)").
:- load_kif("(documentation ContinuousFunction EnglishLanguage \"&%Functions which are continuous. This concept is taken as primitive until representations for limits are devised.\")").
:- load_kif("(subclass BinaryPredicate Predicate)").
:- load_kif("(subclass BinaryPredicate BinaryRelation)").
:- load_kif("(subclass BinaryPredicate InheritableRelation)").
:- load_kif("(documentation BinaryPredicate EnglishLanguage \"A &%Predicate relating two items - its valence is two.\")").
:- load_kif("(=> (instance ?REL BinaryPredicate) (valence ?REL 2))").
:- load_kif("(subclass TernaryPredicate Predicate)").
:- load_kif("(subclass TernaryPredicate TernaryRelation)").
:- load_kif("(subclass TernaryPredicate InheritableRelation)").
:- load_kif("(documentation TernaryPredicate EnglishLanguage \"The &%Class of &%Predicates that require exactly three arguments.\")").
:- load_kif("(=> (instance ?REL TernaryPredicate) (valence ?REL 3))").
:- load_kif("(subclass QuaternaryPredicate Predicate)").
:- load_kif("(subclass QuaternaryPredicate QuaternaryRelation)").
:- load_kif("(subclass QuaternaryPredicate InheritableRelation)").
:- load_kif("(documentation QuaternaryPredicate EnglishLanguage \"The &%Class of &%Predicates that require four arguments.\")").
:- load_kif("(=> (instance ?REL QuaternaryPredicate) (valence ?REL 4))").
:- load_kif("(subclass QuintaryPredicate Predicate)").
:- load_kif("(subclass QuintaryPredicate QuintaryRelation)").
:- load_kif("(subclass QuintaryPredicate InheritableRelation)").
:- load_kif("(documentation QuintaryPredicate EnglishLanguage \"The &%Class of &%Predicates that require five arguments.\")").
:- load_kif("(=> (instance ?REL QuintaryPredicate) (valence ?REL 5))").
:- load_kif("(subclass VariableArityRelation Relation)").
:- load_kif("(documentation VariableArityRelation EnglishLanguage \"The &%Class of &%Relations that do not have a fixed number of arguments.\")").
:- load_kif("(=> (instance ?REL VariableArityRelation) (not (exists (?INT) (valence ?REL ?INT))))").
:- load_kif("(subclass RelationExtendedToQuantities Relation)").
:- load_kif("(subclass RelationExtendedToQuantities InheritableRelation)").
:- load_kif("(documentation RelationExtendedToQuantities EnglishLanguage \"A &%RelationExtendedToQuantities is a &%Relation that, when it is true on a sequence of arguments that are &%RealNumbers, it is also true on a sequence of instances of &%ConstantQuantity with those magnitudes in some unit of measure. For example, the &%lessThan relation is extended to quantities. This means that for all pairs of quantities ?QUANTITY1 and ?QUANTITY2,"). (&%lessThan ?QUANTITY1 ?QUANTITY2) if and only if, for some ?NUMBER1, ?NUMBER2, and ?UNIT, ?QUANTITY1 = (&%MeasureFn ?NUMBER1 ?UNIT), ?QUANTITY2 = (&%MeasureFn ?NUMBER2 ?UNIT), and (&%lessThan ?NUMBER1 ?NUMBER2), for all units ?UNIT on which ?QUANTITY1 and ?QUANTITY2 can be measured. Note that, when a &%RelationExtendedToQuantities is extended from &%RealNumbers to instances of &%ConstantQuantity, the &%ConstantQuantity must be measured along the same physical dimension.\")").
:- load_kif("(subclass LogicalOperator Predicate)").
:- load_kif("(documentation LogicalOperator EnglishLanguage \"This &%Class comprises all of the logical operators (viz. 'and', 'or', 'not', '=>', and '<=>').\")").
:- load_kif("(subclass Proposition Abstract)").
:- load_kif("(documentation Proposition EnglishLanguage \"&%Propositions are &%Abstract entities that express a complete thought or a set of such thoughts. As an example, the formula '(instance Yojo Cat)' expresses the &%Proposition that the entity named Yojo is an element of the &%Class of Cats. Note that propositions are not restricted to the content expressed by individual sentences of a &%Language. They may encompass the content expressed by theories, books, and even whole libraries. It is important to distinguish &%Propositions from the &%ContentBearingObjects that express them. A &%Proposition is a piece of information, e.g. that the cat is on the mat, but a &%ContentBearingObject is an &%Object that represents this information. A &%Proposition is an abstraction that may have multiple representations: strings, sounds, icons, etc. For example, the &%Proposition that the cat is on the mat is represented here as a string of graphical characters displayed on a monitor and/or printed on paper, but it can be represented by a sequence of sounds or by some non-latin alphabet or by some cryptographic form\")").
:- load_kif("(instance closedOn BinaryPredicate)").
:- load_kif("(instance closedOn AsymmetricRelation)").
:- load_kif("(domain closedOn 1 Function)").
:- load_kif("(domain closedOn 2 SetOrClass)").
:- load_kif("(documentation closedOn EnglishLanguage \"A &%BinaryFunction is closed on a &%SetOrClass if it is defined for all instances of the &%SetOrClass and its value is always an instance of the &%SetOrClass.\")").
:- load_kif("(=> (and (closedOn ?FUNCTION ?CLASS) (instance ?FUNCTION UnaryFunction)) (forall (?INST) (=> (instance ?INST ?CLASS) (instance (AssignmentFn ?FUNCTION ?INST) ?CLASS))))").
:- load_kif("(=> (and (closedOn ?FUNCTION ?CLASS) (instance ?FUNCTION BinaryFunction)) (forall (?INST1 ?INST2) (=> (and (instance ?INST1 ?CLASS) (instance ?INST2 ?CLASS)) (instance (AssignmentFn ?FUNCTION ?INST1 ?INST2) ?CLASS))))").
:- load_kif("(instance reflexiveOn BinaryPredicate)").
:- load_kif("(instance reflexiveOn AsymmetricRelation)").
:- load_kif("(domain reflexiveOn 1 BinaryRelation)").
:- load_kif("(domain reflexiveOn 2 SetOrClass)").
:- load_kif("(documentation reflexiveOn EnglishLanguage \"A &%BinaryRelation is reflexive on a &%SetOrClass only if every instance of the &%SetOrClass bears the relation to itself.\")").
:- load_kif("(=> (and (reflexiveOn ?RELATION ?CLASS) (instance ?RELATION Predicate)) (forall (?INST) (=> (instance ?INST ?CLASS) (?RELATION ?INST ?INST))))").
:- load_kif("(instance irreflexiveOn BinaryPredicate)").
:- load_kif("(instance irreflexiveOn AsymmetricRelation)").
:- load_kif("(domain irreflexiveOn 1 BinaryRelation)").
:- load_kif("(domain irreflexiveOn 2 SetOrClass)").
:- load_kif("(documentation irreflexiveOn EnglishLanguage \"A &%BinaryRelation is irreflexive on a &%SetOrClass only if no instance of the &%SetOrClass bears the relation to itself.\")").
:- load_kif("(=> (and (irreflexiveOn ?RELATION ?CLASS) (instance ?RELATION Predicate)) (forall (?INST) (=> (instance ?INST ?CLASS) (not (?RELATION ?INST ?INST)))))").
:- load_kif("(instance partialOrderingOn BinaryPredicate)").
:- load_kif("(instance partialOrderingOn AsymmetricRelation)").
:- load_kif("(domain partialOrderingOn 1 BinaryRelation)").
:- load_kif("(domain partialOrderingOn 2 SetOrClass)").
:- load_kif("(documentation partialOrderingOn EnglishLanguage \"A &%BinaryRelation is a partial ordering on a &%SetOrClass only if the relation is &%reflexiveOn the &%SetOrClass, and it is both an &%AntisymmetricRelation, and a &%TransitiveRelation.\")").
:- load_kif("(=> (partialOrderingOn ?RELATION ?CLASS) (and (reflexiveOn ?RELATION ?CLASS) (instance ?RELATION TransitiveRelation) (instance ?RELATION AntisymmetricRelation)))").
:- load_kif("(instance totalOrderingOn BinaryPredicate)").
:- load_kif("(instance totalOrderingOn AsymmetricRelation)").
:- load_kif("(domain totalOrderingOn 1 BinaryRelation)").
:- load_kif("(domain totalOrderingOn 2 SetOrClass)").
:- load_kif("(documentation totalOrderingOn EnglishLanguage \"A &%BinaryRelation ?REL is a total ordering on a &%SetOrClass only if it is a partial ordering for which either").
:- load_kif("(?REL ?INST1 ?INST2) or (?REL ?INST2 ?INST1) for every ?INST1 and ?INST2 in the &%SetOrClass.\")").
:- load_kif("(<=> (totalOrderingOn ?RELATION ?CLASS) (and (partialOrderingOn ?RELATION ?CLASS) (trichotomizingOn ?RELATION ?CLASS)))").
:- load_kif("(instance trichotomizingOn BinaryPredicate)").
:- load_kif("(instance trichotomizingOn AsymmetricRelation)").
:- load_kif("(domain trichotomizingOn 1 BinaryRelation)").
:- load_kif("(domain trichotomizingOn 2 SetOrClass)").
:- load_kif("(documentation trichotomizingOn EnglishLanguage \"A &%BinaryRelation ?REL is trichotomizing on a &%SetOrClass only if, for all instances ?INST1 and ?INST2 of the &%SetOrClass, at least one of the following holds: (?REL ?INST1 ?INST2),").
:- load_kif("(?REL ?INST2 ?INST1) or (equal ?INST1 ?INST2).\")").
:- load_kif("(=> (and (trichotomizingOn ?RELATION ?CLASS) (instance ?RELATION Predicate)) (forall (?INST1 ?INST2) (=> (and (instance ?INST1 ?CLASS) (instance ?INST2 ?CLASS)) (or (?RELATION ?INST1 ?INST2) (?RELATION ?INST2 ?INST1) (equal ?INST1 ?INST2)))))").
:- load_kif("(instance equivalenceRelationOn BinaryPredicate)").
:- load_kif("(instance equivalenceRelationOn AsymmetricRelation)").
:- load_kif("(domain equivalenceRelationOn 1 BinaryRelation)").
:- load_kif("(domain equivalenceRelationOn 2 SetOrClass)").
:- load_kif("(documentation equivalenceRelationOn EnglishLanguage \"A &%BinaryRelation is an &%equivalenceRelationOn a &%SetOrClass only if the relation is &%reflexiveOn the &%SetOrClass and it is both a &%TransitiveRelation and a &%SymmetricRelation.\")").
:- load_kif("(=> (equivalenceRelationOn ?RELATION ?CLASS) (and (instance ?RELATION TransitiveRelation) (instance ?RELATION SymmetricRelation) (reflexiveOn ?RELATION ?CLASS)))").
:- load_kif("(instance distributes BinaryPredicate)").
:- load_kif("(domain distributes 1 BinaryFunction)").
:- load_kif("(domain distributes 2 BinaryFunction)").
:- load_kif("(documentation distributes EnglishLanguage \"A &%BinaryFunction ?FUNCTION1 is distributive over another &%BinaryFunction ?FUNCTION2 just in case").
:- load_kif("(?FUNCTION1 ?INST1 (?FUNCTION2 ?INST2 ?INST3)) is equal to").
:- load_kif("(?FUNCTION2 (?FUNCTION1 ?INST1 ?INST2) (?FUNCTION1 ?INST1 ?INST3)), for all ?INST1, ?INST2, and ?INST3.\")").
:- load_kif("(=> (distributes ?FUNCTION1 ?FUNCTION2) (forall (?INST1 ?INST2 ?INST3) (=> (and (domain ?FUNCTION1 1 ?CLASS1) (instance ?INST1 ?CLASS1) (instance ?INST2 ?CLASS1) (instance ?INST3 ?CLASS1) (domain ?FUNCTION2 1 ?CLASS2) (instance ?INST1 ?CLASS2) (instance ?INST2 ?CLASS2) (instance ?INST3 ?CLASS2)) (equal (AssignmentFn ?FUNCTION1 ?INST1 (AssignmentFn ?FUNCTION2 ?INST2 ?INST3)) (AssignmentFn ?FUNCTION2 (AssignmentFn ?FUNCTION1 ?INST1 ?INST2) (AssignmentFn ?FUNCTION1 ?INST1 ?INST3))))))").
:- load_kif("(documentation relatedEvent EnglishLanguage \"(relatedEvent ?EVENT1 ?EVENT2) means that the &%Process ?EVENT1 is related to the Process ?EVENT2. The relationship is between separate individual events, not events and their subprocesses. On the other hand, two &%subProcesses of the same overarching event may be &%relatedEvents. The argument order does not imply temporal ordering.\")").
:- load_kif("(instance relatedEvent BinaryPredicate)").
:- load_kif("(instance relatedEvent SymmetricRelation)").
:- load_kif("(domain relatedEvent 1 Process)").
:- load_kif("(domain relatedEvent 2 Process)").
:- load_kif("(=> (and (subProcess ?S1 ?P) (subProcess ?S2 ?P)) (relatedEvent ?S1 ?S2))").
:- load_kif("(instance causes BinaryPredicate)").
:- load_kif("(instance causes AsymmetricRelation)").
:- load_kif("(domain causes 1 Process)").
:- load_kif("(domain causes 2 Process)").
:- load_kif("(relatedInternalConcept causes causesSubclass)").
:- load_kif("(subrelation causes relatedEvent)").
:- load_kif("(documentation causes EnglishLanguage \"The causation relation between instances of &%Process.  (&%causes ?PROCESS1 ?PROCESS2) means that the instance of &%Process ?PROCESS1 brings about the instance of &%Process ?PROCESS2.\")").
:- load_kif("(=> (instance ?PROC1 Process) (exists (?PROC2) (causes ?PROC2 ?PROC1)))").
:- load_kif("(=> (causes ?P1 ?P2) (earlier (WhenFn ?P1) (WhenFn ?P2)))").
:- load_kif("(instance causesSubclass BinaryPredicate)").
:- load_kif("(instance causesSubclass AsymmetricRelation)").
:- load_kif("(domainSubclass causesSubclass 1 Process)").
:- load_kif("(domainSubclass causesSubclass 2 Process)").
:- load_kif("(documentation causesSubclass EnglishLanguage \"The causation relation between subclasses of &%Process.  (&%causesSubclass ?PROCESS1 ?PROCESS2) means that the subclass of &%Process ?PROCESS1 brings about the subclass of &%Process ?PROCESS2, e.g. (&%causesSubclass &%Killing &%Death).\")").
:- load_kif("(=> (causesSubclass ?PROC1 ?PROC2) (forall (?INST2) (=> (instance ?INST2 ?PROC2) (exists (?INST1) (and (instance ?INST1 ?PROC1) (causes ?INST1 ?INST2))))))").
:- load_kif("(instance causesProposition BinaryPredicate)").
:- load_kif("(instance causesProposition AsymmetricRelation)").
:- load_kif("(domain causesProposition 1 Formula)").
:- load_kif("(domain causesProposition 2 Formula)").
:- load_kif("(documentation causesProposition EnglishLanguage \"(&%causesProposition ?FORMULA1 ?FORMULA2) means that the state of affairs described by ?FORMULA1 causes, or mechanistically brings about, the state of affairs described by ?FORMULA2. Note that unlike &%entails, the time during which ?FORMULA2 holds cannot precede the time during which ?FORMULA1 holds, although ?FORMULA1 and ?FORMULA2 can hold simultaneously. Note, also, that &%causesProposition is a predicate, not a truth function. The following rule").
:- load_kif("(contraposition) does not hold: (=> (causesProp ?FORMULA1 ?FORMULA2) (causesProp (not ?FORMULA2) (not ?FORMULA1))).\")").
:- load_kif("(=> (and (holdsDuring ?T2 ?SIT2) (holdsDuring ?T1 ?SIT1) (instance ?T1 TimeInterval) (instance ?T2 TimeInterval) (causesProposition ?SIT1 ?SIT2)) (beforeOrEqual (BeginFn ?T1) (BeginFn ?T2)))").
:- load_kif("(instance copy BinaryPredicate)").
:- load_kif("(instance copy EquivalenceRelation)").
:- load_kif("(domain copy 1 Object)").
:- load_kif("(domain copy 2 Object)").
:- load_kif("(documentation copy EnglishLanguage \"relates an &%Object to an exact copy of the &%Object, where an exact copy is indistinguishable from the original with regard to every property except (possibly) spatial and/or temporal location.\")").
:- load_kif("(=> (copy ?OBJ1 ?OBJ2) (forall (?ATTR) (=> (attribute ?OBJ1 ?ATTR) (attribute ?OBJ2 ?ATTR))))").
:- load_kif("(instance time BinaryPredicate)").
:- load_kif("(instance time TemporalRelation)").
:- load_kif("(instance time AsymmetricRelation)").
:- load_kif("(domain time 1 Physical)").
:- load_kif("(domain time 2 TimePosition)").
:- load_kif("(relatedInternalConcept time located)").
:- load_kif("(relatedInternalConcept time holdsDuring)").
:- load_kif("(documentation time EnglishLanguage \"This relation holds between an instance of &%Physical and an instance of &%TimePosition just in case the temporal lifespan of the former includes the latter. In other words, (&%time ?THING ?TIME) means that ?THING existed or occurred at ?TIME. Note that &%time does for instances of &%Physical what &%holdsDuring does for instances of &%Formula. The constants &%located and &%time are the basic spatial and temporal predicates, respectively.\")").
:- load_kif("(instance holdsDuring BinaryPredicate)").
:- load_kif("(instance holdsDuring AsymmetricRelation)").
:- load_kif("(domain holdsDuring 1 TimePosition)").
:- load_kif("(domain holdsDuring 2 Formula)").
:- load_kif("(documentation holdsDuring EnglishLanguage \"(&%holdsDuring ?TIME ?FORMULA) means that the proposition denoted by ?FORMULA is true in the time frame ?TIME. Note that this implies that ?FORMULA is true at every &%TimePoint which is a &%temporalPart of ?TIME.\")").
:- load_kif("(=> (and (holdsDuring ?TIME ?SITUATION1) (entails ?SITUATION1 ?SITUATION2)) (holdsDuring ?TIME ?SITUATION2))").
:- load_kif("(=> (holdsDuring ?TIME (not ?SITUATION)) (not (holdsDuring ?TIME ?SITUATION)))").
:- load_kif("(instance capability TernaryPredicate)").
:- load_kif("(domainSubclass capability 1 Process)").
:- load_kif("(domain capability 2 CaseRole)").
:- load_kif("(domain capability 3 Object)").
:- load_kif("(documentation capability EnglishLanguage \"(&%capability ?PROCESS ?ROLE ?OBJ) means that ?OBJ has the ability to play the role of ?ROLE in &%Processes of type ?PROCESS.\")").
:- load_kif("(=> (and (instance ?ROLE CaseRole) (?ROLE ?ARG1 ?ARG2) (instance ?ARG1 ?PROC) (subclass ?PROC Process)) (capability ?PROC ?ROLE ?ARG2))").
:- load_kif("(instance exploits BinaryPredicate)").
:- load_kif("(instance exploits AsymmetricRelation)").
:- load_kif("(domain exploits 1 Object)").
:- load_kif("(domain exploits 2 Agent)").
:- load_kif("(documentation exploits EnglishLanguage \"(&%exploits ?OBJ ?AGENT) means that ?OBJ is used by ?AGENT as a &%resource in an unspecified instance of &%Process. This &%Predicate, as its corresponding axiom indicates, is a composition of the relations &%agent and &%resource.\")").
:- load_kif("(=> (exploits ?OBJ ?AGENT) (exists (?PROCESS) (and (agent ?PROCESS ?AGENT) (resource ?PROCESS ?OBJ))))").
:- load_kif("(instance hasPurpose BinaryPredicate)").
:- load_kif("(instance hasPurpose AsymmetricRelation)").
:- load_kif("(domain hasPurpose 1 Physical)").
:- load_kif("(domain hasPurpose 2 Formula)").
:- load_kif("(documentation hasPurpose EnglishLanguage \"This &%Predicate expresses the concept of a conventional goal, i.e. a goal with a neutralized agent's intention. Accordingly, (&%hasPurpose ?THING ?FORMULA) means that the instance of &%Physical ?THING has, as its purpose, the &%Proposition expressed by ?FORMULA. Note that there is an important difference in meaning between the &%Predicates &%hasPurpose and &%result. Although the second argument of the latter can satisfy the second argument of the former, a conventional goal is an expected and desired outcome, while a result may be neither expected nor desired. For example, a machine process may have outcomes but no goals, aimless wandering may have an outcome but no goal, a learning process may have goals with no outcomes, and so on.\")").
:- load_kif("(instance hasPurposeForAgent TernaryPredicate)").
:- load_kif("(domain hasPurposeForAgent 1 Physical)").
:- load_kif("(domain hasPurposeForAgent 2 Formula)").
:- load_kif("(domain hasPurposeForAgent 3 CognitiveAgent)").
:- load_kif("(documentation hasPurposeForAgent EnglishLanguage \"Expresses a cognitive attitude of an agent with respect to a particular instance of Physical. More precisely,"). (&%hasPurposeForAgent ?THING ?FORMULA ?AGENT) means that the purpose of ?THING for ?AGENT is the proposition expressed by ?FORMULA. Very complex issues are involved here. In particular, the rules of inference of the first order predicate calculus are not truth-preserving for the second argument position of this &%Predicate.\")").
:- load_kif("(=> (hasPurpose ?THING ?PURPOSE) (exists (?AGENT) (hasPurposeForAgent ?THING ?PURPOSE ?AGENT)))").
:- load_kif("(instance hasSkill BinaryPredicate)").
:- load_kif("(instance hasSkill AsymmetricRelation)").
:- load_kif("(domainSubclass hasSkill 1 Process)").
:- load_kif("(domain hasSkill 2 Agent)").
:- load_kif("(documentation hasSkill EnglishLanguage \"Similar to the &%capability &%Predicate with the additional restriction that the ability be practised/ demonstrated to some measurable degree.\")").
:- load_kif("(=> (hasSkill ?PROC ?AGENT) (capability ?PROC agent ?AGENT))").
:- load_kif("(instance confersNorm TernaryPredicate)").
:- load_kif("(domain confersNorm 1 Entity)").
:- load_kif("(domain confersNorm 2 Formula)").
:- load_kif("(domain confersNorm 3 ObjectiveNorm)").
:- load_kif("(documentation confersNorm EnglishLanguage \"Expresses the relationship between a &%Formula, an &%Entity, and an &%ObjectiveNorm when the &%Entity brings it about that the &%Formula has the &%ObjectiveNorm.\")").
:- load_kif("(=> (holdsDuring ?TIME (confersNorm ?ENTITY ?FORMULA ?NORM)) (and (holdsDuring (ImmediatePastFn ?TIME) (not (modalAttribute ?FORMULA ?NORM))) (holdsDuring (ImmediateFutureFn ?TIME) (modalAttribute ?FORMULA ?NORM))))").
:- load_kif("(instance deprivesNorm TernaryPredicate)").
:- load_kif("(disjointRelation deprivesNorm confersNorm)").
:- load_kif("(domain deprivesNorm 1 Entity)").
:- load_kif("(domain deprivesNorm 2 Formula)").
:- load_kif("(domain deprivesNorm 3 ObjectiveNorm)").
:- load_kif("(documentation deprivesNorm EnglishLanguage \"Expresses the relationship between an &%Entity, a &%Formula, and an &%ObjectiveNorm when the &%Entity brings it about that the &%Formula does not have the &%ObjectiveNorm.\")").
:- load_kif("(=> (holdsDuring ?TIME (deprivesNorm ?ENTITY ?FORMULA ?NORM)) (and (holdsDuring (ImmediatePastFn ?TIME) (modalAttribute ?FORMULA ?NORM)) (holdsDuring (ImmediateFutureFn ?TIME) (not (modalAttribute ?FORMULA ?NORM)))))").
:- load_kif("(instance partlyLocated SpatialRelation)").
:- load_kif("(instance partlyLocated BinaryPredicate)").
:- load_kif("(domain partlyLocated 1 Physical)").
:- load_kif("(domain partlyLocated 2 Object)").
:- load_kif("(documentation partlyLocated EnglishLanguage \"(&%partlyLocated ?OBJ1 ?OBJ2) means that the instance of &%Physical ?OBJ1 is at least partially located at ?OBJ2. For example, Istanbul is partly located in &%Asia and partly located in &%Europe. Note that &%partlyLocated is the most basic localization relation: &%located is an immediate &%subrelation of &%partlyLocated and &%exactlyLocated is an immediate &%subrelation of &%located.\")").
:- load_kif("(=> (and (instance ?OBJ1 Object) (partlyLocated ?OBJ1 ?OBJ2)) (overlapsSpatially ?OBJ1 ?OBJ2))").
:- load_kif("(=> (and (instance ?OBJ1 Object) (partlyLocated ?OBJ1 ?OBJ2)) (exists (?SUB) (and (part ?SUB ?OBJ1) (located ?SUB ?OBJ2))))").
:- load_kif("(instance located AntisymmetricRelation)").
:- load_kif("(instance located TransitiveRelation)").
:- load_kif("(subrelation located partlyLocated)").
:- load_kif("(documentation located EnglishLanguage \"(&%located ?OBJ1 ?OBJ2) means that ?OBJ1 is &%partlyLocated at ?OBJ2, and there is no &%part of ?OBJ1 that is not &%located at ?OBJ2.\")").
:- load_kif("(=> (located ?OBJ1 ?OBJ2) (forall (?SUB) (=> (part ?SUB ?OBJ1) (located ?SUB ?OBJ2))))").
:- load_kif("(instance eventPartlyLocated CaseRole)").
:- load_kif("(subrelation eventPartlyLocated partlyLocated)").
:- load_kif("(domain eventPartlyLocated 1 Process)").
:- load_kif("(domain eventPartlyLocated 2 Object)").
:- load_kif("(documentation eventPartlyLocated EnglishLanguage \"(&%eventPartlyLocated ?PROC ?OBJ) means that some &%subProcess of &%Process ?PROC is located in &%Object ?OBJ.\")").
:- load_kif("(termFormat EnglishLanguage eventPartlyLocated \"event partly located\")").
:- load_kif("(=> (eventPartlyLocated ?PROC ?OBJ) (exists (?SUB) (and (subProcess ?SUB ?PROC) (eventLocated ?SUB ?OBJ))))").
:- load_kif("(=> (origin ?PROCESS ?LOC) (eventPartlyLocated ?PROCESS ?LOC))").
:- load_kif("(=> (destination ?PROCESS ?LOC) (eventPartlyLocated ?PROCESS ?LOC))").
:- load_kif("(subrelation eventLocated eventPartlyLocated)").
:- load_kif("(documentation eventLocated EnglishLanguage \"(&%eventLocated ?PROC ?OBJ) means that the entire &%Process ?PROC is located on &%Object ?OBJ, meaning that all &%subProcess of ?PROC is located on ?OBJ\")").
:- load_kif("(termFormat EnglishLanguage eventLocated \"event located\")").
:- load_kif("(=> (eventLocated ?PROCESS ?OBJ) (forall (?SUB) (=> (subProcess ?SUB ?PROCESS) (eventLocated ?SUB ?OBJ))))").
:- load_kif("(subrelation exactlyLocated located)").
:- load_kif("(documentation exactlyLocated EnglishLanguage \"The actual, minimal location of an Object. This is a subrelation of the more general &%Predicate &%located.\")").
:- load_kif("(=> (exactlyLocated ?OBJ ?REGION) (not (exists (?OTHEROBJ) (and (exactlyLocated ?OTHEROBJ ?REGION) (not (equal ?OTHEROBJ ?OBJ))))))").
:- load_kif("(instance between SpatialRelation)").
:- load_kif("(instance between TernaryPredicate)").
:- load_kif("(domain between 1 Object)").
:- load_kif("(domain between 2 Object)").
:- load_kif("(domain between 3 Object)").
:- load_kif("(documentation between EnglishLanguage \"(between ?OBJ1 ?OBJ2 ?OBJ3) means that ?OBJ2 is spatially located between ?OBJ1 and ?OBJ3. Note that this implies that ?OBJ2 is directly between ?OBJ1 and ?OBJ3, i.e. the projections of ?OBJ1 and ?OBJ3 overlap with ?OBJ2.\")").
:- load_kif("(=> (between ?END1 ?MID ?END2) (between ?END2 ?MID ?END1))").
:- load_kif("(documentation betweenOnPath EnglishLanguage \"(betweenOnPath ?OBJ1 ?OBJ2 ?OBJ3 ?PATH) means that ?OBJ2 is spatially located between ?OBJ1 and ?OBJ3 on the path ?PATH. Note that this is a more specialized relation of between since any object that is between others with respect to a particular path is also simply between them.\")").
:- load_kif("(instance betweenOnPath SpatialRelation)").
:- load_kif("(instance betweenOnPath QuaternaryPredicate)").
:- load_kif("(domain betweenOnPath 1 Object) 	").
:- load_kif("(domain betweenOnPath 2 Object) 	").
:- load_kif("(domain betweenOnPath 3 Object)").
:- load_kif("(domain betweenOnPath 4 Object)").
:- load_kif("(=> (betweenOnPath ?OBJ1 ?OBJ2 ?OBJ3 ?PATH) (between ?OBJ1 ?OBJ2 ?OBJ3))").
:- load_kif("(instance traverses BinaryPredicate)").
:- load_kif("(instance traverses SpatialRelation)").
:- load_kif("(domain traverses 1 Object)").
:- load_kif("(domain traverses 2 Object)").
:- load_kif("(documentation traverses EnglishLanguage \"(&%traverses ?OBJ1 ?OBJ2) means that ?OBJ1 crosses or extends across ?OBJ2. Note that &%crosses and &%penetrates are subrelations of &%traverses.\")").
:- load_kif("(=> (traverses ?OBJ1 ?OBJ2) (or (crosses ?OBJ1 ?OBJ2) (penetrates ?OBJ1 ?OBJ2)))").
:- load_kif("(subrelation crosses traverses)").
:- load_kif("(instance crosses AsymmetricRelation)").
:- load_kif("(instance crosses TransitiveRelation)").
:- load_kif("(disjointRelation crosses connected)").
:- load_kif("(documentation crosses EnglishLanguage \"(crosses ?OBJ1 ?OBJ2) means that &%Object ?OBJ1 &%traverses Object ?OBJ2, without being &%connected to it.\")").
:- load_kif("(subrelation penetrates traverses)").
:- load_kif("(subrelation penetrates meetsSpatially)").
:- load_kif("(instance penetrates AsymmetricRelation)").
:- load_kif("(instance penetrates IntransitiveRelation)").
:- load_kif("(documentation penetrates EnglishLanguage \"(penetrates ?OBJ1 ?OBJ2) means that ?OBJ1 is &%connected to ?OBJ2 along at least one whole dimension (length, width or depth).\")").
:- load_kif("(instance WhereFn BinaryFunction)").
:- load_kif("(instance WhereFn SpatialRelation)").
:- load_kif("(instance WhereFn TotalValuedRelation)").
:- load_kif("(domain WhereFn 1 Physical)").
:- load_kif("(domain WhereFn 2 TimePoint)").
:- load_kif("(range WhereFn Region)").
:- load_kif("(relatedInternalConcept WhereFn WhenFn)").
:- load_kif("(documentation WhereFn EnglishLanguage \"Maps an &%Object and a &%TimePoint at which the &%Object exists to the &%Region where the &%Object existed at that &%TimePoint.\")").
:- load_kif("(<=> (equal (WhereFn ?THING ?TIME) ?REGION) (holdsDuring ?TIME (exactlyLocated ?THING ?REGION)))").
:- load_kif("(instance possesses BinaryPredicate)").
:- load_kif("(instance possesses AsymmetricRelation)").
:- load_kif("(domain possesses 1 Agent)").
:- load_kif("(domain possesses 2 Object)").
:- load_kif("(documentation possesses EnglishLanguage \"&%Relation that holds between an &%Agent and an &%Object when the &%Agent has ownership of the &%Object.\")").
:- load_kif("(=> (possesses ?PERSON ?OBJ) (modalAttribute (uses ?OBJ ?PERSON) Permission))").
:- load_kif("(=> (and (instance ?TIME TimePosition) (holdsDuring ?TIME (possesses ?AGENT1 ?OBJ)) (holdsDuring ?TIME (possesses ?AGENT2 ?OBJ))) (equal ?AGENT1 ?AGENT2))").
:- load_kif("(instance PropertyFn UnaryFunction)").
:- load_kif("(instance PropertyFn TotalValuedRelation)").
:- load_kif("(domain PropertyFn 1 Agent)").
:- load_kif("(range PropertyFn Set)").
:- load_kif("(documentation PropertyFn EnglishLanguage \"A &%UnaryFunction that maps an &%Agent to the &%Set of &%Objects owned by the &%Agent.\")").
:- load_kif("(<=> (instance ?OBJ (PropertyFn ?PERSON)) (possesses ?PERSON ?OBJ))").
:- load_kif("(instance precondition BinaryPredicate)").
:- load_kif("(instance precondition AsymmetricRelation)").
:- load_kif("(instance precondition TransitiveRelation)").
:- load_kif("(domainSubclass precondition 1 Process)").
:- load_kif("(domainSubclass precondition 2 Process)").
:- load_kif("(documentation precondition EnglishLanguage \"A very general &%Predicate. (&%precondition ?PROC1 ?PROC2) means that an instance of ?PROC2 can exist only if an instance of ?PROC1 also exists.\")").
:- load_kif("(=> (precondition ?PROC1 ?PROC2) (=> (exists (?INST2) (instance ?INST2 ?PROC2)) (exists (?INST1) (instance ?INST1 ?PROC1))))").
:- load_kif("(instance hindersSubclass BinaryPredicate)").
:- load_kif("(instance hindersSubclass IrreflexiveRelation)").
:- load_kif("(domainSubclass hindersSubclass 1 Process)").
:- load_kif("(domainSubclass hindersSubclass 2 Process)").
:- load_kif("(documentation hindersSubclass EnglishLanguage \"A very general &%Predicate. (&%hindersSubclass ?PROC1 ?PROC2) means that the &%Process ?PROC1 hindersSubclass or hinders the occurrence of the &%Process ?PROC2. For example, obstructing an object hindersSubclass moving it. Note that this is a relation between types of &%Processes, not between instances.\")").
:- load_kif("(=> (hindersSubclass ?PROC1 ?PROC2) (forall (?TIME ?PLACE) (decreasesLikelihood (holdsDuring ?TIME (exists (?INST1) (and (instance ?INST1 ?PROC1) (eventLocated ?INST1 ?PLACE)))) (holdsDuring ?TIME (exists (?INST2) (and (instance ?INST2 ?PROC2) (eventLocated ?INST2 ?PLACE)))))))").
:- load_kif("(instance preventsSubclass BinaryPredicate)").
:- load_kif("(instance preventsSubclass IrreflexiveRelation)").
:- load_kif("(domainSubclass preventsSubclass 1 Process)").
:- load_kif("(domainSubclass preventsSubclass 2 Process)").
:- load_kif("(relatedInternalConcept preventsSubclass hindersSubclass)").
:- load_kif("(documentation preventsSubclass EnglishLanguage \"A very general &%Predicate. (preventsSubclass ?PROC1 ?PROC2) means that ?PROC1 preventsSubclass the occurrence of ?PROC2. In other words, if ?PROC1 is occurring in a particular time and place, ?PROC2 cannot occur at the same time and place. For example, innoculating preventsSubclass contracting disease. Note that this is a relation between types of &%Processes, not between instances.\")").
:- load_kif("(=> (preventsSubclass ?PROC1 ?PROC2) (forall (?TIME ?PLACE) (=> (holdsDuring ?TIME (exists (?INST1) (and (instance ?INST1 ?PROC1) (eventLocated ?INST1 ?PLACE)))) (not (holdsDuring ?TIME (exists (?INST2) (and (instance ?INST2 ?PROC2) (eventLocated ?INST2 ?PLACE))))))))").
:- load_kif("(instance prevents BinaryPredicate)").
:- load_kif("(instance prevents IrreflexiveRelation)").
:- load_kif("(domain prevents 1 Process)").
:- load_kif("(domainSubclass prevents 2 Process)").
:- load_kif("(relatedInternalConcept prevents preventsSubclass)").
:- load_kif("(relatedInternalConcept prevents hinders)").
:- load_kif("(documentation prevents EnglishLanguage \"A very general &%Predicate. (prevents ?PROC1 ?PROC2) means that an instance of ?PROC1 prevents the occurrence of ?PROC2. Note the difference between this relation and &%preventsSubclass, which is relating classes and not instance-class.\")").
:- load_kif("(=> (and (prevents ?X ?P) (equal (WhenFn ?X) ?T) (eventLocated ?X ?L)) (not (holdsDuring ?T (exists (?Y) (and (instance ?Y ?P) (eventLocated ?Y ?L))))))").
:- load_kif("(instance hinders BinaryPredicate)").
:- load_kif("(instance hinders IrreflexiveRelation)").
:- load_kif("(domain hinders 1 Process)").
:- load_kif("(domainSubclass hinders 2 Process)").
:- load_kif("(relatedInternalConcept hinders hindersSubclass)").
:- load_kif("(documentation hinders EnglishLanguage \"A genral &%Predicate, where (hinders ?PROC1 ?PROC2) means that an instance of ?PROC1 &%decreasesLikelihood of occurrence of ?PROC2. Compare with &%prevents which is stronger and &%hindersSubclass which relates classes of &%Processes.\")").
:- load_kif("(=> (hinders ?X ?PROC) (decreasesLikelihood (and (equal (WhenFn ?X) ?T) (eventLocated ?X ?L)) (holdsDuring ?T (exists (?Y) (and (instance ?Y ?PROC) (eventLocated ?Y ?L))))))").
:- load_kif("(instance refers BinaryPredicate)").
:- load_kif("(domain refers 1 Entity)").
:- load_kif("(domain refers 2 Entity)").
:- load_kif("(documentation refers EnglishLanguage \"(&%refers ?OBJ1 ?OBJ2) means that ?OBJ1 mentions or includes a reference to ?OBJ2. Note that &%refers is more general in meaning than &%represents, because presumably something can represent something else only if it refers to this other thing. For example, an article whose topic is a recent change in the price of oil may refer to many other things, e.g. the general state of the economy, the weather in California, the prospect of global warming, the options for alternative energy sources, the stock prices of various oil companies, etc.\")").
:- load_kif("(subrelation names refers)").
:- load_kif("(domain names 1 SymbolicString)").
:- load_kif("(documentation names EnglishLanguage \"(&%names ?STRING ?ENTITY) means that the thing ?ENTITY has the &%SymbolicString ?STRING as its name. Note that &%names and &%represents are the two immediate &%subrelations of &%refers. The predicate &%names is used when the referring item is merely a tag without connotative content, while the predicate &%represents is used for referring items that have such content.\")").
:- load_kif("(subrelation uniqueIdentifier names)").
:- load_kif("(instance uniqueIdentifier SingleValuedRelation)").
:- load_kif("(documentation uniqueIdentifier EnglishLanguage \"The class of &%names that uniquely identify an instance of &%Entity. Some examples of &%uniqueIdentifiers are the keys of tables in database applications and the ISBN (International Standard Book Number).\")").
:- load_kif("(subrelation represents refers)").
:- load_kif("(documentation represents EnglishLanguage \"A very general semiotics &%Predicate.  (&%represents ?THING ?ENTITY) means that ?THING in some way indicates, expresses, connotes, pictures, describes, etc. ?ENTITY. The &%Predicates &%containsInformation and &%realization are subrelations of &%represents. Note that &%represents is a subrelation of &%refers, since something can represent something else only if it refers to this other thing. See the documentation string for &%names.\")").
:- load_kif("(instance representsForAgent TernaryPredicate)").
:- load_kif("(domain representsForAgent 1 Entity)").
:- load_kif("(domain representsForAgent 2 Entity)").
:- load_kif("(domain representsForAgent 3 Agent)").
:- load_kif("(documentation representsForAgent EnglishLanguage \"A very general predicate.  (&%representsForAgent ?ENTITY1 ?ENTITY2 ?AGENT) means that the ?AGENT chooses to use ?ENTITY1 to 'stand for' ?ENTITY2.\")").
:- load_kif("(=> (representsForAgent ?REP ?ENTITY ?AGENT) (represents ?REP ?ENTITY))").
:- load_kif("(instance representsInLanguage TernaryPredicate)").
:- load_kif("(domain representsInLanguage 1 LinguisticExpression)").
:- load_kif("(domain representsInLanguage 2 Entity)").
:- load_kif("(domain representsInLanguage 3 Language)").
:- load_kif("(documentation representsInLanguage EnglishLanguage \"A very general predicate.  (&%representsInLanguage ?THING ?ENTITY ?LANGUAGE) means that the &%LinguisticExpression ?THING stands for ?ENTITY in the &%Language ?LANGUAGE.\")").
:- load_kif("(=> (representsInLanguage ?REP ?ENTITY ?LANGUAGE) (exists (?AGENT) (representsForAgent ?REP ?ENTITY ?AGENT)))").
:- load_kif("(subrelation equivalentContentClass subsumesContentClass)").
:- load_kif("(instance equivalentContentClass EquivalenceRelation)").
:- load_kif("(domainSubclass equivalentContentClass 1 ContentBearingPhysical)").
:- load_kif("(domainSubclass equivalentContentClass 2 ContentBearingPhysical)").
:- load_kif("(documentation equivalentContentClass EnglishLanguage \"A &%BinaryPredicate that relates two subclasses of &%ContentBearingPhysical. (&%equivalentContentClass ?CLASS1 ?CLASS2) means that the content expressed by each instance of ?CLASS1 is also expressed by each instance of ?CLASS2, and vice versa. An example would be the relationship between English and Russian editions of Agatha Christie's 'Murder on the Orient Express'. Note that"). (&%equivalentContentClass ?CLASS1 ?CLASS2) implies (&%subsumesContentClass ?CLASS1 ?CLASS2) and (&%subsumesContentClass ?CLASS2 ?CLASS1).\")").
:- load_kif("(<=> (and (subsumesContentClass ?CLASS1 ?CLASS2) (subsumesContentClass ?CLASS2 ?CLASS1)) (equivalentContentClass ?CLASS1 ?CLASS2))").
:- load_kif("(instance subsumesContentClass BinaryPredicate)").
:- load_kif("(instance subsumesContentClass PartialOrderingRelation)").
:- load_kif("(domainSubclass subsumesContentClass 1 ContentBearingPhysical)").
:- load_kif("(domainSubclass subsumesContentClass 2 ContentBearingPhysical)").
:- load_kif("(documentation subsumesContentClass EnglishLanguage \"A &%BinaryPredicate that relates two subclasses of &%ContentBearingPhysical. (&%subsumesContentClass ?CLASS1 ?CLASS2) means that the content expressed by each instance of ?CLASS2 is also expressed by each instance of ?CLASS1. Examples include the relationship between a poem and one of its stanzas or between a book and one of its chapters. Note that this is a relation between subclasses of &%ContentBearingObject, rather than instances. If one wants to relate instances, the &%Predicate &%subsumesContentInstance can be used. Note that &%subsumesContentClass is needed in many cases. Consider, for example, the relation between the King James edition of the Bible and its book of Genesis. This relation holds for every copy of this edition and not just for a single instance.\")").
:- load_kif("(=> (subsumesContentClass ?CLASS1 ?CLASS2) (forall (?OBJ2 ?INFO) (=> (and (instance ?OBJ2 ?CLASS2) (containsInformation ?OBJ2 ?INFO)) (exists (?OBJ1) (and (instance ?OBJ1 ?CLASS1) (containsInformation ?OBJ1 ?INFO))))))").
:- load_kif("(subrelation equivalentContentInstance subsumesContentInstance)").
:- load_kif("(instance equivalentContentInstance EquivalenceRelation)").
:- load_kif("(domain equivalentContentInstance 1 ContentBearingPhysical)").
:- load_kif("(domain equivalentContentInstance 2 ContentBearingPhysical)").
:- load_kif("(relatedInternalConcept equivalentContentInstance equivalentContentClass)").
:- load_kif("(documentation equivalentContentInstance EnglishLanguage \"A &%BinaryPredicate relating two instances of &%ContentBearingPhysical. (&%equivalentContentInstance ?OBJ1 ?OBJ2) means that the content expressed by ?OBJ1 is identical to the content expressed by ?OBJ2. An example would be the relationship between a handwritten draft of a letter to one's lawyer and a typed copy of the same letter. Note that (&%equivalentContentInstance ?OBJ1 ?OBJ2) implies (&%subsumesContentInstance ?OBJ1 ?OBJ2) and"). (&%subsumesContentInstance ?OBJ2 ?OBJ2).\")").
:- load_kif("(<=> (and (subsumesContentInstance ?OBJ1 ?OBJ2) (subsumesContentInstance ?OBJ2 ?OBJ1)) (equivalentContentInstance ?OBJ1 ?OBJ2))").
:- load_kif("(instance subsumesContentInstance BinaryPredicate)").
:- load_kif("(instance subsumesContentInstance PartialOrderingRelation)").
:- load_kif("(domain subsumesContentInstance 1 ContentBearingPhysical)").
:- load_kif("(domain subsumesContentInstance 2 ContentBearingPhysical)").
:- load_kif("(relatedInternalConcept subsumesContentInstance subsumesContentClass)").
:- load_kif("(documentation subsumesContentInstance EnglishLanguage \"A &%BinaryPredicate relating two instances of &%ContentBearingPhysical. (&%subsumesContentInstance ?OBJ1 ?OBJ2) means that the content expressed by ?OBJ2 is part of the content expressed by ?OBJ1. An example is the relationship between a handwritten poem and one of its stanzas. Note that this is a relation between instances, rather than &%Classes. If one wants to assert a content relationship between &%Classes, e.g. between the version of an intellectual work and a part of that work, the relation &%subsumesContentClass should be used.\")").
:- load_kif("(=> (subsumesContentInstance ?OBJ1 ?OBJ2) (forall (?INFO) (=> (containsInformation ?OBJ2 ?INFO) (containsInformation ?OBJ1 ?INFO))))").
:- load_kif("(subrelation realization represents)").
:- load_kif("(instance realization AsymmetricRelation)").
:- load_kif("(domain realization 1 Process)").
:- load_kif("(domain realization 2 Proposition)").
:- load_kif("(relatedInternalConcept realization equivalentContentInstance)").
:- load_kif("(relatedInternalConcept realization containsInformation)").
:- load_kif("(documentation realization EnglishLanguage \"A subrelation of &%represents.  (&%realization ?PROCESS ?PROP) means that ?PROCESS is a Process which expresses the content of ?PROP. Examples include a particular musical performance, which realizes the content of a musical score, or the reading of a poem.\")").
:- load_kif("(=> (realization ?PROCESS ?PROP) (exists (?OBJ) (and (instance ?OBJ ContentBearingObject) (containsInformation ?OBJ ?PROP))))").
:- load_kif("(instance expressedInLanguage BinaryPredicate)").
:- load_kif("(instance expressedInLanguage AsymmetricRelation)").
:- load_kif("(domain expressedInLanguage 1 LinguisticExpression)").
:- load_kif("(domain expressedInLanguage 2 Language)").
:- load_kif("(documentation expressedInLanguage EnglishLanguage \"(&%expressedInLanguage ?EXPRESS ?LANG) means that the &%LinguisticExpression ?EXPRESS is part of the &%Language ?LANG.\")").
:- load_kif("(<=> (expressedInLanguage ?EXPRESS ?LANGUAGE) (exists (?PROP) (representsInLanguage ?EXPRESS ?PROP ?LANGUAGE)))").
:- load_kif("(instance subProposition BinaryPredicate)").
:- load_kif("(instance subProposition TransitiveRelation)").
:- load_kif("(instance subProposition IrreflexiveRelation)").
:- load_kif("(domain subProposition 1 Proposition)").
:- load_kif("(domain subProposition 2 Proposition)").
:- load_kif("(documentation subProposition EnglishLanguage \"(&%subProposition ?PROP1 ?PROP2) means that ?PROP1 is a &%Proposition which is a proper part of the &%Proposition ?PROP2. In other words, &%subProposition is the analogue of &%properPart for chunks of abstract content.\")").
:- load_kif("(=> (subProposition ?PROP1 ?PROP2) (forall (?OBJ1 ?OBJ2) (=> (and (containsInformation ?OBJ1 ?PROP1) (containsInformation ?OBJ2 ?PROP2)) (subsumesContentInstance ?OBJ2 ?OBJ1))))").
:- load_kif("(subrelation subPlan subProposition)").
:- load_kif("(instance subPlan TransitiveRelation)").
:- load_kif("(instance subPlan IrreflexiveRelation)").
:- load_kif("(domain subPlan 1 Plan)").
:- load_kif("(domain subPlan 2 Plan)").
:- load_kif("(documentation subPlan EnglishLanguage \"(&%subPlan ?PLAN1 ?PLAN2) means that ?PLAN1 is a &%Plan which is a proper part of ?PLAN2. This relation is generally used to relate a supporting &%Plan to the overall &%Plan in a particular context.\")").
:- load_kif("(instance uses BinaryPredicate)").
:- load_kif("(instance uses AsymmetricRelation)").
:- load_kif("(domain uses 1 Object)").
:- load_kif("(domain uses 2 Agent)").
:- load_kif("(documentation uses EnglishLanguage \"(&%uses ?OBJECT AGENT) means that ?OBJECT is used by ?AGENT as an instrument in an unspecified &%Process. This &%Predicate, as its corresponding axiom indicates, is a composition of the &%CaseRoles &%agent and &%instrument.\")").
:- load_kif("(=> (uses ?OBJ ?AGENT) (exists (?PROC) (and (agent ?PROC ?AGENT) (instrument ?PROC ?OBJ))))").
% :- load_kif("; END FILE").
% :- load_kif("; BEGIN FILE").
% :- load_kif(";  NUMERIC FUNCTIONS  ").
% :- load_kif("; INCLUDES 'STRUCTURAL ONTOLOGY' INCLUDES 'BASE ONTOLOGY'").
:- load_kif("(instance MultiplicationFn BinaryFunction)").
:- load_kif("(instance MultiplicationFn AssociativeFunction)").
:- load_kif("(instance MultiplicationFn CommutativeFunction)").
:- load_kif("(instance MultiplicationFn RelationExtendedToQuantities)").
:- load_kif("(instance MultiplicationFn TotalValuedRelation)").
:- load_kif("(domain MultiplicationFn 1 Quantity)").
:- load_kif("(domain MultiplicationFn 2 Quantity)").
:- load_kif("(range MultiplicationFn Quantity)").
:- load_kif("(documentation MultiplicationFn EnglishLanguage \"If ?NUMBER1 and ?NUMBER2 are &%Numbers, then (&%MultiplicationFn ?NUMBER1 ?NUMBER2) is the arithmetical product of these numbers.\")").
:- load_kif("(instance AdditionFn BinaryFunction)").
:- load_kif("(instance AdditionFn AssociativeFunction)").
:- load_kif("(instance AdditionFn CommutativeFunction)").
:- load_kif("(instance AdditionFn RelationExtendedToQuantities)").
:- load_kif("(instance AdditionFn TotalValuedRelation)").
:- load_kif("(domain AdditionFn 1 Quantity)").
:- load_kif("(domain AdditionFn 2 Quantity)").
:- load_kif("(range AdditionFn Quantity)").
:- load_kif("(documentation AdditionFn EnglishLanguage \"If ?NUMBER1 and ?NUMBER2 are &%Numbers, then"). (&%AdditionFn ?NUMBER1 ?NUMBER2) is the arithmetical sum of these numbers.\")").
:- load_kif("(forall (?NUMBER) (equal (SuccessorFn ?NUMBER) (AdditionFn ?NUMBER 1)))").
:- load_kif("(instance SubtractionFn BinaryFunction)").
:- load_kif("(instance SubtractionFn AssociativeFunction)").
:- load_kif("(instance SubtractionFn RelationExtendedToQuantities)").
:- load_kif("(instance SubtractionFn TotalValuedRelation)").
:- load_kif("(domain SubtractionFn 1 Quantity)").
:- load_kif("(domain SubtractionFn 2 Quantity)").
:- load_kif("(range SubtractionFn Quantity)").
:- load_kif("(documentation SubtractionFn EnglishLanguage \"If ?NUMBER1 and ?NUMBER2 are &%Numbers, then (&%SubtractionFn ?NUMBER1 ?NUMBER2) is the arithmetical difference between ?NUMBER1 and ?NUMBER2, i.e. ?NUMBER1 minus ?NUMBER2. An exception occurs when ?NUMBER1 is equal to 0, in which case"). (&%SubtractionFn ?NUMBER1 ?NUMBER2) is the negation of ?NUMBER2.\")").
:- load_kif("(forall (?NUMBER) (equal (PredecessorFn ?NUMBER) (SubtractionFn ?NUMBER 1)))").
:- load_kif("(instance DivisionFn BinaryFunction)").
:- load_kif("(instance DivisionFn AssociativeFunction)").
:- load_kif("(instance DivisionFn RelationExtendedToQuantities)").
:- load_kif("(instance DivisionFn PartialValuedRelation)").
:- load_kif("(domain DivisionFn 1 Quantity)").
:- load_kif("(domain DivisionFn 2 Quantity)").
:- load_kif("(range DivisionFn Quantity)").
:- load_kif("(documentation DivisionFn EnglishLanguage \"If ?NUMBER1 and ?NUMBER2 are &%Numbers, then"). (&%DivisionFn ?NUMBER1 ?NUMBER2) is the result of dividing ?NUMBER1 by ?NUMBER2. Note that when ?NUMBER1 = 1 (&%DivisionFn ?NUMBER1 ?NUMBER2) is the reciprocal of ?NUMBER2. Note too that (&%DivisionFn ?NUMBER1 ?NUMBER2) is undefined when ?NUMBER2 = 0.\")").
:- load_kif("(=> (instance ?NUMBER RationalNumber) (exists (?INT1 ?INT2) (and (instance ?INT1 Integer) (instance ?INT2 Integer) (equal ?NUMBER (DivisionFn ?INT1 ?INT2)))))").
:- load_kif("(instance AbsoluteValueFn UnaryFunction)").
:- load_kif("(instance AbsoluteValueFn TotalValuedRelation)").
:- load_kif("(domain AbsoluteValueFn 1 RealNumber)").
:- load_kif("(range AbsoluteValueFn NonnegativeRealNumber)").
:- load_kif("(documentation AbsoluteValueFn EnglishLanguage \"The value of (&%AbsoluteValueFn ?NUMBER) is the absolute value of the &%RealNumber ?NUMBER.\")").
:- load_kif("(<=> (and (equal (AbsoluteValueFn ?NUMBER1) ?NUMBER2) (instance ?NUMBER1 RealNumber) (instance ?NUMBER2 RealNumber)) (or (and (instance ?NUMBER1 NonnegativeRealNumber) (equal ?NUMBER1 ?NUMBER2)) (and (instance ?NUMBER1 NegativeRealNumber) (equal ?NUMBER2 (SubtractionFn 0 ?NUMBER1)))))").
:- load_kif("(instance CeilingFn UnaryFunction)").
:- load_kif("(instance CeilingFn TotalValuedRelation)").
:- load_kif("(domain CeilingFn 1 RealNumber)").
:- load_kif("(range CeilingFn Integer)").
:- load_kif("(documentation CeilingFn EnglishLanguage \"(&%CeilingFn ?NUMBER) returns the smallest &%Integer greater than or equal to the &%RealNumber ?NUMBER.\")").
:- load_kif("(=> (equal (CeilingFn ?NUMBER) ?INT) (not (exists (?OTHERINT) (and (instance ?OTHERINT Integer) (greaterThanOrEqualTo ?OTHERINT ?NUMBER) (lessThan ?OTHERINT ?INT)))))").
:- load_kif("(instance CosineFn UnaryFunction)").
:- load_kif("(instance CosineFn TotalValuedRelation)").
:- load_kif("(domain CosineFn 1 PlaneAngleMeasure)").
:- load_kif("(range CosineFn RealNumber)").
:- load_kif("(documentation CosineFn EnglishLanguage \"(&%CosineFn ?DEGREE) returns the cosine of the &%PlaneAngleMeasure ?DEGREE. The cosine of ?DEGREE is the ratio of the side next to ?DEGREE to the hypotenuse in a right-angled triangle.\")").
:- load_kif("(instance DenominatorFn UnaryFunction)").
:- load_kif("(instance DenominatorFn TotalValuedRelation)").
:- load_kif("(domain DenominatorFn 1 RealNumber)").
:- load_kif("(range DenominatorFn Integer)").
:- load_kif("(documentation DenominatorFn EnglishLanguage \"(&%DenominatorFn ?NUMBER) returns the denominator of the canonical reduced form of the &%RealNumber ?NUMBER.\")").
:- load_kif("(instance ExponentiationFn BinaryFunction)").
:- load_kif("(instance ExponentiationFn RelationExtendedToQuantities)").
:- load_kif("(instance ExponentiationFn TotalValuedRelation)").
:- load_kif("(domain ExponentiationFn 1 Quantity)").
:- load_kif("(domain ExponentiationFn 2 Integer)").
:- load_kif("(range ExponentiationFn Quantity)").
:- load_kif("(documentation ExponentiationFn EnglishLanguage \"(&%ExponentiationFn ?NUMBER ?INT) returns the &%RealNumber ?NUMBER raised to the power of the &%Integer ?INT.\")").
:- load_kif("(instance FloorFn UnaryFunction)").
:- load_kif("(instance FloorFn TotalValuedRelation)").
:- load_kif("(domain FloorFn 1 RealNumber)").
:- load_kif("(range FloorFn Integer)").
:- load_kif("(documentation FloorFn EnglishLanguage \"(&%FloorFn ?NUMBER) returns the largest &%Integer less than or equal to the &%RealNumber ?NUMBER.\")").
:- load_kif("(=> (equal (FloorFn ?NUMBER) ?INT) (not (exists (?OTHERINT) (and (instance ?OTHERINT Integer) (lessThanOrEqualTo ?OTHERINT ?NUMBER) (greaterThan ?OTHERINT ?INT)))))").
:- load_kif("(instance GreatestCommonDivisorFn Function)").
:- load_kif("(instance GreatestCommonDivisorFn VariableArityRelation)").
:- load_kif("(instance GreatestCommonDivisorFn PartialValuedRelation)").
:- load_kif("(range GreatestCommonDivisorFn Integer)").
:- load_kif("(documentation GreatestCommonDivisorFn EnglishLanguage \"(&%GreatestCommonDivisorFn ?NUMBER1 ?NUMBER2 ... ?NUMBER) returns the greatest common divisor of ?NUMBER1 through ?NUMBER.\")").
:- load_kif("(=> (equal (GreatestCommonDivisorFn @ROW) ?NUMBER) (forall (?ELEMENT) (=> (inList ?ELEMENT (ListFn @ROW)) (equal (RemainderFn ?ELEMENT ?NUMBER) 0))))").
:- load_kif("(=> (equal (GreatestCommonDivisorFn @ROW) ?NUMBER) (not (exists (?GREATER) (and (greaterThan ?GREATER ?NUMBER) (forall (?ELEMENT) (=> (inList ?ELEMENT (ListFn @ROW)) (equal (RemainderFn ?ELEMENT ?GREATER) 0)))))))").
:- load_kif("(documentation multiplicativeFactor EnglishLanguage \"(multiplicativeFactor ?NUMBER1 ?NUMBER2) means that ?NUMBER1 is a factor of ?NUMBER2, i.e. ?NUMBER1 can be multiplied by some &%Integer to give ?NUMBER2 as a result.\")").
:- load_kif("(instance multiplicativeFactor BinaryPredicate)").
:- load_kif("(instance multiplicativeFactor TransitiveRelation)").
:- load_kif("(domain multiplicativeFactor 1 Integer)").
:- load_kif("(domain multiplicativeFactor 2 Integer)").
:- load_kif("(=> (multiplicativeFactor ?N1 ?N2) (exists (?I) (and (instance ?I Integer) (equal ?N2 (MultiplicationFn ?N1 ?I)))))").
:- load_kif("(instance ImaginaryPartFn UnaryFunction)").
:- load_kif("(instance ImaginaryPartFn TotalValuedRelation)").
:- load_kif("(domain ImaginaryPartFn 1 ComplexNumber)").
:- load_kif("(range ImaginaryPartFn ImaginaryNumber)").
:- load_kif("(documentation ImaginaryPartFn EnglishLanguage \"(&%ImaginaryPartFn ?NUMBER) returns the part of ?NUMBER that has the square root of -1 as its factor.\")").
:- load_kif("(=> (instance ?NUMBER ComplexNumber) (exists (?PART1 ?PART2) (and (equal ?PART1 (RealNumberFn ?NUMBER)) (equal ?PART2 (ImaginaryPartFn ?NUMBER)))))").
:- load_kif("(instance IntegerSquareRootFn UnaryFunction)").
:- load_kif("(instance IntegerSquareRootFn PartialValuedRelation)").
:- load_kif("(domain IntegerSquareRootFn 1 RealNumber)").
:- load_kif("(range IntegerSquareRootFn NonnegativeInteger)").
:- load_kif("(documentation IntegerSquareRootFn EnglishLanguage \"(&%IntegerSquareRootFn ?NUMBER) returns the integer square root of ?NUMBER.\")").
:- load_kif("(instance LeastCommonMultipleFn Function)").
:- load_kif("(instance LeastCommonMultipleFn PartialValuedRelation)").
:- load_kif("(instance LeastCommonMultipleFn VariableArityRelation)").
:- load_kif("(range LeastCommonMultipleFn Integer)").
:- load_kif("(documentation LeastCommonMultipleFn EnglishLanguage \"(&%LeastCommonMultipleFn ?NUMBER1 ?NUMBER2 ... ?NUMBER) returns the least common multiple of ?NUMBER1 through ?NUMBER.\")").
:- load_kif("(=> (equal (LeastCommonMultipleFn @ROW) ?NUMBER) (forall (?ELEMENT) (=> (inList ?ELEMENT (ListFn @ROW)) (equal (RemainderFn ?NUMBER ?ELEMENT) 0))))").
:- load_kif("(=> (equal (LeastCommonMultipleFn @ROW) ?NUMBER) (not (exists (?LESS) (and (lessThan ?LESS ?NUMBER) (forall (?ELEMENT) (=> (inList ?ELEMENT (ListFn @ROW)) (equal (RemainderFn ?LESS ?ELEMENT) 0)))))))").
:- load_kif("(instance LogFn BinaryFunction)").
:- load_kif("(domain LogFn 1 RealNumber)").
:- load_kif("(domain LogFn 2 PositiveInteger)").
:- load_kif("(range LogFn RealNumber)").
:- load_kif("(documentation LogFn EnglishLanguage \"(LogFn ?NUMBER ?INT) returns the logarithm of the &%RealNumber ?NUMBER in the base denoted by the &%Integer ?INT.\")").
:- load_kif("(instance MaxFn BinaryFunction)").
:- load_kif("(instance MaxFn AssociativeFunction)").
:- load_kif("(instance MaxFn CommutativeFunction)").
:- load_kif("(instance MaxFn RelationExtendedToQuantities)").
:- load_kif("(instance MaxFn TotalValuedRelation)").
:- load_kif("(domain MaxFn 1 Quantity)").
:- load_kif("(domain MaxFn 2 Quantity)").
:- load_kif("(range MaxFn Quantity)").
:- load_kif("(documentation MaxFn EnglishLanguage \"(&%MaxFn ?NUMBER1 ?NUMBER2) is the largest of ?NUMBER1 and ?NUMBER2. In cases where ?NUMBER1 is equal to ?NUMBER2, &%MaxFn returns one of its arguments.\")").
:- load_kif("(=> (equal (MaxFn ?NUMBER1 ?NUMBER2) ?NUMBER) (or (and (equal ?NUMBER ?NUMBER1) (greaterThan ?NUMBER1 ?NUMBER2)) (and (equal ?NUMBER ?NUMBER2) (greaterThan ?NUMBER2 ?NUMBER1)) (and (equal ?NUMBER ?NUMBER1) (equal ?NUMBER ?NUMBER2))))").
:- load_kif("(instance MinFn BinaryFunction)").
:- load_kif("(instance MinFn AssociativeFunction)").
:- load_kif("(instance MinFn CommutativeFunction)").
:- load_kif("(instance MinFn RelationExtendedToQuantities)").
:- load_kif("(instance MinFn TotalValuedRelation)").
:- load_kif("(domain MinFn 1 Quantity)").
:- load_kif("(domain MinFn 2 Quantity)").
:- load_kif("(range MinFn Quantity)").
:- load_kif("(documentation MinFn EnglishLanguage \"(&%MinFn ?NUMBER1 ?NUMBER2) is the smallest of ?NUMBER1 and ?NUMBER2. In cases where ?NUMBER1 is equal to ?NUMBER2, &%MinFn returns one of its arguments.\")").
:- load_kif("(=> (equal (MinFn ?NUMBER1 ?NUMBER2) ?NUMBER) (or (and (equal ?NUMBER ?NUMBER1) (lessThan ?NUMBER1 ?NUMBER2)) (and (equal ?NUMBER ?NUMBER2) (lessThan ?NUMBER2 ?NUMBER1)) (and (equal ?NUMBER ?NUMBER1) (equal ?NUMBER ?NUMBER2))))").
:- load_kif("(instance NumeratorFn UnaryFunction)").
:- load_kif("(instance NumeratorFn TotalValuedRelation)").
:- load_kif("(domain NumeratorFn 1 RealNumber)").
:- load_kif("(range NumeratorFn Integer)").
:- load_kif("(documentation NumeratorFn EnglishLanguage \"(&%NumeratorFn ?NUMBER) returns the numerator of the canonical reduced form ?NUMBER.\")").
:- load_kif("(instance Pi PositiveRealNumber)").
:- load_kif("(documentation Pi EnglishLanguage \"&%Pi is the &%RealNumber that is the ratio of the perimeter of a circle to its diameter. It is approximately equal to 3.141592653589793.\")").
:- load_kif("(instance NumberE PositiveRealNumber)").
:- load_kif("(documentation NumberE EnglishLanguage \"&%NumberE is the &%RealNumber that is the base for natural logarithms. It is approximately equal to 2.718282.\")").
:- load_kif("(instance RationalNumberFn UnaryFunction)").
:- load_kif("(domain RationalNumberFn 1 Number)").
:- load_kif("(range RationalNumberFn RationalNumber)").
:- load_kif("(documentation RationalNumberFn EnglishLanguage \"(&%RationalNumberFn ?NUMBER) returns the rational representation of ?NUMBER.\")").
:- load_kif("(instance RealNumberFn UnaryFunction)").
:- load_kif("(domain RealNumberFn 1 Number)").
:- load_kif("(range RealNumberFn RealNumber)").
:- load_kif("(documentation RealNumberFn EnglishLanguage \"(RealNumberFn ?NUMBER) returns the part of ?NUMBER that is a &%RealNumber.\")").
:- load_kif("(instance ReciprocalFn UnaryFunction)").
:- load_kif("(instance ReciprocalFn RelationExtendedToQuantities)").
:- load_kif("(instance ReciprocalFn TotalValuedRelation)").
:- load_kif("(domain ReciprocalFn 1 Quantity)").
:- load_kif("(range ReciprocalFn Quantity)").
:- load_kif("(documentation ReciprocalFn EnglishLanguage \"(ReciprocalFn ?NUMBER) is the reciprocal element of ?NUMBER with respect to the multiplication operator"). (&%MultiplicationFn), i.e. 1/?NUMBER. Not all numbers have a reciprocal element. For example the number 0 does not. If a number ?NUMBER has a reciprocal ?RECIP, then the product of ?NUMBER and ?RECIP will be 1, e.g. 3*1/3 = 1. The reciprocal of an element is &%equal to applying the &%ExponentiationFn function to the element to the power -1.\")").
:- load_kif("(=> (instance ?NUMBER Quantity) (equal (ReciprocalFn ?NUMBER) (ExponentiationFn ?NUMBER -1)))").
:- load_kif("(=> (instance ?NUMBER Quantity) (equal 1 (MultiplicationFn ?NUMBER (ReciprocalFn ?NUMBER))))").
:- load_kif("(instance RemainderFn BinaryFunction)").
:- load_kif("(instance RemainderFn RelationExtendedToQuantities)").
:- load_kif("(instance RemainderFn PartialValuedRelation)").
:- load_kif("(domain RemainderFn 1 Quantity)").
:- load_kif("(domain RemainderFn 2 Quantity)").
:- load_kif("(range RemainderFn Quantity)").
:- load_kif("(documentation RemainderFn EnglishLanguage \"(RemainderFn ?NUMBER ?DIVISOR) is the remainder of the number ?NUMBER divided by the number ?DIVISOR. The result has the same sign as ?DIVISOR.\")").
:- load_kif("(<=> (equal (RemainderFn ?NUMBER1 ?NUMBER2) ?NUMBER) (equal (AdditionFn (MultiplicationFn (FloorFn (DivisionFn ?NUMBER1 ?NUMBER2)) ?NUMBER2) ?NUMBER) ?NUMBER1))").
:- load_kif("(=> (equal (RemainderFn ?NUMBER1 ?NUMBER2) ?NUMBER) (equal (SignumFn ?NUMBER2) (SignumFn ?NUMBER)))").
:- load_kif("(=> (instance ?NUMBER EvenInteger) (equal (RemainderFn ?NUMBER 2) 0))").
:- load_kif("(=> (instance ?NUMBER OddInteger) (equal (RemainderFn ?NUMBER 2) 1))").
:- load_kif("(=> (instance ?PRIME PrimeNumber) (forall (?NUMBER) (=> (equal (RemainderFn ?PRIME ?NUMBER) 0) (or (equal ?NUMBER 1) (equal ?NUMBER ?PRIME)))))").
:- load_kif("(instance RoundFn UnaryFunction)").
:- load_kif("(instance RoundFn RelationExtendedToQuantities)").
:- load_kif("(instance RoundFn TotalValuedRelation)").
:- load_kif("(domain RoundFn 1 Quantity)").
:- load_kif("(range RoundFn Quantity)").
:- load_kif("(documentation RoundFn EnglishLanguage \"(&%RoundFn ?NUMBER) is the &%Integer closest to ?NUMBER on the number line. If ?NUMBER is halfway between two &%Integers (for example 3.5), it denotes the larger &%Integer.\")").
:- load_kif("(=> (equal (RoundFn ?NUMBER1) ?NUMBER2) (or (=> (lessThan (SubtractionFn ?NUMBER1 (FloorFn ?NUMBER1)) 0.5) (equal ?NUMBER2 (FloorFn ?NUMBER1))) (=> (greaterThanOrEqualTo (SubtractionFn ?NUMBER1 (FloorFn ?NUMBER1)) 0.5) (equal ?NUMBER2 (CeilingFn ?NUMBER1)))))").
:- load_kif("(instance SignumFn UnaryFunction)").
:- load_kif("(instance SignumFn TotalValuedRelation)").
:- load_kif("(domain SignumFn 1 RealNumber)").
:- load_kif("(range SignumFn Integer)").
:- load_kif("(documentation SignumFn EnglishLanguage \"(SignumFn ?NUMBER) denotes the sign of ?NUMBER. This is one of the following values: -1, 1, or 0.\")").
:- load_kif("(=> (instance ?NUMBER NonnegativeRealNumber) (or (equal (SignumFn ?NUMBER) 1) (equal (SignumFn ?NUMBER) 0)))").
:- load_kif("(=> (instance ?NUMBER PositiveRealNumber) (equal (SignumFn ?NUMBER) 1))").
:- load_kif("(=> (instance ?NUMBER NegativeRealNumber) (equal (SignumFn ?NUMBER) -1))").
:- load_kif("(instance SineFn UnaryFunction)").
:- load_kif("(instance SineFn TotalValuedRelation)").
:- load_kif("(domain SineFn 1 PlaneAngleMeasure)").
:- load_kif("(range SineFn RealNumber)").
:- load_kif("(documentation SineFn EnglishLanguage \"(&%SineFn ?DEGREE) is the sine of the &%PlaneAngleMeasure ?DEGREE. The sine of ?DEGREE is the ratio of the side opposite ?DEGREE to the hypotenuse in a right-angled triangle.\")").
:- load_kif("(instance SquareRootFn UnaryFunction)").
:- load_kif("(domain SquareRootFn 1 RealNumber)").
:- load_kif("(range SquareRootFn Number)").
:- load_kif("(documentation SquareRootFn EnglishLanguage \"(SquareRootFn ?NUMBER) is the principal square root of ?NUMBER.\")").
:- load_kif("(=> (equal (SquareRootFn ?NUMBER1) ?NUMBER2) (equal (MultiplicationFn ?NUMBER2 ?NUMBER2) ?NUMBER1))").
:- load_kif("(instance TangentFn UnaryFunction)").
:- load_kif("(instance TangentFn TotalValuedRelation)").
:- load_kif("(domain TangentFn 1 PlaneAngleMeasure)").
:- load_kif("(range TangentFn RealNumber)").
:- load_kif("(documentation TangentFn EnglishLanguage \"(&%TangentFn ?DEGREE) is the tangent of the &%PlaneAngleMeasure ?DEGREE. The tangent of ?DEGREE is the ratio of the side opposite ?DEGREE to the side next to ?DEGREE in a right-angled triangle.\")").
:- load_kif("(=> (instance ?DEGREE PlaneAngleMeasure) (equal (TangentFn ?DEGREE) (DivisionFn (SineFn ?DEGREE) (CosineFn ?DEGREE))))").
:- load_kif("(instance identityElement BinaryPredicate)").
:- load_kif("(instance identityElement AsymmetricRelation)").
:- load_kif("(domain identityElement 1 BinaryFunction)").
:- load_kif("(domain identityElement 2 Entity)").
:- load_kif("(documentation identityElement EnglishLanguage \"An object ?ID is the identity element for BinaryFunction ?FUNCTION just in case, for every instance ?INST, applying ?FUNCTION to ?INST and ?ID results in ?INST.\")").
:- load_kif("(=> (identityElement ?FUNCTION ?ID) (forall (?INST) (=> (and (domain ?FUNCTION 1 ?CLASS) (instance ?INST ?CLASS)) (equal (AssignmentFn ?FUNCTION ?ID ?INST) ?INST))))").
:- load_kif("(identityElement MultiplicationFn 1)").
:- load_kif("(identityElement AdditionFn 0)").
:- load_kif("(identityElement SubtractionFn 0)").
:- load_kif("(identityElement DivisionFn 1)").
:- load_kif("(instance SuccessorFn UnaryFunction)").
:- load_kif("(instance SuccessorFn TotalValuedRelation)").
:- load_kif("(domain SuccessorFn 1 Integer)").
:- load_kif("(range SuccessorFn Integer)").
:- load_kif("(documentation SuccessorFn EnglishLanguage \"A &%UnaryFunction that maps an &%Integer to its successor, e.g. the successor of 5 is 6.\")").
:- load_kif("(=> (equal (SuccessorFn ?INT1) (SuccessorFn ?INT2)) (equal ?INT1 ?INT2))").
:- load_kif("(=> (instance ?INT Integer) (lessThan ?INT (SuccessorFn ?INT)))").
:- load_kif("(=> (and (instance ?INT1 Integer) (instance ?INT2 Integer)) (not (and (lessThan ?INT1 ?INT2) (lessThan ?INT2 (SuccessorFn ?INT1)))))").
:- load_kif("(=> (instance ?INT Integer) (equal ?INT (SuccessorFn (PredecessorFn ?INT))))").
:- load_kif("(=> (instance ?INT Integer) (equal ?INT (PredecessorFn (SuccessorFn ?INT))))").
:- load_kif("(instance PredecessorFn UnaryFunction)").
:- load_kif("(instance PredecessorFn TotalValuedRelation)").
:- load_kif("(domain PredecessorFn 1 Integer)").
:- load_kif("(range PredecessorFn Integer)").
:- load_kif("(documentation PredecessorFn EnglishLanguage \"A &%UnaryFunction that maps an &%Integer to its predecessor, e.g. the predecessor of 5 is 4.\")").
:- load_kif("(=> (equal (PredecessorFn ?INT1) (PredecessorFn ?INT2)) (equal ?INT1 ?INT2))").
:- load_kif("(=> (instance ?INT Integer) (greaterThan ?INT (PredecessorFn ?INT)))").
:- load_kif("(=> (and (instance ?INT1 Integer) (instance ?INT2 Integer)) (not (and (lessThan ?INT2 ?INT1) (lessThan (PredecessorFn ?INT1) ?INT2))))").
:- load_kif("(instance average PartialValuedRelation)").
:- load_kif("(instance average BinaryPredicate)").
:- load_kif("(instance average SingleValuedRelation)").
:- load_kif("(domain average 1 List)").
:- load_kif("(domain average 2 RealNumber)").
:- load_kif("(termFormat EnglishLanguage average \"average\")").
:- load_kif("(documentation average EnglishLanguage \"A partial function that relates a &%List to a &%RealNumber, provided that the &%List only has list elements that are &%RealNumbers. The &%RealNumber associated with the &%List is equal to the mathematical average of the &%RealNumbers in the &%List divided by the total number of list elements.\")").
:- load_kif("(=> (average ?LIST ?AVERAGE) (forall (?LISTITEM) (=> (inList ?LISTITEM ?LIST) (instance ?LISTITEM RealNumber))))").
% :- load_kif("; END FILE").
% :- load_kif("; BEGIN FILE").
% :- load_kif(";   SET/CLASS THEORY   ").
% :- load_kif("; INCLUDES 'STRUCTURAL ONTOLOGY' INCLUDES 'BASE ONTOLOGY'").
% :- load_kif("; The following part of the ontology covers set-theoretic predicates and functions. Most of the content here is taken from the kif-sets ontology (available on the Ontolingua server).").
:- load_kif("(subrelation subset subclass)").
:- load_kif("(domain subset 1 Set)").
:- load_kif("(domain subset 2 Set)").
:- load_kif("(documentation subset EnglishLanguage \"(subset ?SET1 ?SET2) is true just in case the &%elements of the &%Set ?SET1 are also &%elements of the &%Set ?SET2.\")").
:- load_kif("(=> (subset ?SUBSET ?SET) (forall (?ELEMENT) (=> (element ?ELEMENT ?SUBSET) (element ?ELEMENT ?SET))))").
:- load_kif("(instance element BinaryPredicate)").
:- load_kif("(instance element AsymmetricRelation)").
:- load_kif("(subrelation element instance)").
:- load_kif("(domain element 1 Entity)").
:- load_kif("(domain element 2 Set)").
:- load_kif("(documentation element EnglishLanguage \"(element ?ENTITY ?SET) is true just in case ?ENTITY is contained in the &%Set ?SET. An &%Entity can be an &%element of another &%Entity only if the latter is a &%Set.\")").
:- load_kif("(=> (forall (?ELEMENT) (<=> (element ?ELEMENT ?SET1) (element ?ELEMENT ?SET2))) (equal ?SET1 ?SET2))").
:- load_kif("(instance UnionFn BinaryFunction)").
:- load_kif("(instance UnionFn TotalValuedRelation)").
:- load_kif("(domain UnionFn 1 SetOrClass)").
:- load_kif("(domain UnionFn 2 SetOrClass)").
:- load_kif("(range UnionFn SetOrClass)").
:- load_kif("(documentation UnionFn EnglishLanguage \"A &%BinaryFunction that maps two &%SetOrClasses to the union of these &%SetOrClasses. An object is an &%element of the union of two &%SetOrClasses just in case it is an &%instance of either &%SetOrClass.\")").
:- load_kif("(<=> (equal ?U (UnionFn ?C1 ?C2)) (forall (?I1 ?I2 ?I3) (=> (and (instance ?I1 ?C1) (instance ?I2 ?C2) (instance ?I3 ?U)) (and (instance ?I1 ?U) (instance ?I2 ?U) (or (instance ?I3 ?C1) (instance ?I3 ?C2))))))").
:- load_kif("(instance IntersectionFn BinaryFunction)").
:- load_kif("(instance IntersectionFn TotalValuedRelation)").
:- load_kif("(domain IntersectionFn 1 SetOrClass)").
:- load_kif("(domain IntersectionFn 2 SetOrClass)").
:- load_kif("(range IntersectionFn SetOrClass)").
:- load_kif("(documentation IntersectionFn EnglishLanguage \"A &%BinaryFunction that maps two &%SetOrClasses to the intersection of these &%SetOrClasses. An object is an instance of the intersection of two &%SetOrClasses just in case it is an instance of both of those &%SetOrClasses.\")").
:- load_kif("(instance RelativeComplementFn BinaryFunction)").
:- load_kif("(instance RelativeComplementFn TotalValuedRelation)").
:- load_kif("(domain RelativeComplementFn 1 SetOrClass)").
:- load_kif("(domain RelativeComplementFn 2 SetOrClass)").
:- load_kif("(range RelativeComplementFn SetOrClass)").
:- load_kif("(documentation RelativeComplementFn EnglishLanguage \"A &%BinaryFunction that maps two &%SetOrClasses to the difference between these &%SetOrClasses. More precisely, (&%RelativeComplementFn ?CLASS1 ?CLASS2) denotes the instances of ?CLASS1 that are not also instances of ?CLASS2.\")").
:- load_kif("(instance ComplementFn UnaryFunction)").
:- load_kif("(instance ComplementFn TotalValuedRelation)").
:- load_kif("(domain ComplementFn 1 SetOrClass)").
:- load_kif("(range ComplementFn SetOrClass)").
:- load_kif("(documentation ComplementFn EnglishLanguage \"The complement of a given &%SetOrClass C is the &%SetOrClass of all things that are not instances of C. In other words, an object is an instance of the complement of a &%SetOrClass C just in case it is not an instance of C.\")").
:- load_kif("(instance GeneralizedUnionFn UnaryFunction)").
:- load_kif("(instance GeneralizedUnionFn TotalValuedRelation)").
:- load_kif("(domainSubclass GeneralizedUnionFn 1 SetOrClass)").
:- load_kif("(range GeneralizedUnionFn SetOrClass)").
:- load_kif("(documentation GeneralizedUnionFn EnglishLanguage \"A &%UnaryFunction that takes a &%SetOrClass of &%Classes as its single argument and returns a &%SetOrClass which is the merge of all of the &%Classes in the original &%SetOrClass, i.e. the &%SetOrClass containing just those instances which are instances of an instance of the original &%SetOrClass.\")").
:- load_kif("(instance GeneralizedIntersectionFn UnaryFunction)").
:- load_kif("(instance GeneralizedIntersectionFn TotalValuedRelation)").
:- load_kif("(domainSubclass GeneralizedIntersectionFn 1 SetOrClass)").
:- load_kif("(range GeneralizedIntersectionFn SetOrClass)").
:- load_kif("(documentation GeneralizedIntersectionFn EnglishLanguage \"A &%UnaryFunction that takes a &%SetOrClass of &%Classes as its single argument and returns a &%SetOrClass which is the intersection of all of the &%Classes in the original &%SetOrClass, i.e. the &%SetOrClass containing just those instances which are instances of all instances of the original &%SetOrClass.\")").
:- load_kif("(instance CardinalityFn UnaryFunction)").
:- load_kif("(instance CardinalityFn TotalValuedRelation)").
:- load_kif("(instance CardinalityFn AsymmetricRelation)").
:- load_kif("(domain CardinalityFn 1 (UnionFn SetOrClass Collection))").
:- load_kif("(range CardinalityFn Number)").
:- load_kif("(documentation CardinalityFn EnglishLanguage \"(CardinalityFn ?CLASS) returns the number of instances in the &%SetOrClass ?CLASS or the number of members in the ?CLASS &%Collection.\")").
:- load_kif("(subclass NullSet SetOrClass)").
:- load_kif("(documentation NullSet EnglishLanguage \"Any &%SetOrClass that contains no instances.\")").
:- load_kif("(=> (instance ?SET NullSet) (not (exists (?INST) (instance ?INST ?SET))))").
:- load_kif("(subclass NonNullSet SetOrClass)").
:- load_kif("(documentation NonNullSet EnglishLanguage \"Any &%SetOrClass that contains at least one instance.\")").
:- load_kif("(=> (instance ?SET NonNullSet) (exists (?INST) (instance ?INST ?SET)))").
:- load_kif("(subclass FiniteSet Set)").
:- load_kif("(documentation FiniteSet EnglishLanguage \"A &%Set containing a finite number of elements.\")").
:- load_kif("(=> (instance ?SET FiniteSet) (exists (?NUMBER) (and (instance ?NUMBER NonnegativeInteger) (equal ?NUMBER (CardinalityFn ?SET)))))").
:- load_kif("(subclass PairwiseDisjointClass SetOrClass)").
:- load_kif("(documentation PairwiseDisjointClass EnglishLanguage \"A &%SetOrClass is a &%PairwiseDisjointClass just in case every instance of the &%SetOrClass is either &%equal to or &%disjoint from every other instance of the &%SetOrClass.\")").
:- load_kif("(=> (instance ?SUPERCLASS PairwiseDisjointClass) (forall (?CLASS1 ?CLASS2) (=> (and (instance ?CLASS1 ?SUPERCLASS) (instance ?CLASS2 ?SUPERCLASS)) (or (equal ?CLASS1 ?CLASS2) (disjoint ?CLASS1 ?CLASS2)))))").
:- load_kif("(subclass MutuallyDisjointClass SetOrClass)").
:- load_kif("(documentation MutuallyDisjointClass EnglishLanguage \"A &%SetOrClass is a &%MutuallyDisjointClass just in case there exists nothing which is an instance of all of the instances of the original &%SetOrClass.\")").
:- load_kif("(=> (instance ?CLASS MutuallyDisjointClass) (forall (?INST1 ?INST2) (=> (and (instance ?INST1 ?CLASS) (instance ?INST2 ?INST1)) (exists (?INST3) (and (instance ?INST3 ?CLASS) (not (instance ?INST2 ?INST3)))))))").
:- load_kif("(instance KappaFn BinaryFunction)").
:- load_kif("(domain KappaFn 1 SymbolicString)").
:- load_kif("(domain KappaFn 2 Formula)").
:- load_kif("(range KappaFn Class)").
:- load_kif("(documentation KappaFn EnglishLanguage \"A class-forming operator that takes two arguments: a variable and a formula containing at least one unbound occurrence of the variable. The result of applying &%KappaFn to a variable and a formula is the &%SetOrClass of things that satisfy the formula. For example, we can denote the &%SetOrClass of prime numbers that are less than 100 with the following expression: (KappaFn ?NUMBER").
:- load_kif("(and (instance ?NUMBER PrimeNumber) (lessThan ?NUMBER 100))). Note that the use of this function is discouraged, since there is currently no axiomatic support for it.\")").
% :- load_kif("; At some point we may be able to make use of 'KappaFn' by implementing a macro that decomposes every occurrence of 'KappaFn' into a complex formula. For example the macro might replace every instance of Schema 1 with an instance of Schema 2. Schema 1: (KappaFn <variable> <formula>) Schema 2: (exists (?LIST) (and (instance ?LIST UniqueList) (forall (<variable>) (<=> (inList <variable> ?LIST) <formula>)))) ").
% :- load_kif("; END FILE").
% :- load_kif("; BEGIN FILE").
% :- load_kif(";  GRAPH THEORY  ").
% :- load_kif("; INCLUDES 'NUMERIC FUNCTIONS'").
:- load_kif("(subclass Graph Abstract)").
:- load_kif("(documentation Graph EnglishLanguage \"The &%Class of graphs, where a graph is understood to be a set of &%GraphNodes connected by &%GraphArcs. Note that this &%Class includes only connected graphs, i.e. graphs in which there is a &%GraphPath between any two &%GraphNodes. Note too that every &%Graph is required to contain at least two &%GraphArcs and three &%GraphNodes.\")").
:- load_kif("(=> (and (instance ?GRAPH Graph) (instance ?NODE1 GraphNode) (instance ?NODE2 GraphNode) (graphPart ?NODE1 ?GRAPH) (graphPart ?NODE2 ?GRAPH) (not (equal ?NODE1 ?NODE2))) (exists (?ARC ?PATH) (or (links ?NODE1 ?NODE2 ?ARC) (and (subGraph ?PATH ?GRAPH) (instance ?PATH GraphPath) (or (and (equal (BeginNodeFn ?PATH) ?NODE1) (equal (EndNodeFn ?PATH) ?NODE2)) (and (equal (BeginNodeFn ?PATH) ?NODE2) (equal (EndNodeFn ?PATH) ?NODE1)))))))").
:- load_kif("(=> (instance ?GRAPH Graph) (exists (?NODE1 ?NODE2 ?NODE3 ?ARC1 ?ARC2) (and (graphPart ?NODE1 ?GRAPH) (graphPart ?NODE2 ?GRAPH) (graphPart ?NODE3 ?GRAPH) (graphPart ?ARC1 ?GRAPH) (graphPart ?ARC2 ?GRAPH) (links ?NODE1 ?NODE2 ?ARC1) (links ?NODE2 ?NODE3 ?ARC2) (not (equal ?NODE1 ?NODE2)) (not (equal ?NODE2 ?NODE3)) (not (equal ?NODE1 ?NODE3)) (not (equal ?ARC1 ?ARC2)))))").
:- load_kif("(subclass DirectedGraph Graph)").
:- load_kif("(documentation DirectedGraph EnglishLanguage \"The &%Class of directed graphs. A directed graph is a &%Graph in which all &%GraphArcs have direction, i.e. every &%GraphArc has an initial node (see &%InitialNodeFn) and a terminal node (see &%TerminalNodeFn).\")").
:- load_kif("(=> (and (instance ?GRAPH DirectedGraph) (instance ?ARC GraphArc) (graphPart ?ARC ?GRAPH)) (exists (?NODE1 ?NODE2) (and (equal (InitialNodeFn ?ARC) ?NODE1) (equal (TerminalNodeFn ?ARC) ?NODE2))))").
:- load_kif("(subclass Tree DirectedGraph)").
:- load_kif("(documentation Tree EnglishLanguage \"A Tree is a &%DirectedGraph that has no &%GraphLoops.\")").
:- load_kif("(=> (instance ?GRAPH Tree) (not (exists (?LOOP) (and (instance ?LOOP GraphLoop) (graphPart ?LOOP ?GRAPH)))))").
:- load_kif("(=> (instance ?GRAPH Tree) (not (exists (?LOOP) (and (instance ?LOOP GraphCircuit) (graphPart ?LOOP ?GRAPH)))))").
:- load_kif("(=> (instance ?GRAPH Tree) (not (exists (?ARC1 ?ARC2 ?NODE) (and (graphPart ?ARC1 ?GRAPH) (graphPart ?ARC2 ?GRAPH) (graphPart ?NODE ?GRAPH) (equal (TerminalNodeFn ?ARC1) ?NODE) (equal (TerminalNodeFn ?ARC2) ?NODE) (not (equal ?ARC1 ?ARC2))))))").
:- load_kif("(subclass GraphPath DirectedGraph)").
:- load_kif("(documentation GraphPath EnglishLanguage \"Informally, a single, directed route between two &%GraphNodes in a &%Graph. Formally, a &%DirectedGraph that is a &%subGraph of the original &%Graph and such that no two &%GraphArcs in the &%DirectedGraph have the same intial node (see &%InitialNodeFn) or the same terminal node (see &%TerminalNodeFn).\")").
:- load_kif("(=> (and (instance ?GRAPH GraphPath) (instance ?ARC GraphArc) (graphPart ?ARC ?GRAPH)) (=> (equal (InitialNodeFn ?ARC) ?NODE) (not (exists (?OTHER) (and (equal (InitialNodeFn ?OTHER) ?NODE) (not (equal ?OTHER ?ARC)))))))").
:- load_kif("(=> (and (instance ?GRAPH GraphPath) (instance ?ARC GraphArc) (graphPart ?ARC ?GRAPH)) (=> (equal (TerminalNodeFn ?ARC) ?NODE) (not (exists (?OTHER) (and (equal (TerminalNodeFn ?OTHER) ?NODE) (not (equal ?OTHER ?ARC)))))))").
:- load_kif("(subclass GraphCircuit GraphPath)").
:- load_kif("(documentation GraphCircuit EnglishLanguage \"A &%GraphPath that begins (see &%BeginNodeFn) and ends (see &%EndNodeFn) at the same &%GraphNode.\")").
:- load_kif("(<=> (instance ?GRAPH GraphCircuit) (exists (?NODE) (and (equal (BeginNodeFn ?GRAPH) ?NODE) (equal (EndNodeFn ?GRAPH) ?NODE))))").
:- load_kif("(subclass MultiGraph Graph)").
:- load_kif("(documentation MultiGraph EnglishLanguage \"The &%Class of multigraphs. A multigraph is a &%Graph containing at least one pair of &%GraphNodes that are connected by more than one &%GraphArc.\")").
:- load_kif("(<=> (instance ?GRAPH MultiGraph) (exists (?ARC1 ?ARC2 ?NODE1 ?NODE2) (and (graphPart ?ARC1 ?GRAPH) (graphPart ?ARC2 ?GRAPH) (graphPart ?NODE1 ?GRAPH) (graphPart ?NODE2 ?GRAPH) (links ?NODE1 ?NODE2 ?ARC1) (links ?NODE1 ?NODE2 ?ARC2) (not (equal ?ARC1 ?ARC2)))))").
:- load_kif("(subclass PseudoGraph Graph)").
:- load_kif("(documentation PseudoGraph EnglishLanguage \"The &%Class of pseudographs. A pseudograph is a &%Graph containing at least one &%GraphLoop.\")").
:- load_kif("(<=> (instance ?GRAPH PseudoGraph) (exists (?LOOP) (and (instance ?LOOP GraphLoop) (graphPart ?LOOP ?GRAPH))))").
:- load_kif("(subclass GraphElement Abstract)").
:- load_kif("(partition GraphElement GraphNode GraphArc)").
:- load_kif("(documentation GraphElement EnglishLanguage \"Noncompositional parts of &%Graphs. These parts are restricted to &%GraphNodes and &%GraphArcs.\")").
:- load_kif("(=> (instance ?PART GraphElement) (exists (?GRAPH) (and (instance ?GRAPH Graph) (graphPart ?PART ?GRAPH))))").
:- load_kif("(subclass GraphNode GraphElement)").
:- load_kif("(documentation GraphNode EnglishLanguage \"&%Graphs are comprised of &%GraphNodes and &%GraphArcs. Every &%GraphNode is linked by a &%GraphArc.\")").
:- load_kif("(=> (instance ?NODE GraphNode) (exists (?OTHER ?ARC) (links ?NODE ?OTHER ?ARC)))").
:- load_kif("(subclass GraphArc GraphElement)").
:- load_kif("(documentation GraphArc EnglishLanguage \"&%Graphs are comprised of &%GraphNodes and &%GraphArcs. Every &%GraphArc links two &%GraphNodes.\")").
:- load_kif("(=> (instance ?ARC GraphArc) (exists (?NODE1 ?NODE2) (links ?NODE1 ?NODE2 ?ARC)))").
:- load_kif("(subclass GraphLoop GraphArc)").
:- load_kif("(documentation GraphLoop EnglishLanguage \"A &%GraphArc in which a &%GraphNode is linked to itself.\")").
:- load_kif("(<=> (instance ?LOOP GraphLoop) (exists (?NODE) (links ?NODE ?NODE ?LOOP)))").
:- load_kif("(=> (and (equal (InitialNodeFn ?ARC) ?NODE) (equal (TerminalNodeFn ?ARC) ?NODE)) (instance ?ARC GraphLoop))").
:- load_kif("(instance links TernaryPredicate)").
:- load_kif("(domain links 1 GraphNode)").
:- load_kif("(domain links 2 GraphNode)").
:- load_kif("(domain links 3 GraphArc)").
:- load_kif("(documentation links EnglishLanguage \"a &%TernaryPredicate that specifies the &%GraphArc connecting two &%GraphNodes.\")").
:- load_kif("(=> (links ?NODE1 ?NODE2 ?ARC) (links ?NODE2 ?NODE1 ?ARC))").
:- load_kif("(instance graphPart BinaryPredicate)").
:- load_kif("(instance graphPart AsymmetricRelation)").
:- load_kif("(instance graphPart IrreflexiveRelation)").
:- load_kif("(domain graphPart 1 GraphElement)").
:- load_kif("(domain graphPart 2 Graph)").
:- load_kif("(documentation graphPart EnglishLanguage \"A basic relation for &%Graphs and their parts. (&%graphPart ?PART ?GRAPH) means that ?PART is a &%GraphArc or &%GraphNode of the &%Graph ?GRAPH.\")").
:- load_kif("(instance subGraph BinaryPredicate)").
:- load_kif("(instance subGraph ReflexiveRelation)").
:- load_kif("(instance subGraph TransitiveRelation)").
:- load_kif("(domain subGraph 1 Graph)").
:- load_kif("(domain subGraph 2 Graph)").
:- load_kif("(documentation subGraph EnglishLanguage \"The relation between two &%Graphs when one &%Graph is a part of the other. (&%subGraph ?GRAPH1 ?GRAPH2) means that ?GRAPH1 is a part of ?GRAPH2.\")").
:- load_kif("(=> (and (subGraph ?GRAPH1 ?GRAPH2) (graphPart ?ELEMENT ?GRAPH1)) (graphPart ?ELEMENT ?GRAPH2))").
:- load_kif("(instance pathLength BinaryPredicate)").
:- load_kif("(instance pathLength AsymmetricRelation)").
:- load_kif("(instance pathLength IrreflexiveRelation)").
:- load_kif("(domain pathLength 1 GraphPath)").
:- load_kif("(domain pathLength 2 PositiveInteger)").
:- load_kif("(documentation pathLength EnglishLanguage \"A &%BinaryPredicate that specifies the length (in number of &%GraphNodes) of a &%GraphPath.  (&%pathLength ?PATH ?NUMBER) means that there are ?NUMBER nodes in the &%GraphPath ?PATH.\")").
:- load_kif("(instance InitialNodeFn UnaryFunction)").
:- load_kif("(instance InitialNodeFn PartialValuedRelation)").
:- load_kif("(domain InitialNodeFn 1 GraphArc)").
:- load_kif("(range InitialNodeFn GraphNode)").
:- load_kif("(documentation InitialNodeFn EnglishLanguage \"A &%UnaryFunction that maps a &%GraphArc to the initial node of the &%GraphArc. Note that this is a partial function. In particular, the function is undefined for &%GraphArcs that are not part of a &%DirectedGraph.\")").
:- load_kif("(instance TerminalNodeFn UnaryFunction)").
:- load_kif("(instance TerminalNodeFn PartialValuedRelation)").
:- load_kif("(domain TerminalNodeFn 1 GraphArc)").
:- load_kif("(range TerminalNodeFn GraphNode)").
:- load_kif("(documentation TerminalNodeFn EnglishLanguage \"A &%UnaryFunction that maps a &%GraphArc to the terminal node of the &%GraphArc. Note that this is a partial function. In particular, the function is undefined for &%GraphArcs that are not part of a &%DirectedGraph.\")").
:- load_kif("(instance BeginNodeFn UnaryFunction)").
:- load_kif("(instance BeginNodeFn TotalValuedRelation)").
:- load_kif("(domain BeginNodeFn 1 GraphPath)").
:- load_kif("(range BeginNodeFn GraphNode)").
:- load_kif("(relatedInternalConcept BeginNodeFn InitialNodeFn)").
:- load_kif("(documentation BeginNodeFn EnglishLanguage \"A &%UnaryFunction that maps a &%GraphPath to the &%GraphNode that is the beginning of the &%GraphPath. Note that, unlike &%InitialNodeFn (which relates a &%GraphArc to a &%GraphNode), &%BeginNodeFn is a total function - every &%GraphPath has a beginning.\")").
:- load_kif("(instance EndNodeFn UnaryFunction)").
:- load_kif("(instance EndNodeFn TotalValuedRelation)").
:- load_kif("(domain EndNodeFn 1 GraphPath)").
:- load_kif("(range EndNodeFn GraphNode)").
:- load_kif("(relatedInternalConcept EndNodeFn TerminalNodeFn)").
:- load_kif("(documentation EndNodeFn EnglishLanguage \"A &%UnaryFunction that maps a &%GraphPath to the &%GraphNode that is the end of the &%GraphPath. Note that, unlike &%TerminalNodeFn (which relates a &%GraphArc to a &%GraphNode), &%EndNodeFn is a total function - every &%GraphPath has a end.\")").
:- load_kif("(instance arcWeight BinaryPredicate)").
:- load_kif("(instance arcWeight SingleValuedRelation)").
:- load_kif("(domain arcWeight 1 GraphArc)").
:- load_kif("(domain arcWeight 2 Quantity)").
:- load_kif("(documentation arcWeight EnglishLanguage \"This predicate indicates the value of a &%GraphArc in a &%Graph. This could map to the length of a road in a road network or the flow rate of a pipe in a plumbing system.\")").
:- load_kif("(instance PathWeightFn UnaryFunction)").
:- load_kif("(domain PathWeightFn 1 GraphPath)").
:- load_kif("(range PathWeightFn Quantity)").
:- load_kif("(documentation PathWeightFn EnglishLanguage \"A &%UnaryFunction that maps a &%GraphPath to the sum of the &%arcWeights on the &%GraphArcs in the &%GraphPath.\")").
:- load_kif("(=> (and (equal (PathWeightFn ?PATH) ?SUM) (subGraph ?SUBPATH ?PATH) (graphPart ?ARC1 ?PATH) (arcWeight ?ARC1 ?NUMBER1) (forall (?ARC2) (=> (graphPart ?ARC2 ?PATH) (or (graphPart ?ARC2 ?SUBPATH) (equal ?ARC2 ?ARC1))))) (equal ?SUM (AdditionFn (PathWeightFn ?SUBPATH) ?NUMBER1)))").
:- load_kif("(=> (and (equal (PathWeightFn ?PATH) ?SUM) (graphPart ?ARC1 ?PATH) (graphPart ?ARC2 ?PATH) (arcWeight ?ARC1 ?NUMBER1) (arcWeight ?ARC2 ?NUMBER2) (forall (?ARC3) (=> (graphPart ?ARC3 ?PATH) (or (equal ?ARC3 ?ARC1) (equal ?ARC3 ?ARC2))))) (equal (PathWeightFn ?PATH) (AdditionFn ?NUMBER1 ?NUMBER2)))").
:- load_kif("(instance MinimalWeightedPathFn BinaryFunction)").
:- load_kif("(domain MinimalWeightedPathFn 1 GraphNode)").
:- load_kif("(domain MinimalWeightedPathFn 2 GraphNode)").
:- load_kif("(range MinimalWeightedPathFn GraphPath)").
:- load_kif("(documentation MinimalWeightedPathFn EnglishLanguage \"This &%BinaryFunction assigns two &%GraphNodes to the &%GraphPath with the smallest sum of weighted arcs between the two &%GraphNodes.\")").
:- load_kif("(=> (equal (MinimalWeightedPathFn ?NODE1 ?NODE2) ?PATH) (instance ?PATH (GraphPathFn ?NODE1 ?NODE2)))").
:- load_kif("(=> (and (equal (MinimalWeightedPathFn ?NODE1 ?NODE2) ?PATH) (equal (PathWeightFn ?PATH) ?NUMBER)) (forall (?PATH2) (=> (and (instance ?PATH2 (GraphPathFn ?NODE1 ?NODE2)) (equal (PathWeightFn ?PATH2) ?NUMBER2)) (greaterThanOrEqualTo ?NUMBER2 ?NUMBER1))))").
:- load_kif("(instance MaximalWeightedPathFn BinaryFunction)").
:- load_kif("(domain MaximalWeightedPathFn 1 GraphNode)").
:- load_kif("(domain MaximalWeightedPathFn 2 GraphNode)").
:- load_kif("(range MaximalWeightedPathFn GraphPath)").
:- load_kif("(documentation MaximalWeightedPathFn EnglishLanguage \"This &%BinaryFunction assigns two &%GraphNodes to the &%GraphPath with the largest sum of weighted arcs between the two &%GraphNodes.\")").
:- load_kif("(=> (equal (MaximalWeightedPathFn ?NODE1 ?NODE2) ?PATH) (instance ?PATH (GraphPathFn ?NODE1 ?NODE2)))").
:- load_kif("(=> (and (equal (MaximalWeightedPathFn ?NODE1 ?NODE2) ?PATH) (equal (PathWeightFn ?PATH) ?NUMBER)) (forall (?PATH2) (=> (and (instance ?PATH2 (GraphPathFn ?NODE1 ?NODE2)) (equal (PathWeightFn ?PATH2) ?NUMBER2)) (lessThanOrEqualTo ?NUMBER2 ?NUMBER1))))").
:- load_kif("(instance GraphPathFn BinaryFunction)").
:- load_kif("(instance GraphPathFn TotalValuedRelation)").
:- load_kif("(domain GraphPathFn 1 GraphNode)").
:- load_kif("(domain GraphPathFn 2 GraphNode)").
:- load_kif("(rangeSubclass GraphPathFn GraphPath)").
:- load_kif("(documentation GraphPathFn EnglishLanguage \"A &%BinaryFunction that maps two &%GraphNodes to the &%Class of &%GraphPaths between those two nodes. Note that the two &%GraphNodes must belong to the same &%Graph.\")").
:- load_kif("(=> (and (graphPart ?PATH ?GRAPH) (not (instance ?GRAPH DirectedGraph))) (<=> (equal (GraphPathFn ?NODE1 ?NODE2) ?PATH) (equal (GraphPathFn ?NODE2 ?NODE1) ?PATH)))").
:- load_kif("(instance CutSetFn UnaryFunction)").
:- load_kif("(domain CutSetFn 1 Graph)").
:- load_kif("(rangeSubclass CutSetFn GraphPath)").
:- load_kif("(documentation CutSetFn EnglishLanguage \"A &%UnaryFunction that assigns a &%Graph the &%Class of &%GraphPaths that partition the graph into two separate graphs if cut. There may be more than one cutset for a given graph.\")").
:- load_kif("(instance MinimalCutSetFn UnaryFunction)").
:- load_kif("(domain MinimalCutSetFn 1 Graph)").
:- load_kif("(rangeSubclass MinimalCutSetFn GraphPath)").
:- load_kif("(relatedInternalConcept MinimalCutSetFn CutSetFn)").
:- load_kif("(documentation MinimalCutSetFn EnglishLanguage \"A &%UnaryFunction that assigns a &%Graph the &%Class of &%GraphPaths which comprise cutsets for the &%Graph and which have the least number of &%GraphArcs.\")").
:- load_kif("(=> (instance ?GRAPH Graph) (subclass (MinimalCutSetFn ?GRAPH) (CutSetFn ?GRAPH)))").
:- load_kif("(=> (equal (MinimalCutSetFn ?GRAPH) ?PATHCLASS) (exists (?NUMBER) (forall (?PATH) (=> (instance ?PATH ?PATHCLASS) (pathLength ?PATH ?NUMBER)))))").
:- load_kif("(not (exists (?PATH1 ?PATH2) (and (instance ?PATH1 (CutSetFn ?GRAPH)) (instance ?PATH2 (MinimalCutSetFn ?GRAPH)) (pathLength ?PATH1 ?NUMBER1) (pathLength ?PATH2 ?NUMBER2) (lessThan ?NUMBER1 ?NUMBER2))))").
% :- load_kif("; link the physical system to the abstract graph").
:- load_kif("(instance abstractCounterpart BinaryPredicate)").
:- load_kif("(domain abstractCounterpart 1 Abstract)").
:- load_kif("(domain abstractCounterpart 2 Physical)").
:- load_kif("(subrelation abstractCounterpart represents)").
:- load_kif("(documentation abstractCounterpart EnglishLanguage \"(abstractCounterpart ?AB ?PHYS relates a &%Physical entity to an &%Abstract one which is an idealized model in some dimension of the &%Physical entity. For example, an &%Abstract &%GraphNode could be stated to be the counterpart of an actual &%Computer in a &%ComputerNetwork.\")").
:- load_kif("(subclass PhysicalSystem Physical)").
:- load_kif("(documentation PhysicalSystem EnglishLanguage \"&%PhysicalSystem is the class of complex &%Physical things. A &%PhysicalSystem may have one or more corresponding abstract &%Graph representations.\")").
:- load_kif("(instance subSystem BinaryPredicate)").
:- load_kif("(domain subSystem 1 PhysicalSystem)").
:- load_kif("(domain subSystem 2 PhysicalSystem)").
:- load_kif("(documentation subSystem EnglishLanguage \"(&%subSystem ?SUB ?SYSTEM) means that the &%PhysicalSystem ?SUB is a part of the &%PhysicalSystem ?SYSTEM.\")").
:- load_kif("(instance systemPart BinaryPredicate)").
:- load_kif("(domain systemPart 1 Physical)").
:- load_kif("(domain systemPart 2 PhysicalSystem)").
:- load_kif("(documentation systemPart EnglishLanguage \"(&%systemPart ?PART ?SYSTEM) means that the &%Physical thing ?PART is a &%SystemElement in the &%PhysicalSystem ?SYSTEM.\")").
:- load_kif("(=> (and (subSystem ?SUB ?SYSTEM) (systemPart ?PART ?SUB)) (systemPart ?PART ?SYSTEM))").
:- load_kif("(instance graphMeasure BinaryPredicate)").
:- load_kif("(domain graphMeasure 1 Graph)").
:- load_kif("(domain graphMeasure 2 UnitOfMeasure)").
:- load_kif("(documentation graphMeasure EnglishLanguage \"(graphMeasure ?GRAPH ?MEAS) fixes a &%UnitOfMeasure that is used for the &%arcWeight of a given &%Graph. Stating such a relationship entails that the components of given graph are the &%abstractCounterparts of sets of &%Physical &%Entity(ies).\")").
:- load_kif("(=> (graphMeasure ?G ?M) (forall (?AC) (and (graphPart ?AC ?G) (exists (?PC) (abstractCounterpart ?AC ?PC)))))").
:- load_kif("(=> (and (graphMeasure ?G ?M) (instance ?AN GraphNode) (instance ?AA GraphArc) (abstractCounterpart ?AN ?PN) (abstractCounterpart ?AA ?PA) (arcWeight ?AA (MeasureFn ?N ?M))) (measure ?PA (MeasureFn ?N ?M)))").
% :- load_kif("; END FILE").
% :- load_kif("; BEGIN FILE").
% :- load_kif(";  UNITS OF MEASURE  ").
% :- load_kif("; INCLUDES 'NUMERIC FUNCTIONS'").
% :- load_kif("; This section was originally based on the relations in the Quantities ontology (developed by ITBM-CNR) and the units of measure in the \"Standard Units\" and \"Standard Dimensions\" ontologies on the Ontolingua server. This content has been extensively revised by Helena Sofia Pinto of the Instituto Superior Tecnico in Portugal. The sources for these revisions were: - Barry Taylor, NIST Special Publication 811, Guide for the Use of the International System of Units (SI), 1995. - Encyclopaedia Britannica (on-line version at http://www.britannica.com)").
:- load_kif("(subclass UnitOfMeasure PhysicalQuantity)").
:- load_kif("(documentation UnitOfMeasure EnglishLanguage \"A standard of measurement for some dimension. For example, the &%Meter is a &%UnitOfMeasure for the dimension of length, as is the &%Inch. There is no intrinsic property of a &%UnitOfMeasure that makes it primitive or fundamental, rather, a system of units (e.g. &%SystemeInternationalUnit) defines a set of orthogonal dimensions and assigns units for each.\")").
:- load_kif("(subclass CompositeUnitOfMeasure UnitOfMeasure)").
:- load_kif("(documentation CompositeUnitOfMeasure EnglishLanguage \"Instances of this &%Class are &%UnitsOfMeasure defined by the functional composition of other units, each of which might be a &%CompositeUnitOfMeasure or a &%NonCompositeUnitOfMeasure.\")").
:- load_kif("(subclass NonCompositeUnitOfMeasure UnitOfMeasure)").
:- load_kif("(documentation NonCompositeUnitOfMeasure EnglishLanguage \"Instances of this &%Class are &%UnitsOfMeasure that are applied to a single dimension, and so are not intrinsically defined by the functional composition of other units.\")").
:- load_kif("(partition UnitOfMeasure CompositeUnitOfMeasure NonCompositeUnitOfMeasure)").
:- load_kif("(=> (and (instance ?QUANT (MeasureFn ?N ?UNIT)) (instance ?UNIT CompositeUnitOfMeasure)) (instance ?QUANT FunctionQuantity))").
:- load_kif("(=> (and (instance ?QUANT (MeasureFn ?N ?UNIT)) (instance ?UNIT NonCompositeUnitOfMeasure)) (instance ?QUANT ConstantQuantity))").
:- load_kif("(subclass SystemeInternationalUnit UnitOfMeasure)").
:- load_kif("(documentation SystemeInternationalUnit EnglishLanguage \"The &%Class of Systeme International (SI) units.\")").
:- load_kif("(subclass LengthMeasure ConstantQuantity)").
:- load_kif("(documentation LengthMeasure EnglishLanguage \"A &%subclass of &%ConstantQuantity, instances of which are measures of length.\")").
:- load_kif("(subclass UnitOfLength NonCompositeUnitOfMeasure)").
:- load_kif("(documentation UnitOfLength EnglishLanguage \"Every instance of this &%Class is a &%UnitOfMeasure that can be used with &%MeasureFn to form instances of &%LengthMeasure.\")").
:- load_kif("(subclass MassMeasure ConstantQuantity)").
:- load_kif("(documentation MassMeasure EnglishLanguage \"A &%subclass of &%ConstantQuantity, instances of which are measures of the amount of matter in an &%Object.\")").
:- load_kif("(subclass UnitOfMass NonCompositeUnitOfMeasure)").
:- load_kif("(documentation UnitOfMass EnglishLanguage \"Every instance of this &%Class is a &%UnitOfMeasure that can be used with &%MeasureFn to form instances of &%MassMeasure, which denote the amount of matter in &%PhysicalObjects.\")").
:- load_kif("(subclass AreaMeasure FunctionQuantity)").
:- load_kif("(documentation AreaMeasure EnglishLanguage \"Measures of the amount of space in two dimensions.\")").
:- load_kif("(subclass UnitOfArea CompositeUnitOfMeasure)").
:- load_kif("(documentation UnitOfArea EnglishLanguage \"Every instance of this &%Class is a &%UnitOfMeasure that can be used with &%MeasureFn to form instances of &%AreaMeasure.\")").
:- load_kif("(subclass VolumeMeasure FunctionQuantity)").
:- load_kif("(documentation VolumeMeasure EnglishLanguage \"Measures of the amount of space in three dimensions.\")").
:- load_kif("(subclass UnitOfVolume CompositeUnitOfMeasure)").
:- load_kif("(documentation UnitOfVolume EnglishLanguage \"Every instance of this &%Class is a &%UnitOfMeasure that can be used with &%MeasureFn to form instances of &%VolumeMeasure.\")").
:- load_kif("(subclass TemperatureMeasure ConstantQuantity)").
:- load_kif("(documentation TemperatureMeasure EnglishLanguage \"Measures of temperature. In scientific circles, the temperature of something is understood as the average velocity of the atoms or molecules that make up the thing.\")").
:- load_kif("(subclass UnitOfTemperature NonCompositeUnitOfMeasure)").
:- load_kif("(documentation UnitOfTemperature EnglishLanguage \"Every instance of this &%Class is a &%UnitOfMeasure that can be used with &%MeasureFn to form instances of &%TemperatureMeasure.\")").
:- load_kif("(subclass CurrencyMeasure ConstantQuantity)").
:- load_kif("(documentation CurrencyMeasure EnglishLanguage \"Instances of this &%subclass of &%ConstantQuantity are measures of &%monetaryValue stated in terms of some &%UnitOfCurrency such as &%UnitedStatesDollar, &%UnitedStatesCent, Lire, Yen, etc.\")").
:- load_kif("(subclass UnitOfCurrency NonCompositeUnitOfMeasure)").
:- load_kif("(documentation UnitOfCurrency EnglishLanguage \"Every instance of this &%Class is a &%UnitOfMeasure that can be used with &%MeasureFn to form instances of &%CurrencyMeasure.\")").
:- load_kif("(subclass AngleMeasure ConstantQuantity)").
:- load_kif("(documentation AngleMeasure EnglishLanguage \"The value of an angle in a plane or in a solid.\")").
:- load_kif("(subclass UnitOfAngularMeasure NonCompositeUnitOfMeasure)").
:- load_kif("(documentation UnitOfAngularMeasure EnglishLanguage \"Every instance of this &%Class is a &%UnitOfMeasure that can be used with &%MeasureFn to form instances of &%AngleMeasure.\")").
:- load_kif("(subclass PlaneAngleMeasure AngleMeasure)").
:- load_kif("(documentation PlaneAngleMeasure EnglishLanguage \"The value of an angle in a plane.\")").
:- load_kif("(subclass SolidAngleMeasure AngleMeasure)").
:- load_kif("(disjoint SolidAngleMeasure PlaneAngleMeasure)").
:- load_kif("(documentation SolidAngleMeasure EnglishLanguage \"The value of an angle in a solid.\")").
:- load_kif("(subclass UnitOfInformation NonCompositeUnitOfMeasure)").
:- load_kif("(documentation UnitOfInformation EnglishLanguage \"Every instance of this &%Class is a &%UnitOfMeasure that can be used with &%MeasureFn to form instances of &%InformationMeasure.\")").
:- load_kif("(subclass UnitOfDuration NonCompositeUnitOfMeasure)").
:- load_kif("(documentation UnitOfDuration EnglishLanguage \"Every instance of this &%Class is a &%UnitOfMeasure that can be used with &%MeasureFn to form instances of &%TimeDuration. Note that &%TimeDuration is a &%subclass of &%TimeMeasure.\")").
:- load_kif("(instance MeasureFn BinaryFunction)").
:- load_kif("(instance MeasureFn TotalValuedRelation)").
:- load_kif("(domain MeasureFn 1 RealNumber)").
:- load_kif("(domain MeasureFn 2 UnitOfMeasure)").
:- load_kif("(range MeasureFn PhysicalQuantity)").
:- load_kif("(documentation MeasureFn EnglishLanguage \"This &%BinaryFunction maps a &%RealNumber and a &%UnitOfMeasure to that &%Number of units. It is used to express `measured' instances of &%PhysicalQuantity. Example: the concept of three meters is represented as (&%MeasureFn 3 &%Meter).\")").
:- load_kif("(=> (and (equal (MeasureFn ?NUMBER ?UNIT) ?QUANT) (instance ?UNIT UnitOfLength)) (instance ?QUANT LengthMeasure))").
:- load_kif("(=> (and (equal (MeasureFn ?NUMBER ?UNIT) ?QUANT) (instance ?UNIT UnitOfMass)) (instance ?QUANT MassMeasure))").
:- load_kif("(=> (and (equal (MeasureFn ?NUMBER ?UNIT) ?QUANT) (instance ?UNIT UnitOfArea)) (instance ?QUANT AreaMeasure))").
:- load_kif("(=> (and (equal (MeasureFn ?NUMBER ?UNIT) ?QUANT) (instance ?UNIT UnitOfVolume)) (instance ?QUANT VolumeMeasure))").
:- load_kif("(=> (and (equal (MeasureFn ?NUMBER ?UNIT) ?QUANT) (instance ?UNIT UnitOfTemperature)) (instance ?QUANT TemperatureMeasure))").
:- load_kif("(=> (and (equal (MeasureFn ?NUMBER ?UNIT) ?QUANT) (instance ?UNIT UnitOfCurrency)) (instance ?QUANT CurrencyMeasure))").
:- load_kif("(=> (and (equal (MeasureFn ?NUMBER ?UNIT) ?QUANT) (instance ?UNIT UnitOfAngularMeasure)) (instance ?QUANT AngleMeasure))").
:- load_kif("(=> (and (equal (MeasureFn ?NUMBER ?UNIT) ?QUANT) (instance ?UNIT UnitOfDuration)) (instance ?QUANT TimeDuration))").
:- load_kif("(=> (and (equal (MeasureFn ?NUMBER ?UNIT) ?QUANT) (instance ?UNIT UnitOfInformation)) (instance ?QUANT InformationMeasure))").
% :- load_kif("; AP - this axiom below doesn't look right, what about functions that return unitless ratios").
:- load_kif(" (=> (and (instance ?REL RelationExtendedToQuantities) (instance ?REL TernaryRelation) (instance ?NUMBER1 RealNumber) (instance ?NUMBER2 RealNumber) (?REL ?NUMBER1 ?NUMBER2 ?VALUE)) (forall (?UNIT) (=> (instance ?UNIT UnitOfMeasure) (?REL (MeasureFn ?NUMBER1 ?UNIT) (MeasureFn ?NUMBER2 ?UNIT) (MeasureFn ?VALUE ?UNIT)))))").
:- load_kif("(=> (and (instance ?REL RelationExtendedToQuantities) (instance ?REL BinaryRelation) (instance ?NUMBER1 RealNumber) (instance ?NUMBER2 RealNumber) (?REL ?NUMBER1 ?NUMBER2)) (forall (?UNIT) (=> (instance ?UNIT UnitOfMeasure) (?REL (MeasureFn ?NUMBER1 ?UNIT) (MeasureFn ?NUMBER2 ?UNIT)))))").
:- load_kif("(subclass UnitOfMeasureMultiplier UnaryFunction)").
:- load_kif("(subclass UnitOfMeasureMultiplier TotalValuedRelation)").
:- load_kif("(documentation UnitOfMeasureMultiplier EnglishLanguage \"Each &%instance of this &%Class is a &%UnaryFunction that, when evaluated on its single argument, a &%UnitOfMeasure, produces another &%UnitOfMeasure that is a numeric multiple of the argument.\")").
:- load_kif("(=> (and (instance ?FUNCTION UnitOfMeasureMultiplier) (instance ?UNIT CompositeUnitOfMeasure)) (instance (?FUNCTION ?UNIT) CompositeUnitOfMeasure))").
:- load_kif("(=> (and (instance ?FUNCTION UnitOfMeasureMultiplier) (instance ?UNIT NonCompositeUnitOfMeasure)) (instance (?FUNCTION ?UNIT) NonCompositeUnitOfMeasure))").
:- load_kif("(instance KiloFn UnitOfMeasureMultiplier)").
:- load_kif("(domain KiloFn 1 UnitOfMeasure)").
:- load_kif("(range KiloFn UnitOfMeasure)").
:- load_kif("(documentation KiloFn EnglishLanguage \"A &%UnaryFunction that maps a &%UnitOfMeasure into a &%UnitOfMeasure that is equal to 1,000 units of the original &%UnitOfMeasure. For example, (&%KiloFn &%Gram) is 1,000 &%Grams.\")").
:- load_kif("(=> (and (instance ?UNIT UnitOfMeasure) (equal ?KILOUNIT (KiloFn ?UNIT))) (equal (MeasureFn 1 ?KILOUNIT) (MeasureFn 1000 ?UNIT)))").
:- load_kif("(instance MegaFn UnitOfMeasureMultiplier)").
:- load_kif("(domain MegaFn 1 UnitOfMeasure)").
:- load_kif("(range MegaFn UnitOfMeasure)").
:- load_kif("(documentation MegaFn EnglishLanguage \"A &%UnaryFunction that maps a &%UnitOfMeasure into a &%UnitOfMeasure that is equal to 1,000,000 units of the original &%UnitOfMeasure. For example, (&%MegaFn &%Hertz) is 1,000,000 &%Hertz.\")").
:- load_kif("(=> (and (instance ?UNIT UnitOfMeasure) (equal ?MEGAUNIT (MegaFn ?UNIT))) (equal (MeasureFn 1 ?MEGAUNIT) (MeasureFn 1000000 ?UNIT)))").
:- load_kif("(instance GigaFn UnitOfMeasureMultiplier)").
:- load_kif("(domain GigaFn 1 UnitOfMeasure)").
:- load_kif("(range GigaFn UnitOfMeasure)").
:- load_kif("(documentation GigaFn EnglishLanguage \"A &%UnaryFunction that maps a &%UnitOfMeasure into a &%UnitOfMeasure that is equal to 1,000,000,000 units of the original &%UnitOfMeasure. For example, (&%GigaFn &%Hertz) is 1,000,000,000 &%Hertz.\")").
:- load_kif("(=> (and (instance ?UNIT UnitOfMeasure) (equal ?GIGAUNIT (GigaFn ?UNIT))) (equal (MeasureFn 1 ?GIGAUNIT) (MeasureFn 1000000000 ?UNIT)))").
:- load_kif("(instance TeraFn UnitOfMeasureMultiplier)").
:- load_kif("(domain TeraFn 1 UnitOfMeasure)").
:- load_kif("(range TeraFn UnitOfMeasure)").
:- load_kif("(documentation TeraFn EnglishLanguage \"A &%UnaryFunction that maps a &%UnitOfMeasure into a &%UnitOfMeasure that is equal to 1,000,000,000,000 units of the original &%UnitOfMeasure. For example, (&%TeraFn &%Hertz) is 1,000,000,000,000 &%Hertz.\")").
:- load_kif("(=> (and (instance ?UNIT UnitOfMeasure) (equal ?TERAUNIT (TeraFn ?UNIT))) (equal (MeasureFn 1 ?TERAUNIT) (MeasureFn 1000000000000 ?UNIT)))").
:- load_kif("(instance MilliFn UnitOfMeasureMultiplier)").
:- load_kif("(domain MilliFn 1 UnitOfMeasure)").
:- load_kif("(range MilliFn UnitOfMeasure)").
:- load_kif("(documentation MilliFn EnglishLanguage \"A &%UnaryFunction that maps a &%UnitOfMeasure into a &%UnitOfMeasure that is equal to .001 units of the original &%UnitOfMeasure. For example, (&%MilliFn &%Gram) is .001 &%Grams.\")").
:- load_kif("(=> (and (instance ?UNIT UnitOfMeasure) (equal ?MILLIUNIT (MilliFn ?UNIT))) (equal (MeasureFn 1 ?MILLIUNIT) (MeasureFn 0.001 ?UNIT)))").
:- load_kif("(instance MicroFn UnitOfMeasureMultiplier)").
:- load_kif("(domain MicroFn 1 UnitOfMeasure)").
:- load_kif("(range MicroFn UnitOfMeasure)").
:- load_kif("(documentation MicroFn EnglishLanguage \"A &%UnaryFunction that maps a &%UnitOfMeasure into a &%UnitOfMeasure that is equal to .000001 units of the original &%UnitOfMeasure. For example, (&%MicroFn &%Meter) is .000001 &%Meters.\")").
:- load_kif("(=> (and (instance ?UNIT UnitOfMeasure) (equal ?MICROUNIT (MicroFn ?UNIT))) (equal (MeasureFn 1 ?MICROUNIT) (MeasureFn 0.000001 ?UNIT)))").
:- load_kif("(instance NanoFn UnitOfMeasureMultiplier)").
:- load_kif("(domain NanoFn 1 UnitOfMeasure)").
:- load_kif("(range NanoFn UnitOfMeasure)").
:- load_kif("(documentation NanoFn EnglishLanguage \"A &%UnaryFunction that maps a &%UnitOfMeasure into a &%UnitOfMeasure that is equal to .000000001 units of the original &%UnitOfMeasure. For example, (&%MicroFn &%SecondDuration) is .000000001 &%SecondDurations.\")").
:- load_kif("(=> (and (instance ?UNIT UnitOfMeasure) (equal ?NANOUNIT (NanoFn ?UNIT))) (equal (MeasureFn 1 ?NANOUNIT) (MeasureFn 0.000000001 ?UNIT)))").
:- load_kif("(instance PicoFn UnitOfMeasureMultiplier)").
:- load_kif("(domain PicoFn 1 UnitOfMeasure)").
:- load_kif("(range PicoFn UnitOfMeasure)").
:- load_kif("(documentation PicoFn EnglishLanguage \"A &%UnaryFunction that maps a &%UnitOfMeasure into a &%UnitOfMeasure that is equal to .000000000001 units of the original &%UnitOfMeasure. For example, (&%PicoFn &%SecondDuration) is .000000000001 &%SecondDurations.\")").
:- load_kif("(=> (and (instance ?UNIT UnitOfMeasure) (equal ?PICOUNIT (PicoFn ?UNIT))) (equal (MeasureFn 1 ?PICOUNIT) (MeasureFn 0.000000000001 ?UNIT)))").
:- load_kif("(instance IntervalFn BinaryFunction)").
:- load_kif("(domain IntervalFn 1 ConstantQuantity)").
:- load_kif("(domain IntervalFn 2 ConstantQuantity)").
:- load_kif("(rangeSubclass IntervalFn ConstantQuantity)").
:- load_kif("(relatedInternalConcept IntervalFn RecurrentTimeIntervalFn)").
:- load_kif("(documentation IntervalFn EnglishLanguage \"A &%BinaryFunction that maps two instances of &%ConstantQuantity to the &%subclass of &%ConstantQuantity that comprises the interval from the first &%ConstantQuantity to the second &%ConstantQuantity. For example, (&%IntervalFn (&%MeasureFn 8 &%Meter) (&%MeasureFn 14 &%Meter)) would return the &%subclass of &%ConstantQuantity comprising quantities between 8 and 14 meters in length.\")").
:- load_kif("(=> (and (instance ?QUANTITY (IntervalFn ?FROM ?TO)) (instance ?FROM ?CLASS) (instance ?TO ?CLASS)) (instance ?QUANTITY ?CLASS))").
:- load_kif("(<=> (instance ?QUANTITY (IntervalFn ?FROM ?TO)) (and (greaterThanOrEqualTo ?QUANTITY ?FROM) (lessThanOrEqualTo ?QUANTITY ?TO)))").
:- load_kif("(instance MagnitudeFn UnaryFunction)").
:- load_kif("(domain MagnitudeFn 1 PhysicalQuantity)").
:- load_kif("(range MagnitudeFn RealNumber)").
:- load_kif("(documentation MagnitudeFn EnglishLanguage \"The magnitude of a &%PhysicalQuantity is the numeric value for the quantity. In other words, &%MagnitudeFn converts a &%PhysicalQuantity with an associated &%UnitOfMeasure into an ordinary &%RealNumber. For example, the magnitude of the &%ConstantQuantity 2 &%Kilometers is the &%RealNumber 2. Note that the magnitude of a quantity in a given unit times that unit is equal to the original quantity.\")").
:- load_kif("(=> (and (equal ?NUMBER (MultiplicationFn 1 ?NUMBER)) (instance ?UNIT UnitOfMeasure) (equal ?QUANT (MeasureFn ?NUMBER ?UNIT))) (equal (MagnitudeFn ?QUANT) ?NUMBER))").
:- load_kif("(instance UnitFn UnaryFunction)").
:- load_kif("(domain UnitFn 1 PhysicalQuantity)").
:- load_kif("(range UnitFn UnitOfMeasure)").
:- load_kif("(documentation UnitFn EnglishLanguage \"&%UnitFn returns just the &%UnitOfMeasure of a &%PhysicalQuantity with an associated &%UnitOfMeasure and &%RealNumber magnitude. For example, the unit of the &%ConstantQuantity (&%MeasureFn 2 &%Kilometer) is the &%UnitOfMeasure &%Kilometer.\")").
:- load_kif("(=> (and (equal ?NUMBER (MultiplicationFn 1 ?NUMBER)) (instance ?UNIT UnitOfMeasure) (equal ?QUANT (MeasureFn ?NUMBER ?UNIT))) (equal (UnitFn ?QUANT) ?UNIT))").
:- load_kif("(instance PerFn BinaryFunction)").
:- load_kif("(instance PerFn TotalValuedRelation)").
:- load_kif("(domain PerFn 1 PhysicalQuantity)").
:- load_kif("(domain PerFn 2 PhysicalQuantity)").
:- load_kif("(range PerFn FunctionQuantity)").
:- load_kif("(documentation PerFn EnglishLanguage \"&%PerFn maps two instances of &%PhysicalQuantity to the &%FunctionQuantity composed of these two instances. For example,"). (&%PerFn (&%MeasureFn 2 (&%MicroFn &%Gram)) (&%MeasureFn 1 (&%KiloFn &%Gram))) denotes the &%FunctionQuantity of 2 micrograms per kiogram. This function is useful, because it allows the knowledge engineer to dynamically generate instances of &%FunctionQuantity.\")").
:- load_kif("(subrelation DensityFn PerFn)").
:- load_kif("(instance DensityFn TotalValuedRelation)").
:- load_kif("(domain DensityFn 1 MassMeasure)").
:- load_kif("(domain DensityFn 2 VolumeMeasure)").
:- load_kif("(range DensityFn FunctionQuantity)").
:- load_kif("(documentation DensityFn EnglishLanguage \"&%DensityFn maps an instance of &%MassMeasure and an instance of &%VolumeMeasure to the density represented by this proportion of mass and volume. For example, (&%DensityFn (&%MeasureFn 3 &%Gram)"). (&%MeasureFn 1 &%Liter)) represents the density of 3 grams per liter.\")").
:- load_kif("(subrelation SpeedFn PerFn)").
:- load_kif("(instance SpeedFn TotalValuedRelation)").
:- load_kif("(domain SpeedFn 1 LengthMeasure)").
:- load_kif("(domain SpeedFn 2 TimeDuration)").
:- load_kif("(range SpeedFn FunctionQuantity)").
:- load_kif("(documentation SpeedFn EnglishLanguage \"Maps an instance of &%LengthMeasure and an instance of &%TimeDuration to the speed represented by this proportion of distance and time. For example, (&%SpeedFn (&%MeasureFn 55 &%Mile)(&%MeasureFn 1 &%HourDuration)) represents the velocity of 55 miles per hour.\")").
:- load_kif("(instance VelocityFn QuaternaryFunction)").
:- load_kif("(instance VelocityFn TotalValuedRelation)").
:- load_kif("(domain VelocityFn 1 LengthMeasure)").
:- load_kif("(domain VelocityFn 2 TimeDuration)").
:- load_kif("(domain VelocityFn 3 Region)").
:- load_kif("(domain VelocityFn 4 DirectionalAttribute)").
:- load_kif("(range VelocityFn FunctionQuantity)").
:- load_kif("(documentation VelocityFn EnglishLanguage \"Specifies the velocity of an object, i.e. the speed and the direction of the speed. For example (&%VelocityFn (&%MeasureFn 55 &%Mile)"). (&%MeasureFn 2 &%HourDuration) ?REFERENCE &%North) denotes the velocity of 55 miles per hour North of the given reference point ?REFERENCE.\")").
:- load_kif("(=> (measure ?OBJECT (VelocityFn ?DISTANCE ?TIME ?REF ?DIRECTION)) (measure ?OBJECT (SpeedFn ?DISTANCE ?TIME)))").
% :- load_kif("; Now the units of measure:").
% :- load_kif("; First base units for the SI system. No conversion functions are provided for these units.").
% :- load_kif("; Length Base Unit").
:- load_kif("(instance Meter UnitOfLength)").
:- load_kif("(instance Meter SystemeInternationalUnit)").
:- load_kif("(documentation Meter EnglishLanguage \"SI &%UnitOfLength. Symbol: m. It is one of the base units in SI, and it is currently defined as follows: the &%Meter is the length of the path traveled by light in a vacuum during a time interval of 1/299792458 of a &%SecondDuration.\")").
% :- load_kif("; Mass Base Unit").
:- load_kif("(instance Gram UnitOfMass)").
:- load_kif("(instance Gram SystemeInternationalUnit)").
:- load_kif("(documentation Gram EnglishLanguage \"Submultiple of kilogram. Symbol: g. 1 kilogram = 1000 &%Grams.\")").
% :- load_kif("; Time Base Unit").
:- load_kif("(instance SecondDuration UnitOfDuration)").
:- load_kif("(instance SecondDuration SystemeInternationalUnit)").
:- load_kif("(documentation SecondDuration EnglishLanguage \"SI &%UnitOfDuration. Symbol: s. It is one of the base units in SI, and it is currently defined as follows: the &%SecondDuration is the duration of 9192631770 periods of the radiation corresponding to the transition between the two hyperfine levels of the ground state of the cesium 133 atom.\")").
% :- load_kif("; Electric Current Base Unit").
:- load_kif("(instance Ampere CompositeUnitOfMeasure)").
:- load_kif("(instance Ampere SystemeInternationalUnit)").
:- load_kif("(documentation Ampere EnglishLanguage \"SI electric current measure. Symbol: A. It is one of the base units in SI. It is defined as follows: the &%Ampere is that constant current which, if maintained in two straight parallel conductors of infinite length, of negligible circular cross-section, and placed 1 &%Meter apart in a vacuum, would produce between these conductors a force equal to 2*10^(-7) &%Newton per &%Meter of length.\")").
% :- load_kif("; Thermodynamic Temperature Base Unit").
:- load_kif("(instance KelvinDegree UnitOfTemperature)").
:- load_kif("(instance KelvinDegree SystemeInternationalUnit)").
:- load_kif("(documentation KelvinDegree EnglishLanguage \"SI &%UnitOfMeasure used with &%MeasureFn to produce terms denoting instances of &%TemperatureMeasure. Symbol: K. It is one of the base units in SI (it is also a unit in the ITS system). Kelvin differs from the Celsius scale in that the triple point of water is defined to be 273.16 &%KelvinDegrees while it is 0 &%CelsiusDegrees. The magnitudes of intervals in the two scales are the same. By definition the conversion constant is 273.15.\")").
% :- load_kif("; Amount Of Substance Base Unit").
:- load_kif("(instance Mole UnitOfMass)").
:- load_kif("(instance Mole SystemeInternationalUnit)").
:- load_kif("(documentation Mole EnglishLanguage \"SI amount of substance unit. symbol: mol. It is one of the base units in SI. It is defined as follows: the &%Mole is the amount of substance of a system which contains as many elementary entities as there are atoms in 0.012 &%Kilograms of carbon 12. Note that, when this &%UnitOfMeasure is used, the elementary entities must be specified - they may be atoms, molecules, ions, electrons, etc. or groups of such particles.\")").
% :- load_kif("; Luminosity Intensity Base Unit").
:- load_kif("(instance Candela CompositeUnitOfMeasure)").
:- load_kif("(instance Candela SystemeInternationalUnit)").
:- load_kif("(documentation Candela EnglishLanguage \"SI luminosity intensity measure. Symbol: cd. It is one of the base units in SI, and it is currently defined as follows: the &%Candela is the luminous intensity, in a given direction, of a source that emits monochromatic radiation of frequency 540*10^12 &%Hertz and that has a radiant intensity in that direction of 1/683 &%Watt per &%Steradian.\")").
:- load_kif("(instance Liter UnitOfVolume)").
:- load_kif("(documentation Liter EnglishLanguage \"Unit of volume in the metric system. It is currently defined to be equal to one cubic decimeter (0.001 cubic meter). Symbol: l.\")").
:- load_kif("(instance Centimeter UnitOfLength)").
:- load_kif("(documentation Centimeter EnglishLanguage \"Submultiple of &%Meter. Symbol: cm. It is the 100th part of a &%Meter\")").
:- load_kif("(=> (equal ?NUMBER (MultiplicationFn 1 ?NUMBER)) (equal (MeasureFn ?NUMBER Centimeter) (MeasureFn (MultiplicationFn ?NUMBER 0.01) Meter)))").
:- load_kif("(documentation Millimeter EnglishLanguage \"Submultiple of Meter. Symbol: mm. A millimeter is the 1000th part of a meter\")").
:- load_kif("(instance Millimeter UnitOfLength)").
:- load_kif("(=> (equal ?NUMBER (MultiplicationFn 1 ?NUMBER)) (equal (MeasureFn ?NUMBER Millimeter) (MeasureFn (MultiplicationFn ?NUMBER 0.001) Meter)))").
:- load_kif("(instance Kilometer UnitOfLength)").
:- load_kif("(documentation Kilometer EnglishLanguage \"Supermultiple of &%Meter. Symbol: km. A &%Meter is the 1000th part of a &%Kilometer\")").
:- load_kif("(=> (equal ?NUMBER (MultiplicationFn 1 ?NUMBER)) (equal (MeasureFn ?NUMBER Kilometer) (MeasureFn (MultiplicationFn ?NUMBER 1000) Meter)))").
% :- load_kif("; What follows are derived SI units with special names and symbols (multiples and submultiples are together since they represent quantities of the same kind).").
% :- load_kif("; Plane angle unit").
:- load_kif("(instance Radian UnitOfAngularMeasure)").
:- load_kif("(instance Radian SystemeInternationalUnit)").
:- load_kif("(documentation Radian EnglishLanguage \"SI plane angle measure. Symbol: rad. It is the angle of a circle subtended by an arc equal in length to the circle's radius. Another definition is: the plane angle between two radii of a circle which cut off on the circumference an arc equal in length to the radius. &%Radian = m/m = 1.\")").
% :- load_kif("; Solid angle unit").
:- load_kif("(instance Steradian UnitOfAngularMeasure)").
:- load_kif("(instance Steradian SystemeInternationalUnit)").
:- load_kif("(documentation Steradian EnglishLanguage \"SI solid angle measure. Symbol: sr. It is the solid angle of a sphere subtended by a portion of the surface whose area is equal to the square of the sphere's radius. Another definition is: the solid angle which, having its vertex in the center of the sphere, cuts off an area of the surface of the sphere equal to that of a square with sides of length equal to the radius of the sphere. &%Steradian = m^2/m^2 = 1.\")").
% :- load_kif("; Frequency units").
:- load_kif("(subclass FrequencyMeasure TimeDependentQuantity)").
:- load_kif("(documentation FrequencyMeasure EnglishLanguage \"A &%subclass of &%TimeDependentQuantity, instances of which are measures of the frequency with which some &%Process occurs.\")").
:- load_kif("(subclass UnitOfFrequency CompositeUnitOfMeasure)").
:- load_kif("(documentation UnitOfFrequency EnglishLanguage \"Every instance of this &%Class is a &%UnitOfMeasure that can be used with &%MeasureFn to form instances of &%FrequencyMeasure.\")").
:- load_kif("(=> (and (equal (MeasureFn ?NUMBER ?UNIT) ?QUANT) (instance ?UNIT UnitOfFrequency)) (instance ?QUANT FrequencyMeasure))").
:- load_kif("(instance Hertz UnitOfFrequency)").
:- load_kif("(instance Hertz SystemeInternationalUnit)").
:- load_kif("(documentation Hertz EnglishLanguage \"SI frequency measure. Symbol: Hz. It is the number of cycles per second. &%Hertz = s^(-1). Note that &%Hertz does not have a conversion function.\")").
% :- load_kif("; Force Unit").
:- load_kif("(instance Newton CompositeUnitOfMeasure)").
:- load_kif("(instance Newton SystemeInternationalUnit)").
:- load_kif("(documentation Newton EnglishLanguage \"SI force measure. Symbol: N. It is that force which gives to a mass of 1 kilogram an acceleration of 1 &%Meter per &%SecondDuration. &%Newton = m*kg*s^(-2).\")").
% :- load_kif("; Pressure unit").
:- load_kif("(instance Pascal CompositeUnitOfMeasure)").
:- load_kif("(instance Pascal SystemeInternationalUnit)").
:- load_kif("(documentation Pascal EnglishLanguage \"SI pressure measure. Symbol:Pa. It is the pressure of one &%Newton per square &%Meter. &%Pascal = N/m^2 = m^(-1)*kg*s^(-2).\")").
% :- load_kif("; Energy Unit").
:- load_kif("(instance Joule CompositeUnitOfMeasure)").
:- load_kif("(instance Joule SystemeInternationalUnit)").
:- load_kif("(documentation Joule EnglishLanguage \"SI energy measure. Symbol: J. It is the work done when the point of application of 1 &%Newton is displaced a distance of 1 &%Meter in the direction of the force. &%Joule = N*m = m^2*kg*s^(-2).\")").
% :- load_kif("; Power Units").
:- load_kif("(instance Watt CompositeUnitOfMeasure)").
:- load_kif("(instance Watt SystemeInternationalUnit)").
:- load_kif("(documentation Watt EnglishLanguage \"SI power measure. Symbol: W. A &%UnitOfMeasure that measures power, i.e. energy produced or expended divided by &%TimeDuration. It is the power which gives rise to the production of energy (or work) at the rate of one &%Joule per &%SecondDuration. &%Watt = J/s = m^2*kg*s^(-3).\")").
:- load_kif("(instance Horsepower CompositeUnitOfMeasure)").
:- load_kif("(documentation Horsepower EnglishLanguage \"A power measure that is equal to 746 &%Watts.\")").
:- load_kif("(=> (instance ?N RealNumber) (equal (MeasureFn ?N Horsepower) (MeasureFn (MultiplicationFn ?N 746) Watt)))").
% :- load_kif("; Note: According to SI one should not use the expression \"per unit of.\"").
% :- load_kif("; Electric Charge Units").
:- load_kif("(instance Coulomb CompositeUnitOfMeasure)").
:- load_kif("(instance Coulomb SystemeInternationalUnit)").
:- load_kif("(documentation Coulomb EnglishLanguage \"SI electric charge measure. Symbol: C. It is the quantity of electric charge transported through a cross section of a conductor in an electric circuit during each &%SecondDuration by a current of 1 &%Ampere. Coulomb = s*A.\")").
:- load_kif("(=> (equal ?QUANTITY (MeasureFn ?NUMBER Coulomb)) (instance ?QUANTITY TimeDependentQuantity))").
% :- load_kif("; Electric Potential Units").
:- load_kif("(instance Volt CompositeUnitOfMeasure)").
:- load_kif("(instance Volt SystemeInternationalUnit)").
:- load_kif("(documentation Volt EnglishLanguage \"SI electric potential measure. Symbol: V. It is the difference of electric potential between two points of a conducting wire carrying a constant current of 1 &%Ampere, when the power dissipated between these points is equal to 1 &%Watt. &%Volt = W/A = m^2*kg*s^(-3)*A^(-1).\")").
% :- load_kif("; Capacitance Units").
:- load_kif("(instance Farad CompositeUnitOfMeasure)").
:- load_kif("(instance Farad SystemeInternationalUnit)").
:- load_kif("(documentation Farad EnglishLanguage \"SI capacitance measure. Symbol: F. It is the capacitance of a capacitator between the plates of which there appears a difference of potential of 1 &%Volt when it is charged by a quantity of electricity equal to 1 Coulomb. &%Farad = C/V = m^(-2)*kg(-1)*s^4*A^2.\")").
:- load_kif(" Electric Resistance Units").
:- load_kif("(instance Ohm CompositeUnitOfMeasure)").
:- load_kif("(instance Ohm SystemeInternationalUnit)").
:- load_kif("(documentation Ohm EnglishLanguage \"SI electric resistance measure. It is the electric resistance between two points of a conductor when a constant difference of potential of 1 &%Volt, applied between these two points, produces in this conductor a current of 1 &%Ampere, this conductor not being the force of any electromotive force. &%Ohm = V/A = m^2*kg*s^(-3)*A^(-2).\")").
% :- load_kif("; Electric Conductance Units").
:- load_kif("(instance Siemens CompositeUnitOfMeasure)").
:- load_kif("(instance Siemens SystemeInternationalUnit)").
:- load_kif("(documentation Siemens EnglishLanguage \"SI electric conductance measure. Symbol: S. In the case of direct current, the conductance in &%Siemens is the reciprocal of the resistance in &%Ohms, in the case of alternating current, it is the reciprocal of the impedance in ohms. siemens = A/V = m^(-2)*kg(-1)*s^(3)*A^2.\")").
% :- load_kif("; Magnetic Flux Units").
:- load_kif("(instance Weber CompositeUnitOfMeasure)").
:- load_kif("(instance Weber SystemeInternationalUnit)").
:- load_kif("(documentation Weber EnglishLanguage \"SI magnetic flux measure. Symbol: Wb. It is the magnetic flux which, linking a circuit of one turn, produces in it an electromotive force of 1 &%Volt as it is reduced to zero at a uniform rate in 1 &%SecondDuration. &%Weber = V*s = m^2*kg*s^(-2)*A^(-1).\" )").
% :- load_kif("; Magnetic Flux Density Units").
:- load_kif("(instance Tesla CompositeUnitOfMeasure)").
:- load_kif("(instance Tesla SystemeInternationalUnit)").
:- load_kif("(documentation Tesla EnglishLanguage \"SI magnetic flux density measure. Symbol: T. One &%Tesla equals one &%Weber per square &%Meter. &%Tesla = Wb/m^2 = kg*s^(-2)*A^(-1).\")").
% :- load_kif("; Inductance Units").
:- load_kif("(instance Henry CompositeUnitOfMeasure)").
:- load_kif("(instance Henry SystemeInternationalUnit)").
:- load_kif("(documentation Henry EnglishLanguage \"SI inductance measure. Symbol: H. One &%Henry is equivalent to one &%Volt divided by one &%Ampere per &%SecondDuration. If a current changing at the rate of one &%Ampere per &%SecondDuration induces an electromotive force of one &%Volt, the circuit has an inductance of one &%Henry. &%Henry = Wb/A = m^2*kg*s^(-2)*A^(-2).\")").
% :- load_kif("; Celsius Temperature unit").
:- load_kif("(instance CelsiusDegree UnitOfTemperature)").
:- load_kif("(instance CelsiusDegree SystemeInternationalUnit)").
:- load_kif("(documentation CelsiusDegree EnglishLanguage \"A &%TemperatureMeasure. The freezing point and the boiling point of water are, respectively, 0 &%CelsiusDegrees and 100 &%CelsiusDegrees.\")").
:- load_kif("(=> (equal ?NUMBER (MultiplicationFn 1 ?NUMBER)) (equal (MeasureFn ?NUMBER CelsiusDegree) (MeasureFn (SubtractionFn ?NUMBER 273.15) KelvinDegree)))").
:- load_kif("(=> (equal ?NUMBER (MultiplicationFn 1 ?NUMBER)) (equal (MeasureFn ?NUMBER CelsiusDegree) (MeasureFn (DivisionFn (SubtractionFn ?NUMBER 32) 1.8) FahrenheitDegree)))").
% :- load_kif("; Luminous Flux Units").
:- load_kif("(instance Lumen CompositeUnitOfMeasure)").
:- load_kif("(instance Lumen SystemeInternationalUnit)").
:- load_kif("(documentation Lumen EnglishLanguage \"SI luminous flux measure. Symbol: lm. It is the amount streaming outward through one solid angle of 1 &%Steradian from a uniform point source having an intensity of one &%Candela. &%Lumen = cd*sr = cd * 1.\")").
% :- load_kif("; Illuminance Units").
:- load_kif("(instance Lux CompositeUnitOfMeasure)").
:- load_kif("(instance Lux SystemeInternationalUnit)").
:- load_kif("(documentation Lux EnglishLanguage \"SI illuminance measure. Symbol: lx. It is the amount of illumination provided when one &%Lumen is evenly distributed over an area of 1 square &%Meter. This is also equivalent to the illumination that would exist on a surface all points of which are one &%Meter from a point source of one &%Candela. &%Lux = lm/m^2 = m^(-2)*cd.\")").
% :- load_kif("; Activity Units").
:- load_kif("(instance Becquerel CompositeUnitOfMeasure)").
:- load_kif("(instance Becquerel SystemeInternationalUnit)").
:- load_kif("(documentation Becquerel EnglishLanguage \"SI activity measure. Symbol: Bq. It measures the amount of radioactivity contained in a given sample of matter. It is that quantity of a radioactive element in which there is one atomic disintegration per &%SecondDuration. &%Becquerel = s^(-1).\")").
:- load_kif("(=> (equal ?QUANTITY (MeasureFn ?NUMBER Becquerel)) (instance ?QUANTITY TimeDependentQuantity))").
% :- load_kif("; Absorbed Dose Units").
:- load_kif("(instance Gray CompositeUnitOfMeasure)").
:- load_kif("(instance Gray SystemeInternationalUnit)").
:- load_kif("(documentation Gray EnglishLanguage \"SI absorbed dose measure. Symbol: Gy. It measures the dose of radiation absorbed in living tissue. It is equal approximately to the absorbed dose delivered when the energy per unit mass imparted to matter by ionizing radiation is 1 &%Joule per kilogram. &%Gray = J/kg = m^2*s^(-2).\")").
% :- load_kif("; Dose Equivalent Units").
:- load_kif("(instance Sievert CompositeUnitOfMeasure)").
:- load_kif("(instance Sievert SystemeInternationalUnit)").
:- load_kif("(documentation Sievert EnglishLanguage \"SI dose equivalent measure. Symbol: Sv. It is a unit of biologic dose of ionizing radiation. The &%Sievert makes it possible to normalize doses of different types of radiation. It takes into account the relative biologic effectiveness of ionizing radiation, since each form of such radiation--e.g., X rays, gamma rays, neutrons-- has a slightly different effect on living tissue for a given absorbed dose. The dose equivalent of a given type of radiation (in &%Sievert) is the dose of the radiation in &%Gray multiplied by a quality factor that is based on the relative biologic effectiveness of the radiation. Accordingly, one &%Sievert is generally defined as the amount of radiation roughly equivalent in biologic effectiveness to one &%Gray of gamma radiation. &%Sievert = J/kg = m^2*s^(-2)\")").
% :- load_kif("; Units that are accepted for -use- with SI").
:- load_kif("(instance DayDuration UnitOfDuration)").
:- load_kif("(documentation DayDuration EnglishLanguage \"Time unit. 1 day = 24 hours.\")").
:- load_kif("(=> (equal ?NUMBER (MultiplicationFn 1 ?NUMBER)) (equal (MeasureFn ?NUMBER DayDuration) (MeasureFn (MultiplicationFn ?NUMBER 24) HourDuration)))").
:- load_kif("(instance HourDuration UnitOfDuration)").
:- load_kif("(documentation HourDuration EnglishLanguage \"Time unit. 1 hour = 60 minutes.\")").
:- load_kif("(=> (equal ?NUMBER (MultiplicationFn 1 ?NUMBER)) (equal (MeasureFn ?NUMBER HourDuration) (MeasureFn (MultiplicationFn ?NUMBER 60) MinuteDuration)))").
:- load_kif("(instance MinuteDuration UnitOfDuration)").
:- load_kif("(documentation MinuteDuration EnglishLanguage \"Time unit. 1 minute = 60 seconds. \")").
:- load_kif("(=> (equal ?NUMBER (MultiplicationFn 1 ?NUMBER)) (equal (MeasureFn ?NUMBER MinuteDuration) (MeasureFn (MultiplicationFn ?NUMBER 60) SecondDuration)))").
:- load_kif("(instance WeekDuration UnitOfDuration)").
:- load_kif("(documentation WeekDuration EnglishLanguage \"Time unit. A week's duration is seven days.\")").
:- load_kif("(=> (equal ?NUMBER (MultiplicationFn 1 ?NUMBER)) (equal (MeasureFn ?NUMBER WeekDuration) (MeasureFn (MultiplicationFn ?NUMBER 7) DayDuration)))").
:- load_kif("(instance MonthDuration UnitOfDuration)").
:- load_kif("(documentation MonthDuration EnglishLanguage \"Time unit. A month's duration is at least 28 days, and no more than 31 days. Note that this unit is a range, rather than an exact amount, unlike most other units.\")").
:- load_kif("(=> (equal ?NUMBER (MultiplicationFn 1 ?NUMBER)) (greaterThanOrEqualTo (MeasureFn ?NUMBER MonthDuration) (MeasureFn (MultiplicationFn ?NUMBER 28) DayDuration)))").
:- load_kif("(=> (equal ?NUMBER (MultiplicationFn 1 ?NUMBER)) (lessThanOrEqualTo (MeasureFn ?NUMBER MonthDuration) (MeasureFn (MultiplicationFn ?NUMBER 31) DayDuration)))").
:- load_kif("(instance YearDuration UnitOfDuration)").
:- load_kif("(documentation YearDuration EnglishLanguage \"Time unit. one calendar year. 1 year = 365 days = 31536000 seconds.\")").
:- load_kif("(=> (equal ?NUMBER (MultiplicationFn 1 ?NUMBER)) (equal (MeasureFn ?NUMBER YearDuration) (MeasureFn (MultiplicationFn ?NUMBER 365) DayDuration)))").
% :- load_kif("; What follows are units that are also accepted for use with SI. The SI equivalents for these units are obtained experimentally.").
:- load_kif("(instance Amu UnitOfMass)").
:- load_kif("(documentation Amu EnglishLanguage \"Atomic mass unit. Symbol: u. It is the mass of the twelfth part of an atom of the Carbon 12 isotope.\")").
:- load_kif("(=> (equal ?NUMBER (MultiplicationFn 1 ?NUMBER)) (equal (MeasureFn ?NUMBER Amu) (MeasureFn (MultiplicationFn ?NUMBER 1.6605402E-24) Gram)))").
:- load_kif("(instance ElectronVolt CompositeUnitOfMeasure)").
:- load_kif("(documentation ElectronVolt EnglishLanguage \"The &%ElectronVolt is an energy measure. Symbol: eV. It is the kinetic energy acquired by an electron in passing through a potential difference of 1 &%Volt in a vacuum.\")").
:- load_kif("(=> (equal ?NUMBER (MultiplicationFn 1 ?NUMBER)) (equal (MeasureFn ?NUMBER ElectronVolt) (MeasureFn (MultiplicationFn ?NUMBER 1.60217733E-19) Joule)))").
% :- load_kif("; The following units have been temporarily accepted for use with SI units.").
:- load_kif("(instance Angstrom UnitOfLength)").
:- load_kif("(documentation Angstrom EnglishLanguage \"The &%Angstrom is a &%LengthMeasure. 1 &%Angstrom = 10^(-10) m\")").
:- load_kif("(=> (equal ?NUMBER (MultiplicationFn 1 ?NUMBER)) (equal (MeasureFn ?NUMBER Angstrom) (MeasureFn (MultiplicationFn ?NUMBER 1.0E-10) Meter)))").
% :- load_kif("; The following units are unacceptable in SI but are part of other systems of measurement that are widely used.").
% :- load_kif("; More Length units").
:- load_kif("(instance FootLength UnitOfLength)").
:- load_kif("(documentation FootLength EnglishLanguage \"English length unit of feet.\")").
:- load_kif("(=> (equal ?NUMBER (MultiplicationFn 1 ?NUMBER)) (equal (MeasureFn ?NUMBER FootLength) (MeasureFn (MultiplicationFn ?NUMBER 0.3048) Meter)))").
:- load_kif("(instance Inch UnitOfLength)").
:- load_kif("(documentation Inch EnglishLanguage \"English length unit of inches.\")").
:- load_kif("(=> (equal ?NUMBER (MultiplicationFn 1 ?NUMBER)) (equal (MeasureFn ?NUMBER Inch) (MeasureFn (MultiplicationFn ?NUMBER 0.0254) Meter)))").
:- load_kif("(instance Mile UnitOfLength)").
:- load_kif("(documentation Mile EnglishLanguage \"English length unit of miles.\")").
:- load_kif("(=> (equal ?NUMBER (MultiplicationFn 1 ?NUMBER)) (equal (MeasureFn ?NUMBER Mile) (MeasureFn (MultiplicationFn ?NUMBER 1609.344) Meter)))").
% :- load_kif("; More Volume units").
:- load_kif("(instance UnitedStatesGallon UnitOfVolume)").
:- load_kif("(relatedInternalConcept UnitedStatesGallon UnitedKingdomGallon)").
:- load_kif("(documentation UnitedStatesGallon EnglishLanguage \"Unit of volume commonly used in the United States.\")").
:- load_kif("(=> (equal ?NUMBER (MultiplicationFn 1 ?NUMBER)) (equal (MeasureFn ?NUMBER UnitedStatesGallon) (MeasureFn (MultiplicationFn ?NUMBER 3.785411784) Liter)))").
:- load_kif("(instance Quart UnitOfVolume)").
:- load_kif("(documentation Quart EnglishLanguage \"English unit of volume equal to 1/4 of a &%UnitedStatesGallon.\")").
:- load_kif("(=> (equal ?NUMBER (MultiplicationFn 1 ?NUMBER)) (equal (MeasureFn ?NUMBER Quart) (MeasureFn (DivisionFn ?NUMBER 4) UnitedStatesGallon)))").
:- load_kif("(instance Pint UnitOfVolume)").
:- load_kif("(documentation Pint EnglishLanguage \"English unit of volume equal to 1/2 of a &%Quart.\")").
:- load_kif("(=> (equal ?NUMBER (MultiplicationFn 1 ?NUMBER)) (equal (MeasureFn ?NUMBER Pint) (MeasureFn (DivisionFn ?NUMBER 2) Quart)))").
:- load_kif("(instance Cup UnitOfVolume)").
:- load_kif("(documentation Cup EnglishLanguage \"English unit of volume equal to 1/2 of a &%Pint.\")").
:- load_kif("(=> (equal ?NUMBER (MultiplicationFn 1 ?NUMBER)) (equal (MeasureFn ?NUMBER Cup) (MeasureFn (DivisionFn ?NUMBER 2) Pint)))").
:- load_kif("(instance Ounce UnitOfVolume)").
:- load_kif("(documentation Ounce EnglishLanguage \"English unit of volume equal to 1/8 of a &%Cup.\")").
:- load_kif("(=> (equal ?NUMBER (MultiplicationFn 1 ?NUMBER)) (equal (MeasureFn ?NUMBER Ounce) (MeasureFn (DivisionFn ?NUMBER 8) Cup)))").
:- load_kif("(instance UnitedKingdomGallon UnitOfVolume)").
:- load_kif("(documentation UnitedKingdomGallon EnglishLanguage \"Unit of volume commonly used in the United Kingdom.\")").
:- load_kif("(=> (equal ?NUMBER (MultiplicationFn 1 ?NUMBER)) (equal (MeasureFn ?NUMBER UnitedKingdomGallon) (MeasureFn (MultiplicationFn ?NUMBER 4.54609) Liter)))").
% :- load_kif("; More Mass units").
:- load_kif("(instance AtomGram UnitOfMass)").
:- load_kif("(documentation AtomGram EnglishLanguage \"&%MassMeasure that is also known as the gram-atom. Defined as the mass in grams of 1 &%Mole of pure substance. For example, 1 &%AtomGram of Carbon 12 will be 12 &%Grams of pure Carbon 12. 2 &%AtomGrams of the same substance will be 24 &%Grams of it. This is an unusual unit in that it is essentially 1 &%Mole of 'stuff' measured in grams, so that the actual value (i.e. mass) depends on the type of substance.\")").
:- load_kif("(documentation Kilogram EnglishLanguage \"Supermultiple of &%Gramm. Symbol: kg. 1 &%Kilogram = 1000 Grams.\")").
:- load_kif("(instance Kilogram SystemeInternationalUnit)").
:- load_kif("(instance Kilogram UnitOfMass)").
:- load_kif("(=> (equal ?NUMBER (MultiplicationFn 1 ?NUMBER)) (equal (MeasureFn ?NUMBER Kilogram) (MeasureFn (MultiplicationFn ?NUMBER 1000) Gram)))").
:- load_kif("(domain weight 1 SelfConnectedObject)").
:- load_kif("(domain weight 2 MassMeasure)").
:- load_kif("(instance weight BinaryPredicate)").
:- load_kif("(subrelation weight measure)").
:- load_kif("(documentation weight EnglishLanguage \"(&%weight ?O ?MM) means that on planet earth the &%SelfConnectedObject ?O has the weight ?MM.\")").
:- load_kif("(instance PoundMass UnitOfMass)").
:- load_kif("(documentation PoundMass EnglishLanguage \"English mass unit of pounds.\")").
:- load_kif("(=> (equal ?NUMBER (MultiplicationFn 1 ?NUMBER)) (equal (MeasureFn ?NUMBER PoundMass) (MeasureFn (MultiplicationFn ?NUMBER 453.59237) Gram)))").
:- load_kif("(instance Slug UnitOfMass)").
:- load_kif("(documentation Slug EnglishLanguage \"English mass unit of slugs.\")").
:- load_kif("(=> (equal ?NUMBER (MultiplicationFn 1 ?NUMBER)) (equal (MeasureFn ?NUMBER Slug) (MeasureFn (MultiplicationFn ?NUMBER 14593.90) Gram)))").
% :- load_kif("; More Temperature units").
:- load_kif("(instance RankineDegree UnitOfTemperature)").
:- load_kif("(documentation RankineDegree EnglishLanguage \"A &%TemperatureMeasure. Note that 0 &%RankineDegrees is the same as the absolute zero (i.e. 0 &%KelvinDegrees).\")").
:- load_kif("(=> (equal ?NUMBER (MultiplicationFn 1 ?NUMBER)) (equal (MeasureFn ?NUMBER RankineDegree) (MeasureFn (MultiplicationFn ?NUMBER 1.8) KelvinDegree)))").
:- load_kif("(instance FahrenheitDegree UnitOfTemperature)").
:- load_kif("(documentation FahrenheitDegree EnglishLanguage \"A &%UnitOfTemperature that is commonly used in the United States. On the Fahrenheit scale, the freezing point of water is 32 &%FahrenheitDegrees, and the boiling point of water is 212 &%FahrenheitDegrees.\")").
% :- load_kif("; More Force units").
:- load_kif("(instance PoundForce CompositeUnitOfMeasure)").
:- load_kif("(documentation PoundForce EnglishLanguage \"English pound of force. The conversion factor depends on the local value of the acceleration of free fall. A mean value is used in the conversion axiom associated with this constant.\")").
:- load_kif("(=> (equal ?NUMBER (MultiplicationFn 1 ?NUMBER)) (equal (MeasureFn ?NUMBER PoundForce) (MeasureFn (MultiplicationFn ?NUMBER 4.448222) Newton)))").
% :- load_kif("; More Energy units").
:- load_kif("(instance Calorie CompositeUnitOfMeasure)").
:- load_kif("(documentation Calorie EnglishLanguage \"A &%Calorie is an energy measure.\")").
:- load_kif("(=> (equal ?NUMBER (MultiplicationFn 1 ?NUMBER)) (equal (MeasureFn ?NUMBER Calorie) (MeasureFn (MultiplicationFn ?NUMBER 4.1868) Joule)))").
:- load_kif("(instance BritishThermalUnit CompositeUnitOfMeasure)").
:- load_kif("(documentation BritishThermalUnit EnglishLanguage \"An energy measure.\")").
:- load_kif("(=> (equal ?NUMBER (MultiplicationFn 1 ?NUMBER)) (equal (MeasureFn ?NUMBER BritishThermalUnit) (MeasureFn (MultiplicationFn ?NUMBER 1055.05585262) Joule)))").
% :- load_kif("; More plane angle units").
:- load_kif("(instance AngularDegree UnitOfAngularMeasure)").
:- load_kif("(documentation AngularDegree EnglishLanguage \"A plane angle measure.\")").
:- load_kif("(=> (equal ?QUANT (MeasureFn ?N AngularDegree)) (instance ?QUANT PlaneAngleMeasure))").
:- load_kif("(=> (equal ?NUMBER (MultiplicationFn 1 ?NUMBER)) (equal (MeasureFn ?NUMBER AngularDegree) (MeasureFn (MultiplicationFn ?NUMBER (DivisionFn Pi 180)) Radian)))").
:- load_kif("(=> (measure ?ANGLE (MeasureFn ?NUMBER AngularDegree)) (and (greaterThanOrEqualTo ?NUMBER 0) (lessThanOrEqualTo ?NUMBER 360)))").
:- load_kif("(equal (MeasureFn 0 AngularDegree) (MeasureFn 360 AngularDegree))").
% :- load_kif("; Other interesting units of measure").
% :- load_kif("; Currency units").
:- load_kif("(instance UnitedStatesDollar UnitOfCurrency)").
:- load_kif("(documentation UnitedStatesDollar EnglishLanguage \"A currency measure.\")").
:- load_kif("(instance UnitedStatesCent UnitOfCurrency)").
:- load_kif("(documentation UnitedStatesCent EnglishLanguage \"A currency measure. 1 &%UnitedStatesCent is equal to .01 &%UnitedStatesDollars.\")").
:- load_kif("(=> (equal ?NUMBER (MultiplicationFn 1 ?NUMBER)) (equal (MeasureFn ?NUMBER UnitedStatesCent) (MeasureFn (MultiplicationFn ?NUMBER 0.01) UnitedStatesDollar)))").
:- load_kif("(instance EuroDollar UnitOfCurrency)").
:- load_kif("(documentation EuroDollar EnglishLanguage \"A currency measure of most European Union countries.\")").
:- load_kif("(instance EuroCent UnitOfCurrency)").
:- load_kif("(documentation EuroCent EnglishLanguage \"A currency measure. 1 &%EuroCent is equal to .01 &%EuroDollars.\")").
:- load_kif("(=> (equal ?NUMBER (MultiplicationFn 1 ?NUMBER)) (equal (MeasureFn ?NUMBER EuroCent) (MeasureFn (MultiplicationFn ?NUMBER 0.01) EuroDollar)))").
% :- load_kif("; Information units").
:- load_kif("(subclass InformationMeasure ConstantQuantity)").
:- load_kif("(documentation InformationMeasure EnglishLanguage \"Measures of the amount of information. Includes &%Bit, &%Byte, and multiples of these, e.g. &%KiloByte and &%MegaByte.\")").
:- load_kif("(instance Bit UnitOfInformation)").
:- load_kif("(documentation Bit EnglishLanguage \"One &%Bit of information. A one or a zero.\")").
:- load_kif("(instance Byte UnitOfInformation)").
:- load_kif("(documentation Byte EnglishLanguage \"One &%Byte of information. A &%Byte is eight &%Bits.\")").
:- load_kif("(=> (equal ?NUMBER (MultiplicationFn 1 ?NUMBER)) (equal (MeasureFn ?NUMBER Byte) (MeasureFn (MultiplicationFn ?NUMBER 8) Bit)))").
:- load_kif("(instance KiloByte UnitOfInformation)").
:- load_kif("(documentation KiloByte EnglishLanguage \"One &%KiloByte (KB) of information. One &%KiloByte is 1024 &%Bytes. Note that this sense of 'kilo' is different from the one accepted in the SI system.\")").
:- load_kif("(=> (equal ?NUMBER (MultiplicationFn 1 ?NUMBER)) (equal (MeasureFn ?NUMBER KiloByte) (MeasureFn (MultiplicationFn ?NUMBER 1024) Byte)))").
:- load_kif("(instance MegaByte UnitOfInformation)").
:- load_kif("(documentation MegaByte EnglishLanguage \"One &%MegaByte (MB) of information. One &%MegaByte is 1024 &%KiloBytes. Note that this sense of 'mega' is different from the one accepted in the SI system.\")").
:- load_kif("(=> (equal ?NUMBER (MultiplicationFn 1 ?NUMBER)) (equal (MeasureFn ?NUMBER MegaByte) (MeasureFn (MultiplicationFn ?NUMBER 1024) KiloByte)))").
% :- load_kif("; The following content was inspired by the Quantities ontology developed by ITBM-CNR.").
:- load_kif("(instance measure BinaryPredicate)").
:- load_kif("(instance measure AsymmetricRelation)").
:- load_kif("(domain measure 1 Object)").
:- load_kif("(domain measure 2 PhysicalQuantity)").
:- load_kif("(documentation measure EnglishLanguage \"A very general &%Predicate for asserting that a particular &%Object is measured by a particular &%PhysicalQuantity. In general, the second argument of this &%Predicate will be a term produced with the &%Function &%MeasureFn.\")").
:- load_kif("(instance age SingleValuedRelation)").
:- load_kif("(subrelation age measure)").
:- load_kif("(domain age 2 TimeDuration)").
:- load_kif("(documentation age EnglishLanguage \"Simply relates an &%Object to a &%ConstantQuantity specifying the age of the &%Object.\")").
:- load_kif("(=> (and (instance ?TIME TimePoint) (holdsDuring ?TIME (age ?OBJ ?DURATION))) (duration (TimeIntervalFn (BeginFn (WhenFn ?OBJ)) ?TIME) ?DURATION))").
:- load_kif("(subrelation linearExtent measure)").
:- load_kif("(domain linearExtent 2 LengthMeasure)").
:- load_kif("(documentation linearExtent EnglishLanguage \"&%BinaryPredicate that is used to state the measure of an &%Object from one point to another point along its surface. Note that the difference between the predicates &%length and &%distance is that the &%length is used to state the &%LengthMeasure of one of the dimensions of a single object, while &%distance is used to state the &%LengthMeasure that separates two distinct objects.\")").
:- load_kif("(instance width SingleValuedRelation)").
:- load_kif("(subrelation width linearExtent)").
:- load_kif("(documentation width EnglishLanguage \"&%BinaryPredicate that is used to state the measure of an &%Object from side to side at its widest span.\")").
:- load_kif("(subrelation height linearExtent)").
:- load_kif("(domain height 1 SelfConnectedObject)").
:- load_kif("(domain height 2 LengthMeasure)").
:- load_kif("(documentation height EnglishLanguage \"The height of an &%Object is the distance between its &%top and its &%bottom.\")").
:- load_kif("(instance length SingleValuedRelation)").
:- load_kif("(subrelation length linearExtent)").
:- load_kif("(documentation length EnglishLanguage \"&%BinaryPredicate that is used to state the measure of an &%Object along its longest span.\")").
:- load_kif("(=> (length ?O ?M) (not (exists (?M2) (and (linearExtent ?O ?M2) (greaterThan ?M2 ?M)))))").
:- load_kif("(subclass Oval ClosedTwoDimensionalFigure)").
:- load_kif("(documentation Oval EnglishLanguage \"The class of &%ClosedTwoDimensionalFigures that are produced by the intersection of a &%Cone with a &%ClosedTwoDimensionalFigure.\")").
:- load_kif("(subclass Circle Oval)").
:- load_kif("(documentation Circle EnglishLanguage \"The class of &%Ovals such that all &%GeometricPoints that make up the &%Circle are equidistant from a single &%GeometricPoint, known as the center of the &%Circle.\")").
:- load_kif("(=> (instance ?C Circle) (exists (?R) (radius ?C ?R)))").
:- load_kif("(=> (instance ?C Circle) (exists (?P) (equal (CenterOfCircleFn ?C) ?P)))").
:- load_kif("(documentation CenterOfCircleFn EnglishLanguage \"(CenterOfCircleFn ?CIRCLE) denotes the &%GeometricPoint that is the center of the &%Circle ?CIRCLE.\")").
:- load_kif("(instance CenterOfCircleFn UnaryFunction)").
:- load_kif("(instance CenterOfCircleFn TotalValuedRelation)").
:- load_kif("(domain CenterOfCircleFn 1 Circle)").
:- load_kif("(range CenterOfCircleFn GeometricPoint)").
:- load_kif("(instance radius BinaryPredicate)").
:- load_kif("(instance radius SingleValuedRelation)").
:- load_kif("(instance radius TotalValuedRelation)").
:- load_kif("(domain radius 1 Circle)").
:- load_kif("(domain radius 2 LengthMeasure)").
:- load_kif("(documentation radius EnglishLanguage \"(&%radius ?CIRCLE ?LENGTH) means that the radius of the &%Circle ?CIRCLE has a length of ?LENGTH.\")").
:- load_kif("(=> (radius ?CIRCLE ?RADIUS) (exists (?POINT) (forall (?PART) (=> (pointOfFigure ?PART ?CIRCLE) (geometricDistance ?PART ?POINT ?RADIUS)))))").
:- load_kif("(subrelation diameter width)").
:- load_kif("(instance diameter BinaryPredicate)").
:- load_kif("(instance diameter SingleValuedRelation)").
:- load_kif("(instance diameter TotalValuedRelation)").
:- load_kif("(domain diameter 1 Circle)").
:- load_kif("(domain diameter 2 LengthMeasure)").
:- load_kif("(documentation diameter EnglishLanguage \"(&%diameter ?CIRCLE ?LENGTH) means that the diameter of the &%Circle ?CIRCLE has a length of ?LENGTH.\")").
:- load_kif("(=> (diameter ?CIRCLE ?LENGTH) (exists (?HALF) (and (radius ?CIRCLE ?HALF) (equal (MultiplicationFn ?HALF 2) ?LENGTH))))").
:- load_kif("(instance distance SingleValuedRelation)").
:- load_kif("(instance distance SpatialRelation)").
:- load_kif("(instance distance TernaryPredicate)").
:- load_kif("(domain distance 1 Physical)").
:- load_kif("(domain distance 2 Physical)").
:- load_kif("(domain distance 3 LengthMeasure)").
:- load_kif("(documentation distance EnglishLanguage \"(&%distance ?OBJ1 ?OBJ2 ?QUANT) means that the shortest distance between the two objects ?OBJ1 and ?OBJ2 is ?QUANT. Note that the difference between the predicates &%length and &%distance is that the &%length is used to state the &%LengthMeasure of one of the dimensions of a single object, while &%distance is used to state the &%LengthMeasure that separates two distinct objects.\")").
:- load_kif("(=> (distance ?OBJ1 ?OBJ2 ?QUANT) (distance ?OBJ2 ?OBJ1 ?QUANT))").
:- load_kif("(subrelation altitude distance)").
:- load_kif("(instance altitude SingleValuedRelation)").
:- load_kif("(documentation altitude EnglishLanguage \"A &%TernaryPredicate that is used to state the &%distance between the &%top of an &%Object and another point that is below the &%top of the &%Object (often this other point will be sea level). Note that this &%Predicate can be used to specify, for example, the height of geographic features, e.g. mountains, the altitude of aircraft, and the orbit of satellites around the Earth.\")").
:- load_kif("(=> (altitude ?OBJ1 ?OBJ2 ?HEIGHT) (orientation ?OBJ1 ?OBJ2 Above))").
:- load_kif("(=> (altitude ?OBJ1 ?OBJ2 ?HEIGHT) (exists (?TOP) (and (top ?TOP ?OBJ1) (distance ?TOP ?OBJ2 ?HEIGHT))))").
:- load_kif("(subrelation depth distance)").
:- load_kif("(instance depth SingleValuedRelation)").
:- load_kif("(documentation depth EnglishLanguage \"A &%TernaryPredicate that is used to state the &%distance between the &%top of an &%Object and another point that is above the &%top of the &%Object (often this other point will be sea level). Note that this &%Predicate can be used to specify, for example, the depth of marine life or submarines, for example.\")").
:- load_kif("(=> (depth ?OBJ1 ?OBJ2 ?DEPTH) (orientation ?OBJ1 ?OBJ2 Below))").
:- load_kif("(=> (depth ?OBJ1 ?OBJ2 ?DEPTH) (exists (?BOTTOM) (and (bottom ?BOTTOM ?OBJ1) (distance ?BOTTOM ?OBJ2 ?DEPTH))))").
:- load_kif("(instance larger BinaryPredicate)").
:- load_kif("(instance larger SpatialRelation)").
:- load_kif("(instance larger TransitiveRelation)").
:- load_kif("(instance larger IrreflexiveRelation)").
:- load_kif("(domain larger 1 Object)").
:- load_kif("(domain larger 2 Object)").
:- load_kif("(documentation larger EnglishLanguage \"(&%larger ?OBJ1 ?OBJ2) means that ?OBJ1 is larger, with respect to all &%LengthMeasures, than ?OBJ2.\")").
:- load_kif("(<=> (larger ?OBJ1 ?OBJ2) (forall (?QUANT1 ?QUANT2 ?UNIT) (=> (and (measure ?OBJ1 (MeasureFn ?QUANT1 ?UNIT)) (measure ?OBJ2 (MeasureFn ?QUANT2 ?UNIT)) (instance ?UNIT UnitOfLength)) (greaterThan ?QUANT1 ?QUANT2))))").
:- load_kif("(instance smaller BinaryPredicate)").
:- load_kif("(instance smaller SpatialRelation)").
:- load_kif("(instance smaller TransitiveRelation)").
:- load_kif("(instance smaller IrreflexiveRelation)").
:- load_kif("(domain smaller 1 Object)").
:- load_kif("(domain smaller 2 Object)").
:- load_kif("(inverse smaller larger)").
:- load_kif("(documentation smaller EnglishLanguage \"(&%smaller ?OBJ1 ?OBJ2) means that ?OBJ1 is smaller, with respect to all &%LengthMeasures, than ?OBJ2.\")").
:- load_kif("(instance monetaryValue SingleValuedRelation)").
:- load_kif("(subrelation monetaryValue measure)").
:- load_kif("(domain monetaryValue 1 Physical)").
:- load_kif("(domain monetaryValue 2 CurrencyMeasure)").
:- load_kif("(documentation monetaryValue EnglishLanguage \"A &%BinaryPredicate that associates an &%Object or &%Process with its value expressed as an instance of &%CurrencyMeasure.\")").
:- load_kif("(instance WealthFn UnaryFunction)").
:- load_kif("(domain WealthFn 1 Agent)").
:- load_kif("(range WealthFn CurrencyMeasure)").
:- load_kif("(documentation WealthFn EnglishLanguage \"A &%UnaryFunction that maps an &%Agent to a &%CurrencyMeasure specifying the value of the property owned by the &%Agent. Note that this &%Function is generally used in conjunction with the &%Function &%PropertyFn, e.g. (&%WealthFn (&%PropertyFn BillGates)) would return the monetary value of the sum of Bill Gates' holdings.\")").
:- load_kif("(<=> (equal (WealthFn ?PERSON) ?AMOUNT) (monetaryValue (PropertyFn ?PERSON) ?AMOUNT))").
:- load_kif("(instance barometricPressure BinaryPredicate)").
:- load_kif("(instance barometricPressure AsymmetricRelation)").
:- load_kif("(subrelation barometricPressure measure)").
:- load_kif("(domain barometricPressure 1 Object)").
:- load_kif("(domain barometricPressure 2 UnitOfAtmosphericPressure)").
:- load_kif("(documentation barometricPressure EnglishLanguage \"(&%barometricPressure ?AREA ?PRESSURE) means that the atmospheric pressure measured at ?AREA is ?PRESSURE. Barometric pressure is typically expressed in units of &%InchMercury or &%MmMercury. For example, standard sea level pressure is 29.92 inches (760 mm) of mercury:"). (&%barometricPressure &%SeaLevel (&%MeasureFn 29.92 &%InchMercury)).\")").
:- load_kif("(subclass UnitOfAtmosphericPressure CompositeUnitOfMeasure)").
:- load_kif("(documentation UnitOfAtmosphericPressure EnglishLanguage \"&%UnitOfAtmosphericPressure includes those instances of &%UnitOfMeasure used to measure atmospheric pressure (&%barometricPressure), e.g., &%InchMercury.\")").
:- load_kif("(instance InchMercury UnitOfAtmosphericPressure)").
:- load_kif("(documentation InchMercury EnglishLanguage \"&%InchMercury is a &%UnitOfMeasure for &%barometricPressure. It is used to express the number of inches of mercury supported in a mercurial barometer by the surrounding air pressure.\")").
:- load_kif("(instance MmMercury UnitOfAtmosphericPressure)").
:- load_kif("(documentation MmMercury EnglishLanguage \"&%MmMercury is a &%UnitOfMeasure for &%barometricPressure. It is used to express the number of millimeters of mercury supported in a mercurial barometer by the surrounding air pressure.\")").
% :- load_kif("; END FILE").
% :- load_kif("; BEGIN FILE").
% :- load_kif(";  TEMPORAL CONCEPTS  ").
% :- load_kif("; INCLUDES 'STRUCTURAL ONTOLOGY' INCLUDES 'BASE ONTOLOGY'").
% :- load_kif("; The first part of this section contains definitions relations involving temporal notions. Most of these definitions and axioms were derived from the work of James Allen. This part of the section was extensively revised on the basis of comments from Pat Hayes. The second part of this section is an attempt to incorporate the Simple-Time ontology from the Ontolingua server into the SUMO.").
:- load_kif("(instance PositiveInfinity TimePoint)").
:- load_kif("(documentation PositiveInfinity EnglishLanguage \"The &%TimePoint that is after all other &%TimePoints.\")").
:- load_kif("(=> (and (instance ?POINT TimePoint) (not (equal ?POINT PositiveInfinity))) (before ?POINT PositiveInfinity))").
:- load_kif("(=> (and (instance ?POINT TimePoint) (not (equal ?POINT PositiveInfinity))) (exists (?OTHERPOINT) (temporallyBetween ?POINT ?OTHERPOINT PositiveInfinity)))").
:- load_kif("(instance NegativeInfinity TimePoint)").
:- load_kif("(documentation NegativeInfinity EnglishLanguage \"The &%TimePoint that is before all other &%TimePoints.\")").
:- load_kif("(=> (and (instance ?POINT TimePoint) (not (equal ?POINT NegativeInfinity))) (before NegativeInfinity ?POINT))").
:- load_kif("(=> (and (instance ?POINT TimePoint) (not (equal ?POINT NegativeInfinity))) (exists (?OTHERPOINT) (temporallyBetween NegativeInfinity ?OTHERPOINT ?POINT)))").
:- load_kif("(instance duration BinaryPredicate)").
:- load_kif("(instance duration AsymmetricRelation)").
:- load_kif("(instance duration TotalValuedRelation)").
:- load_kif("(domain duration 1 TimeInterval)").
:- load_kif("(domain duration 2 TimeDuration)").
:- load_kif("(documentation duration EnglishLanguage \"(&%duration ?POS ?TIME) means that the duration of the &%TimePosition ?POS is ?TIME. Note that this &%Predicate can be used in conjunction with the &%Function &%WhenFn to specify the duration of any instance of &%Physical.\")").
:- load_kif("(instance frequency BinaryPredicate)").
:- load_kif("(instance frequency AsymmetricRelation)").
:- load_kif("(domainSubclass frequency 1 Process)").
:- load_kif("(domain frequency 2 TimeDuration)").
:- load_kif("(documentation frequency EnglishLanguage \"(&%frequency ?PROC ?TIME) means that the &%Process type of ?PROC recurs after every interval of ?TIME.\")").
:- load_kif("(=> (frequency ?PROC ?TIME1) (forall (?TIME2) (=> (duration ?TIME2 ?TIME1) (exists (?POSITION) (and (temporalPart ?POSITION ?TIME2) (holdsDuring ?POSITION (exists (?INST) (instance ?INST ?PROC))))))))").
:- load_kif("(instance temporalPart BinaryPredicate)").
:- load_kif("(instance temporalPart TemporalRelation)").
:- load_kif("(instance temporalPart PartialOrderingRelation)").
:- load_kif("(domain temporalPart 1 TimePosition)").
:- load_kif("(domain temporalPart 2 TimePosition)").
:- load_kif("(documentation temporalPart EnglishLanguage \"The temporal analogue of the spatial &%part predicate. (&%temporalPart ?POS1 ?POS2) means that &%TimePosition ?POS1 is part of &%TimePosition ?POS2. Note that since &%temporalPart is a &%ReflexiveRelation every &%TimePostion is a &%temporalPart of itself.\")").
:- load_kif("(=> (instance ?POINT TimePoint) (exists (?INTERVAL) (and (instance ?INTERVAL TimeInterval) (temporalPart ?POINT ?INTERVAL))))").
:- load_kif("(=> (instance ?INTERVAL TimeInterval) (exists (?POINT) (and (instance ?POINT TimePoint) (temporalPart ?POINT ?INTERVAL))))").
:- load_kif("(=> (and (holdsDuring ?TIME1 ?SITUATION) (temporalPart ?TIME2 ?TIME1)) (holdsDuring ?TIME2 ?SITUATION))").
:- load_kif("(=> (and (holdsDuring ?INTERVAL (?REL ?INST1 ?INST2)) (instance ?INST1 Physical) (instance ?INST2 Physical)) (and (time ?INST1 ?INTERVAL) (time ?INST2 ?INTERVAL)))").
:- load_kif("(<=> (temporalPart ?POS (WhenFn ?THING)) (time ?THING ?POS))").
:- load_kif("(instance BeginFn TemporalRelation)").
:- load_kif("(instance BeginFn UnaryFunction)").
:- load_kif("(instance BeginFn TotalValuedRelation)").
:- load_kif("(domain BeginFn 1 TimeInterval)").
:- load_kif("(range BeginFn TimePoint)").
:- load_kif("(documentation BeginFn EnglishLanguage \"A &%UnaryFunction that maps a &%TimeInterval to the &%TimePoint at which the interval begins.\")").
:- load_kif("(=> (origin ?PROCESS ?OBJ) (eventLocated (WhereFn ?PROCESS (BeginFn (WhenFn ?PROCESS))) (WhereFn ?OBJ (BeginFn (WhenFn ?OBJ)))))").
:- load_kif("(=> (equal (BeginFn ?INTERVAL) ?POINT) (forall (?OTHERPOINT) (=> (and (temporalPart ?OTHERPOINT ?INTERVAL) (not (equal ?OTHERPOINT ?POINT))) (before ?POINT ?OTHERPOINT))))").
:- load_kif("(instance EndFn TemporalRelation)").
:- load_kif("(instance EndFn UnaryFunction)").
:- load_kif("(instance EndFn TotalValuedRelation)").
:- load_kif("(domain EndFn 1 TimeInterval)").
:- load_kif("(range EndFn TimePoint)").
:- load_kif("(documentation EndFn EnglishLanguage \"A &%UnaryFunction that maps a &%TimeInterval to the &%TimePoint at which the interval ends.\")").
:- load_kif("(=> (equal (EndFn ?INTERVAL) ?POINT) (forall (?OTHERPOINT) (=> (and (temporalPart ?OTHERPOINT ?INTERVAL) (not (equal ?OTHERPOINT ?POINT))) (before ?OTHERPOINT ?POINT))))").
:- load_kif("(=> (and (resource ?PROC ?OBJ) (holdsDuring (BeginFn (WhenFn ?PROC)) (measure ?OBJ ?QUANT1)) (holdsDuring (EndFn (WhenFn ?PROC)) (measure ?OBJ ?QUANT2))) (greaterThan ?QUANT1 ?QUANT2))").
:- load_kif("(subrelation starts temporalPart)").
:- load_kif("(instance starts TemporalRelation)").
:- load_kif("(instance starts TransitiveRelation)").
:- load_kif("(instance starts IrreflexiveRelation)").
:- load_kif("(domain starts 1 TimeInterval)").
:- load_kif("(domain starts 2 TimeInterval)").
:- load_kif("(documentation starts EnglishLanguage \"(&%starts ?INTERVAL1 ?INTERVAL2) means that ?INTERVAL1 and ?INTERVAL2 are both &%TimeIntervals that have the same initial &%TimePoint and that ?INTERVAL1 ends before ?INTERVAL2.\")").
:- load_kif("(<=> (starts ?INTERVAL1 ?INTERVAL2) (and (equal (BeginFn ?INTERVAL1) (BeginFn ?INTERVAL2)) (before (EndFn ?INTERVAL1) (EndFn ?INTERVAL2))))").
:- load_kif("(subrelation finishes temporalPart)").
:- load_kif("(instance finishes TemporalRelation)").
:- load_kif("(instance finishes TransitiveRelation)").
:- load_kif("(instance finishes IrreflexiveRelation)").
:- load_kif("(domain finishes 1 TimeInterval)").
:- load_kif("(domain finishes 2 TimeInterval)").
:- load_kif("(documentation finishes EnglishLanguage \"(&%finishes ?INTERVAL1 ?INTERVAL2) means that ?INTERVAL1 and ?INTERVAL2 are both &%TimeIntervals that have the same ending &%TimePoint and that ?INTERVAL2 begins before ?INTERVAL1.\")").
:- load_kif("(<=> (finishes ?INTERVAL1 ?INTERVAL2) (and (before (BeginFn ?INTERVAL2) (BeginFn ?INTERVAL1)) (equal (EndFn ?INTERVAL2) (EndFn ?INTERVAL1))))").
:- load_kif("(instance before TemporalRelation)").
:- load_kif("(instance before IrreflexiveRelation)").
:- load_kif("(instance before TransitiveRelation)").
:- load_kif("(subrelation before beforeOrEqual)").
:- load_kif("(relatedInternalConcept before earlier)").
:- load_kif("(domain before 1 TimePoint)").
:- load_kif("(domain before 2 TimePoint)").
:- load_kif("(documentation before EnglishLanguage \"(&%before ?POINT1 ?POINT2) means that ?POINT1 precedes ?POINT2 on the universal timeline.\")").
% :- load_kif("; An Object exists (and, hence, retains its identity) over time, i.e., an object exists at every point over some interval of time.").
:- load_kif("(=> (instance ?OBJ Object) (exists (?TIME1 ?TIME2) (and (instance ?TIME1 TimePoint) (instance ?TIME2 TimePoint) (before ?TIME1 ?TIME2) (forall (?TIME) (=> (and (beforeOrEqual ?TIME1 ?TIME) (beforeOrEqual ?TIME ?TIME2)) (time ?OBJ ?TIME))))))").
:- load_kif("(=> (result ?PROC ?OBJ) (forall (?TIME) (=> (before ?TIME (BeginFn (WhenFn ?PROC))) (not (time ?OBJ ?TIME)))))").
:- load_kif("(=> (instance ?INTERVAL TimeInterval) (before (BeginFn ?INTERVAL) (EndFn ?INTERVAL)))").
:- load_kif("(instance beforeOrEqual BinaryPredicate)").
:- load_kif("(instance beforeOrEqual TemporalRelation)").
:- load_kif("(instance beforeOrEqual PartialOrderingRelation)").
:- load_kif("(domain beforeOrEqual 1 TimePoint)").
:- load_kif("(domain beforeOrEqual 2 TimePoint)").
:- load_kif("(documentation beforeOrEqual EnglishLanguage \"(&%beforeOrEqual ?POINT1 ?POINT2) means that ?POINT1 is identical with ?POINT2 or occurs before it on the universal timeline.\")").
:- load_kif("(=> (beforeOrEqual ?POINT1 ?POINT2) (or (before ?POINT1 ?POINT2) (equal ?POINT1 ?POINT2)))").
:- load_kif("(instance temporallyBetween TemporalRelation)").
:- load_kif("(instance temporallyBetween TernaryPredicate)").
:- load_kif("(subrelation temporallyBetween temporallyBetweenOrEqual)").
:- load_kif("(domain temporallyBetween 1 TimePoint)").
:- load_kif("(domain temporallyBetween 2 TimePoint)").
:- load_kif("(domain temporallyBetween 3 TimePoint)").
:- load_kif("(documentation temporallyBetween EnglishLanguage \"(&%temporallyBetween ?POINT1 ?POINT2 ?POINT3) means that the &%TimePoint ?POINT2 is between the &%TimePoints ?POINT1 and ?POINT3, i.e. ?POINT1 is before ?POINT2 and ?POINT2 is before ?POINT3.\")").
:- load_kif("(<=> (temporallyBetween ?POINT1 ?POINT2 ?POINT3) (and (before ?POINT1 ?POINT2) (before ?POINT2 ?POINT3)))").
:- load_kif("(instance temporallyBetweenOrEqual TemporalRelation)").
:- load_kif("(instance temporallyBetweenOrEqual TernaryPredicate)").
:- load_kif("(domain temporallyBetweenOrEqual 1 TimePoint)").
:- load_kif("(domain temporallyBetweenOrEqual 2 TimePoint)").
:- load_kif("(domain temporallyBetweenOrEqual 3 TimePoint)").
:- load_kif("(documentation temporallyBetweenOrEqual EnglishLanguage \"(&%temporallyBetweenOrEqual ?POINT1 ?POINT2 ?POINT3) means that the &%TimePoint ?POINT1 is before or equal to the &%TimePoint ?POINT2 and ?POINT2 is before or equal to the &%TimePoint ?POINT3.\")").
:- load_kif("(<=> (temporallyBetweenOrEqual ?POINT1 ?POINT2 ?POINT3) (and (beforeOrEqual ?POINT1 ?POINT2) (beforeOrEqual ?POINT2 ?POINT3)))").
:- load_kif("(<=> (and (time ?PHYS ?TIME) (instance ?TIME TimePoint)) (temporallyBetweenOrEqual (BeginFn (WhenFn ?PHYS)) ?TIME (EndFn (WhenFn ?PHYS))))").
:- load_kif("(instance overlapsTemporally BinaryPredicate)").
:- load_kif("(instance overlapsTemporally TemporalRelation)").
:- load_kif("(instance overlapsTemporally ReflexiveRelation)").
:- load_kif("(instance overlapsTemporally SymmetricRelation)").
:- load_kif("(domain overlapsTemporally 1 TimeInterval)").
:- load_kif("(domain overlapsTemporally 2 TimeInterval)").
:- load_kif("(documentation overlapsTemporally EnglishLanguage \"(&%overlapsTemporally ?INTERVAL1 ?INTERVAL2) means that the &%TimeIntervals ?INTERVAL1 and ?INTERVAL2 have a &%TimeInterval as a common part.\")").
:- load_kif("(<=> (overlapsTemporally ?INTERVAL1 ?INTERVAL2) (exists (?INTERVAL3) (and (instance ?INTERVAL3 TimeInterval) (temporalPart ?INTERVAL3 ?INTERVAL1) (temporalPart ?INTERVAL3 ?INTERVAL2))))").
:- load_kif("(=> (and (instance ?REL BinaryPredicate) (instance ?REL SpatialRelation) (?REL ?OBJ1 ?OBJ2)) (overlapsTemporally (WhenFn ?OBJ1) (WhenFn ?OBJ2)))").
:- load_kif("(subrelation during temporalPart)").
:- load_kif("(instance during TransitiveRelation)").
:- load_kif("(instance during IrreflexiveRelation)").
:- load_kif("(subrelation during overlapsTemporally)").
:- load_kif("(domain during 1 TimeInterval)").
:- load_kif("(domain during 2 TimeInterval)").
:- load_kif("(documentation during EnglishLanguage \"(&%during ?INTERVAL1 ?INTERVAL2) means that ?INTERVAL1 starts after and ends before ?INTERVAL2.\")").
:- load_kif("(=> (during ?INTERVAL1 ?INTERVAL2) (and (before (EndFn ?INTERVAL1) (EndFn ?INTERVAL2)) (before (BeginFn ?INTERVAL2) (BeginFn ?INTERVAL1))))").
:- load_kif("(instance meetsTemporally BinaryPredicate)").
:- load_kif("(instance meetsTemporally TemporalRelation)").
:- load_kif("(instance meetsTemporally AsymmetricRelation)").
:- load_kif("(instance meetsTemporally IntransitiveRelation)").
:- load_kif("(domain meetsTemporally 1 TimeInterval)").
:- load_kif("(domain meetsTemporally 2 TimeInterval)").
:- load_kif("(documentation meetsTemporally EnglishLanguage \"(&%meetsTemporally ?INTERVAL1 ?INTERVAL2) means that the terminal point of the &%TimeInterval ?INTERVAL1 is the initial point of the &%TimeInterval ?INTERVAL2.\")").
:- load_kif("(<=> (meetsTemporally ?INTERVAL1 ?INTERVAL2) (equal (EndFn ?INTERVAL1) (BeginFn ?INTERVAL2)))").
:- load_kif("(=> (and (equal (BeginFn ?INTERVAL1) (BeginFn ?INTERVAL2)) (equal (EndFn ?INTERVAL1) (EndFn ?INTERVAL2))) (equal ?INTERVAL1 ?INTERVAL2))").
:- load_kif("(instance earlier BinaryPredicate)").
:- load_kif("(instance earlier TemporalRelation)").
:- load_kif("(instance earlier TransitiveRelation)").
:- load_kif("(instance earlier IrreflexiveRelation)").
:- load_kif("(domain earlier 1 TimeInterval)").
:- load_kif("(domain earlier 2 TimeInterval)").
:- load_kif("(documentation earlier EnglishLanguage \"(&%earlier ?INTERVAL1 ?INTERVAL2) means that the &%TimeInterval ?INTERVAL1 ends before the &%TimeInterval ?INTERVAL2 begins.\")").
:- load_kif("(<=> (earlier ?INTERVAL1 ?INTERVAL2) (before (EndFn ?INTERVAL1) (BeginFn ?INTERVAL2)))").
:- load_kif("(instance cooccur BinaryPredicate)").
:- load_kif("(instance cooccur TemporalRelation)").
:- load_kif("(instance cooccur EquivalenceRelation)").
:- load_kif("(domain cooccur 1 Physical)").
:- load_kif("(domain cooccur 2 Physical)").
:- load_kif("(documentation cooccur EnglishLanguage \"(&%cooccur ?THING1 ?THING2) means that the &%Object or &%Process ?THING1 occurs at the same time as, together with, or jointly with the &%Object or &%Process ?THING2. This covers the following temporal relations: is co-incident with, is concurrent with, is contemporaneous with, and is concomitant with.\")").
:- load_kif("(<=> (cooccur ?PHYS1 ?PHYS2) (equal (WhenFn ?PHYS1) (WhenFn ?PHYS2)))").
% :- load_kif("; The following functions generate &%TimeIntervals.").
:- load_kif("(instance TimeIntervalFn BinaryFunction)").
:- load_kif("(instance TimeIntervalFn TemporalRelation)").
:- load_kif("(domain TimeIntervalFn 1 TimePoint)").
:- load_kif("(domain TimeIntervalFn 2 TimePoint)").
:- load_kif("(range TimeIntervalFn TimeInterval)").
:- load_kif("(documentation TimeIntervalFn EnglishLanguage \"A &%BinaryFunction that takes two &%TimePoints as arguments and returns the &%TimeInterval defined by these two &%TimePoints. Note that the first &%TimePoint must occur earlier than the second &%TimePoint.\")").
:- load_kif("(=> (and (instance ?POINT1 TimePoint) (instance ?POINT2 TimePoint) (instance ?INTERVAL TimeInterval) (equal (TimeIntervalFn ?POINT1 ?POINT2) ?INTERVAL)) (and (equal (BeginFn ?INTERVAL) ?POINT1) (equal (EndFn ?INTERVAL) ?POINT2)))").
:- load_kif("(=> (and (instance ?POINT1 TimePoint) (instance ?POINT2 TimePoint) (instance ?INTERVAL TimeInterval) (equal (TimeIntervalFn ?POINT1 ?POINT2) ?INTERVAL)) (forall (?POINT) (<=> (temporallyBetweenOrEqual ?POINT1 ?POINT ?POINT2) (temporalPart ?POINT ?INTERVAL))))").
:- load_kif("(instance RecurrentTimeIntervalFn TemporalRelation)").
:- load_kif("(instance RecurrentTimeIntervalFn BinaryFunction)").
:- load_kif("(domainSubclass RecurrentTimeIntervalFn 1 TimeInterval)").
:- load_kif("(domainSubclass RecurrentTimeIntervalFn 2 TimeInterval)").
:- load_kif("(rangeSubclass RecurrentTimeIntervalFn TimeInterval)").
:- load_kif("(documentation RecurrentTimeIntervalFn EnglishLanguage \"A function that is useful for generating recurring time intervals. For example, (&%RecurrentTimeIntervalFn (&%HourFn 6 &%Day)"). (&%HourFn 12 &%Day)) returns the &%Class of &%TimeIntervals beginning at 6 in the morning and ending at 12 noon. For another example, (&%RecurrentTimeInterval &%Saturday &%Sunday) returns the &%Class of all weekends. For still another example,"). (&%RecurrentTimeInterval &%June &%August) returns the &%Class containing the academic summer period.\")").
:- load_kif("(=> (instance ?INTERVAL (RecurrentTimeIntervalFn ?TIMECLASS1 ?TIMECLASS2)) (exists (?TIME1 ?TIME2) (and (instance ?TIME1 ?TIMECLASS1) (instance ?TIME2 ?TIMECLASS2) (starts ?TIME1 ?INTERVAL) (finishes ?TIME2 ?INTERVAL))))").
:- load_kif("(instance WhenFn TemporalRelation)").
:- load_kif("(instance WhenFn UnaryFunction)").
:- load_kif("(instance WhenFn TotalValuedRelation)").
:- load_kif("(domain WhenFn 1 Physical)").
:- load_kif("(range WhenFn TimeInterval)").
:- load_kif("(documentation WhenFn EnglishLanguage \"A &%UnaryFunction that maps an &%Object or &%Process to the exact &%TimeInterval during which it exists. Note that, for every &%TimePoint ?TIME outside of the &%TimeInterval").
:- load_kif("(WhenFn ?THING), (time ?THING ?TIME) does not hold.\")").
:- load_kif("(instance PastFn TemporalRelation)").
:- load_kif("(instance PastFn UnaryFunction)").
:- load_kif("(instance PastFn TotalValuedRelation)").
:- load_kif("(domain PastFn 1 TimePosition)").
:- load_kif("(range PastFn TimeInterval)").
:- load_kif("(documentation PastFn EnglishLanguage \"A &%UnaryFunction that maps a &%TimePosition to the &%TimeInterval that meets it and that begins at &%NegativeInfinity.\")").
:- load_kif("(=> (instance ?INTERVAL TimeInterval) (meetsTemporally (PastFn ?INTERVAL) ?INTERVAL))").
:- load_kif("(=> (instance ?INTERVAL TimeInterval) (equal (PastFn ?INTERVAL) (TimeIntervalFn NegativeInfinity (BeginFn ?INTERVAL))))").
:- load_kif("(instance ImmediatePastFn TemporalRelation)").
:- load_kif("(instance ImmediatePastFn UnaryFunction)").
:- load_kif("(instance ImmediatePastFn TotalValuedRelation)").
:- load_kif("(domain ImmediatePastFn 1 TimePosition)").
:- load_kif("(range ImmediatePastFn TimeInterval)").
:- load_kif("(documentation ImmediatePastFn EnglishLanguage \"A &%UnaryFunction that maps a &%TimePosition to a short, indeterminate &%TimeInterval that immediately precedes the &%TimePosition.\")").
:- load_kif("(=> (instance ?INTERVAL TimeInterval) (finishes (ImmediatePastFn ?INTERVAL) (PastFn ?INTERVAL)))").
:- load_kif("(instance FutureFn TemporalRelation)").
:- load_kif("(instance FutureFn UnaryFunction)").
:- load_kif("(instance FutureFn TotalValuedRelation)").
:- load_kif("(domain FutureFn 1 TimePosition)").
:- load_kif("(range FutureFn TimeInterval)").
:- load_kif("(documentation FutureFn EnglishLanguage \"A &%UnaryFunction that maps a &%TimePosition to the &%TimeInterval which it meets and which ends at &%PositiveInfinity.\")").
:- load_kif("(=> (instance ?INTERVAL TimeInterval) (meetsTemporally ?INTERVAL (FutureFn ?INTERVAL)))").
:- load_kif("(=> (instance ?INTERVAL TimeInterval) (equal (FutureFn ?INTERVAL) (TimeIntervalFn (EndFn ?INTERVAL) PositiveInfinity)))").
:- load_kif("(instance ImmediateFutureFn TemporalRelation)").
:- load_kif("(instance ImmediateFutureFn UnaryFunction)").
:- load_kif("(instance ImmediateFutureFn TotalValuedRelation)").
:- load_kif("(domain ImmediateFutureFn 1 TimePosition)").
:- load_kif("(range ImmediateFutureFn TimeInterval)").
:- load_kif("(documentation ImmediateFutureFn EnglishLanguage \"A &%UnaryFunction that maps a &%TimePosition to a short, indeterminate &%TimeInterval that immediately follows the &%TimePosition.\")").
:- load_kif("(=> (instance ?INTERVAL TimeInterval) (starts (ImmediateFutureFn ?INTERVAL) (FutureFn ?INTERVAL)))").
% :- load_kif("; The following definitions and axioms (down to the next section break) cover the content in the Simple-Time ontology on the Ontolingua server.").
:- load_kif("(instance date BinaryPredicate)").
:- load_kif("(instance date SingleValuedRelation)").
:- load_kif("(instance date AsymmetricRelation)").
:- load_kif("(domain date 1 Physical)").
:- load_kif("(domain date 2 Day)").
:- load_kif("(subrelation date time)").
:- load_kif("(documentation date EnglishLanguage \"A &%BinaryPredicate that specifies a &%TimePosition in absolute calendar time, at the resolution of one day, for a particular &%Object or &%Process.\")").
:- load_kif("(instance YearFn TemporalRelation)").
:- load_kif("(instance YearFn UnaryFunction)").
:- load_kif("(domain YearFn 1 Integer)").
:- load_kif("(rangeSubclass YearFn Year)").
:- load_kif("(documentation YearFn EnglishLanguage \"A &%UnaryFunction that maps a number to the corresponding calendar &%Year. For example, (&%YearFn 1912) returns the &%Class containing just one instance, the year of 1912. As might be expected, positive integers return years in the Common Era, while negative integers return years in B.C.E. Note that this function returns a &%Class as a value. The reason for this is that the related functions, viz. &%MonthFn, &%DayFn, &%HourFn, &%MinuteFn, and &%SecondFn, are used to generate both specific &%TimeIntervals and recurrent intervals, and the only way to do this is to make the domains and ranges of these functions classes rather than individuals.\")").
:- load_kif("(instance MonthFn TemporalRelation)").
:- load_kif("(instance MonthFn BinaryFunction)").
:- load_kif("(domainSubclass MonthFn 1 Month)").
:- load_kif("(domainSubclass MonthFn 2 Year)").
:- load_kif("(rangeSubclass MonthFn Month)").
:- load_kif("(documentation MonthFn EnglishLanguage \"A &%BinaryFunction that maps a subclass of &%Month and a subclass of &%Year to the class containing the &%Months corresponding to thos &%Years. For example (&%MonthFn &%January (&%YearFn 1912)) is the class containing the eighth &%Month, i.e. August, of the &%Year 1912. For another example, (&%MonthFn &%August &%Year) is equal to &%August, the class of all months of August. Note that this function returns a &%Class as a value. The reason for this is that the related functions, viz. DayFn, HourFn, MinuteFn, and SecondFn, are used to generate both specific &%TimeIntervals and recurrent intervals, and the only way to do this is to make the domains and ranges of these functions classes rather than individuals.\")").
:- load_kif("(instance DayFn TemporalRelation)").
:- load_kif("(instance DayFn BinaryFunction)").
:- load_kif("(domain DayFn 1 PositiveInteger)").
:- load_kif("(domainSubclass DayFn 2 Month)").
:- load_kif("(rangeSubclass DayFn Day)").
:- load_kif("(documentation DayFn EnglishLanguage \"A &%BinaryFunction that assigns a &%PositiveRealNumber and a subclass of &%Months to the &%Days within each &%Month corresponding to that &%PositiveRealNumber. For example, (&%DayFn 16 &%August) is the &%Class of all sixteenth days of August. For another example, (&%DayFn 9 &%Month) would return the class of all ninth days of any month. For still another example, (&%DayFn 18"). (&%MonthFn &%August (&%YearFn 1912))) denotes the 18th day of August 1912.\")").
:- load_kif("(=> (instance ?DAY (DayFn ?NUMBER ?MONTH)) (lessThanOrEqualTo ?NUMBER 31))").
:- load_kif("(=> (and (instance ?DAY1 (DayFn ?NUMBER1 ?MONTH)) (instance ?DAY2 (DayFn ?NUMBER2 ?MONTH)) (equal (SubtractionFn ?NUMBER2 ?NUMBER1) 1)) (meetsTemporally ?DAY1 ?DAY2))").
:- load_kif("(instance HourFn TemporalRelation)").
:- load_kif("(instance HourFn BinaryFunction)").
:- load_kif("(domain HourFn 1 NonnegativeInteger)").
:- load_kif("(domainSubclass HourFn 2 Day)").
:- load_kif("(rangeSubclass HourFn Hour)").
:- load_kif("(documentation HourFn EnglishLanguage \"A &%BinaryFunction that assigns a &%PositiveRealNumber and a subclass of &%Days to the &%Hours within each &%Day corresponding to that &%NonnegativeInteger. For example, (&%HourFn 12 &%Thursday) is the &%Class of all instances of noon Thursday. For another example, (&%HourFn 0 &%Day) would return the class of all instances of midnight. For still another example, (&%HourFn 14"). (&%DayFn 18 (&%MonthFn &%August (&%YearFn 1912)))) denotes 2 PM on the 18th day of August 1912.\")").
:- load_kif("(=> (instance ?HOUR (HourFn ?NUMBER ?DAY)) (lessThan ?NUMBER 24))").
:- load_kif("(=> (and (instance ?HOUR1 (HourFn ?NUMBER1 ?DAY)) (instance ?HOUR2 (HourFn ?NUMBER2 ?DAY)) (equal (SubtractionFn ?NUMBER2 ?NUMBER1) 1)) (meetsTemporally ?HOUR1 ?HOUR2))").
:- load_kif("(instance MinuteFn TemporalRelation)").
:- load_kif("(instance MinuteFn BinaryFunction)").
:- load_kif("(domain MinuteFn 1 NonnegativeInteger)").
:- load_kif("(domainSubclass MinuteFn 2 Hour)").
:- load_kif("(rangeSubclass MinuteFn Minute)").
:- load_kif("(documentation MinuteFn EnglishLanguage \"A &%BinaryFunction that assigns a &%PositiveRealNumber and a subclass of &%Hours to the &%Minutes within each &%Hour corresponding to that &%NonnegativeInteger. For example, (&%MinuteFn 30 (&%HourFn 17 &%Day)) is the &%Class of all 5:30's in the afternoon. For another example, (&%MinuteFn 15 &%Hour) would return the class of all instances of quarter past the hour. For still another example,"). (&%MinuteFn 15 (&%HourFn 14 (&%DayFn 18 (&%MonthFn &%August (&%YearFn 1912))))) denotes 15 minutes after 2 PM on the 18th day of August 1912.\")").
:- load_kif("(=> (instance ?MINUTE (MinuteFn ?NUMBER ?HOUR)) (lessThan ?NUMBER 60))").
:- load_kif("(=> (and (instance ?MINUTE1 (MinuteFn ?NUMBER1 ?HOUR)) (instance ?MINUTE2 (MinuteFn ?NUMBER2 ?HOUR)) (equal (SubtractionFn ?NUMBER2 ?NUMBER1) 1)) (meetsTemporally ?MINUTE1 ?MINUTE2))").
:- load_kif("(instance SecondFn TemporalRelation)").
:- load_kif("(instance SecondFn BinaryFunction)").
:- load_kif("(domain SecondFn 1 PositiveRealNumber)").
:- load_kif("(domainSubclass SecondFn 2 Minute)").
:- load_kif("(rangeSubclass SecondFn Second)").
:- load_kif("(documentation SecondFn EnglishLanguage \"A &%BinaryFunction that assigns a &%PositiveRealNumber and a subclass of &%Minutes to the &%Seconds within each &%Minute corresponding to that &%PositiveRealNumber. For example, (&%SecondFn 4 (&%MinuteFn 5 &%Hour)) is the &%Class of all fourth &%Seconds of every fifth &%Minute of every hour. For another example,"). (&%SecondFn 8 &%Minute) would return the eighth second of every minute. For still another example, (&%SecondFn 9 (&%MinuteFn 15 (&%HourFn 14 (&%DayFn 18 (&%MonthFn &%August (&%YearFn 1912)))))) denotes 9 seconds and 15 minutes after 2 PM on the 18th day of August 1912.\")").
:- load_kif("(=> (instance ?SECOND (SecondFn ?NUMBER ?MINUTE)) (lessThan ?NUMBER 60))").
:- load_kif("(=> (and (instance ?SECOND1 (SecondFn ?NUMBER1 ?MINUTE)) (instance ?SECOND2 (SecondFn ?NUMBER2 ?MINUTE)) (equal (SubtractionFn ?NUMBER2 ?NUMBER1) 1)) (meetsTemporally ?SECOND1 ?SECOND2))").
:- load_kif("(subclass Year TimeInterval)").
:- load_kif("(relatedInternalConcept Year YearFn)").
:- load_kif("(relatedInternalConcept Year YearDuration)").
:- load_kif("(documentation Year EnglishLanguage \"The &%Class of all calendar &%Years.\")").
:- load_kif("(=> (instance ?YEAR Year) (duration ?YEAR (MeasureFn 1 YearDuration)))").
:- load_kif("(=> (and (instance ?YEAR1 Year) (instance ?YEAR2 Year) (equal (SubtractionFn ?YEAR2 ?YEAR1) 1)) (meetsTemporally ?YEAR1 ?YEAR2))").
:- load_kif("(subclass LeapYear Year)").
:- load_kif("(documentation LeapYear EnglishLanguage \"The &%Class of all leap years. These are years which are either (i.) evenly divisible by 4 and not by 100 or (ii.) evenly divisible by 400 (this latter case is known as a leap century).\")").
:- load_kif("(=> (and (instance ?LEAP LeapYear) (instance ?LEAP (YearFn ?NUMBER))) (or (and (equal (RemainderFn ?NUMBER 4) 0) (not (equal (RemainderFn ?NUMBER 100) 0))) (equal (RemainderFn ?NUMBER 400) 0)))").
:- load_kif("(subclass Month TimeInterval)").
:- load_kif("(relatedInternalConcept Month MonthFn)").
:- load_kif("(documentation Month EnglishLanguage \"The &%Class of all calendar &%Months.\")").
:- load_kif("(subclass January Month)").
:- load_kif("(documentation January EnglishLanguage \"The &%Class of all &%Months which are January.\")").
:- load_kif("(=> (instance ?MONTH January) (duration ?MONTH (MeasureFn 31 DayDuration)))").
:- load_kif("(=> (and (instance ?MONTH1 (MonthFn January ?YEAR)) (instance ?MONTH2 (MonthFn February ?YEAR))) (meetsTemporally ?MONTH1 ?MONTH2))").
:- load_kif("(subclass February Month)").
:- load_kif("(documentation February EnglishLanguage \"The &%Class of all &%Months which are February.\")").
:- load_kif("(=> (and (instance ?MONTH (MonthFn February ?YEAR)) (instance ?Y ?YEAR) (not (instance ?Y LeapYear))) (duration ?MONTH (MeasureFn 28 DayDuration)))").
:- load_kif("(=> (and (instance ?MONTH (MonthFn February ?YEAR)) (instance ?Y ?YEAR) (instance ?Y LeapYear)) (duration ?MONTH (MeasureFn 29 DayDuration)))").
:- load_kif("(=> (and (instance ?MONTH1 (MonthFn February ?YEAR)) (instance ?MONTH2 (MonthFn March ?YEAR))) (meetsTemporally ?MONTH1 ?MONTH2))").
:- load_kif("(subclass March Month)").
:- load_kif("(documentation March EnglishLanguage \"The &%Class of all &%Months which are March.\")").
:- load_kif("(=> (instance ?MONTH March) (duration ?MONTH (MeasureFn 31 DayDuration)))").
:- load_kif("(=> (and (instance ?MONTH1 (MonthFn March ?YEAR)) (instance ?MONTH2 (MonthFn April ?YEAR))) (meetsTemporally ?MONTH1 ?MONTH2))").
:- load_kif("(subclass April Month)").
:- load_kif("(documentation April EnglishLanguage \"The &%Class of all &%Months which are April.\")").
:- load_kif("(=> (instance ?MONTH April) (duration ?MONTH (MeasureFn 30 DayDuration)))").
:- load_kif("(=> (and (equal ?MONTH1 (MonthFn April ?YEAR)) (equal ?MONTH2 (MonthFn May ?YEAR))) (meetsTemporally ?MONTH1 ?MONTH2))").
:- load_kif("(subclass May Month)").
:- load_kif("(documentation May EnglishLanguage \"The &%Class of all &%Months which are May.\")").
:- load_kif("(=> (instance ?MONTH May) (duration ?MONTH (MeasureFn 31 DayDuration)))").
:- load_kif("(=> (and (instance ?MONTH1 (MonthFn May ?YEAR)) (instance ?MONTH2 (MonthFn June ?YEAR))) (meetsTemporally ?MONTH1 ?MONTH2))").
:- load_kif("(subclass June Month)").
:- load_kif("(documentation June EnglishLanguage \"The &%Class of all &%Months which are June.\")").
:- load_kif("(=> (instance ?MONTH June) (duration ?MONTH (MeasureFn 30 DayDuration)))").
:- load_kif("(=> (and (instance ?MONTH1 (MonthFn June ?YEAR)) (instance ?MONTH2 (MonthFn July ?YEAR))) (meetsTemporally ?MONTH1 ?MONTH2))").
:- load_kif("(subclass July Month)").
:- load_kif("(documentation July EnglishLanguage \"The &%Class of all &%Months which are July.\")").
:- load_kif("(=> (instance ?MONTH July) (duration ?MONTH (MeasureFn 31 DayDuration)))").
:- load_kif("(=> (and (instance ?MONTH1 (MonthFn July ?YEAR)) (instance ?MONTH2 (MonthFn August ?YEAR))) (meetsTemporally ?MONTH1 ?MONTH2))").
:- load_kif("(subclass August Month)").
:- load_kif("(documentation August EnglishLanguage \"The &%Class of all &%Months which are August.\")").
:- load_kif("(=> (instance ?MONTH August) (duration ?MONTH (MeasureFn 31 DayDuration)))").
:- load_kif("(=> (and (instance ?MONTH1 (MonthFn August ?YEAR)) (instance ?MONTH2 (MonthFn September ?YEAR))) (meetsTemporally ?MONTH1 ?MONTH2))").
:- load_kif("(subclass September Month)").
:- load_kif("(documentation September EnglishLanguage \"The &%Class of all &%Months which are September.\")").
:- load_kif("(=> (instance ?MONTH September) (duration ?MONTH (MeasureFn 30 DayDuration)))").
:- load_kif("(=> (and (instance ?MONTH1 (MonthFn September ?YEAR)) (instance ?MONTH2 (MonthFn October ?YEAR))) (meetsTemporally ?MONTH1 ?MONTH2))").
:- load_kif("(subclass October Month)").
:- load_kif("(documentation October EnglishLanguage \"The &%Class of all &%Months which are October.\")").
:- load_kif("(=> (instance ?MONTH October) (duration ?MONTH (MeasureFn 31 DayDuration)))").
:- load_kif("(=> (and (instance ?MONTH1 (MonthFn October ?YEAR)) (instance ?MONTH2 (MonthFn November ?YEAR))) (meetsTemporally ?MONTH1 ?MONTH2))").
:- load_kif("(subclass November Month)").
:- load_kif("(documentation November EnglishLanguage \"The &%Class of all &%Months which are November.\")").
:- load_kif("(=> (instance ?MONTH November) (duration ?MONTH (MeasureFn 30 DayDuration)))").
:- load_kif("(=> (and (instance ?MONTH1 (MonthFn November ?YEAR)) (instance ?MONTH2 (MonthFn December ?YEAR))) (meetsTemporally ?MONTH1 ?MONTH2))").
:- load_kif("(subclass December Month)").
:- load_kif("(documentation December EnglishLanguage \"The &%Class of all &%Months which are December.\")").
:- load_kif("(=> (instance ?MONTH December) (duration ?MONTH (MeasureFn 31 DayDuration)))").
:- load_kif("(=> (and (instance ?MONTH1 (MonthFn December ?YEAR1)) (instance ?MONTH2 (MonthFn January ?YEAR2)) (instance ?Y1 ?YEAR1) (instance ?Y2 ?YEAR2) (meetsTemporally ?Y1 ?Y2)) (meetsTemporally ?MONTH1 ?MONTH2))").
:- load_kif("(subclass Day TimeInterval)").
:- load_kif("(relatedInternalConcept Day DayFn)").
:- load_kif("(relatedInternalConcept Day DayDuration)").
:- load_kif("(documentation Day EnglishLanguage \"The &%Class of all calendar &%Days.\")").
:- load_kif("(=> (instance ?DAY Day) (duration ?DAY (MeasureFn 1 DayDuration)))").
:- load_kif("(subclass Monday Day)").
:- load_kif("(documentation Monday EnglishLanguage \"The &%Class of all calendar Mondays.\")").
:- load_kif("(subclass Tuesday Day)").
:- load_kif("(documentation Tuesday EnglishLanguage \"The &%Class of all calendar Tuesdays.\")").
:- load_kif("(=> (and (instance ?DAY1 Monday) (instance ?DAY2 Tuesday) (instance ?WEEK Week) (temporalPart ?DAY1 ?WEEK) (temporalPart ?DAY2 ?WEEK)) (meetsTemporally ?DAY1 ?DAY2))").
:- load_kif("(subclass Wednesday Day)").
:- load_kif("(documentation Wednesday EnglishLanguage \"The &%Class of all calendar Wednesdays.\")").
:- load_kif("(=> (and (instance ?DAY1 Tuesday) (instance ?DAY2 Wednesday) (instance ?WEEK Week) (temporalPart ?DAY1 ?WEEK) (temporalPart ?DAY2 ?WEEK)) (meetsTemporally ?DAY1 ?DAY2))").
:- load_kif("(subclass Thursday Day)").
:- load_kif("(documentation Thursday EnglishLanguage \"The &%Class of all calendar Thursdays.\")").
:- load_kif("(=> (and (instance ?DAY1 Wednesday) (instance ?DAY2 Thursday) (instance ?WEEK Week) (temporalPart ?DAY1 ?WEEK) (temporalPart ?DAY2 ?WEEK)) (meetsTemporally ?DAY1 ?DAY2))").
:- load_kif("(subclass Friday Day)").
:- load_kif("(documentation Friday EnglishLanguage \"The &%Class of all calendar Fridays.\")").
:- load_kif("(=> (and (instance ?DAY1 Thursday) (instance ?DAY2 Friday) (instance ?WEEK Week) (temporalPart ?DAY1 ?WEEK) (temporalPart ?DAY2 ?WEEK)) (meetsTemporally ?DAY1 ?DAY2))").
:- load_kif("(subclass Saturday Day)").
:- load_kif("(documentation Saturday EnglishLanguage \"The &%Class of all calendar Saturdays.\")").
:- load_kif("(=> (and (instance ?DAY1 Friday) (instance ?DAY2 Saturday) (instance ?WEEK Week) (temporalPart ?DAY1 ?WEEK) (temporalPart ?DAY2 ?WEEK)) (meetsTemporally ?DAY1 ?DAY2))").
:- load_kif("(subclass Sunday Day)").
:- load_kif("(documentation Sunday EnglishLanguage \"The &%Class of all calendar Sundays.\")").
:- load_kif("(=> (and (instance ?DAY1 Saturday) (instance ?DAY2 Sunday) (instance ?WEEK Week) (temporalPart ?DAY1 ?WEEK) (temporalPart ?DAY2 ?WEEK)) (meetsTemporally ?DAY1 ?DAY2))").
:- load_kif("(=> (and (instance ?DAY1 Sunday) (instance ?DAY2 Monday) (instance ?WEEK1 Week) (instance ?WEEK2 Week) (temporalPart ?DAY1 ?WEEK1) (temporalPart ?DAY2 ?WEEK2) (meetsTemporally ?WEEK1 ?WEEK2)) (meetsTemporally ?DAY1 ?DAY2))").
:- load_kif("(subclass Week TimeInterval)").
:- load_kif("(documentation Week EnglishLanguage \"The &%Class of all calendar weeks.\")").
:- load_kif("(=> (instance ?WEEK Week) (duration ?WEEK (MeasureFn 1 WeekDuration)))").
:- load_kif("(subclass Hour TimeInterval)").
:- load_kif("(relatedInternalConcept Hour HourFn)").
:- load_kif("(relatedInternalConcept Hour HourDuration)").
:- load_kif("(documentation Hour EnglishLanguage \"The &%Class of all clock &%Hours.\")").
:- load_kif("(=> (instance ?HOUR Hour) (duration ?HOUR (MeasureFn 1 HourDuration)))").
:- load_kif("(subclass Minute TimeInterval)").
:- load_kif("(relatedInternalConcept Minute MinuteFn)").
:- load_kif("(relatedInternalConcept Minute MinuteDuration)").
:- load_kif("(documentation Minute EnglishLanguage \"The &%Class of all clock &%Minutes.\")").
:- load_kif("(=> (instance ?MINUTE Minute) (duration ?MINUTE (MeasureFn 1 MinuteDuration)))").
:- load_kif("(subclass Second TimeInterval)").
:- load_kif("(relatedInternalConcept Second SecondDuration)").
:- load_kif("(relatedInternalConcept Second SecondFn)").
:- load_kif("(documentation Second EnglishLanguage \"The &%Class of all clock &%Seconds.\")").
:- load_kif("(=> (instance ?SECOND Second) (duration ?SECOND (MeasureFn 1 SecondDuration)))").
:- load_kif("(instance TemporalCompositionFn TemporalRelation)").
:- load_kif("(instance TemporalCompositionFn BinaryFunction)").
:- load_kif("(domain TemporalCompositionFn 1 TimeInterval)").
:- load_kif("(domainSubclass TemporalCompositionFn 2 TimeInterval)").
:- load_kif("(rangeSubclass TemporalCompositionFn TimeInterval)").
:- load_kif("(documentation TemporalCompositionFn EnglishLanguage \"The basic &%Function for expressing the composition of larger &%TimeIntervals out of smaller &%TimeIntervals. For example, if &%ThisSeptember is an &%instance of &%September,"). (&%TemporalCompositionFn &%ThisSeptember &%Day) denotes the &%Class of consecutive days that make up &%ThisSeptember. Note that one can obtain the number of instances of this &%Class by using the function &%CardinalityFn.\")").
:- load_kif("(=> (equal (TemporalCompositionFn ?INTERVAL ?INTERVAL-TYPE) ?CLASS) (forall (?TIME1 ?TIME2) (=> (and (instance ?TIME1 ?INTERVAL-TYPE) (instance ?TIME2 ?CLASS)) (exists (?DURATION) (and (duration ?TIME1 ?DURATION) (duration ?TIME2 ?DURATION))))))").
:- load_kif("(=> (equal (TemporalCompositionFn ?INTERVAL ?INTERVAL-TYPE) ?CLASS) (forall (?TIME1 ?TIME2) (=> (and (instance ?TIME1 ?CLASS) (instance ?TIME2 ?CLASS) (not (equal ?TIME1 ?TIME2))) (or (meetsTemporally ?TIME1 ?TIME2) (meetsTemporally ?TIME2 ?TIME1) (earlier ?TIME1 ?TIME2) (earlier ?TIME2 ?TIME1)))))").
:- load_kif("(=> (equal (TemporalCompositionFn ?INTERVAL ?INTERVAL-TYPE) ?CLASS) (exists (?TIME) (and (instance ?TIME ?CLASS) (starts ?TIME ?INTERVAL))))").
:- load_kif("(=> (equal (TemporalCompositionFn ?INTERVAL ?INTERVAL-TYPE) ?CLASS) (exists (?TIME) (and (instance ?TIME ?CLASS) (finishes ?TIME ?INTERVAL))))").
:- load_kif("(=> (equal (TemporalCompositionFn ?INTERVAL ?INTERVAL-TYPE) ?CLASS) (forall (?TIME1) (=> (and (instance ?TIME1 ?CLASS) (not (finishes ?TIME1 ?INTERVAL))) (exists (?TIME2) (and (instance ?TIME2 ?CLASS) (meetsTemporally ?TIME1 ?TIME2))))))").
:- load_kif("(=> (equal (TemporalCompositionFn ?INTERVAL ?INTERVAL-TYPE) ?CLASS) (forall (?TIME1) (=> (and (instance ?TIME1 ?CLASS) (not (starts ?TIME1 ?INTERVAL))) (exists (?TIME2) (and (instance ?TIME2 ?CLASS) (meetsTemporally ?TIME2 ?TIME1))))))").
:- load_kif("(=> (equal (TemporalCompositionFn ?INTERVAL ?INTERVAL-TYPE) ?CLASS) (forall (?TIME) (=> (and (instance ?TIME TimePoint) (temporalPart ?TIME ?INTERVAL)) (exists (?INSTANCE) (and (instance ?INSTANCE ?CLASS) (temporalPart ?TIME ?INSTANCE))))))").
:- load_kif("(=> (instance ?YEAR Year) (equal (CardinalityFn (TemporalCompositionFn ?YEAR Month)) 12))").
:- load_kif("(=> (and (instance ?MONTH Month) (duration ?MONTH (MeasureFn ?NUMBER DayDuration))) (equal (CardinalityFn (TemporalCompositionFn ?MONTH Day)) ?NUMBER))").
:- load_kif("(=> (instance ?WEEK Week) (equal (CardinalityFn (TemporalCompositionFn ?WEEK Day)) 7))").
:- load_kif("(=> (instance ?DAY Day) (equal (CardinalityFn (TemporalCompositionFn ?DAY Hour)) 24))").
:- load_kif("(=> (instance ?HOUR Hour) (equal (CardinalityFn (TemporalCompositionFn ?HOUR Minute)) 60))").
:- load_kif("(=> (instance ?MINUTE Minute) (equal (CardinalityFn (TemporalCompositionFn ?MINUTE Second)) 60))").
% :- load_kif("; END FILE").
% :- load_kif("; BEGIN FILE").
% :- load_kif(";  MEREOTOPOLOGY  ").
% :- load_kif("; INCLUDES 'STRUCTURAL ONTOLOGY' INCLUDES 'BASE ONTOLOGY'").
% :- load_kif("; Most of this content is taken from Barry Smith's and Nicola Guarino's papers on the subject.").
:- load_kif("(instance connected BinaryPredicate)").
:- load_kif("(instance connected SpatialRelation)").
:- load_kif("(instance connected ReflexiveRelation)").
:- load_kif("(instance connected SymmetricRelation)").
:- load_kif("(domain connected 1 Object)").
:- load_kif("(domain connected 2 Object)").
:- load_kif("(documentation connected EnglishLanguage \"(connected ?OBJ1 ?OBJ2) means that ?OBJ1 &%meetsSpatially ?OBJ2 or that ?OBJ1 &%overlapsSpatially ?OBJ2.\")").
:- load_kif("(=> (connected ?OBJ1 ?OBJ2) (or (meetsSpatially ?OBJ1 ?OBJ2) (overlapsSpatially ?OBJ1 ?OBJ2)))").
:- load_kif("(<=> (instance ?OBJ SelfConnectedObject) (forall (?PART1 ?PART2) (=> (equal ?OBJ (MereologicalSumFn ?PART1 ?PART2)) (connected ?PART1 ?PART2))))").
:- load_kif("(instance connects SpatialRelation)").
:- load_kif("(instance connects TernaryPredicate)").
:- load_kif("(domain connects 1 SelfConnectedObject)").
:- load_kif("(domain connects 2 SelfConnectedObject)").
:- load_kif("(domain connects 3 SelfConnectedObject)").
:- load_kif("(documentation connects EnglishLanguage \"The relationship between three things, when one of the three things connects the other two. More formally, (&%connects ?OBJ1 ?OBJ2 ?OBJ3) means that (&%connected ?OBJ1 ?OBJ2) and (&%connected ?OBJ1 ?OBJ3) and not (&%connected ?OBJ2 ?OBJ3).\")").
:- load_kif("(<=> (connects ?OBJ1 ?OBJ2 ?OBJ3) (between ?OBJ2 ?OBJ1 ?OBJ3))").
:- load_kif("(<=> (connects ?OBJ1 ?OBJ2 ?OBJ3) (and (connected ?OBJ1 ?OBJ2) (connected ?OBJ1 ?OBJ3) (not (connected ?OBJ2 ?OBJ3))))").
:- load_kif("(=> (connects ?ARC ?NODE1 ?NODE2) (connects ?ARC ?NODE2 ?NODE1))").
:- load_kif("(subrelation meetsSpatially connected)").
:- load_kif("(instance meetsSpatially IrreflexiveRelation)").
:- load_kif("(instance meetsSpatially SymmetricRelation)").
:- load_kif("(disjointRelation meetsSpatially overlapsSpatially)").
:- load_kif("(documentation meetsSpatially EnglishLanguage \"(&%meetsSpatially ?OBJ1 ?OBJ2) means that ?OBJ1 and ?OBJ2 are &%connected but that neither ?OBJ1 nor ?OBJ2 &%overlapsSpatially the other.\")").
:- load_kif("(subrelation overlapsSpatially connected)").
:- load_kif("(instance overlapsSpatially ReflexiveRelation)").
:- load_kif("(instance overlapsSpatially SymmetricRelation)").
:- load_kif("(documentation overlapsSpatially EnglishLanguage \"(&%overlapsSpatially ?OBJ1 ?OBJ2) means that the &%Objects ?OBJ1 and ?OBJ2 have some parts in common. This is a reflexive and symmetric (but not transitive) relation.\")").
:- load_kif("(<=> (overlapsSpatially ?OBJ1 ?OBJ2) (exists (?OBJ3) (and (part ?OBJ3 ?OBJ1) (part ?OBJ3 ?OBJ2))))").
:- load_kif("(=> (and (member ?OBJ1 ?COLL) (member ?OBJ2 ?COLL) (not (equal ?OBJ1 ?OBJ2))) (not (overlapsSpatially ?OBJ1 ?OBJ2)))").
:- load_kif("(=> (and (instance ?REL CaseRole) (instance ?OBJ Object) (?REL ?PROCESS ?OBJ)) (exists (?TIME) (overlapsSpatially (WhereFn ?PROCESS ?TIME) ?OBJ)))").
:- load_kif("(instance overlapsPartially SymmetricRelation)").
:- load_kif("(instance overlapsPartially IrreflexiveRelation)").
:- load_kif("(subrelation overlapsPartially overlapsSpatially)").
:- load_kif("(documentation overlapsPartially EnglishLanguage \"(&%overlapsPartially ?OBJ1 ?OBJ2) means that ?OBJ1 and ?OBJ2 have part(s) in common, but neither ?OBJ1 nor ?OBJ2 is a &%part of the other.\")").
:- load_kif("(<=> (overlapsPartially ?OBJ1 ?OBJ2) (and (not (part ?OBJ1 ?OBJ2)) (not (part ?OBJ2 ?OBJ1)) (exists (?OBJ3) (and (part ?OBJ3 ?OBJ1) (part ?OBJ3 ?OBJ2)))))").
:- load_kif("(subrelation superficialPart part)").
:- load_kif("(instance superficialPart IrreflexiveRelation)").
:- load_kif("(instance superficialPart TransitiveRelation)").
:- load_kif("(documentation superficialPart EnglishLanguage \"(&%superficialPart ?OBJ1 ?OBJ2) means that ?OBJ1 is a part of ?OBJ2 that has no interior parts of its own").
:- load_kif("(or, intuitively, that only overlaps those parts of ?OBJ2 that are externally connected with the mereological complement of ?OBJ2). This too is a transitive relation closed under &%MereologicalSumFn and &%MereologicalProductFn.\")").
:- load_kif("(=> (superficialPart ?OBJ1 ?OBJ2) (and (not (interiorPart ?OBJ1 ?OBJ2)) (not (exists (?OBJ3) (interiorPart ?OBJ3 ?OBJ1)))))").
:- load_kif("(instance surface AsymmetricRelation)").
:- load_kif("(subrelation surface superficialPart)").
:- load_kif("(domain surface 1 SelfConnectedObject)").
:- load_kif("(domain surface 2 SelfConnectedObject)").
:- load_kif("(documentation surface EnglishLanguage \"(&%surface ?OBJ1 ?OBJ2) means that ?OBJ1 is a maximally connected &%superficialPart of ?OBJ2. Note that some &%SelfConnectedObjects have more than one surface, e.g. a hollow object like a tennis ball has both an inner and an outer surface.\")").
:- load_kif("(=> (surface ?OBJ1 ?OBJ2) (forall (?OBJ3) (=> (superficialPart ?OBJ3 ?OBJ2) (part ?OBJ3 ?OBJ1))))").
:- load_kif("(subrelation interiorPart part)").
:- load_kif("(instance interiorPart AsymmetricRelation)").
:- load_kif("(instance interiorPart TransitiveRelation)").
:- load_kif("(documentation interiorPart EnglishLanguage \"(&%interiorPart ?OBJ1 ?OBJ2) means that ?OBJ1 is part ?OBJ2 and there is no overlap between ?OBJ1 and any &%superficialPart ?OBJ2.\")").
:- load_kif("(=> (interiorPart ?OBJ1 ?OBJ2) (forall (?PART) (=> (superficialPart ?PART ?OBJ2) (not (overlapsSpatially ?OBJ1 ?PART)))))").
:- load_kif("(subrelation bottom superficialPart)").
:- load_kif("(domain bottom 1 SelfConnectedObject)").
:- load_kif("(domain bottom 2 SelfConnectedObject)").
:- load_kif("(documentation bottom EnglishLanguage \"(&%bottom ?BOTTOM ?OBJECT) holds if ?BOTTOM is the lowest or deepest maximal superficial part of ?OBJECT.\")").
:- load_kif("(=> (and (bottom ?BOTTOM ?OBJECT) (part ?PART ?OBJECT) (not (connected ?PART ?BOTTOM))) (orientation ?PART ?BOTTOM Above))").
:- load_kif("(subrelation top superficialPart)").
:- load_kif("(domain top 1 SelfConnectedObject)").
:- load_kif("(domain top 2 SelfConnectedObject)").
:- load_kif("(documentation top EnglishLanguage \"(&%top ?TOP ?OBJECT) means that ?TOP is the highest maximal superficial part of ?OBJECT.\")").
:- load_kif("(=> (and (top ?TOP ?OBJECT) (part ?PART ?OBJECT) (not (connected ?PART ?TOP))) (orientation ?PART ?TOP Below))").
:- load_kif("(subrelation side superficialPart)").
:- load_kif("(domain side 1 SelfConnectedObject)").
:- load_kif("(domain side 2 SelfConnectedObject)").
:- load_kif("(documentation side EnglishLanguage \"(&%side ?SIDE ?OBJECT) means that ?SIDE is a side of the object, as opposed to the &%top or &%bottom.\")").
:- load_kif("(=> (and (side ?SIDE ?OBJECT) (part ?PART ?OBJECT) (not (connected ?PART ?SIDE))) (exists (?DIRECT) (orientation ?SIDE ?PART ?DIRECT)))").
:- load_kif("(=> (and (top ?TOP ?O) (side ?S ?O)) (not (equal ?TOP ?S)))").
:- load_kif("(<=> (width ?OBJECT ?WIDTH) (exists (?SIDE1 ?SIDE2) (and (side ?SIDE1 ?OBJECT) (side ?SIDE2 ?OBJECT) (distance ?SIDE1 ?SIDE2 ?WIDTH))))").
:- load_kif("(=> (and (height ?OBJECT ?HEIGHT) (top ?TOP ?OBJECT) (bottom ?BOTTOM ?OBJECT)) (distance ?TOP ?BOTTOM ?HEIGHT))").
:- load_kif("(instance MereologicalSumFn SpatialRelation)").
:- load_kif("(instance MereologicalSumFn BinaryFunction)").
:- load_kif("(instance MereologicalSumFn TotalValuedRelation)").
:- load_kif("(domain MereologicalSumFn 1 Object)").
:- load_kif("(domain MereologicalSumFn 2 Object)").
:- load_kif("(range MereologicalSumFn Object)").
:- load_kif("(relatedInternalConcept MereologicalSumFn MereologicalProductFn)").
:- load_kif("(relatedInternalConcept MereologicalSumFn MereologicalDifferenceFn)").
:- load_kif("(documentation MereologicalSumFn EnglishLanguage \"(&%MereologicalSumFn ?OBJ1 ?OBJ2) denotes the &%Object consisting of the parts which belong to either ?OBJ1 or ?OBJ2.\")").
:- load_kif("(=> (equal ?OBJ3 (MereologicalSumFn ?OBJ1 ?OBJ2)) (forall (?PART) (<=> (part ?PART ?OBJ3) (or (part ?PART ?OBJ1) (part ?PART ?OBJ2)))))").
:- load_kif("(instance MereologicalProductFn SpatialRelation)").
:- load_kif("(instance MereologicalProductFn BinaryFunction)").
:- load_kif("(instance MereologicalProductFn TotalValuedRelation)").
:- load_kif("(domain MereologicalProductFn 1 Object)").
:- load_kif("(domain MereologicalProductFn 2 Object)").
:- load_kif("(range MereologicalProductFn Object)").
:- load_kif("(relatedInternalConcept MereologicalProductFn MereologicalDifferenceFn)").
:- load_kif("(documentation MereologicalProductFn EnglishLanguage \"(&%MereologicalProductFn ?OBJ1 ?OBJ2) denotes the &%Object consisting of the parts which belong to both ?OBJ1 and ?OBJ2.\")").
:- load_kif("(=> (equal ?OBJ3 (MereologicalProductFn ?OBJ1 ?OBJ2)) (forall (?PART) (<=> (part ?PART ?OBJ3) (and (part ?PART ?OBJ1) (part ?PART ?OBJ2)))))").
:- load_kif("(instance MereologicalDifferenceFn SpatialRelation)").
:- load_kif("(instance MereologicalDifferenceFn BinaryFunction)").
:- load_kif("(instance MereologicalDifferenceFn TotalValuedRelation)").
:- load_kif("(domain MereologicalDifferenceFn 1 Object)").
:- load_kif("(domain MereologicalDifferenceFn 2 Object)").
:- load_kif("(range MereologicalDifferenceFn Object)").
:- load_kif("(documentation MereologicalDifferenceFn EnglishLanguage \"(&%MereologicalDifferenceFn ?OBJ1 ?OBJ2) denotes the &%Object consisting of the parts which belong to ?OBJ1 and not to ?OBJ2.\")").
:- load_kif("(=> (equal ?OBJ3 (MereologicalDifferenceFn ?OBJ1 ?OBJ2)) (forall (?PART) (<=> (properPart ?PART ?OBJ3) (and (properPart ?PART ?OBJ1) (not (properPart ?PART ?OBJ2))))))").
% :- load_kif("; What follows is an alignment of the Casati and Varzi theory of holes with the SUMO.").
:- load_kif("(instance hole BinaryPredicate)").
:- load_kif("(instance hole SpatialRelation)").
:- load_kif("(instance hole AsymmetricRelation)").
:- load_kif("(domain hole 1 Hole)").
:- load_kif("(domain hole 2 SelfConnectedObject)").
:- load_kif("(documentation hole EnglishLanguage \"(&%hole ?HOLE ?OBJ) means that ?HOLE is a &%Hole in ?OBJ. A &%Hole is a fillable body located at the &%surface an &%Object.\")").
:- load_kif("(subclass Hole Region)").
:- load_kif("(documentation Hole EnglishLanguage \"A hole is an immaterial body located at the surface of an &%Object. Since every &%Hole is ontologically dependent on its host").
:- load_kif("(i.e., the object in which it is a hole), being a &%Hole is defined as being a &%hole in something. Note that two &%Holes may occupy the same region, or part of the same region, without sharing any parts. Any two hosts of a hole have a common proper part that entirely hosts the hole. A common host of two holes hosts all parts of the sum of those holes. Any object that includes the host of a hole is a host of that hole, unless its parts also include parts of that very hole. Overlapping holes have overlapping hosts. No hole is atomic. Holes are connected with their hosts. No hole can have a proper part that is externally connected with exactly the same things as the hole itself.\")").
:- load_kif("(<=> (instance ?HOLE Hole) (exists (?OBJ) (hole ?HOLE ?OBJ)))").
:- load_kif("(=> (hole ?HOLE ?OBJ) (not (instance ?OBJ Hole)))").
:- load_kif("(=> (hole ?HOLE ?OBJ) (not (overlapsSpatially ?HOLE ?OBJ)))").
:- load_kif("(=> (and (hole ?HOLE ?OBJ1) (hole ?HOLE ?OBJ2)) (exists (?OBJ3) (and (properPart ?OBJ3 (MereologicalProductFn ?OBJ1 ?OBJ2)) (hole ?HOLE ?OBJ3))))").
:- load_kif("(=> (and (hole ?HOLE1 ?OBJ) (hole ?HOLE2 ?OBJ)) (forall (?HOLE3) (=> (part ?HOLE3 (MereologicalSumFn ?HOLE1 ?HOLE2)) (hole ?HOLE3 ?OBJ))))").
:- load_kif("(=> (and (hole ?HOLE ?OBJ1) (part ?OBJ1 ?OBJ2)) (or (overlapsSpatially ?HOLE ?OBJ2) (hole ?HOLE ?OBJ2)))").
:- load_kif("(=> (and (hole ?HOLE1 ?OBJ1) (hole ?HOLE2 ?OBJ2) (overlapsSpatially ?HOLE1 ?HOLE2)) (overlapsSpatially ?OBJ1 ?OBJ2))").
:- load_kif("(=> (instance ?HOLE1 Hole) (exists (?HOLE2) (properPart ?HOLE2 ?HOLE1)))").
:- load_kif("(instance HoleHostFn SpatialRelation)").
:- load_kif("(instance HoleHostFn UnaryFunction)").
:- load_kif("(instance HoleHostFn TotalValuedRelation)").
:- load_kif("(instance HoleHostFn AsymmetricRelation)").
:- load_kif("(domain HoleHostFn 1 Hole)").
:- load_kif("(range HoleHostFn Object)").
:- load_kif("(documentation HoleHostFn EnglishLanguage \"A &%UnaryFunction that maps a &%Hole to the &%Object which is its principal host. The principle host of a &%Hole is its maximally connected host (a notion taken here to be defined only when the argument is a hole).\")").
:- load_kif("(=> (hole ?HOLE ?OBJ) (connected ?HOLE ?OBJ))").
:- load_kif("(=> (and (instance ?HOLE1 Hole) (properPart ?HOLE2 ?HOLE1)) (exists (?OBJ) (and (meetsSpatially ?HOLE1 ?OBJ) (not (meetsSpatially ?HOLE2 ?OBJ)))))").
:- load_kif("(instance Fillable ShapeAttribute)").
:- load_kif("(documentation Fillable EnglishLanguage \"Something is &%Fillable if it can be filled by something else. Note that 'filled' here means perfectly filled. Something is fillable just in case it is part of a hole, i.e., fillability is an exclusive property of holes and their parts.\")").
:- load_kif("(=> (exists (?TIME) (holdsDuring ?TIME (fills ?OBJ ?HOLE))) (attribute ?HOLE Fillable))").
:- load_kif("(<=> (attribute ?HOLE1 Fillable) (exists (?HOLE2) (and (instance ?HOLE2 Hole) (part ?HOLE1 ?HOLE2))))").
:- load_kif("(subrelation partiallyFills located)").
:- load_kif("(instance partiallyFills SpatialRelation)").
:- load_kif("(instance partiallyFills AsymmetricRelation)").
:- load_kif("(domain partiallyFills 1 Object)").
:- load_kif("(domain partiallyFills 2 Hole)").
:- load_kif("(documentation partiallyFills EnglishLanguage \"(&%partiallyFills ?OBJ ?HOLE) means that ?OBJ &%completelyFills some part of ?HOLE. Note that if (&%partiallyFills ?OBJ1 ?HOLE) and (&%part ?OBJ1 ?OBJ2), then (&%partiallyFills ?OBJ2 ?HOLE). Note too that a partial filler need not be wholly inside a hole (it may stick out), which means that every complete filler also qualifies as").
:- load_kif("(is a limit case of) a partial one.\")").
:- load_kif("(=> (partiallyFills ?OBJ ?HOLE1) (exists (?HOLE2) (and (part ?HOLE2 ?HOLE1) (completelyFills ?OBJ ?HOLE2))))").
:- load_kif("(instance properlyFills AsymmetricRelation)").
:- load_kif("(subrelation properlyFills partiallyFills)").
:- load_kif("(domain properlyFills 1 Object)").
:- load_kif("(domain properlyFills 2 Hole)").
:- load_kif("(documentation properlyFills EnglishLanguage \"(&%properlyFills ?OBJ ?HOLE) means that ?HOLE is properly (though perhaps incompletely) filled by ?OBJ, i.e. some part of ?HOLE is perfectly filled by ?OBJ. Note that &%properlyFills is the dual of &%completelyFills, and is so related to &%partiallyFills that ?OBJ &%properlyFills ?HOLE just in case ?OBJ &%partiallyFills every part of ?HOLE. (Thus, every perfect filler is both complete and proper in this sense). Every hole is connected with everything with which a proper filler of the hole is connected. Every proper part of a perfect filler of (a part of) a hole properly fills (that part of) that hole.\")").
:- load_kif("(=> (properlyFills ?OBJ ?HOLE1) (exists (?HOLE2) (and (part ?HOLE2 ?HOLE1) (fills ?OBJ ?HOLE2))))").
:- load_kif("(instance completelyFills AsymmetricRelation)").
:- load_kif("(subrelation completelyFills partiallyFills)").
:- load_kif("(documentation completelyFills EnglishLanguage \"(&%completelyFills ?OBJ ?HOLE) means that some &%part of the &%Object ?OBJ fills the &%Hole ?HOLE. Note that if (&%completelyFills ?OBJ1 ?HOLE) and (&%part ?OBJ1 ?OBJ2), then (&%completelyFills ?OBJ2 ?HOLE). A complete filler of (a part of) a hole is connected with everything with which (that part of) the hole itself is connected. A perfect filler of (a part of) a hole completely fills every proper part of (that part of) that hole.\")").
:- load_kif("(=> (completelyFills ?OBJ1 ?HOLE) (exists (?OBJ2) (and (part ?OBJ2 ?OBJ1) (fills ?OBJ2 ?HOLE))))").
:- load_kif("(instance fills AsymmetricRelation)").
:- load_kif("(subrelation fills completelyFills)").
:- load_kif("(subrelation fills properlyFills)").
:- load_kif("(domain fills 1 Object)").
:- load_kif("(domain fills 2 Hole)").
:- load_kif("(relatedInternalConcept fills Fillable)").
:- load_kif("(documentation fills EnglishLanguage \"Holes can be filled. (&%fills ?OBJ ?HOLE) means that the &%Object ?OBJ fills the &%Hole ?HOLE. Note that &%fills here means perfectly filled. Perfect fillers and fillable entities have no parts in common (rather, they may occupy the same spatial region).\")").
:- load_kif("(=> (and (fills ?OBJ1 ?HOLE) (attribute ?OBJ2 Fillable)) (not (overlapsSpatially ?OBJ1 ?OBJ2)))").
:- load_kif("(=> (completelyFills ?OBJ1 ?HOLE) (forall (?OBJ2) (=> (connected ?OBJ2 ?HOLE) (connected ?OBJ2 ?OBJ1))))").
:- load_kif("(=> (and (properlyFills ?OBJ1 ?HOLE) (connected ?OBJ2 ?OBJ1)) (connected ?HOLE ?OBJ2))").
:- load_kif("(=> (and (fills ?OBJ ?HOLE1) (properPart ?HOLE2 ?HOLE1)) (completelyFills ?OBJ ?HOLE2))").
:- load_kif("(=> (and (fills ?OBJ1 ?HOLE) (properPart ?OBJ2 ?OBJ1)) (properlyFills ?OBJ2 ?HOLE))").
:- load_kif("(instance HoleSkinFn SpatialRelation)").
:- load_kif("(instance HoleSkinFn UnaryFunction)").
:- load_kif("(instance HoleSkinFn TotalValuedRelation)").
:- load_kif("(instance HoleSkinFn AsymmetricRelation)").
:- load_kif("(domain HoleSkinFn 1 Hole)").
:- load_kif("(range HoleSkinFn Object)").
:- load_kif("(documentation HoleSkinFn EnglishLanguage \"A &%UnaryFunction that maps a &%Hole to the skin of the &%Hole. The skin of a &%Hole is the fusion of those superficial parts (see &%superficialPart) of the &%Hole's principal host (see &%HoleHostFn) with which the &%Hole is externally connected.\")").
:- load_kif("(=> (equal ?OBJ1 (HoleSkinFn ?HOLE)) (forall (?OBJ2) (<=> (overlapsSpatially ?OBJ2 ?OBJ1) (exists (?OBJ3) (and (superficialPart ?OBJ3 (HoleHostFn ?HOLE)) (meetsSpatially ?HOLE ?OBJ3) (overlapsSpatially ?OBJ2 ?OBJ3))))))").
% :- load_kif("; END FILE").
% :- load_kif("; BEGIN FILE").
% :- load_kif(";  PROCESSES  ").
% :- load_kif("; INCLUDES 'TEMPORAL CONCEPTS' INCLUDES 'OBJECTS' INCLUDES 'QUALITIES'").
:- load_kif("(instance subProcess BinaryPredicate)").
:- load_kif("(instance subProcess PartialOrderingRelation)").
:- load_kif("(domain subProcess 1 Process)").
:- load_kif("(domain subProcess 2 Process)").
:- load_kif("(documentation subProcess EnglishLanguage \"(&%subProcess ?SUBPROC ?PROC) means that ?SUBPROC is a subprocess of ?PROC. A subprocess is here understood as a temporally distinguished part (proper or not) of a &%Process.\")").
:- load_kif("(=> (subProcess ?SUBPROC ?PROC) (temporalPart (WhenFn ?SUBPROC) (WhenFn ?PROC)))").
% :- load_kif("; NS: delete. This rule is stated above with different (but equivalent) antecedent order. ").
% :- load_kif("; (=> (subProcess ?SUBPROC ?PROC) (forall (?REGION) (=> (located ?PROC ?REGION) (located ?SUBPROC ?REGION))))").
% :- load_kif("; NS: delete. Redundant ... we've already stated elsewhere that every Physical exists at some time and at some location. ").
% :- load_kif("; (=> (and (instance ?PROC Process) (subProcess ?SUBPROC ?PROC)) (exists (?TIME) (time ?SUBPROC ?TIME)))").
% :- load_kif("; The following formulas cover the hierarchy of &%Classes under &%BiologicalProcess.").
:- load_kif("(subclass BiologicalProcess InternalChange)").
:- load_kif("(documentation BiologicalProcess EnglishLanguage \"A &%Process embodied in an &%Organism.\")").
:- load_kif("(=> (instance ?PROC BiologicalProcess) (exists (?OBJ) (and (instance ?OBJ Organism) (eventLocated ?PROC ?OBJ))))").
:- load_kif("(=> (and (instance ?PROC BiologicalProcess) (experiencer ?PROC ?ORG)) (instance ?ORG Organism))").
:- load_kif("(subclass PhysiologicProcess BiologicalProcess)").
:- load_kif("(documentation PhysiologicProcess EnglishLanguage \"A normal process of an &%Organism or part of an &%Organism.\")").
:- load_kif("(subclass AutonomicProcess PhysiologicProcess)").
:- load_kif("(disjoint AutonomicProcess IntentionalProcess)").
:- load_kif("(documentation AutonomicProcess EnglishLanguage \"The class of &%PhysiologicProcesses of which there is not conscious awareness and control.\")").
:- load_kif("(subclass OrganOrTissueProcess AutonomicProcess)").
:- load_kif("(disjoint OrganOrTissueProcess OrganismProcess)").
:- load_kif("(documentation OrganOrTissueProcess EnglishLanguage \"A &%PhysiologicProcess of a particular &%Organ or &%Tissue.\")").
:- load_kif("(=> (instance ?PROC OrganOrTissueProcess) (exists (?THING) (and (eventLocated ?PROC ?THING) (or (instance ?THING Organ) (instance ?THING Tissue)))))").
:- load_kif("(subclass OrganismProcess PhysiologicProcess)").
:- load_kif("(documentation OrganismProcess EnglishLanguage \"A physiologic function of the &%Organism as a whole, of multiple organ systems or of multiple &%Organs or &%Tissues.\")").
:- load_kif("(subclass Birth OrganismProcess)").
:- load_kif("(documentation Birth EnglishLanguage \"The &%Process of being born.\")").
% :- load_kif("; NS: We need to provide more explanation of the intended meaning of &%Birth.").
:- load_kif("(=> (and (instance ?BIRTH Birth) (experiencer ?BIRTH ?AGENT)) (exists (?DEATH) (and (instance ?DEATH Death) (experiencer ?DEATH ?AGENT))))").
:- load_kif("(subclass Death OrganismProcess)").
:- load_kif("(documentation Death EnglishLanguage \"The &%Process of dying.\")").
% :- load_kif("; NS: delete. Organisms cannot be Dead. (=> (and (instance ?DEATH Death) (experiencer ?DEATH ?AGENT)) (holdsDuring (FutureFn (WhenFn ?DEATH)) (attribute ?AGENT Dead)))").
% :- load_kif("; NS: add. The result of an Organism's Death is a Dead OrganicObject, the parts of which were parts of the Organism immediately before its Death. This rule should be considered true only by default, since it is not really accurate for an organism that dies, e.g., by being instantaneously vaporized.").
:- load_kif("(=> (and (instance ?DEATH Death) (instance ?ORG Organism) (experiencer ?DEATH ?ORG)) (exists (?REM) (and (result ?DEATH ?REM) (instance ?REM OrganicObject) (holdsDuring (FutureFn (WhenFn ?DEATH)) (attribute ?REM Dead)) (=> (holdsDuring (ImmediateFutureFn (WhenFn ?DEATH)) (part ?OBJ ?REM)) (holdsDuring (ImmediatePastFn (WhenFn ?DEATH)) (part ?OBJ ?ORG))))))").
:- load_kif("(subclass Breathing OrganismProcess)").
:- load_kif("(subclass Breathing AutonomicProcess)").
:- load_kif("(documentation Breathing EnglishLanguage \"The &%Process of respiration, by which oxygen is made available to an &%Animal. This covers processes of inhalation, exhalation, and alternations between the two.\")").
:- load_kif("(subclass Ingesting OrganismProcess)").
:- load_kif("(documentation Ingesting EnglishLanguage \"The &%Process by which food is taken into an &%Animal.\")").
:- load_kif("(=> (and (instance ?ACT Ingesting) (resource ?ACT ?FOOD)) (instance ?FOOD (FoodForFn Organism)))").
:- load_kif("(subclass Eating Ingesting)").
:- load_kif("(documentation Eating EnglishLanguage \"The &%Process by which solid food is incorporated into an &%Animal.\")").
:- load_kif("(=> (and (instance ?ACT Eating) (resource ?ACT ?FOOD)) (attribute ?FOOD Solid))").
:- load_kif("(subclass Drinking Ingesting)").
:- load_kif("(documentation Drinking EnglishLanguage \"The &%Process by which liquid food, i.e. &%Beverages, are incorporated into an &%Animal.\")").
:- load_kif("(=> (and (instance ?ACT Drinking) (resource ?ACT ?FOOD)) (attribute ?FOOD Liquid))").
:- load_kif("(subclass Digesting OrganismProcess)").
:- load_kif("(subclass Digesting AutonomicProcess)").
:- load_kif("(documentation Digesting EnglishLanguage \"The &%Process by which &%Food that has been ingested is broken down into simpler chemical compounds and absorbed by the &%Organism.\")").
:- load_kif("(=> (and (instance ?DIGEST Digesting) (agent ?DIGEST ?ORGANISM)) (exists (?INGEST) (and (instance ?INGEST Ingesting) (agent ?INGEST ?ORGANISM) (overlapsTemporally (WhenFn ?INGEST) (WhenFn ?DIGEST)))))").
:- load_kif("(=> (instance ?DIGEST Digesting) (exists (?DECOMP) (and (instance ?DECOMP ChemicalDecomposition) (subProcess ?DECOMP ?DIGEST))))").
:- load_kif("(subclass Growth AutonomicProcess)").
:- load_kif("(documentation Growth EnglishLanguage \"The &%Process of biological development in which an &%Organism or part of an &%Organism changes its form or its size.\")").
:- load_kif("(subclass Replication OrganismProcess)").
:- load_kif("(documentation Replication EnglishLanguage \"The &%Process of biological reproduction. This can be either a sexual or an asexual process.\")").
:- load_kif("(=> (and (instance ?REP Replication) (agent ?REP ?PARENT) (result ?REP ?CHILD)) (parent ?CHILD ?PARENT))").
:- load_kif("(=> (instance ?REP Replication) (exists (?BODY) (and (instance ?BODY ReproductiveBody) (result ?REP ?BODY))))").
:- load_kif("(subclass SexualReproduction Replication)").
:- load_kif("(disjoint SexualReproduction AsexualReproduction)").
:- load_kif("(documentation SexualReproduction EnglishLanguage \"Sexual &%Processes of biological reproduction.\")").
:- load_kif("(=> (and (instance ?REP SexualReproduction) (result ?REP ?ORGANISM)) (exists (?MOTHER ?FATHER) (and (mother ?ORGANISM ?MOTHER) (father ?ORGANISM ?FATHER))))").
:- load_kif("(subclass AsexualReproduction Replication)").
:- load_kif("(documentation AsexualReproduction EnglishLanguage \"Asexual &%Processes of biological reproduction.\")").
% :- load_kif("; NS: delete. (=> (and (instance ?REP AsexualReproduction) (result ?REP ?ORGANISM)) (not (exists (?PARENT1 ?PARENT2) (and (parent ?ORGANISM ?PARENT1) (parent ?ORGANISM ?PARENT2) (not (equal ?PARENT1 ?PARENT2))))))").
% :- load_kif("; NS: add.").
:- load_kif("(=> (and (instance ?REP AsexualReproduction) (result ?REP ?ORGANISM) (parent ?ORGANISM ?PARENT1) (parent ?ORGANISM ?PARENT2)) (equal ?PARENT1 ?PARENT2))").
:- load_kif("(subclass PsychologicalProcess BiologicalProcess)").
:- load_kif("(documentation PsychologicalProcess EnglishLanguage \"A &%BiologicalProcess which takes place in the mind or brain of an &%Organism and which may be manifested in the behavior of the &%Organism.\")").
:- load_kif("(=> (instance ?PROCESS PsychologicalProcess) (exists (?ANIMAL) (and (instance ?ANIMAL Animal) (experiencer ?PROCESS ?ANIMAL))))").
:- load_kif("(subclass PathologicProcess BiologicalProcess)").
:- load_kif("(disjoint PathologicProcess PhysiologicProcess)").
:- load_kif("(documentation PathologicProcess EnglishLanguage \"A disordered process, activity, or state of the &%Organism as a whole, of a body system or systems, or of multiple &%Organs or &%Tissues. Included here are normal responses to a negative stimulus as well as patholologic conditions or states that are less specific than a disease. Pathologic functions frequently have systemic effects.\")").
:- load_kif("(=> (and (instance ?PATH PathologicProcess) (experiencer ?PATH ?ORG)) (exists (?PART ?DISEASE) (and (part ?PART ?ORG) (instance ?DISEASE DiseaseOrSyndrome) (attribute ?PART ?DISEASE))))").
:- load_kif("(subclass Injuring PathologicProcess)").
:- load_kif("(subclass Injuring Damaging)").
:- load_kif("(documentation Injuring EnglishLanguage \"The process of creating a traumatic wound or injury. Since &%Injuring is not possible without some biologic function of the organism being injured, it is a subclass of &%BiologicalProcess.\")").
:- load_kif("(=> (instance ?INJ Injuring) (exists (?STRUCT) (and (instance ?STRUCT AnatomicalStructure) (patient ?INJ ?STRUCT))))").
:- load_kif("(<=> (instance ?INJ Injuring) (and (instance ?INJ Damaging) (exists (?ORGANISM) (and (instance ?ORGANISM Organism) (patient ?INJ ?ORGANISM)))))").
:- load_kif("(subclass Poisoning Injuring)").
:- load_kif("(documentation Poisoning EnglishLanguage \"A &%Poisoning is caused by an external substance. Since &%Poisoning is not possible without some biologic function which affects the &%Organism being injured, it is a subclass of &%BiologicalProcess.\")").
:- load_kif("(=> (instance ?POISON Poisoning) (exists (?THING) (and (patient ?POISON ?THING) (or (instance ?THING Organism) (instance ?THING AnatomicalStructure)))))").
:- load_kif("(=> (instance ?POISON Poisoning) (exists (?SUBSTANCE) (and (instance ?SUBSTANCE BiologicallyActiveSubstance) (instrument ?POISON ?SUBSTANCE))))").
:- load_kif("(documentation NaturalProcess EnglishLanguage \"A &%Process that take place in nature spontanously.\")").
:- load_kif("(subclass NaturalProcess Process)").
:- load_kif("(disjoint NaturalProcess IntentionalProcess)").
:- load_kif("(subclass IntentionalProcess Process)").
:- load_kif("(documentation IntentionalProcess EnglishLanguage \"A &%Process that has a specific purpose for the &%CognitiveAgent who performs it.\")").
:- load_kif("(=> (and (instance ?PROC IntentionalProcess) (agent ?PROC ?AGENT)) (exists (?PURP) (hasPurposeForAgent ?PROC ?PURP ?AGENT)))").
:- load_kif("(=> (instance ?PROC IntentionalProcess) (exists (?AGENT) (and (instance ?AGENT CognitiveAgent) (agent ?PROC ?AGENT))))").
:- load_kif("(=> (and (instance ?PROC IntentionalProcess) (agent ?PROC ?HUMAN) (instance ?HUMAN Animal)) (holdsDuring (WhenFn ?PROC) (attribute ?HUMAN Awake)))").
:- load_kif("(subclass IntentionalPsychologicalProcess IntentionalProcess)").
:- load_kif("(subclass IntentionalPsychologicalProcess PsychologicalProcess)").
:- load_kif("(documentation IntentionalPsychologicalProcess EnglishLanguage \"An &%IntentionalProcess that can be realized entirely within the mind or brain of an &%Organism. Thus, for example, &%Reasoning is a subclass of &%IntentionalPsychologicalProcess, because one can reason simply by exercising one's mind/brain. On the other hand, &%RecreationOrExercise is not a subclass of &%IntentionalPsychologicalProcess, because many instances of &%RecreationOrExercise necessarily have &%subProcesses of &%BodyMotion.\")").
:- load_kif("(subclass RecreationOrExercise IntentionalProcess)").
:- load_kif("(documentation RecreationOrExercise EnglishLanguage \"A &%Process that is carried out for the purpose of recreation or exercise. Since &%RecreationOrExercise is a subclass of &%IntentionalProcess, the intent of a process determines whether or not it is an instance of the class. Hence, if John and Bill watch the same program on television, and John watches it to relax while Bill watches it solely to satisfy an educational requirement, then John's watching the movie is an instance of &%RecreationOrExercise, while Bill's is not (both cases of watching the television program would however be in the class of &%Seeing, since being an instance of this latter class is not determined by intention).\")").
:- load_kif("(subclass OrganizationalProcess IntentionalProcess)").
:- load_kif("(documentation OrganizationalProcess EnglishLanguage \"An &%IntentionalProcess that involves an &%Organization.\")").
:- load_kif("(=> (and (instance ?ACT OrganizationalProcess) (agent ?ACT ?AGENT)) (or (instance ?AGENT Organization) (exists (?ORG) (and (instance ?ORG Organization) (member ?AGENT ?ORG)))))").
:- load_kif("(subclass Election OrganizationalProcess)").
:- load_kif("(documentation Election EnglishLanguage \"&%Election is the class of events conducted by an organization, in which qualified participants vote for officers, adopt resolutions, or settle other issues in that &%Organization.\")").
:- load_kif("(subclass ReligiousProcess OrganizationalProcess)").
:- load_kif("(documentation ReligiousProcess EnglishLanguage \"An &%OrganizationalProcess that is carried out within or by a &%ReligiousOrganization.\")").
:- load_kif("(=> (and (instance ?ACT ReligiousProcess) (agent ?ACT ?AGENT)) (or (instance ?AGENT ReligiousOrganization) (exists (?ORG) (and (member ?AGENT ?ORG) (instance ?ORG ReligiousOrganization)))))").
:- load_kif("(subclass JoiningAnOrganization OrganizationalProcess)").
:- load_kif("(documentation JoiningAnOrganization EnglishLanguage \"The &%OrganizationalProcess of becoming a &%member of an &%Organization.\")").
:- load_kif("(=> (and (instance ?JOIN JoiningAnOrganization) (instance ?ORG Organization) (agent ?JOIN ?PERSON) (patient ?JOIN ?ORG)) (and (holdsDuring (BeginFn (WhenFn ?JOIN)) (not (member ?PERSON ?ORG))) (holdsDuring (EndFn (WhenFn ?JOIN)) (member ?PERSON ?ORG))))").
:- load_kif("(subclass LeavingAnOrganization OrganizationalProcess)").
:- load_kif("(disjoint LeavingAnOrganization JoiningAnOrganization)").
:- load_kif("(documentation LeavingAnOrganization EnglishLanguage \"The &%OrganizationalProcess of leaving an &%Organization, whether voluntarily or involuntarily.\")").
:- load_kif("(=> (and (instance ?LEAVE LeavingAnOrganization) (instance ?ORG Organization) (agent ?LEAVE ?PERSON) (patient ?LEAVE ?ORG)) (and (holdsDuring (BeginFn (WhenFn ?LEAVE)) (member ?PERSON ?ORG)) (holdsDuring (EndFn (WhenFn ?LEAVE)) (not (member ?PERSON ?ORG)))))").
:- load_kif("(subclass Graduation LeavingAnOrganization)").
:- load_kif("(documentation Graduation EnglishLanguage \"The &%OrganizationalProcess of graduating from an &%EducationalOrganization.\")").
:- load_kif("(=> (and (instance ?GRAD Graduation) (agent ?GRAD ?ORG)) (instance ?ORG EducationalOrganization))").
:- load_kif("(subclass Matriculation JoiningAnOrganization)").
:- load_kif("(documentation Matriculation EnglishLanguage \"The &%OrganizationalProcess of joining an &%EducationalOrganization as a student.\")").
:- load_kif("(=> (and (instance ?MAT Matriculation) (agent ?MAT ?ORG)) (instance ?ORG EducationalOrganization))").
:- load_kif("(subclass Hiring JoiningAnOrganization)").
:- load_kif("(documentation Hiring EnglishLanguage \"&%OrganizationalProcesses where someone is made an employee of an &%Organization.\")").
:- load_kif("(=> (and (instance ?HIRE Hiring) (instance ?ORG Organization) (agent ?HIRE ?ORG) (patient ?HIRE ?PERSON)) (and (holdsDuring (BeginFn (WhenFn ?HIRE)) (not (employs ?ORG ?PERSON))) (holdsDuring (EndFn (WhenFn ?HIRE)) (employs ?ORG ?PERSON))))").
:- load_kif("(subclass TerminatingEmployment LeavingAnOrganization)").
:- load_kif("(documentation TerminatingEmployment EnglishLanguage \"&%OrganizationalProcesses where someone ceases to be an employee of an &%Organization. Note that this covers being laid off, being fired, and voluntarily leaving a job.\")").
:- load_kif("(=> (and (instance ?FIRE TerminatingEmployment) (instance ?ORG Organization) (agent ?FIRE ?ORG) (patient ?FIRE ?PERSON)) (and (holdsDuring (BeginFn (WhenFn ?FIRE)) (employs ?ORG ?PERSON)) (holdsDuring (EndFn (WhenFn ?FIRE)) (not (employs ?ORG ?PERSON)))))").
:- load_kif("(subclass PoliticalProcess OrganizationalProcess)").
:- load_kif("(documentation PoliticalProcess EnglishLanguage \"An &%OrganizationalProcess carried out by, for or against officially constituted governments. Some examples would be voting on proposed legislation, electing a government representative, or even overthrowing a government in a revolution.\")").
:- load_kif("(=> (instance ?PROC PoliticalProcess) (exists (?POL) (and (or (instance ?POL Government) (exists (?GOV) (and (instance ?GOV Government) (member ?POL ?GOV)))) (or (agent ?PROC ?POL) (patient ?PROC ?POL)))))").
:- load_kif("(subclass JudicialProcess PoliticalProcess)").
:- load_kif("(documentation JudicialProcess EnglishLanguage \"Any legal proceeding which is conducted by a &%JudicialOrganization. Note that there is an important difference between the concepts &%LegalAction and &%JudicialProcess. The former refers to legal claims that are brought by a plaintiff, e.g. law suits, while the second refers to trials and other sorts of judicial hearings where the merits of a &%LegalAction are decided.\")").
:- load_kif("(=> (and (instance ?PROCESS JudicialProcess) (agent ?PROCESS ?ORG) (instance ?ORG Organization)) (instance ?ORG JudicialOrganization))").
:- load_kif("(subclass LegalDecision JudicialProcess)").
:- load_kif("(subclass LegalDecision Declaring)").
:- load_kif("(documentation LegalDecision EnglishLanguage \"A decision issued by a court with respect to a &%LegalAction. Note that a &%LegalDecision is the act of &%Declaring a decision of a court, it is not the act of judge or jury &%Deciding the merits of a particular &%LegalAction.\")").
:- load_kif("(=> (instance ?DECISION LegalDecision) (exists (?ACTION) (and (instance ?ACTION LegalAction) (refers ?DECISION ?ACTION))))").
:- load_kif("(=> (instance ?DECISION LegalDecision) (exists (?DECIDE) (and (instance ?DECIDE Deciding) (earlier (WhenFn ?DECIDE) (WhenFn ?DECISION)))))").
:- load_kif("(subclass MilitaryProcess PoliticalProcess)").
:- load_kif("(subclass MilitaryProcess OrganizationalProcess)").
:- load_kif("(documentation MilitaryProcess EnglishLanguage \"Any &%Process that is carried out by a military organization. Note that this class covers &%Processes, e.g. military operations, that are the result of careful planning, as well as those which are unscripted.\")").
:- load_kif("(subclass RegulatoryProcess Guiding)").
:- load_kif("(documentation RegulatoryProcess EnglishLanguage \"an &%Guiding whose aim is the enforcement of rules or regulations. Note the key differences between &%RegulatoryProcess and the related concept &%Managing. The latter implies a long-term relationship between a single manager and limited number of agents who are managed, while the former implies a normative standard to which the activities of the regulated are referred.\")").
:- load_kif("(subclass Managing OrganizationalProcess)").
:- load_kif("(subclass Managing Guiding)").
:- load_kif("(documentation Managing EnglishLanguage \"&%OrganizationalProcesses that involve overseeing the activities of others. Note the key differences between &%RegulatoryProcess and its sibling &%Managing. The latter implies a long-term relationship between the manager and the managed, while the former implies a normative standard to which the activities of the regulated are referred.\")").
:- load_kif("(subclass Planning IntentionalPsychologicalProcess)").
:- load_kif("(documentation Planning EnglishLanguage \"Specifying a set of actions in order to meet a set of goals or objectives.\")").
:- load_kif("(=> (and (instance ?EVENT Planning) (result ?EVENT ?CBO) (instance ?CBO ContentBearingObject)) (exists (?PLAN) (and (instance ?PLAN Plan) (containsInformation ?CBO ?PLAN))))").
:- load_kif("(subclass Designing IntentionalPsychologicalProcess)").
:- load_kif("(documentation Designing EnglishLanguage \"The spatial analogue of &%Planning. &%Designing a &%Collection of &%Objects involves determining a placement of the &%Objects with respect to one another and perhaps other &%Objects as well, in order to satisfy a particular purpose.\")").
:- load_kif("(subclass Interpreting IntentionalPsychologicalProcess)").
:- load_kif("(documentation Interpreting EnglishLanguage \"Any &%Process of assigning a &%Proposition to a &%Text, i.e. understanding the &%Text.\")").
:- load_kif("(=> (and (instance ?INTERPRET Interpreting) (agent ?INTERPRET ?AGENT) (patient ?INTERPRET ?CONTENT) (instance ?CONTENT ContentBearingObject)) (exists (?PROP) (holdsDuring (EndFn (WhenFn ?INTERPRET)) (believes ?AGENT (containsInformation ?CONTENT ?PROP)))))").
:- load_kif("(subclass QuantityChange InternalChange)").
:- load_kif("(partition QuantityChange Increasing Decreasing)").
:- load_kif("(documentation QuantityChange EnglishLanguage \"Any &%InternalChange where a &%PhysicalQuantity associated with the &%patient is altered.\")").
:- load_kif("(subclass Increasing QuantityChange)").
:- load_kif("(relatedInternalConcept Increasing Putting)").
:- load_kif("(documentation Increasing EnglishLanguage \"Any &%QuantityChange where the &%PhysicalQuantity is increased.\")").
:- load_kif("(=> (and (instance ?INCREASE Increasing) (patient ?INCREASE ?OBJ)) (exists (?UNIT ?QUANT1 ?QUANT2) (and (holdsDuring (BeginFn (WhenFn ?INCREASE)) (equal (MeasureFn ?OBJ ?UNIT) ?QUANT1)) (holdsDuring (EndFn (WhenFn ?INCREASE)) (equal (MeasureFn ?OBJ ?UNIT) ?QUANT2)) (greaterThan ?QUANT2 ?QUANT1))))").
:- load_kif("(subclass Heating Increasing)").
:- load_kif("(disjoint Heating Cooling)").
:- load_kif("(documentation Heating EnglishLanguage \"Any &%Increasing &%Process where the &%PhysicalQuantity increased is a &%TemperatureMeasure.\")").
:- load_kif("(=> (and (instance ?HEAT Heating) (patient ?HEAT ?OBJ)) (exists (?UNIT ?QUANT1 ?QUANT2) (and (instance ?UNIT TemperatureMeasure) (holdsDuring (BeginFn (WhenFn ?HEAT)) (equal (MeasureFn ?OBJ ?UNIT) ?QUANT1)) (holdsDuring (EndFn (WhenFn ?HEAT)) (equal (MeasureFn ?OBJ ?UNIT) ?QUANT2)) (greaterThan ?QUANT2 ?QUANT1))))").
:- load_kif("(subclass Decreasing QuantityChange)").
:- load_kif("(relatedInternalConcept Decreasing Removing)").
:- load_kif("(documentation Decreasing EnglishLanguage \"Any &%QuantityChange where the &%PhysicalQuantity is decreased.\")").
:- load_kif("(=> (and (instance ?DECREASE Decreasing) (patient ?DECREASE ?OBJ)) (exists (?UNIT ?QUANT1 ?QUANT2) (and (holdsDuring (BeginFn (WhenFn ?DECREASE)) (equal (MeasureFn ?OBJ ?UNIT) ?QUANT1)) (holdsDuring (EndFn (WhenFn ?DECREASE)) (equal (MeasureFn ?OBJ ?UNIT) ?QUANT2)) (lessThan ?QUANT2 ?QUANT1))))").
:- load_kif("(subclass Cooling Decreasing)").
:- load_kif("(documentation Cooling EnglishLanguage \"Any &%Decreasing &%Process where the &%PhysicalQuantity decreased is a &%TemperatureMeasure.\")").
:- load_kif("(=> (and (instance ?COOL Cooling) (patient ?COOL ?OBJ)) (exists (?UNIT ?QUANT1 ?QUANT2) (and (instance ?UNIT TemperatureMeasure) (holdsDuring (BeginFn (WhenFn ?COOL)) (equal (MeasureFn ?OBJ ?UNIT) ?QUANT1)) (holdsDuring (EndFn (WhenFn ?COOL)) (equal (MeasureFn ?OBJ ?UNIT) ?QUANT2)) (lessThan ?QUANT2 ?QUANT1))))").
% :- load_kif("; NS: moved from Mid-level-ontology.kif").
:- load_kif("(instance moves CaseRole)").
:- load_kif("(domain moves 1 Motion)").
:- load_kif("(domain moves 2 Object)").
:- load_kif("(documentation moves EnglishLanguage \"(&%moves ?MOTION ?OBJECT) means that during the &%Motion event ?MOTION, ?OBJECT moves. This does not necessarily imply that the location of ?OBJECT changes during ?MOTION. See also &%changesLocation and &%Translocation.\")").
:- load_kif("(subrelation moves involvedInEvent)").
% :- load_kif("; NS: moved from Mid-level-ontology.kif").
:- load_kif("(instance changesLocation CaseRole)").
:- load_kif("(domain changesLocation 1 Translocation)").
:- load_kif("(domain changesLocation 2 Object)").
:- load_kif("(documentation changesLocation EnglishLanguage \"(&%changesLocation ?EVENT ?OBJECT) means that during the &%Translocation event ?EVENT, ?OBJECT's location changes. ?OBJECT might also be the &%agent, &%patient, or &%experiencer of ?EVENT.\")").
:- load_kif("(subrelation changesLocation moves) MS: added rule.").
:- load_kif("(=> (changesLocation ?EVENT ?OBJ) (and (instance ?EVENT Translocation) (instance ?OBJ Object) (or (patient ?EVENT ?OBJ) (agent ?EVENT ?OBJ) (experiencer ?EVENT ?OBJ))))").
:- load_kif("(subclass Motion Process)").
:- load_kif("(documentation Motion EnglishLanguage \"Any &%Process of movement.\")").
% :- load_kif("; NS: add.").
:- load_kif("(=> (instance ?MOTION Motion) (exists (?OBJ) (and (instance ?OBJ Object) (moves ?MOTION ?OBJ))))").
% :- load_kif("; NS: delete. The imprecision of &%patient here will be a problem if this rule is ever used in inference, given the number and variety of subclasses of Motion. (=> (and (instance ?MOTION Motion) (patient ?MOTION ?OBJ) (origin ?MOTION ?PLACE)) (holdsDuring (BeginFn (WhenFn ?MOTION)) (located ?OBJ ?PLACE)))").
% :- load_kif("; NS: add.").
:- load_kif("(=> (and (instance ?MOTION Motion) (moves ?MOTION ?OBJ) (origin ?MOTION ?PLACE)) (holdsDuring (BeginFn (WhenFn ?MOTION)) (located ?OBJ ?PLACE)))").
% :- load_kif("; NS: delete. (=> (and (instance ?MOTION Motion) (patient ?MOTION ?OBJ) (destination ?MOTION ?PLACE)) (holdsDuring (EndFn (WhenFn ?MOTION)) (located ?OBJ ?PLACE)))").
% :- load_kif("; NS: add.").
:- load_kif("(=> (and (instance ?MOTION Motion) (moves ?MOTION ?OBJ) (destination ?MOTION ?PLACE)) (holdsDuring (EndFn (WhenFn ?MOTION)) (located ?OBJ ?PLACE)))").
:- load_kif("(subclass MotionUpward Motion)").
:- load_kif("(disjoint MotionUpward MotionDownward)").
:- load_kif("(documentation MotionUpward EnglishLanguage \"&%Motion where an &%Object is moving away from the ground.\")").
:- load_kif("(subclass MotionDownward Motion)").
:- load_kif("(documentation MotionDownward EnglishLanguage \"&%Motion where an &%Object is moving toward the ground.\")").
:- load_kif("(instance path CaseRole) KJN: Deleting this as it is redundant. (subrelation path involvedInEvent)").
:- load_kif("(subrelation path eventPartlyLocated)").
:- load_kif("(domain path 1 Motion)").
:- load_kif("(domain path 2 Object)").
:- load_kif("(documentation path EnglishLanguage \"(&%path ?MOTION ?PATH) means that ?PATH is a route along which ?MOTION occurs. For example, Highway 101 is the path in the following proposition: the car drove up Highway 101.\")").
:- load_kif("(=> (and (path ?PROCESS ?PATH1) (origin ?PROCESS ?SOURCE) (destination ?PROCESS ?DEST) (length ?PATH1 ?MEASURE1) (distance ?SOURCE ?DEST ?DISTANCE) (not (greaterThan ?MEASURE1 ?DISTANCE))) (forall (?OBJ) (=> (part ?OBJ ?PATH1) (between ?SOURCE ?OBJ ?DEST))))").
:- load_kif("(subclass BodyMotion Motion)").
:- load_kif("(documentation BodyMotion EnglishLanguage \"Any &%Motion where the &%agent is an &%Organism and the &%patient is a &%BodyPart.\")").
% :- load_kif("; NS: delete. (=> (instance ?MOTION BodyMotion) (exists (?OBJ ?AGENT) (and (instance ?OBJ BodyPart) (patient ?MOTION ?OBJ) (instance ?AGENT Organism) (agent ?MOTION ?AGENT))))").
% :- load_kif("; NS: add.").
:- load_kif("(=> (and (instance ?MOTION BodyMotion) (moves ?MOTION ?OBJ)) (and (instance ?OBJ BodyPart) (patient ?MOTION ?OBJ)))").
% :- load_kif("; NS: add.").
:- load_kif("(=> (instance ?MOTION BodyMotion) (exists (?AGENT) (and (instance ?AGENT Organism) (agent ?MOTION ?AGENT))))").
:- load_kif("(subclass VocalCords Organ)").
:- load_kif("(documentation VocalCords EnglishLanguage \"The vocal cords, are composed of two folds of mucous membrane stretched horizontally across the larynx. They vibrate, modulating the flow of air being expelled from the lungs during &%Vocalizing. \")").
:- load_kif("(typicalPart VocalCords Human)").
:- load_kif("(typicallyContainsPart VocalCords Human)").
:- load_kif("(initialPart VocalCords Human) NS: delete. (=> (instance ?VOCAL Vocalizing) (exists (?HUMAN) (and (instance ?HUMAN Human) (agent ?VOCAL ?HUMAN) (instrument ?VOCAL ?HUMAN))))").
% :- load_kif("; NS: add. This probably is too restrictive, given that primates and many other types of mammals, as well as birds, also vocalize. Create VocalCord, and add the instrument part of the rule above, in Mid-level-ontology.kif.").
:- load_kif("(subclass Vocalizing RadiatingSound)").
:- load_kif("(documentation Vocalizing EnglishLanguage \"Any instance of &%RadiatingSound where the instrument is the vocal cord. This covers grunts, screams, raors, as well as &%Speaking.\")").
:- load_kif("(=> (instance ?VOCAL Vocalizing) (exists (?CORD ?ORGANISM) (and (instance ?CORD VocalCords) (instrument ?VOCAL ?CORD) (part ?VOCAL ?ORGANISM) (instance ?ORGANISM Organism))))").
:- load_kif("(subclass Speaking LinguisticCommunication)").
:- load_kif("(subclass Speaking Vocalizing)").
% :- load_kif("; NS: delete. &%LinguisticGeneration does not exist. (documentation Speaking EnglishLanguage \"Any &%LinguisticGeneration which is also a &%Vocalizing, i.e. any &%LinguisticCommunication by a &%Human which involves his/her vocal cords.\")").
% :- load_kif("; NS: add.").
:- load_kif("(documentation Speaking EnglishLanguage \"Any &%LinguisticCommunication by a &%Human which involves his/her vocal cords.\")").
% :- load_kif("; KJN: Deleting. Fixed Music ontology and replacing it with concepts from that. (subclass Singing Speaking) (subclass Singing Music) (documentation Singing EnglishLanguage \"&%Speaking that is also &%Music.\")").
:- load_kif("(subclass Ambulating BodyMotion)").
:- load_kif("(subclass Ambulating Translocation)").
:- load_kif("(partition Ambulating Walking Running)").
:- load_kif("(documentation Ambulating EnglishLanguage \"Any &%BodyMotion which is accomplished by means of the legs of an &%Animal for the purpose of moving from one point to another.\")").
:- load_kif("(subclass Walking Ambulating)").
:- load_kif("(documentation Walking EnglishLanguage \"&%Ambulating relatively slowly, i.e. moving in such a way that at least one foot is always in contact with the ground.\")").
:- load_kif("(subclass Running Ambulating)").
:- load_kif("(documentation Running EnglishLanguage \"&%Ambulating relatively quickly, i.e. moving in such a way that, with each step, neither foot is in contact with the ground for a period of time.\")").
:- load_kif("(=> (and (instance ?WALK Walking) (instance ?RUN Running) (agent ?WALK ?AGENT) (agent ?RUN ?AGENT) (holdsDuring (WhenFn ?WALK) (measure ?AGENT (SpeedFn ?LENGTH1 ?TIME))) (holdsDuring (WhenFn ?RUN) (measure ?AGENT (SpeedFn ?LENGTH2 ?TIME)))) (greaterThan ?LENGTH2 ?LENGTH1))").
:- load_kif("(subclass Swimming BodyMotion)").
:- load_kif("(documentation Swimming EnglishLanguage \"Any deliberate and controlled &%BodyMotion through water that is accomplished by an &%Organism.\")").
:- load_kif("(=> (and (instance ?SWIM Swimming) (agent ?SWIM ?AGENT)) (exists (?AREA) (and (instance ?AREA WaterArea) (located ?AGENT ?AREA))))").
:- load_kif("(subclass Dancing BodyMotion)").
:- load_kif("(documentation Dancing EnglishLanguage \"Any &%BodyMotion of &%Humans which is deliberately coordinated with music.\")").
:- load_kif("(subclass GeologicalProcess Motion)").
:- load_kif("(subclass GeologicalProcess InternalChange)").
:- load_kif("(disjoint GeologicalProcess IntentionalProcess)").
:- load_kif("(documentation GeologicalProcess EnglishLanguage \"The class of activities that are caused by geological forces and affect geological features, and which may affect the biosphere as well.\")").
:- load_kif("(subclass WeatherProcess Motion)").
:- load_kif("(disjoint WeatherProcess IntentionalProcess)").
:- load_kif("(documentation WeatherProcess EnglishLanguage \"&%WeatherProcess is the broadest class of processes that involve weather, including weather seasons (not to be confused with instances of &%SeasonOfYear), weather systems, and short-term weather events.\")").
:- load_kif("(subclass Precipitation WeatherProcess)").
:- load_kif("(subclass Precipitation WaterMotion)").
:- load_kif("(subclass Precipitation Falling)").
:- load_kif("(documentation Precipitation EnglishLanguage \"&%Precipitation is the process of water molecules falling from the air to the ground, in either a liquid or frozen state.\")").
:- load_kif("(=> (instance ?PROCESS Precipitation) (exists (?STUFF) (and (instance ?STUFF Water) (patient ?PROCESS ?STUFF))))").
:- load_kif("(subclass LiquidMotion Motion)").
:- load_kif("(documentation LiquidMotion EnglishLanguage \"Any &%Motion where the &%patient is a &%Liquid. This class would cover, in particular, the flow of &%Water.\")").
:- load_kif("(=> (and (instance ?MOTION LiquidMotion) (patient ?MOTION ?OBJ)) (attribute ?OBJ Liquid))").
:- load_kif("(subclass WaterMotion LiquidMotion)").
:- load_kif("(documentation WaterMotion EnglishLanguage \"Any &%LiquidMotion where the &%Liquid is &%Water.\")").
:- load_kif("(=> (instance ?MOTION WaterMotion) (exists (?WATER) (and (patient ?MOTION ?WATER) (instance ?WATER Water))))").
:- load_kif("(subclass GasMotion Motion)").
:- load_kif("(documentation GasMotion EnglishLanguage \"Any &%Motion where the &%patient is a &%Gas. This class would cover, in particular, the motion of &%Air, e.g. a breeze or wind.\")").
:- load_kif("(=> (and (instance ?MOTION GasMotion) (patient ?MOTION ?OBJ)) (attribute ?OBJ Gas))").
:- load_kif("(subclass Wind GasMotion)").
:- load_kif("(documentation Wind EnglishLanguage \"Any &%Motion of &%Air.\")").
:- load_kif("(subclass DirectionChange Motion)").
:- load_kif("(documentation DirectionChange EnglishLanguage \"The act of changing the direction in which the &%patient of the act is oriented.\")").
:- load_kif("(=> (instance ?PROC DirectionChange) (exists (?ATTR) (and (instance ?ATTR DirectionalAttribute) (or (and (holdsDuring (BeginFn (WhenFn ?PROC)) (manner ?PROC ?ATTR)) (holdsDuring (EndFn (WhenFn ?PROC)) (not (manner ?PROC ?ATTR)))) (and (holdsDuring (EndFn (WhenFn ?PROC)) (manner ?PROC ?ATTR)) (holdsDuring (BeginFn (WhenFn ?PROC)) (not (manner ?PROC ?ATTR))))))))").
:- load_kif("(subclass Transfer Translocation)").
:- load_kif("(documentation Transfer EnglishLanguage \"Any &%instance of &%Translocation where the &%agent and the &%patient are not the same thing.\")").
:- load_kif("(=> (and (instance ?TRANSFER Transfer) (agent ?TRANSFER ?AGENT) (patient ?TRANSFER ?PATIENT)) (not (equal ?AGENT ?PATIENT)))").
:- load_kif("(=> (and (instance ?T1 Translocation) (instance ?T2 Translocation) (origin ?T1 ?O1) (origin ?T2 ?D1) (destination ?T1 ?D1) (destination ?T2 ?D2) (experiencer ?T1 ?P) (experiencer ?T2 ?P)) (exists (?T) (and (instance ?T Translocation) (origin ?T ?O1) (destination ?T ?D2) (subProcess ?T1 ?T) (subProcess ?T2 ?T) (experiencer ?T ?P) (starts (WhenFn ?T1) (WhenFn ?T)) (finishes (WhenFn ?T2) (WhenFn ?T)))))").
:- load_kif("(subclass Carrying Transfer)").
:- load_kif("(documentation Carrying EnglishLanguage \"&%Transfer from one point to another by means of an &%Animal or &%Human.\")").
:- load_kif("(=> (instance ?CARRY Carrying) (exists (?ANIMAL) (and (instance ?ANIMAL Animal) (instrument ?CARRY ?ANIMAL))))").
:- load_kif("(subclass Removing Transfer)").
:- load_kif("(documentation Removing EnglishLanguage \"The &%Class of &%Processes where something is taken away from a location. Note that the thing removed and the location are specified with the &%CaseRoles &%patient and &%origin, respectively.\")").
:- load_kif("(=> (and (instance ?REMOVE Removing) (origin ?REMOVE ?PLACE) (patient ?REMOVE ?OBJ)) (and (holdsDuring (BeginFn (WhenFn ?REMOVE)) (located ?OBJ ?PLACE)) (holdsDuring (EndFn (WhenFn ?REMOVE)) (not (located ?OBJ ?PLACE)))))").
:- load_kif("(subclass Uncovering Removing)").
:- load_kif("(disjoint Uncovering Covering)").
:- load_kif("(documentation Uncovering EnglishLanguage \"The &%Class of &%Removing processes where the &%agent uncovers the &%patient, either completely or only partially.\")").
:- load_kif("(subclass Putting Transfer)").
:- load_kif("(documentation Putting EnglishLanguage \"The &%Class of &%Processes where something is put in a location. Note that the location is specified with the &%CaseRole &%destination.\")").
:- load_kif("(=> (and (instance ?PUT Putting) (destination ?PUT ?PLACE) (patient ?PUT ?OBJ)) (and (holdsDuring (BeginFn (WhenFn ?PUT)) (not (located ?OBJ ?PLACE))) (holdsDuring (EndFn (WhenFn ?PUT)) (located ?OBJ ?PLACE))))").
:- load_kif("(subclass Covering Putting)").
:- load_kif("(documentation Covering EnglishLanguage \"The &%Class of &%Putting processes where the &%agent covers the &%patient, either completely or only partially, with something else.\")").
:- load_kif("(subclass Inserting Putting)").
:- load_kif("(documentation Inserting EnglishLanguage \"&%Putting one thing inside of another thing.\")").
:- load_kif("(=> (and (instance ?INSERT Inserting) (patient ?INSERT ?OBJ1) (destination ?INSERT ?OBJ2)) (and (holdsDuring (BeginFn (WhenFn ?INSERT)) (not (contains ?OBJ2 ?OBJ1))) (holdsDuring (EndFn (WhenFn ?INSERT)) (contains ?OBJ2 ?OBJ1))))").
:- load_kif("(subclass Injecting Inserting)").
:- load_kif("(documentation Injecting EnglishLanguage \"&%Inserting a &%BiologicallyActiveSubstance into an &%Animal or a &%Human with a syringe.\")").
:- load_kif("(=> (instance ?INJECT Injecting) (exists (?SUBSTANCE ?ANIMAL) (and (patient ?INJECT ?SUBSTANCE) (instance ?SUBSTANCE BiologicallyActiveSubstance) (attribute ?SUBSTANCE Liquid) (destination ?INJECT ?ANIMAL) (instance ?ANIMAL Animal))))").
:- load_kif("(subclass Substituting Transfer)").
:- load_kif("(subclass Substituting DualObjectProcess)").
:- load_kif("(documentation Substituting EnglishLanguage \"The &%Class of &%Transfers where one thing is replaced with something else.\")").
:- load_kif("(=> (instance ?SUB Substituting) (exists (?PUT ?REMOVE ?OBJ1 ?OBJ2 ?PLACE) (and (instance ?PUT Putting) (instance ?REMOVE Removing) (subProcess ?PUT ?SUB) (subProcess ?REMOVE ?SUB) (patient ?REMOVE ?OBJ1) (origin ?REMOVE ?PLACE) (patient ?PUT ?OBJ2) (destination ?PUT ?PLACE) (not (equal ?OBJ1 ?OBJ2)))))").
:- load_kif("(subclass Impelling Transfer)").
:- load_kif("(documentation Impelling EnglishLanguage \"The &%subclass of &%Transfer where the &%patient travels through space by means of a sudden, forceful event. Some examples would be shooting, throwing, tossing, etc.\")").
:- load_kif("(subclass Shooting Impelling)").
:- load_kif("(documentation Shooting EnglishLanguage \"The &%subclass of &%Impelling where the &%patient is a projectile that is fired through the air by means of some sort of &%Device.\")").
:- load_kif("(subclass Touching Transfer)").
:- load_kif("(documentation Touching EnglishLanguage \"Any &%Transfer where two &%Objects are brought into immediate physical contact with one another.\")").
:- load_kif("(=> (and (instance ?TOUCH Touching) (agent ?TOUCH ?OBJ1) (patient ?TOUCH ?OBJ2)) (and (holdsDuring (BeginFn (WhenFn ?TOUCH)) (not (connected ?OBJ1 ?OBJ2))) (holdsDuring (EndFn (WhenFn ?TOUCH)) (connected ?OBJ1 ?OBJ2))))").
:- load_kif("(subrelation grasps meetsSpatially)").
:- load_kif("(domain grasps 1 Animal)").
:- load_kif("(domain grasps 2 Object)").
:- load_kif("(documentation grasps EnglishLanguage \"The state of grasping an &%Object. (&%grasps ?ANIMAL ?OBJ) means that the &%Animal ?ANIMAL is intentionally holding on to the &%Object ?OBJ.\")").
:- load_kif("(subclass Grabbing Touching)").
:- load_kif("(subclass Grabbing Attaching)").
:- load_kif("(documentation Grabbing EnglishLanguage \"Any instance of &%Touching which results in a situation where the &%agent &%grasps the &%patient of the &%Touching.\")").
:- load_kif("(=> (and (instance ?GRAB Grabbing) (agent ?GRAB ?AGENT) (patient ?GRAB ?THING)) (and (holdsDuring (BeginFn (WhenFn ?GRAB)) (not (grasps ?AGENT ?THING))) (holdsDuring (EndFn (WhenFn ?GRAB)) (grasps ?AGENT ?THING))))").
:- load_kif("(subclass Releasing Transfer)").
:- load_kif("(documentation Releasing EnglishLanguage \"Any instance of &%Transfer which results in a situation where it is not the case that the &%agent &%grasps something which he/she &%grasps previously.\")").
:- load_kif("(=> (and (instance ?RELEASE Releasing) (agent ?GRAB ?AGENT) (patient ?GRAB ?THING)) (and (holdsDuring (BeginFn (WhenFn ?RELEASE)) (grasps ?AGENT ?THING)) (holdsDuring (EndFn (WhenFn ?RELEASE)) (not (grasps ?AGENT ?THING)))))").
:- load_kif("(subclass Impacting Touching)").
:- load_kif("(documentation Impacting EnglishLanguage \"Any &%Touching where something comes into sudden, forceful, physical contact with something else. Some examples would be striking, knocking, whipping etc.\")").
:- load_kif("(=> (and (instance ?IMPACT Impacting) (patient ?IMPACT ?OBJ)) (exists (?IMPEL) (and (instance ?IMPEL Impelling) (patient ?IMPEL ?OBJ) (earlier (WhenFn ?IMPEL) (WhenFn ?IMPACT)))))").
:- load_kif("(subclass Translocation Motion)").
:- load_kif("(documentation Translocation EnglishLanguage \"&%Translocation is that class of &%Motions in which an object moves from one place to another. In the case of round trips, the &%origin and &%destination are the same, but the intervening motion passes through other locations. &%Translocation represents linear motion, in contrast to rotation or other movement in place. A vehicle is not necessary, &%Ambulating is a kind of &%Translocation.\")").
:- load_kif("(=> (and (instance ?MOVEMENT Translocation) (origin ?MOVEMENT ?PLACE1)) (exists (?PLACE2 ?STAGE) (and (instance ?PLACE2 Region) (not (equal ?PLACE1 ?PLACE2)) (subProcess ?STAGE ?MOVEMENT) (located ?STAGE ?PLACE2))))").
:- load_kif("(=> (instance ?T Translocation) (exists (?O ?D ?P) (and (instance ?O Object) (instance ?D Object) (instance ?P Object) (path ?T ?P) (origin ?T ?O) (destination ?T ?D))))").
:- load_kif("(subclass Falling Translocation)").
:- load_kif("(subclass Falling MotionDownward)").
:- load_kif("(documentation Falling EnglishLanguage \"&%Falling is the class of events in which something moves from a higher location to a lower location under the force of gravity.\")").
:- load_kif("(=> (and (instance ?DROP Falling) (origin ?DROP ?START) (destination ?DROP ?FINISH)) (orientation ?FINISH ?START Below))").
:- load_kif("(subclass Transportation Translocation)").
:- load_kif("(relatedInternalConcept Transportation TransportationDevice)").
:- load_kif("(documentation Transportation EnglishLanguage \"&%Motion from one point to another by means of a &%TransportationDevice.\")").
:- load_kif("(=> (instance ?TRANS Transportation) (exists (?DEVICE) (and (instance ?DEVICE TransportationDevice) (instrument ?TRANS ?DEVICE))))").
:- load_kif("(subclass Guiding IntentionalProcess)").
:- load_kif("(documentation Guiding EnglishLanguage \"Any &%IntentionalProcess where the &%agent tries to direct the behavior of another &%Object, whether an &%Agent or not.\")").
:- load_kif("(subclass Driving Guiding)").
:- load_kif("(documentation Driving EnglishLanguage \"Controlling the direction and/or speed of a &%Vehicle. This includes navigating a ship, driving a car or truck, operating a train, etc.\")").
:- load_kif("(=> (instance ?DRIVE Driving) (exists (?VEHICLE) (and (instance ?VEHICLE Vehicle) (patient ?DRIVE ?VEHICLE))))").
:- load_kif("(subclass EducationalProcess Guiding)").
:- load_kif("(documentation EducationalProcess EnglishLanguage \"Any &%Process which is intended to result in &%Learning.\")").
:- load_kif("(=> (and (instance ?EDUCATION EducationalProcess) (patient ?EDUCATION ?PERSON)) (hasPurpose ?EDUCATION (exists (?LEARN) (and (instance ?LEARN Learning) (patient ?LEARN ?PERSON)))))").
:- load_kif("(subclass ChangeOfPossession SocialInteraction)").
:- load_kif("(relatedInternalConcept ChangeOfPossession possesses)").
:- load_kif("(documentation ChangeOfPossession EnglishLanguage \"The &%Class of &%Processes where ownership of something is transferred from one &%Agent to another.\")").
:- load_kif("(=> (and (instance ?CHANGE ChangeOfPossession) (patient ?CHANGE ?OBJ) (holdsDuring (BeginFn (WhenFn ?CHANGE)) (possesses ?AGENT1 ?OBJ)) (holdsDuring (EndFn (WhenFn ?CHANGE)) (possesses ?AGENT2 ?OBJ))) (not (equal ?AGENT1 ?AGENT2)))").
:- load_kif("(=> (and (instance ?CHANGE ChangeOfPossession) (origin ?CHANGE ?AGENT1) (destination ?CHANGE ?AGENT2) (instance ?AGENT1 Agent) (instance ?AGENT2 Agent) (patient ?CHANGE ?OBJ)) (and (holdsDuring (BeginFn (WhenFn ?CHANGE)) (possesses ?AGENT1 ?OBJ)) (holdsDuring (EndFn (WhenFn ?CHANGE)) (possesses ?AGENT2 ?OBJ))))").
:- load_kif("(subclass Giving ChangeOfPossession)").
:- load_kif("(documentation Giving EnglishLanguage \"The &%subclass of &%ChangeOfPossession where the &%agent gives the &%destination something.\")").
:- load_kif("(=> (and (instance ?GIVE Giving) (agent ?GIVE ?AGENT1) (destination ?GIVE ?AGENT2) (instance ?AGENT2 Agent) (patient ?GIVE ?OBJ)) (exists (?GET) (and (instance ?GET Getting) (agent ?GET ?AGENT2) (origin ?GET ?AGENT1) (patient ?GET ?OBJ))))").
:- load_kif("(=> (and (instance ?GIVE Giving) (agent ?GIVE ?AGENT)) (origin ?GIVE ?AGENT))").
:- load_kif("(subclass Funding Giving)").
:- load_kif("(documentation Funding EnglishLanguage \"Any instance of &%Giving where the &%patient is an instance of &%Currency. Note that this class covers both financing, e.g. where a firm funds a software company with venture capital with the agreement that a certain percentage of the profits on the investment will be returned to the firm, and instances of &%UnilateralGiving, e.g. providing a tuition waiver and/or a stipend to a student as part of scholarship or fellowship.\")").
:- load_kif("(=> (instance ?FUND Funding) (exists (?MONEY) (and (instance ?MONEY Currency) (patient ?FUND ?MONEY))))").
:- load_kif("(subclass UnilateralGiving Giving)").
:- load_kif("(documentation UnilateralGiving EnglishLanguage \"Any instance of &%Giving that is not part of a &%Transaction. In other words, any instance of &%Giving where nothing is received in return. Some examples of &%UnilateralGiving are: honorary awards, gifts, and financial grants.\")").
:- load_kif("(=> (instance ?GIVE UnilateralGiving) (not (exists (?TRANS) (and (instance ?TRANS Transaction) (subProcess ?GIVE ?TRANS)))))").
:- load_kif("(subclass Lending Giving)").
:- load_kif("(documentation Lending EnglishLanguage \"The &%subclass of &%Giving &%Processes where the &%agent gives the &%destination something for a limited period of time with the expectation that it will be returned later (perhaps with interest).\")").
:- load_kif("(<=> (exists (?BORROW) (and (instance ?BORROW Borrowing) (agent ?BORROW ?AGENT1) (origin ?BORROW ?AGENT2) (patient ?BORROW ?OBJECT))) (exists (?LEND) (and (instance ?LEND Lending) (agent ?LEND ?AGENT2) (destination ?LEND ?AGENT1) (patient ?LEND ?OBJECT))))").
:- load_kif("(subclass GivingBack Giving)").
:- load_kif("(documentation GivingBack EnglishLanguage \"Any instance of &%Giving where the &%agent gives something to the &%destination which was previously given to the &%agent by the &%destination, e.g. returing a book that was borrowed from someone.\")").
:- load_kif("(=> (and (instance ?RETURN GivingBack) (agent ?RETURN ?AGENT) (destination ?RETURN ?DEST)) (exists (?GIVE) (and (instance ?GIVE Giving) (agent ?GIVE ?DEST) (destination ?GIVE ?AGENT) (earlier (WhenFn ?GIVE) (WhenFn ?RETURN)))))").
:- load_kif("(subclass Getting ChangeOfPossession)").
:- load_kif("(documentation Getting EnglishLanguage \"The &%subclass of &%ChangeOfPossession where the &%agent gets something. Note that the source from which something is obtained is specified with the &%origin &%CaseRole.\")").
:- load_kif("(=> (and (instance ?GET Getting) (agent ?GET ?AGENT)) (destination ?GET ?AGENT))").
:- load_kif("(subclass UnilateralGetting Getting)").
:- load_kif("(relatedInternalConcept UnilateralGetting UnilateralGiving)").
:- load_kif("(documentation UnilateralGetting EnglishLanguage \"Any instance of &%Getting that is not part of a &%Transaction. In other words, any instance of &%Getting where nothing is given in return. Some examples of &%UnilateralGetting are: appropriating, commandeering, stealing, etc.\")").
:- load_kif("(=> (instance ?GET UnilateralGetting) (not (exists (?TRANS) (and (instance ?TRANS Transaction) (subProcess ?GET ?TRANS)))))").
:- load_kif("(subclass Borrowing Getting)").
:- load_kif("(documentation Borrowing EnglishLanguage \"The &%subclass of &%Getting &%Processes where the &%agent gets something for a limited period of time with the expectation that it will be returned later (perhaps with interest).\")").
:- load_kif("(subclass Transaction ChangeOfPossession)").
:- load_kif("(subclass Transaction DualObjectProcess)").
:- load_kif("(documentation Transaction EnglishLanguage \"The &%subclass of &%ChangeOfPossession where something is exchanged for something else.\")").
:- load_kif("(=> (instance ?TRANS Transaction) (exists (?AGENT1 ?AGENT2 ?GIVE1 ?GIVE2 ?OBJ1 ?OBJ2) (and (instance ?GIVE1 Giving) (instance ?GIVE2 Giving) (subProcess ?GIVE1 ?TRANS) (subProcess ?GIVE2 ?TRANS) (agent ?GIVE1 ?AGENT1) (agent ?GIVE2 ?AGENT2) (patient ?GIVE1 ?OBJ1) (patient ?GIVE2 ?OBJ2) (destination ?GIVE1 ?AGENT2) (destination ?GIVE2 ?AGENT1) (not (equal ?AGENT1 ?AGENT2)) (not (equal ?OBJ1 ?OBJ2)))))").
:- load_kif("(subclass FinancialTransaction Transaction)").
:- load_kif("(documentation FinancialTransaction EnglishLanguage \"A &%Transaction where an instance of &%FinancialInstrument is the subject of the action, and often is exchanged for something else.\")").
:- load_kif("(=> (instance ?TRANS FinancialTransaction) (exists (?OBJ) (and (patient ?TRANS ?OBJ) (instance ?OBJ FinancialInstrument))))").
:- load_kif("(instance transactionAmount BinaryPredicate)").
:- load_kif("(instance transactionAmount SingleValuedRelation)").
:- load_kif("(instance transactionAmount TotalValuedRelation)").
:- load_kif("(domain transactionAmount 1 FinancialTransaction)").
:- load_kif("(domain transactionAmount 2 CurrencyMeasure)").
:- load_kif("(documentation transactionAmount EnglishLanguage \"(&%transactionAmount ?TRANSACTION ?AMOUNT) means that ?AMOUNT is an instance of &%CurrencyMeasure being exhanged in the &%FinancialTransaction ?TRANSACTION.\")").
:- load_kif("(=> (transactionAmount ?TRANS ?AMOUNT) (exists (?OBJ) (and (patient ?TRANS ?OBJ) (monetaryValue ?OBJ ?AMOUNT))))").
:- load_kif("(subclass ServiceProcess SocialInteraction)").
:- load_kif("(documentation ServiceProcess EnglishLanguage \"&%ServiceProcess denotes the class of events in which one agent performs a service for another. The service need not be commercial, and it need not be the case that the &%serviceRecipient pays or recompenses the &%serviceProvider for the service.\")").
:- load_kif("(subclass CommercialService FinancialTransaction)").
:- load_kif("(documentation CommercialService EnglishLanguage \"Any &%FinancialTransaction by a &%CommercialAgent where the aim is to produce a profit.\")").
:- load_kif("(subclass CommercialService ServiceProcess)").
:- load_kif("(=> (instance ?BUSINESS CommercialService) (exists (?AGENT) (and (instance ?AGENT CommercialAgent) (agent ?BUSINESS ?AGENT))))").
:- load_kif("(subclass Betting FinancialTransaction)").
:- load_kif("(documentation Betting EnglishLanguage \"A &%FinancialTransaction where an instance of &%CurrencyMeasure is exchanged for the possibility of winning a larger instance of &%CurrencyMeasure within the context of some sort of &%Game.\")").
:- load_kif("(subclass Buying FinancialTransaction)").
:- load_kif("(relatedInternalConcept Buying Selling)").
:- load_kif("(documentation Buying EnglishLanguage \"A &%FinancialTransaction in which an instance of &%CurrencyMeasure is exchanged for an instance of &%Physical.\")").
:- load_kif("(=> (and (instance ?BUY Buying) (agent ?BUY ?AGENT)) (destination ?BUY ?AGENT))").
:- load_kif("(subclass Selling FinancialTransaction)").
:- load_kif("(documentation Selling EnglishLanguage \"A &%FinancialTransaction in which an instance of &%Physical is exchanged for an instance of &%CurrencyMeasure.\")").
:- load_kif("(<=> (exists (?BUY) (and (instance ?BUY Buying) (agent ?BUY ?AGENT1) (origin ?BUY ?AGENT2) (patient ?BUY ?OBJECT))) (exists (?SELL) (and (instance ?SELL Selling) (agent ?SELL ?AGENT2) (destination ?SELL ?AGENT1) (patient ?SELL ?OBJECT))))").
:- load_kif("(=> (and (instance ?SELL Selling) (agent ?SELL ?AGENT)) (origin ?SELL ?AGENT))").
:- load_kif("(subclass Learning IntentionalPsychologicalProcess)").
:- load_kif("(documentation Learning EnglishLanguage \"The &%Class of &%Processes which relate to the acquisition of information.\")").
:- load_kif("(=> (and (instance ?LEARN Learning) (agent ?LEARN ?AGENT)) (instance ?AGENT CognitiveAgent))").
:- load_kif("(=> (and (instance ?LEARN Learning) (agent ?LEARN ?AGENT) (patient ?LEARN ?PROP)) (and (holdsDuring (BeginFn (WhenFn ?LEARN)) (not (knows ?AGENT ?PROP))) (holdsDuring (EndFn (WhenFn ?LEARN)) (knows ?AGENT ?PROP))))").
:- load_kif("(subclass Discovering IntentionalPsychologicalProcess)").
:- load_kif("(documentation Discovering EnglishLanguage \"Finding something that was sought. Note that this class is restricted to cases of discovering something &%Physical. For cases involving the acquisition of knowledge, the class &%Learning should be used.\")").
:- load_kif("(=> (and (instance ?DISCOVER Discovering) (patient ?DISCOVER ?OBJ)) (exists (?PURSUE) (and (instance ?PURSUE Pursuing) (meetsTemporally (WhenFn ?PURSUE) (WhenFn ?DISCOVER)))))").
:- load_kif("(=> (and (instance ?DISCOVER Discovering) (patient ?DISCOVER ?OBJ) (holdsDuring (WhenFn ?DISCOVER) (located ?OBJ ?PLACE))) (exists (?LEARN) (and (instance ?LEARN Learning) (subProcess ?LEARN ?DISCOVER) (patient ?LEARN (located ?OBJ ?PLACE)))))").
:- load_kif("(subclass Classifying IntentionalPsychologicalProcess)").
:- load_kif("(documentation Classifying EnglishLanguage \"The &%Class of &%IntentionalPsychologicalProcesses which involve attaching a name or category to a thing or set of things. Note that &%Classifying is distinguished from &%Learning by the fact that the latter covers the acquisition by a &%CognitiveAgent of any &%Proposition, while the former involves the assignment of a label or category.\")").
:- load_kif("(subclass Reasoning IntentionalPsychologicalProcess)").
:- load_kif("(documentation Reasoning EnglishLanguage \"The &%Class of &%IntentionalPsychologicalProcesses which involve concluding, on the basis of either deductive or inductive evidence, that a particular &%Proposition or &%Sentence is true.\")").
:- load_kif("(=> (instance ?AGENT CognitiveAgent) (capability Reasoning agent ?AGENT))").
:- load_kif("(subclass Selecting IntentionalPsychologicalProcess)").
:- load_kif("(documentation Selecting EnglishLanguage \"The &%Class of &%IntentionalPsychologicalProcesses which involve opting for one or more &%Entity out of a larger set of &%Entities. Note that this covers all cases of judging or evaluating.\")").
:- load_kif("(subclass Deciding Selecting)").
:- load_kif("(documentation Deciding EnglishLanguage \"The subclass of &%Selecting where the &%agent opts for one course of action out of a set of multiple possibilities that are open to him/her.\")").
:- load_kif("(=> (and (instance ?DECIDE Deciding) (agent ?DECIDE ?AGENT) (patient ?DECIDE ?PROCESS)) (and (instance ?PROCESS IntentionalProcess) (agent ?PROCESS ?AGENT)))").
:- load_kif("(subclass Voting Deciding)").
:- load_kif("(documentation Voting EnglishLanguage \"&%Voting is the activity of voting in an &%Election. Voting is typically done by individuals, while &%Elections are conducted by &%Organizations. The voting process by an individual voter is part of an &%Election process.\")").
:- load_kif("(=> (instance ?VOTE Voting) (exists (?ELECT) (and (instance ?ELECT Election) (subProcess ?VOTE ?ELECT))))").
:- load_kif("(subclass Judging Selecting)").
:- load_kif("(documentation Judging EnglishLanguage \"The subclass of &%Selecting where the &%agent opts for one belief out of a set of multiple possibilities that are available to him/her.\")").
:- load_kif("(=> (and (instance ?JUDGE Judging) (agent ?JUDGE ?AGENT) (patient ?JUDGE ?PROPOSITION)) (and (holdsDuring (BeginFn (WhenFn ?JUDGE)) (not (believes ?AGENT ?PROPOSITION))) (holdsDuring (EndFn (WhenFn ?JUDGE)) (believes ?AGENT ?PROPOSITION))))").
:- load_kif("(subclass Comparing IntentionalPsychologicalProcess)").
:- load_kif("(subclass Comparing DualObjectProcess)").
:- load_kif("(documentation Comparing EnglishLanguage \"The &%Class of &%IntentionalPsychologicalProcesses which involve comparing, relating, contrasting, etc. the properties of two or more &%Entities.\")").
:- load_kif("(subclass Calculating IntentionalPsychologicalProcess)").
:- load_kif("(documentation Calculating EnglishLanguage \"&%IntentionalPsychologicalProcesses which involve the consideration and/or manipulation of instances of &%Quantity.\")").
:- load_kif("(subclass Measuring Calculating)").
:- load_kif("(documentation Measuring EnglishLanguage \"The &%Class of &%Calculating &%Processes where the aim is to determine the &%PhysicalQuantity of some aspect of the &%patient.\")").
:- load_kif("(=> (and (instance ?MEAS Measuring) (agent ?MEAS ?AGENT) (patient ?MEAS ?OBJ)) (exists (?QUANT ?UNIT) (holdsDuring (EndFn (WhenFn ?MEAS)) (knows ?AGENT (measure ?OBJ (MeasureFn ?QUANT ?UNIT))))))").
:- load_kif("(subclass Counting Calculating)").
:- load_kif("(documentation Counting EnglishLanguage \"Enumerating something. The &%Class of &%Calculating &%Processes where the aim is to determine the &%Number corresponding to the &%patient.\")").
:- load_kif("(=> (and (instance ?COUNT Counting) (agent ?COUNT ?AGENT) (patient ?COUNT ?ENTITY)) (exists (?NUMBER) (knows ?AGENT (equal (CardinalityFn ?ENTITY) ?NUMBER))))").
:- load_kif("(subclass Predicting IntentionalPsychologicalProcess)").
:- load_kif("(documentation Predicting EnglishLanguage \"The &%Class of &%IntentionalPsychologicalProcesses which involve the formulation of a &%Proposition about a state of affairs which might be realized in the future.\")").
:- load_kif("(=> (and (instance ?PREDICT Predicting) (patient ?PREDICT ?FORMULA)) (exists (?TIME) (and (holdsDuring ?TIME ?FORMULA) (or (before ?TIME (WhenFn ?PREDICT)) (earlier ?TIME (WhenFn ?PREDICT))))))").
:- load_kif("(subclass Remembering PsychologicalProcess)").
:- load_kif("(documentation Remembering EnglishLanguage \"The &%Class of &%PsychologicalProcesses which involve the recollection of prior experiences and/or of knowledge which was previously acquired.\")").
:- load_kif("(=> (and (instance ?REMEMBER Remembering) (patient ?REMEMBER ?FORMULA)) (exists (?TIME) (and (holdsDuring ?TIME ?FORMULA) (or (before ?TIME (WhenFn ?REMEMBER)) (earlier ?TIME (WhenFn ?REMEMBER))))))").
:- load_kif("(subclass Keeping IntentionalProcess)").
:- load_kif("(documentation Keeping EnglishLanguage \"The &%Class of &%Processes where the &%agent keeps something in a particular location for an extended period of time.\")").
:- load_kif("(=> (and (instance ?KEEP Keeping) (agent ?KEEP ?AGENT) (patient ?KEEP ?OBJ)) (exists (?PUT) (and (instance ?PUT Putting) (agent ?PUT ?AGENT) (patient ?PUT ?OBJ) (earlier (WhenFn ?PUT) (WhenFn ?KEEP)))))").
:- load_kif("(=> (and (instance ?KEEP Keeping) (patient ?KEEP ?OBJ)) (exists (?PLACE) (forall (?TIME) (=> (temporalPart ?TIME (WhenFn ?KEEP)) (holdsDuring ?TIME (located ?OBJ ?PLACE))))))").
:- load_kif("(subclass Confining Keeping)").
:- load_kif("(documentation Confining EnglishLanguage \"The &%Class of &%Keeping &%Processes where the &%patient is a &%Human or an &%Animal and is kept involuntarily. This covers caging, imprisonment, jailing, etc.\")").
:- load_kif("(=> (instance ?CONFINE Confining) (exists (?AGENT) (and (instance ?AGENT Animal) (patient ?CONFINE ?AGENT))))").
:- load_kif("(=> (and (instance ?CONFINE Confining) (patient ?CONFINE ?PERSON) (instance ?PERSON Human)) (not (desires ?PERSON (patient ?CONFINE ?PERSON))))").
:- load_kif("(subclass Maintaining IntentionalProcess)").
:- load_kif("(documentation Maintaining EnglishLanguage \"The &%Class of &%Processes where the &%agent cares for or maintains the &%Object.\")").
:- load_kif("(subclass Repairing IntentionalProcess)").
:- load_kif("(relatedInternalConcept Repairing Maintaining)").
:- load_kif("(documentation Repairing EnglishLanguage \"The &%Class of &%Processes where the &%agent makes a modification or series of modifications to an &%Object that is not functioning as intended so that it works properly.\")").
:- load_kif("(=> (and (instance ?REPAIR Repairing) (patient ?REPAIR ?OBJ)) (exists (?DAMAGE) (and (instance ?DAMAGE Damaging) (patient ?DAMAGE ?OBJ) (earlier (WhenFn ?DAMAGE) (WhenFn ?REPAIR)))))").
:- load_kif("(subclass TherapeuticProcess Repairing)").
:- load_kif("(documentation TherapeuticProcess EnglishLanguage \"A &%Process that is carried out for the purpose of curing, improving or reducing the pain associated with a &%DiseaseOrSyndrome.\")").
:- load_kif("(=> (and (instance ?PROC TherapeuticProcess) (patient ?PROC ?BIO)) (or (instance ?BIO Organism) (exists (?ORG) (and (instance ?ORG Organism) (part ?BIO ?ORG)))))").
:- load_kif("(subclass Surgery TherapeuticProcess)").
:- load_kif("(documentation Surgery EnglishLanguage \"Any &%TherapeuticProcess that involves making an incision in the &%Animal that is the &%patient of the &%TherapeuticProcess.\")").
:- load_kif("(=> (and (instance ?ACT Surgery) (patient ?ACT ?ANIMAL)) (exists (?SUBACT) (and (instance ?SUBACT Cutting) (instance ?ANIMAL Animal) (patient ?SUBACT ?ANIMAL) (subProcess ?SUBACT ?ACT))))").
:- load_kif("(subclass Damaging InternalChange)").
:- load_kif("(disjoint Damaging Repairing)").
:- load_kif("(documentation Damaging EnglishLanguage \"The &%Class of &%Processes where the &%agent brings about a situation where the &%patient no longer functions normally or as intended.\")").
:- load_kif("(subclass Destruction Damaging)").
:- load_kif("(documentation Destruction EnglishLanguage \"The &%subclass of &%Damagings in which the &%patient (or an essential element of the &%patient) is destroyed. Note that the difference between this concept and its superclass is solely one of extent.\")").
:- load_kif("(<=> (instance ?PROCESS Destruction) (exists (?PATIENT) (and (patient ?PROCESS ?PATIENT) (time ?PATIENT (BeginFn (WhenFn ?PROCESS))) (not (time ?PATIENT (EndFn (WhenFn ?PROCESS)))))))").
:- load_kif("(subclass Killing Destruction)").
:- load_kif("(documentation Killing EnglishLanguage \"The &%subclass of &%Destruction in which the death of an &%Organism is caused by an &%Organism. Note that in cases of suicide the &%Organism would be the same in both cases.\")").
:- load_kif("(=> (and (instance ?KILL Killing) (agent ?KILL ?AGENT) (patient ?KILL ?PATIENT)) (and (instance ?AGENT Organism) (instance ?PATIENT Organism)))").
:- load_kif("(=> (and (instance ?KILL Killing) (patient ?KILL ?PATIENT)) (and (holdsDuring (BeginFn (WhenFn ?KILL)) (attribute ?PATIENT Living)) (holdsDuring (FutureFn (WhenFn ?KILL)) (attribute ?PATIENT Dead))))").
:- load_kif("(=> (and (instance ?KILL Killing) (patient ?KILL ?OBJ)) (exists (?DEATH) (and (instance ?DEATH Death) (experiencer ?DEATH ?OBJ) (causes ?KILL ?DEATH))))").
:- load_kif("(subclass Poking IntentionalProcess)").
:- load_kif("(documentation Poking EnglishLanguage \"The &%Class of &%Processes where the &%agent pierces the surface of the &%Object with an &%instrument.\")").
:- load_kif("(=> (and (instance ?POKE Poking) (agent ?POKE ?AGENT) (patient ?POKE ?OBJ) (instrument ?POKE ?INST)) (holdsDuring (WhenFn ?POKE) (connects ?INST ?AGENT ?OBJ)))").
:- load_kif("(subclass Cutting Poking)").
:- load_kif("(documentation Cutting EnglishLanguage \"The &%subclass of &%Poking &%Processes which involve a sharp &%instrument.\")").
:- load_kif("(subclass Attaching DualObjectProcess)").
:- load_kif("(disjoint Attaching Detaching)").
:- load_kif("(relatedInternalConcept Attaching Putting)").
:- load_kif("(documentation Attaching EnglishLanguage \"A &%Process where one &%Object becomes attached to another &%Object. Note that this differs from &%Putting in that two things which are attached may already be in the same location. Note that &%Combining is different from &%Attaching in that the former applies to &%Substances, while the latter applies to &%CorpuscularObjects. Note too that &%Attaching is different from &%Putting in that one or both of the two things which are attached may or may not be moved from the location where they were combined.\")").
:- load_kif("(=> (and (instance ?ATTACH Attaching) (patient ?ATTACH ?OBJ1) (patient ?ATTACH ?OBJ2)) (and (holdsDuring (BeginFn (WhenFn ?ATTACH)) (not (connected ?OBJ1 ?OBJ2))) (holdsDuring (EndFn (WhenFn ?ATTACH)) (connected ?OBJ1 ?OBJ2))))").
:- load_kif("(=> (instance ?ATTACH Attaching) (exists (?OBJ) (and (instance ?OBJ CorpuscularObject) (patient ?ATTACH ?OBJ))))").
:- load_kif("(subclass Detaching DualObjectProcess)").
:- load_kif("(documentation Detaching EnglishLanguage \"A &%Process where the &%agent detaches one thing from something else. Note that &%Detaching is different from &%Separating in that the latter applies to &%Substances, while the former applies to &%CorpuscularObjects. Note too that &%Detaching is different from &%Removing in that one or both of the two things which are detached may or may not be moved from the location where they were attached.\")").
:- load_kif("(=> (and (instance ?DETACH Detaching) (patient ?DETACH ?OBJ1) (patient ?DETACH ?OBJ2)) (and (holdsDuring (BeginFn (WhenFn ?DETACH)) (connected ?OBJ1 ?OBJ2)) (holdsDuring (EndFn (WhenFn ?DETACH)) (not (connected ?OBJ1 ?OBJ2)))))").
:- load_kif("(=> (instance ?DETACH Detaching) (exists (?OBJ) (and (instance ?OBJ CorpuscularObject) (patient ?DETACH ?OBJ))))").
:- load_kif("(subclass Ungrasping Detaching)").
:- load_kif("(documentation Ungrasping EnglishLanguage \"Any instance of &%Detaching which results in a situation where it is not the case that the &%agent &%grasps something which he/she &%grasps previously.\")").
:- load_kif("(=> (and (instance ?RELEASE Ungrasping) (agent ?GRAB ?AGENT) (patient ?GRAB ?THING)) (and (holdsDuring (BeginFn (WhenFn ?RELEASE)) (grasps ?AGENT ?THING)) (holdsDuring (EndFn (WhenFn ?RELEASE)) (not (grasps ?AGENT ?THING)))))").
:- load_kif("(subclass Combining DualObjectProcess)").
:- load_kif("(documentation Combining EnglishLanguage \"A &%Process where two or more &%SelfConnectedObjects are incorporated into a single &%SelfConnectedObject. Note that &%Combining is different from &%Attaching in that the former results in one of the objects being &%part of the other, while &%Attaching only results in the two objects being &%connected with one another. Note too that &%Combining is different from &%Putting in that one or both of the two things which are combined may or may not be moved from the location where they were combined.\")").
:- load_kif("(<=> (and (instance ?COMBINE Combining) (resource ?COMBINE ?OBJ1) (result ?COMBINE ?OBJ2)) (and (holdsDuring (BeginFn (WhenFn ?COMBINE)) (not (part ?OBJ1 ?OBJ2))) (holdsDuring (EndFn (WhenFn ?COMBINE)) (part ?OBJ1 ?OBJ2))))").
:- load_kif("(=> (instance ?COMBINE Combining) (exists (?OBJ) (and (instance ?OBJ SelfConnectedObject) (patient ?COMBINE ?OBJ))))").
:- load_kif("(subclass Separating DualObjectProcess)").
:- load_kif("(disjoint Separating Combining)").
:- load_kif("(documentation Separating EnglishLanguage \"A &%Process where a &%SelfConnectedObject is separated into (some of) its &%parts. Note that &%Separating is different from &%Detaching in that the latter only results in the two objects not being &%connected. Note too that &%Separating is different from &%Removing in that one or both of the two things which are separated may or may not be moved from the location where they were separated.\")").
:- load_kif("(=> (instance ?SEPARATE Separating) (exists (?OBJ) (and (instance ?OBJ SelfConnectedObject) (patient ?SEPARATE ?OBJ))))").
:- load_kif("(subclass ChemicalProcess InternalChange)").
:- load_kif("(partition ChemicalProcess ChemicalSynthesis ChemicalDecomposition)").
:- load_kif("(documentation ChemicalProcess EnglishLanguage \"A &%ChemicalProcess occurs whenever chemical compounds (&%CompoundSubstances) are formed or decomposed. For example, reactants disappear as chemical change occurs, and products appear as chemical change occurs. In a chemical change a chemical reaction takes place. Catalysts in a &%ChemicalProcess may speed up the reaction, but aren't themselves produced or consumed. Examples: rusting of iron and the decomposition of water, induced by an electric current, to gaseous hydrogen and gaseous oxygen.\")").
:- load_kif("(=> (and (instance ?PROC ChemicalProcess) (or (resource ?PROC ?STUFF) (result ?PROC ?STUFF))) (instance ?STUFF PureSubstance))").
:- load_kif("(subclass ChemicalSynthesis ChemicalProcess)").
:- load_kif("(subclass ChemicalSynthesis Combining)").
:- load_kif("(documentation ChemicalSynthesis EnglishLanguage \"The &%Class of &%ChemicalProcesses in which a &%CompoundSubstance is formed from simpler reactants.\")").
:- load_kif("(=> (and (resource ?PROC ?SUBSTANCE1) (result ?PROC ?SUBSTANCE2) (instance ?SUBSTANCE1 ElementalSubstance) (instance ?SUBSTANCE2 CompoundSubstance)) (instance ?PROC ChemicalSynthesis))").
:- load_kif("(<=> (instance ?COMPOUND CompoundSubstance) (exists (?ELEMENT1 ?ELEMENT2 ?PROCESS) (and (instance ?ELEMENT1 ElementalSubstance) (instance ?ELEMENT2 ElementalSubstance) (not (equal ?ELEMENT1 ?ELEMENT2)) (instance ?PROCESS ChemicalSynthesis) (resource ?PROCESS ?ELEMENT1) (resource ?PROCESS ?ELEMENT2) (result ?PROCESS ?COMPOUND))))").
:- load_kif("(subclass ChemicalDecomposition ChemicalProcess)").
:- load_kif("(subclass ChemicalDecomposition Separating)").
:- load_kif("(documentation ChemicalDecomposition EnglishLanguage \"The &%Class of &%ChemicalProcesses in which a &%CompoundSubstance breaks down into simpler products.\")").
:- load_kif("(=> (and (resource ?PROC ?SUBSTANCE1) (result ?PROC ?SUBSTANCE2) (instance ?SUBSTANCE1 CompoundSubstance) (instance ?SUBSTANCE2 ElementalSubstance)) (instance ?PROC ChemicalDecomposition))").
:- load_kif("(subclass Combustion ChemicalDecomposition)").
:- load_kif("(documentation Combustion EnglishLanguage \"The &%Class of &%ChemicalProcesses in which an &%Object reacts with oxygen and gives off heat. This includes all &%Processes in which something is burning.\")").
:- load_kif("(=> (instance ?COMBUSTION Combustion) (exists (?HEAT ?LIGHT) (and (instance ?HEAT Heating) (instance ?LIGHT RadiatingLight) (subProcess ?HEAT ?COMBUSTION) (subProcess ?LIGHT ?COMBUSTION))))").
:- load_kif("(instance Flammable PhysicalAttribute)").
:- load_kif("(documentation Flammable EnglishLanguage \"The &%Attribute of being flammable at normal temperatures").
:- load_kif("(i.e. not while a &%Plasma).\")").
:- load_kif("(=> (attribute ?X Flammable) (capability Combustion patient ?X))").
:- load_kif("(subclass InternalChange Process)").
:- load_kif("(documentation InternalChange EnglishLanguage \"&%Processes which involve altering an internal property of an &%Object, e.g. the shape of the &%Object, its coloring, its structure, etc. &%Processes that are not instances of this class include changes that only affect the relationship to other objects, e.g. changes in spatial or temporal location.\")").
:- load_kif("(=> (and (instance ?CHANGE InternalChange) (patient ?CHANGE ?OBJ)) (exists (?PROPERTY) (or (and (holdsDuring (BeginFn (WhenFn ?CHANGE)) (attribute ?OBJ ?PROPERTY)) (holdsDuring (EndFn (WhenFn ?CHANGE)) (not (attribute ?OBJ ?PROPERTY)))) (and (holdsDuring (BeginFn (WhenFn ?CHANGE)) (not (attribute ?OBJ ?PROPERTY))) (holdsDuring (EndFn (WhenFn ?CHANGE)) (attribute ?OBJ ?PROPERTY))))))").
:- load_kif("(subclass SurfaceChange InternalChange)").
:- load_kif("(documentation SurfaceChange EnglishLanguage \"&%Processes which involve altering the properties that apply to the surface of an &%Object.\")").
:- load_kif("(=> (and (instance ?ALT SurfaceChange) (patient ?ALT ?OBJ)) (exists (?PART ?PROPERTY) (and (superficialPart ?PART ?OBJ) (or (and (holdsDuring (BeginFn (WhenFn ?ALT)) (attribute ?PART ?PROPERTY)) (holdsDuring (EndFn (WhenFn ?ALT)) (not (attribute ?PART ?PROPERTY)))) (and (holdsDuring (BeginFn (WhenFn ?ALT)) (not (attribute ?PART ?PROPERTY))) (holdsDuring (EndFn (WhenFn ?ALT)) (attribute ?PART ?PROPERTY)))))))").
:- load_kif("(subclass Coloring SurfaceChange)").
:- load_kif("(documentation Coloring EnglishLanguage \"The &%subclass of &%SurfaceChange where a &%ColorAttribute of the &%patient is altered. Note that the change in color may apply to just part of the object.\")").
:- load_kif("(=> (and (instance ?COLORING Coloring) (patient ?COLORING ?OBJ)) (exists (?PROPERTY ?PART) (and (part ?PART ?OBJ) (instance ?PROPERTY ColorAttribute) (or (and (holdsDuring (BeginFn (WhenFn ?COLORING)) (attribute ?PART ?PROPERTY)) (holdsDuring (EndFn (WhenFn ?COLORING)) (not (attribute ?PART ?PROPERTY)))) (and (holdsDuring (BeginFn (WhenFn ?COLORING)) (not (attribute ?PART ?PROPERTY))) (holdsDuring (EndFn (WhenFn ?COLORING)) (attribute ?PART ?PROPERTY)))))))").
:- load_kif("(subclass ShapeChange InternalChange)").
:- load_kif("(documentation ShapeChange EnglishLanguage \"The &%Process of changing the shape of an &%Object.\")").
:- load_kif("(=> (and (instance ?ALT ShapeChange) (patient ?ALT ?OBJ)) (exists (?PROPERTY) (and (instance ?PROPERTY ShapeAttribute) (or (and (holdsDuring (BeginFn (WhenFn ?ALT)) (attribute ?OBJ ?PROPERTY)) (holdsDuring (EndFn (WhenFn ?ALT)) (not (attribute ?OBJ ?PROPERTY)))) (and (holdsDuring (BeginFn (WhenFn ?ALT)) (not (attribute ?OBJ ?PROPERTY))) (holdsDuring (EndFn (WhenFn ?ALT)) (attribute ?OBJ ?PROPERTY)))))))").
:- load_kif("(subclass ContentDevelopment IntentionalProcess)").
:- load_kif("(documentation ContentDevelopment EnglishLanguage \"A &%subclass of &%IntentionalProcess in which content is modified, its form is altered or it is created anew.\")").
:- load_kif("(=> (instance ?DEVELOP ContentDevelopment) (exists (?OBJ) (and (instance ?OBJ ContentBearingObject) (result ?DEVELOP ?OBJ))))").
:- load_kif("(subclass Reading ContentDevelopment)").
:- load_kif("(relatedInternalConcept Reading Interpreting)").
:- load_kif("(documentation Reading EnglishLanguage \"A &%subclass of &%ContentDevelopment in which content is converted from a written form into a spoken representation. Note that the class &%Interpreting should be used in cases where a &%Text is read silently.\")").
:- load_kif("(=> (instance ?READ Reading) (exists (?TEXT ?PROP) (and (instance ?TEXT Text) (containsInformation ?TEXT ?PROP) (realization ?READ ?PROP))))").
:- load_kif("(subclass Writing ContentDevelopment)").
:- load_kif("(documentation Writing EnglishLanguage \"A &%subclass of &%ContentDevelopment in which content is converted from one form (e.g. uttered, written or represented mentally) into a written form. Note that this class covers both transcription and original creation of written &%Texts.\")").
:- load_kif("(subclass Encoding Writing)").
:- load_kif("(documentation Encoding EnglishLanguage \"Converting a document or message into a formal language or into a code that can be understood only by a relatively small body of &%Agents. Generally speaking, this hinders wide dissemination of the content in the original document or message.\")").
:- load_kif("(subclass Decoding Writing)").
:- load_kif("(disjoint Decoding Encoding)").
:- load_kif("(documentation Decoding EnglishLanguage \"Converting a document or message that has previously been encoded (see &%Encoding) into a &%Language that can be understood by a relatively large number of speakers.\")").
:- load_kif("(=> (and (instance ?DECODE Decoding) (patient ?DECODE ?DOC1)) (exists (?ENCODE ?DOC2 ?TIME) (and (containsInformation ?DOC2 ?PROP) (containsInformation ?DOC1 ?PROP) (temporalPart ?TIME (PastFn (WhenFn ?DECODE))) (holdsDuring ?TIME (and (instance ?ENCODE Encoding) (patient ?ENCODE ?DOC2))))))").
:- load_kif("(subclass Translating ContentDevelopment)").
:- load_kif("(subclass Translating DualObjectProcess)").
:- load_kif("(documentation Translating EnglishLanguage \"Converting content from one &%Language into another. This covers oral translation (i.e. interpreting) as well as written translation.\")").
:- load_kif("(=> (and (instance ?TRANSLATE Translating) (patient ?TRANSLATE ?EXPRESSION1) (result ?TRANSLATE ?EXPRESSION2)) (exists (?LANGUAGE1 ?LANGUAGE2 ?ENTITY) (and (representsInLanguage ?EXPRESSION1 ?ENTITY ?LANGUAGE1) (representsInLanguage ?EXPRESSION2 ?ENTITY ?LANGUAGE2) (not (equal ?LANGUAGE1 ?LANGUAGE2)))))").
:- load_kif("(subclass Wetting Putting)").
:- load_kif("(documentation Wetting EnglishLanguage \"The &%Class of &%Processes where a &%Liquid is added to an &%Object.\")").
:- load_kif("(=> (instance ?WET Wetting) (exists (?OBJ) (and (patient ?WET ?OBJ) (holdsDuring (BeginFn (WhenFn ?WET)) (not (attribute ?OBJ Damp))) (holdsDuring (EndFn (WhenFn ?WET)) (attribute ?OBJ Damp)))))").
:- load_kif("(=> (instance ?WET Wetting) (exists (?OBJ) (and (attribute ?OBJ Liquid) (patient ?WET ?OBJ))))").
:- load_kif("(subclass Drying Removing)").
:- load_kif("(documentation Drying EnglishLanguage \"The &%Class of &%Processes where a &%Liquid is removed from an &%Object.\")").
:- load_kif("(=> (and (instance ?DRY Drying) (patient ?DRY ?OBJ)) (holdsDuring (EndFn (WhenFn ?DRY)) (attribute ?OBJ Dry)))").
:- load_kif("(subclass Creation InternalChange)").
:- load_kif("(relatedInternalConcept Creation Destruction)").
:- load_kif("(documentation Creation EnglishLanguage \"The &%subclass of &%Process in which something is created. Note that the thing created is specified with the &%result &%CaseRole.\")").
:- load_kif("(=> (instance ?ACTION Creation) (exists (?RESULT) (result ?ACTION ?RESULT)))").
:- load_kif("(<=> (instance ?PROCESS Creation) (exists (?PATIENT) (and (patient ?PROCESS ?PATIENT) (time ?PATIENT (EndFn (WhenFn ?PROCESS))) (not (time ?PATIENT (BeginFn (WhenFn ?PROCESS)))))))").
:- load_kif("(subclass Making Creation)").
:- load_kif("(subclass Making IntentionalProcess)").
:- load_kif("(documentation Making EnglishLanguage \"The &%subclass of &%Creation in which an individual &%Artifact or a type of &%Artifact is made.\")").
:- load_kif("(subclass Constructing Making)").
:- load_kif("(documentation Constructing EnglishLanguage \"The &%subclass of &%Making in which a &%StationaryArtifact is built.\")").
:- load_kif("(<=> (exists (?BUILD) (and (instance ?BUILD Constructing) (result ?BUILD ?ARTIFACT))) (instance ?ARTIFACT StationaryArtifact))").
:- load_kif("(subclass Manufacture Making)").
:- load_kif("(documentation Manufacture EnglishLanguage \"The &%Making of &%Artifacts on a mass scale.\")").
:- load_kif("(subclass Publication Manufacture)").
:- load_kif("(subclass Publication ContentDevelopment)").
:- load_kif("(documentation Publication EnglishLanguage \"The &%Manufacture of &%Texts. Note that there is no implication that the &%Texts are distributed. Such distribution, when it occurs, is an instance of &%Dissemination.\")").
:- load_kif("(=> (and (instance ?PUB Publication) (patient ?PUB ?TEXT)) (subclass ?TEXT Text))").
:- load_kif("(subclass Cooking Making)").
:- load_kif("(documentation Cooking EnglishLanguage \"The &%Making of an &%instance of &%Food. Note that this can cover any preparation of &%Food, e.g. making a salad, cutting up fruit, etc. It does not necessarily involve the application of heat.\")").
:- load_kif("(=> (instance ?COOK Cooking) (exists (?FOOD) (and (instance ?FOOD (FoodForFn Organism)) (result ?COOK ?FOOD))))").
:- load_kif("(subclass Pursuing IntentionalProcess)").
:- load_kif("(documentation Pursuing EnglishLanguage \"The class of &%IntentionalProcesses where something is sought. Some examples would be hunting, shopping, trawling, and stalking.\")").
:- load_kif("(=> (instance ?PURSUE Pursuing) (exists (?OBJ) (and (instance ?OBJ Object) (patient ?PURSUE ?OBJ))))").
:- load_kif("(=> (and (instance ?PURSUE Pursuing) (agent ?PURSUE ?AGENT) (patient ?PURSUE ?OBJ)) (holdsDuring ?PURSUE (wants ?AGENT ?OBJ)))").
:- load_kif("(=> (and (instance ?PURSUE Pursuing) (agent ?PURSUE ?AGENT) (patient ?PURSUE ?OBJ)) (holdsDuring ?PURSUE (not (possesses ?AGENT ?OBJ))))").
:- load_kif("(subclass Hunting Pursuing)").
:- load_kif("(documentation Hunting EnglishLanguage \"Hunting is the class of &%Processes in which an animal or animals are pursued and sometimes captured and/or killed.\")").
:- load_kif("(=> (instance ?H Hunting) (exists (?T) (and (instance ?T Animal) (patient ?H ?T))))").
:- load_kif("(=> (and (instance ?H Hunting) (patient ?H ?P)) (hasPurpose ?H (exists (?PROC) (and (patient ?PROC ?P) (or (instance ?PROC Confining) (instance ?PROC Killing))))))").
:- load_kif("(subclass Investigating IntentionalPsychologicalProcess)").
:- load_kif("(documentation Investigating EnglishLanguage \"The class of &%IntentionalPsychologicalProcesses where the &%agent attempts to obtaina information (i.e. a &%Proposition denoted by a &%Formula).\")").
:- load_kif("(=> (and (instance ?INVESTIGATE Investigating) (patient ?INVESTIGATE ?PROP)) (instance ?PROP Formula))").
:- load_kif("(=> (and (instance ?INVESTIGATE Investigating) (agent ?INVESTIGATE ?AGENT) (patient ?INVESTIGATE ?PROP)) (holdsDuring (WhenFn ?INVESTIGATE) (not (knows ?AGENT ?PROP))))").
:- load_kif("(subclass Experimenting Investigating)").
:- load_kif("(documentation Experimenting EnglishLanguage \"&%Investigating the truth of a &%Proposition by constructing and observing a trial. Note that the trial may be either controlled or uncontrolled, blind or not blind.\")").
:- load_kif("(subclass DiagnosticProcess Investigating)").
:- load_kif("(documentation DiagnosticProcess EnglishLanguage \"A &%Process that is carried out for the purpose of determining the nature of a &%DiseaseOrSyndrome.\")").
:- load_kif("(=> (and (instance ?PROC DiagnosticProcess) (agent ?PROC ?AGENT)) (exists (?CAUSE) (hasPurposeForAgent ?PROC (knows ?AGENT (causes ?CAUSE ?PROC)) ?AGENT)))").
:- load_kif("(subclass SocialInteraction IntentionalProcess)").
:- load_kif("(documentation SocialInteraction EnglishLanguage \"The &%subclass of &%IntentionalProcess that involves interactions between &%CognitiveAgents.\")").
:- load_kif("(=> (instance ?INTERACTION SocialInteraction) (exists (?AGENT1 ?AGENT2) (and (involvedInEvent ?INTERACTION ?AGENT1) (involvedInEvent ?INTERACTION ?AGENT2) (instance ?AGENT1 Agent) (instance ?AGENT2 Agent) (not (equal ?AGENT1 ?AGENT2)))))").
:- load_kif("(subclass Pretending SocialInteraction)").
:- load_kif("(documentation Pretending EnglishLanguage \"Any &%SocialInteraction where a &%CognitiveAgent or &%Group of &%CognitiveAgents attempts to make another &%CognitiveAgent or &%Group of &%CognitiveAgents believe something that is false. This covers deceit, affectation, impersonation, and entertainment productions, to give just a few examples.\")").
:- load_kif("(=> (instance ?PRETEND Pretending) (exists (?PERSON ?PROP) (and (hasPurpose ?PRETEND (believes ?PERSON ?PROP)) (truth ?PROP False))))").
:- load_kif("(subclass Communication SocialInteraction)").
:- load_kif("(subclass Communication ContentBearingProcess)").
:- load_kif("(partition Communication Stating Supposing Directing Committing Expressing Declaring)").
:- load_kif("(relatedInternalConcept Communication ContentDevelopment)").
:- load_kif("(documentation Communication EnglishLanguage \"A &%SocialInteraction that involves the transfer of information between two or more &%CognitiveAgents. Note that &%Communication is closely related to, but essentially different from, &%ContentDevelopment. The latter involves the creation or modification of a &%ContentBearingObject, while &%Communication is the transfer of information for the purpose of conveying a message.\")").
:- load_kif("(=> (instance ?COMMUNICATE Communication) (exists (?PHYS ?ENTITY ?AGENT1 ?AGENT2) (and (refers ?PHYS ?ENTITY) (patient ?COMMUNICATE ?PHYS) (instance ?AGENT1 CognitiveAgent) (agent ?COMMUNICATE ?AGENT1) (instance ?AGENT2 CognitiveAgent) (destination ?COMMUNICATE ?AGENT2))))").
:- load_kif("(subclass Disseminating Communication)").
:- load_kif("(documentation Disseminating EnglishLanguage \"Any &%Communication that involves a single &%agent and many &%destinations. This covers the release of a published book, broadcasting, a theatrical performance, giving orders to assembled troops, delivering a public lecture, etc.\")").
:- load_kif("(=> (instance ?DISSEMINATE Disseminating) (exists (?AGENT1 ?AGENT2) (and (destination ?DISSEMINATE ?AGENT1) (instance ?AGENT1 CognitiveAgent) (destination ?DISSEMINATE ?AGENT2) (instance ?AGENT2 CognitiveAgent) (not (equal ?AGENT1 ?AGENT2)))))").
:- load_kif("(subclass Demonstrating Disseminating)").
:- load_kif("(documentation Demonstrating EnglishLanguage \"Exhibiting something or a range of things before the public in a particular location. This would cover software demos, theatrical plays, lectures, dance and music recitals, museum exhibitions, etc.\")").
:- load_kif("(=> (instance ?DEMO Demonstrating) (exists (?PERSON) (attends ?DEMO ?PERSON)))").
:- load_kif("(subrelation attends experiencer)").
:- load_kif("(domain attends 1 Demonstrating)").
:- load_kif("(domain attends 2 Human)").
:- load_kif("(documentation attends EnglishLanguage \"(&%attends ?DEMO ?PERSON) means that ?PERSON attends, i.e. is a member of the audience, of the performance event ?DEMO.\")").
:- load_kif("(subclass Gesture Communication)").
:- load_kif("(subclass Gesture BodyMotion)").
:- load_kif("(documentation Gesture EnglishLanguage \"Any &%BodyMotion, e.g. a hand wave, a nod of the head, a smile, which is also an instance of &%Communication.\")").
:- load_kif("(subclass Advertising Disseminating)").
:- load_kif("(documentation Advertising EnglishLanguage \"A &%Disseminating whose purpose is to promote the sale of an &%Object represented in a &%Text or &%Icon").
:- load_kif("(the advertisement).\")").
:- load_kif("(=> (instance ?ADVERT Advertising) (exists (?OBJ) (and (refers ?ADVERT ?OBJ) (hasPurpose ?ADVERT (exists (?SALE) (and (instance ?SALE Selling) (patient ?SALE ?OBJ)))))))").
:- load_kif("(subclass Expressing Communication)").
:- load_kif("(documentation Expressing EnglishLanguage \"Instances of this &%Class express a state of the &%agent. For example, Jane thanked Barbara for the present she had given her. The thanking in this case expresses the gratitude of Jane towards Barbara. Note that &%Expressing, unlike the other speech act types, is not a subclass of &%LinguisticCommunication. This is because emotions, for example, can be expressed without language, e.g. by smiling.\")").
:- load_kif("(=> (and (instance ?EXPRESS Expressing) (agent ?EXPRESS ?AGENT)) (exists (?STATE) (and (instance ?STATE StateOfMind) (attribute ?AGENT ?STATE) (represents ?EXPRESS ?STATE))))").
:- load_kif("(subclass LinguisticCommunication Communication)").
:- load_kif("(documentation LinguisticCommunication EnglishLanguage \"A &%Communication that involves the transfer of information via a &%LinguisticExpression.\")").
:- load_kif("(=> (instance ?COMMUNICATE LinguisticCommunication) (exists (?OBJ) (and (represents ?COMMUNICATE ?OBJ) (instance ?OBJ LinguisticExpression) (patient ?COMMUNICATE ?OBJ))))").
:- load_kif("(subclass Stating LinguisticCommunication)").
:- load_kif("(documentation Stating EnglishLanguage \"Instances of this &%Class commit the &%agent to some truth. For example, John claimed that the moon is made of green cheese.\")").
:- load_kif("(=> (and (instance ?STATE Stating) (agent ?STATE ?AGENT) (patient ?STATE ?FORMULA) (instance ?FORMULA Formula)) (holdsDuring (WhenFn ?STATE) (believes ?AGENT ?FORMULA)))").
:- load_kif("(subclass Disagreeing Stating)").
:- load_kif("(documentation Disagreeing EnglishLanguage \"A &%Stating in which two &%Agents have contradictory statements. This is distinguished from &%Arguing in that the statement in dispute may be a simple assertion, rather than a chain of deduction, and that two entities must be disagreeing with each other, whereas a single entity may craft an argument for a given point of view, without the need for another agent to disagree with.\")").
:- load_kif("(=> (instance ?DIS Disagreeing) (exists (?A1 ?A2 ?STATE1 ?STATE2 ?STMT1 ?STMT2) (and (subProcess ?STATE1 ?DIS) (subProcess ?STATE2 ?DIS) (agent ?STATE1 ?A1) (agent ?STATE2 ?A2) (not (equal ?A1 ?A2)) (containsInformation ?STATE1 ?STMT1) (containsInformation ?STATE2 ?STMT2) (not (consistent ?STMT1 ?STMT2)))))").
:- load_kif("(subclass Supposing LinguisticCommunication)").
:- load_kif("(documentation Supposing EnglishLanguage \"Instances of this &%Class suppose, for the sake of argument, that a proposition is true. For example, John considered what he would do if he won the lottery.\")").
:- load_kif("(subclass Directing LinguisticCommunication)").
:- load_kif("(documentation Directing EnglishLanguage \"Instances of this &%Class urge some further action among the receivers. A &%Directing can be an &%Ordering, a &%Requesting or a &%Questioning.\")").
:- load_kif("(subclass Ordering Directing)").
:- load_kif("(documentation Ordering EnglishLanguage \"A &%Directing in which the receiver is commanded to realize the content of a &%ContentBearingObject. Orders are injunctions, the disobedience of which involves sanctions, or which express an obligation upon the part of the orderee.\")").
:- load_kif("(=> (and (instance ?ORDER Ordering) (patient ?ORDER ?FORMULA)) (modalAttribute ?FORMULA Obligation))").
:- load_kif("(subclass Requesting Directing)").
:- load_kif("(documentation Requesting EnglishLanguage \"A request expresses a desire that some future action be performed. For example, the 5th Battalion requested air support from the 3rd Bomber Group. Note that this class covers proposals, recommendations, suggestions, etc.\")").
:- load_kif("(=> (and (instance ?REQUEST Requesting) (agent ?REQUEST ?AGENT) (patient ?REQUEST ?FORMULA) (instance ?FORMULA Formula)) (desires ?AGENT ?FORMULA))").
:- load_kif("(subclass Questioning Directing)").
:- load_kif("(documentation Questioning EnglishLanguage \"A request for information. For example, John asked bill if the President had said anything about taxes in his State of the Union address.\")").
:- load_kif("(=> (and (instance ?QUESTION Questioning) (agent ?QUESTION ?AGENT) (patient ?QUESTION ?FORMULA) (instance ?FORMULA Formula)) (holdsDuring (WhenFn ?QUESTION) (not (knows ?AGENT ?FORMULA))))").
:- load_kif("(subclass Committing LinguisticCommunication)").
:- load_kif("(documentation Committing EnglishLanguage \"Instances of this &%Class commit the &%agent to some future course. For example, Bob promised Susan that he would be home by 11pm.\")").
:- load_kif("(=> (and (instance ?COMMIT Committing) (patient ?COMMIT ?FORMULA) (instance ?FORMULA Formula)) (modalAttribute ?FORMULA Promise))").
:- load_kif("(subclass Offering Committing)").
:- load_kif("(documentation Offering EnglishLanguage \"The subclass of &%Committing in which a &%CognitiveAgent offers something &%Physical to another agent. Offerings may be unconditional (in which case they are a promise to effect a &%UnilateralGiving) or conditional (in which case they are a promise to effect a &%Transaction of some sort).\")").
:- load_kif("(subclass Declaring LinguisticCommunication)").
:- load_kif("(documentation Declaring EnglishLanguage \"The &%Class of &%LinguisticCommunications that effect an institutional alteration when performed by competent authority. Some examples are nominating, marrying, and excommunicating.\")").
:- load_kif("(=> (instance ?DECLARE Declaring) (exists (?PROP ?NORM) (or (confersNorm ?DECLARE ?PROP ?NORM) (deprivesNorm ?DECLARE ?PROP ?NORM))))").
:- load_kif("(subclass Naming Declaring)").
:- load_kif("(documentation Naming EnglishLanguage \"The &%Process of assigning a name to someone or something.\")").
:- load_kif("(=> (and (instance ?PROCESS Naming) (patient ?PROCESS ?THING) (destination ?PROCESS ?NAME)) (holdsDuring (FutureFn (WhenFn ?PROCESS)) (names ?NAME ?THING)))").
:- load_kif("(subclass Cooperation SocialInteraction)").
:- load_kif("(documentation Cooperation EnglishLanguage \"The &%subclass of &%SocialInteraction where the participants involved work together for the achievement of a common goal.\")").
:- load_kif("(=> (instance ?COOPERATE Cooperation) (exists (?PURP) (forall (?AGENT) (=> (agent ?COOPERATE ?AGENT) (hasPurposeForAgent ?COOPERATE ?PURP ?AGENT)))))").
:- load_kif("(subclass Meeting SocialInteraction)").
:- load_kif("(documentation Meeting EnglishLanguage \"The coming together of two or more &%CognitiveAgents for the purpose of &%Communication. This covers informal meetings, e.g. visits with family members, and formal meetings, e.g. a board of directors meeting.\")").
:- load_kif("(=> (and (instance ?MEET Meeting) (agent ?MEET ?AGENT1) (agent ?MEET ?AGENT2)) (holdsDuring (WhenFn ?MEET) (orientation ?AGENT1 ?AGENT2 Near)))").
:- load_kif("(=> (instance ?MEET Meeting) (exists (?AGENT1 ?AGENT2) (and (agent ?MEET ?AGENT1) (agent ?MEET ?AGENT2) (hasPurpose ?MEET (exists (?COMM) (and (instance ?COMM Communication) (agent ?COMM ?AGENT1) (agent ?COMM ?AGENT2)))))))").
:- load_kif("(subclass Contest SocialInteraction)").
:- load_kif("(documentation Contest EnglishLanguage \"A &%SocialInteraction where the &%agent and &%patient are &%CognitiveAgents who are trying to defeat one another. Note that this concept is often applied in a metaphorical sense in natural language, when we speak, e.g., of the struggle of plants for space or sunlight, or of bacteria for food resources in some environment.\")").
:- load_kif("(=> (instance ?CONTEST Contest) (exists (?AGENT1 ?AGENT2 ?PURP1 ?PURP2) (and (agent ?CONTEST ?AGENT1) (agent ?CONTEST ?AGENT2) (hasPurposeForAgent ?CONTEST ?PURP1 ?AGENT1) (hasPurposeForAgent ?CONTEST ?PURP2 ?AGENT2) (not (equal ?AGENT1 ?AGENT2)) (not (equal ?PURP1 ?PURP2)))))").
:- load_kif("(subclass ViolentContest Contest)").
:- load_kif("(documentation ViolentContest EnglishLanguage \"A &%Contest where one participant attempts to physically injure another participant.\")").
:- load_kif("(subclass War ViolentContest)").
:- load_kif("(documentation War EnglishLanguage \"A military confrontation between two or more &%GeopoliticalAreas or &%Organizations whose members are &%GeopoliticalAreas. As the corresponding axiom specifies, a &%War is made up of &%Battles.\")").
:- load_kif("(=> (instance ?WAR War) (exists (?BATTLE) (and (instance ?BATTLE Battle) (subProcess ?BATTLE ?WAR))))").
:- load_kif("(=> (and (instance ?WAR War) (agent ?WAR ?AGENT)) (or (instance ?AGENT GeopoliticalArea) (and (instance ?AGENT Organization) (forall (?MEMBER) (=> (member ?MEMBER ?AGENT) (instance ?MEMBER GeopoliticalArea))))))").
:- load_kif("(subclass Battle ViolentContest)").
:- load_kif("(documentation Battle EnglishLanguage \"A &%ViolentContest between two or more military units within the context of a war. Note that this does not cover the metaphorical sense of 'battle', which simply means a struggle of some sort. This sense should be represented with the more general concept of &%Contest.\")").
:- load_kif("(=> (instance ?BATTLE Battle) (exists (?WAR) (and (instance ?WAR War) (subProcess ?BATTLE ?WAR))))").
:- load_kif("(=> (instance ?BATTLE Battle) (exists (?ATTACK) (and (instance ?ATTACK ViolentContest) (subProcess ?ATTACK ?BATTLE))))").
:- load_kif("(subclass Game Contest)").
:- load_kif("(subclass Game RecreationOrExercise)").
:- load_kif("(documentation Game EnglishLanguage \"A &%Contest whose purpose is the enjoyment/stimulation of the participants or spectators of the &%Game.\")").
:- load_kif("(subclass Sport Game)").
:- load_kif("(documentation Sport EnglishLanguage \"A &%Game which requires some degree of physical exercion from the participants of the game.\")").
:- load_kif("(subclass LegalAction Contest)").
:- load_kif("(documentation LegalAction EnglishLanguage \"Any &%Process where a &%CognitiveAgent seeks to obtain something through a court of law.\")").
:- load_kif("(subclass Maneuver IntentionalProcess)").
:- load_kif("(documentation Maneuver EnglishLanguage \"An intentional move or play within a &%Contest. In many cases, a &%Maneuver is a realization of part of a strategy for winning the &%Contest, but it also may be just an arbitrary or semi-arbitrary division of the overarching &%Contest, e.g. innings in a baseball game.\")").
:- load_kif("(=> (instance ?MOVE Maneuver) (exists (?CONTEST) (and (instance ?CONTEST Contest) (subProcess ?MOVE ?CONTEST))))").
:- load_kif("(subclass Attack Maneuver)").
:- load_kif("(documentation Attack EnglishLanguage \"A &%Maneuver in a &%ViolentContest where the &%agent attempts to inflict damage on the &%patient.\")").
:- load_kif("(=> (instance ?ATTACK Attack) (exists (?CONTEST) (and (instance ?CONTEST ViolentContest) (subProcess ?ATTACK ?CONTEST))))").
:- load_kif("(=> (and (instance ?ATTACK Attack) (agent ?ATTACK ?AGENT) (patient ?ATTACK ?OBJ)) (hasPurposeForAgent ?ATTACK (exists (?DAMAGE) (and (instance ?DAMAGE Damaging) (patient ?DAMAGE ?OBJ))) ?AGENT))").
:- load_kif("(subclass DefensiveManeuver Maneuver)").
:- load_kif("(documentation DefensiveManeuver EnglishLanguage \"A &%Maneuver in a &%ViolentContest where the &%agent attempts to avoid being damaged.\")").
:- load_kif("(=> (instance ?DEFENSE DefensiveManeuver) (exists (?CONTEST) (and (instance ?CONTEST ViolentContest) (subProcess ?DEFENSE ?CONTEST))))").
:- load_kif("(=> (and (instance ?DEFENSE DefensiveManeuver) (agent ?DEFENSE ?AGENT)) (hasPurposeForAgent ?DEFENSE (not (exists (?DAMAGE) (and (instance ?DAMAGE Damaging) (patient ?DAMAGE ?AGENT)))) ?AGENT))").
:- load_kif("(=> (and (instance ?MANEUVER Maneuver) (instance ?CONTEST ViolentContest) (subProcess ?MANEUVER ?CONTEST)) (or (instance ?MANEUVER Attack) (instance ?MANEUVER DefensiveManeuver)))").
:- load_kif("(subclass Perception PsychologicalProcess)").
:- load_kif("(documentation Perception EnglishLanguage \"Sensing some aspect of the material world. Note that the &%agent of this sensing is assumed to be an &%Animal.\")").
:- load_kif("(=> (and (instance ?PERCEPT Perception) (agent ?PERCEPT ?AGENT)) (instance ?AGENT Animal))").
:- load_kif("(=> (instance ?AGENT SentientAgent) (capability Perception experiencer ?AGENT))").
:- load_kif("(subclass Seeing Perception)").
:- load_kif("(documentation Seeing EnglishLanguage \"The &%subclass of &%Perception in which the sensing is done by an ocular &%Organ.\")").
:- load_kif("(=> (and (instance ?SEE Seeing) (agent ?SEE ?AGENT) (patient ?SEE ?OBJ)) (exists (?PROP) (and (instance ?PROP ColorAttribute) (knows ?AGENT (attribute ?OBJ ?PROP)))))").
:- load_kif("(=> (and (instance ?SEE Seeing) (patient ?SEE ?OBJ)) (holdsDuring (WhenFn ?SEE) (attribute ?OBJ Illuminated)))").
:- load_kif("(=> (and (instance ?SEE Seeing) (patient ?SEE ?OBJ)) (exists (?ATTR) (and (instance ?ATTR ColorAttribute) (holdsDuring (WhenFn ?SEE) (attribute ?OBJ ?ATTR)))))").
:- load_kif("(subclass Looking Seeing)").
:- load_kif("(subclass Looking IntentionalProcess)").
:- load_kif("(documentation Looking EnglishLanguage \"Any instance of &%Seeing which is intentional.\")").
:- load_kif("(subclass Smelling Perception)").
:- load_kif("(documentation Smelling EnglishLanguage \"The &%subclass of &%Perception in which the sensing is done by an olefactory &%Organ.\")").
:- load_kif("(=> (and (instance ?SMELL Smelling) (patient ?SMELL ?OBJ)) (exists (?ATTR) (and (instance ?ATTR OlfactoryAttribute) (attribute ?OBJ ?ATTR))))").
:- load_kif("(subclass Tasting Perception)").
:- load_kif("(documentation Tasting EnglishLanguage \"The &%subclass of &%Perception in which the sensing is done by of an &%Organ which can discriminate various tastes.\")").
:- load_kif("(=> (and (instance ?TASTE Tasting) (patient ?TASTE ?OBJ)) (exists (?ATTR) (and (instance ?ATTR TasteAttribute) (attribute ?OBJ ?ATTR))))").
:- load_kif("(subclass Hearing Perception)").
:- load_kif("(documentation Hearing EnglishLanguage \"The &%subclass of &%Perception in which the sensing is done by an auditory &%Organ.\")").
:- load_kif("(=> (and (instance ?HEAR Hearing) (patient ?HEAR ?OBJ)) (exists (?ATTR) (and (instance ?ATTR SoundAttribute) (attribute ?OBJ ?ATTR))))").
% :- load_kif("; NS: add.").
:- load_kif("(subclass SoundAttribute PerceptualAttribute)").
:- load_kif("(subclass SoundAttribute RelationalAttribute)").
:- load_kif("(documentation SoundAttribute EnglishLanguage \"The volume of sound relative to a listener.\")").
:- load_kif("(instance Audible SoundAttribute)").
:- load_kif("(documentation Audible EnglishLanguage \"A sound level capable of being heard by a &%Human.\")").
:- load_kif("(=> (and (instance ?SOUND RadiatingSound) (agent ?SOUND ?OBJ) (attribute ?SOUND Audible)) (exists (?HUMAN) (and (instance ?HUMAN Human) (capability (KappaFn ?HEAR (and (instance ?HEAR Hearing) (agent ?HEAR ?HUMAN) (destination ?HEAR ?HUMAN) (origin ?HEAR ?OBJ))) agent ?HUMAN))))").
:- load_kif("(subclass Listening Hearing)").
:- load_kif("(subclass Listening IntentionalProcess)").
:- load_kif("(documentation Listening EnglishLanguage \"Any instance of &%Hearing which is intentional.\")").
:- load_kif("(subclass TactilePerception Perception)").
:- load_kif("(documentation TactilePerception EnglishLanguage \"The &%subclass of &%Perception in which the sensing is done by &%Touching. Note that &%Touching need not involve &%TactilePerception. For example, a person who has lost all sensation in both of his legs would have no &%TactilePerception of anything his legs were &%Touching.\")").
:- load_kif("(=> (instance ?TACTILE TactilePerception) (exists (?TOUCH) (and (instance ?TOUCH Touching) (subProcess ?TOUCH ?TACTILE))))").
:- load_kif("(subclass Radiating Motion)").
:- load_kif("(documentation Radiating EnglishLanguage \"Processes in which some form of electromagnetic radiation, e.g. radio waves, light waves, electrical energy, etc., is given off or absorbed by something else.\")").
:- load_kif("(subclass RadiatingLight RadiatingElectromagnetic)").
:- load_kif("(documentation RadiatingLight EnglishLanguage \"The &%subclass of &%Radiating in which light is given off or absorbed. Some examples include blinking, flashing, and glittering.\")").
:- load_kif("(<=> (exists (?EMIT) (and (instance ?EMIT RadiatingLight) (patient ?EMIT ?REGION) (instance ?REGION Region))) (attribute ?REGION Illuminated))").
:- load_kif("(subclass RadiatingInfrared RadiatingElectromagnetic)").
:- load_kif("(subclass RadiatingXRay RadiatingElectromagnetic)").
:- load_kif("(subclass RadiatingSound Radiating)").
:- load_kif("(documentation RadiatingSound EnglishLanguage \"The &%subclass of &%Radiating in which sound waves are given off or absorbed. Some examples include creaking, roaring, and whistling.\")").
:- load_kif("(=> (and (instance ?EMIT RadiatingSound) (agent ?EMIT ?SOUND)) (exists (?ATTR) (and (instance ?ATTR SoundAttribute) (attribute ?SOUND ?ATTR))))").
% :- load_kif("; KJN: Removing this and renaming it to MakingMusic to be consistent with the Verb form of most Processes (subclass Music RadiatingSound) (documentation Music EnglishLanguage \"The &%subclass of &%RadiatingSound where the sound is intended to be melodic and is produced deliberately.\")").
:- load_kif("(subclass MakingMusic RadiatingSound)").
:- load_kif("(documentation MakingMusic EnglishLanguage \"&%MakingMusic is a type of &%RadiatingSound where the &%result is intended to be melodic and is produced delibrately\")").
:- load_kif("(termFormat EnglishLanguage MakingMusic \"making music\")").
:- load_kif("(comment MakingMusic \"Changing the old Music to &%MakingMusic to be more consistent with the verb-form that &%Process seem to take. (09-14-2011)\" \"KJN\")").
:- load_kif("(partition MakingMusic MakingInstrumentalMusic MakingVocalMusic)").
:- load_kif("(subclass MakingInstrumentalMusic MakingMusic)").
:- load_kif("(documentation MakingInstrumentalMusic EnglishLanguage \"&%MakingInstrumentalMusic is a type of &%MakingMusic which is produced using some kind of &%MusicalInstrument\")").
:- load_kif("(termFormat EnglishLanguage MakingInstrumentalMusic \"instrumental music\")").
:- load_kif("(=> (instance ?M MakingInstrumentalMusic) (exists (?I) (and (instance ?I MusicalInstrument) (instrument ?M ?I))))").
:- load_kif("(subclass MakingVocalMusic MakingMusic)").
:- load_kif("(subclass MakingVocalMusic Vocalizing)").
:- load_kif("(documentation MakingVocalMusic EnglishLanguage \"&%MakingVocalMusic is a type of &%MakingMusic which is produced by using the vocal cords\")").
:- load_kif("(termFormat EnglishLanguage MakingVocalMusic \"singing\")").
:- load_kif("(comment MakingVocalMusic \"Currently, &%VocalMusic also inherits from &%Speaking. Changing this to more general &%Vocalizing. Acapella groups today use their &%VocalCord to create music in ways that are more than just speaking. (09-14-2011)\" \"KJN\")").
:- load_kif("(subclass Singing MakingVocalMusic)").
:- load_kif("(documentation Singing EnglishLanguage \"&%Singing is a type of &%MakingVocalMusic wherein words are produced by the singer. This is different from other forms of &%MakingVocalMusic such as humming or scatting or beatboxing , where the vocal cords are used to create the music but no words are formed.\")").
:- load_kif("(=> (instance ?S Singing) (exists (?W) (and (patient ?S ?W) (instance ?W Word))))").
:- load_kif("(subclass RadiatingElectromagnetic Radiating)").
:- load_kif("(documentation RadiatingElectromagnetic EnglishLanguage \"&%RadiatingElectromagnetic is the subclass of &%Radiating processes in which electromagnetic radiation is transmitted or absorbed.\")").
:- load_kif("(subclass RadiatingNuclear Radiating)").
:- load_kif("(documentation RadiatingNuclear EnglishLanguage \"Releasing atomic energy, i.e. energy from a nuclear reaction.\")").
:- load_kif("(subclass StateChange InternalChange)").
:- load_kif("(documentation StateChange EnglishLanguage \"Any &%Process where the &%PhysicalState of &%part of the &%patient of the &%Process changes.\")").
:- load_kif("(=> (and (instance ?PROCESS StateChange) (patient ?PROCESS ?OBJ)) (exists (?PART ?STATE1 ?STATE2) (and (part ?PART ?OBJ) (instance ?STATE1 PhysicalState) (instance ?STATE2 PhysicalState) (not (equal ?STATE1 ?STATE2)) (holdsDuring (BeginFn (WhenFn ?PROCESS)) (attribute ?PART ?STATE1)) (holdsDuring (EndFn (WhenFn ?PROCESS)) (attribute ?PART ?STATE2)))))").
:- load_kif("(instance atomicNumber BinaryPredicate)").
:- load_kif("(instance atomicNumber AsymmetricRelation)").
:- load_kif("(domainSubclass atomicNumber 1 ElementalSubstance)").
:- load_kif("(domain atomicNumber 2 PositiveInteger)").
:- load_kif("(documentation atomicNumber EnglishLanguage \"(&%atomicNumber ?ELEMENT ?NUMBER) means that the &%ElementalSubstance ?ELEMENT has the atomic number ?NUMBER. The atomic number is the number of &%Protons in the nucleus of an &%Atom.\")").
:- load_kif("(=> (atomicNumber ?TYPE ?NUMBER) (=> (and (instance ?SUBSTANCE ?TYPE) (part ?ATOM ?SUBSTANCE) (instance ?ATOM Atom)) (equal ?NUMBER (CardinalityFn (KappaFn ?PROTON (and (part ?PROTON ?ATOM) (instance ?PROTON Proton)))))))").
:- load_kif("(instance boilingPoint BinaryPredicate)").
:- load_kif("(domainSubclass boilingPoint 1 PureSubstance)").
:- load_kif("(domain boilingPoint 2 TemperatureMeasure)").
:- load_kif("(documentation boilingPoint EnglishLanguage \"The temperature at which a &%PureSubstance changes state from a &%Liquid to a &%Gas.\")").
:- load_kif("(=> (and (instance ?X ?Y) (subclass ?Y PureSubstance) (barometricPressure ?X (MeasureFn ?PRES InchMercury)) (greaterThan 29.92 ?PRES) (boilingPoint ?Y (MeasureFn ?BOIL KelvinDegree)) (measure ?X (MeasureFn ?TEMP KelvinDegree)) (greaterThan ?TEMP ?BOIL)) (attribute ?X Gas))").
:- load_kif("(=> (and (instance ?X ?Y) (subclass ?Y PureSubstance) (boilingPoint ?Y (MeasureFn ?BOIL KelvinDegree)) (meltingPoint ?Y (MeasureFn ?MELT KelvinDegree)) (measure ?X (MeasureFn ?TEMP KelvinDegree)) (greaterThan ?TEMP ?MELT) (lessThan ?TEMP ?BOIL)) (attribute ?X Liquid))").
:- load_kif("(=> (and (instance ?X ?Y) (subclass ?Y PureSubstance) (meltingPoint ?Y (MeasureFn ?MELT KelvinDegree)) (barometricPressure ?X (MeasureFn ?PRES InchMercury)) (greaterThan ?PRES 29.92) (measure ?X (MeasureFn ?TEMP KelvinDegree)) (lessThan ?TEMP ?MELT)) (attribute ?X Solid))").
:- load_kif("(=> (and (instance ?BOILING Boiling) (boilingPoint ?TYPE (MeasureFn ?TEMP1 ?MEASURE)) (instance ?SUBSTANCE ?TYPE) (patient ?BOILING ?SUBSTANCE) (holdsDuring (WhenFn ?BOILING) (measure ?SUBSTANCE (MeasureFn ?TEMP2 ?MEASURE))) (instance ?MEASURE UnitOfTemperature)) (greaterThanOrEqualTo ?TEMP2 ?TEMP1))").
:- load_kif("(=> (and (boilingPoint ?TYPE (MeasureFn ?TEMP1 ?MEASURE)) (instance ?SUBSTANCE ?TYPE) (holdsDuring ?TIME (measure ?SUBSTANCE (MeasureFn ?TEMP2 ?MEASURE))) (instance ?MEASURE UnitOfTemperature) (greaterThanOrEqualTo ?TEMP2 ?TEMP1)) (or (holdsDuring ?TIME (attribute ?SUBSTANCE Gas)) (exists (?BOIL) (and (overlapsTemporally (WhenFn ?BOIL) ?TIME) (instance ?BOIL Boiling) (patient ?BOIL ?SUBSTANCE)))))").
:- load_kif("(instance meltingPoint BinaryPredicate)").
:- load_kif("(domainSubclass meltingPoint 1 PureSubstance)").
:- load_kif("(domain meltingPoint 2 TemperatureMeasure)").
:- load_kif("(documentation meltingPoint EnglishLanguage \"The temperature at which a &%PureSubstance changes state from a &%Solid to a &%Liquid. Note that &%Arsenic can sublimate directly from &%Solid to &%Gas which means that its melting and boiling points are equal.\")").
:- load_kif("(=> (and (instance ?SUBSTANCE ?TYPE) (boilingPoint ?TYPE (MeasureFn ?TEMP1 ?MEASURE)) (meltingPoint ?TYPE (MeasureFn ?TEMP2 ?MEASURE)) (instance ?MEASURE UnitOfTemperature) (holdsDuring ?TIME (measure ?SUBSTANCE (MeasureFn ?TEMP3 ?MEASURE))) (greaterThan ?TEMP3 ?TEMP2) (lessThan ?TEMP3 ?TEMP1)) (or (holdsDuring ?TIME (attribute ?SUBSTANCE Liquid)) (exists (?MELT) (and (overlapsTemporally (WhenFn ?MELT) ?TIME) (instance ?MELT Melting) (patient ?BOIL ?SUBSTANCE)))))").
:- load_kif("(=> (and (instance ?SUBSTANCE ?TYPE) (meltingPoint ?TYPE (MeasureFn ?TEMP1 ?MEASURE)) (holdsDuring ?TIME (measure ?SUBSTANCE (MeasureFn ?TEMP2 ?MEASURE))) (instance ?MEASURE UnitOfTemperature) (lessThan ?TEMP2 ?TEMP1)) (or (holdsDuring ?TIME (attribute ?SUBSTANCE Solid)) (exists (?FREEZE) (and (overlapsTemporally (WhenFn ?FREEZE) ?TIME) (instance ?FREEZE Freezing) (patient ?FREEZE ?SUBSTANCE)))))").
:- load_kif("(=> (and (meltingPoint ?TYPE ?MELT) (boilingPoint ?TYPE ?BOIL)) (greaterThanOrEqualTo ?BOIL ?MELT))").
:- load_kif("(subclass Melting StateChange)").
:- load_kif("(documentation Melting EnglishLanguage \"The &%Class of &%Processes where an &%Object is heated and converted from a &%Solid to a &%Liquid.\")").
:- load_kif("(=> (instance ?MELT Melting) (exists (?HEAT) (and (instance ?HEAT Heating) (subProcess ?HEAT ?MELT))))").
:- load_kif("(=> (and (instance ?MELT Melting) (patient ?MELT ?OBJ)) (exists (?PART) (and (part ?PART ?OBJ) (holdsDuring (BeginFn (WhenFn ?MELT)) (attribute ?PART Solid)) (holdsDuring (EndFn (WhenFn ?MELT)) (attribute ?PART Liquid)))))").
:- load_kif("(subclass Boiling StateChange)").
:- load_kif("(documentation Boiling EnglishLanguage \"The &%Class of &%Processes where a &%Substance is heated and converted from a &%Liquid to a &%Gas.\")").
:- load_kif("(=> (instance ?BOIL Boiling) (exists (?HEAT) (and (instance ?HEAT Heating) (subProcess ?HEAT ?BOIL))))").
:- load_kif("(=> (and (instance ?BOIL Boiling) (patient ?BOIL ?OBJ)) (exists (?PART) (and (part ?PART ?OBJ) (holdsDuring (BeginFn (WhenFn ?BOIL)) (attribute ?PART Liquid)) (holdsDuring (EndFn (WhenFn ?BOIL)) (attribute ?PART Gas)))))").
:- load_kif("(subclass Evaporating StateChange)").
:- load_kif("(documentation Evaporating EnglishLanguage \"The &%Class of &%Processes where a &%Substance is converted from a &%Liquid to a &%Gas at a temperature below its &%Boiling point.\")").
:- load_kif("(=> (and (instance ?EVAP Evaporating) (boilingPoint ?OBJ (MeasureFn ?BOILVAL ?MEAS)) (measure ?OBJ (MeasureFn ?VAL ?MEAS)) (instance ?MEAS UnitOfTemperature) (patient ?EVAP ?OBJ)) (exists (?PART) (and (part ?PART ?OBJ) (greaterThan ?BOILVAL ?VAL) (holdsDuring (BeginFn (WhenFn ?EVAP)) (attribute ?PART Liquid)) (holdsDuring (EndFn (WhenFn ?EVAP)) (attribute ?PART Gas)))))").
:- load_kif("(subclass Condensing StateChange)").
:- load_kif("(documentation Condensing EnglishLanguage \"The &%Class of &%Processes where an &%Object is cooled and converted from a &%Gas to a &%Liquid.\")").
:- load_kif("(=> (instance ?COND Condensing) (exists (?COOL) (and (instance ?COOL Cooling) (subProcess ?COOL ?COND))))").
:- load_kif("(=> (and (instance ?COND Condensing) (patient ?COND ?OBJ)) (exists (?PART) (and (part ?PART ?OBJ) (holdsDuring (BeginFn (WhenFn ?COND)) (attribute ?PART Gas)) (holdsDuring (EndFn (WhenFn ?COND)) (attribute ?PART Liquid)))))").
:- load_kif("(subclass Freezing StateChange)").
:- load_kif("(documentation Freezing EnglishLanguage \"The &%Class of &%Processes where an &%Object is cooled and converted from a &%Liquid to a &%Solid.\")").
:- load_kif("(=> (instance ?FREEZE Freezing) (exists (?COOL) (and (instance ?COOL Cooling) (subProcess ?COOL ?FREEZE))))").
:- load_kif("(=> (and (instance ?FREEZE Freezing) (patient ?FREEZE ?OBJ)) (exists (?PART) (and (part ?PART ?OBJ) (holdsDuring (BeginFn (WhenFn ?FREEZE)) (attribute ?PART Liquid)) (holdsDuring (EndFn (WhenFn ?FREEZE)) (attribute ?PART Solid)))))").
% :- load_kif("; END FILE").
% :- load_kif("; BEGIN FILE").
% :- load_kif(";  OBJECTS  ").
% :- load_kif("; INCLUDES 'MEREOTOPOLOGY' INCLUDES 'PROCESSES' INCLUDES 'QUALITIES'").
:- load_kif("(subclass AstronomicalBody Object)").
:- load_kif("(disjoint AstronomicalBody GeographicArea)").
:- load_kif("(documentation AstronomicalBody EnglishLanguage \"The &%Class of all astronomical objects of significant size. It includes &%SelfConnectedObjects like planets, stars, and asteroids, as well as &%Collections like nebulae, galaxies, and constellations. Note that the planet Earth is an &%AstronomicalBody, but every &%Region of Earth is a &%GeographicArea.\")").
:- load_kif("(subclass GeographicArea Region)").
:- load_kif("(partition GeographicArea WaterArea LandArea)").
:- load_kif("(documentation GeographicArea EnglishLanguage \"A geographic location, generally having definite boundaries. Note that this differs from its immediate superclass &%Region in that a &%GeographicArea is a three-dimensional &%Region of the earth. Accordingly, all astronomical objects other than earth and all one-dimensional and two-dimensional &%Regions are not classed under &%GeographicArea.\")").
:- load_kif("(subclass LocalizablePlace GeographicArea)").
:- load_kif("(instance geographicSubregion BinaryPredicate)").
:- load_kif("(instance geographicSubregion TransitiveRelation)").
:- load_kif("(instance geographicSubregion AsymmetricRelation)").
:- load_kif("(subrelation geographicSubregion properPart)").
:- load_kif("(subrelation geographicSubregion located)").
:- load_kif("(domain geographicSubregion 1 GeographicArea)").
:- load_kif("(domain geographicSubregion 2 GeographicArea)").
:- load_kif("(documentation geographicSubregion EnglishLanguage \"(&%geographicSubregion ?PART ?WHOLE) means that the &%GeographicArea ?PART is part of the &%GeographicArea ?WHOLE.\")").
:- load_kif("(subclass GeopoliticalArea GeographicArea)").
:- load_kif("(subclass GeopoliticalArea Agent)").
:- load_kif("(documentation GeopoliticalArea EnglishLanguage \"Any &%GeographicArea which is associated with some sort of political structure. This class includes &%Lands, &%Cities, districts of cities, counties, etc. Note that the identity of a &%GeopoliticalArea may remain constant after a change in borders.\")").
:- load_kif("(=> (and (instance ?AREA GeopoliticalArea) (leader (GovernmentFn ?AREA) ?PERSON)) (leader ?AREA ?PERSON))").
:- load_kif("(=> (and (instance ?AREA GeopoliticalArea) (leader ?AREA ?PERSON)) (leader (GovernmentFn ?AREA) ?PERSON))").
:- load_kif("(=> (and (instance ?EVENT Election) (agent ?EVENT ?AGENT) (instance ?AGENT GeopoliticalArea)) (instance ?EVENT PoliticalProcess))").
:- load_kif("(=> (and (instance ?EVENT Election) (agent ?EVENT ?AGENT) (instance ?AREA GeopoliticalArea) (instance ?AGENT (GovernmentFn ?AREA))) (instance ?EVENT PoliticalProcess))").
:- load_kif("(instance geopoliticalSubdivision AsymmetricRelation)").
:- load_kif("(instance geopoliticalSubdivision TransitiveRelation)").
:- load_kif("(subrelation geopoliticalSubdivision geographicSubregion)").
:- load_kif("(domain geopoliticalSubdivision 1 GeopoliticalArea)").
:- load_kif("(domain geopoliticalSubdivision 2 GeopoliticalArea)").
:- load_kif("(documentation geopoliticalSubdivision EnglishLanguage \"(&%geopoliticalSubdivision ?AREA1 ?AREA2) means that ?AREA1 is any geopolitical part of ?AREA2, that is, ?AREA1 is an integral &%geographicSubregion of ?AREA2 (not a &%DependencyOrSpecialSovereigntyArea), having its own associated &%GovernmentOrganization which is subordinated to or constrained by the government of ?AREA2. Cf. &%dependentGeopoliticalArea.\")").
% :- load_kif("; NS: delete. (=> (geopoliticalSubdivision ?SUB ?AREA) (not (instance ?SUB Nation)))").
% :- load_kif("; NS: Note that Government.kif contains the following formulae:").
% :- load_kif("(subclass IndependentState Nation) (=> (geopoliticalSubdivision ?SUB ?AREA) (not (instance ?SUB IndependentState)))").
% :- load_kif("; It would be best to rewrite all rules in which &%Nation is used in the sense of &%IndependentState so that they use &%IndependentState. This would allow &%Nation to be interpreted in a more general \"cultural\" or \"homeland\" sense, as suggested by (subclass IndependentState Nation). England, Scotland, Wales, and NorthernIreland, for example, would be &%Nations, but not (currently) &%IndependentStates.").
:- load_kif("(subclass WaterArea GeographicArea)").
:- load_kif("(documentation WaterArea EnglishLanguage \"A body which is made up predominantly of water, e.g. rivers, lakes, oceans, etc.\")").
:- load_kif("(=> (instance ?AREA WaterArea) (exists (?BED ?HOLE ?WATER) (and (equal (HoleHostFn ?HOLE) ?BED) (instance ?WATER Water) (properlyFills ?WATER ?HOLE) (equal (MereologicalSumFn ?BED ?WATER) ?AREA))))").
:- load_kif("(subclass SaltWaterArea WaterArea)").
:- load_kif("(disjoint SaltWaterArea FreshWaterArea)").
:- load_kif("(documentation SaltWaterArea EnglishLanguage \"A &%WaterArea whose &%Water is saline, e.g. oceans and seas.\")").
:- load_kif("(subclass FreshWaterArea WaterArea)").
:- load_kif("(documentation FreshWaterArea EnglishLanguage \"A &%WaterArea whose &%Water is not saline, e.g. most rivers and lakes.\")").
% :- load_kif("; KJN: Moving this to MILO to remove dependencies (subclass StreamWaterArea WaterArea) (disjoint StreamWaterArea StaticWaterArea) (documentation StreamWaterArea EnglishLanguage \"A relatively narrow &%WaterArea where the water flows constantly and in the same direction, e.g. a river, a stream, etc.\")").
% :- load_kif(" (subclass StaticWaterArea WaterArea) (documentation StaticWaterArea EnglishLanguage \"A &%WaterArea in which water does not flow constantly or in the same direction, e.g. most lakes and ponds.\")").
:- load_kif("(subclass LandArea GeographicArea)").
:- load_kif("(documentation LandArea EnglishLanguage \"An area which is predominantly solid ground, e.g. a &%Nation, a mountain, a desert, etc. Note that a &%LandArea may contain some relatively small &%WaterAreas. For example, Australia is a &%LandArea even though it contains various rivers and lakes.\")").
:- load_kif("(=> (instance ?LAND1 LandArea) (exists (?LAND2) (and (part ?LAND1 ?LAND2) (or (instance ?LAND2 Continent) (instance ?LAND2 Island)))))").
:- load_kif("(subclass ShoreArea LandArea)").
:- load_kif("(documentation ShoreArea EnglishLanguage \"A &%ShoreArea is a &%LandArea approximately 1-3 km wide bordering a body of water, such as an ocean, bay, river, or lake. A &%ShoreArea may comprise a variety of &%LandForms, such as dunes, sloughs, and marshes.\")").
:- load_kif("(=> (instance ?BANK ShoreArea) (exists (?WATER) (and (instance ?WATER WaterArea) (meetsSpatially ?BANK ?WATER))))").
:- load_kif("(subclass Continent LandArea)").
:- load_kif("(equal (CardinalityFn Continent) 7)").
:- load_kif("(documentation Continent EnglishLanguage \"As defined in the CIA World Fact Book, &%Continent covers seven land masses: &%Africa, &%NorthAmerica, &%SouthAmerica, &%Antarctica, &%Europe, &%Asia, and &%Oceania. Note that &%Australia, counted as a continent in some other systems, is included in &%Oceania in the Fact Book. As a consequence, there is no &%Nation which is also a &%Continent.\")").
:- load_kif("(subclass Island LandArea)").
:- load_kif("(documentation Island EnglishLanguage \"A &%LandArea that is completely surrounded by a &%WaterArea.\")").
:- load_kif("(=> (instance ?ISLAND Island) (not (exists (?AREA ?PART1 ?PART2) (and (instance ?AREA LandArea) (part ?PART1 ?ISLAND) (part ?PART2 ?AREA) (not (part ?ISLAND ?AREA)) (not (part ?AREA ?ISLAND)) (meetsSpatially ?PART1 ?PART2)))))").
:- load_kif("(=> (and (instance ?ISLE Island) (instance ?AREA GeographicArea) (meetsSpatially ?ISLE ?AREA)) (not (instance ?AREA LandArea)))").
:- load_kif("(=> (instance ?ISLE Island) (exists (?WATER) (and (instance ?WATER WaterArea) (meetsSpatially ?ISLE ?WATER))))").
:- load_kif("(subclass Nation GeopoliticalArea)").
:- load_kif("(subclass Nation LandArea)").
:- load_kif("(documentation Nation EnglishLanguage \"The broadest &%GeopoliticalArea, i.e. &%Nations are &%GeopoliticalAreas that are not part of any other overarching and comprehensive governance structure (excepting commonwealths and other sorts of loose international organizations).\")").
:- load_kif("(subclass StateOrProvince GeopoliticalArea)").
:- load_kif("(subclass StateOrProvince LandArea)").
:- load_kif("(documentation StateOrProvince EnglishLanguage \"Administrative subdivisions of a &%Nation that are broader than any other political subdivisions that may exist. This &%Class includes the states of the United States, as well as the provinces of Canada and European countries.\")").
:- load_kif("(=> (instance ?STATE StateOrProvince) (exists (?LAND) (and (instance ?LAND Nation) (properPart ?STATE ?LAND))))").
:- load_kif("(subclass City GeopoliticalArea)").
:- load_kif("(subclass City LandArea)").
:- load_kif("(documentation City EnglishLanguage \"A &%LandArea of relatively small size, inhabited by a community of people, and having some sort of political structure. Note that this class includes both large cities and small settlements like towns, villages, hamlets, etc.\")").
:- load_kif("(subclass County GeopoliticalArea)").
:- load_kif("(subclass County LandArea)").
:- load_kif("(documentation County EnglishLanguage \"A &%GeopoliticalArea that is larger than a city, usually encompassing several cities, and smaller than a &%StateOrProvince. Aside from City, this is the smallest geopolitical subdivision, and it is known by various names in various counties, e.g. parrish, commune, etc.\")").
:- load_kif("(=> (instance ?STATE County) (exists (?LAND) (and (instance ?LAND StateOrProvince) (properPart ?STATE ?LAND))))").
:- load_kif("(subclass Transitway Region)").
:- load_kif("(subclass Transitway SelfConnectedObject)").
:- load_kif("(documentation Transitway EnglishLanguage \"&%Transitway is the broadest class of regions which may be passed through as a &%path in instances of &%Translocation. &%Transitway includes land, air, and sea regions, and it includes both natural and artificial transitways.\")").
:- load_kif("(subclass LandTransitway Transitway)").
:- load_kif("(subclass LandTransitway LandArea)").
:- load_kif("(documentation LandTransitway EnglishLanguage \"&%LandTransitway is the subclass of &%Transitway that represents areas intended for motion over the ground.\")").
:- load_kif("(=> (instance ?WAY Transitway) (hasPurpose ?WAY (exists (?TRANSPORT) (and (instance ?TRANSPORT Transportation) (path ?TRANSPORT ?WAY)))))").
:- load_kif("(subclass Roadway LandTransitway)").
:- load_kif("(documentation Roadway EnglishLanguage \"&%Roadway is the subclass of &%LandTransitways that are areas intended for surface travel by self-powered, wheeled vehicles, excluding those that travel on tracks. &%Roadways have been at least minimally improved to enable the passage of vehicles. &%Roadways include dirt and gravelled roads, paved streets, and expressways.\")").
:- load_kif("(subclass Water CompoundSubstance)").
:- load_kif("(documentation Water EnglishLanguage \"The &%Class of samples of the compound H20. Note that this &%Class covers both pure and impure &%Water.\")").
:- load_kif("(subclass Mineral Substance)").
:- load_kif("(documentation Mineral EnglishLanguage \"Any of various naturally occurring homogeneous substances (such as stone, coal, salt, sulfur, sand, petroleum), or synthetic substances having the chemical composition and crystalline form and properties of a naturally occurring mineral.\")").
:- load_kif("(instance developmentalForm BinaryPredicate)").
:- load_kif("(instance developmentalForm AsymmetricRelation)").
:- load_kif("(instance developmentalForm TransitiveRelation)").
:- load_kif("(subrelation developmentalForm attribute)").
:- load_kif("(domain developmentalForm 1 OrganicObject)").
:- load_kif("(domain developmentalForm 2 DevelopmentalAttribute)").
% :- load_kif("; NS: delete. (documentation developmentalForm EnglishLanguage \"(&%developmentalForm ?OBJECT ?FORM) means that ?FORM is an earlier stage in the individual maturation of ?OBJECT. For example, tadpole and caterpillar are &%developmentalForms of frogs and butterflies, respectively.\")").
% :- load_kif("; NS: add.").
:- load_kif("(documentation developmentalForm EnglishLanguage \"(&%developmentalForm ?OBJECT ?FORM) means that ?FORM describes a stage in the individual maturation of ?OBJECT. For example, tadpole and caterpillar are &%developmentalForms of frogs and butterflies, respectively.\")").
:- load_kif("(=> (and (holdsDuring ?TIME1 (developmentalForm ?OBJ ?ATTR1)) (successorAttributeClosure ?ATTR2 ?ATTR1)) (exists (?TIME2) (and (earlier ?TIME2 ?TIME1) (holdsDuring ?TIME2 (developmentalForm ?OBJ ?ATTR2)))))").
:- load_kif("(subclass OrganicObject CorpuscularObject)").
:- load_kif("(subclass OrganicObject OrganicThing)").
:- load_kif("(partition OrganicObject Organism AnatomicalStructure)").
:- load_kif("(documentation OrganicObject EnglishLanguage \"This class encompasses &%Organisms, &%CorpuscularObjects that are parts of &%Organisms, i.e. &%BodyParts, and &%CorpuscularObjects that are nonintentionally produced by &%Organisms, e.g. &%ReproductiveBodies.\")").
:- load_kif("(subclass Organism OrganicObject)").
:- load_kif("(subclass Organism Agent)").
:- load_kif("(disjoint Organism Artifact)").
:- load_kif("(partition Organism Animal Plant Microorganism)").
:- load_kif("(documentation Organism EnglishLanguage \"Generally, a living individual, including all &%Plants and &%Animals.\")").
:- load_kif("(=> (instance ?ORGANISM Organism) (exists (?BIRTH) (and (instance ?BIRTH Birth) (experiencer ?BIRTH ?ORGANISM))))").
:- load_kif("(instance inhabits BinaryPredicate)").
:- load_kif("(instance inhabits AsymmetricRelation)").
:- load_kif("(domain inhabits 1 Organism)").
:- load_kif("(domain inhabits 2 Object)").
:- load_kif("(documentation inhabits EnglishLanguage \"A very basic notion of living within something else. (&%inhabits ?ORGANISM ?OBJECT) means that ?OBJECT is the residence").
:- load_kif("(either permanent or temporary), nest, etc. of ?ORGANISM.\")").
:- load_kif("(=> (holdsDuring ?T1 (inhabits ?ORGANISM ?OBJ)) (exists (?TIME) (and (instance ?TIME TimeInterval) (temporalPart ?TIME ?T1) (holdsDuring ?TIME (located ?ORGANISM ?OBJ)))))").
:- load_kif("(subrelation home inhabits)").
:- load_kif("(domain home 1 Human)").
:- load_kif("(domain home 2 PermanentResidence)").
:- load_kif("(documentation home EnglishLanguage \"The relation between a &%Human and a &%PermanentResidence of the &%Human.\")").
:- load_kif("(subrelation stays inhabits)").
:- load_kif("(disjointRelation stays home)").
:- load_kif("(domain stays 1 Human)").
:- load_kif("(domain stays 2 TemporaryResidence)").
:- load_kif("(documentation stays EnglishLanguage \"The relation between a &%Human and a &%TemporaryResidence of the &%Human.\")").
:- load_kif("(subclass Plant Organism)").
:- load_kif("(documentation Plant EnglishLanguage \"An &%Organism having cellulose cell walls, growing by synthesis of &%Substances, generally distinguished by the presence of chlorophyll, and lacking the power of locomotion.\")").
:- load_kif("(subclass FloweringPlant Plant)").
:- load_kif("(documentation FloweringPlant EnglishLanguage \"A &%Plant that produces seeds and flowers. This class includes trees, shrubs, herbs, and flowers.\")").
:- load_kif("(subclass NonFloweringPlant Plant)").
:- load_kif("(disjoint NonFloweringPlant FloweringPlant)").
:- load_kif("(disjointDecomposition NonFloweringPlant Alga Fern Moss)").
:- load_kif("(documentation NonFloweringPlant EnglishLanguage \"A &%Plant that reproduces with spores and does not produce flowers.\")").
:- load_kif("(subclass Alga NonFloweringPlant)").
:- load_kif("(documentation Alga EnglishLanguage \"A chiefly aquatic plant that contains chlorophyll, but does not form embryos during development and lacks vascular tissue.\")").
:- load_kif("(=> (instance ?ALGA Alga) (exists (?WATER) (and (inhabits ?ALGA ?WATER) (instance ?WATER Water))))").
:- load_kif("(subclass Fungus Organism)").
:- load_kif("(documentation Fungus EnglishLanguage \"A eukaryotic &%Organism characterized by the absence of chlorophyll and the presence of rigid cell walls. Included here are both slime molds and true fungi such as yeasts, molds, mildews, and mushrooms.\")").
:- load_kif("(=> (and (instance ?FUNGUS Fungus) (inhabits ?FUNGUS ?OBJ)) (instance ?OBJ Organism))").
:- load_kif("(subclass Moss NonFloweringPlant)").
:- load_kif("(documentation Moss EnglishLanguage \"A &%NonFloweringPlant without true roots and little if any vascular tissue.\")").
:- load_kif("(subclass Fern NonFloweringPlant)").
:- load_kif("(documentation Fern EnglishLanguage \"A &%NonFloweringPlant that contains vascular tissue. This class includes true ferns, as well as horsetails, club mosses, and whisk ferns.\")").
:- load_kif("(subclass Animal Organism)").
:- load_kif("(partition Animal Vertebrate Invertebrate)").
:- load_kif("(documentation Animal EnglishLanguage \"An &%Organism with eukaryotic &%Cells, and lacking stiff cell walls, plastids, and photosynthetic pigments.\")").
:- load_kif("(subclass Microorganism Organism)").
:- load_kif("(documentation Microorganism EnglishLanguage \"An &%Organism that can be seen only with the aid of a microscope.\")").
:- load_kif("(subclass Bacterium Microorganism)").
:- load_kif("(documentation Bacterium EnglishLanguage \"A small, typically one-celled, prokaryotic &%Microorganism.\")").
:- load_kif("(=> (instance ?BACTERIUM Bacterium) (exists (?CELL1) (and (component ?CELL1 ?BACTERIUM) (instance ?CELL1 Cell) (forall (?CELL2) (=> (and (component ?CELL2 ?BACTERIUM) (instance ?CELL2 Cell)) (equal ?CELL1 ?CELL2))))))").
:- load_kif("(=> (and (instance ?BACTERIUM Bacterium) (inhabits ?BACTERIUM ?OBJ)) (instance ?OBJ Organism))").
:- load_kif("(subclass Virus Microorganism)").
:- load_kif("(documentation Virus EnglishLanguage \"An &%Organism consisting of a core of a single nucleic acid enclosed in a protective coat of protein. A virus may replicate only inside a host living cell. A virus exhibits some but not all of the usual characteristics of living things.\")").
:- load_kif("(=> (and (instance ?VIRUS Virus) (inhabits ?VIRUS ?OBJ)) (instance ?OBJ Organism))").
:- load_kif("(=> (and (instance ?VIRUS Virus) (instance ?PROC Replication) (agent ?PROC ?VIRUS)) (exists (?CELL) (and (located ?PROC ?CELL) (instance ?CELL Cell))))").
:- load_kif("(subclass Vertebrate Animal)").
:- load_kif("(documentation Vertebrate EnglishLanguage \"An &%Animal which has a spinal column.\")").
:- load_kif("(subclass Invertebrate Animal)").
:- load_kif("(disjointDecomposition Invertebrate Worm Mollusk Arthropod)").
:- load_kif("(documentation Invertebrate EnglishLanguage \"An &%Animal which has no spinal column.\")").
:- load_kif("(subclass Worm Invertebrate)").
:- load_kif("(documentation Worm EnglishLanguage \"Long, narrow, soft-bodied &%Invertebrates.\")").
:- load_kif("(subclass Mollusk Invertebrate)").
:- load_kif("(documentation Mollusk EnglishLanguage \"Soft-bodied &%Invertebrate that is usually contained in a shell. Includes oysters, clams, mussels, snails, slugs, octopi, and squid.\")").
:- load_kif("(subclass Arthropod Invertebrate)").
:- load_kif("(disjointDecomposition Arthropod Arachnid Myriapod Insect Crustacean)").
:- load_kif("(documentation Arthropod EnglishLanguage \"A &%Class of &%Invertebrate that includes &%Arachnids and &%Insects.\")").
:- load_kif("(subclass Arachnid Arthropod)").
:- load_kif("(documentation Arachnid EnglishLanguage \"A &%Class of &%Arthropods that includes ticks and spiders.\")").
:- load_kif("(subclass Myriapod Arthropod)").
:- load_kif("(documentation Myriapod EnglishLanguage \"A &%Class of &%Arthropods that includes centipedes and millipedes.\")").
:- load_kif("(subclass Insect Arthropod)").
:- load_kif("(documentation Insect EnglishLanguage \"A &%Class of small &%Arthropods that are air-breathing and that are distinguished by appearance.\")").
:- load_kif("(subclass Crustacean Arthropod)").
:- load_kif("(documentation Crustacean EnglishLanguage \"A &%Class of &%Arthropods that mainly dwells in water and has a segmented body and a chitinous exoskeleton. Includes lobsters, crabs, shrimp, and barnacles.\")").
:- load_kif("(subclass ColdBloodedVertebrate Vertebrate)").
:- load_kif("(disjointDecomposition ColdBloodedVertebrate Amphibian Fish Reptile)").
:- load_kif("(documentation ColdBloodedVertebrate EnglishLanguage \"&%Vertebrates whose body temperature is not internally regulated.\")").
:- load_kif("(subclass WarmBloodedVertebrate Vertebrate)").
:- load_kif("(disjoint WarmBloodedVertebrate ColdBloodedVertebrate)").
:- load_kif("(documentation WarmBloodedVertebrate EnglishLanguage \"&%Vertebrates whose body temperature is internally regulated.\")").
:- load_kif("(subclass Amphibian ColdBloodedVertebrate)").
:- load_kif("(documentation Amphibian EnglishLanguage \"A cold-blooded, smooth-skinned &%Vertebrate which characteristically hatches as an aquatic larva, breathing by gills. When mature, the &%Amphibian breathes with &%Lungs.\")").
:- load_kif("(subclass Bird WarmBloodedVertebrate)").
:- load_kif("(disjoint Bird Mammal)").
:- load_kif("(documentation Bird EnglishLanguage \"A &%Vertebrate having a constant body temperature and characterized by the presence of feathers.\")").
:- load_kif("(subclass Fish ColdBloodedVertebrate)").
:- load_kif("(documentation Fish EnglishLanguage \"A cold-blooded aquatic &%Vertebrate characterized by fins and breathing by gills. Included here are &%Fish having either a bony skeleton, such as a perch, or a cartilaginous skeleton, such as a shark. Also included are those &%Fish lacking a jaw, such as a lamprey or hagfish.\")").
:- load_kif("(=> (instance ?FISH Fish) (exists (?WATER) (and (inhabits ?FISH ?WATER) (instance ?WATER Water))))").
:- load_kif("(subclass Mammal WarmBloodedVertebrate)").
:- load_kif("(disjointDecomposition Mammal AquaticMammal HoofedMammal Marsupial Rodent Primate)").
:- load_kif("(documentation Mammal EnglishLanguage \"A &%Vertebrate having a constant body temperature and characterized by the presence of hair, mammary glands, and sweat glands.\")").
:- load_kif("(subclass AquaticMammal Mammal)").
:- load_kif("(documentation AquaticMammal EnglishLanguage \"The &%Class of &%Mammals that dwell chiefly in the water. Includes whales, dolphins, manatees, seals, and walruses.\")").
:- load_kif("(subclass HoofedMammal Mammal)").
:- load_kif("(documentation HoofedMammal EnglishLanguage \"The &%Class of quadruped &%Mammals with hooves. Includes horses, cows, sheep, pigs, antelope, etc.\")").
:- load_kif("(subclass Marsupial Mammal)").
:- load_kif("(documentation Marsupial EnglishLanguage \"The &%Class of &%Mammals which have a pouch for their young.\")").
:- load_kif("(subclass Carnivore Mammal)").
:- load_kif("(documentation Carnivore EnglishLanguage \"The &%Class of flesh-eating &%Mammals. Members of this &%Class typically have four or five claws on each paw. Includes cats, dogs, bears, racoons, and skunks.\")").
:- load_kif("(=> (and (instance ?CARNIVORE Carnivore) (instance ?EAT Eating) (agent ?EAT ?CARNIVORE) (patient ?EAT ?PREY)) (instance ?PREY Animal))").
:- load_kif("(subclass Canine Carnivore)").
:- load_kif("(disjoint Canine Feline)").
:- load_kif("(documentation Canine EnglishLanguage \"The &%Class of &%Carnivores with completely separable toes, nonretractable claws, and long muzzles.\")").
:- load_kif("(subclass Feline Carnivore)").
:- load_kif("(documentation Feline EnglishLanguage \"The &%Class of &%Carnivores with completely separable toes, slim bodies, and rounded heads. All felines other than the cheetah have retractable claws.\")").
:- load_kif("(subclass Rodent Mammal)").
:- load_kif("(documentation Rodent EnglishLanguage \"The &%Class of &%Mammals with one or two pairs of incisors for gnawing. Includes rats, mice, guinea pigs, and rabbits.\")").
:- load_kif("(subclass Primate Mammal)").
:- load_kif("(disjointDecomposition Primate Ape Monkey Hominid)").
:- load_kif("(documentation Primate EnglishLanguage \"The &%Class of &%Mammals which are &%Primates.\")").
:- load_kif("(subclass Ape Primate)").
:- load_kif("(documentation Ape EnglishLanguage \"Various &%Primates with no tails or only short tails.\")").
:- load_kif("(subclass Monkey Primate)").
:- load_kif("(documentation Monkey EnglishLanguage \"Various &%Primates with relatively long tails.\")").
:- load_kif("(subclass Hominid Primate)").
:- load_kif("(documentation Hominid EnglishLanguage \"Includes &%Humans and relatively recent ancestors of &%Humans.\")").
:- load_kif("(subclass Human Hominid)").
:- load_kif("(subclass Human CognitiveAgent)").
:- load_kif("(partition Human Man Woman)").
:- load_kif("(documentation Human EnglishLanguage \"Modern man, the only remaining species of the Homo genus.\")").
:- load_kif("(subclass Man Human)").
:- load_kif("(documentation Man EnglishLanguage \"The class of &%Male &%Humans.\")").
:- load_kif("(=> (instance ?MAN Man) (attribute ?MAN Male))").
:- load_kif("(subclass Woman Human)").
:- load_kif("(documentation Woman EnglishLanguage \"The class of &%Female &%Humans.\")").
:- load_kif("(=> (instance ?WOMAN Woman) (attribute ?WOMAN Female))").
:- load_kif("(subclass Reptile ColdBloodedVertebrate)").
:- load_kif("(documentation Reptile EnglishLanguage \"A &%ColdBloodedVertebrate having an external covering of scales or horny plates. &%Reptiles breathe by means of &%Lungs and generally lay eggs.\")").
% :- load_kif("; The following formulas cover biologically related &%Classes under &%Substance.").
:- load_kif("(subclass BiologicallyActiveSubstance Substance)").
:- load_kif("(documentation BiologicallyActiveSubstance EnglishLanguage \"A &%Substance that is capable of inducing a change in the structure or functioning of an &%Organism. This &%Class includes &%Substances used in the treatment, diagnosis, prevention or analysis of normal and abnormal body function. This &%Class also includes &%Substances that occur naturally in the body and are administered therapeutically. Finally, &%BiologicallyActiveSubstance includes &%Nutrients, most drugs of abuse, and agents that require special handling because of their toxicity.\")").
:- load_kif("(subclass Nutrient BiologicallyActiveSubstance)").
:- load_kif("(disjointDecomposition Nutrient Protein Carbohydrate Vitamin)").
:- load_kif("(documentation Nutrient EnglishLanguage \"A &%BiologicallyActiveSubstance required by an &%Organism. It is generally ingested as &%Food, and it is of primary interest because of its role in the biologic functioning of the &%Organism.\")").
:- load_kif("(subclass Protein Nutrient)").
:- load_kif("(documentation Protein EnglishLanguage \"A &%Nutrient made up of amino acids joined by peptide bonds.\")").
:- load_kif("(subclass Enzyme Protein)").
:- load_kif("(documentation Enzyme EnglishLanguage \"A complex &%Protein that is produced by living cells and which catalyzes specific biochemical reactions. There are six main types of enzymes: oxidoreductases, transferases, hydrolases, lyases, isomerases, and ligases.\")").
:- load_kif("(subclass Carbohydrate Nutrient)").
:- load_kif("(documentation Carbohydrate EnglishLanguage \"An element of living cells and a source of energy for &%Animals. This class includes both simple &%Carbohydrates, i.e. sugars, and complex &%Carbohydrates, i.e. starches.\")").
:- load_kif("(subclass Vitamin Nutrient)").
:- load_kif("(documentation Vitamin EnglishLanguage \"A &%Nutrient present in natural products or made synthetically, which is essential in the diet of &%Humans and other higher &%Animals. Included here are &%Vitamin precursors and provitamins.\")").
:- load_kif("(subclass LiquidMixture Mixture)").
:- load_kif("(partition LiquidMixture Solution Suspension)").
:- load_kif("(documentation LiquidMixture EnglishLanguage \"Any &%Mixture that satisfies two conditions, viz. it is made up predominantly of things which are a &%Liquid and any component other than &%Liquid in the &%Mixture is in the form of fine particles which are suspended in the &%Liquid.\")").
:- load_kif("(=> (instance ?MIX LiquidMixture) (exists (?PART) (and (part ?PART ?MIX) (attribute ?PART Liquid))))").
:- load_kif("(subclass Solution LiquidMixture)").
:- load_kif("(documentation Solution EnglishLanguage \"A liquid mixture. The most abundant component in a solution is called the solvent. Other components are called solutes. A solution, though homogeneous, may nonetheless have variable composition. Any amount of salt, up to a maximum limit, can be dissolved in a given amount of water.\")").
:- load_kif("(subclass Suspension LiquidMixture)").
:- load_kif("(documentation Suspension EnglishLanguage \"A &%LiquidMixture where at least one of the components of the &%Mixture is equally distributed throughout the &%Mixture but is not dissolved in it.\")").
:- load_kif("(subclass GasMixture Mixture)").
:- load_kif("(disjoint GasMixture LiquidMixture)").
:- load_kif("(documentation GasMixture EnglishLanguage \"Any &%Mixture that satisfies two conditions, viz. it is made up predominantly of things which are a &%Gas and any component other than &%Gas in the &%Mixture is in the form of fine particles which are suspended in the &%Gas.\")").
:- load_kif("(=> (instance ?MIX GasMixture) (exists (?PART) (and (part ?PART ?MIX) (attribute ?PART Gas))))").
:- load_kif("(subclass Cloud GasMixture)").
:- load_kif("(documentation Cloud EnglishLanguage \"Any &%GasMixture that is visible, e.g. &%Smoke produced by a fire or clouds of water vapor in the sky.\")").
:- load_kif("(=> (instance ?CLOUD Cloud) (capability Seeing patient ?CLOUD))").
:- load_kif("(subclass Smoke Cloud)").
:- load_kif("(documentation Smoke EnglishLanguage \"A mixture of fine particles suspended in a gas that is produced by &%Combustion.\")").
:- load_kif("(=> (instance ?SMOKE Smoke) (exists (?BURNING) (and (instance ?BURNING Combustion) (result ?BURNING ?SMOKE))))").
:- load_kif("(subclass WaterCloud Cloud)").
:- load_kif("(documentation WaterCloud EnglishLanguage \"Any &%Cloud that is composed primarily of water vapor.\")").
:- load_kif("(=> (instance ?CLOUD WaterCloud) (exists (?WATER) (and (instance ?WATER Water) (part ?WATER ?CLOUD))))").
:- load_kif("(=> (instance ?CLOUD WaterCloud) (forall (?PART) (=> (and (part ?PART ?CLOUD) (not (instance ?PART Water))) (exists (?WATER) (and (instance ?WATER Water) (part ?WATER ?CLOUD) (measure ?WATER ?MEASURE1) (measure ?PART ?MEASURE2) (greaterThan ?MEASURE1 ?MEASURE2))))))").
:- load_kif("(subclass Air GasMixture)").
:- load_kif("(documentation Air EnglishLanguage \"&%Air is the gaseous stuff that makes up the atmosphere surrounding Earth.\")").
:- load_kif("(=> (instance ?WIND Wind) (exists (?AIR) (and (patient ?WIND ?AIR) (instance ?AIR Air))))").
:- load_kif("(subclass BodySubstance Substance)").
:- load_kif("(documentation BodySubstance EnglishLanguage \"Extracellular material and mixtures of cells and extracellular material that are produced, excreted or accreted by an &%Organism. Included here are &%Substances such as saliva, dental enamel, sweat, hormones, and gastric acid.\")").
:- load_kif("(subclass AnimalSubstance BodySubstance)").
:- load_kif("(documentation AnimalSubstance EnglishLanguage \"&%BodySubstances that are produced exclusively by &%Animals.\")").
:- load_kif("(=> (and (instance ?SUBSTANCE AnimalSubstance) (instance ?ANIMAL Organism) (part ?SUBSTANCE ?ANIMAL)) (instance ?ANIMAL Animal))").
:- load_kif("(subclass PlantSubstance BodySubstance)").
:- load_kif("(documentation PlantSubstance EnglishLanguage \"&%BodySubstances that are produced exclusively by &%Plants.\")").
:- load_kif("(=> (and (instance ?SUBSTANCE PlantSubstance) (instance ?PLANT Organism) (part ?SUBSTANCE ?PLANT)) (instance ?PLANT Plant))").
:- load_kif("(subclass Hormone BodySubstance)").
:- load_kif("(subclass Hormone BiologicallyActiveSubstance)").
:- load_kif("(documentation Hormone EnglishLanguage \"In &%Animals, a chemical secreted by an endocrine gland whose products are released into the circulating fluid. &%Plant hormones or synthetic hormones which are used only to alter or control various physiologic processes, e.g., reproductive control agents, are assigned to the &%Class &%BiologicallyActiveSubstance. &%Hormones act as chemical messengers and regulate various physiologic processes such as growth, reproduction, metabolism, etc. They usually fall into two broad categories, viz. steroid hormones and peptide hormones.\")").
:- load_kif("(=> (instance ?HORMONE Hormone) (exists (?PROCESS ?GLAND) (and (instance ?GLAND Gland) (instrument ?PROCESS ?GLAND) (result ?PROCESS ?HORMONE))))").
:- load_kif("(subclass Blood BodySubstance)").
:- load_kif("(documentation Blood EnglishLanguage \"A fluid present in &%Animals that transports &%Nutrients to and waste products away from various &%BodyParts.\")").
:- load_kif("(instance FoodForFn UnaryFunction)").
:- load_kif("(domainSubclass FoodForFn 1 Organism)").
:- load_kif("(rangeSubclass FoodForFn SelfConnectedObject)").
:- load_kif("(documentation FoodForFn EnglishLanguage \"A &%Function that denotes &%SelfConnectedObject containing &%Nutrients, such as carbohydrates, proteins, and fats, that can be ingested by a the given class of living &%Animal and metabolized into energy and body tissue.\")").
:- load_kif("(=> (instance ?FOOD (FoodForFn ?A)) (exists (?NUTRIENT) (and (instance ?NUTRIENT Nutrient) (part ?NUTRIENT ?FOOD))))").
:- load_kif("(subclass Meat SelfConnectedObject)").
:- load_kif("(documentation Meat EnglishLanguage \"Any food which was originally part of an &%Animal and is not ingested by drinking, including eggs and animal blood that is eaten as food. Note that this class covers both raw meat and meat that has been prepared in some way, e.g. by cooking. Note too that preparations involving &%Meat and &%FruitOrVegetable are classed directly under &%Food.\")").
:- load_kif("(=> (instance ?MEAT Meat) (forall (?PART) (=> (part ?PART ?MEAT) (exists (?SUBPART ?TIME ?ANIMAL) (and (part ?SUBPART ?PART) (holdsDuring ?TIME (and (instance ?ANIMAL Animal) (part ?SUBPART ?ANIMAL))))))))").
% :- load_kif("; KJN: Moving this to Mid-level-ontology.kif as it is causing dependency errors. (subclass Beverage Substance) (disjoint Meat Beverage) (documentation Beverage EnglishLanguage \"Any food that is ingested by &%Drinking. Note that this class is disjoint &%Meat and &%FruitOrVegetable.\")").
% :- load_kif(" (=> (instance ?BEV Beverage) (attribute ?BEV Liquid))").
% :- load_kif(" (=> (and (instance ?DRINK Drinking) (patient ?DRINK ?BEV)) (instance ?BEV Beverage))").
:- load_kif("(subclass AnatomicalStructure OrganicObject)").
:- load_kif("(partition AnatomicalStructure BodyPart AbnormalAnatomicalStructure)").
:- load_kif("(partition AnatomicalStructure AnimalAnatomicalStructure PlantAnatomicalStructure)").
:- load_kif("(documentation AnatomicalStructure EnglishLanguage \"A normal or pathological part of the anatomy or structural organization of an &%Organism. This class covers &%BodyParts, as well as structures that are given off by &%Organisms, e.g. &%ReproductiveBodies.\")").
:- load_kif("(=> (instance ?ANAT AnatomicalStructure) (exists (?ORGANISM ?TIME) (and (instance ?ORGANISM Organism) (temporalPart ?TIME (WhenFn ?ORGANISM)) (holdsDuring ?TIME (part ?ANAT ?ORGANISM)))))").
:- load_kif("(=> (instance ?PART AnatomicalStructure) (exists (?CELL) (and (instance ?CELL Cell) (part ?CELL ?PART))))").
:- load_kif("(subclass AbnormalAnatomicalStructure AnatomicalStructure)").
:- load_kif("(documentation AbnormalAnatomicalStructure EnglishLanguage \"Any &%AnatomicalStructure which is not normally found in the &%Organism of which it is a part, i.e. it is the result of a &%PathologicProcess. This class covers tumors, birth marks, goiters, etc.\")").
:- load_kif("(=> (instance ?STRUCTURE AbnormalAnatomicalStructure) (exists (?PROC) (and (instance ?PROC PathologicProcess) (result ?PROC ?STRUCTURE))))").
:- load_kif("(subclass BodyPart AnatomicalStructure)").
:- load_kif("(documentation BodyPart EnglishLanguage \"A collection of &%Cells and &%Tissues which are localized to a specific area of an &%Organism and which are not pathological. The instances of this &%Class range from gross structures to small components of complex &%Organs.\")").
:- load_kif("(=> (instance ?PART BodyPart) (exists (?ORGANISM ?TIME) (and (instance ?ORGANISM Organism) (temporalPart ?TIME (WhenFn ?ORGANISM)) (holdsDuring ?TIME (component ?PART ?ORGANISM)))))").
:- load_kif("(=> (instance ?PART BodyPart) (exists (?PROC) (and (instance ?PROC PhysiologicProcess) (result ?PROC ?PART))))").
:- load_kif("(subclass AnimalAnatomicalStructure AnatomicalStructure)").
:- load_kif("(documentation AnimalAnatomicalStructure EnglishLanguage \"&%AnatomicalStructures that are possessed exclusively by &%Animals.\")").
:- load_kif("(=> (and (instance ?STRUCTURE AnimalAnatomicalStructure) (instance ?ANIMAL Organism) (part ?STRUCTURE ?ANIMAL)) (instance ?ANIMAL Animal))").
:- load_kif("(subclass PlantAnatomicalStructure AnatomicalStructure)").
:- load_kif("(documentation PlantAnatomicalStructure EnglishLanguage \"&%AnatomicalStructures that are possessed exclusively by &%Plants.\")").
:- load_kif("(=> (and (instance ?STRUCTURE PlantAnatomicalStructure) (instance ?PLANT Organism) (part ?STRUCTURE ?PLANT)) (instance ?PLANT Plant))").
:- load_kif("(subclass ReproductiveBody BodyPart)").
:- load_kif("(documentation ReproductiveBody EnglishLanguage \"Reproductive structure of &%Organisms. Consists of an &%Embryonic &%Object and a nutritive/protective envelope. Note that this class includes seeds, spores, and &%FruitOrVegetables, as well as the eggs produced by &%Animals.\")").
:- load_kif("(subclass Egg ReproductiveBody)").
:- load_kif("(subclass Egg AnimalAnatomicalStructure)").
:- load_kif("(documentation Egg EnglishLanguage \"The fertilized or unfertilized female &%ReproductiveBody of an &%Animal. This includes &%Bird and &%Reptile eggs, as well as mammalian ova.\")").
% :- load_kif("; KJN: This is moved from Economy.kif. Seed being subclassed from FruitOrVegetable seems incorrect, though, as FruitOrVegetable talks about a ripened Reproductive Body (although no formal rule states it) while a seed may not necessarily be so. commenting it out for now. (subclass Seed FruitOrVegetable)").
:- load_kif("(subclass Seed ReproductiveBody)").
:- load_kif("(subclass Seed PlantAnatomicalStructure)").
:- load_kif("(documentation Seed EnglishLanguage \"The fertilized or unfertilized female &%ReproductiveBody of a &%FloweringPlant.\")").
:- load_kif("(=> (instance ?SEED Seed) (exists (?PLANT ?TIME) (and (instance ?PLANT FloweringPlant) (holdsDuring ?TIME (part ?SEED ?PLANT)))))").
:- load_kif("(subclass Pollen ReproductiveBody)").
:- load_kif("(subclass Pollen PlantAnatomicalStructure)").
:- load_kif("(documentation Pollen EnglishLanguage \"A powder produced by &%FloweringPlants that contains male gametes and is capable of fertilizing the seeds of &%FloweringPlants of the same species.\")").
:- load_kif("(subclass FruitOrVegetable PlantAnatomicalStructure)").
:- load_kif("(subclass FruitOrVegetable ReproductiveBody)").
:- load_kif("(documentation FruitOrVegetable EnglishLanguage \"Any fruit or vegetable, i.e. a ripened &%ReproductiveBody of a &%Plant. Note that &%FruitOrVegetable is not a subclass of &%Food, because some fruits, e.g. poisonous berries, are not edible.\")").
:- load_kif("(subclass Spore ReproductiveBody)").
:- load_kif("(subclass Spore PlantAnatomicalStructure)").
:- load_kif("(documentation Spore EnglishLanguage \"Any &%ReproductiveBody of a &%NonFloweringPlant.\")").
:- load_kif("(=> (instance ?SPORE Spore) (exists (?PLANT ?TIME) (and (instance ?PLANT NonFloweringPlant) (holdsDuring ?TIME (part ?SPORE ?PLANT)))))").
:- load_kif("(subclass BodyCovering BodyPart)").
:- load_kif("(documentation BodyCovering EnglishLanguage \"Any &%BodyPart which is a covering of another &%BodyPart or of an entire &%Organism. This would include the rinds of &%FruitOrVegetables and the skins of &%Animals.\")").
:- load_kif("(=> (instance ?COVER BodyCovering) (exists (?BODY) (and (superficialPart ?COVER ?BODY) (or (instance ?BODY Organism) (instance ?BODY BodyPart)))))").
:- load_kif("(subclass BodyJunction BodyPart)").
:- load_kif("(documentation BodyJunction EnglishLanguage \"The place where two &%BodyParts meet or connect.\")").
:- load_kif("(=> (instance ?JUNCT BodyJunction) (exists (?OBJ1 ?OBJ2) (and (instance ?OBJ1 BodyPart) (instance ?OBJ2 BodyPart) (connects ?JUNCT ?OBJ1 ?OBJ2))))").
:- load_kif("(subclass BodyCavity BodyPart)").
:- load_kif("(documentation BodyCavity EnglishLanguage \"Any &%BodyPart which contains an unfilled space, e.g. &%BodyVessels, the atria and ventricles of the heart, the lungs, etc.\")").
:- load_kif("(subclass BodyVessel BodyCavity)").
:- load_kif("(documentation BodyVessel EnglishLanguage \"Any tube-like structure which occurs naturally in an &%Organism and through which a &%BodySubstance can circulate.\")").
:- load_kif("(subclass Cell BodyPart)").
:- load_kif("(documentation Cell EnglishLanguage \"The fundamental structural and functional unit of living &%Organisms.\")").
:- load_kif("(subclass Organ BodyPart)").
:- load_kif("(documentation Organ EnglishLanguage \"A somewhat independent &%BodyPart that performs a specialized function. Note that this functional definition covers bodily systems, e.g. the digestive system or the central nervous system.\")").
:- load_kif("(=> (instance ?ORGAN Organ) (exists (?PURP) (hasPurpose ?ORGAN ?PURP)))").
:- load_kif("(subclass Gland Organ)").
:- load_kif("(documentation Gland EnglishLanguage \"An &%Organ that removes &%Substances from the &%Blood, alters them in some way, and then releases them.\")").
:- load_kif("(subclass Tissue BodySubstance)").
:- load_kif("(disjointDecomposition Tissue Bone Muscle FatTissue)").
:- load_kif("(documentation Tissue EnglishLanguage \"An aggregation of similarly specialized &%Cells and the associated intercellular substance. &%Tissues are relatively non-localized in comparison to &%BodyParts, &%Organs or &%Organ components. The main features of &%Tissues are self-connectivity (see &%SelfConnectedObject) and being a homogeneous mass (all parts in the same granularity are instances of &%Tissue as well).\")").
:- load_kif("(=> (instance ?STUFF Tissue) (exists (?PART) (and (instance ?PART Cell) (part ?PART ?STUFF))))").
:- load_kif("(=> (instance ?STUFF Tissue) (exists (?ORGANISM) (and (instance ?ORGANISM Organism) (part ?STUFF ?ORGANISM))))").
:- load_kif("(subclass Bone Tissue)").
:- load_kif("(subclass Bone AnimalSubstance)").
:- load_kif("(documentation Bone EnglishLanguage \"Rigid &%Tissue composed largely of calcium that makes up the skeleton of &%Vertebrates. Note that this &%Class also includes teeth.\")").
:- load_kif("(=> (instance ?BONE Bone) (exists (?VERT) (and (instance ?VERT Vertebrate) (part ?BONE ?VERT))))").
:- load_kif("(subclass Muscle Tissue)").
:- load_kif("(subclass Muscle AnimalSubstance)").
:- load_kif("(documentation Muscle EnglishLanguage \"Nonrigid &%Tissue appearing only in &%Animals and composed largely of contractile cells.\")").
:- load_kif("(subclass FatTissue Tissue)").
:- load_kif("(documentation FatTissue EnglishLanguage \"Nonrigid &%Tissue that is composed largely of fat cells.\")").
:- load_kif("(subclass Noun Word)").
:- load_kif("(partition Word Noun Verb Adjective Adverb ParticleWord)").
:- load_kif("(documentation Noun EnglishLanguage \"One of the parts of speech. The &%Class of &%Words that conventionally denote &%Objects.\")").
:- load_kif("(subclass Verb Word)").
:- load_kif("(documentation Verb EnglishLanguage \"One of the parts of speech. The &%Class of &%Words that conventionally denote &%Processes.\")").
:- load_kif("(subclass Adjective Word)").
:- load_kif("(documentation Adjective EnglishLanguage \"One of the parts of speech. The &%Class of &%Words that conventionally denote &%Attributes of &%Objects.\")").
:- load_kif("(subclass Adverb Word)").
:- load_kif("(documentation Adverb EnglishLanguage \"One of the parts of speech. The &%Class of &%Words that conventionally denote &%Attributes of &%Processes.\")").
:- load_kif("(subclass ParticleWord Word)").
:- load_kif("(documentation ParticleWord EnglishLanguage \"An umbrella &%Class for any &%Word that does not fit into the other subclasses of &%Word. A &%ParticleWord is generally a small term that serves a grammatical or logical function, e.g. 'and', 'of', 'since', etc. At some point, this class might be broken up into the subclasses 'Connective', 'Preposition', etc. Note that the class &%ParticleWord includes both personal and possessive pronouns, e.g. 'she', 'hers', 'it', 'its', etc.\")").
:- load_kif("(subclass Morpheme LinguisticExpression)").
:- load_kif("(documentation Morpheme EnglishLanguage \"Part of a &%Word which cannot be subdivided and which expresses a meaning.\")").
:- load_kif("(=> (instance ?MORPH Morpheme) (not (exists (?OTHERMORPH) (and (instance ?OTHERMORPH Morpheme) (part ?OTHERMORPH ?MORPH) (not (equal ?OTHERMORPH ?MORPH))))))").
:- load_kif("(=> (instance ?MORPH Morpheme) (exists (?WORD) (and (instance ?WORD Word) (part ?MORPH ?WORD))))").
:- load_kif("(=> (instance ?WORD Word) (exists (?PART) (and (part ?PART ?WORD) (instance ?PART Morpheme))))").
:- load_kif("(subclass Phrase LinguisticExpression)").
:- load_kif("(disjointDecomposition Phrase VerbPhrase NounPhrase PrepositionalPhrase)").
:- load_kif("(documentation Phrase EnglishLanguage \"A set of &%Words in a &%Language which form a unit, i.e. express a meaning in the &%Language.\")").
:- load_kif("(=> (instance ?PHRASE Phrase) (exists (?PART1 ?PART2) (and (part ?PART1 ?PHRASE) (part ?PART2 ?PHRASE) (instance ?PART1 Word) (instance ?PART2 Word) (not (equal ?PART1 ?PART2)))))").
:- load_kif("(subclass VerbPhrase Phrase)").
:- load_kif("(documentation VerbPhrase EnglishLanguage \"A &%Phrase that has the same function as a &%Verb.\")").
:- load_kif("(=> (instance ?PHRASE VerbPhrase) (exists (?VERB) (and (instance ?VERB Verb) (part ?VERB ?PHRASE))))").
:- load_kif("(subclass NounPhrase Phrase)").
:- load_kif("(disjoint NounPhrase VerbPhrase)").
:- load_kif("(documentation NounPhrase EnglishLanguage \"A &%Phrase that has the same function as a &%Noun.\")").
:- load_kif("(=> (instance ?SENTENCE Sentence) (exists (?PHRASE1 ?PHRASE2) (and (instance ?PHRASE1 NounPhrase) (instance ?PHRASE2 VerbPhrase) (part ?PHRASE1 ?SENTENCE) (part ?PHRASE2 ?SENTENCE))))").
:- load_kif("(=> (instance ?PHRASE NounPhrase) (exists (?NOUN) (and (instance ?NOUN Noun) (part ?NOUN ?PHRASE))))").
:- load_kif("(subclass PrepositionalPhrase Phrase)").
:- load_kif("(documentation PrepositionalPhrase EnglishLanguage \"A &%Phrase that begins with a preposition and that functions as an &%Adjective or an &%Adverb.\")").
:- load_kif("(=> (instance ?PHRASE PrepositionalPhrase) (exists (?PREP) (and (instance ?PREP ParticleWord) (part ?PREP ?PHRASE))))").
:- load_kif("(subclass Text LinguisticExpression)").
:- load_kif("(subclass Text ContentBearingObject)").
:- load_kif("(subclass Text Artifact)").
:- load_kif("(documentation Text EnglishLanguage \"A &%LinguisticExpression or set of &%LinguisticExpressions that perform a specific function related to &%Communication, e.g. express a discourse about a particular topic, and that are inscribed in a &%CorpuscularObject by &%Humans.\")").
:- load_kif("(=> (instance ?TEXT Text) (exists (?PART) (and (part ?PART ?TEXT) (instance ?PART LinguisticExpression))))").
:- load_kif("(=> (instance ?TEXT Text) (exists (?WRITE) (and (instance ?WRITE Writing) (result ?WRITE ?TEXT))))").
:- load_kif("(subclass FactualText Text)").
:- load_kif("(disjoint FactualText FictionalText)").
:- load_kif("(documentation FactualText EnglishLanguage \"The class of &%Texts that purport to reveal facts about the world. Such texts are often known as information or as non-fiction. Note that something can be an instance of &%FactualText, even if it is wholly inaccurate. Whether something is a &%FactualText is determined by the beliefs of the agent creating the text.\")").
% :- load_kif("; NS: delete. The 2nd argument to &%authors must be a &%Class. (=> (and (instance ?TEXT FactualText) (authors ?AGENT ?TEXT) (subsumesContentInstance ?TEXT ?CONTENT) (instance ?CONTENT Formula)) (believes ?AGENT ?CONTENT))").
% :- load_kif("; NS: add.").
:- load_kif("(=> (and (instance ?TEXT ?CLASS) (subclass ?CLASS FactualText) (authors ?AGENT ?CLASS) (subsumesContentInstance ?TEXT ?CONTENT) (instance ?CONTENT Formula)) (believes ?AGENT ?CONTENT))").
:- load_kif("(subclass FictionalText Text)").
:- load_kif("(documentation FictionalText EnglishLanguage \"The class of &%Texts that purport to be largely a product of the author's imagination, i.e. the author does not believe that most of the content conveyed by the text is an accurate depiction of the real world. Note that something can be an instance of &%FictionalText, even if it is completely true. Whether something is a &%FictionalText is determined by the beliefs of the agent creating the text.\")").
% :- load_kif("; NS: delete. Wrong use of &%authors. Also, the rule is supposed to be about &%FictionalText, not &%FactualText. (=> (and (instance ?TEXT FactualText) (authors ?AGENT ?TEXT)) (exists (?CONTENT) (and (subsumesContentInstance ?TEXT ?CONTENT) (instance ?CONTENT Formula) (not (believes ?AGENT ?CONTENT)))))").
% :- load_kif("; NS: add.").
:- load_kif("(=> (and (instance ?TEXT ?CLASS) (subclass ?CLASS FictionalText) (authors ?AGENT ?CLASS)) (exists (?CONTENT) (and (subsumesContentInstance ?TEXT ?CONTENT) (instance ?CONTENT Formula) (not (believes ?AGENT ?CONTENT)))))").
:- load_kif("(subclass Sentence LinguisticExpression)").
:- load_kif("(documentation Sentence EnglishLanguage \"A syntactically well-formed formula of a &%Language. It includes, at minimum, a predicate and a subject (which may be explicit or implicit), and it expresses a &%Proposition.\")").
:- load_kif("(=> (instance ?SENT Sentence) (exists (?PROP) (and (instance ?PROP Proposition) (containsInformation ?SENT ?PROP))))").
:- load_kif("(instance authors BinaryPredicate)").
:- load_kif("(instance authors AsymmetricRelation)").
:- load_kif("(domain authors 1 Agent)").
:- load_kif("(domainSubclass authors 2 Text)").
% :- load_kif("; NS: edit.").
:- load_kif("(documentation authors EnglishLanguage \"(&%authors ?AGENT ?TEXTCLASS) means that ?AGENT is creatively responsible for the content of all instances of ?TEXTCLASS. For example, Agatha Christie is author of Murder_on_the_Orient_Express.\")").
% :- load_kif("; NS: edit.").
:- load_kif("(=> (authors ?AGENT ?TEXTCLASS) (exists (?PROCESS ?INSTANCE) (and (agent ?PROCESS ?AGENT) (instance ?INSTANCE ?TEXTCLASS) (result ?PROCESS ?INSTANCE))))").
:- load_kif("(instance editor BinaryPredicate)").
:- load_kif("(instance editor AsymmetricRelation)").
:- load_kif("(domain editor 1 Agent)").
:- load_kif("(domainSubclass editor 2 Text)").
% :- load_kif("; NS: edit.").
:- load_kif("(documentation editor EnglishLanguage \"(&%editor ?AGENT ?TEXTCLASS) means that ?AGENT is an editor of the content contained (or realized) in all instances of ?TEXTCLASS.\")").
:- load_kif("(instance publishes BinaryPredicate)").
:- load_kif("(instance publishes AsymmetricRelation)").
:- load_kif("(domain publishes 1 Organization)").
:- load_kif("(domainSubclass publishes 2 Text)").
% :- load_kif("; NS: edit.").
:- load_kif("(documentation publishes EnglishLanguage \"(&%publishes ?ORG ?TEXTCLASS) means that ?ORG publishes all instances of ?TEXTCLASS. For example, Bantam Books publishes Agatha Christie's Murder_on_the_Orient_Express.\")").
% :- load_kif("; NS: edit.").
:- load_kif("(=> (and (publishes ?ORG ?TEXTCLASS) (instance ?INST ?TEXTCLASS)) (exists (?PUB) (and (instance ?PUB Publication) (agent ?PUB ?ORG) (patient ?PUB ?INST))))").
:- load_kif("(instance EditionFn BinaryFunction)").
:- load_kif("(instance EditionFn PartialValuedRelation)").
:- load_kif("(domainSubclass EditionFn 1 ContentBearingObject)").
:- load_kif("(domain EditionFn 2 PositiveInteger)").
:- load_kif("(rangeSubclass EditionFn ContentBearingObject)").
:- load_kif("(documentation EditionFn EnglishLanguage \"A &%BinaryFunction that maps a type of text").
:- load_kif("(e.g. Agatha Christie's Murder_on_the_Orient_Express) and a number to the edition of the text type corresponding to the number.\")").
:- load_kif("(=> (and (equal (EditionFn ?TEXT ?INT1) ?EDITION1) (equal (EditionFn ?TEXT ?INT2) ?EDITION2) (greaterThan ?INT2 ?INT1) (instance ?PUB1 Publication) (instance ?PUB2 Publication) (instance ?CBO1 ?EDITION1) (instance ?CBO2 ?EDITION2) (patient ?PUB1 ?CBO1) (patient ?PUB2 ?CBO2) (date ?PUB1 ?DATE1) (date ?PUB2 ?DATE2)) (before (EndFn ?DATE1) (EndFn ?DATE2)))").
:- load_kif("(=> (equal (EditionFn ?TEXT1 ?NUMBER) ?TEXT2) (subsumesContentClass ?TEXT1 ?TEXT2))").
% :- load_kif("; KJN: Moving to MILO (instance SeriesVolumeFn BinaryFunction) (instance SeriesVolumeFn PartialValuedRelation) (domainSubclass SeriesVolumeFn 1 Series) (domain SeriesVolumeFn 2 PositiveInteger) (rangeSubclass SeriesVolumeFn Text) (documentation SeriesVolumeFn EnglishLanguage \"A &%BinaryFunction that maps a type of &%Series (e.g. the Encyclopedia_Britannica or the Popular_Mechanics periodical) and a number to the volumes of the text type designated by the number.\")").
% :- load_kif(" (=> (and (subclass ?TEXT Periodical) (equal (SeriesVolumeFn ?TEXT ?INT1) ?VOLUME1) (equal (SeriesVolumeFn ?TEXT ?INT2) ?VOLUME2) (greaterThan ?INT2 ?INT1) (instance ?PUB1 Publication) (instance ?PUB2 Publication) (instance ?CBO1 ?VOLUME1) (instance ?CBO2 ?VOLUME2) (patient ?PUB1 ?CBO1) (patient ?PUB2 ?CBO2) (date ?PUB1 ?DATE1) (date ?PUB2 ?DATE2)) (before (EndFn ?DATE1) (EndFn ?DATE2)))").
% :- load_kif(" (=> (equal (SeriesVolumeFn ?SERIES ?NUMBER) ?VOLUME) (subsumesContentClass ?SERIES ?VOLUME))").
% :- load_kif("; KJN: Moving to MILO (instance PeriodicalIssueFn BinaryFunction) (instance PeriodicalIssueFn PartialValuedRelation) (domainSubclass PeriodicalIssueFn 1 Periodical) (domain PeriodicalIssueFn 2 PositiveInteger) (rangeSubclass PeriodicalIssueFn Periodical) (documentation PeriodicalIssueFn EnglishLanguage \"A &%BinaryFunction that maps a subclass of &%Periodical and a number to all of the issues of the &%Periodical corresponding to the number.\")").
% :- load_kif(" (=> (equal (PeriodicalIssueFn ?PERIODICAL ?NUMBER) ?ISSUE) (subsumesContentClass ?PERIODICAL ?ISSUE))").
% :- load_kif("; KJN: Moving to MILO (subclass Book Text) (documentation Book EnglishLanguage \"A &%Text that has pages and is bound.\")").
:- load_kif("(subclass Summary Text)").
:- load_kif("(documentation Summary EnglishLanguage \"A short &%Text that is a summary of another, longer &%Text.\")").
:- load_kif("(=> (instance ?TEXT Summary) (exists (?TEXT2) (and (instance ?TEXT2 Text) (subsumesContentInstance ?TEXT2 ?TEXT))))").
% :- load_kif("; KJN: Moving to MILO (subclass Series Text) (documentation Series EnglishLanguage \"A &%Text consisting of multiple self-contained units. Some examples are an encyclopedia containing a couple dozen volumes, a television series made up of many episodes, a film serial, etc.\")").
% :- load_kif(" (=> (instance ?SERIES Series) (exists (?BOOK1 ?BOOK2) (and (instance ?BOOK1 Book) (instance ?BOOK2 Book) (subsumesContentInstance ?SERIES ?BOOK1) (subsumesContentInstance ?SERIES ?BOOK2) (not (equal ?BOOK1 ?BOOK2)))))").
% :- load_kif(" (subclass Periodical Series) (documentation Periodical EnglishLanguage \"A &%Series whose elements are published separately and on a periodic basis.\")").
% :- load_kif(" (subclass Article Text) (disjoint Article Book) (documentation Article EnglishLanguage \"A relatively short &%Text that either is unbound or is bound with other &%Articles in a &%Book.\")").
% :- load_kif(" (=> (and (instance ?ARTICLE1 Article) (instance ?BOOK Book) (subsumesContentInstance ?BOOK ?ARTICLE1)) (exists (?ARTICLE2) (and (instance ?ARTICLE2 Article) (not (equal ?ARTICLE2 ?ARTICLE1)) (subsumesContentInstance ?BOOK ?ARTICLE2))))").
:- load_kif("(subclass Certificate Text)").
:- load_kif("(documentation Certificate EnglishLanguage \"A &%Text that confers a right or obligation on the holder of the &%Certificate. Note that the right or obligation need not be a legal one, as in the case of an academic diploma that grants certain privileges in the professional world.\")").
:- load_kif("(=> (instance ?DOC Certificate) (exists (?PROP ?NORM) (or (confersNorm ?DOC ?PROP ?NORM) (deprivesNorm ?DOC ?PROP ?NORM))))").
:- load_kif("(subclass FinancialInstrument Certificate)").
:- load_kif("(documentation FinancialInstrument EnglishLanguage \"A document having monetary value or recording a monetary transaction\")").
:- load_kif("(subclass Currency FinancialInstrument)").
:- load_kif("(documentation Currency EnglishLanguage \"Any element of the official currrency of some &%Nation. This covers both &%CurrencyBills and &%CurrencyCoins.\")").
:- load_kif("(=> (instance ?CURRENCY Currency) (exists (?MEASURE) (monetaryValue ?CURRENCY ?MEASURE)))").
:- load_kif("(subclass Patent Certificate)").
:- load_kif("(documentation Patent EnglishLanguage \"A &%Certificate that expresses the content of an invention that has been accorded legal protection by a governemental entity.\")").
:- load_kif("(subclass Molecule CompoundSubstance)").
:- load_kif("(documentation Molecule EnglishLanguage \"A molecule is the smallest unit of matter of a &%CompoundSubstance that retains all the physical and chemical properties of that substance, e.g., Ne, H2, H2O. A molecule is two or more &%Atoms linked by a chemical bond.\")").
:- load_kif("(=> (instance ?MOLE Molecule) (exists (?ATOM1 ?ATOM2) (and (instance ?ATOM1 Atom) (instance ?ATOM2 Atom) (part ?ATOM1 ?MOLE) (part ?ATOM2 ?MOLE) (not (equal ?ATOM1 ?ATOM2)))))").
:- load_kif("(subclass Artifact Object)").
:- load_kif("(documentation Artifact EnglishLanguage \"An &%Object that is the product of a &%Making.\")").
:- load_kif("(<=> (instance ?ARTIFACT Artifact) (exists (?MAKING) (and (instance ?MAKING Making) (result ?MAKING ?ARTIFACT))))").
:- load_kif("(subclass Product Artifact)").
:- load_kif("(documentation Product EnglishLanguage \"An &%Artifact that is produced by &%Manufacture.\")").
:- load_kif("(=> (instance ?PRODUCT Product) (exists (?MANUFACTURE) (and (instance ?MANUFACTURE Manufacture) (result ?MANUFACTURE ?PRODUCT))))").
:- load_kif("(instance version BinaryPredicate)").
:- load_kif("(instance version AsymmetricRelation)").
:- load_kif("(instance version TransitiveRelation)").
:- load_kif("(domainSubclass version 1 Artifact)").
:- load_kif("(domainSubclass version 2 Artifact)").
:- load_kif("(documentation version EnglishLanguage \"Some &%Artifacts have a life cycle with discrete stages or versions. (&%version ARTIFACT1 ARTIFACT2) means that ARTIFACT1 is a version of ARTIFACT2. Note that this &%Predicate relates subclasses of &%Artifact and not instances.\")").
:- load_kif("(=> (version ?ARTIFACT1 ?ARTIFACT2) (subclass ?ARTIFACT1 ?ARTIFACT2))").
% :- load_kif("; The following part of the ontology will eventually encompass all artifacts. For the time being, it is mostly restricted to the content of the Ontolingua ontology component-assemblies, which covers the types of elements used to construct engineering systems.").
:- load_kif("(subclass StationaryArtifact Artifact)").
:- load_kif("(documentation StationaryArtifact EnglishLanguage \"A &%StationaryArtifact is an &%Artifact that has a fixed spatial location. Most instances of this &%Class are architectural works, e.g. the Eiffel Tower, the Great Pyramids, office towers, single-family houses, etc.\")").
:- load_kif("(=> (instance ?ARTIFACT StationaryArtifact) (exists (?PLACE) (and (holdsDuring (WhenFn ?ARTIFACT) (located ?ARTIFACT ?PLACE)) (not (exists (?P2) (and (holdsDuring (WhenFn ?ARTIFACT) (located ?ARTIFACT ?P2)) (not (equal ?PLACE ?P2))))))))").
:- load_kif("(subclass Building StationaryArtifact)").
:- load_kif("(documentation Building EnglishLanguage \"The Class of &%StationaryArtifacts which are intended to house &%Humans and their activities.\")").
:- load_kif("(=> (instance ?BUILDING Building) (exists (?HUMAN) (and (instance ?HUMAN Human) (or (inhabits ?HUMAN ?BUILDING) (exists (?ACT) (and (agent ?ACT ?HUMAN) (located ?ACT ?BUILDING)))))))").
:- load_kif("(subclass Room StationaryArtifact)").
:- load_kif("(disjoint Room Building)").
:- load_kif("(documentation Room EnglishLanguage \"A &%properPart of a &%Building which is separated from the exterior of the &%Building and/or other &%Rooms of the &%Building by walls. Some &%Rooms may have a specific purpose, e.g. sleeping, bathing, cooking, entertainment, etc.\")").
:- load_kif("(=> (instance ?ROOM Room) (exists (?BUILD) (and (instance ?BUILD Building) (properPart ?ROOM ?BUILD))))").
:- load_kif("(subclass House ResidentialBuilding)").
:- load_kif("(subclass House SingleFamilyResidence)").
:- load_kif("(documentation House EnglishLanguage \"A &%ResidentialBuilding which is intended to be inhabited by members of the same &%SocialUnit. &%Houses are distinguished from temporary housing like hotels and multi-family dwellings like condominium and apartment buildings.\")").
:- load_kif("(subclass Residence StationaryArtifact)").
:- load_kif("(partition Residence PermanentResidence TemporaryResidence)").
:- load_kif("(documentation Residence EnglishLanguage \"A &%Building or part of a &%Building which provides some accomodation for sleeping.\")").
:- load_kif("(=> (instance ?RESIDENCE Residence) (or (instance ?RESIDENCE House) (exists (?BUILDING) (and (instance ?BUILDING ResidentialBuilding) (part ?RESIDENCE ?BUILDING)))))").
:- load_kif("(subclass PermanentResidence Residence)").
:- load_kif("(documentation PermanentResidence EnglishLanguage \"A &%Residence where people live, i.e. where people have a &%home.\")").
:- load_kif("(=> (instance ?RESIDENCE PermanentResidence) (exists (?PERSON) (home ?PERSON ?RESIDENCE)))").
:- load_kif("(subclass TemporaryResidence Residence)").
:- load_kif("(documentation TemporaryResidence EnglishLanguage \"A &%Residence which is strictly temporary, i.e. where no one makes his/her &%home.\")").
:- load_kif("(=> (instance ?RESIDENCE TemporaryResidence) (not (exists (?PERSON) (home ?PERSON ?RESIDENCE))))").
:- load_kif("(subclass ResidentialBuilding Building)").
:- load_kif("(subclass ResidentialBuilding Residence)").
:- load_kif("(documentation ResidentialBuilding EnglishLanguage \"A &%Building which provides some accomodation for sleeping. Note that this class does not cover just permanent residences, e.g. &%Houses and condominium and apartment buildings, but also temporary residences, e.g. hotels and dormitories. &%ResidentialBuildings are also distinguished from &%CommercialBuildings, which are intended to serve an organizational rather than a residential function.\")").
% :- load_kif("; Moved hotel definition to Hotel.kif. (subclass Hotel ResidentialBuilding) (subclass Hotel TemporaryResidence) (subclass Hotel CommercialAgent) (documentation Hotel EnglishLanguage \"A &%ResidentialBuilding which provides temporary accommodations to guests in exchange for money.\")").
:- load_kif("(subclass SingleFamilyResidence PermanentResidence)").
:- load_kif("(documentation SingleFamilyResidence EnglishLanguage \"A &%PermanentResidence which is intended to be the &%home of a single &%SocialUnit. This class covers &%Houses, &%ApartmentUnits, and &%CondominiumUnits.\")").
:- load_kif("(=> (instance ?RESIDENCE SingleFamilyResidence) (hasPurpose ?RESIDENCE (forall (?AGENT1 ?AGENT2) (=> (and (home ?AGENT1 ?RESIDENCE) (home ?AGENT2 ?RESIDENCE)) (exists (?UNIT) (and (instance ?UNIT SocialUnit) (member ?AGENT1 ?UNIT) (member ?AGENT2 ?UNIT)))))))").
:- load_kif("(subclass ArtWork Artifact)").
:- load_kif("(documentation ArtWork EnglishLanguage \"&%Artifacts that are created primarily for aesthetic appreciation. Note that this &%Class does not include most examples of architecture, which belong under &%StationaryArtifact.\")").
:- load_kif("(=> (instance ?AW ArtWork) (hasPurpose ?AW (exists (?H ?P) (and (instance ?H Human) (instance ?P Perception) (experiencer ?P ?H) (patient ?P ?AW)))))").
:- load_kif("(=> (instance ?AW ArtWork) (hasPurpose ?AW (exists (?P) (and (instance ?H Human) (desires ?H (exists (?P) (and (instance ?P Perception) (experiencer ?P ?H) (patient ?P ?AW))))))))").
:- load_kif("(subclass RepresentationalArtWork ArtWork)").
:- load_kif("(subclass RepresentationalArtWork Icon)").
:- load_kif("(documentation RepresentationalArtWork EnglishLanguage \"Any &%ArtWork that &%represents something &%Physical.\")").
:- load_kif("(=> (instance ?RA RepresentationalArtWork) (exists (?P) (and (instance ?P Physical) (represents ?RA ?P))))").
:- load_kif("(subclass Fabric Artifact)").
:- load_kif("(disjoint Fabric StationaryArtifact)").
:- load_kif("(documentation Fabric EnglishLanguage \"&%Artifacts that are created by weaving together natural or synthetic fibers or by treating the skins of certain sorts of &%Animals. Note that this &%Class includes articles that are created by stitching together various types of fabrics, e.g. bedspreads. On the other hand, &%Clothing is not a &%subclass of &%Fabric, because many clothing items contain elements that are not fabrics.\")").
:- load_kif("(subclass WearableItem Artifact)").
:- load_kif("(documentation WearableItem EnglishLanguage \"&%WearableItem is the subclass of &%Artifacts that are made to be worn on the body.\")").
:- load_kif("(=> (instance ?WI WearableItem) (hasPurpose ?WI (exists (?H) (and (instance ?H Human) (wears ?H ?WI)))))").
:- load_kif("(subclass Clothing WearableItem)").
:- load_kif("(disjoint Clothing StationaryArtifact)").
:- load_kif("(documentation Clothing EnglishLanguage \"&%Artifact made out of fabrics and possibly other materials that are used to cover the bodies of &%Humans.\")").
:- load_kif("(=> (instance ?CLOTHING Clothing) (exists (?FABRIC) (and (instance ?FABRIC Fabric) (part ?FABRIC ?CLOTHING))))").
:- load_kif("(instance wears BinaryPredicate)").
:- load_kif("(domain wears 1 Animal)").
:- load_kif("(domain wears 2 WearableItem)").
:- load_kif("(documentation wears EnglishLanguage \"(&%wears ?AGENT ?WI) means that ?AGENT is wearing the &%WearableItem item ?WI.\")").
:- load_kif("(=> (wears ?AGENT ?WI) (located ?WI ?AGENT))").
:- load_kif("(=> (and (wears ?A ?C) (part ?P ?C)) (wears ?A ?P))").
:- load_kif("(subclass Device Artifact)").
:- load_kif("(documentation Device EnglishLanguage \"A &%Device is an &%Artifact whose purpose is to serve as an &%instrument in a specific subclass of &%Process.\")").
:- load_kif("(=> (instance ?DEVICE Device) (exists (?PROC) (capability ?PROC instrument ?DEVICE)))").
:- load_kif("(=> (instance ?DEVICE Device) (exists (?PROC) (hasPurpose ?DEVICE (exists (?INST) (and (instance ?INST ?PROC) (instrument ?INST ?DEVICE))))))").
:- load_kif("(subclass MusicalInstrument Device)").
:- load_kif("(documentation MusicalInstrument EnglishLanguage \"A &%Device which is manipulated by a &%Human and whose purpose is &%MakingMusic.\")").
:- load_kif("(=> (instance ?INSTRUMENT MusicalInstrument) (capability MakingMusic instrument ?INSTRUMENT))").
:- load_kif("(subclass TransportationDevice Device)").
:- load_kif("(documentation TransportationDevice EnglishLanguage \"A &%TransportationDevice is a &%Device which serves as the &%instrument in a &%Transportation &%Process which carries the &%patient of the &%Process from one point to another.\")").
:- load_kif("(=> (instance ?DEVICE TransportationDevice) (capability Transportation instrument ?DEVICE))").
:- load_kif("(subclass Vehicle TransportationDevice)").
:- load_kif("(documentation Vehicle EnglishLanguage \"&%Vehicle is the subclass of &%TransportationDevices that transport passengers or goods from one place to another by moving from one place to the other with them, e.g., cars, trucks, ferries, and airplanes. Contrast with devices such as pipelines, escalators, or supermarket checkout belts, which carry items from one place to another by means of a moving part, without the device removing from the origin to the destination.\")").
:- load_kif("(=> (and (instance ?TRANSPORT Vehicle) (instance ?MOVE Translocation) (instrument ?MOVE ?TRANSPORT) (origin ?MOVE ?FROM)) (holdsDuring (BeginFn (WhenFn ?MOVE)) (located ?TRANSPORT ?FROM)))").
:- load_kif("(=> (and (instance ?TRANSPORT Vehicle) (instance ?MOVE Translocation) (instrument ?MOVE ?TRANSPORT) (destination ?MOVE ?TO)) (holdsDuring (BeginFn (WhenFn ?MOVE)) (located ?TRANSPORT ?TO)))").
:- load_kif("(subclass MeasuringDevice Device)").
:- load_kif("(documentation MeasuringDevice EnglishLanguage \"Any &%Device whose purpose is to measure a &%PhysicalQuantity.\")").
:- load_kif("(=> (instance ?DEVICE MeasuringDevice) (hasPurpose ?DEVICE (exists (?MEASURE) (and (instance ?MEASURE Measuring) (instrument ?MEASURE ?DEVICE)))))").
:- load_kif("(subclass AttachingDevice Device)").
:- load_kif("(documentation AttachingDevice EnglishLanguage \"A &%Device whose purpose is to attach one thing to something else, e.g. nails, screws, buttons, etc.\")").
:- load_kif("(=> (instance ?DEVICE AttachingDevice) (exists (?ATTACH) (and (instance ?ATTACH Attaching) (instrument ?ATTACH ?DEVICE))))").
:- load_kif("(subclass Weapon Device)").
:- load_kif("(documentation Weapon EnglishLanguage \"The &%Class of &%Devices that are designed primarily to damage or destroy &%Humans/&%Animals, &%StationaryArtifacts or the places inhabited by &%Humans/&%Animals.\")").
:- load_kif("(=> (instance ?WEAPON Weapon) (capability Damaging instrument ?WEAPON))").
:- load_kif("(=> (instance ?WEAPON Weapon) (hasPurpose ?WEAPON (exists (?D ?PATIENT) (and (instance ?D Damaging) (instrument ?D ?WEAPON) (patient ?D ?PATIENT)))))").
:- load_kif("(subclass Machine Device)").
:- load_kif("(documentation Machine EnglishLanguage \"&%Machines are &%Devices that that have a well-defined &%resource and &%result and that automatically convert the &%resource into the &%result.\")").
:- load_kif("(=> (instance ?MACHINE Machine) (forall (?PROC) (=> (instrument ?PROC ?MACHINE) (exists (?RESOURCE ?RESULT) (and (resource ?PROC ?RESOURCE) (result ?PROC ?RESULT))))))").
:- load_kif("(subclass EngineeringComponent Device)").
:- load_kif("(documentation EngineeringComponent EnglishLanguage \"A fundamental concept that applies in many engineering domains. An &%EngineeringComponent is an element of a &%Device that is a physically whole object, such as one might see listed as standard parts in a catalog. The main difference betweeen &%EngineeringComponents and arbitrary globs of matter is that &%EngineeringComponents are object-like in a modeling sense. Thus, an &%EngineeringComponent is not an arbtrary subregion, but a part of a system with a stable identity.\")").
:- load_kif("(=> (instance ?COMP EngineeringComponent) (exists (?DEVICE) (and (instance ?DEVICE Device) (component ?COMP ?DEVICE))))").
:- load_kif("(=> (instance ?MACHINE Machine) (exists (?COMP1 ?COMP2) (and (instance ?COMP1 EngineeringComponent) (instance ?COMP2 EngineeringComponent) (not (equal ?COMP1 ?COMP2)) (part ?COMP1 ?MACHINE) (part ?COMP2 ?MACHINE))))").
:- load_kif("(subrelation engineeringSubcomponent properPart)").
:- load_kif("(domain engineeringSubcomponent 1 EngineeringComponent)").
:- load_kif("(domain engineeringSubcomponent 2 EngineeringComponent)").
:- load_kif("(documentation engineeringSubcomponent EnglishLanguage \"(&%engineeringSubcomponent ?SUB ?SUPER) means that the &%EngineeringComponent ?SUB is structurally a &%properPart of ?SUPER. This relation is an &%AsymmetricRelation, since two &%EngineeringComponents cannot be subcomponents of each other.\")").
:- load_kif("(instance connectedEngineeringComponents SymmetricRelation)").
:- load_kif("(instance connectedEngineeringComponents IrreflexiveRelation)").
:- load_kif("(subrelation connectedEngineeringComponents connected)").
:- load_kif("(domain connectedEngineeringComponents 1 EngineeringComponent)").
:- load_kif("(domain connectedEngineeringComponents 2 EngineeringComponent)").
:- load_kif("(documentation connectedEngineeringComponents EnglishLanguage \"This is the most general connection relation between &%EngineeringComponents. If"). (&%connectedEngineeringComponents ?COMP1 ?COMP2), then neither ?COMP1 nor ?COMP2 can be an &%engineeringSubcomponent of the other. The relation &%connectedEngineeringComponents is a &%SymmetricRelation, there is no information in the direction of connection between two components. It is also an &%IrreflexiveRelation, no &%EngineeringComponent bears this relation to itself. Note that this relation does not associate a name or type with the connection.\")").
:- load_kif("(=> (connectedEngineeringComponents ?COMP1 ?COMP2) (and (not (engineeringSubcomponent ?COMP1 ?COMP2)) (not (engineeringSubcomponent ?COMP2 ?COMP1))))").
:- load_kif("(subclass EngineeringConnection EngineeringComponent)").
:- load_kif("(documentation EngineeringConnection EnglishLanguage \"An &%EngineeringConnection is an &%EngineeringComponent that represents a connection relationship between two other &%EngineeringComponents. It is a reification of the &%Predicate &%connectedEngineeringComponents. That means that whenever this &%Predicate holds between two &%EngineeringComponents, there exists an &%EngineeringConnection. The practical reason for reifying a relationship is to be able to attach other information about it. For example, one might want to say that a particular connection is associated with some shared parameters, or that it is of a particular type. &%EngineeringConnections are &%EngineeringComponents and can therefore be an &%engineeringSubcomponent of other &%EngineeringComponents. However, to provide for modular regularity in component systems, &%EngineeringConnections cannot be connected. For each pair of &%EngineeringComponents related by &%connectedEngineeringComponents, there exists at least one &%EngineeringConnection. However, that object may not be unique, and the same &%EngineeringConnection may be associated with several pairs of &%EngineeringComponents.\")").
:- load_kif("(=> (instance ?CONNECTION EngineeringConnection) (exists (?COMP1 ?COMP2) (connectsEngineeringComponents ?CONNECTION ?COMP1 ?COMP2)))").
:- load_kif("(=> (connectedEngineeringComponents ?COMP1 ?COMP2) (not (or (instance ?COMP1 EngineeringConnection) (instance ?COMP2 EngineeringConnection))))").
:- load_kif("(<=> (connectedEngineeringComponents ?COMP1 ?COMP2) (exists (?CONNECTION) (connectsEngineeringComponents ?CONNECTION ?COMP1 ?COMP2)))").
:- load_kif("(subrelation connectsEngineeringComponents connects)").
:- load_kif("(domain connectsEngineeringComponents 1 EngineeringConnection)").
:- load_kif("(domain connectsEngineeringComponents 2 EngineeringComponent)").
:- load_kif("(domain connectsEngineeringComponents 3 EngineeringComponent)").
:- load_kif("(documentation connectsEngineeringComponents EnglishLanguage \"&%connectsEngineeringComponents is a &%TernaryPredicate that maps from an &%EngineeringConnection to the &%EngineeringComponents it connects. Since &%EngineeringComponents cannot be connected to themselves and there cannot be an &%EngineeringConnection without a &%connectedEngineeringComponents &%Predicate, the second and third arguments of any &%connectsEngineeringComponents relationship will always be distinct for any given first argument.\")").
% :- load_kif("; This following part contains definitions and axioms relating to social groups and the relations between them.").
:- load_kif("(subclass CommercialAgent Agent)").
:- load_kif("(documentation CommercialAgent EnglishLanguage \"An &%Agent that provides products and/or services for a fee with the aim of making a profit.\")").
% :- load_kif("; KJN: Moved from Media.kif").
:- load_kif("(subclass Business CommercialAgent)").
:- load_kif("(subclass Business Organization)").
:- load_kif("(documentation Business EnglishLanguage \"An &%instance of &%Business is an &%Organization that is a &%CommercialAgent.\")").
% :- load_kif("; KJN: Removing this and putting in definition of Corporation that subclasses it from Business (subclass Corporation CommercialAgent) (subclass Corporation Organization)").
:- load_kif("(subclass Corporation Business) NS: add.").
:- load_kif("(subclass Corporation LegalAgent)").
:- load_kif("(documentation Corporation EnglishLanguage \"An &%Organization that has a special legal status that allows a group of persons to act as a &%CommercialAgent and that insulates the owners (shareholders) from many liabilities that might result from the corporation's operation.\")").
:- load_kif("(subclass Manufacturer Corporation)").
:- load_kif("(documentation Manufacturer EnglishLanguage \"Any &%Corporation which manufactures &%Products.\")").
:- load_kif("(=> (instance ?ORG Manufacturer) (hasPurpose ?ORG (exists (?MANUFACTURE) (and (instance ?MANUFACTURE Manufacture) (instance ?MANUFACTURE CommercialService) (agent ?MANUFACTURE ?ORG)))))").
:- load_kif("(subclass MercantileOrganization Corporation)").
:- load_kif("(documentation MercantileOrganization EnglishLanguage \"Any &%Corporation which sells goods or services to customers for a profit.\")").
:- load_kif("(=> (instance ?ORG MercantileOrganization) (hasPurpose ?ORG (exists (?SELL) (and (instance ?SELL Selling) (instance ?SELL CommercialService) (agent ?SELL ?ORG)))))").
:- load_kif("(subclass Group Collection)").
:- load_kif("(subclass Group Agent)").
:- load_kif("(documentation Group EnglishLanguage \"A &%Collection of &%Agents, e.g. a flock of sheep, a herd of goats, or the local Boy Scout troop.\")").
:- load_kif("(=> (and (instance ?GROUP Group) (member ?MEMB ?GROUP)) (instance ?MEMB Agent))").
:- load_kif("(subclass GroupOfPeople Group)").
:- load_kif("(documentation GroupOfPeople EnglishLanguage \"Any &%Group whose &%members are exclusively &%Humans.\")").
:- load_kif("(=> (and (instance ?GROUP GroupOfPeople) (member ?MEMBER ?GROUP)) (instance ?MEMBER Human))").
:- load_kif("(subclass AgeGroup GroupOfPeople)").
:- load_kif("(documentation AgeGroup EnglishLanguage \"A &%GroupOfPeople whose &%members all have the same &%age.\")").
:- load_kif("(=> (instance ?GROUP AgeGroup) (forall (?MEMB1 ?MEMB2 ?AGE1 ?AGE2) (=> (and (member ?MEMB1 ?GROUP) (member ?MEMB2 ?GROUP) (age ?MEMB1 ?AGE1) (age ?MEMB2 ?AGE2)) (equal ?AGE1 ?AGE2))))").
:- load_kif("(subclass FamilyGroup GroupOfPeople)").
:- load_kif("(documentation FamilyGroup EnglishLanguage \"A &%GroupOfPeople whose &%members bear &%familyRelations to one another.\")").
:- load_kif("(=> (instance ?GROUP FamilyGroup) (forall (?MEMB1 ?MEMB2) (=> (and (member ?MEMB1 ?GROUP) (member ?MEMB2 ?GROUP)) (familyRelation ?MEMB1 ?MEMB2))))").
:- load_kif("(subclass SocialUnit GroupOfPeople)").
:- load_kif("(documentation SocialUnit EnglishLanguage \"A &%GroupOfPeople who all have the same &%home.\")").
:- load_kif("(=> (instance ?UNIT SocialUnit) (exists (?HOME) (=> (member ?MEMBER ?UNIT) (home ?MEMBER ?HOME))))").
:- load_kif("(instance ImmediateFamilyFn UnaryFunction)").
:- load_kif("(domain ImmediateFamilyFn 1 Human)").
:- load_kif("(range ImmediateFamilyFn FamilyGroup)").
:- load_kif("(documentation ImmediateFamilyFn EnglishLanguage \"(&%ImmediateFamilyFn ?PERSON) denotes the immediate family of ?PERSON, i.e. the &%Group consisting of the &%parents of ?PERSON and anyone of whom ?PERSON is a &%parent.\")").
:- load_kif("(=> (equal (ImmediateFamilyFn ?P) ?FAMILY) (forall (?MEMBER) (=> (member ?MEMBER ?FAMILY) (exists (?OTHER) (or (parent ?MEMBER ?OTHER) (parent ?OTHER ?MEMBER))))))").
:- load_kif("(=> (equal (ImmediateFamilyFn ?PERSON) ?FAMILY) (exists (?MEMBER) (and (member ?MEMBER ?FAMILY) (or (parent ?MEMBER ?PERSON) (parent ?PERSON ?MEMBER)))))").
:- load_kif("(instance relative BinaryPredicate)").
:- load_kif("(instance relative SymmetricRelation)").
:- load_kif("(domain relative 1 Organism)").
:- load_kif("(domain relative 2 Organism)").
:- load_kif("(documentation relative EnglishLanguage \"(&%relative ?O1 ?O2) means that ?O1 and ?O2 are relatives, whether through common ancestry (consanguinity), someone's marriage (affinity), or someone's adoption. This definition is intentionally broad, so as to capture a wide array of `familial' relations. The notion of who counts as `family' also varies between cultures, but that aspect of meaning is not addressed here.\")").
:- load_kif("(subrelation familyRelation relative)").
:- load_kif("(subrelation spouse relative)").
:- load_kif("(=> (and (holdsDuring ?T1 (legalRelation ?A1 ?A2)) (instance ?A1 Organism) (instance ?A2 Organism)) (holdsDuring ?T1 (relative ?A1 ?A2)))").
:- load_kif("(instance familyRelation BinaryPredicate)").
:- load_kif("(instance familyRelation EquivalenceRelation)").
:- load_kif("(domain familyRelation 1 Organism)").
:- load_kif("(domain familyRelation 2 Organism)").
:- load_kif("(documentation familyRelation EnglishLanguage \"A very general &%Predicate for biological relationships. (&%familyRelation ?ORGANISM1 ?ORGANISM2) means that ?ORGANISM1 and ?ORGANISM2 are biologically derived from a common ancestor.\")").
:- load_kif("(=> (familyRelation ?ORGANISM1 ?ORGANISM2) (exists (?ORGANISM3) (and (ancestor ?ORGANISM3 ?ORGANISM1) (ancestor ?ORGANISM3 ?ORGANISM2))))").
:- load_kif("(subrelation ancestor familyRelation)").
:- load_kif("(instance ancestor TransitiveRelation)").
:- load_kif("(instance ancestor IrreflexiveRelation)").
:- load_kif("(domain ancestor 1 Organism)").
:- load_kif("(domain ancestor 2 Organism)").
:- load_kif("(documentation ancestor EnglishLanguage \"The transitive closure of the &%parent predicate.  (&%ancestor ?DESCENDANT ?ANCESTOR) means that ?ANCESTOR is either the &%parent of ?DESCENDANT or the &%parent of the &%parent of &%DESCENDANT or etc.\")").
:- load_kif("(subrelation parent ancestor)").
:- load_kif("(instance parent BinaryPredicate)").
:- load_kif("(subrelation parent familyRelation)").
:- load_kif("(instance parent AsymmetricRelation)").
:- load_kif("(instance parent IntransitiveRelation)").
:- load_kif("(domain parent 1 Organism)").
:- load_kif("(domain parent 2 Organism)").
:- load_kif("(documentation parent EnglishLanguage \"The general relationship of parenthood.  (&%parent ?CHILD ?PARENT) means that ?PARENT is a biological parent of ?CHILD.\")").
:- load_kif("(=> (parent ?CHILD ?PARENT) (before (BeginFn (WhenFn ?PARENT)) (BeginFn (WhenFn ?CHILD))))").
:- load_kif("(=> (and (parent ?CHILD ?PARENT) (subclass ?CLASS Organism) (instance ?PARENT ?CLASS)) (instance ?CHILD ?CLASS))").
:- load_kif("(=> (and (parent ?CHILD ?PARENT) (instance ?REP SexualReproduction) (agent ?REP ?PARENT) (result ?REP ?CHILD)) (or (mother ?CHILD ?PARENT) (father ?CHILD ?PARENT)))").
:- load_kif("(=> (instance ?ORGANISM Organism) (exists (?PARENT) (parent ?ORGANISM ?PARENT)))").
:- load_kif("(instance mother SingleValuedRelation)").
:- load_kif("(subrelation mother parent)").
:- load_kif("(domain mother 1 Organism)").
:- load_kif("(domain mother 2 Organism)").
:- load_kif("(documentation mother EnglishLanguage \"The general relationship of motherhood.  (&%mother ?CHILD ?MOTHER) means that ?MOTHER is the biological mother of ?CHILD.\")").
:- load_kif("(=> (mother ?CHILD ?MOTHER) (attribute ?MOTHER Female))").
:- load_kif("(instance father SingleValuedRelation)").
:- load_kif("(subrelation father parent)").
:- load_kif("(domain father 1 Organism)").
:- load_kif("(domain father 2 Organism)").
:- load_kif("(documentation father EnglishLanguage \"The general relationship of fatherhood.  (&%father ?CHILD ?FATHER) means that ?FATHER is the biological father of ?CHILD.\")").
:- load_kif("(=> (father ?CHILD ?FATHER) (attribute ?FATHER Male))").
:- load_kif("(subrelation daughter parent)").
:- load_kif("(domain daughter 1 Organism)").
:- load_kif("(domain daughter 2 Organism)").
:- load_kif("(documentation daughter EnglishLanguage \"The general relationship of daughterhood.  (&%daughter ?CHILD ?PARENT) means that ?CHILD is the biological daughter of ?PARENT.\")").
:- load_kif("(=> (daughter ?CHILD ?PARENT) (attribute ?CHILD Female))").
:- load_kif("(subrelation son parent)").
:- load_kif("(domain son 1 Organism)").
:- load_kif("(domain son 2 Organism)").
:- load_kif("(documentation son EnglishLanguage \"The general relationship of being a son.  (&%son ?CHILD ?PARENT) means that ?CHILD is the biological son of ?PARENT.\")").
:- load_kif("(=> (son ?CHILD ?PARENT) (attribute ?CHILD Male))").
:- load_kif("(instance sibling BinaryPredicate)").
:- load_kif("(subrelation sibling familyRelation)").
:- load_kif("(instance sibling SymmetricRelation)").
:- load_kif("(instance sibling IrreflexiveRelation)").
:- load_kif("(domain sibling 1 Organism)").
:- load_kif("(domain sibling 2 Organism)").
:- load_kif("(documentation sibling EnglishLanguage \"The relationship between two &%Organisms that have the same &%mother and &%father. Note that this relationship does not hold between half-brothers, half-sisters, etc.\")").
:- load_kif("(=> (and (parent ?ORGANISM1 ?PARENT1) (parent ?ORGANISM2 ?PARENT1) (parent ?ORGANISM1 ?PARENT2) (parent ?ORGANISM2 ?PARENT2) (not (equal ?ORGANISM1 ?ORGANISM2)) (not (equal ?PARENT1 ?PARENT2))) (sibling ?ORGANISM1 ?ORGANISM2))").
:- load_kif("(=> (and (sibling ?ORG1 ?ORG2) (parent ?ORG1 ?PARENT)) (parent ?ORG2 ?PARENT))").
:- load_kif("(=> (and (parent ?CHILD ?PARENT) (attribute ?PARENT Male)) (father ?CHILD ?PARENT))").
:- load_kif("(=> (and (parent ?CHILD ?PARENT) (attribute ?PARENT Female)) (mother ?CHILD ?PARENT))").
:- load_kif("(subrelation brother sibling)").
:- load_kif("(instance brother IrreflexiveRelation)").
:- load_kif("(instance brother TransitiveRelation)").
:- load_kif("(domain brother 1 Man)").
:- load_kif("(domain brother 2 Human)").
:- load_kif("(documentation brother EnglishLanguage \"The general relationship of being a brother.  (&%brother ?MAN ?PERSON) means that ?MAN is the brother of ?PERSON.\")").
:- load_kif("(subrelation sister sibling)").
:- load_kif("(instance sister IrreflexiveRelation)").
:- load_kif("(instance sister TransitiveRelation)").
:- load_kif("(domain sister 1 Woman)").
:- load_kif("(domain sister 2 Human)").
:- load_kif("(documentation sister EnglishLanguage \"The general relationship of being a sister.  (&%sister ?WOMAN ?PERSON) means that ?WOMAN is the sister of ?PERSON.\")").
:- load_kif("(instance legalRelation BinaryPredicate)").
:- load_kif("(instance legalRelation SymmetricRelation)").
:- load_kif("(domain legalRelation 1 Human)").
:- load_kif("(domain legalRelation 2 Human)").
:- load_kif("(documentation legalRelation EnglishLanguage \"(&%legalRelation ?AGENT1 ?AGENT2) means that ?AGENT1 and ?AGENT2 are relatives by virtue of a legal relationship. Some examples include marriage, adoption, etc.\")").
:- load_kif("(=> (legalRelation ?AGENT1 ?AGENT2) (exists (?DECLARE ?OBLIGATION) (and (instance ?DECLARE Declaring) (confersObligation ?OBLIGATION ?DECLARE ?AGENT1) (confersObligation ?OBLIGATION ?DECLARE ?AGENT2))))").
:- load_kif("(instance acquaintance BinaryPredicate)").
:- load_kif("(documentation acquaintance EnglishLanguage \"(&%acquaintance ?H1 ?H2) means that ?H1 has met and knows something about ?H2, such as ?H2's name and appearance. Statements made with this predicate should be temporally specified with &%holdsDuring. Note that &%acquaintance is not symmetric. For the symmetric version, see &%mutualAcquaintance.\")").
:- load_kif("(domain acquaintance 1 Human)").
:- load_kif("(domain acquaintance 2 Human)").
:- load_kif("(instance mutualAcquaintance BinaryPredicate)").
:- load_kif("(instance mutualAcquaintance SymmetricRelation)").
:- load_kif("(documentation mutualAcquaintance EnglishLanguage \"(&%mutualAcquaintance ?H1 ?H2) means that ?H1 and ?H2 have met each other and know something about each other, such as name and appearance. Statements made with this predicate should be temporally specified with &%holdsDuring. See also the weaker, non-symmetric version of this predicate, &%acquaintance.\")").
:- load_kif("(domain mutualAcquaintance 1 Human)").
:- load_kif("(domain mutualAcquaintance 2 Human)").
:- load_kif("(subrelation mutualAcquaintance acquaintance)").
:- load_kif("(subrelation spouse mutualAcquaintance)").
:- load_kif("(subrelation spouse legalRelation)").
:- load_kif("(instance spouse IrreflexiveRelation)").
:- load_kif("(instance spouse SymmetricRelation)").
:- load_kif("(domain spouse 1 Human)").
:- load_kif("(domain spouse 2 Human)").
:- load_kif("(documentation spouse EnglishLanguage \"The relationship of marriage between two &%Humans.\")").
:- load_kif("(subrelation husband spouse)").
:- load_kif("(instance husband AsymmetricRelation)").
:- load_kif("(instance husband IrreflexiveRelation)").
:- load_kif("(domain husband 1 Man)").
:- load_kif("(domain husband 2 Woman)").
:- load_kif("(inverse husband wife)").
:- load_kif("(documentation husband EnglishLanguage \"(&%husband ?MAN ?WOMAN) means that ?MAN is the husband of ?WOMAN.\")").
:- load_kif("(subrelation wife spouse)").
:- load_kif("(instance wife AsymmetricRelation)").
:- load_kif("(instance wife IrreflexiveRelation)").
:- load_kif("(domain wife 1 Woman)").
:- load_kif("(domain wife 2 Man)").
:- load_kif("(documentation wife EnglishLanguage \"(&%wife ?WOMAN ?MAN) means that ?WOMAN is the wife of ?MAN.\")").
:- load_kif("(subclass EthnicGroup GroupOfPeople)").
:- load_kif("(documentation EthnicGroup EnglishLanguage \"A &%GroupOfPeople whose &%members originate from the same &%GeographicArea or share the same &%Language and/or cultural practices.\")").
:- load_kif("(subclass BeliefGroup GroupOfPeople)").
:- load_kif("(documentation BeliefGroup EnglishLanguage \"A &%GroupOfPeople whose &%members share a belief or set of beliefs.\")").
:- load_kif("(=> (instance ?GROUP BeliefGroup) (exists (?BELIEF) (forall (?MEMB) (=> (member ?MEMB ?GROUP) (believes ?MEMB ?BELIEF)))))").
:- load_kif("(subclass Organization Group)").
% :- load_kif("; NS: delete. (subclass Organization CognitiveAgent)").
% :- load_kif("; NS: add.").
:- load_kif("(subclass Organization Agent)").
:- load_kif("(documentation Organization EnglishLanguage \"An &%Organization is a corporate or similar institution. The &%members of an &%Organization typically have a common purpose or function. Note that this class also covers divisions, departments, etc. of organizations. For example, both the Shell Corporation and the accounting department at Shell would both be instances of &%Organization. Note too that the existence of an &%Organization is dependent on the existence of at least one &%member (since &%Organization is a subclass of &%Collection). Accordingly, in cases of purely legal organizations, a fictitious &%member should be assumed.\")").
:- load_kif("(=> (instance ?ORG Organization) (exists (?PURP) (forall (?MEMBER) (=> (member ?MEMBER ?ORG) (hasPurpose ?MEMBER ?PURP)))))").
:- load_kif("(=> (and (instance ?ORG Organization) (member ?AGENT ?ORG)) (instance ?AGENT Agent))").
:- load_kif("(instance employs BinaryPredicate)").
:- load_kif("(domain employs 1 Organization)").
:- load_kif("(domain employs 2 CognitiveAgent)").
:- load_kif("(documentation employs EnglishLanguage \"(&%employs ?ORG ?PERSON) means that ?ORG has hired ?PERSON and currently retains ?PERSON, on a salaried, hourly or contractual basis, to provide services in exchange for monetary compensation.\")").
:- load_kif("(subclass PoliticalOrganization Organization)").
:- load_kif("(documentation PoliticalOrganization EnglishLanguage \"An &%Organization that is attempting to bring about some sort of political change.\")").
:- load_kif("(=> (instance ?POL PoliticalOrganization) (exists (?PROC) (and (instance ?PROC PoliticalProcess) (agent ?PROC ?POL))))").
:- load_kif("(subclass MilitaryForce PoliticalOrganization)").
:- load_kif("(documentation MilitaryForce EnglishLanguage \"&%MilitaryForce is the subclass of &%Organizations that are organized along military lines and for the purpose of either defensive or offensive combat, whether or not the force is an official &%GovernmentOrganization.\")").
:- load_kif("(=> (instance ?ORG MilitaryForce) (capability ViolentContest agent ?ORG))").
:- load_kif("(subclass MilitaryOrganization MilitaryForce)").
:- load_kif("(subclass MilitaryOrganization GovernmentOrganization)").
:- load_kif("(documentation MilitaryOrganization EnglishLanguage \"Any heavily armed &%Organization that is part of a &%Government and that is charged with representing the &%Government in international conflicts.\")").
:- load_kif("(subclass ParamilitaryOrganization MilitaryForce)").
:- load_kif("(disjoint ParamilitaryOrganization GovernmentOrganization)").
:- load_kif("(documentation ParamilitaryOrganization EnglishLanguage \"An &%Organization which is much like a &%MilitaryOrganization, e.g. it is made up of armed fighters, except that it is not associated with a &%Government.\")").
:- load_kif("(subclass GovernmentOrganization Organization)").
:- load_kif("(documentation GovernmentOrganization EnglishLanguage \"&%GovernmentOrganization is the class of official &%Organizations that are concerned with the government of a &%GeopoliticalArea at some level. They may be a &%subOrganization of a government.\")").
:- load_kif("(<=> (instance ?ORG GovernmentOrganization) (exists (?GOV) (and (instance ?GOV Government) (subOrganization ?ORG ?GOV))))").
:- load_kif("(=> (and (subOrganization ?ORG ?GOV) (instance ?GOV GovernmentOrganization)) (instance ?ORG GovernmentOrganization))").
:- load_kif("(subclass Government GovernmentOrganization)").
% :- load_kif("; NS: add.").
:- load_kif("(subclass Government LegalAgent)").
:- load_kif("(documentation Government EnglishLanguage \"The ruling body of a &%GeopoliticalArea.\")").
:- load_kif("(instance GovernmentFn UnaryFunction)").
:- load_kif("(domain GovernmentFn 1 GeopoliticalArea)").
:- load_kif("(range GovernmentFn Government)").
:- load_kif("(documentation GovernmentFn EnglishLanguage \"(&%GovernmentFn ?AREA) denotes the &%Government of the &%GeopoliticalArea ?AREA. For example,"). (&%GovernmentFn &%UnitedStates) denotes the Federal-level government of the United States, (&%GovernmentFn &%PuertoRico) denotes the government of the Commonwealth of Puerto Rico.\")").
:- load_kif("(subclass PoliceOrganization GovernmentOrganization)").
:- load_kif("(documentation PoliceOrganization EnglishLanguage \"Any &%GovernmentOrganization that is charged with domestic enforcement of the laws of the &%Government.\")").
:- load_kif("(subclass JudicialOrganization Organization)").
:- load_kif("(documentation JudicialOrganization EnglishLanguage \"&%JudicialOrganization is the class of &%Organizations whose primary purpose is to render judgments according to the statutes or regulations of a government or other organization. Judicial bodies are not necessarily government organizations, for example, those associated with sporting associations.\")").
:- load_kif("(subclass EducationalOrganization Organization)").
:- load_kif("(documentation EducationalOrganization EnglishLanguage \"A &%EducationalOrganization is an institution of learning. Some examples are public and private K-12 schools, and colleges and universities.\")").
:- load_kif("(subclass ReligiousOrganization Organization)").
:- load_kif("(subclass ReligiousOrganization BeliefGroup)").
:- load_kif("(documentation ReligiousOrganization EnglishLanguage \"An &%Organization whose members share a set of religious beliefs.\")").
:- load_kif("(subrelation subOrganization subCollection)").
:- load_kif("(instance subOrganization PartialOrderingRelation)").
:- load_kif("(domain subOrganization 1 Organization)").
:- load_kif("(domain subOrganization 2 Organization)").
:- load_kif("(documentation subOrganization EnglishLanguage \"(&%subOrganization ?ORG1 ?ORG2) means that ?ORG1 is an &%Organization which is a part of the &%Organization ?ORG2. Note that &%subOrganization is a &%ReflexiveRelation, so every &%Organization is a &%subOrganization of itself.\")").
:- load_kif("(instance citizen BinaryPredicate)").
:- load_kif("(instance citizen AsymmetricRelation)").
:- load_kif("(domain citizen 1 Human)").
:- load_kif("(domain citizen 2 Nation)").
:- load_kif("(documentation citizen EnglishLanguage \"(&%citizen ?PERSON ?NATION) means that the &%Human ?PERSON is a citizen of &%Nation ?NATION.\")").
% :- load_kif("; END FILE").
% :- load_kif("; BEGIN FILE").
% :- load_kif(";  QUALITIES  ").
% :- load_kif("; INCLUDES 'MEREOTOPOLOGY' INCLUDES 'PROCESSES' INCLUDES 'OBJECTS'").
:- load_kif("(subclass FieldOfStudy Proposition)").
:- load_kif("(documentation FieldOfStudy EnglishLanguage \"An academic or applied discipline with recognized experts and with a core of accepted theory or practice. Note that &%FieldOfStudy is a &%subclass of &%Proposition, because a &%FieldOfStudy is understood to be a body of abstract, informational content, with varying degrees of certainty attached to each element of this content.\")").
:- load_kif("(subclass Procedure Proposition)").
:- load_kif("(documentation Procedure EnglishLanguage \"A sequence-dependent specification. Some examples are &%ComputerPrograms, finite-state machines, cooking recipes, musical scores, conference schedules, driving directions, and the scripts of plays and movies.\")").
:- load_kif("(subclass ComputerProgram Procedure)").
:- load_kif("(documentation ComputerProgram EnglishLanguage \"A set of instructions in a computer programming language that can be executed by a computer.\")").
:- load_kif("(subclass ComputerProgramming ContentDevelopment)").
:- load_kif("(documentation ComputerProgramming EnglishLanguage \"The process of developing a &%ComputerProgram\")").
:- load_kif("(=> (instance ?CP ComputerProgramming) (exists (?C) (and (instance ?C ComputerProgram) (result ?CP ?C))))").
:- load_kif("(subclass Plan Procedure)").
:- load_kif("(documentation Plan EnglishLanguage \"A specification of a sequence of &%Processes which is intended to satisfy a specified purpose at some future time.\")").
:- load_kif("(=> (and (instance ?PLAN Plan) (instance ?OBJ ContentBearingObject) (containsInformation ?OBJ ?PLAN)) (exists (?PLANNING) (and (instance ?PLANNING Planning) (result ?PLANNING ?OBJ))))").
:- load_kif("(subclass Argument Proposition)").
:- load_kif("(partition Argument DeductiveArgument InductiveArgument)").
:- load_kif("(documentation Argument EnglishLanguage \"Any proposition which has the form of a deductive or inductive argument, i.e. a set of premises which, it is claimed, imply a conclusion.\")").
:- load_kif("(=> (instance ?REASON Reasoning) (exists (?ARGUMENT) (and (instance ?ARGUMENT Argument) (realization ?REASON ?ARGUMENT))))").
:- load_kif("(=> (instance ?ARGUMENT Argument) (exists (?PREMISES ?CONCLUSION) (and (equal (PremisesFn ?ARGUMENT) ?PREMISES) (conclusion ?CONCLUSION ?ARGUMENT))))").
:- load_kif("(subclass DeductiveArgument Argument)").
:- load_kif("(partition DeductiveArgument ValidDeductiveArgument InvalidDeductiveArgument)").
:- load_kif("(documentation DeductiveArgument EnglishLanguage \"An &%Argument which has the form of a deduction, i.e. it is claimed that the set of &%premises &%entails the &%conclusion.\")").
:- load_kif("(subclass ValidDeductiveArgument DeductiveArgument)").
:- load_kif("(documentation ValidDeductiveArgument EnglishLanguage \"A &%DeductiveArgument which is valid, i.e. the set of &%premises in fact &%entails the &%conclusion.\")").
:- load_kif("(=> (and (instance ?ARGUMENT ValidDeductiveArgument) (equal ?PREMISES (PremisesFn ?ARGUMENT)) (conclusion ?CONCLUSION ?ARGUMENT)) (exists (?FORMULA1 ?FORMULA2) (and (containsInformation ?FORMULA1 ?PREMISES) (containsInformation ?FORMULA2 ?CONCLUSION) (entails ?PREMISES ?CONCLUSION))))").
:- load_kif("(subclass InvalidDeductiveArgument DeductiveArgument)").
:- load_kif("(documentation InvalidDeductiveArgument EnglishLanguage \"&%DeductiveArguments that are not &%ValidDeductiveArguments, i.e. it is not the case that the set of &%premises in fact &%entails the &%conclusion.\")").
:- load_kif("(subclass Explanation DeductiveArgument)").
:- load_kif("(documentation Explanation EnglishLanguage \"An &%Argument where the conclusion is an observed fact and the premises are other facts which collectively imply the conclusion. Note that this is the they hypothetico-deductive model of explanation.\")").
:- load_kif("(subclass InductiveArgument Argument)").
:- load_kif("(documentation InductiveArgument EnglishLanguage \"An &%Argument which is inductive, i.e. it is claimed that a set of specific cases makes the &%conclusion, which generalizes these cases, more likely to be true.\")").
:- load_kif("(instance premise BinaryPredicate)").
:- load_kif("(instance premise TotalValuedRelation)").
:- load_kif("(domain premise 1 Argument)").
:- load_kif("(domain premise 2 Proposition)").
:- load_kif("(documentation premise EnglishLanguage \"(&%premise ?ARGUMENT ?PROPOSITION) means that the &%Proposition ?PROPOSITION is an explicit assumption of the &%Argument ?ARGUMENT.\")").
:- load_kif("(instance PremisesFn UnaryFunction)").
:- load_kif("(domain PremisesFn 1 Argument)").
:- load_kif("(range PremisesFn Proposition)").
:- load_kif("(documentation PremisesFn EnglishLanguage \"(&%PremisesFn ?ARGUMENT) returns the complete set of &%premises of the &%Argument ?ARGUMENT.\")").
:- load_kif("(=> (and (instance ?ARGUMENT Argument) (equal ?PREMISES (PremisesFn ?ARGUMENT))) (<=> (subProposition ?PROPOSITION ?PREMISES) (premise ?ARGUMENT ?PROPOSITION)))").
:- load_kif("(instance conclusion BinaryPredicate)").
:- load_kif("(instance conclusion SingleValuedRelation)").
:- load_kif("(instance conclusion TotalValuedRelation)").
:- load_kif("(domain conclusion 1 Argument)").
:- load_kif("(domain conclusion 2 Proposition)").
:- load_kif("(documentation conclusion EnglishLanguage \"(&%conclusion ?ARGUMENT ?PROPOSITION) means that the &%Proposition ?PROPOSITION is the conclusion explicitly drawn from the &%Argument ?ARGUMENT. Note that it may or may not be the case that ?ARGUMENT &%entails ?PROPOSITION.\")").
:- load_kif("(instance consistent BinaryPredicate)").
:- load_kif("(instance consistent SymmetricRelation)").
:- load_kif("(domain consistent 1 Proposition)").
:- load_kif("(domain consistent 2 Proposition)").
:- load_kif("(documentation consistent EnglishLanguage \"(&%consistent ?PROP1 ?PROP2) means that the two &%Propositions ?PROP1 and ?PROP2 are consistent with one another, i.e. it is possible for both of them to be true at the same time.\")").
:- load_kif("(instance orientation SpatialRelation)").
:- load_kif("(instance orientation TernaryPredicate)").
:- load_kif("(domain orientation 1 Object)").
:- load_kif("(domain orientation 2 Object)").
:- load_kif("(domain orientation 3 PositionalAttribute)").
:- load_kif("(documentation orientation EnglishLanguage \"A general &%Predicate for indicating how two &%Objects are oriented with respect to one another. For example,").
:- load_kif("(orientation ?OBJ1 ?OBJ2 North) means that ?OBJ1 is north of ?OBJ2, and").
:- load_kif("(orientation ?OBJ1 ?OBJ2 Vertical) means that ?OBJ1 is positioned vertically with respect to ?OBJ2.\")").
:- load_kif("(=> (and (orientation ?OBJ1 ?OBJ2 ?ATTR1) (contraryAttribute @ROW) (inList ?ATTR1 (ListFn @ROW)) (inList ?ATTR2 (ListFn @ROW)) (not (equal ?ATTR1 ?ATTR2))) (not (orientation ?OBJ1 ?OBJ2 ?ATTR2)))").
:- load_kif("(instance direction CaseRole)").
:- load_kif("(domain direction 1 Process)").
:- load_kif("(domain direction 2 PositionalAttribute)").
:- load_kif("(subrelation direction involvedInEvent)").
:- load_kif("(documentation direction EnglishLanguage \"(&%direction ?PROC ?ATTR) means that the &%Process ?PROC is moving in the direction ?ATTR. For example, one would use this &%Predicate to represent the fact that Max is moving &%North.\")").
:- load_kif("(=> (holdsDuring ?TIME (direction ?PROC ?ATTR1)) (forall (?ATTR2) (=> (holdsDuring ?TIME (direction ?PROC ?ATTR2)) (equal ?ATTR2 ?ATTR1))))").
:- load_kif("(instance faces BinaryPredicate)").
:- load_kif("(domain faces 1 Object)").
:- load_kif("(domain faces 2 DirectionalAttribute)").
:- load_kif("(documentation faces EnglishLanguage \"(&%faces ?OBJ ?DIRECTION) means that the front of ?OBJ (see &%FrontFn) is positioned towards the compass direction ?DIRECTION. More precisely, it means that if a line were extended from the center of ?DIRECTION, the line would intersect with the front of ?OBJ before it intersected with its back (see &%BackFn).\")").
:- load_kif("(=> (holdsDuring ?TIME (faces ?PROC ?ATTR1)) (forall (?ATTR2) (=> (holdsDuring ?TIME (faces ?PROC ?ATTR2)) (equal ?ATTR2 ?ATTR1))))").
:- load_kif("(subclass TruthValue RelationalAttribute)").
:- load_kif("(documentation TruthValue EnglishLanguage \"The &%Class of truth values, e.g. &%True and &%False. These are &%Attributes of &%Sentences and &%Propositions.\")").
:- load_kif("(=> (and (property ?ITEM ?VALUE) (instance ?VALUE TruthValue)) (or (instance ?ITEM Sentence) (instance ?ITEM Proposition)))").
:- load_kif("(instance True TruthValue)").
:- load_kif("(documentation True EnglishLanguage \"The &%TruthValue of being true.\")").
:- load_kif("(instance False TruthValue)").
:- load_kif("(contraryAttribute False True)").
:- load_kif("(documentation False EnglishLanguage \"The &%TruthValue of being false.\")").
:- load_kif("(subrelation truth property)").
:- load_kif("(domain truth 1 Sentence)").
:- load_kif("(domain truth 2 TruthValue)").
:- load_kif("(documentation truth EnglishLanguage \"The &%BinaryPredicate that relates a &%Sentence to its &%TruthValue.\")").
:- load_kif("(subclass PositionalAttribute RelationalAttribute)").
:- load_kif("(documentation PositionalAttribute EnglishLanguage \"&%Attributes characterizing the orientation of an &%Object, e.g. &%Vertical versus &%Horizontal, &%Left versus &%Right etc.\")").
:- load_kif("(subclass DirectionalAttribute PositionalAttribute)").
:- load_kif("(documentation DirectionalAttribute EnglishLanguage \"The subclass of &%PositionalAttributes that concern compass directions.\")").
:- load_kif("(=> (and (instance ?DIRECT DirectionalAttribute) (orientation ?OBJ1 ?OBJ2 ?DIRECT) (orientation ?OBJ2 ?OBJ3 ?DIRECT)) (between ?OBJ1 ?OBJ2 ?OBJ3))").
:- load_kif("(instance North DirectionalAttribute)").
:- load_kif("(contraryAttribute North South East West)").
:- load_kif("(documentation North EnglishLanguage \"The compass direction of &%North.\")").
:- load_kif("(instance South DirectionalAttribute)").
:- load_kif("(documentation South EnglishLanguage \"The compass direction of &%South.\")").
:- load_kif("(<=> (orientation ?OBJ1 ?OBJ2 North) (orientation ?OBJ2 ?OBJ1 South))").
:- load_kif("(instance East DirectionalAttribute)").
:- load_kif("(documentation East EnglishLanguage \"The compass direction of &%East.\")").
:- load_kif("(instance West DirectionalAttribute)").
:- load_kif("(documentation West EnglishLanguage \"The compass direction of &%West.\")").
:- load_kif("(<=> (orientation ?OBJ1 ?OBJ2 East) (orientation ?OBJ2 ?OBJ1 West))").
:- load_kif("(instance Vertical PositionalAttribute)").
:- load_kif("(documentation Vertical EnglishLanguage \"Attribute used to indicate that an &%Object is positioned height-wise with respect to another &%Object.\")").
:- load_kif("(<=> (orientation ?OBJ1 ?OBJ2 Vertical) (orientation ?OBJ2 ?OBJ1 Vertical))").
:- load_kif("(instance Horizontal PositionalAttribute)").
:- load_kif("(contraryAttribute Horizontal Vertical)").
:- load_kif("(documentation Horizontal EnglishLanguage \"Attribute used to indicate that an &%Object is positioned width-wise with respect to another &%Object.\")").
:- load_kif("(<=> (orientation ?OBJ1 ?OBJ2 Horizontal) (orientation ?OBJ2 ?OBJ1 Horizontal))").
:- load_kif("(subclass SymmetricPositionalAttribute PositionalAttribute)").
:- load_kif("(documentation SymmetricPositionalAttribute EnglishLanguage \"&%SymmetricAttribute is the class of &%PositionalAttribute that hold between two items regardless of their order or orientation.\")").
:- load_kif("(=> (and (instance ?P SymmetricPositionalAttribute) (orientation ?O1 ?O2 ?P)) (orientation ?O2 ?O1 ?P))").
:- load_kif("(subclass AntiSymmetricPositionalAttribute PositionalAttribute)").
:- load_kif("(documentation AntiSymmetricPositionalAttribute EnglishLanguage \"&%AntiSymmetricPositionalAttribute is the class of &%PositionalAttribute that hold in only one direction. I.e. two objects cannot simulataneously be &%On each other.\")").
:- load_kif("(=> (and (instance ?P AntiSymmetricPositionalAttribute) (orientation ?O1 ?O2 ?P)) (not (orientation ?O2 ?O1 ?P)))").
:- load_kif("(instance Above PositionalAttribute)").
:- load_kif("(instance Above AntiSymmetricPositionalAttribute)").
:- load_kif("(contraryAttribute Above Below)").
:- load_kif("(documentation Above EnglishLanguage \"This is a &%PositionalAttribute derived from the up/down schema and not involving contact. Note that this means directly above, i.e., if one object is &%Above another object, then the projections of the two objects overlap.\")").
:- load_kif("(=> (orientation ?OBJ1 ?OBJ2 Above) (not (connected ?OBJ1 ?OBJ2)))").
:- load_kif("(instance Below PositionalAttribute)").
:- load_kif("(instance Below AntiSymmetricPositionalAttribute)").
:- load_kif("(documentation Below EnglishLanguage \"This &%PositionalAttribute is derived from the up/down schema and may or may not involve contact. Note that this means directly below, i.e., if one object is &%Below another object, then the projections of the two objects overlap.\")").
:- load_kif("(<=> (orientation ?OBJ1 ?OBJ2 Below) (or (orientation ?OBJ2 ?OBJ1 On) (orientation ?OBJ2 ?OBJ1 Above)))").
:- load_kif("(instance Adjacent SymmetricPositionalAttribute)").
:- load_kif("(documentation Adjacent EnglishLanguage \"Used to assert that an object ?OBJ1 is close to, near or abutting ?OBJ2. This &%PositionalAttribute covers the following common sense notions: adjoins, abuts, is contiguous to, is juxtaposed, and is close to.\")").
:- load_kif("(<=> (orientation ?OBJ1 ?OBJ2 Adjacent) (or (orientation ?OBJ1 ?OBJ2 Near) (connected ?OBJ1 ?OBJ2)))").
:- load_kif("(instance Left PositionalAttribute)").
:- load_kif("(instance Left AntiSymmetricPositionalAttribute)").
:- load_kif("(documentation Left EnglishLanguage \"This &%PositionalAttribute is derived from the left/right schema. Note that this means directly to the left, so that, if one object is to the left of another, then the projections of the two objects overlap.\")").
:- load_kif("(instance Right PositionalAttribute)").
:- load_kif("(instance Right AntiSymmetricPositionalAttribute)").
:- load_kif("(contraryAttribute Right Left)").
:- load_kif("(documentation Right EnglishLanguage \"This &%PositionalAttribute is derived from the left/right schema. Note that this means directly to the right, so that, if one object is to the right of another, then the projections of the two objects overlap.\")").
:- load_kif("(<=> (orientation ?OBJ1 ?OBJ2 Right) (orientation ?OBJ2 ?OBJ1 Left))").
:- load_kif("(instance Near SymmetricPositionalAttribute)").
:- load_kif("(documentation Near EnglishLanguage \"The relation of common sense adjacency. Note that, if an object is &%Near another object, then the objects are not &%connected.\")").
:- load_kif("(=> (orientation ?OBJ1 ?OBJ2 Near) (not (connected ?OBJ1 ?OBJ2)))").
:- load_kif("(=> (orientation ?OBJ1 ?OBJ2 Near) (orientation ?OBJ2 ?OBJ1 Near))").
:- load_kif("(instance On AntiSymmetricPositionalAttribute)").
:- load_kif("(documentation On EnglishLanguage \"This is used to assert that an object is on top of another object, and it is derived from the up/down schema and involves contact.\")").
:- load_kif("(=> (orientation ?OBJ1 ?OBJ2 On) (connected ?OBJ1 ?OBJ2))").
:- load_kif("(=> (orientation ?OBJ1 ?OBJ2 On) (located ?OBJ1 ?OBJ2))").
:- load_kif("(=> (orientation ?OBJ1 ?OBJ2 On) (not (orientation ?OBJ2 ?OBJ1 On)))").
:- load_kif("(subclass TimeZone RelationalAttribute)").
:- load_kif("(documentation TimeZone EnglishLanguage \"An &%Attribute which is used to specify coordinates in which time measures are uniform, i.e. all time devices are synchronized to the same &%TimePositions.\")").
:- load_kif("(instance CoordinatedUniversalTimeZone TimeZone)").
:- load_kif("(documentation CoordinatedUniversalTimeZone EnglishLanguage \"A &%TimeZone which functions as the standard time zone. It is also known as Zulu time (in the military), Greenwich Mean Time, and the Western European time zone. Note that whenever a &%TimeZone is not specified, the &%TimePosition is understood to be with respect to the &%CoordinatedUniversalTimeZone.\")").
:- load_kif("(instance PacificTimeZone TimeZone)").
:- load_kif("(documentation PacificTimeZone EnglishLanguage \"A &%TimeZone that covers much of the western part of the United States.\")").
:- load_kif("(=> (equal (RelativeTimeFn ?TIME1 PacificTimeZone) ?TIME2) (equal ?TIME2 (AdditionFn ?TIME1 8)))").
:- load_kif("(instance MountainTimeZone TimeZone)").
:- load_kif("(documentation MountainTimeZone EnglishLanguage \"A &%TimeZone that covers much of the Rocky Mountain region of the United States.\")").
:- load_kif("(=> (equal (RelativeTimeFn ?TIME1 MountainTimeZone) ?TIME2) (equal ?TIME2 (AdditionFn ?TIME1 7)))").
:- load_kif("(instance CentralTimeZone TimeZone)").
:- load_kif("(documentation CentralTimeZone EnglishLanguage \"A &%TimeZone that covers much of the midwestern United States.\")").
:- load_kif("(=> (equal (RelativeTimeFn ?TIME1 CentralTimeZone) ?TIME2) (equal ?TIME2 (AdditionFn ?TIME1 6)))").
:- load_kif("(instance EasternTimeZone TimeZone)").
:- load_kif("(documentation EasternTimeZone EnglishLanguage \"A &%TimeZone that covers much of the eastern United States.\")").
:- load_kif("(=> (equal (RelativeTimeFn ?TIME1 EasternTimeZone) ?TIME2) (equal ?TIME2 (AdditionFn ?TIME1 5)))").
:- load_kif("(instance RelativeTimeFn BinaryFunction)").
:- load_kif("(instance RelativeTimeFn TemporalRelation)").
:- load_kif("(instance RelativeTimeFn TotalValuedRelation)").
:- load_kif("(domain RelativeTimeFn 1 TimePosition)").
:- load_kif("(domain RelativeTimeFn 2 TimeZone)").
:- load_kif("(range RelativeTimeFn TimePosition)").
:- load_kif("(documentation RelativeTimeFn EnglishLanguage \"A means of converting &%TimePositions between different &%TimeZones. (&%RelativeTimeFn ?TIME ?ZONE) denotes the &%TimePosition in &%CoordinatedUniversalTime that is contemporaneous with the &%TimePosition ?TIME in &%TimeZone ?ZONE. For example, (&%RelativeTimeFn (&%MeasureFn 14 &%HourDuration) &%EasternTimeZone) would return the value (&%MeasureFn 19 &%HourDuration).\")").
:- load_kif("(subclass SocialRole RelationalAttribute)").
:- load_kif("(documentation SocialRole EnglishLanguage \"The &%Class of all &%Attributes that specify the position or status of a &%CognitiveAgent within an &%Organization or other &%Group.\")").
:- load_kif("(=> (and (attribute ?PERSON ?ATTRIBUTE) (instance ?ATTRIBUTE SocialRole)) (instance ?PERSON Human))").
:- load_kif("(instance Unemployed SocialRole)").
:- load_kif("(documentation Unemployed EnglishLanguage \"The &%Attribute of a &%CognitiveAgent when he/she is unemployed.\")").
:- load_kif("(<=> (and (instance ?PERSON Human) (forall (?ORG) (not (employs ?ORG ?PERSON)))) (attribute ?PERSON Unemployed))").
:- load_kif("(subclass Position SocialRole)").
:- load_kif("(documentation Position EnglishLanguage \"A formal position of reponsibility within an &%Organization. Examples of &%Positions include president, laboratory director, senior researcher, sales representative, etc.\")").
:- load_kif("(instance occupiesPosition TernaryPredicate)").
:- load_kif("(domain occupiesPosition 1 Human)").
:- load_kif("(domain occupiesPosition 2 Position)").
:- load_kif("(domain occupiesPosition 3 Organization)").
:- load_kif("(documentation occupiesPosition EnglishLanguage \"(&%occupiesPosition ?PERSON ?POSITION ?ORG) means that ?PERSON holds the &%Position ?POSITION at &%Organization ?ORG. For example, (&%occupiesPosition &%TomSmith &%ResearchDirector &%AcmeLaboratory) means that &%TomSmith is a research director at Acme Labs.\")").
:- load_kif("(=> (occupiesPosition ?AGENT ?POSITION ?ORG) (attribute ?AGENT ?POSITION))").
:- load_kif("(=> (employs ?ORG ?PERSON) (exists (?POSITION) (occupiesPosition ?PERSON ?POSITION ?ORG)))").
:- load_kif("(=> (occupiesPosition ?PERSON ?POSITION ?ORGANIZATION) (member ?PERSON ?ORGANIZATION))").
:- load_kif("(subclass NormativeAttribute RelationalAttribute)").
:- load_kif("(documentation NormativeAttribute EnglishLanguage \"A &%Class containing all of the &%Attributes that are specific to morality, legality, aesthetics, etiquette, etc. Many of these attributes express a judgement that something ought or ought not to be the case.\")").
:- load_kif("(instance modalAttribute BinaryPredicate)").
:- load_kif("(instance modalAttribute AsymmetricRelation)").
:- load_kif("(instance modalAttribute IrreflexiveRelation)").
:- load_kif("(subrelation modalAttribute property)").
:- load_kif("(domain modalAttribute 1 Formula)").
:- load_kif("(domain modalAttribute 2 NormativeAttribute)").
:- load_kif("(documentation modalAttribute EnglishLanguage \"A &%BinaryRelation that is used to state the normative force of a &%Proposition. (&%modalAttribute ?FORMULA ?PROP) means that the &%Proposition expressed by ?FORMULA has the &%NormativeAttribute ?PROP. For example, (&%modalAttribute (&%exists (?ACT ?OBJ) (&%and"). (&%instance ?ACT &%Giving) (&%agent ?ACT John) (&%patient ?ACT ?OBJ)"). (&%destination ?ACT Tom))) &%Obligation) means that John is obligated to give Tom something.\")").
:- load_kif("(=> (and (modalAttribute ?FORMULA1 ?PROP) (entails ?FORMULA1 ?FORMULA2)) (modalAttribute ?FORMULA2 ?PROP))").
:- load_kif("(=> (modalAttribute ?FORMULA Permission) (modalAttribute ?FORMULA Possibility))").
:- load_kif("(subclass SubjectiveAssessmentAttribute NormativeAttribute)").
:- load_kif("(disjoint SubjectiveAssessmentAttribute ObjectiveNorm)").
:- load_kif("(documentation SubjectiveAssessmentAttribute EnglishLanguage \"The &%Class of &%NormativeAttributes which lack an objective criterion for their attribution, i.e. the attribution of these &%Attributes varies from subject to subject and even with respect to the same subject over time. This &%Class is, generally speaking, only used when mapping external knowledge sources to the SUMO. If a term from such a knowledge source seems to lack objective criteria for its attribution, it is assigned to this &%Class.\")").
:- load_kif("(=> (and (attribute ?OBJ ?ATR) (instance ?ATR SubjectiveAssessmentAttribute)) (exists (?TIME ?JUDGE ?AGENT) (and (instance ?JUDGE Judging) (agent ?JUDGE ?AGENT) (patient ?JUDGE ?OBJ) (patient ?JUDGE (attribute ?OBJ ?ATR)) (holdsDuring ?TIME (believes ?AGENT (property ?OBJ ?ATR))))))").
:- load_kif("(subclass SubjectiveStrongPositiveAttribute SubjectiveAssessmentAttribute)").
:- load_kif("(documentation SubjectiveStrongPositiveAttribute EnglishLanguage \"The &%Class of &%NormativeAttributes which lack an objective criterion for their attribution, though statistically tends to be used in a strongly positive sense. This &%Class is, generally speaking, only used when mapping external knowledge sources to the SUMO. If a term from such a knowledge source seems to lack objective criteria for its attribution, it is assigned to this &%Class.\")").
:- load_kif("(subclass SubjectiveWeakPositiveAttribute SubjectiveAssessmentAttribute)").
:- load_kif("(documentation SubjectiveWeakPositiveAttribute EnglishLanguage \"The &%Class of &%NormativeAttributes which lack an objective criterion for their attribution, though statistically tends to be used in a weakly positive sense. This &%Class is, generally speaking, only used when mapping external knowledge sources to the SUMO. If a term from such a knowledge source seems to lack objective criteria for its attribution, it is assigned to this &%Class.\")").
:- load_kif("(subclass SubjectiveStrongNegativeAttribute SubjectiveAssessmentAttribute)").
:- load_kif("(documentation SubjectiveStrongNegativeAttribute EnglishLanguage \"The &%Class of &%NormativeAttributes which lack an objective criterion for their attribution, though statistically tends to be used in a strongly negative sense. This &%Class is, generally speaking, only used when mapping external knowledge sources to the SUMO. If a term from such a knowledge source seems to lack objective criteria for its attribution, it is assigned to this &%Class.\")").
:- load_kif("(subclass SubjectiveWeakNegativeAttribute SubjectiveAssessmentAttribute)").
:- load_kif("(documentation SubjectiveWeakNegativeAttribute EnglishLanguage \"The &%Class of &%NormativeAttributes which lack an objective criterion for their attribution, though statistically tends to be used in a weakly negative sense. This &%Class is, generally speaking, only used when mapping external knowledge sources to the SUMO. If a term from such a knowledge source seems to lack objective criteria for its attribution, it is assigned to this &%Class.\")").
:- load_kif("(subclass ObjectiveNorm NormativeAttribute)").
:- load_kif("(documentation ObjectiveNorm EnglishLanguage \"The &%Class of &%NormativeAttributes that are associated with an objective criterion for their attribution, i.e. there is broad consensus about the cases where these attributes are applicable.\")").
:- load_kif("(subclass ContestAttribute ObjectiveNorm)").
:- load_kif("(documentation ContestAttribute EnglishLanguage \"A &%Class containing &%Attributes that are specific to participants in a &%Contest. In particular, these &%Attributes indicate the position of one of the &%agents in the &%Contest with respect to other &%agent(s) in the &%Contest. Some examples of these &%Attributes are winning, losing, won, lost, etc.\")").
:- load_kif("(=> (and (property ?THING ?ATTR) (instance ?ATTR ContestAttribute)) (exists (?CONTEST) (and (instance ?CONTEST Contest) (or (agent ?CONTEST ?THING) (patient ?CONTEST ?THING) (subProcess ?THING ?CONTEST)))))").
:- load_kif("(subclass AlethicAttribute ObjectiveNorm)").
:- load_kif("(documentation AlethicAttribute EnglishLanguage \"A &%Class containing all of the &%Attributes relating to the notions of possibility and necessity.\")").
:- load_kif("(instance Possibility AlethicAttribute)").
:- load_kif("(documentation Possibility EnglishLanguage \"Attribute that applies to &%Propositions that are possible, i.e. true in at least one possible world.\")").
:- load_kif("(instance Necessity AlethicAttribute)").
:- load_kif("(documentation Necessity EnglishLanguage \"Attribute that applies to &%Propositions that are necessary, i.e. true in every possible world.\")").
:- load_kif("(<=> (modalAttribute ?FORMULA Necessity) (not (modalAttribute (not ?FORMULA) Possibility)))").
:- load_kif("(=> (modalAttribute ?FORMULA Necessity) (modalAttribute ?FORMULA Possibility))").
:- load_kif("(instance holdsRight BinaryPredicate)").
:- load_kif("(instance holdsRight AsymmetricRelation)").
:- load_kif("(domain holdsRight 1 Formula)").
:- load_kif("(domain holdsRight 2 CognitiveAgent)").
:- load_kif("(documentation holdsRight EnglishLanguage \"Expresses a relationship between a &%Formula and a &%CognitiveAgent whereby the &%CognitiveAgent has the right to bring it about that the &%Formula is true.\")").
:- load_kif("(instance confersRight TernaryPredicate)").
:- load_kif("(domain confersRight 1 Formula)").
:- load_kif("(domain confersRight 2 Entity)").
:- load_kif("(domain confersRight 3 CognitiveAgent)").
:- load_kif("(documentation confersRight EnglishLanguage \"Expresses the relationship between a &%Formula, an &%Entity, and a &%CognitiveAgent when the &%Entity authorizes the &%CognitiveAgent to bring it about that the &%Formula is true.\")").
:- load_kif("(=> (confersRight ?FORMULA ?AGENT1 ?AGENT2) (holdsRight ?FORMULA ?AGENT2))").
:- load_kif("(instance holdsObligation BinaryPredicate)").
:- load_kif("(instance holdsObligation AsymmetricRelation)").
:- load_kif("(domain holdsObligation 1 Formula)").
:- load_kif("(domain holdsObligation 2 CognitiveAgent)").
:- load_kif("(relatedInternalConcept holdsObligation holdsRight)").
:- load_kif("(documentation holdsObligation EnglishLanguage \"Expresses a relationship between a &%Formula and a &%CognitiveAgent whereby the &%CognitiveAgent has the obligation to bring it about that the &%Formula is true.\")").
:- load_kif("(instance confersObligation TernaryPredicate)").
:- load_kif("(domain confersObligation 1 Formula)").
:- load_kif("(domain confersObligation 2 Entity)").
:- load_kif("(domain confersObligation 3 CognitiveAgent)").
:- load_kif("(relatedInternalConcept confersObligation confersRight)").
:- load_kif("(documentation confersObligation EnglishLanguage \"Expresses the relationship between a a &%Formula, an &%Entity, and a &%CognitiveAgent when the &%Entity obligates the &%CognitiveAgent to bring it about that the &%Formula is true.\")").
:- load_kif("(=> (confersObligation ?FORMULA ?AGENT1 ?AGENT2) (holdsObligation ?FORMULA ?AGENT2))").
:- load_kif("(subclass DeonticAttribute ObjectiveNorm)").
:- load_kif("(documentation DeonticAttribute EnglishLanguage \"A &%Class containing all of the &%Attributes relating to the notions of permission, obligation, and prohibition.\")").
:- load_kif("(instance Permission DeonticAttribute)").
:- load_kif("(documentation Permission EnglishLanguage \"&%Attribute that applies to &%Propositions that an &%Agent is permitted, by some authority, to make true.\")").
:- load_kif("(instance Obligation DeonticAttribute)").
:- load_kif("(documentation Obligation EnglishLanguage \"&%Attribute that applies to &%Propositions that an &%Agent is required, by some authority, to make true.\")").
:- load_kif("(<=> (modalAttribute ?FORMULA Obligation) (not (modalAttribute (not ?FORMULA) Permission)))").
:- load_kif("(=> (modalAttribute ?FORMULA Obligation) (modalAttribute ?FORMULA Permission))").
:- load_kif("(subAttribute Law Obligation)").
:- load_kif("(instance Law DeonticAttribute)").
:- load_kif("(documentation Law EnglishLanguage \"&%Attribute that applies to &%Propositions that are required by a government or a branch of the government and that are enforced with penalties for noncompliance. These &%Propositions may be codified as legislation or they may be more informal, as in the case of government policy.\")").
:- load_kif("(subAttribute Promise Obligation)").
:- load_kif("(documentation Promise EnglishLanguage \"&%Attribute that applies to &%Propositions that an &%Agent promises to make true. &%Promises may be implicit or explicit. They may be expressed in a written or verbal or gestural manner.\")").
% :- load_kif("; KJN: Moving this to Mid-level-ontology as the definition for Contract is all there. ").
% :- load_kif(" (=> (property ?ENTITY Promise) (or (property ?ENTITY Contract) (property ?ENTITY NakedPromise)))").
% :- load_kif(" (subAttribute NakedPromise Promise) (documentation NakedPromise EnglishLanguage \"A &%Promise where nothing is promised in return, i.e. a nudum pactum.\")").
:- load_kif("(instance Prohibition DeonticAttribute)").
:- load_kif("(documentation Prohibition EnglishLanguage \"&%Prohibition is the &%DeonticAttribute that applies to &%Formulas that an &%Agent is forbidden, by some authority, to make true.\")").
:- load_kif("(<=> (modalAttribute ?FORMULA Prohibition) (not (modalAttribute ?FORMULA Permission)))").
:- load_kif("(subclass ProbabilityAttribute ObjectiveNorm)").
:- load_kif("(documentation ProbabilityAttribute EnglishLanguage \"A class containing all of the &%Attributes relating to objective, qualitative assessments of probability, e.g. &%Likely and &%Unlikely.\")").
:- load_kif("(instance Likely ProbabilityAttribute)").
:- load_kif("(contraryAttribute Likely Unlikely)").
:- load_kif("(documentation Likely EnglishLanguage \"The &%ProbabilityAttribute of being probable, i.e. more likely than not to be &%True.\")").
:- load_kif("(=> (modalAttribute ?FORMULA Likely) (greaterThan (ProbabilityFn (truth ?FORMULA True)) (ProbabilityFn (truth ?FORMULA False))))").
:- load_kif("(instance Unlikely ProbabilityAttribute)").
:- load_kif("(documentation Unlikely EnglishLanguage \"The &%ProbabilityAttribute of being improbable, i.e. more likely than not to be &%False.\")").
:- load_kif("(=> (modalAttribute ?FORMULA Unlikely) (greaterThan (ProbabilityFn (truth ?FORMULA False)) (ProbabilityFn (truth ?FORMULA True))))").
:- load_kif("(subclass PhysicalState InternalAttribute)").
:- load_kif("(contraryAttribute Solid Liquid Gas Plasma)").
:- load_kif("(exhaustiveAttribute PhysicalState Solid Fluid Liquid Gas Plasma)").
:- load_kif("(documentation PhysicalState EnglishLanguage \"The physical state of an &%Object. There are three reified instances of this &%Class: &%Solid, &%Liquid, and &%Gas. Physical changes are not characterized by the transformation of one substance into another, but rather by the change of the form (physical states) of a given substance. For example, melting an iron nail yields a substance still called iron.\")").
:- load_kif("(instance Solid PhysicalState)").
:- load_kif("(documentation Solid EnglishLanguage \"An &%Object has the &%Attribute of &%Solid if it has a fixed shape and a fixed volume.\")").
:- load_kif("(instance Fluid PhysicalState)").
:- load_kif("(documentation Fluid EnglishLanguage \"&%Fluid is the &%PhysicalState attribute of an &%Object that does not have a fixed shape and thus tends to flow or to conform to the shape of a container.\")").
:- load_kif("(instance Liquid PhysicalState)").
:- load_kif("(subAttribute Liquid Fluid)").
:- load_kif("(documentation Liquid EnglishLanguage \"An &%Object has the &%Attribute of &%Liquid if it has a fixed volume but not a fixed shape.\")").
:- load_kif("(=> (instance ?OBJ Solution) (attribute ?OBJ Liquid))").
:- load_kif("(instance Gas PhysicalState)").
:- load_kif("(subAttribute Gas Fluid)").
:- load_kif("(documentation Gas EnglishLanguage \"An &%Object has the &%Attribute of &%Gas if it has neither a fixed volume nor a fixed shape.\")").
:- load_kif("(<=> (instance ?OBJ Substance) (exists (?ATTR) (and (instance ?ATTR PhysicalState) (attribute ?OBJ ?ATTR))))").
:- load_kif("(instance Plasma PhysicalState)").
:- load_kif("(subAttribute Plasma Fluid)").
:- load_kif("(documentation Plasma EnglishLanguage \"An extremely energetic &%PhysicalState that consists of atomic nuclei stripped of electrons. That is, a plasma is composed of positive ions and free electrons. &%Plasma behaves differently enough from &%Gas that it is referred to as the fourth state of matter.\")").
% :- load_kif("; NS: delete. Instances of SoundAttribute should also be instances of PerceptualAttribute, but SoundAttribute is a subclass of RelationalAttribute, not of InternalAttribute. Some PerceptualAttributes are InternalAttributes. Others are RelationalAttributes. (subclass PerceptualAttribute InternalAttribute)").
% :- load_kif("; NS: add.").
:- load_kif("(subclass PerceptualAttribute Attribute)").
:- load_kif("(documentation PerceptualAttribute EnglishLanguage \"Any &%Attribute whose presence is detected by an act of &%Perception.\")").
:- load_kif("(=> (and (instance ?PERCEPTION Perception) (patient ?PERCEPTION ?OBJ)) (exists (?PROP) (and (instance ?PROP PerceptualAttribute) (attribute ?OBJ ?PROP))))").
:- load_kif("(subclass TasteAttribute PerceptualAttribute)").
:- load_kif("(documentation TasteAttribute EnglishLanguage \"The &%Class of &%Attributes relating to the taste of &%Objects.\")").
:- load_kif("(=> (instance ?OBJ (FoodForFn Animal)) (exists (?ATTR) (and (instance ?ATTR TasteAttribute) (attribute ?OBJ ?ATTR))))").
:- load_kif("(subclass OlfactoryAttribute PerceptualAttribute)").
:- load_kif("(documentation OlfactoryAttribute EnglishLanguage \"The &%Class of properties that are detectable by smell.\")").
:- load_kif("(subclass VisualAttribute PerceptualAttribute)").
:- load_kif("(documentation VisualAttribute EnglishLanguage \"The &%Class of visually discernible properties.\")").
:- load_kif("(instance Illuminated VisualAttribute)").
:- load_kif("(documentation Illuminated EnglishLanguage \"The &%Attribute of &%Regions that are illuminated to some degree, i.e. in which some shapes are visually discernable.\")").
:- load_kif("(instance Unilluminated VisualAttribute)").
:- load_kif("(contraryAttribute Unilluminated Illuminated)").
:- load_kif("(documentation Unilluminated EnglishLanguage \"The &%Attribute of &%Regions that are unilluminated, i.e in which no shapes are visually discernable.\")").
% :- load_kif("; NS: delete. (subclass ColorAttribute InternalAttribute)").
% :- load_kif("; NS: add.").
:- load_kif("(subclass ColorAttribute VisualAttribute)").
:- load_kif("(documentation ColorAttribute EnglishLanguage \"The &%Class of &%Attributes relating to the color of &%Objects.\")").
:- load_kif("(subclass PrimaryColor ColorAttribute)").
:- load_kif("(documentation PrimaryColor EnglishLanguage \"Colors which can be blended to form any color and which cannot be derived from any other colors.\")").
:- load_kif("(instance Red PrimaryColor)").
:- load_kif("(documentation Red EnglishLanguage \"The &%Attribute of redness.\")").
:- load_kif("(instance Blue PrimaryColor)").
:- load_kif("(documentation Blue EnglishLanguage \"The &%Attribute of being blue in color.\")").
:- load_kif("(instance Yellow PrimaryColor)").
:- load_kif("(documentation Yellow EnglishLanguage \"The &%Attribute of being yellow in color.\")").
:- load_kif("(instance White PrimaryColor)").
:- load_kif("(documentation White EnglishLanguage \"The &%Attribute of being white in color.\")").
:- load_kif("(instance Black PrimaryColor)").
:- load_kif("(documentation Black EnglishLanguage \"The &%Attribute of being black in color.\")").
:- load_kif("(instance Monochromatic ColorAttribute)").
:- load_kif("(documentation Monochromatic EnglishLanguage \"An &%Object with this &%Attribute has the same color on every part of its surface.\")").
:- load_kif("(=> (and (attribute ?OBJ Monochromatic) (superficialPart ?PART ?OBJ) (attribute ?PART ?COLOR) (instance ?COLOR PrimaryColor)) (forall (?ELEMENT) (=> (superficialPart ?ELEMENT ?OBJ) (attribute ?ELEMENT ?COLOR))))").
:- load_kif("(=> (instance ?OBJ Object) (or (attribute ?OBJ Monochromatic) (attribute ?OBJ Polychromatic)))").
:- load_kif("(instance Polychromatic ColorAttribute)").
:- load_kif("(contraryAttribute Polychromatic Monochromatic)").
:- load_kif("(documentation Polychromatic EnglishLanguage \"An &%Object with this &%Attribute has different colors on different parts of its surface.\")").
:- load_kif("(=> (attribute ?OBJ Polychromatic) (exists (?PART1 ?PART2 ?COLOR1 ?COLOR2) (and (superficialPart ?PART1 ?OBJ) (superficialPart ?PART2 ?OBJ) (attribute ?PART1 ?COLOR1) (attribute ?PART2 ?COLOR2) (instance ?COLOR1 ColorAttribute) (instance ?COLOR2 ColorAttribute) (not (equal ?COLOR1 ?COLOR2)))))").
% :- load_kif("; KJN: Moved in from Media.kif").
:- load_kif("(subclass StructureAttribute InternalAttribute)").
:- load_kif("(documentation StructureAttribute EnglishLanguage \"Each &%subclass of &%StructureAttribute denotes some facet of the structure of physical entities. Each &%instance of &%StructureAttribute denotes some structural characteristic that may pertain to some &%Physical entity.\")").
:- load_kif("(subclass ShapeAttribute StructureAttribute)").
% :- load_kif("; KJN: Delete as StructureAttribute already subclasses from InternalAttribute (subclass ShapeAttribute InternalAttribute)").
:- load_kif("(documentation ShapeAttribute EnglishLanguage \"Any &%Attribute that relates to the shape of an &%Object.\")").
:- load_kif("(=> (and (instance ?ATTRIBUTE ShapeAttribute) (attribute ?OBJ ?ATTRIBUTE) (surface ?SURFACE ?OBJ)) (attribute ?SURFACE ?ATTRIBUTE))").
:- load_kif("(instance Pliable InternalAttribute)").
:- load_kif("(documentation Pliable EnglishLanguage \"The shape of an &%Object with this &%Attribute can easily be altered.\")").
:- load_kif("(=> (and (instance ?OBJ Object) (attribute ?OBJ Pliable)) (exists (?CHANGE) (and (instance ?CHANGE ShapeChange) (patient ?CHANGE ?OBJ))))").
:- load_kif("(instance Rigid InternalAttribute)").
:- load_kif("(contraryAttribute Rigid Pliable)").
:- load_kif("(documentation Rigid EnglishLanguage \"The shape of an &%Object with this &%Attribute cannot be altered without breaking.\")").
:- load_kif("(subclass TextureAttribute PerceptualAttribute)").
:- load_kif("(documentation TextureAttribute EnglishLanguage \"Any &%Attribute that characterizes the texture of an &%Object.\")").
:- load_kif("(=> (and (instance ?ATTRIBUTE TextureAttribute) (attribute ?OBJ ?ATTRIBUTE) (surface ?SURFACE ?OBJ)) (attribute ?SURFACE ?ATTRIBUTE))").
:- load_kif("(instance Smooth TextureAttribute)").
:- load_kif("(documentation Smooth EnglishLanguage \"An &%Object with this &%Attribute has a smooth surface.\")").
:- load_kif("(instance Rough TextureAttribute)").
:- load_kif("(contraryAttribute Smooth Rough)").
:- load_kif("(documentation Rough EnglishLanguage \"An &%Object with this &%Attribute has a rough surface.\")").
:- load_kif("(subclass GeometricFigure ShapeAttribute)").
:- load_kif("(partition GeometricFigure GeometricPoint OneDimensionalFigure TwoDimensionalFigure ThreeDimensionalFigure)").
:- load_kif("(documentation GeometricFigure EnglishLanguage \"The class of all geometric figures, i.e. the class of all abstract, spatial representations. The instances of this class are &%GeometricPoints, &%TwoDimensionalFigures or &%ThreeDimensionalFigures.\")").
:- load_kif("(subclass GeometricPoint GeometricFigure)").
:- load_kif("(documentation GeometricPoint EnglishLanguage \"The class of zero-dimensional &%GeometricFigures, i.e. the class of &%GeometricFigures that have position but lack extension in any dimension.\")").
:- load_kif("(subclass OneDimensionalFigure GeometricFigure)").
:- load_kif("(documentation OneDimensionalFigure EnglishLanguage \"The class of &%GeometricFigures that have position and an extension along a single dimension, viz. straight lines.\")").
:- load_kif("(subclass TwoDimensionalFigure GeometricFigure)").
:- load_kif("(partition TwoDimensionalFigure OpenTwoDimensionalFigure ClosedTwoDimensionalFigure)").
:- load_kif("(documentation TwoDimensionalFigure EnglishLanguage \"The class of &%GeometricFigures that have position and an extension along two dimensions, viz. plane figures like circles and polygons.\")").
:- load_kif("(subclass OpenTwoDimensionalFigure TwoDimensionalFigure)").
:- load_kif("(documentation OpenTwoDimensionalFigure EnglishLanguage \"The class of &%TwoDimensionalFigures that are not &%ClosedTwoDimensionalFigures.\")").
:- load_kif("(subclass TwoDimensionalAngle OpenTwoDimensionalFigure)").
:- load_kif("(documentation TwoDimensionalAngle EnglishLanguage \"Any two &%OneDimensionalFigures (i.e. straight lines) meeting at a single &%GeometricPoint.\")").
:- load_kif("(subclass ClosedTwoDimensionalFigure TwoDimensionalFigure)").
:- load_kif("(documentation ClosedTwoDimensionalFigure EnglishLanguage \"Any &%TwoDimensionalFigure which has a well defined interior and exterior.\")").
:- load_kif("(subclass ThreeDimensionalFigure GeometricFigure)").
:- load_kif("(documentation ThreeDimensionalFigure EnglishLanguage \"The class of &%GeometricFigures that have position and an extension along three dimensions, viz. geometric solids like polyhedrons and cylinders.\")").
:- load_kif("(instance geometricPart BinaryPredicate)").
:- load_kif("(instance geometricPart PartialOrderingRelation)").
:- load_kif("(domain geometricPart 1 GeometricFigure)").
:- load_kif("(domain geometricPart 2 GeometricFigure)").
:- load_kif("(documentation geometricPart EnglishLanguage \"(&%geometricPart ?PART ?WHOLE) means that the &%GeometricFigure ?PART is part of the &%GeometricFigure ?WHOLE.\")").
:- load_kif("(subrelation pointOfFigure geometricPart)").
:- load_kif("(domain pointOfFigure 1 GeometricPoint)").
:- load_kif("(domain pointOfFigure 2 GeometricFigure)").
:- load_kif("(documentation pointOfFigure EnglishLanguage \"(&%pointOfFigure ?POINT ?FIGURE) means that the &%GeometricPoint ?POINT is part of the &%GeometricFigure ?FIGURE.\")").
:- load_kif("(subrelation angleOfFigure geometricPart)").
:- load_kif("(domain angleOfFigure 1 TwoDimensionalAngle)").
:- load_kif("(domain angleOfFigure 2 GeometricFigure)").
:- load_kif("(documentation angleOfFigure EnglishLanguage \"(&%angleOfFigure ?ANGLE ?FIGURE) means that the &%TwoDimensionalAngle ?ANGLE is part of the &%GeometricFigure ?FIGURE.\")").
:- load_kif("(instance pointOfIntersection TernaryPredicate)").
:- load_kif("(domain pointOfIntersection 1 OneDimensionalFigure)").
:- load_kif("(domain pointOfIntersection 2 OneDimensionalFigure)").
:- load_kif("(domain pointOfIntersection 3 GeometricPoint)").
:- load_kif("(documentation pointOfIntersection EnglishLanguage \"(&%pointOfIntersection ?FIGURE1 ?FIGURE2 ?POINT) means that the two straight lines ?FIGURE1 and ?FIGURE2 meet at the point ?POINT.\")").
:- load_kif("(=> (pointOfIntersection ?FIGURE1 ?FIGURE2 ?POINT) (and (pointOfFigure ?POINT ?FIGURE1) (pointOfFigure ?POINT ?FIGURE2)))").
:- load_kif("(instance parallel BinaryPredicate)").
:- load_kif("(domain parallel 1 OneDimensionalFigure)").
:- load_kif("(domain parallel 2 OneDimensionalFigure)").
:- load_kif("(documentation parallel EnglishLanguage \"(&%parallel ?LINE1 ?LINE2) means that the &%OneDimensionalFigures ?LINE1 and ?LINE2 are parallel to one another, i.e. they are equidistant from one another at every point.\")").
:- load_kif("(=> (parallel ?LINE1 ?LINE2) (not (exists (?POINT) (pointOfIntersection ?LINE1 ?LINE2 ?POINT))))").
:- load_kif("(instance angularMeasure BinaryPredicate)").
:- load_kif("(instance angularMeasure TotalValuedRelation)").
:- load_kif("(domain angularMeasure 1 TwoDimensionalAngle)").
:- load_kif("(domain angularMeasure 2 PlaneAngleMeasure)").
:- load_kif("(documentation angularMeasure EnglishLanguage \"(&%angularMeasure ?ANGLE ?MEASURE) means that the two-dimensional geometric angle ?ANGLE has the &%PlaneAngleMeasure of ?MEASURE.\")").
:- load_kif("(instance lineMeasure BinaryPredicate)").
:- load_kif("(instance lineMeasure TotalValuedRelation)").
:- load_kif("(domain lineMeasure 1 OneDimensionalFigure)").
:- load_kif("(domain lineMeasure 2 LengthMeasure)").
:- load_kif("(documentation lineMeasure EnglishLanguage \"(&%lineMeasure ?LINE ?MEASURE) means that the straight line ?LINE has the &%LengthMeasure of ?MEASURE.\")").
:- load_kif("(instance geometricDistance TernaryPredicate)").
:- load_kif("(instance geometricDistance SingleValuedRelation)").
:- load_kif("(instance geometricDistance TotalValuedRelation)").
:- load_kif("(domain geometricDistance 1 GeometricPoint)").
:- load_kif("(domain geometricDistance 2 GeometricPoint)").
:- load_kif("(domain geometricDistance 3 LengthMeasure)").
:- load_kif("(documentation geometricDistance EnglishLanguage \"(&%geometricDistance ?POINT1 ?POINT2 ?LENGTH) means that ?LENGTH is the distance between the two &%GeometricPoints ?POINT1 and ?POINT2.\")").
:- load_kif("(=> (geometricDistance ?POINT1 ?POINT2 ?LENGTH) (geometricDistance ?POINT2 ?POINT1 ?LENGTH))").
:- load_kif("(subclass SaturationAttribute InternalAttribute)").
:- load_kif("(documentation SaturationAttribute EnglishLanguage \"A &%Class of &%Attributes that specify, in a qualitative manner, the extent of the presence of one kind of &%Object in another kind of &%Object.\")").
:- load_kif("(instance Dry SaturationAttribute)").
:- load_kif("(contraryAttribute Dry Damp)").
:- load_kif("(documentation Dry EnglishLanguage \"An &%Attribute which indicates that the associated &%Object contains no &%Liquid.\")").
:- load_kif("(=> (attribute ?OBJ Dry) (not (exists (?SUBOBJ) (and (part ?SUBOBJ ?OBJ) (attribute ?SUBOBJ Liquid)))))").
:- load_kif("(instance Damp SaturationAttribute)").
:- load_kif("(documentation Damp EnglishLanguage \"An &%Attribute which indicates that the associated &%Object contains some &%Liquid.\")").
:- load_kif("(instance Wet SaturationAttribute)").
:- load_kif("(subAttribute Wet Damp)").
:- load_kif("(documentation Wet EnglishLanguage \"An &%Attribute which indicates that the associated &%Object is fully saturated with a &%Liquid, i.e. every part of the &%Object has a subpart which is a &%Liquid.\")").
:- load_kif("(=> (attribute ?OBJ Wet) (forall (?PART) (=> (part ?PART ?OBJ) (exists (?SUBPART) (and (part ?SUBPART ?PART) (attribute ?SUBPART Liquid))))))").
:- load_kif("(subclass BiologicalAttribute InternalAttribute)").
:- load_kif("(documentation BiologicalAttribute EnglishLanguage \"&%Attributes that apply specifically to instances of &%Organism.\")").
% :- load_kif("; NS: delete. Many current instances of BiologicalAttribute apply to OrganicObjects that are not Organisms. (=> (and (attribute ?ORG ?ATT) (instance ?ATT BiologicalAttribute)) (instance ?ORG Organism))").
% :- load_kif("; NS: add.").
:- load_kif("(=> (and (attribute ?ORG ?ATT) (instance ?ATT BiologicalAttribute)) (instance ?ORG OrganicObject))").
:- load_kif("(subclass BodyPosition BiologicalAttribute)").
:- load_kif("(documentation BodyPosition EnglishLanguage \"The class of &%Attributes expressing configurations of bodies or parts of bodies of animals or humans, e.g. standing, sitting, kneeling, lying down, etc.\")").
:- load_kif("(=> (instance ?ANIMAL Animal) (or (exists (?MOTION) (and (instance ?MOTION BodyMotion) (agent ?MOTION ?ANIMAL))) (exists (?ATTR) (and (instance ?ATTR BodyPosition) (attribute ?ANIMAL ?ATTR)))))").
:- load_kif("(instance Standing BodyPosition)").
:- load_kif("(documentation Standing EnglishLanguage \"The &%BodyPosition of being upright, i.e. being fully extended and supported by nothing other than one's own feet.\")").
:- load_kif("(=> (and (instance ?AMBULATE Ambulating) (agent ?AMBULATE ?AGENT)) (attribute ?AGENT Standing))").
:- load_kif("(instance Sitting BodyPosition)").
:- load_kif("(documentation Sitting EnglishLanguage \"The &%BodyPosition of being recumbent, i.e. knees bent and back side supported.\")").
:- load_kif("(instance Prostrate BodyPosition)").
:- load_kif("(documentation Prostrate EnglishLanguage \"The &%BodyPosition of lying down, being in a horizontal position.\")").
:- load_kif("(subclass AnimacyAttribute BiologicalAttribute)").
:- load_kif("(exhaustiveAttribute AnimacyAttribute Living Dead)").
:- load_kif("(documentation AnimacyAttribute EnglishLanguage \"&%Attributes that indicate whether an &%Organism is alive or not.\")").
:- load_kif("(instance Living AnimacyAttribute)").
:- load_kif("(documentation Living EnglishLanguage \"This &%Attribute applies to &%Organisms that are alive.\")").
:- load_kif("(=> (and (instance ?ORGANISM Organism) (agent ?PROCESS ?ORGANISM)) (holdsDuring (WhenFn ?PROCESS) (attribute ?ORGANISM Living)))").
:- load_kif("(instance Dead AnimacyAttribute)").
:- load_kif("(subAttribute Dead Unconscious)").
:- load_kif("(contraryAttribute Dead Living)").
:- load_kif("(documentation Dead EnglishLanguage \"This &%Attribute applies to &%Organisms that are not alive.\")").
:- load_kif("(=> (instance ?ORG Organism) (exists (?ATTR) (and (instance ?ATTR AnimacyAttribute) (attribute ?ORG ?ATTR))))").
:- load_kif("(subclass SexAttribute BiologicalAttribute)").
:- load_kif("(exhaustiveAttribute SexAttribute Female Male)").
:- load_kif("(documentation SexAttribute EnglishLanguage \"&%Attributes that indicate the sex of an &%Organism.\")").
:- load_kif("(instance Female SexAttribute)").
:- load_kif("(documentation Female EnglishLanguage \"An &%Attribute indicating that an &%Organism is female in nature.\")").
:- load_kif("(=> (and (instance ?BODY ReproductiveBody) (part ?BODY ?ORG) (instance ?ORG Organism)) (attribute ?ORG Female))").
:- load_kif("(instance Male SexAttribute)").
:- load_kif("(contraryAttribute Male Female)").
:- load_kif("(documentation Male EnglishLanguage \"An &%Attribute indicating that an &%Organism is male in nature.\")").
:- load_kif("(=> (instance ?ANIMAL Animal) (exists (?ATTR) (and (instance ?ATTR SexAttribute) (attribute ?ANIMAL ?ATTR))))").
:- load_kif("(subclass DevelopmentalAttribute BiologicalAttribute)").
:- load_kif("(exhaustiveAttribute DevelopmentalAttribute FullyFormed NonFullyFormed)").
:- load_kif("(documentation DevelopmentalAttribute EnglishLanguage \"&%Attributes that indicate the stage of development of an &%Organism.\")").
:- load_kif("(instance FullyFormed DevelopmentalAttribute)").
:- load_kif("(documentation FullyFormed EnglishLanguage \"The stage of an &%Organism when it has reached the end of its growth phase.\")").
:- load_kif("(=> (attribute ?OBJ FullyFormed) (exists (?GROWTH) (and (instance ?GROWTH Growth) (experiencer ?GROWTH ?OBJ) (holdsDuring (BeginFn (WhenFn ?OBJ)) (attribute ?OBJ NonFullyFormed)))))").
:- load_kif("(instance NonFullyFormed DevelopmentalAttribute)").
:- load_kif("(contraryAttribute NonFullyFormed FullyFormed)").
:- load_kif("(successorAttribute NonFullyFormed FullyFormed)").
:- load_kif("(documentation NonFullyFormed EnglishLanguage \"The stage of an &%Organism before it is &%FullyFormed.\")").
:- load_kif("(=> (instance ?ORG Organism) (exists (?ATTR) (and (instance ?ATTR DevelopmentalAttribute) (attribute ?ORG ?ATTR))))").
:- load_kif("(subAttribute Larval NonFullyFormed)").
:- load_kif("(documentation Larval EnglishLanguage \"Form of most &%Invertebrates, &%Amphibians, and &%Fish immediately after they hatch. This form is fundamentally unlike the adult form, and metamorphosis is required to reach the latter form.\")").
% :- load_kif("; NS: delete. (=> (holdsDuring ?TIME (attribute ?ORG Larval)) (holdsDuring (PastFn ?TIME) (exists (?BIRTH) (and (instance ?BIRTH Birth) (experiencer ?BIRTH ?ORG)))))").
% :- load_kif("; NS: add.").
:- load_kif("(=> (and (holdsDuring ?TIME (attribute ?ORG Larval)) (instance ?BIRTH Birth) (equal ?BW (WhenFn ?BIRTH)) (experiencer ?BIRTH ?ORG)) (meetsTemporally ?BW ?TIME))").
:- load_kif("(subAttribute Embryonic NonFullyFormed)").
:- load_kif("(contraryAttribute Embryonic Larval)").
:- load_kif("(documentation Embryonic EnglishLanguage \"The stage of an &%Organism or an &%AnatomicalStructure that exists only before the &%Organism is born. &%Mammals, for example, have this &%Attribute only prior to their birth.\")").
:- load_kif("(=> (attribute ?ORG Embryonic) (exists (?BODY) (and (instance ?BODY ReproductiveBody) (located ?ORG ?BODY))))").
% :- load_kif("; NS: delete. It's best not to put either (exists ... or (not (exists ... inside the scope of holdsDuring. (=> (holdsDuring ?TIME (attribute ?ORG Embryonic)) (holdsDuring ?TIME (not (exists (?BIRTH) (and (instance ?BIRTH Birth) (experiencer ?BIRTH ?ORG))))))").
% :- load_kif("; NS: add.").
:- load_kif("(=> (and (holdsDuring ?TIME (attribute ?ORG Embryonic)) (instance ?BIRTH Birth) (equal ?BW (WhenFn ?BIRTH)) (experiencer ?BIRTH ?ORG)) (not (overlapsTemporally ?TIME ?BW)))").
:- load_kif("(subclass DiseaseOrSyndrome BiologicalAttribute)").
:- load_kif("(documentation DiseaseOrSyndrome EnglishLanguage \"A &%BiologicalAttribute which qualifies something that alters or interferes with a normal process, state or activity of an &%Organism. It is usually characterized by the abnormal functioning of one or more of the host's systems, parts, or &%Organs.\")").
% :- load_kif("; NS: If we wanted to ascribe PsychologicalAttributes to a non-OrganicObject robot or an AI, this statement would have to be changed.").
:- load_kif("(subclass PsychologicalAttribute BiologicalAttribute)").
:- load_kif("(partition PsychologicalAttribute StateOfMind TraitAttribute)").
:- load_kif("(documentation PsychologicalAttribute EnglishLanguage \"&%Attributes that characterize the mental or behavioral life of an &%Organism.\")").
% :- load_kif("; NS: delete. (=> (instance ?ATTR PsychologicalAttribute) (=> (holdsDuring ?TIME (attribute ?ORGANISM ?ATTR)) (holdsDuring ?TIME (attribute ?ORGANISM Living))))").
% :- load_kif("; NS: add.").
:- load_kif("(=> (instance ?ATTR PsychologicalAttribute) (=> (and (holdsDuring ?TIME (attribute ?ORGANISM ?ATTR)) (instance ?ORGANISM Organism)) (holdsDuring ?TIME (attribute ?ORGANISM Living))))").
:- load_kif("(=> (and (instance ?ATTR PsychologicalAttribute) (attribute ?AGENT ?ATTR)) (instance ?AGENT SentientAgent))").
:- load_kif("(subclass StateOfMind PsychologicalAttribute)").
:- load_kif("(documentation StateOfMind EnglishLanguage \"The class &%StateOfMind is distinguished from its complement &%TraitAttribute by the fact that instances of the former are transient while instances of the latter are persistent features of a creature's behavioral/psychological make-up.\")").
:- load_kif("(subclass EmotionalState StateOfMind)").
:- load_kif("(documentation EmotionalState EnglishLanguage \"The &%Class of &%Attributes that denote emotional states of &%Organisms.\")").
:- load_kif("(subclass ConsciousnessAttribute StateOfMind)").
:- load_kif("(documentation ConsciousnessAttribute EnglishLanguage \"&%Attributes that indicate whether an &%Organism is conscious or the qualitative degree of consciousness of an &%Organism.\")").
:- load_kif("(<=> (and (instance ?AGENT SentientAgent) (attribute ?AGENT Living)) (exists (?ATTR) (and (instance ?ATTR ConsciousnessAttribute) (attribute ?AGENT ?ATTR))))").
:- load_kif("(instance Asleep ConsciousnessAttribute)").
:- load_kif("(documentation Asleep EnglishLanguage \"&%Attribute that applies to &%Organisms that are sleeping.\")").
:- load_kif("(instance Unconscious ConsciousnessAttribute)").
:- load_kif("(contraryAttribute Unconscious Awake)").
:- load_kif("(documentation Unconscious EnglishLanguage \"&%Attribute that applies to &%Organisms that are unconscious. An &%Organism may be &%Unconscious because it is &%Dead or because of a blow to the head, a drug, etc.\")").
:- load_kif("(instance Awake ConsciousnessAttribute)").
:- load_kif("(documentation Awake EnglishLanguage \"&%Attribute that applies to &%Organisms that are neither &%Unconscious nor &%Asleep.\")").
:- load_kif("(=> (or (attribute ?AGENT Asleep) (attribute ?AGENT Awake)) (attribute ?AGENT Living))").
:- load_kif("(subclass TraitAttribute PsychologicalAttribute)").
:- load_kif("(documentation TraitAttribute EnglishLanguage \"&%Attributes that indicate the the behavior/personality traits of an &%Organism.\")").
:- load_kif("(subclass PsychologicalDysfunction PsychologicalAttribute)").
:- load_kif("(subclass PsychologicalDysfunction DiseaseOrSyndrome)").
:- load_kif("(documentation PsychologicalDysfunction EnglishLanguage \"A clinically significant dysfunction whose major manifestation is behavioral or psychological. These dysfunctions may have identified or presumed biological etiologies or manifestations.\")").
:- load_kif("(instance comment TernaryPredicate)").
:- load_kif("(documentation comment EnglishLanguage \"(&%comment ?ENT ?STR ?PER) is a convenience relationship that allows ontologists represented by &%SymbolicString ?PER to write down commentaries ?STR on a defined &%Entity ?ENT\")").
:- load_kif("(termFormat EnglishLanguage comment \"comment\")").
:- load_kif("(domain comment 1 Entity)").
:- load_kif("(domain comment 2 SymbolicString)").
:- load_kif("(domain comment 3 SymbolicString)").
:- load_kif("(instance MakingFn UnaryFunction)").
:- load_kif("(domainSubclass MakingFn 1 Making)").
:- load_kif("(rangeSubclass MakingFn Object)").
:- load_kif("(documentation MakingFn EnglishLanguage \"A &%Function that denotes the creation of an &%Object.\")").
:- load_kif("(=> (instance ?X (MakingFn ?Y)) (exists (?OBJECT) (and (instance ?OBJECT ?Y) (result ?X ?OBJECT))))").
end_of_file.
% :- load_kif("; END FILE").
% :- load_kif("; BEGIN FILE").
% :- load_kif(";  Sequestered Axioms  ").
% :- load_kif("; This section is not a subontology of the SUMO. It contains axioms from other sections of the ontology that may cause problems for an inference engine. In particular, they can be used to construct arbitrarily complex terms that often appear in proofs with unhelpful conclusions. Before this file is loaded into the inference engine, the axioms in this section should be commented out.").
% :- load_kif(" (<=> (instance ?ENTITY (UnionFn ?CLASS1 ?CLASS2)) (or (instance ?ENTITY ?CLASS1) (instance ?ENTITY ?CLASS2)))").
% :- load_kif(" (<=> (instance ?ENTITY (IntersectionFn ?CLASS1 ?CLASS2)) (and (instance ?ENTITY ?CLASS1) (instance ?ENTITY ?CLASS2)))").
% :- load_kif(" (<=> (instance ?ENTITY (ComplementFn ?CLASS)) (not (instance ?ENTITY ?CLASS)))").
% :- load_kif(" (=> (and (instance ?CLASS1 SetOrClass) (instance ?CLASS2 SetOrClass)) (equal (RelativeComplementFn ?CLASS1 ?CLASS2) (IntersectionFn ?CLASS1 (ComplementFn ?CLASS2))))").
% :- load_kif(" (<=> (instance ?ENTITY (GeneralizedUnionFn ?SUPERCLASS)) (exists (?CLASS) (and (instance ?CLASS ?SUPERCLASS) (instance ?ENTITY ?CLASS))))").
% :- load_kif(" (<=> (instance ?ENTITY (GeneralizedIntersectionFn ?SUPERCLASS)) (forall (?CLASS) (=> (instance ?CLASS ?SUPERCLASS) (instance ?ENTITY ?CLASS))))").
% :- load_kif(" (<=> (instance ?SUBCLASS (PowerSetFn ?CLASS)) (subclass ?SUBCLASS ?CLASS))").
% :- load_kif(" (=> (instance ?REL Function) (<=> (equal (AssignmentFn ?REL @ROW) ?INST) (?REL @ROW ?INST)))").
%  NS: sequestered 2007-08-01 
% :- load_kif(";  (<=> (instance ?REL BinaryRelation) (not (exists (?ITEM1 ?ITEM2 ?ITEM3 @ROW) (?REL ?ITEM1 ?ITEM2 ?ITEM3 @ROW))))").
% :- load_kif(";  (=> (instance ?REL TernaryRelation) (not (exists (?ITEM1 ?ITEM2 ?ITEM3 ?ITEM4 @ROW) (?REL ?ITEM1 ?ITEM2 ?ITEM3 ?ITEM4 @ROW))))").
% :- load_kif(";  (=> (instance ?REL QuaternaryRelation) (not (exists (?ITEM1 ?ITEM2 ?ITEM3 ?ITEM4 ?ITEM5 @ROW) (?REL ?ITEM1 ?ITEM2 ?ITEM3 ?ITEM4 ?ITEM5 @ROW))))").
% :- load_kif(";  (=> (instance ?REL QuintaryRelation) (not (exists (?ITEM1 ?ITEM2 ?ITEM3 ?ITEM4 ?ITEM5 ?ITEM6 @ROW) (?REL ?ITEM1 ?ITEM2 ?ITEM3 ?ITEM4 ?ITEM5 ?ITEM6 @ROW))))").
