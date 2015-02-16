;; ================================================
;;   SMINK011 An ontology derived from
;;      SUMO (Suggested Upper Merged Ontology) (Teknowledge)
;;        (from merge.txt version 1.566 for Ontolog ontology)
;;      MILO (MId-LevelOntology)  (Teknowledge)
;;       and Invoices -- an ontology developed by the Ontolog
;;           study group
;;      file version samin011.txt -- modified for import into Protege
;;           by P. Cassidy.
;;      Last edit March 2, 2004
;; ================================================

;;  Part 1 -- SUMO 1.56 
;; This is the source file for the SUMO (Suggested Upper Merged Ontology),
;; an ontology that was developed within the SUO Working Group by merging
;; the SUO "candidate content" sources and refining and extending this content on
;; the basis of various knowledge engineering projects and input from the SUO
;; Working Group.

;; The SUMO incorporates elements of John Sowa's upper ontology (as described at
;; http://www.bestweb.net/~sowa/ontology/toplevel.htm and in Chapter 2 of his
;; book _Knowledge Representation_, Brooks/Cole, 2000), Russell and Norvig's
;; ontology, PSL (Process Specification Language), Casati and Varzi's theory of
;; holes, Allen's temporal axioms, the relatively noncontroversial elements of
;; Smith's and Guarino's respective mereotopologies, the KIF formalization of the
;; CPR (Core Plan Representation), the ontologies available on the Ontolingua
;; server maintained by Stanford University's Knowledge Systems Laboratory, the
;; ontologies developed by ITBM-CNR, some of the spatial relations from an
;; unpublished paper by Iris Tommelein and Anil Gupta entitled "Conceptual
;; Structures for Spatial Reasoning", and a "Structural Ontology" proposed by
;; David Whitten and substantially revised and extended by Chris Menzel.
;; Note that some of the subclasses of 'Process' in the SUMO were originally
;; inspired by some of the verb classes from the second part of Beth Levin's book
;; "English Verb Classes and Alternations:  A Preliminary Investigation."

;; The knowledge representation language in which the SUMO is expressed is SUO-KIF,
;; which stands for "Standard Upper Ontology - Knowledge Interchange Format".  SUO-KIF
;; is a simplified form of the popular KIF knowledge representation language.  A
;; specification of SUO-KIF can be found at:  http://suo.ieee.org/suo-kif.html.  It
;; should be noted that some of the axioms in the SUMO make use of row variables
;; (indicated with a "@" prefix).  Such variables are not currently part of the SUO-
;; KIF specification, but they simplify matters significantly in some cases.  Details
;; about row variables can be found in the following paper:
;; http://reliant.teknowledge.com/IJCAI01/HayesMenzel-SKIF-IJCAI2001.pdf.

;; The SUMO is a modular ontology.  That is, the ontology is divided into
;; self-contained subontologies.  Each subontology is indicated by a section
;; header, and the dependencies between the subontologies are specified with
;; statements of the form ";; INCLUDES '<SUBONTOLOGY>'".  These statements are
;; found at the beginning of each section.  The dependencies between the
;; various subontologies can also be graphed informally as follows:
;;
;;                             STRUCTURAL ONTOLOGY
;;                                      +
;;                                      |
;;                                      |
;;                                      +
;;                                 BASE ONTOLOGY
;;                                 /   |   |   \
;;                                /    |   |    \
;;                               /     |   |     \
;;                              /      |   |      \
;;                             /       |   |       \
;;                            +        +   +        +
;;             SET/CLASS THEORY   NUMERIC  TEMPORAL  MEREOTOPOLOGY
;;                               /     |        |           |
;;                              /      |        |           |
;;                             /       |        |           |
;;                            +        +        +           +
;;                        GRAPH   MEASURE   PROCESSES +--+ OBJECTS
;;                                              +           +
;;                                               \         /
;;                                                \       /
;;                                                 \     /
;;                                                  +   +
;;                                                 QUALITIES
;;
;;
;; Note that the "+" sign at the end of an arc indicates the direction of
;; dependency - the node near the sign includes the subontology at the other
;; end of the arc.  Note too that in some cases the dependency is
;; bidirectional.  Separating ontologies in cases like these is useful when
;; their respective topics can be cleanly differentiated.

;; The SUMO is copyrighted by Teknowledge (c) 2002.  It is released under the
;; GNU Public License <http://www.gnu.org/copyleft/gpl.html>.  Users of this
;; code also consent, by use of this material, to credit Teknowledge in any writings,
;; briefings, publications, presentations, or other representations of any code
;; or other product which incorporates, builds on, or uses this material.


;; <module>STRUCTURAL_ONTOLOGY</module>
;; BEGIN FILE


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   STRUCTURAL ONTOLOGY   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; INCLUDES 'BASE ONTOLOGY'

;; The Structural Ontology consists of definitions of certain syntactic
;; abbreviations that can be both heuristically useful and computationally
;; advantageous.

(instance instance BinaryPredicate)
(domain instance 1 Entity)
(domain instance 2 SetOrClass)
(documentation instance "An object is an &%instance of a &%SetOrClass if
it is included in that &%SetOrClass.  An individual may be an instance of many
classes, some of which may be subclasses of others.  Thus, there is no
assumption in the meaning of &%instance about specificity or uniqueness.")

(subrelation immediateInstance instance)
(instance immediateInstance AsymmetricRelation)
(instance immediateInstance IntransitiveRelation)
(documentation immediateInstance "An object is an &%immediateInstance of
a &%SetOrClass if it is an instance of the &%SetOrClass and there does not exist a subclass of &%SetOrClass such that it is an instance of the subclass.")

(=>
   (immediateInstance ?ENTITY ?CLASS)
   (not (exists (?SUBCLASS)
      (and
         (subclass ?SUBCLASS ?CLASS)
         (instance ?ENTITY ?SUBCLASS)))))

(instance inverse BinaryPredicate)
(instance inverse IrreflexiveRelation)
(instance inverse IntransitiveRelation)
(instance inverse SymmetricRelation)
(domain inverse 1 BinaryRelation)
(domain inverse 2 BinaryRelation)
(documentation inverse "The inverse of a &%BinaryRelation is a relation
in which all the tuples of the original relation are reversed.  In
other words, one &%BinaryRelation is the inverse of another if they are
equivalent when their arguments are swapped.")

(=>
   (inverse ?REL1 ?REL2)
   (forall (?INST1 ?INST2)
      (<=>
         (holds ?REL1 ?INST1 ?INST2)
         (holds ?REL2 ?INST2 ?INST1))))

(instance subclass BinaryPredicate)
(instance subclass PartialOrderingRelation)
(domain subclass 1 SetOrClass)
(domain subclass 2 SetOrClass)
(documentation subclass "(&%subclass ?CLASS1 ?CLASS2) means that ?CLASS1 is
a subclass of ?CLASS2, i.e. every instance of ?CLASS1 is also an instance of
?CLASS2.  A class may have multiple superclasses and subclasses.")

(<=>
   (subclass ?SUBCLASS ?CLASS)
   (and
      (instance ?SUBCLASS SetOrClass)
      (instance ?CLASS SetOrClass)
      (forall (?INST)
         (=>
            (instance ?INST ?SUBCLASS)
            (instance ?INST ?CLASS)))))

(subrelation immediateSubclass subclass)
(instance immediateSubclass AsymmetricRelation)
(instance immediateSubclass IntransitiveRelation)
(documentation immediateSubclass "A &%SetOrClass ?CLASS1 is an &%immediateSubclass
of another &%SetOrClass ?CLASS2 just in case ?CLASS1 is a subclass of ?CLASS2 and
there is no other subclass of ?CLASS2 such that ?CLASS1 is also a subclass of it.")

(=>
   (immediateSubclass ?CLASS1 ?CLASS2)
   (not (exists (?CLASS3)
      (and
         (subclass ?CLASS3 ?CLASS2)
         (subclass ?CLASS1 ?CLASS3)
         (not (equal ?CLASS2 ?CLASS3))
         (not (equal ?CLASS1 ?CLASS3))))))

(instance subrelation BinaryPredicate)
(instance subrelation PartialOrderingRelation)
(domain subrelation 1 Relation)
(domain subrelation 2 Relation)
(documentation subrelation "(&%subrelation ?REL1 ?REL2) means that
every tuple of ?REL1 is also a tuple of ?REL2.  In other words, if
the &%Relation ?REL1 holds for some arguments arg_1, arg_2, ... arg_n,
then the &%Relation ?REL2 holds for the same arguments.  A consequence
of this is that a &%Relation and its subrelations must have the same
&%valence. In CycL, &%subrelation is called #$genlPreds.")

(=>
   (and
      (subrelation ?PRED1 ?PRED2)
      (valence ?PRED1 ?NUMBER))
   (valence ?PRED2 ?NUMBER))

(=>
   (and
      (subrelation ?PRED1 ?PRED2)
      (domain ?PRED2 ?NUMBER ?CLASS1))
   (domain ?PRED1 ?NUMBER ?CLASS1))

(=>
   (and
      (subrelation ?REL1 ?REL2)
      (holds ?REL1 @ROW))
   (holds ?REL2 @ROW))

(=>
   (and
      (subrelation ?PRED1 ?PRED2)
      (instance ?PRED2 ?CLASS)
      (instance ?CLASS InheritableRelation))
   (instance ?PRED1 ?CLASS))

(instance domain TernaryPredicate)
(domain domain 1 Relation)
(domain domain 2 PositiveInteger)
(domain domain 3 SetOrClass)
(documentation domain "Provides a computationally and heuristically
convenient mechanism for declaring the argument types of a given relation.
The formula (&%domain ?REL ?INT ?CLASS) means that the ?INT'th element of each
tuple in the relation ?REL must be an instance of ?CLASS.  Specifying argument
types is very helpful in maintaining ontologies.  Representation systems can
use these specifications to classify terms and check integrity constraints.
If the restriction on the argument type of a &%Relation is not captured by a
&%SetOrClass already defined in the ontology, one can specify a &%SetOrClass
compositionally with the functions &%UnionFn, &%IntersectionFn, etc.")

;; <axiomgloss>The following axiom is relevant to cases where a subrelation of 
;; a parent relation has its own declared domain, which must be a subclass
;; of the corresponding parent domain class.

(=>
   (and
      (domain ?REL ?NUMBER ?CLASS1)
      (domain ?REL ?NUMBER ?CLASS2))
   (or
      (subclass ?CLASS1 ?CLASS2)
      (subclass ?CLASS2 ?CLASS1)))

(instance domainSubclass TernaryPredicate)
(domain domainSubclass 1 Relation)
(domain domainSubclass 2 PositiveInteger)
(domain domainSubclass 3 SetOrClass)
(documentation domainSubclass "&%Predicate used to specify argument
type restrictions of &%Predicates.  The formula (&%domainSubclass
?REL ?INT ?CLASS) means that the ?INT'th element of each tuple in the
relation ?REL must be a subclass of ?CLASS.")

(=>
   (and
      (subrelation ?REL1 ?REL2)
      (domainSubclass ?REL2 ?NUMBER ?CLASS1))
   (domainSubclass ?REL1 ?NUMBER ?CLASS1))

(=>
   (and
      (domainSubclass ?REL ?NUMBER ?CLASS1)
      (domainSubclass ?REL ?NUMBER ?CLASS2))
   (or
      (subclass ?CLASS1 ?CLASS2)
      (subclass ?CLASS2 ?CLASS1)))

(instance equal BinaryPredicate)
(instance equal EquivalenceRelation)
(instance equal RelationExtendedToQuantities)
(domain equal 1 Entity)
(domain equal 2 Entity)
(documentation equal "(equal ?ENTITY1 ?ENTITY2) is true just in case
?ENTITY1 is identical with ?ENTITY2.")

(=>
   (equal ?THING1 ?THING2)
   (forall (?ATTR)
      (<=>
         (property ?THING1 ?ATTR)
         (property ?THING2 ?ATTR))))

(=>
   (equal ?ATTR1 ?ATTR2)
   (forall (?THING)
      (<=>
         (property ?THING ?ATTR1)
         (property ?THING ?ATTR2))))

(=>
   (equal ?THING1 ?THING2)
   (forall (?CLASS)
      (<=>
         (instance ?THING1 ?CLASS)
         (instance ?THING2 ?CLASS))))

(=>
   (equal ?CLASS1 ?CLASS2)
   (forall (?THING)
      (<=>
         (instance ?THING ?CLASS1)
         (instance ?THING ?CLASS2))))

(=>
   (equal ?REL1 ?REL2)
   (forall (@ROW)
      (<=>
         (holds ?REL1 @ROW)
         (holds ?REL2 @ROW))))

(=>
   (equal (ListFn @ROW1) (ListFn @ROW2))
      (<=>
         (holds @ROW1)
         (holds @ROW2)))

(=>
   (equal ?LIST1 ?LIST2)
      (=>
         (and
            (equal ?LIST1 (ListFn @ROW1))
            (equal ?LIST2 (ListFn @ROW2)))
         (forall (?NUMBER)
            (equal (ListOrderFn (ListFn @ROW1) ?NUMBER) (ListOrderFn (ListFn @ROW2) ?NUMBER)))))

(instance range BinaryPredicate)
(instance range AsymmetricRelation)
(domain range 1 Function)
(domain range 2 SetOrClass)
(documentation range "Gives the range of a function.  In other words,
(&%range ?FUNCTION ?CLASS) means that all of the values assigned by
?FUNCTION are &%instances of ?CLASS.")

(=>
   (and
      (range ?FUNCTION ?CLASS)
      (equal (AssignmentFn ?FUNCTION @ROW) ?VALUE))
   (instance ?VALUE ?CLASS))

(=>
   (and
      (subrelation ?REL1 ?REL2)
      (range ?REL2 ?CLASS1))
   (range ?REL1 ?CLASS1))

(=>
   (and
      (range ?REL ?CLASS1)
      (range ?REL ?CLASS2))
   (or
      (subclass ?CLASS1 ?CLASS2)
      (subclass ?CLASS2 ?CLASS1)))

(instance rangeSubclass BinaryPredicate)
(instance rangeSubclass AsymmetricRelation)
(domain rangeSubclass 1 Function)
(domainSubclass rangeSubclass 2 SetOrClass)
(documentation rangeSubclass "(&%rangeSubclass ?FUNCTION ?CLASS) means that
all of the values assigned by ?FUNCTION are &%subclasses of ?CLASS.")

(=>
   (and
      (rangeSubclass ?FUNCTION ?CLASS)
      (equal (AssignmentFn ?FUNCTION @ROW) ?VALUE))
   (subclass ?VALUE ?CLASS))

(=>
   (and
      (subrelation ?REL1 ?REL2)
      (rangeSubclass ?REL2 ?CLASS1))
   (rangeSubclass ?REL1 ?CLASS1))

(=>
   (and
      (rangeSubclass ?REL ?CLASS1)
      (rangeSubclass ?REL ?CLASS2))
   (or
      (subclass ?CLASS1 ?CLASS2)
      (subclass ?CLASS2 ?CLASS1)))

(instance valence BinaryPredicate)
(instance valence AsymmetricRelation)
(instance valence SingleValuedRelation)
(domain valence 1 Relation)
(domain valence 2 PositiveInteger)
(documentation valence "Specifies the number of arguments that a
relation can take.  If a relation does not have a fixed number of
arguments, it does not have a valence and it is an instance of
&%VariableArityRelation.  For example, &%holds is a
&%VariableArityRelation.")

(instance documentation BinaryPredicate)
(instance documentation AsymmetricRelation)
(domain documentation 1 Entity)
(domain documentation 2 SymbolicString)
(documentation documentation "A relation between objects in the domain
of discourse and strings of natural language text.  The domain of
&%documentation is not constants (names), but the objects themselves.
This means that one does not quote the names when associating them with
their documentation.")

(instance disjoint BinaryPredicate)
(instance disjoint SymmetricRelation)
(domain disjoint 1 SetOrClass)
(domain disjoint 2 SetOrClass)
(documentation disjoint "&%Classes are &%disjoint only if they share no
instances, i.e. just in case the result of applying &%IntersectionFn to
them is empty.")

(<=>
   (disjoint ?CLASS1 ?CLASS2)
   (and
      (instance ?CLASS1 NonNullSet)
      (instance ?CLASS2 NonNullSet)
      (forall (?INST)
         (not
            (and
               (instance ?INST ?CLASS1)
               (instance ?INST ?CLASS2))))))

(instance disjointRelation Predicate)
(instance disjointRelation VariableArityRelation)
(relatedInternalConcept disjointRelation disjoint)
(domain disjointRelation 1 Relation)
(documentation disjointRelation "This predicate relates any number of &%Relations.  (&%disjointRelation @ROW) means that any two relations in @ROW have no tuples in common.  As a consequence, the intersection of all of the relations in @ROW is the null set.")

(=>
   (and
      (disjointRelation @ROW)
      (inList ?REL (ListFn @ROW)))
   (instance ?REL Relation))

(=>
   (and
      (disjointRelation @ROW)
      (inList ?REL1 (ListFn @ROW))
      (inList ?REL2 (ListFn @ROW))
      (valence ?REL1 ?NUMBER))
   (valence ?REL2 ?NUMBER))

(=>
   (and
      (domain ?REL1 ?NUMBER ?CLASS1)
      (domain ?REL2 ?NUMBER ?CLASS2)
      (disjoint ?CLASS1 ?CLASS2))
   (disjointRelation ?REL1 ?REL2))

(=>
   (and
      (domainSubclass ?REL1 ?NUMBER ?CLASS1)
      (domainSubclass ?REL2 ?NUMBER ?CLASS2)
      (disjoint ?CLASS1 ?CLASS2))
   (disjointRelation ?REL1 ?REL2))

(=>
   (and
      (range ?REL1 ?CLASS1)
      (range ?REL2 ?CLASS2)
      (disjoint ?CLASS1 ?CLASS2))
   (disjointRelation ?REL1 ?REL2))

(=>
   (and
      (rangeSubclass ?REL1 ?CLASS1)
      (rangeSubclass ?REL2 ?CLASS2)
      (disjoint ?CLASS1 ?CLASS2))
   (disjointRelation ?REL1 ?REL2))

(=>
   (and
      (disjointRelation @ROW1)
      (inList ?REL1 (ListFn @ROW1))
      (inList ?REL2 (ListFn @ROW1))
      (not (equal ?REL1 ?REL2))
      (holds ?REL1 @ROW2))
   (not (holds ?REL2 @ROW2)))

;; the domain 1 statement was added to contraryAttribute for the
;; Protege conversion 1-16-04

(instance contraryAttribute Predicate)
(instance contraryAttribute VariableArityRelation)
(domain contraryAttribute 1 Attribute)
(domain contraryAttribute 2 Attribute)
;; domain 2 added 1/16/04 for Protege alignment
(documentation contraryAttribute "A &%contraryAttribute is a set of &%Attributes
such that something can not simultaneously have more than one of these &%Attributes.
For example, (&%contraryAttribute &%Pliable &%Rigid) means that nothing can be both
&%Pliable and &%Rigid.")

(=>
   (contraryAttribute @ROW)
   (=>
      (inList ?ELEMENT (ListFn @ROW))
      (instance ?ELEMENT Attribute)))

(=>
   (contraryAttribute @ROW)
   (forall (?ATTR1 ?ATTR2)
      (=>
         (and
            (equal ?ATTR1 (ListOrderFn (ListFn @ROW) ?NUMBER1))
            (equal ?ATTR2 (ListOrderFn (ListFn @ROW) ?NUMBER2))
            (not (equal ?NUMBER1 ?NUMBER2)))
         (=>
            (property ?OBJ ?ATTR1)
            (not (property ?OBJ ?ATTR2))))))

(instance exhaustiveAttribute Predicate)
(instance exhaustiveAttribute VariableArityRelation)
(domainSubclass exhaustiveAttribute 1 Attribute)
(documentation exhaustiveAttribute "This predicate relates a &%Class to a
set of &%Attributes, and it means that the elements of this set exhaust the
instances of the &%Class.  For example, (&%exhaustiveAttribute &%PhysicalState
&%Solid &%Liquid &%Gas) means that there are only three instances of the class
&%PhysicalState, viz. &%Solid, &%Liquid, and &%Gas.")

(=>
   (exhaustiveAttribute ?CLASS @ROW)
   (=>
      (inList ?ATTR (ListFn @ROW))
      (instance ?ATTR Attribute)))

(=>
   (exhaustiveAttribute ?CLASS @ROW)
   (forall (?OBJ)
      (=>
         (instance ?ATTR1 ?CLASS)
         (exists (?ATTR2)
            (and
               (inList ?ATTR2 (ListFn @ROW))
               (equal ?ATTR1 ?ATTR2))))))

(instance exhaustiveDecomposition Predicate)
(instance exhaustiveDecomposition VariableArityRelation)
(domain exhaustiveDecomposition 1 Class)
(relatedInternalConcept exhaustiveDecomposition partition)
(documentation exhaustiveDecomposition "An &%exhaustiveDecomposition of a
&%Class C is a set of subclasses of C such that every subclass of C either
is an element of the set or is a subclass of an element of the set.  Note:
this does not necessarily mean that the elements of the set are disjoint
(see &%partition - a &%partition is a disjoint exhaustive decomposition.)")

(=>
   (exhaustiveDecomposition @ROW)
   (=>
      (inList ?ELEMENT (ListFn @ROW))
      (instance ?ELEMENT Class)))

(=>
   (exhaustiveDecomposition ?CLASS @ROW)
   (forall (?OBJ)
      (=>
         (instance ?OBJ ?CLASS)
         (exists (?ITEM)
            (and
               (inList ?ITEM (ListFn @ROW))
               (instance ?OBJ ?ITEM))))))

(instance disjointDecomposition Predicate)
(instance disjointDecomposition VariableArityRelation)
(domain disjointDecomposition 1 Class)
(relatedInternalConcept disjointDecomposition exhaustiveDecomposition)
(relatedInternalConcept disjointDecomposition disjoint)
(documentation disjointDecomposition "A &%disjointDecomposition of a &%Class
C is a set of subclasses of C that are mutually &%disjoint.")

(=>
   (disjointDecomposition @ROW)
   (=>
      (inList ?ELEMENT (ListFn @ROW))
      (instance ?ELEMENT Class)))

(=>
   (disjointDecomposition ?CLASS @ROW)
   (forall (?ITEM)
      (=>
         (inList ?ITEM (ListFn @ROW))
         (subclass ?ITEM ?CLASS))))

(=>
   (disjointDecomposition ?CLASS @ROW)
   (forall (?ITEM1 ?ITEM2)
      (=>
         (and
            (inList ?ITEM1 (ListFn @ROW))
            (inList ?ITEM2 (ListFn @ROW))
            (not
               (equal ?ITEM1 ?ITEM2)))
         (disjoint ?ITEM1 ?ITEM2))))

(instance partition Predicate)
(instance partition VariableArityRelation)
(domain partition 1 Class)
(documentation partition "A &%partition of a class C is a set of
mutually &%disjoint classes (a subclass partition) which covers C.
Every instance of C is an instance of exactly one of the subclasses
in the partition.")

(<=>
   (partition @ROW)
   (and
      (exhaustiveDecomposition @ROW)
      (disjointDecomposition @ROW)))

(instance relatedInternalConcept BinaryPredicate)
(instance relatedInternalConcept EquivalenceRelation)
(domain relatedInternalConcept 1 Entity)
(domain relatedInternalConcept 2 Entity)
(documentation relatedInternalConcept "Means that the two arguments are
related concepts within the SUMO, i.e. there is a significant similarity
of meaning between them.  To indicate a meaning relation between a SUMO
concept and a concept from another source, use the Predicate
&%relatedExternalConcept.")

(instance relatedExternalConcept TernaryPredicate)
(domain relatedExternalConcept 1 SymbolicString)
(domain relatedExternalConcept 2 Entity)
(domain relatedExternalConcept 3 Language)
(relatedInternalConcept relatedExternalConcept relatedInternalConcept)
(documentation relatedExternalConcept "Used to signify a three-place
relation between a concept in an external knowledge source, a concept
in the SUMO, and the name of the other knowledge source.")

(subrelation synonymousExternalConcept relatedExternalConcept)
(disjointRelation synonymousExternalConcept subsumedExternalConcept subsumingExternalConcept)
(documentation synonymousExternalConcept "(&%synonymousExternalConcept
?STRING ?THING ?LANGUAGE) means that the SUMO concept ?THING has the
same meaning as ?STRING in ?LANGUAGE.")

(subrelation subsumingExternalConcept relatedExternalConcept)
(documentation subsumingExternalConcept "(&%subsumingExternalConcept
?STRING ?THING ?LANGUAGE) means that the SUMO concept ?THING subsumes
the meaning of ?STRING in ?LANGUAGE, i.e. the concept ?THING is broader
in meaning than ?STRING.")

(subrelation subsumedExternalConcept relatedExternalConcept)
(documentation subsumedExternalConcept "(&%subsumedExternalConcept
?STRING ?THING ?LANGUAGE) means that the SUMO concept ?THING is subsumed
by the meaning of ?STRING in ?LANGUAGE, i.e. the concept ?THING is narrower
in meaning than ?STRING.")

(instance subAttribute BinaryPredicate)
(instance subAttribute PartialOrderingRelation)
(domain subAttribute 1 Attribute)
(domain subAttribute 2 Attribute)
(disjointRelation subAttribute successorAttribute)
(documentation subAttribute "Means that the second argument can be
ascribed to everything which has the first argument ascribed to it.")

(=>
   (subAttribute ?ATTR1 ?ATTR2)
   (forall (?OBJ)
      (=>
         (property ?OBJ ?ATTR1)
         (property ?OBJ ?ATTR2))))

(=>
   (and
      (subAttribute ?ATTR1 ?ATTR2)
      (instance ?ATTR2 ?CLASS))
   (instance ?ATTR1 ?CLASS))

(instance successorAttribute BinaryPredicate)
(instance successorAttribute AsymmetricRelation)
(domain successorAttribute 1 Attribute)
(domain successorAttribute 2 Attribute)
(documentation successorAttribute "(&%successorAttribute ?ATTR1 ?ATTR2)
means that ?ATTR2 is the &%Attribute that comes immediately after ?ATTR1
on the scale that they share.")

(=>
   (and
      (successorAttribute ?ATTR1 ?ATTR2)
      (holdsDuring ?TIME1 (property ?ENTITY ?ATTR2)))
   (exists (?TIME2)
      (and
         (temporalPart ?TIME2 (PastFn ?TIME1))
         (holdsDuring ?TIME2 (property ?ENTITY ?ATTR1)))))

(instance successorAttributeClosure BinaryPredicate)
(instance successorAttributeClosure TransitiveRelation)
(instance successorAttributeClosure IrreflexiveRelation)
(domain successorAttributeClosure 1 Attribute)
(domain successorAttributeClosure 2 Attribute)
(relatedInternalConcept successorAttributeClosure successorAttribute)
(documentation successorAttributeClosure "The transitive closure of
&%successorAttribute.  (&%successorAttributeClosure ?ATTR1 ?ATTR2) means
that there is a chain of &%successorAttribute assertions connecting
?ATTR1 and ?ATTR2.")

(=>
   (successorAttribute ?ATTR1 ?ATTR2)
   (successorAttributeClosure ?ATTR1 ?ATTR2))

(instance and VariableArityRelation)
(instance and LogicalOperator)
(domain and 1 Formula)
(domain and 2 Formula)
(documentation and "The truth-functional connective of conjunction.")

(instance or VariableArityRelation)
(instance or LogicalOperator)
(domain or 1 Formula)
(domain or 2 Formula)
(documentation or "The truth-functional connective of disjunction.")

(instance => BinaryPredicate)
(instance => LogicalOperator)
(domain => 1 Formula)
(domain => 2 Formula)
(documentation => "The truth-functional connective of implication.")

(instance <=> BinaryPredicate)
(instance <=> LogicalOperator)
(domain <=> 1 Formula)
(domain <=> 2 Formula)
(documentation <=> "The truth-functional connective of bi-implication.")

(instance not UnaryRelation)
(instance not LogicalOperator)
(domain not 1 Formula)
(domain not 2 Formula)
(documentation not "The truth-functional connective of negation.    This relation definition was modified by P. Cassidy November 2003 to specify that 'not' is an instance of UnaryRelation")

(instance forall BinaryPredicate)
(instance forall LogicalOperator)
(domain forall 1 List)
(domain forall 2 Formula)
(documentation forall "The universal quantifier of predicate logic.")

(instance exists BinaryPredicate)
(instance exists LogicalOperator)
(domain exists 1 List)
(domain exists 2 Formula)
(documentation exists "The existential quantifier of predicate logic.")

(instance entails BinaryPredicate)
(instance entails LogicalOperator)
(domain entails 1 Formula)
(domain entails 2 Formula)
(documentation entails "The operator of logical entailment.  (&%entails
?FORMULA1 ?FORMULA2) means that ?FORMULA2 can be derived from ?FORMULA1
by means of the proof theory of SUO-KIF.")

;; The following axiom is commented out, because it is rejected by the
;; inference engine's parser.

;;(=>
;;   (entails ?FORMULA1 ?FORMULA2)
;;   (=> ?FORMULA1 ?FORMULA2))

(instance AssignmentFn Function)
(instance AssignmentFn VariableArityRelation)
(domain AssignmentFn 1 Function)
(range AssignmentFn Entity)
(documentation AssignmentFn "If F is a function with a value for the
objects denoted by N1,..., NK, then the term (AssignmentFn F N1 ... NK)
denotes the value of applying F to the objects denoted by N1,..., NK.
Otherwise, the value is undefined.")

;; SMINK NOTE:  the relation holdsInContext was added below in module
;; EXTENSIONS for referring to contexts

(instance holds Predicate)
(instance holds VariableArityRelation)
(domain holds 1 Entity)
(documentation holds "(holds P N1 ... NK) is true just in case
the tuple of objects denoted by N1,..., NK is an element of
the &%Relation P.")

(=>
   (instance ?REL Function)
   (<=>
      (equal (AssignmentFn ?REL @ROW) ?INST)
      (holds ?REL @ROW ?INST)))

(instance PowerSetFn UnaryFunction)
(instance PowerSetFn TotalValuedRelation)
(domain PowerSetFn 1 SetOrClass)
(rangeSubclass PowerSetFn SetOrClass)
(documentation PowerSetFn "(&%PowerSetFn ?CLASS) maps the &%SetOrClass
?CLASS to the &%SetOrClass of all &%subclasses of ?CLASS.")

;; The following two relations, minimumCardinality and maximumCardinality,
;;  were suggested by Ian Niles 17-Jan-2004 to enable the
;;  SKIF representation of minimum and maximum cardinalities
;;  as used in Protege and other frame-based representations.
;; Note that being an instance of TotalValuedRelation means that
;; the minimum cardinality is always 1, and being an instance
;; of SingleValuedRelation means that the maximum cardinality is
;; always 1. Those restrictions have not yet been related to
;; the relations below.

(instance minCardinalityAtClasses QuaternaryPredicate)
(domainSubclass minCardinalityAtClasses 1 Entity)
(domain minCardinalityAtClasses 2 BinaryRelation)
(domain minCardinalityAtClasses 3 NonnegativeInteger)
(domainSubclass minCardinalityAtClasses 4 Entity)
(documentation minCardinalityAtClasses "(minCardinalityAtClasses ?CLASS1 ?REL ?NUMBER ?CLASS2) means that, for any given instance ?INST1 of ?CLASS1 ?REL, there will be at least ?NUMBER instances of ?CLASS2 related to ?INST1 by the relation ?REL.")

(=>
   (and
      (minCardinalityAtClasses ?CLASS1 ?REL ?NUMBER1 ?CLASS2)
      (instance ?ARG1 ?CLASS1)
      (instance ?ARG2 ?CLASS2)
      (equal (CardinalityFn (KappaFn ?ARG2 (?REL ?ARG1 ?ARG2))) ?NUMBER2))
   (greaterThanOrEqualTo ?NUMBER2 ?NUMBER1))

(instance minimumCardinality BinaryPredicate)
(domain minimumCardinality 1 BinaryRelation)
(domain minimumCardinality 2 NonnegativeInteger)
(documentation minimumCardinality "(minimumCardinality ?REL ?NUMBER) means that, for any given assignment of a first argument to ?REL, there will be at least ?NUMBER assignments to the second argument of ?REL.")

(=>
   (and
      (minimumCardinality ?REL ?NUMBER1)
      (equal (CardinalityFn (KappaFn ?ARG2 (?REL ?ARG1 ?ARG2))) ?NUMBER2))
   (greaterThanOrEqualTo ?NUMBER2 ?NUMBER1))

(instance maximumCardinality BinaryPredicate)
(domain maximumCardinality 1 BinaryRelation)
(domain maximumCardinality 2 NonnegativeInteger)
(documentation maximumCardinality "(maximumCardinality ?REL ?NUMBER) means that, for any given
assignment of a first argument to ?REL, there will be at most ?NUMBER assignments to the second
argument of ?REL.")

(=>
   (and
      (maximumCardinality ?REL ?NUMBER1)
      (equal (CardinalityFn (KappaFn ?ARG2 (?REL ?ARG1 ?ARG2))) ?NUMBER2))
   (lessThanOrEqualTo ?NUMBER2 ?NUMBER1))


;; END FILE

;; <module>BASE_ONTOLOGY</module>
;; BEGIN FILE


;;;;;;;;;;;;;;;;;;;;;;;
;;   BASE ONTOLOGY   ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; INCLUDES 'STRUCTURAL ONTOLOGY'

;; The following hierarchy incorporates content from Sowa, Russell & Norvig,
;; and the top-level ontology from ITBM-CNR.

(partition Entity Physical Abstract)
(documentation Entity "The universal class of individuals.  This is the root node of the ontology.")

(instance ?THING Entity)

(exists (?THING) (instance ?THING Entity))

(<=>
   (instance ?CLASS Class)
   (subclass ?CLASS Entity))

(subclass Physical Entity)
(partition Physical Object Process)
(documentation Physical "An entity that has a location in space-time.
Note that locations are themselves understood to have a location in
space-time.")

(<=>
   (instance ?PHYS Physical)
   (exists (?LOC ?TIME)
       (and
          (located ?PHYS ?LOC)
          (time ?PHYS ?TIME))))

(subclass Object Physical)
(documentation Object "Corresponds roughly to the class of ordinary
objects.  Examples include normal physical objects, geographical regions,
and locations of &%Processes, the complement of &%Objects in the &%Physical
class.  In a 4D ontology, an &%Object is something whose spatiotemporal
extent is thought of as dividing into spatial parts roughly parallel to the
time-axis.")

(subclass SelfConnectedObject Object)
(documentation SelfConnectedObject "A &%SelfConnectedObject is any
&%Object that does not consist of two or more disconnected parts.")

(instance FrontFn SpatialRelation)
(instance FrontFn PartialValuedRelation)
(instance FrontFn UnaryFunction)
(instance FrontFn AsymmetricRelation)
(instance FrontFn IrreflexiveRelation)
(domain FrontFn 1 SelfConnectedObject)
(range FrontFn SelfConnectedObject)
(documentation FrontFn "A &%Function that maps an &%Object to the side
that generally receives the most attention or that typically faces the
direction in which the &%Object moves.  Note that this is a partial
function, since some &%Objects do not have sides, e.g. apples and
spheres.  Note too that the &%range of this &%Function is indefinite in
much the way that &%ImmediateFutureFn and &%ImmediatePastFn are indefinite.
Although this indefiniteness is undesirable from a theoretical standpoint,
it does not have significant practical implications, since there is
widespread intersubjective agreement about the most common cases.")

(=>
   (instance ?OBJ SelfConnectedObject)
   (side (FrontFn ?OBJ) ?OBJ))

(instance BackFn SpatialRelation)
(instance BackFn PartialValuedRelation)
(instance BackFn UnaryFunction)
(instance BackFn AsymmetricRelation)
(instance BackFn IrreflexiveRelation)
(domain BackFn 1 SelfConnectedObject)
(range BackFn SelfConnectedObject)
(documentation BackFn "A &%Function that maps an &%Object to the side
that is opposite the &%FrontFn of the &%Object.  Note that this is a
partial function, since some &%Objects do not have sides, e.g. apples
and spheres.  Note too that the &%range of this &%Function is indefinite in
much the way that &%ImmediateFutureFn and &%ImmediatePastFn are indefinite.
Although this indefiniteness is undesirable from a theoretical standpoint,
it does not have significant practical implications, since there is
widespread intersubjective agreement about the most common cases.")

(=>
   (instance ?OBJ SelfConnectedObject)
   (side (BackFn ?OBJ) ?OBJ))

(instance part SpatialRelation)
(instance part PartialOrderingRelation)
(domain part 1 Object)
(domain part 2 Object)
(documentation part "The basic mereological relation.  All other
mereological relations are defined in terms of this one.
(&%part ?PART ?WHOLE) simply means that the &%Object ?PART is part
of the &%Object ?WHOLE.  Note that, since &%part is a
&%ReflexiveRelation, every &%Object is a part of itself.")

(instance properPart AsymmetricRelation)
(instance properPart TransitiveRelation)
(subrelation properPart part)
(documentation properPart "(&%properPart ?OBJ1 ?OBJ2) means that
?OBJ1 is a part of ?OBJ2 other than ?OBJ2 itself. This is a
&%TransitiveRelation and &%AsymmetricRelation (hence an
&%IrreflexiveRelation).")

(<=>
   (properPart ?OBJ1 ?OBJ2)
      (and
        (part ?OBJ1 ?OBJ2)
        (not
           (part ?OBJ2 ?OBJ1))))

(subrelation piece part)
(domain piece 1 Substance)
(domain piece 2 Substance)
(documentation piece "A specialized common sense notion of part for
arbitrary parts of &%Substances.  Quasi-synonyms are: chunk, hunk, bit,
etc.  Compare &%component, the other subrelation of &%part.")

(=>
   (piece ?SUBSTANCE1 ?SUBSTANCE2)
   (forall (?CLASS)
      (=>
         (instance ?SUBSTANCE1 ?CLASS)
         (instance ?SUBSTANCE2 ?CLASS))))

(subrelation component part)
(domain component 1 CorpuscularObject)
(domain component 2 CorpuscularObject)
(documentation component "A specialized common sense notion of part
for heterogeneous parts of complexes. (&%component ?COMPONENT ?WHOLE)
means that ?COMPONENT is a component of ?WHOLE. Examples of component
include the doors and walls of a house, the states or provinces of a
country, or the limbs and organs of an animal.  Compare &%piece, which
is also a subrelation of &%part.")

(instance material BinaryPredicate)
(domainSubclass material 1 Substance)
(domain material 2 CorpuscularObject)
(documentation material "(&%material ?SUBSTANCE ?OBJECT) means that
?OBJECT is structurally made up in part of ?SUBSTANCE. This relation
encompasses the concepts of 'composed of', 'made of', and 'formed of'.
For example, plastic is a &%material of my computer monitor.  Compare
&%part and its subrelations, viz &%component and &%piece.")

(subrelation contains partlyLocated)
(instance contains SpatialRelation)
(instance contains AsymmetricRelation)
(disjointRelation contains part)
(domain contains 1 SelfConnectedObject)
(domain contains 2 Object)
(documentation contains "The relation of spatial containment for two
separable objects.  When the two objects are not separable (e.g. an
automobile and one of its seats), the relation of &%part should be used.
(&%contains ?OBJ1 ?OBJ2) means that the &%SelfConnectedObject ?OBJ1 has
a space (i.e. a &%Hole) which is at least partially filled by ?OBJ2.")

(<=>
   (contains ?OBJ1 ?OBJ2)
   (exists (?HOLE)
      (and
         (hole ?HOLE ?OBJ1)
         (properlyFills ?OBJ2 ?HOLE))))

(subclass Substance SelfConnectedObject)
(documentation Substance "An &%Object in which every part is similar to
every other in every relevant respect.  More precisely, something is a
&%Substance when it has only arbitrary pieces as parts - any parts have
properties which are similar to those of the whole.  Note that a &%Substance
may nonetheless have physical properties that vary.  For example, the
temperature, chemical constitution, density, etc. may change from one part
to another.  An example would be a body of water.")

(=>
   (and
      (subclass ?OBJECTTYPE Substance)
      (instance ?OBJECT ?OBJECTTYPE)
      (part ?PART ?OBJECT))
   (instance ?PART ?OBJECTTYPE))

(=>
   (and
      (instance ?OBJ Substance)
      (attribute ?OBJ ?ATTR)
      (part ?PART ?OBJ))
   (attribute ?PART ?ATTR))

(subclass PureSubstance Substance)
(partition PureSubstance CompoundSubstance ElementalSubstance)
(documentation PureSubstance "The &%Class of &%Substances with constant
composition.  A &%PureSubstance can be either an element (&%ElementalSubstance)
or a compound of elements (&%CompoundSubstance).  Examples: Table salt
(sodium chloride, NaCl), sugar (sucrose, C_{12}H_{22}O_{11}), water (H_2O),
iron (Fe), copper (Cu), and oxygen (O_2).")

(subclass ElementalSubstance PureSubstance)
(documentation ElementalSubstance "The &%Class of &%PureSubstances that
cannot be separated into two or more &%Substances by ordinary chemical
(or physical) means. This excludes nuclear reactions. &%ElementalSubstances
are composed of only one kind of atom. Examples: Iron (Fe), copper (Cu),
and oxygen (O_2).  &%ElementalSubstances are the simplest
&%PureSubstances.")

(subclass Metal ElementalSubstance)
(documentation Metal "A &%Metal is an &%ElementalSubstance that conducts heat
and electricity, is shiny and reflects many colors of light, and can be hammered
into sheets or drawn into wire.  About 80% of the known chemical elements
(&%ElementalSubstances) are metals.")

(subclass Atom ElementalSubstance)
(documentation Atom "An extremely small unit of matter that retains its
identity in Chemical reactions.  It consists of an &%AtomicNucleus and
&%Electrons surrounding the &%AtomicNucleus.")

(=>
   (instance ?ATOM Atom)
   (exists (?PROTON ?ELECTRON)
      (and
         (component ?PROTON ?ATOM)
         (component ?ELECTRON ?ATOM)
         (instance ?PROTON Proton)
         (instance ?ELECTRON Electron))))

(=>
   (instance ?ATOM Atom)
   (forall (?NUCLEUS1 ?NUCLEUS2)
      (=>
         (and
            (component ?NUCLEUS1 ?ATOM)
            (component ?NUCLEUS2 ?ATOM)
            (instance ?NUCLEUS1 AtomicNucleus)
            (instance ?NUCLEUS2 AtomicNucleus))
            (equal ?NUCLEUS1 ?NUCLEUS2))))

(subclass SubatomicParticle ElementalSubstance)
(documentation SubatomicParticle "The class of &%ElementalSubstances that
are smaller than &%Atoms and compose &%Atoms.")

(=>
   (instance ?PARTICLE SubatomicParticle)
   (exists (?ATOM)
      (and
         (instance ?ATOM Atom)
         (part ?PARTICLE ?ATOM))))

(subclass AtomicNucleus SubatomicParticle)
(documentation AtomicNucleus "The core of the &%Atom.  It is composed of
&%Protons and &%Neutrons.")

(=>
   (instance ?NUCLEUS AtomicNucleus)
   (exists (?NEUTRON ?PROTON)
      (and
         (component ?NEUTRON ?NUCLEUS)
         (component ?PROTON ?NUCLEUS)
         (instance ?NEUTRON Neutron)
         (instance ?PROTON Proton))))

(subclass Electron SubatomicParticle)
(documentation Electron "&%SubatomicParticles that surround the
&%AtomicNucleus.  They have a negative charge.")

(subclass Proton SubatomicParticle)
(documentation Proton "Components of the &%AtomicNucleus.  They have a
positive charge.")

(subclass Neutron SubatomicParticle)
(documentation Neutron "Components of the &%AtomicNucleus.  They have no
charge.")

(subclass CompoundSubstance PureSubstance)
(documentation CompoundSubstance "The &%Class of &%Substances that contain
two or more elements (&%ElementalSubstances), in definite proportion by weight.
The composition of a pure compound will be invariant, regardless of the method
of preparation. Compounds are composed of more than one kind of atom (element).
The term molecule is often used for the smallest unit of a compound that still
retains all of the properties of the compound.  Examples: Table salt (sodium
chloride, NaCl), sugar (sucrose, C_{12}H_{22}O_{11}), and water (H_2O). ")

(subclass Mixture Substance)
(documentation Mixture "A &%Mixture is two or more &%PureSubstances,
combined in varying proportions - each retaining its own specific properties.
The components of a &%Mixture can be separated by physical means, i.e. without
the making and breaking of chemical bonds. Examples: Air, table salt thoroughly
dissolved in water, milk, wood, and concrete. ")

(=>
   (instance ?MIXTURE Mixture)
   (exists (?PURE1 ?PURE2)
      (and
         (instance ?PURE1 PureSubstance)
         (instance ?PURE2 PureSubstance)
         (not (equal ?PURE1 ?PURE2))
         (part ?PURE1 ?MIXTURE)
         (part ?PURE2 ?MIXTURE))))

(=>
   (and
      (instance ?MIXTURE Mixture)
      (part ?SUBSTANCE ?MIXTURE)
      (not (instance ?SUBSTANCE Mixture)))
   (instance ?SUBSTANCE PureSubstance))

(subclass Solution Mixture)
(documentation Solution "A liquid mixture. The most abundant component in
a solution is called the solvent. Other components are called solutes.
A solution, though homogeneous, may nonetheless have variable composition.
Any amount of salt, up to a maximum limit, can be dissolved in a given
amount of water.")

(subclass CorpuscularObject SelfConnectedObject)
(disjoint CorpuscularObject Substance)
(documentation CorpuscularObject "A &%SelfConnectedObject whose parts have
properties that are not shared by the whole.")

(=>
   (instance ?OBJ CorpuscularObject)
   (exists (?SUBSTANCE1 ?SUBSTANCE2)
      (and
         (subclass ?SUBSTANCE1 Substance)
         (subclass ?SUBSTANCE2 Substance)
         (material ?SUBSTANCE1 ?OBJ)
         (material ?SUBSTANCE2 ?OBJ)
         (not (equal ?SUBSTANCE1 ?SUBSTANCE2)))))

(subclass Region Object)
(subclass Region Location)
(partition Region GeographicArea SpaceRegion)
(documentation Region "A topographic location.  &%Regions encompass
surfaces of &%Objects, imaginary places, and &%GeographicAreas.  Note
that a &%Region is the only kind of &%Object which can be located at
itself.  Note too that &%Region is not a subclass of &%SelfConnectedObject,
because some &%Regions, e.g. archipelagos, have &%parts which are not
&%connected with one another.")

(=>
   (instance ?REGION Region)
   (exists (?PHYS)
      (located ?PHYS ?REGION)))

(subclass Collection Object)
(disjoint Collection SelfConnectedObject)
(documentation Collection "Collections have &%members like &%Classes, but,
unlike &%Classes, they have a position in space-time and &%members can be
added and subtracted without thereby changing the identity of the
&%Collection.  Some examples are toolkits, football teams, and flocks
of sheep.")

(=>
   (instance ?COLL Collection)
   (exists (?OBJ)
          (member ?OBJ ?COLL)))

(subrelation member part)
(instance member AsymmetricRelation)
(instance member IntransitiveRelation)
(domain member 1 SelfConnectedObject)
(domain member 2 Collection)
(relatedInternalConcept member instance)
(relatedInternalConcept member element)
(documentation member "A specialized common sense notion of part for
uniform parts of &%Collections.  For example, each sheep in a flock of
sheep would have the relationship of member to the flock.")

(instance subCollection BinaryPredicate)
(instance subCollection PartialOrderingRelation)
(domain subCollection 1 Collection)
(domain subCollection 2 Collection)
(documentation subCollection "(&%subCollection ?COLL1 ?COLL2) means that
the &%Collection ?COLL1 is a proper part of the &%Collection ?COLL2.")

(<=>
   (subCollection ?COLL1 ?COLL2)
   (and
      (instance ?COLL1 Collection)
      (instance ?COLL2 Collection)
      (forall (?MEMBER)
         (=>
            (member ?MEMBER ?COLL1)
            (member ?MEMBER ?COLL2)))))

(subclass ContentBearingObject CorpuscularObject)
(relatedInternalConcept ContentBearingObject containsInformation)
(documentation ContentBearingObject "Any &%SelfConnectedObject that expresses
information.")

(subclass SymbolicString ContentBearingObject)
(:hasRestrictedVal SymbolicString hasAbstractContent AbstractString)
(documentation SymbolicString "The &%Class of alphanumeric sequences.")

(subclass Character SymbolicString)
(documentation Character "An element of an alphabet, a set of numerals, etc.
Note that a &%Character may or may not be part of a &%Language.  &%Character
is a subclass of &%SymbolicString, because every instance of &%Character is
an alphanumeric sequence consisting of a single element.")

(=>
    (instance ?STRING SymbolicString)
    (exists (?PART)
        (and
            (part ?PART ?STRING)
            (instance ?PART Character))))

(instance containsInformation BinaryPredicate)
(instance containsInformation AsymmetricRelation)
(subrelation containsInformation represents)
(domain containsInformation 1 ContentBearingObject)
(domain containsInformation 2 Proposition)
(documentation containsInformation "A subrelation of &%represents.  This
predicate relates a &%ContentBearingObject to the &%Proposition that is`
expressed by the &%ContentBearingObject. Examples include the relationships
between a physical novel and its story and between a printed score and its
musical content.")

(subclass Icon ContentBearingObject)
(documentation Icon "This is the subclass of &%ContentBearingObjects
which are not part of a &%Language and which have some sort of similarity
with the &%Objects that they represent.  This &%Class would include symbolic
roadway signs, representational art works, photographs, etc.")

(subclass MotionPicture Text)
(documentation MotionPicture "A &%ContentBearingObject which depicts motion
(and which may have an audio or text component as well).  This &%Class covers
films, videos, etc.")

(subclass LinguisticExpression ContentBearingObject)
(disjoint LinguisticExpression Icon)
(documentation LinguisticExpression "This is the subclass of
&%ContentBearingObjects which are language-related.  Note that this &%Class
encompasses both &%Language and the the elements of &%Languages,
e.g. &%Words.")

(subclass Language LinguisticExpression)
(disjointDecomposition Language AnimalLanguage HumanLanguage ComputerLanguage)
(documentation Language "A system of signs for expressing thought.  The
system can be either natural or artificial, i.e. something that emerges
gradually as a cultural artifact or something that is intentionally created
by a person or group of people.")

(subclass AnimalLanguage Language)
(documentation AnimalLanguage "The &%subclass of &%Languages used by
&%Animals other than &%Humans.")

(=>
   (and
      (instance ?LANG AnimalLanguage)
      (agent ?PROC ?AGENT)
      (instrument ?PROC ?LANG))
   (and
      (instance ?AGENT Animal)
      (not (instance ?AGENT Human))))

(subclass ArtificialLanguage Language)
(documentation ArtificialLanguage "The &%subclass of &%Languages that are
designed by &%Humans.")

(subclass ComputerLanguage ArtificialLanguage)
(documentation ComputerLanguage "The class of &%Languages designed for
and interpreted by a computer.")

(=>
   (and
      (instance ?LANG ComputerLanguage)
      (agent ?PROC ?AGENT)
      (instrument ?PROC ?LANG))
   (instance ?AGENT Machine))

(subclass HumanLanguage Language)
(partition HumanLanguage NaturalLanguage ConstructedLanguage)
(partition HumanLanguage SpokenHumanLanguage ManualHumanLanguage)
(documentation HumanLanguage "The &%subclass of &%Languages used by
&%Humans.")

(=>
   (and
      (instance ?LANG HumanLanguage)
      (agent ?PROC ?AGENT)
      (instrument ?PROC ?LANG))
   (instance ?AGENT Human))

(subclass ConstructedLanguage HumanLanguage)
(subclass ConstructedLanguage ArtificialLanguage)
(documentation ConstructedLanguage "An &%ConstructedLanguage is a
&%HumanLanguage that did not evolve spontaneously within a language
community, but rather had its core grammar and vocabulary invented by
one or more language experts, often with an aim to produce a more
grammatically regular language than any language that has evolved
naturally.  This &%Class includes languages like Esperanto that were
created to facilitate international communication")

(=>
   (instance ?LANG ConstructedLanguage)
   (exists (?PLAN)
      (and
         (instance ?PLAN Planning)
         (result ?PLAN ?LANG))))

(subclass NaturalLanguage HumanLanguage)
(documentation NaturalLanguage "The &%subclass of &%HumanLanguages which
are not designed and which evolve from generation to generation.  This
&%Class includes all of the national languages, e.g. English, Spanish,
Japanese, etc.  Note that this class includes dialects of natural
languages.")

(subclass ManualHumanLanguage HumanLanguage)
(documentation ManualHumanLanguage "A &%ManualHumanLanguage is a
&%HumanLanguage which has as its medium gestures and movement, such
as the shape, position, and movement of the hands.")

(subclass SpokenHumanLanguage HumanLanguage)
(documentation SpokenHumanLanguage "A &%SpokenHumanLanguage is a
&%HumanLanguage which has as its medium the human voice. It can also
berepresented visually through writing, although not all
&%SpokenHumanLanguages have a codified written form.")

(subclass Word LinguisticExpression)
(partition Word Noun Verb Adjective Adverb Particle)
(documentation Word "A term of a &%Language that represents a concept.")

(subclass Formula Sentence)
(documentation Formula "A syntactically well-formed formula in the
SUO-KIF knowledge representation language.")

;; The following ground facts incorporate the 'Agent' hierarchy from the
;; corresponding ontology on the Ontolingua server.  It also includes
;; predicates defined in the ITBM-CNR ontology "Actors".

(subclass Agent Object)
(documentation Agent "Something or someone that can act on its own and
produce changes in the world.")

(<=>
   (instance ?AGENT Agent)
   (exists (?PROC)
       (agent ?PROC ?AGENT)))

(subclass SentientAgent Agent)
(documentation SentientAgent "An &%Agent that has rights but may or may
not have responsibilities and the ability to reason.  If the latter are
present, then the &%Agent is also an instance of &%CognitiveAgent.
Domesticated animals are an example of &%SentientAgents that are not
also &%CognitiveAgents.")

(subclass CognitiveAgent SentientAgent)
(documentation CognitiveAgent "A &%SentientAgent with responsibilities
and the ability to reason, deliberate, make plans, etc.  This is
essentially the legal/ethical notion of a person.  Note that, although
&%Human is a subclass of &%CognitiveAgent, there may be instances of
&%CognitiveAgent which are not also instances of &%Human.  For example,
chimpanzees, gorillas, dolphins, whales, and some extraterrestrials
(if they exist) may be &%CognitiveAgents.")

(instance leader BinaryPredicate)
(instance leader AsymmetricRelation)
(instance leader SingleValuedRelation)
(domain leader 1 CognitiveAgent)
(domain leader 2 Agent)
(documentation leader "(&%leader ?ORGANIZATION ?PERSON)
means that the leader of ?ORGANIZATION is ?PERSON.")

(=>
   (and
      (instance ?AREA GeopoliticalArea)
	(leader (GovernmentFn ?AREA) ?PERSON))
   (leader ?AREA ?PERSON))

(=>
   (and
      (instance ?AREA GeopoliticalArea)
	(leader ?AREA ?PERSON))
(leader (GovernmentFn ?AREA) ?PERSON))

(subclass Process Physical)
(documentation Process "Intuitively, the class of things that happen
and have temporal parts or stages.  Examples include extended events
like a football match or a race, actions like &%Pursuing and &%Reading,
and biological processes. The formal definition is: anything that lasts
for a time but is not an &%Object.  Note that a &%Process may have
participants 'inside' it which are &%Objects, such as the players
in a football match.  In a 4D ontology, a &%Process is something whose
spatiotemporal extent is thought of as dividing into temporal stages
roughly perpendicular to the time-axis.")

(subclass DualObjectProcess Process)
(documentation DualObjectProcess "Any &%Process that requires two,
nonidentical &%patients.")

(=>
   (instance ?PROCESS DualObjectProcess)
   (exists (?OBJ1 ?OBJ2)
      (and
         (patient ?PROCESS ?OBJ1)
         (patient ?PROCESS ?OBJ2)
         (not (equal ?OBJ1 ?OBJ2)))))

(subclass Abstract Entity)
(disjointDecomposition Abstract Quantity Attribute SetOrClass Relation Proposition Graph GraphElement)
(documentation Abstract "Properties or qualities as distinguished from any
particular embodiment of the properties/qualities in a physical medium.
Instances of Abstract can be said to exist in the same sense as mathematical
objects such as sets and relations, but they cannot exist at a particular
place and time without some physical encoding or embodiment.")

;; Something is Abstract just in case it has neither a spatial nor temporal
;; location.

(<=>
   (instance ?ABS Abstract)
   (not
      (exists (?POINT)
         (or
            (located ?ABS ?POINT)
            (time ?ABS ?POINT)))))

(subclass Quantity Abstract)
(partition Quantity FiniteQuantity InfiniteQuantity)
(documentation Quantity "Any specification of how many or how much of
something there is.  Accordingly, there are two subclasses of &%Quantity:
&%Number (how many) and &%PhysicalQuantity (how much).")

(subclass Attribute Abstract)
(partition Attribute InternalAttribute RelationalAttribute)
(documentation Attribute "Qualities which we cannot or choose not to
reify into subclasses of &%Object.")

(instance property BinaryPredicate)
(domain property 1 Entity)
(domain property 2 Attribute)
(documentation property "This &%Predicate holds between an instance of
&%Entity and an instance of &%Attribute.  (property ?ENTITY ?ATTR)
means that ?ENTITY has the &%Attribute ?ATTR.")

(instance attribute AsymmetricRelation)
(instance attribute IrreflexiveRelation)
(subrelation attribute property)
(domain attribute 1 Object)
(documentation attribute "(&%attribute ?OBJECT ?PROPERTY) means that
?PROPERTY is a &%Attribute of ?OBJECT.  For example,
(&%attribute &%MyLittleRedWagon &%Red).")

(instance manner AsymmetricRelation)
(instance manner IrreflexiveRelation)
(subrelation manner property)
(domain manner 1 Process)
(disjointRelation manner attribute)
(documentation manner "(&%manner ?PROCESS ?MANNER) means that the
&%Process ?PROCESS is qualified by the &%Attribute ?MANNER.  The &%Attributes
of &%Processes are usually denoted by adverbs and include things like the
speed of the wind, the style of a dance, or the intensity of a sports
competition.")

(instance AbstractionFn UnaryFunction)
(instance AbstractionFn PartialValuedRelation)
(domain AbstractionFn 1 Class)
(range AbstractionFn Attribute)
(documentation AbstractionFn "A &%UnaryFunction that maps a &%Class into
the instance of &%Attribute that specifies the condition(s) for membership
in the &%Class.")

(<=>
   (equal (AbstractionFn ?CLASS) ?ATTR)
   (forall (?INST)
      (<=>
         (instance ?INST ?CLASS)
         (property ?INST ?ATTR))))

(instance ExtensionFn UnaryFunction)
(instance ExtensionFn PartialValuedRelation)
(domain ExtensionFn 1 Attribute)
(range ExtensionFn Class)
(documentation ExtensionFn "A &%UnaryFunction that maps an &%Attribute
into the &%Class whose condition for membership is the &%Attribute.")

(<=>
   (equal (ExtensionFn ?ATTRIBUTE) ?CLASS)
   (equal (AbstractionFn ?CLASS) ?ATTRIBUTE))

(subclass InternalAttribute Attribute)
(documentation InternalAttribute "Any &%Attribute of an &%Entity that is an
internal property of the &%Entity, e.g. its shape, its color, its fragility,
etc.")

(subclass RelationalAttribute Attribute)
(documentation RelationalAttribute "Any &%Attribute that an &%Entity has by
virtue of a relationship that it bears to another &%Entity or set of &%Entities,
e.g. &%SocialRoles and &%PositionalAttributes.")

;; The following formulas incorporate the Number hierarchy from the
;; ontology 'kif-numbers' on the Ontolingua server.

(subclass Number Quantity)
(necessarily Number isReferencedBy AbstractNumericString)
(:hasRestrictedVal Number isRepresentedBy NumericString)
(partition Number RealNumber ImaginaryNumber ComplexNumber)
(documentation Number "A measure of how many things there are, or how
much there is, of a certain kind.  &%Numbers are subclassed into
&%RealNumber, &%ComplexNumber, and &%ImaginaryNumber.")

(instance lessThan BinaryPredicate)
(instance lessThan TransitiveRelation)
(instance lessThan IrreflexiveRelation)
(instance lessThan RelationExtendedToQuantities)
(trichotomizingOn lessThan RealNumber)
(domain lessThan 1 Quantity)
(domain lessThan 2 Quantity)
(documentation lessThan "(&%lessThan ?NUMBER1 ?NUMBER2) is true just
in case the &%Quantity ?NUMBER1 is less than the &%Quantity ?NUMBER2.")

(instance greaterThan BinaryPredicate)
(instance greaterThan TransitiveRelation)
(instance greaterThan IrreflexiveRelation)
(instance greaterThan RelationExtendedToQuantities)
(trichotomizingOn greaterThan RealNumber)
(domain greaterThan 1 Quantity)
(domain greaterThan 2 Quantity)
(inverse greaterThan lessThan)
(documentation greaterThan "(&%greaterThan ?NUMBER1 ?NUMBER2) is true
just in case the &%Quantity ?NUMBER1 is greater than the &%Quantity
?NUMBER2.")

(instance lessThanOrEqualTo BinaryPredicate)
(instance lessThanOrEqualTo PartialOrderingRelation)
(instance lessThanOrEqualTo RelationExtendedToQuantities)
(trichotomizingOn lessThanOrEqualTo RealNumber)
(domain lessThanOrEqualTo 1 Quantity)
(domain lessThanOrEqualTo 2 Quantity)
(documentation lessThanOrEqualTo "(&%lessThanOrEqualTo ?NUMBER1 ?NUMBER2)
is true just in case the &%Quantity ?NUMBER1 is less than or equal to
the &%Quantity ?NUMBER2.")

(<=>
   (lessThanOrEqualTo ?NUMBER1 ?NUMBER2)
   (or
     (equal ?NUMBER1 ?NUMBER2)
     (lessThan ?NUMBER1 ?NUMBER2)))

(instance greaterThanOrEqualTo BinaryPredicate)
(instance greaterThanOrEqualTo PartialOrderingRelation)
(instance greaterThanOrEqualTo RelationExtendedToQuantities)
(trichotomizingOn greaterThanOrEqualTo RealNumber)
(domain greaterThanOrEqualTo 1 Quantity)
(domain greaterThanOrEqualTo 2 Quantity)
(inverse greaterThanOrEqualTo lessThanOrEqualTo)
(documentation greaterThanOrEqualTo "(&%greaterThanOrEqualTo ?NUMBER1
?NUMBER2) is true just in case the &%Quantity ?NUMBER1 is greater
than the &%Quantity ?NUMBER2.")

(<=>
   (greaterThanOrEqualTo ?NUMBER1 ?NUMBER2)
   (or
     (equal ?NUMBER1 ?NUMBER2)
     (greaterThan ?NUMBER1 ?NUMBER2)))

(subclass RealNumber Number)
(partition RealNumber NegativeRealNumber NonnegativeRealNumber)
(partition RealNumber RationalNumber IrrationalNumber)
(documentation RealNumber "Any &%Number that can be expressed as a
(possibly infinite) decimal, i.e. any &%Number that has a position
on the number line.")

(subclass ImaginaryNumber Number)
(documentation ImaginaryNumber "Any &%Number that is the result of
multiplying a &%RealNumber by the square root of -1.")

(=>
   (instance ?NUMBER ImaginaryNumber)
   (exists (?REAL)
      (and
         (instance ?REAL RealNumber)
         (equal ?NUMBER (MultiplicationFn ?REAL (SquareRootFn -1))))))

(subclass RationalNumber RealNumber)
(documentation RationalNumber "Any &%RealNumber that is the product of
dividing two &%Integers.")

(subclass IrrationalNumber RealNumber)
(documentation IrrationalNumber "Any &%RealNumber that is not also a
&%RationalNumber.")

(subclass NonnegativeRealNumber RealNumber)
(documentation NonnegativeRealNumber "A &%RealNumber that is greater than
or equal to zero.")

(<=>
   (instance ?NUMBER NonnegativeRealNumber)
   (and
      (greaterThanOrEqualTo ?NUMBER 0)
      (instance ?NUMBER RealNumber)))

(subclass PositiveRealNumber NonnegativeRealNumber)
(documentation PositiveRealNumber "A &%RealNumber that is greater than
zero.")

(<=>
   (instance ?NUMBER PositiveRealNumber)
   (and
      (greaterThan ?NUMBER 0)
      (instance ?NUMBER RealNumber)))

(subclass NegativeRealNumber RealNumber)
(documentation NegativeRealNumber "A &%RealNumber that is less than
zero.")

(<=>
   (instance ?NUMBER NegativeRealNumber)
   (and
      (lessThan ?NUMBER 0)
      (instance ?NUMBER RealNumber)))

(subclass Integer RationalNumber)
(partition Integer OddInteger EvenInteger)
(partition Integer NegativeInteger NonnegativeInteger)
(documentation Integer "A negative or nonnegative whole number.")

(subclass EvenInteger Integer)
(documentation EvenInteger "An &%Integer that is evenly divisible
by 2.")

(subclass OddInteger Integer)
(documentation OddInteger "An &%Integer that is not evenly divisible
by 2.")

(subclass PrimeNumber Integer)
(documentation PrimeNumber "An &%Integer that is evenly divisible only
by itself and 1.")

(subclass NonnegativeInteger Integer)
(subclass NonnegativeInteger NonnegativeRealNumber)
(documentation NonnegativeInteger "An &%Integer that is greater than
or equal to zero.")

(subclass NegativeInteger Integer)
(subclass NegativeInteger NegativeRealNumber)
(documentation NegativeInteger "An &%Integer that is less than zero.")

(subclass PositiveInteger NonnegativeInteger)
(subclass PositiveInteger PositiveRealNumber)
(documentation PositiveInteger "An &%Integer that is greater than zero.")

(subclass BinaryNumber RealNumber)
(documentation BinaryNumber "Elements from the number system with base 2.
Every &%BinaryNumber is expressed as a sequence of the digits 1 and 0.")

(subclass ComplexNumber Number)
(disjoint ComplexNumber RealNumber)
(documentation ComplexNumber "A &%Number that has the form: x + yi, where x
and y are &%RealNumbers and i is the square root of -1.")

(=>
   (instance ?NUMBER ComplexNumber)
   (exists (?REAL1 ?REAL2)
      (and
         (instance ?REAL1 RealNumber)
         (instance ?REAL2 RealNumber)
         (equal ?NUMBER (AdditionFn ?REAL1 (MultiplicationFn ?REAL2 (SquareRootFn -1)))))))

(subclass PhysicalQuantity Quantity)
(partition PhysicalQuantity ConstantQuantity FunctionQuantity)
(documentation PhysicalQuantity "&%A PhysicalQuantity is a measure of
some quantifiable aspect of the modeled world, such as 'the earth's
diameter' (a constant length) and 'the stress in a loaded deformable
solid' (a measure of stress, which is a function of three spatial
coordinates).  All &%PhysicalQuantities are either &%ConstantQuantities
or &%FunctionQuantities.  Instances of &%ConstantQuantity are dependent
on a &%UnitOfMeasure, while instances of &%FunctionQuantity are
&%Functions that map instances of &%ConstantQuantity to other instances
of &%ConstantQuantity (e.g., &%TimeDependentQuantities are
&%FunctionQuantities).  Although the name and definition of
&%PhysicalQuantity is borrowed from physics, &%PhysicalQuantities need
not be material.  Aside from the dimensions of length, time, velocity,
etc., nonphysical dimensions such as currency are also possible.
Accordingly, amounts of money would be instances of &%PhysicalQuantity.
&%PhysicalQuantities are distinguished from &%Numbers by the fact that
the former are associated with a dimension of measurement.")

(subclass ConstantQuantity PhysicalQuantity)
(documentation ConstantQuantity "A &%ConstantQuantity is a
&%PhysicalQuantity which has a constant value, e.g. 3 meters and 5 hours.
The magnitude (see &%MagnitudeFn) of every &%ConstantQuantity is a
&%RealNumber.  &%ConstantQuantities are distinguished from
&%FunctionQuantities, which map &%ConstantQuantities to other
&%ConstantQuantities.  All &%ConstantQuantites are expressed with the
&%BinaryFunction &%MeasureFn, which takes a &%Number and a &%UnitOfMeasure
as arguments.  For example, 3 &%Meters can be expressed as (&%MeasureFn 3
&%Meter).  &%ConstantQuantities form a partial order (see
&%PartialOrderingRelation) with the &%lessThan relation, since &%lessThan
is a &%RelationExtendedToQuantities and &%lessThan is defined over the
&%RealNumbers.  The &%lessThan relation is not a total order (see
&%TotalOrderingRelation) over the class &%ConstantQuantity since elements
of some subclasses of &%ConstantQuantity (such as length quantities)
are incomparable to elements of other subclasses of &%ConstantQuantity
(such as mass quantities).")

(subclass TimeMeasure ConstantQuantity)
(documentation TimeMeasure "The class of temporal durations (instances
of &%TimeDuration) and positions of &%TimePoints and &%TimeIntervals along
the universal timeline (instances of &%TimePosition).")

(subclass TimeDuration TimeMeasure)
(documentation TimeDuration "Any measure of length of time,
with or without respect to the universal timeline.")

(subclass TimePosition TimeMeasure)
(subclass TimePosition Context)
(partition TimePosition TimeInterval TimePoint)
(documentation TimePosition "Any &%TimePoint or &%TimeInterval
along the universal timeline from &%NegativeInfinity to
&%PositiveInfinity.")

(subclass TimeInterval TimePosition)
(documentation TimeInterval "An interval of time.  Note that a
&%TimeInterval has both an extent and a location on the universal
timeline.  Note too that a &%TimeInterval has no gaps, i.e. this
class contains only convex time intervals.")

(subclass RecurrentTimeInterval TimeInterval)
(documentation RecurrentTimeInterval "A class of intervals of time that may recur, such as specific hours of a day or specific days of the year.  Note that a &%TimeInterval has both an extent and a location on the universal
timeline.  Note too that a &%TimeInterval has no gaps, i.e. this
class contains only convex time intervals.")

(subclass TimePoint TimePosition)
(documentation TimePoint "An extensionless point on the universal timeline.
The &%TimePoints at which &%Processes occur can be known with various
degrees of precision and approximation, but conceptually &%TimePoints are
point-like and not interval-like.  That is, it doesn't make sense to talk
about how long a &%TimePoint lasts.")

(subclass FunctionQuantity PhysicalQuantity)
;;(subclass FunctionQuantity Function)
;; commented out because ofdisjoint conflict -- PJC
(documentation FunctionQuantity "A &%FunctionQuantity is a &%Function that
maps from one or more instances of &%ConstantQuantity to another instance
of &%ConstantQuantity.  For example, the velocity of a particle would be
represented by a &%FunctionQuantity mapping values of time (which are
&%ConstantQuantities) to values of distance (also &%ConstantQuantities).
Note that all instances of &%FunctionQuantity are &%Functions with a fixed
arity.  Note too that all elements of the range of a &%FunctionQuantity
have the same physical dimension as the &%FunctionQuantity itself.")

(subclass UnaryConstantFunctionQuantity FunctionQuantity)
(subclass UnaryConstantFunctionQuantity UnaryFunction)
(documentation UnaryConstantFunctionQuantity "The class of &%UnaryFunctions
that map from the &%Class &%ConstantQuantity to the &%Class
&%ConstantQuantity.")

(=>
   (instance ?FUNCTION UnaryConstantFunctionQuantity)
   (and
      (domain ?FUNCTION 1 ConstantQuantity)
      (range ?FUNCTION ConstantQuantity)))

(subclass TimeDependentQuantity UnaryConstantFunctionQuantity)
;; (subclass TimeDependentQuantity ContinuousFunction)
(documentation TimeDependentQuantity "A &%UnaryConstantFunction of continuous
time.  All instances of this &%Class map a time quantity into another
&%ConstantQuantity such as temperature.  For example, 'the temperature at
the top of the Empire State Building' is a &%TimeDependentQuantity since
its value depends on the time.")

(=>
   (instance ?FUNCTION TimeDependentQuantity)
   (domain ?FUNCTION 1 TimeMeasure))

(subclass SetOrClass Abstract)
(partition SetOrClass Set Class)
(documentation SetOrClass "The &%SetOrClass of &%Sets and &%Classes, i.e. any instance
of &%Abstract that has &%elements or &%instances.")

(subclass Class SetOrClass)
(documentation Class "&%Classes differ from &%Sets in three important respects.
First, &%Classes are not assumed to be extensional.  That is, distinct
&%Classes might well have exactly the same instances.  Second, &%Classes typically
have an associated `condition' that determines the instances of the &%Class.  So,
for example, the condition `human' determines the &%Class of &%Humans.  Note that
some &%Classes might satisfy their own condition (e.g., the &%Class of &%Abstract
things is &%Abstract) and hence be instances of themselves.  Third, the instances
of a class may occur only once within the class, i.e. a class cannot contain
duplicate instances.")

(subclass Set SetOrClass)
(documentation Set "A &%SetOrClass that satisfies extensionality as well as
other constraints specified by some choice of set theory.  &%Sets differ
from &%Classes in two important respects.  First, &%Sets are extensional -
two &%Sets with the same &%elements are identical.  Second, a &%Set can be
an arbitrary stock of objects.  That is, there is no requirement that &%Sets
have an associated condition that determines their membership.  Note that &%Sets
are not assumed to be unique sets, i.e. &%elements of a &%Set may occur more
than once in the &%Set.")

(subclass Relation Abstract)
(disjointDecomposition Relation UnaryRelation BinaryRelation TernaryRelation QuaternaryRelation QuintaryRelation VariableArityRelation)
(partition Relation Predicate Function List)
(partition Relation TotalValuedRelation PartialValuedRelation)
(documentation Relation "The &%Class of relations.  There are three kinds
of &%Relation:  &%Predicate, &%Function, and &%List.  &%Predicates and
&%Functions both denote sets of ordered n-tuples.  The difference between
these two &%Classes is that &%Predicates cover formula-forming operators, while &%Functions cover term-forming operators.  A &%List, on the other hand, is a particular ordered n-tuple.  This statement was modified by P. Cassidy January 2004 to include UnaryRelation.")

(=>
   (instance ?REL Relation)
   (<=>
        (holds ?REL @ROW)
        (?REL @ROW)))

;; The following part of the ontology covers the various classes under
;; 'Relation'.  Most of the content here is taken from frame-ontology,
;; abstract-algebra, kif-relations, and kif-extensions (ontologies
;; available on the Ontolingua server).

(subclass SingleValuedRelation Relation)
(instance SingleValuedRelation InheritableRelation)
(documentation SingleValuedRelation "A &%Relation is a &%SingleValuedRelation
just in case an assignment of values to every argument position except the last
one determines at most one assignment for the last argument position.  Note
that not all &%SingleValuedRelations are &%TotalValuedRelations.")

(=>
   (instance ?REL SingleValuedRelation)
   (forall (@ROW ?ITEM1 ?ITEM2)
      (=>
         (and
            (holds ?REL @ROW ?ITEM1)
            (holds ?REL @ROW ?ITEM2))
         (equal ?ITEM1 ?ITEM2))))

(subclass TotalValuedRelation Relation)
(instance TotalValuedRelation InheritableRelation)
(documentation TotalValuedRelation "A &%Relation is a &%TotalValuedRelation
just in case there exists an assignment for the last argument position of the &%Relation given any assignment of values to every argument position except the last one.  Note that declaring a &%Relation to be both a &%TotalValuedRelation and a &%SingleValuedRelation means that it is a total function.")

(<=>
   (instance ?REL TotalValuedRelation)
   (exists (?VALENCE)
      (and
         (instance ?REL Relation)
         (valence ?REL ?VALENCE)
         (=>
            (forall (?NUMBER ?ELEMENT ?CLASS)
               (=>
                  (and
                     (lessThan ?NUMBER ?VALENCE)
                     (domain ?REL ?NUMBER ?CLASS)
                     (equal ?ELEMENT (ListOrderFn (ListFn @ROW) ?NUMBER)))
                  (instance ?ELEMENT ?CLASS)))
            (exists (?ITEM)
               (holds ?REL @ROW ?ITEM))))))

(subclass PartialValuedRelation Relation)
(documentation PartialValuedRelation "A &%Relation is a &%PartialValuedRelation
just in case it is not a &%TotalValuedRelation, i.e. just in case assigning values to every argument position except the last one does not necessarily mean that there is a value assignment for the last argument position.  Note that, if a &%Relation is both a &%PartialValuedRelation and a &%SingleValuedRelation, then it is a partial function.")

(subclass UnaryRelation Relation)
(documentation UnaryRelation "This relation was added to the SUMO 1.55 in November 2003 by P. Cassidy to allow consistent inclusion of the 'not' logical operator within the hierarchy of relations.")

(subclass BinaryRelation Relation)
(instance BinaryRelation InheritableRelation)
(documentation BinaryRelation "&%BinaryRelations are relations that are
true only of pairs of things.  &%BinaryRelations are represented as slots
in frame systems.")

(=>
   (instance ?REL BinaryRelation)
   (not
      (exists (?ITEM1 ?ITEM2 ?ITEM3 @ROW)
         (holds ?REL ?ITEM1 ?ITEM2 ?ITEM3 @ROW))))

(subclass ReflexiveRelation BinaryRelation)
(documentation ReflexiveRelation "&%Relation ?REL is reflexive if
(?REL ?INST ?INST) for all ?INST.")

(=>
   (instance ?REL ReflexiveRelation)
   (=>
      (or
         (holds ?REL ?INST1 ?INST2)
         (holds ?REL ?INST2 ?INST1))
      (holds ?REL ?INST1 ?INST1)))

(subclass IrreflexiveRelation BinaryRelation)
(documentation IrreflexiveRelation "&%Relation ?REL is irreflexive
if (?REL ?INST ?INST) holds for no value of ?INST.")

(=>
   (instance ?REL IrreflexiveRelation)
   (forall (?INST)
      (not
         (holds ?REL ?INST ?INST))))

(subclass SymmetricRelation BinaryRelation)
(documentation SymmetricRelation "A &%BinaryRelation ?REL is
symmetric just in case (?REL ?INST1 ?INST2) imples (?REL
?INST2 ?INST1), for all ?INST1 and ?INST2.")


(=>
   (instance ?REL SymmetricRelation)
   (forall (?INST1 ?INST2)
      (=>
         (holds ?REL ?INST1 ?INST2)
         (holds ?REL ?INST2 ?INST1))))

(subclass AsymmetricRelation IrreflexiveRelation)
(subclass AsymmetricRelation AntisymmetricRelation)
(documentation AsymmetricRelation "A &%BinaryRelation is asymmetric only
if it is both an &%AntisymmetricRelation and an &%IrreflexiveRelation.")

(=>
   (and
      (instance ?REL BinaryRelation)
      (or
         (domain ?REL 1 ?CLASS1)
         (domainSubclass ?REL 1 ?CLASS1))
      (or
         (domain ?REL 2 ?CLASS2)
         (domainSubclass ?REL 2 ?CLASS2)
         (range ?REL ?CLASS2)
         (rangeSubclass ?REL ?CLASS2))
      (disjoint ?CLASS1 ?CLASS2))
   (instance ?REL AsymmetricRelation))

(subclass AntisymmetricRelation BinaryRelation)
(documentation AntisymmetricRelation "&%BinaryRelation ?REL is an
&%AntisymmetricRelation if for distinct ?INST1 and ?INST2, (?REL ?INST1
?INST2) implies not (?REL ?INST2 ?INST1).  In other words, for all ?INST1
and ?INST2, (?REL ?INST1 ?INST2) and (?REL ?INST2 ?INST1) imply that ?INST1
and ?INST2 are identical.  Note that it is possible for an
&%AntisymmetricRelation to be a &%ReflexiveRelation.")

(=>
   (instance ?REL AntisymmetricRelation)
   (forall (?INST1 ?INST2)
      (=>
         (and
            (holds ?REL ?INST1 ?INST2)
            (holds ?REL ?INST2 ?INST1))
         (equal ?INST1 ?INST2))))

(subclass TrichotomizingRelation BinaryRelation)
(documentation TrichotomizingRelation "A &%BinaryRelation ?REL is a
&%TrichotomizingRelation just in case all ordered pairs consisting of
distinct individuals are elements of ?REL.")

(=>
   (instance ?REL TrichotomizingRelation)
   (forall (?INST1 ?INST2)
      (or
         (holds ?REL ?INST1 ?INST2)
         (equal ?INST1 ?INST2)
         (holds ?REL ?INST2 ?INST1))))

(subclass TransitiveRelation BinaryRelation)
(documentation TransitiveRelation "A &%BinaryRelation ?REL is transitive
if (?REL ?INST1 ?INST2) and (?REL ?INST2 ?INST3) imply (?REL ?INST1 ?INST3),
for all ?INST1, ?INST2, and ?INST3.")

(=>
   (instance ?REL TransitiveRelation)
   (forall (?INST1 ?INST2 ?INST3)
      (=>
         (and
            (holds ?REL ?INST1 ?INST2)
            (holds ?REL ?INST2 ?INST3))
         (holds ?REL ?INST1 ?INST3))))

(subclass IntransitiveRelation BinaryRelation)
(documentation IntransitiveRelation "A &%BinaryRelation ?REL is
intransitive only if (?REL ?INST1 ?INST2) and (?REL ?INST2 ?INST3) imply not
(?REL ?INST1 ?INST3), for all ?INST1, ?INST2, and ?INST3.")

(=>
   (instance ?REL IntransitiveRelation)
   (forall (?INST1 ?INST2 ?INST3)
      (=>
         (and
            (holds ?REL ?INST1 ?INST2)
            (holds ?REL ?INST2 ?INST3))
         (not
            (holds ?REL ?INST1 ?INST3)))))

(subclass PartialOrderingRelation TransitiveRelation)
(subclass PartialOrderingRelation AntisymmetricRelation)
(subclass PartialOrderingRelation ReflexiveRelation)
(documentation PartialOrderingRelation "A &%BinaryRelation is a partial
ordering if it is a &%ReflexiveRelation, an &%AntisymmetricRelation, and
a &%TransitiveRelation.")

(subclass TotalOrderingRelation PartialOrderingRelation)
(subclass TotalOrderingRelation TrichotomizingRelation)
(documentation TotalOrderingRelation  "A &%BinaryRelation is a
&%TotalOrderingRelation if it is a &%PartialOrderingRelation
and a &%TrichotomizingRelation.")

(=>
   (instance ?REL TotalOrderingRelation)
   (forall (?INST1 ?INST2)
      (or
         (holds ?REL ?INST1 ?INST2)
         (holds ?REL ?INST2 ?INST1))))

(subclass EquivalenceRelation TransitiveRelation)
(subclass EquivalenceRelation SymmetricRelation)
(subclass EquivalenceRelation ReflexiveRelation)
(documentation EquivalenceRelation "A &%BinaryRelation is an equivalence
relation if it is a &%ReflexiveRelation, a &%SymmetricRelation, and a
&%TransitiveRelation.")

(subclass CaseRole BinaryPredicate)
(instance CaseRole InheritableRelation)
(subclass CaseRole AsymmetricRelation)
(documentation CaseRole "The &%Class of &%Predicates relating the
spatially distinguished parts of a &%Process. &%CaseRoles include, for
example, the &%agent, &%patient or &%destination of an action, the flammable
substance in a burning process, or the water that falls in rain.")

(instance agent CaseRole)
(domain agent 1 Process)
(domain agent 2 Agent)
(documentation agent "(&%agent ?PROCESS ?AGENT) means that ?AGENT is
an active determinant, either animate or inanimate, of the &%Process
?PROCESS, with or without voluntary intention.  For example, water is
the &%agent of erosion in the following proposition:  the water
eroded the coastline.  For another example, Eve is an &%agent in the
following proposition: Eve bit an apple.")

(=>
   (instance ?PROCESS Process)
   (exists (?CAUSE)
      (agent ?PROCESS ?CAUSE)))

(instance destination CaseRole)
(domain destination 1 Process)
(domain destination 2 Entity)
(documentation destination "(destination ?PROCESS ?GOAL) means that
?GOAL is the target or goal of the Process ?PROCESS.  For example,
Danbury would be the destination in the following proposition:  Bob went
to Danbury.  Note that this is a very general &%CaseRole and, in
particular, that it covers the concepts of 'recipient' and 'beneficiary'.
Thus, John would be the &%destination in the following proposition:
Tom gave a book to John.")

(instance experiencer CaseRole)
(domain experiencer 1 Process)
(domain experiencer 2 Agent)
(documentation experiencer "(&%experiencer ?PROCESS ?AGENT) means
that ?AGENT experiences the &%Process ?PROCESS.  For example, Yojo
is the &%experiencer of seeing in the following proposition:  Yojo
sees the fish.  Note that &%experiencer, unlike &%agent, does
not entail a causal relation between its arguments.")

(subrelation instrument patient)
(domain instrument 1 Process)
(domain instrument 2 Object)
(documentation instrument "(instrument ?EVENT ?TOOL) means that ?TOOL
is used by an agent in bringing about ?EVENT and that ?TOOL is not
changed by ?EVENT.  For example, the key is an &%instrument in the
following proposition: The key opened the door.  Note that &%instrument
and &%resource cannot be satisfied by the same ordered pair.")

(instance origin CaseRole)
(domain origin 1 Process)
(domain origin 2 Object)
(documentation origin "(&%origin ?PROCESS ?SOURCE) means that ?SOURCE
indicates where the ?Process began.  Note that this relation implies
that ?SOURCE is present at the beginning of the process, but need not
participate throughout the process.  For example, the submarine is the
&%origin in the following proposition: the missile was launched from a
submarine.")

(instance patient CaseRole)
(domain patient 1 Process)
(domain patient 2 Entity)
(documentation patient "(&%patient ?PROCESS ?ENTITY) means that ?ENTITY
is a participant in ?PROCESS that may be moved, said, experienced, etc.
For example, the direct objects in the sentences 'The cat swallowed the
canary' and 'Billy likes the beer' would be examples of &%patients.  Note
that the &%patient of a &%Process may or may not undergo structural
change as a result of the &%Process.  The &%CaseRole of &%patient is used
when one wants to specify as broadly as possible the object of a
&%Process.")

(subrelation resource patient)
(domain resource 1 Process)
(domain resource 2 Object)
(disjointRelation resource result instrument)
(documentation resource "(&%resource ?PROCESS ?RESOURCE) means that
?RESOURCE is present at the beginning of ?PROCESS, is used by ?PROCESS,
and as a consequence is changed by ?PROCESS.  For example, soap is a
&%resource in the following proposition:  the gun was carved out of soap.
Note that &%resource differs from &%instrument, another subrelation of
&%patient, in that its internal or physical properties are altered in
some way by the &%Process.")

(subrelation result patient)
(domain result 1 Process)
(domain result 2 Entity)
(documentation result "(result ?ACTION ?OUTPUT) means that ?OUTPUT is
a product of ?ACTION.  For example, house is a &%result in the
following proposition: Eric built a house.")

(instance InheritableRelation Class)
(documentation InheritableRelation "This is a &%Class of &%Classes.  Each
&%instance of &%InheritableRelation is a &%subclass of &%Relation whose
properties can be inherited downward in the class hierarchy via the
&%subrelation &%Predicate.")

(subclass ProbabilityRelation Relation)
(instance ProbabilityRelation InheritableRelation)
(documentation ProbabilityRelation "The &%Class of &%Relations that
permit assessment of the probability of an event or situation.")

(instance ProbabilityFn ProbabilityRelation)
(instance ProbabilityFn TotalValuedRelation)
(instance ProbabilityFn UnaryFunction)
(domain ProbabilityFn 1 Formula)
(range ProbabilityFn RealNumber)
(instance ProbabilityFn AsymmetricRelation)
(documentation ProbabilityFn "One of the basic &%ProbabilityRelations,
&%ProbabilityFn is used to state the a priori probability of a state of
affairs.  (&%ProbabilityFn ?FORMULA) denotes the a priori probability
of ?FORMULA.")

(instance conditionalProbability ProbabilityRelation)
(instance conditionalProbability TernaryPredicate)
(domain conditionalProbability 1 Formula)
(domain conditionalProbability 2 Formula)
(domain conditionalProbability 3 RealNumber)
(documentation conditionalProbability "One of the basic &%ProbabilityRelations.
&%conditionalProbability is used to state the numeric value of a conditional
probability.  (&%conditionalProbability ?FORMULA1 ?FORMULA2 ?NUMBER) means
that the probability of ?FORMULA2 being true given that ?FORMULA1 is true is
?NUMBER.")

(instance increasesLikelihood ProbabilityRelation)
(instance increasesLikelihood BinaryPredicate)
(instance increasesLikelihood IrreflexiveRelation)
(domain increasesLikelihood 1 Formula)
(domain increasesLikelihood 2 Formula)
(disjointRelation increasesLikelihood decreasesLikelihood independentProbability)
(documentation increasesLikelihood "One of the basic &%ProbabilityRelations.
(&%increasesLikelihood ?FORMULA1 ?FORMULA2) means that ?FORMULA2 is more
likely to be true if ?FORMULA1 is true.")

(=>
   (and
      (increasesLikelihood ?FORMULA1 ?FORMULA2)
      (equal (ProbabilityFn ?FORMULA2) ?NUMBER1)
      (conditionalProbability ?FORMULA1 ?FORMULA2 ?NUMBER2))
   (greaterThan ?NUMBER2 ?NUMBER1))

(instance decreasesLikelihood ProbabilityRelation)
(instance decreasesLikelihood BinaryPredicate)
(instance decreasesLikelihood IrreflexiveRelation)
(domain decreasesLikelihood 1 Formula)
(domain decreasesLikelihood 2 Formula)
(documentation decreasesLikelihood "One of the basic &%ProbabilityRelations.
(&%decreasesLikelihood ?FORMULA1 ?FORMULA2) means that ?FORMULA2 is less
likely to be true if ?FORMULA1 is true.")

(=>
   (and
      (decreasesLikelihood ?FORMULA1 ?FORMULA2)
      (equal (ProbabilityFn ?FORMULA2) ?NUMBER1)
      (conditionalProbability ?FORMULA1 ?FORMULA2 ?NUMBER2))
   (lessThan ?NUMBER2 ?NUMBER1))

(instance independentProbability ProbabilityRelation)
(instance independentProbability BinaryPredicate)
(instance independentProbability SymmetricRelation)
(domain independentProbability 1 Formula)
(domain independentProbability 2 Formula)
(documentation independentProbability "One of the basic &%ProbabilityRelations.
(&%independentProbability ?FORMULA1 ?FORMULA2) means that the probabilities of
?FORMULA1 and ?FORMULA2 being true are independent.")

(=>
   (and
      (independentProbability ?FORMULA1 ?FORMULA2)
      (equal (ProbabilityFn ?FORMULA2) ?NUMBER1)
      (conditionalProbability ?FORMULA1 ?FORMULA2 ?NUMBER2))
   (equal ?NUMBER2 ?NUMBER1))

(=>
   (and
      (instance ?FORMULA1 Formula)
      (instance ?FORMULA2 Formula))
   (or
      (increasesLikelihood ?FORMULA1 ?FORMULA2)
      (decreasesLikelihood ?FORMULA1 ?FORMULA2)
      (independentProbability ?FORMULA1 ?FORMULA2)))

(subclass SpatialRelation Relation)
(instance SpatialRelation InheritableRelation)
(documentation SpatialRelation "The &%Class of &%Relations that are
spatial in a wide sense.  This &%Class includes mereological relations
and topological relations.")

(subclass TemporalRelation Relation)
(instance TemporalRelation InheritableRelation)
(documentation TemporalRelation "The &%Class of temporal &%Relations.
This &%Class includes notions of (temporal) topology of intervals,
(temporal) schemata, and (temporal) extension.")

(instance IntentionalRelation InheritableRelation)
(documentation IntentionalRelation "The &%Class of &%Relations between
an &%Agent and one or more &%Entities, where the &%Relation requires that
the &%Agent have awareness of the &%Entity.")

(=>
   (and
      (instance ?REL IntentionalRelation)
      (holds ?REL ?AGENT @ROW)
      (inList ?OBJ (ListFn @ROW)))
   (inScopeOfInterest ?AGENT ?OBJ))

(instance prefers TernaryPredicate)
(instance prefers IntentionalRelation)
(domain prefers 1 CognitiveAgent)
(domain prefers 2 Formula)
(domain prefers 3 Formula)
(documentation prefers "(&%prefers ?AGENT ?FORMULA1 ?FORMULA2) means that
&%CognitiveAgent ?AGENT prefers the state of affairs expressed by ?FORMULA1
over the state of affairs expressed by ?FORMULA2 all things being equal.")

(=>
   (prefers ?AGENT ?FORMULA1 ?FORMULA2)
   (not (and
      (true ?FORMULA1 True)
      (true ?FORMULA2 True))))

(subclass PropositionalAttitude IntentionalRelation)
(subclass PropositionalAttitude AsymmetricRelation)
(instance PropositionalAttitude InheritableRelation)
(documentation PropositionalAttitude "The &%Class of
&%IntentionalRelations where the &%Agent has awareness of a
&%Proposition.")

(=>
   (and
      (instance ?REL PropositionalAttitude)
      (holds ?REL ?AGENT ?FORMULA))
   (instance ?FORMULA Formula))

(subclass ObjectAttitude IntentionalRelation)
(instance ObjectAttitude InheritableRelation)
(disjoint ObjectAttitude PropositionalAttitude)
(documentation ObjectAttitude "The &%Class of &%IntentionalRelations
where the &%Agent has awareness of an instance of &%Physical.")

(=>
   (and
      (instance ?REL ObjectAttitude)
      (holds ?REL ?AGENT ?THING))
   (instance ?THING Physical))

(instance inScopeOfInterest BinaryPredicate)
(instance inScopeOfInterest IntentionalRelation)
(domain inScopeOfInterest 1 CognitiveAgent)
(domain inScopeOfInterest 2 Entity)
(documentation inScopeOfInterest "A very general &%Predicate.
(&%inScopeOfInterest ?AGENT ?ENTITY) means that ?ENTITY is within the
scope of interest of ?AGENT.  Note that the interest indicated can be
either positive or negative, i.e. the ?AGENT can have an interest in
avoiding or promoting ?ENTITY.")

(=>
   (and
      (instance ?PROCESS IntentionalProcess)
      (agent ?PROCESS ?AGENT)
      (patient ?PROCESS ?OBJECT))
   (inScopeOfInterest ?AGENT ?OBJECT))

(instance needs ObjectAttitude)
(subrelation needs inScopeOfInterest)
(domain needs 1 CognitiveAgent)
(domain needs 2 Physical)
(documentation needs "(&%needs ?AGENT ?OBJECT) means that ?OBJECT is
physically required for the continued existence of ?AGENT.")

(=>
    (needs ?AGENT ?OBJECT)
    (wants ?AGENT ?OBJECT))

(instance wants ObjectAttitude)
(subrelation wants inScopeOfInterest)
(relatedInternalConcept wants desires)
(domain wants 1 CognitiveAgent)
(domain wants 2 Physical)
(documentation wants "(&%wants ?AGENT ?OBJECT) means that ?OBJECT is desired by ?AGENT,
i.e. ?AGENT believes that ?OBJECT will satisfy one of its goals.  Note that there is
no implication that what is wanted by an agent is not already possessed by the agent.")

(=>
   (wants ?AGENT ?OBJ)
   (exists (?PURP)
      (hasPurposeForAgent ?OBJ ?PURP ?AGENT)))

(=>
   (wants ?AGENT ?OBJ)
   (desires ?AGENT (possesses ?AGENT ?OBJ)))

(instance desires PropositionalAttitude)
(subrelation desires inScopeOfInterest)
(relatedInternalConcept desires wants)
(domain desires 1 CognitiveAgent)
(domain desires 2 Formula)
(documentation desires "(&%desires ?AGENT ?FORMULA) means that ?AGENT wants
to bring about the state of affairs expressed by ?FORMULA.  Note that there
is no implication that what is desired by the agent is not already true.
Note too that &%desires is distinguished from &%wants only in that the former
is a &%PropositionalAttitude, while &%wants is an &%ObjectAttitude.")

(instance considers PropositionalAttitude)
(subrelation considers inScopeOfInterest)
(domain considers 1 CognitiveAgent)
(domain considers 2 Formula)
(documentation considers "(&%considers ?AGENT ?FORMULA) means that ?AGENT
considers or wonders about the truth of the proposition expressed by
?FORMULA.")

(instance believes PropositionalAttitude)
(subrelation believes inScopeOfInterest)
(domain believes 1 CognitiveAgent)
(domain believes 2 Formula)
(documentation believes "The epistemic predicate of belief.
(&%believes ?AGENT ?FORMULA) means that ?AGENT believes the proposition
expressed by ?FORMULA.")

(=>
   (believes ?AGENT ?FORMULA)
   (exists (?TIME)
      (holdsDuring ?TIME (considers ?AGENT ?FORMULA))))

(instance knows PropositionalAttitude)
(subrelation knows inScopeOfInterest)
(domain knows 1 CognitiveAgent)
(domain knows 2 Formula)
(documentation knows "The epistemic predicate of knowing.  (&%knows
?AGENT ?FORMULA) means that ?AGENT knows the proposition expressed by
?FORMULA.  Note that &%knows entails conscious awareness, so this
&%Predicate cannot be used to express tacit or subconscious or
unconscious knowledge.")

(=>
    (knows ?AGENT ?FORMULA)
    (believes ?AGENT ?FORMULA))

(=>
   (knows ?AGENT ?FORMULA)
   (true ?FORMULA True))

(subclass TernaryRelation Relation)
(instance TernaryRelation InheritableRelation)
(documentation TernaryRelation "&%TernaryRelations relate three items.
The two &%subclasses of &%TernaryRelation are &%TernaryPredicate and
&%BinaryFunction.")

(=>
   (instance ?REL TernaryRelation)
   (not
      (exists (?ITEM1 ?ITEM2 ?ITEM3 ?ITEM4 @ROW)
         (holds ?REL ?ITEM1 ?ITEM2 ?ITEM3 ?ITEM4 @ROW))))

(subclass QuaternaryRelation Relation)
(instance QuaternaryRelation InheritableRelation)
(documentation QuaternaryRelation "&%QuaternaryRelations relate four
items.  The two &%subclasses of &%QuaternaryRelation are
&%QuaternaryPredicate and &%TernaryFunction.")

(=>
   (instance ?REL QuaternaryRelation)
   (not
      (exists (?ITEM1 ?ITEM2 ?ITEM3 ?ITEM4 ?ITEM5 @ROW)
         (holds ?REL ?ITEM1 ?ITEM2 ?ITEM3 ?ITEM4 ?ITEM5 @ROW))))

(subclass QuintaryRelation Relation)
(instance QuintaryRelation InheritableRelation)
(documentation QuintaryRelation "&%QuintaryRelations relate five items.
The two &%subclasses of &%QuintaryRelation are &%QuintaryPredicate and
&%QuaternaryFunction.")

(=>
   (instance ?REL QuintaryRelation)
   (not
      (exists (?ITEM1 ?ITEM2 ?ITEM3 ?ITEM4 ?ITEM5 ?ITEM6 @ROW)
         (holds ?REL ?ITEM1 ?ITEM2 ?ITEM3 ?ITEM4 ?ITEM5 ?ITEM6 @ROW))))

(subclass List Relation)
(documentation List "Every &%List is a particular ordered n-tuple of
items.  Generally speaking, &%Lists are created by means of the &%ListFn
&%Function, which takes any number of items as arguments and returns a
&%List with the items in the same order.  Anything, including other
&%Lists, may be an item in a &%List.  Note too that &%Lists are
extensional - two lists that have the same items in the same order are
identical.  Note too that a &%List may contain no items.  In that case,
the &%List is the &%NullList.")

(=>
   (instance ?LIST List)
   (exists (?NUMBER1)
      (exists (?ITEM1)
         (and
            (not (equal (ListOrderFn ?LIST ?NUMBER1) ?ITEM1))
            (forall (?NUMBER2)
               (=>
                  (and
                     (instance ?NUMBER2 PositiveInteger)
                     (lessThan ?NUMBER2 ?NUMBER1))
                  (exists (?ITEM2)
                     (equal (ListOrderFn ?LIST ?NUMBER2) ?ITEM2))))))))

(subclass UniqueList List)
(documentation UniqueList "A &%List in which no item appears more than once, i.e. a &%List for which there are no distinct numbers ?NUMBER1 and ?NUMBER2 such that (&%ListOrderFn ?LIST ?NUMBER1) and (&%ListOrderFn ?LIST ?NUMBER2) return the same value.")

(=>
   (instance ?LIST UniqueList)
   (forall (?NUMBER1 ?NUMBER2)
      (=>
         (equal (ListOrderFn ?LIST ?NUMBER1) (ListOrderFn ?LIST ?NUMBER2))
         (equal ?NUMBER1 ?NUMBER2))))

(instance NullList List)
(documentation NullList "The &%List that has no items.  The uniqueness of
&%NullList follows from the extensionality of &%Lists, i.e. the fact that
two &%Lists with the same items in the same order are identical.")

(<=>
   (equal ?LIST NullList)
   (and
      (instance ?LIST List)
      (not
         (exists (?ITEM)
            (inList ?ITEM ?LIST)))))

(instance ListFn Function)
(instance ListFn VariableArityRelation)
(instance ListFn TotalValuedRelation)
(domain ListFn 1 Entity)
(range ListFn List)
(documentation ListFn "A &%Function that takes any number of arguments and
returns the &%List containing those arguments in exactly the same order.")

(instance ListOrderFn BinaryFunction)
(instance ListOrderFn PartialValuedRelation)
(domain ListOrderFn 1 List)
(domain ListOrderFn 2 PositiveInteger)
(range ListOrderFn Entity)
(documentation ListOrderFn "(&%ListOrderFn ?LIST ?NUMBER) denotes the item
that is in the ?NUMBER position in the &%List ?LIST.  For example,
(&%ListOrderFn (&%ListFn &%Monday &%Tuesday &%Wednesday) 2) would return the
value &%Tuesday.")

(=>
   (and
      (instance ?LIST1 List)
      (instance ?LIST2 List)
      (forall (?NUMBER)
         (equal (ListOrderFn ?LIST1 ?NUMBER) (ListOrderFn ?LIST2 ?NUMBER))))
   (equal ?LIST1 ?LIST2))

(=>
   (and
      (domain ?REL ?NUMBER ?CLASS)
      (holds ?REL @ROW))
   (instance (ListOrderFn (ListFn @ROW) ?NUMBER) ?CLASS))

(=>
   (and
      (domainSubclass ?REL ?NUMBER ?CLASS)
      (holds ?REL @ROW))
   (subclass (ListOrderFn (ListFn @ROW) ?NUMBER) ?CLASS))

(instance ListLengthFn UnaryFunction)
(instance ListLengthFn TotalValuedRelation)
(domain ListLengthFn 1 List)
(range ListLengthFn NonnegativeInteger)
(documentation ListLengthFn "A &%Function that takes a &%List as its sole
argument and returns the number of items in the &%List.  For example,
(&%ListLengthFn (&%ListFn &%Monday &%Tuesday &%Wednesday)) would return the
value 3.")

(=>
   (and
      (equal (ListLengthFn ?LIST) ?NUMBER1)
      (instance ?LIST List)
      (instance ?NUMBER1 PositiveInteger))
   (forall (?NUMBER2)
      (<=>
         (exists (?ITEM)
            (and
               (equal (ListOrderFn ?LIST ?NUMBER2) ?ITEM)
               (inList ?ITEM ?LIST)))
         (lessThanOrEqualTo ?NUMBER2 ?NUMBER1))))

(equal (ListLengthFn (ListFn @ROW ?ITEM)) (SuccessorFn (ListLengthFn (ListFn @ROW))))

(equal (ListOrderFn (ListFn @ROW ?ITEM) (ListLengthFn (ListFn @ROW ?ITEM))) ?ITEM)

(=>
   (valence ?REL ?NUMBER)
   (forall (@ROW)
      (=>
         (holds ?REL @ROW)
         (equal (ListLengthFn (ListFn @ROW)) ?NUMBER))))

(=>
   (and
      (equal (ListLengthFn ?LIST1) ?NUMBER)
      (instance ?LIST List)
      (instance ?NUMBER1 PositiveInteger))
   (exists (?LIST2 ?ITEM)
      (and
         (initialList ?LIST1 ?LIST2)
         (equal (SuccessorFn ?NUMBER) (ListLengthFn ?LIST2))
         (equal (ListOrderFn ?LIST2 (SuccessorFn ?NUMBER)) ?ITEM))))

(instance ListConcatenateFn BinaryFunction)
(instance ListConcatenateFn TotalValuedRelation)
(domain ListConcatenateFn 1 List)
(domain ListConcatenateFn 2 List)
(range ListConcatenateFn List)
(documentation ListConcatenateFn "A &%Function that returns the concatenation
of the two &%Lists that are given as arguments.  For example, the value of
(&%ListConcatenateFn (&%ListFn &%Monday &%Tuesday) (&%ListFn &%Wednesday
&%Thursday)) would be (&%ListFn &%Monday &%Tuesday &%Wednesday &%Thursday).")

(<=>
   (equal ?LIST3 (ListConcatenateFn ?LIST1 ?LIST2))
   (forall (?NUMBER1 ?NUMBER2)
      (=>
         (and
            (lessThanOrEqualTo ?NUMBER1 (ListLengthFn ?LIST1))
            (lessThanOrEqualTo ?NUMBER2 (ListLengthFn ?LIST2))
            (instance ?NUMBER1 PositiveInteger)
            (instance ?NUMBER2 PositiveInteger))
         (and
            (equal (ListOrderFn ?LIST3 ?NUMBER1) (ListOrderFn ?LIST1 ?NUMBER1))
            (equal (ListOrderFn ?LIST3 (AdditionFn (ListLengthFn ?LIST1) ?NUMBER2)) (ListOrderFn ?LIST2 ?NUMBER2))))))

(instance inList BinaryPredicate)
(instance inList IrreflexiveRelation)
(instance inList AsymmetricRelation)
(domain inList 1 Entity)
(domain inList 2 List)
(documentation inList "The analog of &%element and &%instance for &%Lists.
(&%inList ?OBJ ?LIST) means that ?OBJ is in the &%List ?LIST.  For example,
(&%inList &%Tuesday (&%ListFn &%Monday &%Tuesday &%Wednesday)) would be true.")

(=>
   (inList ?ITEM ?LIST)
   (exists (?NUMBER)
      (equal (ListOrderFn ?LIST ?NUMBER) ?ITEM)))

(instance subList BinaryPredicate)
(instance subList PartialOrderingRelation)
(domain subList 1 List)
(domain subList 2 List)
(documentation subList "(&%subList ?LIST1 ?LIST2) means that ?LIST1 is a
sublist of ?LIST2, i.e. every element of ?LIST1 is an element of ?LIST2 and
the elements that are common to both &%Lists have the same order in both
&%Lists.")

(=>
   (subList ?LIST1 ?LIST2)
   (forall (?ITEM)
      (=>
         (inList ?ITEM ?LIST1)
         (inList ?ITEM ?LIST2))))

(=>
   (subList ?LIST1 ?LIST2)
   (exists (?NUMBER3)
      (forall (?ITEM)
         (=>
            (inList ?ITEM ?LIST1)
            (exists (?NUMBER1 ?NUMBER2)
               (and
                  (equal (ListOrderFn ?LIST1 ?NUMBER1) ?ITEM)
                  (equal (ListOrderFn ?LIST2 ?NUMBER2) ?ITEM)
                  (equal ?NUMBER2 (AdditionFn ?NUMBER1 ?NUMBER3))))))))

(instance initialList BinaryPredicate)
(instance initialList PartialOrderingRelation)
(subrelation initialList subList)
(documentation initialList "(&%initialList ?LIST1 ?LIST2) means that ?LIST1 is a &%subList of ?LIST2 and (&%ListOrderFn ?LIST1 ?NUMBER) returns the same value as (&%ListOrderFn ?LIST2 ?NUMBER) for all of the values of ?NUMBER over which (&%ListOrderFn ?LIST1 ?NUMBER) is defined.")

(=>
   (initialList ?LIST1 ?LIST2)
   (forall (?NUMBER1 ?NUMBER2)
      (=>
         (and
            (equal (ListLengthFn ?LIST1) ?NUMBER1)
            (lessThanOrEqualTo ?NUMBER2 ?NUMBER1))
         (equal (ListOrderFn ?LIST1 ?NUMBER2) (ListOrderFn ?LIST2 ?NUMBER2)))))

;; (initialList (ListFn @ROW) (ListFn @ROW ?ITEM))
;;  SkifTab parser doesn't know what to do with the above, so 
;;  we rephrase it -- PJC

(=>
   (equal ?LIST (ListFn @ROW))
   (forall (?ITEM)
     (initialList ?LIST (ListFn @ROW ?ITEM))))

(subclass Predicate Relation)
(instance Predicate InheritableRelation)
(documentation Predicate "A &%Predicate is a sentence-forming &%Relation.
Each tuple in the &%Relation is a finite, ordered sequence of objects.
The fact that a particular tuple is an element of a &%Predicate is denoted
by '(*predicate* arg_1 arg_2 .. arg_n)', where the arg_i are the
objects so related.  In the case of &%BinaryPredicates, the fact can
be read as `arg_1 is *predicate* arg_2' or `a *predicate* of
arg_1 is arg_2'.")

(subclass Function SingleValuedRelation)
(instance Function InheritableRelation)
(documentation Function "A &%Function is a term-forming &%Relation that
maps from a n-tuple of arguments to a range and that associates this
n-tuple with at most one range element.  Note that the range is a &%SetOrClass,
and each element of the range is an instance of the &%SetOrClass.")

(subclass UnaryFunction Function)
(subclass UnaryFunction BinaryRelation)
(instance UnaryFunction InheritableRelation)
(documentation UnaryFunction "The &%Class of &%Functions that require a
single argument.")

(=>
   (instance ?FUNCTION UnaryFunction)
   (valence ?FUNCTION 1))

(subclass OneToOneFunction UnaryFunction)
(documentation OneToOneFunction "The &%Class of &%UnaryFunctions which
are one to one.  A function F is one to one just in case for all X, Y in the
domain of F, if X is not identical to Y, then F(X) is not identical to F(Y).")

(=>
   (instance ?FUN OneToOneFunction)
   (forall (?ARG1 ?ARG2)
      (=>
         (and
            (domain ?FUN 1 ?CLASS)
            (instance ?ARG1 ?CLASS)
            (instance ?ARG2 ?CLASS)
            (not (equal ?ARG1 ?ARG2)))
         (not (equal (AssignmentFn ?FUN ?ARG1) (AssignmentFn ?FUN ?ARG2))))))

(subclass SequenceFunction OneToOneFunction)
(documentation SequenceFunction "The &%Class of &%OneToOneFunctions whose range
is a subclass of the &%PositiveIntegers.")

(=>
   (and
      (instance ?SEQ SequenceFunction)
      (range ?SEQ ?CLASS))
   (subclass ?CLASS Integer))

(subclass BinaryFunction Function)
(subclass BinaryFunction TernaryRelation)
(instance BinaryFunction InheritableRelation)
(documentation BinaryFunction "The &%Class of &%Functions that require
two arguments.")

(=>
   (instance ?FUNCTION BinaryFunction)
   (valence ?FUNCTION 2))

(subclass AssociativeFunction BinaryFunction)
(documentation AssociativeFunction "A &%BinaryFunction is associative if
bracketing has no effect on the value returned by the &%Function.  More
precisely, a &%Function ?FUNCTION is associative just in case
(?FUNCTION ?INST1 (?FUNCTION ?INST2 ?INST3)) is equal to
(?FUNCTION (?FUNCTION ?INST1 ?INST2) ?INST3), for all ?INST1, ?INST2,
and ?INST3.")

(=>
   (instance ?FUNCTION AssociativeFunction)
   (forall (?INST1 ?INST2 ?INST3)
      (=>
         (and
            (domain ?FUNCTION 1 ?CLASS)
            (instance ?INST1 ?CLASS)
            (instance ?INST2 ?CLASS)
            (instance ?INST3 ?CLASS))
         (equal (AssignmentFn ?FUNCTION ?INST1 (AssignmentFn ?FUNCTION ?INST2 ?INST3))
             (AssignmentFn ?FUNCTION (AssignmentFn ?FUNCTION ?INST1 ?INST2) ?INST3)))))

(subclass CommutativeFunction BinaryFunction)
(documentation CommutativeFunction "A &%BinaryFunction is commutative if
the ordering of the arguments of the function has no effect on the value
returned by the function.  More precisely, a function ?FUNCTION is
commutative just in case (?FUNCTION ?INST1 ?INST2) is equal to (?FUNCTION
?INST2 ?INST1), for all ?INST1 and ?INST2.")

(=>
   (instance ?FUNCTION CommutativeFunction)
   (forall (?INST1 ?INST2)
      (=>
         (and
            (domain ?FUNCTION 1 ?CLASS)
            (instance ?INST1 ?CLASS)
            (instance ?INST2 ?CLASS))
         (equal (AssignmentFn ?FUNCTION ?INST1 ?INST2)
                (AssignmentFn ?FUNCTION ?INST2 ?INST1)))))

(subclass TernaryFunction Function)
(subclass TernaryFunction QuaternaryRelation)
(instance TernaryFunction InheritableRelation)
(documentation TernaryFunction "The &%Class of &%Functions that require
exactly three arguments.")

(=>
   (instance ?FUNCTION TernaryFunction)
   (valence ?FUNCTION 3))

(subclass QuaternaryFunction Function)
(subclass QuaternaryFunction QuintaryRelation)
(instance QuaternaryFunction InheritableRelation)
(documentation QuaternaryFunction "The &%Class of &%Functions that require
exactly four arguments.")

(=>
   (instance ?FUNCTION QuaternaryFunction)
   (valence ?FUNCTION 4))

(subclass ContinuousFunction Function)
(documentation ContinuousFunction "&%Functions which are continuous.
This concept is taken as primitive until representations for limits
are devised.")

(subclass LogicalOperator Predicate)
(documentation LogicalOperator "This &%Class currently comprises all
of the logical operators (viz. 'and', 'or', 'not', '=>', and '<=>').")

(subclass BinaryPredicate Predicate)
(subclass BinaryPredicate BinaryRelation)
(instance BinaryPredicate InheritableRelation)
(documentation BinaryPredicate "A &%Predicate relating two items - its
valence is two.")

(=>
   (instance ?REL BinaryPredicate)
   (valence ?REL 2))

(subclass TernaryPredicate Predicate)
(subclass TernaryPredicate TernaryRelation)
(instance TernaryPredicate InheritableRelation)
(documentation TernaryPredicate "The &%Class of &%Predicates that require
exactly three arguments.")

(=>
   (instance ?REL TernaryPredicate)
   (valence ?REL 3))

(subclass QuaternaryPredicate Predicate)
(subclass QuaternaryPredicate QuaternaryRelation)
(instance QuaternaryPredicate InheritableRelation)
(documentation QuaternaryPredicate "The &%Class of &%Predicates that
require four arguments.")

(=>
   (instance ?REL QuaternaryPredicate)
   (valence ?REL 4))

(subclass QuintaryPredicate Predicate)
(subclass QuintaryPredicate QuintaryRelation)
(instance QuintaryPredicate InheritableRelation)
(documentation QuintaryPredicate "The &%Class of &%Predicates that
require five arguments.")

(=>
   (instance ?REL QuintaryPredicate)
   (valence ?REL 5))

(subclass VariableArityRelation Relation)
(documentation VariableArityRelation "The &%Class of &%Relations that
do not have a fixed number of arguments.")

(=>
        (instance ?REL VariableArityRelation)
        (not
           (exists (?INT)
              (valence ?REL ?INT))))

(subclass RelationExtendedToQuantities Relation)
(instance RelationExtendedToQuantities InheritableRelation)
(documentation RelationExtendedToQuantities "A
&%RelationExtendedToQuantities is a &%Relation that, when it is true on
a sequence of arguments that are &%RealNumbers, it is also true on a
sequence of &%ConstantQuantites with those magnitudes in some unit of
measure.  For example, the &%lessThan relation is extended to quantities.
This means that for all pairs of quantities ?QUANTITY1 and ?QUANTITY2,
(lessThan ?QUANTITY1 ?QUANTITY2) if and only if, for some ?NUMBER1,
?NUMBER2, and ?UNIT, ?QUANTITY1 = (MeasureFn ?NUMBER1 ?UNIT),
?QUANTITY2 = (MeasureFn ?NUMBER2 ?UNIT), and (lessThan ?NUMBER1 ?NUMBER2),
for all units ?UNIT on which ?QUANTITY1 and ?QUANTITY2 can be measured.
Note that, when a &%RelationExtendedToQuantities is extended from
&%RealNumbers to &%ConstantQuantities, the &%ConstantQuantities must be
measured along the same physical dimension.")

(subclass Proposition Abstract)
(documentation Proposition "&%Propositions are &%Abstract entities that
express a complete thought or a set of such thoughts.  As an example,
the formula '(instance Yojo Cat)' expresses the &%Proposition that the
entity named Yojo is an element of the &%Class of Cats.  Note that
propositions are not restricted to the content expressed by individual
sentences of a &%Language.  They may encompass the content expressed by
theories, books, and even whole libraries.  It is important to distinguish
&%Propositions from the &%ContentBearingObjects that express them.  A
&%Proposition is a piece of information, e.g. that the cat is on the mat,
but a &%ContentBearingObject is an &%Object that represents this information.
A &%Proposition is an abstraction that may have multiple representations:
strings, sounds, icons, etc.  For example, the &%Proposition that the cat is
on the mat is represented here as a string of graphical characters displayed
on a monitor and/or printed on paper, but it can be represented by a sequence
of sounds or by some non-latin alphabet or by some cryptographic form")

(instance closedOn BinaryPredicate)
(instance closedOn AsymmetricRelation)
(domain closedOn 1 Function)
(domain closedOn 2 SetOrClass)
(documentation closedOn "A &%BinaryFunction is closed on a &%SetOrClass
if it is defined for all instances of the &%SetOrClass and its value is
always an instance of the &%SetOrClass.")

(=>
   (and
      (closedOn ?FUNCTION ?CLASS)
      (instance ?FUNCTION UnaryFunction))
   (forall (?INST)
      (=>
         (instance ?INST ?CLASS)
         (instance (AssignmentFn ?FUNCTION ?INST) ?CLASS))))

(=>
   (and
      (closedOn ?FUNCTION ?CLASS)
      (instance ?FUNCTION BinaryFunction))
   (forall (?INST1 ?INST2)
      (=>
         (and
            (instance ?INST1 ?CLASS)
            (instance ?INST2 ?CLASS))
         (instance (AssignmentFn ?FUNCTION ?INST1 ?INST2) ?CLASS))))

(instance reflexiveOn BinaryPredicate)
(instance reflexiveOn AsymmetricRelation)
(domain reflexiveOn 1 BinaryRelation)
(domain reflexiveOn 2 SetOrClass)
(documentation reflexiveOn "A &%BinaryRelation is reflexive on a
&%SetOrClass only if every instance of the &%SetOrClass bears the relation
to itself.")

(=>
   (reflexiveOn ?RELATION ?CLASS)
   (forall (?INST)
      (=>
         (instance ?INST ?CLASS)
         (holds ?RELATION ?INST ?INST))))

(instance irreflexiveOn BinaryPredicate)
(instance irreflexiveOn AsymmetricRelation)
(domain irreflexiveOn 1 BinaryRelation)
(domain irreflexiveOn 2 SetOrClass)
(documentation irreflexiveOn "A &%BinaryRelation is irreflexive on a
&%SetOrClass only if no instance of the &%SetOrClass bears the relation to
itself.")

(=>
   (irreflexiveOn ?RELATION ?CLASS)
   (forall (?INST)
      (=>
         (instance ?INST ?CLASS)
         (not
            (holds ?RELATION ?INST ?INST)))))

(instance partialOrderingOn BinaryPredicate)
(instance partialOrderingOn AsymmetricRelation)
(domain partialOrderingOn 1 BinaryRelation)
(domain partialOrderingOn 2 SetOrClass)
(documentation partialOrderingOn "A &%BinaryRelation is a partial
ordering on a &%SetOrClass only if the relation is &%reflexiveOn the
&%SetOrClass, and it is both an &%AntisymmetricRelation, and a
&%TransitiveRelation.")

(=>
   (partialOrderingOn ?RELATION ?CLASS)
   (and
      (reflexiveOn ?RELATION ?CLASS)
      (instance ?RELATION TransitiveRelation)
      (instance ?RELATION AntisymmetricRelation)))

(instance totalOrderingOn BinaryPredicate)
(instance totalOrderingOn AsymmetricRelation)
(domain totalOrderingOn 1 BinaryRelation)
(domain totalOrderingOn 2 SetOrClass)
(documentation totalOrderingOn "A &%BinaryRelation ?REL is a total
ordering on a &%SetOrClass only if it is a partial ordering for which either
(?REL ?INST1 ?INST2) or (?REL ?INST2 ?INST1) for every ?INST1 and ?INST2
in the &%SetOrClass.")

(<=>
     (totalOrderingOn ?RELATION ?CLASS)
     (and
          (partialOrderingOn ?RELATION ?CLASS)
          (trichotomizingOn ?RELATION ?CLASS)))

(instance trichotomizingOn BinaryPredicate)
(instance trichotomizingOn AsymmetricRelation)
(domain trichotomizingOn 1 BinaryRelation)
(domain trichotomizingOn 2 SetOrClass)
(documentation trichotomizingOn "A &%BinaryRelation ?REL is
trichotomizing on a &%SetOrClass only if, for all instances ?INST1 and ?INST2
of the &%SetOrClass, at least one of the following holds:  (?REL ?INST1 ?INST2),
(?REL ?INST2 ?INST1) or (equal ?INST1 ?INST2).")

(=>
   (trichotomizingOn ?RELATION ?CLASS)
   (forall (?INST1 ?INST2)
      (=>
         (and
            (instance ?INST1 ?CLASS)
            (instance ?INST2 ?CLASS))
         (or
            (holds ?RELATION ?INST1 ?INST2)
            (holds ?RELATION ?INST2 ?INST1)
            (equal ?INST1 ?INST2)))))

(instance equivalenceRelationOn BinaryPredicate)
(instance equivalenceRelationOn AsymmetricRelation)
(domain equivalenceRelationOn 1 BinaryRelation)
(domain equivalenceRelationOn 2 SetOrClass)
(documentation equivalenceRelationOn "A &%BinaryRelation is an
&%equivalenceRelationOn a &%SetOrClass only if the relation is &%reflexiveOn
the &%SetOrClass and it is both a &%TransitiveRelation and a
&%SymmetricRelation.")

(=>
   (equivalenceRelationOn ?RELATION ?CLASS)
   (and
      (instance ?RELATION TransitiveRelation)
      (instance ?RELATION SymmetricRelation)
      (reflexiveOn ?RELATION ?CLASS)))

(instance distributes BinaryPredicate)
(instance distributes BinaryRelation)
(domain distributes 1 BinaryFunction)
(domain distributes 2 BinaryFunction)
(documentation distributes "A &%BinaryFunction ?FUNCTION1 is
distributive over another &%BinaryFunction ?FUNCTION2 just in case
(?FUNCTION1 ?INST1 (?FUNCTION2 ?INST2 ?INST3)) is equal to
(?FUNCTION2 (?FUNCTION1 ?INST1 ?INST2) (?FUNCTION1 ?INST1 ?INST3)),
for all ?INST1, ?INST2, and ?INST3.")

(=>
   (distributes ?FUNCTION1 ?FUNCTION2)
   (forall (?INST1 ?INST2 ?INST3)
      (=>
         (and
            (domain ?FUNCTION1 1 ?CLASS1)
            (instance ?INST1 ?CLASS1)
            (instance ?INST2 ?CLASS1)
            (instance ?INST3 ?CLASS1)
            (domain ?FUNCTION2 1 ?CLASS2)
            (instance ?INST1 ?CLASS2)
            (instance ?INST2 ?CLASS2)
            (instance ?INST3 ?CLASS2))
         (equal (AssignmentFn ?FUNCTION1 ?INST1
                        (AssignmentFn ?FUNCTION2 ?INST2 ?INST3))
                (AssignmentFn ?FUNCTION2
                        (AssignmentFn ?FUNCTION1 ?INST1 ?INST2)
                        (AssignmentFn ?FUNCTION1 ?INST1 ?INST3))))))

(instance causes BinaryPredicate)
(instance causes AsymmetricRelation)
(domain causes 1 Process)
(domain causes 2 Process)
(relatedInternalConcept causes causesSubclass)
(documentation causes "The causation relation between instances of &%Process.
(&%causes ?PROCESS1 ?PROCESS2) means that the instance of &%Process ?PROCESS1
brings about the instance of &%Process ?PROCESS2, e.g. (&%causes &%Killing &%Death).")

(=>
   (instance ?PROC1 Process)
   (exists (?PROC2)
      (causes ?PROC2 ?PROC1)))

(instance causesSubclass BinaryPredicate)
(instance causesSubclass AsymmetricRelation)
(domainSubclass causesSubclass 1 Process)
(domainSubclass causesSubclass 2 Process)
(documentation causesSubclass "The causation relation between subclasses of &%Process.
(&%causesSubclass ?PROCESS1 ?PROCESS2) means that the subclass of &%Process ?PROCESS1
brings about the subclass of &%Process ?PROCESS2, e.g. (&%causes &%Killing &%Death).")

(=>
   (causesSubclass ?PROC1 ?PROC2)
   (forall (?INST2)
      (=>
         (instance ?INST2 ?PROC2)
         (exists (?INST1)
   	(and
   	   (instance ?INST1 ?PROC1)
   	   (causes ?INST1 ?INST2))))))

(instance copy BinaryPredicate)
(instance copy EquivalenceRelation)
(domain copy 1 Object)
(domain copy 2 Object)
(documentation copy "relates an &%Object to an exact copy of the
&%Object, where an exact copy is indistinguishable from the original
with regard to every property except (possibly) spatial and/or temporal
location.")

(=>
   (copy ?OBJ1 ?OBJ2)
   (forall (?ATTR)
      (=>
         (attribute ?OBJ1 ?ATTR)
         (attribute ?OBJ2 ?ATTR))))

(instance time BinaryPredicate)
(instance time TemporalRelation)
(instance time AsymmetricRelation)
(domain time 1 Physical)
(domain time 2 TimePosition)
(documentation time "This relation holds between an instance of
&%Physical and an instance of &%TimePosition just in case the temporal
lifespan of the former includes the latter.  The constants &%located
and &%time are the basic spatial and temporal predicates,
respectively.")

(instance holdsDuring BinaryPredicate)
(instance holdsDuring AsymmetricRelation)
(domain holdsDuring 1 TimePosition)
(domain holdsDuring 2 Formula)
(documentation holdsDuring "(&%holdsDuring ?TIME ?FORMULA) means that the
proposition denoted by ?FORMULA is true in the time frame ?TIME.  Note
that this implies that ?FORMULA is true at every &%TimePoint which is a
&%temporalPart of ?TIME.")

(=>
   (and
      (holdsDuring ?TIME ?SITUATION1)
      (entails ?SITUATION1 ?SITUATION2))
   (holdsDuring ?TIME ?SITUATION2))

(=>
   (holdsDuring ?TIME (not ?SITUATION))
   (not (holdsDuring ?TIME ?SITUATION)))

(instance capability TernaryPredicate)
(domainSubclass capability 1 Process)
(domain capability 2 CaseRole)
(domain capability 3 Object)
(documentation capability "(&%capability ?PROCESS ?ROLE ?OBJ) means
that ?OBJ has the ability to play the role of ?ROLE in &%Processes of
type ?PROCESS.")

(=>
   (and
      (instance ?ROLE CaseRole)
      (holds ?ROLE ?ARG1 ?ARG2)
      (instance ?ARG1 ?PROC)
      (subclass ?PROC Process))
   (capability ?PROC ?ROLE ?ARG2))

(instance exploits BinaryPredicate)
(instance exploits AsymmetricRelation)
(domain exploits 1 Object)
(domain exploits 2 Agent)
(documentation exploits "(&%exploits ?OBJ ?AGENT) means that ?OBJ is used
by ?AGENT as a &%resource in an unspecified instance of &%Process.  This
&%Predicate, as its corresponding axiom indicates, is a composition of the
relations &%agent and &%resource.")

(=>
     (exploits ?OBJ ?AGENT)
     (exists (?PROCESS)
          (and
               (agent ?PROCESS ?AGENT)
               (resource ?PROCESS ?OBJ))))

(instance hasPurpose BinaryPredicate)
(instance hasPurpose AsymmetricRelation)
(domain hasPurpose 1 Physical)
(domain hasPurpose 2 Formula)
(documentation hasPurpose "This &%Predicate expresses the concept of a
conventional goal, i.e. a goal with a neutralized agent's intention.
Accordingly, (&%hasPurpose ?THING ?FORMULA) means that the instance of
&%Physical ?THING has, as its purpose, the &%Proposition expressed by
?FORMULA.  Note that there is an important difference in meaning between
the &%Predicates &%hasPurpose and &%result.  Although the second argument
of the latter can satisfy the second argument of the former,
a conventional goal is an expected and desired outcome, while a result
may be neither expected nor desired.  For example, a machine process may
have outcomes but no goals, aimless wandering may have an outcome but no
goal; a learning process may have goals with no outcomes, and so on.")

(instance hasPurposeForAgent TernaryPredicate)
(domain hasPurposeForAgent 1 Physical)
(domain hasPurposeForAgent 2 Formula)
(domain hasPurposeForAgent 3 CognitiveAgent)
(documentation hasPurposeForAgent "Expresses a cognitive attitude of an
agent with respect to a particular instance of Physical.  More precisely,
(&%hasPurposeForAgent ?THING ?FORMULA ?AGENT) means that the purpose of
?THING for ?AGENT is the proposition expressed by ?FORMULA.  Very complex
issues are involved here.  In particular, the rules of inference of the
first order predicate calculus are not truth-preserving for the second
argument position of this &%Predicate.")

(=>
   (hasPurpose ?THING ?PURPOSE)
   (exists (?AGENT)
      (hasPurposeForAgent ?THING ?PURPOSE ?AGENT)))

(instance hasSkill BinaryPredicate)
(instance hasSkill AsymmetricRelation)
(domainSubclass hasSkill 1 Process)
(domain hasSkill 2 Agent)
(documentation hasSkill "Similar to the &%capability &%Predicate
with the additional restriction that the ability be practised/
demonstrated to some measurable degree.")

(=>
   (hasSkill ?PROC ?AGENT)
   (capability ?PROC agent ?AGENT))

(instance holdsRight BinaryPredicate)
(instance holdsRight AsymmetricRelation)
(domain holdsRight 1 Formula)
(domain holdsRight 2 CognitiveAgent)
(documentation holdsRight "Expresses a relationship between a &%Formula
and a &%CognitiveAgent whereby the &%CognitiveAgent has the right to
bring it about that the &%Formula is true.")

(instance confersRight TernaryPredicate)
(domain confersRight 1 Formula)
(domain confersRight 2 Entity)
(domain confersRight 3 CognitiveAgent)
(documentation confersRight "Expresses the relationship between a &%Formula,
an &%Entity, and a &%CognitiveAgent when the &%Entity authorizes the
&%CognitiveAgent to bring it about that the &%Formula is true.")

(=>
   (confersRight ?FORMULA ?AGENT1 ?AGENT2)
   (holdsRight ?FORMULA ?AGENT2))

(instance holdsObligation BinaryPredicate)
(instance holdsObligation AsymmetricRelation)
(domain holdsObligation 1 Formula)
(domain holdsObligation 2 CognitiveAgent)
(relatedInternalConcept holdsObligation holdsRight)
(documentation holdsObligation "Expresses a relationship between a
&%Formula and a &%CognitiveAgent whereby the &%CognitiveAgent has
the obligation to bring it about that the &%Formula is true.")

(instance confersObligation TernaryPredicate)
(domain confersObligation 1 Formula)
(domain confersObligation 2 Entity)
(domain confersObligation 3 CognitiveAgent)
(relatedInternalConcept confersObligation confersRight)
(documentation confersObligation "Expresses the relationship between a
a &%Formula, an &%Entity, and a &%CognitiveAgent when the &%Entity
obligates the &%CognitiveAgent to bring it about that the &%Formula is
true.")

(=>
   (confersObligation ?FORMULA ?AGENT1 ?AGENT2)
   (holdsObligation ?FORMULA ?AGENT2))

(instance partlyLocated SpatialRelation)
(instance partlyLocated AntisymmetricRelation)
(instance partlyLocated BinaryPredicate)
(domain partlyLocated 1 Physical)
(domain partlyLocated 2 Object)
(documentation partlyLocated "(&%partlyLocated ?THING ?OBJ) means that the
instance of &%Physical ?THING is at least partially located at ?OBJ.  For
example, Istanbul is partly located in Asia and partly located in Europe.
Note that &%partlyLocated is the most basic localization relation:  &%located
is an immediate &%subrelation of &%partlyLocated and &%exactlyLocated is
an immediate &%subrelation of &%located.")

(=>
   (and
      (instance ?OBJ1 Object)
      (partlyLocated ?OBJ1 ?OBJ2))
   (overlapsSpatially ?OBJ1 ?OBJ2))

(=>
   (and
      (instance ?OBJ1 Object)
      (partlyLocated ?OBJ1 ?OBJ2))
   (exists (?SUB)
      (and
         (part ?SUB ?OBJ1)
         (located ?SUB ?OBJ2))))

(instance located AntisymmetricRelation)
(instance located TransitiveRelation)
(subrelation located partlyLocated)
(documentation located "(&%located ?PHYS ?OBJ) means that ?PHYS is &%partlyLocated
at ?OBJ, and there is no &%part or &%subProcess of ?PHYS that is not &%located at
?OBJ.")

(=>
   (located ?OBJ1 ?OBJ2)
   (forall (?SUB)
      (=>
         (part ?SUB ?OBJ1)
         (located ?SUB ?OBJ2))))

(=>
   (located ?PROCESS ?OBJ)
   (forall (?SUB)
      (=>
         (subProcess ?SUB ?PROCESS)
         (located ?SUB ?OBJ))))

(subrelation exactlyLocated located)
(documentation exactlyLocated "The actual, minimal location of an
Object.  This is a subrelation of the more general &%Predicate
&%located.")

(=>
   (exactlyLocated ?OBJ ?REGION)
   (not
      (exists (?OTHEROBJ)
         (and
            (exactlyLocated ?OTHEROBJ ?REGION)
            (not
               (equal ?OTHEROBJ ?OBJ))))))

(instance between SpatialRelation)
(instance between TernaryPredicate)
(domain between 1 Object)
(domain between 2 Object)
(domain between 3 Object)
(documentation between "(between ?OBJ1 ?OBJ2 ?OBJ3) means that ?OBJ2 is
spatially located between ?OBJ1 and ?OBJ3.  Note that this implies that
?OBJ2 is directly between ?OBJ1 and ?OBJ3, i.e. the projections of ?OBJ1
and ?OBJ3 overlap with ?OBJ2.")

(instance traverses SpatialRelation)
(instance traverses BinaryRelation)
(domain traverses 1 Object)
(domain traverses 2 Object)
(documentation traverses "(&%traverses ?OBJ1 ?OBJ2) means that ?OBJ1
crosses or extends across ?OBJ2.  Note that &%crosses and
&%penetrates are subrelations of &%traverses.")

(=>
   (traverses ?OBJ1 ?OBJ2)
   (or
      (crosses ?OBJ1 ?OBJ2)
      (penetrates ?OBJ1 ?OBJ2)))

(subrelation crosses traverses)
(instance crosses AsymmetricRelation)
(instance crosses TransitiveRelation)
(disjointRelation crosses connected)
(documentation crosses "(crosses ?OBJ1 ?OBJ2) means that
&%Object ?OBJ1 &%traverses Object ?OBJ2, without being &%connected
to it.")

(subrelation penetrates traverses)
(subrelation penetrates meetsSpatially)
(instance penetrates AsymmetricRelation)
(instance penetrates IntransitiveRelation)
(documentation penetrates "(penetrates ?OBJ1 ?OBJ2) means that
?OBJ1 is &%connected to ?OBJ2 along at least one whole dimension (length,
width or depth).")

(instance WhereFn BinaryFunction)
(instance WhereFn SpatialRelation)
(instance WhereFn TotalValuedRelation)
(domain WhereFn 1 Physical)
(domain WhereFn 2 TimePoint)
(range WhereFn Region)
(relatedInternalConcept WhereFn WhenFn)
(documentation WhereFn "Maps an &%Object and a &%TimePoint at which the
&%Object exists to the &%Region where the &%Object existed at that
&%TimePoint.")

(<=>
   (equal (WhereFn ?THING ?TIME) ?REGION)
   (holdsDuring ?TIME (exactlyLocated ?THING ?REGION)))

(instance possesses BinaryPredicate)
(instance possesses AsymmetricRelation)
(domain possesses 1 Agent)
(domain possesses 2 Object)
(documentation possesses "&%Relation that holds between an &%Agent and
an &%Object when the &%Agent has ownership of the &%Object.")

(=>
   (possesses ?PERSON ?OBJ)
   (holdsRight (uses ?OBJ ?PERSON) ?PERSON))

(=>
   (and
      (instance ?TIME TimePosition)
      (holdsDuring ?TIME (possesses ?AGENT1 ?OBJ))
      (holdsDuring ?TIME (possesses ?AGENT2 ?OBJ)))
   (equal ?AGENT1 ?AGENT2))

(instance PropertyFn UnaryFunction)
(instance PropertyFn TotalValuedRelation)
(domain PropertyFn 1 Agent)
(range PropertyFn Set)
(documentation PropertyFn "A &%UnaryFunction that maps an &%Agent to the &%Set of &%Property owned by the &%Agent.")

(<=>
   (instance ?OBJ (PropertyFn ?PERSON))
   (possesses ?PERSON ?OBJ))

(instance precondition BinaryPredicate)
(instance precondition AsymmetricRelation)
(instance precondition TransitiveRelation)
(domainSubclass precondition 1 Process)
(domainSubclass precondition 2 Process)
(documentation precondition "A very general &%Predicate.  (&%precondition
?PROC1 ?PROC2) means that an instance of ?PROC2 can exist only if an
instance of ?PROC1 also exists.")

(=>
   (precondition ?PROC1 ?PROC2)
   (=>
      (exists (?INST2) (instance ?INST2 ?PROC2))
      (exists (?INST1) (instance ?INST1 ?PROC1))))

(instance inhibits BinaryPredicate)
(instance inhibits IrreflexiveRelation)
(domainSubclass inhibits 1 Process)
(domainSubclass inhibits 2 Process)
(documentation inhibits "A very general &%Predicate.  (&%inhibits
?PROC1 ?PROC2) means that the &%Process ?PROC1 inhibits or hinders
the occurrence of the &%Process ?PROC2.  For example, obstructing an
object inhibits moving it.  Note that this is a relation between types
of &%Processes, not between instances.")

(=>
   (inhibits ?PROC1 ?PROC2)
   (forall (?TIME ?PLACE)
      (decreasesLikelihood
         (holdsDuring ?TIME (exists (?INST1) (and (instance ?INST1 ?PROC1) (located ?INST1 ?PLACE))))
            (holdsDuring ?TIME (exists (?INST2) (and (instance ?INST2 ?PROC2) (located ?INST2 ?PLACE)))))))

(instance prevents BinaryPredicate)
(instance prevents IrreflexiveRelation)
(domainSubclass prevents 1 Process)
(domainSubclass prevents 2 Process)
(relatedInternalConcept prevents inhibits)
(documentation prevents "A very general &%Predicate.  (&%prevents ?PROC1
?PROC2) means that ?PROC1 prevents the occurrence of ?PROC2.  In other
words, if ?PROC1 is occurring in a particular time and place, ?PROC2
cannot occur at the same time and place.  For example, innoculating
prevents contracting disease.  Note that this is a relation between types
of &%Processes, not between instances.")

(=>
   (prevents ?PROC1 ?PROC2)
   (forall (?TIME ?PLACE)
      (=>
         (holdsDuring ?TIME (exists (?INST1) (and (instance ?INST1 ?PROC1) (located ?INST1 ?PLACE))))
         (not (holdsDuring ?TIME (exists (?INST2) (and (instance ?INST2 ?PROC2) (located ?INST2 ?PLACE))))))))

(instance refers BinaryPredicate)
(domain refers 1 Physical)
(domain refers 2 Entity)
(documentation refers "(&%refers ?OBJ1 ?OBJ2) means that ?OBJ1
mentions or includes a reference to ?OBJ2. Note that &%refers is
more general in meaning than &%represents, because presumably something
can represent something else only if it refers to this other thing.
For example, an article whose topic is a recent change in the price of
oil may refer to many other things, e.g. the general state of the economy,
the weather in California, the prospect of global warming, the options
for alternative energy sources, the stock prices of various oil companies,
etc.")

(subrelation names refers)
(domain names 1 SymbolicString)
(documentation names "(&%names ?STRING ?ENTITY) means that the thing ?ENTITY
has the &%SymbolicString ?STRING as its name.  Note that &%names and &%represents
are the two immediate &%subrelations of &%refers.  The predicate &%names is used
when the referring item is merely a tag without connotative content, while the
predicate &%represents is used for referring items that have such content.")

(subrelation uniqueIdentifier names)
(instance uniqueIdentifier SingleValuedRelation)
(documentation uniqueIdentifier "The class of &%names that uniquely identify
an instance of &%Entity.  Some examples of &%uniqueIdentifiers are the keys
of tables in database applications and the ISBN (International Standard Book
Number).")

(subrelation represents refers)
(documentation represents "A very general semiotics &%Predicate.
(&%represents ?THING ?ENTITY) means that ?THING in some way indicates,
expresses, connotes, pictures, describes, etc. ?ENTITY.  The &%Predicates
&%containsInformation and &%realization are subrelations of &%represents.
Note that &%represents is a subrelation of &%refers, since something can
represent something else only if it refers to this other thing.  See the
documentation string for &%names.")

(instance representsForAgent TernaryPredicate)
(domain representsForAgent 1 Physical)
(domain representsForAgent 2 Entity)
(domain representsForAgent 3 Agent)
(documentation representsForAgent "A very general predicate.
(&%representsForAgent ?THING ?ENTITY ?AGENT) means that the ?AGENT
chooses to use the &%instance of &%Physical ?THING to 'stand for'
?ENTITY.")

(=>
   (representsForAgent ?REP ?ENTITY ?AGENT)
   (represents ?REP ?ENTITY))

(instance representsInLanguage TernaryPredicate)
(domain representsInLanguage 1 LinguisticExpression)
(domain representsInLanguage 2 Entity)
(domain representsInLanguage 3 Language)
(documentation representsInLanguage "A very general predicate.
(&%representsInLanguage ?THING ?ENTITY ?LANGUAGE) means that the
&%LinguisticExpression ?THING stands for ?ENTITY in the &%Language
?LANGUAGE.")

(=>
   (representsInLanguage ?REP ?ENTITY ?LANGUAGE)
   (exists (?AGENT)
      (representsForAgent ?REP ?ENTITY ?AGENT)))

(subrelation equivalentContentClass subsumesContentClass)
(instance equivalentContentClass EquivalenceRelation)
(domainSubclass equivalentContentClass 1 ContentBearingObject)
(domainSubclass equivalentContentClass 2 ContentBearingObject)
(documentation equivalentContentClass "A &%BinaryPredicate that relates two
subclasses of &%ContentBearingObject.  (&%equivalentContentClass ?CLASS1
?CLASS2) means that the content expressed by each instance of ?CLASS1 is
also expressed by each instance of ?CLASS2, and vice versa.  An example
would be the relationship between English and Russian editions of Agatha
Christie's 'Murder on the Orient Express'.  Note that
(&%equivalentContentClass ?CLASS1 ?CLASS2) implies (&%subsumesContentClass
?CLASS1 ?CLASS2) and (&%subsumesContentClass ?CLASS2 ?CLASS1).")

(<=>
   (and
      (subsumesContentClass ?CLASS1 ?CLASS2)
      (subsumesContentClass ?CLASS2 ?CLASS1))
   (equivalentContentClass ?CLASS1 ?CLASS2))

(instance subsumesContentClass BinaryPredicate)
(instance subsumesContentClass PartialOrderingRelation)
(domainSubclass subsumesContentClass 1 ContentBearingObject)
(domainSubclass subsumesContentClass 2 ContentBearingObject)
(documentation subsumesContentClass "A &%BinaryPredicate that relates two
subclasses of &%ContentBearingObject.  (&%subsumesContentClass ?CLASS1
?CLASS2) means that the content expressed by each instance of ?CLASS2 is
also expressed by each instance of ?CLASS1.  Examples include the
relationship between a poem and one of its stanzas or between a book and
one of its chapters.  Note that this is a relation between subclasses of
&%ContentBearingObject, rather than instances.  If one wants to relate
instances, the &%Predicate &%subsumesContentInstance can be used.  Note
that &%subsumesContentClass is needed in many cases.  Consider, for
example, the relation between the King James edition of the Bible and its
Book of Genesis.  This relation holds for every copy of this edition and
not just for a single instance.")

(=>
  (subsumesContentClass ?CLASS1 ?CLASS2)
  (forall (?OBJ2 ?INFO)
    (=>
       (and
          (instance ?OBJ2 ?CLASS2)
          (containsInformation ?OBJ2 ?INFO))
          (exists (?OBJ1)
            (and
              (instance ?OBJ1 ?CLASS1)
              (containsInformation ?OBJ1 ?INFO))))))

(subrelation equivalentContentInstance subsumesContentInstance)
(instance equivalentContentInstance EquivalenceRelation)
(domain equivalentContentInstance 1 ContentBearingObject)
(domain equivalentContentInstance 2 ContentBearingObject)
(relatedInternalConcept equivalentContentInstance equivalentContentClass)
(documentation equivalentContentInstance "A &%BinaryPredicate relating two
instances of &%ContentBearingObject.  (&%equivalentContentInstance
?OBJ1 ?OBJ2) means that the content expressed by ?OBJ1 is identical to
the content expressed by ?OBJ2.  An example would be the relationship
between a handwritten draft of a letter to one's lawyer and a typed
copy of the same letter.  Note that (&%equivalentContentInstance ?OBJ1
?OBJ2) implies (&%subsumesContentInstance ?OBJ1 ?OBJ2) and
(&%subsumesContentInstance ?OBJ2 ?OBJ2).")

(<=>
   (and
      (subsumesContentInstance ?OBJ1 ?OBJ2)
      (subsumesContentInstance ?OBJ2 ?OBJ1))
   (equivalentContentInstance ?OBJ1 ?OBJ2))

(instance subsumesContentInstance BinaryPredicate)
(instance subsumesContentInstance PartialOrderingRelation)
(domain subsumesContentInstance 1 ContentBearingObject)
(domain subsumesContentInstance 2 ContentBearingObject)
(relatedInternalConcept subsumesContentInstance subsumesContentClass)
(documentation subsumesContentInstance "A &%BinaryPredicate relating two
instances of &%ContentBearingObject.  (&%subsumesContentInstance ?OBJ1 ?OBJ2)
means that the content expressed by ?OBJ2 is part of the content expressed
by ?OBJ1.  An example is the relationship between a handwritten poem and
one of its stanzas.  Note that this is a relation between instances,
rather than &%Classes.  If one wants to assert a content relationship
between &%Classes, e.g. between the version of an intellectual work and a
part of that work, the relation &%subsumesContentClass should be used.")

(=>
   (subsumesContentInstance ?OBJ1 ?OBJ2)
   (forall (?INFO)
      (=>
         (containsInformation ?OBJ2 ?INFO)
         (containsInformation ?OBJ1 ?INFO))))

(subrelation realization represents)
(instance realization AsymmetricRelation)
(domain realization 1 Process)
(domain realization 2 Proposition)
(relatedInternalConcept realization equivalentContentInstance)
(relatedInternalConcept realization containsInformation)
(documentation realization "A subrelation of &%represents.
(&%realization ?PROCESS ?PROP) means that ?PROCESS is a Process which
expresses the content of ?PROP. Examples include a particular musical
performance, which realizes the content of a musical score, or the
reading of a poem.")

(=>
   (realization ?PROCESS ?PROP)
   (exists (?OBJ)
      (and
         (instance ?OBJ ContentBearingObject)
         (containsInformation ?OBJ ?PROP))))

(instance expressedInLanguage BinaryPredicate)
(instance expressedInLanguage AsymmetricRelation)
(domain expressedInLanguage 1 LinguisticExpression)
(domain expressedInLanguage 2 Language)
(documentation expressedInLanguage "(&%expressedInLanguage ?EXPRESS ?LANG)
means that ?EXPRESS is expressed in &%Language ?LANG.")

(<=>
   (expressedInLanguage ?EXPRESS ?LANGUAGE)
   (exists (?PROP)
      (representsInLanguage ?EXPRESS ?PROP ?LANGUAGE)))

(instance subProposition BinaryPredicate)
(instance subProposition TransitiveRelation)
(instance subProposition IrreflexiveRelation)
(domain subProposition 1 Proposition)
(domain subProposition 2 Proposition)
(documentation subProposition "(&%subProposition ?PROP1 ?PROP2) means that
?PROP1 is a &%Proposition which is a proper part of the &%Proposition ?PROP2.
In other words, &%subProposition is the analogue of &%properPart for chunks
of abstract content.")

(=>
   (subProposition ?PROP1 ?PROP2)
   (forall (?OBJ1 ?OBJ2)
      (=>
         (and
            (containsInformation ?OBJ1 ?PROP1)
            (containsInformation ?OBJ2 ?PROP2))
         (subsumesContentInstance ?OBJ2 ?OBJ1))))

(subrelation subPlan subProposition)
(instance subPlan TransitiveRelation)
(instance subPlan IrreflexiveRelation)
(domain subPlan 1 Plan)
(domain subPlan 2 Plan)
(documentation subPlan "(&%subPlan ?PLAN1 ?PLAN2) means that ?PLAN1
is a &%Plan which is a proper part of ?PLAN2.  This relation is generally
used to relate a supporting &%Plan to the overall &%Plan in a particular
context.")

(instance uses BinaryPredicate)
(instance uses AsymmetricRelation)
(domain uses 1 Object)
(domain uses 2 Agent)
(documentation uses "(&%uses ?OBJECT AGENT) means that ?OBJECT is used by
?AGENT as an instrument in an unspecified &%Process.  This &%Predicate,
as its corresponding axiom indicates, is a composition of the &%CaseRoles
&%agent and &%instrument.")

(=>
     (uses ?OBJ ?AGENT)
     (exists (?PROC)
          (and
               (agent ?PROC ?AGENT)
               (instrument ?PROC ?OBJ))))

;; END FILE

;; <module>NUMERIC_FUNCTIONS</module>
;; BEGIN FILE


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  NUMERIC FUNCTIONS   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; INCLUDES 'STRUCTURAL ONTOLOGY'
;; INCLUDES 'BASE ONTOLOGY'

(instance MultiplicationFn BinaryFunction)
(instance MultiplicationFn AssociativeFunction)
(instance MultiplicationFn CommutativeFunction)
(instance MultiplicationFn RelationExtendedToQuantities)
(instance MultiplicationFn TotalValuedRelation)
(domain MultiplicationFn 1 Quantity)
(domain MultiplicationFn 2 Quantity)
(range MultiplicationFn Quantity)
(identityElement MultiplicationFn 1)
(documentation MultiplicationFn "If ?NUMBER1 and ?NUMBER2 are &%Numbers,
then (&%MultiplicationFn ?NUMBER1 ?NUMBER2) is the arithmetical product
of these numbers.")

(instance AdditionFn BinaryFunction)
(instance AdditionFn AssociativeFunction)
(instance AdditionFn CommutativeFunction)
(instance AdditionFn RelationExtendedToQuantities)
(instance AdditionFn TotalValuedRelation)
(domain AdditionFn 1 Quantity)
(domain AdditionFn 2 Quantity)
(range AdditionFn Quantity)
(identityElement AdditionFn 0)
(documentation AdditionFn "If ?NUMBER1 and ?NUMBER2 are &%Numbers, then
(&%AdditionFn ?NUMBER1 ?NUMBER2) is the arithmetical sum of these
numbers.")

(equal (SuccessorFn ?NUMBER) (AdditionFn ?NUMBER 1))

(instance SubtractionFn BinaryFunction)
(instance SubtractionFn AssociativeFunction)
(instance SubtractionFn RelationExtendedToQuantities)
(instance SubtractionFn TotalValuedRelation)
(domain SubtractionFn 1 Quantity)
(domain SubtractionFn 2 Quantity)
(range SubtractionFn Quantity)
(identityElement SubtractionFn 0)
(documentation SubtractionFn "If ?NUMBER1 and ?NUMBER2 are &%Numbers,
then (&%SubtractionFn ?NUMBER1 ?NUMBER2) is the arithmetical difference
between ?NUMBER1 and ?NUMBER2, i.e. ?NUMBER1 minus ?NUMBER2.  An
exception occurs when ?NUMBER1 is equal to 0, in which case
(&%SubtractionFn ?NUMBER1 ?NUMBER2) is the negation of ?NUMBER2.")

(equal (PredecessorFn ?NUMBER) (SubtractionFn ?NUMBER 1))

(instance DivisionFn BinaryFunction)
(instance DivisionFn AssociativeFunction)
(instance DivisionFn RelationExtendedToQuantities)
(instance DivisionFn PartialValuedRelation)
(domain DivisionFn 1 Quantity)
(domain DivisionFn 2 Quantity)
(range DivisionFn Quantity)
(identityElement DivisionFn 1)
(documentation DivisionFn "If ?NUMBER1 and ?NUMBER2 are &%Numbers, then
(&%DivisionFn ?NUMBER1 ?NUMBER2) is the result of dividing ?NUMBER1 by
?NUMBER2.  An exception occurs when ?NUMBER1 = 1, in which case
(&%DivisionFn ?NUMBER1 ?NUMBER2) is the reciprocal of ?NUMBER2.")

(=>
   (instance ?NUMBER RationalNumber)
   (exists (?INT1 ?INT2)
      (and
         (instance ?INT1 Integer)
         (instance ?INT2 Integer)
         (equal ?NUMBER (DivisionFn ?INT1 ?INT2)))))

(instance AbsoluteValueFn UnaryFunction)
(instance AbsoluteValueFn TotalValuedRelation)
(domain AbsoluteValueFn 1 RealNumber)
(range AbsoluteValueFn NonnegativeRealNumber)
(documentation AbsoluteValueFn "The value of (&%AbsoluteValueFn ?NUMBER)
is the absolute value of the &%RealNumber ?NUMBER.")

(<=>
     (and
         (equal (AbsoluteValueFn ?NUMBER1) ?NUMBER2)
         (instance ?NUMBER1 RealNumber)
         (instance ?NUMBER2 RealNumber))
     (or
         (and
              (instance ?NUMBER1 NonnegativeRealNumber)
              (equal ?NUMBER1 ?NUMBER2))
         (and
              (instance ?NUMBER1 NegativeRealNumber)
              (equal ?NUMBER2 (SubtractionFn 0 ?NUMBER1)))))

(instance CeilingFn UnaryFunction)
(instance CeilingFn TotalValuedRelation)
(domain CeilingFn 1 RealNumber)
(range CeilingFn Integer)
(documentation CeilingFn "(&%CeilingFn ?NUMBER) returns the smallest
&%Integer greater than or equal to the &%RealNumber ?NUMBER.")

(=>
   (equal (CeilingFn ?NUMBER) ?INT)
   (not
      (exists (?OTHERINT)
         (and
            (instance ?OTHERINT Integer)
            (greaterThanOrEqualTo ?OTHERINT ?NUMBER)
            (lessThan ?OTHERINT ?INT)))))

(instance CosineFn UnaryFunction)
(instance CosineFn TotalValuedRelation)
(domain CosineFn 1 PlaneAngleMeasure)
(range CosineFn RealNumber)
(documentation CosineFn "(&%CosineFn ?DEGREE) returns the cosine of the
&%PlaneAngleMeasure ?DEGREE.  The cosine of ?DEGREE is the ratio of the
side next to ?DEGREE to the hypotenuse in a right-angled triangle.")

(instance DenominatorFn UnaryFunction)
(instance DenominatorFn TotalValuedRelation)
(domain DenominatorFn 1 RealNumber)
(range DenominatorFn Integer)
(documentation DenominatorFn "(&%DenominatorFn ?NUMBER) returns the
denominator of the canonical reduced form of the &%RealNumber ?NUMBER.")

(instance ExponentiationFn BinaryFunction)
(instance ExponentiationFn RelationExtendedToQuantities)
(instance ExponentiationFn TotalValuedRelation)
(domain ExponentiationFn 1 Quantity)
(domain ExponentiationFn 2 Integer)
(range ExponentiationFn Quantity)
(documentation ExponentiationFn "(&%ExponentiationFn ?NUMBER ?INT) returns
the &%RealNumber ?NUMBER raised to the power of the &%Integer ?INT.")

(instance FloorFn UnaryFunction)
(instance FloorFn TotalValuedRelation)
(domain FloorFn 1 RealNumber)
(range FloorFn Integer)
(documentation FloorFn "(&%FloorFn ?NUMBER) returns the largest &%Integer
less than or equal to the &%RealNumber ?NUMBER.")

(=>
   (equal (FloorFn ?NUMBER) ?INT)
   (not
      (exists (?OTHERINT)
         (and
            (instance ?OTHERINT Integer)
            (lessThanOrEqualTo ?OTHERINT ?NUMBER)
            (greaterThan ?OTHERINT ?INT)))))

(instance GreatestCommonDivisorFn Function)
(instance GreatestCommonDivisorFn VariableArityRelation)
(instance GreatestCommonDivisorFn PartialValuedRelation)
(domain GreatestCommonDivisorFn 1 Integer)
(range GreatestCommonDivisorFn Integer)
(documentation GreatestCommonDivisorFn "(&%GreatestCommonDivisorFn
?NUMBER1 ?NUMBER2 ... ?NUMBER) returns the greatest common divisor of
?NUMBER1 through ?NUMBER.")

(=>
   (equal (GreatestCommonDivisorFn @ROW) ?NUMBER)
   (forall (?ELEMENT)
      (=>
         (inList ?ELEMENT (ListFn @ROW))
         (equal (RemainderFn ?ELEMENT ?NUMBER) 0))))

(=>
   (equal (GreatestCommonDivisorFn @ROW) ?NUMBER)
   (not (exists (?GREATER)
      (and
         (greaterThan ?GREATER ?NUMBER)
         (forall (?ELEMENT)
            (=>
               (inList ?ELEMENT (ListFn @ROW))
               (equal (RemainderFn ?ELEMENT ?GREATER) 0)))))))

(instance ImaginaryPartFn UnaryFunction)
(instance ImaginaryPartFn TotalValuedRelation)
(domain ImaginaryPartFn 1 ComplexNumber)
(range ImaginaryPartFn ImaginaryNumber)
(documentation ImaginaryPartFn "(&%ImaginaryPartFn ?NUMBER) returns
the part of ?NUMBER that has the square root of -1 as its factor.")

(=>
   (instance ?NUMBER ComplexNumber)
   (exists (?PART1 ?PART2)
      (and
         (equal ?PART1 (RealNumberFn ?NUMBER))
         (equal ?PART2 (ImaginaryPartFn ?NUMBER)))))

(instance IntegerSquareRootFn UnaryFunction)
(instance IntegerSquareRootFn PartialValuedRelation)
(domain IntegerSquareRootFn 1 RealNumber)
(range IntegerSquareRootFn NonnegativeInteger)
(documentation IntegerSquareRootFn "(&%IntegerSquareRootFn ?NUMBER)
returns the integer square root of ?NUMBER.")

(instance LeastCommonMultipleFn Function)
(instance LeastCommonMultipleFn PartialValuedRelation)
(instance LeastCommonMultipleFn VariableArityRelation)
(domain LeastCommonMultipleFn 1 Integer)
(range LeastCommonMultipleFn Integer)
(documentation LeastCommonMultipleFn "(&%LeastCommonMultipleFn
?NUMBER1 ?NUMBER2 ... ?NUMBER) returns the least common multiple of
?NUMBER1 through ?NUMBER.")

(=>
   (equal (LeastCommonMultipleFn @ROW) ?NUMBER)
   (forall (?ELEMENT)
      (=>
         (inList ?ELEMENT (ListFn @ROW))
         (equal (RemainderFn ?NUMBER ?ELEMENT) 0))))

(=>
   (equal (LeastCommonMultipleFn @ROW) ?NUMBER)
   (not (exists (?LESS)
      (and
         (lessThan ?LESS ?NUMBER)
         (forall (?ELEMENT)
            (=>
               (inList ?ELEMENT (ListFn @ROW))
               (equal (RemainderFn ?LESS ?ELEMENT) 0)))))))

(instance LogFn BinaryFunction)
(domain LogFn 1 RealNumber)
(domain LogFn 2 PositiveInteger)
(range LogFn RealNumber)
(documentation LogFn "(LogFn ?NUMBER ?INT) returns the logarithm of the
&%RealNumber ?NUMBER in the base denoted by the &%Integer ?INT.")

(instance MaxFn BinaryFunction)
(instance MaxFn AssociativeFunction)
(instance MaxFn CommutativeFunction)
(instance MaxFn RelationExtendedToQuantities)
(instance MaxFn TotalValuedRelation)
(domain MaxFn 1 Quantity)
(domain MaxFn 2 Quantity)
(range MaxFn Quantity)
(documentation MaxFn "(&%MaxFn ?NUMBER1 ?NUMBER2) is the largest of
?NUMBER1 and ?NUMBER2.  In cases where ?NUMBER1 is equal to ?NUMBER2,
&%MaxFn returns one of its arguments.")

(=>
   (equal (MaxFn ?NUMBER1 ?NUMBER2) ?NUMBER)
   (or
      (and
         (equal ?NUMBER ?NUMBER1)
         (greaterThan ?NUMBER1 ?NUMBER2))
      (and
         (equal ?NUMBER ?NUMBER2)
         (greaterThan ?NUMBER2 ?NUMBER1))
      (and
         (equal ?NUMBER ?NUMBER1)
         (equal ?NUMBER ?NUMBER2))))

(instance MinFn BinaryFunction)
(instance MinFn AssociativeFunction)
(instance MinFn CommutativeFunction)
(instance MinFn RelationExtendedToQuantities)
(instance MinFn TotalValuedRelation)
(domain MinFn 1 Quantity)
(domain MinFn 2 Quantity)
(range MinFn Quantity)
(documentation MinFn "(&%MinFn ?NUMBER1 ?NUMBER2) is the smallest of
?NUMBER1 and ?NUMBER2.  In cases where ?NUMBER1 is equal to ?NUMBER2,
&%MinFn returns one of its arguments.")

(=>
   (equal (MinFn ?NUMBER1 ?NUMBER2) ?NUMBER)
   (or
      (and
         (equal ?NUMBER ?NUMBER1)
         (lessThan ?NUMBER1 ?NUMBER2))
      (and
         (equal ?NUMBER ?NUMBER2)
         (lessThan ?NUMBER2 ?NUMBER1))
      (and
         (equal ?NUMBER ?NUMBER1)
         (equal ?NUMBER ?NUMBER2))))

(instance NumeratorFn UnaryFunction)
(instance NumeratorFn TotalValuedRelation)
(domain NumeratorFn 1 RealNumber)
(range NumeratorFn Integer)
(documentation NumeratorFn "(&%NumeratorFn ?NUMBER) returns the numerator
of the canonical reduced form ?NUMBER.")

(instance Pi PositiveRealNumber)
(documentation Pi "&%Pi is the &%RealNumber that
is the ratio of the perimeter of a circle to its diameter.  It is
approximately equal to 3.141592653589793.")

(instance NumberE PositiveRealNumber)
(documentation NumberE "&%NumberE is the &%RealNumber that is the base for
natural logarithms.  It is approximately equal to 2.718282.")

(instance RationalNumberFn UnaryFunction)
(domain RationalNumberFn 1 Number)
(range RationalNumberFn RationalNumber)
(documentation RationalNumberFn "(&%RationalNumberFn ?NUMBER) returns
the rational representation of ?NUMBER.")

(instance RealNumberFn UnaryFunction)
(domain RealNumberFn 1 Number)
(range RealNumberFn RealNumber)
(documentation RealNumberFn "(RealNumberFn ?NUMBER) returns the part of
?NUMBER that is a &%RealNumber.")

(instance ReciprocalFn UnaryFunction)
(instance ReciprocalFn RelationExtendedToQuantities)
(instance ReciprocalFn TotalValuedRelation)
(domain ReciprocalFn 1 Quantity)
(range ReciprocalFn Quantity)
(documentation ReciprocalFn "(ReciprocalFn ?NUMBER) is the reciprocal
element of ?NUMBER with respect to the multiplication operator
(&%MultiplicationFn), i.e. 1/?NUMBER.  Not all numbers have a reciprocal
element.  For example the number 0 does not.  If a number ?NUMBER has a
reciprocal ?RECIP, then the product of ?NUMBER and ?RECIP will be
1, e.g. 3*1/3 = 1.  The reciprocal of an element is &%equal to
applying the &%ExponentiationFn function to the element to the power
-1.")

(=>
   (instance ?NUMBER Quantity)
   (equal (ReciprocalFn ?NUMBER) (ExponentiationFn ?NUMBER -1)))

(=>
   (instance ?NUMBER Quantity)
   (equal 1 (MultiplicationFn ?NUMBER (ReciprocalFn ?NUMBER))))

(instance RemainderFn BinaryFunction)
(instance RemainderFn RelationExtendedToQuantities)
(instance RemainderFn PartialValuedRelation)
(domain RemainderFn 1 Quantity)
(domain RemainderFn 2 Quantity)
(range RemainderFn Quantity)
(documentation RemainderFn "(RemainderFn ?NUMBER ?DIVISOR) is the
remainder of the number ?NUMBER divided by the number ?DIVISOR.
The result has the same sign as ?DIVISOR.")

(<=>
   (equal (RemainderFn ?NUMBER1 ?NUMBER2) ?NUMBER)
   (equal (AdditionFn (MultiplicationFn (FloorFn (DivisionFn ?NUMBER1 ?NUMBER2)) ?NUMBER2) ?NUMBER) ?NUMBER1))

(=>
   (equal (RemainderFn ?NUMBER1 ?NUMBER2) ?NUMBER)
   (equal (SignumFn ?NUMBER2) (SignumFn ?NUMBER)))


(=>
   (instance ?NUMBER EvenInteger)
   (equal (RemainderFn ?NUMBER 2) 0))

(=>
   (instance ?NUMBER OddInteger)
   (equal (RemainderFn ?NUMBER 2) 1))

(=>
   (instance ?PRIME PrimeNumber)
   (forall (?NUMBER)
      (=>
         (equal (RemainderFn ?PRIME ?NUMBER) 0)
         (or
            (equal ?NUMBER 1)
            (equal ?NUMBER ?PRIME)))))

(instance RoundFn UnaryFunction)
(instance RoundFn RelationExtendedToQuantities)
(instance RoundFn TotalValuedRelation)
(domain RoundFn 1 Quantity)
(range RoundFn Quantity)
(documentation RoundFn "(&%RoundFn ?NUMBER) is the &%Integer closest
to ?NUMBER on the number line.  If ?NUMBER is halfway between two
&%Integers (for example 3.5), it denotes the larger &%Integer.")

(=>
   (equal (RoundFn ?NUMBER1) ?NUMBER2)
   (or
      (=>
         (lessThan (SubtractionFn ?NUMBER1 (FloorFn ?NUMBER1)) 0.5)
         (equal ?NUMBER2 (FloorFn ?NUMBER1)))
      (=>
         (greaterThanOrEqualTo (SubtractionFn ?NUMBER1 (FloorFn ?NUMBER1)) 0.5)
         (equal ?NUMBER2 (CeilingFn ?NUMBER1)))))

(instance SignumFn UnaryFunction)
(instance SignumFn TotalValuedRelation)
(domain SignumFn 1 RealNumber)
(range SignumFn Integer)
(documentation SignumFn "(SignumFn ?NUMBER) denotes the sign of ?NUMBER.
This is one of the following values:  -1, 1, or 0.")

(=>
   (instance ?NUMBER NonnegativeRealNumber)
   (or
      (equal (SignumFn ?NUMBER) 1)
      (equal (SignumFn ?NUMBER) 0)))

(=>
   (instance ?NUMBER PositiveRealNumber)
   (equal (SignumFn ?NUMBER) 1))

(=>
   (instance ?NUMBER NegativeRealNumber)
   (equal (SignumFn ?NUMBER) -1))

(instance SineFn UnaryFunction)
(instance SineFn TotalValuedRelation)
(domain SineFn 1 PlaneAngleMeasure)
(range SineFn RealNumber)
(documentation SineFn "(&%SineFn ?DEGREE) is the sine of the
&%PlaneAngleMeasure ?DEGREE.  The sine of ?DEGREE is the ratio of the side
opposite ?DEGREE to the hypotenuse in a right-angled triangle.")

(instance SquareRootFn UnaryFunction)
(domain SquareRootFn 1 RealNumber)
(range SquareRootFn Number)
(documentation SquareRootFn "(SquareRootFn ?NUMBER) is the principal
square root of ?NUMBER.")

(=>
   (equal (SquareRootFn ?NUMBER1) ?NUMBER2)
   (equal (MultiplicationFn ?NUMBER2 ?NUMBER2) ?NUMBER1))

(instance tangent BinaryPredicate)
(domain tangent 1 OneDimensionalFigure)
(domain tangent 2 Circle)
(documentation tangent "(&%tangent ?LINE ?CIRCLE) means that the straight line ?LINE is tangent to the &%Circle ?CIRCLE, i.e. ?LINE touches ?CIRCLE without intersecting it.")

(=>
   (tangent ?LINE ?CIRCLE)
   (exists (?POINT1)
      (and
         (pointOfFigure ?POINT1 ?LINE)
         (pointOfFigure ?POINT1 ?CIRCLE)
         (forall (?POINT2)
            (=>
               (and
                  (pointOfFigure ?POINT2 ?LINE)
                  (pointOfFigure ?POINT2 ?CIRCLE))
               (equal ?POINT1 ?POINT2))))))

(instance TangentFn UnaryFunction)
(instance TangentFn TotalValuedRelation)
(domain TangentFn 1 PlaneAngleMeasure)
(range TangentFn RealNumber)
(documentation TangentFn "(&%TangentFn ?DEGREE) is the tangent of the
&%PlaneAngleMeasure ?DEGREE.  The tangent of ?DEGREE is the ratio of
the side opposite ?DEGREE to the side next to ?DEGREE in a right-angled
triangle.")

(=>
   (instance ?DEGREE PlaneAngleMeasure)
   (equal (TangentFn ?DEGREE) (DivisionFn (SineFn ?DEGREE) (CosineFn ?DEGREE))))

(instance identityElement BinaryPredicate)
(instance identityElement AsymmetricRelation)
(domain identityElement 1 BinaryFunction)
(domain identityElement 2 Entity)
(documentation identityElement "An object ?ID is the identity element
for BinaryFunction ?FUNCTION just in case, for every instance ?INST,
applying ?FUNCTION to ?INST and ?ID results in ?INST.")

(=>
   (identityElement ?FUNCTION ?ID)
   (forall (?INST)
      (=>
         (and
            (domain ?FUNCTION 1 ?CLASS)
            (instance ?INST ?CLASS))
         (equal (AssignmentFn ?FUNCTION ?ID ?INST) ?INST))))

(instance SuccessorFn UnaryFunction)
(instance SuccessorFn TotalValuedRelation)
(domain SuccessorFn 1 Integer)
(range SuccessorFn Integer)
(documentation SuccessorFn "A &%UnaryFunction that maps an &%Integer to
its successor, e.g. the successor of 5 is 6.")

(=>
   (equal (SuccessorFn ?INT1) (SuccessorFn ?INT2))
   (equal ?INT1 ?INT2))

(=>
   (instance ?INT Integer)
   (lessThan ?INT (SuccessorFn ?INT)))

(=>
   (and
      (instance ?INT1 Integer)
      (instance ?INT2 Integer))
   (not
      (and
         (lessThan ?INT1 ?INT2)
         (lessThan ?INT2 (SuccessorFn ?INT1)))))

(=>
   (instance ?INT Integer)
   (equal ?INT (SuccessorFn (PredecessorFn ?INT))))

(=>
   (instance ?INT Integer)
   (equal ?INT (PredecessorFn (SuccessorFn ?INT))))

(instance PredecessorFn UnaryFunction)
(instance PredecessorFn TotalValuedRelation)
(domain PredecessorFn 1 Integer)
(range PredecessorFn Integer)
(documentation PredecessorFn "A &%UnaryFunction that maps an &%Integer to
its predecessor, e.g. the predecessor of 5 is 4.")

(=>
   (equal (PredecessorFn ?INT1) (PredecessorFn ?INT2))
   (equal ?INT1 ?INT2))

(=>
   (instance ?INT Integer)
   (greaterThan ?INT (PredecessorFn ?INT)))

(=>
   (and
      (instance ?INT1 Integer)
      (instance ?INT2 Integer))
   (not
      (and
         (lessThan ?INT2 ?INT1)
         (lessThan (PredecessorFn ?INT1) ?INT2))))

;; END FILE

;; <module>SET/CLASS_THEORY</module>
;; BEGIN FILE


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     SET/CLASS THEORY      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; INCLUDES 'STRUCTURAL ONTOLOGY'
;; INCLUDES 'BASE ONTOLOGY'

;; The following part of the ontology covers set-theoretic predicates
;; and functions.  Most of the content here is taken from the kif-sets
;; ontology (available on the Ontolingua server).

(subrelation subset subclass)
(domain subset 1 Set)
(domain subset 2 Set)
(documentation subset "(subset ?SET1 ?SET2) is true just in case the
&%elements of the &%Set ?SET1 are also &%elements of the &%Set ?SET2.")

(=>
   (subset ?SUBSET ?SET)
   (forall (?ELEMENT)
      (=>
         (element ?ELEMENT ?SUBSET)
         (element ?ELEMENT ?SET))))

(instance element BinaryPredicate)
(instance element AsymmetricRelation)
(instance element IntransitiveRelation)
(subrelation element instance)
(domain element 1 Entity)
(domain element 2 Set)
(documentation element "(element ?ENTITY ?SET) is true just in case
?ENTITY is contained in the &%Set ?SET.  An &%Entity can be an &%element
of another &%Entity only if the latter is a &%Set.")

(=>
   (forall (?ELEMENT)
         (<=>
            (element ?ELEMENT ?SET1)
            (element ?ELEMENT ?SET2)))
   (equal ?SET1 ?SET2))

(instance UnionFn BinaryFunction)
(instance UnionFn TotalValuedRelation)
(domain UnionFn 1 SetOrClass)
(domain UnionFn 2 SetOrClass)
(range UnionFn SetOrClass)
(documentation UnionFn "A &%BinaryFunction that maps two &%SetOrClasses to
the union of these &%SetOrClasses.  An object is an &%element of the union
of two &%SetOrClasses just in case it is an &%instance of either &%SetOrClass.")

(instance IntersectionFn BinaryFunction)
(instance IntersectionFn TotalValuedRelation)
(domain IntersectionFn 1 SetOrClass)
(domain IntersectionFn 2 SetOrClass)
(range IntersectionFn SetOrClass)
(documentation IntersectionFn "A &%BinaryFunction that maps two
%SetOrClasses to the intersection of these &%SetOrClasses.  An object is
an instance of the intersection of two &%SetOrClasses just in case it is
an instance of both of those &%SetOrClasses.")

(instance RelativeComplementFn BinaryFunction)
(instance RelativeComplementFn TotalValuedRelation)
(domain RelativeComplementFn 1 SetOrClass)
(domain RelativeComplementFn 2 SetOrClass)
(range RelativeComplementFn SetOrClass)
(documentation RelativeComplementFn "A &%BinaryFunction that maps two
&%SetOrClasses to the difference between these &%SetOrClasses.  More
precisely, (&%RelativeComplementFn ?CLASS1 ?CLASS2) denotes the instances
of ?CLASS1 that are not also instances of ?CLASS2.")

(instance ComplementFn UnaryFunction)
(instance ComplementFn TotalValuedRelation)
(domain ComplementFn 1 SetOrClass)
(range ComplementFn SetOrClass)
(documentation ComplementFn "The complement of a given &%SetOrClass C is the &%SetOrClass of all things that are not instances of C.  In other words, an object is an instance of the complement of a &%SetOrClass C just in case it is not an instance of C.")

(instance GeneralizedUnionFn UnaryFunction)
(instance GeneralizedUnionFn TotalValuedRelation)
(domainSubclass GeneralizedUnionFn 1 SetOrClass)
(range GeneralizedUnionFn SetOrClass)
(documentation GeneralizedUnionFn "A &%UnaryFunction that takes a &%SetOrClass
of &%Classes as its single argument and returns a &%SetOrClass which is the
merge of all of the &%Classes in the original &%SetOrClass, i.e. the &%SetOrClass
containing just those instances which are instances of an instance of the
original &%SetOrClass.")

(instance GeneralizedIntersectionFn UnaryFunction)
(instance GeneralizedIntersectionFn TotalValuedRelation)
(domainSubclass GeneralizedIntersectionFn 1 SetOrClass)
(range GeneralizedIntersectionFn SetOrClass)
(documentation GeneralizedIntersectionFn "A &%UnaryFunction that takes a
&%SetOrClass of &%Classes as its single argument and returns a &%SetOrClass which
is the intersection of all of the &%Classes in the original &%SetOrClass, i.e.
the &%SetOrClass containing just those instances which are instances of all
instances of the original &%SetOrClass.")

(instance CardinalityFn UnaryFunction)
(instance CardinalityFn TotalValuedRelation)
(instance CardinalityFn AsymmetricRelation)
(domain CardinalityFn 1 (UnionFn SetOrClass Collection))
(range CardinalityFn Number)
(documentation CardinalityFn "(CardinalityFn ?CLASS) returns the
number of instances in the &%SetOrClass or &%Collection ?CLASS.")

(subclass NullSet SetOrClass)
(documentation NullSet "Any &%SetOrClass that contains no instances.")

(=>
   (instance ?SET NullSet)
   (not (exists (?INST) (instance ?INST ?SET))))

(subclass NonNullSet SetOrClass)
(documentation NonNullSet "Any &%SetOrClass that contains at least one
instance.")

(=>
   (instance ?SET NonNullSet)
   (exists (?INST) (instance ?INST ?SET)))

(subclass FiniteSet Set)
(documentation FiniteSet "A &%Set containing a finite number of elements.")

(=>
   (instance ?SET FiniteSet)
   (exists (?NUMBER)
      (and
         (instance ?NUMBER NonnegativeInteger)
         (equal ?NUMBER (CardinalityFn ?SET)))))

(subclass PairwiseDisjointClass SetOrClass)
(documentation PairwiseDisjointClass "A &%SetOrClass is a &%PairwiseDisjointClass
just in case every instance of the &%SetOrClass is either &%equal to or &%disjoint
from every other instance of the &%SetOrClass.")

(=>
   (instance ?SUPERCLASS PairwiseDisjointClass)
   (forall (?CLASS1 ?CLASS2)
      (=>
         (and
            (instance ?CLASS1 ?SUPERCLASS)
            (instance ?CLASS2 ?SUPERCLASS))
         (or
            (equal ?CLASS1 ?CLASS2)
            (disjoint ?CLASS1 ?CLASS2)))))

(subclass MutuallyDisjointClass SetOrClass)
(documentation MutuallyDisjointClass "A &%SetOrClass is a &%MutuallyDisjointClass
just in case there exists nothing which is an instance of all of the instances of
the original &%SetOrClass.")

(=>
   (instance ?CLASS MutuallyDisjointClass)
   (forall (?INST1 ?INST2)
      (=>
         (and
            (instance ?INST1 ?CLASS)
            (instance ?INST2 ?INST1))
         (exists (?INST3)
            (and
               (instance ?INST3 ?CLASS)
               (not (instance ?INST2 ?INST3)))))))

(instance KappaFn BinaryFunction)
(domain KappaFn 1 SymbolicString)
(domain KappaFn 2 Formula)
(range KappaFn Class)
(documentation KappaFn "A class-forming operator that takes two
arguments:  a variable and a formula containing at least one unbound
occurrence of the variable.  The result of applying &%KappaFn to a
variable and a formula is the &%SetOrClass of things that satisfy the formula.
For example, we can denote the &%SetOrClass of prime numbers that are less
than 100 with the following expression:  (KappaFn ?NUMBER
(and (instance ?NUMBER PrimeNumber) (lessThan ?NUMBER 100))).  Note that
the use of this function is discouraged, since there is currently no
axiomatic support for it.")

;; At some point we may be able to make use of 'KappaFn' by implementing a macro
;; that decomposes every occurrence of 'KappaFn' into a complex formula.  For
;; example the macro might replace every instance of Schema 1 with an instance
;; of Schema 2.
;;
;; Schema 1:  (KappaFn <variable> <formula>)
;;
;; Schema 2: (exists (?LIST)
;;	          (and
;;                 (instance ?LIST UniqueList)
;;		       (forall (<variable>)
;;			    (<=>
;;			       (inList <variable> ?LIST)
;;				 <formula>))))
;;


;; END FILE

;; <module>GRAPH_THEORY</module>
;; BEGIN FILE


;;;;;;;;;;;;;;;;;;;;;
;;  GRAPH THEORY   ;;
;;;;;;;;;;;;;;;;;;;;;

(subclass Graph Abstract)
(documentation Graph "The &%Class of graphs, where a graph is understood
to be a set of &%GraphNodes connected by &%GraphArcs.  Note that this
&%Class includes only connected graphs, i.e. graphs in which there is a
&%GraphPath between any two &%GraphNodes.  Note too that every &%Graph
is assumed to contain at least two &%GraphArcs and three &%GraphNodes.")

(=>
   (and
      (instance ?GRAPH Graph)
      (instance ?NODE1 GraphNode)
      (instance ?NODE2 GraphNode)
      (graphPart ?NODE1 ?GRAPH)
      (graphPart ?NODE2 ?GRAPH)
      (not (equal ?NODE1 ?NODE2)))
   (exists (?ARC ?PATH)
      (or
         (links ?NODE1 ?NODE2 ?ARC)
         (and
            (subGraph ?PATH ?GRAPH)
            (instance ?PATH GraphPath)
            (or
               (and
                  (equal (BeginNodeFn ?PATH) ?NODE1)
                  (equal (EndNodeFn ?PATH) ?NODE2))
               (and
                  (equal (BeginNodeFn ?PATH) ?NODE2)
                  (equal (EndNodeFn ?PATH) ?NODE1)))))))

(=>
   (instance ?GRAPH Graph)
   (exists (?NODE1 ?NODE2 ?NODE3 ?ARC1 ?ARC2)
      (and
         (graphPart ?NODE1 ?GRAPH)
         (graphPart ?NODE2 ?GRAPH)
         (graphPart ?NODE3 ?GRAPH)
         (graphPart ?ARC1 ?GRAPH)
         (graphPart ?ARC2 ?GRAPH)
         (links ?ARC1 ?NODE1 ?NODE2)
         (links ?ARC2 ?NODE2 ?NODE3)
         (not (equal ?NODE1 ?NODE2))
         (not (equal ?NODE2 ?NODE3))
      (not (equal ?NODE1 ?NODE3))
         (not (equal ?ARC1 ?ARC2)))))

(subclass DirectedGraph Graph)
(documentation DirectedGraph "The &%Class of directed graphs.  A
directed graph is a &%Graph in which all &%GraphArcs
have direction, i.e. every &%GraphArc has an initial node (see
&%InitialNodeFn) and a terminal node (see &%TerminalNodeFn).")

(=>
   (and
      (instance ?GRAPH DirectedGraph)
      (instance ?ARC GraphArc)
      (graphPart ?ARC ?GRAPH))
   (exists (?NODE1 ?NODE2)
      (and
         (equal (InitialNodeFn ?ARC) ?NODE1)
         (equal (TerminalNodeFn ?ARC) ?NODE2))))

(subclass TreeGraph Graph)
(documentation TreeGraph "A TreeGraph is a &%DirectedGraph that has no &%GraphLoops.")

(=>
   (instance ?GRAPH TreeGraph)
   (not (exists (?LOOP)
      (and
         (instance ?LOOP GraphLoop)
         (graphPart ?LOOP ?GRAPH)))))

(subclass GraphPath DirectedGraph)
(documentation GraphPath "Informally, a single, directed route between
two &%GraphNodes in a &%Graph.  Formally, a &%DirectedGraph that is a
&%subGraph of the original &%Graph and such that no two &%GraphArcs in
the &%DirectedGraph have the same intial node (see &%InitialNodeFn) or
the same terminal node (see &%TerminalNodeFn).")

(=>
   (and
      (instance ?GRAPH GraphPath)
      (instance ?ARC GraphArc)
      (graphPart ?ARC ?GRAPH))
   (=>
      (equal (InitialNodeFn ?ARC) ?NODE)
      (not (exists (?OTHER)
         (and
            (equal (InitialNodeFn ?OTHER) ?NODE)
            (not (equal ?OTHER ?ARC)))))))

(=>
   (and
      (instance ?GRAPH GraphPath)
      (instance ?ARC GraphArc)
      (graphPart ?ARC ?GRAPH))
   (=>
      (equal (TerminalNodeFn ?ARC) ?NODE)
      (not (exists (?OTHER)
         (and
            (equal (TerminalNodeFn ?OTHER) ?NODE)
            (not (equal ?OTHER ?ARC)))))))

(subclass GraphCircuit GraphPath)
(documentation GraphCircuit "A &%GraphPath that begins (see
&%BeginNodeFn) and ends (see &%EndNodeFn) at the same
&%GraphNode.")

(<=>
   (instance ?GRAPH GraphCircuit)
   (exists (?NODE)
      (and
         (equal (BeginNodeFn ?GRAPH) ?NODE)
         (equal (EndNodeFn ?GRAPH) ?NODE))))

(subclass MultiGraph Graph)
(documentation MultiGraph "The &%Class of multigraphs.  A multigraph
is a &%Graph containing at least one pair of &%GraphNodes that are
connected by more than one &%GraphArc.")

(<=>
   (instance ?GRAPH MultiGraph)
   (exists (?ARC1 ?ARC2 ?NODE1 ?NODE2)
      (and
         (graphPart ?ARC1 ?GRAPH)
         (graphPart ?ARC2 ?GRAPH)
         (graphPart ?NODE1 ?GRAPH)
         (graphPart ?NODE2 ?GRAPH)
         (links ?NODE1 ?NODE2 ?ARC1)
         (links ?NODE1 ?NODE2 ?ARC2)
         (not (equal ?ARC1 ?ARC2)))))

(subclass PseudoGraph Graph)
(documentation PseudoGraph "The &%Class of pseudographs.  A pseudograph
is a &%Graph containing at least one &%GraphLoop.")

(<=>
   (instance ?GRAPH PseudoGraph)
   (exists (?LOOP)
      (and
         (instance ?LOOP GraphLoop)
         (graphPart ?LOOP ?GRAPH))))

(subclass GraphElement Abstract)
(partition GraphElement GraphNode GraphArc)
(documentation GraphElement "Noncompositional parts of &%Graphs.
These parts are restricted to &%GraphNodes and &%GraphArcs.")

(=>
   (instance ?PART GraphElement)
   (exists (?GRAPH)
      (and
         (instance ?GRAPH Graph)
         (graphPart ?PART ?GRAPH))))

(subclass GraphNode GraphElement)
(documentation GraphNode "&%Graphs are comprised of &%GraphNodes
and &%GraphArcs.  Every &%GraphNode is linked by a &%GraphArc.")

(=>
   (instance ?NODE GraphNode)
   (exists (?OTHER ?ARC)
      (links ?NODE ?OTHER ?ARC)))

(subclass GraphArc GraphElement)
(documentation GraphArc "&%Graphs are comprised of &%GraphNodes
and &%GraphArcs.  Every &%GraphArc links two &%GraphNodes.")

(=>
   (instance ?ARC GraphArc)
   (exists (?NODE1 ?NODE2)
      (links ?NODE1 ?NODE2 ?ARC)))

(subclass GraphLoop GraphArc)
(documentation GraphLoop "A &%GraphArc in which a &%GraphNode is
linked to itself.")

(<=>
   (instance ?LOOP GraphLoop)
   (exists (?NODE)
      (links ?NODE ?NODE ?LOOP)))

(=>
   (and
      (equal (InitialNodeFn ?ARC) ?NODE)
      (equal (TerminalNodeFn ?ARC) ?NODE))
   (instance ?ARC GraphLoop))

(instance links TernaryPredicate)
(domain links 1 GraphNode)
(domain links 2 GraphNode)
(domain links 3 GraphArc)
(documentation links "a &%TernaryPredicate that specifies the
&%GraphArc connecting two &%GraphNodes.")

(=>
   (links ?NODE1 ?NODE2 ?ARC)
   (links ?NODE2 ?NODE1 ?ARC))

(instance graphPart BinaryPredicate)
(instance graphPart AsymmetricRelation)
(instance graphPart IrreflexiveRelation)
(domain graphPart 1 GraphElement)
(domain graphPart 2 Graph)
(documentation graphPart "A basic relation for &%Graphs and their
parts.  (&%graphPart ?PART ?GRAPH) means that ?PART is a &%GraphArc
or &%GraphNode of the &%Graph ?GRAPH.")

(instance subGraph BinaryPredicate)
(instance subGraph ReflexiveRelation)
(instance subGraph TransitiveRelation)
(domain subGraph 1 Graph)
(domain subGraph 2 Graph)
(documentation subGraph "The relation between two &%Graphs when one
&%Graph is a part of the other.  (&%subGraph ?GRAPH1 ?GRAPH2) means
that ?GRAPH1 is a part of ?GRAPH2.")

(=>
   (and
      (subGraph ?GRAPH1 ?GRAPH2)
      (graphPart ?ELEMENT ?GRAPH1))
   (graphPart ?ELEMENT ?GRAPH2))

(instance pathLength BinaryPredicate)
(instance pathLength AsymmetricRelation)
(instance pathLength IrreflexiveRelation)
(domain pathLength 1 GraphPath)
(domain pathLength 2 PositiveInteger)
(documentation pathLength "A &%BinaryPredicate that specifies the
length (in number of &%GraphNodes) of a &%GraphPath.
(&%pathLength ?PATH ?NUMBER) means that there are ?NUMBER nodes in
the &%GraphPath ?PATH.")

(instance InitialNodeFn UnaryFunction)
(instance InitialNodeFn PartialValuedRelation)
(domain InitialNodeFn 1 GraphArc)
(range InitialNodeFn GraphNode)
(documentation InitialNodeFn "A &%UnaryFunction that maps a
&%GraphArc to the initial node of the &%GraphArc.  Note
that this is a partial function.  In particular, the function is
undefined for &%GraphArcs that are not part of a &%DirectedGraph.")

(instance TerminalNodeFn UnaryFunction)
(instance TerminalNodeFn PartialValuedRelation)
(domain TerminalNodeFn 1 GraphArc)
(range TerminalNodeFn GraphNode)
(documentation TerminalNodeFn "A &%UnaryFunction that maps a
&%GraphArc to the terminal node of the &%GraphArc.  Note that this
is a partial function.  In particular, the function is undefined
for &%GraphArcs that are not part of a &%DirectedGraph.")

(instance BeginNodeFn UnaryFunction)
(instance BeginNodeFn TotalValuedRelation)
(domain BeginNodeFn 1 GraphPath)
(range BeginNodeFn GraphNode)
(relatedInternalConcept BeginNodeFn InitialNodeFn)
(documentation BeginNodeFn "A &%UnaryFunction that maps a &%GraphPath
to the &%GraphNode that is the beginning of the &%GraphPath.  Note that,
unlike &%InitialNodeFn (which relates a &%GraphArc to a &%GraphNode),
&%BeginNodeFn is a total function - every &%GraphPath has a beginning.")

(instance EndNodeFn UnaryFunction)
(instance EndNodeFn TotalValuedRelation)
(domain EndNodeFn 1 GraphPath)
(range EndNodeFn GraphNode)
(relatedInternalConcept EndNodeFn TerminalNodeFn)
(documentation EndNodeFn "A &%UnaryFunction that maps a &%GraphPath
to the &%GraphNode that is the end of the &%GraphPath.  Note that, unlike
&%TerminalNodeFn (which relates a &%GraphArc to a &%GraphNode),
&%EndNodeFn is a total function - every &%GraphPath has a end.")

(instance arcWeight BinaryPredicate)
(instance arcWeight SingleValuedRelation)
(domain arcWeight 1 GraphArc)
(domain arcWeight 2 RealNumber)
(documentation arcWeight "This predicate indicates the value of a
&%GraphArc in a &%Graph.  This could map to the length of a road in
a road network or the flow rate of a pipe in a plumbing system.")

(instance PathWeightFn UnaryFunction)
(domain PathWeightFn 1 GraphPath)
(range PathWeightFn RealNumber)
(documentation PathWeightFn "A &%UnaryFunction that maps a
&%GraphPath to the sum of the &%arcWeights on the &%GraphArcs in
the &%GraphPath.")

(=>
   (and
      (equal (PathWeightFn ?PATH) ?SUM)
      (subGraph ?SUBPATH ?PATH)
      (graphPart ?ARC1 ?PATH)
      (arcWeight ?ARC1 ?NUMBER1)
      (forall (?ARC2)
         (=>
            (graphPart ?ARC2 ?PATH)
            (or
               (graphPart ?ARC2 ?SUBPATH)
               (equal ?ARC2 ?ARC1)))))
   (equal ?SUM (AdditionFn (PathWeightFn ?SUBPATH) ?NUMBER1)))

(=>
   (and
      (equal (PathWeightFn ?PATH) ?SUM)
      (graphPart ?ARC1 ?PATH)
      (graphPart ?ARC2 ?PATH)
      (arcWeight ?ARC1 ?NUMBER1)
      (arcWeight ?ARC2 ?NUMBER2)
      (forall (?ARC3)
         (=>
            (graphPart ?ARC3 ?PATH)
            (or
               (equal ?ARC3 ?ARC1)
               (equal ?ARC3 ?ARC2)))))
   (equal (PathWeightFn ?PATH) (AdditionFn ?NUMBER1 ?NUMBER2)))

(instance MinimalWeightedPathFn BinaryFunction)
(domain MinimalWeightedPathFn 1 GraphNode)
(domain MinimalWeightedPathFn 2 GraphNode)
(range MinimalWeightedPathFn GraphPath)
(documentation MinimalWeightedPathFn "This &%BinaryFunction assigns two
&%GraphNodes to the &%GraphPath with the smallest sum of weighted arcs
between the two &%GraphNodes.")

(=>
   (equal (MinimalWeightedPathFn ?NODE1 ?NODE2) ?PATH)
   (instance ?PATH (GraphPathFn ?NODE1 ?NODE2)))

(=>
   (and
      (equal (MinimalWeightedPathFn ?NODE1 ?NODE2) ?PATH)
      (equal (PathWeightFn ?PATH) ?NUMBER))
   (forall (?PATH2)
      (=>
         (and
            (instance ?PATH2 (GraphPathFn ?NODE1 ?NODE2))
            (equal (PathWeightFn ?PATH2) ?NUMBER2))
         (greaterThanOrEqualTo ?NUMBER2 ?NUMBER1))))

(instance MaximalWeightedPathFn BinaryFunction)
(domain MaximalWeightedPathFn 1 GraphNode)
(domain MaximalWeightedPathFn 2 GraphNode)
(range MaximalWeightedPathFn GraphPath)
(documentation MaximalWeightedPathFn "This &%BinaryFunction assigns two
&%GraphNodes to the &%GraphPath with the largest sum of weighted arcs
between the two &%GraphNodes.")

(=>
   (equal (MaximalWeightedPathFn ?NODE1 ?NODE2) ?PATH)
   (instance ?PATH (GraphPathFn ?NODE1 ?NODE2)))

(=>
   (and
      (equal (MaximalWeightedPathFn ?NODE1 ?NODE2) ?PATH)
      (equal (PathWeightFn ?PATH) ?NUMBER))
   (forall (?PATH2)
      (=>
         (and
            (instance ?PATH2 (GraphPathFn ?NODE1 ?NODE2))
            (equal (PathWeightFn ?PATH2) ?NUMBER2))
         (lessThanOrEqualTo ?NUMBER2 ?NUMBER1))))

(instance GraphPathFn BinaryFunction)
(instance GraphPathFn TotalValuedRelation)
(domain GraphPathFn 1 GraphNode)
(domain GraphPathFn 2 GraphNode)
(rangeSubclass GraphPathFn GraphPath)
(documentation GraphPathFn "A &%BinaryFunction that maps two &%GraphNodes
to the &%Class of &%GraphPaths between those two nodes.  Note that the two
&%GraphNodes must belong to the same &%Graph.")

(=>
   (and
      (graphPart ?PATH ?GRAPH)
      (not (instance ?GRAPH DirectedGraph)))
   (<=>
      (equal (GraphPathFn ?NODE1 ?NODE2) ?PATH)
      (equal (GraphPathFn ?NODE2 ?NODE1) ?PATH)))

(instance CutSetFn UnaryFunction)
(domain CutSetFn 1 Graph)
(rangeSubclass CutSetFn GraphPath)
(documentation CutSetFn "A &%UnaryFunction that assigns a &%Graph the
&%Class of &%GraphPaths that partition the graph into two separate
graphs if cut.  There may be more than one cutset for a given graph.")

(instance MinimalCutSetFn UnaryFunction)
(domain MinimalCutSetFn 1 Graph)
(rangeSubclass MinimalCutSetFn GraphPath)
(relatedInternalConcept MinimalCutSetFn CutSetFn)
(documentation MinimalCutSetFn "A &%UnaryFunction that assigns a &%Graph
the &%Class of &%GraphPaths which comprise cutsets for the &%Graph and
which have the least number of &%GraphArcs.")

(=>
   (instance ?GRAPH Graph)
   (subclass (MinimalCutSetFn ?GRAPH) (CutSetFn ?GRAPH)))

(=>
   (equal (MinimalCutSetFn ?GRAPH) ?PATHCLASS)
   (exists (?NUMBER)
      (forall (?PATH)
         (=>
            (instance ?PATH ?PATHCLASS)
            (pathLength ?PATH ?NUMBER)))))

(not (exists (?PATH1 ?PATH2)
   (and
      (instance ?PATH1 (CutSetFn ?GRAPH))
      (instance ?PATH2 (MinimalCutSetFn ?GRAPH))
      (pathLength ?PATH1 ?NUMBER1)
      (pathLength ?PATH2 ?NUMBER2)
      (lessThan ?NUMBER1 ?NUMBER2))))

;; END FILE

;; <module>UNITS_OF_MEASURE</module>
;; BEGIN FILE


;;;;;;;;;;;;;;;;;;;;;;;;;
;;  UNITS OF MEASURE   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; INCLUDES 'NUMERIC FUNCTIONS'

;; This section was originally based on the relations in the Quantities
;; ontology (developed by ITBM-CNR) and the units of measure in the
;; "Standard Units" and "Standard Dimensions" ontologies on the Ontolingua
;; server.  This content has been extensively revised by Helena Sofia Pinto
;; of the Instituto Superior Tecnico in Portugal.  The sources for these
;; revisions were:
;; - Barry Taylor, NIST Special Publication 811, Guide for the Use of the
;;   International System of Units (SI), 1995.
;; - Encyclopaedia Britannica (on-line version at http://www.britannica.com)

(subclass UnitOfMeasure PhysicalQuantity)
(documentation UnitOfMeasure "A standard of measurement for some dimension.
For example, the &%Meter is a &%UnitOfMeasure for the dimension of length,
as is the &%Inch.  There is no intrinsic property of a &%UnitOfMeasure that
makes it primitive or fundamental; rather, a system of units (e.g.
&%SystemeInternationalUnit) defines a set of orthogonal dimensions and
assigns units for each.")

(subclass SystemeInternationalUnit UnitOfMeasure)
(documentation SystemeInternationalUnit "The &%Class of Systeme
International (SI) units.")

(subclass LengthMeasure ConstantQuantity)
(documentation LengthMeasure "The &%Class of &%ConstantQuantities relating
to length.")

(subclass MassMeasure ConstantQuantity)
(documentation MassMeasure "The &%Class of &%ConstantQuantities relating
to the amount of matter in an &%Object.")

(subclass AreaMeasure ConstantQuantity)
(documentation AreaMeasure "Measures of the amount of space in two
dimensions.")

(subclass VolumeMeasure ConstantQuantity)
(documentation VolumeMeasure "Measures of the amount of space in three
dimensions.")

(subclass TemperatureMeasure ConstantQuantity)
(documentation TemperatureMeasure "Measures of temperature.
In scientific circles, the temperature of something is understood as the
average velocity of the atoms or molecules that make up the thing.")

(subclass CurrencyMeasure ConstantQuantity)
(documentation CurrencyMeasure "Includes all standard measures of monetary
value, including &%UnitedStatesDollar, &%UnitedStatesCent, Lire, Yen, etc.")

(subclass AngleMeasure ConstantQuantity)
(documentation AngleMeasure "The value of an angle in a plane or in a solid.")

(subclass PlaneAngleMeasure AngleMeasure)
(documentation PlaneAngleMeasure "The value of an angle in a plane.")

(subclass SolidAngleMeasure AngleMeasure)
(disjoint SolidAngleMeasure PlaneAngleMeasure)
(documentation SolidAngleMeasure "The value of an angle in a solid.")

(instance MeasureFn BinaryFunction)
(instance MeasureFn TotalValuedRelation)
(domain MeasureFn 1 RealNumber)
(domain MeasureFn 2 UnitOfMeasure)
(range MeasureFn ConstantQuantity)
(documentation MeasureFn "This &%BinaryFunction maps a &%RealNumber and a &%UnitOfMeasure to that &%Number of units.  It is used for expressing &%ConstantQuantities.  For example, the concept of three meters is represented as (&%MeasureFn 3 &%Meter).")

(=>
   (and
      (equal (MeasureFn ?NUMBER ?UNIT) ?QUANT)
      (subclass ?UNIT ?QUANTTYPE))
   (instance ?QUANT ?QUANTTYPE))

(=>
   (and
      (instance ?REL RelationExtendedToQuantities)
      (instance ?REL TernaryRelation)
      (instance ?NUMBER1 RealNumber)
      (instance ?NUMBER2 RealNumber)
      (holds ?REL ?NUMBER1 ?NUMBER2 ?VALUE))
   (forall (?UNIT)
      (=>
         (instance ?UNIT UnitOfMeasure)
         (holds ?REL (MeasureFn ?NUMBER1 ?UNIT) (MeasureFn ?NUMBER2 ?UNIT) (MeasureFn ?VALUE ?UNIT)))))

(=>
   (and
      (instance ?REL RelationExtendedToQuantities)
      (instance ?REL BinaryRelation)
      (instance ?NUMBER1 RealNumber)
      (instance ?NUMBER2 RealNumber)
      (holds ?REL ?NUMBER1 ?NUMBER2))
   (forall (?UNIT)
      (=>
         (instance ?UNIT UnitOfMeasure)
         (holds ?REL (MeasureFn ?NUMBER1 ?UNIT) (MeasureFn ?NUMBER2 ?UNIT)))))

(instance KiloFn UnaryFunction)
(instance KiloFn TotalValuedRelation)
(domain KiloFn 1 UnitOfMeasure)
(range KiloFn UnitOfMeasure)
(documentation KiloFn "A &%UnaryFunction that maps a &%UnitOfMeasure into
a &%UnitOfMeasure that is equal to 1,000 units of the original &%UnitOfMeasure.
For example, (&%KiloFn &%Gram) is 1,000 &%Grams.")

(=>
   (instance ?UNIT UnitOfMeasure)
   (equal (KiloFn ?UNIT) (MeasureFn 1000 ?UNIT)))

(instance MegaFn UnaryFunction)
(instance MegaFn TotalValuedRelation)
(domain MegaFn 1 UnitOfMeasure)
(range MegaFn UnitOfMeasure)
(documentation MegaFn "A &%UnaryFunction that maps a &%UnitOfMeasure into
a &%UnitOfMeasure that is equal to 1,000,000 units of the original
&%UnitOfMeasure.  For example, (&%MegaFn &%Hertz) is 1,000,000 &%Hertz.")

(=>
   (instance ?UNIT UnitOfMeasure)
   (equal (MegaFn ?UNIT) (MeasureFn 1000000 ?UNIT)))

(instance GigaFn UnaryFunction)
(instance GigaFn TotalValuedRelation)
(domain GigaFn 1 UnitOfMeasure)
(range GigaFn UnitOfMeasure)
(documentation GigaFn "A &%UnaryFunction that maps a &%UnitOfMeasure into
a &%UnitOfMeasure that is equal to 1,000,000,000 units of the original
&%UnitOfMeasure.  For example, (&%GigaFn &%Hertz) is 1,000,000,000 &%Hertz.")

(=>
   (instance ?UNIT UnitOfMeasure)
   (equal (GigaFn ?UNIT) (MeasureFn 1000000000 ?UNIT)))

(instance TeraFn UnaryFunction)
(instance TeraFn TotalValuedRelation)
(domain TeraFn 1 UnitOfMeasure)
(range TeraFn UnitOfMeasure)
(documentation TeraFn "A &%UnaryFunction that maps a &%UnitOfMeasure
into a &%UnitOfMeasure that is equal to 1,000,000,000,000 units of the original
&%UnitOfMeasure.  For example, (&%TeraFn &%Hertz) is 1,000,000,000,000 &%Hertz.")

(=>
   (instance ?UNIT UnitOfMeasure)
   (equal (TeraFn ?UNIT) (MeasureFn 1000000000000 ?UNIT)))

(instance MilliFn UnaryFunction)
(instance MilliFn TotalValuedRelation)
(domain MilliFn 1 UnitOfMeasure)
(range MilliFn UnitOfMeasure)
(documentation MilliFn "A &%UnaryFunction that maps a &%UnitOfMeasure into
a &%UnitOfMeasure that is equal to .001 units of the original &%UnitOfMeasure.
For example, (&%MilliFn &%Ampere) is .001 &%Amperes.")

(=>
   (instance ?UNIT UnitOfMeasure)
   (equal (MilliFn ?UNIT) (MeasureFn 0.001 ?UNIT)))

(instance MicroFn UnaryFunction)
(instance MicroFn TotalValuedRelation)
(domain MicroFn 1 UnitOfMeasure)
(range MicroFn UnitOfMeasure)
(documentation MicroFn "A &%UnaryFunction that maps a &%UnitOfMeasure into
a &%UnitOfMeasure that is equal to .000001 units of the original &%UnitOfMeasure.
For example, (&%MicroFn &%Meter) is .000001 &%Meters.")

(=>
   (instance ?UNIT UnitOfMeasure)
   (equal (MicroFn ?UNIT) (MeasureFn 0.000001 ?UNIT)))

(instance NanoFn UnaryFunction)
(instance NanoFn TotalValuedRelation)
(domain NanoFn 1 UnitOfMeasure)
(range NanoFn UnitOfMeasure)
(documentation NanoFn "A &%UnaryFunction that maps a &%UnitOfMeasure into
a &%UnitOfMeasure that is equal to .000000001 units of the original
&%UnitOfMeasure.  For example, (&%MicroFn &%SecondDuration) is .000000001
&%SecondDurations.")

(=>
   (instance ?UNIT UnitOfMeasure)
   (equal (NanoFn ?UNIT) (MeasureFn 0.000000001 ?UNIT)))

(instance PicoFn UnaryFunction)
(instance PicoFn TotalValuedRelation)
(domain PicoFn 1 UnitOfMeasure)
(range PicoFn UnitOfMeasure)
(documentation PicoFn "A &%UnaryFunction that maps a &%UnitOfMeasure into
a &%UnitOfMeasure that is equal to .000000000001 units of the original
&%UnitOfMeasure.  For example, (&%PicoFn &%Ampere) is .000000000001
&%Amperes.")

(=>
   (instance ?UNIT UnitOfMeasure)
   (equal (PicoFn ?UNIT) (MeasureFn 0.000000000001 ?UNIT)))

(instance IntervalFn BinaryFunction)
(domain IntervalFn 1 ConstantQuantity)
(domain IntervalFn 2 ConstantQuantity)
(rangeSubclass IntervalFn ConstantQuantity)
(relatedInternalConcept IntervalFn RecurrentTimeIntervalFn)
(documentation IntervalFn "A &%BinaryFunction that maps two &%ConstantQuantities
to the &%Class of &%ConstantQuantities that comprise the interval from the first
&%ConstantQuantity to the second &%ConstantQuantity.  For example, (&%IntervalFn
(&%MeasureFn 8 &%Meter) (&%MeasureFn 14 &%Meter)) would return the &%Class of
&%ConstantQuantities between 8 and 14 meters in length.")

(<=>
   (instance ?QUANTITY (IntervalFn ?FROM ?TO))
   (and
        (greaterThanOrEqualTo ?QUANTITY ?FROM)
        (lessThanOrEqualTo ?QUANTITY ?TO)))

(instance MagnitudeFn UnaryFunction)
(domain MagnitudeFn  1 ConstantQuantity)
(range MagnitudeFn RealNumber)
(documentation MagnitudeFn "The magnitude of a &%ConstantQuantity is the
numeric value for the quantity.  In other words, &%MagnitudeFn converts
a &%ConstantQuantity with an associated &%UnitOfMeasure into an ordinary
&%RealNumber.  For example, the magnitude of the &%ConstantQuantity 2
&%Kilometers is the &%RealNumber 2.   Note that the magnitude of a
quantity in a given unit times that unit is equal to the original
quantity.")

(=>
   (and
      (instance ?NUMBER RealNumber)
      (instance ?UNIT UnitOfMeasure))
(equal (MagnitudeFn (MeasureFn ?NUMBER ?UNIT)) ?NUMBER))

(instance PerFn BinaryFunction)
(instance PerFn TotalValuedRelation)
(domain PerFn 1 ConstantQuantity)
(domain PerFn 2 ConstantQuantity)
(range PerFn FunctionQuantity)
(documentation PerFn "&%PerFn maps two instances of &%ConstantQuantity to the &%FunctionQuantity composed of these two instances.  For example, (&%PerFn (&%MeasureFn 2 (&%MicroFn &%Gram)) (&%MeasureFn 1 (&%KiloFn &%Gram))) denotes the &%FunctionQuantity of
2 micrograms per kiogram.  This function is useful, because it allows the knowledge
engineer to dynamically generate instances of &%FunctionQuantity.")

(subrelation DensityFn PerFn)
(instance DensityFn TotalValuedRelation)
(domain DensityFn 1 MassMeasure)
(domain DensityFn 2 VolumeMeasure)
(range DensityFn FunctionQuantity)
(documentation DensityFn "&%DensityFn maps an instance of &%MassMeasure
and an instance of &%VolumeMeasure to the density represented by this
proportion of mass and volume.  For example, (&%DensityFn (&%MeasureFn 3 &%Gram)
(&%MeasureFn 1 &%Liter)) represents the density of 3 grams per liter.")

(subrelation SpeedFn PerFn)
(instance SpeedFn TotalValuedRelation)
(domain SpeedFn 1 LengthMeasure)
(domain SpeedFn 2 TimeDuration)
(range SpeedFn FunctionQuantity)
(documentation SpeedFn "Maps an instance of &%LengthMeasure and an instance of
&%TimeDuration to the speed represented by this proportion of distance and time.
For example, (&%SpeedFn (&%MeasureFn 55 &%Mile)(&%MeasureFn 1 &%HourDuration))
represents the velocity of 55 miles per hour.")

(instance VelocityFn QuaternaryFunction)
(instance VelocityFn TotalValuedRelation)
(domain VelocityFn 1 LengthMeasure)
(domain VelocityFn 2 TimeDuration)
(domain VelocityFn 3 Region)
(domain VelocityFn 4 DirectionalAttribute)
(range VelocityFn FunctionQuantity)
(documentation VelocityFn "Specifies the velocity of an object, i.e. the speed
and the direction of the speed.  For example (&%VelocityFn (&%MeasureFn 55 &%Mile)
(&%MeasureFn 2 &%HourDuration) ?REFERENCE &%North) denotes the velocity of 55 miles
per hour North of the given reference point ?REFERENCE.")

(=>
   (measure ?OBJECT (VelocityFn ?DISTANCE ?TIME ?REF ?DIRECTION))
   (measure ?OBJECT (SpeedFn ?DISTANCE ?TIME)))

;; Now the units of measure:

;; First base units for the SI system. No conversion functions are
;; provided for these units.

;; Length Base Unit

(subclass Meter LengthMeasure)
(instance Meter SystemeInternationalUnit)
(documentation Meter "SI &%LengthMeasure.  Symbol:  m. It is one of the
base units in SI, and it is currently defined as follows: the &%Meter
is the length of the path traveled by light in a vacuum during a time
interval of 1/299792458 of a &%SecondDuration.")

;; Mass Base Unit

(subclass Gram MassMeasure)
(instance Gram SystemeInternationalUnit)
(documentation Gram "Submultiple of kilogram.  Symbol: g.
1 kilogram = 1000 &%Grams.")

;; Time Base Unit

(subclass SecondDuration TimeDuration)
(instance SecondDuration SystemeInternationalUnit)
(documentation SecondDuration "SI &%TimeDuration.  Symbol: s.
It is one of the base units in SI, and it is currently defined as
follows: the &%SecondDuration is the duration of 9192631770 periods of
the radiation corresponding to the transition between the two hyperfine
levels of the ground state of the cesium 133 atom.")

;; Electric Current Base Unit

(subclass Ampere FunctionQuantity)
(instance Ampere SystemeInternationalUnit)
(documentation Ampere "SI electric current measure.  Symbol: A. It is
one of the base units in SI. It is defined as follows: the &%Ampere is
that constant current which, if maintained in two straight parallel
conductors of infinite length, of negligible circular cross-section, and
placed 1 &%Meter apart in a vacuum, would produce between these conductors
a force equal to 2*10^(-7) &%Newton per &%Meter of length.")

;; Thermodynamic Temperature Base Unit

(subclass KelvinDegree TemperatureMeasure)
(instance KelvinDegree SystemeInternationalUnit)
(documentation KelvinDegree "SI &%TemperatureMeasure.  Symbol: K.
It is one of the base units in SI (it is also a unit in the ITS system).
Kelvin differs from the Celsius scale in that the triple point of water
is defined to be 273.16 &%KelvinDegrees while it is 0 &%CelsiusDegrees.
The magnitudes of intervals in the two scales are the same.  By definition
the conversion constant is 273.15.")

;; Amount Of Substance Base Unit

(subclass Mole MassMeasure)
(instance Mole SystemeInternationalUnit)
(documentation Mole "SI amount of substance unit. symbol: mol. It is one
of the base units in SI. It is defined as follows:  the &%Mole is the
amount of substance of a system which contains as many elementary entities
as there are atoms in 0.012 &%Kilograms of carbon 12.  Note that, when this
&%UnitOfMeasure is used, the elementary entities must be specified - they
may be atoms, molecules, ions, electrons, etc. or groups of such
particles.")

;; Luminosity Intensity Base Unit

(subclass Candela FunctionQuantity)
(instance Candela SystemeInternationalUnit)
(documentation Candela "SI luminosity intensity measure.  Symbol: cd.
It is one of the base units in SI, and it is currently defined as
follows:  the &%Candela is the luminous intensity, in a given direction,
of a source that emits monochromatic radiation of frequency 540*10^12
&%Hertz and that has a radiant intensity in that direction of 1/683
&%Watt per &%Steradian.")

(subclass Liter VolumeMeasure)
(instance Liter UnitOfMeasure)
(documentation Liter "Unit of volume in the metric system.  It is currently
defined to be equal to one cubic decimeter (0.001 cubic meter).  Symbol: l.")

(subclass Centimeter LengthMeasure)
(instance Centimeter UnitOfMeasure)
(documentation Centimeter "Submultiple of &%Meter.  Symbol: cm. It is
the 100th part of a &%Meter")

(=>
   (instance ?NUMBER RealNumber)
   (equal
        (MeasureFn ?NUMBER Centimeter)
        (MeasureFn (MultiplicationFn ?NUMBER 0.01) Meter)))

;; What follows are derived SI units with special names and symbols
;; (multiples and submultiples are together since they represent
;; quantities of the same kind).

;; Plane angle unit

(subclass Radian PlaneAngleMeasure)
(instance Radian SystemeInternationalUnit)
(documentation Radian "SI plane angle measure.  Symbol: rad.  It is the
angle of a circle subtended by an arc equal in length to the circle's
radius. Another definition is:  the plane angle between two radii of a
circle which cut off on the circumference an arc equal in length to the
radius.  &%Radian = m/m = 1.")

;; Solid angle unit

(subclass Steradian SolidAngleMeasure)
(instance Steradian SystemeInternationalUnit)
(documentation Steradian "SI solid angle measure.  Symbol: sr.  It is
the solid angle of a sphere subtended by a portion of the surface whose
area is equal to the square of the sphere's radius.  Another definition
is: the solid angle which, having its vertex in the center of the sphere,
cuts off an area of the surface of the sphere equal to that of a square
with sides of length equal to the radius of the sphere.  &%Steradian =
m^2/m^2 = 1.")

;; Frequency units

(subclass Hertz TimeDependentQuantity)
(instance Hertz SystemeInternationalUnit)
(documentation Hertz "SI frequency measure.  Symbol: Hz. It is the
number of cycles per second.  &%Hertz = s^(-1).  Note that &%Hertz
does not have a conversion function.")

; Force Unit

(subclass Newton FunctionQuantity)
(instance Newton SystemeInternationalUnit)
(documentation Newton "SI force measure.  Symbol: N. It is that force
which gives to a mass of 1 kilogram an acceleration of 1 &%Meter per
&%SecondDuration.  &%Newton = m*kg*s^(-2).")

; Pressure unit

(subclass Pascal FunctionQuantity)
(instance Pascal SystemeInternationalUnit)
(documentation Pascal "SI pressure measure.  Symbol:Pa. It is the
pressure of one &%Newton per square &%Meter.  &%Pascal = N/m^2
= m^(-1)*kg*s^(-2).")

; Energy Unit

(subclass Joule FunctionQuantity)
(instance Joule SystemeInternationalUnit)
(documentation Joule "SI energy measure.  Symbol: J.  It is the work
done when the point of application of 1 &%Newton is displaced a distance
of 1 &%Meter in the direction of the force.  &%Joule = N*m =
m^2*kg*s^(-2).")

; Power Units

(subclass Watt FunctionQuantity)
(instance Watt SystemeInternationalUnit)
(documentation Watt "SI power measure.  Symbol: W.  A &%UnitOfMeasure
that measures power, i.e. energy produced or expended divided by
&%TimeDuration. It is the power which gives rise to the production
of energy (or work) at the rate of one &%Joule per &%SecondDuration.
&%Watt = J/s = m^2*kg*s^(-3).")

;;; Note: According to SI one should not use the expression "per unit of."

; Electric Charge Units

(subclass Coulomb TimeDependentQuantity)
(instance Coulomb SystemeInternationalUnit)
(documentation Coulomb "SI electric charge measure.  Symbol: C. It is
the quantity of electric charge transported through a cross section of
a conductor in an electric circuit during each &%SecondDuration by a
current of 1 &%Ampere.  Coulomb = s*A.")

; Electric Potential Units

(subclass Volt FunctionQuantity)
(instance Volt SystemeInternationalUnit)
(documentation Volt "SI electric potential measure.  Symbol: V.  It is
the difference of electric potential between two points of a conducting
wire carrying a constant current of 1 &%Ampere, when the power dissipated
between these points is equal to 1 &%Watt.  &%Volt = W/A =
m^2*kg*s^(-3)*A^(-1).")

; Capacitance Units

(subclass Farad FunctionQuantity)
(instance  Farad SystemeInternationalUnit)
(documentation Farad "SI capacitance measure.  Symbol: F.  It is the
capacitance of a capacitator between the plates of which there appears
a difference of potential of 1 &%Volt when it is charged by a quantity
of electricity equal to 1 Coulomb.  &%Farad = C/V =
m^(-2)*kg(-1)*s^4*A^2.")

;Electric Resistance Units

(subclass Ohm FunctionQuantity)
(instance Ohm SystemeInternationalUnit)
(documentation Ohm "SI electric resistance measure. It is the electric
resistance between two points of a conductor when a constant difference
of potential of 1 &%Volt, applied between these two points,
produces in this conductor a current of 1 &%Ampere, this conductor not
being the force of any electromotive force.  &%Ohm = V/A =
m^2*kg*s^(-3)*A^(-2).")

; Electric Conductance Units

(subclass Siemens FunctionQuantity)
(instance Siemens SystemeInternationalUnit)
(documentation Siemens "SI electric conductance measure.  Symbol:  S.
In the case of direct current, the conductance in &%Siemens is the
reciprocal of the resistance in &%Ohms; in the case of alternating current,
it is the reciprocal of the impedance in ohms.  siemens = A/V =
m^(-2)*kg(-1)*s^(3)*A^2.")

; Magnetic Flux Units

(subclass Weber FunctionQuantity)
(instance  Weber SystemeInternationalUnit)
(documentation Weber "SI magnetic flux measure.  Symbol: Wb. It is the
magnetic flux which, linking a circuit of one turn, produces in it an
electromotive force of 1 &%Volt as it is reduced to zero at a uniform
rate in 1 &%SecondDuration.  &%Weber = V*s = m^2*kg*s^(-2)*A^(-1)." )

; Magnetic Flux Density Units

(subclass Tesla FunctionQuantity)
(instance Tesla SystemeInternationalUnit)
(documentation Tesla "SI magnetic flux density measure.  Symbol:  T.
One &%Tesla equals one &%Weber per square &%Meter.  &%Tesla = Wb/m^2 =
kg*s^(-2)*A^(-1).")

; Inductance Units

(subclass Henry FunctionQuantity)
(instance Henry SystemeInternationalUnit)
(documentation Henry "SI inductance measure.  Symbol: H.  One &%Henry
is equivalent to one &%Volt divided by one &%Ampere per &%SecondDuration.
If a current changing at the rate of one &%Ampere per &%SecondDuration
induces an electromotive force of one &%Volt, the circuit has an
inductance of one &%Henry.  &%Henry = Wb/A = m^2*kg*s^(-2)*A^(-2).")

; Celsius Temperature unit

(subclass CelsiusDegree TemperatureMeasure)
(instance CelsiusDegree SystemeInternationalUnit)
(documentation CelsiusDegree "A &%TemperatureMeasure.  The freezing point
and the boiling point of water are, respectively, 0 &%CelsiusDegrees and 100 &%CelsiusDegrees.")

(=>
   (instance ?NUMBER RealNumber)
   (equal
      (MeasureFn ?NUMBER CelsiusDegree)
      (MeasureFn (SubtractionFn ?NUMBER 273.15) KelvinDegree)))

(=>
   (instance ?NUMBER RealNumber)
   (equal
      (MeasureFn ?NUMBER CelsiusDegree)
      (MeasureFn (DivisionFn (SubtractionFn ?NUMBER 32) 1.8) FahrenheitDegree)))

; Luminous Flux Units

(subclass Lumen FunctionQuantity)
(instance Lumen SystemeInternationalUnit)
(documentation Lumen "SI luminous flux measure.  Symbol: lm.  It is the
amount streaming outward through one solid angle of 1 &%Steradian from a
uniform point source having an intensity of one &%Candela.  &%Lumen =
cd*sr = cd * 1.")

; Illuminance Units

(subclass Lux FunctionQuantity)
(instance Lux SystemeInternationalUnit)
(documentation Lux "SI illuminance measure.  Symbol: lx.  It is the
amount of illumination provided when one &%Lumen is evenly distributed
over an area of 1 square &%Meter. This is also equivalent to the
illumination that would exist on a surface all points of which are one
&%Meter from a point source of one &%Candela.  &%Lux = lm/m^2 =
m^(-2)*cd.")

; Activity Units

(subclass Becquerel TimeDependentQuantity)
(instance Becquerel SystemeInternationalUnit)
(documentation Becquerel "SI activity measure.  Symbol: Bq.  It measures
the amount of radioactivity contained in a given sample of matter. It is
that quantity of a radioactive element in which there is one atomic
disintegration per &%SecondDuration.  &%Becquerel = s^(-1).")

; Absorbed Dose Units

(subclass Gray FunctionQuantity)
(instance Gray SystemeInternationalUnit)
(documentation Gray "SI absorbed dose measure.  Symbol: Gy.  It measures
the dose of radiation absorbed in living tissue. It is equal approximately
to the absorbed dose delivered when the energy per unit mass imparted to
matter by ionizing radiation is 1 &%Joule per kilogram.  &%Gray = J/kg
= m^2*s^(-2).")

; Dose Equivalent Units

(subclass Sievert FunctionQuantity)
(instance Sievert SystemeInternationalUnit)
(documentation Sievert "SI dose equivalent measure.  Symbol: Sv.  It is
a unit of biologic dose of ionizing radiation.  The &%Sievert makes it
possible to normalize doses of different types of radiation. It takes
into account the relative biologic effectiveness of ionizing radiation,
since each form of such radiation--e.g., X rays, gamma rays, neutrons--
has a slightly different effect on living tissue for a given absorbed
dose. The dose equivalent of a given type of radiation (in &%Sievert) is
the dose of the radiation in &%Gray multiplied by a quality factor that
is based on the relative biologic effectiveness of the radiation.
Accordingly, one &%Sievert is generally defined as the amount of radiation
roughly equivalent in biologic effectiveness to one &%Gray of gamma
radiation.  &%Sievert = J/kg = m^2*s^(-2)")

; Units that are accepted for -use- with SI

(subclass DayDuration TimeDuration)
(instance DayDuration UnitOfMeasure)
(documentation DayDuration "Time unit. 1 day = 24 hours.")

(=>
   (instance ?NUMBER RealNumber)
   (equal
   (MeasureFn ?NUMBER DayDuration)
   (MeasureFn (MultiplicationFn ?NUMBER 24) HourDuration)))

(subclass HourDuration TimeDuration)
(instance HourDuration UnitOfMeasure)
(documentation HourDuration "Time unit. 1 hour = 60 minutes.")

(=>
  (instance ?NUMBER RealNumber)
  (equal
     (MeasureFn ?NUMBER HourDuration)
     (MeasureFn (MultiplicationFn ?NUMBER 60) MinuteDuration)))

(subclass MinuteDuration TimeDuration)
(instance MinuteDuration UnitOfMeasure)
(documentation MinuteDuration "Time unit. 1 minute = 60 seconds. ")

(=>
   (instance ?NUMBER RealNumber)
   (equal
       (MeasureFn ?NUMBER MinuteDuration)
       (MeasureFn (MultiplicationFn ?NUMBER 60) SecondDuration)))

(subclass WeekDuration TimeDuration)
(instance WeekDuration UnitOfMeasure)
(documentation WeekDuration "Time unit.  A week's duration is seven days.")

(=>
   (instance ?NUMBER RealNumber)
   (equal
      (MeasureFn ?NUMBER WeekDuration)
      (MeasureFn (MultiplicationFn ?NUMBER 7) DayDuration)))

(subclass YearDuration TimeDuration)
(instance YearDuration UnitOfMeasure)
(documentation YearDuration "Time unit. one calendar year. 1 year =
365 days = 31536000 seconds.")

(=>
   (instance ?NUMBER RealNumber)
   (equal
     (MeasureFn ?NUMBER YearDuration)
     (MeasureFn (MultiplicationFn ?NUMBER 365) DayDuration)))

;; What follows are units that are also accepted for use with SI.  The
;; SI equivalents for these units are obtained experimentally.

(subclass Amu MassMeasure)
(instance Amu UnitOfMeasure)
(documentation Amu "Atomic mass unit.  Symbol: u. It is the mass of
the twelfth part of an atom of the Carbon 12 isotope.")

(=>
   (instance ?NUMBER RealNumber)
   (equal
        (MeasureFn ?NUMBER Amu)
        (MeasureFn (MultiplicationFn ?NUMBER 1.6605402E-24) Gram)))

(subclass ElectronVolt FunctionQuantity)
(instance ElectronVolt UnitOfMeasure)
(documentation ElectronVolt "The &%ElectronVolt is an energy measure.
Symbol: eV.  It is the kinetic energy acquired by an electron in passing
through a potential difference of 1 &%Volt in a vacuum.")

(=>
   (instance ?NUMBER RealNumber)
   (equal
        (MeasureFn ?NUMBER ElectronVolt)
        (MeasureFn (MultiplicationFn ?NUMBER 1.60217733E-19) Joule)))

;; The following units have been temporarily accepted for use with
;; SI units.

(subclass Angstrom LengthMeasure)
(instance Angstrom UnitOfMeasure)
(documentation Angstrom "The &%Angstrom is a &%LengthMeasure.
1 &%Angstrom = 10^(-10) m")

(=>
   (instance ?NUMBER RealNumber)
   (equal
        (MeasureFn ?NUMBER Angstrom)
        (MeasureFn (MultiplicationFn ?NUMBER 1.0E-10) Meter)))

;; The following units are unacceptable in SI but are part of other
;; systems of measurement that are widely used.

;; More Length units

(subclass FootLength LengthMeasure)
(instance FootLength UnitOfMeasure)
(documentation FootLength "English length unit of feet.")

(=>
   (instance ?NUMBER RealNumber)
   (equal
        (MeasureFn ?NUMBER FootLength)
        (MeasureFn (MultiplicationFn ?NUMBER 0.3048) Meter)))

(subclass Inch LengthMeasure)
(instance Inch UnitOfMeasure)
(documentation Inch "English length unit of inches.")

(=>
   (instance ?NUMBER RealNumber)
   (equal
        (MeasureFn ?NUMBER Inch)
        (MeasureFn (MultiplicationFn ?NUMBER 0.0254) Meter)))

(subclass Mile LengthMeasure)
(instance Mile UnitOfMeasure)
(documentation Mile "English length unit of miles.")

(=>
   (instance ?NUMBER RealNumber)
   (equal
        (MeasureFn ?NUMBER Mile)
        (MeasureFn (MultiplicationFn ?NUMBER 1609.344) Meter)))

;; More Volume units

(subclass UnitedStatesGallon VolumeMeasure)
(instance UnitedStatesGallon UnitOfMeasure)
(relatedInternalConcept UnitedStatesGallon UnitedKingdomGallon)
(documentation UnitedStatesGallon "Unit of volume commonly used in the
United States.")

(=>
   (instance ?NUMBER RealNumber)
   (equal
        (MeasureFn ?NUMBER UnitedStatesGallon)
        (MeasureFn (MultiplicationFn ?NUMBER 3.785411784) Liter)))

(subclass Quart VolumeMeasure)
(instance Quart UnitOfMeasure)
(documentation Quart "English unit of volume equal to 1/4 of a
&%UnitedStatesGallon.")

(=>
   (instance ?NUMBER RealNumber)
   (equal
        (MeasureFn ?NUMBER Quart)
        (MeasureFn (DivisionFn ?NUMBER 4) UnitedStatesGallon)))

(subclass Pint VolumeMeasure)
(instance Pint UnitOfMeasure)
(documentation Pint "English unit of volume equal to 1/2 of a
&%Quart.")

(=>
   (instance ?NUMBER RealNumber)
   (equal
        (MeasureFn ?NUMBER Pint)
        (MeasureFn (DivisionFn ?NUMBER 2) Quart)))

(subclass CupMeasure VolumeMeasure)
(instance CupMeasure UnitOfMeasure)
(documentation CupMeasure "English unit of volume equal to 1/2 of a
&%Pint.")

(=>
   (instance ?NUMBER RealNumber)
   (equal
        (MeasureFn ?NUMBER Cup)
        (MeasureFn (DivisionFn ?NUMBER 2) Pint)))

(subclass Ounce VolumeMeasure)
(instance Ounce UnitOfMeasure)
(documentation Ounce "English unit of volume equal to 1/8 of a
&%Cup.")

(=>
   (instance ?NUMBER RealNumber)
   (equal
        (MeasureFn ?NUMBER Ounce)
        (MeasureFn (DivisionFn ?NUMBER 8) Cup)))

(subclass UnitedKingdomGallon VolumeMeasure)
(instance UnitedKingdomGallon UnitOfMeasure)
(documentation UnitedKingdomGallon "Unit of volume commonly used in the
United Kingdom.")

(=>
   (instance ?NUMBER RealNumber)
   (equal
        (MeasureFn ?NUMBER UnitedKingdomGallon)
        (MeasureFn (MultiplicationFn ?NUMBER 4.54609) Liter)))

;; More Mass units

(subclass AtomGram MassMeasure)
(instance AtomGram UnitOfMeasure)
(documentation AtomGram "&%MassMeasure that is also known as the gram-atom.
Defined as the mass in grams of 1 &%Mole of pure substance.  For example,
1 &%AtomGram of Carbon 12 will be 12 &%Grams of pure Carbon 12.  2 &%AtomGrams
of the same substance will be 24 &%Grams of it.  This is an unusual unit in
that it is essentially 1 &%Mole of 'stuff' measured in grams, so that the
actual value (i.e. mass) depends on the type of substance.")

(subclass PoundMass MassMeasure)
(instance PoundMass UnitOfMeasure)
(documentation PoundMass "English mass unit of pounds.")

(=>
   (instance ?NUMBER RealNumber)
   (equal
        (MeasureFn ?NUMBER PoundMass)
        (MeasureFn (MultiplicationFn ?NUMBER 453.59237) Gram)))

(subclass Slug MassMeasure)
(instance Slug UnitOfMeasure)
(documentation Slug "English mass unit of slugs.")

(=>
   (instance ?NUMBER RealNumber)
   (equal
        (MeasureFn ?NUMBER Slug)
        (MeasureFn (MultiplicationFn ?NUMBER 14593.90) Gram)))

;; More Temperature units

(subclass RankineDegree TemperatureMeasure)
(instance RankineDegree UnitOfMeasure)
(documentation RankineDegree "A &%TemperatureMeasure.  Note
that 0 &%RankineDegrees is the same as the absolute zero (i.e. 0
&%KelvinDegrees).")

(=>
   (instance ?NUMBER RealNumber)
   (equal
        (MeasureFn ?NUMBER RankineDegree)
        (MeasureFn (MultiplicationFn ?NUMBER 1.8) KelvinDegree)))

(subclass FahrenheitDegree TemperatureMeasure)
(instance FahrenheitDegree UnitOfMeasure)
(documentation FahrenheitDegree "A &%TemperatureMeasure that is commonly
used in the United States.  On the Fahrenheit scale, the freezing point
of water is 32 &%FahrenheitDegrees, and the boiling point of water is
212 &%FahrenheitDegrees.")

;; More Force units

(subclass PoundForce FunctionQuantity)
(instance PoundForce UnitOfMeasure)
(documentation PoundForce "English pound of force. The conversion
factor depends on the local value of the acceleration of free fall. A
mean value is used in the conversion axiom associated with this
constant.")

(=>
   (instance ?NUMBER RealNumber)
   (equal
        (MeasureFn ?NUMBER PoundForce)
        (MeasureFn (MultiplicationFn ?NUMBER 4.448222) Newton)))

;; More Energy units

(subclass Calorie FunctionQuantity)
(instance Calorie UnitOfMeasure)
(documentation Calorie "A &%Calorie is an energy measure.")

(=>
   (instance ?NUMBER RealNumber)
   (equal
        (MeasureFn ?NUMBER Calorie)
        (MeasureFn (MultiplicationFn ?NUMBER 4.1868) Joule)))

(subclass BritishThermalUnit FunctionQuantity)
(instance BritishThermalUnit UnitOfMeasure)
(documentation BritishThermalUnit "An energy measure.")

(=>
   (instance ?NUMBER RealNumber)
   (equal
        (MeasureFn ?NUMBER BritishThermalUnit)
        (MeasureFn (MultiplicationFn ?NUMBER 1055.05585262) Joule)))

;; More plane angle units

(subclass AngularDegree PlaneAngleMeasure)
(instance AngularDegree UnitOfMeasure)
(documentation AngularDegree "A plane angle measure.")

(=>
   (instance ?NUMBER RealNumber)
   (equal
     (MeasureFn ?NUMBER AngularDegree)
     (MeasureFn (MultiplicationFn ?NUMBER (DivisionFn Pi 180)) Radian)))

(=>
   (measure ?ANGLE (MeasureFn ?NUMBER AngularDegree))
   (and
      (greaterThanOrEqualTo ?NUMBER 0)
      (lessThanOrEqualTo ?NUMBER 360)))

(equal (MeasureFn 0 AngularDegree) (MeasureFn 360 AngularDegree))

; Other interesting units of measure

; Currency units

(subclass UnitedStatesDollar CurrencyMeasure)
(instance UnitedStatesDollar UnitOfCurrency)
(documentation UnitedStatesDollar "A currency measure.")

(subclass UnitedStatesCent CurrencyMeasure)
(instance UnitedStatesCent UnitOfCurrency)
(documentation UnitedStatesCent "A currency measure.  1 &%UnitedStatesCent is
equal to .01 &%UnitedStatesDollars.")

(=>
   (instance ?NUMBER RealNumber)
   (equal
        (MeasureFn ?NUMBER UnitedStatesCent)
        (MeasureFn (MultiplicationFn ?NUMBER 0.01) UnitedStatesDollar)))

(subclass EuroDollar CurrencyMeasure)
(instance EuroDollar UnitOfCurrency)
(documentation EuroDollar "A currency measure of most European Union countries.
It is based on the &%UnitedStatesDollar.")

(subclass EuroCent CurrencyMeasure)
(instance EuroCent UnitOfCurrency)
(documentation EuroCent "A currency measure.  1 &%EuroCent is equal to .01
&%EuroDollars.")

(=>
   (instance ?NUMBER RealNumber)
   (equal
        (MeasureFn ?NUMBER EuroCent)
        (MeasureFn (MultiplicationFn ?NUMBER 0.01) EuroDollar)))

; Information units

(subclass InformationMeasure ConstantQuantity)
(documentation InformationMeasure "Measures of the amount of information.
Includes &%Bit, &%Byte, and multiples of these, e.g. &%KiloByte and
&%MegaByte.")

(subclass Bit InformationMeasure)
(instance Bit UnitOfMeasure)
(documentation Bit "One &%Bit of information.  A one or a zero.")

(subclass Byte InformationMeasure)
(instance Byte UnitOfMeasure)
(documentation Byte "One &%Byte of information.  A &%Byte is eight
&%Bits.")

(=>
   (instance ?NUMBER RealNumber)
   (equal
        (MeasureFn ?NUMBER Byte)
        (MeasureFn (MultiplicationFn ?NUMBER 8) Bit)))

(subclass KiloByte InformationMeasure)
(instance KiloByte UnitOfMeasure)
(documentation KiloByte "One &%KiloByte (KB) of information.  One
&%KiloByte is 1024 &%Bytes.  Note that this sense of 'kilo' is
different from the one accepted in the SI system.")

(=>
   (instance ?NUMBER RealNumber)
   (equal
        (MeasureFn ?NUMBER KiloByte)
        (MeasureFn (MultiplicationFn ?NUMBER 1024) Byte)))

(subclass MegaByte InformationMeasure)
(instance MegaByte UnitOfMeasure)
(documentation MegaByte "One &%MegaByte (MB) of information.  One
&%MegaByte is 1024 &%KiloBytes.  Note that this sense of 'mega' is
different from the one accepted in the SI system.")

(=>
   (instance ?NUMBER RealNumber)
   (equal
        (MeasureFn ?NUMBER MegaByte)
        (MeasureFn (MultiplicationFn ?NUMBER 1024) KiloByte)))

;; The following content was inspired by the Quantities ontology
;; developed by ITBM-CNR.

(instance measure BinaryPredicate)
(instance measure AsymmetricRelation)
(domain measure 1 Object)
(domain measure 2 PhysicalQuantity)
(documentation measure "A very general &%Predicate for asserting that a
particular &%Object is measured by a particular &%ConstantQuantity.
In general, the second argument of this &%Predicate will be an instance
of the &%Function &%MeasureFn.")

(instance age SingleValuedRelation)
(subrelation age measure)
(domain age 2 TimeDuration)
(documentation age "Simply relates an &%Object to a &%ConstantQuantity
specifying the age of the &%Object.")

(subrelation length measure)
(domain length 2 LengthMeasure)
(documentation length "&%BinaryPredicate that is used to state the measure
of an &%Object from one point to another point along its surface.  Note
that the difference between the predicates &%length and &%distance is that
the &%length is used to state the &%LengthMeasure of one of the dimensions of
a single object, while &%distance is used to state the &%LengthMeasure that
separates two distinct objects")

(instance width SingleValuedRelation)
(subrelation width length)
(documentation width "&%BinaryPredicate that is used to state the measure
of an &%Object from side to side at its widest span.")

(instance distance SingleValuedRelation)
(instance distance SpatialRelation)
(instance distance TernaryPredicate)
(domain distance 1 Physical)
(domain distance 2 Physical)
(domain distance 3 LengthMeasure)
(documentation distance "(&%distance ?OBJ1 ?OBJ2 ?QUANT) means that the
shortest distance between the two objects ?OBJ1 and ?OBJ2 is ?QUANT.  Note
that the difference between the predicates &%length and &%distance is that
the &%length is used to state the &%LengthMeasure of one of the dimensions of
a single object, while &%distance is used to state the &%LengthMeasure that
separates two distinct objects.")

(=>
   (distance ?OBJ1 ?OBJ2 ?QUANT)
   (distance ?OBJ2 ?OBJ1 ?QUANT))

(subrelation altitude distance)
(instance altitude SingleValuedRelation)
(documentation altitude "A &%TernaryPredicate that is used to state the &%distance
between the &%top of an &%Object and another point that is below the &%top of the
&%Object (often this other point will be sea level).  Note that this &%Predicate can
be used to specify, for example, the height of geographic features, e.g. mountains,
the altitude of aircraft, and the orbit of satellites around the Earth.")

(=>
   (altitude ?OBJ1 ?OBJ2 ?HEIGHT)
   (orientation ?OBJ1 ?OBJ2 Above))

(=>
   (altitude ?OBJ1 ?OBJ2 ?HEIGHT)
   (exists (?TOP)
      (and
         (top ?TOP ?OBJ1)
         (distance ?TOP ?OBJ2 ?HEIGHT))))

(subrelation depth distance)
(instance depth SingleValuedRelation)
(documentation depth "A &%TernaryPredicate that is used to state the &%distance
between the &%top of an &%Object and another point that is above the &%top of the
&%Object (often this other point will be sea level).  Note that this &%Predicate can
be used to specify, for example, the depth of marine life or submarines, for example.")

(=>
   (depth ?OBJ1 ?OBJ2 ?DEPTH)
   (orientation ?OBJ1 ?OBJ2 Below))

(=>
   (depth ?OBJ1 ?OBJ2 ?DEPTH)
   (exists (?BOTTOM)
      (and
         (bottom ?BOTTOM ?OBJ1)
         (distance ?BOTTOM ?OBJ2 ?DEPTH))))

(instance larger BinaryPredicate)
(instance larger SpatialRelation)
(instance larger TransitiveRelation)
(instance larger IrreflexiveRelation)
(domain larger 1 Object)
(domain larger 2 Object)
(documentation larger "(&%larger ?OBJ1 ?OBJ2) simply means that ?OBJ1 is
larger, with respect to all &%LengthMeasures, than ?OBJ2.")

(=>
   (larger ?OBJ1 ?OBJ2)
   (forall (?QUANT1 ?QUANT2)
      (=>
         (and
            (measure ?OBJ1 (MeasureFn ?QUANT1 LengthMeasure))
            (measure ?OBJ2 (MeasureFn ?QUANT2 LengthMeasure)))
         (greaterThan ?QUANT1 ?QUANT2))))

(instance smaller BinaryPredicate)
(instance smaller SpatialRelation)
(instance smaller TransitiveRelation)
(instance smaller IrreflexiveRelation)
(domain smaller 1 Object)
(domain smaller 2 Object)
(inverse smaller larger)
(documentation smaller "(&%smaller ?OBJ1 ?OBJ2) simply means that ?OBJ1
is smaller, with respect to all &%LengthMeasures, than ?OBJ2.")

(instance monetaryValue SingleValuedRelation)
(subrelation monetaryValue measure)
(domain monetaryValue 1 Object)
(domain monetaryValue 2 CurrencyMeasure)
(documentation monetaryValue "A &%BinaryPredicate that associates an
&%Object with its value expressed as an instance of &%CurrencyMeasure.")

(instance WealthFn UnaryFunction)
(domain WealthFn 1 Agent)
(range WealthFn CurrencyMeasure)
(documentation WealthFn "A &%UnaryFunction that maps an &%Agent to a
&%CurrencyMeasure specifying the value of the property owned by the &%Agent.
Note that this &%Function is generally used in conjunction with the
&%Function &%PropertyFn, e.g. (&%WealthFn (&%PropertyFn BillGates)) would
return the monetary value of the sum of Bill Gates' holdings.")

(<=>
   (equal (WealthFn ?PERSON) ?AMOUNT)
   (monetaryValue (PropertyFn ?PERSON) ?AMOUNT))

;; END FILE

;; <module>TEMPORAL_CONCEPTS</module>
;; BEGIN FILE


;;;;;;;;;;;;;;;;;;;;;;;
;; TEMPORAL CONCEPTS ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; INCLUDES 'STRUCTURAL ONTOLOGY'
;; INCLUDES 'BASE ONTOLOGY'

;; The first part of this section contains definitions relations involving
;; temporal notions.  Most of these definitions and axioms were derived from
;; James Allen's work.  This part of the section was extensively revised on
;; the basis of comments from Pat Hayes.  The second part of this section
;; is an attempt to incorporate the Simple-Time ontology from the
;; Ontolingua server into the SUMO.

(instance PositiveInfinity TimePoint)
(documentation PositiveInfinity "The &%TimePoint that is after
all other &%TimePoints.")

(=>
   (and
      (instance ?POINT TimePoint)
      (not (equal ?POINT PositiveInfinity)))
   (before ?POINT PositiveInfinity))

(=>
   (and
      (instance ?POINT TimePoint)
      (not (equal ?POINT PositiveInfinity)))
   (exists (?OTHERPOINT)
      (temporallyBetween ?POINT ?OTHERPOINT PositiveInfinity)))

(instance NegativeInfinity TimePoint)
(documentation NegativeInfinity "The &%TimePoint that is before
all other &%TimePoints.")

(=>
   (and
      (instance ?POINT TimePoint)
      (not (equal ?POINT NegativeInfinity)))
    (before NegativeInfinity ?POINT))

(=>
   (and
      (instance ?POINT TimePoint)
      (not (equal ?POINT NegativeInfinity)))
   (exists (?OTHERPOINT)
      (temporallyBetween NegativeInfinity ?OTHERPOINT ?POINT)))

(instance duration BinaryPredicate)
(instance duration AsymmetricRelation)
(instance duration TotalValuedRelation)
(domain duration 1 TimeInterval)
(domain duration 2 TimeDuration)
(documentation duration "(&%duration ?POS ?TIME) means that the
duration of the &%TimePosition ?POS is ?TIME.  Note that this
&%Predicate can be used in conjunction with the &%Function &%WhenFn
to specify the duration of any instance of &%Physical.")

(instance frequency BinaryPredicate)
(instance frequency AsymmetricRelation)
(domainSubclass frequency 1 Process)
(domain frequency 2 TimeDuration)
(documentation frequency "(frequency ?PROC ?TIME) means that the
&%Process type of ?PROC recurs after every interval of ?TIME.")

(=>
   (frequency ?PROC ?TIME1)
   (forall (?TIME2)
      (=>
         (duration ?TIME2 ?TIME1)
         (exists (?POSITION)
            (and
               (temporalPart ?POSITION ?TIME2)
               (holdsDuring ?POSITION (exists (?INST) (instance ?INST ?PROC))))))))

(instance temporalPart BinaryPredicate)
(instance temporalPart TemporalRelation)
(instance temporalPart PartialOrderingRelation)
(domain temporalPart 1 TimePosition)
(domain temporalPart 2 TimePosition)
(documentation temporalPart "The temporal analogue of the spatial &%part predicate.
(&%temporalPart ?POS1 ?POS2) means that &%TimePosition ?POS1 is part of &%TimePosition ?POS2.  Note that since &%temporalPart is a &%ReflexiveRelation every &%TimePostion is a
&%temporalPart of itself.")

(=>
   (instance ?POINT TimePoint)
   (exists (?INTERVAL)
      (and
         (instance ?INTERVAL TimeInterval)
         (temporalPart ?POINT ?INTERVAL))))

(=>
   (instance ?INTERVAL TimeInterval)
   (exists (?POINT)
      (and
         (instance ?POINT TimePoint)
         (temporalPart ?POINT ?INTERVAL))))

(=>
   (and
      (holdsDuring ?TIME1 ?SITUATION)
      (temporalPart ?TIME2 ?TIME1))
   (holdsDuring ?TIME2 ?SITUATION))

(=>
   (and
      (holdsDuring ?INTERVAL (holds ?REL ?INST1 ?INST2))
      (instance ?INST1 Physical)
      (instance ?INST2 Physical))
   (and
      (time ?INST1 ?INTERVAL)
      (time ?INST2 ?INTERVAL)))

(<=>
   (temporalPart ?POS (WhenFn ?THING))
   (time ?THING ?POS))

(instance BeginFn TemporalRelation)
(instance BeginFn UnaryFunction)
(instance BeginFn TotalValuedRelation)
(domain BeginFn 1 TimeInterval)
(range BeginFn TimePoint)
(documentation BeginFn "A &%UnaryFunction that maps a &%TimeInterval to
the &%TimePoint at which the interval begins.")

(=>
   (origin ?PROCESS ?OBJ)
   (located (WhereFn ?PROCESS (BeginFn (WhenFn ?PROCESS))) (WhereFn ?OBJ (BeginFn (WhenFn ?OBJ)))))

(=>
   (equal (BeginFn ?INTERVAL) ?POINT)
   (forall (?OTHERPOINT)
      (=>
         (and
            (temporalPart ?OTHERPOINT ?INTERVAL)
            (not (equal ?OTHERPOINT ?POINT)))
         (before ?POINT ?OTHERPOINT))))

(instance EndFn TemporalRelation)
(instance EndFn UnaryFunction)
(instance EndFn TotalValuedRelation)
(domain EndFn 1 TimeInterval)
(range EndFn TimePoint)
(documentation EndFn "A &%UnaryFunction that maps a &%TimeInterval to
the &%TimePoint at which the interval ends.")

(=>
   (equal (EndFn ?INTERVAL) ?POINT)
   (forall (?OTHERPOINT)
      (=>
         (and
            (temporalPart ?OTHERPOINT ?INTERVAL)
            (not (equal ?OTHERPOINT ?POINT)))
         (before ?OTHERPOINT ?POINT))))

(=>
     (and
          (resource ?PROC ?OBJ)
          (holdsDuring (BeginFn (WhenFn ?PROC)) (measure ?OBJ ?QUANT1))
          (holdsDuring (EndFn (WhenFn ?PROC)) (measure ?OBJ ?QUANT2)))
     (greaterThan ?QUANT1 ?QUANT2))

(instance starts BinaryPredicate)
(instance starts TemporalRelation)
(instance starts TransitiveRelation)
(instance starts IrreflexiveRelation)
(domain starts 1 TimeInterval)
(domain starts 2 TimeInterval)
(documentation starts "(&%starts ?INTERVAL1 ?INTERVAL2) means that
?INTERVAL1 and ?INTERVAL2 are both &%TimeIntervals that have the same
initial &%TimePoint and that ?INTERVAL1 ends before ?INTERVAL2.")

(<=>
   (starts ?INTERVAL1 ?INTERVAL2)
   (and
      (equal
         (BeginFn ?INTERVAL1)
         (BeginFn ?INTERVAL2))
      (before
         (EndFn ?INTERVAL1)
         (EndFn ?INTERVAL2))))

(instance finishes BinaryPredicate)
(instance finishes TemporalRelation)
(instance finishes TransitiveRelation)
(instance finishes IrreflexiveRelation)
(domain finishes 1 TimeInterval)
(domain finishes 2 TimeInterval)
(documentation finishes "(&%finishes ?INTERVAL1 ?INTERVAL2) means that
?INTERVAL1 and ?INTERVAL2 are both &%TimeIntervals that have the same
ending &%TimePoint and that ?INTERVAL2 begins before ?INTERVAL1.")

(<=>
   (finishes ?INTERVAL1 ?INTERVAL2)
   (and
      (before
         (BeginFn ?INTERVAL2)
         (BeginFn ?INTERVAL1))
      (equal
         (EndFn ?INTERVAL2)
         (EndFn ?INTERVAL1))))

(instance before TemporalRelation)
(instance before IrreflexiveRelation)
(instance before TransitiveRelation)
(subrelation before beforeOrEqual)
(relatedInternalConcept before earlier)
(domain before 1 TimePoint)
(domain before 2 TimePoint)
(documentation before "(&%before ?POINT1 ?POINT2) means that ?POINT1
precedes ?POINT2 on the universal timeline.")

;; An Object exists (and, hence, retains its identity) over time, i.e.,
;; an object exists at every point over some interval of time.

(=>
    (instance ?OBJ Object)
    (exists (?TIME1 ?TIME2)
       (and
         (instance ?TIME1 TimePoint)
         (instance ?TIME2 TimePoint)
         (before ?TIME1 ?TIME2)
         (forall (?TIME)
           (=>
             (and
                (beforeOrEqual ?TIME1 ?TIME)
                (beforeOrEqual ?TIME ?TIME2))
                (time ?OBJ ?TIME))))))

(=>
   (result ?PROC ?OBJ)
   (forall (?TIME)
      (=>
         (before ?TIME (BeginFn (WhenFn ?PROC)))
         (not
            (time ?OBJ ?TIME)))))

(=>
   (instance ?INTERVAL TimeInterval)
   (before (BeginFn ?INTERVAL) (EndFn ?INTERVAL)))

(instance beforeOrEqual BinaryPredicate)
(instance beforeOrEqual TemporalRelation)
(instance beforeOrEqual PartialOrderingRelation)
(domain beforeOrEqual 1 TimePoint)
(domain beforeOrEqual 2 TimePoint)
(documentation beforeOrEqual "(&%beforeOrEqual ?POINT1 ?POINT2) means that ?POINT1
is identical with ?POINT2 or occurs before it on the universal timeline.")

(=>
   (beforeOrEqual ?POINT1 ?POINT2)
   (or
       (before ?POINT1 ?POINT2)
       (equal ?POINT1 ?POINT2)))

(instance temporallyBetween TemporalRelation)
(instance temporallyBetween TernaryPredicate)
(subrelation temporallyBetween temporallyBetweenOrEqual)
(domain temporallyBetween 1 TimePoint)
(domain temporallyBetween 2 TimePoint)
(domain temporallyBetween 3 TimePoint)
(documentation temporallyBetween "(&%temporallyBetween ?POINT1 ?POINT2
?POINT3) means that the &%TimePoint ?POINT2 is between the &%TimePoints
?POINT1 and ?POINT3, i.e. ?POINT1 is before ?POINT2 and ?POINT2 is before
?POINT3.")

(<=>
   (temporallyBetween ?POINT1 ?POINT2 ?POINT3)
   (and
      (before ?POINT1 ?POINT2)
      (before ?POINT2 ?POINT3)))

(instance temporallyBetweenOrEqual TemporalRelation)
(instance temporallyBetweenOrEqual TernaryPredicate)
(domain temporallyBetweenOrEqual 1 TimePoint)
(domain temporallyBetweenOrEqual 2 TimePoint)
(domain temporallyBetweenOrEqual 3 TimePoint)
(documentation temporallyBetweenOrEqual "(&%temporallyBetweenOrEqual ?POINT1 ?POINT2
?POINT3) means that the &%TimePoint ?POINT1 is before or equal to the
&%TimePoint ?POINT2 and ?POINT2 is before or equal to the &%TimePoint
?POINT3.")

(<=>
   (temporallyBetweenOrEqual ?POINT1 ?POINT2 ?POINT3)
   (and
      (beforeOrEqual ?POINT1 ?POINT2)
      (beforeOrEqual ?POINT2 ?POINT3)))

(<=>
   (and
      (time ?PHYS ?TIME)
      (instance ?TIME TimePoint))
   (temporallyBetweenOrEqual (BeginFn (WhenFn ?PHYS)) ?TIME (EndFn (WhenFn ?PHYS))))

(instance overlapsTemporally BinaryPredicate)
(instance overlapsTemporally TemporalRelation)
(instance overlapsTemporally ReflexiveRelation)
(instance overlapsTemporally SymmetricRelation)
(domain overlapsTemporally 1 TimeInterval)
(domain overlapsTemporally 2 TimeInterval)
(documentation overlapsTemporally "(&%overlapsTemporally ?INTERVAL1
?INTERVAL2) means that the &%TimeIntervals ?INTERVAL1 and ?INTERVAL2
have a &%TimeInterval as a common part.")

(<=>
   (overlapsTemporally ?INTERVAL1 ?INTERVAL2)
   (exists (?INTERVAL3)
      (and
         (instance ?INTERVAL3 TimeInterval)
         (temporalPart ?INTERVAL3 ?INTERVAL1)
         (temporalPart ?INTERVAL3 ?INTERVAL2))))

(=>
   (and
      (instance ?REL SpatialRelation)
      (holds ?REL ?OBJ1 ?OBJ2))
   (overlapsTemporally (WhenFn ?OBJ1) (WhenFn ?OBJ2)))

(subrelation during temporalPart)
(instance during TransitiveRelation)
(instance during IrreflexiveRelation)
(subrelation during overlapsTemporally)
(domain during 1 TimeInterval)
(domain during 2 TimeInterval)
(documentation during "(&%during ?INTERVAL1 ?INTERVAL2) means that
?INTERVAL1 starts after and ends before ?INTERVAL2.")

(=>
   (during ?INTERVAL1 ?INTERVAL2)
   (and
      (before (EndFn ?INTERVAL1) (EndFn ?INTERVAL2))
      (before (BeginFn ?INTERVAL2) (BeginFn ?INTERVAL1))))

(instance meetsTemporally BinaryPredicate)
(instance meetsTemporally TemporalRelation)
(instance meetsTemporally AsymmetricRelation)
(instance meetsTemporally IntransitiveRelation)
(domain meetsTemporally 1 TimeInterval)
(domain meetsTemporally 2 TimeInterval)
(documentation meetsTemporally "(&%meetsTemporally ?INTERVAL1 ?INTERVAL2)
means that the terminal point of the &%TimeInterval ?INTERVAL1 is the
initial point of the &%TimeInterval ?INTERVAL2.")

(<=>
   (meetsTemporally ?INTERVAL1 ?INTERVAL2)
   (equal
      (EndFn ?INTERVAL1)
      (BeginFn ?INTERVAL2)))

(=>
  (and
    (equal
      (BeginFn ?INTERVAL1)
      (BeginFn ?INTERVAL2))
    (equal
      (EndFn ?INTERVAL1)
      (EndFn ?INTERVAL2)))
    (equal ?INTERVAL1 ?INTERVAL2))

(instance earlier BinaryPredicate)
(instance earlier TemporalRelation)
(instance earlier TransitiveRelation)
(instance earlier IrreflexiveRelation)
(domain earlier 1 TimeInterval)
(domain earlier 2 TimeInterval)
(documentation earlier "(&%earlier ?INTERVAL1 ?INTERVAL2) means that
the &%TimeInterval ?INTERVAL1 ends before the &%TimeInterval ?INTERVAL2
begins.")

(<=>
    (earlier ?INTERVAL1 ?INTERVAL2)
    (before (EndFn ?INTERVAL1) (BeginFn ?INTERVAL2)))

(instance cooccur BinaryPredicate)
(instance cooccur TemporalRelation)
(instance cooccur EquivalenceRelation)
(domain cooccur 1 Physical)
(domain cooccur 2 Physical)
(documentation cooccur "(&%cooccur ?THING1 ?THING2) means that the
&%Object or &%Process ?THING1 occurs at the same time as, together with,
or jointly with the &%Object or &%Process ?THING2.  This covers the
following temporal relations:  is co-incident with, is concurrent with,
is contemporaneous with, and is concomitant with.")

(<=>
   (cooccur ?PHYS1 ?PHYS2)
   (equal (WhenFn ?PHYS1) (WhenFn ?PHYS2)))

;; The following functions generate &%TimeIntervals.

(instance TimeIntervalFn BinaryFunction)
(instance TimeIntervalFn TemporalRelation)
(domain TimeIntervalFn 1 TimePoint)
(domain TimeIntervalFn 2 TimePoint)
(range TimeIntervalFn TimeInterval)
(documentation TimeIntervalFn "A &%BinaryFunction that takes two &%TimePoints
as arguments and returns the &%TimeInterval defined by these two &%TimePoints.
Note that the first &%TimePoint must occur earlier than the second &%TimePoint.")

(=>
   (and
      (instance ?POINT1 TimePoint)
      (instance ?POINT2 TimePoint)
      (instance ?INTERVAL TimeInterval)
      (equal (TimeIntervalFn ?POINT1 ?POINT2) ?INTERVAL))
   (and
      (equal (BeginFn ?INTERVAL) ?POINT1)
      (equal (EndFn ?INTERVAL) ?POINT2)))

(=>
   (and
      (instance ?POINT1 TimePoint)
      (instance ?POINT2 TimePoint)
      (instance ?INTERVAL TimeInterval)
      (equal (TimeIntervalFn ?POINT1 ?POINT2) ?INTERVAL))
   (forall (?POINT)
      (<=>
         (temporallyBetweenOrEqual ?POINT1 ?POINT ?POINT2)
         (temporalPart ?POINT ?INTERVAL))))

(instance RecurrentTimeIntervalFn TemporalRelation)
(instance RecurrentTimeIntervalFn BinaryFunction)
(domainSubclass RecurrentTimeIntervalFn 1 TimeInterval)
(domainSubclass RecurrentTimeIntervalFn 2 TimeInterval)
(rangeSubclass RecurrentTimeIntervalFn RecurrentTimeInterval)
(documentation RecurrentTimeIntervalFn "A function that is useful for generating
recurring time intervals.  For example, (&%RecurrentTimeIntervalFn (&%HourFn 6 &%Day) (&%HourFn 12 &%Day)) returns the &%Class of &%TimeIntervals beginning at 6 in the morning and ending at 12 noon.  For another example, (&%RecurrentTimeInterval &%Saturday &%Sunday) returns the &%Class of all weekends.  For still another example,(&%RecurrentTimeInterval &%June &%August) returns the &%Class containing the academic summer period.")

(=>
   (instance ?INTERVAL (RecurrentTimeIntervalFn ?TIMECLASS1 ?TIMECLASS2))
   (exists (?TIME1 ?TIME2)
      (and
         (instance ?TIME1 ?TIMECLASS1)
         (instance ?TIME2 ?TIMECLASS2)
         (starts ?TIME1 ?INTERVAL)
         (finishes ?TIME2 ?INTERVAL))))

(instance WhenFn TemporalRelation)
(instance WhenFn UnaryFunction)
(instance WhenFn TotalValuedRelation)
(domain WhenFn 1 Physical)
(range WhenFn TimeInterval)
(documentation WhenFn "A &%UnaryFunction that maps an &%Object or
&%Process to the exact &%TimeInterval during which it exists.  Note
that, for every &%TimePoint ?TIME outside of the &%TimeInterval
(WhenFn ?THING), (time ?THING ?TIME) does not hold.")

(instance PastFn TemporalRelation)
(instance PastFn UnaryFunction)
(instance PastFn TotalValuedRelation)
(domain PastFn 1 TimePosition)
(range PastFn TimeInterval)
(documentation PastFn "A &%UnaryFunction that maps a &%TimePosition
to the &%TimeInterval that meets it and that begins at
&%NegativeInfinity.")

(=>
   (instance ?INTERVAL TimeInterval)
   (meetsTemporally (PastFn ?INTERVAL) ?INTERVAL))

(=>
   (instance ?INTERVAL TimeInterval)
   (equal (PastFn ?INTERVAL) (TimeIntervalFn NegativeInfinity (BeginFn ?INTERVAL))))

(instance ImmediatePastFn TemporalRelation)
(instance ImmediatePastFn UnaryFunction)
(instance ImmediatePastFn TotalValuedRelation)
(domain ImmediatePastFn 1 TimePosition)
(range ImmediatePastFn TimeInterval)
(documentation ImmediatePastFn "A &%UnaryFunction that maps a
&%TimePosition to a short, indeterminate &%TimeInterval that
immediately precedes the &%TimePosition.")

(=>
   (instance ?INTERVAL TimeInterval)
   (finishes (ImmediatePastFn ?INTERVAL) (PastFn ?INTERVAL)))

(instance FutureFn TemporalRelation)
(instance FutureFn UnaryFunction)
(instance FutureFn TotalValuedRelation)
(domain FutureFn 1 TimePosition)
(range FutureFn TimeInterval)
(documentation FutureFn "A &%UnaryFunction that maps a &%TimePosition
to the &%TimeInterval which it meets and which ends at
&%PositiveInfinity.")

(=>
   (instance ?INTERVAL TimeInterval)
   (meetsTemporally ?INTERVAL (FutureFn ?INTERVAL)))

(=>
   (instance ?INTERVAL TimeInterval)
   (equal (FutureFn ?INTERVAL) (TimeIntervalFn (EndFn ?INTERVAL) PositiveInfinity)))

(instance ImmediateFutureFn TemporalRelation)
(instance ImmediateFutureFn UnaryFunction)
(instance ImmediateFutureFn TotalValuedRelation)
(domain ImmediateFutureFn 1 TimePosition)
(range ImmediateFutureFn TimeInterval)
(documentation ImmediateFutureFn "A &%UnaryFunction that maps a
&%TimePosition to a short, indeterminate &%TimeInterval that
immediately follows the &%TimePosition.")

(=>
   (instance ?INTERVAL TimeInterval)
   (starts (ImmediateFutureFn ?INTERVAL) (FutureFn ?INTERVAL)))

;; The following definitions and axioms (down to the next section break)
;; cover the content in the Simple-Time ontology on the Ontolingua server.

(instance date BinaryPredicate)
(instance date SingleValuedRelation)
(instance date AsymmetricRelation)
(domain date 1 Physical)
(domain date 2 Day)
(subrelation date time)
(documentation date "A &%BinaryPredicate that specifies a
&%TimePosition in absolute calendar time, at the resolution
of one day, for a particular &%Object or &%Process.")

(instance YearFn TemporalRelation)
(instance YearFn UnaryFunction)
(domain YearFn 1 Integer)
(rangeSubclass YearFn Year)
(documentation YearFn "A &%UnaryFunction that maps a number to the corresponding calendar
&%Year.  For example, (&%YearFn 1912) returns the &%Class containing just one instance,
the year of 1912.  As might be expected, positive integers return years in the Common Era,
while negative integers return years in B.C.E.  Note that this function returns a &%Class
as a value.  The reason for this is that the related functions, viz. &%MonthFn, &%DayFn,
&%HourFn, &%MinuteFn, and &%SecondFn, are used to generate both specific &%TimeIntervals
and recurrent intervals, and the only way to do this is to make the domains and ranges of
these functions classes rather than individuals.")

(instance MonthFn TemporalRelation)
(instance MonthFn BinaryFunction)
(domainSubclass MonthFn 1 Month)
(domainSubclass MonthFn 2 Year)
(rangeSubclass MonthFn Month)
(documentation MonthFn "A &%BinaryFunction that maps a subclass of &%Month and a
subclass of &%Year to the class containing the &%Months corresponding to thos &%Years.
For example (&%MonthFn &%January (&%YearFn 1912)) is the class containing the eighth
&%Month, i.e. August, of the &%Year 1912.  For another example, (&%MonthFn &%August
&%Year) is equal to &%August, the class of all months of August.  Note that this function
returns a &%Class as a value.  The reason for this is that the related functions, viz.
DayFn, HourFn, MinuteFn, and SecondFn, are used to generate both specific &%TimeIntervals
and recurrent intervals, and the only way to do this is to make the domains and ranges of
these functions classes rather than individuals.")

(instance DayFn TemporalRelation)
(instance DayFn BinaryFunction)
(domain DayFn 1 PositiveRealNumber)
(domainSubclass DayFn 2 Month)
(rangeSubclass DayFn RecurringDay)
(documentation DayFn "A &%BinaryFunction that assigns a &%PositiveRealNumber and
a subclass of &%Months to the &%Days within each &%Month corresponding to that
&%PositiveRealNumber.  For example, (&%DayFn 16 &%August) is the &%Class of all
sixteenth days of August.  For another example, (&%DayFn 9 &%Month) would return
the class of all ninth days of any month.  For still another example, (&%DayFn 18
(&%MonthFn 8 (YearFn 1912))) denotes the 18th day of August 1912.")

(=>
   (instance ?DAY (DayFn ?NUMBER ?MONTH))
   (lessThanOrEqualTo ?NUMBER 31))

(=>
   (and
      (instance ?DAY1 (DayFn ?NUMBER1 ?MONTH))
      (instance ?DAY2 (DayFn ?NUMBER2 ?MONTH))
      (equal (SubtractionFn ?NUMBER2 ?NUMBER1) 1))
   (meetsTemporally ?DAY1 ?DAY2))

(instance HourFn TemporalRelation)
(instance HourFn BinaryFunction)
(domain HourFn 1 PositiveRealNumber)
(domainSubclass HourFn 2 Day)
(rangeSubclass HourFn Hour)
(documentation HourFn "A &%BinaryFunction that assigns a &%PositiveRealNumber and
a subclass of &%Days to the &%Hours within each &%Day corresponding to that
&%PositiveRealNumber.  For example, (&%HourFn 12 &%Thursday) is the &%Class of all
instances of noon Thursday.  For another example, (&%HourFn 24 &%Day) would return
the class of all instances of midnight.  For still another example, (&%HourFn 14
(&%DayFn 18 (&%MonthFn 8 (YearFn 1912)))) denotes 2 PM on the 18th day of August
1912.")

(=>
   (instance ?HOUR (HourFn ?NUMBER ?DAY))
   (lessThan ?NUMBER 24))

(=>
   (and
      (instance ?HOUR1 (HourFn ?NUMBER1 ?DAY))
      (instance ?HOUR2 (HourFn ?NUMBER2 ?DAY))
      (equal (SubtractionFn ?NUMBER2 ?NUMBER1) 1))
   (meetsTemporally ?HOUR1 ?HOUR2))

(instance MinuteFn TemporalRelation)
(instance MinuteFn BinaryFunction)
(domain MinuteFn 1 PositiveRealNumber)
(domainSubclass MinuteFn 2 Hour)
(rangeSubclass MinuteFn Minute)
(documentation MinuteFn "A &%BinaryFunction that assigns a &%PositiveRealNumber and
a subclass of &%Hours to the &%Minutes within each &%Hour corresponding to that
&%PositiveRealNumber.  For example, (&%MinuteFn 30 (&%HourFn 17 &%Day)) is the &%Class
of all 5:30's in the afternoon.  For another example, (&%MinuteFn 15 &%Hour) would return
the class of all instances of quarter past the hour.  For still another example,
(&%MinuteFn 15 (&%HourFn 14 (&%DayFn 18 (&%MonthFn 8 (YearFn 1912))))) denotes 15
minutes after 2 PM on the 18th day of August 1912.")

(=>
   (instance ?MINUTE (MinuteFn ?NUMBER ?HOUR))
   (lessThan ?NUMBER 60))

(=>
   (and
      (instance ?MINUTE1 (MinuteFn ?NUMBER1 ?HOUR))
      (instance ?MINUTE2 (MinuteFn ?NUMBER2 ?HOUR))
      (equal (SubtractionFn ?NUMBER2 ?NUMBER1) 1))
   (meetsTemporally ?MINUTE1 ?MINUTE2))

(instance SecondFn TemporalRelation)
(instance SecondFn BinaryFunction)
(domain SecondFn 1 PositiveRealNumber)
(domainSubclass SecondFn 2 Minute)
(rangeSubclass SecondFn Second)
(documentation SecondFn "A &%BinaryFunction that assigns a &%PositiveRealNumber and a
subclass of &%Minutes to the &%Seconds within each &%Minute corresponding to that
&%PositiveRealNumber.  For example, (&%SecondFn 4 (&%MinuteFn 5 &%Hour)) is the &%Class
of all fourth &%Seconds of every fifth &%Minute of every hour.  For another example,
(&%SecondFn 8 &%Minute) would return the eighth second of every minute.  For still
another example, (&%SecondFn 9 (&%MinuteFn 15 (&%HourFn 14 (&%DayFn 18 (&%MonthFn 8
(YearFn 1912)))))) denotes 9 seconds and 15 minutes after 2 PM on the 18th day of
August 1912.")

(=>
   (instance ?SECOND (SecondFn ?NUMBER ?MINUTE))
   (lessThan ?NUMBER 60))

(=>
   (and
      (instance ?SECOND1 (SecondFn ?NUMBER1 ?MINUTE))
      (instance ?SECOND2 (SecondFn ?NUMBER2 ?MINUTE))
      (equal (SubtractionFn ?NUMBER2 ?NUMBER1) 1))
   (meetsTemporally ?SECOND1 ?SECOND2))

(subclass Year TimeInterval)
(relatedInternalConcept Year YearFn)
(relatedInternalConcept Year YearDuration)
(documentation Year "The &%Class of all calendar &%Years.")

(=>
   (instance ?YEAR Year)
   (duration ?YEAR (MeasureFn 1 YearDuration)))

(=>
   (and
      (instance ?YEAR1 Year)
      (instance ?YEAR2 Year)
      (equal (SubtractionFn ?YEAR2 ?YEAR1) 1))
   (meetsTemporally ?YEAR1 ?YEAR2))

(subclass LeapYear Year)
(documentation LeapYear "The &%Class of all leap years.  These are years
which are either (i.) evenly divisible by 4 and not by 100 or (ii.) evenly
divisible by 400 (this latter case is known as a leap century).")

(=>
   (and
      (instance ?LEAP LeapYear)
      (equal ?LEAP (MeasureFn ?NUMBER Year)))
   (or
      (and
         (equal (RemainderFn ?NUMBER 4) 0)
         (not (equal (RemainderFn ?NUMBER 100) 0)))
      (equal (RemainderFn ?NUMBER 400) 0)))

(subclass Month TimeInterval)
(relatedInternalConcept Month MonthFn)
(documentation Month "The &%Class of all calendar &%Months.")

(subclass January Month)
(documentation January "The &%Class of all &%Months which are January.")

(=>
   (instance ?MONTH January)
   (duration ?MONTH (MeasureFn 31 DayDuration)))

(=>
   (and
      (equal ?MONTH1 (MonthFn January ?YEAR))
      (equal ?MONTH2 (MonthFn February ?YEAR)))
   (meetsTemporally ?MONTH1 ?MONTH2))

(subclass February Month)
(documentation February "The &%Class of all &%Months which are February.")

(=>
   (and
      (equal (MonthFn February ?YEAR) ?MONTH)
      (not (instance ?YEAR LeapYear)))
   (duration ?MONTH (MeasureFn 28 DayDuration)))

(=>
   (and
      (equal (MonthFn February ?YEAR) ?MONTH)
      (instance ?YEAR LeapYear))
   (duration ?MONTH (MeasureFn 29 DayDuration)))

(=>
   (and
      (equal ?MONTH1 (MonthFn February ?YEAR))
      (equal ?MONTH2 (MonthFn March ?YEAR)))
   (meetsTemporally ?MONTH1 ?MONTH2))

(subclass March Month)
(documentation March "The &%Class of all &%Months which are March.")

(=>
   (instance ?MONTH March)
   (duration ?MONTH (MeasureFn 31 DayDuration)))

(=>
   (and
      (equal ?MONTH1 (MonthFn March ?YEAR))
      (equal ?MONTH2 (MonthFn April ?YEAR)))
   (meetsTemporally ?MONTH1 ?MONTH2))

(subclass April Month)
(documentation April "The &%Class of all &%Months which are April.")

(=>
   (instance ?MONTH April)
   (duration ?MONTH (MeasureFn 30 DayDuration)))

(=>
   (and
      (equal ?MONTH1 (MonthFn April ?YEAR))
      (equal ?MONTH2 (MonthFn May ?YEAR)))
   (meetsTemporally ?MONTH1 ?MONTH2))

(subclass May Month)
(documentation May "The &%Class of all &%Months which are May.")

(=>
   (instance ?MONTH May)
   (duration ?MONTH (MeasureFn 31 DayDuration)))

(=>
   (and
      (equal ?MONTH1 (MonthFn May ?YEAR))
      (equal ?MONTH2 (MonthFn June ?YEAR)))
   (meetsTemporally ?MONTH1 ?MONTH2))

(subclass June Month)
(documentation June "The &%Class of all &%Months which are June.")

(=>
   (instance ?MONTH June)
   (duration ?MONTH (MeasureFn 30 DayDuration)))

(=>
   (and
      (equal ?MONTH1 (MonthFn June ?YEAR))
      (equal ?MONTH2 (MonthFn July ?YEAR)))
   (meetsTemporally ?MONTH1 ?MONTH2))

(subclass July Month)
(documentation July "The &%Class of all &%Months which are July.")

(=>
   (instance ?MONTH July)
   (duration ?MONTH (MeasureFn 31 DayDuration)))

(=>
   (and
      (equal ?MONTH1 (MonthFn July ?YEAR))
      (equal ?MONTH2 (MonthFn August ?YEAR)))
   (meetsTemporally ?MONTH1 ?MONTH2))

(subclass August Month)
(documentation August "The &%Class of all &%Months which are August.")

(=>
   (instance ?MONTH August)
   (duration ?MONTH (MeasureFn 31 DayDuration)))

(=>
   (and
      (equal ?MONTH1 (MonthFn August ?YEAR))
      (equal ?MONTH2 (MonthFn September ?YEAR)))
   (meetsTemporally ?MONTH1 ?MONTH2))

(subclass September Month)
(documentation September "The &%Class of all &%Months which are September.")

(=>
   (instance ?MONTH September)
   (duration ?MONTH (MeasureFn 30 DayDuration)))

(=>
   (and
      (equal ?MONTH1 (MonthFn September ?YEAR))
      (equal ?MONTH2 (MonthFn October ?YEAR)))
   (meetsTemporally ?MONTH1 ?MONTH2))

(subclass October Month)
(documentation October "The &%Class of all &%Months which are October.")

(=>
   (instance ?MONTH October)
   (duration ?MONTH (MeasureFn 31 DayDuration)))

(=>
   (and
      (equal ?MONTH1 (MonthFn October ?YEAR))
      (equal ?MONTH2 (MonthFn November ?YEAR)))
   (meetsTemporally ?MONTH1 ?MONTH2))

(subclass November Month)
(documentation November "The &%Class of all &%Months which are November.")

(=>
   (instance ?MONTH November)
   (duration ?MONTH (MeasureFn 30 DayDuration)))

(=>
   (and
      (equal ?MONTH1 (MonthFn November ?YEAR))
      (equal ?MONTH2 (MonthFn December ?YEAR)))
   (meetsTemporally ?MONTH1 ?MONTH2))

(subclass December Month)
(documentation December "The &%Class of all &%Months which are December.")

(=>
   (instance ?MONTH December)
   (duration ?MONTH (MeasureFn 31 DayDuration)))

(=>
   (and
      (equal ?MONTH1 (MonthFn December ?YEAR1))
      (equal ?MONTH2 (MonthFn January ?YEAR2))
      (meetsTemporally ?YEAR1 ?YEAR2))
   (meetsTemporally ?MONTH1 ?MONTH2))

(subclass Day TimeInterval)
(relatedInternalConcept Day DayFn)
(relatedInternalConcept Day DayDuration)
(documentation Day "The &%Class of all calendar &%Days.")

(=>
   (instance ?DAY Day)
   (duration ?DAY (MeasureFn 1 DayDuration)))

(subclass Monday RecurringDay)
(documentation Monday "The &%Class of all calendar Mondays.")

(subclass Tuesday RecurringDay)
(documentation Tuesday "The &%Class of all calendar Tuesdays.")

(=>
   (and
      (instance ?DAY1 Monday)
      (instance ?DAY2 Tuesday)
      (instance ?WEEK Week)
      (temporalPart ?DAY1 ?WEEK)
      (temporalPart ?DAY2 ?WEEK))
   (meetsTemporally ?DAY1 ?DAY2))

(subclass Wednesday RecurringDay)
(documentation Wednesday "The &%Class of all calendar Wednesdays.")

(=>
   (and
      (instance ?DAY1 Tuesday)
      (instance ?DAY2 Wednesday)
      (instance ?WEEK Week)
      (temporalPart ?DAY1 ?WEEK)
      (temporalPart ?DAY2 ?WEEK))
   (meetsTemporally ?DAY1 ?DAY2))

(subclass Thursday RecurringDay)
(documentation Thursday "The &%Class of all calendar Thursdays.")

(=>
   (and
      (instance ?DAY1 Wednesday)
      (instance ?DAY2 Thursday)
      (instance ?WEEK Week)
      (temporalPart ?DAY1 ?WEEK)
      (temporalPart ?DAY2 ?WEEK))
   (meetsTemporally ?DAY1 ?DAY2))

(subclass Friday RecurringDay)
(documentation Friday "The &%Class of all calendar Fridays.")

(=>
   (and
      (instance ?DAY1 Thursday)
      (instance ?DAY2 Friday)
      (instance ?WEEK Week)
      (temporalPart ?DAY1 ?WEEK)
      (temporalPart ?DAY2 ?WEEK))
   (meetsTemporally ?DAY1 ?DAY2))

(subclass Saturday RecurringDay)
(documentation Saturday "The &%Class of all calendar Saturdays.")

(=>
   (and
      (instance ?DAY1 Friday)
      (instance ?DAY2 Saturday)
      (instance ?WEEK Week)
      (temporalPart ?DAY1 ?WEEK)
      (temporalPart ?DAY2 ?WEEK))
   (meetsTemporally ?DAY1 ?DAY2))

(subclass Sunday RecurringDay)
(documentation Sunday "The &%Class of all calendar Sundays.")

(=>
   (and
      (instance ?DAY1 Saturday)
      (instance ?DAY2 Sunday)
      (instance ?WEEK Week)
      (temporalPart ?DAY1 ?WEEK)
      (temporalPart ?DAY2 ?WEEK))
   (meetsTemporally ?DAY1 ?DAY2))

(=>
   (and
      (instance ?DAY1 Sunday)
      (instance ?DAY2 Monday)
      (instance ?WEEK1 Week)
      (instance ?WEEK2 Week)
      (temporalPart ?DAY1 ?WEEK1)
      (temporalPart ?DAY2 ?WEEK2)
      (meetsTemporally ?WEEK1 ?WEEK2))
   (meetsTemporally ?DAY1 ?DAY2))

(subclass Week TimeInterval)
(documentation Week "The &%Class of all calendar weeks.")

(=>
   (instance ?WEEK Week)
   (duration ?WEEK (MeasureFn 1 WeekDuration)))

(subclass Hour TimeInterval)
(relatedInternalConcept Hour HourFn)
(relatedInternalConcept Hour HourDuration)
(documentation Hour "The &%Class of all clock &%Hours.")

(=>
   (instance ?HOUR Hour)
   (duration ?HOUR (MeasureFn 1 HourDuration)))

(subclass Minute TimeInterval)
(relatedInternalConcept Minute MinuteFn)
(relatedInternalConcept Minute MinuteDuration)
(documentation Minute "The &%Class of all clock &%Minutes.")

(=>
   (instance ?MINUTE Minute)
   (duration ?MINUTE (MeasureFn 1 MinuteDuration)))

(subclass Second TimeInterval)
(relatedInternalConcept Second SecondDuration)
(relatedInternalConcept Second SecondFn)
(documentation Second "The &%Class of all clock &%Seconds.")

(=>
   (instance ?SECOND Second)
   (duration ?SECOND (MeasureFn 1 SecondDuration)))

(instance TemporalCompositionFn TemporalRelation)
(instance TemporalCompositionFn BinaryFunction)
(domain TemporalCompositionFn 1 TimeInterval)
(domainSubclass TemporalCompositionFn 2 TimeInterval)
(rangeSubclass TemporalCompositionFn TimeInterval)
(documentation TemporalCompositionFn "The basic &%Function for expressing
the composition of larger &%TimeIntervals out of smaller &%TimeIntervals.
For example, if &%ThisSeptember is an &%instance of &%September,
(&%TemporalCompositionFn &%ThisSeptember &%Day) denotes the &%Class of
consecutive days that make up &%ThisSeptember.  Note that one can obtain
the number of instances of this &%Class by using the function &%CardinalityFn.")

(=>
   (equal (TemporalCompositionFn ?INTERVAL ?INTERVAL-TYPE) ?CLASS)
   (forall (?TIME1 ?TIME2)
      (=>
         (and
            (instance ?TIME1 ?INTERVAL-TYPE)
            (instance ?TIME2 ?CLASS))
         (exists (?DURATION)
            (and
               (duration ?TIME1 ?DURATION)
               (duration ?TIME2 ?DURATION))))))

(=>
   (equal (TemporalCompositionFn ?INTERVAL ?INTERVAL-TYPE) ?CLASS)
   (forall (?TIME1 ?TIME2)
      (=>
         (and
            (instance ?TIME1 ?CLASS)
            (instance ?TIME2 ?CLASS)
            (not (equal ?TIME1 ?TIME2)))
         (or
            (meetsTemporally ?TIME1 ?TIME2)
            (meetsTemporally ?TIME2 ?TIME1)
            (earlier ?TIME1 ?TIME2)
            (earlier ?TIME2 ?TIME1)))))

(=>
   (equal (TemporalCompositionFn ?INTERVAL ?INTERVAL-TYPE) ?CLASS)
   (exists (?TIME)
      (and
         (instance ?TIME ?CLASS)
         (starts ?TIME ?INTERVAL))))

(=>
   (equal (TemporalCompositionFn ?INTERVAL ?INTERVAL-TYPE) ?CLASS)
   (exists (?TIME)
      (and
         (instance ?TIME ?CLASS)
         (finishes ?TIME ?INTERVAL))))

(=>
   (equal (TemporalCompositionFn ?INTERVAL ?INTERVAL-TYPE) ?CLASS)
   (forall (?TIME1)
      (=>
         (and
            (instance ?TIME1 ?CLASS)
            (not (finishes ?TIME1 ?INTERVAL)))
         (exists (?TIME2)
            (and
               (instance ?TIME2 ?CLASS)
               (meetsTemporally ?TIME1 ?TIME2))))))

(=>
   (equal (TemporalCompositionFn ?INTERVAL ?INTERVAL-TYPE) ?CLASS)
   (forall (?TIME1)
      (=>
         (and
            (instance ?TIME1 ?CLASS)
            (not (starts ?TIME1 ?INTERVAL)))
         (exists (?TIME2)
            (and
               (instance ?TIME2 ?CLASS)
               (meetsTemporally ?TIME2 ?TIME1))))))

(=>
   (equal (TemporalCompositionFn ?INTERVAL ?INTERVAL-TYPE) ?CLASS)
   (forall (?TIME)
      (=>
         (and
            (instance ?TIME TimePoint)
            (temporalPart ?TIME ?INTERVAL))
         (exists (?INSTANCE)
            (and
               (instance ?INSTANCE ?CLASS)
               (temporalPart ?TIME ?INSTANCE))))))

(=>
   (instance ?YEAR Year)
   (equal (CardinalityFn (TemporalCompositionFn ?YEAR Month)) 12))

(=>
   (and
      (instance ?MONTH Month)
      (duration ?MONTH (MeasureFn ?NUMBER DayDuration)))
   (equal (CardinalityFn (TemporalCompositionFn ?MONTH Day)) ?NUMBER))

(=>
   (instance ?WEEK Week)
   (equal (CardinalityFn (TemporalCompositionFn ?WEEK Day)) 7))

(=>
   (instance ?DAY Day)
   (equal (CardinalityFn (TemporalCompositionFn ?DAY Hour)) 24))

(=>
   (instance ?HOUR Hour)
   (equal (CardinalityFn (TemporalCompositionFn ?HOUR Minute)) 60))

(=>
   (instance ?MINUTE Minute)
   (equal (CardinalityFn (TemporalCompositionFn ?MINUTE Second)) 60))


;; END FILE

;; <module>MEREOTOPOLOGY</module>
;; BEGIN FILE


;;;;;;;;;;;;;;;;;;;
;; MEREOTOPOLOGY ;;
;;;;;;;;;;;;;;;;;;;

;; INCLUDES 'STRUCTURAL ONTOLOGY'
;; INCLUDES 'BASE ONTOLOGY'

;; Most of this content is taken from Barry Smith's and Nicola Guarino's
;; papers on the subject.

(instance connected BinaryPredicate)
(instance connected SpatialRelation)
(instance connected ReflexiveRelation)
(instance connected SymmetricRelation)
(domain connected 1 Object)
(domain connected 2 Object)
(documentation connected "(connected ?OBJ1 ?OBJ2) means that ?OBJ1
&%meetsSpatially ?OBJ2 or that ?OBJ1 &%overlapsSpatially ?OBJ2.")

(=>
     (connected ?OBJ1 ?OBJ2)
     (or
          (meetsSpatially ?OBJ1 ?OBJ2)
          (overlapsSpatially ?OBJ1 ?OBJ2)))

(<=>
   (instance ?OBJ SelfConnectedObject)
   (forall (?PART1 ?PART2)
      (=>
        (equal ?OBJ (MereologicalSumFn ?PART1 ?PART2))
        (connected ?PART1 ?PART2))))

(instance connects SpatialRelation)
(instance connects TernaryPredicate)
(domain connects 1 SelfConnectedObject)
(domain connects 2 SelfConnectedObject)
(domain connects 3 SelfConnectedObject)
(documentation connects "The relationship between three things, when one of
the three things connects the other two.  More formally, (&%connects ?OBJ1
?OBJ2 ?OBJ3) means that (&%connected ?OBJ1 ?OBJ2) and (&%connected ?OBJ1 ?OBJ3)
and not (&%connected ?OBJ2 ?OBJ3).")

(<=>
   (connects ?OBJ1 ?OBJ2 ?OBJ3)
   (and
      (connected ?OBJ1 ?OBJ2)
      (connected ?OBJ1 ?OBJ3)
      (not
         (connected ?OBJ2 ?OBJ3))))

(subrelation meetsSpatially connected)
(instance meetsSpatially IrreflexiveRelation)
(instance meetsSpatially SymmetricRelation)
(disjointRelation meetsSpatially overlapsSpatially)
(documentation meetsSpatially "(&%meetsSpatially ?OBJ1 ?OBJ2) means that
?OBJ1 and ?OBJ2 are &%connected but that neither ?OBJ1 nor ?OBJ2
&%overlapsSpatially the other.")

(subrelation overlapsSpatially connected)
(instance overlapsSpatially ReflexiveRelation)
(instance overlapsSpatially SymmetricRelation)
(documentation overlapsSpatially "(&%overlapsSpatially ?OBJ1 ?OBJ2) means
that the &%Objects ?OBJ1 and ?OBJ2 have some parts in common.  This is a
reflexive and symmetric (but not transitive) relation.")

(<=>
	(overlapsSpatially ?OBJ1 ?OBJ2)
     	(exists (?OBJ3)
          	(and
               	(part ?OBJ3 ?OBJ1)
               	(part ?OBJ3 ?OBJ2))))

(=>
   (and
      (member ?OBJ1 ?COLL)
      (member ?OBJ2 ?COLL)
	(not (equal ?OBJ1 ?OBJ2)))
   (not
         (overlapsSpatially ?OBJ1 ?OBJ2)))

(=>
   (and
      (instance ?REL CaseRole)
      (instance ?OBJ Object)
      (holds ?REL ?PROCESS ?OBJ))
   (exists (?TIME)
      (overlapsSpatially (WhereFn ?PROCESS ?TIME) ?OBJ)))

(instance overlapsPartially SymmetricRelation)
(instance overlapsPartially IrreflexiveRelation)
(subrelation overlapsPartially overlapsSpatially)
(documentation overlapsPartially "(&%overlapsPartially ?OBJ1 ?OBJ2) means
that ?OBJ1 and ?OBJ2 have part(s) in common, but neither ?OBJ1 nor ?OBJ2
is a &%part of the other.")

(<=>
   (overlapsPartially ?OBJ1 ?OBJ2)
      (and
         (not (part ?OBJ1 ?OBJ2))
         (not (part ?OBJ2 ?OBJ1))
         (exists (?OBJ3)
            (and
               (part ?OBJ3 ?OBJ1)
               (part ?OBJ3 ?OBJ2)))))

(subrelation superficialPart part)
(instance superficialPart IrreflexiveRelation)
(instance superficialPart TransitiveRelation)
(documentation superficialPart "(&%superficialPart ?OBJ1 ?OBJ2)
means that ?OBJ1 is a part of ?OBJ2 that has no interior parts of its own
(or, intuitively, that only overlaps those parts of ?OBJ2 that are
externally connected with the mereological complement of ?OBJ2). This too
is a transitive relation closed under &%MereologicalSumFn and
&%MereologicalProductFn.")

(=>
	(superficialPart ?OBJ1 ?OBJ2)
	(and
		(not
			(interiorPart ?OBJ1 ?OBJ2))
		(not
			(exists (?OBJ3)
				(interiorPart ?OBJ3 ?OBJ1)))))

(instance surface AsymmetricRelation)
(subrelation surface superficialPart)
(domain surface 1 SelfConnectedObject)
(domain surface 2 SelfConnectedObject)
(documentation surface "(&%surface ?OBJ1 ?OBJ2) means that ?OBJ1
is a maximally connected &%superficialPart of ?OBJ2.  Note that some
&%SelfConnectedObjects have more than one surface, e.g. a hollow
object like a tennis ball has both an inner and an outer surface.")

(=>
   (surface ?OBJ1 ?OBJ2)
   (forall (?OBJ3)
      (=>
	   (superficialPart ?OBJ3 ?OBJ2)
	   (part ?OBJ3 ?OBJ1))))

(subrelation interiorPart part)
(instance interiorPart AsymmetricRelation)
(instance interiorPart TransitiveRelation)
(documentation interiorPart "(&%interiorPart ?OBJ1 ?OBJ2) means
that ?OBJ1 is part ?OBJ2 and there is no overlap between ?OBJ1 and
any &%superficialPart ?OBJ2.")

(=>
	(interiorPart ?OBJ1 ?OBJ2)
     	(forall (?PART)
               	(=>
                    	(superficialPart ?PART ?OBJ2)
                    	(not
                         	(overlapsSpatially ?OBJ1 ?PART)))))

(subrelation bottom superficialPart)
(domain bottom 1 SelfConnectedObject)
(domain bottom 2 SelfConnectedObject)
(documentation bottom "(&%bottom ?BOTTOM ?OBJECT) holds if ?BOTTOM is the
lowest or deepest maximal superficial part of ?OBJECT.")

(=>
	(and
		(bottom ?BOTTOM ?OBJECT)
		(part ?PART ?OBJECT)
		(not (connected ?PART ?BOTTOM)))
	(orientation ?PART ?BOTTOM Above))

(subrelation top superficialPart)
(domain top 1 SelfConnectedObject)
(domain top 2 SelfConnectedObject)
(documentation top "(&%top ?TOP ?OBJECT) means that ?TOP is the highest maximal
superficial part of ?OBJECT.")

(=>
	(and
		(top ?TOP ?OBJECT)
		(part ?PART ?OBJECT)
		(not (connected ?PART ?TOP)))
	(orientation ?PART ?TOP Below))

(subrelation side superficialPart)
(domain side 1 SelfConnectedObject)
(domain side 2 SelfConnectedObject)
(documentation side "(&%side ?SIDE ?OBJECT) means that ?SIDE is a side of the object,
as opposed to the &%top or &%bottom.")

(=>
	(and
		(side ?SIDE ?OBJECT)
		(part ?PART ?OBJECT)
		(not (connected ?PART ?SIDE)))
      (exists (?DIRECT)
	      (orientation ?SIDE ?PART ?DIRECT)))

(<=>
	(width ?OBJECT ?WIDTH)
	(exists (?SIDE1 ?SIDE2)
		(and
			(side ?SIDE1 ?OBJECT)
			(side ?SIDE2 ?OBJECT)
			(distance ?SIDE1 ?SIDE2 ?WIDTH))))

(subrelation height length)
(domain height 1 SelfConnectedObject)
(domain height 2 LengthMeasure)
(documentation height "The height of an &%Object is the distance between
its &%top and its &%bottom.")

(=>
	(and
		(height ?OBJECT ?HEIGHT)
		(top ?TOP ?OBJECT)
            (bottom ?BOTTOM ?OBJECT))
	(distance ?TOP ?BOTTOM ?HEIGHT))

(instance MereologicalSumFn SpatialRelation)
(instance MereologicalSumFn BinaryFunction)
(instance MereologicalSumFn TotalValuedRelation)
(domain MereologicalSumFn 1 Object)
(domain MereologicalSumFn 2 Object)
(range MereologicalSumFn Object)
(relatedInternalConcept MereologicalSumFn MereologicalProductFn)
(relatedInternalConcept MereologicalSumFn MereologicalDifferenceFn)
(documentation MereologicalSumFn "(&%MereologicalSumFn ?OBJ1 ?OBJ2)
denotes the &%Object consisting of the parts which belong to either
?OBJ1 or ?OBJ2.")

(=>
     (equal ?OBJ3 (MereologicalSumFn ?OBJ1 ?OBJ2))
     (forall (?PART)
	       (<=>
	            (part ?PART ?OBJ3)
		      (or
	  	           (part ?PART ?OBJ1)
			     (part ?PART ?OBJ2)))))

(instance MereologicalProductFn SpatialRelation)
(instance MereologicalProductFn BinaryFunction)
(instance MereologicalProductFn TotalValuedRelation)
(domain MereologicalProductFn 1 Object)
(domain MereologicalProductFn 2 Object)
(range MereologicalProductFn Object)
(relatedInternalConcept MereologicalProductFn MereologicalDifferenceFn)
(documentation MereologicalProductFn "(&%MereologicalProductFn ?OBJ1 ?OBJ2)
denotes the &%Object consisting of the parts which belong to both ?OBJ1
and ?OBJ2.")

(=>
     (equal ?OBJ3 (MereologicalProductFn ?OBJ1 ?OBJ2))
     (forall (?PART)
	       (<=>
	            (part ?PART ?OBJ3)
		      (and
	  	           (part ?PART ?OBJ1)
			     (part ?PART ?OBJ2)))))

(instance MereologicalDifferenceFn SpatialRelation)
(instance MereologicalDifferenceFn BinaryFunction)
(instance MereologicalDifferenceFn TotalValuedRelation)
(domain MereologicalDifferenceFn 1 Object)
(domain MereologicalDifferenceFn 2 Object)
(range MereologicalDifferenceFn Object)
(documentation MereologicalDifferenceFn "(&%MereologicalDifferenceFn ?OBJ1
?OBJ2) denotes the &%Object consisting of the parts which belong to ?OBJ1
and not to ?OBJ2.")

(=>
     (equal ?OBJ3 (MereologicalDifferenceFn ?OBJ1 ?OBJ2))
     (forall (?PART)
	       (<=>
	            (part ?PART ?OBJ3)
		      (and
	  	           (part ?PART ?OBJ1)
                       (not
			          (part ?PART ?OBJ2))))))

;; What follows is an alignment of Casati and Varzi's formal theory of
;; holes with the SUMO.

(instance hole BinaryPredicate)
(instance hole SpatialRelation)
(instance hole AsymmetricRelation)
(domain hole 1 Hole)
(domain hole 2 SelfConnectedObject)
(documentation hole "(&%hole ?HOLE ?OBJ) means that ?HOLE is a
&%Hole in ?OBJ.  A &%Hole is a fillable body located at the
&%surface an &%Object.")

(subclass Hole Region)
(documentation Hole "A hole is an immaterial body located at the surface
of an &%Object.  Since every &%Hole is ontologically dependent on its host
(i.e., the object in which it is a hole), being a &%Hole is defined as
being a &%hole in something.  Note that two &%Holes may occupy the same
region, or part of the same region, without sharing any parts.")

(<=>
	(instance ?HOLE Hole)
	(exists (?OBJ)
		(hole ?HOLE ?OBJ)))

(=>
	(hole ?HOLE ?OBJ)
      (not
		(instance ?OBJ Hole)))

(=>
	(hole ?HOLE ?OBJ)
          (not
		 (overlapsSpatially ?HOLE ?OBJ)))

;; Any two hosts of a hole have a common proper part that entirely hosts
;; the hole.

(=>
	(and
		(hole ?HOLE ?OBJ1)
            (hole ?HOLE ?OBJ2))
      (exists (?OBJ3)
             (and
			(properPart ?OBJ3 (MereologicalProductFn ?OBJ1 ?OBJ2))
                  (hole ?HOLE ?OBJ3))))

;; A common host of two holes hosts all parts of the sum of those holes.

(=>
	(and
		(hole ?HOLE1 ?OBJ)
      	(hole ?HOLE2 ?OBJ))
     	(forall (?HOLE3)
        	(=>
			(part ?HOLE3 (MereologicalSumFn ?HOLE1 ?HOLE2))
                  (hole ?HOLE3 ?OBJ))))

;; Any object that includes the host of a hole is a host of that hole,
;; unless its parts also include parts of that very hole.

(=>
	(and
		(hole ?HOLE ?OBJ1)
            (part ?OBJ1 ?OBJ2))
      (or
		(overlapsSpatially ?HOLE ?OBJ2)
            (hole ?HOLE ?OBJ2)))

;; Overlapping holes have overlapping hosts.

(=>
	(and
		(hole ?HOLE1 ?OBJ1)
            (hole ?HOLE2 ?OBJ2)
            (overlapsSpatially ?HOLE1 ?HOLE2))
      (overlapsSpatially ?OBJ1 ?OBJ2))

;; No hole is atomic

(=>
	(instance ?HOLE1 Hole)
      (exists (?HOLE2)
      	(properPart ?HOLE2 ?HOLE1)))

;; Topological Definitions

;; Definition of 'PrincipalHostFn'

(instance PrincipalHostFn SpatialRelation)
(instance PrincipalHostFn UnaryFunction)
(instance PrincipalHostFn TotalValuedRelation)
(instance PrincipalHostFn AsymmetricRelation)
(domain PrincipalHostFn 1 Hole)
(range PrincipalHostFn Object)
(documentation PrincipalHostFn "A &%UnaryFunction that maps a &%Hole to
the &%Object which is its principal host.  The principle host of a &%Hole
is its maximally connected host (a notion taken here to be defined only
when the argument is a hole).")

(=>
     (equal ?OBJ1 (PrincipalHostFn ?HOLE))
     (forall (?OBJ2)
		(<=>
			(overlapsSpatially ?OBJ2 ?OBJ1)
			(exists (?OBJ3)
				(and
					(hole ?HOLE ?OBJ3)
				      (overlapsSpatially ?OBJ2 ?OBJ3))))))

;; Holes are connected with their hosts.

(=>
	(hole ?HOLE ?OBJ)
      (connected ?HOLE ?OBJ))

;; No hole can have a proper part that is externally connected
;; with exactly the same things as the hole itself.

(=>
	(and
		(instance ?HOLE1 Hole)
            (properPart ?HOLE2 ?HOLE1))
      (exists (?OBJ)
            (and
			(meetsSpatially ?HOLE1 ?OBJ)
                  (not
				(meetsSpatially ?HOLE2 ?OBJ)))))

(instance Fillable ShapeAttribute)
(documentation Fillable "Something is &%Fillable if it can be filled by
something else.  Note that 'filled' here means perfectly filled.")

(=>
   (exists (?TIME)
      (holdsDuring ?TIME
         (fills ?OBJ ?HOLE)))
     (attribute ?HOLE Fillable))

;; Something is fillable just in case it is part of a hole; i.e.,
;; fillability is an exclusive property of holes and their parts.

(<=>
	(attribute ?HOLE1 Fillable)
       	(exists (?HOLE2)
        	(and
			(instance ?HOLE2 Hole)
                  (part ?HOLE1 ?HOLE2))))

(subrelation partiallyFills located)
(instance partiallyFills SpatialRelation)
(instance partiallyFills AsymmetricRelation)
(domain partiallyFills 1 Object)
(domain partiallyFills 2 Hole)
(documentation partiallyFills "(&%partiallyFills ?OBJ ?HOLE) means that
?OBJ &%completelyFills some part of ?HOLE. Note that if (&%partiallyFills
?OBJ1 ?HOLE) and (&%part ?OBJ1 ?OBJ2), then (&%partiallyFills ?OBJ2 ?HOLE).
Note too that a partial filler need not be wholly inside a hole (it may
stick out), which means that every complete filler also qualifies as
(is a limit case of) a partial one.")

(=>
	(partiallyFills ?OBJ ?HOLE1)
	(exists (?HOLE2)
		(and
			(part ?HOLE2 ?HOLE1)
			(completelyFills ?OBJ ?HOLE2))))

(instance properlyFills AsymmetricRelation)
(subrelation properlyFills partiallyFills)
(domain properlyFills 1 Object)
(domain properlyFills 2 Hole)
(documentation properlyFills "(&%properlyFills ?OBJ ?HOLE)
means that ?HOLE is properly (though perhaps incompletely) filled by
?OBJ, i.e. some part of ?HOLE is perfectly filled by ?OBJ.  Note that
&%properlyFills is the dual of &%completelyFills, and is so
related to &%partiallyFills that ?OBJ &%properlyFills ?HOLE just in
case ?OBJ &%partiallyFills every part of ?HOLE.  (Thus, every perfect
filler is both complete and proper in this sense).")

(=>
	(properlyFills ?OBJ ?HOLE1)
	(exists (?HOLE2)
		(and
			(part ?HOLE2 ?HOLE1)
			(fills ?OBJ ?HOLE2))))

(instance completelyFills AsymmetricRelation)
(subrelation completelyFills partiallyFills)
(documentation completelyFills "(&%completelyFills ?OBJ ?HOLE)
means that some &%part of the &%Object ?OBJ fills the &%Hole ?HOLE.
Note that if (&%completelyFills ?OBJ1 ?HOLE) and (&%part
?OBJ1 ?OBJ2), then (&%completelyFills ?OBJ2 ?HOLE).")

(=>
	(completelyFills ?OBJ1 ?HOLE)
	(exists (?OBJ2)
		(and
			(part ?OBJ2 ?OBJ1)
			(fills ?OBJ2 ?HOLE))))

(instance fills AsymmetricRelation)
(subrelation fills completelyFills)
(subrelation fills properlyFills)
(domain fills 1 Object)
(domain fills 2 Hole)
(relatedInternalConcept fills Fillable)
(documentation fills "Holes can be filled.  (&%fills ?OBJ ?HOLE)
means that the &%Object ?OBJ fills the &%Hole ?HOLE.  Note that
&%fills here means perfectly filled.")

;; Perfect fillers and fillable entities have no parts in common (rather,
;; they may occupy the same spatial region).

(=>
	(and
		(fills ?OBJ1 ?HOLE)
           	(attribute ?OBJ2 Fillable))
      (not
		(overlapsSpatially ?OBJ1 ?OBJ2)))

;; A complete filler of (a part of) a hole is connected with everything
;; with which (that part of) the hole itself is connected.

(=>
   (completelyFills ?OBJ1 ?HOLE)
      (forall (?OBJ2)
   	   (=>
	      (connected ?OBJ2 ?HOLE)
            (connected ?OBJ2 ?OBJ1))))

;; Every hole is connected with everything with which a proper filler
;; of the hole is connected.

(=>
	(and
		(properlyFills ?OBJ1 ?HOLE)
          	(connected ?OBJ2 ?OBJ1))
     	(connected ?HOLE ?OBJ2))

;; A perfect filler of (a part of) a hole completely fills every proper
;; part of (that part of) that hole.

(=>
	(and
		(fills ?OBJ ?HOLE1)
            (properPart ?HOLE2 ?HOLE1))
     	(completelyFills ?OBJ ?HOLE2))

;; Every proper part of a perfect filler of (a part of) a hole properly
;; fills (that part of) that hole.

(=>
	(and
		(fills ?OBJ1 ?HOLE)
            (properPart ?OBJ2 ?OBJ1))
     	(properlyFills ?OBJ2 ?HOLE))

(instance SkinFn SpatialRelation)
(instance SkinFn UnaryFunction)
(instance SkinFn TotalValuedRelation)
(instance SkinFn AsymmetricRelation)
(domain SkinFn 1 Hole)
(range SkinFn Object)
(documentation SkinFn "A &%UnaryFunction that maps a &%Hole to the skin
of the &%Hole.  The skin of a &%Hole is the fusion of those superficial
parts (see &%superficialPart) of the &%Hole's principal host (see
&%PrincipalHostFn) with which the &%Hole is externally connected.")

(=>
     (equal ?OBJ1 (SkinFn ?HOLE))
     (forall (?OBJ2)
		(<=>
			(overlapsSpatially ?OBJ2 ?OBJ1)
			(exists (?OBJ3)
				(and
				      (superficialPart ?OBJ3 (PrincipalHostFn ?HOLE))
				      (meetsSpatially ?HOLE ?OBJ3)
				      (overlapsSpatially ?OBJ2 ?OBJ3))))))

;; END FILE

;; <module>PROCESSES</module>
;; BEGIN FILE


;;;;;;;;;;;;;;;;;;;;;
;;    PROCESSES    ;;
;;;;;;;;;;;;;;;;;;;;;

;; INCLUDES 'TEMPORAL CONCEPTS'
;; INCLUDES 'OBJECTS'
;; INCLUDES 'QUALITIES'

(instance subProcess BinaryPredicate)
(instance subProcess PartialOrderingRelation)
(domain subProcess 1 Process)
(domain subProcess 2 Process)
(documentation subProcess "(&%subProcess ?SUBPROC ?PROC) means that ?SUBPROC
is a subprocess of ?PROC.  A subprocess is here understood as a temporally
distinguished part (proper or not) of a &%Process.")

(=>
   (subProcess ?SUBPROC ?PROC)
      (or
         (equal (WhenFn ?SUBPROC) (WhenFn ?PROC))
         (during (WhenFn ?SUBPROC) (WhenFn ?PROC))))

(=>
   (subProcess ?SUBPROC ?PROC)
   (forall (?REGION)
      (=>
         (located ?PROC ?REGION)
         (located ?SUBPROC ?REGION))))

;; Each temporal part of a Process exists at some timepoint.

(=>
	(and
            (instance ?PROC Process)
            (subProcess ?SUBPROC ?PROC))
     	(exists (?TIME)
      	    (time ?SUBPROC ?TIME)))

;; The following formulas cover the hierarchy of &%Classes under
;; &%BiologicalProcess.

(subclass BiologicalProcess InternalChange)
(documentation BiologicalProcess "A &%Process embodied in an &%Organism.")

(=>
   (instance ?PROC BiologicalProcess)
   (exists (?OBJ)
      (and
         (instance ?OBJ Organism)
         (located ?PROC ?OBJ))))

(=>
   (and
      (instance ?PROC BiologicalProcess)
      (experiencer ?PROC ?ORG))
   (instance ?ORG Organism))

(subclass PhysiologicProcess BiologicalProcess)
(documentation PhysiologicProcess "A normal process of an &%Organism
or part of an &%Organism.")

(subclass AutonomicProcess BiologicalProcess)
(disjoint AutonomicProcess IntentionalProcess)
(documentation AutonomicProcess "The class of &%BiologicalProcesses of
which there is not conscious awareness and control.")

(=>
   (and
      (instance ?PROCESS AutonomicProcess)
      (agent ?PROCESS ?AGENT))
   (instance ?AGENT Hypothalamus))

(subclass OrganismProcess PhysiologicProcess)
(documentation OrganismProcess "A physiologic function of the
&%Organism as a whole, of multiple organ systems or of multiple
&%Organs or &%Tissues.")

(subclass Birth OrganismProcess)
(documentation Birth "The &%Process of being born.")

(=>
   (and
      (instance ?BIRTH Birth)
      (experiencer ?BIRTH ?AGENT))
   (exists (?DEATH)
      (and
         (instance ?DEATH Death)
         (experiencer ?DEATH ?AGENT))))

(subclass Death OrganismProcess)
(documentation Death "The &%Process of dying.")

(=>
   (and
      (instance ?DEATH Death)
      (experiencer ?DEATH ?AGENT))
   (holdsDuring (FutureFn (WhenFn ?DEATH)) (attribute ?AGENT Dead)))

(=>
   (and
      (instance ?DEATH Death)
      (instance ?BIRTH Birth)
      (experiencer ?DEATH ?AGENT)
      (experiencer ?BIRTH ?AGENT))
   (exists (?TIME)
      (and
         (meetsTemporally (WhenFn ?BIRTH) ?TIME)
         (meetsTemporally ?TIME (WhenFn ?DEATH))
         (holdsDuring ?TIME (attribute ?AGENT Living)))))

(subclass Breathing OrganismProcess)
(documentation Breathing "The &%Process of respiration, by which oxygen
is made available to an &%Animal.  This covers processes of inhalation,
exhalation, and alternations between the two.")

(subclass Ingesting OrganismProcess)
(documentation Ingesting "The &%Process by which &%Food is
taken into an &%Animal.")

(=>
   (and
      (instance ?ACT Ingesting)
      (patient ?ACT ?FOOD))
   (instance ?FOOD Food))

(subclass Eating Ingesting)
(documentation Eating "The &%Process by which solid &%Food is
incorporated into an &%Animal.")

(=>
   (and
      (instance ?ACT Eating)
      (patient ?ACT ?FOOD))
   (attribute ?FOOD Solid))

(subclass Drinking Ingesting)
(documentation Drinking "The &%Process by which liquid &%Food, i.e.
&%Beverages, are incorporated into an &%Animal.")

(subclass Digesting OrganismProcess)
(documentation Digesting "The &%Process by which &%Food that has been
ingested is broken down into simpler chemical compounds and absorbed by
the &%Organism.")

(=>
   (and
      (instance ?DIGEST Digesting)
      (agent ?DIGEST ?ORGANISM))
   (exists (?INGEST)
      (and
         (instance ?INGEST Ingesting)
         (agent ?INGEST ?ORGANISM)
         (overlapsTemporally (WhenFn ?INGEST) (WhenFn ?DIGEST)))))

(=>
   (instance ?DIGEST Digesting)
   (exists (?DECOMP)
      (and
         (instance ?DECOMP ChemicalDecomposition)
         (subProcess ?DECOMP ?DIGEST))))

(subclass Growth OrganismProcess)
(documentation Growth "The &%Process of biological development in which
an &%Organism or part of an &%Organism changes its form or its size.")

(subclass Replication OrganismProcess)
(documentation Replication "The &%Process of biological reproduction.
This can be either a sexual or an asexual process.")

(=>
   (and
      (instance ?REP Replication)
      (agent ?REP ?PARENT)
      (result ?REP ?CHILD))
   (parent ?CHILD ?PARENT))

(=>
   (instance ?REP Replication)
   (exists (?BODY)
      (and
         (instance ?BODY ReproductiveBody)
         (result ?REP ?BODY))))

(subclass SexualReproduction Replication)
(disjoint SexualReproduction AsexualReproduction)
(documentation SexualReproduction "Sexual &%Processes of biological
reproduction.")

(=>
   (and
      (instance ?REP SexualReproduction)
      (result ?REP ?ORGANISM))
   (exists (?MOTHER ?FATHER)
      (and
         (mother ?ORGANISM ?MOTHER)
         (father ?ORGANISM ?FATHER))))

(subclass AsexualReproduction Replication)
(documentation AsexualReproduction "Asexual &%Processes of biological
reproduction.")

(=>
   (and
      (instance ?REP AsexualReproduction)
      (result ?REP ?ORGANISM))
   (not (exists (?PARENT1 ?PARENT2)
      (and
         (parent ?ORGANISM ?PARENT1)
         (parent ?ORGANISM ?PARENT2)
         (not (equal ?PARENT1 ?PARENT2))))))

(subclass PsychologicalProcess BiologicalProcess)
(documentation PsychologicalProcess "A &%BiologicalProcess which takes place in
the mind or brain of an &%Organism and which may be manifested in the behavior
of the &%Organism.")

(=>
    (instance ?PROCESS PsychologicalProcess)
    (exists (?ANIMAL)
        (and
            (instance ?ANIMAL Animal)
            (experiencer ?PROCESS ?ANIMAL))))

(subclass OrganOrTissueProcess PhysiologicProcess)
(disjoint OrganOrTissueProcess OrganismProcess)
(documentation OrganOrTissueProcess "A &%PhysiologicProcess of a
particular &%Organ or &%Tissue.")

(=>
    (instance ?PROC OrganOrTissueProcess)
    (exists (?THING)
        (and
            (located ?PROC ?THING)
            (or
                (instance ?THING Organ)
                (instance ?THING Tissue)))))

(subclass PathologicProcess BiologicalProcess)
(disjoint PathologicProcess PhysiologicProcess)
(documentation PathologicProcess "A disordered process, activity, or
state of the &%Organism as a whole, of a body system or systems, or of
multiple &%Organs or &%Tissues. Included here are normal responses to a
negative stimulus as well as patholologic conditions or states that are
less specific than a disease. Pathologic functions frequently have
systemic effects.")

(=>
   (and
      (instance ?PATH PathologicProcess)
      (experiencer ?PATH ?ORG))
   (exists (?PART ?DISEASE)
      (and
         (part ?PART ?ORG)
         (instance ?DISEASE DiseaseOrSyndrome)
         (attribute ?PART ?DISEASE))))

(subclass Injuring PathologicProcess)
(subclass Injuring Damaging)
(documentation Injuring "The process of creating a traumatic wound or
injury.  Since &%Injuring is not possible without some biologic function
of the organism being injured, it is a subclass of &%BiologicalProcess.")

(=>
    (instance ?INJ Injuring)
    (exists (?STRUCT)
        (and
            (instance ?STRUCT AnatomicalStructure)
            (patient ?INJ ?STRUCT))))

(<=>
   (instance ?INJ Injuring)
   (and
      (instance ?INJ Damaging)
      (exists (?ORGANISM)
         (and
            (instance ?ORGANISM Organism)
            (patient ?INJ ?ORGANISM)))))

(subclass Poisoning Injuring)
(documentation Poisoning "A &%Poisoning is caused by an external
substance.  Since &%Poisoning is not possible without some biologic
function which affects the &%Organism being injured, it is a subclass
of &%BiologicalProcess.")

(=>
    (instance ?POISON Poisoning)
    (exists (?THING)
        (and
            (patient ?POISON ?THING)
            (or
                (instance ?THING Organism)
                (instance ?THING AnatomicalStructure)))))

(=>
    (instance ?POISON Poisoning)
    (exists (?SUBSTANCE)
        (and
		(instance ?SUBSTANCE BiologicallyActiveSubstance)
            (instrument ?POISON ?SUBSTANCE))))

(subclass IntentionalProcess Process)
(documentation IntentionalProcess "A &%Process that has a specific
purpose for the &%CognitiveAgent who performs it.")

(=>
   (and
      (instance ?PROC IntentionalProcess)
      (agent ?PROC ?AGENT))
   (exists (?PURP)
      (hasPurposeForAgent ?PROC ?PURP ?AGENT)))

(=>
   (instance ?PROC IntentionalProcess)
   (exists (?AGENT)
      (and
         (instance ?AGENT CognitiveAgent)
         (agent ?PROC ?AGENT))))

(=>
   (and
      (instance ?PROC IntentionalProcess)
      (agent ?PROC ?HUMAN)
      (instance ?HUMAN Animal))
   (holdsDuring (WhenFn ?PROC) (attribute ?HUMAN Awake)))

(subclass IntentionalPsychologicalProcess IntentionalProcess)
(subclass IntentionalPsychologicalProcess PsychologicalProcess)
(documentation IntentionalPsychologicalProcess "An &%IntentionalProcess that
can be realized entirely within the mind or brain of an &%Organism.  Thus,
for example, &%Reasoning is a subclass of &%IntentionalPsychologicalProcess,
because one can reason simply by exercising one's mind/brain.  On the other
hand, &%RecreationOrExercise is not a subclass of &%IntentionalPsychologicalProcess,
because many instances of &%RecreationOrExercise necessarily have &%subProcesses
of &%BodyMotion.")

(subclass RecreationOrExercise IntentionalProcess)
(documentation RecreationOrExercise "A &%Process that is carried out for
the purpose of recreation or exercise.  Since &%RecreationOrExercise is a
subclass of &%IntentionalProcess, the intent of a process determines whether
or not it is an instance of the class.  Hence, if John and Bill watch the same
program on television, and John watches it to relax while Bill watches it solely
to satisfy an educational requirement, then John's watching the movie is an
instance of &%RecreationOrExercise, while Bill's is not (both cases of
watching the television program would however be in the class of &%Seeing, since
being an instance of this latter class is not determined by intention).")

(subclass OrganizationalProcess IntentionalProcess)
(documentation OrganizationalProcess "An &%IntentionalProcess that
involves an &%Organization.")

(=>
   (and
      (instance ?ACT OrganizationalProcess)
      (agent ?ACT ?AGENT))
   (or
      (instance ?AGENT Organization)
      (exists (?ORG)
         (and
            (instance ?ORG Organization)
            (member ?AGENT ?ORG)))))

(subclass Election OrganizationalProcess)
(documentation Election "&%Election is the class of events conducted by an
organization, in which qualified participants vote for officers, adopt
resolutions, or settle other issues in that &%Organization.")

(=>
	(and
		(instance ?EVENT Election)
		(agent ?EVENT ?AGENT)
		(instance ?AGENT GeopoliticalArea))
	(instance ?EVENT PoliticalProcess))

(=>
	(and
		(instance ?EVENT Election)
		(agent ?EVENT ?AGENT)
		(instance ?AREA GeopoliticalArea)
		(instance ?AGENT (GovernmentFn ?AREA)))
	(instance ?EVENT PoliticalProcess))

(subclass ReligiousProcess OrganizationalProcess)
(documentation ReligiousProcess "An &%OrganizationalProcess that is
carried out within or by a &%ReligiousOrganization.")

(=>
   (and
      (instance ?ACT ReligiousProcess)
      (agent ?ACT ?AGENT))
   (or
      (instance ?AGENT ReligiousOrganization)
      (exists (?ORG)
         (and
            (member ?AGENT ?ORG)
            (instance ?ORG ReligiousOrganization)))))

(subclass JoiningAnOrganization OrganizationalProcess)
(documentation JoiningAnOrganization "The &%OrganizationalProcess of
becoming a &%member of an &%Organization.")

(=>
   (and
      (instance ?JOIN JoiningAnOrganization)
      (instance ?ORG Organization)
      (agent ?JOIN ?PERSON)
      (patient ?JOIN ?ORG))
   (and
      (holdsDuring (BeginFn (WhenFn ?JOIN)) (not (member ?PERSON ?ORG)))
      (holdsDuring (EndFn (WhenFn ?JOIN)) (member ?PERSON ?ORG))))

(subclass LeavingAnOrganization OrganizationalProcess)
(disjoint LeavingAnOrganization JoiningAnOrganization)
(documentation LeavingAnOrganization "The &%OrganizationalProcess of
leaving an &%Organization, whether voluntarily or involuntarily.")

(=>
   (and
      (instance ?LEAVE LeavingAnOrganization)
      (instance ?ORG Organization)
      (agent ?LEAVE ?PERSON)
      (patient ?LEAVE ?ORG))
   (and
      (holdsDuring (BeginFn (WhenFn ?LEAVE)) (member ?PERSON ?ORG))
      (holdsDuring (EndFn (WhenFn ?LEAVE)) (not (member ?PERSON ?ORG)))))

(subclass Graduation LeavingAnOrganization)
(documentation Graduation "The &%OrganizationalProcess of graduating
from an &%EducationalOrganization.")

(=>
   (and
      (instance ?GRAD Graduation)
      (agent ?GRAD ?ORG)
      (patient ?GRAD ?PERSON))
   (instance ?ORG EducationalOrganization))

(subclass Matriculation JoiningAnOrganization)
(documentation Matriculation "The &%OrganizationalProcess of joining an
&%EducationalOrganization as a student.")

(=>
   (and
      (instance ?MAT Matriculation)
      (agent ?MAT ?ORG)
      (patient ?MAT ?PERSON))
   (instance ?ORG EducationalOrganization))

(subclass Hiring JoiningAnOrganization)
(documentation Hiring "&%OrganizationalProcesses where someone is made an
employee of an &%Organization.")

(=>
   (and
      (instance ?HIRE Hiring)
      (instance ?ORG Organization)
      (agent ?HIRE ?ORG)
      (patient ?HIRE ?PERSON))
   (and
      (holdsDuring (BeginFn (WhenFn ?HIRE)) (not (employs ?ORG ?PERSON)))
      (holdsDuring (EndFn (WhenFn ?HIRE)) (employs ?ORG ?PERSON))))

(subclass TerminatingEmployment LeavingAnOrganization)
(documentation TerminatingEmployment "&%OrganizationalProcesses where someone
ceases to be an employee of an &%Organization.  Note that this covers being
laid off, being fired, and voluntarily leaving a job.")

(=>
   (and
      (instance ?FIRE TerminatingEmployment)
      (instance ?ORG Organization)
      (agent ?FIRE ?ORG)
      (patient ?FIRE ?PERSON))
   (and
      (holdsDuring (BeginFn (WhenFn ?FIRE)) (employs ?ORG ?PERSON))
      (holdsDuring (EndFn (WhenFn ?FIRE)) (not (employs ?ORG ?PERSON)))))

(subclass PoliticalProcess OrganizationalProcess)
(documentation PoliticalProcess "An &%OrganizationalProcess carried
out by, for or against officially constituted governments.  Some examples
would be voting on proposed legislation, electing a government representative,
or even overthrowing a government in a revolution.")

(=>
   (instance ?PROC PoliticalProcess)
   (exists (?POL)
      (and
         (or
            (instance ?POL Government)
            (exists (?GOV)
               (and
                  (instance ?GOV Government)
                  (member ?POL ?GOV))))
         (or
            (agent ?PROC ?POL)
            (patient ?PROC ?POL)))))

(subclass JudicialProcess PoliticalProcess)
(documentation JudicialProcess "Any legal proceeding which is conducted
by a &%JudicialOrganization.  Note that there is an important difference
between the concepts &%LegalAction and &%JudicialProcess.  The former
refers to legal claims that are brought by a plaintiff, e.g. law suits,
while the second refers to trials and other sorts of judicial hearings
where the merits of a &%LegalAction are decided.")

(=>
   (and
      (instance ?PROCESS JudicialProcess)
      (agent ?PROCESS ?ORG)
      (instance ?ORG Organization))
   (instance ?ORG JudicialOrganization))

(subclass MilitaryProcess PoliticalProcess)
(documentation MilitaryProcess "Any &%Process that is carried out by a
military organization.  Note that this class covers &%Processes, e.g.
military operations, that are the result of careful planning, as well as
those which are unscripted.")

(subclass RegulatoryProcess Guiding)
(documentation RegulatoryProcess "an &%Guiding whose aim is the enforcement
of rules or regulations.  Note the key differences between &%RegulatoryProcess
and the related concept &%Managing.  The latter implies a long-term relationship
between a single manager and limited number of agents who are managed, while the
former implies a normative standard to which the activities of the regulated are
referred.")

(subclass Managing OrganizationalProcess)
(subclass Managing Guiding)
(documentation Managing "&%OrganizationalProcesses that involve overseeing
the activities of others.  Note the key differences between &%RegulatoryProcess
and its sibling &%Managing.  The latter implies a long-term relationship between
the manager and the managed, while the former implies a normative standard to which
the activities of the regulated are referred.")

(subclass Planning IntentionalPsychologicalProcess)
(documentation Planning "Specifying a set of actions in order to meet a
set of goals or objectives.")

(subclass Designing IntentionalPsychologicalProcess)
(documentation Designing "The spatial analogue of &%Planning.  &%Designing a
&%Collection of &%Objects involves determining a placement of the &%Objects
with respect to one another and perhaps other &%Objects as well, in order to
satisfy a particular purpose.")

(subclass Interpreting IntentionalPsychologicalProcess)
(documentation Interpreting "Any &%Process of assigning a &%Proposition to
a &%Text, i.e. understanding the &%Text.")

(=>
   (and
      (instance ?INTERPRET Interpreting)
      (agent ?INTERPRET ?AGENT)
      (patient ?INTERPRET ?CONTENT)
      (instance ?CONTENT ContentBearingObject))
   (exists (?PROP)
      (holdsDuring (EndFn (WhenFn ?INTERPRET)) (believes ?AGENT (containsInformation ?CONTENT ?PROP)))))

(subclass QuantityChange InternalChange)
(partition QuantityChange Increasing Decreasing)
(documentation QuantityChange "Any &%InternalChange where a &%PhysicalQuantity
associated with the &%patient is altered.")

(subclass Increasing QuantityChange)
(relatedInternalConcept Increasing Putting)
(documentation Increasing "Any &%QuantityChange where the &%PhysicalQuantity
is increased.")

(=>
   (and
      (instance ?INCREASE Increasing)
      (patient ?INCREASE ?OBJ))
   (exists (?UNIT ?QUANT1 ?QUANT2)
   	(and
         (holdsDuring (BeginFn (WhenFn ?INCREASE)) (equal (MeasureFn ?OBJ ?UNIT) ?QUANT1))
	   (holdsDuring (EndFn (WhenFn ?INCREASE)) (equal (MeasureFn ?OBJ ?UNIT) ?QUANT2))
	   (greaterThan ?QUANT2 ?QUANT1))))

(subclass Heating Increasing)
(disjoint Heating Cooling)
(documentation Heating "Any &%Increasing &%Process where the &%PhysicalQuantity
increased is a &%TemperatureMeasure.")

(=>
   (and
      (instance ?HEAT Heating)
      (patient ?HEAT ?OBJ))
   (exists (?UNIT ?QUANT1 ?QUANT2)
   	(and
         (instance ?UNIT TemperatureMeasure)
         (holdsDuring (BeginFn (WhenFn ?HEAT)) (equal (MeasureFn ?OBJ ?UNIT) ?QUANT1))
	   (holdsDuring (EndFn (WhenFn ?HEAT)) (equal (MeasureFn ?OBJ ?UNIT) ?QUANT2))
	   (greaterThan ?QUANT2 ?QUANT1))))

(subclass Decreasing QuantityChange)
(relatedInternalConcept Decreasing Removing)
(documentation Decreasing "Any &%QuantityChange where the &%PhysicalQuantity
is decreased.")

(=>
   (and
      (instance ?DECREASE Decreasing)
      (patient ?DECREASE ?OBJ))
   (exists (?UNIT ?QUANT1 ?QUANT2)
   	(and
         (holdsDuring (BeginFn (WhenFn ?DECREASE)) (equal (MeasureFn ?OBJ ?UNIT) ?QUANT1))
	   (holdsDuring (EndFn (WhenFn ?DECREASE)) (equal (MeasureFn ?OBJ ?UNIT) ?QUANT2))
	   (lessThan ?QUANT2 ?QUANT1))))

(subclass Cooling Decreasing)
(documentation Cooling "Any &%Decreasing &%Process where the &%PhysicalQuantity
decreased is a &%TemperatureMeasure.")

(=>
   (and
      (instance ?COOL Cooling)
      (patient ?COOL ?OBJ))
   (exists (?UNIT ?QUANT1 ?QUANT2)
   	(and
         (instance ?UNIT TemperatureMeasure)
         (holdsDuring (BeginFn (WhenFn ?COOL)) (equal (MeasureFn ?OBJ ?UNIT) ?QUANT1))
	   (holdsDuring (EndFn (WhenFn ?COOL)) (equal (MeasureFn ?OBJ ?UNIT) ?QUANT2))
	   (lessThan ?QUANT2 ?QUANT1))))

(subclass Motion Process)
(documentation Motion "Any &%Process of movement.")

(=>
   (and
      (instance ?MOTION Motion)
      (patient ?MOTION ?OBJ)
      (origin ?MOTION ?PLACE))
   (holdsDuring (BeginFn (WhenFn ?MOTION)) (located ?OBJ ?PLACE)))

(=>
   (and
      (instance ?MOTION Motion)
      (patient ?MOTION ?OBJ)
      (destination ?MOTION ?PLACE))
   (holdsDuring (EndFn (WhenFn ?MOTION)) (located ?OBJ ?PLACE)))

(instance path CaseRole)
(domain path 1 Motion)
(domain path 2 Object)
(documentation path "(&%path ?MOTION ?PATH) means that ?PATH is a route
along which ?MOTION occurs.  For example, Highway 101 is the path in the
following proposition:  the car drove up Highway 101.")

(=>
   (and
      (path ?PROCESS ?PATH1)
      (origin ?PROCESS ?SOURCE)
      (destination ?PROCESS ?DEST)
      (length ?PATH1 ?MEASURE1)
      (distance ?SOURCE ?DEST ?DISTANCE)
      (not (greaterThan ?MEASURE1 ?DISTANCE)))
   (forall (?OBJ)
      (=>
         (part ?OBJ ?PATH1)
         (between ?SOURCE ?OBJ ?DEST))))

(subclass BodyMotion Motion)
(documentation BodyMotion "Any &%Motion where the &%agent is an &%Organism
and the &%patient is a &%BodyPart.")

(=>
   (instance ?MOTION BodyMotion)
   (exists (?OBJ ?AGENT)
      (and
         (instance ?OBJ BodyPart)
         (patient ?MOTION ?OBJ)
         (instance ?AGENT Organism)
         (agent ?MOTION ?AGENT))))

(subclass Vocalizing RadiatingSound)
(subclass Vocalizing BodyMotion)
(documentation Vocalizing "Any instance of &%RadiatingSound where the
&%instrument is the &%Human vocal cords.  This covers grunts, screams,
roars, as well as &%Speaking.")

(=>
   (instance ?VOCAL Vocalizing)
   (exists (?HUMAN)
      (and
         (instance ?HUMAN Human)
         (agent ?VOCAL ?HUMAN)
         (instrument ?VOCAL ?HUMAN))))

(subclass OralCommunicating Process)
(documentation OralCommunicating "")

(subclass Speaking LinguisticCommunication)
(subclass Speaking Vocalizing)
(subclass Speaking CommunicationsChannel)
(documentation Speaking "Any &%LinguisticGeneration which is also a
&%Vocalizing, i.e. any &%LinguisticCommunication by a &%Human which
involves his/her vocal cords. This is a process in which IntelligentAgents communicate by exchanging spoken sounds, whether in person (not necessarily face-to-face, but where the sounds are not converted to other forms), or at a distance via electronic or other means of converting sound to signal and back.")

(subclass Singing Speaking)
(subclass Singing Music)
(documentation Singing "&%Speaking that is also &%Music.")

(subclass Ambulating BodyMotion)
(subclass Ambulating Translocation)
(partition Ambulating Walking Running)
(documentation Ambulating "Any &%BodyMotion which is accomplished by
means of the legs of an &%Animal for the purpose of moving from one
point to another.")

(subclass Walking Ambulating)
(documentation Walking "&%Ambulating relatively slowly, i.e. moving in such a way that at least one foot is always in contact with the ground.")

(subclass Running Ambulating)
(documentation Running "&%Ambulating relatively quickly, i.e. moving in such a way that, with each step, neither foot is in contact with the ground for a period of time.")

(=>
   (and
      (instance ?WALK Walking)
      (instance ?RUN Running)
      (agent ?WALK ?AGENT)
      (agent ?RUN ?AGENT)
      (holdsDuring (WhenFn ?WALK) (measure ?AGENT (SpeedFn ?LENGTH1 ?TIME)))
      (holdsDuring (WhenFn ?RUN) (measure ?AGENT (SpeedFn ?LENGTH2 ?TIME))))
   (greaterThan ?LENGTH2 ?LENGTH1))

(subclass Swimming BodyMotion)
(documentation Swimming "Any deliberate and controlled &%BodyMotion
through water that is accomplished by an &%Organism.")

(=>
   (and
      (instance ?SWIM Swimming)
      (agent ?SWIM ?AGENT))
   (exists (?AREA)
      (and
         (instance ?AREA WaterArea)
         (located ?AGENT ?AREA))))

(subclass Dancing BodyMotion)
(documentation Dancing "Any &%BodyMotion of &%Humans which is
deliberately coordinated with music.")

;; SUMO Mid-Level uses this rather than the disjoint
;;   proposition.  For simplicity, we leave in the
;;   disjopint proposition -- PJC 01-19-04
;;(subclass GeologicalProcess (ComplementFn IntentionalProcess))

(subclass GeologicalProcess Motion)
(disjoint GeologicalProcess IntentionalProcess)
(documentation GeologicalProcess "The class of activities that
are caused by geological forces and affect geological features,
and which may affect the biosphere as well.")

(subclass DirectionChange Motion)
(documentation DirectionChange "The act of changing the direction in
which the &%patient of the act is oriented.")

(=>
   (instance ?PROC DirectionChange)
   (exists (?ATTR)
      (and
         (instance ?ATTR DirectionalAttribute)
         (or
            (and
               (holdsDuring (BeginFn (WhenFn ?PROC)) (manner ?PROC ?ATTR))
               (holdsDuring (EndFn (WhenFn ?PROC)) (not (manner ?PROC ?ATTR))))
            (and
               (holdsDuring (EndFn (WhenFn ?PROC)) (manner ?PROC ?ATTR))
               (holdsDuring (BeginFn (WhenFn ?PROC)) (not (manner ?PROC ?ATTR))))))))

(subclass Transfer Translocation)
(documentation Transfer "Any &%instance of &%Translocation where the &%agent
and the &%patient are not the same thing.")

(=>
   (and
      (instance ?TRANSFER Transfer)
      (agent ?TRANSFER ?AGENT)
      (patient ?TRANSFER ?PATIENT))
   (not
      (equal ?AGENT ?PATIENT)))

(subclass Carrying Transfer)
(documentation Carrying "&%Transfer from one point to another by means of
an &%Animal or &%Human.")

(=>
   (instance ?CARRY Carrying)
   (exists (?ANIMAL)
      (and
         (instance ?ANIMAL Animal)
         (instrument ?TRANS ?ANIMAL))))

(subclass Removing Transfer)
(documentation Removing "The &%Class of &%Processes where something is
taken away from a location.  Note that the thing removed and the location
are specified with the &%CaseRoles &%patient and &%origin, respectively.")

(=>
   (and
      (instance ?REMOVE Removing)
      (origin ?REMOVE ?PLACE)
      (patient ?REMOVE ?OBJ))
   (and
      (holdsDuring (BeginFn (WhenFn ?REMOVE)) (located ?OBJ ?PLACE))
      (holdsDuring (EndFn (WhenFn ?REMOVE)) (not (located ?OBJ ?PLACE)))))

(subclass Uncovering Removing)
(disjoint Uncovering Covering)
(documentation Uncovering "The &%Class of &%Removing processes where the &%agent
uncovers the &%patient, either completely or only partially.")

(subclass Putting Transfer)
(documentation Putting "The &%Class of &%Processes where something is put
in a location.  Note that the location is specified with the &%CaseRole
&%destination.")

(=>
   (and
      (instance ?PUT Putting)
      (destination ?PUT ?PLACE)
      (patient ?PUT ?OBJ))
   (and
      (holdsDuring (BeginFn (WhenFn ?PUT)) (not (located ?OBJ ?PLACE)))
      (holdsDuring (EndFn (WhenFn ?PUT)) (located ?OBJ ?PLACE))))

(subclass Covering Putting)
(documentation Covering "The &%Class of &%Putting processes where the &%agent
covers the &%patient, either completely or only partially, with something
else.")

(subclass Dressing Putting)
(subclass Dressing Covering)
(documentation Dressing "The &%Process of putting on &%Clothing.")

(=>
   (and
      (instance ?DRESS Dressing)
      (agent ?DRESS ?AGENT)
      (patient ?DRESS ?CLOTHING))
   (and
      (instance ?CLOTHING Clothing)
      (holdsDuring (BeginFn (WhenFn ?DRESS)) (not (wears ?AGENT ?CLOTHING)))
      (holdsDuring (EndFn (WhenFn ?DRESS)) (wears ?AGENT ?CLOTHING))))

(subclass Inserting Putting)
(documentation Inserting "&%Putting one thing inside of another thing.")

(=>
   (and
      (instance ?INSERT Inserting)
      (patient ?INSERT ?OBJ1)
      (destination ?INSERT ?OBJ2))
   (and
      (holdsDuring (BeginFn (WhenFn ?INSERT)) (not (contains ?OBJ2 ?OBJ1)))
      (holdsDuring (EndFn (WhenFn ?INSERT)) (contains ?OBJ2 ?OBJ1))))

(subclass Injecting Inserting)
(documentation Injecting "&%Inserting a &%BiologicallyActiveSubstance into an &%Animal or a &%Human with a syringe.")

(=>
   (instance ?INJECT Injecting)
   (exists (?SUBSTANCE ?ANIMAL)
      (and
         (patient ?INJECT ?SUBSTANCE)
         (instance ?SUBSTANCE BiologicallyActiveSubstance)
         (attribute ?SUBSTANCE Liquid)
         (destination ?INJECT ?ANIMAL)
         (instance ?ANIMAL Animal))))

(subclass Substituting Transfer)
(subclass Substituting DualObjectProcess)
(documentation Substituting "The &%Class of &%Transfers where one thing is
replaced with something else.")

(=>
   (instance ?SUB Substituting)
   (exists (?PUT ?REMOVE ?OBJ1 ?OBJ2 ?PLACE)
      (and
         (instance ?PUT Putting)
         (instance ?REMOVE Removing)
         (subProcess ?PUT ?SUB)
         (subProcess ?REMOVE ?SUB)
         (patient ?REMOVE ?OBJ1)
         (origin ?REMOVE ?PLACE)
         (patient ?PUT ?OBJ2)
         (destination ?PUT ?PLACE)
         (not (equal ?OBJ1 ?OBJ2)))))

(subclass Impelling Transfer)
(documentation Impelling "The &%subclass of &%Transfer where the &%patient
travels through space by means of a sudden, forceful event.  Some examples
would be shooting, throwing, tossing, etc.")

(subclass Shooting Impelling)
(documentation Shooting "The &%subclass of &%Impelling where the &%patient
is a projectile that is fired through the air by means of some sort of
&%Device.")

(subclass Touching Transfer)
(documentation Touching "Any &%Transfer where two &%Objects are
brought into immediate physical contact with one another.")

(=>
   (and
      (instance ?TOUCH Touching)
      (agent ?TOUCH ?OBJ1)
      (patient ?TOUCH ?OBJ2))
   (and
      (holdsDuring (BeginFn (WhenFn ?TOUCH)) (not (connected ?OBJ1 ?OBJ2)))
      (holdsDuring (EndFn (WhenFn ?TOUCH)) (connected ?OBJ1 ?OBJ2))))

(subrelation grasps meetsSpatially)
(domain grasps 1 Animal)
(domain grasps 2 Object)
(documentation grasps "The state of grasping an &%Object.  (&%grasps
?ANIMAL ?OBJ) means that the &%Animal ?ANIMAL is intentionally holding
on to the &%Object ?OBJ.")

(subclass Grabbing Touching)
(documentation Grabbing "Any instance of &%Touching which results in
a situation where the &%agent &%grasps the &%patient of the &%Touching.")

(=>
   (and
      (instance ?GRAB Grabbing)
      (agent ?GRAB ?AGENT)
      (patient ?GRAB ?THING))
   (and
      (holdsDuring (BeginFn (WhenFn ?GRAB)) (not (grasps ?AGENT ?THING)))
      (holdsDuring (EndFn (WhenFn ?GRAB)) (grasps ?AGENT ?THING))))

(subclass Releasing Transfer)
(documentation Releasing "Any instance of &%Transfer which results in
a situation where it is not the case that the &%agent &%grasps something
which he/she &%grasps previously.")

(=>
   (and
      (instance ?RELEASE Releasing)
      (agent ?GRAB ?AGENT)
      (patient ?GRAB ?THING))
   (and
      (holdsDuring (BeginFn (WhenFn ?RELEASE)) (grasps ?AGENT ?THING))
      (holdsDuring (EndFn (WhenFn ?RELEASE)) (not (grasps ?AGENT ?THING)))))

(subclass Impacting Touching)
(documentation Impacting "Any &%Touching where something comes into
sudden, forceful, physical contact with something else.  Some examples
would be striking, knocking, whipping etc.")

(=>
   (and
      (instance ?IMPACT Impacting)
      (patient ?IMPACT ?OBJ))
   (exists (?IMPEL)
      (and
         (instance ?IMPEL Impelling)
         (patient ?IMPEL ?OBJ)
         (earlier (WhenFn ?IMPEL) (WhenFn ?IMPACT)))))

(subclass Translocation Motion)
(documentation Translocation "&%Translocation is that class of &%Motions
in which an object moves from one place to another.  In the case of round
trips, the &%origin and &%destination are the same, but the intervening
motion passes through other locations.  &%Translocation represents linear
motion, in contrast to rotation or other movement in place.  A vehicle is
not necessary; &%Ambulating is a kind of &%Translocation.")

(=>
	(and
		(instance ?MOVEMENT Translocation)
		(origin ?MOVEMENT ?PLACE1))
	(exists (?PLACE2 ?STAGE)
		(and
			(instance ?PLACE2 Region)
			(not (equal ?PLACE1 ?PLACE2))
			(subProcess ?STAGE ?MOVEMENT)
			(located ?STAGE ?PLACE2))))

(subclass Transportation Translocation)
(relatedInternalConcept Transportation TransportationDevice)
(documentation Transportation "&%Motion from one point to another by means
of a &%TransportationDevice.")

(=>
   (instance ?TRANS Transportation)
   (exists (?DEVICE)
      (and
         (instance ?DEVICE TransportationDevice)
         (instrument ?TRANS ?DEVICE))))

(subclass Guiding IntentionalProcess)
(documentation Guiding "Any &%IntentionalProcess where the &%agent tries to
direct the movements of another &%Object, whether an &%Agent or not.")

(subclass Steering Guiding)
(documentation Steering "Controlling the direction and/or speed of a
&%TransportationDevice.  This includes navigating a ship, driving a car
or truck, operating a train, etc.")

(=>
   (instance ?STEER Steering)
   (exists (?VEHICLE)
      (and
         (instance ?VEHICLE TransportationDevice)
         (patient ?STEER ?VEHICLE))))

(subclass EducationalProcess Guiding)
(documentation EducationalProcess "Any &%Process which is intended to result
in &%Learning.")

(=>
   (and
      (instance ?EDUCATION EducationalProcess)
      (patient ?EDUCATION ?PERSON))
   (hasPurpose ?EDUCATION (exists (?LEARN)
                             (and
                                (instance ?LEARN Learning)
                                (patient ?LEARN ?PERSON)))))

(subclass ChangeOfPossession SocialInteraction)
(relatedInternalConcept ChangeOfPossession possesses)
(documentation ChangeOfPossession "The &%Class of &%Processes where
ownership of something is transferred from one &%Agent to another.")

(=>
   (and
      (instance ?CHANGE ChangeOfPossession)
      (patient ?CHANGE ?OBJ)
      (holdsDuring (BeginFn (WhenFn ?CHANGE)) (possesses ?AGENT1 ?OBJ))
      (holdsDuring (EndFn (WhenFn ?CHANGE)) (possesses ?AGENT2 ?OBJ)))
   (not
      (equal ?AGENT1 ?AGENT2)))

(=>
   (and
      (instance ?CHANGE ChangeOfPossession)
      (origin ?CHANGE ?AGENT1)
      (destination ?CHANGE ?AGENT2)
      (instance ?AGENT1 Agent)
      (instance ?AGENT2 Agent)
      (patient ?CHANGE ?OBJ))
   (and
      (holdsDuring (BeginFn (WhenFn ?CHANGE)) (possesses ?AGENT1 ?OBJ))
      (holdsDuring (EndFn (WhenFn ?CHANGE)) (possesses ?AGENT2 ?OBJ))))

(subclass Giving ChangeOfPossession)
(documentation Giving "The &%subclass of &%ChangeOfPossession where the
&%agent gives the &%destination something.")

(=>
   (and
      (instance ?GIVE Giving)
      (agent ?GIVE ?AGENT1)
      (destination ?GIVE ?AGENT2)
      (instance ?AGENT2 Agent)
      (patient ?GIVE ?OBJ))
   (exists (?GET)
      (and
         (instance ?GET Getting)
         (agent ?GET ?AGENT2)
         (origin ?GET ?AGENT1)
         (patient ?GET ?OBJ))))

(=>
   (and
      (instance ?GIVE Giving)
      (agent ?GIVE ?AGENT))
   (origin ?GIVE ?AGENT))

(subclass Funding Giving)
(documentation Funding "Any instance of &%Giving where the &%patient is an
instance of &%Currency.  Note that this class covers both
&%FinancialTransactions, e.g. where a firm funds a software company with
venture capital with the agreement that a certain percentage of the profits
on the investment will be returned to the firm, and instances of
&%UnilateralGiving, e.g. a stipend provided to a student as part of
scholarship or fellowship.")

(=>
   (instance ?FUND Funding)
   (exists (?MONEY)
      (and
         (instance ?MONEY Currency)
         (patient ?FUND ?MONEY))))

(subclass UnilateralGiving Giving)
(documentation UnilateralGiving "Any instance of &%Giving that is not part
of a &%Transaction.  In other words, any instance of &%Giving where nothing
is received in return.  Some examples of &%UnilateralGiving are:  honorary
awards, gifts, and financial grants.")

(=>
   (instance ?GIVE UnilateralGiving)
   (not
      (exists (?TRANS)
         (and
            (instance ?TRANS Transaction)
            (subProcess ?GIVE ?TRANS)))))

(subclass Lending Giving)
(documentation Lending "The &%subclass of &%Giving &%Processes where
the &%agent gives the &%destination something for a limited period of
time with the expectation that it will be returned later (perhaps with
interest).")

(<=>
   (exists (?BORROW)
      (and
         (instance ?BORROW Borrowing)
         (agent ?BORROW ?AGENT1)
         (origin ?BORROW ?AGENT2)
         (patient ?BORROW ?OBJECT)))
   (exists (?LEND)
      (and
         (instance ?LEND Lending)
         (agent ?LEND ?AGENT2)
         (destination ?LEND ?AGENT1)
         (patient ?LEND ?OBJECT))))

(subclass GivingBack Giving)
(documentation GivingBack "Any instance of &%Giving where the &%agent gives
something to the &%destination which was previously given to the &%agent by
the &%destination, e.g. returing a book that was borrowed from someone.")

(=>
   (and
      (instance ?RETURN GivingBack)
      (agent ?RETURN ?AGENT)
      (destination ?RETURN ?DEST))
   (exists (?GIVE)
      (and
         (instance ?GIVE Giving)
         (agent ?GIVE ?DEST)
         (destination ?GIVE ?AGENT)
         (earlier (WhenFn ?GIVE) (WhenFn ?RETURN)))))

(subclass Getting ChangeOfPossession)
(documentation Getting "The &%subclass of &%ChangeOfPossession where the
&%agent gets something.  Note that the source from which something is
obtained is specified with the &%origin &%CaseRole.")

(=>
   (and
      (instance ?GET Getting)
      (agent ?GET ?AGENT))
   (destination ?GET ?AGENT))

(subclass UnilateralGetting Getting)
(relatedInternalConcept UnilateralGetting UnilateralGiving)
(documentation UnilateralGetting "Any instance of &%Getting that is not part
of a &%Transaction.  In other words, any instance of &%Getting where nothing
is given in return.  Some examples of &%UnilateralGetting are:  appropriating,
commandeering, stealing, etc.")

(=>
   (instance ?GET UnilateralGetting)
   (not
      (exists (?TRANS)
         (and
            (instance ?TRANS Transaction)
            (subProcess ?GET ?TRANS)))))

(subclass Borrowing Getting)
(documentation Borrowing "The &%subclass of &%Getting &%Processes where
the &%agent gets something for a limited period of time with the expectation
that it will be returned later (perhaps with interest).")

(subclass Transaction ChangeOfPossession)
(subclass Transaction DualObjectProcess)
(documentation Transaction "The &%subclass of &%ChangeOfPossession where
something is exchanged for something else.")

(=>
   (instance ?TRANS Transaction)
   (exists (?AGENT1 ?AGENT2 ?GIVE1 ?GIVE2 ?OBJ1 ?OBJ2)
      (and
         (instance ?GIVE1 Giving)
         (instance ?GIVE2 Giving)
	   (subProcess ?GIVE1 ?TRANS)
   	   (subProcess ?GIVE2 ?TRANS)
         (agent ?GIVE1 ?AGENT1)
         (agent ?GIVE2 ?AGENT2)
         (patient ?GIVE1 ?OBJ1)
         (patient ?GIVE2 ?OBJ2)
         (destination ?GIVE1 ?AGENT2)
         (destination ?GIVE2 ?AGENT1)
         (not
            (equal ?AGENT1 ?AGENT2))
         (not
            (equal ?OBJ1 ?OBJ2)))))

(subclass FinancialTransaction Transaction)
(documentation FinancialTransaction "A &%Transaction where an instance
of &%CurrencyMeasure is exchanged for something else.")

(=>
   (instance ?TRANS FinancialTransaction)
   (exists (?OBJ)
      (and
         (patient ?TRANS ?OBJ)
         (instance ?OBJ CurrencyMeasure))))

(instance transactionAmount BinaryPredicate)
(instance transactionAmount SingleValuedRelation)
(instance transactionAmount TotalValuedRelation)
(domain transactionAmount 1 FinancialTransaction)
(domain transactionAmount 2 CurrencyMeasure)
(documentation transactionAmount "(&%transactionAmount ?Transaction
?Amount) means that ?Amount is an instance of &%CurrencyMeasure being
exhanged in a &%FinancialTransaction ?Transaction.")

(=>
   (transactionAmount ?Transaction ?Amount)
   (exists (?Obj)
      (and
         (patient ?Transaction ?Obj)
         (monetaryValue ?Obj ?Amount))))

(subclass CommercialService FinancialTransaction)
(documentation CommercialService "Any &%FinancialTransaction by a
&%Corporation where the aim is to produce a &%profit.")

(=>
   (instance ?BUSINESS CommercialService)
   (hasPurpose ?BUSINESS
      (exists (?PROFIT)
         (profit ?BUSINESS ?PROFIT))))

(=>
	(instance ?Service CommercialService)
	(exists (?Org ?Agent)
            (and
			(instance ?Org Corporation)
                  (employs ?Org ?Agent)
			(agent ?Service ?Agent))))

;; NOTE THAT the axiom below was changed from the original to
;; use CommercialAgent rather than Corporation as the
;; Agent

(=>
   (instance ?BUSINESS CommercialService)
   (exists (?AGENT)
      (and
         (instance ?AGENT CommercialAgent)
         (agent ?BUSINESS ?AGENT))))

(subclass Betting FinancialTransaction)
(documentation Betting "A &%FinancialTransaction where an instance of
&%CurrencyMeasure is exchanged for the possibility of winning a larger
instance of &%CurrencyMeasure within the context of some sort of
&%Game.")

(subclass Buying FinancialTransaction)
(relatedInternalConcept Buying Selling)
(documentation Buying "A &%FinancialTransaction in which an instance of
&%CurrencyMeasure is exchanged for an instance of &%Physical.")

(=>
   (and
      (instance ?BUY Buying)
      (agent ?BUY ?AGENT))
   (destination ?BUY ?AGENT))

(subclass Selling FinancialTransaction)
(documentation Selling "A &%FinancialTransaction in which an instance of
&%Physical is exchanged for an instance of &%CurrencyMeasure.")

(<=>
   (exists (?BUY)
      (and
         (instance ?BUY Buying)
         (agent ?BUY ?AGENT1)
         (origin ?BUY ?AGENT2)
         (patient ?BUY ?OBJECT)))
   (exists (?SELL)
      (and
         (instance ?SELL Selling)
         (agent ?SELL ?AGENT2)
         (destination ?SELL ?AGENT1)
         (patient ?SELL ?OBJECT))))

(=>
   (and
      (instance ?SELL Selling)
      (agent ?SELL ?AGENT))
   (origin ?SELL ?AGENT))

(subclass Learning IntentionalPsychologicalProcess)
(documentation Learning "The &%Class of &%Processes which relate to the
acquisition of information.")

(=>
   (and
      (instance ?LEARN Learning)
      (agent ?LEARN ?AGENT))
   (instance ?AGENT CognitiveAgent))

(=>
   (and
      (instance ?LEARN Learning)
      (agent ?LEARN ?AGENT)
      (patient ?LEARN ?PROP))
   (and
      (holdsDuring (BeginFn (WhenFn ?LEARN)) (not (knows ?AGENT ?PROP)))
      (holdsDuring (EndFn (WhenFn ?LEARN)) (knows ?AGENT ?PROP))))

(subclass Discovering IntentionalPsychologicalProcess)
(documentation Discovering "Finding something that was sought.  Note that
this class is restricted to cases of discovering something &%Physical.
For cases involving the acquisition of knowledge, the class &%Learning
should be used.")

(=>
   (and
      (instance ?DISCOVER Discovering)
      (patient ?DISCOVER ?OBJ))
   (exists (?PURSUE)
      (and
         (instance ?PURSUE Pursuing)
         (meetsTemporally (WhenFn ?PURSUE) (WhenFn ?DISCOVER)))))

(=>
   (and
      (instance ?DISCOVER Discovering)
      (patient ?DISCOVER ?OBJ)
      (holdsDuring (WhenFn ?DISCOVER) (located ?OBJ ?PLACE)))
   (exists (?LEARN)
      (and
         (instance ?LEARN Learning)
         (subProcess ?LEARN ?DISCOVER)
         (patient ?LEARN (located ?OBJ ?PLACE)))))

(subclass Classifying IntentionalPsychologicalProcess)
(documentation Classifying "The &%Class of &%IntentionalPsychologicalProcesses
which involve attaching a name or category to a thing or set of things.
Note that &%Classifying is distinguished from &%Learning by the fact
that the latter covers the acquisition by a &%CognitiveAgent of any
&%Proposition, while the former involves the assignment of a label
or category.")

(subclass Reasoning IntentionalPsychologicalProcess)
(documentation Reasoning "The &%Class of &%IntentionalPsychologicalProcesses
which involve concluding, on the basis of either deductive or inductive
evidence, that a particular &%Proposition or &%Sentence is true.")

(=>
   (instance ?AGENT CognitiveAgent)
   (capability Reasoning agent ?AGENT))

(subclass Selecting IntentionalPsychologicalProcess)
(documentation Selecting "The &%Class of &%IntentionalPsychologicalProcesses
which involve opting for one or more &%Entity out of a larger set of &%Entities.")

(subclass Deciding Selecting)
(documentation Deciding "The subclass of &%Selecting where the &%agent
opts for one course of action out of a set of multiple possibilities.")

(=>
   (and
      (instance ?DECIDE Deciding)
      (agent ?DECIDE ?AGENT)
      (patient ?DECIDE ?PROCESS))
   (and
      (instance ?PROCESS IntentionalProcess)
      (agent ?PROCESS ?AGENT)))

(subclass LegalDecision Deciding)
(subclass LegalDecision Declaring)
(documentation LegalDecision "A decision issued by a court with respect to
a &%LegalAction.")

(=>
   (instance ?DECISION LegalDecision)
   (exists (?ACTION)
      (and
         (instance ?ACTION LegalAction)
         (refers ?DECISION ?ACTION))))

(=>
   (instance ?DECISION LegalDecision)
   (exists (?PROCESS)
      (and
         (instance ?PROCESS JudicialProcess)
         (subProcess ?DECISION ?PROCESS))))

(subclass Voting Selecting)
(documentation Voting "&%Voting is the activity of voting in an
&%Election.  Voting is typically done by individuals, while &%Elections
are conducted by &%Organizations.  The voting process by an individual
voter is part of an &%Election process.")

(=>
	(instance ?VOTE Voting)
	(exists (?ELECT)
		(and
			(instance ?ELECT Election)
			(subProcess ?VOTE ?ELECT))))

(subclass Comparing IntentionalPsychologicalProcess)
(subclass Comparing DualObjectProcess)
(documentation Comparing "The &%Class of &%IntentionalPsychologicalProcesses
which involve comparing, relating, contrasting, etc. the properties of
two or more &%Entities.")

(subclass Calculating IntentionalPsychologicalProcess)
(documentation Calculating "&%IntentionalPsychologicalProcesses which involve
the consideration and/or manipulation of instances of &%Quantity.")

(subclass Measuring Calculating)
(documentation Measuring "The &%Class of &%Calculating &%Processes where
the aim is to determine the &%PhysicalQuantity of some aspect of the &%patient.")

(=>
   (and
      (instance ?MEAS Measuring)
      (agent ?MEAS ?AGENT)
      (patient ?MEAS ?OBJ))
   (exists (?QUANT ?UNIT)
      (holdsDuring (EndFn (WhenFn ?MEAS))
         (knows ?AGENT (measure ?OBJ (MeasureFn ?QUANT ?UNIT))))))

(subclass Counting Calculating)
(documentation Counting "Enumerating something.  The &%Class of &%Calculating
&%Processes where the aim is to determine the &%Number corresponding to the
&%patient.")

(=>
   (and
      (instance ?COUNT Counting)
      (agent ?COUNT ?AGENT)
      (patient ?COUNT ?ENTITY))
   (exists (?NUMBER)
      (knows ?AGENT (equal (CardinalityFn ?ENTITY) ?NUMBER))))

(subclass Predicting IntentionalPsychologicalProcess)
(documentation Predicting "The &%Class of &%IntentionalPsychologicalProcesses
which involve the formulation of a &%Proposition about a state of affairs
which might be realized in the future.")

(=>
   (and
      (instance ?PREDICT Predicting)
      (patient ?PREDICT ?FORMULA))
   (exists (?TIME)
      (and
         (holdsDuring ?TIME ?FORMULA)
         (or
            (before ?TIME (WhenFn ?PREDICT))
            (earlier ?TIME (WhenFn ?PREDICT))))))

(subclass Remembering PsychologicalProcess)
(documentation Remembering "The &%Class of &%PsychologicalProcesses which
involve the recollection of prior experiences and/or of knowledge
which was previously acquired.")

(=>
   (and
      (instance ?REMEMBER Remembering)
      (patient ?REMEMBER ?FORMULA))
   (exists (?TIME)
      (and
         (holdsDuring ?TIME ?FORMULA)
         (or
            (before ?TIME (WhenFn ?REMEMBER))
            (earlier ?TIME (WhenFn ?REMEMBER))))))

(subclass Keeping IntentionalProcess)
(documentation Keeping "The &%Class of &%Processes where the &%agent
keeps something in a particular location for an extended period of time.")

(=>
   (and
      (instance ?KEEP Keeping)
      (agent ?KEEP ?AGENT)
      (patient ?KEEP ?OBJ))
   (exists (?PUT)
      (and
         (instance ?PUT Putting)
         (agent ?PUT ?AGENT)
         (patient ?PUT ?OBJ)
         (earlier (WhenFn ?PUT) (WhenFn ?KEEP)))))

(=>
   (and
      (instance ?KEEP Keeping)
      (patient ?KEEP ?OBJ))
   (exists (?PLACE)
      (forall (?TIME)
         (=>
            (temporalPart ?TIME (WhenFn ?KEEP))
            (holdsDuring ?TIME (located ?OBJ ?PLACE))))))

(subclass Confining Keeping)
(documentation Confining "The &%Class of &%Keeping &%Processes where the
&%patient is a &%Human or an &%Animal and is kept involuntarily.  This covers
caging, imprisonment, jailing, etc.")

(=>
   (instance ?CONFINE Confining)
   (exists (?AGENT)
      (and
         (or
            (instance ?AGENT Animal)
            (instance ?AGENT Human))
         (patient ?CONFINE ?AGENT))))

(=>
   (and
      (instance ?CONFINE Confining)
      (patient ?CONFINE ?PERSON)
      (instance ?PERSON Human))
   (not (desires ?PERSON (patient ?CONFINE ?PERSON))))

(subclass Maintaining IntentionalProcess)
(documentation Maintaining "The &%Class of &%Processes where the &%agent
cares for or maintains the &%Object.")

(subclass Repairing IntentionalProcess)
(relatedInternalConcept Repairing Maintaining)
(documentation Repairing "The &%Class of &%Processes where the &%agent
makes a modification or series of modifications to an &%Object that is not
functioning as intended so that it works properly.")

(=>
   (and
      (instance ?REPAIR Repairing)
      (patient ?REPAIR ?OBJ))
   (exists (?DAMAGE)
      (and
         (instance ?DAMAGE Damaging)
         (patient ?DAMAGE ?OBJ)
         (earlier (WhenFn ?DAMAGE) (WhenFn ?REPAIR)))))

(subclass TherapeuticProcess Repairing)
(documentation TherapeuticProcess "A &%Process that is carried out
for the purpose of curing, improving or reducing the pain associated
with a &%DiseaseOrSyndrome.")

(=>
   (and
      (instance ?PROC TherapeuticProcess)
      (patient ?PROC ?BIO))
   (or
      (instance ?BIO Organism)
      (exists (?ORG)
         (and
            (instance ?ORG Organism)
            (part ?BIO ?ORG)))))

(subclass Surgery TherapeuticProcess)
(documentation Surgery "Any &%TherapeuticProcess that involves making an
incision in the &%Animal that is the &%patient of the &%TherapeuticProcess.")

(=>
   (and
      (instance ?ACT Surgery)
      (patient ?ACT ?ANIMAL))
   (exists (?SUBACT)
      (and
         (instance ?SUBACT Cutting)
         (instance ?ANIMAL Animal)
         (patient ?ANIMAL ?CUTTING)
         (subProcess ?SUBACT ?ACT))))

(subclass Damaging InternalChange)
(disjoint Damaging Repairing)
(documentation Damaging "The &%Class of &%Processes where the &%agent
brings about a situation where the &%patient no longer functions normally
or as intended.")

(subclass Destruction Damaging)
(documentation Destruction "The &%subclass of &%Damagings in which
the &%patient (or an essential element of the &%patient) is destroyed.
Note that the difference between this concept and its superclass is solely
one of extent.")

(<=>
   (instance ?PROCESS Destruction)
   (exists (?PATIENT)
      (and
         (patient ?PROCESS ?PATIENT)
         (time ?PATIENT (BeginFn(WhenFn ?PROCESS)))
         (not
            (time ?PATIENT (EndFn (WhenFn ?PROCESS)))))))

(subclass Killing Destruction)
(documentation Killing "The &%subclass of &%Destruction in which the
death of an &%Organism is caused by an &%Organism.  Note that in cases
of suicide the &%Organism would be the same in both cases.")

(=>
   (and
      (instance ?KILL Killing)
      (agent ?KILL ?AGENT)
      (patient ?KILL ?PATIENT))
   (and
      (instance ?AGENT Organism)
      (instance ?PATIENT Organism)))

(=>
   (and
      (instance ?KILL Killing)
      (patient ?KILL ?PATIENT))
   (and
      (holdsDuring (BeginFn (WhenFn ?KILL)) (attribute ?PATIENT Living))
      (holdsDuring (FutureFn (WhenFn ?KILL)) (attribute ?PATIENT Dead))))

(=>
   (and
      (instance ?KILL Killing)
      (patient ?KILL ?OBJ))
   (exists (?DEATH)
      (and
         (instance ?DEATH Death)
         (experiencer ?DEATH ?OBJ)
         (causes ?KILL ?DEATH))))

(subclass Poking IntentionalProcess)
(documentation Poking "The &%Class of &%Processes where the &%agent
pierces the surface of the &%Object with an &%instrument.")

(=>
   (and
      (instance ?POKE Poking)
      (agent ?POKE ?AGENT)
      (patient ?POKE ?OBJ)
      (instrument ?POKE ?INST))
   (holdsDuring (WhenFn ?POKE) (connects ?INST ?AGENT ?OBJ)))

(subclass Cutting Poking)
(documentation Cutting "The &%subclass of &%Poking &%Processes which
involve a sharp &%instrument.")

(subclass Attaching DualObjectProcess)
(disjoint Attaching Detaching)
(relatedInternalConcept Attaching Putting)
(documentation Attaching "A &%Process where one &%Object becomes attached
to another &%Object.  Note that this differs from &%Putting in that two
things which are attached may already be in the same location.")

(=>
   (and
      (instance ?ATTACH Attaching)
      (patient ?ATTACH ?OBJ1)
      (patient ?ATTACH ?OBJ2))
   (and
      (holdsDuring (BeginFn (WhenFn ?ATTACH)) (not (connected ?OBJ1 ?OBJ2)))
      (holdsDuring (EndFn (WhenFn ?ATTACH)) (connected ?OBJ1 ?OBJ2))))

(subclass Detaching DualObjectProcess)
(documentation Detaching "A &%Process where the &%agent detaches one thing
from something else.  Note that this is different from &%Removing in that
neither of the two things which are detached may be removed from the location
where it was attached.")

(=>
   (and
      (instance ?DETACH Detaching)
      (patient ?DETACH ?OBJ1)
      (patient ?DETACH ?OBJ2))
   (and
      (holdsDuring (BeginFn (WhenFn ?DETACH)) (connected ?OBJ1 ?OBJ2))
      (holdsDuring (EndFn (WhenFn ?DETACH)) (not (connected ?OBJ1 ?OBJ2)))))

(subclass Combining DualObjectProcess)
(documentation Combining "A &%Process where two or more &%Substances are
combined into a single &%Substance.")

(<=>
   (and
      (instance ?COMBINE Combining)
      (resource ?COMBINE ?OBJ1)
      (result ?COMBINE ?OBJ2))
   (and
      (holdsDuring (BeginFn (WhenFn ?COMBINE)) (not (piece ?OBJ1 ?OBJ2)))
      (holdsDuring (EndFn (WhenFn ?COMBINE)) (piece ?OBJ1 ?OBJ2))))

(subclass Separating DualObjectProcess)
(disjoint Separating Combining)
(documentation Separating "A &%Process where a &%Substance is separated
into (some of) its &%pieces.")

(subclass ChemicalProcess InternalChange)
(partition ChemicalProcess ChemicalSynthesis ChemicalDecomposition)
(documentation ChemicalProcess "A &%ChemicalProcess occurs whenever
chemical compounds (&%CompoundSubstances) are formed or decomposed.
For example, reactants disappear as chemical change occurs, and products
appear as chemical change occurs.  In a chemical change a chemical
reaction takes place.  Catalysts in a &%ChemicalProcess may speed up the
reaction, but aren't themselves produced or consumed.  Examples: rusting of
iron and the decomposition of water, induced by an electric current, to
gaseous hydrogen and gaseous oxygen.")

(=>
   (and
      (instance ?PROC ChemicalProcess)
      (or
         (resource ?PROC ?STUFF)
         (result ?PROC ?STUFF)))
   (instance ?STUFF PureSubstance))

(subclass ChemicalSynthesis ChemicalProcess)
(subclass ChemicalSynthesis Combining)
(documentation ChemicalSynthesis "The &%Class of &%ChemicalProcesses in
which a &%CompoundSubstance is formed from simpler reactants.")

(=>
   (and
      (resource ?PROC ?SUBSTANCE1)
      (result ?PROC ?SUBSTANCE2)
      (instance ?SUBSTANCE1 ElementalSubstance)
      (instance ?SUBSTANCE2 CompoundSubstance))
   (instance ?PROC ChemicalSynthesis))

(<=>
   (instance ?COMPOUND CompoundSubstance)
   (exists (?ELEMENT1 ?ELEMENT2 ?PROCESS)
      (and
         (instance ?ELEMENT1 ElementalSubstance)
         (instance ?ELEMENT2 ElementalSubstance)
         (not (equal ?ELEMENT1 ?ELEMENT2))
 	   (instance ?PROCESS ChemicalSynthesis)
         (resource ?PROCESS ?ELEMENT1)
         (resource ?PROCESS ?ELEMENT2)
         (result ?PROCESS ?COMPOUND))))

(subclass ChemicalDecomposition ChemicalProcess)
(subclass ChemicalDecomposition Separating)
(documentation ChemicalDecomposition "The &%Class of &%ChemicalProcesses
in which a &%CompoundSubstance breaks down into simpler products.")

(=>
   (and
      (resource ?PROC ?SUBSTANCE1)
      (result ?PROC ?SUBSTANCE2)
      (instance ?SUBSTANCE1 CompoundSubstance)
      (instance ?SUBSTANCE2 ElementalSubstance))
   (instance ?PROC ChemicalDecomposition))

(subclass Combustion ChemicalDecomposition)
(documentation Combustion "The &%Class of &%ChemicalProcesses in which an &%Object
reacts with oxygen and gives off heat.  This includes all &%Processes in which
something is burning.")

(=>
   (instance ?COMBUSTION Combustion)
   (exists (?HEAT ?LIGHT)
      (and
         (instance ?HEAT Heating)
         (instance ?LIGHT RadiatingLight)
         (subProcess ?HEAT ?COMBUSTION)
         (subProcess ?LIGHT ?COMBUSTION))))

(subclass InternalChange Process)
(documentation InternalChange "&%Processes which involve altering an internal
property of an &%Object, e.g. the shape of the &%Object, its coloring, its
structure, etc.  &%Processes that are not instances of this class include
changes that only affect the relationship to other objects, e.g. changes in
spatial or temporal location.")

(=>
   (and
      (instance ?CHANGE InternalChange)
      (patient ?CHANGE ?OBJ))
   (exists (?PROPERTY)
      (or
         (and
            (holdsDuring (BeginFn (WhenFn ?CHANGE)) (attribute ?OBJ ?PROPERTY))
            (holdsDuring (EndFn (WhenFn ?CHANGE)) (not (attribute ?OBJ ?PROPERTY))))
         (and
            (holdsDuring (BeginFn (WhenFn ?CHANGE)) (not (attribute ?OBJ ?PROPERTY)))
            (holdsDuring (EndFn (WhenFn ?CHANGE)) (attribute ?OBJ ?PROPERTY))))))

(subclass WeatherProcess InternalChange)
(documentation WeatherProcess "&%WeatherProcess is the broadest class of
processes that involve weather, including weather seasons (not to be confused
with instances of &%SeasonOfYear), weather systems, and short-term weather
events.")

(subclass Precipitation WeatherProcess)
(subclass Precipitation WaterMotion)
(subclass Precipitation Falling)
(documentation Precipitation "&%Precipitation is the process of
water molecules falling from the air to the ground, in either a
liquid or frozen state.")

(=>
	(instance ?PROCESS Precipitation)
	(exists (?STUFF)
		(and
			(instance ?STUFF Water)
			(patient ?PROCESS ?STUFF))))

(subclass SurfaceChange InternalChange)
(documentation SurfaceChange "&%Processes which involve altering
the properties that apply to the surface of an &%Object.")

(=>
   (and
      (instance ?ALT SurfaceChange)
      (patient ?ALT ?OBJ))
   (exists (?PART ?PROPERTY)
      (and
         (superficialPart ?PART ?OBJ)
         (or
            (and
               (holdsDuring (BeginFn (WhenFn ?ALT)) (attribute ?PART ?PROPERTY))
               (holdsDuring (EndFn (WhenFn ?ALT)) (not (attribute ?PART ?PROPERTY))))
            (and
               (holdsDuring (BeginFn (WhenFn ?ALT)) (not (attribute ?PART ?PROPERTY)))
               (holdsDuring (EndFn (WhenFn ?ALT)) (attribute ?PART ?PROPERTY)))))))

(subclass Coloring SurfaceChange)
(documentation Coloring "The &%subclass of &%SurfaceChange where a
&%ColorAttribute of the &%patient is altered.")

(=>
   (and
      (instance ?COLORING Coloring)
      (patient ?COLORING ?OBJ))
   (exists (?PROPERTY)
      (and
         (instance ?PROPERTY ColorAttribute)
         (or
            (and
               (holdsDuring (BeginFn (WhenFn ?ALT)) (attribute ?PART ?PROPERTY))
               (holdsDuring (EndFn (WhenFn ?ALT)) (not (attribute ?PART ?PROPERTY))))
            (and
               (holdsDuring (BeginFn (WhenFn ?ALT)) (not (attribute ?PART ?PROPERTY)))
               (holdsDuring (EndFn (WhenFn ?ALT)) (attribute ?PART ?PROPERTY)))))))

(subclass ShapeChange InternalChange)
(documentation ShapeChange "The &%Process of changing the shape of an &%Object.")

(=>
   (and
      (instance ?ALT ShapeChange)
      (patient ?ALT ?OBJ))
   (exists (?PROPERTY)
      (and
         (instance ?PROPERTY ShapeAttribute)
         (or
            (and
               (holdsDuring (BeginFn (WhenFn ?ALT)) (attribute ?OBJ ?PROPERTY))
               (holdsDuring (EndFn (WhenFn ?ALT)) (not (attribute ?OBJ ?PROPERTY))))
            (and
               (holdsDuring (BeginFn (WhenFn ?ALT)) (not (attribute ?OBJ ?PROPERTY)))
               (holdsDuring (EndFn (WhenFn ?ALT)) (attribute ?OBJ ?PROPERTY)))))))

(subclass ContentDevelopment IntentionalProcess)
(documentation ContentDevelopment "A &%subclass of &%IntentionalProcess in
which content is modified, its form is altered or it is created anew.")

(=>
   (instance ?DEVELOP ContentDevelopment)
   (exists (?OBJ)
      (and
         (instance ?OBJ ContentBearingObject)
         (patient ?DEVELOP ?OBJ))))

(subclass Reading ContentDevelopment)
(relatedInternalConcept Reading Interpreting)
(documentation Reading "A &%subclass of &%ContentDevelopment in which
content is converted from a written form into a spoken representation.
Note that the class &%Interpreting should be used in cases where a
&%Text is read silently.")

(=>
   (instance ?READ Reading)
   (exists (?TEXT ?PROP)
      (and
         (instance ?TEXT Text)
         (containsInformation ?TEXT ?PROP)
         (realization ?READ ?PROP))))

(subclass Writing ContentDevelopment)
(documentation Writing "A &%subclass of &%ContentDevelopment in which
content is converted from one form (e.g. uttered, written or represented
mentally) into a written form.  Note that this class covers both
transcription and original creation of written &%Texts.")

(subclass Encoding Writing)
(:hasRestrictedVal Encoding creates EncodedDocument)
(documentation Encoding "Converting a document or message into a formal
language or into a code that can be understood only by a relatively small
body of &%Agents.  Generally speaking, this hinders wide dissemination of
the content in the original document or message.    Encoding must use some procedure other than simple writing in a widely-used language, it must have some non-obvious features that make interpretation more difficult without knowledge of the encoding procedure.  However, intentional hiding of the information is not Encoding, but Encryption.")

(subclass Decoding Writing)
(disjoint Decoding Encoding)
(documentation Decoding "Converting a document or message that has previously been encoded (see &%Encoding) into a &%Language that can be understood by a relatively large number of speakers.")

(=>
   (and
      (instance ?DECODE Decoding)
      (patient ?DECODE ?DOC1))
   (exists (?ENCODE ?DOC2 ?TIME)
      (and
         (containsInformation ?DOC2 ?PROP)
         (containsInformation ?DOC1 ?PROP)
 	   (temporalPart ?TIME (PastFn (WhenFn ?DECODE)))
         (holdsDuring ?TIME
            (and
               (instance ?ENCODE Encoding)
               (patient ?ENCODE ?DOC2))))))

(subclass Translating ContentDevelopment)
(subclass Translating DualObjectProcess)
(documentation Translating "Converting content from one &%Language into another.")

(=>
   (and
      (instance ?TRANSLATE Translating)
      (resource ?TRANSLATE ?EXPRESSION1)
      (result ?TRANSLATE ?EXPRESSION2))
   (exists (?LANGUAGE1 ?LANGUAGE2 ?ENTITY)
      (and
         (representsInLanguage ?EXPRESSION1 ?ENTITY ?LANGUAGE1)
         (representsInLanguage ?EXPRESSION2 ?ENTITY ?LANGUAGE2)
         (not (equal ?LANGUAGE1 ?LANGUAGE2)))))

(subclass Wetting Putting)
(documentation Wetting "The &%Class of &%Processes where a &%Liquid is
added to an &%Object.")

(=>
   (and
      (instance ?WET Wetting)
      (patient ?WET ?OBJ))
   (holdsDuring (EndFn (WhenFn ?WET))
      (or
         (attribute ?OBJ Wet)
         (attribute ?OBJ Damp))))

(=>
   (instance ?WET Wetting)
   (exists (?OBJ)
      (and
         (attribute ?OBJ Liquid)
         (patient ?WET ?OBJ))))

(subclass Drying Removing)
(documentation Drying "The &%Class of &%Processes where a &%Liquid is removed
from an &%Object.")

(=>
   (and
      (instance ?DRY Drying)
      (patient ?DRY ?OBJ))
   (holdsDuring (EndFn (WhenFn ?DRY))
      (attribute ?OBJ Dry)))

(subclass Creation InternalChange)
(relatedInternalConcept Creation Destruction)
(documentation Creation "The &%subclass of &%Process in which
something is created.  Note that the thing created is specified
with the &%result &%CaseRole.")

(=>
   (instance ?ACTION Creation)
   (exists (?RESULT)
      (result ?ACTION ?RESULT)))

(<=>
   (instance ?PROCESS Creation)
   (exists (?PATIENT)
      (and
         (patient ?PROCESS ?PATIENT)
         (time ?PATIENT (EndFn(WhenFn ?PROCESS)))
         (not
            (time ?PATIENT (BeginFn (WhenFn ?PROCESS)))))))

(subclass Making Creation)
(subclass Making IntentionalProcess)
(documentation Making "The &%subclass of &%Creation in which an individual
&%Artifact or a type of &%Artifact is made.")

(subclass Constructing Making)
(documentation Constructing "The &%subclass of &%Making in which a
&%StationaryArtifact is built.")

(<=>
   (exists (?BUILD)
      (and
         (instance ?BUILD Constructing)
         (result ?BUILD ?ARTIFACT)))
   (instance ?ARTIFACT StationaryArtifact))

(subclass Manufacture Making)
(documentation Manufacture "The &%Making of &%Artifacts on a mass
scale.")

(subclass Publication Manufacture)
(subclass Publication ContentDevelopment)
(documentation Publication "The &%Manufacture of &%Texts.  Note that
there is no implication that the &%Texts are distributed.  Such
distribution, when it occurs, is an instance of &%Dissemination.")

(=>
   (and
      (instance ?PUB Publication)
      (patient ?PUB ?TEXT))
   (subclass ?TEXT Text))

(subclass Cooking Making)
(documentation Cooking "The &%Making of an &%instance of &%Food.")

(=>
   (instance ?COOK Cooking)
   (exists (?FOOD)
      (and
         (instance ?FOOD Food)
         (result ?COOK ?FOOD))))

(subclass Pursuing IntentionalProcess)
(documentation Pursuing "The class of &%IntentionalProcesses where something is
sought.  Some examples would be hunting, shopping, trawling, and stalking.")

(=>
   (instance ?PURSUE Pursuing)
   (exists (?OBJ)
      (and
         (instance ?OBJ Object)
         (patient ?PURSUE ?OBJ))))

(=>
   (and
      (instance ?PURSUE Pursuing)
      (agent ?PURSUE ?AGENT)
      (patient ?PURSUE ?OBJ))
   (holdsDuring ?PURSUE (wants ?AGENT ?OBJ)))

(=>
   (and
      (instance ?PURSUE Pursuing)
      (agent ?PURSUE ?AGENT)
      (patient ?PURSUE ?OBJ))
   (holdsDuring ?PURSUE (not (possesses ?AGENT ?OBJ))))

(subclass Investigating IntentionalPsychologicalProcess)
(documentation Investigating "The class of &%IntentionalPsychologicalProcesses
where the &%agent attempts to obtaina information (i.e. a &%Proposition denoted
by a &%Formula).")

(=>
   (and
      (instance ?INVESTIGATE Investigating)
      (patient ?INVESTIGATE ?PROP))
   (instance ?PROP Formula))

(=>
   (and
      (instance ?INVESTIGATE Investigating)
      (agent ?INVESTIGATE ?AGENT)
      (patient ?INVESTIGATE ?PROP))
   (holdsDuring (WhenFn ?INVESTIGATE) (not (knows ?AGENT ?PROP))))

(subclass DiagnosticProcess Investigating)
(documentation DiagnosticProcess "A &%Process that is carried out for
the purpose of determining the nature of a &%DiseaseOrSyndrome.")

(=>
   (and
      (instance ?PROC DiagnosticProcess)
      (agent ?PROC ?AGENT))
   (exists (?CAUSE)
      (hasPurposeForAgent ?PROC (knows ?AGENT (causes ?CAUSE ?PROC)) ?AGENT)))

(subclass SocialInteraction IntentionalProcess)
(documentation SocialInteraction "The &%subclass of
&%IntentionalProcess that involves interactions between
&%CognitiveAgents.")

(=>
   (instance ?INTERACTION SocialInteraction)
   (exists (?AGENT1 ?AGENT2)
      (and
         (agent ?INTERACTION ?AGENT1)
         (agent ?INTERACTION ?AGENT2)
         (not
            (equal ?AGENT1 ?AGENT2)))))

(subclass Pretending SocialInteraction)
(documentation Pretending "Any &%SocialInteraction where a
&%CognitiveAgent or &%Group of &%CognitiveAgents attempts to make
another &%CognitiveAgent or &%Group of &%CognitiveAgents believe
something that is false.  This covers deceit, affectation,
impersonation, and entertainment productions, to give just a few
examples.")

(=>
   (instance ?PRETEND Pretending)
   (exists (?PERSON ?PROP)
      (and
         (hasPurpose ?PRETEND (believes ?PERSON ?PROP))
         (true ?PROP True))))

(subclass Communicating SocialInteraction)
(relatedInternalConcept Communicating ContentDevelopment)
(documentation Communicating "A &%SocialInteraction that involves
the transfer of information between two or more &%CognitiveAgents.
Note that &%Communicating is closely related to, but essentially
different from, &%ContentDevelopment.  The latter involves the creation
or modification of a &%ContentBearingObject, while &%Communicating is
the transfer of information for the purpose of conveying a message.  The product of the process Communicating is a Communication, which is a Proposition.  See realization -- Communicating is the realization of a Proposition.")

(=>
   (instance ?COMMUNICATE Communicating)
   (exists (?PHYS ?ENTITY ?AGENT1 ?AGENT2)
      (and
         (refers ?PHYS ?ENTITY)
         (patient ?COMMUNICATE ?PHYS)
         (instance ?AGENT1 CognitiveAgent)
         (agent ?COMMUNICATE ?AGENT1)
         (instance ?AGENT2 CognitiveAgent)
         (destination ?COMMUNICATE ?AGENT2))))

(subclass Disseminating Communicating)
(documentation Disseminating "Any &%Communicating that involves a
single &%agent and many &%destinations.  This covers the release
of a published book, broadcasting, a theatrical performance, giving
orders to assembled troops, delivering a public lecture, etc.")

(=>
   (instance ?DISSEMINATE Disseminating)
   (exists (?AGENT1 ?AGENT2)
      (and
         (destination ?DISSEMINATE ?AGENT1)
         (instance ?AGENT1 CognitiveAgent)
         (destination ?DISSEMINATE ?AGENT2)
         (instance ?AGENT2 CognitiveAgent)
         (not (equal ?AGENT1 ?AGENT2)))))

(subclass Demonstrating Disseminating)
(documentation Demonstrating "Exhibiting something or a range of things
before the public in a particular location.  This would cover software
demos, theatrical plays, lectures, dance and music recitals, museum
exhibitions, etc.")

(=>
   (instance ?DEMO Demonstrating)
   (exists (?PERSON)
      (attends ?DEMO ?PERSON)))

(subrelation attends experiencer)
(domain attends 1 Demonstrating)
(domain attends 2 Human)
(documentation attends "(&%attends ?DEMO ?PERSON) means that ?PERSON attends,
i.e. is a member of the audience, of the performance event ?DEMO.")

(subclass Advertising Disseminating)
(documentation Advertising "A &%Disseminating whose purpose is to
promote the sale of an &%Object represented in a &%Text or &%Icon
(the advertisement).")

(=>
   (instance ?ADVERT Advertising)
   (exists (?OBJ)
      (and
         (refers ?ADVERT ?OBJ)
         (hasPurpose ?ADVERT (exists (?SALE) (and (instance ?SALE Selling) (patient ?SALE ?OBJ)))))))

(subclass Expressing Communicating)
(partition Expressing Gesture ExpressingInLanguage)
(disjointDecomposition Expressing ExpressingApproval ExpressingDisapproval)
(documentation Expressing "Instances of this &%Class express a state of the &%agent.  For example, Jane thanked Barbara for the present she had given her.  The thanking in this case expresses the gratitude of Jane towards Barbara.  Note that &%Expressing, unlike the other speech act types, is not a subclass of &%LinguisticCommunication.  This is because emotions, for example, can be expressed without language, e.g. by smiling.")

(=>
   (and
      (instance ?EXPRESS Expressing)
      (agent ?EXPRESS ?AGENT))
   (exists (?STATE)
      (and
         (instance ?STATE StateOfMind)
         (attribute ?AGENT ?STATE)
         (represents ?EXPRESS ?STATE))))

(subclass Gesture Expressing)
(subclass Gesture BodyMotion)
(documentation Gesture "Any &%BodyMotion, e.g. a hand wave, a nod of the
head, a smile, which expresses a &%StateOfMind.")

(=>
   (and
      (instance ?GESTURE Gesture)
      (agent ?GESTURE ?AGENT))
   (exists (?STATE)
      (and
         (instance ?STATE StateOfMind)
         (attribute ?AGENT ?STATE)
         (represents ?GESTURE ?STATE))))

(subclass ExpressingInLanguage Expressing)
(subclass ExpressingInLanguage LinguisticCommunication)
(documentation ExpressingInLanguage "Any instance of &%Expressing that is also an instance of &%LinguisticCommunication, e.g. thanking someone, expressing condolence, expressing disapproval with an utterance rather than a &%Gesture, etc.")

(subclass LinguisticCommunication Communicating)
(partition LinguisticCommunication Stating Supposing Directing Committing ExpressingInLanguage Declaring)
(documentation LinguisticCommunication "A &%Communicating that involves
the transfer of information via a &%LinguisticExpression.")

(=>
   (instance ?COMMUNICATE LinguisticCommunication)
   (exists (?OBJ)
      (and
         (represents ?COMMUNICATE ?OBJ)
         (instance ?OBJ LinguisticExpression)
         (patient ?COMMUNICATE ?OBJ))))

(subclass Stating LinguisticCommunication)
(documentation Stating "Instances of this &%Class commit the &%agent to some truth.
For example, John claimed that the moon is made of green cheese.")

(=>
   (and
      (instance ?STATE Stating)
      (agent ?STATE ?AGENT)
      (patient ?STATE ?FORMULA)
      (instance ?FORMULA Formula))
   (holdsDuring (WhenFn ?STATE) (believes ?AGENT ?FORMULA)))

(subclass Supposing LinguisticCommunication)
(documentation Supposing "Instances of this &%Class suppose, for the sake of
argument, that a proposition is true.  For example, John considered what he
would do if he won the lottery.")

(subclass Directing LinguisticCommunication)
(documentation Directing "Instances of this &%Class urge some further action
among the receivers.  A &%Directing can be an &%Ordering, a &%Requesting or
a &%Questioning.")

(subclass Ordering Directing)
(documentation Ordering "A &%Directing in which the receiver is
commanded to realize the content of a &%ContentBearingObject.  Orders
are injunctions, the disobedience of which involves sanctions, or
which express an obligation upon the part of the orderee.")

(=>
   (and
      (instance ?ORDER Ordering)
      (patient ?ORDER ?FORMULA))
   (modalAttribute ?FORMULA Obligatory))

(subclass Requesting Directing)
(documentation Requesting "A request expresses a desire that some future
action be performed.  For example, the 5th Battalion requested air support
from the 3rd Bomber Group.  Note that this class covers proposals,
recommendations, suggestions, etc.")

(=>
   (and
      (instance ?REQUEST Requesting)
      (agent ?REQUEST ?AGENT)
      (patient ?REQUEST ?FORMULA)
      (instance ?FORMULA Formula))
   (desires ?AGENT ?FORMULA))

(subclass Questioning Directing)
(documentation Questioning "A request for information.  For example, John asked
Bill if the President had said anything about taxes in his State of the Union address.")

(=>
   (and
      (instance ?QUESTION Questioning)
      (agent ?QUESTION ?AGENT)
      (patient ?QUESTION ?FORMULA)
      (instance ?FORMULA Formula))
   (holdsDuring (WhenFn ?QUESTION) (not (knows ?AGENT ?FORMULA))))

(subclass Committing LinguisticCommunication)
(documentation Committing "Instances of this &%Class commit the &%agent to some
future course.  For example, Bob promised Susan that he would be home by 11pm.")

(=>
   (and
      (instance ?COMMIT Committing)
      (patient ?COMMIT ?FORMULA)
      (instance ?FORMULA Formula))
   (modalAttribute ?FORMULA Promise))

(subclass Offering Committing)
(documentation Offering "The subclass of &%Committing in which a &%CognitiveAgent offers something &%Physical to another agent. Offerings may be unconditional (in which case they are a promise to effect a &%UnilateralGiving) or conditional (in which case they are a promise to effect a &%Transaction of some sort).")

(subclass Declaring LinguisticCommunication)
(documentation Declaring "The &%Class of &%LinguisticCommunications that
effect an institutional alteration when performed by competent authority.
Some examples are nominating, marrying, and excommunicating.")

(=>
   (and
      (instance ?DECLARE Declaring)
      (agent ?DECLARE ?AGENT1))
   (exists (?PROC ?AGENT2)
      (or
         (confersRight ?PROC ?DECLARE ?AGENT2)
         (confersObligation ?PROC ?DECLARE ?AGENT2))))

(subclass Wedding Declaring)
(documentation Wedding "Any &%Declaring that leads to one person being
the &%spouse of another.")

(=>
   (instance ?WED Wedding)
   (exists (?PERSON1 ?PERSON2)
      (result ?WED (spouse ?PERSON1 ?PERSON2))))

(subclass Naming Declaring)
(documentation Naming "The &%Process of assigning a name to someone or something.")

(=>
   (and
      (instance ?PROCESS Naming)
      (patient ?PROCESS ?THING)
      (destination ?PROCESS ?NAME))
   (holdsDuring (FutureFn (WhenFn ?PROCESS)) (names ?NAME ?THING)))

(subclass Cooperation SocialInteraction)
(documentation Cooperation "The &%subclass of &%SocialInteraction where
the participants involved work together for the achievement of a common
goal.")

(=>
   (instance ?COOPERATE Cooperation)
   (exists (?PURP)
      (forall (?AGENT)
         (=>
            (agent ?COOPERATE ?AGENT)
            (hasPurposeForAgent ?COOPERATE ?PURP ?AGENT)))))

(subclass Meeting SocialInteraction)
(documentation Meeting "The coming together of two or more
&%CognitiveAgents for the purpose of &%Communicating.  This covers informal
meetings, e.g. visits with family members, and formal meetings, e.g. a board
of directors meeting.")

(=>
   (and
      (instance ?MEET Meeting)
      (agent ?MEET ?AGENT1)
      (agent ?MEET ?AGENT2))
   (holdsDuring (WhenFn ?MEET) (orientation ?AGENT1 ?AGENT2 Near)))

(=>
   (instance ?MEET Meeting)
   (exists (?AGENT1 ?AGENT2)
      (and
         (agent ?MEET ?AGENT1)
         (agent ?MEET ?AGENT2)
         (hasPurpose ?MEET (exists (?COMM) (and (instance ?COMM Communicating) (agent ?COMM ?AGENT1) (agent ?COMM ?AGENT2)))))))

(subclass Contest SocialInteraction)
(documentation Contest "A &%SocialInteraction where the &%agent and
&%patient are &%CognitiveAgents who are trying to defeat one another.
Note that this concept is often applied in a metaphorical sense in natural
language, when we speak, e.g., of the struggle of plants for space or
sunlight, or of bacteria for food resources in some environment.")

(=>
   (instance ?CONTEST Contest)
   (exists (?AGENT1 ?AGENT2 ?PURP1 ?PURP2)
      (and
         (agent ?CONTEST ?AGENT1)
         (agent ?CONTEST ?AGENT2)
         (hasPurposeForAgent ?CONTEST ?PURP1 ?AGENT1)
         (hasPurposeForAgent ?CONTEST ?PURP2 ?AGENT2)
         (not
            (equal ?AGENT1 ?AGENT2))
         (not
            (equal ?PURP1 ?PURP2)))))

(subclass ViolentContest Contest)
(documentation ViolentContest "A &%Contest where one participant attempts to
physically injure another participant.")

(subclass War ViolentContest)
(documentation War "A military confrontation between two or more
&%GeopoliticalAreas or &%Organizations whose members are &%GeopoliticalAreas.
As the corresponding axiom specifies, a &%War is made up of &%Battles.")

(=>
   (instance ?WAR War)
   (exists (?BATTLE)
      (and
         (instance ?BATTLE Battle)
         (subProcess ?BATTLE ?WAR))))

(=>
   (and
      (instance ?WAR War)
      (agent ?WAR ?AGENT))
   (or
      (instance ?AGENT GeopoliticalArea)
      (and
         (instance ?AGENT Organization)
         (forall (?MEMBER)
            (=>
               (member ?MEMBER ?AGENT)
               (instance ?MEMBER GeopoliticalArea))))))

(subclass Battle ViolentContest)
(documentation Battle "A &%ViolentContest between two or more military
units within the context of a war.  Note that this does not cover the
metaphorical sense of 'battle', which simply means a struggle of some
sort.  This sense should be represented with the more general concept of
&%Contest.")

(=>
   (instance ?BATTLE Battle)
   (exists (?WAR)
      (and
         (instance ?WAR War)
         (subProcess ?BATTLE ?WAR))))

(=>
   (instance ?BATTLE Battle)
   (exists (?ATTACK)
      (and
         (instance ?ATTACK ViolentContest)
         (subProcess ?ATTACK ?BATTLE))))

(subclass Game Contest)
(subclass Game RecreationOrExercise)
(documentation Game "A &%Contest whose purpose is the
enjoyment/stimulation of the participants or spectators of the &%Game.")

(subclass Sport Game)
(documentation Sport "A &%Game which requires some degree of physical
exercion from the participants of the game.")

(subclass LegalAction Contest)
(documentation LegalAction "Any &%Process where a &%CognitiveAgent seeks
to obtain something through a court of law.")

(subclass Maneuver IntentionalProcess)
(documentation Maneuver "An intentional move or play within a &%Contest.
In many cases, a &%Maneuver is a realization of part of a strategy for
winning the &%Contest, but it also may be just an arbitrary or semi-arbitrary
division of the overarching &%Contest, e.g. innings in a baseball game.")

(=>
   (instance ?MOVE Maneuver)
   (exists (?CONTEST)
      (and
         (instance ?CONTEST Contest)
         (subProcess ?MOVE ?CONTEST))))

(subclass Attack Maneuver)
(documentation Attack "A &%Maneuver in a &%ViolentContest where the
&%agent attempts to inflict damage on the &%patient.")

(=>
   (instance ?ATTACK Attack)
   (exists (?CONTEST)
      (and
         (instance ?CONTEST ViolentContest)
         (subProcess ?ATTACK ?CONTEST))))

(=>
   (and
      (instance ?ATTACK Attack)
      (agent ?ATTACK ?AGENT)
      (patient ?ATTACK ?OBJ))
   (hasPurposeForAgent ?ATTACK (exists (?DAMAGE)
                                  (and
                                     (instance ?DAMAGE Damaging)
                                     (patient ?DAMAGE ?OBJ))) ?AGENT))

(subclass DefensiveManeuver Maneuver)
(documentation DefensiveManeuver "A &%Maneuver in a &%ViolentContest
where the &%agent attempts to avoid being damaged.")

(=>
   (instance ?DEFENSE DefensiveManeuver)
   (exists (?CONTEST)
      (and
         (instance ?CONTEST ViolentContest)
         (subProcess ?DEFENSE ?CONTEST))))

(=>
   (and
      (instance ?DEFENSE DefensiveManeuver)
      (agent ?DEFENSE ?AGENT))
   (hasPurposeForAgent ?DEFENSE (not (exists (?DAMAGE)
                                        (and
                                           (instance ?DAMAGE Damaging)
                                           (patient ?DAMAGE ?AGENT)))) ?AGENT))

(=>
   (and
      (instance ?MANEUVER Maneuver)
      (instance ?CONTEST ViolentContest)
      (subProcess ?MANEUVER ?CONTEST))
   (or
      (instance ?MANEUVER Attack)
      (instance ?MANEUVER DefensiveManeuver)))

(subclass Perception PsychologicalProcess)
(documentation Perception "Sensing some aspect of the material world.
Note that the &%agent of this sensing is assumed to be an &%Animal.")

(=>
   (and
      (instance ?PERCEPT Perception)
      (agent ?PERCEPT ?AGENT))
   (instance ?AGENT Animal))

(=>
   (instance ?AGENT SentientAgent)
   (capability Perception experiencer ?AGENT))

(subclass Seeing Perception)
(documentation Seeing "The &%subclass of &%Perception in which the
sensing is done by an ocular &%Organ.")

(=>
   (and
      (instance ?SEE Seeing)
      (agent ?SEE ?AGENT)
      (patient ?SEE ?OBJ))
   (and
      (attribute ?OBJ Illuminated)
      (exists (?PROP)
         (and
            (instance ?PROP ColorAttribute)
            (knows ?AGENT (attribute ?OBJ ?PROP))))))

(subclass Looking Seeing)
(subclass Looking IntentionalProcess)
(documentation Looking "Any instance of &%Seeing which is intentional.")

(subclass Smelling Perception)
(documentation Smelling "The &%subclass of &%Perception in which the
sensing is done by an olefactory &%Organ.")

(=>
   (and
      (instance ?SMELL Smelling)
      (patient ?SMELL ?OBJ))
   (exists (?ATTR)
      (and
         (instance ?ATTR OlfactoryAttribute)
         (attribute ?OBJ ?ATTR))))

(subclass Tasting Perception)
(documentation Tasting "The &%subclass of &%Perception in which the
sensing is done by of an &%Organ which can discriminate various tastes.")

(=>
   (and
      (instance ?TASTE Tasting)
      (patient ?TASTE ?OBJ))
   (exists (?ATTR)
      (and
         (instance ?ATTR TasteAttribute)
         (attribute ?OBJ ?ATTR))))

(subclass Hearing Perception)
(documentation Hearing "The &%subclass of &%Perception in which the
sensing is done by an auditory &%Organ.")

(=>
   (and
      (instance ?HEAR Hearing)
      (patient ?HEAR ?OBJ))
   (exists (?ATTR)
      (and
         (instance ?ATTR SoundAttribute)
         (attribute ?OBJ ?ATTR))))

(subclass Listening Hearing)
(subclass Listening IntentionalProcess)
(documentation Listening "Any instance of &%Hearing which is intentional.")

(subclass TactilePerception Perception)
(documentation TactilePerception "The &%subclass of &%Perception in which
the sensing is done by &%Touching.  Note that &%Touching need not involve
&%TactilePerception.  For example, a person who has lost all sensation in
both of his legs would have no &%TactilePerception of anything his legs
were &%Touching.")

(=>
   (instance ?TACTILE TactilePerception)
   (exists (?TOUCH)
      (and
         (instance ?TOUCH Touching)
         (subProcess ?TOUCH ?TACTILE))))

(subclass Radiating Motion)
(documentation Radiating "Processes in which some form of electromagnetic
radiation, e.g. radio waves, light waves, electrical energy, etc., is given
off or absorbed by something else.")

(subclass RadiatingLight Radiating)
(documentation RadiatingLight "The &%subclass of &%Radiating in which
light is given off or absorbed.  Some examples include blinking, flashing,
and glittering.")

(<=>
   (exists (?EMIT)
      (and
         (instance ?EMIT RadiatingLight)
         (patient ?EMIT ?REGION)
         (instance ?REGION Region)))
   (attribute ?REGION Illuminated))

(subclass RadiatingSound Radiating)
(documentation RadiatingSound "The &%subclass of &%Radiating in which
sound waves are given off or absorbed.  Some examples include creaking,
roaring, and whistling.")

(=>
   (and
      (instance ?EMIT RadiatingSound)
      (agent ?EMIT ?SOUND))
   (exists (?ATTR)
      (and
         (instance ?ATTR SoundAttribute)
         (attribute ?SOUND ?ATTR))))

(subclass Music RadiatingSound)
(partition Music MonophonicMusic PolyphonicMusic)
(documentation Music "The &%subclass of &%RadiatingSound where the
sound is intended to be melodic and is produced deliberately.")

(subclass RadiatingElectromagnetic Radiating)
(documentation RadiatingElectromagnetic "&%RadiatingElectromagnetic
is the subclass of &%Radiating processes in which electromagnetic
radiation is transmitted or absorbed.")

(subclass RadiatingNuclear Radiating)
(documentation RadiatingNuclear "Releasing atomic energy, i.e. energy from
a nuclear reaction.")

(subclass StateChange InternalChange)
(documentation StateChange "Any &%Process where the &%PhysicalState
of &%part of the &%patient of the &%Process changes.")

(=>
   (and
      (instance ?PROCESS StateChange)
      (patient ?PROCESS ?OBJ))
   (exists (?PART ?STATE1 ?STATE2)
      (and
         (part ?PART ?OBJ)
         (instance ?STATE1 PhysicalState)
         (instance ?STATE2 PhysicalState)
         (not (equal ?STATE1 ?STATE2))
         (holdsDuring (BeginFn (WhenFn ?PROCESS)) (attribute ?PART ?STATE1))
         (holdsDuring (EndFn (WhenFn ?PROCESS)) (attribute ?PART ?STATE2)))))

(subclass Melting StateChange)
(documentation Melting "The &%Class of &%Processes where an &%Object is
heated and converted from a &%Solid to a &%Liquid.")

(=>
   (instance ?MELT Melting)
   (exists (?HEAT)
      (and
         (instance ?HEAT Heating)
         (subProcess ?HEAT ?MELT))))

(=>
   (and
      (instance ?MELT Melting)
      (patient ?MELT ?OBJ))
   (exists (?PART)
      (and
         (part ?PART ?OBJ)
         (holdsDuring (BeginFn (WhenFn ?MELT)) (attribute ?PART Solid))
         (holdsDuring (EndFn (WhenFn ?MELT)) (attribute ?PART Liquid)))))

(subclass Boiling StateChange)
(documentation Boiling "The &%Class of &%Processes where an &%Object is
heated and converted from a &%Liquid to a &%Gas.")

(=>
   (instance ?BOIL Boiling)
   (exists (?HEAT)
      (and
         (instance ?HEAT Heating)
         (subProcess ?HEAT ?BOIL))))

(=>
   (and
      (instance ?BOIL Boiling)
      (patient ?BOIL ?OBJ))
   (exists (?PART)
      (and
         (part ?PART ?OBJ)
         (holdsDuring (BeginFn (WhenFn ?BOIL)) (attribute ?PART Liquid))
         (holdsDuring (EndFn (WhenFn ?BOIL)) (attribute ?PART Gas)))))

(subclass Condensing StateChange)
(documentation Condensing "The &%Class of &%Processes where an &%Object is
cooled and converted from a &%Gas to a &%Liquid.")

(=>
   (instance ?COND Condensing)
   (exists (?COOL)
      (and
         (instance ?COOL Cooling)
         (subProcess ?COOL ?COND))))

(=>
   (and
      (instance ?COND Condensing)
      (patient ?COND ?OBJ))
   (exists (?PART)
      (and
         (part ?PART ?OBJ)
         (holdsDuring (BeginFn (WhenFn ?COND)) (attribute ?PART Gas))
         (holdsDuring (EndFn (WhenFn ?COND)) (attribute ?PART Liquid)))))

(subclass Freezing StateChange)
(documentation Freezing "The &%Class of &%Processes where an &%Object is
cooled and converted from a &%Liquid to a &%Solid.")

(=>
   (instance ?FREEZE Freezing)
   (exists (?COOL)
      (and
         (instance ?COOL Cooling)
         (subProcess ?COOL ?FREEZE))))

(=>
   (and
      (instance ?FREEZE Freezing)
      (patient ?FREEZE ?OBJ))
   (exists (?PART)
      (and
         (part ?PART ?OBJ)
         (holdsDuring (BeginFn (WhenFn ?FREEZE)) (attribute ?PART Liquid))
         (holdsDuring (EndFn (WhenFn ?FREEZE)) (attribute ?PART Solid)))))



;; END FILE

;; <module>OBJECTS</module>
;; BEGIN FILE


;;;;;;;;;;;;;;;;;;;
;;    OBJECTS    ;;
;;;;;;;;;;;;;;;;;;;

;; INCLUDES 'MEREOTOPOLOGY'
;; INCLUDES 'PROCESSES'
;; INCLUDES 'QUALITIES'

(subclass AstronomicalBody Region)
(disjoint AstronomicalBody GeographicArea)
(documentation AstronomicalBody "The &%Class of all astronomical
objects of significant size.  It includes &%SelfConnectedObjects
like planets, stars, and asteroids, as well as &%Collections like
nebulae, galaxies, and constellations.  Note that the planet Earth
is an &%AstronomicalBody, but every &%Region of Earth is a
&%GeographicArea.")

(subclass GeographicArea Region)
(partition GeographicArea WaterArea LandArea)
(documentation GeographicArea "A geographic location, generally having
definite boundaries.  Note that this differs from its immediate superclass
&%Region in that a &%GeographicArea is a three-dimensional &%Region of the
earth.  Accordingly, all astronomical objects other than earth and all
one-dimensional and two-dimensional &%Regions are not classed under
&%GeographicArea.")

(instance geographicSubregion BinaryPredicate)
(instance geographicSubregion TransitiveRelation)
(instance geographicSubregion AsymmetricRelation)
(subrelation geographicSubregion properPart)
(subrelation geographicSubregion located)
(domain geographicSubregion 1 GeographicArea)
(domain geographicSubregion 2 GeographicArea)
(documentation geographicSubregion "(&%geographicSubregion ?PART ?WHOLE)
means that the &%GeographicArea ?PART is part of the &%GeographicArea
?WHOLE.")

(subclass GeopoliticalArea GeographicArea)
(subclass GeopoliticalArea Agent)
(documentation GeopoliticalArea "Any &%GeographicArea which is associated
with some sort of political structure.  This class includes &%Lands,
&%Cities, districts of cities, counties, etc.  Note that the identity
of a &%GeopoliticalArea may remain constant after a change in borders.")

(=>
   (and
      (instance ?AREA GeopoliticalArea)
	(leader (GovernmentFn ?AREA) ?PERSON))
   (leader ?AREA ?PERSON))

(=>
   (and
      (instance ?AREA GeopoliticalArea)
	(leader ?AREA ?PERSON))
(leader (GovernmentFn ?AREA) ?PERSON))

(=>
	(and
		(instance ?EVENT Election)
		(agent ?EVENT ?AGENT)
		(instance ?AGENT GeopoliticalArea))
	(instance ?EVENT PoliticalProcess))

(=>
	(and
		(instance ?EVENT Election)
		(agent ?EVENT ?AGENT)
		(instance ?AREA GeopoliticalArea)
		(instance ?AGENT (GovernmentFn ?AREA)))
	(instance ?EVENT PoliticalProcess))

(instance geopoliticalSubdivision AsymmetricRelation)
(instance geopoliticalSubdivision TransitiveRelation)
(subrelation geopoliticalSubdivision geographicSubregion)
(domain geopoliticalSubdivision 1 GeopoliticalArea)
(domain geopoliticalSubdivision 2 GeopoliticalArea)
(documentation geopoliticalSubdivision "(&%geopoliticalSubdivision
?AREA1 ?AREA2) means that ?AREA1 is any geopolitical part of ?AREA2;
that is, ?AREA1 is an integral &%geographicSubregion of ?AREA2 (not a
&%DependencyOrSpecialSovereigntyArea), having its own associated
&%GovernmentOrganization which is subordinated to or constrained by
the government of ?AREA2.  Cf. &%dependentGeopoliticalArea.")

(=>
	(geopoliticalSubdivision ?SUB ?AREA)
	(not (instance ?SUB Nation)))

(subclass WaterArea GeographicArea)
(documentation WaterArea "A body which is made up predominantly of water,
e.g. rivers, lakes, oceans, etc.")

(=>
   (instance ?AREA WaterArea)
   (exists (?BED ?HOLE ?WATER)
      (and
         (equal (PrincipalHostFn ?HOLE) ?BED)
         (instance ?WATER Water)
         (properlyFills ?WATER ?HOLE)
         (equal (MereologicalSumFn ?BED ?WATER) ?AREA))))

(subclass SaltWaterArea WaterArea)
(disjoint SaltWaterArea FreshWaterArea)
(documentation SaltWaterArea "A &%WaterArea whose &%Water is saline, e.g.
oceans and seas.")

(subclass FreshWaterArea WaterArea)
(documentation FreshWaterArea "A &%WaterArea whose &%Water is not saline,
e.g. most rivers and lakes.")

(subclass StreamWaterArea WaterArea)
(disjoint StreamWaterArea StaticWaterArea)
(documentation StreamWaterArea "A relatively narrow &%WaterArea where the
water flows constantly and in the same direction, e.g. a river, a stream,
etc.")

(subclass StaticWaterArea WaterArea)
(documentation StaticWaterArea "A &%WaterArea in which water does not flow
constantly or in the same direction, e.g. most lakes and ponds.")

(subclass LandArea GeographicArea)
(documentation LandArea "An area which is predominantly solid ground,
e.g. a &%Nation, a mountain, a desert, etc.  Note that a &%LandArea may
contain some relatively small &%WaterAreas.  For example, Australia is
a &%LandArea even though it contains various rivers and lakes.")

(=>
   (instance ?LAND1 LandArea)
   (exists (?LAND2)
      (and
         (part ?LAND1 ?LAND2)
         (or
            (instance ?LAND2 Continent)
            (instance ?LAND2 Island)))))

(subclass ShoreArea LandArea)
(documentation ShoreArea "A &%ShoreArea is a &%LandArea approximately 1-3 km wide bordering a body of water, such as an ocean, bay, river, or lake.  A &%ShoreArea may comprise a variety of &%LandForms, such as dunes, sloughs, and marshes.")

(=>
   (instance ?BANK ShoreArea)
   (exists (?WATER)
      (and
         (instance ?WATER WaterArea)
         (meetsSpatially ?BANK ?WATER))))

(subclass Continent LandArea)
(documentation Continent "One of the seven largest land masses on earth,
viz. Africa, North America, South America, Antarctica, Europe, Asia, and
Oceania (also known by the &%Nation which is coextensive with the
&%Continent - Australia).")

(equal (CardinalityFn Continent) 7)

(subclass Island LandArea)
(documentation Island "A &%LandArea that is completely surrounded by a &%WaterArea.")

(=>
   (instance ?ISLAND Island)
   (not
      (exists (?AREA ?PART1 ?PART2)
         (and
            (instance ?AREA LandArea)
            (part ?PART1 ?ISLAND)
            (part ?PART2 ?AREA)
            (not
               (part ?ISLAND ?AREA))
            (not
               (part ?AREA ?ISLAND))
            (meetsSpatially ?PART1 ?PART2)))))

(=>
	(and
		(instance ?ISLE Island)
		(instance ?AREA GeographicArea)
		(meetsSpatially ?ISLE ?AREA))
	(not
		(instance ?AREA LandArea)))

(=>
	(instance ?ISLE Island)
	(exists (?WATER)
		(and
			(instance ?WATER WaterArea)
			(meetsSpatially ?ISLE ?WATER))))

(subclass Nation GeopoliticalArea)
(subclass Nation LandArea)
(documentation Nation "The broadest &%GeopoliticalArea, i.e. &%Nations are
&%GeopoliticalAreas that are not part of any other overarching and
comprehensive governance structure (excepting commonwealths and other sorts
of loose international organizations).")

(subclass StateOrProvince GeopoliticalArea)
(subclass StateOrProvince LandArea)
(documentation StateOrProvince "Administrative subdivisions of a
&%Nation that are broader than any other political subdivisions that
may exist.  This &%Class includes the states of the United States, as
well as the provinces of Canada and European countries.")

(=>
   (instance ?STATE StateOrProvince)
   (exists (?LAND)
      (and
         (instance ?LAND Nation)
         (properPart ?STATE ?LAND))))

(subclass City GeopoliticalArea)
(subclass City LandArea)
(documentation City "A &%LandArea of relatively small size, inhabited
by a community of people, and having some sort of political structure.
Note that this class includes both large cities and small settlements
like towns, villages, hamlets, etc.")

(subclass Transitway Region)
(subclass Transitway SelfConnectedObject)
(documentation Transitway "&%Transitway is the broadest class
of regions which may be passed through as a &%path in instances
of &%Translocation.  &%Transitway includes land, air, and sea
regions, and it includes both natural and artificial transitways.")

(subclass Roadway LandTransitway)
(subclass Roadway LandArea)
(documentation Roadway "&%Roadway is the subclass of &%LandTransitways
that are areas intended for surface travel by self-powered, wheeled
vehicles, excluding those that travel on tracks.  &%Roadways have been
at least minimally improved to enable the passage of vehicles.
&%Roadways include dirt and gravelled roads, paved streets, and
expressways.")

(subclass LandTransitway Transitway)
(subclass LandTransitway LandArea)
(documentation LandTransitway "&%LandTransitway is the subclass of
&%Transitway that represents areas intended for motion over the ground.")

(=>
   (instance ?WAY LandTransitway)
   (hasPurpose (exists (?TRANSPORT)
                  (and
                     (instance ?TRANSPORT Transportation)
                     (path ?TRANSPORT ?WAY)))))

(subclass Water CompoundSubstance)
(documentation Water "The &%Class of samples of the compound H20.  Note
that this &%Class covers both pure and impure &%Water.")

(subclass Mineral Mixture)
(documentation Mineral " Any of various naturally occurring homogeneous substances (such as stone, coal, salt, sulfur, sand, petroleum), or synthetic substances having the chemical composition and crystalline form and properties of a naturally occurring mineral.")

(instance developmentalForm BinaryPredicate)
(instance developmentalForm AsymmetricRelation)
(instance developmentalForm TransitiveRelation)
(subrelation developmentalForm attribute)
(domain developmentalForm 1 OrganicObject)
(domain developmentalForm 2 DevelopmentalAttribute)
(documentation developmentalForm "(&%developmentalForm ?OBJECT ?FORM)
means that ?FORM is an earlier stage in the individual maturation of
?OBJECT.  For example, tadpole and caterpillar are &%developmentalForms
of frogs and butterflies, respectively.")

(=>
   (and
      (holdsDuring ?TIME1 (developmentalForm ?OBJ ?ATTR1))
      (successorAttributeClosure ?ATTR2 ?ATTR1))
   (exists (?TIME2)
      (and
         (earlier ?TIME2 ?TIME1)
         (holdsDuring ?TIME2 (developmentalForm ?OBJ ?ATTR2)))))

(subclass OrganicObject CorpuscularObject)
(partition OrganicObject Organism AnatomicalStructure)
(documentation OrganicObject "This class encompasses &%Organisms,
&%CorpuscularObjects that are parts of &%Organisms, i.e. &%BodyParts,
and &%CorpuscularObjects that are nonintentionally produced by
&%Organisms, e.g. &%ReproductiveBodies.")

(subclass Organism OrganicObject)
(subclass Organism Agent)
(partition Organism Animal Plant Microorganism)
(documentation Organism "Generally, a living individual, including all
&%Plants and &%Animals.")

(=>
   (instance ?ORGANISM Organism)
   (exists (?BIRTH)
      (and
         (instance ?BIRTH Birth)
         (experiencer ?BIRTH ?ORGANISM))))

(instance inhabits BinaryPredicate)
(instance inhabits AsymmetricRelation)
(domain inhabits 1 Organism)
(domain inhabits 2 Object)
(documentation inhabits "A very basic notion of living within something
else.  (&%inhabits ?ORGANISM ?OBJECT) means that ?OBJECT is the residence,
nest, home, etc. of ?ORGANISM.")

(=>
   (inhabits ?ORGANISM ?OBJ)
   (exists (?TIME)
      (holdsDuring ?TIME (located ?ORGANISM ?OBJ))))

(subrelation home inhabits)
(domain home 1 Human)
(domain home 2 StationaryArtifact)
(documentation home "The relation between a &%Human and a home of the
&%Human.")

(instance parent BinaryPredicate)
(subrelation parent familyRelation)
(instance parent AsymmetricRelation)
(instance parent IntransitiveRelation)
(domain parent 1 Organism)
(domain parent 2 Organism)
(documentation parent "The general relationship of parenthood.
(&%parent ?CHILD ?PARENT) means that ?PARENT is a biological parent
of ?CHILD.")

(=>
   (parent ?CHILD ?PARENT)
   (before (BeginFn (WhenFn ?PARENT)) (BeginFn (WhenFn ?CHILD))))

(=>
   (and
      (parent ?CHILD ?PARENT)
      (subclass ?CLASS Organism)
      (instance ?PARENT ?CLASS))
   (instance ?CHILD ?CLASS))

(=>
   (parent ?CHILD ?PARENT)
   (or
      (mother ?CHILD ?PARENT)
      (father ?CHILD ?PARENT)))

(=>
   (instance ?ORGANISM Organism)
   (exists (?PARENT)
      (parent ?ORGANISM ?PARENT)))

(instance mother SingleValuedRelation)
(subrelation mother parent)
(domain mother 1 Organism)
(domain mother 2 Organism)
(documentation mother "The general relationship of motherhood.
(&%mother ?CHILD ?MOTHER) means that ?MOTHER is the biological mother
of ?CHILD.")

(=>
   (mother ?CHILD ?MOTHER)
   (attribute ?MOTHER Female))

(instance father SingleValuedRelation)
(subrelation father parent)
(domain father 1 Organism)
(domain father 2 Organism)
(documentation father "The general relationship of fatherhood.
(&%father ?CHILD ?FATHER) means that ?FATHER is the biological father
of ?CHILD.")

(=>
   (father ?CHILD ?FATHER)
   (attribute ?FATHER Male))

(instance sibling BinaryPredicate)
(subrelation sibling familyRelation)
(instance sibling SymmetricRelation)
(instance sibling IrreflexiveRelation)
(domain sibling 1 Organism)
(domain sibling 2 Organism)
(documentation sibling "The relationship between two &%Organisms that
have the same &%mother and &%father.  Note that this relationship does
not hold between half-brothers, half-sisters, etc.")

(<=>
   (sibling ?ANIMAL1 ?ANIMAL2)
   (and
      (not (equal ?ANIMAL1 ?ANIMAL2))
      (exists (?FATHER ?MOTHER)
         (and
            (father ?ANIMAL1 ?FATHER)
            (father ?ANIMAL2 ?FATHER)
            (mother ?ANIMAL1 ?MOTHER)
            (mother ?ANIMAL2 ?MOTHER)))))

(subrelation daughter parent)
(domain daughter 1 Woman)
(domain daughter 2 Human)
(documentation daughter "The general relationship of daughterhood.
(&%daughter ?CHILD ?PARENT) means that ?CHILD is the biological daughter
of ?PARENT.")

(subrelation son parent)
(domain son 1 Man)
(domain son 2 Human)
(documentation son "The general relationship of being a son.
(&%son ?CHILD ?PARENT) means that ?CHILD is the biological
son of ?PARENT.")

(subrelation brother sibling)
(instance brother IrreflexiveRelation)
(instance brother TransitiveRelation)
(domain brother 1 Man)
(domain brother 2 Human)
(documentation brother "The general relationship of being a brother.
(&%brother ?MAN ?PERSON) means that ?MAN is the brother of ?PERSON.")

(subrelation sister sibling)
(instance sister IrreflexiveRelation)
(instance sister TransitiveRelation)
(domain sister 1 Woman)
(domain sister 2 Human)
(documentation sister "The general relationship of being a sister.
(&%sister ?WOMAN ?PERSON) means that ?WOMAN is the sister of ?PERSON.")

(subrelation ancestor familyRelation)
(instance ancestor TransitiveRelation)
(instance ancestor IrreflexiveRelation)
(domain ancestor 1 Organism)
(domain ancestor 2 Organism)
(documentation ancestor "The transitive closure of the &%parent predicate.
(&%ancestor ?DESCENDANT ?ANCESTOR) means that ?ANCESTOR is either the
&%parent of ?DESCENDANT or the &%parent of the &%parent of &%DESCENDANT or
etc.")

(=>
   (parent ?PARENT ?CHILD)
   (ancestor ?PARENT ?CHILD))

;; The following formulas incorporate the content in the Natural-Kinds
;; ontology developed by ITBM-CNR.  This content is essentially a set of
;; high-level biological categories.

(subclass Plant Organism)
(documentation Plant "An &%Organism having cellulose cell walls, growing
by synthesis of &%Substances, generally distinguished by the presence of
chlorophyll, and lacking the power of locomotion.")

(subclass FloweringPlant Plant)
(documentation FloweringPlant "A &%Plant that produces seeds and flowers.
This class includes trees, shrubs, herbs, and flowers.")

(subclass NonFloweringPlant Plant)
(disjoint NonFloweringPlant FloweringPlant)
(disjointDecomposition NonFloweringPlant Alga Fern Fungus Moss)
(documentation NonFloweringPlant "A &%Plant that reproduces with spores and
does not produce flowers.")

(subclass Alga NonFloweringPlant)
(documentation Alga "A chiefly aquatic plant that contains chlorophyll,
but does not form embryos during development and lacks vascular tissue.")

(=>
	(instance ?ALGA Alga)
	(exists (?WATER)
		(and
			(inhabits ?ALGA ?WATER)
			(instance ?WATER Water))))

(subclass Fungus NonFloweringPlant)
(documentation Fungus "A eukaryotic &%Organism characterized by the
absence of chlorophyll and the presence of rigid cell walls. Included
here are both slime molds and true fungi such as yeasts, molds, mildews,
and mushrooms.")

(=>
	(and
		(instance ?FUNGUS Fungus)
		(inhabits ?FUNGUS ?OBJ))
	(instance ?OBJ Organism))

(subclass Moss NonFloweringPlant)
(documentation Moss "A &%NonFloweringPlant without true roots and little
if any vascular tissue.")

(subclass Fern NonFloweringPlant)
(documentation Fern "A &%NonFloweringPlant that contains vascular tissue.
This class includes true ferns, as well as horsetails, club mosses, and
whisk ferns.")

(subclass Animal Organism)
(partition Animal Vertebrate Invertebrate)
(documentation Animal "An &%Organism with eukaryotic &%Cells, and lacking
stiff cell walls, plastids, and photosynthetic pigments.")

(subclass Microorganism Organism)
(documentation Microorganism "An &%Organism that can be seen only with
the aid of a microscope.")

(subclass Bacterium Microorganism)
(documentation Bacterium "A small, typically one-celled, prokaryotic
&%Microorganism.")

(=>
   (instance ?BACTERIUM Bacterium)
   (exists (?CELL1)
      (and
         (component ?CELL1 ?BACTERIUM)
         (instance ?CELL1 Cell)
         (forall (?CELL2)
            (=>
               (and
                  (component ?CELL2 ?BACTERIUM)
                  (instance ?CELL2 Cell))
               (equal ?CELL1 ?CELL2))))))

(=>
	(and
		(instance ?BACTERIUM Bacterium)
		(inhabits ?BACTERIUM ?OBJ))
	(instance ?OBJ Organism))

(subclass Virus Microorganism)
(documentation Virus "An &%Organism consisting of a core of a single
nucleic acid enclosed in a protective coat of protein. A virus may replicate
only inside a host living cell. A virus exhibits some but not all of the
usual characteristics of living things.")

(=>
   (instance ?VIRUS Virus)
   (exists (?MOL1)
      (and
         (component ?MOL1 ?VIRUS)
         (instance ?MOL1 Molecule)
         (forall (?MOL2)
            (=>
               (and
                  (component ?MOL2 ?VIRUS)
                  (instance ?MOL2 Molecule))
               (equal ?MOL1 ?MOL2))))))

(=>
	(and
		(instance ?VIRUS Virus)
		(inhabits ?VIRUS ?OBJ))
	(instance ?OBJ Organism))

(=>
	(and
		(instance ?VIRUS Virus)
		(instance ?PROC Replication)
		(agent ?PROC ?VIRUS))
	(exists (?CELL)
		(and
			(located ?PROC ?CELL)
			(instance ?CELL Cell))))

(subclass Vertebrate Animal)
(documentation Vertebrate "An &%Animal which has a spinal column.")

(subclass Invertebrate Animal)
(disjointDecomposition Invertebrate Worm Mollusk Arthropod)
(documentation Invertebrate "An &%Animal which has no spinal column.")

(subclass Worm Invertebrate)
(documentation Worm "Long, narrow, soft-bodied &%Invertebrates.")

(subclass Mollusk Invertebrate)
(documentation Mollusk "Soft-bodied &%Invertebrate that is usually
contained in a shell.  Includes oysters, clams, mussels, snails, slugs,
octopi, and squid.")

(subclass Arthropod Invertebrate)
(disjointDecomposition Arthropod Arachnid Myriapod Insect Crustacean)
(documentation Arthropod "A &%Class of &%Invertebrate that includes
&%Arachnids and &%Insects.")

(subclass Arachnid Arthropod)
(documentation Arachnid "A &%Class of &%Arthropods that includes
ticks and spiders.")

(subclass Myriapod Arthropod)
(documentation Myriapod "A &%Class of &%Arthropods that includes
centipedes and millipedes.")

(subclass Insect Arthropod)
(documentation Insect "A &%Class of small &%Arthropods that are
air-breathing and that are distinguished by appearance.")

(subclass Crustacean Arthropod)
(documentation Crustacean "A &%Class of &%Arthropods that mainly dwells
in water and has a segmented body and a chitinous exoskeleton.  Includes
lobsters, crabs, shrimp, and barnacles.")

(subclass ColdBloodedVertebrate Vertebrate)
(disjointDecomposition ColdBloodedVertebrate Amphibian Fish Reptile)
(documentation ColdBloodedVertebrate "&%Vertebrates whose body temperature
is not internally regulated.")

(subclass WarmBloodedVertebrate Vertebrate)
(disjoint WarmBloodedVertebrate ColdBloodedVertebrate)
(documentation WarmBloodedVertebrate "&%Vertebrates whose body temperature
is internally regulated.")

(subclass Amphibian ColdBloodedVertebrate)
(documentation Amphibian "A cold-blooded, smooth-skinned &%Vertebrate
which characteristically hatches as an aquatic larva, breathing by
gills.  When mature, the &%Amphibian breathes with &%Lungs.")

(subclass Bird WarmBloodedVertebrate)
(disjoint Bird Mammal)
(documentation Bird "A &%Vertebrate having a constant body temperature
and characterized by the presence of feathers.")

(subclass Fish ColdBloodedVertebrate)
(documentation Fish "A cold-blooded aquatic &%Vertebrate characterized by
fins and breathing by gills. Included here are &%Fish having either a bony
skeleton, such as a perch, or a cartilaginous skeleton, such as a shark.
Also included are those &%Fish lacking a jaw, such as a lamprey or
hagfish.")

(=>
	(instance ?FISH Fish)
	(exists (?WATER)
		(and
			(inhabits ?FISH ?WATER)
			(instance ?WATER Water))))

(subclass Mammal WarmBloodedVertebrate)
(disjointDecomposition Mammal AquaticMammal HoofedMammal Marsupial Rodent Primate)
(documentation Mammal "A &%Vertebrate having a constant body temperature
and characterized by the presence of hair, mammary glands, and sweat
glands.")

(subclass AquaticMammal Mammal)
(documentation AquaticMammal "The &%Class of &%Mammals that dwell chiefly
in the water.  Includes whales, dolphins, manatees, seals, and walruses.")

(subclass HoofedMammal Mammal)
(documentation HoofedMammal "The &%Class of quadruped &%Mammals with hooves.
Includes horses, cows, sheep, pigs, antelope, etc.")

(subclass Marsupial Mammal)
(documentation Marsupial "The &%Class of &%Mammals which have a pouch for
their young.")

(subclass Carnivore Mammal)
(documentation Carnivore "The &%Class of flesh-eating &%Mammals.  Members
of this &%Class typically have four or five claws on each paw.  Includes
cats, dogs, bears, racoons, and skunks.")

(=>
   (and
      (instance ?CARNIVORE Carnivore)
      (instance ?EAT Eating)
      (agent ?EAT ?CARNIVORE)
      (patient ?EAT ?PREY))
   (instance ?PREY Animal))

(subclass Canine Carnivore)
(disjoint Canine Feline)
(documentation Canine "The &%Class of &%Carnivores with completely
separable toes, nonretractable claws, and long muzzles.")

(subclass Feline Carnivore)
(documentation Feline "The &%Class of &%Carnivores with completely
separable toes, nonretractable claws, slim bodies, and rounded heads.")

(subclass Rodent Mammal)
(documentation Rodent "The &%Class of &%Mammals with one or two pairs
of incisors for gnawing.  Includes rats, mice, guinea pigs, and
rabbits.")

(subclass Primate Mammal)
(disjointDecomposition Primate Ape Monkey Hominid)
(documentation Primate "The &%Class of &%Mammals which are
&%Primates.")

(subclass Ape Primate)
(documentation Ape "Various &%Primates with no tails or only short
tails.")

(subclass Monkey Primate)
(documentation Monkey "Various &%Primates with relatively long
tails.")

(subclass Hominid Primate)
(documentation Hominid "Includes &%Humans and relatively recent
ancestors of &%Humans.")

(subclass Human Hominid)
(subclass Human CognitiveAgent)
(documentation Human "Modern man, the only remaining species of the Homo
genus.")

(subclass Man Human)
(documentation Man "The class of &%Male &%Humans.")

(=>
   (instance ?MAN Man)
   (attribute ?MAN Male))

(subclass Woman Human)
(documentation Woman "The class of &%Female &%Humans.")

(=>
   (instance ?WOMAN Woman)
   (attribute ?WOMAN Female))

(subclass Reptile ColdBloodedVertebrate)
(documentation Reptile "A &%ColdBloodedVertebrate having an external
covering of scales or horny plates.  &%Reptiles breathe by means of
&%Lungs and generally lay eggs.")

;; The following formulas cover biologically related &%Classes under
;; &%Substance.

(subclass BiologicallyActiveSubstance Substance)
(documentation BiologicallyActiveSubstance "A &%Substance that is
capable of inducing a change in the structure or functioning of an
&%Organism.  This &%Class includes &%Substances used in the treatment,
diagnosis, prevention or analysis of normal and abnormal body function.
This &%Class also includes &%Substances that occur naturally in the body
and are administered therapeutically.  Finally, &%BiologicallyActiveSubstance
includes &%Nutrients, most drugs of abuse, and agents that require special
handling because of their toxicity.")

(subclass Nutrient BiologicallyActiveSubstance)
(disjointDecomposition Nutrient Protein Carbohydrate Vitamin)
(documentation Nutrient "A &%BiologicallyActiveSubstance required by an &%Organism.
It is generally ingested as &%Food, and it is of primary interest because of its role
in the biologic functioning of the &%Organism.")

(subclass Protein Nutrient)
(documentation Protein "A &%Nutrient made up of amino acids joined by
peptide bonds.")

(subclass Enzyme Protein)
(documentation Enzyme "A complex &%Protein that is produced by living
cells and which catalyzes specific biochemical reactions. There are six
main types of enzymes:  oxidoreductases, transferases, hydrolases,
lyases, isomerases, and ligases.")

(subclass Carbohydrate Nutrient)
(documentation Carbohydrate "An element of living cells and a source of
energy for &%Animals.  This class includes both simple &%Carbohydrates,
i.e. sugars, and complex &%Carbohydrates, i.e. starches.")

(subclass Vitamin Nutrient)
(documentation Vitamin "A &%Nutrient present in natural products or made
synthetically, which is essential in the diet of &%Humans and other higher
&%Animals.  Included here are &%Vitamin precursors and provitamins.")

(subclass GasMixture Mixture)
(documentation GasMixture "Any &%Mixture that satisfies two conditions,
viz. it is made up predominantly of things which are a &%Gas and any
component other than &%Gas in the &%Mixture is in the form of fine particles which are suspended in the &%Gas.")

(subclass Smoke GasMixture)
(documentation Smoke "A mixture of fine particles suspended in a gas that is
produced by &%Combustion.")

(=>
   (instance ?SMOKE Smoke)
   (exists (?BURNING)
      (and
         (instance ?BURNING Combustion)
         (result ?BURNING ?SMOKE))))

(subclass Cloud GasMixture)
(documentation Cloud "Any &%GasMixture that is visible, e.g. clouds of &%Smoke produced by a fire or clouds of water vapor in the sky.")

(=>
   (instance ?CLOUD Cloud)
   (capability Seeing patient ?CLOUD))

(subclass WaterCloud Cloud)
(documentation WaterCloud "Any &%Cloud that is composed primarily of water vapor.")

(=>
   (instance ?CLOUD WaterCloud)
   (exists (?WATER)
      (and
         (instance ?WATER Water)
         (part ?WATER ?CLOUD))))

(=>
   (instance ?CLOUD WaterCloud)
   (forall (?PART)
      (=>
         (and
            (part ?PART ?CLOUD)
            (not (instance ?PART Water)))
         (exists (?WATER)
            (and
               (instance ?WATER Water)
               (part ?WATER ?CLOUD)
               (measure ?WATER ?MEASURE1)
               (measure ?PART ?MEASURE2)
               (greaterThan ?WATER ?PART))))))

(subclass Air Mixture)
(subclass Air (ExtensionFn Gas))
(documentation Air "&%Air is the gaseous stuff that makes up the
atmosphere surrounding Earth.")

(=>
	(instance ?AIR Air)
	(piece ?AIR EarthsAtmosphere))

(=>
	(instance ?AIRSPACE AtmosphericRegion)
	(exists (?AIR)
		(and
			(instance ?AIR Air)
			(part ?AIR ?AIRSPACE))))

(=>
	(instance ?AIR Air)
	(exists (?PART)
		(and
		     (instance ?PART Oxygen)
		     (part ?PART ?AIR))))

(=>
	(instance ?AIR Air)
	(exists (?PART)
		(and
		     (instance ?PART Nitrogen)
		     (part ?PART ?AIR))))

(=>
   (instance ?WIND Wind)
   (exists (?AIR)
      (and
         (patient ?WIND ?AIR)
         (instance ?AIR Air))))

(subclass BodySubstance Mixture)
(documentation BodySubstance "Extracellular material and mixtures of
cells and extracellular material that are produced, excreted or accreted
by an &%Organism.  Included here are &%Substances such as saliva, dental
enamel, sweat, and gastric acid.")

(subclass Hormone BodySubstance)
(subclass Hormone BiologicallyActiveSubstance)
(documentation Hormone "In &%Animals, a chemical secreted by an
endocrine gland whose products are released into the circulating fluid.
&%Plant hormones or synthetic hormones which are used only to alter or
control various physiologic processes, e.g., reproductive control agents,
are assigned to the &%Class &%BiologicallyActiveSubstance. &%Hormones act as
chemical messengers and regulate various physiologic processes such as
growth, reproduction, metabolism, etc.  They usually fall into two broad
categories, viz. steroid hormones and peptide hormones.")

(=>
   (instance ?HORMONE Hormone)
   (exists (?PROCESS ?GLAND)
      (and
         (instance ?GLAND Gland)
         (instrument ?PROCESS ?GLAND)
         (result ?PROCESS ?HORMONE))))

(subclass Blood BodySubstance)
(documentation Blood "A fluid present in &%Animals that transports
&%Nutrients to and waste products away from various &%BodyParts.")

(subclass Food SelfConnectedObject)
(disjointDecomposition Food Meat Beverage)
(documentation Food "Any &%SelfConnectedObject containing &%Nutrients,
such as carbohydrates, proteins, and fats, that can be ingested by a
living &%Animal and metabolized into energy and body tissue.")

(=>
    (instance ?FOOD Food)
    (exists (?NUTRIENT)
        (and
            (instance ?NUTRIENT Nutrient)
            (part ?NUTRIENT ?FOOD))))

(=>
   (instance ?FOOD Food)
   (forall (?PART1)
      (=>
         (part ?PART1 ?FOOD)
         (exists (?PART2 ?ANIMAL)
            (and
               (part ?PART1 ?PART2)
               (part ?PART2 ?ANIMAL)
               (instance ?ANIMAL Animal))))))

(subclass Meat Food)
(documentation Meat "Any &%Food which was originally part of an
&%Animal and is not ingested by drinking, including eggs and animal
blood that is eaten as food.  Note that this class covers both raw
meat and meat that has been prepared in some way, e.g. by cooking.
Note too that preparations involving &%Meat and &%FruitOrVegetable
are classed directly under &%Food.")

(=>
   (instance ?MEAT Meat)
   (forall (?PART)
      (=>
         (part ?PART ?MEAT)
         (exists (?SUBPART ?TIME ?ANIMAL)
            (and
               (part ?SUBPART ?PART)
               (holdsDuring ?TIME (and (instance ?ANIMAL Animal) (part ?SUBPART ?ANIMAL))))))))

(subclass Beverage Food)
(documentation Beverage "Any &%Food that is ingested by &%Drinking.
Note that this class is disjoint with the other subclasses of &%Food,
i.e. &%Meat and &%FruitOrVegetable.")

(=>
   (instance ?BEV Beverage)
   (attribute ?BEV Liquid))

(=>
   (and
      (instance ?DRINK Drinking)
      (patient ?DRINK ?BEV))
   (instance ?BEV Beverage))

(subclass AnatomicalStructure OrganicObject)
(partition AnatomicalStructure BodyPart AbnormalAnatomicalStructure)
(partition AnatomicalStructure AnimalAnatomicalStructure PlantAnatomicalStructure)
(documentation AnatomicalStructure "A normal or pathological part
of the anatomy or structural organization of an &%Organism.  This
class covers &%BodyParts, as well as structures that are given off
by &%Organisms, e.g. &%ReproductiveBodies.")

(=>
    (instance ?ANAT AnatomicalStructure)
    (exists (?ORGANISM ?TIME)
        (and
            (instance ?ORGANISM Organism)
            (temporalPart ?TIME (WhenFn ?ORGANISM))
            (holdsDuring ?TIME (part ?ANAT ?ORGANISM)))))

(=>
    (instance ?PART AnatomicalStructure)
    (exists (?CELL)
        (and
            (instance ?CELL Cell)
            (part ?CELL ?PART))))

(subclass AbnormalAnatomicalStructure AnatomicalStructure)
(documentation AbnormalAnatomicalStructure "Any &%AnatomicalStructure which
is not normally found in the &%Organism of which it is a part, i.e. it is
the result of a &%PathologicProcess.  This class covers tumors, birth marks,
goiters, etc.")

(=>
   (instance ?STRUCTURE AbnormalAnatomicalStructure)
   (exists (?PROC)
      (and
         (instance ?PROC PathologicProcess)
         (result ?PROC ?PART))))

(subclass BodyPart AnatomicalStructure)
(documentation BodyPart "A collection of &%Cells and &%Tissues which
are localized to a specific area of an &%Organism and which are not
pathological. The instances of this &%Class range from gross structures
to small components of complex &%Organs.")

(=>
    (instance ?PART BodyPart)
    (exists (?ORGANISM ?TIME)
        (and
            (instance ?ORGANISM Organism)
            (temporalPart ?TIME (WhenFn ?ORGANISM))
            (holdsDuring ?TIME (part ?ANAT ?ORGANISM)))))

(=>
   (instance ?PART BodyPart)
   (exists (?PROC)
      (and
         (instance ?PROC PhysiologicProcess)
         (result ?PROC ?PART))))

(subclass AnimalAnatomicalStructure AnatomicalStructure)
(documentation AnimalAnatomicalStructure "&%AnatomicalStructures that
are possessed exclusively by &%Animals.")

(=>
   (and
      (instance ?STRUCTURE AnimalAnatomicalStructure)
      (instance ?ANIMAL Organism)
      (part ?STRUCTURE ?ANIMAL))
   (instance ?ANIMAL Animal))

(subclass PlantAnatomicalStructure AnatomicalStructure)
(documentation PlantAnatomicalStructure "&%AnatomicalStructures that
are possessed exclusively by &%Plants.")

(=>
   (and
      (instance ?STRUCTURE PlantAnatomicalStructure)
      (instance ?PLANT Organism)
      (part ?STRUCTURE ?PLANT))
   (instance ?PLANT Plant))

(subclass ReproductiveBody BodyPart)
(documentation ReproductiveBody "Reproductive structure of &%Organisms.
Consists of an &%Embryonic &%Object and a nutritive/protective envelope.
Note that this class includes seeds, spores, and &%FruitOrVegetables, as
well as the eggs produced by &%Animals.")

(subclass Egg ReproductiveBody)
(subclass Egg AnimalAnatomicalStructure)
(documentation Egg "The fertilized or unfertilized female &%ReproductiveBody of an &%Animal.  This includes &%Bird and &%Reptile eggs, as well as mammalian ova.")

(subclass Pollen ReproductiveBody)
(subclass Pollen PlantAnatomicalStructure)
(documentation Pollen "A powder produced by &%FloweringPlants that contains male
gametes and is capable of fertilizing the seeds of &%FloweringPlants of the same
species.")

(subclass Spore ReproductiveBody)
(subclass Spore PlantAnatomicalStructure)
(documentation Spore "Any &%ReproductiveBody of a &%NonFloweringPlant.")

(=>
   (instance ?SPORE Spore)
   (exists (?PLANT ?TIME)
      (and
         (instance ?PLANT NonFloweringPlant)
         (holdsDuring ?TIME (part ?SPORE ?PLANT)))))

(subclass Seed ReproductiveBody)
(subclass Seed PlantAnatomicalStructure)
(documentation Seed "The fertilized or unfertilized female &%ReproductiveBody of a &%FloweringPlant.")

(=>
   (instance ?SEED Seed)
   (exists (?PLANT ?TIME)
      (and
         (instance ?PLANT FloweringPlant)
         (holdsDuring ?TIME (part ?SEED ?PLANT)))))

(subclass FruitOrVegetable PlantAnatomicalStructure)
(subclass FruitOrVegetable ReproductiveBody)
(documentation FruitOrVegetable "Any fruit or vegetable, i.e. a
ripened &%ReproductiveBody of a &%Plant.  Note that &%FruitOrVegetable
is not a subclass of &%Food, because some fruits, e.g. poisonous
berries, are not edible.")

(subclass BodyCovering BodyPart)
(documentation BodyCovering "Any &%BodyPart which is a covering of another
&%BodyPart or of an entire &%Organism.  This would include the rinds of
&%FruitOrVegetables and the skins of &%Animals.")

(=>
   (instance ?COVER BodyCovering)
   (exists (?BODY)
      (and
         (superficialPart ?COVER ?BODY)
         (or
            (instance ?BODY Organism)
            (instance ?BODY BodyPart)))))

(subclass BodyJunction BodyPart)
(documentation BodyJunction "The place where two &%BodyParts
meet or connect.")

(=>
    (instance ?JUNCT BodyJunction)
    (exists (?STRUCT)
        (and
            (instance ?STRUCT BodyPart)
            (component ?JUNCT ?STRUCT))))

(=>
    (instance ?JUNCT BodyJunction)
    (exists (?STRUCT1 ?STRUCT2)
        (and
            (connected ?JUNCT ?STRUCT1)
	      (connected ?JUNCT ?STRUCT2)
            (instance ?STRUCT1 BodyPart)
            (instance ?STRUCT2 BodyPart)
            (not
                (equal ?STRUCT1 ?STRUCT2)))))

(subclass BodyVessel BodyPart)
(documentation BodyVessel "Any tube-like structure which occurs naturally in
an &%Organism and through which a &%BodySubstance can circulate.")

(subclass Cell BodyPart)
(documentation Cell "The fundamental structural and functional unit of
living &%Organisms.")

(subclass Organ BodyPart)
(documentation Organ "A somewhat independent &%BodyPart that performs a
specialized function.  Note that this functional definition covers bodily
systems, e.g. the digestive system or the central nervous system.")

(=>
   (instance ?ORGAN Organ)
   (exists (?PURP)
      (hasPurpose ?ORGAN ?PURP)))

(subclass Gland Organ)
(documentation Gland "An &%Organ that removes &%Substances from the &%Blood,
alters them in some way, and then releases them.")

(subclass Tissue BodySubstance)
(disjointDecomposition Tissue Bone Muscle FatTissue)
(documentation Tissue "An aggregation of similarly specialized &%Cells
and the associated intercellular substance. &%Tissues are relatively
non-localized in comparison to &%BodyParts, &%Organs or &%Organ components.
The main features of &%Tissues are self-connectivity (see
&%SelfConnectedObject) and being a homogeneous mass (all parts in the
same granularity are instances of &%Tissue as well).")

(=>
    (instance ?STUFF Tissue)
    (exists (?PART)
        (and
            (instance ?PART Cell)
            (part ?PART ?STUFF))))

(=>
    (instance ?STUFF Tissue)
    (exists (?ORGANISM)
        (and
            (instance ?ORGANISM Organism)
            (part ?STUFF ?ORGANISM))))

(subclass Bone Tissue)
(documentation Bone "Rigid &%Tissue composed largely of calcium that makes up
the skeleton of &%Vertebrates.  Note that this &%Class also includes teeth.")

(=>
   (instance ?BONE Bone)
   (exists (?VERT)
      (and
         (instance ?VERT Vertebrate)
         (part ?BONE ?VERT))))

(subclass Muscle Tissue)
(documentation Muscle "Nonrigid &%Tissue appearing only in &%Animals and
composed largely of contractile cells.")

(subclass FatTissue Tissue)
(documentation FatTissue "Nonrigid &%Tissue that is composed largely of
fat cells.")

(subclass Noun Word)
(documentation Noun "One of the parts of speech.  The &%Class of &%Words
that conventionally denote &%Objects.")

(subclass Verb Word)
(documentation Verb "One of the parts of speech.  The &%Class of &%Words
that conventionally denote &%Processes.")

(subclass Adjective Word)
(documentation Adjective "One of the parts of speech.  The &%Class of
&%Words that conventionally denote &%Attributes of &%Objects.")

(subclass Adverb Word)
(documentation Adverb "One of the parts of speech.  The &%Class of &%Words
that conventionally denote &%Attributes of &%Processes.")

(subclass Particle Word)
(documentation Particle "An umbrella &%Class for any &%Word that does not
fit into the other subclasses of &%Word.  A &%Particle is generally a small
term that serves a grammatical or logical function, e.g. 'and', 'of',
'since', etc.  At some point, this class might be broken up into the
subclasses 'Connective', 'Preposition', etc.  Note that the class &%Particle
includes both personal and possessive pronouns, e.g. 'she', 'hers', 'it', 'its',
etc.")

(subclass Morpheme LinguisticExpression)
(documentation Morpheme "Part of a &%Word which cannot be subdivided
and which expresses a meaning.")

(=>
   (instance ?MORPH Morpheme)
   (not
      (exists (?OTHERMORPH)
         (and
            (instance ?OTHERMORPH Morpheme)
            (part ?OTHERMORPH ?MORPH)
            (not (equal ?OTHERMORPH ?MORPH))))))

(=>
   (instance ?MORPH Morpheme)
   (exists (?WORD)
         (and
            (instance ?WORD Word)
            (part ?MORPH ?WORD))))

(=>
    (instance ?WORD Word)
    (exists (?PART)
        (and
            (part ?PART ?WORD)
            (instance ?PART Morpheme))))

(subclass Phrase LinguisticExpression)
(disjointDecomposition Phrase VerbPhrase NounPhrase PrepositionalPhrase)
(documentation Phrase "A set of &%Words in a &%Language which form a unit,
i.e. express a meaning in the &%Language.")

(=>
    (instance ?PHRASE Phrase)
    (exists (?PART1 ?PART2)
        (and
            (part ?PART1 ?PHRASE)
	      (part ?PART2 ?PHRASE)
            (instance ?PART1 Word)
	      (instance ?PART2 Word)
            (not (equal ?PART1 ?PART2)))))

(subclass VerbPhrase Phrase)
(documentation VerbPhrase "A &%Phrase that has the same function as a
&%Verb.")

(=>
   (instance ?PHRASE VerbPhrase)
   (exists (?VERB)
      (and
         (instance ?VERB Verb)
         (part ?VERB ?PHRASE))))

(subclass NounPhrase Phrase)
(disjoint NounPhrase VerbPhrase)
(documentation NounPhrase "A &%Phrase that has the same function as a
&%Noun.")

(=>
    (instance ?SENTENCE Sentence)
    (exists (?PHRASE1 ?PHRASE2)
        (and
           (instance ?PHRASE1 NounPhrase)
           (instance ?PHRASE2 VerbPhrase)
           (part ?PHRASE1 ?SENTENCE)
           (part ?PHRASE2 ?SENTENCE))))

(=>
   (instance ?PHRASE NounPhrase)
   (exists (?NOUN)
      (and
         (instance ?NOUN Noun)
         (part ?NOUN ?PHRASE))))

(subclass PrepositionalPhrase Phrase)
(documentation PrepositionalPhrase "A &%Phrase that begins with a
preposition and that functions as an &%Adjective or an &%Adverb.")

(=>
   (instance ?PHRASE PrepositionalPhrase)
   (exists (?PREP)
      (and
         (instance ?PREP Particle)
         (part ?PREP ?PHRASE))))

(subclass Text LinguisticExpression)
(subclass Text Artifact)
(documentation Text "A &%LinguisticExpression or set of
&%LinguisticExpressions that perform a specific function related
to &%Communicating, e.g. express a discourse about a particular
topic, and that are inscribed in a &%CorpuscularObject by &%Humans.")

(=>
    (instance ?TEXT Text)
    (exists (?PART)
        (and
            (part ?PART ?TEXT)
            (instance ?PART LinguisticExpression))))

(=>
   (instance ?TEXT Text)
   (exists (?WRITE)
      (and
         (instance ?WRITE Writing)
         (result ?WRITE ?TEXT))))

(subclass FactualText Text)
(disjoint FactualText FictionalText)
(documentation FactualText "The class of &%Texts that purport to
reveal facts about the world.  Such texts are often known as information
or as non-fiction.  Note that something can be an instance of
&%FactualText, even if it is wholly inaccurate.  Whether something
is a &%FactualText is determined by the beliefs of the agent creating
the text.")

(=>
   (and
      (instance ?TEXT FactualText)
      (authors ?AGENT ?TEXT)
      (subsumesContentInstance ?TEXT ?CONTENT)
      (instance ?CONTENT Formula))
   (believes ?AGENT ?CONTENT))

(subclass FictionalText Text)
(documentation FictionalText "The class of &%Texts that purport to
be largely a product of the author's imagination, i.e. the author
does not believe that most of the content conveyed by the text is
an accurate depiction of the real world.  Note that something can
be an instance of &%FictionalText, even if it is completely true.
Whether something is a &%FictionalText is determined by the beliefs
of the agent creating the text.")

(=>
   (and
      (instance ?TEXT FactualText)
      (authors ?AGENT ?TEXT))
   (exists (?CONTENT)
      (and
         (subsumesContentInstance ?TEXT ?CONTENT)
         (instance ?CONTENT Formula)
         (not (believes ?AGENT ?CONTENT)))))

(subclass Sentence LinguisticExpression)
(disjointDecomposition Sentence Statement Supposition Question Request Order)
(documentation Sentence "A syntactically well-formed formula of a
&%Language.  It includes, at minimum, a predicate and a subject (which
may be explicit or implicit), and it expresses a &%Proposition.")

(=>
   (instance ?SENT Sentence)
   (exists (?NOUN ?VERB)
      (and
         (instance ?NOUN NounPhrase)
         (instance ?VERB VerbPhrase)
         (part ?NOUN ?SENT)
         (part ?VERB ?SENT))))

(=>
    (instance ?SENT Sentence)
    (exists (?PROP)
        (and
            (instance ?PROP Proposition)
            (containsInformation ?SENT ?PROP))))

(instance authors BinaryPredicate)
(instance authors AsymmetricRelation)
(domain authors 1 Agent)
(domainSubclass authors 2 Text)
(documentation authors "(&%authors ?AGENT ?TEXT) means that ?AGENT is
creatively responsible for ?TEXT.  For example, Agatha Christie is
author of Murder_on_the_Orient_Express.")

(=>
     (authors ?AGENT ?TEXT)
     (exists (?PROCESS ?INSTANCE)
          (and
               (agent ?PROCESS ?AGENT)
               (instance ?INSTANCE ?TEXT)
               (result ?PROCESS ?TEXT))))

(instance editor BinaryPredicate)
(instance editor AsymmetricRelation)
(domain editor 1 Agent)
(domainSubclass editor 2 Text)
(documentation editor "(&%editor ?AGENT ?TEXT) means that ?AGENT is
an editor of ?TEXT.")

(instance publishes BinaryPredicate)
(instance publishes AsymmetricRelation)
(domain publishes 1 Organization)
(domainSubclass publishes 2 Text)
(documentation publishes "(&%publishes ?ORG ?TEXT) means that ?ORG
publishes ?TEXT.  For example, Bantam Books publishes Agatha Christie's
Murder_on_the_Orient_Express.")

(<=>
   (publishes ?ORG ?TEXT)
   (exists (?PUB)
      (and
         (instance ?PUB Publication)
         (agent ?PUB ?ORG)
         (patient ?PUB ?TEXT))))

(instance EditionFn BinaryFunction)
(instance EditionFn PartialValuedRelation)
(domainSubclass EditionFn 1 ContentBearingObject)
(domain EditionFn 2 PositiveInteger)
(rangeSubclass EditionFn ContentBearingObject)
(documentation EditionFn "A &%BinaryFunction that maps a type of text
(e.g. Agatha Christie's Murder_on_the_Orient_Express) and a number
to the edition of the text type corresponding to the number.")

(=>
   (and
      (equal (EditionFn ?TEXT ?INT1) ?EDITION1)
      (equal (EditionFn ?TEXT ?INT2) ?EDITION2)
      (greaterThan ?INT2 ?INT1)
      (instance ?PUB1 Publication)
      (instance ?PUB2 Publication)
      (patient ?PUB1 ?EDITION1)
      (patient ?PUB2 ?EDITION2)
      (date ?PUB1 ?DATE1)
      (date ?PUB2 ?DATE2))
   (before (EndFn ?DATE1) (EndFn ?DATE2)))

(=>
   (equal (EditionFn ?TEXT1 ?NUMBER) ?TEXT2)
   (subsumesContentClass ?TEXT1 ?TEXT2))

(instance SeriesVolumeFn BinaryFunction)
(instance SeriesVolumeFn PartialValuedRelation)
(domainSubclass SeriesVolumeFn 1 Series)
(domain SeriesVolumeFn 2 PositiveInteger)
(rangeSubclass SeriesVolumeFn Text)
(documentation SeriesVolumeFn "A &%BinaryFunction that maps a type of &%Series
(e.g. the Encyclopedia_Britannica or the Popular_Mechanics periodical) and a
number to the volumes of the text type designated by the number.")

(=>
   (and
      (subclass ?TEXT Periodical)
      (equal (SeriesVolumeFn ?TEXT ?INT1) ?VOLUME1)
      (equal (SeriesVolumeFn ?TEXT ?INT2) ?VOLUME2)
      (greaterThan ?INT2 ?INT1)
      (instance ?PUB1 Publication)
      (instance ?PUB2 Publication)
      (patient ?PUB1 ?VOLUME1)
      (patient ?PUB2 ?VOLUME2)
      (date ?PUB1 ?DATE1)
      (date ?PUB2 ?DATE2))
   (before (EndFn ?DATE1) (EndFn ?DATE2)))

(=>
   (equal (SeriesVolumeFn ?SERIES ?NUMBER) ?VOLUME)
   (subsumesContentClass ?SERIES ?VOLUME))

(instance PeriodicalIssueFn BinaryFunction)
(instance PeriodicalIssueFn PartialValuedRelation)
(domainSubclass PeriodicalIssueFn 1 Periodical)
(domain PeriodicalIssueFn 2 PositiveInteger)
(rangeSubclass PeriodicalIssueFn Periodical)
(documentation PeriodicalIssueFn "A &%BinaryFunction that maps a subclass of
&%Periodical and a number to all of the issues of the &%Periodical corresponding
to the number.")

(=>
   (equal (PeriodicalIssueFn ?PERIODICAL ?NUMBER) ?ISSUE)
   (subsumesContentClass ?PERIODICAL ?ISSUE))

(subclass Book Text)
(documentation Book "A &%Text that has pages and is bound.")

(subclass Summary Text)
(documentation Summary "A short &%Text that is a summary of another,
longer &%Text.")

(=>
   (instance ?TEXT Summary)
   (exists (?TEXT2)
      (and
         (instance ?TEXT2 Text)
         (subsumesContentInstance ?TEXT2 ?TEXT))))

(subclass Series Text)
(documentation Series "A &%Text consisting of multiple self-contained units.
Some examples are an encyclopedia containing a couple dozen volumes, a television series made up of many episodes, a film serial, etc.")

(=>
   (instance ?SERIES Series)
   (exists (?BOOK1 ?BOOK2)
      (and
         (instance ?BOOK1 Book)
         (instance ?BOOK2 Book)
         (subsumesContentInstance ?SERIES ?BOOK1)
         (subsumesContentInstance ?SERIES ?BOOK2)
         (not (equal ?BOOK1 ?BOOK2)))))

(subclass Periodical Series)
(documentation Periodical "A &%Series whose elements are published separately
and on a periodic basis.")

(subclass Article Text)
(disjoint Article Book)
(documentation Article "A relatively short &%Text that either is unbound or is
bound with other &%Articles in a &%Book.")

(=>
   (and
      (instance ?ARTICLE1 Article)
      (instance ?BOOK Book)
      (subsumesContentInstance ?BOOK ?ARTICLE1))
   (exists (?ARTICLE2)
      (and
         (instance ?ARTICLE2 Article)
         (not (equal ?ARTICLE2 ?ARTICLE1))
         (subsumesContentInstance ?BOOK ?ARTICLE2))))

(subclass Certificate Text)
(documentation Certificate "A &%Text that confers a right or obligation
on the holder of the &%Certificate.  Note that the right or obligation
need not be a legal one, as in the case of an academic diploma that grants
certain privileges in the professional world.")

(=>
   (and
      (instance ?DOC Certificate)
      (possesses ?AGENT ?DOC))
   (exists (?PROC)
      (or
         (confersRight ?PROC ?DOC ?AGENT)
         (confersObligation ?PROC ?DOC ?AGENT))))

(subclass FinancialInstrument Certificate)
(documentation FinancialInstrument "A document having monetary value
or recording a monetary transaction")

(subclass Currency FinancialInstrument)
(partition Currency CurrencyBill CurrencyCoin)
(documentation Currency "Any element of the official currency of some
&%Nation.  This covers both &%CurrencyBills and &%CurrencyCoins.")

(=>
   (instance ?CURRENCY Currency)
   (exists (?MEASURE)
      (monetaryValue ?CURRENCY ?MEASURE)))

(subclass CurrencyBill Currency)
(documentation CurrencyBill "Any instance of &%Currency that is made
of paper.")

(=>
   (instance ?BILL CurrencyBill)
   (exists (?PAPER)
      (and
         (instance ?PAPER Paper)
         (part ?PAPER ?BILL))))

(subclass CurrencyCoin Currency)
(documentation CurrencyCoin "Any instance of &%Currency that is made
of &%Metal.")

(=>
   (instance ?COIN CurrencyCoin)
   (exists (?METAL)
      (and
         (subclass ?METAL Metal)
         (material ?METAL ?COIN))))

(subclass Patent Certificate)
(documentation Patent "A &%Certificate that expresses the content of an
invention that has been accorded legal protection by a governemental
entity.")

(subclass Molecule CompoundSubstance)
(documentation Molecule "A molecule is the smallest unit of matter of a
&%CompoundSubstance that retains all the physical and chemical properties
of that substance, e.g., Ne, H2, H2O.  A molecule is two or more &%Atoms
linked by a chemical bond.")

(=>
   (instance ?MOLE Molecule)
   (exists (?ATOM1 ?ATOM2)
      (and
         (instance ?ATOM1 Atom)
         (instance ?ATOM2 Atom)
         (part ?ATOM1 ?MOLE)
         (part ?ATOM2 ?MOLE)
         (not
            (equal ?ATOM1 ?ATOM2)))))

(subclass Artifact CorpuscularObject)
(documentation Artifact "A &%CorpuscularObject that is the product of a
&%Making.")

(<=>
   (instance ?ARTIFACT Artifact)
   (exists (?MAKING)
      (and
         (instance ?MAKING Making)
         (result ?MAKING ?ARTIFACT))))

(subclass Product Artifact)
(documentation Product "An &%Artifact that is produced by &%Manufacture and
that is intended to be sold.")

(=>
   (instance ?PRODUCT Product)
   (exists (?MANUFACTURE)
      (and
         (instance ?MANUFACTURE Manufacture)
         (result ?MANUFACTURE ?PRODUCT))))

(instance version BinaryPredicate)
(instance version AsymmetricRelation)
(instance version TransitiveRelation)
(domainSubclass version 1 Artifact)
(domainSubclass version 2 Artifact)
(documentation version "Some &%Artifacts have a life cycle with discrete
stages or versions.  (&%version ARTIFACT1 ARTIFACT2) means that ARTIFACT1
is a version of ARTIFACT2.  Note that this &%Predicate relates subclasses of
&%Artifact and not instances.")

(=>
    (version ?ARTIFACT1 ?ARTIFACT2)
    (subclass ?ARTIFACT1 ?ARTIFACT2))

;; The following part of the ontology will eventually encompass all
;; artifacts.  For the time being, it is mostly restricted to the content
;; of the Ontolingua ontology component-assemblies, which covers the types
;; of elements used to construct engineering systems.

(subclass StationaryArtifact Artifact)
(documentation StationaryArtifact "A &%StationaryArtifact is an &%Artifact
that has a fixed spatial location.  Most instances of this &%Class are
architectural works, e.g. the Eiffel Tower, the Great Pyramids, office towers,
single-family houses, etc.")

(=>
    (instance ?ARTIFACT StationaryArtifact)
    (exists (?PLACE)
       (holdsDuring (WhenFn ?ARTIFACT) (located ?ARTIFACT ?PLACE))))

(subclass Building StationaryArtifact)
(documentation Building "The Class of &%StationaryArtifacts which are
intended to house &%Humans and their activities.")

(=>
   (instance ?BUILDING Building)
   (exists (?HUMAN)
      (and
         (instance ?HUMAN Human)
         (or
            (inhabits ?HUMAN ?BUILDING)
            (exists (?ACT)
               (and
                  (agent ?ACT ?HUMAN)
                  (located ?ACT ?BUILDING)))))))

(subclass Room StationaryArtifact)
(disjoint Room Building)
(documentation Room "A &%properPart of a &%Building which is separated from
the exterior of the &%Building and/or other &%Rooms of the &%Building by walls.
Some &%Rooms may have a specific purpose, e.g. sleeping, bathing, cooking,
entertainment, etc.")

(=>
   (instance ?ROOM Room)
   (exists (?BUILD)
      (and
         (instance ?BUILD Building)
         (properPart ?ROOM ?BUILD))))

(subclass Residence StationaryArtifact)
(partition Residence PermanentResidence TemporaryResidence)
(documentation Residence "A &%Building or part of a &%Building which provides
some accomodation for sleeping.")

(=>
   (instance ?RESIDENCE Residence)
   (or
      (instance ?RESIDENCE House)
      (exists (?BUILDING)
         (and
            (instance ?BUILDING ResidentialBuilding)
            (part ?RESIDENCE ?BUILDING)))))

(subclass PermanentResidence Residence)
(documentation PermanentResidence "A &%Residence where people live, i.e.
where people have a &%home.")

(=>
   (instance ?RESIDENCE PermanentResidence)
   (exists (?PERSON)
      (home ?PERSON ?HOTEL)))

(subclass TemporaryResidence Residence)
(documentation TemporaryResidence "A &%Residence which is strictly temporary,
i.e. where no one makes his/her &%home.")

(=>
   (instance ?RESIDENCE TemporaryResidence)
   (not (exists (?PERSON)
      (home ?PERSON ?HOTEL))))

(subclass ResidentialBuilding Building)
(subclass ResidentialBuilding Residence)
(documentation ResidentialBuilding "A &%Building which provides some
accomodation for sleeping.  Note that this class does not cover just
permanent residences, e.g. &%Houses and condominium and apartment buildings,
but also temporary residences, e.g. hotels and dormitories.
&%ResidentialBuildings are also distinguished from &%CommercialBuildings,
which are intended to serve an organizational rather than a residential
function.")

(subclass Hotel ResidentialBuilding)
(subclass Hotel TemporaryResidence)
(subclass Hotel CommercialAgent)
(documentation Hotel "A &%ResidentialBuilding which provides temporary
accommodations to guests in exchange for money.")

(subclass SingleFamilyResidence PermanentResidence)
(documentation SingleFamilyResidence "A &%PermanentResidence which is
intended to be the &%home of a single &%SocialUnit.  This class covers
&%Houses, &%ApartmentUnits, and &%CondominiumUnits.")

(=>
   (instance ?RESIDENCE SingleFamilyResidence)
   (hasPurpose ?RESIDENCE (forall (?AGENT1 ?AGENT2)
                         (=>
                            (and
                               (home ?AGENT1 ?RESIDENCE)
                               (home ?AGENT2 ?RESIDENCE))
                            (exists (?UNIT)
                               (and
                                  (instance ?UNIT SocialUnit)
                                  (member ?AGENT1 ?UNIT)
                                  (member ?AGENT2 ?UNIT)))))))

(subclass ArtWork Artifact)
(documentation ArtWork "&%Artifacts that are created primarily for
aesthetic appreciation.  Note that this &%Class does not include
most examples of architecture, which belong under &%StationaryArtifact.")

(subclass RepresentationalArtWork ArtWork)
(subclass RepresentationalArtWork Icon)
(documentation RepresentationalArtWork "Any &%ArtWork that represents
something &%Physical.")

(subclass Fabric Artifact)
(disjoint Fabric StationaryArtifact)
(documentation Fabric "&%Artifacts that are created by weaving together
natural or synthetic fibers or by treating the skins of certain sorts of
&%Animals.  Note that this &%Class includes articles that are created by
stitching together various types of fabrics, e.g. bedspreads.  On the other
hand, &%Clothing is not a &%subclass of &%Fabric, because many clothing items
contain elements that are not fabrics.")

(subclass Clothing Artifact)
(disjoint Clothing StationaryArtifact)
(documentation Clothing "&%Artifact made out of fabrics and possibly other
materials that are used to cover the bodies of &%Humans.")

(=>
   (instance ?CLOTHING Clothing)
   (exists (?FABRIC)
      (and
         (instance ?FABRIC Fabric)
         (part ?FABRIC ?CLOTHING))))

(instance wears BinaryPredicate)
(domain wears 1 Animal)
(domain wears 2 Clothing)
(documentation wears "(&%wears ?AGENT ?CLOTHING) means that ?AGENT is wearing the item of &%Clothing ?CLOTHING.")

(=>
   (wears ?AGENT ?CLOTHING)
   (located ?CLOTHING ?AGENT))

(subclass Device Artifact)
(documentation Device "A &%Device is an &%Artifact whose purpose is to
serve as an &%instrument in a specific subclass of &%Process.")

(=>
   (instance ?DEVICE Device)
   (exists (?PROC)
      (capability ?PROC instrument ?DEVICE)))

(=>
   (instance ?DEVICE Device)
   (exists (?PROC)
      (hasPurpose ?DEVICE (exists (?INST)
                             (and
                                (instance ?INST ?PROC)
                                (instrument ?INST ?DEVICE))))))

(subclass MusicalInstrument Device)
(documentation MusicalInstrument "A &%Device which is manipulated by a &%Human and whose purpose is to produce &%Music.")

(=>
   (instance ?INSTRUMENT MusicalInstrument)
   (capability Music instrument ?INSTRUMENT))

(subclass TransportationDevice Device)
(documentation TransportationDevice "A &%TransportationDevice is a &%Device
which serves as the &%instrument in a &%Transportation &%Process which carries
the &%patient of the &%Process from one point to another.")

(=>
   (instance ?DEVICE TransportationDevice)
   (capability Transportation instrument ?DEVICE))

(subclass Vehicle TransportationDevice)
(documentation Vehicle "&%Vehicle is the subclass of
&%TransportationDevices that transport passengers or goods
from one place to another by moving from one place to the other
with them, e.g., cars, trucks, ferries, and airplanes.  Contrast
with devices such as &%Pipelines, escalators, or supermarket
checkout belts, which carry items from one place to another by means
of a moving part, without the device removing from the origin to
the destination.")

(=>
	(and
		(instance ?TRANSPORT Vehicle)
		(instance ?MOVE Translocation)
		(instrument ?MOVE ?TRANSPORT)
		(origin ?MOVE ?FROM))
	(holdsDuring (BeginFn (WhenFn ?MOVE)) (located ?TRANSPORT ?FROM)))

(=>
	(and
		(instance ?TRANSPORT Vehicle)
		(instance ?MOVE Translocation)
		(instrument ?MOVE ?TRANSPORT)
		(destination ?MOVE ?TO))
	(holdsDuring (BeginFn (WhenFn ?MOVE)) (located ?TRANSPORT ?TO)))

(subclass MeasuringDevice Device)
(documentation MeasuringDevice "Any &%Device whose purpose is to measure a
&%PhysicalQuantity.")

(=>
   (instance ?DEVICE MeasuringDevice)
   (hasPurpose ?DEVICE (exists (?MEASURE)
                           (and
                               (instance ?MEASURE Measuring)
                               (instrument ?MEASURE ?DEVICE)))))

(subclass AttachingDevice Device)
(documentation AttachingDevice "A &%Device whose purpose is to attach one thing
to something else, e.g. nails, screws, buttons, etc.")

(=>
   (instance ?DEVICE AttachingDevice)
   (exists (?ATTACH)
      (and
         (instance ?ATTACH Attaching)
         (instrument ?ATTACH ?DEVICE))))

(subclass Weapon Device)
(documentation Weapon "The &%Class of &%Devices that are designed
primarily to damage or destroy &%Humans/&%Animals, &%StationaryArtifacts or
the places inhabited by &%Humans/&%Animals.")

(=>
   (instance ?WEAPON Weapon)
   (capability Damaging instrument ?WEAPON))

(=>
   (instance ?WEAPON Weapon)
   (hasPurpose ?WEAPON (exists (?DEST ?PATIENT)
                          (and
	 			     (instance ?DEST Damaging)
                             (patient ?DEST ?PATIENT)
                             (or
                                (instance ?PATIENT StationaryArtifact)
                                (instance ?PATIENT Animal)
                                (exists (?ANIMAL)
                                   (and
                                      (instance ?ANIMAL Animal)
                                      (inhabits ?ANIMAL ?PATIENT))))))))

(subclass Machine Device)
(documentation Machine "&%Machines are &%Devices that that have a
well-defined &%resource and &%result and that automatically convert
the &%resource into the &%result.")

(=>
   (instance ?MACHINE Machine)
   (forall (?PROC)
      (=>
         (instrument ?PROC ?MACHINE)
         (exists (?RESOURCE ?RESULT)
            (and
               (resource ?PROC ?RESOURCE)
               (result ?PROC ?RESULT))))))

(=>
   (instance ?MACHINE Machine)
   (exists (?COMP1 ?COMP2)
      (and
         (instance ?COMP1 EngineeringComponent)
         (instance ?COMP2 EngineeringComponent)
         (not (equal ?COMP1 ?COMP2))
         (part ?COMP1 ?MACHINE)
         (part ?COMP2 ?MACHINE))))

(subclass EngineeringComponent Device)
(documentation EngineeringComponent "A fundamental concept that applies
in many engineering domains.  An &%EngineeringComponent is an element of
a &%Device that is a physically whole object, such as one might
see listed as standard parts in a catalog.  The main difference betweeen
&%EngineeringComponents and arbitrary globs of matter is that
&%EngineeringComponents are object-like in a modeling sense.  Thus, an
&%EngineeringComponent is not an arbtrary subregion, but a part of a
system with a stable identity.")

(=>
   (instance ?COMP EngineeringComponent)
   (exists (?DEVICE)
      (and
         (instance ?DEVICE Device)
         (component ?COMP ?DEVICE))))

(subrelation engineeringSubcomponent properPart)
(domain engineeringSubcomponent 1 EngineeringComponent)
(domain engineeringSubcomponent 2 EngineeringComponent)
(documentation engineeringSubcomponent "(&%engineeringSubcomponent ?SUB
?SUPER) means that the &%EngineeringComponent ?SUB is structurally a
&%properPart ?SUPER.  This relation is an &%AsymmetricRelation, since
two &%EngineeringComponents cannot be subcomponents of each other.")

(instance connectedEngineeringComponents SymmetricRelation)
(instance connectedEngineeringComponents IrreflexiveRelation)
(subrelation connectedEngineeringComponents connected)
(domain connectedEngineeringComponents 1 EngineeringComponent)
(domain connectedEngineeringComponents 2 EngineeringComponent)
(documentation connectedEngineeringComponents "This is the most general
connection relation between &%EngineeringComponents.  If
(&%connectedEngineeringComponents ?COMP1 ?COMP2), then neither ?COMP1 nor
?COMP2 can be an &%engineeringSubcomponent of the other.  The relation
&%connectedEngineeringComponents is a &%SymmetricRelation; there is no
information in the direction of connection between two components.  It is
also an &%IrreflexiveRelation; no &%EngineeringComponent bears this relation
to itself.  Note that this relation does not associate a name or type
with the connection.")

(=>
   (connectedEngineeringComponents ?COMP1 ?COMP2)
   (and
      (not
         (engineeringSubcomponent ?COMP1 ?COMP2))
      (not
         (engineeringSubcomponent ?COMP2 ?COMP1))))

(=>
   (connectedEngineeringComponents ?COMP1 ?COMP2)
   (not
      (or
         (instance ?COMP1 EngineeringConnection)
         (instance ?COMP2 EngineeringConnection))))

(<=>
   (connectedEngineeringComponents ?COMP1 ?COMP2)
   (exists (?CONNECTION)
      (connectsEngineeringComponents ?CONNECTION ?COMP1 ?COMP2)))

(subclass EngineeringConnection EngineeringComponent)
(documentation EngineeringConnection "An &%EngineeringConnection is an
&%EngineeringComponent that represents a connection relationship between
two other &%EngineeringComponents.  It is a reification of the
&%Predicate &%connectedEngineeringComponents.  That means that whenever
this &%Predicate holds between two &%EngineeringComponents, there exists an
&%EngineeringConnection.  The practical reason for reifying a relationship
is to be able to attach other information about it. For example, one
might want to say that a particular connection is associated with some
shared parameters, or that it is of a particular type.
&%EngineeringConnections are &%EngineeringComponents and can therefore be
an &%engineeringSubcomponent of other &%EngineeringComponents.  However,
to provide for modular regularity in component systems,
&%EngineeringConnections cannot be connected.  For each pair of
&%EngineeringComponents related by &%connectedEngineeringComponents, there
exists at least one &%EngineeringConnection.  However, that object may not
be unique, and the same &%EngineeringConnection may be associated with
several pairs of &%EngineeringComponents.")

(=>
   (instance ?CONNECTION EngineeringConnection)
   (exists (?COMP1 ?COMP2)
      (connectsEngineeringComponents ?CONNECTION ?COMP1 ?COMP2)))

(subrelation connectsEngineeringComponents connects)
(domain connectsEngineeringComponents 1 EngineeringConnection)
(domain connectsEngineeringComponents 2 EngineeringComponent)
(domain connectsEngineeringComponents 3 EngineeringComponent)
(documentation connectsEngineeringComponents "&%connectsEngineeringComponents
is a &%TernaryPredicate that maps from an &%EngineeringConnection to the
&%EngineeringComponents it connects.  Since &%EngineeringComponents cannot
be connected to themselves and there cannot be an &%EngineeringConnection
without a &%connectedEngineeringComponents &%Predicate, the second and third
arguments of any &%connectsEngineeringComponents relationship will always be
distinct for any given first argument.")

;; This following part contains definitions and axioms relating to social
;; groups and the relations between them.

(subclass CommercialAgent Agent)
(documentation CommercialAgent "An &%Agent that provides products and/or
services for a fee with the aim of making a profit.")

(subclass Corporation CommercialAgent)
(subclass Corporation Organization)
(documentation Corporation "An &%Organization that has a special legal status
that allows a group of persons to act as a &%CommercialAgent and that insulates
the owners (shareholders) from many liabilities that might result from the
corporation's operation.")

(subclass Manufacturer Corporation)
(documentation Manufacturer "Any &%Corporation which manufactures &%Products.")

(=>
   (instance ?MANUFACTURER Manufacturer)
   (exists (?PLANT)
      (and
         (instance ?PLANT IndustrialPlant)
         (possesses ?MANUFACTURER ?PLANT))))

(=>
   (instance ?ORG Manufacturer)
   (hasPurpose (exists (?MANUFACTURE)
                  (and
                     (instance ?MANUFACTURE Manufacturing)
                     (instance ?MANUFACTURE CommercialService)
                     (agent ?MANUFACTURE ?ORG)))))

(subclass MercantileOrganization Corporation)
(documentation MercantileOrganization "Any &%Corporation which sells
physical goods to customers for a profit.")

(=>
   (instance ?ORG MercantileOrganization)
   (hasPurpose (exists (?SELL)
                  (and
                     (instance ?SELL Selling)
                     (instance ?SELL CommercialService)
                     (agent ?SELL ?ORG)))))

(subclass Group Collection)
(subclass Group Agent)
(documentation Group "A &%Collection of &%Agents, e.g. a flock
of sheep, a herd of goats, or the local Boy Scout troop.")

(=>
   (and
      (instance ?GROUP Group)
      (member ?MEMB ?GROUP))
   (instance ?MEMB Agent))

(subclass GroupOfPeople Group)
(subclass GroupOfPeople AgentGroup)
(documentation GroupOfPeople "Any &%Group whose &%members are
exclusively &%Humans.")

(=>
   (and
      (instance ?GROUP GroupOfPeople)
      (member ?MEMBER ?GROUP))
   (instance ?MEMBER Human))

(subclass AgeGroup Group)
(documentation AgeGroup "A &%Group whose &%members all have the same &%age.")

(=>
   (instance ?GROUP AgeGroup)
   (forall (?MEMB1 ?MEMB2 ?AGE1 ?AGE2)
      (=>
         (and
            (member ?MEMB1 ?GROUP)
            (member ?MEMB2 ?GROUP)
            (age ?MEMB1 ?AGE1)
            (age ?MEMB2 ?AGE2))
         (equal ?AGE1 ?AGE2))))

(subclass FamilyGroup Group)
(documentation FamilyGroup "A &%Group whose &%members bear
&%familyRelations to one another.")

(=>
   (instance ?GROUP FamilyGroup)
   (forall (?MEMB1 ?MEMB2)
      (=>
         (and
            (member ?MEMB1 ?GROUP)
            (member ?MEMB2 ?GROUP))
         (familyRelation ?MEMB1 ?MEMB2))))

(subclass SocialUnit Group)
(documentation SocialUnit "A group of people who all have the same &%home.")

(=>
   (instance ?UNIT SocialUnit)
   (exists (?HOME)
      (=>
         (member ?MEMBER ?UNIT)
         (home ?MEMBER ?HOME))))

(instance ImmediateFamilyFn UnaryFunction)
(domain ImmediateFamilyFn 1 Human)
(range ImmediateFamilyFn FamilyGroup)
(documentation ImmediateFamilyFn "(&%ImmediateFamilyFn ?PERSON) denotes the
immediate family of ?PERSON, i.e. the &%Group consisting of the &%parents of
?PERSON and anyone of whom ?PERSON is a &%parent.")

(=>
   (and
      (instance ?PERSON Human)
      (equal (ImmediateFamilyFn ?PERSON) ?FAMILY))
   (forall (?MEMBER)
      (<=>
         (member ?MEMBER ?FAMILY)
         (or
            (parent ?MEMBER ?PERSON)
            (parent ?PERSON ?MEMBER)))))

(instance familyRelation BinaryPredicate)
(instance familyRelation EquivalenceRelation)
(domain familyRelation 1 Organism)
(domain familyRelation 2 Organism)
(documentation familyRelation "A very general &%Predicate for biological
relationships. (&%familyRelation ?ORGANISM1 ?ORGANISM2) means that
?ORGANISM1 and ?ORGANISM2 are biologically derived from a common ancestor.")

(=>
   (familyRelation ?ORGANISM1 ?ORGANISM2)
   (exists (?ORGANISM3)
      (and
         (familyRelation ?ORGANISM3 ?ORGANISM1)
         (familyRelation ?ORGANISM3 ?ORGANISM2))))

(instance legalRelation BinaryPredicate)
(instance legalRelation SymmetricRelation)
(domain legalRelation 1 CognitiveAgent)
(domain legalRelation 2 CognitiveAgent)
(documentation legalRelation "(&%legalRelation ?AGENT1 ?AGENT2) means
there is a relationship involving legal obligations between ?AGENT1 and
?AGENT2.  Some examples include marriage, adoption, etc.")

(=>
   (legalRelation ?AGENT1 ?AGENT2)
   (exists (?DECLARE ?OBLIGATION)
      (and
         (instance ?DECLARE Declaring)
         (confersObligation ?OBLIGATION ?DECLARE ?AGENT1)
         (confersObligation ?OBLIGATION ?DECLARE ?AGENT2))))

(subrelation spouse legalRelation)
(instance spouse IrreflexiveRelation)
(instance spouse SymmetricRelation)
(domain spouse 1 Human)
(domain spouse 2 Human)
(documentation spouse "The relationship of marriage between two &%Humans.")

(subrelation husband spouse)
(instance husband AsymmetricRelation)
(instance husband IrreflexiveRelation)
(domain husband 1 Man)
(domain husband 2 Woman)
(inverse husband wife)
(documentation husband "(&%husband ?MAN ?WOMAN) means that ?MAN is the
husband of ?WOMAN.")

(subrelation wife spouse)
(instance wife AsymmetricRelation)
(instance wife IrreflexiveRelation)
(domain wife 1 Woman)
(domain wife 2 Man)
(documentation wife "(&%wife ?WOMAN ?MAN) means that ?WOMAN is the wife of
?MAN.")

(subclass EthnicGroup Group)
(documentation EthnicGroup "A &%Group whose &%members originate from
the same &%GeographicArea or share the same &%Language and/or cultural
practices.")

(subclass BeliefGroup Group)
(documentation BeliefGroup "A &%Group whose &%members share a belief or
set of beliefs.")

(=>
   (instance ?GROUP BeliefGroup)
   (exists (?BELIEF)
      (forall (?MEMB)
         (=>
            (member ?MEMB ?GROUP)
            (believes ?MEMB ?BELIEF)))))

(subclass Organization Group)
(subclass Organization CognitiveAgent)
(documentation Organization "An &%Organization is a corporate or similar
institution. The &%members of an &%Organization typically have a common
purpose or function. Note that this class also covers divisions, departments,
etc. of organizations.  For example, both the Shell Corporation and the
accounting department at Shell would both be instances of &%Organization.
Note too that the existence of an &%Organization is dependent on the existence
of at least one &%member (since &%Organization is a subclass of &%Collection).
Accordingly, in cases of purely legal organizations, a fictitious &%member
should be assumed.")

(=>
   (instance ?ORG Organization)
   (exists (?PURP)
      (forall (?MEMBER)
         (=>
            (member ?MEMBER ?ORG)
            (hasPurpose ?MEMBER ?PURP)))))

(=>
   (and
      (instance ?ORG Organization)
      (member ?AGENT ?ORG))
   (instance ?AGENT Agent))

(instance employs BinaryPredicate)
(domain employs 1 Organization)
(domain employs 2 CognitiveAgent)
(documentation employs "(&%employs ?ORG ?PERSON) means that ?ORG has
hired ?PERSON and currently retains ?PERSON, on a salaried or
contractual basis, to provide services in exchange for monetary
compensation.")

(=>
   (employs ?ORG ?PERSON)
   (member ?PERSON ?ORG))

(subclass PoliticalOrganization Organization)
(documentation PoliticalOrganization "An &%Organization that is a &%Government,
a &%subOrganization of a &%Government, or an &%Organization that is attempting
to bring about some sort of political change.")

(=>
   (instance ?POL PoliticalOrganization)
   (exists (?PROC)
      (and
         (instance ?PROC PoliticalProcess)
         (agent ?PROC ?POL))))

(subclass GovernmentOrganization Organization)
(documentation GovernmentOrganization "&%GovernmentOrganization is the
class of official &%Organizations that are concerned with the government
of a &%GeopoliticalArea at some level.  They may be a &%Government or the
&%subOrganization of a government.")

(<=>
   (instance ?ORG GovernmentOrganization)
      (or
	   (instance ?ORG Government)
	   (exists (?GOV)
	      (and
		   (instance ?GOV Government)
		   (subOrganization ?ORG ?GOV)))))

(subclass Government GovernmentOrganization)
(documentation Government "The ruling body of a &%GeopoliticalArea.")

(instance GovernmentFn UnaryFunction)
(domain GovernmentFn 1 GeopoliticalArea)
(range GovernmentFn Government)
(documentation GovernmentFn "(&%GovernmentFn ?AREA) denotes the
&%Government of the &%GeopoliticalArea ?AREA.  For example,
(&%GovernmentFn &%UnitedStates) denotes the Federal-level government of
the United States; (&%GovernmentFn &%PuertoRico) denotes the government of
the Commonwealth of Puerto Rico.")

(<=>
	(instance ?COUNTRY Nation)
	(instance (GovernmentFn ?COUNTRY) NationalGovernment))

(subclass MilitaryOrganization GovernmentOrganization)
(documentation MilitaryOrganization "Any heavily armed &%Organization
that is part of a &%Government and that is charged with representing the
&%Government in international conflicts.")

(subclass PoliceOrganization GovernmentOrganization)
(documentation PoliceOrganization "Any &%GovernmentOrganization
that is charged with domestic enforcement of the laws of the &%Government.")

(subclass JudicialOrganization Organization)
(documentation JudicialOrganization "&%JudicialOrganization is the class
of &%Organizations whose primary purpose is to render judgments according
to the statutes or regulations of a government or other organization.
Judicial bodies are not necessarily government organizations, for example,
those associated with sporting associations.")

(=>
	(and
		(instance ?ORG JudicialOrganization)
		(subOrganization ?ORG ?GOV)
		(instance ?GOV GovernmentOrganization))
	(instance ?ORG GovernmentOrganization))

(subclass EducationalOrganization Organization)
(documentation EducationalOrganization "A &%EducationalOrganization is
an institution of learning. Some examples are public and private K-12
schools, and colleges and universities.")

(subclass ReligiousOrganization Organization)
(subclass ReligiousOrganization BeliefGroup)
(documentation ReligiousOrganization "An &%Organization whose members
share a set of religious beliefs.")

(subrelation subOrganization subCollection)
(instance subOrganization IrreflexiveRelation)
(instance subOrganization TransitiveRelation)
(domain subOrganization 1 Organization)
(domain subOrganization 2 Organization)
(documentation subOrganization "(&%subOrganization ?ORG1 ?ORG2) means
that ?ORG1 is an &%Organization which is a proper part of the
&%Organization ?ORG2.")

(instance citizen BinaryPredicate)
(instance citizen AsymmetricRelation)
(domain citizen 1 Human)
(domain citizen 2 Nation)
(documentation citizen "(&%citizen ?PERSON ?NATION) means that the
&%Human ?PERSON is a citizen of &%Nation ?NATION.")

;; END FILE

;; <module>QUALITIES</module>
;; BEGIN FILE


;;;;;;;;;;;;;;;;;;;
;;   QUALITIES   ;;
;;;;;;;;;;;;;;;;;;;

;; INCLUDES 'MEREOTOPOLOGY'
;; INCLUDES 'PROCESSES'
;; INCLUDES 'OBJECTS'

(subclass FieldOfStudy Proposition)
(documentation FieldOfStudy "An academic or applied discipline with
recognized experts and with a core of accepted theory or practice.  Note
that &%FieldOfStudy is a &%subclass of &%Proposition, because a
&%FieldOfStudy is understood to be a body of abstract, informational
content, with varying degrees of certainty attached to each element of
this content.")

(subclass Procedure Proposition)
(subclass Procedure Specification)
(documentation Procedure "A sequence-dependent specification.  Some
examples are &%ComputerPrograms, finite-state machines, cooking recipes,
musical scores, conference schedules, driving directions, and the scripts
of plays and movies.")

(subclass ComputerProgram Procedure)
(documentation ComputerProgram "A set of instructions in a computer
programming language that can be executed by a computer.")

(subclass Plan Procedure)
(documentation Plan "A specification of a sequence of &%Processes which
is intended to satisfy a specified purpose at some future time.")

(=>
    (and
       (instance ?PLAN Plan)
       (instance ?OBJ ContentBearingObject)
       (containsInformation ?OBJ ?PLAN))
    (exists (?PLANNING)
       (and
          (instance ?PLANNING Planning)
          (result ?PLANNING ?OBJ))))

(=>
    (instance ?PLAN Plan)
    (exists (?PURP)
        (hasPurpose ?PLAN ?PURP)))

(subclass Argument Assertion)
(documentation Argument "Any proposition which has the form of a deductive
or inductive argument, i.e. a set of premises which, it is claimed, imply
a conclusion.")

(=>
   (instance ?REASON Reasoning)
   (exists (?ARGUMENT)
      (and
         (instance ?ARGUMENT Argument)
         (realization ?REASON ?ARGUMENT))))

(=>
   (instance ?ARGUMENT Argument)
   (exists (?PREMISES ?CONCLUSION)
      (and
         (equal (PremisesFn ?ARGUMENT) ?PREMISES)
         (conclusion ?CONCLUSION ?ARGUMENT))))

(subclass DeductiveArgument Argument)
(partition DeductiveArgument ValidDeductiveArgument InvalidDeductiveArgument)
(documentation DeductiveArgument "An &%Argument which has the form of a
deduction, i.e. it is claimed that the set of &%premises &%entails the
&%conclusion.")

(subclass ValidDeductiveArgument DeductiveArgument)
(documentation ValidDeductiveArgument "A &%DeductiveArgument which is
valid, i.e. the set of &%premises in fact &%entails the &%conclusion.")

(=>
   (and
      (instance ?ARGUMENT ValidDeductiveArgument)
      (equal ?PREMISES (PremisesFn ?ARGUMENT))
      (conclusion ?CONCLUSION ?ARGUMENT))
   (exists (?FORMULA1 ?FORMULA2)
      (and
         (containsInformation ?FORMULA1 ?PREMISES)
         (containsInformation ?FORMULA2 ?CONCLUSION)
         (entails ?PREMISES ?CONCLUSION))))

(subclass InvalidDeductiveArgument DeductiveArgument)
(documentation InvalidDeductiveArgument "&%DeductiveArguments that are not
&%ValidDeductiveArguments, i.e. it is not the case that the set of &%premises
in fact &%entails the &%conclusion.")

(subclass Explanation DeductiveArgument)
(documentation Explanation "An &%Argument where the conclusion is an
observed fact and the premises are other facts which collectively imply
the conclusion.  Note that this is the they hypothetico-deductive model
of explanation.")

(instance premise BinaryPredicate)
(instance premise TotalValuedRelation)
(domain premise 1 Argument)
(domain premise 2 Proposition)
(documentation premise "(&%premise ?ARGUMENT ?PROPOSITION) means that the
&%Proposition ?PROPOSITION is an explicit assumption of the &%Argument
?ARGUMENT.")

(instance PremisesFn UnaryFunction)
(domain PremisesFn 1 Argument)
(range PremisesFn Proposition)
(documentation PremisesFn "(&%PremisesFn ?ARGUMENT) returns the complete
set of &%premises of the &%Argument ?ARGUMENT.")

(=>
   (and
      (instance ?ARGUMENT Argument)
      (equal ?PREMISES (PremisesFn ?ARGUMENT)))
   (<=>
      (subProposition ?PROPOSITION ?PREMISES)
      (premise ?ARGUMENT ?PROPOSITION)))

(instance conclusion BinaryPredicate)
(instance conclusion SingleValuedRelation)
(instance conclusion TotalValuedRelation)
(domain conclusion 1 Argument)
(domain conclusion 2 Proposition)
(documentation conclusion "(&%conclusion ?ARGUMENT ?PROPOSITION) means that
the &%Proposition ?PROPOSITION is the conclusion explicitly drawn from the
&%Argument ?ARGUMENT.  Note that it may or may not be the case that ?ARGUMENT
&%entails ?PROPOSITION.")

(instance consistent BinaryPredicate)
(instance consistent SymmetricRelation)
(domain consistent 1 Proposition)
(domain consistent 2 Proposition)
(documentation consistent "(&%consistent ?PROP1 ?PROP2) means that the two
&%Propositions ?PROP1 and ?PROP2 are consistent with one another, i.e. it
is possible for both of them to be true at the same time.")

(instance orientation SpatialRelation)
(instance orientation TernaryPredicate)
(domain orientation 1 Object)
(domain orientation 2 Object)
(domain orientation 3 PositionalAttribute)
(documentation orientation "A general &%Predicate for indicating how two
&%Objects are oriented with respect to one another.  For example,
(orientation ?OBJ1 ?OBJ2 North) means that ?OBJ1 is north of ?OBJ2, and
(orientation ?OBJ1 ?OBJ2 Vertical) means that ?OBJ1 is positioned
vertically with respect to ?OBJ2.")

(=>
   (and
      (orientation ?OBJ1 ?OBJ2 ?ATTR1)
      (contraryAttribute @ROW)
      (inList ?ATTR1 (ListFn @ROW))
      (inList ?ATTR2 (ListFn @ROW))
      (not (equal ?ATTR1 ?ATTR2)))
   (not
      (orientation ?OBJ1 ?OBJ2 ?ATTR2)))

(instance direction CaseRole)
(domain direction 1 Process)
(domain direction 2 DirectionalAttribute)
(documentation direction "(&%direction ?PROC ?ATTR) means that the
&%Process ?PROC is moving in the direction ?ATTR.  For example, one
would use this &%Predicate to represent the fact that Max is moving
&%North.")

(=>
   (holdsDuring ?TIME (direction ?PROC ?ATTR1))
   (forall (?ATTR2)
      (=>
         (holdsDuring ?TIME (direction ?PROC ?ATTR2))
         (equal ?ATTR2 ?ATTR1))))

(instance faces BinaryPredicate)
(domain faces 1 Object)
(domain faces 2 DirectionalAttribute)
(documentation faces "(&%faces ?OBJ ?DIRECTION) means that the front of
?OBJ (see &%FrontFn) is positioned towards the compass direction ?DIRECTION.
More precisely, it means that if a line were extended from the center of
?DIRECTION, the line would intersect with the front of ?OBJ before it
intersected with its back (see &%BackFn).")

(=>
   (holdsDuring ?TIME (faces ?PROC ?ATTR1))
   (forall (?ATTR2)
      (=>
         (holdsDuring ?TIME (faces ?PROC ?ATTR2))
         (equal ?ATTR2 ?ATTR1))))

(subclass TruthValue RelationalAttribute)
(documentation TruthValue "The &%Class of truth values, e.g. &%True and
&%False.  These are &%Attributes of &%Sentences and &%Propositions.")

(=>
   (and
      (property ?ITEM ?VALUE)
      (instance ?VALUE TruthValue))
   (or
      (instance ?ITEM Sentence)
      (instance ?ITEM Proposition)))

(instance True TruthValue)
(documentation True "The &%TruthValue of being true.")

(instance False TruthValue)
(contraryAttribute False True)
(documentation False "The &%TruthValue of being false.")

(instance Likely TruthValue)
(contraryAttribute Likely Unlikely)
(documentation Likely "The &%TruthValue of being probable, i.e. more likely than
not to be &%True.")

(=>
   (property ?FORMULA Likely)
   (greaterThan (ProbabilityFn (true ?FORMULA True)) (ProbabilityFn (true ?FORMULA False))))

(instance Unlikely TruthValue)
(documentation Unlikely "The &%TruthValue of being improbable, i.e. more likely
than not to be &%False.")

(=>
   (property ?FORMULA Unlikely)
   (greaterThan (ProbabilityFn (true ?FORMULA False)) (ProbabilityFn (true ?FORMULA True))))

(subrelation true property)
(domain true 1 Sentence)
(domain true 2 TruthValue)
(documentation true "The &%BinaryPredicate that relates a &%Sentence
to its &%TruthValue.")

(subclass PositionalAttribute RelationalAttribute)
(documentation PositionalAttribute "&%Attributes characterizing the
orientation of an &%Object, e.g. &%Vertical versus &%Horizontal, &%Left
versus &%Right etc.")

(subclass DirectionalAttribute PositionalAttribute)
(documentation DirectionalAttribute "The subclass of &%PositionalAttributes
that concern compass directions.")

(=>
   (and
      (orientation ?OBJ1 ?OBJ2 ?ATTR1)
      (instance ?ATTR1 DirectionalAttribute)
      (instance ?ATTR2 DirectionalAttribute)
      (not
         (equal ?ATTR1 ?ATTR2)))
   (not
      (orientation ?OBJ1 ?OBJ2 ?ATTR2)))

(=>
	(and
		(instance ?DIRECT DirectionalAttribute)
		(orientation ?OBJ1 ?OBJ2 ?DIRECT)
		(orientation ?OBJ2 ?OBJ3 ?DIRECT))
	(between ?OBJ1 ?OBJ2 ?OBJ33))

(instance North DirectionalAttribute)
(contraryAttribute North South East West)
(documentation North "The compass direction of &%North.")

(instance South DirectionalAttribute)
(documentation South "The compass direction of &%South.")

(<=>
   (orientation ?OBJ1 ?OBJ2 North)
   (orientation ?OBJ2 ?OBJ1 South))

(instance East DirectionalAttribute)
(documentation East "The compass direction of &%East.")

(instance West DirectionalAttribute)
(documentation West "The compass direction of &%West.")

(<=>
   (orientation ?OBJ1 ?OBJ2 East)
   (orientation ?OBJ2 ?OBJ1 West))

(instance Vertical PositionalAttribute)
(documentation Vertical "Attribute used to indicate that an &%Object
is positioned height-wise with respect to another &%Object.")

(<=>
   (orientation ?OBJ1 ?OBJ2 Vertical)
   (orientation ?OBJ2 ?OBJ1 Vertical))

(instance Horizontal PositionalAttribute)
(contraryAttribute Horizontal Vertical)
(documentation Horizontal "Attribute used to indicate that an &%Object
is positioned width-wise with respect to another &%Object.")

(<=>
   (orientation ?OBJ1 ?OBJ2 Horizontal)
   (orientation ?OBJ2 ?OBJ1 Horizontal))

(instance Above PositionalAttribute)
(contraryAttribute Above Below)
(documentation Above "This is a &%PositionalAttribute derived from the
up/down schema and not involving contact.  Note that this means directly
above, i.e., if one object is &%Above another object, then the projections
of the two objects overlap.")

(=>
   (orientation ?OBJ1 ?OBJ2 Above)
   (not
      (connected ?OBJ1 ?OBJ2)))

(instance Below PositionalAttribute)
(documentation Below "This &%PositionalAttribute is derived from the
up/down schema and may or may not involve contact.  Note that this means
directly below, i.e., if one object is &%Below another object, then the
projections of the two objects overlap.")

(<=>
   (orientation ?OBJ1 ?OBJ2 Below)
   (or
      (orientation ?OBJ2 ?OBJ1 On)
      (orientation ?OBJ2 ?OBJ1 Above)))

(instance Adjacent PositionalAttribute)
(documentation Adjacent "Used to assert that an object ?OBJ1 is close
to, near or abutting ?OBJ2.  This &%PositionalAttribute covers the
following common sense notions:  adjoins, abuts, is contiguous to,
is juxtaposed, and is close to.")

(<=>
   (orientation ?OBJ1 ?OBJ2 Adjacent)
   (or
      (orientation ?OBJ1 ?OBJ2 Near)
      (connected ?OBJ1 ?OBJ2)))

(instance Left PositionalAttribute)
(documentation Left "This &%PositionalAttribute is derived from the
left/right schema.  Note that this means directly to the left, so that,
if one object is to the left of another, then the projections of the
two objects overlap.")

(instance Right PositionalAttribute)
(contraryAttribute Right Left)
(documentation Right "This &%PositionalAttribute is derived from the
left/right schema.  Note that this means directly to the right, so that,
if one object is to the right of another, then the projections of the
two objects overlap.")

(<=>
   (orientation ?OBJ1 ?OBJ2 Right)
   (orientation ?OBJ2 ?OBJ1 Left))

(instance Near PositionalAttribute)
(documentation Near "The relation of common sense adjacency.  Note that, if
an object is &%Near another object, then the objects are not &%connected.")

(=>
   (orientation ?OBJ1 ?OBJ2 Near)
   (not
      (connected ?OBJ1 ?OBJ2)))

(=>
   (orientation ?OBJ1 ?OBJ2 Near)
   (orientation ?OBJ2 ?OBJ1 Near))

(instance On PositionalAttribute)
(documentation On "This is used to assert that an object is on top of
another object, and it is derived from the up/down schema and involves
contact.")

(=>
   (orientation ?OBJ1 ?OBJ2 On)
   (connected ?OBJ1 ?OBJ2))

(=>
   (orientation ?OBJ1 ?OBJ2 On)
   (located ?OBJ1 ?OBJ2))

(=>
   (orientation ?OBJ1 ?OBJ2 On)
   (not
      (orientation ?OBJ2 ?OBJ1 On)))

(subclass TimeZone RelationalAttribute)
(documentation TimeZone "An &%Attribute which is used to specify coordinates
in which time measures are uniform, i.e. all time devices are synchronized to
the same &%TimePositions.")

(instance CoordinatedUniversalTimeZone TimeZone)
(documentation CoordinatedUniversalTimeZone "A &%TimeZone which functions
as the standard time zone.  It is also known as Zulu time (in the military),
Greenwich Mean Time, and the Western European time zone.  Note that whenever
a &%TimeZone is not specified, the &%TimePosition is understood to be with
respect to the &%CoordinatedUniversalTimeZone.")

(instance PacificTimeZone TimeZone)
(documentation PacificTimeZone "A &%TimeZone that covers much of the
western part of the United States.")

(=>
   (equal (RelativeTimeFn ?TIME1 PacificTimeZone) ?TIME2)
   (equal ?TIME2 (AdditionFn ?TIME1 8)))

(instance MountainTimeZone TimeZone)
(documentation MountainTimeZone "A &%TimeZone that covers much of the
Rocky Mountain region of the United States.")

(=>
   (equal (RelativeTimeFn ?TIME1 MountainTimeZone) ?TIME2)
   (equal ?TIME2 (AdditionFn ?TIME1 7)))

(instance CentralTimeZone TimeZone)
(documentation CentralTimeZone "A &%TimeZone that covers much of the
midwestern United States.")

(=>
   (equal (RelativeTimeFn ?TIME1 CentralTimeZone) ?TIME2)
   (equal ?TIME2 (AdditionFn ?TIME1 6)))

(instance EasternTimeZone TimeZone)
(documentation EasternTimeZone "A &%TimeZone that covers much of the
eastern United States.")

(=>
   (equal (RelativeTimeFn ?TIME1 EasternTimeZone) ?TIME2)
   (equal ?TIME2 (AdditionFn ?TIME1 5)))

(instance RelativeTimeFn BinaryFunction)
(instance RelativeTimeFn TemporalRelation)
(instance RelativeTimeFn TotalValuedRelation)
(domain RelativeTimeFn 1 TimePosition)
(domain RelativeTimeFn 2 TimeZone)
(range RelativeTimeFn TimePosition)
(documentation RelativeTimeFn "A means of converting &%TimePositions
between different &%TimeZones.  (&%RelativeTimeFn ?TIME ?ZONE)
denotes the &%TimePosition in &%CoordinatedUniversalTime that is
contemporaneous with the &%TimePosition ?TIME in &%TimeZone ?ZONE.
For example, (&%RelativeTimeFn (&%MeasureFn 14 &%Hour) &%EasternTimeZone)
would return the value (&%MeasureFn 19 &%Hour).")

(subclass SocialRole RelationalAttribute)
(documentation SocialRole "The &%Class of all &%Attributes that
specify the position or status of a &%CognitiveAgent within an
&%Organization or other &%Group.")

(=>
   (and
      (attribute ?PERSON ?ATTRIBUTE)
      (instance ?ATTRIBUTE SocialRole))
   (instance ?PERSON Human))

(instance Unemployed SocialRole)
(documentation Unemployed "The &%Attribute of a &%CognitiveAgent when
he/she is unemployed.")

(<=>
   (forall (?ORG)
      (not (employs ?ORG ?PERSON)))
   (attribute ?PERSON Unemployed))

(subclass Position SocialRole)
(partition Position FullTimePosition PartTimePosition)
(documentation Position "A formal position of reponsibility within an &%Organization. Examples of &%Positions include president, laboratory director, senior researcher, sales representative, etc.")

(instance occupiesPosition TernaryPredicate)
(domain occupiesPosition 1 Human)
(domain occupiesPosition 2 Position)
(domain occupiesPosition 3 Organization)
(documentation occupiesPosition "(&%occupiesPosition ?PERSON ?POSITION ?ORG)
means that ?PERSON holds the &%Position ?POSITION at &%Organization ?ORG.
For example, (&%occupiesPosition &%TomSmith &%ResearchDirector
&%AcmeLaboratory) means that &%TomSmith is a research director at Acme Labs.")

(=>
	(occupiesPosition ?AGENT ?POSITION ?ORG)
	(attribute ?AGENT ?POSITION))

(<=>
   (employs ?ORG ?PERSON)
   (exists (?POSITION)
      (occupiesPosition ?PERSON ?POSITION ?ORG)))

(subclass NormativeAttribute RelationalAttribute)
(documentation NormativeAttribute "A &%Class containing all of the
&%Attributes that are specific to morality, legality, aesthetics,
etiquette, etc.  Many of these attributes express a judgement that
something ought or ought not to be the case.")

(instance modalAttribute BinaryPredicate)
(instance modalAttribute AsymmetricRelation)
(instance modalAttribute IrreflexiveRelation)
(subrelation modalAttribute property)
(inverse modalAttribute isaModalAttributeOf)
(domain modalAttribute 1 Formula)
(domain modalAttribute 2 NormativeAttribute)
(documentation modalAttribute "A &%BinaryRelation that is used to state the normative force of a &%Proposition.  (&%modalAttribute ?FORMULA ?PROP) means that the &%Proposition expressed by ?FORMULA has the &%NormativeAttribute ?PROP.  For example, (&%modalAttribute (&%exists (?ACT ?OBJ) (&%and (&%instance ?ACT &%Giving) (&%agent ?ACT John) (&%patient ?ACT ?OBJ) (&%destination ?ACT Tom))) &%Obligatory) means that John is obligated to give Tom something.")

(=>
   (and
      (modalAttribute ?FORMULA1 ?PROP)
      (entails ?FORMULA1 ?FORMULA2))
   (modalAttribute ?FORMULA2 ?PROP))

(=>
   (modalAttribute ?FORMULA Obligatory)
   (exists (?AGENT)
      (holdsObligation ?FORMULA ?AGENT)))

(=>
   (modalAttribute ?FORMULA Permission)
   (exists (?AGENT)
      (holdsRight ?FORMULA ?AGENT)))

(=>
    (holdsRight ?FORMULA ?AGENT)
    (modalAttribute ?FORMULA Possibility))

(=>
    (holdsObligation ?FORMULA ?AGENT)
    (modalAttribute ?FORMULA Obligatory))

(subclass SubjectiveAssessmentAttribute NormativeAttribute)
(disjoint SubjectiveAssessmentAttribute ObjectiveNorm)
(documentation SubjectiveAssessmentAttribute "The &%Class of &%NormativeAttributes
which lack an objective criterion for their attribution, i.e. the attribution of
these &%Attributes varies from subject to subject and even with respect to the
same subject over time.  This &%Class is, generally speaking, only used when
mapping external knowledge sources to the SUMO.  If a term from such a knowledge
source seems to lack objective criteria for its attribution, it is assigned to
this &%Class.")

(subclass ObjectiveNorm NormativeAttribute)
(documentation ObjectiveNorm "The &%Class of &%NormativeAttributes that are
associated with an objective criterion for their attribution, i.e. there is
broad consensus about the cases where these attributes are applicable.")

(subclass ContestAttribute ObjectiveNorm)
(documentation ContestAttribute "A &%Class containing all of the
&%Attributes that are specific to participants in a &%Contest.  Some
of these &%Attributes are winning, losing, won, lost, etc.")

(=>
   (and
      (property ?THING ?ATTR)
      (instance ?ATTR ContestAttribute))
   (exists (?CONTEST)
      (and
         (instance ?CONTEST Contest)
         (or
            (agent ?CONTEST ?THING)
            (patient ?CONTEST ?THING)
            (subProcess ?THING ?CONTEST)))))

(subclass AlethicAttribute ObjectiveNorm)
(documentation AlethicAttribute "A &%Class containing all of the &%Attributes
relating to the notions of possibility and necessity.")

(instance Possibility AlethicAttribute)
(documentation Possibility "Attribute that applies to &%Propositions that are
possible, i.e. true in at least one possible world.")

(instance Necessity AlethicAttribute)
(documentation Necessity "Attribute that applies to &%Propositions that are
necessary, i.e. true in every possible world.")

(<=>
   (modalAttribute ?FORMULA Necessity)
   (not (modalAttribute (not ?FORMULA) Possibility)))

(=>
   (modalAttribute ?FORMULA Necessity)
   (modalAttribute ?FORMULA Possibility))

(subclass DeonticAttribute ObjectiveNorm)
(documentation DeonticAttribute "A &%Class containing all of the &%Attributes relating to the notions of permission, obligation, and prohibition.")

(instance Permission DeonticAttribute)
(documentation Permission "&%Attribute that applies to &%Propositions that an &%Agent is permitted, by some authority, to make true.")

(instance Obligatory DeonticAttribute)
(documentation Obligatory "Obligatory is an &%Attribute that applies to &%Propositions that an &%Agent is required, by some authority, to make true.")

(<=>
   (modalAttribute ?FORMULA Obligatory)
   (not (modalAttribute (not ?FORMULA) Permission)))

(=>
   (modalAttribute ?FORMULA Obligatory)
   (modalAttribute ?FORMULA Permission))

(subAttribute Law Obligatory)
(documentation Law "&%Attribute that applies to &%Propositions that are
required by a government or a branch of the government and that are enforced
with penalties for noncompliance.  These &%Propositions may be codified as
legislation or they may be more informal, as in the case of government policy.")

(subAttribute Promise Obligatory)
(documentation Promise "&%Attribute that applies to &%Propositions that
an &%Agent promises to make true.  &%Promises may be implicit or explicit.
They may be expressed in a written or verbal or gestural manner.")

(=>
   (property ?ENTITY Promise)
      (or
         (property ?ENTITY Contractual)
         (property ?ENTITY NakedPromise)))

(subAttribute Contractual Promise)
(documentation Contractual "&%Attribute that applies to &%Propositions where something is promised in return, i.e. a reciprocal promise.")

(subAttribute PurchaseContract Contractual)
(documentation PurchaseContract "A &%Contract between two &%Agents in
which one &%Agent agrees to render the other some good or service in
exchange for currency.")

(subAttribute ServiceContract Contractual)
(documentation ServiceContract "A &%Contract where an &%Agent agrees to
perform a service for another &%Agent (usually for a price).")

(subAttribute Warranty ServiceContract)
(documentation Warranty "A &%Contract that states the cirumstances
under which defects in the product will be corrected for no charge.
A &%Warranty is usually limited to a length of time that is specified
in the &%Warranty itself.  A &%Warranty also includes information about
what is not covered and actions that invalidate the &%Warranty.")

(subAttribute NakedPromise Promise)
(contraryAttribute NakedPromise Contractual)
(documentation NakedPromise "A &%Promise where nothing is promised in return, i.e. a nudum pactum.")

(subclass PhysicalState InternalAttribute)
(exhaustiveAttribute PhysicalState Solid Fluid Liquid Gas)
(documentation PhysicalState "The physical state of an &%Object.  There
are three reified instances of this &%Class:  &%Solid, &%Liquid, and &%Gas.
Physical changes are not characterized by the transformation of one
substance into another, but rather by the change of the form (physical
states) of a given substance.  For example, melting an iron nail yields a
substance still called iron.")

(instance Solid PhysicalState)
(contraryAttribute Solid Liquid Gas)
(documentation Solid "An &%Object has the &%Attribute of &%Solid if it
has a fixed shape and a fixed volume.")

(instance Fluid PhysicalState)
(documentation Fluid "&%Fluid is the &%PhysicalState attribute of an
&%Object that does not have a fixed shape and thus tends to flow or to
conform to the shape of a container.")

(instance Liquid PhysicalState)
(subAttribute Liquid Fluid)
(documentation Liquid "An &%Object has the &%Attribute of &%Liquid if
it has a fixed volume but not a fixed shape.")

(=>
   (instance ?OBJ Solution)
   (attribute ?OBJ Liquid))

(=>
   (and
      (instance ?MOTION LiquidMotion)
      (patient ?MOTION ?OBJ))
   (attribute ?OBJ Liquid))

(instance Gas PhysicalState)
(subAttribute Gas Fluid)
(documentation Gas "An &%Object has the &%Attribute of &%Gas if it has
neither a fixed volume nor a fixed shape.")

(<=>
   (instance ?OBJ Substance)
   (exists (?ATTR)
      (and
         (instance ?ATTR PhysicalState)
         (attribute ?OBJ ?ATTR))))

(=>
   (and
      (instance ?MOTION GasMotion)
      (patient ?MOTION ?OBJ))
   (attribute ?OBJ Gas))

(subclass PerceptualAttribute InternalAttribute)
(documentation PerceptualAttribute "Any &%Attribute whose presence is detected
by an act of &%Perception.")

(=>
   (and
      (instance ?PERCEPTION Perception)
      (patient ?PERCEPTION ?OBJ))
   (exists (?PROP)
      (and
         (instance ?PROP PerceptualAttribute)
         (attribute ?OBJ ?PROP))))

(subclass TasteAttribute PerceptualAttribute)
(documentation TasteAttribute "The &%Class of &%Attributes relating to
the taste of &%Objects.")

(=>
   (instance ?OBJ Food)
   (exists (?ATTR)
      (and
         (instance ?ATTR TasteAttribute)
         (attribute ?OBJ ?ATTR))))

(subclass OlfactoryAttribute PerceptualAttribute)
(documentation OlfactoryAttribute "The &%Class of properties that are
detectable by smell.")

(subclass VisualAttribute PerceptualAttribute)
(documentation VisualAttribute "The &%Class of visually discernible
properties.")

(instance Illuminated VisualAttribute)
(documentation Illuminated "The &%Attribute of &%Regions that are
illuminated to some degree, i.e. in which some shapes are visually
discernable.")

(instance Unilluminated VisualAttribute)
(contraryAttribute Unilluminated Illuminated)
(documentation Unilluminated "The &%Attribute of &%Regions that are
unilluminated, i.e in which no shapes are visually discernable.")

(subclass ColorAttribute VisualAttribute)
(documentation ColorAttribute "The &%Class of &%VisualAttributes
relating to the color of &%Objects.")

(subclass PrimaryColor ColorAttribute)
(documentation PrimaryColor "Colors which can be blended to form any
color and which cannot be derived from any other colors.")

(instance Red PrimaryColor)
(documentation Red "The &%Attribute of redness.")

(instance Blue PrimaryColor)
(documentation Blue "The &%Attribute of being blue in color.")

(instance Yellow PrimaryColor)
(documentation Yellow "The &%Attribute of being yellow in color.")

(instance White PrimaryColor)
(documentation White "The &%Attribute of being white in color.")

(instance Black PrimaryColor)
(documentation Black "The &%Attribute of being black in color.")

(instance Monochromatic ColorAttribute)
(documentation Monochromatic "An &%Object with this &%Attribute has
the same color on every part of its surface.")

(=>
   (and
      (attribute ?OBJ Monochromatic)
      (superficialPart ?PART ?OBJ)
      (attribute ?PART ?COLOR)
      (instance ?COLOR PrimaryColor))
   (forall (?ELEMENT)
      (=>
         (superficialPart ?ELEMENT ?OBJ)
         (attribute ?ELEMENT ?COLOR))))

(=>
   (instance ?OBJ Object)
   (or
      (attribute ?OBJ Monochromatic)
      (attribute ?OBJ Polychromatic)))

(instance Polychromatic ColorAttribute)
(contraryAttribute Polychromatic Monochromatic)
(documentation Polychromatic "An &%Object with this &%Attribute has
different colors on different parts of its surface.")

(=>
   (attribute ?OBJ Polychromatic)
   (exists (?PART1 ?PART2 ?COLOR1 ?COLOR2)
      (and
         (superficialPart ?PART1 ?OBJ)
         (superficialPart ?PART2 ?OBJ)
         (attribute ?PART1 ?COLOR1)
         (attribute ?PART2 ?COLOR2)
         (instance ?COLOR1 ColorAttribute)
         (instance ?COLOR2 ColorAttribute)
         (not (equal ?COLOR1 ?COLOR2)))))

(subclass ShapeAttribute InternalAttribute)
(documentation ShapeAttribute "Any &%Attribute that relates to the
shape of an &%Object.")

(instance Pliable ShapeAttribute)
(documentation Pliable "The shape of an &%Object with this &%Attribute
can be altered.")

(=>
   (exists (?CHANGE)
      (and
         (instance ?CHANGE ShapeChange)
         (patient ?CHANGE ?OBJ)))
   (attribute ?OBJ Pliable))

(instance Rigid ShapeAttribute)
(contraryAttribute Rigid Pliable)
(documentation Rigid "The shape of an &%Object with this &%Attribute
cannot be altered.")

(=>
   (instance ?OBJ SelfConnectedObject)
   (or
      (attribute ?OBJ Pliable)
      (attribute ?OBJ Rigid)))

(subclass TwoDimensionalAngle OpenTwoDimensionalFigure)
(documentation TwoDimensionalAngle "Any two &%OneDimensionalFigures (i.e.
straight lines) meeting at a single &%GeometricPoint.")

(subclass RightAngle TwoDimensionalAngle)
(documentation RightAngle "Any &%TwoDimensionalAngle that has the
&%angularMeasure of 90 &%AngularDegrees.")

(=>
   (instance ?ANGLE RightAngle)
   (angularMeasure ?ANGLE (MeasureFn 90 AngularDegree)))

(subclass ClosedTwoDimensionalFigure TwoDimensionalFigure)
(documentation ClosedTwoDimensionalFigure "Any &%TwoDimensionalFigure which
has a well defined interior and exterior.")

(subclass Polygon ClosedTwoDimensionalFigure)
(documentation Polygon "A &%ClosedTwoDimensionalFigure that is composed
exclusively of straight lines, i.e. &%OneDimensionalFigures.")

(=>
   (instance ?POLYGON Polygon)
   (=>
      (geometricPart ?PART ?POLYGON)
      (or
         (sideOfFigure ?PART ?POLYGON)
         (exists (?SIDE)
            (and
               (sideOfFigure ?SIDE ?POLYGON)
               (geometricPart ?PART ?SIDE))))))

(subclass Triangle Polygon)
(documentation Triangle "Any three-sided &%Polygon.")

(=>
   (instance ?TRIANGLE Triangle)
   (equal (CardinalityFn (KappaFn ?SIDE (sideOfFigure ?SIDE ?TRIANGLE))) 3))

(subclass Quadrilateral Polygon)
(documentation Quadrilateral "Any four-sided &%Polygon.")

(=>
   (instance ?QUAD Quadrilateral)
   (equal (CardinalityFn (KappaFn ?SIDE (sideOfFigure ?SIDE ?QUAD))) 4))

(subclass Rectangle Quadrilateral)
(documentation Rectangle "Any &%Quadrilateral whose angles are all
&%RightAngles.")

(=>
   (instance ?RECTANGLE Rectangle)
   (=>
      (angleOfFigure ?ANGLE ?RECTANGLE)
      (instance ?ANGLE RightAngle)))

(subclass Square Rectangle)
(documentation Square "Any &%Rectangle whose sides are all equal.")

(=>
   (instance ?SQUARE Square)
   (exists (?LENGTH)
      (forall (?SIDE)
         (=>
            (sideOfFigure ?SIDE ?SQUARE)
            (lineMeasure ?SIDE ?LENGTH)))))

(subclass Circle ClosedTwoDimensionalFigure)
(documentation Circle "The class of &%ClosedTwoDimensionalFigures such that
all &%GeometricPoints that make up the &%Circle are equidistant from a
single &%GeometricPoint, known as the center of the &%Circle.")

(=>
   (instance ?CIRCLE Circle)
   (exists (?RADIUS)
      (radius ?CIRCLE ?RADIUS)))

(subclass ThreeDimensionalFigure GeometricFigure)
(documentation ThreeDimensionalFigure "The class of &%GeometricFigures that
have position and an extension along three dimensions, viz. geometric solids
like polyhedrons and cylinders.")

(instance geometricPart BinaryPredicate)
(instance geometricPart PartialOrderingRelation)
(domain geometricPart 1 GeometricFigure)
(domain geometricPart 2 GeometricFigure)
(documentation geometricPart "(&%geometricPart ?PART ?WHOLE) means that the
&%GeometricFigure ?PART is part of the &%GeometricFigure ?WHOLE.")

(subrelation pointOfFigure geometricPart)
(domain pointOfFigure 1 GeometricPoint)
(domain pointOfFigure 2 GeometricFigure)
(documentation pointOfFigure "(&%pointOfFigure ?POINT ?FIGURE) means that
the &%GeometricPoint ?POINT is part of the &%GeometricFigure ?FIGURE.")

(subrelation sideOfFigure geometricPart)
(domain sideOfFigure 1 OneDimensionalFigure)
(domain sideOfFigure 2 GeometricFigure)
(documentation sideOfFigure "(&%sideOfFigure ?SIDE ?FIGURE) means that the
&%OneDimensionalFigure ?POINT is a side of the &%GeometricFigure ?FIGURE.")

(subrelation angleOfFigure geometricPart)
(domain angleOfFigure 1 TwoDimensionalAngle)
(domain angleOfFigure 2 GeometricFigure)
(documentation angleOfFigure "(&%angleOfFigure ?ANGLE ?FIGURE) means that
the &%TwoDimensionalAngle ?ANGLE is part of the &%GeometricFigure ?FIGURE.")

(instance pointOfIntersection TernaryPredicate)
(domain pointOfIntersection 1 OneDimensionalFigure)
(domain pointOfIntersection 2 OneDimensionalFigure)
(domain pointOfIntersection 3 GeometricPoint)
(documentation pointOfIntersection "(&%pointOfIntersection ?FIGURE1 ?FIGURE2 ?POINT) means that the two straight lines ?FIGURE1 and ?FIGURE2 meet at the point ?POINT.")

(=>
   (pointOfIntersection ?FIGURE1 ?FIGURE2 ?POINT)
   (and
      (pointOfFigure ?POINT ?FIGURE1)
      (pointOfFigure ?POINT ?FIGURE2)))

(instance parallel BinaryPredicate)
(domain parallel 1 OneDimensionalFigure)
(domain parallel 2 OneDimensionalFigure)
(documentation parallel "(&%parallel ?LINE1 ?LINE2) means that the
&%OneDimensionalFigures ?LINE1 and ?LINE2 are parallel to one another,
i.e. they are equidistant from one another at every point.")

(=>
   (parallel ?LINE1 ?LINE2)
   (not (exists (?POINT)
      (pointOfIntersection ?LINE1 ?LINE2 ?POINT))))

(instance angularMeasure BinaryPredicate)
(instance angularMeasure TotalValuedRelation)
(domain angularMeasure 1 TwoDimensionalAngle)
(domain angularMeasure 2 PlaneAngleMeasure)
(documentation angularMeasure "(&%angularMeasure ?ANGLE ?MEASURE) means that
the two-dimensional geometric angle ?ANGLE has the &%PlaneAngleMeasure of
?MEASURE.")

(instance lineMeasure BinaryPredicate)
(instance lineMeasure TotalValuedRelation)
(domain lineMeasure 1 OneDimensionalFigure)
(domain lineMeasure 2 LengthMeasure)
(documentation lineMeasure "(&%lineMeasure ?LINE ?MEASURE) means that the
straight line ?LINE has the &%LengthMeasure of ?MEASURE.")

(instance geometricDistance TernaryPredicate)
(instance geometricDistance SingleValuedRelation)
(instance geometricDistance TotalValuedRelation)
(domain geometricDistance 1 GeometricPoint)
(domain geometricDistance 2 GeometricPoint)
(domain geometricDistance 3 LengthMeasure)
(documentation geometricDistance "(&%geometricDistance ?POINT1 ?POINT2
?LENGTH) means that ?LENGTH is the distance between the two
&%GeometricPoints ?POINT1 and ?POINT2.")

(=>
   (geometricDistance ?POINT1 ?POINT2 ?LENGTH)
   (geometricDistance ?POINT2 ?POINT1 ?LENGTH))

(instance radius BinaryPredicate)
(instance radius SingleValuedRelation)
(instance radius TotalValuedRelation)
(domain radius 1 Circle)
(domain radius 2 LengthMeasure)
(documentation radius "(&%radius ?CIRCLE ?LENGTH) means that the radius of
the &%Circle ?CIRCLE has a length of ?LENGTH.")

(=>
   (radius ?CIRCLE ?RADIUS)
   (exists (?POINT)
      (forall (?PART)
         (=>
            (pointOfFigure ?PART ?CIRCLE)
            (geometricDistance ?PART ?POINT ?RADIUS)))))

(instance diameter BinaryPredicate)
(instance diameter SingleValuedRelation)
(instance diameter TotalValuedRelation)
(domain diameter 1 Circle)
(domain diameter 2 LengthMeasure)
(documentation diameter "&%BinaryPredicate that is used to state the measure of a circular &%Object from side to side.  (&%diameter ?CIRCLE ?LENGTH) means that the diameter of the &%Circle ?CIRCLE has a length of ?LENGTH.")

(=>
   (diameter ?CIRCLE ?LENGTH)
   (exists (?HALF)
      (and
         (radius ?CIRCLE ?HALF)
         (equal (MultiplicationFn ?HALF 2) ?LENGTH))))

(subclass TextureAttribute PerceptualAttribute)
(subclass TextureAttribute ShapeAttribute)
(documentation TextureAttribute "Any &%Attribute that characterizes the
texture of an &%Object.  Note that a &%TextureAttribute always applies
to the surface of an object whenever it applies to the object itself.")

(=>
   (and
      (instance ?ATTRIBUTE TextureAttribute)
      (attribute ?OBJ ?ATTRIBUTE)
      (surface ?SURFACE ?OBJ))
   (attribute ?SURFACE ?ATTRIBUTE))

(subclass SoundAttribute PerceptualAttribute)
(documentation SoundAttribute "Any &%Attribute that characterizes the
sound made by an &%Object.")

(subclass SaturationAttribute InternalAttribute)
(documentation SaturationAttribute "A &%Class of &%Attributes that specify, in
a qualitative manner, the extent of the presence of one kind of &%Object in
another kind of &%Object.")

(instance Dry SaturationAttribute)
(contraryAttribute Dry Damp)
(documentation Dry "An &%Attribute which indicates that the associated
&%Object contains no &%Liquid.")

(=>
   (attribute ?OBJ Dry)
   (not
      (exists (?SUBOBJ)
         (and
            (part ?SUBOBJ ?OBJ)
            (attribute ?SUBOBJ Liquid)))))

(instance Damp SaturationAttribute)
(documentation Damp "An &%Attribute which indicates that the associated
&%Object contains some &%Liquid.")

(instance Wet SaturationAttribute)
(subAttribute Wet Damp)
(documentation Wet "An &%Attribute which indicates that the
associated &%Object is fully saturated with a &%Liquid, i.e.
every part of the &%Object has a subpart which is a &%Liquid.")

(=>
   (attribute ?OBJ Wet)
   (forall (?PART)
      (=>
         (part ?PART ?OBJ)
         (exists (?SUBPART)
            (and
               (part ?SUBPART ?PART)
               (attribute ?SUBPART Liquid))))))

(subclass BreakabilityAttribute InternalAttribute)
(documentation BreakabilityAttribute "A &%subclass of &%Attributes for
characterizing the breakability of &%CorpuscularObjects.")

(instance Fragile BreakabilityAttribute)
(documentation Fragile "An &%Attribute which indicates that the
associated &%Object is very breakable.")

(instance Unbreakable BreakabilityAttribute)
(contraryAttribute Unbreakable Fragile)
(documentation Unbreakable "An &%Attribute which indicates that the
associated &%Object cannot be broken.")

(=>
   (attribute ?OBJ Unbreakable)
   (not (exists (?DAMAGE)
      (and
         (instance ?DAMAGE Damaging)
         (patient ?DAMAGE ?OBJ)))))

(subclass BiologicalAttribute InternalAttribute)
(documentation BiologicalAttribute "&%Attributes that apply specifically
to instances of &%Organism.")

(=>
    (and
        (attribute ?ORG ?ATT)
        (instance ?ATT BiologicalAttribute))
    (instance ?ORG Organism))

(subclass BodyPosition BiologicalAttribute)
(documentation BodyPosition "The class of &%Attributes expressing
configurations of bodies or parts of bodies of animals or humans,
e.g. standing, sitting, kneeling, lying down, etc.")

(=>
   (instance ?ANIMAL Animal)
   (or
      (exists (?MOTION)
         (and
            (instance ?MOTION BodyMotion)
            (agent ?MOTION ?ANIMAL)))
      (exists (?ATTR)
         (and
            (instance ?ATTR BodyPosition)
            (attribute ?ANIMAL ?ATTR)))))

(instance Standing BodyPosition)
(documentation Standing "The &%BodyPosition of being upright, i.e. being
fully extended and supported by nothing other than one's own feet.")

(=>
   (and
      (instance ?AMBULATE Ambulating)
      (agent ?AMBULATE ?AGENT))
   (attribute ?AGENT Standing))

(instance Sitting BodyPosition)
(documentation Sitting "The &%BodyPosition of being recumbent, i.e.
knees bent and back side supported.")

(instance Prostrate BodyPosition)
(documentation Prostrate "The &%BodyPosition of lying down, being in a
horizontal position.")

(subclass AnimacyAttribute BiologicalAttribute)
(exhaustiveAttribute AnimacyAttribute Living Dead)
(documentation AnimacyAttribute "&%Attributes that indicate whether an
&%Organism is alive or not.")

(instance Living AnimacyAttribute)
(documentation Living "This &%Attribute applies to &%Organisms that are
alive.")

(=>
   (and
      (instance ?ORGANISM Organism)
      (agent ?PROCESS ?ORGANISM))
   (holdsDuring (WhenFn ?PROCESS) (attribute ?ORGANISM Living)))

(instance Dead AnimacyAttribute)
(subAttribute Dead Unconscious)
(contraryAttribute Dead Living)
(documentation Dead "This &%Attribute applies to &%Organisms that are
not alive.")

(=>
   (instance ?ORG Organism)
   (exists (?ATTR)
      (and
         (instance ?ATTR AnimacyAttribute)
         (attribute ?ORG ?ATTR))))

(subclass SexAttribute BiologicalAttribute)
(exhaustiveAttribute SexAttribute Female Male)
(documentation SexAttribute "&%Attributes that indicate the sex of an
&%Organism.")

(instance Female SexAttribute)
(documentation Female "An &%Attribute indicating that an &%Organism is
female in nature.")

(=>
   (and
      (instance ?BODY ReproductiveBody)
      (part ?BODY ?ORG)
      (instance ?ORG Organism))
(attribute ?ORG Female))

(=>
   (instance ?WOMAN Woman)
   (attribute ?WOMAN Female))

(instance Male SexAttribute)
(contraryAttribute Male Female)
(documentation Male "An &%Attribute indicating that an &%Organism is
male in nature.")

(=>
   (instance ?ANIMAL Animal)
   (exists (?ATTR)
      (and
         (instance ?ATTR SexAttribute)
         (attribute ?ANIMAL ?ATTR))))

(=>
   (instance ?MAN Man)
   (attribute ?MAN Male))

(subclass DevelopmentalAttribute BiologicalAttribute)
(exhaustiveAttribute DevelopmentalAttribute FullyFormed NonFullyFormed)
(documentation DevelopmentalAttribute "&%Attributes that indicate the
stage of development of an &%Organism.")

(instance FullyFormed DevelopmentalAttribute)
(documentation FullyFormed "The stage of an &%Organism when it has reached
the end of its growth phase.")

(=>
   (attribute ?OBJ FullyFormed)
   (exists (?GROWTH)
      (and
         (instance ?GROWTH Growth)
         (experiencer ?GROWTH ?OBJ)
         (holdsDuring (BeginFn (WhenFn ?OBJ)) (attribute ?OBJ NonFullyFormed)))))

(instance NonFullyFormed DevelopmentalAttribute)
(contraryAttribute NonFullyFormed FullyFormed)
(successorAttribute NonFullyFormed FullyFormed)
(documentation NonFullyFormed "The stage of an &%Organism before it is
&%FullyFormed.")

(=>
   (instance ?ORG Organism)
   (exists (?ATTR)
      (and
         (instance ?ATTR DevelopmentalAttribute)
         (attribute ?ORG ?ATTR))))

(instance Larval DevelopmentalAttribute)
(subAttribute Larval NonFullyFormed)
(documentation Larval "Form of most &%Invertebrates, &%Amphibians, and
&%Fish immediately after they hatch.  This form is fundamentally unlike
the adult form, and metamorphosis is required to reach the latter form.")

(=>
   (holdsDuring ?TIME (attribute ?ORG Larval))
   (holdsDuring (PastFn ?TIME) (exists (?BIRTH) (and (instance ?BIRTH Birth) (experiencer ?BIRTH ?ORG)))))

(instance Embryonic DevelopmentalAttribute)
(subAttribute Embryonic NonFullyFormed)
(contraryAttribute Embryonic Larval)
(documentation Embryonic "The stage of an &%Organism or an
&%AnatomicalStructure that exists only before the &%Organism is born.
&%Mammals, for example, have this &%Attribute only prior to
their birth.")

(=>
   (attribute ?ORG Embryonic)
   (exists (?BODY)
      (and
         (instance ?BODY ReproductiveBody)
         (located ?ORG ?BODY))))

(=>
   (holdsDuring ?TIME (attribute ?ORG Embryonic))
   (holdsDuring ?TIME (not (exists (?BIRTH) (and (instance ?BIRTH Birth) (experiencer ?BIRTH ?ORG))))))

(subclass DiseaseOrSyndrome BiologicalAttribute)
(documentation DiseaseOrSyndrome "A &%BiologicalAttribute which qualifies
something that alters or interferes with a normal process, state or activity
of an &%Organism.  It is usually characterized by the abnormal functioning of
one or more of the host's systems, parts, or &%Organs.")

(subclass PsychologicalAttribute BiologicalAttribute)
(partition PsychologicalAttribute StateOfMind TraitAttribute)
(documentation PsychologicalAttribute "&%Attributes that characterize the mental
or behavioral life of an &%Organism.")

(=>
   (instance ?ATTR PsychologicalAttribute)
   (=>
      (holdsDuring ?TIME (attribute ?ORGANISM ?ATTR))
      (holdsDuring ?TIME (attribute ?ORGANISM Living))))

(=>
   (and
      (instance ?ATTR PsychologicalAttribute)
      (attribute ?AGENT ?ATTR))
   (instance ?AGENT SentientAgent))

(subclass StateOfMind PsychologicalAttribute)
(documentation StateOfMind  "The class &%StateOfMind is distinguished from
its complement &%TraitAttribute by the fact that instances of the former are
transient while instances of the latter are persistent features of a creature's behavioral/psychological make-up.")

(subclass EmotionalState StateOfMind)
(documentation EmotionalState "The &%Class of &%Attributes that denote emotional
states of &%Organisms.")

(subclass ConsciousnessAttribute StateOfMind)
(documentation ConsciousnessAttribute "&%Attributes that indicate whether
an &%Organism is conscious or the qualitative degree of consciousness of
an &%Organism.")

(<=>
   (and
      (instance ?AGENT SentientAgent)
      (attribute ?AGENT Living))
   (exists (?ATTR)
      (and
         (instance ?ATTR ConsciousnessAttribute)
         (attribute ?AGENT ?ATTR))))

(subAttribute Asleep Unconscious)
(documentation Asleep "This &%Attribute applies to &%Organisms that are
sleeping.")

(instance Unconscious ConsciousnessAttribute)
(contraryAttribute Unconscious Awake)
(documentation Unconscious "This &%Attribute applies to &%Organisms that
are unconscious.  An &%Organism may be &%Unconscious because it is &%Dead
or &%Asleep or because of a blow to the head, a drug, etc.")

(instance Awake ConsciousnessAttribute)
(documentation Awake "This &%Attribute applies to &%Organisms that are
neither &%Unconscious nor &%Asleep.")

(=>
   (or
      (attribute ?AGENT Asleep)
      (attribute ?AGENT Awake))
   (attribute ?AGENT Living))

(subclass TraitAttribute PsychologicalAttribute)
(documentation TraitAttribute "&%Attributes that indicate the the
behavior/personality traits of an &%Organism.")

(subclass PsychologicalDysfunction PsychologicalAttribute)
(subclass PsychologicalDysfunction DiseaseOrSyndrome)
(documentation PsychologicalDysfunction "A clinically significant
dysfunction whose major manifestation is behavioral or psychological.
These dysfunctions may have identified or presumed biological etiologies
or manifestations.")

;; END FILE

;; <module>Sequestered_Axioms</module>
;; BEGIN FILE


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Sequestered Axioms    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  This section is not a subontology of the SUMO.  It contains axioms that relate
;;  to other sections of the ontology but that should not be used by the inference
;;  engine, because they can be used to construct arbitrarily complex terms that often
;;  appear in proofs with unhelpful conclusions.  Before this file is loaded into the
;;  inference engine, the axioms in this section should be commented out.

(<=>
   (instance ?ENTITY (UnionFn ?CLASS1 ?CLASS2))
   (or
      (instance ?ENTITY ?CLASS1)
      (instance ?ENTITY ?CLASS2)))

(<=>
   (instance ?ENTITY (IntersectionFn ?CLASS1 ?CLASS2))
   (and
      (instance ?ENTITY ?CLASS1)
      (instance ?ENTITY ?CLASS2)))

(<=>
   (instance ?ENTITY (ComplementFn ?CLASS))
   (not
      (instance ?ENTITY ?CLASS)))

(=>
   (and
      (instance ?CLASS1 SetOrClass)
      (instance ?CLASS2 SetOrClass))
   (equal (RelativeComplementFn ?CLASS1 ?CLASS2) (IntersectionFn ?CLASS1 (ComplementFn ?CLASS2))))

(<=>
   (instance ?ENTITY (GeneralizedUnionFn ?SUPERCLASS))
   (exists (?CLASS)
      (and
         (instance ?CLASS ?SUPERCLASS)
         (instance ?ENTITY ?CLASS))))

(<=>
   (instance ?ENTITY (GeneralizedIntersectionFn ?SUPERCLASS))
   (forall (?CLASS)
         (=>
            (instance ?CLASS ?SUPERCLASS)
            (instance ?ENTITY ?CLASS))))

(<=>
   (instance ?SUBCLASS (PowerSetFn ?CLASS))
   (subclass ?SUBCLASS ?CLASS))

;; <module>MidLevel</module>
;;  BEGIN FILE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      MILO (MId-Level Ontology)  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;; Part 2 of samin002.txt: the Teknowledge Mid-level ontology
;;    after removal of concepts duplicating those in the SUMO 1.56
;;
;; This is the source file for the MILO (MId-Level Ontology), an ontology that
;; is being developed as a bridge between the abstract content of the SUMO and
;; the rich detail of the various Teknowledge domain ontologies.  The MILO is
;; at this point in time (April 2003) provisional and incomplete.  It will be
;; actively modified and greatly enlarged at least through the end of 2003.
;; Those who are interested in making use of this ontology are urged
;; to contact Adam Pease (apease@ks.teknowledge.com) or Ian Niles
;; (iniles@teknowledge.com) for the latest version.

;; The MILO (MId-Level Ontology) is copyrighted by Teknowledge (c)
;; 2003.  It is released under the GNU Public License
;; <http://www.gnu.org/copyleft/gpl.html>.  Users of this code also consent, by
;; use of this material, to credit Teknowledge in any writings, briefings,
;; publications, presentations, or other representations of any code or other
;; product which incorporates, builds on, or uses this material.


;; ===========
;; New Content
;; ===========

;; The following content (down to the header for "Elements Ontology") was
;; newly created for the MILO.

(subclass HumanCorpse Human)
(documentation HumanCorpse "A &%Human which is &%Dead.")

(<=>
   (instance ?CORPSE HumanCorpse)
   (and
      (instance ?CORPSE Human)
      (attribute ?CORPSE Dead)))

(subclass HumanSlave Human)
(documentation HumanSlave "A &%Human who is owned by someone else.")

(=>
   (instance ?SLAVE HumanSlave)
   (exists (?PERSON)
      (and
         (instance ?PERSON Human)
         (not (equal ?PERSON ?SLAVE))
         (possesses ?PERSON ?SLAVE))))

(subclass HumanAdult Human)
(documentation HumanAdult "The class of &%Humans that are 18 years of
age or older.")

(=>
   (and
      (instance ?ADULT HumanAdult)
      (age ?ADULT (MeasureFn ?NUMBER YearDuration)))
   (greaterThanOrEqualTo ?NUMBER 18))

(subclass HumanYouth Human)
(documentation HumanYouth "The class of &%Humans that are younger than
18 years of age.")

(=>
   (and
      (instance ?YOUTH HumanYouth)
      (age ?YOUTH (MeasureFn ?NUMBER YearDuration)))
   (lessThan ?NUMBER 18))

(subclass HumanChild HumanYouth)
(documentation HumanChild "A &%HumanYouth between birth and puberty, i.e a
&%Human who is &%NonFullyFormed.")

(=>
   (instance ?CHILD HumanChild)
   (attribute ?CHILD NonFullyFormed))

(=>
   (and
      (instance ?CHILD HumanChild)
      (age ?CHILD (MeasureFn ?NUMBER YearDuration)))
   (lessThanOrEqualTo ?NUMBER 14))

(subclass Teenager HumanYouth)
(documentation Teenager "A &%HumanYouth between puberty and the age of 20.")

(=>
   (and
      (instance ?TEEN Teenager)
      (age ?TEEN (MeasureFn ?NUMBER YearDuration)))
   (and
      (greaterThan ?NUMBER 12)
      (lessThan ?NUMBER 20)))

(subclass Boy HumanChild)
(subclass Boy Man)
(documentation Boy "A &%HumanChild who is &%Male.")

(subclass Girl HumanChild)
(subclass Girl Woman)
(documentation Girl "A &%HumanChild who is &%Female.")

(subclass HumanBaby HumanChild)
(documentation HumanBaby "A &%Human between birth and the first year of age.")

(=>
   (and
      (instance ?BABY HumanBaby)
      (age ?CHILD (MeasureFn ?NUMBER YearDuration)))
   (lessThanOrEqualTo ?NUMBER 1))

(subclass Stepping BodyMotion)
(documentation Stepping "The &%BodyMotion of extending one foot forward
and then bringing the other foot to the same lateral position as the
first leg.")

(=>
   (and
      (instance ?AMBULATE Ambulating)
      (agent ?AMBULATE ?AGENT))
   (attribute ?AGENT Standing))

(=>
   (instance ?AMBULATE Ambulating)
   (exists (?SUB)
      (and
         (subclass ?SUB Stepping)
         (forall (?INST)
            (<=>
               (instance ?INST ?SUB)
               (subProcess ?INST ?AMBULATE))))))

(subclass ReflectingLight RadiatingLight)
(documentation ReflectingLight "Those instances of &%RadiatingLight where the
&%instrument is not a light source, but is simply a surface which bends light
waves that come in contact with it.")

(subclass Lyrics Text)
(documentation Lyrics "Any &%Text which is intended to be sung.")

(=>
   (instance ?LYRIC Lyrics)
   (hasPurpose ?LYRIC (exists (?SING)
                         (and
                            (instance ?SING Singing)
                            (patient ?SING ?LYRIC)))))

(subclass InstrumentalMusic Music)
(documentation InstrumentalMusic "&%Music which is produced (at least in part)
by a &%MusicalInstrument.")

(=>
   (instance ?MUSIC InstrumentalMusic)
   (exists (?INSTRUMENT)
      (and
         (instance ?INSTRUMENT MusicalInstrument)
         (instrument ?MUSIC ?INSTRUMENT))))

(subclass Piano MusicalInstrument)
(documentation Piano "A &%MusicalInstrument with keys that, when pressed down,
activate hammers that, in turn, strike strings.")

(subclass MonophonicMusic Music)
(documentation MonophonicMusic "&%Music which has a single part, i.e. &%Music
which cannot be divided into two or more contemporaneous &%subProcesses which
are also instances of &%Music.")

(subclass PolyphonicMusic Music)
(documentation PolyphonicMusic "&%Music which has two or more parts, i.e. &%Music
which can be divided into two or more contemporaneous &%subProcesses which are
also instances of &%Music.")

(<=>
   (instance ?MUSIC PolyphonicMusic)
   (exists (?PART1 ?PART2)
      (and
         (instance ?MUSIC Music)
         (instance ?PART1 Music)
         (instance ?PART2 Music)
         (subProcess ?PART1 ?MUSIC)
         (subProcess ?PART2 ?MUSIC)
         (not (equal ?PART1 ?PART2))
         (cooccur ?PART1 ?MUSIC)
         (cooccur ?PART2 ?MUSIC))))

(subclass WrittenCommunication LinguisticCommunication)
(documentation WrittenCommunication "Any &%LinguisticCommunication where the
&%instrument is a &%Text, e.g. a letter, an email, a memo, etc.")

(=>
   (instance ?COMMUNICATE WrittenCommunication)
   (exists (?WRITE ?READ ?TEXT)
      (and
         (instance ?WRITE Writing)
         (instance ?READ Reading)
         (instance ?TEXT Text)
         (instrument ?COMMUNICATE ?TEXT)
         (result ?WRITE ?TEXT)
         (patient ?READ ?TEXT)
         (subProcess ?WRITE ?COMMUNICATE)
         (subProcess ?READ ?COMMUNICATE))))

(subclass Reminding Requesting)
(documentation Reminding "Any &%Requesting that is intended to cause a
&%Remembering of something.")

(=>
   (instance ?REMIND Reminding)
   (exists (?REMEMBER)
      (and
         (instance ?REMEMBER Remembering)
         (causes ?REMIND ?REMEMBER))))

(subclass Threatening Committing)
(documentation Threatening "Any &%Committing where the thing promised
is something that is deemed undesirable by the &%destination of the
&%Committing.")

(=>
   (and
      (instance ?THREATEN Threatening)
      (agent ?THREATEN ?AGENT)
      (patient ?THREATEN ?PROP)
      (destination ?THREATEN ?DEST))
  (not (desires ?DEST ?PROP)))

(subclass Punishing OrganizationalProcess)
(documentation Punishing "Any &%OrganizationalProcess where the &%agent
does something to the &%destination that the &%agent knows is undesirable
for the &%destination.")

(=>
   (and
      (instance ?PUNISH Punishing)
      (agent ?PUNISH ?AGENT)
      (patient ?PUNISH ?PROP)
      (destination ?PUNISH ?DEST))
  (knows ?AGENT (not (desires ?DEST ?PROP))))

(subclass Registering Stating)
(subclass Registering PoliticalProcess)
(documentation Registering "Submitting official paperwork in a government
agency, e.g. filing for divorce, making a legal claim against someone.")

(=>
   (instance ?ACTION LegalAction)
   (exists (?REGISTER)
      (and
         (instance ?REGISTER Registering)
         (subProcess ?REGISTER ?ACTION))))

(subclass Answering Stating)
(documentation Answering "Responding to a &%Questioning, i.e. trying to answer
someone's question.")

(=>
   (instance ?ANSWER Answering)
   (exists (?QUESTION)
      (and
         (instance ?QUESTION Questioning)
         (refers ?ANSWER ?QUESTION)
         (earlier (WhenFn ?QUESTION) (WhenFn ?ANSWER)))))

(subclass Arguing Stating)
(documentation Arguing "Any &%Stating which has the form of an &%Argument.")

(=>
   (instance ?ARGUE Arguing)
   (exists (?STATEMENT)
      (and
         (patient ?ARGUE ?STATEMENT)
         (instance ?STATEMENT Statement)
         (containsInformation ?STATEMENT ?ARGUMENT)
         (instance ?ARGUMENT Argument))))

(subclass StatingALie Stating)
(documentation StatingALie "Any &%Stating which is both &%False and believed
to be &%False by the &%agent of the &%Stating.")

(=>
   (and
      (instance ?STATE StatingALie)
      (agent ?STATE ?AGENT)
      (patient ?STATE ?STATEMENT))
   (and
      (true ?STATMENT False)
      (believes ?AGENT (true ?STATEMENT False))))

(subclass Founding Declaring)
(subclass Founding OrganizationalProcess)
(documentation Founding "Setting up an &%Organization.")

(=>
   (instance ?FOUND Founding)
   (exists (?ORG)
      (and
         (instance ?ORG Organization)
         (result ?FOUND ?ORG))))

(subclass Indicating Communicating)
(subclass Indicating BodyMotion)
(documentation Indicating "Pointing out a person, place or thing with
one's hand or with an &%Artifact.")

(subclass Projectile Weapon)
(documentation Projectile "A missile, bullet, etc. that is fired from
a &%Weapon.")

(=>
   (instance ?PROJECTILE Projectile)
   (capability Shooting patient ?PROJECTILE))

(subclass ProjectileShell Container)
(documentation ProjectileShell "The outer casing of a &%Projectile.")

(=>
   (instance ?PROJECTILE Projectile)
   (exists (?SHELL)
      (and
         (instance ?SHELL ProjectileShell)
         (part ?SHELL ?PROJECTILE))))

(subclass Gun Weapon)
(partition Gun ArtilleryGun Firearm)
(documentation Gun "A &%Weapon that shoots a &%Projectile.")

(=>
   (instance ?GUN Gun)
   (capability Shooting instrument ?GUN))

(=>
   (instance ?SHOOT Shooting)
   (exists (?PROJECTILE ?GUN)
      (and
         (instance ?PROJECTILE Projectile)
         (patient ?SHOOT ?PROJECTILE)
         (instance ?GUN Gun)
         (instrument ?SHOOT ?GUN))))

(subclass GunStock EngineeringComponent)
(documentation GunStock "The part of a &%Gun that is placed against the
shoulder to absorb some of the recoil action when it is fired.")

(=>
   (instance ?STOCK GunStock)
   (exists (?GUN)
      (and
         (instance ?GUN Gun)
         (part ?STOCK ?GUN))))

(subclass ArtilleryGun Gun)
(documentation ArtilleryGun "A &%Gun that is too large to be carried and fired
by a single &%Human.  Typically, &%ArtilleryGuns are on wheels.")

(subclass Firearm Gun)
(partition Firearm Rifle Pistol)
(documentation Firearm "A &%Gun that is small enough to be carried and fired by
a single &%Human.")

(subclass Rifle Firearm)
(documentation Rifle "A &%Firearm with a long barrel that is intended to be fired
from the shoulder.")

(subclass Pistol Firearm)
(documentation Pistol "A &%Firearm that is intended to be aimed and fired with a
single hand.")

(subclass Manifold EngineeringComponent)
(documentation Manifold "A pipe which has several outlets for other pipes that flow
into or out of it.")

(subclass BirthControlDevice Device)
(documentation BirthControlDevice "&%Devices which permit sexual intercourse but
which reduce the likelihood of conception.")

(subclass SwitchDevice Device)
(documentation SwitchDevice "A &%Device which is capable of turning an
&%ElectricDevice on and off.")

(=>
   (instance ?DEVICE SwitchDevice)
   (exists (?PROC1 ?PROC2 ?ELECTRIC)
      (and
         (instrument ?PROC1 ?DEVICE)
         (causes ?PROC1 ?PROC2)
         (instrument ?PROC2 ?ELECTRIC)
         (instance ?ELECTRIC ElectricDevice))))

(subclass Transducer Device)
(documentation Transducer "A &%Device which is capable of converting one form of
energy into another.")

(subclass Aerator Device)
(documentation Aerator "A &%Device whose purpose is to mix &%Substances with
&%Air.")

(=>
   (instance ?AERATOR Aerator)
   (hasPurpose ?AERATOR (exists (?COMBINE)
                           (and
                              (instance ?COMBINE Combining)
                              (resource ?COMBINE ?AIR)
                              (instance ?AIR Air)
                              (instrument ?COMBINE ?AERATOR)))))

(subclass Filter Device)
(documentation Filter "A &%Device whose purpose is to remove part of a
&%Solution that is passed through the &%Filter.")

(=>
   (instance ?FILTER Filter)
   (hasPurpose ?FILTER (exists (?REMOVE ?SOLUTION)
                          (and
                             (instance ?REMOVE Removing)
                             (origin ?REMOVE ?SOLUTION)
                             (instance ?SOLUTION Solution)
                             (instrument ?REMOVE ?FILTER)))))

(subclass FileDevice Device)
(documentation FileDevice "A &%Device whose purpose is to make something
smoother.  For example, a nail file is used to even out the tips of one's
finger nails.")

(=>
   (instance ?DEVICE FileDevice)
   (capability SurfaceChange instrument ?DEVICE))

(subclass ElectricDevice Device)
(documentation ElectricDevice "Any &%Device that is powered by electricity.")

(subclass Radar ElectricDevice)
(documentation Radar "An &%ElectricDevice that emits and receives microwave
radiation for the purpose of locating and tracking distant objects.")

(=>
   (instance ?RADAR Radar)
   (hasPurpose ?RADAR (exists (?DISCOVER)
                         (and
                            (instance ?DISCOVER Discovering)
                            (instrument ?DISCOVER ?RADAR)))))

(subclass SecurityDevice Device)
(documentation SecurityDevice "A &%Device whose purpose is to protect people or
property from kidnappers and/or thieves.")

(subclass Lock SecurityDevice)
(documentation Lock "A &%Device, which, through a &%Key or a combination prevents
access to a &%Container or &%StationaryArtifact.")

(subclass Key SecurityDevice)
(documentation Key "A &%Device which opens and closes a &%Lock.")

(=>
   (instance ?KEY Key)
   (exists (?LOCK)
      (and
         (instance ?LOCK Lock)
         (capability Opening instrument ?LOCK)
         (capability Closing instrument ?LOCK))))

(subclass SecurityAlarm SecurityDevice)
(subclass SecurityAlarm ElectricDevice)
(documentation SecurityAlarm "A &%SecurityDevice that detects intrusions to
a &%StationaryArtifact and issues a warning of some sort.")

(subclass FiniteQuantity Quantity)
(documentation FiniteQuantity "Any &%Quantity that is limited or bounded in
magnitude.")

(subclass InfiniteQuantity Quantity)
(documentation InfiniteQuantity "Any &%Quantity that is not limited or bounded
in magnitude.")

(subclass Clock MeasuringDevice)
(documentation Clock "Any &%Device that measures and represents &%TimeDuration
or &%TimePosition.")

(=>
   (and
      (instance ?MEASURE Measuring)
      (result ?MEASURE ?QUANTITY)
      (instrument ?MEASURE ?CLOCK)
      (instance ?CLOCK Clock))
   (instance ?QUANTITY TimeMeasure))

(subclass Thermometer MeasuringDevice)
(documentation Thermometer "Any &%Device that measures and represents
&%TemperatureMeasure.")

(=>
   (and
      (instance ?MEASURE Measuring)
      (result ?MEASURE ?QUANTITY)
      (instrument ?MEASURE ?THERMOMETER)
      (instance ?THERMOMETER Thermometer))
   (instance ?QUANTITY TemperatureMeasure))

(subclass Telephone ElectricDevice)
(subclass Telephone CommunicationDevice)
(documentation Telephone "A &%Device that permits &%LinguisticCommunication
between remote points by converting sound into electrical signals that are
then transmitted.  When the signals are received, they are converted back
into sound.")

(subrelation telephoneNumber uniqueIdentifier)
(domain telephoneNumber 1 SymbolicString)
(domain telephoneNumber 2 Agent)
(relatedInternalConcept telephoneNumber address)
(documentation telephoneNumber "(&%telephoneNumber ?NUMBER ?AGENT) means
that ?NUMBER is a telephone number at which ?AGENT can be regularly contacted.")

(subclass LightFixture Device)
(documentation LightFixture "Any &%Device whose purpose is to be a source of
visible light.")

(=>
   (instance ?FIXTURE LightFixture)
   (capability RadiatingLight instrument ?FIXTURE))

(subclass PaintedPicture ArtWork)
(documentation PaintedPicture "Any &%ArtWork which is produced by &%Painting.")

(subclass Sketch ArtWork)
(documentation Sketch "Any &%ArtWork which is produced by a pencil or
piece of charcoal.")

(subclass Collage ArtWork)
(subclass Collage (ComplementFn RepresentationalArtWork))
(documentation Collage "Any abstract &%ArtWork that is produced by arranging
bits of paper or photographs.")

(subclass Painting Covering)
(subclass Painting Coloring)
(documentation Painting "The application of &%Paint to a &%surface.  Note that
this class covers both &%ArtPainting (the creation of &%PaintedPictures), as
well as painting one's kitchen, for example.")

(=>
   (and
      (instance ?PAINT Painting)
      (patient ?PAINT ?SURFACE)
      (instrument ?PAINT ?STUFF))
   (exists (?OBJ)
      (and
         (surface ?SURFACE ?OBJ)
         (instance ?STUFF Paint))))

(subclass Paint Solution)
(documentation Paint "Any &%Solution which is capable of &%Coloring something.")

(=>
   (instance ?PAINT Paint)
   (capability Coloring instrument ?PAINT))

(subclass ArtPainting ContentDevelopment)
(subclass ArtPainting Painting)
(documentation ArtPainting "Any &%ContentDevelopment that results in a
&%PaintedPicture.")

(=>
   (instance ?PAINT ArtPainting)
   (exists (?PICTURE)
      (and
         (instance ?PICTURE PaintedPicture)
         (result ?PAINT ?PICTURE))))

(subclass Drawing ContentDevelopment)
(subclass Drawing SurfaceChange)
(documentation Drawing "Any &%ContentDevelopment that results in a &%Sketch.")

(=>
   (instance ?DRAW Drawing)
   (exists (?SKETCH)
      (and
         (instance ?SKETCH Sketch)
         (result ?DRAW ?SKETCH))))

(subclass DisplayBoard Artifact)
(documentation DisplayBoard "A large board for posting information so
that it can be disseminated to the public.")

(=>
   (instance ?BOARD DisplayBoard)
   (hasPurpose ?BOARD (exists (?PUT ?DISSEMINATE ?INFO)
                         (and
                            (instance ?PUT Putting)
                            (instance ?DISSEMINATE Disseminating)
                            (instance ?INFO ContentBearingObject)
                            (patient ?PUT ?INFO)
                            (patient ?DISSEMINATE ?INFO)
                            (destination ?PUT ?BOARD)))))

(subclass Wheel Artifact)
(documentation Wheel "A circular &%Artifact which is a component of
&%LandVehicles and of some &%Devices.")

(=>
   (instance ?VEHICLE LandVehicle)
   (exists (?WHEEL)
      (and
         (instance ?WHEEL Wheel)
         (part ?WHEEL ?VEHICLE))))

(subclass Paper Artifact)
(documentation Paper "An &%Artifact made of cellulose pulp that is intended
to contain a &%Text.")

(=>
   (instance ?PAPER Paper)
   (hasPurpose ?PAPER (exists (?TEXT)
                         (and
                            (instance ?TEXT Text)
                            (part ?TEXT ?PAPER)))))

(subclass Wire Artifact)
(documentation Wire "A long, thin strand of &%Metal that is used in a wide
range of applications, including the wiring of electrical systems, creating
bundles and the construction of cages.")

(=>
   (instance ?WIRE Wire)
   (material Metal ?WIRE))

(subclass Plug Artifact)
(documentation Plug "An &%Artifact which is designed to fit snugly within
a &%Hole.")

(=>
   (instance ?PLUG Plug)
   (exists (?HOLE)
      (completelyFills ?PLUG ?HOLE)))

(subclass CigarOrCigarette Artifact)
(documentation CigarOrCigarette "A tube of thin paper containing finely ground tobacco
that is smoked.")

(subclass Pottery Artifact)
(documentation Pottery "Household &%Artifacts that are made out of baked &%Clay.")

(=>
   (instance ?POTTERY Pottery)
   (exists (?CLAY)
      (and
         (instance ?CLAY Clay)
         (part ?CLAY ?POTTERY))))

(subclass Furniture Artifact)
(disjoint Furniture Device)
(documentation Furniture "Any free-standing &%Artifacts which are not
&%Devices and which are used in day-to-day living.")

(subclass Seat Furniture)
(documentation Seat "Any instance of &%Furniture which is designed to
accommodate &%Humans who are &%Sitting.")

(=>
   (instance ?SEAT Seat)
   (hasPurpose ?SEAT (exists (?PERSON)
                        (and
                           (instance ?PERSON Human)
                           (located ?PERSON ?SEAT)
                           (attribute ?PERSON Sitting)))))

(subclass Chair Seat)
(documentation Chair "A &%Seat that is designed to accommodate a single
&%Human.")

(=>
   (instance ?CHAIR Chair)
   (not (exists (?PERSON1 ?PERSON2)
      (and
         (instance ?PERSON1 Human)
         (instance ?PERSON2 Human)
         (attribute ?PERSON1 Sitting)
         (attribute ?PERSON2 Sitting)
         (located ?PERSON1 ?CHAIR)
         (located ?PERSON2 ?CHAIR)
         (not (equal ?PERSON1 ?PERSON2))))))

(subclass Bed Furniture)
(documentation Bed "A piece of &%Furniture which is primarily for sleeping.")

(=>
   (instance ?BED Bed)
   (hasPurpose ?BED (exists (?PERSON)
                       (and
                          (attribute ?PERSON Asleep)
                          (located ?PERSON ?BED)))))

(subclass Table Furniture)
(documentation Table "A piece of &%Furniture with four legs and a flat top.
It is used either for eating, paperwork or meetings.")

(subclass Desk Table)
(documentation Desk "A &%Table for a single person which is intended to be
used for paperwork.")

(subclass Screw AttachingDevice)
(documentation Screw "An &%AttachingDevice which contains a spiral of grooves to
hold it in place and which is fastened with a screwdriver.")

(subclass Tape AttachingDevice)
(documentation Tape "A thin strip of &%Fabric or &%Paper that is used to attach
two things.")

(=>
   (instance ?TAPE Tape)
   (exists (?PART)
      (and
         (part ?PART ?TAPE)
         (or
            (instance ?PART Paper)
            (instance ?PART Fabric)))))

(subclass Holder Device)
(documentation Holder "A large class of &%Devices whose purpose is to hold
something else, i.e. be the &%instrument of a &%Keeping.")

(=>
   (instance ?DEVICE Holder)
   (capability Keeping instrument ?DEVICE))

(subclass Saddle Holder)
(documentation Saddle "A &%Device which allows a &%Human to ride on a
&%Horse.")

(=>
   (instance ?SADDLE Saddle)
   (capability (KappaFn ?RIDE (and
                                 (instance ?RIDE Carrying)
                                 (patient ?RIDE ?HUMAN)
                                 (instance ?HUMAN Human)
                                 (agent ?RIDE ?HORSE)
                                 (instance ?HORSE Horse))) instrument ?SADDLE))

(subclass Tray Holder)
(documentation Tray "A &%Holder that is designed for &%Food, dishes, and
flatware.")

(subclass Container Holder)
(documentation Container "Any &%Holder whose purpose is to contain
something else.  Note that &%Container is more specific in meaning
than &%Holder, because a &%Container must have a &%Hole that is at
least partially filled by the thing contained.")

(=>
   (instance ?CONTAINER Container)
   (hasPurpose ?CONTAINER (exists (?OBJ) (contains ?CONTAINER ?OBJ))))

(subclass Box Container)
(documentation Box "Any six-sided &%Container whose sides are
rectangular in shape.")

(=>
   (instance ?BOX Box)
   (equal (CardinalityFn (KappaFn ?SIDE (or
                                           (top ?SIDE ?BOX)
                                           (bottom ?SIDE ?BOX)
                                           (side ?SIDE ?BOX)))) 6))

(subclass TravelContainer Container)
(documentation TravelContainer "Any &%Container which is intended to be
used for carrying clothing, toiletries, and other personal effects that
would be needed on a overnight trip.")

(subclass FluidContainer Container)
(documentation FluidContainer "A &%Container which is used to store &%Fluids, i.e. &%Liquids and &%Gases.")

(=>
   (and
      (instance ?TANK TankContainer)
      (contains ?TANK ?STUFF))
   (attribute ?STUFF Fluid))

(subclass Bottle FluidContainer)
(documentation Bottle "A &%Container whose top is narrower than its bottom, which has no handle, and which is intended to store &%Liquids.")

(=>
   (and
      (instance ?BOTTLE Bottle)
      (contains ?BOTTLE ?STUFF))
   (attribute ?STUFF Liquid))

(=>
   (and
      (instance ?BOTTLE Bottle)
      (bottom ?BOTTOM ?BOTTLE)
      (top ?TOP ?BOTTLE)
      (width ?BOTTOM ?WIDTH1)
      (width ?TOP ?WIDTH2))
   (lessThan ?WIDTH2 ?WIDTH1))

(subclass Cup FluidContainer)
(documentation Cup "An open &%FluidContainer that is intended to serve a &%Beverage to a single person.  Note that this class includes both cups with handles and drinking glasses.")

(=>
   (and
      (instance ?CUP Cup)
      (contains ?CUP ?STUFF))
   (instance ?STUFF Beverage))

(subclass BoardOrBlock Artifact)
(documentation BoardOrBlock "A piece of material with flat, rectangular sides.
Note that boards and blocks are lumped into a single concept, because the
difference between these notions cannot be precisely defined.")

(=>
   (instance ?BOARD BoardOrBlock)
   (capability Constructing resource ?BOARD))

(subclass Nest CorpuscularObject)
(documentation Nest "Any structure which is created by nonhuman &%Animals for
the purpose of giving birth to their offspring.")

(=>
   (instance ?NEST Nest)
   (hasPurpose ?NEST (exists (?BIRTH)
                        (and
                           (instance ?BIRTH Birth)
                           (located ?BIRTH ?NEST)))))

(subclass OutdoorClothing Clothing)
(documentation OutdoorClothing "&%Clothing that is intended to be worn
outdoors.")

(=>
   (and
      (instance ?CLOTHING OutdoorClothing)
      (holdsDuring ?TIME (wears ?PERSON ?CLOTHING)))
   (not (exists (?BUILDING)
      (and
         (instance ?BUILDING Building)
         (holdsDuring ?TIME (located ?PERSON ?BUILDING))))))

(subclass Hat OutdoorClothing)
(documentation Hat "A type of &%Clothing that is worn on the &%Head.  Note
that this class covers caps, bonnets, berets, etc.")

(=>
   (and
      (wears ?PERSON ?HAT)
      (instance ?HAT Hat))
   (exists (?HEAD)
      (and
         (instance ?HEAD Head)
         (part ?HEAD ?PERSON)
         (meetsSpatially ?HAT ?HEAD))))

(subclass Coat OutdoorClothing)
(documentation Coat "&%Clothing that has sleeves and covers from the neck
down.  &%Coats are intended to be worn outdoors.")

(subclass Shoe Clothing)
(documentation Shoe "&%Clothing that is intended to be worn on the &%Foot.
It consists of an upper, a sole, and a heel.")

(=>
   (and
      (wears ?PERSON ?SHOE)
      (instance ?SHOE Shoe))
   (exists (?FOOT)
      (and
         (instance ?FOOT Foot)
         (part ?FOOT ?PERSON)
         (meetsSpatially ?SHOE ?FOOT))))

(subclass Shirt Clothing)
(documentation Shirt "An item of &%Clothing which covers the upper body of a
&%Human.")

(=>
   (and
      (instance ?SHIRT Shirt)
      (wears ?PERSON ?SHIRT))
   (instance ?PERSON Human))

(subclass Dress Clothing)
(documentation Dress "An item of &%Clothing which covers the lower body of a
&%Woman.")

(=>
   (and
      (instance ?DRESS Dress)
      (wears ?PERSON ?DRESS))
   (instance ?PERSON Woman))

(subclass ClothingSuit Collection)
(documentation ClothingSuit "A &%Collection of instances of &%Clothing that
are designed to be worn together.")

(=>
   (and
      (instance ?SUIT ClothingSuit)
      (member ?ITEM1 ?SUIT)
      (holdsDuring ?TIME (wears ?PERSON ?ITEM1)))
   (forall (?ITEM2)
      (=>
         (member ?ITEM2 ?SUIT)
         (holdsDuring ?TIME (wears ?PERSON ?ITEM2)))))

(subclass Leather Fabric)
(documentation Leather "A &%Fabric that is the result of tanning an &%Animal
&%Skin.")

(=>
   (instance ?LEATHER Leather)
   (exists (?MAKE)
      (and
         (instance ?MAKE Making)
         (resource ?MAKE ?SKIN)
         (instance ?SKIN Skin)
         (result ?MAKE ?LEATHER))))

(subclass Pocket Fabric)
(documentation Pocket "A pouch of &%Fabric in an instance of &%Clothing where
something can be kept.")

(=>
   (instance ?POCKET Pocket)
   (exists (?CLOTHING)
      (and
         (instance ?CLOTHING Clothing)
         (part ?POCKET ?CLOTHING))))

(=>
   (instance ?POCKET Pocket)
   (capability Keeping instrument ?POCKET))

(subclass Blanket Fabric)
(documentation Blanket "A piece of &%Fabric whose purpose is to keep a sleeping
person warm.")

(=>
   (instance ?BLANKET Blanket)
   (hasPurpose ?BLANKET (exists (?HEAT ?PERSON)
                           (and
                              (instance ?HEAT Heating)
                              (patient ?HEAT ?PERSON)
                              (instance ?PERSON Human)
                              (located ?PERSON ?BED)
                              (instance ?BED Bed)))))

(subclass Tying Attaching)
(documentation Tying "The &%Process of tying two things, or two strands of
the same thing, together.")

(subclass Untying Detaching)
(disjoint Untying Tying)
(documentation Untying "The &%Process of untying two things, or two strands
of the same thing.")

(instance patientMedical BinaryPredicate)
(domain patientMedical 1 Human)
(domain patientMedical 2 CognitiveAgent)
(documentation patientMedical "The relation of receiving medical care
from a recognized medical practitioner.  (&%patientMedical ?PATIENT
?DOCTOR) means that ?PATIENT is the patient of ?DOCTOR.  Note that
argument type restriction on the second argument is &%CognitiveAgent
to allow for cases where someone is the patient of an &%Organization,
viz. a &%CareOrganization.")

(=>
   (patientMedical ?PATIENT ?DOCTOR)
   (exists (?PROCESS)
      (and
         (patient ?PROCESS ?PATIENT)
         (agent ?PROCESS ?DOCTOR)
         (or
            (instance ?PROCESS DiagnosticProcess)
            (instance ?PROCESS TherapeuticProcess)))))

(subclass NonspecificDisease DiseaseOrSyndrome)
(documentation NonspecificDisease "The class of &%DiseaseOrSyndromes that are not
caused by a single type of &%Microorganism.")

(=>
   (instance ?DISEASE NonspecificDisease)
   (not (exists (?AGENT)
      (and
         (instance ?AGENT Microorganism)
         (forall (?VICTIM)
            (=>
               (attribute ?VICTIM ?DISEASE)
               (located ?AGENT ?VICTIM)))))))

(instance Cancer DiseaseOrSyndrome)
(documentation Cancer "A &%DiseaseOrSyndrome characterized by pathologic and
uncontrolled cell division that results in a &%Tumor.")

(=>
   (attribute ?PERSON Cancer)
   (exists (?TUMOR)
      (and
         (instance ?TUMOR Tumor)
         (part ?TUMOR ?PERSON))))

(instance conjugate BinaryPredicate)
(instance conjugate IrreflexiveRelation)
(instance conjugate SymmetricRelation)
(instance conjugate TransitiveRelation)
(domain conjugate 1 CompoundSubstance)
(domain conjugate 2 CompoundSubstance)
(documentation conjugate "(&%conjugate ?COMPOUND1 ?COMPOUND2) means that
?COMPOUND1 and ?COMPOUND2 are identical &%CompoundSubstances except that
one has one more &%Proton than the other.")

(=>
   (conjugate ?COMPOUND1 ?COMPOUND2)
   (exists (?NUMBER1 ?NUMBER2)
      (and
         (protonNumber ?COMPOUND1 ?NUMBER1)
         (protonNumber ?COMPOUND2 ?NUMBER2)
         (or
            (equal ?NUMBER1 (AdditionFn ?NUMBER2 1))
            (equal ?NUMBER2 (AdditionFn ?NUMBER1 1))))))

(subclass Biting Grabbing)
(documentation Biting "Any instance of &%Grabbing where the &%instrument is
the &%Mouth of the &%agent.")

(=>
   (and
      (instance ?BITE Biting)
      (agent ?BITE ?ANIMAL))
   (exists (?MOUTH)
      (and
         (instance ?MOUTH Mouth)
         (part ?MOUTH ?ANIMAL)
         (instrument ?BITE ?MOUTH))))

(subclass Spitting Impelling)
(documentation Spitting "Any instance of &%Impelling where the &%origin is
the &%Mouth of the &%agent.")

(=>
   (and
      (instance ?SPIT Spitting)
      (agent ?SPIT ?ANIMAL))
   (exists (?MOUTH)
      (and
         (instance ?MOUTH Mouth)
         (part ?MOUTH ?ANIMAL)
         (origin ?SPIT ?MOUTH))))

(subclass Kicking Impelling)
(documentation Kicking "Any instance of &%Impelling where the &%instrument
is a &%Foot of the &%agent.")

(=>
   (and
      (instance ?KICK Kicking)
      (agent ?KICK ?ANIMAL))
   (exists (?FOOT)
      (and
         (instance ?FOOT Foot)
         (part ?FOOT ?ANIMAL)
         (instrument ?KICK ?FOOT))))

(subclass Mailing Transfer)
(documentation Mailing "Any instance of &%Transfer where a postal system is
used to move the &%patient, either a letter or a package.")

(instance neighbor BinaryPredicate)
(instance neighbor SymmetricRelation)
(instance neighbor IrreflexiveRelation)
(domain neighbor 1 Human)
(domain neighbor 2 Human)
(documentation neighbor "(&%neighbor ?PERSON1 ?PERSON2) means that ?PERSON1 is
a neighbor of ?PERSON2, i.e. ?PERSON1 and ?PERSON2 have their &%homes &%Near
one another.")

(=>
   (neighbor ?PERSON1 ?PERSON2)
   (exists (?HOME1 ?HOME2)
      (and
         (home ?PERSON1 ?HOME1)
         (home ?PERSON2 ?HOME2)
         (not (equal ?HOME1 ?HOME2))
         (orientation ?HOME1 ?HOME2 Near))))

(instance capacity BinaryPredicate)
(domain capacity 1 SelfConnectedObject)
(domain capacity 2 ConstantQuantity)
(relatedInternalConcept capacity humanCapacity)
(documentation capacity "(&%capacity ?OBJ ?QUANTITY) means that ?OBJ can contain
something that has the &%measure of ?QUANTITY.  This predicate denotes maximal
capacity, i.e. ?OBJ can hold no more than ?QUANTITY.  Note, however, that this
does not mean that &%capacity is a &%SingleValuedRelation, since an object may
have various maximal capacities across different dimensions, e.g. a particular
box may have a &%capacity of 3 pounds and a &%capacity of 1 liter.")

(=>
   (and
      (measure ?OBJ1 ?MEAS)
      (contains ?OBJ2 ?OBJ1))
   (capacity ?OBJ2 ?MEAS))

(instance humanCapacity BinaryPredicate)
(instance humanCapacity SingleValuedRelation)
(domain humanCapacity 1 StationaryArtifact)
(domain humanCapacity 2 PositiveInteger)
(documentation humanCapacity "(&%humanCapacity ?CONSTRUCT ?NUMBER) means that the
&%StationaryArtifact ?CONSTRUCT, e.g. a &%Building or a &%Room, can hold a maximum
of ?NUMBER &%Humans without crowding.")

(instance LastFn UnaryFunction)
(domain LastFn 1 List)
(range LastFn Entity)
(documentation LastFn "(&%LastFn ?LIST) returns the last item in the
&%List ?LIST.  For example, (&%LastFn (&%ListFn &%Monday &%Tuesday
&%Wednesday)) would return the value of &%Wednesday.")

(<=>
   (and
      (instance ?LIST List)
      (equal (LastFn ?LIST) ?ITEM))
   (exists (?NUMBER)
      (and
         (equal (ListLengthFn ?LIST) ?NUMBER)
         (equal (ListOrderFn ?LIST ?NUMBER) ?ITEM))))

(instance FirstFn UnaryFunction)
(domain FirstFn 1 List)
(range FirstFn Entity)
(documentation FirstFn "(&%FirstFn ?LIST) returns the first item in
the &%List ?LIST.  For example, (&%FirstFn (&%ListFn &%Monday &%Tuesday
&%Wednesday)) would return the value of &%Monday.")

(=>
   (instance ?LIST List)
   (equal (FirstFn ?LIST) (ListOrderFn ?LIST 1)))

(instance address BinaryPredicate)
(domain address 1 Agent)
(domain address 2 Address)
(documentation address "(&%address ?AGENT ?ADDRESS) means that ?ADDRESS
is an address or part of an address for the &%Agent ?AGENT.  address relates an instance of a CognitiveAgent to the unique designation of a stationary artifact, location, or mail drop where that agent can be contacted.  This address includes buildings, room numbers, streets, mail drop numbers, and post office boxes, but excludes telephone numbers and other addresses that are not stationary.  This is not the most general type of address.")

(subclass Address RelationalAttribute)
(subclass Address Location)
(documentation Address "A &%RelationalAttribute that indicates an address
where an &%Agent can regularly be contacted.")

(subrelation postalCode uniqueIdentifier)
(domain postalCode 1 PositiveInteger)
(domain postalCode 2 Address)
(documentation postalCode "(&%postalCode ?NUMBER ?ADDRESS) means that the
the postal code, e.g. zip code, ?NUMBER is part of the address ?ADDRESS.")

(subrelation postalBoxNumber uniqueIdentifier)
(domain postalBoxNumber 1 PositiveInteger)
(domain postalBoxNumber 2 Address)
(documentation postalBoxNumber "(&%postalBoxNumber ?NUMBER ?ADDRESS) means
that the post office box ?NUMBER is part of the address ?ADDRESS.")

(subrelation cityAddress address)
(domain cityAddress 1 City)
(domain cityAddress 2 Address)
(documentation cityAddress "(&%cityAddress ?CITY ?ADDRESS) means that the
&%City ?CITY is part of the address ?ADDRESS.")

(subrelation streetAddress address)
(domain streetAddress 1 Roadway)
(domain streetAddress 2 Address)
(documentation streetAddress "(&%streetAddress ?STREET ?ADDRESS) means
that the &%Roadway ?STREET is part of the address ?ADDRESS.")

(subrelation streetNumber address)
(domain streetNumber 1 Building)
(domain streetNumber 2 Address)
(documentation streetNumber "(&%streetNumber ?BUILDING ?ADDRESS) means
that the &%Building ?BUILDING is part of the address ?ADDRESS.")

(subrelation unitNumber address)
(domain unitNumber 1 StationaryArtifact)
(domain unitNumber 2 Address)
(documentation unitNumber "(&%unitNumber ?UNIT ?ADDRESS) means that the
&%StationaryArtifact ?UNIT is part of the address ?ADDRESS.")

(instance StreetAddressFn QuaternaryFunction)
(domain StreetAddressFn 1 StationaryArtifact)
(domain StreetAddressFn 2 Roadway)
(domain StreetAddressFn 3 City)
(domain StreetAddressFn 4 Nation)
(range StreetAddressFn Agent)
(documentation StreetAddressFn "(&%StreetAddressFn ?BUILDING ?ROAD ?CITY
?COUNTRY) returns the &%Agent, e.g. a family, an organization, a person,
etc. that resides or conducts business at the corresponding &%address.")

(=>
   (equal (StreetAddressFn ?PLACE ?ROAD ?CITY ?COUNTRY) ?AGENT)
   (address ?AGENT ?PLACE))

(=>
   (equal (StreetAddressFn ?PLACE ?ROAD ?CITY ?COUNTRY) ?AGENT)
   (exists (?BUILDING)
      (and
         (instance ?BUILDING Building)
         (part ?PLACE ?BUILDING))))

(subclass ArtStudio StationaryArtifact)
(documentation ArtStudio "A &%Room, suite of &%Rooms or &%Building which is
devoted to the creation of &%ArtWorks.")

(=>
   (instance ?STUDIO ArtStudio)
   (hasPurpose ?STUDIO (exists (?MAKE)
                          (and
                             (instance ?MAKE Making)
                             (result ?MAKE ?WORK)
                             (instance ?WORK ArtWork)))))

(subclass Mine StationaryArtifact)
(documentation Mine "An construction in the earth from which &%Minerals are removed,
either in pure form or as part of ores.")

(=>
   (instance ?MINE Mine)
   (hasPurpose ?MINE (exists (?REMOVE ?MINERAL)
                        (and
                           (instance ?REMOVE Removing)
                           (patient ?REMOVE ?MINERAL)
                           (instance ?MINERAL Mineral)
                           (origin ?REMOVE ?MINE)))))

(subclass MobileResidence Artifact)
(disjoint MobileResidence Residence)
(documentation MobileResidence "Anything which serves to house people but
which changes its location from time to time, e.g. a motorhome, a mobile
home, a camp, etc.  Note that &%MobileResidence is disjoint from &%Residence,
because the latter is a subclass of &%StationaryArtifact.")

(subclass Camp MobileResidence)
(documentation Camp "A &%MobileResidence consisting of tents and other temporary
living quarters that is constructed on an undeveloped &%LandArea.")

(=>
   (instance ?CAMP Camp)
   (exists (?TENT)
      (and
         (instance ?TENT Tent)
         (part ?TENT ?CAMP))))

(subclass Tent MobileResidence)
(documentation Tent "A &%MobileResidence that is made of &%Fabric and poles and
can be easily assembled and disassembled.")

(=>
   (instance ?TENT Tent)
   (exists (?FABRIC)
      (and
         (instance ?FABRIC Fabric)
         (part ?FABRIC ?TENT))))

(subclass ExecutiveResidence PermanentResidence)
(documentation ExecutiveResidence "A &%Residence of a &%chiefOfState, e.g.
the White House, a state governor's mansion, Buckingham Palace, etc.")

(=>
   (instance ?RESIDENCE ExecutiveResidence)
   (exists (?AREA ?POSITION ?PERSON)
      (and
         (located ?RESIDENCE ?AREA)
         (home ?PERSON ?RESIDENCE)
         (chiefOfState ?AREA ?POSITION ?PERSON))))

(subclass ApartmentUnit SingleFamilyResidence)
(documentation ApartmentUnit "A &%SingleFamilyResidence that is not owned
by any member of the &%SocialUnit that lives there.")

(=>
   (and
      (instance ?UNIT ApartmentUnit)
      (home ?PERSON ?UNIT))
   (not (possesses ?PERSON ?UNIT)))

(=>
   (instance ?UNIT ApartmentUnit)
   (exists (?BUILDING)
      (and
         (instance ?BUILDING ApartmentBuilding)
         (part ?UNIT ?BUILDING))))

(subclass CondominiumUnit SingleFamilyResidence)
(documentation CondominiumUnit "A &%SingleFamilyResidence that may be owned
by a member of the &%SocialUnit that lives there.")

(=>
   (and
      (instance ?UNIT SingleFamilyResidence)
      (home ?PERSON ?UNIT)
      (possesses ?PERSON ?UNIT)
      (not (instance ?UNIT Building)))
   (instance ?UNIT CondominiumUnit))

(=>
   (instance ?UNIT CondominiumUnit)
   (exists (?BUILDING)
      (and
         (instance ?BUILDING CondominiumBuilding)
         (part ?UNIT ?BUILDING))))

(subclass House ResidentialBuilding)
(subclass House SingleFamilyResidence)
(documentation House "A &%ResidentialBuilding which is intended to be
inhabited by members of the same &%SocialUnit.  &%Houses are distinguished
from temporary housing like hotels and multi-family dwellings like condominium
and apartment buildings.")

(subclass ApartmentBuilding ResidentialBuilding)
(disjoint ApartmentBuilding SingleFamilyResidence)
(documentation ApartmentBuilding "A &%ResidentialBuilding containing
&%ApartmentUnits.")

(=>
   (instance ?BUILDING ApartmentBuilding)
   (exists (?UNIT1 ?UNIT2)
      (and
         (instance ?UNIT1 ApartmentUnit)
         (instance ?UNIT2 ApartmentUnit)
         (part ?UNIT1 ?BUILDING)
         (part ?UNIT2 ?BUILDING)
         (not (equal ?UNIT1 ?UNIT2)))))

(subclass CondominiumBuilding ResidentialBuilding)
(disjoint CondominiumBuilding SingleFamilyResidence)
(documentation CondominiumBuilding "A &%ResidentialBuilding containing
&%CondominiumUnits.")

(=>
   (instance ?BUILDING CondominiumBuilding)
   (exists (?UNIT1 ?UNIT2)
      (and
         (instance ?UNIT1 CondominiumUnit)
         (instance ?UNIT2 CondominiumUnit)
         (part ?UNIT1 ?BUILDING)
         (part ?UNIT2 ?BUILDING)
         (not (equal ?UNIT1 ?UNIT2)))))

(subclass AnimalResidence Artifact)
(documentation AnimalResidence "An &%Artifact which is intended to house
&%Animals and not &%Humans.  Note that an &%AnimalResidence may or may not
be a &%StationaryArtifact, e.g. a horse stall is stationary while a doghouse
generally is not.")

(=>
   (instance ?RESIDENCE AnimalResidence)
   (hasPurpose ?RESIDENCE (exists (?ANIMAL)
                             (and
                                (instance ?ANIMAL Animal)
                                (not (instance ?ANIMAL Human))
                                (inhabits ?ANIMAL ?RESIDENCE)))))

(subclass PlaceOfCommerce StationaryArtifact)
(partition PlaceOfCommerce CommercialBuilding CommercialUnit)
(documentation PlaceOfCommerce "A &%Building or part of a &%Building which is
intended for organizational activities, e.g. retail or wholesale selling,
manufacturing, office work, etc.")

(=>
   (instance ?OFFICE PlaceOfCommerce)
   (or
      (instance ?OFFICE CommercialBuilding)
      (exists (?BUILDING)
         (and
            (instance ?BUILDING CommercialBuilding)
            (part ?OFFICE ?BUILDING)))))

(subclass CommercialBuilding Building)
(subclass CommercialBuilding PlaceOfCommerce)
(documentation CommercialBuilding "A &%Building which is intended for
organizational activities, e.g. retail or wholesale selling, manufacturing,
office work, etc.")

(subclass CommercialUnit PlaceOfCommerce)
(documentation CommercialUnit "A &%Room or suite of &%Rooms intended for
clerical and/or professional work of a single &%Organization.")

(=>
   (and
      (instance ?UNIT CommercialUnit)
      (instance ?ORG Organization)
      (located ?ORG ?UNIT))
   (not (exists (?OTHER)
      (and
         (instance ?OTHER Organization)
         (located ?OTHER ?UNIT)
         (not (equal ?OTHER ?ORG))))))

(subclass Auditorium Building)
(documentation Auditorium "Any &%Building whose purpose is to hold concerts,
sports events, plays, etc. before an audience.  This class includes theaters,
sports stadiums, university auditoriums, etc.")

(=>
   (instance ?AUDITORIUM Auditorium)
   (hasPurpose ?AUDITORIUM (exists (?DEMO)
                              (and
                                 (instance ?DEMO Demonstrating)
                                 (located ?DEMO ?AUDITORIUM)))))

(=>
   (instance ?AUDITORIUM Auditorium)
   (exists (?STAGE)
      (and
         (instance ?STAGE PerformanceStage)
         (part ?STAGE ?AUDITORIUM))))

(subclass AuditoriumSeat Seat)
(documentation AuditoriumSeat "A &%Seat within an &%Auditorium from which one
can observe the &%PerformanceStage.")

(=>
   (instance ?SEAT AuditoriumSeat)
   (exists (?AUDITORIUM)
      (and
         (instance ?AUDITORIUM Auditorium)
         (part ?SEAT ?AUDITORIUM))))

(=>
   (and
      (instance ?SEAT AuditoriumSeat)
      (part ?SEAT ?AUDITORIUM)
      (part ?STAGE ?AUDITORIUM)
      (instance ?AUDITORIUM Auditorium)
      (instance ?STAGE PerformanceStage)
      (located ?PERSON ?SEAT)
      (instance ?PERSON Human)
      (subclass ?SEE Seeing)
      (forall (?INST)
         (=>
            (instance ?INST ?SEE)
            (patient ?INST ?STAGE))))
   (capability ?SEE agent ?PERSON))

(subrelation enjoys inScopeOfInterest)
(domain enjoys 1 CognitiveAgent)
(domainSubclass enjoys 2 IntentionalProcess)
(documentation enjoys "(&%enjoys ?AGENT ?PROCESS) means that the
&%CognitiveAgent ?AGENT tends to enjoy actions of type ?PROCESS,
i.e. tends to enjoy being the &%agent or &%experiencer of such
actions.")

(=>
   (enjoys ?AGENT ?PROCESS)
   (desires ?AGENT (exists (?INSTANCE)
                      (and
                         (instance ?INSTANCE ?PROCESS)
                         (or
                            (agent ?INSTANCE ?AGENT)
                            (experiencer ?INSTANCE ?AGENT))))))

(subrelation expects believes)
(documentation expects "Any belief about the future.  (&%expects
?AGENT ?BELIEF) means that (&%believes ?AGENT ?BELIEF) and, if
?BELIEF happens, it will happen in the future, i.e. after the
expectation.")

(=>
   (and
      (holdsDuring ?TIME1 (expects ?AGENT ?FORMULA))
      (holdsDuring ?TIME2 (true ?FORMULA True)))
   (earlier ?TIME1 ?TIME2))

(subrelation fears expects)
(documentation fears "(&%fears ?AGENT ?FORMULA) means that ?AGENT fears that
the proposition ?FORMULA will be true, i.e. he/she believes that it will
come to pass in the future and that it will be undesirable for ?AGENT.")

(=>
   (fears ?AGENT ?FORMULA)
   (not (desires ?AGENT ?FORMULA)))

(subrelation hopes expects)
(documentation hopes "(&%hopes ?AGENT ?FORMULA) means that ?AGENT hopes that
the proposition ?FORMULA will be true, i.e. he/she believes that it will
come to pass in the future and that it will be desirable for ?AGENT.")

(=>
   (hopes ?AGENT ?FORMULA)
   (desires ?AGENT ?FORMULA))

(instance doubts PropositionalAttitude)
(domain doubts 1 CognitiveAgent)
(domain doubts 2 Formula)
(documentation doubts "(&%doubts ?AGENT ?FORMULA) means that ?AGENT is unsure
about the truth of ?FORMULA, in particular ?AGENT does not believe that
?FORMULA is true.")

(=>
   (doubts ?AGENT ?FORMULA)
   (not (believes ?AGENT ?FORMULA)))

(instance dislikes ObjectAttitude)
(subrelation dislikes inScopeOfInterest)
(disjointRelation dislikes wants)
(relatedInternalConcept dislikes disapproves)
(domain dislikes 1 CognitiveAgent)
(domain dislikes 2 Object)
(documentation dislikes "(&%dislikes ?AGENT ?OBJECT) means that ?AGENT has a
feeling of antipathy to ?OBJECT, i.e. ?AGENT believes that ?OBJECT will
thwart one of his/her goals.  Note that there is no implication that what
is hated by an agent is not already possessed by the agent.")

(=>
   (dislikes ?AGENT ?OBJECT)
   (desires ?AGENT (not (possesses ?AGENT ?OBJECT))))

(instance disapproves PropositionalAttitude)
(subrelation disapproves inScopeOfInterest)
(disjointRelation disapproves desires)
(domain disapproves 1 CognitiveAgent)
(domain disapproves 2 Formula)
(documentation disapproves "(&%disapproves ?AGENT ?FORMULA) means that
?AGENT has a feeling of antipathy to the state of affairs represented by
?FORMULA, i.e. ?AGENT believes that the realization of ?FORMULA will
thwart one of his/her goals.  Note that there is no implication that what
is disapproved of by an agent is not already true.")

(=>
   (dislikes ?AGENT ?OBJECT)
   (desires ?AGENT (not (possesses ?AGENT ?OBJECT))))

(subrelation lacks needs)
(documentation lacks "(&%lacks ?AGENT ?OBJECT) means that ?AGENT &%needs
?OBJECT and it is not currently the case that ?AGENT &%possesses ?OBJECT.")

(=>
   (holdsDuring ?TIME (lacks ?AGENT ?OBJECT))
   (holdsDuring ?TIME (not (possesses ?AGENT ?OBJECT))))

(instance Antisemitism BeliefGroup)
(documentation Antisemitism "The &%BeliefGroup that is characterized by a
dislike for &%Judaism.")

(=>
   (member ?MEMBER Antisemitism)
   (dislikes ?MEMBER Judaism))

(instance DescendantsFn UnaryFunction)
(domain DescendantsFn 1 Human)
(range DescendantsFn FamilyGroup)
(documentation DescendantsFn "(&%DescendantsFn ?PERSON) denotes all and only
the descendants of ?PERSON, i.e. the &%Group consisting of ?OFFSPRING who
satisfy the following formula:  (&%ancestor ?OFFSPRING ?PERSON).")

(=>
   (and
      (instance ?PERSON Human)
      (equal (DescendantsFn ?PERSON) ?DESCENDANTS))
   (forall (?MEMBER)
      (<=>
         (member ?MEMBER ?DESCENDANTS)
         (ancestor ?MEMBER ?PERSON))))

(subclass GroupOfAnimals Group)
(documentation GroupOfAnimals "Any &%Group which contains exclusively
non-human &%members.")

(=>
   (instance ?GROUP GroupOfAnimals)
   (forall (?MEMBER)
      (=>
         (member ?MEMBER ?GROUP)
         (and
            (instance ?MEMBER Animal)
            (not (instance ?MEMBER Human))))))

(subclass Brood GroupOfAnimals)
(documentation Brood "A &%GroupOfAnimals that are all born at the same time
and to the same parents.")

(=>
   (instance ?BROOD Brood)
   (forall (?MEMBER1 ?MEMBER2)
      (=>
         (and
            (member ?MEMBER1 ?BROOD)
            (member ?MEMBER2 ?BROOD))
         (sibling ?MEMBER1 ?MEMBER2))))

(=>
   (instance ?BROOD Brood)
   (exists (?TIME)
      (forall (?MEMBER)
         (=>
            (member ?MEMBER ?BROOD)
            (exists (?BIRTH)
               (and
                  (instance ?BIRTH Birth)
                  (experiencer ?BIRTH ?MEMBER)
                  (equal ?TIME (WhenFn ?BIRTH))))))))

(subclass SportsTeam GroupOfPeople)
(documentation SportsTeam "A &%GroupOfPeople who habitually play a &%Sport
together, either as an occupation or as a leisure activity.")

(=>
   (instance ?TEAM SportsTeam)
   (exists (?SPORT)
      (and
	 (subclass ?SPORT Sport)
         (capability ?SPORT agent ?TEAM))))

(subrelation groupMember member)
(domain groupMember 1 Human)
(domain groupMember 2 GroupOfPeople)
(documentation groupMember "A &%subrelation of &%member, &%groupMember
is used to relate a &%Human to a &%GroupOfPeople of which he/she is a
&%member.")

(subclass CareOrganization Organization)
(partition CareOrganization Hospital MedicalClinic)
(documentation CareOrganization "Any &%Organization whose purpose is to
provide medical care for for &%Humans who reside there, either permanently
or temporarily.")

(=>
   (instance ?ORG CareOrganization)
   (hasPurpose ?ORG (exists (?PATIENT) (medicalPatient ?PATIENT ?ORG))))

(subclass HospitalBuilding TemporaryResidence)
;; (subclass Hospital CareOrganization)
;; commented out due to disjoint conflict -- PJC
(documentation HospitalBuilding "The building facility of a Hospital (A &%CareOrganization) where patients reside for a short period of time while they undergo treatment for a disease or disorder.")

(subclass Hospital CareOrganization)
;; (subclass Hospital TemporaryResidence)
;; comented out due to disjoint conflict - PJC. see HospitalBuilding
(documentation Hospital "A &%CareOrganization that maintains a &%HospitalBuilding (which see)a where patients reside for a short period of time while they undergo treatment for a disease or disorder.")

(subclass MedicalClinic CareOrganization)
(subclass MedicalClinic (ComplementFn Residence))
(documentation MedicalClinic "A &%CareOrganization which provides medical care
on an out-patient basis only, i.e. there are no rooms where patients may take
up residence for a period of time while they receive care.")

(subclass Proprietorship Corporation)
(documentation Proprietorship "A &%Corporation that is owned by a single person.")

(=>
   (instance ?CORP Proprietorship)
   (exists (?HUMAN)
      (and
         (instance ?HUMAN Human)
         (possesses ?HUMAN ?CORP)
         (forall (?PERSON)
            (=>
               (possesses ?PERSON ?CORP)
               (equal ?PERSON ?HUMAN))))))

(subclass Restaurant Corporation)
(documentation Restaurant "Any &%Corporation whose services include selling
&%Food to customers which is intended to be eaten on the premises.")

(=>
   (instance ?COMPANY Restaurant)
   (exists (?SERVICE ?FOOD)
      (and
         (instance ?SERVICE CommercialService)
         (agent ?SERVICE ?COMPANY)
         (instance ?SERVICE Selling)
         (patient ?SERVICE ?FOOD)
         (instance ?FOOD Food))))

(subclass Cafeteria Restaurant)
(documentation Cafeteria "Any &%Restaurant which does not offer table service.
&%Food is selected and purchased at a central counter.")

(subclass Tavern Restaurant)
(documentation Tavern "A &%Restaurant whose primary service is selling
&%AlcoholicBeverages to customers.")

(=>
   (instance ?COMPANY Tavern)
   (exists (?SERVICE ?BEVERAGE)
      (and
         (instance ?SERVICE CommercialService)
         (agent ?SERVICE ?COMPANY)
         (instance ?SERVICE Selling)
         (patient ?SERVICE ?BEVERAGE)
         (instance ?BEVERAGE Beverage))))

(subclass TransportationCompany Corporation)
(documentation TransportationCompany "Any &%Corporation whose services
include &%Transportation, e.g. a &%RailroadCompany, an airline, a cruise
ship line, etc.")

(=>
   (instance ?COMPANY TransportationCompany)
   (exists (?SERVICE)
      (and
         (instance ?SERVICE CommercialService)
         (agent ?SERVICE ?COMPANY)
         (instance ?SERVICE Transportation))))

(subclass RailroadCompany TransportationCompany)
(documentation RailroadCompany "Any &%TransportationCompany whose services
include &%Transportation by &%Train.")

(=>
   (instance ?COMPANY RailroadCompany)
   (exists (?SERVICE)
      (and
         (instance ?SERVICE CommercialService)
         (agent ?SERVICE ?COMPANY)
         (instance ?SERVICE Transportation)
         (instrument ?SERVICE ?TRAIN)
         (instance ?TRAIN Train))))

(subclass WholesaleStore MercantileOrganization)
(documentation WholesaleStore "A &%MercantileOrganization that sells its
goods exclusively to &%Corporations.")

(=>
   (and
      (instance ?STORE WholesaleStore)
      (customer ?CUSTOMER ?STORE))
   (instance ?CUSTOMER Corporation))

(subclass RetailStore MercantileOrganization)
(disjoint RetailStore WholesaleStore)
(documentation RetailStore "The complement of &%WholesaleStore, i.e.
&%MercantileOrganizations that sell their goods to the general public.")

(subclass GroceryStore RetailStore)
(documentation GroceryStore "A &%RetailStore that sells &%Food, and perhaps
other items as well.")

(=>
   (instance ?STORE GroceryStore)
   (exists (?SELL)
      (and
         (instance ?SELL Selling)
         (agent ?SELL ?STORE)
         (patient ?SELL ?FOOD)
         (instance ?FOOD Food))))

(subclass OfferingForSale Offering)
(documentation OfferingForSale "&%Offering to sell something to someone.")

(=>
   (and
      (instance ?SALE OfferingForSale)
      (agent ?SALE ?AGENT))
   (patient ?SALE (exists (?SELL ?OBJ)
                     (and
                        (instance ?SELL Selling)
                        (patient ?SELL ?OBJ)
                        (agent ?SELL ?AGENT)))))

(subclass BargainSale Offering)
(documentation BargainSale "&%Offering to sell something to someone at a
reduced price.")

(=>
   (and
      (instance ?SALE BargainSale)
      (patient ?SALE ?OBJ)
      (instance ?OBJ Object))
   (exists (?PRICE1 ?PRICE2 ?AGENT1 ?AGENT2)
      (and
         (holdsDuring (ImmediatePastFn (WhenFn ?SALE)) (price ?OBJ ?PRICE1 ?AGENT1))
         (holdsDuring (WhenFn ?SALE) (price ?OBJ ?PRICE2 ?AGENT2))
         (lessThan ?PRICE2 ?PRICE1))))

(subclass HolyBible Book)
(documentation HolyBible "Any instance of the collection of writings which is regarded as scripture by those who embrace &%Christianity.")

(subclass MissionOrganization ReligiousOrganization)
(documentation MissionOrganization "The class of &%ReligiousOrganizations
that send members to foreign countries with the aim of coverting citizens
of those countries to the beliefs of the &%ReligiousOrganization.")

(subclass ReligiousBuilding Building)
(documentation ReligiousBuilding "A &%Building which is intended to be
used for religious worship.  This class covers churches, temples,
religious shrines, etc.")

(=>
   (instance ?BUILDING ReligiousBuilding)
   (hasPurpose ?BUILDING (exists (?SERVICE)
                            (and
                               (instance ?SERVICE ReligiousService)
                               (located ?SERVICE ?BUILDING)))))

(subclass Steeple StationaryArtifact)
(documentation Steeple "A component of a &%ReligiousBuilding that is tall
and narrow and symbolizes the connection between humanity and a deity.")

(=>
   (instance ?STEEPLE Steeple)
   (exists (?BUILDING)
      (and
         (instance ?BUILDING Building)
         (part ?STEEPLE ?BUILDING))))

(subclass ReligiousService ReligiousProcess)
(subclass ReligiousService Demonstrating)
(documentation ReligiousService "A formal process of public worship which is
typically carried out in a church, temple or other sanctified building and
which typically accords with a prescribed set of rules.")

(subclass ChristianService ReligiousService)
(documentation ChristianService "Any &%ReligiousService that is conducted by
&%members of &%Christianity.")

(=>
   (and
      (instance ?SERVICE ChristianService)
      (agent ?SERVICE ?PERSON)
      (instance ?PERSON Human))
   (member ?PERSON Christianity))

(subclass Praying ReligiousProcess)
(subclass Praying Requesting)
(documentation Praying "A formal or informal process of private worship which
may or may not be carried out in a &%ReligiousBuilding.")

(subclass PartyPlatform FactualText)
(documentation PartyPlatform "A &%Text which is authored by a &%PoliticalParty
and which contains the core goals and principles of the &%PoliticalParty for a
particular year or election cycle.")

(=>
   (and
      (subclass ?PLATFORM PartyPlatform)
      (authors ?PARTY ?PLATFORM)
      (instance ?PARTY Organization))
   (instance ?PARTY PoliticalParty))

(=>
   (and
      (subclass ?PLATFORM PartyPlatform)
      (authors ?PARTY ?PLATFORM)
      (instance ?PARTY PoliticalParty)
      (instance ?INST ?PLATFORM)
      (containsInformation ?INST ?PROP))
   (believes ?PARTY ?PROP))

(instance ResidentFn UnaryFunction)
(domain ResidentFn 1 GeopoliticalArea)
(range ResidentFn GroupOfPeople)
(documentation ResidentFn "(&%ResidentFn ?AREA) denotes the
&%GroupOfPeople who have their &%home in ?AREA.")

(=>
   (member ?PERSON (ResidentFn ?AREA))
   (home ?PERSON ?AREA))

(subrelation CitizenryFn ResidentFn)
(domain CitizenryFn 1 GeopoliticalArea)
(range CitizenryFn GroupOfPeople)
(documentation CitizenryFn "(&%CitizenryFn ?AREA) denotes the
&%GroupOfPeople who are legal and permanent residents of the
&%GeopoliticalArea ?AREA.")

(=>
   (and
      (instance ?AREA GeopoliticalArea)
      (equal ?CITIZENRY (CitizenryFn ?AREA))
      (equal ?POPULATION (ResidentFn ?AREA)))
   (greaterThanOrEqualTo ?POPULATION ?CITIZENRY))

(instance PerCapitaFn BinaryFunction)
(domain PerCapitaFn 1 Quantity)
(domain PerCapitaFn 2 GeopoliticalArea)
(range PerCapitaFn Quantity)
(documentation PerCapitaFn "(&%PerCapitaFn ?AREA ?QUANTITY) denotes the
average amount of ?QUANTITY possessed by a resident of &%GeopoliticalArea
?AREA.")

(=>
   (equal (PerCapitaFn ?AREA ?QUANTITY1) ?QUANTITY2)
   (exists (?POPULATION)
      (and
         (equal ?POPULATION (CardinalityFn (ResidentFn ?AREA)))
         (equal ?QUANTITY2 (DivisionFn ?QUANTITY1 ?POPULATION)))))

(subclass CityDistrict GeopoliticalArea)
(documentation CityDistrict "Any &%geopoliticalSubdivision of a &%City.")

(<=>
   (instance ?DISTRICT CityDistrict)
   (exists (?CITY)
      (and
         (instance ?CITY City)
         (geopoliticalSubdivision ?DISTRICT ?CITY))))

(subclass Downtown CityDistrict)
(documentation Downtown "The commercial center of a &%City.  The part of the
&%City that contains more shops and offices than any other part.")

(subclass Park LandArea)
(documentation Park "A publicly owned &%LandArea which is intended to be used
for recreation and/or exercise.")

(=>
   (instance ?PARK Park)
   (exists (?GOV)
      (and
         (instance ?GOV Government)
         (possesses ?GOV ?PARK))))

(=>
   (instance ?PARK Park)
   (hasPurpose ?PARK (exists (?REC)
                        (and
                           (instance ?REC RecreationOrExercise)
                           (located ?REC ?PARK)))))

(subclass County GeopoliticalArea)
(subclass County LandArea)
(documentation County "A &%GeopoliticalArea that is a subdivision of a
&%StateOrProvince.")

(=>
   (instance ?COUNTY County)
   (exists (?STATE)
      (and
         (instance ?STATE StateOrProvince)
         (part ?COUNTY ?STATE))))

(subclass AmericanState StateOrProvince)
(documentation AmericanState "The class of states that make up the
&%UnitedStates.")

(=>
   (instance ?STATE AmericanState)
   (part ?STATE UnitedStates))

(subclass AmericanCity City)
(documentation AmericanCity "The class of cities that are in the
&%UnitedStates.")

(=>
   (instance ?CITY AmericanCity)
   (part ?CITY UnitedStates))

(subclass EuropeanCity City)
(documentation EuropeanCity "The class of cities that are in &%Europe.")

(=>
   (instance ?CITY EuropeanCity)
   (part ?CITY Europe))

(subclass GovernmentSecretary Position)
(documentation GovernmentSecretary "The class of &%Positions where the position holder
is head of an adminstrative department of &%Government.")

(subclass StateGovernment Government)
(documentation StateGovernment "The class of &%Governments whose
jurisdictions are &%StateOrProvinces.")

(=>
   (instance ?GOVERNMENT StateGovernment)
   (exists (?STATE)
      (and
         (instance ?STATE StateOrProvince)
         (equal (GovernmentFn ?STATE) ?GOVERNMENT))))

(subclass MilitaryService MilitaryOrganization)
(documentation MilitaryService "A branch of the armed forces of a &%Nation.
For example, there are five military services in the United States, the army,
the navy, the air force, the marines, and the coast guard.")

(subclass Army MilitaryService)
(documentation Army "&%MilitaryServices that are land forces.")

(subclass MilitaryUnit MilitaryOrganization)
(documentation MilitaryUnit "Any &%MilitaryOrganization that can be dispatched
to an area of operations.")

(subclass InfantryUnit MilitaryUnit)
(documentation InfantryUnit "A &%MilitaryUnit composed primarily of &%Soldiers
who fight on foot, i.e. without the use of heavy artillery.")

(subclass MilitaryDivision MilitaryUnit)
(documentation MilitaryDivision "Any &%MilitaryUnit with the rank of
division.")

(subclass MilitaryBrigade MilitaryUnit)
(documentation MilitaryBrigade "Any &%MilitaryUnit with the rank of
brigade.")

(subclass MilitaryCompany MilitaryUnit)
(documentation MilitaryCompany "Any &%MilitaryUnit with the rank of
company.")

(subclass MilitaryRegiment MilitaryUnit)
(documentation MilitaryRegiment "Any &%MilitaryUnit with the rank of
regiment.")

(subclass Commission Organization)
(documentation Commission "A small, temporary &%Organization whose purpose
is to investigate some issue.")

(=>
   (instance ?COMMISSION Commission)
   (hasPurpose ?COMMISSION (exists (?INVESTIGATE ?ISSUE)
                              (and
                                 (instance ?INVESTIGATE Investigating)
                                 (patient ?INVESTIGATE ?ISSUE)
                                 (agent ?INVESTIGATE ?COMMISSION)))))

(subclass ServiceOrganization Organization)
(documentation ServiceOrganization "An &%Organization that performs
a public service and is regulated by the &%Government.")

(=>
   (instance ?ORG ServiceOrganization)
   (exists (?PROC ?GOV)
      (and
         (instance ?PROC RegulatoryProcess)
         (patient ?PROC ?ORG)
         (agent ?PROC ?GOV)
         (instance ?GOV Government))))

(subclass OrganizationalBoard Organization)
(documentation OrganizationalBoard "Part of an &%Organization that
is responsible for managing the &%Organization.")

(=>
   (instance ?BOARD OrganizationalBoard)
   (exists (?ORG ?MANAGE)
      (and
         (subOrganization ?BOARD ?ORG)
         (instance ?MANAGE Managing)
         (agent ?MANAGE ?BOARD)
         (patient ?MANAGE ?ORG))))

(subclass SecurityUnit Organization)
(documentation SecurityUnit "The &%Organization that is charged with
ensuring the security of members of the overall &%Organization and the
property of the &%Organization.")

(=>
   (and
      (instance ?UNIT SecurityUnit)
      (subOrganization (OrganizationFn ?UNIT) ?ORG))
   (holdsObligation (exists (?MAINTAIN)
                       (and
                          (instance ?MAINTAIN Maintaining)
                          (agent ?MAINTAIN ?UNIT)
                          (patient ?MAINTAIN ?ORG)))))

(subclass UnionOrganization Organization)
(documentation UnionOrganization "An &%Organization comprised of workers from
the same &%Corporation or &%Industry.  The purpose of the &%UnionOrganization
is to strengthen its representation in bargaining with the &%Corporation or
&%Industry.")

(=>
   (instance ?ORG UnionOrganization)
   (exists (?COLL)
      (=>
         (member ?MEMBER ?ORG)
         (or
            (and
               (instance ?COLL Corporation)
               (employs ?COLL ?MEMBER))
            (exists (?CORP)
               (and
                  (instance ?COLL Industry)
                  (member ?CORP ?COLL)
                  (employs ?CORP ?MEMBER)))))))

(subclass OrganicCompound CompoundSubstance)
(documentation OrganicCompound "Any &%CompoundSubstance that has a &%Carbon
base.")

(=>
   (and
      (instance ?COMPOUND OrganicCompound)
      (instance ?COMPOUND Molecule))
   (exists (?CARBON)
      (and
         (instance ?CARBON Carbon)
         (part ?CARBON ?COMPOUND))))

(subclass Alcohol OrganicCompound)
(documentation Alcohol "&%OrganicCompounds that are produced from hydrocarbons
by distillation.")

(subclass SodiumChloride CompoundSubstance)
(documentation SodiumChloride "The &%CompoundSubstance formed from &%Sodium
and &%Chlorine.")

(=>
   (instance ?SALT SodiumChloride)
   (exists (?SYNTHESIS ?SODIUM ?CHLORINE)
      (and
         (instance ?SYNTHESIS ChemicalSynthesis)
         (resource ?SYNTHESIS ?SODIUM)
         (instance ?SODIUM Sodium)
         (resource ?SYNTHESIS ?CHLORINE)
         (instance ?CHLORINE Chlorine)
         (result ?SYNTHESIS ?SALT))))

(subclass SalineSolution Solution)
(documentation SalineSolution "A &%Solution consisting of &%SodiumChloride and
&%Water.")

(=>
   (and
      (instance ?SOLUTION SalineSolution)
      (part ?PART ?SOLUTION))
   (or
      (instance ?PART SodiumChloride)
      (instance ?PART Water)))

(=>
   (instance ?WATER Water)
   (exists (?SYNTHESIS ?HYDROGEN ?OXYGEN)
      (and
         (instance ?SYNTHESIS ChemicalSynthesis)
         (resource ?SYNTHESIS ?HYDROGEN)
         (instance ?HYDROGEN Hydrogen)
         (resource ?SYNTHESIS ?OXYGEN)
         (instance ?OXYGEN Oxygen)
         (result ?SYNTHESIS ?WATER))))

(subclass Oil Solution)
(documentation Oil "A greasy, viscous &%Solution that cannot be mixed with &%Water.
Note that this general class covers petroleum oil, vegetable oil, animal fat, etc.")

(=>
   (instance ?OIL Oil)
   (not (exists (?MIX ?WATER)
      (and
         (instance ?MIX Mixture)
         (part ?WATER ?MIX)
         (instance ?WATER Water)
         (part ?OIL ?MIX)))))

(subclass Detergent Mixture)
(documentation Detergent "Any &%Mixture whose purpose is to remove &%Soil
and/or other undesirable substances from the surfaces of objects.")

(=>
   (instance ?DETERGENT Detergent)
   (hasPurpose ?DETERGENT (exists (?REMOVE ?SUBSTANCE ?SURFACE ?OBJECT)
                             (and
                                (instance ?REMOVE Removing)
                                (instance ?SUBSTANCE Substance)
                                (patient ?REMOVE ?SUBSTANCE)
                                (origin ?REMOVE ?SURFACE)
                                (surface ?SURFACE ?OBJECT)))))

(subclass Glue Mixture)
(documentation Glue "Any &%Mixture whose purpose is to be used as the &%instrument
of &%Attaching one thing to another.")

(=>
   (instance ?GLUE Glue)
   (hasPurpose ?GLUE (exists (?ATTACH)
                            (and
                               (instance ?ATTACH Attaching)
                               (instrument ?ATTACH ?GLUE)))))

(subclass Glass Mixture)
(documentation Glass "A transparent or translucent &%Mixture of silicates.")

(subclass MetallicAlloy Mixture)
(documentation MetallicAlloy "A &%Mixture of two or more &%Metals, and possibly
nonmetallic elements as well.  For example, steel is an alloy containing iron
and manganese.")

(=>
   (instance ?ALLOY MetallicAlloy)
   (exists (?METAL1 ?METAL2)
      (and
         (instance ?METAL1 Metal)
         (instance ?METAL2 Metal)
         (not (equal ?METAL1 ?METAL2))
         (part ?METAL1 ?ALLOY)
         (part ?METAL2 ?ALLOY))))

(subclass Steel MetallicAlloy)
(documentation Steel "A &%MetallicAlloy made from &%Iron and other elements.")

(=>
   (instance ?STEEL Steel)
   (exists (?IRON)
      (and
         (instance ?IRON Iron)
         (part ?IRON ?STEEL))))

(subclass Brass MetallicAlloy)
(documentation Brass "A &%MetallicAlloy made from &%Copper and &%Zinc.")

(=>
   (instance ?BRASS Brass)
   (exists (?COPPER ?ZINC)
      (and
         (instance ?COPPER Copper)
         (instance ?ZINC Zinc)
         (part ?COPPER ?BRASS)
         (part ?ZINC ?BRASS))))

(subclass Powder Substance)
(documentation Powder "Any &%Solid &%Substance which consists of loose,
identical, and very small particles.")

(=>
   (instance ?POWDER Powder)
   (attribute ?POWDER Solid))

(subclass Fallout Powder)
(documentation Fallout "Radioactive powder that is typically dispersed by
the explosion of a nuclear weapon.")

(=>
   (instance ?POWDER Fallout)
   (capability RadiatingNuclear instrument ?POWDER))

(subclass Fog WaterCloud)
(documentation Fog "Any &%WaterCloud that is in contact with the ground.")

(=>
   (instance ?FOG Fog)
   (exists (?LAND)
      (and
         (instance ?LAND LandArea)
         (meetsSpatially ?FOG ?LAND))))

(subclass Ice Water)
(documentation Ice "&%Water that has the &%PhysicalState of &%Solid.")

(<=>
   (instance ?ICE Ice)
   (and
      (instance ?ICE Water)
      (attribute ?ICE Solid)))

(=>
   (and
      (instance ?ICE Ice)
      (measure ?ICE (MeasureFn ?NUMBER CelsiusDegree)))
   (lessThanOrEqualTo ?NUMBER 0))

(subclass Field LandArea)
(documentation Field "A &%LandArea that has been cleared of &%BotanicalTrees.  Note that a &%Field is not necessarily used for the cultivation of crops and that a &%Field may be very small, e.g. &%Lawn is a subclass of &%Field.")

(=>
   (instance ?FIELD Field)
   (not (exists (?TREE)
      (and
         (instance ?TREE BotanicalTree)
         (located ?TREE ?FIELD)))))

(subclass Lawn Field)
(documentation Lawn "A &%Field of cultivated and mowed &%Grass.")

(=>
   (instance ?LAWN Lawn)
   (exists (?GRASS)
      (and
         (instance ?GRASS Grass)
         (located ?GRASS ?LAWN))))

(subclass MilitaryFront GeographicArea)
(documentation MilitaryFront "A &%GeographicArea along which opposing military
forces confront one another in a &%Battle.")

(=>
   (holdsDuring ?TIME (instance ?AREA MilitaryFront))
   (exists (?BATTLE)
      (and
         (instance ?BATTLE Battle)
         (located ?BATTLE ?AREA)
         (temporalPart ?TIME ?BATTLE))))

(subclass SpaceRegion Region)
(disjoint SpaceRegion GeographicArea)
(partition SpaceRegion AtmosphericRegion OuterSpaceRegion)
(documentation SpaceRegion "The class of all &%Regions which are not
&%GeographicAreas.")

(subclass OuterSpaceRegion SpaceRegion)
(documentation OuterSpaceRegion "The class of all &%Regions which are
neither &%GeographicAreas nor &%AtmosphericRegions.")

(subclass StormFront AtmosphericRegion)
(documentation StormFront "The &%Region where two or more unstable air
masses meet.")

(=>
   (instance ?FRONT StormFront)
   (exists (?AIR1 ?AIR2)
      (and
         (instance ?AIR1 Air)
         (instance ?AIR2 Air)
         (between ?AIR1 ?FRONT ?AIR2))))

(instance Outside Region)
(documentation Outside "Any &%Region which is not enclosed by a &%Building
or part of a &%Building.")

(=>
   (instance ?OUTSIDE Outside)
   (not (exists (?BUILDING ?THING)
      (and
         (instance ?BUILDING Building)
         (located ?THING ?BUILDING)
         (located ?THING ?OUTSIDE)))))

(subclass BiologicalSpecies Class)
(documentation BiologicalSpecies "The &%Class of all biological species, i.e.
the class of all classes of &%Organism whose instances can interbreed.")

(=>
   (instance ?SPECIES BiologicalSpecies)
   (subclass ?SPECIES Organism))

(subclass Wood PlantSubstance)
(documentation Wood "A &%Tissue that comprises the inner trunk of &%Trees.
It is often used in constructing &%Buildings and other &%Artifacts; Wood is a natural substance obtained by processing trees, typically by removing the outer portions of the trunk or of thick branches.  For commercial purposes Wood may be considered as non-living.  The 'Wood' (woody substance) which may contain living cells in a living tree should therefore be labeled by a different name.")

(subclass Opium BiologicallyActiveSubstance)
(subclass Opium PlantSubstance)
(documentation Opium "A substance harvested from the seed capsules of the
opium poppy that contains various powerful alkaloids.")

(subclass Grass FloweringPlant)
(documentation Grass "&%FloweringPlants with green, narrow leaves that are
used for lawns and &%Fields.")

(subclass Tumor AbnormalAnatomicalStructure)
(documentation Tumor "Any &%AbnormalAnatomicalStructure which consists of a
mass of &%Tissue.  Note that this class covers both malignant (i.e. cancerous)
and benign tumors.")

(=>
   (instance ?TUMOR Tumor)
   (forall (?PART)
      (=>
         (part ?PART ?TUMOR)
         (instance ?PART Tissue))))

(subclass AnimalSubstance BodySubstance)
(documentation AnimalSubstance "&%BodySubstances that are produced
exclusively by &%Animals.")

(=>
   (and
      (instance ?SUBSTANCE AnimalSubstance)
      (instance ?ANIMAL Organism)
      (part ?SUBSTANCE ?ANIMAL))
   (instance ?ANIMAL Animal))

(subclass PlantSubstance BodySubstance)
(documentation PlantSubstance "&%BodySubstances that are produced
exclusively by &%Plants.")

(=>
   (and
      (instance ?SUBSTANCE PlantSubstance)
      (instance ?PLANT Organism)
      (part ?SUBSTANCE ?PLANT))
   (instance ?PLANT Plant))

(subclass Sweat AnimalSubstance)
(documentation Sweat "An &%AnimalSubstance that contains &%SodiumChloride
and is produced by the sweat glands.")

(=>
   (instance ?SWEAT Sweat)
   (exists (?PART)
      (and
         (instance ?PART SodiumChloride)
         (part ?PART ?SWEAT))))

(subclass PlantLeaf PlantAnatomicalStructure)
(subclass PlantLeaf Organ)
(documentation PlantLeaf "An &%Organ of &%Plants whose main purpose is
photosynthesis.")

(subclass PlantBranch PlantAnatomicalStructure)
(subclass PlantBranch BodyPart)
(documentation PlantBranch "The stem of a &%Plant or any shoot arising from
the stem of a &%Plant.")

(subclass PlantRoot PlantAnatomicalStructure)
(subclass PlantRoot Organ)
(documentation PlantRoot "An &%Organ of &%Plants whose main purpose is
twofold, viz. to absorb nutrients from the ground and to anchor the &%Plant
in place.")

(subclass Flower PlantAnatomicalStructure)
(subclass Flower Organ)
(documentation Flower "The reproductive organ of &%FloweringPlants.")

(=>
   (instance ?FLOWER Flower)
   (exists (?PLANT)
      (and
         (instance ?PLANT FloweringPlant)
         (part ?FLOWER ?PLANT))))

(subclass Antibody Protein)
(documentation Antibody "An immunoglobulin which is produced by the body
and which has the ability to neutralize &%Antigens.")

(=>
   (instance ?BODY Antibody)
   (hasPurpose ?BODY (exists (?DEST)
                        (and
                           (instance ?DEST Destruction)
                           (agent ?DEST ?BODY)
                           (patient ?DEST ?ANTI)
                           (instance ?ANTI Antigen)))))

(subclass Antigen BiologicallyActiveSubstance)
(documentation Antigen "Any &%BiologicallyActiveSubstance that has the
capacity to stimulate the production of &%Antibodies.")

(subclass LiquidBodySubstance BodySubstance)
(documentation LiquidBodySubstance "Any &%BodySubstance which is &%Liquid
under normal circumstances.")

(=>
   (instance ?SUBSTANCE LiquidBodySubstance)
   (attribute ?SUBSTANCE Liquid))

(subclass Serum LiquidBodySubstance)
(disjoint Serum Blood)
(documentation Serum "Any &%LiquidBodySubstance other than &%Blood.")

(subclass Milk LiquidBodySubstance)
(subclass Milk Beverage)
(documentation Milk "A nutritious &%BodySubstance produced by &%Mammals.")

(=>
   (instance ?MILK Milk)
   (exists (?PROCESS ?MAMMAL)
      (and
         (instance ?MAMMAL Mammal)
         (instrument ?PROCESS ?MAMMAL)
         (result ?PROCESS ?MILK))))

(subclass CellNucleus OrganicObject)
(documentation CellNucleus "The part of the &%Cell that contains DNA and
RNA.")

(=>
   (instance ?NUCLEUS CellNucleus)
   (exists (?CELL)
      (and
         (instance ?CELL Cell)
         (part ?NUCLEUS ?CELL))))

(subclass AlcoholicBeverage Beverage)
(documentation AlcoholicBeverage "Any &%Beverage that contains &%Alcohol.")

(=>
   (instance ?BEVERAGE AlcoholicBeverage)
   (exists (?ALCOHOL)
      (and
         (instance ?ALCOHOL Alcohol)
         (part ?ALCOHOL ?BEVERAGE))))

(subclass DistilledAlcoholicBeverage AlcoholicBeverage)
(documentation DistilledAlcoholicBeverage "An &%AlcoholicBeverage that has
had some part of its &%Water content removed by distillation.  This class
covers drinks of unmixed, hard liquor.")

(=>
   (instance ?BEVERAGE DistilledAlcoholicBeverage)
   (exists (?REMOVE ?WATER)
      (and
         (instance ?REMOVE Removing)
         (patient ?REMOVE ?WATER)
         (instance ?WATER Water)
         (origin ?REMOVE ?BEVERAGE))))

(subclass Whiskey DistilledAlcoholicBeverage)
(documentation Whiskey "A &%DistilledAlcoholicBeverage that is prepared by
distilling fermented grain mash.")

(subclass Beer AlcoholicBeverage)
(documentation Beer "An &%AlcoholicBeverage that is prepared by fermenting
malt and hops.")

(subclass SpinalColumn Organ)
(subclass SpinalColumn AnimalAnatomicalStructure)
(documentation SpinalColumn "A flexible column made out of bones called
vertebrae. The main function of the &%SpinalColumn is to protect the
spinal cord.")

(<=>
   (instance ?VERT Vertebrate)
   (exists (?SPINE)
      (and
         (instance ?VERT Animal)
         (component ?SPINE ?VERT)
         (instance ?SPINE SpinalColumn))))

(subclass Skin BodyCovering)
(subclass Skin AnimalAnatomicalStructure)
(documentation Skin "A BodyCovering that comprises part of the surface
of &%Animals.")

(=>
   (instance ?SKIN Skin)
   (exists (?SURFACE ?ANIMAL)
      (and
         (surface ?SURFACE ?ANIMAL)
         (part ?SKIN ?ANIMAL)
         (instance ?ANIMAL Animal)
         (overlapsSpatially ?SKIN ?SURFACE))))

(subclass BronchialDuct BodyVessel)
(subclass BronchialDuct AnimalAnatomicalStructure)
(documentation BronchialDuct "Any &%BodyVessel which is located in a
&%Lung and which carries oxygen from the trachea to the alveoli.")

(=>
   (instance ?DUCT BronchialDuct)
   (exists (?LUNG)
      (and
         (instance ?LUNG Lung)
         (located ?DUCT ?LUNG))))

(subclass BloodVessel BodyVessel)
(subclass BloodVessel AnimalAnatomicalStructure)
(documentation BloodVessel "Any &%BodyVessel which is used to circulate
&%Blood from one part of the body to another.")

(=>
   (instance ?VESSEL BloodVessel)
   (exists (?BLOOD ?TRANSFER)
      (and
         (instance ?BLOOD Blood)
         (instance ?TRANSFER Transfer)
         (patient ?TRANSFER ?BLOOD)
         (instrument ?TRANSFER ?VESSEL))))

(subclass Artery BloodVessel)
(documentation Artery "Any &%BloodVessel which transfers &%Blood from
the &%Heart to the extremities of the body.")

(=>
   (and
      (instance ?ARTERY Artery)
      (instance ?TRANSFER Transfer)
      (patient ?TRANSFER ?BLOOD)
      (instrument ?TRANSFER ?ARTERY)
      (instance ?BLOOD Blood))
   (exists (?HEART)
      (and
         (instance ?HEART Heart)
         (origin ?TRANSFER ?HEART))))

(subclass PulmonaryArtery Artery)
(documentation PulmonaryArtery "An &%Artery that carries &%Blood from
the &%Heart to a &%Lung.")

(=>
   (and
      (instance ?ARTERY PulmonaryArtery)
      (instance ?TRANSFER Transfer)
      (patient ?TRANSFER ?BLOOD)
      (instrument ?TRANSFER ?ARTERY)
      (instance ?BLOOD Blood))
   (exists (?LUNG)
      (and
         (instance ?LUNG Lung)
         (destination ?TRANSFER ?LUNG))))

(subclass Lung Organ)
(subclass Lung AnimalAnatomicalStructure)
(documentation Lung "A respiratory organ of &%Vertebrates.  Its function is
to furnish the blood with oxygen and to remove carbon dioxide.")

(=>
   (instance ?VERT Vertebrate)
   (exists (?LUNG)
      (and
         (component ?LUNG ?VERT)
         (instance ?LUNG Lung))))

(=>
   (capability Breathing experiencer ?ANIMAL)
   (exists (?LUNG)
      (and
         (component ?LUNG ?ANIMAL)
         (instance ?LUNG Lung))))

(subclass Heart Organ)
(subclass Heart AnimalAnatomicalStructure)
(documentation Heart "The &%Organ that pumps &%Blood throughout the body.")

(=>
   (instance ?HEART Heart)
   (exists (?TRANSFER ?BLOOD)
      (and
         (instance ?TRANSFER Transfer)
         (instance ?BLOOD Blood)
         (instrument ?TRANSFER ?HEART)
         (patient ?TRANSFER ?BLOOD))))

(subclass Liver Organ)
(subclass Liver AnimalAnatomicalStructure)
(documentation Liver "An &%Organ that secretes bile and serves metabolic
functions.")

(subclass Mouth AnimalAnatomicalStructure)
(subclass Mouth BodyPart)
(documentation Mouth "Part of the &%Face, used for &%Ingesting &%Food
and &%Vocalizing.")

(=>
   (instance ?MOUTH Mouth)
   (exists (?FACE)
      (and
         (instance ?FACE Face)
         (part ?FACE ?MOUTH))))

(subclass Tongue AnimalAnatomicalStructure)
(subclass Tongue BodyPart)
(documentation Tongue "Part of the &%Mouth, used for &%Tasting &%Food,
&%Vocalizing, and the initial stage of &%Digesting.")

(=>
   (instance ?TONGUE Tongue)
   (exists (?MOUTH)
      (and
         (instance ?MOUTH Mouth)
         (part ?TONGUE ?MOUTH))))

(subclass Tooth Bone)
(documentation Tooth "Part of the &%Mouth, used for biting and chewing.")

(=>
   (instance ?TOOTH Tooth)
   (exists (?MOUTH)
      (and
         (instance ?MOUTH Mouth)
         (part ?TOOTH ?MOUTH))))

(subclass Chewing BodyMotion)
(documentation Chewing "Breaking up or mashing &%Food with one's teeth.")

(=>
   (and
      (instance ?CHEW Chewing)
      (resource ?CHEW ?FOOD))
   (instance ?FOOD Food))

(=>
   (and
      (instance ?CHEW Chewing)
      (instrument ?CHEW ?TOOTH))
   (instance ?TOOTH Tooth))

(=>
   (instance ?CHEW Chewing)
   (exists (?EAT)
      (and
         (instance ?EAT Eating)
         (subProcess ?CHEW ?EAT))))

(subclass Lip AnimalAnatomicalStructure)
(subclass Lip BodyPart)
(documentation Lip "Folds of &%Tissue surrounding the mouths of some
&%Vertebrates.")

(=>
   (instance ?LIP Lip)
   (exists (?MOUTH)
      (and
         (instance ?MOUTH Mouth)
         (part ?LIP ?MOUTH))))

(subclass Kissing Touching)
(documentation Kissing "The class of &%Touching processes where the lips
of two persons are brought into contact with each other.")

(=>
   (instance ?KISS Kissing)
   (exists (?PERSON1 ?PERSON2 ?LIP1 ?LIP2)
      (and
         (agent ?KISS ?PERSON1)
         (agent ?KISS ?PERSON2)
         (instance ?PERSON1 Human)
         (instance ?PERSON2 Human)
         (not (equal ?PERSON1 ?PERSON2))
         (instance ?LIP1 Lip)
         (instance ?LIP2 Lip)
         (part ?LIP1 ?PERSON1)
         (part ?LIP2 ?PERSON2)
         (holdsDuring (BeginFn (WhenFn ?KISS)) (not (meetsSpatially ?LIP1 ?LIP2)))
         (holdsDuring (EndFn (WhenFn ?KISS)) (meetsSpatially ?LIP1 ?LIP2)))))

(subclass Skeleton AnimalAnatomicalStructure)
(subclass Skeleton BodyPart)
(documentation Skeleton "The system of &%Bones that make up the supporting structure
of &%Vertebrates.")

(=>
   (and
      (instance ?ANIMAL Animal)
      (instance ?SKELETON Skeleton)
      (part ?SKELETON ?ANIMAL))
   (instance ?ANIMAL Vertebrate))

(<=>
   (instance ?BONE Bone)
   (exists (?SKELETON)
      (and
         (instance ?SKELETON Skeleton)
         (part ?BONE ?SKELETON))))

(subclass Throat AnimalAnatomicalStructure)
(subclass Throat BodyVessel)
(documentation Throat "A &%BodyVessel which connects the &%Mouth to the
lungs and stomach.")

(=>
   (instance ?THROAT Throat)
   (exists (?MOUTH)
      (and
         (instance ?MOUTH Mouth)
         (connected ?THROAT ?MOUTH))))

(subclass Hair AnimalAnatomicalStructure)
(documentation Hair "A filament that covers part of the body of many
&%Mammals.")

(=>
   (instance ?HAIR Hair)
   (exists ( ?MAMMAL ?TIME)
      (and
         (instance ?MAMMAL Mammal)
         (holdsDuring ?TIME (part ?HAIR ?MAMMAL)))))

(subclass HairRemoval Removing)
(documentation HairRemoval "&%Removing (some or all) the &%Hair from the
body of an &%Animal.  Note that this covers shaving hair, cutting hair,
pulling hair out by the roots, etc.")

(=>
   (and
      (instance ?REMOVE HairRemoval)
      (resource ?REMOVE ?HAIR))
   (instance ?HAIR Hair))

(subclass Brain Organ)
(subclass Brain AnimalAnatomicalStructure)
(documentation Brain "The seat of the central nervous system.")

(subclass Stomach Organ)
(subclass Stomach AnimalAnatomicalStructure)
(documentation Stomach "A muscular sac that is the principal organ of
digestion.")

(=>
   (instance ?STOMACH Stomach)
   (capability Digesting instrument ?STOMACH))

(subclass Hypothalamus BodyPart)
(subclass Hypothalamus AnimalAnatomicalStructure)
(documentation Hypothalamus "The part of the &%Brain lying below the
thalamus that serves to regulate &%AutonomicProcesses.")

(=>
   (instance ?HYPO Hypothalamus)
   (exists (?BRAIN)
      (and
         (instance ?BRAIN Brain)
         (part ?HYPO ?BRAIN))))

(subclass Eye Organ)
(subclass Eye AnimalAnatomicalStructure)
(documentation Eye "The &%Organ of sight.")

(=>
   (instance ?EYE Eye)
   (capability Seeing instrument ?EYE))

(=>
   (instance ?EYE Eye)
   (exists (?HEAD)
      (and
         (instance ?HEAD Head)
         (part ?EYE ?HEAD))))

(subclass Ear Organ)
(subclass Ear AnimalAnatomicalStructure)
(documentation Ear "The &%Organ of hearing.")

(=>
   (instance ?EAR Ear)
   (capability Hearing instrument ?EAR))

(=>
   (instance ?EAR Ear)
   (exists (?HEAD)
      (and
         (instance ?HEAD Head)
         (part ?EAR ?HEAD))))

(subclass Nose Organ)
(subclass Nose AnimalAnatomicalStructure)
(documentation Nose "The &%Organ of &%Smelling.")

(=>
   (instance ?NOSE Nose)
   (capability Smelling instrument ?NOSE))

(=>
   (instance ?NOSE Nose)
   (exists (?FACE)
      (and
         (instance ?FACE Face)
         (part ?NOSE ?FACE))))

(subclass ThyroidGland Gland)
(documentation ThyroidGland "A &%Gland in the neck that produces
&%HormoneTSH, which regulates body weight, metabolic rate, etc.")

(subclass HormoneTSH Hormone)
(documentation HormoneTSH "A &%Hormone secreted by the &%ThyroidGland.")

(=>
   (instance ?HORMONE HormoneTSH)
   (exists (?PROC ?GLAND)
      (and
         (instance ?GLAND ThyroidGland)
         (instrument ?PROC ?GLAND)
         (result ?PROC ?HORMONE))))

(subclass Arm Limb)
(documentation Arm "The upper &%Limbs of a &%Primate.")

(=>
   (instance ?ARM Arm)
   (exists (?PRIMATE)
      (and
         (instance ?PRIMATE Primate)
         (part ?ARM ?PRIMATE))))

(subclass Hand AnimalAnatomicalStructure)
(subclass Hand BodyPart)
(documentation Hand "The grasping, fingered part of an upper limb of a
&%Primate.")

(=>
   (instance ?HAND Hand)
   (exists (?ARM)
      (and
         (instance ?ARM Arm)
         (part ?HAND ?ARM))))

(subclass Finger AnimalAnatomicalStructure)
(subclass Finger BodyPart)
(documentation Finger "The five extremities of &%Hands.")

(=>
   (instance ?FINGER Finger)
   (exists (?HAND)
      (and
         (instance ?HAND Hand)
         (part ?FINGER ?HAND))))

(subclass Limb AnimalAnatomicalStructure)
(subclass Limb BodyPart)
(documentation Limb "Any of the limbs of a &%Vertebrate.")

(=>
   (instance ?LIMB Limb)
   (exists (?VERTEBRATE)
      (and
         (instance ?VERTEBRATE Vertebrate)
         (part ?LIMB ?VERTEBRATE))))

(subclass Snake Reptile)
(documentation Snake "A long and narrow &%Reptile which lacks &%Limbs.")

(=>
   (instance ?SNAKE Snake)
   (not (exists (?LIMB)
      (and
         (instance ?LIMB Limb)
         (part ?LIMB ?SNAKE)))))

(subclass ConstrictorSnake Snake)
(documentation ConstrictorSnake "A &%Snake that lacks venom and kills its
prey by crushing it to death.")

(subclass Anaconda ConstrictorSnake)
(documentation Anaconda "A very large Boa that is found in South America.")

(subclass Bee Insect)
(documentation Bee "A hairy &%Insect, some species of which produce honey
and/or sting.")

(subclass BumbleBee Bee)
(documentation BumbleBee "A large &%Bee which lacks a stinger.")

(subclass QueenInsect Insect)
(documentation QueenInsect "A &%Female &%Insect which is the sole member of
her colony with the capability to reproduce.")

(=>
   (instance ?INSECT QueenInsect)
      (and
         (attribute ?INSECT Female)
         (capability Replication agent ?INSECT)))

(=>
   (instance ?INSECT QueenInsect)
   (exists (?GROUP)
      (and
         (instance ?GROUP Group)
         (member ?INSECT ?GROUP)
         (not (exists (?MEMBER)
                 (and
                    (member ?MEMBER ?GROUP)
                    (capability Replication agent ?MEMBER)
                    (not (equal ?MEMBER ?INSECT))))))))

(subclass Leg Limb)
(documentation Leg "The lower &%Limbs of &%Primates.")

(=>
   (instance ?LEG Leg)
   (exists (?PRIMATE)
      (and
         (instance ?PRIMATE Primate)
         (part ?LEG ?PRIMATE))))

(subclass Foot AnimalAnatomicalStructure)
(subclass Foot BodyPart)
(documentation Foot "The lower part of a &%Limb, the part which makes contact
with the ground in locomotion of the &%Animal.")

(=>
   (instance ?FOOT Foot)
   (exists (?LIMB)
      (and
         (instance ?LIMB Leg)
         (part ?FOOT ?LIMB))))

(subclass Toe AnimalAnatomicalStructure)
(subclass Toe BodyPart)
(documentation Toe "The five extremities of a &%Foot.")

(=>
   (instance ?TOE Toe)
   (exists (?FOOT)
      (and
         (instance ?FOOT Foot)
         (part ?TOE ?FOOT))))

(subclass Knee AnimalAnatomicalStructure)
(subclass Knee BodyJunction)
(documentation Knee "The joint in the &%Leg connecting the tibia and fibula
with the femur.")

(=>
   (instance ?KNEE Knee)
   (exists (?LEG)
      (and
         (instance ?LEG Leg)
         (part ?KNEE ?LEG))))

(subclass Shoulder AnimalAnatomicalStructure)
(subclass Shoulder BodyPart)
(documentation Shoulder "The part of a &%Primate between the &%Arm and
the neck.")

(=>
   (instance ?SHOULDER Shoulder)
   (exists (?PRIMATE)
      (and
         (instance ?PRIMATE Primate)
         (part ?SHOULDER ?PRIMATE))))

(subclass Torso AnimalAnatomicalStructure)
(subclass Torso BodyPart)
(documentation Torso "The body of a &%Primate excluding its &%Limbs.")

(=>
   (and
      (instance ?TORSO Torso)
      (instance ?LIMB Limb))
   (not (overlapsSpatially ?TORSO ?LIMB)))

(subclass Head AnimalAnatomicalStructure)
(subclass Head BodyPart)
(documentation Head "The part of the body containing the sense organs and
the brain.")

(subclass Neck AnimalAnatomicalStructure)
(subclass Neck BodyPart)
(documentation Neck "The part of the body that connects the &%Head to the
rest of the body.")

(=>
   (instance ?NECK Neck)
   (exists (?HEAD)
      (and
         (instance ?HEAD Head)
         (connected ?NECK ?HEAD))))

(subclass Face AnimalAnatomicalStructure)
(subclass Face BodyPart)
(documentation Face "The part of the &%Head from forehead to chin and
from ear to ear.")

(=>
   (instance ?FACE Face)
   (exists (?HEAD)
      (and
         (instance ?HEAD Head)
         (part ?FACE ?HEAD))))

(=>
   (instance ?FACE Face)
   (exists (?VERTEBRATE)
      (and
         (instance ?VERTEBRATE Vertebrate)
         (part ?FACE ?VERTEBRATE))))

(subclass Chin AnimalAnatomicalStructure)
(subclass Chin BodyPart)
(documentation Chin "A part of the &%Face which protrudes slightly and which
is lower than all other parts of the &%Face.")

(=>
   (instance ?CHIN Chin)
   (exists (?FACE)
      (and
         (instance ?FACE Face)
         (part ?CHIN ?FACE))))

(=>
   (instance ?CHIN Chin)
   (forall (?PART)
      (=>
         (and
            (part ?PART ?FACE)
            (not (part ?PART ?CHIN)))
         (orientation ?PART ?CHIN Below))))


(subclass ExpressingApproval Expressing)
(documentation ExpressingApproval "&%Expressing favor about a physical thing
or a state of affairs.")

(=>
   (and
      (instance ?EXPRESS ExpressingApproval)
      (agent ?EXPRESS ?AGENT)
      (patient ?EXPRESS ?THING))
   (or
      (wants ?AGENT ?THING)
      (desires ?AGENT ?THING)))

(subclass ExpressingDisapproval Expressing)
(documentation ExpressingDisapproval "&%Expressing disfavor about a physical
thing or a state of affairs.")

(=>
   (and
      (instance ?EXPRESS ExpressingDisapproval)
      (agent ?EXPRESS ?AGENT)
      (patient ?EXPRESS ?THING))
   (or
      (dislikes ?AGENT ?THING)
      (disapproves ?AGENT ?THING)))

(subclass FacialExpression Gesture)
(documentation FacialExpression "Any &%Gesture whose &%instrument is the &%Face.")

(=>
   (and
      (instance ?EXPRESS FacialExpression)
      (agent ?EXPRESS ?AGENT))
   (exists (?FACE)
      (and
         (part ?FACE ?AGENT)
         (instance ?FACE Face)
         (instrument ?EXPRESS ?FACE))))

(subclass Smiling FacialExpression)
(documentation Smiling "Spreading the lips in such a way as to convey
happiness.")

(=>
   (and
      (instance ?SMILE Smiling)
      (agent ?SMILE ?AGENT))
   (holdsDuring (WhenFn ?SMILE) (attribute ?AGENT Happiness)))

(subclass Frowning FacialExpression)
(documentation Frowning "Furrowing the forehead in such a way as to convey
unhappiness.")

(=>
   (and
      (instance ?FROWN Frowning)
      (agent ?FROWN ?AGENT))
   (holdsDuring (WhenFn ?FROWN) (attribute ?AGENT Unhappiness)))

(subclass Laughing Vocalizing)
(subclass Laughing FacialExpression)
(documentation Laughing "Expressing happiness by &%Vocalizing in a
certain way.")

(=>
   (instance ?LAUGH Laughing)
   (exists (?SMILE)
      (and
         (instance ?SMILE Smiling)
         (subProcess ?SMILE ?LAUGH))))

(subclass Weeping FacialExpression)
(documentation Weeping "&%Expressing unhappiness by shedding tears.")

(=>
   (and
      (instance ?WEEP Weeping)
      (agent ?WEEP ?AGENT))
   (holdsDuring (WhenFn ?WEEP) (attribute ?AGENT Unhappiness)))

(subclass Nodding Gesture)
(documentation Nodding "Moving the &%Head up and down or side to side
to indicate approval or disapproval.")

(=>
   (and
      (instance ?NOD Nodding)
      (patient ?NOD ?HEAD))
   (instance ?HEAD Head))

(subclass Waving Gesture)
(documentation Waving "Moving a &%Hand to indicate a greeting, farewell,
recognition, goodwill, etc.")

(=>
   (and
      (instance ?WAVE Waving)
      (patient ?WAVE ?HAND))
   (instance ?HAND Hand))

(subclass Bowing Gesture)
(subclass Bowing MotionDownward)
(documentation Bowing "Any downward motion of the body that indicates respect
for or submission to another &%Agent.")

(subclass Ducking IntentionalProcess)
(subclass Ducking BodyMotion)
(subclass Ducking MotionDownward)
(documentation Ducking "Purposely moving one's body downward in such a way as
to avoid being hit by something.")

(=>
   (and
      (instance ?MOTION Ducking)
      (agent ?MOTION ?AGENT))
   (hasPurpose ?MOTION (not (exists (?IMPACT)
                          (and
                             (instance ?IMPACT Impacting)
                             (patient ?IMPACT ?AGENT))))))

(subclass Thanking ExpressingInLanguage)
(documentation Thanking "Any &%ExpressingInLanguage of appreciation to a person
for something that the person did in the past.")

(=>
   (and
      (instance ?THANK Thanking)
      (agent ?THANK ?AGENT)
      (patient ?THANK ?THING)
      (destination ?THANK ?PERSON))
   (and
      (instance ?PERSON Human)
      (or
         (holdsDuring (WhenFn ?THANK) (wants ?AGENT ?THING))
         (holdsDuring (WhenFn ?THANK) (desires ?AGENT ?THING)))))

(subclass Greeting Expressing)
(documentation Greeting "Any instance of &%Expressing an acknowledgment of a
person's arrival.  Note that this class is not a subclass of &%ExpressingInLanguage,
because it covers gestures of greeting, e.g. &%Waving and &%Nodding in certain
circumstances.")

(subrelation half part)
(documentation half "(&%half ?HALF ?WHOLE) means that ?HALF is one half
of ?WHOLE.")

(=>
   (half ?HALF ?WHOLE)
   (exists (?OTHER)
      (and
         (half ?OTHER ?WHOLE)
         (not (equal ?OTHER ?HALF))
         (equal ?WHOLE (MereologicalSumFn ?HALF ?OTHER)))))

(subrelation most part)
(documentation most "(&%most ?MOST ?WHOLE) means that ?MOST is a &%part
of ?WHOLE that is greater than &%half of ?WHOLE.")

(=>
   (most ?MOST ?WHOLE)
   (exists (?HALF ?NUMBER1 ?NUMBER2 ?UNIT)
      (and
         (half ?HALF ?WHOLE)
         (measure ?HALF (MeasureFn ?NUMBER1 ?UNIT))
         (measure ?MOST (MeasureFn ?NUMBER2 ?UNIT))
         (greaterThan ?NUMBER2 ?NUMBER1))))

(subclass Blueprint Icon)
(documentation Blueprint "An &%Icon which is a scale model of an &%Artifact,
whether the &%Artifact actually exists or not.")

(=>
   (instance ?PLAN Blueprint)
   (exists (?ARTIFACT)
      (and
         (instance ?ARTIFACT Artifact)
         (represents ?PLAN ?ARTIFACT))))

(subclass GraphIcon Icon)
(documentation GraphIcon "An &%Icon which depicts one or more quantities.")

(=>
   (instance ?GRAPH Graph)
   (exists (?QUANTITY)
      (and
         (instance ?QUANTITY PhysicalQuantity)
         (refers ?GRAPH ?QUANTITY))))

(subclass Flag Icon)
(documentation Flag "An &%Icon made of &%Fabric that refers to a particular
&%GeopoliticalArea.")

(=>
   (instance ?FLAG Flag)
   (exists (?FABRIC)
      (and
         (instance ?FABRIC Fabric)
         (part ?FABRIC ?FLAG))))

(=>
   (instance ?FLAG Flag)
   (exists (?AREA)
      (and
         (instance ?AREA GeopoliticalArea)
         (refers ?FLAG ?AREA))))

(subclass ArrowIcon Icon)
(documentation ArrowIcon "An &%Icon which has the shape of an arrow and which
is used to indicate direction or a relationship betwee two things.")

(subclass Photograph Icon)
(documentation Photograph "An &%Icon that is the result of a process of
&%Photographing.")

(subclass Photographing ContentDevelopment)
(documentation Photographing "&%ContentDevelopment where the &%instrument
is a camera and the &%result is a &%Photograph.")

(=>
   (instance ?SHOOT Photographing)
   (exists (?PHOTO)
      (and
         (instance ?PHOTO Photograph)
         (result ?SHOOT ?PHOTO)
         (instrument ?SHOOT ?CAMERA)
         (instance ?CAMERA Camera))))

(subclass Camera Device)
(documentation Camera "A &%Device which is capable of &%Photographing.")

(=>
   (instance ?CAMERA Camera)
   (capability Photographing instrument ?CAMERA))

(subclass Composing ContentDevelopment)
(documentation Composing "&%ContentDevelopment which results in a
&%MusicalComposition.")

(=>
   (instance ?COMPOSE Composing)
   (exists (?MUSIC)
      (and
         (instance ?MUSIC MusicalComposition)
         (result ?COMPOSE ?MUSIC))))

(subclass TonMass MassMeasure)
(instance TonMass UnitOfMeasure)
(documentation TonMass "English mass unit that is equal to 2000 pounds.")

(=>
   (instance ?NUMBER RealNumber)
   (equal
        (MeasureFn ?NUMBER TonMass)
        (MeasureFn (MultiplicationFn ?NUMBER 2000) PoundMass)))

(subclass Page InformationMeasure)
(instance Page UnitOfMeasure)
(documentation Page "A single page of &%Text.")

(subclass LiquorShot VolumeMeasure)
(instance LiquorShot UnitOfMeasure)
(documentation LiquorShot "A &%UnitOfMeasure used in preparing &%AlcoholicBeverages.")

(subclass Acre AreaMeasure)
(instance Acre UnitOfMeasure)
(documentation Acre "A &%UnitOfMeasure equal to 4840 square yards.")

(subclass MusicalComposition Text)
(documentation MusicalComposition "A &%Text that expresses the notes,
words, etc. of a song or other sort of &%Music.")

(=>
   (and
      (instance ?COMP MusicalComposition)
      (containsInformation ?COMP ?INFO)
      (realization ?MUSIC ?INFO))
   (instance ?MUSIC Music))

(subclass DeviceAttribute ObjectiveNorm)
(documentation DeviceAttribute "This class contains two &%Attributes to
indicate whether a &%Device is or is not behaving as it is intended to
behave, &%Functioning and &%Malfunctioning.")

(=>
   (and
      (attribute ?DEVICE ?ATTRIBUTE)
      (instance ?ATTRIBUTE DeviceAttribute))
   (instance ?DEVICE Device))

(instance Functioning DeviceAttribute)
(contraryAttribute Functioning Malfunctioning)
(documentation Functioning "Indicates that a &%Device is performing its
intended function.")

(=>
   (and
      (attribute ?DEVICE Functioning)
      (hasPurpose ?DEVICE ?PROP))
   (true ?PROP True))

(instance Malfunctioning DeviceAttribute)
(documentation Malfunctioning "Indicates that a &%Device is not performing
its intended function.")

(=>
   (and
      (attribute ?DEVICE Malfunctioning)
      (hasPurpose ?DEVICE ?PROP))
   (true ?PROP False))

(instance LegislativeBill DeonticAttribute)
(documentation LegislativeBill "The &%Attribute of being a proposed law,
i.e. being under consideration by a legislative body of &%Government.")

(=>
   (holdsDuring ?TIME1 (modalAttribute ?TEXT Law))
   (exists (?TIME2)
      (and
         (holdsDuring ?TIME2 (attribute ?TEXT LegislativeBill))
         (earlier ?TIME2 ?TIME1))))

(subclass PassingABill PoliticalProcess)
(subclass PassingABill Declaring)
(documentation PassingABill "The &%Process of converting a &%LegislativeBill
into &%Law by a duly authorized legislative body of &%Government.")

(=>
   (instance ?ORG LegislativeOrganization)
   (capability PassingABill agent ?ORG))

(=>
   (and
      (instance ?PASS PassingABill)
      (patient ?PASS ?TEXT))
   (and
      (holdsDuring (BeginFn (WhenFn ?PASS)) (attribute ?TEXT LegislativeBill))
      (holdsDuring (EndFn (WhenFn ?PASS)) (attribute ?TEXT Law))))

(subAttribute InternationalLaw Law)
(documentation InternationalLaw "An &%Attribute that applies to &%Propositions
that express &%Laws concerning the relations between &%Nations.")

(subclass CriminalAction IntentionalProcess)
(documentation CriminalAction "Any &%IntentionalProcess that violates a &%Law.")

(=>
   (instance ?ACTION CriminalAction)
   (exists (?LAW ?CONTENT ?CRIME)
      (and
         (modalAttribute ?LAW Law)
         (containsInformation ?LAW ?CONTENT)
         (realization ?ACTION ?CRIME)
         (not (consistent ?CONTENT ?CRIME)))))

(subclass Statement Sentence)
(documentation Statement "A &%Sentence that is stated to be true; A communication between two agents asserting what the speaker considers as a fact.")

(=>
   (and
      (instance ?STATE Stating)
      (result ?STATE ?SENTENCE)
      (instance ?SENTENCE Sentence))
   (instance ?SENTENCE Statement))

(subclass Fact Statement)
(documentation Fact "The class of &%Statements that are &%True.")

(=>
   (instance ?FACT Fact)
   (true ?FACT True))

(subclass Question Sentence)
(documentation Question "An interrogative &%Sentence, a &%Sentence that
poses a question.")

(=>
   (and
      (instance ?QUESTION Questioning)
      (result ?QUESTION ?SENTENCE)
      (instance ?SENTENCE Sentence))
   (instance ?SENTENCE Question))

(subclass Supposition Sentence)
(documentation Supposition "A &%Sentence that is assumed to be true, possibly
just for the sake of argument.")

(=>
   (and
      (instance ?SUPPOSE Supposing)
      (result ?SUPPOSE ?SENTENCE)
      (instance ?SENTENCE Sentence))
   (instance ?SENTENCE Supposition))

(subclass Request Sentence)
(documentation Request "A &%Sentence that expresses a request for something or
that something be done.")

(=>
   (and
      (instance ?REQUEST Requesting)
      (result ?REQUEST ?SENTENCE)
      (instance ?SENTENCE Sentence))
   (instance ?SENTENCE Request))

(subclass Order Sentence)
(documentation Order "A &%Sentence that expresses an order for something or
that something be done.")

(=>
   (and
      (instance ?ORDER Ordering)
      (result ?ORDER ?SENTENCE)
      (instance ?SENTENCE Sentence))
   (instance ?SENTENCE Order))

(subclass SoundRecording Text)
(documentation SoundRecording "Any &%Text which is a recording of
&%RadiatingSound.")

(=>
   (instance ?RECORD SoundRecording)
   (exists (?INFO ?SOUND)
      (and
         (containsInformation ?RECORD ?INFO)
         (realization ?SOUND ?INFO)
         (instance ?SOUND RadiatingSound))))

(subclass RecordAlbum SoundRecording)
(documentation RecordAlbum "A &%SoundRecording that has the form of a
plastic or glass disk with continuous grooves that are transformed into
sound by a record player.")

(subclass Label Text)
(documentation Label "A very brief &%Text that is attached to an &%Object
and that indicates very specific information about the &%Object, e.g. its
name, its &%monetaryValue, etc.")

(=>
   (instance ?LABEL Label)
   (exists (?OBJ)
      (and
         (instance ?OBJ SelfConnectedObject)
         (connected ?LABEL ?OBJ)
         (refers ?LABEL ?OBJ))))

(subclass Form Text)
(documentation Form "A page or set of pages containing spaces where
information is to be entered by an &%Agent.")

(subclass TaxReturn Form)
(documentation TaxReturn "A &%Form that is used for calculating the amount
of income tax owed in a given year.")

(=>
   (instance ?RETURN TaxReturn)
   (hasPurpose ?RETURN (exists (?CALCULATE ?AMOUNT ?TAX)
                          (and
                             (instance ?CALULATE Calculating)
                             (instrument ?CALCULATE ?RETURN)
                             (result ?CALCULATE ?AMOUNT)
                             (transactionAmount ?TAX ?AMOUNT)
                             (instance ?TAX Tax)))))

(subclass Application Form)
(documentation Application "A &%Form whose purpose is to obtain admission
to an &%Organization or to receive assistance from an &%Organization.")

(=>
   (instance ?APP Application)
   (hasPurpose ?APP (exists (?JOIN ?GIVE)
                       (or
                          (instance ?JOIN JoiningAnOrganization)
                          (instance ?GIVE UnilateralGiving)))))

(subclass MotionPictureShot MotionPicture)
(documentation MotionPictureShot "A unit of action in a &%MotionPicture, a
&%MotionPictureShot is a sequence of images which are captured by a single
camera without interruption.")

(subclass BroadcastProgram Series)
(documentation BroadcastProgram "A &%Series of episodes that are broadcast
on television or radio.")

(=>
   (instance ?PROGRAM BroadcastProgram)
   (exists (?BROADCAST)
      (and
         (instance ?BROADCAST Broadcasting)
         (patient ?BROADCAST ?PROGRAM))))

(subclass NewsProgram BroadcastProgram)
(documentation NewsProgram "A &%BroadcastProgram that is devoted to
reporting the latest events in a city, region, nation or the world at
large.")

(=>
   (and
      (subclass ?PROGRAM NewsProgram)
      (equal ?EPISODE (SeriesVolumeFn ?PROGRAM ?NUMBER)))
   (exists (?DISSEMINATE)
      (and
         (instance ?DISSEMINATE Disseminating)
         (patient ?DISSEMINATE ?EPISODE))))

(subclass Chapter Article)
(documentation Chapter "A numbered and/or titled section of a &%Book, which is
typically indicated in a table of contents for the &%Book.")

(=>
   (instance ?CHAPTER Chapter)
   (exists (?BOOK)
      (and
         (instance ?BOOK Book)
         (subsumesContentInstance ?BOOK ?CHAPTER))))

(subrelation titles names)
(domain titles 1 SymbolicString)
(domainSubclass titles 2 Text)
(documentation titles "A predicate used to indicate the title of a &%Text.
Note that the second argument type restriction is a subclass, rather than
an instance, of &%Text.  Thus, the title Murder_on_the_Orient_Express
corresponds to a large class of &%Books, and not just to a single copy of
the book.")

(subclass Report FactualText)
(subclass Report Article)
(documentation Report "A relatively brief &%FactualText, often it
describes the findings of a study or experiment, or a series of
observations.")

(subclass Newspaper Periodical)
(documentation Newspaper "A &%Periodical that is published on a daily or
weekly basis, that contains &%Reports, and whose issues are printed on
newsprint paper.")

(=>
   (instance ?PAPER Newspaper)
   (exists (?REPORT)
      (and
         (instance ?REPORT Report)
         (subsumesContentInstance ?PAPER ?REPORT))))

(=>
   (and
      (subclass ?PAPER Newspaper)
      (instance ?WEEK Week))
   (exists (?PUBLICATION)
      (and
         (instance ?PUBLICATION Publication)
         (temporalPart (WhenFn ?PUBLICATION) ?WEEK)
         (result ?PUBLICATION ?ISSUE)
         (instance ?ISSUE ?PAPER))))

(subclass Magazine Periodical)
(documentation Magazine "A &%Periodical that is softbound and printed on
glossy paper.")

(subclass Letter FactualText)
(documentation Letter "A brief message which is intended to be mailed
to a person or &%Organization.")

(subclass HistoricalAccount FactualText)
(documentation HistoricalAccount "A &%FactualAccount that describes
significant events that occurred in the past.")

(=>
   (instance ?ACCOUNT HistoricalAccount)
   (exists (?EVENT)
      (and
         (represents ?ACCOUNT ?EVENT)
         (earlier (WhenFn ?EVENT) (WhenFn ?ACCOUNT)))))

(subclass FinancialText Report)
(documentation FinancialText "A &%Report about monetary figures.  This
class covers &%FinancialBills, balance sheets, account statements, etc.")

(subclass FinancialBill FinancialText)
(documentation FinancialBill "A brief statement that the stated amount
of money is owed by the person to whom the bill is delivered.")

(subclass ReferenceBook Book)
(subclass ReferenceBook FactualText)
(documentation ReferenceBook "A &%Book which is not intended to be read
from cover to cover, but which is meant to be consulted to answer specific
factual questions, e.g. about the meaning of a word, the location of a
country, etc.")

(subclass Dictionary ReferenceBook)
(documentation Dictionary "A &%ReferenceBook which specifies the meanings
of the &%Words of a &%Language.")

(subclass NarrativeText Text)
(partition NarrativeText FictionalText HistoricalAccount)
(documentation NarrativeText "Any &%Text that tells a story, whether true
or false.")

(subclass ShortStory FictionalText)
(subclass ShortStory Article)
(documentation ShortStory "A brief work of fiction, often bound with other
short stories in a &%Book or &%Periodical.")

(subclass Novel FictionalText)
(subclass Novel Book)
(documentation Novel "A &%FictionalText that is larger than a &%ShortStory
and that is bound independently (i.e. it is a &%Book).")

(subclass DramaticPlay FictionalText)
(documentation DramaticPlay "A &%FictionalText that is intended to be realized
as &%DramaticActing.")

(=>
   (instance ?PLAY DramaticPlay)
   (hasPurpose ?PLAY (exists (?ACT ?PROP)
                        (and
                           (instance ?ACT DramaticActing)
                           (containsInformation ?PLAY ?PROP)
                           (realization ?ACT ?PROP)))))

(subclass Opera DramaticPlay)
(documentation Opera "A &%DramaticPlay that is set to &%Music.")

(=>
   (and
      (instance ?OPERA Opera)
      (realization ?ACT ?OPERA))
   (instance ?ACT Music))

(instance EnglishLanguage NaturalLanguage)
(documentation EnglishLanguage "A Germanic language that incorporates many roots
from the Romance languages.  It is the official language of the &%UnitedStates,
the &%UnitedKingdomOfGreatBritainAndNorthernIreland, and many other countries.")

(subclass Industry Collection)
(documentation Industry "The class of &%Collections of &%Corporations
which are in the same line of business.")

(=>
   (instance ?INDUSTRY Industry)
   (=>
      (and
         (member ?MEMB1 ?INDUSTRY)
         (member ?MEMB2 ?INDUSTRY))
      (exists (?CLASS)
         (and
            (subclass ?CLASS Corporation)
            (immediateInstance ?MEMB1 ?CLASS)
            (immediateInstance ?MEMB2 ?CLASS)))))

(subclass Steps StationaryArtifact)
(documentation Steps "A &%StationaryArtifact which allows one to climb, step
by step, from one level to another.")

(=>
   (instance ?STEPS Steps)
   (and
      (capability MotionUpward instrument ?STEPS)
      (capability MotionDownward instrument ?STEPS)))

(=>
   (and
      (instance ?LEVEL1 BuildingLevel)
      (instance ?LEVEL2 BuildingLevel)
      (instance ?BUILDING Building)
      (part ?LEVEL1 ?BUILDING)
      (part ?LEVEL2 ?BUILDING))
   (exists (?STEPS)
      (and
         (instance ?STEPS Steps)
         (connects ?STEPS ?LEVEL1 ?LEVEL2))))

(subclass Door Artifact)
(documentation Door "An &%Artifact that restricts and permits access to a
&%StationaryArtifact (e.g. &%Building or &%Room) depending on whether the
&%Door is open or locked.  Note that the class &%Door also covers gates,
because it is not possible to define objective criteria that reliably
distinguish doors from gates.")

(=>
   (instance ?DOOR Door)
   (exists (?ARTIFACT)
      (and
         (part ?DOOR ?WAY)
         (instance ?WAY Doorway))))

(subclass Doorway StationaryArtifact)
(documentation Doorway "A &%StationaryArtifact consisting of a frame that
holds a &%Door.")

(=>
   (instance ?WAY Doorway)
   (exists (?ARTIFACT)
      (and
         (part ?WAY ?ARTIFACT)
         (or
            (instance ?ARTIFACT Building)
            (instance ?ARTIFACT Room)))))

(subclass Window StationaryArtifact)
(documentation Window "A &%StationaryArtifact composed of transparent glass
or plastic that admits light (and possibly air) into a &%Room or &%Building.")

(=>
   (instance ?DOOR Door)
   (exists (?ARTIFACT)
      (and
         (part ?DOOR ?ARTIFACT)
         (or
            (instance ?ARTIFACT Building)
            (instance ?ARTIFACT Room)))))

(subclass Wall StationaryArtifact)
(documentation Wall "A &%StationaryArtifact that supports a &%Building or
partitions it into &%Rooms.")

(=>
   (instance ?WALL Wall)
   (exists (?ARTIFACT)
      (and
         (part ?WALL ?ARTIFACT)
         (or
            (instance ?ARTIFACT Building)
            (instance ?ARTIFACT Room)))))

(subclass Floor StationaryArtifact)
(documentation Floor "A &%StationaryArtifact that is the bottom surface
of a &%Room.")

(=>
   (instance ?FLOOR Floor)
   (exists (?ARTIFACT)
      (and
         (part ?FLOOR ?ARTIFACT)
         (instance ?ARTIFACT Room))))

(subclass Roof StationaryArtifact)
(documentation Roof "The &%top of a &%Building.")

(=>
   (instance ?ROOF Roof)
   (exists (?BUILDING)
      (and
         (instance ?BUILDING Building)
         (top ?ROOF ?BUILDING))))

(subclass BuildingLevel StationaryArtifact)
(documentation BuildingLevel "The story or level of a building, e.g. the &%Basement,
the &%Attic, the ground level, the fourteenth floor, etc.")

(=>
   (instance ?LEVEL BuildingLevel)
   (exists (?BUILDING)
      (and
         (instance ?BUILDING Building)
         (part ?LEVEL ?BUILDING))))

(subclass Basement BuildingLevel)
(documentation Basement "A &%BuildingLevel which satisfies two conditions, viz. it is
lower than all of the other &%BuildingLevels in the same &%Building and it is below
ground level.")

(=>
   (instance ?BASEMENT Basement)
   (not (exists (?LEVEL ?BUILDING)
      (and
         (instance ?LEVEL BuildingLevel)
         (instance ?BUILDING Building)
         (part ?LEVEL ?BUILDING)
         (part ?BASEMENT ?BUILDING)
         (not (equal ?LEVEL ?BASEMENT))
         (orientation ?LEVEL ?BASEMENT Below)))))

(subclass Garage StationaryArtifact)
(documentation Garage "A &%Building or part of a &%Building which is intended to house
one or more &%Automobiles.")

(=>
   (instance ?GARAGE Garage)
   (hasPurpose (exists (?AUTO)
                  (and
                     (instance ?AUTO Automobile)
                     (contains ?GARAGE ?Automobile)))))

(=>
   (instance ?GARAGE Garage)
   (exists (?BUILDING)
      (and
         (instance ?BUILDING Building)
         (part ?GARAGE ?BUILDING))))

(subclass Kitchen Room)
(documentation Kitchen "A &%Room intended for &%Cooking.")

(=>
   (instance ?ROOM Kitchen)
   (hasPurpose ?ROOM (exists (?COOK)
                        (and
                           (instance ?COOK Cooking)
                           (located ?COOK ?ROOM)))))

(subclass Bedroom Room)
(documentation Bedroom "A &%Room intended primarily for sleeping.")

(=>
   (instance ?ROOM Bedroom)
   (exists (?BED)
      (and
         (instance ?BED Bed)
         (located ?BED ?ROOM))))

(subclass Porch StationaryArtifact)
(documentation Porch "A &%StationaryArtifact which is &%connected to a
&%Building and which provides some shelter in entering or leaving the
&%Building or in sitting outside.")

(=>
   (instance ?PORCH Porch)
   (exists (?BUILDING)
      (and
         (instance ?BUILDING Building)
         (connected ?PORCH ?BUILDING))))

(subclass Sidewalk StationaryArtifact)
(documentation Sidewalk "A prepared path for pedestrians alongside a &%Roadway.")

(=>
   (instance ?SIDE Sidewalk)
   (hasPurpose ?SIDE (exists (?WALK)
                        (and
                           (instance ?WALK Walking)
                           (path ?WALK ?SIDE)))))

(=>
   (instance ?SIDE Sidewalk)
   (exists (?ROAD)
      (and
         (instance ?ROAD Roadway)
         (orientation ?SIDE ?ROAD Near))))

(subclass Fence StationaryArtifact)
(documentation Fence "A &%StationaryArtifact that serves to demarcate or
to prevent access to or from the area that the &%Fence surrounds.")

(subclass SportsGround StationaryArtifact)
(documentation SportsGround "A specially designated and maintained area
where &%Sports are played.")

(=>
   (instance ?GROUND SportsGround)
   (hasPurpose ?GROUND (exists (?SPORT)
                          (and
                             (instance ?SPORT Sport)
                             (located ?SPORT ?GROUND)))))

(subclass IndustrialPlant StationaryArtifact)
(documentation IndustrialPlant "A &%Building or part of a &%Building or group
of &%Buildings whose purpose is to &%Manufacture something.")

(=>
   (instance ?PLANT IndustrialPlant)
   (or
      (instance ?PLANT Building)
      (exists (?BUILDING)
         (and
            (instance ?BUILDING Building)
            (located ?BUILDING ?PLANT)))))

(=>
   (instance ?PLANT IndustrialPlant)
   (hasPurpose ?PLANT (exists (?MANUFACTURE)
                         (and
                            (instance ?MANUFACTURE Manufacture)
                            (located ?MANUFACTURE ?PLANT)))))

(subclass Laboratory StationaryArtifact)
(documentation Laboratory "A &%Building, &%Room or suite of &%Rooms where
scientific research, i.e. &%Experimenting, is conducted.")

(=>
   (instance ?LAB Laboratory)
   (exists (?EXPERIMENT)
      (and
         (instance ?EXPERIMENT Experimenting)
         (located ?EXPERIMENT ?LAB))))

(subclass Farm StationaryArtifact)
(documentation Farm "A &%StationaryArtifact consisting of a cultivated
&%LandArea and &%Buildings for maintaining the land and/or the &%Animals
on the land.")

(subclass Barn Building)
(disjoint Barn ResidentialBuilding)
(documentation Barn "A &%Building on a &%Farm that is used for keeping
&%DomesticAnimals, &%Fodder or harvested crops.")

(=>
   (instance ?BARN Barn)
   (exists (?FARM)
      (and
         (instance ?FARM Farm)
         (located ?BARN ?FARM))))

(subclass PerformanceStage StationaryArtifact)
(documentation PerformanceStage "A large platform for theatrical plays,
lectures, dances, music recitals, etc, which can be observed by an audience.")

(=>
   (instance ?STAGE PerformanceStage)
   (hasPurpose ?STAGE (exists (?PERF)
                         (and
                            (instance ?PERF Demonstrating)
                            (located ?PERF ?STAGE)))))

(=>
   (instance ?STAGE PerformanceStage)
   (exists (?AUDITORIUM)
      (and
         (instance ?AUDITORIUM Auditorium)
         (part ?STAGE ?AUDITORIUM))))

(subclass Magnetism RadiatingElectromagnetic)
(documentation Magnetism "Any instance of &%RadiatingElectromagnetic which
involves the attraction of &%Iron.")

(subclass RadioEmission RadiatingElectromagnetic)
(documentation RadioEmission "Any instance of &%RadiatingElectromagnetic
where the waves have a wavelength between 5 milimeters and 30,000 meters.")

(subclass Broadcasting Disseminating)
(subclass Broadcasting RadioEmission)
(documentation Broadcasting "&%Disseminating information by using a
&%CommunicationDevice that radiates &%RadioEmissions.")

(=>
   (instance ?BROADCAST Broadcasting)
   (exists (?DEVICE)
      (and
         (instance ?DEVICE CommunicationDevice)
         (instrument ?BROADCAST ?DEVICE))))

(subclass RadioBroadcasting Broadcasting)
(documentation RadioBroadcasting "Any instance of &%Broadcasting which is
intended to be received by a &%Radio.")

(=>
	(instance ?RADIO Radio)
      (capability RadioBroadcasting patient ?RADIO))

(subclass TelevisionBroadcasting Broadcasting)
(documentation TelevisionBroadcasting "Any instance of &%Broadcasting which
is intended to be received by a &%Television.")

(=>
	(instance ?TELEVISION Television)
      (capability TelevisionBroadcasting patient ?TELEVISION))

(subclass Antenna CommunicationDevice)
(documentation Antenna "A &%CommunicationDevice which enables or improves
the reception of &%RadioEmissions by another &%CommunicationDevice (the
radio or television receiver).")

(=>
   (instance ?ANTENNA Antenna)
   (capability RadioEmission instrument ?ANTENNA))

(subclass Explosion Radiating)
(documentation Explosion "Any sudden and massive release of energy that is
the product of a chemical reaction.")

(=>
   (instance ?EXPLOSION Explosion)
   (exists (?PROC)
      (and
         (instance ?PROC ChemicalProcess)
         (causes ?PROC ?EXPLOSION))))

(=>
   (instance ?EXPLOSION Explosion)
   (capability Damaging instrument ?EXPLOSION))

(subclass Performance Demonstrating)
(documentation Performance "A &%Demonstrating which includes &%DramaticActing
and/or &%Music and which is intended to entertain the audience.")

(=>
   (instance ?PERFORMANCE Performance)
   (exists (?SUB)
      (and
         (subProcess ?SUB ?PERFORMANCE)
         (or
            (instance ?SUB DramaticActing)
            (instance ?SUB Music)))))

(=>
   (instance ?PERFORMANCE Performance)
   (exists (?STAGE)
      (and
         (instance ?STAGE PerformanceStage)
         (located ?PERFORMANCE ?STAGE))))

(subclass Lecture Demonstrating)
(subclass Lecture Speaking)
(documentation Lecture "Any instance of &%Speaking which is done before an
assembled audience.")

(subclass Sermon Lecture)
(documentation Sermon "A &%Lecture that is part of a &%ReligiousService.")

(=>
   (instance ?SERMON Sermon)
   (exists (?SERVICE)
      (and
         (instance ?SERVICE ReligiousService)
         (subProcess ?SERMON ?SERVICE))))

(subclass DramaticActing Pretending)
(documentation DramaticActing "Playing a character in a &%Performance,
&%MotionPicture, etc.")

(subclass DramaticCast GroupOfPeople)
(documentation DramaticCast "The &%GroupOfPeople who engage in &%DramaticActing
as part of the realization of a single &%FictionalText.")

(=>
   (instance ?CAST DramaticCast)
   (exists (?TEXT ?PROP ?PROC)
      (and
         (instance ?TEXT FictionalText)
         (containsInformation ?TEXT ?PROP)
         (realization ?PROC ?PROP)
         (forall (?MEMBER)
            (=>
               (member ?MEMBER ?CAST)
               (exists (?SUB)
                  (and
                     (instance ?SUB DramaticActing)
                     (agent ?SUB ?MEMBER)
                     (subProcess ?SUB ?PROC))))))))

(subclass SocialParty Meeting)
(subclass SocialParty RecreationOrExercise)
(documentation SocialParty "Any &%Meeting where the intent is primarily
to socialize and be entertained.")

(subclass FormalMeeting Meeting)
(disjoint FormalMeeting SocialParty)
(documentation FormalMeeting "Any &%Meeting which is the result of &%Planning
and whose purpose is not socializing.")

(=>
   (instance ?MEETING FormalMeeting)
   (exists (?PLANNING)
      (and
         (instance ?PLANNING Planning)
         (result ?PLANNING ?MEETING)
         (earlier (WhenFn ?PLANNING) (WhenFn ?MEETING)))))

(subclass Resolution Deciding)
(documentation Resolution "Any instance of &%Deciding which is conducted at a
&%FormalMeeting and where the &%agent is an &%Organization.")

(=>
   (instance ?RESOLUTION Resolution)
   (exists (?AGENT ?MEETING)
      (and
         (instance ?AGENT Organization)
         (agent ?RESOLUTION ?AGENT)
         (subProcess ?RESOLUTION ?MEETING)
         (instance ?MEETING FormalMeeting))))

(subclass Smoking RecreationOrExercise)
(documentation Smoking "Inhaling and exhaling &%Smoke produced by a
&%CigarOrCigarette.")

(=>
   (instance ?SMOKING Smoking)
   (exists (?BURN ?CIGAR ?BREATHE)
      (and
         (subProcess ?BURN ?SMOKING)
         (instance ?BURN Combustion)
         (resource ?BURN ?CIGAR)
         (instance ?CIGAR CigarOrCigarette)
         (result ?BURN ?SMOKE)
         (patient ?BREATHE ?SMOKE)
         (instance ?BREATHE Breathing)
         (subProcess ?BREATHE ?SMOKING))))

(instance Jury GroupOfPeople)
(documentation Jury "A &%GroupOfPeople who are given the duty of rendering a
verdict with respect to a &%LegalAction.")

(=>
   (instance ?JURY Jury)
   (holdsRight (exists (?DECISION)
                  (and
                     (instance ?DECISION LegalDecision)
                     (agent ?DECISION ?JURY))) ?JURY))

(subclass LegalCharge LegalAction)
(documentation LegalCharge "Any &%LegalAction of which a &%Government is
the &%agent.")

(=>
   (instance ?CHARGE LegalCharge)
   (exists (?GOV)
      (and
         (instance ?GOV Government)
         (agent ?CHARGE ?GOV))))

(subclass Testifying Stating)
(documentation Testifying "Giving testimony as part of a &%JudicialProcess.")

(=>
   (instance ?TESTIFY Testifying)
   (exists (?PROC)
      (and
         (instance ?PROC JudicialProcess)
         (subProcess ?TESTIFY ?PROC))))

(subclass CourtRoom Room)
(documentation CourtRoom "Any &%Room whose purpose is to realize
&%JudicialProcesses.")

(=>
   (instance ?PROCESS JudicialProcess)
   (exists (?ROOM)
      (and
         (instance ?ROOM CourtRoom)
         (located ?PROCESS ?ROOM))))

(subclass LegalOpinion Argument)
(documentation LegalOpinion "An &%Argument that explains the reasoning behind a
&%LegalDecision.")

(=>
   (instance ?OPINION LegalOpinion)
   (exists (?DECISION ?TEXT ?PROPOSITION)
      (and
         (instance ?DECISION LegalDecision)
         (result ?DECISION ?TEXT)
         (containsInformation ?TEXT ?PROPOSITION)
         (conclusion ?PROPOSITION ?OPINION))))

(subclass LegalAward LegalDecision)
(documentation LegalAward "Any &%LegalDecision which gives to the plaintiff of
the corresponding &%LegalAction some amount of monetary compensation.")

(=>
   (and
      (instance ?AWARD LegalAward)
      (refers ?AWARD ?ACTION)
      (instance ?ACTION LegalAction)
      (agent ?ACTION ?PLAINTIFF))
   (confersRight (exists (?GET ?OBJ ?VALUE)
                    (and
                       (instance ?GET Getting)
                       (experiencer ?GET ?PLAINTIFF)
                       (patient ?GET ?OBJ)
                       (monetaryValue ?OBJ ?VALUE))) ?AWARD ?PLAINTIFF))

(subclass LegalConviction LegalDecision)
(documentation LegalConviction "Any &%LegalDecision where the defendant is found
guilty of the crime for which the corresponding trial was held.")

(subclass LegalAquittal LegalDecision)
(documentation LegalAquittal "Any &%LegalDecision where the defendant is found
not to be guilty of the crime for which the corresponding trial was held.")

(subclass GameCall Deciding)
(subclass GameCall Declaring)
(documentation GameCall "A decision issued by an official referee in a
&%Game.  Note that &%GameCall is a subclass of &%Declaring, because these
decisions have binding, normative force.")

(=>
   (instance ?CALL GameCall)
   (exists (?GAME)
      (and
         (instance ?GAME Game)
         (refers ?CALL ?GAME))))

(subclass BeginningOperations OrganizationalProcess)
(disjoint BeginningOperations CeasingOperations)
(documentation BeginningOperations "The process of an &%Organization
commencing operations.  In the case of a &%Corporation, this would be
the process of going into business.")

(=>
   (and
      (instance ?OP BeginningOperations)
      (instance ?ORG Organization)
      (agent ?OP ?ORG))
   (starts ?OP (WhenFn ?ORG)))

(subclass CeasingOperations OrganizationalProcess)
(documentation CeasingOperations "The process of an &%Organization
ceasing operations, i.e. its folding or going out of business in
some other fashion.")

(=>
   (and
      (instance ?OP CeasingOperations)
      (instance ?ORG Organization)
      (agent ?OP ?ORG))
   (finishes ?OP (WhenFn ?ORG)))

(subclass FallingAsleep PsychologicalProcess)
(disjoint FallingAsleep WakingUp)
(documentation FallingAsleep "The process of transitioning from a state of
being &%Awake to a state of being &%Asleep.")

(=>
   (and
      (instance ?FALL FallingAsleep)
      (experiencer ?FALL ?AGENT))
   (exists (?START ?FINISH)
      (and
         (starts ?START (WhenFn ?FALL))
         (finishes ?FINISH (WhenFn ?FALL))
         (holdsDuring ?START (attribute ?AGENT Awake))
         (holdsDuring ?FINISH (attribute ?AGENT Asleep)))))

(subclass WakingUp PsychologicalProcess)
(documentation WakingUp "The process of transitioning from a state of being
&%Asleep to a state of being &%Awake.")

(=>
   (and
      (instance ?WAKE WakingUp)
      (experiencer ?WAKE ?AGENT))
   (exists (?START ?FINISH)
      (and
         (starts ?START (WhenFn ?WAKE))
         (finishes ?FINISH (WhenFn ?WAKE))
         (holdsDuring ?START (attribute ?AGENT Asleep))
         (holdsDuring ?FINISH (attribute ?AGENT Awake)))))

(subclass AcademicDegree Certificate)
(documentation AcademicDegree "A &%Certificate that demonstrates that the holder
of the &%Certificate has successfully completed an &%EducationalProgram.")

(=>
   (and
      (instance ?DEGREE AcademicDegree)
      (possesses ?AGENT ?DEGREE))
   (exists (?PROGRAM ?STUDY)
      (and
         (instance ?PROGRAM EducationalProgram)
         (realization ?STUDY ?PROGRAM)
         (experiencer ?STUDY ?AGENT))))

(subclass EducationalProgram Plan)
(documentation EducationalProgram "A series of &%EducationalCourses that must
be completed to receive an &%AcademicDegree or other &%Certificate.  Note that
an &%EducationalProgram, unlike an &%EducationalCourse, may be realized at more
than one &%EducationalOrganization.")

(=>
   (instance ?PROGRAM EducationalProgram)
   (exists (?COURSE1 ?COURSE2)
      (and
         (instance ?COURSE1 EducationalCourse)
         (instance ?COURSE2 EducationalCourse)
         (not (equal ?COURSE1 ?COURSE2))
         (subPlan ?COURSE1 ?PROGRAM)
         (subPlan ?COURSE2 ?PROGRAM))))

(subclass EducationalCourse EducationalProgram)
(documentation EducationalCourse "A schedule of class meetings offered by an
&%EducationalOrganization.")

(=>
   (instance ?COURSE EducationalCourse)
   (exists (?CLASS)
      (and
         (realization ?CLASS ?COURSE)
         (instance ?CLASS EducationalProcess)
         (located ?CLASS ?ORG)
         (instance ?ORG EducationalOrganization))))

(subclass School EducationalOrganization)
(documentation School "An &%EducationalOrganization with a curriculum,
teachers, and students.  Most &%Schools are housed in a &%Building
dedicated to the &%EducationalOrganization.")

(subclass HighSchool School)
(documentation HighSchool "A &%School which admits &%students who have
graduated from a middle school and which normally covers the ninth through
twelfth grades.  A &%HighSchool confers a high school diploma.")

(=>
   (and
      (instance ?ENTER Matriculation)
      (agent ?ENTER ?COLLEGE)
      (patient ?ENTER ?STUDENT)
      (instance ?COLLEGE College))
   (exists (?GRAD ?SCHOOL)
      (and
         (instance ?GRAD Graduation)
         (agent ?GRAD ?SCHOOL)
         (patient ?GRAD ?STUDENT)
         (instance ?SCHOOL HighSchool)
         (earlier (WhenFn ?GRAD) (WhenFn ?ENTER)))))

(subclass PostSecondarySchool School)
(partition PostSecondarySchool JuniorCollege College University)
(documentation PostSecondarySchool "The class of &%Schools that offer
an associate's degree or a bachelor's degree.")

(subclass JuniorCollege PostSecondarySchool)
(documentation JuniorCollege "The class of &%PostSecondarySchools that
offer an associate's degree and do not offer a bachelor's degree.")

(subclass College PostSecondarySchool)
(documentation College "A &%School which admits &%students who have
graduated from high school and which confers a bachelor's degree,
normally requiring four years of study.  Note that a &%College does
not confer any graduate degrees.  For institutions that confer both
bachelor's and graduate degrees, the concept &%University should be
used.")

(subclass University PostSecondarySchool)
(documentation University "A &%School which admits &%students that
have graduated from high school (known as undergraduate students) and
&%students who have received a bachelor's degree (known as graduate
students).  A &%University confers both bachelor's and graduate
degrees.")

(subrelation student member)
(domain student 1 EducationalOrganization)
(domain student 2 CognitiveAgent)
(documentation student "(&%student ?AGENT ?ORG) means that ?AGENT is enrolled
in the &%EducationalOrganization ?ORG.")

(=>
   (student ?ORG ?AGENT)
   (exists (?PROCESS)
      (and
         (instance ?PROCESS EducationalProcess)
	   (located ?PROCESS ?ORG)
         (destination ?PROCESS ?AGENT))))

(subrelation teacher member)
(domain teacher 1 EducationalOrganization)
(domain teacher 2 CognitiveAgent)
(documentation teacher "(&%teacher ?AGENT ?ORG) means that ?AGENT is a
teacher at the &%EducationalOrganization ?ORG.")

(=>
   (teacher ?ORG ?AGENT)
   (exists (?PROCESS)
      (and
         (instance ?PROCESS EducationalProcess)
	   (located ?PROCESS ?ORG)
         (agent ?PROCESS ?AGENT))))

(subclass InsurancePolicy Certificate)
(documentation InsurancePolicy "A &%Certificate that states the terms of an
insurance contract.")

(subclass Telephoning Speaking)
(documentation Telephoning "Any instance of &%Speaking where the
&%instrument of &%Communicating is a &%Telephone.")

(=>
   (instance ?TEL Telephoning)
   (exists (?DEVICE)
      (and
         (instance ?DEVICE Telephone)
         (instrument ?TEL ?DEVICE))))

(subclass EconomicRelation BinaryRelation)
(documentation EconomicRelation "A class of &%Relations which are used
to specify various economic measures, e.g. the GDP, the consumer price
index, and the trade deficit.")

(=>
   (and
      (instance ?REL EconomicRelation)
      (domain ?REL 1 ?CLASS))
   (subclass ?CLASS GeopoliticalArea))

(subclass LaborStriking OrganizationalProcess)
(documentation LaborStriking "A &%Process in which some or all of the
employees of an &%Organization refuse to work until their pay is
increased or their working conditions are improved in some respect.")

(=>
   (and
      (instance ?STRIKE LaborStriking)
      (agent ?STRIKE ?PERSON)
      (instance ?PERSON Human)
      (patient ?STRIKE ?ORG)
      (instance ?ORG Organization))
   (employs ?ORG ?PERSON))

(subclass Retiring TerminatingEmployment)
(documentation Retiring "Voluntarily leaving employment at the end of one's
career in order to take time off in the later years of one's life.")

(instance monetaryWage QuaternaryPredicate)
(domain monetaryWage 1 Organization)
(domain monetaryWage 2 Human)
(domain monetaryWage 3 TimeDuration)
(domain monetaryWage 4 CurrencyMeasure)
(documentation monetaryWage "(&%monetaryWage ?ORG ?PERSON ?TIME ?MONEY) means
that the &%Organization employs ?PERSON and pays him/her the amount of money
?MONEY per &%TimeDuration ?TIME.")

(=>
   (monetaryWage ?ORG ?PERSON ?TIME ?MONEY)
   (employs ?ORG ?PERSON))

(subclass GameArtifact Artifact)
(disjointDecomposition GameArtifact GameBoard GamePiece)
(documentation GameArtifact "An &%Artifact that is designed to be used as an
&%instrument in a &%Game.")

(=>
   (instance ?ARTIFACT GameArtifact)
   (exists (?GAME)
      (and
         (subclass ?GAME Game)
         (capability ?GAME instrument ?ARTIFACT))))

(subclass GameBoard GameArtifact)
(documentation GameBoard "A &%GameArtifact which is intended to be used as the
game area for playing a particular game.")

(=>
   (instance ?BOARD GameBoard)
   (hasPurpose ?BOARD (exists (?GAME)
                         (and
                            (instance ?GAME Game)
                            (located ?GAME ?BOARD)))))

(subclass GamePiece GameArtifact)
(documentation GamePiece "A &%GameArtifact that is moved around in a game
area.")

(subclass Ball GamePiece)
(documentation Ball "Any &%GamePiece which has the shape of a sphere.")

(subclass GameShot Impelling)
(subclass GameShot Maneuver)
(documentation GameShot "Impelling a &%GamePiece for the purpose of
scoring a point or preventing the opposing player or team from scoring
a point.  Note that this class does not cover shots which are disallowed
by the rules of the game.")

(=>
   (instance ?SHOT GameShot)
   (exists (?PIECE)
      (and
         (instance ?PIECE GamePiece)
         (patient ?SHOT ?PIECE))))

(=>
   (instance ?SHOT GameShot)
   (exists (?GAME)
      (and
         (instance ?GAME Game)
         (subProcess ?SHOT ?GAME))))

(=>
   (instance ?SHOT GameShot)
   (hasPurpose ?SHOT (instance ?SHOT Score)))

(subclass Score GameShot)
(documentation Score "A successful attempt to score a point in a &%Game.")

(subclass GameGoal GameArtifact)
(documentation GameGoal "The location where a &%GameShot must end up if it
is to constitute a &%Score.")

(=>
   (and
      (instance ?GOAL GameGoal)
      (instrument ?GAME ?GOAL)
      (instance ?GAME Game)
      (subProcess ?SCORE ?GAME)
      (instance ?SCORE Score))
   (exists (?PIECE ?TIME)
      (and
         (instance ?PIECE GamePiece)
         (patient ?SCORE ?PIECE)
         (temporalPart ?TIME (WhenFn ?SCORE))
         (holdsDuring ?TIME (located ?PIECE ?GOAL)))))

(subclass BaseballManeuver Maneuver)
(documentation BaseballManeuver "Any &%Maneuver in &%Baseball.")

(=>
   (instance ?MOVE BaseballManeuver)
   (exists (?BASEBALL)
      (and
         (instance ?BASEBALL Baseball)
         (subProcess ?MOVE ?BASEBALL))))

(subclass BaseballWalk BaseballManeuver)
(documentation BaseballWalk "Taking first base after four balls have been
called by the umpire.")

(subclass BaseballHit GameShot)
(subclass BaseballHit BaseballManeuver)
(documentation BaseballHit "Any base hit in &%Baseball.")

(subclass BaseballRun BaseballHit)
(subclass BaseballRun Score)
(documentation BaseballRun "A &%Score in &%Baseball.  It consists of hitting
the ball with the bat and then touching all four bases of the diamond before
being tagged with the ball by a member of the opposite team.")

(subclass HomeRun BaseballRun)
(documentation HomeRun "A &%BaseballRun where the batter touches home base
during his turn at bat.")

(subclass Throwing Impelling)
(subclass Throwing BodyMotion)
(documentation Throwing "Any instance of &%Impelling where the &%instrument is
an &%Arm.")

(=>
   (instance ?THROW Throwing)
   (exists (?ARM)
      (and
         (instance ?ARM Arm)
         (instrument ?THROW ?ARM))))

(subclass Pitching Throwing)
(subclass Pitching GameShot)
(documentation Pitching "&%Throwing a &%Ball to the batter in a game of
&%Baseball or softball.")

(=>
   (and
      (instance ?PITCH Pitching)
      (patient ?PITCH ?BALL))
   (instance ?BALL Ball))

(subclass Catching Touching)
(subclass Catching Maneuver)
(documentation Catching "Any &%Maneuver in a &%Game which results in a
situation where the &%agent &%grasps the &%Ball.")

(=>
   (and
      (instance ?CATCH Catching)
      (patient ?CATCH ?BALL))
   (instance ?CATCH Ball))

(=>
   (and
      (instance ?CATCH Catching)
      (agent ?CATCH ?AGENT)
      (patient ?CATCH ?BALL))
   (holdsDuring (EndFn (WhenFn ?CATCH)) (grasps ?AGENT ?BALL)))

(subclass Stretching Motion)
(documentation Stretching "Moving two sides of an object in opposite
directions so that the object becomes both longer and thinner.")

(subclass Accelerating Translocation)
(subclass Accelerating Increasing)
(documentation Accelerating "Increasing the speed with which someone
or something is moving.")

(=>
   (and
      (instance ?ACCELERATE Accelerating)
      (agent ?ACCELERATE ?AGENT))
   (exists (?LENGTH1 ?LENGTH2 ?TIME1 ?TIME2)
      (and
         (holdsDuring (BeginFn (WhenFn ?ACCELERATE)) (measure ?AGENT (SpeedFn ?LENGTH1 ?TIME1)))
         (holdsDuring (EndFn (WhenFn ?ACCELERATE)) (measure ?AGENT (SpeedFn ?LENGTH2 ?TIME2)))
         (or
            (greaterThan ?LENGTH2 ?LENGTH1)
            (greaterThan ?TIME2 ?TIME1)))))

(subclass Flying Translocation)
(documentation Flying "Any instance of &%Translocation which is through an
&%AtmosphericRegion and which is powered by the wings of an &%Animal.")

(=>
   (instance ?FLY Flying)
   (exists (?REGION)
      (and
         (instance ?REGION AtmosphericRegion)
         (located ?FLY ?REGION))))

(subclass TakingOff Translocation)
(documentation TakingOff "Any instance of &%Translocation which starts on something other
than an &%AtmosphericRegion and which has an instance of &%Flying as a &%subProcess.")

(=>
   (instance ?OFF TakingOff)
   (exists (?REGION ?FLYING)
      (and
         (not (instance ?REGION AtmosphericRegion))
         (holdsDuring (BeginFn (WhenFn ?OFF)) (located ?OFF ?REGION))
         (subProcess ?FLYING ?OFF)
         (instance ?FLYING Flying))))

(subclass Landing Translocation)
(documentation Landing "Any instance of &%Translocation which ends up on something other
than an &%AtmosphericRegion and which has an instance of &%Flying as a &%subProcess.")

(=>
   (instance ?LAND Landing)
   (exists (?REGION ?FLYING)
      (and
         (not (instance ?REGION AtmosphericRegion))
         (holdsDuring (EndFn (WhenFn ?LAND)) (located ?LAND ?REGION))
         (subProcess ?FLYING ?OFF)
         (instance ?FLYING Flying))))

(subclass Returning Translocation)
(documentation Returning "Any instance of &%Translocation where the &%agent
goes to a location where he/she had been before the &%Translocation took place.")

(=>
   (and
      (instance ?RETURN Returning)
      (experiencer ?RETURN ?AGENT)
      (destination ?RETURN ?DEST))
   (exists (?TIME)
      (and
         (earlier ?TIME (WhenFn ?RETURN))
         (holdsDuring ?TIME (located ?AGENT ?DEST)))))

(subclass Escaping Translocation)
(documentation Escaping "Any instance of &%Translocation where the &%agent brings
it about that he/she is no longer confined without having the right to do
so.")

(=>
   (and
      (instance ?ESCAPE Escaping)
      (agent ?ESCAPE ?AGENT))
   (exists (?CONFINE)
      (and
         (instance ?CONFINE Confining)
         (patient ?CONFINE ?AGENT)
         (meetsTemporally (WhenFn ?CONFINE) (WhenFn ?ESCAPE)))))

(=>
   (instance ?ESCAPE Escaping)
   (not (holdsRight (agent ?ESCAPE ?AGENT) ?AGENT)))

(subclass Leaving Motion)
(documentation Leaving "The initial part of any instance of &%Translocation.")

(=>
   (instance ?LEAVE Leaving)
   (exists (?GO)
      (and
         (instance ?GO Translocation)
         (subProcess ?LEAVE ?GO)
         (starts (WhenFn ?LEAVE) (WhenFn ?GO)))))

(subclass Arriving Motion)
(documentation Arriving "The final part of any instance of &%Translocation.")

(=>
   (instance ?ARRIVE Arriving)
   (exists (?GO)
      (and
         (instance ?GO Translocation)
         (subProcess ?ARRIVE ?GO)
         (finishes (WhenFn ?ARRIVE) (WhenFn ?GO)))))

(subclass Rotating Motion)
(documentation Rotating "&%Motion that begins and ends at the same point,
because the trajectory of the &%Motion is circular.")

(=>
   (and
      (instance ?ROTATE Rotating)
      (experiencer ?ROTATE ?AGENT))
   (exists (?LOC)
      (and
         (holdsDuring (BeginFn (WhenFn ?ROTATE)) (located ?AGENT ?LOC))
         (holdsDuring (EndFn (WhenFn ?ROTATE)) (located ?AGENT ?LOC)))))

(subclass MotionUpward Motion)
(disjoint MotionUpward MotionDownward)
(documentation MotionUpward "&%Motion where an &%Object is moving away
from the ground.")

(subclass MotionDownward Motion)
(documentation MotionDownward "&%Motion where an &%Object is moving toward the
ground.")

(subclass Reversing Motion)
(documentation Reversing "Moving something in such a way that its &%top
becomes its &%bottom and vice versa.")

(=>
   (and
      (instance ?REVERSE Reversing)
      (patient ?REVERSE ?OBJ)
      (holdsDuring (BeginFn (WhenFn ?REVERSE)) (and (top ?TOP ?OBJ) (bottom ?BOTTOM ?OBJ))))
   (holdsDuring (EndFn (WhenFn ?REVERSE)) (and (top ?BOTTOM ?OBJ) (bottom ?TOP ?OBJ))))

(subclass LiquidMotion Motion)
(documentation LiquidMotion "Any &%Motion where the &%patient is a
&%Liquid.  This class would cover, in particular, the flow of
&%Water.")

(=>
   (and
      (instance ?MOTION LiquidMotion)
      (patient ?MOTION ?OBJ))
   (attribute ?OBJ Liquid))

(subclass Dripping LiquidMotion)
(documentation Dripping "Any &%LiquidMotion where the &%Liquid is moved
drop by drop.")

(subclass Pouring LiquidMotion)
(subclass Pouring Transfer)
(documentation Pouring "Any instance of &%Transfer from one &%Container to
another, where the thing transferred is a &%Liquid.")

(=>
   (instance ?POUR Pouring)
   (exists (?LIQUID ?CONTAINER1 ?CONTAINER2)
      (and
         (origin ?POUR ?CONTAINER1)
         (destination ?POUR ?CONTAINER2)
         (instance ?CONTAINER1 Container)
         (instance ?CONTAINER2 Container))))

(subclass WaterMotion LiquidMotion)
(documentation WaterMotion "Any &%LiquidMotion where the &%Liquid is &%Water.")

(=>
   (instance ?MOTION WaterMotion)
   (exists (?WATER)
      (and
         (patient ?MOTION ?WATER)
         (instance ?WATER Water))))

(subclass WaterWave LiquidMotion)
(documentation WaterWave "A &%WaterWave is a raised ridge of water
moving along the surface of a body of water.  The &%WaterWave moves
in a direction approximately transverse to the crest line of the wave.
The &%patient of the &%WaterWave is successive regions of water, which
do not travel in the direction of the wave or with it.")

(=>
   (instance ?WAVE WaterWave)
   (exists (?AREA)
      (and
         (instance ?AREA WaterArea)
         (located ?WAVE ?AREA))))

(subclass GasMotion Motion)
(documentation GasMotion "Any &%Motion where the &%patient is a
&%Gas.  This class would cover, in particular, the motion of
&%Air, e.g. a breeze or wind.")

(=>
   (and
      (instance ?MOTION GasMotion)
      (patient ?MOTION ?OBJ))
   (attribute ?OBJ Gas))

(subclass Wind AirStream)
;; (subclass Wind GasMotion)
;; commented out due to disjoint conflict - PJC
(documentation Wind "&%Wind is the class of variable &%AirStreams
that occur in &%EarthsAtmosphere.")

(=>
	(instance ?BLOW Wind)
	(located ?BLOW EarthsAtmosphere))

(=>
   (instance ?WIND Wind)
   (exists (?AIR)
      (and
         (patient ?WIND ?AIR)
         (instance ?AIR Air))))

(subclass Pulling Transportation)
(documentation Pulling "Any instance of &%Transportation, where a
&%TransportationDevice is dragged by something else, whether the something
else is an &%Animal or a self-powered &%TransportationDevice.")

(=>
   (and
      (instance ?PULL Pulling)
      (patient ?PULL ?DEVICE))
   (instance ?DEVICE TransportationDevice))

(subclass AirTransportation Transportation)
(documentation AirTransportation "Any instance of &%Transportation where the &%instrument is an &%Aircraft and which is through an &%AtmosphericRegion.")

(=>
   (instance ?TRANSPORT AirTransportation)
   (exists (?CRAFT ?REGION)
      (and
         (instance ?CRAFT Aircraft)
         (instance ?REGION AtmosphericRegion)
         (instrument ?TRANSPORT ?CRAFT)
         (located ?TRANSPORT ?REGION))))

(=>
   (instance ?CRAFT Aircraft)
   (capability AirTransportation instrument ?CRAFT))

(subclass Helicopter Aircraft)
(documentation Helicopter "Any &%Aircraft with rapidly rotating wings.")

(subclass LandTransportation Transportation)
(documentation LandTransportation "Any instance of &%Transportation where the
&%instrument is a &%LandVehicle.")

(=>
   (instance ?LAND LandTransportation)
   (exists (?CRAFT ?AREA)
      (and
         (instance ?CRAFT LandCraft)
         (instance ?AREA LandArea)
         (instrument ?LAND ?CRAFT)
         (located ?CRAFT ?AREA))))

(=>
   (instance ?VEHICLE LandVehicle)
   (capability LandTransportation instrument ?VEHICLE))

(subclass StageCoach Wagon)
(documentation StageCoach "A &%Wagon that is pulled by &%Horses and whose purpose
was to transport &%Humans and their luggage from one &%City to the next, especially
in areas which did not have an established transportation system, e.g. the old west.")

(=>
   (and
      (instance ?STAGE StageCoach)
      (instance ?PULL Pulling)
      (patient ?PULL ?STAGE)
      (agent ?PULL ?HORSE))
   (instance ?HORSE Horse))

(subclass WaterTransportation Transportation)
(documentation WaterTransportation "Any instance of &%Transportation where
the &%instrument is a &%Watercraft.")

(=>
   (instance ?TRANSPORT WaterTransportation)
   (exists (?CRAFT ?AREA)
      (and
         (instance ?CRAFT Watercraft)
         (instance ?AREA WaterArea)
         (instrument ?TRANSPORT ?CRAFT)
         (located ?TRANSPORT ?AREA))))

(=>
   (instance ?CRAFT Watercraft)
   (capability WaterTransportation instrument ?CRAFT))

(subclass SpaceTransportation Transportation)
(documentation SpaceTransportation "Any instance of &%Transportation where the
&%instrument is a &%Spacecraft and which is through a &%SpaceRegion.")

(=>
   (instance ?TRANSPORT SpaceTransportation)
   (exists (?CRAFT ?REGION)
      (and
         (instance ?CRAFT Spacecraft)
         (instance ?REGION SpaceRegion)
         (instrument ?TRANSPORT ?CRAFT)
         (located ?TRANSPORT ?REGION))))

(subclass Spacecraft Vehicle)
(documentation Spacecraft "Any &%Vehicle which is capable of
&%SpaceTransportation.")

(=>
   (instance ?CRAFT Spacecraft)
   (capability SpaceTransportation instrument ?CRAFT))

(subclass Missile Spacecraft)
(documentation Missile "A &%Spacecraft which is propelled by a rocket.")

(subclass Installing Putting)
(documentation Installing "&%Putting a &%Device in a location and configuring
the &%Device so that it can be used as intended after the installation.")

(=>
   (and
      (instance ?INSTALL Installing)
      (patient ?INSTALL ?DEVICE))
   (instance ?DEVICE Device))

(=>
   (and
      (instance ?INSTALL Installing)
      (patient ?INSTALL ?DEVICE)
      (hasPurpose ?DEVICE ?PURPOSE))
   (holdsDuring (EndFn (WhenFn ?INSTALL)) (true ?PURPOSE True)))

(subclass MovingResidence Transfer)
(documentation MovingResidence "The process of changing one's residence, i.e. moving one's belongs to a new &%home.")

(=>
   (and
      (instance ?MOVE MovingResidence)
      (agent ?MOVE ?AGENT))
   (exists (?HOME1 ?HOME2)
      (and
         (holdsDuring (BeginFn (WhenFn ?MOVE)) (home ?AGENT ?HOME1))
         (holdsDuring (EndFn (WhenFn ?MOVE)) (home ?AGENT ?HOME2))
         (not (equal ?HOME1 ?HOME2)))))

(subclass Tilling IntentionalProcess)
(subclass Tilling SurfaceChange)
(documentation Tilling "Any &%Process of altering &%Soil in such a way as to
facilitate &%Agriculture.")

(=>
   (and
      (instance ?TILL Tilling)
      (patient ?TILL ?SOIL))
   (instance ?SOIL Soil))

(=>
   (instance ?TILL Tilling)
   (exists (?CULTURE)
      (and
         (instance ?CULTURE Agriculture)
         (subProcess ?TILL ?CULTURE))))

(subclass Drilling IntentionalProcess)
(subclass Drilling SurfaceChange)
(documentation Drilling "Any &%Process of producing a &%hole in a
&%SelfConnectedObject which involves rotating a long, thin bit.")

(=>
   (and
      (instance ?DRILL Drilling)
      (patient ?DRILL ?OBJ))
   (exists (?HOLE)
      (and
         (holdsDuring (BeginFn (WhenFn ?DRILL)) (not (hole ?HOLE ?OBJ)))
         (holdsDuring (EndFn (WhenFn ?DRILL)) (hole ?HOLE ?OBJ)))))

(subclass Imagining PsychologicalProcess)
(documentation Imagining "Forming a mental picture of something which
is not present.")

(subclass Dreaming Imagining)
(disjoint Dreaming IntentionalProcess)
(documentation Dreaming "A &%Process of producing metal images which occurs
while one is &%Asleep.")

(=>
   (and
      (instance ?DREAM Dreaming)
      (experiencer ?DREAM ?AGENT))
   (holdsDuring (WhenFn ?DREAM) (attribute ?AGENT Asleep)))

(subclass Frightening PsychologicalProcess)
(documentation Frightening "Any &%PsychologicalProcess where the &%patient
comes to feel &%Anxiety.")

(=>
   (and
      (instance ?FRIGHTEN Frightening)
      (experiencer ?FRIGHTEN ?AGENT))
   (and
      (holdsDuring (BeginFn (WhenFn ?FRIGHTEN)) (not (attribute ?AGENT Anxiety)))
      (holdsDuring (EndFn (WhenFn ?FRIGHTEN)) (attribute ?AGENT Anxiety))))

(subclass Murder Killing)
(subclass Murder CriminalAction)
(documentation Murder "Impermissible &%Killing of a &%Human.")

(=>
   (instance ?MURDER Murder)
   (exists (?PERSON)
      (and
         (patient ?MURDER ?PERSON)
         (instance ?PERSON Human))))

(subclass Hanging Killing)
(documentation Hanging "&%Killing someone by suspending him/her from a
rope wound around the neck until asphyxiation occurs.")

(subclass OrchestralConducting Guiding)
(documentation OrchestralConducting "The &%Process of directing an orchestra.")

(=>
   (instance ?CONDUCT OrchestralConducting)
   (exists (?MUSIC)
      (and
         (instance ?MUSIC Music)
         (result ?CONDUCT ?MUSIC))))

(=>
   (and
      (instance ?CONDUCT OrchestralConducting)
      (patient ?CONDUCT ?ORCHESTRA))
   (instance ?ORCHESTRA Orchestra))

(subclass Orchestra GroupOfPeople)
(documentation Orchestra "A &%GroupOfPeople that create &%InstrumentalMusic
together.")

(=>
   (instance ?ORCHESTRA Orchestra)
   (hasPurpose ?ORCHESTRA (exists (?MUSIC)
                             (and
                                (instance ?MUSIC InstrumentalMusic)
                                (agent ?MUSIC ?ORCHESTRA)))))

(subclass DramaticDirecting Guiding)
(documentation DramaticDirecting "The process of directing a &%DramaticActing
in a &%MotionPicture or the &%Performance of a &%DramaticPlay.")

(=>
   (and
      (instance ?DIRECT DramaticDirecting)
      (patient ?DIRECT ?ACT))
   (instance ?ACT DramaticActing))

(subclass Experimenting Investigating)
(documentation Experimenting "&%Investigating the truth of a &%Proposition
by constructing and observing a trial.  Note that the trial may be either
controlled or uncontrolled, blind or not blind.")

(subclass Sharing ChangeOfPossession)
(documentation Sharing "The subclass of &%ChangeOfPossession where a
&%properPart of the &%patient is given by the &%agent or the &%destination.")

(=>
   (and
      (instance ?SHARE Sharing)
      (agent ?SHARE ?AGENT1)
      (destination ?SHARE ?AGENT2)
      (patient ?SHARE ?OBJ))
   (exists (?GIVE ?PART)
      (and
         (instance ?GIVE Giving)
         (subProcess ?GIVE ?SHARE)
         (patient ?GIVE ?PART)
         (properPart ?PART ?OBJ)
         (agent ?GIVE ?AGENT1)
         (destination ?GIVE ?AGENT2))))

(subclass Stealing UnilateralGetting)
(subclass Stealing CriminalAction)
(documentation Stealing "Any &%UnilateralGetting which is not permitted by the
&%origin of the &%UnilateralGetting.  These cases of &%UnilateralGetting are
distinguished from ones where the &%destination is the subject of charity or
other forms of benefaction.")

(=>
   (and
      (instance ?STEAL Stealing)
      (destination ?STEAL ?AGENT)
      (origin ?STEAL ?VICTIM)
      (instance ?VICTIM CognitiveAgent))
   (not (confersRight (destination ?STEAL ?AGENT) ?VICTIM ?AGENT)))

(subclass Inheriting UnilateralGetting)
(documentation Inheriting "Any &%UnilateralGetting where the &%agent
receives some part of the property of a person upon the death of the
person.")

(=>
   (and
      (instance ?INHERIT Inheriting)
      (agent ?INHERIT ?HEIR)
      (origin ?INHERIT ?PERSON)
      (patient ?INHERIT ?PROPERTY))
   (exists (?DEATH)
      (and
         (instance ?DEATH Death)
         (experiencer ?DEATH ?PERSON)
         (earlier (WhenFn ?DEATH) (WhenFn ?INHERIT))
         (holdsDuring (ImmediatePastFn (WhenFn ?DEATH)) (possesses ?PERSON ?PROPERTY))
         (confersRight (possesses ?HEIR ?PROPERTY) ?PERSON ?HEIR))))

(subclass FullTimePosition Position)
(documentation FullTimePosition "Any &%Position where the employee is either
salaried or paid for at least 40 hour of work per week.")

(subclass PartTimePosition Position)
(documentation PartTimePosition "Any &%Position where the employee is not
salaried and is paid for less than 40 hours of work per week.")

(subclass ClericalSecretary Position)
(documentation ClericalSecretary "The class of &%Positions where the position
holder is responsible for clerical duties, e.g. typing documents, answering
phones, keeping schedules, etc.")

(subclass Soldier Position)
(documentation Soldier "The class of &%Positions which involve serving in
the armed forces of a &%Nation.")

(subclass RedcoatSoldier Soldier)
(documentation RedcoatSoldier "Any &%Soldier that served on the British side during
the American revolutionary war.")

(subclass ConfederateSoldier Soldier)
(documentation ConfederateSoldier "Any &%Soldier that served on the confederate side
during the American Civil War.")

(subclass MilitaryPrivate Soldier)
(documentation MilitaryPrivate "An enlisted &%Soldier of the lowest rank.")

(subclass MilitaryOfficer Soldier)
(documentation MilitaryOfficer "The class of &%Soldiers who have authority or command.")

(=>
   (instance ?OFFICER MilitaryOfficer)
   (exists (?MANAGE ?PATIENT)
      (and
         (instance ?MANAGE Managing)
         (agent ?MANAGE ?OFFICER)
         (patient ?MANAGE ?PATIENT)
         (or
            (instance ?PATIENT MilitaryOrganization)
            (instance ?PATIENT Soldier)))))

(subclass Lieutenant MilitaryOfficer)
(documentation Lieutenant "A commissioned &%MilitaryOfficer.")

(subclass CaptainOfficer MilitaryOfficer)
(documentation CaptainOfficer "A commissioned &%MilitaryOfficer who ranks
above a &%Lieutenant but below a &%MajorOfficer.")

(subclass Colonel MilitaryOfficer)
(documentation Colonel "A commissioned &%MilitaryOfficer who ranks above
a lieutenant colonel and below a brigadier general.")

(subclass Century TimeDuration)
(documentation Century "The &%TimeDuration of 100 years.")

(=>
   (instance ?CENTURY Century)
   (duration ?CENTURY (MeasureFn 100 YearDuration)))

(subclass Decade TimeDuration)
(documentation Decade "The &%TimeDuration of 10 years.")

(=>
   (instance ?DECADE Decade)
   (duration ?DECADE (MeasureFn 10 YearDuration)))

(subclass YardLength LengthMeasure)
(instance YardLength UnitOfMeasure)
(documentation YardLength "English unit of length, equal to 3 &%FeetLength.")

(=>
   (instance ?NUMBER RealNumber)
   (equal
        (MeasureFn ?NUMBER YardLength)
        (MeasureFn (MultiplicationFn 3 ?NUMBER) FootLength)))

(subclass NightTime RecurrentTimeInterval)
(documentation NightTime "The class of &%TimeIntervals that begin at &%Sunset
and end at &%Sunrise.")

(=>
   (instance ?NIGHT NightTime)
   (exists (?DAY1 ?DAY2)
      (and
         (instance ?DAY1 DayTime)
         (instance ?DAY2 DayTime)
         (meetsTemporally ?NIGHT ?DAY1)
         (meetsTemporally ?DAY2 ?NIGHT))))

(=>
   (instance ?NIGHT NightTime)
   (exists (?RISE ?SET)
      (and
         (instance ?RISE Sunrise)
         (instance ?SET Sunset)
         (starts ?SET ?NIGHT)
         (finishes ?RISE ?NIGHT))))

(subclass DayTime RecurrentTimeInterval)
(documentation DayTime "The class of &%TimeIntervals that begin at &%Sunrise
and end at &%Sunset.")

(=>
   (instance ?DAY DayTime)
   (exists (?NIGHT1 ?NIGHT2)
      (and
         (instance ?NIGHT1 NightTime)
         (instance ?NIGHT2 NightTime)
         (meetsTemporally ?DAY ?NIGHT1)
         (meetsTemporally ?NIGHT2 ?DAY))))

(=>
   (instance ?DAY DayTime)
   (exists (?RISE ?SET)
      (and
         (instance ?RISE Sunrise)
         (instance ?SET Sunset)
         (starts ?RISE ?DAY)
         (finishes ?SET ?DAY))))

(subclass Morning DayTime)
(documentation Morning "The class of &%TimeIntervals that begin at &%Sunrise
and end at noon.")

(=>
   (instance ?MORNING Morning)
   (exists (?HOUR)
      (and
         (instance ?HOUR (HourFn 12 ?DAY))
         (finishes ?HOUR ?MORNING))))

(subclass Afternoon DayTime)
(documentation Afternoon "The class of &%TimeIntervals that begin at noon and
end at &%Sunset.")

(=>
   (instance ?AFTERNOON Afternoon)
   (exists (?HOUR)
      (and
         (instance ?HOUR (HourFn 12 ?DAY))
         (starts ?HOUR ?AFTERNOON))))

(subclass Sunrise TimeInterval)
(documentation Sunrise "The &%TimeInterval of each &%Day when the sun is rising
and is partially overlapped by the horizon line.")

(subclass Sunset TimeInterval)
(documentation Sunset "The &%TimeInterval of each &%Day when the sun is setting
and is partially overlapped by the horizon line.")

(subclass Weekend TimeInterval)
(documentation Weekend "Any &%Saturday and &%Sunday which are contiguous.")

(=>
   (instance ?WEEKEND Weekend)
   (exists (?SATURDAY ?SUNDAY)
      (and
         (instance ?SATURDAY Saturday)
         (instance ?SUNDAY Sunday)
         (starts ?SATURDAY ?WEEKEND)
         (finishes ?SUNDAY ?WEEKEND)
         (meetsTemporally ?SATURDAY ?SUNDAY))))

(subclass SeasonOfYear TimeInterval)
(partition SeasonOfYear WinterSeason SpringSeason SummerSeason FallSeason)
(documentation SeasonOfYear "&%SeasonOfYear is the class of four seasons correlated with the calendar &%Year and associated with changes in the length of daylight and with overall temperature changes. Depending upon the &%GeographicArea, a &%SeasonOfYear may also be associated with weather patterns (e.g., rainy, dry, windy). The characteristics of seasons (cold vs. hot temperatures, long vs. short days) are reversed from the &%NorthernHemisphere to the &%SouthernHemisphere.")

(subclass WinterSeason SeasonOfYear)
(documentation WinterSeason "The &%SeasonOfYear that begins at the winter
solstice and ends at the spring equinox.")

(=>
   (instance ?WINTER WinterSeason)
   (exists (?SPRING)
      (and
         (instance ?SPRING SpringSeason)
         (meetsTemporally ?WINTER ?SPRING))))

(=>
   (instance ?WINTER WinterSeason)
   (exists (?AUTUMN)
      (and
         (instance ?AUTUMN FallSeason)
         (meetsTemporally ?AUTUMN ?WINTER))))

(subclass SpringSeason SeasonOfYear)
(documentation SpringSeason "The &%SeasonOfYear that begins at the spring
equinox and ends at the summer solstice.")

(=>
   (instance ?SPRING SpringSeason)
   (exists (?SUMMER)
      (and
         (instance ?SUMMER SummerSeason)
         (meetsTemporally ?SPRING ?SUMMER))))

(=>
   (instance ?Spring SpringSeason)
   (exists (?WINTER)
      (and
         (instance ?WINTER WinterSeason)
         (meetsTemporally ?WINTER ?SPRING))))

(subclass SummerSeason SeasonOfYear)
(documentation SummerSeason "The &%SeasonOfYear that begins at the summer
solstice and ends at the autumnal equinox.")

(=>
   (instance ?SUMMER SummerSeason)
   (exists (?SPRING)
      (and
         (instance ?SPRING SpringSeason)
         (meetsTemporally ?SPRING ?SUMMER))))

(=>
   (instance ?SUMMER SummerSeason)
   (exists (?AUTUMN)
      (and
         (instance ?AUTUMN FallSeason)
         (meetsTemporally ?SUMMER ?AUTUMN))))

(subclass FallSeason SeasonOfYear)
(documentation FallSeason "The &%SeasonOfYear that begins at the autumnal
equinox and ends at the winter solstice.")

(=>
   (instance ?AUTUMN FallSeason)
   (exists (?SUMMER)
      (and
         (instance ?SUMMER SummerSeason)
         (meetsTemporally ?SUMMER ?AUTUMN))))

(=>
   (instance ?AUTUMN FallSeason)
   (exists (?WINTER)
      (and
         (instance ?WINTER WinterSeason)
         (meetsTemporally ?AUTUMN ?WINTER))))

(instance Upstairs PositionalAttribute)
(documentation Upstairs "A &%PositionalAttribute to indicate that one thing is
one or more floors above a second thing in the same building.")

(=>
   (orientation ?OBJ1 ?OBJ2 Upstairs)
   (exists (?LEVEL1 ?LEVEL2 ?BUILDING)
      (and
         (instance ?LEVEL1 BuildingLevel)
         (instance ?LEVEL2 BuildingLevel)
         (instance ?BUILDING Building)
         (part ?LEVEL1 ?BUILDING)
         (part ?LEVEL2 ?BUILDING)
         (located ?OBJ1 ?LEVEL1)
         (located ?OBJ2 ?LEVEL2)
         (orientation ?LEVEL1 ?LEVEL2 Above))))

(instance Downstairs PositionalAttribute)
(documentation Downstairs "A &%PositionalAttribute to indicate that one thing is
one or more floors below a second thing in the same building.")

(=>
   (orientation ?OBJ1 ?OBJ2 Downstairs)
   (exists (?LEVEL1 ?LEVEL2 ?BUILDING)
      (and
         (instance ?LEVEL1 BuildingLevel)
         (instance ?LEVEL2 BuildingLevel)
         (instance ?BUILDING Building)
         (part ?LEVEL1 ?BUILDING)
         (part ?LEVEL2 ?BUILDING)
         (located ?OBJ1 ?LEVEL1)
         (located ?OBJ2 ?LEVEL2)
         (orientation ?LEVEL1 ?LEVEL2 Below))))

(instance Happiness EmotionalState)
(documentation Happiness "The state of being happy, experiencing pleasure,
joy or contentment.  Note that this &%Attribute covers both active enjoyment,
as well as the emotional state of simply being free from anxiety or fear.")

(=>
   (attribute ?PERSON Happiness)
   (not (exists (?PROP)
      (fears ?PERSON ?PROP))))

(instance Tranquility EmotionalState)
(subAttribute Tranquility Happiness)
(contraryAttribute Tranquility Anxiety)
(documentation Tranquility "The state of being free from &%Anxiety.")

(instance Unhappiness EmotionalState)
(contraryAttribute Unhappiness Happiness)
(documentation Unhappiness "The state of being unhappy, experiencing pain,
sorrow or unease.")

(instance Anxiety EmotionalState)
(subAttribute Anxiety Unhappiness)
(documentation Anxiety "The state of being worried, troubled or uneasy.")

(=>
   (attribute ?PERSON Anxiety)
   (exists (?PROP)
      (fears ?PERSON ?PROP)))

(subAttribute Anger Unhappiness)
(documentation Anger "The state of being wrathful, irate or indignant.")

(=>
   (attribute ?PERSON Anger)
   (exists (?PROP)
      (disapproves ?PERSON ?PROP)))

(subAttribute Pain Unhappiness)
(documentation Pain "A physical sensation of discomfort which can vary widely in intensity.")

(instance Surprise EmotionalState)
(documentation Surprise "The &%EmotionalState that one experiences when something unexpected and of significance occurs.")

(=>
   (holdsDuring ?TIME (attribute ?AGENT Surprise))
   (exists (?PART ?PROP)
      (and
         (temporalPart ?PART (PastFn ?TIME))
         (holdsDuring ?PART (expects ?AGENT ?PROP))
         (holdsDuring ?TIME (true ?PROP False)))))


(subclass SecondaryColor ColorAttribute)
(documentation SecondaryColor "A color that is the product of mixing together two or more &%PrimaryColors.")

(=>
   (and
      (instance ?COLOR SecondaryColor)
      (attribute ?OBJ ?COLOR))
   (exists (?PROCESS ?RESOURCE1 ?RESOURCE2)
      (and
         (result ?PROCESS ?OBJ)
         (resource ?PROCESS ?RESOURCE1)
         (resource ?PROCESS ?RESOURCE2)
         (attribute ?RESOURCE1 ?PRIMARY1)
         (attribute ?RESOURCE2 ?PRIMARY2)
         (instance ?PRIMARY1 PrimaryColor)
         (instance ?PRIMARY2 PrimaryColor)
         (not (equal ?PRIMARY1 ?PRIMARY2)))))

(instance Gray SecondaryColor)
(documentation Gray "A &%SecondaryColor that results from mixing &%Black and
&%White.")

(instance Pink SecondaryColor)
(documentation Pink "A &%SecondaryColor that results from mixing &%Red and
&%White.")

(instance Brown SecondaryColor)
(documentation Brown "A &%SecondaryColor that resembles the color of wood or
of soil.")

(instance Green SecondaryColor)
(documentation Green "A &%SecondaryColor that resembles the color of fresh
grass.")

(instance LineFormation ShapeAttribute)
(documentation LineFormation "A &%ShapeAttribute that applies to
&%Collections and indicates that all of the &%members of the &%Collection
are arrayed in a line, i.e. each &%member (except possibly the first) is
behind or to the side of exactly one other &%member.")

(=>
   (attribute ?COLLECTION LineFormation)
   (instance ?COLLECTION Collection))

(instance Stressed SoundAttribute)
(documentation Stressed "A &%SoundAttribute of &%Syllables.  It denotes
the quality of being emphasized over the other &%Syllables in the same
&%Word.")

(=>
   (attribute ?SYLLABLE Stressed)
   (instance ?SYLLABLE Syllable))

(=>
   (and
      (attribute ?SYLLABLE Stressed)
      (instance ?WORD Word)
      (part ?SYLLABLE ?WORD))
   (not (exists (?SYLLABLE2)
      (and
         (instance ?SYLLABLE2 Syllable)
         (part ?SYLLABLE2 ?WORD)
         (not (equal ?SYLLABLE2 ?SYLLABLE))))))

(subclass Syllable SymbolicString)
(documentation Syllable "A sequence of &%Characters from the same &%Word
that denote a single sound.")

(=>
   (instance ?WORD Word)
   (exists (?SYLLABLE)
      (and
         (instance ?SYLLABLE Syllable)
         (part ?SYLLABLE ?WORD))))

(subclass DigitCharacter Character)
(documentation DigitCharacter "Any &%Character that is comprised of a single digit,
i.e. one of the numerals 0, 1, 2, 3, 4, 5, 6, 7, 8, 9.")

(instance Lost ContestAttribute)
(contraryAttribute Lost Won)
(documentation Lost "The &%ContestAttribute that applies to a &%Contest
participant who has lost the &%Contest.")

(instance Won ContestAttribute)
(documentation Won "The &%ContestAttribute that applies to a &%Contest
participant who has won the &%Contest.")

(subclass GameAttribute ContestAttribute)
(documentation GameAttribute "Any &%ContestAttribute that is specific
to a &%Game.")

(=>
   (and
      (property ?THING ?ATTR)
      (instance ?ATTR GameAttribute))
   (exists (?GAME)
      (and
         (instance ?GAME Game)
         (or
            (agent ?GAME ?THING)
            (patient ?GAME ?THING)
            (subProcess ?THING ?GAME)))))

(subclass SportAttribute GameAttribute)
(documentation SportAttribute "Any &%GameAttribute that is specific to
a &%Sport.")

(=>
   (and
      (property ?THING ?ATTR)
      (instance ?ATTR SportAttribute))
   (exists (?SPORT)
      (and
         (instance ?SPORT Game)
         (or
            (agent ?SPORT ?THING)
            (patient ?SPORT ?THING)
            (subProcess ?THING ?SPORT)))))

(instance BaseballStrike SportAttribute)
(documentation BaseballStrike "A baseball pitch that is in the strike zone
and that is not hit by the batter.")

(subclass PoliticoEconomicAttribute RelationalAttribute)
(partition PoliticoEconomicAttribute FormOfGovernment EconomicAttribute)
(documentation PoliticoEconomicAttribute "Any &%Attribute of a &%Government
which specifies some aspect of the political or economic system of the
&%Government.")

(=>
   (and
      (instance ?ATTRIBUTE PoliticoEconomicAttribute)
      (attribute ?GOVERNMENT ?ATTRIBUTE))
   (instance ?GOVERNMENT Government))

(subclass Oxidation ChemicalProcess)
(documentation Oxidation "Any &%ChemicalProcess where &%Electrons are removed
from the substance undergoing the &%ChemicalProcess.")

(=>
   (and
      (instance ?OXIDATE Oxidation)
      (patient ?OXIDATE ?SUBSTANCE)
      (holdsDuring (BeginFn (WhenFn ?OXIDATE)) (electronNumber ?SUBSTANCE ?GREATER)))
   (exists (?LOWER)
      (and
         (lessThan ?LOWER ?GREATER)
         (holdsDuring (EndFn (WhenFn ?OXIDATE)) (electronNumber ?SUBSTANCE ?LOWER)))))

(subclass Dialysis Separating)
(documentation Dialysis "Any process of &%Separating a &%Solution into two or more
constituent &%PureSubstances by means of their unequal diffusion through membranes
that are partially permeable.")

(=>
   (instance ?DIALYSIS Dialysis)
   (exists (?SOLUTION ?SUBSTANCE1 ?SUBSTANCE2)
      (and
         (resource ?DIALYSIS ?SOLUTION)
         (instance ?SOLUTION Solution)
         (result ?DIALYSIS ?SUBSTANCE1)
         (instance ?SUBSTANCE1 PureSubstance)
         (result ?DIALYSIS ?SUBSTANCE2)
         (instance ?SUBSTANCE2 PureSubstance)
         (not (equal ?SUBSTANCE1 ?SUBSTANCE2)))))

(instance ChemicalEquilibrium InternalAttribute)
(documentation ChemicalEquilibrium "The &%Attribute of being in a chemically
stable state, i.e. the relative proportions of &%resources and &%results will
not longer change.")

(=>
   (property ?PROCESS ChemicalEquilibrium)
   (instance ?PROCESS ChemicalProcess))

(=>
   (property ?PROCESS ChemicalEquilibrium)
   (exists (?RATIO)
      (=>
         (and
            (holdsDuring ?TIME (resource ?PROCESS ?RESOURCE))
            (holdsDuring ?TIME (result ?PROCESS ?RESULT)))
         (equal ?RATIO (DivisionFn ?RESOURCE ?RESULT)))))

(subclass GeometricFigure ShapeAttribute)
(partition GeometricFigure GeometricPoint OneDimensionalFigure TwoDimensionalFigure ThreeDimensionalFigure)
(documentation GeometricFigure "The class of all geometric figures, i.e. the
class of all abstract, spatial representations.  The instances of this class
are &%GeometricPoints, &%TwoDimensionalFigures or &%ThreeDimensionalFigures.")

(subclass GeometricPoint GeometricFigure)
(documentation GeometricPoint "The class of zero-dimensional
&%GeometricFigures, i.e. the class of &%GeometricFigures that have position
but lack extension in any dimension.")

(subclass OneDimensionalFigure GeometricFigure)
(documentation OneDimensionalFigure "The class of &%GeometricFigures that
have position and an extension along a single dimension, viz. straight lines.")

(subclass TwoDimensionalFigure GeometricFigure)
(partition TwoDimensionalFigure OpenTwoDimensionalFigure ClosedTwoDimensionalFigure)
(documentation TwoDimensionalFigure "The class of &%GeometricFigures that
have position and an extension along two dimensions, viz. plane figures
like circles and polygons.")

(subclass OpenTwoDimensionalFigure TwoDimensionalFigure)
(documentation OpenTwoDimensionalFigure "The class of &%TwoDimensionalFigures that
are not &%ClosedTwoDimensionalFigures.")

(subrelation contestParticipant agent)
(instance contestParticipant TotalValuedRelation)
(domain contestParticipant 1 Contest)
(domain contestParticipant 2 Agent)
(documentation contestParticipant "(&%contestParticipant ?CONTEST ?AGENT)
means that ?AGENT is one of the sides in the &%Contest ?CONTEST.  For
example, if the ?CONTEST is a football game, then ?AGENT would be one of
the opposing teams.  For another example, if ?CONTEST is a &%Battle, then
?AGENT would be one of the sides fighting each other.")

(subclass CivilWar War)
(documentation CivilWar "A &%War in which the fighting &%GeopoliticalAreas
are both part of the same &%Nation.")

(=>
   (instance ?WAR CivilWar)
   (exists (?NATION)
      (and
         (instance ?NATION Nation)
         (forall (?AGENT)
            (=>
               (contestParticipant ?WAR ?AGENT)
               (geopoliticalSubdivision ?AGENT ?NATION))))))

(subclass Debating Contest)
(subclass Debating LinguisticCommunication)
(documentation Debating "A &%Contest where each participant holds a different
view regarding some issue, and each participant attempts to prove, by
rhetoric or evidence, that his/her own views about a particular matter are
correct and/or that the views of the other participants are incorrect.")

(subclass Negotiating Contest)
(subclass Negotiating LinguisticCommunication)
(documentation Negotiating "A &%Contest where each participant attempts to
maximize his self-interest in a &%Promise that marks the end of the &%Contest.")

(=>
   (instance ?NEGOTIATE Negotiating)
   (hasPurpose ?NEGOTIATE (exists (?COMMIT)
                             (and
                                (instance ?COMMIT Committing)
                                (subProcess ?COMMIT ?NEGOTIATE)
                                (finishes (WhenFn ?COMMIT) (WhenFn ?NEGOTIATE))))))

(subclass BusinessCompetition Contest)
(documentation BusinessCompetition "Any &%Contest where the &%contestParticipants
are &%Corporations and the aim is to win as many customers as possible.")

(=>
   (and
      (instance ?CONTEST BusinessCompetition)
      (contestParticipant ?CONTEST ?AGENT))
   (instance ?AGENT Corporation))

(subclass Boxing ViolentContest)
(subclass Boxing Sport)
(documentation Boxing "A sport which involves two participants who try to
knock each other out in a limited number of rounds.  Note that each instance
of &%Boxing is a boxing match.")

(subclass Gymnastics Sport)
(documentation Gymnastics "A sport which involves exercises of agility on a
range of gymnastic equipment.")

(subclass Baseball Sport)
(documentation Baseball "A sport which involves two teams of 9 players each
that take turns at bat and attempt to score runs.  Note that each instance
of &%Baseball is a baseball game.")

(subclass BaseballTeam SportsTeam)
(documentation BaseballTeam "The class of &%SportsTeams that play &%Baseball.")

(=>
   (instance ?TEAM BaseballTeam)
   (capability Baseball agent ?TEAM))

(subclass BaseballInning Maneuver)
(documentation BaseballInning "A division of a &%Baseball game.  A normal game
consists of 9 innings, and each inning involves a turn a bat for both teams.")

(=>
   (instance ?INNING BaseballInning)
   (exists (?GAME)
      (and
         (instance ?GAME Baseball)
         (subProcess ?INNING ?GAME))))

(instance Blind BiologicalAttribute)
(documentation Blind "The &%Attribute that applies to &%Animals and &%Humans
that are unable to see.")

(=>
   (attribute ?AGENT Blind)
   (not (capability Seeing agent ?AGENT)))

(instance Fist BodyPosition)
(documentation Fist "The &%BodyPosition of having the fingers drawn into
the palm so that the hand can be used for striking something.")

(=>
   (attribute ?HAND Fist)
   (instance ?HAND Hand))

(subclass LyingDown BodyMotion)
(documentation LyingDown "The &%BodyMotion of moving from a &%Sitting
to a &%Prostrate position.")

(=>
   (and
      (instance ?LIE LyingDown)
      (agent ?LIE ?AGENT))
   (and
      (holdsDuring (BeginFn (WhenFn ?LIE)) (attribute ?AGENT Sitting))
      (holdsDuring (EndFn (WhenFn ?LIE)) (attribute ?AGENT Prostrate))))

(subclass SittingDown BodyMotion)
(documentation SittingDown "The &%BodyMotion of moving from a &%Standing
to a &%Sitting position.")

(=>
   (and
      (instance ?SIT SittingDown)
      (agent ?SIT ?AGENT))
   (and
      (holdsDuring (BeginFn (WhenFn ?SIT)) (attribute ?AGENT Standing))
      (holdsDuring (EndFn (WhenFn ?SIT)) (attribute ?AGENT Sitting))))

(subclass StandingUp BodyMotion)
(documentation StandingUp "The &%BodyMotion of moving from a &%Sitting
to a &%Standing position.")

(=>
   (and
      (instance ?STAND StandingUp)
      (agent ?STAND ?AGENT))
   (and
      (holdsDuring (BeginFn (WhenFn ?STAND)) (attribute ?AGENT Sitting))
      (holdsDuring (EndFn (WhenFn ?STAND)) (attribute ?AGENT Standing))))

(subclass OpeningEyes BodyMotion)
(disjoint OpeningEyes ClosingEyes)
(documentation OpeningEyes "The &%BodyMotion of relaxing the eye lids so that
the corneas are exposed to light.")

(subclass ClosingEyes BodyMotion)
(documentation ClosingEyes "The &%BodyMotion of tensing the eye lids so that
the corneas are not exposed to light.")

(subclass Winking ClosingEyes)
(subclass Winking Gesture)
(documentation Winking "Any instance of &%ClosingEyes which is intended to
express something to someone else.")

(subclass Shrugging BodyMotion)
(subclass Shrugging Gesture)
(documentation Shrugging "Moving the &%Shoulders in such a way that the motion
is intended to express something to someone else.")

(=>
   (and
      (instance ?SHRUG Shrugging)
      (patient ?SHRUG ?SHOULDER))
   (instance ?SHOULDER Shoulder))

(subclass Trembling BodyMotion)
(subclass Trembling (ComplementFn IntentionalProcess))
(documentation Trembling "Any &%BodyMotion which is involuntary and which is
repeated many times over a short time frame, e.g. a tremor in the hands.")

(subclass DomesticAnimal Animal)
(disjoint DomesticAnimal Human)
(documentation DomesticAnimal "Any &%Animal that is kept by a &%Human, as
a pet, as livestock, for exhibition, etc.")

(=>
   (instance ?ANIMAL DomesticAnimal)
   (exists (?KEEP ?PERSON)
      (and
         (instance ?KEEP Keeping)
         (agent ?KEEP ?PERSON)
         (instance ?PERSON Human)
         (patient ?KEEP ?ANIMAL))))

(=>
   (instance ?FEED Fodder)
   (hasPurpose ?FEED (exists (?ANIMAL ?EAT)
                        (and
                           (instance ?ANIMAL DomesticAnimal)
                           (instance ?EAT Eating)
                           (agent ?EAT ?ANIMAL)
                           (patient ?EAT ?FEED)))))

(subclass Horse HoofedMammal)
(subclass Horse DomesticAnimal)
(documentation Horse "A domesticated &%HoofedMammal that is used for
transportation and work.")

(subclass Donkey HoofedMammal)
(subclass Donkey DomesticAnimal)
(documentation Donkey "A domesticated &%HoofedMammal that is used for
work.")

(subclass Mule HoofedMammal)
(subclass Mule DomesticAnimal)
(documentation Mule "The product of a &%Male &%Donkey and a &%Female
&%Horse.  &%Mules are always sterile.")

(=>
   (instance ?MULE Mule)
   (exists (?DONKEY ?HORSE)
      (and
         (father ?MULE ?DONKEY)
         (instance ?DONKEY Donkey)
         (mother ?MULE ?HORSE)
         (instance ?HORSE Horse))))

(=>
   (instance ?MULE Mule)
   (not (capability SexualReproduction agent ?MULE)))

(subclass Sheep HoofedMammal)
(subclass Sheep DomesticAnimal)
(documentation Sheep "A domesticated &%HoofedMammal that is bred for its
wool and for its meat (known as mutton).")

(subclass Cow HoofedMammal)
(subclass Cow DomesticAnimal)
(documentation Cow "A domesticated &%HoofedMammal that is raised for milk
and beef, and is also used for work.")

(subclass FemaleCow Cow)
(documentation FemaleCow "A &%Cow that is &%Female.")

(<=>
   (instance ?COW FemaleCow)
   (and
      (instance ?COW Cow)
      (attribute ?COW Female)))

(subclass Chicken Bird)
(subclass Chicken DomesticAnimal)
(documentation Chicken "A subclass of &%Bird that is raised for its meat
and for its eggs.")

(subclass Hen Chicken)
(documentation Hen "A &%Female &%Chicken.")

(<=>
   (instance ?HEN Hen)
   (and
      (instance ?HEN Chicken)
      (attribute ?HEN Female)))

(subclass ChickenMeat Meat)
(documentation ChickenMeat "&%Meat that was originally part of a &%Chicken.")

(<=>
   (instance ?MEAT ChickenMeat)
   (exists (?CHICKEN)
      (and
         (instance ?MEAT Meat)
         (instance ?CHICKEN Chicken)
         (part ?MEAT ?CHICKEN))))

(subclass Mouse Rodent)
(documentation Mouse "A &%Rodent that has a hairless tail like a rat but that
is smaller than a rat.")

(subclass Hay Fodder)
(documentation Hay "&%Grass that has been cut and cured for use as &%Fodder.")

(=>
   (instance ?HAY Hay)
   (exists (?MAKE)
      (and
         (instance ?MAKE Making)
         (resource ?MAKE ?GRASS)
         (instance ?GRASS Grass)
         (result ?MAKE ?HAY))))

(subclass Copying Making)
(documentation Copying "&%Making a &%copy of something.")

(=>
   (and
      (instance ?COPY Copying)
      (resource ?COPY ?THING1)
      (result ?COPY ?THING2))
   (copy ?THING1 ?THING2))

(subclass Vacationing RecreationOrExercise)
(documentation Vacationing "Taking time off from &%Working.")

(=>
   (and
      (instance ?VACATION Vacationing)
      (instance ?WORK Working)
      (agent ?VACATION ?PERSON)
      (agent ?WORK ?PERSON)
      (instance ?PERSON Human))
   (not (overlapsTemporally (WhenFn ?VACATION) (WhenFn ?WORK))))

(subclass Working FinancialTransaction)
(documentation Working "Any &%FinancialTransaction where someone exchanges
his/her labor for an instance of &%CurrencyMeasure.")

(subclass Farming Working)
(documentation Farming "Operating a farm, e.g. planting and harvesting crops,
tending livestock, etc.")

(=>
   (and
      (instance ?FARMING Farming)
      (agent ?FARMING ?FARMER))
   (exists (?FARM)
      (and
         (instance ?FARM Farm)
         (holdsDuring (WhenFn ?FARMING) (located ?FARMER ?FARM)))))

(subclass Serving Working)
(documentation Serving "Working as a waiter or servant, either for an
&%Organization (e.g. a restaurant) or for a person or family.")

(subclass Sales Working)
(documentation Sales "Any instance of &%Working that involves &%Selling or
trying to sell items.")

(subAttribute Retired Unemployed)
(documentation Retired "Voluntary unemployment toward the end of one's life.")

(subclass SkilledOccupation Position)
(documentation SkilledOccupation "Any &%Position which requires
learning a set of skills.")

(=>
   (and
      (instance ?OCCUPATION SkilledOccupation)
      (attribute ?PERSON ?OCCUPATION))
   (exists (?TRAINING)
      (and
         (instance ?TRAINING EducationalProcess)
         (destination ?TRAINING ?PERSON))))

(subclass ManualLabor Position)
(documentation ManualLabor "Any &%Position which involves manual
work.")

(subclass UnskilledOccupation ManualLabor)
(disjoint UnskilledOccupation SkilledOccupation)
(documentation UnskilledOccupation "Any &%Position which does not
require learning a set of skills.")

(instance Maid UnskilledOccupation)
(documentation Maid "A &%Position which involves &%Serving a person
or family.  Note that this &%Position is filled only by a &%Woman.")

(=>
   (attribute ?MAID Maid)
   (instance ?MAID Woman))

(=>
   (attribute ?MAID Maid)
   (exists (?SERVE)
      (and
         (instance ?SERVE Serving)
         (agent ?SERVE ?MAID))))

(instance TheaterProfession SkilledOccupation)
(documentation TheaterProfession "Often know as the stage, the
&%Position of performing live plays.")

(subclass Coach SkilledOccupation)
(documentation Coach "Any occupation that involves training an athlete or a
sports team.")

(subclass OccupationalTrade SkilledOccupation)
(subclass OccupationalTrade ManualLabor)
(documentation OccupationalTrade "Any &%Position that involves skilled
manual work.")

(subclass Profession SkilledOccupation)
(disjoint Profession OccupationalTrade)
(documentation Profession "SUMO: Any occupation that requires at least a bachelor's degree.  NOTE: this wasn't always true, and still may not be true in borderline cases.  Better would be to require a degree *or* some form of licensing - e.g. one can be a teacher by passing a teacher certification test. - PJC.")

(=>
   (and
      (or
         (instance ?ROLE SkilledTrade)
         (instance ?ROLE Profession))
      (attribute ?HUMAN ?ROLE))
   (exists (?FIELD)
      (and
         (instance ?FIELD FieldOfStudy)
         (knows ?HUMAN ?FIELD))))

(subclass Cleric Profession)
(documentation Cleric "The &%Profession of being in charge of or ministering
to a &%ReligousOrganization.")

(<=>
   (occupiesPosition ?PERSON Cleric ?ORG)
   (and
      (leader ?ORG ?PERSON)
      (instance ?ORG ReligiousOrganization)))

(subclass PoliceOfficer Profession)
(documentation PoliceOfficer "The &%Profession of being a police officer, i.e.
working for a law enforcement agency that is part of a &%Government.")

(subclass PoliceDetective PoliceOfficer)
(documentation PoliceDetective "The &%Profession of being a police
detective, i.e. being a &%PoliceOfficer whose duties include the
investigation of crimes.")

(subclass PrivateDetective Profession)
(disjoint PrivateDetective PoliceDetective)
(documentation PrivateDetective "The &%Profession of being a private detective,
i.e. a detective who can be hired for a fee to investigate something.")

(subclass Professor Profession)
(documentation Professor "The &%Profession of being a &%teacher at a
&%PostSecondarySchool.")

(=>
   (and
      (instance ?PROF Professor)
      (occupiesPosition ?PERSON ?PROF ?ORG))
   (and
      (teacher ?ORG ?PROF)
      (instance ?ORG PostSecondarySchool)))

(subclass MedicalDoctor Profession)
(documentation MedicalDoctor "The &%Profession of being a medical doctor,
i.e. having attended medical school and being licensed to practice medicine.")

(=>
   (attribute ?DOCTOR MedicalDoctor)
   (exists (?PROCESS1 ?PROCESS2)
      (and
         (subclass ?PROCESS1 DiagnosticProcess)
         (subclass ?PROCESS2 TherapeuticProcess)
         (capability ?PROCESS1 agent ?DOCTOR)
         (capability ?PROCESS2 agent ?DOCTOR))))

(subclass NewsReporter Profession)
(documentation NewsReporter "The &%Profession of being a news reporter, i.e.
investigating and reporting, in a publication or broadcast program, current
events.")

(subclass Science FieldOfStudy)
(documentation Science "Any &%FieldOfStudy which tests theories on the basis of careful observations and/or experiments and which has a cumulative body of results.")

(subclass SocialScience Science)
(documentation SocialScience "Any &%Science which studies human behavior, either in the aggregate, as do, for example, &%Economics and &%Linguistics, or with respect to the individual, as does &%Psychology.")

(subrelation subField subProposition)
(instance subField TransitiveRelation)
(instance subField IrreflexiveRelation)
(domain subField 1 FieldOfStudy)
(domain subField 2 FieldOfStudy)
(documentation subField "(&%subField ?FIELD1 ?FIELD2) means that ?FIELD1 is a proper part of the &%FieldOfStudy ?FIELD2.  For example, &%Physiology is a &%subField of &%Biology.")

(instance hasExpertise BinaryPredicate)
(domain hasExpertise 1 Human)
(domain hasExpertise 2 FieldOfStudy)
(documentation hasExpertise "(&%hasExpertise ?PERSON ?FIELD) means that ?PERSON has
studied the &%FieldOfStudy ?FIELD and is regarded as an expert.")

(=>
   (hasExpertise ?PERSON ?FIELD)
   (exists (?LEARN)
      (and
         (instance ?LEARN Learning)
         (agent ?LEARN ?PERSON)
         (realization ?LEARN ?FIELD))))

(instance hasOccupation BinaryPredicate)
(domain hasOccupation 1 Human)
(domainSubclass hasOccupation 2 IntentionalProcess)
(documentation hasOccupation "(&%hasOccupation ?PERSON ?WORK) means that ?PERSON
engages in activities of the class ?WORK as a means of earning a living.")

(=>
   (hasOccupation ?PERSON ?WORK)
   (exists (?INST)
      (and
         (instance ?INST ?WORK)
         (instance ?INST Working)
         (agent ?INST ?PERSON))))

(subclass Bleeding AutonomicProcess)
(documentation Bleeding "The release of &%Blood from an &%Animal in response
to an &%Injuring of some sort.")

(=>
   (instance ?BLEED Bleeding)
   (exists (?INJURY)
      (and
         (instance ?INJURY Injuring)
         (causes ?INJURY ?BLEED))))

(=>
   (and
      (instance ?BLEED Bleeding)
      (experiencer ?BLEED ?ANIMAL))
   (instance ?ANIMAL Animal))

(=>
   (and
      (instance ?BLEED Bleeding)
      (experiencer ?BLEED ?ANIMAL))
   (exists (?BLOOD)
      (and
         (instance ?BLOOD Blood)
         (holdsDuring (BeginFn (WhenFn ?BLEED)) (part ?BLOOD ?ANIMAL))
         (holdsDuring (EndFn (WhenFn ?BLEED)) (not (part ?BLOOD ?ANIMAL))))))


;; NOTE that the folllowing class was addeded 01-19-04 by JC
;; in order to reconcile certain propositions which use
;; 'Contract' as a class.  'Contract' was in SUMO a DeonticAttribute.
;; and we substitute LegalContract where a class is required,
;; and changed the attribute 'Contract' to 'Contractual'.
;;  This class is not yet properly axiomatized.

(subclass LegalContract Assertion)
(subclass LegalContract DocumentalProposition)
(hasAttributeInstance LegalContract Contractual)
(documentation LegalContract "A LegalContract is a Proposition consisting of a pair of promises, each conditioned upon the other.")

(=>
    (instance ?Contr LegalContract)
    (hasModalAttribute ?Contr Contractual))

(instance hasAttributeInstance BinaryPredicate)
(instance hasAttributeInstance AsymmetricRelation)
(instance hasAttributeInstance IrreflexiveRelation)
(domainSubclass hasAttributeInstance 1 Entity)
(domain hasAttributeInstance 2 Attribute)
(documentation hasAttributeInstance "hasAttributeInstance is used to assert that every member of a particular class of entities necessarily has a specific attribute.")

(=>
   (hasAttributeInstance ?CLASS ?ATTRIB)
   (forall (?ENT)
      (=>
          (instance ?ENT ?CLASS)
          (property ?ENT ?ATTRIB))))

(subclass ClosingContract Committing)
(documentation ClosingContract "Completing a &%Contract of some sort,
e.g. the purchase of a house, closing a business deal, etc.")

(=>
   (and
      (instance ?CLOSE ClosingContract)
      (agent ?CLOSE ?AGENT)
      (destination ?CLOSE ?CONTRACT)
      (instance ?CONTRACT LegalContract))
   (agreementMember ?CONTRACT ?AGENT))

(subclass Opening Motion)
(relatedInternalConcept Opening Closing)
(documentation Opening "The &%Class of &%Processes where an aperture is
created in an &%Object.  Note that the aperture may be created intentionally, as when one opens a door, or unintentionally, as when the ground ruptures in a seismic event.")

(subclass Closing Motion)
(documentation Closing "The &%Class of &%Processes where an aperture is
closed in an &%Object.")

(subclass PreparedFood Food)
(documentation PreparedFood "&%Food that is the result of &%Cooking.")

(=>
   (instance ?FOOD PreparedFood)
   (exists (?COOK)
      (and
         (instance ?COOK Cooking)
         (result ?COOK ?FOOD))))

(subclass RawFood Food)
(documentation RawFood "&%Food that is not the result of &%Cooking.")

(=>
   (instance ?FOOD RawFood)
   (not (exists (?COOK)
      (and
         (instance ?COOK Cooking)
         (result ?COOK ?FOOD)))))

(subclass SoupStock PreparedFood)
(documentation SoupStock "&%Food which is prepared by reducing &%Meat
and/or &%FruitOrVegetables to a translucent broth which can be used as
a base for soups or sauces.")

(subclass Coffee Beverage)
(subclass Coffee PreparedFood)
(documentation Coffee "A &%Beverage which is prepared by infusing ground,
roasted coffee beans into hot water.")

(subclass DateFruit FruitOrVegetable)
(documentation DateFruit "A &%FruitOrVegetable that is produced by the date
palm.")

(subclass Avocado FruitOrVegetable)
(documentation Avocado "A &%FruitOrVegetable that is shaped like a pear and
has a dark green skin and a rich meat.")

(instance protonNumber BinaryPredicate)
(instance protonNumber SingleValuedRelation)
(instance protonNumber TotalValuedRelation)
(domain protonNumber 1 PureSubstance)
(domain protonNumber 2 PositiveInteger)
(documentation protonNumber "(&%protonNumber ?SUBSTANCE ?NUMBER) means that
the &%PureSubstance ?SUBSTANCE has the number of &%Protons ?NUMBER.")

(=>
   (protonNumber ?SUBSTANCE ?NUMBER)
   (=>
      (and
         (part ?ATOM ?SUBSTANCE)
         (or
            (instance ?ATOM Atom)
            (instance ?ATOM Molecule)))
      (equal ?NUMBER (CardinalityFn (KappaFn ?PROTON
                                       (and
                                          (part ?PROTON ?ATOM)
                                          (instance ?PROTON Proton)))))))

(instance electronNumber BinaryPredicate)
(instance electronNumber SingleValuedRelation)
(instance electronNumber TotalValuedRelation)
(domain electronNumber 1 PureSubstance)
(domain electronNumber 2 PositiveInteger)
(documentation electronNumber "(&%electronNumber ?SUBSTANCE ?NUMBER) means that
the &%PureSubstance ?SUBSTANCE has the number of &%Electrons ?NUMBER.")

(=>
   (electronNumber ?TYPE ?NUMBER)
   (=>
      (and
         (part ?ATOM ?SUBSTANCE)
         (or
            (instance ?ATOM Atom)
            (instance ?ATOM Molecule)))
      (equal ?NUMBER (CardinalityFn (KappaFn ?ELECTRON
                                       (and
                                          (part ?ELECTRON ?ATOM)
                                          (instance ?ELECTRON Electron)))))))


;; =================
;; Elements Ontology
;; =================

;; The following content is borrowed from the Elements Ontology.

(subrelation atomicNumber protonNumber)
(domainSubclass atomicNumber 1 ElementalSubstance)
(domain atomicNumber 2 PositiveInteger)
(documentation atomicNumber "(&%atomicNumber ?ELEMENT ?NUMBER) means that the
&%ElementalSubstance ?ELEMENT has the atomic number ?NUMBER. The atomic number
is the number of &%Protons in the nucleus of an &%Atom.")

(subclass Oxygen ElementalSubstance)
(atomicNumber Oxygen 8)
(documentation Oxygen "A colourless, odourless gaseous element belonging to
group 16 of the periodic table. It is the most abundant element present in
the earth's crust. It also makes up 20.8 percent of the Earth's atmosphere.
For industrial purposes, it is separated from liquid air by fractional
distillation. It is used in high temperature welding, and in breathing. It
commonly comes in the form of Oxygen, but is found as Ozone in the upper
atmosphere. It was discovered by Priestley in 1774.")

(=>
   (and
      (instance ?ATOM Oxygen)
      (instance ?ATOM Atom))
   (measure ?ATOM (MeasureFn 15.9994 Amu)))

(subclass Chlorine ElementalSubstance)
(atomicNumber Chlorine 17)
(documentation Chlorine "Halogen element.  Poisonous greenish-yellow gas.
Occurs widely in nature as sodium chloride in seawater.  Reacts directly
with many elements and compounds, strong oxidizing agent.  Discovered by
Karl Scheele in 1774.  Humphrey David confirmed it as an element in
1810.")

(=>
  (and
    (instance ?ATOM Chlorine)
    (instance ?ATOM Atom))
  (measure ?ATOM (MeasureFn 35.453 Amu)))

(subclass Sodium ElementalSubstance)
(atomicNumber Sodium 11)
(documentation Sodium "Soft silvery reactive element belonging to group 1
of the periodic table (alkali metals).  It is highly reactive, oxidizing
in air and reacting violently with water, forcing it to be kept under oil.
It was first isolated by Humphrey Davy in 1807.")

(=>
  (and
    (instance ?ATOM Sodium)
    (instance ?ATOM Atom))
  (measure ?ATOM (MeasureFn 22.9898 Amu)))

(subclass Hydrogen ElementalSubstance)
(atomicNumber Hydrogen 1)
(documentation Hydrogen "Colourless, odourless gaseous chemical element.
Lightest and most abundant element in the universe.  Present in water and
in all organic compounds.  Chemically reacts with most elements.
Discovered by Henry Cavendish in 1776.")

(=>
  (and
    (instance ?ATOM Hydrogen)
    (instance ?ATOM Atom))
  (measure ?ATOM (MeasureFn 1.0079 Amu)))

(subclass Carbon ElementalSubstance)
(atomicNumber Carbon 6)
(documentation Carbon "Carbon is a member of group 14 of the periodic
table.  It has three allotropic forms of it, diamonds, graphite and
fullerite.  Carbon-14 is commonly used in radioactive dating.  Carbon
occurs in all organic life and is the basis of organic chemistry.  Carbon
has the interesting chemical property of being able to bond with itself,
and a wide variety of other elements.")

(=>
  (and
    (instance ?ATOM Carbon)
    (instance ?ATOM Atom))
  (measure ?ATOM (MeasureFn 12.01115 Amu)))

(subclass Iodine ElementalSubstance)
(atomicNumber Iodine 53)
(documentation Iodine "Dark violet nonmetallic element, belongs to group
17 of the periodic table.  Insoluble in water.  Required as a trace
element for living organisms.  One stable isotope, I-127 exists, in
addition to fourteen radioactive isotopes.  Chemically the least reactive
of the halogens, and the most electropositive metallic halogen.
Discovered in 1812 by Courtois.")

(=>
  (and
    (instance ?ATOM Iodine)
    (instance ?ATOM Atom))
  (measure ?ATOM (MeasureFn 126.904 Amu)))

(subclass Iron ElementalSubstance)
(atomicNumber Iron 26)
(documentation Iron "Silvery malleable and ductile metallic transition
element.  Has nine isotopes and is the fourth most abundant element in the
earth's crust.  Required by living organisms as a trace element (used in
hemoglobin in humans.) Quite reactive, oxidizes in moist air, displaces
hydrogen from dilute acids and combines with nonmetallic elements.")

(=>
  (and
    (instance ?ATOM Iron)
    (instance ?ATOM Atom))
  (measure ?ATOM (MeasureFn 55.847 Amu)))

(subclass Helium ElementalSubstance)
(atomicNumber Helium 2)
(documentation Helium "Colourless, odourless gaseous nonmetallic element.
Belongs to group 18 of the periodic table.  Lowest boiling point of all
elements and can only be solidified under pressure.  Chemically inert, no
known compounds.  Discovered in the solar spectrum in 1868 by Lockyer.")

(=>
  (and
    (instance ?ATOM Helium)
    (instance ?ATOM Atom))
  (measure ?ATOM (MeasureFn 4.0026 Amu)))

(subclass Nitrogen ElementalSubstance)
(atomicNumber Nitrogen 7)
(documentation Nitrogen "Colourless, gaseous element which belongs to
group 15 of the periodic table.  Constitutes ~78 percent of the atmosphere
and is an essential part of the ecosystem.  Nitrogen for industrial
purposes is acquired by the fractional distillation of liquid air.
Chemically inactive, reactive generally only at high temperatures or in
electrical discharges.  It was discovered in 1772 by D.  Rutherford.")

(=>
  (and
    (instance ?ATOM Nitrogen)
    (instance ?ATOM Atom))
  (measure ?ATOM (MeasureFn 14.0067 Amu)))

(subclass Copper ElementalSubstance)
(atomicNumber Copper 29)
(documentation Copper "Red-brown transition element.  Known by the Romans
as 'cuprum.' Extracted and used for thousands of years.  Malleable,
ductile and an excellent conductor of heat and electricity.  When in moist
conditions, a greenish layer forms on the outside.")

(=>
  (and
    (instance ?ATOM Copper)
    (instance ?ATOM Atom))
  (measure ?ATOM (MeasureFn 63.54 Amu)))

(subclass Zinc ElementalSubstance)
(atomicNumber Zinc 30)
(documentation Zinc "Blue-white metallic element.  Occurs in multiple
compounds naturally.  Five stable isotopes are six radioactive isotopes
have been found.  Chemically a reactive metal, combines with oxygen and
other non-metals, reacts with dilute acids to release hydrogen.")

(=>
  (and
    (instance ?ATOM Zinc)
    (instance ?ATOM Atom))
  (measure ?ATOM (MeasureFn 65.38 Amu)))


;; ==================
;; Financial Ontology
;; ==================

;; The following content is borrowed from the Financial Ontology.

(subclass Stock FinancialInstrument)
(documentation Stock "An instrument that signifies an ownership position,
or equity, in a &%Corporation, and represents a claim on its proportionate
share in the corporation's assets and profits.")

(subclass Share CurrencyMeasure)
(instance Share UnitOfMeasure)
(documentation Share "A &%UnitOfMeasure representing one unit of ownership
in a corporation, &%MutualFund, or limited partnership.")

(instance stockOf BinaryPredicate)
(domain stockOf 1 Stock)
(domain stockOf 2 Corporation)
(documentation stockOf "(&%stockOf ?Stock ?Corporation) means that ?Stock
is issued by the &%Corporation ?Corporation.")

(instance stockHolder BinaryPredicate)
(domain stockHolder 1 Stock)
(domain stockHolder 2 CognitiveAgent)
(documentation stockHolder "(&%stockHolder ?Stock ?Agent) means that
?Agent possesses the &%Stock ?Stock.")

(subclass FinancialOrganization Corporation)
(documentation FinancialOrganization "The class &%FinancialOrganization
includes, as subclasses, &%BankOrganization, &%CreditUnion and
&%SavingsAndLoan.")

(subclass BankOrganization FinancialOrganization)
(documentation BankOrganization "An organization, chartered by a state
or federal government, which does most or all of the following: receives
&%Deposits, honors &%FinancialInstruments drawn on them, and pays
interest on them; discounts &%Notes, makes &%Loans, and invests in
&%SecuredLoans; collects &%Checks, &%Drafts and &%Notes, certifies
depositor's checks; and issues drafts and Cashier's checks.")

(subclass CreditUnion FinancialOrganization)
(documentation CreditUnion "Credit unions are non-profit, member-owned,
financial cooperatives. They are operated entirely by and for their
members. When money is deposited in a credit union, the depositor
becomes a member of the union because the deposit is considered partial
ownership in the credit union. Many large organizations have established
credit unions for their employees.")

(subclass SavingsAndLoan FinancialOrganization)
(documentation SavingsAndLoan "A federally or state chartered
&%FinancialOrganization that takes &%Deposits from individuals, funds
&%Mortgages, and pays &%Dividends.")

(subclass FinancialAccount LegalContract)
(documentation FinancialAccount "A formal banking, brokerage, or business
relationship established to provide for regular services, dealings, and
other financial transactions.")

(instance agreementMember BinaryPredicate)
(instance agreementMember TotalValuedRelation)
(domain agreementMember 1 LegalContract)
(domain agreementMember 2 CognitiveAgent)
(documentation agreementMember "(&%agreementMember ?Agreement ?Agent)
means that ?Agent is one of the participants of the &%Contract
?Agreement.")

(subrelation financialAccount agreementMember)
(domain financialAccount 1 FinancialAccount)
(domain financialAccount 2 FinancialOrganization)
(documentation financialAccount "A formal banking, brokerage, or business
relationship established to provide for regular services, dealings, and
other financial transactions.  (&%accountAt ?ACCOUNT ?ORG) means that
?ACCOUNT is a financial account opened at the &%FinancialOrganization
?ORG.")

(instance price TernaryPredicate)
(domain price 1 Object)
(domain price 2 CurrencyMeasure)
(domain price 3 Agent)
(documentation price "(&%price ?Obj ?Money ?Agent) means that ?Agent
pays the amount of money ?Money for ?Obj.")

(=>
   (price ?Obj ?Money ?Agent)
   (exists (?Buying)
      (and
         (instance ?Buying Buying)
         (agent ?Buying ?Agent)
         (patient ?Buying ?Obj)
         (transactionAmount ?Buying ?Money))))

(instance profit AsymmetricRelation)
(domain profit 1 FinancialTransaction)
(domain profit 2 CurrencyMeasure)
(documentation profit "The positive gain from an investment or business
operation after subtracting for all expenses.")

(instance loss AsymmetricRelation)
(domain loss 1 FinancialTransaction)
(domain loss 2 CurrencyMeasure)
(documentation loss "The amount by which the cost of an investment or
business operation exceeds its return, i.e. the negative quantity left
after subtracting for all expenses.")

(subrelation financialAsset possesses)
(domain financialAsset 1 Agent)
(domain financialAsset 2 Object)
(documentation financialAsset "A predicate that relates an &%Agent to any
item of economic value owned by the &%Agent.  Examples of financial assets
are cash, securities, accounts receivable, inventory, office equipment, a
house, a car, and other property.")

(=>
	(financialAsset ?Agent ?Asset)
	(exists (?Value)
		(monetaryValue ?Asset ?Value)))

(subclass SomethingOfValue Entity)
(documentation SomethingOfValue "Any entity which has any kind of value, monetary or emotional, to a cognitive agent. The value may be negative.")

(subclass SomethingOfPositiveValue SomethingOfValue)
(documentation SomethingOfPositiveValue "Anything which is viewed as a positive or good thing by a CognitiveAgent.  CognitiveAgents will try to acquire good things unless they are associated with something negative.")

(subclass SomethingOfNegativeValue SomethingOfValue)
(documentation SomethingOfNegativeValue "Anything which is viewed as a negative or bad thing by a CognitiveAgent.  CognitiveAgents will try to avoid bad things unless they are associated with something positive.")

(subclass Asset SomethingOfPositiveValue)
(documentation Asset "Something of positive monetary value owned by a cognitive agent.  Something that has negative monetary vaue is an obligaion or a debt.")

;;  <axcom>An asset belongs to a CognitiveAgent.</axcom>

(=>
	(instance ?Asset Asset)
	(exists (?Agent)
		(and
		  (instance ?Agent CognitiveAgent)
		  (financialAsset ?Agent ?Asset))))


;;  <axcom>An asset has a positive monetary value for the agent that owns it.</axcom>

(=>
	(instance ?Asset Asset)
	(exists (?Value)
		(and
		  (instance ?Value CurrencyMeasure)
		  (monetaryValue ?Asset ?Value)
		  (greaterThanOrEqualTo MagnitudeFn(?Value) 0))))

(subclass Debt SomethingOfNegativeValue)
(documentation Debt "Something of negative monetary value which is an obligation (legal or moral) of a CognitiveAgent.")

;;  <axcom>A debt has a negative monetary value to the agent.  Note that debts are always expressed as negative monetary values -- a "debt" with a negative value would have to be an asset.</axcom>

(=>
	(instance ?Debt Debt)
	(exists (?Value)
		(and
		  (instance ?Value CurrencyMeasure)
		  (monetaryValue ?Debt ?Value)
		  (greaterThan 0 MagnitudeFn(?Value)))))

(instance hasAssociatedSituation BinaryPredicate)
(domain hasAssociatedSituation 1 Situation)
(domain hasAssociatedSituation 2 Situation)
(documentation hasAssociatedSituation "(hasAssociatedSituation ?Sit1 ?Sit2) asserts that ?Sit1 has ?Sit2 associated with it, either necessarily or pragmatically (i.e. probabilistically, e.g. one situation usually is followed by or simultaneously occurs with ?Sit2, whether causally or otherwise).  Such associations are important to Agent in determining whether one action may be beneficial or harmful.")

(subclass FillingAnOrder FinancialTransaction)
(documentation FillingAnOrder "Execute an order or buy or sell a &%Security.")

(=>
   (instance ?FILL FillingAnOrder)
   (exists (?OBJ)
      (and
         (patient ?TRANS ?OBJ)
         (instance ?OBJ Security))))

(subclass Security FinancialInstrument)
(documentation Security "An investment instrument, other than an insurance
policy or a fixed annuity issued by a corporation, government, or other
organization which offers evidence of debt or equity.")

(instance currentAccountBalance TernaryPredicate)
(domain currentAccountBalance 1 FinancialAccount)
(domain currentAccountBalance 2 Day)
(domain currentAccountBalance 3 CurrencyMeasure)
(documentation currentAccountBalance "(&%currentAccountBalance ?Account
?Date ?Amount) means that ?Amount is the balance of the &%FinancialAccount
?Account as of the date ?Date.")

(instance interestEarned TernaryPredicate)
(domain interestEarned 1 FinancialAccount)
(domain interestEarned 2 CurrencyMeasure)
(domain interestEarned 3 TimeInterval)
(documentation interestEarned "(&%interestEarned ?Account ?Interest ?Period)
means that ?Interest is the amount earned on the &%FinancialAccount ?Account,
for the &%TimeInterval ?Period.")

(=>
   (and
      (interestEarned ?Account ?Interest ?Time)
      (currentAccountBalance ?Account (BeginFn ?Time) ?Balance))
   (currentAccountBalance ?Account (EndFn ?Time) (AdditionFn ?Balance ?Interest)))

(subclass FinancialService CommercialService)
(documentation FinancialService "Services performed by
&%FinancialOrganizations.")

(=>
	(instance ?Service FinancialService)
	(exists (?Org)
		(and
			(instance ?Org FinancialOrganization)
			(agent ?Service ?Org))))

(instance customer IrreflexiveRelation)
(instance customer AsymmetricRelation)
(instance customer BinaryPredicate)
(domain customer 1 Agent)
(domain customer 2 Corporation)
(documentation customer "A very general relation that exists whenever there
is a &%CommercialService between an &%Agent and an &%Organization.
(&%customer ?AGENT ?ORG) means that ?AGENT is a customer of the &%Corporation
?ORG.")

(<=>
	(customer ?Agent ?Org)
	(exists (?Service)
		(and
			(instance ?Service CommercialService)
			(agent ?Service ?Org)
			(destination ?Service ?Agent))))

(=>
	(and
		(accountHolder ?Account ?Agent)
		(financialAccount ?Account ?Bank))
	(customer ?Agent ?Bank))

(subrelation accountHolder agreementMember)
(domain accountHolder 1 FinancialAccount)
(domain accountHolder 2 CognitiveAgent)
(documentation accountHolder "(&%accountHolder ?Account ?Agent) means that
?Agent is the account holder of the &%FinancialAccount ?Account.")

(instance income TernaryPredicate)
(domain income 1 Human)
(domain income 2 CurrencyMeasure)
(domain income 3 TimeInterval)
(documentation income "(&%income ?Agent ?Money ?Period) means that
?Money is the amount of money or its equivalent received during a period
of time in exchange for labor or services, from the sale of goods or
property, or as profit from financial investments")

(instance incomeEarned TernaryPredicate)
(domain incomeEarned 1 Human)
(domain incomeEarned 2 CurrencyMeasure)
(domain incomeEarned 3 IntentionalProcess)
(documentation incomeEarned "(&%incomeEarned ?Agent ?Money ?Action) means
that ?Agent earned the amount of money ?Money from performing ?Action.  Note
that &%incomeEarned denotes that amount of money made before taxes are
deducted.")

(=>
	(incomeEarned ?Agent ?Money ?Activity)
	(agent ?Activity ?Agent))

(subclass ChargingAFee FinancialTransaction)
(documentation ChargingAFee "An activity of a fee being charged")

(subclass Tax ChargingAFee)
(documentation Tax "A fee charged by a government on a product, income
or activity.")

(=>
	(instance ?Tax Tax)
	(exists (?Org)
		(and
			(instance ?Org Government)
			(agent ?Tax ?Org))))

(subclass DutyTax Tax)
(documentation DutyTax "A &%Tax that is levied on imports and/or exports.")

(=>
   (and
      (instance ?TAX DutyTax)
      (patient ?TAX ?OBJ))
   (exists (?TRANSFER ?NATION1 ?NATION2)
      (and
         (instance ?TRANSFER Transfer)
         (patient ?TRANSFER ?OBJ)
         (origin ?TRANSFER ?NATION1)
         (destination ?TRANSFER ?NATION2)
         (instance ?NATION1 Nation)
         (instance ?NATION2 Nation)
         (not (equal ?NATION1 ?NATION2))
         (earlier (WhenFn ?TRANSFER) (WhenFn ?TAX))
         (causes ?TRANSFER ?TAX))))

;; definition of Payment

(subclass Payment FinancialTransaction)
(documentation Payment "The partial or complete discharge of an obligation by its settlement in the form of the transfer of funds, assets, or services equal to the monetary value of part or all of the debtor's obligation.")

(=>
	(and
		(instance ?Payment Payment)
		(origin ?Payment ?Account)
		(instance ?Account FinancialAccount)
		(transactionAmount ?Payment ?Amount)
		(currentAccountBalance ?Account (ImmediatePastFn (WhenFn ?Payment)) ?Balance1)
		(equal ?Balance2 (SubtractionFn ?Balance1 ?Amount)))
	(currentAccountBalance ?Account (ImmediateFutureFn (WhenFn ?Payment)) ?Balance2))

;; definition of Check

(subclass Check FinancialInstrument)
(documentation Check "A &%FinancialInstrument drawn against deposited funds,
to pay a specified amount of money to a specific person upon demand.")

(=>
	(instance ?Check Check)
	(exists (?Value)
		(monetaryValue ?Check ?Value)))

(=>
	(instance ?Check Check)
	(exists (?Account)
		(checkAccount ?Check ?Account)))

(=>
	(and
		(monetaryValue ?Check ?Amount)
		(checkAccount ?Check ?Account)
		(instance ?Processing ProcessingACheck)
		(patient ?Processing ?Check))
	(exists (?Withdrawal)
		(and
			(instance ?Withdrawal Withdrawal)
			(instrument ?Withdrawal ?Check)
			(meetsTemporally (WhenFn ?Processing) (WhenFn ?Withdrawal))
			(transactionAmount ?Withdrawal ?Amount)
			(origin ?Withdrawal ?Account))))

(=>
	(and
		(monetaryValue ?Check ?Amount)
		(instance ?Processing ProcessingACheck)
		(patient ?Processing ?Check)
		(destination ?Processing ?Account)
		(instance ?Account FinancialAccount))
	(exists (?Deposit)
		(and
			(instance ?Deposit Deposit)
			(instrument ?Deposit ?Check)
			(meetsTemporally (WhenFn ?Processing) (WhenFn ?Deposit))
			(transactionAmount ?Deposit ?Amount)
			(destination ?Deposit ?Account))))

(=>
	(and
		(instance ?Check Check)
		(instance ?Processing ProcessingACheck)
		(patient ?Processing ?Check))
	(exists (?Depositing)
		(and
			(instance ?Depositing DepositingACheck)
			(patient ?Depositing ?Check)
			(time ?Depositing (ImmediatePastFn (WhenFn ?Processing))))))

(=>
	(and
		(instance ?Depositing DepositingACheck)
		(instance ?Check Check)
		(agent ?Depositing ?Agent))
	(signedBy ?Check ?Agent))

(=>
	(and
		(instance ?Drawing DrawingACheck)
		(patient ?Drawing ?Check)
		(instance ?Processing ProcessingACheck)
		(patient ?Processing ?Check)
		(meetsTemporally (WhenFn ?Drawing) ?Time)
		(meetsTemporally ?Time (WhenFn ?Processing))
            (duration ?Time ?Duration))
	(lessThan ?Duration (MeasureFn 6 MonthDuration)))

;; Definition of PayCheck.

(subclass PayCheck Check)
(documentation PayCheck "A check issued to an employee in payment of salary or wages.")

(=>
	(and
	    (instance ?Check Paycheck)
	    (issuedBy ?Check ?Organization)
          (instance ?Give Giving)
          (destination ?Give ?Agent))
	(employs ?Organization ?Agent))

(subclass EconomicActivity IntentionalProcess)
(documentation EconomicActivity "Broadly, any intentional action by a CognitiveAgent that involves creation or consumption of articles of commerce, transfer or conversion of financial assets, or any other action performed with the general intention of designing, creating, transporting, or exchanging things of value.")

(subclass UsingAnAccount EconomicActivity)
(documentation UsingAnAccount "Putting money into ro taking money out of a FinancialAccount.")

;; definition of checkAccount

(instance checkAccount BinaryPredicate)
(domain checkAccount 1 Check)
(domain checkAccount 2 FinancialAccount)
(documentation checkAccount "(&%checkAccount ?Check ?Account) means that ?Account
is the &%FinancialAccount from which the amount specifed on the check is paid.")

;; definition of DrawingACheck

(subclass DrawingACheck UsingAnAccount)
(documentation DrawingACheck "An activity of paying by a check.")

(=>
	(and
		(instance ?Drawing DrawingACheck)
		(patient ?Drawing ?Check)
		(agent ?Drawing ?Agent)
		(checkAccount ?Check ?Account))
	(accountHolder ?Account ?Agent))

;; definition of DepositingACheck

(subclass DepositingACheck UsingAnAccount)
(documentation DepositingACheck "An activity of depositing a check into a
&%FinancialOrganization.")

(=>
	(instance ?Depositing DepositingACheck)
	(exists (?Check)
		(and
			(instance ?Check Check)
			(patient ?Depositing ?Check))))

(=>
	(and
		(instance ?Depositing DepositingACheck)
		(patient ?Depositing ?Check)
		(instance ?Check Check)
		(checkAccount ?Check ?Account)
		(monetaryValue ?Check ?Amount))
	(exists (?Deposit)
		(and
			(instance ?Deposit Deposit)
			(destination ?Deposit ?Account)
			(transactionAmount ?Deposit ?Amount))))

;; definition of ProcessingACheck

(subclass ProcessingACheck AuthorizationOfTransaction)
(documentation ProcessingACheck "An activity of paying the amount specified on the
check from funds on deposit.")

(=>
	(instance ?Processing ProcessingACheck)
	(exists (?Check)
		(and
			(instance ?Check Check)
			(patient ?Processing ?Check))))

(=>
	(instance ?Processing ProcessingACheck)
	(exists (?Authorization)
		(and
			(instance ?Authorization AuthorizationOfTransaction)
			(subProcess ?Authorization ?Processing))))

;; definition of DepositAccount

(subclass DepositAccount FinancialAccount)
(documentation DepositAccount "An account where money is deposited for checking, savings or
brokerage use.")

;; Definition of CheckingAccount

(subclass CheckingAccount DepositAccount)
(documentation CheckingAccount "A bank account against which the depositor can draw checks")

(=>
	(and
		(instance ?Account CheckingAccount)
		(instance ?Transaction FinancialTransaction)
		(origin ?Transaction ?Account))
	(exists (?Check)
		(and
			(instance ?Check Check)
			(instrument ?Transaction ?Check))))

;; definition of AuthorizationOfTransaction

(subclass AuthorizationOfTransaction FinancialService)
(subclass AuthorizationOfTransaction RegulatoryProcess)
(documentation AuthorizationOfTransaction "An activity which approves or
disapproves a transaction.")

(=>
	(and
		(instance ?Authorization AuthorizationOfTransaction)
		(patient ?Authorization ?Account)
		(financialAccount ?Account ?Bank))
	(agent ?Authorization ?Bank))

(=>
	(and
		(financialAccount ?Account ?Organization)
		(instance ?Transaction FinancialTransaction)
		(origin ?Transaction ?Account))
	(exists (?Authorization)
		(and
			(instance ?Authorization AuthorizationOfTransaction)
			(subProcess ?Authorization ?Transaction))))

;; definition of Deposit

(subclass Deposit FinancialTransaction)
(disjoint Deposit Withdrawal)
(documentation Deposit "An Activity of money being transferred into a customer's
account at a financial institution.")

(=>
	(instance ?Deposit Deposit)
	(exists (?Account)
            (and
                  (instance ?Account FinancialAccount)
		      (destination ?Deposit ?Account))))

(=>
	(and
		(instance ?Deposit Deposit)
		(time ?Deposit ?TimeOfDeposit)
		(instance ?Account FinancialAccount)
		(destination ?Deposit ?Account)
		(transactionAmount ?Deposit ?Amount)
		(currentAccountBalance ?Account (ImmediatePastFn (WhenFn ?Deposit)) ?Balance1)
		(equal ?Balance2 (AdditionFn ?Balance1 ?Amount)))
	(currentAccountBalance ?Account (ImmediateFutureFn (FutureFn ?Deposit)) ?Balance2))

;; definition of Withdrawal

(subclass Withdrawal FinancialTransaction)
(documentation Withdrawal "An activity of money being transferred from a customer's
account at a financial institution.")

(=>
	(instance ?Withdrawal Withdrawal)
	(exists (?Account)
		(and
			(instance ?Account FinancialAccount)
			(origin ?Withdrawal ?Account))))

(=>
	(and
		(instance ?Withdrawal Withdrawal)
		(time ?Withdrawal ?TimeOfWithdrawal)
		(instance ?Account FinancialAccount)
		(origin ?Withdrawal ?Account)
		(transactionAmount ?Withdrawal ?Amount)
		(currentAccountBalance ?Account (ImmediatePastFn (WhenFn ?Withdrawal)) ?Balance1)
		(equal ?Balance2 (SubtractionFn ?Balance1 ?Amount)))
	(currentAccountBalance ?Account (ImmediateFutureFn (FutureFn ?Withdrawal)) ?Balance2))

;; definition of issuedBy

(instance issuedBy BinaryPredicate)
(domain issuedBy 1 FinancialInstrument)
(domain issuedBy 2 CognitiveAgent)
(documentation issuedBy "(&%issuedBy ?Instrument ?Agent) means that a
&%FinancialInstrument ?Instrument is produced and offered by ?Agent.")

;; definition of signedBy

(instance signedBy BinaryPredicate)
(domain signedBy 1 FinancialInstrument)
(domain signedBy 2 CognitiveAgent)
(documentation signedBy "(&%signedBy ?Instrument ?Agent) means that ?Instrument
has been signed by ?Agent.")

(<=>
   (signedBy ?INSTRUMENT ?AGENT)
   (exists (?SIGN)
      (and
         (instance ?SIGN SigningADocument)
         (agent ?SIGN ?AGENT)
         (resource ?SIGN ?INSTRUMENT))))

;; definition of Investing

(subclass Investing FinancialTransaction)
(documentation Investing "An activity of commiting money or capital in order to
gain a financial return.")

(=>
	(and
		(agent ?Purchase ?Buyer)
		(origin ?Purchase ?Seller)
		(patient ?Purchase ?Object)
		(monetaryValue ?Object ?Money))
	(exists (?Payment)
		(and
			(subProcess ?Payment ?Purchase)
			(instance ?Payment Payment)
			(transactionAmount ?Payment ?Money)
			(destination ?Payment ?Seller))))

;; Definition of Bond.

(subclass Bond FinancialInstrument)
(documentation Bond "A debt instrument issued for a period of more than one year with
the purpose of raising capital by borrowing. The Federal government, states, cities,
corporations, and many other types of institutions sell bonds. A bond is generally a
promise to repay the principal along with interest on a specified &%maturityDate.")

(=>
	(instance ?Bond Bond)
	(exists (?Date)
		(maturityDate ?Bond ?Date)))


(=>
	(and
		(instance ?Bond Bond)
		(couponInterest ?Bond ?Interest)
		(possesses ?BondHolder ?Bond))
	(exists (?Period)
		(and
			(periodicPayment (AccountFn ?Bond) ?Interest ?Period)
			(destination ?Interest ?BondHolder))))

;; Definition of couponInterest.

(instance couponInterest BinaryPredicate)
(domain couponInterest 1 Bond)
(domain couponInterest 2 CurrencyMeasure)
(documentation couponInterest "(&%couponInterest ?BOND ?INTEREST) means that ?INTEREST is
the periodic interest payment made to bondholders during the life of the ?BOND.")

;; Definition of maturityDate.

(instance maturityDate BinaryPredicate)
(domain maturityDate 1 FinancialAccount)
(domain  maturityDate  2 Day)
(documentation maturityDate "The date on which the principal amount of the account
becomes due and payable.")

(=>
	(and
		(maturityDate ?Account ?Date)
		(principalAmount ?Account ?Principal))
	(amountDue ?Account ?Principal ?Date))

(<=>
      (exists (?Period)
	   (and
		(agreementPeriod ?Account ?Period)
		(finishes ?End ?Period)))
	(maturityDate ?Account ?End))

;; Definition of principalAmount.

(instance principalAmount BinaryPredicate)
(domain principalAmount 1 FinancialAccount)
(domain principalAmount 2 CurrencyMeasure)
(documentation principalAmount "(&%principalAmount ?ACCOUNT ?BALANCE) means
that ?BALANCE is the amount originally borrowed (excluding interest).")

;; Definition of periodicPayment.

(instance periodicPayment TernaryPredicate)
(domain periodicPayment 1 FinancialAccount)
(domain periodicPayment 2 CurrencyMeasure)
(domain periodicPayment 3 TimeDuration)
(documentation periodicPayment "(&%periodicPayment ?Account ?Amount ?Period) holds if
?Amount is the amount that must be made from the &%FinancialAccount ?Account after every
period of duration ?Period.")

(=>
	(periodicPayment ?Account ?Amount ?Period)
	(exists (?Payment ?Type)
		(and
			(instance ?Payment ?Type)
                  (subclass ?Type Payment)
			(origin ?Payment ?Account)
			(transactionAmount ?Payment ?Amount)
			(frequency ?Type ?Period))))

;; Definition of amountDue.

(instance amountDue TernaryPredicate)
(domain amountDue  1 FinancialAccount)
(domain amountDue 2 CurrencyMeasure)
(domain amountDue 3 TimePosition)
(documentation amountDue "(&%amountDue ?ACCOUNT ?AMOUNT ?DATE) means ?DATE is the
date on which the amount of Money ?AMOUNT of a particular ?ACCOUNT is due and payable")

(=>
	(and
		(amountDue ?Account ?Amount ?DueDate)
		(accountHolder ?Account ?Agent))
	(holdsObligation (exists (?Payment ?Date)
				  (and
					(instance ?Payment Payment)
					(transactionAmount ?Payment ?Amount)
                              (agent ?Payment ?Agent)
					(origin ?Payment ?Account)
					(date ?Payment ?Date)
					(beforeOrEqual (EndFn ?Date) (EndFn ?DueDate)))) ?Agent))

;; definition of securedBy

(subclass Collateral Asset)
(documentation Collateral "An Asset which has been pledged as security for some form of credit.")

(instance securedBy BinaryPredicate)
(domain securedBy 1 FinancialAccount)
(domain securedBy 2 Collateral)
(documentation securedBy "Assets pledged by a borrower to secure a loan or other credit, and subject to seizure in the event of &%FinancialDefault.")


;; ==================
;; Geography Ontology
;; ==================

;; The following content is borrowed from the Geography Ontology.

(subclass Fishing Hunting)
(documentation Fishing "&%Fishing is the class of &%Processes in which
&%Fish are hunted.")

(=>
	(and
		(instance ?FISHING Fishing)
		(patient ?FISHING ?TARGET)
		(instance ?TARGET Animal))
	(instance ?TARGET Fish))

(subclass Hunting Pursuing)
(documentation Hunting "&%Hunting is the class of &%Processes in which
an animal or animals are pursued and sometimes captured and/or killed.")

(=>
	(instance ?HUNT Hunting)
	(exists (?TARGET)
		(and
			(instance ?TARGET Animal)
			(patient ?HUNT ?TARGET))))

(=>
     (and
        (instance ?HUNT Hunting)
        (patient ?HUNT ?PREY))
     (hasPurpose ?HUNT (exists (?PROC)
                          (and
                             (patient ?PROC ?PREY)
                             (or
                                (instance ?PROC Confining)
                                (instance ?PROC Killing))))))

(subclass SquareMeter AreaMeasure)
(instance SquareMeter UnitOfMeasure)
(documentation SquareMeter "&%SquareMeter represents a &%UnitOfMeasure
equal to one square &%Meter.")

(equal (MeasureFn 1 SquareMeter)
	 (PerFn (MeasureFn 1 Meter)(MeasureFn 1 Meter)))

(subclass Earthquake GeologicalProcess)
(documentation Earthquake "&%Earthquake is the class of events in
which the earth shakes while its layers readjust due to tensional
stresses in the surface of the earth.  A single earthquake may consist
of one or more &%EarthTremors.")

(=>
	(instance ?QUAKE Earthquake)
	(exists (?TREMOR)
		(and
			(instance ?TREMOR EarthTremor)
			(subProcess ?TREMOR ?QUAKE))))

(subclass EarthTremor GeologicalProcess)
(documentation EarthTremor "An &%EarthTremor is an individual seismic
event in which the earth shakes due to release of seismic pressures.")

(subclass Planting Putting)
(documentation Planting "&%Planting is the class of processes in
which &%Plants are planted or transplanted, whether as seeds, seedlings,
or mature plants.")

(=>
   (and
      (instance ?PLANT Planting)
      (patient ?PLANT ?OBJ))
   (or
      (instance ?OBJ Plant)
      (instance ?OBJ Seed)
      (instance ?OBJ Spore)))

(instance DocumentFn UnaryFunction)
(domain DocumentFn 1 Proposition)
(rangeSubclass DocumentFn Text)
(documentation DocumentFn "(&%DocumentFn ?PROP) denotes the class
of &%ContentBearingObjects that contain the information ?PROP.")

(=>
	(instance ?TEXT (DocumentFn ?PROP))
	(containsInformation ?TEXT ?PROP))

(subclass SigningADocument Committing)
(documentation SigningADocument "&%SigningADocument is the class of
actions in which an agent affixes a signature, stamp, or other evidence
of authorization or attestation to a document.  The document and signature
may be electronic.  Signings count as &%SocialInteractions even if done in
private, because their significance derives from a social context.")

(=>
   (and
	(instance ?SIGN SigningADocument)
      (agent ?SIGN ?AGENT)
      (result ?SIGN ?SIGNATURE)
      (resource ?SIGN ?TEXT))
   (exists (?NAME)
      (and
         (names ?NAME ?AGENT)
         (refers ?SIGNATURE ?NAME)
         (instance ?TEXT Text))))

(subclass Star AstronomicalBody)
(documentation Star "&%Star is the class of hot gaseous astronomical bodies.")

(instance Sol Star)
(documentation Sol "&%Sol is the nearest &%Star to &%PlanetEarth and
the focus of its &%SolarSystem.")

(subclass SolarSystem Collection)
(documentation SolarSystem "&%SolarSystem is the class of systems that
consist of a star or stars and any encircling astronomical bodies.")

(subclass Meteoroid AstronomicalBody)
(documentation Meteoroid "Any &%AstronomicalBody that breaks through the
atmosphere of &%Earth.")

(subclass Meteorite Meteoroid)
(documentation Meteorite "Any &%Meteoroid that leaves traces on the surface
of &%Earth.")

(instance orbits BinaryPredicate)
(instance orbits AsymmetricRelation)
(domain orbits 1 Object)
(domain orbits 2 AstronomicalBody)
(documentation orbits "(&%orbits ?SATELLITE ?FOCUS) means that the &%Object
?SATELLITE revolves around the &%AstronomicalBody ?FOCUS.")

(=>
	(instance ?SAT Satellite)
	(exists (?BODY)
		(and
			(instance ?BODY AstronomicalBody)
			(orbits ?SAT ?FOCUS))))

(subclass NaturalSatellite Satellite)
(subclass NaturalSatellite AstronomicalBody)
(disjoint NaturalSatellite ArtificialSatellite)
(documentation NaturalSatellite "&%NaturalSatellite is the class of
large, naturally occurring astronomical bodies orbiting some other
&%AstronomicalBody.")

(subclass Moon NaturalSatellite)
(documentation Moon "&%Moon is the class of &%NaturalSatellites that
orbit planets or large asteroids.")

(subclass Planet NaturalSatellite)
(documentation Planet "&%Planet is the class of large &%NaturalSatellites
that revolve around a star.")

(=>
	(instance ?AREA GeographicArea)
	(geographicSubregion ?AREA PlanetEarth))

(subclass WeatherFront WeatherProcess)
(documentation WeatherFront "&%WeatherFront is the class of weather
processes that are involve relationships between two air masses, such
as a high pressure weather system or a low pressure system.")

(=>
   (instance ?WEATHER WeatherFront)
   (exists (?FRONT)
      (and
         (instance ?FRONT StormFront)
         (located ?WEATHER ?FRONT))))

(instance barometricPressure BinaryPredicate)
(instance barometricPressure AsymmetricRelation)
(subrelation barometricPressure measure)
(domain barometricPressure 1 Object)
(domain barometricPressure 2 PressureMeasure)
(documentation barometricPressure "(&%barometricPressure ?AREA ?PRESSURE)
means that the atmospheric pressure measured at ?AREA is ?PRESSURE.
Barometric pressure is typically expressed in units of &%InchMercury or
&%MmMercury.  For example, standard sea level pressure is 29.92 inches
(760 mm) of mercury: (&%barometricPressure &%SeaLevel (&%MeasureFn 29.92
&%InchMercury)).")

(subclass PressureMeasure ConstantQuantity)
(documentation PressureMeasure "&%PressureMeasure is the class
of &%UnitsOfMeasure used to measure pressure (&%barometricPressure),
e.g., &%InchMercury.")

(subclass InchMercury PressureMeasure)
(instance InchMercury UnitOfMeasure)
(documentation InchMercury "&%InchMercury is a &%UnitOfMeasure
for &%barometricPressure.  It is used to express the number of
inches of mercury supported in a mercurial barometer by the
surrounding air pressure.")

(subclass MmMercury PressureMeasure)
(instance MmMercury UnitOfMeasure)
(documentation MmMercury "&%MmMercury is a &%UnitOfMeasure
for &%barometricPressure.  It is used to express the number
of millimeters of mercury supported in a mercurial barometer
 by the surrounding air pressure.")

(subclass Atmosphere Region)
(subclass Atmosphere (ExtensionFn Gas))
(documentation Atmosphere "&%Atmosphere is a mixture of gases
surrounding any celestial object that has a gravitational field
strong enough to prevent the gases from escaping.")

(=>
	(instance ?AIR Atmosphere)
	(exists (?BODY)
		(and
			(instance ?BODY AstronomicalBody)
			(meetsSpatially ?AIR ?BODY))))

(instance EarthsAtmosphere Atmosphere)
(documentation EarthsAtmosphere "The envelope of air surrounding the Earth, extending about 100 miles and thinning gradually outward.")

(subclass AtmosphericRegion GeographicArea)
(documentation AtmosphericRegion "&%AtmosphericRegion is the class of
all subregions of &%EarthsAtmosphere.")

(=>
	(instance ?AIRSPACE AtmosphericRegion)
	(part ?AIRSPACE EarthsAtmosphere))

(subclass FlowRegion SelfConnectedObject)
(subclass FlowRegion (ExtensionFn Fluid))
(documentation FlowRegion "&%FlowRegion is a class of things whose
boundaries are relatively stable but whose constitutive &%material is
continuously moving through the region itself and being replaced by
other, similar material.  Each &%FlowRegion is constituted by a stream
of matter moving as a whole.  A &%FlowRegion may be liquid or gaseous.
A wind may be considered as a &%Process or as a &%FlowRegion, similarly
an ocean current or a &%WaterWave.  Note that certain
properties belong to the &%FlowRegion itself (e.g., mass, length, volume,
temperature, and speed or velocity of the region moving as a whole),
while other properties of interest belong to the &%Motion of its
constitutive stuff (e.g., velocity, direction).  The motion of
a &%FlowRegion as a whole (e.g. the jet stream moves within the atmosphere)
is distinguished from the motion of the &%pieces of stuff constituting the
&%FlowRegion.")

(=>
	(instance ?FLUID FlowRegion)
	(attribute ?FLUID Fluid))

(subclass AirStream FlowRegion)
(subclass AirStream Air)
(documentation AirStream "&%AirStream is the class of &%FlowRegions that
consist of air.")

(subclass Falling Translocation)
(subclass Falling MotionDownward)
(documentation Falling "&%Falling is the class of events in
which something moves from a higher location to a lower location
under the force of gravity.")

(=>
	(and
		(instance ?DROP Falling)
		(origin ?DROP ?START)
		(destination ?DROP ?FINISH))
	(orientation ?FINISH ?START Below))

(subclass Raining Precipitation)
(documentation Raining "&%Raining is a precipitation process
in which water falls in a &%Liquid state.")

(=>
	(instance ?PROCESS Raining)
	(attribute ?PROCESS Liquid))

(subclass Snowing Precipitation)
(documentation Snowing "&%Snowing is a precipitation process
in which water falls in a &%Solid state.")

(=>
	(instance ?PROCESS Snowing)
	(attribute ?PROCESS Solid))

(subclass BotanicalTree FloweringPlant)
(subclass BotanicalTree Tree)
(documentation BotanicalTree "&%BotanicalTree is an imprecise term
for a perennial woody plant that is larger than a bush or shrub,
generally understood to describe a large growth having one main trunk
with few or no branches projecting from its base, a well-developed crown
of foliage, and a height at maturity of at least 12 feet.")

(=>
   (instance ?TREE BotanicalTree)
   (exists (?WOOD)
      (and
         (instance ?WOOD Wood)
         (part ?WOOD ?TREE))))

(subclass Shrub FloweringPlant)
(documentation Shrub "&%Shrub is the class of low, perennial,
typically multi-stemmed woody plants, called shrubs or bushes.")

(=>
	(and
		(instance ?TREE BotanicalTree)
		(instance ?BUSH Shrub)
		(height ?TREE ?TALL)
		(height ?BUSH ?SHORT))
	(greaterThan ?TALL ?SHORT))

(subclass Forest LandArea)
(disjoint Forest Field)
(documentation Forest "&%Forest is the class of large &%LandAreas that
are covered by trees and associated undergrowth, either growing wild or
managed for the purpose of timber production.")

(=>
   (instance ?WOODS Forest)
   (exists (?TREE)
      (and
         (instance ?TREE BotanicalTree)
         (located ?TREE ?WOODS))))

(subclass Agriculture Maintaining)
(documentation Agriculture "&%Agriculture is a class of &%Processes
in which land, plants, or animals are cultivated in order to produce
food or other organic products.")

(=>
	(instance ?AGRO Agriculture)
	(exists (?GROWTH)
		(and
			(instance ?GROWTH Growth)
			(subProcess ?GROWTH ?AGRO))))

(subclass Inlet BodyOfWater)
(documentation Inlet "&%Inlet is the class of bays or other recesses
into the shore of a lake, sea, or river; includes narrow inlets, which
are passages leading from open water through some barrier to a bay or
lagoon.")

(=>
	(instance ?INLET Inlet)
	(exists (?LAND)
		(and
			(instance ?LAND LandArea)
			(penetrates ?INLET ?LAND))))

(=>
	(instance ?INLET Inlet)
	(exists (?WATER)
		(and
			(instance ?WATER WaterArea)
			(connected ?INLET ?WATER))))

(=>
	(and
		(instance ?INLET Inlet)
		(meetsSpatially ?INLET ?WATER)
		(instance ?WATER SaltWaterArea))
	(instance ?INLET SaltWaterArea))

(=>
	(and
		(instance ?INLET Inlet)
		(meetsSpatially ?INLET ?WATER)
		(instance ?WATER FreshWaterArea))
	(instance ?INLET FreshWaterArea))

(subclass BodyOfWater WaterArea)
(subclass BodyOfWater SelfConnectedObject)
(documentation BodyOfWater "A &%BodyOfWater is a connected body of
water with established boundaries marked by either geographical features
or conventional borders.")

(subclass River BodyOfWater)
(subclass River StreamWaterArea)
(subclass River FreshWaterArea)
(documentation River "&%River is the class of large streams of fresh
water flowing through land into a lake, ocean, or other body of water.")

(instance WorldOcean SaltWaterArea)
(instance WorldOcean BodyOfWater)
(documentation WorldOcean "The &%WorldOcean is the collective mass of
sea water that covers 70% of the surface of &%PlanetEarth, surrounding
all of its dry land areas.  Earth's individual &%Oceans are parts of
the &%WorldOcean.")

(=>
	(instance ?AREA Continent)
	(meetsSpatially ?AREA WorldOcean))

(subclass Ocean SaltWaterArea)
(subclass Ocean BodyOfWater)
(documentation Ocean "&%Ocean is the class containing the oceans
that are the major subdivisions of the &%WorldOcean.  According to
the International Hydrographic Association, there are five oceans:
the &%AtlanticOcean, &%PacificOcean, &%IndianOcean, &%SouthernOcean,
and &%ArcticOcean.  Note: The largest oceans, the Atlantic and Pacific,
are subdivided into Northern and Southern regions, but those regions
are not separate &%Oceans.")

(=>
	(and
		(instance ?WATER BodyOfWater)
		(not (instance ?WATER Ocean))
		(instance ?OCEAN Ocean))
	(larger ?OCEAN ?water))

(=>
	(instance ?OCEAN Ocean)
	(properPart ?OCEAN WorldOcean))

(instance AtlanticOcean Ocean)
(documentation AtlanticOcean "&%AtlanticOcean represents the Atlantic
Ocean.")

(instance PacificOcean Ocean)
(documentation PacificOcean "&%PacificOcean represents the Pacific
Ocean.")

(subclass LandForm LandArea)
(documentation LandForm "A &%LandForm is the class of geographically and/or
geologically distinct areas that occur on Earth's surface, including mountains,
hills, plains, valleys, deltas, and features of submerged land areas such as
the ocean floor.")

(subclass SlopedArea LandForm)
(documentation SlopedArea "A &%SlopedArea is a land surface which lies at an
angle to the horizontal so that some points on it are higher than others; a
slope.")

(=>
	(instance ?incline SlopedArea)
	(exists (?top ?bottom ?height1 ?height2)
		(and
			(top ?top ?incline)
			(bottom ?bottom ?incline)
			(altitude ?top ?height1)
			(altitude ?bottom ?height2)
			(successorAttributeClosure ?height2 ?height1) ; needed?
			(greaterThan ?height1 ?height2))))

(subclass Soil Mixture)
(documentation Soil "&%Soil is a substance composed of fine rock material
disintegrated by geological processes, mixed with humus, the organic remains
of decomposed vegetation.")

(=>
	(instance ?Soil Soil)
	(exists (?Humus ?Mineral)
		(and
			(instance ?Humus Humus)
			(instance ?Mineral Mineral)
			(part ?Humus ?Soil)
			(part ?Mineral ?Soil))))

(subclass Humus Mixture)
(documentation Humus "&%Humus is decaying organic matter found in &%Soil and derived
from dead animal and plant material.")

(=>
	(instance ?Humus Humus)
	(exists (?Soil)
		(and
			(instance ?Soil Soil)
			(part ?Humus ?Soil))))

(subclass Clay Soil)
(documentation Clay "Fine-grained soil consisting of mineral particles, not
necessarily clay minerals, that are less than 0.002 mm in their maximum dimension.")

(=>
	(and
		(part ?Particle ?Soil)
		(instance ?Soil Clay)
		(diameter ?Particle (MeasureFn ?Size Centimeter)))
	(greaterThan 0.0002 ?Size))

(subclass Sand Soil)
(documentation Sand "&%Sand is loose fragments of minerals or rocks. Smaller than gravel and larger than silt and clay, sand particles range from 8/10,000 to 8/100 inch (0.02 to 2 millimeters) in diameter. &%Sand is formed by the &%Erosion of rocks through the action of water, ice, or air.")

(<=>
   (instance ?CONTINENT Continent)
   (or
      (equal Africa ?CONTINENT)
      (equal NorthAmerica ?CONTINENT)
      (equal SouthAmerica ?CONTINENT)
      (equal Antarctica ?CONTINENT)
      (equal Europe ?CONTINENT)
      (equal Asia ?CONTINENT)
      (equal Oceania ?CONTINENT)))

(instance Africa Continent)
(documentation Africa "From Mid-Level Ontology used for Ontolog.")

(instance NorthAmerica Continent)
(documentation NorthAmerica "From Mid-Level Ontology used for Ontolog.")

(instance SouthAmerica Continent)
(documentation SouthAmerica "From Mid-Level Ontology used for Ontolog.")

(instance Antarctica Continent)
(documentation Antarctica "From Mid-Level Ontology used for Ontolog.")

(instance Europe Continent)
(documentation Europe "From Mid-Level Ontology used for Ontolog.")

(instance Asia Continent)
(documentation Asia "From Mid-Level Ontology used for Ontolog.")

(instance Oceania Continent)
(documentation Oceania "From Mid-Level Ontology used for Ontolog.")

(instance ArcticRegion GeographicArea)
(documentation ArcticRegion "The &%ArcticRegion is the region of
&%PlanetEarth that lies north of the Arctic Circle (approximately
66 and one half degrees &%North latitude) or beyond the northern
treeline: 'The Arctic'. See also &%ArcticArea.")


;; ===================
;; Government Ontology
;; ===================

;; The following content is borrowed from the Government Ontology.

(instance dependentGeopoliticalArea AsymmetricRelation)
(instance dependentGeopoliticalArea TransitiveRelation)
(domain dependentGeopoliticalArea 1 GeopoliticalArea)
(domain dependentGeopoliticalArea 2 GeopoliticalArea)
(relatedInternalConcept dependentGeopoliticalArea primaryGeopoliticalSubdivision)
(documentation dependentGeopoliticalArea "(&%dependentGeopoliticalArea
?AREA1 ?AREA2) means that ?AREA1 is a geopolitical possession of the
&%GeopoliticalArea ?AREA2 and is not a &%geopoliticalSubdivision of
?AREA2.  For example, (&%dependentGeopoliticalArea &%Guam &%UnitedStates),
because Guam is a territory of the &%UnitedStates, not one of the fifty
U.S. states.  Contrast &%primaryGeopoliticalSubdivision.")

(=>
	(dependentGeopoliticalArea ?AREA ?COUNTRY)
	(possesses ?COUNTRY ?AREA))

(=>
  	(dependentGeopoliticalArea ?AREA ?COUNTRY)
  	(not
    		(geopoliticalSubdivision ?AREA ?COUNTRY)))

(subclass NationalGovernment Government)
(documentation NationalGovernment "&%NationalGovernment is the class of
national-level governments of &%Nations.")

(instance governmentType BinaryPredicate)
(domain governmentType 1 Agent)
(domain governmentType 2 FormOfGovernment)
(subrelation governmentType attribute)
(documentation governmentType "(&%governmentType ?BODY ?FORM) means that
the &%GeopoliticalArea or &%Organization ?BODY has a government with
characteristic(s) of the type ?FORM.")

(=>
	(governmentType ?AGENT ?TYPE)
	(or
		(instance ?AGENT Organization)
		(instance ?AGENT GeopoliticalArea)))

(=>
	(and
		(instance ?AREA GeopoliticalArea)
		(governmentType ?AREA ?TYPE))
	(attribute (GovernmentFn ?AREA) ?TYPE))

(=>
	(and
		(attribute (GovernmentFn ?AREA) ?TYPE)
		(instance ?TYPE FormOfGovernment))
	(governmentType ?AREA ?TYPE))

(subclass FormOfGovernment PoliticoEconomicAttribute)
(documentation FormOfGovernment "&%FormOfGovernment is a class of
&%Attributes used to describe the characteristics of a government,
especially a &%NationalGovernment.  The concept &%FormOfGovernment is
interpreted broadly enough to include &%Anarchy and &%Factionalism.")

(instance Monarchy FormOfGovernment)
(contraryAttribute Monarchy Republic)
(contraryAttribute Monarchy Federation)
(documentation Monarchy "&%Monarchy is the attribute of a government
that is ruled by a monarch, which is usually a hereditary role.")

(=>
	(and
		(governmentType ?PLACE ?TYPE)
		(subAttribute ?TYPE Monarchy))
	(exists (?PERSON ?ROLE)
		(and
			(instance ?PERSON Human)
			(chiefOfState ?PLACE ?ROLE ?PERSON)
			(subAttribute ?ROLE Monarch))))

(=>
	(and
		(governmentType ?PLACE AbsoluteMonarchy)
		(instance ?PLACE GeopoliticalArea))
	(leaderPosition ?PLACE Monarch))

(instance HereditaryMonarchy FormOfGovernment)
(subAttribute HereditaryMonarchy Monarchy)
(documentation HereditaryMonarchy "A monarchy in which the position of monarch is inherited.")

(instance AbsoluteMonarchy FormOfGovernment)
(subAttribute AbsoluteMonarchy Monarchy)
(subAttribute AbsoluteMonarchy AuthoritarianRegime)
(documentation AbsoluteMonarchy "A monarchy in which the supreme power is held by the monarch, in contrast to ConstitutionalMonarchy, where the monarch's power is limited.")

(instance Chiefdom FormOfGovernment)
(subAttribute Chiefdom Monarchy)
(documentation Chiefdom "A monarchy in which the monarch is a Chief.")

(instance ConstitutionalMonarchy FormOfGovernment)
(subAttribute ConstitutionalMonarchy Monarchy)
(subAttribute ConstitutionalMonarchy ConstitutionalGovernment)
(documentation ConstitutionalMonarchy "A monarchy in which the power of the monarch is limited by a constitution, written or unwritten. The United Kingdom is an example.")

(instance Republic FormOfGovernment)
(documentation Republic "&%Republic is the attribute of a government
whose power and authority are vested in its members, who elect
representatives to exercise that power.")

(instance FederalRepublic FormOfGovernment)
(subAttribute FederalRepublic Republic)
(subAttribute FederalRepublic FederalGovernment)
(documentation FederalRepublic "From Mid-Level Ontology used in Ontolog.")

(instance FederalDemocraticRepublic FormOfGovernment)
(subAttribute FederalDemocraticRepublic Republic)
(subAttribute FederalDemocraticRepublic FederalGovernment)
(subAttribute FederalDemocraticRepublic Democracy)
(documentation FederalDemocraticRepublic "From Mid-Level Ontology used in Ontolog.")

(instance ParliamentaryGovernment FormOfGovernment)
(documentation ParliamentaryGovernment "&%ParliamentaryGovernment is the
attribute of a government whose chief &%LegislativeOrganization is a
&%Parliament.  A parliamentary government is compatible with various
other government types, including &%Monarchy.")

(=>
	(and
		(governmentType ?PLACE ?TYPE)
		(instance ?PLACE GeopoliticalArea)
		(subAttribute ?TYPE ParliamentaryGovernment))
	(exists (?ORG)
		(and
			(instance ?ORG Parliament)
			(subOrganizations ?ORG (GovernmentFn ?PLACE)))))

(instance ParliamentaryRepublic FormOfGovernment)
(subAttribute ParliamentaryRepublic Republic)
(subAttribute ParliamentaryRepublic ParliamentaryGovernment)
(documentation ParliamentaryRepublic "From Mid-Level Ontology used in Ontolog.")

(instance ParliamentaryDemocracy FormOfGovernment)
(subAttribute ParliamentaryDemocracy ParliamentaryGovernment)
(subAttribute ParliamentaryDemocracy Democracy)
(documentation ParliamentaryDemocracy "From Mid-Level Ontology used in Ontolog.")

(instance ParliamentaryDemocraticRepublic FormOfGovernment)
(subAttribute ParliamentaryDemocraticRepublic Republic)
(subAttribute ParliamentaryDemocraticRepublic Democracy)
(documentation ParliamentaryDemocraticRepublic "From Mid-Level Ontology used in Ontolog.")

(instance FederalParliamentaryDemocracy FormOfGovernment)
(subAttribute FederalParliamentaryDemocracy ParliamentaryGovernment)
(subAttribute FederalParliamentaryDemocracy FederalGovernment)
(subAttribute FederalParliamentaryDemocracy Democracy)
(documentation FederalParliamentaryDemocracy "From Mid-Level Ontology used in Ontolog.")

(instance PresidentialGovernment FormOfGovernment)
(documentation PresidentialGovernment "From Mid-Level Ontology used for Ontolog.")

(instance ConstitutionalDemocracy FormOfGovernment)
(subAttribute ConstitutionalDemocracy ConstitutionalGovernment)
(documentation ConstitutionalDemocracy "From Mid-Level Ontology used for Ontolog.")

(instance ConstitutionalGovernment FormOfGovernment)
(documentation ConstitutionalGovernment "&%ConstitutionalGovernment is
the attribute of a government whose authority and rule are guided by
principles expressed in a written &%Constitution.")

(instance ConstitutionalRepublic FormOfGovernment)
(subAttribute ConstitutionalRepublic Republic)
(subAttribute ConstitutionalRepublic ConstitutionalGovernment)
(documentation ConstitutionalRepublic "From Mid-Level Ontology used for Ontolog.")

(instance ConstitutionalParliamentaryDemocracy FormOfGovernment)
(subAttribute ConstitutionalParliamentaryDemocracy ParliamentaryGovernment)
(subAttribute ConstitutionalParliamentaryDemocracy Democracy)
(subAttribute ConstitutionalParliamentaryDemocracy ConstitutionalDemocracy)
(documentation ConstitutionalParliamentaryDemocracy "From Mid-Level Ontology used for Ontolog.")

(instance ConstitutionalDemocraticRepublic FormOfGovernment)
(subAttribute ConstitutionalDemocraticRepublic Democracy)
(subAttribute ConstitutionalDemocraticRepublic Republic)
(documentation ConstitutionalDemocraticRepublic "From Mid-Level Ontology used for Ontolog.")

(instance FederalGovernment FormOfGovernment)
(contraryAttribute FederalGovernment UnitaryRule)
(contraryAttribute FederalGovernment AuthoritarianRegime)
(documentation FederalGovernment "&%FederalGovernment is the attribute of
a government that is formed by agreement between a collection of political
units that agree to give up some of their power to the central government,
while reserving some powers to themselves.  The government of the
&%UnitedStates is a federal government, in which power is shared between
the states and the central goverment, as set out in the U.S.
Constitution.")

(instance Federation FormOfGovernment)
(subAttribute Federation FederalGovernment)
(documentation Federation "From Mid-Level Ontology used for Ontolog.")

(instance Commonwealth FormOfGovernment)
(documentation Commonwealth "From Mid-Level Ontology used for Ontolog.")

(instance Democracy FormOfGovernment)
(documentation Democracy "&%Democracy is the attribute of a government whose authority and rule are based in the will of the people governed.  The will of the people is usually expressed through &%Elections, direct or indirect.")

(instance MultipartyDemocracy FormOfGovernment)
(subAttribute MultipartyDemocracy Democracy)
(documentation MultipartyDemocracy "From Mid-Level Ontology used for Ontolog.")

(instance TransitionalGovernment FormOfGovernment)
(documentation TransitionalGovernment "&%TransitionalGovernment is the
attribute of a government that is changing from one form of government
to another.  This may be accompanied by social unrest or instability.")

(instance EmergingDemocracy FormOfGovernment)
(subAttribute EmergingDemocracy Democracy)
(subAttribute EmergingDemocracy TransitionalGovernment)
(documentation EmergingDemocracy "From Mid-Level Ontology used for Ontolog.")

(instance Factionalism FormOfGovernment)
(subAttribute Factionalism TransitionalGovernment)
(documentation Factionalism "From Mid-Level Ontology used for Ontolog.")

(instance Anarchy FormOfGovernment)
(documentation Anarchy "From Mid-Level Ontology used for Ontolog.")

(instance AuthoritarianRegime FormOfGovernment)
(contraryAttribute AuthoritarianRegime MultipartyDemocracy)
(documentation AuthoritarianRegime "&%AuthoritarianRegime is the
attribute of a government that rules autocratically, not allowing
opposition.")

(instance Dictatorship FormOfGovernment)
(subAttribute Dictatorship AuthoritarianRegime)
(documentation Dictatorship "From Mid-Level Ontology used for Ontolog.")

(instance MilitaryDictatorship FormOfGovernment)
(subAttribute MilitaryDictatorship Dictatorship)
(documentation MilitaryDictatorship "From Mid-Level Ontology used for Ontolog.")

(=>
	(governmentType ?PLACE MilitaryDictatorship)
	(leaderPosition ?PLACE MilitaryCommander))

(instance CommunistState FormOfGovernment)
(subAttribute CommunistState AuthoritarianRegime)
(documentation CommunistState "From Mid-Level Ontology used for Ontolog.")

(instance AuthoritarianSocialist FormOfGovernment)
(subAttribute AuthoritarianSocialist AuthoritarianRegime)
(documentation AuthoritarianSocialist "From Mid-Level Ontology used for Ontolog.")

(instance TheocraticGovernment FormOfGovernment)
(subAttribute TheocraticGovernment AuthoritarianRegime)
(documentation TheocraticGovernment "&%TheocraticGovernment is the
attribute of a government that bases its authority on &%Religion.")

(=>
	(and
		(governmentType ?AGENT ?TYPE)
		(subAttribute ?TYPE TheocraticGovernment)
		(instance ?AGENT Organization))
	(instance ?AGENT ReligiousOrganization))

(=>
	(and
		(governmentType ?AGENT ?TYPE)
		(subAttribute ?TYPE TheocraticGovernment)
		(instance ?AGENT GeopoliticalArea))
	(instance (GovernmentFn ?AGENT) ReligiousOrganization))

(instance TheocraticRepublic FormOfGovernment)
(subAttribute TheocraticRepublic TheocraticGovernment)
(documentation TheocraticRepublic "From Mid-Level Ontology used for Ontolog.")

(instance EcclesiasticalGovernment FormOfGovernment)
(subAttribute EcclesiasticalGovernment TheocraticGovernment)
(documentation EcclesiasticalGovernment "From Mid-Level Ontology used for Ontolog.")

(instance IslamicGovernment FormOfGovernment)
(subAttribute IslamicGovernment TheocraticGovernment)
(documentation IslamicGovernment "From Mid-Level Ontology used for Ontolog.")

(instance CompactOfFreeAssociationWithUnitedStates FormOfGovernment)
(documentation CompactOfFreeAssociationWithUnitedStates "From Mid-Level Ontology used for Ontolog.")

(instance CompactOfFreeAssociationWithNewZealand FormOfGovernment)
(documentation CompactOfFreeAssociationWithNewZealand "From Mid-Level Ontology used for Ontolog.")

(instance UnitaryRule FormOfGovernment)
(documentation UnitaryRule "&%UnitaryRule is a &%FormOfGovernment in which
the central government controls affairs at all levels, including the local
level.")

(instance capitalCity BinaryPredicate)
(domain capitalCity 1 City)
(domain capitalCity 2 GeopoliticalArea)
(subrelation capitalCity administrativeCenter)
(documentation capitalCity "(&%capitalCity ?CITY ?REGION) means that the
&%City ?CITY is the capital of the &%GeopoliticalArea ?REGION.")

(instance administrativeCenter BinaryPredicate)
(domain administrativeCenter 1 GeopoliticalArea)
(domain administrativeCenter 2 GeopoliticalArea)
(subrelation administrativeCenter geopoliticalSubdivision)
(documentation administrativeCenter "(&%administrativeCenter ?CENTER
?REGION) means that ?CENTER is the &%City (or other area) from which
the larger &%GeopoliticalArea ?REGION is administered.")

(instance primaryGeopoliticalSubdivision BinaryPredicate)
(instance primaryGeopoliticalSubdivision AsymmetricRelation)
(domain primaryGeopoliticalSubdivision 1 GeopoliticalArea)
(domain primaryGeopoliticalSubdivision 2 GeopoliticalArea)
(subrelation primaryGeopoliticalSubdivision geopoliticalSubdivision)
(documentation primaryGeopoliticalSubdivision "(&%primaryGeopoliticalSubdivision ?AREA ?COUNTRY) means that the
&%GeopoliticalArea ?AREA is one of the first-order administrative
divisions of the &%Nation ?COUNTRY.  For example, in the United States,
any of the fifty states.  This does not include subordinate regions that
have a lesser status, such as British Crown colonies, U.S. territories,
or protectorates.  See &%geopoliticalSubdivision.")

(subclass Holiday TimeInterval)
(documentation Holiday "&%Holiday is the class of time periods that are
observed as holidays in a country, culture, or religion.  Holidays may
recur annually on the same date, or they may be moveable, for example,
Thanksgiving Day falls on the last &%Thursday of each &%November.")

(subclass FixedHoliday Holiday)
(documentation FixedHoliday "&%FixedHoliday is the class of &%Holidays
whose observance is fixed to recurrences of the calendar day that the
holiday commemorates.")

(subclass MoveableHoliday Holiday)
(documentation MoveableHoliday "&%MoveableHoliday is the class of
&%Holidays whose observance is not fixed to recurrences of any particular
calendar day.  For example, Memorial Day is observed on the last &%Monday
of &%May.")

(instance ExecutiveBranchFn UnaryFunction)
(domain ExecutiveBranchFn 1 Agent)
(range ExecutiveBranchFn Organization)
(documentation ExecutiveBranchFn "(&%ExecutiveBranchFn ?ORG) denotes the
executive branch of ?ORG, with all its officials and agencies, considered
as a whole.")

(instance leaderPosition BinaryPredicate)
(instance leaderPosition AsymmetricRelation)
(domain leaderPosition 1 Agent)
(domain leaderPosition 2 Position)
(documentation leaderPosition "(&%leaderPosition ?ORG ?ROLE)
means that in the organization ?ORG, the leader is the person
who holds the &%Position ?ROLE in the organization.")

(instance representativeAgentToAgent TernaryPredicate)
(domain representativeAgentToAgent 1 Agent)
(domain representativeAgentToAgent 2 Agent)
(domain representativeAgentToAgent 3 Agent)
(documentation representativeAgentToAgent "(&%representativeAgentToAgent
?SENDER ?REP ?RECEIVER) means that the &%Agent ?SENDER has the &%Agent
?REP as its representative to the &%Agent ?RECEIVER.  ?REP works for ?SENDER
and is not assumed to be an impartial mediator.")

(=>
	(and
		(representativeAgentToAgent ?SENDER ?REP ?RECEIVER)
		(instance ?SENDER Organization)
		(instance ?REP CognitiveAgent))
	(employs ?SENDER ?REP))

(=>
	(and
		(representativeAgentToAgent ?SENDER ?REP ?RECEIVER)
		(instance ?SENDER Nation)
		(instance ?REP CognitiveAgent))
	(employs (GovernmentFn ?SENDER) ?REP))

;; <assertion>

(instance President Position)
(:hasValueClass President isUniqueFor Nation)
(documentation President "From Mid-Level Ontology used for Ontolog.")

;; </assertion>

(instance PrimeMinister Position)
(documentation PrimeMinister "From Mid-Level Ontology used for Ontolog.")

(instance VicePresident Position)
(documentation VicePresident "From Mid-Level Ontology used for Ontolog.")

(instance GovernmentDeputy Position)
(documentation GovernmentDeputy "From Mid-Level Ontology used for Ontolog.")

(instance Chairman Position)
(documentation Chairman "From Mid-Level Ontology used for Ontolog.")

(instance ViceChairman Position)
(documentation ViceChairman "From Mid-Level Ontology used for Ontolog.")

(instance MilitaryCommander Position)
(documentation MilitaryCommander "From Mid-Level Ontology used for Ontolog.")

(instance Monarch Position)
(documentation Monarch "From Mid-Level Ontology used for Ontolog.")

(instance Queen Position)
(subAttribute Queen Monarch)
(documentation Queen "From Mid-Level Ontology used for Ontolog.")

(instance King Position)
(subAttribute King Monarch)
(documentation King "From Mid-Level Ontology used for Ontolog.")

(instance chiefOfState TernaryPredicate)
(domain chiefOfState 1 GeopoliticalArea)
(domain chiefOfState 2 Position)
(domain chiefOfState 3 Human)
(documentation chiefOfState "(&%chiefOfState ?POLITY ?ROLE ?PERSON) means
that ?PERSON is the titular leader of the government of the
&%GeopoliticalArea ?POLITY and represents it at official functions.  The
office held by this chief of state is ?ROLE (e.g., President, Queen,
Chairman).  Note: this term is defined as in the CIA World Fact Book.")

(=>
	(chiefOfState ?AREA ?POSITION ?PERSON)
	(occupiesPosition ?PERSON ?POSITION (GovernmentFn ?AREA)))

(=>
	(and
		(chiefOfState ?AREA ?POSITION ?PERSON)
		(instance ?AREA Nation))
	(citizen ?PERSON ?AREA))

(instance headOfGovernment TernaryPredicate)
(domain headOfGovernment 1 GeopoliticalArea)
(domain headOfGovernment 2 Position)
(domain headOfGovernment 3 Human)
(documentation headOfGovernment "(&%headOfGovernment ?POLITY ?ROLE
?PERSON) means that ?PERSON is the top administrative leader of the
&%Government of the &%GeopoliticalArea ?POLITY, with authority for
managing its day-to-day functions.  The office held by this person
is the &%Position ?ROLE (e.g., President, Prime Minister, Governor).
Note: this term is defined as in the CIA World Fact Book.")

(=>
	(and
		(headOfGovernment ?AREA ?POSITION ?PERSON)
		(instance ?AREA Nation))
	(citizen ?PERSON ?AREA))

(=>
	(headOfGovernment ?AREA ?POSITION ?PERSON)
	(occupiesPosition ?PERSON ?POSITION (GovernmentFn ?AREA)))

(instance electionForPosition BinaryPredicate)
(domain electionForPosition 1 Election)
(domain electionForPosition 2 SocialRole)
(documentation electionForPosition "(&%electionForPosition ?ELECTION
?POSITION) means that in the &%Election ?ELECTION, candidates run for
election to the role(s) ?POSITION.")

(=>
	(electionForPosition ?ELECTION ?ROLE)
	(exists (?CANDIDATE)
		(and
			(instance ?CANDIDATE Human)
			(candidateForPosition ?ELECTION ?ROLE ?CANDIDATE))))

(instance candidateForPosition TernaryPredicate)
(domain candidateForPosition 1 Election)
(domain candidateForPosition 2 SocialRole)
(domain candidateForPosition 3 Agent)
(documentation candidateForPosition "(&%candidateForPosition ?ELECTION
?POSITION ?CONTENDER) means that in the &%Election ?ELECTION for
?POSITION, the &%Agent ?CONTENDER was one of the candidates.")

(=>
	(candidateForPosition ?ELECTION ?POSITION ?CONTENDER)
	(electionForPosition ?ELECTION ?POSITION))

(=>
	(and
		(candidateForPosition ?ELECTION ?POSITION ?CONTENDER)
		(agent ?ELECTION ?AGENT))
	(desires ?CONTENDER (occupiesPosition ?CONTENDER ?POSITION ?AGENT)))

(instance voteFractionReceived QuaternaryPredicate)
(domain voteFractionReceived 1 Election)
(domain voteFractionReceived 2 SocialRole)
(domain voteFractionReceived 3 Agent)
(domain voteFractionReceived 4 RealNumber)
(documentation voteFractionReceived "(&%voteFractionReceived ?ELECTION
?POSITION ?CONTENDER ?FRACTION) means that in the &%Election ?ELECTION for
?POSITION, the &%Agent ?CONTENDER received ?FRACTION of the votes cast.
Contenders may be either persons or political parties.")

(=>
	(voteFractionReceived ?ELECTION ?POSITION ?CONTENDER ?FRACTION)
	(candidateForPosition ?ELECTION ?POSITION ?CONTENDER))

(instance electionWinner TernaryPredicate)
(domain electionWinner 1 Election)
(domain electionWinner 2 SocialRole)
(domain electionWinner 3 Agent)
(subrelation electionWinner candidateForPosition)
(documentation electionWinner "(&%electionWinner ?ELECTION ?POSITION
?CONTENDER) means that in the &%Election ?ELECTION, ?POSITION was won by
the &%Agent ?CONTENDER.  Contenders may be either persons or political
parties.")

(=>
   (and
       (instance ?ELECTION PopularElection)
       (electionWinner ?ELECTION ?POSITION ?PERSON1)
       (voteFractionReceived ?ELECTION ?POSITION ?PERSON1 ?NUMBER1))
   (not
       (exists (?PERSON2 ?NUMBER2)
            (and
               (voteFractionReceived ?ELECTION ?POSITION ?PERSON2 ?NUMBER2)
               (not (equal ?PERSON1 ?PERSON2))
               (greaterThanOrEqualTo ?NUMBER2 ?NUMBER1)))))

(=>
   (and
       (instance ?ELECTION PopularElection)
       (electionWinner ?ELECTION ?POSITION ?PERSON1)
       (voteFractionReceived ?ELECTION ?POSITION ?PERSON1 ?NUMBER1)
       (voteFractionReceived ?ELECTION ?POSITION ?PERSON2 ?NUMBER2)
       (not (equal ?PERSON1 ?PERSON2)))
   (greaterThan ?NUMBER1 ?NUMBER2))

(subclass LegislativeOrganization Organization)
(documentation LegislativeOrganization "&%LegislativeOrganization is the
class of &%Organizations that have as their main purpose the passing of
laws or regulations.")

(=>
	(and
		(instance ?ORG LegislativeOrganization)
		(subOrganization ?ORG ?GOV)
		(instance ?GOV GovernmentOrganization))
	(instance ?ORG GovernmentOrganization))

(subclass Parliament LegislativeOrganization)
(documentation Parliament "&%Parliament is the subclass of
&%LegislativeOrganizations similar to that of the United Kingdom.")

(instance LegislatureFn UnaryFunction)
(domain LegislatureFn 1 GeopoliticalArea)
(range LegislatureFn LegislativeOrganization)
(documentation LegislatureFn "(&%LegislatureFn ?AREA) denotes the
legislative branch of the &%GeopoliticalArea ?AREA.")

(instance JudiciaryFn UnaryFunction)
(domain JudiciaryFn 1 GeopoliticalArea)
(range JudiciaryFn GovernmentOrganization)
(range JudiciaryFn JudicialOrganization)
(documentation JudiciaryFn "(&%JudiciaryFn ?AREA) denotes the judicial
branch of the &%GeopoliticalArea ?AREA, that is, the
&%JudicialOrganization(s) associated with the government of ?AREA,
considered as a whole.")

(subclass PoliticalParty PoliticalOrganization)
(documentation PoliticalParty "&%PoliticalParty is the class of
&%PoliticalOrganizations that may sponsor candidates for &%Elections.")

(=>
   (instance ?PARTY PoliticalParty)
   (hasPurpose ?PARTY (exists (?MEMBER ?GOVERNMENT)
                         (and
                            (instance ?GOVERNMENT Government)
                            (member ?MEMBER ?GOVERNMENT)
                            (member ?MEMBER ?PARTY)))))

(=>
	(and
		(occupiesPosition ?PERSON ?POSITION ?ORGANIZATION)
		(instance ?ORGANIZATION PoliticalParty))
	(member ?PERSON ?ORGANIZATION))

(instance MilitaryGeneral MilitaryOfficer)
(documentation MilitaryGeneral "&%MilitaryGeneral is a generic &%Position
that indicates holding (or having held) the rank of General in some
military force.  An indicator that someone uses the title, without
committing to his or her exact rank or military affiliation.")

;; =======================
;; Transportation Ontology
;; =======================

;; The following content is borrowed from the Transportation Ontology.

(subclass Airport TransitTerminal)
(subclass Airport LandTransitway)
(documentation Airport "&%Airport is the subclass of &%TransitTerminals
for &%Airplanes (fixed-wing &%Aircraft).")

(=>
   (instance ?PORT Airport)
   (exists (?RUNWAY)
      (and
         (instance ?RUNWAY Runway)
         (part ?RUNWAY ?PORT))))

(subclass Runway LandTransitway)
(documentation Runway "&%Runway is the class of &%Transitways that are
used for the takeoff and landing of &%Airplanes.  &%Runways are
&%Transitways for an intermodal transit, which begins with a land transit
and ends with air transit, or vice versa.")

(subclass Pipeline Transitway)
(documentation Pipeline "&%Pipeline is the class of pipelines used
to transport various kinds of fluids.")

(=>
	(and
		(instance ?PIPE Pipeline)
		(instance ?MOTION Motion)
		(instrument ?MOTION ?PIPE)
		(patient ?MOTION ?STUFF))
	(instance ?STUFF (ExtensionFn Fluid)))

(subclass Waterway Transitway)
(subclass Waterway WaterArea)
(documentation Waterway "&%Waterway is the class of navigable waters,
including oceans, seaLanes, rivers, canals, lakes, and inland bodies
of water.")

(subclass Canal Waterway)
(subclass Canal StationaryArtifact)
(documentation Canal "&%Canal is the subclass of &%Waterways that are
&%Artifacts constructed for the passage of &%Ships.")

(subclass SurfacedRoadway Roadway)
(disjoint SurfacedRoadway UnsurfacedRoadway)
(documentation SurfacedRoadway "&%SurfacedRoadway is the subclass of
&%Roadways that have been improved by covering them with a substance
to increase the hardness and smoothness of the surface.  Covering
materials include pavement, concrete, asphalt, macadam, and gravel.")

(subclass Expressway SurfacedRoadway)
(documentation Expressway "&%Expressway is the subclass of
&%SurfacedRoadways that are multiple-lane, limited-access highways
designed for rapid travel by &%MotorVehicles.")

(subclass UnsurfacedRoadway Roadway)
(documentation UnsurfacedRoadway "&%UnsurfacedRoadway is the subclass
of &%Roadways that have natural, unimproved surfaces of dirt or sand.")

(subclass Railway LandTransitway)
(subclass Railway StationaryArtifact)
(documentation Railway "&%Railway is the subclass of
&%LandTransitways that have rails along which &%Trains may travel.
A railway consists of the rail bed, sleepers, tracks, electric
rails, switches, sensors, lights, crossing grades, and any other
integral machinery or parts of a section of railway.")

(subclass Bridge LandTransitway)
(subclass Bridge StationaryArtifact)
(documentation Bridge "&%Bridge is the subclass of &%LandTransitways
that are artifacts used for crossing water or air-filled gaps that
could not be transited over a natural surface.")

(subclass Tunnel LandTransitway)
(subclass Tunnel StationaryArtifact)
(documentation Tunnel "&%Tunnel is a subclass of &%Transitways that
consist of a lengthwise enclosed &%Hole that allows for transit
underground, as through mountains, below a body of water, or beneath a
city.")

(subclass RailroadTrack StationaryArtifact)
(documentation RailroadTrack "&%RailroadTrack is the class of
&%StationaryArtifacts consisting of rails laid on supports to form
a track for railway vehicles.")

(subclass LandVehicle Vehicle)
(documentation LandVehicle "&%LandVehicle is the class of &%Vehicles that
travel on land.  The two main types of &%LandVehicle are &%RoadVehicle and
&%RailVehicle.")

(subclass RoadVehicle LandVehicle)
(documentation RoadVehicle "A LandVehicle which is designed primarily to travel on roads, in contrast to land vehicles like tanks that can travel across country.")

(subclass Automobile RoadVehicle)
(documentation Automobile "A &%RoadVehicle with an internal combustion engine.")

(subclass Truck RoadVehicle)
(documentation Truck "An &%RoadVehicle whose primary purpose is the &%Transportation of entities other than &%Humans.")

(subclass Trailer RoadVehicle)
(documentation Trailer "An unpowered vehicle moved by attaching to the rear of a powered vehicle.")

(subclass UserPoweredDevice Device)
(documentation UserPoweredDevice "A device which moves by application of the muscle power of the user.")

(subclass Cycle LandVehicle)
(subclass Cycle UserPoweredDevice)
(documentation Cycle "A type of land vehicle with wheels ppowered by the user.")

(subclass Bicycle Cycle)
(documentation Bicycle "A type of Cycle having two wheels.")

(subclass Motorcycle RoadVehicle)
(documentation Motorcycle "A powered vehicle similar to a Cycle, but with a motor.")

(subclass RailVehicle LandVehicle)
(documentation RailVehicle "A &%LandVehicle that runs along fixed rails.")

(subclass Wagon LandVehicle)
(documentation Wagon "A &%Landcraft that is not self-propelled, but must be
pulled by either an &%Animal or a self-propelled &%Vehicle to move along the
ground.")

(=>
   (and
      (instance ?WAGON Wagon)
      (instance ?TRANSPORT Transportation)
      (instrument ?TRANSPORT ?WAGON))
   (exists (?POWER)
      (and
         (instance ?TRANSPORT Pulling)
         (agent ?PULL ?POWER)
         (patient ?PULL ?WAGON)
         (or
            (instance ?POWER DomesticAnimal)
            (instance ?POWER Vehicle)))))

(subclass Train RailVehicle)
(documentation Train "&%Train is the subclass of &%Vehicle whose instances
are linked sequences of &%Wagons.")

(=>
   (instance ?TRAIN Train)
   (exists (?WAGON)
      (and
         (instance ?WAGON Wagon)
         (part ?WAGON ?TRAIN))))

(subclass Watercraft Vehicle)
(documentation Watercraft "&%Watercraft is the class of all &%Vehicles
used to travel on or in water.")

(=>
	(and
		(instance ?CRAFT Watercraft)
		(instance ?EVENT Transportation)
		(instrument ?EVENT ?CRAFT))
	(exists (?WATER)
		(and
			(instance ?WATER WaterArea)
			(located ?EVENT ?WATER))))

(=>
	(and
		(subclass ?TYPE Watercraft)
		(instance ?EVENT WaterTransportation)
		(located ?EVENT ?PLACE))
	(instance ?PLACE WaterArea))

(subclass CubicFoot VolumeMeasure)
(instance CubicFoot UnitOfMeasure)
(documentation CubicFoot "&%CubicFoot is a unit for measuring volume,
equal to a volume of one foot length in each dimension of length, width,
and height.")

(equal (MeasureFn 1 CubitFoot)
	 (MultiplicationFn (MeasureFn 1 Foot)
				 (MultiplicationFn (MeasureFn 1 Foot) (MeasureFn 1 Foot))))

(subclass Aircraft Vehicle)
(partition Aircraft FixedWingAircraft Helicopter)
(documentation Aircraft "Any &%Vehicle which is capable of
&%AirTransportation.  Note that this class covers both fixed-wing aircraft
and helicopters.")

(subclass FixedWingAircraft Aircraft)
(documentation FixedWingAircraft "A heavier-than-air aricraft whose wings do not rotate in order to provide lift.")

(subclass Airplane Aircraft)
(documentation Airplane "&%Airplane is the subclass of &%Aircraft that
are fixed-wing aircraft which carry their own power sources.  &%Airplane
includes jet airplanes and propeller planes, but not gliders.")

(subclass TransitTerminal StationaryArtifact)
(documentation TransitTerminal "A &%TransitTerminal is a place where
travellers or transportation devices begin or end their journeys, or
where passengers and/or goods may be transferred.  At a terminal,
&%Vehicles may be received, assigned, sent out, or
stored.")

(subclass TrainStation TransitTerminal)
(documentation TrainStation "A TransitTerminal where trains may stop to pick up or discharge passengers or cargo.")

(subclass TerminalBuilding Building)
(documentation TerminalBuilding "A &%TerminalBuilding is a &%Building
located at a &%TransitTerminal and used in connection with its
functions.")

(subclass CommonCarrier TransportationCompany)
(disjoint CommonCarrier ContractCarrier)
(documentation CommonCarrier "&%CommonCarrier is the subclass of
&%TransportationCompany whose instances must offer services to all
customers.  Contrast with &%ContractCarrier.")

(subclass ContractCarrier TransportationCompany)
(documentation ContractCarrier "&%ContractCarrier is the subclass of
&%TransportationCompany whose instances offer services to only one
customer, under contract.  Contrast with &%CommonCarrier.")


;; =======================
;; Communications Ontology
;; =======================

;; The following content is borrowed from the Communications Ontology.

(subclass CommunicationDevice EngineeringComponent)
(relatedInternalConcept CommunicationDevice Communicating)
(documentation CommunicationDevice "A &%CommunicationDevice is a &%Device
which serves at the &%instrument in a &%Communication &%Process by allowing
the communicated message to be conveyed between the participants.")

(=>
	(instance ?DEVICE CommunicationDevice)
	(capability Communicating instrument ?DEVICE))

(=>
   (instance ?DEVICE CommunicationDevice)
   (hasPurpose ?DEVICE (exists (?COMMUNICATION)
                           (and
                               (instance ?COMMUNICATION Communicating)
                               (instrument ?COMMUNICATION ?DEVICE)))))

(subclass Satellite Object)
(partition Satellite NaturalSatellite ArtificialSatellite)
(documentation Satellite "&%Satellite is the collection of bodies that
revolve around some  astronomical body, e.g., planets around a star.")

(subclass ArtificialSatellite Satellite)
(subclass ArtificialSatellite Device)
(documentation ArtificialSatellite "An &%ArtificialSatellite is a &%Device
that orbits the earth in space and performs various functions such as
aiding in communication, photographing the earth's surface, and others.  A Satellite that is an artifact; a solid object created by humans orbiting an astronomical body.")

(subclass BroadcastingStation StationaryArtifact)
(partition BroadcastingStation RadioStation TelevisionStation)
(documentation BroadcastingStation "A &%BroadcastingStation is
a &%TelevisionStation or a &%RadioStation.")

(subclass RadioStation BroadcastingStation)
(documentation RadioStation "A &%RadioStation is a &%BroadcastingStation
that broadcasts programs that are intended to be received by &%Radios.")

(subclass Radio CommunicationDevice)
(subclass Radio ElectricDevice)
(documentation Radio "A &%Radio is a &%Device for receiving radio
broadcast signals from a &%RadioStation.")

(subclass TelevisionStation BroadcastingStation)
(documentation TelevisionStation "A &%TelevisionStation is a
&%BroadcastingStation that broadcasts programs that are intended to be
received by &%Television.")

(subclass Television CommunicationDevice)
(subclass Television ElectricDevice)
(documentation Television "A &%Television is a &%Device for receiving
television broadcast signals from a &%TelevisionStation.")


;; ================
;; Economy Ontology
;; ================

;; The following content is borrowed from the Economy Ontology.


(instance economyType BinaryPredicate)
(domain economyType 1 Agent)
(domain economyType 2 EconomicAttribute)
(subrelation economyType attribute)
(documentation economyType "(&%economyType ?POLITY ?TYPE) means that the
&%GeopoliticalArea ?POLITY has an economic system of &%TYPE.")

(=>
	(economyType ?AGENT ?ATTRIBUTE)
	(or
		(instance ?AGENT GeopoliticalArea)
		(instance ?AGENT Organization)))

(subclass EconomicAttribute PoliticoEconomicAttribute)
(documentation EconomicAttribute "&%EconomicAttribute is the class
of terms including all &%Attributes used to characterize the
economic systems or development levels of &%Nations or dependent
&%GeopoliticalAreas.")

(subclass EconomicSystemAttribute EconomicAttribute)
(documentation EconomicSystemAttribute "&%EconomicSystemAttribute
is the class of &%Attributes that describe the type of economic
system that a country or area has.  For example, &%CapitalistEconomy
or &%SocialistEconomy.")

(instance CapitalistEconomy EconomicSystemAttribute)
(subAttribute CapitalistEconomy PrivateEnterpriseEconomy)
(documentation CapitalistEconomy "&%CapitalistEconomy is the
&%Attribute used to characterize a country whose economy is based
on private ownership of the means of production and distribution,
and on private accumulation of capital.")

(instance PureCapitalistEconomy EconomicSystemAttribute)
(subAttribute PureCapitalistEconomy CapitalistEconomy)
(contraryAttribute PureCapitalistEconomy MixedEconomy)
(documentation PureCapitalistEconomy "&%PureCapitalistEconomy is an
&%Attribute representing a capitalist economy that has no admixture of
socialism.")

(instance PrivateEnterpriseEconomy EconomicSystemAttribute)
(subAttribute PrivateEnterpriseEconomy CapitalistEconomy)
(documentation PrivateEnterpriseEconomy "&%PrivateEnterpriseEconomy is
the &%Attribute used to characterize a country in which private
enterprise is the main source of economic wealth.")

(instance MarketEconomy EconomicSystemAttribute)
(documentation MarketEconomy "&%MarketEconomy is an &%Attribute that
describes an economy in which market forces, specifically supply and
demand, provide input for privately managed decisions about pricing
and production of goods.")

(instance SocialistEconomy EconomicSystemAttribute)
(documentation SocialistEconomy "&%SocialistEconomy is the &%Attribute
used to characterize a country in which there is government ownership
or direction of the means of production and distribution.")

(instance PureSocialistEconomy EconomicSystemAttribute)
(subAttribute PureSocialistEconomy SocialistEconomy)
(contraryAttribute PureSocialistEconomy MixedEconomy)
(contraryAttribute PureSocialistEconomy PureCapitalistEconomy)
(documentation PureSocialistEconomy "&%PureSocialistEconomy is an
&%Attribute representing a socialist economy that has no admixture of
capitalism.")

(instance DemocraticSocialism EconomicSystemAttribute)
(subAttribute DemocraticSocialism SocialistEconomy)
(documentation DemocraticSocialism "&%DemocraticSocialism is an
&%Attribute that describes a country in which socialism is promoted
by a political party or parties within a democratic government.
Under &%DemocraticSocialism, the government participates in central
planning of the economy and may also manage nationalized industries.")

(=>
	(attribute ?AREA DemocraticSocialism)
	(governmentType ?AREA Democracy))

(instance MarketSocialism EconomicSystemAttribute)
(subAttribute MarketSocialism PartialMarketEconomy)
(documentation MarketSocialism "An attribute of an economic system.")

(instance CommunalLandOwnershipEconomy EconomicSystemAttribute)
(subAttribute CommunalLandOwnershipEconomy SocialistEconomy)
(documentation CommunalLandOwnershipEconomy "An attribute of an economic system.")

(instance MixedEconomy EconomicSystemAttribute)
(documentation MixedEconomy "&%MixedEconomy is the &%Attribute
of a country whose economy has elements of more than one pure
economic system, e.g., a market economy with government welfare
for unemployed workers.  A mixed-economy country may be a
country in transition, as from a prior communist economy to
capitalism, but a mixed economy may also be a stable combination
of different economic approaches in different areas of a national
economy, e.g., nationally managed health care and education systems
in an otherwise private-enterprise economy.")

(instance PartialMarketEconomy EconomicSystemAttribute)
(subAttribute PartialMarketEconomy MixedEconomy)
(documentation PartialMarketEconomy "An attribute of an economic system.")

(instance GovernmentRegulatedEconomy EconomicSystemAttribute)
(documentation GovernmentRegulatedEconomy "&%GovernmentRegulatedEconomy
is an &%Attribute that describes the economy of a country in which the
government determines prices, production, wages, allocation of resources,
or other economic factors.  An economy that is wholly government planned
is a &%CentrallyPlannedEconomy.")

(instance CentrallyPlannedEconomy EconomicSystemAttribute)
(subAttribute CentrallyPlannedEconomy GovernmentRegulatedEconomy)
(documentation CentrallyPlannedEconomy "&%CentrallyPlannedEconomy
is a term used mainly to describe communist or formerly communist
states, many of which are now evolving away from command economies
towards market-oriented systems.  Also known as a 'command economy'.")

(=>
	(and
		(attribute ?AREA CommunistState)
		(instance ?AREA Nation))
	(economyType ?AREA CentrallyPlannedEconomy))

(instance PrivatizingEconomy EconomicSystemAttribute)
(subAttribute PrivatizingEconomy MixedEconomy)
(documentation PrivatizingEconomy "&%PrivatizingEconomy is an
&%Attribute that describes a country in which formerly government-
owned industries are being transferred into private holdings.")

(instance NationalizedIndustryEconomy EconomicSystemAttribute)
(subAttribute NationalizedIndustryEconomy GovernmentRegulatedEconomy)
(documentation NationalizedIndustryEconomy "&%NationalizedIndustryEconomy
is an &%Attribute describing an economy in which the major industries,
such as energy and transportation, are owned by the national government.")

(instance GovernmentSubsidizedEconomy EconomicSystemAttribute)
(documentation GovernmentSubsidizedEconomy "&%GovernmentSubsidizedEconomy
is an &%Attribute describing an economy in which the government provides
subsidies to various industries, workers, or other groups as part of its
economic policy.")

(instance WelfareCapitalism EconomicSystemAttribute)
(subAttribute WelfareCapitalism MixedEconomy)
(subAttribute WelfareCapitalism GovernmentSubsidizedEconomy)
(documentation WelfareCapitalism "&%WelfareCapitalism is an &%Attribute
describing an economy in which the government provides economic subsidies
to unemployed or disabled individuals.")

(subclass IndustryAttribute Attribute)
(documentation IndustryAttribute "The economic sector, in terms of good or services produced.  Used in industryOfArea relation.")

(instance industryOfArea BinaryPredicate)
(domain industryOfArea 1 GeopoliticalArea)
(domain industryOfArea 2 IndustryAttribute)
(documentation industryOfArea "(&%industryOfArea ?AREA ?SECTOR)
means that the &%GeopoliticalArea ?AREA produces goods or services
in the economic area ?SECTOR.")

(instance industryProductType BinaryPredicate)
(domain industryProductType 1 IndustryAttribute)
(domainSubclass industryProductType 2 Object)
(documentation industryProductType
"(&%industryProductType ?INDUSTRY ?TYPE) means that organizations with
the &%IndustryAttribute ?INDUSTRY produce products of the kind ?TYPE.")

(=>
	(and
		(instance ?ORG Organization)
		(attribute ?ORG ?INDUSTRY)
		(industryProductType ?INDUSTRY ?TYPE))
	(exists (?EVENT ?ITEM)
		(and
			(instance ?EVENT Making)
			(instance ?ITEM ?TYPE)
			(agent ?EVENT ?ORG)
			(result ?EVENT ?ITEM))))

(subclass KilowattHour FunctionQuantity)
(instance KilowattHour UnitOfMeasure)
(documentation KilowattHour "&%KilowattHour is a &%UnitOfMeasure for
energy that represents 1000 &%Watts (1 kW) of power expended over one
hour (1 h) of time.  This is the unit commonly used in commercial
power contexts.  It is equivalent to 3,600,000 &%Joules.")

(<=>
	(equal ?AMOUNT (MeasureFn ?NUMBER KilowattHour))
	(equal ?AMOUNT (MeasureFn (MultiplicationFn 3600000 ?NUMBER) Joule)))

(<=>
	(equal ?AMOUNT (MeasureFn ?NUMBER Joule))
	(equal ?AMOUNT (MeasureFn (MultiplicationFn 0.0000002778 ?NUMBER) Kilowatt)))

(subclass Exporting FinancialTransaction)
(documentation Exporting "&%Exporting is the class of actions in
which there is a &%ChangeOfPossession of goods shipped from a
provider in one &%Nation to a destination in another &%Nation.
Typically, there are &%Selling and &%Buying events associated
with an &%Exporting.  Either the seller or the exporting country
may be considered the &%origin of &%Exporting.")

(=>
	(instance ?EXPORT Exporting)
	(exists (?ITEM)
		(and
			(instance ?ITEM Object)
			(patient ?EXPORT ?ITEM))))

(=>
	(and
		(instance ?EXPORT Exporting)
		(patient ?EXPORT ?ITEM)
		(instance ?AREA GeopoliticalArea)
		(origin ?EXPORT ?AREA))
	(holdsDuring (BeginFn (WhenFn ?EXPORT))
		(located ?ITEM ?AREA)))

(=>
	(and
		(instance ?EXPORT Exporting)
		(patient ?EXPORT ?ITEM)
		(instance ?AREA GeopoliticalArea)
		(holdsDuring (BeginFn (WhenFn ?EXPORT))
			(located ?ITEM ?AREA)))
	(holdsDuring (EndFn (WhenFn ?EXPORT))
			(not
				(located ?ITEM ?AREA))))

(=>
	(and
		(instance ?EXPORT Exporting)
		(patient ?EXPORT ?ITEM)
		(instance ?AREA GeopoliticalArea)
		(holdsDuring (BeginFn (WhenFn ?EXPORT))
			(located ?ITEM ?AREA)))
	(exists (?AREA2)
		(and
			(instance ?AREA2 GeopoliticalArea)
			(not (geopoliticalSubdivision ?AREA1 ?AREA2))
			(not (geopoliticalSubdivision ?AREA2 ?AREA1))
			(holdsDuring (EndFn (WhenFn ?EXPORT))
				(located ?ITEM ?AREA2)))))

(=>
	(and
		(instance ?EXPORT Exporting)
		(patient ?EXPORT ?ITEM)
		(instance ?AREA GeopoliticalArea)
		(destination ?EXPORT ?AREA))
	(holdsDuring (EndFn (WhenFn ?EXPORT))
		(located ?ITEM ?AREA)))

(=>
	(and
		(instance ?EXPORT Exporting)
		(patient ?EXPORT ?ITEM)
		(instance ?AREA1 GeopoliticalArea)
		(instance ?AREA2 GeopoliticalArea)
		(holdsDuring (BeginFn (WhenFn ?EXPORT))
			(located ?ITEM ?AREA1))
		(holdsDuring (EndFn (WhenFn ?EXPORT))
			(located ?ITEM ?AREA2)))
	(not (located ?AREA2 ?AREA1)))

(=>
	(and
		(instance ?EXPORT Exporting)
		(patient ?EXPORT ?ITEM)
		(instance ?AREA1 GeopoliticalArea)
		(origin ?EXPORT ?AREA1)
		(instance ?AREA2 GeopoliticalArea)
		(destination ?EXPORT ?AREA2))
	(not (equal ?AREA1 ?AREA2)))

(=>
	(and
		(instance ?EXPORT Exporting)
		(patient ?EXPORT ?ITEM)
		(instance ?AREA1 GeopoliticalArea)
		(origin ?EXPORT ?AREA1)
		(instance ?AREA2 GeopoliticalArea)
		(destination ?EXPORT ?AREA2))
	(not (located ?AREA2 ?AREA1)))

(instance currencyType BinaryPredicate)
(domain currencyType 1 GeopoliticalArea)
(domain currencyType 2 UnitOfMeasure)
(domainSubclass currencyType 2 CurrencyMeasure)
(documentation currencyType "(&%currencyType ?AREA ?UNIT) means
that the official currency used in the &%GeopoliticalArea ?AREA
is the &%UnitOfMeasure ?UNIT.")

(instance fiscalYearPeriod BinaryPredicate)
(domain fiscalYearPeriod 1 Agent)
(domainSubclass fiscalYearPeriod 2 TimeInterval)
(documentation fiscalYearPeriod "The predicate &%fiscalYearPeriod
indicates the period that an &%Agent or &%Organization uses as its
12-month accounting period.  (&%fiscalYearPeriod ?AGENT &%Year) means
that ?AGENT observes its 12-month accounting period during the
regular calendar year (CY), from &%January to &%December.  For
fiscal years with other beginning and ending months (FYs), use
(&%fiscalYearPeriod ?AGENT (&%RecurrentTimeIntervalFn ?STARTMONTH ?ENDMONTH)).
For example, (&%fiscalYearPeriod (&%GovernmentFn &%UnitedStates)
(&%RecurrentTimeIntervalFn &%October &%September)).  For FYs that begin
or end mid-month, days may be specified within &%RecurrentTimeIntervalFn.")

(subclass Narcotic ControlledSubstance)
(documentation Narcotic "&%Narcotic is a subclass of addictive
&%BiologicallyActiveSubstances that have damping effects on the
nervous system and may be fatal in large doses.")

(subclass ControlledSubstance BiologicallyActiveSubstance)
(documentation ControlledSubstance "&%ControlledSubstance is
the subclass of &%BiologicallyActiveSubstances whose distribution
and use is controlled by government regulation.")

(subclass Fodder Food)
(documentation Fodder "&%Fodder is the subclass of &%Food that is intended
for instances of &%DomesticAnimal.")

(subclass Cement Mixture)
(documentation Cement "&%Cement is a subclass of &%Mixture
whose instances may contain various minerals or ores, prepared by
heating and pulverizing, and used in binding &%Concrete or in laying
brick or stone.")

(subclass Concrete Mixture)
(documentation Concrete "&%Concrete is a subclass of &%Mixture
used as building materials.  Concrete is made up of &%Mineral pieces
(sand or gravel) and a &%Cement material used to bind them together.")

(=>
	(instance ?CONCRETE Concrete)
	(exists (?PART)
		(and
			(instance ?PART Mineral)
			(component ?PART ?CONCRETE))))

(=>
	(instance ?CONCRETE Concrete)
	(exists (?PART)
		(and
			(instance ?PART Cement)
			(component ?PART ?CONCRETE))))

(=>
   (instance ?CONCRETE Concrete)
   (hasPurpose ?CONCRETE (exists (?CONSTRUCT)
                            (and
                               (instance ?CONSTRUCT Constructing)
                               (instrument ?CONSTRUCT ?CONCRETE)))))


;; ===============
;; People Ontology
;; ===============

;; The following content is borrowed from the People Ontology.

(instance PopulationFn UnaryFunction)
(domain PopulationFn 1 GeopoliticalArea)
(range PopulationFn Integer)
(documentation PopulationFn "(&%PopulationFn ?AREA) denotes the &%Integer
that represents the count of the number of people inhabiting the
&%GeopoliticalArea ?AREA.  The total population presents one overall
measure of the potential impact of the country on the world and within its
region.")

(equal (PopulationFn ?AREA)
	 (CardinalityFn
		(KappaFn ?PERSON
			(and
				(instance ?PERSON Human)
				(inhabits ?PERSON ?AREA)))))

(instance average BinaryPredicate)
(instance average PartialValuedRelation)
(instance average SingleValuedRelation)
(domain average 1 List)
(domain average 2 RealNumber)
(documentation average "A partial function that relates a &%List to a
&%RealNumber, provided that the &%List only has list elements that are
&%RealNumbers. The &%RealNumber associated with the &%List is equal to the
mathematical average of the &%RealNumbers in the &%List divided by the total
number of list elements.")

;; The &%List in the domain 1 of &%average may only have list elements that
;; are &%RealNumbers.
(=>
	(average ?LIST ?AVERAGE)
        (forall (?LISTITEM)
		(=>
			(inList ?LISTITEM ?LIST)
			(instance ?LISTITEM RealNumber))))

;; Calculation of the average of the elements of the list involves the
;; stipulation of a second list described as a running total of the elements
;; in the original list. The average of the first list is equal to the final
;; element in the running total divided by the number of list elements.
(<=>
	(average ?LIST1 ?AVERAGE)
	(exists (?LIST2)
		(and
			(equal (ListLengthFn ?LIST2) (ListLengthFn ?LIST1))
			(equal (ListOrderFn ?LIST2 1) (ListOrderFn ?LIST1 1))
			(forall (?ITEMFROM2)
				(=>
					(inList ?ITEMFROM2 ?LIST2)
					(exists (?POSITION ?POSITIONMINUSONE ?ITEMFROM1 ?PRIORFROM2)
						(and
							(greaterThan ?POSITION 1)
							(lessThanOrEqualTo ?POSITION (ListLengthFn ?LIST2))
							(equal (ListOrderFn ?LIST2 ?ITEMFROM2) ?POSITION)
							(inList ?ITEMFROM1 ?LIST1)
							(equal ?POSITION (ListOrderFn ?LIST1 ?ITEMFROM1))
							(inList ?PRIORFROM2 ?LIST2)
							(equal ?POSITIONMINUSONE (SubtractionFn ?POSITION 1))
							(equal ?POSITIONMINUSONE (ListOrderFn ?LIST2 ?PRIORFROM2))
							(equal ?ITEMFROM2 (AdditionFn ?ITEMFROM1 ?PRIORFROM2))))))
			(equal ?LASTPLACE (ListLengthFn ?LIST2))
			(equal ?AVERAGE (DivisionFn (ListOrderFn ?LIST2 ?LASTPLACE) ?LASTPLACE)))))

(subclass RacialEthnicGroup EthnicGroup)
(documentation RacialEthnicGroup "A &%RacialEthnicGroup is an
&%EthnicGroup based on common racial background.")

(subclass DeafSignLanguage ManualHumanLanguage)
(documentation DeafSignLanguage "A &%DeafSignLanguage is a
&%ManualHumanLanguage primarily intended for communication between a
deaf individual and a hearing individual or between deaf individuals.")

(subclass CreoleLanguage SpokenHumanLanguage)
(documentation CreoleLanguage "A &%CreoleLanguage is a &%PidginLanguage that
has developed and become the mother tongue for a community of people. This
process is called 'creolization' and results in an expanded vocabulary and
grammar structure that allow for communication as rich and complex as that of
non-creole languages. While pidgins are regarded as reduced languages, creoles
are considered expanded languages. That is, while pidgins develop to enable
communication in relatively isolated domains, creoles allow for a full range
of expressive possibilities on a par with more 'recognized' languages.")

(subclass PidginLanguage SpokenHumanLanguage)
(documentation PidginLanguage "A &%PidginLanguage is not the native language
of anyone but is used as an auxiliary or supplemental language between two
mutually unintelligible speech communities. Pidgins are reduced languages,
characterized by having a limited vocabulary and a simple grammar which serve
to satisfy basic communication needs. Historically these languages have
primarily arisen in trade centers and plantations (with slaves from different
language backgrounds), areas where large groups of people lacking a common
language need to communicate. By definition, a pidgin has no native speakers;
it is always a person's second (or more) language.")

(subclass MixedLanguage SpokenHumanLanguage)
(documentation MixedLanguage "A &%MixedLanguage is a &%SpokenHumanLanguage
that combines grammar and lexical items from two or more languages to create a new language that is essentially a linguistic mixture.")

(subclass LiteracyAttribute TraitAttribute)
(documentation LiteracyAttribute "If an ?INDIVIDUAL has the &%attribute
&%LiteracyAttribute, that ?INDIVIDUAL is able to read and write.")

(<=>
	(attribute ?INDIVIDUAL LiteracyAttribute)
	(and
		(hasSkill Reading ?INDIVIDUAL)
		(hasSkill Writing ?INDIVIDUAL)))

;; <assertion>
;; Real-world assertions about planets

(instance EarthsMoon Moon)
(orbits EarthsMoon PlanetEarth)
(documentation EarthsMoon "The natural moon that orbits planet Earth.")

(instance PlanetEarth Planet)
(orbits PlanetEarth Sol)
(documentation PlanetEarth "The third planet from the sun in the solar system, where humans evolved.")

(instance PlanetMercury Planet)
(orbits PlanetMercury Sol)
(documentation PlanetMercury "The planet clolsest to the sun in the solar system.")

(instance PlanetVenus Planet)
(orbits PlanetVenus Sol)
(documentation PlanetVenus "The second planet from the sun in the solar system.")

(instance PlanetMars Planet)
(orbits PlanetMars Sol)
(documentation PlanetMars "The fourth planet from the sun in the solar system.")

(instance PlanetJupiter Planet)
(orbits PlanetJupiter Sol)
(documentation PlanetJupiter "The fifth planet from the sun in the solar system, the largest planet in the solar system.")

(instance PlanetSaturn Planet)
(orbits PlanetSaturn Sol)
(documentation PlanetSaturn "The sixth planet from the sun in the solar system, with pretty rings around it.")

(instance PlanetNeptune Planet)
(orbits PlanetNeptune Sol)
(documentation PlanetNeptune "The seventh planet from the sun in the solar system.")

(instance PlanetUranus Planet)
(orbits PlanetUranus Sol)
(documentation PlanetUranus "The eighth planet from the sun in the solar system.")

(instance PlanetPluto Planet)
(orbits PlanetPluto Sol)
(documentation PlanetPluto "The ninth planet from the sun in the solar system -- far out!")


;; Real-world assertions about ethnicity and religion

(instance BlackEthnicity RacialEthnicGroup)
(documentation BlackEthnicity "A broad racial division encompassing
various African, African-American, and Caribbean peoples.")

(instance WhiteEthnicity EthnicGroup)
(documentation WhiteEthnicity "A broad ethnic division encompassing
various European, Hispanic, and Middle Eastern peoples. Also known as
Caucasian.")

(instance AmerindianEthnicity EthnicGroup)
(documentation AmerindianEthnicity "A broad ethnic group encompassing any
of the North, Central, or South American tribal peoples.")

(instance Judaism BeliefGroup)
(documentation Judaism "The religion developed among the ancient Hebrews and
characterized by belief in one transcendent God who has revealed Himself to
Abraham, Moses, and the Hebrew prophets and by a religious life in accordance
with Scriptures and rabbinic traditions.")

(instance Christianity BeliefGroup)
(documentation Christianity "The religion derived from Jesus Christ, based on the Bible as sacred scripture, and professed by Eastern, Catholic, and
Protestant bodies, among other subdivisions.")

(instance Protestantism BeliefGroup)
(subCollection Protestantism Christianity)
(documentation Protestantism "&%Protestantism is one of the three major
divisions of &%Christianity.")

(instance RomanCatholicism BeliefGroup)
(subCollection RomanCatholicism Christianity)
(documentation RomanCatholicism "&%RomanCathoicism is one of the three major divisions of &%Christianity.")

(instance RomanCatholicChurch ReligiousOrganization)
(documentation RomanCatholicChurch "The &%RomanCatholicChurch is the
&%ReligiousOrganization that promulgates &%RomanCatholicism.")

;; fields of study

(instance Mathematics FieldOfStudy)
(documentation Mathematics "The &%FieldOfStudy dealing with quantities and their relations to one another.")

(instance Economics SocialScience)
(documentation Economics "The field of economics.")

(instance Linguistics SocialScience)
(documentation Linguistics "The field of linguistics.")

(instance Psychology SocialScience)
(documentation Psychology "The field of psychology.")

(instance Biology Science)
(documentation Biology "The study of the classification, development, and
functioning of &%Organisms.")

(instance Physiology Science)
(subField Physiology Biology)
(documentation Physiology "The part of &%Biology dealing with the functioning of &%Organisms.")

(instance MedicalScience Science)
(subField MedicalScience Biology)
(documentation MedicalScience "The field of medicine.")

(instance Chemistry Science)
(documentation Chemistry "The study of the compositions, properties, and
reactions of &%Substances.")

(instance Physics Science)
(documentation Physics "The study of matter and energy and their relations.")

(instance Engineering Science)
(documentation Engineering "The application of instances of &%Science to the solution
of practical problems, i.e. the creation of various forms of technology.")

(instance Electronics Science)
(subField Electronics Physics)
(subField Electronics Engineering)
(documentation Electronics "The branch of &%Physics that deals with the theory and applications of electron emissions.")

(instance Theology FieldOfStudy)
(documentation Theology "The systematic study of religious practice and religious truth.")

(instance MilitaryScience FieldOfStudy)
(documentation MilitaryScience "The study of the principles of war.")

(instance History FieldOfStudy)
(documentation History "The recording and interpretation of past events involving
&%Humans, including political events and cultural practices.")

(instance Philosophy FieldOfStudy)
(documentation Philosophy "The study of first principles, including epistemology,
metaphysics, and ethics.")

(instance FieldOfLaw FieldOfStudy)
(documentation FieldOfLaw "The study of legal principles and the framework of national and/or international laws.")

;; geography

(instance RedRiver River)
(part RedRiver UnitedStates)
(documentation RedRiver "A tributary of the Mississippi River.")

(instance Mexico Nation)
(meetsSpatially Mexico UnitedStates)
(documentation Mexico "A large Spanish-speaking country that borders on
the &%UnitedStates.")

(instance Laos Nation)
(documentation Laos "A &%Nation in southeastern Asia.")

(instance VirginIslands Collection)
(documentation VirginIslands "A &%Collection of &%Islands in the
West Indies that are dependencies of the &%UnitedStates and the
&%UnitedKingdomOfGreatBritainAndNorthernIreland.")

(=>
   (member ?ISLAND VirginIslands)
   (instance ?ISLAND Island))

(instance Guam Island)
(documentation Guam "An &%Island in the &%PacificOcean that is a
protectorate of the &%UnitedStates.")

(instance Cuba Nation)
(instance Cuba Island)
(documentation Cuba "An &%Island &%Nation in the Carribbean.")

(instance UnitedStates Nation)
(documentation UnitedStates "The North American republic of 50 states.")

(instance Alaska AmericanState)
(documentation Alaska "The largest state in the &%UnitedStates.")

(=>
   (and
      (subclass ?UNIT AreaMeasure)
      (measure Alaska (MeasureFn ?NUMBER1 ?UNIT))
      (measure ?STATE (MeasureFn ?NUMBER2 ?UNIT))
      (instance ?STATE AmericanState)
      (not (equal Alaska ?STATE)))
   (lessThan ?NUMBER2 ?NUMBER1))

(instance California AmericanState)
(documentation California "The &%AmericanState with the highest population.")

(=>
   (and
      (instance ?STATE AmericanState)
      (not (equal ?STATE California)))
   (greaterThan (ResidentFn California) (ResidentFn ?STATE)))

(instance NewYorkState AmericanState)
(documentation NewYorkState "A populous state in the northeastern &%UnitedStates.")

(instance Pennsylvania AmericanState)
(documentation Pennsylvania "A mid-Atlantic &%AmericanState.  Its two major cities
are Philadelphia and Pittsburgh.")

(instance Texas AmericanState)
(documentation Texas "The second largest &%AmericanState, located in the southwest
on the Gulf of Mexico.")

(instance Virginia AmericanState)
(meetsSpatially Virginia WashingtonDC)
(documentation Virginia "A state in the southeastern &%UnitedStates
that borders on &%WashingtonDC.")

(instance Georgia AmericanState)
(documentation Georgia "A state in the southeastern &%UnitedStates.")

(instance NewEngland GeographicArea)
(part NewEngland UnitedStates)
(documentation NewEngland "A &%GeographicArea in the &%UnitedStates that is
made up of the states of Maine, New Hampshire, Vermont, Massachusetts, Rhode Island,
and Connecticut.")

(instance NewYorkCity AmericanCity)
(part NewYorkCity NewYorkState)
(documentation NewYorkCity "The largest city in the &%UnitedStates.
A worldwide center of finance and culture, it is comprised of five boroughs.")

(=>
   (instance ?CITY AmericanCity)
   (lessThanOrEqualTo (CardinalityFn (ResidentFn ?CITY)) (CardinalityFn (ResidentFn NewYorkCity))))

(instance WashingtonDC AmericanCity)
(documentation WashingtonDC "The capital city of the &%UnitedStates.")

(instance Chicago AmericanCity)
(documentation Chicago "Often referred to as the second city, Chicago is
the largest city in the midwestern United States.")

(instance Dallas AmericanCity)
(documentation Dallas "A large &%City in northeastern Texas.")

(instance KansasCityMissouri AmericanCity)
(documentation KansasCityMissouri "A large &%City at the western edge of Missouri.")

(instance Boston AmericanCity)
(documentation Boston "The largest &%City in Massachusetts.")

(instance LosAngeles AmericanCity)
(part LosAngeles California)
(documentation LosAngeles "The largest &%City in &%California.")

(=>
   (and
      (instance ?CITY AmericanCity)
      (part ?CITY California)
      (not (equal ?CITY LosAngeles)))
   (greaterThan (ResidentFn LosAngeles) (ResidentFn ?CITY)))

(instance SanFrancisco AmericanCity)
(part SanFrancisco California)
(documentation SanFrancisco "A large &%City in &%California, located on the
San Francisco Bay.")

(instance ManchesterNewHampshire AmericanCity)
(documentation ManchesterNewHampshire "The largest &%City in New Hampshire.")

(instance PuertoRico Island)
(part PuertoRico UnitedStates)
(documentation PuertoRico "An autonomous part of the &%UnitedStates.")

(instance Europe Continent)
(documentation Europe "The second smallest &%Continent.")

(instance Paris EuropeanCity)
(part Paris France)
(documentation Paris "The capital of &%France and the largest &%City of the country.")

(=>
   (and
      (instance ?CITY City)
      (part ?CITY France))
   (lessThanOrEqualTo (CardinalityFn (ResidentFn ?CITY)) (CardinalityFn (ResidentFn Paris))))

(instance France Nation)
(documentation France "A large, industrialized European &%Nation.")

(instance UnitedKingdomOfGreatBritainAndNorthernIreland Nation)
(documentation UnitedKingdomOfGreatBritainAndNorthernIreland "The &%Nation comprising
England, Scotland, Wales, and Northern Ireland.")

(instance London City)
(part London UnitedKingdomOfGreatBritainAndNorthernIreland)
(documentation London "The capital city and the largest city of the
&%UnitedKingdomOfGreatBritainAndNorthernIreland.")

(instance Ireland Nation)
(documentation Ireland "An independent &%Nation that borders on the Irish Sea.")

(instance SovietUnion Nation)
(documentation SovietUnion "The former communist nation of the Soviet Union.")

(instance Russia Nation)
(documentation Russia "A &%Nation which is currently independent, but which
was once part of the &%SovietUnion.")

(instance Greece Nation)
(part Greece Europe)
(documentation Greece "A small Balkan &%Nation that is known primarily for the
literature, philosophy, and art produced there during the ancient period.")

(instance Germany Nation)
(part Germany Europe)
(documentation Germany "A large European &%Nation.")

(instance Japan Nation)
(instance Japan Island)
(documentation Japan "A large industrialized Asian &%Nation.")

(instance UnitedStatesDepartmentOfState GovernmentOrganization)
(subOrganization UnitedStatesDepartmentOfState (GovernmentFn UnitedStates))
(documentation UnitedStatesDepartmentOfState "The &%subOrganization of the US
government that sets and enforces foreign policy.")

(instance UnitedStatesDepartmentOfInterior GovernmentOrganization)
(subOrganization UnitedStatesDepartmentOfInterior (GovernmentFn UnitedStates))
(documentation UnitedStatesDepartmentOfInterior "Manages and preserves public lands
and natural resources in the &%UnitedStates.")

(instance SecretaryOfTheInterior GovernmentSecretary)
(documentation SecretaryOfTheInterior "The head of the &%UnitedStatesDepartmentOfInterior.")

(=>
   (occupiesPosition ?PERSON SecretaryOfTheInterior UnitedStatesDepartmentOfInterior)
   (leader UnitedStatesDepartmentOfInterior ?PERSON))

(instance SecretaryOfTheTreasury GovernmentSecretary)
(documentation SecretaryOfTheTreasury "The head of the United States Treasury Department.")

(instance UnitedStatesCongress LegislativeOrganization)
(subOrganization UnitedStatesCongress (GovernmentFn UnitedStates))
(documentation UnitedStatesCongress "The legislative branch of the government
of the &%UnitedStates.")

;; </assertion>

;; END FILE


;; <module>Invoices</module>
;;  Part 3 of samin002.txt
;;  This module contains a set of propositions that were created as
;;  part of the  development of an Invoice ontology by the Ontolog
;;  study group.
;;  Started January 2004.
;;  This is only an initial set of classes, relations, and axioms
;;        for discussion.
;;  Last edit January 10, 2004
;;  First version created by Adam Pease
;;   this version has some modifications by Patrick Cassidy
;;  BEGIN FILE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     INVOICES              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; INCLUDES 'BASE ONTOLOGY'
;; INCLUDES 'STRUCTURALONTOLOGY'
;; INCLUDES 'NUMERIC FUNCTIONS'
;; INCLUDES 'SET/CLASS THEORY'
;; INCLUDES 'UNITS OF MEASURE'
;; INCLUDES 'TEMPORAL CONCEPTS'
;; INCLUDES 'MEREOTOPOLOGY'
;; INCLUDES 'PROCESSES'
;; INCLUDES 'OBJECTS'
;; INCLUDES 'QUALITIES'


;; PJC Jan 9, 2004
;; BEGIN Definitions added by PJC to those proposed by Adam Pease
;; *****************************************************

(subclass Tree Plant)
(documentation Tree "A plant having a permanently woody stem or trunk, usually developing branches at a distance from the ground.")

(subclass CommercialItem Product)
(documentation CommercialItem "An article of commerce sold as a unit, corresponding to the 'Item' class used in the UBL specification.  This
represents a 'package' for sale and may have any number of individual
manufactured objects in it, such as a pair of shoes, a bag of potatoes,
or a kit with several different types of items as components.")

;; note that the order of the arguments for the
;; hasName relation is the opposite of that for 'name'

(instance hasName BinaryRelation)
(domain hasName 1 Entity)
(domain hasName 2 SymbolicString)
(inverse hasName names)
(documentation hasName "hasName relates an instance of an entity to a string of linguistic characters used to reference the entity in linguistic communication.  This is the inverse of the SUMO relation 'names', added to allow more flexible representation in Protege.  The hasName relation is not a necessary relation since not every entity is named by a SymbolicString and not every SymbolicString is the label for an entity.")

(instance hasAbstractName BinaryRelation)
(domain hasAbstractName 1 Entity)
(domain hasAbstractName 2 AbstractString)
(inverse hasAbstractName isaNameOf)
(relatedInternalConcept hasAbstractName hasName)
(documentation hasAbstractName "hasAbstractName relates an instance of an entity to an abstract string of linguistic characters used to reference the entity in linguistic communication.  This is closely related to hasName, the value of which is a SymbolicString (a physical object)  as domain 2.  This is not the inverse of the SUMO relation 'names', since the SUMO relation has a physical object as domain 1.  The hasAbstractName relation is not a necessary relation since not every entity is named by a AbstractString and not every AbstractString is the label for an entity.")

(instance hasPostalCode BinaryRelation)
(domain hasPostalCode 1 Address)
(domain hasPostalCode 2 PostOfficeCode)
(documentation hasPostalCode "hasPostalCode is not exactly the inverse of relation postalCode because the code value is a string, not an integer.")

(subclass PostOfficeCode SymbolicString)
(documentation PostOfficeCode "A string of symbols which need not be exclusively numeric. A ZIP code was originally all numeric, but the extended ZIP+4 can have a hyphen in it.")

(subclass ZipCode PostOfficeCode)
(documentation ZipCode "A ZipCode is a US post office code, which may be five digits or in ZIP+4 format, nine digits in 5-4 format.")

(subclass Message Communication)
(:hasRestrictedVal Message hasIntendedAudience Addressees)
(documentation Message "Message is a Communication which is an AbstractText and for which the intended audience is an instance of Addressees, group corresponding to a specified list of CognitiveAgents and the distribution is restricted to those agents, i.e. it is not a broadcast or a publication which is intended to be read by anyone who chances upon it. There may be cases where an author publishes a book and writes it for understanding by a particular audience, but if its distribution is not restricted it is not a Message.")

(instance hasIntendedAudience BinaryRelation)
(domain hasIntendedAudience 1 Communication)
(domain hasIntendedAudience 2 AgentGroup)
(documentation hasIntendedAudience "hasIntendedAudience relates a Communication, which is an abstract Proposition, to the audience to whom the author of the Communication has directed it.  This is not necessarily identical to the group of agents that read or experience the communication.  The intended audiences of subclasses of Communication may be more restricted.  See class 'Message'.")

;;(instance hasRestrictedAudience BinaryRelation)
;;(instance hasRestrictedAudience TotalValuedRelation)
;;(subrelation hasRestrictedAudience hasIntendedAudience)
;;(domain hasRestrictedAudience 1 Message)
;;(domain hasRestrictedAudience 2 SpecifiedGroup)
;;(documentation hasRestrictedAudience "hasRestrictedAudience relates a Message 
;;with a restricted list of intended recipients to that list of recipients.")

(instance hasAuthors BinaryRelation)
(domain hasAuthors 1 Communication)
(domain hasAuthors 2 AgentGroup)
(documentation hasAuthors "hasAuthors relates a Communication to the Agent or Agents that created it.  This is not directly the inverse of relation 'authors' because that relation relates a physical text to its authors, however there is a direct implied relation from authors to hasAuthors.  The implication is not bidirectional, however, as the domain of hasAuthors includes the content of a speech, which is an event, whereas authors relates only physical objects to their originators.")

(=>
  (authors ?AGENT ?TEXT)
  (exists ?PROP
    (and
      (instance ?PROP Communication)
      (containsInformation ?TEXT ?PROP)
      (hasAuthors ?PROP ?AGENT))))


(subclass AgentGroup Group)
(subclass AgentGroup Agent)
(documentation AgentGroup "An AgentGroup is one or more agents considered for some purpose as a single Agent.")

(subclass SpecifiedGroup AgentGroup)
(documentation SpecifiedGroup "A SpecifiedGroup is one or more agents who are individually identified and form the elements of a list (whether actually recorded in some physical medium or merely implied by the circumstances of a message.)  For example, a phone call is a communication and the listener or recipient of each component message transmitted is the unique member of the SpecifiedGroup which is the intended recipeint of the message -- specified by the circumstances of the telephone call, not by any recorded list.")

;; for each SpecifiedGroup there is a list whose members have a one-to-one 
;; correspondence with the members of the group.

(=>
  (instance ?SPECGROUP SpecifiedGroup)
  (exists (?LIST)
    (and 
       (forall (?AGENT)
          (<=>
             (member ?AGENT ?SPECGROUP)
             (inList ?AGENT ?LIST))))))

(subclass Addressees SpecifiedGroup)
(documentation Addressees "Each instance of Addressees is a group of agents to whom a message is addressed.")

;; Added for samin006: authority, obligations, etc

(instance hasModalAttribute BinaryPredicate)
(instance hasModalAttribute AsymmetricRelation)
(instance hasModalAttribute IrreflexiveRelation)
(inverse hasModalAttribute isaModalAttributeOf)
(domain hasModalAttribute 1 Assertion)
(domain hasModalAttribute 2 NormativeAttribute)
(documentation hasModalAttribute "This is a relation analogous to the SUMO &%modalObligation, but applying to abstract assertions rather than physical Formulas as in SUMO.  A &%BinaryRelation that is used to state the normative force of a &%Proposition.  (&%hasModalAttribute ?Assertion ?PROP) means that the &%Proposition expressed by ?Assertion has the &%NormativeAttribute ?PROP.  For example, (&%hasModalAttribute (&%exists (?ACT ?OBJ) (&%and (&%instance ?ACT &%Giving) (&%agent ?ACT John) (&%patient ?ACT ?OBJ) (&%destination ?ACT Tom))) &%Obligatory) means that John is obligated to give Tom something.")

(=>
   (and
      (hasModalAttribute ?ASSERTION1 ?PROP)
      (entails ?ASSERTION1 ?ASSERTION2))
   (hasModalAttribute ?ASSERTION2 ?PROP))

(=>
   (hasModalAttribute ?ASSERTION Obligatory)
   (exists (?AGENT)
      (holdsObligation ?ASSERTION ?AGENT)))

(=>
   (hasModalAttribute ?ASSERTION Permission)
   (exists (?AGENT)
      (holdsRight ?ASSERTION ?AGENT)))

(=>
    (holdsRight ?ASSERTION ?AGENT)
    (hasModalAttribute ?ASSERTION Possibility))

(=>
    (holdsObligation ?ASSERTION ?AGENT)
    (hasModalAttribute ?ASSERTION Obligatory))


(<=>
   (hasModalAttribute ?ASSERTION Necessity)
   (not (hasModalAttribute (not ?ASSERTION) Possibility)))

(=>
   (hasModalAttribute ?ASSERTION Necessity)
   (hasModalAttribute ?ASSERTION Possibility))


(<=>
   (hasModalAttribute ?ASSERTION Obligatory)
   (not (hasModalAttribute (not ?ASSERTION) Permission)))

(=>
   (hasModalAttribute ?ASSERTION Obligatory)
   (hasModalAttribute ?ASSERTION Permission))



;; *****************************************************
;; End definitions added by PJC

;;  BEGIN Propositions and axioms suggested by Adam Pease.
;;  The order of the propositions has been changed from
;;  the original.
;   Modified Jan. 10, 2004
;; ********************************************

(subclass Invoice Statement)
(subclass Invoice DocumentPhysical)
(:hasRestrictedVal Invoice containsInformation InvoiceProposition)
(:hasRestrictedVal Invoice hasAbstractContent AbstractInvoice)
(documentation Invoice "A physical document which states that a buyer owes, or does not owe, money for goods or services purchased from a seller.")

(=>
  (instance ?I Invoice)
  (exists (?EV)
    (and
      (instance ?EV Selling)
      (refers ?I ?EV))))

(instance quantityInEvent TernaryRelation)
(domain quantityInEvent 1 Process)
(domain quantityInEvent 2 Integer)
(domainSubclass quantityInEvent 3 Object)
(documentation quantityInEvent "(quantityInEvent ?PROC ?QUANT ?OBJ)
means that the number of &%Objects that are instances of ?OBJ that
participate in an event ?PROC in the role of &%patient is ?QUANT.")

(=>
  (quantityInEvent ?P ?Q ?OBJ)
  (exists (?INST)
    (equal ?Q
      (CardinalityFn
        (KappaFn ?INST
          (and
            (instance ?INST ?OBJ)
            (patient ?P ?INST)))))))


(subclass Softwood Wood)
(documentation Softwood "Wood obtained from trees classified in the family of Softwoods or gymnosperms.  Examples include scrub pine, spruce and cedar.")

(subclass Hardwood Wood)
(disjoint Hardwood Softwood)
(documentation Hardwood "Wood obtained from trees classified in the family of Hardwoods, or angiosperms.  Examples include oak, birch and maple.")

(=>
  (and
    (instance ?H Hardwood)
    (instance ?S Softwood)
    (measure ?H
      (DensityFn ?MASSH ?VOLUME))
    (measure ?S
      (DensityFn ?MASSS ?VOLUME)))
  (greaterThan ?MASSH ?MASSS))


;; <assertion>
;; ------------------------------------------------------
;; Instance level content
;;  The following propositions describe a sample invoice,
;;  referring to a fictional transaction, for illustrative
;;  purposes

;; not yet formalized: Delivery Doc: DEL-03/55-712
;; not yet formalized: Your Order No: S03-034257
;; not yet formalized: Contact: Eva Brick

(instance JoineryInvoice-2003-00645 Invoice)
(hasName  JoineryInvoice-2003-00645 "IN 2003/00645")
(date JoineryInvoice-2003-00645 (DayFn 25 (MonthFn 2 (YearFn 2003))))
(refers JoineryInvoice-2003-00645 JoineryPurchase-2003-00645)
(documentation JoineryInvoice-2003-00645 "This is a sample invoice used for illustrative purposes in development of the Ontology Invoice ontology.  All agents and events are fictional.")

(instance JoineryPurchase-2003-00645 FinancialTransaction)
(date JoineryPurchase-2003-00645 (DayFn 3 (MonthFn 2 (YearFn 2003))))
(quantityInEvent JoineryPurchase-2003-00645 2 JoineryObject-236WV)
(documentation JoineryPurchase-2003-00645 "This is the selling event referred to in the sample invoice JoineryInvoice-2003-00645, used in development of the Ontology Invoice ontology.  This is a fictional event.")

(subclass JoineryObject-236WV Product)
(subclass JoineryObject-236WV Softwood)
(documentation JoineryObject-236WV "This is the class of objects, the sale of certain instances of which is referred to in the sample invoice JoineryInvoice-2003-00645, used in development of the Ontology Invoice ontology.")

;; The Axiom below states that every instance of the class of objects
;; JoineryObject-236WV has the value GBP 102.5
;; see PJC note 6 above for an alternative representation

(=>
  (instance ?X JoineryObject-236WV)
  (monetaryValue ?X (MeasureFn 102.50 BritishPound)))

(instance Buying-2003-00645 Buying)
(subProcess Buying-2003-00645 JoineryPurchase-2003-00645)
(agent Buying-2003-00645 JerryBuilderPLC)
(documentation Buying-2003-00645 "The buying aspect of the
commercial transaction JoineryPurchase-2003-00645; the
transaction from the perspective of the buyer JerryBuilderPLC.")

(instance Selling-2003-00645 Selling)
(subProcess Selling-2003-00645 JoineryPurchase-2003-00645)
(agent Selling-2003-00645 SpecialistWindowsPLC)
(documentation Selling-2003-00645 "The selling aspect of the commercial transaction JoineryPurchase-2003-00645; the
transaction from the perspective of the seller SpecialistWindowsPLC.")

(instance JerryBuilderPLC CognitiveAgent)
(hasName  JerryBuilderPLC "Jerry Builder plc")
(address JerryBuilderPLC MarshLane)
(foo JerryBuilderPLC MarshLane)
;; above line added to test relation check
(documentation JerryBuilderPLC "The buyer in the
commercial transaction JoineryPurchase-2003-00645.")

(instance SpecialistWindowsPLC CognitiveAgent)
(hasName  SpecialistWindowsPLC "Specialist Windows plc")
(address SpecialistWindowsPLC SnowhillWorks)
(located SpecialistWindowsPLC SnowhillWorks)
(documentation SpecialistWindowsPLC "The seller in the
commercial transaction JoineryPurchase-2003-00645.")

(instance MarshLane Address)
(hasName  MarshLane "Marsh Lane")
(located MarshLane NowhereNorfolk)
(hasPostalCode MarshLane "NR18 4XX")
(documentation MarshLane "The fictional address of the buyer JerryBuilderPLC in the fictional commercial transaction JoineryPurchase-2003-00645.")

(instance NowhereNorfolk City)
(located NowhereNorfolk NorfolkUK)
(documentation NowhereNorfolk "The fictional city where the buyer JerryBuilderPLC in the fictional commercial transaction JoineryPurchase-2003-00645 is located.")

(instance NorfolkUK StateOrProvince)
(located NorfolkUK UnitedKingdom)
(documentation NorfolkUK "The fictional state where the buyer JerryBuilderPLC in the fictional commercial transaction JoineryPurchase-2003-00645 is located.")

(instance SnowhillWorks Address)
(hasName  SnowhillWorks "Snowhill Works")
(located SnowhillWorks LittleSnoringWhereshire)
(hasPostalCode SnowhillWorks "SM2 3NW")
(documentation SnowhillWorks "The fictional address where the seller SpecialistWindowsPLC in the fictional commercial transaction JoineryPurchase-2003-00645 is located.")

(instance LittleSnoringWhereshire City)
(located LittleSnoringWhereshire WhereshireUK)
(documentation LittleSnoringWhereshire "The fictional city where the seller SpecialistWindowsPLC in the fictional commercial transaction JoineryPurchase-2003-00645 is located.")

(instance WhereshireUK StateOrProvince)
(located WhereshireUK UnitedKingdom)
(documentation WhereshireUK "The fictional state where the seller SpecialistWindowsPLC in the fictional commercial transaction JoineryPurchase-2003-00645 is located.")

;; </assertion>

;;  End Propositions and axioms suggested by Adam Pease
;; ********************************************


;;  <module>Core</module>
;;  Additional concepts required for representation of
;;  Code and CodeList as well as some other Core concepts

(instance SumFn VariableArityRelation)
(instance SumFn TotalValuedRelation)
(instance SumFn SingleValuedRelation)
(domain SumFn 1 RealNumber)
(domain SumFn 2 RealNumber)
(documentation SumFn "(SumFn @ROW) returns the total sum of all elements in the @ROW, each of which must be a real number.  This is left as a mostly primitive concept left for implementation by the computer's arithmetic processes.")

;; <axiomgloss>Every element of a row variable which is an argument the SumFn 
;; must be a real number

(=>
   (SumFn @ROW)
   (forall (?NUM)
      (=>
        (inList ?NUM (ListFn @ROW))
        (instance ?NUM RealNumber))))

(subclass EfficiencyMeasure Attribute)
;; (subclass EfficiencyMeasure Quantity)
(documentation EfficiencyMeasure "an attribute of a process or procedure which may be quantitative (as, ten units per hour), measuring the outputs per unit input.")

(instance mm-dd-yy DateFormat)
(documentation mm-dd-yy "mm-dd-yy is a format for dates consisting of two digits representing month of the year (1 to 12), followed by a hyphen, followed by two digits representing the day of the month (1 to 31), followed by a hyphen, followed by two digits representing the last two digits of the year. In this format, today's date is 03-02-04, i.e. March 2nd, 2004.")

(instance mm/dd/yy DateFormat)
(documentation mm/dd/yy "mm/dd/yy is a format for dates consisting of two digits representing month of the year (1 to 12), followed by a forward slash, followed by two digits representing the day of the month (1 to 31), followed by a forward slash, followed by two digits representing the last two digits of the year. In this format, today's date is 03/02/04, i.e. March 2nd, 2004.")

(instance dd.Mon.yyyy DateFormat)
(documentation dd.Mon.yyyy "dd.Mon.yyyy is a format for dates consisting of two digits representing the day of the month (1 to 31), followed by a period, followed by a three-letter month abbreviation  representing the month of the year (Jan,Feb, Dec,etc.), followed by a period, followed by two digits representing the last two digits of the year. In this format, today's date is 03.Mar.2002, i.e. March 2nd, 2004.")

(instance dd/mm/yy DateFormat)
(documentation dd/mm/yy "dd/mm/yy is a format for dates consisting of two digits representing the day of the month (1 to 31), followed by a forward slash, followed by two digits representing the month of the year (1 to 12), followed by a forward slash, followed by two digits representing the last two digits of the year. In this format, today's date is 02/03/04, i.e. March 2nd, 2004.  This format is used in Europe and may be confusing for Americans.")

(instance Inefficient EfficiencyMeasure)
(documentation Inefficient "lack of efficiency is a semiquantitative attribute related to efficiency - this assumes that there is a known high efficiency method, and methods that are not as efficient have the property of inefficiency.  i.e. it is possible to do the process more efficiently")

;; deliberate process with output -- 
;; efficiency  -- ratio of outputs/results to inputs/resources 
;; (consumes **physical** resources: at least time)

(instance necessarily TernaryRelation)
(domainSubclass necessarily 1 Entity)
(domain necessarily 2 BinaryRelation)
(domainSubclass necessarily 3 Entity)
(documentation necessarily "This is the inverse of :relationAllExists.  (necessarily ?CLASS1 ?REL ?CLASS2) means that every instance of ?CLASS1 is related to some instance of ?CLASS2 by the ?REL relation.")

;; <axiomgloss> Relation necessarily is logically identical to
;; relation :relationAllExists, with arguments 1 and 2 reversed.
;; this relation specifies that every instance of argument 1 is related to some 
;; instance of argument 3 by the relation arg2.

(<=>
    (necessarily ?CLASS1 ?REL ?CLASS2)
    (:relationAllExists ?REL ?CLASS1 ?CLASS2))

;; PhysicalResource
;; an object, quantity of substance, or time
;; note SUMO resource relation: (subrelation resource patient)


(subclass ProductiveProcess Process)
(documentation ProductiveProcess "A ProductiveProcess is an IntentionalProcess which has some output that can be measured, so that a measure of productivity (= efficiency) can be created and used.  Mere recreation would not qualify.  Even though the same form of recreation can be performed, for example, for different prices, the payment of money for a product or service is not considered as consumption of resources. Money is not a consumable resource for the purpose of measuring efficiency.  Price and efficiency measure different concepts in this ontology.")

(subclass InheritableClassRelation BinaryPredicate)
(documentation InheritableClassRelation "An InheritableClassRelation  is a binary relation that has a class as its argument1 type (domainSubclass 1) and which also applies to all subclasses.  Thus, though it is a class-level predicate, it functions as a template slot in a frame representation.  Any assertion using a class-level relation that is not an InheritableClassRelation will not necessarily be true of all subclasses of the domain 1 asserted in the assertion.  For binary relations, the arg2 class variable may be restricted at a subclass level by the :hasRestrictedVal relation.")

(=>
   (and
       (instance ?REL InheritableClassRelation)
       (?REL ?CLASS1 ?CLASS2))
   (forall ?CLASS
     (=>
       (subclass ?CLASS ?CLASS1)
       (?REL ?CLASS ?CLASS2))))

;; inheritability also applies to ternary predicates,
;;   if the last argument is a class -- this latter must
;; be true for ternary relations that are inheritable,
;;  so it is not specified in the defining axiom

(=>
   (and
       (instance ?REL InheritableClassRelation)
       (?REL ?CLASS1 ?ENT ?CLASS2))
   (forall ?CLASS
     (=>
       (subclass ?CLASS ?CLASS1)
       (?REL ?CLASS ?ENT ?CLASS2))))


(subclass TotalValuedClassRelation TernaryRelation)
(instance TotalValuedClassRelation InheritableClassRelation)
(documentation TotalValuedClassRelation "A TotalValuedClassRelation is a binary relation between classes that implies that every instance of every subclass of the first class is related to some instance of the second class by an instance-level relation.")

(=>
  (and
    (instance ?REL TotalValuedClassRelation)
    (?REL ?CLASS1 ?INSTREL ?CLASS2))
  (forall (?INST1)
      (=>
        (instance ?INST1 ?CLASS1 ?CLASS2)
        (exists (?INST2)
          (and
            (instance ?INSTREL BinaryRelation)
            (instance ?INST2 ?CLASS2)
            (?INSTREL ?INST1 ?INST2))))))

(instance hasNecessaryPart BinaryPredicate)
(instance hasNecessaryPart InheritableClassRelation)
(domainSubclass hasNecessaryPart 1 Entity)
(domainSubclass hasNecessaryPart 2 Entity)
(documentation hasNecessaryPart "(hasNecessaryPart ?CLASS1 ?CLASS2) means that every instance of ?CLASS1 has a part which is an instance of ?CLASS2.  Since this is an InheritableClassRelation, then it is true of every instance of every subclass of ?CLASS1 that each such instance also has a part which is an instance of ?CLASS2.  See hasOptionalPart for an alternative part relation.")

(=>
   (hasNecessaryPart ?Class1 ?Class2)
   (forall (?INST)
      (=>
         (instance ?INST ?Class1)
         (exists (?PART)
            (and
               (instance ?PART ?Class2)
               (properPart ?PART ?INST))))))

(instance hasOptionalPart BinaryPredicate)
(domainSubclass hasOptionalPart 1 Entity)
(domainSubclass hasOptionalPart 2 Entity)
(documentation hasOptionalPart "(hasOptionalPart ?CLASS1 ?CLASS2) means that there is some instance of ?CLASS1 which has a part which is an instance of ?CLASS2.  More informally, this relation may be stated 'instances of CLASS1 may have an instance of CLASS 2 as a part'.  Note that not every subclass of CLASS1 will necessarily have instances with parts of type CLASS2.  A car manufacturer may sell a line of 'Belcher' autos with GPS as an optional part.  But the 'Belcher' (a subclass of Belcher) may not have that part as an option.  Thus it is true that  (hasOptionalPart Belcher GPS) but it is not true that (hasOptionalPart BelcherQQ GPS).  Thus the relation is not an InheritableClassRelation. If there are subclasses of Belcher that have optional GPS, that assertion must be made on that subclass explicitly.")

(=>
   (hasOptionalPart  ?Class1 ?Class2)
   (exists (?INST ?PART)
      (and
         (instance ?INST ?Class1)
         (instance ?PART ?Class2)
         (properPart ?PART ?INST))))

(instance creates BinaryRelation)
(subrelation creates result)
(domain creates 1 PhysicalEvent)
(domain creates 2 Physical)
(documentation creates "relates a physical event to an entity whose existence may be caused by the event.  The entity usually comes into existence as a result of the event.   The entity may be a new state, a physical embodiment of a mental entity (such as a musical composition), or a physical object.  Note that this is a time-dependent relation, and therefore strictly abstract entities cannot be created, though their physical embodiments can.  No creation event can be instantaneous, it must proceed over some time interval.")

(=>
  (creates ?EVENT ?X)
  (and
     (exists (?TIME1 ?TIME2 ?INTERVAL)
        (and
           (instance ?INTERVAL TimeInterval)
           (time ?EVENT ?INTERVAL)
           (instance ?TIME1 TimePoint)
           (equal ?TIME1 (BeginFn ?INTERVAL))
           (instance ?TIME2 TimePoint)
           (equal ?TIME2 (EndFn ?INTERVAL))
           (holdsDuring ?TIME1 (not
                                   (property ?X ActualExistence)))
           (holdsDuring ?TIME2 (property ?X ActualExistence))))))

(instance createdBy BinaryRelation)
(domain createdBy 1 Physical)
(domain createdBy 2 CognitiveAgent)
(:isRestrictedTo createdBy AbstractCode StandardsAuthority)
(documentation createdBy "createdBy is used to assert that a particular instance of an entity was created in an action which has a particular agent as its main actor.  In this relation, the event itself is unspecified.")

(=>
   (createdBy ?ENT ?AGENT)
   (exists (?EVENT ?PROC ?TIMEINT)
        (and
             (instance ?TIMEINT TimeInterval) 
             (instance ?PROC IntentionalProcess) 
             (hasResultingEvent ?PROC ?TIMEINT ?EVENT)
             (creates ?EVENT ?ENT))))

(instance isMaintainedBy BinaryRelation)
(domain isMaintainedBy 1 Entity)
(domain isMaintainedBy 2 CognitiveAgent)
(:isRestrictedTo isMaintainedBy AbstractCode StandardsAuthority)
(documentation isMaintainedBy "isMaintainedBy is used to assert that a particular instance of an entity can be modified in some way, and specifies the agent who has the authority or responsibility to do the modifying.  In this relation, the method of modifying is unspecified.  The entity maintained can be physical, such as a building maintained by its owner or manager, or an AbstractCode, maintained by a StandardsAuthority.  In the general case, there may be more than one maintaining agent.")

(instance hasResultingEvent TernaryRelation)
(domain hasResultingEvent 1 Process)
(domain hasResultingEvent 2 TimeInterval)
(domain hasResultingEvent 3 PhysicalEvent)
(documentation hasResultingEvent "hasResultingEvent relates a physical process (in SUMO class Process is a pysical process) to the event that is generated by that process operating over a time interval.")

;;  domain 1 of SUMO containsInformation  is ContentBearingObject
;;   in SKIF, not in Protege!!  -- problem in importation

(subclass PhysicalSymbol ContentBearingObject)
(documentation PhysicalSymbol "a PhysicalSymbol is a ContentBearingObject that respresents a single concept.  It may be composed of more than one smaller symbol, as a word can be composed of letters, but the conceptual content of the whole symbol must not be merely a concatenation of the individual symbols, but must have content unpredictable from the individual symbols.  Thus a sentence is not a symbol because its content is predictable by analyzing the syntactic relations among the individual words.  Printing characters, icons, and images are the most common symbols.  Each of these has a corresponding AbstractSymbol that it represents.")

(subclass ContentBearingArtifact ContentBearingObject)
(subclass ContentBearingArtifact Artifact)
(documentation ContentBearingArtifact "A ContentBearingObject that is an Artifact.")

(subclass IntelligentAgent CognitiveAgent)
(documentation IntelligentAgent "An IntelligentAgent is a class of CognitiveAgents, the typical adult member of which can understand and generate abstract linguistic utterances (not necessarily speech).  This distinguishes people (and at some time in the future intelligent machines) from animals such as apes that have some understanding and planning ability, but no use of language with the complexity of natural languages.  This also includes groups, organizations, and artifactual agents with the legal capabilities of people, such as corporations.")

(subclass NormativeProposition Assertion)
(documentation NormativeProposition  "A Proposition that asserts that you should or should not do something.  This may be asserted by some agent, but may have no force or no agreement from other agents.  Such an ineffective assertion might be, for example, 'If you don't eat your spinach, the boogieman will get you'.")

(instance isaModalAttributeOf BinaryRelation)
(domain isaModalAttributeOf 1 NormativeAttribute)
(domain isaModalAttributeOf 2 Assertion)
(documentation isaModalAttributeOf "The inverse of hasModalAttribute - it relates a NormativeAttribute to a proposition that it applies to.")

(subclass Rule NormativeProposition)
(documentation Rule "A Rule is a set of one or more NormativePropositions  that are issued by some Agent which is considered by some group of agents to be as an authority --not necessarily a legal authority, it can be very informal, such as an agreement among a group of people.  Failing to adhere to a Rule need not have any negative consequences, but typically there is some negative consequence, which may be minimal, so most Rules are also Obligations.  A formalistic grammatical rule (which is often violated without loss of comprehensibility) may be an example of a rule with no consequences for violation.")

(subclass Obligation Rule)
(documentation Obligation "An obligation is a Rule stating that a certain act, if not performed by a cognitive agent, leads to some potential negative consequence -- a 'Sanction', which may be an event, state, or process.   The sanction may not actually be imposed -- one can get away with crimes-- but it must cause some negative state -- e.g. the potential for being imprisoned is itself a negative.  Potential danger is also considered a negative state.
    Obligations must be imposed by some authority: legal obligations are imposed by a legal authority, moral obligations are imposed by a moral authority, which can be the obligated agent - if an agent imposes rules on him/herself, the resulting negative effect for violation will be pangs of conscience. .
    SUMO NOTE: the SUMO Attribute 'Obligation' has been renamed 'Obligatory'.
  Note that individual may disagree as to whether something is or is not an obligation. A rule may only be a desideratum of some agent, with no real sanctions attached, in which case it is not an obligation.
========
Morality:
  moral rules have no objective basis but they achieve force by being agreed to by some community.
    Communities try to impose the dominant morality on the whole  community.
    some members may reject the dominant consensus
    Is a rule of the community an obligation to all, or only to those who agree to the rules?
    If the community enforces its rules by some sanction, it is an obligation
    If a member accepts the rule, it is a moral obligation, and the sanction is conscience
    if there is no enforcement and member does not accept the rule, it is not an obligation.")

(subclass Authority IntelligentAgent)
(documentation Authority "An Authority is an agent that is recognized by some CognitiveAgent as having the capability of creating a Rule.")

(subclass StandardsAuthority Authority)
(subclass StandardsAuthority Organization)
(documentation StandardsAuthority "An authority which is an organization recognized by some group of agents as being able to declare a standard.")

;; has assent (to code-issuing power)
;;    by group 

(subclass CodeListAgency StandardsAuthority)
(necessarily CodeListAgency hasAbstractName CodeListAgencyNameString)
(documentation CodeListAgency "A CodeListAgency is a StandardsAuthority that maintains a CodeList.  UBL: 'An agency that maintains one or more code lists'.")

(subclass CodeListAgencyIdentifier Identifier)
(:hasRestrictedVal CodeListAgencyIdentifier isaNameOf CodeListAgency)
(:hasRestrictedVal CodeListAgencyIdentifier hasReferent CodeListAgency)
(necessarily CodeListAgencyIdentifier isaNameOf CodeListAgency)
(documentation CodeListAgencyIdentifier "A CodeListAgencyIdentifier is an abstract string that is a formal identifier (in some identification scheme) of a CodeListAgency.")

(subclass DocumentPhysical Text)
(:hasRestrictedVal DocumentPhysical hasAbstractContent AbstractDocument)
(:hasRestrictedVal DocumentPhysical containsInformation DocumentalProposition)
(documentation DocumentPhysical "A DocumentPhysical is a solid object containing at least some written linguistic information.")

(subclass EncodedDocument DocumentPhysical)
(documentation EncodedDocument "An EncodedDocument is a Document produced by some process of Encoding.  Encoding must use some procedure other than writing in a widely-used language, it must have some non-obvious features that make interpretation more difficult without knowledge of the encoding procedure.")

(subclass EncryptedDocument EncodedDocument)
(documentation EncryptedDocument "An EncryptedDocument is a Document produced by an EncryptionProcess.  The encryption is intended to prevent interpretation of the document by unintended recipients.")

(subclass AbstractInformationalEntity AbstractObject)
(subclass AbstractInformationalEntity SomethingUseful)
(documentation AbstractInformationalEntity "An AbstractInformationalEntity  is any abstract entity that carries information that can be interpreted by a cognitive agent (possibly with the use of tools such as a computer).  It can be a simple abstract symbol, or a complex concatenation of propositions.  It is any form of abstract information in a symbolic form that can be represented in a physical object and interpreted by IntelligentAgents.  This symbolic information refers to another entity, but is not itself the entity it refers to.")

(subclass Specification Rule)
(subclass Specification TextualProposition)
(subclass Specification SomethingUseful)
(documentation Specification "A Specification is a set of Rules prescribing how something must be done in order to achieve a desired result.  This is a very general concept. It includes natural-language grammars and computer programs as well as industrial manufacturing procedures.  It is a TextualProposition, and therefore needs to be encoded as text in some physical object at some time.")

(instance specifiesProcess BinaryPredicate)
(domainSubclass specifiesProcess 1 Specification)
(domainSubclass specifiesProcess 2 Process)
(documentation specifiesProcess "Each type of Specification specifies a type of process, which is the realization of that specification.  Thus a play or screenplay specifies a process of acting.  (specifiesProcess ?SPECCLASS ?PROCCLASS) means that every instance ?PROC of ?PROCCLASS that is related to a specification by the realization relation (realization ?PROC ?SPEC) is a realization of some instance of ?SPECCLASS.  This is not a necessary relation (TotalValuedRelation) because there may be specifications for processes which are never executed.")

(=>
  (specifiesProcess ?SPECCLASS ?PROCCLASS)
  (forall (?PROC ?SPEC)
    (=>
      (and
        (instance ?PROC ?PROCCLASS)
        (realization ?PROC ?SPEC))
      (instance ?SPEC ?SPECCLASS))))

(subclass InformationAttribute InternalAttribute)
(documentation InformationAttribute "An InformationAttribute is any of a broad class of attributes that apply specifically to abstract information.")

(subclass FileAttribute InformationAttribute)
(documentation FileAttribute  "A FileAttribute is an attribute of an abstract ComputerFile.")

(subclass FileFormat FileAttribute)
(subclass FileFormat Format)
(documentation FileFormat  "A FileFormat is a specification for the linear arrangement of data in a ComputerFile.  A FileFormat is any of several specific formats that may apply to ComputerFiles.  Some FileFormats are compatible with others, i.e. a file may have more than one format.  All normal files are binary encoded.")

(subclass TextAttribute InformationAttribute)
(documentation TextAttribute "A TextAttribute  is any of several attributes that a text may have.  It may be shared by physical texts and abstract texts.")

(subclass DateFormat TextFormat)
(documentation DateFormat "A DateFormat is some specified arrangement of text representing a specific date, such as 3/4/96 or 4 March 1996.")

(subclass TimeFormat TextFormat)
(documentation TimeFormat "A TimeFormat is some specified arrangement of text representing a time of day, such as '1:30 PM' or '13:30'.")

(subclass DateTimeFormat TextFormat)
(documentation DateTimeFormat "A DateTimeFormat is some specified arrangement of a date string and a time string, such a '1:30 PM on 4 March 1996'.")

(instance Binary InformationAttribute)
(documentation Binary "An informational entity has Binary format if it is represented as a linear string of bits, each having only one of two values,typically represented as 1 and 0.")

(instance MimeEncoded FileFormat)
(documentation MimeEncoded "An informational entity has MimeEncoded format if its encoding is recognizable as one of the Mime formats.")

;; <module>UBL-invoice</module>

(subclass Agreement Proposition)
(documentation Agreement  "An agreement is a proposition that states that the speaker believes the proposition asserted by another speaker.")

(subclass Assent Agreement)
(documentation Assent "Assent is a response by one agent to an assertion by another agent that the responding agent agrees to the truth of the assertion.")

(subclass Communication AbstractInformationalEntity)
(subclass Communication Proposition)
(:hasRestrictedVal Communication isUsedIn Communicating)
(relatedInternalConcept Communication Communicating)
(documentation Communication "A Communication is the product of the process of Communicating. It is an Proposition which could have been conveyed by a text or a non-textual signal.  It has one or more authors and an audience -- i.e. an agent or group of agents to whom it is directed.  For some Communications, e.g. general publications or novels, the audience may be the entire human race.  Note that the process of Communicating has an experiencer -- but an experiencer is not necessarily a member of the audience to whom the communication is addressed; an intercepted private communication provides an example. Therefore thereis a need for a special relation describing the audience to whom a communication is directed.  Note that this is the abstract content of a communication, which may be realized as a series of sound waves or electromagnetic waves or gestures, or in some fixed medium such as text of a CD-ROM; the realization may be a physical object or an event.")

(subclass Assertion Communication)
(:hasRestrictedVal Assertion isRepresentedBy Statement)
(documentation Assertion "An Assertion is a Communication which describes a state of affairs and asserts that the state holds. It may have the value of true or false.  Assertions usually are simple, describing one fact, but may be more complex, i.e. may be composed of multiple assertions.  An Assertion is a communication from one IntelligentAgent to another.  This is the abstract propositional content of a SUMO physical 'Statement'.")

(subclass EncodingProcedure Specification)
(specifiesProcess EncodingProcedure Encoding)
(documentation EncodingProcedure "An EncodingProcedure is a specification of how information is to be represented so that the content can be recovered from the encoded form.")

(subclass EncryptionProcedure EncodingProcedure)
(specifiesProcess EncryptionProcedure Encryption)
(documentation EncryptionProcedure "An EncryptionProcedure is a specification of how information in a human-recognizable form is to be represented so that the content cannot be recovered without knowledge of the decryption method.  The decryption method may or may not be deducible from knowledge external to the encrypted data.")

(subclass Encryption Encoding)
(:hasRestrictedVal Encryption creates EncryptedDocument)
(documentation Encryption "Encryption is the process of executing an EncryptionProcedure so that information in a human-recognizable form is represented so that the content cannot be recovered without knowledge of the decryption method.  The decryption method may or may not be deducible from knowledge external to the encrypted data.")

(subclass FormatSpecification EncodingProcedure)
(documentation FormatSpecification "A FormatSpecification is a specification for a spatial arrangement of information, which may be linear or higher-dimensional.  The spatial relations between component entities in a Format may or may not contain information not otherwise encoded.  A simple layout Format will not usually have information encoded in the arrangement of the components.")

(subclass Grammar Specification)
(documentation Grammar "A Grammar is a set of rules regarding how symbols may be combined so as to form meaningful combinations.  This includes natural-language grammars and formal grammars.")

(subclass AbstractLanguage AbstractLinguisticExpression)
(:hasRestrictedVal AbstractLanguage isAbstractContentOf Language)
(:hasRestrictedVal AbstractLanguage isRepresentedBy Language)
(necessarily AbstractLanguage isAbstractContentOf Language)
(documentation AbstractLanguage "An AbstractLanguage is the abstract informational entity which is thought of as a language, natural or artificial.  This is the abstract correlate of the SUMO Physical Object labeled 'Language'.  It is any language, natural or artificial, viewed as the abstract set of vocabulary and grammatical rules.  this includes dialects.")

(instance isaTextComponentOf BinaryPredicate)
(domain isaTextComponentOf 1 AbstractText)
(domain isaTextComponentOf 2 AbstractText)
(inverse isaTextComponentOf hasaTextComponent)
(documentation isaTextComponentOf "isaTextComponentOf  relates a proper part of an AbstractText to a whole or a larger part of which it is a component.")

(=>
   (isaTextComponentOf ?TEXT1  ?TEXT2)
   (hasLocation ?TEXT1  ?TEXT2))

(instance usesLanguage BinaryPredicate)
(domain usesLanguage 1 (UnionFn AbstractLinguisticExpression Proposition))
(domain usesLanguage 2 AbstractLanguage)
(relatedInternalConcept usesLanguage expressedInLanguage)
(documentation usesLanguage "(usesLanguage ?EXP ?LANG) means that ?EXP is a valid expression or combination of expressions of the language ?LANG, and contains propositional content.  This relation is similar to the SUMO expressedInLanguage, but the SUMO relation has physical objects as its domain 1, rather than abstract objects.")

(subclass AbstractLinguisticExpression AbstractInformationalEntity)
(:hasRestrictedVal AbstractLinguisticExpression isAbstractContentOf LinguisticExpression)
(documentation AbstractLinguisticExpression "Any linguistic elements in their abstract form.")

(subclass Description TextualProposition)
(:hasRestrictedVal Description hasTextualExpression DescriptiveText)
(documentation Description "A Description is the propositional content a text that is intended to explain a relatively restricted concept or object.  It has one central theme and can be considered as a description of that theme.  It is very flexible and can vary from a brief definition to an encylopedia-length article, provided that it sticks pretty closely to a single theme.")

(subclass DescriptiveText AbstractText)
(documentation DescriptiveText "A DescriptiveText is the AbstractText representation of a proposition that is intended to explain a relatively restricted concept or object.  It has one central theme and can be considered as a description of that theme.  It is very flexible and can vary from a brief definition to an encylopedia-length article, provided that it sticks pretty closely to a single theme.")

(subclass Definition DescriptiveText)
(documentation Definition "A Definition is a structured AbstractText that has term string (which may be more than one word), and a definition of that term.")

(instance hasListElement BinaryRelation)
(instance hasListElement TotalValuedRelation)
(domainSubclass hasListElement 1 List)
(domainSubclass hasListElement 2 Entity)
(documentation hasListElement "hasListElement relates a class of Lists to another class.  (hasListElement ?LISTCLASS ?ENTITYCLASS) asserts that every instance list of ?LISTCLASS has as an element at least one instance of ?ENTITYCLASS.")

(=>
  (hasListElement ?LISTCLASS ?ENTITYCLASS)
  (forall (?LIST)
     (=>
        (instance ?LIST ?LISTCLASS)
        (and
            (exists (?ENT)
               (instance ?ENT ?ENTITYCLASS))
            (inList ?ENT ?LIST)))))

(subclass BinaryObject AbstractInformationalEntity)
(hasAttributeInstance BinaryObject Binary)
(documentation BinaryObject "A BinaryObject is any abstract binary-encoded data representable in any physical form, whether or not it is readable by a computer.")

(subclass ComputerFile BinaryObject)
(necessarily ComputerFile property FileFormat)
(necessarily ComputerFile hasAbstractName FileName)
(:hasRestrictedVal ComputerFile hasAbstractName FileName)
(documentation ComputerFile "A ComputerFile is binary-encoded data in any physical form that can be read into a computer, usually using some computer peripheral device.  This is the abstract concept, and a linear set of bytes on paper tape is considered to represent the exact same ComputerFile as the same set of bytes in a computer memory.")

(subclass TextFile ComputerFile)
(subclass TextFile AbstractText)
(documentation TextFile "A TextFile is a computerFile whose content is predominantly a text, that is the content contains mostly linear strings of characters.  Some images may be associated with the text.  The text is encoded in some CharacterSet")

(subclass ImageFile ComputerFile)
(documentation ImageFile "An ImageFile is a ComputerFile whose content is predominantly some image, though some text may be associated with the image.")

(subclass FileName Identifier)
(:hasRestrictedVal FileName isaNameOf ComputerFile)
(documentation FileName "A FileName is the identifying string that names a ComputerFile.")

(subclass Location Entity)
(documentation Location "A Location can be abstract, for abstract objects, or physical, for physical objects.  This is a broad concept.")

(subclass InternetLocation Location)
(hasNecesaryRelationTo InternetLocation hasAbstractName URL)
(documentation InternetLocation "The InternetLocation is a location in the abstract space of the Internet -- it can be a domain, a directory, or a file -- whether or not it is accessible at any one time.")

(subclass FileInternetLocation InternetLocation)
(documentation FileInternetLocation "The InternetLocation of a ComputerFile.")

;; communication methods: oral, written, electronic
;;  start with events

(subclass AbstractText AbstractLinguisticExpression)
(:hasRestrictedval AbstractText isAbstractContentOf Text)
(documentation AbstractText "An AbstractText is an AbstractInformationalEntity containing at least some linguistic elements, though image and musical elements may be included.  A song is an AbstractText.  It is not necessarily a Proposition - AbstractTexts that are propositions are found in the subclass TextualProposition.")

(subclass TextualProposition Proposition)
(documentation TextualProposition "A TextualProposition is a Proposition that is the content of a Text.  Single words like 'cat' may have certain entities as referents, but cannot be said to express a proposition.")

(subclass CommunicationProposition TextualProposition)
(:hasRestrictedval CommunicationProposition isReferencedBy Communication)
(documentation CommunicationProposition "A CommunicationProposition is a Proposition that is the propositional content of a communication.")

(subclass MessageProposition CommunicationProposition)
(:hasRestrictedval MessageProposition isReferencedBy MessageProposition)
(documentation MessageProposition "A MessageProposition is a Proposition that is the propositional content of a Message.")

(subclass AbstractFormattedText AbstractText)
(minCardinalityAtClasses AbstractFormattedText hasaTextComponent 2 AbstractText)
(documentation AbstractFormattedText "A text with at least two components in a particular arrangement.  See also AbstractDocument.")

(subclass TextFormat TextAttribute)
(documentation TextFormat "A TextFormat is one of the attributes of a formatted text.  It can take any of a very large variety of specific values, as for example, any HTML format specification.")

(instance hasTextFormat BinaryPredicate)
(domain hasTextFormat 1 AbstractLinguisticExpression)
(domain hasTextFormat 2 TextFormat)
(documentation hasTextFormat "hasTextFormat relates an AbstractLinguisticExpression to the format in which it is arranged.")

(subclass AbstractDateTimeText AbstractFormattedText)
(subclass AbstractDateTimeText DocumentFieldString)
(necessarily AbstractDateTimeText hasaTextComponent AbstractDateText)
(:hasRestrictedVal AbstractDateTimeText hasComponentList DateAndTimeList)
(:hasRestrictedVal AbstractDateTimeText hasReferent TimePosition)
(:hasRestrictedVal AbstractDateTimeText hasTextFormat DateTimeFormat)
(:hasRestrictedVal AbstractDateTimeText isAbstractContentOf SymbolicString)
(documentation AbstractDateTimeText "An AbstractDateTimeText is an AbstractText that contains both date and time strings.")

(subclass ExpirationDateText AbstractFormattedText)
(documentation ExpirationDateText "An ExpirationDateText specifies explicitly or in abbreviated form the day on which some condition expires, such as validity of a credit card.  The ExpirationDateText may contain only a specified year or year and month, in which case it is the end of the year or end of the month which is the implied meaning of the date.")

(subclass AbstractYearText AbstractString)
(necessarily AbstractYearText hasaTextComponent AbstractYearNumberText)
(documentation AbstractYearText "An AbstractYearText must contain a number and may contain an era - 40 BC or 2004 AD, or 1993.  Abnormal-looking dates (such as 345 without an era) must be caught by the implementation with respect to the context.")

(subclass AbstractYearNumberText AbstractString)
(documentation AbstractYearNumberText "An AbstractYearNumberText is a numeric string specifying some year, without any era information.")

(subclass AbstractDateText AbstractFormattedText)
(subclass AbstractDateText DocumentFieldString)
(:hasRestrictedVal AbstractDateText represents TimeInterval)
(:hasRestrictedVal AbstractDateText hasReferent TimeInterval)
(:hasRestrictedVal AbstractDateText hasTextFormat DateFormat)
(:hasRestrictedVal AbstractDateText isAbstractContentOf SymbolicString)
(documentation AbstractDateText "An AbstractDateText is an AbstractText specifying some specific day in some date era.")

(subclass AbstractTimeText AbstractFormattedText)
(subclass AbstractTimeText DocumentFieldString)
(:hasRestrictedVal AbstractTimeText hasTextFormat TimeFormat)
(:hasRestrictedVal AbstractTimeText isAbstractContentOf SymbolicString)
(:hasRestrictedVal AbstractTimeText hasReferent TimePosition)
(documentation AbstractTimeText "An AbstractTimeText is an AbstractText specifying a time of day.  It may be in twelve-hour or twenty-four hour format.")

(subclass TimeOfDay RecurrentTimeInterval)
(documentation TimeOfDay "A TimeOfDay is a class of TimeIntervals, each of which specifies an hour and minute, without specifying the specific day.  12:01 AM would be a subclass of TimeOfDay, as would 12:01.34 AM.  This is considered as an interval rather than a TimePoint, the interval lasting as long as the precision of the specification indicates.  The typical precision is a one-munite interval, such as 14:30.")

(subclass RecurringDay Day)
(subclass RecurringDay RecurrentTimeInterval)
(documentation RecurringDay "A RecurringDay  is class of Day that has more than one instance, e.g. the class of Mondays or the class of January 1sts.")

(subclass DocumentalProposition TextualProposition)
(:hasRestrictedVal DocumentalProposition  isTheInformationContainedIn DocumentPhysical)
(:hasRestrictedVal DocumentalProposition  isReferencedBy AbstractDocument)
(:hasRestrictedVal DocumentalProposition  hasTextualExpression AbstractDocument)
(documentation DocumentalProposition  "A DocumentalProposition is a Proposition that is the conceptual content of a Document.")

(subclass AbstractDocument AbstractFormattedText)
(:hasRestrictedVal AbstractDocument hasPropositionalContent DocumentalProposition)
(:hasRestrictedVal AbstractDocument hasReferent DocumentalProposition)
(documentation AbstractDocument "A AbstractDocument is an AbstractFormattedText with at least two different AbstractText components. The document expresses at least one proposition")

(subclass StructuredDocumentText AbstractFormattedText)
(necessarily StructuredDocumentText hasPropositionalContent DocumentalProposition)
(documentation StructuredDocumentText "A StructuredDocumentText the AbstractText representation of the propositional content of a DocumentPhysical, having at least two different AbstractText components.")

(subclass CommunicationEvent PhysicalEvent)
(documentation CommunicationEvent "A CommunicationEvent  is an event in which one IntelligentAgent transmits information to another via a communications channel.  The information may be of any form, speech ,music, images, etc.")

(subclass ConversationEvent CommunicationEvent)
(documentation ConversationEvent "A CommunicationEvent in which two or more IntelligentAgents transmit information by language, which may be sign language, formal or informal.  The transmission can occur at short range or over a distance by electronic communication, but must be interactive in the sense that a response can come within a few seconds, and the event includes speaking by and transmission of information between at least two participants.")

(subclass BusinessMessage Message)
(:hasRestrictedVal BusinessMessage isUsedIn BusinessProcess)
(documentation BusinessMessage "A BusinessMessage  is a Message used to conduct a business goal, commonly in purchasing and sales, but any business goal can be a purpose.")

(subclass TelephoneConversation ConversationEvent)
(documentation TelephoneConversation "A TelephoneConversation is a ConversationEvent in which voice signals are converted to transmitted as electrical signals at a point near the speaker and the electrical signals are reconverted back to voice at a point near the listener.")

(subclass TelephoneOrder TelephoneConversation)
(:hasRestrictedVal TelephoneOrder represents TelephoneOrderProposition)
(documentation TelephoneOrder "A telephone conversation or part of a telephone conversation in which a product or service was ordered.  This is a physical event with a propositional content.")

(subclass TelephoneOrderProposition BusinessMessage)
(documentation TelephoneOrderProposition "A TelephoneOrderProposition is the propositional content of a telephone conversation or part of a telephone conversation in which a product or service was ordered.  This is directly related to a TelephoneOrder, a physical event with the same propositional content.")

(subclass TransactionMessage AbstractDocument)
(:hasRestrictedVal TransactionMessage isUsedIn FinancialTransaction)
(documentation TransactionMessage "A TransactionMessage is an AbstractDocument  transmitted in the course of purchasing and sales.")

(subclass AbstractOrder TransactionMessage)
(documentation AbstractOrder "UBL: A document that contains information directly relating to the economic event of ordering products.")

(subclass AbstractInvoice TransactionMessage)
(:hasRestrictedval AbstractInvoice isRepresentedBy Invoice)
(:hasRestrictedVal AbstractInvoice isAbstractContentOf Invoice)
(necessarily AbstractInvoice hasaTextComponent InvoiceLine)
(necessarily AbstractInvoice hasaTextComponent LegalTotalsText)
(documentation AbstractInvoice "UBL: the document that describes the financial commitment of the Order.")

(subclass InvoiceProposition MessageProposition)
(documentation InvoiceProposition "An InvoiceProposition is a MessageProposition expressing the propositional content of an Invoice.")

(subclass AbstractOrderChange TransactionMessage)
(documentation AbstractOrderChange "UBL: A document that contains information directly relating to the economic event of changing an order.")

(subclass AbstractOrderResponseSimple TransactionMessage)
(documentation AbstractOrderResponseSimple "UBL: The document responding to the Buyer to indicate simple acceptance or rejection of the entire Order.")

(subclass AbstractOrderResponse TransactionMessage)
(documentation AbstractOrderResponse "UBL: The document responding to the Buyer to indicate detailed responses against a single Order.")

(subclass AbstractOrderCancellation TransactionMessage)
(documentation AbstractOrderCancellation "UBL: The document that advises either party of the cancellation of an Order.")

(subclass AbstractDespatchAdvice TransactionMessage)
(documentation AbstractDespatchAdvice "UBL: The document that describes the content of goods shipped.")

(subclass AbstractReceiptAdvice TransactionMessage)
(documentation AbstractReceiptAdvice "UBL: The document that advises the goods received and accepted by the buyer.")

(subclass WrittenCommunicationEvent CommunicationEvent)
(documentation WrittenCommunicationEvent "In a WrittenCommunicationEvent, one IntelligentAgent creates a written document intended to be read by another agent.")

(instance isUsedIn BinaryPredicate)
(domain isUsedIn 1 SomethingUseful)
(domain isUsedIn 2 IntentionalProcess)
(documentation isUsedIn "isUsedIn relates a useful thing - concrete or abstract to the Process in which it is used by some unspecified CognitiveAgent who uses it in that process.  See related relation 'uses', specifying an object used by an Agent in an unspecified process.")

(subclass BusinessProcess IntentionalProcess)
(documentation BusinessProcess "Any process used in business including but not restricted to buying, selling, manufacturing, or performing a service.")

(subclass SomethingUseful Entity)
(documentation SomethingUseful "SomethingUseful can be any entity, an object, process, event, procedure, theory, that a CognitiveAgent uses to accomplish a goal.")

(subclass CommunicationsChannel SomethingUseful)
(documentation CommunicationsChannel "Any means used by IntelligentAgents to transmit information from one to another.")

(subclass TelephoneConversing Speaking)
(documentation TelephoneConversing "A process of orally communicating in which a telephone is used to convert sounds to electrical signals at one end and reconvert the signals to sound at another end.")

(subclass BusinessAgent IntelligentAgent)
(documentation BusinessAgent "An IntelligentAgent that is participating in a BusinessProcess.")

(subclass BusinessParty BusinessAgent)
(documentation BusinessParty "A BusinessAgent who is participating in a FinancialTransaction - buying or selling.")

(subclass :UBL-Synonym :Synonym)
(documentation :UBL-Synonym ":UBL-Synonym is the class containing instances of UBLK terms which will have pointers to corresponding terms in the ontology.")

(instance hasInternetLocation BinaryRelation)
(domain hasInternetLocation 1 ComputerFile)
(domain hasInternetLocation 2 InternetLocation)
(documentation hasInternetLocation "hasInternetLocation  specifies the internet location (usually a URI), if one exists, of a computer file.  Not every computer file will have an InternetLocation.")

(instance hasBinaryEncoding BinaryPredicate)
(domain hasBinaryEncoding 1 TextualProposition)
(domain hasBinaryEncoding 2 BinaryObject)
(documentation hasBinaryEncoding "hasBinaryEncoding points to the abstract BinaryObject that encodes the propositional content of a textual proposition.  The Binary object will usually be a ab abstract ComputerFile with encoded text, but is not necessarily so.")

(instance holdsThePosition QuaternaryRelation)
(domain holdsThePosition 1 Human)
(domain holdsThePosition 2 Position)
(domain holdsThePosition 3 :FunctionTerm)
(domain holdsThePosition 4 Organization)
(documentation holdsThePosition "(holdsThePosition ?PERS ?POS of ?ORG) means that ?PERS holds the position of ?POS in organization ?ORG, and there is only one person at a time that can hold that position.  so (holdsInContext (holdsThePositionOf BillClinton President of UnitedStates) 1994-2000) means that in the time interval 1994 through 2000 BillClinton was the one who occupied the position of President of the United States.  The 'of' term is logically superfluous, added only to make the relation easier to read.  This relation is not defined for function terms other than 'of'.")

(=>
   (holdsThePosition ?PERS ?POS of ?ORG)
   (and
      (instance ?PERS Human)
      (isUniqueFor ?POS ?ORG)
      (instance ?ORG Organization)
      (occupiesPosition ?PERS ?POS ?ORG)))

(instance isUniqueFor BinaryPredicate)
(domain isUniqueFor 1 Position)
(domain isUniqueFor 2 Organization)
(documentation isUniqueFor "isUniqueFor specifies that a particular instance of Position (e.g. President) is unique in a particular organization, i.e. only one person can hold that position at any one time.  This allows definition of positions (e.g. Vice President) that may or may not be unique, depending on the organization.  If a position is unique in an entire class of organizations, use (:hasValueClass ?POS isUniqueFor ?ORGCLASS).")

(=>
  (isUniqueFor ?POS ?ORG)
  (forall (?X ?Y)
    (=>
      (and
        (occupiesPosition ?X ?POS ?ORG)
        (occupiesPosition ?Y ?POS ?ORG))
      (equal ?X ?Y))))

(instance hasUniquePosition BinaryPredicate)
(domain hasUniquePosition 1 Organization)
(domain hasUniquePosition 2 Position)
(inverse hasUniquePosition isUniqueFor)
(documentation hasUniquePosition "hasUniquePosition specifies the name of a position in an organization for which there can be only one person occupying that position at any one time.")

(subclass UnitOfCurrency UnitOfMeasure)
(documentation UnitOfCurrency "A UnitOfCurrency is the specific unit in which amounts of currency are measured.")

(subclass CurrencyAmount RationalNumber)
(documentation CurrencyAmount "A CurrencyAmount is a rational number that is used to specify some number of monetary units in a business transaction.  This is the equivalent of UBL Amount.Content.  This is rational number rather than a real number because the number of decimal digits will in general be restricted.  In invoices, it is unlikely that more than 3 decimal digits will appear in a price.")

(subclass UBLCurrencyMeasure CurrencyMeasure)
(documentation UBLCurrencyMeasure "UBLCurrencyMeasure is a class added to correspond to UBL 'Amount.Type'.  This is represented as a subclass of SUMO CurrencyMeasure because according to the UBL specification, the unit of currency may only be implied rather than explicitly represented in the physical embodiments of this abstract concept.  The 'Content' (quantifier) of this UBL concept is a rational number rather than a real number because the number of decimal digits will in general be restricted.  In invoices, it is unlikely that more than 3 decimal digits will appear in a price.  UBL: A number of monetary units specified in a currency where the unit of currency is explicit or implied.")

(subclass AbstractNumericString AbstractString)
(necessarily AbstractNumericString hasTextFormat NumericFormat)
(:hasRestrictedVal AbstractNumericString hasTextFormat NumericFormat)
(:hasRestrictedVal AbstractNumericString hasReferent Number)
(:hasRestrictedVal AbstractNumericString isRepresentedBy NumericString)
(documentation AbstractNumericString "A AbstractNumericString is an abstract string of characters representing a number.")

(subclass NumericString SymbolicString)
(necessarily NumericString represents Number)
(:hasRestrictedVal NumericString represents Number)
(necessarily NumericString isReferencedBy AbstractNumericString)
(:hasRestrictedVal NumericString isReferencedBy AbstractNumericString)
(documentation NumericString "A NumericString is a physical object bearing the character sequence representing a number.  It is referenced by an AbstractNumericString.")

(subclass AbstractCharacter AbstractSymbol)
(:hasRestrictedVal AbstractCharacter isRepresentedBy Character)
(documentation AbstractCharacter "An AbstractCharacter is one of a class of symbols contained in AbstractStrings, linear sequences of which constitute an AbstractString.  Most such characters are printable, but characters such as white space and control characters are instances.")

(subclass AbstractDigit AbstractCharacter)
(:hasRestrictedVal AbstractDigit isRepresentedBy DigitCharacter)
(documentation AbstractDigit "An AbstractDigit is one of a restricted class of symbols representing single numerals, usually 0 to 9.")

(subclass ComponentList List)
(documentation ComponentList "A ComponentList is a list of Classes each of which has instances which are potentially a component of another compound entity.  This concept provides a mechanism to state that an instance of class A may or may not have parts which are instances of classes B, C, or D.  This is an *optional* part list, but any given part may be specified as *required* by a hasNecessaryPart or necessarily proposition.  An entity *must* have at least one component from this list.")

(=>
   (and
     (instance ?CLIST ComponentList)
     (inList ?CLASS ?CLIST))
   (instance ?CLASS CLASS))

(subclass DateAndTimeList ComponentList)
(hasListElement DateAndTimeList AbstractDateText)
(hasListElement DateAndTimeList AbstractTimeText)
(documentation DateAndTimeList "A DateAndTimeList is a list containing two elements, a date string and a time string".)

(instance hasComponentList BinaryPredicate)
(domainSubclass hasComponentList 1 Entity)
(domain hasComponentList 2 ComponentList)
(documentation hasComponentList "hasComponentList specifies a list of entity classes which may or may not be present as components of the whole entity.  The list must be an exhaustive list, but it can contain an 'other' category.  Every instance of the first argument must have at least one proper part which is an instance of one of the classes in the Component List")

;; <axiomgloss> Every instance of every class of entities that has a component 
;; list must have at least one proper part that is an instance of one of the 
;; classes in the component list.  

(=>
   (hasComponentList ?ENTCLASS ?CLIST)
   (forall (?INST)
     (=>
       (instance ?INST ?ENTCLASS)
       (and
          (exists (?COMPONENT ?CLASS)
            (inList ?CLASS ?CLIST)
            (instance ?COMPONENT ?CLASS)
            (hasProperPart ?INST ?COMPONENT))))))

(subclass TextComponentList ComponentList)
(documentation TextComponentList "A TextComponentList is a list containing a set of text classes which constitute the exclusive text classes that may be found as the primary partition of a larger structured text.  Note that each of these text components may itself have smaller components, but the TextComponentList must contain only the immediate components, not the subcomponents.")

(instance hasTextPartition BinaryPredicate)
(domain hasTextPartition 1 AbstractFormattedText)
(domain hasTextPartition 2 TextComponentList)
(documentation hasTextPartition "hasTextPartition asserts that an AbstractFormattedText is composed of immediate subdivisions and every subtext of that text is one of those subdivisions or a part of one of those subdivisions.  In most cases, those subdivisions will be composed of disjoint classes, but at this time that condition is not included in the definition.  Note that this relation is required because a necessary assertion that a text is composed only of specific subtexts using the (necessarily Text hasaTextComponent (UnionFn . . .)) would preclude existence of any text subcomponents in that text.")

;; <axiomgloss>If a text has a text partition, any text component of that
;; text must be an instance of one of the partition classes, or be a 
;; text component of an instance of one of the partition classes.

(=>
   (hasTextPartition ?TEXT ?PARTLIST)
   (forall (?SUBTEXT)
      (=>
        (isaTextComponentOf ?SUBTEXT ?TEXT)
        (exists (?TEXTCLASS)
           (or
             (and
                (inList ?TEXTCLASS ?PARTLIST)
                (instance ?SUBTEXT ?TEXTCLASS))
             (exists (?SUBTEXT2)
                (and
                   (inList ?TEXTCLASS ?PARTLIST)
                   (instance ?SUBTEXT2 ?TEXTCLASS)
                   (isaTextComponentOf ?SUBTEXT ?SUBTEXT2))))))))

(subclass AbstractObject Abstract)
(subclass AbstractObject Location)
(documentation AbstractObject "AbstractObject can be a useful concept for things that have abstract locations, such as conceptual objects in an abstract space.")

(instance hasLocation BinaryPredicate)
(instance hasLocation TransitiveRelation)
(domain hasLocation 1 (UnionFn AbstractObject Physical))
(domain hasLocation 2 Location)
(documentation hasLocation "hasLocation is a more general location relation than SUMO 'located', which is restricted to Physical as arg1 and 'Object' as arg2.  We can use this to specify the location of abstract objects in abstract spaces.  This relation specifies an abstract or concrete region (not a point) as a location, and that region must contain all parts of the located object.  This is not a point location.")

(instance hasProperPart BinaryPredicate)
(domain hasProperPart 1 Entity)
(domain hasProperPart 2 Entity)
(documentation hasProperPart "(hasProperPart ?WHOLE ?PART) specifies that ?Part is a part of ?WHOLE and there is some other entity that is also a part of ?WHOLE.  If a ?WHOLE is located somewhere, the ?PART is also located there.  The manner of connection of the ?PART to the other parts of the ?WHOLE is unspecified. This is required to specify parts of entities other than physical objects, as the SUMO 'part' relations have 'Object' as their argument 1.")

;; <axiomgloss> For every proper part of an object ?OBJ there is another 
;; different entity which is also a proper par of the object ?OBJ

(=>
   (hasProperPart ?ENT ?PART1)
   (exists ?PART2
      (and
         (hasProperPart ?ENT ?PART2)
         (not (equal ?PART1 ?PART2)))))

;; <axiomgloss> Every proper part of an object ?OBJ has the same location as the 
;; object ?OBJ.  This axiom relates proper parts
;; to abstract locations specified by the 'hasLocation' relation.

(=>
   (and
      (hasProperPart ?ENT ?PART)
      (hasLocation ?ENT ?LOC))
   (hasLocation ?PART ?LOC))

;; <axiomgloss> Every proper part of a physical object ?OBJ has the same 
;; location as the physical object ?OBJ.  This axiom relates proper parts
;; to physical locations specified by the 'located' relation.

(=>
   (and
      (hasProperPart ?ENT ?PART)
      (located ?ENT ?LOC))
   (located ?PART ?LOC))

(subclass NumericFormat TextFormat)
(documentation NumericFormat  "A NumericFormat is a specification for the linear arrangement of characters to form a number - the format may apply to an AnstractNumericString or a physical NumericString.  Formats are of varying type, for example scientific format uses a decimal fraction plus a power of ten.  The minimum and maximum number of decimal digits may be specified by a NumericFormat.")

(subclass DocumentFieldString AbstractString)
(:hasRestrictedVal DocumentFieldString hasLocation AbstractDocument)
(:hasRestrictedVal DocumentFieldString isaTextComponentOf AbstractDocument)
(:hasRestrictedVal DocumentFieldString isUsedIn Communicating)
(:hasRestrictedVal DocumentFieldString isRepresentedBy SymbolicString)
(:hasRestrictedVal DocumentFieldString hasaTextComponent AbstractString)
(documentation DocumentFieldString "A DocumentFieldString is an AbstractString which is located within a defined field in an AbstractDocument.")

(subclass InvoiceLine DocumentFieldString)
(necessarily InvoiceLine hasLocation AbstractInvoice)
(:hasRestrictedVal InvoiceLine hasLocation AbstractInvoice)
(documentation InvoiceLine "InvoiceLine  is an abstract string containing the textual content of one line item of an invoice.  It contains a reference to the item ordered as well as other information relevant to the item, such as quantity and base price. This is necessarily a part of an abstract invoice.  UBL: information directly relating to a line item of a transaction. It identifies the item but only includes details about the item that are pertinent  to one occurrence on a line item, e.g. quantity etc.; an invoice has one or more invoice lines.")

(subclass AmountType DocumentFieldString)
(:hasRestrictedVal AmountType hasReferent CurrencyMeasure)
(documentation AmountType "An AmountType is a string specifying a number of monetary units, as part of a document.")

(subclass LineExtensionAmount AmountType)
(necessarily LineExtensionAmount hasLocation AbstractInvoice)
(documentation LineExtensionAmount "A DocumentFieldString specific to an invoice, stating the total for one line item.  UBL: the monetary amount that is the total for the line item, including any pricing variation (allowances, charges or discounts) but not adjusted by any overall payment settlement discount or taxation. (equals BasePrice multiplied by Quantity).")

(subclass BasePriceAmount AmountType)
(necessarily BasePriceAmount hasLocation BasePriceText)
(:hasRestrictedVal BasePriceAmount hasLocation BasePriceText)
(documentation BasePriceAmount "A DocumentFieldString specific to an invoice, stating the price per unit for one line item.  A unit price.  UBL: specifies the base price.")

(subclass BasePriceText DocumentFieldString)
(necessarily BasePriceText hasLocation TransactionMessage)
(necessarily BasePriceText hasaTextComponent BasePriceAmount)
(documentation BasePriceText "A DocumentFieldString specific to an invoice, stating at least the price per quantity unit for one line item, and potentially other information such as the number of units on which the base price is predicated, maximum and minimum quantities required.  This information may occur in several types of transaction document.  UBL: information that directly relates to a base price for an object.")

(subclass LegalTotalsText DocumentFieldString)
(:hasRestrictedVal LegalTotalsText hasLocation AbstractInvoice)
(necessarily LegalTotalsText hasaTextComponent LineExtensionTotal)
(necessarily LegalTotalsText hasaTextComponent AmountDue)
(documentation LegalTotalsText "One or more amounts of currency due, stated in an invoice in a manner adequate to create a legally binding demand.  Note that in the propositions above, the AmountDue and the LineExtensionTotal may be the same text string.  UBL: Calculated amounts that are required on an invoice for legal purposes. (Note: totals required for taxation purposes are under tax totals). Associates the invoice with a set of totals required for the invoice to be a legal document.  **NOTE** this may be a set of currency amounts, rather than one.  It may be modified depending on clarification - 022504.")

(subclass LineExtensionTotal AmountType)
(:hasRestrictedVal LineExtensionTotal hasLocation LegalTotalsText)
(documentation LineExtensionTotal "The sum of all of the extension amounts in the line items of an invoice.  UBL: the total of line item extension amounts for the entire invoice, but not adjusted by an +R204y payment settlement discount or taxation.")

(subclass AmountDue AmountType)
(:hasRestrictedVal AmountDue hasLocation LegalTotalsText)
(documentation AmountDue "The sum of all of the extension amounts in the line items of an invoice, including taxes.  UBL: the total that is to be paid for this invoice, including all taxes, but not adjusted by any payment settlement discount or possible penalty charges.")

(subclass QuantityType DocumentFieldString)
(:hasRestrictedVal QuantityType hasReferent ConstantQuantity)
(documentation QuantityType "A QuantityType is a string specifying a number of units of objects, potentially including fractions.")

(subclass AmountCurrencyType DocumentFieldString)
(:hasRestrictedVal AmountCurrencyType hasReferent UnitOfCurrency)
(documentation AmountCurrencyType "An AmountCurrencyType is a string specifying a monetary unit.")

(subclass MeasureType DocumentFieldString)
(:hasRestrictedVal MeasureType hasReferent ConstantQuantity)
(documentation MeasureType "An MeasureUnit is a string specifying a unit of measure.")

(subclass MeasureUnitType DocumentFieldString)
(:hasRestrictedVal MeasureUnitType hasReferent UnitOfMeasure)
(documentation MeasureUnitType "An MeasureUnit is a string specifying a unit of measure.")

(subclass QuantityUnitType DocumentFieldString)
(:hasRestrictedVal QuantityUnitType hasReferent UnitOfMeasure)
(documentation QuantityUnitType "A QuantityUnitType is a string specifying some kind of unit of measure.  The UBL specification is vague, and it appears intended to refer to units of items being ordered on a line item, therefore the unit might be any individual object.  But in SUMO concrete objects would not qualify as units of measure.  This needs clarification.")

(subclass RealNumberString AbstractNumericString)
(:hasRestrictedVal RealNumberString hasReferent RealNumber)
(documentation RealNumberString "A RealNumberString is a string of mostly numeric characters that represent a real number.  Certain alphabetical characters may be present in a RealNumberString depending on format, such as in scientific notation 6.023 E23 could represent Avogadro's Number.")

(instance hasaTextComponent BinaryPredicate)
(instance hasaTextComponent TotalValuedRelation)
(domain hasaTextComponent 1 AbstractText)
(domain hasaTextComponent 2 AbstractText)
(documentation hasaTextComponent "hasaTextComponent relates an AbstractText to one or more AbstractTexts contained within the document.  It is a TotalValuedRelation, so every AbstractText has at last one AbstractText in it.  The text component can be as short as one word, e.g. a brief memo or an image with a label.")

;; <axiomgloss> The abstract location of any text component is within the
;; text of which it is a component

(=>
   (hasaTextComponent ?TEXT1  ?TEXT2)
   (hasLocation ?TEXT2 ?TEXT1))

;; <axiomgloss> A physical object representing a text also represents every
;; text component of the full text

(=>
   (and
      (hasaTextComponent ?TEXT1  ?TEXT2)
      (isRepresentedBy ?TEXT1 ?OBJ))
   (isRepresentedBy ?TEXT2 ?OBJ))

;; <axiomgloss> The physical location of an object representing a text component 
;; is within the object representing the text of which it is a component

(=>
   (and
      (hasaTextComponent ?TEXT1  ?TEXT2)
      (isRepresentedBy ?TEXT1 ?OBJ1)
      (isRepresentedBy ?TEXT2 ?OBJ2))
   (located ?OBJ2 ?OBJ1))

;; <module>STANDARDS</module> standards, codes, and identifiers

(subclass AbstractString AbstractText)
(:hasRestrictedval AbstractString isAbstractContentOf SymbolicString)
(:hasRestrictedval AbstractString hasProperPart (UnionFn AbstractCharacter AbstractString))
(hasNecessaryPart AbstractString  AbstractCharacter)
(documentation AbstractString "The abstract notion of a String of abstract character symbols.  This is the abstract equivalent of a SymbolicString, which in SUMO is a physical object.  This concept is the string itself, and is not the referent entity that the string symbolizes.  The abstract String represented by the characters 'cat', for example, may have many concrete references in large numbers of physical documents, but each of these physical strings will represent a single conceptual symbol, the abstract symbol represented by the physical symbol 'cat', which abstract symbol in turn refers to a specific type of physical object.  It does not necessarily refer to a specific physical object, but may refer to the class of cats, depending on the context in which the symbol occurs.")

(subclass Standard Specification)
(:hasRestrictedVal Standard isMaintainedBy StandardsAuthority)
(documentation Standard "A Standard is a set of assertions that, in order to achieve some specified interoperability, an object must be designed with certain properties or must behave in a certain manner.  A standard is promulgated and maintained by a StandardsAuthority, which may be relatively informal such as a group that agrees to do certain things a certain way.")

(subclass NameString Identifier)
(documentation NameString "A NameString is an AbstractString that identifies an entity.  It can be a label for anything.")

(subclass ProperNameString NameString)
(documentation ProperNameString "A ProperNameString is an abstract string which is a name for some individual thing.  It may be composed of more than one component string separated by spaces and sometimes by other characters such as commas.  The named individual may be physical or abstract.  Common things having names are people, places (cities, countries), organizations, books, movies, boats, as well as many others.  The name is not necessarily unique for a named individual, and several individuals may have the same name.  In English a ProperNameString typically has capitals as the initial letters of each component string.")

(subclass AgentNameString ProperNameString)
(necessarily AgentNameString isaNameOf Agent)
(:hasRestrictedVal AgentNameString isaNameOf Agent)
(documentation AgentNameString "A AgentNameString is an abstract string that is a name for some agent, usually a human or an organization - but it could be an animal.")

(subclass PersonNameString ProperNameString)
(necessarily PersonNameString isaNameOf Human)
(:hasRestrictedVal PersonNameString isaNameOf Human)
(documentation PersonNameString "A PersonNameString is any name applied to a human, family name, give nname, nickname, alias, cognoment, etc.")

(subclass OrganizationNameString ProperNameString)
(necessarily OrganizationNameString isaNameOf Organization)
(:hasRestrictedVal OrganizationNameString isaNameOf Organization)
(documentation OrganizationNameString "An OrganizationNameString is an abstract text which is the name of an Organization.")

(subclass CodeListAgencyNameString OrganizationNameString)
(necessarily CodeListAgencyNameString isaNameOf CodeListAgency)
(:hasRestrictedVal CodeListAgencyNameString isaNameOf CodeListAgency)
(documentation CodeListAgencyNameString "A CodeListAgencyNameString is the formal name of a CodeListAgency.  It can be different from an identifier.")

(subclass OpusNameString ProperNameString)
(documentation OpusNameString "An OpusNameString is the name of some form of conceptual work such as a text, a movie, a song, a musical composition.  It is the name of an individual, so that if it is the name of a series, such as a magazine, the individual is the series considered as an instance of a class of SerialPublications.  An abstract serial will have its representation in the instances of physical Series.")

(subclass CodeListName OpusNameString)
(documentation CodeListName "A CodeListName is an abstract string naming a CodeList.")

(subclass CharacterCodeSet AbstractCodeList)
(documentation CharacterCodeSet "A CharacterCodeSet is the set of binary codes, usually an integral number of bytes (1 to 4) that are used in a binary file to represent characters, spaces, and formatting characters in visual representations -- on a computer screen or printout.")

(subclass IdentifierList UniqueList)
(documentation IdentifierList "An IdentifierList is a list of definitions of identifiers, each definition containing an abstract string and a description of what the string is intended to symbolize.  The terms in the list do not need to be unique, though each entry on the list (which is a combination of a term and its definition) does have to be unique.")

(subclass AbstractCodeList IdentifierList)
;; ?(subclass AbstractCodeList TextualProposition)
;;?(necessarily AbstractCodeList subProposition CodeStandard)
(:hasRestrictedVal AbstractCodeList hasListElement CodeDefinition)
(hasListElement AbstractCodeList CodeDefinition)
(documentation AbstractCodeList "An AbstractCodeList is a list of CodeDefinitions (and nothing else), each of which consists of at least one abstract entity, a code string, plus optionally the definition of the code.  The list may have references to the language of the code, and a back-reference to the code list of which it is a part.  That is, a code list consists of a list of codes together with the code definitions, not merely the list of codes.  A full CodeStandard may contain other information in addition to the AbstractCodeList, such as when to apply it and how to expand it.  The code list is an Assertion because it functions as a statement, in effect:
     'In context ?C, symbol ?x shall represent entity ?E'.
Those who agree to this assertion and adopt the code list assent to the truth of this assertion.")

(subclass StandardSpecificationDocument AbstractDocument)
(documentation StandardSpecificationDocument "A StandardSpecificationDocument is a document describing the specification of a standard.   It may contain texts representing lists, such as identifier lists or code lists.")

(instance conformsToSpecification BinaryPredicate)
(domain conformsToSpecification 1 StandardSpecificationDocument)
(domain conformsToSpecification 2 StandardSpecificationSchemeDocument)
(documentation conformsToSpecification "conformsToSpecification  relates a specific abstract text of a specification to the general scheme that describes how to create a specification of that type.")

(subclass StandardSpecificationSchemeDocument AbstractDocument)
(documentation StandardSpecificationSchemeDocument "A StandardSpecificationSchemeDocument is a document describing the manner for creating a standard of a particular type.")

(subclass IdentificationSchemeDocument StandardSpecificationSchemeDocument)
(documentation IdentificationSchemeDocument "An IdentificationSchemeDocument is a document specifying the manner of creating an Identification Scheme.  There may be informal identification schemes without any formal specifications.")

(subclass IdentificationScheme StandardSpecificationDocument)
(necessarily IdentificationScheme hasaTextComponent IdentifierListText)
(necessarily IdentificationScheme hasaTextComponent Identifier)
(documentation IdentificationScheme "An IdentificationScheme is a document specifying a list of unique identifiers, references to the agent that maintains the list, a pointer to the list itself, and potentially other attributes of the list.  There may be informal identification schemes without any formal specifications.")

(subclass CodeListIdentifierString Identifier)
(documentation CodeListIdentifierString "A CodeListIdentifier is an AbstractString which is the identifier for a CodeListTypeText.  URL: The identification of a list of codes.")

(subclass CodeListText IdentifierListText)
(documentation CodeListText "A CodeListText is the abstract text representation of a list of codes, i.e. a list of abstract texts each consisting of a code string and a description of the meaning (and/or usage) of the code string.")

(subclass CodeListTypeText StructuredDocumentText)
(documentation CodeListTypeText "A CodeListTypeText is a document that is not itself a code list nor a complete code specification, but has information about and pointers to a code list and the specification that describes the code list.  This is intended to correspond to the UBL Code.List.Type, but the description of that concept is somewhat ambiguous and this identifaction may change. (PJC 030104).")

(subclass CodeStandardText IdentificationScheme)
(necessarily CodeStandardText hasPropositionalContent CodeStandard)
(:hasRestrictedVal CodeStandardText hasPropositionalContent CodeStandard)
(necessarily CodeStandardText hasaTextComponent CodeListText)
(documentation CodeStandardText "A CodeStandardText is the AbstractText representation of the propositional content of a Code Standard, and has direct correlation with the physical documents that represent the code standard.  The propositional content of the CodeStandardText is contained in a CodeStandard.  This corresponds to UBL Code.List.Scheme.Type.")

(subclass CodeStandard Standard)
(:hasRestrictedVal CodeStandard hasAbstractName CodeListName)
(documentation CodeStandard "The SMINK correlate of UBL CodeList.Type.  This is a text with subtexts, including a SMINK AbstractCodeList, which is the actual list of code symbols and their associated definitions.  A CodeStandard contains other information, such as version and referents to the agency that promulgates or maintains the code standard.")

(subclass CodeDefinition Specification)
(:SYNONYMS CodeDefinition UBLCode)
(necessarily CodeDefinition subProposition CodeStandard)
;;(hasListElement CodeDefinition AbstractCodeString)
(necessarily CodeDefinition usesLanguage AbstractLanguage)
(necessarily CodeDefinition inList AbstractCodeList)
(documentation CodeDefinition "A CodeDefinition is the propositional content of a CodeDefinitionText, consisting of two elements, each of which has a textual representation: (1) an abstract Code, which is an AbstractSymbol referring to some other entity; (2) a proposition definining the code.  The proposition describes the entity to which the code refers (and may describe the circumstances for using the code).  This list is related to two other entities (a) the CodeList which the CodeDefinition is an element of; and (b) the language of the code.  Its implied propositional content is the assertion that these correspondences of code and meaning are required for interoperability among those agents using the code in some activity.")

(subclass IdentifierDefinition Specification)
(necessarily IdentifierDefinition usesLanguage AbstractLanguage)
(necessarily IdentifierDefinition inList IdentifierList)
(documentation IdentifierDefinition "A IdentifierDefinition is the propositional content of a IdentifierDefinitionText, consisting of two elements, each of which has a textual representation: (1) an abstract Identifier, which is an AbstractSymbol referring to some other entity; (2) a proposition defining the Identifier.  The proposition describes the entity to which the Identifier refers (and may describe the circumstances for using the Identifier).  This list is related to two other entities (a) the IdentifierList which the IdentifierDefinition is an element of; and (b) the language of the code.  Its implied propositional content is the assertion that these correspondences of Identifier and meaning are required for interoperability among those agents using the Identifier in some activity.  An Identifier may be defined and used by only a single Agent and is not necessarily part of a formal standard.")

(subclass CodeDefinitionText IdentifierDefinitionText)
(necessarily CodeDefinitionText hasaTextComponent AbstractCodeString)
(necessarily CodeDefinitionText hasaTextComponent CodeNameText)
(necessarily CodeDefinitionText hasPropositionalContent CodeDefinition)
;; (:hasRestrictedVal CodeDefinitionText hasaTextComponent (UnionFn CodeNameText AbstractCodeString))
(documentation CodeDefinitionText "A CodeDefinitionText is a text containing a code string (the AbstractCodeString) and a descriptive text (the CodeNameText) explaining the intended meaning and possibly also usage of the code string.  This is the abstract textual representation of the CodeDefinition, which is a proposition.")

(subclass IdentifierDefinitionText Definition)
(necessarily IdentifierDefinitionText hasaTextComponent Identifier)
(necessarily IdentifierDefinitionText hasaTextComponent IdentifierDefiningText)
(necessarily IdentifierDefinitionText hasPropositionalContent IdentifierDefinition)
(documentation IdentifierDefinitionText "A IdentifierDefinitionText is a text containing an identifier string (the Identifier) and a descriptive text (the IdentifierDefiningText) explaining the intended meaning and possibly also usage of the identifier string.  This is the abstract textual representation of the IdentifierDefinition, which is a proposition.")

(subclass IdentifierDefiningText DescriptiveText)
(documentation IdentifierDefiningText "A IdentifierDefiningText is a text accompanying an Identifier (a string) within a IdentifierDefinitionText, and which contains a definition of the accompanying identifier.")

(subclass IdentifierSchemeProposition Specification)
(documentation IdentifierSchemeProposition "An IdentifierSchemeProposition is the propositional content of an IdentifierListText, which is an abstract text.  This proposition states explicitly or implicitly that a particular list of identifiers should be used in some context or process to achieve some form of interoperability of different information processing activities.  The scheme contains an identifier list, which does not have to be a UniqueList.")

(subclass IdentifierListText StructuredDocumentText)
(necessarily IdentifierListText hasReferent IdentifierList)
(:hasRestrictedVal IdentifierListText hasReferent IdentifierList)
(documentation IdentifierListText "A IdentifierListText is the AbstractText representation of an Identifier List.  There may or may not exist any physical documents that represent the text, except what is in the mind of an individual.")

(subclass CodeNameText DescriptiveText)
(documentation CodeNameText "A CodeNameText is an AbstractText that accompanies a code symbol, and explains in language what the symbol is intended to refer to.")

(subclass AbstractSymbol AbstractInformationalEntity)
(documentation AbstractSymbol "An AbstractSymbol is an abstract representation of a simple object that serves as a symbolic referent to something else.  It can be an abstract image of an icon, an abstract string, or something else non-physical which serves as a referent for another entity.")

(subclass AbstractCode AbstractSymbol)
(:hasRestrictedVal AbstractCode createdBy StandardsAuthority)
(documentation AbstractCode "An AbstractCode is an Abstract Symbol, which can be an AbstractString, that is created and maintained by a coding authority to refer to some other entity.  A word in a natural language may refer to some other entity, but is not a code unless it is designated as such by a coding authority.")

(subclass AbstractCodeString Identifier)
(subclass AbstractCodeString AbstractCode)
;; (necessarily AbstractCodeString inList CodeDefinition)
(documentation AbstractCodeString "An AbstractCode is an Abstract code which is an AbstractString, that is created and maintained by a coding authority to refer to some other entity.  A word in a natural language may refer to some other entity, but is not a code unless it is designated as such by a coding authority.")

(subclass CurrencyCode AbstractCodeString)
(necessarily CurrencyCode isaNameOf UnitOfCurrency)
(:hasRestrictedVal CurrencyCode isaNameOf UnitOfCurrency)
(documentation CurrencyCode "A CurrencyCode is an AbstractString of characters which is the assigned code for some unit of currency on some AbstractCodeList.  For example the abstract string 'USD' may refer to the unit United States Dollar")

(subclass Identifier AbstractString)
(documentation Identifier "An identifier is a String which is intended to provide a unique designation for something.  An identifier can be created by anyone for personal purposes, therefore does not require a coding authority and is not a code.  A thing can have more than one identifier, but one identifier should refer only to one thing.")

(subclass URL Identifier)
(subclass URL AbstractCode)
(:hasRestrictedVal URL isaNameOf FileInternetLocation)
;; ****+++ not really.  it should be a location
(documentation URL "A URL is a String which identifies the location on the Internet where a resource (file) can be found.")

(subclass VersionIdentifier Identifier)
(documentation VersionIdentifier "A VersionIdentifier is an abstract string that is intended to identify a version of something.")

(subclass CodeListVersionIdentifier Identifier)
(:hasRestrictedVal CodeListVersionIdentifier isaNameOf CodeListVersion)
(:hasRestrictedVal CodeListVersionIdentifier hasReferent CodeListVersion)
(documentation CodeListVersionIdentifier "A VersionIdentifier is an abstract string that is intended to identify a CodeStandard.  UBL: 'The Version of the UN/ECE Rec. 9 code list'.")

(subclass TextVersion TextAttribute)
(documentation TextVersion "A TextVersion is an attribute that specifies which version of a text the text is.")

(subclass CodeListVersion TextVersion)
(documentation CodeListVersion "A CodeListVersion is an attribute of a CodeStandard that specifies which version of the CodeStandard the specific text is.")

(instance hasUniqueIdentifier TernaryRelation)
(domain hasUniqueIdentifier 1 Entity)
(domain hasUniqueIdentifier 2 IdentifierList)
(domain hasUniqueIdentifier 3 Identifier)
(documentation hasUniqueIdentifier "hasUniqueIdentifier specifies the name of some entity which is unigue on some list of unique identifiers. The list must be specified, otherwise there is no guarantee that the same identifier is not also used to label some completely different entity.  The IdentifierList is a UniqueList, i.e. the Identifier only occurs once as a term on that list.  Each term ")

(=>
   (and
      (hasUniqueIdentifier ?THING1 ?LIST ?ID)
      (hasUniqueIdentifier ?THING2 ?LIST ?ID))
   (equal ?THING ?THING2))

(=>
   (and
      (hasUniqueIdentifier ?THING ?LIST ?ID1)
      (hasUniqueIdentifier ?THING ?LIST ?ID2))
   (equal ?ID1 ?ID2))

(subclass VersionNumberString VersionIdentifier)
(documentation VersionNumberString "A version number is the number assigned to some copy of an object with propositional content to distinguish it from earlier and somewhat different instances of  related objects of the same name. A version number can contain letters and/or numbers, e.g. '1.2a'.  The relation .hasVersionNumber associates an abstract text of a particular name with its version number; all versions are tagged with the same name")

(instance hasCreationTime BinaryPredicate)
(domain hasCreationTime 1 Physical)
(domain hasCreationTime 2 TimePoint)
(documentation hasCreationTime "hasCreationTime specifies a time position when an entity came into existence. The entity must be physical.")

(=>
    (hasCreationTime ?ENT ?TIMEPOINT)
    (exists (?PROCESS)
        (and
            (instance ?PROCESS Process)
            (equal ?TIMEPOINT (EndFn (WhenFn ?PROCESS)))
            (creates ?PROCESS ?ENT))))

(instance hasVersionNumber TernaryPredicate)
(domain hasVersionNumber 1 AbstractText)
(domain hasVersionNumber 2 OpusNameString)
(domain hasVersionNumber 3 VersionNumberString)
(documentation hasVersionNumber "hasVersionNumber  associates a number with the name of a conceptual work and the abstract text of the work.  Typically there will besome physical embodiment (representation) of the conceptual work which bears the number.  A VersionNumberString may contain letters as well as numbers, but if it is not a real number, at this point (samin011) the time sequence of versions cannot be related to the string.")

;; <axiomgloss> Every version number proposition implies that there is a physical 
;; text representing the abstract text; that physical text was created as a result of a 
;; writing process and that the physical text and abstract text have the same version
;; number 

(=>
    (hasVersionNumber ?ABSTEXT ?NAME ?NUMBER)
     (exists (?PHYSTEXT ?WRITING ?TIMEPOINT ?PHYSSTRING)
            (and 
                 (instance ?PHYSTEXT Text)
                 (isRepresentedBy ?ABSTEXT ?PHYSTEXT)
                 (isRepresentedBy ?NAME ?PHYSSTRING)
                 (titles ?PHYSSTRING ?PHYSTEXT)
                 (instance ?WRITING Writing)
                 (creates ?WRITING ?PHYSTEXT)
                 (instance ?TIMEPOINT TimePoint)
                 (hasCreationTime ?PHYSTEXT ?TIMEPOINT)
                 (equal ?TIMEPOINT (Endfn (WhenFn ?WRITING)))
                 (isaNameOf ?NAME ?ABSTEXT)
                 (isaNameOf ?NAME ?PHYSTEXT)
                 (hasVersionNumber ?PHYSTEXT ?NAME ?NUMBER))))

;; <axiomgloss> If a version number proposition references a real number, then
;;  the version with the higher number was created after the version with the 
;; lower number.  This assumes that every entity with the same name is a 
;; version of another entity with the same name.  This will only be true
;; if version number uses some unique naming scheme, which needs to be specified

(=>
   (and
       (hasVersionNumber ?ABSTEXT ?NAME ?NUMBER1)
       (hasVersionNumber ?ABSTEXT2 ?NAME ?NUMBER2)
        (instance ?NUMBER RealNumberString)
        (instance ?NUMBER2 RealNumberString)
        (hasReferent ?NUMBER1 ?REALNUM1)
        (hasReferent ?NUMBER2 ?REALNUM2)
        (greaterThan ?REALNUM2 ?REALNUM1))
     (exists (?TIME1 ?TIME2)
            (and
                 (hasCreationTime ?ABSTEXT ?TIME1)
                 (hasCreationTime ?ABSTEXT2 ?TIME2)
                 (before ?TIME1 ?TIME2))))

(subclass AttributeValueString AbstractString)
(necessarily AttributeValueString hasReferent Attribute)
(documentation AttributeValueString "An AttributeValueString is a subclass of AbstractString which includes those strings that refer to values of Attributes (instances of the class 'Attribute').")

;; NOTE - the axiomatization of the notion of an exhaustive list
;; has not been done here yet (samni011).

(subclass AttributeList List)
(documentation AttributeList "An AttributeList is an exhaustive list of all possible values of a particular attribute.  The values are not necessarily disjoint or contradictory.")

(=>
   (and
     (instance ?ALIST AttributeList)
     (inList ?INST ?ALIST))
   (instance ?INST Attribute))

(subclass BinaryList List)
(documentation BinaryList "A BinaryList is a list having exactly two elements.")

(=>
   (instance ?BINLIST BinaryList)
   (equal (CardinalityFn (KappaFn ?THING (inList ?THING ?BINLIST))) 2))

(subclass IndicatorList AttributeList)
(subclass IndicatorList BinaryList)
(documentation IndicatorList "A IndicatorList is an AttributeList list having exactly two elements, each of which is an Attribute.  This corresponds to the UBL Indicator.Type.  UBL: A list of two mutually exclusive Boolean values that express the only possible states of a Property.")

;; <axiomgloss>If an entity possesses one of the two attributes in
;;  an IndicatorList, it can not possess the other attribute in that list.

(=>
   (and
      (property ?THING ?ATTRIB1)
      (instance ?LIST IndicatorList)
      (inList ?ATTRIB1 ?LIST))
   (exists (?ATTRIB2)
      (and
         (inList ?ATTRIB2 ?LIST)
         (not (equal ?ATTRIB2 ?ATTRIB1))
         (not (property ?THING ?ATTRIB2)))))

;; <module>UBL-Synonyms</module>

;; <assertion>

(instance IndicatorContent :UBL-Synonym)
(:synonymousTerm IndicatorContent "Indicator Content")
(:inLanguage IndicatorContent UBLv10)
(:hasFrequency IndicatorContent 1.0)
(:SUO-name IndicatorContent "Indicator Content")
(:isaSynonymOf IndicatorContent AttributeValueString)
(documentation IndicatorContent "The UBL equivalent of SMINK AttributeValueString.  This is an AbstractString referring to an instance value of some Attribute.")

(instance UblIndicator :UBL-Synonym)
(:synonymousTerm UblIndicator "Indicator Type")
(:inLanguage UblIndicator UBLv10)
(:hasFrequency UblIndicator 1.0)
(:SUO-name UblIndicator "Indicator Type")
(:isaSynonymOf UblIndicator IndicatorList)
(documentation UblIndicator "The UBL equivalent of SMINK IndicatorList.  This is a list containing exactly two Attributes, which represent alternative possible values of an Attribute.  UBL: A list of two mutually exclusive Boolean values that express the only possible states of a Property.")

(instance UblLanguage :UBL-Synonym)
(:synonymousTerm UblLanguage "Language Type")
(:inLanguage UblLanguage UBLv10)
(:hasFrequency UblLanguage 1.0)
(:SUO-name UblLanguage "Language Type")
(:isaSynonymOf UblLanguage AbstractLanguage)
(documentation UblLanguage "The UBL equivalent of SMINK AbstractLanguage.  This is an abstract notion of language, corresponding to the SUMO term 'Language', which is a Physical entity.  It can be a human or artificial language")

(instance CodeListAgencyName :UBL-Synonym)
(:synonymousTerm CodeListAgencyName "CodeList Agency Name Text")
(:inLanguage CodeListAgencyName UBLv10)
(:hasFrequency CodeListAgencyName 1.0)
(:SUO-name CodeListAgencyName "CodeList Agency Name Text")
(:isaSynonymOf CodeListAgencyName CodeListAgencyNameString)
(documentation CodeListAgencyName "The UBL equivalent of SMINK CodeListAgencyNameString.  This is the textual representation of a formal name of a CodeListAgency.  It can be different from an identifier.  UBL: The name of the agency that maintains the code list.")

(instance IdentificationSchemeData :UBL-Synonym)
(:synonymousTerm IdentificationSchemeData "Identification Scheme Data Type")
(:inLanguage IdentificationSchemeData UBLv10)
(:hasFrequency IdentificationSchemeData 1.0)
(:SUO-name IdentificationSchemeData "Identification Scheme Data Type")
(:isaSynonymOf IdentificationSchemeData IdentifierListText)
(documentation IdentificationSchemeData "The UBL equivalent of SMINK IdentifierListText.  This is the textual representation of a list of identifiers, a parent class of CodeListText.  The abstract textual representation may have a URI because it may be in the form of a computer file, but it is not a physical object.")

(instance IdentificationSchemeType :UBL-Synonym)
(:synonymousTerm IdentificationSchemeType "Identification Scheme Type")
(:inLanguage IdentificationSchemeType UBLv10)
(:hasFrequency IdentificationSchemeType 1.0)
(:SUO-name IdentificationSchemeType "Identification Scheme Type")
(:isaSynonymOf IdentificationSchemeType IdentificationScheme)
(documentation IdentificationSchemeType "The UBL equivalent of SMINK IdentificationScheme.  This is the textual representation of an identification Scheme, a parent of CodeStandardText.  The abstract textual representation may have a URI because it may be in the form of a computer file, but it is not a physical object.")

(instance CodeListScheme :UBL-Synonym)
(:synonymousTerm CodeListScheme "Code List Scheme")
(:inLanguage CodeListScheme UBLv10)
(:hasFrequency CodeListScheme 1.0)
(:SUO-name CodeListScheme "Code List Scheme")
(:isaSynonymOf CodeListScheme CodeStandardText)
(documentation CodeListScheme "The UBL equivalent of SMINK CodeStandardText.  This is the textual representation of a CodeStandard, which is a proposition describing a CodeList.  The textual representation may have a URI because it may be in the form of a computer file, but it is not a physical object.")

(instance UblLegalTotals :UBL-Synonym)
(:synonymousTerm UblLegalTotals "Legal Totals Details")
(:inLanguage UblLegalTotals UBLv10)
(:hasFrequency UblLegalTotals 1.0)
(:SUO-name UblLegalTotals "Legal Totals Details")
(:isaSynonymOf UblLegalTotals LegalTotalsText)
(documentation UblLegalTotals "The UBL equivalent of SMINK LegalTotalsText.  Instances of UblDateTime are text fields in an invoice that specify amounts owed before and after taxes are included.  UBL: calculated amounts that are required on an invoice for legal purposes. (Note: totals required for taxation purposes are under tax totals).")

(instance UblExtensionTotal :UBL-Synonym)
(:synonymousTerm UblExtensionTotal "Extension Total")
(:inLanguage UblExtensionTotal UBLv10)
(:hasFrequency UblExtensionTotal 1.0)
(:SUO-name UblExtensionTotal "Extension Total")
(:isaSynonymOf UblExtensionTotal LineExtensionTotal)
(documentation UblExtensionTotal "The UBL equivalent of SMINK LineExtensionTotal.  Instances of UblExtensionTotal are abstract strings that represent an amount of currency equal to the total of all the extensions in the line items of an invoice.  UBL: the total of line item extension amounts for the entire invoice, but not adjusted by an +R204y payment settlement discount or taxation")

(instance UblToBePaid :UBL-Synonym)
(:synonymousTerm UblToBePaid "To Be Paid")
(:inLanguage UblToBePaid UBLv10)
(:hasFrequency UblToBePaid 1.0)
(:SUO-name UblToBePaid "To Be Paid")
(:isaSynonymOf UblToBePaid AmountDue)
(documentation UblToBePaid "The UBL equivalent of SMINK AmountDue.  Instances of UblToBePaid are abstract strings that represent a total amount to be paid which includes taxes.  UBL: the total that is to be paid for this invoice, including all taxes, but not adjusted by any payment settlement discount or possible penalty charges.")

(instance UblDateTime :UBL-Synonym)
(:synonymousTerm UblDateTime "DateTime Type")
(:inLanguage UblDateTime UBLv10)
(:hasFrequency UblDateTime 1.0)
(:SUO-name UblDateTime "DateTime Type")
(:isaSynonymOf UblDateTime AbstractDateTimeText)
(documentation UblDateTime "The UBL equivalent of SMINK AbstractDateTimeText.  Instances of UblDateTime are abstract strings that represent a date and possibly a time.  They may be in any of a variety of specified formats.")

(instance UblNumeric :UBL-Synonym)
(:synonymousTerm UblNumeric "Numeric Type")
(:inLanguage UblNumeric UBLv10)
(:hasFrequency UblNumeric 1.0)
(:SUO-name UblNumeric "Numeric Type")
(:isaSynonymOf UblNumeric AbstractNumericString)
(documentation UblNumeric "The UBL equivalent of SMINK AbstractNumericString.  AbstractNumericString is a class of abstract texts that represent a number but may have certain specified properties corresponding to the UBL Numeric Type.  Specifically, it can be in any format and the format may be specified on instances of this class..")

(instance UblNumericContent :UBL-Synonym)
(:synonymousTerm UblNumericContent "Numeric Content")
(:inLanguage UblNumericContent UBLv10)
(:hasFrequency UblNumericContent 1.0)
(:SUO-name UblNumericContent "Numeric Content")
(:isaSynonymOf UblNumericContent AbstractNumericString)
(documentation UblNumericContent "The UBL equivalent of SMINK Number.  This is the conceptual content of a number but it necessarily has a representation as anAbstractNumbericString.  The string representing it can be in any format and the format may be specified on instances of the physical or abstract class which represent it.")

(instance UblQuantityUnit :UBL-Synonym)
(:synonymousTerm UblQuantityUnit "Quantity Unit Type")
(:inLanguage UblQuantityUnit UBLv10)
(:hasFrequency UblQuantityUnit 1.0)
(:SUO-name UblQuantityUnit "Quantity Unit Type")
(:isaSynonymOf UblQuantityUnit QuantityUnitType)
(documentation UblQuantityUnit "The UBL equivalent of SMINK UnitOfMeasure.  This is indistinguishable from the semantics of UBL Measure Unit Type, and differs only by being applied to the Quantity field in a line item of an Order or Invoice, which raises the question of whether such a unit could be an individual object, rather than a traditional unit of measure.  It is the most general UBL type for units of measure unit of measure.  It is intended in UBL to be a quantity specifying the number of items of a particular kind that are ordered, usually determined by counting, distinct from numbers determined by a measurement, and distinct from currency measures.")

(instance UblMeasureUnit :UBL-Synonym)
(:synonymousTerm UblMeasureUnit "Measure Unit Type")
(:inLanguage UblMeasureUnit UBLv10)
(:hasFrequency UblMeasureUnit 1.0)
(:SUO-name UblMeasureUnit "Measure Unit Type")
(:isaSynonymOf UblMeasureUnit MeasureUnitType)
(documentation UblMeasureUnit "The UBL equivalent of SMINK UnitOfMeasure.  This is the most general UBL type for units of measure unit of measure.  It is intended in UBL to be a quantity determined by a measurement, distinct from counting and distinct from currency measures.  It is almost indistinguishable from the semantics of UBL Quantity Unit Type ")

(instance UblMeasure :UBL-Synonym)
(:synonymousTerm UblMeasure "Measure Type")
(:inLanguage UblMeasure UBLv10)
(:hasFrequency UblMeasure 1.0)
(:SUO-name UblMeasure "Measure Type")
(:isaSynonymOf UblMeasure MeasureType)
(documentation UblMeasure "The UBL equivalent of SMINK MeasureType.  This is a string which is the most general UBL type that measures things numerically, as a combination of a real number and a unit of measure.  UBL: A numeric value determined by measuring an object along with the specified unit of measure.")

(instance AmountContent :UBL-Synonym)
(:synonymousTerm AmountContent "Amount Content")
(:inLanguage AmountContent UBLv10)
(:hasFrequency AmountContent 1.0)
(:SUO-name AmountContent "Amount Content")
(:isaSynonymOf AmountContent RealNumberString)
(documentation AmountContent "The UBL equivalent of SMINK RealNumberString, restricted to use with Monetary units.  UBL: A  number of monetary units specified in a currency where the unit of currency is explicit or implied.")

(instance MeasureContent :UBL-Synonym)
(:synonymousTerm MeasureContent "Measure Content")
(:inLanguage MeasureContent UBLv10)
(:hasFrequency MeasureContent 1.0)
(:SUO-name MeasureContent "Measure Content")
(:isaSynonymOf MeasureContent RealNumberString)
(documentation MeasureContent "The UBL equivalent of SMINK RealNumberString,  use with the most general types of measure.  UBL: The numeric value determined by measuring an object.")

(instance QuantityContent :UBL-Synonym)
(:synonymousTerm QuantityContent "Quantity Content")
(:inLanguage QuantityContent UBLv10)
(:hasFrequency QuantityContent 1.0)
(:SUO-name QuantityContent "Quantity Content")
(:isaSynonymOf QuantityContent RealNumberString)
(documentation QuantityContent "The UBL equivalent of SMINK RealNumberString, restricted to use with Monetary units.  UBL: A counted number of non-monetary units possibly including fractions.")

(instance TextType :UBL-Synonym)
(:synonymousTerm TextType "Text Type")
(:inLanguage TextType UBLv10)
(:hasFrequency TextType 1.0)
(:SUO-name TextType "Text Type")
(:isaSynonymOf TextType DocumentFieldString)
(documentation TextType "The UBL equivalent of SMINK DocumentFieldString, restricted to use with Monetary units.  UBL: A character string (i.e. a finite set of characters) generally in the form of words of a language.")

(instance UBLAmount :UBL-Synonym)
(:synonymousTerm UBLAmount "Amount Type")
(:inLanguage UBLAmount UBLv10)
(:hasFrequency UBLAmount 1.0)
(:SUO-name UBLAmount "Amount Type")
(:isaSynonymOf UBLAmount AmountType)
(documentation UBLAmount "The UBL equivalent of SMINK AmountType.  This is refers to SUMO CurrencyMeasure because according to the UBL specification, the unit of currency may only be implied rather than explicitly represented in the physical embodiments of this abstract concept.
The 'Content' (quantifier) of this UBL concept is a rational number rather than a real number because the number of decimal digits will in general be restricted.  In invoices, it is unlikely that more than 3 decimal digits will appear in a price.  UBL: A number of monetary units specified in a currency where the unit of currency is explicit or implied.")

(instance UBLBasePrice :UBL-Synonym)
(:synonymousTerm UBLBasePrice "Base Price Amount")
(:inLanguage UBLBasePrice UBLv10)
(:hasFrequency UBLBasePrice 1.0)
(:SUO-name UBLBasePrice "Base Price Amount")
(:isaSynonymOf UBLBasePrice BasePriceAmount)
(documentation UBLBasePrice "The UBL equivalent of SMINK BasePriceAmount.  This  refers to SUMO CurrencyMeasure.  A DocumentFieldString specific to an invoice, stating the price per unit for one line item.  A unit price.  UBL: specifies the base price.")

(instance UBLExtensionAmount :UBL-Synonym)
(:synonymousTerm UBLExtensionAmount "Line Extension Amount")
(:inLanguage UBLExtensionAmount UBLv10)
(:hasFrequency UBLExtensionAmount 1.0)
(:SUO-name UBLExtensionAmount "Line Extension Amount")
(:isaSynonymOf UBLExtensionAmount LineExtensionAmount)
(documentation UBLExtensionAmount "The UBL equivalent of SMINK LineExtensionAmount.  This is refers to SUMO CurrencyMeasure.  UBL: the monetary amount that is the total for the line item, including any pricing variation (allowances, charges or discounts) but not adjusted by any overall payment settlement discount or taxation. (equals BasePrice multiplied by Quantity).")

(instance UBLLegalTotals :UBL-Synonym)
(:synonymousTerm UBLLegalTotals "Legal Totals")
(:inLanguage UBLLegalTotals UBLv10)
(:hasFrequency UBLLegalTotals 1.0)
(:SUO-name UBLLegalTotals "Legal Totals")
(:isaSynonymOf UBLLegalTotals LegalTotalsText)
(documentation UBLLegalTotals "The UBL equivalent of SMINK LegalTotalsText.  This refers to one or more instances of SUMO CurrencyMeasure, being the currency amounts required to make an invoice legally binding.  UBL: associates the invoice with a set of totals required for the invoice to be a legal document.")

(instance UBLCurrencyType :UBL-Synonym)
(:synonymousTerm UBLCurrencyType "Amount Currency Type")
(:inLanguage UBLCurrencyType UBLv10)
(:hasFrequency UBLCurrencyType 1.0)
(:SUO-name UBLCurrencyType "Amount Currency Type")
(:isaSynonymOf UBLCurrencyType AmountCurrencyType)
(documentation UBLCurrencyType "The UBL equivalent of SMINK AmountCurrencyType, which refers to a UnitOfCurrency.  UBL: A currency used in Amount instances.")

(instance UBLQuantity :UBL-Synonym)
(:synonymousTerm UBLQuantity "Quantity Type")
(:inLanguage UBLQuantity UBLv10)
(:hasFrequency UBLQuantity 1.0)
(:SUO-name UBLQuantity "Quantity Type")
(:isaSynonymOf UBLQuantity QuantityType)
(documentation UBLQuantity "The UBL equivalent of SMINK ConstantQuantity.  UBL: A counted number of non-monetary units possibly including fractions.")

(instance UBLBinObject :UBL-Synonym)
(:synonymousTerm UBLBinObject "Binary Object")
(:inLanguage UBLBinObject UBLv10)
(:hasFrequency UBLBinObject 1.0)
(:SUO-name UBLBinObject "Binary Object")
(:isaSynonymOf UBLBinObject BinaryObject)
(documentation UBLBinObject "The UBL equivalent of SMINK BinaryObject.  UBL: A set of finite-length sequences of binary octets.")

(instance UBLIdentifierContent :UBL-Synonym)
(:synonymousTerm UBLIdentifierContent "Identifier Content")
(:inLanguage UBLIdentifierContent UBLv10)
(:hasFrequency UBLIdentifierContent 1.0)
(:SUO-name UBLIdentifierContent "Identifier Content")
(:isaSynonymOf UBLIdentifierContent Identifier)
(documentation UBLIdentifierContent "The UBL equivalent of SMINK Identifier, the string of characters constituting a term used as an abbreviation for a longer description of a concept.  This is equivalent to UBL Identifier.Content.  UBL: A character string to identify and distinguish uniquely, one instance of an object in an identification scheme from all other objects within the same scheme.")

(instance UBLIdentifierType :UBL-Synonym)
(:synonymousTerm UBLIdentifierType "Identifier Type")
(:inLanguage UBLIdentifierType UBLv10)
(:hasFrequency UBLIdentifierType 1.0)
(:SUO-name UBLIdentifierType "Identifier Type")
(:isaSynonymOf UBLIdentifierType IdentifierDefinitionText)
(documentation UBLIdentifierType "The UBL equivalent of SMINK IdentifierDefinitionText, which has two text components: an Identifier, the string of characters constituting a term used as an abbreviation for a longer description of a concept; and the IdentifierDefinitionText, a descriptive text explaining the intended meaning and referents of the term.  This is equivalent to UBL Identifier.Type.  UBL: A character string to identify and distinguish uniquely, one instance of an object in an identification scheme from all other objects in the same scheme, together with relevant supplementary information.")

(instance CodeListIdentifier :UBL-Synonym)
(:synonymousTerm CodeListIdentifier "CodeList Identifier")
(:inLanguage CodeListIdentifier UBLv10)
(:hasFrequency CodeListIdentifier 1.0)
(:SUO-name CodeListIdentifier "CodeList Identifier")
(:isaSynonymOf CodeListIdentifier CodeListIdentifierString)
(documentation CodeListIdentifier "The UBL equivalent of SMINK CodeListIdentifierString, a brief string which is an identifier of a CodeListTypeText (UBL CodeList.Type). UBL: The identification of a list of codes.")

(instance UBLCode :UBL-Synonym)
(:synonymousTerm UBLCode "Code")
(:inLanguage UBLCode UBLv10)
(:hasFrequency UBLCode 1.0)
(:SUO-name UBLCode "Code")
(documentation UBLCode "The UBL equivalent of SMINK CodeDefinition.  UBL: A character String (letters, figures, and symbols) that for brevity and/or language independence may be used to represent or replace a definitive value or text of an Attribute together with relevant supplementary information.")

(instance UBLCodeContent :UBL-Synonym)
(:synonymousTerm UBLCodeContent "Code Content")
(:inLanguage UBLCodeContent UBLv10)
(:hasFrequency UBLCodeContent 1.0)
(:SUO-name UBLCodeContent "Code Content")
(:isaSynonymOf UBLCodeContent AbstractCodeString)
(documentation UBLCodeContent "The UBL equivalent of SMINK AbstractCodeString, the string of characters consituting the code used as an abbreviation for a longer description of a concept.  This is a necessary component of a UBL Code, in SMINK represented by CodeDefinition.")

(instance UBLCodeListType :UBL-Synonym)
(:synonymousTerm UBLCodeListType "Code List Type")
(:inLanguage UBLCodeListType UBLv10)
(:hasFrequency UBLCodeListType 1.0)
(:SUO-name UBLCodeListType "Code List Type")
(:isaSynonymOf UBLCodeListType CodeListTypeText)
(documentation UBLCodeListType "The UBL equivalent of SMINK CodeListTypeText, a subclass of text that refers to a CodeListText and a CodeListScheme, both abstract text, referring to a CodeStandard,a proposition.  A CodeListTypeText may or may not include the actual list of codes or a pointer to its location, plus ancillary information, such as the maintaining Code Authority, the conditions of use, the manner for changing the code, etc.")

(instance CodeListVersionID :UBL-Synonym)
(:synonymousTerm CodeListVersionID "Code List Version ID")
(:inLanguage CodeListVersionID UBLv10)
(:hasFrequency CodeListVersionID 1.0)
(:SUO-name CodeListVersionID "Code List Version ID")
(:isaSynonymOf CodeListVersionID CodeListVersionIdentifier)
(documentation CodeListVersionID "The UBL equivalent of SMINK CodeListVersionIdentifier.  This is the version ID (an abstract string) assigned to a specific version of a CodeStandard by the maintaining Code Authority.")

(instance CurrencyCodeID :UBL-Synonym)
(:synonymousTerm CurrencyCodeID "Amount Currency Identifier")
(:inLanguage CurrencyCodeID UBLv10)
(:hasFrequency CurrencyCodeID 1.0)
(:SUO-name CurrencyCodeID "Amount Currency Identifier")
(:isaSynonymOf CurrencyCodeID CurrencyCode)
(documentation CurrencyCodeID "The UBL equivalent of SMINK CurrencyCodeID.  This is the abstract text of a currency code (an abstract string) assigned by a standards authority to represent a unit of currency, such as 'USD' for United States Dollar.")

(instance CodeAgencyID :UBL-Synonym)
(:synonymousTerm CodeAgencyID "Agency Identifier")
(:inLanguage CodeAgencyID UBLv10)
(:hasFrequency CodeAgencyID 1.0)
(:SUO-name CodeAgencyID "Agency Identifier")
(:isaSynonymOf CodeAgencyID CodeListAgencyIdentifier)
(documentation CodeAgencyID "The UBL equivalent of SMINK CodeListAgencyIdentifier.  This is the abstract string that serves to identify a CodeList Agency, in some identification scheme.  UBL: 'An agency that maintains one or more code lists'.")

(subclass PhysicalEvent Physical)
;;(subclass PhysicalEvent Event)
(documentation PhysicalEvent "A PhysicalEvent is an Event that occurs in SpaceTime, involving PhysicalObjects.  It is the total collection of all changes in a physical state occurring during the operation of some PhysicalProcess.")

;; </assertion>

(instance isRepresentedBy BinaryPredicate)
(instance isRepresentedBy AsymmetricRelation)
(instance isRepresentedBy IrreflexiveRelation)
(instance isRepresentedBy PartialValuedRelation)
;; (inverse isRepresentedBy containsInformation)
(domain isRepresentedBy 1 Abstract)
(domain isRepresentedBy 2 ContentBearingObject)
(documentation isRepresentedBy "isRepresentedBy relates a Proposition to one or more ContentBearingObjects (physical objects) that express that proposition.  The object may be a Group of objects, such as a set of identical books.")

(instance isaNameOf BinaryPredicate)
(instance isaNameOf TotalValuedRelation)
(domain isaNameOf 1 Identifier)
(domain isaNameOf 2 Entity)
(subrelation isaNameOf hasReferent)
(relatedInternalConcept isaNameOf names)
(documentation isaNameOf "isaNameOf is the relation between a string intended to identify something and the thing it identifies.  This is the analogous relation for AbstractStrings to 'names' which relates physical symbols to their referents.  The same name may apply to an abstract text and to the physical object that represents the text.")

(=>
   (isRepresentedBy ?ABSTEXT ?PHYSTEXT)
   (<=>
      (isaNameOf ?NAME ?ABSTEXT)
      (isaNameOf ?NAME ?PHYSTEXT)))

(instance isTheInformationContainedIn BinaryRelation)
(domain isTheInformationContainedIn 1 Proposition)
(domain isTheInformationContainedIn 2 ContentBearingObject)
(inverse isTheInformationContainedIn containsInformation)
(:isRestrictedTo isTheInformationContainedIn TextualProposition Text)
(relatedInternalConcept isTheInformationContainedIn isRepresentedBy)
(documentation isTheInformationContainedIn "isTheInformationContainedIn is the relation between a Proposition and the ContentBearingObject that represents it.")

(instance hasPropositionalContent BinaryRelation)
(domain hasPropositionalContent 1 AbstractText)
(domain hasPropositionalContent 2 TextualProposition)
(inverse hasPropositionalContent hasTextualExpression)
(relatedInternalConcept hasPropositionalContent isRepresentedBy)
(documentation hasPropositionalContent "hasPropositionalContent is the relation between an AbstractText and the Proposition it represents.")

(instance hasTextualExpression BinaryRelation)
(domain hasTextualExpression 1 TextualProposition)
(domain hasTextualExpression 2 AbstractText)
(inverse hasTextualExpression hasPropositionalContent)
(relatedInternalConcept hasTextualExpression isRepresentedBy)
(documentation hasTextualExpression "hasTextualExpression is the relation between a Proposition and the AbstractText that represents it.")

(instance isReferencedBy BinaryPredicate)
(domain isReferencedBy 1 Entity)
(domain isReferencedBy 2 AbstractInformationalEntity)
(documentation isReferencedBy "isReferencedBy  relates any entity to an AbstractInformationalEntity (such as an Abstract Text) that represents the entity in some way.  This is the inverse of relation 'hasReferent'. Thus a physical cat may be represented by the AbstractString 'c-a-t'.")

(instance hasReferent BinaryPredicate)
(domain hasReferent 1 AbstractInformationalEntity)
(domain hasReferent 2 Entity)
(inverse hasReferent isReferencedBy)
(documentation hasReferent "hasReferent relates an AbstractInformationalEntity to an entity, physical or abstract, to which the abstract informational entity refers.  This the abstract string 'cat' may refer to a physical cat.")

(instance hasAbstractContent BinaryPredicate)
(domain hasAbstractContent 1 ContentBearingObject)
(domain hasAbstractContent 2 AbstractText)
(inverse hasAbstractContent isAbstractContentOf)
(subrelation hasAbstractContent isReferencedBy)
(relatedInternalConcept hasAbstractContent containsInformation)
(documentation hasAbstractContent "hasAbstractContent relates a physical object haing information content to the abstract entity that the physical object represents.  This differs from the SUMO containsInformation' only in that the SUMO relation has Proposition as its domain 2 and this relation has AbstractInformationalEntity, which is not necessarily a Proposition, e.g. it could be an AbstractString.")

(instance isAbstractContentOf BinaryPredicate)
(domain isAbstractContentOf 1 AbstractLinguisticExpression)
(domain isAbstractContentOf 2 ContentBearingObject)
(inverse isAbstractContentOf hasAbstractContent)
(documentation isAbstractContentOf "isAbstractContentOf relates some information content to a physical object which represents that content.  This is the inverse of hasAbstractContent, which see.  See also 'hasInformationalContent'.")


;; <module>EXTENSIONS</module>
;; EXTENSIONS was added Feb-23-04 to be a place where concepts can be added
;;  that are not immediately required for work in progress, and can be
;;  easily removed to allow viewing a simplified ontology.  This module and all
;;  after it should be able to be deleted from the SKIF ontology file
;;  without causing any logical inconsistencies

(subclass BadSituation SomethingOfNegativeValue)
(subclass BadSituation Situation)
(documentation BadSituation "A BadSituation is any state, process, or event the as negative value for a CognitiveAgent.  A BadSituation can be the consequence of certain events or processes,in which case a knowledgeable CognitiveAgent will try to avoid such events or processes unless they also have counterbalancing positive consequences.")


;; <axgloss>every BadSituation is bad for some CognitiveAgent</axgloss>

(=>
   (instance ?BadSit BadSituation)
   (and
      (exists ?AGENT
         (instance ?AGENT CognitiveAgent)))
      (isBadForAgent ?BadSit ?AGENT))

;; <axgloss>Any non-zero probability of a BadSituation is also a 
;;  BadSituation</axgloss>

(=>
    (and
       (instance ?Sit Situation)
       (instance ?BadSit BadSituation)
       (holdsInContext (greaterThan (ProbabilityFn (exists ?BadSit)) 0) ?SIT))
    (instance ?BadSit BadSituation))

;; Mental Entities

(instance hasInformationalContent BinaryRelation)
(domain hasInformationalContent 1 MentalEntity)
(domain hasInformationalContent 2 (UnionFn AbstractInformationalEntity Proposition))
(:isRestrictedTo hasInformationalContent MentalText AbstractText)
(relatedInternalConcept hasInformationalContent represents)
(documentation hasInformationalContent "hasInformationalContent is the relation between a Proposition and the corresponding mental entity.")

(instance isTheInformationalContentOf BinaryRelation)
(domain isTheInformationalContentOf 1 AbstractInformationalEntity)
(domain isTheInformationalContentOf 2 MentalEntity)
(inverse isTheInformationalContentOf hasInformationalContent)
(:isRestrictedTo isTheInformationalContentOf AbstractText MentalText)
(relatedInternalConcept isTheInformationalContentOf isRepresentedBy)
(documentation isTheInformationalContentOf "isTheInformationalContentOf is the relation between a Proposition and the MentalEntity it corresponds to.  The entity can be abstract or concrete.")

(instance hasMentalRepresentation BinaryRelation)
(domain hasMentalRepresentation 1 Physical)
(domain hasMentalRepresentation 2 MentalEntity)
(inverse hasMentalRepresentation isTheMentalRepresentationOf)
(:isRestrictedTo hasMentalRepresentation LinguisticExpression MentalLinguisticExpression)
(relatedInternalConcept hasMentalRepresentation isRepresentedBy)
(relatedInternalConcept hasMentalRepresentation hasAbstractContent)
(documentation hasMentalRepresentation "hasMentalRepresentation is the relation between a Physical Entity and the MentalEntity that represents it.")

(instance isTheMentalRepresentationOf BinaryRelation)
(domain isTheMentalRepresentationOf 1 MentalEntity)
(domain isTheMentalRepresentationOf 2 Physical)
(inverse isTheMentalRepresentationOf hasMentalRepresentation)
(:isRestrictedTo isTheMentalRepresentationOf MentalLinguisticExpression LinguisticExpression)
(relatedInternalConcept isTheMentalRepresentationOf represents)
(documentation isTheMentalRepresentationOf "hasMentalRepresentation is the relation between a MentalEntity and the Physical entity that represents it.")

(subclass MentalEntity Entity)
(disjoint MentalEntity Physical)
(documentation MentalEntity "An entity which is created by a CognitiveAgent and is not a physical entity.  The important distinction of MentalEntity from Abstract entities is that a MentalEntity can be conceived of as existing in Time, such as a law that may exist for a while, then be repealed.  It can thus have temporal existence, but is not a physical object.")

;; every MentalEntity was created by some CognitiveAgent

(=>
   (instance ?M MentalEntity)
   (exists (?AGENT)
     (and
         (instance ?AGENT CognitiveAgent)
         (createdBy ?M ?AGENT))))

(subclass MentalContent MentalEntity)
(:hasRestrictedVal MentalContent isTheMentalRepresentationOf ContentBearingObject)
(documentation MentalContent "MentalContent is the MentalEntity that is represented by a ContentBearingObject and has a Proposition as its propositional content.")

(subclass MentalLinguisticExpression MentalContent)
(:hasRestrictedVal MentalLinguisticExpression hasInformationalContent AbstractLinguisticExpression)
(:hasRestrictedVal MentalLinguisticExpression isTheMentalRepresentationOf LinguisticExpression)
(documentation MentalLinguisticExpression "A MentalLinguisticExpression is the MentalEntity that is represented by a LinguisticExpression and has an AbstractLinguisticExpression as its propositional content.")

(subclass MentalString MentalText)
(:hasRestrictedVal MentalString hasInformationalContent AbstractString)
(:hasRestrictedVal MentalString isTheMentalRepresentationOf SymbolicString)
(documentation MentalString "A MentalString is a MentalEntity that is represented by a physical string of characters 'SymbolicString' in SUMO, and has abstract content that may be a proposition or something simpler.")

(subclass MentalDocument MentalText)
(:hasRestrictedVal MentalDocument hasInformationalContent AbstractDocument)
(:hasRestrictedVal MentalDocument isTheMentalRepresentationOf DocumentPhysical)
(documentation MentalDocument "A MentalDocument is a MentalEntity that is represented by a physical document, which is a type of Text in SUMO.")

(subclass MentalText MentalLinguisticExpression)
(:hasRestrictedVal MentalText hasInformationalContent AbstractText)
(:hasRestrictedVal MentalText isTheMentalRepresentationOf Text)
(documentation MentalText "A MentalText is the mental representation of a physical text, which contains some linguistic words and may contain some visual or audio components as well.  This is not the physical object that represents the text, and not the entity or propositional content that is referred to by the text.  It is one of a triad of concept classes that represent different aspects of texts.")

(subclass MentalSymbol MentalEntity)
(:hasRestrictedVal MentalSymbol isTheMentalRepresentationOf PhysicalSymbol)
(documentation MentalSymbol "A MentalEntity which is used to represent another entity.  A single abstract symbol, such as the abstract letter 'A', may have innumerable physical referents.")

;; CONTEXT

(instance ActualExistence Attribute)
(documentation ActualExistence "ActualExistence  is the attribute of something that is present in some particular context.  It can apply to fictional contexts, and is intended to allow assertions that something exists - whether in the real world, or a hypothetical world.  (holdsInContext (property AlbertEinstein ActualExistence) SpaceAndTime) means that something named AlbertEinstein exists or did exists at some time in our real world.")

(instance holdsInContext BinaryRelation)
(domain holdsInContext 1 Assertion)
(domain holdsInContext 2 Context)
(relatedInternalConcept holdsInContext holds)
(documentation holdsInContext "holdsInContext means that there is some context in which a particular assertion is true.  The context can be a real-world context, a fictional context, a hypothetical context, etc.")

(subclass Context Entity)
(documentation Context "Context is a very broad concept which is used to allow assertions to be made relative to different situations, theories, hypotheses, possible worlds, time intervals, or anything else that can affect the truth of an assertion.")

(instance SpaceAndTime Context)
(instance SpaceAndTime Physical)
(documentation SpaceAndTime "SpaceAndTime  is the space-time that humans live in.  It is the context that applies when one wants to assert that something is true in the real world.")

(subclass Situation Context)
(partition Situation State Process Event)
(documentation Situation "A Situation can be any state, process or event.  This is a convenient class which can be used as an argument when an instance of any one of the component classes may serve as a value of an argument.")

(subclass State Situation)
(documentation State "A State is a set of properties of objects and processes in some System.  Changes in a State are represented as Processes and Events.")

(subclass Event Situation)
(documentation Event "An Event is the total collection of all changes in state occurring during the operation of some Process.")

;; (subclass Situation )
;; (documentation contextState: existence in a place, (e.g. textual context ;; is existence between other texts)

;; END FILE
