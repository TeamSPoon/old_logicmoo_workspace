:- module(domain,_,_).
:- use_package('logicmoo/planner/tesis/pddltostrips.pl').


'(define (domain blocks_world)
  (:requirements :strips :equality :conditional-effects :disjuntive-preconditions :universal-preconditions)
  (:predicates (on ?x ?y)
	       (clear ?y)
	       (holding ?x))

  (:constants table)

  (:action stack
	     :parameters (?x ?y)
	     :precondition (and (holding ?x) (clear ?y) (not (= ?x ?y)))
	     :effect (and (on ?x ?y) (not(clear ?y)) (not(holding ?x)))
  )

  (:action pickup
	     :parameters (?x ?y)
	     :precondition (and (on ?x ?y) (clear ?y) (not (= ?x ?y)))
	     :effect (and (clear ?y) (holding ?x) (not(on ?x ?y)))
  )


)'.

