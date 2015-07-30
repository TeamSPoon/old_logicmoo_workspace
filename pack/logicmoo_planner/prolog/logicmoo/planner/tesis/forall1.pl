:- module(forall1,_,_).
:- use_package('logicmoo/planner/tesis/pddltostrips.pl').

'(define (problem blocksword_problem)
  (:domain blocks_world)
  (:objects table)
  (:goal (on a f))
  (:init (clear table)
	 (on a b)
	 (on b c) 
	 (on c table)
	 (clear a)
	 (on d table)
	 (on f d)
	 (clear f)
	 (clear e)
	 (on e table))
)',

'(define (domain blocks_world)
  (:requirements :strips :equality :universal-preconditions)
  (:predicates (on ?x ?y)
	       (clear ?y)
	       (holding ?x))

  (:constants table)

  (:action ptodo
	     :parameters (?b)
	     :precondition (forall (?b) (at ?c) (on ?v ?b))
	     :effect (and (clear ?b))
  )


)'.
