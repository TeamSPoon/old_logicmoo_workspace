:- module(bkwup,_,_).
:- use_package(library(show_trans)). 
:- use_package('logicmoo/planner/tesis/pddltostrips.pl').

'(define (problem pb1)
   (:domain bkwup)
   (:objects a b c)
   (:goal (on a b))
   (:init (ontable c) (ontable b) (ontable a) (on a c) (clear a)  (clear b) (armempty))
)',

'(define (domain bkwup)
(:requirements :strips :universal-preconditions)
(:predicates (clear ?x)
             (ontable ?x)
	     (armempty)
             (holding ?x)
             (on ?x ?y))

(:action pickup
  :parameters (?ob)
  :precondition (and (clear ?ob) (armempty))
  :effect (and (holding ?ob) (not (clear ?ob)) (not (armempty))))

(:action stack
  :parameters  (?ob ?underob)
  :precondition (and (forall (?block) (ontable ?block))
		     (clear ?underob) (holding ?ob))
  :effect (and (clear ?ob) (on ?ob ?underob) (armempty)
               (not (clear ?underob)) (not (holding ?ob))))
)'.