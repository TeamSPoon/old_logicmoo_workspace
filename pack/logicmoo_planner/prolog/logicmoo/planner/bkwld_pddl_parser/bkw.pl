:- module(bkw,_,_).
:- use_package(library(show_trans)). 
:- use_package('/home/lenovook/Documentos/Tesis/branches/DisjuntivePreconditions/pddltostrips.pl').

'(define (problem pb2)
   (:domain blocksworld)
   (:objects a b)
   (:goal (and(on a b)))
   (:init (ontable c) (ontable b) (on a c) (clear a)  (clear b) (armempty))
)',

'(define (domain blocksworld)
(:requirements :strips :equality)
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
  :precondition (and  (clear ?underob) (holding ?ob))
  :effect (and (clear ?ob) (on ?ob ?underob) (armempty)
               (not (clear ?underob)) (not (holding ?ob))))
)'.
