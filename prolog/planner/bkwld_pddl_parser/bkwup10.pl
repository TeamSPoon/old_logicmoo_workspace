:- module(bkwup,_,_).
:- use_package(library(show_trans)). 
:- use_package('/home/lenovook/Documentos/Tesis/trunk/pddltostrips.pl').


'(define (problem pb2)
   (:domain blocksworld)
   (:objects block1 block2)
   (:goal (tableisclear))
   (:init (clear block1) (clear block2) (armempty))
)',

'(define (domain blocksworld)
  (:requirements :strips :universal-preconditions)
  (:predicates (holding ?y)
	       (clear ?y)
               (armempty))

  (:constants table)

  (:action pickup2
	     :parameters (?a)
	     :precondition (and (forall (?a) (clear ?a)) (armempty))
	     :effect (and (holding ?a) (not (armempty)))
  )
)'.
