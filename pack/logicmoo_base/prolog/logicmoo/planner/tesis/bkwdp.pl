:- module(bkwdp,_,_).
:- use_package(library(show_trans)). 
:- use_package('logicmoo/planner/tesis/pddltostrips.pl').


'(define (problem pb2)
   (:domain blocksworld)
   (:objects a b c d)
   (:goal (on a c))
   (:init (block a) (on a b) (clear a) (clear c))
)',

'(define(domain attbw)
  (:requirements :strips :equality :disjuntive-preconditions)
  (:predicates (istable ?t)
	       (block ?b)
	       (clear ?x)
	       (on ?x ?y))
  
  (:action move
   :parameters (?obj ?source ?dest)
        :precondition (and (or (istable ?dest) (clear ?dest))
                           (block ?obj)
                           (clear ?obj)
                           (on ?obj ?source))
        :effect (and (clear ?source)
		     (on ?obj ?dest)
		     (not (on ?obj ?source))
                     (not (clear ?dest)))
    )
)'.
