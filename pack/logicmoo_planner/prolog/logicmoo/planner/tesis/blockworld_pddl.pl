:- module(blockworld_pddl,_,_).
:- use_package('logicmoo/planner/tesis/pddltostrips.pl').


'(define (problem blocksword_problem)
  (:domain blocks_world)
  (:objects a b c d e f)
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
)'.
