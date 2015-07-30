
(define (domain gripper)
(:requirements :strips)
(:predicates (room ?r)
             (ball ?b)
             (gripper ?g)
             (at-robby ?r)
             (at ?b ?r)
             (free ?g)
             (carry ?o ?g))



(:action move
:parameters  (?from ?to)
:precondition (and  (room ?from) (room ?to) (at-robby ?from))
:effect (and  (at-robby ?to) (not (at-robby ?from))))



(:action pick
:parameters (?obj ?room ?gripper)
:precondition  (and  (ball ?obj) (room ?room) (gripper ?gripper)
                     (at ?obj ?room) (at-robby ?room) (free ?gripper))
:effect (and (carry ?obj ?gripper) (not (at ?obj ?room)) 
              (not (free ?gripper))))
 

(:action drop
:parameters  (?obj  ?room ?gripper)
:precondition  (and  (ball ?obj) (room ?room) (gripper ?gripper)
                     (carry ?obj ?gripper) (at-robby ?room))
:effect (and (at ?obj ?room) (free ?gripper) (not (carry ?obj ?gripper)))))


(define (problem gripper6)
    (:domain gripper)
  (:objects roomA roomB Ball1 Ball2 Ball3 Ball4  Ball5 Ball6 left right)


(:init 

(room roomA)
(room roomB)
(ball Ball1)
(ball Ball2)
(ball Ball3)
(ball Ball4)
(ball Ball5)
(ball Ball6)
(gripper left)
(gripper right)
(at-robby roomA) 
(free left) 
(free right)  
(at Ball1 roomA)
(at Ball2 roomA)
(at Ball3 roomA)
(at Ball4 roomA)
(at Ball5 roomA)
(at Ball6 roomA))

(:goal (and (at Ball1 roomB) (at Ball2 roomB)
            (at Ball3 roomB) (at Ball4 roomB)
            (at Ball5 roomB) (at Ball6 roomB)
)))

 
