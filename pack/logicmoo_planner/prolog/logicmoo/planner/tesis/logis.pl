:- module(logis,_,_).
:- use_package(library(show_trans)). 
:- use_package('logicmoo/planner/tesis/pddltostrips.pl').

'(define (problem log001)
    (:domain logistics-strips)
    (:objects
        package1
        package2
        package3

        airplane1
        airplane2

        pgh
        bos
        la

        pghtruck
        bostruck
        latruck

        pghpo
        bospo
        lapo

        pghcentral
        boscentral
        lacentral

        pghairport
        bosairport
        laairport
    )

    (:goal (and
        (at package1 bospo)
        (at package2 lapo)
        (at package3 bospo)
    ))        

    (:init
        (OBJ package1)
        (OBJ package2)
        (OBJ package3)

        (AIRPLANE airplane1)
        (AIRPLANE airplane2)

        (CITY pgh)
        (CITY bos)
        (CITY la)

        (TRUCK pghtruck)
        (TRUCK bostruck)
        (TRUCK latruck)

        (LOCATION bospo)
        (LOCATION lapo)
        (LOCATION pghpo)

        (LOCATION boscentral)
        (LOCATION lacentral)
        (LOCATION pghcentral)

        (AIRPORT bosairport)
        (LOCATION bosairport)
        (AIRPORT pghairport)
        (LOCATION pghairport)
        (AIRPORT laairport)
        (LOCATION laairport)

        (incity pghpo pgh)
        (incity pghairport pgh)
        (incity pghcentral pgh)

        (incity bospo bos)
        (incity bosairport bos)
        (incity boscentral bos)

        (incity lapo la)
        (incity laairport la)
        (incity lacentral la)

        (at package1 pghpo)
        (at package2 pghpo)
        (at package3 pghpo)

        (at airplane1 pghairport)
        (at airplane2 pghairport)

        (at bostruck bospo)
        (at pghtruck pghpo)
        (at latruck lapo)

    )

)',

'(define (domain logistics-strips)
  (:requirements :strips) 
  (:predicates 	(OBJ ?obj)
	       	(TRUCK ?truck)
               	(LOCATION ?loc)
		(AIRPLANE ?airplane)
                (CITY ?city)
                (AIRPORT ?airport)
		(at ?obj ?loc)
		(in ?obj ?obj)
		(incity ?obj ?city))
 
(:action LOADTRUCK
  :parameters
   (?obj
    ?truck
    ?loc)
  :precondition
   (and (OBJ ?obj) (TRUCK ?truck) (LOCATION ?loc)
   (at ?truck ?loc) (at ?obj ?loc))
  :effect
   (and (not (at ?obj ?loc)) (in ?obj ?truck)))

(:action LOADAIRPLANE
  :parameters
   (?obj
    ?airplane
    ?loc)
  :precondition
   (and (OBJ ?obj) (AIRPLANE ?airplane) (LOCATION ?loc)
   (at ?obj ?loc) (at ?airplane ?loc))
  :effect
   (and (not (at ?obj ?loc)) (in ?obj ?airplane)))

(:action UNLOADTRUCK
  :parameters
   (?obj
    ?truck
    ?loc)
  :precondition
   (and (OBJ ?obj) (TRUCK ?truck) (LOCATION ?loc)
        (at ?truck ?loc) (in ?obj ?truck))
  :effect
   (and (not (in ?obj ?truck)) (at ?obj ?loc)))

(:action UNLOADAIRPLANE
  :parameters
   (?obj
    ?airplane
    ?loc)
  :precondition
   (and (OBJ ?obj) (AIRPLANE ?airplane) (LOCATION ?loc)
        (in ?obj ?airplane) (at ?airplane ?loc))
  :effect
   (and (not (in ?obj ?airplane)) (at ?obj ?loc)))

(:action DRIVETRUCK
  :parameters
   (?truck
    ?locfrom
    ?locto
    ?city)
  :precondition
   (and (TRUCK ?truck) (LOCATION ?locfrom) (LOCATION ?locto) (CITY ?city)
   (at ?truck ?locfrom)
   (incity ?locfrom ?city)
   (incity ?locto ?city))
  :effect
   (and (not (at ?truck ?locfrom)) (at ?truck ?locto)))

(:action FLYAIRPLANE
  :parameters
   (?airplane
    ?locfrom
    ?locto)
  :precondition
   (and (AIRPLANE ?airplane) (AIRPORT ?locfrom) (AIRPORT ?locto)
	(at ?airplane ?locfrom))
  :effect
   (and (not (at ?airplane ?locfrom)) (at ?airplane ?locto)))
)'.