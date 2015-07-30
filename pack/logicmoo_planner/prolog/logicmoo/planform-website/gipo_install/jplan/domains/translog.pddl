; ***************************************************************************
; *  All rights reserved. Use of this software is permitted for non-commercial
; *  research purposes, and it may be copied only for that use.  All copies must
; *  include this copyright message.  This software is made available AS IS, and
; *  neither the GIPO team nor the University of Huddersfield make any warranty
; *  about the software or its performance.
; *
; *  Automatically generated PDDL Domain from  GIPO Version 3.0
; *
; *  Author: Ron  Simpson, Donghong Liu, Lee McCLuskey
; *  Institution: University of Huddersfield
; *  Date created: August 1999
; *  Date last modified: 2015/07/07 at 08:29:43 AM PDT
; *  Description:
; *    Derived from Univ. of Maryland's literal-based specification
; *    Reworking of domain to comply with GIPO and OCL(h)
; *    This model captures the object structure and actions in a "transport
; *    logistics" domain where packages have to be transported around
; *    different locations in different cities, using trucks, trains and planes.
; * OCL File name : translog.ocl
; *************************************************************************

(define (domain translog)
  (:requirements :strips :equality :typing :conditional-effects)

  (:types  traincar train truck aircraft package train_station clocation post_office airport city road_route rail_route region
     physical_obj - (either traincar train truck aircraft package) 
     vehicle - (either traincar train truck aircraft) 
     railv - (either traincar train) 
     location - (either train_station clocation post_office airport city) 
     city_location - (either train_station clocation post_office) 
     tcentre - (either train_station) 
     not_tcentre - (either clocation post_office) 
     route - (either road_route rail_route) )


  (:predicates
    (at ?physical_obj1 -  (either traincar train truck aircraft package) ?location1 -  (either train_station clocation post_office airport city))
    (moveable ?vehicle1 -  (either traincar train truck aircraft))
    (available ?vehicle1 -  (either traincar train truck aircraft))
    (busy ?vehicle1 -  (either traincar train truck aircraft))
    (attached ?vehicle1 -  (either traincar train truck aircraft) ?vehicle2 -  (either traincar train truck aircraft))
    (unattached ?vehicle1 -  (either traincar train truck aircraft))
    (waiting ?package1 - package)
    (certified ?package1 - package)
    (uncertified ?package1 - package)
    (loaded ?package1 - package ?vehicle1 -  (either traincar train truck aircraft))
    (delivered ?package1 - package)
    (ap_serves ?airport1 - airport ?city1 - city)
    (connects ?route1 -  (either road_route rail_route) ?location1 -  (either train_station clocation post_office airport city) ?location2 -  (either train_station clocation post_office airport city))
    (in_city ?location1 -  (either train_station clocation post_office airport city) ?city1 - city)
    (in_region ?location1 -  (either train_station clocation post_office airport city) ?region1 - region)
    (serves_region ?airport1 - airport ?region1 - region)
    (route_available ?route1 -  (either road_route rail_route))
    (serves ?train_station1 - train_station ?city1 - city)
  )
  (:action pay_fees
       :parameters ( ?P - package)
       :precondition 
            (uncertified ?P)
       :effect (and 
            (not (uncertified ?P))
            (waiting ?P)
            (certified ?P)
        )
    )
  (:action fly
       :parameters ( ?A - aircraft ?D1 - (either train_station clocation post_office airport city) ?D2 - (either train_station clocation post_office airport city))
       :precondition (and 
            (at ?A ?D1)
            (is_of_sort ?D2 airport)
            (is_of_sort ?D1 airport)
            (not (= ?D1 ?D2))
       )
       :effect (and 
            (not (at ?A ?D1))
            (not (is_of_sort ?D2 airport))
            (not (is_of_sort ?D1 airport))
            (at ?A ?D2)
            (forall (?X - package)
              (when (and
                (loaded ?X ?A)
                (certified ?X)
                (at ?X ?D1)
               )
              (and
                (not (at ?X ?D1))
                (at ?X ?D2)
               )))
        )
    )
  (:action move
       :parameters ( ?V - truck ?O - (either train_station clocation post_office airport city) ?City - city ?L - (either train_station clocation post_office airport city) ?City1 - city ?R - (either road_route rail_route))
       :precondition (and 
            (at ?V ?O)
            (is_of_sort ?R road_route)
            (moveable ?V)
            (in_city ?O ?City)
            (in_city ?L ?City1)
            (not (= ?City ?City1))
            (connects ?R ?City ?City1)
       )
       :effect (and 
            (not (at ?V ?O))
            (not (is_of_sort ?R road_route))
            (not (moveable ?V))
            (at ?V ?L)
            (forall (?X - package)
              (when (and
                (loaded ?X ?V)
                (certified ?X)
                (at ?X ?O)
               )
              (and
                (not (at ?X ?O))
                (at ?X ?L)
               )))
        )
    )
  (:action move_in_city
       :parameters ( ?V - truck ?O - (either train_station clocation post_office airport city) ?City - city ?L - (either train_station clocation post_office airport city))
       :precondition (and 
            (at ?V ?O)
            (moveable ?V)
            (in_city ?O ?City)
            (in_city ?L ?City)
       )
       :effect (and 
            (not (at ?V ?O))
            (not (moveable ?V))
            (at ?V ?L)
            (forall (?X - package)
              (when (and
                (loaded ?X ?V)
                (certified ?X)
                (at ?X ?O)
               )
              (and
                (not (at ?X ?O))
                (at ?X ?L)
               )))
        )
    )
  (:action pull_traincar
       :parameters ( ?Train - train ?O - (either train_station clocation post_office airport city) ?V1 - traincar ?Rt - (either road_route rail_route) ?L - (either train_station clocation post_office airport city))
       :precondition (and 
            (at ?Train ?O)
            (attached ?Train ?V1)
            (moveable ?Train)
            (connects ?Rt ?O ?L)
            (is_of_sort ?Rt rail_route)
            (at ?V1 ?O)
            (attached ?V1 ?Train)
       )
       :effect (and 
            (not (at ?Train ?O))
            (not (moveable ?Train))
            (not (at ?V1 ?O))
            (at ?Train ?L)
            (at ?V1 ?L)
            (forall (?P - package)
              (when (and
                (loaded ?P ?V1)
                (certified ?P)
                (at ?P ?O)
               )
              (and
                (not (at ?P ?O))
                (at ?P ?L)
               )))
        )
    )
  (:action move_train
       :parameters ( ?V - train ?O - (either train_station clocation post_office airport city) ?Rt - (either road_route rail_route) ?L - (either train_station clocation post_office airport city))
       :precondition (and 
            (at ?V ?O)
            (unattached ?V)
            (moveable ?V)
            (available ?V)
            (connects ?Rt ?O ?L)
            (is_of_sort ?Rt rail_route)
       )
       :effect (and 
            (not (at ?V ?O))
            (at ?V ?L)
        )
    )
  (:action attach_traincar
       :parameters ( ?Train - train ?O - (either train_station clocation post_office airport city) ?V - traincar)
       :precondition (and 
            (at ?Train ?O)
            (moveable ?Train)
            (available ?Train)
            (unattached ?Train)
            (at ?V ?O)
            (unattached ?V)
       )
       :effect (and 
            (not (available ?Train))
            (not (unattached ?Train))
            (not (unattached ?V))
            (attached ?Train ?V)
            (busy ?Train)
            (attached ?V ?Train)
        )
    )
  (:action detach_traincar
       :parameters ( ?Train - train ?V - traincar)
       :precondition (and 
            (attached ?Train ?V)
            (moveable ?Train)
            (busy ?Train)
            (attached ?V ?Train)
       )
       :effect (and 
            (not (attached ?Train ?V))
            (not (busy ?Train))
            (not (attached ?V ?Train))
            (unattached ?Train)
            (available ?Train)
            (unattached ?V)
        )
    )
  (:action commission
       :parameters ( ?V - (either traincar train truck aircraft))
       :precondition (and 
            (moveable ?V)
            (available ?V)
       )
       :effect (and 
            (not (available ?V))
            (busy ?V)
        )
    )
  (:action load_package
       :parameters ( ?V - (either traincar train truck aircraft) ?L - (either train_station clocation post_office airport city) ?P - package)
       :precondition (and 
            (at ?V ?L)
            (at ?P ?L)
            (waiting ?P)
            (certified ?P)
       )
       :effect (and 
            (not (waiting ?P))
            (loaded ?P ?V)
        )
    )
  (:action unload_package
       :parameters ( ?P - package ?L - (either train_station clocation post_office airport city) ?V - (either traincar train truck aircraft))
       :precondition (and 
            (at ?P ?L)
            (loaded ?P ?V)
            (certified ?P)
            (at ?V ?L)
            (moveable ?V)
            (busy ?V)
       )
       :effect (and 
            (not (loaded ?P ?V))
            (not (busy ?V))
            (waiting ?P)
            (available ?V)
        )
    )
  (:action deliver
       :parameters ( ?P - package ?L - (either train_station clocation post_office airport city))
       :precondition (and 
            (at ?P ?L)
            (waiting ?P)
            (certified ?P)
       )
       :effect (and 
            (not (waiting ?P))
            (not (certified ?P))
            (delivered ?P)
        )
    )
  )
