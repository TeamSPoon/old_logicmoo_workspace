
(define (domain fairytalecastle)

(:requirements :typing)

(:types
	special couch table chest seating opencontainer
	openclosecontainer treasury drawingroom containerobject
	player room sword dragon worm frog golden black red brown
	yellow white silver green colour apple necklace crown key
	castle seat couchleg climbable edible takeable westexit
	eastexit southexit northexit exit wall small ugly wooden
	genericcontainer weapon notsoeasytokill easytokill
	property object lockedunlocked openclosed being happy 
	bored victorious
)

(:action take=agent=theme=source
	:parameters (?w - player ?x - takeable ?y - genericcontainer)
	:precondition 
			(accessible ?x) 
			(no-hold ?w ?x)
			(hold ?y ?x) 
	:effect 
                (not(no-hold ?w ?x))
		(hold ?w ?x)
		(not(hold ?y ?x))
		(no-hold ?y ?x) 		
)

(:action take=agent=theme=source
	:parameters (?w - player ?x - takeable ?y - top)
	:precondition 
			(accessible ?x) 
			(no-hold ?w ?x)
			(hasdetail ?y ?x) 
	:effect 
                (not(no-hold ?w ?x))
		(hold ?w ?x)
		(not(hasdetail ?y ?x))
		(no-hasdetail ?y ?x) 		
)


(:action drop=agent=theme=goal
	:parameters (?w - player ?x - object ?y - genericcontainer)
	:precondition 
			(hold ?y ?w)
			(hold ?w ?x)
	:effect 
		(not(hold ?w ?x))
		(no-hold ?w ?x)
		(hold ?y ?x)
		(not(no-hold ?y ?x)) 
)

(:action put=agent=theme=goal
	:parameters (?w - player ?x - takeable ?y - genericcontainer)
	:precondition 
		(hold ?w ?x) 
		(accessible ?y)
	:effect 
		(not(hold ?w ?x))
		(no-hold ?w x)
		(hold ?y ?x) 
		(not(no-hold ?y ?x))
)			    
			
(:action throw=agent=theme=goal
	 :parameters (?w - player ?x - takeable ?y - genericcontainer)
	 :precondition 
		       (hold ?w ?x)
		       (alive ?x)
	 :effect 
		 (not(hold ?w ?x))
		 (not(alive ?x))
		 (no-hold ?w ?x)
		 (no-alive ?x)
)

(:action kill=killer=victim
	 :parameters (?w - player ?x - easytokill)
	 :precondition 
		       (alive ?x)
		       (accessible ?x)
	 :effect (not(alive ?x))
	 		 (no-alive ?x)
)

(:action kill=killer=victim=instrument
	 :parameters (?w - player ?x - being ?y - weapon)
	 :precondition 
		       (alive ?x)
		       (accessible ?x)
		       (hold ?w ?y)
	 :effect (not(alive ?x))
			 (no-alive ?x)
)

(:action open=agent=object
	 :parameters (?w - player ?x - openclosecontainer)
	 :precondition 
		       (closed ?x)
		       (unlocked ?x)
		       (accessible ?x)
	 :effect
		(not(closed ?x))
		(no-closed ?x)
		(not(no-open ?x))
		(open ?x)
)

(:action shut=agent=object
	 :parameters (?w - player ?x - openclosecontainer)
	 :precondition
			(open ?x)
			(accessible ?x)
	 :effect
		(closed ?x)
		(not(open ?x))
		(not(no-closed ?x))
		(no-open ?x)
)

(:action unlock=agent=theme=instrument
	 :parameters (?w - player ?x - openclosecontainer ?y - key)
	 :precondition
		       (accessible ?x)
		       (locked ?x)
		       (hold ?w ?y)
		       (fitsin ?y ?x)
	 :effect
		(not(locked ?x))
		(no-locked ?x)	
		(not (no-unlocked ?x))	
		(unlocked ?x)
)

(:action lock=agent=theme=instrument
	 :parameters (?w - player ?x - openclosecontainer ?y - key)
	 :precondition
	 		   (accessible ?x)
		       (closed ?x)
		       (unlocked ?x)
		       (hold ?w ?y)
		       (fitsin ?y ?x)
	 :effect
		(locked ?x)
		(not(no-locked ?x))
		(not(unlocked ?x))
		(no-unlocked ?x)
)

(:action eat=ingestor=ingestible
	:parameters (?p - player ?x - edible)
	:precondition 
		(no-disgusting ?x)
		(hold ?p ?x)
	:effect 
		(not(hold ?p ?x))
		(no-hold ?p ?x)
		(gone ?x)
		(not(no-gone ?x))
)

(:action sitdown=protagonist=source=goal
	 :parameters (?w - player ?x - seating ?y - genericcontainer)
	 :precondition
	 		(hold ?y ?w)
			(no-seated ?w)
			(hold ?y ?x)
	 :effect
	    (not (no-seated ?w))
	    (seated ?w)
	 	(hold ?x ?w)
	 	(not(no-hold ?x ?w))
	 	(not(hold ?y ?w))
	 	(no-hold ?y ?w)
		(here ?x)
		(not(no-here ?x))
		(not(here ?y))
		(no-here ?y)
		(not(accessible ?y))
		(no-accessible ?y)
)

(:action standup=protagonist=source=goal
	 :parameters (?w - player ?x - seating ?y - genericcontainer)
	 :precondition
			(hold ?x ?w)
			(seated ?w)
			(hold ?y ?x)
	 :effect
	    (not(seated ?w))
	    (no-seated ?w)
	 	(not(hold ?x ?w))
	 	(no-hold ?x ?w)
	 	(hold ?y ?w)
	 	(not(no-hold ?y ?w))
		(here ?y)
		(no-here ?x)
		(not(here ?x))
		(not(no-here ?y))
		(accessible ?y)
		(not(no-accessible ?y))
)

(:action move=protagonist=goal=exit=source
	 :parameters (?w - player ?x - genericcontainer ?z - exit ?s - genericcontainer)
	 :precondition
		    (hold ?s ?w)
		    (no-seated ?w)
		    (hasexit ?s ?z)
		    (leadsto ?z ?x)
	 :effect 
	 		(not(hold ?s ?w))
	 		(no-hold ?s ?w)	 
	 		(not(no-hold ?x ?w))
	 		(hold ?x ?w)	 						
			(accessible ?x) 
			(not(no-accessible ?x))
			(no-accessible ?s) 
			(not(accessible ?s))
)

(:action kiss=agent=entity
	 :parameters (?a - player ?e - being)
	 :precondition
	 		(alive ?e)
	 		(accessible ?e)
	 		(beautiful ?a)
	 :effect
	 		(happy ?e)
	 		(victorious ?a)
)

(:action kiss=agent=entity 
	 :parameters (?a - player ?e - being)
	 :precondition
	 		(alive ?e)
	 		(accessible ?e)
	 		(no-beautiful ?a)
	 :effect
	 		(bored ?e)
)

(:action wear=agent=theme
	:parameters (?a - player ?t - wearable)
	:precondition 
			(hold ?a ?t)
			(accessible ?t)
	:effect
			(not(hold ?a ?t))
			(no-hold ?a ?t)
			(hasdetail ?a ?t)
			(not(no-hasdetail ?a ?t))
)

(:action access
	 :parameters (?x - genericcontainer ?z - top)
	 :precondition
			(accessible ?x)
			(open ?x)
			(hold ?x ?z)
	 :effect 
			(accessible ?z) 
			(not(no-accessible ?z))
)

(:action unaccessfirst
	 :parameters (?x - genericcontainer ?z - top)
	 :precondition
			(accessible ?z)
			(closed ?x)
			(hold ?x ?z)
	 :effect 
			(not(accessible ?z)) 
			(no-accessible ?z)
)

(:action unaccessrecursion
	 :parameters (?x - genericcontainer ?z - top)
	 :precondition
			(no-accessible ?x)
			(hold ?x ?z)
			(accessible ?z)
			(no-here ?z)
	 :effect 
			(not(accessible ?z)) 
			(no-accessible ?z)
)

(:action accessbeautiful
	:parameters (?p - player ?c - (either crown golden))
	:precondition 
		(hasdetail ?p ?c)
	:effect
		(beautiful ?p)
)


(:action unaccessdisgusting
	:parameters (?e - edible ?w - worm)
	:precondition
		(no-hasdetail ?e ?w)
	:effect
		(not(disgusting ?e))
		(no-disgusting ?e)
)


)
 

