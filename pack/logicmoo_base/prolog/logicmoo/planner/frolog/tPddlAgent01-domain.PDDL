
(define (domain tPddlAgent01)

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

(:action take_agent_theme_source
	:parameters (?w - player ?x - takeable ?y - genericcontainer)
	:precondition (and
			(accessible ?x) 
			(no_hold ?w ?x)
			(hold ?y ?x) 
	) :effect (and  
                (not(no_hold ?w ?x))
		(hold ?w ?x)
		(not(hold ?y ?x))
		(no_hold ?y ?x)
))

(:action take_agent_theme_source
	:parameters (?w - player ?x - takeable ?y - top)
	:precondition (and 
			(accessible ?x) 
			(no_hold ?w ?x)
			(hasdetail ?y ?x) 
	) :effect (and  
                (not(no_hold ?w ?x))
		(hold ?w ?x)
		(not(hasdetail ?y ?x))
		(no_hasdetail ?y ?x) 		
))


(:action drop_agent_theme_goal
	:parameters (?w - player ?x - object ?y - genericcontainer)
	:precondition (and 
			(hold ?y ?w)
			(hold ?w ?x)
	) :effect (and  
		(not(hold ?w ?x))
		(no_hold ?w ?x)
		(hold ?y ?x)
		(not(no_hold ?y ?x)) 
))

(:action put_agent_theme_goal
	:parameters (?w - player ?x - takeable ?y - genericcontainer)
	:precondition (and 
		(hold ?w ?x) 
		(accessible ?y)
	) :effect (and  
		(not(hold ?w ?x))
		(no_hold ?w x)
		(hold ?y ?x) 
		(not(no_hold ?y ?x))
))			    
			
(:action throw_agent_theme_goal
	 :parameters (?w - player ?x - takeable ?y - genericcontainer)
	 :precondition (and 
		       (hold ?w ?x)
		       (alive ?x)
	 ) :effect (and  
		 (not(hold ?w ?x))
		 (not(alive ?x))
		 (no_hold ?w ?x)
		 (no_alive ?x)
))

(:action kill_killer_victim
	 :parameters (?w - player ?x - easytokill)
	 :precondition (and 
		       (alive ?x)
		       (accessible ?x)
	 ) :effect (and  (not(alive ?x))
	 		 (no_alive ?x)
))

(:action kill_killer_victim_instrument
	 :parameters (?w - player ?x - being ?y - weapon)
	 :precondition (and 
		       (alive ?x)
		       (accessible ?x)
		       (hold ?w ?y)
	 ) :effect (and  (not(alive ?x))
			 (no_alive ?x)
))

(:action open_agent_object
	 :parameters (?w - player ?x - openclosecontainer)
	 :precondition (and 
		       (closed ?x)
		       (unlocked ?x)
		       (accessible ?x)
	 ) :effect (and 
		(not(closed ?x))
		(no_closed ?x)
		(not(no_open ?x))
		(open ?x)
))

(:action shut_agent_object
	 :parameters (?w - player ?x - openclosecontainer)
	 :precondition (and
			(open ?x)
			(accessible ?x)
	 ) :effect (and 
		(closed ?x)
		(not(open ?x))
		(not(no_closed ?x))
		(no_open ?x)
))

(:action unlock_agent_theme_instrument
	 :parameters (?w - player ?x - openclosecontainer ?y - key)
	 :precondition (and
		       (accessible ?x)
		       (locked ?x)
		       (hold ?w ?y)
		       (fitsin ?y ?x)
	 ) :effect (and 
		(not(locked ?x))
		(no_locked ?x)	
		(not (no_unlocked ?x))	
		(unlocked ?x)
))

(:action lock_agent_theme_instrument
	 :parameters (?w - player ?x - openclosecontainer ?y - key)
	 :precondition (and
	 		   (accessible ?x)
		       (closed ?x)
		       (unlocked ?x)
		       (hold ?w ?y)
		       (fitsin ?y ?x)
	 ) :effect (and 
		(locked ?x)
		(not(no_locked ?x))
		(not(unlocked ?x))
		(no_unlocked ?x)
))

(:action eat_ingestor_ingestible
	:parameters (?p - player ?x - edible)
	:precondition (and 
		(no_disgusting ?x)
		(hold ?p ?x)
	) :effect (and  
		(not(hold ?p ?x))
		(no_hold ?p ?x)
		(gone ?x)
		(not(no_gone ?x))
))

(:action sitdown_protagonist_source_goal
	 :parameters (?w - player ?x - seating ?y - genericcontainer)
	 :precondition (and
	 		(hold ?y ?w)
			(no_seated ?w)
			(hold ?y ?x)
	 ) :effect (and 
	    (not (no_seated ?w))
	    (seated ?w)
	 	(hold ?x ?w)
	 	(not(no_hold ?x ?w))
	 	(not(hold ?y ?w))
	 	(no_hold ?y ?w)
		(here ?x)
		(not(no_here ?x))
		(not(here ?y))
		(no_here ?y)
		(not(accessible ?y))
		(no_accessible ?y)
))

(:action standup_protagonist_source_goal
	 :parameters (?w - player ?x - seating ?y - genericcontainer)
	 :precondition (and
			(hold ?x ?w)
			(seated ?w)
			(hold ?y ?x)
	 ) :effect (and 
	    (not(seated ?w))
	    (no_seated ?w)
	 	(not(hold ?x ?w))
	 	(no_hold ?x ?w)
	 	(hold ?y ?w)
	 	(not(no_hold ?y ?w))
		(here ?y)
		(no_here ?x)
		(not(here ?x))
		(not(no_here ?y))
		(accessible ?y)
		(not(no_accessible ?y))
))

(:action move_protagonist_goal_exit_source
	 :parameters (?w - player ?x - genericcontainer ?z - exit ?s - genericcontainer)
	 :precondition (and
		    (hold ?s ?w)
		    (no_seated ?w)
		    (hasexit ?s ?z)
		    (leadsto ?z ?x)
	 ) :effect (and  
	 		(not(hold ?s ?w))
	 		(no_hold ?s ?w)	 
	 		(not(no_hold ?x ?w))
	 		(hold ?x ?w)	 						
			(accessible ?x) 
			(not(no_accessible ?x))
			(no_accessible ?s) 
			(not(accessible ?s))
))

(:action kiss_agent_entity
	 :parameters (?a - player ?e - being)
	 :precondition (and
	 		(alive ?e)
	 		(accessible ?e)
	 		(beautiful ?a)
	 ) :effect (and 
	 		(happy ?e)
	 		(victorious ?a)
))

(:action kiss_agent_entity 
	 :parameters (?a - player ?e - being)
	 :precondition (and
	 		(alive ?e)
	 		(accessible ?e)
	 		(no_beautiful ?a)
	 ) :effect (and 
	 		(bored ?e)
))

(:action wear_agent_theme
	:parameters (?a - player ?t - wearable)
	:precondition (and 
			(hold ?a ?t)
			(accessible ?t)
	) :effect (and 
			(not(hold ?a ?t))
			(no_hold ?a ?t)
			(hasdetail ?a ?t)
			(not(no_hasdetail ?a ?t))
))

(:action access
	 :parameters (?x - genericcontainer ?z - top)
	 :precondition (and
			(accessible ?x)
			(open ?x)
			(hold ?x ?z)
	 ) :effect (and  
			(accessible ?z) 
			(not(no_accessible ?z))
))

(:action unaccessfirst
	 :parameters (?x - genericcontainer ?z - top)
	 :precondition (and 
			(accessible ?z)
			(closed ?x)
			(hold ?x ?z)
	 ) :effect (and  
			(not(accessible ?z)) 
			(no_accessible ?z)
))

(:action unaccessrecursion
	 :parameters (?x - genericcontainer ?z - top)
	 :precondition (and 
			(no_accessible ?x)
			(hold ?x ?z)
			(accessible ?z)
			(no_here ?z)
	 ) :effect (and  
			(not(accessible ?z)) 
			(no_accessible ?z)
))

(:action accessbeautiful
	:parameters (?p - player ?c - (either crown golden))
	:precondition (and 
		(hasdetail ?p ?c)
	) :effect (and 
		(beautiful ?p)
))


(:action unaccessdisgusting
	:parameters (?e - edible ?w - worm)
	:precondition (and 
		(no_hasdetail ?e ?w)
	) :effect (and 
		(not(disgusting ?e))
		(no_disgusting ?e)
))


)
 

