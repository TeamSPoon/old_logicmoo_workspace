
(define (domain fairytalecastle)

(:requirements :typing)

(:types
	special couch table chest seating open-container
	open-close-container treasury drawing-room container-object
	player room sword dragon worm frog golden black red brown
	yellow white silver green colour apple necklace crown key
	castle seat couchleg climbable edible takeable west-exit
	east-exit south-exit north-exit exit wall small ugly wooden
	generic-container weapon not-so-easy-to-kill easy-to-kill
	property object locked-unlocked open-closed 
)

(:action take=patient
	:parameters (?x - takeable ?y - generic-container)
	:precondition 
			(accessible ?x) 
			(no-inventory-object ?x)
			(has-location ?x ?y) 
	:effect 
		(inventory-object ?x)
		(not(no-inventory-object ?x)) 
		(not(has-location ?x ?y))
		
)

(:action take=patient
	:parameters (?x - takeable ?y - top)
	:precondition 	
			(accessible ?x)
			(no-inventory-object ?x)
                        (has-detail ?y ?x)
	:effect	
		(inventory-object ?x)
		(not(no-inventory-object ?x)) 
                (not(has-detail ?y ?x)) 
		(no-has-detail ?y ?x) 
)

(:action drop=patient
	:parameters (?x - object ?y - generic-container)
	:precondition (inventory-object ?x)
                      (here ?y)
	:effect 
		(not(inventory-object ?x))
		(no-inventory-object ?x)
                (has-location ?x ?y) 
)

(:action put=patient
	:parameters (?x - takeable ?y - generic-container)
	:precondition 
		(inventory-object ?x) 
		(accessible ?x)
		(accessible ?y)
	:effect 
		(has-location ?x ?y) 
		(not(inventory-object ?x))
)			    
		
(:action throw=patient=target 
	 :parameters (?x - takeable ?y - generic-container)
	 :precondition 
		       (inventory-object ?x)
		       (alive ?x)
		       (accessible ?x)
	 :effect 
		 (not(inventory-object ?x))
		 (no-inventory-object ?x)
		 (has-location ?x ?y)
		 (not(alive ?x))
)

(:action kill=patient
	 :parameters (?x - easy-to-kill)
	 :precondition 
		       (alive ?x)
		       (accessible ?x)
	 :effect (not(alive ?x))
		 (not(accessible ?x))
		 (no-accessible ?x)
)

(:action kill=patient=instr
	 :parameters (?x - not-so-easy-to-kill ?y - weapon)
	 :precondition 
		       (alive ?x)
		       (accessible ?x)
		       (inventory-object ?y)
	 :effect (not(alive ?x))
		 (not(accessible))
		 (no-accessible ?x)
)

(:action open=patient
	 :parameters (?x - open-close-container)
	 :precondition 
		       (closed ?x)
		       (unlocked ?x)
		       (accessible ?x)
	 :effect
		(not(closed ?x))
		(open ?x)
)

(:action close=patient
	 :parameters (?x - open-close-container)
	 :precondition
			(open ?x)
			(accessible ?x)
	 :effect
		(closed ?x)
		(not(open ?x))
)

(:action unlock=patient=instr
	 :parameters (?x - open-close-container ?y - key)
	 :precondition
		       (locked ?x)
		       (accessible ?x)
		       (inventory-object ?y)
		       (fits-in ?y ?x)
	 :effect
		(not(locked ?x))
		(unlocked ?x)
)

(:action lock=patient=instr
	 :parameters (?x - open-close-container ?y - key)
	 :precondition
		       (closed ?x)
		       (unlocked ?x)
		       (accessible ?x)
		       (inventory-object ?y)
		       (fits-in ?y ?x)
	 :effect
		(locked ?x)
		(not(unlocked ?x))
)

(:action eat=patient
	:parameters (?x - edible ?y - worm)
	:precondition (inventory-object ?x)
		      (no-has-detail ?x ?y)	
	:effect (nirvana ?x)
		(not(inventory-object ?x))
		(no-inventory-object ?x)
		(not(accessible ?x))
		(no-accessible ?x)
)

(:action sitdown=patient
	 :parameters (?x - seating ?y - generic-container)
	 :precondition
			(accessible ?x)
			(no-here ?x)
			(here ?y)
	 :effect
		(here ?x)
		(not(no-here ?x))
		(not(here ?y))
		(no-here ?y)
		(not(accessible ?y))
		(no-accessible ?y)
)

(:action standup=patient
	 :parameters (?x - seating ?y - generic-container)
	 :precondition
			(here ?x)
			(has-location ?x ?y)
	 :effect
		(here ?y)
		(no-here ?x)
		(not(here ?x))
		(not(no-here ?y))
		(accessible ?y)
		(not(no-accessible ?y))
)

(:action access
	 :parameters (?x - generic-container ?z - top)
	 :precondition
			(accessible ?x)
			(open ?x)
			(has-location ?z ?x)
	 :effect 
			(accessible ?z) 
			(not(no-accessible ?z))
)

(:action unaccess-first
	 :parameters (?x - generic-container ?z - top)
	 :precondition
			(accessible ?z)
			(closed ?x)
			(has-location ?z ?x)
	 :effect 
			(not(accessible ?z)) 
			(no-accessible ?z)
)

(:action unaccess-recursion
	 :parameters (?x - generic-container ?z - top)
	 :precondition
			(no-accessible ?x)
			(has-location ?z ?x)
			(accessible ?z)
			(no-here ?z)
	 :effect 
			(not(accessible ?z)) 
			(no-accessible ?z)
)


)
 

