(in-tbox fairytalecastle)

(signature
 :atomic-concepts (special
		   player alive dead easytokill notsoeasytokill
		   dragon frog yummy
		   weapon sword
		   object takeable edible climbable wooden empty
		   genericcontainer	; room oder containerobject
		   containerobject openclosecontainer opencontainer
		   hereobject hereroom seated inventoryobject accessible visible takeable here
		   room wall treasury drawingroom
		   exit northexit southexit eastexit westexit
		   openclosed open closed lockedunlocked locked unlocked
		   green silver white yellow golden
		   brown red black happy
		   couch couchleg seat seating
		   castle
		   crown necklace key
		   apple worm
		   chest table
		   property colour ugly small
		   retinascanner foodpaste
		   disgusting gone being
		   beautiful wearable victorious bored
		   pizza
  		   )
  		   

 :roles ((haslocation :feature t :inverse hold)
	 (hasexit :inverse exitof)
	 (hascounterpart)
	 (leadsto :feature t)
	 (hasdetail :inverse partof)
	 (fitsin)
	 (controls)))
         

;;; domain & range restrictions for roles

(implies *top* (all haslocation genericcontainer))
(implies (or player object) (or (some partof *top*) (some haslocation *top*)))
(implies (some hasexit *top*) room)
(implies *top* (all leadsto room))
(implies *top* (all hasexit exit))
(implies *top* (all hascounterpart exit))
(implies (not exit) (not (some hascounterpart *top*)))

(implies (and genericcontainer (exactly 0 hold)) empty)

;;; define "hereobject" and "inventoryobject"
(equivalent here (some hold player))
(equivalent hereobject (and object (some haslocation here)))  ;;; all
(equivalent inventoryobject (and object (some haslocation player)))   ;;; all

;;; define "disgusting"
(equivalent disgusting (some hasdetail worm))

;;; define "beautiful"
(equivalent beautiful (some hasdetail (and crown golden)))

;; define "yummy"
(equivalent yummy (and edible (not disgusting)))

;; define "dead"
(equivalent dead (not alive))

;;; define "seated"
(equivalent seated (and player (some haslocation seating)))

;; This concept causes the concept special to be inconsistent, special should be father and not child
;; but if this is changed in the ontology this has to be changed in the RR and RG modules too. 
;;define "hereroom" limited to depth 3
;;(equivalent hereroom (and room (or here
;;								   (some hold here)
;;								   (some hold (some hold here)))))

;;; define "visible" and "accessible"
(equivalent visible (or here 
			(some haslocation here)
			(some haslocation (and visible open))
			(some exitof visible)  ;; some (not all) because some things don't have an exit
			(some partof visible)
			(some hold (and visible open)) ;; this definition does not work if the player is inside 3 or more
			(some haslocation (some hold (and open here))))) ;; anidated containers
;; Can be 'visible' defined in more general terms? For an arbitrary depth of containers of the player?

(equivalent accessible (or here 
			   (some haslocation here) 
			   (some haslocation (and accessible open))
			   (some partof accessible)))

;;; special concepts needed for processing of the game engine

(implies special hereobject)
;;(implies special hereroom)
(implies special here)
(implies special inventoryobject)
(implies special accessible)
(implies special visible)
(implies special takeable)
;(implies special player)

;;; concepts

(equivalent genericcontainer (or player room containerobject))

(disjoint room object)
(disjoint room player)
(disjoint object player)
(disjoint exit player)
(disjoint exit object)
(disjoint exit room)

(implies drawingroom room)
(implies treasury room)

(implies crown wearable)

(implies colour property)
(implies takeable object)
(implies edible object)
(implies climbable object)
(implies containerobject object)
(implies openclosecontainer (and containerobject openclosed lockedunlocked))
(implies opencontainer (and containerobject open))
(implies northexit exit)
(implies southexit exit)
(implies eastexit exit)
(implies westexit exit)
(implies exit (and openclosed lockedunlocked))
(implies open openclosed)
(implies closed openclosed)
(implies locked lockedunlocked)
(implies unlocked lockedunlocked)
;;(implies locked closed)
;;(disjoint open closed)
(disjoint locked unlocked)
(implies seating open)
(implies room open)
(implies wall object)

(implies green colour)
(implies silver colour)
(implies white colour)
(implies yellow colour)
(implies brown colour)
(implies red colour)
(implies black colour)
(implies golden colour)

(implies player being)
(implies frog being)
(implies dragon being)

(implies player (and open notsoeasytokill))

(implies treasury room)
(implies drawingroom room)

(implies frog (and takeable easytokill))
(implies dragon (and object notsoeasytokill))
(implies worm (and takeable easytokill))

(implies seating (and containerobject open))
(implies couch seating)
(implies couchleg object)
(implies seat object)

(implies castle object)

(implies apple edible)
(implies apple takeable)
(implies pizza edible)
(implies pizza takeable)
(implies chest takeable)
(implies chest openclosecontainer)
(implies key takeable)
(implies table opencontainer)
(implies sword (and takeable weapon))
(implies crown takeable)
(implies necklace takeable)

(implies retinascanner object)
