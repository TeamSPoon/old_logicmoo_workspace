(in-abox gameinitial fairytalecastle)

;;; define types of objects
;; if this abox is changed, the primitive concepts and roles in frolog.java should be updated accordingly

(instance room1 drawingroom)
(instance room2 treasury)
(instance nirvana room)

(instance wall1 (and wall green))
(instance wall2 (and wall white))
(related room1 wall1 hasdetail)
(related room2 wall2 hasdetail)

(instance myself player)
(instance myself alive)
(related couch1 myself hold)

(related couch1 priesemut hold)
(related couch1 priesemut2 hold)

(instance priesemut (and frog ugly green))
(instance priesemut alive)

(instance priesemut2 (and frog ugly brown))
(instance priesemut2 alive)

(instance sword2 (and sword small))
(related priesemut sword2 hasdetail)

(instance crown2 (and crown silver small))
(related priesemut crown2 hasdetail)

(instance grisu dragon)
(instance grisu alive)
(related room2 grisu hold)

(instance apple1 (and apple green))
(related couch1 apple1 hold)
(instance worm1 (and worm ugly))
(instance worm1 alive)
(related apple1 worm1 hasdetail)

(instance apple2 (and apple red small))
(related couch1 apple2 hold)

(instance chest1 chest)
(instance chest1 wooden)
(instance chest1 locked)
(instance chest1 closed)
(related room2 chest1 hold)

(instance couch1 (and couch brown))
(related room1 couch1 hold)
(instance couchleg1 (and couchleg wooden))
(related couch1 couchleg1 hasdetail)
(instance couchleg2 (and couchleg wooden))
(related couch1 couchleg2 hasdetail)
(instance couchleg3 (and couchleg wooden))
(related couch1 couchleg3 hasdetail)
(instance couchleg4 (and couchleg wooden))
(related couch1 couchleg4 hasdetail)

;(instance couch2 (and couch white))
;(related room1 couch2 hold)

(instance crown1 (and crown golden))
(related chest1 crown1 hold)

(instance sword1 (and sword silver))
(related chest1 sword1 hold)

(instance key1 (and key golden))
(related myself key1 hold)
(related key1 chest1 fitsin)

(instance key2 (and key silver))
(related table1 key2 hold)

(instance apple3 (and apple yellow))
(related table1 apple3 hold)

(instance table1 (and table red))
(related room1 table1 hold)

;;(instance table2 (and table black))
;;(related room2 table2 hold)


;;; exits
; (and locked)  auskommentiert zu testzwecken (ak)

(instance drawing2treasureexit southexit)
(instance treasure2drawingexit northexit)

(related room1 drawing2treasureexit hasexit)
(related room2  treasure2drawingexit hasexit)

(related drawing2treasureexit room2 leadsto)
(related treasure2drawingexit room1 leadsto)
(related drawing2treasureexit treasure2drawingexit hascounterpart)
(related treasure2drawingexit drawing2treasureexit hascounterpart)


