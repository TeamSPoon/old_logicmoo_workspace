;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort object
object O1

event E(object)

fluent F1(object)
fluent F2(object)

[object,time]
Initiates(E(object),F1(object),time).

[object,time]
HoldsAt(F1(object),time) <-> HoldsAt(F2(object),time).

!HoldsAt(F2(O1),0).
Happens(E(O1),0).

range time 0 1
range offset 1 1

; End of file.
