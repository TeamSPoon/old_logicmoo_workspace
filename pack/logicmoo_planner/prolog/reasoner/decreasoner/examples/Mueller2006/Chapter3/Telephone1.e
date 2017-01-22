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

sort agent
sort phone

agent Agent1, Agent2
phone Phone1, Phone2

fluent Ringing(phone,phone)
fluent DialTone(phone)
fluent BusySignal(phone)
fluent Idle(phone)
fluent Connected(phone,phone)
fluent Disconnected(phone)

event PickUp(agent,phone)
event SetDown(agent,phone)
event Dial(agent,phone,phone)

; Sigma

[agent,phone,time]
HoldsAt(Idle(phone),time) ->
Initiates(PickUp(agent,phone),DialTone(phone),time).

[agent,phone,time]
HoldsAt(Idle(phone),time) ->
Terminates(PickUp(agent,phone),Idle(phone),time).

[agent,phone,time]
HoldsAt(DialTone(phone),time) ->
Initiates(SetDown(agent,phone),Idle(phone),time).

[agent,phone,time]
HoldsAt(DialTone(phone),time) ->
Terminates(SetDown(agent,phone),DialTone(phone),time).

[agent,phone1,phone2,time]
HoldsAt(DialTone(phone1),time) &
HoldsAt(Idle(phone2),time) ->
Initiates(Dial(agent,phone1,phone2),Ringing(phone1,phone2),time).

[agent,phone1,phone2,time]
HoldsAt(DialTone(phone1),time) &
HoldsAt(Idle(phone2),time) ->
Terminates(Dial(agent,phone1,phone2),DialTone(phone1),time).

[agent,phone1,phone2,time]
HoldsAt(DialTone(phone1),time) &
HoldsAt(Idle(phone2),time) ->
Terminates(Dial(agent,phone1,phone2),Idle(phone2),time).

[agent,phone1,phone2,time]
HoldsAt(DialTone(phone1),time) &
!HoldsAt(Idle(phone2),time) ->
Initiates(Dial(agent,phone1,phone2),BusySignal(phone1),time).

[agent,phone1,phone2,time]
HoldsAt(DialTone(phone1),time) &
!HoldsAt(Idle(phone2),time) ->
Terminates(Dial(agent,phone1,phone2),DialTone(phone1),time).

[agent,phone,time]
HoldsAt(BusySignal(phone),time) ->
Initiates(SetDown(agent,phone),Idle(phone),time).

[agent,phone,time]
HoldsAt(BusySignal(phone),time) ->
Terminates(SetDown(agent,phone),BusySignal(phone),time).

[agent,phone1,phone2,time]
HoldsAt(Ringing(phone1,phone2),time) ->
Initiates(SetDown(agent,phone1),Idle(phone1),time).

[agent,phone1,phone2,time]
HoldsAt(Ringing(phone1,phone2),time) ->
Initiates(SetDown(agent,phone1),Idle(phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Ringing(phone1,phone2),time) ->
Terminates(SetDown(agent,phone1),Ringing(phone1,phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Ringing(phone1,phone2),time) ->
Initiates(PickUp(agent,phone2),Connected(phone1,phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Ringing(phone1,phone2),time) ->
Terminates(PickUp(agent,phone2),Ringing(phone1,phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) ->
Initiates(SetDown(agent,phone1),Idle(phone1),time).

[agent,phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) ->
Initiates(SetDown(agent,phone1),Disconnected(phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) ->
Terminates(SetDown(agent,phone1),Connected(phone1,phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) ->
Initiates(SetDown(agent,phone2),Idle(phone2),time).

[agent,phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) ->
Initiates(SetDown(agent,phone2),Disconnected(phone1),time).

[agent,phone1,phone2,time]
HoldsAt(Connected(phone1,phone2),time) ->
Terminates(SetDown(agent,phone2),Connected(phone1,phone2),time).

[agent,phone,time]
HoldsAt(Disconnected(phone),time) ->
Initiates(SetDown(agent,phone),Idle(phone),time).

[agent,phone,time]
HoldsAt(Disconnected(phone),time) ->
Terminates(SetDown(agent,phone),Disconnected(phone),time).

; Delta

Delta: Happens(PickUp(Agent1,Phone1),0).
Delta: Happens(Dial(Agent1,Phone1,Phone2),1).
Delta: Happens(PickUp(Agent2,Phone2),2).

; Gamma

[phone] HoldsAt(Idle(phone),0).
[phone] !HoldsAt(DialTone(phone),0).
[phone] !HoldsAt(BusySignal(phone),0).
[phone1,phone2] !HoldsAt(Ringing(phone1,phone2),0).
[phone1,phone2] !HoldsAt(Connected(phone1,phone2),0).
[phone] !HoldsAt(Disconnected(phone),0).

completion Delta Happens

range time 0 3
range offset 1 1

; End of file.
