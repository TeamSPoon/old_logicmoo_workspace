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
load foundations/Root.e
load foundations/EC.e

sort agent

fluent Awake(agent)
event WakeUp(agent)

[agent,time] Initiates(WakeUp(agent),Awake(agent),time).

agent James
!HoldsAt(Awake(James),0).
HoldsAt(Awake(James),1).

range time 0 1
range offset 1 1
