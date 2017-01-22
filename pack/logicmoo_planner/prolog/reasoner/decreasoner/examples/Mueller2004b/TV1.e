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
; @inproceedings{Mueller:2004b,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "A tool for satisfiability-based commonsense reasoning in the event calculus",
;   editor = "Valerie Barr and Zdravko Markov",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}eventeenth \uppercase{I}nternational \uppercase{F}lorida \uppercase{A}rtificial \uppercase{I}ntelligence \uppercase{R}esearch \uppercase{S}ociety \uppercase{C}onference",
;   pages = "147--152",
;   address = "Menlo Park, CA",
;   publisher = "AAAI Press",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent
sort switch
sort tv

function TVOf(switch): tv
fluent SwitchOn(switch)
fluent TVOn(tv)
fluent PluggedIn(tv)
event TurnOn(agent,switch)
event TurnOff(agent,switch)

[agent,switch,time] Initiates(TurnOn(agent,switch),SwitchOn(switch),time).

[agent,switch,tv,time]
TVOf(switch)=tv & HoldsAt(PluggedIn(tv),time) ->
Initiates(TurnOn(agent,switch),TVOn(tv),time).

agent James
switch Switch1
tv TV1

TVOf(Switch1)=TV1.
HoldsAt(PluggedIn(TV1),0).
!HoldsAt(SwitchOn(Switch1),0).
!HoldsAt(TVOn(TV1),0).
Happens(TurnOn(James,Switch1),0).

range time 0 1
range offset 1 1

; End of file.
