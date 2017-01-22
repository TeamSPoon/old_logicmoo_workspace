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
; @article{Giunchiglia:2004,
;   author = "Enrico Giunchiglia and Joohyung Lee and Vladimir Lifschitz and Norman C. McCain and Hudson Turner",
;   year = "2004",
;   title = "Nonmonotonic causal theories",
;   journal = "Artificial Intelligence",
;   volume = "153",
;   pages = "49--104",
; }
;

; postdiction

load foundations/Root.e
load foundations/EC.e
load examples/GiunchigliaEtAl2004/MonkeyBananas.e

HoldsAt(At(Monkey,L1),0).
HoldsAt(At(Bananas,L2),0).
Happens(Walk(L3),0).
Happens(PushBox(L2),1).

completion Happens

range time 0 2
range offset 0 0

; End of file.
