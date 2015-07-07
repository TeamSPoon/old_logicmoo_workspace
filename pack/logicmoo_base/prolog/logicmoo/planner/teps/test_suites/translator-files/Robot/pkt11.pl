:- consult(pkt_initial).

goal(F) :- sequence([at(o1,r2),and(at(o1,r4),at(o2,r4)),and(at(o1,c1),at(o2,c1)),and(at(o1,r3),at(o3,r3)),and(at(o1,c4),at(o4,c4)),at(o1,r2)],F).


