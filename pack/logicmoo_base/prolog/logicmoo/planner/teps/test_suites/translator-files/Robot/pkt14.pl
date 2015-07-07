:- consult(pkt_initial).

goal(F) :- sequence([at(o1,r2),and(and(and(at(o1,r4),at(o2,r4)),at(o3,r4)),at(o4,r4)),and(and(and(at(o1,c1),at(o2,c1)),at(o3,c1)),at(o4,c1)),and(and(and(at(o1,r1),at(o2,r1)),at(o3,r1)),at(o4,r1)),and(and(and(at(o1,r3),at(o2,r3)),at(o3,r3)),at(o4,r3))],F).


