:- consult(pkt_initial).

%goal 0
%goal(eventually(always(at(robot,r4)))).

% %goal 1
% goal(eventually(always(at(o1,r4)))).

% %goal 2
%goal(eventually(and(at(o1,r2),next(eventually(at(o1,r4)))))).

% %goal 3
% goal(F) :- sequence([at(o1,r2),at(o1,r4),at(o1,c1)],F).

% %goal 4
% goal(F) :- sequence([at(o1,r2),at(o1,r4),at(o1,c1),at(o1,r3)],F).

% %goal 5
% goal(F) :- sequence([at(o1,r2),at(o1,r4),at(o1,c1),at(o1,r3),at(o1,c2)],F).

% %goal 6
%goal(F) :- sequence([at(o1,r2),at(o1,r4),at(o1,c1),at(o1,r3),at(o1,c2),at(o1,r2)],F).

% %goal 7
goal(F) :- sequence([at(o1,r2),at(o1,r4),at(o1,c1),at(o2,r3),at(o2,c4),at(o2,r2)],F).

% %goal 8
% goal(F) :- sequence([at(o1,r2),at(o2,r4),at(o2,c1),at(o3,r3),at(o4,c2),at(o1,r2)],F).
