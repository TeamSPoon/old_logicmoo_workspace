
w(W):-writeln(W).

p :- reset(q,Cont,Term),
   writeln(Term),
   call_continuation(Cont).

q :- catch(r,Ball,writeln(Ball)).

r :- shift(rterm), throw(rball).

% ?- p.

c :- 
   reset(d,Cont,Term),
   w(Term),
   call_continuation(Cont).

d :- 
(X=1 ; X=2),
 shift(t(X)),
w(aftershift(X)).


head(h, a, b).
head(g, b, b).
head(h, x, y).



:- dynamic(scce0/0).

% hide from optimizations (well as anything)
call_w_detr(Goal,Det):- call((Goal,deterministic(Det),true)).


scce1(Setup,Goal,Undo):-
      once(Setup),
      (call_w_detr(Goal,Det)
        *-> (Det == true -> once(Undo) ; (once(Undo);(once(Setup),fail)))
        ; (once(Undo),fail)).

scce2(Setup,Goal,Undo):-
   reset(scce2r(true,Undo),Cont,Term),
   once(Setup),
   call(Term),
   Goal,
   call_continuation(Cont).

% scce2r(Setup,Goal,Undo):- once(Setup), (Goal *-> shift(once(Undo)) ; (!,once(Undo),fail)).
scce2r(Goal,Undo):- Goal ,shift(once(Undo)).


scce3(S,G,C):- scce2(
   (writeln(bmid(REF,X)),asserta(scce0,REF),G,writeln(mid(G,REF,X))),
   (between(1,3,X),S,writeln(ssstart(S,REF,X))),
   (((writeln(bend(C,REF,X)),C,writeln(eend(C,REF,X)))))),fail.


y11:- scce1(writeln(start),(between(1,3,X),between(1,X,Y),writeln(X-Y)), writeln(end)),fail.  % Works
y12:- scce1(asserta(scce0,REF),(between(1,3,X),between(1,X,Y),writeln(X-Y)),writeln(REF)),fail.  % Broken
y21:- scce2(writeln(start),(between(1,3,X),between(1,X,Y),writeln(X-Y)), writeln(end)),fail.  % Works
y22:- scce2((asserta(scce0,REF),writeln(start(REF))),(between(1,3,X),between(1,X,Y),writeln(X-Y)),writeln(end(REF))),fail.  % Broken

y23:- scce2((asserta(scce0,REF),writeln(start(REF,X))),(between(1,3,X),writeln(mid(REF,X))),writeln(end(REF,X))),fail.  % Broken

y24:- scce2((writeln(bmid(REF,X)),asserta(scce0,REF),writeln(mid(REF,X))),(between(1,3,X),writeln(start(REF,X))),writeln(end(REF,X))),fail.  % Broken

y32:- scce3((asserta(scce0,REF),writeln(start(REF))),(between(1,3,X),between(1,X,Y),writeln(X-Y)),writeln(end(REF))),fail.  % Broken

/*

?- y11.
start
1-1
end
start
2-1
end
start
2-2
end
start
3-1
end
start
3-2
end
start
3-3
end
false.

?- y12.
1
<clause>(0x21a0970)
ERROR: Uninstantiated argument expected, found <clause>(0x21a0970) (2-nd argument)
ERROR: In:
ERROR:   [14] asserta(scce0,<clause>(0x21a0970))

what i wanted was the output of:

?- repeat, asserta(scce0,REF),call((between(1,3,X),between(1,X,Y),writeln(X-Y))),  writeln(REF),  X-Y == 3-3.

*/

