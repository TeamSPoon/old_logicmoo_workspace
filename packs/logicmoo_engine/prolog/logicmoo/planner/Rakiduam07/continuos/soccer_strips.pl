:- module(soccer_strips,['<-'/2,'<<'/2,achieves/2,preconditions/2,deletes/2,holds/2,primitive/1],[]).

:- op(1200,xfx,[<-,<<]).
:- data holds/2.



% ACTIONS
% move(Ag,Pos,Pos_1) is the action of Ag moving from Pos to Pos_1
preconditions(move(Ag,Pos,Pos_1),
    [player(Ag), waiting_at(Ag,Pos), valid_move(Pos,Pos_1)]).
%     [player(Ag), waiting_at(Ag,Pos)]).

%una posible mejora es hacer waiting para Ag, y ball_at para Obj
%hacer que kick patee a otra posicion.
preconditions(grabBall(Ag,Obj,Pos),
    [player(Ag), Ag \= Obj, waiting_at(Obj,Pos), at(Ag,Pos) ]).
% putdown(Ag,Obj,Pos)
preconditions(kick(Ag,Obj,From,To), 
    [player(Ag),  Ag \= Obj, at(Ag,From), carrying(Ag,Obj), inReach(From,To)]).
% unlock(Ag,Door)

% move(Ag,Pos,Pos_1) is the action of Ag moving from Pos to Pos_1
achieves(move(Ag,_Pos,Pos_1),waiting_at(Ag,Pos_1)).
% grabBall(Ag,Obj,Pos) is the action of agent Ag picking up Obj.
achieves(grabBall(Ag,Obj,_Pos), carrying(Ag,Obj)).
% kick(Ag,Obj,Pos)
achieves(kick(_Ag,Obj,_From,To),waiting_at(Obj,To)).

achieves(init,X) :-
   holds(X,init).



% move(Ag,Pos,Pos_1) is the action of Ag moving from Pos to Pos_1
deletes(move(Ag,Pos,_Pos_1),waiting_at(Ag,Pos)).
% grabBall(Ag,Obj,Pos) is the action of agent Ag picking up Obj.
deletes(grabBall(_Ag,Obj,Pos), waiting_at(Obj,Pos)).
% kick(Ag,Obj,Pos)
deletes(kick(Ag,Obj,_From,_To),carrying(Ag,Obj)).


 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                           Time and Relations					  %
% • It helps to distinguish between two basic types of relations:		  %
%    - Static relations: truth value does not depend on time			  %
%    - Dynamic relations: truth values depends on time				  %
% • Dynamic relations can be further classified as:				  %
%    - Primitive relations: truth value can be determined by considering its	  %
%       value in the past and what actions have been performed			  %
%    - Derived relations: truth value can be derived from other relations	  %
% • Why is it useful to distinguish?						  %
%    - Static always the same: no need to recompute them at each time point	  %
%    - Derived need to be re-derived at each time point (usually from primitives) %
%    - Only need to reason about how the primitive relations change over time	  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% PRIMITIVE RELATIONS
primitive(carrying(_,_)).
primitive(waiting_at(_,_)).

% STATIC RELATIONS

%static(player(kula)).

player(kula) << [].


% %Neighbor
% %Case 1: the same column
% neighbor(cell(C,R1),cell(C,R2)) << [R2 is R1-1].
% neighbor(cell(C,R1),cell(C,R2)) << [R2 is R1+1].
% %Case 2: the same Row
% neighbor(cell(C1,R),cell(C2,R)) << [C2 is C1-1].
% neighbor(cell(C1,R),cell(C2,R)) << [C2 is C1+1].
% %Case 3: (+,+)
% neighbor(cell(C1,R1),cell(C2,R2)) << [R2 is R1+1, C2 is C1+1].
% %Case 4: (+,-)
% neighbor(cell(C1,R1),cell(C2,R2)) << [R2 is R1+1, C2 is C1-1].
% %Case 5: (-,+)
% neighbor(cell(C1,R1),cell(C2,R2)) << [R2 is R1-1, C2 is C1+1].
% %Case 6: (-,-)
% neighbor(cell(C1,R1),cell(C2,R2)) << [R2 is R1-1, C2 is C1-1].

inReach(cell(1,_),oppGoal) << [].
inReach(cell(2,_),oppGoal) << [].
inReach(cell(3,_),oppGoal) << [].
inReach(cell(4,_),oppGoal) << [].
inReach(cell(5,_),oppGoal) << [].
%inReach(cell(C1,R1),cell(C2,R2)) << [DifC is abs(C1-C2), DifC < 5, DifR is abs(R1-R2), DifR < 5].

valid_move(cell(C1,R1),cell(C2,R2)) << [neighbor(cell(C1,R1),cell(C2,R2))].

% DERIVED RELATIONS

at(Obj,Pos) <-
   [waiting_at(Obj,Pos)].
at(Obj,Pos) <-
   [player(Ag), Ag \= Obj, carrying(Ag,Obj), at(Ag,Pos)].


% +---------+---------+---------+
% |cell(1,3)|cell(2,3)|cell(3,3)|
% |         |         |         |
% |         |         |         |
% |         |         |         |
% |         |         |         |
% +---------+---------+---------+
% |cell(1,2)|cell(2,2)|cell(3,2)|
% |         |         |         |
% |         |         |         |
% |         |         |         |
% |         |         |         |
% +---------+---------+---------+
% |cell(1,1)|cell(2,1)|cell(3,1)|
% |         |         |         |
% |         |         |         |
% |         |         |         |
% |         |         |         |
% +---------+---------+---------+


% % adjacent(cell(C,R1),cell(C,R2)) <- [1 is abs(R1-R2)].
% % adjacent(cell(C1,R),cell(C2,R)) <- [1 is abs(C1-C2)].

% %columna 1
% adjacent(cell(1,1),cell(1,2)) <- [].
% adjacent(cell(1,1),cell(2,1)) <- [].

% adjacent(cell(1,2),cell(1,1)) <- [].
% adjacent(cell(1,2),cell(1,3)) <- [].
% adjacent(cell(1,2),cell(2,2)) <- [].

% adjacent(cell(1,3),cell(1,2)) <- [].
% adjacent(cell(1,3),cell(2,3)) <- [].
% %columna 2
% adjacent(cell(2,1),cell(3,1)) <- [].
% adjacent(cell(2,1),cell(2,2)) <- [].
% adjacent(cell(2,1),cell(1,1)) <- [].

% adjacent(cell(2,2),cell(3,2)) <- [].
% adjacent(cell(2,2),cell(2,3)) <- [].
% adjacent(cell(2,2),cell(2,1)) <- [].
% adjacent(cell(2,2),cell(1,2)) <- [].

% adjacent(cell(2,3),cell(3,3)) <- [].
% adjacent(cell(2,3),cell(1,3)) <- [].
% adjacent(cell(2,3),cell(2,2)) <- [].

% %columna3
% adjacent(cell(3,1),cell(3,2)) <- [].
% adjacent(cell(3,1),cell(2,1)) <- [].

% adjacent(cell(3,2),cell(3,3)) <- [].
% adjacent(cell(3,2),cell(3,1)) <- [].
% adjacent(cell(3,2),cell(2,2)) <- [].

% adjacent(cell(3,3),cell(3,2)) <- [].
% adjacent(cell(3,3),cell(2,3)) <- [].


% % adjacent(field1,field2) <- [].
% % adjacent(field1,field4) <- [].
% % adjacent(field2,field3) <- [].
% % adjacent(field2,field1) <- [].
% % adjacent(field2,field5) <- [].
% % adjacent(field3,field2) <- [].
% % adjacent(field3,field6) <- [].
% % adjacent(field4,field1) <- [].
% % adjacent(field4,field7) <- [].
% % adjacent(field4,field5) <- [].
% % adjacent(field5,field2) <- [].
% % adjacent(field5,field4) <- [].
% % adjacent(field5,field6) <- [].
% % adjacent(field5,field7) <- [].
% % adjacent(field5,field8) <- [].
% % adjacent(field6,field3) <- [].
% % adjacent(field6,field5) <- [].
% % adjacent(field6,field8) <- [].
% % adjacent(field7,field4) <- [].
% % adjacent(field7,field5) <- [].
% % adjacent(field7,field8) <- [].
% % adjacent(field8,field5) <- [].
% % adjacent(field8,field6) <- [].
% % adjacent(field8,field7) <- [].


% % inReach(cell(2,_),cell(3,_)) <- [].
% % inReach(cell(2,_),cell(1,_)) <- [].
% % inReach(cell(3,_),cell(2,_)) <- [].


% % inReach(field5,field6) <- [].
% % inReach(field4,field7) <- [].
% % inReach(field7,oppGoal) <- [].
% % inReach(field6,oppGoal) <- [].


% INITIAL SITUATION
holds(waiting_at(kula,cell(25,19)),init).
holds(waiting_at(ball,cell(15,12)),init).

% holds(waiting_at(kula,cell(25,19)),init).
% holds(waiting_at(ball,cell(15,12)),init).

%[waiting_at(kiñe,cell(28,12)),waiting_at(epu,cell(26,6)),waiting_at(kula,cell(25,19)),waiting_at(meli,cell(19,6)),waiting_at(kechu,cell(19,18)),waiting_at(ball,cell(15,12))]


