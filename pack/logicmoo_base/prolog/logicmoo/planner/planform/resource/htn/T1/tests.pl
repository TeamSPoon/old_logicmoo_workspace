
% TESTS


test(1) :- 
      initial_state(1,I), 
      solve(I, 
        p([ transport(pk_1,city3_cl1,city1_cl1) ] ,[],[]) 
            ).

test(2) :- 
      initial_state(1,I), 
      solve(I, 
        p([ achieve(ss(traincar,traincar1,[at(traincar1,city1_ts1)])
                    )] ,[],[]) 
            ).
test(3) :- 
      initial_state(1,I), 
      solve(I, 
           p([ transport(pk_2,city3_cl1,city2_cl1), % hp1
             transport(pk_1,city3_cl1,city1_cl1)],  % hp2
             [before(hp1,hp2)],
             [ ])  
            ).

test(4) :-  
      initial_state(1,I), 
      solve(I, 
        p([ transport(pk_1,city3_cl1,city1_cl1), 
            transport(pk_2,city3_cl1,CL) ],
            [],
            [in_city(CL,X),ne(X,city1)])  
            ).

% transport to two different cities..
test(5) :-  
      initial_state(1,I), 
      solve(I, 
        p([ transport(pk_1,city3_cl1,X), 
            transport(pk_2,city3_cl1,Y) ],
            [before(hp1,hp2)],
            [in_city(X,CT1),in_city(Y,CT2),ne(CT1,CT2)])  
            ).

test(6) :-  
      initial_state(1,I), 
      solve(I, 
        p([ transport(pk_1,city3_cl1,X), 
            transport(pk_2,city3_cl1,Y) ],
            [before(hp1,hp2)],
            [in_city(X,CT1),in_city(Y,CT2),ne(CT1,CT2),ne(CT1,city3),ne(CT2,city3)])  
            ).


test(7) :- 
      initial_state(1,I), 
      solve(I, 
        p([ achieve(
              ss(traincar,traincar1,[at(traincar1,city1_ts1)])
                    ),
             transport(pk_6,city1_ts1,C) ],[],[ne(C,city2_ts1)]) 
            ).

test(8) :- 
      initial_state(1,I), 
      solve(I, 
        p([ 
             transport(pk_6,city1_ts1,city2_ts1) ],[],[]) 
            ).

test(9) :- 
      initial_state(1,I), 
      solve(I, 
        p([ transport(pk_2,city3_cl1,city1_cl1), 
            transport(pk_4,city1_cl1,city2_cl1), 
            transport(pk_1,city3_cl1,city1_cl1)],[],[])  
            ).


test(10) :- 
      initial_state(1,I), 
      solve(I, 
        p([ achieve(ss(traincar,traincar1,[at(traincar1,city1_ts1)])),
            transport(pk_1,city3_cl1,city2_cl1),
            achieve(ss(package,pk_5,[at(pk_5,X),delivered(pk_5)] )) ],
          [before(hp1,hp3)],
          [serves(X,city3)])  
            ).

test(11) :- 
      initial_state(1,I), 
      solve(I, 
        p([ achieve(ss(traincar,traincar1,[at(traincar1,city1_ts1)])),
            transport(pk_1,city3_cl1,city2_cl1),
            transport(pk_3,city3_cl1,city1_cl1),
            achieve(ss(package,pk_5,[at(pk_5,X),delivered(pk_5)] )) ],
          [before(hp1,hp3),before(hp2,hp3)],
          [serves(X,city3)])  
            ).
test(12) :-
      initial_state(1,I),
      solve(I,
        p([ achieve(
              ss(traincar,traincar1,[at(traincar1,city1_ts1)])
                    ),
             transport(pk_6,city1_ts1,C),
           transport(pk_1,city3_cl1,city2_cl1) ],
       [],[ne(C,city2_ts1)])
            ).

test(13) :-
      initial_state(1,I),
      solve(I,
        p([ transport(pk_1,city3_cl1,city1_cl1),
            transport(pk_2,city3_cl1,city2_cl1),
            transport(pk_3,city3_cl1,city1_cl1),
            transport(pk_4,city1_cl1,city3_cl1)],
            [before(hp1,hp3),before(hp2,hp3) ],[ ])
             ).
test(14) :-
      initial_state(1,I),
      solve(I,
           p([ transport(Y,city3_cl1,city2_cl1), % hp1
             transport(X,city3_cl1,city1_cl1)],  % hp2
             [before(hp1,hp2)],
             [ne(X,Y) ])
            ).
test(15) :-
      initial_state(1,I),
      solve(I,
        p([ transport(pk_1,city3_cl1,city1_cl1),
            transport(pk_3,city3_cl1,city1_cl1),
            transport(pk_4,city1_cl1,city3_cl1),
            transport(pk_2,city3_cl1,CL) ],
            [],
            [in_city(CL,X),ne(X,city1)])
            ).

test(16) :-
      initial_state(1,I), 
      solve(I, 
        p([ transport(pk_1,city3_cl1,city1_cl1),
            transport(pk_2,city3_cl1,city2_cl1),
            transport(pk_3,city3_cl1,city1_cl1),
            transport(pk_4,city1_cl1,city3_cl1)],
            [ ],[ ])     
             ).

test(17) :-
      initial_state(1,I), 
      solve(I, 
        p([ transport(pk_1,city3_cl1,city1_cl1),
            transport(pk_2,city3_cl1,city2_cl1),
            transport(pk_3,city3_cl1,city1_cl1),
            transport(pk_4,city1_cl1,city3_cl1),
            transport(pk_5,city3_cl1,city1_cl1) ],
            [ ],[ ])     
             ).

test(18) :-
      initial_state(1,I), 
      solve(I, 
        p([ transport(pk_1,city3_cl1,city1_cl1),
            transport(pk_2,city3_cl1,city2_cl1),
            transport(pk_3,city3_cl1,city1_cl1),
            transport(pk_4,city1_cl1,city3_cl1),
            transport(pk_5,city3_cl1,city1_cl1) ],
            [before(hp1,hp2),before(hp3,hp2)],[])     
           ).

test(19) :-
      initial_state(1,I),
      solve(I,
        p([ transport(pk_1,city3_cl1,city1_cl1),
            transport(pk_2,city3_cl1,city2_cl1),
            transport(pk_3,city3_cl1,city1_cl1),
            achieve(ss(traincar,traincar1,[at(traincar1,city1_ts1)])),
            transport(pk_4,city1_cl1,city3_cl1),
            transport(pk_5,city3_cl1,city1_cl1) ],
            [before(hp1,hp2),before(hp3,hp2)],[])
           ).
 test(20) :-
       initial_state(1,I),
       solve(I,
         p([ transport(pk_1,city3_cl1,city1_cl1),
             transport(pk_2,city3_cl1,city2_cl1),
             transport(pk_3,city3_cl1,city1_ts1),
             transport(pk_4,city1_cl1,city3_cl1),
             transport(pk_6,city1_ts1,city3_ts1),
             transport(pk_5,city3_cl1,city1_cl1) ],
             [before(hp1,hp2),before(hp3,hp2)],[])
            ).


% 
/**************************************************************/

initial_state(1, [ 
                  ss(package,pk_1, [at(pk_1,city3_cl1),
                                    uncertified(pk_1)]),
                  ss(package,pk_2, [at(pk_2,city3_cl1),
                                    uncertified(pk_2)]),
                  ss(package,pk_3, [at(pk_3,city3_cl1),
                                    uncertified(pk_3)]),
                  ss(package,pk_4, [at(pk_4,city1_cl1),
                                    uncertified(pk_4)]),
                  ss(package,pk_5, [at(pk_5,city3_cl1),
                                    uncertified(pk_5)]),
                  ss(package,pk_6, [at(pk_6,city1_ts1),
                                    uncertified(pk_6)]),
                  ss(truck,truck_1, [at(truck_1,city1_cl1),
                                     unattached(truck_1),
                                     moveable(truck_1),
                                     available(truck_1)]),
                  ss(truck,truck_11, [at(truck_11,city1_cl1),
                                     unattached(truck_11),
                                     moveable(truck_11),
                                     available(truck_11)]),
                  ss(truck,truck_2, [at(truck_2,city2_cl1),
                                     unattached(truck_2),
                                     moveable(truck_2),
                                     available(truck_2)]),
                  ss(truck,truck_22, [at(truck_22,city2_cl1),
                                     unattached(truck_22),
                                     moveable(truck_22),
                                     available(truck_22)]),
                  ss(truck,truck_3, [at(truck_3,city3_cl1),
                                     unattached(truck_3),
                                     moveable(truck_3),
                                     available(truck_3)]),
                  ss(truck,truck_33, [at(truck_33,city3_cl1),
                                     unattached(truck_33),
                                     moveable(truck_33),
                                     available(truck_33)]),
                  ss(traincar,traincar1, [at(traincar1,city2_ts1),
                                     unattached(traincar1),
                                     moveable(traincar1),available(traincar1)]),
                  ss(train,train2, [at(train2,city2_ts1),
                                     unattached(train2),
                                    moveable(train2),available(train2)]),
                  ss(train,train1, [at(train1,city1_ts1),
                                     unattached(train1),
                                    moveable(train1),available(train1)]) 

   ]).

