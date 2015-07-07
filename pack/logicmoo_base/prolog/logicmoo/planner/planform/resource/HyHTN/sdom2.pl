/* Simple Translog, August, 1999. *****************************************

  Written in: OCLh
  Originator: Donghong Liu      Aug 99
  Updated:    Lee McCLuskey     Sept 99

   Derived from Univ. of Maryland's literal-based specification     

This model captures the object structure and actions in a "transport
logistics" domain where packages have to be transported around 
different locations in different cities, using trucks and trains */

/*********************** sort hierarchy *****************************/

domain_name(translog2).

sorts(non_primitive_sorts, [
       location, city_location,tcentre,not_tcentre, route, 
       physical_obj, vehicle, railv]).
sorts(primitive_sorts, [
        airport, aircraft, train_station, post_office, clocation, city, package, 
        train, traincar, truck, road_route, rail_route, region]).
sorts(physical_obj, [vehicle, package]).
sorts(vehicle, [railv,truck,aircraft]).
sorts(railv, [traincar,train]).
sorts(location, [city_location,city,airport]).
sorts(city_location, [tcentre,not_tcentre]).
sorts(tcentre, [train_station]).
sorts(not_tcentre, [clocation,post_office]).
sorts(route, [road_route, rail_route]).

objects(aircraft,[ac1,ac2,ac3,ac4,ac5,ac6,ac7,ac8]).
objects(airport, [ap1, ap2 ]).

objects(train_station, [ 
                        city1_ts1_x,city2_ts1_x,city3_ts1_x,
                        city1_ts1,city2_ts1,city3_ts1]).
objects(clocation, [ 
                        city1_cl1_x,city1_cl2_x,city2_cl1_x,city3_cl1_x,
                        city1_cl1,city1_cl2,city2_cl1,city3_cl1]).
objects(post_office, [ post_1]).
objects(city, [ 
                        city1_x, city2_x, city3_x,
                        city1, city2, city3]).
objects(train,[ 
                        train1_x,train2_x,
                        train1,train2]).
objects(traincar,[ 
                        traincar1_x,
                        traincar1]).
objects(road_route, [ 
                        road_route_21_x,road_route_32_x, road_route_31_x ,
                        road_route_21,road_route_32, road_route_31 ]).
objects(rail_route,[
                        rail_route_2_x,rail_route_3_x ,
                        rail_route_2,rail_route_3 ]).
objects(truck, [
                        truck_1_x, truck_2_x, truck_3_x, truck_11_x, truck_22_x, truck_33_x,
                        truck_1, truck_2, truck_3, truck_11, truck_22, truck_33]).

objects(package,[pk_1, pk_2, pk_3, pk_4, pk_5, pk_6, 
                  pk_1_x, pk_2_x, pk_3_x, pk_4_x, pk_5_x, pk_6_x]).

objects(region,[east,west]).

/*********************** predcate defns ***********************************/

predicates([
% dynamic 
  at(physical_obj,city_location),
  moveable(vehicle),
  available(vehicle),
  busy(vehicle),
  attached(railv,railv),
  unattached(railv),
  waiting(package),
  certified(package),
  uncertified(package),
  loaded(package,truck),
  loaded(package,traincar),
  loaded(package,aircraft),
  delivered(package),
% static
  rv_compatible(route,vehicle),
  ap_serves(airport,city),
  connects(route,location,location),
  in_city(location, city),
  in_region(location,region),
  serves_region(airport,region),
  route_available(route) ]).

/*********************** invariants ****************************************/

% LHS vars univ. quantified over primitive sorts
% RHS free vars are existentially quantified

implied_invariant([loaded(P,V)], [at(V,L),at(P,L)]).

inconsistent_constraint([certified(P), not_insured(P)]).

atomic_invariants([
      rv_compatible(rail_route,traincar),
      rv_compatible(rail_route,train),
      rv_compatible(road_route,truck),

      serves_region(ap1,east),
      in_city(ap1,city1),
      in_city(city1_cl1,city1), in_city(city1_ts1,city1),
      in_city(city1_cl2,city1), in_city(city1_ts2,city1),
      in_city(city2_cl1,city2), in_city(city2_ts1,city2),
      in_city(city3_cl1,city3), in_city(city3_ts1,city3),
      serves(city1_ts1,city1), serves(city1_ts2,city1),
      serves(city2_ts1,city2),
      serves(city3_ts1,city3),
      route_available(road_route_31),
      connects(road_route_31,city3,city1),
      connects(road_route_31,city1,city3),
      route_available(road_route_32),
      connects(road_route_32,city3,city2),
      connects(road_route_32,city2,city3),
      route_available(rail_route_1),
      connects(rail_route_1,city1_ts2,city1_ts1),
      connects(rail_route_1,city1_ts1,city1_ts2),
      route_available(rail_route_2),
      connects(rail_route_2,city2_ts1,city1_ts1),
      connects(rail_route_2,city1_ts1,city2_ts1),
      connects(road_route_21,city2,city1),
      route_available(road_route_21),
      connects(road_route_21,city1,city2),

      in_region(city1_ts1,east),in_region(city1,east),
      in_region(city2_ts1,east), in_region(city2,east),
      in_region(city3_ts1,east), in_region(city3,east),
      in_region(city1_ts2,east), in_region(city1_cl1,east),
      in_region(city1_cl2,east), in_region(city2_cl1,east),
      in_region(city3_cl1,east), in_region(ap1,east),

      serves_region(ap2,west),
      in_city(ap2,city1_x),
      in_region(city1_ts1_x,west),in_region(city1_x,west),
      in_region(city2_ts1_x,west), in_region(city2_x,west),
      in_region(city3_ts1_x,west), in_region(city3_x,west),
      in_region(city1_ts2_x,west), in_region(city1_cl1_x,west),
      in_region(city1_cl2_x,west), in_region(city2_cl1_x,west),
      in_region(city3_cl1_x,west), in_region(ap2,west),

      in_city(city1_cl1_x,city1_x), in_city(city1_ts1_x,city1_x),
      in_city(city1_cl2_x,city1_x), in_city(city1_ts2_x,city1_x),
      in_city(city2_cl1_x,city2_x), in_city(city2_ts1_x,city2_x),
      in_city(city3_cl1_x,city3_x), in_city(city3_ts1_x,city3_x),
      serves(city1_ts1_x,city1_x), serves(city1_ts2_x,city1_x),
      serves(city2_ts1_x,city2_x),
      serves(city3_ts1_x,city3_x),
      route_available(road_route_31_x),
      connects(road_route_31_x,city3_x,city1_x),
      connects(road_route_31_x,city1_x,city3_x),
      route_available(road_route_32_x),
      connects(road_route_32_x,city3_x,city2_x),
      connects(road_route_32_x,city2_x,city3_x),
      route_available(rail_route_1_x),
      connects(rail_route_1_x,city1_ts2_x,city1_ts1_x),
      connects(rail_route_1_x,city1_ts1_x,city1_ts2_x),
      route_available(rail_route_2_x),
      connects(rail_route_2_x,city2_ts1_x,city1_ts1_x),
      connects(rail_route_2_x,city1_ts1_x,city2_ts1_x),

      connects(road_route_21_x,city1_x,city2_x),
      connects(road_route_21_x,city2_x,city1_x),
      route_available(road_route_21_x)
/*
      in_region(truck_1_x,west),
      in_region(truck_2_x,west),
      in_region(truck_3_x, west),
      in_region(truck_11_x, weat),
      in_region(truck_22_x, west),
      in_region(truck_33_x,west),
      in_region(truck_1, east),
      in_region(truck_2, east),
      in_region(truck_3, east),
      in_region(truck_11, east),
      in_region(truck_22, east),
      in_region(truck_33, east)
*/ 
     ]).

/*********************** ss classes ****************************************/

substate_classes(physical_obj, P,
       [
        [at(P,L)]
       ]).
substate_classes(railv, V,
       [
        [unattached(V)] , [attached(V,V1)]
       ]).

substate_classes(vehicle, TC,
       [
        [moveable(TC),available(TC)],
        [moveable(TC),busy(TC) ]
       ]).

substate_classes(package, P,
       [
        [uncertified(P)],
        [waiting(P),certified(P)],
        [loaded(P,V),certified(P)],
        [delivered(P)]
      ]) .
/*********************** operators ****************************************/

% method(name,precons,transitions,statics,temps,decomposition)
% operator(name,prevail,transitions,cond_transitions)


method(
 % 1. name
      transport(P,O,D),
 % 2. dynamic constraints
      [ ],
 % 3. list of necessary substate changes
      [ sc(package, P, [at(P,O), is_of_sort(P,package)] => 
                       [at(P,D), delivered(P)]) ],
 % 4. static constraints
      [ ne(O,D),in_region(O,R),in_region(D,R)
       % list of static predicates that must be instantiated to
       % be true. Static preds may also appear in 2. and 3. if
       % its clearer that way 
       ],

 % 5.  temporal  constraints
       % list of static predicates before(N1,N2)
      [before(1,2),before(2,3)],
 % 6. decomposition
      [ achieve( ss(package, P,[waiting(P),certified(P)]) ), carry_direct(P,O,D), deliver(P,D)]
 ).

method(
      transport(P,O,D),
      [ ],
      [ sc(package, P, [at(P,O), is_of_sort(P,package)] =>
                       [at(P,D), delivered(P)]) ],
      [ ne(O,D),ne(R1,R2),ne(V1,V2),is_of_sort(AV,aircraft),
        in_region(O,R1),in_region(D,R2),
        serves_region(A1,R1),serves_region(A2,R2)
       ],
      [before(1,2),before(2,3),before(3,4),before(4,5)],
      [ achieve( ss(package, P,[waiting(P),certified(P)]) ), 
        carry_direct(P,O,A1), carry_via_ap(A1,A2,P), 
        carry_direct(P,A2,D), deliver(P,D)]
 ).


 
method(
   carry_via_ap(O,D,P),
  [ ],
  [ sc(package, P, [at(P,O),waiting(P),certified(P)] =>
                       [at(P,D),waiting(P),certified(P)]) ],
      [ ne(O,D), 
        is_of_sort(O,airport), is_of_sort(D,airport),
        is_of_sort(P,package), is_of_sort(V,aircraft)],
 [before(1,3), before(2,3), before(3,4),before(4,5)],
  [    commission(V),
       achieve(ss(aircraft,V,[at(V,O)])),
       load_package(P,V,O),
       fly(V,O,D),
       unload_package(P,V,D)
       ]
).



% carry between two cities by traincar
method(
  carry_direct(P,O,D),
 [ ],
  [ sc(package, P, [at(P,O),waiting(P),certified(P)] => 
                       [at(P,D),waiting(P),certified(P)]) ],
 [is_of_sort(P,package),
       is_of_sort(V,traincar),
       connects(R2,O1,O),
       is_of_primitive_sort(R,RS),
       rv_compatible(RS,traincar),
       route_available(R),
       ne(Train,V),
       connects(R,O,D)
       ],
 [before(1,3), before(2,3), before(3,4),before(4,5),before(5,6)],
 [   
       commission(V),
       achieve(ss(traincar,V,[at(V,O),moveable(V),available(V),attached(V,Train)])),
       load_package(P,V,O),
       pull_traincar(Train,V,O,D,R),
       detach_traincar(Train,V,D),
       unload_package(P,V,D)
       ]
).
% 
% carry in one city
method(  
  carry_direct(P,O,D),
 [ ],
  [ sc(package, P, [at(P,O),waiting(P),certified(P)] => 
                       [at(P,D),waiting(P),certified(P)]) ],
 [is_of_sort(P,package),
       is_of_sort(V,truck),
       in_city(O,CY),
       in_city(D,CY)
       ],
 [before(1,2), before(2,3), before(3,4),before(4,5)
       ],
 [   %  achieve(ss(truck,V,[moveable(V), busy(V)])),
       commission(V),
       achieve(ss(truck,V,[at(V,O)])),
       load_package(P,V,O),
       move(V,O,D,local_roads),
       unload_package(P,V,D)
       ]
).


% carry between two cities by truck
method(
  carry_direct(P,O,D),
 [ ],
  [ sc(package, P, [at(P,O),waiting(P),certified(P)] => 
                       [at(P,D),waiting(P),certified(P)]) ],
 [is_of_sort(P,package),
       is_of_sort(V,truck),
       in_city(O,CY),
       in_city(D,CY1),
       ne(CY,CY1),
       connects(R,CY,CY1),
       is_of_sort(R,road_route),
       route_available(R)
       ],
 [before(1,2), before(2,3), before(3,4),before(4,5) ],
 [   %  achieve(ss(truck,V,[moveable(V), busy(V)])),
       commission(V),
       achieve(ss(truck,V,[at(V,O)])),
       load_package(P,V,O),
       move(V,O,D,R),
       unload_package(P,V,D)
       ]
).

method(
  move_traincar(V, O, L, R2),
 [ ],
         [sc(traincar,V,[at(V,O) ]
            =>[at(V,L)] )],
 [is_of_sort(V,traincar),
       connects(R2,O,L),
       is_of_sort(R2,rail_route),
       is_of_sort(Train,train) ],
 [before(1,2), before(2,3)],
 [   achieve(ss(train,Train,[at(Train,O)])),
          attach_traincar(Train,V,O),
          pull_traincar(Train,V,O,L,R2)]
).

/* getting docs ready */
 operator( pay_fees(P),
      [],
      [sc(package,P,[uncertified(P)]
      =>[waiting(P),certified(P)])],
      [ ]).

operator(fly(A,D1,D2),
      [ ],
      [sc(aircraft,A,[at(A,D1)]
            =>[at(A,D2)] )],
         [sc(package,X,[loaded(X,A),certified(X),at(X,D1)]
            => [loaded(X,A),certified(X),at(X,D2)])  ]
).


%move truck
operator( move(V, O, L, R), 
        [ ],
         [sc(truck,V,[at(V,O),
             is_of_sort(R,road_route),
             moveable(V),
             in_city(O,City),
             in_city(L,City1),
             ne(City,City1),
             connects(R,City,City1)]
            =>[at(V,L)] )],
         [sc(package,X,[loaded(X,V),certified(X),at(X,O)] 
            => [loaded(X,V),certified(X),at(X,L)])  ]
).

%move truck inside city
operator( move(V, O, L, local_roads), 
         [],
         [sc(truck,V,[at(V,O),
             moveable(V),
             in_city(O,City),
             in_city(L,City)]
            =>[at(V,L)]  )],
         [ sc(package,X,[loaded(X,V),certified(X),at(X,O)] 
            =>[loaded(X,V),certified(X),at(X,L)])   ]
).

%move traincar
operator( pull_traincar(Train,V1, O, L, Rt), 
         [  ],
         [ sc(train,Train,[at(Train,O),
             attached(Train,V1),
             moveable(Train),
             connects(Rt,O,L),
             is_of_sort(Rt,rail_route)]
            =>[at(Train,L),attached(Train,V1)] ),
           sc(traincar,V1,[at(V1,O),attached(V1,Train)]
            =>[at(V1,L),attached(V1,Train)]) ],
         [sc(package,P,[loaded(P,V1),certified(P),at(P,O)]
            =>[loaded(P,V1),certified(P),at(P,L)]) ]
).

operator( move_train(V, O, L, Rt),
         [ ],
          [sc(train,V,[at(V,O),unattached(V),
             moveable(V),available(V),
             connects(Rt,O,L),
             is_of_sort(Rt,rail_route)]
            =>[at(V,L),unattached(V),moveable(V),available(V)] )],
       [ ]
).

operator(attach_traincar(Train,V,O),
     [  ],
     [sc(train, Train, [at(Train,O),moveable(Train),available(Train),unattached(Train)]
        =>[at(Train,O),attached(Train,V),moveable(Train),busy(Train)] ),
     sc(traincar, V, [at(V,O),unattached(V)]
        =>[at(V,O),attached(V,Train)] ) ],
     [ ]
).

operator(detach_traincar(Train,V,O),
     [ ],
     [sc(train, Train, [attached(Train,V),moveable(Train),busy(Train)]
        =>[unattached(Train),moveable(Train),available(Train)] ),
     sc(traincar, V, [attached(V,Train)]
        =>[unattached(V)] ) ],
     [ ]
).

operator(commission(V),
      [ ],
      [sc(vehicle, V,[moveable(V),available(V)] =>[moveable(V), busy(V)])],
      [ ]).

         

operator( load_package(P,V,L),
[se(vehicle,V,[at(V,L)])],
[sc(package, P, [at(P,L),waiting(P),certified(P)]=>
   [at(P,L),loaded(P,V),certified(P)]
)],
[]
).

/*
 operator( unload_plane(P,V,L),
 [ss(vehicle,V, [at(V,L)])],
 [sc(package, P, [at(P,L),loaded(P,V),certified(P)]=>[at(P,L),waiting(P),certified(P)]
 )],
 []
 ).
*/

% operator( unload_package(P,V,L),
% [ss(vehicle,V, [at(V,L)])],
% [sc(package, P, [at(P,L),loaded(P,V),certified(P)]=>[at(P,L),waiting(P),certified(P)]
% )],
% []
% ).

operator( unload_package(P,V,L),
  [],
  [sc(package, P, [at(P,L),loaded(P,V),certified(P)]=>[at(P,L),waiting(P),certified(P)]),
  sc(vehicle,V, [at(V,L),moveable(V),busy(V)] => [at(V,L),moveable(V),available(V)])
 ],
  []
  ).


operator( deliver(P,L),
        [],
        [sc(package, P, [at(P,L),waiting(P),certified(P)]=>
          [at(P,L),delivered(P)] )],
        []
).

