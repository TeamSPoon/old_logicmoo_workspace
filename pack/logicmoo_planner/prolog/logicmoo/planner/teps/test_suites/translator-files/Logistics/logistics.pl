fluent(obj(_)).
fluent(truck(_)).
fluent(location(_)).
fluent(airplane(_)).
fluent(city(_)).
fluent(airport(_)).
fluent(at(_,_)).
fluent(in(_,_)).
fluent(in_city(_,_)).

action(load(_,_,_)).
action(unload(_,_,_)).
action(drive_truck(_,_,_,_)).
action(fly_airplane(_,_,_)).

poss(load(Object,Vehicle,Location),and(obj(Object),
				       and(or(airplane(Vehicle),truck(Vehicle)),
					   and(at(Object,Location),at(Vehicle,Location))))).

poss(unload(Obj,Vehicle,Loc),and(obj(Obj),and(or(airplane(Vehicle),truck(Vehicle)),
					      and(location(Loc),and(in(Obj,Vehicle),at(Vehicle,Loc)))))).


poss(drive_truck(Truck,From,To,City),and(truck(Truck),
					and(location(From),
					    and(location(To),
						and(city(City),
						    and(at(Truck,From),
							and(in_city(From,City),in_city(To,City)))))))).

poss(fly_airplane(Airplane,From,To),and(airplane(Airplane),
					and(airport(From),
					    and(airport(To),at(Airplane,From))))).


% the load action
causes_true(in(Obj,Vehicle),load(Obj,Vehicle,_),true).
causes_false(at(Obj,Loc),load(Obj,_,Loc),true).

% the unload action
causes_false(in(Obj,Vehicle),unload(Obj,Vehicle,_),true).
causes_true(at(Obj,Loc),unload(Obj,_,Loc),true).

% the drive_truck action
causes_true(at(Truck,To),drive_truck(Truck,_,To,_),true).
causes_false(at(Truck,From),drive_truck(Truck,From,_,_),true).

% the fly_airplane action
causes_true(at(Airplane,To),fly_airplane(Airplane,_,To),true).
causes_false(at(Airplane,From),fly_airplane(Airplane,From,_),true).

