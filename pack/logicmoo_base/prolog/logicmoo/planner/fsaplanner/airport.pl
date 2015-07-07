%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  The robot wants to board a plan with newspaper and a hot drink.
%  Objects are for sale at different locations.
%  To board the plane, the robot must be at the correct terminal.
%  In this version, the boarding gate is unknown, but there is a 
%    sensing action, read_screen, which says what the boarding gate is
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- include(fsaplanner).
:- dynamic for_sale/2, drink/1, gate_location/2, gate/1.

%%%%%%%%%%%%%%%%%  Static background knowledge  %%%%%%%%%%%%%%%%%%%%%%%%%%%%

for_sale(newspaper,departures).          drink(cola).
for_sale(coffee,terminal1).              drink(coffee).
for_sale(tea,terminal2).                 drink(tea).

gate_location(gate_a,terminal1).         gate(gate_a).
gate_location(gate_b,terminal2).         gate(gate_b).
gate_location(gate_c,terminal1).         gate(gate_c).

%%%%%%%%%%%%%%%%%  Dynamic background knowledge  %%%%%%%%%%%%%%%%%%%%%%%%%%%

prim_fluent(have(_)).           % have(X) is true if robot bought X
prim_fluent(location).          % where the robot is located
prim_fluent(on_board).          % true if the robot has boarded the plane
prim_fluent(boarding_gate).     % where the plane is parked, unknown.

prim_action(buy(_),[ok]).       % buy(X) causes robot to have X
prim_action(go(_),[ok]).        % go(X) changes the robot location to X
prim_action(board,[ok]).        % board causes on_board to be true
prim_action(read_screen,[gate_a,gate_b,gate_c]).  % read the departure screen

% preconditions for actions
poss(buy(Item),location=L) :- for_sale(Item,L).
poss(board,and(boarding_gate=G,location=L)) :- gate_location(G,L).
poss(go(_), true).
poss(read_screen,location=departures).

% the causal laws
causes(buy(Item),have(Item),true,true).
causes(go(L),location,L,true).
causes(board,on_board,true,true).
causes(board,location,in_transit,true).

% sensing the boarding gate
settles(read_screen,G,boarding_gate,G,true).

% all fluents have known initial values.
init(have(_),false).
init(location,home).
init(on_board,false).
init(boarding_gate,G) :- gate(G).   % can be any of the possible gates

% Top goal is having newspaper, a drink, and being on the plane

top :- kplan(and(on_board, 
                 and(some(x,drink(x),have(x)),
                     have(newspaper)))).
