
% end_of_file.

% robot world - OCL1.1

% sorts
% ------------------
sorts(primitive_sorts,[room, door, box, robot]) .

% objects
%-------------------
objects(room, 
	[room1, room2, room3, room4, room5, room6, room7]) .
objects(door, [door12, door23, door24, door25, door35, door45, door47, door56, door57,door67]) .
objects(box, [box1, box2, big_box]) .
objects(robot, [tom]) .

% All predicates
%-------------------
predicates([
	in_room(box,room),
	at_door(box,door,room),
	robot_in(robot,room),
	robot_at(robot,door,room),
	robot_near(robot,box),
	connect(room,room,door)
	]) .

% atomic invariants
%-------------------
atomic_invariants([ connect(room2,room3,door23),
		    connect(room3,room2,door23),
                    connect(room2,room4,door24),
		    connect(room4,room2,door24),
                    connect(room2,room5,door25),
		    connect(room5,room2,door25),
                    connect(room6,room5,door56),
		    connect(room5,room6,door56),
                    connect(room5,room3,door35),
		    connect(room3,room5,door35),
                    connect(room5,room4,door45),
 		    connect(room4,room5,door45),
                    connect(room4,room7,door47),
		    connect(room7,room4,door47),
                    connect(room5,room7,door57),
		    connect(room7,room5,door57),
                    connect(room6,room7,door67),
		    connect(room7,room6,door67),
                    connect(room2,room1,door12),
		    connect(room1,room2,door12)  ]).

% substate classes
%-------------------
substate_classes(box, Box, [
		[in_room(Box, Room), at_door(Box, Door,Room), 
			connect(Room, Room1, Door)],
		[in_room(Box, Room)] ]).

substate_classes(robot, Robot,[
		[robot_in(Robot,Room), robot_at(Robot,Door,Room), 
			connect(Room, Room1, Door)],
       		[robot_in(Robot,Room), robot_near(Robot,Box)],
		[robot_in(Robot,Room), robot_near(Robot,Box), 
                        robot_at(Robot,Door,Room),
		 	connect(Room, Room1, Door)],
		[robot_in(Robot,Room)] ]).

% negative invariants
%-------------------

inconsistent_constraint([in_room(B, R1), in_room(B, R2), ne(R1, R2)]) .
inconsistent_constraint([in_room(B, R1), at_door(B, Door, R2), ne(R1, R2)]) .
inconsistent_constraint([at_door(B, Door, R1), at_door(B, Door, R2), 
		ne(R1, R2)]) .

inconsistent_constraint([at_door(B, Door, Room), at_door(B1, Door, Room), 
		ne(B, B1)]) .

inconsistent_constraint([robot_near(Robot,B1), robot_near(Robot,B2), ne(B1,B2)]) .

inconsistent_constraint([robot_in(Robot,R1), robot_in(Robot,R2), ne(R1,R2)]) .
inconsistent_constraint([robot_in(Robot,R1), robot_at(Robot,Door, R2), 
                ne(R1,R2)]) .
inconsistent_constraint([robot_at(Robot,Door, R1), robot_at(Robot,Door, R2), 
                ne(R1,R2)]) .

inconsistent_constraint([in_room(B, R1), robot_in(Robot,R2), 
                robot_near(Robot,B), ne(R1,R2)]) .
inconsistent_constraint([in_room(B, R1), robot_near(Robot,B), 
                robot_at(Robot,Door,R2),ne(R1,R2)]) .

% implied invariants
% --------------------------------
implied_invariant([robot_in(robot,Room),robot_near(robot,Box)],
	[in_room(Box,Room)]) .

% operators
% -------------

operator(goto_box(Robot,Box,Room), 
     % prevail
     [se(box, Box,[in_room(Box,Room)])],
     % necessary
     [sc(robot, Robot, [robot_in(Robot,Room)] => 
                  [robot_in(Robot,Room),robot_near(Robot,Box)])],
     % conditional
     []) .

operator(leave_box(Robot,Box,Room), 
     % prevail
     [se(box,Box,[in_room(Box,Room)])],
     % necessary
     [sc(robot, Robot, [robot_in(Robot,Room),robot_near(Robot,Box)]=> 
                  [robot_in(Robot,Room)])],
     % conditional
     []) .

operator(gotodoor(Robot,Door,Room), 
     % prevail
      [],
     % necessary
     [sc(robot, Robot, [robot_in(Robot,Room)]=> 
              [robot_in(Robot,Room),robot_at(Robot,Door,Room),
               connect(Room,R,Door)])],
     % conditional
     []) .

operator(gothrudoor(Robot,Door,Room,R), 
     % prevail
     [],
     % necessary
     [sc(robot, Robot, [robot_in(Robot,Room),robot_at(Robot,Door,Room), 
            connect(Room,R,Door)] => [robot_in(Robot,R)])],
     % conditional
     []) .


operator(pushtodoor(Robot,Box,Door,Room), 
     % prevail
     [],
     % necessary
     [sc(box,Box, [in_room(Box,Room)]=>
              [in_room(Box, Room), at_door(Box, Door,Room), 
               connect(Room, Room1, Door)]), 
      sc(robot, Robot, [robot_in(Robot,Room), robot_near(Robot,Box)]=>
	      [robot_in(Robot,Room),robot_near(Robot,Box),
               robot_at(Robot,Door,Room),connect(Room,R,Door)])],
     % conditional
     []) .
 
operator(pushthrudoor(Robot,Box,Door,Room,Room1), 
     % prevail
     [],
     % necessary
     [sc(box,Box, [in_room(Box, Room), at_door(Box, Door,Room), 
              connect(Room, Room1, Door)] => [in_room(Box,Room1)]),
      sc(robot, Robot,[robot_in(Robot,Room), robot_near(Robot,Box), 
              robot_at(Robot,Door,Room),connect(Room,Room1,Door)]=>
              [robot_in(Robot,Room1),robot_near(Robot,Box)])],
     % conditional
     []) .






planner_task(1,
[se(box,box1,[at_door(box1,door24,room2)]),
 se(box,box2,[in_room(box2,room5),at_door(box2,door56,room5)]),
 se(box,big_box,[in_room(big_box,room2),at_door(big_box,door12,room2)])],

[ss(robot,tom,[robot_in(tom,room2)]),
 ss(box,box1,[in_room(box1,room2),at_door(box1,door23,room2)]),
 ss(box,box2,[in_room(box2,room5)]),
 ss(box,big_box,[in_room(big_box,room4),at_door(big_box,door24,room4)])
]).

planner_task(2,
[se(box,box1,[in_room(box1,room6)])],

[ss(robot,tom,[robot_in(tom,room2)]),
 ss(box,box1,[in_room(box1,room2),at_door(box1,door23,room2)]),
 ss(box,box2,[in_room(box2,room6)]),
 ss(box,big_box,[in_room(big_box,room7),at_door(big_box,door57,room7)])
]).

planner_task(3,
[se(robot,tom,[robot_in(tom,room3),robot_near(tom,big_box)]),
 se(box,box2,[in_room(box2,room6),at_door(box2,door67,room6)])],

[ss(robot,tom,[robot_in(tom,room1),robot_near(tom,box1)]),
 ss(box,box1,[in_room(box1,room1)]),
 ss(box,box2,[in_room(box2,room7),at_door(box2,door67,room7)]),
 ss(box,big_box,[in_room(big_box,room4)])
]).

planner_task(4,
[se(box,box1,[in_room(box1,room2)])],

[ss(robot,tom,[robot_in(tom,room4),robot_near(tom,box2)]),
 ss(box,box1,[in_room(box1,room5)]),
 ss(box,box2,[in_room(box2,room4)]),
 ss(box,big_box,[in_room(big_box,room4)])
]).

planner_task(5,
[se(robot,tom,[robot_in(tom,room5)]),
se(box,box2,[in_room(box2,room6),at_door(box2,door56,room6)]),
se(box,big_box,[in_room(big_box,room1)])],

[ss(robot,tom,[robot_in(tom,room2),robot_near(tom,box1)]),
 ss(box,box1,[in_room(box1,room2),at_door(box1,door25,room2)]),
 ss(box,box2,[in_room(box2,room6)]),
 ss(box,big_box,[in_room(big_box,room4),at_door(big_box,door24,room4)])
]).


