:- include(fsaplanner).

% There's a bunch of blocks on A, an equal number of red and blue.
% There's no blocks on B or C.  The object is to get all the blocks
% onto C, alternating red and blue, with red at the bottom.

prim_action(testA,[empty,nonempty]).
prim_action(testB,[empty,nonempty]).
prim_action(testH,[red,blue]).
prim_action(pickA,[ok]).
prim_action(pickB,[ok]).
prim_action(putB,[ok]).
prim_action(putC,[ok]).

prim_fluent(onA).                  % number of blocks on A  (unknown)
prim_fluent(onB).                  % number of blocks on B
prim_fluent(topB).                 % the block at the top of B.
prim_fluent(wantC).                % the block to go on C next
prim_fluent(holding).              % the block being held

opp_colour(blue,red).
opp_colour(red,blue).

poss(testA,true). 
poss(testB,true).
poss(testH,neg(holding=nothing)).
poss(pickA,and(holding=nothing,onA>0)).
poss(pickB,and(holding=nothing,onB>0)).
poss(putC,holding=wantC).
poss(putB,opp_colour(holding,wantC)).

causes(pickA,onA,N,N is onA-1).
causes(pickB,onB,N,N is onB-1).
causes(putB,onB,N,N is onB+1).

causes(putC,wantC,X,opp_colour(wantC,X)).
causes(putB,topB,X,holding=X).
causes(putC,holding,nothing,true).
causes(putB,holding,nothing,true).
causes(pickB,holding,X,topB=X).
causes(pickA,holding,blue,true).   % one possible value
causes(pickA,holding,red,true).    % the other possible value

settles(testA,empty,onA,0,true).
rejects(testA,nonempty,onA,0,true).
settles(testB,empty,onB,0,true).
rejects(testB,nonempty,onB,0,true).
settles(testH,X,holding,X,true).

% The key!
rejects(testH,X,onA,Y,and(neg(wantC=X),between(0,onB,Y))).

init(topB,nothing).
init(wantC,red).
init(holding,nothing).
init(onB,0).
parm_fluent(onA).
init_parm(generate,onA,V) :- V=0 ; V=2 ; V=4.
init_parm(test,onA,6). 

top :- mygoal(G), kplan(G).
mygoal(G) :- G=and(onA=0,and(onB=0,holding=nothing)).

:- gen_max(25).
:- test_max(35).
:- max_state(16).

:- good_action(testA,and(wantC=red,holding=nothing)).
:- good_action(testB,now=[o(putC,_)|_]).
