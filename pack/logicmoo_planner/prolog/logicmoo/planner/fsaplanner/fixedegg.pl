%%  Get eggs for an omelette without knowing how many bad eggs 
%%    will be seen before getting the good ones needed.

:- include(fsaplanner).
:- gen_max(40).
:- max_state(50).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prim_action(dump_dish,[ok]).                  % discard contents of dish   
prim_action(sniff_dish,[good_egg,bad_egg]).   % smell to see if egg is bad
prim_action(next_to_dish,[ok]).               % break new egg in dish
prim_action(dish_to_bowl,[ok]).               % transfer dish egg to bowl
 
prim_fluent(dish).            % empty, good_egg, or bad_egg
prim_fluent(bowl).            % number of good eggs in bowl
prim_fluent(bad_max).         % bound on num of bad eggs, unknown.

poss(dump_dish,dish=bad_egg).
poss(dish_to_bowl,dish=good_egg).
poss(next_to_dish,dish=empty).
poss(sniff_dish,neg(dish=empty)).

causes(dump_dish,bad_max,X,X is bad_max-1).
causes(dump_dish,dish,empty,true).

causes(dish_to_bowl,bowl,X,X is bowl+1).
causes(dish_to_bowl,dish,empty,true).

causes(next_to_dish,dish,bad_egg,true).
causes(next_to_dish,dish,good_egg,true).

settles(sniff_dish,X,dish,X,true).
rejects(sniff_dish,bad_egg,bad_max,0,true).

init(dish,empty).
init(bowl,0).
parm_fluent(bad_max).
init_parm(generate,bad_max,1).
init_parm(test,bad_max,2).

top :- write('Type top(k), where k is the number of eggs desired.').
top(N) :- kplan(bowl=N).

