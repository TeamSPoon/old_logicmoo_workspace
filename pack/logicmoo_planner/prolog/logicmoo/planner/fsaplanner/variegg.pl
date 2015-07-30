%%  Get eggs for an omelette without knowing how many eggs are needed or
%%    how many bad eggs will be seen before getting there

:- include(fsaplanner).
:- max_state(6).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prim_action(next_to_dish,[ok]).    % break new egg into dish
prim_action(dump_dish,[ok]).       % discard contents of dish
prim_action(dish_to_bowl,[ok]).    % transfer contents of dish to bowl
prim_action(sniff_dish,[good_egg,bad_egg]).      % is egg in dish good?
prim_action(check_bowl,[enough_eggs,need_eggs]). % do we have enough eggs?
 
prim_fluent(dish).            % empty, good_egg, or bad_egg
prim_fluent(bowl).            % enough_eggs or need_eggs
prim_fluent(egg_max).         % max eggs to break

poss(dump_dish,dish=bad_egg).
poss(dish_to_bowl,dish=good_egg).
poss(next_to_dish,dish=empty).
poss(sniff_dish,neg(dish=empty)).
poss(check_bowl,true).

causes(dump_dish,dish,empty,true).

causes(dish_to_bowl,dish,empty,true).
causes(dish_to_bowl,bowl,enough_eggs,true).
causes(dish_to_bowl,bowl,need_eggs,true).

causes(next_to_dish,dish,bad_egg,true).
causes(next_to_dish,dish,good_egg,true).
causes(next_to_dish,egg_max,V,V is egg_max-1).

settles(sniff_dish,X,dish,X,true).
rejects(sniff_dish,bad_egg,egg_max,0,bowl=need_eggs).

settles(check_bowl,X,bowl,X,true).
rejects(check_bowl,need_eggs,egg_max,0,neg(dish=good_egg)).

init(dish,empty).
init(bowl,need_eggs).
init(bowl,enough_eggs).
parm_fluent(egg_max).
init_parm(generate,egg_max,X) :- X=0;X=1;X=2.
init_parm(test,egg_max,6).

top :- kplan(bowl=enough_eggs).
