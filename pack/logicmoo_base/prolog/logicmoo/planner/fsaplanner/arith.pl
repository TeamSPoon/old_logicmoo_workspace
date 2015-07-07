%  An arithmetic job: you have two accumulators, Acc(1) and Acc(2), and
%  given that input > 0, the goal is to make Acc(2) = 2*input-1.
%  The actions: increment an accumulator (both start at 0).
%  The tests:  test if the value of Acc(1) = input.

:- include(fsaplanner).
:- filter_beyond_goal.
:- max_state(5).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

acc_num(1).  
acc_num(2).

prim_fluent(input).                   % the unknown fluent
prim_fluent(acc(N)) :- acc_num(N).    % the two accumulators

prim_action(increment_acc(N),[ok]) :- acc_num(N).
prim_action(input_equals_acc1,[true,false]).

poss(increment_acc(_),true).
poss(input_equals_acc1,true).

causes(increment_acc(N),acc(N),V,V is acc(N)+1).
settles(input_equals_acc1,true,input,V,V=acc(1)).
rejects(input_equals_acc1,false,input,V,V=acc(1)).

init(acc(_),0).                 % accumulators start at 0

parm_fluent(input).
init_parm(generate,input,V) :- V=1 ; V=2.
init_parm(test,input,V) :- V=1 ; V=2; V=3.

top :- kplan(acc(2) is 2*input-1).


