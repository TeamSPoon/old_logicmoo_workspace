%  An arithmetic job: you have two accumulators, Acc(1) and Acc(2), and
%  given that input > 0, the goal is to make Acc(2) = factorial(input).
%  The actions: increment an accumulator (both start at 0)
%               multiply one accumulator by another
%  The tests:  test if the value of acc1 = input.

:- include(fsaplanner).
:- gen_max(8).
:- max_state(5).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

acc_num(1).  
acc_num(2).

prim_fluent(input).                   % the unknown fluent
prim_fluent(acc(N)) :- acc_num(N).    % the two accumulators

prim_action(increment_acc(N),[ok]) :- acc_num(N).
prim_action(multiply_acc_by_acc(N,M),[ok]) :- acc_num(N), acc_num(M).
prim_action(input_equals_acc(1),[true,false]).

poss(increment_acc(_),true).
poss(multiply_acc_by_acc(_,_),true).
poss(input_equals_acc(1),true).

causes(increment_acc(N),acc(N),V,V is acc(N)+1).
causes(multiply_acc_by_acc(N,M),acc(N),V,V is acc(N)*acc(M)).
settles(input_equals_acc(1),true,input,V,V=acc(1)).
rejects(input_equals_acc(1),false,input,V,V=acc(1)).

init(acc(_),0).                 % accumulators start at 0

parm_fluent(input).
init_parm(generate,input,V) :- inbetween(0,V,1). 
init_parm(test,input,V) :- inbetween(0,V,3). 

top :- kplan(fact(input,acc(2))).

% definition of factorial
fact(0,1).
fact(N,M) :- N>0, N1 is N-1, fact(N1,M1), M is N*M1.

% inbetween(+I,-J,+K) is I <= J <= K.
inbetween(_,M,M).
inbetween(N,K,M) :- N<M, M1 is M-1, inbetween(N,K,M1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  A big hint: don't bother checking to see if Acc(2) = input
%  And only change Acc(2) by multiplication

:- good_action(input_equals_acc(N),N=1).
:- good_action(multiply_acc_by_acc(N,M),and(N=2,acc(M)>0)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

