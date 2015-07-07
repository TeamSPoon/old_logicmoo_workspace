%  The job is to search a binary tree for a target.
%  Each node on the tree is either a target node, a non-target leaf node, 
%    or a non-target internal node with a left and right child.
%  The actions are: (1) check which of these 3 cases holds of the current node;
%    (2) move down to a left or right child; or (3) move up to a parent.
%  A stack of moves is needed to solve this problem. So the move down actions
%    also push the move onto a stack, and the move up action pops the stack 
%    and returns the popped value as a sensing result.

:- include(fsaplanner).

:- max_state(5).

% One limitation: this version only allows searching the left branch first.

first_dir(left).   % first branch to search
last_dir(right).   % second branch to search

candidate_direction(D) :- first_dir(D) ; last_dir(D).
candidate_node_type(V) :- V=target ; V=leaf ; V=internal.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prim_action(push_down_to(D),[ok]) :- candidate_direction(D).
prim_action(pop_up_from,L) :- bagof(D,candidate_direction(D),L).
prim_action(check_node_type,L) :- bagof(V,candidate_node_type(V),L).

prim_fluent(stack).       % a stack of left or right moves
prim_fluent(current).     % type of current node: target, leaf, or internal
prim_fluent(new_visit).   % is this the first time visiting the current node?
prim_fluent(depth_max).   % unknown bound on how deep to search

causes(push_down_to(D),_,stack,S,S=[D|stack]).
causes(push_down_to(_),_,new_visit,true,true).
causes(push_down_to(_),_,current,V,true) :- candidate_node_type(V).
causes(push_down_to(_),_,depth_max,N,N is depth_max-1).

causes(pop_up_from,D,stack,S,stack=[D|S]).
causes(pop_up_from,_,new_visit,false,true).
causes(pop_up_from,_,current,internal,true).
causes(pop_up_from,_,depth_max,N,N is depth_max+1).

settles(check_node_type,V,current,V,true).
rejects(check_node_type,internal,depth_max,0,true).
rejects(check_node_type,leaf,stack,S,all(x,member(x,S),last_dir(x))).
rejects(pop_up_from,left,stack,[right|_],true).
rejects(pop_up_from,right,stack,[left|_],true).

poss(push_down_to(D),
   and(current=internal,(new_visit -> first_dir(D) ; last_dir(D)))).
poss(pop_up_from, neg(or(current=target,stack=[]))).
poss(check_node_type, true).

init(stack,[]).
init(current,V) :- candidate_node_type(V).
init(new_visit,true).
parm_fluent(depth_max).
init_parm(generate,depth_max,2).
init_parm(test,depth_max,3).

% the goal is to get the current node to be a target node
top :- kplan(current=target).


