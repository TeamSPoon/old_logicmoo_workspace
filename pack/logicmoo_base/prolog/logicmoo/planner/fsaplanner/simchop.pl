%% The goal is to get a tree down and store the axe.
%% In this version, a bound is known on how many chops will be needed

:-include(fsaplanner).
:-filter_beyond_goal.

prim_action(chop,[ok]).		  % hit the tree with the axe
prim_action(look,[down,up]).	  % look if the tree is up or down
prim_action(store,[ok]).	  % put away the axe

prim_fluent(axe).                 % stored or out
prim_fluent(tree).	          % up or down
prim_fluent(chops_max).	          % bound on the number of chops needed

poss(chop,and(axe=out,tree=up)).
poss(look,true).
poss(store,axe=out).

causes(store,axe,stored,true). % store puts the axe away
causes(chop,chops_max,X,X is chops_max-1).  % chopping lowers the bound
causes(chop,tree,down,true).   % after a chop, the tree may be down
causes(chop,tree,up,true).     % after a chop, the tree may be up

% looking determines whether or not the tree is up
settles(look,X,tree,X,true).
% if the tree is seen to be up, chops_max cannot be 0
rejects(look,up,chops_max,0,true).

init(axe,out).      % the axe is out and available
init(tree,up).      % the tree may be up
init(tree,down).    % the tree may be down
init(chops_max,2).  % at most 2 chops will be needed

top :- kplan(and(tree=down,axe=stored)).
