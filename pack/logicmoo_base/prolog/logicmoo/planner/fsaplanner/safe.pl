%%  The goal is to get a safe open by opening its door.  
%%  The door only opens after a certain sequence of buttons has been pushed.
%%  This sequence is not known in advance, but is written on a piece of paper.
%%  If the paper is picked up, each digit in the sequence can be read in turn.

:-include(fsaplanner).

button_num(0).
button_num(1). 

prim_action(pickup_paper,[ok]).
prim_action(open_safe_door,[ok]).
prim_action(push_button(N),[ok]) :- button_num(N).
prim_action(read_next_button,[done|L]) :- setof(N,button_num(N),L).

prim_fluent(have_paper).        % true or false
prim_fluent(safe).              % open, closed, or jammed
prim_fluent(no_wrong_pushes).   % true or false
prim_fluent(next_button).       % a button to push, or 'done'
prim_fluent(buttons_max).       % bound on pushes needed (unknown)

poss(pickup_paper,neg(have_paper)).
poss(open_safe_door,safe=closed).
poss(read_next_button,have_paper).
poss(push_button(_),true).

causes(pickup_paper,have_paper,true,true).

causes(open_safe_door,safe,open,
   and(next_button=done,no_wrong_pushes=true)).
causes(open_safe_door,safe,jammed,
   neg(and(next_button=done,no_wrong_pushes=true))).

causes(push_button(N),no_wrong_pushes,true,
   and(no_wrong_pushes=true,next_button=N)).
causes(push_button(N),no_wrong_pushes,false,
   neg(and(no_wrong_pushes=true,next_button=N))).
causes(push_button(_),buttons_max,V,V is buttons_max-1).
causes(push_button(_),next_button,V,true) :- V=done;button_num(V).

settles(read_next_button,N,next_button,N,true).
rejects(read_next_button,N,buttons_max,0,true) :- button_num(N).

init(have_paper,false).
init(safe,closed).
init(next_button,N) :- N=done;button_num(N).
init(no_wrong_pushes,true).

parm_fluent(buttons_max).    
init_parm(generate,buttons_max,1). 
init_parm(test,buttons_max,4). 

top :- kplan(safe=open).
