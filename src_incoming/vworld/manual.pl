% manual.pl
% July 1, 1995
% John Eikenberry
% Dec 13, 2035
% Douglas Miles
%
/** <module> 
% now unused.. it could exist when we want a 
% manual agent... (meant for debugging purposes)
%
*/

:- module(man,[]).

end_of_file.

:- dynamic take/2.

% write out stats and percepts and ask for an action
% Possible agent actions.
:- include(logicmoo('vworld/vworld_header.pl')).
:- register_module_type(utility).

moo:agent_call_command(_,_):-fail.

actr:agent_action(Actor,Action) :-var(Action),!,
        look_to_conso;e(Actor),
	write('What would you like to do?'), nl,
	write('(1=take, 2=sit, 3=move, 4=climb, 5=push, 6=drop, 7=eat, 8=attack)'), nl,
	write('---?--->'), get_single_char(Num),
	which_one(Num,_,Action), nl,!.


% Complete the action chosen
% Take something
which_one(49,[take,obj(1)],Take) :-
	list_objects,
	write('Take what: '),
	findall(take(N,O),take_object(N,O),List),
	forall(member(take(Num,Obj),List),
	(write(Num), write('='), write(Obj), write(' '))),
	get_single_char(Char),
	convert_num(Char,Number),
	take_object(Number,Obj),
	Take = take(Obj).
% Drop something
which_one(54,[drop,obj(1)],Drop) :-
	write('Drop what: '),
	findall(take(N,O),take_object(N,O),List),
	forall(member(take(Num,Obj),List),
	(write(Num), write('='), write(Obj), write(' '))),
	get_single_char(Char),
	convert_num(Char,Number),
	take_object(Number,Obj),
	Drop = drop(Obj).
% Eat something held
which_one(55,[eat,obj(1)],Eat) :-
	write('Eat what: '),
	findall(take(N,O),take_object(N,O),List),
	forall(member(take(Num,Obj),List),
	(write(Num), write('='), write(Obj), write(' '))),
	get_single_char(Char),
	convert_num(Char,Number),
	take_object(Number,Obj),
	Eat = eat(Obj).
%Sit there
which_one(50,[sit],sit).
% Move 
which_one(51,[move,dir(1)],Move) :-
	write('Ok. Which way (1=n,2=s,3=e,4=w,5=ne,6=nw,7=se,8=sw) : '),
	get_single_char(Num), 
	move_dir(Num,Dir),
	Move = move(Dir).
% Climb
which_one(52,[climb,dir(1)],Climb) :-
	write('Which way (1=n,2=s,3=e,4=w,5=ne,6=nw,7=se,8=sw) : '),
	get_single_char(Num),
	move_dir(Num,Dir),
	Climb = climb(Dir).
% Push
which_one(53,[push,dir(1)],Push) :-
	write('Which way (1=n,2=s,3=e,4=w,5=ne,6=nw,7=se,8=sw) : '),
	get_single_char(Num),
	move_dir(Num,Dir),
	Push = push(Dir).
% Attack
which_one(56,[attack,dir(1)],Attack) :-
	write('Ok. Which way (1=n,2=s,3=e,4=w,5=ne,6=nw,7=se,8=sw) : '),
	get_single_char(Num), 
	move_dir(Num,Dir),
	Attack = attack(Dir).

% make list of the objects in the world
:-dynamic list_objects_num/1.
list_objects :-
	take_object(A,_),
	A \== 0.

list_objects :-
	world_retractall(list_objects_num(_)),
	world_assert(list_objects_num(0)),
	world_retractall(take_object(_,_)),
	findall(Obj,
	     (kquery(Obj,_), Obj \= 0),
	     List),
	forall(member(Obj,List),
	     (advance_counter, 
	     list_objects_num(Num), 
	     world_assert(take_object(Num,Obj)))),
	world_retract(list_objects_num(_)).

advance_counter :-
	world_retract(list_objects_num(Old)),
	New is Old + 1,
	world_assert(list_objects_num(New)).

convert_num(49,1).
convert_num(50,2).
convert_num(51,3).
convert_num(52,4).
convert_num(53,5).
convert_num(54,6).
convert_num(55,7).
convert_num(56,8).
convert_num(57,9).
convert_num(58,10).


move_dir(49,n).
move_dir(50,s).
move_dir(51,e).
move_dir(52,w).
move_dir(53,ne).
move_dir(54,nw).
move_dir(55,se).
move_dir(56,sw).
move_dir(0'd,d).
move_dir(0'u,u).

take_object(0,_).

:- include(logicmoo('vworld/vworld_footer.pl')).


