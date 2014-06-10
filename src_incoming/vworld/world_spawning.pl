/** <module>  
% File is responsible for 
%  laying out new objects in the mud based on some frame rules
%
%
% Project LogicMoo: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/

/*
% This file is "included" from world.pl 
*/


amzi_timer(T1):-get_time(T1).


moo:on_world_load :- retractall(spawn_objects(_)).

growth :-
	findall(([Obj,Chance]),
	    props(Obj,spawn_rate(Chance)),
	    Objects),
	assert(spawn_objects(Objects)).



% For Quintus alter 'Y is random(LLL)' to random(0,LLL,Y)
% defined pred maybe.... be sure to comment this pred out before going
% back to quitus (located below).
spread :-
	spawn_objects(Objects),
	forall(prop_memb([Object,Chance],Objects),
	spread(Object,Chance)).

spread(Object,Chance) :-        
	maybe(Chance),!,
        gensym(Object,Named),
        create_instance(Named,Object,[]),!.

spread(_,_).

:-style_check(-singleton).
:-style_check(-discontiguous).


csdmsg(M):-'format'('% ~q. ~n', [M]).




:-style_check(-singleton).
:-style_check(-discontiguous).


callStub(P,F,A):- predicate_property(P,number_of_clauses(N)),(N==1 -> (csdmsg(failed(callStub(P,F,A),!,fail))); ((functor(PP,F,A),csdmsg(callStub(P,F,A)),retractall((PP:-callStub(PP,F,A))),callStub(P,F,A)))).

createStub(F,A):- dynamic(F/A),!. % functor(P,F,A),asserta((P:-callStub(P,F,A))).

/*

:- forall(member(F/A,[
      conflict_set/1,  
      cls/0,  
      %create_instance/3,  
      frinst/4,  
      list/1,  
      mea/1,  
      prop_memb/2,  
      spawn_objects/1,  
      string_integer/2,  
      string_list/2,  
      frame/2,
      old_flots/3,
      instantiation/1,

                 root/3,
      bi/4,
      tes/4,
      rul/3,
      varg/1,
      nid/1
      ]),createStub(F,A)).

*/

% FOOPS - an integration of frames, forward chaining with LEX and MEA,
% and Prolog.
% Copyright (c) Dennis Merritt, 1986 - Permission granted for 
% non-commercial use

% The first section of the code contains the basic OOPS code, the
% second section contains the FRAMES code.

% operator definitions

:-op(800,xfx,==>).          % used to separate LHS and RHS of rule
:-op(500,xfy,:).            % used to separate attributes and values
:-op(810,fx,rule).          % used to define rule
:-op(700,xfy,#).            % used for unification instead of =
:-op(700,xfy,\=).	          % not equal
:-op(600,xfy,with).		    % used for frame instances in rules

main :- welcome, supervisor.

welcome  :-
	write('FOOPS - A Toy Production System'),nl,nl,
	write('This is an interpreter for files containing rules coded in the'),nl,
	write('FOOPS format.'),nl,nl,
	write('The => prompt accepts four commands:'),nl,nl,
	write('   load.       - prompts for name of rules file'),nl,
	write('                 enclose in single quotes'),nl,
	write('   list.       - lists stuff'),nl,
	write('   go.         -  starts the inference'),nl,
	write('   exit.       - does what you"d expect'),nl,nl,
        load,ignore(lst).


% the supervisor, uses a repeat fail loop to read and process commands
% from the user

supervisor :-
	repeat,
	write('=>'),
	read(X),
	doit(X),
	X = exit.

doit(X) :- do(X).

% actions to take based on commands

do(exit) :- !.
do(go) :-!,
	initialize,
	amzi_timer(T1),
	go,
	amzi_timer(T2),
	T is 10 * (T2 - T1),
	write(time-T),nl,!.
do(load) :- load, !.
do(list) :- lst, !.       % lists all of working storage
do(list(X)) :- lst(X), !. % lists all which match the pattern
do(_) :- write('invalid command'),nl.

% loads the rules (Prolog terms) into the Prolog database

load :- reconsult('room.dyn'),!.
load :-
	write('Enter the file name in single quotes (ex. ''room.fkb''.): '),
	read(F),
	reconsult(F).            % loads a rule file into interpreter work space

% assert each of the initial conditions into working storage

initialize :-
	setchron(1),
	createStub(instantiation,1),
	delf(all),
	assert(mea(no)),
	assert(gid(100)),
	initial_data(X),
	assert_list(X), !.
initialize :-
	error(301,[initialization,error]).

% working storage is represented by database terms stored
% under the key "fact"

assert_list([]) :- !.
assert_list([H|T]) :-
	getchron(Time),
	assert_ws( fact(H,Time) ),
	!,assert_list(T).

% the main inference loop, find a rule and try it.  if it fired, say so
% and repeat the process.  if not go back and try the next rule.  when
% no rules succeed, stop the inference

go :-
        conflict_set(CS),
	
        write_cs(CS),
        select_rule(CS,r(Inst,ID,LHS,RHS)),
        write('Rule Selected '),write(ID),nl,
        (process(RHS,LHS); true),
        asserta( instantiation(Inst) ),
	write('Rule fired '),write(ID),nl,
	!,go.
go.

write_cs([]).
write_cs([r(I,ID,L,R)|X]) :-
	write(ID),nl,
	writeinst(I),
	write_cs(X).

writeinst([]).
writeinst([H|T]) :-
	tab(5),
	write(H),nl,
	writeinst(T).

get_rule(Inst,ID,LHS,RHS):- (rule ID # LHS ==> RHS, match(LHS,Inst)).

conflict_set(CS) :-
	bagof(r(Inst,ID,LHS,RHS),get_rule(Inst,ID,LHS,RHS), CS).

select_rule(CS,R) :-
	refract(CS,CS1),
	mea_filter(0,CS1,[],CSR),
	lex_sort(CSR,R).

list_cs([]).
list_cs([K-r(_,ID,_,_)|T]) :-
	write(ID-K),nl,
	list_cs(T).

% eliminate those rules which have already been tried

refract([],[]).
refract([r(Inst,_,_,_)|T],TR) :-
	instantiation(Inst), 
	!, refract(T,TR).
refract([H|T],[H|TR]) :-
	refract(T,TR).

% sort the rest of the conflict set according to the lex strategy

lex_sort(L,R) :-
	build_keys(L,LK),
%	keysort(LK,X),
	sort(LK,X),
	reverse(X,[K-R|_]).

% build lists of time stamps for lex sort keys

build_keys([],[]).
build_keys([r(Inst,A,B,C)|T],[Key-r(Inst,A,B,C)|TR]) :-
	build_chlist(Inst,ChL),
	sort(ChL,X),
	reverse(X,Key),
	build_keys(T,TR).

% build a list of just the times of the various matched attributes
% for use in rule selection

build_chlist([],[]).
build_chlist([_/Chron|T],[Chron|TC]) :-
	build_chlist(T,TC).	

% add the test for mea if appropriate that emphasizes the first attribute
% selected.

mea_filter(_,X,_,X) :- not(mea(yes)), !.
mea_filter(_,[],X,X).
mea_filter(Max,[r([A/T|Z],B,C,D)|X],Temp,ML) :-
	T < Max,
	!, mea_filter(Max,X,Temp,ML).
mea_filter(Max,[r([A/T|Z],B,C,D)|X],Temp,ML) :-
	T = Max,
	!, mea_filter(Max,X,[r([A/T|Z],B,C,D)|Temp],ML).
mea_filter(Max,[r([A/T|Z],B,C,D)|X],Temp,ML) :-
	T > Max,
	!, mea_filter(T,X,[r([A/T|Z],B,C,D)],ML).

% recursively go through the LHS list, matching conditions against
% working storage

match([],[]).
match([Prem|Rest],[Prem/Time|InstRest]) :-
	mat(Prem,Time),
	match(Rest,InstRest).

mat(N:Prem,Time) :-
	!,fact(Prem,Time).
mat(Prem,Time) :-
	fact(Prem,Time).
mat(Test,0) :-
	test(Test).
	
fact(Prem,Time) :-
	conv(Prem,Class,Name,ReqList),
	getf(Class,Name,ReqList,Time).

assert_ws( fact(Prem,Time) ) :-
	conv(Prem,Class,Name,UList),
	addf(Class,Name,UList).

update_ws( fact(Prem,Time) ) :-
	conv(Prem,Class,Name,UList),
	uptf(Class,Name,UList).

retract_ws( fact(Prem,Time) ) :-
	conv(Prem,Class,Name,UList),
	delf(Class,Name,UList).

conv(Class-Name with List, Class, Name, List).
conv(Class-Name, Class, Name, []).

% various tests allowed on the LHS

test(not(X)) :-
	fact(X,_),
	!,fail.
test(not(X)) :- !.
test(X#Y) :- X=Y,!.
test(X>Y) :- X>Y,!.
test(X>=Y) :- X>=Y,!.
test(X<Y) :- X<Y,!.
test(X=<Y) :- X=<Y,!.
test(X \= Y) :- not( X=Y), !.
%test(X = Y) :- X=Y, !.
test(X = Y) :- X is Y,!.
test(is_on(X,Y)) :- is_on(X,Y),!.
test(call(X)) :- call(X).

% recursively execute each of the actions in the RHS list

process([],_) :- !.
process([Action|Rest],LHS) :-
	take(Action,LHS),
	!,process(Rest,LHS).
process([Action|Rest],LHS) :-
	error(201,[Action,fails]).

% if its retract, use the reference numbers stored in the Lrefs list,
% otherwise just take the action

take(retract(N),LHS) :-
	(N == all; integer(N)),
	retr(N,LHS),!.
take(A,_) :-take(A),!.

take(retract(X)) :- retract_ws(fact(X,_)), !.
take(assert(X)) :-
	getchron(T),
	assert_ws(fact(X,T)),
	write(adding-X),nl,
	!.
take(update(X)) :-
	getchron(T),
	update_ws(fact(X,T)),
	write(updating-X),nl,
	!.
take(X # Y) :- X=Y,!.
take(X = Y) :- X is Y,!.
take(write(X)) :- write(X),!.
take(write_line(X)) :- write_line(X),!.
take(nl) :- nl,!.
take(read(X)) :- read(X),!.
take(prompt(X,Y)) :- nl,write(X),read(Y),!.
take(cls) :- cls, !.
take(is_on(X,Y)) :- is_on(X,Y), !.
take(list(X)) :- lst(X), !.
take(call(X)) :- call(X).

% logic for retraction

retr(all,LHS) :-retrall(LHS),!.
retr(N,[]) :- error(202,['retract error, no ',N]), !.
retr(N,[N:Prem|_]) :- retract_ws(fact(Prem,_)),!.
retr(N,[_|Rest]) :- !,retr(N,Rest).

retrall([]).
retrall([N:Prem|Rest]) :-
	retract_ws(fact(Prem,_)),
	!, retrall(Rest).
retrall([Prem|Rest]) :-
	retract_ws(fact(Prem,_)),
	!, retrall(Rest).
retrall([_|Rest]) :-		% must have been a test
	retrall(Rest).

% list all of the terms in working storage

lst :-
	fact(X,_),
	write(X),nl,
	fail.
lst.

% lists all of the terms which match the pattern

lst(X) :-
	fact(X,_),
	write(X),nl,
	fail.
lst(_).

% utilities

% member(X,[X|Y]).
% member(X,[Y|Z]) :- member(X,Z).

% reverse(F,R) :- rever(F,[],R).

rever([],R,R).
rever([X|Y],T,R) :- rever(Y,[X|T],R).

% maintain a time counter

setchron(N) :-
	retract( chron(_) ),
	asserta( chron(N) ),!.
setchron(N) :-
	asserta( chron(N) ).

getchron(N) :-
	retract( chron(N) ),
	NN is N + 1,
	asserta( chron(NN) ), !.
	
%
% this section implements a frame based scheme for knowledge representation
%

:- op(600,fy,val).
:- op(600,fy,calc).
:- op(600,fy,def).
:- op(600,fy,add).
:- op(600,fy,del).

% prep_req takes a request of the form Slot-Val, and forms it into the
% more accurate req(Class,Slot,Facet,Value).  If no facet was mentioned
% in the original request, then the facet of "any" is used to indicate
% the system should use everything possible to find a value.

prep_req(Slot-X,req(C,N,Slot,val,X)) :- var(X), !.
prep_req(Slot-X,req(C,N,Slot,Facet,Val)) :-
	nonvar(X),
	X =.. [Facet,Val],
	facet_list(FL),
	is_on(Facet,FL), !.
prep_req(Slot-X,req(C,N,Slot,val,X)).

facet_list([val,def,calc,add,del,edit]).


% retrieve a list of slot values

get_frame(Class, ReqList) :-
	frame(Class, SlotList),
	slot_vals(Class,_,ReqList,SlotList).

getf(Class,Name,ReqList) :-
	getf(Class,Name,ReqList,_).
	
getf(Class,Name,ReqList,TimeStamp) :-
	frinst(Class, Name, SlotList, TimeStamp),
	slot_vals(Class, Name, ReqList, SlotList).

slot_vals(_,_,ReqL,SlotL) :-
	var(ReqL),
	!,
	ReqL = SlotL.
slot_vals(_,_,[],_).
slot_vals(C,N,[Req|Rest],SlotList) :-
	prep_req(Req,req(C,N,S,F,V)),
	find_slot(req(C,N,S,F,V),SlotList), 
	!, slot_vals(C,N,Rest,SlotList).
slot_vals(C,N, Req, SlotList) :-
	not(list(Req)),
	prep_req(Req,req(C,N,S,F,V)),
	find_slot(req(C,N,S,F,V), SlotList).

find_slot(req(C,N,S,F,V), SlotList) :-
	nonvar(V), !,
	find_slot(req(C,N,S,F,Val), SlotList), !,
	(Val = V; list(Val),is_on(V,Val)).
find_slot(req(C,N,S,F,V), SlotList) :-
	is_on(S-FacetList, SlotList), !,
	facet_val(req(C,N,S,F,V),FacetList).
find_slot(req(C,N,S,F,V), SlotList) :-
	is_on(ako-FacetList, SlotList),
	facet_val(req(C,N,ako,val,Ako),FacetList),
	(is_on(X,Ako); X = Ako),
	frame(X, HigherSlots),
	find_slot(req(C,N,S,F,V), HigherSlots), !.
find_slot(Req,_) :-
	error(99,['frame error looking for:',Req]).

facet_val(req(C,N,S,F,V),FacetList) :-
	FV =.. [F,V],
	is_on(FV,FacetList), !.
facet_val(req(C,N,S,val,V),FacetList) :-
	is_on(val ValList,FacetList),
	is_on(V,ValList), !.
facet_val(req(C,N,S,val,V),FacetList) :-
	is_on(calc Pred,FacetList),
	CalcPred =.. [Pred,C,N,S-V],
	call(CalcPred), !.
facet_val(req(C,N,S,val,V),FacetList) :-
	is_on(def V,FacetList), !.

% add a list of slot values

add_frame(Class, UList) :-
	old_slots(Class,SlotList),
	add_slots(Class,_,UList,SlotList,NewList),
	retract(frame(Class,_)),
	asserta(frame(Class,NewList)), !.

addf(Class,Nm,UList) :-
	(var(Nm),genid(Name);Name=Nm),
	add_slots(Class,Name,[ako-Class|UList],SlotList,NewList),
	getchron(TimeStamp),
	asserta( frinst(Class,Name,NewList,TimeStamp) ),
	!.

uptf(Class,Name,UList) :-
	frinst(Class,Name,SlotList,_),
	add_slots(Class,Name,UList,SlotList,NewList),
	retract( frinst(Class,Name,_,_) ),
	getchron(TimeStamp),
	asserta( frinst(Class,Name,NewList,TimeStamp) ),
	!.
uptf(Class,Name,UList) :-
	error(105,[update,failed,Class,Name,UList]).

genid(G) :-
	retract(gid(N)),
	G is N + 1,
	asserta(gid(G)).

old_slots(Class,SlotList) :-
	frame(Class,SlotList), !.
old_slots(Class,[]) :-
	asserta(frame(Class,[])).

add_slots(_,_,[],X,X).
add_slots(C,N,[U|Rest],SlotList,NewList) :-
	prep_req(U,req(C,N,S,F,V)),
	add_slot(req(C,N,S,F,V),SlotList,Z),
	!, add_slots(C,N,Rest,Z,NewList).
add_slots(C,N,X,SlotList,NewList) :-
	prep_req(X,req(C,N,S,F,V)),
	add_slot(req(C,N,S,F,V),SlotList,NewList).

add_slot(req(C,N,S,F,V),SlotList,[S-FL2|SL2]) :-
	delete(S-FacetList,SlotList,SL2),
	add_facet(req(C,N,S,F,V),FacetList,FL2).

add_facet(req(C,N,S,F,V),FacetList,[FNew|FL2]) :-
	FX =.. [F,OldVal],
	delete(FX,FacetList,FL2),
	add_newval(OldVal,V,NewVal),
	!, check_add_demons(req(C,N,S,F,V),FacetList),
	FNew =.. [F,NewVal].

add_newval(X,Val,Val) :- var(X), !.
add_newval(OldList,ValList,NewList) :-
	list(OldList),
	list(ValList),
	append(ValList,OldList,NewList), !.
add_newval([H|T],Val,[Val,H|T]).
add_newval(_,Val,Val).

check_add_demons(req(C,N,S,F,V),FacetList) :-
	get_frame(C,S-add(Add)), !,
	AddFunc =.. [Add,C,N,S-V],
	call(AddFunc).
check_add_demons(_,_).


% delete a list of slot values

del_frame(Class) :-
	retract(frame(Class,_)).
del_frame(Class) :-
	error(203,['No frame',Class,'to delete']).

del_frame(Class, UList) :-
	old_slots(Class,SlotList),
	del_slots(Class,_,UList,SlotList,NewList),
	retract(frame(Class,_)),
	asserta(frame(Class,NewList)).

delf(all) :-
	retract( frinst(_,_,_,_) ),
	fail.
delf(all).

delf(Class,Name) :-
	retract( frinst(Class,Name,_,_) ),
	!.
delf(Class,Name) :-
	error(103,['No instance of ',Class,' for ',Name]).

delf(Class,Name,UList) :-
	old_flots(Class,Name,SlotList),
	del_slots(Class,Name,UList,SlotList,NewList),
	retract( frinst(Class,Name,_,_) ),
	getchron(TimeStamp),
	asserta( frinst(Class,Name,NewList,TimeStamp) ).

del_slots(_,_,[],X,X).
del_slots(C,N,[U|Rest],SlotList,NewList) :-
	prep_req(U,req(C,N,S,F,V)),
	del_slot(req(C,N,S,F,V),SlotList,Z),
	del_slots(C,N,Rest,Z,NewList).
del_slots(C,N,X,SlotList,NewList) :-
	prep_req(X,req(C,N,S,F,V)),
	del_slot(req(C,N,S,F,V),SlotList,NewList).

del_slot(req(C,N,S,F,V),SlotList,[S-FL2|SL2]) :-
	remove(S-FacetList,SlotList,SL2),
	del_facet(req(C,N,S,F,V),FacetList,FL2).
del_slot(Req,_,_) :-
	error(104,['del_slot - unable to remove',Req]).

del_facet(req(C,N,S,F,V),FacetList,FL) :-
	FV =.. [F,V],
	remove(FV,FacetList,FL),
	!, check_del_demons(req(C,N,S,F,V),FacetList).
del_facet(req(C,N,S,F,V),FacetList,[FNew|FL]) :-
	FX =.. [F,OldVal],
	remove(FX,FacetList,FL),
	remove(V,OldVal,NewValList),
	FNew =.. [F,NewValList],	
	!, check_del_demons(req(C,N,S,F,V),FacetList).
del_facet(Req,_,_) :-
	error(105,['del_facet - unable to remove',Req]).

check_del_demons(req(C,N,S,F,V),FacetList) :-
	get_frame(C,S-del(Del)), !,
	DelFunc =.. [Del,C,N,S-V],
	call(DelFunc).
check_del_demons(_,_).

% print a frame

print_frames :-
	frame(Class, SlotList),
	print_frame(Class),
	fail.
print_frames.

print_frame(Class) :-
	frame(Class,SlotList),
	write_line(['Frame:',Class]),
	print_slots(SlotList), nl.

printfs :-
	frame(Class,_),
	printf(Class,_),
	fail.
printfs.

printf(Class,Name) :-
	frinst(Class,Name,SlotList,Time),
	write_line(['Frame:',Class,Name,Time]),
	print_slots(SlotList), nl.

printf(Class) :-
	frinst(Class,Name,SlotList,Time),
	write_line(['Frame:',Class,Name,Time]),
	print_slots(SlotList), nl, fail.
printf(_).

print_slots([]).
print_slots([Slot|Rest]) :-
	write_line(['  Slot:',Slot]),
	print_slots(Rest).

% utilities

delete(X,[],[]).
delete(X,[X|Y],Y) :- !.
delete(X,[Y|Z],[Y|W]) :- delete(X,Z,W).

remove(X,[X|Y],Y) :- !.
remove(X,[Y|Z],[Y|W]) :- remove(X,Z,W).

is_on(X,[X|Y]).
is_on(X,[Y|Z]) :- is_on(X,Z).

error_threshold(100).

error(NE,_) :- error_threshold(N), N > NE, !, fail.
error(NE,E) :-
	nl, write('*** '),write(error-NE),tab(1),
	write_line(E),
	!, fail.

write_line([]) :- nl.
write_line([H|T]) :-
	write(H),tab(1),
	write_line(T).
	
time_test :-
	write('TT> '),
	read(X),
	amzi_timer(T1),
	X,
	amzi_timer(T2),
	nl,nl,
	T is T2 - T1,
	write(time-T).                                                                  

% When no other rules fire, here is the summary

finished :-
	output_data.

% Prolog predicates called by various rules to perform functions better
% handled by Prolog.

% Gather the input data from the user.

gather_data :-
	read_furniture,
	read_walls.

read_furniture :-
	get_frame(furniture,[legal_types-LT]),
	write('Enter name of furniture at the prompt.  It must be one of:'),nl,
	write(LT),nl,
	write('Enter end to stop input.'),nl,
	write('At the length prompt enter y or a new number.'),nl,
	repeat,
	write('>'),read(X),
	process_furn(X), !.

process_furn(end).
process_furn(X) :-
	get_frame(X,[length-DL]),
	write(length-DL),write('>'),
	read(NL),
	get_length(NL,DL,L),
	assert_ws(X - _ with [length-L]),
	fail.

get_length(y,L,L) :- !.
get_length(L,_,L).

read_walls :-
	nl,write('Enter data for the walls.'),nl,
	write('What is the length of the north & south walls? '),
	read(NSL),
	update_ws(wall-north with [length-NSL]),
	update_ws(wall-south with [length-NSL]),
	write('What is the length of the east & west walls? '),
	read(EWL),
	update_ws(wall-east with [length-EWL]),
	update_ws(wall-west with [length-EWL]),
	write('Which wall has the door? '),
	read(DoorWall),
	write('What is its length? '),
	read(DoorLength),
	assert_ws(door-D with [length-DoorLength]),
	update_ws(door-D with [position-wall/DoorWall]),
	write('Which walls have outlets? (a list)'),
	read(PlugWalls),
	process_plugs(PlugWalls).

process_plugs([]) :- !.
process_plugs([H|T]) :-
	update_ws(wall-H with [outlets-1]),
	!, process_plugs(T).
process_plugs(X) :-
	update_ws(wall-X with [outlets-1]).

output_data :-
	write('The final results are:'),nl,
	output_walls,
	output_tables,
	output_recommends,
	output_unplaced.

output_walls :-
	getf(wall,W,[holding-HL]),
	write_line([W,wall,holding|HL]),
	fail.
output_walls.

output_tables :-
	getf(C,N,[holding-HL]),
	not(C = wall),
	write_line([C,N,holding|HL]),
	fail.
output_tables.

output_recommends :-
	getf(recommend,_,[buy-BL]),
	write_line([purchase|BL]),
	fail.
output_recommends.

output_unplaced :-
	write('Unplaced furniture:'),nl,
	getf(T,N,[position-none]),
	write(T-N),nl,
	fail.
output_unplaced.                                                                                                  






% ROOM - a version of ROOM for use with RETE-FOOPS.

frame(furniture, [
	legal_types - [val [couch,chair,coffee_table,end_table,standing_lamp,
		table_lamp,tv,knickknack]],
	position - [def none, add pos_add],
	length - [def 3],
	place_on - [def floor],
	can_hold - [def 0]]).

frame(couch, [
	ako - [val furniture],
	length - [def 6]]).

frame(chair, [
	ako - [val furniture],
	length - [def 3]]).

% A table is different from most furniture in that it can hold things
% on it.

frame(table, [
	ako - [val furniture],
	space - [def 4],
	length - [def 4],
	can_support - [def yes],
	holding - [def []]]).

frame(end_table, [
	ako - [val table],
	length - [def 2]]).

frame(coffee_table, [
	ako - [val table],
	length - [def 4]]).

% electric is used as a super class for anything electrical.  It contains
% the defaults for those attributes unique to electrical things.

frame(electric, [
	needs_outlet - [def yes]]).

frame(lamp, [
	ako - [val [furniture, electric]]]).

frame(standing_lamp, [
	ako - [val lamp]]).

frame(table_lamp, [
	ako - [val lamp],
	place_on - [def table]]).

frame(tv, [
	ako - [val [furniture, electric]],
	place_on - [calc tv_support]]).

frame(knickknack, [
	ako - [val furniture],
	length - [def 1],
	place_on - [def table]]).

frame(wall, [
	length - [def 10],
	outlets - [def 0],
	space - [calc space_calc],
	holding - [def []]]).

frame(door, [
	ako - [val furniture],
	length - [def 4]]).

frame(goal, []).

frame(recommend, []).

% calculate the available space if needed.  The available space is
% computed from the length of the item minus the sum of the lengths of
% the items it is holding.  The held items are in the holding list.
% The items in the list are identified only by their unique names.
% This is used by walls and tables.

space_calc(C,N,space-S) :-
	getf(C,N,[length-L,holding-HList]),
	sum_lengths(HList,0,HLen),
	S is L - HLen.

sum_lengths([],L,L).
sum_lengths([C/N|T],X,L) :-
	getf(C,N,[length-HL]),
	XX is X + HL,
	sum_lengths(T,XX,L).

% When placing the tv, check with the user to see if it goes on the
% floor or a table.

tv_support(tv,N,place_on-table) :-
	nl,
	write('Should the TV go on a table? '),
	read(yes),
	uptf(tv,N,[place_on-table]).
tv_support(tv,N,place_on-floor) :-
	uptf(tv,N,[place_on-floor]).

% Whenever a piece is placed in position, update the holding list of the
% item which holds it (table or wall) and the available space.  If something
% is placed in front of something else, then do nothing.

pos_add(_,_,position-frontof(X)) :-
	uptf(C,N,[holding-[X]]).
pos_add(C,N,position-CP/P) :-
	getf(CP,P,[space-OldS]),
	getf(C,N,[length-L]),
	NewS is OldS - L,
	NewS >= 0,
	uptf(CP,P,[holding-[C/N],space-NewS]).
pos_add(C,N,position-CP/P) :-
	nl,write_line(['Not enough room on',CP,P,for,C,N]),
	!,fail.

% The forward chaining rules of the system.  They make use of call
% to activate some pure Prolog predicates at the end of the knowledge
% base.  In particular, data gathering, and wall space calculations
% are done in Prolog.


% These are the terms which are initially stored in working storage.
% They set a goal used to force firing of certain preliminary rules,
% and various facts about the problem domain used by the actual
% configuration rules.

initial_data([
	wall - north with [opposite-south,right-west,left-east],
	wall - south with [opposite-north,right-east,left-west],
	wall - east with [opposite-west,right-north,left-south],
	wall - west with [opposite-east,right-south,left-north],
	goal - door_first,
	door - d1 with [length - 3],
	couch - c1 with [length - 6],
	chair - ch1 with [length - 3],
	chair - ch2 with [length - 3],
	chair - ch3 with [length - 3],
	chair - ch4 with [length - 3],
	chair - ch5 with [length - 3],
	chair - ch6 with [length - 3],
	chair - ch7 with [length - 3],
	tv - tv1 with [length - 2, place_on - floor] ]).

% first gather data, then try the couch first.

rule 1#
	[goal - gather_data]
	==>
	[call(gather_data),
	 assert( goal - couch_first )].

rule a1#
	[goal - door_first]
	==>
	[update( door - d1 with [position - wall/east]),
	 assert( goal - couch_first )].
	 
% Rules f1-f13 illustrate the strength of rule based programming.
% Each rule captures a rule of thumb used in configuring furniture
% in a living room.  The rules are all independent, transparent,
% and can be easily maintained.  Complexity can be added without
% concern for the flow of control.

% f1, f2 - place the couch first, it should be either opposite the
% door, or to its right, depending on which wall has more space.

rule f1#
	[goal - couch_first,
	 couch - C with [position-none,length-LenC],
	 door - D with [position-wall/W],
	 wall - W with [right-RW]]
	==>
	[update(couch - C with [position-wall/RW])].

rule f2#
	[goal - couch_first,
	 couch - C with [position-none,length-LenC],
	 door - D with [position-wall/W],
	 wall - W with [opposite-OW]]
	==>
	[update(couch - C with [position-wall/OW])].

% f3 - f3a the tv should be opposite the couch.  if it needs a table, an
% end table should be placed under it, if no table is available put
% it on the floor anyway and recommend the purchase of a table.  The rules
% first check to see if the couch has been placed.

rule f3#
	[couch - C with [position-wall/W],
	 wall - W with [opposite-OW],
	 tv - TV with [position-none,place_on-floor]]
	==>
	[update(tv - TV with [position-wall/OW])].

rule f4#
	[couch - C with [position-wall/W],
	 wall - W with [opposite-OW],
	 tv - TV with [position-none,place_on-table],
	 end_table - T with [position-none]]
	==>
	[update(end_table - T with [position-wall/OW]),
	 update(tv - TV with [position-end_table/T])].

rule f4a#
	[tv - TV with [position-none,place_on-table]]
	==>
	[assert(recommend - R with [buy-['table for tv']])].

	 
% f5 - the coffee table should be in front of the couch.

rule f5#
	[coffee_table - CT with [position-none],
	 couch - C]
	==>
	[update(coffee_table - CT with [position-frontof(couch/C)])].

% f6, f7 - chairs should be on adjacent walls from the couch, which ever
% has the most space

rule f6#
	[chair - Ch with [position-none],
	 couch - C with [position-wall/W],
	 wall - W with [right-RW]]
	==>
	[update(chair - Ch with [position-wall/RW])].
	
rule f7#
	[chair - Ch with [position-none],
	 couch - C with [position-wall/W],
	 wall - W with [left-LW]]
	==>
	[update(chair - Ch with [position-wall/LW])].
	

% put end_tables next to the couch first, then on the walls with
% the chairs

rule f9#
	[end_table - ET with [position-none],
	 not(tv) - TV with [position-none,place_on-table],
	 couch - C with [position-wall/W],
	 not(end_table)- ET2 with [position-wall/W]]
	==>
	[update(end_table - ET with [position-wall/W])].

rule f10#
	[end_table - ET with [position-none],
	 not(tv) - TV with [position-none,place_on-table],
	 chair - C with [position-wall/W],
	 not(end_table) - ET2 with [position-wall/W]]
	==>
	[update(end_table - ET with [position-wall/W])].

% put the table lamps on the end tables

rule f11#
	[table_lamp - TL with [position-none],
	 end_table - ET with [position-wall/W]]
	==>
	[update( table_lamp - TL with [position-end_table/ET] )].

% put the knickknacks on anything which will hold them.

rule f11a#
	[knickknack - KK with [position-none],
	 Table - T with [can_support-yes, position-wall/W]]
	==>
	[update( knickknack - KK with [position-Table/T] )].

% get extension cords if needed

rule f12#
	[Thing - X with [needs_outlet-yes, position-wall/W],
	 wall - W with [outlets-0]]
	==>
	[assert(recommend - R with [buy-['extension cord'-W]])].

rule f13#
	[Thing - X with [needs_outlet-yes, position-C/N],
	 C - N with [position-wall/W],
	 wall - W with [outlets-0]]
	==>
	[assert(recommend - R with [buy-['extension cord'-Thing/W]])].


              
