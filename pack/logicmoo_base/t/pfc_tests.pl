%% some simple tests to see if Pfc is working properly
% :-use_module(library(pfc)).
% :-user:ensure_loaded(logicmoo(mpred/logicmoo_props)).

:-dynamic(b/1).

time(Call,Time) :-
  statistics(runtime,_),
  call(Call),
  statistics(runtime,[_,Time]).

test(0) :- 
  pfc_add([(p(X) => q),
       p(1),
       (p(X), ~r(X) => s(X)),
       (t(X), {X>0} => r(X)),
       (t(X), {X<0} => minusr(X)),
       t(-2),
       t(1)]).


test(1) :- \+ exists_file('kinship.pfc'),!.
test(1) :- 
  consult('kinship.pfc'),
  consult('finin.pfc').


test(2) :-
  pfc_add([(a(X),~b(Y)/(Y>X) => biggest(a)),
       (b(X),~a(Y)/(Y>X) => biggest(b)),
        a(5)]).

test(3) :-!.
test(3) :- logOnError(
  pfc_add([(a(X),\+(b(Y))/(Y>X) => biggest(a)),
       (b(X),\+a((Y))/(Y>X) => biggest(b)),
        a(5)])).


test(4) :-
    pfc_add([(foo(X), bar(Y)/{X=:=Y} => foobar(X)),
         (foobar(X), go => found(X)),
	 (found(X), {X>=100} => big(X)),
	 (found(X), {X>=10,X<100} => medium(X)),
	 (found(X), {X<10} => little(X)),
	 foo(1),
	 bar(2),
	 bar(1),
	 foo(100),
	 goAhead,
	 bar(100)
	]).


test(5) :-
    pfc_add([(faz(X), ~baz(Y)/{X=:=Y} => fazbaz(X)),
         (fazbaz(X), go => found(X)),
	 (found(X), {X>=100} => big(X)),
	 (found(X), {X>=10,X<100} => medium(X)),
	 (found(X), {X<10} => little(X)),
	 faz(1),
	 goAhead,
	 baz(2),
	 baz(1)
	]).


test(6) :-
    pfc_add([(d(X), ~f(Y)/{X=:=Y} => justD(X)),
         (justD(X), go => dGo(X)),
	 d(1),
	 go,
	 f(1)
	]).


test(7) :-
    pfc_add([(g(X), h(Y)/{X=:=Y} => justG(X)),
         (justG(X), go => gGo(X)),
	 g(1),
	 go,
	 h(1)
	]).


test(8) :-
    pfc_add([(j(X), k(Y) => bothJK(X,Y)),
         (bothJK(X,Y), go => jkGo(X,Y)),
	 j(1),
	 go,
	 k(2)
	]).


test(9) :-
    pfc_add([(j(X), k(Y) => bothJK(X,Y)),
         (bothJK(X,Y) => jkGo(X,Y)),
	 j(1),
	 k(2)
	]).

test(10) :-
  pfc_add([
	(j(X), k(Y) => bothJK(X,Y)),
	(bothJK(X,Y), go => jkGo(X,Y)),
	j(1),
	go,
	k(2)
       ]).



:- pfc_trace.

run_tests:-forall(between(1,10,X),debugOnError((once(test(X))))).


