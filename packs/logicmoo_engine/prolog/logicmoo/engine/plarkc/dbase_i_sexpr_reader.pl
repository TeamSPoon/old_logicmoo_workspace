

:- user:use_module(dbase_i_cyc_api).

end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    Lisprolog -- Interpreter for a simple Lisp. Written in Prolog.
    Written Nov. 26th, 2006 by Markus Triska (triska@gmx.at).
    Public domain code.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%:-module(logicmoo_i_sexp_reader,[codelist_to_forms/2,parse_to_source/2,get_source/1,lisp_read/2,l_open_input/2]).
:- style_check(-singleton).
:- style_check(-discontiguous).
% :- style_check(-atom).
:- set_prolog_flag(double_quotes, codes). 
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Parsing
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
:-dynamic(user:mpred_prop/2).
:-multifile(user:mpred_prop/2).
:- use_module('../../src_lib/logicmoo_util/logicmoo_util_all.pl').

parse_sexpr(String, Expr) :- string(String),!,string_codes(String,Codes),phrase(sexpr(Expr), Codes).
parse_sexpr(String, Expr) :- phrase(sexpr(Expr), String).

:-export(sexpr//1).

% Use DCG for parser.


sexpr(L)                      --> "(", !, swhite, sexpr_list(L), swhite.
sexpr(vec(V))                 --> "#(", !, sexpr_vector(V), swhite.
sexpr(s(t))                 --> "#t", !, swhite.
sexpr(s(f))                 --> "#f", !, swhite.
sexpr(s(E))              --> "#$", !, swhite, sexpr(E).
sexpr(chr(N))                 --> "#\\", [C], !, {N is C}, swhite.
sexpr(str(S))                 --> """", !, sexpr_string(S), swhite.
sexpr([s(quote),E])              --> "'", !, swhite, sexpr(E).
sexpr([s(backquote),E])         --> "`", !, swhite, sexpr(E).
sexpr(['unquote-splicing',E]) --> ",@", !, swhite, sexpr(E).
sexpr(comma(E))            --> ",", !, swhite, sexpr(E).
sexpr(s(E))                      --> sym_or_num(E),!, swhite.

sblank --> [C], {C =< 32}, swhite.
sblank --> ";", line_comment, swhite.

swhite --> sblank.
swhite --> [].

line_comment --> [C], {eoln(C)}, !.
line_comment --> [C], line_comment.

sexprs([H|T]) --> sexpr(H), !, sexprs(T).
sexprs([]) --> [].


:-export(sexpr_list//1).

sexpr_list([]) --> ")", !.
sexpr_list(_) --> ".", [C], {\+ sym_char(C)}, !, fail.
sexpr_list([Car|Cdr]) --> sexpr(Car), !, sexpr_rest(Cdr).

sexpr_rest([]) --> ")", !.
sexpr_rest(E) --> ".", [C], {\+ sym_char(C)}, !, sexpr(E,C), !, ")".
sexpr_rest([Car|Cdr]) --> sexpr(Car), !, sexpr_rest(Cdr).

sexpr_vector([]) --> ")", !.
sexpr_vector([First|Rest]) --> sexpr(First), !, sexpr_vector(Rest).

sexpr_string([]) --> """", !.
sexpr_string([C|S]) --> chr(C), sexpr_string(S).

chr(92) --> "\\", !.
chr(34) --> "\"", !.
chr(N)  --> [C], {C >= 32, N is C}.

sym_or_num(s(E)) --> [C], {sym_char(C)}, sym_string(S), {string_to_atom([C|S],E)}.
sym_or_num(n(E)) --> snumber(E).

sym_string([H|T]) --> [H], {sym_char(H)}, sym_string(T).
sym_string([]) --> [].

snumber(N) --> unsigned_number(N).
snumber(N) --> "-", unsigned_number(M), {N is -M}.
snumber(N) --> "+", unsigned_number(N).

unsigned_number(N) --> cdigit(X), unsigned_number(X,N).
unsigned_number(N,M) --> cdigit(X), {Y is N*10+X}, unsigned_number(Y,M).
unsigned_number(N,N) --> [].

cdigit(N) --> [C], {C >= 48, C =<57, N is C-48}.

% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

sexpr(E,C,X,Z) :- swhite([C|X],Y), sexpr(E,Y,Z).

% dquote semicolon parens  hash qquote comma, backquote
sym_char(C) :- C > 32, not(member(C,[34,59,40,41,35,39,44,96])).  


l_open_input(InS,InS):-is_stream(InS),!.
l_open_input((InS),In):-string(InS),!,l_open_input(string(InS),In).
l_open_input(string(InS),In):-text_to_string(InS,Str),string_codes(Str,Codes),open_chars_stream(Codes,In),!.
l_open_input(Filename,In) :- catch(see(Filename),_,fail),current_input(In),!.
l_open_input(InS,In):-text_to_string(InS,Str),string_codes(Str,Codes),open_chars_stream(Codes,In),!.


to_untyped(s(S),O):-!,to_untyped(S,O).
to_untyped(comma(S),comma(O)):-!,to_untyped(S,O).
to_untyped(vect(S),vect(O)):-!,to_untyped(S,O).
to_untyped(n(S),O):-!,to_untyped(S,O).
to_untyped([[]],[]):-!.
to_untyped([s(quote),Rest],Out):-!,maplist(to_untyped,Rest,Out).
to_untyped([s(backquote),Rest],Out):-!,to_untyped(Rest,Out).
to_untyped([s(S)|Rest],Out):-is_list(Rest),!,maplist(to_untyped,[S|Rest],Mid),Out=..Mid,!.
to_untyped([H|T],[HH|TT]):-!,to_untyped(H,HH),to_untyped(T,TT).
to_untyped(str(S),O):-atom_string(S,O),!.
to_untyped(char(S),char(S)):-!.
to_untyped(S,O):- string(S),atom_string(A,S),!,to_untyped(A,O).
to_untyped(S,O):- catch((atom_codes(S,C), number_codes(O,C),!),_,fail),!.
to_untyped(H,O):- compound(H),!,H=..HL,maplist(to_untyped,HL,OL),O=..OL.
to_untyped(S,'$VAR'(VarName)):-atom_concat('??',VarNameI,S),atom_concat('_',VarNameI,VarName).
to_untyped(S,'$VAR'(VarNameU)):-atom_concat('?',VarName,S),atom_upper(VarName,VarNameU).
to_untyped(S,S):-!.

atom_upper(A,U):-string_upper(A,S),atom_string(U,S).

lisp_read(Forms):-lisp_read(current_input,Forms),!.
lisp_read(I,Forms):-stream_source_typed(I,Type),!,must(to_untyped(Type,Forms)).
stream_source_typed(I,Expr):-   l_open_input(I,In),
  see(In),
   read_line_to_codes(current_input,AsciiCodes),(parse_sexpr(AsciiCodes,Expr);read_term_from_codes(AsciiCodes,Expr,[])),!,seen.


lowcase([],[]).
lowcase([C1|T1],[C2|T2]) :- lowercase(C1,C2), lowcase(T1,T2).

lowercase(C1,C2) :- C1 >= 65, C1 =< 90, !, C2 is C1+32.
lowercase(C,C).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Interpretation
   --------------

   Declaratively, execution of a Lisp form is a relation between the
   (function and variable) binding environment before its execution
   and the environment after its execution. A Lisp program is a
   sequence of Lisp forms, and its result is the sequence of their
   results. The environment is represented as a pair of association
   lists Fs-Vs, associating function names with argument names and
   bodies, and variables with values. DCGs are used to implicitly
   thread the environment state through.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


codelist_to_forms(AsciiCodesList,FormsOut):-
    parse_sexpr(AsciiCodesList, Forms0),    
    compile_all(Forms0, FormsOut),!.

run(Program, Values) :-
    parse_sexpr(Program, Forms0),    
    empty_assoc(E),
    compile_all(Forms0, Forms),
    writeq(seeingFormas(Forms)),nl,
    phrase(eval_all(Forms, Values0), [E-E], _),
    maplist(unfunc, Values0, Values).

unfunc(s(S), S).
unfunc(t, t).
unfunc(n(N), N).
unfunc([], []).
unfunc([Q0|Qs0], [Q|Qs]) :- unfunc(Q0, Q), unfunc(Qs0, Qs).

fold([], _, V, n(V)).
fold([n(F)|Fs], Op, V0, V) :- E =.. [Op,V0,F], V1 is E, fold(Fs, Op, V1, V).

compile_all(Fs0, Fs) :- maplist(compile, Fs0, Fs).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    compile/2 marks (with 'user/1') calls of user-defined functions.
    This eliminates an otherwise defaulty representation of function
    calls and thus allows for first argument indexing in eval//3.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

compile(F0, F) :-
    (   F0 = n(_)   -> F = F0
    ;   F0 = s(t)   -> F = t
    ;   F0 = s(nil) -> F = []
    ;   F0 = s(_)   -> F = F0
    ;   F0 = [] -> F = []
    ;   F0 = [s(quote),Arg] -> F = [quote,Arg]
    ;   F0 = [s(setq),s(Var),Val0] -> compile(Val0, Val), F = [setq,Var,Val]
    ;   F0 = [s(Op)|Args0],
        memberchk(Op, [+,-,*,equal,if,>,<,=,progn,eval,list,car,cons,
                       cdr,while,not]) ->
        compile_all(Args0, Args),
        F = [Op|Args]
    ;   F0 = [s(defun),s(Name),Args0|Body0] ->
        compile_all(Body0, Body),
        maplist(arg(1), Args0, Args),
        F = [defun,Name,Args|Body]
    ;   F0 = [s(Op)|Args0] -> compile_all(Args0, Args), F = [user(Op)|Args]
    ).

eval_all([], [])         --> [].
eval_all([A|As], [B|Bs]) --> eval(A, B), eval_all(As, Bs).

eval(n(N), n(N))       --> [].
eval(t, t)             --> [].
eval([], [])           --> [].
eval(s(A), V), [Fs-Vs] --> [Fs-Vs], { get_assoc(A, Vs, V) }.
eval([L|Ls], Value)    --> eval(L, Ls, Value).

eval(quote, [Q], Q) --> [].
eval(+, As0, V)     --> eval_all(As0, As), { fold(As, +, 0, V) }.
eval(-, As0, V)     --> eval_all(As0, [n(V0)|Vs0]), { fold(Vs0, -, V0, V) }.
eval(*, As0, V)     --> eval_all(As0, Vs), { fold(Vs, *, 1, V) }.
eval(car, [A], C)   --> eval(A, V), { V == [] -> C = [] ; V = [C|_] }.
eval(cdr, [A], C)   --> eval(A, V), { V == [] -> C = [] ; V = [_|C] }.
eval(list, Ls0, Ls) --> eval_all(Ls0, Ls).
eval(not, [A], V)   --> eval(A, V0), goal_truth(V0=[], V).
eval(>, [A,B], V)   --> eval(A, n(V1)), eval(B, n(V2)), goal_truth(V1>V2, V).
eval(<, [A,B], V)   --> eval(>, [B,A], V).
eval(=, [A,B], V)   --> eval(A, n(V1)), eval(B, n(V2)), goal_truth(V1=:=V2, V).
eval(progn, Ps, V)  --> eval_all(Ps, Vs), { last(Vs, V) }.
eval(eval, [A], V)  --> eval(A, F0), { compile(F0, F1) }, eval(F1, V).
eval(equal, [A,B], V) --> eval(A, V1), eval(B, V2), goal_truth(V1=V2, V).
eval(cons, [A,B], [V0|V1])  --> eval(A, V0), eval(B, V1).
eval(while, [Cond|Bs], [])  -->
    (   eval(Cond, []) -> []
    ;   eval_all(Bs, _),
        eval(while, [Cond|Bs], _)
    ).
eval(defun, [F,As|Body], s(F)), [Fs-Vs0] -->
    [Fs0-Vs0],
    { put_assoc(F, Fs0, As-Body, Fs) }.
eval(user(F), As0, V), [Fs-Vs] -->
    eval_all(As0, As1),
    [Fs-Vs],
    { empty_assoc(E),
      get_assoc(F, Fs, As-Body),
      bind_arguments(As, As1, E, Bindings),
      phrase(eval_all(Body, Results), [Fs-Bindings], _),
      last(Results, V) }.
eval(setq, [Var,V0], V), [Fs0-Vs] -->
    eval(V0, V),
    [Fs0-Vs0],
    { put_assoc(Var, Vs0, V, Vs) }.
eval(if, [Cond,Then|Else], Value) -->
    (   eval(Cond, []) -> eval_all(Else, Values), { last(Values, Value) }
    ;   eval(Then, Value)
    ).

:- meta_predicate user:goal_truth(0,*,*,*).
goal_truth(Goal, T) --> { Goal -> T = t ; T = [] }.

bind_arguments([], [], Bs, Bs).
bind_arguments([A|As], [V|Vs], Bs0, Bs) :-
    put_assoc(A, Bs0, V, Bs1),
    bind_arguments(As, Vs, Bs1, Bs).

run(S):-'format'('~n~s~n',[S]),run(S,V),writeq(V).

if_script_file_time(X):-if_startup_script(time(X)).

% Append:
    :- if_script_file_time(run("
        (defun append (x y)
          (if x
              (cons (car x) (append (cdr x) y))
            y))

        (append '(a b) '(3 4 5))"
)).

    %@ V = [append, [a, b, 3, 4, 5]].
    

% Fibonacci, naive version:
    :- if_script_file_time(run("
        (defun fib (n)
          (if (= 0 n)
              0
            (if (= 1 n)
                1
              (+ (fib (- n 1)) (fib (- n 2))))))
        (fib 24)"
)).

    %@ % 14,255,802 inferences, 3.71 CPU in 3.87 seconds (96% CPU, 3842534 Lips)
    %@ V = [fib, 46368].
    

% Fibonacci, accumulating version:
    :- if_script_file_time(run("
        (defun fib (n)
          (if (= 0 n) 0 (fib1 0 1 1 n)))

        (defun fib1 (f1 f2 i to)
          (if (= i to)
              f2
            (fib1 f2 (+ f1 f2) (+ i 1) to)))

        (fib 250)"
)).

    %@ % 39,882 inferences, 0.010 CPU in 0.013 seconds (80% CPU, 3988200 Lips)
    %@ V = [fib, fib1, 7896325826131730509282738943634332893686268675876375].
    

% Fibonacci, iterative version:
    :- if_script_file_time(run("
        (defun fib (n)
          (setq f (cons 0 1))
          (setq i 0)
          (while (< i n)
            (setq f (cons (cdr f) (+ (car f) (cdr f))))
            (setq i (+ i 1)))
          (car f))

        (fib 350)"
)).

    %@ % 34,233 inferences, 0.010 CPU in 0.010 seconds (98% CPU, 3423300 Lips)
    %@ V = [fib, 6254449428820551641549772190170184190608177514674331726439961915653414425].
    

% Higher-order programming and eval:
    :- if_startup_script(run("
        (defun map (f xs)
          (if xs
              (cons (eval (list f (car xs))) (map f (cdr xs)))
            ()))

        (defun plus1 (x) (+ 1 x))

        (map 'plus1 '(1 2 3))
        "
        )).

    %@ V = [map, plus1, [2, 3, 4]].


