:- if(\+ current_module(logicmoo_utils)).
:- module(logicmoo_util_bb_gvar,
    [  % when the predciates are not being moved from file to file the exports will be moved here
      ]).  
:- include(logicmoo_util_header).
:- endif.

end_of_file.

:- use_module(library(rbtrees)).
% :- use_module(library(nb_rbtrees)).

p_e(P,ENV):-functor(P,ENV,_),!. % ,functor(ENV,F,A).
p_e(_P,test123).

pe_get(P,ENV,Q):-p_e(P,ENV), nb_getval(ENV,Q),!.
pe_set(P,ENV,Q):-p_e(P,ENV), nb_setval(ENV,Q),!.

pe_get_key(P,_,Q):-p_e(P,Q).

:- dynamic(in_bb/2).

gvar_update_value(Before,After):-p_e(Before,BB),nb_current(BB,Before),!,nb_setval(BB,After).
gvar_update_value(Before,After):-retract(Before),assert(After),!.
gvar_update_value(Before,After):-env_retract(Before),env_assert(After).

gvar_value(BB,OP,Value):-must(gvar_value0(BB,OP,Value)).
gvar_value0(BB,call,Value):-!,nb_current(BB,Value),!.
gvar_value0(BB,assert,Value):-!,nb_setval(BB,Value).
gvar_value0(BB,asserta,Value):-!,nb_setval(BB,Value),!.
gvar_value0(BB,retract,Value):-nb_getval(BB,Value),!,nb_setval(BB,[]).
gvar_value0(BB,retractall,_):-nb_setval(BB,[]).

gvar_list(BB,call,Value):-!,must(nb_current(BB,List)),!,dmsg(member(Value,List)),!,member(Value,List).
gvar_list(BB,OP,Value):-must(gvar_list0(BB,OP,Value)).

gvar_list0(BB,assert,Value):-!,must(nb_current(BB,List)), (List=[] ->  nb_setval(BB,[Value]); append_el_via_setarg(List,Value)).
gvar_list0(BB,asserta,Value):-!,must(nb_current(BB,List)),nb_setval(BB,[Value|List]).
gvar_list0(BB,retract,Value):-!,must(nb_current(BB,List)), ( List=[Value|Rest]-> nb_setval(BB,Rest); remove_el_via_setarg(List,Value) ).
gvar_list0(BB,retractall,_F/_A):- !,nb_setval(BB,[]).
gvar_list0(BB,retractall,Value):- args_all_vars(Value)-> nb_setval(BB,[]) ;  
  ((   must(nb_current(BB,List)) , gvar_remove_all_list_matches(BB,List,Value) )).


args_all_vars(Value):- not((arg(_,Value,Nv),nonvar(Nv))).

gvar_remove_all_list_matches(BB,List,Value) :-
  ( List ==[] -> true ; 
    ((List \= [Value|Rest] ->  gvar_remove_all_list_matches(BB, Rest,Value) ;
       ((nb_setval(BB,Rest),gvar_remove_all_list_matches(BB,Rest,Value)))))).

remove_el_via_setarg(List,Value):- [_|T] = List, [_,Was|_] = List,(Was=Value -> nb_setarg(2,List,T) ;  remove_el_via_setarg(Was|T,Value)).
append_el_via_setarg(List,Value):- List = [_|T], (T==[] -> setarg(2,List,[Value]) ; append_el_via_setarg(T,Value)).



bnb_current(BB,LIST):- trace_or_throw(bnb_current(BB,LIST)),!,fail.
bnb_current(BB,LIST):-nb_current(BB,LIST),!.
bnb_current(BB,LIST):-nb_setval(BB,[]),nb_current(BB,LIST),!.

bb_lookup(BB,P):-bnb_current(BB,LIST),!,member(P,LIST).
bb_lookup(BB,P):-in_bb(BB,P).

bb_add(BB,P):-bnb_current(BB,LIST),!,nb_setval(BB,[P|LIST]).
bb_add(BB,P):-asserta(in_bb(BB,P)).

bb_rem(BB,P):-bnb_current(BB,LIST),!,remove_el(LIST,P,NLIST),nb_setval(BB,NLIST).
bb_rem(BB,P):-retract(in_bb(BB,P)).

bb_op(ENV,call,P):-pe_get_key(P,ENV,BB),bb_lookup(BB,P).
bb_op(ENV,assert,P):-pe_get_key(P,ENV,BB),bb_add(BB,P).
bb_op(ENV,asserta,P):-pe_get_key(P,ENV,BB),bb_add(BB,P).
bb_op(ENV,retract,P):-pe_get_key(P,ENV,BB),bb_rem(BB,P).
bb_op(ENV,retractall,P):-pe_get_key(P,ENV,BB),forall(bb_lookup(BB,P),bb_rem(BB,P)).

bb_op_rb(ENV,call,P):-pe_get(P,ENV,BB),rb_lookup(P,P,BB).
bb_op_rb(ENV,assert,P):-pe_get(P,ENV,BB),nb_rb_insert(P,P,BB).
bb_op_rb(ENV,asserta,P):-pe_get(P,ENV,BB),nb_rb_insert(P,P,BB).
bb_op_rb(ENV,retract,P):-pe_get(P,ENV,BB),nb_rb_get_node(P,P,BB).
bb_op_rb(ENV,retractall,P):-rb_new(BB),pe_set(P,ENV,BB).

bb_op_qu(ENV,call,P):-pe_get(P,ENV,Q),inside_queue(Q,P).
bb_op_qu(ENV,assert,P):-pe_get(P,ENV,Q),push_slow_queue(Q,P).
bb_op_qu(ENV,asserta,P):-pe_get(P,ENV,Q),push_fast_queue(Q,P).
bb_op_qu(ENV,retract,P):-pe_get(P,ENV,Q),pop_queue(Q,P).
bb_op_qu(ENV,retractall,P):-make_queue(Q),pe_set(P,ENV,Q).

% FIFO queue

make_queue(Q) :- nb_setval(Q, fast_slow(QU-QU, L-L)).

push_fast_queue(Q,E) :-
        b_getval(Q, fast_slow(H-[E|T], L)),
        b_setval(Q, fast_slow(H-T, L)).

push_slow_queue(Q,E) :-
        b_getval(Q, fast_slow(L, H-[E|T])),
        b_setval(Q, fast_slow(L, H-T)).

pop_queue(Q,E) :-
        b_getval(Q, fast_slow(H-T, I-U)),
        (   nonvar(H) ->
            H = [E|NH],
            b_setval(Q, fast_slow(NH-T, I-U))
        ;   nonvar(I) ->
            I = [E|NI],
            b_setval(Q, fast_slow(H-T, NI-U))
        ;   false
        ).

inside_queue(Q,E) :- %  trace_or_throw(inside_queue(Q,E)),!,fail.
        b_getval(Q, fast_slow(H-_T, I-_U)),(   nonvar(H) , member(E,H)  ;   nonvar(I) , member(E,I)).




























































end_of_file.





























































































































%:-expects_dialect(ifprolog).

% :-ifprolog_term_expansion(assign_alias(debug,user_error),Call),Call.

clause_asserted(C):- as_clause(C,H,B),!,clause_asserted(H,B).
clause_asserted(H,B):- predicate_property(H,number_of_clauses(N)),N>0,copy_term(H:B,HH:BB),!, clause(HH, BB, Ref),clause(Head, Body, Ref),H=@=Head,Body=@=B,!.
as_clause( ((H :- B)),H,B):-!.
as_clause( H,  H,  true).

assert_if_new(C):-notrace(clause_asserted(C)->true;assert(C)).
:- dynamic((time_taken/1,sum/1,soln_size/1)).
:- assert_if_new(time_taken(0)).
:- assert_if_new(sum(0)).
:- assert_if_new(soln_size(0)).



failOnError(Call):-catch(Call,_,fail).

fresh_line:-current_output(Strm),fresh_line(Strm),!.
fresh_line(Strm):-failOnError((stream_property(Strm,position('$stream_position'(_,_,POS,_))),(POS>0 ->nl(Strm);true))),!.
fresh_line(Strm):-failOnError(nl(Strm)),!.
fresh_line(_).

% ===================================================================
% Substitution based on ==
% ===================================================================

% Usage: subst(+Fml,+X,+Sk,?FmlSk)


nd_subst(  Var, VarS,SUB,SUB ) :- Var==VarS,!.
nd_subst(  Var, _,_,Var ) :- var(Var),!.
nd_subst(  P, X,Sk, P1 ) :- functor(P,_,N),nd_subst1( X, Sk, P, N, P1 ).

nd_subst1( _,  _, P, 0, P  ).
nd_subst1( X, Sk, P, N, P1 ) :- N > 0,
            P =.. [F|Args], 
            nd_subst2( X, Sk, Args, ArgS ),
            nd_subst2( X, Sk, [F], [FS] ),  
            P1 =.. [FS|ArgS].

nd_subst2( _,  _, [], [] ).
nd_subst2( X, Sk, [A|As], [Sk|AS] ) :- X == A, !, nd_subst2( X, Sk, As, AS).
nd_subst2( X, Sk, [A|As], [A|AS]  ) :- var(A), !, nd_subst2( X, Sk, As, AS).
nd_subst2( X, Sk, [A|As], [Ap|AS] ) :- nd_subst( A,X,Sk,Ap ),nd_subst2( X, Sk, As, AS).
nd_subst2( _X, _Sk, L, L ).

:- meta_predicate(with_assertions(+,0)).
with_assertions( [],Call):- !,Call.
with_assertions( [With|MORE],Call):- !,with_assertions(With,with_assertions(MORE,Call)).
with_assertions( (With,MORE),Call):- !,with_assertions(With,with_assertions(MORE,Call)).
with_assertions( (With;MORE),Call):- !,with_assertions(With,Call);with_assertions(MORE,Call).
with_assertions( -TL:With,Call):- !,with_no_assertions(TL:With,Call).
with_assertions( +TL:With,Call):- !,with_assertions(TL:With,Call).
with_assertions( not(With),Call):- !,with_no_assertions(With,Call).
with_assertions( -With,Call):- !,with_no_assertions(With,Call).
with_assertions( +With,Call):- !,with_assertions(With,Call).

% with_assertions(THead,Call):- functor(THead,F,_),b_setval(F,THead).
with_assertions(THead,Call):- 
 must(to_thread_head(THead,M,_Head,H)),
   copy_term(H,  WithA), !,
   ( M:WithA -> Call ; setup_call_cleanup(M:asserta(WithA),Call,must(M:retract(WithA)))).

with_assertions(THead,Call):- 
 must(to_thread_head(THead,M,_Head,H)),
   copy_term(H,  WithA), !,
   with_assertions(M,WithA,Call).
   
:- meta_predicate(with_assertions(+,+,0)).
with_assertions(M,WithA,Call):- M:WithA,!,Call.
with_assertions(M,WithA,Call):-
   setup_call_cleanup(M:asserta(WithA),Call,must(M:retract(WithA))).

:- meta_predicate(with_no_assertions(+,0)).
with_no_assertions(THead,Call):-
 must(to_thread_head((THead:-!,fail),M,_Head,H)),
   copy_term(H,  WithA), !, setup_call_cleanup(M:asserta(WithA),Call,must(M:retract(WithA))).


to_thread_head((H:-B),TL,(HO:-B),(HH:-B)):-!,to_thread_head(H,TL,HO,HH),!.
to_thread_head(thglobal:Head,thglobal,thglobal:Head,Head):- slow_sanity((predicate_property(thglobal:Head,(dynamic)),not(predicate_property(thglobal:Head,(thread_local))))).
to_thread_head(TL:Head,TL,TL:Head,Head):-!, slow_sanity(( predicate_property(TL:Head,(dynamic)),must(predicate_property(TL:Head,(thread_local))))).
to_thread_head(Head,thlocal,thlocal:Head,Head):-!, slow_sanity(( predicate_property(thlocal:Head,(dynamic)),must(predicate_property(thlocal:Head,(thread_local))))).
to_thread_head(Head,tlbugger,tlbugger:Head,Head):- slow_sanity(( predicate_property(tlbugger:Head,(dynamic)),predicate_property(tlbugger:Head,(thread_local)))).

slow_sanity(X):-must(X).


%se/cond == se. state == ss. trans == sc. dif == ne.  operator == operator
term_expansion_hyhtn(In,Out):-nonvar(In),term_expansion_alias(In,Out).


/*
:- use_module(library(pce)).
:- use_module(library(gui_tracer)).
*/

:- thread_local tlbugger:inside_loop_check/1.
:- module_transparent(tlbugger:inside_loop_check/1).

:- thread_local tlbugger:inside_loop_check_local/2.
:- module_transparent(tlbugger:inside_loop_check_local/2).


make_key(CC,KeyA):- notrace(ground(CC)->KeyA=CC ;(copy_term(CC,Key,_),numbervars(Key,0,_))),!,KeyA=Key. % ,term_to_atom(Key,KeyA).
loop_check(B):- loop_check(B,fail).
:- trace.
loop_check(B, TODO):- make_key(B,BC),!, loop_check_term(B,BC,TODO).

loop_check_term(B,BC,TODO):-  ( \+(tlbugger:inside_loop_check(BC)) 
   -> setup_call_cleanup(asserta(tlbugger:inside_loop_check(BC)),B, retract((tlbugger:inside_loop_check(BC)))) ;call(TODO) ).


must(E):-E *-> true ; (trace,E).

functor_h(P,F,A):-var(P),!,(number(A)->functor(P,F,A);((mpred_arity(F,A);throw(var_functor_h(P,F,A))))).
functor_h(F/A,F,A):-number(A),!,( atom(F) ->  true ; mpred_arity(F,A)).
functor_h(':'(_,P),F,A):-nonvar(P),!,functor_h(P,F,A).
functor_h(':-'(P),F,A):-!,functor_h(P,F,A).
functor_h(':-'(P,_),F,A):-!,functor_h(P,F,A).
functor_h(kb(P),F,A):- atom(P),!, ( P=F -> mpred_arity(F,A) ; env_mpred(P,F,A)).
functor_h(P,F,A):-functor(P,F,A).

decl_mpred(ENV,Var):- (var(ENV);var(Var)),!, trace,throw(var_env_learn_pred(ENV,Var)).
decl_mpred(ENV,(A,B)):-!, decl_mpred(ENV,A), decl_mpred(ENV,B).
decl_mpred(ENV,[A|B]):-!, decl_mpred(ENV,A), decl_mpred(ENV,B).
decl_mpred([],_):- !.
decl_mpred(_,[]):- !.
decl_mpred([H|T],P):- !, decl_mpred(H,P),decl_mpred(T,P).
decl_mpred((H,T),P):- !, decl_mpred(H,P),decl_mpred(T,P).
decl_mpred(call(ENV),P):- functor_h(P,F,A),call(ENV,F/A),!.
decl_mpred(ENV,P):- functor_h(P,F,A),!,decl_mpred_fa(ENV,F,A).

decl_mpred_fa(ENV,F,A):- env_mpred(ENV,F,A),!.
decl_mpred_fa(ENV,F,A):- assert_if_new(mpred_arity(F,A)),assert_if_new(env_kb(ENV)),assert(env_mpred(ENV,F,A)), doall(decl_mpred_fa_hooks(ENV,F,A)).



 
count(1_000_000).
 
my_asserta(N) :- asserta(asserted_a(N)).
my_assertz(N) :- assertz(asserted_z(N)).
my_recorda(N) :- recorda(recorded_a, N).
my_recordz(N) :- recordz(recorded_z, N).
my_flag(N) :- flag(some_flag, _, N).
 
my_setval(N) :-
    b_getval(global_variable, Old),
    b_setval(global_variable, [N|Old]).
 
bench :-
    count(Count),
    ns_op(many(Count, my_asserta)),
    ns_op(many(Count, my_assertz)),
    ns_op(many(Count, my_recorda)),
    ns_op(many(Count, my_recordz)),
    ns_op(many(Count, my_flag)),
    ns_op(many(Count, my_setval)),
    clear.
 
ns_op(Goal) :-
    get_time(Start),
    call(Goal),
    get_time(Done),
    count(Count),
    Ns is (Done - Start) / Count * 1_000_000_000,
    'format'("~w: ~1f ns/op~n", [Goal, Ns]).
 
many(N, Goal) :-
    ( N > 0 ->
        call(Goal, N),
        succ(N0, N),
        many(N0, Goal)
    ; true
    ).
 
clear :-
    ns_op(retractall(asserted_a(_))),
    ns_op(retractall(asserted_z(_))),
    ns_op(eraseall(recorded_a)),
    ns_op(eraseall(recorded_z)),
    ns_op(b_setval(global_variable, [])).
 
eraseall(Key) :-
    foreach( recorded(Key,A,Ref), erase_Safe(recorded(Key,A,Ref),Ref) ).


dmsg(Term):-dmsg(debug,Term).
dmsg(Color,Term):-   tell(user),fresh_line, sformat(S,Term,[],[]),print_message_lines(user_output,kind(Color),[S-[]]),fresh_line,told,!.
/*
dmsg(Color,Term):- current_prolog_flag(tty_control, true),!,  tell(user),fresh_line,to_petty_color(Color,Type),
   call_cleanup(((sformat(S,Term,[],[]),print_message(Type,if_tty([S-[]])))),told).
*/

sformat(O,T,_Vs,_Opts):- (true;functor(T,':-',_)),with_output_to(chars(Codes),portray_clause(':-'(T))),
  append([_,_,_],PrintCodes,Codes),'sformat'(O,'~s',[PrintCodes]),!.
sformat(O,T,Vs,Opts):- with_output_to(chars(Codes),(current_output(CO),pp_termclause(CO,':-'(T),Vs,Opts))),
  append([_,_,_],PrintCodes,Codes),'sformat'(O,'~s',[PrintCodes]),!.

pp_termclause(O,T,Vs,Options):- prolog_listing:do_portray_clause(O,T,[variable_names(Vs),numbervars(true),character_escapes(true),quoted(true)|Options]),!.

to_petty_clause(V,('VARIABLE':-V)):-var(V),!.
to_petty_clause((H:-B),(H:-B)):-!.
to_petty_clause((':-'(B)),(':-'(B))):-!.
to_petty_clause(((B)),(':-'(B))):-!.
to_petty_color(X,X).

nop(_P).
doall(G):-ignore((G,fail)).
one_must(Call,Else):- trye(Call)*->true;Else.

:- dynamic((env_mpred/3, mpred_arity/2, env_kb/1)).



