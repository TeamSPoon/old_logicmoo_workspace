:- module(aterm, 
          [   mi/1, mi/3,
            aterm/2                    % Var, ?ATerm ?NVar
          ]).

:- prolog_load_context(directory,H),absolute_file_name('../..',A,[file_type(directory),relative_to(H)]),asserta(user:library_directory(A)).
:- prolog_load_context(directory,H),absolute_file_name('../../../..',A,[file_type(directory),relative_to(H)]),asserta(user:file_search_path(pack,A)).
:- attach_packs.
:- user: ensure_loaded(library(logicmoo/logicmoo_utils)).
:- set_prolog_flag(answer_write_options,[quoted(true), portray(true), max_depth(1000), attributes(portray)]).
:- set_prolog_flag(debugger_write_options,[quoted(true), portray(true), max_depth(1000), attributes(portray)]).

% i should ask on list .. but i like giving this channel a chance first ,,,  has anyone implemented prolog variables in a metainterp using b_setarg ?

:- use_module(library(ordsets)).
:- visible(+unify).
:- style_check(-singleton).

lvar(E,N):-var(E),must((format(codes([_,_|C]),'~q',[E]),number_codes(N,C))),!.
lvar(E,E).

make_aterm(C,C):-get_attr(C,aterm,_),!.
make_aterm(G,GG):-compound(G),make_aterm((','),1,G,GG),!.
make_aterm(G,GG):-make_aterm(name,W,G,GG),!,ignore(var_property(GG,name(W))),ignore(var_property(G,name(W))).

make_aterm(Outer,N,C,C):- var(C),!, (get_attr(C,aterm,_) -> true; 
   (dict_create(Dict,_,[var:C,val:[],nat:[],pred:[]]), put_attr(C, aterm, Dict))),!.
make_aterm(Outer,N,a(C),a(C)):- !. % put_aterm(C,[pred:argiza(Outer,N)],CC).
make_aterm(Outer,N,C,CC):- compound(C),functor(C,F,A),
 dont_touch(C,F,A) -> CC=C;
  ( C=..[F|Args] , !, 
  (is_a_function(Outer,F) -> make_atermv([func:C],Outer,N,C,CC) ; 
  ((make_aterm_l(F,1,Args,OArgs),
       (Args=@=OArgs -> CC=C ; CC=..[F|OArgs]))))).
make_aterm(Outer,N,C,CC):- make_atermv([],Outer,N,C,CC).

make_aterm_l(F,N,[A],[OA]):- !, make_aterm(F,N,A,OA).
make_aterm_l(F,N,[A|Args],[OA|OArgs]):- make_aterm(F,N,A,OA),N2 is N+1,make_aterm_l(F,N2,Args,OArgs).

label(V):-label(V,V).
label(V,Val):-get_aterm(V,Dict),(get_dict(val,Dict,Val);get_dict(nat,Dict,Val)),nonvar(Val).

is_mp((,)).
is_mp((;)).

dont_touch(C,F,A):- \+ is_mp(F),  \+ predicate_property(C,dynamic).

is_a_function(Outer,_):- \+ is_mp(Outer).

make_atermv(MORE,Outer,N,C,CC):- make_aterm(Outer,N,CC,CC),add_val(C,VAL),put_aterm(CC,[VAL,pred:argiza(Outer,N)|MORE]).




avar_merge_to(A,B,C,D):-merge_ls(A,B,C),merge_ls(B,A,D),!.

merge_ls(B,A,D):-merge(B,A,C),intersection(C,C,CC),list_to_set(CC,D).

avar_merge_to(A,B,CC):-avar_merge(A,B,C),avar_merge(B,C,CC).

is_shared(var(X)).
is_shared(val(X)).
%is_shared(forms(X,Y,Z)).


avar_merge([], _, []) :- !.
avar_merge([Shared|T], L, Intersect) :- is_shared(Shared),!,
	Intersect = [Shared|R],
	avar_merge(T, L, R).
avar_merge([X|T], L, Intersect) :-
	memberchk(X, L), !,
	Intersect = [X|R],
	avar_merge(T, L, R).
avar_merge([_|T], L, R) :-
	avar_merge(T, L, R).


aterm(X, List) :- var(List), must(get_attr(X, aterm, List)).
aterm(X, List) :- must(is_list(List)), put_aterm(X, List).

%       An attributed variable with attribute val ATerm has been
%       assigned the val Y

aterm:attr_unify_hook(a(XDict), Y) :- must(memberchk(var:X,XDict)), aunify(X, Y),!.



get_aterm(Y,Dict):-var(Y),!, (get_attr(Y, aterm, Dict) -> true ;  (make_aterm(Y,Y),get_aterm(Y,Dict))),!.
get_aterm(Dict,Dict):-is_dict(Dict),!.
get_aterm(a(Dict),Dict):-!.

put_aterm(Y,New):- var(Y),get_aterm(Y,Before),!,add_to(Before,New).

add_to(Y,New):- must(is_dict(Y)),b_put_dict(Y,New).

b_put_dict(Y,New):-is_dict(New),!,forall(get_dict(K,New,V),b_put_dkv(Y,K,V)).
b_put_dict(Y,New):-is_list(New),!,forall(member(E,New),b_put_dict(Y,E)).
b_put_dict(Y,K:V):-!,b_put_dkv(Y,K,V).
b_put_dict(Y,K=V):-!,b_put_dkv(Y,K,V).
b_put_dict(Y,K-V):-!,b_put_dkv(Y,K,V).
b_put_dict(Y,KV):- compound(KV),KV=..[K,V],!,b_put_dkv(Y,K,V).

b_put_dkv(Y,K,V):-nb_link_dict(K,Y,V),b_set_dict(K,Y,V).

push_val(XDict,Y):- add_val(Y,YY),!,add_to(XDict,YY),!.

aunify(X,Y):-X==Y,!.
aunify(X,Y):-get_aterm(Y,YDict),get_aterm(X,XDict),!,add_to(XDict,YDict),add_to(YDict,XDict),!.
aunify(X,Y):-get_aterm(X,XDict),!,push_val(XDict,Y),!.
aunify(Y,X):-get_aterm(X,XDict),!,push_val(XDict,Y),!.
aunify(X,Y):-make_aterm(V1,A1),make_aterm(V2,A2),!,A1=A2.
aunify(X,Y):-trace_or_throw(aunify(X,Y)).

add_val(Val,var:(Val)):-var(Val).
add_val(Val,val:(Val)):-number(Val).
add_val(Val,val:(Val)):-atom(Val),!.
add_val(Val,nat:(Str)):-catch(text_to_string(Val,Str),_,fail),!.
add_val(Val,nat:(Val)).

% Translate attributes from this module to residual goals
aterm:attribute_goals(X) --> { get_attr(X, aterm, List) }, [aterm(X, List)].

aterm:attr_portray_hook(AttValue, Val):- label(AttValue, Val),!.
aterm:attr_portray_hook(X,Y)-trace_or_throw(aterm:attr_portray_hook(X,Y)).


unify_to_nonvar(ATerm,Y):- trace, ord_memberchk(Y, ATerm),trace.



% I have solved the question I have posed, so never mind. 
% Here is the meta-circular PROLOG interpreter which includes 
% both "cut" and "ancestor cut". (For explanation of what 
% "ancestor cut" is see Sterling and Shapiro book 
% "the art of prolog"). A proof of correctness is going 
% to appear elsewhere. 
%---------------------------------------------------------- 
% meta-circular interpreter which includes 
% "cut", "ancestor cut", "or" and "not" at the meta-level. 
%---------------------------------------------------------- 
% Author: Rafail Ostrovsky                    4/23/1987 
%---------------------------------------------------------- 
%

find_label(Pred,S):- dmsg(must(find_label(Pred,S))),sanity((member(G,S),functor(G,Pred,_))),!.

:- meta_predicate mi(?),mi(?,?),mi(?,?,?),mi(?,?,?,?).

mi(G) :-mi(true,G).
mi(Then,G) :- mi(Then,[mi(G)],G,_). 
mi(S,G1,L) :-mi(true,S,G1,L).

mi(Then,S,G1,L) :- nonvar(L),!,dmsg(cutted_solve(Then,S,G1,L)),!. % This is the REAL cutter
mi(Then,S,V, _):- var(V), ( \+ get_attr(V,aterm,_)),!, throw(error(instantiation_error, _)).
mi(Then,S,true,_) :-!.
mi(Then,S,!,L) :- must(Then==true), !, (var(L);L=cut).
% mi(Then,S,!,L) :- arg(1,S,G),!, functor(G,F,_),!,(var(L);L=cut_to(F)).
mi(Then,S,!(Pred),L) :- !, find_label(Pred,S) -> (var(L);L=cut_to(Pred)) ; L=cut_to(Pred).
mi(Then,S, \+(G1),L) :- !, \+ mi(Then,S,G1,L). 
mi(Then,S, mi(G1),L) :- !, mi(Then,S,G1,L). 
mi(Then,S,(G1->G2;G3),L) :- !, (mi(S,G1,L)->mi(S,G2,L);mi(S,G3,L)),do_then(Then).
mi(Then,S,(G1->G2),L) :- !, (mi(S,G1,L)->mi(S,G2,L)),do_then(Then).
mi(Then,S,(G1,G2),L) :- !,mi(S,G1,L),mi(Then,S,G2,L).
mi(Then,S,(G1;G2),L) :- !,(mi(Then,S,G1,L);mi(Then,S,G2,L)).
mi(Then,S,G,_):- (( \+ predicate_property(G,number_of_clauses(_)));system_pred(G)),!,make_aterm(G,GG),call(GG),do_then(Then).
mi(Then,S,H,L):- 
 make_aterm(H,HA),!,
 copy_term_nat(HA,HH),
   clause(HH,G2),
   (G2==true -> HA=HH ;  
          ((make_aterm(G2,GG2),
            mi([HH|S],(HA=HH,GG2),CUTTO)),
             ((var(CUTTO);non_cutted(H,CUTTO,L))->true;(!,fail)))),
 do_then(Then).

do_then(true):-!.
do_then(Then):- mi(Then).

non_cutted(G,cut_to(F),_):-functor(G,F,_),!,fail.
non_cutted(_,cut,_):-!,fail.
non_cutted(_,CUTTO,CUTTO).


system_pred(clause(_,_)). 
system_pred(true). 
system_pred(mi(_,_,_,_)). 

make_aterm(Var) :- make_aterm(Var, Var).

var_index_root(V, I, Root) :- !. % get_attr(V, clpb, index_root(I,Root)).
remove_hidden_variables(_,_).

project_attributes(QueryVars0, AttrVars) :-
        append(QueryVars0, AttrVars, QueryVars1),
        include(make_aterm, QueryVars1, QueryVars),
        maplist(var_index_root, QueryVars, _, Roots0),
        sort(Roots0, Roots),
        maplist(remove_hidden_variables(QueryVars), Roots).


:- dynamic((term_expansion/2,user:term_expansion/2,system:term_expansion/2)).
:- multifile((term_expansion/2,user:term_expansion/2,system:term_expansion/2)).

%system:goal_expansion(G,_):-writeq(system:goal_expansion(G)),nl,fail.
user:goal_expansion(G,_):-writeq(goal_expansion(G)),nl,fail.
user:goal_expansion(V1=V2,aunify(V1,V2)).
%user:term_expansion(G,_):-writeq(user:term_expansion(G)),nl,fail.
system:term_expansion(G,_):-writeq(system:term_expansion(G)),nl,fail.

%---------------------------------------------------------- 

:- dynamic((p/1),(q/1),(test/1)).

% Example: 
test(X) :- p(X),q(X). 
test(X) :- q(X). 
p(1). 
p(2). 
p(3) :- !(test). 
p(4). 
q(1). 
q(2):-!. 
q(3). 
q(4). 
% ?- mi(Then,S,test(X)) will succeed with X = {1,2,3}. 





% see http://www.cs.bham.ac.uk/~pjh/modules/current/25433/examples/l15_example3.html
:- module(aterm).

% :-at_initialization(mi).


  