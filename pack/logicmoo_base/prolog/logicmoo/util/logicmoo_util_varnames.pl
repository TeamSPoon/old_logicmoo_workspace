/** <module> Logicmoo Debug Tools
% ===================================================================
% File 'logicmoo_util_varnames.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_util_varnames.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% ===================================================================
*/

:- multifile
	unify_goal/5,			% +Read, +Decomp, +M, +Pos, -Pos
	unify_clause_hook/5,
	make_varnames_hook/5,
	open_source/2.			% +Input, -Stream

:- predicate_options(prolog_clause:clause_info/5, 5,
		     [ variable_names(-list)
		     ]).

:- use_module(library(prolog_source)).
:- use_module(library(prolog_clause)). % read_term_at_line/6


show_call_when(P,In,Out):- pmust(call(P,In,Out)),ignore((In\=@=Out,dmsg((show_call_when(P) :- (In,Out))))).

:- thread_local(thlocal:dont_varname/0).
:- thread_local(thlocal:dont_varname_te/0).
:- thread_local(thlocal:try_varname_clause_next/1).

no_varnaming(G):-!, G.
no_varnaming(G):-with_assertions(thlocal:dont_varname,G).

not_member_eq(_,[]):-!.
not_member_eq(E,REST):- \+ identical_member(E,REST).

all_different_vals(_):- thlocal:dont_varname,!.
all_different_vals([_]):-!.
all_different_vals([]):-!.
all_different_vals(Vs):-all_different_vals(Vs,Vs),!.

all_different_vals([],_):-!.
all_different_vals([V|Vs],SET):-subtract_eq(SET,V,REST),!,v_dif_rest(V,REST),all_different_vals(Vs,SET).

v_dif_rest(V,REST):- not_member_eq(V,REST), when('?='(V,_),not_member_eq(V,REST)).

lock_vars(Var):-atomic(Var),!.
lock_vars(Var):-var(Var),!,when(nonvar(Var),is_ftVar(Var)).
lock_vars('$VAR'(_)):-!.
lock_vars([X|XM]):-!,lock_vars(X),lock_vars(XM),!.
lock_vars(XXM):-XXM=..[_,X|XM],lock_vars(X),lock_vars(XM).

unlock_vars(Var):-var(Var),!,del_attr(Var,when),del_attr(Var,clpfd).
unlock_vars(Var):-atomic(Var),!.
unlock_vars('$VAR'(_)):-!.
unlock_vars([X|XM]):-!,unlock_vars(X),unlock_vars(XM),!.
unlock_vars(XXM):-XXM=..[_,X|XM],unlock_vars(X),unlock_vars(XM).

make_subterm_path(Sub,Term,PathO):-vmust(subterm_path(Sub,Term,Path)),!,PathO=Path.

subterm_path(Sub,Term,[]):-Sub==Term,!.
subterm_path(Sub,Term,[arg(N)|Path]):-compound(Term),!,arg(N,Term,TermE),subterm_path(Sub,TermE,Path),!.

get_clause_vars(_):- thlocal:dont_varname,!.
get_clause_vars(MHB):- term_variables(MHB,Vs),get_clause_vars(MHB,Vs).

del_attr_type(Type,Var):-ignore(del_attr(Var,Type)).

get_clause_vars(_,[]):-!.
get_clause_vars(MHB,[V|Vs]):- all_different_vals([V|Vs]),vmust((get_clause_vars_copy(MHB,WVARS),!,vmust(MHB=WVARS),unlock_vars(MHB),must(check_varnames(MHB)))),!,maplist(del_attr_type(clpfd),[V|Vs]).
get_clause_vars(MHB,Vs):- vmust((get_clause_vars_copy(MHB,WVARS),!,vmust(MHB=WVARS),unlock_vars(MHB),must(check_varnames(Vs)))),!.
get_clause_vars(_,_):- !.


get_clause_vars_copy(HB,HB):- ground(HB),!.
get_clause_vars_copy(HH,HH):- sub_term(S,HH),compound(S),S='$VAR'(_),!. % already labled
get_clause_vars_copy(H0,MHB):- must((name_vars(H0,MHB),lock_vars(MHB),as_clause_no_m( MHB,  H, B),
    get_clause_vars_hb_int(H,B))),!.


get_clause_vars_hb_int(H,B):- user:varname_info(H,B,Vs,_),must_maplist(assign_name_equal_var,Vs),!.
get_clause_vars_hb_int(H,B):- call_return_tf(try_get_body_vars(B),_TF1),call_return_tf(try_get_head_vars(H),_TF2),!.


fix_varcase_name(N,VN):-atom_subst(N,'-','_',O),atom_subst(O,'?','_',VN).

no_vars_needed(H):- (thlocal:dont_varname; ( ground(H) ; \+ compound(H))) ,!.
try_get_inner_vars(H):- once((functor(H,_,N),arg(N,H,List),member(vars(Vs),List))),is_list(Vs),term_variables(H,VL),must_maplist(assign_name_equal_var,Vs,VL).

call_return_tf(Call,TF):- ((Call-> TF = t ; TF = nil)).

try_get_head_vars(H):- no_vars_needed(H),!.
try_get_head_vars(H):- user:varname_info(H,_,Vs,_),maplist(assign_name_equal_var,Vs),!.
try_get_head_vars(H):- try_get_inner_vars(H),!.
try_get_head_vars(H):- user:varname_info(_,H,Vs,_),maplist(assign_name_equal_var,Vs),!.

try_get_body_vars(H):- no_vars_needed(H),!.
try_get_body_vars(H):- user:varname_info(_,H,Vs,_),maplist(assign_name_equal_var,Vs),!.
try_get_body_vars(H):- try_get_inner_vars(H).
try_get_body_vars(H):- user:varname_info(H,_,Vs,_),maplist(assign_name_equal_var,Vs),!.
try_get_body_vars((A,B)):-!,try_get_head_vars(A),try_get_head_vars(B).
try_get_body_vars((A;B)):-!,try_get_head_vars(A),try_get_head_vars(B).
try_get_body_vars(C):- C=..[_,L],maplist(try_get_body_vars,L).
try_get_body_vars(_).

vmust(G):-must(G).
:- multifile(user:varname_info/4).
:- dynamic(user:varname_info/4).

% assign_name_equal_var(B):-var(B),!.
assign_name_equal_var(B):-var(B),writeq(assign_name_equal_var(B)),nl,trace,trace_or_throw(var_assign_varname_vars(B)).
assign_name_equal_var(N=V):-must(assign_name_equal_var(N,V)),!.
assign_name_equal_var(_).

assign_name_equal_var(N,V):- (var(V),var(N)),trace_or_throw(var_ignoreq(N,V)).
%assign_name_equal_var(N,V):-var(N),!, (var(V)->ignore(V=N);ignore(V=N)),!.
assign_name_equal_var(N,V):-atom(N),var(V),!,ignoreN(N,V).
assign_name_equal_var(_,'$VAR'(N)):-number(N),!.
assign_name_equal_var(_,'$VAR'(N)):-atom(N),!.
assign_name_equal_var(_,NV):-nonvar(NV),NV\='$VAR'(_),!.
assign_name_equal_var('$VAR'(N),V):- number(N),!,format(atom(VN),'~w',[N]),ignoreN(VN,V).
assign_name_equal_var('$VAR'(N),V):-atom(N),atom_concat('"?',LS,N),atom_concat(NN,'"',LS),fix_varcase_name(NN,VN),!,ignoreN(VN,V).
assign_name_equal_var('$VAR'(N),V):-atom(N),ignoreN(N,V).
assign_name_equal_var(N,V):-atom(N),!,ignoreN(N,V).

assign_name_equal_var(N,V):-ignore(N=V).

ignoreN('$VAR'(Name),Var):-atom(Name),!,ignoreN(Name,Var).
ignoreN(N,V):-var(V),var(N),!,V=N.
ignoreN(Name,Var):- try_save1var(Name,Var),ignoreN0(Name,Var),!.

% ignoreN0(Name,Var):- attvar(Var),put_attr(Var,varname,Name),!.
ignoreN0(N,'$VAR'(N)):-atom(N),!.
ignoreN0(N,V):-ignore(V='$VAR'(N)),!.
ignoreN0(Name,Var):- vmust(Var = '$VAR'(Name)).


try_save1var(N,V):-nb_current('$variable_names',Vs),!,register_var(N=V,Vs,NewVS),b_setval('$variable_names',NewVS).
try_save1var(N,V):-b_setval('$variable_names',[N=V]).




:- thread_local(thlocal:current_local_why/2).
:- thread_local(thlocal:current_why_source/1).

current_why(F):- thlocal:current_why_source(F),!.
current_why(F):- thlocal:current_local_why(F,_),!.
current_why(F):- current_predicate(current_source_location/1),current_source_location(F),!.
current_why(unk).

current_source_location(F):- catch(notrace((current_source_location0(W), (W= (F:foo) -> true;F=W))),E,F=error(E)),!.
current_source_location0(F:L):-source_location(F,L),!.
current_source_location0(F:L):-prolog_load_context(file,F),current_input(S),line_position(S,L),!.
current_source_location0(When):-loading_file(When).
current_source_location0(F:L):- current_filesource(F),ignore((prolog_load_context(stream,S),!,line_count(S,L))),!.
current_source_location0(F:L):- prolog_load_context(file,F),!,ignore((prolog_load_context(stream,S),!,line_count(S,L))),!.
current_source_location0(module(M)):-source_module(M),!.
current_source_location0(When):-current_input(S),findall(NV,stream_property(S,NV),When),!.
current_source_location0(module(user)):-!.
current_source_location0(unknown:0).

:-export(save_clause_vars/2).
% save_clause_vars(_,[]):-!.
save_clause_vars(MHB,Vs):- current_why(Why),!,save_clause_vars(MHB,Vs,Why),!.
save_clause_vars(MHB,Vs):- trace,current_why(Why),!,save_clause_vars(MHB,Vs,Why).

% ?- clause(pui_help:prolog_help_topic(A),B,ClauseRef), prolog_clause:clause_info(ClauseRef, File, TermPos, NameOffset, Options).


:-export(save_clause_vars/3).
save_clause_vars(_, [],_):-!.
save_clause_vars(MHB,Vs,Why:_):-atom(Why),!,save_clause_vars(MHB,Vs,Why).
save_clause_vars(MHB,Vs,Why):-  ( \+ \+ (as_clause_w_m(MHB, M, H, B, MB),save_clause_vars(M,H,MB,B,Vs,Why))).

:-export(clause_eq/3).
%clause_eq(H,B,R):-clause(H,B,R),clause(RH,RB,R),term_variables(RH:RB,RVs),term_variables(H:B,Vs).
clause_eq(H,B,R):-copy_term(H:B,CHB),clause(H,B,R),CHB=@=H:B.

locate_clause_ref(M,H,_MB,_B,_ClauseRef):- (\+ (predicate_property(M:H,number_of_clauses(_)))),(\+ (predicate_property(_:H,number_of_clauses(_)))),!,fail.
locate_clause_ref(M,H,MB,B,ClauseRef):-clause_eq(M:H,MB:B,ClauseRef).
locate_clause_ref(_M,H,MB,B,ClauseRef):-clause_eq(H,MB:B,ClauseRef).
locate_clause_ref(_M,H,MB,B,ClauseRef):-clause_eq(_:H,MB:B,ClauseRef).
locate_clause_ref(M,H,_MB,B,ClauseRef):-clause_eq(M:H,B,ClauseRef).
locate_clause_ref(_M,H,_MB,B,ClauseRef):-clause_eq(H,B,ClauseRef).
locate_clause_ref(_M,H,_MB,B,ClauseRef):-clause_eq(_:H,B,ClauseRef).

clause_ref_vars(ClauseRef,Was):-prolog_clause:clause_info(ClauseRef, _File, _TermPos, _NameOffset, [variable_names(Was)]).
clause_ref_file(ClauseRef,File):-prolog_clause:clause_info(ClauseRef, File, _TermPos, _NameOffset, []).

:-export(save_to_clause_ref/3).
save_to_clause_ref(ClauseRef,Vs,Why):- assert_if_new_kv(names(ClauseRef,Vs)),assert_if_new_kv(names_why(ClauseRef,Why)),!.

:-export(save_clause_vars/6).
save_clause_vars(M,H,MB,B,Vs,Why:_):-atom(Why),!,save_clause_vars(M,H,MB,B,Vs,Why).
save_clause_vars(M,H,MB,B,Vs,Why):- locate_clause_ref(M,H,MB,B,ClauseRef),clause_ref_vars(ClauseRef,Was),
   (show_call_failure(Was=Vs)->true;save_to_clause_ref(ClauseRef,Vs,Why)),!.
save_clause_vars(_M,H,_MB,B,Vs,Why):- assert_if_new_kv(user:varname_info(H,B,Vs,Why)).

assert_if_new_kv(A):- clause_asserted(A),!.
assert_if_new_kv(A):- assertz(A).


ensure_vars_labled_r(I,O):- 
  once((((nb_current('$variable_names',Vs),Vs\==[])),
   copy_term(I:Vs,O:OVs),
    must_maplist(assign_name_equal_var,OVs))),
   (O \=@= I ;  ground(O)),!.

ensure_vars_labled_r(I,O):- 
     once((get_clause_vars_copy(I,O),unlock_vars(O))),
     (O \=@= I ;  ground(O)),!.

ensure_vars_labled_r(I,O):- name_vars(I,O),I\=@=O.



contains_singletons(Term):- not(ground(Term)),call_not_not(((term_variables(Term,Vs),
   snumbervars4(Term,0,_,[attvar(bind),singletons(true)]),member('$VAR'('_'),Vs)))).

:- meta_predicate(call_not_not(0)).

call_not_not(G):- \+ \+ G.

contains_badvarnames(Term):- fail, notrace((sub_term(SubV,Term),compound(SubV),SubV='$VAR'(Sub),bad_varnamez(Sub))),!.

bad_varnamez(Sub):- atom(Sub),!,atom_contains(Sub,'.').
bad_varnamez(Sub):- var(Sub),!.
bad_varnamez(Sub):- integer(Sub),!, (Sub < 0 ; Sub > 991000).
bad_varnamez(Sub):- number(Sub).




  
% ========================================================================================
% safe_numbervars/1 (just simpler safe_numbervars.. will use a random start point so if a partially numbered getPrologVars wont get dup getPrologVars)
% Each prolog has a specific way it could unnumber the result of a safe_numbervars
% ========================================================================================
% 7676767
safe_numbervars(E,EE):-duplicate_term(E,EE),
  get_gtime(G),snumbervars4(EE,G,End,[attvar(skip),functor_name('$VAR'),singletons(true)]),
  term_variables(EE,AttVars),
  snumbervars4(EE,End,_,[attvar(skip),functor_name('$VAR'),singletons(true)]),
  forall(member(V,AttVars),(copy_term(V,VC,Gs),V='$VAR'(VC=Gs))),check_varnames(EE).

name_vars(Term,Named):- ignore((source_variables_l(AllS))), copy_term(Term+AllS,Named+CAllS),maplist(assign_name_equal_var,CAllS).

get_gtime(GG):- get_time(T),convert_time(T,_A,_B,_C,_D,_E,_F,G),GG is (floor(G) rem 500).

safe_numbervars(EE):-get_gtime(G),snumbervars4(EE,G,_End,[attvar(skip),functor_name('$VAR'),singletons(true)]),check_varnames(EE).



%=========================================
% unnumbervars
%=========================================

% source_module(M):-!,M=u.
source_module(M):-nonvar(M),source_module(M0),!,M0=M.
source_module(M):-loading_module(M),!.
source_module(M):-'$set_source_module'(M,   M),!.

:- thread_local(thlocal:last_source_file/1).
loading_file(FIn):- ((source_file0(F) *-> (retractall(thlocal:last_source_file(_)),asserta(thlocal:last_source_file(F))) ; (fail,thlocal:last_source_file(F)))),!,F=FIn.
source_file0(F):-source_location(F,_).
source_file0(F):-prolog_load_context(file, F).
source_file0(F):-prolog_load_context(source, F).
source_file0(F):-seeing(X),is_stream(X),stream_property(X,file_name(F)),exists_file(F).
source_file0(F):-prolog_load_context(stream, S),stream_property(S,file_name(F)),exists_file(F).
source_file0(F):-findall(E,catch((stream_property( S,mode(read)),stream_property(S,file_name(E)),exists_file(E),
  line_count(S,C),C>0),_,fail),L),last(L,F).


source_variables_l(AllS):-
  (prolog_load_context(variable_names,Vs1);Vs1=[]),
  (nb_current('$variable_names', Vs2);Vs2=[]),
  catch((parent_goal('$toplevel':'$execute_goal2'(_, Vs3),_);Vs3=[]),_,Vs3=[]),
  append(Vs1,Vs2,Vs12),append(Vs12,Vs3,All),!,list_to_set(All,AllS),
  nb_linkval('$variable_names', AllS).


source_variables(Vs):- (((prolog_load_context(variable_names,Vs),Vs\==[]);
   (b_getval('$variable_names', Vs),Vs\==[]))),!.
source_variables(Vars):-var(Vars),parent_goal('$toplevel':'$execute_goal2'(_, Vars),_),!.
source_variables([]).

b_implode_varnames(_):-!.
b_implode_varnames0([]):-!.
b_implode_varnames0([N=V|Vs]):- ignore((V='$VAR'(N);V=N)),b_implode_varnames0(Vs),!.

imploded_copyvars(C,CT):-vmust((source_variables(Vs),copy_term(C-Vs,CT-VVs),b_implode_varnames(VVs))),!.


:- export(unnumbervars/2).
unnumbervars(X,YY):- lbl_vars(X,[],Y,_Vs),!, vmust(YY=Y).
% TODO compare the speed
% unnumbervars(X,YY):- vmust(unnumbervars0(X,Y)),!,vmust(Y=YY).



:- export(unnumbervars_and_save/2).
unnumbervars_and_save(X,YY):-
   lbl_vars(X,[],Y,Vs),
    (Vs==[]->vmust(X=YY);
    ( % writeq((lbl_vars(X,Y,Vs))),nl,
     save_clause_vars(Y,Vs),vmust(Y=YY))).

/*
% todo this slows the system!
unnumbervars0(X,clause(UH,UB,Ref)):- sanity(nonvar(X)),
  X = clause(H,B,Ref),!,
  vmust(unnumbervars0((H:-B),(UH:-UB))),!.

unnumbervars0(X,YY):-lbl_vars(X,YY,_Vs).
*/
/*
lbl_vars(X,YY):-
   must_det_l((with_output_to(string(A),write_term(X,[snumbervars4(true),character_escapes(true),ignore_ops(true),quoted(true)])),
   atom_to_term(A,Y,_NewVars),!,vmust(YY=Y))),check_varnames(YY).
lbl_vars(X,YY,Vs):-!,lbl_vars(X,[],YY,Vs).
*/

lbl_vars(X,Vs,X,Vs):- ( \+ compound(X)),!.
lbl_vars('$VAR'(X),IVs,Y,Vs):-!, (memberchk(X=Y,IVs)->Vs=IVs;Vs=[X=Y|IVs]).
lbl_vars([X|XM],IVs,[Y|YM],Vs):-!,
  lbl_vars(X,IVs,Y,VsM),
  lbl_vars(XM,VsM,YM,Vs).
lbl_vars(XXM,IVs,YYM,Vs):-
  XXM=..[F,X|XM],
  lbl_vars(X,IVs,Y,VsM),
  lbl_vars(XM,VsM,YM,Vs),
  YYM=..[F,Y|YM].

/*
lbl_vars(X,YY,Vs):-
 must_det_l((
   with_output_to(codes(A),write_term(X,[numbervars(true),character_escapes(true),ignore_ops(true),quoted(true)])),   
   read_term_from_codes(A,Y,[variable_names(Vs),character_escapes(true),ignore_ops(true)]),!,vmust(YY=Y),check_varnames(YY))).

unnumbervars_and_save(X,YO):-
 term_variables(X,TV),
 vmust((source_variables(Vs),
   with_output_to(string(A),write_term(X,[numbervars(true),variable_names(Vs),character_escapes(true),ignore_ops(true),quoted(true)])))),
   vmust(atom_to_term(A,Y,NewVars)),
   (NewVars==[]-> YO=X ; (length(TV,TVL),length(NewVars,NewVarsL),(NewVarsL==TVL-> (YO=X) ; (trace,add_newvars(NewVars),Y=X)))).



unnumbervars_and_copy(X,YO):-
 term_variables(X,TV),
 vmust((source_variables(Vs),
   with_output_to(string(A),write_term(X,[numbervars(true),variable_names(Vs),character_escapes(true),ignore_ops(true),quoted(true)])))),
   vmust(atom_to_term(A,Y,NewVars)),
   (NewVars==[]-> YO=X ; (length(TV,TVL),length(NewVars,NewVarsL),(NewVarsL==TVL-> (YO=X) ; (trace,add_newvars(NewVars),Y=X)))).
*/

%add_newvars(_):-!.
add_newvars(Vs):- (var(Vs);Vs=[]),!.
add_newvars([N=V|Vs]):- add_newvar(N,V),!,add_newvars(Vs).


add_newvar(_,V):-nonvar(V),!.
add_newvar(N,_):-var(N),!.
add_newvar('A',_):-!.
add_newvar('B',_):-!.
add_newvar(N,_):- atom(N),atom_concat('_',_,N),!.
add_newvar(N,V):- 
  b_getval('$variable_names', V0s),
  remove_grounds(V0s,Vs),
 once((member(NN=Was,Vs),N==NN,var(Was),var(V),(Was=V))-> (V0s==Vs->true;nb_linkval('$variable_names',Vs)); nb_linkval('$variable_names',[N=V|Vs])).

remove_grounds(Vs,Vs):-var(Vs),!.
remove_grounds([],[]):-!.
remove_grounds([N=V|NewCNamedVarsS],NewCNamedVarsSG):-
   (N==V;ground(V)),remove_grounds(NewCNamedVarsS,NewCNamedVarsSG).
remove_grounds([N=V|V0s],[N=NV|Vs]):-
   (var(V) -> NV=V ; NV=_ ),
   remove_grounds(V0s,Vs).

% renumbervars(X,X):-ground(X),!.
renumbervars(X,Y):-renumbervars1(X,[],Y,_),!.
renumbervars(X,Z):-unnumbervars(X,Y),safe_numbervars(Y,Z),!.
renumbervars(Y,Z):-safe_numbervars(Y,Z),!.


renumbervars1(X,Y):-renumbervars1(X,[],Y,_).

renumbervars1(V,IVs,'$VAR'(X),Vs):- var(V), sformat(atom(X),'~w_RNV',[V]), !, (memberchk(X=V,IVs)->Vs=IVs;Vs=[X=V|IVs]).
renumbervars1(X,Vs,X,Vs):- ( \+ compound(X)),!.
renumbervars1('$VAR'(V),IVs,Y,Vs):- sformat(atom(X),'~w_VAR',[V]), !, (memberchk(X=Y,IVs)->Vs=IVs;Vs=[X=Y|IVs]).
renumbervars1([X|XM],IVs,[Y|YM],Vs):-!,
  renumbervars1(X,IVs,Y,VsM),
  renumbervars1(XM,VsM,YM,Vs).
renumbervars1(XXM,IVs,YYM,Vs):-
  XXM=..[F,X|XM],
  renumbervars1(X,IVs,Y,VsM),
  renumbervars1(XM,VsM,YM,Vs),
  YYM=..[F,Y|YM].


% register_var(?, ?, ?)
%
%   During copying one has to remeber copies of variables which can be used further during copying.
%   Therefore the register of variable copies is maintained.
%
register_var(N=V,IN,OUT):- (var(N)->true;register_var(N,IN,V,OUT)),!.

register_var(N,T,V,OUTO):-register_var_0(N,T,V,OUT),vmust(OUT=OUTO),!.
register_var(N,T,V,O):-append(T,[N=V],O),!.

register_var_0(N,T,V,OUT):- atom(N),is_list(T),member(NI=VI,T),atom(NI),N=NI,V=@=VI,samify(V,VI),!,OUT=T.
register_var_0(N,T,V,OUT):- atom(N),is_list(T),member(NI=VI,T),atom(NI),N=NI,V=VI,!,OUT=T.

register_var_0(N,T,V,OUT):- vmust(nonvar(N)),
   ((name_to_var(N,T,VOther)-> vmust((OUT=T,samify(V,VOther)));
     (once(nb_getval('$variable_names',Before);Before=[]),
      (name_to_var(N,Before,VOther)  -> vmust((samify(V,VOther),OUT= [N=V|T]));
         (var_to_name(V,T,_OtherName)                  -> OUT= [N=V|T];
           (var_to_name(V,Before,_OtherName)              -> OUT= [N=V|T];fail)))))),!.


register_var_0(N,T,V,OUT):- var(N),
   (var_to_name(V,T,N)                -> OUT=T;
     (once(nb_getval('$variable_names',Before);Before=[]),
          (var_to_name(V,Before,N)   -> OUT= [N=V|T];
               OUT= [N=V|T]))),!.





% different variables (now merged)
samify(V,V0):-var(V),var(V0),!,vmust(V=V0).
samify(V,V0):-vmust(V=@=V0),V=V0. 

var_to_name(V,[N=V0|T],N):-
    V==V0 -> true ;          % same variables
    var_to_name(V,T,N).

name_to_var(N,T,V):- var(N),!,var_to_name(N,T,V).
name_to_var(N,[N0=V0|T],V):- 
   N0==N -> samify(V,V0) ; name_to_var(N,T,V).


/*
% ===================================================================
% Safely number vars
% ===================================================================
bugger_numbervars_with_names(Term):-
   term_variables(Term,Vars),bugger_name_variables(Vars),!,snumbervars4(Vars,91,_,[attvar(skip),singletons(true)]),!,

bugger_name_variables([]).
bugger_name_variables([Var|Vars]):-
   (var_property(Var, name(Name)) -> Var = '$VAR'(Name) ; true),
   bugger_name_variables(Vars).

*/
:- export(snumbervars/1).
snumbervars(Term):-snumbervars(Term,0,_).

:- export(snumbervars/3).
snumbervars(Term,Start,End):- integer(Start),var(End),!,snumbervars4(Term,Start,End,[]).
snumbervars(Term,Start,List):- integer(Start),is_list(List),!,snumbervars4(Term,Start,_,List).
snumbervars(Term,Functor,Start):- integer(Start),atom(Functor),!,snumbervars4(Term,Start,_End,[functor_name(Functor)]).
snumbervars(Term,Functor,List):- is_list(List),atom(Functor),!,snumbervars4(Term,0,_End,[functor_name(Functor)]).


:- export(snumbervars/4).
snumbervars(Term,Start,End,List):-snumbervars4(Term,Start,End,List).

% snumbervars(Term,Functor,Start,End,List):-vmust(( vmust(var(End);number(End)),snumbervars4(Term,Start,End,[functor_name(Functor)|List]))),check_varnames(Term).


check_varnames(Vs):-var(Vs),!.
check_varnames([]):-!.
check_varnames([N=V|Vs]):-atom(N),var(V),!,check_varnames(Vs).
check_varnames(Term):- contains_badvarnames(Term),!,dumpST0,trace,stop_rtrace,trace,!,dtrace(contains_badvarnames(Term)).
check_varnames(_).

snumbervars4(Term,Start,End,List):-  \+ member(attvar(_),List),!,snumbervars4(Term,Start,End,[attvar(skip)|List]).
snumbervars4(Term,Start,End,List):-must_det_l((integer(Start),is_list(List), numbervars(Term,Start,End,List),check_varnames(Term))).

:- public varname:attr_unify_hook/2.
:- public varname:attr_portray_hook/2.
varname:attr_unify_hook(_,_).
varname:attr_portray_hook(Value, _Var) :- nonvar(Value),!,writeq('?'(Value)).
:- public
	varname:portray_attvar/1.
'varname':portray_attvar(Var) :-
	write('{<'),
        
        ((get_attr(Var,varname, VarName))->true;sformat(VarName,'~q',[Var])),
	get_attrs(Var, Attr),
	catch(writeq('??'(VarName,Attr)),_,'$attvar':portray_attrs(Attr, Var)),
	write('>}').

:-export(try_save_vars/1).
try_save_vars(_):- thlocal:dont_varname,!.
try_save_vars(HB):-ignore((nb_current('$variable_names',Vs),Vs\==[],save_clause_vars(HB,Vs))),!.

read_source_files:- 
 ensure_loaded(library(make)),
 forall(make:modified_file(F),retractall(user:varname_info_file(F))),
 forall(source_file(F),catch(with_assertions(thlocal:dont_varname_te,read_source_file_vars(F)),_,true)).

:- dynamic(user:varname_info_file/1).
read_source_file_vars(F):- \+ ((atom(F),exists_file(F))),!, forall(filematch(F,E),read_source_file_vars(E)).
read_source_file_vars(F):- clause_asserted(user:varname_info_file(F)),!.
read_source_file_vars(F):- assert(user:varname_info_file(F)), catch(read_source_file_vars_1(F),E,(dmsg(E),retractall(user:varname_info_file(F)))).


save_file_source_vars(_F,end_of_file,_Vs):-!.
save_file_source_vars(_F,_T,[]):-!.
save_file_source_vars(F,T,Vs):- b_setval('$variable_names',Vs),!,with_assertions(thlocal:current_why_source(F),save_clause_vars(T,Vs)),!.

:- if(true).

read_source_terms(File,In):-
	repeat,
	  catch(prolog_read_source_term(In, Term, Expanded, [ variable_names(Vs) /*, term_position(TermPos) */ ]),
		E,(dmsg(E),trace,fail)),
          % stream_position_data(line_count, TermPos, LineNo),
	  ignore(save_file_source_vars(File /* :LineNo */,Term,Vs)),
	  (   is_list(Expanded)
	  ->  member(T, Expanded)
	  ;   T = Expanded
	  ),
	(   T == end_of_file
	->  !
	;  ( T\==Term, save_file_source_vars(File/* :LineNo */,T,Vs)),
	    fail
	).


% new method
read_source_file_vars_1(File):-
	setup_call_cleanup(
	    prolog_open_source(File, In),
	    read_source_terms(File,In),
	    prolog_close_source(In)),!.

:- else.

% old method
read_source_file_vars_1(F):- 
   catch(((setup_call_cleanup(open(F,S),read_vars_until_oes(F,S),close(S)))),E,(dmsg(E),throw(E))).

read_vars_until_oes(_,S):-at_end_of_stream(S),!.
read_vars_until_oes(F,S):- repeat,catch(once(read_vars_until_oes_1(F,S)),E,ddmsg(E)),at_end_of_stream(S).

eat_to_term(S):-repeat, \+ eat_to_term_1(S).
eat_to_term_1(S):-peek_char(S,W),char_type(W,white),get_char(S,_),!.
eat_to_term_1(S):-line_count(S,C),C=1,peek_char(S,'#'),read_line_to_codes(S,_),!.

read_vars_until_oes_1(_,S):- eat_to_term(S).
read_vars_until_oes_1(F,S):-
   line_count(S,C),
   read_term(S,T,[variable_names(Vs)]),   
   save_file_source_vars(F:C,T,Vs).

:- endif.

ensure_vars_labled(I,I):- (thlocal:dont_varname;no_vars_needed(I)),!.
% ensure_vars_labled(I,I):- !.
% ensure_vars_labled(I,OO):- acyclic_term(O),term_variables(I,Vs),all_different_vals(Vs),ensure_vars_labled_r(I,O),vmust(acyclic_term(O)),!,OO=O.
ensure_vars_labled(I,OO):- acyclic_term(O),term_variables(I,Vs),ensure_vars_labled_r(I,O),\+ \+ ((I=O,vmust(all_different_vals(Vs)))),!,vmust(acyclic_term(O)),!,OO=O.
ensure_vars_labled(I,OO):- acyclic_term(O),ensure_vars_labled_r(I,O),vmust(acyclic_term(O)),!,OO=O.
ensure_vars_labled(I,I).

% :- prolog_load_context(module,M),source_location(S,_),forall(source_file(H,S),(functor(H,F,A),export(F/A),'$set_predicate_attribute'(M:H, hide_childs, 1),module_transparent(F/A))).

logicmoo_util_varnames_file.
:- export(logicmoo_util_varnames_file/0).

:- initialization(read_source_files).

user:term_expansion(HB,_):- \+ thlocal:dont_varname_te,\+ thlocal:dont_varname, current_predicate(logicmoo_util_varnames_file/0),once(try_save_vars(HB)),fail.

