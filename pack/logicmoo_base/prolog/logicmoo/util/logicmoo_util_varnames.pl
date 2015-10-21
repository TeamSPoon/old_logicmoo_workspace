/* <module> Logicmoo Debug Tools
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
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_varnames.pl
:- module(logicmoo_varnames,
          [ ain00/1,
            count_members_eq/3,
            all_different_vals/1,
            all_different_vals/2,
            set_varname/2,
            set_varname/3,
            write_functor/2,
            atom_subst_frak_0/4,
            logicmoo_varnames:attr_portray_hook/2,
            logicmoo_varnames:attr_unify_hook/2,
            renumbervars/3,
            b_implode_varnames/1,
            b_implode_varnames0/1,
            bad_varnamez/1,
            call_not_not/1,
            call_return_tf/2,
            check_varnames/1,
            clause_ref_file/2,
            clause_ref_vars/2,
            contains_badvarnames/1,
            contains_singletons/1,
            del_attr_type/2,
            copy_term_and_varnames/2,
            mpred_numbervars_with_names/1,
            ensure_vars_labled/2,
            ensure_vars_labled_r/2,
            fix_varcase_name/2,
            get_clause_vars/1,
            get_clause_vars/2,
            get_clause_vars_copy/2,
            get_clause_vars_hb_int/2,
            imploded_copyvars/2,
            locate_clause_ref/5,
            lock_vars/1,
            logicmoo_util_varnames_file/0,
            make_subterm_path/3,
            maybe_record_scanned_file/0,
            
            no_varnaming/1,
            no_vars_needed/1,
            not_member_eq/2,            
   print_numbervars_maybe/1,
   print_numbervars/1,
            read_source_file_vars/1,
            read_source_file_vars_1/1,
            read_source_vars/2,
            replace_in_string_frak_0/4,
            save_clause_vars/2,
            save_clause_vars/3,
            save_clause_vars/6,
            save_file_source_vars/3,
            save_to_clause_ref/3,
            scan_source_files_for_varnames/0,
            dcall_if_verbose/1,
            dcall_when/3,
            snumbervars4/4,
            snumbervars5/4,
            subterm_path/3,
            term_expansion_save_vars/1,
            try_get_body_vars/1,
            try_get_head_vars/1,
            try_get_inner_vars/1,
            set_varname/2,
            try_save_vars/1,
            unlock_vars/1,
            v_dif_rest/2,
            vmust/1,
            init_varname_stores/1,
            maybe_scan_source_files_for_varnames/0
          ]).
:- multifile
        prolog:make_hook/2.
:- meta_predicate
       ain00(:),
        call_not_not(0),
        call_return_tf(0, ?),
        contains_singletons(*),
        get_clause_vars(:),
        get_clause_vars(:, ?),        
        no_varnaming(0),
        dcall_when(2, ?, ?),
        snumbervars4(*, ?, ?, ?),
        snumbervars5(*, ?, ?, ?),
        try_save_vars(:),
        init_varname_stores(?),
        vmust(0).

:- module_transparent
            all_different_vals/1,
            all_different_vals/2,
            set_varname/2,
            set_varname/3,
            atom_subst_frak_0/4,
            logicmoo_varnames:attr_portray_hook/2,
            logicmoo_varnames:attr_unify_hook/2,
            b_implode_varnames/1,
            b_implode_varnames0/1,
            bad_varnamez/1,
            call_not_not/1,
            call_return_tf/2,
            check_varnames/1,
            clause_ref_file/2,
            clause_ref_vars/2,
            contains_badvarnames/1,
            contains_singletons/1,
            del_attr_type/2,
            ensure_vars_labled/2,
            ensure_vars_labled_r/2,
            fix_varcase_name/2,
            get_clause_vars/1,
            get_clause_vars/2,
            get_clause_vars_copy/2,
            get_clause_vars_hb_int/2,
            imploded_copyvars/2,
            locate_clause_ref/5,
            lock_vars/1,
            logicmoo_util_varnames_file/0,
            make_subterm_path/3,
            maybe_record_scanned_file/0,            
            no_varnaming/1,
            no_vars_needed/1,
            not_member_eq/2,            
   print_numbervars_maybe/1,
   print_numbervars/1,
            read_source_file_vars/1,
            read_source_file_vars_1/1,
            read_source_vars/2,
            replace_in_string_frak_0/4,
            save_clause_vars/2,
            save_clause_vars/3,
            save_clause_vars/6,
            save_file_source_vars/3,
            save_to_clause_ref/3,
            scan_source_files_for_varnames/0,
            dcall_if_verbose/1,
            dcall_when/3,
            snumbervars4/4,
            snumbervars5/4,
            subterm_path/3,
            term_expansion_save_vars/1,
            try_get_body_vars/1,
            try_get_head_vars/1,
            try_get_inner_vars/1,
            set_varname/2,
            try_save_vars/1,
            unlock_vars/1,
            v_dif_rest/2,
            vmust/1,
            init_varname_stores/1
 .

 :- meta_predicate logicmoo_varnames:dcall_if_verbose(0).
:- include('logicmoo_util_header.pi').

:- use_module(library(when)).

:- prolog_clause:multifile((
	unify_goal/5,			% +Read, +Decomp, +M, +Pos, -Pos
	unify_clause_hook/5,
	make_varnames_hook/5,
	open_source/2)).			% +Input, -Stream

:- predicate_options(prolog_clause:clause_info/5, 5,
		     [ variable_names(-list)
		     ]).

:- use_module(library(prolog_source)).
:- use_module(library(prolog_clause)). % read_term_at_line/6


:- meta_predicate vmust(0).
vmust(G):-must(G).

dcall_when(P,In,Out):- must(call(P,In,Out)),ignore((In\=@=Out,dmsg((dcall_when(P) :- (In,Out))))).

:- create_prolog_flag(mpred_vars, false, [type(boolean)]).
:- thread_local(t_l:dont_varname/0).
:- thread_local(t_l:dont_varname_te/0).
:- thread_local(t_l:try_varname_clause_next/1).

no_varnaming(G):-!, G.
no_varnaming(G):-w_tl(t_l:dont_varname,G).

not_member_eq(_,[]):-!.
not_member_eq(E,REST):- \+ identical_member(E,REST).

all_different_vals(_):- t_l:dont_varname,!.
all_different_vals([_]):-!.
all_different_vals([]):-!.
all_different_vals(Vs):-all_different_vals(Vs,Vs),!.

all_different_vals([],_):-!.
all_different_vals([V|Vs],SET):-delete_eq(SET,V,REST),!,v_dif_rest(V,REST),all_different_vals(Vs,SET).

v_dif_rest(V,REST):- not_member_eq(V,REST), when('?='(V,_),not_member_eq(V,REST)).

lock_vars(Var):-atomic(Var),!.
lock_vars(Var):-var(Var),!,when:when(nonvar(Var),Var='$VAR'(_)).
lock_vars('$VAR'(_)):-!.
lock_vars([X|XM]):-!,lock_vars(X),lock_vars(XM),!.
lock_vars(XXM):-XXM=..[_,X|XM],lock_vars(X),lock_vars(XM).

unlock_vars(Var):-var(Var),!,del_attr(Var,when).
unlock_vars(Var):-atomic(Var),!.
unlock_vars('$VAR'(_)):-!.
unlock_vars([X|XM]):-!,unlock_vars(X),unlock_vars(XM),!.
unlock_vars(XXM):-XXM=..[_,X|XM],unlock_vars(X),unlock_vars(XM).


make_subterm_path(Sub,Term,PathO):-vmust(subterm_path(Sub,Term,Path)),!,PathO=Path.

subterm_path(Sub,Term,[]):-Sub==Term,!.
subterm_path(Sub,Term,[arg(N)|Path]):-compound(Term),!,arg(N,Term,TermE),subterm_path(Sub,TermE,Path),!.

get_clause_vars(CV):- hotrace(get_clause_vars_nontraced(CV)).
:- export(get_clause_vars_nontraced/1).
get_clause_vars_nontraced(_):- t_l:dont_varname,!.
get_clause_vars_nontraced(_:V):- var(V),!,ignore((nb_current('$variable_names',Vs),member(N=V0,Vs),V0==V,set_varname(write_functor,N,V))).
get_clause_vars_nontraced(MHB):- term_variables(MHB,Vs),must(get_clause_vars(MHB,Vs)).

:- '$set_predicate_attribute'(get_clause_vars(_), trace, 1).
:- '$set_predicate_attribute'(get_clause_vars(_), hide_childs, 1).

del_attr_type(Type,Var):-ignore(del_attr(Var,Type)).

get_clause_vars(_,[]):-!.
get_clause_vars(MHB,[V|Vs]):- all_different_vals([V|Vs]),vmust((get_clause_vars_copy(MHB,WVARS),!,
   vmust(MHB=WVARS),unlock_vars(MHB),must(check_varnames(MHB)))),!,maplist(del_attr_type(when),[V|Vs]).
get_clause_vars(MHB,Vs):- vmust((get_clause_vars_copy(MHB,WVARS),!,vmust(MHB=WVARS),unlock_vars(MHB),must(check_varnames(Vs)))),!.
get_clause_vars(_,_):- !.


get_clause_vars_copy(HB,HB):- ground(HB),!.
get_clause_vars_copy(HH,HH):- sub_term(S,HH),compound(S),S='$VAR'(_),!. % already labled
get_clause_vars_copy(H0,MHB):- must((copy_term_and_varnames(H0,MHB),lock_vars(MHB),as_clause_no_m( MHB,  H, B),
    get_clause_vars_hb_int(H,B))),!.


get_clause_vars_hb_int(H,B):- varname_cache:varname_info(H,B,Vs,_),must_maplist(set_varname(write_functor),Vs),!.
get_clause_vars_hb_int(H,B):- call_return_tf(try_get_body_vars(B),_TF1),call_return_tf(try_get_head_vars(H),_TF2),!.


atom_subst_frak_0(A,F,R,K):-replace_in_string_frak_0(F,R,A,K),!.
replace_in_string_frak_0(F,R,A,K):-atom(A),!,atom_string(A,S),replace_in_string_frak_0(F,R,S,C),atom_string(K,C).
replace_in_string_frak_0(SepChars,Repl,A,C):- atomics_to_string(B,SepChars,A),atomics_to_string(B,Repl,C).


fix_varcase_name(N,VN):-atom_subst_frak_0(N,'-','_',O),atom_subst_frak_0(O,'?','_',VN).

no_vars_needed(H):- (t_l:dont_varname; ( ground(H) ; \+ compound(H))) ,!.
try_get_inner_vars(H):- once((functor(H,_,N),arg(N,H,List),member(vars(Vs),List))),is_list(Vs),term_variables(H,VL),must_maplist(set_varname(write_functor),Vs,VL).

call_return_tf(Call,TF):- ((Call-> TF = t ; TF = nil)).

try_get_head_vars(H):- no_vars_needed(H),!.
try_get_head_vars(H):- varname_cache:varname_info(H,_,Vs,_),maplist(set_varname(write_functor),Vs),!.
try_get_head_vars(H):- try_get_inner_vars(H),!.
try_get_head_vars(H):- varname_cache:varname_info(_,H,Vs,_),maplist(set_varname(write_functor),Vs),!.

try_get_body_vars(H):- no_vars_needed(H),!.
try_get_body_vars(H):- varname_cache:varname_info(_,H,Vs,_),maplist(set_varname(write_functor),Vs),!.
try_get_body_vars(H):- try_get_inner_vars(H).
try_get_body_vars(H):- varname_cache:varname_info(H,_,Vs,_),maplist(set_varname(write_functor),Vs),!.
try_get_body_vars((A,B)):-!,try_get_head_vars(A),try_get_head_vars(B).
try_get_body_vars((A;B)):-!,try_get_head_vars(A),try_get_head_vars(B).
try_get_body_vars(C):- C=..[_,L],maplist(try_get_body_vars,L).
try_get_body_vars(_).

:- multifile(varname_cache:varname_info/4).
:- dynamic(varname_cache:varname_info/4).

:- meta_predicate logicmoo_varnames:renumbervars(*,*,*).

:- meta_predicate set_varname(+,*,*).
:- meta_predicate set_varname(+,*).
% set_varname(How,B):-var(B),!.
set_varname(How,B):-var(B),writeq(set_varname(How,B)),nl,trace,trace_or_throw(var_assign_varname_vars(How,B)).
set_varname(How,N=V):-must(set_varname(How,N,V)),!.

%set_varname(How,N,V):-var(V),var(N),!,V=N.
set_varname(How,N,V):- (var(N);var(How)),trace_or_throw(var_var_set_varname(How,N,V)).
set_varname(_,_,NV):-nonvar(NV),ignore((NV='$VAR'(N),must(number(N);atom(N)))).
set_varname(How,'$VAR'(Name),V):- !, set_varname(How,Name,V).
set_varname([How],N,V):- !, set_varname(How,N,V).
set_varname([How|List],N,V):- !, set_varname(How,N,V),set_varname(List,N,V).
set_varname(How,N,V):- number(N),!,format(atom(VN),'~w',[N]),set_varname(How,VN,V).
set_varname(How,N,V):- atom(N),atom_concat('"?',LS,N),atom_concat(NN,'"',LS),fix_varcase_name(NN,VN),!,set_varname(How,VN,V).
set_varname(write_functor,N,V):- !,ignore('$VAR'(N)=V),!.
set_varname(write_attribute,N,V):-!,put_attr(V,logicmoo_varnames,N).
set_varname(Nb_setval,N,V):-nb_current('$variable_names',Vs),!,register_var(N=V,Vs,NewVS),call(Nb_setval,'$variable_names',NewVS).
set_varname(Nb_setval,N,V):-call(Nb_setval,'$variable_names',[N=V]).
set_varname(Nb_setval,N,V):- must(call(Nb_setval,N,V)).
set_varname(_How,_,_).


write_functor(N,V):-ignore('$VAR'(N)=V),!.

:-export(save_clause_vars/2).
:-module_transparent(save_clause_vars/1).
% save_clause_vars(_,[]):-!.
save_clause_vars(MHB,Vs):- maybe_record_scanned_file, current_why(Why),!,save_clause_vars(MHB,Vs,Why),!.
save_clause_vars(MHB,Vs):- trace,current_why(Why),!,save_clause_vars(MHB,Vs,Why).

% ?- clause(pui_help:prolog_help_topic(A),B,ClauseRef), prolog_clause:clause_info(ClauseRef, File, TermPos, NameOffset, Options).


:-export(save_clause_vars/3).
save_clause_vars(_, [],_):-!.
save_clause_vars(MHB,Vs,Why:_):-atom(Why),!,save_clause_vars(MHB,Vs,Why).
save_clause_vars(MHB,Vs,Why):-  ( \+ \+ (logicmoo_util_term_listing:as_clause_w_m(MHB, M, H, B, MB),save_clause_vars(M,H,MB,B,Vs,Why))).


locate_clause_ref(M,H,_MB,_B,_ClauseRef):- ( \+ (predicate_property(M:H,number_of_clauses(_)))),( \+ (predicate_property(_:H,number_of_clauses(_)))),!,fail.
locate_clause_ref(M,H,MB,B,ClauseRef):-clause_eq(M:H,MB:B,ClauseRef).
locate_clause_ref(_M,H,MB,B,ClauseRef):-clause_eq(H,MB:B,ClauseRef).
locate_clause_ref(_M,H,MB,B,ClauseRef):-clause_eq(_:H,MB:B,ClauseRef).
locate_clause_ref(M,H,_MB,B,ClauseRef):-clause_eq(M:H,B,ClauseRef).
locate_clause_ref(_M,H,_MB,B,ClauseRef):-clause_eq(H,B,ClauseRef).
locate_clause_ref(_M,H,_MB,B,ClauseRef):-clause_eq(_:H,B,ClauseRef).

clause_ref_vars(ClauseRef,Was):-prolog_clause:clause_info(ClauseRef, _File, _TermPos, _NameOffset, [variable_names(Was)]).
clause_ref_file(ClauseRef,File):-prolog_clause:clause_info(ClauseRef, File, _TermPos, _NameOffset, []).

:-export(save_to_clause_ref/3).
save_to_clause_ref(ClauseRef,Vs,Why):- ain00(names(ClauseRef,Vs)),ain00(names_why(ClauseRef,Why)),!.

:-export(save_clause_vars/6).
save_clause_vars(M,H,MB,B,Vs,Why:_):-atom(Why),!,save_clause_vars(M,H,MB,B,Vs,Why).
save_clause_vars(M,H,MB,B,Vs,Why):- fail, locate_clause_ref(M,H,MB,B,ClauseRef),clause_ref_vars(ClauseRef,Was),
   ((Was=Vs) -> fail ; save_to_clause_ref(ClauseRef,Vs,Why)),!.
save_clause_vars(_M,H,_MB,B,Vs,Why):- ain00(varname_cache:varname_info(H,B,Vs,Why)).

ain00(A):- logicmoo_util_database:clause_asserted(A),!.
ain00(A):- assertz(A).



ensure_vars_labled_r(I,O):- 
  once((((nb_current('$variable_names',Vs),Vs\==[])),
   copy_term(I:Vs,O:OVs),
    must_maplist(set_varname(write_functor),OVs))),
   (O \=@= I ;  ground(O)),!.

ensure_vars_labled_r(I,O):- 
     once((get_clause_vars_copy(I,O),unlock_vars(O))),
     (O \=@= I ;  ground(O)),!.

ensure_vars_labled_r(I,O):- copy_term_and_varnames(I,O),I\=@=O.

:-export(copy_term_and_varnames/2).
copy_term_and_varnames(Term,Named):- 
   notrace((ignore((source_variables_lv(AllS))), copy_term(Term+AllS,Named+CAllS),maplist(set_varname([write_functor,b_setarg]),CAllS))).

renumbervars(How,Term,Named):- 
   notrace((ignore((source_variables_lv(AllS))), copy_term(Term+AllS,Named+CAllS),maplist(set_varname(How),CAllS))).




source_variables_lv(AllS):-
  (prolog_load_context(variable_names,Vs1);Vs1=[]),
  (nb_current('$variable_names', Vs2);Vs2=[]),
  % notrace(catch((parent_goal('$toplevel':'$execute_goal2'(_, Vs3),_);Vs3=[]),E,(writeq(E),Vs3=[]))),
  ignore(Vs3=[]),
  append(Vs1,Vs2,Vs12),append(Vs12,Vs3,All),!,list_to_set(All,AllS),
  nb_linkval('$variable_names', AllS).


contain_numbervars(Term):- sub_term(Sub,Term),compound(Sub),Sub='$VAR'(_),!.



contains_singletons(Term):-contains_singletons(Term,N),N>0. 

contains_singletons(Term,N):-  sanity(\+contain_numbervars(Term)),
     \+ground(Term),
     copy_term_nat(Term,Nat),
     term_variables(Nat,Vs), 
     numbervars(Nat,0,_,[attvar(bind),singletons(true)]),
     count_members_eq('$VAR'('_'),Vs,N).

count_members_eq(_,[],0):-!.
count_members_eq(Find,[E|List],N):- 
   ( (E == Find) -> N is NN+1 ; NN=N),
    count_members_eq(Find,List,NN).

% = :- meta_predicate(call_not_not(0)).

call_not_not(G):- \+ \+ G.

contains_badvarnames(Term):- fail, notrace((sub_term(SubV,Term),compound(SubV),SubV='$VAR'(Sub),bad_varnamez(Sub))),!.

bad_varnamez(Sub):- atom(Sub),!,sub_string(Sub,_,_,_,'.').
bad_varnamez(Sub):- var(Sub),!.
bad_varnamez(Sub):- integer(Sub),!, (Sub < 0 ; Sub > 991000).
bad_varnamez(Sub):- number(Sub).

mpred_numbervars_with_names(Term):- term_variables(Term,Vars),mpred_name_variables(Vars),!,numbervars(Vars,91,_,[attvar(skip),singletons(true)]),!.

mpred_name_variables([]).
mpred_name_variables([Var|Vars]):-
   (var_property(Var, name(Name)) -> Var = '$VAR'(Name) ; true),
   mpred_name_variables(Vars).


%=========================================
% unnumbervars
%=========================================

b_implode_varnames(_):-!.
b_implode_varnames0([]):-!.
b_implode_varnames0([N=V|Vs]):- ignore((V='$VAR'(N);V=N)),b_implode_varnames0(Vs),!.

imploded_copyvars(C,CT):-vmust((logicmoo_util_bugger_catch:source_variables(Vs),copy_term(C-Vs,CT-VVs),b_implode_varnames(VVs))),!.



% snumbervars(Term,Functor,Start,End,List):-vmust(( vmust(var(End);number(End)),snumbervars4(Term,Start,End,[functor_name(Functor)|List]))),check_varnames(Term).


check_varnames(Vs):-var(Vs),!.
check_varnames([]):-!.
check_varnames([N=V|Vs]):-atom(N),var(V),!,check_varnames(Vs).
check_varnames(Term):- contains_badvarnames(Term),!,dumpST0,trace,stop_rtrace,trace,!,dtrace(contains_badvarnames(Term)).
check_varnames(_).

:-meta_predicate(snumbervars4(0,?,?,?)).
snumbervars4(Term,Start,End,List):-  \+ member(attvar(_),List),!,snumbervars5(Term,Start,End,[attvar(skip)|List]).
snumbervars4(Term,Start,End,List):- snumbervars5(Term,Start,End,List).

:-meta_predicate(snumbervars5(0,?,?,?)).
snumbervars5(Term,Start,End,List):-must_det_l((integer(Start),is_list(List), numbervars(Term,Start,End,List),check_varnames(Term))).

:- public ((logicmoo_varnames:attr_unify_hook/2,
          logicmoo_varnames:attr_portray_hook/2)).
:- public logicmoo_varnames:portray_attvar/1.
:- export(logicmoo_varnames:attr_unify_hook/2).
:- export(logicmoo_varnames:attr_portray_hook/2).
:- export(logicmoo_varnames:portray_attvar/1).
:- export(logicmoo_varnames:attribute_goals/3).

logicmoo_varnames:attr_unify_hook(_,_).
logicmoo_varnames:attr_portray_hook(Value, _Var) :- nonvar(Value),!,writeq('?'(Value)).
logicmoo_varnames:portray_attvar(Var) :-
	write('{<'),
        
        ((get_attr(Var,logicmoo_varnames, VarName))->true;sformat(VarName,'~q',[Var])),
	get_attrs(Var, Attr),
	catch(writeq('??'(VarName,Attr)),_,'$attvar':portray_attrs(Attr, Var)),
	write('>}').


attribute_goals(A, B, E) :-
    logicmoo_varnames:
    (   get_attr(A, logicmoo_varnames, D),
        C=B,
        C=[put_attr(A,logicmoo_varnames, D)|E]
    ).


:-export(try_save_vars/1).
try_save_vars(_):- t_l:dont_varname,!.
try_save_vars(HB):-ignore((nb_current('$variable_names',Vs),Vs\==[],save_clause_vars(HB,Vs))),!.

:-export(maybe_scan_source_files_for_varnames/0).
maybe_scan_source_files_for_varnames:- current_prolog_flag(mpred_vars, true),!,scan_source_files_for_varnames.
maybe_scan_source_files_for_varnames.

:-export(scan_source_files_for_varnames/0).
scan_source_files_for_varnames:- 
 set_prolog_flag(mpred_vars, true),
 ensure_loaded(library(make)),
 forall(make:modified_file(F),retractall(varname_cache:varname_info_file(F))),
 forall(source_file(F),read_source_file_vars(F)),!.

dcall_if_verbose(G):-!, notrace(G).
dcall_if_verbose(G):-show_call(why,G).

:- dynamic(varname_cache:varname_info_file/1).
read_source_file_vars(_):- \+ prolog_flag(mpred_vars, true),!.
read_source_file_vars(F):- \+ ((atom(F),exists_file(F))),!, forall(filematch(F,E),read_source_file_vars(E)).
read_source_file_vars(F):- clause_asserted(varname_cache:varname_info_file(F)),!.
read_source_file_vars(F):- asserta(varname_cache:varname_info_file(F,Ref)), catch(dcall_if_verbose(read_source_file_vars_1(F)),E,(dmsg(E),erase(Ref))).

save_file_source_vars(_F,end_of_file,_Vs):-!.
save_file_source_vars(_F,_T,[]):-!.
save_file_source_vars(F,T,Vs):- b_setval('$variable_names',Vs),!,w_tl(t_l:current_why_source(F),save_clause_vars(T,Vs)),!.

:- if(true).

read_source_vars(File,In):-
	repeat,
	  catch(prolog_read_source_term(In, Term, Expanded, [ variable_names(Vs), syntax_errors(error) /*, term_position(TermPos) */ ]),
		E,(nop((dmsg(E),trace)),fail)),
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
   current_prolog_flag(xref, Was),
   set_prolog_flag(xref, true),
	setup_call_cleanup(
	    prolog_open_source(File, In),
	    read_source_vars(File,In),
	    (prolog_close_source(In),
            current_prolog_flag(xref, Was))),!.

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



:-export(ensure_vars_labled/2).
ensure_vars_labled(I,O):-nonvar(O),!,must(ensure_vars_labled(I,M)),!,M=O.
ensure_vars_labled(I,I):- (t_l:dont_varname;no_vars_needed(I)),!.
% ensure_vars_labled(I,I):- !.
% ensure_vars_labled(I,OO):- acyclic_term(O),term_variables(I,Vs),all_different_vals(Vs),ensure_vars_labled_r(I,O),vmust(acyclic_term(O)),!,OO=O.
ensure_vars_labled(I,OO):- acyclic_term(O),term_variables(I,Vs),ensure_vars_labled_r(I,O),\+ \+ ((I=O,vmust(all_different_vals(Vs)))),!,vmust(acyclic_term(O)),!,OO=O.
ensure_vars_labled(I,OO):- acyclic_term(O),ensure_vars_labled_r(I,O),vmust(acyclic_term(O)),!,OO=O.
ensure_vars_labled(I,I).

% :- prolog_load_context(module,M),source_location(S,_),forall(source_file(H,S),(functor(H,F,A),export(F/A),'$set_predicate_attribute'(M:H, hide_childs, 1),module_transparent(F/A))).


:- multifile(user:portray/1).
user:portray(A) :-  \+ tracing, 
  catch(notrace(((compound(A);var(A)), current_prolog_flag(mpred_vars, true), set_prolog_flag(mpred_vars, false),
    call_cleanup((((user:portray(A) -> ! ; print_numbervars_maybe(A)))),set_prolog_flag(mpred_vars, true)))),E,(writeq(E),nl,fail)).

print_numbervars(H):- tlbugger:no_slow_io,!, writeq(H),!.
print_numbervars(H):- must( \+ \+ ((on_x_log_fail(print_numbervars_maybe(H));on_x_log_fail(print_numbervars_1(H))))),!.


print_numbervars_maybe(H):-(compound(H);var(H)) , \+ \+ ((copy_term(H,HC),get_clause_vars(H), HC\=@=H, print_numbervars_1(H))),!.

print_numbervars_1(H):- loop_check(print_numbervars_2(H),format('~N~q.~n',[H])).

% print_numbervars_2(H):- trace,lmconf:portray_one_line_hook(H),!.
print_numbervars_2(H):- current_output(S),prolog_listing:portray_clause(S,H,[portrayed(true),singletons(false)]),!.
print_numbervars_2(H):- write_term(H,[portrayed(false)]),nl,!.

 
term_expansion_save_vars(HB):- \+ ground(HB),  \+ t_l:dont_varname_te,\+ t_l:dont_varname, % \+ current_prolog_flag(xref, true), 
   current_predicate(logicmoo_util_varnames_file/0), current_prolog_flag(mpred_vars,true),  
   source_context_module(M),init_varname_stores(M),logicmoo_util_with_assertions:w_tl([t_l:dont_varname_te],logicmoo_varnames:try_save_vars(M:HB)),!,fail.

maybe_record_scanned_file:-ignore((  fail,source_location(F,_), \+ varname_cache:varname_info_file(F), asserta(varname_cache:varname_info_file(F)))).

init_varname_stores(_):- !.
init_varname_stores(M):- 
   M:use_module(logicmoo_util_with_assertions),
   M:use_module(logicmoo_util_varnames).


% :- initialization(maybe_scan_source_files_for_varnames).

prolog:make_hook(before, Files):-forall(member(File,Files),retractall(varname_cache:varname_info_file(File))).
prolog:make_hook(after, Files):-forall(member(File,Files),show_call(why,ain00(varname_cache:varname_info_file(File)))).

user:term_expansion(HB,_):- current_prolog_flag(mpred_vars,true),term_expansion_save_vars(HB),fail.


logicmoo_util_varnames_file.

% :- set_prolog_flag(mpred_vars, true).


