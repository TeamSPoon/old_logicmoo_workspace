/** <module> Logicmoo Debug Tools
% ===================================================================
% File 'logicmoo_util_bugger.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_util_bugger.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% ===================================================================
*/


as_clause_no_m( MHB,  H, B):- strip_module(MHB,_M,HB), as_clause( HB,  MH, MB),strip_module(MH,_M2H,H),strip_module(MB,_M2B,B).

:-swi_export(is_ftCompound/1).
is_ftCompound(C):-compound(C),C\='$VAR'(_).

:-swi_export(is_ftVar/1).
is_ftVar(V):-var(V),!.
is_ftVar('$VAR'(_)).

:-swi_export(is_ftNonvar/1).
is_ftNonvar(V):- \+ is_ftVar(V).

show_call_when(P,In,Out):- pmust(call(P,In,Out)),ignore((In\=@=Out,dmsg((show_call_when(P) :- (In,Out))))).

lock_vars(Var):-var(Var),!,when(nonvar(Var),is_ftVar(Var)).
lock_vars(Var):-atomic(Var),!.
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

get_clause_vars(MHB):- vmust((get_clause_vars_copy(MHB,WVARS),!,vmust(MHB=WVARS),unlock_vars(MHB),check_varnames(MHB))).

get_clause_vars_copy(HB,HB):- ground(HB),!.
get_clause_vars_copy(HH,HH):- sub_term(S,HH),compound(S),S='$VAR'(_),!. % already labled
get_clause_vars_copy(H0,MHB):- name_vars(H0,MHB),lock_vars(MHB),as_clause_no_m( MHB,  H, B),
    get_clause_vars_hb_int(H,B),!.


get_clause_vars_hb_int(H,B):- user:varname_info(H,B,Vs,_),must_maplist(assign_name_equal_var,Vs),!.
get_clause_vars_hb_int(H,B):- call_return_tf(try_get_body_vars(B),_TF1),call_return_tf(try_get_head_vars(H),_TF2),!.


fix_varcase_name(N,VN):-atom_subst(N,'-','_',O),atom_subst(O,'?','_',VN).

no_vars_needed(H):- ( ground(H) ; \+ compound(H)) ,!.
try_get_inner_vars(H):- once((functor(H,_,N),arg(N,H,List),member(vars(Vs),List))),is_list(Vs),term_variables(H,VL),must_maplist(assign_name_equal_var,Vs,VL).

call_return_tf(Call,TF):- ((Call-> TF = t ; TF = nil)).

try_get_head_vars(H):- no_vars_needed(H),!.
try_get_head_vars(H):- user:varname_info(H,_,Vs,_),maplist(assign_name_equal_var,Vs),!.
try_get_head_vars(H):- try_get_inner_vars(H),!.
try_get_head_vars(H):- user:varname_info(_,H,Vs,_),maplist(assign_name_equal_var,Vs),!.
try_get_body_vars(_).

try_get_body_vars(H):- no_vars_needed(H),!.
try_get_body_vars(H):- user:varname_info(_,H,Vs,_),maplist(assign_name_equal_var,Vs),!.
try_get_body_vars(H):- try_get_inner_vars(H).
try_get_body_vars(H):- user:varname_info(H,_,Vs,_),maplist(assign_name_equal_var,Vs),!.
try_get_body_vars((A,B)):-!,try_get_head_vars(A),try_get_head_vars(B).
try_get_body_vars((A;B)):-!,try_get_head_vars(A),try_get_head_vars(B).
try_get_body_vars(_).

vmust(G):-must(G).
:-multifile(user:varname_info/4).
:-dynamic(user:varname_info/4).

% assign_name_equal_var(B):-var(B),!.
assign_name_equal_var(B):-var(B),writeq(assign_name_equal_var(B)),nl,trace_or_throw(var_assign_varname_vars(B)).
assign_name_equal_var(N=V):-assign_name_equal_var(N,V),!.
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

ignoreN(N,V):-var(V),var(N),!,V=N.
ignoreN('$VAR'(Name),Var):-atom(Name),!,ignoreN(Name,Var).
ignoreN(Name,Var):- attvar(Var),put_attr(Var,varname,Name),!.
ignoreN(N,'$VAR'(N)):-atom(N),!.
ignoreN(N,V):-ignore(V='$VAR'(N)),!.
ignoreN(Name,Var):- vmust(Var = '$VAR'(Name)).



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


:- thread_local(thlocal:current_local_why/2).
:- thread_local(thlocal:current_why_source/1).

current_why(F):- thlocal:current_why_source(F),!.
current_why(F):- thlocal:current_local_why(F,_),!.
current_why(F):- current_source_location(F),!.
current_why(unk).

save_clause_vars(_,[]):-!.
save_clause_vars(MHB,Vs):- current_why(Why),!,save_clause_vars(MHB,Vs,Why).
save_clause_vars(MHB,Vs):- trace,current_why(Why),!,save_clause_vars(MHB,Vs,Why).

save_clause_vars(_, [],_):-!.
save_clause_vars(MHB,Vs,Why):- ( \+ \+ ((as_clause_no_m(MHB,  H, B), assert_if_new_kv(user:varname_info(H,B,Vs,Why))))).

assert_if_new_kv(A):- clause_asserted(A),!.
assert_if_new_kv(A):- asserta(A).



ensure_vars_labled(I,I):- no_vars_needed(I),!.
% ensure_vars_labled(I,I):- !.
ensure_vars_labled(I,OO):- acyclic_term(O),ensure_vars_labled_r(I,O),vmust(acyclic_term(O)),!,OO=O.
ensure_vars_labled(I,I).

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
   numbervars(Term,0,_,[attvar(bind),singletons(true)]),member('$VAR'('_'),Vs)))).

:-meta_predicate(call_not_not(0)).

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
  get_gtime(G),numbervars(EE,G,End,[attvar(skip),functor_name('$VAR'),singletons(true)]),
  term_variables(EE,AttVars),
  numbervars(EE,End,_,[attvar(skip),functor_name('$VAR'),singletons(true)]),
  forall(member(V,AttVars),(copy_term(V,VC,Gs),V='$VAR'(VC=Gs))),check_varnames(EE).

name_vars(Term,Named):- ignore((source_variables_l(AllS))), copy_term(Term+AllS,Named+CAllS),maplist(assign_name_equal_var,CAllS).

get_gtime(GG):- get_time(T),convert_time(T,_A,_B,_C,_D,_E,_F,G),GG is (floor(G) rem 500).

safe_numbervars(EE):-get_gtime(G),numbervars(EE,G,_End,[attvar(skip),functor_name('$VAR'),singletons(true)]),check_varnames(EE).



%=========================================
% unnumbervars
%=========================================

% source_module(M):-!,M=u.
source_module(M):-nonvar(M),source_module(M0),!,M0=M.
source_module(M):-loading_module(M),!.
source_module(M):-'$set_source_module'(M,   M),!.

:-thread_local(thlocal:last_source_file/1).
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


:-swi_export(unnumbervars/2).
unnumbervars(X,YY):- lock_vars(X,[],Y,_Vs),!, vmust(YY=Y).
% TODO compare the speed
% unnumbervars(X,YY):- vmust(unnumbervars0(X,Y)),!,vmust(Y=YY).



:-swi_export(unnumbervars_and_save/2).
unnumbervars_and_save(X,YY):-
   lock_vars(X,[],Y,Vs),
    (Vs==[]->vmust(X=YY);
    ( % writeq((lock_vars(X,Y,Vs))),nl,
     save_clause_vars(Y,Vs),vmust(Y=YY))).

/*
% todo this slows the system!
unnumbervars0(X,clause(UH,UB,Ref)):- sanity(nonvar(X)),
  X = clause(H,B,Ref),!,
  vmust(unnumbervars0((H:-B),(UH:-UB))),!.

unnumbervars0(X,YY):-lock_vars(X,YY,_Vs).
*/
/*
lock_vars(X,YY):-
   must_det_l((with_output_to(string(A),write_term(X,[numbervars(true),character_escapes(true),ignore_ops(true),quoted(true)])),
   atom_to_term(A,Y,_NewVars),!,vmust(YY=Y))),check_varnames(YY).
lock_vars(X,YY,Vs):-!,lock_vars(X,[],YY,Vs).
*/

lock_vars(X,Vs,X,Vs):- ( \+ compound(X)),!.
lock_vars('$VAR'(X),IVs,Y,Vs):-!, (memberchk(X=Y,IVs)->Vs=IVs;Vs=[X=Y|IVs]).
lock_vars([X|XM],IVs,[Y|YM],Vs):-!,
  lock_vars(X,IVs,Y,VsM),
  lock_vars(XM,VsM,YM,Vs).
lock_vars(XXM,IVs,YYM,Vs):-
  XXM=..[F,X|XM],
  lock_vars(X,IVs,Y,VsM),
  lock_vars(XM,VsM,YM,Vs),
  YYM=..[F,Y|YM].

/*
lock_vars(X,YY,Vs):-
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
   term_variables(Term,Vars),bugger_name_variables(Vars),!,numbervars(Vars,91,_,[attvar(skip),singletons(true)]),!,

bugger_name_variables([]).
bugger_name_variables([Var|Vars]):-
   (var_property(Var, name(Name)) -> Var = '$VAR'(Name) ; true),
   bugger_name_variables(Vars).

*/
:- swi_export(snumbervars/1).
snumbervars(Term):-snumbervars(Term,0,_).

:- swi_export(snumbervars/3).
snumbervars(Term,Start,End):- integer(Start),var(End),!,snumbervars4(Term,Start,End,[]).
snumbervars(Term,Start,List):- integer(Start),is_list(List),!,snumbervars4(Term,Start,_,List).
snumbervars(Term,Functor,Start):- integer(Start),atom(Functor),!,snumbervars4(Term,Start,_End,[functor_name(Functor)]).
snumbervars(Term,Functor,List):- is_list(List),atom(Functor),!,snumbervars4(Term,0,_End,[functor_name(Functor)]).


:- swi_export(snumbervars/4).
snumbervars(Term,Start,End,List):-snumbervars4(Term,Start,End,List).

% snumbervars(Term,Functor,Start,End,List):-vmust(( vmust(var(End);number(End)),numbervars(Term,Start,End,[functor_name(Functor)|List]))),check_varnames(Term).


check_varnames(Vs):-var(Vs),!.
check_varnames([]):-!.
check_varnames([N=V|Vs]):-atom(N),var(V),!,check_varnames(Vs).
check_varnames(Term):- contains_badvarnames(Term),!,dumpST0,trace,stop_rtrace,trace,!,dtrace(contains_badvarnames(Term)).
check_varnames(_).

snumbervars4(Term,Start,End,List):-must_det_l((integer(Start),is_list(List), numbervars(Term,Start,End,List),check_varnames(Term))).


varname:attr_unify_hook(_,_).
varname:attr_portray_hook(Value, _Var) :- nonvar(Value),!,writeq('?'(Value)).
:- public
	varname:portray_attvar/1.
'varname':portray_attvar(Var) :-
	write('{<'),
	get_attrs(Var, Attr),
	catch(writeq('??'(Attr)),_,'$attvar':portray_attrs(Attr, Var)),
	write('>}').


try_save_vars(HB):-ignore((nb_current('$variable_names',Vs),Vs\==[],save_clause_vars(HB,Vs))),!.
user:term_expansion(HB,_):- once(try_save_vars(HB)),fail.

read_source_files:- 
 forall(source_file(F),catch(read_source_file_vars(F),_,true)).

read_source_file_vars(F):- clause_asserted(user:varname_info_file(F)),!.
read_source_file_vars(F):- assert(user:varname_info_file(F)),
   open(F,read,S), 
   call_cleanup(read_vars_until_oes(F,S),close(S)),!.

read_vars_until_oes(_,S):-at_end_of_stream(S),!.
read_vars_until_oes(F,S):-
  repeat,
   read_term(S,T,[variable_names(Vs)]),
   (T==end_of_file->!;
    ((Vs\==[],vmust(line_count(S,C)),    
    b_setval('$variable_names',Vs),
    with_assertions(thlocal:current_why_source(F:C),
    (save_clause_vars(T,Vs),fail))))).

    
:- initialization(read_source_files).
:- read_source_files.
