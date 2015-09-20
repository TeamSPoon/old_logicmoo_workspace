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

get_clause_vars(MHB):- (ground(MHB); \+ compound(MHB)),!.
get_clause_vars(MHB):- as_clause_no_m( MHB,  H, B), get_clause_vars(H,B),!.



get_clause_vars(H,B):- user:saved_varname_info(H,B,_),!.
get_clause_vars(H,B):- try_get_head_vars(H),try_get_head_vars(B),!.
get_clause_vars(_,_):-!.
get_clause_vars(H,B):- dmsg(missed_clause_vars(H,B)).

ignoreq(X,Y):-var(Y),ignore(X=Y).
ignoreq(X,'$VAR'(Y)):-!,ignore(X='$VAR'(Y)).
ignoreq(X, Y):-atom(Y),atom_concat('"?',LS,Y),atom_concat(N,'"',LS),fix_varcase_name(N,VN),!,ignore(X='$VAR'(VN)).
ignoreq(X,Y):-ignore(X=Y).

fix_varcase_name(N,VN):-atom_subst(N,'-','_',O),atom_subst(O,'?','_',VN).

try_get_head_vars(H):- ground(H),!.
try_get_head_vars(H):- (\+ compound(H)) ,!.
try_get_head_vars(H):- once((functor(H,_,N),arg(N,H,List),member(vars(Vs),List))),is_list(Vs),term_variables(H,VL),maplist(ignoreq,VL,Vs).
try_get_head_vars((A,B)):-!,try_get_head_vars(A),try_get_head_vars(B).
try_get_head_vars((A;B)):-!,try_get_head_vars(A),try_get_head_vars(B).
try_get_head_vars(H):- term_variables(H,HVs),user:saved_varname_info(H,_,_),maplist(is_ftVar,HVs),!.
try_get_head_vars(_).

:-multifile(user:saved_varname_info/3).
:-dynamic(user:saved_varname_info/3).
assign_varname(O=I):-assign_varname_l(I,'$VAR'(O)),!.

assign_varname_vars(N='$VAR'(N)).


current_source_location(F):- (current_source_location0(W), (W= (F:foo) -> true;F=W)),!.
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
save_clause_vars(MHB,Vs,Why):-
 ( \+ \+
    ((
    once(maplist(assign_varname_vars,Vs)),
    as_clause_no_m(MHB,  H, B),
%     once(map list(assign_varname,Vs)),
%    numberv ars(HB,555,_,[attvar(skip)]), % singletons(true) 
    assert_if_new_kv(user:saved_varname_info(H,B,Why))))),!.


assert_if_new_kv(A):- clause_asserted(A),!.
assert_if_new_kv(A):- asserta(A).

ensure_vars_labled(I,I):-!.
ensure_vars_labled(I,I):- ground(I),!.
ensure_vars_labled(I,I):- ( \+ compound(I)),!.
ensure_vars_labled(I,OO):- must(ensure_vars_labled_r(I,O)),!,OO=O.
ensure_vars_labled(I,I).

ensure_vars_labled_r(I,O):- 
  ((nb_current('$variable_names',Vs),Vs\==[])),
   copy_term(I:Vs,O:OVs),
    must_maplist(assign_varname,OVs),
   (O \=@= I ;  ground(O)),!.

ensure_vars_labled_r(I,O):- 
     term_variables(I,Vs),
     copy_term(I,O),
     copy_term(I:Vs,O:OVs),
     get_clause_vars(O),
     must_maplist(assign_varname_l,Vs,OVs),
     (O \=@= I ;  ground(O)),!.

ensure_vars_labled_r(I,O):- unnumbervars_and_save_r(I,UI),I\=@=UI,O=I.



unnumbervars_and_save_r(I,O):-unnumbervars(I,UI),term_variables(UI,UIV),copy_term(UI-UIV,UIC-UIVC),
    I=UIC,
    must_maplist(assign_varname_l,UIV,UIVC),!,O=UI.

assign_varname_l(I,O):-var(I),var(O),!,I=O.
assign_varname_l(I,O):-is_ftVar(O),var(I),!, put_varname(I,O).
assign_varname_l(I,O):-trace,!,I=O.

contains_singletons(Term):- not(ground(Term)),call_not_not(((term_variables(Term,Vs),
   numbervars(Term,0,_,[attvar(bind),singletons(true)]),member('$VAR'('_'),Vs)))).

:-meta_predicate(call_not_not(0)).

call_not_not(G):- \+ \+ G.

contains_badvarnames(Term):- notrace((sub_term(SubV,Term),compound(SubV),SubV='$VAR'(Sub),nonvar(Sub),bad_varnamez(Sub))),!.
bad_varnamez(Sub):- integer(Sub),!, (Sub < 0 ; Sub > 1000).
bad_varnamez(Sub):- number(Sub).
bad_varnamez(Sub):- atom(Sub),!,atom_contains(Sub,'.').




  
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

name_vars(Term,Named):- ignore((source_variables_l(AllS))), copy_term(Term+AllS,Named+CAllS),maplist(must,CAllS).

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

imploded_copyvars(C,CT):-must((source_variables(Vs),copy_term(C-Vs,CT-VVs),b_implode_varnames(VVs))),!.


:-swi_export(unnumbervars/2).
unnumbervars(X,YY):- must(unnumbervars0(X,Y)),!,must(Y=YY).



:-swi_export(unnumbervars_saved/2).
unnumbervars_saved(X,YY):-
   unnumbervars1(X,[],Y,Vs),
    (Vs==[]->must(X=YY);
    ( % writeq((unnumbervars1(X,Y,Vs))),nl,
     save_clause_vars(Y,Vs),must(Y=YY))).

% todo this slows the system!
unnumbervars0(X,clause(UH,UB,Ref)):- sanity(nonvar(X)),
  X = clause(H,B,Ref),!,
  must(unnumbervars0((H:-B),(UH:-UB))),!.

unnumbervars0(X,YY):-
   must_det_l((with_output_to(string(A),write_term(X,[numbervars(true),character_escapes(true),ignore_ops(true),quoted(true)])),
   atom_to_term(A,Y,_NewVars),!,must(YY=Y))),check_varnames(YY).

unnumbervars1(X,Vs,X,Vs):- ( \+ compound(X)),!.
unnumbervars1('$VAR'(X),VsIn,Y,Vs):-!, (memberchk(X=Y,VsIn)->Vs=VsIn;Vs=[X=Y|VsIn]).
unnumbervars1([X|XM],VsIn,[Y|YM],Vs):-!,
  unnumbervars1(X,VsIn,Y,VsM),
  unnumbervars1(XM,VsM,YM,Vs).
unnumbervars1(XXM,VsIn,YYM,Vs):-
  XXM=..[F,X|XM],
  unnumbervars1(X,VsIn,Y,VsM),
  unnumbervars1(XM,VsM,YM,Vs),
  YYM=..[F,Y|YM].

unnumbervars_and_save(X,YO):-
 term_variables(X,TV),
 must((source_variables(Vs),
   with_output_to(string(A),write_term(X,[numbervars(true),variable_names(Vs),character_escapes(true),ignore_ops(true),quoted(true)])))),
   must(atom_to_term(A,Y,NewVars)),
   (NewVars==[]-> YO=X ; (length(TV,TVL),length(NewVars,NewVarsL),(NewVarsL==TVL-> (YO=X) ; (trace,add_newvars(NewVars),Y=X)))).

/*


unnumbervars_and_copy(X,YO):-
 term_variables(X,TV),
 must((source_variables(Vs),
   with_output_to(string(A),write_term(X,[numbervars(true),variable_names(Vs),character_escapes(true),ignore_ops(true),quoted(true)])))),
   must(atom_to_term(A,Y,NewVars)),
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
renumbervars(X,Z):-unnumbervars(X,Y),safe_numbervars(Y,Z),!.
renumbervars(Y,Z):-safe_numbervars(Y,Z),!.


% register_var(?, ?, ?)
%
%   During copying one has to remeber copies of variables which can be used further during copying.
%   Therefore the register of variable copies is maintained.
%
register_var(N=V,IN,OUT):- (var(N)->true;register_var(N,IN,V,OUT)),!.
register_var(N,T,V,OUTO):-register_var_0(N,T,V,OUT),must(OUT=OUTO),!.
register_var(N,T,V,O):-append(T,[N=V],O),!.

register_var_0(N,T,V,OUT):- must(nonvar(N)),
   ((name_to_var(N,T,VOther)-> must((OUT=T,samify(V,VOther)));
     (once(nb_getval('$variable_names',Before);Before=[]),
      (name_to_var(N,Before,VOther)  -> must((samify(V,VOther),OUT= [N=V|T]));
         (var_to_name(V,T,_OtherName)                  -> OUT= [N=V|T];
           (var_to_name(V,Before,_OtherName)              -> OUT= [N=V|T];fail)))))),!.


register_var_0(N,T,V,OUT):- var(N),
   (var_to_name(V,T,N)                -> OUT=T;
     (once(nb_getval('$variable_names',Before);Before=[]),
          (var_to_name(V,Before,N)   -> OUT= [N=V|T];
               OUT= [N=V|T]))),!.





% different variables (now merged)
samify(V,V0):-must(V=@=V0),V=V0. 

var_to_name(V,[N=V0|T],N):-
    V==V0 -> true ;          % same variables
    var_to_name(V,T,N).

name_to_var(N,T,V):- var(N),!,var_to_name(N,T,V).
name_to_var(N,[N0=V0|T],V):- 
   N0==N -> samify(V,V0) ; name_to_var(N,T,V).



% ===================================================================
% Safely number vars
% ===================================================================
bugger_numbervars_with_names(Term):-
   term_variables(Term,Vars),bugger_name_variables(Vars),!,numbervars(Vars,91,_,[attvar(skip),singletons(true)]),!,

bugger_name_variables([]).
bugger_name_variables([Var|Vars]):-
   (var_property(Var, name(Name)) -> Var = '$VAR'(Name) ; true),
   bugger_name_variables(Vars).


:- swi_export(snumbervars/1).
snumbervars(Term):-snumbervars(Term,0,_).

:- swi_export(snumbervars/3).
snumbervars(Term,Start,End):- integer(Start),var(End),!,snumbervars4(Term,Start,End,[]).
snumbervars(Term,Start,List):- integer(Start),is_list(List),!,snumbervars4(Term,Start,_,List).
snumbervars(Term,Functor,Start):- integer(Start),atom(Functor),!,snumbervars4(Term,Start,_End,[functor_name(Functor)]).
snumbervars(Term,Functor,List):- is_list(List),atom(Functor),!,snumbervars4(Term,0,_End,[functor_name(Functor)]).


:- swi_export(snumbervars/4).
snumbervars(Term,Start,End,List):-snumbervars4(Term,Start,End,List).

% snumbervars(Term,Functor,Start,End,List):-must(( must(var(End);number(End)),numbervars(Term,Start,End,[functor_name(Functor)|List]))),check_varnames(Term).


check_varnames(Vs):-var(Vs),!.
check_varnames([]):-!.
check_varnames([N=V|Vs]):-atom(N),var(V),check_varnames(Vs).
check_varnames(Term):- contains_badvarnames(Term),!,dumpST0,trace,stop_rtrace,trace,!,dtrace(contains_badvarnames(Term)).
check_varnames(_).

snumbervars4(Term,Start,End,List):-must_det_l((integer(Start),is_list(List), numbervars(Term,Start,End,List),check_varnames(Term))).


put_varname(Var,'$VAR'(Name)):-atom(Name),!, put_attr(Var,varname,Name).
put_varname(Var,Name):-  put_attr(Var,varname,Name).
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
user:term_expansion(HB,_):- try_save_vars(HB),fail.

read_source_files:- 
 forall(source_file(F),catch(read_source_file_vars(F),_,true)).

read_source_file_vars(F):- clause_asserted(user:saved_varname_info(F,F,F)),!.
read_source_file_vars(F):- assert(user:saved_varname_info(F,F,F)),
   open(F,read,S), 
   call_cleanup(read_vars_until_oes(F,S),close(S)),!.

read_vars_until_oes(_,S):-at_end_of_stream(S),!.
read_vars_until_oes(F,S):-
  repeat,
   read_term(S,T,[variable_names(Vs)]),
   (T==end_of_file->!;
    ((Vs\==[],must(line_count(S,C)),    
    b_setval('$variable_names',Vs),
    with_assertions(thlocal:current_why_source(F:C),
    (save_clause_vars(T,Vs),fail))))).

    
:- initialization(read_source_files).
:- read_source_files.
