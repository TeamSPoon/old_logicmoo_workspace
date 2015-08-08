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


contains_singletons(Term):- not(ground(Term)),not(not((term_variables(Term,Vs),
   numbervars(Term,0,_,[attvar(bind),singletons(true)]),member('$VAR'('_'),Vs)))).

% ========================================================================================
% safe_numbervars/1 (just simpler safe_numbervars.. will use a random start point so if a partially numbered getPrologVars wont get dup getPrologVars)
% Each prolog has a specific way it could unnumber the result of a safe_numbervars
% ========================================================================================
% 7676767
safe_numbervars(E,EE):-duplicate_term(E,EE),
  get_gtime(G),numbervars(EE,G,End,[attvar(skip),functor_name('$VAR'),singletons(true)]),
  term_variables(EE,AttVars),
  numbervars(EE,End,_,[attvar(skip),functor_name('$VAR'),singletons(true)]),
  forall(member(V,AttVars),(copy_term(V,VC,Gs),V='$VAR'(VC=Gs))).

name_vars(Term,Named):- ignore((source_variables_l(AllS))), copy_term(Term+AllS,Named+CAllS),maplist(must,CAllS).

get_gtime(G):- get_time(T),convert_time(T,_A,_B,_C,_D,_E,_F,G).

safe_numbervars(EE):-get_gtime(G),numbervars(EE,G,_End,[attvar(skip),functor_name('$VAR'),singletons(true)]).

%safe_numbervars(Copy,X,Z):-numbervars(Copy,X,Z,[attvar(skip)]).
%safe_numbervars(Copy,_,X,Z):-numbervars(Copy,X,Z,[attvar(skip)]).

check_varnames(Vs):-var(Vs),!.
check_varnames([]):-!.
check_varnames([N=V|Vs]):-atom(N),var(V),check_varnames(Vs).


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
source_file0(F):-findall(E,catch((stream_property( S,mode(read)),stream_property(S,file_name(E)),exists_file(E),line_count(S,C),C>0),_,fail),L),last(L,F).


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
/*
*/

:-export(unnumbervars/2).


unnumbervars(X,YY):- unnumbervars0(X,Y),!,must(Y=YY).

/*
unnumbervars(In,Out):- contains_term('$VAR'(I),In),atomic(I),!,must(( term_string(In,String,[numbervars(true)]),term_string(Out,String))),!.
unnumbervars(In,Out):- copy_term(In,Out),!.
unnumbervars(In,Out):- =(In,Out),!.
unnumbervars(X,YY):- fail,
 source_variables(Vs),copy_term(X-Vs,CXVs-CVs),b_implode_varnames(CVs),
     unnumbervars1(CXVs,YY),!.


unnumbervars(X,YY):-unnumbervars0(X,Y),!,must(Y=YY).
*/


unnumbervars0(X,YY):- 
   must_det_l((with_output_to(string(A),write_term(X,[numbervars(true),character_escapes(true),ignore_ops(true),quoted(true)])),
   atom_to_term(A,Y,_NewVars),!,must(YY=Y))).

/*

unnumbervars_and_save(X,YO):-
 term_variables(X,TV),
 must((source_variables(Vs),
   with_output_to(string(A),write_term(X,[numbervars(true),variable_names(Vs),character_escapes(true),ignore_ops(true),quoted(true)])))),
   must(atom_to_term(A,Y,NewVars)),
   (NewVars==[]-> YO=X ; (length(TV,TVL),length(NewVars,NewVarsL),(NewVarsL==TVL-> (YO=X) ; (trace,add_newvars(NewVars),Y=X)))).

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
register_var(N=V,IN,OUT):-register_var(N,IN,V,OUT).

register_var(N,T,V,OUT):- must(nonvar(N)),
   ((name_to_var(N,T,VOther)-> must((OUT=T,samify(V,VOther)));
     (once(nb_getval('$variable_names',Before);Before=[]),
      (name_to_var(N,Before,VOther)  -> must((samify(V,VOther),OUT= [N=V|T]));
         (var_to_name(V,T,_OtherName)                  -> OUT= [N=V|T];
           (var_to_name(V,Before,_OtherName)              -> OUT= [N=V|T];fail)))))).


register_var(N,T,V,OUT):- var(N),
   (var_to_name(V,T,N)                -> OUT=T;
     (once(nb_getval('$variable_names',Before);Before=[]),
          (var_to_name(V,Before,N)   -> OUT= [N=V|T];
               OUT= [N=V|T]))),!.


register_var(N,T,V,[N=V|T]).

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
   term_variables(Term,Vars),bugger_name_variables(Vars),!,numbervars(Vars,91,_,[attvar(skip),singletons(true)]),!.

bugger_name_variables([]).
bugger_name_variables([Var|Vars]):-
   (var_property(Var, name(Name)) -> Var = '$VAR'(Name) ; true),
   bugger_name_variables(Vars).


numbervars_impl(Term,Start,List):- integer(Start),!,numbervars_impl(Term,'$VAR',Start,List).
numbervars_impl(Term,Functor,Start):- integer(Start),atom(Functor),!,numbervars_impl(Term,Functor,Start,_End).
numbervars_impl(Term,Functor,List):- is_list(List),atom(Functor),!, term_variables(Term,Vars),bugger_name_variables(Vars),!,must(( numbervars(Term,0,_End,[functor_name(Functor)|List]))).

numbervars_impl(Term,Start,End,List):-number(Start),is_list(List),!,must(( numbervars(Term,Start,End,List) )).
numbervars_impl(Term,Functor,Start,List):- is_list(List),sanity(integer(Start)),!,must(( numbervars(Term,Start,_End,[functor_name(Functor)|List]))).
numbervars_impl(Term,Functor,Start,End):- !,debugOnError((numbervars(Term,Start,End,[attvar(skip),functor_name(Functor),singletons(true)]))).
numbervars_impl(Term,Functor,Start,End):- sanity((must(var(End);integer(End)),numbervars(Term,Start,End,[attvar(skip),functor_name(Functor),singletons(true)]))).

numbervars_impl(Term,Functor,Start,End,List):-must(( must(var(End);number(End)),numbervars(Term,Start,End,[functor_name(Functor)|List]))).


:- export(snumbervars/3).
snumbervars(Term,Start,End):- integer(Start),!,numbervars_impl(Term,'$VAR',Start,End).
:- export(snumbervars/4).
snumbervars(Term,Functor,Start,List):-numbervars_impl(Term,Functor,Start,List).
:- export(snumbervars/1).
snumbervars(Term):-numbervars_impl(Term,0,_).

