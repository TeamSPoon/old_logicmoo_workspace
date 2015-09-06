:- module(logicmoo_util_structs,[
  prop_get/1,prop_get/3,prop_set/1,prop_set/3,prop_merge/3,
  prop_set_nvlist/2,
  decl_argtypes/1,
  decl_struct/1,struct_decl/1,
  if_changed/3,
  ain/1,pfc_ain/1,prop_get_nvlist/2,
  term_to_ord_term/2,
  new_struct/2,
  ensure_struct/2,ensure_struct/3,
  ensure_instance/2,ensure_instance/3]).

:- use_module(library(record)).

:- record point(x:integer=0, y:integer=0).

     /*
        default_point(Point),
        point_x(Point, X),
        set_x_of_point(10, Point, Point1),

        make_point([y(20)], YPoint),
   */
:- expects_dialect(sicstus).


pfc_ain(X):-if_defined(pfc_add(X),assert_if_new(X)).
ain(X):-if_defined(pfc_add(X),assert_if_new(X)).

:-swi_export(user:struct_decl/1).
:-multifile(user:struct_decl/1).
:-dynamic(user:struct_decl/1).

:-swi_export(user:struct_names/2).
:-multifile(user:struct_names/2).
:-dynamic(user:struct_names/2).

:-swi_export(user:struct_datatype/2).
:-multifile(user:struct_datatype/2).
:-dynamic(user:struct_datatype/2).

:-swi_export(user:struct_prototype/2).
:-multifile(user:struct_prototype/2).
:-dynamic(user:struct_prototype/2).


:-swi_export(user:member_datatype/3).
:-multifile(user:member_datatype/3).
:-dynamic(user:member_datatype/3).

:-swi_export(user:member_loc/3).
:-multifile(user:member_loc/3).
:-dynamic(user:member_loc/3).

:-swi_export(user:member_init/3).
:-multifile(user:member_init/3).
:-dynamic(user:member_init/3).

:- struct_datatype(_,_) -> true; true.

record_onto_var(AttribName,AV,Value):-
 ignore((
 atom(Value),var(AV), 
 ((get_attr(AV,lp,Dict),nonvar(Dict))-> true; (new_struct(map,Dict),put_attr(AV,lp,Dict))),
 (prop_get(AttribName,Dict,_)->true;prop_set(AttribName,Dict,AV)))).


attr_unify_hook(_,_).
attr_portray_hook(Value, _Var) :- nonvar(Value),!,write((Value)).


record_var_names(V):- \+ compound(V),!.
record_var_names(N=V):-!,record_var_names(V,N).
record_var_names(List):-is_list(List),!,maplist(record_var_names,List).
record_var_names(Comp):-functor(Comp,_,A),arg(A,Comp,E),!,record_var_names(E).

record_var_names(ATTVAR,Value):-record_onto_var(varname,ATTVAR,Value).


record_var_type(ATTVAR,Type):-record_onto_var(type,ATTVAR,Type).


/*

prop_get(Name,mutable(Dict),Value):-!,nonvar(Dict),prop_get(Name,Dict,Value).


?- 
  prop_get(uses_domain, problem('blocks-3-0',blocks,[],[block([a,b,c])],[handempty,clear(a),clear(b),clear(c),ontable(a),ontable(b),ontable(c)],
   [on(b,a),on(c,b)],[],[],[],extraprops{constraints:[],goal:[on(b,a),on(c,b)],init:[handempty,clear(a),clear(b),clear(c),ontable(a),ontable(b),ontable(c)],
     length:[],metric:[],object:[block([a,b,c])],
      problem_filename:'/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/planner/orig_pddl_parser/test/blocks/blocks-03-0.pddl',
      problem_name:'blocks-3-0',requires:[],uses_domain:blocks}),X).



  ?-

   Y = problem('blocks-3-0',blocks,[],[block([a,b,c])],[handempty,clear(a),clear(b),clear(c),ontable(a),ontable(b),ontable(c)],[on(b,a),
     on(c,b)],[],[],[],extraprops{constraints:[],goal:[on(b,a),on(c,b)],init:[handempty,clear(a),clear(b),clear(c),ontable(a),
     ontable(b),ontable(c)],length:[],metric:[],object:[block([a,b,c])],problem_filename:
     '/opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/planner/orig_pddl_parser/test/blocks/blocks-03-0.pddl',
     problem_name:'blocks-3-0',requires:[],uses_domain:blocks}).

   ?- prop_get(init, $Y , O).

   ?- prop_merge(init, $Y , suey(y)).

      ?- prop_get(init, $Y , O).


   ?- prop_set(init, $Y , suey(y)).


*/

prop_get(Call):- Call=..[P,A,B],prop_get(P,A,B).

prop_get(Name,Dict,Value):- (var(Name);var(Dict)),!,trace_or_throw(var_prop_get(Name,Dict,Value)).
prop_get(Name,Dict,Value):- nonvar(Value),!,must(prop_get(Name,Dict,ValueVar)),!,Value=ValueVar.
prop_get(_,     Dict, _ ):- (\+ \+ Dict=[] ),!, fail.
prop_get(Name, Struct,  Value):- prop_get_try(Name, Struct,  Value, _),!.
prop_get(Name, Struct,  Value):- Name \= extraprops, prop_get(extraprops, Struct,  Extra),
                                              prop_get_try(Name, Extra,  Value, _),!.
prop_get(Name, _Struct, Value):- bb_get(Name,Value),!.
prop_get(Name, _Struct, Value):- nb_current(Name,Value),!.


key_match(Name,N):-atom(N), (Name=N -> true ; atom_concat(':',Name,N)).

prop_get_try(_,     Dict, _  ,_ ):- (\+ \+ Dict=[] ),!, fail.
prop_get_try(Name, bb,   Value, gvar(Name,Value)):- !, must(bb_get(Name,Value)).
prop_get_try(_   , Atomic,  _  , _  ):- atomic(Atomic),!,fail.
prop_get_try(Name, Dict,   Value, Ref):- prop_get_map(Name, Dict, Value),!,must(Dict=Ref).
prop_get_try(Name, STRUCT,  Value, Ref):- STRUCT = mutable(Struct), !, prop_get_try(Name, Struct,  Value, Ref).

prop_get_map(_,    Dict,       _ ):- (\+ \+ Dict=[] ),!, fail.
prop_get_map(Name, Struct,  Value):- is_list(Struct),memberchk(Name=Value,Struct).
prop_get_map(Name, Dict,    Value):- is_dict(Dict),!,get_dict(Name,Dict,Value).
prop_get_map(Name, Dict,    Value):- is_rbtree(Dict),!,trace,nb_rb_get_node(Dict,Name,Value).
prop_get_map(Name, Dict,    Value):- is_assoc(Dict),!,get_assoc(Dict,Name,Value).


prop_get_map(sclass, sterm(Type,_), Type).
prop_get_map(Name, sterm(_,LIST), Value):- append(_,[N,V|_],LIST),key_match(Name,N),!,V=Value.

prop_get_map(Indx, Struct,  Value):- integer(Indx),!, arg(Indx,Struct,Value).

prop_get_map(Name, Struct,  Value):- member_loc(StructName,Name,N), functor(Struct,StructName,_),!,
      must((integer(N) -> arg(N,Struct,Value); prop_get_map(Name, Struct,  Value))).



prop_put_extra_extra(Struct,More):- must_det_l((prop_get(extraprops,Struct,Extras),prop_set(extraprops,Extras,More))).
  


prop_set(Call):- Call=..[P,A,B],prop_set(P,A,B).


prop_set(Name,Dict,Value):- (var(Name);var(Dict)),!,trace,trace_or_throw(var_prop_set(Name,Dict,Value)).
prop_set(_,     Dict, _ ):- (\+ \+ Dict=[] ),!, fail.
prop_set(Name,Dict,Value):- 
 member_arg_convert(Dict,Name,_,Value,NewValue) -> 
    must(((prop_set_try(Name,Dict,NewValue, NewDict ),NewDict==Dict)));
    must(((prop_set_try(Name,Dict,Value, NewDict ),NewDict==Dict))).

prop_set_try(Name,Dict,Value,_):- (var(Name);var(Dict);var(Value)),!,trace,trace_or_throw(var_prop_set(Name,Dict,Value)).
prop_set_try(_,    Dict,   _, _):- (\+ \+ Dict=[] ),!, fail.
prop_set_try([Name],Dict,Value,NewDict):-!, prop_set_try(Name,Dict,Value, NewDict).
prop_set_try(Name, bb,   Value, _):- !, bb_put(Name,Value).
prop_set_try(_,    Struct,   _, _):- ( \+ compound(Struct)),!,fail.
prop_set_try([Name,Last],Dict,Value,Dict):- prop_get(Name,Dict,SubDict),prop_set_try(Last,SubDict,Value,NewSubDict),NewSubDict\==SubDict,prop_set_try(Name,Dict,NewSubDict),!.
prop_set_try([Name|More],Dict,Value,WasNewDict):-prop_get(Name,Dict,SubDict),prop_set_try(More,SubDict,Value,NewDict),NewDict\==SubDict,prop_set_try(Name,Dict,NewDict,WasNewDict).

prop_set_try( Name,Dict,Value, Dict):-  prop_set_map(Name,Dict,Value),!.
prop_set_try( Name,Dict,Value, NewDict) :- is_dict(Dict),!,prop_set_dict_real(Name,Dict,Value,NewDict).


prop_set_map(Name,Dict,Value):- (var(Name);var(Dict);var(Value)),!,trace,trace_or_throw(var_prop_set_map(Name,Dict,Value)).
prop_set_map(sclass, STERM, Type):- STERM=sterm(Type,_), nb_setarg_ex(1,STERM,Type).
prop_set_map(Name, STERM, Value):- STERM=sterm(_,List),
  nb_set_s2list(Name,List,Value,NewList),(List\==NewList -> nb_setarg_ex(2,STERM,NewList) ; true).

prop_set_map(Name,HDict,Value):- is_list(HDict), memberchk(sclass=_,HDict),!,nb_set_pairlist(Name,HDict,Value).

prop_set_map(Name,HDict,Value):- compound(HDict), HDict = mutable(Dict),
   must_det_l((prop_set_try(Name,Dict,Value,NewDict),(Dict == NewDict -> true ; (must(nonvar(NewDict)),nb_setarg_ex(1,HDict,NewDict))))).

prop_set_map(Name,Dict,Value):- is_rbtree(Dict),!,trace, nb_rb_insert(Name,Dict,Value).
prop_set_map(Name,List,Value):- is_list(List), !, nb_set_pairlist(Name,List,Value).
prop_set_map(Index,Dict,Value):- integer(Index),!, nb_setarg_ex(Index,Dict,Value).
prop_set_map(Name,Dict,Value):- functor(Dict,StructName,_),
   (member_loc(StructName,Name,N) -> nb_setarg_ex(N,Dict,Value);
     must_det_l((prop_get(extraprops,Dict,Extra),nonvar(Extra),prop_set(Name,Extra,Value)))).




prop_set_dict_real(Name,Dict,Value, Dict):-  get_dict(Name,Dict,Old),!, (Value==Old -> (!); ((nb_set_dict(Name,Dict,Value)))),!.
prop_set_dict_real(Name,Dict,Value, NewDict):- put_dict(Name,Dict,Value,NewDict).

nb_set_pairlist(Name,List,Value):- must(List=[_|_]), must(nb_set_pairlist0(Name,List,Value)).
nb_set_pairlist0(Name,List,Value):- 
     List = [PName=_|T],!,
          ((PName==Name -> nb_setarg_ex(1,List,Name=Value) ;
           T == [] -> nb_setarg_ex(2,List,[Name=Value]) ;
           nb_set_pairlist0(Name,T,Value))).

nb_set_s2list(Name,List,Value,NewList):- must(List=[_|_]), must(nb_set_s2list0(Name,List,Value,NewList)).

nb_set_s2list0(Name,[],Value,[Name,Value]).
nb_set_s2list0(Name,LIST,Value,LIST):- append(_,[N|REST],LIST),key_match(Name,N),nb_setarg_ex(1,REST,Value),!.
nb_set_s2list0(Name,LIST,Value,LIST):- LIST = [_|A2REST],A2REST=[_|REST],nb_setarg_ex(2,A2REST,[Name,Value|REST]).

nb_set_s2list0(Name,LIST,Value,NEWLIST):- 
   ((append(_,[N|REST],LIST),key_match(Name,N)) -> (nb_setarg_ex(1,REST,Value),NEWLIST=LIST);
   append(LIST,[Name,Value],NEWLIST)).


prop_merge([Name],Struct,Value):-!, prop_merge(Name,Struct,Value).
prop_merge([Name|More],Struct,Value):-!, prop_get(Name,Struct,Ref),prop_merge(More,Ref,Value).
prop_merge(Name,Struct,ValueIn):- term_to_ord_term(ValueIn,Value),
   (prop_get(Name,Struct,Old) -> merge_values(Old,Value,New) ;  Value=New),
   prop_set(Name,Struct,New),!.


% term_to_ord_term(+Term, -OrdTerm)
%
%   Go throught the term and look for sets, return the same term
%   with all sets become ordered.
%

term_to_ord_term(Term, OrdTerm):-t2ot(Term, OrdTerm).

t2ot(A, A):- \+ compound(A), !.
t2ot(vv(T), T):-!.
t2ot(T, OTO):-t2ot_0(T, OT),(T==OT->OTO=T;OTO=OT).


t2ot_0([H|T], R):-
    t2ot(H, OH),
    t2ot(T, OT),
    ord_add_element(OT, OH, R),
    !.
%    write(OH), write(OT), write('   '), write(R), nl.

t2ot_0(T, OT):-
    T =.. [F,P],
    !,
    t2ot(P, OP),
    OT =..[F,OP].
t2ot_0(T, OT):-
    T =.. [F,P|Ps],
    NT=.. [F|Ps],
    t2ot(P, OP),
    t2ot(NT, ONT),
    ONT =.. [_|OPs],
    OT =.. [F,OP|OPs],
    !. 


merge_values(Var,Value,Value):-var(Var),!.
merge_values([], Value,Value).
merge_values(Old,Value,Value):-Old==Value,!.
merge_values(Old,Value,New):-is_list(Old),!,(is_list(Value)->ord_union(Old,Value,New);ord_add_element(Old,Value,New)).
merge_values(Old,Value,[Value,Old]).

nb_setarg_ex(Name,Struct,New):-(var(Name);var(Struct);var(New)),!,trace,trace_or_throw(var_prop_set_map(Name,Struct,New)).
nb_setarg_ex(Name,Struct,New):-arg(Name,Struct,Old),nb_setarg(Name,Struct,New),ignore(Old=New).

member_datatype(prototype,compound).

by_name_datatype(init, sorted).
by_name_datatype(goal, sorted).
by_name_datatype(assign_effect, sorted).
by_name_datatype(effects, sorted).
by_name_datatype(negativ_effect, sorted).
by_name_datatype(positiv_effect, sorted).
by_name_datatype(preconditions, sorted).


:- functor(t{a:t},A,_),asserta(dict_functor(A)).


%% member_arg_convert(+StructName,+Name,?N,+Value,-NewValue).

struct_sclass(sterm(SC,_),SC).
struct_sclass(mutable(Struct),SC):-!,struct_sclass(Struct,SC).
struct_sclass([sclass=SC|_],SC).
struct_sclass([],any).
struct_sclass(Struct,SC):-prop_get(sclass,Struct,SC).
struct_sclass(Struct,SC):-functor(Struct,F,_),(dict_functor(F)->prop_get(sclass,Struct,SC);SC=F).

member_arg_convert( _,Name,_,Value,Value):-var(Name),!.
member_arg_convert(_,Name,_N,Value,NewValue):-by_name_datatype(Name,Type),!,to_datatype(Type,Value,NewValue).
member_arg_convert(_,varnames,_N,Value,Value):-!.
member_arg_convert(Struct,Name,N,Value,NewValue):- \+ \+ (Struct=[] ),!,member_arg_convert(any,Name,N,Value,NewValue).
member_arg_convert(Struct,Name,N,Value,NewValue):- \+atom(Struct),!, struct_sclass(Struct,SC),!,member_arg_convert(SC,Name,N,Value,NewValue).
member_arg_convert(uppercase_string,charAt(_),_,Char,Converted):-to_upper(Char,Converted).
member_arg_convert(StructName,Name,_N,Value,NewValue):-member_datatype(StructName,Name,Type),to_datatype(Type,Value,NewValue).
member_arg_convert(StructName,_Name,N,Value,NewValue):-struct_datatype(StructName,ArgTypes),arg(N,ArgTypes,Datatype),to_datatype(Datatype,Value,NewValue).
member_arg_convert(_Type,Datatype,_,Value,NewValue):-to_datatype(Datatype,Value,NewValue).
member_arg_convert(_Type,_Named,_,UnConverted,UnConverted).


if_changed(Value,NewValue,NewValueO):- must((NewValue=@=Value -> NewValueO=Value ; NewValueO=NewValue)).

to_datatype(=,Value,Value).
to_datatype(sorted,Value,NewValueO):-term_to_ord_term(Value,NewValue),!,if_changed(Value,NewValue,NewValueO).
to_datatype(_Type,Value,Value).


decl_struct(StructDecl):- 
  must_det_l((
    compile_argtypes(StructDecl,1,StructPrototype),    
    functor(StructPrototype,StructName,_),
    show_call(ain(struct_prototype(StructName,StructPrototype))))),!.

decl_argtypes(StructDecl):- 
  compile_argtypes(StructDecl,"NotSlotted",_),!.

compile_argtypes(StructDecl,Loc,StructPrototype):- 
 must_det_l((
    functor(StructDecl,StructName,_),
    StructDecl=..[StructName|PARGS],
    compile_struct_slots(StructName,Loc,PARGS,PArgNames,PArgTypes,InitArgs),
    StructPrototype=..[StructName|InitArgs],
  (number(Loc) -> 
    ((
      ArgNames=..[StructName|PArgNames],ain(struct_names(StructName,ArgNames)),
      Datatypes=..[StructName|PArgTypes],ain(struct_datatypes(StructName,Datatypes))));
    true))).
    


compile_struct_slots(_,_,[],[],[],[]).
compile_struct_slots(StructType,Loc,[Param|ARGS],[Name|ArgNames],[Datatype|Datatypes],[Init|InitTypes]):-
   extract_struct_parameter(=,Param,Name,Datatype,Init),
   (number(Loc)->(ain(member_loc(StructType,Name,Loc)), Loc2 is Loc + 1);Loc2=Loc),
   ain(member_datatype(StructType,Name,Datatype)),
   (nonvar(Init)-> ain(member_init(StructType,Name,Datatype));true),   
   compile_struct_slots(StructType,Loc2,ARGS,ArgNames,Datatypes,InitTypes).


extract_struct_parameter(_Def,Name:Datatype,Name,Datatype,Init):-!,
  datatype_to_init(Datatype,Init).

extract_struct_parameter(Def,Decl=Init,Name,Datatype,Init):-!,
  extract_struct_parameter(Def,Decl,Name,Datatype).

extract_struct_parameter(Def,Decl,Name,Datatype,Init):-
   extract_struct_parameter(Def,Decl,Name,Datatype),!,
   datatype_to_init(Datatype,Init).

extract_struct_parameter(_Def,Decl,Name,Type):-Decl=..[K1,K2,Name],!,Type=..[K1,K2].
extract_struct_parameter(_Def,Decl,Name,Type):-Decl=..[Type,Name],!.
extract_struct_parameter(Def,Name,Name,Def).
   
:-ain(=>(struct_decl(StructDecl),decl_struct(StructDecl))).


ensure_instance(Type,Struct):-ensure_struct(Type,Struct).
ensure_instance(Type,List,Struct):-ensure_struct(Type,List,Struct).

ensure_struct(Type,Struct):- nonvar(Struct)->prop_set(sclass,Struct,Type);new_struct(Type,Struct).
ensure_struct(Type,List,Struct):- must_det_l((ensure_instance(Type,Struct),prop_set_nvlist(Struct,List))).

prop_set_nvlist(Struct,[N=V|More]):-must_det_l((prop_set(N,Struct,V),( More==[]->true;prop_set_nvlist(Struct,More)))).
prop_get_nvlist(Struct,[N=V|More]):-must_det_l((ignore(show_call_failure(prop_get(N,Struct,V))),( More==[]->true;prop_get_nvlist(Struct,More)))).

new_struct(Type,Struct):- var(Type),!,trace_or_throw(var_new_struct(Type,Struct)).
new_struct(Type,Struct):- struct_prototype(Type,Struct),!,struct_prototype(Type,Struct).
new_struct(Type,Struct):- struct_datatype(Type,DType),!,new_struct(DType,Struct).
new_struct(Type,mutable([sclass=Type])):-!.
new_struct(Type,[sclass=Type]):-!.



datatype_to_init(dict, mutable([sclass=dict])).
datatype_to_init(rb,   NewArg):-rb_new(NewArg),!.
datatype_to_init(assoc,NewArg):-empty_assoc(NewArg),!.
datatype_to_init(actions,[]).
datatype_to_init(sorted,[]).
datatype_to_init(_,_).

/*
new_struct(map, mutable(O)):- dict_create(O,Name,[]),!.
%new_struct(Type,Struct):- rb_new(O),rb_insert_new(O,sclass,Type,Struct),!.
%new_struct(Type,Struct):- rb_insert_new(_O,sclass,Type,Struct),!.
new_struct(Name,mutable(O)):- dict_create(O,Name,[]),!.
*/

