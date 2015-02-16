/** <module> 
% File used as storage place for all predicates which change as
% the world is run.
%
% props(Obj,height(ObjHt)) == k(height,Obj,ObjHt) == rdf(Obj,height,ObjHt) == height(Obj,ObjHt)
% padd(Obj,height(ObjHt)) == padd(height,Obj,ObjHt,...) == add(QueryForm)
% kretract[all](Obj,height(ObjHt)) == kretract[all](Obj,height,ObjHt) == pretract[all](height,Obj,ObjHt) == del[all](QueryForm)
% keraseall(AnyTerm).
%
%
% Dec 13, 2035
% Douglas Miles
*/
:- use_module(library(semweb/turtle)).
:- include(dbase_i_header).
:- multifile(user:term_expansion/2).

% [Manditory] define how we interact with the module system
swi_module(M,E):-dmsg(swi_module(M,E)).

:-dynamic_multifile_exported(is_stable/0).
is_stable:-fail.

fast_mud.

xperimental:-fail.
xperimental_big_data:-fail.
:-dynamic_multifile_exported(is_release/0).
is_release :- fail,1 is random(3).
:-dynamic_multifile_exported(not_is_release/0).
not_is_release :- true. % 1 is random(3).
simple_code :- fail.
save_in_dbase_t:-true.
not_simple_code :- \+ simple_code.
type_error_checking:-false.


:-dynamic(thlocal:infForward).
thlocal:infForward.


:-export(((current_context_module/1,
    term_expansion_local/2,
         register_module_type/1,          
         end_module_type/1,
         register_timer_thread/3))).

:-  op(1120,fx,export),op(1120,fx,dynamic_multifile_exported).


mudIsa_motel(tCol,tCol).
mudIsa_motel(I,T):-no_repeats_av(deduce_M(isa(I,T))),I\=tCol,I\==isTDisjoint(tBOT),I\==tTOP,T\==isTDisjoint(tBOT),T\==tTOP.


:- set_prolog_flag(double_quotes, atom).
:- set_prolog_flag(double_quotes, string).
/*
:- '@'((ensure_loaded(logicmoo(logicmoo_util/logicmoo_util_bugger)),
         ensure_loaded(logicmoo(logicmoo_util/logicmoo_util_library)),
         use_module(logicmoo(logicmoo_util/logicmoo_util_ctx_frame)),
         ensure_loaded(logicmoo(logicmoo_util/logicmoo_util_strings)),
         use_module(logicmoo(logicmoo_util/logicmoo_util_terms)),
         use_module(logicmoo(logicmoo_util/logicmoo_util_dcg))),'user').
*/

% ========================================
% include_moo_files(MASK)
% ========================================

include_moo_files(Mask):- expand_file_name(Mask,X),
     forall(member(E,X),ensure_moo_loaded(E)).
/*
module(M,Preds):-
    'format'(user_error,'% visting module ~w.~n',[M]),
    forall(member(P,Preds),dynamic_multifile_exported(P)).
*/
scan_updates:-thread_property(X,alias(loading_code)),thread_property(X,status(running)),!.
scan_updates:-!.
scan_updates:-ignore(catch(make,_,true)).


do_term_expansions:- context_module(CM), notrace(do_term_expansions(CM)).

do_term_expansions(_):- thread_self(ID),always_expand_on_thread(ID),!.
do_term_expansions(_):- always_transform_heads,not(prevent_transform_moo_preds),!.
do_term_expansions(_):- is_compiling_clause.
do_term_expansions(CM):- may_moo_term_expand(CM),!, not(ended_transform_moo_preds), not(prevent_transform_moo_preds).

check_term_expansions:- not(do_term_expansions).

:- meta_predicate locate_moo_file(:,-).

locate_moo_file(I,O):-locate_moo_file0(I,O),!.
locate_moo_file(_:O,O):-!.
locate_moo_file(O,O).
locate_moo_file0(user:SpecPre, Path):-!,locate_moo_file0(SpecPre, Path).
locate_moo_file0(_:SpecPre, Path):-!,locate_moo_file0(SpecPre, Path).
locate_moo_file0(SpecPre, Path) :-
        catch((expand_file_search_path(SpecPre,Spec)),_,fail),
        catch(absolute_file_name(Spec,
                           [ file_type(prolog),
                             access(read)
                           ],
                           Path),_,fail),
        exists_file(Path),!.

:- meta_predicate ensure_moo_loaded(:).

% the once/1s here arte just for dmiles institional memory
ensure_moo_loaded(A) :-
   setup_call_cleanup(once(asserta(may_moo_term_expand(_))),
        load_moo_files(A),
        once(retract(may_moo_term_expand(asdasdasd)))).

:- meta_predicate load_moo_files(:,+).

load_moo_files(F0):-!, with_assertions(thlocal:in_dynamic_reader, user_ensure_loaded(F0)).
% load_moo_files(F0):-use_module(F0).

load_moo_files(M:F0,List):-!,
  locate_moo_file(M:F0,F),  % scope_settings  expand(true),register(false),
  % 'format'(user_error,'%  ~q + ~q -> ~q.~n',[M,F0,F]),
  load_files(F,[if(not_loaded), must_be_module(true)|List]).
   %load_files(F,[redefine_module(false),if(not_loaded),silent(false),redynamic_multifile_exported(true),must_be_module(true)|List]).   
load_moo_files(M:F0,List):-
  locate_moo_file(M:F0,F),  % scope_settings
  'format'(user_error,'% load_moo_files_M ~q.~n',[M=locate_moo_file(F0,F)]),
   load_files(F,[redefine_module(false),module(M),expand(true),if(not_loaded),redynamic_multifile_exported(true),register(false),silent(false),must_be_module(true)|List]).




:- meta_predicate tick_every(*,*,0).
:- meta_predicate register_timer_thread(*,*,0).

register_timer_thread(Name,_Seconds,_OnTick):-current_thread(Name,_Status).
register_timer_thread(Name,Seconds,OnTick):-
   thread_create(tick_every(Name,Seconds,OnTick),_ID,[alias(Name)]). 

tick_every(Name,Seconds,OnTick):-repeat,sleep(Seconds),catch(OnTick,E,dmsg(caused(Name,OnTick,E))),fail.

hdr_debug(_,_):-!.
hdr_debug(F,A):-'format'(F,A).
:-meta_predicate term_expansion_local(?,?),term_expansion_local0(?,?).
% :-meta_predicate user:term_expansion(?,?).


term_expansion_local(X,_):-not(compound(X)),!,fail.
term_expansion_local( ((':-'(_))) , _ ):-!,fail.

term_expansion_local(_:B1,B2):-!,term_expansion_local(B1,B2),!.

term_expansion_local(((H1:-B1)),H2B2):- !,
   nonvar(B1),  current_context_module(CM),!,        
      functor_catch(H1,F,A), atom_concat(P,'_hook',F),!,atomic_list_concat_catch([_,_|_],'_',P),
      H1=..[F|ARGS], H2=..[P|ARGS],
      B2 = '@'((nop(CM), B1), CM ),
      module_transparent((P/A)),
      % copy_term(H2,META), meta_predicate(META),
      dynamic(P/A),
      dynamic_multifile_exported(P/A),
      multifile(P/A),
      ignore(H2B2 = ((H2 :- B2))),!.


term_expansion_local(X,Y):- compound(X),loading_module_h(CM),functor_catch(X,F,A),term_expand_local(CM,X,F,A,Y).



term_expand_local(CM,X,F,A,Y):-findall(Y,term_expand_local_each(CM,X,F,A,Y),Ys), Ys == [],!,fail.  

term_expand_local_each(_,_,F,A,_):- member(F / A,[never_expand]),!,fail.
term_expand_local_each(CM,X,F,A,X):-registered_module_type(CM,utility),dynamic_multifile_exported(F/A).
term_expand_local_each(CM,X,F,A,X):-registered_module_type(CM,dynamic),dynamic(F/A).

term_expansion_local0(A,B):- compound(A),term_expansion_local(A,B),!.

% user:term_expansion(X,Y):- term_expansion_local0(X,Y).



current_context_module(Ctx):-user:loading_module_h(Ctx),!.
current_context_module(Ctx):-context_module(Ctx).

% ========================================
% register_module_type/end_module_type
% ========================================
:- module_transparent register_module_type/1.
:- dynamic_multifile_exported registered_module_type/2.

register_module_type(Type):-current_context_module(CM),register_module_type(CM,Type).
register_module_type(CM,Types):-is_list(Types),!,forall(member(T,Types),register_module_type(CM,T)).
register_module_type(CM,Type):-asserta_new(registered_module_type(CM,Type)).

:-dynamic_multifile_exported(end_module_type/2).
end_module_type(Type):-current_context_module(CM),end_module_type(CM,Type).
end_module_type(CM,Type):-retractall(registered_module_type(CM,Type)).



% ================================================
% Thread Locals
% ================================================

:- thread_local adding_from_srcfile/0.
:- thread_local agent_current_action/2.
:- thread_local dbase_capture/2.
:- thread_local dbase_change/2.
:- thread_local dbase_opcall/2.
:- thread_local deduceArgTypes/1.
:- thread_local do_slow_kb_op_now/0.
:- thread_local enable_src_loop_checking/0.
:- thread_local in_prolog_source_code/0.
:- thread_local infAssertedOnly/1.
:- thread_local infInstanceOnly/1.
:- thread_local no_arg_type_error_checking/0.
:- thread_local noRandomValues/1.
:- thread_local skip_db_op_hooks/0.
:- thread_local thlocal:adding_from_srcfile/0.
:- thread_local thlocal:agent_current_action/2.
:- thread_local thlocal:caller_module/2.
:- thread_local thlocal:dbase_capture/2.
:- thread_local thlocal:dbase_change/2.
:- thread_local thlocal:dbase_opcall/2.
:- thread_local thlocal:deduceArgTypes/1.
:- thread_local thlocal:do_slow_kb_op_now/0.
:- thread_local thlocal:enable_src_loop_checking/0.
:- thread_local thlocal:in_dynamic_reader/1.
:- thread_local thlocal:in_prolog_source_code/0.
:- thread_local thlocal:infAssertedOnly/1.
:- thread_local thlocal:infInstanceOnly/1.
:- thread_local thlocal:infSkipArgIsa/0.
:- thread_local thlocal:into_form_code/0.
:- thread_local thlocal:inVoProp/0.
:- thread_local thlocal:no_arg_type_error_checking/0.
:- thread_local thlocal:noDBaseHOOKS/1.
:- thread_local thlocal:noDBaseMODs/1.
:- thread_local thlocal:noRandomValues/1.
:- thread_local thlocal:session_agent/2.
:- thread_local thlocal:skip_db_op_hooks/0.
:- thread_local thlocal:tracing80/0.
:- thread_local thlocal:trust_argIsas/0.
:- thread_local thlocal:useAltPOS/0.
:- thread_local thlocal:useOnlyExternalDBs/0.
:- thread_local thlocal:usePlTalk/0.
:- thread_local thlocal:with_callMPred/1.

:- thread_local repl_to_string/2.
:- thread_local repl_writer/2.

:-dynamic_multifile_exported(hasInstance/2).


% ========================================
% begin/end_transform_moo_preds
% ========================================

:-decl_thlocal is_compiling_clause/0.
is_compiling:-is_compiling_clause;compiling.
:-decl_thlocal ended_transform_moo_preds/0, always_expand_on_thread/1, prevent_transform_moo_preds/0, may_moo_term_expand/1, always_transform_heads/0.
:-module_transparent begin_transform_moo_preds/0, end_transform_moo_preds/0.
:-dynamic_multifile_exported(((begin_transform_moo_preds/0,end_transform_moo_preds/0))).
begin_transform_moo_preds:- retractall(ended_transform_moo_preds),context_module(CM),asserta(may_moo_term_expand(CM)).
end_transform_moo_preds:- retractall(ended_transform_moo_preds),asserta(ended_transform_moo_preds).


% ========================================
% user_notice_predicate
% ========================================
user_notice_predicate(H):- var(H),!.
user_notice_predicate((H1,H2)):-!,user_notice_predicate(H1),user_notice_predicate(H2).
user_notice_predicate((H1;H2)):-!,user_notice_predicate(H1),user_notice_predicate(H2).
user_notice_predicate(H):- compound(H),  may_expand_currently(H), get_pifunctor(H,HH,F,A),
           \+(current_predicate(F/A)),!,
           not(mpred_prop(F,prologOnly)),
           assert_arity(F,A),        
           really_add_mpred_storage_op(HH),
           %  dynamic(F/A),
           compile_predicates([F/A]),
           asserta_if_new(mpred_prop(F,prologHybrid)),
           asserta_if_new(mpred_prop(F,predStub(prologHybrid))),
           exported(F/A).

expanded_already_functor(was_imported_kb_content).
expanded_already_functor(was_enabled).
expanded_already_functor(F):-mpred_prop(F,prologOnly).

may_expand_currently(CL):- not(thlocal:into_form_code),not(thlocal:adding_from_srcfile),not((get_functor(CL,F),expanded_already_functor(F))).


% user:goal_expanstion(H,_):- notice_predicate(H),fail.
user:term_expanstion((H:-_),_):- may_expand_currently(H),user_notice_predicate(H),fail.
%user:term_expanston((X:-TRUE),OUT):-TRUE==true,X\=was_enabled(_),X=enabled(G),show_call(call_if_defined(add(G))),OUT=(was_enabled(G):-true).

% ========================================
% Shared Preds
% ========================================
:- dynamic(dbase_module_ready).

:-op(1150,fx,export).

% ========================================
% dbase_mod/1
% ========================================

:- dynamic_multifile_exported(dbase_mod/1).
:- dynamic dbase_mod/1.
dbase_mod(user).

% TODO uncomment the next line without breaking it all!
% thglobal:use_cyc_database.


% =================================================================================================
% macros
% =================================================================================================
enabled(G):-add(G).


% TODO: canonicalize clauses first!
with_kb_assertions([],Call):- !,Call.
with_kb_assertions([With|MORE],Call):-!,with_kb_assertions(With,with_kb_assertions(MORE,Call)).
with_kb_assertions(With,Call):-
   setup_call_cleanup(asserta(With,Ref),Call,erase(Ref)).



slow_sanity(A):-nop(A).
xtreme_debug(P):- is_release,!,nop(P).
xtreme_debug(P):- not_is_release, verify_sanity(P).
verify_sanity(P):-(true; is_release),!,nop(P).
verify_sanity(P):- debugOnError(notrace(P)),!.
verify_sanity(P):- dmsg('$ERROR_incomplete_SANITY'(P)),!.


:-meta_predicate_transparent(when_debugging(+,0)).
when_debugging(What,Call):- debugging(What),!,Call.
when_debugging(_,_).

:- dynamic_multifile_exported(listprolog/0).
listprolog:-listing(mpred_prop(_,prologOnly)).


% ================================================
% A tiny bit of TMS
% ================================================

user:decl_database_hook(change(assert,_),Fact):- resolve_if_false(Fact).

was_known_false(Fact):-is_known_false(Fact),doall(retract((is_known_false(_):-true))),dmsg(trace_or_throw(error+was_known_false(Fact))).

resolve_if_false(Fact):- ignore(((is_known_false(Fact),was_known_false(Fact)))).

user:ruleRewrite(mudLabelTypeProps(Lbl,T,Props),typeProps(T,[glyphType(Lbl)|Props])).

user:decl_database_hook(change(assert,_A_or_Z),typeProps(T,_)):- decl_type_safe(T).

user:decl_database_hook(change(assert,_A_or_Z),mpred_prop(F,W)):- sanity((nop(mpred_prop(F,W)),atom(F))).



% ================================================
% DBASE_T System
% ================================================

toUpperCamelcase(Type,TypeUC):-toCamelcase(Type,TypeC),toPropercase(TypeC,TypeUC),!.
:-export(i_name/2).
i_name(OType,IType):-typename_to_iname0('',OType,IOType),!,IOType=IType.
:-export(i_name/3).
i_name(I,OType,IType):-typename_to_iname0(I,OType,IOType),!,IOType=IType.

typename_to_iname0(I,OType,IType):-type_prefix(Prefix,_),atom_concat(Prefix,Type,OType),capitalized(Type),!,typename_to_iname0(I,Type,IType).
typename_to_iname0(I,Type,IType):-nonvar(Type),toUpperCamelcase(Type,UType),atom_concat(I,UType,IType).

:- thread_local(thlocal:assert_op_override/1).

% ================================================
% MPRED_PROP System
% ================================================
:- ensure_loaded(dbase_i_coroutining).
:- ensure_loaded(dbase_i_pldoc).
:- ensure_loaded(dbase_i_term_expansion).
:- ensure_loaded(dbase_i_kb_agenda).
:- ensure_loaded(dbase_i_kb_store).
:- ensure_loaded(dbase_i_mpred_props).
:- ensure_loaded(dbase_i_mpred_stubs).
:- ensure_loaded(dbase_i_mpred_prolog).
:- ensure_loaded(dbase_i_mpred_dbase_t).
:- ensure_loaded(dbase_i_call).
:- ensure_loaded(dbase_i_isa_subclass).
:- ensure_loaded(dbase_i_formattypes).

:- decl_mpred_hybrid(predArgTypes/1).
:- forall((current_predicate(F/A),functor(P,F,A),(predicate_property(P,foreign)->true;predicate_property(P,imported_from(system)))),
  (F\=instance,F\=',',asserta_if_new(mpred_prop(F,prologOnly)))).

:- ensure_loaded(dbase_i_call_kb).
:- ensure_loaded(dbase_i_deduce).
:- ensure_loaded(dbase_i_propvals).

user:goal_expansion(G,isa(I,C)):-notrace((was_isa(G,I,C),(is_ftVar(C)->true;(not(mpred_prop(C,prologOnly)))))).
user:term_expansion(G,isa(I,C)):-notrace((was_isa(G,I,C),(is_ftVar(C)->true;(not(mpred_prop(C,prologOnly)))))).
user:term_expansion(IN,OUT):- may_expand_currently(IN),kb_term_expansion(IN,WHY),!,must(OUT = user:WHY).
dbase_module_ready.

:- assert_hasInstance(tCol,tCol).
:- debug.
:- decl_type(tCol).
:- decl_mpred_hybrid isa/2.
:- decl_mpred_hybrid subclass/2.
:- decl_mpred_hybrid disjointWith/2.
:- decl_type(ttCompleteExtentAsserted).
:- decl_type(ttFormatType).
:- define_ft(ftVar).

:- decl_type(predArgTypes).

:- ensure_loaded(dbase_i_mpred_pfc).


%may_expand_currently(A), 
user:term_expansion(A,B):- once((true;thlocal:pfcExpansion)), once(pfc_term_expansion(A,B)),A\=@=B.

is_pred_declarer(ArgsIsa)=>isa(ArgsIsa,tCol).

:- pfcAdd((isa(_,ArgsIsa)=>tCol(ArgsIsa))).

:- compile_predicates([isa/2]).

:-add(predArgTypes(typeGenls(ttTypeType,tCol))).
:- if_startup_script(with_assertions(thlocal:pfcExpansion,ensure_loaded(dbase_i_mpred_pfc_testing))).


:-decl_mpred_hybrid(typeGenls/2).
:-decl_mpred_prolog(arg/3).

isa(_,ArgsIsa)=>tCol(ArgsIsa).

typeGenls(ttRegionType,tRegion).
typeGenls(ttAgentType,tAgentGeneric).
typeGenls(ttItemType,tItem).
typeGenls(ttObjectType,tObj).
typeGenls(ttPredType,tPred).
typeGenls(ttFormatTypeType,ttFormatType).
typeGenls(ttTypeType,tCol).
typeGenls(ttSpatialType,tSpatialThing).
       
isa(COLTYPEINST,COLTYPE) , typeGenls(COLTYPE,COL) => subclass(COLTYPEINST,COL).

subclass(Sub, _Super) => tCol(Sub).
subclass(_Sub, Super) => tCol(Super).

ttFormatType(Super) , subclass(Sub, Super) => ttFormatType(Sub).

subclass(I,Sub),{is_asserted(subclass(I,Sub)),is_asserted(subclass(Sub, Super)), nonvar(I),nonvar(Sub),nonvar(Super)}
    => subclass(I,Super) , noBackchainRequired(subclass).

tCol(Inst), {isa_from_morphology(Inst,Type)} => isa(Inst,Type).

isa(Inst,Type), tCol(Inst) => isa(Type,ttTypeType).
isa(TypeType,ttTypeType) , isa(Inst,TypeType), subclass(SubInst,Inst) => isa(SubInst,TypeType).



isa(Inst,ttSpatialType), tCol(Inst) => subclass(Inst,tSpatialThing).

disjointWith(Sub, Super) <=> disjointWith( Super, Sub).
disjointWith(tRegion,tObj).
=> tCol(vtDirection).
disjointWith(ttSpatialType,ttAbstractType).

:-decl_mpred_hybrid(dividesBetween(tCol,tCol,tCol)).

:-add(isa(tRegion,ttSpatialType)).
:-add(isa(tObj,ttSpatialType)).
:-add(isa(ttFormatType,ttAbstractType)).

dividesBetween(S,C1,C2) => disjointWith(C1,C2) , subclass(C1,S) ,subclass(C2,S).
dividesBetween(tSpatialThing,tObj,tRegion).
dividesBetween(tObj,tMassfull,tMassless).
dividesBetween(tObj,tItem,tAgentGeneric).
dividesBetween(tItem,tMassfull,tMassless).

:- decl_type(prologSingleValued).
:- decl_type(prologMultiValued).
:- decl_type(macroDeclarer).
:- decl_type(F):- hasInstance(macroDeclarer,F).


:- decl_mpred(tCol(ftID),[predIsFlag]).
:- decl_type(macroDeclarer).
:- decl_type(tFunction).
:- decl_type(tPred).
:- decl_type(tRelation).
:- decl_type(ttSpatialType).


:- decl_type(discoverableType).
:- decl_mpred(tDeleted(ftID),[predIsFlag]).
:- decl_mpred_hybrid((genlInverse/2,genlPreds/2)).
:- decl_mpred_hybrid(( tCol/1, subclass/2, predArgTypes/1)).
:- decl_mpred_hybrid(argIsa/3).
:- decl_mpred_hybrid(argSingleValueDefault, 3).

:- decl_mpred_prolog(repl_to_string(tAgentGeneric,ftTerm)).
:- decl_mpred_prolog(repl_writer(tAgentGeneric,ftTerm)).

:- decl_mpred(tAgentGeneric(ftID),[predIsFlag]).
:- decl_type(vtActionTemplate).
:- decl_mpred_hybrid(( ttNotSpatialType/1,ttSpatialType/1 )).
:- decl_mpred_hybrid(macroDeclarer/1).
:- decl_mpred_hybrid(predModule, 2).
:- decl_mpred_hybrid(subclass/2).
:- decl_mpred_hybrid(predProxyAssert, 2).
:- decl_mpred_hybrid(predProxyAssert,2).
:- decl_mpred_hybrid(predProxyQuery, 2).
:- decl_mpred_hybrid(predProxyRetract, 2).
:- decl_mpred_hybrid(predTypeMax/3).
:- decl_mpred_hybrid(prologSingleValued/1).
:- decl_mpred_prolog((ruleForward/2,latitude/2, mudMoveDist/2, longitude/2)).
:- decl_mpred_prolog(call_mpred/1).
:- decl_mpred_prolog(cycAssert/2).
:- decl_mpred_prolog(integer/1).
:- decl_mpred_prolog(makeConstant/1).
:- decl_mpred_prolog(naf/1).
:- decl_mpred_prolog(number/1).
:- decl_mpred_prolog(string/1).
:- decl_mpred_prolog(var/1).

:- ensure_loaded(dbase_i_builtin).

:- ensure_loaded(dbase_i_loader).

:-decl_mpred_hybrid(instTypeProps/3).

:- with_no_term_expansions(ensure_loaded(dbase_i_mpred_pttp)).
:- with_no_term_expansions(ensure_loaded(dbase_i_mpred_snark)).


:-decl_mpred_prolog(create_meta/4).
% if SuggestedName was 'food666' it'd like the SuggestedClass to be 'food' and the stystem name will remain 'food666'
% if SuggestedName was 'food' it'd like the SuggestedClass to be 'food' and the stystem name will become a gensym like 'food1'
create_meta(SuggestedName,SuggestedClass,BaseClass,SystemName):-
   must_det(split_name_type(SuggestedName,SystemName,NewSuggestedClass)),
   ignore(SuggestedClass=NewSuggestedClass),   
   assert_subclass_safe(SuggestedClass,BaseClass),
   assert_subclass_safe(NewSuggestedClass,BaseClass),
   assert_isa_safe(SystemName,BaseClass),
   assert_isa_safe(SystemName,NewSuggestedClass),
   assert_isa_safe(SystemName,SuggestedClass).

isa(ftInt,ttFormatType).
isa(ftNumber,ttFormatType).
isa(ftString,ttFormatType).
typeGenls(tRegion,ttRegionType).
typeGenls(tAgentGeneric,ttAgentType).
typeGenls(tItem,ttItemType).
typeGenls(tObj,ttObjectType).
typeGenls(tPred,ttPredType).

isa(vtDirection,ttValueType).

:- decl_mpred_hybrid(disjointWith/2).
:- decl_type(tCol).

:- decl_mpred_hybrid(resultIsa/2).

:- decl_type(ttCompleteExtentAsserted).
:- decl_type(ttFormatted).
:- decl_type(ttFormatType).
:- decl_type(ttValueType).
:- define_ft(ftString).

fwc:-true.

ttCompleteExtentAsserted(Ext):- fwc, arg(_,vv(tCol,vtDirection,ttFormatType,tRegion,ftString,genlPreds),Ext).
%ttCompleteExtentAsserted(F):- ttSpatialType(F).
%ttCompleteExtentAsserted(F):- is_pred_declarer(F).
%ttCompleteExtentAsserted(F):- (isa_asserted(F,ttCompleteExtentAsserted)).



:- ensure_loaded(dbase_i_db_preds).
% :- ensure_loaded(dbase_i_cyc).




:- dynamic_multifile_exported(coerce/4).
coerce(What,Type,NewThing,_Else):-coerce(What,Type,NewThing),!.
coerce(_ ,_,     NewThing,Else):- NewThing = Else.


% :- if_file_exists(user_ensure_loaded(logicmoo(../externalsdbase/dbase_rules_pttp))).

:- sanity(dbase_mod(user)).

:- xperimental->if_file_exists(ensure_loaded(logicmoo('dbase/dbase_types_motel')));true.


:- register_module_type(utility).

user:agent_call_command(_Gent,actGrep(Obj)):- term_listing(Obj).


%:- discontiguous(prologSingleValued/2).
%:- discontiguous(prologMultiValued/1).

user:agent_text_command(_Agent,_Text,_AgentTarget,_Cmd):-fail.
/*
  coerce/3,
  rescan_dbase_facts/0,
     enter_term_anglify/2,
      user:agent_text_command/4,     
*/

once(A,B,C,D):-trace_or_throw(once(A,B,C,D)).

%:- ensure_loaded(logicmoo('vworld/moo_footer.pl')).

% :- with_no_term_expansions(if_file_exists(user_ensure_loaded(logicmoo(mobs/planner/dbase_i_hyhtn)))).


:-'$hide'(rescan_dbase_ops/0).
:-'$hide'(do_db_op_hooks/0).
%:- rescan_missing_stubs.
%:- rescan_mpred_props.

:-test_expand_units(tCol(_A)).


user:semweb_startup:- with_no_term_expansions(if_file_exists(user_ensure_loaded((dbase_i_rdf_store)))).




end_of_file.

% ========================================
% enter_term_anglify(MASK)
% ========================================

enter_term_anglify(X,Y):-findall(X-Y-Body,clause( mudTermAnglify(X,Y),Body),List),!,member(X-Y-Body,List),call(Body).
enter_term_anglify(X,Y):-findall(X-Y-Body,clause( term_anglify_np(X,Y),Body),List),!,member(X-Y-Body,List),call(Body).
enter_term_anglify(X,Y):-findall(X-Y-Body,clause( term_anglify_last(X,Y),Body),List),!,member(X-Y-Body,List),call(Body).
enter_term_anglify(X,Y):-findall(X-Y-Body,clause( term_anglify_np_last(X,Y),Body),List),!,member(X-Y-Body,List),call(Body).


