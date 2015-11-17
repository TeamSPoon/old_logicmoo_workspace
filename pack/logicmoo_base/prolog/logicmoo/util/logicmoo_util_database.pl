% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_database.pl
:- module(logicmoo_util_database,
          [ ain/1,
            ain0/1,
            aina/1,
            ainz/1,

            attr_bind/1,attr_bind/2,attr_bind_complete/1,
            split_attrs/3,is_attr_bind/1,

            paina/1,pain/1,painz/1,

            ainz_clause/1,ainz_clause/2,

            append_term/3,
            expand_to_hb/3,
            assert_if_new/1,
            asserta_if_new/1,
            asserta_new/1,
            assertz_if_new/1,
            assertz_new/1,
            call_provider/1,
            call_provider/2,
            clause_true/1,
            modulize_head/2,

            clause_asserted/1,clause_asserted/2,clause_asserted/3,
            clause_asserted_i/1,clause_asserted_i/2,clause_asserted_i/3,
            clause_i/1,clause_i/2,clause_i/3,

            clause_safe/2,
            debugCallWhy/2,
            erase_safe/2,
            eraseall/2,
            find_and_call/1,
            find_and_call/3,
            std_provider/3,
            mpred_mop/3,
            mpred_op_prolog/2,
            mpred_split_op_data/3,
            retract_eq/1,
            safe_univ/2,
            safe_univ0/2,
            clausify_attributes/2,
            my_module_sensitive_code/1
          ]).
:- meta_predicate
        ain(:),
        ain0(:),
        pain(:),
        paina(:),
        painz(:),
        aina(:),
        ainz(:),
        ainz_clause(:),
        ainz_clause(:, ?),
        expand_to_hb(?, ?, ?),
        assert_if_new(:),
        asserta_if_new(:),
        asserta_new(:),
        assertz_if_new(:),
        call_provider(0),
        clause_asserted(:),
        clause_asserted(:, ?),
        clause_asserted(:, ?, -),
        clause_safe(?, ?),
        debugCallWhy(?, 0),
        eraseall(+, +),
        find_and_call(+, +, ?),
        mpred_mop(+, 1, ?),
        mpred_op_prolog(?, :),
        mpred_op_prolog0(1,?),
        my_module_sensitive_code(?).

:- module_transparent
        append_term/3,
        my_module_sensitive_code/1,
        assertz_new/1,
        call_provider/2,
        clause_asserted/3,
        erase_safe/2,
        find_and_call/1,
        lmconf:first_std_provider/3,
        std_provider/3,
        mpred_split_op_data/3,
        retract_eq/1,
        safe_univ/2,
        safe_univ0/2.



:- if(false).
:- else.
:- include('logicmoo_util_header.pi').
:- endif.


:- meta_predicate clause_safe(:, ?).
:- module_transparent clause_safe/2.
:- export(clause_safe/2).


:- meta_predicate my_module_sensitive_code(?).

%= 	 	 

%% my_module_sensitive_code( ?E) is semidet.
%
% My Module Sensitive Code.
%
my_module_sensitive_code(_E):- source_context_module(CM),writeln(source_context_module=CM).


% clause_safe(M:H,B):-!,predicate_property(M:H,number_of_clauses(_)),clause(H,B).
% clause_safe(H,B):-predicate_property(_:H,number_of_clauses(_)),clause(H,B).

%= 	 	 

%% clause_safe( ?H, ?B) is semidet.
%
% Clause Safely Paying Attention To Corner Cases.
%
clause_safe(H,B):-predicate_property(H,number_of_clauses(C)),C>0,clause(H,B).


%= 	 	 

%% debugCallWhy( ?Why, :GoalC) is semidet.
%
% Debug Call Generation Of Proof.
%
debugCallWhy(Why, C):- hotrace(wdmsg(Why)),dtrace(C).

:- export(mpred_op_prolog/2).
:- module_transparent(mpred_op_prolog/2).
% mpred_op_prolog(P):-mpred_split_op_data(P,OP,Term),mpred_op_prolog(OP,Term).


%= 	 	 

%% mpred_split_op_data( ?OP, ?O, ?P) is semidet.
%
% Managed Predicate Split Oper. Data.
%
mpred_split_op_data(M:OP,M:O,P):-sanity(compound(OP)),OP=..[O,P],!.
mpred_split_op_data(M:OP,M:call,OP):-!.
mpred_split_op_data(OP,O,P):-sanity(compound(OP)),OP=..[O,P],!.
mpred_split_op_data(OP,call,OP).



% mpred_mop(OP,CALL):- sanity(not_ftVar(OP)),fail.
:- export(mpred_mop/3).
:- meta_predicate mpred_mop(+,1,?).

%= 	 	 

%% mpred_mop( +M, :PRED1Op, ?Term) is semidet.
%
% Managed Predicate Mop.
%
mpred_mop(M,C:call,CALL):-!,find_and_call(C,M,CALL).
mpred_mop(M,C:Op,Term):-!,append_term(Op,Term,CALL),find_and_call(C,M,CALL).
mpred_mop(M,Op,Term):-append_term(Op,Term,CALL),find_and_call(M,M,CALL).
mpred_mop(M,call,CALL):-!,find_and_call(M,M,CALL).
mpred_mop(M,Op,Term):-append_term(Op,Term,CALL),find_and_call(M,M,CALL).

:-meta_predicate(find_and_call(+,+,?)).

%= 	 	 

%% find_and_call( +OUT1, +C, ?G) is semidet.
%
% Find And Call.
%
find_and_call(_,_,C:G):-current_predicate(_,C:G),!,on_x_rtrace(C:G).
find_and_call(_,C,  G):-current_predicate(_,C:G),!,on_x_rtrace(C:G).
find_and_call(C,_,  G):-current_predicate(_,C:G),!,on_x_rtrace(C:G).
find_and_call(_,_,  G):-current_predicate(_,C:G),!,on_x_rtrace(C:G).
find_and_call(C,M,  G):-trace,C:on_x_rtrace(M:G).


%= 	 	 

%% find_and_call( :TermG) is semidet.
%
% Find And Call.
%
find_and_call(C:G):-current_predicate(_,C:G),!,on_x_rtrace(C:G).
find_and_call(_:G):-current_predicate(_,R:G),!,on_x_rtrace(R:G).
find_and_call(G):-current_predicate(_,G),!,on_x_rtrace(G).
find_and_call(G):-current_predicate(_,R:G),!,on_x_rtrace(R:G).


%= 	 	 

%% ain0( ?N) is semidet.
%
% Assert If New Primary Helper.
%
ain0(N):-notrace(clause_asserted(N))->true;mpred_op_prolog(assert,N).

:- export(mpred_op_prolog/2).
:- module_transparent(mpred_op_prolog/2).
:- meta_predicate mpred_op_prolog(?,:).

%= 	 	 

%% mpred_op_prolog( ?UPARAM1, ?N) is semidet.
%
% Managed Predicate Oper. Prolog.
%
mpred_op_prolog(ain0,N):- !,(notrace(clause_asserted(N))->true;mpred_op_prolog0(assert,N)).
mpred_op_prolog(paina,N):-!,(notrace(clause_asserted(N))->true;mpred_op_prolog0(asserta,N)).
mpred_op_prolog(painz,N):-!,(notrace(clause_asserted(N))->true;mpred_op_prolog0(assertz,N)).
mpred_op_prolog(pain,N):- !,(notrace(clause_asserted(N))->true;mpred_op_prolog0(assert,N)).
mpred_op_prolog(aina,N):- !,(clause_asserted(N)->true;mpred_op_prolog0(asserta,N)).
mpred_op_prolog(ainz,N):- !,(clause_asserted(N)->true;mpred_op_prolog0(assertz,N)).
mpred_op_prolog(ain,N):-  !,(clause_asserted(N)->true;mpred_op_prolog0(assert,N)).
% mpred_op_prolog(OP,M:Term):- unnumbervars(Term,Unumbered),Term \=@= Unumbered,!,trace,mpred_mop(M,OP,Unumbered).
mpred_op_prolog(OP,M:Term):-  trace,!,mpred_mop(M, OP,Term).
mpred_op_prolog(OP,M:Term):- 
  copy_term(Term, Copy, Gs),
  (Gs==[] -> mpred_mop(M,OP,Term);
    show_call(why,(
      expand_to_hb(Copy,H,B),conjoin(maplist(call,Gs),B,NB),trace,mpred_mop(M,OP,(H:-NB))))).
  

%= 	 	 

%% mpred_op_prolog0( :PRED1OP, ?MTerm) is semidet.
%
% Managed Predicate Oper. Prolog Primary Helper.
%
mpred_op_prolog0(OP,MTerm):- call(OP,MTerm).

% peekAttributes/2,pushAttributes/2,pushCateElement/2.
:- module_transparent((aina/1,ain/1,ainz/1,ain0/1,ainz_clause/1,ainz_clause/2,clause_asserted/2,expand_to_hb/2,clause_asserted/1,eraseall/2)).
:- module_transparent((asserta_new/1,asserta_if_new/1,assertz_new/1,assertz_if_new/1,assert_if_new/1)). % ,assertz_if_new_clause/1,assertz_if_new_clause/2,clause_asserted/2,expand_to_hb/2,clause_asserted/1,eraseall/2)).

:- meta_predicate paina(:),pain(:),painz(:),ain0(:),ainz_clause(:),ainz_clause(:,?).
:- meta_predicate clause_asserted(:,?),expand_to_hb(?,?,?),clause_asserted(:),eraseall(+,+).

% aina(NEW):-ignore((retract(NEW),fail)),asserta(NEW).
% ainz(NEW):-ignore((retract(NEW),fail)),assertz(NEW).
% aina(_Ctx,NEW):-ignore((retract(NEW),fail)),asserta(NEW).
% writeqnl(_Ctx,NEW):- fmt('~q.~n',[NEW]),!.


%= 	 	 

%% eraseall( +F, +A) is semidet.
%
% Eraseall.
%
eraseall(M:F,A):-!,forall((current_predicate(M:F/A),functor_catch(C,F,A)),forall(clause(M:C,B,X),erase_safe(clause(M:C,B,X),X))).
eraseall(F,A):-forall((current_predicate(M:F/A),functor_catch(C,F,A)),forall(clause(M:C,B,X),erase_safe(clause(M:C,B,X),X))).


:-thread_local(t_l:std_provider/3).
:-thread_local(t_l:current_std_provider/1).
:-dynamic(lmconf:first_std_provider/2).
:-dynamic(lmconf:next_std_provider/2).
:-multifile(lmconf:first_std_provider/2).
:-multifile(lmconf:next_std_provider/2).

%= 	 	 

%% mpred_provider( ?OP, ?Term, ?PROVIDER) is semidet.
%
% Hook To [std_provider/3] For Module Logicmoo_util_database.
% Managed Predicate Provider.
%
std_provider(OP,Term,PROVIDER):- t_l:std_provider(OP,Term,PROVIDER).
std_provider(_,_,PROVIDER):- t_l:current_std_provider(PROVIDER).
std_provider(OP,Term,PROVIDER):- lmconf:first_std_provider(OP,Term,PROVIDER).

lmconf:first_std_provider(_,_,mpred_op_prolog).

:- meta_predicate call_provider(?).

%= 	 	 

%% call_provider( ?P) is semidet.
%
% Call Provider.
%
call_provider(P):-mpred_split_op_data(P,OP,Term),call_provider(OP,Term).


%= 	 	 

%% call_provider( ?OP, ?Term) is semidet.
%
% Call Provider.
%
call_provider(OP,Term):- must(std_provider(OP,Term,PROVIDER)),!,call(PROVIDER,OP,Term).

call_provider(OP,Term):- must(std_provider(OP,Term,PROVIDER)),!,
   (loop_check_early(call(PROVIDER,OP,Term),fail)*->true;
   (loop_check_early(must(lmconf:next_std_provider(PROVIDER,NEXT)),NEXT=mpred_op_prolog),!,PROVIDER\=NEXT,call(NEXT,OP,Term))).

:- meta_predicate assert_if_new(:).

%= 	 	 

%% assert_if_new( ?X) is semidet.
%
% Assert If New.
%
assert_if_new(X):-mpred_op_prolog(pain,X).
:- meta_predicate asserta_if_new(:).

%= 	 	 

%% asserta_if_new( ?X) is semidet.
%
% Asserta If New.
%
asserta_if_new(X):-mpred_op_prolog(paina,X).
:- meta_predicate assertz_if_new(:).

%= 	 	 

%% assertz_if_new( ?X) is semidet.
%
% Assertz If New.
%
assertz_if_new(X):-mpred_op_prolog(painz,X).

:- meta_predicate asserta_new(:).

%= 	 	 

%% asserta_new( ?X) is semidet.
%
% Asserta New.
%
asserta_new(X):-mpred_op_prolog(paina,X).
:- meta_predicate asserta_new(:).

%= 	 	 

%% assertz_new( ?X) is semidet.
%
% Assertz New.
%
assertz_new(X):-mpred_op_prolog(painz,X).


%= 	 	 

%% pain( ?N) is semidet.
%
% Pain.
%
pain(N):- call_provider(pain(N)).

%= 	 	 

%% paina( ?N) is semidet.
%
% Paina.
%
paina(N):-call_provider(paina(N)).

%= 	 	 

%% painz( ?N) is semidet.
%
% Painz.
%
painz(N):-call_provider(painz(N)).


:-module_transparent(ain/1).
:-module_transparent(aina/1).
:-module_transparent(ainz/1).
:-dynamic(ain/1).
:-dynamic(aina/1).
:-dynamic(ainz/1).

%= 	 	 

%% ain( ?N) is semidet.
%
% Assert If New.
%
ain(N):- call_provider(pain(N)).

%= 	 	 

%% aina( ?N) is semidet.
%
% Aina.
%
aina(N):-call_provider(paina(N)).

%= 	 	 

%% ainz( ?N) is semidet.
%
% Ainz.
%
ainz(N):-call_provider(painz(N)).


%= 	 	 

%% ainz_clause( ?C) is semidet.
%
% Ainz Clause.
%
ainz_clause(C):- expand_to_hb(C,H,B),ainz_clause(H,B).

%= 	 	 

%% ainz_clause( ?H, ?B) is semidet.
%
% Ainz Clause.
%
ainz_clause(H,B):- clause_asserted(H,B)->true;call_provider(assertz((H:-B))).



%% expand_to_hb( ?Clause, ?H, ?B) is semidet.
%
% Split a Head+Body from Clause.
%
expand_to_hb( Var, H, B):- var(Var),!,when(nnvar(Var),expand_to_hb( Var, H, B)).
expand_to_hb( M:((H :- B)),M:H,B):-!.
expand_to_hb( ((H :- B)),H,B):-!.
expand_to_hb( H,  H,  true).


clausify_attributes(M:Data,M:THIS):- nonvar(M),!, clausify_attributes(Data,THIS).
clausify_attributes((Data:-B),THIS):- B==true,!,clausify_attributes(Data,THIS).
clausify_attributes((Data:-B),THIS):- !,
   copy_term((Data:-B),(Data0:-B0),Extra),   
   (Extra == [] -> (THIS = (Data:-B)) ; (hb_to_clause(Data0,(attr_bind(Extra),B0),THIS))).
clausify_attributes(Data,THIS):- 
   copy_term(Data,Data0,Extra),   
   (Extra == [] -> THIS = Data ; (hb_to_clause(Data0,attr_bind(Extra),THIS))).



to_mod_if_needed(M,B,MB):- B==true-> MB=B ; MB = M:B.

split_attrs(B,true,B):-var(B),!.
split_attrs(A,A,true):- is_attr_bind(A),!.
split_attrs(true,true,true):-!.
split_attrs(_:AB,A,B):- split_attrs(AB,A,B),!.

split_attrs(attr_bind(G,Call),attr_bind(G),(Call,attr_bind(G))):- !.
split_attrs(M:AB,A,MB):- !,split_attrs(AB,A,B),to_mod_if_needed(M,B,MB),!.
split_attrs((B,A),A,B):- is_attr_bind(A),!.
split_attrs((A,B),A,B):- is_attr_bind(A),!.
split_attrs((L,B),AB,(L,R)):- !,split_attrs(B,AB,R).
split_attrs(AB,true,AB).

is_attr_bind(B):- \+ compound(B),!,fail.
is_attr_bind(B):- functor(B,attr_bind,_).
is_attr_bind(_:B):-!,compound(B),functor(B,attr_bind,_).

:- meta_predicate attr_bind(0,0).
:- module_transparent attr_bind/2.
:- meta_predicate attr_bind(0).
:- module_transparent attr_bind/1.
:- meta_predicate attr_bind_complete(0).
:- module_transparent attr_bind_complete/1.
attr_bind(G):-must_det_l(G).
attr_bind_complete(G):-must_det_l(G).
attr_bind(G,Call):-must_det_l(G),(Call==true->true;(Call,attr_bind_complete(G))).


%% hb_to_clause( ?H, ?B, ?Clause ) is semidet.
%
% Join a Head+Body To Clause.
%
hb_to_clause(H,B,H):- B==true,!.
hb_to_clause(M:(H:-B1),B2,(M:H:- (B2,B1))):-!.
hb_to_clause((H:-B1),B2,(H:- (B2,B1))):-!.
hb_to_clause(H,B,(H:-B)).


assert_i(HB):-clausify_attributes(HB,CL),assert(CL).
asserta_i(HB):-clausify_attributes(HB,CL),asserta(CL).
assertz_i(HB):-clausify_attributes(HB,CL),assertz(CL).

:-export(clause_asserted/1).
:-meta_predicate(clause_asserted(:)).

%= 	 	 

%% clause_asserted( ?C) is semidet.
%
% Clause Asserted.
%
clause_asserted(C):- expand_to_hb(C,H,B),clause_asserted(H,B).

:-export(clause_asserted/2).
:-meta_predicate(clause_asserted(:,?)).

%= 	 	 

%% clause_asserted( ?H, ?B) is semidet.
%
% Clause Asserted.
%
clause_asserted(H,B):-clause_asserted(H,B,_),!.

:-export(clause_asserted/3).
:-meta_predicate(clause_asserted(:,?,-)).

%= 	 	 

%% clause_asserted( ?M, ?B, -R) is semidet.
%
% Clause Asserted.
%
clause_asserted(M:H,B,R):- clause(M:H,B,R),clause(M:CH,CB,R),(CH:-CB)=@=(H:-B).


:-meta_predicate(modulize_head(?,?)).

%modulize_head(_:G,O:G):- !, no_repeats_old(O,(current_module(M),'$get_predicate_attribute'(M:G, imported, O))).

%= 	 	 

%% modulize_head( ?R, ?M) is semidet.
%
% Modulize Head.
%
modulize_head(R:G,M:G):- !, (M=R; (current_predicate(_,M:G),M\==R)),\+ predicate_property(M:G,imported_from(_)).
modulize_head(G,O:G):- !, no_repeats_old(O,(current_module(M),'$get_predicate_attribute'(M:G, imported, O))).
modulize_head(G,M:G):- current_predicate(_,M:G),\+ predicate_property(M:G,imported_from(_)).


put_clause_ref(Ref,V):- !, nop(put_clause_ref(Ref,V)).
put_clause_ref(Ref,V):-put_attr(V,cref,Ref).


clause_asserted_i(HB):- expand_to_hb(HB,H,B),clause_asserted_i(H,B,_).
clause_asserted_i(H,B):- clause_asserted_i(H,B,_).
% clause_asserted_i(H00,B000,Ref):- unnumbervars((H00:B000),(H:B0)), split_attrs(B0,_A,B),!,clause_i(H,B,Ref), (clause_i(HH,BB,Ref),HH=@=H,BB=@=B,A).
clause_asserted_i(H00,B000,Ref):- unnumbervars((H00:B000),(H:B0)), split_attrs(B0,A,B),!, clause_i(H,B,Ref), (clause_i(HH,BB,Ref),HH=@=H,BB=@=B,A).


%% clause_i( ?H, ?B, ?Ref) is semidet.
%
% Clause For Internal Interface.
%
clause_i(HB):- expand_to_hb(HB,H,B),clause_i(H,B,_).
clause_i(H,B):- clause_i(H,B,_).
% clause_i(H00,B000,Ref):- unnumbervars((H00:B000),(H:B0)), split_attrs(B0,_A,B),!,clause_i(H,B,Ref), (clause_i(HH,BB,Ref),HH=@=H,BB=@=B,A).
% clause_i(H,B,Ref):- clause(H,AB,Ref), (must(split_attrs(AB,A,B0)->A),B=B0).
clause_i(H0,B0,Ref):- copy_term_nat(H0,H),clause(H,B,Ref),split_attrs(BC,AV,B)-> AV -> H=H0 -> B=B0.




:-meta_predicate(clause_true(?)).


%= 	 	 

%% clause_true( ?G) is semidet.
%
% Clause True.
%
clause_true(M:G):-!,clause(M:G,true)*->true;(current_module(M2),clause(M2:G,true)).
clause_true(G):- !, notrace((current_module(M), \+ \+  clause(M:G,_,_))),!, clause(M:G,true).
clause_true(M:G):-predicate_property(M:G,number_of_clauses(_)),!,clause(M:G,true).
clause_true(_:G):-!,predicate_property(M:G,number_of_clauses(_)),clause(M:G,true).
clause_true(G):-!,predicate_property(M:G,number_of_clauses(_)),clause(M:G,true).

:-export(retract_eq/1).

%= 	 	 

%% retract_eq( ?HB) is semidet.
%
% Retract Using (==/2) (or =@=/2) ).
%
retract_eq(HB):-expand_to_hb(HB,H,B),show_failure(modulize_head(H,MH)),clause_asserted(MH,B,Ref),erase(Ref).


:-export(safe_univ/2).

%= 	 	 

%% safe_univ( ?Call, ?List) is semidet.
%
% Safely Paying Attention To Corner Cases Univ.
%
safe_univ(Call,List):-hotrace(safe_univ0(Call,List)),!.


%= 	 	 

%% safe_univ0( ?M, :TermN) is semidet.
%
% Safely Paying Attention To Corner Cases Univ Primary Helper.
%
safe_univ0(M:Call,[N:L|List]):- nonvar(M),nonvar(N),!,safe_univ0(Call,[L|List]).
safe_univ0(Call,[M:L|List]):- nonvar(M),!,safe_univ(Call,[L|List]).
safe_univ0(M:Call,[L|List]):- nonvar(M),!,safe_univ(Call,[L|List]).
safe_univ0(Call,[L|List]):- not(is_list(Call)),sanity(atom(L);compound(Call)), Call =..[L|List],!,warn_bad_functor(L).
safe_univ0([L|List],[L|List]):- var(List),atomic(Call),!,grtrace,Call =.. [L|List],warn_bad_functor(L).
safe_univ0(Call,[L|List]):- sanity(atom(L);compound(Call)),catchv(Call =.. [L|List],E,(dumpST,'format'('~q~n',[E=safe_univ(Call,List)]))),warn_bad_functor(L).

:- export(append_term/3).

%= 	 	 

%% append_term( ?T, ?I, ?HEAD) is semidet.
%
% Append Term.
%
append_term(T,I,HEAD):-atom(T),HEAD=..[T,I],!.
append_term(Call,E,CallE):-var(Call), must(compound(CallE)),CallE=..ListE,append(List,[E],ListE),Call=..List.
append_term(Call,E,CallE):-must(compound(Call)), Call=..List, append(List,[E],ListE), CallE=..ListE.



:-export(erase_safe/2).

%= 	 	 

%% erase_safe( ?VALUE1, ?REF) is semidet.
%
% Erase Safely Paying Attention To Corner Cases.
%
erase_safe(_,REF):-erase(REF).
/*
erase_safe(((M:A):-B),REF):-!,erase_safe(clause(M:A,B),REF).
erase_safe(clause(U:A,B),REF):-U=user,!, erase_safe(clause(A,B),REF).
%erase_safe(clause(A,U:B),REF):-U=user,!, erase_safe(clause(A,B),REF).
%erase_safe(clause(M:A,B),REF):-!, erase_safe_now(M,clause(A,B),REF).
erase_safe(clause(A,B),REF):-!, erase_safe_now(_,clause(A,B),REF).
erase_safe(M:(A:-B),REF):-!,erase_safe(clause(M:A,B),REF).
erase_safe((A:-B),REF):-!,erase_safe(clause(A,B),REF).
erase_safe(clause(A,B,_),REF):-!,erase_safe(clause(A,B),REF).
erase_safe(asserta(A,_),REF):-!,erase_safe(clause(A,true),REF).
erase_safe(M:A,REF):-M==user,!,erase_safe(A,REF).
erase_safe(A,REF):-!,erase_safe(clause(A,true),REF).


erase_safe_now(_,clause(M:A,B),REF):-!,erase_safe_now(M,clause(A,B),REF).
erase_safe_now(M,clause(A,B),REF):-!,
   ignore((show_success(erase_safe_now, \+ clause(M:A,B, REF)))),
   (((var(REF);
   show_success(erase_safe_now, \+ nth_clause(A, _Index, REF));   
   show_success(erase_safe_now, clause_property(REF,erased));
   show_success(erase_safe_now, \+ clause_property(REF,_))))
   -> ddmsg(warn(var_erase_safe(clause(A,B),REF))) ; 
       erase(REF)).
*/

:- source_location(S,_),prolog_load_context(module,M),!,ignore((source_file(M:H,S),(functor(H,F,A),M:module_transparent(M:F/A)),M:export(M:F/A),fail)).

