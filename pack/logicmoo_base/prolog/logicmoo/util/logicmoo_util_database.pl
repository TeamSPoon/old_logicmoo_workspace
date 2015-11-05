% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_database.pl
:- module(logicmoo_util_database,
          [ ain/1,
            ain0/1,
            aina/1,
            ainz/1,
paina/1,pain/1,            painz/1,
            ainz_clause/1,
            ainz_clause/2,
            append_term/3,
            as_clause/3,
            assert_if_new/1,
            asserta_if_new/1,
            asserta_new/1,
            assertz_if_new/1,
            assertz_new/1,
            call_provider/1,
            call_provider/2,
            clause_true/1,
            modulize_head/2,
            clause_asserted/1,
            clause_asserted/2,
            clause_asserted/3,   
            clause_safe/2,
            debugCallWhy/2,
            erase_safe/2,
            eraseall/2,
            find_and_call/1,
            find_and_call/3,
            lmconf:mpred_provider/3,
            mpred_mop/3,
            mpred_op_prolog/2,
            mpred_split_op_data/3,
            retract_eq/1,
            safe_univ/2,
            safe_univ0/2,
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
        as_clause(?, ?, ?),
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
        lmconf:first_lmconf:mpred_provider/3,
        lmconf:mpred_provider/3,
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

% 	 	 
%% my_module_sensitive_code( ?E) is semidet.
%
% My Module Sensitive Code.
%
my_module_sensitive_code(_E):- source_context_module(CM),writeln(source_context_module=CM).


% clause_safe(M:H,B):-!,predicate_property(M:H,number_of_clauses(_)),clause(H,B).
% clause_safe(H,B):-predicate_property(_:H,number_of_clauses(_)),clause(H,B).

% 	 	 
%% clause_safe( ?H, ?B) is semidet.
%
% Clause Safely Paying Attention To Corner Cases.
%
clause_safe(H,B):-predicate_property(H,number_of_clauses(C)),C>0,clause(H,B).


% 	 	 
%% debugCallWhy( ?Why, :GoalC) is semidet.
%
% Debug Call Generation Of Proof.
%
debugCallWhy(Why, C):- hotrace(wdmsg(Why)),dtrace(C).

:- export(mpred_op_prolog/2).
:- module_transparent(mpred_op_prolog/2).
% mpred_op_prolog(P):-mpred_split_op_data(P,OP,Term),mpred_op_prolog(OP,Term).


% 	 	 
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

% 	 	 
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

% 	 	 
%% find_and_call( +OUT1, +C, ?G) is semidet.
%
% Find And Call.
%
find_and_call(_,_,C:G):-current_predicate(_,C:G),!,on_x_rtrace(C:G).
find_and_call(_,C,  G):-current_predicate(_,C:G),!,on_x_rtrace(C:G).
find_and_call(C,_,  G):-current_predicate(_,C:G),!,on_x_rtrace(C:G).
find_and_call(_,_,  G):-current_predicate(_,C:G),!,on_x_rtrace(C:G).
find_and_call(C,M,  G):-trace,C:on_x_rtrace(M:G).


% 	 	 
%% find_and_call( :TermG) is semidet.
%
% Find And Call.
%
find_and_call(C:G):-current_predicate(_,C:G),!,on_x_rtrace(C:G).
find_and_call(_:G):-current_predicate(_,R:G),!,on_x_rtrace(R:G).
find_and_call(G):-current_predicate(_,G),!,on_x_rtrace(G).
find_and_call(G):-current_predicate(_,R:G),!,on_x_rtrace(R:G).


% 	 	 
%% ain0( ?N) is semidet.
%
% Assert If New Primary Helper.
%
ain0(N):-notrace(clause_asserted(N))->true;mpred_op_prolog(assert,N).

:- export(mpred_op_prolog/2).
:- module_transparent(mpred_op_prolog/2).
:- meta_predicate mpred_op_prolog(?,:).

% 	 	 
%% mpred_op_prolog( ?UPARAM1, ?CALL2) is semidet.
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
      as_clause(Copy,H,B),conjoin(maplist(call,Gs),B,NB),trace,mpred_mop(M,OP,(H:-NB))))).
  

% 	 	 
%% mpred_op_prolog0( :PRED1OP, ?MTerm) is semidet.
%
% Managed Predicate Oper. Prolog Primary Helper.
%
mpred_op_prolog0(OP,MTerm):- call(OP,MTerm).

% peekAttributes/2,pushAttributes/2,pushCateElement/2.
:- module_transparent((aina/1,ain/1,ainz/1,ain0/1,ainz_clause/1,ainz_clause/2,clause_asserted/2,as_clause/2,clause_asserted/1,eraseall/2)).
:- module_transparent((asserta_new/1,asserta_if_new/1,assertz_new/1,assertz_if_new/1,assert_if_new/1)). % ,assertz_if_new_clause/1,assertz_if_new_clause/2,clause_asserted/2,as_clause/2,clause_asserted/1,eraseall/2)).

:- meta_predicate paina(:),pain(:),painz(:),ain0(:),ainz_clause(:),ainz_clause(:,?).
:- meta_predicate clause_asserted(:,?),as_clause(?,?,?),clause_asserted(:),eraseall(+,+).

% aina(NEW):-ignore((retract(NEW),fail)),asserta(NEW).
% ainz(NEW):-ignore((retract(NEW),fail)),assertz(NEW).
% aina(_Ctx,NEW):-ignore((retract(NEW),fail)),asserta(NEW).
% writeqnl(_Ctx,NEW):- fmt('~q.~n',[NEW]),!.


% 	 	 
%% eraseall( +F, +A) is semidet.
%
% Eraseall.
%
eraseall(M:F,A):-!,forall((current_predicate(M:F/A),functor_catch(C,F,A)),forall(clause(M:C,B,X),erase_safe(clause(M:C,B,X),X))).
eraseall(F,A):-forall((current_predicate(M:F/A),functor_catch(C,F,A)),forall(clause(M:C,B,X),erase_safe(clause(M:C,B,X),X))).


:-thread_local(t_l:lmconf:mpred_provider/3).
:-thread_local(t_l:current_lmconf:mpred_provider/1).
:-dynamic(lmconf:first_lmconf:mpred_provider/2).
:-dynamic(lmconf:next_lmconf:mpred_provider/2).
:-multifile(lmconf:first_lmconf:mpred_provider/2).
:-multifile(lmconf:next_lmconf:mpred_provider/2).

% 	 	 
%% lmconf:mpred_provider( ?OP, ?Term, ?PROVIDER) is semidet.
%
% Hook To [lmconf:mpred_provider/3] For Module Logicmoo_util_database.
% Managed Predicate Provider.
%
lmconf:mpred_provider(OP,Term,PROVIDER):- t_l:lmconf:mpred_provider(OP,Term,PROVIDER).
lmconf:mpred_provider(_,_,PROVIDER):- t_l:current_lmconf:mpred_provider(PROVIDER).
lmconf:mpred_provider(OP,Term,PROVIDER):- lmconf:first_lmconf:mpred_provider(OP,Term,PROVIDER).

lmconf:first_lmconf:mpred_provider(_,_,mpred_op_prolog).

:- meta_predicate call_provider(?).

% 	 	 
%% call_provider( ?P) is semidet.
%
% Call Provider.
%
call_provider(P):-mpred_split_op_data(P,OP,Term),call_provider(OP,Term).


% 	 	 
%% call_provider( ?OP, ?Term) is semidet.
%
% Call Provider.
%
call_provider(OP,Term):- must(lmconf:mpred_provider(OP,Term,PROVIDER)),!,call(PROVIDER,OP,Term).

call_provider(OP,Term):- must(lmconf:mpred_provider(OP,Term,PROVIDER)),!,
   (loop_check_early(call(PROVIDER,OP,Term),fail)*->true;
   (loop_check_early(must(lmconf:next_lmconf:mpred_provider(PROVIDER,NEXT)),NEXT=mpred_op_prolog),!,PROVIDER\=NEXT,call(NEXT,OP,Term))).

:- meta_predicate assert_if_new(:).

% 	 	 
%% assert_if_new( ?X) is semidet.
%
% Assert If New.
%
assert_if_new(X):-mpred_op_prolog(pain,X).
:- meta_predicate asserta_if_new(:).

% 	 	 
%% asserta_if_new( ?X) is semidet.
%
% Asserta If New.
%
asserta_if_new(X):-mpred_op_prolog(paina,X).
:- meta_predicate assertz_if_new(:).

% 	 	 
%% assertz_if_new( ?X) is semidet.
%
% Assertz If New.
%
assertz_if_new(X):-mpred_op_prolog(painz,X).

:- meta_predicate asserta_new(:).

% 	 	 
%% asserta_new( ?X) is semidet.
%
% Asserta New.
%
asserta_new(X):-mpred_op_prolog(paina,X).
:- meta_predicate asserta_new(:).

% 	 	 
%% assertz_new( ?X) is semidet.
%
% Assertz New.
%
assertz_new(X):-mpred_op_prolog(painz,X).


% 	 	 
%% pain( ?N) is semidet.
%
% Pain.
%
pain(N):- call_provider(pain(N)).

% 	 	 
%% paina( ?N) is semidet.
%
% Paina.
%
paina(N):-call_provider(paina(N)).

% 	 	 
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

% 	 	 
%% ain( ?N) is semidet.
%
% Assert If New.
%
ain(N):- call_provider(pain(N)).

% 	 	 
%% aina( ?N) is semidet.
%
% Aina.
%
aina(N):-call_provider(paina(N)).

% 	 	 
%% ainz( ?N) is semidet.
%
% Ainz.
%
ainz(N):-call_provider(painz(N)).


% 	 	 
%% ainz_clause( ?C) is semidet.
%
% Ainz Clause.
%
ainz_clause(C):- as_clause(C,H,B),ainz_clause(H,B).

% 	 	 
%% ainz_clause( ?H, ?B) is semidet.
%
% Ainz Clause.
%
ainz_clause(H,B):- clause_asserted(H,B)->true;call_provider(assertz((H:-B))).


% 	 	 
%% as_clause( ?UPARAM1, ?UPARAM2, ?UPARAM3) is semidet.
%
% Converted To Clause.
%
as_clause( M:((H :- B)),M:H,B):-!.
as_clause( ((H :- B)),H,B):-!.
as_clause( H,  H,  true).

:-export(clause_asserted/1).
:-meta_predicate(clause_asserted(:)).

% 	 	 
%% clause_asserted( ?C) is semidet.
%
% Clause Asserted.
%
clause_asserted(C):- as_clause(C,H,B),clause_asserted(H,B).

:-export(clause_asserted/2).
:-meta_predicate(clause_asserted(:,?)).

% 	 	 
%% clause_asserted( ?H, ?B) is semidet.
%
% Clause Asserted.
%
clause_asserted(H,B):-clause_asserted(H,B,_),!.

:-export(clause_asserted/3).
:-meta_predicate(clause_asserted(:,?,-)).

% 	 	 
%% clause_asserted( ?CALL1, ?UPARAM2, -IN3) is semidet.
%
% Clause Asserted.
%
clause_asserted(M:H,B,R):- clause(M:H,B,R),clause(M:CH,CB,R),(CH:-CB)=@=(H:-B).


:-meta_predicate(modulize_head(?,?)).

%modulize_head(_:G,O:G):- !, no_repeats_old(O,(current_module(M),'$get_predicate_attribute'(M:G, imported, O))).

% 	 	 
%% modulize_head( ?UPARAM1, ?UPARAM2) is semidet.
%
% Modulize Head.
%
modulize_head(R:G,M:G):- !, (M=R; (current_predicate(_,M:G),M\==R)),\+ predicate_property(M:G,imported_from(_)).
modulize_head(G,O:G):- !, no_repeats_old(O,(current_module(M),'$get_predicate_attribute'(M:G, imported, O))).
modulize_head(G,M:G):- current_predicate(_,M:G),\+ predicate_property(M:G,imported_from(_)).


:-meta_predicate(clause_true(?)).


% 	 	 
%% clause_true( ?G) is semidet.
%
% Clause True.
%
clause_true(M:G):-!,clause(M:G,true)*->true;(current_module(M2),clause(M2:G,true)).
clause_true(G):-!, (current_module(M),clause(M:G,true)).
clause_true(M:G):-predicate_property(M:G,number_of_clauses(_)),!,clause(M:G,true).
clause_true(_:G):-!,predicate_property(M:G,number_of_clauses(_)),clause(M:G,true).
clause_true(G):-!,predicate_property(M:G,number_of_clauses(_)),clause(M:G,true).

:-export(retract_eq/1).

% 	 	 
%% retract_eq( ?HB) is semidet.
%
% Retract Using (==/2) (or =@=/2) ).
%
retract_eq(HB):-as_clause(HB,H,B),show_failure(modulize_head(H,MH)),clause_asserted(MH,B,Ref),erase(Ref).


:-export(safe_univ/2).

% 	 	 
%% safe_univ( ?Call, ?List) is semidet.
%
% Safely Paying Attention To Corner Cases Univ.
%
safe_univ(Call,List):-hotrace(safe_univ0(Call,List)),!.


% 	 	 
%% safe_univ0( ?VALUE1, :Term_G6705) is semidet.
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

% 	 	 
%% append_term( ?T, ?I, ?HEAD) is semidet.
%
% Append Term.
%
append_term(T,I,HEAD):-atom(T),HEAD=..[T,I],!.
append_term(Call,E,CallE):-var(Call), must(compound(CallE)),CallE=..ListE,append(List,[E],ListE),Call=..List.
append_term(Call,E,CallE):-must(compound(Call)), Call=..List, append(List,[E],ListE), CallE=..ListE.



:-export(erase_safe/2).

% 	 	 
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

:- source_location(S,_),prolog_load_context(module,M),doall((source_file(M:H,S),(functor(H,F,A),M:module_transparent(M:F/A),M:export(M:F/A)))).

