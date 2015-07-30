%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% $ID$
% $Log: eqtrafo.pl,v $
% Revision 1.10  1999/04/19 12:25:04  peter
% one more bug fix
%
% Revision 1.9  1999/04/19 12:08:42  peter
% Bug fix: numbers in input file treated correctly now.
%
% Revision 1.8  1999/04/14 08:57:24  peter
% added foreign_flags.
%
% Revision 1.7  1998/04/07 10:48:49  bthomas
% updated to 3.6.1.
%
% Revision 1.6  1998/01/21 10:44:53  bthomas
% added automatic version number by revision var
%
% Revision 1.5  1998/01/16 15:30:38  bthomas
% now we REALLY work with rcs ...
%
%
% ------------------- this is old stuff ------------------- 
% Revision 1.5  1997/07/16 16:49:23  bthomas
% facts and disjunctions are also extended by cost notation
%
% Revision 1.4  1997/07/16 16:31:40  bthomas
% corrected little bug in write query
%
% Revision 1.3  1997/07/16 16:20:18  bthomas
% added the new flag protein_cost
%
% Revision 1.2  1997/06/30 13:34:32  bthomas
% changed default setting of constructor mode to noscan
%
% Revision 1.1  1997/03/06 12:01:09  bthomas
% Initial revision
%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% This is the handmade documentation: 
%%
%% 
%% EQTrafo
%% --------
%%
%% equality transformation
%% 
%% ------------------------------------------------------------
%% File: eqtrafo.pl
%% Author: Bernd Thomas [bthomas@informatik.uni-koblenz.de]
%% ------------------------------------------------------------
%%
%% Flags:
%%       eqtrafo_flag(mode,linear|exp)
%%         linear: use special transformation to break down
%%             exponential complexity of symetrical variants
%%         exp (default): old fashioned ...
%% 
%%       eqtrafo_flag(output,pretty|ilf)
%%         ilf: special output that can be processed by ilf    
%%         pretty: (default)
%%
%%       eqtrafo_flag(protein_cost,on|off)
%%         on: adds to all clauses that contain the  'eqt/3' pred in
%%             its head the protein_cost notation head :- body #(0,0).
%% constructors:
%%       (scan): only top function symbols are NO constructors, means
%%               they are pulled out, of every term. Constants and nested
%%               terms (functions) stay where they are as long as they are
%%               not part of any equation, top functions ...
%%               If eqtrafo detects an equations X = xxx , it automatically
%%               changes its behaviour as if it is running the noscan mode.
%%               
%%
%%       (noscan): every constant, function is pulled out. This means, every
%%               argument of a function or predicate is pulled out. The
%%               Input clause set is totally flattened. (default)
%%
%%       (C1,C2,...,CN): Only the specified Functors (C1 to Cn) are considered
%%               to be constructors. This means they stay where they are all
%%               other constants, functions are pulled out. 
%%
%%
%%
%% ------------------------------------------------------------
%%
%% [only major changes are added here, everything else is handled
%%  by the RCS stuff] =:-]
%%
%% Change: 8.1.97
%% Reason: added the mechanism off eqtrafo_flag, in scan_const.
%%         Two different transformation modi, old fashion with
%%         2**n possible axioms according to symetrical variants
%%         and 2n possible axioms with new trafo.
%%
%% Change: 7.1.97
%% Reason: Break down the exponential power of the symetric
%%         variants of equations (2**n). Example: 
%%         a = b \/ R => a(X) \/ [R] \/ -b(X)
%%                       -a(X) \/ [R] \/ b(X) also for every
%%         Equation contained in R. We lower this complexity
%%         down to 2n, by introducing a new transformation as
%%         follows: a = b \/ R =>  eqt(NR,a,b) \/ [R] 
%%         and additional axioms for each eqt(Nr,X,Y):
%%         a(X) \/ eqt(NR,a,b) \/ -b(X)
%%         -a(X) \/ eqt(NR,a,b) \/ b(X).
%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

version_number("$Revision: 1.10 $").

:- dynamic func_tpos/3,
	   p_funcs/1,
	   c_funcs/1,
	   def_cons/1,
	   eqtrafo_flag/2,
	   last_name/1.

:- lib(lists),
   lib(numbervars).

:- set_flag(occur_check,on),
   set_flag(print_depth,900).

:- op(900,fy,'partial_functions'),
   op(900,fy,'constructors'),
   op(1200,xfx,'<-'),
   op(1200,fx,'<-').

:- global_op( 1200, yfx, '#').

:- [myread].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% main pred: eqtrafo(+FileName)
% expects extender .tme 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eqtrafo('') :- usage.
eqtrafo('-h') :- help_eqtrafo.
eqtrafo(F) :-
	clean_up,
	setval(sort_mode,no),
	concat_atoms(F,'.tme',File),
	concat_atoms(F,'-eqt.tme',OFile),
	( \+ exists(File) ->
	    msg(["\n*** ERROR : no file named ",File]),
	    halt
	;
	    true ),
	open(File,read,S),
	scan_for_constructors(S),
	open(OFile,write,OUT), 
	logo,
	msg(["\n\nreading ",File]),
	header(OUT,File),
	repeat,
	myread(S,Term),
	( Term \== end_of_file ->
          transform_clause(OUT,Term)
       ;
	true ), 
	Term == end_of_file, !,
	close(S),

	create_protos,
	create_skolem(OUT),
	create_tpos_eq(OUT),
	tail(OUT),
	close(OUT),
	msg(["\n\nresult written to : ",OFile]).

beqt(_) :-
	msg(["\n\nERROR: unknown error !! \n"]),
	halt.

clean_up :-
	retract_all(func_tpos(_,_,_)) ,
	retract_all(p_funcs(_)),
	retract_all(eqtrafo_flag(_,_)),
	setval(eqt,0),
	setval(nr_input,0),
	setval(nr_output,0),
	setval(name_ok,0),
	default_flag_settings.

default_flag_settings :-
	assert(eqtrafo_flag(mode,exp)),
	assert(eqtrafo_flag(output,pretty)),
	assert(eqtrafo_flag(protein_cost,off)).


% =============================================
% constructor treatment 
% =============================================

scan_for_constructors(S) :-
	assert(c_funcs([])),
	assert(def_cons([noscan])),  % this is the default setting !
	repeat,
	 read(S,Term),
	( Term \== end_of_file ->
	    ( Term = (constructors C) ->
		C =.. [:|CO],
		retract(def_cons(_)),
		assert(def_cons(CO))
	    ;
	        scan_const(Term)
	    )
	;
	    true 
        ), 
	Term == end_of_file, 
	seek(S,0), !.

scan_const( eqtrafo_flag(_F,_X) ) :- !.

scan_const( (?- Rule) ):-
	neg_term_list(Rule,Clause),
	check_cons(Clause), !.
scan_const( Rule ) :-
	build_clause(Rule,Clause),
        check_cons(Clause), !.

check_cons([-(L=R)|More]) :-
	check_cons([L=R|More]).
check_cons([L=R|More]) :-
	mynonvar(L), mynonvar(R),
	add_cons(L), add_cons(R),
	check_cons(More).

% if an equation X = atom|term appears
% we HAVE TO perform termpullout on
% EVERY every argument of an term !!!

check_cons([L=R|More]) :-
	myvar(L), mynonvar(R),
	( def_cons = [scan] ->
	    retract(def_cons(_)),
	    assert(def_cons([noscan]))
	;
	   true
	),
	add_cons(R),
	check_cons(More).

check_cons([L=R|More]) :-
	mynonvar(L),myvar(R),
	( def_cons = [scan] ->
	    retract(def_cons(_)),
	    assert(def_cons([noscan]))
	;
	  true
	),
	add_cons(L),
	check_cons(More).


check_cons([_|More]) :-
	check_cons(More).

add_cons(C) :-
	functor(C,F,A),
	c_funcs(CL),
	( member([F,B],CL), A \= B ->
	    error(unique_fctnames(F,B,A))
	;
	 true
        ),
	retract(c_funcs(CL)),
	assert(c_funcs([[F,A]|CL])).

add_cons(_).

% ========================================
% error treatment
% ========================================

error_msg(unique_fctnames(F,B,A),["*** Error: unique functionnames with different arity\n",F,"/",B," and ",F,"/",A]).

error(Type) :- 
	error_msg(Type,Msg),
	msg(stderr,Msg),
	halt.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% transform a clause in 2n mode
% -----------------------------
% (1) [trafo_unequations]: remove unequations
% (2) [ren_equ]: introduce a new pred for each equation to 
%     reduce exponential power of symetricial variants
% (3) [trafo_eqt]: transform functions into predicates and 
%     add symetrical variant
% (4) [term_pullout]: pull out arguments of predicates that
%     are NO Variables, they are functions and have to be
%     transformed into predicates.
% (5) [post_processing]: 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% transform a clause in 2**n mode
% -------------------------------
% (1) [trafo_unequations]: remove unequations
% (2) [trafo_symetry]: add symetrical variant of equations
% (3) [trafo_equations]: transform functions into predicates 
% (4) [term_pullout]: pull out arguments of predicates that
%     are NO Variables, they are functions and have to be
%     transformed into predicates.
% (5) [post_processing]:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% first check the different types
transform_clause(OUT,COMMENT) :-
	string(COMMENT),
	( substring(COMMENT,"% NAME",1) ->
            retract_all(last_name(_)),
	    assert(last_name(COMMENT)),
	    setval(name_ok,1)
	;
	  setval(name_ok,0),
	  writeln(OUT,COMMENT) 
        ),
        !.


transform_clause(_OUT,Term) :-
	flags(Term), 
	setval(name_ok,0),
	!.

transform_clause(OUT,Term) :-
	foreign_flags(Term),
	setval(name_ok,0),
	msg(OUT,[Term,'.']), !.

% I know this is not a good place for this check but ...
transform_clause(OUT,(A = B)) :-
	var(A),
	var(B),
	mmsg(OUT,['% eqtrafo considers this to be redundant: ',(A=B)]),
        setval(name_ok,0),
	!.

transform_clause(OUT,Term) :-
	transform(Term,TTerm),
        post_processing(TTerm,TrafoTerm),
	incval(nr_input),
	write_clauses(OUT,TrafoTerm), 
	setval(name_ok,0),
	!.

% some protein preds: ignore them =|-]
foreign_flags(protein_flag(_X,_Y)).
foreign_flags(depth_increment(_X)).
foreign_flags(calculus(_X)).
% Peter: some other prover preds: ignore them =|-]
foreign_flags(dp_flag(_X,_Y)).
foreign_flags(ht_flag(_X,_Y)).

flags(eqtrafo_flag(X,Y)) :-
	retract_all(eqtrafo_flag(X,_)),
	assert(eqtrafo_flag(X,Y)),
	msg(["\n",X," = ",Y]).

% -------------------------------------------------
% Ok, here the real work of transformation starts
%

transform( (partial_functions P_FUNCS), [[]] ) :-
	P_FUNCS =.. [:|PF],
	assert(p_funcs(PF)), !.
transform( (constructors _C), [[]] ) :- !. 


% special treatment of queries
transform( (?- Rule), [?- |BEQT] ) :-
	neg_term_list(Rule,Clause),
	trafo_unequations(Clause, ABOLISHED_UNEQS),
	( eqtrafo_flag(mode,linear) ->
	    exp_2n(ABOLISHED_UNEQS,EBEQT)
	 ;
	    exp_2powerN(ABOLISHED_UNEQS,EBEQT)
        ),
	( EBEQT = [] -> BEQT = [[-true]]
                     ;  BEQT = EBEQT ), !.

transform( Rule, BEQT ) :-
	build_clause(Rule,Clause),
	trafo_unequations(Clause, ABOLISHED_UNEQS),
	( eqtrafo_flag(mode,linear) ->
	    exp_2n(ABOLISHED_UNEQS,BEQT)
	 ;
	    exp_2powerN(ABOLISHED_UNEQS,BEQT)
        ), !.

% 2n axioms
exp_2n(Clause,ClauseSet) :-
	check_redu(Clause,RClause),
	ren_equ(RClause,REN_EQS),
	trafo_eqt(REN_EQS,REQ_SET),
	term_pullout(REQ_SET, ClauseSet).
 
exp_2powerN(Clause,ClauseSet) :-
	trafo_symetry([], Clause, SSYM_SET),
	safe_symetry(SSYM_SET,SYM_SET),
	trafo_equations(SYM_SET, REQ_SET),
	term_pullout(REQ_SET, ClauseSet). 
	
build_clause( (Head :- Body), Clause ) :-
	term_list(Head,HeadL),
	neg_term_list(Body,BodyL),
	append(HeadL,BodyL,Clause).

build_clause( (Head <- Body), Clause ) :-
	term_list(Head,HeadL),
	neg_term_list(Body,BodyL),
	append(HeadL,BodyL,Clause).

build_clause( C, Clause ) :-
	term_list(C,Clause).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Variablen Ungleichungen
% -----------------------
% try to unify Equation with U = U
% we do this to eliminate equations in the body.
% if:
% o both sides are variables
% o or only one side is a variable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

trafo_unequations([],[]).
trafo_unequations([-(Left '=' Right)|More],TMore) :-
	var_equation( (Left '=' Right) ),
	trafo_unequations(More,TMore).
trafo_unequations([Term|More],[Term|TMore]) :-
	trafo_unequations(More,TMore).

var_equation( (L '=' R) ) :-
	( myvar(L),myvar(R)
        ;
	  myvar(L),mynonvar(R)
        ; 
	  mynonvar(L), myvar(R) ),
	(U '=' U) = (L '=' R), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Rename Equations And Introduce New Predicates
% ---------------------------------------------
% Simply replace every equation-symbol '=' by a pred
% eqt(Nr,LeftSide,RideSide).
% The negative equations are replaced by their predicative
% encoded version, e.g. functions become predicates.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ren_equ([],[]) :- !.
ren_equ([-(A=B)|More], [-EQT|EQTMore]) :-
	eqt(A=B,EQT),
	ren_equ(More,EQTMore).

ren_equ([A=B|More],[EQT|EQTMore]) :-
	getval(eqt,NR),
	EQT =.. [eqt,NR,A,B],
	incval(eqt),
	ren_equ(More,EQTMore).
ren_equ([Term|More],[Term|EQTMore]) :-
	ren_equ(More,EQTMore).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% trafo_eqt
% ---------
% This is the heart for the 2n trafo.
% every eqt pred. creates a clause, with a symetrical
% variant of the equation and the predicative encoded
% functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

trafo_eqt(Clause,[Clause|EQTClause]) :-
	member(eqt(_,_,_),Clause),
	trafo_eqt_h(Clause,EQTClause).
trafo_eqt(Clause,[Clause]).

trafo_eqt_h([],[]).
trafo_eqt_h([-(eqt(_Nr,L,R))|More],[[EQT1,EQT2]|MoreEQT]) :-
	eqt(L=R,EQT1),
	eqt(R=L,EQT2),
	trafo_eqt_h(More,MoreEQT).

trafo_eqt_h([eqt(Nr,L,R)|More],[Ax1,Ax2|MoreEQT]) :-
	eqt(L=R,EQT1),
	Ax1 = [-(eqt(Nr,L,R)),EQT1],
	eqt(R=L,EQT2),
	Ax2 = [-(eqt(Nr,L,R)),EQT2],
	trafo_eqt_h(More,MoreEQT).

trafo_eqt_h([_|More],MoreEQT) :-
	trafo_eqt_h(More,MoreEQT).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% trafo_equations
% ---------------
% Here the old fashion trafo with 2**n complexity is done
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

trafo_equations([],[]).
trafo_equations([Clause|Rest], [EqtClause|EqtSet]) :-
	check_redu(Clause,RClause),
	trans_eq(RClause,EqtClause),
	trafo_equations(Rest,EqtSet).

trans_eq([],[]).
trans_eq([EQ|Rest],[Eqt|MoreT]) :-
	( EQ = (L=R) ; EQ = -(L=R) ),
	eqt(EQ,Eqt),
	trans_eq(Rest,MoreT).

trans_eq([Lit|Rest],[Lit|MoreT]) :-
	trans_eq(Rest,MoreT).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SYMETRY
% -------
% for every equation we have to add its symetrical variant
% We only use this if 2 power N is used
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

trafo_symetry(PSet,[],PSet).
% peter fragte:
% Kann es sein, dass du negative gleichungen
%	-(X = f(X))
% nicht umorientierst zu 
%	-(f(X) = X)
% und dann die trafo machst? 
% benno: JETZT DOCH !

trafo_symetry(PrevSet,[-(L=R)|Rest],SymSet) :-
	myvar(L),
	add(-(R=L),PrevSet,NewPrevSet),
	trafo_symetry(NewPrevSet,Rest,SymSet), !.

% don't add the symetrical variant for commutative equations
% like f(A,B) = f(B,A) where A,B in Var
%
trafo_symetry(PrevSet,[L=R|Rest],SymSet) :-
	nonvar(L),
	nonvar(R),
	L =.. [Func,A1,A2],
	\+ \+ (
	  A1 = x,
	  A2 = y,
	  R =.. [Func,A2,A1]
      ),
	add((L=R),PrevSet,NewPrevSet),
	trafo_symetry(NewPrevSet,Rest,SymSet), !.

trafo_symetry(PrevSet,[L=R|Rest],SymSet) :-
	add_sym(L=R,PrevSet,NewPrevSet),
	trafo_symetry(NewPrevSet,Rest,SymSet), !.

trafo_symetry(PrevSet,[T|Rest],SymSet) :-
	add(T,PrevSet,NewPrevSet),
	trafo_symetry(NewPrevSet,Rest,SymSet), !.

add(T,[],[[T]]).
add(T,[Last],[NLast]) :-
   append(Last,[T],NLast).

add(T,[C|MC],[NC|MMC]  ) :-
   append(C,[T],NC),
   add(T,MC,MMC).

add_sym(L=R,[],[[R=L],[L=R]]).
add_sym(L=R,[Last],[LAST1,LAST2]) :-
	append(Last,[L=R],LAST1),
	append(Last,[R=L],LAST2).
add_sym(L=R,[C|MC],[NC1,NC2|MMC]) :-
	append(C,[L=R],NC1),
	append(C,[R=L],NC2),
	add_sym(L=R,MC,MMC).

% negative sind ein bisschen tricky
% wenn var(L) und nonvar(R) dann nehmen 
% wir nur die sym variante R=L auf 

add_sym(-(L=R),[],Sym) :-
	( nonvar(R), nonvar(L), Sym = [[-(R=L)],[-(L=R)]] 
    ;
	nonvar(L), Sym = [-(L=R)]
    ;
	nonvar(R), Sym = [-(R=L)] 
    ), 
	!.

add_sym(-(L=R),[Last],Sym) :-
	( nonvar(L), nonvar(R), 
	append(Last,[-(L=R)],LAST1), 
	append(Last,[-(R=L)],LAST2),
	Sym = [LAST1,LAST2]
    ;
	nonvar(L),
	append(Last,[-(L=R)],LAST1),
	Sym = [LAST1]
    ;
	
	nonvar(R), 
	append(Last,[-(R=L)],LAST1),
	Sym = [LAST1]
    
    ),
	!.

add_sym(-(L=R),[C|MC],Sym) :-
	( nonvar(L), nonvar(R), 
	append(C,[-(L=R)],NC1), 
	append(C,[-(R=L)],NC2),
	Sym = [NC1,NC2|MMC]
    ;
	nonvar(L),
	append(C,[-(L=R)],NC1),
	Sym = [NC1|MMC]
    ;
	nonvar(R), 
	append(C,[-(R=L)],NC1),
	Sym = [NC1|MMC]
    
    ),
	add_sym(-(L=R),MC,MMC), 
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Equation Transformation
% -----------------------
% every function with n arguments is converted
% into a predicate with n+1 arity, where n+1 is considered
% to be the value of the function.
% remark: here we recognize functions easily, if we find
%         a equation we also have found functions.
%         To recognize functions nested as arguments we
%         use the term-pullout predicate, because every none
%         variable argument of a predicate has to be a 
%         function. =;-] 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ok here we make a n+1 arity pred from a n arity func.
% -----------------------------------------------------
% f(s1:t1,...sn:tn):t = sn+1:t
% => f(s1:t1,...sn:tn,sn+1:t)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eqt((L = R),Eqt) :-
	mynonvar(L),
	( sort_term(L,LT,LS),
	  sort_term(R,_RT,LS),
	  setval(sort_mode,yes),
	  LT =.. [Functor|Args]
        ;
	  L =.. [Functor|Args] ),
	append(Args,[R],NArgs),
	save_mk_term([Functor|NArgs], Eqt),
	memo(Eqt).

eqt(-(L = R),-Eqt) :-
	mynonvar(L),
	( sort_term(L,LT,LS),
	  sort_term(R,_RT,LS),
	  setval(sort_mode,yes),
	  LT =.. [Functor|Args]
	;
	  L =.. [Functor|Args] ),
	append(Args,[R],NArgs),
	save_mk_term([Functor|NArgs], Eqt),
%	Eqt =.. [Functor|NArgs],
	memo(Eqt).

eqt((L=R),tpos(L,R)) :-
	myvar(L),
	( sort_term(L,_LT,LS),
	  sort_term(R,_RT,LS),
	  setval(sort_mode,yes)
        ; 
	  true ).

save_mk_term([H|T],Term) :-
	\+ number(H), !,
	Term =.. [H|T].
save_mk_term([H|T],Term) :-
	number_string(H,HS),
	atom_string(HA,HS),
	Term =.. [HA|T].
	

% ----------------------------------------
% memo
% ----
% we have to keep the functions in mind, to
% create the appropriate tpos-clauses at the 
% end of all transformations.

memo(tpos(_,_)).
memo(T) :-
	functor(T,Functor,L),
	getval(sort_mode,V), 
	( V = yes ->
	    assert(func_tpos(Functor,T,L))
	;
	  \+ func_tpos(Functor,_,L),
	  assert(func_tpos(Functor,T,L)) ).
memo(_).

%
% check if the term is an annotated sort-term
%
sort_term(SortTerm,Term,Sort) :-
	nonvar(SortTerm),
	SortTerm =.. [:,Term,Sort].

%
% check if there are equations X = X , if so throw it away.
%
check_redu(Clause,[]) :-
	\+ \+ member(re_d = re_d, Clause), % there are equations with two vars
	\+ member(re_d = re_du, Clause).   % there are equations: X=X

check_redu(Clause,[]) :-
	\+ \+ member(re_d : A = re_d : A, Clause), % there are equations with two vars
	\+ member(re_d : A = re_du : A, Clause).   % there are equations: X=X
	
check_redu(Clause,Clause).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Term-Pullout
% ------------
% pullout all non variable subterms
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% clause from clause list
term_pullout([],[]) :- !.
term_pullout([Clause|Rest], [PClause|PRest]) :-
	pull_clause(Clause,PClause),
	term_pullout(Rest,PRest).

% literal from clause
pull_clause([],[]) :- !.
pull_clause([Lit|MoreLit],PClause) :-
	pullout(Lit,PLit),
	pull_clause(MoreLit,MorePClause), 
	append(PLit,MorePClause,PClause).

% ignore the eqt-preds
pullout(-(eqt(Nr,L,R)),[-(eqt(Nr,L,R))]).
pullout(eqt(Nr,L,R),[eqt(Nr,L,R)]).

pullout(Lit,[Lit]) :-
	myvar(Lit), !.

pullout(-Lit,[-PLit|PClause]) :-
	Lit =.. [F|Args],
	length(Args,L),
	length(Template,L),
	PLit =.. [F|Template],
	pullout_args(F,Args, Template, PClause), !.

pullout(Lit,[PLit|PClause]) :-
	Lit =.. [F|Args],
	length(Args,L),
	length(Template,L),
	PLit =.. [F|Template],
	pullout_args(F,Args, Template, PClause), !.

pullout_args(_,[],[],[]).

pullout_args(F,[Arg|Args],[Arg|TArgs],PClause) :-
	myvar(Arg),
	pullout_args(F,Args,TArgs,PClause), !.

%pullout_args(F,[Arg|Args],[Arg|TArgs],PClause) :-
%	number(Arg),
%	pullout_args(F,Args,TArgs,PClause), !.

pullout_args(F,[Arg:Sort|Args],[NewConst:Sort|TArgs],PClause) :-
	check_const(Arg),
	pullout(Arg,[NewConst|NewClauses]),
	pullout_args(F,Args,TArgs,MoreClauses),
	append(NewClauses,MoreClauses,PClause), !.
	
pullout_args(F,[Arg|Args],[NewConst|TArgs],PClause) :-
	check_const(Arg),
	pullout(Arg,[NewConst|NewClauses]),
	pullout_args(F,Args,TArgs,MoreClauses),
	append(NewClauses,MoreClauses,PClause), !.

pullout_args(F,[Arg:Sort|Args],[Var:Sort|TArgs],PClause) :-
	eqt(Arg:Sort = Var:Sort ,PArg),
	pullout(-PArg,NewClause),
	pullout_args(F,Args,TArgs,MoreClauses),
	append(NewClause,MoreClauses,PClause).

pullout_args(F,[Arg|Args],[Var|TArgs],PClause) :-
	eqt(Arg=Var,PArg),
	pullout(-PArg,NewClause),
	pullout_args(F,Args,TArgs,MoreClauses),
	append(NewClause,MoreClauses,PClause).

% no constructors at all
% reason: 1) X = atom|term 
%      or 2) constructors: (noscan)

check_const(_) :-
	def_cons([noscan]),
	!, 
	fail.

% EQTrafo tries to recognize
check_const(Arg) :-
	def_cons([scan]),
	!,
	functor(Arg,Fun,Arity),
	c_funcs(CL),
	\+ member([Fun,Arity],CL).

% Use the specified functors for detection
check_const(Arg) :-
	def_cons(DEF),
	DEF \= [],
	!,
	functor(Arg,Fun,_Arity),
	member(Fun,DEF).

			
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create protos
% -------------
% this is only needed if we have to deal with 
% sorted terms
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_protos :-
	getval(sort_mode,yes),
	func_tpos(Functor,T,Le),
	T =.. [Functor|Arg],
	setval(gp,Arg),
	create_it(Functor),
	func_tpos(Functor,X,Le),
	X =.. [Functor|NArg],
	greatest(NArg),
	fail.

create_protos :- !.

create_it(_).
create_it(Functor) :-
	getval(gp,GArg),
	retract_all(func_tpos(Functor,_,_)),
	length(GArg,L),
	T =.. [Functor|GArg],
	assert(func_tpos(Functor,T,L)), !.

greatest(NArg) :-
	getval(gp,PArg),
	great(PArg,NArg,GArg),
	setval(gp,GArg), !.

great([],[],[]) :- !.
great([_T1:S1|MP],[_T2:S2|MN],[_VAR:GList|MG]) :-
	( var(S2), GList = S2 
        ; 
          var(S1), GList = S1 ),
	great(MP,MN,MG), !.

% last arg is the func value so close this sort list
great([_:PS],[_:NS],[_:GS]) :- 
	length(PS,LP),
	length(NS,LN),
	( LP > LN, G = NS
        ; LN > LP, G = PS
        ; G = PS ),
        ( open_list(G), append(G,[],GS)
	; GS = G ), !.

great([_:PList|MP],[_:NList|MN],[_:GEList|MG]) :-
	length(PList,PL),
	length(NList,NL),
	( PL < NL -> GList = PList
                  ; ( PL > NL -> GList = NList 
	                      ;  ( open_list(PList) -> GList = PList
			                       ;  GList = NList ) ) ),

	( open_list(GList),
	  GEList = GList
         ;
          make_open_list(GList,GEList) ),
	great(MP,MN,MG), !.
		              
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Create skolem functions
% -----------------------
% this is easy, for every none partial function
% a skolem functions has to be created.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_skolem(OUT) :-
	( p_funcs(P_FUNCS); P_FUNCS = [] ), % get the partial func. 
	                                    % if there are some

	( eqtrafo_flag(output,ilf) ->
	    once(func_tpos(_,_,_)),
	    mmsg(OUT,['\n% BEGIN_AXIOMS'])
	;
	  true
        ),
	create_skolem(OUT,P_FUNCS), !,
	( eqtrafo_flag(output,ilf) ->
	    mmsg(OUT,['\n% END_AXIOMS'])
	;
	  true 
        ).

create_skolem(OUT,P_FUNCS) :-
	func_tpos(Functor,T,Le),
	getval(sort_mode,V),
	T =.. [Functor|Arg],
	\+ member(Functor,P_FUNCS), % if its a partial function no skolem.

	( V = yes ->
	  free_sorts(Arg,SArg),
	  skolem(sort,Functor,SArg,Le,Skolem) 
	; 
	  skolem(no,Functor,Arg,Le,Skolem)
        ),
	incval(nr_output),
	( eqtrafo_flag(output,ilf) ->
	    mmsg(OUT,['\n% NAME: skolem(',Functor,')'])
	;
	  true ),
	msg(OUT,[Skolem,'.']),
	fail.

create_skolem(_,_).

skolem(no,Functor,_,Le,Skolem) :-
	NL is Le-1,
	length(ArgL,NL),
%	concat_atoms('sk_',Functor,SK_FUNC),
%	SK_Term =.. [SK_FUNC|ArgL],
	( integer_atom(RealFunctor,Functor) ->
	  true
        ; RealFunctor = Functor
        ),
	SK_Term =.. [RealFunctor|ArgL],
	append(ArgL,[SK_Term],Arg),
	Skolem =.. [Functor|Arg], !.

skolem(sort,Functor,Arg,_,Skolem) :-
	append(ArgL,[Last],Arg),
	Last = _T:SORT,
%	concat_atoms('sk_',Functor,SK_FUNC),
%	PSK_Term =.. [SK_FUNC|ArgL],
	( integer_atom(RealFunctor,Functor) ->
	  true
        ; RealFunctor = Functor
        ),
	PSK_Term =.. [RealFunctor|ArgL],
	SK_Term = PSK_Term:SORT,
	append(ArgL,[SK_Term],Arg),
	Skolem =.. [Functor|Arg], !.

free_sorts([],[]) :- !.
free_sorts([_X:S|More],[_Y:S|FMore]) :-
	free_sorts(More,FMore), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Create tpos-clauses
% ----------------------
% add for every function a tpos clause 
% o  f(...,FX) <- tpos(X,FX), f(...,X). 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_tpos_eq(OUT) :-
	( eqtrafo_flag(output,ilf) ->
	    once(func_tpos(_,_,_)),
	    mmsg(OUT,['\n% BEGIN_AXIOMS'])
	;
	    fail
	),
	getval(sort_mode,V),
	func_tpos(Functor,Term,Le),
	mmsg(OUT,['\n% NAME: tpos(',Functor,')']),
	tpos_eq(V,Functor,Term,Le,Clause),
	incval(nr_output),
	msg(OUT,[Clause,'.']),
	fail.


create_tpos_eq(OUT) :-
	eqtrafo_flag(output,pretty),
	getval(sort_mode,V),
	func_tpos(Functor,Term,Le),
	tpos_eq(V,Functor,Term,Le,Clause),
	incval(nr_output),
	msg(OUT,[Clause,'.']),
	fail.

create_tpos_eq(OUT) :-
	eqtrafo_flag(output,ilf),
	func_tpos(_,_,_),
	mmsg(OUT,['\n% END_AXIOMS']).

create_tpos_eq(_OUT) :-
	eqtrafo_flag(output,pretty).


tpos_eq(no,Functor,_Term,Le,Clause) :-
	NL is Le - 1,
	length(ArgL,NL),
	append(ArgL,[FX],HeadArg),
	append(ArgL,[X],BodyArg),
	Head =.. [Functor|HeadArg],
	Body =.. [Functor|BodyArg],
	Clause = ( Head :- tpos(X,FX), Body ).

tpos_eq(yes,Functor,Term,_Le,Clause) :-
	Term =.. [Functor|Arg],
	append(ArgL,[Last],Arg),
	Last = _V:Sort,
	append(ArgL,[FX:Sort],HeadArg),
	append(ArgL,[X:Sort],BodyArg),
	Head =.. [Functor|HeadArg],
	Body =.. [Functor|BodyArg],
	Clause = ( Head :- tpos(X:Sort,FX:Sort), Body ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% post_processing
% ---------------
% 
% o remove tautologies 
% o remove double appearances of terms within one clause
%   and -(true) 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

post_processing([],[]) :- !.
post_processing([?- |Rest], [?- |Rest]) :- !.
post_processing([Clause|More],[PClause|PMore]) :-
	remove_taut(Clause,Clause,TClause),
	remove_double(TClause,PClause),
	post_processing(More,PMore), !.

remove_double([],[]).
remove_double([Lit|Rest],PRest) :-
	my_member(Lit,Rest),
	remove_double(Rest,PRest).
remove_double([- true|Rest],PRest) :-
	remove_double(Rest,PRest).
remove_double([Lit|Rest],[Lit|PRest]) :-
	remove_double(Rest,PRest).

remove_taut([],Clause,Clause).

remove_taut([-Lit|Rest],_,[]) :-
	my_member(Lit,Rest).

remove_taut([Lit|Rest],_,[]) :-
	my_member(-Lit,Rest).

remove_taut([_|Rest],Clause,TClause) :-
	remove_taut(Rest,Clause,TClause).

my_member(Term,[T|MT]) :-
	( Term == T -> 
	  true
         ; 
	  my_member(Term,MT) ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% build rule (prolog,syntax)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_rule([],[],[]).
build_rule([-C|MC],Head,[C|MB]) :-
	build_rule(MC,Head,MB).

build_rule([C|MC],[C|MH],Body) :-
	build_rule(MC,MH,Body).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% clause output
% -------------
%
% !!! eqtrafo_flag(protein_cost,on) is evaluated here !!!!
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

write_clauses(OUT,[]) :- nl(OUT), !.
write_clauses(OUT,[?- | MC]) :-
	incval(nr_output),
	write_query(OUT,MC), !.
write_clauses(OUT,[[]|MC]) :-
	write_clauses(OUT,MC), !.
write_clauses(OUT,[Clause|MC]) :-
	nl(OUT),
	numbervars(Clause,0,_),
	build_rule(Clause,HEAD,BODY),
	incval(nr_output),
	( getval(name_ok,1),eqtrafo_flag(output,ilf) ->
	    last_name(NAME),
	    msg(OUT,[NAME])
	;
	  true ),
	clause_output(OUT,HEAD,BODY),
	write_clauses(OUT,MC), !.

clause_output(OUT,[],BODY) :-
	term_list(TB,BODY),
	neg_term_list(TB,NBODY),
	( eqtrafo_flag(protein_cost,on), once(member(eqt(_,_,_),NBODY)) ->
	    write_disjunc(OUT,NBODY,cost)
	;
	    write_disjunc(OUT,NBODY,nocost)
	).
	    
clause_output(OUT,HEAD,[]) :-
	( eqtrafo_flag(protein_cost,on), once(member(eqt(_,_,_),HEAD)) ->
	    write_disjunc(OUT,HEAD,cost)
	;
	    write_disjunc(OUT,HEAD,nocost)
	).	

clause_output(OUT,HEAD,BODY) :-
	write_head(OUT,HEAD),
	( eqtrafo_flag(protein_cost,on), once(member(eqt(_,_,_),HEAD)) ->
	    write_body(OUT,BODY,cost)
	;
	    write_body(OUT,BODY,nocost)
	).

% equries have to be handled in a special way negation etc. ...
write_query(OUT,[]) :- nl(OUT).
write_query(OUT,[Q|MQ]) :-
	nl(OUT),
	write(OUT,'?- '),
	term_list(TQ,Q),
	neg_term_list(TQ,NQ),
	numbervars(NQ,0,_),
	write_body(OUT,NQ,nocost),
	write_query(OUT,MQ).

write_disjunc(OUT,[LAST],FLAG) :-
	printf(OUT,'%VDQw',[LAST]),
	( FLAG = cost, write(OUT,' # (0,0)') ; true ),
	write(OUT,'.\n'), !.
write_disjunc(OUT,[L|ML],FLAG) :-
	printf(OUT,'%VDQw',[L]),
	write(OUT,'; \n'),
	write_disjunc(OUT,ML,FLAG).

write_body(OUT,[LAST],FLAG) :-
	( eqtrafo_flag(output,pretty) ->
	    write(OUT,'\t '),printf(OUT,'%VDQw',[LAST]),
	    ( FLAG = cost, write(OUT,' # (0,0)') ; true ),
	    write(OUT,'.\n')
	;
	printf(OUT,'%VDQw',[LAST]),
	( FLAG = cost, write(OUT,' # (0,0)') ; true ), 
	write(OUT,'.')
       ).
       
write_body(OUT,[L|ML],FLAG) :-
	( eqtrafo_flag(output,pretty) ->
	    write(OUT,'\t '),
	    printf(OUT,'%VDQw',[L]), write(OUT,',\n')
	;
	   printf(OUT,'%VDQw',[L]), write(OUT,' , ')
       ),
	write_body(OUT,ML,FLAG), !.

write_head( OUT,[LAST] ) :-
	printf(OUT,'%VDQw',[LAST]),
	( eqtrafo_flag(output,pretty) -> 
	   write(OUT,' :- \n')
        ; 
	   write(OUT,' :- ')
        ),
        !.

write_head( OUT,[H|MH] ) :-
        printf(OUT,'%VDQw',[H]),
	( eqtrafo_flag(output,pretty) -> 
	    write(OUT,'; \n')
	;
	    write(OUT,'; ')
	),
	write_head(OUT,MH).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

open_list([_X|Y]) :-
	var(Y).
open_list([_X|Y]) :-
	open_list(Y).

make_open_list([E],[E|_]).
make_open_list([E|ME],[E|OME]) :-
	make_open_list(ME,OME).

safe_symetry([],[]).
safe_symetry([C|MC],[SC|SMC]) :-
	copy_term(C,SC),
	safe_symetry(MC,SMC).

myvar(V) :-
	var(V).
myvar(V:_Sort) :-
	var(V).

mynonvar(V) :-
	\+ myvar(V).

last([Last],Last).
last([_|M],Last) :-
	last(M,Last).

% This looks awfull and it is awfull 
% build a list from a term and subst. ~ with - 
% nasty ....

term_list(~U,[-U]) :- myvar(U), !.
term_list(U,[U]) :- myvar(U), !.
term_list((~A,B),[-A|More]) :-
	term_list(B,More), !.
term_list((A,B),[A|More]) :-
	term_list(B,More), !.
term_list((~A;B),[-A|More]) :-
	term_list(B,More), !.
term_list((A;B),[A|More]) :-
	term_list(B,More), !.
term_list(~A,[-A]) :- !.
term_list(A,[A]) :- !.

neg_term_list(-U,[U]) :- myvar(U), !.
neg_term_list(~U,[U]) :- myvar(U), !.
neg_term_list(U,[-U]) :- myvar(U), !.
neg_term_list((-A,B),[A|More]) :-
	neg_term_list(B,More), !.
neg_term_list((~A,B),[A|More]) :-
	neg_term_list(B,More), !.
neg_term_list((A,B),[-A|More]) :-
	neg_term_list(B,More), !.
neg_term_list(-A,[A]) :- !.
neg_term_list(~A,[A]) :- !.
neg_term_list(A,[-A]) :- !.

logo :-
	msg(["\n\n\t\to===================================o"]),
	msg(["\t\t|      EQuality TRAnsFOrmation      |"]),
	msg(["\t\t| bthomas@informatik.uni-koblenz.de |"]),
	msg(["\t\to===================================o"]),
	version_number(V),
	msg(["\t\t          ",V]).


header(OUT,File) :-
	mmsg(OUT,['%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%']),
        mmsg(OUT,['%% EQuality TRAnsFOrmation']),
        mmsg(OUT,['%% -----------------------']),
        mmsg(OUT,['%%\n%% Input: ',File,'\n%%' ]),
        mmsg(OUT,['%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%']).
 
tail(OUT) :-
	mmsg(OUT,['\n\n%%%%%%%%% EQTrafo Statistics %%%%%%%%%%%%%%%%']),
        findall((M=FL),eqtrafo_flag(M,FL),FLAGS),
        findall(F,p_funcs(F),PF),
	findall(C,def_cons(C),DCC),
	flatten(DCC,DC),
	c_funcs(CFF),
	remove_doubles(CFF,CF),
	getval(nr_input,NR_INPUT_CL),
	getval(nr_output,NR_OUTPUT_CL),
	mmsg(OUT,['%% flags: ',FLAGS]),
        mmsg(OUT,['%% partial functions: ']),
 	list_msg(OUT,PF),
	( ( DC = [scan] ; DC = [noscan] ) ->
	    mmsg(OUT,['%% constructor mode:']),
            list_mmsg(OUT,DC)
          ; 
            mmsg(OUT,['%% defined constructors: ', DC])
        ),
        mmsg(OUT,['%% top function symbols: ']),
	list_mmsg(OUT,CF),
        mmsg(OUT,['%% input clauses: ',NR_INPUT_CL]),
        mmsg(OUT,['%% output clauses: ',NR_OUTPUT_CL]),
        mmsg(OUT,['%%%%%%%%%%%%%%%%%%%%%%%% come back soon =:-]']).


%% be careful with this !!!
%% numbervars may do some strange things ...
msg([]) :- nl,flush(stdout),!.
msg([E|ME]) :-
	numbervars(E,0,_),
	write(E),
	msg(ME), !.

msg(S,[]) :- nl(S),!.
msg(S,[E|ME]) :-
	numbervars(E,0,_),
	printf(S,'%VDQw',[E]),
%	write(S,E),
	msg(S,ME), !.

mmsg(S,[]) :- nl(S),!.
mmsg(S,[E|ME]) :-
	numbervars(E,0,_),
	write(S,E),
	mmsg(S,ME), !.


list_msg(_S,[]).
list_msg(S,[T|MT]) :-
    msg(S,['%% \t',T]),
    list_msg(S,MT).

list_mmsg(_S,[]).
list_mmsg(S,[T|MT]) :-
    mmsg(S,['%% \t',T]),
    list_mmsg(S,MT).

remove_doubles([],[]).
remove_doubles([F|M],[F|NM]) :-
	help_remove_doubles(F,M,NewMore),
	remove_doubles(NewMore,NM).

help_remove_doubles(_,[],[]).
help_remove_doubles(F,[F|M],NM) :-
	!,
	help_remove_doubles(F,M,NM).
help_remove_doubles(F,[G|M],[G|NM]) :-
	help_remove_doubles(F,M,NM).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% fun stuff
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_blinky :-
	setval(blinky,99), flush(stdout), !.

stop_blinky :-
	clean,
	write(' '),
	flush(stdout), !.

blinky :-
	flush(stdout),
	getval(blinky,V),
	( V=99, write('-'), setval(blinky,0) ;
	  V=0 , clean, write('\\'), setval(blinky,1) ;
	  V=1, clean, write('|'), setval(blinky,2) ;
	  V=2, clean, write('/'), setval(blinky,3) ;
	  V=3, clean, write('-'), setval(blinky,0) ), 
	 flush(stdout), !.
  
clean :- write('\b'), !.

usage :-
	nl,
	version_number(V),
	write('EQTRAFO '),
	writeln(V),
	writeln('-------------------'),
	writeln('\nusage: eqtrafo filename.tme'),
	writeln('omit extender .tme'),
	writeln('\nor\n\nusage: eqtrafo -h'),
	writeln('for help menu'), 
	halt.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Help Mode =:-]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


help_eqtrafo :-
	help_eqtrafo(help),
	help_eqtrafo2.

help_eqtrafo(quit).
help_eqtrafo(help) :-
	msg(stdout,["\n\n\n\t\t *** EQTRAFO help mode***\n\n"]),
	msg(stdout,["help is available for:"]),
	msg(stdout,["eqtrafo_flag\tconstructors\tpartial_functions"]),
	msg(stdout,["\ntype in topic followed by a full stop ('.')"]),
	msg(stdout,["'help.' shows this list\t 'quit.' ends help mode\n\n"]),
	!.

help_eqtrafo(eqtrafo_flag) :-
	msg(stdout,["\nSYNTAX:  eqtrafo_flag(mode,OPTION).\n\nwhere OPTION is:\n\n\tlinear:\n\tuse special transformation to break down\n\texponential complexity of symetrical variants\n\n\texp (default):\n\tnumber of clauses may grow exponentially."]),
	msg(stdout,["\nSYNTAX:  eqtrafo_flag(output,OPTION).\n\nwhere OPTION is:\n\n\tilf:\n\tspecial output that can be processed by ilf\n\n\tpretty (default):\n\tthe standard output style."]),
	msg(stdout,["\nSYNTAX:  eqtrafo_flag(protein_cost,OPTION).\n\nwhere OPTION is:\n\n\ton:\n\tadds a special information for protein\n\tto those clauses that contain\n\t 'eqt/3' predicates in its head (linear mode)\n\n\toff (default):\n\tthe standard syntax."]), !.

help_eqtrafo(partial_functions) :-
	msg(stdout,["\nSYNTAX:  partial_functions: (F1,F2,...,Fn).\n\n\t Normally every function is considered to be total.\n\tFor this reason there are always special\n\tskolem terms added to the transformed clause set.\n\tIf you want to declare special functions\n\tto be partial, you just have to add their functor\n\tname (F1 to Fn) to the list of this eqtrafo command.\n\tIf a function is declared partial,\n\tthere won't be any skolem terms added to the\n\ttransformed clause set.\n"]), !.



help_eqtrafo(constructors) :-
	msg(stdout,["\nSYNTAX:  constructors: (OPTION).\n\nwhere OPTION is:\n\n\tscan:\n\t Only top function symbols are NO constructors, means \n\tthey are pulled out, of every term. Constants and nested \n\tterms (functions) stay where they are as long as they are\n\tnot part of any equation, top functions ...\n\tIf eqtrafo detects an equations X = xxx , it automatically\n\tchanges its behaviour as if it is running the noscan mode."]),
	msg(stdout,["\n\tnoscan (default) :\n\t Every constant, function is pulled out. This means, every\n\targument of a function or predicate is pulled out. The\n\tInput clause set is totally flattened."]),
	msg(stdout,["\n\tC1,C2,...,CN:\n\t Only the specified Functors (C1 to Cn) are considered\n\tto be constructors. This means they stay where they are all\n\tother constants, functions are pulled out."]), !.


help_eqtrafo(benno) :- 
	msg(stdout,["\nHey ! Have a nice day =;-]\n"]), !.

help_eqtrafo(X) :-
	msg(stdout,["\nSORRY, no help available for topic *",X,"* !\n"]), !.

help_eqtrafo2 :-
	repeat,
	write("topic ? >"),
	read(stdin,Topic),
	( Topic \= quit ->
	    help_eqtrafo(Topic)
	;
	     true
	),
	Topic == quit.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  end of eqtrafo.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

