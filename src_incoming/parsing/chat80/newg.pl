sentence(A, B, E, C, G) :-
	declarative(A, B, D, C, F),
	terminator('.', D, E, F, G).
sentence(A, B, E, C, G) :-
	wh_question(A, B, D, C, F),
	terminator(?, D, E, F, G).
sentence(A, B, E, C, G) :-
	yn_question(A, B, D, C, F),
	terminator(?, D, E, F, G).
sentence(A, B, E, C, G) :-
	imperative(A, B, D, C, F),
	terminator(!, D, E, F, G).


declarative(decl(A), B, C, D, E) :-
	s(A, _, B, C, D, E).


wh_question(whq(A, F), B, H, C, J) :-
	variable_q(A,
		   _,
		   D,
		   E,
		   B,
		   G,
		   C,
		   I),
	question(D, E, F, G, H, I, J).


np(B, C, D, E, F, G, H, A, A, I, J) :-
	virtual(np(B, C, D, E, F, G, H),
		I,
		J).
np(np(A, C, []), A, J, def, _, B, H, D, E, F, G) :-
	is_pp(B),
	pers_pron(C, A, I, D, E, F, G),
	empty(H),
	role(I, decl, J).
np(np(A, C, E), A, _, D, I, B, L, F, N, G, P) :-
	is_pp(B),
	np_head(C,
		A,
		D+H,
		J,
		E,
		F,
		M,
		G,
		O),
	np_all(K),
	np_compls(H,
		  A,
		  I,
		  J,
		  K,
		  L,
		  M,
		  N,
		  O,
		  P).
np(part(B, H), 3+C, _, indef, J, A, L, D, N, E, P) :-
	is_pp(A),
	determiner(B, C, indef, D, F, E, G),
	terminal(of, F, M, G, O),
	s_all(K),
	prep_case(I),
	np(H,
	   3+plu,
	   I,
	   def,
	   J,
	   K,
	   L,
	   M,
	   N,
	   O,
	   P).


variable_q(C, A, E, B, F, G, H, x(gap, nonterminal, np(D, A, B, _, _, J, K), I)) :-
	whq(C, A, D, E, F, G, H, I),
	trace80(J, K).
variable_q(D, E, compl, M, B, H, C, x(gap, nonterminal, pp(pp(A, F), compl, K, L), J)) :-
	prep(A, B, G, C, I),
	whq(D, E, F, _, G, H, I, J),
	trace80(K, L),
	compl_case(M).
variable_q(B, A, compl, K, E, F, G, x(gap, nonterminal, adv_phrase(pp(C, np(A, np_head(int_det(B), [], D), [])), I, J), H)) :-
	context_pron(C, D, E, F, G, H),
	trace80(I, J),
	verb_case(K).
variable_q(A, _, compl, J, B, F, C, x(gap, nonterminal, pred(adj, value(D, wh(A)), I), H)) :-
	terminal(how, B, E, C, G),
	adj(quant, D, E, F, G, H),
	empty(I),
	verb_case(J).


pp(B, C, D, E, A, A, F, G) :-
	virtual(pp(B, C, D, E), F, G).
pp(pp(A, D), F, G, H, B, J, C, L) :-
	prep(A, B, I, C, K),
	prep_case(E),
	np(D,
	   _,
	   E,
	   _,
	   F,
	   G,
	   H,
	   I,
	   J,
	   K,
	   L).


adv_phrase(B, C, D, A, A, E, F) :-
	virtual(adv_phrase(B, C, D), E, F).
adv_phrase(pp(A, D), E, F, B, H, C, J) :-
	loc_pred(A, B, G, C, I),
	pp(pp(prep(of), D),
	   compl,
	   E,
	   F,
	   G,
	   H,
	   I,
	   J).


pred(B, C, D, A, A, E, F) :-
	virtual(pred(B, C, D), E, F).
pred(_, A, B, C, D, E, F) :-
	adj_phrase(A, B, C, D, E, F).
pred(neg, A, C, D, E, F, G) :-
	s_all(B),
	pp(A, compl, B, C, D, E, F, G).
pred(_, A, C, D, E, F, G) :-
	s_all(B),
	adv_phrase(A, B, C, D, E, F, G).


whq(A, B, E, undef, C, H, D, J) :-
	int_det(A, B, C, G, D, I),
	s_all(F),
	np(E,
	   B,
	   _,
	   _,
	   subj,
	   F,
	   _,
	   G,
	   H,
	   I,
	   J).
whq(B, 3+A, np(3+A, wh(B), []), C, D, E, F, G) :-
	int_pron(C, D, E, F, G).


int_det(A, 3+B, C, D, E, F) :-
	whose(A, B, C, D, E, F).
int_det(A, 3+B, C, D, E, F) :-
	int_art(A, B, C, D, E, F).


np_head0(B, C, D, A, A, E, F) :-
	virtual(np_head0(B, C, D), E, F).
np_head0(name(A), 3+sin, def+proper, B, C, D, E) :-
	name(A, B, C, D, E).
np_head0(np_head(A, F, I), 3+B, C+common, D, K, E, M) :-
	determiner(A, B, C, D, G, E, H),
	adjs(F, G, J, H, L),
	noun(I, B, J, K, L, M).
np_head0(A, B, def+proper, C, D, E, x(nogap, nonterminal, gen_marker, F)) :-
	poss_pron(A, B, C, D, E, F).
np_head0(np_head(A, [], B), 3+sin, indef+common, C, D, E, F) :-
	quantifier_pron(A, B, C, D, E, F).


gen_marker(A, A, B, C) :-
	virtual(gen_marker, B, C).
gen_marker(A, D, B, F) :-
	terminal('\'', A, C, B, E),
	an_s(C, D, E, F).


whose(A, B, C, D, E, x(nogap, nonterminal, np_head0(wh(A), B, proper), x(nogap, nonterminal, gen_marker, F))) :-
	terminal(whose, C, D, E, F).


question(A, B, C, D, E, F, G) :-
	subj_question(A),
	role(subj, _, B),
	s(C, _, D, E, F, G).
question(A, B, E, C, G, D, I) :-
	fronted_verb(A, B, C, F, D, H),
	s(E, _, F, G, H, I).


det(B, C, D, A, A, E, F) :-
	virtual(det(B, C, D), E, F).
det(det(G), F, H, A, B, C, D) :-
	terminal(E, A, B, C, D),
	det(E, F, G, H).
det(generic, _, generic, A, A, B, B).


int_art(B, A, D, E, F, x(nogap, nonterminal, det(C, A, def), G)) :-
	int_art(B, A, C, D, E, F, G).


subj_qustion(subj).


subj_question(undef).


yn_question(q(C), A, E, B, G) :-
	fronted_verb(nil, _, A, D, B, F),
	s(C, _, D, E, F, G).


verb_form(B, C, D, E, A, A, F, G) :-
	virtual(verb_form(B, C, D, E), F, G).
verb_form(F, G, H, _, A, B, C, D) :-
	terminal(E, A, B, C, D),
	verb_form(E, F, G, H).


neg(B, C, A, A, D, E) :-
	virtual(neg(B, C), D, E).
neg(aux+_, neg, A, B, C, D) :-
	terminal(not, A, B, C, D).
neg(_, pos, A, A, B, B).


fronted_verb(F, H, D, K, E, x(gap, nonterminal, verb_form(A, B, C, G), x(nogap, nonterminal, neg(_, I), M))) :-
	verb_form(A,
		  B,
		  C,
		  _,
		  D,
		  J,
		  E,
		  L),
	verb_type(A, aux+_),
	role(F, G, H),
	neg(_, I, J, K, L, M).


imperative(imp(C), A, E, B, G) :-
	imperative_verb(A, D, B, F),
	s(C, _, D, E, F, G).


imperative_verb(B, C, D, x(nogap, terminal, you, x(nogap, nonterminal, verb_form(A, imp+fin, 2+sin, main), E))) :-
	verb_form(A, inf, _, _, B, C, D, E).


s(s(A, D, J, P), S, B, U, C, W) :-
	subj(A, E, F, B, G, C, H),
	verb(D,
	     E,
	     F,
	     I,
	     G,
	     L,
	     H,
	     M),
	empty(K),
	s_all(N),
	verb_args(F,
		  I,
		  J,
		  K,
		  O,
		  L,
		  T,
		  M,
		  V),
	minus(N, O, Q),
	plus(N, O, R),
	verb_mods(P,
		  Q,
		  R,
		  S,
		  T,
		  U,
		  V,
		  W).


subj(there, _, _+be, A, B, C, D) :-
	terminal(there, A, B, C, D).
subj(A, B, _, E, F, G, H) :-
	s_all(D),
	subj_case(C),
	np(A,
	   B,
	   C,
	   _,
	   subj,
	   D,
	   _,
	   E,
	   F,
	   G,
	   H).


np_head(G, H, I, J, K, A, M, B, O) :-
	np_head0(C, D, E, A, L, B, N),
	possessive(C,
		   D,
		   E,
		   F,
		   F,
		   G,
		   H,
		   I,
		   J,
		   K,
		   L,
		   M,
		   N,
		   O).


np_compls(proper, _, _, [], _, C, A, A, B, B) :-
	empty(C).
np_compls(common, A, B, C, D, K, F, M, G, O) :-
	np_all(E),
	np_mods(A,
		B,
		H,
		C,
		D,
		I,
		E,
		J,
		F,
		L,
		G,
		N),
	relative(A,
		 H,
		 I,
		 J,
		 K,
		 L,
		 M,
		 N,
		 O).


possessive(I, H, _, [], J, L, M, N, O, P, A, R, B, T) :-
	gen_case(A, C, B, D),
	np_head0(E, F, G, C, Q, D, S),
	possessive(E,
		   F,
		   G,
		   K,
		   [pp(poss, np(H, I, J))|K],
		   L,
		   M,
		   N,
		   O,
		   P,
		   Q,
		   R,
		   S,
		   T).
possessive(A, B, C, D, E, A, B, C, D, E, F, F, G, G).


gen_case(A, B, C, x(nogap, terminal, the, D)) :-
	gen_marker(A, B, C, D).


an_s(A, B, C, D) :-
	terminal(s, A, B, C, D).
an_s(A, A, B, B).


determiner(A, B, C, D, E, F, G) :-
	det(A, B, C, D, E, F, G).
determiner(A, B, C, D, E, F, G) :-
	quant_phrase(A, B, C, D, E, F, G).


quant_phrase(quant(A, E), F, B, C, H, D, J) :-
	quant(A, B, C, G, D, I),
	number(E, F, G, H, I, J).


quant(A, indef, B, H, C, J) :-
	neg_adv(D, A, B, E, C, F),
	comp_adv(D, E, G, F, I),
	terminal(than, G, H, I, J).
quant(H, indef, A, D, B, F) :-
	terminal(at, A, C, B, E),
	sup_adv(G, C, D, E, F),
	sup_op(G, H).
quant(the, def, A, B, C, D) :-
	terminal(the, A, B, C, D).
quant(same, indef, A, A, B, B).


neg_adv(A, not+A, B, C, D, E) :-
	terminal(not, B, C, D, E).
neg_adv(A, A, B, B, C, C).


sup_op(least, not+less).
sup_op(most, not+more).


np_mods(A, B, J, [C|K], D, M, _, O, E, Q, F, S) :-
	np_mod(A,
	       B,
	       C,
	       D,
	       H,
	       E,
	       P,
	       F,
	       R),
	trace80(G),
	plus(G, H, I),
	minus(D, I, L),
	plus(H, D, N),
	np_mods(A,
		B,
		J,
		K,
		L,
		M,
		N,
		O,
		P,
		Q,
		R,
		S).
np_mods(_, _, A, A, B, B, C, C, D, D, E, E).


np_mod(_, B, A, C, D, E, F, G, H) :-
	pp(A, B, C, D, E, F, G, H).
np_mod(A, _, B, C, D, E, F, G, H) :-
	reduced_relative(A,
			 B,
			 C,
			 D,
			 E,
			 F,
			 G,
			 H).


verb_mods([A|H], B, _, K, C, M, D, O) :-
	verb_mod(A, B, F, C, L, D, N),
	trace80(E),
	plus(E, F, G),
	minus(B, G, I),
	plus(F, B, J),
	verb_mods(H,
		  I,
		  J,
		  K,
		  L,
		  M,
		  N,
		  O).
verb_mods([], _, A, A, B, B, C, C).


verb_mod(A, B, C, D, E, F, G) :-
	adv_phrase(A, B, C, D, E, F, G).
verb_mod(B, A, G, C, D, E, F) :-
	is_adv(A),
	adverb(B, C, D, E, F),
	empty(G).
verb_mod(A, B, C, D, E, F, G) :-
	pp(A, compl, B, C, D, E, F, G).


adjs([A|D], B, F, C, H) :-
	pre_adj(A, B, E, C, G),
	adjs(D, E, F, G, H).
adjs([], A, A, B, B).


pre_adj(A, B, C, D, E) :-
	adj(_, A, B, C, D, E).
pre_adj(A, B, C, D, E) :-
	sup_phrase(A, B, C, D, E).


sup_phrase(sup(most, A), B, C, D, E) :-
	sup_adj(A, B, C, D, E).
sup_phrase(sup(_, C), A, E, B, G) :-
	sup_adv(_, A, D, B, F),
	adj(quant, C, D, E, F, G).


comp_phrase(comp(A, B, E), H, C, J, D, L) :-
	comp(A, B, C, I, D, K),
	np_no_trace(G),
	prep_case(F),
	np(E,
	   _,
	   F,
	   _,
	   compl,
	   G,
	   H,
	   I,
	   J,
	   K,
	   L).


comp(A, D, B, H, C, J) :-
	comp_adv(A, B, E, C, F),
	adj(quant, D, E, G, F, I),
	terminal(than, G, H, I, J).
comp(more, A, B, E, C, G) :-
	rel_adj(A, B, D, C, F),
	terminal(than, D, E, F, G).
comp(same, C, A, G, B, I) :-
	terminal(as, A, D, B, E),
	adj(quant, C, D, F, E, H),
	terminal(as, F, G, H, I).


relative(B, [C], A, _, D, E, F, G, H) :-
	is_pred(A),
	rel_conj(B,
		 _,
		 C,
		 D,
		 E,
		 F,
		 G,
		 H).
relative(_, [], _, A, A, B, B, C, C).


rel_conj(A, D, F, H, B, J, C, L) :-
	rel(A, E, G, B, I, C, K),
	rel_rest(A,
		 D,
		 E,
		 F,
		 G,
		 H,
		 I,
		 J,
		 K,
		 L).


rel_rest(F, A, B, C, _, I, D, K, E, M) :-
	conj(A,
	     G,
	     B,
	     H,
	     C,
	     D,
	     J,
	     E,
	     L),
	rel_conj(F,
		 G,
		 H,
		 I,
		 J,
		 K,
		 L,
		 M).
rel_rest(_, _, A, A, B, B, C, C, D, D).


rel(C, rel(D, G), L, A, N, B, P) :-
	island(A, E, B, F),
	variable(C, D, E, H, F, I),
	s(G, J, H, M, I, O),
	trace80(K),
	minus(J, K, L),
	dnalsi(M, N, O, P).


variable(A, B, C, D, E, x(gap, nonterminal, np(np(A, wh(B), []), A, _, _, _, G, H), F)) :-
	terminal(that, C, D, E, F),
	trace80(G, H).
variable(B, A, F, G, H, x(gap, nonterminal, np(C, D, E, _, _, J, K), I)) :-
	wh(A,
	   B,
	   C,
	   D,
	   E,
	   F,
	   G,
	   H,
	   I),
	trace80(J, K).
variable(E, D, B, H, C, x(gap, nonterminal, pp(pp(A, F), compl, K, L), J)) :-
	prep(A, B, G, C, I),
	wh(D,
	   E,
	   F,
	   _,
	   M,
	   G,
	   H,
	   I,
	   J),
	trace80(K, L),
	compl_case(M).


wh(B, A, np(A, wh(B), []), A, H, C, D, E, F) :-
	rel_pron(G, C, D, E, F),
	role(G, decl, H).
wh(H, I, np(A, B, [pp(E, J)]), A, _, C, L, D, N) :-
	np_head0(B,
		 A,
		 _+common,
		 C,
		 F,
		 D,
		 G),
	prep(E, F, K, G, M),
	wh(H,
	   I,
	   J,
	   _,
	   _,
	   K,
	   L,
	   M,
	   N).
wh(A, B, E, F, G, C, J, D, L) :-
	whose(A, B, C, I, D, K),
	s_all(H),
	np(E,
	   F,
	   G,
	   def,
	   subj,
	   H,
	   _,
	   I,
	   J,
	   K,
	   L).


reduced_relative(B, C, A, D, E, F, G, H) :-
	is_pred(A),
	reduced_rel_conj(B,
			 _,
			 C,
			 D,
			 E,
			 F,
			 G,
			 H).


reduced_rel_conj(A, D, F, H, B, J, C, L) :-
	reduced_rel(A, E, G, B, I, C, K),
	reduced_rel_rest(A,
			 D,
			 E,
			 F,
			 G,
			 H,
			 I,
			 J,
			 K,
			 L).


reduced_rel_rest(F, A, B, C, _, I, D, K, E, M) :-
	conj(A,
	     G,
	     B,
	     H,
	     C,
	     D,
	     J,
	     E,
	     L),
	reduced_rel_conj(F,
			 G,
			 H,
			 I,
			 J,
			 K,
			 L,
			 M).
reduced_rel_rest(_, _, A, A, B, B, C, C, D, D).


reduced_rel(C, reduced_rel(D, G), L, A, N, B, P) :-
	island(A, E, B, F),
	reduced_wh(C, D, E, H, F, I),
	s(G, J, H, M, I, O),
	trace80(K),
	minus(J, K, L),
	dnalsi(M, N, O, P).


reduced_wh(A, B, D, I, E, x(nogap, nonterminal, np(np(A, wh(B), []), A, N, _, _, L, M), x(nogap, nonterminal, verb_form(be, pres+fin, A, main), x(nogap, nonterminal, neg(_, C), x(nogap, nonterminal, pred(C, F, G), K))))) :-
	neg(_, C, D, H, E, J),
	pred(C, F, G, H, I, J, K),
	trace80(L, M),
	subj_case(N).
reduced_wh(A, B, F, G, H, x(nogap, nonterminal, np(np(A, wh(B), []), A, L, _, _, J, K), x(nogap, nonterminal, verb(C, _, D, E), I))) :-
	participle(C, D, E, F, G, H, I),
	trace80(J, K),
	subj_case(L).
reduced_wh(A, B, I, J, K, x(nogap, nonterminal, np(E, F, C, G, _, M, N), x(gap, nonterminal, np(np(A, wh(B), []), A, D, _, _, O, P), L))) :-
	s_all(H),
	subj_case(C),
	verb_case(D),
	np(E,
	   F,
	   _,
	   G,
	   subj,
	   H,
	   _,
	   I,
	   J,
	   K,
	   L),
	trace80(M, N),
	trace80(O, P).


verb(B, C, D, E, A, A, F, G) :-
	virtual(verb(B, C, D, E), F, G).
verb(verb(L, A, B+fin, M, H), C, R, A, D, O, E, Q) :-
	verb_form(F,
		  B+fin,
		  C,
		  K,
		  D,
		  I,
		  E,
		  J),
	verb_type(F, G),
	neg(G, H, I, N, J, P),
	rest_verb(K,
		  F,
		  L,
		  A,
		  M,
		  N,
		  O,
		  P,
		  Q),
	verb_type(L, R).


rest_verb(aux, have, D, E, [perf|F], A, H, B, J) :-
	verb_form(C,
		  past+part,
		  _,
		  _,
		  A,
		  G,
		  B,
		  I),
	have(C, D, E, F, G, H, I, J).
rest_verb(aux, be, E, F, G, A, I, B, K) :-
	verb_form(D,
		  C,
		  _,
		  _,
		  A,
		  H,
		  B,
		  J),
	be(C,
	   D,
	   E,
	   F,
	   G,
	   H,
	   I,
	   J,
	   K).
rest_verb(aux, do, A, active, [], B, C, D, E) :-
	verb_form(A, inf, _, _, B, C, D, E).
rest_verb(main, A, A, active, [], B, B, C, C).


have(be, E, F, G, A, I, B, K) :-
	verb_form(D,
		  C,
		  _,
		  _,
		  A,
		  H,
		  B,
		  J),
	be(C,
	   D,
	   E,
	   F,
	   G,
	   H,
	   I,
	   J,
	   K).
have(A, A, active, [], B, B, C, C).


be(past+part, A, A, passive, [], B, B, C, C).
be(pres+part, A, B, C, [prog], D, E, F, G) :-
	passive(A, B, C, D, E, F, G).


passive(be, A, passive, B, C, D, E) :-
	verb_form(A,
		  past+part,
		  _,
		  _,
		  B,
		  C,
		  D,
		  E),
	verb_type(A, F),
	passive(F).
passive(A, A, active, B, B, C, C).


participle(verb(E, A, inf, K, B), L, A, C, G, D, I) :-
	neg(_, B, C, F, D, H),
	verb_form(E,
		  J,
		  _,
		  _,
		  F,
		  G,
		  H,
		  I),
	participle(J, A, K),
	verb_type(E, L).


passive(_+trans).
passive(_+ditrans).


participle(pres+part, active, [prog]).
participle(past+part, passive, []).


dnalsi(A, A, B, C) :-
	virtual(dnalsi, B, C).


island(A, A, B, x(gap, nonterminal, dnalsi, B)).


verb_args(_+D, E, A, G, H, B, J, C, L) :-
	advs(A, F, _, B, I, C, K),
	verb_args(D,
		  E,
		  F,
		  G,
		  H,
		  I,
		  J,
		  K,
		  L).
verb_args(trans, active, [arg(dir, A)], _, B, C, D, E, F) :-
	verb_arg(np, A, B, C, D, E, F).
verb_args(ditrans, _, [arg(D, A)|E], _, G, B, I, C, K) :-
	verb_arg(np, A, F, B, H, C, J),
	object(D,
	       E,
	       F,
	       G,
	       H,
	       I,
	       J,
	       K).
verb_args(be, _, [void], A, A, B, C, D, E) :-
	terminal(there, B, C, D, E).
verb_args(be, _, [arg(pred, A)], _, B, C, D, E, F) :-
	pred_conj(_, A, B, C, D, E, F).
verb_args(be, _, [arg(dir, A)], _, B, C, D, E, F) :-
	verb_arg(np, A, B, C, D, E, F).
verb_args(have, active, [arg(dir, A)], _, B, C, D, E, F) :-
	verb_arg(np, A, B, C, D, E, F).
verb_args(D, _, [], A, A, B, B, C, C) :-
	no_args(D).


object(G, C, B, I, E, K, F, M) :-
	adv(A),
	minus(A, B, D),
	advs(C, H, D, E, J, F, L),
	obj(G,
	    H,
	    B,
	    I,
	    J,
	    K,
	    L,
	    M).


obj(ind, [arg(dir, A)], _, B, C, D, E, F) :-
	verb_arg(np, A, B, C, D, E, F).
obj(dir, [], A, A, B, B, C, C).


pred_conj(C, E, G, A, I, B, K) :-
	pred(_, D, F, A, H, B, J),
	pred_rest(C,
		  D,
		  E,
		  F,
		  G,
		  H,
		  I,
		  J,
		  K).


pred_rest(A, B, C, _, H, D, J, E, L) :-
	conj(A,
	     F,
	     B,
	     G,
	     C,
	     D,
	     I,
	     E,
	     K),
	pred_conj(F, G, H, I, J, K, L).
pred_rest(_, A, A, B, B, C, C, D, D).


verb_arg(np, A, D, E, F, G, H) :-
	s_all(C),
	verb_case(B),
	np(A,
	   _,
	   B,
	   _,
	   compl,
	   C,
	   D,
	   E,
	   F,
	   G,
	   H).


advs([B|E], F, A, C, H, D, J) :-
	is_adv(A),
	adverb(B, C, G, D, I),
	advs(E, F, A, G, H, I, J).
advs(A, A, _, B, B, C, C).


adj_phrase(A, F, B, C, D, E) :-
	adj(_, A, B, C, D, E),
	empty(F).
adj_phrase(A, B, C, D, E, F) :-
	comp_phrase(A, B, C, D, E, F).


no_args(trans).
no_args(ditrans).
no_args(intrans).


conj(conj(A, D), conj(A, E), B, C, conj(A, B, C), F, G, H, I) :-
	conj(A, D, E, F, G, H, I).


noun(F, G, A, B, C, D) :-
	terminal(E, A, B, C, D),
	noun_form(E, F, G).


adj(F, adj(A), B, C, D, E) :-
	terminal(A, B, C, D, E),
	adj(A, F).


prep(prep(A), B, C, D, E) :-
	terminal(A, B, C, D, E),
	prep(A).


rel_adj(adj(F), A, B, C, D) :-
	terminal(E, A, B, C, D),
	rel_adj(E, F).


sup_adj(adj(F), A, B, C, D) :-
	terminal(E, A, B, C, D),
	sup_adj(E, F).


comp_adv(less, A, B, C, D) :-
	terminal(less, A, B, C, D).
comp_adv(more, A, B, C, D) :-
	terminal(more, A, B, C, D).


sup_adv(least, A, B, C, D) :-
	terminal(least, A, B, C, D).
sup_adv(most, A, B, C, D) :-
	terminal(most, A, B, C, D).


rel_pron(F, A, B, C, D) :-
	terminal(E, A, B, C, D),
	rel_pron(E, F).


name(C, A, E, B, G) :-
	opt_the(A, D, B, F),
	terminal(C, D, E, F, G),
	name(C).


int_art(A, plu, quant(same, wh(A)), B, E, C, G) :-
	terminal(how, B, D, C, F),
	terminal(many, D, E, F, G).
int_art(F, G, H, A, B, C, D) :-
	terminal(E, A, B, C, D),
	int_art(E, F, G, H).


int_pron(F, A, B, C, D) :-
	terminal(E, A, B, C, D),
	int_pron(E, F).


adverb(adv(A), B, C, D, E) :-
	terminal(A, B, C, D, E),
	adverb(A).


poss_pron(pronoun(F), G+H, A, B, C, D) :-
	terminal(E, A, B, C, D),
	poss_pron(E, F, G, H).


pers_pron(pronoun(F), G+H, I, A, B, C, D) :-
	terminal(E, A, B, C, D),
	pers_pron(E, F, G, H, I).


quantifier_pron(F, G, A, B, C, D) :-
	terminal(E, A, B, C, D),
	quantifier_pron(E, F, G).


context_pron(prep(in), place, A, B, C, D) :-
	terminal(where, A, B, C, D).
context_pron(prep(at), time, A, B, C, D) :-
	terminal(when, A, B, C, D).


number(nb(F), G, A, B, C, D) :-
	terminal(E, A, B, C, D),
	number(E, F, G).


terminator(F, A, B, C, D) :-
	terminal(E, A, B, C, D),
	terminator(E, F).


opt_the(A, A, B, B).
opt_the(A, B, C, D) :-
	terminal(the, A, B, C, D).


conj(_, list, list, A, B, C, D) :-
	terminal(',', A, B, C, D).
conj(A, list, end, B, C, D, E) :-
	terminal(A, B, C, D, E),
	conj(A).


loc_pred(F, A, B, C, D) :-
	terminal(E, A, B, C, D),
	loc_pred(E, F).


