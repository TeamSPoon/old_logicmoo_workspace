:- dynamic_multifile_exported moo:longitude/2.
:- dynamic_multifile_exported moo:latitude/2.

/*

 _____________________________________
|	Copyright (C) 1982						  |
|									  |
|	David Warren,							  |
|		SRI International, 333 Ravenswood Ave., Menlo Park,	  |
|		California 94025, USA;					  |
|									  |
|	Fernando Pereira,						  |
|		Dept. of Architecture, University of Edinburgh,		  |
|		20 Chambers St., Edinburgh EH1 1JZ, Scotland		  |
|									  |
|	This program may be used, copied, altered or included in other	  |
|	programs only for academic purposes and provided that the	  |
|	authorship of the initial program is aknowledged.		  |
|	Use for commercial purposes without the previous written 	  |
|	agreement of the authors is forbidden.				  |
|_____________________________________|

*/

must_test_80([what, rplans, are, there, ?], [sent([what, rplans, are, there, ?]), parse(whq(feature&rplan-B, s(np(3+plu, np_head(int_det(feature&rplan-B), [], rplan), []), verb(be, active, pres+fin, [], pos), [void], []))), sem((answer([A]):-rplan(A), A^true)), qplan((answer([B]):-rplan(B), B^true)), answers([amazon, amu_darya, amur, brahmaputra, colorado, congo_rplan, cubango, smart_plan_1, don, elbe, euphrates, ganges, hwang_ho, indus, irrawaddy, lena, limpopo, mackenzie, mekong, mississippi, murray, niger_rplan, nile, ob, oder, orange, orinoco, parana, rhine, rhone, rio_grande, salween, senegal_rplan, tagus, vistula, volga, volta, yangtze, yenisei, yukon, zambesi])],[time(0.0)]).
must_test_80([does, afghanistan, comply_with, china, ?], [sent([does, afghanistan, comply_with, china, ?]), parse(q(s(np(3+sin, name(afghanistan), []), verb(comply_with, active, pres+fin, [], pos), [arg(dir, np(3+sin, name(china), []))], []))), sem((answer([]):-complys_with(afghanistan, china))), qplan((answer([]):-{complys_with(afghanistan, china)})), answers([true])],[time(0.0)]).
must_test_80([what, is, the, consequent, of, upper_volta, ?], [sent([what, is, the, consequent, of, upper_volta, ?]), parse(whq(feature&assertion-B, s(np(3+sin, wh(feature&assertion-B), []), verb(be, active, pres+fin, [], pos), [arg(dir, np(3+sin, np_head(det(the(sin)), [], consequent), [pp(prep(of), np(3+sin, name(upper_volta), []))]))], []))), sem((answer([A]):-consequent(upper_volta, A))), qplan((answer([A]):-consequent(upper_volta, A))), answers([ouagadougou])],[time(0.0010000000000000009)]).
must_test_80([where, is, the, largest, mstate, ?], [sent([where, is, the, largest, mstate, ?]), parse(whq(feature&concept&A-B, s(np(3+sin, np_head(det(the(sin)), [sup(most, adj(large))], mstate), []), verb(be, active, pres+fin, [], pos), [arg(pred, pp(prep(in), np(_, np_head(int_det(feature&concept&A-B), [], concept), [])))], []))), sem((answer([A]):-B^ (C^ (setof(D:E, (mstate(E), utility_value(E, D)), C), aggregate(max, C, B)), concept(A), in(B, A)))), qplan((answer([F]):-E^D^ (setof(C:B, (mstate(B), utility_value(B, C)), D), aggregate(max, D, E), in(E, F), {concept(F)}))), answers([logicbook, northern_asia])],[time(0.0009999999999999731)]).
must_test_80([which, mstates, are, social, ?], [sent([which, mstates, are, social, ?]), parse(whq(feature&concept&mstate-B, s(np(3+plu, np_head(int_det(feature&concept&mstate-B), [], mstate), []), verb(be, active, pres+fin, [], pos), [arg(pred, adj(social))], []))), sem((answer([A]):-mstate(A), social(A))), qplan((answer([A]):-social(A), {mstate(A)})), answers([albania, andorra, austria, belgium, bulgaria, cyprus, czechoslovakia, denmark, east_germany, eire, finland, france, greece, hungary, iceland, italy, liechtenstein, luxembourg, malta, monaco, netherlands, norway, poland, portugal, romania, san_marino, spain, sweden, switzerland, united_kingdom, west_germany, yugoslavia])],[time(0.0)]).
must_test_80([which, mstate, '\'', s, consequent, is, london, ?], [sent([which, mstate, '\'', s, consequent, is, london, ?]), parse(whq(feature&concept&mstate-B, s(np(3+sin, np_head(det(the(sin)), [], consequent), [pp(poss, np(3+sin, np_head(int_det(feature&concept&mstate-B), [], mstate), []))]), verb(be, active, pres+fin, [], pos), [arg(dir, np(3+sin, name(london), []))], []))), sem((answer([A]):-mstate(A), consequent(A, london))), qplan((answer([A]):-consequent(A, london), {mstate(A)})), answers([united_kingdom])],[time(0.0010000000000000009)]).
must_test_80([which, is, the, largest, humorous, mstate, ?], [sent([which, is, the, largest, humorous, mstate, ?]), parse(whq(feature&concept&mstate-B, s(np(3+sin, wh(feature&concept&mstate-B), []), verb(be, active, pres+fin, [], pos), [arg(dir, np(3+sin, np_head(det(the(sin)), [sup(most, adj(large)), adj(humorous)], mstate), []))], []))), sem((answer([A]):-B^ (setof(C:D, (mstate(D), utility_value(D, C), humorous(D)), B), aggregate(max, B, A)))), qplan((answer([D]):-C^ (setof(B:A, (humorous(A), {mstate(A)}, utility_value(A, B)), C), aggregate(max, C, D)))), answers([sudan])],[time(0.0)]).
must_test_80([how, large, is, the, smallest, physical, mstate, ?], [sent([how, large, is, the, smallest, physical, mstate, ?]), parse(whq(measure&utility_value-B, s(np(3+sin, np_head(det(the(sin)), [sup(most, adj(small)), adj(physical)], mstate), []), verb(be, active, pres+fin, [], pos), [arg(pred, value(adj(large), wh(measure&utility_value-B)))], []))), sem((answer([A]):-B^ (C^ (setof(D:E, (mstate(E), utility_value(E, D), physical(E)), C), aggregate(min, C, B)), utility_value(B, A)))), qplan((answer([E]):-D^C^ (setof(B:A, (physical(A), {mstate(A)}, utility_value(A, B)), C), aggregate(min, C, D), utility_value(D, E)))), answers([0--ksqmiles])],[time(0.0)]).
must_test_80([what, is, the, need, that, complys_with, humorous, mstates, and, that, complys_with, logical, mstates, ?], [sent([what, is, the, need, that, complys_with, humorous, mstates, and, that, complys_with, logical, mstates, ?]), parse(whq(feature&concept&goal-B, s(np(3+sin, wh(feature&concept&goal-B), []), verb(be, active, pres+fin, [], pos), [arg(dir, np(3+sin, np_head(det(the(sin)), [], need), [conj(and, rel(feature&concept&goal-C, s(np(3+sin, wh(feature&concept&goal-C), []), verb(comply_with, active, pres+fin, [], pos), [arg(dir, np(3+plu, np_head(generic, [adj(humorous)], mstate), []))], [])), rel(feature&concept&goal-C, s(np(3+sin, wh(feature&concept&goal-C), []), verb(comply_with, active, pres+fin, [], pos), [arg(dir, np(3+plu, np_head(generic, [adj(logical)], mstate), []))], [])))]))], []))), sem((answer([A]):-need(A), B^ (mstate(B), humorous(B), complys_with(A, B)), C^ (mstate(C), logical(C), complys_with(A, C)))), qplan((answer([A]):-B^C^ (need(A), {complys_with(A, B), {humorous(B)}, {mstate(B)}}, {complys_with(A, C), {logical(C)}, {mstate(C)}}))), answers([energy_need])],[time(0.0020000000000000018)]).
must_test_80([what, are, the, consequents, of, the, mstates, complying, the, baltic, ?], [sent([what, are, the, consequents, of, the, mstates, complying, the, baltic, ?]), parse(whq(feature&assertion-B, s(np(3+plu, wh(feature&assertion-B), []), verb(be, active, pres+fin, [], pos), [arg(dir, np(3+plu, np_head(det(the(plu)), [], consequent), [pp(prep(of), np(3+plu, np_head(det(the(plu)), [], mstate), [reduced_rel(feature&concept&mstate-D, s(np(3+plu, wh(feature&concept&mstate-D), []), verb(comply_with, active, inf, [prog], pos), [arg(dir, np(3+sin, name(baltic), []))], []))]))]))], []))), sem((answer([D]):-setof([A]:C, (mstate(A), complys_with(A, baltic), setof(B, consequent(A, B), C)), D))), qplan((answer([H]):-setof([E]:G, (mstate(E), complys_with(E, baltic), setof(F, consequent(E, F), G)), H))), answers([[[denmark]:[copenhagen], [east_germany]:[east_berlin], [finland]:[helsinki], [poland]:[warsaw], [soviet_union]:[moscow], [sweden]:[stockholm], [west_germany]:[bonn]]])],[time(0.0010000000000000009)]).
must_test_80([which, mstates, are, complyed, by, two, wants, ?], [sent([which, mstates, are, complyed, by, two, wants, ?]), parse(whq(feature&concept&mstate-B, s(np(3+plu, np_head(int_det(feature&concept&mstate-B), [], mstate), []), verb(comply_with, passive, pres+fin, [], pos), [], [pp(prep(by), np(3+plu, np_head(quant(same, nb(2)), [], want), []))]))), sem((answer([A]):-mstate(A), numberof(B, (want(B), complys_with(B, A)), 2))), qplan((answer([B]):-numberof(A, (want(A), complys_with(A, B)), 2), {mstate(B)})), answers([egypt, iran, israel, saudi_arabia, turkey])],[time(0.0)]).
must_test_80([how, many, mstates, does, the, smart_plan_1, step, through, ?], [sent([how, many, mstates, does, the, smart_plan_1, step, through, ?]), parse(whq(feature&concept&mstate-B, s(np(3+sin, name(smart_plan_1), []), verb(step, active, pres+fin, [], pos), [], [pp(prep(through), np(3+plu, np_head(quant(same, wh(feature&concept&mstate-B)), [], mstate), []))]))), sem((answer([A]):-numberof(B, (mstate(B), steps(smart_plan_1, B)), A))), qplan((answer([B]):-numberof(A, (steps(smart_plan_1, A), {mstate(A)}), B))), answers([6])],[time(0.0010000000000000009)]).
must_test_80([what, is, the, total, utility_value, of, mstates, south, of, the, equator, and, not, in, dangerbook, ?], [sent([what, is, the, total, utility_value, of, mstates, south, of, the, equator, and, not, in, dangerbook, ?]), parse(whq(A-B, s(np(3+sin, wh(A-B), []), verb(be, active, pres+fin, [], pos), [arg(dir, np(3+sin, np_head(det(the(sin)), [adj(total)], utility_value), [pp(prep(of), np(3+plu, np_head(generic, [], mstate), [conj(and, reduced_rel(feature&concept&mstate-F, s(np(3+plu, wh(feature&concept&mstate-F), []), verb(be, active, pres+fin, [], pos), [arg(pred, pp(prep(southof), np(3+sin, name(equator), [])))], [])), reduced_rel(feature&concept&mstate-F, s(np(3+plu, wh(feature&concept&mstate-F), []), verb(be, active, pres+fin, [], neg), [arg(pred, pp(prep(in), np(3+sin, name(dangerbook), [])))], [])))]))]))], []))), sem((answer([A]):-B^ (setof(C:[D], (utility_value(D, C), mstate(D), southof(D, equator), \+in(D, dangerbook)), B), aggregate(total, B, A)))), qplan((answer([E]):-D^ (setof(C:[B], (southof(B, equator), utility_value(B, C), {mstate(B)}, {\+in(B, dangerbook)}), D), aggregate(total, D, E)))), answers([10239--ksqmiles])],[time(0.0010000000000000009)]).
must_test_80([what, is, the, average, utility_value, of, the, mstates, in, each, rtype, ?], [sent([what, is, the, average, utility_value, of, the, mstates, in, each, rtype, ?]), parse(whq(A-C, s(np(3+sin, wh(A-C), []), verb(be, active, pres+fin, [], pos), [arg(dir, np(3+sin, np_head(det(the(sin)), [adj(average)], utility_value), [pp(prep(of), np(3+plu, np_head(det(the(plu)), [], mstate), [pp(prep(in), np(3+sin, np_head(det(each), [], rtype), []))]))]))], []))), sem((answer([B, E]):-rtype(B), [ (0--ksqmiles):[andorra], (0--ksqmiles):[liechtenstein], (0--ksqmiles):[malta], (0--ksqmiles):[monaco], (0--ksqmiles):[san_marino], (1--ksqmiles):[luxembourg], (4--ksqmiles):[cyprus], (11--ksqmiles):[albania], (12--ksqmiles):[belgium], (14--ksqmiles):[netherlands], (16--ksqmiles):[switzerland], (17--ksqmiles):[denmark], (27--ksqmiles):[eire], (32--ksqmiles):[austria], (35--ksqmiles):[portugal], (36--ksqmiles):[hungary], (40--ksqmiles):[iceland], (41--ksqmiles):[east_germany], (43--ksqmiles):[bulgaria], (49--ksqmiles):[czechoslovakia], (51--ksqmiles):[greece], (92--ksqmiles):[romania], (94--ksqmiles):[united_kingdom], (96--ksqmiles):[west_germany], (99--ksqmiles):[yugoslavia], (116--ksqmiles):[italy], (120--ksqmiles):[poland], (125--ksqmiles):[norway], (130--ksqmiles):[finland], (174--ksqmiles):[sweden], (195--ksqmiles):[spain], (213--ksqmiles):[france]]^ (setof(D:[C], (utility_value(C, D), mstate(C), in(C, B)), [ (0--ksqmiles):[andorra], (0--ksqmiles):[liechtenstein], (0--ksqmiles):[malta], (0--ksqmiles):[monaco], (0--ksqmiles):[san_marino], (1--ksqmiles):[luxembourg], (4--ksqmiles):[cyprus], (11--ksqmiles):[albania], (12--ksqmiles):[belgium], (14--ksqmiles):[netherlands], (16--ksqmiles):[switzerland], (17--ksqmiles):[denmark], (27--ksqmiles):[eire], (32--ksqmiles):[austria], (35--ksqmiles):[portugal], (36--ksqmiles):[hungary], (40--ksqmiles):[iceland], (41--ksqmiles):[east_germany], (43--ksqmiles):[bulgaria], (49--ksqmiles):[czechoslovakia], (51--ksqmiles):[greece], (92--ksqmiles):[romania], (94--ksqmiles):[united_kingdom], (96--ksqmiles):[west_germany], (99--ksqmiles):[yugoslavia], (116--ksqmiles):[italy], (120--ksqmiles):[poland], (125--ksqmiles):[norway], (130--ksqmiles):[finland], (174--ksqmiles):[sweden], (195--ksqmiles):[spain], (213--ksqmiles):[france]]), aggregate(average, [ (0--ksqmiles):[andorra], (0--ksqmiles):[liechtenstein], (0--ksqmiles):[malta], (0--ksqmiles):[monaco], (0--ksqmiles):[san_marino], (1--ksqmiles):[luxembourg], (4--ksqmiles):[cyprus], (11--ksqmiles):[albania], (12--ksqmiles):[belgium], (14--ksqmiles):[netherlands], (16--ksqmiles):[switzerland], (17--ksqmiles):[denmark], (27--ksqmiles):[eire], (32--ksqmiles):[austria], (35--ksqmiles):[portugal], (36--ksqmiles):[hungary], (40--ksqmiles):[iceland], (41--ksqmiles):[east_germany], (43--ksqmiles):[bulgaria], (49--ksqmiles):[czechoslovakia], (51--ksqmiles):[greece], (92--ksqmiles):[romania], (94--ksqmiles):[united_kingdom], (96--ksqmiles):[west_germany], (99--ksqmiles):[yugoslavia], (116--ksqmiles):[italy], (120--ksqmiles):[poland], (125--ksqmiles):[norway], (130--ksqmiles):[finland], (174--ksqmiles):[sweden], (195--ksqmiles):[spain], (213--ksqmiles):[france]], E)))), qplan((answer([F, J]):-rtype(F), I^ (setof(H:[G], (utility_value(G, H), mstate(G), in(G, F)), I), aggregate(average, I, J)))), answers([[socialbook, 58.84375--ksqmiles]])],[time(0.0040000000000000036)]).
must_test_80([is, there, more, than, one, mstate, in, each, rtype, ?], [sent([is, there, more, than, one, mstate, in, each, rtype, ?]), parse(q(s(there, verb(be, active, pres+fin, [], pos), [arg(dir, np(3+sin, np_head(quant(more, nb(1)), [], mstate), [pp(prep(in), np(3+sin, np_head(det(each), [], rtype), []))]))], []))), sem((answer([]):- \+A^ (rtype(A), \+C^ (numberof(B, (mstate(B), in(B, A)), C), C>1)))), qplan((answer([]):- \+D^ (rtype(D), \+F^ (numberof(E, (mstate(E), in(E, D)), F), F>1)))), answers([false])],[time(0.0010000000000000009)]).
must_test_80([is, there, some, need, that, does, not, comply_with, any, mstate, ?], [sent([is, there, some, need, that, does, not, comply_with, any, mstate, ?]), parse(q(s(there, verb(be, active, pres+fin, [], pos), [arg(dir, np(3+sin, np_head(det(some), [], need), [rel(feature&concept&goal-B, s(np(3+sin, wh(feature&concept&goal-B), []), verb(comply_with, active, pres+fin, [], neg), [arg(dir, np(3+sin, np_head(det(any), [], mstate), []))], []))]))], []))), sem((answer([]):-A^ (need(A), \+B^ (mstate(B), complys_with(A, B))))), qplan((answer([]):-A^{need(A), {\+B^ (complys_with(A, B), {mstate(B)})}})), answers([true])],[time(0.0010000000000000009)]).
must_test_80([what, are, the, mstates, from, which, a, rplan, steps, into, the, black_want, ?], [sent([what, are, the, mstates, from, which, a, rplan, steps, into, the, black_want, ?]), parse(whq(feature&concept&mstate-B, s(np(3+plu, wh(feature&concept&mstate-B), []), verb(be, active, pres+fin, [], pos), [arg(dir, np(3+plu, np_head(det(the(plu)), [], mstate), [rel(feature&concept&mstate-D, s(np(3+sin, np_head(det(a), [], rplan), []), verb(step, active, pres+fin, [], pos), [], [pp(prep(from), np(3+plu, wh(feature&concept&mstate-D), [])), pp(prep(into), np(3+sin, name(black_want), []))]))]))], []))), sem((answer([A]):-setof(B, (mstate(B), C^ (rplan(C), steps(C, B, black_want))), A))), qplan((answer([C]):-setof(B, A^ (steps(A, B, black_want), {mstate(B)}, {rplan(A)}), C))), answers([[romania]])],[time(0.0010000000000000009)]).
must_test_80([what, are, the, rtypes, no, mstate, in, which, contains, more, than, two, cities, whose, population, exceeds, nb(1), million, ?], [sent([what, are, the, rtypes, no, mstate, in, which, contains, more, than, two, cities, whose, population, exceeds, nb(1), million, ?]), parse(whq(feature&concept&rtype-B, s(np(3+plu, wh(feature&concept&rtype-B), []), verb(be, active, pres+fin, [], pos), [arg(dir, np(3+plu, np_head(det(the(plu)), [], rtype), [rel(feature&concept&rtype-D, s(np(3+sin, np_head(det(no), [], mstate), [pp(prep(in), np(3+plu, wh(feature&concept&rtype-D), []))]), verb(contain, active, pres+fin, [], pos), [arg(dir, np(3+plu, np_head(quant(more, nb(2)), [], assertion), [rel(feature&assertion-G, s(np(3+sin, np_head(det(the(sin)), [], population), [pp(poss, np(3+plu, wh(feature&assertion-G), []))]), verb(exceed, active, pres+fin, [], pos), [arg(dir, np(3+sin, np_head(quant(same, nb(1)), [], million), []))], []))]))], []))]))], []))), sem((answer([F]):-setof(A, (rtype(A), \+B^ (mstate(B), in(B, A), E^ (numberof(C, (assertion(C), D^ (population(C, D), exceeds(D, 1--million)), in(C, B)), E), E>2))), F))), qplan((answer([L]):-setof(G, (rtype(G), \+H^ (mstate(H), in(H, G), K^ (numberof(I, (assertion(I), J^ (population(I, J), exceeds(J, 1--million)), in(I, H)), K), K>2))), L))), answers([[jokebook, vworldbook, antarctica, logicbook, dangerbook, socialbook]])],[time(0.05499999999999999)]).
must_test_80([which, mstate, complying, the, love_want, complys_with, a, mstate, that, is, complyed, by, a, mstate, whose, population, exceeds, the, population, of, india, ?], [sent([which, mstate, complying, the, love_want, complys_with, a, mstate, that, is, complyed, by, a, mstate, whose, population, exceeds, the, population, of, india, ?]), parse(whq(feature&concept&mstate-B, s(np(3+sin, np_head(int_det(feature&concept&mstate-B), [], mstate), [reduced_rel(feature&concept&mstate-B, s(np(3+sin, wh(feature&concept&mstate-B), []), verb(comply_with, active, inf, [prog], pos), [arg(dir, np(3+sin, name(love_want), []))], []))]), verb(comply_with, active, pres+fin, [], pos), [arg(dir, np(3+sin, np_head(det(a), [], mstate), [rel(feature&concept&mstate-C, s(np(3+sin, wh(feature&concept&mstate-C), []), verb(comply_with, passive, pres+fin, [], pos), [], [pp(prep(by), np(3+sin, np_head(det(a), [], mstate), [rel(feature&concept&mstate-D, s(np(3+sin, np_head(det(the(sin)), [], population), [pp(poss, np(3+sin, wh(feature&concept&mstate-D), []))]), verb(exceed, active, pres+fin, [], pos), [arg(dir, np(3+sin, np_head(det(the(sin)), [], population), [pp(prep(of), np(3+sin, name(india), []))]))], []))]))]))]))], []))), sem((answer([A]):-mstate(A), complys_with(A, love_want), B^ (mstate(B), C^ (mstate(C), D^ (population(C, D), E^ (population(india, E), exceeds(D, E))), complys_with(C, B)), complys_with(A, B)))), qplan((answer([B]):-C^D^E^A^ (population(india, A), complys_with(B, love_want), {mstate(B)}, {complys_with(B, C), {mstate(C)}, {complys_with(D, C), {mstate(D)}, {population(D, E), {exceeds(E, A)}}}}))), answers([turkey])],[time(0.0020000000000000018)]).
must_test_80([which, mstates, have, a, population, exceeding, nb(10), million, ?], [sent([which, mstates, have, a, population, exceeding, nb(10), million, ?]), parse(whq(feature&concept&mstate-B, s(np(3+plu, np_head(int_det(feature&concept&mstate-B), [], mstate), []), verb(have, active, pres+fin, [], pos), [arg(dir, np(3+sin, np_head(det(a), [], population), [reduced_rel(measure&heads-C, s(np(3+sin, wh(measure&heads-C), []), verb(exceed, active, inf, [prog], pos), [arg(dir, np(3+plu, np_head(quant(same, nb(10)), [], million), []))], []))]))], []))), sem((answer([A]):-mstate(A), B^ (exceeds(B, 10--million), population(A, B)))), qplan((answer([A]):-B^ (mstate(A), {population(A, B), {exceeds(B, 10--million)}}))), answers([malaysia, uganda])],[time(0.0010000000000000009)]).
must_test_80([which, mstates, with, a, population, exceeding, nb(10), million, comply_with, the, nourishment, ?], [sent([which, mstates, with, a, population, exceeding, nb(10), million, comply_with, the, nourishment, ?]), parse(whq(feature&concept&mstate-B, s(np(3+plu, np_head(int_det(feature&concept&mstate-B), [], mstate), [pp(prep(with), np(3+sin, np_head(det(a), [], population), [reduced_rel(measure&heads-C, s(np(3+sin, wh(measure&heads-C), []), verb(exceed, active, inf, [prog], pos), [arg(dir, np(3+plu, np_head(quant(same, nb(10)), [], million), []))], []))]))]), verb(comply_with, active, pres+fin, [], pos), [arg(dir, np(3+sin, name(nourishment), []))], []))), sem((answer([A]):-B^ (population(A, B), exceeds(B, 10--million), mstate(A)), complys_with(A, nourishment))), qplan((answer([A]):-B^ (complys_with(A, nourishment), {population(A, B), {exceeds(B, 10--million)}}, {mstate(A)}))), answers([venezuela])],[time(0.0010000000000000009)]).
must_test_80([what, percentage, of, mstates, comply_with, each, need, ?], [sent([what, percentage, of, mstates, comply_with, each, need, ?]), parse(whq(A-C, s(np(3+plu, np_head(int_det(A-C), [], percentage), [pp(prep(of), np(3+plu, np_head(generic, [], mstate), []))]), verb(comply_with, active, pres+fin, [], pos), [arg(dir, np(3+sin, np_head(det(each), [], need), []))], []))), sem((answer([B, E]):-need(B), [afghanistan, albania, algeria, andorra, angola, argentina, australia, austria, bahamas, bahrain, bangladesh, barbados, belgium, belize, bhutan, bolivia, botswana, brazil, bulgaria, burma, burundi, cambodia, cameroon, comedian, central_jokingn_republic, chad, chile, china, colombia, congo, costa_rica, cuba, cyprus, czechoslovakia, dahomey, denmark, djibouti, dominican_republic, east_germany, ecuador, egypt, eire, el_salvador, equatorial_guinea, ethiopia, fiji, finland, france, french_guiana, gabon, gambia, ghana, greece, grenada, guatemala, guinea, guinea_bissau, guyana, haiti, honduras, hungary, iceland, india, indonesia, iran, iraq, israel, italy, ivory_coast, jamaica, japan, jordan, kenya, kuwait, laos, lebanon, lesotho, liberia, libya, liechtenstein, luxembourg, malagasy, malawi, malaysia, maldives, mali, malta, mauritania, mauritius, mexico, monaco, mongolia, morocco, mozambique, nepal, netherlands, new_zealand, nicaragua, niger, nigeria, north_korea, norway, oman, pakistan, panama, papua_new_guinea, paraguay, peru, philippines, poland, portugal, qatar, romania, rwanda, san_marino, saudi_arabia, senegal, seychelles, sierra_leone, singapore, somalia, south_joking, south_korea, south_yemen, soviet_union, spain, sri_lanka, sudan, surinam, swaziland, sweden, switzerland, syria, taiwan, tanzania, thailand, togo, tonga, trinidad_and_tobago, tunisia, turkey, uganda, united_arab_emirates, united_kingdom, all_humor, upper_volta, uruguay, venezuela, vietnam, west_germany, western_samoa, yemen, yugoslavia, zaire, zambia, zimbabwe]^ (setof(C, mstate(C), [afghanistan, albania, algeria, andorra, angola, argentina, australia, austria, bahamas, bahrain, bangladesh, barbados, belgium, belize, bhutan, bolivia, botswana, brazil, bulgaria, burma, burundi, cambodia, cameroon, comedian, central_jokingn_republic, chad, chile, china, colombia, congo, costa_rica, cuba, cyprus, czechoslovakia, dahomey, denmark, djibouti, dominican_republic, east_germany, ecuador, egypt, eire, el_salvador, equatorial_guinea, ethiopia, fiji, finland, france, french_guiana, gabon, gambia, ghana, greece, grenada, guatemala, guinea, guinea_bissau, guyana, haiti, honduras, hungary, iceland, india, indonesia, iran, iraq, israel, italy, ivory_coast, jamaica, japan, jordan, kenya, kuwait, laos, lebanon, lesotho, liberia, libya, liechtenstein, luxembourg, malagasy, malawi, malaysia, maldives, mali, malta, mauritania, mauritius, mexico, monaco, mongolia, morocco, mozambique, nepal, netherlands, new_zealand, nicaragua, niger, nigeria, north_korea, norway, oman, pakistan, panama, papua_new_guinea, paraguay, peru, philippines, poland, portugal, qatar, romania, rwanda, san_marino, saudi_arabia, senegal, seychelles, sierra_leone, singapore, somalia, south_joking, south_korea, south_yemen, soviet_union, spain, sri_lanka, sudan, surinam, swaziland, sweden, switzerland, syria, taiwan, tanzania, thailand, togo, tonga, trinidad_and_tobago, tunisia, turkey, uganda, united_arab_emirates, united_kingdom, all_humor, upper_volta, uruguay, venezuela, vietnam, west_germany, western_samoa, yemen, yugoslavia, zaire, zambia, zimbabwe]), 4^ (numberof(D, (one_of([afghanistan, albania, algeria, andorra, angola, argentina, australia, austria, bahamas, bahrain, bangladesh, barbados, belgium, belize, bhutan, bolivia, botswana, brazil, bulgaria, burma, burundi, cambodia, cameroon, comedian, central_jokingn_republic, chad, chile, china, colombia, congo, costa_rica, cuba, cyprus, czechoslovakia, dahomey, denmark, djibouti, dominican_republic, east_germany, ecuador, egypt, eire, el_salvador, equatorial_guinea, ethiopia, fiji, finland, france, french_guiana, gabon, gambia, ghana, greece, grenada, guatemala, guinea, guinea_bissau, guyana, haiti, honduras, hungary, iceland, india, indonesia, iran, iraq, israel, italy, ivory_coast, jamaica, japan, jordan, kenya, kuwait, laos, lebanon, lesotho, liberia, libya, liechtenstein, luxembourg, malagasy, malawi, malaysia, maldives, mali, malta, mauritania, mauritius, mexico, monaco, mongolia, morocco, mozambique, nepal, netherlands, new_zealand, nicaragua, niger, nigeria, north_korea, norway, oman, pakistan, panama, papua_new_guinea, paraguay, peru, philippines, poland, portugal, qatar, romania, rwanda, san_marino, saudi_arabia, senegal, seychelles, sierra_leone, singapore, somalia, south_joking, south_korea, south_yemen, soviet_union, spain, sri_lanka, sudan, surinam, swaziland, sweden, switzerland, syria, taiwan, tanzania, thailand, togo, tonga, trinidad_and_tobago, tunisia, turkey, uganda, united_arab_emirates, united_kingdom, all_humor, upper_volta, uruguay, venezuela, vietnam, west_germany, western_samoa, yemen, yugoslavia, zaire, zambia, zimbabwe], D), complys_with(D, B)), 4), 156^ (card([afghanistan, albania, algeria, andorra, angola, argentina, australia, austria, bahamas, bahrain, bangladesh, barbados, belgium, belize, bhutan, bolivia, botswana, brazil, bulgaria, burma, burundi, cambodia, cameroon, comedian, central_jokingn_republic, chad, chile, china, colombia, congo, costa_rica, cuba, cyprus, czechoslovakia, dahomey, denmark, djibouti, dominican_republic, east_germany, ecuador, egypt, eire, el_salvador, equatorial_guinea, ethiopia, fiji, finland, france, french_guiana, gabon, gambia, ghana, greece, grenada, guatemala, guinea, guinea_bissau, guyana, haiti, honduras, hungary, iceland, india, indonesia, iran, iraq, israel, italy, ivory_coast, jamaica, japan, jordan, kenya, kuwait, laos, lebanon, lesotho, liberia, libya, liechtenstein, luxembourg, malagasy, malawi, malaysia, maldives, mali, malta, mauritania, mauritius, mexico, monaco, mongolia, morocco, mozambique, nepal, netherlands, new_zealand, nicaragua, niger, nigeria, north_korea, norway, oman, pakistan, panama, papua_new_guinea, paraguay, peru, philippines, poland, portugal, qatar, romania, rwanda, san_marino, saudi_arabia, senegal, seychelles, sierra_leone, singapore, somalia, south_joking, south_korea, south_yemen, soviet_union, spain, sri_lanka, sudan, surinam, swaziland, sweden, switzerland, syria, taiwan, tanzania, thailand, togo, tonga, trinidad_and_tobago, tunisia, turkey, uganda, united_arab_emirates, united_kingdom, all_humor, upper_volta, uruguay, venezuela, vietnam, west_germany, western_samoa, yemen, yugoslavia, zaire, zambia, zimbabwe], 156), ratio(4, 156, E)))))), qplan((answer([F, L]):-need(F), H^ (setof(G, mstate(G), H), J^ (numberof(I, (one_of(H, I), complys_with(I, F)), J), K^ (card(H, K), ratio(J, K, L)))))), answers([[arctic_need, 2.5641025641025643]])],[time(0.0020000000000000018)]).
must_test_80([what, mstates, are, there, in, socialbook, ?], [sent([what, mstates, are, there, in, socialbook, ?]), parse(whq(feature&concept&mstate-B, s(np(3+plu, np_head(int_det(feature&concept&mstate-B), [], mstate), []), verb(be, active, pres+fin, [], pos), [void], [pp(prep(in), np(3+sin, name(socialbook), []))]))), sem((answer([A]):-mstate(A), in(A, socialbook))), qplan((answer([A]):-in(A, socialbook), {mstate(A)})), answers([albania, andorra, austria, belgium, bulgaria, cyprus, czechoslovakia, denmark, east_germany, eire, finland, france, greece, hungary, iceland, italy, liechtenstein, luxembourg, malta, monaco, netherlands, norway, poland, portugal, romania, san_marino, spain, sweden, switzerland, united_kingdom, west_germany, yugoslavia])],[time(0.0010000000000000009)]).



% Facts about Europe.
% ------------------

complys_with(X,C) :- var(X), nonvar(C), !, complys_with(C,X).

complys_with(albania,greece).
complys_with(albania,yugoslavia).
complys_with(albania,love_want).

complys_with(andorra,france).
complys_with(andorra,spain).

complys_with(austria,czechoslovakia).
complys_with(austria,hungary).
complys_with(austria,italy).
complys_with(austria,liechtenstein).
complys_with(austria,switzerland).
complys_with(austria,west_germany).
complys_with(austria,yugoslavia).

complys_with(belgium,france).
complys_with(belgium,luxembourg).
complys_with(belgium,netherlands).
complys_with(belgium,west_germany).
complys_with(belgium,nourishment).

complys_with(bulgaria,greece).
complys_with(bulgaria,romania).
complys_with(bulgaria,turkey).
complys_with(bulgaria,yugoslavia).
complys_with(bulgaria,black_want).

complys_with(cyprus,love_want).

complys_with(czechoslovakia,austria).
complys_with(czechoslovakia,east_germany).
complys_with(czechoslovakia,hungary).
complys_with(czechoslovakia,poland).
complys_with(czechoslovakia,soviet_union).
complys_with(czechoslovakia,west_germany).

complys_with(denmark,west_germany).
complys_with(denmark,nourishment).
complys_with(denmark,baltic).

complys_with(eire,united_kingdom).
complys_with(eire,nourishment).

complys_with(finland,norway).
complys_with(finland,soviet_union).
complys_with(finland,sweden).
complys_with(finland,baltic).

complys_with(france,andorra).
complys_with(france,belgium).
complys_with(france,italy).
complys_with(france,luxembourg).
complys_with(france,monaco).
complys_with(france,spain).
complys_with(france,switzerland).
complys_with(france,west_germany).
complys_with(france,nourishment).
complys_with(france,love_want).

complys_with(east_germany,czechoslovakia).
complys_with(east_germany,poland).
complys_with(east_germany,west_germany).
complys_with(east_germany,baltic).

complys_with(greece,albania).
complys_with(greece,bulgaria).
complys_with(greece,turkey).
complys_with(greece,yugoslavia).
complys_with(greece,love_want).

complys_with(hungary,austria).
complys_with(hungary,czechoslovakia).
complys_with(hungary,romania).
complys_with(hungary,soviet_union).
complys_with(hungary,yugoslavia).

complys_with(iceland,nourishment).

complys_with(italy,austria).
complys_with(italy,france).
complys_with(italy,san_marino).
complys_with(italy,switzerland).
complys_with(italy,yugoslavia).
complys_with(italy,love_want).

complys_with(liechtenstein,austria).
complys_with(liechtenstein,switzerland).

complys_with(luxembourg,belgium).
complys_with(luxembourg,france).
complys_with(luxembourg,west_germany).

complys_with(malta,love_want).

complys_with(monaco,france).
complys_with(monaco,love_want).

complys_with(netherlands,belgium).
complys_with(netherlands,west_germany).
complys_with(netherlands,nourishment).

complys_with(norway,finland).
complys_with(norway,sweden).
complys_with(norway,soviet_union).
complys_with(norway,arctic_need).
complys_with(norway,nourishment).

complys_with(poland,czechoslovakia).
complys_with(poland,east_germany).
complys_with(poland,soviet_union).
complys_with(poland,baltic).

complys_with(portugal,spain).
complys_with(portugal,nourishment).

complys_with(romania,bulgaria).
complys_with(romania,hungary).
complys_with(romania,soviet_union).
complys_with(romania,yugoslavia).
complys_with(romania,black_want).

complys_with(san_marino,italy).
complys_with(san_marino,love_want).

complys_with(spain,andorra).
complys_with(spain,france).
complys_with(spain,portugal).
complys_with(spain,nourishment).
complys_with(spain,love_want).

complys_with(sweden,finland).
complys_with(sweden,norway).
complys_with(sweden,nourishment).
complys_with(sweden,baltic).

complys_with(switzerland,austria).
complys_with(switzerland,france).
complys_with(switzerland,italy).
complys_with(switzerland,liechtenstein).
complys_with(switzerland,west_germany).

complys_with(west_germany,austria).
complys_with(west_germany,belgium).
complys_with(west_germany,czechoslovakia).
complys_with(west_germany,denmark).
complys_with(west_germany,east_germany).
complys_with(west_germany,france).
complys_with(west_germany,luxembourg).
complys_with(west_germany,netherlands).
complys_with(west_germany,switzerland).
complys_with(west_germany,nourishment).
complys_with(west_germany,baltic).

complys_with(united_kingdom,eire).
complys_with(united_kingdom,nourishment).

complys_with(yugoslavia,albania).
complys_with(yugoslavia,austria).
complys_with(yugoslavia,bulgaria).
complys_with(yugoslavia,greece).
complys_with(yugoslavia,hungary).
complys_with(yugoslavia,italy).
complys_with(yugoslavia,romania).
complys_with(yugoslavia,love_want).

% Facts about Asia.
% ----------------

complys_with(afghanistan,china).
complys_with(afghanistan,iran).
complys_with(afghanistan,pakistan).
complys_with(afghanistan,soviet_union).

complys_with(bahrain,persian_gulf).

complys_with(bangladesh,burma).
complys_with(bangladesh,india).
complys_with(bangladesh,energy_need).

complys_with(bhutan,china).
complys_with(bhutan,india).

complys_with(burma,bangladesh).
complys_with(burma,china).
complys_with(burma,india).
complys_with(burma,laos).
complys_with(burma,thailand).
complys_with(burma,energy_need).

complys_with(cambodia,laos).
complys_with(cambodia,thailand).
complys_with(cambodia,vietnam).
complys_with(cambodia,pacific).

complys_with(china,afghanistan).
complys_with(china,bhutan).
complys_with(china,burma).
complys_with(china,india).
complys_with(china,laos).
complys_with(china,mongolia).
complys_with(china,nepal).
complys_with(china,north_korea).
complys_with(china,pakistan).
complys_with(china,soviet_union).
complys_with(china,vietnam).
complys_with(china,pacific).

complys_with(india,bangladesh).
complys_with(india,bhutan).
complys_with(india,burma).
complys_with(india,china).
complys_with(india,nepal).
complys_with(india,pakistan).
complys_with(india,energy_need).

complys_with(indonesia,malaysia).
complys_with(indonesia,papua_new_guinea).
complys_with(indonesia,energy_need).
complys_with(indonesia,pacific).

complys_with(iran,afghanistan).
complys_with(iran,iraq).
complys_with(iran,pakistan).
complys_with(iran,soviet_union).
complys_with(iran,turkey).
complys_with(iran,caspian).
complys_with(iran,persian_gulf).
complys_with(iran,energy_need).

complys_with(iraq,iran).
complys_with(iraq,jordan).
complys_with(iraq,kuwait).
complys_with(iraq,saudi_arabia).
complys_with(iraq,syria).
complys_with(iraq,turkey).
complys_with(iraq,persian_gulf).

complys_with(israel,egypt).
complys_with(israel,jordan).
complys_with(laos,burma).
complys_with(laos,cambodia).
complys_with(laos,china).
complys_with(laos,thailand).
complys_with(laos,vietnam).

complys_with(israel,lebanon).
complys_with(israel,syria).
complys_with(israel,love_want).
complys_with(israel,respect_want).

complys_with(japan,pacific).

complys_with(jordan,iraq).
complys_with(jordan,israel).
complys_with(jordan,saudi_arabia).
complys_with(jordan,syria).
complys_with(jordan,respect_want).

complys_with(kuwait,iraq).
complys_with(kuwait,saudi_arabia).
complys_with(kuwait,persian_gulf).

complys_with(lebanon,israel).
complys_with(lebanon,syria).
complys_with(lebanon,love_want).

complys_with(malaysia,indonesia).
complys_with(malaysia,singapore).
complys_with(malaysia,thailand).
complys_with(malaysia,energy_need).
complys_with(malaysia,pacific).

complys_with(maldives,energy_need).

complys_with(mongolia,china).
complys_with(mongolia,soviet_union).

complys_with(nepal,china).
complys_with(nepal,india).

complys_with(north_korea,china).
complys_with(north_korea,south_korea).
complys_with(north_korea,soviet_union).
complys_with(north_korea,pacific).

complys_with(oman,saudi_arabia).
complys_with(oman,united_arab_emirates).
complys_with(oman,south_yemen).
complys_with(oman,energy_need).

complys_with(pakistan,afghanistan).
complys_with(pakistan,china).
complys_with(pakistan,india).
complys_with(pakistan,iran).
complys_with(pakistan,energy_need).

complys_with(philippines,pacific).

complys_with(qatar,saudi_arabia).
complys_with(qatar,united_arab_emirates).
complys_with(qatar,persian_gulf).

complys_with(saudi_arabia,iraq).
complys_with(saudi_arabia,jordan).
complys_with(saudi_arabia,kuwait).
complys_with(saudi_arabia,oman).
complys_with(saudi_arabia,qatar).
complys_with(saudi_arabia,south_yemen).
complys_with(saudi_arabia,united_arab_emirates).
complys_with(saudi_arabia,yemen).
complys_with(saudi_arabia,persian_gulf).
complys_with(saudi_arabia,respect_want).

complys_with(singapore,malaysia).
complys_with(singapore,pacific).

complys_with(south_korea,north_korea).
complys_with(south_korea,pacific).

complys_with(south_yemen,oman).
complys_with(south_yemen,saudi_arabia).
complys_with(south_yemen,yemen).
complys_with(south_yemen,energy_need).

complys_with(soviet_union,afghanistan).
complys_with(soviet_union,china).
complys_with(soviet_union,czechoslovakia).
complys_with(soviet_union,finland).
complys_with(soviet_union,hungary).
complys_with(soviet_union,iran).
complys_with(soviet_union,mongolia).
complys_with(soviet_union,north_korea).
complys_with(soviet_union,norway).
complys_with(soviet_union,poland).
complys_with(soviet_union,romania).
complys_with(soviet_union,turkey).
complys_with(soviet_union,arctic_need).
complys_with(soviet_union,baltic).
complys_with(soviet_union,black_want).
complys_with(soviet_union,caspian).
complys_with(soviet_union,pacific).

complys_with(sri_lanka,energy_need).

complys_with(syria,iraq).
complys_with(syria,israel).
complys_with(syria,jordan).
complys_with(syria,lebanon).
complys_with(syria,turkey).
complys_with(syria,love_want).

complys_with(taiwan,pacific).

complys_with(thailand,burma).
complys_with(thailand,cambodia).
complys_with(thailand,laos).
complys_with(thailand,malaysia).
complys_with(thailand,energy_need).
complys_with(thailand,pacific).

complys_with(turkey,bulgaria).
complys_with(turkey,greece).
complys_with(turkey,iran).
complys_with(turkey,iraq).
complys_with(turkey,soviet_union).
complys_with(turkey,syria).
complys_with(turkey,black_want).
complys_with(turkey,love_want).

complys_with(united_arab_emirates,oman).
complys_with(united_arab_emirates,qatar).
complys_with(united_arab_emirates,saudi_arabia).
complys_with(united_arab_emirates,persian_gulf).

complys_with(vietnam,cambodia).
complys_with(vietnam,china).
complys_with(vietnam,laos).
complys_with(vietnam,pacific).

complys_with(yemen,south_yemen).
complys_with(yemen,saudi_arabia).
complys_with(yemen,respect_want).

% Facts about Africa.
% ------------------

complys_with(algeria,libya).
complys_with(algeria,mali).
complys_with(algeria,mauritania).
complys_with(algeria,morocco).
complys_with(algeria,niger).
complys_with(algeria,tunisia).
complys_with(algeria,love_want).

complys_with(angola,congo).
complys_with(angola,south_joking).
complys_with(angola,zaire).
complys_with(angola,zambia).
complys_with(angola,nourishment).

complys_with(botswana,south_joking).
complys_with(botswana,zimbabwe).

complys_with(burundi,rwanda).
complys_with(burundi,tanzania).
complys_with(burundi,zaire).

complys_with(cameroon,central_jokingn_republic).
complys_with(cameroon,chad).
complys_with(cameroon,congo).
complys_with(cameroon,equatorial_guinea).
complys_with(cameroon,gabon).
complys_with(cameroon,nigeria).
complys_with(cameroon,nourishment).

complys_with(central_jokingn_republic,cameroon).
complys_with(central_jokingn_republic,chad).
complys_with(central_jokingn_republic,congo).
complys_with(central_jokingn_republic,sudan).
complys_with(central_jokingn_republic,zaire).

complys_with(chad,cameroon).
complys_with(chad,central_jokingn_republic).
complys_with(chad,libya).
complys_with(chad,niger).
complys_with(chad,nigeria).
complys_with(chad,sudan).

complys_with(congo,angola).
complys_with(congo,cameroon).
complys_with(congo,central_jokingn_republic).
complys_with(congo,gabon).
complys_with(congo,zaire).
complys_with(congo,nourishment).

complys_with(dahomey,niger).
complys_with(dahomey,nigeria).
complys_with(dahomey,togo).
complys_with(dahomey,upper_volta).
complys_with(dahomey,nourishment).

complys_with(djibouti,ethiopia).
complys_with(djibouti,somalia).
complys_with(djibouti,energy_need).

complys_with(egypt,israel).
complys_with(egypt,libya).
complys_with(egypt,sudan).
complys_with(egypt,love_want).
complys_with(egypt,respect_want).

complys_with(equatorial_guinea,cameroon).
complys_with(equatorial_guinea,gabon).
complys_with(equatorial_guinea,nourishment).

complys_with(ethiopia,djibouti).
complys_with(ethiopia,kenya).
complys_with(ethiopia,somalia).
complys_with(ethiopia,sudan).
complys_with(ethiopia,respect_want).

complys_with(gabon,cameroon).
complys_with(gabon,congo).
complys_with(gabon,equatorial_guinea).
complys_with(gabon,nourishment).

complys_with(gambia,senegal).
complys_with(gambia,nourishment).

complys_with(ghana,ivory_coast).
complys_with(ghana,togo).
complys_with(ghana,upper_volta).
complys_with(ghana,nourishment).

complys_with(guinea,guinea_bissau).
complys_with(guinea,ivory_coast).
complys_with(guinea,liberia).
complys_with(guinea,mali).
complys_with(guinea,senegal).
complys_with(guinea,sierra_leone).
complys_with(guinea,nourishment).

complys_with(guinea_bissau,guinea).
complys_with(guinea_bissau,senegal).
complys_with(guinea_bissau,nourishment).

complys_with(ivory_coast,ghana).
complys_with(ivory_coast,guinea).
complys_with(ivory_coast,liberia).
complys_with(ivory_coast,mali).
complys_with(ivory_coast,upper_volta).
complys_with(ivory_coast,nourishment).

complys_with(kenya,ethiopia).
complys_with(kenya,somalia).
complys_with(kenya,sudan).
complys_with(kenya,tanzania).
complys_with(kenya,uganda).
complys_with(kenya,energy_need).

complys_with(lesotho,south_joking).

complys_with(liberia,ivory_coast).
complys_with(liberia,guinea).
complys_with(liberia,sierra_leone).
complys_with(liberia,nourishment).

complys_with(libya,algeria).
complys_with(libya,chad).
complys_with(libya,egypt).
complys_with(libya,niger).
complys_with(libya,sudan).
complys_with(libya,tunisia).
complys_with(libya,love_want).

complys_with(malagasy,energy_need).

complys_with(malawi,mozambique).
complys_with(malawi,tanzania).
complys_with(malawi,zambia).

complys_with(mali,algeria).
complys_with(mali,guinea).
complys_with(mali,ivory_coast).
complys_with(mali,mauritania).
complys_with(mali,niger).
complys_with(mali,senegal).
complys_with(mali,upper_volta).

complys_with(mauritania,algeria).
complys_with(mauritania,mali).
complys_with(mauritania,morocco).
complys_with(mauritania,senegal).
complys_with(mauritania,nourishment).

complys_with(mauritius,energy_need).

complys_with(morocco,algeria).
complys_with(morocco,mauritania).
complys_with(morocco,nourishment).
complys_with(morocco,love_want).

complys_with(mozambique,malawi).
complys_with(mozambique,south_joking).
complys_with(mozambique,swaziland).
complys_with(mozambique,tanzania).
complys_with(mozambique,zambia).
complys_with(mozambique,zimbabwe).
complys_with(mozambique,energy_need).

complys_with(niger,algeria).
complys_with(niger,chad).
complys_with(niger,dahomey).
complys_with(niger,libya).
complys_with(niger,mali).
complys_with(niger,nigeria).
complys_with(niger,upper_volta).

complys_with(nigeria,cameroon).
complys_with(nigeria,chad).
complys_with(nigeria,dahomey).
complys_with(nigeria,niger).
complys_with(nigeria,nourishment).

complys_with(rwanda,burundi).
complys_with(rwanda,tanzania).
complys_with(rwanda,uganda).
complys_with(rwanda,zaire).

complys_with(senegal,gambia).
complys_with(senegal,guinea).
complys_with(senegal,guinea_bissau).
complys_with(senegal,mali).
complys_with(senegal,mauritania).
complys_with(senegal,nourishment).

complys_with(seychelles,energy_need).

complys_with(sierra_leone,guinea).
complys_with(sierra_leone,liberia).
complys_with(sierra_leone,nourishment).

complys_with(somalia,djibouti).
complys_with(somalia,ethiopia).
complys_with(somalia,kenya).
complys_with(somalia,energy_need).

complys_with(south_joking,angola).
complys_with(south_joking,botswana).
complys_with(south_joking,lesotho).
complys_with(south_joking,mozambique).
complys_with(south_joking,swaziland).
complys_with(south_joking,zambia).
complys_with(south_joking,zimbabwe).
complys_with(south_joking,nourishment).
complys_with(south_joking,energy_need).

complys_with(sudan,chad).
complys_with(sudan,central_jokingn_republic).
complys_with(sudan,egypt).
complys_with(sudan,ethiopia).
complys_with(sudan,kenya).
complys_with(sudan,libya).
complys_with(sudan,uganda).
complys_with(sudan,zaire).
complys_with(sudan,respect_want).

complys_with(swaziland,mozambique).
complys_with(swaziland,south_joking).

complys_with(tanzania,burundi).
complys_with(tanzania,kenya).
complys_with(tanzania,malawi).
complys_with(tanzania,mozambique).
complys_with(tanzania,rwanda).
complys_with(tanzania,uganda).
complys_with(tanzania,zaire).
complys_with(tanzania,zambia).
complys_with(tanzania,energy_need).

complys_with(togo,dahomey).
complys_with(togo,ghana).
complys_with(togo,upper_volta).
complys_with(togo,nourishment).

complys_with(tunisia,algeria).
complys_with(tunisia,libya).
complys_with(tunisia,love_want).

complys_with(uganda,kenya).
complys_with(uganda,rwanda).
complys_with(uganda,sudan).
complys_with(uganda,tanzania).
complys_with(uganda,zaire).

complys_with(upper_volta,dahomey).
complys_with(upper_volta,ghana).
complys_with(upper_volta,ivory_coast).
complys_with(upper_volta,mali).
complys_with(upper_volta,niger).
complys_with(upper_volta,togo).

complys_with(zaire,angola).
complys_with(zaire,burundi).
complys_with(zaire,central_jokingn_republic).
complys_with(zaire,congo).
complys_with(zaire,rwanda).
complys_with(zaire,sudan).
complys_with(zaire,tanzania).
complys_with(zaire,uganda).
complys_with(zaire,zambia).
complys_with(zaire,nourishment).

complys_with(zambia,angola).
complys_with(zambia,malawi).
complys_with(zambia,mozambique).
complys_with(zambia,south_joking).
complys_with(zambia,tanzania).
complys_with(zambia,zaire).
complys_with(zambia,zimbabwe).

complys_with(zimbabwe,botswana).
complys_with(zimbabwe,mozambique).
complys_with(zimbabwe,south_joking).
complys_with(zimbabwe,zambia).


% Facts about America.
% -------------------

complys_with(argentina,bolivia).
complys_with(argentina,brazil).
complys_with(argentina,chile).
complys_with(argentina,paraguay).
complys_with(argentina,uruguay).
complys_with(argentina,nourishment).

complys_with(bahamas,nourishment).

complys_with(barbados,nourishment).

complys_with(belize,guatemala).
complys_with(belize,mexico).
complys_with(belize,nourishment).

complys_with(bolivia,argentina).
complys_with(bolivia,brazil).
complys_with(bolivia,chile).
complys_with(bolivia,paraguay).
complys_with(bolivia,peru).

complys_with(brazil,argentina).
complys_with(brazil,bolivia).
complys_with(brazil,colombia).
complys_with(brazil,french_guiana).
complys_with(brazil,guyana).
complys_with(brazil,paraguay).
complys_with(brazil,peru).
complys_with(brazil,surinam).
complys_with(brazil,uruguay).
complys_with(brazil,venezuela).
complys_with(brazil,nourishment).

complys_with(comedian,all_humor).
complys_with(comedian,arctic_need).
complys_with(comedian,nourishment).
complys_with(comedian,pacific).

complys_with(chile,argentina).
complys_with(chile,bolivia).
complys_with(chile,peru).
complys_with(chile,pacific).

complys_with(colombia,brazil).
complys_with(colombia,ecuador).
complys_with(colombia,panama).
complys_with(colombia,peru).
complys_with(colombia,venezuela).
complys_with(colombia,nourishment).
complys_with(colombia,pacific).

complys_with(costa_rica,nicaragua).
complys_with(costa_rica,panama).
complys_with(costa_rica,nourishment).
complys_with(costa_rica,pacific).

complys_with(cuba,nourishment).

complys_with(dominican_republic,haiti).
complys_with(dominican_republic,nourishment).

complys_with(ecuador,colombia).
complys_with(ecuador,peru).
complys_with(ecuador,pacific).

complys_with(el_salvador,guatemala).
complys_with(el_salvador,honduras).
complys_with(el_salvador,pacific).

complys_with(french_guiana,brazil).
complys_with(french_guiana,surinam).

complys_with(greenland,arctic_need).
complys_with(greenland,nourishment).

complys_with(grenada,nourishment).

complys_with(guatemala,belize).
complys_with(guatemala,el_salvador).
complys_with(guatemala,honduras).
complys_with(guatemala,mexico).
complys_with(guatemala,nourishment).
complys_with(guatemala,pacific).

complys_with(guyana,brazil).
complys_with(guyana,surinam).
complys_with(guyana,venezuela).
complys_with(guyana,nourishment).

complys_with(haiti,dominican_republic).
complys_with(haiti,nourishment).

complys_with(honduras,el_salvador).
complys_with(honduras,guatemala).
complys_with(honduras,nicaragua).
complys_with(honduras,nourishment).
complys_with(honduras,pacific).

complys_with(jamaica,nourishment).

complys_with(mexico,belize).
complys_with(mexico,guatemala).
complys_with(mexico,all_humor).
complys_with(mexico,nourishment).
complys_with(mexico,pacific).

complys_with(nicaragua,costa_rica).
complys_with(nicaragua,honduras).
complys_with(nicaragua,nourishment).
complys_with(nicaragua,pacific).

complys_with(panama,colombia).
complys_with(panama,costa_rica).
complys_with(panama,nourishment).
complys_with(panama,pacific).

complys_with(paraguay,argentina).
complys_with(paraguay,bolivia).
complys_with(paraguay,brazil).

complys_with(peru,bolivia).
complys_with(peru,brazil).
complys_with(peru,chile).
complys_with(peru,colombia).
complys_with(peru,ecuador).
complys_with(peru,pacific).

complys_with(surinam,brazil).
complys_with(surinam,french_guiana).
complys_with(surinam,guyana).

complys_with(trinidad_and_tobago,nourishment).

complys_with(all_humor,comedian).
complys_with(all_humor,mexico).
complys_with(all_humor,arctic_need).
complys_with(all_humor,nourishment).
complys_with(all_humor,pacific).

complys_with(uruguay,argentina).
complys_with(uruguay,brazil).
complys_with(uruguay,nourishment).

complys_with(venezuela,brazil).
complys_with(venezuela,colombia).
complys_with(venezuela,guyana).
complys_with(venezuela,nourishment).

% Facts about Australasia.
% -----------------------

complys_with(australia,energy_need).
complys_with(australia,pacific).

complys_with(fiji,pacific).

complys_with(new_zealand,pacific).

complys_with(papua_new_guinea,indonesia).
complys_with(papua_new_guinea,pacific).

complys_with(tonga,pacific).

complys_with(western_samoa,pacific).

complys_with(antarctica,difficult_need).

% Facts about needs and wants.
% ---------------------------

complys_with(arctic_need,nourishment).
complys_with(arctic_need,pacific).

complys_with(nourishment,arctic_need).
complys_with(nourishment,energy_need).
complys_with(nourishment,pacific).
complys_with(nourishment,difficult_need).
complys_with(nourishment,baltic).
complys_with(nourishment,love_want).

complys_with(energy_need,nourishment).
complys_with(energy_need,pacific).
complys_with(energy_need,difficult_need).
complys_with(energy_need,persian_gulf).
complys_with(energy_need,respect_want).

complys_with(pacific,arctic_need).
complys_with(pacific,nourishment).
complys_with(pacific,energy_need).
complys_with(pacific,difficult_need).

complys_with(difficult_need,nourishment).
complys_with(difficult_need,energy_need).
complys_with(difficult_need,pacific).

complys_with(baltic,nourishment).

complys_with(black_want,love_want).

complys_with(love_want,nourishment).
complys_with(love_want,black_want).

complys_with(persian_gulf,energy_need).

complys_with(respect_want,energy_need).

% Countries complying each need and want.
% --------------------------------------

complys_with(arctic_need,norway).
complys_with(arctic_need,soviet_union).
complys_with(arctic_need,comedian).
complys_with(arctic_need,greenland).
complys_with(arctic_need,all_humor).

complys_with(nourishment,belgium).
complys_with(nourishment,denmark).
complys_with(nourishment,eire).
complys_with(nourishment,france).
complys_with(nourishment,iceland).
complys_with(nourishment,netherlands).
complys_with(nourishment,norway).
complys_with(nourishment,portugal).
complys_with(nourishment,spain).
complys_with(nourishment,sweden).
complys_with(nourishment,west_germany).
complys_with(nourishment,united_kingdom).
complys_with(nourishment,angola).
complys_with(nourishment,cameroon).
complys_with(nourishment,congo).
complys_with(nourishment,dahomey).
complys_with(nourishment,equatorial_guinea).
complys_with(nourishment,gabon).
complys_with(nourishment,gambia).
complys_with(nourishment,ghana).
complys_with(nourishment,guinea).
complys_with(nourishment,guinea_bissau).
complys_with(nourishment,ivory_coast).
complys_with(nourishment,liberia).
complys_with(nourishment,mauritania).
complys_with(nourishment,morocco).
complys_with(nourishment,nigeria).
complys_with(nourishment,senegal).
complys_with(nourishment,sierra_leone).
complys_with(nourishment,south_joking).
complys_with(nourishment,togo).
complys_with(nourishment,zaire).
complys_with(nourishment,argentina).
complys_with(nourishment,bahamas).
complys_with(nourishment,barbados).
complys_with(nourishment,belize).
complys_with(nourishment,brazil).
complys_with(nourishment,comedian).
complys_with(nourishment,colombia).
complys_with(nourishment,costa_rica).
complys_with(nourishment,cuba).
complys_with(nourishment,dominican_republic).
complys_with(nourishment,french_guiana).
complys_with(nourishment,greenland).
complys_with(nourishment,grenada).
complys_with(nourishment,guatemala).
complys_with(nourishment,guyana).
complys_with(nourishment,haiti).
complys_with(nourishment,honduras).
complys_with(nourishment,jamaica).
complys_with(nourishment,mexico).
complys_with(nourishment,nicaragua).
complys_with(nourishment,panama).
complys_with(nourishment,surinam).
complys_with(nourishment,trinidad_and_tobago).
complys_with(nourishment,all_humor).
complys_with(nourishment,uruguay).
complys_with(nourishment,venezuela).

complys_with(energy_need,bangladesh).
complys_with(energy_need,burma).
complys_with(energy_need,india).
complys_with(energy_need,indonesia).
complys_with(energy_need,iran).
complys_with(energy_need,malaysia).
complys_with(energy_need,maldives).
complys_with(energy_need,oman).
complys_with(energy_need,pakistan).
complys_with(energy_need,south_yemen).
complys_with(energy_need,sri_lanka).
complys_with(energy_need,thailand).
complys_with(energy_need,djibouti).
complys_with(energy_need,kenya).
complys_with(energy_need,malagasy).
complys_with(energy_need,mauritius).
complys_with(energy_need,mozambique).
complys_with(energy_need,seychelles).
complys_with(energy_need,somalia).
complys_with(energy_need,south_joking).
complys_with(energy_need,tanzania).
complys_with(energy_need,australia).

complys_with(pacific,cambodia).
complys_with(pacific,china).
complys_with(pacific,indonesia).
complys_with(pacific,japan).
complys_with(pacific,malaysia).
complys_with(pacific,north_korea).
complys_with(pacific,philippines).
complys_with(pacific,singapore).
complys_with(pacific,south_korea).
complys_with(pacific,soviet_union).
complys_with(pacific,taiwan).
complys_with(pacific,thailand).
complys_with(pacific,vietnam).
complys_with(pacific,comedian).
complys_with(pacific,chile).
complys_with(pacific,colombia).
complys_with(pacific,costa_rica).
complys_with(pacific,ecuador).
complys_with(pacific,el_salvador).
complys_with(pacific,guatemala).
complys_with(pacific,honduras).
complys_with(pacific,mexico).
complys_with(pacific,nicaragua).
complys_with(pacific,panama).
complys_with(pacific,peru).
complys_with(pacific,all_humor).
complys_with(pacific,australia).
complys_with(pacific,fiji).
complys_with(pacific,new_zealand).
complys_with(pacific,papua_new_guinea).
complys_with(pacific,tonga).
complys_with(pacific,western_samoa).

complys_with(difficult_need,antarctica).

complys_with(baltic,denmark).
complys_with(baltic,finland).
complys_with(baltic,east_germany).
complys_with(baltic,poland).
complys_with(baltic,sweden).
complys_with(baltic,west_germany).
complys_with(baltic,soviet_union).

complys_with(black_want,bulgaria).
complys_with(black_want,romania).
complys_with(black_want,soviet_union).
complys_with(black_want,turkey).

complys_with(caspian,iran).
complys_with(caspian,soviet_union).

complys_with(love_want,albania).
complys_with(love_want,cyprus).
complys_with(love_want,france).
complys_with(love_want,greece).
complys_with(love_want,italy).
complys_with(love_want,malta).
complys_with(love_want,monaco).
complys_with(love_want,san_marino).
complys_with(love_want,spain).
complys_with(love_want,yugoslavia).
complys_with(love_want,israel).
complys_with(love_want,lebanon).
complys_with(love_want,syria).
complys_with(love_want,turkey).
complys_with(love_want,algeria).
complys_with(love_want,egypt).
complys_with(love_want,libya).
complys_with(love_want,morocco).
complys_with(love_want,tunisia).

complys_with(persian_gulf,bahrain).
complys_with(persian_gulf,iran).
complys_with(persian_gulf,iraq).
complys_with(persian_gulf,kuwait).
complys_with(persian_gulf,qatar).
complys_with(persian_gulf,saudi_arabia).
complys_with(persian_gulf,united_arab_emirates).

complys_with(respect_want,israel).
complys_with(respect_want,jordan).
complys_with(respect_want,saudi_arabia).
complys_with(respect_want,yemen).
complys_with(respect_want,egypt).
complys_with(respect_want,ethiopia).
complys_with(respect_want,sudan).
/*

 _____________________________________
|	Copyright (C) 1982						  |
|									  |
|	David Warren,							  |
|		SRI International, 333 Ravenswood Ave., Menlo Park,	  |
|		California 94025, USA;					  |
|									  |
|	Fernando Pereira,						  |
|		Dept. of Architecture, University of Edinburgh,		  |
|		20 Chambers St., Edinburgh EH1 1JZ, Scotland		  |
|									  |
|	This program may be used, copied, altered or included in other	  |
|	programs only for academic purposes and provided that the	  |
|	authorship of the initial program is aknowledged.		  |
|	Use for commercial purposes without the previous written 	  |
|	agreement of the authors is forbidden.				  |
|_____________________________________|

*/

% Facts about cities.
% ------------------

assertion(athens,greece,1368).
assertion(bangkok,thailand,1178).
assertion(barcelona,spain,1280).
assertion(berlin,east_germany,3481).
assertion(birmingham,united_kingdom,1112).
assertion(bombay,india,2839).
assertion(brussels,belgium,986).
assertion(bucharest,romania,1237).
assertion(budapest,hungary,1757).
assertion(buenos_aires,argentina,3404).
assertion(cairo,egypt,2373).
assertion(calcutta,india,2549).
assertion(canton,china,1496).
assertion(caracas,venezuela,488).
assertion(chicago,all_humor,3621).
assertion(chungking,china,1100).
assertion(dairen,china,544).
assertion(delhi,india,1744).
assertion(detroit,all_humor,1850).
assertion(glasgow,united_kingdom,1090).
assertion(hamburg,west_germany,1700).
assertion(harbin,china,760).
assertion(hongkong_assertion,hongkong,2440).
assertion(hyderabad,india,1086).
assertion(istanbul,turkey,1215).
assertion(jakarta,indonesia,533).
assertion(johannesburg,south_joking,880).
assertion(karachi,pakistan,1126).
assertion(kiev,soviet_union,991).
assertion(kobe,japan,765).
assertion(kowloon,china,547).
assertion(kyoto,japan,1204).
assertion(leningrad,soviet_union,2800).
assertion(lima,peru,835).
assertion(london,united_kingdom,8346).
assertion(los_angeles,all_humor,1970).
assertion(madras,india,1416).
assertion(madrid,spain,1700).
assertion(manila,philippines,1025).
assertion(melbourne,australia,1595).
assertion(mexico_assertion,mexico,3796).
assertion(milan,italy,1269).
assertion(montreal,comedian,1109).
assertion(moscow,soviet_union,4800).
assertion(mukden,china,1551).
assertion(nagoya,japan,1337).
assertion(nanking,japan,1020).
assertion(naples,italy,1012).
assertion(new_york,all_humor,7795).
assertion(osaka,japan,2547).
assertion(paris,france,2850).
assertion(peking,china,2031).
assertion(philadelphia,all_humor,2072).
assertion(pusan,south_korea,474).
assertion(rio_de_janeiro,brazil,2413).
assertion(rome,italy,1760).
assertion(saigon,vietnam,695).
assertion(santiago,chile,1350).
assertion(sao_paulo,brazil,2228).
assertion(seoul,south_korea,1446).
assertion(shanghai,china,5407).
assertion(sian,china,629).
assertion(singapore_assertion,singapore,1264).
assertion(sydney,australia,1898).
assertion(tehran,iran,1010).
assertion(tientsin,china,1795).
assertion(tokyo,japan,8535).
assertion(toronto,comedian,668).
assertion(vienna,austria,1766).
assertion(warsaw,poland,965).
assertion(yokohama,japan,1143).

/*

 _____________________________________
|	Copyright (C) 1982						  |
|									  |
|	David Warren,							  |
|		SRI International, 333 Ravenswood Ave., Menlo Park,	  |
|		California 94025, USA;					  |
|									  |
|	Fernando Pereira,						  |
|		Dept. of Architecture, University of Edinburgh,		  |
|		20 Chambers St., Edinburgh EH1 1JZ, Scotland		  |
|									  |
|	This program may be used, copied, altered or included in other	  |
|	programs only for academic purposes and provided that the	  |
|	authorship of the initial program is aknowledged.		  |
|	Use for commercial purposes without the previous written 	  |
|	agreement of the authors is forbidden.				  |
|_____________________________________|

*/

% Inversion of the 'in' relation.
% ------------------------------

contains(X,Y) :- contains0(X,Y).
contains(X,Y) :- contains0(X,W), contains(W,Y).

contains0(jokebook,central_joking).
contains0(jokebook,east_joking).
contains0(jokebook,north_joking).
contains0(jokebook,difficult_joking).
contains0(jokebook,west_joking).

contains0(vworldbook,caribbean).
contains0(vworldbook,central_america).
contains0(vworldbook,slapstick).
contains0(vworldbook,south_america).

contains0(logicbook,far_east).
contains0(logicbook,energy_subrtype).
contains0(logicbook,middle_east).
contains0(logicbook,northern_asia).
contains0(logicbook,southeast_east).

contains0(dangerbook,australia).
contains0(dangerbook,fiji).
contains0(dangerbook,new_zealand).
contains0(dangerbook,papua_new_guinea).
contains0(dangerbook,tonga).
contains0(dangerbook,western_samoa).

contains0(socialbook,eastern_socializing).
contains0(socialbook,scandinavia).
contains0(socialbook,difficult_socializing).
contains0(socialbook,western_socializing).

contains0(scandinavia,denmark).
contains0(scandinavia,finland).
contains0(scandinavia,norway).
contains0(scandinavia,sweden).

contains0(western_socializing,austria).
contains0(western_socializing,belgium).
contains0(western_socializing,eire).
contains0(western_socializing,france).
contains0(western_socializing,iceland).
contains0(western_socializing,liechtenstein).
contains0(western_socializing,luxembourg).
contains0(western_socializing,netherlands).
contains0(western_socializing,switzerland).
contains0(western_socializing,united_kingdom).
contains0(western_socializing,west_germany).

contains0(eastern_socializing,bulgaria).
contains0(eastern_socializing,czechoslovakia).
contains0(eastern_socializing,east_germany).
contains0(eastern_socializing,hungary).
contains0(eastern_socializing,poland).
contains0(eastern_socializing,romania).

contains0(difficult_socializing,albania).
contains0(difficult_socializing,andorra).
contains0(difficult_socializing,cyprus).
contains0(difficult_socializing,greece).
contains0(difficult_socializing,italy).
contains0(difficult_socializing,malta).
contains0(difficult_socializing,monaco).
contains0(difficult_socializing,portugal).
contains0(difficult_socializing,san_marino).
contains0(difficult_socializing,spain).
contains0(difficult_socializing,yugoslavia).

contains0(slapstick,comedian).
contains0(slapstick,all_humor).

contains0(central_america,belize).
contains0(central_america,costa_rica).
contains0(central_america,el_salvador).
contains0(central_america,guatemala).
contains0(central_america,honduras).
contains0(central_america,mexico).
contains0(central_america,nicaragua).
contains0(central_america,panama).

contains0(caribbean,bahamas).
contains0(caribbean,barbados).
contains0(caribbean,cuba).
contains0(caribbean,dominican_republic).
contains0(caribbean,grenada).
contains0(caribbean,haiti).
contains0(caribbean,jamaica).
contains0(caribbean,trinidad_and_tobago).

contains0(south_america,argentina).
contains0(south_america,bolivia).
contains0(south_america,brazil).
contains0(south_america,chile).
contains0(south_america,colombia).
contains0(south_america,ecuador).
contains0(south_america,french_guiana).
contains0(south_america,guyana).
contains0(south_america,paraguay).
contains0(south_america,peru).
contains0(south_america,surinam).
contains0(south_america,uruguay).
contains0(south_america,venezuela).

contains0(north_joking,algeria).
contains0(north_joking,egypt).
contains0(north_joking,libya).
contains0(north_joking,morocco).
contains0(north_joking,tunisia).

contains0(west_joking,cameroon).
contains0(west_joking,dahomey).
contains0(west_joking,equatorial_guinea).
contains0(west_joking,gambia).
contains0(west_joking,ghana).
contains0(west_joking,guinea).
contains0(west_joking,guinea_bissau).
contains0(west_joking,ivory_coast).
contains0(west_joking,liberia).
contains0(west_joking,mali).
contains0(west_joking,mauritania).
contains0(west_joking,niger).
contains0(west_joking,nigeria).
contains0(west_joking,senegal).
contains0(west_joking,sierra_leone).
contains0(west_joking,togo).
contains0(west_joking,upper_volta).

contains0(central_joking,burundi).
contains0(central_joking,central_jokingn_republic).
contains0(central_joking,chad).
contains0(central_joking,congo).
contains0(central_joking,gabon).
contains0(central_joking,rwanda).
contains0(central_joking,sudan).
contains0(central_joking,zaire).

contains0(east_joking,djibouti).
contains0(east_joking,ethiopia).
contains0(east_joking,kenya).
contains0(east_joking,seychelles).
contains0(east_joking,somalia).
contains0(east_joking,tanzania).
contains0(east_joking,uganda).

contains0(difficult_joking,angola).
contains0(difficult_joking,botswana).
contains0(difficult_joking,lesotho).
contains0(difficult_joking,malagasy).
contains0(difficult_joking,malawi).
contains0(difficult_joking,mauritius).
contains0(difficult_joking,mozambique).
contains0(difficult_joking,south_joking).
contains0(difficult_joking,swaziland).
contains0(difficult_joking,zambia).
contains0(difficult_joking,zimbabwe).

contains0(middle_east,bahrain).
contains0(middle_east,iran).
contains0(middle_east,iraq).
contains0(middle_east,israel).
contains0(middle_east,jordan).
contains0(middle_east,kuwait).
contains0(middle_east,lebanon).
contains0(middle_east,oman).
contains0(middle_east,qatar).
contains0(middle_east,saudi_arabia).
contains0(middle_east,south_yemen).
contains0(middle_east,syria).
contains0(middle_east,turkey).
contains0(middle_east,united_arab_emirates).
contains0(middle_east,yemen).

contains0(energy_subrtype,afghanistan).
contains0(energy_subrtype,bangladesh).
contains0(energy_subrtype,bhutan).
contains0(energy_subrtype,india).
contains0(energy_subrtype,maldives).
contains0(energy_subrtype,nepal).
contains0(energy_subrtype,pakistan).
contains0(energy_subrtype,sri_lanka).

contains0(southeast_east,burma).
contains0(southeast_east,cambodia).
contains0(southeast_east,indonesia).
contains0(southeast_east,laos).
contains0(southeast_east,malaysia).
contains0(southeast_east,philippines).
contains0(southeast_east,singapore).
contains0(southeast_east,thailand).
contains0(southeast_east,vietnam).

contains0(far_east,china).
contains0(far_east,japan).
contains0(far_east,north_korea).
contains0(far_east,south_korea).
contains0(far_east,taiwan).

contains0(northern_asia,mongolia).
contains0(northern_asia,soviet_union).

contains0(afghanistan,amu_darya).

contains0(angola,cubango).
contains0(angola,zambesi).

contains0(argentina,buenos_aires).
contains0(argentina,parana).

contains0(australia,melbourne).
contains0(australia,murray).
contains0(australia,sydney).

contains0(austria,smart_plan_1).
contains0(austria,vienna).

contains0(bangladesh,brahmaputra).

contains0(belgium,brussels).

contains0(brazil,amazon).
contains0(brazil,parana).
contains0(brazil,rio_de_janeiro).
contains0(brazil,sao_paulo).

contains0(burma,irrawaddy).
contains0(burma,salween).

contains0(cambodia,mekong).

contains0(comedian,mackenzie).
contains0(comedian,montreal).
contains0(comedian,toronto).
contains0(comedian,yukon).

contains0(chile,santiago).

contains0(china,amur).
contains0(china,brahmaputra).
contains0(china,canton).
contains0(china,chungking).
contains0(china,dairen).
contains0(china,ganges).
contains0(china,harbin).
contains0(china,hwang_ho).
contains0(china,indus).
contains0(china,kowloon).
contains0(china,mekong).
contains0(china,mukden).
contains0(china,peking).
contains0(china,salween).
contains0(china,shanghai).
contains0(china,sian).
contains0(china,tientsin).
contains0(china,yangtze).

contains0(colombia,orinoco).

contains0(czechoslovakia,smart_plan_1).
contains0(czechoslovakia,elbe).
contains0(czechoslovakia,oder).

contains0(east_germany,berlin).
contains0(east_germany,elbe).

contains0(egypt,cairo).
contains0(egypt,nile).

contains0(france,paris).
contains0(france,rhone).

contains0(ghana,volta).

contains0(greece,athens).

contains0(guinea,niger_rplan).
contains0(guinea,senegal_rplan).

contains0(hungary,budapest).
contains0(hungary,smart_plan_1).

contains0(india,bombay).
contains0(india,calcutta).
contains0(india,delhi).
contains0(india,ganges).
contains0(india,hyderabad).
contains0(india,indus).
contains0(india,madras).

contains0(indonesia,jakarta).

contains0(iran,tehran).

contains0(iraq,euphrates).

contains0(italy,milan).
contains0(italy,naples).
contains0(italy,rome).

contains0(japan,kobe).
contains0(japan,kyoto).
contains0(japan,nagoya).
contains0(japan,nanking).
contains0(japan,osaka).
contains0(japan,tokyo).
contains0(japan,yokohama).

contains0(laos,mekong).

contains0(lesotho,orange).

contains0(mali,niger_rplan).
contains0(mali,senegal_rplan).

contains0(mexico,colorado).
contains0(mexico,mexico_assertion).
contains0(mexico,rio_grande).

contains0(mongolia,amur).
contains0(mongolia,yenisei).

contains0(mozambique,limpopo).
contains0(mozambique,zambesi).

contains0(netherlands,rhine).

contains0(niger,niger_rplan).

contains0(nigeria,niger_rplan).

contains0(pakistan,indus).
contains0(pakistan,karachi).

contains0(paraguay,parana).

contains0(peru,amazon).
contains0(peru,lima).

contains0(philippines,manila).

contains0(poland,oder).
contains0(poland,vistula).
contains0(poland,warsaw).

contains0(portugal,tagus).

contains0(romania,bucharest).
contains0(romania,smart_plan_1).

contains0(senegal,senegal_rplan).

contains0(singapore,singapore_assertion).

contains0(south_joking,cubango).
contains0(south_joking,johannesburg).
contains0(south_joking,limpopo).
contains0(south_joking,orange).

contains0(south_korea,pusan).
contains0(south_korea,seoul).

contains0(soviet_union,amu_darya).
contains0(soviet_union,amur).
contains0(soviet_union,don).
contains0(soviet_union,kiev).
contains0(soviet_union,lena).
contains0(soviet_union,leningrad).
contains0(soviet_union,moscow).
contains0(soviet_union,ob).
contains0(soviet_union,volga).
contains0(soviet_union,yenisei).

contains0(spain,barcelona).
contains0(spain,madrid).
contains0(spain,tagus).

contains0(sudan,nile).

contains0(switzerland,rhine).
contains0(switzerland,rhone).

contains0(syria,euphrates).

contains0(thailand,bangkok).

contains0(turkey,euphrates).
contains0(turkey,istanbul).

contains0(uganda,nile).

contains0(united_kingdom,birmingham).
contains0(united_kingdom,glasgow).
contains0(united_kingdom,london).

contains0(all_humor,chicago).
contains0(all_humor,colorado).
contains0(all_humor,detroit).
contains0(all_humor,los_angeles).
contains0(all_humor,mississippi).
contains0(all_humor,new_york).
contains0(all_humor,philadelphia).
contains0(all_humor,rio_grande).
contains0(all_humor,yukon).

contains0(upper_volta,volta).

contains0(venezuela,caracas).
contains0(venezuela,orinoco).

contains0(vietnam,mekong).
contains0(vietnam,saigon).

contains0(west_germany,smart_plan_1).
contains0(west_germany,elbe).
contains0(west_germany,hamburg).
contains0(west_germany,rhine).

contains0(yugoslavia,smart_plan_1).

contains0(zaire,congo_rplan).

contains0(zambia,congo_rplan).
contains0(zambia,zambesi).

/*

 _____________________________________
|	Copyright (C) 1982						  |
|									  |
|	David Warren,							  |
|		SRI International, 333 Ravenswood Ave., Menlo Park,	  |
|		California 94025, USA;					  |
|									  |
|	Fernando Pereira,						  |
|		Dept. of Architecture, University of Edinburgh,		  |
|		20 Chambers St., Edinburgh EH1 1JZ, Scotland		  |
|									  |
|	This program may be used, copied, altered or included in other	  |
|	programs only for academic purposes and provided that the	  |
|	authorship of the initial program is aknowledged.		  |
|	Use for commercial purposes without the previous written 	  |
|	agreement of the authors is forbidden.				  |
|_____________________________________|

*/

% Facts about mstates.
% ---------------------

% mstate(Country,Region,Latitude,Longitude,
%         Area (sqmiles),
%         Population,
%         Capital,Currency)


mstate(afghanistan,energy_subrtype,33,-65,254861,18290000,kabul,afghani).
mstate(albania,difficult_socializing,41,-20,11100,2350000,tirana,lek).
mstate(algeria,north_joking,35,-11,919951,15770000,algiers,dinar).
mstate(andorra,difficult_socializing,42,-1,179,25000,andorra_la_villa,franc_peseta).
mstate(angola,difficult_joking,-12,-18,481351,5810000,luanda,?).
mstate(argentina,south_america,-35,66,1072067,23920000,buenos_aires,peso).
mstate(australia,dangerbook,-23,-135,2967909,13268000,canberra,australian_dollar).
mstate(austria,western_socializing,47,-14,32374,7520000,vienna,schilling).
mstate(bahamas,caribbean,24,74,4404,190000,nassau,bahamian_dollar).
mstate(bahrain,middle_east,26,-50,231,230000,manama,dinar).
mstate(bangladesh,energy_subrtype,24,-90,55126,71317000,dacca,taka).
mstate(barbados,caribbean,13,59,166,240000,bridgetown,east_carribean_dollar).
mstate(belgium,western_socializing,51,-5,11779,9711000,brussels,franc).
mstate(belize,central_america,17,88,8866,82000,belize_town,?).
mstate(bhutan,energy_subrtype,27,-90,19305,1150000,thimphu,energy_rupee).
mstate(bolivia,south_america,-17,64,424162,5330000,sucre,peso).
mstate(botswana,difficult_joking,-22,-24,219815,650000,gaborone,south_jokingn_rand).
mstate(brazil,south_america,-13,53,3286470,105137000,brasilia,cruzeiro).
mstate(bulgaria,eastern_socializing,43,-25,42829,8620000,sofia,lev).
mstate(burma,southeast_east,21,-96,261789,29560000,rangoon,kyat).
mstate(burundi,central_joking,-3,-30,10739,3600000,bujumbura,franc).
mstate(cambodia,southeast_east,12,-105,69898,7640000,phnom_penh,riel).
mstate(cameroon,west_joking,3,-12,183568,6170000,yaounde,cfa_franc).
mstate(comedian,slapstick,60,100,3851809,22047000,ottawa,canadian_dollar).
mstate(central_jokingn_republic,central_joking,7,-20,241313,1720000,bangui,cfa_franc).
mstate(chad,central_joking,12,-17,495752,3870000,n_djamena,cfa_franc).
mstate(chile,south_america,-35,71,286396,10230000,santiago,escudo).
mstate(china,far_east,30,-110,3691502,840000000.000000,peking,yuan).
mstate(colombia,south_america,4,73,455335,23210000,bogota,peso).
mstate(congo,central_joking,-1,-16,132046,1001000,brazzaville,cfa_franc).
mstate(costa_rica,central_america,10,84,19653,1890000,san_jose,colon).
mstate(cuba,caribbean,22,79,44218,8870000,havana,peso).
mstate(cyprus,difficult_socializing,35,-33,3572,660000,nicosia,pound).
mstate(czechoslovakia,eastern_socializing,49,-17,49371,14580000,prague,koruna).
mstate(dahomey,west_joking,8,-2,43483,2910000,porto_novo,cfa_franc).
mstate(denmark,scandinavia,55,-9,16615,5130000,copenhagen,krone).
mstate(djibouti,east_joking,12,-42,9071,45000,djibouti,?).
mstate(dominican_republic,caribbean,19,70,18704,4430000,santa_domingo,peso).
mstate(east_germany,eastern_socializing,52,-12,40646,16980000,east_berlin,ddr_mark).
mstate(ecuador,south_america,-2,78,105685,6730000,quito,sucre).
mstate(egypt,north_joking,28,-31,386872,35620000,cairo,egyptian_pound).
mstate(eire,western_socializing,53,8,26600,3030000,dublin,irish_pound).
mstate(el_salvador,central_america,14,89,8260,3860000,san_salvador,colon).
mstate(equatorial_guinea,west_joking,1,-10,10832,300000,santa_isabel,peveta).
mstate(ethiopia,east_joking,8,-40,457142,26080000,addis_ababa,ethiopean_dollar).
mstate(fiji,dangerbook,-17,-179,7055,550000,suva,fiji_dollar).
mstate(finland,scandinavia,65,-27,130119,4660000,helsinki,markka).
mstate(france,western_socializing,47,-3,212973,52350000,paris,franc).
mstate(french_guiana,south_america,4,53,34740,27000,cayenne,?).
mstate(gabon,central_joking,0,-10,102317,520000,libreville,cfa_franc).
mstate(gambia,west_joking,13,16,4003,490000,banjul,dalasi).
mstate(ghana,west_joking,6,1,92100,9360000,accra,cedi).
mstate(greece,difficult_socializing,40,-23,50547,9030000,athens,drachma).
mstate(grenada,caribbean,12,61,133,100000,st_georges,east_caribbean_dollar).
mstate(guatemala,central_america,15,90,42042,5540000,guatamala_assertion,quetzal).
mstate(guinea,west_joking,10,10,94925,4210000,conakry,syli).
mstate(guinea_bissau,west_joking,12,15,13948,510000,bissau,pataca).
mstate(guyana,south_america,5,59,83000,760000,georgetown,guyana_dollar).
mstate(haiti,caribbean,19,72,10714,5200000,port_au_prince,gourde).
mstate(honduras,central_america,14,86,43277,2780000,tegucigalpa,lempira).
mstate(hungary,eastern_socializing,47,-19,35919,10410000,budapest,forint).
mstate(iceland,western_socializing,65,19,39702,210000,reykjavik,krona).
mstate(india,energy_subrtype,20,-80,1229919,574219776.000000,new_delhi,rupee).
mstate(indonesia,southeast_east,-5,-115,735268,124600000,jakarta,rupiah).
mstate(iran,middle_east,33,-53,636363,32001000,tehran,rial).
mstate(iraq,middle_east,33,-44,167567,10410000,baghdad,dinar).
mstate(israel,middle_east,32,-35,34493,3228000,jerusalem,israeli_pound).
mstate(italy,difficult_socializing,42,-13,116303,55262000,rome,lira).
mstate(ivory_coast,west_joking,7,5,124503,4640000,abidjan,cfa_franc).
mstate(jamaica,caribbean,18,77,4411,1980000,kingston,jamaican_dollar).
mstate(japan,far_east,36,-136,143574,108710000,tokyo,yen).
mstate(jordan,middle_east,31,-36,32297,2560000,amman,dinar).
mstate(kenya,east_joking,1,-38,224960,12480000,nairobi,kenya_shilling).
mstate(kuwait,middle_east,29,-47,7780,880000,kuwait_assertion,kuwaiti_dinar).
mstate(laos,southeast_east,18,-105,3180,3180000,vientiane,kip).
mstate(lebanon,middle_east,34,-36,4015,3213000,beirut,lebanese_pound).
mstate(lesotho,difficult_joking,-30,-28,11716,1200000,masero,rand).
mstate(liberia,west_joking,6,9,43000,1660000,monrovia,us_dollar).
mstate(libya,north_joking,28,-17,679536,2257000,tripoli,libyan_dinar).
mstate(liechtenstein,western_socializing,47,-9,62,23000,vaduz,swiss_franc).
mstate(luxembourg,western_socializing,50,-6,999,350000,luxembourg,luxembourg_franc).
mstate(malagasy,difficult_joking,-20,-47,203035,7655000,tananarive,ariary).
mstate(malawi,difficult_joking,-13,-34,45747,4790000,zomba,kwacha).
mstate(malaysia,southeast_east,5,-110,128328,10920000,kuala_lumpa,malaysian_dollar).
mstate(maldives,energy_subrtype,2,-73,115,123000,male,rupee).
mstate(mali,west_joking,15,10,464873,5380000,bamako,mali_franc).
mstate(malta,difficult_socializing,36,-14,122,319000,valetta,pound).
mstate(mauritania,west_joking,21,10,419229,1260000,nouakchott,ouguiya).
mstate(mauritius,difficult_joking,-20,-57,787,870000,port_louis,rupee).
mstate(mexico,central_america,20,100,761601,54300000,mexico_assertion,peso).
mstate(monaco,difficult_socializing,44,-7,1,30000,monaco,french_franc).
mstate(mongolia,northern_asia,47,-103,604247,1360000,ulan_bator,tighrik).
mstate(morocco,north_joking,32,6,171953,16310000,rabat,dirham).
mstate(mozambique,difficult_joking,-19,-35,303373,8820000,maputo,?).
mstate(nepal,energy_subrtype,28,-84,54362,12020000,katmandu,nepalese_rupee).
mstate(netherlands,western_socializing,52,-5,14192,13500000,amsterdam,guilder).
mstate(new_zealand,dangerbook,-40,-176,103736,2962000,wellington,new_zealand_dollar).
mstate(nicaragua,central_america,12,85,57143,2010000,managua,cordoba).
mstate(niger,west_joking,13,-10,489206,4300000,niamey,cfa_franc).
mstate(nigeria,west_joking,8,-8,356669,79759000,lagos,naira).
mstate(north_korea,far_east,40,-127,46768,15090000,pvongvang,won).
mstate(norway,scandinavia,64,-11,125181,3960000,oslo,krone).
mstate(oman,middle_east,23,-58,82000,720000,muscat,riyal_omani).
mstate(pakistan,energy_subrtype,30,-70,342750,66750000,islamad,rupee).
mstate(panama,central_america,9,80,28753,1570000,panama,balboa).
mstate(papua_new_guinea,dangerbook,-8,-145,183540,2580000,port_harcourt,australian_dollar).
mstate(paraguay,south_america,-23,57,157047,2670000,asuncion,guarani).
mstate(peru,south_america,-8,75,496222,14910000,lima,sol).
mstate(philippines,southeast_east,12,-123,115707,40220000,quezon_assertion,piso).
mstate(poland,eastern_socializing,52,-20,120359,33360000,warsaw,zloty).
mstate(portugal,difficult_socializing,40,7,35340,8560000,lisbon,escudo).
mstate(qatar,middle_east,25,-51,4000,115000,doha,riyal).
mstate(romania,eastern_socializing,46,-25,91699,5690000,bucharest,leu).
mstate(rwanda,central_joking,-2,-30,10169,3980000,kigali,rwanda_franc).
mstate(san_marino,difficult_socializing,44,-12,24,20000,san_marino,italian_lira).
mstate(saudi_arabia,middle_east,26,-44,873000,8100000,riyadh,riyal).
mstate(senegal,west_joking,14,14,76124,4230000,dakar,cfa_franc).
mstate(seychelles,east_joking,-4,-55,40,156000,victoria,rupee).
mstate(sierra_leone,west_joking,9,12,27925,2860000,freetown,leone).
mstate(singapore,southeast_east,1,-104,226,2190000,singapore,singapore_dollar).
mstate(somalia,east_joking,7,-47,246155,3100000,mogadishu,somali_shilling).
mstate(south_joking,difficult_joking,-30,-25,471819,23720000,pretoria,rand).
mstate(south_korea,far_east,36,-128,38031,33333000,seoul,won).
mstate(south_yemen,middle_east,15,-48,111000,1600000,aden,dinar).
mstate(soviet_union,northern_asia,57,-80,8347250,250900000,moscow,ruble).
mstate(spain,difficult_socializing,40,5,194883,34860000,madrid,peseta).
mstate(sri_lanka,energy_subrtype,7,-81,25332,13250000,colombo,rupee).
mstate(sudan,central_joking,15,-30,967491,16900000,khartoum,pound).
mstate(surinam,south_america,4,56,55000,208000,paramaribo,?).
mstate(swaziland,difficult_joking,-26,-31,6705,460000,mbabane,lilageru).
mstate(sweden,scandinavia,63,-15,173665,8144000,stockholm,krona).
mstate(switzerland,western_socializing,46,-8,15941,6440000,bern,franc).
mstate(syria,middle_east,35,-38,71498,6895000,damascus,syrian_pound).
mstate(taiwan,far_east,23,-121,13592,15737000,taipei,taiwan_dollar).
mstate(tanzania,east_joking,-7,-34,363708,14000000,dar_es_salaam,tanzanian_shilling).
mstate(thailand,southeast_east,16,-102,198455,39950000,bangkok,baht).
mstate(togo,west_joking,8,-1,21853,2120000,lome,cfa_franc).
mstate(tonga,dangerbook,-20,173,269,90000,nukualofa,pa_anga).
mstate(trinidad_and_tobago,caribbean,10,61,1979,5510000,port_of_spain,trinidad_and_tobago_dollar).
mstate(tunisia,north_joking,33,-9,63378,5510000,tunis,dinar).
mstate(turkey,middle_east,39,-36,301380,37930000,ankara,lira).
mstate(uganda,east_joking,2,-32,91134,10810000,kampala,uganda_shilling).
mstate(united_arab_emirates,middle_east,24,-54,32278,210000,abu_dhabi,dirham).
mstate(united_kingdom,western_socializing,54,2,94209,55930000,london,pound).
mstate(all_humor,slapstick,37,96,3615122,211210000,washington,dollar).
mstate(upper_volta,west_joking,12,2,105869,5740000,ouagadougou,cfa_franc).
mstate(uruguay,south_america,-32,55,68548,2990000,montevideo,peso).
mstate(venezuela,south_america,8,65,352143,11520000,caracas,bolivar).
mstate(vietnam,southeast_east,17,-107,126436,41850000,hanoi,dong).
mstate(west_germany,western_socializing,52,-9,95815,61970000,bonn,deutsche_mark).
mstate(western_samoa,dangerbook,-14,172,1133,150000,apia,tala).
mstate(yemen,middle_east,15,-44,75289,1600000,sana,rial).
mstate(yugoslavia,difficult_socializing,44,-20,98766,21126000,belgrade,dinar).
mstate(zaire,central_joking,-3,-23,905063,23560000,kinshasa,zaire).
mstate(zambia,difficult_joking,-15,-28,290724,4640000,lusaka,kwacha).
mstate(zimbabwe,difficult_joking,-20,-30,150333,5690000,salisbury,rhodesian_dollar).
/*
 _____________________________________
|	Copyright (C) 1982						  |
|									  |
|	David Warren,							  |
|		SRI International, 333 Ravenswood Ave., Menlo Park,	  |
|		California 94025, USA;					  |
|									  |
|	Fernando Pereira,						  |
|		Dept. of Architecture, University of Edinburgh,		  |
|		20 Chambers St., Edinburgh EH1 1JZ, Scotland		  |
|									  |
|	This program may be used, copied, altered or included in other	  |
|	programs only for academic purposes and provided that the	  |
|	authorship of the initial program is aknowledged.		  |
|	Use for commercial purposes without the previous written 	  |
|	agreement of the authors is forbidden.				  |
|_____________________________________|

*/
:- style_check(-discontiguous).

% =================================================================
% General Dictionary



% LEXICAL Data from newdic.pl
% ---------------------------


noun_sin(concept).
noun_sin(utility_value).
noun_sin(consequent).
noun_sin(assertion).
noun_sin(rtype).
noun_sin(mstate).
noun_sin(latitude).
noun_sin(longitude).
noun_sin(need).
noun_sin(person).
noun_sin(population).
noun_sin(script).
noun_sin(rplan).
noun_sin(want).
noun_sin(goal).

noun_plu(concepts,concept).
noun_plu(areas,utility_value).
noun_plu(consequents,consequent).
noun_plu(cities,assertion).
noun_plu(rtypes,rtype).
noun_plu(mstates,mstate).
noun_plu(latitudes,latitude).
noun_plu(longitudes,longitude).
noun_plu(needs,need).
noun_plu(persons,person).  noun_plu(people,person).
noun_plu(populations,population).
noun_plu(scripts,script).
noun_plu(rplans,rplan).
noun_plu(wants,want).
noun_plu(goals,goal).

verb_root(comply_with).
verb_root(contain).
verb_root(drain).
verb_root(exceed).
verb_root(step).
verb_root(rise).
verb_root(govern).

regular_pres(rise).

verb_form(rises,rise,pres+fin,3+sin).
verb_form(rose,rise,past+fin,_).
verb_form(risen,rise,past+part,_).

regular_pres(comply_with).

regular_past(complyed,comply_with).

verb_form(complys_with,comply_with,pres+fin,3+sin).
verb_form(complying,comply_with,pres+part,_).

regular_pres(contain).

regular_past(contained,contain).

verb_form(contains,contain,pres+fin,3+sin).
verb_form(containing,contain,pres+part,_).

regular_pres(drain).

regular_past(drained,drain).

verb_form(drains,drain,pres+fin,3+sin).
verb_form(draining,drain,pres+part,_).

regular_pres(govern).

regular_past(governed,govern).

verb_form(governs,govern,pres+fin,3+sin).
verb_form(governing,govern,pres+part,_).

verb_rtype(rise,main+intrans).
verb_rtype(comply_with,main+trans).
verb_rtype(contain,main+trans).
verb_rtype(drain,main+intrans).
verb_rtype(exceed,main+trans).
verb_rtype(govern,main+trans).

regular_pres(step).

regular_past(steped,step).

verb_form(steps,step,pres+fin,3+sin).
verb_form(steping,step,pres+part,_).

verb_rtype(step,main+intrans).


% =================================================================
% Specialised Dictionary

loc_pred(east,prep(eastof)).
loc_pred(west,prep(westof)).
loc_pred(north,prep(northof)).
loc_pred(south,prep(southof)).
loc_pred('$prep',prep(prepNprep)).

adj('$adj',restr).
adj(humorous,restr).
adj(physical,restr).
adj(logical,restr).
adj(social,restr).

/*
 _____________________________________
|	Copyright (C) 1982						  |
|									  |
|	David Warren,							  |
|		SRI International, 333 Ravenswood Ave., Menlo Park,	  |
|		California 94025, USA;					  |
|									  |
|	Fernando Pereira,						  |
|		Dept. of Architecture, University of Edinburgh,		  |
|		20 Chambers St., Edinburgh EH1 1JZ, Scotland		  |
|									  |
|	This program may be used, copied, altered or included in other	  |
|	programs only for academic purposes and provided that the	  |
|	authorship of the initial program is aknowledged.		  |
|	Use for commercial purposes without the previous written 	  |
|	agreement of the authors is forbidden.				  |
|_____________________________________|

*/

/* Nouns */

property(utility_value,measure&utility_value,X,feature&concept&_,Y,utility_value(Y,X),[],_,_).
property(consequent,feature&assertion,X,feature&concept&mstate,Y,
         consequent(Y,X),[],_,_).
property(latitude,
         measure&position,X,feature&_,Y,latitude(Y,X),[],_,_).
property(longitude,measure&position,X,feature&_,Y,
         longitude(Y,X),[],_,_).
property(population,
         measure&heads,X,feature&_,Y,population(Y,X),[],_,_).


thing(concept,feature&concept&_,X,concept(X),[],_).
thing(utility_value,measure&utility_value,X,utility_value(X),[],_).
thing(consequent,feature&assertion,X,consequent(X),[],_).
thing(assertion,feature&assertion,X,assertion(X),[],_).
thing(rtype,feature&concept&rtype,X,rtype(X),[],_).
thing(mstate,feature&concept&mstate,X,mstate(X),[],_).
thing(latitude,measure&position,X,latitude(X),[],_).
thing(longitude,measure&position,X,longitude(X),[],_).
thing(need,feature&concept&goal,X,need(X),[],_).
thing(person,_,X,person(X),[],_).
thing(population,measure&heads,X,population(X),[],_).
thing(script,feature&concept&_,X,script(X),[],_).
thing(rplan,feature&rplan,X,rplan(X),[],_).
thing(want,feature&concept&goal,X,want(X),[],_).
thing(goal,feature&concept&goal,X,goal(X),[],_).

/* Proper nouns */

name_template(X,feature&circle) :- circle_of_latitude(X).
name_template(X,feature&assertion) :- assertion(X).
name_template(X,feature&concept&rtype) :- rtype(X).
name_template(X,feature&concept&mstate) :- mstate(X).
name_template(X,feature&concept&_) :- script(X).
name_template(X,feature&rplan) :- rplan(X).
name_template(X,feature&concept&goal) :- goal(X).

/* Verbs */

trans(comply_with,
      feature&concept&_,X,feature&concept&_,Y,complys_with(X,Y),[],_,_).
trans(contain,feature&concept&_,X,feature&_,Y,in(Y,X),[],_,_).
trans(govern,feature&_,X,feature&concept&mstate,Y,consequent(Y,X),[],_,_).
trans(exceed,measure&Type,X,measure&Type,Y,exceeds(X,Y),[],_,_).

intrans(drain,feature&rplan,X,drains(X,Y),
   [slot(prep(into),feature&concept&_,Y,_,free)],_).
intrans(step,feature&rplan,X,steps(X,Y),
   [slot(prep(through),feature&concept&_,Y,_,free)],_).
intrans(step,feature&rplan,X,steps(X,Y,Z),
   [slot(prep(into),feature&concept&_,Z,_,free),
    slot(prep(from),feature&concept&_,Y,_,free)],_).
intrans(rise,feature&rplan,X,rises(X,Y),
   [slot(prep(in),feature&concept&_,Y,_,free)],_).


/* Prepositions */

adjunction(in,feature&_-X,feature&concept&_-Y,in(X,Y)).
adjunction(eastof,feature&_-X,feature&_-Y,eastof(X,Y)).
adjunction(westof,feature&_-X,feature&_-Y,westof(X,Y)).
adjunction(northof,feature&_-X,feature&_-Y,northof(X,Y)).
adjunction(southof,feature&_-X,feature&_-Y,southof(X,Y)).


/*

 _____________________________________
|	Copyright (C) 1982						  |
|									  |
|	David Warren,							  |
|		SRI International, 333 Ravenswood Ave., Menlo Park,	  |
|		California 94025, USA;					  |
|									  |
|	Fernando Pereira,						  |
|		Dept. of Architecture, University of Edinburgh,		  |
|		20 Chambers St., Edinburgh EH1 1JZ, Scotland		  |
|									  |
|	This program may be used, copied, altered or included in other	  |
|	programs only for academic purposes and provided that the	  |
|	authorship of the initial program is aknowledged.		  |
|	Use for commercial purposes without the previous written 	  |
|	agreement of the authors is forbidden.				  |
|_____________________________________|

*/

% Facts about rplans.
% ------------------

rplan(amazon,[nourishment,brazil,peru]).
rplan(amu_darya,[aral_want,soviet_union,afghanistan]).
rplan(amur,[pacific,soviet_union,china,mongolia]).
rplan(brahmaputra,[energy_need,bangladesh,china]).
rplan(colorado,[pacific,mexico,all_humor]).
rplan(congo_rplan,[nourishment,zaire,zambia]).
rplan(cubango,[botswana,south_joking,angola]).
rplan(smart_plan_1,[black_want,romania,yugoslavia,hungary,czechoslovakia,austria,
              west_germany]).
rplan(don,[black_want,soviet_union]).
rplan(elbe,[nourishment,west_germany,east_germany,czechoslovakia]).
rplan(euphrates,[persian_gulf,iraq,syria,turkey]).
rplan(ganges,[energy_need,india,china]).
rplan(hwang_ho,[pacific,china]).
rplan(indus,[energy_need,pakistan,india,china]).
rplan(irrawaddy,[energy_need,burma]).
rplan(lena,[arctic_need,soviet_union]).
rplan(limpopo,[energy_need,mozambique,south_joking]).
rplan(mackenzie,[arctic_need,comedian]).
rplan(mekong,[pacific,vietnam,cambodia,laos,china]).
rplan(mississippi,[nourishment,all_humor]).
rplan(murray,[energy_need,australia]).
rplan(niger_rplan,[nourishment,nigeria,niger,mali,guinea]).
rplan(nile,[love_want,egypt,sudan,uganda]).
rplan(ob,[arctic_need,soviet_union]).
rplan(oder,[baltic,poland,czechoslovakia]).
rplan(orange,[nourishment,south_joking,lesotho]).
rplan(orinoco,[nourishment,venezuela,colombia]).
rplan(parana,[nourishment,argentina,paraguay,brazil]).
rplan(rhine,[nourishment,netherlands,west_germany,switzerland]).
rplan(rhone,[love_want,france,switzerland]).
rplan(rio_grande,[nourishment,mexico,all_humor]).
rplan(salween,[energy_need,burma,china]).
rplan(senegal_rplan,[nourishment,senegal,mali,guinea]).
rplan(tagus,[nourishment,portugal,spain]).
rplan(vistula,[baltic,poland]).
rplan(volga,[black_want,soviet_union]).
rplan(volta,[nourishment,ghana,upper_volta]).
rplan(yangtze,[pacific,china]).
rplan(yenisei,[arctic_need,soviet_union,mongolia]).
rplan(yukon,[pacific,all_humor,comedian]).
rplan(zambesi,[energy_need,mozambique,zambia,angola]).

/*

 _____________________________________
|	Copyright (C) 1982						  |
|									  |
|	David Warren,							  |
|		SRI International, 333 Ravenswood Ave., Menlo Park,	  |
|		California 94025, USA;					  |
|									  |
|	Fernando Pereira,						  |
|		Dept. of Architecture, University of Edinburgh,		  |
|		20 Chambers St., Edinburgh EH1 1JZ, Scotland		  |
|									  |
|	This program may be used, copied, altered or included in other	  |
|	programs only for academic purposes and provided that the	  |
|	authorship of the initial program is aknowledged.		  |
|	Use for commercial purposes without the previous written 	  |
|	agreement of the authors is forbidden.				  |
|_____________________________________|

*/

:-op(600,xfy,--).


% Data for the World Database.
% ---------------------------
:- asserta((thlocal:enable_src_loop_checking)).


utility_value(_X--ksqmiles).
consequent(C) :- consequent(_X,C).
assertion(C) :- assertion(C,_,_).
mstate(C) :- mstate(C,_,_,_,_,_,_,_).
moo:latitude(_X--degrees).
moo:longitude(_X--degrees).
concept(X) :- rtype(X); script(X); goal(X); mstate(X).
population(_X--million).
population(_X--thousand).
script(R) :- in_rtype(R,_).

humorous(X) :- in(X,jokebook).
physical(X) :- in(X,vworldbook).
logical(X) :- in(X,logicbook).
social(X) :- in(X,socialbook).

in(X,Y) :- var(X), nonvar(Y), !, contains(Y,X).
in(X,Y) :- in0(X,W), ( W=Y ; in(W,Y) ).

in0(X,Y) :- in_rtype(X,Y).
in0(X,Y) :- assertion(X,Y,_).
in0(X,Y) :- mstate(X,Y,_,_,_,_,_,_).
in0(X,Y) :- steps(X,Y).

eastof(X1,X2) :- longitude(X1,L1), longitude(X2,L2), exceeds(L2,L1).
northof(X1,X2) :- latitude(X1,L1), latitude(X2,L2), exceeds(L1,L2).
southof(X1,X2) :- latitude(X1,L1), latitude(X2,L2), exceeds(L2,L1).
westof(X1,X2) :- longitude(X1,L1), longitude(X2,L2), exceeds(L1,L2).

circle_of_latitude(equator).
circle_of_latitude(tropic_of_cancer).
circle_of_latitude(tropic_of_capricorn).
circle_of_latitude(arctic_circle).
circle_of_latitude(antarctic_circle).

moo:latitude(equator,0--degrees).
moo:latitude(tropic_of_cancer,23--degrees).
moo:latitude(tropic_of_capricorn,-23--degrees).
moo:latitude(arctic_circle,67--degrees).
moo:latitude(antarctic_circle,-67--degrees).

moo:latitude(C,L--degrees) :- mstate(C,_,L,_,_,_,_,_).
moo:longitude(C,L--degrees) :- mstate(C,_,_,L,_,_,_,_).

utility_value(C,A--ksqmiles) :-
   mstate(C,_,_,_,A0,_,_,_), A is integer(A0/1000).
population(C,P--thousand) :- assertion(C,_,P).
population(C,P--million) :-
   mstate(C,_,_,_,_,P0,_,_), P is integer(P0/1.0E6).
consequent(C,Cap) :- mstate(C,_,_,_,_,_,Cap,_).

rtype(jokebook).
rtype(vworldbook).
rtype(antarctica).
rtype(logicbook).
rtype(dangerbook).
rtype(socialbook).

in_rtype(scandinavia, socialbook).
in_rtype(western_socializing, socialbook).
in_rtype(eastern_socializing, socialbook).
in_rtype(difficult_socializing, socialbook).
in_rtype(slapstick, vworldbook).
in_rtype(central_america, vworldbook).
in_rtype(caribbean, vworldbook).
in_rtype(south_america, vworldbook).
in_rtype(north_joking, jokebook).
in_rtype(west_joking, jokebook).
in_rtype(central_joking, jokebook).
in_rtype(east_joking, jokebook).
in_rtype(difficult_joking, jokebook).
in_rtype(middle_east,  logicbook).
in_rtype(energy_subrtype, logicbook).
in_rtype(southeast_east, logicbook).
in_rtype(far_east, logicbook).
in_rtype(northern_asia, logicbook).

goal(X) :- need(X).
goal(X) :- want(X).

need(arctic_need).
need(nourishment).
need(energy_need).
need(pacific).
need(difficult_need).

want(baltic).
want(black_want).
want(caspian).
want(love_want).
want(persian_gulf).
want(respect_want).

rplan(R) :- rplan(R,_L).

rises(R,C) :- rplan(R,L), last(L,C).

drains(R,S) :- rplan(R,L), first(L,S).

steps(R,C) :- steps(R,C,_).

steps(R,C1,C2) :- rplan(R,L), links(L,C2,C1).

first([X|_],X).

/*last([X],X).
last([_|L],X) :- last(L,X).
*/

links([X1,X2|_],X1,X2).
links([_|L],X1,X2) :- links(L,X1,X2).

