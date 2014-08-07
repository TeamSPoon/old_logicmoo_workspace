/*

 _________________________________________________________________________
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
|_________________________________________________________________________|

*/


must_test_80([what, rivers, are, there, ?], [sent([what, rivers, are, there, ?]), parse(whq(feature&river-B, s(np(3+plu, np_head(int_det(feature&river-B), [], river), []), verb(be, active, pres+fin, [], pos), [void], []))), sem((answer([A]):-river(A), A^true)), qplan((answer([B]):-river(B), B^true)), answers([amazon, amu_darya, amur, brahmaputra, colorado, congo_river, cubango, danube, don, elbe, euphrates, ganges, hwang_ho, indus, irrawaddy, lena, limpopo, mackenzie, mekong, mississippi, murray, niger_river, nile, ob, oder, orange, orinoco, parana, rhine, rhone, rio_grande, salween, senegal_river, tagus, vistula, volga, volta, yangtze, yenisei, yukon, zambesi])],[time(0.0)]).
must_test_80([does, afghanistan, border, china, ?], [sent([does, afghanistan, border, china, ?]), parse(q(s(np(3+sin, name(afghanistan), []), verb(border, active, pres+fin, [], pos), [arg(dir, np(3+sin, name(china), []))], []))), sem((answer([]):-borders(afghanistan, china))), qplan((answer([]):-{borders(afghanistan, china)})), answers([true])],[time(0.0)]).
must_test_80([what, is, the, capital, of, upper_volta, ?], [sent([what, is, the, capital, of, upper_volta, ?]), parse(whq(feature&city-B, s(np(3+sin, wh(feature&city-B), []), verb(be, active, pres+fin, [], pos), [arg(dir, np(3+sin, np_head(det(the(sin)), [], capital), [pp(prep(of), np(3+sin, name(upper_volta), []))]))], []))), sem((answer([A]):-capital(upper_volta, A))), qplan((answer([A]):-capital(upper_volta, A))), answers([ouagadougou])],[time(0.0010000000000000009)]).
must_test_80([where, is, the, largest, country, ?], [sent([where, is, the, largest, country, ?]), parse(whq(feature&place&A-B, s(np(3+sin, np_head(det(the(sin)), [sup(most, adj(large))], country), []), verb(be, active, pres+fin, [], pos), [arg(pred, pp(prep(in), np(_, np_head(int_det(feature&place&A-B), [], place), [])))], []))), sem((answer([A]):-B^ (C^ (setof(D:E, (country(E), area(E, D)), C), aggregate(max, C, B)), place(A), in(B, A)))), qplan((answer([F]):-E^D^ (setof(C:B, (country(B), area(B, C)), D), aggregate(max, D, E), in(E, F), {place(F)}))), answers([asia, northern_asia])],[time(0.0009999999999999731)]).
must_test_80([which, countries, are, european, ?], [sent([which, countries, are, european, ?]), parse(whq(feature&place&country-B, s(np(3+plu, np_head(int_det(feature&place&country-B), [], country), []), verb(be, active, pres+fin, [], pos), [arg(pred, adj(european))], []))), sem((answer([A]):-country(A), european(A))), qplan((answer([A]):-european(A), {country(A)})), answers([albania, andorra, austria, belgium, bulgaria, cyprus, czechoslovakia, denmark, east_germany, eire, finland, france, greece, hungary, iceland, italy, liechtenstein, luxembourg, malta, monaco, netherlands, norway, poland, portugal, romania, san_marino, spain, sweden, switzerland, united_kingdom, west_germany, yugoslavia])],[time(0.0)]).
must_test_80([which, country, '\'', s, capital, is, london, ?], [sent([which, country, '\'', s, capital, is, london, ?]), parse(whq(feature&place&country-B, s(np(3+sin, np_head(det(the(sin)), [], capital), [pp(poss, np(3+sin, np_head(int_det(feature&place&country-B), [], country), []))]), verb(be, active, pres+fin, [], pos), [arg(dir, np(3+sin, name(london), []))], []))), sem((answer([A]):-country(A), capital(A, london))), qplan((answer([A]):-capital(A, london), {country(A)})), answers([united_kingdom])],[time(0.0010000000000000009)]).
must_test_80([which, is, the, largest, african, country, ?], [sent([which, is, the, largest, african, country, ?]), parse(whq(feature&place&country-B, s(np(3+sin, wh(feature&place&country-B), []), verb(be, active, pres+fin, [], pos), [arg(dir, np(3+sin, np_head(det(the(sin)), [sup(most, adj(large)), adj(african)], country), []))], []))), sem((answer([A]):-B^ (setof(C:D, (country(D), area(D, C), african(D)), B), aggregate(max, B, A)))), qplan((answer([D]):-C^ (setof(B:A, (african(A), {country(A)}, area(A, B)), C), aggregate(max, C, D)))), answers([sudan])],[time(0.0)]).
must_test_80([how, large, is, the, smallest, american, country, ?], [sent([how, large, is, the, smallest, american, country, ?]), parse(whq(measure&area-B, s(np(3+sin, np_head(det(the(sin)), [sup(most, adj(small)), adj(american)], country), []), verb(be, active, pres+fin, [], pos), [arg(pred, value(adj(large), wh(measure&area-B)))], []))), sem((answer([A]):-B^ (C^ (setof(D:E, (country(E), area(E, D), american(E)), C), aggregate(min, C, B)), area(B, A)))), qplan((answer([E]):-D^C^ (setof(B:A, (american(A), {country(A)}, area(A, B)), C), aggregate(min, C, D), area(D, E)))), answers([0--ksqmiles])],[time(0.0)]).
must_test_80([what, is, the, ocean, that, borders, african, countries, and, that, borders, asian, countries, ?], [sent([what, is, the, ocean, that, borders, african, countries, and, that, borders, asian, countries, ?]), parse(whq(feature&place&seamass-B, s(np(3+sin, wh(feature&place&seamass-B), []), verb(be, active, pres+fin, [], pos), [arg(dir, np(3+sin, np_head(det(the(sin)), [], ocean), [conj(and, rel(feature&place&seamass-C, s(np(3+sin, wh(feature&place&seamass-C), []), verb(border, active, pres+fin, [], pos), [arg(dir, np(3+plu, np_head(generic, [adj(african)], country), []))], [])), rel(feature&place&seamass-C, s(np(3+sin, wh(feature&place&seamass-C), []), verb(border, active, pres+fin, [], pos), [arg(dir, np(3+plu, np_head(generic, [adj(asian)], country), []))], [])))]))], []))), sem((answer([A]):-ocean(A), B^ (country(B), african(B), borders(A, B)), C^ (country(C), asian(C), borders(A, C)))), qplan((answer([A]):-B^C^ (ocean(A), {borders(A, B), {african(B)}, {country(B)}}, {borders(A, C), {asian(C)}, {country(C)}}))), answers([indian_ocean])],[time(0.0020000000000000018)]).
must_test_80([what, are, the, capitals, of, the, countries, bordering, the, baltic, ?], [sent([what, are, the, capitals, of, the, countries, bordering, the, baltic, ?]), parse(whq(feature&city-B, s(np(3+plu, wh(feature&city-B), []), verb(be, active, pres+fin, [], pos), [arg(dir, np(3+plu, np_head(det(the(plu)), [], capital), [pp(prep(of), np(3+plu, np_head(det(the(plu)), [], country), [reduced_rel(feature&place&country-D, s(np(3+plu, wh(feature&place&country-D), []), verb(border, active, inf, [prog], pos), [arg(dir, np(3+sin, name(baltic), []))], []))]))]))], []))), sem((answer([D]):-setof([A]:C, (country(A), borders(A, baltic), setof(B, capital(A, B), C)), D))), qplan((answer([H]):-setof([E]:G, (country(E), borders(E, baltic), setof(F, capital(E, F), G)), H))), answers([[[denmark]:[copenhagen], [east_germany]:[east_berlin], [finland]:[helsinki], [poland]:[warsaw], [soviet_union]:[moscow], [sweden]:[stockholm], [west_germany]:[bonn]]])],[time(0.0010000000000000009)]).
must_test_80([which, countries, are, bordered, by, two, seas, ?], [sent([which, countries, are, bordered, by, two, seas, ?]), parse(whq(feature&place&country-B, s(np(3+plu, np_head(int_det(feature&place&country-B), [], country), []), verb(border, passive, pres+fin, [], pos), [], [pp(prep(by), np(3+plu, np_head(quant(same, nb(2)), [], sea), []))]))), sem((answer([A]):-country(A), numberof(B, (sea(B), borders(B, A)), 2))), qplan((answer([B]):-numberof(A, (sea(A), borders(A, B)), 2), {country(B)})), answers([egypt, iran, israel, saudi_arabia, turkey])],[time(0.0)]).
must_test_80([how, many, countries, does, the, danube, flow, through, ?], [sent([how, many, countries, does, the, danube, flow, through, ?]), parse(whq(feature&place&country-B, s(np(3+sin, name(danube), []), verb(flow, active, pres+fin, [], pos), [], [pp(prep(through), np(3+plu, np_head(quant(same, wh(feature&place&country-B)), [], country), []))]))), sem((answer([A]):-numberof(B, (country(B), flows(danube, B)), A))), qplan((answer([B]):-numberof(A, (flows(danube, A), {country(A)}), B))), answers([6])],[time(0.0010000000000000009)]).
must_test_80([what, is, the, total, area, of, countries, south, of, the, equator, and, not, in, australasia, ?], [sent([what, is, the, total, area, of, countries, south, of, the, equator, and, not, in, australasia, ?]), parse(whq(A-B, s(np(3+sin, wh(A-B), []), verb(be, active, pres+fin, [], pos), [arg(dir, np(3+sin, np_head(det(the(sin)), [adj(total)], area), [pp(prep(of), np(3+plu, np_head(generic, [], country), [conj(and, reduced_rel(feature&place&country-F, s(np(3+plu, wh(feature&place&country-F), []), verb(be, active, pres+fin, [], pos), [arg(pred, pp(prep(southof), np(3+sin, name(equator), [])))], [])), reduced_rel(feature&place&country-F, s(np(3+plu, wh(feature&place&country-F), []), verb(be, active, pres+fin, [], neg), [arg(pred, pp(prep(in), np(3+sin, name(australasia), [])))], [])))]))]))], []))), sem((answer([A]):-B^ (setof(C:[D], (area(D, C), country(D), southof(D, equator), \+in(D, australasia)), B), aggregate(total, B, A)))), qplan((answer([E]):-D^ (setof(C:[B], (southof(B, equator), area(B, C), {country(B)}, {\+in(B, australasia)}), D), aggregate(total, D, E)))), answers([10239--ksqmiles])],[time(0.0010000000000000009)]).
must_test_80([what, is, the, average, area, of, the, countries, in, each, continent, ?], [sent([what, is, the, average, area, of, the, countries, in, each, continent, ?]), parse(whq(A-C, s(np(3+sin, wh(A-C), []), verb(be, active, pres+fin, [], pos), [arg(dir, np(3+sin, np_head(det(the(sin)), [adj(average)], area), [pp(prep(of), np(3+plu, np_head(det(the(plu)), [], country), [pp(prep(in), np(3+sin, np_head(det(each), [], continent), []))]))]))], []))), sem((answer([B, E]):-continent(B), [ (0--ksqmiles):[andorra], (0--ksqmiles):[liechtenstein], (0--ksqmiles):[malta], (0--ksqmiles):[monaco], (0--ksqmiles):[san_marino], (1--ksqmiles):[luxembourg], (4--ksqmiles):[cyprus], (11--ksqmiles):[albania], (12--ksqmiles):[belgium], (14--ksqmiles):[netherlands], (16--ksqmiles):[switzerland], (17--ksqmiles):[denmark], (27--ksqmiles):[eire], (32--ksqmiles):[austria], (35--ksqmiles):[portugal], (36--ksqmiles):[hungary], (40--ksqmiles):[iceland], (41--ksqmiles):[east_germany], (43--ksqmiles):[bulgaria], (49--ksqmiles):[czechoslovakia], (51--ksqmiles):[greece], (92--ksqmiles):[romania], (94--ksqmiles):[united_kingdom], (96--ksqmiles):[west_germany], (99--ksqmiles):[yugoslavia], (116--ksqmiles):[italy], (120--ksqmiles):[poland], (125--ksqmiles):[norway], (130--ksqmiles):[finland], (174--ksqmiles):[sweden], (195--ksqmiles):[spain], (213--ksqmiles):[france]]^ (setof(D:[C], (area(C, D), country(C), in(C, B)), [ (0--ksqmiles):[andorra], (0--ksqmiles):[liechtenstein], (0--ksqmiles):[malta], (0--ksqmiles):[monaco], (0--ksqmiles):[san_marino], (1--ksqmiles):[luxembourg], (4--ksqmiles):[cyprus], (11--ksqmiles):[albania], (12--ksqmiles):[belgium], (14--ksqmiles):[netherlands], (16--ksqmiles):[switzerland], (17--ksqmiles):[denmark], (27--ksqmiles):[eire], (32--ksqmiles):[austria], (35--ksqmiles):[portugal], (36--ksqmiles):[hungary], (40--ksqmiles):[iceland], (41--ksqmiles):[east_germany], (43--ksqmiles):[bulgaria], (49--ksqmiles):[czechoslovakia], (51--ksqmiles):[greece], (92--ksqmiles):[romania], (94--ksqmiles):[united_kingdom], (96--ksqmiles):[west_germany], (99--ksqmiles):[yugoslavia], (116--ksqmiles):[italy], (120--ksqmiles):[poland], (125--ksqmiles):[norway], (130--ksqmiles):[finland], (174--ksqmiles):[sweden], (195--ksqmiles):[spain], (213--ksqmiles):[france]]), aggregate(average, [ (0--ksqmiles):[andorra], (0--ksqmiles):[liechtenstein], (0--ksqmiles):[malta], (0--ksqmiles):[monaco], (0--ksqmiles):[san_marino], (1--ksqmiles):[luxembourg], (4--ksqmiles):[cyprus], (11--ksqmiles):[albania], (12--ksqmiles):[belgium], (14--ksqmiles):[netherlands], (16--ksqmiles):[switzerland], (17--ksqmiles):[denmark], (27--ksqmiles):[eire], (32--ksqmiles):[austria], (35--ksqmiles):[portugal], (36--ksqmiles):[hungary], (40--ksqmiles):[iceland], (41--ksqmiles):[east_germany], (43--ksqmiles):[bulgaria], (49--ksqmiles):[czechoslovakia], (51--ksqmiles):[greece], (92--ksqmiles):[romania], (94--ksqmiles):[united_kingdom], (96--ksqmiles):[west_germany], (99--ksqmiles):[yugoslavia], (116--ksqmiles):[italy], (120--ksqmiles):[poland], (125--ksqmiles):[norway], (130--ksqmiles):[finland], (174--ksqmiles):[sweden], (195--ksqmiles):[spain], (213--ksqmiles):[france]], E)))), qplan((answer([F, J]):-continent(F), I^ (setof(H:[G], (area(G, H), country(G), in(G, F)), I), aggregate(average, I, J)))), answers([[europe, 58.84375--ksqmiles]])],[time(0.0040000000000000036)]).
must_test_80([is, there, more, than, one, country, in, each, continent, ?], [sent([is, there, more, than, one, country, in, each, continent, ?]), parse(q(s(there, verb(be, active, pres+fin, [], pos), [arg(dir, np(3+sin, np_head(quant(more, nb(1)), [], country), [pp(prep(in), np(3+sin, np_head(det(each), [], continent), []))]))], []))), sem((answer([]):- \+A^ (continent(A), \+C^ (numberof(B, (country(B), in(B, A)), C), C>1)))), qplan((answer([]):- \+D^ (continent(D), \+F^ (numberof(E, (country(E), in(E, D)), F), F>1)))), answers([false])],[time(0.0010000000000000009)]).
must_test_80([is, there, some, ocean, that, does, not, border, any, country, ?], [sent([is, there, some, ocean, that, does, not, border, any, country, ?]), parse(q(s(there, verb(be, active, pres+fin, [], pos), [arg(dir, np(3+sin, np_head(det(some), [], ocean), [rel(feature&place&seamass-B, s(np(3+sin, wh(feature&place&seamass-B), []), verb(border, active, pres+fin, [], neg), [arg(dir, np(3+sin, np_head(det(any), [], country), []))], []))]))], []))), sem((answer([]):-A^ (ocean(A), \+B^ (country(B), borders(A, B))))), qplan((answer([]):-A^{ocean(A), {\+B^ (borders(A, B), {country(B)})}})), answers([true])],[time(0.0010000000000000009)]).
must_test_80([what, are, the, countries, from, which, a, river, flows, into, the, black_sea, ?], [sent([what, are, the, countries, from, which, a, river, flows, into, the, black_sea, ?]), parse(whq(feature&place&country-B, s(np(3+plu, wh(feature&place&country-B), []), verb(be, active, pres+fin, [], pos), [arg(dir, np(3+plu, np_head(det(the(plu)), [], country), [rel(feature&place&country-D, s(np(3+sin, np_head(det(a), [], river), []), verb(flow, active, pres+fin, [], pos), [], [pp(prep(from), np(3+plu, wh(feature&place&country-D), [])), pp(prep(into), np(3+sin, name(black_sea), []))]))]))], []))), sem((answer([A]):-setof(B, (country(B), C^ (river(C), flows(C, B, black_sea))), A))), qplan((answer([C]):-setof(B, A^ (flows(A, B, black_sea), {country(B)}, {river(A)}), C))), answers([[romania]])],[time(0.0010000000000000009)]).
must_test_80([what, are, the, continents, no, country, in, which, contains, more, than, two, cities, whose, population, exceeds, nb(1), million, ?], [sent([what, are, the, continents, no, country, in, which, contains, more, than, two, cities, whose, population, exceeds, nb(1), million, ?]), parse(whq(feature&place&continent-B, s(np(3+plu, wh(feature&place&continent-B), []), verb(be, active, pres+fin, [], pos), [arg(dir, np(3+plu, np_head(det(the(plu)), [], continent), [rel(feature&place&continent-D, s(np(3+sin, np_head(det(no), [], country), [pp(prep(in), np(3+plu, wh(feature&place&continent-D), []))]), verb(contain, active, pres+fin, [], pos), [arg(dir, np(3+plu, np_head(quant(more, nb(2)), [], city), [rel(feature&city-G, s(np(3+sin, np_head(det(the(sin)), [], population), [pp(poss, np(3+plu, wh(feature&city-G), []))]), verb(exceed, active, pres+fin, [], pos), [arg(dir, np(3+sin, np_head(quant(same, nb(1)), [], million), []))], []))]))], []))]))], []))), sem((answer([F]):-setof(A, (continent(A), \+B^ (country(B), in(B, A), E^ (numberof(C, (city(C), D^ (population(C, D), exceeds(D, 1--million)), in(C, B)), E), E>2))), F))), qplan((answer([L]):-setof(G, (continent(G), \+H^ (country(H), in(H, G), K^ (numberof(I, (city(I), J^ (population(I, J), exceeds(J, 1--million)), in(I, H)), K), K>2))), L))), answers([[africa, america, antarctica, asia, australasia, europe]])],[time(0.05499999999999999)]).
must_test_80([which, country, bordering, the, mediterranean, borders, a, country, that, is, bordered, by, a, country, whose, population, exceeds, the, population, of, india, ?], [sent([which, country, bordering, the, mediterranean, borders, a, country, that, is, bordered, by, a, country, whose, population, exceeds, the, population, of, india, ?]), parse(whq(feature&place&country-B, s(np(3+sin, np_head(int_det(feature&place&country-B), [], country), [reduced_rel(feature&place&country-B, s(np(3+sin, wh(feature&place&country-B), []), verb(border, active, inf, [prog], pos), [arg(dir, np(3+sin, name(mediterranean), []))], []))]), verb(border, active, pres+fin, [], pos), [arg(dir, np(3+sin, np_head(det(a), [], country), [rel(feature&place&country-C, s(np(3+sin, wh(feature&place&country-C), []), verb(border, passive, pres+fin, [], pos), [], [pp(prep(by), np(3+sin, np_head(det(a), [], country), [rel(feature&place&country-D, s(np(3+sin, np_head(det(the(sin)), [], population), [pp(poss, np(3+sin, wh(feature&place&country-D), []))]), verb(exceed, active, pres+fin, [], pos), [arg(dir, np(3+sin, np_head(det(the(sin)), [], population), [pp(prep(of), np(3+sin, name(india), []))]))], []))]))]))]))], []))), sem((answer([A]):-country(A), borders(A, mediterranean), B^ (country(B), C^ (country(C), D^ (population(C, D), E^ (population(india, E), exceeds(D, E))), borders(C, B)), borders(A, B)))), qplan((answer([B]):-C^D^E^A^ (population(india, A), borders(B, mediterranean), {country(B)}, {borders(B, C), {country(C)}, {borders(D, C), {country(D)}, {population(D, E), {exceeds(E, A)}}}}))), answers([turkey])],[time(0.0020000000000000018)]).
must_test_80([which, countries, have, a, population, exceeding, nb(10), million, ?], [sent([which, countries, have, a, population, exceeding, nb(10), million, ?]), parse(whq(feature&place&country-B, s(np(3+plu, np_head(int_det(feature&place&country-B), [], country), []), verb(have, active, pres+fin, [], pos), [arg(dir, np(3+sin, np_head(det(a), [], population), [reduced_rel(measure&heads-C, s(np(3+sin, wh(measure&heads-C), []), verb(exceed, active, inf, [prog], pos), [arg(dir, np(3+plu, np_head(quant(same, nb(10)), [], million), []))], []))]))], []))), sem((answer([A]):-country(A), B^ (exceeds(B, 10--million), population(A, B)))), qplan((answer([A]):-B^ (country(A), {population(A, B), {exceeds(B, 10--million)}}))), answers([malaysia, uganda])],[time(0.0010000000000000009)]).
must_test_80([which, countries, with, a, population, exceeding, nb(10), million, border, the, atlantic, ?], [sent([which, countries, with, a, population, exceeding, nb(10), million, border, the, atlantic, ?]), parse(whq(feature&place&country-B, s(np(3+plu, np_head(int_det(feature&place&country-B), [], country), [pp(prep(with), np(3+sin, np_head(det(a), [], population), [reduced_rel(measure&heads-C, s(np(3+sin, wh(measure&heads-C), []), verb(exceed, active, inf, [prog], pos), [arg(dir, np(3+plu, np_head(quant(same, nb(10)), [], million), []))], []))]))]), verb(border, active, pres+fin, [], pos), [arg(dir, np(3+sin, name(atlantic), []))], []))), sem((answer([A]):-B^ (population(A, B), exceeds(B, 10--million), country(A)), borders(A, atlantic))), qplan((answer([A]):-B^ (borders(A, atlantic), {population(A, B), {exceeds(B, 10--million)}}, {country(A)}))), answers([venezuela])],[time(0.0010000000000000009)]).
must_test_80([what, percentage, of, countries, border, each, ocean, ?], [sent([what, percentage, of, countries, border, each, ocean, ?]), parse(whq(A-C, s(np(3+plu, np_head(int_det(A-C), [], percentage), [pp(prep(of), np(3+plu, np_head(generic, [], country), []))]), verb(border, active, pres+fin, [], pos), [arg(dir, np(3+sin, np_head(det(each), [], ocean), []))], []))), sem((answer([B, E]):-ocean(B), [afghanistan, albania, algeria, andorra, angola, argentina, australia, austria, bahamas, bahrain, bangladesh, barbados, belgium, belize, bhutan, bolivia, botswana, brazil, bulgaria, burma, burundi, cambodia, cameroon, canada, central_african_republic, chad, chile, china, colombia, congo, costa_rica, cuba, cyprus, czechoslovakia, dahomey, denmark, djibouti, dominican_republic, east_germany, ecuador, egypt, eire, el_salvador, equatorial_guinea, ethiopia, fiji, finland, france, french_guiana, gabon, gambia, ghana, greece, grenada, guatemala, guinea, guinea_bissau, guyana, haiti, honduras, hungary, iceland, india, indonesia, iran, iraq, israel, italy, ivory_coast, jamaica, japan, jordan, kenya, kuwait, laos, lebanon, lesotho, liberia, libya, liechtenstein, luxembourg, malagasy, malawi, malaysia, maldives, mali, malta, mauritania, mauritius, mexico, monaco, mongolia, morocco, mozambique, nepal, netherlands, new_zealand, nicaragua, niger, nigeria, north_korea, norway, oman, pakistan, panama, papua_new_guinea, paraguay, peru, philippines, poland, portugal, qatar, romania, rwanda, san_marino, saudi_arabia, senegal, seychelles, sierra_leone, singapore, somalia, south_africa, south_korea, south_yemen, soviet_union, spain, sri_lanka, sudan, surinam, swaziland, sweden, switzerland, syria, taiwan, tanzania, thailand, togo, tonga, trinidad_and_tobago, tunisia, turkey, uganda, united_arab_emirates, united_kingdom, united_states, upper_volta, uruguay, venezuela, vietnam, west_germany, western_samoa, yemen, yugoslavia, zaire, zambia, zimbabwe]^ (setof(C, country(C), [afghanistan, albania, algeria, andorra, angola, argentina, australia, austria, bahamas, bahrain, bangladesh, barbados, belgium, belize, bhutan, bolivia, botswana, brazil, bulgaria, burma, burundi, cambodia, cameroon, canada, central_african_republic, chad, chile, china, colombia, congo, costa_rica, cuba, cyprus, czechoslovakia, dahomey, denmark, djibouti, dominican_republic, east_germany, ecuador, egypt, eire, el_salvador, equatorial_guinea, ethiopia, fiji, finland, france, french_guiana, gabon, gambia, ghana, greece, grenada, guatemala, guinea, guinea_bissau, guyana, haiti, honduras, hungary, iceland, india, indonesia, iran, iraq, israel, italy, ivory_coast, jamaica, japan, jordan, kenya, kuwait, laos, lebanon, lesotho, liberia, libya, liechtenstein, luxembourg, malagasy, malawi, malaysia, maldives, mali, malta, mauritania, mauritius, mexico, monaco, mongolia, morocco, mozambique, nepal, netherlands, new_zealand, nicaragua, niger, nigeria, north_korea, norway, oman, pakistan, panama, papua_new_guinea, paraguay, peru, philippines, poland, portugal, qatar, romania, rwanda, san_marino, saudi_arabia, senegal, seychelles, sierra_leone, singapore, somalia, south_africa, south_korea, south_yemen, soviet_union, spain, sri_lanka, sudan, surinam, swaziland, sweden, switzerland, syria, taiwan, tanzania, thailand, togo, tonga, trinidad_and_tobago, tunisia, turkey, uganda, united_arab_emirates, united_kingdom, united_states, upper_volta, uruguay, venezuela, vietnam, west_germany, western_samoa, yemen, yugoslavia, zaire, zambia, zimbabwe]), 4^ (numberof(D, (one_of([afghanistan, albania, algeria, andorra, angola, argentina, australia, austria, bahamas, bahrain, bangladesh, barbados, belgium, belize, bhutan, bolivia, botswana, brazil, bulgaria, burma, burundi, cambodia, cameroon, canada, central_african_republic, chad, chile, china, colombia, congo, costa_rica, cuba, cyprus, czechoslovakia, dahomey, denmark, djibouti, dominican_republic, east_germany, ecuador, egypt, eire, el_salvador, equatorial_guinea, ethiopia, fiji, finland, france, french_guiana, gabon, gambia, ghana, greece, grenada, guatemala, guinea, guinea_bissau, guyana, haiti, honduras, hungary, iceland, india, indonesia, iran, iraq, israel, italy, ivory_coast, jamaica, japan, jordan, kenya, kuwait, laos, lebanon, lesotho, liberia, libya, liechtenstein, luxembourg, malagasy, malawi, malaysia, maldives, mali, malta, mauritania, mauritius, mexico, monaco, mongolia, morocco, mozambique, nepal, netherlands, new_zealand, nicaragua, niger, nigeria, north_korea, norway, oman, pakistan, panama, papua_new_guinea, paraguay, peru, philippines, poland, portugal, qatar, romania, rwanda, san_marino, saudi_arabia, senegal, seychelles, sierra_leone, singapore, somalia, south_africa, south_korea, south_yemen, soviet_union, spain, sri_lanka, sudan, surinam, swaziland, sweden, switzerland, syria, taiwan, tanzania, thailand, togo, tonga, trinidad_and_tobago, tunisia, turkey, uganda, united_arab_emirates, united_kingdom, united_states, upper_volta, uruguay, venezuela, vietnam, west_germany, western_samoa, yemen, yugoslavia, zaire, zambia, zimbabwe], D), borders(D, B)), 4), 156^ (card([afghanistan, albania, algeria, andorra, angola, argentina, australia, austria, bahamas, bahrain, bangladesh, barbados, belgium, belize, bhutan, bolivia, botswana, brazil, bulgaria, burma, burundi, cambodia, cameroon, canada, central_african_republic, chad, chile, china, colombia, congo, costa_rica, cuba, cyprus, czechoslovakia, dahomey, denmark, djibouti, dominican_republic, east_germany, ecuador, egypt, eire, el_salvador, equatorial_guinea, ethiopia, fiji, finland, france, french_guiana, gabon, gambia, ghana, greece, grenada, guatemala, guinea, guinea_bissau, guyana, haiti, honduras, hungary, iceland, india, indonesia, iran, iraq, israel, italy, ivory_coast, jamaica, japan, jordan, kenya, kuwait, laos, lebanon, lesotho, liberia, libya, liechtenstein, luxembourg, malagasy, malawi, malaysia, maldives, mali, malta, mauritania, mauritius, mexico, monaco, mongolia, morocco, mozambique, nepal, netherlands, new_zealand, nicaragua, niger, nigeria, north_korea, norway, oman, pakistan, panama, papua_new_guinea, paraguay, peru, philippines, poland, portugal, qatar, romania, rwanda, san_marino, saudi_arabia, senegal, seychelles, sierra_leone, singapore, somalia, south_africa, south_korea, south_yemen, soviet_union, spain, sri_lanka, sudan, surinam, swaziland, sweden, switzerland, syria, taiwan, tanzania, thailand, togo, tonga, trinidad_and_tobago, tunisia, turkey, uganda, united_arab_emirates, united_kingdom, united_states, upper_volta, uruguay, venezuela, vietnam, west_germany, western_samoa, yemen, yugoslavia, zaire, zambia, zimbabwe], 156), ratio(4, 156, E)))))), qplan((answer([F, L]):-ocean(F), H^ (setof(G, country(G), H), J^ (numberof(I, (one_of(H, I), borders(I, F)), J), K^ (card(H, K), ratio(J, K, L)))))), answers([[arctic_ocean, 2.5641025641025643]])],[time(0.0020000000000000018)]).
must_test_80([what, countries, are, there, in, europe, ?], [sent([what, countries, are, there, in, europe, ?]), parse(whq(feature&place&country-B, s(np(3+plu, np_head(int_det(feature&place&country-B), [], country), []), verb(be, active, pres+fin, [], pos), [void], [pp(prep(in), np(3+sin, name(europe), []))]))), sem((answer([A]):-country(A), in(A, europe))), qplan((answer([A]):-in(A, europe), {country(A)})), answers([albania, andorra, austria, belgium, bulgaria, cyprus, czechoslovakia, denmark, east_germany, eire, finland, france, greece, hungary, iceland, italy, liechtenstein, luxembourg, malta, monaco, netherlands, norway, poland, portugal, romania, san_marino, spain, sweden, switzerland, united_kingdom, west_germany, yugoslavia])],[time(0.0010000000000000009)]).


% Chat-80 : A small subset of English for database querying.

/* Control loop */

hi :-
   hi(user).

hi(File) :-
   repeat,
      ask(File,P),
      control(test_quiet,P), !,
      end(File).

ask(user,P) :- !,
   prompt(_,'Question: '),
   read_in(P).
ask(F,P) :-
   absolute_file_name(F,File),
   seeing(Old),
   see(File),
   read_in(P),
  % nl,
   doing(P,0),
  % nl,
   see(Old).

doing([],_) :- !,nl.
doing([X|L],N0) :-
   out(X),  
   advance(X,N0,N),
   doing(L,N).

out(nb(X)) :- !,
   write(X).
out(A) :-
   write(A).

advance(X,N0,N) :-
   uses(X,K),
   M is N0+K,
 ( M>72, !,
      nl,
      N is 0;
   N is M+1,
      put(" ")).

uses(nb(X),N) :- !,
   chars(X,N).
uses(X,N) :-
   chars(X,N).

chars(X,N) :- atomic(X), !,
   name(X,L),
   length(L,N).
chars(_,2).

end(user) :- !.
end(F) :- 
   catch(close(F),_,seen).

control(_FB,[bye,'.']) :- !,
   display('Cheerio.'),
   nl.
control(_FB,[trace,'.']) :- !,
   assert(tracing),
   display('Tracing from now on!'), nl, fail.
control(_FB,[do,not,trace,'.']) :-
   retract(tracing), !,
   display('No longer tracing.'), nl, fail.
control(FB,U) :-
   display(FB:U),
   process_run(FB,U),
   fail.

process_run(Report,U) :- process_run_0(Report,U),!.
process_run(_Report,U) :- trace,process_run_0(report,U),!.

process_run_0(Report,U) :-
   runtime(StartParse),   
   sentence(E,U,[],[],[]),
   runtime(StopParse),
   ParseTime is StopParse - StartParse,
   call(Report,E,'Parse',ParseTime,tree),
   runtime(StartSem),
   logic(E,S), !,
   runtime(StopSem),
   SemTime is StopSem - StartSem,
   call(Report,S,'Semantics',SemTime,expr),
   runtime(StartPlan),
   qplan(S,S1), !,
   copy_term(S1,QP),
   runtime(StopPlan),
   TimePlan is StopPlan - StartPlan,
   call(Report,S1,'Planning',TimePlan,expr),
   runtime(StartAns),
   answer(S1,Results),!,
   runtime(StopAns),
   TimeAns is StopAns - StartAns,
   call(Report,Results,'Reply',TimeAns,tree),
   WholeTime is ParseTime + SemTime + TimePlan + TimeAns,
   check_test_80(U,[sent=(U),parse=(E),sem=(S),qplan=(QP),answers=(Results)],[time(WholeTime)]),!.

check_test_80(U,List,Time):-must_test_80(U,BList,BTime),!,reportDif(U,List,BList,Time,BTime).
check_test_80(U,List,Time):-reportDif(U,List,[],Time,[]).

test_quiet(_,_,_,_).


reportDif(_U,List,BList,_Time,_BTime):-forall(member(N=V,List),ignore((member(N=BV,BList),not(BV = V), 'format'('~n1) ~q = ~q ~~n2) ~q = ~q ~n',[N,V,N,BV])))).

process(U) :- process_run(test_quiet,U),!.
process(U) :- failure(U).

failure(U) :-
   display('I don''t understand! '+U), nl.


report(Item,Label,Time,Mode) :-
   tracing, !,
   nl, write(Label), write(': '), write(Time), write('sec.'), nl,
   report_item(Mode,Item).
report(_,_,_,_).

report_item(none,_).
report_item(expr,Item) :-
   write_tree(Item), nl.
report_item(tree,Item) :-
   print_tree(Item), nl.

runtime(Time) :-
   statistics(runtime,[MSec,_]),
   Time is MSec/1000.

quote(A&R) :-
   atom(A), !,
   quote_amp(R).
quote(_-_).
quote(_--_).
quote(_+_).
quote(verb(_,_,_,_,_)).
quote(wh(_)).
quote(name(_)).
quote(prep(_)).
quote(det(_)).
quote(quant(_,_)).
quote(int_det(_)).

quote_amp('$VAR'(_)) :- !.
quote_amp(R) :-
   quote(R).

logic(S0,S) :-
   i_sentence(S0,S1),
   clausify(S1,S2),
   simplify(S2,S).

simplify(C,(P:-R)) :- !,
   unequalise(C,(P:-Q)),
   simplify(Q,R,true).

simplify(setof(X,P0,S),R,R0) :- !,
   simplify(P0,P,true),
   revand(R0,setof(X,P,S),R).
simplify((P,Q),R,R0) :-
   simplify(Q,R1,R0),
   simplify(P,R,R1).
simplify(true,R,R) :- !.
simplify(X^P0,R,R0) :- !,
   simplify(P0,P,true),
   revand(R0,X^P,R).
simplify(numberof(X,P0,Y),R,R0) :- !,
   simplify(P0,P,true),
   revand(R0,numberof(X,P,Y),R).
simplify(\+P0,R,R0) :- !,
   simplify(P0,P1,true),
   simplify_not(P1,P),
   revand(R0,P,R).
simplify(P,R,R0) :-
   revand(R0,P,R).

simplify_not(\+P,P) :- !.
simplify_not(P,\+P).

revand(true,P,P) :- !.
revand(P,true,P) :- !.
revand(P,Q,(Q,P)).

unequalise(C0,C) :-
   numbervars(C0,1,N),
   functor(V,v,N),
   functor(M,v,N),
   inv_map(C0,V,M,C).

inv_map('$VAR'(I),V,_,X) :- !,
   arg(I,V,X).
inv_map(A=B,V,M,T) :- !,
   drop_eq(A,B,V,M,T).
inv_map(X^P0,V,M,P) :- !,
   inv_map(P0,V,M,P1),
   exquant(X,V,M,P1,P).
inv_map(A,_,_,A) :- atomic(A), !.
inv_map(T,V,M,R) :-
   functor(T,F,K),
   functor(R,F,K),
   inv_map_list(K,T,V,M,R).

inv_map_list(0,_,_,_,_) :- !.
inv_map_list(K0,T,V,M,R) :-
   arg(K0,T,A),
   arg(K0,R,B),
   inv_map(A,V,M,B),
   K is K0-1,
   inv_map_list(K,T,V,M,R).

drop_eq('$VAR'(I),'$VAR'(J),V,M,true) :- !,
 ( I=\=J, !,
      irev(I,J,K,L), 
      arg(K,M,L),
      arg(K,V,X),
      arg(L,V,X);
   true).
drop_eq('$VAR'(I),T,V,M,true) :- !,
   arg(I,V,T),
   arg(I,M,0).
drop_eq(T,'$VAR'(I),V,M,true) :- !,
   arg(I,V,T),
   arg(I,M,0).
drop_eq(X,Y,_,_,X=Y).

exquant('$VAR'(I),V,M,P0,P) :-
   arg(I,M,U),
 ( var(U), !,
      arg(I,V,X),
       P=(X^P0);
   P=P0).

irev(I,J,I,J) :- I>J, !.
irev(I,J,J,I).

