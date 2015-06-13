
transfer_rule([[action, switch], [prep, on]],
	      [[action, allumer]]).

transfer_rule([[action, switch], [prep, off]],
	      [[action, �teindre]]).

transfer_rule([[tense, imperative]], []).
transfer_rule([[tense, present]], []).
transfer_rule([[spec, the_sing]], []).
transfer_rule([[pronoun, you]], []).
transfer_rule([[prep, in_loc]], []).
transfer_rule([[voice, active]], []).

transfer_lexicon([utterance_type, imp], [utterance_type, command]).
transfer_lexicon([utterance_type, ynq], [utterance_type, query]).
transfer_lexicon([verb, be], [state, �tre]).
transfer_lexicon([action, dim], [action, baisser]).
transfer_lexicon([device, light], [device, lampe]).
transfer_lexicon([device, fan], [device, ventilateur]).
transfer_lexicon([location, kitchen], [location, cuisine]).
transfer_lexicon([location, living_room], [location, salon]).
transfer_lexicon([prep, on], [onoff, allum�]).
transfer_lexicon([prep, off], [onoff, �teint]).
transfer_lexicon([adj, on], [onoff, allum�]).
transfer_lexicon([adj, off], [onoff, �teint]).

role_transfer_rule(null, null).
role_transfer_rule(agent, agent).
role_transfer_rule(object, object).
role_transfer_rule(adj, adj).
role_transfer_rule(in_loc, loc).

