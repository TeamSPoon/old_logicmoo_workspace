
transfer_lexicon([utterance_type, command], [utterance_type, command]).
transfer_lexicon([utterance_type, query], [utterance_type, query]).
transfer_lexicon([state, be], [state, �tre]).
transfer_lexicon([action, switch_on], [action, allumer]).
transfer_lexicon([action, switch_off], [action, �teindre]).
transfer_lexicon([action, dim], [action, baisser]).
transfer_lexicon([device, light], [device, lampe]).
transfer_lexicon([device, fan], [device, ventilateur]).
transfer_lexicon([location, kitchen], [location, cuisine]).
transfer_lexicon([location, living_room], [location, salon]).
transfer_lexicon([onoff, on], [onoff, allum�]).
transfer_lexicon([onoff, off], [onoff, �teint]).

role_transfer_rule(null, null).
role_transfer_rule(agent, agent).
role_transfer_rule(theme, object).
role_transfer_rule(adj, adj).
role_transfer_rule(in_loc, loc).


