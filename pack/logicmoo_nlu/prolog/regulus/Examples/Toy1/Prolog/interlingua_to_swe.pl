
transfer_lexicon([utterance_type, command], [utterance_type, command]).
transfer_lexicon([utterance_type, query], [utterance_type, query]).
transfer_lexicon([state, be], [state, vara]).

transfer_lexicon([action, dim], [action, vrid_ner]).

macro(light_dependent_rule(From, LightVersion, NonLightVersion),
      (  transfer_rule(From, LightVersion) :- context([device, light]) )
     ).
macro(light_dependent_rule(From, LightVersion, NonLightVersion),
      (  transfer_rule(From, NonLightVersion) :- \+ context([device, light]) )
     ).

@light_dependent_rule([[action, switch_on]], [[action, t�nda]], [[action, s�tta_p�]]).
@light_dependent_rule([[action, switch_off]], [[action, sl�ck]], [[action, st�nga_av]]).
@light_dependent_rule([[onoff, on]], [[onoff, t�nd]], [[onoff, p�]]).
@light_dependent_rule([[onoff, off]], [[onoff, sl�ckt]], [[onoff, av]]).

transfer_lexicon([device, light], [device, lampa]).
transfer_lexicon([device, fan], [device, fl�kt]).
transfer_lexicon([location, kitchen], [location, k�k]).
transfer_lexicon([location, living_room], [location, vardagsrum]).


