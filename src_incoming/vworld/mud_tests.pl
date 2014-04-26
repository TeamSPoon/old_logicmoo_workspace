% Dec 13, 2035
% Douglas Miles
%
/** <module>
% Common place to reduce redundancy World utility prediates
% **************IMPORTANT**************
% To customize your world, you need to modify wstart.pl
%
% *.objects.pl / *.map.pl is defined in wstart.pl for ease of setting up and testing.
% ****************************************
*/
:- module(mud_tests,
	[run_mud_tests/0]).

% do some sanity testing (expects the startrek world is loaded)
run_mud_tests:-
   do_player_action(look).
