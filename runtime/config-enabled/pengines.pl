:- module(conf_pengines, []).
:- if(exists_source(library(pengines))).
:- use_module(api(pengines)).

/** <module> Configure Pengines access

Provide access to ClioPatria's RDF store using pengines.

@see http://www.swi-prolog.org/pldoc/package/pengines.html
*/

% :- set_setting_default(pengines:time_limit, 60).
:- set_setting_default(pengines:allow_from, [(*)]).
% :- set_setting_default(pengines:deny_from,  []).

% DO NOT REMOVE THIS :- endif.
:- endif.
