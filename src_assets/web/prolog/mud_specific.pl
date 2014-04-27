:- module(mud_specific, [
	  style/1]).
/** <module> Code specific to this particular MOO's web presence

*/

:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).


%%	style_name(-Style:atom) is nondet
%
%	true if Style is the style name (whats passed to user:body)
%	for this mud
%
style(name(startrek)).

:- multifile user:head/2, user:body/2.

%%	user:head(+Style:atom, +Head:html, ?A:List, ?B:List) is det
%
%	override head generation for this moo
%
%	this predicate is REQUIRED if style is not logicmoo
%
user:head(startrek, Head) -->
	html(head([
		 title('Star Trek'),
		 Head
	     ])).

%%	user:body(+Style:atom, +Head:html, ?A:List, ?B:List) is det
%
%	override body generation for this moo
%
%	this predicate is REQUIRED if style is not logicmoo
%
user:body(startrek, Body) -->
	html(body([
		 h1('Star Trek'),
		 div(id(content), Body)
	     ])).
