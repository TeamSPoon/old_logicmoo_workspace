
% prolog_xml.pl

:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(prolog_xml,
	  [prolog_xml/2,
	   safe_prolog_xml/2]
	 ).

%------------------------------------------------------------------------------------

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
:- use_module(library(xml)).

%------------------------------------------------------------------------------------

/*

prolog_xml(?Prolog, ?XMLString)

Convert Prolog term into XML format, or vice versa. Useful for passing data between Prolog
and non-Prolog processes.

Version to produce "compact" XML. Key principles:

- Map XML tags onto Prolog functors

- Treat some special Prolog functors specially (=, list)

Example:

| ?- Term = foo(a,b,[c, d, 6, p=q, some_vars(F, F, G)]), prolog_xml(Term, XML), format('~N~s~n~n', [XML]), prolog_xml(Term1, XML), format('~N~q~n~n', [Term1]), fail.


*/

safe_prolog_xml(Prolog, XMLString) :-
	on_exception(
	Exception, 
	prolog_xml(Prolog, XMLString),
	handle_exception_in_prolog_xml(Exception)
    ),
	!.

handle_exception_in_prolog_xml(Exception) :-
	format('~N~n*** Exception in call to prolog_xml/2 ***~n~n', []),
	print_message(error, Exception),
	fail.

%------------------------------------------------------------------------------------

prolog_xml(Prolog, XMLString) :-
	nonvar(Prolog),
	prolog_to_xml_term(Prolog, XMLTerm),
	WrappedXMLTerm = xml([], XMLTerm),
	(   xml_parse(XMLString, WrappedXMLTerm) ->
	    true
	;
	    otherwise ->
	    format('~N*** Error: unable to convert to XML: ~n~q~n', [WrappedXMLTerm]),
	    fail
	),  
	!.
prolog_xml(Prolog, XMLString) :-
	is_prolog_string(XMLString),
	(   xml_parse(XMLString, XMLParseResult) ->
	    true
	;
	    otherwise ->
	    format('~N*** Error: unable to parse XML: ~n~s~n', [XMLString]),
	    fail
	),
	XMLParseResult = xml(_Attributes, XMLBody),
	(   XMLBody = [SingleXMLTerm] ->
	    xml_term_to_prolog(SingleXMLTerm, Prolog)
	;
	    is_list(XMLBody) ->
	    xml_term_list_to_prolog_list(XMLTerm, Prolog)
	;
	    otherwise ->
	    xml_term_to_prolog(XMLTerm, Prolog)
	),
	!.

%------------------------------------------------------------------------------------

/*

document ::= xml(attributes, content) { well-formed document } |
malformed(attributes, content) { malformed document }

attributes ::= [] | [name=chardata | attributes]

content ::= [] | [cterm | content]

cterm ::= pcdata(char-data) { text } |
comment(char-data) { an XML comment } |
namespace(URI,prefix,element) { a Namespace } |
element(tag, attributes, content) { <tag>..</tag> encloses content or <tag /> if empty } |
instructions(name,chardata) { A PI <? name char-data ?> } |
cdata(char-data) { <![CDATA[char-data]]> } |
doctype(tag,doctype-id) { DTD <!DOCTYPE .. > } |
unparsed(char-data) { text that hasn't been parsed } |
out_of_context(tag) { tag is not closed }

tag ::= atom { naming an element }

name ::= atom { not naming an element }

URI ::= atom { giving the URI of a namespace }

char-data ::= code-list

doctype-id ::= public(char-data,chardata) |
public(char-data,dtdliterals) |
system(char-data) |
system(char-data,dtdliterals) |
local |
local,dtd-literals

dtd-literals ::= [] |
[dtd_literal(chardata) | dtd-literals]
*/

prolog_to_xml_term(Var, element(var, [], [pcdata(Codes)])) :-
	var(Var),
	format_to_atom('~w', [Var], Atom),
	atom_codes(Atom, Codes),
	!.
prolog_to_xml_term(Atom, element(atom, [], [pcdata(Codes)])) :-
	atom(Atom),
	atom_codes(Atom, Codes),
	!.
prolog_to_xml_term(N, element(number, [], [pcdata(Codes)])) :-
	number(N),
	number_codes(N, Codes),
	!.
% Special case for list
prolog_to_xml_term(List, element(list, [], XMLList)) :-
	is_list(List),
	prolog_list_to_xml_term_list(List, XMLList),
	!.
% Special case for functor wrapping list, e.g. foo([1, 2, 3])
prolog_to_xml_term(Term, element(TagName, [], XMLList)) :-
	compound(Term),
	functor(Term, F, 1),
	arg(1, Term, List),
	is_list(List),	
	prolog_list_wrapper_to_xml_tag_name(F, TagName),
	prolog_list_to_xml_term_list(List, XMLList),
	!.
% Special case for key/val pair, e.g. foo=bar
prolog_to_xml_term((Key = Value), element(TagName, [], [XMLValue])) :-
	prolog_key_to_xml_tag_name(Key, TagName),
	prolog_to_xml_term(Value, XMLValue),
	!.
% Normal term
prolog_to_xml_term(Term, element(TagName, [], XMLArgs)) :-
	( compound(Term) ; atom(Term) ),
	Term =.. [Functor | Args],
	prolog_functor_to_xml_tag_name(Functor, TagName),
	prolog_list_to_xml_term_list(Args, XMLArgs),
	!.
prolog_to_xml_term(Prolog, XMLTerm) :-
	format('~N*** Error: bad call: ~w~n', [prolog_to_xml_term(Prolog, XMLTerm)]),
	fail.

prolog_list_to_xml_term_list([], []).
prolog_list_to_xml_term_list([F | R], [F1 | R1]) :-
	prolog_to_xml_term(F, F1),
	!,
	prolog_list_to_xml_term_list(R, R1).
prolog_list_to_xml_term_list(Args, XMLArgs) :-
	format('~N*** Error: bad call: ~w~n', [prolog_list_to_xml_term_list(Args, XMLArgs)]),
	fail.

%------------------------------------------------------------------------------------

prolog_functor_to_xml_tag_name(Functor, TagName) :-
	atom_codes(Functor, String),
	string_to_xml_tag_string(String, TagString),
	atom_codes(TagName, TagString).

prolog_key_to_xml_tag_name(Key, TagName) :-
	prolog_functor_to_xml_tag_name(Key, Key1),
	format_to_atom('~w---key', [Key1], TagName).

prolog_list_wrapper_to_xml_tag_name(F, TagName) :-
	prolog_functor_to_xml_tag_name(F, F1),
	format_to_atom('~w---list', [F1], TagName).
	
%------------------------------------------------------------------------------------

xml_tag_name_to_prolog_functor(TagName, Functor) :-
	atom_codes(TagName, TagString),
	xml_tag_string_to_string(TagString, String),
	atom_codes(Functor, String).

xml_tag_name_to_prolog_list_wrapper(TagName, F) :-
	atom_codes(TagName, TagNameString),
	append(FString, "---list", TagNameString),
	atom_codes(F0, FString),
	xml_tag_name_to_prolog_functor(F0, F).

xml_tag_name_to_prolog_key(TagName, F) :-
	atom_codes(TagName, TagNameString),
	append(FString, "---key", TagNameString),
	atom_codes(F0, FString),
	xml_tag_name_to_prolog_functor(F0, F).

%------------------------------------------------------------------------------------

xml_term_to_prolog(XMLTerm, Prolog) :-
	xml_term_to_prolog(XMLTerm, Prolog, []-_VarAssocFinal).

xml_term_list_to_prolog_list(XMLTerm, Prolog) :-
	xml_term_list_to_prolog_list(XMLTerm, Prolog, []-_VarAssocFinal).

xml_term_to_prolog(element(var, [], [pcdata(Codes)]), Var, VarAssocIn-VarAssocOut) :-
	atom_codes(VarName, Codes),
	(   member(VarName-Var, VarAssocIn) ->
	    VarAssocOut = VarAssocIn
	;
	    otherwise ->
	    VarAssocOut = [VarName-Var | VarAssocIn]
	),
	!.
xml_term_to_prolog(element(atom, [], [pcdata(Codes)]), Atom, VarAssocIn-VarAssocIn) :-
	atom_codes(Atom, Codes),
	!.
xml_term_to_prolog(element(string, [], [pcdata(String)]), String, VarAssocIn-VarAssocIn) :-
	!.
xml_term_to_prolog(element(number, [], [pcdata(Codes)]), N, VarAssocIn-VarAssocIn) :-
	number_codes(N, Codes),
	!.
% Special case for list
xml_term_to_prolog(element(list, [], XMLList), List, VarAssocIn-VarAssocOut) :-
	xml_term_list_to_prolog_list(XMLList, List, VarAssocIn-VarAssocOut),
	!.
% Special case for functor wrapping list, e.g. foo([1, 2, 3])
xml_term_to_prolog(element(TagName, [], XMLList), Term, VarAssocIn-VarAssocOut) :-
	xml_tag_name_to_prolog_list_wrapper(TagName, F),
	functor(Term, F, 1),
	arg(1, Term, List),
	xml_term_list_to_prolog_list(XMLList, List, VarAssocIn-VarAssocOut),
	!.
% Special case for key/val pair, e.g. foo=bar
xml_term_to_prolog(element(TagName, [], [XMLValue]), (Key = Value), VarAssocIn-VarAssocOut) :-
	xml_tag_name_to_prolog_key(TagName, Key),
	xml_term_to_prolog(XMLValue, Value, VarAssocIn-VarAssocOut),
	!.
xml_term_to_prolog(element(TagName, [], XMLList), Term, VarAssocIn-VarAssocOut) :-
	xml_tag_name_to_prolog_functor(TagName, Functor),
	xml_term_list_to_prolog_list(XMLList, List, VarAssocIn-VarAssocOut),
	Term =.. [Functor | List],
	!.
xml_term_to_prolog(XMLTerm, Prolog, VarAssoc) :-
	format('~N*** Error: bad call: ~w~n', [xml_term_to_prolog(XMLTerm, Prolog, VarAssoc)]),
	fail.

xml_term_list_to_prolog_list([], [], VarAssocIn-VarAssocIn) :-
	!.
xml_term_list_to_prolog_list([F | R], [F1 | R1], VarAssocIn-VarAssocOut) :-
	xml_term_to_prolog(F, F1, VarAssocIn-VarAssocNext),
	!,
	xml_term_list_to_prolog_list(R, R1, VarAssocNext-VarAssocOut).
xml_term_list_to_prolog_list(XMLList, List, VarAssoc) :-
	format('~N*** Error: bad call: ~w~n', [xml_term_list_to_prolog_list(XMLList, List, VarAssoc)]),
	fail.

%------------------------------------------------------------------------------------

xml_tag_string_to_string(TagString, String) :-
	string_to_xml_tag_string(String, TagString).

string_to_xml_tag_string([], []).
string_to_xml_tag_string([F | R], Result) :-
	escape_sequence_for_char(F, EscapeSequence),
	append(EscapeSequence, R1, Result),
	!,
	string_to_xml_tag_string(R, R1).
string_to_xml_tag_string([F | R], [F | R1]) :-
	!,
	string_to_xml_tag_string(R, R1).

escape_sequence_for_char(0' , "XXSPAXX").
escape_sequence_for_char(0',, "XXCOMXX").
escape_sequence_for_char(0'., "XXPERXX").
escape_sequence_for_char(0';, "XXSEMXX").
escape_sequence_for_char(0':, "XXCOLXX").
escape_sequence_for_char(0'-, "XXDSHXX").
escape_sequence_for_char(0'&, "XXAMPXX").
escape_sequence_for_char(0'!, "XXEXCXX").

escape_sequence_for_char(0'=, "XXEQUXX").
escape_sequence_for_char(0'<, "XXLTXX").
escape_sequence_for_char(0'>, "XXGTXX").
escape_sequence_for_char(0'/, "XXSLAXX").
escape_sequence_for_char(0'\\, "XXBSLXX").
escape_sequence_for_char(0'(, "XXLPRXX").
escape_sequence_for_char(0'), "XXRPRXX").
escape_sequence_for_char(0'\', "XXQUOXX").
escape_sequence_for_char(0'\", "XXDBLQUXX").
escape_sequence_for_char(0'$, "XXDOLXX").
escape_sequence_for_char(0'�, "XXPOUXX").
escape_sequence_for_char(0'@, "XXATXX").
escape_sequence_for_char(0'#, "XXHASHXX").
escape_sequence_for_char(0'%, "XXPERXX").
escape_sequence_for_char(0'+, "XXPLUXX").

escape_sequence_for_char(0'�, "XXa1XX").
escape_sequence_for_char(0'�, "XXa2XX").
escape_sequence_for_char(0'�, "XXa3XX").
escape_sequence_for_char(0'�, "XXa4XX").
escape_sequence_for_char(0'�, "XXa5XX").

escape_sequence_for_char(0'�, "XXc1XX").

escape_sequence_for_char(0'�, "XXe1XX").
escape_sequence_for_char(0'�, "XXe2XX").
escape_sequence_for_char(0'�, "XXe3XX").
escape_sequence_for_char(0'�, "XXe4XX").
escape_sequence_for_char(0'�, "XXe6XX").

escape_sequence_for_char(0'�, "XXi1XX").
escape_sequence_for_char(0'�, "XXi2XX").
escape_sequence_for_char(0'�, "XXi3XX").
escape_sequence_for_char(0'�, "XXi4XX").

escape_sequence_for_char(0'�, "XXn1XX").

escape_sequence_for_char(0'�, "XXo1XX").
escape_sequence_for_char(0'�, "XXo2XX").
escape_sequence_for_char(0'�, "XXo3XX").
escape_sequence_for_char(0'�, "XXo4XX").

escape_sequence_for_char(0'�, "XXu1XX").
escape_sequence_for_char(0'�, "XXu2XX").
escape_sequence_for_char(0'�, "XXu3XX").
escape_sequence_for_char(0'�, "XXu4XX").

escape_sequence_for_char(0'�, "XXA1XX").
escape_sequence_for_char(0'�, "XXA2XX").
escape_sequence_for_char(0'�, "XXA3XX").
escape_sequence_for_char(0'�, "XXA4XX").
escape_sequence_for_char(0'�, "XXA5XX").

escape_sequence_for_char(0'�, "XXC1XX").

escape_sequence_for_char(0'�, "XXE1XX").
escape_sequence_for_char(0'�, "XXE2XX").
escape_sequence_for_char(0'�, "XXE3XX").
escape_sequence_for_char(0'�, "XXE4XX").
escape_sequence_for_char(0'�, "XXE6XX").

escape_sequence_for_char(0'�, "XXI1XX").
escape_sequence_for_char(0'�, "XXI2XX").
escape_sequence_for_char(0'�, "XXI3XX").
escape_sequence_for_char(0'�, "XXI4XX").

escape_sequence_for_char(0'�, "XXN1XX").

escape_sequence_for_char(0'�, "XXO1XX").
escape_sequence_for_char(0'�, "XXO2XX").
escape_sequence_for_char(0'�, "XXO3XX").
escape_sequence_for_char(0'�, "XXO4XX").

escape_sequence_for_char(0'�, "XXU1XX").
escape_sequence_for_char(0'�, "XXU2XX").
escape_sequence_for_char(0'�, "XXU3XX").
escape_sequence_for_char(0'�, "XXU4XX").

