/*************************************************************************

    File: readLine.pl
    Copyright (C) 2007 

    First Programmer: Johan Bos and Patrick Blackburn
    Second Programmer: Luciana Benotti

    This file is part of Frolog, version 0.1 (October 2007).

    Frolog is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Frolog is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Frolog; if not, write to the Free Software Foundation, Inc., 
    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*************************************************************************/

:- module(readLine,[readLine/2]).

/*************************************************************************

    Function: Reads from a stream and convert into list of words

    Technical: 

    Wishlist: 

*************************************************************************/


/*========================================================================
   Reads from a stream and convert into list of words
========================================================================*/

readLine(Stream, WordList):-
   readWords(Stream,Words),
   checkWords(Words,WordList).


/*========================================================================
   Read in a sequence of characters, until an end of line is registered
========================================================================*/

readWords(Stream,[Word|Rest]):-
   get0(Stream,Char),
   readWord(Stream,Char,Chars,State),
   name(Word,Chars), 
   readRest(Stream,Rest,State).

readRest(_,[],ended).

readRest(Stream,Rest,notended):-
   readWords(Stream,Rest).


/*========================================================================
   Read a word coded as Chars (a list of ascii values), starting 
   with with ascii value Char, and determine the State of input
   (`ended' = end of line, `notended' = not end of line).
   Blanks and full stops split words, a return ends a line.
========================================================================*/

readWord(_,32,[],notended):-!.     %%% blank

readWord(_,46,[],notended):-!.     %%% full stop

readWord(_,10,[],ended):-!.        %%% return

readWord(Stream,Code,[Code|Rest],State):-
   get0(Stream,Char),
   readWord(Stream,Char,Rest,State).


/*========================================================================
   Check if all words are unquoted atoms, if not convert them into atoms.
========================================================================*/

checkWords([],[]):- !.

checkWords([''|Rest1],Rest2):-
   checkWords(Rest1,Rest2).

checkWords([Atom|Rest1],[Atom2|Rest2]):-
   name(Atom,Word1),
   convertWord(Word1,Word2),
   name(Atom2,Word2),
   checkWords(Rest1,Rest2).


/*========================================================================
   Convert upper into lower case characters, and eliminate
   non-alphabetic characters.
========================================================================*/

convertWord([],[]):- !.

convertWord([Capital|Rest1],[Small|Rest2]):-
   Capital > 64, Capital < 91, !,
   Small is Capital + 32,
   convertWord(Rest1,Rest2).

convertWord([Number|Rest1],[Number|Rest2]):-
   Number > 47, Number < 58, !,
   convertWord(Rest1,Rest2).

convertWord([Weird|Rest1],Rest2):-
   (Weird < 97; Weird > 122), !,
   convertWord(Rest1,Rest2).

convertWord([Char|Rest1],[Char|Rest2]):-
   convertWord(Rest1,Rest2).


