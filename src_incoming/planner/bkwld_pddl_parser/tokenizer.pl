:- module(tokenizer,_,_).


tokenizer([],[]).

%tokenizer([101,110,100,95,111,102,95,102,105,108,101],_).

%layout spaces,newlines,tabs...

tokenizer([C|RC],Words):- ( C=32 ; C=10 ; C=9 ; C=13 ; C=92 ), !, tokenizer(RC,Words).


% Brackets, comma, period or question marks are treated as separed words

tokenizer([C|RC], [Char|Words_1]) :- ( C=40 ; C=41 ; C=44 ; C=45 ; C=46 ; C=63 ; C=58 ) , name(Char, [C]), !,

			tokenizer(RC, Words_1).


% Words

tokenizer([C|RC], [Word|Words_1]):- word([C|RC],Chars,Next), name(Word,Chars), tokenizer(Next,Words_1).
 


word([C|RC],[],[C|RC]):- ( C=32 ; C=44 ; C=10 ; C=9 ; C=13 ; C=46 ; C=63 ; C=40 ; C=41 ; C=58 ; C= -1 ) , !.

word([C|RC],[LC|Chars],Next):- lower_case(C,LC), word(RC,Chars,Next).



% Convert to lower case if necessary.

lower_case(C,C) :- ( C <  65 ; C > 90 ) , !.
lower_case(C,LC) :- LC is C + 32.

