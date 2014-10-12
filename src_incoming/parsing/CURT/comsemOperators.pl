/*************************************************************************

         name: comsemOperators.pl
      version: May 25, 1999
  description: Operator definitions
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(comsemOperators,[

op(950,yfx,@),         % application
op(900,yfx,'<>'),      % bin impl
op(900,yfx,>),         % implication
op(850,yfx,v),         % disjunction
op(800,yfx,&),         % conjunction
op(750, fy,~)         % negation
]).

/*========================================================================
   Operator Definitions
========================================================================*/

:- op(950,yfx,@).         % application
:- op(900,yfx,'<>').      % bin impl
:- op(900,yfx,>).         % implication
:- op(850,yfx,v).         % disjunction
:- op(800,yfx,&).         % conjunction
:- op(750, fy,~).         % negation
