/*
 * GIPO COPYRIGHT NOTICE, LICENSE AND DISCLAIMER.
 *
 * Copyright 2001 by R.M.Simpson W.Zhao T.L.McCLuskey D Lui D. Kitchin
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appear in all copies and that
 * both the copyright notice and this permission notice and warranty
 * disclaimer appear in supporting documentation, and that the names of
 * the authors or their employers not be used in advertising or publicity 
 * pertaining to distribution of the software without specific, written 
 * prior permission.
 *
 * The authors and their employers disclaim all warranties with regard to 
 * this software, including all implied warranties of merchantability and 
 * fitness.  In no event shall the authors or their employers be liable 
 * for any special, indirect or consequential damages or any damages 
 * whatsoever resulting from loss of use, data or profits, whether in an 
 * action of contract, negligence or other tortious action, arising out of 
 * or in connection with the use or performance of this software.
 */
package jplan.tools.lifeHist;
import java_cup.runtime.Symbol;


public class Yylex implements java_cup.runtime.Scanner {
	private final int YY_BUFFER_SIZE = 512;
	private final int YY_F = -1;
	private final int YY_NO_STATE = -1;
	private final int YY_NOT_ACCEPT = 0;
	private final int YY_START = 1;
	private final int YY_END = 2;
	private final int YY_NO_ANCHOR = 4;
	private final int YY_BOL = 128;
	private final int YY_EOF = 129;
	private java.io.BufferedReader yy_reader;
	private int yy_buffer_index;
	private int yy_buffer_read;
	private int yy_buffer_start;
	private int yy_buffer_end;
	private char yy_buffer[];
	private int yychar;
	private int yyline;
	private boolean yy_at_bol;
	private int yy_lexical_state;

	public Yylex (java.io.Reader reader) {
		this ();
		if (null == reader) {
			throw (new Error("Error: Bad input stream initializer."));
		}
		yy_reader = new java.io.BufferedReader(reader);
	}

	public Yylex (java.io.InputStream instream) {
		this ();
		if (null == instream) {
			throw (new Error("Error: Bad input stream initializer."));
		}
		yy_reader = new java.io.BufferedReader(new java.io.InputStreamReader(instream));
	}

	private Yylex () {
		yy_buffer = new char[YY_BUFFER_SIZE];
		yy_buffer_read = 0;
		yy_buffer_index = 0;
		yy_buffer_start = 0;
		yy_buffer_end = 0;
		yychar = 0;
		yyline = 0;
		yy_at_bol = true;
		yy_lexical_state = YYINITIAL;
	}

	private boolean yy_eof_done = false;
	private final int YYINITIAL = 0;
	private final int yy_state_dtrans[] = {
		0
	};
	private void yybegin (int state) {
		yy_lexical_state = state;
	}
	private int yy_advance ()
		throws java.io.IOException {
		int next_read;
		int i;
		int j;

		if (yy_buffer_index < yy_buffer_read) {
			return yy_buffer[yy_buffer_index++];
		}

		if (0 != yy_buffer_start) {
			i = yy_buffer_start;
			j = 0;
			while (i < yy_buffer_read) {
				yy_buffer[j] = yy_buffer[i];
				++i;
				++j;
			}
			yy_buffer_end = yy_buffer_end - yy_buffer_start;
			yy_buffer_start = 0;
			yy_buffer_read = j;
			yy_buffer_index = j;
			next_read = yy_reader.read(yy_buffer,
					yy_buffer_read,
					yy_buffer.length - yy_buffer_read);
			if (-1 == next_read) {
				return YY_EOF;
			}
			yy_buffer_read = yy_buffer_read + next_read;
		}

		while (yy_buffer_index >= yy_buffer_read) {
			if (yy_buffer_index >= yy_buffer.length) {
				yy_buffer = yy_double(yy_buffer);
			}
			next_read = yy_reader.read(yy_buffer,
					yy_buffer_read,
					yy_buffer.length - yy_buffer_read);
			if (-1 == next_read) {
				return YY_EOF;
			}
			yy_buffer_read = yy_buffer_read + next_read;
		}
		return yy_buffer[yy_buffer_index++];
	}
	private void yy_move_end () {
		if (yy_buffer_end > yy_buffer_start &&
		    '\n' == yy_buffer[yy_buffer_end-1])
			yy_buffer_end--;
		if (yy_buffer_end > yy_buffer_start &&
		    '\r' == yy_buffer[yy_buffer_end-1])
			yy_buffer_end--;
	}
	private boolean yy_last_was_cr=false;
	private void yy_mark_start () {
		int i;
		for (i = yy_buffer_start; i < yy_buffer_index; ++i) {
			if ('\n' == yy_buffer[i] && !yy_last_was_cr) {
				++yyline;
			}
			if ('\r' == yy_buffer[i]) {
				++yyline;
				yy_last_was_cr=true;
			} else yy_last_was_cr=false;
		}
		yychar = yychar
			+ yy_buffer_index - yy_buffer_start;
		yy_buffer_start = yy_buffer_index;
	}
	private void yy_mark_end () {
		yy_buffer_end = yy_buffer_index;
	}
	private void yy_to_mark () {
		yy_buffer_index = yy_buffer_end;
		yy_at_bol = (yy_buffer_end > yy_buffer_start) &&
		            ('\r' == yy_buffer[yy_buffer_end-1] ||
		             '\n' == yy_buffer[yy_buffer_end-1] ||
		             2028/*LS*/ == yy_buffer[yy_buffer_end-1] ||
		             2029/*PS*/ == yy_buffer[yy_buffer_end-1]);
	}
	private java.lang.String yytext () {
		return (new java.lang.String(yy_buffer,
			yy_buffer_start,
			yy_buffer_end - yy_buffer_start));
	}
	private int yylength () {
		return yy_buffer_end - yy_buffer_start;
	}
	private char[] yy_double (char buf[]) {
		int i;
		char newbuf[];
		newbuf = new char[2*buf.length];
		for (i = 0; i < buf.length; ++i) {
			newbuf[i] = buf[i];
		}
		return newbuf;
	}
	private final int YY_E_INTERNAL = 0;
	private final int YY_E_MATCH = 1;
	private java.lang.String yy_error_string[] = {
		"Error: Internal error.\n",
		"Error: Unmatched input.\n"
	};
	private void yy_error (int code,boolean fatal) {
		java.lang.System.out.print(yy_error_string[code]);
		java.lang.System.out.flush();
		if (fatal) {
			throw new Error("Fatal Error.\n");
		}
	}
	private int[][] unpackFromString(int size1, int size2, String st) {
		int colonIndex = -1;
		String lengthString;
		int sequenceLength = 0;
		int sequenceInteger = 0;

		int commaIndex;
		String workString;

		int res[][] = new int[size1][size2];
		for (int i= 0; i < size1; i++) {
			for (int j= 0; j < size2; j++) {
				if (sequenceLength != 0) {
					res[i][j] = sequenceInteger;
					sequenceLength--;
					continue;
				}
				commaIndex = st.indexOf(',');
				workString = (commaIndex==-1) ? st :
					st.substring(0, commaIndex);
				st = st.substring(commaIndex+1);
				colonIndex = workString.indexOf(':');
				if (colonIndex == -1) {
					res[i][j]=Integer.parseInt(workString);
					continue;
				}
				lengthString =
					workString.substring(colonIndex+1);
				sequenceLength=Integer.parseInt(lengthString);
				workString=workString.substring(0,colonIndex);
				sequenceInteger=Integer.parseInt(workString);
				res[i][j] = sequenceInteger;
				sequenceLength--;
			}
		}
		return res;
	}
	private int yy_acpt[] = {
		/* 0 */ YY_NOT_ACCEPT,
		/* 1 */ YY_NO_ANCHOR,
		/* 2 */ YY_NO_ANCHOR,
		/* 3 */ YY_NO_ANCHOR,
		/* 4 */ YY_NO_ANCHOR,
		/* 5 */ YY_NO_ANCHOR,
		/* 6 */ YY_NO_ANCHOR,
		/* 7 */ YY_NO_ANCHOR,
		/* 8 */ YY_NO_ANCHOR,
		/* 9 */ YY_NO_ANCHOR,
		/* 10 */ YY_NO_ANCHOR,
		/* 11 */ YY_NO_ANCHOR,
		/* 12 */ YY_NO_ANCHOR,
		/* 13 */ YY_NO_ANCHOR,
		/* 14 */ YY_NO_ANCHOR,
		/* 15 */ YY_NO_ANCHOR,
		/* 16 */ YY_NO_ANCHOR,
		/* 17 */ YY_NO_ANCHOR,
		/* 18 */ YY_NO_ANCHOR,
		/* 19 */ YY_NO_ANCHOR,
		/* 20 */ YY_NO_ANCHOR,
		/* 21 */ YY_NO_ANCHOR,
		/* 22 */ YY_END,
		/* 23 */ YY_NO_ANCHOR,
		/* 24 */ YY_NO_ANCHOR,
		/* 25 */ YY_NO_ANCHOR,
		/* 26 */ YY_NO_ANCHOR,
		/* 27 */ YY_NO_ANCHOR,
		/* 28 */ YY_NO_ANCHOR,
		/* 29 */ YY_NO_ANCHOR,
		/* 30 */ YY_NO_ANCHOR,
		/* 31 */ YY_NO_ANCHOR,
		/* 32 */ YY_NOT_ACCEPT,
		/* 33 */ YY_NO_ANCHOR,
		/* 34 */ YY_NO_ANCHOR,
		/* 35 */ YY_END,
		/* 36 */ YY_NOT_ACCEPT,
		/* 37 */ YY_NO_ANCHOR,
		/* 38 */ YY_NO_ANCHOR,
		/* 39 */ YY_NO_ANCHOR,
		/* 40 */ YY_NO_ANCHOR,
		/* 41 */ YY_NO_ANCHOR,
		/* 42 */ YY_NO_ANCHOR,
		/* 43 */ YY_NO_ANCHOR,
		/* 44 */ YY_NO_ANCHOR,
		/* 45 */ YY_NO_ANCHOR,
		/* 46 */ YY_NO_ANCHOR,
		/* 47 */ YY_NO_ANCHOR,
		/* 48 */ YY_NO_ANCHOR,
		/* 49 */ YY_NO_ANCHOR,
		/* 50 */ YY_NO_ANCHOR,
		/* 51 */ YY_NO_ANCHOR,
		/* 52 */ YY_NO_ANCHOR,
		/* 53 */ YY_NO_ANCHOR,
		/* 54 */ YY_NO_ANCHOR,
		/* 55 */ YY_NO_ANCHOR,
		/* 56 */ YY_NO_ANCHOR,
		/* 57 */ YY_NO_ANCHOR,
		/* 58 */ YY_NO_ANCHOR,
		/* 59 */ YY_NO_ANCHOR,
		/* 60 */ YY_NO_ANCHOR,
		/* 61 */ YY_NO_ANCHOR,
		/* 62 */ YY_NO_ANCHOR,
		/* 63 */ YY_NO_ANCHOR,
		/* 64 */ YY_NO_ANCHOR,
		/* 65 */ YY_NO_ANCHOR,
		/* 66 */ YY_NO_ANCHOR,
		/* 67 */ YY_NO_ANCHOR,
		/* 68 */ YY_NO_ANCHOR,
		/* 69 */ YY_NO_ANCHOR,
		/* 70 */ YY_NO_ANCHOR,
		/* 71 */ YY_NO_ANCHOR
	};
	private int yy_cmap[] = unpackFromString(1,130,
"34:9,35,36,34,35,32,34:18,35,34:2,26,34,33,34:2,2,3,21,19,1,20,31,22,28:10," +
"34:2,24,23,25,34:2,29:26,34:4,30,34,4,27,9,12,11,27,7,27,6,27:2,13,27,8,16," +
"15,27,10,5,18,14,27,17,27:3,34:5,0,37")[0];

	private int yy_rmap[] = unpackFromString(1,72,
"0,1:4,2,1:5,3,4,5,6,7,1:2,8,1:3,9,8:2,10,8:6,10,11,1:2,12,13,12,14,15,16,17" +
",18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,8,38,39,40,41," +
"42,43,44,45")[0];

	private int yy_nxt[][] = unpackFromString(46,38,
"1,2,3,4,5,59,62,63,64,63:3,65,63:3,33,63,66,6,7,8,9,10,11,12,13,63,14,15,16" +
",34,17,38,34,17:2,1,-1:42,63,67,63:2,37,63:10,-1:8,63:4,-1:30,19,-1:37,20,-" +
"1:32,21,-1:47,14,-1:2,32,-1:10,15:15,-1:8,15:4,-1:11,63:15,-1:8,63:4,-1:43," +
"35,-1:29,25,-1:13,63:6,18,63:8,-1:8,63:4,-1:8,36:31,22,36:3,35:2,-1:4,63:8," +
"23,63:6,-1:8,63:4,-1:11,44,63:14,-1:8,63:4,-1:11,63:5,45,63:9,-1:8,63:4,-1:" +
"11,63:14,24,-1:8,63:4,-1:11,63,46,63:13,-1:8,63:4,-1:11,63:2,47,63:12,-1:8," +
"63:4,-1:11,63:9,48,63:5,-1:8,63:4,-1:11,63:6,60,63:8,-1:8,63:4,-1:11,63:14," +
"26,-1:8,63:4,-1:11,63:3,49,63:11,-1:8,63:4,-1:11,63:7,50,63:7,-1:8,63:4,-1:" +
"11,63:4,27,63:10,-1:8,63:4,-1:11,63:8,52,63,53,63:4,-1:8,63:4,-1:11,54,63:1" +
"4,-1:8,63:4,-1:11,63:12,55,63:2,-1:8,63:4,-1:11,63:11,28,63:3,-1:8,63:4,-1:" +
"11,63,56,63:13,-1:8,63:4,-1:11,63:13,58,63,-1:8,63:4,-1:11,63:7,29,63:7,-1:" +
"8,63:4,-1:11,63:7,30,63:7,-1:8,63:4,-1:11,63:4,31,63:10,-1:8,63:4,-1:11,63:" +
"5,39,63:9,-1:8,63:4,-1:11,63:7,51,63:7,-1:8,63:4,-1:11,63,57,63:13,-1:8,63:" +
"4,-1:11,63:4,40,63:10,-1:8,63:4,-1:11,63:12,41,63:2,-1:8,63:4,-1:11,63:7,71" +
",63:7,-1:8,63:4,-1:11,63:7,42,63:7,-1:8,63:4,-1:11,63,43,63:13,-1:8,63:4,-1" +
":11,61,63:14,-1:8,63:4,-1:11,63:7,68,63:7,-1:8,63:4,-1:11,63:6,69,63:8,-1:8" +
",63:4,-1:11,63:5,70,63:9,-1:8,63:4,-1:7");

	public java_cup.runtime.Symbol next_token ()
		throws java.io.IOException {
		int yy_lookahead;
		int yy_anchor = YY_NO_ANCHOR;
		int yy_state = yy_state_dtrans[yy_lexical_state];
		int yy_next_state = YY_NO_STATE;
		int yy_last_accept_state = YY_NO_STATE;
		boolean yy_initial = true;
		int yy_this_accept;

		yy_mark_start();
		yy_this_accept = yy_acpt[yy_state];
		if (YY_NOT_ACCEPT != yy_this_accept) {
			yy_last_accept_state = yy_state;
			yy_mark_end();
		}
		while (true) {
			if (yy_initial && yy_at_bol) yy_lookahead = YY_BOL;
			else yy_lookahead = yy_advance();
			yy_next_state = YY_F;
			yy_next_state = yy_nxt[yy_rmap[yy_state]][yy_cmap[yy_lookahead]];
			if (YY_EOF == yy_lookahead && true == yy_initial) {

  return new Symbol(sym.EOF);
			}
			if (YY_F != yy_next_state) {
				yy_state = yy_next_state;
				yy_initial = false;
				yy_this_accept = yy_acpt[yy_state];
				if (YY_NOT_ACCEPT != yy_this_accept) {
					yy_last_accept_state = yy_state;
					yy_mark_end();
				}
			}
			else {
				if (YY_NO_STATE == yy_last_accept_state) {
					throw (new Error("Lexical Error: Unmatched Input."));
				}
				else {
					yy_anchor = yy_acpt[yy_last_accept_state];
					if (0 != (YY_END & yy_anchor)) {
						yy_move_end();
					}
					yy_to_mark();
					switch (yy_last_accept_state) {
					case 1:
						
					case -2:
						break;
					case 2:
						{ return new Symbol(sym.COMMA,yychar,yyline,
				    new String(yytext())); }
					case -3:
						break;
					case 3:
						{ return new Symbol(sym.RBRA,yychar,yyline,
				    new String(yytext())); }
					case -4:
						break;
					case 4:
						{ return new Symbol(sym.RKET,yychar,yyline,
				    new String(yytext())); }
					case -5:
						break;
					case 5:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -6:
						break;
					case 6:
						{return new Symbol(sym.PLUS,yychar,yyline,
                              new String(yytext())); }
					case -7:
						break;
					case 7:
						{return new Symbol(sym.MINUS,yychar,yyline,
                              new String(yytext())); }
					case -8:
						break;
					case 8:
						{return new Symbol(sym.MULT,yychar,yyline,
                              new String(yytext())); }
					case -9:
						break;
					case 9:
						{return new Symbol(sym.DIV,yychar,yyline,
                              new String(yytext())); }
					case -10:
						break;
					case 10:
						{return new Symbol(sym.EQUALS,yychar,yyline,
                              new String(yytext())); }
					case -11:
						break;
					case 11:
						{return new Symbol(sym.LESS,yychar,yyline,
                              new String(yytext())); }
					case -12:
						break;
					case 12:
						{return new Symbol(sym.GREATER,yychar,yyline,
                              new String(yytext())); }
					case -13:
						break;
					case 13:
						{ System.err.println("Illegal character: "+yytext()); }
					case -14:
						break;
					case 14:
						{ return new Symbol(sym.NUMBER,yychar,yyline, 
				       new Integer(yytext())); }
					case -15:
						break;
					case 15:
						{ return new Symbol(sym.VAR,yychar,yyline, 
                                       new String(yytext())); }
					case -16:
						break;
					case 16:
						{ return new Symbol(sym.VAR,yychar,yyline, 
				    new String(yytext())); }
					case -17:
						break;
					case 17:
						{ /* ignore white space. */ }
					case -18:
						break;
					case 18:
						{return new Symbol(sym.OR,yychar,yyline,
                              new String(yytext())); }
					case -19:
						break;
					case 19:
						{return new Symbol(sym.LESSEQ,yychar,yyline,
                              new String(yytext())); }
					case -20:
						break;
					case 20:
						{return new Symbol(sym.GREATEREQ,yychar,yyline,
                              new String(yytext())); }
					case -21:
						break;
					case 21:
						{return new Symbol(sym.TIME,yychar,yyline,
                              new String(yytext())); }
					case -22:
						break;
					case 22:
						{ /* ignore comment. */ }
					case -23:
						break;
					case 23:
						{return new Symbol(sym.AND,yychar,yyline,
                              new String(yytext())); }
					case -24:
						break;
					case 24:
						{return new Symbol(sym.NOT,yychar,yyline,
                              new String(yytext())); }
					case -25:
						break;
					case 25:
						{ return new Symbol(sym.DOUBLE,yychar,yyline, 
				       new Double(yytext())); }
					case -26:
						break;
					case 26:
						{return new Symbol(sym.TEST,yychar,yyline,
                              new String(yytext())); }
					case -27:
						break;
					case 27:
						{return new Symbol(sym.ASSIGN,yychar,yyline,
                              new String(yytext())); }
					case -28:
						break;
					case 28:
						{return new Symbol(sym.SCALEUP,yychar,yyline,
                              new String(yytext())); }
					case -29:
						break;
					case 29:
						{return new Symbol(sym.INCREASE,yychar,yyline,
                              new String(yytext())); }
					case -30:
						break;
					case 30:
						{return new Symbol(sym.DECREASE,yychar,yyline,
                              new String(yytext())); }
					case -31:
						break;
					case 31:
						{return new Symbol(sym.SCALEDOWN,yychar,yyline,
                              new String(yytext())); }
					case -32:
						break;
					case 33:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -33:
						break;
					case 34:
						{ System.err.println("Illegal character: "+yytext()); }
					case -34:
						break;
					case 35:
						{ /* ignore comment. */ }
					case -35:
						break;
					case 37:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -36:
						break;
					case 38:
						{ System.err.println("Illegal character: "+yytext()); }
					case -37:
						break;
					case 39:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -38:
						break;
					case 40:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -39:
						break;
					case 41:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -40:
						break;
					case 42:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -41:
						break;
					case 43:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -42:
						break;
					case 44:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -43:
						break;
					case 45:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -44:
						break;
					case 46:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -45:
						break;
					case 47:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -46:
						break;
					case 48:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -47:
						break;
					case 49:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -48:
						break;
					case 50:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -49:
						break;
					case 51:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -50:
						break;
					case 52:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -51:
						break;
					case 53:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -52:
						break;
					case 54:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -53:
						break;
					case 55:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -54:
						break;
					case 56:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -55:
						break;
					case 57:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -56:
						break;
					case 58:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -57:
						break;
					case 59:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -58:
						break;
					case 60:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -59:
						break;
					case 61:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -60:
						break;
					case 62:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -61:
						break;
					case 63:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -62:
						break;
					case 64:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -63:
						break;
					case 65:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -64:
						break;
					case 66:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -65:
						break;
					case 67:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -66:
						break;
					case 68:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -67:
						break;
					case 69:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -68:
						break;
					case 70:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -69:
						break;
					case 71:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -70:
						break;
					default:
						yy_error(YY_E_INTERNAL,false);
					case -1:
					}
					yy_initial = true;
					yy_state = yy_state_dtrans[yy_lexical_state];
					yy_next_state = YY_NO_STATE;
					yy_last_accept_state = YY_NO_STATE;
					yy_mark_start();
					yy_this_accept = yy_acpt[yy_state];
					if (YY_NOT_ACCEPT != yy_this_accept) {
						yy_last_accept_state = yy_state;
						yy_mark_end();
					}
				}
			}
		}
	}
}
