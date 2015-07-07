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
package jplan.ocl;
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
	private final int DESCRIPTION = 2;
	private final int YYINITIAL = 0;
	private final int COMMENT = 1;
	private final int yy_state_dtrans[] = {
		0,
		97,
		68
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
		/* 22 */ YY_NO_ANCHOR,
		/* 23 */ YY_NO_ANCHOR,
		/* 24 */ YY_NO_ANCHOR,
		/* 25 */ YY_NO_ANCHOR,
		/* 26 */ YY_NO_ANCHOR,
		/* 27 */ YY_NO_ANCHOR,
		/* 28 */ YY_NO_ANCHOR,
		/* 29 */ YY_NO_ANCHOR,
		/* 30 */ YY_END,
		/* 31 */ YY_NO_ANCHOR,
		/* 32 */ YY_NO_ANCHOR,
		/* 33 */ YY_NO_ANCHOR,
		/* 34 */ YY_NO_ANCHOR,
		/* 35 */ YY_NO_ANCHOR,
		/* 36 */ YY_NO_ANCHOR,
		/* 37 */ YY_NO_ANCHOR,
		/* 38 */ YY_NO_ANCHOR,
		/* 39 */ YY_NO_ANCHOR,
		/* 40 */ YY_NO_ANCHOR,
		/* 41 */ YY_NO_ANCHOR,
		/* 42 */ YY_NO_ANCHOR,
		/* 43 */ YY_END,
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
		/* 63 */ YY_END,
		/* 64 */ YY_END,
		/* 65 */ YY_END,
		/* 66 */ YY_END,
		/* 67 */ YY_END,
		/* 68 */ YY_NO_ANCHOR,
		/* 69 */ YY_NO_ANCHOR,
		/* 70 */ YY_NO_ANCHOR,
		/* 71 */ YY_NO_ANCHOR,
		/* 72 */ YY_NOT_ACCEPT,
		/* 73 */ YY_NO_ANCHOR,
		/* 74 */ YY_NO_ANCHOR,
		/* 75 */ YY_NO_ANCHOR,
		/* 76 */ YY_END,
		/* 77 */ YY_END,
		/* 78 */ YY_NO_ANCHOR,
		/* 79 */ YY_END,
		/* 80 */ YY_END,
		/* 81 */ YY_END,
		/* 82 */ YY_END,
		/* 83 */ YY_END,
		/* 84 */ YY_NO_ANCHOR,
		/* 85 */ YY_NOT_ACCEPT,
		/* 86 */ YY_NO_ANCHOR,
		/* 87 */ YY_NO_ANCHOR,
		/* 88 */ YY_NO_ANCHOR,
		/* 89 */ YY_NO_ANCHOR,
		/* 90 */ YY_NOT_ACCEPT,
		/* 91 */ YY_NO_ANCHOR,
		/* 92 */ YY_NO_ANCHOR,
		/* 93 */ YY_NO_ANCHOR,
		/* 94 */ YY_NOT_ACCEPT,
		/* 95 */ YY_NO_ANCHOR,
		/* 96 */ YY_NO_ANCHOR,
		/* 97 */ YY_NOT_ACCEPT,
		/* 98 */ YY_NO_ANCHOR,
		/* 99 */ YY_NOT_ACCEPT,
		/* 100 */ YY_NO_ANCHOR,
		/* 101 */ YY_NOT_ACCEPT,
		/* 102 */ YY_NO_ANCHOR,
		/* 103 */ YY_NOT_ACCEPT,
		/* 104 */ YY_NO_ANCHOR,
		/* 105 */ YY_NOT_ACCEPT,
		/* 106 */ YY_NO_ANCHOR,
		/* 107 */ YY_NOT_ACCEPT,
		/* 108 */ YY_NO_ANCHOR,
		/* 109 */ YY_NOT_ACCEPT,
		/* 110 */ YY_NO_ANCHOR,
		/* 111 */ YY_NOT_ACCEPT,
		/* 112 */ YY_NO_ANCHOR,
		/* 113 */ YY_NOT_ACCEPT,
		/* 114 */ YY_NO_ANCHOR,
		/* 115 */ YY_NOT_ACCEPT,
		/* 116 */ YY_NO_ANCHOR,
		/* 117 */ YY_NOT_ACCEPT,
		/* 118 */ YY_NO_ANCHOR,
		/* 119 */ YY_NOT_ACCEPT,
		/* 120 */ YY_NO_ANCHOR,
		/* 121 */ YY_NOT_ACCEPT,
		/* 122 */ YY_NO_ANCHOR,
		/* 123 */ YY_NOT_ACCEPT,
		/* 124 */ YY_NO_ANCHOR,
		/* 125 */ YY_NOT_ACCEPT,
		/* 126 */ YY_NO_ANCHOR,
		/* 127 */ YY_NOT_ACCEPT,
		/* 128 */ YY_NO_ANCHOR,
		/* 129 */ YY_NOT_ACCEPT,
		/* 130 */ YY_NO_ANCHOR,
		/* 131 */ YY_NOT_ACCEPT,
		/* 132 */ YY_NO_ANCHOR,
		/* 133 */ YY_NOT_ACCEPT,
		/* 134 */ YY_NO_ANCHOR,
		/* 135 */ YY_NOT_ACCEPT,
		/* 136 */ YY_NO_ANCHOR,
		/* 137 */ YY_NOT_ACCEPT,
		/* 138 */ YY_NO_ANCHOR,
		/* 139 */ YY_NOT_ACCEPT,
		/* 140 */ YY_NO_ANCHOR,
		/* 141 */ YY_NOT_ACCEPT,
		/* 142 */ YY_NO_ANCHOR,
		/* 143 */ YY_NOT_ACCEPT,
		/* 144 */ YY_NO_ANCHOR,
		/* 145 */ YY_NOT_ACCEPT,
		/* 146 */ YY_NO_ANCHOR,
		/* 147 */ YY_NOT_ACCEPT,
		/* 148 */ YY_NO_ANCHOR,
		/* 149 */ YY_NOT_ACCEPT,
		/* 150 */ YY_NOT_ACCEPT,
		/* 151 */ YY_NOT_ACCEPT,
		/* 152 */ YY_NOT_ACCEPT,
		/* 153 */ YY_NOT_ACCEPT,
		/* 154 */ YY_NOT_ACCEPT,
		/* 155 */ YY_NOT_ACCEPT,
		/* 156 */ YY_NOT_ACCEPT,
		/* 157 */ YY_NOT_ACCEPT,
		/* 158 */ YY_NOT_ACCEPT,
		/* 159 */ YY_NOT_ACCEPT,
		/* 160 */ YY_NOT_ACCEPT,
		/* 161 */ YY_NOT_ACCEPT,
		/* 162 */ YY_NOT_ACCEPT,
		/* 163 */ YY_NOT_ACCEPT,
		/* 164 */ YY_NOT_ACCEPT,
		/* 165 */ YY_NOT_ACCEPT,
		/* 166 */ YY_NOT_ACCEPT,
		/* 167 */ YY_NOT_ACCEPT,
		/* 168 */ YY_NOT_ACCEPT,
		/* 169 */ YY_NOT_ACCEPT,
		/* 170 */ YY_NOT_ACCEPT,
		/* 171 */ YY_NO_ANCHOR,
		/* 172 */ YY_NOT_ACCEPT,
		/* 173 */ YY_NOT_ACCEPT,
		/* 174 */ YY_NOT_ACCEPT,
		/* 175 */ YY_NOT_ACCEPT,
		/* 176 */ YY_NOT_ACCEPT,
		/* 177 */ YY_NOT_ACCEPT,
		/* 178 */ YY_NO_ANCHOR,
		/* 179 */ YY_NO_ANCHOR,
		/* 180 */ YY_NO_ANCHOR,
		/* 181 */ YY_NO_ANCHOR,
		/* 182 */ YY_NO_ANCHOR,
		/* 183 */ YY_NO_ANCHOR,
		/* 184 */ YY_NO_ANCHOR,
		/* 185 */ YY_NO_ANCHOR,
		/* 186 */ YY_NO_ANCHOR,
		/* 187 */ YY_NO_ANCHOR,
		/* 188 */ YY_NO_ANCHOR,
		/* 189 */ YY_NO_ANCHOR,
		/* 190 */ YY_NO_ANCHOR,
		/* 191 */ YY_NO_ANCHOR,
		/* 192 */ YY_NO_ANCHOR,
		/* 193 */ YY_NO_ANCHOR,
		/* 194 */ YY_NO_ANCHOR,
		/* 195 */ YY_NO_ANCHOR,
		/* 196 */ YY_NO_ANCHOR,
		/* 197 */ YY_NO_ANCHOR,
		/* 198 */ YY_NO_ANCHOR,
		/* 199 */ YY_NO_ANCHOR,
		/* 200 */ YY_NO_ANCHOR,
		/* 201 */ YY_NO_ANCHOR,
		/* 202 */ YY_NO_ANCHOR,
		/* 203 */ YY_NO_ANCHOR,
		/* 204 */ YY_NO_ANCHOR,
		/* 205 */ YY_NO_ANCHOR,
		/* 206 */ YY_NOT_ACCEPT,
		/* 207 */ YY_NO_ANCHOR,
		/* 208 */ YY_NO_ANCHOR,
		/* 209 */ YY_NO_ANCHOR,
		/* 210 */ YY_NO_ANCHOR,
		/* 211 */ YY_NO_ANCHOR,
		/* 212 */ YY_NO_ANCHOR,
		/* 213 */ YY_NO_ANCHOR,
		/* 214 */ YY_NO_ANCHOR,
		/* 215 */ YY_NO_ANCHOR,
		/* 216 */ YY_NO_ANCHOR,
		/* 217 */ YY_NO_ANCHOR,
		/* 218 */ YY_NO_ANCHOR,
		/* 219 */ YY_NO_ANCHOR,
		/* 220 */ YY_NO_ANCHOR,
		/* 221 */ YY_NO_ANCHOR,
		/* 222 */ YY_NO_ANCHOR,
		/* 223 */ YY_NO_ANCHOR,
		/* 224 */ YY_NO_ANCHOR,
		/* 225 */ YY_NO_ANCHOR,
		/* 226 */ YY_NO_ANCHOR,
		/* 227 */ YY_NO_ANCHOR,
		/* 228 */ YY_NO_ANCHOR,
		/* 229 */ YY_NO_ANCHOR,
		/* 230 */ YY_NO_ANCHOR,
		/* 231 */ YY_NO_ANCHOR,
		/* 232 */ YY_NO_ANCHOR,
		/* 233 */ YY_NOT_ACCEPT,
		/* 234 */ YY_NO_ANCHOR,
		/* 235 */ YY_NO_ANCHOR,
		/* 236 */ YY_NO_ANCHOR,
		/* 237 */ YY_NO_ANCHOR,
		/* 238 */ YY_NO_ANCHOR,
		/* 239 */ YY_NO_ANCHOR,
		/* 240 */ YY_NO_ANCHOR,
		/* 241 */ YY_NO_ANCHOR,
		/* 242 */ YY_NO_ANCHOR,
		/* 243 */ YY_NO_ANCHOR,
		/* 244 */ YY_NO_ANCHOR,
		/* 245 */ YY_NO_ANCHOR,
		/* 246 */ YY_NO_ANCHOR,
		/* 247 */ YY_NO_ANCHOR,
		/* 248 */ YY_NO_ANCHOR,
		/* 249 */ YY_NO_ANCHOR,
		/* 250 */ YY_NO_ANCHOR,
		/* 251 */ YY_NO_ANCHOR,
		/* 252 */ YY_NO_ANCHOR,
		/* 253 */ YY_NO_ANCHOR,
		/* 254 */ YY_NO_ANCHOR,
		/* 255 */ YY_NO_ANCHOR,
		/* 256 */ YY_NOT_ACCEPT,
		/* 257 */ YY_NO_ANCHOR,
		/* 258 */ YY_NO_ANCHOR,
		/* 259 */ YY_NO_ANCHOR,
		/* 260 */ YY_NO_ANCHOR,
		/* 261 */ YY_NO_ANCHOR,
		/* 262 */ YY_NO_ANCHOR,
		/* 263 */ YY_NO_ANCHOR,
		/* 264 */ YY_NO_ANCHOR,
		/* 265 */ YY_NO_ANCHOR,
		/* 266 */ YY_NO_ANCHOR,
		/* 267 */ YY_NO_ANCHOR,
		/* 268 */ YY_NO_ANCHOR,
		/* 269 */ YY_NO_ANCHOR,
		/* 270 */ YY_NO_ANCHOR,
		/* 271 */ YY_NO_ANCHOR,
		/* 272 */ YY_NO_ANCHOR,
		/* 273 */ YY_NO_ANCHOR,
		/* 274 */ YY_NOT_ACCEPT,
		/* 275 */ YY_NO_ANCHOR,
		/* 276 */ YY_NO_ANCHOR,
		/* 277 */ YY_NO_ANCHOR,
		/* 278 */ YY_NO_ANCHOR,
		/* 279 */ YY_NO_ANCHOR,
		/* 280 */ YY_NO_ANCHOR,
		/* 281 */ YY_NO_ANCHOR,
		/* 282 */ YY_NO_ANCHOR,
		/* 283 */ YY_NO_ANCHOR,
		/* 284 */ YY_NO_ANCHOR,
		/* 285 */ YY_NO_ANCHOR,
		/* 286 */ YY_NO_ANCHOR,
		/* 287 */ YY_NO_ANCHOR,
		/* 288 */ YY_NO_ANCHOR,
		/* 289 */ YY_NO_ANCHOR,
		/* 290 */ YY_NO_ANCHOR,
		/* 291 */ YY_NO_ANCHOR,
		/* 292 */ YY_NO_ANCHOR,
		/* 293 */ YY_NO_ANCHOR,
		/* 294 */ YY_NO_ANCHOR,
		/* 295 */ YY_NO_ANCHOR,
		/* 296 */ YY_NO_ANCHOR,
		/* 297 */ YY_NO_ANCHOR,
		/* 298 */ YY_NO_ANCHOR,
		/* 299 */ YY_NO_ANCHOR,
		/* 300 */ YY_NO_ANCHOR,
		/* 301 */ YY_NO_ANCHOR,
		/* 302 */ YY_NO_ANCHOR,
		/* 303 */ YY_NO_ANCHOR,
		/* 304 */ YY_NO_ANCHOR,
		/* 305 */ YY_NO_ANCHOR,
		/* 306 */ YY_NO_ANCHOR,
		/* 307 */ YY_NO_ANCHOR,
		/* 308 */ YY_NO_ANCHOR,
		/* 309 */ YY_NO_ANCHOR,
		/* 310 */ YY_NO_ANCHOR,
		/* 311 */ YY_NO_ANCHOR,
		/* 312 */ YY_NO_ANCHOR,
		/* 313 */ YY_NO_ANCHOR,
		/* 314 */ YY_NO_ANCHOR,
		/* 315 */ YY_NO_ANCHOR,
		/* 316 */ YY_NO_ANCHOR,
		/* 317 */ YY_NO_ANCHOR,
		/* 318 */ YY_NO_ANCHOR,
		/* 319 */ YY_NO_ANCHOR,
		/* 320 */ YY_NO_ANCHOR,
		/* 321 */ YY_NO_ANCHOR,
		/* 322 */ YY_NO_ANCHOR,
		/* 323 */ YY_NO_ANCHOR,
		/* 324 */ YY_NO_ANCHOR,
		/* 325 */ YY_NO_ANCHOR,
		/* 326 */ YY_NO_ANCHOR,
		/* 327 */ YY_NO_ANCHOR,
		/* 328 */ YY_NO_ANCHOR,
		/* 329 */ YY_NO_ANCHOR,
		/* 330 */ YY_NO_ANCHOR,
		/* 331 */ YY_NO_ANCHOR,
		/* 332 */ YY_NO_ANCHOR,
		/* 333 */ YY_NO_ANCHOR,
		/* 334 */ YY_NO_ANCHOR,
		/* 335 */ YY_NO_ANCHOR,
		/* 336 */ YY_NO_ANCHOR
	};
	private int yy_cmap[] = unpackFromString(1,130,
"47:9,41,42,47,41,43,47:18,50,47:2,37,47,51,47:2,3,4,34,32,2,33,1,35,39:10,4" +
"6,47,36,29,30,47:2,45,40:2,49,40:4,48,40:17,5,47,6,47,13,47,10,18,20,7,14,2" +
"2,28,26,11,19,27,24,9,12,8,21,38,16,15,17,23,25,31,38:3,47:5,0,44")[0];

	private int yy_rmap[] = unpackFromString(1,337,
"0,1:7,2,1,3,4,1:3,5,6,7,8,9,1,10:3,11,1:2,12,1:3,10:2,13,10:9,1,10:17,1:7,1" +
"4,15,16,1,13,17,1,18,19,20,21,22,23,24,25,26,18,27,28,27,29,30,31,32,33,34," +
"35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59," +
"60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84," +
"85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,10" +
"7,108,109,110,16,111,112,113,114,115,116,117,118,119,120,121,122,123,124,12" +
"5,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,1" +
"44,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162," +
"163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181" +
",182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,20" +
"0,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,2" +
"19,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237," +
"238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,256" +
",257,258,259,260,261,262,263,264,265,10,266,267,268,269,270,271,272,273,274" +
",275")[0];

	private int yy_nxt[][] = unpackFromString(276,52,
"1,2,3,4,5,6,7,8,73,255,171,321,178,9,232,86,326,205,257,326:2,273,329,326:2" +
",234,331,326,207,10,11,326,12,13,14,15,16,17,326,18,19,20:3,1,19,74:2,19:2," +
"20,87,-1:59,326,333,326:5,275,326:14,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:" +
"2,-1:32,25,-1:50,26,-1:56,27,-1:46,28,-1:39,29,-1:35,72,-1:37,18,-1:19,19:2" +
"2,-1:2,19,-1:6,19:3,-1:4,19,-1:2,19:2,-1:9,326:22,-1:2,326,-1:6,326:3,-1:4," +
"326,-1:2,326:2,-1:9,326:3,241,326:18,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:" +
"2,-1:36,90,-1:56,33,-1:12,1,84:33,69,70,84:5,75,20,75,1,84:5,75,84,-1,84:33" +
",89,71,84:6,-1,84,-1,84:7,-1,84:33,-1,93,84:6,-1,84,-1,84:7,-1:7,326:9,21,3" +
"26,258,326:2,235,326:7,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:3,84:33,1" +
"69,170,84:6,-1,84,-1,84:7,-1:42,30,-1:51,43,-1:44,62,-1:58,63,-1:51,64,-1:5" +
"1,65,-1:51,66,-1:51,67,-1:10,85:41,30,76,30,85:7,-1:7,326,209,326:5,22,23,3" +
"26:4,24,326:2,279,326:5,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:25,99,-1" +
":29,84:33,89,170,84:6,-1,84,-1,84:7,-1:34,172,-1:24,31,326:21,-1:2,326,-1:6" +
",326:3,-1:4,326,-1:2,326:2,-1:14,101,-1:40,84:33,169,93,84:6,-1,84,-1,84:7," +
"-1:42,43,77,43,-1:14,326:10,32,326:11,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326" +
":2,-1:12,173,-1:3,174,-1:37,1,61:33,78,61:6,20:3,1,88,61:2,92,96,20,61,-1:7" +
",326:10,34,326:11,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:19,103,-1:41,3" +
"26:17,35,326:4,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:17,105,-1:43,326:" +
"10,36,326:11,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:28,111,-1:32,326:8," +
"37,326:13,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:19,113,-1:41,326:7,38," +
"326:14,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:16,115,-1:44,326:5,39,326" +
":16,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:22,117,-1:38,40,326:21,-1:2," +
"326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:10,175,-1:50,326:5,41,326:16,-1:2,326" +
",-1:6,326:3,-1:4,326,-1:2,326:2,-1:13,119,-1:47,326:7,42,326:14,-1:2,326,-1" +
":6,326:3,-1:4,326,-1:2,326:2,-1:52,121,-1:8,326:8,44,326:13,-1:2,326,-1:6,3" +
"26:3,-1:4,326,-1:2,326:2,-1:18,123,-1:42,326:7,45,326:14,-1:2,326,-1:6,326:" +
"3,-1:4,326,-1:2,326:2,-1:19,127,-1:41,326:14,46,326:7,-1:2,326,-1:6,326:3,-" +
"1:4,326,-1:2,326:2,-1:22,129,-1:3,131,-1:34,326:8,47,326:13,-1:2,326,-1:6,3" +
"26:3,-1:4,326,-1:2,326:2,-1:13,133,-1:47,326:7,48,326:14,-1:2,326,-1:6,326:" +
"3,-1:4,326,-1:2,326:2,-1:48,135,-1:12,326:9,49,326:12,-1:2,326,-1:6,326:3,-" +
"1:4,326,-1:2,326:2,-1:25,137,-1:35,326:7,50,326:14,-1:2,326,-1:6,326:3,-1:4" +
",326,-1:2,326:2,-1:18,139,-1:42,326:8,51,326:13,-1:2,326,-1:6,326:3,-1:4,32" +
"6,-1:2,326:2,-1:12,141,-1:48,326:20,52,326,-1:2,326,-1:6,326:3,-1:4,326,-1:" +
"2,326:2,-1:23,274,-1:37,326:5,53,326:16,-1:2,326,-1:6,326:3,-1:4,326,-1:2,3" +
"26:2,-1:3,135:41,63,79,63,135:7,-1:7,326:8,54,326:13,-1:2,326,-1:6,326:3,-1" +
":4,326,-1:2,326:2,-1:19,143,-1:41,326:7,55,326:14,-1:2,326,-1:6,326:3,-1:4," +
"326,-1:2,326:2,-1:16,145,-1:44,326:20,56,326,-1:2,326,-1:6,326:3,-1:4,326,-" +
"1:2,326:2,-1:17,147,-1:43,326:8,57,326:13,-1:2,326,-1:6,326:3,-1:4,326,-1:2" +
",326:2,-1:13,149,-1:47,326:8,58,326:13,-1:2,326,-1:6,326:3,-1:4,326,-1:2,32" +
"6:2,-1:12,176,-1:48,326:10,59,326:11,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:" +
"2,-1:19,150,-1:41,326:10,60,326:11,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2," +
"-1:10,151,-1:93,153,-1:13,154,-1:53,155,-1:46,156,-1:88,157,-1:12,158,-1:52" +
",159,-1:44,157:41,64,80,64,157:7,-1:46,161,-1:12,162,-1:45,160:41,65,81,65," +
"160:7,-1,161:41,66,82,66,161:7,-1:11,163,-1:62,164,-1:40,165,-1:54,166,-1:4" +
"4,167,-1:90,168,-1:6,168:41,67,83,67,168:7,-1,84:33,89,-1,84:6,-1,84,-1,84:" +
"7,-1:7,326:5,91,326:2,237,326,276,326:2,259,326:8,-1:2,326,-1:6,326:3,-1:4," +
"326,-1:2,326:2,-1:36,94,-1:34,107,-1:49,109,-1:52,125,-1:52,152,-1:80,160,-" +
"1:12,326,95,326:20,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:8,98,32" +
"6:13,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:3,100,326:18,-1:2,326" +
",-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:5,102,326:16,-1:2,326,-1:6,326:3,-" +
"1:4,326,-1:2,326:2,-1:9,326:10,104,326:11,-1:2,326,-1:6,326:3,-1:4,326,-1:2" +
",326:2,-1:9,326:16,106,326:5,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,3" +
"26,108,326:20,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326,110,326:20,-" +
"1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:21,112,-1:2,326,-1:6,326:3," +
"-1:4,326,-1:2,326:2,-1:9,326:9,114,326:12,-1:2,326,-1:6,326:3,-1:4,326,-1:2" +
",326:2,-1:9,326:10,116,326:11,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9," +
"326:18,118,326:3,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,224,326:15,12" +
"0,326:5,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:8,122,326:13,-1:2," +
"326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:8,124,326:13,-1:2,326,-1:6,326:" +
"3,-1:4,326,-1:2,326:2,-1:9,326,126,326:20,-1:2,326,-1:6,326:3,-1:4,326,-1:2" +
",326:2,-1:9,326:8,128,326:13,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,3" +
"26:9,130,326:12,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:8,132,326:" +
"13,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:22,-1:2,134,-1:6,326:3," +
"-1:4,326,-1:2,326:2,-1:9,326:7,136,326:14,-1:2,326,-1:6,326:3,-1:4,326,-1:2" +
",326:2,-1:9,326:2,138,326:19,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,3" +
"26:8,140,326:13,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:7,142,326:" +
"14,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:10,144,326:11,-1:2,326," +
"-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:5,146,326:16,-1:2,326,-1:6,326:3,-1" +
":4,326,-1:2,326:2,-1:9,326:5,148,326:16,-1:2,326,-1:6,326:3,-1:4,326,-1:2,3" +
"26:2,-1:9,326:7,179,326:14,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:14,17" +
"7,-1:46,326,180,326:20,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:7,1" +
"81,326:14,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:9,182,326:12,-1:" +
"2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:17,183,326:4,-1:2,326,-1:6,32" +
"6:3,-1:4,326,-1:2,326:2,-1:9,326:4,184,326:17,-1:2,326,-1:6,326:3,-1:4,326," +
"-1:2,326:2,-1:9,326:19,185,326:2,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1" +
":9,326:4,186,326:17,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326,187,32" +
"6:20,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:13,188,326:8,-1:2,326" +
",-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:7,189,326:14,-1:2,326,-1:6,326:3,-" +
"1:4,326,-1:2,326:2,-1:9,326:7,190,326:14,-1:2,326,-1:6,326:3,-1:4,326,-1:2," +
"326:2,-1:9,326:7,191,326:14,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,32" +
"6:3,192,326:18,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:10,193,326:" +
"11,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:3,194,326:18,-1:2,326,-" +
"1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326,195,326:20,-1:2,326,-1:6,326:3,-1:4," +
"326,-1:2,326:2,-1:9,326:3,196,326:18,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:" +
"2,-1:9,326,197,326:20,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:10,1" +
"98,326:11,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:3,199,326:18,-1:" +
"2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:3,200,326:18,-1:2,326,-1:6,32" +
"6:3,-1:4,326,-1:2,326:2,-1:9,326:8,201,326:13,-1:2,326,-1:6,326:3,-1:4,326," +
"-1:2,326:2,-1:9,326:5,202,326:16,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1" +
":9,326:3,203,326:18,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:4,204," +
"326:17,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:18,208,326:3,-1:2,3" +
"26,-1:6,326:3,-1:4,326,-1:2,326:2,-1:10,206,-1:50,326:3,210,326:18,-1:2,326" +
",-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:7,262,326:2,211,326:11,-1:2,326,-1" +
":6,326:3,-1:4,326,-1:2,326:2,-1:9,326:10,212,326:11,-1:2,326,-1:6,326:3,-1:" +
"4,326,-1:2,326:2,-1:9,326:8,213,326:13,-1:2,326,-1:6,326:3,-1:4,326,-1:2,32" +
"6:2,-1:9,326:15,214,326:6,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:" +
"7,215,326:14,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:4,216,326:17," +
"-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:17,217,326:4,-1:2,326,-1:6" +
",326:3,-1:4,326,-1:2,326:2,-1:9,326:13,218,326:8,-1:2,326,-1:6,326:3,-1:4,3" +
"26,-1:2,326:2,-1:9,326:7,219,326:14,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2" +
",-1:9,326:3,220,326:18,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:7,2" +
"21,326:14,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:10,222,326:11,-1" +
":2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:10,223,326:11,-1:2,326,-1:6," +
"326:3,-1:4,326,-1:2,326:2,-1:9,326:3,225,326:18,-1:2,326,-1:6,326:3,-1:4,32" +
"6,-1:2,326:2,-1:9,326:5,226,326:16,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2," +
"-1:9,326:10,227,326:11,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:8,2" +
"28,326:13,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:3,229,326:18,-1:" +
"2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:4,230,326:17,-1:2,326,-1:6,32" +
"6:3,-1:4,326,-1:2,326:2,-1:9,326:3,231,326:18,-1:2,326,-1:6,326:3,-1:4,326," +
"-1:2,326:2,-1:9,326:7,236,326:14,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1" +
":13,233,-1:47,326:7,238,326:14,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9" +
",326:12,239,326:9,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:19,240,3" +
"26:2,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326,242,326:5,286,326:14," +
"-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:9,243,326:12,-1:2,326,-1:6" +
",326:3,-1:4,326,-1:2,326:2,-1:9,326:9,244,326:12,-1:2,326,-1:6,326:3,-1:4,3" +
"26,-1:2,326:2,-1:9,326,323,326:7,245,326:12,-1:2,326,-1:6,326:3,-1:4,326,-1" +
":2,326:2,-1:9,326:13,246,326:8,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9" +
",326:6,247,326:15,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:13,248,3" +
"26:8,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:6,249,326:15,-1:2,326" +
",-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:6,250,326:15,-1:2,326,-1:6,326:3,-" +
"1:4,326,-1:2,326:2,-1:9,326:3,251,326:18,-1:2,326,-1:6,326:3,-1:4,326,-1:2," +
"326:2,-1:9,326:4,252,326:17,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,32" +
"6:9,253,326:12,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:9,254,326:1" +
"2,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:9,260,326:7,280,326:4,-1" +
":2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:19,256,-1:41,326:13,261,326:8,-1:2" +
",326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326,283,326:20,-1:2,326,-1:6,326:3" +
",-1:4,326,-1:2,326:2,-1:9,326:14,284,326:7,-1:2,326,-1:6,326:3,-1:4,326,-1:" +
"2,326:2,-1:9,326:13,263,326:8,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9," +
"326:11,285,326:10,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:3,287,32" +
"6:18,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:5,264,326:16,-1:2,326" +
",-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:5,265,326:16,-1:2,326,-1:6,326:3,-" +
"1:4,326,-1:2,326:2,-1:9,326:2,324,326:19,-1:2,326,-1:6,326:3,-1:4,326,-1:2," +
"326:2,-1:9,326:17,328,326:4,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,32" +
"6:8,289,326:13,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,290,326:21,-1:2" +
",326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:5,327,326:16,-1:2,326,-1:6,326" +
":3,-1:4,326,-1:2,326:2,-1:9,326:4,291,326:17,-1:2,326,-1:6,326:3,-1:4,326,-" +
"1:2,326:2,-1:9,326:10,295,326:11,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1" +
":9,326:4,266,326:17,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:5,267," +
"326:16,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:13,296,326:8,-1:2,3" +
"26,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:7,297,326:14,-1:2,326,-1:6,326:3" +
",-1:4,326,-1:2,326:2,-1:9,326:8,298,326:13,-1:2,326,-1:6,326:3,-1:4,326,-1:" +
"2,326:2,-1:9,326:3,299,326:18,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9," +
"326:6,301,326:15,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,302,326:21,-1" +
":2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:4,303,326:17,-1:2,326,-1:6,3" +
"26:3,-1:4,326,-1:2,326:2,-1:9,326:10,304,326:11,-1:2,326,-1:6,326:3,-1:4,32" +
"6,-1:2,326:2,-1:9,326:9,268,326:12,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2," +
"-1:9,326:4,330,326:17,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:6,33" +
"6,326:15,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:8,305,326:13,-1:2" +
",326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:7,306,326:14,-1:2,326,-1:6,326" +
":3,-1:4,326,-1:2,326:2,-1:9,326:10,308,326:11,-1:2,326,-1:6,326:3,-1:4,326," +
"-1:2,326:2,-1:9,326:6,309,326:15,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1" +
":9,326:18,310,326:3,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:7,334," +
"326:14,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:13,312,326:8,-1:2,3" +
"26,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:3,313,326:18,-1:2,326,-1:6,326:3" +
",-1:4,326,-1:2,326:2,-1:9,326:18,314,326:3,-1:2,326,-1:6,326:3,-1:4,326,-1:" +
"2,326:2,-1:9,326:17,269,326:4,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9," +
"326:9,270,326:12,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:3,271,326" +
":18,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:10,316,326:11,-1:2,326" +
",-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:6,317,326:15,-1:2,326,-1:6,326:3,-" +
"1:4,326,-1:2,326:2,-1:9,326:13,318,326:8,-1:2,326,-1:6,326:3,-1:4,326,-1:2," +
"326:2,-1:9,326,335,326:20,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:" +
"8,320,326:13,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:10,272,326:11" +
",-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:2,277,326:2,278,326:16,-1" +
":2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:3,288,326:18,-1:2,326,-1:6,3" +
"26:3,-1:4,326,-1:2,326:2,-1:9,326:5,294,326:16,-1:2,326,-1:6,326:3,-1:4,326" +
",-1:2,326:2,-1:9,326:4,292,326:17,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-" +
"1:9,326:7,300,326:14,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:5,325" +
",326:16,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:4,293,326:17,-1:2," +
"326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:16,281,326:5,-1:2,326,-1:6,326:" +
"3,-1:4,326,-1:2,326:2,-1:9,326:5,307,326:16,-1:2,326,-1:6,326:3,-1:4,326,-1" +
":2,326:2,-1:9,326:10,282,326:11,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:" +
"9,326:5,311,326:16,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:2,322,3" +
"26:19,-1:2,326,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:5,315,326:16,-1:2,32" +
"6,-1:6,326:3,-1:4,326,-1:2,326:2,-1:9,326:5,319,326:16,-1:2,326,-1:6,326:3," +
"-1:4,326,-1:2,326:2,-1:9,326:4,332,326:17,-1:2,326,-1:6,326:3,-1:4,326,-1:2" +
",326:2,-1:2");

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
						{ return new Symbol(sym.STOP,yychar,yyline,
				    new String(yytext())); }
					case -3:
						break;
					case 3:
						{ return new Symbol(sym.COMMA,yychar,yyline,
				    new String(yytext())); }
					case -4:
						break;
					case 4:
						{ return new Symbol(sym.RBRA,yychar,yyline,
				    new String(yytext())); }
					case -5:
						break;
					case 5:
						{ return new Symbol(sym.RKET,yychar,yyline,
				    new String(yytext())); }
					case -6:
						break;
					case 6:
						{ return new Symbol(sym.SBRA,yychar,yyline,
				    new String(yytext())); }
					case -7:
						break;
					case 7:
						{ return new Symbol(sym.SKET,yychar,yyline,
				    new String(yytext())); }
					case -8:
						break;
					case 8:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -9:
						break;
					case 9:
						{ return new Symbol(sym.VAR,yychar,yyline, 
				    new String(yytext())); }
					case -10:
						break;
					case 10:
						{return new Symbol(sym.EQUALS,yychar,yyline,
                              new String(yytext())); }
					case -11:
						break;
					case 11:
						{return new Symbol(sym.GREATER,yychar,yyline,
                              new String(yytext())); }
					case -12:
						break;
					case 12:
						{return new Symbol(sym.PLUS,yychar,yyline,
                              new String(yytext())); }
					case -13:
						break;
					case 13:
						{return new Symbol(sym.MINUS,yychar,yyline,
                              new String(yytext())); }
					case -14:
						break;
					case 14:
						{return new Symbol(sym.MULT,yychar,yyline,
                              new String(yytext())); }
					case -15:
						break;
					case 15:
						{return new Symbol(sym.DIV,yychar,yyline,
                              new String(yytext())); }
					case -16:
						break;
					case 16:
						{return new Symbol(sym.LESS,yychar,yyline,
                              new String(yytext())); }
					case -17:
						break;
					case 17:
						{ System.err.println("Illegal character: "+yytext()); }
					case -18:
						break;
					case 18:
						{ return new Symbol(sym.NUMBER,yychar,yyline, 
				       new Integer(yytext())); }
					case -19:
						break;
					case 19:
						{ return new Symbol(sym.VAR,yychar,yyline, 
                                       new String(yytext())); }
					case -20:
						break;
					case 20:
						{ /* ignore white space. */ }
					case -21:
						break;
					case 21:
						{return new Symbol(sym.OR,yychar,yyline,
                              new String(yytext())); }
					case -22:
						break;
					case 22:
						{return new Symbol(sym.SE,yychar,yyline,
				    new String(yytext())); }
					case -23:
						break;
					case 23:
						{return new Symbol(sym.SS,yychar,yyline,
				    new String(yytext())); }
					case -24:
						break;
					case 24:
						{return new Symbol(sym.SC,yychar,yyline,
				    new String(yytext())); }
					case -25:
						break;
					case 25:
						{return new Symbol(sym.STATETRANS,yychar,yyline,
				    new String(yytext())); }
					case -26:
						break;
					case 26:
						{return new Symbol(sym.GREATEREQ,yychar,yyline,
                              new String(yytext())); }
					case -27:
						break;
					case 27:
						{yybegin(COMMENT);}
					case -28:
						break;
					case 28:
						{return new Symbol(sym.LESSEQ,yychar,yyline,
                              new String(yytext())); }
					case -29:
						break;
					case 29:
						{return new Symbol(sym.TIME,yychar,yyline,
                              new String(yytext())); }
					case -30:
						break;
					case 30:
						{ /* ignore comment. */ }
					case -31:
						break;
					case 31:
						{return new Symbol(sym.AND,yychar,yyline,
                              new String(yytext())); }
					case -32:
						break;
					case 32:
						{return new Symbol(sym.NOT,yychar,yyline,
                              new String(yytext())); }
					case -33:
						break;
					case 33:
						{ return new Symbol(sym.DOUBLE,yychar,yyline, 
				       new Double(yytext())); }
					case -34:
						break;
					case 34:
						{return new Symbol(sym.TEST,yychar,yyline,
                              new String(yytext())); }
					case -35:
						break;
					case 35:
						{return new Symbol(sym.HTNGOAL,yychar,yyline,
                                  new String(yytext())); }
					case -36:
						break;
					case 36:
						{return new Symbol(sym.EVENT,yychar,yyline,
                              new String(yytext())); }
					case -37:
						break;
					case 37:
						{return new Symbol(sym.SORTS,yychar,yyline,
				       new String(yytext())); }
					case -38:
						break;
					case 38:
						{return new Symbol(sym.VALUE,yychar,yyline,
                              new String(yytext())); }
					case -39:
						break;
					case 39:
						{return new Symbol(sym.OPTION,yychar,yyline,
                              new String(yytext())); }
					case -40:
						break;
					case 40:
						{return new Symbol(sym.METHOD,yychar,yyline,
                              new String(yytext())); }
					case -41:
						break;
					case 41:
						{return new Symbol(sym.ASSIGN,yychar,yyline,
                              new String(yytext())); }
					case -42:
						break;
					case 42:
						{return new Symbol(sym.BEFORE,yychar,yyline,
                              new String(yytext())); }
					case -43:
						break;
					case 43:
						{yybegin(DESCRIPTION);}
					case -44:
						break;
					case 44:
						{return new Symbol(sym.OBJECTS,yychar,yyline,
					 new String(yytext())); }
					case -45:
						break;
					case 45:
						{return new Symbol(sym.ACHIEVE,yychar,yyline,
                              new String(yytext())); }
					case -46:
						break;
					case 46:
						{return new Symbol(sym.SCALEUP,yychar,yyline,
                              new String(yytext())); }
					case -47:
						break;
					case 47:
						{return new Symbol(sym.PROCESS,yychar,yyline,
                              new String(yytext())); }
					case -48:
						break;
					case 48:
						{return new Symbol(sym.DECREASE,yychar,yyline,
                              new String(yytext())); }
					case -49:
						break;
					case 49:
						{return new Symbol(sym.OPERATOR,yychar,yyline,
                              new String(yytext())); }
					case -50:
						break;
					case 50:
						{return new Symbol(sym.INCREASE,yychar,yyline,
                              new String(yytext())); }
					case -51:
						break;
					case 51:
						{return new Symbol(sym.FUNCTORS,yychar,yyline
                                	,new String(yytext())); }
					case -52:
						break;
					case 52:
						{return new Symbol(sym.HTNTASK,yychar,yyline,
                                  new String(yytext())); }
					case -53:
						break;
					case 53:
						{return new Symbol(sym.SCALEDOWN,yychar,yyline,
                              new String(yytext())); }
					case -54:
						break;
					case 54:
						{return new Symbol(sym.PREDICATES,yychar,yyline
                                ,new String(yytext())); }
					case -55:
						break;
					case 55:
						{return new Symbol(sym.DOMNAME,yychar,yyline,
					     new String(yytext())); }
					case -56:
						break;
					case 56:
						{return new Symbol(sym.TASK,yychar,yyline,
                                  new String(yytext())); }
					case -57:
						break;
					case 57:
						{return new Symbol(sym.SSCLASSES,yychar,yyline,
                                      new String(yytext())); }
					case -58:
						break;
					case 58:
						{return new Symbol(sym.ATOMINVAR,yychar,yyline,
                                       new String(yytext())); }
					case -59:
						break;
					case 59:
						{return new Symbol(sym.IMPINVAR,yychar,yyline,
                                  new String(yytext())); }
					case -60:
						break;
					case 60:
						{return 
					   new Symbol(sym.INCONCONST,
						      yychar,yyline,
						      new String(yytext())); }
					case -61:
						break;
					case 61:
						{ /* otherwise ignore every thing */}
					case -62:
						break;
					case 62:
						{yybegin(YYINITIAL);}
					case -63:
						break;
					case 63:
						{return new Symbol(sym.AUTHOR,yychar,yyline, 
				    new String(yytext()));  }
					case -64:
						break;
					case 64:
						{return new Symbol(sym.INSTITUTION,yychar,yyline, 
				    new String(yytext()));  }
					case -65:
						break;
					case 65:
						{yybegin(DESCRIPTION);}
					case -66:
						break;
					case 66:
						{return new Symbol(sym.DATEC,yychar,yyline, 
				    new String(yytext()));  }
					case -67:
						break;
					case 67:
						{return new Symbol(sym.DATEL,yychar,yyline, 
				    new String(yytext()));  }
					case -68:
						break;
					case 68:
						{return new Symbol(sym.DESCDET,yychar,yyline, 
				    new String(yytext()));}
					case -69:
						break;
					case 69:
						{/* Ignore blank comment line */}
					case -70:
						break;
					case 70:
						{System.err.println("Unrecognised character" +
                                    new String(yytext()));
                    yybegin(YYINITIAL);
                   return new Symbol(sym.DESCEND,yychar,yyline, 
				    new String(yytext()));}
					case -71:
						break;
					case 71:
						{yybegin(YYINITIAL);
                   return new Symbol(sym.DESCEND,yychar,yyline, 
				    new String(yytext()));}
					case -72:
						break;
					case 73:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -73:
						break;
					case 74:
						{ System.err.println("Illegal character: "+yytext()); }
					case -74:
						break;
					case 75:
						{ /* ignore white space. */ }
					case -75:
						break;
					case 76:
						{ /* ignore comment. */ }
					case -76:
						break;
					case 77:
						{yybegin(DESCRIPTION);}
					case -77:
						break;
					case 78:
						{ /* otherwise ignore every thing */}
					case -78:
						break;
					case 79:
						{return new Symbol(sym.AUTHOR,yychar,yyline, 
				    new String(yytext()));  }
					case -79:
						break;
					case 80:
						{return new Symbol(sym.INSTITUTION,yychar,yyline, 
				    new String(yytext()));  }
					case -80:
						break;
					case 81:
						{yybegin(DESCRIPTION);}
					case -81:
						break;
					case 82:
						{return new Symbol(sym.DATEC,yychar,yyline, 
				    new String(yytext()));  }
					case -82:
						break;
					case 83:
						{return new Symbol(sym.DATEL,yychar,yyline, 
				    new String(yytext()));  }
					case -83:
						break;
					case 84:
						{return new Symbol(sym.DESCDET,yychar,yyline, 
				    new String(yytext()));}
					case -84:
						break;
					case 86:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -85:
						break;
					case 87:
						{ System.err.println("Illegal character: "+yytext()); }
					case -86:
						break;
					case 88:
						{ /* otherwise ignore every thing */}
					case -87:
						break;
					case 89:
						{return new Symbol(sym.DESCDET,yychar,yyline, 
				    new String(yytext()));}
					case -88:
						break;
					case 91:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -89:
						break;
					case 92:
						{ /* otherwise ignore every thing */}
					case -90:
						break;
					case 93:
						{return new Symbol(sym.DESCDET,yychar,yyline, 
				    new String(yytext()));}
					case -91:
						break;
					case 95:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -92:
						break;
					case 96:
						{ /* otherwise ignore every thing */}
					case -93:
						break;
					case 98:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -94:
						break;
					case 100:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -95:
						break;
					case 102:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -96:
						break;
					case 104:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -97:
						break;
					case 106:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -98:
						break;
					case 108:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -99:
						break;
					case 110:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -100:
						break;
					case 112:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -101:
						break;
					case 114:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -102:
						break;
					case 116:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -103:
						break;
					case 118:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -104:
						break;
					case 120:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -105:
						break;
					case 122:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -106:
						break;
					case 124:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -107:
						break;
					case 126:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -108:
						break;
					case 128:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -109:
						break;
					case 130:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -110:
						break;
					case 132:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -111:
						break;
					case 134:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -112:
						break;
					case 136:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -113:
						break;
					case 138:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -114:
						break;
					case 140:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -115:
						break;
					case 142:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -116:
						break;
					case 144:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -117:
						break;
					case 146:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -118:
						break;
					case 148:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -119:
						break;
					case 171:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -120:
						break;
					case 178:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -121:
						break;
					case 179:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -122:
						break;
					case 180:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -123:
						break;
					case 181:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -124:
						break;
					case 182:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -125:
						break;
					case 183:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -126:
						break;
					case 184:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -127:
						break;
					case 185:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -128:
						break;
					case 186:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -129:
						break;
					case 187:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -130:
						break;
					case 188:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -131:
						break;
					case 189:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -132:
						break;
					case 190:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -133:
						break;
					case 191:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -134:
						break;
					case 192:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -135:
						break;
					case 193:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -136:
						break;
					case 194:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -137:
						break;
					case 195:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -138:
						break;
					case 196:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -139:
						break;
					case 197:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -140:
						break;
					case 198:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -141:
						break;
					case 199:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -142:
						break;
					case 200:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -143:
						break;
					case 201:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -144:
						break;
					case 202:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -145:
						break;
					case 203:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -146:
						break;
					case 204:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -147:
						break;
					case 205:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -148:
						break;
					case 207:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -149:
						break;
					case 208:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -150:
						break;
					case 209:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -151:
						break;
					case 210:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -152:
						break;
					case 211:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -153:
						break;
					case 212:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -154:
						break;
					case 213:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -155:
						break;
					case 214:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -156:
						break;
					case 215:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -157:
						break;
					case 216:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -158:
						break;
					case 217:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -159:
						break;
					case 218:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -160:
						break;
					case 219:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -161:
						break;
					case 220:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -162:
						break;
					case 221:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -163:
						break;
					case 222:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -164:
						break;
					case 223:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -165:
						break;
					case 224:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -166:
						break;
					case 225:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -167:
						break;
					case 226:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -168:
						break;
					case 227:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -169:
						break;
					case 228:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -170:
						break;
					case 229:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -171:
						break;
					case 230:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -172:
						break;
					case 231:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -173:
						break;
					case 232:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -174:
						break;
					case 234:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -175:
						break;
					case 235:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -176:
						break;
					case 236:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -177:
						break;
					case 237:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -178:
						break;
					case 238:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -179:
						break;
					case 239:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -180:
						break;
					case 240:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -181:
						break;
					case 241:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -182:
						break;
					case 242:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -183:
						break;
					case 243:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -184:
						break;
					case 244:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -185:
						break;
					case 245:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -186:
						break;
					case 246:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -187:
						break;
					case 247:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -188:
						break;
					case 248:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -189:
						break;
					case 249:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -190:
						break;
					case 250:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -191:
						break;
					case 251:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -192:
						break;
					case 252:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -193:
						break;
					case 253:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -194:
						break;
					case 254:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -195:
						break;
					case 255:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -196:
						break;
					case 257:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -197:
						break;
					case 258:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -198:
						break;
					case 259:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -199:
						break;
					case 260:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -200:
						break;
					case 261:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -201:
						break;
					case 262:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -202:
						break;
					case 263:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -203:
						break;
					case 264:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -204:
						break;
					case 265:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -205:
						break;
					case 266:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -206:
						break;
					case 267:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -207:
						break;
					case 268:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -208:
						break;
					case 269:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -209:
						break;
					case 270:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -210:
						break;
					case 271:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -211:
						break;
					case 272:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -212:
						break;
					case 273:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -213:
						break;
					case 275:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -214:
						break;
					case 276:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -215:
						break;
					case 277:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -216:
						break;
					case 278:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -217:
						break;
					case 279:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -218:
						break;
					case 280:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -219:
						break;
					case 281:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -220:
						break;
					case 282:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -221:
						break;
					case 283:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -222:
						break;
					case 284:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -223:
						break;
					case 285:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -224:
						break;
					case 286:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -225:
						break;
					case 287:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -226:
						break;
					case 288:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -227:
						break;
					case 289:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -228:
						break;
					case 290:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -229:
						break;
					case 291:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -230:
						break;
					case 292:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -231:
						break;
					case 293:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -232:
						break;
					case 294:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -233:
						break;
					case 295:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -234:
						break;
					case 296:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -235:
						break;
					case 297:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -236:
						break;
					case 298:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -237:
						break;
					case 299:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -238:
						break;
					case 300:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -239:
						break;
					case 301:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -240:
						break;
					case 302:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -241:
						break;
					case 303:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -242:
						break;
					case 304:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -243:
						break;
					case 305:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -244:
						break;
					case 306:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -245:
						break;
					case 307:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -246:
						break;
					case 308:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -247:
						break;
					case 309:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -248:
						break;
					case 310:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -249:
						break;
					case 311:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -250:
						break;
					case 312:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -251:
						break;
					case 313:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -252:
						break;
					case 314:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -253:
						break;
					case 315:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -254:
						break;
					case 316:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -255:
						break;
					case 317:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -256:
						break;
					case 318:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -257:
						break;
					case 319:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -258:
						break;
					case 320:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -259:
						break;
					case 321:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -260:
						break;
					case 322:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -261:
						break;
					case 323:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -262:
						break;
					case 324:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -263:
						break;
					case 325:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -264:
						break;
					case 326:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -265:
						break;
					case 327:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -266:
						break;
					case 328:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -267:
						break;
					case 329:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -268:
						break;
					case 330:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -269:
						break;
					case 331:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -270:
						break;
					case 332:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -271:
						break;
					case 333:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -272:
						break;
					case 334:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -273:
						break;
					case 335:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -274:
						break;
					case 336:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -275:
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
