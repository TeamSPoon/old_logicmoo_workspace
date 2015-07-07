/*
 * GIPO COPYRIGHT NOTICE, LICENSE AND DISCLAIMER.
 *
 * Copyright 2001 - 2003 by R.M.Simpson W.Zhao T.L.McCLuskey D Liu D. Kitchin
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

package jplan.pddl;
import java_cup.runtime.Symbol;


public class pddlYylex implements java_cup.runtime.Scanner {
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

	public pddlYylex (java.io.Reader reader) {
		this ();
		if (null == reader) {
			throw (new Error("Error: Bad input stream initializer."));
		}
		yy_reader = new java.io.BufferedReader(reader);
	}

	public pddlYylex (java.io.InputStream instream) {
		this ();
		if (null == instream) {
			throw (new Error("Error: Bad input stream initializer."));
		}
		yy_reader = new java.io.BufferedReader(new java.io.InputStreamReader(instream));
	}

	private pddlYylex () {
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
		/* 11 */ YY_END,
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
		/* 30 */ YY_NO_ANCHOR,
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
		/* 71 */ YY_NO_ANCHOR,
		/* 72 */ YY_NOT_ACCEPT,
		/* 73 */ YY_NO_ANCHOR,
		/* 74 */ YY_NO_ANCHOR,
		/* 75 */ YY_END,
		/* 76 */ YY_NOT_ACCEPT,
		/* 77 */ YY_NO_ANCHOR,
		/* 78 */ YY_NO_ANCHOR,
		/* 79 */ YY_NOT_ACCEPT,
		/* 80 */ YY_NO_ANCHOR,
		/* 81 */ YY_NOT_ACCEPT,
		/* 82 */ YY_NO_ANCHOR,
		/* 83 */ YY_NOT_ACCEPT,
		/* 84 */ YY_NO_ANCHOR,
		/* 85 */ YY_NOT_ACCEPT,
		/* 86 */ YY_NO_ANCHOR,
		/* 87 */ YY_NOT_ACCEPT,
		/* 88 */ YY_NO_ANCHOR,
		/* 89 */ YY_NOT_ACCEPT,
		/* 90 */ YY_NO_ANCHOR,
		/* 91 */ YY_NOT_ACCEPT,
		/* 92 */ YY_NO_ANCHOR,
		/* 93 */ YY_NOT_ACCEPT,
		/* 94 */ YY_NO_ANCHOR,
		/* 95 */ YY_NOT_ACCEPT,
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
		/* 108 */ YY_NOT_ACCEPT,
		/* 109 */ YY_NOT_ACCEPT,
		/* 110 */ YY_NOT_ACCEPT,
		/* 111 */ YY_NOT_ACCEPT,
		/* 112 */ YY_NOT_ACCEPT,
		/* 113 */ YY_NOT_ACCEPT,
		/* 114 */ YY_NOT_ACCEPT,
		/* 115 */ YY_NOT_ACCEPT,
		/* 116 */ YY_NOT_ACCEPT,
		/* 117 */ YY_NOT_ACCEPT,
		/* 118 */ YY_NOT_ACCEPT,
		/* 119 */ YY_NOT_ACCEPT,
		/* 120 */ YY_NOT_ACCEPT,
		/* 121 */ YY_NOT_ACCEPT,
		/* 122 */ YY_NOT_ACCEPT,
		/* 123 */ YY_NOT_ACCEPT,
		/* 124 */ YY_NOT_ACCEPT,
		/* 125 */ YY_NOT_ACCEPT,
		/* 126 */ YY_NOT_ACCEPT,
		/* 127 */ YY_NOT_ACCEPT,
		/* 128 */ YY_NOT_ACCEPT,
		/* 129 */ YY_NOT_ACCEPT,
		/* 130 */ YY_NOT_ACCEPT,
		/* 131 */ YY_NOT_ACCEPT,
		/* 132 */ YY_NOT_ACCEPT,
		/* 133 */ YY_NOT_ACCEPT,
		/* 134 */ YY_NOT_ACCEPT,
		/* 135 */ YY_NOT_ACCEPT,
		/* 136 */ YY_NOT_ACCEPT,
		/* 137 */ YY_NOT_ACCEPT,
		/* 138 */ YY_NOT_ACCEPT,
		/* 139 */ YY_NOT_ACCEPT,
		/* 140 */ YY_NOT_ACCEPT,
		/* 141 */ YY_NOT_ACCEPT,
		/* 142 */ YY_NOT_ACCEPT,
		/* 143 */ YY_NOT_ACCEPT,
		/* 144 */ YY_NOT_ACCEPT,
		/* 145 */ YY_NOT_ACCEPT,
		/* 146 */ YY_NOT_ACCEPT,
		/* 147 */ YY_NOT_ACCEPT,
		/* 148 */ YY_NOT_ACCEPT,
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
		/* 171 */ YY_NOT_ACCEPT,
		/* 172 */ YY_NOT_ACCEPT,
		/* 173 */ YY_NOT_ACCEPT,
		/* 174 */ YY_NOT_ACCEPT,
		/* 175 */ YY_NOT_ACCEPT,
		/* 176 */ YY_NOT_ACCEPT,
		/* 177 */ YY_NOT_ACCEPT,
		/* 178 */ YY_NOT_ACCEPT,
		/* 179 */ YY_NOT_ACCEPT,
		/* 180 */ YY_NOT_ACCEPT,
		/* 181 */ YY_NOT_ACCEPT,
		/* 182 */ YY_NOT_ACCEPT,
		/* 183 */ YY_NOT_ACCEPT,
		/* 184 */ YY_NOT_ACCEPT,
		/* 185 */ YY_NOT_ACCEPT,
		/* 186 */ YY_NOT_ACCEPT,
		/* 187 */ YY_NOT_ACCEPT,
		/* 188 */ YY_NOT_ACCEPT,
		/* 189 */ YY_NOT_ACCEPT,
		/* 190 */ YY_NOT_ACCEPT,
		/* 191 */ YY_NOT_ACCEPT,
		/* 192 */ YY_NOT_ACCEPT,
		/* 193 */ YY_NOT_ACCEPT,
		/* 194 */ YY_NOT_ACCEPT,
		/* 195 */ YY_NOT_ACCEPT,
		/* 196 */ YY_NOT_ACCEPT,
		/* 197 */ YY_NOT_ACCEPT,
		/* 198 */ YY_NOT_ACCEPT,
		/* 199 */ YY_NOT_ACCEPT,
		/* 200 */ YY_NOT_ACCEPT,
		/* 201 */ YY_NOT_ACCEPT,
		/* 202 */ YY_NOT_ACCEPT,
		/* 203 */ YY_NOT_ACCEPT,
		/* 204 */ YY_NOT_ACCEPT,
		/* 205 */ YY_NOT_ACCEPT,
		/* 206 */ YY_NOT_ACCEPT,
		/* 207 */ YY_NOT_ACCEPT,
		/* 208 */ YY_NOT_ACCEPT,
		/* 209 */ YY_NOT_ACCEPT,
		/* 210 */ YY_NOT_ACCEPT,
		/* 211 */ YY_NOT_ACCEPT,
		/* 212 */ YY_NOT_ACCEPT,
		/* 213 */ YY_NOT_ACCEPT,
		/* 214 */ YY_NOT_ACCEPT,
		/* 215 */ YY_NOT_ACCEPT,
		/* 216 */ YY_NOT_ACCEPT,
		/* 217 */ YY_NOT_ACCEPT,
		/* 218 */ YY_NOT_ACCEPT,
		/* 219 */ YY_NOT_ACCEPT,
		/* 220 */ YY_NOT_ACCEPT,
		/* 221 */ YY_NOT_ACCEPT,
		/* 222 */ YY_NOT_ACCEPT,
		/* 223 */ YY_NOT_ACCEPT,
		/* 224 */ YY_NOT_ACCEPT,
		/* 225 */ YY_NOT_ACCEPT,
		/* 226 */ YY_NOT_ACCEPT,
		/* 227 */ YY_NOT_ACCEPT,
		/* 228 */ YY_NOT_ACCEPT,
		/* 229 */ YY_NOT_ACCEPT,
		/* 230 */ YY_NOT_ACCEPT,
		/* 231 */ YY_NOT_ACCEPT,
		/* 232 */ YY_NOT_ACCEPT,
		/* 233 */ YY_NOT_ACCEPT,
		/* 234 */ YY_NOT_ACCEPT,
		/* 235 */ YY_NOT_ACCEPT,
		/* 236 */ YY_NOT_ACCEPT,
		/* 237 */ YY_NOT_ACCEPT,
		/* 238 */ YY_NOT_ACCEPT,
		/* 239 */ YY_NOT_ACCEPT,
		/* 240 */ YY_NOT_ACCEPT,
		/* 241 */ YY_NOT_ACCEPT,
		/* 242 */ YY_NOT_ACCEPT,
		/* 243 */ YY_NOT_ACCEPT,
		/* 244 */ YY_NOT_ACCEPT,
		/* 245 */ YY_NOT_ACCEPT,
		/* 246 */ YY_NOT_ACCEPT,
		/* 247 */ YY_NOT_ACCEPT,
		/* 248 */ YY_NOT_ACCEPT,
		/* 249 */ YY_NOT_ACCEPT,
		/* 250 */ YY_NOT_ACCEPT,
		/* 251 */ YY_NOT_ACCEPT,
		/* 252 */ YY_NOT_ACCEPT,
		/* 253 */ YY_NOT_ACCEPT,
		/* 254 */ YY_NOT_ACCEPT,
		/* 255 */ YY_NOT_ACCEPT,
		/* 256 */ YY_NOT_ACCEPT,
		/* 257 */ YY_NOT_ACCEPT,
		/* 258 */ YY_NOT_ACCEPT,
		/* 259 */ YY_NOT_ACCEPT,
		/* 260 */ YY_NOT_ACCEPT,
		/* 261 */ YY_NOT_ACCEPT,
		/* 262 */ YY_NOT_ACCEPT,
		/* 263 */ YY_NOT_ACCEPT,
		/* 264 */ YY_NOT_ACCEPT,
		/* 265 */ YY_NOT_ACCEPT,
		/* 266 */ YY_NOT_ACCEPT,
		/* 267 */ YY_NOT_ACCEPT,
		/* 268 */ YY_NOT_ACCEPT,
		/* 269 */ YY_NOT_ACCEPT,
		/* 270 */ YY_NOT_ACCEPT,
		/* 271 */ YY_NOT_ACCEPT,
		/* 272 */ YY_NOT_ACCEPT,
		/* 273 */ YY_NOT_ACCEPT,
		/* 274 */ YY_NO_ANCHOR,
		/* 275 */ YY_NOT_ACCEPT,
		/* 276 */ YY_NOT_ACCEPT,
		/* 277 */ YY_NOT_ACCEPT,
		/* 278 */ YY_NOT_ACCEPT,
		/* 279 */ YY_NOT_ACCEPT,
		/* 280 */ YY_NOT_ACCEPT,
		/* 281 */ YY_NOT_ACCEPT,
		/* 282 */ YY_NOT_ACCEPT,
		/* 283 */ YY_NOT_ACCEPT,
		/* 284 */ YY_NOT_ACCEPT,
		/* 285 */ YY_NOT_ACCEPT,
		/* 286 */ YY_NOT_ACCEPT,
		/* 287 */ YY_NOT_ACCEPT,
		/* 288 */ YY_NOT_ACCEPT,
		/* 289 */ YY_NOT_ACCEPT,
		/* 290 */ YY_NOT_ACCEPT,
		/* 291 */ YY_NOT_ACCEPT,
		/* 292 */ YY_NOT_ACCEPT,
		/* 293 */ YY_NOT_ACCEPT,
		/* 294 */ YY_NOT_ACCEPT,
		/* 295 */ YY_NOT_ACCEPT,
		/* 296 */ YY_NOT_ACCEPT,
		/* 297 */ YY_NOT_ACCEPT,
		/* 298 */ YY_NOT_ACCEPT,
		/* 299 */ YY_NOT_ACCEPT,
		/* 300 */ YY_NOT_ACCEPT,
		/* 301 */ YY_NOT_ACCEPT,
		/* 302 */ YY_NOT_ACCEPT,
		/* 303 */ YY_NOT_ACCEPT,
		/* 304 */ YY_NOT_ACCEPT,
		/* 305 */ YY_NOT_ACCEPT,
		/* 306 */ YY_NOT_ACCEPT,
		/* 307 */ YY_NOT_ACCEPT,
		/* 308 */ YY_NOT_ACCEPT,
		/* 309 */ YY_NOT_ACCEPT,
		/* 310 */ YY_NOT_ACCEPT,
		/* 311 */ YY_NOT_ACCEPT,
		/* 312 */ YY_NOT_ACCEPT,
		/* 313 */ YY_NOT_ACCEPT,
		/* 314 */ YY_NOT_ACCEPT,
		/* 315 */ YY_NOT_ACCEPT,
		/* 316 */ YY_NOT_ACCEPT,
		/* 317 */ YY_NOT_ACCEPT,
		/* 318 */ YY_NOT_ACCEPT,
		/* 319 */ YY_NOT_ACCEPT,
		/* 320 */ YY_NOT_ACCEPT,
		/* 321 */ YY_NOT_ACCEPT,
		/* 322 */ YY_NOT_ACCEPT,
		/* 323 */ YY_NOT_ACCEPT,
		/* 324 */ YY_NOT_ACCEPT,
		/* 325 */ YY_NOT_ACCEPT,
		/* 326 */ YY_NOT_ACCEPT,
		/* 327 */ YY_NO_ANCHOR,
		/* 328 */ YY_NOT_ACCEPT,
		/* 329 */ YY_NOT_ACCEPT,
		/* 330 */ YY_NOT_ACCEPT,
		/* 331 */ YY_NOT_ACCEPT,
		/* 332 */ YY_NOT_ACCEPT,
		/* 333 */ YY_NOT_ACCEPT,
		/* 334 */ YY_NOT_ACCEPT,
		/* 335 */ YY_NOT_ACCEPT,
		/* 336 */ YY_NOT_ACCEPT,
		/* 337 */ YY_NOT_ACCEPT,
		/* 338 */ YY_NOT_ACCEPT,
		/* 339 */ YY_NOT_ACCEPT,
		/* 340 */ YY_NOT_ACCEPT,
		/* 341 */ YY_NOT_ACCEPT,
		/* 342 */ YY_NOT_ACCEPT,
		/* 343 */ YY_NOT_ACCEPT,
		/* 344 */ YY_NOT_ACCEPT,
		/* 345 */ YY_NOT_ACCEPT,
		/* 346 */ YY_NO_ANCHOR,
		/* 347 */ YY_NOT_ACCEPT,
		/* 348 */ YY_NOT_ACCEPT,
		/* 349 */ YY_NOT_ACCEPT,
		/* 350 */ YY_NOT_ACCEPT,
		/* 351 */ YY_NOT_ACCEPT,
		/* 352 */ YY_NOT_ACCEPT,
		/* 353 */ YY_NO_ANCHOR,
		/* 354 */ YY_NO_ANCHOR,
		/* 355 */ YY_NO_ANCHOR,
		/* 356 */ YY_NO_ANCHOR,
		/* 357 */ YY_NO_ANCHOR,
		/* 358 */ YY_NO_ANCHOR,
		/* 359 */ YY_NO_ANCHOR,
		/* 360 */ YY_NO_ANCHOR,
		/* 361 */ YY_NO_ANCHOR,
		/* 362 */ YY_NO_ANCHOR,
		/* 363 */ YY_NO_ANCHOR,
		/* 364 */ YY_NO_ANCHOR,
		/* 365 */ YY_NO_ANCHOR,
		/* 366 */ YY_NO_ANCHOR,
		/* 367 */ YY_NOT_ACCEPT,
		/* 368 */ YY_NOT_ACCEPT,
		/* 369 */ YY_NOT_ACCEPT,
		/* 370 */ YY_NO_ANCHOR,
		/* 371 */ YY_NO_ANCHOR,
		/* 372 */ YY_NO_ANCHOR,
		/* 373 */ YY_NO_ANCHOR,
		/* 374 */ YY_NO_ANCHOR,
		/* 375 */ YY_NO_ANCHOR,
		/* 376 */ YY_NO_ANCHOR,
		/* 377 */ YY_NO_ANCHOR,
		/* 378 */ YY_NO_ANCHOR,
		/* 379 */ YY_NO_ANCHOR,
		/* 380 */ YY_NO_ANCHOR,
		/* 381 */ YY_NO_ANCHOR,
		/* 382 */ YY_NOT_ACCEPT,
		/* 383 */ YY_NO_ANCHOR,
		/* 384 */ YY_NO_ANCHOR,
		/* 385 */ YY_NO_ANCHOR,
		/* 386 */ YY_NO_ANCHOR,
		/* 387 */ YY_NO_ANCHOR,
		/* 388 */ YY_NO_ANCHOR,
		/* 389 */ YY_NO_ANCHOR,
		/* 390 */ YY_NO_ANCHOR,
		/* 391 */ YY_NO_ANCHOR,
		/* 392 */ YY_NO_ANCHOR,
		/* 393 */ YY_NO_ANCHOR,
		/* 394 */ YY_NO_ANCHOR,
		/* 395 */ YY_NO_ANCHOR,
		/* 396 */ YY_NO_ANCHOR,
		/* 397 */ YY_NO_ANCHOR,
		/* 398 */ YY_NO_ANCHOR,
		/* 399 */ YY_NO_ANCHOR,
		/* 400 */ YY_NO_ANCHOR,
		/* 401 */ YY_NO_ANCHOR,
		/* 402 */ YY_NO_ANCHOR,
		/* 403 */ YY_NO_ANCHOR,
		/* 404 */ YY_NO_ANCHOR,
		/* 405 */ YY_NO_ANCHOR,
		/* 406 */ YY_NO_ANCHOR,
		/* 407 */ YY_NO_ANCHOR
	};
	private int yy_cmap[] = unpackFromString(1,130,
"58:9,59,60,58,59,56,58:18,59,58:7,1,2,58:3,28,58:2,55:10,3,57,58,52,58,53,5" +
"8,19,50,44,46,16,48,49,40,21,51,54,20,42,41,45,43,17,34,35,22,18,33,39,54,2" +
"3,54,58:4,47,58,13,31,25,27,5,29,30,37,8,36,54,14,9,10,26,24,6,4,12,11,7,32" +
",38,54,15,54,58:5,0,61")[0];

	private int yy_rmap[] = unpackFromString(1,408,
"0,1:3,2,3,1:3,4,1,5,3:6,1:8,3:6,1:14,3:2,1:8,3:2,1:14,6,1,7,1,8,9,10,11,12," +
"13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37," +
"38,39,40,41,42,9,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,6" +
"2,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,8" +
"7,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,1" +
"09,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127," +
"128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146" +
",147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,16" +
"5,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,1" +
"84,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202," +
"203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221" +
",222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,24" +
"0,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,256,257,258,2" +
"59,260,261,262,263,264,265,266,267,268,269,270,271,272,273,274,275,276,277," +
"278,279,280,281,282,283,284,285,286,287,288,289,290,291,292,293,294,295,296" +
",297,298,299,300,301,302,303,304,305,306,307,308,309,310,311,312,313,314,31" +
"5,316,317,318,319,320,321,322,323,324,325,326,327,328,329,330,331,332,333,3" +
"34,335,336,337,338")[0];

	private int yy_nxt[][] = unpackFromString(339,62,
"1,2,3,4,5,392,5:4,274,5,406,327,5:2,393,5:2,346,5:4,400,5:2,394,6,5:6,407,5" +
":2,366,370,5,353,5,401,5:2,395,73,5:4,7,8,5,9,10,77,73,10:2,1,-1:66,72,76,-" +
"1:2,79,-1:2,81,83,85,275,-1,87,-1:2,89,91,93,95,-1,97,99,101,278,-1:2,328,-" +
"1,103,105,276,107,-1:7,108,109,110,280,-1:2,329,-1:16,5:48,-1:2,5:2,-1:61,9" +
",-1:66,75,-1:6,112,-1:60,5:7,12,5:40,-1:2,5:2,-1:12,113,-1:22,114,-1:33,111" +
":55,11,111:3,75:2,-1:4,5:23,13,5:24,-1:2,5:2,-1:16,115,-1:55,5:42,14,5:5,-1" +
":2,5:2,-1:14,116,-1:6,117,-1:50,5:18,15,5:29,-1:2,5:2,-1:11,118,-1:2,119,-1" +
":2,367,-1:54,5:6,16,5:41,-1:2,5:2,-1:31,284,-1:40,5:37,17,5:10,-1:2,5:2,-1:" +
"23,121,-1:30,122,-1:17,26,5:47,-1:2,5:2,-1:50,123,-1:21,5:30,27,5:17,-1:2,5" +
":2,-1:22,277,-1:49,5,28,5:46,-1:2,5:2,-1:47,124,-1:24,5:6,29,5:41,-1:2,5:2," +
"-1:27,125,-1,126,-1:42,5:12,30,5:35,-1:2,5:2,-1:10,127,-1:8,283,-1:52,5:37," +
"31,5:10,-1:2,5:2,-1:32,285,-1:39,5:5,46,5:42,-1:2,5:2,-1:37,128,-1:34,5:38," +
"47,5:9,-1:2,5:2,-1:19,330,-1:52,5:6,56,5:41,-1:2,5:2,-1:25,129,-1:46,5:37,5" +
"7,5:10,-1:2,5:2,-1:22,290,-1:4,286,332,-1:58,347,-1:14,131,-1:72,132,-1:66," +
"133,-1:17,281,-1:62,135,-1:83,289,-1:40,136,-1:62,331,-1:76,137,-1:41,138,-" +
"1:68,139,-1:60,140,-1:69,292,-1:91,291,-1:35,287,-1:60,142,-1:82,333,-1:62," +
"143,-1:23,144,-1:92,146,-1:59,149,-1:44,150,-1:60,152,-1:86,153,-1:71,154,-" +
"1:29,155,-1:55,156,-1:59,18,-1:55,158,-1:2,159,-1:61,160,-1:60,299,-1:84,16" +
"3,-1:80,167,-1:34,19,-1:55,168,-1:4,169,-1:65,300,-1,170,-1:46,301,-1:14,30" +
"6,-1:39,298,-1:70,20,-1:59,21,-1:84,22,-1:44,337,-1:64,172,-1:84,304,-1,175" +
",-1:50,305,-1:10,308,-1:31,303,-1:65,23,-1:55,350,-1:72,177,-1:48,24,-1:59," +
"179,-1:64,180,-1:72,181,-1:63,182,-1:46,183,-1:70,351,-1:85,184,-1:62,185,-" +
"1:38,186,-1:74,25,-1:67,188,-1:28,190,-1:62,309,-1:4,191,-1:66,196,-1:85,19" +
"8,-1:38,199,-1:21,310,-1:40,201,-1:44,341,-1:68,32,-1:55,204,-1:86,33,-1:45" +
",34,-1:59,35,-1:59,36,-1:88,37,-1:46,38,-1:80,39,-1:60,40,-1:37,206,-1:94,4" +
"1,-1:22,207,-1:76,311,-1:50,208,-1:60,209,-1:59,368,-1:60,42,-1:85,211,-1:4" +
"7,43,-1:63,212,-1:74,44,-1:46,316,-1:82,214,-1:64,314,-1:36,215,-1:83,45,-1" +
":32,219,-1:57,220,-1:88,222,-1:53,223,-1:39,225,-1:66,318,-1:63,48,-1:65,22" +
"6,-1:66,227,-1:62,229,-1:85,230,-1:56,321,-1:55,49,-1:35,232,-1:67,50,-1:58" +
",51,-1:75,233,-1:58,52,-1:73,53,-1:34,344,-1:64,322,-1:64,54,-1:89,320,-1:6" +
"4,237,-1:36,55,-1:57,238,-1:66,345,-1:62,239,-1:44,323,-1:66,58,-1:55,244,-" +
"1:69,59,-1:59,245,-1:92,60,-1:54,246,-1:43,248,-1:80,61,-1:67,249,-1:28,251" +
",-1:65,62,-1:61,63,-1:62,252,-1:83,64,-1:47,254,-1:75,65,-1:45,255,-1:53,25" +
"6,-1:76,257,-1:49,258,-1:69,259,-1:84,260,-1:36,261,-1:53,66,-1:59,67,-1:79" +
",262,-1:68,68,-1:67,69,-1:67,263,-1:19,264,-1:72,265,-1:74,325,-1:80,326,-1" +
":18,268,-1:72,269,-1:70,270,-1:80,271,-1:28,272,-1:72,273,-1:51,70,-1:84,71" +
",-1:30,5:22,74,5:25,-1:2,5:2,-1:11,120,-1:72,130,-1:86,141,-1:46,282,-1:48," +
"147,-1:93,288,-1:23,349,-1:63,335,-1:56,293,-1:68,336,-1:60,145,-1:73,295,-" +
"1:60,166,-1:82,348,-1:24,157,-1:90,151,-1:43,165,-1:64,164,-1:55,171,-1:56," +
"161,-1:71,307,-1:64,173,-1:54,178,-1:72,312,-1:49,338,-1:74,189,-1:46,192,-" +
"1:70,187,-1:85,313,-1:62,200,-1:38,202,-1:47,193,-1:72,197,-1:63,382,-1:45," +
"352,-1:72,213,-1:58,224,-1:59,210,-1:72,216,-1:58,231,-1:47,234,-1:72,228,-" +
"1:71,236,-1:46,235,-1:95,241,-1:32,324,-1:67,240,-1:44,243,-1:66,250,-1:92," +
"253,-1:49,266,-1:80,267,-1:17,5:6,78,5:41,-1:2,5:2,-1:32,279,-1:80,134,-1:2" +
"0,148,-1:62,297,-1:90,296,-1:43,302,-1:64,174,-1:55,339,-1:56,162,-1:74,195" +
",-1:51,205,-1:58,194,-1:74,203,-1:45,217,-1:67,218,-1:72,221,-1:50,242,-1:7" +
"2,247,-1:43,5:37,80,5:10,-1:2,5:2,-1:40,334,-1:46,340,-1:50,176,-1:61,342,-" +
"1:74,343,-1:51,315,-1:54,5:41,82,5:6,-1:2,5:2,-1:10,5,84,5:46,-1:2,5:2,-1:1" +
"0,5:12,86,5:35,-1:2,5:2,-1:10,5,88,5:46,-1:2,5:2,-1:10,5:12,90,5:35,-1:2,5:" +
"2,-1:10,5:6,92,5:41,-1:2,5:2,-1:10,5:4,94,5:43,-1:2,5:2,-1:10,5:37,96,5:10," +
"-1:2,5:2,-1:10,5:17,98,5:30,-1:2,5:2,-1:10,5,100,5:46,-1:2,5:2,-1:10,5:12,1" +
"02,5:35,-1:2,5:2,-1:10,5:22,104,5:25,-1:2,5:2,-1:10,5:41,106,5:6,-1:2,5:2,-" +
"1:10,5:33,354,5:14,-1:2,5:2,-1:10,294,-1:65,317,-1:74,319,-1:44,5:36,355,5:" +
"11,-1:2,5:2,-1:10,5:33,356,5:14,-1:2,5:2,-1:10,5:36,357,5:11,-1:2,5:2,-1:10" +
",5:4,358,5:43,-1:2,5:2,-1:10,5:9,359,5:38,-1:2,5:2,-1:10,5:17,360,5:30,-1:2" +
",5:2,-1:10,5:15,361,5:32,-1:2,5:2,-1:10,5:10,362,5:37,-1:2,5:2,-1:10,5:16,3" +
"63,5:31,-1:2,5:2,-1:10,5:4,364,5:43,-1:2,5:2,-1:10,5:17,365,5:30,-1:2,5:2,-" +
"1:10,5:7,371,5:40,-1:2,5:2,-1:28,369,-1:43,5:18,372,5:29,-1:2,5:2,-1:10,5:2" +
"5,373,5:22,-1:2,5:2,-1:10,5:5,374,5:42,-1:2,5:2,-1:10,5:44,375,5:3,-1:2,5:2" +
",-1:10,5:38,376,5:9,-1:2,5:2,-1:10,5:27,377,5:20,-1:2,5:2,-1:10,5:46,378,5," +
"-1:2,5:2,-1:10,5:7,379,5:40,-1:2,5:2,-1:10,5:18,380,5:29,-1:2,5:2,-1:10,5:4" +
",381,5:43,-1:2,5:2,-1:10,5:17,383,5:30,-1:2,5:2,-1:10,5,384,5:20,385,5:25,-" +
"1:2,5:2,-1:10,5:12,386,5:28,387,5:6,-1:2,5:2,-1:10,5:22,388,5:25,-1:2,5:2,-" +
"1:10,5:41,389,5:6,-1:2,5:2,-1:10,5:9,390,5:38,-1:2,5:2,-1:10,5:15,391,5:32," +
"-1:2,5:2,-1:10,396,5:47,-1:2,5:2,-1:10,5:30,397,5:17,-1:2,5:2,-1:10,5:3,398" +
",5:44,-1:2,5:2,-1:10,5:14,399,5:33,-1:2,5:2,-1:10,5:7,402,5:40,-1:2,5:2,-1:" +
"10,5:18,403,5:29,-1:2,5:2,-1:10,5:4,404,5:43,-1:2,5:2,-1:10,5:17,405,5:30,-" +
"1:2,5:2,-1:6");

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
						{ return new Symbol(sym.RBRA,yychar,yyline,
				    new String(yytext())); }
					case -3:
						break;
					case 3:
						{ return new Symbol(sym.RKET,yychar,yyline,
				    new String(yytext())); }
					case -4:
						break;
					case 4:
						{ System.err.println("Illegal character: "+yytext()); }
					case -5:
						break;
					case 5:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -6:
						break;
					case 6:
						{return new Symbol(sym.MINUS,yychar,yyline,
				    new String(yytext())); }
					case -7:
						break;
					case 7:
						{return new Symbol(sym.EQ,yychar,yyline,
				    new String(yytext())); }
					case -8:
						break;
					case 8:
						{return new Symbol(sym.Q,yychar,yyline,
				    new String(yytext())); }
					case -9:
						break;
					case 9:
						{ return new Symbol(sym.INTEGER,yychar,yyline, 
                                       new String(yytext())); }
					case -10:
						break;
					case 10:
						{ /* ignore white space. */ }
					case -11:
						break;
					case 11:
						{ /* ignore comment */ }
					case -12:
						break;
					case 12:
						{return new Symbol(sym.DEL,yychar,yyline,
				    new String(yytext())); }
					case -13:
						break;
					case 13:
						{return new Symbol(sym.AND,yychar,yyline,
				    new String(yytext())); }
					case -14:
						break;
					case 14:
						{return new Symbol(sym.AND,yychar,yyline,
				    new String(yytext())); }
					case -15:
						break;
					case 15:
						{return new Symbol(sym.DEL,yychar,yyline,
				    new String(yytext())); }
					case -16:
						break;
					case 16:
						{return new Symbol(sym.WHEN,yychar,yyline,
				    new String(yytext())); }
					case -17:
						break;
					case 17:
						{return new Symbol(sym.WHEN,yychar,yyline,
				    new String(yytext())); }
					case -18:
						break;
					case 18:
						{return new Symbol(sym.INITIALLY,yychar,yyline,
				    new String(yytext())); }
					case -19:
						break;
					case 19:
						{return new Symbol(sym.INITIALLY,yychar,yyline,
				    new String(yytext())); }
					case -20:
						break;
					case 20:
						{return new Symbol(sym.GOALS,yychar,yyline,
				    new String(yytext())); }
					case -21:
						break;
					case 21:
						{return new Symbol(sym.VARS,yychar,yyline,
				    new String(yytext())); }
					case -22:
						break;
					case 22:
						{return new Symbol(sym.VARS,yychar,yyline,
				    new String(yytext())); }
					case -23:
						break;
					case 23:
						{return new Symbol(sym.GOALS,yychar,yyline,
				    new String(yytext())); }
					case -24:
						break;
					case 24:
						{return new Symbol(sym.TYPES,yychar,yyline,
				    new String(yytext())); }
					case -25:
						break;
					case 25:
						{return new Symbol(sym.TYPES,yychar,yyline,
				    new String(yytext())); }
					case -26:
						break;
					case 26:
						{return new Symbol(sym.EITHER,yychar,yyline,
				    new String(yytext())); }
					case -27:
						break;
					case 27:
						{return new Symbol(sym.EITHER,yychar,yyline,
				    new String(yytext())); }
					case -28:
						break;
					case 28:
						{return new Symbol(sym.DEFINE,yychar,yyline,
				    new String(yytext())); }
					case -29:
						break;
					case 29:
						{return new Symbol(sym.DOMAIN,yychar,yyline,
				    new String(yytext())); }
					case -30:
						break;
					case 30:
						{return new Symbol(sym.DEFINE,yychar,yyline,
				    new String(yytext())); }
					case -31:
						break;
					case 31:
						{return new Symbol(sym.DOMAIN,yychar,yyline,
				    new String(yytext())); }
					case -32:
						break;
					case 32:
						{return new Symbol(sym.EFFECTS,yychar,yyline,
				    new String(yytext())); }
					case -33:
						break;
					case 33:
						{return new Symbol(sym.TYPING,yychar,yyline,
				    new String(yytext())); }
					case -34:
						break;
					case 34:
						{return new Symbol(sym.SERIAL,yychar,yyline,
				    new String(yytext())); }
					case -35:
						break;
					case 35:
						{return new Symbol(sym.STRIPS,yychar,yyline,
				    new String(yytext())); }
					case -36:
						break;
					case 36:
						{return new Symbol(sym.ACTION,yychar,yyline,
				    new String(yytext())); }
					case -37:
						break;
					case 37:
						{return new Symbol(sym.LENGTH,yychar,yyline,
				    new String(yytext())); }
					case -38:
						break;
					case 38:
						{return new Symbol(sym.EFFECTS,yychar,yyline,
				    new String(yytext())); }
					case -39:
						break;
					case 39:
						{return new Symbol(sym.ACTION,yychar,yyline,
				    new String(yytext())); }
					case -40:
						break;
					case 40:
						{return new Symbol(sym.LENGTH,yychar,yyline,
				    new String(yytext())); }
					case -41:
						break;
					case 41:
						{return new Symbol(sym.TYPING,yychar,yyline,
				    new String(yytext())); }
					case -42:
						break;
					case 42:
						{return new Symbol(sym.FORDOMAIN,yychar,yyline,
				    new String(yytext())); }
					case -43:
						break;
					case 43:
						{return new Symbol(sym.SERIAL,yychar,yyline,
				    new String(yytext())); }
					case -44:
						break;
					case 44:
						{return new Symbol(sym.STRIPS,yychar,yyline,
				    new String(yytext())); }
					case -45:
						break;
					case 45:
						{return new Symbol(sym.FORDOMAIN,yychar,yyline,
				    new String(yytext())); }
					case -46:
						break;
					case 46:
						{return new Symbol(sym.PROBLEM,yychar,yyline,
				    new String(yytext())); }
					case -47:
						break;
					case 47:
						{return new Symbol(sym.PROBLEM,yychar,yyline,
				    new String(yytext())); }
					case -48:
						break;
					case 48:
						{return new Symbol(sym.OBJECTS,yychar,yyline,
				    new String(yytext())); }
					case -49:
						break;
					case 49:
						{return new Symbol(sym.OBJECTS,yychar,yyline,
				    new String(yytext())); }
					case -50:
						break;
					case 50:
						{return new Symbol(sym.EQ,yychar,yyline,
				    new String(yytext())); }
					case -51:
						break;
					case 51:
						{return new Symbol(sym.STATICS,yychar,yyline,
				    new String(yytext())); }
					case -52:
						break;
					case 52:
						{return new Symbol(sym.EQ,yychar,yyline,
				    new String(yytext())); }
					case -53:
						break;
					case 53:
						{return new Symbol(sym.STATICS,yychar,yyline,
				    new String(yytext())); }
					case -54:
						break;
					case 54:
						{return new Symbol(sym.PARALLEL,yychar,yyline,
				    new String(yytext())); }
					case -55:
						break;
					case 55:
						{return new Symbol(sym.PARALLEL,yychar,yyline,
				    new String(yytext())); }
					case -56:
						break;
					case 56:
						{return new Symbol(sym.SITUATION,yychar,yyline,
				    new String(yytext())); }
					case -57:
						break;
					case 57:
						{return new Symbol(sym.SITUATION,yychar,yyline,
				    new String(yytext())); }
					case -58:
						break;
					case 58:
						{return new Symbol(sym.SITUATION,yychar,yyline,
				    new String(yytext())); }
					case -59:
						break;
					case 59:
						{return new Symbol(sym.CONSTANTS,yychar,yyline,
				    new String(yytext())); }
					case -60:
						break;
					case 60:
						{return new Symbol(sym.SITUATION,yychar,yyline,
				    new String(yytext())); }
					case -61:
						break;
					case 61:
						{return new Symbol(sym.CONSTANTS,yychar,yyline,
				    new String(yytext())); }
					case -62:
						break;
					case 62:
						{return new Symbol(sym.PREDS,yychar,yyline,
				    new String(yytext())); }
					case -63:
						break;
					case 63:
						{return new Symbol(sym.ARGS,yychar,yyline,
				    new String(yytext())); }
					case -64:
						break;
					case 64:
						{return new Symbol(sym.ARGS,yychar,yyline,
				    new String(yytext())); }
					case -65:
						break;
					case 65:
						{return new Symbol(sym.PREDS,yychar,yyline,
				    new String(yytext())); }
					case -66:
						break;
					case 66:
						{return new Symbol(sym.REQS,yychar,yyline,
				    new String(yytext())); }
					case -67:
						break;
					case 67:
						{return new Symbol(sym.PRE,yychar,yyline,
				    new String(yytext())); }
					case -68:
						break;
					case 68:
						{return new Symbol(sym.REQS,yychar,yyline,
				    new String(yytext())); }
					case -69:
						break;
					case 69:
						{return new Symbol(sym.PRE,yychar,yyline,
				    new String(yytext())); }
					case -70:
						break;
					case 70:
						{return new Symbol(sym.CONDEFF,yychar,yyline,
				    new String(yytext())); }
					case -71:
						break;
					case 71:
						{return new Symbol(sym.CONDEFF,yychar,yyline,
				    new String(yytext())); }
					case -72:
						break;
					case 73:
						{ System.err.println("Illegal character: "+yytext()); }
					case -73:
						break;
					case 74:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -74:
						break;
					case 75:
						{ /* ignore comment */ }
					case -75:
						break;
					case 77:
						{ System.err.println("Illegal character: "+yytext()); }
					case -76:
						break;
					case 78:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -77:
						break;
					case 80:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -78:
						break;
					case 82:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -79:
						break;
					case 84:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -80:
						break;
					case 86:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -81:
						break;
					case 88:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -82:
						break;
					case 90:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -83:
						break;
					case 92:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -84:
						break;
					case 94:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -85:
						break;
					case 96:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -86:
						break;
					case 98:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -87:
						break;
					case 100:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -88:
						break;
					case 102:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -89:
						break;
					case 104:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -90:
						break;
					case 106:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -91:
						break;
					case 274:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -92:
						break;
					case 327:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -93:
						break;
					case 346:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -94:
						break;
					case 353:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -95:
						break;
					case 354:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -96:
						break;
					case 355:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -97:
						break;
					case 356:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -98:
						break;
					case 357:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -99:
						break;
					case 358:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -100:
						break;
					case 359:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -101:
						break;
					case 360:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -102:
						break;
					case 361:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -103:
						break;
					case 362:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -104:
						break;
					case 363:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -105:
						break;
					case 364:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -106:
						break;
					case 365:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -107:
						break;
					case 366:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -108:
						break;
					case 370:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -109:
						break;
					case 371:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -110:
						break;
					case 372:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -111:
						break;
					case 373:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -112:
						break;
					case 374:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -113:
						break;
					case 375:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -114:
						break;
					case 376:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -115:
						break;
					case 377:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -116:
						break;
					case 378:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -117:
						break;
					case 379:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -118:
						break;
					case 380:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -119:
						break;
					case 381:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -120:
						break;
					case 383:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -121:
						break;
					case 384:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -122:
						break;
					case 385:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -123:
						break;
					case 386:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -124:
						break;
					case 387:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -125:
						break;
					case 388:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -126:
						break;
					case 389:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -127:
						break;
					case 390:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -128:
						break;
					case 391:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -129:
						break;
					case 392:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -130:
						break;
					case 393:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -131:
						break;
					case 394:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -132:
						break;
					case 395:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -133:
						break;
					case 396:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -134:
						break;
					case 397:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -135:
						break;
					case 398:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -136:
						break;
					case 399:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -137:
						break;
					case 400:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -138:
						break;
					case 401:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -139:
						break;
					case 402:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -140:
						break;
					case 403:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -141:
						break;
					case 404:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -142:
						break;
					case 405:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -143:
						break;
					case 406:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -144:
						break;
					case 407:
						{ return new Symbol(sym.NAME,yychar,yyline, 
                                       new String(yytext())); }
					case -145:
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
