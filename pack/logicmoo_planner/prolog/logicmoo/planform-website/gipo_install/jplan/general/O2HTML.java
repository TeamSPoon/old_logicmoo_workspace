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

package jplan.general;

/**
 * This class provied a number of convenience functions to help HTML
 * formatting. These form 2 broad classes
 * functions that provide standard HTML tags as an opening and
 * closing pair.
 * functions that deal with specific ocl components
 */

public class O2HTML {

    public static String H1 (String content) {
	return new String("<H1>" + content + "</H1>");
    }
    public static String H2 (String content) {
	return new String("<H2>" + content + "</H2>");
    }
    public static String H3 (String content) {
	return new String("<H3>" + content + "</H3>");
    }
    public static String H4 (String content) {
	return new String("<H4>" + content + "</H4>");
    }
    public static String H5 (String content) {
	return new String("<H5>" + content + "</H5>");
    }
    public static String green(String content) {
	return new String("<FONT COLOR=GREEN>" + content + "</FONT>");
    }
    public static String br() {
	return new String("<BR>");
    }

    public static String red(String content) {
	return new String("<FONT COLOR=RED>" + content + "</FONT>");
    }
    public static String pre(String content) {
	return new String("<PRE>" + content + "</PRE>");
    }
}
