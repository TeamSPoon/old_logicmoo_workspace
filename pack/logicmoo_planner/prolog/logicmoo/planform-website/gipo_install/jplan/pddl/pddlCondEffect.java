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

// pddlCondEfeect - Store when - clause details 
package jplan.pddl;

import java.util.List;
import java.util.ArrayList;
import java.util.ListIterator;
import java.io.PrintWriter;

/** Store PDDl conditional effects i.e. when action clauses in effect
 * section.
 */
public class pddlCondEffect implements pddlPrint {
    public List pre = new ArrayList();
    public List post = new ArrayList();

    /**
     * Add precondition clause
     * @param fname - the predicate name as a String
     * @param negated - indicator to determine if the predicate is negated
     * @return reference to newly added pddlPrdicate
     */
    public pddlPredicate addPrecondition (String fname, boolean negated) {
	pddlPredicate cur;
	if (negated)
	    cur = new pddlPredicate(fname,true);
	else
	    cur = new pddlPredicate(fname);
	pre.add(cur);
	return cur;
    }

    /**
     * Add post condition clause
     * @param fname - the predicate name as a String
     * @param negated - indicator to determine if the predicate is negated
     * @return reference to newly added pddlPrdicate
     */
    public pddlPredicate addPostcondition (String fname, boolean negated) {
	pddlPredicate cur;
	if (negated)
	    cur = new pddlPredicate(fname,true);
	else
	    cur = new pddlPredicate(fname);
	post.add(cur);
	return cur;
    }

    /** Standard print routine for conditional effects
     * @param ps - stream as a PrintWriter
     * @param indent the level of left padding required 
     * @param nline indicating if structure concludes with a new line 
     */
    public void pddlPrintComponent(PrintWriter ps,int indent,boolean nline) {
	ListIterator li;
	String pad = "";
	ps.println("");
	for(int i = 0;i < indent;i++)
	    pad = pad + " ";
	ps.print(pad + "(when ");
	if (pre.size() > 1) {
	    ps.print("(and");
	    li = pre.listIterator();
	    while (li.hasNext()) {
		((pddlPredicate)li.next()).pddlPrintComponent(ps,0,false);
	    }
	    ps.println(indent + ")");
	} else {
	    li = pre.listIterator();
	    while (li.hasNext()) {
		((pddlPredicate)li.next()).pddlPrintComponent(ps,0,true);
	    }
	}
	if (post.size() > 1) {
	    ps.print(pad + "  (and");
	    li = post.listIterator();
	    while (li.hasNext()) {
		((pddlPredicate)li.next()).pddlPrintComponent(ps,0,false);
	    }
	    ps.print(" )");
	} else {
	    li = post.listIterator();
	    while (li.hasNext()) {
		((pddlPredicate)li.next()).pddlPrintComponent(ps,indent + 4,true);
	    }
	    ps.print(")");
	}
    }
}
