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

// InconsistentConst - Store inconsistent predicate list
package jplan.ocl;

import java.util.List;
import java.util.ArrayList;
import java.util.ListIterator;
import java.io.*;

/**
 * oclInconsistentConst
 * store the list of predicates forming a nigative invariant
 * @author Ron Simpson
 * @version 0
 */
public class oclInconsistentConst implements oclPrint,Serializable,Cloneable {
    private List  preds = new ArrayList();

    /**
     * add a predicate to to be built the list
     * @param fname - the predicate name
     * @return reference to the new predicate added
     */
    public oclPredicate addClause(String fname) {
	oclPredicate cur = new oclPredicate(fname);
	preds.add(cur);
	return cur;
    }

    /**
     * getPredicateList
     * return the predicate of this invariant
     * @return List - the predicate list
     */
    public List getPredicateList() {
	return preds;
    }

    /**
     * setPredicateList
     * update the predicate list with new list of oclPredicates
     * @param pList - the predicate list
     */
    public void setPredicateList(List pList) {
	preds = pList;
    }

    /**
     * toString()
     * standard single line string representation
     * @return String
     */
    public String toString() {
	String res = "";
	ListIterator li = preds.listIterator();
	while(li.hasNext()) {
	    res = res.concat(((oclPredicate)li.next()).toString());
	    if (li.hasNext())
		res = res.concat(new String(" "));
	}
	return res;
    }

    /**
     * clone()
     *
     */
    public Object clone() throws CloneNotSupportedException {
	try {
	    oclInconsistentConst oIC = new oclInconsistentConst();
	    ListIterator li = preds.listIterator();
	    while (li.hasNext()) {
		oIC.preds.add(((oclPredicate)li.next()).clone());
	    }
	    return oIC;
	} catch (CloneNotSupportedException  e) {
	    throw e;
	}
    }

    /**
     * standard ocl print routine to output canonical ocl
     */
    public void oclPrintComponent(PrintWriter ps,int indent,boolean nline) {
	ps.print("inconsistent_constraint([");
	ListIterator li = preds.listIterator();
	while (li.hasNext()) {
	    ((oclPredicate)li.next()).oclPrintComponent(ps,0,false);
	    if (li.hasNext())
		ps.print(",");
	}
	ps.println("]).");
    }
}
