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

// oclImpliedInvar.java Store optional implied invarients
package jplan.ocl;

import java.util.List;
import java.util.ArrayList;
import java.util.ListIterator;
import java.io.*;

/**
 * oclImpliedInvar -
 * Store a single implied invarient clause
 * @author Ron Simpson
 * @version 0
 */
public class oclImpliedInvar implements oclPrint,Serializable,Cloneable {
    private List  left = new ArrayList();
    private List  right = new ArrayList();

    /**
     * addLeftClause
     * create a new left clause with the given predicate name
     * @param fname - the predicate name
     * @return reference to the new clause
     */
    public oclPredicate addLeftClause(String fname) {
	oclPredicate cur = new oclPredicate(fname);
	left.add(cur);
	return cur;
    }
    /**
     * addRightClause
     * create a new left clause with the given predicate name
     * @param fname - the predicate name
     * @return reference to the new clause
     */
    public oclPredicate addRightClause(String fname) {
	oclPredicate cur = new oclPredicate(fname);
	right.add(cur);
	return cur;
    }
    /**
     * addLeft
     * add a predicate to the left list predicate name
     * @param pred the predicate 
     */
    public void addLeft(oclPredicate pred) {
	left.add(pred);
    }
    /**
     * addRight
     * add a predicate to the right list predicate name
     * @param pred the predicate 
     */
    public void addRight(oclPredicate pred) {
	right.add(pred);
    }
    /**
     * getLeftList-
     * @return List the left - antecedent list of the implication
     */
    public List getLeftList() {
	return left;
    }

    /**
     * getRightList-
     * @return List the left - consequent list of the implication
     */
    public List getRightList() {
	return right;
    }

    /** clone
     * standard clone method
     */
    public Object clone() throws CloneNotSupportedException{
	oclImpliedInvar res = new oclImpliedInvar();
	ListIterator li = left.listIterator();
	while (li.hasNext()) {
	    res.left.add(((oclPredicate)li.next()).clone());
	}
	li = right.listIterator();
	while (li.hasNext()) {
	    res.right.add(((oclPredicate)li.next()).clone());
	}
	return res;
    }

    /**
     * toString 
     * default text representation
     * @return String
     */
    public String toString() {
	String res = "[";
	ListIterator li = left.listIterator();
	while(li.hasNext()) {
	    res = res.concat(((oclPredicate)li.next()).toString());
	    if(li.hasNext())
		 res = res.concat(",");
	}
	res = res.concat("],[");
	li = right.listIterator();
	while(li.hasNext()) {
	    res = res.concat(((oclPredicate)li.next()).toString());
	    if(li.hasNext())
		 res = res.concat(",");
	}
	res = res.concat("]");
	return res;
    }


    public void oclPrintComponent(PrintWriter ps,int indent,boolean nline) {
	ps.print("implied_invariant([");
	ListIterator li = left.listIterator();
	while(li.hasNext()) {
	    ((oclPredicate)li.next()).oclPrintComponent(ps,0,false);
	    if(li.hasNext())
		 ps.print(",");
	}
	ps.print("],[");
	li = right.listIterator();
	while(li.hasNext()) {
	    ((oclPredicate)li.next()).oclPrintComponent(ps,0,false);
	    if(li.hasNext())
		 ps.print(",");
	}
	ps.println("]).");
    }
}
