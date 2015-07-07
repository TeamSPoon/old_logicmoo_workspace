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

// pddlPredicate - Store Predicate Details 
package jplan.pddl;

import java.util.List;
import java.util.ArrayList;
import java.util.ListIterator;
import java.io.PrintWriter;

import jplan.ocl.oclPredicate;
import jplan.general.Utility;

/** Store PDDl predicates
 */
public class pddlPredicate implements pddlPrint {
    boolean isNegated = false;
    private List  args = new ArrayList();
    private String functor;

    /**
     * constructor
     * @param name of predicate as a String
     * @param negated flag to indicate if predicate id negated
     */
    public pddlPredicate (String name, boolean negated) {
	functor = name;
	isNegated = negated;
    }
    /**
     * default constructor for non negated predicates
     * @param name of predicate as a String
     */
    public pddlPredicate (String name) {
	functor = name;
    }

    /**
     * getName
     * @return String - the functor Name
     */
    public String getName() {
	return functor;
    }

    /**
     * getArgs
     * @return List - the list of pddlTypedVar arguments
     */
    public List getArgs() {
	return args;
    }

    /**
     * setNegated
     * @param neg - value of isNegated
     */
    public void setNegated(boolean neg) {
	isNegated = neg;
    }

    /**
     * isNegated
     * @return boolean - value of isNegated
     */
    public boolean isNegated() {
	return isNegated;
    }			   

    /**
     * Add a variable / constant as a String to the predicate
     */
    public void addVar(pddlTypedVar var) {
	args.add(var);
    }

    /**
     * mainParam
     * select the first parameter as controlling variable for the predicate
     * if there is no parameters create one with the variable ?impliedAgent
     * of type "agent"
     * @return pddlTypedVar - the controlling variable
     */
    public pddlTypedVar mainParam() {
	if(args.size() > 0) {
	    return((pddlTypedVar)args.get(0));
	} else {
	    pddlTypedVar impAgent = new pddlTypedVar("?impliedAgent");
	    impAgent.addType("agent");
	    return impAgent;
	}
    }

    /**
     * transToOcl
     * convert to ocl structure merge negations into functor name
     * @return oclPredicate 
     */
    public oclPredicate transToOcl() {
	oclPredicate curOclPred;
	if (isNegated) {
	    curOclPred = new oclPredicate("not_" + functor);
	} else {
	    curOclPred = new oclPredicate(functor);
	}
	ListIterator li = args.listIterator();
	while (li.hasNext()) {
	    pddlTypedVar param = (pddlTypedVar)li.next();
	    String oclParam = param.transToOcl();
	    if (param.isVar()) {
		curOclPred.addVarArgument(oclParam);
	    } else {
		curOclPred.addConstArgument(oclParam);
	    }
	}
	return curOclPred;
	    
    }

    /**
     * isNegOf
     * test to see if this predicate is the negation of the given predicate
     * @param given - the given predicate
     * @return boolean - true if is negation of given predicate
     */
    public boolean isNegOf(pddlPredicate given) {
	if (isNegated && !given.isNegated){
	    if (functor.equals(given.functor) &&
		(args.size() == given.args.size())) {
		return true;
	    } else {
		return false;
	    }
	} if (given.isNegated && !isNegated) {
	    if (functor.equals(given.functor) &&
		(args.size() == given.args.size())) {
		return true;
	    } else {
		return false;
	    }
	} else {
	    return false;
	}
    }
	

    /**
     * clone
     * @return Object
     */
    public Object clone() throws CloneNotSupportedException {
	pddlPredicate temp = new pddlPredicate(functor,isNegated);
	ListIterator li = args.listIterator();
	while (li.hasNext()) {
	    pddlTypedVar var = (pddlTypedVar)li.next();
	    try {
		temp.addVar((pddlTypedVar)var.clone());
	    } catch (CloneNotSupportedException e){
		Utility.debugPrintln("Cannot clone pddlTypedVar.");
		throw e;
	    }
	}
	return temp;
    }

    /**
     * toString
     * print in standard form to a String
     * @return String
     */
    public String toString() {
	String res = new String("(");
	if (isNegated) {
	    res = res.concat("not(");
	    ListIterator li = args.listIterator();
	    res = res.concat(functor + " ");
	    while(li.hasNext()) {
		res = res.concat(((pddlTypedVar)li.next()).toString());
		if (li.hasNext())
		    res = res.concat(" ");
	    }
	    res = res.concat(")");
	} else {
	    ListIterator li = args.listIterator();
	    res = res.concat(functor + " ");
	    while(li.hasNext()) {
		res = res.concat(((pddlTypedVar)li.next()).toString());
		if (li.hasNext())
		    res = res.concat(" ");
	    }
	}
	res = res.concat(")");
	return res;
    }


    /**
     * printPredicateList
     * print a pddl predicate list
     * used for DEBUGGING
     * @param curClause
     */
    static void printPredicateList(List curClause) {
	ListIterator liTemp = curClause.listIterator();
	Utility.debugPrint("Current clause: " + curClause.size() + " : ");
	while (liTemp.hasNext()) {
	    pddlPredicate cur = (pddlPredicate)liTemp.next();
	    Utility.debugPrint(cur.toString());
	}
	Utility.debugPrintln(" -end");
    }
    
	

    /** Standard print routine for actions
     * @param ps as a PrintWriter
     * @param indent the level of left padding required - ignored by actions
     * @param nline indicating if structure concludes with a new line - ignored
     */
    public void pddlPrintComponent(PrintWriter ps,int indent,boolean nline) {
	String pad = "";
	for(int i = 0;i < indent;i++)
	    pad = pad + " ";
	if (isNegated) {
	    ps.print(pad +  "(not ");
	    ListIterator li = args.listIterator();
	    ps.print("(" + functor + " ");
	    while(li.hasNext()) {
		((pddlTypedVar)li.next()).pddlPrintComponent(ps,0,false);
		if (li.hasNext())
		    ps.print(" ");
	    }
	    ps.print(")");
	} else {
	    ListIterator li = args.listIterator();
	    ps.print(pad + "(" + functor + " ");
	    while(li.hasNext()) {
		((pddlTypedVar)li.next()).pddlPrintComponent(ps,0,false);
		if (li.hasNext())
		    ps.print(" ");
	    }
	}
	if (nline)
	    ps.println(")");
	else
	    ps.print(")");
    }
}
