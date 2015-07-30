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


package jplan.ocl;

import java.util.List;
import java.util.ArrayList;
import java.util.ListIterator;
import java.util.Hashtable;
import java.io.*;
import jplan.general.OPredicate;    /* Weihong added on 01/06/2001 */
import jplan.general.OCLSelectionException;


// public class oclSE implements oclPrint,Serializable{
/**
 * To store a state expression as part of prevail section.
 */
public class oclSE implements Cloneable, oclPrint, Serializable{ /* Weihong added on 04/06/2001 */
    String sort;
    String name;
    protected List  state = new ArrayList();

    /**
     * Creates an instance of oclSE.
     * @param kind sort
     * @param ID name
     */
    public oclSE(String kind, String ID) {
	sort = new String(kind);
	name = new String(ID);
    }

    /**
     * Add a new predicate
     * @param fname name of the predicate
     * @return oclPredicate
     */
    public oclPredicate addClause(String fname) {
	oclPredicate cur = new oclPredicate(fname);
	state.add(cur);
	return cur;
    }

    /* Weihong added on 10/5/2001 */
    /**
     * Get the value of Sort.
     * @return Value of Sort.
     */
    public String getSort() {return sort;}

    /* Weihong added on 10/05/2001 */    
    /**
     * Set the value of Sort.
     * @param v Value to assign to Sort.
     */
    public void setSort(String  v) {this.sort = v;}
    

    /* Weihong added on 10/05/2001 */  
    /**
     * Get the value of Name.
     * @return Value of Name.
     */
    public String getName() {return name;}

    /* Weihong added on 10/05/2001 */      
    /**
     * Set the value of Name.
     * @param v  Value to assign to Name.
     */
    public void setName(String  v) {this.name = v;}

    /* Weihong added on 9/5/2001 */
    /**
     * Set the predicate lists.
     * @param prd predicate lists to assign to state.
     * 
     */
    public void setPredicateList(java.util.List prd) {
	state = prd;
    }

    /* Weihong added on 11/5/2001 */
    /**
     * Get the predicate lists.
     * @return predicate lists.
     */
    public List getPredicateList() {
	return state;
    }

    // Ron 28/11/01
    /**
     * getStrippedState
     * returns the state list with all statics removed
     * @return - the stripped list
     */
    public List getStrippedState() {
	List ret = new ArrayList();
	ListIterator li = state.listIterator();
	while (li.hasNext()) {
	    oclPredicate cur = (oclPredicate)li.next();
	    if (! cur.isStatic()) {
		ret.add(cur);
	    }
	}
	return ret;
    }

    /* Weihong added on 9/5/2001 */
    /**
     * Add one predicate to the predicate lists.
     * @param prd predicate to add to state.
     * 
     */
    public void addPredicate(oclPredicate prd) {
	state.add(prd);
    }

    /* Weihong added on 25/05/2001 */
    /**
     * Get the predicate lists.
     * @return predicate lists.
     */
    public List getState(){
	return state;
    }

    /**
     * check that each predicate is fully instantiated
     * @return boolean
     */
    public boolean isFullyInstantiated() {
	ListIterator li = state.listIterator();
	while (li.hasNext()) {
	    oclPredicate curPred = (oclPredicate)li.next();
	    if (!curPred.isInstantiated()) {
		return false;
	    }
	}
	return true;
    }

    /**
     * refersTo
     * check to see if this se refers to the give predicate argument
     * @param name - the given predicate argument
     * @return - true if referred to
     */
    public boolean refersTo(String name) {
	ListIterator li = state.listIterator();
	boolean found = false;
	while (!found && li.hasNext()) {
	    oclPredicate cur = (oclPredicate)li.next();
	    if (cur.refersTo(name)) {
		found = true;
	    }
	}
	return found;
    }
	
	/**
     * refersToSort
     * check to see if this se refers to the given Sort
     * @param name - the given sort
     * @return - true if referred to
     */
    public boolean refersToSort(String name) {
	ListIterator li = state.listIterator();
	boolean found = false;
	while (!found && li.hasNext()) {
	    oclPredicate cur = (oclPredicate)li.next();
	    if (cur.refersToSort(name)) {
		found = true;
	    }
	}
	return found;
    }

    /**
     * verify that every predicate in the state is legal
     * that the state excluding static predicates is a valid
     * se expression.
     * A side effect of this procedure is to set the sorts for each
     * predicates arguments in the state description.
     * alse foe oclPlus sets fluent flag.
     * @param cur the current domain definition
     * @param mssgs The list to append error messages to
     * @return true if all checks passed
     */
    public boolean check(oclDomain cur, List mssgs) {
	List strippedState = new ArrayList();
	boolean res = true;
	ListIterator li = state.listIterator();
	while (li.hasNext()) {
	    oclPredicate curPred = (oclPredicate)li.next();
	    if (curPred.getName().equals("ne")) {
		// ne should be checked relative to the operators
		continue;
	    }
	    try {
		oclPredicate match = cur.findPrototype(curPred);
		curPred.setArgSorts(match);
		curPred.setSortForVariable(name, sort); /* WZ 5/6/02 */
		if (! match.isStatic() &&
		    ! match.getName().equals("is_of_sort")) {
		    if( !curPred.refersTo(name)) {
			mssgs.add("Predicate " + curPred.toString() +
				     " does not refer to the expression Id " +
				     name +
				     " of sort " +
				     sort);
			res = false;
		    }
		    if (match.isFluent()) {
		    	curPred.setFluent(true);
		    }
		    strippedState.add(curPred);
		} else if (match.isStatic()) {
		    curPred.setStatic(true);
		}
	    } catch (OCLSelectionException e) {
		mssgs.add("Predicate " + curPred.toString() +
			     " is not defined in the predicates list.");
		res = false;
	    }
	}
	if (!res) {
	    // dont check the sub state - a waste of time
	    res = false;
	} else {
	    res = cur.seUnifiesWithStateDef(name,sort,strippedState);
	    if (!res) {
		mssgs.add("Expression " +
			     this.toString() +
			     " does not match any state definition.");
	    }
	}
	return res;
    }

    /**
     * populate the consumer list of the matching state definition.
     * @param cur the current domain
     * @param opID the operator name/id that contains this clause
     * 
     */
    public void checkConsumer(oclDomain cur,String opID) {
	List strippedState = new ArrayList();
	ListIterator li = state.listIterator();
	while (li.hasNext()) {
	    oclPredicate curPred = (oclPredicate)li.next();
	    if (curPred.getName().equals("ne")) {
		continue;
	    }
	    try {
		oclPredicate match = cur.findPrototype(curPred);
		if (! match.isStatic() &&
		    ! match.getName().equals("is_of_sort")) {
		    strippedState.add(curPred);
		}
	    } catch (OCLSelectionException e) {
		continue;
	    }
	}
	cur.seUsesStateDef(name,sort,strippedState,opID);
    }

    /* Weihong added on 01/06/2001 */
    /**
     * for all the predicate replace with a new variable value.
     * @param before the varible
     * @param after new element name
     * 
     */
    public void replaceVariableName(OPredicate.pArg before, String after){
	if (before.name.equals(name)){    /* Weihong added on 04/06/2001 */
	    name = after;
	}
	ListIterator li = state.listIterator();
	while(li.hasNext()) {
	    oclPredicate oprd = (oclPredicate)li.next();
	    try {
		oprd.replaceVariableName(before, after);
	    } catch (Exception e){}
	}
    }

    /* Weihong added on 15/08/2001 */
    /**
     * for all the predicate replace with a new variable value.
     * @param before the varible
     * @param after new element name
     * 
     */
    public void replaceVariableName(String before, String after){
	if (before.equals(name)){  
	    name = after;
	}
	ListIterator li = state.listIterator();
	while(li.hasNext()) {
	    oclPredicate oprd = (oclPredicate)li.next();
	    try {
		oprd.replaceVariableNameByName(before, after);
	    } catch (Exception e){}
	}
    }

    /**
     * toSubStateList(
     * This is part of the PDDL->OCL translation Routines
     * turns this list belonging to a transition into a substate definition
     * changes the predicates to refer to the state ID
     * @param def - the definition to add to
     * @param ID - the Definition ID
     */
    public void toSubStateList(oclSSClassDef def,String ID){
	oclSE newStateSource = null;
	try {
	    newStateSource = (oclSE)this.clone();
	} catch (Exception e) {
	    System.err.println("Unexpected failure to clone se expression [oclSE]");
	    return;
	}
	newStateSource.replaceVariableName(name,ID);
	oclStateList newState = def.addState();
	ListIterator li = newStateSource.state.listIterator();
	while (li.hasNext()) {
	    newState.addPredicate((oclPredicate)li.next());
	}
	
    }

    /* Weihong added on 04/06/2001 */
    /**
     * clone a copy of oclSE
     * @throws CloneNotSupportedException
     * @return Object
     */
    public Object clone() throws CloneNotSupportedException{
	try {
	    oclSE se = new oclSE(sort, name);
	    java.util.List prdList = new ArrayList();
	    ListIterator li = state.listIterator();
	    while(li.hasNext()) {
		oclPredicate oprd = (oclPredicate)li.next();
		prdList.add((oclPredicate)oprd.clone());
	    }
	    se.setPredicateList(prdList);
	    return se;
	} catch (CloneNotSupportedException  e) {
	    throw e;
	}
    }
    
    /**
     * equals
     * equality operator
     * @param other
     * @return
     */
    public boolean equals(oclSE other){
    	return this.toString().equals(other.toString());
    }
    
    /**
     * collectVariableAssociations
     * used by life history editor to ensure standard variables for different Property predicates
	 * are not accidently unified
	 * problem occurs when there are multiple vare occurances within the same se/sc LHS
     * @param hashVars
     */
    public void collectVariableAssociations(Hashtable hashVars) {
    	ListIterator li = state.listIterator();
    	while (li.hasNext()) {
    		Object curPred = li.next();
    		if (curPred instanceof oclPredicate) {
    			((oclPredicate)curPred).collectVariableAssociations(hashVars);
    		}
    	}
    }

    /* Weihong added on 18/04/2001 */
    /**
     * Returns a string representation of the oclSE
     * @return String
     */
    public String toString(){
	StringBuffer pad = new StringBuffer();
	pad.append("se(" + sort + "," + name + ",[");
	ListIterator li = state.listIterator();
	while(li.hasNext()) {
	    	pad.append(((oclPredicate)li.next()).toString());
	    if(li.hasNext())
		pad.append(",");
	}
	pad.append( "])");
	return pad.toString();
    }

    /* WZ 19/6/02 */
    /**
     * Returns a string representation of the oclSE
     * For saving purpose
     * @return String
     */
    public String to_String(){
	StringBuffer pad = new StringBuffer();
	pad.append("se(" + sort + "," + name + ",[");
	ListIterator li = state.listIterator();
	while(li.hasNext()) {
	    	pad.append(((oclPredicate)li.next()).toString());
	    if(li.hasNext())
		pad.append("^");
	}
	pad.append( "])");
	return pad.toString();
    }

    /**
     * to print the current oclSE to a PrintWriter.
     * @param ps PrintWriter
     * @param indent value of indentation
     * @param nline not being used really
     * 
     */
    public void oclPrintComponent(PrintWriter ps, int indent, boolean nline) {
	String pad = "";
	for(int i = 0;i < indent;i++)
	    pad = pad + " ";
	ps.print(pad + "se(" + sort + "," + name + ",[");
	ListIterator li = state.listIterator();
	while(li.hasNext()) {
	    ((oclPredicate)li.next()).oclPrintComponent(ps,0,false);
	    if(li.hasNext())
		 ps.print(",");
	}
	ps.print("])");
    }
}
					      
