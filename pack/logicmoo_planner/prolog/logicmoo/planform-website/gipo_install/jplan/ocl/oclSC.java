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

import java.util.Hashtable;
import java.util.List;
import java.util.ArrayList;
import java.util.ListIterator;
import java.util.Iterator;
import java.util.Set;
import java.io.*; /* Weihong changed on 30/06/2001 */
import jplan.general.OPredicate;    /* Weihong added on 01/06/2001 */
import jplan.general.Utility;

import jplan.general.OPredicate;
import jplan.general.OCLSelectionException;

/**
 * To store a state transition as part of necessary or conditional.
 */
public class oclSC implements Cloneable, oclPrint,Serializable{ /* Weihong added on 30/06/2001 */
    String sort;
    String name;
    protected List  pre = new ArrayList();
    protected List  post = new ArrayList();

    /**
     * Creates an instance of oclSC.
     * @param kind sort
     * @param ID name
     */
    public oclSC(String kind, String ID) {
	sort = new String(kind);
	name = new String(ID);
    }

    /**
     * Add a new predicate to the precondition
     * @param fname name of the predicate
     * @return oclPredicate
     */
    public oclPredicate addPreClause(String fname) {
	oclPredicate cur = new oclPredicate(fname);
	pre.add(cur);
	return cur;
    }

    /**
     * Add a new predicate to the poststate
     * @param fname name of the predicate
     * @return oclPredicate
     */
    public oclPredicate addPostClause(String fname) {
	oclPredicate cur = new oclPredicate(fname);
	post.add(cur);
	return cur;
    }

    /* Weihong added on 11/05/2001 */
    /**
     * Add a new predicate to the prestate
     * @param opd the predicate
     */
    public void addPre(oclPredicate opd){
	pre.add(opd);
    }

    /* Weihong added on 11/05/2001 */
    /**
     * Add a new predicate to the poststate
     * @param opd the predicate
     */
    public void addPost(oclPredicate opd){  
	post.add(opd);
    }

    /* Weihong added on 04/05/2001 */
    /**
     * Get the value of Sort.
     * @return Value of Sort.
     */
    public String getSort() {return sort;}

    /* Weihong added on 04/05/2001 */    
    /**
     * Set the value of Sort.
     * @param v  Value to assign to Sort.
     */
    public void setSort(String  v) {this.sort = v;}

    /* Weihong added on 04/05/2001 */  
    /**
     * Get the value of Name.
     * @return Value of Name.
     */
    public String getName() {return name;}

    /* Weihong added on 04/05/2001 */      
    /**
     * Set the value of Name.
     * @param v Value to assign to Name.
     */
    public void setName(String  v) {this.name = v;}

    /* Weihong added on 12/04/2001 */
    /**
     * Get the predicate lists in the prestate.
     * @return predicate lists in the prestate.
     */
    public List getPre() {
	return pre;
    }

    /* Weihong added on 9/5/2001 */
    /**
     * Set the predicate lists in the prestate.
     * @param prd predicate lists to assign to the prestate.
     * 
     */
    public void setPre(List prd) {
	pre = prd;
    }

    /* Weihong added on 12/04/2001 */
    /**
     * Get the predicate lists in the poststate.
     * @return predicate lists in the poststate.
     */
    public List getPost() {
	return post;
    }
    /* Weihong added on 9/5/2001 */
    /**
     * Set the predicate lists in the poststate.
     * @param prd predicate lists to assign to the poststate.
     * 
     */
    public void setPost(List prd) {
	post = prd;
    }

    /**
     * verify that every predicate in the state is legal
     * that LHS state excluding static predicates is a valid
     * se expression
     * that RHS state excluding static predicates
     * unifies with a state definition
     * This method has a SIDE EFFECT of setting
     * argument sorts and setting fluent flag for oclPlus domains
     * @param cur the current domain definition
     * @param mssgs The list to append error messages to
     * @return true if all checks passed
     */
    public boolean check(oclDomain cur, List mssgs) {
	List strippedPre = new ArrayList();
	List strippedPost = new ArrayList();
	boolean res = true;
	boolean postRes = true;
	ListIterator li = pre.listIterator();
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
		if (! match.isStatic()&&
		    ! match.getName().equals("is_of_sort")) {
		    if( !curPred.refersTo(name)) {
			mssgs.add("Predicate " + curPred.toString() +
				     " does not refer to the Transition Id " +
				     name +
				     " of sort " +
				     sort);
			res = false;
		    }
		    if (match.isFluent()) {
		    	curPred.setFluent(true);
		    }
		    strippedPre.add(curPred);
		} else if (match.isStatic()) {
		    curPred.setStatic(true);
		}
	    } catch (OCLSelectionException e) {
		mssgs.add("Predicate " + curPred.toString() +
			     " is not defined in the predicates list.");
		res = false;
	    }
	}
	li = post.listIterator();
	while (li.hasNext()) {
	    oclPredicate curPred = (oclPredicate)li.next();
	    if (curPred.getName().equals("ne")) {
		strippedPost.add(curPred);
		// leave them in - may be in state def
		// ne should be checked relative to the operators
		continue;
	    }
	    try {
		oclPredicate match = cur.findPrototype(curPred);
		curPred.setArgSorts(match);
		curPred.setSortForVariable(name, sort); /* WZ 5/6/02 */
		if (! match.isStatic()&&
		    ! match.getName().equals("is_of_sort")) {
		    if( !curPred.refersTo(name)) {
			mssgs.add("Predicate " + curPred.toString() +
				     " does not refer to the Transition Id " +
				     name +
				     " of sort " +
				     sort);
			res = false;
		    }
		    strippedPost.add(curPred);
		} else if (curPred.getName().equals("is_of_sort") &&
			   curPred.getNthElementName(0).equals(name)) {
		    // This restriction clause refers to the transition
		    // id - so should be in the class def
		    strippedPost.add(curPred);
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
	    res = cur.seUnifiesWithStateDef(name,sort,strippedPre);
	    if (!res) {	
		mssgs.add("LHS Expression " +
			     OPredicate.predListToString(strippedPre) +
			     " does not match any state definition.");
	    }
	    postRes =
		cur.ssUnifiesWithStateDef(name,sort,strippedPost);
	    if (!postRes) {
		mssgs.add("RHS Expression " +
			     OPredicate.predListToString(strippedPost) +
			     " does not match any state definition.");
	    }
	}
	return (res && postRes);
    }

    /**
     * find consumed and produced state in class def.
     * @param cur the current domain definition
     * @param opID the operator name/id containing this clause
     * 
     */
    public void checkUses(oclDomain cur,String opID) {
	List strippedPre = new ArrayList();
	List strippedPost = new ArrayList();
	boolean res = true;
	boolean postRes = true;
	ListIterator li = pre.listIterator();
	while (li.hasNext()) {
	    oclPredicate curPred = (oclPredicate)li.next();
	    if (curPred.getName().equals("ne")) {
		continue;
	    }
	    try {
		oclPredicate match = cur.findPrototype(curPred);
		if (! match.isStatic()&&
		    ! match.getName().equals("is_of_sort")) {
		    strippedPre.add(curPred);
		}
	    } catch (OCLSelectionException e) {
		continue;
	    }
	}
	li = post.listIterator();
	while (li.hasNext()) {
	    oclPredicate curPred = (oclPredicate)li.next();
	    if (curPred.getName().equals("ne")) {
		strippedPost.add(curPred);
		continue;
	    }
	    try {
		oclPredicate match = cur.findPrototype(curPred);
		if (! match.isStatic()&&
		    ! match.getName().equals("is_of_sort")) {
		    strippedPost.add(curPred);
		}
	    } catch (OCLSelectionException e) {
		continue;
	    }
	}
	cur.seUsesStateDef(name,sort,strippedPre,opID);
	cur.ssUsesStateDef(name,sort,strippedPost,opID);
    }


    /* Weihong added on 14/08/2001 */
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
	replaceVariableNameFromAList(pre, before, after);
	replaceVariableNameFromAList(post, before, after);
    }

    /* Weihong added on 14/08/2001 */
    /**
     * replace variable name from a given list.
     * @param listPred the predicate list
     * @param before the varible
     * @param after new element name
     * 
     */
    public void replaceVariableNameFromAList(List listPred, String before, String after){
	ListIterator li = listPred.listIterator();
	while(li.hasNext()) {
	    oclPredicate oprd = (oclPredicate)li.next();
	    try {
		oprd.replaceVariableNameByName(before, after);
	    } catch (Exception e){}
	}
    }


    /* Weihong added on 01/06/2001 */
    /**
     * for all the predicate replace with a new variable value.
     * @param before the varible
     * @param after new element name
     * 
     */
    public void replaceVariableName(OPredicate.pArg before, String after){
	Utility.debugPrintln("replaceVariableName","XXXX >> " + before.name + "  with " + after);
	if (before.name.equals(name)){    /* Weihong added on 04/06/2001 */
	    name = after;
	}
	replaceVariableNameFromAList(pre, before, after);
	replaceVariableNameFromAList(post, before, after);
    }

    /**
     * replace variable name from a given list.
     * @param listPred the predicate list
     * @param before the varible
     * @param after new element name
     * 
     */
    public void replaceVariableNameFromAList(List listPred, OPredicate.pArg before, String after){
	Utility.debugPrintln("sc","XXXX >> " + before.name + "  with " + after);
	ListIterator li = listPred.listIterator();
	while(li.hasNext()) {
	    oclPredicate oprd = (oclPredicate)li.next();
	    try {
		oprd.replaceVariableName(before, after);
	    } catch (Exception e){}
	}
    }
    
    /**
     * renameConflictVariables
     * used by life history editor to ensure standard variables for different Property predicates
	 * are not accidently unified
	 * problem occurs when there are multiple var occurances within the same se/sc LHS
	 * @param dom - the oclDomain being built
     */
    public void renameConflictVariables(oclDomain dom) {
    	renameConflictVariablesInList(dom,pre);
    	renameConflictVariablesInList(dom,post);
    }
    /**
     * renameConflictVariablesInList
     * @param dom
     * @param element the pre or post predicate list
     */
    public void renameConflictVariablesInList(oclDomain dom,List element) {
    	ListIterator li = element.listIterator();
    	Hashtable hashVars = new Hashtable();
    	while (li.hasNext()) {
    		Object curPred = li.next();
    		if (curPred instanceof oclPredicate) {
    			((oclPredicate)curPred).collectVariableAssociations(hashVars);
    		}
    	}
    	Set keys = hashVars.keySet();
    	Iterator it = keys.iterator();
    	while (it.hasNext()) {
    		String var = (String)it.next();
    		if (!var.equals(name)) {
    			List preds = (List)hashVars.get(var);
    			if (preds.size() >  1) {
    				// Possible conflict
    				// is this a variable promoted from a sorm name
    				String sort = oclPredicate.toConst(var);
    				if (dom.isSort(sort)) {
    					//Conflict
    					List delList = new ArrayList();
    					ListIterator pli = preds.listIterator();
    					while (pli.hasNext()){
    						oclPredicate curPred = (oclPredicate)pli.next();
    						// are there multiple occurrances of this predicate
    						ListIterator pli2 = preds.listIterator();
    						pli2.next(); // ignore the first one this the one we are comparing with
    						boolean found = false;
    						while(pli2.hasNext()) {
    							oclPredicate predCompare = (oclPredicate)pli2.next();
    							if (predCompare == curPred) {
    								found = true;
    							}
    						}
    						if (found) {
    							// single predicate with multiple occurances of same variable
    							curPred.destinguishVarsAlpha("x");
    							delList.add(curPred);
    						}
    					}
    					ListIterator dli = delList.listIterator();
    					while (dli.hasNext()) {
    						oclPredicate delPred = (oclPredicate)dli.next();
    						preds.remove(delPred);
    					}
    					// Now only variables occuring in multiple predicates left
    					pli = preds.listIterator();
    					String nextSuffix = "A";
    					while (pli.hasNext()){
    						oclPredicate curPred = (oclPredicate)pli.next();
    						try {
    							curPred.replaceVariableNameByName(var,var + "x" + nextSuffix);
    						} catch (Exception ex){
    							System.out.println("ERROR [oclSC cannot rename variable");
    						}
    						nextSuffix = new String("" + (char)((nextSuffix.charAt(0)) + 1));
    					}
    				}
    			}
    		} 
    	}
    }

    // Ron new for opmaker 19/11/01
    /**
     * transitionRefersTo
     * scan the predicates to see if given value is a name argument
     * in one of the predicates of the transition
     * @param val - the given name
     * @return - true if name found
     */
    public boolean transitionRefersTo(String val) {
	ListIterator li = pre.listIterator();
	boolean found = false;
	while (!found && li.hasNext()) {
	    oclPredicate cur = (oclPredicate)li.next();
	    if (cur.refersTo(val)) {
		found = true;
	    }
	}
	li = post.listIterator();
	while (!found && li.hasNext()) {
	    oclPredicate cur = (oclPredicate)li.next();
	    if (cur.refersTo(val)) {
		found = true;
	    }
	}
	return found;
    }

    // Ron added to improve operator checks 7/6/02
    /**
     * transitionLHSRefersTo
     * scan the predicates to see if given value is a name argument
     * in one of the predicates of the transition's LHS
     * @param val - the given name
     * @return - true if name found
     */
    public boolean transitionLHSRefersTo(String val) {
	ListIterator li = pre.listIterator();
	boolean found = false;
	while (!found && li.hasNext()) {
	    oclPredicate cur = (oclPredicate)li.next();
	    if (cur.refersTo(val)) {
		found = true;
	    }
	}
	return found;
    }
	/**
	  * transitionRHSRefersTo
	  * scan the predicates to see if given value is a name argument
	  * in one of the predicates of the transition's RHS
	  * @param val - the given name
	  * @return - true if name found
	  */
	 public boolean transitionRHSRefersTo(String val) {
	 ListIterator li = post.listIterator();
	 boolean found = false;
	 while (!found && li.hasNext()) {
		 oclPredicate cur = (oclPredicate)li.next();
		 if (cur.refersTo(val)) {
		 found = true;
		 }
	 }
	 return found;
	 }

    /**
     * equivalentLHS
     * check to see if two transitions
     * are the same apart from id
     * @param other - the transition to check against
     * @return - true if equivalent
     */
    public boolean equivalentLHS(oclSC other) {
	oclSC target = null;
	try {
	    target = (oclSC)other.clone();
	} catch (CloneNotSupportedException e) {
	    System.err.println("Unexpectef failure to clone transition {oclSC]");
	}
	target.replaceVariableName(target.getName(),name);
	if (pre.size() != target.pre.size()) {
	    return false;
	}
	ListIterator li = pre.listIterator();
	ListIterator liT = target.pre.listIterator();
	boolean Same = true;
	while (Same && li.hasNext()) {
	    oclPredicate thisPred = (oclPredicate)li.next();
	    oclPredicate targPred = (oclPredicate)liT.next();
	    if (! thisPred.equals(targPred)) {
		Same = false;
	    }
	}
	return Same;
    }

	
    /**
     * toLHSSubStateList
     * This is part of the PDDL->OCL translation Routines
     * turns this list belonging to a transition into a substate definition
     * changes the predicates to refer to the state ID
     * Deal with LHS (pre) only
     * @param   def - the definition to add to
     * @param ID - the Definition ID
     */
    public void toLHSSubStateList(oclSSClassDef def,String ID){
	toSubStateList(def,ID,pre);
    }

    /**
     * toRHSSubStateList
     * This is part of the PDDL->OCL translation Routines
     * turns this list belonging to a transition into a substate definition
     * changes the predicates to refer to the state ID
     * Deal with RHS (prost) only
     * @param  def - the definition to add to
     * @param ID - the Definition ID
     */
    public void toRHSSubStateList(oclSSClassDef def,String ID){
	toSubStateList(def,ID,post);
    }

    /**
     * toSubStateList
     * This is part of the PDDL->OCL translation Routines
     * turns this list belonging to a transition into a substate definition
     * changes the predicates to refer to the state ID
     * @param  def - the definition to add to
     * @param ID- the Definition ID
     * @param preds the candidate state list
     */

    public void toSubStateList(oclSSClassDef def,String ID, List preds){
	oclStateList newState = def.addState();
	ListIterator li = preds.listIterator();
	while (li.hasNext()) {
	    newState.addPredicate((oclPredicate)li.next());
	}
	
    }

    /* Weihong added on 04/06/2001 */
    /**
     * clone a copy of oclSC
     * @throws CloneNotSupportedException
     * @return Object
     */
    public Object clone() throws CloneNotSupportedException{
	try{
	    oclSC sc = new oclSC(sort, name);
	    java.util.List prdList = new ArrayList();
	    ListIterator li = pre.listIterator();
	    while(li.hasNext()) {
		oclPredicate oprd = (oclPredicate)li.next();
		sc.addPre((oclPredicate)oprd.clone());
	    }
	    
	    li = post.listIterator();
	    while(li.hasNext()) {
		oclPredicate oprd = (oclPredicate)li.next();
		sc.addPost((oclPredicate)oprd.clone());
	    }

	    return sc;
	} catch (CloneNotSupportedException e){
	    throw e;
	}
    }

    /* WZ 5/6/02 */
    /**
     * Returns a string representation of the oclSC
     * @return String
     */
    public String toString(){
	String res = "";
	res = res.concat("sc(" + sort + "," + name + ",[");
	ListIterator li = getPre().listIterator();
	while(li.hasNext()) {
	    res = res.concat(((oclPredicate)li.next()).toString());
	    if (li.hasNext())
		res = res.concat(new String(","));
	}
	res = res.concat("]=>[");
        li = getPost().listIterator();
	while(li.hasNext()) {
	    res = res.concat(((oclPredicate)li.next()).toString());
	    if (li.hasNext())
		res = res.concat(new String(","));
	}
	res = res.concat("])");
	return res;
    }

    /* WZ 19/6/02 */
    /**
     * Returns a string representation of the oclSC
     * @return String
     */
    public String to_String(){
	String res = "";
	res = res.concat("sc(" + sort + "," + name + ",[");
	ListIterator li = getPre().listIterator();
	while(li.hasNext()) {
	    res = res.concat(((oclPredicate)li.next()).toString());
	    if (li.hasNext())
		res = res.concat(new String("^"));
	}
	res = res.concat("]=>[");
        li = getPost().listIterator();
	while(li.hasNext()) {
	    res = res.concat(((oclPredicate)li.next()).toString());
	    if (li.hasNext())
		res = res.concat(new String("^"));
	}
	res = res.concat("])");
	return res;
    }

    /**
     * to print the current oclSC to a PrintWriter.
     * @param ps PrintWriter
     * @param indent value of indentation
     * @param nline not being used really
     * 
     */
    public void oclPrintComponent(PrintWriter ps,int indent,boolean nline) {
	String pad = "";
	for(int i = 0;i < indent;i++)
	    pad = pad + " ";
	ps.print(pad + "sc(" + sort + "," + name + ",[");
	ListIterator li = pre.listIterator();
	while(li.hasNext()) {
	    ((oclPredicate)li.next()).oclPrintComponent(ps,0,false);
	    if(li.hasNext())
		 ps.print(",");
	}
	ps.print("]=>[");
	li = post.listIterator();
	while(li.hasNext()) {
	    ((oclPredicate)li.next()).oclPrintComponent(ps,0,false);
	    if(li.hasNext())
		 ps.print(",");
	}
	ps.print("])");
    }
}
