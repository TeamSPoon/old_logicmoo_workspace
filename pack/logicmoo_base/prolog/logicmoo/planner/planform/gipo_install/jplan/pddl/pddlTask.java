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

import java.util.List;
import java.util.ArrayList;
import java.util.ListIterator;
import java.io.PrintWriter;

import jplan.ocl.*;
import jplan.general.Utility;

/** pddlTask : Top level storage class for a PDDL representation of a
 *  planning domain Task/Problem.
 * @author Ron Simpson
 * @version 0.0
 *
 */
public class pddlTask implements pddlPrint {
    /** The task name.
     */
    public String tName;
    /** The domain name for this task.
     */
    public String dName;
    /** The task length in a parallel plan.
     */
    public int    pLength = 0;
    /** The task length in a serial plan.
     */
    public int    sLength = 0;
    /** The Objects referred to in this task.
     */
    public List  objects = new ArrayList();
    /** The Innitial state.
     */
    public List  inits = new ArrayList();
    /** The goal predicates to be achieved.
     */
    public List  goals = new ArrayList();

    /** Constructor
     * @param name of the task as a String
     * @param dom -  domain name to which this task applies as a String
     */
    public pddlTask(String name,String dom) {
	tName = name;
	dName = dom;
	pLength = 0;
	sLength = 0;
    }

    /** Adds an object description to the Objects section of a PDDL task
     * @param obj - object description as a pddlTypedVar
     */
    public void addObject(pddlTypedVar obj) {
	objects.add(obj);
    }
 
    /** Adds a predicate to the initial section of a PDDL task
     * @param fname the predicate / functor name as a String
     * @return reference to added PDDL predicate
     */
    public pddlPredicate addInit(String fname) {
	pddlPredicate cur = new pddlPredicate(fname);
	inits.add(cur);
	return cur;
    } 

    /** Adds a predicate to the goal section of a PDDL task
     * @param fname the predicate / functor name as a String
     * @param negated boolean flag indicatin if predicated is negated
     * @return reference to added PDDL predicate
     */
    public pddlPredicate addGoal(String fname, boolean negated) {
	pddlPredicate cur = new pddlPredicate(fname,negated);
	goals.add(cur);
	return cur;
    } 

    /** Sets the optimal parallel length of a task
     */
    public void setParallelLength(int len){
	pLength = len;
    }

    /** Sets the optimal serial length of a task
     */
    public void setSerialLength(int len){
	sLength = len;
    }

    /**
     * transToOcl
     * translate the task to ocl form
     * maintain or types
     * @param oclDom - the emerging ocl domain
     * @return oclTask - the translated task
     */
    public oclTask transToOcl(oclDomain oclDom) {
	// First add the objects in this task to the objects list
	ListIterator li = objects.listIterator();
	while (li.hasNext()) {
	    pddlTypedVar curObj = (pddlTypedVar)li.next();
	    String sort = convertToOclSort(curObj);
	    if (!oclDom.isObjectOfSort(curObj.getvName() ,sort)) {
		
		oclObject curOclObj = oclDom.getObjectDefOfSort(sort);
		if (curOclObj == null) {
		    curOclObj = oclDom.addObject(sort);
		}
		curOclObj.addObjID(curObj.transToOcl());
	    }
	}
	oclTask curOclTask = new oclTask(tName);
	List work = new ArrayList();
	li = inits.listIterator();
	while (li.hasNext()) {
	    pddlPredicate curPred = (pddlPredicate)li.next();
	    try {
		work.add(curPred.clone());
	    } catch (Exception e) {
		Utility.debugPrintln("Unexpected failure to clone pddl predicate.");
	    }
	}
	while (work.size() > 0) {
	    List curClause = new ArrayList();
	    pddlPredicate curPred = (pddlPredicate)work.get(0);
	    pddlTypedVar mainParam = curPred.mainParam();
	    Utility.debugPrintln("Main param: "+mainParam.toString());
 	    curClause.add(curPred);
 	    work.remove(curPred);
 	    boolean found = filterListForObject(work,mainParam,curClause);
	    pddlPredicate.printPredicateList(curClause);
	    //create ss clause
	    Utility.debugPrintln("Task Init ss ");
	    if (!setTypeInfo(mainParam,oclDom)) {
		Utility.debugPrintln("Parameter not defined in the objects list");
	    }
	    String sort = convertToOclSort(mainParam);
	    oclSS curSS = createSS(sort,mainParam.transToOcl(),curClause);
	    curOclTask.addInitSS(curSS);
	}
	transGoalStates(oclDom,curOclTask);
	return curOclTask;
    }

    /**
     * transGoalStates
     * translate the goals
     * @param oclDom the developing domain
     * @param curOclTask the task being defined
     */
    private void transGoalStates(oclDomain oclDom,oclTask curOclTask){
	List work = new ArrayList();
	ListIterator li = goals.listIterator();
	while (li.hasNext()) {
	    pddlPredicate curPred = (pddlPredicate)li.next();
	    try {
		work.add(curPred.clone());
	    } catch (Exception e) {
		Utility.debugPrintln("Unexpected failure to clone pddl predicate.");
	    }
	}
	while (work.size() > 0) {
	    List curClause = new ArrayList();
	    pddlPredicate curPred = (pddlPredicate)work.get(0);
	    pddlTypedVar mainParam = curPred.mainParam();
	    Utility.debugPrintln("Main param: "+mainParam.toString());
 	    curClause.add(curPred);
 	    work.remove(curPred);
 	    boolean found = filterListForObject(work,mainParam,curClause);
	    pddlPredicate.printPredicateList(curClause);
	    //create ss clause
	    Utility.debugPrintln("Task Init se ");
	    if (!setTypeInfo(mainParam,oclDom)) {
		Utility.debugPrintln("Parameter not defined in the objects list");
	    }
	    String sort = convertToOclSort(mainParam);
	    oclSE curSE = createSE(sort,mainParam.transToOcl(),curClause);
	    curOclTask.addGoalSE(curSE);
	}
    }


    /**
     * convertToOclSort
     * create a hierarchical sort to match the pddl 'or' sort
     * concate the sort names to create the new sort but order "sort"
     * them to ensure that unnecessary duplicates are not created.
     * @param curVar the info for the sort to create
     * @return String the sort name
     */
    private String convertToOclSort(pddlTypedVar curVar) {
	ListIterator li = curVar.getTypeList().listIterator();
	List hSort = new ArrayList();
	// First build the sort name
	while (li.hasNext()) {
	    String type = (String)li.next();
	    hSort = Utility.addInOrder(hSort,type);
	}
	String sort = new String("");
	li = hSort.listIterator();
	while (li.hasNext()) {
	    String s = (String)li.next();
	    sort = sort.concat(s);
	    if (li.hasNext()) {
		sort = sort.concat("-");
	    }
	}
	return sort;
    }

    /**
     * setTypeInfo
     * collect the type info from the objects list
     * if constant look for object in oclDomain
     * @param var - the variable to type
     * @param curOclDom - the domain being constructed
     * @return boolean - true if info present in param list
     */
    private boolean setTypeInfo(pddlTypedVar var,oclDomain curOclDom) {
	boolean found = false;
	ListIterator li = objects.listIterator();
	while (!found && li.hasNext()) {
	    pddlTypedVar cur = (pddlTypedVar)li.next();
	    if (var.getvName().equals(cur.getvName())) {
		ListIterator liTypes = cur.getTypeList().listIterator();
		while (liTypes.hasNext()) {
		    String type = (String)liTypes.next();
		    var.addType(type);
		}
		found = true;
	    }
	}
	if (!found) {
	    //Perhaps This is a constant
	    String sort = curOclDom.getSortOfObject(var.transToOcl());
	    if (sort != null) {
		var.addType(sort);
		found = true;
	    }
	}
	return found;
    }

    /**
     * filterListForObject
     * collect all predicates with same given main parameter
     * remove them from the main list
     * @param work -pddlPredicate list to search
     * @param mainParam - the given main parameter
     * @param clause - clause being constructed contains matching predicates
     * @return boolean - true if > 0 predicates found
     */
    private boolean filterListForObject(List work,pddlTypedVar mainParam,
					List clause) {
	boolean found = false;
	List workCopy = new ArrayList();
	
	ListIterator li = work.listIterator();
	while (li.hasNext()) {
	    pddlPredicate curPred = (pddlPredicate)li.next();
	    pddlTypedVar curMainVar = curPred.mainParam();
	    if (mainParam.equals(curMainVar)) {
		Utility.debugPrintln("Found match :" + curMainVar.toString());
		clause.add(curPred);
	    } else {
		Utility.debugPrintln("Found other:" + curMainVar.toString());
		workCopy.add(curPred);
	    }
	}
	work.clear();
	work.addAll(workCopy);
	return found;
    }

    /**
     * createSS
     * create an ocl SS clause
     * @param sort sort - the sort name
     * @param ID id - the object id
     * @param lhs - list of pddlPredicate clauses
     * @return oclSS - the created clause
     */
    private oclSS createSS(String sort,String ID, List lhs) {
	oclSS curSS = new oclSS(sort,ID);
	ListIterator lilhs = lhs.listIterator();
	while(lilhs.hasNext()) {
	    pddlPredicate curPred = (pddlPredicate)lilhs.next();
	    oclPredicate trans = curPred.transToOcl();
	    Utility.debugPrintln(">> " + trans.toString());
	    curSS.addPredicate(curPred.transToOcl());
	    
	}
	return curSS;
    }

    /**
     * createSE
     * create an ocl SE clause
     * @param  sort - the sort name
     * @param ID - the object id
     * @param lhs - LHS list of pddlPredicate clauses
     * @return oclSE - the created clause
     */
    private oclSE createSE(String sort,String ID, List lhs) {
	oclSE curSE = new oclSE(sort,ID);
	ListIterator lilhs = lhs.listIterator();
	while(lilhs.hasNext()) {
	    pddlPredicate curPred = (pddlPredicate)lilhs.next();
	    oclPredicate trans = curPred.transToOcl();
	    Utility.debugPrintln(trans.toString());
	    curSE.addPredicate(curPred.transToOcl());
	    
	}
	return curSE;
    }
    
    /** Top level printing routine to print out a text representation of the
     * domain task in cannonical form. This routine calls pddlPrint on all
     * subsuiduary elements in the pddl task.
     * @param ps A PrintWriter stream to print to.
     * @param indent ignored at top level
     * @param nline ignored at top level
     */
    public void pddlPrintComponent(PrintWriter ps,int indent,boolean nline) {
	ListIterator li;
	pddlTypedVar v;

	ps.print("(define (problem  ");
	ps.println(tName + ")");
	ps.println("    (:domain " + dName + ")");
	if (objects.size() > 0) {
	    ps.print("    (:objects ");
	    li = objects.listIterator();
	    while(li.hasNext()) {
		v = (pddlTypedVar)(li.next());
		ps.print( v.getvName() );
		if ((v.getTypeList()).size() > 0) {
		    ps.print( " - ");
		    ListIterator li2 = (v.getTypeList()).listIterator();
		    while(li2.hasNext()) {
			ps.print((String)li2.next());
			if (li2.hasNext())
			    ps.print(" ");
		    }
		}   
		if (li.hasNext())
		    ps.print(" ");
	    }
	    ps.println(")");
	}
	ps.println("    (:init");
	li = inits.listIterator();
	while(li.hasNext()){
	    ((pddlPredicate)li.next()).pddlPrintComponent(ps,8,false);
	    if (li.hasNext())
		ps.println("");
	}
	ps.println(")");
	ps.println("    (:goal");
	if (goals.size() > 1) {
	    ps.println("      (and");
	    li = goals.listIterator();
	    while(li.hasNext()){
		((pddlPredicate)li.next()).pddlPrintComponent(ps,8,false);
		if (li.hasNext())
		    ps.println("");
	    }
	    ps.print("))");
	} else {
	    li = goals.listIterator();
	    while(li.hasNext()){
		((pddlPredicate)li.next()).pddlPrintComponent(ps,8,false);
		if (li.hasNext())
		    ps.println("");
	    } 
	    ps.print(")"); 
	}
	if (pLength > 0 || sLength > 0) {
	    ps.print("    (:length ");
	    if(sLength > 0 )
		ps.print("(:serial " + sLength + ")");
	    if(pLength >0)
		ps.print("(:parallel " + pLength + ")");
	    ps.print(")");
	}
	ps.println(")");
    }
}
	
