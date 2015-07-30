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

/** 
 * Top level class to store PDDL action representations
 *  deals with conditional-effects
 */

public class pddlOperator implements pddlPrint {
    public String opName;
    public List  param = new ArrayList();
    public List  vars = new ArrayList();
    public List  pre = new ArrayList();
    public List  effect = new ArrayList();
    public List  condEffects = new ArrayList();


    /**
     * Constructor
     * @param name - the name of the action as a String
     */
    public pddlOperator(String name) {
	opName = name;
    }

    /** 
     * Add a parameter to the action parameter list
     * @param var - the typed variable to add
     */
    public void addParam(pddlTypedVar var) {
	param.add(var);
    }

    /** 
     * Add a variable to the actions optional variable list
     * @param var - the typed variable to add
     */
    public void addVar(pddlTypedVar var) {
	vars.add(var);
    }  

    /** 
     * Add a precondition to an action
     * @param fname - the predicate name as a String
     * @param negated - indicator to determine if the predicate is negated
     * @return reference to newly added pddlPredicate
     */
    public pddlPredicate addPreCond(String fname, boolean negated) {
	pddlPredicate cur;
	if (negated)
	    cur = new pddlPredicate(fname,true);
	else
	    cur = new pddlPredicate(fname);
	pre.add(cur);
	return cur;
    }

    /** 
     * Add an effect predicate to an action
     * @param fname - the predicate name as a String
     * @param negated indicator to determine if the predicate is negated
     * @return reference to newly added pddlPrdicate
     */
    public pddlPredicate addEffect(String fname, boolean negated) {
	pddlPredicate cur;
	if (negated)
	    cur = new pddlPredicate(fname,true);
	else
	    cur = new pddlPredicate(fname);
	effect.add(cur);
	return cur;
    }

    /** 
     * Add a new when - conditional effect clause
     * @return reference to clause as pddlCondEffect
     */
    public pddlCondEffect addCondEffect () {
	pddlCondEffect cur = new pddlCondEffect();
	condEffects.add(cur);
	return cur;
    }

    /**
     * transToOcl
     * translate pddl action to ocl operator
     * ASSUME no or types at the moment
     * @param oclDom - the domain being constructed
     */
    public void transToOcl(oclDomain oclDom) {
	ListIterator li = pre.listIterator();
	List work = new ArrayList();
	oclOperator curOclOp = oclDom.addOP();
	oclPredicate curOclOpName = curOclOp.addName(opName);
	try {
	    while(li.hasNext()) {
		pddlPredicate cur = (pddlPredicate)li.next();
		work.add((pddlPredicate)cur.clone());
	    }
	} catch(CloneNotSupportedException e) {
	    Utility.debugPrintln("Unexpected failure to clone pddlPredicate.");
	    return;
	}
	while (work.size() > 0) {
	    List curClause = new ArrayList();
	    pddlPredicate curPred = (pddlPredicate)work.get(0);
	    Utility.debugPrintln("pddl","Current predicate: " + curPred.toString());
 	    pddlTypedVar mainParam = curPred.mainParam();
	    Utility.debugPrintln("pddl","Main param: "+mainParam.toString());
 	    curClause.add(curPred);
 	    work.remove(curPred);
 	    boolean found = filterListForObject(work,mainParam,curClause);
	    pddlPredicate.printPredicateList(curClause);
	    List curClauseRHS = new ArrayList();
	    if (usedIn(mainParam,effect,curClauseRHS)) {
		//create nec clause
		Utility.debugPrintln("pddl","Necessary RHS ");
		curOclOpName.addVarArgument(mainParam.transToOcl());
		if (!setTypeInfo(mainParam,oclDom)) {
		    Utility.debugPrintln("pddl","Parameter not defined in the parameter list");
		}
		String sort = convertToOclSort(mainParam);
		oclSC curSC = createSC(sort,mainParam.transToOcl(),
				       curClause,curClauseRHS);
		curOclOp.addNecSC(curSC);
	    } else {
		//create prevail clause
		Utility.debugPrintln("pddl","Prevail ");
 		curOclOpName.addVarArgument(mainParam.transToOcl());
 		if (!setTypeInfo(mainParam,oclDom)) {
 		    Utility.debugPrintln("pddl","Parameter not defined in the parameter list");
 		}
		String sort = convertToOclSort(mainParam);
 		oclSE curSE = createSE(sort,mainParam.transToOcl(),curClause);
 		curOclOp.addPrevSE(curSE);
	    }
	}
	checkEffectOrphans(oclDom,curOclOp,curOclOpName);
    }

    /**
     * setTypeInfo
     * collect the type info from the parameters list
     * if constant look for object in oclDomain
     * @param var - the variable to type
     * @param curOclDom - the domain being constructed
     * @return boolean - true if info present in param list
     */
    private boolean setTypeInfo(pddlTypedVar var,oclDomain curOclDom) {
	boolean found = false;
	if (var.isVar()) {
	    ListIterator li = param.listIterator();
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
	} else {
	    //This is a constant
	    String sort = curOclDom.getSortOfObject(var.transToOcl());
	    if (sort != null) {
		var.addType(sort);
		found = true;
	    }
	}
	return found;
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
     * createSC
     * create an ocl SC clause
     * @param  sort - the sort name
     * @param ID - the object id
     * @param lhs - LHS list of pddlPredicate clauses
     * @param rhs - RHS list of pddlPredicate clauses
     * @return oclSC - the created clause
     */
    private oclSC createSC(String sort, String ID, List lhs, List rhs) {
	oclSC curSC = new oclSC(sort,ID);
	ListIterator lilhs = lhs.listIterator();
	while(lilhs.hasNext()) {
	    pddlPredicate curPred = (pddlPredicate)lilhs.next();
	    curSC.addPre(curPred.transToOcl());
	    boolean found = false;
	    ListIterator lirhs = rhs.listIterator();
	    while(!found && lirhs.hasNext()) {
		pddlPredicate testPred = (pddlPredicate)lirhs.next();
		if (curPred.isNegOf(testPred)) {
		    found = true;
		}
	    }
	    if (!found) {
		// no negation in rhs this predicate must persist
		try {
		    curSC.addPost(((pddlPredicate)curPred.clone()).transToOcl());
		} catch (Exception e) {
		    Utility.debugPrintln("pddl","Unexpected failure to clone pddl predicate");
		}
	    }
	}
	ListIterator lirhs = rhs.listIterator();
	while(lirhs.hasNext()) {
	    pddlPredicate curPred = (pddlPredicate)lirhs.next();
	    curSC.addPost(curPred.transToOcl());
	    boolean found = false;
	    lilhs = lhs.listIterator();
	    while(!found && lilhs.hasNext()) {
		pddlPredicate testPred = (pddlPredicate)lilhs.next();
		if (curPred.isNegOf(testPred)) {
		    found = true;
		}
	    }
	    if (!found) {
		// no negation in lhs add it
		pddlPredicate curPredNeg = null;
		try {
		    curPredNeg = (pddlPredicate)curPred.clone();
		} catch (Exception e) {
		    Utility.debugPrintln("pddl","Unexpected failure to clone pddl predicate");
		}
		if (curPred.isNegated()) {
		    curPredNeg.setNegated(false);
		} else {
		    curPredNeg.setNegated(true);
		}
		curSC.addPre(curPredNeg.transToOcl());
	    }
	}
	return curSC;
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
	    Utility.debugPrintln("pddl",trans.toString());
	    curSE.addPredicate(curPred.transToOcl());
	    
	}
	return curSE;
    }
		

    /**
     * usedIn
     * test to see if parameter referenced as main paramater
     * in the predicates in the given List of predicates
     * collect the predicates using the parameter in the clause list
     * @param mainP - the main param to look for
     * @param given - list of pddlPredicates to check against
     * @param clause - the collected list of parameters
     * @return boolean - true if param referenced at least once
     */
    public boolean usedIn(pddlTypedVar mainP, List given, List clause) {
	boolean found = false;
	
	ListIterator li = given.listIterator();
	while (li.hasNext()) {
	    pddlPredicate curPred = (pddlPredicate)li.next();
	    pddlTypedVar curMainVar = curPred.mainParam();
	    if (mainP.equals(curMainVar)) {
		found = true;
		Utility.debugPrintln("pddl","Found match :" + curMainVar.toString());
		clause.add(curPred);
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
		Utility.debugPrintln("pddl","Found match :" + curMainVar.toString());
		clause.add(curPred);
	    } else {
		Utility.debugPrintln("pddl","Found other:" + curMainVar.toString());
		workCopy.add(curPred);
	    }
	}
	work.clear();
	work.addAll(workCopy);
	return found;
    }

    /**
     * checkEffectOrphans
     * check for objects refernced in effect clause but not in precondition
     * create an sc clause for all such
     * @param oclDom - the emerging domain
     * @param curOclOp - the op being defined
     * @param curOclOpName - opName
     */
    private void checkEffectOrphans(oclDomain oclDom,oclOperator curOclOp,
				    oclPredicate curOclOpName){
	ListIterator li = effect.listIterator();
	List work = new ArrayList();
	try {
	    while(li.hasNext()) {
		pddlPredicate cur = (pddlPredicate)li.next();
		work.add((pddlPredicate)cur.clone());
	    }
	} catch(CloneNotSupportedException e) {
	    Utility.debugPrintln("pddl","Unexpected failure to clone pddlPredicate.");
	    return;
	}
	while (work.size() > 0) {
	    List curClause = new ArrayList();
	    pddlPredicate curPred = (pddlPredicate)work.get(0);
 	    pddlTypedVar mainParam = curPred.mainParam();
 	    curClause.add(curPred);
 	    work.remove(curPred);
 	    boolean found = filterListForObject(work,mainParam,curClause);
	    List curClauseLHS = new ArrayList();
	    if (! usedIn(mainParam,pre,curClauseLHS)) {
		//create nec clause
		curOclOpName.addVarArgument(mainParam.transToOcl());
		if (!setTypeInfo(mainParam,oclDom)) {
		    Utility.debugPrintln("pddl","Parameter not defined in the parameter list");
		}
		String sort = convertToOclSort(mainParam);
		ListIterator liCur = curClause.listIterator();
		while(liCur.hasNext()) {
		    pddlPredicate oPred = (pddlPredicate)liCur.next();
		    pddlPredicate copyPred = null;
		    try {
			copyPred = (pddlPredicate)oPred.clone();
		    } catch (Exception e){
			Utility.debugPrintln("pddl","Unexpected failure to clone pddl predicate");
		    }
		    copyPred.setNegated(!copyPred.isNegated());
		    curClauseLHS.add(copyPred);
		}  
		oclSC curSC = createSC(sort,mainParam.transToOcl(),
				       curClauseLHS,curClause);
		curOclOp.addNecSC(curSC);
	    }
	}
    }

    /** Standard print routine for actions
     * @param ps - stream as a PrintWriter
     * @param indent - the level of left padding required - ignored by actions
     * @param nline - indicating if structure concludes with a new line - ignored
     */
    public void pddlPrintComponent(PrintWriter ps,int indent,boolean nline) {
	ps.print("  (:action ");
	ps.println(opName);
	ps.print("      :parameters (");
	ListIterator li = param.listIterator();
	while(li.hasNext()) {
	    ((pddlTypedVar)li.next()).pddlPrintComponent(ps,0,false);
	    if (li.hasNext())
		ps.print(" ");
	}
	ps.println(")");
	if (vars.size() > 0) {
	    ps.print("      :vars (");
	    li = vars.listIterator();
	    while(li.hasNext()) {
		((pddlTypedVar)li.next()).pddlPrintComponent(ps,0,false);
		if (li.hasNext())
		    ps.print(" ");
	    }	
	    ps.println(")");  
	}
	ps.print("      :precondition ");
	if (pre.size() > 1) {
	    ps.print("(and ");
	    li = pre.listIterator();
	    while(li.hasNext()) {
		((pddlPredicate)(li.next())).pddlPrintComponent(ps,0,false);
		if (li.hasNext())
		    ps.print(" ");
	    }
	    ps.print(")");
	} else {
	    li = pre.listIterator();
	    if(li.hasNext())
		((pddlPredicate)(li.next())).pddlPrintComponent(ps,0,false);
	}
	ps.println("");
	ps.print("      :effect ");
	if (effect.size() > 1) {
	    ps.print("(and ");
	    li = effect.listIterator();
	    while(li.hasNext()) {
		((pddlPredicate)(li.next())).pddlPrintComponent(ps,0,false);
		if (li.hasNext())
		    ps.print(" ");
	    }
	    ps.print(")");
	} else {
	    li = effect.listIterator();
	    if(li.hasNext())
		((pddlPredicate)(li.next())).pddlPrintComponent(ps,0,false);
	}
	li = condEffects.listIterator();
	while(li.hasNext()) {
	    ((pddlCondEffect)(li.next())).pddlPrintComponent(ps,12,true);
	}
	ps.println(")");
    }
}
