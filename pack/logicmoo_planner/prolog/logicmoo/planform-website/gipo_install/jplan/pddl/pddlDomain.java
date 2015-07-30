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
 * pddlDomain : Top level storage class for a PDDL representation of a
 *  planning domain model.
 * @author Ron Simpson
 * @version 0.0
 *
 */
public class pddlDomain implements pddlPrint{

    private String name;
    public List requirements = new ArrayList();
    public List types = new ArrayList();
    public List constants = new ArrayList();
    public List predicates = new ArrayList();
    public List actions = new ArrayList();
    public List tasks = new ArrayList();

    /**
     * constructor
     * just sets a default domain name "pddlDefault"
     */
    public pddlDomain() {
	name = new String("pddlDefault");
    }

    /** Use to store the name of the domain.
     * @param n - name as a String
     */
    public void setName(String n) {
	name = new String(n);
    }

    /** 
     * Fetch the domain name.
     * @return name as a String
     */
    public String getName() {
	return name;
    }
    /** 
     * Add a domain processing requirement e.g :strips :typing etc
     * Currently only strips, typing and equality supported.
     * @param req the requirement name as a String
     */
    public void addRequirement(String req) {
	requirements.add(req);
    }
    
    /** 
     *Add a constant object to the domain e.g. pump, wrench 
     * in the flat-tyre-world
     * @param con - the name of the constant as a String
     */
    public void addConstant(pddlTypedVar con) {
	constants.add(con);
    }

    /** 
     * Add a predicate to the predicate section of the domain.
     * @param pred - The predicate as a pddlPredicate Object
     */
    public void addPredicate(pddlPredicate pred) {
	predicates.add(pred);
    }

    /** 
     * Fetch the types list of the domain. Domain should support typing
     * though this is not checked.
     * @return reference to the types List as a List
     */
    public List getPDDLTypeList() {
	return types;
    }

    /** 
     * Add an action/operator to the domain.
     * @param name - The name of the action as a String
     * @return a reference to the newly added action.
     */
    public pddlOperator addOP(String name) {
	pddlOperator cur = new pddlOperator(name);
	actions.add(cur);
	return cur;
    }
    /** 
     * Add a task to the domain.
     * @param ID the name of the problem/task as a String
     * @param DomName - The name of the domain to which this task belongs.
     * @return  A reference to the newly added Task.
     */
    public pddlTask addTask(String ID,String DomName) {
	pddlTask cur = new pddlTask(ID,DomName);
	tasks.add(cur);
	return cur;
    }

    /**
     * convertToOcl
     * convert a successfully loaded PDDL domain (strips typing
     * equality conditional effects) to OCL internal representation
     * @return oclDomain - the resulting domain
     */
    public oclDomain convertToOcl() {
	oclDomain oclDom = new oclDomain(false);
	oclDom.setName(name);
	oclSort prims = oclDom.addSort("primitive_sorts");
	ListIterator li = types.listIterator();
	while (li.hasNext()) {
	    prims.addSubType((String)li.next());
	}
	li = constants.listIterator();
	// Make types for each constant and an object instance for it
	// If type not defined assume "object"
	while(li.hasNext()) {
	    pddlTypedVar curVar = (pddlTypedVar)li.next();
	    String constName = curVar.getvName().toLowerCase();
	    List varTypeList = curVar.getTypeList();
	    ListIterator liTVL = varTypeList.listIterator();
	    String varType = "";
	    // constant can only be of one type
	    if (liTVL.hasNext()) {
		varType = (String)liTVL.next();
		Utility.debugPrintln("Pddl var " + constName + "-" + varType);
		if (!Utility.listContainsString(varType,prims.getSubTypes())) {
		    prims.addSubType(varType);
		}
	    } else {
		// assume type object
		varType = "object";
		if (!Utility.listContainsString(varType,prims.getSubTypes())) {
		    prims.addSubType(varType);
		}
	    }
	    oclObject curObj  = oclDom.getObjectDefOfSort(varType);
	    if (curObj == null) {
		curObj = oclDom.addObject(varType);
	    }
	    curObj.addObjID(constName);
	}
	// Now deal with the predicates
	transPredicatesToOcl(oclDom);
	// Now try and deal with actions
	ListIterator liActs = actions.listIterator();
	while (liActs.hasNext()) {
	    pddlOperator curOp = (pddlOperator)liActs.next();
	    curOp.transToOcl(oclDom);
	}
	// Now the tasks
	ListIterator liTasks = tasks.listIterator();
	while (liTasks.hasNext()) {
	    pddlTask curTask = (pddlTask)liTasks.next();
	    oclTask curOclTask = curTask.transToOcl(oclDom);
	    oclDom.tasks.add(curOclTask);
	}
	// Now at last try and form substate class definitions
	// Use the already translated operators and tasks
	ListIterator liOps = oclDom.operators.listIterator();
	while (liOps.hasNext()) {
	    oclOperator curOp = (oclOperator)liOps.next();
	    curOp.extractSubStateDefs(oclDom);
	}
	// Update the predicates list to contain negations 
	// add negation of each predicate
	Utility.debugPrintln("pddl","Update predicate list");
	ListIterator liPreds = oclDom.predicates.listIterator();
	List temp = new ArrayList();
	while (liPreds.hasNext()) {
	    Utility.debugPrintln("pddl","Predicates = " + oclDom.predicates.size());
	    oclPredicate cur = (oclPredicate)liPreds.next();
	    Utility.debugPrintln("pddl","Predicate = "+ cur.toString()); 	
	    if (!cur.getName().startsWith("not_")) {
		oclPredicate neg = null;
		try {
		    neg = (oclPredicate)cur.clone();
		} catch (Exception e) {
		    System.err.println("Unexpected failure to clone predicate [pddlDomain]");
		    break;
		}
		String name = "not_".concat(neg.getName());
		neg.setName(name);
		temp.add(neg);
	    }
	}
	oclDom.predicates.addAll(temp);
	return oclDom;
    }

    /**
     * transPredicatesToFlatOcl
     * Now deal with the predicates - for or types flatten
     * create a new predicate for each type combination
     * make the predicate name reflect the types
     * pred(o - a or b) becomes
     * preda(a)
     * predb(b)
     * NOTE: Not currently used
     * @param oclDom - the emerging domain definition
     */
    private void transPredicatesToFlatOcl(oclDomain oclDom) {
	ListIterator li = predicates.listIterator();
	while (li.hasNext()) {
	    pddlPredicate curPred = (pddlPredicate)li.next();
	    String predName = curPred.getName();
	    List oclPredList = new ArrayList();
	    oclPredicate oclPred = new oclPredicate(predName);
	    oclPredList.add(oclPred);
	    ListIterator liArgs = curPred.getArgs().listIterator();
	    while (liArgs.hasNext()) {
		pddlTypedVar curArg = (pddlTypedVar)liArgs.next();
		List argTypes = curArg.getTypeList();
		if (argTypes.size() > 1) {
		    boolean first = true;
		    List oldPreds = new ArrayList();
		    ListIterator liTypes = argTypes.listIterator();
		    while (liTypes.hasNext()) {
			String typeName = (String)liTypes.next();
			
			if (first) {
			    Utility.debugPrintln("In first ");
			    // change names of existing preds in list
			    ListIterator liPreds = oclPredList.listIterator();
			    while (liPreds.hasNext()) {
				
				oclPredicate cur = 
				    (oclPredicate)liPreds.next();
				Utility.debugPrintln("In First " + cur.toString());
				try {
				    oldPreds.add(cur.clone());
				} catch (CloneNotSupportedException e) {
				    Utility.debugPrintln("Unexpected failure to clone predicate.");
				}
				cur.setName(cur.getName() + typeName);
				cur.addVarArgument(typeName);
			    }
			    first = false;
			} else {
			    Utility.debugPrintln("In Second " + oldPreds.size());
			    ListIterator liOldPreds = oldPreds.listIterator();
			    while (liOldPreds.hasNext()) {
				oclPredicate cur =
				     (oclPredicate)liOldPreds.next();
				oclPredicate copy = null;
				try {
				    copy = (oclPredicate)cur.clone();
				} catch (CloneNotSupportedException e) {
				    Utility.debugPrintln("Unexpected failure to clone predicate.");
				}
				copy.setName(copy.getName() + typeName);
				copy.addVarArgument(typeName);
				oclPredList.add(copy);
				Utility.debugPrintln("In Second " + copy.toString());
			    }
			}
			    

		    }
		} else {
		    ListIterator liPreds = oclPredList.listIterator();
		    while (liPreds.hasNext()) {
			oclPredicate cur = 
			    (oclPredicate)liPreds.next();
			cur.addVarArgument((String)argTypes.get(0));
		    }
		}   
	    }
	    ListIterator liPreds = oclPredList.listIterator();
	    while (liPreds.hasNext()) {
		oclPredicate cur = (oclPredicate)liPreds.next();
		oclDom.addCompletePredicate(cur);
	    }
	}
	// End translate predicates
    }

    /**
     * transPredicatesToOcl
     * translate the predicate info into ocl
     * one to one translation in this version
     * strategy - primitive types are already created or types
     * must be created by concatinating type names 
     * - sorted to ensure consistency
     * @param oclDom - the emerging domain definition
     */
    private void transPredicatesToOcl(oclDomain oclDom) {
	ListIterator li = predicates.listIterator();
	while (li.hasNext()) {
	    pddlPredicate curPred = (pddlPredicate)li.next();
	    String predName = curPred.getName();
	    List oclPredList = new ArrayList();
	    oclPredicate oclPred = new oclPredicate(predName);
	    oclPredList.add(oclPred);
	    ListIterator liArgs = curPred.getArgs().listIterator();
	    while (liArgs.hasNext()) {
		pddlTypedVar curArg = (pddlTypedVar)liArgs.next();
		List argTypes = curArg.getTypeList();
		if (argTypes.size() > 1) {
		    String sort = createHierSort(curArg,oclDom);
		    oclPred.addConstArgument(sort);
		} else {
		    oclPred.addConstArgument((String)argTypes.get(0));
		}   
	    }
	    oclDom.addCompletePredicate(oclPred);
	}
    }

    /**
     * createHierSort
     * create a hierarchical sort to match the pddl 'or' sort
     * concate the sort names to create the new sort but order "sort"
     * them to ensure that unnecessary duplicates are not created.
     * @param curVar the info for the sort to create
     * @param oclDom - the emerging ocl domain
     * @return String the sort name
     */
    private String createHierSort(pddlTypedVar curVar, oclDomain oclDom) {
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
	// Now check if already exists in the domain
	if (! oclDom.isSort(sort)) {
	    oclSort curSort = oclDom.addSort(sort);
	    li = curVar.getTypeList().listIterator();
	    while (li.hasNext()) {
		String type = (String)li.next();
		curSort.addSubType(type);
	    }
	}
	return sort;
    }

    /** 
     * Top level printing routine to print out a text representation of the
     * domain in cannonical form. This routine calls pddlPrint on all
     * subsuiduary elements in the pddl domain.
     * @param ps A PrintWriter stream to print to.
     * @param indent ignored at top level
     * @param nline ignored at top level
     */
    public void pddlPrintComponent(PrintWriter ps,int indent,boolean nline) {
	ps.println(";; Automatically generated PDDL Domain");
	ps.print("(define (domain ");
	ps.println(name + ")");
	ps.print("  (:requirements ");
	ListIterator li = requirements.listIterator();
	while(li.hasNext()) {
	    ps.print(((String)li.next()));
	    if (li.hasNext())
		ps.print(" ");
	}
	ps.println(")");
	if (types.size() > 0) {
	    ps.print("  (:types ");
	    li = types.listIterator();
	    while(li.hasNext()) {
		ps.print(((String)li.next()));
		if (li.hasNext())
		    ps.print(" ");
	    }
	    ps.println(")");
	}
	if (constants.size() > 0) {
	    ps.print("  (:constants ");
	    li= constants.listIterator();
	    while(li.hasNext()) {
		((pddlTypedVar)li.next()).pddlPrintComponent(ps,0,false);
		if (li.hasNext())
		    ps.print(" ");
	    }
	    ps.println(")");
	}
	ps.println("  (:predicates");
	li= predicates.listIterator();
	while(li.hasNext()) {
	    ((pddlPredicate)li.next()).pddlPrintComponent(ps,4,true);
	}
	ps.println("    )");
	li = actions.listIterator();
	while(li.hasNext()) {
	    ((pddlOperator)li.next()).pddlPrintComponent(ps,2,true);
	}	
	ps.println(")");
	li = tasks.listIterator();
	while(li.hasNext()) {
	    ((pddlTask)li.next()).pddlPrintComponent(ps,indent,nline);
	}
    }
}
