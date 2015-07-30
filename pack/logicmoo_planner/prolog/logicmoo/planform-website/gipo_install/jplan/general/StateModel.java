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
 * Class StateModel
 * This class stores the Model of the SubState Class that
 * is being edited - it helps in the process of mapping
 * from the oclPredicate view to the String text view of the editor
 * It stores both the predicates being edited and the prototype
 * for the predicate.
 * @author Ron
 * @ version 0
 */

import javax.swing.*;
import java.util.List;
import java.util.*;

import jplan.ocl.*;
import jplan.general.*;

public class StateModel {
    private List predList;         // This is the complete predicates list
    private List statePredList;    // The predicates in this state
    public DefaultListModel neList;   // The variable binding constraints
    // this state

    /**
     * constructor
     * @param predList - reference to the complete predicate list
     */ 
    public StateModel(List predList) {
	this.predList = predList;   //This is the complete list of predicates 
	                            // defined
	statePredList = new ArrayList();
	neList = new DefaultListModel();
    }

    /**
     * addPredicate
     * @param cur - The predicate to added in the edit window
     * @param offset - The start offset for the predicate in the edit window
     * @return The PredDetail created;
     * Searches for the prototype matching this predicate
     * Throws an StateModelException exception if not found.
     */
    public PredDetail addPredicate(oclPredicate cur,int offset) 
	throws StateModelException{
	try {
	    if ("ne".equals(cur.getName())) {
		neList.addElement((oclPredicate)cur.clone());
		return null;
	    } else if ("is_of_sort".equals(cur.getName())) {
		neList.addElement((oclPredicate)cur.clone());
		return null;
	    }
	} catch (CloneNotSupportedException e) {
	    Utility.debugPrintln("Unexpected clone failure - addPredicate");
	    // Should not fail
	    throw new StateModelException("Could not clone ne Predicate.");
	}    
	PredDetail curPredDetail = null;
	try {
	    Utility.debugPrintln("Start Offset " + offset);
	    curPredDetail = new PredDetail((oclPredicate)cur.clone(),offset);
	    statePredList.add(curPredDetail);
	} catch (CloneNotSupportedException e) {
	    Utility.debugPrintln("Unexpected clone failure - addPredicate");
	    // Should not fail
	    throw new StateModelException("Could not clone Predicate.");
	}
	boolean found = false;
	ListIterator li = predList.listIterator();
	while (li.hasNext() && !found) {
	    oclPredicate curProto = (oclPredicate)li.next();
	    if(curProto.getName().equals(cur.getName()) &&
	       curProto.size() == cur.size()) {
		curPredDetail.proto = curProto;
 		curPredDetail.pred.setStatic(curPredDetail.proto.isStatic());
 		found = true;
 	    }
	}
//          Unifying is too strict does not deal with hierarchical types
// 	    if (curProto.unify(cur,new OEnviron())) {
// 		curPredDetail.proto = curProto;
// 		curPredDetail.pred.setStatic(curPredDetail.proto.isStatic());
// 		found = true;
// 	    }
//	}
	if (!found) {
	    throw new StateModelException("No such Prototype Predicate.");
	}
	return curPredDetail;
    }

    /**
     * addSortedPredicate
     * @param cur - The predicate to added in the edit window
     * @param offset - The start offset for the predicate in the edit window
     * @return The PredDetail created;
     * Uses the sorts provided rather than prototype
     */
    public PredDetail addSortedPredicate(oclPredicate cur,int offset) 
	throws StateModelException{  
	PredDetail curPredDetail = null;
	try {
	    Utility.debugPrintln("Start Offset " + offset);
	    curPredDetail = new PredDetail((oclPredicate)cur.clone(),offset);
	    statePredList.add(curPredDetail);
	} catch (CloneNotSupportedException e) {
	    Utility.debugPrintln("Unexpected clone failure - addPredicate");
	    // Should not fail
	    throw new StateModelException("Could not clone Predicate.");
	}
	curPredDetail.proto = cur.getProtoType();
	return curPredDetail;
    }

    /**
     * editVar - change a variable name
     * propegate change to stored document offsets
     * @param startOffset - start offset of variable - relative to document start
     * @param newVarName -  the new variable name
     * @return String - the old variable name
     */
    public String editVar(int startOffset,String newVarName) throws Exception,
	StateModelException{
	String oldVName = "none";
	int indexPos = 0;
	int changeInc = 0;
	ListIterator li = statePredList.listIterator();
	boolean found = false;
	Utility.debugPrintln("\neditVar given " + startOffset + " " + newVarName);
	while (li.hasNext()) {
	    PredDetail cur = (PredDetail)li.next();
	    Utility.debugPrintln("Start offset = " + indexPos);
	    if (startOffset >= indexPos && 
		startOffset <= indexPos + cur.length) {
		//Found the predicate now replace the variable
		oldVName = cur.pred.elementAt(startOffset - indexPos);
		changeInc = 
		    cur.pred.replaceVariableAt(startOffset - indexPos,newVarName);
		
		// start has not changed
		Utility.debugPrintln("Replaced change inc = " + changeInc);
		cur.length = cur.pred.toString().length();
		//Now update the following predicate start offsets
		found = true;
		indexPos += (cur.length  + 1) - changeInc; //use old length
	    } else if (found) {
		cur.startOffset += changeInc;
		indexPos += cur.length + 1; // +1 for return
	    } else {
		indexPos += cur.length + 1; // +1 for return
	    } 
	}
	if (!found) {
	    throw new StateModelException("\nVariable not found");
	}
	return oldVName;
    }

    /**
     * getPredicateAt - find the predicate containing the text location
     * @param pos text position
     * @ return the oclPredicate
     */
    public oclPredicate getPredicateAt(int pos) throws StateModelException {
	ListIterator li = statePredList.listIterator();
	while (li.hasNext()) {	
	    PredDetail cur = (PredDetail)li.next();
	    Utility.debugPrintln("Start = " + cur.startOffset +
			       " length = " + cur.length);
	    if (pos >= cur.startOffset && 
		pos <= cur.startOffset + cur.length) {
		return cur.pred;
	    } 
	}
	throw new StateModelException("No such predicate");
    }

    /**
     * This version returns the whole pred detail
     */
    public PredDetail getPredDetailAt(int pos) throws StateModelException {
	ListIterator li = statePredList.listIterator();
	while (li.hasNext()) {
	    PredDetail cur = (PredDetail)li.next();
	    Utility.debugPrintln("Start = " + cur.startOffset +
			       " length = " + cur.length);
	    if (pos >= cur.startOffset &&
		pos <= cur.startOffset + cur.length) {
		return cur;
	    }
	}
	throw new StateModelException("No such predicate");
    }

    /**
     * getArgumentAt - find the predicate argument at the
     * given position
     * @param  pos - the text position
     * @return String - the argument name
     */
    public String getArgumentAt(int pos) throws StateModelException,Exception {
	int indexPos = 0;
	PredDetail pd;
	String res;
	try {
	    pd = getPredDetailAt(pos);
	} catch (Exception e) {
	    throw new StateModelException("No predicate here");
	}
	try {
	    res = pd.pred.elementAt(pos - pd.startOffset);
	} catch (Exception e) {
	    throw e;
	}
	return res;
    }

    /**
     * removePredicateAt - remove the predicate containing pos
     * @param  pos
     * @return List - PredDetail(s) for static predicates beyond 
     *removed predicate
     */
    public List removePredicateAt(int pos) throws StateModelException {
	List res = new ArrayList();
	ListIterator li = statePredList.listIterator();
	int count = 0;
	int index = 0;
	int decrement = 0;
	boolean found = false;
	while (li.hasNext()) {
	    PredDetail cur = (PredDetail)li.next();
	    Utility.debugPrintln("Start = " + cur.startOffset +
			       " length = " + cur.length);
	    if (pos >= cur.startOffset &&
		pos <= cur.startOffset + cur.length) {
		found = true;
		index = count;
		decrement = cur.length + 1;
	    } else if (found) {
		Utility.debugPrintln("decrementing from = " +
				   cur.startOffset +
				   " by " + decrement);
		cur.startOffset -= decrement;
		if (cur.proto.isStatic()) {
		    res.add(cur);
		}
	    }
	    count++;
	}
	if (! found) {
	    throw new StateModelException("No such predicate");
	} else {
	    statePredList.remove(index);
	    return res;
	}
	    
    }

    /**
     * neExists - check to see if a ne clause with the given variable
     * name exists
     * @param vName1 - Variable name 1
     * @param vName2 - Variable name 2
     * @return index position if exists otherwise -1
     */
    public int neExists(String vName1,String vName2) {
	int size = neList.getSize();
	int index = 0;
	while (index < size) {
	    oclPredicate cur = (oclPredicate)neList.elementAt(index);
	    List args = cur.getArguments();
	    ListIterator liArgs = args.listIterator();
	    boolean match = true;
	    while (match && liArgs.hasNext()) {
		String argName = ((OPredicate.pArg)liArgs.next()).name;
		if (!argName.equals(vName1) && !argName.equals(vName2)) {
		    match = false;
		}
	    }
	    if (match) { // Found it
		return index;
	    }
	    index++;
	}
	return -1;
    }

    /**
     * addNE - add a ne clause 
     * @param vName1 - Variable name 1
     * @param vName2 - Variable name 2
     * @return oclPredicate - the ne predicate constructed 
     */
    public oclPredicate addNE(String vName1, String vName2) {
	oclPredicate ne = new oclPredicate("ne");
	ne.addVarArgument(vName1);
	ne.addVarArgument(vName2);
	neList.addElement(ne);
	return ne;
    }

    /**
     * illegalNEs - check in turn each ne 
     * @return A List of indexes of the illegal nes
     */
    public List illegalNEs () {
	List ans = new ArrayList();
	int inx = 0;
	while(inx < neList.getSize()) {
	    if (! checkNE((oclPredicate)neList.get(inx))) {
		ans.add(new Integer(inx));
	    }
	    inx++;
	}
	return ans;
    }
	

    /**
     * checkNE - check that both variables in the ne clause still
     * occur in some predicate in the state list
     * @param ne - the ne predicate
     * @return boolean true if ne ok false otherwise
     */
    public boolean checkNE(oclPredicate ne) {
	List args = ne.getArguments();
	String arg1 = (String)((oclPredicate.pArg)args.get(0)).name;
	String arg2 = (String)((oclPredicate.pArg)args.get(1)).name;
	boolean foundArg1 = false;
	boolean foundArg2 = false;
	ListIterator li = statePredList.listIterator();
	while(li.hasNext()) {
	    List pargs = ((PredDetail)li.next()).pred.getArguments();
	    ListIterator liPargs = pargs.listIterator();
	    while (liPargs.hasNext()) {
		String curArg = ((oclPredicate.pArg)liPargs.next()).name;
		if (! foundArg1) {
		    if( arg1.equals(curArg)) {
			foundArg1 = true;
		    }
		}
		if (! foundArg2) {
		    if( arg2.equals(curArg)) {
			foundArg2 = true;
		    }
		}
		if (foundArg1 && foundArg2) {
		    return true;
		}
	    }
	}
	return false;
    }
		
			  

    /**
     * getStateList - convert to oclStateList
     * If the prototype for the predicate is static mark the 
     * predicate as static
     * @return oclStateList
     */
    public oclStateList getStateList() {
	ListIterator li = statePredList.listIterator();
	oclStateList osl = new oclStateList();
	while (li.hasNext()) {
	    PredDetail cur = (PredDetail)li.next();
	    osl.addPredicate(cur.pred);
	}   
	int inx = 0;
	while (inx < neList.size()) {
	    osl.addPredicate((oclPredicate)neList.getElementAt(inx++));
	}
	return osl;
    }

    /**
     * getPlainList - convert to simple list of predicates
     * If the prototype for the predicate is static mark the 
     * predicate as static
     * @return List
     */
    public List getPlainList() {
	ListIterator li = statePredList.listIterator();
	List pList = new ArrayList();
	while (li.hasNext()) {
	    PredDetail cur = (PredDetail)li.next();
	    pList.add(cur.pred);
	}   
	int inx = 0;
	while (inx < neList.size()) {
	    pList.add((oclPredicate)neList.getElementAt(inx++));
	}
	return pList;
    }

    /**
     * removeRestrictionForVariable
     * remove an is_of_sort predicate from the neList
     * @param varName - the argument/variable name
     * @throws StateModelException when no such restriction
     */
    public void removeRestrictionForVariable(String varName) 
	throws StateModelException {
	int noElements = neList.size();
	boolean found = false;
	int inx = 0;
	while (!found && inx < noElements) {
	    oclPredicate cur = (oclPredicate)neList.get(inx);
	    if ("is_of_sort".equals(cur.getName()) &&
		cur.isSortRestrictionFor(varName)) {
		neList.removeElementAt(inx);
		found = true;
	    }
	    inx++;
	}
	if (!found) {
	    throw new StateModelException("No such restriction");
	}
    }

    /**
     * addRestriction
     * add given is_of_sort predicate to the neList
     * throw exception if restriction for variable already exists
     * @param restrict - the given predicate
     * throws StateModel.StateModelException
     */
    public void addRestriction(oclPredicate restrict) 
	throws StateModelException {
	String varName = restrict.getNthElementName(0);
	int noElements = neList.size();
	boolean found = false;
	int inx = 0;
	while (!found && inx < noElements) {
	    oclPredicate cur = (oclPredicate)neList.get(inx);
	    if ("is_of_sort".equals(cur.getName()) &&
		cur.isSortRestrictionFor(varName)) {
		found = true;
	    }
	    inx++;
	}
	if (found) {
	    throw new StateModelException("restriction exists");
	} else {
	    neList.addElement(restrict);
	}
    }

    /**
     * resetModel - remove existing predicates from the model
     */
    public void resetModel() {
	statePredList = null;
	statePredList = new ArrayList();
	neList.removeAllElements();
    }

    public class PredDetail {
	int startOffset;
	int length;
	oclPredicate pred; // The predicate in the state list
	oclPredicate proto;// The typed prototype fot this predicate

	public PredDetail(oclPredicate curPred,int offset) {
	    pred = curPred;
	    startOffset = offset;
	    length = pred.toString().length();
	}
    }

    class StateModelException extends Exception {
	public StateModelException(){super();};
	public StateModelException(String s){super(s);};
    }
    
}
