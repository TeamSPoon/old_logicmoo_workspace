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
 * Class ExpressionModel
 * This class stores the Model of a SubState Expression that
 * is being edited - it helps in the process of mapping
 * from the oclPredicate view to the String text view of the editor
 * It stores both the predicates being edited and the prototype
 * for the predicate.
 * ne clauses are stored in both the predicate list and a seperate ne list
 * is_of_sort clauses are stored in both the predicate list and 
 * the seperate ne list
 * @author Ron
 * @version 0
 */

import javax.swing.*;
import java.util.List;
import java.util.*;

import jplan.ocl.*;
import jplan.general.*;

public class ExpressionModel {
	private oclDomain curDomain;
	private List predList; // This is the complete predicates list
	private List expressionList; // The predicates in this state
	public DefaultListModel neList; // The variable binding constraints
	private oclPredicate sescName; // se or sc clause
	// this state

	/**
	 * constructor
	 * @param dom reference to the complete predicate list
	 */
	public ExpressionModel(oclDomain dom) {
		curDomain = dom;
		predList = new ArrayList();
		try {
			ListIterator li = dom.predicates.listIterator();
			while (li.hasNext()) {
				oclPredicate cur = (oclPredicate) li.next();
				this.predList.add((oclPredicate) cur.clone());
			}
		} catch (Exception e) {
			Utility.debugPrintln("Unexpected failure to clone predicates " + e);
		}
		//This is the complete list of predicates defined
		expressionList = new ArrayList();
		neList = new DefaultListModel();
		sescName = null;
	}

	/**
	 * addPredicate
	 * @param cur The predicate to added in the edit window
	 * @param offset The start offset for the predicate in the edit window
	 * @return The PredDetail created;
	 * Searches for the prototype matching this predicate
	 * Throws an ExpressionException exception if not found.
	 * NOTE :: Should float ne clauses to the end when another is added
	 * No longer make the assumption that predicates are allways added to 
	 * the end of the document.
	 */
	public PredDetail addPredicate(oclPredicate cur, int offset)
		throws ExpressionException {
		String var1Sort = "";
		String var2Sort = "";
		try {
			if ("ne".equals(cur.getName())) {
				neList.addElement((oclPredicate) cur.clone());
				String var1 = cur.getNthElementName(0);
				String var2 = cur.getNthElementName(1);
				var1Sort = findSortForVariable(var1);
				var2Sort = findSortForVariable(var2);
			} else if ("se".equals(cur.getName())) {
				oclPredicate proto = new oclPredicate("se");
				proto.addConstArgument(
					oclPredicate.toVar(cur.getNthElementName(0)));
				proto.addVarArgument(cur.getNthElementName(0));
				predList.add(proto);
				sescName = cur;
			} else if ("sc".equals(cur.getName())) {
				oclPredicate proto = new oclPredicate("sc");
				proto.addConstArgument(
					oclPredicate.toVar(cur.getNthElementName(0)));
				proto.addVarArgument(cur.getNthElementName(0));
				predList.add(proto);
				sescName = cur;
			} else if ("is_of_sort".equals(cur.getName())) {
				Utility.debugPrintln("ADDING restriction " + cur.toString());
				neList.addElement((oclPredicate) cur.clone());
			}
		} catch (CloneNotSupportedException e) {
			Utility.debugPrintln("Unexpected clone failure - addPredicate");
			// Should not fail
			throw new ExpressionException("Could not clone ne Predicate.");
		}
		PredDetail curPredDetail = null;
		try {
			curPredDetail = new PredDetail((oclPredicate) cur.clone(), offset);
		} catch (CloneNotSupportedException e) {
			Utility.debugPrintln("Unexpected clone failure - addPredicate");
			// Should not fail
			throw new ExpressionException("Could not clone Predicate.");
		}

		boolean found;

		found = false;
		if ("ne".equals(cur.getName())) {

			oclPredicate neProto = new oclPredicate("ne");
			if ("not-found".equals(var1Sort)
				&& !"not-found".equals(var2Sort)) {
				neProto.addConstArgument(var2Sort);
				neProto.addConstArgument(var2Sort);
				found = true;
			} else if (
				"not-found".equals(var2Sort)
					&& !"not-found".equals(var1Sort)) {
				neProto.addConstArgument(var1Sort);
				neProto.addConstArgument(var1Sort);
				found = true;
			} else if (var1Sort.equals(var2Sort)) {
				neProto.addConstArgument(var1Sort);
				neProto.addConstArgument(var1Sort);
				found = true;
			} else if (curDomain.sortIsSubSortOf(var1Sort, var2Sort)) {
				neProto.addConstArgument(var1Sort);
				neProto.addConstArgument(var1Sort);
				found = true;
			} else {
				neProto.addConstArgument(var2Sort);
				neProto.addConstArgument(var2Sort);
				found = true;
			}
			curPredDetail.proto = neProto;
			curPredDetail.pred.setStatic(false);
		} else if ("is_of_sort".equals(cur.getName())) {
			Utility.debugPrintln("ADDING restriction 2 " + cur.toString());
			oclPredicate restrictProto = new oclPredicate("is_of_sort");
			restrictProto.addConstArgument(cur.getNthElementName(1));
			restrictProto.addConstArgument("none");
			curPredDetail.proto = restrictProto;
			found = true;
		} else if(cur.getProtoType() != null) {
			oclPredicate protoType = (oclPredicate) cur.getProtoType();
			found = false;
			if (protoType.size() > 0 ) {
				curPredDetail.proto = protoType;
				found = true;
			}
// Ron replaced the following code 9/10/02 - makes no sense to me	
// The above seems to be a rational re-working but I dont know where the prototype
// might not have any arguments!!		
//		} else if (
//			((oclPredicate) cur.getProtoType()).size() > 0) { //WZ 21/8/02
//			found = false;
//			oclPredicate protoType = (oclPredicate) cur.getProtoType();
//			if (protoType != null) {
//				curPredDetail.proto = protoType;
//				found = true;
//			}
		} else {
			found = false;
			ListIterator li = predList.listIterator();
			Utility.debugPrintln("Looking for the prototype");
			while (li.hasNext() && !found) {
				oclPredicate curProto = (oclPredicate) li.next();
				if (curProto.getName().equals(cur.getName())
					&& curProto.size() == cur.size()) {
					curPredDetail.proto = curProto;
					curPredDetail.pred.setStatic(
						curPredDetail.proto.isStatic());
					found = true;
				}
			}
		}
		// 	 //WZ 18/4/02
		// 	 //to add a predicate with sort information
		// 	 if (!found) {
		// 	     found = false;
		// 	     oclPredicate protoType = (oclPredicate)cur.getProtoType();
		// 	     if (protoType != null){
		// 		 curPredDetail.proto = protoType;
		// 		 found = true;
		// 	     }
		// 	 }
		// 	 //WZ 18/4/02 end

		if (!found) {
			throw new ExpressionException("No such Prototype Predicate.");
		}
		expressionList.add(curPredDetail);
		return curPredDetail;
	}

	/**
	 * addPredicate
	 * @param cur The predicate to added in the edit window
	 * @param offset The start offset for the predicate in the edit window
	 * @param proto The predicate prototype
	 * @return The PredDetail created;
	 * Searches for the prototype matching this predicate
	 * Throws an ExpressionException exception if not found.
	 * NOTE :: Should float ne clauses to the end when another is added
	 * No longer make the assumption that predicates are allways added to 
	 * the end of the document.
	 */
	public PredDetail addPredicate(
		oclPredicate cur,
		int offset,
		oclPredicate proto)
		throws ExpressionException {
		PredDetail curPredDetail = null;
		try {
			curPredDetail = new PredDetail((oclPredicate) cur.clone(), offset);
		} catch (CloneNotSupportedException e) {
			Utility.debugPrintln("Unexpected clone failure - addPredicate");
			// Should not fail
			throw new ExpressionException("Could not clone Predicate.");
		}
		curPredDetail.proto = proto;
		curPredDetail.pred.setStatic(false);
		expressionList.add(curPredDetail);
		return curPredDetail;
	}

	/*
	 * findSortForVariable
	 * find an identical variable name in the current predicate list
	 * and return its sort
	 * if not found return "not-found"
	 * @param String the variable name
	 * @return String the variable Sort
	 */
	private String findSortForVariable(String varName) {
		ListIterator li = expressionList.listIterator();
		while (li.hasNext()) {
			PredDetail curDetail = (PredDetail) li.next();
			List args = curDetail.pred.getArguments();
			ListIterator liArgs = args.listIterator();
			int argPos = 0;
			while (liArgs.hasNext()) {
				oclPredicate.pArg curArg = (oclPredicate.pArg) liArgs.next();
				if (varName.equals(curArg.name)) {
					return curDetail.proto.getNthElementName(argPos);
				}
				argPos++;
			}
		}
		return "not-found";
	}

	/**
	 * editVar - change a variable name
	 * propogate change to stored document offsets
	 * @param startOffset of variable - relative to document start
	 * @param newVarName the new variable name
	 * @return String - the old variable name
	 */
	public String editVar(int startOffset, String newVarName)
		throws Exception, ExpressionException {
		String oldVName = "none";
		int indexPos = 0;
		int changeInc = 0;
		ListIterator li = expressionList.listIterator();
		boolean found = false;
		// 	Utility.debugPrintln("\neditVar given " + startOffset + " " + newVarName);
		while (li.hasNext()) {
			PredDetail cur = (PredDetail) li.next();
			// 	    Utility.debugPrintln("Start offset = " + indexPos);
			if (startOffset >= indexPos
				&& startOffset <= indexPos + cur.length) {
				//Found the predicate now replace the variable
				oldVName = cur.pred.elementAt(startOffset - indexPos);
				changeInc =
					cur.pred.replaceVariableAt(
						startOffset - indexPos,
						newVarName);

				// start has not changed
				// 		Utility.debugPrintln("Replaced change inc = " + changeInc);
				cur.length = cur.pred.toString().length();
				//Now update the following predicate start offsets
				found = true;
				indexPos += (cur.length + 1) - changeInc; //use old length
			} else if (found) {
				cur.startOffset += changeInc;
				indexPos += cur.length + 1; // +1 for return
			} else {
				indexPos += cur.length + 1; // +1 for return
			}
		}
		if (!found) {
			throw new ExpressionException("\nVariable not found");
		}
		return oldVName;
	}

	/**
	 * getExpressionList
	 * @return the entire expression List
	 */
	public List getExpressionList() {
		return expressionList;
	}

	/**
	 * getSCSEName
	 * @return oclPredicate - the se or sc clause provided for this
	 *                      - expression list
	 */
	public oclPredicate getSCSEName() throws ExpressionException {
		if (sescName != null) {
			ListIterator li = expressionList.listIterator();
			while (li.hasNext()) {
				oclPredicate cur = ((PredDetail) li.next()).pred;
				if ("sc".equals(cur.getName()) || "se".equals(cur.getName())) {
					try {
						sescName.replaceVariableNo(1, cur.getNthElementName(1));
					} catch (Exception e) {
						Utility.debugPrintln(
							"Unexpected failure to replace se/sc ID " + e);
					}
					Utility.debugPrintln("SE/SC name " + sescName.toString());
					return (sescName);
				}
			}
		}
		throw new ExpressionException("No such predicate");
	}

	/**
	 * getPurePredicateList
	 * @return List - the list of predicate in the expression
	 *                without the se or sc name
	 */
	public List getPurePredicateList() {
		if (sescName == null) {
			return getFullPredicateList();
		} else {
			List ret = new ArrayList();
			ListIterator li = expressionList.listIterator();
			while (li.hasNext()) {
				oclPredicate cur = ((PredDetail) li.next()).pred;
				if (!"se".equals(cur.getName())
					&& !"sc".equals(cur.getName())) {
					ret.add(cur);
				}
			}
			return ret;
		}
	}

	/* Weihong added on 16/5/2001 */
	/**
	 * getFullPredicateList
	 * @return List - the list of predicate in the expression
	 * 
	 */
	public List getFullPredicateList() {
		List ret = new ArrayList();
		ListIterator li = expressionList.listIterator();
		while (li.hasNext()) {
			oclPredicate cur = ((PredDetail) li.next()).pred;
			ret.add(cur);
		}
		return ret;
	}

	/**
	 * getPredicateAt - find the predicate containing the text location
	 * @param pos - loc text position
	 * @ return the oclPredicate
	 */
	public oclPredicate getPredicateAt(int pos) throws ExpressionException {
		ListIterator li = expressionList.listIterator();
		while (li.hasNext()) {
			PredDetail cur = (PredDetail) li.next();
			// 	    Utility.debugPrintln("Start = " + cur.startOffset +
			// 			       " length = " + cur.length);
			if (pos >= cur.startOffset
				&& pos <= cur.startOffset + cur.length) {
				return cur.pred;
			}
		}
		throw new ExpressionException("No such predicate");
	}

	/**
	 * This version returns the whole pred detail
	 */
	public PredDetail getPredDetailAt(int pos) throws ExpressionException {
		ListIterator li = expressionList.listIterator();
		while (li.hasNext()) {
			PredDetail cur = (PredDetail) li.next();
			// 	    Utility.debugPrintln("Start = " + cur.startOffset +
			// 			       " length = " + cur.length);
			if (pos >= cur.startOffset
				&& pos <= cur.startOffset + cur.length) {
				return cur;
			}
		}
		throw new ExpressionException("No such predicate");
	}

	/**
	 * getArgumentAt - find the predicate argument at the
	 * given position
	 * @param pos - the text position
	 * @return String - the argument name
	 */
	public String getArgumentAt(int pos)
		throws ExpressionException, Exception {
		int indexPos = 0;
		PredDetail pd;
		String res;
		try {
			pd = getPredDetailAt(pos);
		} catch (Exception e) {
			throw new ExpressionException("No predicate here");
		}
		try {
			res = pd.pred.elementAt(pos - pd.startOffset);
		} catch (Exception e) {
			throw e;
		}
		return res;
	}

	/**
	* getSortForArgumentAt - find the predicate argument sort at the
	* given position
	* @param pos - the text position
	* @return String - the sort name
	*/
	public String getSortForArgumentAt(int pos)
		throws ExpressionException, Exception {
		int indexPos = 0;
		PredDetail pd;
		String res;
		try {
			pd = getPredDetailAt(pos);
		} catch (Exception e) {
			throw new ExpressionException("No predicate here");
		}
		try {
			int n = pd.pred.elementNoAt(pos - pd.startOffset);
			res = pd.proto.getNthElementName(n);
		} catch (Exception e) {
			throw e;
		}
		return res;
	}

	/* WZ 27/8/02 */
	/**
	 * return size
	 * @reutnr void
	 */
	public int size() {
		return expressionList.size();
	}

	/* WZ 27/8/02 */
	/**
	 * removePredicateAt - remove the predicate containing pos
	 * @param opd
	 * @return List - PredDetail(s) for all predicates beyond 
	 * removed predicate
	 */
	public PredDetail removePredicateByName(oclPredicate opd) {
		PredDetail returnPred = null;
		List res = new ArrayList();
		ListIterator li = expressionList.listIterator();
		int count = 0;
		int index = 0;
		int decrement = 0;
		boolean found = false;
		while (li.hasNext()) {
			PredDetail cur = (PredDetail) li.next();
			if (opd.getName().equals(cur.pred.getName())) {
				found = true;
				index = count;
				decrement = cur.length + 1;
				returnPred = cur;
			} else if (found) {
				cur.startOffset -= decrement;
				res.add(cur);
			}
			count++;
		}
		if (found) {
			expressionList.remove(index);
		}
		return returnPred;
	}

	/**
	 * removePredicateAt - remove the predicate containing pos
	 * @param pos
	 * @return List - PredDetail(s) for all predicates beyond 
	 * removed predicate
	 */
	public List removePredicateAt(int pos) throws ExpressionException {
		List res = new ArrayList();
		ListIterator li = expressionList.listIterator();
		int count = 0;
		int index = 0;
		int decrement = 0;
		boolean found = false;
		while (li.hasNext()) {
			PredDetail cur = (PredDetail) li.next();
			// 	    Utility.debugPrintln("Start = " + cur.startOffset +
			// 			       " length = " + cur.length);
			if (pos >= cur.startOffset
				&& pos <= cur.startOffset + cur.length) {
				found = true;
				index = count;
				decrement = cur.length + 1;
			} else if (found) {
				// 		Utility.debugPrintln("decrementing from = " +
				// 				   cur.startOffset +
				// 				   " by " + decrement);
				cur.startOffset -= decrement;
				res.add(cur);
			}
			count++;
		}
		if (!found) {
			throw new ExpressionException("No such predicate");
		} else {
			expressionList.remove(index);
			return res;
		}

	}

	/**
	 * neExists - check to see if a ne clause with the given variable
	 * name exists
	 * @param vName1 String Variable name 1
	 * @param vName2 String Variable name 2
	 * @return index position if exists otherwise -1
	 */
	// 28/8/01 Ron modified to deal with is_of_sort
	public int neExists(String vName1, String vName2) {
		int size = neList.getSize();
		int index = 0;
		while (index < size) {
			oclPredicate cur = (oclPredicate) neList.elementAt(index);
			if ("ne".equals(cur.getName())) {
				List args = cur.getArguments();
				ListIterator liArgs = args.listIterator();
				boolean match = true;
				while (match && liArgs.hasNext()) {
					String argName = ((OPredicate.pArg) liArgs.next()).name;
					if (!argName.equals(vName1) && !argName.equals(vName2)) {
						match = false;
					}
				}
				if (match) { // Found it
					return index;
				}
			}
			index++;
		}
		return -1;
	}

	/**
	 * removeNE 
	 * remove the ne clause from the private ne list
	 * @param givenNE - the ne to remove
	 */
	public void removeNE(oclPredicate givenNE) {
		int size = neList.getSize();
		int index = 0;
		while (index < size) {
			oclPredicate cur = (oclPredicate) neList.elementAt(index);
			if (givenNE.equals(cur)) {
				neList.removeElementAt(index);
				return;
			}
			index++;
		}
	}

	/**
	 * removeRestrictionForVariable
	 * remove an is_of_sort predicate from the neList
	 * @param varName - the argument/variable name
	 * @throws ExpressionException when no such restriction
	 * @return List- list of affected PredDetails
	 * of all following preddetails (but including the details of the
	 * removed predicate as the first in the list)
	 */
	public List removeRestrictionForVariable(String varName)
		throws ExpressionException {
		Utility.debugPrintln("Asked to remove " + varName);
		List changed = new ArrayList();
		int noElements = neList.size();
		boolean found = false;
		int inx = 0;
		while (!found && inx < noElements) {
			oclPredicate cur = (oclPredicate) neList.get(inx);
			if ("is_of_sort".equals(cur.getName())
				&& cur.isSortRestrictionFor(varName)) {
				neList.removeElementAt(inx);
				found = true;
			}
			inx++;
		}
		if (!found) {
			throw new ExpressionException("No such restriction");
		} else {
			// Now get rid of the visible version
			ListIterator li = expressionList.listIterator();
			int count = 0;
			int index = 0;
			int decrement = 0;
			found = false;
			while (li.hasNext()) {
				PredDetail curPredDetail = (PredDetail) li.next();
				Utility.debugPrintln(
					"Start = "
						+ curPredDetail.startOffset
						+ " length = "
						+ curPredDetail.length);
				if (curPredDetail.pred.isSortRestrictionFor(varName)) {
					found = true;
					index = count;
					decrement = curPredDetail.length + 1;
					changed.add(curPredDetail);
				} else if (found) {
					Utility.debugPrintln(
						"decrementing from = "
							+ curPredDetail.startOffset
							+ " by "
							+ decrement);
					curPredDetail.startOffset -= decrement;
					changed.add(curPredDetail);
				}
				count++;
			}
			if (!found) {
				throw new ExpressionException("No such predicate");
			} else {
				expressionList.remove(index);
				return changed;
			}
		}
	}

	/**
	 * checkRestriction
	 * check to see if restriction already exists
	 * @param restrict - the given predicate
	 * @return boolean - true if restriction exists
	 */
	public boolean checkRestriction(oclPredicate restrict) {
		String varName = restrict.getNthElementName(0);
		int noElements = neList.size();
		boolean found = false;
		int inx = 0;
		while (!found && inx < noElements) {
			oclPredicate cur = (oclPredicate) neList.get(inx);
			if ("is_of_sort".equals(cur.getName())
				&& cur.isSortRestrictionFor(varName)) {
				found = true;
			}
			inx++;
		}
		if (found) {
			return true;
		} else {
			return false;
		}
	}

	/**
	 * addNE - add a ne clause 
	 * @param vName1 Variable name 1
	 * @param vName2 Variable name 2
	 * @param sort The Sort for this ne
	 * @param offset the offset from the start of the ne clause in the edit pane
	 * @return oclPredicate - the ne predicate constructed 
	 */
	public oclPredicate addNE(
		String vName1,
		String vName2,
		String sort,
		int offset) {
		oclPredicate ne = new oclPredicate("ne");
		oclPredicate proto = new oclPredicate("ne");
		ne.addVarArgument(vName1);
		ne.addVarArgument(vName2);
		proto.addConstArgument(sort);
		proto.addConstArgument(sort);
		PredDetail neDetail = new PredDetail(ne, offset);
		neDetail.proto = proto;
		expressionList.add(neDetail);
		return ne;
	}

	/**
	 * illegalNEs - check in turn each ne 
	 * @return A List of indexes of the illegal nes
	 */
	public List illegalNEs() {
		List ans = new ArrayList();
		int inx = 0;
		while (inx < neList.getSize()) {
			if (!checkNE((oclPredicate) neList.get(inx))) {
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
		String arg1 = (String) ((oclPredicate.pArg) args.get(0)).name;
		String arg2 = (String) ((oclPredicate.pArg) args.get(1)).name;
		boolean foundArg1 = false;
		boolean foundArg2 = false;
		ListIterator li = expressionList.listIterator();
		while (li.hasNext()) {
			List pargs = ((PredDetail) li.next()).pred.getArguments();
			ListIterator liPargs = pargs.listIterator();
			while (liPargs.hasNext()) {
				String curArg = ((oclPredicate.pArg) liPargs.next()).name;
				if (!foundArg1) {
					if (arg1.equals(curArg)) {
						foundArg1 = true;
					}
				}
				if (!foundArg2) {
					if (arg2.equals(curArg)) {
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
		ListIterator li = expressionList.listIterator();
		oclStateList osl = new oclStateList();
		while (li.hasNext()) {
			PredDetail cur = (PredDetail) li.next();
			osl.addPredicate(cur.pred);
		}
		int inx = 0;
		while (inx < neList.size()) {
			osl.addPredicate((oclPredicate) neList.getElementAt(inx++));
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
		ListIterator li = expressionList.listIterator();
		List pList = new ArrayList();
		while (li.hasNext()) {
			PredDetail cur = (PredDetail) li.next();
			pList.add(cur.pred);
		}
		return pList;
	}

	/**
	 * resetModel - remove existing predicates from the model
	 */
	public void resetModel() {
		expressionList = null;
		expressionList = new ArrayList();
		neList.removeAllElements();
	}

	public class PredDetail {
		int startOffset;
		int length;
		oclPredicate pred; // The predicate in the state list
		oclPredicate proto; // The typed prototype for this predicate

		public PredDetail(oclPredicate curPred, int offset) {
			pred = curPred;
			startOffset = offset;
			length = pred.toString().length();
		}
	}

	public class ExpressionException extends Exception {
		public ExpressionException() {
			super();
		};
		public ExpressionException(String s) {
			super(s);
		};
	}

}
