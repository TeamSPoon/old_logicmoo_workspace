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

// oclSSClassDef - Store Object Sub State Class Expressions
package jplan.ocl;

import java.util.List;
import java.util.ArrayList;
import java.util.ListIterator;
import java.io.*;
import java.util.NoSuchElementException;

import jplan.general.*;

public class oclSSClassDef implements oclPrint, Serializable, Cloneable {
	private String kind;
	private String Id;
	private List states = new ArrayList();

	/**
	 * Constructor
	 * - just create the ArrayList
	 */
	public oclSSClassDef() {
		states = new ArrayList();
	}

	/**
	 * Constructor
	 * - just create the ArrayList and set the sort and state ID
	 * @param  sort
	 * @param  ID for state definitions
	 */
	public oclSSClassDef(String sort, String ID) {
		this();
		kind = new String(sort);
		Id = new String(ID);
	}

	/**
	 * addState
	 * create an empty state list and add it to the class definition
	 * @return the empty state list
	 */
	public oclStateList addState() {
		oclStateList cur = new oclStateList();
		states.add(cur);
		return cur;
	}

	/* WZ 22/3/02 */
	/**
	 * addState add a new oclStateList
	 * 
	 */
	public void addState(oclStateList oclState) {
		states.add(oclState);
	}

	/**
	 * getStateSort 
	 * @return the sort name
	 */
	public String getStateSort() {
		return kind;
	}

	/**
	 * getStateSortId 
	 * @return the sort variable
	 */
	public String getStateSortId() {
		return Id;
	}
	/**
	 * setStateSortId 
	 * @param newId the new Id
	 */
	public void setStateSortId(String newId) {
		Id = new String(newId);
	}
	/**
	 * getStateList 
	 * @return the list of states
	 */
	public List getStateList() {
		return states;
	}

	/* WZ 22/3/02 */
	/**
	 * getStateList 
	 * @param list
	 */
	public void setStateList(List list) {
		ListIterator li = list.listIterator();
		while (li.hasNext()) {
			states.add((oclStateList) li.next());
		}
	}

	/* WZ 22/3/02 */
	/**
	 * return true if no stateLists stored
	 */
	public boolean isEmpty() {
		if (states.size() == 0)
			return true;
		return false;
	}

	/**
	 * check
	 * do verification checks on this state
	 * check that sort exists
	 * check that ne predicate arguments are all referenced in other none
	 *        ne predicates
	 * that is_of_sort predicates refer to sorts and that the argument
	 * is of the required sort
	 * check that every predicate matches a predicate in the predicate list
	 * and that every dynamic predicate refers to the state ID
	 * that no state is a pure sub-state of any other
	 * set isStatic flag for static predicates in the state
	 * @param cur - the current domain definition
	 * @param mssgList - to append error messages to
	 * @return boolean - true if no errors
	 */
	public boolean check(oclDomain cur, List mssgList) {
		boolean res = true;
		// First check that the sort is defined
		if (!cur.isSort(kind)) {
			mssgList.add(
				"Sort " + kind + " is not defined in the sort hierarchy.");
			res = false;
		}
		// Now chech the state predicates
		ListIterator li = states.listIterator();
		while (li.hasNext()) {
			//First find prototypes for each predicate
			// and check that dynamic preds refer to the state variable
			List preds = (List) ((oclStateList) li.next()).getPredicateList();
			ListIterator liPreds = preds.listIterator();
			while (liPreds.hasNext()) {
				oclPredicate curPred = (oclPredicate) liPreds.next();
				if (curPred.getName().equals("ne")) {
					// checkNEArgs(curPred,preds);
					continue;
				} else if (curPred.getName().equals("is_of_sort")) {
					// checkIsOfSortArgs(curPred,preds,cur,mssgList);
					continue;
				}
				oclPredicate match = null;
				try {
					match = cur.findPrototype(curPred);
					// 		    Utility.debugPrintln("Found match " + match.toString());
					curPred.setArgSorts(match);
					if (!match.isStatic()) {
						if (match.isFluent()) { // Ron 2/7/03 - needed when domain in read in to set fluents within state
							curPred.setFluent(true);
						}
						if (!curPred.refersTo(Id)) {
							// Do we have an is_of_sort restriction to limit
							// one of the arguments
							mssgList.add(
								"Predicate "
									+ curPred.toString()
									+ " does not refer to the state Id "
									+ Id);
							res = false;
						}
					} else {
						curPred.setStatic(true);
					}
				} catch (OCLSelectionException e) {
					if (curPred.isFluent()) { //Ron 27/06/03
						mssgList.add(
							"Functor "
							+ curPred.toString()
							+ " is not defined in the functors list.");
					} else {
						mssgList.add(
								"Predicate "
								+ curPred.toString()
								+ " is not defined in the predicates list.");
					}
					res = false;
				}

			}
			//Now check the ne(s) and is_of_sort predicates
			liPreds = preds.listIterator();
			while (liPreds.hasNext()) {
				oclPredicate curPred = (oclPredicate) liPreds.next();
				if (curPred.getName().equals("ne")) {
					checkNEArgs(curPred, preds);
				} else if (curPred.getName().equals("is_of_sort")) {
					checkIsOfSortArgs(curPred, preds, cur, mssgList);
				}
			}
		}
		// Finally check for substates
		if (res) {
			res = checkNoPureSubStates(mssgList);
		}
		return res;
	}

	/**
	 * checkNoPureSubStates
	 * @param mssgs error message list
	 * return boolean - true if no error
	 */
	private boolean checkNoPureSubStates(List mssgs) {
		ListIterator li = states.listIterator();
		int testExamp = 0;
		while (li.hasNext()) {
			List preds = (List) ((oclStateList) li.next()).getPredicateList();
			List predNames = getPredNames(preds);
			ListIterator liAll = states.listIterator();
			int inner = 0;
			while (liAll.hasNext()) {
				List cpreds =
					(List) ((oclStateList) liAll.next()).getPredicateList();
				if (inner != testExamp) {
					List cpredNames = getPredNames(cpreds);
					if (isStringListSubSet(predNames, cpredNames)) {
						mssgs.add(
							"WARNING State ["
								+ stringListToString(predNames)
								+ "] is a pure sub-set of ["
								+ stringListToString(cpredNames)
								+ "].");
						return false;
					}
				}
				inner++;
			}
			testExamp++;
		}
		return true;
	}

	/**
	 * usesPred
	 * test to see if this class definition uses the given predicate
	 * @param curPred - the given predicate
	 * @return boolean - true if predicate used
	 */
	public boolean usesPred(oclPredicate curPred) {
		ListIterator li = states.listIterator();
		boolean found = false;
		while (!found && li.hasNext()) {
			oclStateList curState = (oclStateList) li.next();
			ListIterator liPreds = curState.getPredicateList().listIterator();
			while (!found && liPreds.hasNext()) {
				oclPredicate cur = (oclPredicate) liPreds.next();
				if (curPred.getName().equals(cur.getName())) {
					found = true;
				}
			}
		}
		return found;
	}

	/**
	 * stringListToString
	 * @param strs - list of strings
	 * @return String
	 */
	private String stringListToString(List strs) {
		ListIterator li = strs.listIterator();
		String res = "";
		while (li.hasNext()) {
			res = res.concat((String) li.next());
			if (li.hasNext()) {
				res = res.concat(",");
			}
		}
		return res;
	}

	/**
	 * getPredNames
	 * collect a list of the names of the predicates in the given list
	 * ignore ne(s) and is_of_sort(s) and statics
	 * @param preds - given oclPredicate list
	 * @return List - String list of predicate names
	 */
	private List getPredNames(List preds) {
		List names = new ArrayList();
		ListIterator li = preds.listIterator();
		while (li.hasNext()) {
			oclPredicate cur = (oclPredicate) li.next();
			if (!cur.isStatic()
				&& !cur.getName().equals("ne")
				&& !cur.getName().equals("is_of_sort")) {
				names.add(cur.getName());
			}
		}
		return names;
	}

	/** 
	 * isStringListSubSet
	 * check if firs list is a subset of the second
	 * @param given list of Strings
	 * @param match list of Strings
	 * @return boolean
	 */
	private boolean isStringListSubSet(List given, List match) {
		ListIterator liGiven = given.listIterator();
		boolean posSubset = true;
		while (posSubset && liGiven.hasNext()) {
			String curName = (String) liGiven.next();
			ListIterator liMatch = match.listIterator();
			boolean found = false;
			while (!found && liMatch.hasNext()) {
				String curMatch = (String) liMatch.next();
				if (curName.equals(curMatch)) {
					found = true;
				}
			}
			if (!found) {
				posSubset = false;
			}
		}
		return posSubset;
	}

	/**
	 * checkIsOfSortArgs
	 * checks that the sort named is a sort 
	 * and that the variable given where referenced in other predicates
	 * may be of that sort
	 * @param is_of_sort - the is_of_sort predicate
	 * @param preds - list of predicates in this state to check against
	 * @param curDom - the current domain
	 * @param mssgs - the error message list
	 * @return boolean
	 */
	private boolean checkIsOfSortArgs(
		oclPredicate is_of_sort,
		List preds,
		oclDomain curDom,
		List mssgs) {
		String sort = is_of_sort.getNthElementName(1);
		if (!curDom.isSort(sort)) {
			mssgs.add(
				"The given sort in the is_of_sort predicate "
					+ sort
					+ " is not defined in the domain.");
			return false;
		}
		String argName = is_of_sort.getNthElementName(0);
		ListIterator liPreds = preds.listIterator();
		boolean found = false;
		while (liPreds.hasNext()) {
			oclPredicate cur = (oclPredicate) liPreds.next();
			if (!cur.getName().equals("ne")
				&& !cur.getName().equals("is_of_sort")) {
				ListIterator li = cur.getArguments().listIterator();
				while (li.hasNext()) {
					OPredicate.pArg arg = (OPredicate.pArg) li.next();
					if (arg.name.equals(argName)) {
						if (!arg.sort.equals(sort)
							&& !curDom.sortIsSubSortOf(sort, arg.sort)) {
							mssgs.add(
								arg.name
									+ " of predicate "
									+ cur.getName()
									+ " is not of sort "
									+ sort
									+ " as required by is_of_sort clause.");
							return false;
						}
					}
				}
			}

		}
		return true;

	}

	/**
	 * checkNEArgs
	 * checks that the given ne clause's arguments all occur 
	 * in the given predicate list
	 * @param ne -  given ne
	 * @param preds
	 * @return boolean
	 */
	private boolean checkNEArgs(oclPredicate ne, List preds) {
		ListIterator li = ne.getArguments().listIterator();
		while (li.hasNext()) {
			oclPredicate.pArg arg = (oclPredicate.pArg) li.next();
			ListIterator liPreds = preds.listIterator();
			boolean found = false;
			while (!found && liPreds.hasNext()) {
				oclPredicate cur = (oclPredicate) liPreds.next();
				if (!cur.getName().equals("ne")) {
					if (cur.refersTo(arg.name)) {
						found = true;
					}
				}
			}
			if (!found) {
				return false;
			}
		}
		return true;
	}

	/**
	 * seUnifiesWithStateDef
	 * find a state definition to unify with the given elements 
	 * of an sub-state expression
	 * @param  name - the se Id
	 * @param strippedState - predicate list - stripped of ne and static predicates
	 * @return boolean - true if matching definition found
	 */
	public boolean seUnifiesWithStateDef(String name, List strippedState) {
		ListIterator li = states.listIterator();
		boolean found = false;
		while (!found && li.hasNext()) {
			oclStateList cur = (oclStateList) li.next();
			found = cur.seUnifiesWithStateList(name, Id, strippedState);
		}
		return found;
	}

	/**
	 * seUnifiesWithHigherarchicalStateDef
	 * find a state definition to unify with the given elements 
	 * of an sub-state expression
	 * @param curDom - the current domain definition
	 * @param  name - the se Id
	 * @param strippedState - predicate list - stripped of ne and static predicates
	 * @return boolean - true if matching definition found
	 */
	public boolean seUnifiesWithHierarchicalStateDef(
		oclDomain curDom,
		String name,
		List strippedState) {
		Utility.debugPrintln(
			"about to check def states for  " + name + " of sort " + kind);
		ListIterator li = states.listIterator();
		int maxFound = 0;
		List remainingPreds = new ArrayList();
		List left = null;
		while (li.hasNext()) {
			oclStateList cur = (oclStateList) li.next();
			left = new ArrayList();
			int found =
				cur.sePartiallyUnifiesWithStateList(
					name,
					Id,
					strippedState,
					left);
			if (found > maxFound) {
				maxFound = found;
				remainingPreds = left;
			}
		}
		// Now move up the sort tree
		String parent = "none";
		try {
			parent = curDom.findSortParent(kind);
		} catch (NoSuchElementException e) {
			Utility.debugPrintln("No Parent for sort " + kind);
			if (remainingPreds.size() == 0) //Ron 11/11/02
				return true;
			else
				return false;
		}
		Utility.debugPrintln(
			"about to check def states Parent for sort " + parent);
		return curDom.seUnifiesWithHierarchicalStateDef(
			name,
			parent,
			remainingPreds);
	}

	/**
	 * ssUnifiesWithStateDef
	 * find a state definition to unify with the given elements 
	 * of an sub-state description
	 * @param  name - the ss/sc Id
	 * @param strippedState - predicate list - stripped of static predicates
	 * @return boolean - true if matching definition found
	 */
	public boolean ssUnifiesWithStateDef(String name, List strippedState) {
		ListIterator li = states.listIterator();
		boolean found = false;
		while (!found && li.hasNext()) {
			oclStateList cur = (oclStateList) li.next();
			found = cur.ssUnifiesWithStateList(name, Id, strippedState);
		}
		return found;
	}

	/**
	 * ssUnifiesWithHigherarchicalStateDef
	 * find a state definition to unify with the given elements 
	 * of an sub-state expression
	 * @param curDom- the current domain definition
	 * @param  name - the ss Id
	 * @param strippedState - predicate list - stripped of ne and static predicates
	 * @param baseSortLevel - baseSortLevel if true this expression must match at
	 *                  the current level in the hierarchy
	 * @return boolean - true if matching definition found
	 */
	public boolean ssUnifiesWithHierarchicalStateDef(
		oclDomain curDom,
		String name,
		List strippedState,
		boolean baseSortLevel) {
		Utility.debugPrintln(
			"SS about to check def states for  " + name + " of sort " + kind);
		Utility.debugPrintln("SS given stripped state ");
		ListIterator liTemp = strippedState.listIterator();
		while (liTemp.hasNext()) {
			oclPredicate ttt = (oclPredicate) liTemp.next();
			Utility.debugPrintln("     " + ttt.toString());
		}
		ListIterator li = states.listIterator();
		int maxFound = -1;
		List remainingPreds = new ArrayList();
		List left = null;
		while (li.hasNext()) {
			oclStateList cur = (oclStateList) li.next();
			left = new ArrayList();
			int found =
				cur.ssPartiallyUnifiesWithStateList(
					name,
					Id,
					strippedState,
					left);
			if (found > maxFound) {
				maxFound = found;
				remainingPreds = left;
			}
		}
		Utility.debugPrintln("SS Matches found " + maxFound);
		Utility.debugPrintln("SS state left ");
		liTemp = remainingPreds.listIterator();
		while (liTemp.hasNext()) {
			oclPredicate ttt = (oclPredicate) liTemp.next();
			Utility.debugPrintln("     " + ttt.toString());
		}
		if (maxFound == -1) {
			// failed to match any state definition for this sort
			// no point continuing up the sort tree
			return false;
		}
		/*
		 *  This is not the rule can match at a higher level
		  if (baseSortLevel && maxFound == 0) {
			// At this level if there is a class def we must
			// match with one of them
			return false;
		}
		*/
		// Now move up the sort tree
		String parent = "none";
		try {
			parent = curDom.findSortParent(kind);
		} catch (NoSuchElementException e) {
			Utility.debugPrintln("No Parent for sort " + kind);
			if (remainingPreds.size() == 0) //Ron 11/11/02 Bug was left
				return true;
			else
				return false;
		}
		Utility.debugPrintln(
			"about to check def states Parent for sort " + parent);
		return curDom.ssUnifiesWithHierarchicalStateDef(
			name,
			parent,
			remainingPreds,
			false);
	}

	/**
	 * seUsesStateDef
	 * find a state definition to unify with the given elements 
	 * and store opID in consumers list
	 * @param  name - the se Id
	 * @param strippedState - predicate list - stripped of ne and static predicates
	 * @param opID - the opID
	 */
	public void seUsesStateDef(String name, List strippedState, String opID) {
		ListIterator li = states.listIterator();
		boolean found = false;
		while (li.hasNext()) {
			oclStateList cur = (oclStateList) li.next();
			found = cur.seUsesStateList(name, Id, strippedState, opID);
		}
	}

	/* WZ 21/8/02 */
	/**
	 * replce the old ID with given one
	 * @param  sortName - the se Id
	 */
	public void replaceSort(String sortName) {
		if (sortName.equals(""))
			return;

		ListIterator li = states.listIterator();
		while (li.hasNext()) {
			oclStateList cur = (oclStateList) li.next();
			ListIterator liState = cur.getPredicateList().listIterator();
			while (liState.hasNext()) {
				oclPredicate opd = (oclPredicate) liState.next();
				try {
					opd.setSortForVariable(Id, sortName);
				} catch (Exception e) {
				}
			}
		}
		kind = sortName;
	}

	/* WZ 20/8/02 */
	/**
	 * replce the old ID with given one
	 * @param objName name - the se Id
	 */
	public void replaceID(String objName) {
		if (objName.equals(""))
			return;

		ListIterator li = states.listIterator();
		while (li.hasNext()) {
			oclStateList cur = (oclStateList) li.next();
			ListIterator liState = cur.getPredicateList().listIterator();
			while (liState.hasNext()) {
				oclPredicate opd = (oclPredicate) liState.next();
				try {
					opd.replaceVariableNameByName(Id, objName);
				} catch (Exception e) {
				}
			}
		}
		Id = objName;
	}

	/* 6/6/02 */
	/**
	 * Return true if current classDef contains a predicate with the given type
	 * @param opd given oclPredicate
	 * @return true if current classDef contains a predicate with the given type
	 */
	public boolean hasPredicate(oclPredicate opd) {
		ListIterator li = getStateList().listIterator();
		while (li.hasNext()) {
			oclStateList osdlist = (oclStateList) li.next();
			ListIterator liState = osdlist.getPredicateList().listIterator();
			while (liState.hasNext()) {
				oclPredicate oclPred = (oclPredicate) liState.next();
				if (oclPred.isSameType(opd))
					return true;
			}
		}
		return false;
	}
	/**
	 * ssUsesStateDef
	 * find a state definition to unify with the given elements 
	 * and store opID in producers list
	 * @param  name - the ss/sc Id
	 * @param strippedState - predicate list - stripped of static predicates
	 * @param opID - the opID
	 */
	public void ssUsesStateDef(String name, List strippedState, String opID) {
		ListIterator li = states.listIterator();
		boolean found = false;
		while (li.hasNext()) {
			oclStateList cur = (oclStateList) li.next();
			found = cur.ssUsesStateList(name, Id, strippedState, opID);
		}
	}

	/**
	 * clone()
	 *
	 */
	public Object clone() throws CloneNotSupportedException {
		try {
			oclSSClassDef oss = new oclSSClassDef(this.kind, this.Id);
			ListIterator li = states.listIterator();
			while (li.hasNext()) {
				oss.states.add(((oclStateList) li.next()).clone());
			}
			return oss;
		} catch (CloneNotSupportedException e) {
			throw e;
		}
	}

	/**
	 * clearProducersConsumers
	 * reset producer consumer lists in the state lists
	 */
	public void clearProducersConsumers() {
		ListIterator li = states.listIterator();
		while (li.hasNext()) {
			oclStateList curss = (oclStateList) li.next();
			curss.clearProducersConsumers();
		}
	}

	/**
	 * printReachabilityAnalysis
	 * @param ps - to print to
	 */
	public void printReachabilityAnalysis(PrintWriter ps) {
		ps.print("Analysis for sort: ");
		ps.println(kind);
		ListIterator li = states.listIterator();
		while (li.hasNext()) {
			oclStateList cur = (oclStateList) li.next();
			cur.printReachabilityAnalysis(ps);
		}
	}

	public void oclPrintComponent(PrintWriter ps, int indent, boolean nline) {
		ps.println("substate_classes(" + kind + "," + Id + ",[");
		ListIterator li = states.listIterator();
		while (li.hasNext()) {
			((oclStateList) li.next()).oclPrintComponent(ps, 4, false);
			if (li.hasNext())
				ps.println(",");
		}
		ps.println("]).");
	}

	/* Weihong added on 02/04/2001 */
	public String toString() {
		StringBuffer sb = new StringBuffer("");
		sb.append("substate_classes(" + kind + "," + Id + ",[");
		ListIterator li = states.listIterator();
		while (li.hasNext()) {
			sb.append(((oclStateList) li.next()).toString());
			if (li.hasNext())
				sb.append(",");
		}
		sb.append("]).");
		return sb.toString();
	}

}
