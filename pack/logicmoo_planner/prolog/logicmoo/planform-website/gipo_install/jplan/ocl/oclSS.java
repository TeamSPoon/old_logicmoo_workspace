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
import java.io.*;
import jplan.general.OPredicate; /* Weihong added on 11/02/2002 */
import jplan.general.OCLSelectionException;

/**
 * To store a state as part of inits section.
 */
public class oclSS implements oclPrint, Serializable, Cloneable {
	String sort;
	String name;
	private List state = new ArrayList();

	/**
	 * Creates an instance of oclSS.
	 * @param kind sort
	 * @param ID name
	 */
	public oclSS(String kind, String ID) {
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

	/* Weihong added on 4/7/2001 */
	/**
	 * Get the value of Sort.
	 * @return Value of Sort.
	 */
	public String getSort() {
		return sort;
	}

	/* Weihong added on 05/06/2001 */
	/**
	 * Get the value of Name.
	 * @return Value of Name.
	 */
	public String getName() {
		return name;
	}

	/* WZ 19/03/02 */
	/**
	 * Set the value of Name.
	 * @param v  Value to assign to Name.
	 */
	public void setName(String v) {
		this.name = v;
	}

	/* Weihong added on 23/05/2001 */
	/**
	 * Get the predicate lists.
	 * @return predicate lists.
	 */
	public List getState() {
		return state;
	}

	/* Weihong added on 25/2/02 */
	/**
	 * Update partially.
	 * @param oclStates
	 * @param ss
	 */
	public void updateWith(List oclStates, oclSS ss) {
		ListIterator matchingLi = oclStates.listIterator();
		//for every new predicate in the new state find a matching predicate and replace it
		while (matchingLi.hasNext()) {
			oclPredicate matchingPred = (oclPredicate) matchingLi.next();
			ListIterator li = state.listIterator();
			while (li.hasNext()) {
				oclPredicate oldPred = (oclPredicate) li.next();
				if (matchingPred.isSameType(oldPred)) {
					state.remove(oldPred);
					break;
				}
			}
		}

		ListIterator newLi = ss.getState().listIterator();
		while (newLi.hasNext()) {
			state.add((oclPredicate) newLi.next());
		}
	}

	/* WZ 28/5/02 */
	/**
	 * Update partially.
	 * @param oldState - the state list before change
	 * @param newState the state list after change
	 * @param curDomain
	 */
	public void updateWith(List oldState, List newState, oclDomain curDomain) {
		ListIterator matchingLi = oldState.listIterator();
		//for every new predicate in the new state find a matching predicate and replace it
		while (matchingLi.hasNext()) {
			oclPredicate matchingPred = (oclPredicate) matchingLi.next();
			ListIterator li = state.listIterator();
			while (li.hasNext()) {
				oclPredicate oldPred = (oclPredicate) li.next();
				if (matchingPred.isSameType(oldPred)) {
					//check from newstates if same level states have a new state
					if (matchingPred
						.hasMatchingStates(sort, curDomain, newState))
						state.remove(oldPred);
					break;
				}
			}
		}

		ListIterator newLi = newState.listIterator();
		while (newLi.hasNext()) {
			state.add((oclPredicate) newLi.next());
		}
	}

	/**
	 * Add one predicate to the predicate lists.
	 * @param prd predicate to add to state.
	 * d
	 */
	public void addPredicate(oclPredicate prd) {
		state.add(prd);
	}

	/* Weihong added on 13/06/2001 */
	/**
	 * Set the predicate lists.
	 * @param newState - predicate lists to assign to state.
	 * d
	 */
	public void setState(List newState) {
		state = newState;
	}

	/**
	 * check that each predicate is fully instantiated
	 * @return boolean
	 */
	public boolean isFullyInstantiated() {
		ListIterator li = state.listIterator();
		while (li.hasNext()) {
			oclPredicate curPred = (oclPredicate) li.next();
			if (!curPred.isInstantiated()) {
				return false;
			}
		}
		return true;
	}

	/**
	 * verify that every predicate in the state is legal
	 * that the state excluding static predicates is a valid
	 * ss expression.
	 * A side effect of this procedure is to set the sorts for each
	 * predicates arguments in the state description
	 * check that every predicate is fully instantiated.
	 * @param cur the current domain definition
	 * @param mssgs The list to append error messages to
	 * @return true if all checks passed
	 */
	public boolean check(oclDomain cur, List mssgs) {
		List strippedState = new ArrayList();
		boolean res = true;
		ListIterator li = state.listIterator();
		while (li.hasNext()) {
			oclPredicate curPred = (oclPredicate) li.next();
			if (curPred.getName().equals("ne")) {
				mssgs.add(
					"ne clauses are unnecessary in instantiated state clauses.");
				continue;
			}
			try {
				oclPredicate match = cur.findPrototype(curPred);
				curPred.setArgSorts(match);
				curPred.setSortForVariable(name, sort); /* WZ 5/6/02 */
				if (!match.isStatic()
					&& !match.getName().equals("is_of_sort")) {
					if (!curPred.refersTo(name)) {
						mssgs.add(
							"Predicate "
								+ curPred.toString()
								+ " does not refer to the expression Id "
								+ name
								+ " of sort "
								+ sort);
						res = false;
					}
					if (!curPred.isInstantiated(cur)) {
						mssgs.add(
							"All predicates ["
								+ curPred.toString()
								+ "] must be fully instantiated.");
					}
					strippedState.add(curPred);
				} else if (match.isStatic()) {
					mssgs.add(
						"Static predicates ["
							+ curPred.toString()
							+ "] are not required in initial clauses.");
				} else if (match.getName().equals("is_of_sort")) {
					mssgs.add(
						"is_of_sort predicates are not required in initial clauses.");
				}
			} catch (jplan.general.OCLSelectionException e) {
				mssgs.add(
					"Predicate "
						+ curPred.toString()
						+ " is not defined in the predicates list.");
				res = false;
			}
		}
		if (!res) {
			// dont check the sub state - a waste of time
			res = false;
		} else {
			res = cur.ssUnifiesWithStateDef(name, sort, strippedState);
			if (!res) {
				mssgs.add(
					"Expression "
						+ this.toString()
						+ " does not match any state definition.");
			}
		}
		return res;
	}

	/**
	 * checkAchieveSS
	 * check this ss clause as a valid achieve clause in a HTN domain
	 * @param curDom - the current working domain
	 * @param mssgs - the error list for error messages
	 * @return bollean - true if all OK
	 */
	public boolean checkAchieveSS(oclDomain curDom, List mssgs) {
		boolean res = true; //Look like a bug Ron 10/9/02
		List strippedState = new ArrayList();
		ListIterator li = state.listIterator();
		while (li.hasNext()) {
			oclPredicate curPred = (oclPredicate) li.next();
			if (curPred.getName().equals("ne")) {
				strippedState.add(curPred);
				// leave them in - may be in state def
				continue;
			}
			try {
				oclPredicate match = curDom.findPrototype(curPred);
				curPred.setArgSorts(match);
				curPred.setSortForVariable(name, sort); /* WZ 5/6/02 */
				if (!match.isStatic()
					&& !match.getName().equals("is_of_sort")) {
					if (!curPred.refersTo(name)) {
						mssgs.add(
							"Achieve ss Predicate "
								+ curPred.toString()
								+ " does not refer to the Transition Id "
								+ name
								+ " of sort "
								+ sort);
						res = false;
					}
					strippedState.add(curPred);
				} else if (
					curPred.getName().equals("is_of_sort")
						&& curPred.getNthElementName(0).equals(name)) {
					// This restriction clause refers to the transition
					// id - so should be in the class def
					strippedState.add(curPred);
				}

			} catch (OCLSelectionException e) {
				mssgs.add(
					"Achieve ss Predicate "
						+ curPred.toString()
						+ " is not defined in the predicates list.");
				res = false;
			}
		}
		if (!res) {
			// dont check the sub state - a waste of time
			res = false;
		} else {
			res = curDom.ssUnifiesWithStateDef(name, sort, strippedState);
			if (!res) {
				mssgs.add(
					"Achieve ss Expression "
						+ OPredicate.predListToString(strippedState)
						+ " does not match any state definition.");
			}
		}
		return (res);
	}


	/* Weihong added on 23/05/2001 */
	/**
	 * Returns a string representation of the oclSS
	 * @return String
	 */
	public String toString() {
		String res = "";
		ListIterator li = state.listIterator();
		while (li.hasNext()) {
//			 Ron update for ocl plus
			oclPredicate pred = (oclPredicate)li.next();
			if (pred.isFluent()) {
				oclFunctor func = new oclFunctor(pred);
				res = res.concat(func.toInstantiatedString());
			} else {
				res = res.concat(pred.toString());
			}
			if (li.hasNext())
				res = res.concat(new String(" "));
		}
		return res;
	}

	/* Weihong added on 11/02/2002 */
	/**
	 * for all the predicate replace with a new variable value.
	 * @param before the varible
	 * @param after new element name
	 * d
	 */
	public void replaceVariableName(OPredicate.pArg before, String after) {
		if (before.name.equals(name)) { /* Weihong added on 04/06/2001 */
			name = after;
		}
		ListIterator li = state.listIterator();
		while (li.hasNext()) {
			oclPredicate oprd = (oclPredicate) li.next();
			try {
				oprd.replaceVariableName(before, after);
			} catch (Exception e) {
			}
		}
	}

	/* WZ 27/3/02 */
	/**
	 * for all the predicate replace with a new variable value.
	 * @param before the varible
	 * @param after new element name
	 * d
	 */
	public void replaceVariableName(String before, String after) {
		if (before.equals(name)) {
			name = after;
		}
		ListIterator li = state.listIterator();
		while (li.hasNext()) {
			oclPredicate oprd = (oclPredicate) li.next();
			try {
				oprd.replaceVariableNameByName(before, after);
			} catch (Exception e) {
			}
		}
	}
	
	/**
	 * test to see if the variable/object name is referenced in the state
	 * @param name - the name to look for
	 * @return - true if found
	 */
	public boolean refersTo(String name) {
		ListIterator li = state.listIterator();
		while (li.hasNext()) {
			oclPredicate oprd = (oclPredicate) li.next();
			if (oprd.refersTo(name)) {
				return true;
			}
		}
		return false;
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
	 * Returns a string representation of the oclSE
	 * slightly sugared to ease reading
	 * @return String
	 */
	public String toSugarString() {
		String res = "";
		ListIterator li = state.listIterator();
		while (li.hasNext()) {
			res = res.concat(((oclPredicate) li.next()).toString());
			if (li.hasNext())
				res = res.concat(new String(" and "));
		}
		return res;
	}

	/**
	 * clone a copy of oclSS
	 * @return the oclSS clause 
	 * @throws CloneNotSupportedException 
	 */
	public Object clone() throws CloneNotSupportedException {
		try {
			oclSS ret = new oclSS(this.sort, this.name);
			ListIterator li = state.listIterator();
			while (li.hasNext()) {
				oclPredicate curPred = (oclPredicate) li.next();
				ret.state.add((oclPredicate) curPred.clone());
			}
			return (oclSS) ret;
		} catch (CloneNotSupportedException e) {
			throw e;
		}
	}

	/* WZ 11/6/02 */
	/**
	 * Collect all variables and its sort and form an oclPredicate format.
	 * @return the oclPredicate formatted expression.
	 */
	public oclPredicate getPredExpression(oclDomain curDom) {
		oclPredicate ret = new oclPredicate("Achieve");
		ListIterator liSS = getState().listIterator();
		while (liSS.hasNext()) {
			oclPredicate opd = (oclPredicate) liSS.next();
			curDom.addSignatureArgument(ret, opd);
		}
		return ret;
	}

	/* WZ 11/6/02 */
	/**
	 * form an oclPredicate format.
	 * @return the oclPredicate formatted expression.
	 */
	public oclPredicate getPredExpression() {
		oclPredicate ret = new oclPredicate("Achieve");
		ListIterator liSS = getState().listIterator();
		while (liSS.hasNext()) {
			oclPredicate opd = (oclPredicate) liSS.next();
			addArgument(ret, opd);
		}
		return ret;
	}

	/**
	 * addArgument - can be constants/objects as well as variables
	 * @param operatorName - the signature
	 * @param prd - a predicate to be searched for new arguments to add
	 */
	private void addArgument(oclPredicate operatorName, oclPredicate prd) {
		boolean gotIt = false;
		OPredicate.pArg newStr, existingStr;
		//get functor/name prd.getName
		// if name is ne ignore
		if (prd.getName().equals("ne")
			|| prd.getName().equals("is_of_sort")
			|| prd.getName().equals("is_of_primitive_sort")) {
			return;
		}
		// otherwise search predicates for matching prd.name
		List list = prd.getArguments();
		ListIterator li = list.listIterator();
		while (li.hasNext()) {
			newStr = (OPredicate.pArg) li.next();
			ListIterator liOPName =
				(operatorName.getArguments()).listIterator();
			while (liOPName.hasNext()) {
				existingStr = (OPredicate.pArg) liOPName.next();
				if (existingStr.name.equals(newStr.name)) {
					gotIt = true;
					break;
				}
			}
			if (!gotIt) {
				operatorName.addSortedVarArgument(
					new String(newStr.name),
					new String(newStr.sort));
			} else
				gotIt = false;
		}
	}

	/**
	 * to print the current oclSS to a PrintWriter.
	 * @param ps PrintWriter
	 * @param indent value of indentation
	 * @param nline not being used really
	 * d
	 */
	public void oclPrintComponent(PrintWriter ps, int indent, boolean nline) {
		String pad = "";
		for (int i = 0; i < indent; i++)
			pad = pad + " ";
		ps.print(pad + "ss(" + sort + "," + name + ",[");
		ListIterator li = state.listIterator();
		while (li.hasNext()) {
			// 3/7/03 Changed for ocl plus - print the value of functors
			oclPredicate cur = (oclPredicate) li.next();
			if (cur.isFluent()) {
					(new oclFunctor(cur)).oclPrintFluent(ps,0,false);
			} else {
				cur.oclPrintComponent(ps, 0, false);
			}
			if (li.hasNext())
				ps.print(",");
		}
		ps.print("])");
	}

	/* WZ 17/6/02 */
	/**
	 * a string expression, mainly used for saving purpose
	 * @return String
	 */
	public String to_String() {
		StringBuffer pad = new StringBuffer();
		pad.append("ss(" + sort + "," + name + ",[");
		ListIterator li = state.listIterator();
		while (li.hasNext()) {
//			 Ron 14/9/03 Updated for oclplus
			oclPredicate cur = (oclPredicate) li.next();
			if (cur.isFluent()) {
				oclFunctor newFunc = new oclFunctor(cur);
				pad.append(newFunc.toInstantiatedString());
			} else {
				pad.append(cur.toString());
			}
			if (li.hasNext())
				pad.append("^");
		}
		pad.append("])");
		return pad.toString();
	}
	
	/**
	 * a string expression, mainly used for saving purpose
	 * @return String
	 */
	public String toStdString() {
		StringBuffer pad = new StringBuffer();
		pad.append("ss(" + sort + "," + name + ",[");
		ListIterator li = state.listIterator();
		while (li.hasNext()) {
			pad.append(((oclPredicate) li.next()).toString());
			if (li.hasNext())
				pad.append(",");
		}
		pad.append("])");
		return pad.toString();
	}
}
