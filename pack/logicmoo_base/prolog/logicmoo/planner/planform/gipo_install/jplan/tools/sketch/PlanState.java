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
 

 /**
 * Created on 04-Sep-2003
 *
 * Author ron
 */
package jplan.tools.sketch;


import java.util.*;

import jplan.ocl.*;
import jplan.general.OEnviron;
import jplan.general.Utility;
import jplan.general.OCLException;

/**
 * @author ron
 *
 * This List maintains a list of object states (oclSS clauses)
 */
public class PlanState {

	/**
	 * time is the time the latest object state was updated by either the application
	 * of an action or an event.
	 * State is not advanced by to updating of processes
	 */	
	private int time;
	
	/**
	 * the current domain
	 */
	private oclDomain curDom;
	
	/**
	 * maintain a list of object states for all dynamic objects in the current task
	 */
	private List lstObjects;
	
	/**
	 * maintain a list of references to the functors/fluents in the state
	 */
	private List lstStateFluents;
	private List staticFluents;
	
	/**
	 * constructor initialises the state to the initial state of the current task
	 * @param initState
	 * @param the problem domain
	 */
	public PlanState(oclTask task,oclDomain dom) {
		curDom = dom;
		lstObjects = new ArrayList();
		lstStateFluents = new ArrayList();
		staticFluents = new ArrayList();
		ListIterator li = task.getInits().listIterator();
		while (li.hasNext()) {
			try {
				oclSS objState = (oclSS)((oclSS)li.next()).clone();
				lstObjects.add(objState);
			} catch (CloneNotSupportedException e){}
		}
		collectFluents();
		time = 0; // The plan start time
	}
	

	
	/**
		 * constructor initialises the state to that of a given state
		 * @param oldState
		 */
		public PlanState(PlanState old) {
			curDom = old.curDom;
			time = old.time;
			lstObjects = new ArrayList();
			ListIterator li = old.lstObjects.listIterator();
			while (li.hasNext()) {
				oclSS objState = (oclSS)li.next(); // Don't clone will be done on an as needed basis
				lstObjects.add(objState);
			}
			lstStateFluents = new ArrayList(old.lstStateFluents.size());
			staticFluents = new ArrayList();
			collectFluents();
		}
		
		/**
		 * makeTransition
		 * apply an oclSCPlus transition to this state
		 * @param curSC
		 */
	  public void makeTransition(oclSCPlus curSC){
		oclSS newState = null;
	  	ListIterator li = lstObjects.listIterator();
	  	boolean found = false;
	  	int index = 0;
	  	while (! found && li.hasNext()) {
	  		oclSS objState = (oclSS)li.next();
	  		if (objState.getName().equals(curSC.getName())) {
	  			// This is the correct object
	  			found = true;
	  			newState = new oclSS(curSC.getSort(),curSC.getName());
	  			ListIterator  liRhs = curSC.getPost().listIterator();
	  			while (liRhs.hasNext()) {
	  				oclPredicate change = (oclPredicate)liRhs.next();
	  				try {
	  					oclPredicate curPred = (oclPredicate)change.clone();
	  					// Ignore statics but shouln't really be here
	  					if (! curPred.isStatic()) {
	  						newState.addPredicate(curPred);
	  					}
	  				} catch (CloneNotSupportedException e){}
	  				
	  			}
	  			// Now do the fluent updates
	  			liRhs = curSC.getStateUpdates().listIterator();
	  			while (liRhs.hasNext()) {
	  				try {
	  					oclExpression change = (oclExpression)liRhs.next();
	  					oclPredicate newFluent = change.updateFluent(lstStateFluents);
	  					newState.addPredicate(newFluent);
//	  					updateFluentList(newFluent);
	  				} catch (OCLException e) {
	  					Utility.debugPrintln("Impossible update: " + e.toString());
	  				}
	  			}
	  			
	  		} else {
	  			index ++;
	  		}
	  	}
	  	lstObjects.remove(index);
	  	lstObjects.add(index,newState);
	  	
	  }
	  
	/**
	 * makeProcessTransition
	 * apply an oclSCPlus derived from a process transition to this state
	 * @param curSC
	 */
	public void makeProcessTransition(oclSCPlus curSC) {
		ListIterator li = lstObjects.listIterator();
		boolean found = false;
		int index = 0;
		while (!found && li.hasNext()) {
			oclSS objState = (oclSS) li.next();
			if (objState.getName().equals(curSC.getName())) {
				// This is the correct object
				found = true;
				// Now do the fluent updates
				ListIterator liRhs = curSC.getStateUpdates().listIterator();
				while (liRhs.hasNext()) {
					try {
						oclExpression change = (oclExpression) liRhs.next();
						oclPredicate newFluent =
							change.updateFluentList(lstStateFluents);
						//							updateFluentList(newFluent);
					} catch (OCLException e) {
						Utility.debugPrintln(
							"Impossible update: " + e.toString());
					}
				}

			} else {
				index++;
			}
		}
	}
	
	/**
	 * getObjectState
	 * find the state of the given object
	 * @param the object name
	 * @return the object state
	 */
	public oclSS getObjectState(String objName) {
		oclSS ret = null;
		ListIterator li = lstObjects.listIterator();
		while (li.hasNext()) {
			oclSS objState = (oclSS)(oclSS)li.next();
			if (objState.getName().equals(objName)) {
				return objState;
			}
		}
		return ret;
	}
	
	// The following added for conditional effects
	// ron 23/05/05
	/**
	 * getObjectStates
	 * find the states of the given object sort
	 * @param the object sort
	 * @return List of objects of sort
	 */
	public List getObjectStates(String objSort) {
		List matchObjects = new ArrayList();
		ListIterator li = lstObjects.listIterator();
		while (li.hasNext()) {
			oclSS objState = (oclSS)(oclSS)li.next();
			if (objState.getSort().equals(objSort)) {
				matchObjects.add(objState);
			}
		}
		return matchObjects;
	}
	
	/**
	 * objectStateMatches
	 * Test to see if the given object instantiated state matches at the current state time
	 * @param objState - an se state description
	 * @param mssgs - a message list to populate if the state fails to unify
	 * @return true if matches
	 */
	public boolean objectStateMatches(oclSEPlus objState, List mssgs) {
		boolean ret = true;
		oclSS curState = getObjectState(objState.getName());
		if (curState == null) {
			mssgs.add(
				"There are no recorded state descriptions for the given object "
					+ objState.getName());
			return false;
		}
		// First check the object states
		ListIterator liObj = objState.getState().listIterator();
		while (liObj.hasNext()) {
			oclPredicate objPred = (oclPredicate) liObj.next();
			if (objPred.isStatic()) {
				if (!staticIsTrue(objPred)) {
					mssgs.add(
						"The static predicate "
							+ objPred.toString()
							+ " is not true in this domain.");
					return false; //Problem false static
				}
			} else if (objPred.getName().equals("ne")) {
				if (objPred.getNthElementName(0).equals(objPred.getNthElementName(1))) {
					mssgs.add(
							"The ne predicate "
							+ objPred.toString()
							+ " is not true.");
					return false;
				}
			} else {
				ListIterator liState = curState.getState().listIterator();
				boolean found = false;
				while (!found && liState.hasNext()) {
					oclPredicate statePred = (oclPredicate) liState.next();
					if (objPred.getName().equals(statePred.getName())) {
						// Possible match
						if (objPred.toString().equals(statePred.toString())) {
							found = true;

						} else {
							// found the object but it failed to match
							mssgs.add(
								"Object Description ["
									+ objPred.toString()
									+ "] does not match actual state ["
									+ statePred.toString()
									+ "]");
							return false;
						}
					}
				}
				if (found == false) {
					mssgs.add(
						"Predicate "
							+ objPred.toString()
							+ " does not appear in the recorded state.");
					return false;
				}
			}
		}
		// Now check the fluents
		ListIterator liTests = objState.getFluentConditions().listIterator();
		while (liTests.hasNext()) {
			oclExpression testClause = (oclExpression) liTests.next();
			// This must be a test expression
			Utility.debugPrintln("patterns","Test Expression " + testClause.toString());
			oclExpression cond = testClause.getRHS();
			boolean res = false;
			try {
				res = cond.evalBoolean(lstStateFluents);
			} catch (OCLException e) {
				Utility.debugPrintln(e.toString());
				mssgs.add("Error cannot evaluate fluent.");
				return false;
			}
			if (!res) {
				mssgs.add(
					"The condition "
						+ cond.toString()
						+ " is false in the current state.");
				return false;
			}

		}
		return true;
	}

	/**
	 * objectStateMatches
	 * Test to see if the given RHS of an operator  matches an
	 *  object instantiated state
	 * Need to see if any object states unify with LHS
	 * @param objState - an se state description
	 * @param matches
	 * @return list if matches
	 */
	public List objectStateMatchesConditional(oclSEPlus objLHS) {
		List matches = new ArrayList();
		List possStates = getObjectStates(objLHS.getSort());
		oclSEPlus objState = null;
		// First check the object states
		ListIterator liObj = possStates.listIterator();
		while (liObj.hasNext()) {
			try {
				objState = (oclSEPlus)objLHS.clone();
			} catch (CloneNotSupportedException ex){}
			oclSS possState = (oclSS) liObj.next();
			OEnviron env = new OEnviron();
			List mssgs = new ArrayList();
			if (unifiesSSwithAbstractSE(possState.getState(),objState.getStrippedState(),env,mssgs)){
				Utility.debugPrintln("patterns",possState.toSugarString());
				List stateList = objState.getPredicateList();
				instantiate_state(stateList, env);
				objState.setName(possState.getName());
				// More debug Lines
				ListIterator templi = stateList.listIterator();
				while (templi.hasNext()) {
					oclPredicate temp = (oclPredicate) templi.next();
					Utility.debugPrintln("patterns",
						">>>+ State predicate " + temp.toString());

				}
				// End debug lines
				if ( checkAbstractStaticsAndBuiltIns(
					curDom,
					stateList,
					env,
					mssgs)) {
					instantiate_state(stateList, env);
					//More debug Lines
					templi = stateList.listIterator();
					while (templi.hasNext()) {
						oclPredicate temp = (oclPredicate) templi.next();
						Utility.debugPrintln(
						">>>+ State predicate " + temp.toString());

					}
				}
				matches.add(new Object []{objState,env});
			}
		}
//		// Now check the fluents
//		ListIterator liTests = objState.getFluentConditions().listIterator();
//		while (liTests.hasNext()) {
//			oclExpression testClause = (oclExpression) liTests.next();
//			// This must be a test expression
//			Utility.debugPrintln("patterns","Test Expression " + testClause.toString());
//			oclExpression cond = testClause.getRHS();
//			boolean res = false;
//			try {
//				res = cond.evalBoolean(lstStateFluents);
//			} catch (OCLException e) {
//				Utility.debugPrintln(e.toString());
//				return false;
//			}
//			if (!res) {
//				return false;
//			}
//
//		}
		return matches;
	}
	//Ron 23/05/05 Added to deal with conditional effects
	/**
	 * check between two states to see if the 
	 * given states are all included in the current states.
	 * The SE list may only be partially instantiated
	 * @param SSList List of oclPredicate (SS)
	 * @param SEList List of oclPredicate (SE)
	 * @param env Environment of bindings
	 * @param mssgs the error message list
	 * @return boolean
	 */
	public static boolean unifiesSSwithAbstractSE(
		List SSList,
		List SEList,
		OEnviron env,
		List mssgs) {
		OEnviron envTemp = new OEnviron();
		List temp = new ArrayList();
		boolean found = false;
		ListIterator liSE = SEList.listIterator();
		while (liSE.hasNext()) { //for every predicate in the SE
			oclPredicate oprd = (oclPredicate) liSE.next();
			ListIterator liSS = SSList.listIterator();
			if (SSList.size() == 0) {
				mssgs.add("HELP no predicates in the state for the object");
			}
			while (liSS.hasNext()) { //for every predicate in the SS
				oclPredicate basePred = (oclPredicate) liSS.next();
				OEnviron envCopy = null;
				try {
					envCopy = (OEnviron) envTemp.clone();
				} catch (CloneNotSupportedException e) {
					// Ignore
				}
				if (oprd.unify(basePred, envCopy)) {
					Utility.debugPrintln("Found " + oprd.toString());
					found = true;
					envTemp = envCopy;
					break;
				} else {
					temp.add(
						"    "
							+ oprd.toString()
							+ " << fails to match >>  "
							+ basePred.toString());
				}

			}
			if (!found) { //for every predicate in the SE there must be an exact match in SS
				mssgs.addAll(temp);
				return false;
			} else
				found = false; //reset found for next iteration
		}
		env.copy(envTemp);
		return true;
	}
	// Ron 23/05/05 added for conditional effects
	/**
	 * Instantiate_state
	 * given an environment replaces variables with the binding recorded in
	 * the environment
	 * @param stateList - State List Partially instantiates se
	 * @param env - environment of bindings
	 * 
	 */
	public static void instantiate_state(List stateList, OEnviron env) {
		ListIterator li = stateList.listIterator();
		while (li.hasNext()) {
			oclPredicate cur = (oclPredicate) li.next();
			cur.instantiate(env);
		}
	}
	
	// Ron 23/05/05 added for conditional effects
	/**
	 * find ne & is_of_sort & statics and check
	 * Statics may not be fully instantiated
	 * @param workingDomain
	 * @param preds predicate list
	 * @param env
	 * @param mssgs Error message list
	 * @return boolean true if all checks passed
	 */
	public static boolean checkAbstractStaticsAndBuiltIns(
		oclDomain workingDomain,
		List preds,
		OEnviron env,
		List mssgs) {
		ListIterator li = preds.listIterator();
		OEnviron envTemp = new OEnviron();
		envTemp.copy(env);
		boolean allOK = true;
		while (allOK && li.hasNext()) {
			oclPredicate cur = (oclPredicate) li.next();
			if ("ne".equals(cur.getName())) {
				if (cur
					.getNthElementName(0)
					.equals(cur.getNthElementName(1))) {
					allOK = false;
					mssgs.add(
						"The inequality " + cur.toString() + " is false.");
				}
			} else if ("is_of_sort".equals(cur.getName())) {
				String objSort =
					workingDomain.getSortOfObject(cur.getNthElementName(0));
				String reqSort = cur.getNthElementName(1);
				if (!objSort.equals(reqSort)
					&& !workingDomain.sortIsSubSortOf(objSort, reqSort)) {
					allOK = false;
					mssgs.add(
						"The sort restriction "
							+ cur.toString()
							+ " is not met.");
				}
			} else if (cur.isStatic()) {
				Utility.debugPrintln("patterns","Finding Match for " + cur.toString());
				ListIterator liAtomic =
					workingDomain.atomicInvars.listIterator();
				boolean found = false;
				while (!found && liAtomic.hasNext()) {
					oclPredicate atom = (oclPredicate) liAtomic.next();
					OEnviron envCopy = null;
					try {
						envCopy = (OEnviron) envTemp.clone();
					} catch (CloneNotSupportedException e) {
						// Ignore
					}
					if (cur.unify(atom,envCopy)) {
						found = true;
						envTemp = envCopy;
					}
				}
				if (!found) {
					allOK = false;
					mssgs.add(
						"The atomic invariant "
							+ cur.toString()
							+ " is not met.");
				}
			}
		}
		env.copy(envTemp);
		return allOK;

	}

	
	/**
	 * refreshFluents
	 * Update fluent list after some update of object states
	 */
	public void refreshFluentList() {
		lstStateFluents.clear();
		ListIterator liObjs = lstObjects.listIterator();
		while (liObjs.hasNext()) {
			oclSS objState = (oclSS)liObjs.next();
			ListIterator liPreds = objState.getState().listIterator();
			while (liPreds.hasNext()) {
				oclPredicate clause = (oclPredicate)liPreds.next();
				if (clause.isFluent()) {
					lstStateFluents.add(clause);
				}
			}
		}
		// Now add the static fluents
		lstStateFluents.addAll(staticFluents);
			
	}
	
	private void collectFluents(){
		ListIterator liObjs = lstObjects.listIterator();
		while (liObjs.hasNext()) {
			oclSS objState = (oclSS)liObjs.next();
			ListIterator liPreds = objState.getState().listIterator();
			while (liPreds.hasNext()) {
				oclPredicate clause = (oclPredicate)liPreds.next();
				if (clause.isFluent()) {
					lstStateFluents.add(clause);
				}
			}
		}
		// Now add the static fluents
		ListIterator liStatics = curDom.atomicInvars.listIterator();
		while (liStatics.hasNext()) {
			oclPredicate cur = (oclPredicate)liStatics.next();
			if (cur.isFluent()) {	
				lstStateFluents.add(cur);
				staticFluents.add(cur);
			}
		}
	}

	/**
	 * objectStateMatches
	 * Test to see if the given object instantiated state matches at the current state time
	 * @param objState - an se state description
	 * @param mssgs - a message list to populate if the state fails to unify
	 * @return true if matches
	 */
	public boolean objectStateMatches(oclSCPlus objState, List mssgs) {
		oclSEPlus equivSE = new oclSEPlus(objState);
		return objectStateMatches(equivSE, mssgs);
	}
	
	/**
	 * objectStateMatchesConditional
	 * Test to see if the given object instantiated state matches at the current state time
	 * @param objState - an se state description
	 * @return List of matching scclauses and instantiating environment
	 */
	public List objectStateMatchesConditional(oclSCPlus condSC) {
		oclSEPlus equivSE = new oclSEPlus(condSC);
		List matches = objectStateMatchesConditional(equivSE);
		ListIterator li = matches.listIterator();
		while (li.hasNext()) {
			Object[] match = (Object[])li.next();
			oclSCPlus matchSC = null;
			try {
				matchSC = (oclSCPlus)condSC.clone();
			} catch (CloneNotSupportedException e){}
			oclSEPlus matchSE = (oclSEPlus)match[0];
			matchSC.setName(matchSE.getName());
			instantiate_state(matchSC.getPre(),(OEnviron)match[1]);
			instantiate_state(matchSC.getPost(),(OEnviron)match[1]);
			match[0] = matchSC;
			// TODO think about fluents 
		}
		return matches;	
	}


	/**
	 * staticIsTrue
	 * test the static for current domain model
	 * @param sPred
	 * @return - true if true
	 */
	private boolean staticIsTrue(oclPredicate sPred) {
		boolean found = false;
		ListIterator li = curDom.atomicInvars.listIterator();
		while (!found && li.hasNext()) {
			oclPredicate pred = (oclPredicate) li.next();
			if (pred.toString().equals(sPred.toString()))
				found = true;
		}
		return found;
	}
	
	/**
	 * toString
	 * This is really just for debugging purposes
	 * @return - the string representation
	 */
	public String toString() {
		String res = "";
		ListIterator li = lstObjects.listIterator();
		while (li.hasNext()) {
			oclSS ss = (oclSS)li.next();
			res = res.concat(ss.toString() + "\n");
		}
		return res;
	}
	/**
	 * the time of the state
	 * @return
	 */
	public int getTime() {
		return time;
	}

	/**
	 * set the time the state applies to
	 * @param i
	 */
	public void setTime(int i) {
		time = i;
	}

}
