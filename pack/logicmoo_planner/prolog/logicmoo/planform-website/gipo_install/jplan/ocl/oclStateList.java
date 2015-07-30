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

import jplan.general.OEnviron;
import jplan.general.Utility;
import jplan.general.OPredicate;

/**
 * oclStateList used in substate class definitions
 * - Store Object Sub State Class Expressions.
 */
/* Weihong changed on 30/11/2001 */
public class oclStateList
	extends jplan.general.TransferableObject
	implements oclPrint, Serializable, Cloneable {
	private List preds = null;
	private List consumers = null; //Ron 24/7/01 reachability check
	private List producers = null; //Ron 24/7/01 reachability check
	// these store the operator names (functor names only) of ops
	// that use this state

	/**
	 * just create the ArrayList
	 */
	public oclStateList() {
		preds = new ArrayList();
		consumers = new ArrayList();
		producers = new ArrayList();
	}

	/**
	 * Add a new name
	 * @param name of the predicate
	 * @return oclPredicate - one clause added
	 */
	public oclPredicate addClause(String name) {
		oclPredicate cur = new oclPredicate(name);
		preds.add(cur);
		return cur;
	}

	/**
	 * Add complete predicate to the list
	 * @param pred predicate to add
	 */
	public void addPredicate(oclPredicate pred) {
		preds.add(pred);
	}

	/**
	 * return the state predicate list
	 * @return List of oclPredicates
	 */
	public List getPredicateList() {
		return preds;
	}
	/**
	 * getPredicateByName
	 * find a predicate in the state list 
	 * @param fName - the name of the predicate to look for
	 * @return - the predicate or null if not found
	 */
	public oclPredicate getPredicateByName(String fName){
		ListIterator li = preds.listIterator();
		oclPredicate retValue = null;
		while(li.hasNext() && retValue == null){
			oclPredicate cur = (oclPredicate)li.next();
			if (cur.getName().equals(fName)) {
				retValue = cur;
			}
		}
		return retValue;
	}

	/**
	 * set the state predicate list
	 */
	public void setPredicateList(List predList) {
		preds = predList;
	}

	/**
	 * get consumers
	 * @return the consumers list
	 */
	public List getConsumers() {
		return consumers;
	}

	/**
	 * get producers
	 * @return the producers list
	 */
	public List getProducers() {
		return producers;
	}

	/**
	 * reset producer consumer lists
	 * 
	 */
	public void clearProducersConsumers() {
		producers.clear();
		consumers.clear();
	}

	/**
	 * index state against operator as a consumer
	 * @param name the se Id
	 * @param id the state definition id
	 * @param strippedState stripped of ne and static predicates
	 * @param opName op functor name
	 * @return true if matching definition found
	 */
	public boolean seUsesStateList(
		String name,
		String id,
		List strippedState,
		String opName) {
		if (seUnifiesWithStateList(name, id, strippedState)) {
			consumers.add(opName);
			return true;
		} else {
			return false;
		}
	}

	/**
	 * index state against operator as a producer
	 * @param name the se Id
	 * @param id the state definition id
	 * @param strippedState stripped of ne and static predicates
	 * @param opName op functor name
	 * @return true if matching definition found
	 */
	public boolean ssUsesStateList(
		String name,
		String id,
		List strippedState,
		String opName) {
		if (ssUnifiesWithStateList(name, id, strippedState)) {
			producers.add(opName);
			return true;
		} else {
			return false;
		}
	}

	/**
	 * find a state definition to unify with the given elements 
	 * of an sub-state expression
	 * @param name the se Id
	 * @param id the state definition id
	 * @param strippedState stripped of ne and static predicates
	 * @return true if matching definition found
	 */
	public boolean seUnifiesWithStateList(
		String name,
		String id,
		List strippedState) {
		OEnviron env = new OEnviron();
		env.addBinding(id, OPredicate.genVariable(id));
		List rePreds = renameVarsInStateList(env);
		ListIterator li = strippedState.listIterator();
		env = new OEnviron();
		env.addBinding(id, name);
		boolean fail = false;
		while (!fail && li.hasNext()) {
			oclPredicate cur = (oclPredicate) li.next();
			ListIterator liState = rePreds.listIterator();
			boolean found = false;
			OEnviron tempEnv = null;
			try {
				tempEnv = (OEnviron) env.clone();
			} catch (CloneNotSupportedException e) {
				Utility.debugPrintln(
					"Unexpected failure to clone OEnvironment");

			}
			while (!found && liState.hasNext()) {
				oclPredicate target = (oclPredicate) liState.next();
				if (target.isStatic()) {
					// just have to ignore it NOT ADEQUATE
					continue;
				}
				if (cur.unify(target, tempEnv)) {
					found = true;
					env = tempEnv;
				}
			}
			if (!found) {
				// 		Utility.debugPrintln("seUnifiesWithStateList >> fail " +cur.toString());
				fail = true;
			}
		}
		return (!fail);
	}

	/**
	 * Assuming a hierarchical definition of object states
	 * find a state definition to that partially unifies 
	 * with the given elements 
	 * of an sub-state expression
	 * @param name the se Id
	 * @param id the state definition id
	 * @param strippedState stripped of ne and static predicates
	 * @param stateUnmatched predicates not matched - this is a returned value
	 * @return number of predicates in statelist unifying
	 */
	public int sePartiallyUnifiesWithStateList(
		String name,
		String id,
		List strippedState,
		List stateUnmatched) {
		int noFound = 0;
		OEnviron env = new OEnviron();
		env.addBinding(id, OPredicate.genVariable(id));
		List rePreds = renameVarsInStateList(env);
		ListIterator li = strippedState.listIterator();
		env = new OEnviron();
		env.addBinding(id, name);
		while (li.hasNext()) {
			oclPredicate cur = (oclPredicate) li.next();
			ListIterator liState = rePreds.listIterator();
			boolean found = false;
			OEnviron tempEnv = null;
			try {
				tempEnv = (OEnviron) env.clone();
			} catch (CloneNotSupportedException e) {
				Utility.debugPrintln(
					"Unexpected failure to clone OEnvironment");

			}
			while (!found && liState.hasNext()) {
				oclPredicate target = (oclPredicate) liState.next();
				if (target.isStatic()) {
					// just have to ignore it NOT ADEQUATE
					continue;
				}
				if (cur.unify(target, tempEnv)) {
					found = true;
					noFound++;
					env = tempEnv;
				}
			}
			if (!found) {
				// 		Utility.debugPrintln("seUnifiesWithStateList >> fail " +cur.toString());
				stateUnmatched.add(cur);
			}
		}
		return (noFound);
	}

	/**
	 * find a state definition to unify with the given elements 
	 * of an sub-state description
	 * @param name the ss/sc Id
	 * @param id the state definition id
	 * @param strippedState stripped of static predicates
	 * @return true if matching definition found
	 */
	public boolean ssUnifiesWithStateList(
		String name,
		String id,
		List strippedState) {
		OEnviron env = new OEnviron();
		env.addBinding(id, OPredicate.genVariable(id));
		List rePreds = renameVarsInStateList(env);
		ListIterator li = strippedState.listIterator();
		env = new OEnviron();
		env.addBinding(id, name);
		boolean fail = false;
		int matches = 0;
		int ignoredNEs = 0;
		int ignoredStatics = 0;
		int ignored_isofsort = 0; // Ron 29/9/01
		ListIterator liState = rePreds.listIterator();
		while (liState.hasNext()) {
			oclPredicate target = (oclPredicate) liState.next();
			if (target.getName().equals("is_of_sort"))
				ignored_isofsort++;
		}
		while (!fail && li.hasNext()) {
			oclPredicate cur = (oclPredicate) li.next();
			liState = rePreds.listIterator();
			boolean found = false;
			OEnviron tempEnv = null;
			try {
				tempEnv = (OEnviron) env.clone();
			} catch (CloneNotSupportedException e) {
				Utility.debugPrintln(
					"Unexpected failure to clone OEnvironment");

			}
			while (!found && liState.hasNext()) {
				oclPredicate target = (oclPredicate) liState.next();
				if (cur.unify(target, tempEnv)) {
					found = true;
					matches++;
					env = tempEnv;
				}
			}

			if (!found) {
				if (cur.getName().equals("ne")) {
					ignoredNEs++;
				} else {
					fail = true;
				}
			} else {
				if (cur.getName().equals("is_of_sort")) {
					// dont ignore it we matched it
					ignored_isofsort--;
				}
			}
		}
		if (preds.size() - ignored_isofsort
			> strippedState.size() - ignoredNEs) {
			int stateSkips = 0;
			liState = rePreds.listIterator();
			while (liState.hasNext()) {
				oclPredicate target = (oclPredicate) liState.next();
				if (target.getName().equals("ne") || target.isStatic()) {
					stateSkips++;
				}
			}
			if (preds.size() - stateSkips > strippedState.size() - ignoredNEs)
				fail = true;
			else
				fail = false;
		}
		return (!fail);
	}

	/**
	 * Assuming a hierarchical definition of object states
	 * find a state definition to that partially unifies 
	 * with the given elements 
	 * of an sub-state definition
	 * @param name the ss Id
	 * @param id the state definition id
	 * @param strippedState stripped of ne and static predicates
	 * @param stateUnmatched predicates not matched - this is a returned value
	 * @return number of predicates in statelist unifying
	 */
	public int ssPartiallyUnifiesWithStateList(
		String name,
		String id,
		List strippedState,
		List stateUnmatched) {
		OEnviron env = new OEnviron();
		env.addBinding(id, OPredicate.genVariable(id));
		List rePreds = renameVarsInStateList(env);
		ListIterator li = strippedState.listIterator();
		env = new OEnviron();
		env.addBinding(id, name);
		boolean fail = false;
		int matches = 0;
		int ignoredNEs = 0;
		int ignoredStatics = 0;
		int ignored_isofsort = 0; // Ron 29/9/01
		ListIterator liState = rePreds.listIterator();
		while (liState.hasNext()) {
			oclPredicate target = (oclPredicate) liState.next();
			if (target.getName().equals("is_of_sort"))
				ignored_isofsort++;
		}
		while (!fail && li.hasNext()) {
			oclPredicate cur = (oclPredicate) li.next();
			liState = rePreds.listIterator();
			boolean found = false;
			OEnviron tempEnv = null;
			try {
				tempEnv = (OEnviron) env.clone();
			} catch (CloneNotSupportedException e) {
				Utility.debugPrintln(
					"Unexpected failure to clone OEnvironment");

			}
			while (!found && liState.hasNext()) {
				oclPredicate target = (oclPredicate) liState.next();
				if (cur.unify(target, tempEnv)) {
					found = true;
					matches++;
					env = tempEnv;
				}
			}

			if (!found) {
				if (cur.getName().equals("ne")) {
					ignoredNEs++;
				} else {
					stateUnmatched.add(cur);
				}
			} else {
				if (cur.getName().equals("is_of_sort")) {
					// dont ignore it we matched it
					ignored_isofsort--;
				}
			}
		}
		if (preds.size() - ignored_isofsort
			> strippedState.size() - ignoredNEs) {
			int stateSkips = 0;
			liState = rePreds.listIterator();
			while (liState.hasNext()) {
				oclPredicate target = (oclPredicate) liState.next();
				if (target.getName().equals("ne") || target.isStatic()) {
					stateSkips++;
				}
			}
			if (matches > 0 &&
				preds.size() - stateSkips > strippedState.size() - ignoredNEs) {
				// We have not matched all the state definition so
				// ignore any matches made this cannot be the correct state
				matches = -1;

			}
		}
		return (matches);
	}

	/**
	 * rename all variable to unique new names
	 * @return list of predicates
	 */
	private List renameVarsInStateList(OEnviron env) {
		List ret = new ArrayList();
		ListIterator li = preds.listIterator();
		while (li.hasNext()) {
			oclPredicate cur = (oclPredicate) li.next();
			oclPredicate renamed = cur.renameVars(env);
			ret.add(renamed);
		}
		return ret;
	}
	
	

	/**
	 * replace all occurances of a given argument by new given name
	 * @param oldName old name
	 * @param newName new name
	 */
	public void renameArgInState(String oldName, String newName) {
		ListIterator li = preds.listIterator();
		while (li.hasNext()) {
			oclPredicate cur = (oclPredicate) li.next();
			try {
				cur.replaceVariableNameByName(oldName, newName);
			} catch (Exception e) {
				//ignore
			}
		}
	}
	
	/**
	 * renamePredArgInState
	 * find state predicate with given name and change all occurances of variable oldName
	 * with newname
	 * @param predName
	 * @param oldName
	 * @param newName
	 */
	public void renamePredArgInState(String predName, String oldName, String newName) {
		ListIterator li = preds.listIterator();
		while (li.hasNext()) {
			oclPredicate cur = (oclPredicate) li.next();
			if (cur.getName().equals(predName))
			try {
				cur.replaceVariableNameByName(oldName, newName);
			} catch (Exception e) {
				//ignore
			}
		}
	}

	/**
	 * renamePropertyArgInState
	 * find all predicates that reference oldName and change to NewName
	 * except if oldName is the state ID (i.e. first argument in a predicate) in which case only change the reference in predName
	 * @param predName
	 * @param oldName
	 * @param newName
	 */
	public void renamePropertyArgInState(String predName, String oldName, String newName) {
		boolean isStateID = false;
		ListIterator li = preds.listIterator();
		while (!isStateID && li.hasNext()) {
			oclPredicate cur = (oclPredicate) li.next();
			if (oldName.equals(cur.getNthElementName(0))) {
				isStateID = true;
			}
		}
		if (isStateID) {
			renamePredArgInState(predName,oldName,newName);
		} else {
			renameArgInState(oldName,newName);
		}
	}	
	
	/**
	 * clone a copy of oclStatelist
	 * @throws CloneNotSupportedException
	 * @return Object
	 */
	public Object clone() throws CloneNotSupportedException {
		try {
			oclStateList osl = new oclStateList();
			ListIterator li = preds.listIterator();
			while (li.hasNext()) {
				osl.preds.add(((oclPredicate) li.next()).clone());
			}
			return osl;
		} catch (CloneNotSupportedException e) {
			throw e;
		}
	}

	/**
	 * simple string representation each predicate
	 * @return String
	 */
	public String toString() {
		String res = "";
		ListIterator li = preds.listIterator();
		while (li.hasNext()) {
			res = res.concat(((oclPredicate) li.next()).toString());
			if (li.hasNext())
				res = res.concat(new String(" "));
		}
		return res;
	}

	/**
	 * print details of reachability analysis
	 * @param ps PrintWriter
	 */
	public void printReachabilityAnalysis(PrintWriter ps) {
		ps.println("    State: " + toString());
		ps.print("        Consumed by : ");
		ListIterator li = consumers.listIterator();
		while (li.hasNext()) {
			ps.print((String) li.next());
			if (li.hasNext()) {
				ps.print(", ");
			}
		}
		ps.println(";");
		ps.print("        Produced by : ");
		li = producers.listIterator();
		while (li.hasNext()) {
			ps.print((String) li.next());
			if (li.hasNext()) {
				ps.print(", ");
			}
		}
		ps.println(";");
	}

	/**
	 * to print the current oclStatelist to a PrintWriter.
	 * @param ps PrintWriter
	 * @param indent value of indentation
	 * @param nline not being used really
	 * 
	 */
	public void oclPrintComponent(PrintWriter ps, int indent, boolean nline) {
		String pad = "";
		for (int i = 0; i < indent; i++)
			pad = pad + " ";
		ps.print(pad + "[");
		ListIterator li = preds.listIterator();
		while (li.hasNext()) {
			((oclPredicate) li.next()).oclPrintComponent(ps, 0, false);
			if (li.hasNext())
				ps.print(",");
		}
		ps.print("]");
	}
}
