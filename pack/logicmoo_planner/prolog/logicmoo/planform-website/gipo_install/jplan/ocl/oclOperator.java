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
import java.util.Hashtable;
import java.io.*;
import jplan.general.OPredicate; /* Weihong added on 01/06/2001 */
import jplan.general.Utility;

/**
 * Top level class to store ocl primitive operator.
 */
/* Weihong added on 30/11/2001 */
public class oclOperator
	extends jplan.general.TransferableObject
	implements Cloneable, oclPrint, Serializable { /* Weihong added on 04/06/2001 */
	public oclPredicate opName;
	protected List docm = new ArrayList();
	protected List prevail = new ArrayList();
	protected List necessary = new ArrayList();
	protected List conditional = new ArrayList();
	///* Weihong added on 29/05/2001 */
	private List varibles = new ArrayList();
	//list of varibles with 'type' information

	/**
	 * Add a new name
	 * @param fname name of the predicate
	 * @return oclPredicate name of the operator
	 */
	public oclPredicate addName(String fname) {
		oclPredicate cur = new oclPredicate(fname);
		opName = cur;
		return cur;
	}

	/* WZ 19/4/02 */
	/**
	 * create the name with sorted variables
	 * find all predicate parameters and their sorts and form the operator name
	 * ignore conditional effect
	 * @param curDomain oclDomain
	 * @return oclPredicate - the signature / name
	 */
	public oclPredicate createOperatorName(oclDomain curDomain) {
		oclPredicate variblePredicate = new oclPredicate(opName.getName());

		ListIterator li, lii;
		//oclSE
		li = prevail.listIterator();
		while (li.hasNext()) {
			oclSE se = (oclSE) li.next();
			lii = se.getPredicateList().listIterator();
			while (lii.hasNext()) {
				curDomain.addSignatureArgument(
					variblePredicate,
					(oclPredicate) lii.next());
			}
		}

		//oclSC 
		li = necessary.listIterator();
		while (li.hasNext()) {
			oclSC necessary = (oclSC) li.next();
			lii = necessary.getPre().listIterator();
			while (lii.hasNext()) {
				curDomain.addSignatureArgument(
					variblePredicate,
					(oclPredicate) lii.next());
			}
			lii = necessary.getPost().listIterator();
			while (lii.hasNext()) {
				curDomain.addSignatureArgument(
					variblePredicate,
					(oclPredicate) lii.next());
			}
		}

		return variblePredicate;
	}

	/* WZ 21/6/02 */
	/**
	 * createAllSignature
	 * find all predicate pArguments (variable and constant)
	 * and their sorts and form the compound operator name
	 * \nIgnore conditional effect.
	 * @param domain
	 * @return oclPredicate - the signature / name
	 */
	public oclPredicate createAllSignature(oclDomain domain) {
		oclPredicate variblePredicate = new oclPredicate(opName.getName());

		ListIterator li, lii;
		//oclSE
		li = getPrevail().listIterator();
		while (li.hasNext()) {
			oclSE se = (oclSE) li.next();
			lii = se.getPredicateList().listIterator();
			while (lii.hasNext()) {
				domain.addAllArgument(
					variblePredicate,
					(oclPredicate) lii.next());
			}
		}

		//oclSC 
		li = getNecessary().listIterator();
		while (li.hasNext()) {
			oclSC necessary = (oclSC) li.next();
			lii = necessary.getPre().listIterator();
			while (lii.hasNext()) {
				domain.addAllArgument(
					variblePredicate,
					(oclPredicate) lii.next());
			}
			lii = necessary.getPost().listIterator();
			while (lii.hasNext()) {
				domain.addAllArgument(
					variblePredicate,
					(oclPredicate) lii.next());
			}
		}

		setName(variblePredicate);

		return variblePredicate;
	}

	/**
	 * Add Prevail condition
	 * @param sort sort of the SE
	 * @param name name of the SE
	 * @return oclSE
	 */
	public oclSE addPrevSE(String sort, String name) {
		oclSE cur = new oclSE(sort, name);
		prevail.add(cur);
		return cur;
	}

	/* Weihong added on 25/04/2001 */
	/**
	 * Sets the name of the opeartor
	 * @param name name of the opeartor
	 * 
	 */
	public void setName(oclPredicate name) {
		opName = name;
	}

	/* Weihong added on 25/04/2001 */
	/**
	 * Add Prevail condition
	 * @param cur Prevail condition
	 * 
	 */
	public void addPrevSE(oclSE cur) {
		prevail.add(cur);
	}

	// Ron 25/8/03
	/**
	 * Add Prevail condition
	 * @param fname name of the SE
	 * @param sort sort of the SE
	 * @return oclSE
	 */
	public oclSEPlus addPrevSEPlus(String sort, String name) {
		oclSEPlus cur = new oclSEPlus(sort, name);
		prevail.add(cur);
		return cur;
	}

	/**
	 * Add necessary change
	 * @param sort sort of the SC
	 * @param name name of the SC
	 * @return oclSC
	 */
	public oclSC addNecSC(String sort, String name) {
		oclSC cur = new oclSC(sort, name);
		necessary.add(cur);
		return cur;
	}

	/* Weihong added on 25/04/2001 */
	/**
	 * Add necessary change
	 * @param cur necessary change
	 * 
	 */
	public void addNecSC(oclSC cur) {
		necessary.add(cur);
	}

	/**
	 * Add necessary change
	 * @param fname name of the SC
	 * @param sort sort of the SC
	 * @return oclSC
	 */
	public oclSCPlus addNecSCPlus(String sort, String name) {
		oclSCPlus cur = new oclSCPlus(sort, name);
		necessary.add(cur);
		return cur;
	}

	/**
	 * Add conditional change
	 * @param sort sort of the SC
	 * @param name name of the SC
	 * @return oclSC
	 */
	public oclSC addCondSC(String sort, String name) {
		oclSC cur = new oclSC(sort, name);
		conditional.add(cur);
		return cur;
	}

	/* Weihong added on 25/04/2001 */
	/**
	 * Add conditional change
	 * @param cur conditional change
	 * 
	 */
	public void addCondSC(oclSC cur) {
		conditional.add(cur);
	}
	/**
	 * Add conditional change
	 * @param fname name of the SC
	 * @param sort sort of the SC
	 * @return oclSC
	 */
	public oclSCPlus addCondSCPlus(String sort, String name) {
		oclSCPlus cur = new oclSCPlus(sort, name);
		conditional.add(cur);
		return cur;
	}

	/* Weihong added on 12/04/2001 */
	/**
	 * Get the prevail condition.
	 * @return the prevail condition.
	 */
	public List getPrevail() {
		return prevail;
	}

	/* Weihong added on 12/04/2001 */
	/**
	 * Get the necessary change.
	 * @return the necessary change.
	 */
	public List getNecessary() {
		return necessary;
	}

	/* Weihong added on 12/04/2001 */
	/**
	 * Get the conditional change.
	 * @return the conditional change.
	 */
	public List getConditional() {
		return conditional;
	}

	/**
	 * addDocmLine
	 * add a comment line of documentation to the operator
	 * @param line - the documentation line
	 */
	public void addDocmLine(String line) {
		docm.add(line);
	}
	/**
	 * check for presence of identical se prevail clause
	 * @param se
	 * @return true if found
	 */
	public boolean containsPrevail(oclSE se) {
		ListIterator li = prevail.listIterator();
		while (li.hasNext()) {
			oclSE cur = (oclSE) li.next();
			if (cur.equals(se))
				return true;
		}
		return false;
	}

	/* Weihong added on 18/04/2001 */
	/**
	 * Returns a string representation of the oclOperator
	 * @return String
	 */
	public String toString() {
		return new String(opName.toString());
	}

	/**
	 * do verification checks on operators.<br>
	 * check each clause is for a different object;
	 * check each transition 
	 * @param curDom the current domain
	 * @param mssgs list to append results messages to
	 * @return true if all checks passed
	 */
	public boolean check(oclDomain curDom, List mssgs) {
		List ids = new ArrayList();
		List sorts = new ArrayList();
		boolean ret = true;

		mssgs.add("Checking operator " + opName.toString());
		ListIterator li = prevail.listIterator();
		while (li.hasNext()) {
			oclSE cur = (oclSE) li.next();
			if (alreadyUsed(cur.getName(), cur.getSort(), ids, sorts)) {
				mssgs.add(
					"The object "
						+ cur.getName()
						+ " has more than one transition clause");
				mssgs.add("    in the operator " + opName.toString());
				ret = false;
			} else {
				ret = cur.check(curDom, mssgs);
				ids.add(cur.getName());
				sorts.add(cur.getSort());
			}
		}
		li = necessary.listIterator();
		while (li.hasNext()) {
			oclSC cur = (oclSC) li.next();
			if (alreadyUsed(cur.getName(), cur.getSort(), ids, sorts)) {
				mssgs.add(
					"The object "
						+ cur.getName()
						+ " has more than one transition clause");
				mssgs.add("    in the operator " + opName.toString());
				ret = false;
			} else {
				ret = cur.check(curDom, mssgs);
				ids.add(cur.getName());
				sorts.add(cur.getSort());
			}
		}
		li = conditional.listIterator();
		while (li.hasNext()) {
			oclSC cur = (oclSC) li.next();
			if (alreadyUsed(cur.getName(), cur.getSort(), ids, sorts)) {
				mssgs.add(
					"The object "
						+ cur.getName()
						+ " has more than one transition clause");
				mssgs.add("    in the operator " + opName.toString());
				ret = false;
			} else {
				ret = cur.check(curDom, mssgs);
				ids.add(cur.getName());
				sorts.add(cur.getSort());
			}
		}
		// Ron 12/11/02 allow rhs warnings to be switched off
		if (curDom.rhsWarnings) {
			boolean unreferenceVars = checkUnreferencedVariables(mssgs);
			if (ret)
				ret = unreferenceVars;
		}

		/* WZ 31/5/02 */
		//change name
		opName = curDom.createOperatorSignature(this);
		// 	opName = createAllSignature(curDom);/* WZ 24/6/02 */
		Utility.debugPrintln("\n>>opName. " + opName.toString());

		return ret;
	}

	/**
	 * checkUnreferencedVariables
	 * check to see if the RHS of any sc clause refers to variables 
	 * not present in the LHS of any sc clause or in an se clause
	 * @param mssgs list to append results messages to
	 * @return true if all checks passed
	 */
	public boolean checkUnreferencedVariables(List mssgs) {
		ArrayList rhsArgs = new ArrayList();
		boolean ret = true;
		ListIterator li = necessary.listIterator();
		// First collect all variables in the RHS of the operator
		while (li.hasNext()) {
			oclSC cur = (oclSC) li.next();
			List post = cur.getPost();
			ListIterator liPost = post.listIterator();
			while (liPost.hasNext()) {
				oclPredicate ppred = (oclPredicate) liPost.next();
				List args = ppred.getArguments();
				rhsArgs.addAll(args);
			}
		}
		li = conditional.listIterator();
		while (li.hasNext()) {
			oclSC cur = (oclSC) li.next();
			List post = cur.getPost();
			ListIterator liPost = post.listIterator();
			while (liPost.hasNext()) {
				oclPredicate ppred = (oclPredicate) liPost.next();
				List args = ppred.getArguments();
				rhsArgs.addAll(args);
			}
		}
		// Now check if args used in LHS
		ListIterator liArgs = rhsArgs.listIterator();
		boolean found = false;
		while (liArgs.hasNext()) {
			found = false;
			OPredicate.pArg curArg = (OPredicate.pArg) liArgs.next();
			li = prevail.listIterator();
			while (!found && li.hasNext()) {
				oclSE cur = (oclSE) li.next();
				if (cur.refersTo(curArg.name)) {
					found = true;
				}
			}
			li = necessary.listIterator();
			while (!found && li.hasNext()) {
				oclSC cur = (oclSC) li.next();
				if (cur.transitionLHSRefersTo(curArg.name)) {
					found = true;
				}
			}
			li = conditional.listIterator();
			while (!found && li.hasNext()) {
				oclSC cur = (oclSC) li.next();
				if (cur.transitionLHSRefersTo(curArg.name)) {
					found = true;
				}
			}
			if (!found) {
				mssgs.add(
					"WARNING: The object "
						+ curArg.name
						+ " of sort "
						+ curArg.sort
						+ " is not referenced in the LHS of any transition.");
				ret = false;
			}
		}
		return ret;
	}

	/**
	 * populate state uses lists (producers and consumers)
	 * @param curDom ocl domain used
	 * 
	 */
	public void checkUses(oclDomain curDom) {

		ListIterator li = prevail.listIterator();
		while (li.hasNext()) {
			oclSE cur = (oclSE) li.next();
			cur.checkConsumer(curDom, opName.getName());
		}
		li = necessary.listIterator();
		while (li.hasNext()) {
			oclSC cur = (oclSC) li.next();
			cur.checkUses(curDom, opName.getName());
		}
		li = conditional.listIterator();
		while (li.hasNext()) {
			oclSC cur = (oclSC) li.next();
			cur.checkUses(curDom, opName.getName());
		}
	}

	/**
	 * checks to see if a name / sort pair occur in the given lists.
	 * @param name name
	 * @param sort sort
	 * @param ids List of names
	 * @param sorts List of sorts
	 * @return true if used
	 */
	protected boolean alreadyUsed(
		String name,
		String sort,
		List ids,
		List sorts) {
		boolean ret = false;
		int noIds = ids.size();
		for (int i = 0; i < noIds; i++) {
			if (name.equals(ids.get(i))) {
				if (sort.equals(sorts.get(i))) {
					return true;
				}
			}
		}
		return false;
	}

	/* Weihong  125/03/2002 */
	/**
	 * to instantiate the current oclOperator with given variables
	 * @param newPred oclPredicate with contains all variables
	 * 
	 */
	// Ron 10/11/02 Bug only rename the corresponding variable in the operator name
	//              was replaceVariableName - which replaces all matching
	//              occurances
	public void instantiateWith(oclPredicate newPred) {
		ListIterator liNew = newPred.getArguments().listIterator();
		ListIterator liCur = opName.getArguments().listIterator();
		Utility.debugPrintln("Instantiate with new >> " + newPred.toString());
		Utility.debugPrintln("Instantiate with old >> " + opName.toString());
		while (liNew.hasNext()) {
			oclPredicate.pArg arg = (oclPredicate.pArg) liNew.next();
			try {
				oclPredicate.pArg curArg =
					(oclPredicate.pArg) ((oclPredicate.pArg) liCur.next())
						.clone();
				replaceVariableName(curArg, new String(arg.name));
			} catch (CloneNotSupportedException e) {
				Utility.debugPrintln("FAILED ON CLONE 1");
			}
		}
		int count = 0;
		liNew = newPred.getArguments().listIterator();
		while (liNew.hasNext()) {
			oclPredicate.pArg arg = (oclPredicate.pArg) liNew.next();
			try {
				opName.replaceVariableNo(count, new String(arg.name));
			} catch (Exception e) {
				Utility.debugPrintln(
					" XXX Failed to find replacement for " + arg.name);
			}
			count++;
		}
		Utility.debugPrintln("Instantiate RESULT  >> " + opName.toString());
	}

	/* Weihong added on 01/06/2001 */
	/**
	 * replace with a new variable value. If not in an element throw exception
	 * @param before the variable
	 * @param after new element name
	 */
	public void replaceVariableName(OPredicate.pArg before, String after) {
		Utility.debugPrintln(
			"***** replaceVariableName in oclOperator ****",
			"XXXX >> " + before.name + "  with " + after);
		try {
			opName.replaceVariableName(before, after);
		} catch (Exception e) {
			Utility.debugPrintln(
				"oclOperator",
				"failed to replace variable name in operator name");
		}
		ListIterator li;
		//oclSE
		li = getPrevail().listIterator();
		while (li.hasNext()) {
			oclSE se = (oclSE) li.next();
			se.replaceVariableName(before, after);
		}
		//oclSC 
		for (int i = 1; i < 3; i++) {
			if (i == 1)
				li = getNecessary().listIterator();
			if (i == 2)
				li = getConditional().listIterator();
			while (li.hasNext()) {
				Object obj = li.next();
				if (obj.getClass().getName().equals("jplan.ocl.oclSCPlus")) {
					oclSCPlus sc = (oclSCPlus) obj;
					sc.replaceVariableName(before, after);
				} else {
					oclSC sc = (oclSC) obj;
					sc.replaceVariableName(before, after);
				}
			}
		}
	}

	/**
	 * replace with a new variable value. If not in an element throw exception
	 * @param before the variable
	 * @param after new element name
	 */
	public void replaceVariableName(String before, String after) {
		try {
			opName.replaceVariableNameByName(before, after); /* WZ 29/5/02 */
		} catch (Exception e) {
			Utility.debugPrintln(
				"oclOperator",
				"failed to replace variable name in operator name");
		}
		ListIterator li;
		//oclSE
		li = getPrevail().listIterator();
		while (li.hasNext()) {
			oclSE se = (oclSE) li.next();
			se.replaceVariableName(before, after);
		}
		//oclSC 
		for (int i = 1; i < 3; i++) {
			if (i == 1)
				li = getNecessary().listIterator();
			if (i == 2)
				li = getConditional().listIterator();

			while (li.hasNext()) {
				Object obj = li.next();
				if (obj.getClass().getName().equals("jplan.ocl.oclSCPlus")) {
					oclSCPlus sc = (oclSCPlus) obj;
					sc.replaceVariableName(before, after);
				} else {
					oclSC sc = (oclSC) obj;
					sc.replaceVariableName(before, after);
				}

			}
		}
	}
	/**
	 * renameConflictingVariables
	 * used by life history editor to ensure standard variables for different Property predicates
	 * are not accidently unified
	 * problem occurs when there are multiple var occurances within the same se/sc LHS
	 * @param dom
	 */
	public void renameConflictingVariables(oclDomain dom) {
		// first form a hashtable linking predicate names with their variables
		// key var name value predicate list

		ListIterator li = necessary.listIterator();
		while (li.hasNext()) {
			oclSC curSC = (oclSC) li.next();
			curSC.renameConflictVariables(dom);
		}
		// TODO working here
	}

	/**
	 * check to see if operator defines a conditional effect.
	 * @return boolean
	 */
	public boolean hasCondEffects() {
		return (conditional.size() > 0);
	}

	/**
	 * extractSubStateDefs
	 * This is part of the PDDL->OCL translation routins
	 * Tries to create the substateclass definitions fron the transitions
	 * and add them if not already present to the domain definition
	 * @param oclDom - the domain being created
	 */
	public void extractSubStateDefs(oclDomain oclDom) {
		ListIterator li = prevail.listIterator();
		while (li.hasNext()) {
			oclSE curSE = (oclSE) li.next();
			String sort = curSE.getSort();
			oclSSClassDef classDef = null;
			try {
				classDef = oclDom.getStateListForSort(sort);
			} catch (OCLNoSuchElementException e) {
				// no state definitions yet add this as the first
				classDef = oclDom.addClassDef(sort, OPredicate.toVar(sort));
			}
			ListIterator liStates = classDef.getStateList().listIterator();
			boolean found = false;
			while (!found && liStates.hasNext()) {
				oclStateList target = (oclStateList) liStates.next();
				List cur = curSE.getPredicateList();
				if (target
					.ssUnifiesWithStateList(
						curSE.getName(),
						OPredicate.toVar(sort),
						cur)) {
					found = true;
				}
			}
			if (!found) {
				//need to add this state
				curSE.toSubStateList(classDef, OPredicate.toVar(sort));
			}
		}
		// Now deal with necessary changes
		li = necessary.listIterator();
		while (li.hasNext()) {
			oclSC curSC = null;
			try {
				curSC = (oclSC) ((oclSC) li.next()).clone();
			} catch (java.lang.CloneNotSupportedException e) {
				System.err.println(
					"Unexpected failure to clone sc [oclOperator]");
			}
			String sort = curSC.getSort();
			oclSSClassDef classDef = null;
			boolean defExists = true;
			try {
				classDef = oclDom.getStateListForSort(sort);
			} catch (OCLNoSuchElementException e) {
				// no state definitions yet add this as the first
				defExists = false;
				classDef = oclDom.addClassDef(sort, OPredicate.toVar(sort));
			}
			curSC.replaceVariableName(curSC.getName(), OPredicate.toVar(sort));
			ListIterator liStates = classDef.getStateList().listIterator();
			boolean foundLHS = false;
			boolean foundRHS = false;
			while (!(foundLHS && foundRHS) && liStates.hasNext()) {
				oclStateList target = (oclStateList) liStates.next();
				List curLHS = curSC.getPre();
				if (target
					.ssUnifiesWithStateList(
						curSC.getName(),
						OPredicate.toVar(sort),
						curLHS)) {
					foundLHS = true;
				}
				List curRHS = curSC.getPost();
				if (target
					.ssUnifiesWithStateList(
						curSC.getName(),
						OPredicate.toVar(sort),
						curRHS)) {
					foundRHS = true;
				}
			}
			if (!foundLHS) {
				//need to add this state
				curSC.toLHSSubStateList(classDef, OPredicate.toVar(sort));
			}
			if (!foundRHS) {
				//need to add this state
				curSC.toRHSSubStateList(classDef, OPredicate.toVar(sort));
			}
		}
		// Now deal with conditional changes
		li = conditional.listIterator();
		while (li.hasNext()) {
			oclSC curSC = null;
			try {
				curSC = (oclSC) ((oclSC) li.next()).clone();
			} catch (java.lang.CloneNotSupportedException e) {
				System.err.println(
					"Unexpected failure to clone sc [oclOperator]");
			}
			String sort = curSC.getSort();
			oclSSClassDef classDef = null;
			boolean defExists = true;
			try {
				classDef = oclDom.getStateListForSort(sort);
			} catch (OCLNoSuchElementException e) {
				// no state definitions yet add this as the first
				defExists = false;
				classDef = oclDom.addClassDef(sort, OPredicate.toVar(sort));
			}
			curSC.replaceVariableName(curSC.getName(), OPredicate.toVar(sort));
			ListIterator liStates = classDef.getStateList().listIterator();
			boolean foundLHS = false;
			boolean foundRHS = false;
			while (!(foundLHS && foundRHS) && liStates.hasNext()) {
				oclStateList target = (oclStateList) liStates.next();
				List curLHS = curSC.getPre();
				if (target
					.ssUnifiesWithStateList(
						curSC.getName(),
						OPredicate.toVar(sort),
						curLHS)) {
					foundLHS = true;
				}
				List curRHS = curSC.getPost();
				if (target
					.ssUnifiesWithStateList(
						curSC.getName(),
						OPredicate.toVar(sort),
						curRHS)) {
					foundRHS = true;
				}
			}
			if (!foundLHS) {
				//need to add this state
				curSC.toLHSSubStateList(classDef, OPredicate.toVar(sort));
			}
			if (!foundRHS) {
				//need to add this state
				curSC.toRHSSubStateList(classDef, OPredicate.toVar(sort));
			}
		}
	}

	/**
	 * removeDuplicatedConditionals
	 * Remove any duplicated conditional transitions
	 */
	public void removeDuplicatedConditionals() {
		boolean done = false;
		int NoConds = conditional.size();
		int inx = 1;
		int NoRemoved = 0;
		while (!done && inx < NoConds - NoRemoved) {
			int inner = 0;
			boolean removed = false;
			while (!removed && inner < inx) {
				oclSC first = (oclSC) conditional.get(inner);
				oclSC second = (oclSC) conditional.get(inx);
				if (first.getSort().equals(second.getSort())
					&& first.equivalentLHS(second)) {
					conditional.remove(inx);
					removed = true;
					NoRemoved++;
				}
				inner++;
			}
			if (!removed) {
				inx++;
			}
		}
	}

	/* Weihong added on 04/06/2001 */
	/**
	 * clone a copy of oclOperator
	 * @throws CloneNotSupportedException
	 * @return Object
	 */
	public Object clone() throws CloneNotSupportedException {
		try {
			oclOperator op = new oclOperator();
			op.opName = (oclPredicate) opName.clone();

			ListIterator li = prevail.listIterator();
			while (li.hasNext()) {
				oclSE se = (oclSE) li.next();
				op.addPrevSE((oclSE) se.clone());
			}

			li = necessary.listIterator();
			while (li.hasNext()) {
				oclSC sc = (oclSC) li.next();
				op.addNecSC((oclSC) sc.clone());
			}

			li = conditional.listIterator();
			while (li.hasNext()) {
				oclSC sc = (oclSC) li.next();
				op.addCondSC((oclSC) sc.clone());
			}

			return op;
		} catch (CloneNotSupportedException e) {
			throw e;
		}
	}

	/**
	 * used for debugging purposes by opmaker
	 * @return The string representation of an operator
	 */
	public String toOPString() {
		StringWriter strW = new StringWriter();
		PrintWriter ps = new PrintWriter(strW);
		oclPrintComponent(ps, 0, true);
		return strW.toString();
	}

	/**
	 * to print the current oclOperator to a PrintWriter.
	 * @param ps PrintWriter
	 * @param indent value of indentation
	 * @param nline not being used really
	 * 
	 */
	public void oclPrintComponent(PrintWriter ps, int indent, boolean nline) {
		if (docm.size() > 0) {
			ps.println("/****");
			ListIterator liDoc = docm.listIterator();
			while (liDoc.hasNext()) {
				ps.print(" * ");
				ps.println((String) liDoc.next());
			}
			ps.println(" */");
		}
		ps.print("operator(");
		opName.oclPrintComponent(ps, 0, false);
		//opName.oclSortedPrint(ps, 0, false);
		ps.println(",\n    % prevail");
		ps.print("    [");
		ListIterator li = prevail.listIterator();
		while (li.hasNext()) {
			((oclSE) li.next()).oclPrintComponent(ps, 5, false);
			if (li.hasNext())
				ps.println(",");
		}
		ps.print("],\n    % necessary\n    [");
		li = necessary.listIterator();
		while (li.hasNext()) {
			((oclSC) li.next()).oclPrintComponent(ps, 5, false);
			if (li.hasNext())
				ps.println(",");
		}
		ps.print("],\n    % conditional\n    [");
		li = conditional.listIterator();
		while (li.hasNext()) {
			((oclSC) li.next()).oclPrintComponent(ps, 5, false);
			if (li.hasNext())
				ps.println(",");
		}
		ps.println("]).");
	}

	/**
	 * to translate to PDDL and print the current oclOperator to a PrintWriter.
	 * @para curDom - the current OCL domain
	 * @param ps PrintWriter
	 * @param indent value of indentation
	 * @param nline not being used really
	 * 
	 */
	public void pddlPrint(oclDomain curDom,PrintWriter ps,int indent,boolean nline) {
		ps.println("  (:action " + opName.getName());
		pddlPrintBody(curDom, ps, indent, nline);
	}

	/**
		 * to translate to PDDL and print the current oclOperator to a PrintWriter.
		 * @para curDom - the current OCL domain
		 * @param ps PrintWriter
		 * @param indent value of indentation
		 * @param nline not being used really
		 * 
		 */
	protected void pddlPrintBody(
		oclDomain curDom,
		PrintWriter ps,
		int indent,
		boolean nline) {
		ps.print("       :parameters (");
		ListIterator li = opName.getArguments().listIterator();
		while (li.hasNext()) {
			OPredicate.pArg arg = (OPredicate.pArg) li.next();
			ps.print(" " + OPredicate.toPDDLVar(arg.name));
			List prims = curDom.getPrimitiveSubTypes(arg.sort);
			if (prims.size() > 0) {
				ps.print(" - (either");
				ListIterator liPrims = prims.listIterator();
				while (liPrims.hasNext()) {
					ps.print(" " + (String) liPrims.next());
				}
				ps.print(")");
			} else {
				ps.print(" - " + arg.sort);
			}
		}
		ps.println(")");
		List preconds = collectPreconds();
		if (preconds.size() > 1) {
			ps.println("       :precondition (and ");
		} else {
			ps.println("       :precondition ");
		}
		li = preconds.listIterator();
		while (li.hasNext()) {
			oclPredicate curPred = (oclPredicate) li.next();
			if (!curPred.isFluent()) {
				curPred.pddlPrint(curDom, ps, 12, true);
			}
		}
		if (curDom.isOclPlus()) {
			List tests = collectStateConditions();
			li = tests.listIterator();
			while (li.hasNext()) {
				oclExpression exp = (oclExpression) li.next();
				exp.pddlPrint(curDom, ps, 12, true);
			}
		}
		if (preconds.size() > 1) {
			ps.println("       )");
		}
		List addList = new ArrayList();
		List delList = new ArrayList();
		collectEffects(curDom, addList, delList);
		List updates = new ArrayList();
		if (curDom.isOclPlus()) {
			updates = collectStateUpdates();
		}
		if ((addList.size() + delList.size() + conditional.size() + updates.size()) > 1) {
			ps.println("       :effect (and ");
		} else {
			ps.println("       :effect");
		}
		li = delList.listIterator();
		String pad = "";
		for (int i = 0; i < 12; i++)
			pad = pad + " ";
		while (li.hasNext()) {
			oclPredicate curPred = (oclPredicate) li.next();
			ps.print(pad + "(not ");
			curPred.pddlPrint(curDom, ps, 0, false);
			ps.println(")");
		}
		li = addList.listIterator();
		while (li.hasNext()) {
			oclPredicate curPred = (oclPredicate) li.next();
			curPred.pddlPrint(curDom, ps, 12, true);
		}
		if (curDom.isOclPlus()) {
			li = updates.listIterator();
			while (li.hasNext()) {
				oclExpression exp = (oclExpression) li.next();
				exp.pddlPrint(curDom, ps, 12, true);
			}
		}
		li = conditional.listIterator();
		while (li.hasNext()) {
			oclSC curCond = (oclSC) li.next();
			ps.print(pad + "(forall (");
			ps.print(OPredicate.toPDDLVar(curCond.getName()));
			List prims = curDom.getPrimitiveSubTypes(curCond.getSort());
			if (prims.size() > 0) {
				ps.print(" - (either");
				ListIterator liPrims = prims.listIterator();
				while (liPrims.hasNext()) {
					ps.print(" " + (String) liPrims.next());
				}
				ps.print(")");
			} else {
				ps.print(" - " + curCond.getSort());
			}
			ps.println(")");
			if (curCond.getPre().size() > 1) {
				ps.println(pad + "  (when (and");
			} else {
				ps.println(pad + "  (when");
			}
			ListIterator liPre = curCond.getPre().listIterator();
			while (liPre.hasNext()) {
				oclPredicate curPred = (oclPredicate) liPre.next();
				curPred.pddlPrint(curDom, ps, 16, true);
			}
			if (curCond.getPre().size() > 1) {
				ps.println(pad + "   )");
			}
			if (curCond.getPost().size() > 1) {
				ps.println(pad + "  (and");
			}
			// Deletes
			liPre = curCond.getPre().listIterator();
			while (liPre.hasNext()) {
				oclPredicate curPred = (oclPredicate) liPre.next();
				boolean negate = true;
				if (curPred.getName().equals("ne"))
					negate = false;
				else if (curPred.isStatic())
					negate = false;
				else {
					ListIterator liPost = curCond.getPost().listIterator();
					while (negate && liPost.hasNext()) {
						oclPredicate curPost = (oclPredicate) liPost.next();
						if (curPost.equals(curPred))
							negate = false;
					}
				}
				if (negate) {
					ps.print("                (not ");
					curPred.pddlPrint(curDom, ps, 0, false);
					ps.println(")");
				}
			}
			// Now adds
			ListIterator liPost = curCond.getPost().listIterator();
			while (liPost.hasNext()) {
				oclPredicate curPred = (oclPredicate) liPost.next();
				boolean add = true;
				if (curPred.getName().equals("ne") || curPred.isStatic())
					add = false;
				else {
					liPre = curCond.getPre().listIterator();
					while (add && liPre.hasNext()) {
						oclPredicate curPre = (oclPredicate) liPre.next();
						if (curPre.equals(curPred))
							add = false;
					}
				}
				if (add)
					curPred.pddlPrint(curDom, ps, 16, true);
			}
			if ((addList.size() + delList.size() + conditional.size() + updates.size()) > 1) {
				ps.println(pad + "   )))");
			} else {
				ps.println(pad + "   ))");
			}
		}
		ps.println("        )");
		ps.println("    )");
	}
	/**
	 * collectPreconds
	 * collect the preconditions predicates fro all prevails and necessary clauses
	 * used in the translation to PDDL 
	 * @return the list of precondition predicates
	 */
	private List collectPreconds() {
		List preconds = new ArrayList();
		ListIterator li = prevail.listIterator();
		while (li.hasNext()) {
			oclSE prev = (oclSE) li.next();
			preconds.addAll(prev.getPredicateList());
		}
		li = necessary.listIterator();
		while (li.hasNext()) {
			oclSC nec = (oclSC) li.next();
			preconds.addAll(nec.getPre());
			ListIterator liPost = nec.getPost().listIterator();
			while (liPost.hasNext()) {
				oclPredicate curPred = (oclPredicate) liPost.next();
				if (curPred.getName().equals("ne")) {
					preconds.add(curPred);
				} else if (curPred.isStatic()) {
					preconds.add(curPred);
				}
			}
		}
		return preconds;
	}

	/**
	 * collectStateConditions
	 * collect the OclPlus test clauses
	 * used in the translation to PDDL 
	 * @return the list of precondition predicates
	 */
	private List collectStateConditions() {
		List preconds = new ArrayList();
		ListIterator li = prevail.listIterator();
		while (li.hasNext()) {
			oclSEPlus prev = (oclSEPlus) li.next();
			preconds.addAll(prev.getFluentConditions());
		}
		li = necessary.listIterator();
		while (li.hasNext()) {
			oclSCPlus nec = (oclSCPlus) li.next();
			preconds.addAll(nec.getStateConditions());
		}
		return preconds;
	}

	/**
	 * collectStateUpdates
	 * collect the OclPlus test clauses
	 * used in the translation to PDDL 
	 * @return the list of precondition predicates
	 */
	private List collectStateUpdates() {
		List updates = new ArrayList();
		ListIterator li = necessary.listIterator();
		while (li.hasNext()) {
			oclSCPlus nec = (oclSCPlus) li.next();
			ListIterator liUp = nec.getStateUpdates().listIterator();
			while (liUp.hasNext()) {
				oclExpression exp = (oclExpression) liUp.next();
				if (!exp.isAssignmentNoOp()) {
					updates.add(exp);
				}
			}
		}
		return updates;
	}

	/**
	 * collectEffects
	 * work out add list and delete lists for PDDL operator
	 * @param curDom - the current domain
	 * @param addList
	 * @param delList
	 */
	protected void collectEffects(oclDomain curDom, List addList, List delList) {
		List preconds = new ArrayList();
		List postpreds = new ArrayList();
		ListIterator li = necessary.listIterator();
		while (li.hasNext()) {
			oclSC clause = (oclSC) li.next();
			preconds.addAll(clause.getPre());
			postpreds.addAll(clause.getPost());
		}
		li = necessary.listIterator();
		while (li.hasNext()) {
			oclSC nec = (oclSC) li.next();
			ListIterator liPost = nec.getPost().listIterator();
			while (liPost.hasNext()) {
				oclPredicate curPred = (oclPredicate) liPost.next();
				if (curPred.getName().equals("ne")) {
					break;
				} else if (curPred.isStatic()) {
					break;
				} else if (curPred.isFluent()) {
					break;
				}
				boolean inPreconds = false;
				ListIterator liPreconds = preconds.listIterator();
				while (!inPreconds && liPreconds.hasNext()) {
					oclPredicate pre = (oclPredicate) liPreconds.next();
					if (pre.equals(curPred)) {
						inPreconds = true;
					}
				}
				if (!inPreconds) {
					addList.add(curPred);
				}
			}
		}
		li = necessary.listIterator();
		while (li.hasNext()) {
			oclSC nec = (oclSC) li.next();
			ListIterator liPre = nec.getPre().listIterator();
			while (liPre.hasNext()) {
				oclPredicate curPred = (oclPredicate) liPre.next();
				// Don't delete atomic invarients
				// Don't delete ne's
				// Dont't delete things in the add list / postlist.
				if (curPred.isStatic())
					break;
				if (curPred.getName().equals("ne"))
					break;
				if (curPred.isFluent())
					break;
				boolean added = false;
				ListIterator liAdd = postpreds.listIterator();
				while (!added && liAdd.hasNext()) {
					oclPredicate addPred = (oclPredicate) liAdd.next();
					if (addPred.equals(curPred)) {
						added = true;
					}
				}
				if (!added) {
					delList.add(curPred);
				}
			}
		}
	}

	/* WZ 17/6/02 */
	/**
	 * a string expression, mainly used for saving purpose
	 * @return String
	 */
	public String to_String() {
		StringBuffer str = new StringBuffer();
		str.append("operator::Description:");
		if (docm.size() > 0) {
			ListIterator liDoc = docm.listIterator();
			while (liDoc.hasNext()) {
				str.append((String) liDoc.next());
				if (liDoc.hasNext())
					str.append("&");
			}
		}
		str.append(";");

		str.append("Name:");
		if (opName != null) {
			str.append(opName.toString());
			str.append("&");
			str.append(opName.getSort().toString()); /* WZ 24/6/02 */
		}
		str.append(";");

		str.append("Prevail:");
		ListIterator li = prevail.listIterator();
		while (li.hasNext()) {
			str.append(((oclSE) li.next()).to_String());
			if (li.hasNext())
				str.append("&");
		}
		str.append(";");

		str.append("Necessary:");
		li = necessary.listIterator();
		while (li.hasNext()) {
			str.append(((oclSC) li.next()).to_String());
			if (li.hasNext())
				str.append("&");
		}
		str.append(";");

		str.append("Conditional:");
		li = conditional.listIterator();
		while (li.hasNext()) {
			str.append(((oclSC) li.next()).to_String());
			if (li.hasNext())
				str.append("&");
		}
		str.append(";\n");

		return str.toString();
	}
}
