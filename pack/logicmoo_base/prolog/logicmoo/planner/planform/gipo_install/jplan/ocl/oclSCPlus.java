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
 *
 *
 *
 * Created on 25-Aug-2003
 *
 * Author ron
 * 
 */
package jplan.ocl;

import java.util.List;
import java.util.ArrayList;
import java.util.ListIterator;
import java.io.*; /* Weihong changed on 30/06/2001 */
import jplan.general.OPredicate; /* Weihong added on 01/06/2001 */
import jplan.general.Utility;

import jplan.general.OPredicate;

/**
 * @author ron
 *
 * Extends oclSC for oclPlus to add two lists for expressions on
 * LHS and RHS
 */
public class oclSCPlus extends oclSC implements oclPrint, Cloneable {

	private List stateCondition;
	private List stateUpdates;

	/**
	 * Creates an instance of oclSCPlus.
	 * @param kind sort
	 * @param ID name
	 */
	public oclSCPlus(String kind, String ID) {
		super(kind, ID);
		stateCondition = new ArrayList();
		stateUpdates = new ArrayList();
	}
	
	/**
	 * Creates an instance of oclSCPlus. from an oclSC
	 * @param kind sort
	 * @param ID name
	 */
	public oclSCPlus(oclSC sc) {
		super(sc.sort, sc.name);
		oclSC temp = null;
		try {
			temp = (oclSC)sc.clone();
		} catch (CloneNotSupportedException e){}
		pre = temp.pre;
		post = temp.post;
		stateCondition = new ArrayList();
		stateUpdates = new ArrayList();
	}

	/**
	 * Add a new boolean expression as a condition
	 * @param the expression to add
	 */
	public void addCondition(oclExpression exp) {
		stateCondition.add(exp);
	}

	/**
	 * Add a new update expression as an transition action
	 * @param the expression to add
	 */
	public void addUpdate(oclExpression exp) {
		stateUpdates.add(exp);
	}
	
	/**
	 * getStateConditions
	 * return the state condition list
	 * @return - the state condition list
	 */
	public List getStateConditions() {
		return stateCondition;
	}
	
	/**
	 * getStateUpdates
	 * return the state update list
	 * @return - the state update list
	 */
	public List getStateUpdates() {
		return stateUpdates;
	}

	/**
	 * clone a copy of oclSC
	 * @throws CloneNotSupportedException
	 * @return Object
	 */
	public Object clone() throws CloneNotSupportedException {
		try {
			oclSCPlus sc = new oclSCPlus(sort, name);
			java.util.List prdList = new ArrayList();
			ListIterator li = pre.listIterator();
			while (li.hasNext()) {
				oclPredicate oprd = (oclPredicate) li.next();
				sc.addPre((oclPredicate) oprd.clone());
			}

			li = post.listIterator();
			while (li.hasNext()) {
				oclPredicate oprd = (oclPredicate) li.next();
				sc.addPost((oclPredicate) oprd.clone());
			}
			li = stateCondition.listIterator();
			while (li.hasNext()) {
				oclExpression exp = (oclExpression) li.next();
				sc.addCondition((oclExpression) exp.clone());
			}
			li = stateUpdates.listIterator();
			while (li.hasNext()) {
				oclExpression exp = (oclExpression) li.next();
				sc.addUpdate((oclExpression) exp.clone());
			}
			return sc;
		} catch (CloneNotSupportedException e) {
			throw e;
		}
	}

	/**
	 * for all the predicate replace with a new variable value.
	 * @param before the varible
	 * @param after new element name
	 * @return void
	 */
	public void replaceVariableName(OPredicate.pArg before, String after) {
		Utility.debugPrintln(
			"replaceVariableName",
			"XXXX >> " + before.name + "  with " + after);
		if (before.name.equals(name)) { /* Weihong added on 04/06/2001 */
			name = after;
		}
		replaceVariableNameFromAList(pre, before, after);
		replaceVariableNameFromAList(post, before, after);
		replaceVariableNameFromExprList(stateCondition, before.name, after);
		replaceVariableNameFromExprList(stateUpdates, before.name, after);
	}

	/**
	 * for all the predicate replace with a new variable value.
	 * @param before the varible
	 * @param after new element name
	 * @return void
	 */
	public void replaceVariableName(String before, String after) {
		if (before.equals(name)) {
			name = after;
		}
		replaceVariableNameFromAList(pre, before, after);
		replaceVariableNameFromAList(post, before, after);
		replaceVariableNameFromExprList(stateCondition, before, after);
		replaceVariableNameFromExprList(stateUpdates, before, after);
	}

	/**
	 * replace variable name from a given list.
	 * @param listPred the predicate list
	 * @param before the varible
	 * @param after new element name
	 * @return void
	 */
	public void replaceVariableNameFromAList(
		List listPred,
		String before,
		String after) {
		ListIterator li = listPred.listIterator();
		while (li.hasNext()) {
			oclPredicate oprd = (oclPredicate) li.next();
			try {
				oprd.replaceVariableNameByName(before, after);
			} catch (Exception e) {
			}
		}
	}

	/**
	 * replace variable name from a given list of expressions.
	 * @param listPred the predicate list
	 * @param before the varible
	 * @param after new element name
	 * @return void
	 */
	public void replaceVariableNameFromExprList(
		List exprs,
		String before,
		String after) {
		ListIterator li = exprs.listIterator();
		while (li.hasNext()) {
			oclExpression oprd = (oclExpression) li.next();
			try {
				oprd.replaceVariableName(before, after);
			} catch (Exception e) {
			}
		}
	}

	/**
	 * to print the current oclSC to a PrintWriter.
	 * @param ps PrintWriter
	 * @param indent value of indentation
	 * @param nline not being used really
	 * @return void
	 */
	public void oclPrintComponent(PrintWriter ps, int indent, boolean nline) {
		boolean hasSC = false;
		String pad = "";
		for (int i = 0; i < indent; i++)
			pad = pad + " ";
		ps.print(pad + "sc(" + sort + "," + name + ",[");
		ListIterator li = pre.listIterator();
		while (li.hasNext()) {
			((oclPredicate) li.next()).oclPrintComponent(ps, 0, false);
			hasSC = true;
			if (li.hasNext())
				ps.print(",");
		}
		li = stateCondition.listIterator();
		while (li.hasNext()) {
			if (hasSC)
				ps.print(",");
			oclExpression cur = (oclExpression) li.next();
			cur.oclPrintComponent(ps, 0, false);
			hasSC = true;
		}
		hasSC = false;
		ps.print("]=>[");
		li = post.listIterator();
		while (li.hasNext()) {
			((oclPredicate) li.next()).oclPrintComponent(ps, 0, false);
			hasSC = true;
			if (li.hasNext())
				ps.print(",");
		}
		li = stateUpdates.listIterator();
		while (li.hasNext()) {
			if (hasSC)
				ps.print(",");
			oclExpression cur = (oclExpression) li.next();
			cur.oclPrintComponent(ps, 0, false);
			hasSC = true;
		}
		ps.print("])");
	}
}
