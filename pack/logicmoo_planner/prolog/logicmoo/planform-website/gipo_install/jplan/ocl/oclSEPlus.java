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
import java.io.*;

import jplan.general.OCLSelectionException;

/**
 * @author ron
 *
 * In addition to a list of predicates an oclSEPlus stores
 * a list of boolean expressions to be tested. Each such expression must 
 * refer to the se object in one of its sub clauses
 */
public class oclSEPlus extends oclSE implements Cloneable, oclPrint {

	private List stateCondition;

	/**
	 * Creates an instance of oclSE.
	 * @param kind sort
	 * @param ID name
	 */
	public oclSEPlus(String kind, String ID) {
		super(kind, ID);
		stateCondition = new ArrayList();
	}
	/**
	 * create from an existing oclSE
	 * @param se
	 */
			
	public oclSEPlus(oclSE se) {
		super(se.sort,se.name);
		oclSE temp = null;
		try {
			temp = (oclSE)se.clone();
		}catch (CloneNotSupportedException e){}
		state = temp.state;
	}
	
	
	/**
	 * Creates an instance of oclSE.
	 * uses the LHS of an oclSCPlus to set values
	 * @param kind sort
	 * @param ID name
	 */
	public oclSEPlus(oclSCPlus scPlus) {
		super(scPlus.sort, scPlus.name);
		stateCondition = new ArrayList();
		stateCondition.addAll(scPlus.getStateConditions());
		this.setPredicateList(scPlus.getPre());
	}

	/**
	 * Add a new boolean expression
	 * @param the expression to add
	 */
	public void addExpression(oclExpression exp) {
		stateCondition.add(exp);
	}
	
	/**
	 * getFluentConditions
	 * @return - the state fluent conditions : the test conditions
	 */
	public List getFluentConditions() {
		return stateCondition;
	}

	/**
	 * clone a copy of oclSE
	 * @throws CloneNotSupportedException
	 * @return Object
	 */
	public Object clone() throws CloneNotSupportedException {
		try {
			oclSEPlus se = new oclSEPlus(sort, name);
			java.util.List prdList = new ArrayList();
			ListIterator li = state.listIterator();
			while (li.hasNext()) {
				oclPredicate oprd = (oclPredicate) li.next();
				prdList.add((oclPredicate) oprd.clone());
			}
			se.setPredicateList(prdList);
			li = stateCondition.listIterator();
			while (li.hasNext()) {
				oclExpression exp = (oclExpression)li.next();
				se.addExpression((oclExpression)exp.clone());
			}
			return se;
		} catch (CloneNotSupportedException e) {
			throw e;
		}
	}
	


	/**
	 * to print the current oclSE to a PrintWriter.
	 * @param ps PrintWriter
	 * @param indent value of indentation
	 * @param nline not being used really
	 * @return void
	 */
	public void oclPrintComponent(PrintWriter ps, int indent, boolean nline) {
		boolean hasSE = false;
		String pad = "";
		for (int i = 0; i < indent; i++)
			pad = pad + " ";
		ps.print(pad + "se(" + sort + "," + name + ",[");
		ListIterator li = state.listIterator();
		while (li.hasNext()) {
			//		3/7/03 Changed for ocl plus - print the value of functors
			oclPredicate cur = (oclPredicate) li.next();
			hasSE = true;
			if (cur.isFluent()) {
				(new oclFunctor(cur)).oclPrintFluent(ps, 0, false);
			} else {
				cur.oclPrintComponent(ps, 0, false);
			}
			if (li.hasNext())
				ps.print(",");
		}
		li = stateCondition.listIterator();
		while (li.hasNext()) {
			if (hasSE)
				ps.print(",");
			oclExpression cur = (oclExpression) li.next();
			cur.oclPrintComponent(ps, 0, false);
			hasSE = true;
		}
		ps.print("])");
	}
}
