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
 * Created on 23-Jun-2003
 * @author ron
 *
 * This is the predicate structure for ocl+
 * The predicate structure encompases fluents
 */

package jplan.ocl;

import java.io.*;
import java.util.List;
import java.util.ArrayList;
import java.util.ListIterator;

public class oclFunctor
	extends oclPredicate
	implements oclPrint, Serializable, Cloneable {

	private double value;

	/**
	 * creates an instance of the oclPredicate
	 * @param name the name of the oclPredicate
	 */
	public oclFunctor(String name) {
		super(name);
		fluent = true; // Ron 14/07/05
	}

	/**
	 * creates an instance of the oclPredicate
	 * @param name the name of the oclPredicate
	 */
	public oclFunctor(oclPredicate pred) {
		super(pred.getName());
		staticPred = pred.isStatic();
		fluent = true;
		fluentValue = pred.getFluentValue();
		ListIterator li = pred.getArguments().listIterator();
		while (li.hasNext()) {
			pArg parg = (pArg) li.next();
			try {
				args.add((pArg) parg.clone());
			} catch (CloneNotSupportedException e) {
			}
		}
	}

	/**
	 * to translate to PDDL and print the current oclPredicate to a PrintWriter.
	 * @param curDom - the current ocl domain
	 * @param ps PrintWriter
	 * @param indent value of indentation
	 * @param nline not being used really
	 * 
	 */
	public void pddlPrint(oclDomain curDom,PrintWriter ps,int indent,boolean nline) {
		String pad = "";
		for (int i = 0; i < indent; i++)
			pad = pad + " ";
		ListIterator li = args.listIterator();
		ps.print(pad + "(= (" + functor + " ");
		while (li.hasNext()) {
			pArg arg = (pArg) li.next();
			String varName = (String) arg.name;
			if (arg.aType == VAR) {
				ps.print(toPDDLVar(varName));
			} else if (arg.aType == CONST) {
				ps.print(varName);
			}
			if (li.hasNext())
				ps.print(" ");
		}
		ps.print(") " +	Double.toString(fluentValue));
		if (nline)
			ps.println(")");
		else
			ps.print(")");
	}

	/**
	 * clone a copy of oclPredicate
	 * @throws CloneNotSupportedException
	 * @return Object
	 */
	public Object clone() throws CloneNotSupportedException {
		oclFunctor res = new oclFunctor(functor);
		res.staticPred = this.staticPred;
		res.fluent = this.fluent; //Ron 25/05/03
		res.fluentValue = this.fluentValue; //Ron 25/05/03
		ListIterator li = args.listIterator();
		res.value = value;
		while (li.hasNext()) {
			/* Weihong added on 31/05/2001 */
			pArg parg = (pArg) li.next();
			res.args.add((pArg) parg.clone());
		}
		return (oclFunctor) res;
	}

	/**
	 * toInstantiatedString
	 * return a string that displays the functor along with its value
	 * @return - the formatted string
	 */
	public String toInstantiatedString() {
		String res = toString() + " is " + fluentValue;
		return res;
	}
	/**
	 * to print the current oclFunctor to a PrintWriter.
	 * and display set value
	 * @param ps PrintWriter
	 * @param indent value of indentation
	 * @param nline not being used really
	 * @return void
	 */
	public void oclPrintFluent(PrintWriter ps, int indent, boolean nline) {
		String pad = "";
		for (int i = 0; i < indent; i++)
			pad = pad + " ";
		ps.print(pad + "value(");
		ListIterator li = args.listIterator();
		ps.print(functor + "(");
		while (li.hasNext()) {
			ps.print(((pArg) li.next()).name);
			if (li.hasNext())
				ps.print(",");
		}
		ps.print("),");
		ps.print(Double.toString(fluentValue));
		ps.print(")");
	}

}
