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

import java.util.List;
import java.util.ArrayList;
import java.util.ListIterator;
import java.io.PrintWriter;
/**
 * This is the base class to support oclPredicate representations, i.e. store predicate details.
 */
public class OPredicate
	implements Cloneable, java.io.Serializable { /* Weihong changed on 31/11/2001 */
	public static final int VAR = 1; /* WZ 5/6/02 */
	public static final int CONST = 2; /* WZ 5/6/02 */
	public static final int INT = 3; /* WZ 5/6/02 */
	protected boolean staticPred;
	protected String functor;
	protected boolean fluent;	   // Ron 25/06/03 added for oclPlus fluents
	protected double	fluentValue; // Ron 25/06/03 added for oclPlus fluents
	protected List args;

	static int varCounter = 0; //Used to create new variables

	/** 
	 * Creates an instance of OPredicate.
	 * @param name - The functor/predicate name as a String
	 */
	public OPredicate(String name) {
		functor = new String(name);
		args = new ArrayList();
		staticPred = false;
	}

	/**
	 * change the name of the predicate - done in conversion from pddl
	 * @param name new name
	 */
	public void setName(String name) {
		functor = name;
	}

	/**
	 * Add a variable to the predicate
	 * @param name name - The variable name as a String
	 */
	public void addVarArgument(String name) {
		args.add(new pArg(VAR, name));
	}
	/** Add a constant value to the predicate
	 * @param name - The constant name as a String
	 */
	public void addConstArgument(String name) {
		args.add(new pArg(CONST, name));
	}
	/** Add a integer value to the predicate
	 * @param name - The integer as an String
	 */
	public void addIntArgument(String name) {
		args.add(new pArg(INT, name));
	}

	/** addSortedvarArgument a variable + sort to the predicate
	 * @param name - The variable name as a String
	 * @param sort - the name of the sort
	 */
	public void addSortedVarArgument(String name, String sort) {
		args.add(new pArg(VAR, name, sort));
	}

	/**
	 * addArgument
	 * add a complete pArg object to the arguments list
	 * @param arg - the argument description
	 */
	public void addArgument(pArg arg) {
		args.add(arg);
	}

	/**
	 * setStatic - set the predicate statis as static or dynamic
	 * @param val - true if predicate static
	 */
	public void setStatic(boolean val) {
		staticPred = val;
	}

	/** 
	 * isStatic - test if predicate is recorded as static
	 * @return boolean - true = static
	 */
	public boolean isStatic() {
		return staticPred;
	}
	// Ron 25/06/03 added setFluent isFluent for oclPlus
	/**
	 * setFluent - set the predicate statis as fluent i.e. return numeric value
	 * @param boolean - true if predicate is a functor
	 */
	public void setFluent(boolean val) {
		fluent = val;
	}

	/** 
	 * isFluent - test if predicate is recorded as a fluent
	 * @return boolean - true = fluent
	 */
	public boolean isFluent() {
		return fluent;
	}
	/** return the number of arguments in the predicate
	 * @return int
	 */
	public int size() {
		return args.size();
	}

	/**
	 * getName - return the predicate name
	 * @return String - the name
	 */
	public String getName() {
		return functor;
	}

	/**
	 * getArguments - return the list of arguments
	 * @return - List of arguments
	 */
	public List getArguments() {
		return args;
	}

	/**
	 * setArgSorts
	 * set the sorts of the arguments to match the sorts in the
	 * given predicate which is assumed to be a prototype predicate
	 * @param proto - the prototype
	 */
	public void setArgSorts(OPredicate proto) {
		ListIterator li = proto.getArguments().listIterator();
		ListIterator liThis = getArguments().listIterator();
		while (li.hasNext()) {
			pArg arg = (pArg) li.next();
			pArg argThis = (pArg) liThis.next();
			argThis.sort = arg.name;
		}
	}

	/**
	 * refersTo
	 * this predicate refers to the given argument value
	 * @param argName - the given argument value
	 * @return boolean 
	 */
	public boolean refersTo(String argName) {
		ListIterator li = getArguments().listIterator();
		while (li.hasNext()) {
			pArg arg = (pArg) li.next();
			if (arg.name.equals(argName)) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * refersToSort
	 * this predicate has an argument of the given Sort
	 * @param argName - the given argument value
	 * @return boolean 
	 */
	// Ron 12/10/02
	public boolean refersToSort(String argName) {
		ListIterator li = getArguments().listIterator();
		while (li.hasNext()) {
			pArg arg = (pArg) li.next();
			if (arg.sort.equals(argName)) {
				return true;
			} else if (arg.sort.equals("N/A")) {
				Utility.debugPrintln("Sort not set for " + functor);
			}
		}
		return false;
	}

	/**
	 * isInstantaited
	 * check that there are no variables in the predicate
	 * @return boolean thrue if fully instantiated
	 */
	public boolean isInstantiated() {
		ListIterator li = getArguments().listIterator();
		while (li.hasNext()) {
			pArg arg = (pArg) li.next();
			if (OEnviron.isVar(arg.name)) {
				return false;
			}
		}
		return true;
	}

	/** Tests if this predicate unifies with the given predicate
	 * @param  given predicate as an OPredicate
	 * @param env the current environment of variable bindings as an OEnviron
	 * @return boolean indicating sucess
	 */
	public boolean unify(OPredicate given, OEnviron env) {
		if (functor.equals(given.functor) && (size() == given.size())) {
			ListIterator li = args.listIterator();
			ListIterator givenli = given.args.listIterator();
			while (li.hasNext()) {
				pArg cur = (pArg) li.next();
				pArg givenCur = (pArg) givenli.next();
				if (!OEnviron.isVar(cur.name)
					&& !OEnviron.isVar(givenCur.name)) {
					if (!cur.name.equals(givenCur.name)) {
						return false;
					}
				} else if (OEnviron.isVar(cur.name)) {
					if (!env.match(cur.name, givenCur.name)) {
						return false;
					}
				} else {
					if (!env.match(givenCur.name, cur.name)) {
						return false;
					}
				}
			}
			return true;
		} else {
			return false;
		}
	}
	
	// Ron 8/4/03
	/**
	 * instantiate  from a given environment
	 * @param env environment
	 */
	public void instantiate (OEnviron env){
		ListIterator li = args.listIterator();
		while (li.hasNext()) {
			pArg cur = (pArg)li.next();
			char n[] = cur.name.toCharArray();
			if (Character.isUpperCase(n[0])) {
				cur.name = env.getVarVal(cur.name);
			}
		}
	}

	/* Weihong added on 21/11/2001 */
	/** Tests if this predicate is the same type with the given predicate
	 * @param given - the given predicate as an OPredicate
	 * @return boolean indicating sucess
	 */
	public boolean isSameType(OPredicate given) {
		if (functor.equals(given.functor) && (size() == given.size())) {
			return true;
		}
		return false;
	}

	/**
	 * genVariable
	 * create a unique new variable name with the prefix of the given name
	 * @param name - given name
	 * @return String - the new variable name
	 */
	public static String genVariable(String name) {
		return new String(name + "_VAR" + varCounter++);
	}

	/* Weihong added on 12/06/2001 */
	public boolean equals(OPredicate opd) {
		if (opd.toString().equals(this.toString()))
			return true;
		return false;
	}

	/**
	 * toString - simple text representation
	 * @return String
	 */
	public String toString() {
		String res;

		ListIterator li = args.listIterator();
		res = new String(functor + "(");
		while (li.hasNext()) {
			res = new String(res + ((pArg) li.next()).name);
			if (li.hasNext())
				res = new String(res + ",");
		}
		return new String(res + ")");
	}

	/**
	 * predListToString
	 * @param predList - of predicates
	 * @return String
	 */
	public static String predListToString(List predList) {
		StringBuffer pad = new StringBuffer();
		pad.append("[");
		ListIterator li = predList.listIterator();
		while (li.hasNext()) {
			pad.append(((OPredicate) li.next()).toString());
			if (li.hasNext())
				pad.append(",");
		}
		pad.append("])");
		return pad.toString();
	}

	/**
	 * elementAt - used to map text offsets to predicate arguments
	 * if offset  not in an element throw exception
	 * count offset from 0 i.e. 0 is first character of functor name
	 * assumes predicate text representation generated by toString()
	 * @param pos offset from start of predicate
	 * @return String - element name
	 */
	public String elementAt(int pos) throws Exception {
		String res = null;
		int curPos = functor.length() + 1; // will include the (
		ListIterator li = args.listIterator();
		while (pos >= curPos && li.hasNext()) {
			res = new String(((pArg) li.next()).name);
			if (pos <= (curPos + res.length())) {
				return res;
			} else {
				curPos = curPos + res.length() + 1;
			}
		}
		throw new Exception("No such element");
	}

	/* Weihong added on 01/06/2001 */
	/**
	 * elementAt - used to map text offsets to predicate arguments
	 * if offset  not in an element throw exception
	 * count offset from 0 i.e. 0 is first character of functor name
	 * assumes predicate text representation generated by toString()
	 * @param pos offset from start of predicate
	 * @return pArg - pArg
	 */
	public pArg pArgAt(int pos) throws Exception {
		pArg res;
		int curPos = functor.length() + 1; // will include the (
		ListIterator li = args.listIterator();
		while (pos >= curPos && li.hasNext()) {
			res = (pArg) li.next();
			if (pos <= (curPos + res.toString().length())) {
				return res;
			} else {
				curPos = curPos + res.toString().length() + 1;
			}
		}
		throw new Exception("No such element");
	}

	/* Weihong added on 09/07/2001 */
	/**
	 * pArg at given index
	 * @param  index of predicate
	 * @return pArg - pArg
	 */
	public pArg pArgOf(int index) throws Exception {
		return (pArg) args.get(index);
	}

	/**
	 * startElementNo - used to map text offsets to predicate arguments
	 * if no not an argument throw an exception
	 * count arguments from 1
	 * count offset from 0 i.e. 0 is first character of functor name
	 * assumes predicate text representation generated by toString()
	 * @param argNo argument number
	 * @return int start offset from 0
	 */
	public int startElementNo(int argNo) throws Exception {
		int curPos = functor.length() + 1; // will include the (
		ListIterator li = args.listIterator();
		int cur = 1;
		while (cur <= argNo && li.hasNext()) {
			String arg = ((pArg) li.next()).name;
			if (cur == argNo) {
				return curPos;
			} else {
				curPos = curPos + arg.length() + 1;
			}
			cur++;
		}
		throw new Exception("No such element");
	}

	/**
	 * startElementAt - used to map text offsets to predicate arguments
	 * if offset  not in an element throw exception
	 * count offset from 0 i.e. 0 is first character of functor name
	 * assumes predicate text representation generated by toString()
	 * @param pos offset from start of predicate
	 * @return int start index of word
	 */
	public int startElementAt(int pos) throws Exception {
		String res = null;
		int curPos = functor.length() + 1; // will include the (
		ListIterator li = args.listIterator();
		while (pos >= curPos && li.hasNext()) {
			res = new String(((pArg) li.next()).name);
			if (pos <= (curPos + res.length())) {
				return curPos;
			} else {
				curPos = curPos + res.length() + 1;
			}
		}
		throw new Exception("No such element");
	}

	/**
	 * elementNoAt - used to map text offsets to predicate arguments
	 * if offset  not in an element throw exception
	 * count offset from 0 i.e. 0 is first character of functor name
	 * assumes predicate text representation generated by toString()
	 * @param pos offset from start of predicate
	 * @return int - argument position counting from 0
	 */
	public int elementNoAt(int pos) throws Exception {
		String res = null; //temp store for argument name
		int argCount = 0;
		int curPos = functor.length() + 1; // will include the (
		ListIterator li = args.listIterator();
		while (pos >= curPos && li.hasNext()) {
			res = new String(((pArg) li.next()).name);
			if (pos <= (curPos + res.length())) {
				return argCount;
			} else {
				curPos = curPos + res.length() + 1;
				argCount++;
			}
		}
		throw new Exception("No such element");
	}

	/**
	 * getNthElement - get the nth argument
	 * throws Exception
	 * @param n int position of argument
	 * @return String argument name
	 */
	public String getNthElementName(int n) {
		return ((pArg) args.get(n)).name;
	}

	/**
	* getNthElementSort - get the nth argument sort
	* throws Exception
	* @param n int position of argument
	* @return String argument sort name
	*/
	/* Weihong added on 31/05/2001 */
	public String getNthElementSort(int n) throws Exception {
		if (((pArg) args.get(n)).sort != "N/A")
			return ((pArg) args.get(n)).sort;

		throw new Exception("Exception: This predicate needs to be verified first.");
	}

	/**
	* set the nth argument sort
	* @param n int position of argument
	* @param sort
	*/
	/* WZ 23/04/2002 */
	public void setNthElementSort(int n, String sort) {
		((pArg) args.get(n)).sort = sort;
	}

	/* WZ 5/6/02 */
	/**
	* set the sort for the given variable
	* @param vName variable name
	* @param sort new sort
	*/
	public void setSortForVariable(String vName, String sort) {
		ListIterator li = args.listIterator();
		while (li.hasNext()) {
			pArg curArg = (pArg) li.next();
			if (curArg.name.equals(vName))
				curArg.sort = sort;
		}
	}

	/**
	 * replaceVariableAt - map offsets to predicate arguments
	 * and replace with a new variable value
	 * if offset  not in an element throw exception
	 * count offset from 0 i.e. 0 is first character of functor name
	 * assumes predicate text representation generated by toString()
	 * @param pos offset from start of predicate
	 * @param vName - new element name
	 * @return the resulting change in string lengths 
	 */
	public int replaceVariableAt(int pos, String vName) throws Exception {
		pArg curArg = null;
		int curPos = functor.length() + 1;
		ListIterator li = args.listIterator();
		while (pos >= curPos && li.hasNext()) {
			curArg = (pArg) li.next();
			if (pos <= (curPos + curArg.name.length())) {
				int change = vName.length() - curArg.name.length();
				curArg.name = new String(vName);
				curArg.aType = VAR;
				return change;
			} else {
				curPos = curPos + curArg.name.length() + 1;
			}
		}
		throw new Exception("No such element");
	}

	/**
	 * replaceVariableNo 
	 * replace with a new variable value
	 * if index  not in an element throw exception
	 * @param index -  pos of variable counting from 0
	 * @param vName - new element name
	 */
	public void replaceVariableNo(int index, String vName) throws Exception {
		pArg curArg = null;
		ListIterator li = args.listIterator();
		int inx = 0;
		while (inx <= index && li.hasNext()) {
			curArg = (pArg) li.next();
			if (inx == index) {
				//this is it 
				curArg.name = new String(vName);
				return;
			} else {
				inx++;
			}
		}
		throw new Exception("No such element");
	}

	/* Weihong added on 01/06/2001 */
	/**
	 * replaceVariableName 
	 * replace with a new variable value
	 * not in an element throw exception
	 * @param  before - the varible
	 * @param after - new element name
	 */
	public void replaceVariableName(pArg before, String after)
		throws Exception {
		pArg curArg = null;
		String tmpStr = null;
		ListIterator li = args.listIterator();
		while (li.hasNext()) {
			curArg = (pArg) li.next();
			if (curArg.name.equals(before.name)) {
				curArg.name = new String(after);
				return;
			}
		}
		throw new Exception("No such element: " + before.toString());
	}

	/* Weihong added on 28/06/2001 */
	/**
	 * replaceVariableName 
	 * replace with a new variable value
	 * not in an element throw exception
	 * @param before element name before - String
	 * @param after - new element name
	 */
	public void replaceVariableNameByName(String before, String after)
		throws Exception {
		pArg curArg = null;
		ListIterator li = args.listIterator();
		while (li.hasNext()) {
			curArg = (pArg) li.next();
			if (curArg.name.equals(before)) {
				curArg.name = new String(after);
				return;
			}
		}
		throw new Exception("No such element: " + before.toString());
	}

	/* Weihong added on 28/06/2001 */
	/**
	 * replaceVariableName 
	 * replace with a new variable value
	 * not in an element throw exception
	 * @param  before - the varible
	 * @param after - new element name
	 */
	public void replaceVariableName(String before, String after)
		throws Exception {
		pArg curArg = null;
		ListIterator li = args.listIterator();
		while (li.hasNext()) {
			curArg = (pArg) li.next();
			if (curArg.sort.equals(before)) {
				curArg.name = new String(after);
				return;
			}
		}
		throw new Exception("No such element: " + before.toString());
	}

	/* ron 29/5/02 */
	/**
	 * removeSortedvarArgument 
	 * remove a predicate argument
	 * @param var - the variable name
	 * @param sort - the variable sort
	 */
	public void removeSortedVarArgument(String var, String sort)
		throws Exception {
		pArg curArg = null;
		ListIterator li = args.listIterator();
		while (li.hasNext()) {
			curArg = (pArg) li.next();
			if (curArg.sort.equals(sort) && curArg.name.equals(var)) {
				args.remove(curArg);
				return;
			}
		}
		throw new Exception("No such element: " + var.toString());
	}

	/** Basic oclPrint routine to print out a predicate in the standard format.
	 */
	public void oclPrintComponent(PrintWriter ps, int indent, boolean nline) {
		String pad = "";
		for (int i = 0; i < indent; i++)
			pad = pad + " ";
		ListIterator li = args.listIterator();
		ps.print(pad + functor + "(");
		while (li.hasNext()) {
			ps.print(((pArg) li.next()).name);
			if (li.hasNext())
				ps.print(",");
		}
		ps.print(")");
	}

	/* Weihong added on 01/06/2001 */
	/**
	 * toVar 
	 * replace with a variable value (upper case)
	 */
	public void toVar() {
		ListIterator li = args.listIterator();
		while (li.hasNext()) {
			pArg curArg = (pArg) li.next();
			curArg.name = toVar(curArg.name);
		}
	}

	/**
	 * toVar = makes the first character uppercase
	 * @param  name
	 * @return String var
	 */
	public static String toVar(String name) {
		char n[] = name.toCharArray();
		if (Character.isLowerCase(n[0])) {
			n[0] = Character.toUpperCase(n[0]);
		}
		return new String(n);
	}
	
	/**
		 * toPDDLVar = makes the first character ?
		 * @param  name
		 * @return String var
		 */
		public static String toPDDLVar(String name) {
			return new String("?" + name);
		}

	/**
	 * toConst = makes the first character lowercase
	 * @param  name
	 * @return String var
	 */
	public static String toConst(String name) {
		char n[] = name.toCharArray();
		if (Character.isUpperCase(n[0])) {
			n[0] = Character.toLowerCase(n[0]);
		}
		return new String(n);
	}

	/**
	 * isSortRestrictionFor
	 * test to see if predicate is a is_of_sort predicate for the
	 * given Variable Name
	 * @param vName - the given Variable Name
	 * @return boolean 
	 */
	public boolean isSortRestrictionFor(String vName) {
		if (functor.equals("is_of_sort")
			&& getNthElementName(0).equals(vName)) {
			return true;
		} else {
			return false;
		}
	}

	/**
	 * clone()
	 *
	 */
	public Object clone() throws CloneNotSupportedException {
		OPredicate res = new OPredicate(functor);
		res.staticPred = this.staticPred;
		ListIterator li = args.listIterator();
		while (li.hasNext()) {
			/* Weihong added on 31/05/2001 */
			pArg parg = (pArg) li.next();
			res.args.add((pArg) parg.clone());
		}
		return (OPredicate) res;
	}

	/**
	 * sortsToVars()
	 *    produce a clone but replace sort name by variable names
	 * @return OPredicate
	 */
	public OPredicate sortsToVars() {
		OPredicate res = new OPredicate(functor);
		res.staticPred = this.staticPred;
		ListIterator li = args.listIterator();
		while (li.hasNext()) {
			/* Weihong added on 31/05/2001 */
			pArg parg = (pArg) li.next();
			res.args.add(new pArg(VAR, toVar(parg.name), parg.name));
		}
		return (OPredicate) res;
	}
	
	/**
	 * get the value of the predicate if the predicate is a fluent
	 * @return
	 */
	public double getFluentValue() {
		return fluentValue;
	}

	/**
	 * set the value of the predicate if the predicate is a fluent
	 * @param d
	 */
	public void setFluentValue(double d) {
		fluentValue = d;
	}

	/** Representation of a predicate argument.
	 * Stores the type and text of an argument.
	 */
	public class pArg
		implements Cloneable, java.io.Serializable { /* Weihong changed on 31/11/2001 */
		public int aType;
		public String name;
		public String sort;

		public pArg(int type, String n) {
			aType = type;
			name = new String(n);
			sort = "N/A";
		}

		public pArg(int type, String n, String s) {
			aType = type;
			name = new String(n);
			sort = new String(s);
		}

		/* Weihong added on 29/05/2001 */
		public boolean equals(pArg parg) {
			if (this.name.equals(parg.name) && this.sort.equals(parg.sort))
				return true;
			return false;
		}

		public String toString() {
			return name;
		}

		public Object clone() throws CloneNotSupportedException {
			return new pArg(aType, name, sort);
		}
	}

}
