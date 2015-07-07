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

import jplan.general.*;

/**
 * Store Predicate Details 
 */
public class oclPredicate
	extends OPredicate
	implements oclPrint, Serializable, Cloneable {
	/**
	 * creates an instance of the oclPredicate
	 * @param name the name of the oclPredicate
	 */
	public oclPredicate(String name) {
		super(name);
	}

	/**
	 * clones the predicate but copies sort name 
	 * to variable name.
	 * @return oclPredicate 
	 */
	public oclPredicate copySortsToVars() {
		oclPredicate res = new oclPredicate(functor);
		ListIterator li = args.listIterator();
		while (li.hasNext()) {
			pArg cur = (pArg) li.next();
			res.addSortedVarArgument(OPredicate.toVar(cur.name), cur.name);
		}
		return res;
	}

	/* WZ 24/6/02 */
	/**
	 * returns an oclpredicate with sorts replaced variables
	 * @return oclPredicate 
	 */
	public oclPredicate getSort() {
		oclPredicate res = new oclPredicate(functor);
		ListIterator li = args.listIterator();
		while (li.hasNext()) {
			pArg cur = (pArg) li.next();
			res.addConstArgument(cur.sort);
		}
		return res;
	}

	/**
	 * rename variables to ensure no name clases when unifying
	 * @param env environment containing Variable Re-namedVariable pairs
	 * @return oclPredicate with renamed variables
	 */
	public oclPredicate renameVars(OEnviron env) {
		oclPredicate newPred = new oclPredicate(this.functor);
		newPred.setStatic(this.isStatic());
		ListIterator li = args.listIterator();
		while (li.hasNext()) {
			pArg cur = (pArg) li.next();
			pArg newArg = new pArg(cur.aType, cur.name, cur.sort);
			if (OEnviron.isVar(cur.name)) {
				String varVal = env.getVarVal(cur.name);
				if (varVal == cur.name) { // No binding - not yet renamed
					String newVar = genVariable(cur.name);
					env.addBinding(cur.name, newVar);
					newArg.name = newVar;
				} else {
					newArg.name = varVal;
				}
			}
			newPred.addArgument(newArg);
		}
		return newPred;
	}
	
	/**
     * collectVariableAssociations
     * used by life history editor to ensure standard variables for different Property predicates
	 * are not accidently unified
	 * problem occurs when there are multiple vare occurances within the same se/sc LHS
     * @param hashVars
     */
    public void collectVariableAssociations(Hashtable hashVars) {
    	ListIterator li = args.listIterator();
    	while (li.hasNext()) {
    		pArg arg = (pArg)li.next();
    		List vals = (List)hashVars.get(arg.name);
    		if (vals == null) {
    			vals = new ArrayList();
    			vals.add(this);
    			hashVars.put(arg.name,vals);
    		} else {
    			vals.add(this);
    		} 
    	}
    }

	/**
	 * check that all arguments are constant and that all name objects
	 * of the correct sort in the domain
	 * @param cur the current domain
	 * @return boolean
	 */
	public boolean isInstantiated(oclDomain cur) {
		ListIterator li = args.listIterator();
		while (li.hasNext()) {
			pArg arg = (pArg) li.next();
			List objs = cur.getObjectsOfSubTypes(arg.sort);
			if (objs == null) {
				return false;
			}
			ListIterator liObjs = objs.listIterator();
			boolean found = false;
			while (!found && liObjs.hasNext()) {
				String cObj = (String) liObjs.next();
				if (cObj.equals(arg.name)) {
					found = true;
				}
			}
			if (!found) {
				return false;
			}
		}
		return true;
	}

	/**
	 * promoteSortsToVars()
	 *    produce a clone but replace sort name by variable names
	 * @return oclPredicate
	 */
	public oclPredicate promoteSortsToVars() {
		oclPredicate res = new oclPredicate(functor);
		res.staticPred = this.staticPred;
		ListIterator li = args.listIterator();
		while (li.hasNext()) {
			/* Weihong added on 31/05/2001 */
			pArg parg = (pArg) li.next();
			res.args.add(new pArg(1, OPredicate.toVar(parg.name), parg.name));
		}
		return (oclPredicate) res;
	}
	
	/**
	 * destinguishVars()
	 *    produce a clone but number repeat variable names
	 * @return oclPredicate
	 */
	public oclPredicate destinguishVars() {
		Hashtable vars = new Hashtable();
		oclPredicate res = new oclPredicate(functor);
		res.staticPred = this.staticPred;
		ListIterator li = args.listIterator();
		while (li.hasNext()) {
			pArg parg = (pArg)li.next();
			Integer val = (Integer)vars.get(parg.name);
			String argName = null;
			if (val == null) {
				// This is the first one
				vars.put(parg.name,new Integer(2));
				argName = parg.name;
			} else {
				int iVal = val.intValue();
				vars.put(parg.name,new Integer(iVal+1));
				argName = parg.name + iVal;
			}
			res.args.add(new pArg(1, argName , parg.sort));
		}
		return (oclPredicate) res;
	}
	
	/**
	 * destinguishVarsAlpha()
	 *    produce a clone but add "A" "B" letter suffix
	 * @return oclPredicate
	 */
	public oclPredicate destinguishVarsAlpha() {
		Hashtable vars = new Hashtable();
		Hashtable suffixes = new Hashtable();
		oclPredicate res = new oclPredicate(functor);
		res.staticPred = this.staticPred;
		ListIterator li = args.listIterator();
		while (li.hasNext()) {
			pArg parg = (pArg)li.next();
			Boolean multi = (Boolean)vars.get(parg.name);
			if (multi == null) {
				// This is the first one
				vars.put(parg.name,new Boolean(false));
			} else {
				vars.put(parg.name,new Boolean(true));
			}
		}
		// Now rename
		li = args.listIterator();
		while (li.hasNext()) {
			pArg parg = (pArg)li.next();
			Boolean multi = (Boolean)vars.get(parg.name);
			String argName = parg.name;
			if (multi.booleanValue()) {
				// This variable repeats
				String argSuffix = (String)suffixes.get(argName);
				if (argSuffix == null) {
					// First one 
					argName = parg.name + "A";
					suffixes.put(parg.name,"A");
				} else {
					String nextSuffix = new String("" + (char)((argSuffix.charAt(0)) + 1));
					suffixes.put(parg.name,nextSuffix);
					argName = parg.name + nextSuffix;
				}
			} 
			res.args.add(new pArg(1, argName , parg.sort));
		}
		return (oclPredicate) res;
	}
	
	/**
	 * destinguishVarsAlpha()
	 *    DOES NOT produce a clone but add "A" "B" letter suffix
	 * @return oclPredicate
	 */
	public void destinguishVarsAlpha(String sep) {
		Hashtable vars = new Hashtable();
		Hashtable suffixes = new Hashtable();
		oclPredicate res = new oclPredicate(functor);
		res.staticPred = this.staticPred;
		ListIterator li = args.listIterator();
		while (li.hasNext()) {
			pArg parg = (pArg)li.next();
			Boolean multi = (Boolean)vars.get(parg.name);
			if (multi == null) {
				// This is the first one
				vars.put(parg.name,new Boolean(false));
			} else {
				vars.put(parg.name,new Boolean(true));
			}
		}
		// Now rename
		li = args.listIterator();
		while (li.hasNext()) {
			pArg parg = (pArg)li.next();
			Boolean multi = (Boolean)vars.get(parg.name);
			String argName = parg.name;
			if (multi.booleanValue()) {
				// This variable repeats
				String argSuffix = (String)suffixes.get(argName);
				if (argSuffix == null) {
					// First one 
					argName = parg.name + sep + "A";
					suffixes.put(parg.name, "A");
				} else {
					String nextSuffix = new String("" + (char)((argSuffix.charAt(0)) + 1));
					suffixes.put(parg.name,nextSuffix);
					argName = parg.name + sep +nextSuffix;
				}
			} 
			res.args.add(new pArg(1, argName , parg.sort));
		}
		this.args = res.args;
	}
	

	/* WZ 18/4/02 */
	/**
	 * returns prototype of this predicate
	 * BUT sets the sorts of the arguments to this predicate to the proto type sorts
	 * @return OPredicate - the prototype or null if not found
	 */
	public oclPredicate getProtoType() {
		//Utility.debugPrintln("studying =====> " + toString());
		oclPredicate proto = new oclPredicate(functor);
		proto.setStatic(staticPred);
		if (getArguments().size() == 0)
			return null;

		ListIterator li = getArguments().listIterator();
		while (li.hasNext()) {
			pArg arg = (pArg) li.next();
			//Utility.debugPrintln("arg.sort ==> '" + arg.sort + "'");
			if (arg.sort != null && !arg.sort.equals("N/A"))
				proto.addSortedVarArgument(arg.sort, arg.sort);
			else
				return null;
		}

		return proto;
	}
	
	// Ron 29/4/03
	/**
	 * returns prototype of this predicate
	 * BUT sets the sorts of the arguments to this predicate to the proto type sorts
	 * @param dom - The current domain
	 * @return OPredicate - the prototype
	 */
	public oclPredicate getProtoType(oclDomain dom) {
		oclPredicate proto = null;
		try {
			proto = dom.findPrototype(this);
		} catch (OCLSelectionException e) {
			return null;
		}
		setArgSorts(proto);
		return proto;
	}

	/**
	 * clone a copy of oclPredicate
	 * @throws CloneNotSupportedException
	 * @return Object
	 */
	public Object clone() throws CloneNotSupportedException {
		oclPredicate res = new oclPredicate(functor);
		res.staticPred = this.staticPred;
		res.fluent = this.fluent;	//Ron 25/05/03
		res.fluentValue = this.fluentValue; //Ron 25/05/03
		ListIterator li = args.listIterator();
		while (li.hasNext()) {
			/* Weihong added on 31/05/2001 */
			pArg parg = (pArg) li.next();
			res.args.add((pArg) parg.clone());
		}
		return (oclPredicate) res;
	}

	/* 6/6/02 */
	/**
	 * to check if the given newState has a state
	 * which is in the same level of the current predicate.
	 * @param newState list of predicates
	 * @return true if there is a matching state
	 */
	public boolean hasMatchingStates(
		String sort,
		oclDomain curDomain,
		List newState) {
		String curSort = sort;
		oclSSClassDef def = curDomain.getStates(curSort);
		if (def != null) {
			if (def.hasPredicate(this)) {
				//check new state
				ListIterator li = newState.listIterator();
				while (li.hasNext()) {
					oclPredicate opd = (oclPredicate) li.next();
					if (def.hasPredicate(opd)) {
						return true;
					}
				}
			}
		}

		try {
			curSort = curDomain.findSortParent(curSort);
			return hasMatchingStates(curSort, curDomain, newState);
		} catch (java.util.NoSuchElementException e) {
			return false;
		}
	}

	/* WZ 11/6/02 */
	/**
	 * to check if current oclPredicate has the given variable.
	 * @param s the given variable.
	 * @return true if current oclPredicate has the given variable.
	 */
	public boolean hasConstant(String s) {
		ListIterator li = args.listIterator();
		while (li.hasNext()) {
			/* Weihong added on 31/05/2001 */
			pArg parg = (pArg) li.next();
			if (s.equals(parg.name))
				return true;
		}
		return false;
	}

	/* WZ 11/6/02 */
	/**
	 * to check between two predicates, find different vaiables
	 * and record the change in a new OPredicate.\n
	 * @param earlyOpd - oclPredicate before change, 
	 * must be same type with cur OPredicate.
	 * @return void
	 */
	public oclPredicate getInstVariables(oclPredicate earlyOpd) {
		oclPredicate ret = new oclPredicate(functor);
		for (int i = 0; i < args.size(); i++) {
			String s = ((pArg) args.get(i)).name;
			String e = earlyOpd.getNthElementName(i);
			if (!e.equals(s)) {
				//check if have it already...
				if (!ret.hasConstant(e))
					ret.addSortedVarArgument(e, s);
			}
		}
		return ret;
	}
	
	/**
	 * to print the current oclPredicate to a PrintWriter.
	 * @param ps PrintWriter
	 * @param indent value of indentation
	 * @param nline not being used really
	 * 
	 */
	public void oclSortedPrint(PrintWriter ps, int indent, boolean nline) {
		String pad = "";
		for (int i = 0; i < indent; i++)
			pad = pad + " ";
		ListIterator li = args.listIterator();
		ps.print(pad + functor + "(");
		while (li.hasNext()) {
			ps.print("arg(");
			pArg arg = (pArg)li.next();
			ps.print(arg.name);
			ps.print(",");
			ps.print(arg.sort);
			ps.print(")");
			if (li.hasNext())
				ps.print(",");
		}
		ps.print(")");
	}

	/**
	 * to print the current oclPredicate to a PrintWriter.
	 * @param ps PrintWriter
	 * @param indent value of indentation
	 * @param nline not being used really
	 * 
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
	
	/**
	 * to translate to PDDL and print the current prototype oclPredicate to a PrintWriter.
	 * @param curDom - the current ocl domain
	 * @param ps PrintWriter
	 * @param indent value of indentation
	 * @param nline not being used really
	 * 
	 */
	public void pddlPrintPrototype(oclDomain curDom,PrintWriter ps, int indent, boolean nline) {
		Hashtable varsHash = new Hashtable();
		String pad = "";
		for (int i = 0; i < indent; i++)
			pad = pad + " ";
		ListIterator li = args.listIterator();
		ps.print(pad + "(" + functor + " ");
		while (li.hasNext()) {
			String sortName = (String)(((pArg) li.next()).name);
			List prims = curDom.getPrimitiveSubTypes(sortName);
			Integer inx = (Integer)varsHash.get(sortName);
			String suffix = "1";
			int newInx = 1;
			if (inx != null) {
				newInx = inx.intValue() + 1;
				suffix = new String("" + newInx);
			}
			varsHash.put(sortName,new Integer(newInx));
			if (prims.size() == 0) {
				ps.print(toPDDLVar(sortName + suffix) + " - " + sortName);
				if (li.hasNext())
					ps.print(" ");
			} else {
				ps.print(toPDDLVar(sortName + suffix) + " -  (either");
				ListIterator liPrims = prims.listIterator();
				while (liPrims.hasNext()) {
					String pSort = (String)liPrims.next();
					ps.print(" " + pSort);
				}
				ps.print(")");
				if (li.hasNext())
					ps.print(" ");
			}
		}
		if (nline)
		ps.println(")");
		else
			ps.print(")");
	}
	
	/**
	 * to translate to PDDL and print the current oclPredicate to a PrintWriter.
	 * @param curDom - the current ocl domain
	 * @param ps PrintWriter
	 * @param indent value of indentation
	 * @param nline not being used really
	 * 
	 */
	public void pddlPrint(oclDomain curDom,PrintWriter ps, int indent, boolean nline) {
		String pad = "";
		for (int i = 0; i < indent; i++)
			pad = pad + " ";
		ListIterator li = args.listIterator();
		if (functor.equals("ne")) {
			ps.print(pad + "(not (= ");
		} else {
			ps.print(pad + "(" + functor + " ");
		}
		while (li.hasNext()) {
			pArg arg = (pArg)li.next();
			String varName = (String)arg.name;
			if (arg.aType == VAR) {
				ps.print(toPDDLVar(varName));
			} else if (arg.aType == CONST){
				ps.print(varName);
			}
			if (li.hasNext())
				ps.print(" ");
		}
		if (functor.equals("ne")) {
			ps.print(")");
		}
		if (nline)
			ps.println(")");
		else
			ps.print(")");
	}

	/**
	 * to print the current oclPredicate to a PrintWriter.
	 * @param ps PrintWriter
	 * @param indent value of indentation
	 * @param nline not being used really
	 * 
	 */
	public void htmlPrint(PrintWriter ps, int indent, boolean nline) {
		String pad = "";
		for (int i = 0; i < indent; i++)
			pad = pad + " ";
		ListIterator li = args.listIterator();
		ps.print(pad + functor + O2HTML.red("("));
		while (li.hasNext()) {
			ps.print(((pArg) li.next()).name);
			if (li.hasNext())
				ps.print(O2HTML.red(","));
		}
		ps.print(O2HTML.red(")"));
	}
}
