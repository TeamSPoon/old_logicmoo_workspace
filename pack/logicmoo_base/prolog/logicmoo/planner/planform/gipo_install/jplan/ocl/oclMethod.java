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

// oclMethod.java Top level class to store ocl hierarchical operator
package jplan.ocl;

import java.util.List;
import java.util.ArrayList;
import java.util.ListIterator;
import java.util.Hashtable;
import java.io.*;
import jplan.general.OPredicate; /* Weihong  11/02/2002 */
import jplan.general.Utility; /* Weihong  11/02/2002 */
import jplan.general.OCLException;

/* Weihong changed on 30/11/2001 */
public class oclMethod
	extends jplan.general.TransferableObject
	implements oclPrint, Serializable {
	oclPredicate methodName;
	String description = new String(); /* WZ 14/5/02 */
	/**
	 * Preconditions - a set of object expressions that must be true before this method,
	 * unlike the prvail in a primitive operator objects in precons may be affected by
	 * the operators in the expansion of this method.
	 */
	private List precons = new ArrayList();
	/**
	 * is a set of necessary state transitions.
	 */
	private List index = new ArrayList();
	/**
	 * static conditions on the parameters in the operator.
	 */
	private List statics = new ArrayList();
	/**
	 * Temporal constraints on the nodes in the decomps respectively.
	 */
	private List temps = new ArrayList();
	/**
	 * contains nodes in the usual HTN fashioin: a node is either the name of
	 * a primitive operator, the name of a primitive operator, the name of a
	 * compound operator, or an expresssion of the form 'achieve-goal(G)',
	 * where G is a class expression.
	 */
	private List decomps = new ArrayList();

	/**
	 * contains comments describing this method
	 */
	private List docm = new ArrayList();

	public oclMethod() {
		super();
	}

	/* WZ 14/5/02 */
	/**
	   * Get the value of description.
	   * @return Value of description.
	   */
	public String getDescription() {
		if (docm.size() > 0) {
			StringBuffer sb = new StringBuffer();
			// 	    sb.append("/****\n");
			ListIterator liDoc = docm.listIterator();
			while (liDoc.hasNext()) {
				// 		sb.append(",");
				if (liDoc.hasNext())
					sb.append((String) liDoc.next() + "\n");
			}
			// 	    sb.append(" */");
			return sb.toString();
		}
		return new String();
	}

	/* WZ 16/8/02 */
	/**
	   * Set the value of description.
	   * 
	   */
	public void setDescription(String desc) {
		docm.clear();
		addDocmLine(desc);
	}

	/* WZ 14/5/02 */
	/**
	   * Set the value of description.
	   * @param v  Value to assign to description. 
	   */
	//     public void setDescription(String  v) {this.description = v;}

	/* Weihong  8/11/2001 */
	/**
	 * Sets the method's name.
	 * @param fname the name of the oclPredicate which forms the method's name.
	 * @return the method's name as an oclPredicate.
	 */
	public oclPredicate addName(String fname) {
		oclPredicate cur = new oclPredicate(fname);
		methodName = cur;
		return cur;
	}

	/* Weihong  8/11/2001 */
	/**
	 * Gets the method's name.
	 * @return the method's name as an oclPredicate.
	 */
	public oclPredicate getName() {
		return methodName;
	}

	/* Weihong  19/11/2001 */
	/**
	 * Sets the method's name.
	 * @param cur the name of the oclMethod
	 * 
	 */
	public void setName(oclPredicate cur) {
		methodName = cur;
	}

	/* Weihong  8/11/2001 */
	/**
	 * Gets the precondition list.
	 * @return a list oclSEs.
	 */
	public List getPrecondition() {
		return precons;
	}

	/* Weihong  8/11/2001 */
	/**
	 * Add preconditions.
	 * @param sort sort
	 * @param name name
	 * @return the oclSE.
	 */
	public oclSE addPreSE(String sort, String name) {
		oclSE cur = new oclSE(sort, name);
		precons.add(cur);
		return cur;
	}

	/* Weihong  19/11/2001 */
	/**
	 * Add preconditions.
	 * @param oclse the oclSE to be added
	 * 
	 */
	public void addPreSE(oclSE oclse) {
		precons.add(oclse);
	}

	/* Weihong  8/11/2001 */
	/**
	 * Gets the index list.
	 * @return a list oclSCs.
	 */
	public List getIndex() {
		return index;
	}

	/* Weihong  8/11/2001 */
	/**
	 * Add index.
	 * @param sort sort
	 * @param name name
	 * @return the index - oclSC.
	 */
	public oclSC addIndexSC(String sort, String name) {
		oclSC cur = new oclSC(sort, name);
		index.add(cur);
		return cur;
	}

	/* Weihong  6/11/2001 */
	/**
	 * Add a set of necessary state transitions.
	 * @param cur a set of necessary state transitions
	 * 
	 */
	public void addIndexSC(oclSC cur) {
		index.add(cur);
	}

	/* Weihong  19/11/2001 */
	/**
	 * Add a set of necessary state transitions.
	 * @param listSC list of oclSCs
	 * 
	 */
	public void setIndexSC(List listSC) {
		ListIterator l = listSC.listIterator();
		while (l.hasNext()) {
			try {
				index.add((oclSC) ((oclSC) l.next()).clone());
			} catch (CloneNotSupportedException e) {
			}
		}
	}

	/* Weihong  8/11/2001 */
	/**
	 * Gets the static condition list.
	 * @return a list oclPredicatess.
	 */
	public List getStatics() {
		return statics;
	}

	/* Weihong  8/11/2001 */
	/**
	 * Add static clauses.
	 * @param fname the name of the oclPredicate.
	 * @return the static conditions.
	 */
	public oclPredicate addStaticClause(String fname) {
		oclPredicate cur = new oclPredicate(fname);
		statics.add(cur);
		return cur;
	}

	/* Weihong  6/11/2001 */
	/**
	 * Add the static conditions.
	 * @param cur oclPredicate: the static conditions
	 * 
	 */
	public void addStatic(oclPredicate cur) {
		statics.add(cur);
	}

	/* WZ 29/4/02 */
	/**
	 * set the static list.
	 * @param list the static list
	 * 
	 */
	public void setStatic(List list) {
		statics = list;
	}

	/* Weihong  8/11/2001 */
	/**
	 * Add temporal clauses.
	 * @param fname the name of the oclPredicate.
	 * @return the temporal constraints.
	 */
	public oclPredicate addTempClause(String fname) {
		oclPredicate cur = new oclPredicate(fname);
		temps.add(cur);
		return cur;
	}

	/* Weihong  21/11/2001 */
	/**
	 * Add temporal clauses.
	 * @param before the position of the decomposed action.
	 * @param after the position of the decomposed action.
	 * 
	 */
	public void addTempClauseBefore(String before, String after) {
		oclPredicate cur = new oclPredicate("before");
		cur.addVarArgument(before);
		cur.addVarArgument(after);
		if (!hasTempClause(cur)) /* WZ 7/5/02 */
			temps.add(cur);
	}

	/* WZ 7/5/02 */
	/**
	 * return true if the given oclPredicate is the same
	 * as the one already in the temporal list.
	 * @param cur given oclPredicate
	 * @return true if the given oclPredicate is the same
	 */
	public boolean hasTempClause(oclPredicate cur) {
		ListIterator li = temps.listIterator();
		while (li.hasNext()) {
			oclPredicate opd = (oclPredicate) li.next();
			if (opd.equals(cur))
				return true;
		}
		return false;
	}

	/* Weihong  6/11/2001 */
	/**
	 * Add the temporal constaints.
	 * @param cur oclPredicate: the temporal constaints
	 * 
	 */
	public void addTemps(oclPredicate cur) {
		temps.add(cur);
	}

	/* Weihong  21/11/2001 */
	/**
	 * Gets the temporal constaints.
	 * @return a list of predicates with information of temporal constraints.
	 */
	public List getTemps() {
		return temps;
	}

	/* Weihong  21/11/2001 */
	/**
	 * Sets the temporal constaints.
	 * @param tempsList a list of predicates with information
	 * of temporal constraints.
	 * 
	 */
	public void setTemps(List tempsList) {
		temps = tempsList;
	}

	/* Weihong  8/11/2001 */
	/**
	 * Add decomposed clauses.
	 * @param fname the name of the oclPredicate which
	 * forms the method/operator's name.
	 * @return the added clause.
	 */
	public oclPredicate addDecompClause(String fname) {
		oclPredicate cur = new oclPredicate(fname);
		decomps.add(cur);
		return cur;
	}

	/* Weihong  8/11/2001 */
	/**
	 * Add decomposed oclSS.
	 * @param kind sort
	 * @param ID name
	 * @return the added oclSS.
	 */
	public oclSS addDecompSS(String kind, String ID) {
		oclSS cur = new oclSS(kind, ID);
		decomps.add(cur);
		return cur;
	}

	/* Weihong  6/11/2001 */
	/**
	 * Add oclSS to achieve.
	 * @param cur the fact (conditions) for the following
	 * operators in the decomposition
	 * 
	 */
	public void addDecomps(oclSS cur) {
		decomps.add(cur);
	}

	/* Weihong  19/11/2001 */
	/**
	 * Gets the decomposition list
	 * @return a list of oclSS/oclMethod/oclOperator
	 */
	public List getDecomps() {
		return decomps;
	}

	/* WZ  25/7/02 */
	/**
	 * replace an old predicate with the given new one.
	 * @param old the name to be matched with oclPredicate Name
	 * @param newPrd the new predicate to place the old one
	 * 
	 */
	public void changeDecomps(String old, oclPredicate newPrd) {
		List shadowDecomps = new ArrayList();
		ListIterator li = decomps.listIterator();
		while (li.hasNext()) {
			Object obj = li.next();
			Utility.debugPrintln("obj: " + obj.getClass().getName());
			if (obj.getClass().getName().equals("jplan.ocl.oclPredicate"))
				if (((oclPredicate) obj).getName().equals(old))
					shadowDecomps.add(newPrd);
				else
					shadowDecomps.add((oclPredicate) obj);
			else
				shadowDecomps.add((oclSS) obj);
		}

		decomps = shadowDecomps;
	}

	/* Weihong  21/11/2001 */
	/**
	 * Sets the decomposition list
	 * @param listDecomp a list of oclSS/oclMethod/oclOperator
	 * 
	 */
	/* WZ added to clone */
	public void setDecomps(List listDecomp) {
		// 	decomps.clear();
		// 	ListIterator li = listDecomp.listIterator();
		// 	while (li.hasNext()){
		// 	    decomps.add(li.next());
		// 	}
		decomps = listDecomp;
	}

	/* Weihong  6/11/2001 */
	/**
	 * Add the operator/method's name.
	 * @param cur oclPredicate: the operator/method's name
	 * 
	 */
	public void addDecomps(oclPredicate cur) {
		decomps.add(cur);
	}

	// Ron 14/4/02
	/**
	 * addDocmLine
	 * add a comment line of documentation to the operator
	 * @param line - the documentation line
	 */
	public void addDocmLine(String line) {
		docm.add(line);
	}

	/* WZ 21/6/02 */
	/**
	 * createAllSignature
	 * find all predicate pArguments (variabl and constant)
	 * and their sorts and form the compound operator name
	 * @param domain
	 * @return oclPredicate - the signature / name
	 */
	public oclPredicate createAllSignature(oclDomain domain) {
		oclPredicate variblePredicate = new oclPredicate(methodName.getName());

		ListIterator li, lii;

		//Index - oclSC
		li = getIndex().listIterator();
		while (li.hasNext()) {
			oclSC sc = (oclSC) li.next();
			lii = sc.getPre().listIterator();
			while (lii.hasNext()) {
				domain.addAllArgument(
					variblePredicate,
					(oclPredicate) lii.next());
			}
			lii = sc.getPost().listIterator();
			while (lii.hasNext()) {
				domain.addAllArgument(
					variblePredicate,
					(oclPredicate) lii.next());
			}
		}

		setName(variblePredicate);

		return variblePredicate;
	}

	/* Weihong  6/2/2002 */
	/**
	 * do verification checks on methods.<br>
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
		oclPredicate lowLevelSortList = new oclPredicate(""); /* WZ 5/6/02 */
		if (methodName != null) { /* WZ 21/6/02 */
			mssgs.add("Checking method " + methodName.toString());
		}
		//precondition
		ListIterator li = precons.listIterator();
		while (li.hasNext()) {
			oclSE cur = (oclSE) li.next();
			if (alreadyUsed(cur.getName(), cur.getSort(), ids, sorts)) {
				mssgs.add(
					"The object "
						+ cur.getName()
						+ " has more than one transition clause");
				mssgs.add("    in the operator " + methodName.toString());
				ret = false;
			} else {
				ret = cur.check(curDom, mssgs);
				ids.add(cur.getName());
				sorts.add(cur.getSort());

				/* WZ 5/6/02 */
				lowLevelSortList.addSortedVarArgument(
					cur.getName(),
					cur.getSort());
			}
		}
		//index
		li = index.listIterator();
		while (li.hasNext()) {
			oclSC cur = (oclSC) li.next();
			if (alreadyUsed(cur.getName(), cur.getSort(), ids, sorts)) {
				mssgs.add(
					"The object "
						+ cur.getName()
						+ " has more than one transition clause");
				mssgs.add("    in the operator " + methodName.toString());
				ret = false;
			} else {
				ret = cur.check(curDom, mssgs);
				ids.add(cur.getName());
				sorts.add(cur.getSort());

				/* WZ 5/6/02 */
				lowLevelSortList.addSortedVarArgument(
					cur.getName(),
					cur.getSort());
			}
		}

		List decomposeL = new ArrayList();
		/* Weihong 19/2/02 consider decomposition */
		//decomposition - oclPredicate or oclSS
		// Ron 15/5/03 make sure that the decomposition is defined
		if (decomps.size() == 0) {
			mssgs.add("There is no decomposition for this method.");
			ret = false;
		}
		li = decomps.listIterator();
		while (li.hasNext()) {
			Object obj = (Object) li.next();
			if (obj.getClass().getName() == "jplan.ocl.oclSS") {
				oclSS cur = (oclSS) obj;
				ret = cur.checkAchieveSS(curDom, mssgs);
				ids.add(cur.getName());
				sorts.add(cur.getSort());
				decomposeL.add(cur);

				/* WZ 5/6/02 */
				lowLevelSortList.addSortedVarArgument(
					cur.getName(),
					cur.getSort());
			}
			//WZ 18/4/02
			else if (obj.getClass().getName() == "jplan.ocl.oclPredicate") {
				oclPredicate sig = (oclPredicate) obj; /* WZ 21/8/02 */
				//add sort info to the predicates
				Object object = curDom.checkObjectType(sig);
				if (object == null) {
					if (methodName != null) {
						mssgs.add(
							new String(
								"found an unrecognised decomposition item: "
									+ sig.toString()
									+ "'."));
						/* WZ 23/8/02 */
						// 			javax.swing.JOptionPane.showMessageDialog(null,
						// 				    "method:"+methodName.toString()+ 
						// 				    "\nhas an unrecognised decomposition item:"
						// 				    +"\n'"+sig.toString()+"'.",
						// 				    "GIPO Error",
						// 				    javax.swing.JOptionPane.ERROR_MESSAGE,
						// 				    null);	
					}
					ret = false;
					// 		    sig = (oclPredicate)obj;
					// 		    continue;
				}
// Ron 22/4/03 Removed this unfortunately I don't see the purpose of this but there must be one!!
// If re-instating it does not work if the decomposition contains instantiated variables
// in the signature - they get stripped out of the signature. THEY NEED TO BE 
// PRESERVED.
//				} else if (
//					object.getClass().getName() == "jplan.ocl.oclMethod") {
//					oclMethod om = (oclMethod) object;
//					// sig = curDom.createMethodSignature(om); Ron 22/4/03
//				} else if (
//					object.getClass().getName() == "jplan.ocl.oclOperator") {
//					oclOperator op = (oclOperator) object;
//					//sig = curDom.createOperatorSignature(op); Ron 22/4/03
//				}
//				decomposeL.add(sig);
				//change the signature but not in the oclDomain
			}
			//setDecomps(decomposeL); Ron 22/4/03
		}

		//statics - oclPredicate
		li = statics.listIterator();
		while (li.hasNext()) {
			oclPredicate curPred = null;
			try {
				curPred = (oclPredicate) li.next();
				if (curPred.getName().equals("ne") && curPred.size() == 2) {
					String var1 = curPred.getNthElementName(0);
					String var2 = curPred.getNthElementName(1);
					String var1Sort = findSortForVariable(var1);
					String var2Sort = findSortForVariable(var2);
					if (var1Sort == null || var2Sort == null)
						mssgs.add(
							"Prediate "
								+ curPred.toString()
								+ " does not have compatible sorts.");
					else {
						Utility.debugPrintln("var1Sort - " + var1Sort);
						curPred.setNthElementSort(0, var1Sort);
						curPred.setNthElementSort(1, var2Sort);
					}
				} else {
					oclPredicate match = curDom.findPrototype(curPred);
					curPred.setArgSorts(match);
					/* WZ 5/6/02 update sort for every objectid variables */
					ListIterator liSortList =
						lowLevelSortList.getArguments().listIterator();
					while (liSortList.hasNext()) {
						OPredicate.pArg parg =
							(OPredicate.pArg) liSortList.next();
						curPred.setSortForVariable(parg.name, parg.sort);
					}

				}
			} catch (jplan.general.OCLSelectionException e) {
				Utility.debugPrintln("OCLSelectionException!  " + e);
				mssgs.add("The static predicate " + curPred.toString() + " has no prototype");
				ret = false;
			}
		}

		/* WZ 31/5/02 */
		//change name
		if (methodName != null) { /* WZ 21/6/02 */
			methodName = curDom.createMethodSignature(this);
			Utility.debugPrintln("\n>>methodName. " + methodName.toString());
		}
		if (ret) {
	//		mssgs.add(" Doing Method Transparency Check");
	//		ret = checkTransparency(curDom, mssgs);
		} else {
			mssgs.add("Check failed");
		}

		return ret;
	}

	
	// Ron 9/4/03
	/**
	 * reOrderDecomp 
	 * require the physical ordering of the decomposition to conform to a legal ordering
	 * as specified by the temporal constraints
	 * @return true if ordering can be achieved.
	 */	
	public boolean reOrderDecomp() {
		boolean ret = true;
		int order[] = new int[decomps.size()];
		int consts[][] = new int[temps.size()][2];
		ArrayList reordDecomps = new ArrayList();
		
		ListIterator li = temps.listIterator();
		int cons = 0;
		while (li.hasNext()) {
			oclPredicate cur = (oclPredicate) li.next();
			consts[cons][0] = Integer.parseInt(cur.getNthElementName(0));
			consts[cons][1] = Integer.parseInt(cur.getNthElementName(1));
			Utility.debugPrintln(consts[cons][0] + " > " + consts[cons][1]);
			cons++;
		}
		boolean visited[] = new boolean[decomps.size() + 1];
		for (int i = 0; i <= decomps.size(); i++) {
			visited[i] = false;
		}
		if (firstOrder(order, decomps.size(), consts, visited)) {
			for (int i = 0; i < decomps.size(); i++) {
				Utility.debugPrint(">" + order[i]);
				reordDecomps.add(decomps.get(order[i] - 1));
			}
			li = temps.listIterator();
			while (li.hasNext()) {
				oclPredicate cur = (oclPredicate) li.next();
				int low =  Integer.parseInt(cur.getNthElementName(0));
				int high =  Integer.parseInt(cur.getNthElementName(1));
				int nlow = 0;
				int nhigh = 0;
				for (int i = 0; i < decomps.size(); i++) {
					if (order[i] == low)
						nlow = i + 1;
					if (order[i] == high) 
						nhigh = i + 1;
				}
				try {
					cur.replaceVariableNo(0,new Integer(nlow).toString());
					cur.replaceVariableNo(1,new Integer(nhigh).toString());
				} catch (Exception e) {
					Utility.debugPrintln("reOrderDecomp: argument index out of range.");
					ret = false;
				}
			}
			Utility.debugPrintln("");
			
		} else {
			ret = false;
		}
		if (ret) {
			decomps = reordDecomps;
		}
		return ret;
	}

	/**
	 * checkTransparancy
	 * Check that this operator is transparent
	 * see "A tool-Supported Approach to Engineering HTN Planning Models"
	 * @param curDom - the working domain
	 * @param mssgs- the messages list
	 * @return - true if ok
	 */
	public boolean checkTransparency(oclDomain curDom, List mssgs) {
		boolean ret = true;
		int order[] = new int[decomps.size()];
		int consts[][] = new int[temps.size()][2];
	
		ListIterator li = temps.listIterator();
		int cons = 0;
		while (li.hasNext()) {
			oclPredicate cur = (oclPredicate) li.next();
			consts[cons][0] = Integer.parseInt(cur.getNthElementName(0));
			consts[cons][1] = Integer.parseInt(cur.getNthElementName(1));
			Utility.debugPrintln(consts[cons][0] + " > " + consts[cons][1]);
			cons++;
		}
		boolean visited[] = new boolean[decomps.size() + 1];
		for (int i = 0; i <= decomps.size(); i++) {
			visited[i] = false;
		}
		if (firstOrder(order, decomps.size(), consts, visited)) {
			for (int i = 0; i < decomps.size(); i++) {
				Utility.debugPrint(">" + order[i]);
			}
			Utility.debugPrintln("");
			if (!checkOrderTransparent(order, curDom, mssgs)) {
				return false;
			}
		} else {
			mssgs.add("Cycle in temporal dependancies");
			ret = false;
		}
		while (nextOrder(order, decomps.size(), consts, visited)) {
			Utility.debugPrintln("NEXT legal orderings");
			for (int i = 0; i < decomps.size(); i++) {
				Utility.debugPrintln(">" + order[i]);
			}
		}
		Utility.debugPrintln("NO More legal orderings");
		return ret;
	}

	/**
	 * firstOrder
	 * provide a legal ordering of the decompositions
	 * @param order - a new ordering is found to match the temp constraints
	 * @param noNodes - the number of decomposition nodes
	 * @param consts - the temporal constraints
	 * @param visited - the visited array - will be changed
	 * @return - true if allocation possible
	 */
	private boolean firstOrder(
		int order[],
		int noNodes,
		int consts[][],
		boolean visited[]) {
		for (int i = 0; i < noNodes; i++) {
			boolean ok = false;
			for (int j = 0; j < noNodes; j++) {
				if (!visited[j + 1] && allVisited(j + 1, visited, consts)) {
					order[i] = j + 1;
					visited[j + 1] = true;
					ok = true;
					break;
				}

			}
			if (!ok) {
				return false;
			}
		}
		return true;
	}

	/**
	 * nextOrder
	 * provide a legal ordering of the decompositions
	 * @param order - a new ordering is found to match the temp constraints
	 * @param noNodes - the number of decomposition nodes
	 * @param consts - the temporal constraints
	 * @param visited - the visited array - will be changed
	 * @return - true if allocation possible
	 */
	private boolean nextOrder(
		int order[],
		int noNodes,
		int consts[][],
		boolean visited[]) {
		boolean back = true;
		int row = noNodes - 2;
		while (row >= 0 && row < noNodes) {
			// Check if No other choice on this row
			if (order[row] == noNodes) {
				row--;
				back = true;
			} else {
				if (back) {
					// clear visited
					for (int j = row; j < noNodes; j++) {
						visited[order[j]] = false;
					}
					// clear order
					for (int i = row + 1; i < noNodes; i++) {
						order[i] = 0;
					}
				}
				// Allocate for this row
				int i = row;
				boolean foundForRow = false;
				for (int j = order[i]; j < noNodes; j++) {
					if (!visited[j + 1]
						&& allVisited(j + 1, visited, consts)) {
						order[i] = j + 1;
						visited[j + 1] = true;
						foundForRow = true;
						break;
					}
				}
				if (!foundForRow) {
					row--;
					back = true;
				} else {
					row++;
					back = false;
				}
			}
		}
		if (row == noNodes) {
			return true;
		} else {
			return false;
		}
	}

	/**
	 * allVisited
	 * check that the given node either has no predecessors or that they
	 * have all been visited
	 * @param given - the given node
	 * @param visited - the visited array
	 * @param consts - the temporal constraints
	 * @return - true if order found
	 */
	private boolean allVisited(int given, boolean visited[], int consts[][]) {
		int noConsts = consts.length;
		for (int i = 0; i < noConsts; i++) {
			if (consts[i][1] == given && !visited[consts[i][0]]) {
				return false;
			}
		}
		return true;
	}

	/**
	 * checkOrderTransparent
	 * check that this ordering is transparent
	 * @param order - order the given order of the decomposition
	 * @param curDom - the current working domain
	 * @param mssgs - the error messages list
	 * @return - true if all OK
	 */
	private boolean checkOrderTransparent(
		int order[],
		oclDomain curDom,
		List mssgs) {
		boolean ok = true;
		ListIterator li = index.listIterator();
		/*
		 * Possible problem - I dont at the moment see why the check for the decomposition is done
		 * afresh for each iteration where we consider a new index element?
		 * Is it that we only look at one object at a time?
		 */
		while (li.hasNext()) {
			oclSC indexSC = (oclSC) li.next();
			String indexSort = indexSC.getSort();
			List unifiers = curDom.getSortUnifiers(indexSort);
			unifiers.add(indexSort);
			AbstractState abState =
				new AbstractState(indexSort, indexSC.getName(), curDom);
			oclSS lhs = new oclSS(indexSort, indexSC.getName());
			lhs.setState(indexSC.getPre());
			// Just treat the LHS of the transition as an achieve clause and 
			// add it to the empty state
			abState.addAchieveToState(lhs);
			for (int i = 0; i < order.length; i++) {
				Object obj = (Object) decomps.get(order[i] - 1);
				if (obj.getClass().getName() == "jplan.ocl.oclSS") {
					//This is an achieve just add the prop list to the abState
					oclSS achieve = (oclSS) obj;
					if (Utility
						.listContainsString(achieve.getSort(), unifiers)) {
						abState.addAchieveToState(achieve);
					} // otherwise not relevant to this transition
				} else if (
					obj.getClass().getName() == "jplan.ocl.oclPredicate") {
					// is this an operator name or a method name
					oclPredicate pred = (oclPredicate)obj;
					Object objNamedByPred = curDom.checkObjectType(pred);	
					if (objNamedByPred.getClass().getName() == "jplan.ocl.oclMethod") {
						oclMethod om = (oclMethod) objNamedByPred;
						if (!abState.stepMethod(om)) {
							mssgs.addAll(abState.getMssgs());
							ok = false;
						}
					} else if (objNamedByPred.getClass().getName() == "jplan.ocl.oclOperator") {
						oclOperator op = (oclOperator) objNamedByPred;
						if (!abState.stepOperator(op)) {
							mssgs.addAll(abState.getMssgs());
							ok = false;
						}
					}
				}
			}
			// Now check that the RHS of the index has been satisfied
			// by the advanced state
			if (ok && !abState.checkIndexRHS(indexSC)) {
				mssgs.addAll(abState.getMssgs());
				ok = false;
			}
		}
		return ok;
	}

	/**
	 * countSucc
	 * count the number of nodes constrained to follow
	 * given node
	 * @param given - given node
	 * @param consts - the ordering constraints
	 * @param noConsts - the number of constraints
	 * @param soFar - the count so far
	 * @throws OCLException if cycle in dependencies
	 * @return - the count
	 */
	private int countSucc(int given, int consts[][], int noConsts, int soFar)
		throws OCLException {
		if (soFar > noConsts)
			throw new OCLException("Cycle in temporal dependancies");
		for (int i = 0; i < noConsts; i++) {
			if (consts[i][0] == given) {
				soFar = countSucc(consts[i][1], consts, noConsts, soFar + 1);
			}

		}
		return soFar;
	}

	/**
	  * countPred
	  * count the number of nodes constrained to follow
	  * given node
	  * @param given - given node
	  * @param consts - the ordering constraints
	  * @param noConsts- the number of constraints
	  * @param soFar - the count so far
	  * @throws OCLException if cycle in dependencies
	  * @return - the count
	  */
	private int countPred(int given, int consts[][], int noConsts, int soFar)
		throws OCLException {
		if (soFar > noConsts)
			throw new OCLException("Cycle in temporal dependancies");
		for (int i = 0; i < noConsts; i++) {
			if (consts[i][1] == given) {
				soFar = countPred(consts[i][0], consts, noConsts, soFar + 1);
			}

		}
		return soFar;
	}

	/**
	 * findSortForVariable
	 * find an identical variable name in the current predicate list
	 * and return its sort
	 * if not found return "not-found"
	 * @param varName the variable name
	 * @return String the variable Sort
	 */
	private String findSortForVariable(String varName) {
		//check index transition

		//pre - oclSC
		ListIterator liIndex = index.listIterator();
		while (liIndex.hasNext()) {
			oclSC curSC = (oclSC) liIndex.next();
			ListIterator li = curSC.getPre().listIterator();
			while (li.hasNext()) {
				oclPredicate cur = (oclPredicate) li.next();
				List args = cur.getArguments();
				ListIterator liArgs = args.listIterator();
				int argPos = 0;
				while (liArgs.hasNext()) {
					oclPredicate.pArg curArg =
						(oclPredicate.pArg) liArgs.next();
					if (varName.equals(curArg.name)) {
						try {
							String rtString = cur.getNthElementSort(argPos);
							return rtString;
						} catch (Exception e) {
						}
					}
					argPos++;
				}
			}
			//post - oclSC
			li = curSC.getPost().listIterator();
			while (li.hasNext()) {
				oclPredicate cur = (oclPredicate) li.next();
				List args = cur.getArguments();
				ListIterator liArgs = args.listIterator();
				int argPos = 0;
				while (liArgs.hasNext()) {
					oclPredicate.pArg curArg =
						(oclPredicate.pArg) liArgs.next();
					if (varName.equals(curArg.name)) {
						try {
							String rtString = cur.getNthElementSort(argPos);
							return rtString;
						} catch (Exception e) {
						}
					}
					argPos++;
				}
			}
		}

		//check statics
		//statics - oclPredicate
		ListIterator liStatics = statics.listIterator();
		while (liStatics.hasNext()) {
			oclPredicate curPred = (oclPredicate) liStatics.next();
			List args = curPred.getArguments();
			ListIterator liArgs = args.listIterator();
			int argPos = 0;
			while (liArgs.hasNext()) {
				oclPredicate.pArg curArg = (oclPredicate.pArg) liArgs.next();
				if (varName.equals(curArg.name)) {
					try {
						String rtString = curPred.getNthElementSort(argPos);
						return rtString;
					} catch (Exception e) {
					}
				}
				argPos++;
			}
		}

		return null;
	}

	/* Weihong  25/03/2002 */
	/**
	 * to instantiate the current oclMethod with given variables
	 * @param newPred oclPredicate with contains all variables
	 * 
	 */
	// Ron 10/11/02 Bug only rename the corresponding variable in the method name
	//              was replaceVariableName - which replaces all matching
	//              occurances
	public void instantiateWith(oclPredicate newPred) {
		ListIterator liNew = newPred.getArguments().listIterator();
		ListIterator liCur = methodName.getArguments().listIterator();
				Utility.debugPrintln("Method Instantiate with new >> " + newPred.toString());
		Utility.debugPrintln("Method Instantiate with old >> " + methodName.toString());
		int count = 0;
		while (liNew.hasNext()) {
			try {
				oclPredicate.pArg arg =
					(oclPredicate.pArg) ((oclPredicate.pArg) liNew.next())
						.clone();
				String argName =
					new String(((oclPredicate.pArg) liCur.next()).name);
				replaceVariableName(argName, new String(arg.name));
				try {
					methodName.replaceVariableNo(
						count,
						new String(arg.name));
				} catch (Exception e) {
					Utility.debugPrintln(" XXX Failed to find replacement for " + arg.name);
				}
			} catch (CloneNotSupportedException e) {
			}
			count++;
		}
	}

	/* Weihong  11/02/2002 */
	/**
	 * replace with a new variable value. If not in an element throw exception
	 * @param before the variable
	 * @param after new element name
	 */
	public void replaceVariableName(OPredicate.pArg before, String after) {
		//method name
		try {
			methodName.replaceVariableName(before, after);
		} catch (Exception e) {
			Utility.debugPrintln(
				"oclMethod",
				"failed to replace variable name in operator name");
		}
		ListIterator li;
		//oclSE
		li = precons.listIterator();
		while (li.hasNext()) {
			oclSE se = (oclSE) li.next();
			se.replaceVariableName(before, after);
		}
		//oclSC 
		li = index.listIterator();
		while (li.hasNext()) {
			oclSC sc = (oclSC) li.next();
			sc.replaceVariableName(before, after);
		}
		//statics - oclPredicates
		li = statics.listIterator();
		while (li.hasNext()) {
			oclPredicate opd = (oclPredicate) li.next();
			try {
				opd.replaceVariableName(before, after);
			} catch (Exception e) {
			}
		}
		//decomposition
		li = decomps.listIterator();
		while (li.hasNext()) {
			Object obj = (Object) li.next();
			if (obj.getClass().getName() == "jplan.ocl.oclSS") {
				oclSS ss = (oclSS) obj;
				ss.replaceVariableName(before, after);
			} else {
				oclPredicate opd = (oclPredicate) obj;
				try {
					opd.replaceVariableName(before, after);
				} catch (Exception e) {
				}
			}
		}
	}

	/* WZ 27/3/02 */
	/**
	 * replace with a new variable value. If not in an element throw exception
	 * @param before the variable
	 * @param after new element name
	 */
	public void replaceVariableName(String before, String after) {
		//method name
		try {
			methodName.replaceVariableNameByName(before, after);
		} catch (Exception e) {
			Utility.debugPrintln(
				"oclMethod",
				"failed to replace variable name in operator name");
		}
		ListIterator li;
		//oclSE
		li = precons.listIterator();
		while (li.hasNext()) {
			oclSE se = (oclSE) li.next();
			// 	    Utility.debugPrintln("replace SE .."+se.toString());
			se.replaceVariableName(before, after);
		}
		//oclSC 
		li = index.listIterator();
		while (li.hasNext()) {
			oclSC sc = (oclSC) li.next();
			// 	    Utility.debugPrintln("replace SC .."+sc.toString());
			sc.replaceVariableName(before, after);
		}
		//statics - oclPredicates
		li = statics.listIterator();
		while (li.hasNext()) {
			oclPredicate opd = (oclPredicate) li.next();
			try {
				opd.replaceVariableNameByName(before, after);
			} catch (Exception e) {
				// 		Utility.debugPrintln(e);
				continue;
			}
		}
		//decomposition
		li = decomps.listIterator();
		while (li.hasNext()) {
			Object obj = (Object) li.next();
			if (obj.getClass().getName() == "jplan.ocl.oclSS") {
				oclSS ss = (oclSS) obj;
				// 		Utility.debugPrintln("replace SS .."+ss.toString());
				ss.replaceVariableName(before, after);
			} else {
				oclPredicate opd = (oclPredicate) obj;
				try {
					// 		    Utility.debugPrintln("replace decomposition .."+opd.toString());
					opd.replaceVariableNameByName(before, after);
				} catch (Exception e) {
				}
			}
		}
	}

	/* WZ 5/4/02 */
	/**
	 * Monitor the vShape which relates to oclMethod in this canvas.
	 * @param vs vShape
	 * 
	 */
	public void monitorMethod(jplan.graphics.gTool.Graphics.vShape vs) {
		vs
			.textField
			.addChangeListener(new jplan.general.ExpressionPaneListener() {
			public void getChange(jplan.general.ExpressionPaneEvent evt) {
				if (evt.getID() == jplan.general.ExpressionPaneEvent.RENAME) {
					if (evt.getScope()
						== jplan.general.ExpressionPaneEvent.GLOBAL) {
						String vName = evt.getVName();
						String oldVName = evt.getOldVName();
						Utility.debugPrintln(
							"vName- " + vName + "; oldVName- " + oldVName);
						replaceVariableName(oldVName, vName);
					}
				}
			}
		});
	}

	/**
	 * checks to see if a name / sort pair occur in the given lists.
	 * @param name name
	 * @param sort sort
	 * @param ids List of names
	 * @param sorts List of sorts
	 * @return true if used
	 */
	private boolean alreadyUsed(
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

	/* Weihong  6/11/2001 */
	/**
	 * clone a copy of oclMethod
	 * @throws CloneNotSupportedException
	 * @return Object
	 */
	public Object clone() throws CloneNotSupportedException {
		try {
			oclMethod om = new oclMethod();
			om.methodName = (oclPredicate) methodName.clone();

			/* WZ 3/7/02 */
			ListIterator li = docm.listIterator();
			while (li.hasNext()) {
				String str = (String) li.next();
				om.addDocmLine(new String(str));
			}
			/* end 3/7/02 */

			li = index.listIterator();
			while (li.hasNext()) {
				oclSC sc = (oclSC) li.next();
				om.addIndexSC((oclSC) sc.clone());
			}

			li = statics.listIterator();
			while (li.hasNext()) {
				oclPredicate opr = (oclPredicate) li.next();
				om.addStatic((oclPredicate) opr.clone());
			}

			li = temps.listIterator();
			while (li.hasNext()) {
				oclPredicate opr = (oclPredicate) li.next();
				om.addTemps((oclPredicate) opr.clone());
			}

			li = decomps.listIterator();
			oclSS tss = new oclSS("TEMP", "TEMP");
			oclPredicate tprd = new oclPredicate("TEMP");
			while (li.hasNext()) {
				Object obj = (Object) li.next();
				if (obj.getClass().getName() == tprd.getClass().getName())
					om.addDecomps((oclPredicate) ((oclPredicate) obj).clone());
				if (obj.getClass().getName() == tss.getClass().getName())
					om.addDecomps((oclSS) ((oclSS) obj).clone());
			}

			return om;
		} catch (CloneNotSupportedException e) {
			throw e;
		}
	}

	/* Weihong  6/11/2001 */
	/**
	 * Returns a string representation of the oclOperator
	 * @return String
	 */
	public String toString() {
		return new String(methodName.toString());
	}

	/**
	 * to print the current oclMethod to a PrintWriter.
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
		ps.print("method(");
		if (methodName != null) /* WZ 18/6/02 */
			methodName.oclPrintComponent(ps, 0, false);
		ps.println(",\n    % pre-condition");
		ps.println("    [");
		ListIterator li = precons.listIterator();
		while (li.hasNext()) {
			((oclSE) li.next()).oclPrintComponent(ps, 5, false);
			if (li.hasNext())
				ps.println(",");
		}
		ps.println("],\n    % Index Transitions\n    [");
		li = index.listIterator();
		while (li.hasNext()) {
			((oclSC) li.next()).oclPrintComponent(ps, 5, false);
			if (li.hasNext())
				ps.println(",");
		}
		ps.println("],\n    % Static\n    [");
		li = statics.listIterator();
		while (li.hasNext()) {
			((oclPredicate) li.next()).oclPrintComponent(ps, 5, false);
			if (li.hasNext())
				ps.println(",");
		}
		ps.println("],\n    % Temporal Constraints\n    [");
		li = temps.listIterator();
		while (li.hasNext()) {
			((oclPredicate) li.next()).oclPrintComponent(ps, 5, false);
			if (li.hasNext())
				ps.println(",");
		}
		ps.println("],\n    % Decomposition\n    [");
		li = decomps.listIterator();
		Object temp;
		while (li.hasNext()) {
			temp = li.next();
			if (temp instanceof oclPredicate)
				 ((oclPredicate) temp).oclPrintComponent(ps, 5, false);
			else {
				ps.print("     achieve(");
				((oclSS) temp).oclPrintComponent(ps, 0, false);
				ps.print(")");
			}
			if (li.hasNext())
				ps.println(",");
		}
		ps.println("]");
		ps.println(").");
	}

	/* WZ 17/6/02 */
	/**
	 * a string expression, mainly used for saving purpose
	 * @return String
	 */
	public String to_String() {
		StringBuffer str = new StringBuffer();
		str.append("method::Description:");
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
		if (methodName != null) {
			str.append(methodName.toString());
			str.append("&");
			str.append(methodName.getSort().toString()); /* WZ 24/6/02 */
		}
		str.append(";");

		str.append("Pre-condition:");
		ListIterator li = precons.listIterator();
		while (li.hasNext()) {
			str.append(((oclSE) li.next()).to_String());
			if (li.hasNext())
				str.append("&");
		}
		str.append(";");

		str.append("Index Transitions:");
		li = index.listIterator();
		while (li.hasNext()) {
			str.append(((oclSC) li.next()).to_String());
			if (li.hasNext())
				str.append("&");
		}
		str.append(";");

		str.append("Static:");
		li = statics.listIterator();
		while (li.hasNext()) {
			str.append(((oclPredicate) li.next()).toString());
			if (li.hasNext())
				str.append("&");
		}
		str.append(";");

		str.append("Temporal Constraints:");
		li = temps.listIterator();
		while (li.hasNext()) {
			str.append(((oclPredicate) li.next()).toString());
			if (li.hasNext())
				str.append("&");
		}
		str.append(";");

		str.append("Decomposition:");
		li = decomps.listIterator();
		Object temp;
		while (li.hasNext()) {
			temp = li.next();
			if (temp instanceof oclPredicate)
				str.append(((oclPredicate) temp).toString());
			else {
				str.append(((oclSS) temp).to_String());
			}
			if (li.hasNext())
				str.append("&");
		}
		str.append(";\n");

		return str.toString();
	}

	/**
	 * AbstractState
	 * the state representation of an individual abstract/non concrete
	 * object.
	 */
	private class AbstractState {
		private oclDomain curDom = null; // The current domain model
		private List state = null;
		// factor the predicates describing the state into
		// the different sort levels of the hierarchy
		private List constraints = null;
		//Any statics ne clauses etc referenced in state
		private String sort = "none"; //The sort for the state
		private String Id = "none"; // The object ID
		private Hashtable predIndex = null;
		//Index of state definitions containing predicates
		private List unifiers = null;
		// The sorts that unify with the main index sort
		// Note this is built incrementally
		private List mssgs = null; //Store error messages

		/**
		 * AbstractState
		 * @param sort - the sort for the state
		 * @param Id - the object Id - probably a variable
		 * @param curDom - the current domain model
		 */
		public AbstractState(String sort, String Id, oclDomain curDom) {
			this.sort = sort;
			this.Id = Id;
			this.curDom = curDom;
			state = new ArrayList();
			constraints = new ArrayList();
			predIndex = new Hashtable();
			mssgs = new ArrayList();
			// now collect some sort details
			unifiers = curDom.getSortUnifiers(sort);
			unifiers.add(sort);
			ListIterator li = unifiers.listIterator();
			while (li.hasNext()) {
				String curSort = (String) li.next();
				oclSSClassDef curDef = null;
				try {
					curDef = curDom.getStateListForSort(curSort);
					try {
						curDef = (oclSSClassDef) curDef.clone();
					} catch (Exception e) {
						System.err.println(
							"Unexpected failure to clone ocl class definition");
					}
				} catch (OCLNoSuchElementException e) {
					//Ignore no def at this level
				}
				if (curDef != null) {
					curDef.replaceID(Id);
					List stateLists = curDef.getStateList();
					ListIterator liStates = stateLists.listIterator();
					while (liStates.hasNext()) {
						oclStateList curState = (oclStateList) liStates.next();
						List statePreds = (List) curState.getPredicateList();
						ListIterator liPreds = statePreds.listIterator();
						while (liPreds.hasNext()) {
							oclPredicate curPred = (oclPredicate) liPreds.next();
							if (curPred.isStatic()
								|| curPred.getName().equals("ne")
								|| curPred.getName().equals("is_of_sort")) {
								constraints.add(curPred);
							} else if (predIndex.containsKey(curPred.getName())) {
								// Got this one do nothing
							} else {
								predIndex.put(curPred.getName(), curDef);
							}
						}
					}
				}
			}
		}

		/**
		 * collectDetails
		 * collect details of the sort hierarchy i.e. build
		 * predicate index into the state hierarchy
		 * @param hSort - the new sort
		 */
		private void collectDetails(String hSort) {
			List newUnifiers = curDom.getSortUnifiers(hSort);
			newUnifiers.add(hSort);
			ListIterator li = newUnifiers.listIterator();
			while (li.hasNext()) {
				String curSort = (String) li.next();
				if (Utility.listContainsString(curSort, unifiers)) {
					// got these details
					break;
				}
				unifiers.add(curSort);
				oclSSClassDef curDef = null;
				try {
					curDef = curDom.getStateListForSort(curSort);
					try {
						curDef = (oclSSClassDef) curDef.clone();
					} catch (Exception e) {
						System.err.println(
							"Unexpected failure to clone ocl class definition");
					}
				} catch (OCLNoSuchElementException e) {
					//Ignore no def at this level
					break;
				}
				curDef.replaceID(Id);
				List stateLists = curDef.getStateList();
				ListIterator liStates = stateLists.listIterator();
				while (liStates.hasNext()) {
					oclStateList curState = (oclStateList) liStates.next();
					List statePreds = (List) curState.getPredicateList();
					ListIterator liPreds = statePreds.listIterator();
					while (liPreds.hasNext()) {
						oclPredicate curPred = (oclPredicate) liPreds.next();
						if (curPred.isStatic()
							|| curPred.getName().equals("ne")
							|| curPred.getName().equals("is_of_sort")) {
							constraints.add(curPred);
						} else if (predIndex.containsKey(curPred.getName())) {
							// Got this one do nothing
						} else {
							predIndex.put(curPred.getName(), curDef);
						}
					}
				}
			}
		}

		/**
		 * addAchieveToState
		 * add detail i.e. a new level to the state description
		 * check that this does not conflict with anything already in the state list
		 * @param achieve - the achieve clause
		 * @return - true if all OK
		 */
		public boolean addAchieveToState(oclSS achieve) {
			boolean ret = true;
			List stateList = achieve.getState();
			String achieveSort = achieve.getSort();
			if (!Utility.listContainsString(achieveSort, unifiers)) {
				// need to further collect the hierarchy details
				collectDetails(achieveSort);
			}
			ListIterator li = stateList.listIterator();
			while (li.hasNext()) {
				oclPredicate curPred = (oclPredicate) li.next();
				if (curPred.isStatic()
					|| curPred.getName().equals("ne")
					|| curPred.getName().equals("is_of_sort")) {
					// Just ignore for the moment
				} else {
					oclSSClassDef def =
						(oclSSClassDef) predIndex.get(curPred.getName());
					// do I have an oclSS in the state list for this level
					oclSS curLevel = null;
					if ((curLevel = stateForLevel(def.getStateSort()))
						== null) {
						oclSS stateLevel = new oclSS(def.getStateSort(), Id);
						stateLevel.addPredicate(curPred);
						state.add(stateLevel);
					} else {
						// I have a state - is it consistant to add this predicate?
						// Lets just add it and check that the state is legal afterwards
						curLevel.addPredicate(curPred);
					}

				}
			}
			// Now need to check the resulting state
			ret = checkState();
			if (!ret) {
				Utility.debugPrintln("Illegal state produced by adding achieve");
				ListIterator liMssgs = mssgs.listIterator();
				while (liMssgs.hasNext()) {
					String mssg = (String)liMssgs.next();
					Utility.debugPrintln(mssg);
				}
			}
			return ret;
		}
		
		/**
		 * stepOperator
		 * advance the state by applying the primitive operator 
		 * and check that resulting state is legal
		 * check that the LHS is supported in the current state
		 * @param op - the operator to step
		 * @return - true if all ok
		 */
		public boolean stepOperator(oclOperator op){
			// Still need to deal with prevails
			mssgs.add("STEP OPERATOR " + op.opName.toString());
			ListIterator li = op.getPrevail().listIterator();
			while (li.hasNext()) {
				oclSE prev = (oclSE)li.next();
				if (prev.name.equals(Id)) {
					if (!checkPrevail(prev)) {
						return false;
					}
				}
			}
			li = op.getNecessary().listIterator();
			while (li.hasNext()) {
				oclSC trans = (oclSC)li.next();
				if (trans.name.equals(Id)) {
					if (!checkTransitionLHS(trans)) {
						return false;
					}
				}
			}
			// If we have got here the LHS is ok so apply the RHS(s)
			li = op.getNecessary().listIterator();
			while (li.hasNext()) {
				oclSC trans = (oclSC)li.next();
				if (trans.name.equals(Id)) {
					applyRHS(trans);
				}
			}
			// Now deal with conditional effects
			// The Ids of these will not have been renamed
			li = op.getConditional().listIterator();
			while (li.hasNext()) {
				oclSC trans = (oclSC)li.next();
				if (Utility.listContainsString(trans.getSort(),unifiers)) {
					// rename ID
					oclSC temp = null;
					try {
						temp = (oclSC)trans.clone();
					} catch (CloneNotSupportedException e){}
					temp.replaceVariableName(trans.name,Id);
					if (checkTransitionLHS(temp)) {
						applyRHS(temp);
					}
				}
			}
			return true;
		}
		
		/**
		 * stepMethod
		 * advance the state by applying the primitive method 
		 * and check that resulting state is legal
		 * check that the LHS is supported in the current state
		 * @param meth - the method to step
		 * @return - true if all ok
		 */
		public boolean stepMethod(oclMethod meth){
			// Still need to deal statics
			mssgs.add("STEP METHOD " + meth.getName().toString());
			ListIterator li = meth.getIndex().listIterator();
			while (li.hasNext()) {
				oclSC trans = (oclSC)li.next();
				if (trans.name.equals(Id)) {
					if (!checkTransitionLHS(trans)) {
						return false;
					}
				}
			}
			// If we have got here the LHS is ok so apply the RHS(s)
			li = meth.getIndex().listIterator();
			while (li.hasNext()) {
				oclSC trans = (oclSC)li.next();
				if (trans.name.equals(Id)) {
					applyRHS(trans);
				}
			}
			return true;
		}
		
		/**
		 * checkPrevail
		 * check that the prevail is supported in the current state
		 * @param prev - the se clause for the object
		 * @return - true if all ok
		 */
		public boolean checkPrevail(oclSE prev){
			oclSC  trans = new oclSC(prev.getSort(),prev.name);
			trans.setPre(prev.getState());
			return checkTransitionLHS(trans);
		}
		
		/**
		 * checkTransitionLHS
		 * check that the LHS of the transition is supported in the current state
		 * @param trans - the sc clause for the object
		 * @return - true if all ok
		 */
		public boolean checkTransitionLHS(oclSC trans){
			boolean ret = true;
			List stateList = trans.getPre();
			String transSort = trans.getSort();
			if (!Utility.listContainsString(transSort, unifiers)) {
				// better collect the details
				collectDetails(transSort);
//				// must already have some information about this level??
//				mssgs.add("Object " + trans.getName() + "of sort " + transSort + " has no corresponding state");
//				return false;
			}
			List pre = trans.getPre();
			ListIterator li = pre.listIterator();
			while (li.hasNext()) {
				oclPredicate curPred = (oclPredicate) li.next();
				if (curPred.isStatic()
					|| curPred.getName().equals("ne")
					|| curPred.getName().equals("is_of_sort")) {
					// Just ignore for the moment
				} else {
					oclSSClassDef def =
						(oclSSClassDef) predIndex.get(curPred.getName());
					if (def == null) {
						mssgs.add("Cannot find state definition for the predicate " + curPred.getName());
						return false;
					}
					// do I have an oclSS in the state list for this level
					oclSS curLevel = null;
					if ((curLevel = stateForLevel(def.getStateSort()))== null) {
						// No current state LHS firs reference at this level just add it
						mssgs.add(" The predicate " + curPred.toString() + " refers to a new level in this state");
						oclSS stateLevel = new oclSS(def.getStateSort(), Id);
						stateLevel.addPredicate(curPred);
						state.add(stateLevel);
					} else {
						// I have a state - is the predicate asserted in this state
						if (! Utility.memberPredEqual(curPred,curLevel.getState())) {
							mssgs.add(" The predicate " + curPred.toString() + " is not asserted in the current state");
							return false;
						}
					}

				}
			}
			return true;
		}
		
		/**
		 * applyRHS
		 * change the appropriate level of the state to match the RHS of
		 * the transition
		 * @param trans - the transition
		 */
		private void applyRHS(oclSC trans) {
			List post = trans.getPost();
			ListIterator li = post.listIterator();
			// On first iteration get rid of existing states for corresponding levels
			while (li.hasNext()) {
				oclPredicate curPred = (oclPredicate) li.next();
				if (curPred.isStatic()
					|| curPred.getName().equals("ne")
					|| curPred.getName().equals("is_of_sort")) {
					// Just ignore for the moment
				} else {
					oclSSClassDef def =
						(oclSSClassDef) predIndex.get(curPred.getName());
					if (def == null) {
						mssgs.add("Cannot find state definition for the predicate " + curPred.getName());
						// Mmmm should not happen
						return;
					}
					// do I have an oclSS in the state list for this level
					oclSS curLevel = null;
					if ((curLevel = stateForLevel(def.getStateSort()))!= null) {
						// I have a state remove
						curLevel.setState(new ArrayList());
					}

				}
			}
			li = post.listIterator();
			// On secon iteration add state predicates for corresponding levels
			while (li.hasNext()) {
				oclPredicate curPred = (oclPredicate) li.next();
				if (curPred.isStatic()
					|| curPred.getName().equals("ne")
					|| curPred.getName().equals("is_of_sort")) {
					// Just ignore for the moment
				} else {
					oclSSClassDef def =
						(oclSSClassDef) predIndex.get(curPred.getName());
					if (def == null) {
						mssgs.add("Cannot find state definition for the predicate " + curPred.getName());
						// Mmmm should not happen
						return;
					}
					// do I have an oclSS in the state list for this level
					oclSS curLevel = null;
					if ((curLevel = stateForLevel(def.getStateSort()))!= null) {
						// I have a state remove
						curLevel.addPredicate(curPred);
					}

				}
			}
		}
		
		/**
		 * checkIndexRHS
		 * check that the RHS of the given index has been satisfied
		 * in the advancing state
		 * @param index - the index transition
		 * @return - true if all ok
		 */
		public boolean checkIndexRHS(oclSC index) {
			boolean ret = true;
			List stateList = index.getPost();
			String indexSort = index.getSort();
			if (!Utility.listContainsString(indexSort, unifiers)) {
				// must already have some information about this level??
				mssgs.add("Index Object " + index.getName() + "of sort " + indexSort + " has no corresponding state");
				return false;
			}
			List pre = index.getPost();
			ListIterator li = pre.listIterator();
			while (li.hasNext()) {
				oclPredicate curPred = (oclPredicate) li.next();
				if (curPred.isStatic()
					|| curPred.getName().equals("ne")
					|| curPred.getName().equals("is_of_sort")) {
					// Just ignore for the moment
				} else {
					oclSSClassDef def =
						(oclSSClassDef) predIndex.get(curPred.getName());
					if (def == null) {
						mssgs.add("Cannot find state definition for the predicate " + curPred.getName());
						return false;
					}
					// do I have an oclSS in the state list for this level
					oclSS curLevel = null;
					if ((curLevel = stateForLevel(def.getStateSort()))== null) {
						// No current state LHS not supported
						mssgs.add(" The predicate on the RHS of the index" + curPred.toString() + " is not asserted in the current state");
						return false;
			
					} else {
						// I have a state - is the predicate asserted in this state
						if (! Utility.memberPred(curPred,curLevel.getState())) {
							mssgs.add(" The predicate on the RHS of the Index " + curPred.toString() + " is not asserted in the current state");
							return false;
						}
					}

				}
			}
			return ret;
		}

		/**
		 * checkState
		 * check that every level in the state description is a legal substate expression
		 * i.e. that it matches exactly one substate class definition for that sort level
		 * @return true if all ok
		 */
		private boolean checkState() {
			boolean ok = true;
			ListIterator li = state.listIterator();
			while (ok && li.hasNext()) {
				oclSS curLevel = (oclSS) li.next();
				ok = curLevel.checkAchieveSS(curDom,mssgs);
			}
			return ok;
		}

		/**
		 * stateForLevel
		 * find the current state for the level of the given sort
		 * @param sort - the given sort level
		 * @return the oclSS state description
		 */
		public oclSS stateForLevel(String sort) {
			ListIterator li = state.listIterator();
			while (li.hasNext()) {
				oclSS curLevel = (oclSS) li.next();
				if (curLevel.getSort().equals(sort)) {
					return curLevel;
				}
			}
			return null;
		}

		/**
		 * Returns the mssgs.
		 * @return List
		 */
		public List getMssgs() {
			return mssgs;
		}

	}

}
