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

// oclGoal.java Top level class to store ocl Task
package jplan.ocl;
/**
 * oclHTNTask.java
 *
 *
 * Created: Thu Apr 11 2002
 *
 * @author R Simpson, W Zhao
 * @version
 */

import java.util.List;
import java.util.ArrayList;
import java.util.ListIterator;
import java.io.*;
import java.awt.print.*;

import jplan.general.OEnviron;
import jplan.general.Utility;

/** 
 * structure for storing domain problems.
 */
public class oclHTNTask
	extends oclTask
	implements Cloneable, oclPrint, Serializable {

	public List constraints = new ArrayList();
	public List statics = new ArrayList();

	public oclHTNTask(String Id) {
		super(Id);
	}

	/**
	 * addAchieveSS
	 * add a new achieve clause to the goal-list
	 * @param sort - the ss sort
	 * @param Id - the ss ID
	 * @return - new ss clause
	 */
	public oclSS addAchieveSS(String sort, String Id) {
		oclSS cur = new oclSS(sort, Id);
		goals.add(cur);
		return cur;
	}

	/**
	 * addGoalClause
	 * - add a new predicate to the goal list
	 * @param name - the predicate name
	 * @return - the predicate
	 */
	public oclPredicate addGoalClause(String name) {
		oclPredicate cur = new oclPredicate(name);
		goals.add(cur);
		return cur;
	}

	/**
	 * addTempClause
	 * - add a new before clause to the temporal constraints list
	 * @param name - the predicate name
	 * @return - the predicate
	 */
	public oclPredicate addTempClause(String name) {
		oclPredicate cur = new oclPredicate(name);
		constraints.add(cur);
		return cur;
	}

	/**
	 * addStaticClause
	 * - add a new static clause to the static constraints list
	 * @param name  the predicate name
	 * @return - the predicate
	 */
	public oclPredicate addStaticClause(String name) {
		oclPredicate cur = new oclPredicate(name);
		statics.add(cur);
		return cur;
	}

	/**
	 * Gets the static condition list.
	 * @return a list oclPredicatess.
	 */
	public List getStatics() {
		return statics;
	}

	/**
	 * Add the static conditions.
	 * @param cur oclPredicate: the static conditions
	 * 
	 */
	public void addStatic(oclPredicate cur) {
		statics.add(cur);
	}

	/* WZ 7/5/02 */
	/**
	 * Sets the statics.
	 * @param staticList a list of predicates with information
	 * of statics.
	 * 
	 */
	public void setStatic(List staticList) {
		statics = staticList;
	}

	/* WZ 7/5/02 */
	/**
	 * To translate the information contanining in
	 * an oclMethod to the current oclHTNTask.
	 * @param md an oclMethod
	 * 
	 */
	public void translate(oclMethod md) {
		setGoalLists(md.getDecomps());
		setTemps(md.getTemps());
		setStatic(md.getStatics());
	}

	/**
	 * Add temporal clauses.
	 * @param before the position of the goalList action.
	 * @param after the position of the goalList action.
	 * 
	 */
	public void addTempClauseBefore(String before, String after) {
		oclPredicate cur = new oclPredicate("before");
		cur.addVarArgument(before);
		cur.addVarArgument(after);
		constraints.add(cur);
	}

	/**
	 * Add the temporal constaints.
	 * @param cur oclPredicate: the temporal constaints
	 * 
	 */
	public void addTemps(oclPredicate cur) {
		constraints.add(cur);
	}

	/**
	 * Gets the temporal constaints.
	 * @return a list of predicates with information of temporal constraints.
	 */
	public List getTemps() {
		return constraints;
	}

	/**
	 * Sets the temporal constaints.
	 * @param tempsList a list of predicates with information
	 * of temporal constraints.
	 * 
	 */
	public void setTemps(List tempsList) {
		constraints = tempsList;
	}

	/**
	 * Add goalListosed oclSS.
	 * @param kind sort
	 * @param ID name
	 * @return the added oclSS.
	 */
	public oclSS addGoalListSS(String kind, String ID) {
		oclSS cur = new oclSS(kind, ID);
		goals.add(cur);
		return cur;
	}

	/**
	 * Add oclSS to achieve.
	 * @param cur the fact (conditions) for the following
	 * operators in the goalList
	 * 
	 */
	public void addGoalLists(oclSS cur) {
		goals.add(cur);
	}

	/**
	 * Gets the getGoalLists
	 * @return a list of oclSS/oclMethod/oclOperator
	 */
	public List getGoalLists() {
		return goals;
	}

	/**
	 * Sets the goalList
	 * @param listGoal a list of oclSS/oclMethod/oclOperator
	 */
	public void setGoalLists(List listGoal) {
		goals = listGoal;
	}

	/**
	 * Add the operator/method's name.
	 * @param cur oclPredicate: the operator/method's name
	 * 
	 */
	public void addGoalLists(oclPredicate cur) {
		goals.add(cur);
	}

	// Ron 8/11/02 - this is just a start only checking init state
	// Ron 29/4/03 - Added check to goal achieve states
	// Ron 30/5/03 - Added check that object states are only defined once in ititial state
	/**
	 * do verification checks.<br>
	 * check that the initial states are fully instantiated and
	 * unify with a state definition for the object;
	 * check that goal states are valid sub-state expressions and
	 * that for each goal state there is an initial state for that
	 * object.
	 * NOTE should check that all referenced dynamic objects have an 
	 * initial state definition
	 * @param cur the current domain
	 * @param mssgs the message list
	 */
	public void check(oclDomain cur, List mssgs) {
		List objs = new ArrayList();
		ListIterator liInits = inits.listIterator();
		while (liInits.hasNext()) {
			oclSS curSS = (oclSS) liInits.next();
			if (!curSS.isFullyInstantiated()) {
				mssgs.add(
					"The initial state for "
						+ curSS.getName()
						+ " is not fully instantiated.");
			}
			curSS.check(cur, mssgs);
			if (Utility.listContainsString(curSS.getName(),objs)) {
				mssgs.add(
					"There are duplicate initial states for the object "
					+ curSS.getName());
			} else {
				objs.add(curSS.getName());
			}
		}
		if (goals.size() == 0) {
			mssgs.add("There are no goals specified.");
		} else {
			ListIterator li = goals.listIterator();
			while (li.hasNext()) {
				Object goal = (Object)li.next();
				if (goal.getClass().getName().equals("jplan.ocl.oclSS")){
					((oclSS)goal).checkAchieveSS(cur,mssgs);
				} else if (goal.getClass().getName().equals("jplan.ocl.oclPredicate")) {
					// This must be a method call
					oclPredicate gMethod = (oclPredicate)goal;
					boolean found = false;
					oclPredicate name = null;
					ListIterator liMethods = cur.methods.listIterator();
					while (!found && liMethods.hasNext()) {
						name  = ((oclMethod)liMethods.next()).getName();
						if (name.getName().equals(gMethod.getName())) {
							found = true;
							if (!gMethod.unify(name,new OEnviron())) {
								mssgs.add("The method name " + name.toString() +
									" does not match the defined method with this name.");
									
							}
						}
					}
					if (! found) {
						mssgs.add("The method name " + name.toString() +
							" dous not match any defined method.");
					}
				}
			}
		}
		
	}

	/* WZ 17/4/02 */
	/**
	 * clone a copy of oclHTNTask
	 * @throws CloneNotSupportedException
	 * @return Object
	 */
	public Object clone() throws CloneNotSupportedException {
		try {
			oclHTNTask copy = new oclHTNTask(new String(this.ID));

			ListIterator li = inits.listIterator();
			while (li.hasNext()) {
				copy.addInitSS((oclSS) li.next());
			}

			li = statics.listIterator();
			while (li.hasNext()) {
				oclPredicate opr = (oclPredicate) li.next();
				copy.addStatic((oclPredicate) opr.clone());
			}

			li = constraints.listIterator();
			while (li.hasNext()) {
				oclPredicate opr = (oclPredicate) li.next();
				copy.addTemps((oclPredicate) opr.clone());
			}

			li = goals.listIterator();
			while (li.hasNext()) {
				Object obj = (Object) li.next();
				if (obj.getClass().getName() == "jplan.ocl.oclPredicate")
					copy.addGoalLists(
						(oclPredicate) ((oclPredicate) obj).clone());
				if (obj.getClass().getName() == "jplan.ocl.oclSS")
					copy.addGoalLists((oclSS) ((oclSS) obj).clone());
				
			}

			return copy;
		} catch (CloneNotSupportedException e) {
			throw e;
		}
	}

	/* WZ 30/4/02 */
	/**
	 * Returns a string representation of the oclTask
	 * @return String
	 */
	public String toString() {
		return "oclHTNTask" + ID;
	}

	//     /**
	//      * Returns a string representation of the oclOperator
	//      * @return String
	//      */
	//     public String toString(){
	// 	StringBuffer sb = new StringBuffer();
	// 	sb.append("goal([");
	// 	ListIterator li = statics.listIterator();
	// 	while(li.hasNext()) {
	// 	    sb.append(((oclPredicate)li.next()).toString());
	// 	    if (li.hasNext())
	// 		sb.append(",");
	// 	}

	// 	li = constraints.listIterator();
	// 	while(li.hasNext()) {
	// 	    sb.append(((oclPredicate)li.next()).toString());
	// 	    if (li.hasNext())
	// 		sb.append(",");
	// 	}

	// 	li = goals.listIterator();
	// 	Object temp;
	// 	while(li.hasNext()) {
	// 	    temp = li.next();
	// 	    if (temp instanceof oclPredicate)
	// 		sb.append(((oclPredicate)temp).toString());
	// 	    else {
	// 		sb.append("     achieve(");
	// 		sb.append(((oclSS)temp).toString());
	// 		sb.append(")");
	// 	    }
	// 	    if (li.hasNext())
	// 		sb.append(",");
	// 	}
	// 	sb.append("]");
	// 	sb.append(").");
	// 	return sb.toString();
	//     }    

	/**
	 * to print the current oclTask to a PrintWriter.
	 * @param ps PrintWriter
	 * @param indent value of indentation
	 * @param nline not being used really
	 * 
	 */
	public void oclPrintComponent(PrintWriter ps, int indent, boolean nline) {
		ps.println("htn_task(" + ID + ",");
		ps.println("    goal(");
		ps.println("          [");
		ListIterator li = goals.listIterator();
		while (li.hasNext()) {
			Object goalClause = li.next();
			oclPredicate goalPred = new oclPredicate("dummy");
			if (goalPred.getClass().isInstance(goalClause)) {
				((oclPredicate) goalClause).oclPrintComponent(ps, 12, false);
			} else {
				// Must be an achieve clause
				ps.print("            achieve(");
				((oclSS) goalClause).oclPrintComponent(ps, 0, false);
				ps.print(")");
			}
			if (li.hasNext())
				ps.println(",");
		}
		ps.println("],\n    % Temporal Constraints");
		ps.println("          [");
		li = constraints.listIterator();
		while (li.hasNext()) {
			oclPredicate cur = (oclPredicate) li.next();
			cur.oclPrintComponent(ps, 12, true);
			if (li.hasNext())
				ps.println(",");
		}
		ps.println("],\n    % Static constraints");
		ps.println("          [");
		li = statics.listIterator();
		while (li.hasNext()) {
			oclPredicate cur = (oclPredicate) li.next();
			cur.oclPrintComponent(ps, 12, true);
			if (li.hasNext())
				ps.println(",");
		}

		ps.println("]),\n    % INIT States");
		ps.println("    [");
		li = inits.listIterator();
		while (li.hasNext()) {
			((oclSS) li.next()).oclPrintComponent(ps, 5, false);
			if (li.hasNext())
				ps.println(",");
		}
		ps.println("]).");
	}
}
