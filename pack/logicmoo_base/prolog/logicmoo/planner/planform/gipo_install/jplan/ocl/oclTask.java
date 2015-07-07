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

import java.util.List;
import java.util.ArrayList;
import java.util.ListIterator;
import java.io.*;
import java.awt.Color; /* Weihong added on 28/08/2001 */
import java.awt.print.*; /* Weihong added on 28/08/2001 */
import java.awt.Graphics; /* Weihong added on 28/08/2001 */
import jplan.general.Utility;

/** 
 * structure for storing domain problems.
 */
public class oclTask
	implements
		Cloneable,
		oclPrint,
		Serializable,
		Printable { /* Weihong changed on 28/08/2001 */
	/**
	 * name of the task.
	 */
	public String ID;
	/**
	 * goal states.
	 */
	public List goals = new ArrayList();
	/**
	 * initial states.
	 */
	public List inits = new ArrayList();

	/**
	 * Creates an instance of oclTask.
	 * @param name name
	 */
	public oclTask(String name) {
		ID = new String(name);
	}

	/**
	 * Add oclSE to the goal state
	 * @param sort sort
	 * @param name name
	 * @return oclSE
	 */
	public oclSE addGoalSE(String sort, String name) {
		oclSE cur = new oclSE(sort, name);
		goals.add(cur);
		return cur;
	}

	/**
	 * Add oclSS to the initial state
	 * @param sort sort
	 * @param name name
	 * @return oclSE
	 */
	public oclSS addInitSS(String sort, String name) {
		oclSS cur = new oclSS(sort, name);
		inits.add(cur);
		return cur;
	}

	/* Weihong added on 29/06/2001 */
	/**
	 * clone a copy of oclTask
	 * @throws CloneNotSupportedException
	 * @return Object
	 */
	public Object clone() throws CloneNotSupportedException {
		oclTask copy = new oclTask(ID);
		ListIterator li = inits.listIterator();
		while (li.hasNext()) {
			copy.addInitSS((oclSS) li.next());
		}

		li = goals.listIterator();
		while (li.hasNext()) {
			copy.addGoalSE((oclSE) li.next());
		}
		return copy;
	}

	/* Weihong added on 28/06/2001 */
	/**
	 * Add oclSS to the initial state
	 * @param ss oclSS
	 * 
	 */
	public void addInitSS(oclSS ss) {
		inits.add(ss);
	}

	/* Weihong added on 28/06/2001 */
	/**
	 * Add oclSE to the goal state
	 * @param se oclSE
	 * 
	 */
	public void addGoalSE(oclSE se) {
		goals.add(se);
	}

	/* WZ 4/4/02 */
	/**
	 * Add oclPredicate to the goal state
	 * @param opd oclPredicate
	 * 
	 */
	public void addGoalPredicate(oclPredicate opd) {
		goals.add(opd);
	}

	/* WZ 4/4/02 */
	/**
	 * Add oclss to the goal state
	 * @param ss oclss
	 * 
	 */
	public void addGoalSS(oclSS ss) {
		goals.add(ss);
	}

	/* WZ 4/4/02 */
	/**
	 * set a list to the goal list
	 * @param goalList List
	 * 
	 */
	public void setGoal(List goalList) {
		goals = goalList;
	}

	/* Weihong added on 23/05/2001 */
	/**
	 * Get the initial state
	 * @return the list of oclSS in the initial states
	 */
	public List getInits() {
		return inits;
	}

	// Ron  19/3/02 used by opmaker
	/**
	 * objectUsedInInits
	 * check to see if object of given sort is given an initial state
	 * @param sort - sort the object sort
	 * @param objID - the object ID
	 * @return - true if found
	 */
	public boolean objectUsedInInits(String sort, String objID) {
		ListIterator li = inits.listIterator();
		boolean found = false;
		while (!found && li.hasNext()) {
			oclSS cur = (oclSS) li.next();
			if (sort.equals(cur.getSort()) && objID.equals(cur.getName())) {
				found = true;
			}
		}
		return found;
	}

	/* Weihong added on 23/05/2001 */
	/**
	 * Get the goal state
	 * @return the list of oclSE in the goal states
	 */
	public List getGoals() {
		return goals;
	}

	// Ron 30.5/03 added check that initial state objects are only defined once	
	//             and same for goal states
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
		List goalObjs = new ArrayList();
		ListIterator liGoals = goals.listIterator();
		while (liGoals.hasNext()) {
			oclSE curSE = (oclSE) liGoals.next();
			// Should I be doing this
			if (!curSE.isFullyInstantiated()) {
				mssgs.add(
					"The goal state for "
						+ curSE.getName()
						+ " is not fully instantiated.");
			}
			curSE.check(cur, mssgs);
			if (Utility.listContainsString(curSE.getName(),goalObjs)) {
				mssgs.add(
					"There are duplicate goal states for the object "
					+ curSE.getName());
			} else {
				goalObjs.add(curSE.getName());
			}
			if (!Utility.listContainsString(curSE.getName(),objs)) {
				mssgs.add( "The object "
					+ curSE.getName()
					+ " has a goal state defined "
					+ "but it has no initial state.");		
			}
		}
	}

	/**
	 * Returns a string representation of the oclTask
	 * @return String
	 */
	public String toString() {
		return "oclTask_" + ID; /* Weihong changed on 23/05/2001 */
	}

	/**
	 * to print the current oclTask to a PrintWriter.
	 * @param ps PrintWriter
	 * @param indent value of indentation
	 * @param nline not being used really
	 * 
	 */
	public void oclPrintComponent(PrintWriter ps, int indent, boolean nline) {
		ps.println("planner_task(" + ID + ",");
		ps.println("    % Goals");
		ps.println("    [");
		ListIterator li = goals.listIterator();
		while (li.hasNext()) {
			((oclSE) li.next()).oclPrintComponent(ps, 5, false);
			if (li.hasNext())
				ps.println(",");
		}
		ps.println("],\n    % INIT States");
		ps.println("    [");
		li = inits.listIterator();
		while (li.hasNext()) {
			((oclSS) li.next()).oclPrintComponent(ps, 5, false);
			if (li.hasNext())
				ps.println(",");
		}
		ps.println("]).");
	}
	
	/**
		 * to translate to PDDL and print the current oclTask to a PrintWriter.
		 * @param curDom - the full domain spec
		 * @param ps PrintWriter
		 * 
		 */
		public void pddlPrintTask(oclDomain curDom, PrintWriter ps) {
			ps.println("(define (problem task" + ID + ")");
			ps.println("   (:domain " +curDom.getName() +")");
			ps.println("   (:objects");
			ListIterator li = curDom.objects.listIterator();
			while (li.hasNext()) {
				ps.print("        ");
				oclObject obj = (oclObject)li.next();
				ListIterator liObjs = obj.getObjectNames().listIterator();
				while (liObjs.hasNext()) {
					ps.print(" "+ (String)liObjs.next());
				}
				ps.println(" - " + obj.getObjectSort());
			}
			ps.println("        )");
			ps.println("    (:init");
			li = inits.listIterator();
			while (li.hasNext()) {
				oclSS curSS = (oclSS)li.next();
				ListIterator liState = curSS.getState().listIterator();
				while (liState.hasNext()) {
					oclPredicate curPred = (oclPredicate)liState.next();
					if (curPred.isFluent()) {
						if (curPred instanceof oclFunctor) {
							((oclFunctor)curPred).pddlPrint(curDom,ps,8,true);
						} else {
							oclFunctor func = new oclFunctor(curPred);
							func.pddlPrint(curDom,ps,8,true);
						}
					} else {
						curPred.pddlPrint(curDom,ps,8,true);
					}
				}
			}
			li = curDom.atomicInvars.listIterator();
			while (li.hasNext()) {
				oclPredicate curPred = (oclPredicate)li.next();
				if (curPred.isFluent()) {
					((oclFunctor)curPred).pddlPrint(curDom,ps,8,true);
				} else {
					curPred.pddlPrint(curDom,ps,8,true);
				}
			}
			ps.println("        )");
			List goalPreds = new ArrayList();
			li = goals.listIterator();
			while (li.hasNext()) {
				oclSE curSE = (oclSE) li.next();
				ListIterator liPreds = curSE.getStrippedState().listIterator();
				while (liPreds.hasNext()) {
					oclPredicate curPred = (oclPredicate)liPreds.next();
					if (!curPred.isStatic()) {
						goalPreds.add(curPred);
					}
				}
			}
			if (goalPreds.size() > 0) {
				ps.println("    (:goal");
				ps.println("      (and");
			} else {
				ps.println("    (:goals");
			}
			li = goalPreds.listIterator();
			while (li.hasNext()) {
				oclPredicate curPred = (oclPredicate)li.next();
				if (curPred.isFluent()) {
					if (curPred instanceof oclFunctor) {
						((oclFunctor)curPred).pddlPrint(curDom,ps,8,true);
					} else {
						oclFunctor func = new oclFunctor(curPred);
						func.pddlPrint(curDom,ps,8,true);
					}
				} else {
					curPred.pddlPrint(curDom,ps,8,true);
				}
			}
			if (goalPreds.size() > 0) {
					ps.println("       ))");
			} else {
				ps.println("        )");
			}
			ps.println(")");
		}

	/* Weihong added on 28/08/2001 */
	/* 
	 * Print to the printer
	 */
	public int print(Graphics g, PageFormat pf, int pageIndex) {
		int x, y;
		x = (int) pf.getImageableX();
		y = (int) pf.getImageableY();
		Utility.debugPrintln("pageIndex: ---" + pageIndex);
		if (pageIndex > 0)
			return NO_SUCH_PAGE;

		g.setFont(new java.awt.Font("Arial", java.awt.Font.PLAIN, 8));
		g.setColor(Color.black);
		java.awt.FontMetrics fm = g.getFontMetrics();
		double theHeight = fm.getHeight();

		StringWriter dom;
		PrintWriter ps = (new PrintWriter(dom = new StringWriter()));
		oclPrintComponent(ps, 0, false);
		String str = new String(dom.getBuffer());
		Utility.debugPrintln("TEST string: " + str);

		int t = 0;
		String tmpStr = "";
		while (true) {
			t = str.indexOf("\n");
			tmpStr = str.substring(0, t);
			y += (int) (theHeight);
			g.drawString(tmpStr, x, y);
			if (str.length() > t + 1) {
				str = str.substring(t + 1);
			} else {
				break;
			}
		};

		return PAGE_EXISTS;
	}
}
