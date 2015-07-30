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
 * Created on 01-Sep-2003
 *
 * Author ron
 * 
 */
package jplan.ocl;

import java.util.List;
import java.util.ArrayList;
import java.util.ListIterator;
import java.io.*;
import jplan.general.OPredicate;
import jplan.general.Utility;

/**
 * @author ron
 * oclPlus version of an event
 */
public class oclEvent extends oclOperator
	implements Cloneable, oclPrint, Serializable {
	
	public oclEvent(){
		super();
	}
	public oclEvent(oclOperator op) {
			opName = op.opName;
			docm = op.docm;
			prevail = op.prevail;
			necessary = op.necessary;
			conditional = op.conditional;
		}

		/**
		 * do verification checks on operators.<br>
		 * check each clause is for a different object;
		 * check each transition 
		 * @param cur the current domain
		 * @param mssgLst list to append results messages to
		 * @return true if all checks passed
		 */
		public boolean check(oclDomain curDom, List mssgs) {
			List ids = new ArrayList();
			List sorts = new ArrayList();
			boolean ret = true;

			mssgs.add("Checking Events " + opName.toString());
			ListIterator li = prevail.listIterator();
			while (li.hasNext()) {
				oclSEPlus cur = (oclSEPlus) li.next();
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
				oclSCPlus cur = (oclSCPlus) li.next();
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
				oclSCPlus cur = (oclSCPlus) li.next();
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

			//change name
			opName = curDom.createEventSignature(this);
			// 	opName = createAllSignature(curDom);
			Utility.debugPrintln("\n>>opName. " + opName.toString());

			return ret;
		}

		/**
		 * clone a copy of oclOperator
		 * @throws CloneNotSupportedException
		 * @return Object
		 */
		public Object clone() throws CloneNotSupportedException {
			try {
				oclEvent op = new oclEvent();
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
				Utility.debugPrintln("Failed to clone event component + " + e.toString());
				throw e;
			}
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
			ps.println("  (:event " + opName.getName());
			pddlPrintBody(curDom, ps, indent, nline);
		}

		
		/**
		 * to print the current oclOperator to a PrintWriter.
		 * @param ps PrintWriter
		 * @param indent value of indentation
		 * @param nline not being used really
		 * @return void
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
			ps.print("event(");
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

		/* WZ 17/6/02 */
		/**
		 * a string expression, mainly used for saving purpose
		 * @return String
		 */
		public String to_String() {
			StringBuffer str = new StringBuffer();
			str.append("event::Description:");
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
