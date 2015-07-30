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

package jplan.edexpt;

import java.awt.*;
import java.util.*;
import javax.swing.*;
import javax.swing.tree.*;
import jplan.ocl.*;

/**
 * PredCellRenderer used to add simple icons to predicate descriptions in the
 * Predicate View List Box Updated for oclPlus - show fluent values
 * 
 * @author Ron Simpson
 * @version 1
 */
public class PredListCellRenderer implements ListCellRenderer {
	JList renderer;

	DefaultListModel lmPreds;

	protected DefaultListCellRenderer defaultRenderer = new DefaultListCellRenderer();

	protected DefaultTreeCellRenderer dtr = new DefaultTreeCellRenderer();

	/**
	 * constructor predListCellRendered - the renderer is based on a JList
	 */
	public PredListCellRenderer() {
		lmPreds = new DefaultListModel();
		lmPreds.addElement("dummy1arg1,arg2,arg3)");
		lmPreds.addElement("dummy2arg1,arg2,arg3)");
		lmPreds.addElement("dummy2arg1,arg2,arg3)");

		renderer = new JList(lmPreds);

		renderer.setBorder(BorderFactory.createLineBorder(Color.gray));
	}

	public Component getListCellRendererComponent(JList list, Object value,
			int index, boolean isSelected, boolean cellhasFocus) {

		if (value instanceof oclStateList) {
			lmPreds.clear();
			ListIterator li = ((oclStateList) value).getPredicateList()
					.listIterator();
			while (li.hasNext()) {
				oclPredicate cur = (oclPredicate) li.next();
				if (cur.isFluent()) { //Ron 2/07/03
					oclFunctor curFun = new oclFunctor(cur);
					lmPreds.addElement(curFun.toInstantiatedString());
				} else {
					lmPreds.addElement(cur.toString());
				}

			}
			if (!isSelected) {
				renderer.setBackground(list.getBackground());
			} else {
				renderer.setBackground(dtr.getBackgroundSelectionColor());
			}
			return renderer;
		}
		/* Weihong added on 23/05/2001 */
		/* for oclSS */
		else if (value instanceof oclSS) {
			lmPreds.clear();
			ListIterator li = ((oclSS) value).getState().listIterator();
			while (li.hasNext()) {
				oclPredicate cur = (oclPredicate) li.next();
				if (cur.isFluent()) { //Ron 2/07/03
					oclFunctor curFun = new oclFunctor(cur);
					lmPreds.addElement(curFun.toInstantiatedString());
				} else {
					lmPreds.addElement(cur.toString());
				}
			}
			if (!isSelected) {
				renderer.setBackground(list.getBackground());
			} else {
				renderer.setBackground(dtr.getBackgroundSelectionColor());
			}
			return renderer;
		}
		/* Weihong added on 25/05/2001 */
		/* for oclSE */
		else if (value instanceof oclSE) {
			lmPreds.clear();
			ListIterator li = ((oclSE) value).getState().listIterator();
			while (li.hasNext()) {
				oclPredicate cur = (oclPredicate) li.next();
				if (cur.isFluent()) { //Ron 2/07/03
					oclFunctor curFun = new oclFunctor(cur);
					lmPreds.addElement(curFun.toInstantiatedString());
				} else {
					lmPreds.addElement(cur.toString());
				}
			}
			if (!isSelected) {
				renderer.setBackground(list.getBackground());
			} else {
				renderer.setBackground(dtr.getBackgroundSelectionColor());
			}
			return renderer;
		} else if (value instanceof oclInconsistentConst) {
			lmPreds.clear();
			ListIterator li = ((oclInconsistentConst) value).getPredicateList()
					.listIterator();
			while (li.hasNext()) {
				oclPredicate cur = (oclPredicate) li.next();
				lmPreds.addElement(cur.toString());
			}
			if (!isSelected) {
				renderer.setBackground(list.getBackground());
			} else {
				renderer.setBackground(dtr.getBackgroundSelectionColor());
			}
			return renderer;
		} else if (value instanceof oclImpliedInvar) {
			lmPreds.clear();
			oclImpliedInvar cur = (oclImpliedInvar) value;
			ListIterator li = cur.getLeftList().listIterator();
			while (li.hasNext()) {
				oclPredicate curPred = (oclPredicate) li.next();
				lmPreds.addElement(curPred.toString());
			}
			lmPreds.addElement(" ==>");
			li = cur.getRightList().listIterator();
			while (li.hasNext()) {
				oclPredicate curPred = (oclPredicate) li.next();
				lmPreds.addElement(curPred.toString());
			}
			if (!isSelected) {
				renderer.setBackground(list.getBackground());
			} else {
				renderer.setBackground(dtr.getBackgroundSelectionColor());
			}
			return renderer;

		} else {

			return defaultRenderer;
		}
	}
}