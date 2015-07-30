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

/*
 * History
 * Ron 10/11/02 Ron added guard to all variable edits to ensure that if
 *              the objectID for the transition has not been set
 *              then edits do not check against this value
 *              Needed for method edit variable MethodVarEdit pane
 */

package jplan.general;

/**
 * This is a pane that contains an editable expression
 * that supports the editing of variables
 * this pane displays ne clauses in addition to normal predicates
 * it supports variable bindings, deleting predicates, 
 * adding static predicates and editing variable names.
 * @author Ron Simpson
 * @version 0
 */

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.*;
import java.util.List;
import java.util.*;
import java.util.NoSuchElementException;
import java.awt.event.*;
import java.awt.*;
import java.beans.*;

import jplan.ocl.*;
/* Weihong added on 15/5/2001 */
import jplan.general.ExpressionModel.ExpressionException;
import javax.swing.plaf.basic.BasicComboPopup;/* WZ 27/8/02 */

/**
 * This class defines the document pane that occupies a node in the operator
 * editor.
 * 
 * @author (s)Ron & Weihong
 */
public class TransExpressionDocPane extends FullExpressionDocPane {
	/**
	 * Left hand side of the transition.
	 */
	public final static int LHS = 0;

	/**
	 * Right hand side of the transition.
	 */
	public final static int RHS = 1;

	/**
	 * Left hand side of the transition without the pair at the right hand side.
	 */
	public final static int PREV = 2;

	private String objectID;

	private int transElement = -1;

	/* WZ 27/8/02 */
	private boolean hierFlag = false;

	private String curSort;

	private BasicComboPopup popupMenuInstantiate;

	private String selectedSort = null;

	private java.awt.Component theInvoker;

	private int locX, locY;

	protected JPopupMenu deleteLevelPopupMenu; // Delete states by level

	/* end 27/8/02 */
	protected JPopupMenu popupMenuAddDelFluent; //Ron 2/7/03 needed for oclPlus

	/**
	 * constructor
	 * 
	 * @param cur -
	 *            the current domain being edited
	 */
	public TransExpressionDocPane(oclDomain cur) {
		super(cur);
	}

	/**
	 * constructor
	 */
	public TransExpressionDocPane() {
		super();
	}

	/* WZ 27/8/02 */
	/**
	 * Set the value of hierFlag.
	 * 
	 * @param v
	 *            Value to assign to hierFlag.
	 */
	public void setHierFlag(boolean v) {
		this.hierFlag = v;
	}

	/**
	 * Get the value of objectID.
	 * 
	 * @return Value of objectID.
	 */
	public String getSort() {
		return curSort;
	}

	/**
	 * Set the value of objectID.
	 * 
	 * @param v
	 *            Value to assign to objectID.
	 */
	public void setSort(String v) {
		this.curSort = v;
	}

	/**
	 * Get the value of objectID.
	 * 
	 * @return Value of objectID.
	 */
	public String getObjectID() {
		return objectID;
	}

	/**
	 * Set the value of objectID.
	 * 
	 * @param v
	 *            Value to assign to objectID.
	 */
	public void setObjectID(String v) {
		this.objectID = v;
	}

	/* Weihong added on 16/5/2001 */
	/**
	 * clone a copy of TransExpressionDocPane
	 * 
	 * @return Object
	 */
	public Object clone() {
		TransExpressionDocPane copy = new TransExpressionDocPane();
		copy.objectID = this.objectID;
		if (curDomain != null) {
			try {
				copy.setCurDomain(curDomain);
				ListIterator li = getFullPredicateList().listIterator();
				while (li.hasNext()) {
					copy.addPredicate((oclPredicate) li.next());
				}
			} catch (OCLSelectionException e) {
				Utility.debugPrintln(e);
				return null;
			}
		}
		return copy;
	}

	/**
	 * Get the value of transElement.
	 * 
	 * @return value of transElement.
	 */
	public int getTransElement() {
		return transElement;
	}

	/**
	 * Set the value of transElement.
	 * 
	 * @param v
	 *            Value to assign to transElement.
	 */
	public void setTransElement(int v) {
		this.transElement = v;
	}

	/**
	 * override parent rename target (selected - green) variable
	 * 
	 * @param evt -
	 *            Action Event
	 */
	protected void renameTargetVariable(java.awt.event.ActionEvent evt) {
		Point selPoint = getSelectedVar();
		try {
			String select = curExpModel.getArgumentAt(selPoint.x);
			if (objectID != null && select.equals(objectID)) {
				JOptionPane.showMessageDialog(null,
						"You Cannot rename The Object Id for this transition.",
						"GIPO Error", JOptionPane.ERROR_MESSAGE, null);
				return;
			}
			JPanel jpanUnify = new JPanel();
			jpanUnify.setBorder(BorderFactory.createTitledBorder("Rename .."));

			JRadioButton jradThis = new JRadioButton("This variable only",
					false);
			JRadioButton jradNode = new JRadioButton("This Node Only", false);
			JRadioButton jradTrans = new JRadioButton("This Transition Only",
					true);
			JRadioButton jradAll = new JRadioButton("All occurances", false);
			jradThis.setMnemonic(KeyEvent.VK_V);
			jradNode.setMnemonic(KeyEvent.VK_N);
			jradTrans.setMnemonic(KeyEvent.VK_T);
			jradAll.setMnemonic(KeyEvent.VK_A);
			Box boxUnify = Box.createVerticalBox();
			ButtonGroup groupUnify = new ButtonGroup();
			boxUnify.add(jradThis);
			boxUnify.add(jradNode);
			boxUnify.add(jradTrans);
			boxUnify.add(jradAll);
			groupUnify.add(jradThis);
			groupUnify.add(jradNode);
			groupUnify.add(jradTrans);
			groupUnify.add(jradAll);
			jpanUnify.add(boxUnify);
			Object message[] = { "Enter New Name for Variable.", jpanUnify };

			JOptionPane query = new JOptionPane();
			query.setMessage(message);
			query.setMessageType(JOptionPane.QUESTION_MESSAGE);
			query.setOptionType(JOptionPane.OK_CANCEL_OPTION);
			query.setWantsInput(true);
			JDialog dia = query.createDialog(null, "Query");
			dia.show();
			if (((Integer) query.getValue()).intValue() == 0) {
				// Selected OK
				String newVName = (String) query.getInputValue();
				// Should check that it doesnt exist No dont allow editing of
				// the transition id
				if (jradThis.isSelected()) {
					curExpModel.editVar(selPoint.x, newVName);
					// Ron 2/5/03 change ID for transition if needed
					if (select.equals(objectID)) {
						setObjectID(newVName);
					}
					doc.remove(selPoint.x, (selPoint.y - selPoint.x));
					doc.insertString(selPoint.x, newVName,
							new SimpleAttributeSet());
				} else if (jradNode.isSelected()) {
					selPoint.x = replaceAllVariables(selPoint.x, select,
							newVName);
					selPoint.y = selPoint.x + newVName.length();
					// Ron 2/5/03 change ID for transition if needed
					Utility.debugPrintln("XX>><< Old = " + select
							+ " objectID = " + objectID);
					if (select.equals(objectID)) {
						setObjectID(newVName);
					}
				} else if (jradTrans.isSelected()) {
					selPoint.x = replaceAllVariables(selPoint.x, select,
							newVName);
					fireEvent(new ExpressionPaneEvent(this,
							ExpressionPaneEvent.RENAME, newVName, select,
							objectID, ExpressionPaneEvent.TRANSITION));

				} else if (jradAll.isSelected()) {
					selPoint.x = replaceAllVariables(selPoint.x, select,
							newVName);
					fireEvent(new ExpressionPaneEvent(this,
							ExpressionPaneEvent.RENAME, newVName, select,
							objectID, ExpressionPaneEvent.GLOBAL));
					/* WZ 22/8/02 */
				} else
					// redo highlights
					removeHighlights();
				fireEvent(new ExpressionPaneEvent(this,
						ExpressionPaneEvent.CLEAR));
			}
		} catch (Exception sme) {
			Utility.debugPrintln("Unexpected failure to rename argument" + sme);
		}
	}

	/**
	 * watch for selections of variables and requests for popups override parent
	 * version - we need a variety of popups depending o mouse position. In
	 * transition view we allow deletion of predicates on LHS
	 * 
	 * @param mouseEvent
	 *            the mouse event triggering action
	 */
	// Ron 29/8/01
	public void mouseWatch(MouseEvent mouseEvent) {
		Point viewPoint = mouseEvent.getPoint();
		clickPos = this.viewToModel(viewPoint);
		oclPredicate curPred = null;
		try {
			curPred = curExpModel.getPredicateAt(clickPos);
			Utility.debugPrintln("MouseWatch: " + curPred.toString()); //Weihong
			// 15/3/02
		} catch (ExpressionModel.ExpressionException ee) {
			Utility.debugPrintln(ee);
			/* WZ 28/8/02 */
			if (hierFlag) {
				theInvoker = mouseEvent.getComponent();
				locX = mouseEvent.getX();
				locY = mouseEvent.getY();

				initDeleteLevelPopup();
				deleteLevelPopupMenu.show(mouseEvent.getComponent(), mouseEvent
						.getX(), mouseEvent.getY());
			}
			/* end 28/8/02 */
			return;
		}

		if (SwingUtilities.isRightMouseButton(mouseEvent)) {
			if (curPred.getName().equals("ne")
					|| curPred.getName().equals("is_of_sort")) {
				deletePopupMenu.show(mouseEvent.getComponent(), mouseEvent
						.getX(), mouseEvent.getY());
			} else {
				Point selPoint = getSelectedVar();
				if (selPoint.x <= clickPos && selPoint.y >= clickPos) {
					// This is the currently selected variable
					List subTypes = curDomain.getSortSubTypes(selectedType);
					if (subTypes != null) {
						restrictPopupMenu.show(mouseEvent.getComponent(),
								mouseEvent.getX(), mouseEvent.getY());
					} else {
						popupMenuRename.show(mouseEvent.getComponent(),
								mouseEvent.getX(), mouseEvent.getY());
					}
				} else {
					ListIterator li = getMatchingVars().listIterator();
					boolean found = false;
					while (li.hasNext() && !found) {
						Point next = (Point) li.next();
						if (next.x <= clickPos && next.y >= clickPos) {
							popupTarget.x = next.x;
							popupTarget.y = next.y;
							found = true;
						}
					}
					if (found) {
						popupMenu.show(mouseEvent.getComponent(), mouseEvent
								.getX(), mouseEvent.getY());
					} else {
						// Ron 29/8/01 added ability to delete predicates
						if (curPred.isStatic()) {
							deletePopupMenu.show(mouseEvent.getComponent(),
									mouseEvent.getX(), mouseEvent.getY());
						} else if (transElement != RHS) {
							deletePopupMenu.show(mouseEvent.getComponent(),
									mouseEvent.getX(), mouseEvent.getY());
						} else {
							/* WZ 28/8/02 */
							if (hierFlag) {
								theInvoker = mouseEvent.getComponent();
								locX = mouseEvent.getX();
								locY = mouseEvent.getY();

								initDeleteLevelPopup();
								deleteLevelPopupMenu.show(mouseEvent
										.getComponent(), mouseEvent.getX(),
										mouseEvent.getY());
							}
							/* end 28/8/02 */
						}
					}
				}
			}
		} else {
			// We need to highlight variables
			int pos = clickPos;
			ExpressionModel.PredDetail cur = null;
			String arg = null;
			try {
				cur = curExpModel.getPredDetailAt(pos);
			} catch (Exception e) {
				// No predicate found
				return;
			}
			try {
				Utility.debugPrintln("green highlight variable... "
						+ cur.toString());
				arg = cur.pred.elementAt(pos - cur.startOffset);
				int startOffset = cur.startOffset
						+ cur.pred.startElementAt(pos - cur.startOffset);
				int n = cur.pred.elementNoAt(pos - cur.startOffset);
				selectedType = cur.proto.getNthElementName(n);
				List unifiers = curDomain.getSortUnifiers(selectedType);
				selectedArg = arg;
				searcher.markSelected(startOffset, startOffset + arg.length());
				unifiers.add(selectedType);
				selectedUnifiers = unifiers;
				searcher.searchUnifiers(unifiers, startOffset, startOffset
						+ arg.length());
				lastSelPred = cur;
				fireEvent(new ExpressionPaneEvent(this,
						ExpressionPaneEvent.SELECTION, arg));
				selectedArg = arg;
			} catch (Exception e) {
				if (e instanceof NoSuchElementException) {
					Utility.debugPrintln("Illegal sort name!!");
					return;
				}

				// Ignore dot outside predicate most likely at the
				// end of the document
			}
		}
	}

	/* WZ 27/8/02 */
	/**
	 * init popup menu to allow deletion of states by hierarchical level create
	 * the popup menu
	 */
	protected void initDeleteLevelPopup() {
		if (!hierFlag)
			return;

		deleteLevelPopupMenu = new JPopupMenu();
		JMenuItem deleteLevelMI = new JMenuItem("Delete States Level");
		deleteLevelMI.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				if (popupMenuInstantiate.getList().getModel().getSize() == 0) {
					JOptionPane
							.showMessageDialog(
									null,
									"No states from higher level of the hierarchy exist.",
									"GIPO Information",
									JOptionPane.WARNING_MESSAGE, null);
				} else {
					popupMenuInstantiate.show(theInvoker, locX, locY);
				}
			}
		});

		//get all sorts at higher level
		Vector items = new Vector();
		String tmpSort = curSort;
		checkanothersort: while (tmpSort != null) {
			try {
				tmpSort = curDomain.findSortParent(tmpSort);
				if (curDomain.hasStates(tmpSort)) {
					//check if at one of the predicates apprears in the pane
					ListIterator li = curDomain.getStates(tmpSort)
							.getStateList().listIterator();
					while (li.hasNext()) {
						oclStateList ostlist = (oclStateList) li.next();
						ListIterator liSTList = ostlist.getPredicateList()
								.listIterator();
						while (liSTList.hasNext()) {
							oclPredicate oprd = (oclPredicate) liSTList.next();
							if (containsPredicate(oprd)) {
								items.addElement(tmpSort);
								continue checkanothersort;
							}
						}
					}
				}
			} catch (NoSuchElementException e) {
				tmpSort = null;//to break;
			}
		}

		//build the Menu
		JComboBox combo = new JComboBox(items);
		combo.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				try {
					selectedSort = (String) (popupMenuInstantiate.getList()
							.getSelectedValue());
					//get list of states of the current selected sort
					oclSSClassDef def = curDomain.getStates(selectedSort);
					ListIterator liGivenStates = def.getStateList()
							.listIterator();
					while (liGivenStates.hasNext()) {
						oclStateList ost = (oclStateList) liGivenStates.next();
						ListIterator liStateList = ost.getPredicateList()
								.listIterator();
						while (liStateList.hasNext()) {
							oclPredicate oprd = (oclPredicate) liStateList
									.next();
							ExpressionModel.PredDetail curPredDetail = curExpModel
									.removePredicateByName(oprd);

							if (curExpModel.size() > 0) {
								if (curPredDetail != null) {
									doc.remove(curPredDetail.startOffset,
											curPredDetail.length + 1);
								}
								setDocAttributes(curExpModel
										.getExpressionList());
							} else {
								doc.remove(0, doc.getLength());
							}
						}
					}

					removeHighlights();
					popupMenuInstantiate.hide(); /* WZ 15/5/02 */
				} catch (Exception e) {
					Utility.debugPrintln(e);
				}
			}
		});
		popupMenuInstantiate = new BasicComboPopup(combo);

		deleteLevelPopupMenu.add(deleteLevelMI);

	}

	public boolean containsPredicate(oclPredicate opd) {
		ListIterator li = getPurePredicateList().listIterator();
		while (li.hasNext()) {
			oclPredicate predicate = (oclPredicate) li.next();
			if (predicate.isSameType(opd))
				return true;
		}
		return false;
	}

	/**
	 * sameBindings ensure target and selected variable have same names and
	 * remove nes if they exist Do not allow renaming of variables corresponding
	 * to the Transition ID
	 * 
	 * @param evt -
	 *            Action Event
	 */
	// Ron 28/8/01
	protected void sameBindings(java.awt.event.ActionEvent evt) {
		// Change target Variable to match selected
		String select = null;
		Point selPoint = getSelectedVar();
		// 		    Utility.debugPrintln("<<>>Fired Same selPoint x = " + selPoint.x);
		try {
			if (selPoint.x != -1) {
				// Selection in this pane
				select = curExpModel.getArgumentAt(selPoint.x);
			} else {
				select = selectedArg;
			}
			Utility.debugPrintln("<<>> found select " + select);
			oclPredicate targPred = curExpModel.getPredicateAt(popupTarget.x);
			String oldVName = curExpModel.editVar(popupTarget.x, select);
			Utility.debugPrintln("<<>> found old var " + oldVName);
			if (objectID != null && objectID.equals(oldVName)) {
				JOptionPane.showMessageDialog(null,
						"You Cannot rename The Object Id for this transition.",
						"GIPO Error", JOptionPane.ERROR_MESSAGE, null);
				return;
			}
			//			Ron 2/5/03 change ID for transition if needed
			if (oldVName.equals(objectID)) {
				setObjectID(select);
			}
			doc.remove(popupTarget.x, (popupTarget.y - popupTarget.x));
			doc.insertString(popupTarget.x, select, new SimpleAttributeSet());
			// Deal with NEs
			Utility.debugPrintln("<<>> Done insert");
			int neIndex = curExpModel.neExists(oldVName, select);
			Utility.debugPrintln("<<>> neIndex = " + neIndex);
			if (neIndex != -1) {
				if (JOptionPane.YES_OPTION == JOptionPane.showConfirmDialog(
						null, "Remove restriction clause  ne(" + oldVName + ","
								+ select + ")", "GIPO Query",
						JOptionPane.YES_NO_OPTION)) {
					curExpModel.neList.removeElementAt(neIndex);
				}
			}

			removeHighlights();
			Utility.debugPrintln("<<>> Removed highlights");

			fireEvent(new ExpressionPaneEvent(this, ExpressionPaneEvent.CLEAR));
			Utility.debugPrintln("<<>> Fired CLEAR Event");
		} catch (Exception sme) {
			Utility.debugPrintln("Unexpected failure to select argument"
					+ sme.toString());
		}
	}

	/**
	 * enforceDifferentBindings ensure target and selected variable have
	 * different names and appropriate ne clause exists Do not allow renaming of
	 * variables corresponding to the Transition ID
	 * 
	 * @param evt -
	 *            Action Event
	 */
	// Ron 11/10/02
	protected void enforceDifferentBindings(java.awt.event.ActionEvent evt) {
		Point selPoint = getSelectedVar();
		try {
			String select = null;
			if (selPoint.x != -1) {
				// Selection in this pane
				select = curExpModel.getArgumentAt(selPoint.x);
			} else {
				select = selectedArg;
			}
			String targArg = curExpModel.getArgumentAt(popupTarget.x);
			String targSort = curExpModel.getSortForArgumentAt(popupTarget.x);
			if (select.equals(targArg)) { // Currently same
				// 		String newVName= JOptionPane.showInputDialog(null,
				// 			      "Enter New Name for Variable.");

				/* Weihong changed/added on 13/2/2002 */
				if (objectID != null && objectID.equals(targArg)) {
					JOptionPane
							.showMessageDialog(
									null,
									"You Cannot rename The Object Id for this transition.",
									"GIPO Error", JOptionPane.ERROR_MESSAGE,
									null);
					return;
				}
				String newVName = GipoInputBox.showVarableInputBox(null,
						"GIPO Input", "Enter New Name for Variable.");
				if (newVName != null) {
					// Should check that it doesnt exist
					curExpModel.editVar(popupTarget.x, newVName);
					//					Ron 2/5/03 change ID for transition if needed
					if (targArg.equals(objectID)) {
						setObjectID(newVName);
					}
					doc.remove(popupTarget.x, (popupTarget.y - popupTarget.x));
					doc.insertString(popupTarget.x, newVName,
							new SimpleAttributeSet());
					if (curExpModel.neExists(newVName, select) == -1) {
						oclPredicate curNE;
						if (curDomain.sortIsSubSortOf(selectedType, targSort)) {
							curNE = curExpModel.addNE(newVName, select,
									selectedType, doc.getLength());
						} else {
							curNE = curExpModel.addNE(newVName, select,
									targSort, doc.getLength());
						}
						int offset = doc.getLength();
						doc.insertString(offset, curNE + "\n".toString(),
								neStyle);
					}
					// redo highlights
					// 		if (selPoint.x != -1) {
					// 		    searcher.searchUnifiers(selectedUnifiers,
					// 					    searcher.selectedVar.x,
					// 					    searcher.selectedVar.y);
					// 		} else {
					// 		    searcher.searchUnifiers(selectedUnifiers,-1,-1);
					// 		}
					searcher.removeHighlights();
					fireEvent(new ExpressionPaneEvent(this,
							ExpressionPaneEvent.CLEAR));
				}
			} else {
				// Check that appropriate NE exists
				if (curExpModel.neExists(targArg, select) == -1) {
					oclPredicate curNE;
					if (curDomain.sortIsSubSortOf(selectedType, targSort)) {
						curNE = curExpModel.addNE(targArg, select,
								selectedType, doc.getLength());
					} else {
						curNE = curExpModel.addNE(targArg, select, targSort,
								doc.getLength());
					}
					doc.insertString(doc.getLength(), curNE + "\n".toString(),
							neStyle);
					searcher.removeHighlights();
					fireEvent(new ExpressionPaneEvent(this,
							ExpressionPaneEvent.CLEAR));

				}
				// Should check that appropriate NE exists
				Utility.debugPrintln("These variables are already different"
						+ "\n given " + targArg + " and " + select);
			}

		} catch (Exception sme) {
			Utility.debugPrintln("Unexpected failure to select argument");
		}
	}

	/**
	 * allowDifferentBindings ensure target and selected variable have different
	 * names BUT no ne clause exists Do not allow renaming of variables
	 * corresponding to the Transition ID
	 * 
	 * @param evt -
	 *            Action Event
	 */
	// Ron 11/10/02
	protected void allowDifferentBindings(java.awt.event.ActionEvent evt) {
		Point selPoint = getSelectedVar();
		try {
			String select = null;
			if (selPoint.x != -1) {
				// Selection in this pane
				select = curExpModel.getArgumentAt(selPoint.x);
			} else {
				select = selectedArg;
			}
			String targArg = curExpModel.getArgumentAt(popupTarget.x);
			if (select.equals(targArg)) { // Currently same
				if (objectID != null && objectID.equals(targArg)) {
					JOptionPane
							.showMessageDialog(
									null,
									"You Cannot rename The Object Id for this transition.",
									"GIPO Error", JOptionPane.ERROR_MESSAGE,
									null);
					return;
				}
				String newVName = JOptionPane.showInputDialog(null,
						"Enter New Name for Variable.");
				// Should check that it doesnt exist
				curExpModel.editVar(popupTarget.x, newVName);
				doc.remove(popupTarget.x, (popupTarget.y - popupTarget.x));
				doc.insertString(popupTarget.x, newVName,
						new SimpleAttributeSet());
				int neIndex = curExpModel.neExists(newVName, select);
				if (neIndex != -1) {
					if (JOptionPane.YES_OPTION == JOptionPane
							.showConfirmDialog(null,
									"Remove ne restriction clause",
									"GIPO Query", JOptionPane.YES_NO_OPTION)) {
						curExpModel.neList.removeElementAt(neIndex);
					}

				}
				// redo highlights
				searcher.removeHighlights();
				fireEvent(new ExpressionPaneEvent(this,
						ExpressionPaneEvent.CLEAR));
			} else {
			}

		} catch (Exception sme) {
			Utility.debugPrintln("Unexpected failure to select argument");
		}
	}
}