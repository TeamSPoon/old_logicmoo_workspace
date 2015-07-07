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

/**
 * StateExpressionDocPane
 * This is a pane that contains an editable expression
 * that supports the editing of variables but is aware of the defined
 * state ID 
 * @author Ron Simpson
 * @version 0
 */

/**
 * History:
 * Ron 18/10/02 : The mechanism in place for preventing renaming of StateIds
 *                was only partly implemented. I have now completed this
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
import jplan.top.OclEd; /* Weihong changed/added on 7/9/2001 */

public class StateExpressionDocPane extends ExpressionDocPane {
	private String stateID; //State id for predicate list 
	private String stateSort; // The sort of the state ID
	private boolean allowStateVarEditing = false;
	// check to see if the state var can be edited

	JPopupMenu renamePopupMenu;
	JPopupMenu restrictPopupMenu;
	private OclEd top; /* Weihong changed/added on 7/9/2001 */
	public StateExpressionDocPane(oclDomain cur, boolean editable) {
		super(cur);
		stateID = new String("");
		stateSort = new String("");
		allowStateVarEditing = editable;
		initStateRenamePopup();
		initRestrictPopup();
	}

	/* Weihong changed/added on 7/9/2001 */
	public StateExpressionDocPane(OclEd top, oclDomain cur, boolean editable) {
		super(cur);
		this.top = top;
		stateID = new String("");
		stateSort = new String("");
		allowStateVarEditing = editable;
		initStateRenamePopup();
		initRestrictPopup();
	}

	public StateExpressionDocPane() {
		super();
		stateID = new String("");
		stateSort = new String("");
		initStateRenamePopup();
		initRestrictPopup();
	}

	/**
	 * setStateID
	 * set the state id for this predicate list
	 * @param ID the state id
	 */
	public void setStateID(String ID) {
		stateID = new String(ID);
	}

	/**
	 * getStateID
	 * @return String - the state id for this predicate list
	 */
	public String getStateID() {
		return stateID;
	}

	/**
	 * setStateSort
	 * set the state sort for this predicate list
	 * @param  sort
	 */
	public void setStateSort(String sort) {
		stateSort = new String(sort);
	}

	/**
	 * setStateVarEditable
	 *  allow/disallow editing of the state variable
	 * @param editable - true enables editing of state var
	 */
	public void setStateVarEditable(boolean editable) {
		allowStateVarEditing = editable;
	}

	/**
	 * mouseWatch
	 * watch for selections of variables and requests for popups
	 * override parent method
	 *
	 */
	public void mouseWatch(MouseEvent mouseEvent) {
		Utility.debugPrintln("Mouse Pressed " + mouseEvent.paramString());
		if ((mouseEvent.getModifiers() & InputEvent.BUTTON1_MASK)
			== InputEvent.BUTTON1_MASK)
			Utility.debugPrintln("Button 1 pressed");
		Point viewPoint = mouseEvent.getPoint();
		int clickPos = this.viewToModel(viewPoint);
		DefaultCaret caret = (DefaultCaret) this.getCaret();
		if (SwingUtilities.isRightMouseButton(mouseEvent)) {
			//caret.positionCaret(mouseEvent);
			int pos = caret.getDot();
			Utility.debugPrintln("Dot = " + pos + " View = " + clickPos);
			// look to see if this is a matching (highlighted) variable
			ListIterator li = getMatchingVars().listIterator();
			boolean found = false;
			while (li.hasNext() && !found) {
				Point next = (Point) li.next();
				Utility.debugPrintln("X = " + next.x + " Y = " + next.y);
				if (next.x <= clickPos && next.y >= clickPos) {
					popupTarget.x = next.x;
					popupTarget.y = next.y;
					found = true;
				}
			}
			if (found) {
				popupMenu.show(
					mouseEvent.getComponent(),
					mouseEvent.getX(),
					mouseEvent.getY());
			} else {
				// this is not a variable matched up with its unifiers
				// All we should do is allow user to change 
				// the name of selected arguments
				int cPos = clickPos;
				StateModel.PredDetail cur = null;
				String arg = null;
				try {
					cur = curStateModel.getPredDetailAt(cPos);
				} catch (Exception e) {
					// No predicate found
					return;
				}
				try {
					arg = cur.pred.elementAt(cPos - cur.startOffset);
					if (!arg.equals(selectedArg)) {
						JOptionPane.showMessageDialog(
							this,
							"Please select the variable (left button) before editing.",
							"GIPO Message",
							JOptionPane.INFORMATION_MESSAGE,
							null);
						return;
					} else {
						List subTypes = curDomain.getSortSubTypes(selectedType);
						if (subTypes != null) {
							Utility.debugPrintln(
								"TYPE " + selectedType + " has sub types");
							restrictPopupMenu.show(
								mouseEvent.getComponent(),
								mouseEvent.getX(),
								mouseEvent.getY());
						} else {
							renamePopupMenu.show(
								mouseEvent.getComponent(),
								mouseEvent.getX(),
								mouseEvent.getY());
						}
					}
				} catch (Exception e) {
					if (e instanceof NoSuchElementException) {
						Utility.debugPrintln("Illegal sort name!!");
						return;
					}
				}
			}
		} else {
			int pos = clickPos;
			StateModel.PredDetail cur = null;
			String arg = null;
			try {
				cur = curStateModel.getPredDetailAt(pos);
			} catch (Exception e) {
				// No predicate found
				return;
			}
			try {
				arg = cur.pred.elementAt(pos - cur.startOffset);

				int startOffset =
					cur.startOffset
						+ cur.pred.startElementAt(pos - cur.startOffset);
				int n = cur.pred.elementNoAt(pos - cur.startOffset);
				selectedType = cur.proto.getNthElementName(n);
				List unifiers = curDomain.getSortUnifiers(selectedType);
				ListIterator li = unifiers.listIterator();
				while (li.hasNext()) {
					Utility.debugPrintln("UNIFIER >>>>> " + (String) li.next());
				}

				selectedArg = arg;
				searcher.markSelected(startOffset, startOffset + arg.length());
				unifiers.add(selectedType);
				selectedUnifiers = unifiers;
				searcher.searchUnifiers(
					unifiers,
					startOffset,
					startOffset + arg.length());
				//	    searcher.searchOthers(selectedType,startOffset,startOffset + arg.length());
				lastSelPred = cur;
				fireEvent(
					new ExpressionPaneEvent(
						this,
						ExpressionPaneEvent.SELECTION,
						arg));
				selectedArg = arg;
			} catch (Exception e) {
				if (e instanceof NoSuchElementException) {
					Utility.debugPrintln("Illegal sort name!!");
					return;
				}
			}
		}
	}

	/**
	 * initStateRenamePopup
	 * init popup menu to allow renaming single variable
	 * create the popup menu
	 */
	private void initStateRenamePopup() {
		renamePopupMenu = new JPopupMenu();
		JMenuItem renameMI = new JMenuItem("Re-Name");
		popupTarget = new Point(-1, -1);
		renameMI.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				// Change target Variable
				renameAL(evt);
			}
		});
		renamePopupMenu.add(renameMI);
	}

	/**
	 * renameAL
	 * event listener for rename popup menu
	 * @param evt the event causing this popup to show
	 */
	private void renameAL(java.awt.event.ActionEvent evt) {
		Utility.debugPrintln("Fired Rename");
		// Ron 18/10/02 Do not allow editing of state variable
		if (!allowStateVarEditing && selectedArg.equals(stateID)) {
			JOptionPane.showMessageDialog(
				this,
				"To rename the state id\n - update and use state change id button.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}
		/* Weihong changed/added on 6/9/2001 */
		String newVarName =
			GipoInputBox.showVarableInputBox(
				top,
				"GIPO Query",
				"Enter new name.");
		Utility.debugPrintln("Fired Rename " + newVarName);
		try {
			this.editVarName(newVarName);
		} catch (OCLSelectionException ose) {
			Utility.debugPrintln("Var Update failed " + ose.toString());
			return;
		}
		Utility.debugPrintln("Selected Arg = " + selectedArg);
		List illegalNEs = curStateModel.illegalNEs();
		ListIterator li = illegalNEs.listIterator();
		int noRemoved = 0;
		while (li.hasNext()) {
			int neIndex = ((Integer) li.next()).intValue();
			oclPredicate curNE =
				(oclPredicate) curStateModel.neList.elementAt(
					neIndex - noRemoved);
			try {
				curNE.replaceVariableNameByName(selectedArg, newVarName);
				curStateModel.neList.removeElementAt(neIndex - noRemoved);
				curStateModel.neList.insertElementAt(
					curNE,
					neIndex - noRemoved);
			} catch (Exception e) {
				curStateModel.neList.removeElementAt(neIndex - noRemoved);
				noRemoved++;
			}
		}
		searcher.removeHighlights();
	}

	/**
	 * initRestrictPopup
	 * init popup menu to allow renaming single variable
	 * create the popup menu + addition of is_of_sort clauses
	 */
	private void initRestrictPopup() {
		restrictPopupMenu = new JPopupMenu();
		JMenuItem renameMI = new JMenuItem("Re-Name");
		popupTarget = new Point(-1, -1);
		renameMI.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				// Change target Variable
				renameAL(evt);
			}
		});
		JMenuItem restrictMI = new JMenuItem("Restrict Sort");
		popupTarget = new Point(-1, -1);
		restrictMI.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				// Change target Variable
				restrictAL(evt);
			}
		});
		JMenuItem delRestrictMI = new JMenuItem("Delete Restriction");
		popupTarget = new Point(-1, -1);
		delRestrictMI.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				// Change target Variable
				delRestrictAL(evt);
			}
		});
		restrictPopupMenu.add(renameMI);
		restrictPopupMenu.add(restrictMI);
		restrictPopupMenu.add(delRestrictMI);

	}

	/**
	 * delRestrictAL
	 * event listener for restrict popup menu
	 * @param evt the event causing this popup to show
	 */
	private void delRestrictAL(java.awt.event.ActionEvent evt) {
		Utility.debugPrintln("Fired del Restrict");
		try {
			curStateModel.removeRestrictionForVariable(selectedArg);
		} catch (StateModel.StateModelException e) {
			JOptionPane.showMessageDialog(
				this,
				"No restriction for this argument.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
		}
		searcher.removeHighlights();
	}

	/**
	 * restrictAL
	 * event listener for delete restriction popup menu
	 * @param evt the event causing this popup to show
	 */
	private void restrictAL(java.awt.event.ActionEvent evt) {
		Utility.debugPrintln("Fired Restrict");
		List subTypes = curDomain.getBaseSortSubTypes(selectedType);
		ListIterator li = subTypes.listIterator();
		while (li.hasNext()) {
			Utility.debugPrintln("Sub >>> " + (String) li.next());
		}
		Object options[] = subTypes.toArray();
		String restrictSort =
			(String) JOptionPane.showInputDialog(
				this,
				"Select the restricting sort",
				"GIPO Query",
				JOptionPane.QUESTION_MESSAGE,
				null,
				options,
				options[0]);
		if (restrictSort == null)
			return;
		oclPredicate restrictPred = new oclPredicate("is_of_sort");
		restrictPred.addVarArgument(selectedArg);
		restrictPred.addConstArgument(restrictSort);
		Utility.debugPrintln("Restrict  " + restrictPred.toString());
		try {
			curStateModel.addRestriction(restrictPred);
		} catch (StateModel.StateModelException e) {
			JOptionPane.showMessageDialog(
				this,
				"A restriction for this argument already exists.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
		}
		searcher.removeHighlights();
	}

	/**
	* enforceDifferentBindings
	* ensure target and selected variable have different names and 
	* appropriate ne clause exists
	* @param evt - Action Event 
	*/
	protected void enforceDifferentBindings(java.awt.event.ActionEvent evt) {
		Point selPoint = getSelectedVar();
		try {
			String select = null;
			if (selPoint.x != -1) {
				// Selection in this pane
				select = curStateModel.getArgumentAt(selPoint.x);
			} else {
				select = selectedArg;
			}
			String targArg = curStateModel.getArgumentAt(popupTarget.x);
			// Ron 18/10/02
			if (!allowStateVarEditing && targArg.equals(stateID)) {
				JOptionPane.showMessageDialog(
					this,
					"To rename the state id\n - update and use state change id button.",
					"GIPO Error",
					JOptionPane.ERROR_MESSAGE,
					null);
				return;
			}
			if (select.equals(targArg)) { // Currently same
				String newVName =
					JOptionPane.showInputDialog(
						null,
						"Enter New Name for Variable.");
				// Should check that it doesnt exist
				curStateModel.editVar(popupTarget.x, newVName);
				ignoreCaretMove = true;
				doc.remove(popupTarget.x, (popupTarget.y - popupTarget.x));
				ignoreCaretMove = true;
				doc.insertString(
					popupTarget.x,
					newVName,
					new SimpleAttributeSet());
				if (curStateModel.neExists(newVName, select) == -1) {
					oclPredicate curNE = curStateModel.addNE(newVName, select);
				}
				// redo highlights
				if (selPoint.x != -1) {
					searcher.searchUnifiers(
						selectedUnifiers,
						searcher.selectedVar.x,
						searcher.selectedVar.y);
				} else {
					searcher.searchUnifiers(selectedUnifiers, -1, -1);
				}
			} else {
				// Check that appropriate NE exists
				if (curStateModel.neExists(targArg, select) == -1) {
					oclPredicate curNE = curStateModel.addNE(targArg, select);
				}
				// Should check that appropriate NE exists
				// 		Utility.debugPrintln("These variables are already different" +
				// 				   "\n given " + targArg + " and " +
				// 				   select);
			}

		} catch (Exception sme) {
			Utility.debugPrintln("Unexpected failure to select argument");
		}
	}
	/**
	* allowDifferentBindings
	* ensure target and selected variable have different names BUT no
	* ne clause exists
	* @param evt - Action Event 
	*/
	protected void allowDifferentBindings(java.awt.event.ActionEvent evt) {
		Point selPoint = getSelectedVar();
		try {
			// 	    Utility.debugPrintln("Selection Point X = " + selPoint.x);
			String select = curStateModel.getArgumentAt(selPoint.x);
			// 	    Utility.debugPrintln("Selected Variable " + select +
			// 			       " popup target x = " + popupTarget.x +
			// 			       " popup target y = " + popupTarget.y );
			String targArg = curStateModel.getArgumentAt(popupTarget.x);
			// Ron 18/10/02
			if (!allowStateVarEditing && targArg.equals(stateID)) {
				JOptionPane.showMessageDialog(
					this,
					"To rename the state id\n - update and use state change id button.",
					"GIPO Error",
					JOptionPane.ERROR_MESSAGE,
					null);
				return;
			}
			if (select.equals(targArg)) { // Currently same
				String newVName =
					JOptionPane.showInputDialog(
						null,
						"Enter New Name for Variable.");
				// Should check that it doesnt exist
				curStateModel.editVar(popupTarget.x, newVName);
				ignoreCaretMove = true;
				doc.remove(popupTarget.x, (popupTarget.y - popupTarget.x));
				ignoreCaretMove = true;
				doc.insertString(
					popupTarget.x,
					newVName,
					new SimpleAttributeSet());
				int neIndex = curStateModel.neExists(newVName, select);
				if (neIndex != -1) {
					if (JOptionPane.YES_OPTION
						== JOptionPane.showConfirmDialog(
							null,
							"Remove ne restriction clause",
							"GIPO Query",
							JOptionPane.YES_NO_OPTION)) {
						curStateModel.neList.removeElementAt(neIndex);
					}

				}
				// 		Utility.debugPrintln("Name = " + newVName);
				// redo highlights
				searcher.searchUnifiers(
					selectedUnifiers,
					searcher.selectedVar.x,
					searcher.selectedVar.y);
			} else {
			}

		} catch (Exception sme) {
			Utility.debugPrintln("Unexpected failure to select argument");
		}
	}

}

