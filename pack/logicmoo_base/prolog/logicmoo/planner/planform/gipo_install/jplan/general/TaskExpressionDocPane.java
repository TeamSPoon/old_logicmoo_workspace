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
 * TaskExpressionDocPane
 * This is a pane that contains an editable expression
 * that supports the editing of variables
 * it supports variable bindings, deleting predicates, 
 * 
 * @history updated for oclPlus add dialog to popup for instantiating fluent values
 * @author Weihong Zhao & Ron Simpson
 * @version 1
 */

import javax.swing.*;
import java.util.List;
import java.util.*;
import java.awt.event.*;
import java.awt.*;
import javax.swing.plaf.basic.BasicComboPopup;
import javax.swing.text.*;

import jplan.ocl.*;


/**
 * Extended from TransExpressionDocPane to allow mouse right click for the instantiation of the states.
 */
public class TaskExpressionDocPane extends TransExpressionDocPane {

    private BasicComboPopup popupMenuInstantiate; // THE Additional Task POPUP MENU
    int ArgNo = 0;
    private String objname = null;
    oclPredicate curPred = null;
    private int argStart = 0; //the position of the argument of a predicate
    private String oldV = null; //the argument to be replaced
    /**
     * the flag to show a predicate has been (not) deleted.
     */
    public boolean onceDeleted = false;
    protected JPopupMenu popupMenuAddDel;
	protected JPopupMenu popupMenuAddDelFluent; //Ron 2/7/03 needed for oclPlus

    /**
     * Creates an instance of the TaskExpressionDocPane.
     * @param cur the current domain being edited
     */
    public TaskExpressionDocPane(oclDomain cur) {
	super(cur);
	initAddDelPopup();
	initAddDelFluentPopup();
    }

    /**
     * Creates an instance of the default TaskExpressionDocPane.
     */
    public TaskExpressionDocPane() {
	super();
    }
    

    /**
     * watch for selections of variables and requests for popups
     * @param mouseEvent the mouse event triggering action
     * 
     */
    public void mouseWatch(MouseEvent mouseEvent) {
	Point viewPoint = mouseEvent.getPoint();
	clickPos = this.viewToModel(viewPoint);

	try {
	    curPred = curExpModel.getPredicateAt(clickPos);
	} catch (ExpressionModel.ExpressionException ee) {
	    return;
	}
	if (curPred.getName().equals("ne")) {
	    JOptionPane.showMessageDialog(this,
		   "You cannot directly edit an ne clause.",
		      "GIPO Error",
		      JOptionPane.WARNING_MESSAGE,
		   null);
	    return;
	}
	if(SwingUtilities.isRightMouseButton(mouseEvent)) {
	    Point selPoint = getSelectedVar();
	    if (selPoint.x <= clickPos && selPoint.y >= clickPos) {
		try {
		    String sortBranch = curExpModel.getSortForArgumentAt(clickPos);
		    Utility.debugPrintln("sortBranch: "+sortBranch);
		    Utility.debugPrintln("curPred: "+curPred);
		    Utility.debugPrintln("clickPos: "+clickPos);
		    ExpressionModel.PredDetail cur = curExpModel.getPredDetailAt(clickPos);
		    ArgNo = curPred.elementNoAt(clickPos - cur.startOffset);
		    Utility.debugPrintln("ArgNo: "+ArgNo);

		    getCurrentPredInfo(clickPos, ArgNo);

		    initInstantiatePopup(sortBranch);
		    // This is the currently selected variable
		    popupMenuInstantiate.show(mouseEvent.getComponent(),mouseEvent.getX(),  mouseEvent.getY());
		} catch (Exception e){Utility.debugPrintln(e);}
	    } else {
	    	oclPredicate curPred = null;
			try {
				curPred = curExpModel.getPredicateAt(clickPos);
			} catch (Exception e) {
				return;
			}
			if (curPred.isFluent()) {
				if (popupMenuAddDelFluent != null)
					popupMenuAddDelFluent.show(
						mouseEvent.getComponent(),
						mouseEvent.getX(),
						mouseEvent.getY());
			} else {
				if (popupMenuAddDel != null)
					popupMenuAddDel.show(
						mouseEvent.getComponent(),
						mouseEvent.getX(),
						mouseEvent.getY());
			}
	    }
	} else {
	    // See if we need to highlight variables
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
		arg = cur.pred.elementAt(pos - cur.startOffset);
		int startOffset = cur.startOffset +
		    cur.pred.startElementAt(pos - cur.startOffset);
		int n = cur.pred.elementNoAt(pos - cur.startOffset);
		selectedType = cur.proto.getNthElementName(n);
		List unifiers = curDomain.getSortUnifiers(selectedType);
		ListIterator li = unifiers.listIterator();
		selectedArg = arg;
		searcher.markSelected(startOffset,startOffset + arg.length());
		unifiers.add(selectedType);
		selectedUnifiers = unifiers;

		searcher.searchUnifiers(unifiers,startOffset,startOffset + arg.length());
		lastSelPred = cur;
		fireEvent(new ExpressionPaneEvent(this,
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
     * init task popup menu to create the edit target variable name.
     * @param sort the oclSort name
     * 
     */
    protected void initInstantiatePopup(String sort) {
	Vector items = new Vector();
	java.util.List objNames = curDomain.getObjectsOfSubTypes(sort);
	// Process the objects
	if (objNames != null) {
	    ListIterator liObj = objNames.listIterator();
	    while (liObj.hasNext()) {
		objname = (String)liObj.next();
		items.addElement(objname);
	    }
	}
	//build the PopupMenu
	JComboBox combo = new JComboBox(items);
	combo.addActionListener (new java.awt.event.ActionListener () {
	    public void actionPerformed (java.awt.event.ActionEvent evt) {
		try {
		    objname = (String)(popupMenuInstantiate.getList().getSelectedValue());

		    // Ron 28/8/01 Bug Fix to propigate offset changes in pane
		    Point selPoint = getSelectedVar();
// 		    curExpModel.editVar(selPoint.x,objname);
// 		    // Ron End
// 		    doc.remove(argStart,oldV.length());
// 		    doc.insertString(argStart, objname, new SimpleAttributeSet());
		    /* WZ 22/8/02 */
		    String select = curExpModel.getArgumentAt(selPoint.x);
		    //we should not allow users to change the objectID
		    if (select.equals(getObjectID())){
			JOptionPane.showMessageDialog(null,
					      "You are not allowed to changed the object ID.\n"+"Please select "+objname+" from sort tree view.",
					      "GIPO Warning",
					      JOptionPane.INFORMATION_MESSAGE,
					      null);
			return;
		    }
		    selPoint.x = replaceAllVariables(selPoint.x,select,
						     objname);
		    fireEvent(new 
			      ExpressionPaneEvent(this,
						  ExpressionPaneEvent.RENAME,
						  objname,
						  select,
						  getObjectID(),
						  ExpressionPaneEvent.TRANSITION));
		    /* end 22/8/02 */
		    // Ron 28/8/01 - remove the hi
		    removeHighlights();
		    popupMenuInstantiate.hide(); /* WZ 15/5/02 */
		} catch (Exception e) {
		    Utility.debugPrintln(e);
		}    
	    }
	}); 
	popupMenuInstantiate = new BasicComboPopup(combo); 
    }


    /**
     * create the add delete popup menu.
     * 
     */
    protected void initAddDelPopup() {    
	popupMenuAddDel = new JPopupMenu();
	JMenuItem delMI = new JMenuItem("Delete");
	delMI.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
		    // Delete the selected predicate
		    try {
			ExpressionModel.PredDetail curPredDetail = curExpModel.getPredDetailAt(clickPos);
			List  neList = curExpModel.removePredicateAt(clickPos);
			doc.remove(curPredDetail.startOffset,curPredDetail.length + 1);
			// redo highlights
			searcher.removeHighlights();

			onceDeleted = true;
		    } catch (Exception sme) {
			Utility.debugPrintln("Unexpected failure to delete predicate");
		    }
		}
	    }
				  );
	popupMenuAddDel.add(delMI);
    }
	// Ron 2/7/03    
	/**
	 * create the delete and Fluent instantiate popup menu.
	 * needed for oclPlus
	 * @return void
	 */
	protected void initAddDelFluentPopup() {
		popupMenuAddDelFluent = new JPopupMenu();
		JMenuItem delMI = new JMenuItem("Delete");
		delMI.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				// Delete the selected predicate
				try {
					ExpressionModel.PredDetail curPredDetail =
						curExpModel.getPredDetailAt(clickPos);
					List neList = curExpModel.removePredicateAt(clickPos);
					doc.remove(
						curPredDetail.startOffset,
						curPredDetail.length + 1);
					// redo highlights
					searcher.removeHighlights();

					onceDeleted = true;
				} catch (Exception sme) {
					Utility.debugPrintln(
						"Unexpected failure to delete predicate");
				}
			}
		});
		popupMenuAddDelFluent.add(delMI);
		JMenuItem instMI = new JMenuItem("Set functor value");
		instMI.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				// get Double value for functor
				try {
					oclPredicate curPred = curExpModel.getPredicateAt(clickPos);
					curPred.setFluentValue(0.0);
					String val =
						GipoInputBox.showDecimalInputBox(
							null,
							"Functor Value",
							"Enter decimal value");
					try {
						double dval = Double.parseDouble(val);
						curPred.setFluentValue(dval);
					} catch (Exception e) {
						JOptionPane.showMessageDialog(
							null,
							"Illegal number format.",
							"GIPO Error",
							JOptionPane.ERROR_MESSAGE,
							null);
					}
					// redo highlights
					searcher.removeHighlights();

				} catch (Exception sme) {
					Utility.debugPrintln(
						"Unexpected failure to set functor value");
				}
			}
		});
		popupMenuAddDelFluent.add(instMI);
	}

    /**
     * hold enough information for the current predicate which are dealt with.
     * @param clickPoint the position of the variable which has been clicked
     * @param argNo the order number of the variable which has been clicked
     * 
     */
    private void getCurrentPredInfo(int clickPoint, int argNo){
	try {
	    ExpressionModel.PredDetail curPredDetail = curExpModel.getPredDetailAt(clickPoint);
	    int offset = curPredDetail.pred.startElementNo(argNo+1);
	    argStart = curPredDetail.startOffset + offset;
	    oldV = curExpModel.getArgumentAt(clickPoint);
	} catch (Exception e) {
	    Utility.debugPrintln("Unexpected failure to edit variable" + e);
	}
    }

    /**
     * clearPane then set 
     * @throws BadLocationException
     * 
     */
    public void clearPane()throws BadLocationException {
	super.clearPane();
	onceDeleted = false;
    }
}
