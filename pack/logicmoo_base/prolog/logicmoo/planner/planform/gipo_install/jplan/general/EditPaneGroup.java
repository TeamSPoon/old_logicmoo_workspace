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
 * EditPaneGroup
 * The function of this class is to control a list of TransExpressionPane(s)
 * to facititate editing across multiple panes.
 * @author Ron Simpson
 * @version 0
 */

import javax.swing.*;
import java.util.List;
import java.util.*;

public class EditPaneGroup {
	List paneList; //The pane group stored as a List
	int changePane; //The ID of the pane currently selected
	String selectedSort = "none"; //The name of the sort selected in the
	// currently selected pane
	String selectedVarName = "none"; //The name of the variable selected in the
	// currently selected pane
	List selectedUnifiers = null; //The matching sorts to the selected sort in
	// currently selected pane

	/**
	 * No Param Constructure expression group to be incrementally built up
	 */
	public EditPaneGroup() {
		paneList = new ArrayList();
		changePane = -1;
	}

	/**
	 * addPane
	 * incrementally add a pane to the group
	 * set up the panes event listener
	 * @param fedp
	 */
	public void addPane(TransExpressionDocPane fedp) {
		paneList.add(fedp);
		fedp.setID(paneList.size() - 1);
		fedp.addChangeListener(new ExpressionPaneListener() {
			public void getChange(ExpressionPaneEvent evt) {
				if (evt.getID() == ExpressionPaneEvent.SELECTION) {
					TransExpressionDocPane sel =
						(TransExpressionDocPane) evt.getSource();
					changePane = sel.getID();
					selectedVarName = evt.getVName();
					selectedSort = sel.getSelectedSort();
					selectedUnifiers = sel.getSelectedUnifiers();
					highlightGroupUnifiers();
				} else if (evt.getID() == ExpressionPaneEvent.CLEAR) {
					TransExpressionDocPane sel =
						(TransExpressionDocPane) evt.getSource();
					changePane = sel.getID();
					removeAllHighlights();
				} else if (evt.getID() == ExpressionPaneEvent.RENAME) {
					TransExpressionDocPane sel =
						(TransExpressionDocPane) evt.getSource();
					changePane = sel.getID();
					String vName = evt.getVName();
					String oldVName = evt.getOldVName();
					String transObjID = evt.getTransObjID();
					int scope = evt.getScope();
					renameGroup(scope, oldVName, vName, transObjID);
				}
			}
		});
	}

	/**
	 * addPane
	 * incrementally add a pane to the group
	 * set up the panes event listener
	 * @param fedp
	 */
	public void addPane(MethodVarPane fedp) {
		paneList.add(fedp);
		fedp.setID(paneList.size() - 1);
		fedp.addChangeListener(new ExpressionPaneListener() {
			public void getChange(ExpressionPaneEvent evt) {
				if (evt.getID() == ExpressionPaneEvent.SELECTION) {
					MethodVarPane sel = (MethodVarPane) evt.getSource();
					changePane = sel.getID();
					selectedVarName = evt.getVName();
					selectedSort = sel.getSelectedSort();
					selectedUnifiers = sel.getSelectedUnifiers();
					highlightGroupUnifiers();
				} else if (evt.getID() == ExpressionPaneEvent.CLEAR) {
					MethodVarPane sel = (MethodVarPane) evt.getSource();
					changePane = sel.getID();
					removeAllHighlights();
				} else if (evt.getID() == ExpressionPaneEvent.RENAME) {
					MethodVarPane sel = (MethodVarPane) evt.getSource();
					changePane = sel.getID();
					String vName = evt.getVName();
					String oldVName = evt.getOldVName();
					String transObjID = evt.getTransObjID();
					int scope = evt.getScope();
					renameGroup(scope, oldVName, vName, transObjID);
				}
			}
		});
	}

	/**
	 * addPane
	 * incrementally add a StateExpressionDocPane to the group
	 * set up the panes event listener
	 * @param fedp
	 */
	public void addPane(StateExpressionDocPane fedp) {
		paneList.add(fedp);
		fedp.setID(paneList.size() - 1);
		fedp.addChangeListener(new ExpressionPaneListener() {
			public void getChange(ExpressionPaneEvent evt) {
				if (evt.getID() == ExpressionPaneEvent.SELECTION) {
					StateExpressionDocPane sel =
						(StateExpressionDocPane) evt.getSource();
					changePane = sel.getID();
					selectedVarName = evt.getVName();
					selectedSort = sel.getSelectedSort();
					selectedUnifiers = sel.getSelectedUnifiers();
					highlightGroupUnifiers();
				} else if (evt.getID() == ExpressionPaneEvent.CLEAR) {
					StateExpressionDocPane sel =
						(StateExpressionDocPane) evt.getSource();
					changePane = sel.getID();
					removeAllHighlights();
				}
			}
		});
	}

	// 20/5/02
//	/**
//	 * addPane
//	 * incrementally add a PatternStateExpressionDocPane to the group
//	 * set up the panes event listener
//	 * @param fedp
//	 */
//	public void addPane(PatternStateExpressionDocPane fedp) {
//		paneList.add(fedp);
//		fedp.setID(paneList.size() - 1);
//		fedp.addChangeListener(new ExpressionPaneListener() {
//			public void getChange(ExpressionPaneEvent evt) {
//				if (evt.getID() == ExpressionPaneEvent.SELECTION) {
//					PatternStateExpressionDocPane sel =
//						(PatternStateExpressionDocPane) evt.getSource();
//					changePane = sel.getID();
//					selectedVarName = evt.getVName();
//					selectedSort = sel.getSelectedSort();
//					selectedUnifiers = sel.getSelectedUnifiers();
//					highlightGroupUnifiers();
//				} else if (evt.getID() == ExpressionPaneEvent.EXPORT) {
//					PatternStateExpressionDocPane sel =
//						(PatternStateExpressionDocPane) evt.getSource();
//					changePane = sel.getID();
//					selectedVarName = evt.getVName();
//					selectedSort = evt.getVSort();
//					exportVarToMajor(selectedVarName, selectedSort);
//				} else if (evt.getID() == ExpressionPaneEvent.DELETE) {
//					PatternStateExpressionDocPane sel =
//						(PatternStateExpressionDocPane) evt.getSource();
//					changePane = sel.getID();
//					selectedVarName = evt.getVName();
//					selectedSort = evt.getVSort();
//					deleteVarInMajor(selectedVarName, selectedSort);
//					removeAllHighlights();
//				} else if (evt.getID() == ExpressionPaneEvent.CLEAR) {
//					PatternStateExpressionDocPane sel =
//						(PatternStateExpressionDocPane) evt.getSource();
//					changePane = sel.getID();
//					removeAllHighlights();
//				}
//			}
//		});
//	}

//	/**
//	 * exportVarToMajor
//	 * add the exported variable to the state selected in the MAJOR pane
//	 * @param var - the selected variable
//	 * @param varSort - the selected variable's sort
//	 */
//	private void exportVarToMajor(String var, String varSort) {
//		PatternStateExpressionDocPane major = null;
//		ListIterator li = paneList.listIterator();
//		boolean found = false;
//		while (!found && li.hasNext()) {
//			major = (PatternStateExpressionDocPane) li.next();
//			if (major.getStatus() == PatternStateExpressionDocPane.MAJOR)
//				found = true;
//		}
//		major.importVariable(var, varSort);
//	}
//
//	/**
//	 * deleteVarInMajor
//	 * if selected variable has been exported then delete it
//	 * @param var - the selected variable
//	 * @param varSort - the selected variable's sort
//	 */
//	private void deleteVarInMajor(String var, String varSort) {
//		PatternStateExpressionDocPane major = null;
//		ListIterator li = paneList.listIterator();
//		boolean found = false;
//		while (!found && li.hasNext()) {
//			major = (PatternStateExpressionDocPane) li.next();
//			if (major.getStatus() == PatternStateExpressionDocPane.MAJOR)
//				found = true;
//		}
//		major.deleteVariable(var, varSort);
//	}

	/**
	 * removePane
	 * mark a pane as deleted in the pane group
	 * @param fedp - the pane to be removed
	 */
	public void removePane(TransExpressionDocPane fedp) {
		paneList.set(fedp.getID(), null);
	}

	/**
	 * renameGroup
	 * rename variables in the edit group
	 */
	// Ron New 21/8/01
	// Ron 10/10/02 NOTE could cycle round the pane group querying about variables 
	// used esp IDs
	public void renameGroup(
		int scope,
		String oldVName,
		String newVName,
		String transID) {
		int Changes = 0;
		for (int i = 0; i < paneList.size(); i++) {
			if (i != changePane) {
				TransExpressionDocPane cur =
					(TransExpressionDocPane) paneList.get(i);
				if (cur != null) {
					if (scope == ExpressionPaneEvent.TRANSITION
						&& cur.getObjectID().equals(transID)) {
						Utility.debugPrintln("Renaming Transition " + oldVName 
						+ " " + newVName + " transID is " + transID );
						cur.replaceAllVariables(0, oldVName, newVName);
						if (oldVName.equals(transID)) {
							cur.setObjectID(newVName);
						}
					} else if (scope == ExpressionPaneEvent.GLOBAL) {

						if (oldVName.equals(transID)) {
							cur.setObjectID(newVName);
						}
						cur.replaceAllVariables(0, oldVName, newVName);
					}
					cur.removeHighlights();
				}
			} else {
				TransExpressionDocPane cur =
					(TransExpressionDocPane) paneList.get(i);
				if (oldVName.equals(transID)) {
					cur.setObjectID(newVName);
				}
			}
		}
	}

	/**
	 * removeAllHighlights
	 * remove all highlights from all registered panes
	 */
	// Ron New 21/8/01
	/* WZ 8/5/02 added more options */
	public void removeAllHighlights() {
		for (int i = 0; i < paneList.size(); i++) {
			if (i != changePane) {
				Object cur = (Object) paneList.get(i);
				if (cur.getClass().getName()
					== "jplan.general.TransExpressionDocPane"
					&& cur != null) {
					TransExpressionDocPane curT = (TransExpressionDocPane) cur;
					curT.removeHighlights();
				} else if (
					cur.getClass().getName() == "jplan.general.MethodVarPane"
						&& cur != null) {
					MethodVarPane curM = (MethodVarPane) cur;
					curM.removeHighlights();
				} 
//				else if (
//					cur.getClass().getName()
//						== "jplan.general.PatternStateExpressionDocPane"
//						&& cur != null) {
//					PatternStateExpressionDocPane curM =
//						(PatternStateExpressionDocPane) cur;
//					curM.removeHighlights();
//				}
			}
		}
	}

	/**
	 * highlightGroupUnifiers
	 * Cycle round the group and highlight all the group except the
	 * pane generating the selection event
	 */
	public void highlightGroupUnifiers() {
		int noPanes = paneList.size();
		for (int i = 0; i < noPanes; i++) {
			if (i != changePane) {
				Object cur = (Object) paneList.get(i);
				if (cur.getClass().getName()
					== "jplan.general.TransExpressionDocPane"
					&& cur != null) {
					TransExpressionDocPane tedp =
						(TransExpressionDocPane) paneList.get(i);
					tedp.highlightForeignUnifiers(
						selectedUnifiers,
						selectedSort,
						selectedVarName);
					/* Weihong added on 10/5/2001 */
					tedp.repaint();
				} else if (
					cur.getClass().getName() == "jplan.general.MethodVarPane"
						&& cur != null) {
					MethodVarPane tedp = (MethodVarPane) paneList.get(i);
					tedp.highlightForeignUnifiers(
						selectedUnifiers,
						selectedSort,
						selectedVarName);
					/* Weihong added on 10/5/2001 */
					tedp.repaint();
				} else if (
					cur.getClass().getName()
						== "jplan.general.StateExpressionDocPane"
						&& cur != null) {
					StateExpressionDocPane sedp =
						(StateExpressionDocPane) paneList.get(i);
					sedp.highlightForeignUnifiers(
						selectedUnifiers,
						selectedSort,
						selectedVarName);
					/* Weihong added on 10/5/2001 */
					sedp.repaint();
//				} else if (
//					cur.getClass().getName()
//						== "jplan.general.PatternStateExpressionDocPane"
//						&& cur != null) {
//					PatternStateExpressionDocPane sedp =
//						(PatternStateExpressionDocPane) paneList.get(i);
//					sedp.highlightForeignUnifiers(
//						selectedUnifiers,
//						selectedSort,
//						selectedVarName);
//
//					sedp.repaint();
				} else if (
					cur.getClass().getName()
						== "jplan.general.HierarchicalStateEXDocPane"
						&& cur != null) {
					HierarchicalStateEXDocPane sedp =
						(HierarchicalStateEXDocPane) paneList.get(i);
					sedp.highlightForeignUnifiers(
						selectedUnifiers,
						selectedSort,
						selectedVarName);
					/* Weihong added on 10/5/2001 */
					sedp.repaint();
				}
			}
		}
	}
}
