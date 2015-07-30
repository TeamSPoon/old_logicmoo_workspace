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

package jplan.tools.stepper;

/**
 * HStepperCanvas.java
 * 20/5/02
 * @author Weihong Zhao
*/

import java.awt.event.*;
import java.util.List;
import java.util.ArrayList;
import java.util.ListIterator;
import java.util.Vector;
import javax.swing.*;
import java.util.Hashtable;
import java.awt.*;

import jplan.graphics.gTool.Graphics.vShape;
import jplan.graphics.gTool.Graphics.vLink;
import jplan.graphics.gTool.Graphics.LinkPoint;
import jplan.ocl.*;
import jplan.graphics.*;
import jplan.general.Utility;
import jplan.general.GipoInternalFrame;
import jplan.general.PositionArray; /* WZ 21/5/02 */
import jplan.general.OEnviron; /* Ron 8/4/03 */
import jplan.tools.animator.OperatorProperty; /* WZ 7/6/02 */
import java.io.BufferedReader; /* WZ 19/6/02 */
import java.util.StringTokenizer; /* WZ 19/6/02 */
import java.io.*; /* WZ 19/6/02 */
import jplan.general.GipoSelectionBox; /* WZ 21/6/02 */

/**
 * The Hierarchical StepperCanvas is generally a graphic displaying canvas 
 * where actions (operator/method) are displayed step by step.
 * <br> Basic mouse interactive functions includes mouse click for viewing/editing.
 */
public class HStepperCanvas extends JGraphCanvas {
	private GipoInternalFrame parent = null;
	private oclDomain workingDomain;
	private vShape curShape; //current global shape which is selected
	private List curState = new ArrayList();
	// current substates of all objects - a collection of oclSS.
	private PositionArray positionRecord;
	//store the position of next possible shape
	/* WZ 21/5/02 */
	private int rowHeight;
	private int columnWidth;
	private int xOffset;
	private int yOffset;
	/* end 21/5/02 */

	/* WZ 22/5/02 */
	private vShape taskShape; // the original shape
	private List curDecompList; //the current decomposition list
	/* end 22/5/02 */

	/* WZ 27/5/02 */
	private oclSS subGoal = null; //any achieves
	/* WZ 28/5/02 */
	private vShape subGoalShape = null; //any achieves
	/* WZ 28/5/02 */
	private boolean onceInstantiated = false;
	//flag to indicate subGoal is once instantiated manually.

	/* WZ 6/6/02 */
	private JPopupMenu popupMenu; //for displaying the general functions
	/* WZ 7/6/02 */
	private vShape popupShape;
	/* WZ 10/6/02 */
	private int recordSize = 1;
	private int curShapeID[] = new int[recordSize];
	private int subgoalShapeID[] = new int[recordSize];
	private List recordDecompList[] = new ArrayList[recordSize];
	private PositionArray recordPosition[] = new PositionArray[recordSize];
	;

	boolean animatorFlag = false;
	/* WZ 21/8/02 to indicate if this cavnas is currently used by an hierarchical animator */

	/**
	 * Creates a canvas allowing drawing objects' state (vShape) in batch,
	 * and support mouse click for viewing/editing, mouse drag for relayout.
	 * @param parent StepperWindow
	 */
	public HStepperCanvas(GipoInternalFrame parent) {
		super();
		this.parent = parent;

		//current default width and height.
		d_Width = 80;
		d_Height = 30;
		mouseAction = SELECT;
		removeMouseMotionListener(this);

		rowHeight = 100;
		columnWidth = 120;
		xOffset = 40;
		yOffset = 20;

		positionRecord =
			new PositionArray(columnWidth, rowHeight, xOffset, yOffset);
	}

	/* WZ 21/8/02 */
	/**
	   * Get the value of animatorFlag.
	   * @return Value of animatorFlag.
	   */
	public boolean getAnimatorFlag() {
		return animatorFlag;
	}
	/* WZ 21/8/02 */
	/**
	   * Set the value of animatorFlag.
	   * @param v  Value to assign to animatorFlag.
	   */
	public void setAnimatorFlag(boolean v) {
		this.animatorFlag = v;
	}

	/**
	 * Sets ocl domain 
	 * @param cur ocl domain 
	 * 
	 */
	public void setWorkingDomain(oclDomain cur) {
		workingDomain = cur;
	}

	/**
	 * returns ocl domain currently opened
	 * @return ocl domain currently opened
	 */
	public oclDomain getWorkingDomain() {
		return workingDomain;
	}

	/* WZ 24/5/02 */
	/**
	 * Set current states
	 * @param currentStates a list of oclSS
	 * 
	 */
	public void setCurrentState(List currentStates) {
		ListIterator li = currentStates.listIterator();
		while (li.hasNext()) {
			oclSS ss = (oclSS) li.next();
			try {
				curState.add((oclSS) ss.clone());
			} catch (CloneNotSupportedException e) {
				Utility.debugPrintln(e);
			}
		}
	}

	/**
	   * Get the value of parent.
	   * @return Value of parent.
	   */
	public GipoInternalFrame getTheParent() {
		return parent;
	}

	/**
	   * Set the value of parent.
	   * @param v  Value to assign to parent.
	   */
	public void setTheParent(GipoInternalFrame v) {
		this.parent = v;
	}

	/* WZ 6/6/02 */
	/**
	 * dobule click to show the property
	 * @param me MouseEvent
	 * 
	 */
	public void mouseClicked(MouseEvent me) {
		if (me.getClickCount() == 1
			&& me.getModifiers() == MouseEvent.BUTTON3_MASK) {
			//check shapelist to see if there at least one shape was highlighted.
			for (int i = 1; i < vShapeList.size() + 1; i++) {
				vShape vs = (vShape) getShape(i);
				if (vs.contains((double) me.getX(), (double) me.getY())) {
					if (vs.equals(taskShape)) { /* WZ 7/6/02 */

					} else {
						popupShape = vs;
						initPopupMenu();
						popupMenu.show(this, me.getX(), me.getY());
					}
					return;
				}
			}
		}
	}

	/**
	 * initialise the popup menu to show all functions
	 * 
	 */
	public void initPopupMenu() {
		HSShapeProperty hsProperty = (HSShapeProperty) popupShape.getObject();
		if (hsProperty == null)
			return;

		Utility.debugPrintln("stepper","HStepperCanvas Initiate popup menu ...");
		popupMenu = new JPopupMenu("Functions ...");

		if (hsProperty.getObject() == null)
			return;

		JMenuItem MI = new JMenuItem("Property");
		MI.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				HSShapeProperty hsProperty =
					(HSShapeProperty) popupShape.getObject();
				switch (popupShape.getShapeID()) {
					case 3 : //if oclSS, check against curStates
						ObjectProperty PropWindow =
							new ObjectProperty(
								parent.getTheParent(),
								(List) ((oclSS) hsProperty.getObject())
									.getState(),
								((oclSS) hsProperty.getObject()).getName());
						PropWindow.setLocation(
							(int) (0.5 * getWidth()),
							(int) (0.5 * getHeight()));
						PropWindow.show();
						break;
					case 0 : //if oclOperator, popup for instantiation, then run it
						OperatorProperty pw =
							new OperatorProperty(
								workingDomain,
								(oclOperator) hsProperty.getObject(),
								parent,
								parent.getTheParent().strImageDir);
						pw.setLocation(
							(int) (0.5 * getWidth()),
							(int) (0.5 * getHeight()));
						pw.show();
						break;
					case 5 : //if oclMethod, show its decomposition, repeat
						MethodInstantiation.showProperty(
							workingDomain,
							(oclMethod) hsProperty.getObject(),
							HStepperCanvas.this);
						break;
				}
			}
		});
		popupMenu.add(MI);

		if (animatorFlag) /* WZ 21/8/02 */
			return;

		popupMenu.addSeparator();

		MI = new JMenuItem("States before & after action ...");
		MI.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				HSShapeProperty hsProperty =
					(HSShapeProperty) popupShape.getObject();
				StateProperty.showProperty(
					parent.getTheParent(),
					hsProperty.getPreState(),
					hsProperty.getPostState());
			}
		});
		popupMenu.add(MI);

		// 	if (hsProperty.getObject().getClass().getName().equals("jplan.ocl.oclMethod")){
		// 	    popupMenu.addSeparator();

		// 	    MI = new JMenuItem("Hide decomposition");
		// 	    MI.addActionListener(new java.awt.event.ActionListener() {
		// 		public void actionPerformed(ActionEvent e) {

		// 		}
		// 	    });
		// 	    popupMenu.add(MI);
		// 	}

	}

	/**
	 * to check if there are some states
	 * that match the transition's left hand side condition 
	 * @param om oclMethod
	 * @param mssgs the message list to populate with errors
	 * @return boolean
	 */
	private boolean findStates(oclMethod om, List mssgs) {
		ListIterator li;
		//check precoditons - oclSE
		oclSE se = null;
		oclSC sc = null;
		li = om.getPrecondition().listIterator();
		while (li.hasNext()) {
			se = (oclSE) li.next();
			mssgs.add(
				"Finding state for preconditional object " + se.getName());
			if (!StepperCanvas
				.unifiesCurrentState(
					workingDomain,
					curState,
					se.getName(),
					se.getState(),
					mssgs)) {
				Utility.debugPrintln(
					"OBJECT "
						+ se.getName()
						+ " FAILED to unify state with precondition.");
				return false;
			}
		}
		//Check index - oclSC
		li = om.getIndex().listIterator();
		while (li.hasNext()) {
			sc = (oclSC) li.next();
			mssgs.add("Finding state for index transition for " + sc.getName());
			if (!StepperCanvas
				.unifiesCurrentState(
					workingDomain,
					curState,
					sc.getName(),
					sc.getPre(),
					mssgs)
				|| !StepperCanvas.checkStaticsAndBuiltIns(
					workingDomain,
					sc.getPost(),
					mssgs)) {
				Utility.debugPrintln(
					"OBJECT "
						+ sc.getName()
						+ " FAILED to unify state with index transition.");
				return false;
			}
		}

		//check all statics against atomic invariants
		return checkStatics(om.getStatics()); /* WZ 30/5/02 */
		// 	//build untrue list
		// 	List untrueList = checkStatics(om.getStatics());

		// 	//find un-instantiated variables in statics
		// 	li = untrueList.listIterator();
		// 	oclPredicate uninstSig = new oclPredicate();
		// 	while (li.hasNext()) {
		// 	    oclPredicate staticPrd = (oclPredicate)li.next();
		// 	    if (staticPrd.isInstantiated())
		// 		workingDomain.addSignatureArgument(uninstSig, staticPrd);
		// 	    else //if this state is not true after run checkStatics once
		// 		return fasle;
		// 	}
		// 	//if there are some uninstantiated variables
		// 	if (uninstSig.getArguments.size() > 0){
		// 	    ListIterator liUninst = uninstSig.getArguments.listIterator();
		// 	    while (li.hasNext()){

		// 	    }
		// 	}
		// 	else 
		// 	    return true;

		// 	return true;
	}

	/* WZ 29/5/02 */
	/**
	 * To eliminate true states and return false states
	 * @param undoneList list to check
	 * @return return true if all instantiated predicates are true
	 */
	private boolean checkStatics(List undoneList) {
		// 	List retList = new ArrayList();
		List mssgs = new ArrayList(); /* WZ 30/5/02 */
		boolean found = false;
		ListIterator li = undoneList.listIterator();
		while (li.hasNext()) {
			oclPredicate staticPrd = (oclPredicate) li.next();
			mssgs.add(
				"Finding state for static predicate for "
					+ staticPrd.getName());
			if (staticPrd.isInstantiated()) { /* WZ 30/5/02 */
				ListIterator atomicList =
					workingDomain.atomicInvars.listIterator();
				while (atomicList.hasNext()) { //for every predicate in the SE
					oclPredicate basePred = (oclPredicate) atomicList.next();
					if (staticPrd.getName().equals("ne")) {
						//if this is a "ne" clause
						if (!(staticPrd.getNthElementName(0))
							.equals(basePred.getNthElementName(1))) {
							found = true;
							break;
						}
					} else if (
						staticPrd.getName().equals("is_of_sort")
							|| staticPrd.getName().equals(
								"is_of_primitive_sort")) {
						//do not check them for the moment
						found = true;
						break;
					} else if (staticPrd.equals(basePred)) {
						found = true;
						break;
					}
				}
			} else { //if not instantiated then ignore it for the moment
				found = true;
			}
			if (!found) {
				// 		retList.add(staticPrd);
				return false;
			} else
				found = false; //reset found for next iteration
		}
		// 	return retList;
		return true;
	}

	/**
	 * to check if there are some states
	 * that match the transition's left hand side condition 
	 * @param op oclOperator
	 * @param mssgs the message list to populate with errors
	 * @return boolean
	 */
	private boolean findStates(oclOperator op, List mssgs) {
		ListIterator li;
		//oclSE
		oclSE se = null;
		oclSC sc = null;
		li = op.getPrevail().listIterator();
		while (li.hasNext()) {
			se = (oclSE) li.next();
			mssgs.add("Finding state for prevailing object " + se.getName());
			if (!StepperCanvas
				.unifiesCurrentState(
					workingDomain,
					curState,
					se.getName(),
					se.getState(),
					mssgs)) {
				Utility.debugPrintln(
					"OBJECT "
						+ se.getName()
						+ " FAILED to unify state with prevail");
				return false;
			}
		}
		//oclSC 
		li = op.getNecessary().listIterator();
		while (li.hasNext()) {
			sc = (oclSC) li.next();
			mssgs.add(
				"Finding state for necessary object transition for "
					+ sc.getName());
			if (!StepperCanvas
				.unifiesCurrentState(
					workingDomain,
					curState,
					sc.getName(),
					sc.getPre(),
					mssgs)
				|| !StepperCanvas.checkStaticsAndBuiltIns(
					workingDomain,
					sc.getPost(),
					mssgs)) {
				Utility.debugPrintln(
					"OBJECT "
						+ sc.getName()
						+ " FAILED to unify state with necessary transition");
				return false;
			}
		}
		return true;
	}

	/**
	 * undo the last recorded action
	 * 
	 */
	public void undo() {
		boolean inDecompList = false; /* WZ 11/6/02 */
		super.undo();
		positionRecord = recordPosition[index_Edit]; /* WZ 10/6/02 */
		Utility.debugPrintln(
			"recordPosition"
				+ index_Edit
				+ ": \n"
				+ recordPosition[index_Edit].toString());

		curShape = shape[curShapeID[index_Edit]];
		focus(curShape); /* WZ 11/6/02 */
		Utility.debugPrintln("stepper","HStepperCanvas curShape after undo: " + curShape.getLabel());
		//there should be only one shape is selected.
		curDecompList = recordDecompList[index_Edit];
		//update vshapes in the list
		ListIterator li = curDecompList.listIterator();
		while (li.hasNext()) {
			vShape vs = (vShape) li.next();
			Utility.debugPrintln("stepper","HStepperCanvas curDecompList member: " + vs.getLabel());
			vs.removeTextField(); /* WZ 11/6/02 */
			shape[vs.getID()] = vs;

			//if current shape in the decomp list
			if (vs.getID() == curShape.getID()) /* WZ 11/6/02 */
				inDecompList = true;
		}

		// 	focus(curShape);
		HSShapeProperty hsProperty = (HSShapeProperty) curShape.getObject();
		if (inDecompList) { /* WZ 11/6/02 */
			hsProperty.setObject(null);
			hsProperty.getPostState().clear();

			//change the current shape's outlinks back to blue
			for (int i = 1; i < curShape.getOutLinks().size() + 1; i++) {
				vLink tmpLink =
					(vLink) curShape.getOutLinks().get("outlinks" + i);
				if (tmpLink.getType() == 2) //if it is green line
					tmpLink.setType(0);
			}
		} else { /* WZ set all decomp's object to null 11/6/02 */
			ListIterator lidecomp = curDecompList.listIterator();
			while (lidecomp.hasNext()) {
				vShape vs = (vShape) lidecomp.next();
				((HSShapeProperty) vs.getObject()).setObject(null);
			}
		}

		//restor current state
		curState.clear();
		ListIterator liCurState = hsProperty.getPreState().listIterator();
		while (liCurState.hasNext()) {
			oclSS ss = (oclSS) liCurState.next();
			try {
				curState.add((oclSS) ss.clone());
			} catch (CloneNotSupportedException e) {
				Utility.debugPrintln(e);
			}
		}
		((HStepperWindow) parent).updateCurState(curState); /* WZ 11/6/02 */

		/* WZ 11/6/02 */
		//restore sub goal
		if (subgoalShapeID[index_Edit] == 0) {
			subGoalShape = null;
			subGoal = null;
			onceInstantiated = false;
		} else {
			subGoalShape = shape[subgoalShapeID[index_Edit]];
			subGoal =
				(oclSS) ((HSShapeProperty) subGoalShape.getObject())
					.getObject();
		}
		repaint();
	}

	/**
	 * For undo and redo - to store the state.<br>
	 * Any actions considered as valuable for later undo/redo process
	 * must be recorded by calling this method immediately after the action was processed.
	 * 
	 */
	public void recordState() {
		super.recordState();
		Utility.debugPrintln("stepper","HStepperCanvas index_Edit  " + index_Edit);
		if (index_Edit > (recordSize - 1)) {
			PositionArray[] copyPosition = new PositionArray[recordSize];
			for (int i = 0; i < recordSize; i++) {
				copyPosition[i] = recordPosition[i];
			}

			int[] copyCurShape = new int[recordSize];
			merge(recordSize, curShapeID, copyCurShape);

			List[] copyDecompList = new ArrayList[recordSize];
			for (int i = 0; i < recordSize; i++) {
				copyDecompList[i] = recordDecompList[i];
			}

			int[] copySubgoalShape = new int[recordSize];
			merge(recordSize, subgoalShapeID, copySubgoalShape);

			//create new array with size increase by 1
			recordSize++;
			curShapeID = new int[recordSize];
			subgoalShapeID = new int[recordSize];
			recordDecompList = new ArrayList[recordSize];
			recordPosition = new PositionArray[recordSize];

			//take existing data
			for (int i = 0; i < recordSize - 1; i++) {
				recordPosition[i] = copyPosition[i];
			}
			merge(recordSize - 1, copyCurShape, curShapeID);
			for (int i = 0; i < recordSize - 1; i++) {
				recordDecompList[i] = copyDecompList[i];
			}
			merge(recordSize - 1, copySubgoalShape, subgoalShapeID);
		}
		curShapeID[index_Edit] = curShape.getID();
		Utility.debugPrintln("stepper","HStepperCanvas current shape:  " + curShape.getLabel());

		recordDecompList[index_Edit] = new ArrayList();
		ListIterator li = curDecompList.listIterator();
		while (li.hasNext()) {
			vShape vs = (vShape) li.next();
			vShape vsp = (vShape) vs.clone();
			recordDecompList[index_Edit].add(vsp);
			Utility.debugPrintln("stepper","HStepperCanvas add to decomp list:  " + vsp.getLabel());
		}

		if (subGoalShape != null) {
			subgoalShapeID[index_Edit] = subGoalShape.getID();
			Utility.debugPrintln("stepper","HStepperCanvas subGoalShape:  " + subGoalShape.getLabel());
		} else {
			subgoalShapeID[index_Edit] = 0; //no subgoal
			Utility.debugPrintln("stepper","HStepperCanvas NO subGoalShape.");
		}

		recordPosition[index_Edit] = (PositionArray) positionRecord.clone();
		/* WZ 10/6/02 */
		Utility.debugPrintln(
			"When recording, recordPosition"
				+ index_Edit
				+ ": \n"
				+ recordPosition[index_Edit].toString());
	}

	private void merge(int size, int[] oldArray, int[] newArray) {
		for (int i = 0; i < size; i++) {
			newArray[i] = oldArray[i];
		}
	}

	/**
	 * return a list of current actions
	 * @return String
	 */
	public List getCurActionList() {
		List returnList = new ArrayList();
		traceOperators(returnList, taskShape); /* WZ 27/6/02 */
		return returnList;
	}

	/* WZ 27/6/02 */
	private void traceOperators(List returnList, vShape vs) {
		HSShapeProperty hsProperty = (HSShapeProperty) vs.getObject();
		ListIterator li = hsProperty.getDecompList().listIterator();
		while (li.hasNext()) {
			vShape tmpSP = (vShape) li.next();
			HSShapeProperty tmpPTY = (HSShapeProperty) tmpSP.getObject();
			if (tmpPTY.getObject() == null)
				return;

			if (tmpPTY
				.getObject()
				.getClass()
				.getName()
				.equals("jplan.ocl.oclOperator"))
				returnList.add(((oclOperator) tmpPTY.getObject()).opName);
			else
				traceOperators(returnList, tmpSP);
		}
	}

	/* WZ 14/6/02 */
	/**
	 * return a list of all steps
	 * @return String
	 */
	public List getStepsList() {
		List returnList = new ArrayList();
		for (int i = 1; i < vShapeList.size() + 1; i++) {
			vShape vs = (vShape) getShape(i);
			HSShapeProperty hsProperty = (HSShapeProperty) vs.getObject();
			if (hsProperty.getObject() != null) {
				if (vs.getShapeID() == 0) //operator
					returnList.add(
						((oclOperator) hsProperty.getObject()).opName);
				if (vs.getShapeID() == 5) //method
					returnList.add(
						((oclMethod) hsProperty.getObject()).getName());
				else
					returnList.add(((oclSS) hsProperty.getObject()));
			}
		}
		return returnList;
	}

	/*
	 * redo the last undo action
	 * 
	 */
	public void redo() {
		// 	super.redo();
	}

	public vShape drawTaskShape(oclHTNTask task) {
		//build an oclMethod first
		oclMethod curOMD = new oclMethod();
		//set decompositions
		List decompList = task.getGoals();
		curOMD.setDecomps(decompList);
		//to build temporal clause
		curOMD.setTemps(task.constraints);
		//to build statics
		curOMD.setStatic(task.statics);

		int curRow = positionRecord.addPosition();
		positionRecord.increase(curRow);
		int x = positionRecord.getX(curRow);
		int y = positionRecord.getY(curRow);
		HSShapeProperty hsProperty =
			new HSShapeProperty(
				curOMD,
				curRow,
				positionRecord.getColumn(curRow));

		hsProperty.setObject(curOMD);
		/* WZ 22/5/02 Only shapes with object not null can be expanded. */
		setDrawingShapeID(5);
		taskShape = createVShape(x, y);
		taskShape.removeTextField();
		taskShape.setLabel(task.toString());
		taskShape.setObject(hsProperty);
		hsProperty.setOwnerShapeIndex(taskShape.getID()); /* WZ 18/6/02 */
		curShape = taskShape; /* WZ 22/5/02 */
		if (parent
			.getClass()
			.getName()
			.equals("jplan.tools.stepper.HStepperWindow"))
			focus(curShape); /* WZ 6/6/02 */

		return taskShape;
	}

	/* WZ 21/5/02 */
	/**
	 * draw decomposition of the current oclMethod
	 * @param vs oclMethod.
	 */
	public void showDecomposition(vShape vs) {
		//which row is this shape in?
		HSShapeProperty hsProperty = (HSShapeProperty) vs.getObject();
		/* WZ 22/5/02 */
		hsProperty.getDecompList().clear(); /* WZ 10/6/02 */

		Utility.debugPrintln("stepper","HStepperCanvas positionRecord: \n" + positionRecord.toString());
		int curRow = hsProperty.getRow();
		//if it is the last row then create an extra row
		if (!(curRow < positionRecord.getSize()))
			curRow = positionRecord.addPosition();
		//create a row and remember it.;
		else { /* WZ 30/5/02 */
			curRow = hsProperty.getRow() + 1;
		}
		Utility.debugPrintln("stepper","HStepperCanvas curRow -- " + curRow);

		oclMethod omd = (oclMethod) hsProperty.getObject();

		//draw shapes
		Vector vector = new Vector(); /* WZ 14/6/02 */
		int x, y, curCol;
		vShape tmpShape = null;
		vShape[] decompList = new vShape[((List) omd.getDecomps()).size()];
		int k = 0;
		ListIterator li = ((List) omd.getDecomps()).listIterator();
		while (li.hasNext()) {
			Object obj = (Object) li.next();
			positionRecord.increase(curRow);
			x = positionRecord.getX(curRow);
			y = positionRecord.getY(curRow);
			curCol = positionRecord.getColumn(curRow);
			if (obj.getClass().getName() == "jplan.ocl.oclSS") {
				oclSS ss = (oclSS) obj;
				hsProperty = new HSShapeProperty(ss, curRow, curCol);
				tmpShape = drawOclSS(hsProperty, x, y);
				hsProperty.setOwnerShapeIndex(tmpShape.getID());
				/* WZ 18/6/02 */
			} else if (obj.getClass().getName() == "jplan.ocl.oclPredicate") {
				Object object =
					workingDomain.checkObjectType((oclPredicate) obj);
				if (object.getClass().getName() == "jplan.ocl.oclOperator") {
					oclOperator op = (oclOperator) object;
					hsProperty = new HSShapeProperty(op, curRow, curCol);
					tmpShape = drawOclOperator(hsProperty, x, y);
					hsProperty.setOwnerShapeIndex(tmpShape.getID());
					/* WZ 18/6/02 */
				} else if (
					object.getClass().getName() == "jplan.ocl.oclMethod") {
					oclMethod om = (oclMethod) object;
					hsProperty = new HSShapeProperty(om, curRow, curCol);
					tmpShape = drawOclMethod(hsProperty, x, y);
					hsProperty.setOwnerShapeIndex(tmpShape.getID());
					/* WZ 18/6/02 */
					/* WZ 14/6/02 */
					String tmpLabel = tmpShape.getLabel();
					int i = checkDuplicateName(vector, tmpLabel);
					if (i > 0)
						tmpShape.setLabel(tmpLabel + i);
				}
				repaint(); /* WZ 22/5/02 */
			}
			setDrawingLinkID(5); //pink color
			createVLink(vs, tmpShape, vLink.STRAIGHT); /* WZ 27/5/02 */
			hsProperty.setParentShape(vs, this); /* WZ 27/5/02 */
			decompList[k] = tmpShape;
			k++;
			//record the decomposition list for parent shape
			((HSShapeProperty) vs.getObject()).addDecompItem(tmpShape);
			/* WZ 22/5/02 */
		}

		setDrawingLinkID(0); //blue color
		//link shapes
		vShape fromShape = null, toShape = null;
		String fromShapeKey = null, toShapeKey = null;
		oclPredicate oprd = null;
		li = ((List) omd.getTemps()).listIterator();
		while (li.hasNext()) {
			oprd = (oclPredicate) li.next();
			fromShapeKey = oprd.getNthElementName(0);
			fromShape =
				decompList[java.lang.Integer.parseInt(fromShapeKey) - 1];
			toShapeKey = oprd.getNthElementName(1);
			toShape = decompList[java.lang.Integer.parseInt(toShapeKey) - 1];
			createVLink(fromShape, toShape, vLink.STRAIGHT);
		}

		curDecompList = ((HSShapeProperty) vs.getObject()).getDecompList();
		/* WZ 27/5/02 */

	}

	/* WZ 14/6/02 */
	/**
	 * to count how many items with the same name of given string exists in given vector.
	 * @param vector given Vector
	 * @param tmpLabel given string
	 * @return number of same items
	 */
	private int checkDuplicateName(Vector vector, String tmpLabel) {
		int no = 0;
		for (int i = 0; i < vector.size(); i++) {
			String curStr = vector.elementAt(i).toString();
			if (curStr.equals(tmpLabel))
				no++;
		}
		vector.addElement(tmpLabel);
		return no;
	}

	/**
	 * Return the shape in a given row with given position/column.
	 * @param row int
	 * @param column int
	 * @return the shape in a given row with given position/column
	 */
	public vShape getStepperShape(int row, int column) {
		for (int i = 1; i < vShapeList.size() + 1; i++) {
			vShape vs = (vShape) getShape(i);
			HSShapeProperty hsProperty = (HSShapeProperty) vs.getObject();
			if (row == hsProperty.getRow() && column == hsProperty.getColumn())
				return vs;
		}
		return null;
	}

	/* WZ  21/5/02 */
	/**
	 * draw the oclOperator's image on this Canvas
	 * @param hsProperty the given oclOperator
	 * @param x
	 * @param y
	 */
	private vShape drawOclOperator(HSShapeProperty hsProperty, int x, int y) {
		oclOperator op = (oclOperator) hsProperty.getReference();
		setDrawingShapeID(0);
		vShape vs = createVShape(x, y);
		vs.removeTextField();
		vs.setLabel(op.opName.getName());
		vs.setObject(hsProperty);

		return vs;
	}

	/* WZ  21/5/02 */
	/**
	 * draw the oclSS's image on this MethodCanvas
	 * @param hsProperty the given oclSS
	 * @param x
	 * @param y
	 * 
	 */
	private vShape drawOclSS(HSShapeProperty hsProperty, int x, int y) {
		oclSS ss = (oclSS) hsProperty.getReference();
		setDrawingShapeID(3);
		vShape vs = createVShape(x, y);
		vs.removeTextField(); /* WZ 24/5/02 */

		vs.setLabel("Achieve");
		vs.setObject(hsProperty);

		return vs;
	}

	/* WZ 21/5/02 */
	/**
	 * draw the oclMethod's image on this MethodCanvas
	 * @param hsProperty the given oclMethod - compound operator
	 * @param x
	 * @param y
	 */
	private vShape drawOclMethod(HSShapeProperty hsProperty, int x, int y) {
		oclMethod om = (oclMethod) hsProperty.getReference();
		setDrawingShapeID(5);
		vShape vs = createVShape(x, y);
		vs.removeTextField();
		vs.setLabel(om.getName().getName());
		vs.setObject(hsProperty);

		return vs;
	}

	/* WZ 6/6/02 */
	/**
	 * Change the focus of the scollPane
	 * @param vshape the new scrollBar value
	 * 
	 */
	private void focus(vShape vshape) {
		selectSingleShape(vshape);
		((HStepperWindow) parent).focus(
			vshape.px / (double) (getSize().width),
			(double) vshape.py / (double) (getSize().height));
	}

	/* WZ 22/5/02 */
	/**
	 * Start the stepper
	 * 
	 */
	public void startStepper() {
		//check current shapes
		Utility.debugPrintln("stepper","HStepperCanvas \ncurShape ==> " + curShape.getLabel() + "\n");

		if (hasSubGoal()) { /* WZ 28/5/02 */
			HSShapeProperty hsProperty = (HSShapeProperty) curShape.getObject();
			runForSubGoal(hsProperty.getRow() + 1);
			// 	    Utility.debugPrintln("stepper","HStepperCanvas hsProperty.getRow() "+hsProperty.getRow());
			return;
		}

		/* WZ 28/5/02 */
		if (curDecompList.size() == 0) {
			JOptionPane.showMessageDialog(
				this,
				"No goals in the current selected task.",
				"GIPO ERROR",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}

		//in the decomposition list, any shape without specific inlinks will be singled out.
		List temList = new ArrayList();
		vShape vs;
		vLink tmpLink;
		ListIterator li = curDecompList.listIterator();
		while (li.hasNext()) {
			vs = (vShape) li.next();
			Utility.debugPrintln("stepper","HStepperCanvas curDecompList ==> " + vs.getLabel());
			int linkSize = 0; /* WZ 24/5/02 */

			if (((HSShapeProperty) vs.getObject()).getObject() == null) {
				for (int i = 1; i < vs.getInLinks().size() + 1; i++) {
					tmpLink = (vLink) vs.getInLinks().get("inlinks" + i);
					if (tmpLink.getType() != 5 //not pink (decomposition line from parent)
						&& tmpLink.getType() != 2) { //not green (finishing line - predecessor has completed)
						linkSize++; /* WZ 24/5/02 */
					}
				}
				if (linkSize == 0) {
					temList.add(vs);
					Utility.debugPrintln(
						"shape added to temList ==> " + vs.getLabel());
				}
			}
		}

		if (temList.size() == 1) {
			li = temList.listIterator();
			curShape = (vShape) li.next();
			focus(curShape); /* WZ 6/6/02 */
			steppingA();
		} else if (
			temList.size() > 1) {
			//popup message, blink shapes for user to pick
			//select the shape
			/* WZ 10/6/02 */
			vShape tmpSP =
				(vShape) GipoSelectionBox.showSelectionBox(
					parent.getTheParent(),
					"Please select one of the actions.",
					temList);
			if (tmpSP == null)
				return;
			//proceed
			curShape = tmpSP;
			focus(curShape);
			steppingA();
		} else if (temList.size() == 0) {
			HSShapeProperty hpty = (HSShapeProperty) curShape.getObject();
			vShape vsMid = hpty.getParentShape(this);
			Utility.debugPrintln("stepper","HStepperCanvas >> vsMid ==> " + vsMid.getLabel());
			/* WZ 28/5/02 set post state */
			 ((HSShapeProperty) vsMid.getObject()).setPostState(curState);
			vShape vsTop =
				((HSShapeProperty) vsMid.getObject()).getParentShape(this);

			if (vsTop == null) { //end of the task
				JOptionPane.showMessageDialog(
					this,
					"Congratulations! The goal has been reached.",
					"OCL Confirm",
					JOptionPane.INFORMATION_MESSAGE,
					null);
				selectSingleShape(taskShape); /* WZ 6/6/02 */
				return;
			} else {
				Utility.debugPrintln("stepper","HStepperCanvas >> vsTop ==> " + vsTop.getLabel());
				curShape = vsMid; //move up
				focus(curShape); /* WZ 6/6/02 */
				//update current decomposition list, moved up 1 level
				curDecompList =
					((HSShapeProperty) vsTop.getObject()).getDecompList();
				startStepper(); /* WZ 28/5/02 going up right */
				return;
			}
		}
	}

	/* WZ 22/5/02 */
	private void steppingA() {
		recordState(); /* WZ 11/6/02 remember for possible re-do */
		vLink tmpLink;
		HSShapeProperty hsProperty = (HSShapeProperty) curShape.getObject();
		hsProperty.setPreState(curState);

		List mssgs = new ArrayList();

		switch (curShape.getShapeID()) {
			case 3 : //if oclSS, check against curStates
				vShape stateShape = curShape; /* WZ 31/5/02 */
				oclSS ss = (oclSS) hsProperty.getReference();
				/* WZ 30/5/02 */
				oclSS testSubGoal = null;
				//check if it is not fully instantiated.
				try {
					testSubGoal = (oclSS) ss.clone();

					//to instantiate subGoal
					while (!testSubGoal.isFullyInstantiated()) {
						oclPredicate beforePred =
							testSubGoal.getPredExpression();
						/* WZ 11/6/02 */
						oclSS tmpss =
							InstSS.showInstantiation(
								workingDomain,
								testSubGoal,
								this);
						if (tmpss == null) {
							return;
						} else {
							/* WZ 11/6/02 */
							oclPredicate afterPred = tmpss.getPredExpression();
							oclPredicate changedVariable =
								afterPred.getInstVariables(beforePred);
							instOtherComponent(changedVariable);
							/* end 11/6/02 */
							testSubGoal = tmpss;
						}
					}
					Utility.debugPrintln(
						"testSubGoal   " + testSubGoal.toString());
					onceInstantiated = true;
				} catch (CloneNotSupportedException e) {
					Utility.debugPrintln(e);
					return;
				}

				if (!StepperCanvas
					.unifiesCurrentState(
						workingDomain,
						curState,
						testSubGoal.getName(),
						testSubGoal.getState(),
						mssgs)) {
					try {
						subGoal = (oclSS) testSubGoal.clone();
						subGoalShape = curShape; /* WZ 28/5/02 */
						//ask user for a proper ocloperator/oclmethod
						int curRow = hsProperty.getRow();
						//if it is the last row then create an extra row
						if (!(curRow < positionRecord.getSize()))
							curRow = positionRecord.addPosition();
						//create a row and remember it.
						else { /* WZ 30/5/02 */
							curRow = hsProperty.getRow() + 1;
						}

						if (!runForSubGoal(curRow)) {
							if (onceInstantiated) {
								//I've changed my mind - once the undo function is active, 
								//then it is no need to ask again. Leave me alone.
								//if onceInstantiated then ask again if want another instantiation
								//if yes go back
								//if not break;
							}
							return;
						}
					} catch (CloneNotSupportedException e) {
						Utility.debugPrintln(e);
					}
				}

				//set Object with current oclss
				if (onceInstantiated) {
					hsProperty.setObject(testSubGoal);
					//is it needed to instantiate other parts (decompositions)?
				} else
					hsProperty.setObject(ss);
				hsProperty.setPostState(curState);

				//mark the shape and its out links
				for (int i = 1; i < stateShape.getOutLinks().size() + 1; i++) {
					tmpLink =
						(vLink) stateShape.getOutLinks().get("outlinks" + i);
					if (tmpLink.getType() == 0) { //if it is blue line
						tmpLink.setType(2);
						//change to green indicating finishing.
						tmpLink.setSelected(true); /* WZ 10/6/02 */
					}
				}

				subGoal = null; //clear
				subGoalShape = null; /* WZ 28/5/02 */
				onceInstantiated = false; /* WZ 31/5/02 */

				break;
			case 0 : //if oclOperator, popup for instantiation, then run it
				/* WZ 31/5/02 */
				oclOperator backupOP = (oclOperator) hsProperty.getReference();
				Utility.debugPrintln("stepper","HStepperCanvas backupOP   " + backupOP.toString());
				oclOperator op =
					OperatorInstantiation.showInstantiation(
						this, //Ron 6/11/02 - need reference to get current state
						workingDomain,
						backupOP,
						parent.getTheParent(),
						parent.getTheParent().strImageDir);
				if (op == null) {
					return;
				}

				hsProperty.setReference(op);

				if (!runOperator(hsProperty)) {
					hsProperty.setReference(backupOP); //reset
					return;
				}

				hsProperty.setReference(backupOP); //reset

				break;
			case 5 : //if oclMethod, show its decomposition, repeat
				if (!runMethod(hsProperty))
					return;
				break;
		}

		//mark the shape and its out links
		for (int i = 1; i < curShape.getOutLinks().size() + 1; i++) {
			tmpLink = (vLink) curShape.getOutLinks().get("outlinks" + i);
			if (tmpLink.getType() == 0) { //if it is blue line
				tmpLink.setType(2); //change to green indicating finishing.
				tmpLink.setSelected(true); /* WZ 10/6/02 */
			}
		}

		repaint();
		//repeat the same procedure
		startStepper();
	}

	/* WZ 27/5/02 */
	private boolean runForSubGoal(int curRow) {
		boolean useMethod = false; //Ron 14/5/03
		boolean gotResult = false;
		HSShapeProperty hsProperty = null;
		vShape tmpShape = null;
		/* WZ 10/6/02 */
		List preState = new ArrayList();
		//clone the current state
		ListIterator li = curState.listIterator();
		while (li.hasNext()) {
			oclSS ss = (oclSS) li.next();
			try {
				preState.add((oclSS) ss.clone());
			} catch (CloneNotSupportedException e) {
				Utility.debugPrintln(e);
				return false;
			}
		}
		/* end 10/6/02 */

		Object object =
			InstStepper.showInstantiation(
				workingDomain,
				this,
				parent.getTheParent().strImageDir,
				subGoal.toString());
		if (object == null)
			return false;

		positionRecord.increase(curRow);
		int curCol = positionRecord.getColumn(curRow);
		int x = positionRecord.getX(curRow);
		int y = positionRecord.getY(curRow);
		if (object.getClass().getName().equals("jplan.ocl.oclMethod")) {
			oclMethod md = (oclMethod) object;
			hsProperty = new HSShapeProperty(md, curRow, curCol);
			// Draw the chosen method
			tmpShape = drawOclMethod(hsProperty, x, y);
			hsProperty.setOwnerShapeIndex(tmpShape.getID());
			// make the method the new current shape
			curShape = tmpShape; /* WZ 28/5/02 */
			Utility.debugPrintln("stepper","HStepperCanvas  Before Run curShape ==> " + curShape.getLabel() + "\n");
			focus(curShape); /* WZ 6/6/02 */
			setDrawingLinkID(5); //pink color - link it up to the achieve goal
			createVLink(subGoalShape, tmpShape, vLink.STRAIGHT); /* WZ 27/5/02 */
			hsProperty.setParentShape(subGoalShape, this);
			hsProperty.setPreState(preState); /* WZ 10/6/02 */
			//record the decomposition list for parent shape
			((HSShapeProperty) subGoalShape.getObject()).addDecompItem(tmpShape);
			/* WZ 27/5/02 */
			repaint(); /* WZ 22/5/02 */
			// Now try and run/step the method itself
			gotResult = runMethod(hsProperty);
			if (gotResult) { /* WZ 28/5/02 */
				useMethod = true;
				/* WZ 22/5/02 */
				//curDecompList should be that of the method just run
				if (curDecompList.size() >= 1) {
					oclSS saveSub = subGoal;
					subGoal = null;
					Utility.debugPrintln("stepper","HStepperCanvas After run curShape ==> " + curShape.getLabel() + "\n");
					startStepper();
					subGoal = saveSub; // Just in case something needs doing to achieve goal after method applied
				} else {
					return false;
				}
				
			}
		} else if (
			object.getClass().getName().equals("jplan.ocl.oclOperator")) {
			oclOperator op = (oclOperator) object;
			hsProperty = new HSShapeProperty(op, curRow, curCol);
			gotResult = runOperator(hsProperty);
			if (gotResult) { /* WZ 28/5/02 */
				//draw this shape
				tmpShape = drawOclOperator(hsProperty, x, y);
				hsProperty.setOwnerShapeIndex(tmpShape.getID());
				/* WZ 18/6/02 */
			}
		}

		if (!gotResult) {
			//ask if try again;
			int k =
				JOptionPane.showConfirmDialog(
					parent.getTheParent(),
					"Not all conditions of the selected"
						+ " operator/method are true."
						+ "\nTry another operator/method?",
					"",
					JOptionPane.YES_NO_OPTION);
			if (k == JOptionPane.NO_OPTION) {
				return false;
			} else if (k == JOptionPane.YES_OPTION) {
				runForSubGoal(curRow);
				return true;
			}
		}

		if (!useMethod) {
			curShape = tmpShape; /* WZ 28/5/02 */
			focus(curShape); /* WZ 6/6/02 */
			setDrawingLinkID(5); //pink color
			createVLink(subGoalShape, tmpShape, vLink.STRAIGHT); /* WZ 27/5/02 */
			hsProperty.setParentShape(subGoalShape, this);
			hsProperty.setPreState(preState); /* WZ 10/6/02 */
			//record the decomposition list for parent shape
			((HSShapeProperty) subGoalShape.getObject()).addDecompItem(tmpShape);
			/* WZ 22/5/02 */
			curDecompList =
				((HSShapeProperty) subGoalShape.getObject()).getDecompList();
			/* WZ 27/5/02 */
			repaint(); /* WZ 22/5/02 */
		}
		Utility.debugPrintln("stepper","HStepperCanvas CHECKING SUB GOAL:\n" + subGoal.toString());
		if (!StepperCanvas
			.unifiesCurrentState(
				workingDomain,
				curState,
				subGoal.getName(),
				subGoal.getState(),
				new ArrayList())) {
			return runForSubGoal(hsProperty.getRow());
		}

		return true; //continue
	}

	/* WZ 27/5/02 */
	private boolean runOperator(HSShapeProperty hsProperty) {
		oclOperator op = (oclOperator) hsProperty.getReference();

		//change current state
		List mssgs = new ArrayList();
		if (!findStates(op, mssgs)) {
			StepperCanvas.opInstDialog(mssgs);
			return false;
		}

		// 	recordState();/* WZ 10/6/02 */

		ListIterator li;
		//oclSC - Necessary
		li = op.getNecessary().listIterator();
		while (li.hasNext()) {
			oclSC sc = (oclSC) li.next();
			updateCurState(sc);
		}
		//oclSC - Conditional
		li = op.getConditional().listIterator();
		while (li.hasNext()) {
			String objName = null;
			String before = null;
			Vector vector = new Vector();
			oclSC sc = (oclSC) li.next();
			String scName = sc.getName();
			if (Character.isUpperCase(scName.charAt(0))) {
				//if the object has not be instantiated
				//find sort of this varible
				List objectList =
					workingDomain.getObjectsOfSubTypes(sc.getSort());
				// 		Utility.debugPrintln("stepper","HStepperCanvas %%%%%%\n sc.getSort()  "+sc.getSort());
				ListIterator obLi = objectList.listIterator();
				while (obLi.hasNext()) {
					objName = obLi.next().toString();
					before = sc.getName();
					sc.replaceVariableName(before, objName);
					OEnviron env = new OEnviron();
					if (StepperCanvas
						.unifiesAbstractCurrentState(
							workingDomain,
							curState,
							sc.getName(),
							sc.getPre(),
							env,
							mssgs)) {
						vector.addElement(objName);
						StepperCanvas.instantiate_state(
							sc.getPost(),
							env);
					}
				}
			} else {
				vector.addElement(scName);
			}

			for (int i = 0; i < vector.size(); i++) {
				objName = vector.elementAt(i).toString();
				before = sc.getName();
				sc.replaceVariableName(before, objName);
				updateCurState(sc);
			}
		}

		//if ok set post state
		hsProperty.setPostState(curState);
		//if ok set Object with instantiated operator
		hsProperty.setObject(op);
		return true;
	}

	/* WZ 28/5/02 */
	private void updateCurState(oclSC sc) {
		ListIterator liCurState = curState.listIterator();
		//oclSC - Necessary
		while (liCurState.hasNext()) {
			oclSS ss = (oclSS) liCurState.next();
			String objectID = sc.getName();
			if (objectID.equals(ss.getName())) {
				ss.updateWith(sc.getPre(), sc.getPost(), workingDomain);
				break;
			}
		}
		((HStepperWindow) parent).updateCurState(curState); /* WZ 5/6/02 */
	}

	/* WZ 28/5/02 */
	/**
	   * Get the value of curState.
	   * @return Value of curState.
	   */
	public List getCurState() {
		return curState;
	}

	/* WZ 28/5/02 */
	/**
	   * Set the value of curState.
	   * @param v  Value to assign to curState.
	   */
	public void setCurState(List v) {
		this.curState = v;
	}

	/* WZ 27/5/02 */
	/**
	 * runMethod tru to use the selected method
	 * @param hsProperty the shape property
	 * @return - true of all OK
	 */
	private boolean runMethod(HSShapeProperty hsProperty) {
		List mssgs = new ArrayList();
		oclMethod mdRef = (oclMethod) hsProperty.getReference();

		//get selected method and popup instantiation window
		oclMethod curOMD =
			MethodInstantiation.showInstantiation(
				workingDomain,
				mdRef,
				this,
				parent.getTheParent().strImageDir);

		//if cancel
		if (curOMD == null) {
			//go back to the list of methods
			return false;
		}
		//check conditions
		/* WZ 24/5/02 */
		if (!findStates(curOMD,
			mssgs)) { //if not ok, popup a message, stop stepping.
			StepperCanvas.opInstDialog(mssgs);
			return false;
		} else {
			//if ok set Object with instantiated method
			hsProperty.setObject(curOMD);
			/* WZ 22/5/02 Only shapes with object not null can be expanded. */

			//showDecomposition
			showDecomposition(curShape);
			return true;
		}
	}

	/* WZ 27/5/02 */
	private boolean hasSubGoal() {
		if (subGoal != null)
			return true;
		return false;
	}

	/* WZ 11/6/02 */
	/**
	 * to update all variables in the given shape list.
	 * @varOpd oclPredicate containing old variable
	 * in arg.name and new object in arg.sort 
	 * 
	 */
	private void instOtherComponent(oclPredicate varOpd) {
		List newList = new ArrayList();
		ListIterator li = curDecompList.listIterator();
		while (li.hasNext()) {
			vShape vs = (vShape) li.next();
			if (((HSShapeProperty) vs.getObject()).getObject() == null) {
				newList.add(vs);
			}
		}
		updateVariables(newList, varOpd);
	}

	/* WZ 11/6/02 */
	/**
	 * to update all variables in the given shape list.
	 * @decompList shape list
	 * @varOpd oclPredicate containing old variable
	 * in arg.name and new object in arg.sort 
	 * 
	 */
	private void updateVariables(List decompList, oclPredicate varOpd) {
		ListIterator lidecomp = curDecompList.listIterator();
		while (lidecomp.hasNext()) {
			vShape vs = (vShape) lidecomp.next();
			HSShapeProperty hsspty = (HSShapeProperty) vs.getObject();
			Object obj = (Object) hsspty.getReference();
			replaceVariable(obj, varOpd);
		}
	}

	/* WZ 11/6/02 */
	/**
	 * to replace variables from oclSS or oclPredicate.
	 * @varOpd oclPredicate containing old variable
	 * in arg.name and new object in arg.sort 
	 * 
	 */
	private void replaceVariable(Object obj, oclPredicate varOpd) {
		ListIterator liArgs = varOpd.getArguments().listIterator();
		while (liArgs.hasNext()) {
			oclPredicate.pArg curArg = (oclPredicate.pArg) liArgs.next();
			if (obj.getClass().getName() == "jplan.ocl.oclSS") {
				oclSS ss = (oclSS) obj;
				ss.replaceVariableName(curArg.name, curArg.sort);
				//here the curArg.sort contains the instantiated objects
			} else if (obj.getClass().getName() == "jplan.ocl.oclOperator") {
				oclOperator op = (oclOperator) obj;
				op.replaceVariableName(curArg.name, curArg.sort);
			} else if (obj.getClass().getName() == "jplan.ocl.oclMethod") {
				oclMethod om = (oclMethod) obj;
				om.replaceVariableName(curArg.name, curArg.sort);
			}
		}
	}

	/* WZ 18/6/02 */
	/**
	 * A string representation of the graphics on the canvas.
	 * Used mainly for saving purpose.
	 * @return String
	 */
	public String to_String() {
		StringBuffer str = new StringBuffer();
		str.append(super.to_String() + "\n");
		HSShapeProperty hsProperty;
		for (int i = 1; i < vShapeList.size() + 1; i++) {
			vShape vs = (vShape) getShape(i);
			hsProperty = (HSShapeProperty) vs.getObject();
			str.append(hsProperty.to_String() + "\n");
		}
		//save global variables
		//decomposition list (shape ids)
		str.append("BEGIN STEPPER CANVAS\n");
		if (curDecompList.size() > 0) {
			str.append("current decomposition list:");
			ListIterator li = curDecompList.listIterator();
			while (li.hasNext()) {
				vShape vs = (vShape) li.next();
				str.append(vs.getID());
				if (li.hasNext()) /* WZ 20/6/02 */
					str.append(",");
			}
			str.append("\n");
		}
		//sub goal and subgoal shape
		if (subGoalShape != null) {
			str.append("subgoal shape id:" + subGoalShape.getID() + "\n");
		} else
			str.append("subgoal shape id:0" + "\n");

		//onceInstantiated
		str.append("onceInstantiated:" + onceInstantiated + "\n");
		//current state
		if (curState.size() > 0) {
			str = str.append("\nBEGIN CURRENT STATE\n");
			ListIterator li = curState.listIterator();
			while (li.hasNext()) {
				oclSS ss = (oclSS) li.next();
				str = str.append(ss.to_String() + "\n");
			}
			str = str.append("END CURRENT STATE\n");
		}

		str.append("\nEND STEPPER CANVAS\n");

		return str.toString();
	}

	/* WZ 18/6/02 */
	/**
	 * To open a presaved .vm (Visual Modeller) file,
	 * load data from a bufferredReader.
	 * 
	 */
	public void loadFile(BufferedReader br) {
		super.loadFile(br);
		String str = "";
		int mySwitch = 0,
			i = 0,
			k = 0,
			j = 0,
			m = 0,
			case5Line = 0,
			n = 0,
			x = 0;
		vShape tempSP = null;
		vLink tempLK = null;
		int ownerShapeID = 0, row = 0, column = 0;
		String referType = "";
		HSShapeProperty hsProperty = null;
		int parentShapeKey = 0;
		List msg = new ArrayList(); /* WZ 23/8/02 */

		while (!str.equals("END STEPPER CANVAS")) {
			try {
				str = br.readLine();
				Utility.debugPrintln(str);
				if (str.equals("BEGIN H STEPPER SHAPE PROPERTY")) {
					mySwitch = 1; //for the HSShapeProperty varibles
					i = 0;
					hsProperty = null;
				}
				if (str.equals("BEGIN REFERENCE")) {
					mySwitch = 2; //for the reference
					j = 0;
				}
				if (str.equals("BEGIN OBJECT")) {
					mySwitch = 3; //for the object
					k = 0;
				}
				if (str.equals("BEGIN PRE STATE")) {
					mySwitch = 4; //for the postState
					m = 0;
				}
				if (str.equals("BEGIN POST STATE")) {
					mySwitch = 5; //for the postState
					case5Line = 0;
				}
				if (str.equals("BEGIN STEPPER CANVAS")) {
					mySwitch = 6; //for the STEPPER CANVAS
					curDecompList = new ArrayList(); //reset
					n = 0;
				}
				if (str.equals("BEGIN CURRENT STATE")) {
					mySwitch = 7; //for the CURRENT STATE
					curState.clear();
					x = 0;
				}
				if (str.startsWith("END")) {
					mySwitch = 0; //reset
				}
				switch (mySwitch) {
					case 1 : //HSShapeProperty varibles
						switch (i) {
							case 1 : //line 1
								ownerShapeID = parseInt(str, "owner shape ID:");
								break;
							case 2 : //line 2
								row = parseInt(str, "row:");
								break;
							case 3 : //line 3
								column = parseInt(str, "column:");
								break;
							case 4 : //line 4
								parentShapeKey =
									parseInt(str, "parent shape key:");
								break;
						}

						i++;
						break;
					case 2 : //REFERENCE varibles
						switch (j) {
							case 1 : //line 1
								referType = getString(str, "reference type:");
								break;
							case 2 : //line 2
								if (referType.equals("oclMethod")) {
									oclMethod md = parseMethod(str, msg);
									/* WZ 23/8/02 */
									hsProperty =
										new HSShapeProperty(md, row, column);
								} else if (referType.equals("oclOperator")) {
									oclOperator op = parseOperator(str, msg);
									/* WZ 23/8/02 */
									hsProperty =
										new HSShapeProperty(op, row, column);
								} else if (referType.equals("oclSS")) {
									oclSS ss = parseSS(str);
									hsProperty =
										new HSShapeProperty(ss, row, column);
								}

								hsProperty.setOwnerShapeIndex(ownerShapeID);
								hsProperty.setParentShapeKey(parentShapeKey);
								hsProperty.setRow(row);
								hsProperty.setColumn(column);
								/* WZ 21/6/02 */
								for (int linkCounter = 1;
									linkCounter
										< shape[ownerShapeID]
											.getOutLinks()
											.size()
											+ 1;
									linkCounter++) {
									vLink tmpLink =
										(vLink) shape[ownerShapeID]
											.getOutLinks()
											.get(
											"outlinks" + linkCounter);
									if (tmpLink.getType() == 5)
										//if it is pink line
										hsProperty.addDecompItem(
											tmpLink.getStopShape());
								}
								/* end 21/6/02 */
								shape[ownerShapeID].setObject(hsProperty);
								break;
						}

						j++;
						break;
					case 3 : //object varibles
						switch (k) {
							case 1 : //line 1
								referType = getString(str, "object type:");
								break;
							case 2 : //line 2
								if (referType.equals("oclMethod")) {
									oclMethod md = parseMethod(str, msg);
									/* WZ 23/8/02 */
									hsProperty.setObject(md);
								} else if (referType.equals("oclOperator")) {
									oclOperator op = parseOperator(str, msg);
									/* WZ 23/8/02 */
									hsProperty.setObject(op);
								} else if (referType.equals("oclSS")) {
									oclSS ss = parseSS(str);
									hsProperty.setObject(ss);
								}
								break;
						}

						k++;
						break;
					case 4 : //preState varibles/* WZ 20/6/02 */
						if (m > 0) {
							oclSS ss = parseSS(str);
							hsProperty.addPreState(ss);
						} else
							m++;

						break;
					case 5 : //postState varibles/* WZ 20/6/02 */
						if (case5Line > 0) {
							oclSS ss = parseSS(str);
							hsProperty.addPostState(ss);
						} else
							case5Line++;
						break;
					case 6 : //STEPPER CANVAS varibles/* WZ 20/6/02 */
						switch (n) {
							case 1 : //line 1:current decomposition list
								String tmpStr =
									getString(
										str,
										"current decomposition list:");
								StringTokenizer st =
									new StringTokenizer(tmpStr, ",");
								while (st.hasMoreTokens()) {
									int spID = Integer.parseInt(st.nextToken());
									vShape vs = shape[spID];
									curDecompList.add(vs);
								}
								break;
							case 2 : //line 2:subgoal shape id
								tmpStr = getString(str, "subgoal shape id:");
								int spID = Integer.parseInt(tmpStr);
								if (spID > 0) {
									subGoalShape = shape[spID];
									HSShapeProperty hspty =
										(HSShapeProperty) subGoalShape
											.getObject();
									oclSS ss = (oclSS) hspty.getObject();
									subGoal = ss;
								} else {
									subGoalShape = null;
									subGoal = null;
								}

								break;
							case 3 : //line 3:onceInstantiated
								tmpStr = getString(str, "onceInstantiated");
								if (tmpStr.equals("true"))
									onceInstantiated = true;
								else
									onceInstantiated = false;

								break;
						}

						n++;
						break;
					case 7 : //CURRENT STATE varibles/* WZ 20/6/02 */
						if (x > 0) {
							oclSS ss = parseSS(str);
							curState.add(ss);
						} else
							x++;
						break;
				}
			} catch (java.io.IOException ex) {
			};
		}

		//get other inferred variables
		taskShape = shape[1];
		//populate current states
		 ((HStepperWindow) parent).updateCurState(curState);
		//curShape
		curShape = getFirstSelectedShape();
		//positionRecord
		int baseRow = 0, baseCol = 0;
		for (int ii = 1; ii < vShapeList.size() + 1; ii++) {
			vShape vs = (vShape) getShape(ii);
			HSShapeProperty hsp = (HSShapeProperty) vs.getObject();
			int r = hsp.getRow();
			int c = hsp.getColumn();
			int cp = positionRecord.getColumn(r);
			while (cp == -1) {
				positionRecord.addPosition();
				cp = positionRecord.getColumn(r);
			};
			if (c > cp)
				positionRecord.setColumn(r, c);

		}
		/* WZ 23/8/02 */
		StepperCanvas.opInstDialog(msg);
	}

	/* WZ 19/6/02 */
	/**
	 * Parse a string description into an oclMethod
	 * @param baseStr base string
	 * @return an oclMethod
	 */
	public oclSS parseSS(String baseStr) {
		if (baseStr.length() == 0)
			return null;

		String tmpStr = getString(baseStr, "ss(");
		String kind = getInternalString(tmpStr, ",");
		tmpStr = getString(tmpStr, kind + ",");
		String id = getInternalString(tmpStr, ",");
		oclSS ss = new oclSS(kind, id);

		tmpStr = getString(tmpStr, id + ",[");
		tmpStr = getInternalString(tmpStr, "]");
		StringTokenizer st = new StringTokenizer(tmpStr, "^");
		while (st.hasMoreTokens()) {
			oclPredicate opd = parsePredicate(st.nextToken());
			ss.addPredicate(opd);
		}
		return ss;
	}

	/* WZ 23/8/02 */
	/**
	 * Parse a string description into an oclOperator
	 * @param baseStr base string
	 * @return an oclOperator
	 */
	public oclOperator parseOperator(String baseStr, List mssg) {
		oclOperator op = parseOperator(baseStr);
		List msg = new ArrayList();
		if (!op.check(workingDomain, msg)) {
			mssg.addAll(msg);
		}
		return op;
	}

	/* WZ 19/6/02 */
	/**
	 * Parse a string description into an oclOperator
	 * @param baseStr base string
	 * @return an oclOperator
	 */
	public oclOperator parseOperator(String baseStr) {
		if (baseStr.length() == 0)
			return null;

		oclOperator op = new oclOperator();
		//Description
		String tmpStr = getString(baseStr, "operator::Description:");
		String description = getInternalString(tmpStr);
		Utility.debugPrintln("stepper","HStepperCanvas description...\n" + description);
		StringTokenizer st = new StringTokenizer(description, "&");
		while (st.hasMoreTokens()) {
			op.addDocmLine(st.nextToken());
		}
		//Name
		tmpStr = getString(tmpStr, ";Name:");
		String name = getInternalString(tmpStr);
		Utility.debugPrintln("stepper","HStepperCanvas name...\n" + name);
		/* WZ 24/6/02 */
		oclPredicate opName = null;
		if (name.length() != 0) {
			String predName = getInternalString(name, "&");
			opName = parsePredicate(predName);
			String predSort = getString(name, predName + "&");
			oclPredicate opNameSort = parsePredicate(predSort);
			opName.setArgSorts(opNameSort);
		}
		/* end 24/6/02 */
		if (opName != null)
			op.setName(opName);
		//Prevail
		tmpStr = getString(tmpStr, ";Prevail:");
		String precondition = getInternalString(tmpStr);
		Utility.debugPrintln("stepper","HStepperCanvas Prevail...\n" + precondition);
		st = new StringTokenizer(precondition, "&");
		while (st.hasMoreTokens()) {
			oclSE se = parseSE(st.nextToken());
			if (se != null)
				op.addPrevSE(se);
		}
		//Necessary
		tmpStr = getString(tmpStr, ";Necessary:");
		String necessary = getInternalString(tmpStr);
		Utility.debugPrintln("stepper","HStepperCanvas Necessary...\n" + necessary);
		st = new StringTokenizer(necessary, "&");
		while (st.hasMoreTokens()) {
			oclSC sc = parseSC(st.nextToken());
			if (sc != null)
				op.addNecSC(sc);
		}
		//Conditional
		tmpStr = getString(tmpStr, ";Conditional:");
		String conditional = getInternalString(tmpStr);
		Utility.debugPrintln("stepper","HStepperCanvas Conditional...\n" + conditional);
		st = new StringTokenizer(conditional, "&");
		while (st.hasMoreTokens()) {
			oclSC sc = parseSC(st.nextToken());
			if (sc != null)
				op.addCondSC(sc);
		}
		/* WZ 21/6/02 */
		// 	List msg = new ArrayList();
		// 	if (!op.check(workingDomain, msg)){
		// 	    StepperCanvas.opInstDialog(msg);
		// // 	    return null;
		// 	}
		/* end 21/6/02 */

		if (opName != null)
			op.setName(opName);
		/* WZ 24/6/02 */

		return op;
	}

	/* WZ 23/8/02 */
	/**
	 * Parse a string description into an oclMethod
	 * @param baseStr base string
	 * @return an oclMethod
	 */
	public oclMethod parseMethod(String baseStr, List mssg) {
		oclMethod md = parseMethod(baseStr);
		List msg = new ArrayList();
		if (!md.check(workingDomain, msg)) {
			mssg.addAll(msg);
		}
		return md;
	}

	/* WZ 19/6/02 */
	/**
	 * Parse a string description into an oclMethod
	 * @param baseStr base string
	 * @return an oclMethod
	 */
	public oclMethod parseMethod(String baseStr) {
		if (baseStr.length() == 0)
			return null;

		oclMethod md = new oclMethod();
		//Description
		String tmpStr = getString(baseStr, "method::Description:");
		String description = getInternalString(tmpStr);
		Utility.debugPrintln("stepper","HStepperCanvas description...\n" + description);
		StringTokenizer st = new StringTokenizer(description, "&");
		while (st.hasMoreTokens()) {
			md.addDocmLine(st.nextToken());
		}
		//Name
		tmpStr = getString(tmpStr, description + ";Name:");
		String name = getInternalString(tmpStr);
		Utility.debugPrintln("stepper","HStepperCanvas name...\n" + name);
		/* WZ 24/6/02 */
		oclPredicate mdName = null;
		if (name.length() != 0) {
			String predName = getInternalString(name, "&");
			mdName = parsePredicate(predName);
			String predSort = getString(name, predName + "&");
			oclPredicate mdNameSort = parsePredicate(predSort);
			mdName.setArgSorts(mdNameSort);
		}
		if (mdName != null)
			md.setName(mdName);

		//Pre-condition
		tmpStr = getString(tmpStr, name + ";Pre-condition:");
		String precondition = getInternalString(tmpStr);
		Utility.debugPrintln("stepper","HStepperCanvas precondition...\n" + precondition);
		st = new StringTokenizer(precondition, "&");
		while (st.hasMoreTokens()) {
			oclSE se = parseSE(st.nextToken());
			if (se != null)
				md.addPreSE(se);
		}
		//Index Transitions
		tmpStr = getString(tmpStr, precondition + ";Index Transitions:");
		String transitions = getInternalString(tmpStr);
		Utility.debugPrintln("stepper","HStepperCanvas transitions...\n" + transitions);
		st = new StringTokenizer(transitions, "&");
		while (st.hasMoreTokens()) {
			oclSC sc = parseSC(st.nextToken());
			if (sc != null)
				md.addIndexSC(sc);
		}
		//Statics
		tmpStr = getString(tmpStr, transitions + ";Static:");
		String statics = getInternalString(tmpStr);
		Utility.debugPrintln("stepper","HStepperCanvas statics...\n" + statics);
		st = new StringTokenizer(statics, "&");
		while (st.hasMoreTokens()) {
			oclPredicate opd = parsePredicate(st.nextToken());
			if (opd != null)
				md.addStatic(opd);
		}
		//Temporal Constraints
		tmpStr = getString(tmpStr, "Temporal Constraints:");
		String temporal = getInternalString(tmpStr);
		Utility.debugPrintln("stepper","HStepperCanvas temporal...\n" + temporal);
		st = new StringTokenizer(temporal, "&");
		while (st.hasMoreTokens()) {
			oclPredicate opd = parsePredicate(st.nextToken());
			if (opd != null)
				md.addTemps(opd);
		}
		//Decomposition
		tmpStr = getString(tmpStr, "Decomposition:");
		String decomposition = getInternalString(tmpStr);
		Utility.debugPrintln("stepper","HStepperCanvas decomposition...\n" + decomposition);
		st = new StringTokenizer(decomposition, "&");
		while (st.hasMoreTokens()) {
			String decom = st.nextToken();
			/* WZ 21/6/02 */
			if (decom.startsWith("ss(")) {
				oclSS ss = parseSS(decom);
				if (ss != null)
					md.addDecomps(ss);
			} else {
				oclPredicate opd = parsePredicate(decom);
				if (opd != null)
					md.addDecomps(opd);
			}
		}

		/* WZ 21/6/02 */
		// 	List msg = new ArrayList();
		// 	if (!md.check(workingDomain, msg)){
		// 	    StepperCanvas.opInstDialog(msg);
		// // 	    return null;
		// 	}
		/* end 21/6/02 */

		if (mdName != null)
			md.setName(mdName);
		/* WZ 24/6/02 */

		return md;
	}

	/* WZ 19/6/02 */
	/**
	 * Parse a string description into an oclPredicate
	 * @param baseStr base string
	 * @return an oclPredicate
	 */
	public oclPredicate parsePredicate(String baseStr) {
		if (baseStr.length() == 0)
			return null;
		if (baseStr.charAt(0) == ',') 
			baseStr = baseStr.substring(1);
		String name = getInternalString(baseStr, "(");
		oclPredicate opd = new oclPredicate(name);
		String tmpStr = getString(baseStr, name + "(");
		tmpStr = getInternalString(tmpStr, ")");
		StringTokenizer st = new StringTokenizer(tmpStr, ",");
		while (st.hasMoreTokens()) {
			opd.addVarArgument(st.nextToken());
		}
		return opd;
	}

	/* WZ 19/6/02 */
	/**
	 * Parse a string description into an oclSE
	 * @param baseStr base string
	 * @return an oclSE
	 */
	public oclSE parseSE(String baseStr) {
		if (baseStr.length() == 0)
			return null;

		String tmpStr = getString(baseStr, "se(");
		String kind = getInternalString(tmpStr, ",");
		tmpStr = getString(tmpStr, kind + ",");
		String id = getInternalString(tmpStr, ",");
		oclSE se = new oclSE(kind, id);

		tmpStr = getString(tmpStr, id + ",[");
		tmpStr = getInternalString(tmpStr, "]");
		StringTokenizer st = new StringTokenizer(tmpStr, "^");
		while (st.hasMoreTokens()) {
			oclPredicate opd = parsePredicate(st.nextToken());
			se.addPredicate(opd);
		}
		return se;
	}

	/* WZ 19/6/02 */
	/**
	 * Parse a string description into an oclSC
	 * @param baseStr base string
	 * @return an oclSC
	 */
	public oclSC parseSC(String baseStr) {
		if (baseStr.length() == 0)
			return null;

		String tmpStr = getString(baseStr, "sc(");
		String kind = getInternalString(tmpStr, ",");
		tmpStr = getString(tmpStr, kind + ",");
		String id = getInternalString(tmpStr, ",");
		oclSC sc = new oclSC(kind, id);
		//pre
		tmpStr = getString(tmpStr, id + ",[");
		String preStr = getInternalString(tmpStr, "]");
		StringTokenizer st = new StringTokenizer(preStr, "^");
		while (st.hasMoreTokens()) {
			oclPredicate opd = parsePredicate(st.nextToken());
			sc.addPre(opd);
		}
		//post
		String postStr = getString(tmpStr, "=>[");
		postStr = getInternalString(postStr, "]");
		st = new StringTokenizer(postStr, "^");
		while (st.hasMoreTokens()) {
			oclPredicate opd = parsePredicate(st.nextToken());
			sc.addPost(opd);
		}

		return sc;
	}

	/* WZ 19/6/02 */
	/**
	 * Return a string in the baseStr from begining to the first ";".
	 * @param baseStr base string
	 * @return a string in the baseStr from begining to the first ";".
	 */
	protected String getInternalString(String baseStr) {
		int index = baseStr.indexOf(";"); /* WZ 23/7/02 */
		if (index == -1)
			return new String("");
		else
			return baseStr.substring(0, baseStr.indexOf(";"));
	}

	/* WZ 19/6/02 */
	/**
	 * Return a string in the baseStr from begining to the first keyChar.
	 * @param baseStr base string
	 * @param keyChar key charactor
	 * @return a string in the baseStr from begining to the first keyChar
	 */
	protected String getInternalString(String baseStr, String keyChar) {
		int index = baseStr.indexOf(keyChar); /* WZ 23/7/02 */
		// Ron 9/5/03 find matching close bracket
		if (keyChar.equals(")")) {
			index = findClosingBra(baseStr, 0);
		}
		if (index == -1)
			return new String("");
		else
			return baseStr.substring(0, index);
	}
	
	/**
	 * findClosingBra
	 * find the closing bracket to match opening bracket in the string
	 * @param given - the string to search
	 * @param from - the index position to search from
	 * @return - the index position of the matching bracket 
	 */
	private int findClosingBra(String given, int from){
		int inxClose = given.indexOf(')',from);
		int inxOpen = given.indexOf('(',from);
		if (inxOpen != -1 && inxOpen < inxClose ) {
			int inxInner = findClosingBra(given,inxOpen + 1);
			return findClosingBra(given, inxInner + 1);
		} else {
			return inxClose;
		}
	}

	/* WZ 18/7/02 */
	/**
	 * To open a planner's running result,
	 * load data from a bufferredReader.
	 * 
	 */
	public void loadHTNResult(BufferedReader br) {
		String str = "";
		int mySwitch = 0, i = 0, x, y;
		HSShapeProperty hsProperty = null;
		vShape tempSP = null;
		oclMethod md = new oclMethod();
		List tempList = new ArrayList();
		String methodID = null;
		Vector vector = new Vector();
		List tmpShapeList = new ArrayList();

		while (!str.equals("END PLANNER RESULT")) {
			try {
				str = br.readLine();
				Utility.debugPrintln("stepper","HStepperCanvas \n>>>>" + str + "<<<<");
				if (str.equals("BEGIN METHOD")) {
					mySwitch = 1; //for the HSShapeProperty varibles
					i = 0;
					// 		    hsProperty = null;
					// 		    tempSP = null;
					md = new oclMethod();
					tempList = new ArrayList();
					tmpShapeList = new ArrayList();
				}
				switch (mySwitch) {
					case 1 : //
						switch (i) {
							case 1 : //line 1
								methodID = getInternalString(str);
								break;
							case 2 : //line 2
								String str_mdName = getString(str, "Name:");
								str_mdName = getInternalString(str_mdName);
								oclPredicate mdName =
									parsePredicate(str_mdName);
								int curRow = 1;
								int curCol;
								if (!(curRow < positionRecord.getSize()))
									curRow = positionRecord.addPosition();
								else {
									curRow = 2;
								}
								if (mdName.getName().equals("achieve")) { 
									tempSP = getAchieveShape(mdName);
									if (tempSP == null) {
										// Top level achieve
										tempSP = createAchieveNode(mdName,str);
									}
									hsProperty =
										(HSShapeProperty) tempSP.getObject();
									//hsProperty.setObject(md);

									//update the parent shape's decomposition
									vShape vsp =
										(vShape) hsProperty.getParentShape(
										this);
									HSShapeProperty hsshpty =
										(HSShapeProperty) vsp.getObject();
								} else {
									try {
										md.setName((oclPredicate) (mdName.clone()));
									} catch (CloneNotSupportedException e) {
									}
									//check all shapes, find the existing shape
									tempSP = getShapeByLabel(methodID);

									if (tempSP == null) {
										//otherwise, draw a new shape
										positionRecord.increase(curRow);
										x = positionRecord.getX(curRow);
										y = positionRecord.getY(curRow);
										curCol = positionRecord.getColumn(curRow);
										hsProperty =
											new HSShapeProperty(md, curRow, curCol);
										hsProperty.setObject(md);
										tempSP = drawOclMethod(hsProperty, x, y);
										Utility.debugPrintln(
											"\nDrawn method shape: " + tempSP.getID());
										//draw link from the parent shape - taskShape
										setDrawingLinkID(5); //pink color
										createVLink(
											taskShape,
											tempSP,
											vLink.STRAIGHT);
									} else { /* WZ 25/7/02 */
										tempSP.setLabel(md.getName().getName());
										hsProperty =
										(HSShapeProperty) tempSP.getObject();
										hsProperty.setObject(md);

										//update the parent shape's decomposition
										vShape vsp =
											(vShape) hsProperty.getParentShape(
											this);
										HSShapeProperty hsshpty =
											(HSShapeProperty) vsp.getObject();
										(
											(oclMethod) hsshpty
												.getObject())
												.changeDecomps(
											methodID,
											md.getName());
									}

									hsProperty.setOwnerShapeIndex(tempSP.getID());

									String tmpLabel = tempSP.getLabel();
									int j = checkDuplicateName(vector, tmpLabel);
									if (j > 0)
										tempSP.setLabel(tmpLabel + j);
								}
								break;
							case 3 : //line 3 - precondition

								break;
							case 4 : //line 4 - temporal constraints
								str = getString(str, "before(");
								while (str.length() != 0) {
									String tmpStr = getInternalString(str, ")");
									tempList.add(tmpStr);
									Utility.debugPrintln(
										"\nAdded temporal:" + tmpStr);
									str = getString(str, "before(");
								}
								break;

								/* WZ 23/7/02 */
							case 5 : //line 5 - decompsition
								str = getString(str, "step(");
								while (str.length() != 0) {
									//update positionRecord
									curRow = hsProperty.getRow();
									if (!(curRow < positionRecord.getSize()))
										curRow = positionRecord.addPosition();
									else {
										curRow = hsProperty.getRow() + 1;
									}

									String stepID = getInternalString(str, ",");
									str = getString(str, ",");

									vShape vs = null;
									//to represent the decomposition shape

									// to draw an "Achieve"
									if (str.startsWith("achieve")) {
										//Doris use "se" instead of "ss", so special actions are taken
										String tmpStr = getString(str, "se(");
										String kind =
											getInternalString(tmpStr, ",");
										tmpStr = getString(tmpStr, kind + ",");
										String id =
											getInternalString(tmpStr, ",");
										oclSS ss = new oclSS(kind, id);
										tmpStr = getString(tmpStr, id + ",[");
										tmpStr = getInternalString(tmpStr, "]");
										StringTokenizer st =
											new StringTokenizer(tmpStr, ")");
//										StringTokenizer st =
//											new StringTokenizer(tmpStr, "),");
										while (st.hasMoreTokens()) {
											oclPredicate opd =
												parsePredicate(
													st.nextToken() + ")");
											ss.addPredicate(opd);
										}
										Utility.debugPrintln(
											"\nss:" + ss.toString());

										md.addDecomps(ss); //add to the method
										positionRecord.increase(curRow);
										x = positionRecord.getX(curRow);
										y = positionRecord.getY(curRow);
										curCol =
											positionRecord.getColumn(curRow);
										HSShapeProperty hsPty =
											new HSShapeProperty(
												ss,
												curRow,
												curCol);
										hsPty.setObject(ss);
										//draw oclSS shape
										vs = drawOclSS(hsPty, x, y);
										// Ron 10/5/03
										vs.setLabel(stepID + "," + "achieve");
										Utility.debugPrintln(
											"\nDrawn SHAPE:" + vs.getLabel());

										//take the following operator/method
										str = getString(str, "exp(");
										//what about if there more than 1 operator
										if (str.startsWith("tn")) {
											//to draw a method
											//so far, I do not consider this possiblity.
											//vs.setLabel(getInternalString(str,",") + "," + "achieve");
											Utility.debugPrintln("stepper","HStepperCanvas ||||||| String Starts with tn " + vs.getLabel() );
										} else { //to draw an operator
											String opStr =
												getInternalString(str, ",step");
											if (opStr.length() == 0)
												opStr = str;

											oclPredicate opd =
												parsePredicate(opStr);
											oclOperator op =
												(
													oclOperator) workingDomain
														.checkObjectType(
													opd);
											//move down 1 row
											curRow = hsPty.getRow();
											if (!(curRow
												< positionRecord.getSize()))
												curRow =
													positionRecord
														.addPosition();
											else {
												curRow = hsPty.getRow() + 1;
											}

											positionRecord.increase(curRow);
											x = positionRecord.getX(curRow);
											y = positionRecord.getY(curRow);
											curCol =
												positionRecord.getColumn(
													curRow);
											HSShapeProperty ophsPty =
												new HSShapeProperty(
													op,
													curRow,
													curCol);
											ophsPty.setObject(op);
											//draw oclOperator shape
											vShape opShape =
												drawOclOperator(ophsPty, x, y);
											opShape.setLabel(
												op.opName.getName());

											//draw link from the parent shape
											setDrawingLinkID(5); //pink color
											createVLink(
												vs,
												opShape,
												vLink.STRAIGHT);
											//record decomposition
											(
												(HSShapeProperty) vs
													.getObject())
													.addDecompItem(
												opShape);
											//setparent shape
											(
												(HSShapeProperty) opShape
													.getObject())
													.setParentShape(
												vs,
												this);
											//move up 1 row
											curRow--;
										}
									} else { //go to look at "exp"
										Utility.debugPrintln("stepper","HStepperCanvas \n" + str);

										str = getString(str, "exp(");

										if (str.startsWith("tn")) {
											//to draw a method
											String tmpStr =
												getInternalString(str, ")");

											positionRecord.increase(curRow);
											x = positionRecord.getX(curRow);
											y = positionRecord.getY(curRow);
											curCol =
												positionRecord.getColumn(
													curRow);
											HSShapeProperty hsPty =
												new HSShapeProperty(
													curRow,
													curCol);
											//draw oclmethod shape
											setDrawingShapeID(5);
											vs = createVShape(x, y);
											vs.removeTextField();
											vs.setLabel(stepID + "," + tmpStr);
											vs.setObject(hsPty);
											Utility.debugPrintln(
												"\nDrawn SHAPE:"
													+ vs.getLabel());
											md.addDecomps(
												new oclPredicate(tmpStr));
											//add to the method
										} else { //to draw an operator
											String tmpStr =
												getInternalString(str, ",step");
											if (tmpStr.length() == 0)
												tmpStr = str;

											oclPredicate opd =
												parsePredicate(tmpStr);
											oclOperator op =
												(
													oclOperator) workingDomain
														.checkObjectType(
													opd);
											md.addDecomps(op.opName);
											//add to the method
											positionRecord.increase(curRow);
											x = positionRecord.getX(curRow);
											y = positionRecord.getY(curRow);
											curCol =
												positionRecord.getColumn(
													curRow);
											HSShapeProperty hsPty =
												new HSShapeProperty(
													op,
													curRow,
													curCol);
											hsPty.setObject(op);
											//draw oclOperator shape
											vs = drawOclOperator(hsPty, x, y);
											vs.setLabel(
												stepID
													+ ","
													+ op.opName.getName());
											Utility.debugPrintln(
												"\nDrawn SHAPE:"
													+ vs.getLabel());
										}
									}

									//draw link from the parent shape
									setDrawingLinkID(5); //pink color
									createVLink(tempSP, vs, vLink.STRAIGHT);
									//record decomposition
									(
										(HSShapeProperty) tempSP
											.getObject())
											.addDecompItem(
										vs);
									//setparent shape
									(
										(HSShapeProperty) vs
											.getObject())
											.setParentShape(
										tempSP,
										this);

									//record the decomposition shapes, later, the label of the shape will be refreshed.
									tmpShapeList.add(vs);

									//continue to parse next decomposition
									str = getString(str, "step(");
								}

								//take temporal constraints, draw links
								ListIterator li = tempList.listIterator();
								while (li.hasNext()) {
									String tmpStr = (String) li.next();
									String fromLabel = null;
									String toLabel = null;
									int inxBra = tmpStr.indexOf('(');
									if (inxBra != -1 && inxBra < tmpStr.indexOf(',')) {
										//fromLabel = getInternalString(tmpStr,"),");
										//fromLabel = fromLabel.concat(")");
										fromLabel = getInternalString(tmpStr,"(");
										toLabel = getString(tmpStr,"),");
										toLabel = getInternalString(toLabel,"(");
									} else {
										fromLabel = getInternalString(tmpStr, ",");
										toLabel = getString(tmpStr, ",");
									}
									//check
									vShape fromShape =
										getShapeByLabel(fromLabel);
									vShape toShape = getShapeByLabel(toLabel);
									setDrawingLinkID(0); //blue color
									createVLink(
										fromShape,
										toShape,
										vLink.STRAIGHT);

									// 			    addTempShape(tmpShapeList,fromShape);
									// 			    addTempShape(tmpShapeList,toShape);
								}
								//change label
								ListIterator lishape =
									tmpShapeList.listIterator();
								while (lishape.hasNext()) {
									changeLabel((vShape) lishape.next());
								}

								//reset tempList
								// 			tempList = new ArrayList();
								repaint();
								break;
						}

						i++;
						break;
				}
			} catch (Exception ex) {
				JOptionPane.showMessageDialog(
					this,
					"The Animator cannot cope with this example.\n"
					+ ex.toString() ,
					"GIPO ERROR",
					JOptionPane.ERROR_MESSAGE,
					null);
				return;
			};
		}
	}

	private void addTempShape(List tmpShapeList, vShape vs) {
		boolean gotIt = false;
		ListIterator lishape = tmpShapeList.listIterator();
		while (lishape.hasNext()) {
			if (((vShape) lishape.next()).getID() == vs.getID())
				return;
		}
		tmpShapeList.add(vs);
	}

	private void changeLabel(vShape vs) {
		String oldLabel = vs.getLabel();
		String newLabel = getString(oldLabel, ",");
		vs.setLabel(newLabel);
	}

	/* WZ 24/7/02 */
	/**
	 * return the vshape with match the given label
	 * @param label the vshape label to match
	 * @return the vshape with match the given label
	 */
	private vShape getShapeByLabel(String label) {
		for (int m = 1; m < vShapeList.size() + 1; m++) {
			vShape vs = (vShape) getShape(m);
			Utility.debugPrint("XXX SHAPE LABEL >>>> " + vs.getLabel());
			// Ron 10/5/03 added or endsWith !!!
			if (vs.getLabel().startsWith(label) || vs.getLabel().endsWith(label)) {
				Utility.debugPrintln("stepper","HStepperCanvas \nvs.getLabel():" + vs.getLabel());
				Utility.debugPrintln("stepper","HStepperCanvas label:" + label);
				return vs;
			}
		}
		return null;
	}

	/**
	 * return the vshape for matching achieve
	 * @param label - the achieve predicate
	 * @return the vshape with match the given label
	 */
	private vShape getAchieveShape(oclPredicate label) {
		vShape vs = null;
		for (int m = 1; m < vShapeList.size() + 1; m++) {
			vs = (vShape) getShape(m);
			Utility.debugPrintln("stepper","HStepperCanvas Label " + vs.getLabel() + " " + vs.getObject().getClass().getName());
			if (((HSShapeProperty)vs.getObject()).getObject().getClass().getName().equals("jplan.ocl.oclSS")) {
				oclSS achieve = (oclSS)((HSShapeProperty)vs.getObject()).getObject();
				Utility.debugPrint("XXX Achieve SHAPE >>>> " + achieve.toStdString());
				return vs;
			}
		}
		return null;
	}
	
	/**
	 * createAchieveNode
	 * create a top level achieve node - looks like a method in the planner output
	 * @param achieve - the achieve clause as a predicate
	 * @param str - the string from the planner output
	 * @return the vShape representing the node
	 */
	private vShape createAchieveNode(oclPredicate achieve,String str){
		vShape vs = null;
		Utility.debugPrintln(achieve.toString());
		Utility.debugPrintln(str);
		return vs;
	}
}
