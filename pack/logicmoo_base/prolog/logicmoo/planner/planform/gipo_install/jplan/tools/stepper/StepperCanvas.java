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
 * StepperCanvas.java
 * 21/5/01
 * @author Weihong Zhao
*/

import javax.swing.JComponent;
import java.awt.event.*;
import java.util.List;
import java.util.ArrayList;
import java.util.ListIterator;
import java.util.Vector;
import javax.swing.*;
import java.util.Hashtable;
import java.awt.*;
import java.lang.Character;

import jplan.graphics.gTool.Graphics.vShape;
import jplan.graphics.gTool.Graphics.vLink;
/* Weihong changed/added on 8/10/2001 */
import jplan.graphics.gTool.Graphics.LinkPoint;
/* Weihong changed/added on 8/10/2001 */
import jplan.ocl.*;
import jplan.graphics.*;
import jplan.general.Utility;
import jplan.general.GipoInternalFrame;
import jplan.general.OEnviron;
import jplan.tools.animator.OperatorProperty;

/**
 * The StepperCanvas is generally a graphic displaying canvas where objects with its state information are displayed according to the result of the execution of an operator.<br> Basic mouse interactive functions includes mouse click for viewing/editing, mouse drag for relayout.
 */
public class StepperCanvas extends JGraphCanvas {
	/**
	 * parent frame
	 */
	public GipoInternalFrame parent = null; /* Weihong changed on 10/10/2001 */
	/**
	 * the initial states
	 */
	public List initStates = new ArrayList();
	/**
	 * the goal states
	 */
	public List goalStates = new ArrayList();
	private oclDomain workingDomain;
	private DynamicState dynamicState;
	private vShape curShape = null;
	private int colDistance = 60, rowDistance = 15;
	private int startingPoint = 0, startingPointOP = 40;
	private int x = startingPoint, y = startingPoint, yOP = startingPointOP;
	//starting point
	private int stateIndex = 1, operatorIndex = 1;
	private int stateSize = 0;
	private vShape tempShape;
	//     private boolean forward = true;
	private int row = 1;
	//  to save the previous position of an operator
	private int preY, preX;
	private boolean direction;
	/**
	 * to indicate the case of necessary condition
	 */
	public final int NECESSARY = 78609;
	/**
	 * to indicate the case of conditional change
	 */
	public final int CONDITIONAL = 79609;

	/**
	 * Creates a canvas allowing drawing objects' state (vShape) in batch,
	 * and support mouse click for viewing/editing, mouse drag for relayout.
	 * @param parent StepperWindow
	 */
	public StepperCanvas(GipoInternalFrame parent) {
		super();
		this.parent = parent;

		//current default width and height.
		d_Width = 40;
		d_Height = 20;
		mouseAction = SELECT;
		removeMouseMotionListener(this); /* WZ 17/5/02 */
	}

	/**
	 * Sets ocl domain 
	 * @param cur ocl domain 
	 * 
	 */
	public void setWorkingDomain(oclDomain cur) {
		workingDomain = cur;
	}

	/* Weihong added on 1/11/01 */
	/**
	 * returns ocl domain currently opened
	 * @return ocl domain currently opened
	 */
	public oclDomain getWorkingDomain() {
		return workingDomain;
	}

	/**
	 * show graphics for oclSS
	 * @param SS_initState the list of oclSS 
	 * @param SE_goalState the list of oclSE or oclSC
	 */
	public void showOclSS(List SS_initState, List SE_goalState) {
		initStates = SS_initState;
		goalStates = SE_goalState;
		showCurrentState(initStates);
		recordState();
	}

	/**
	 * Set current states
	 * @param currentStates a list of oclSS
	 * 
	 */
	public void showCurrentState(List currentStates) {
		//here we distinguish the objects in the prevail or neccessary or conditional by assigning a shapetype parameter
		ListIterator li = currentStates.listIterator();
		calculatePositionForObjects();
		stateSize = currentStates.size();
		dynamicState = new DynamicState(stateSize, d_Height, rowDistance);

		while (li.hasNext()) {
			oclSS ss = (oclSS) li.next();
			tempShape = showAsAnObject(ss);
			Utility.debugPrintln(
				"shape " + tempShape.getID() + " -- " + ss.toString());
			tempShape.setShapeID(4);
			dynamicState.addState(ss, tempShape);
		}
		setMouseAction(SELECT); //always at the selection mode
	}

	/**
	 * create another list of shapes and register in the dynamic state
	 * @param currentStates a list of oclSE or oclSC
	 * 
	 */
	private void updateDynamicState(List currentStates) {
		dynamicState.reset();
		showCurrentState(currentStates);
	}

	/**
	 * show graphics for the states as an object
	 * @param ss the list of full states
	 * @return the graphcial representation of this state
	 */
	private vShape showAsAnObject(oclSS ss) {
		y += d_Height + rowDistance;
		vShape tempShape = createVShape(x, y);
		tempShape.removeTextField(); /* WZ 1/7/02 */
		tempShape.setLabel(ss.getName());
		tempShape.setObject(ss);
		return tempShape;
	}

	/**
	 * record the current position of the object currently being edited
	 * 
	 */
	private void recordPosition() {
		preX = x;
		preY = yOP;
		// 	direction = forward;
	}

	/** 
	 * determine where to place the objects
	 * 
	 */
	private void calculatePositionForOperator() {
		recordPosition();
		calculateX();
		yOP =
			startingPointOP
				+ (row - 1) * stateSize * ((int) d_Height + rowDistance);
		//go to next row
		if (yOP + stateSize * ((int) d_Height + rowDistance) > getHeight()) {
			setScale(1 / 1.5);
		}
	}

	/**
	 * resume previous position of the operator
	 * 
	 */
	private void resumePositionForOperator() {
		x = preX;
		yOP = preY;
		// 	forward = direction;
	}

	/**
	 * determine where to place the objects (the top left point)
	 *    
	 */
	private void calculatePositionForObjects() {
		calculateX();
		y =
			startingPoint
				+ (row - 1) * stateSize * ((int) d_Height + rowDistance);
		if (y + stateSize * ((int) d_Height + rowDistance) > getHeight()) {
			setScale(1 / 1.5);
		}
	}

	/**
	 * zoom in/out: more than 1 is zoom in; less than 1 is zoom out
	 *    
	 */
	public void setScale(double zoom) {
		super.setScale(zoom);
		startingPoint *= zoom;
		startingPointOP *= zoom;
		colDistance *= zoom;
		rowDistance *= zoom;
		resetPosition();
	}

	/**
	 * calculate the position the object at x axis
	 *    
	 */
	private void calculateX() {
		//         if (x + d_Width + colDistance > getWidth() && forward) {
		// 	    forward = false;
		// 	    row ++;
		// 	}
		// 	else if  (x - d_Width - colDistance <= 0 && !forward) {
		// 	    forward = true;
		// 	    row ++;
		// 	}

		// 	if (forward)
		x += d_Width + colDistance;
		// 	else
		// 	    x -= d_Width + colDistance;
	}

	/**
	 * remove a particular shape (not considering it's in- and out links)
	 * @param vs vShape to be removed
	 *   
	 */
	public void deleteShape(vShape vs) {
		vShapeList.remove(getShapeKey(vs));
		remove(vs);
		refreshShapeList();
		repaint();
	}

	/**
	 * get shape key registered in the vShapeList hashtable
	 * @param vs vShape
	 *   
	 */
	private String getShapeKey(vShape vs) {
		String shapeKey = null;
		for (int i = 1; i < vShapeList.size() + 1; i++) {
			shapeKey = "shapeKey" + i;
			vShape tmpShape = (vShape) getShape(i);
			if (tmpShape.getID() == vs.getID()) {
				return shapeKey;
			}
		}
		return null;
	}

	/* Weihong 18/2/02 */
	/**
	 * 
	 */
	private boolean checkMethod(oclMethod om, List mssgs) {
		//check every dedomposition unit
		//create a methodHeadCanvas then draw decompositions on it, but not show it.
		jplan.graphics.transition.MethodHeadCanvas tempCanvas =
			new jplan.graphics.transition.MethodHeadCanvas(workingDomain);
		/* WZ 4/4/02 */
		jplan.graphics.transition.MethodHeadCanvas.setDecomposition(
			tempCanvas,
			tempCanvas.getWorkingDomain(),
			om);

		Hashtable tmpSPList = tempCanvas.getShapeList();
		Hashtable dymSPList = new Hashtable();
		int j = 1;
		//find shapes without inlinks but with outlinks
		for (int i = 1; i < tmpSPList.size() + 1; i++) {
			tempShape = (vShape) tempCanvas.getShape(i);
			//find the first shapes in the chain
			if (tempShape.getInLinks().size() == 0) {
				if (tempShape.getOutLinks().size() != 0) {
					dymSPList.put(
						"shapeKey" + j,
						(Object) String.valueOf(tempShape.getID()));
					j++;
				} else {
					JOptionPane.showMessageDialog(
						this,
						"Found isolated operator in the decomposition of method"
							+ om.getName(),
						"GIPO ERROR",
						JOptionPane.ERROR_MESSAGE,
						null);
					return false;
				}
			}
		}

		//reset current state to next column
		List curState = new ArrayList();
		ListIterator li = initStates.listIterator();
		curState.add(li.next()); //clone the initial states

		//for every shape in the dynamic shape list, check its immediate next shape, if the shape's all inShapes have been executed then check this current shape, otherwise wait.
		while (dymSPList.size() > 0) {
			Hashtable tempSPList = new Hashtable();
			int n = 1;
			for (int x = 1; x < dymSPList.size() + 1; x++) {
				tempShape = (vShape) tempCanvas.getShape(x);
				Object obj = (Object) tempShape.getObject();
				if (obj.getClass().getName() == "jplan.ocl.oclSS") {
					oclSS ss = (oclSS) obj;
					if (!checkState(curState,
						ss.getName(),
						ss.getState(),
						mssgs)) {
						return false;
					}
				} else if (
					obj.getClass().getName() == "jplan.ocl.oclPredicate") {
					Object object = checkObjectType((oclPredicate) obj);
					if (object.getClass().getName() == "jplan.ocl.oclMethod") {
						oclMethod omd = (oclMethod) object;
						return checkMethod(omd, mssgs);
					} else if (
						object.getClass().getName()
							== "jplan.ocl.oclOperator") {
						oclOperator op = (oclOperator) object;
						return checkOperator(curState, op);
					}
				}
				tempShape.setSelected(false); //used to mark the execution.
				//check next shape and update dynamic shape list
				for (int k = 1; k < tempShape.getOutLinks().size() + 1; k++) {
					vLink tmpLink =
						(vLink) tempShape.getOutLinks().get("outlinks" + k);
					vShape tmpSP = tmpLink.getStopShape();
					if (checkShape(tmpSP)) {
						tempSPList.put("shapeKey" + n, tmpSP);
						n++;
					}
				}
			}

			dymSPList.clear();
			dymSPList = tempSPList;
		}
		return true;
	}

	/* Weihong 18/2/02 */
	/**
	 * to find out a name (oclPredicate) stands for oclMethod or oclOperator
	 * @param oprd oclPredicate
	 * @return object
	 */
	private Object checkObjectType(oclPredicate oprd) {
		oclPredicate checkOPD = null;
		//check methods first
		oclMethod returnMethod = null;
		ListIterator li = workingDomain.methods.listIterator();
		while (li.hasNext()) {
			returnMethod = (oclMethod) li.next();
			checkOPD = (oclPredicate) returnMethod.getName();
			if (checkOPD.isSameType(oprd)) {
				return returnMethod;
			}
		}

		//check operator next
		oclOperator returnOperator = null;
		li = workingDomain.operators.listIterator();
		while (li.hasNext()) {
			returnOperator = (oclOperator) li.next();
			checkOPD = (oclPredicate) returnOperator.opName;
			if (checkOPD.isSameType(oprd)) {
				return returnOperator;
			}
		}

		return null;
	}

	/* Weihong 18/2/02 */
	/**
	 * to check a vShape's next shapes 
	 * and find out if their immediate inShapes have been executed.
	 * @param theSP vShape
	 * @return true if all its immediate inShapes have been executed.
	 */
	private boolean checkShape(vShape theSP) {
		for (int i = 1; i < theSP.getInLinks().size() + 1; i++) {
			vLink tmpLink = (vLink) theSP.getInLinks().get("inlinks" + i);
			vShape tmpSP = tmpLink.getStartShape();
			if (!tmpSP.getSelected())
				//check if the shape has been marked to invisible
				return false;
		}
		return false;
	}

	/**
	 * to check if there are some states
	 * that match the transition's left hand side condition 
	 * @param op oclOperator
	 * @param mssgs the message list to populate with errors
	 * @return boolean
	 */
	// Ron 21/8/01 - error messages
	private boolean findStates(oclOperator op, List mssgs) {
		ListIterator li;
		//oclSE
		oclSE se = null;
		oclSC sc = null;
		li = op.getPrevail().listIterator();
		while (li.hasNext()) {
			se = (oclSE) li.next();
			mssgs.add("Finding state for prevailing object " + se.getName());
			if (!unifiesCurrentState(workingDomain,
				dynamicState,
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
			if (!unifiesCurrentState(workingDomain,
				dynamicState,
				sc.getName(),
				sc.getPre(),
				mssgs)
				|| !checkStaticsAndBuiltIns(workingDomain, sc.getPost(), mssgs)) {
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
	 * unifies Current State With Given State
	 * that match the transition's left hand side condition.
	 * @param stateList List of oclPredicate (no ne or static)
	 * @param mssgs the message list to add error messages
	 * @return boolean
	 */
	// Ron 21/8/01 Added error messages
	// Ron 29/8/01 Added checking of statics etc
	public static boolean unifiesCurrentState(
		oclDomain workingDomain,
		DynamicState dynamicState,
		String objectName,
		List stateList,
		List mssgs) {
		List curStateList = dynamicState.getState();
		return unifiesCurrentState(
			workingDomain,
			curStateList,
			objectName,
			stateList,
			mssgs);
	}

	/**
	 * unifies Current State With Given State
	 * that match the transition's left hand side condition.
	 * @param stateList List of oclPredicate (no ne or static)
	 * @param mssgs the message list to add error messages
	 * @return boolean
	 */
	// Ron 21/8/01 Added error messages
	// Ron 29/8/01 Added checking of statics etc
	public static boolean unifiesCurrentState(
		oclDomain workingDomain,
		List curStateList,
		String objectName,
		List stateList,
		List mssgs) {
		Utility.debugPrintln("Looking for state of " + objectName);
		ListIterator li = curStateList.listIterator();
		while (li.hasNext()) {
			oclSS ss = (oclSS) li.next();
			if (objectName.equals(ss.getName())) {
				//check every predicate in this state
				// NOTE Next few lines debugging only
				Utility.debugPrintln("Matching State = " + ss.toString());
				
				if (checkStaticsAndBuiltIns(workingDomain, stateList, mssgs)) {
						return unifiesSSwithSE(
							ss.getState(),
							toStrip(stateList),
							mssgs);
				} else {
					return false;
				}
			}
		}
		mssgs.add("NO state for this object");
		return false;
	}

	// Ron 8/4/03
	/**
	 * unifies Current State With Given State
	 * that match the transition's left hand side condition.
	 * State may onlu be partially instantiated
	 * @param stateList List of oclPredicate (no ne or static)
	 * @param env environment of bindings 
	 * @param mssgs the message list to add error messages
	 * @return boolean
	 */
	// Ron 21/8/01 Added error messages
	// Ron 29/8/01 Added checking of statics etc
	public static boolean unifiesAbstractCurrentState(
		oclDomain workingDomain,
		DynamicState dynamicState,
		String objectName,
		List stateList,
		OEnviron env,
		List mssgs) {
		List curStateList = dynamicState.getState();
		return unifiesAbstractCurrentState(
			workingDomain,
			curStateList,
			objectName,
			stateList,
			env,
			mssgs);
	}

	// Ron 8/4/03
	/**
	 * unifies Current State With Given State
	 * that match the transition's left hand side condition.
	 * @param stateList List of oclPredicate (no ne or static)
	 * @param env  environment of variable bindings
	 * @param mssgs the message list to add error messages
	 * @return boolean
	 */
	// Ron 21/8/01 Added error messages
	// Ron 29/8/01 Added checking of statics etc
	public static boolean unifiesAbstractCurrentState(
		oclDomain workingDomain,
		List curStateList,
		String objectName,
		List stateList,
		OEnviron env,
		List mssgs) {
		Utility.debugPrintln("Looking for state of " + objectName);
		ListIterator li = curStateList.listIterator();
		while (li.hasNext()) {
			oclSS ss = (oclSS) li.next();
			if (objectName.equals(ss.getName())) {
				//check every predicate in this state
				// NOTE Next few lines debugging only
				Utility.debugPrintln("Matching State = " + ss.toString());
				ListIterator templi = stateList.listIterator();
				while (templi.hasNext()) {
					oclPredicate temp = (oclPredicate) templi.next();
					Utility.debugPrintln(
						">+>+>+ State predicate " + temp.toString());

				}
				// End debug lines
				if (unifiesSSwithAbstractSE(ss.getState(),
					toStrip(stateList),
					env,
					mssgs)) {
					// Mmm should I clone?
					instantiate_state(stateList, env);
					// More debug Lines
					templi = stateList.listIterator();
					while (templi.hasNext()) {
						oclPredicate temp = (oclPredicate) templi.next();
						Utility.debugPrintln(
							">>>+ State predicate " + temp.toString());

					}
					// End debug lines
					if ( checkAbstractStaticsAndBuiltIns(
						workingDomain,
						stateList,
						env,
						mssgs)) {
						instantiate_state(stateList, env);
						//More debug Lines
						templi = stateList.listIterator();
						while (templi.hasNext()) {
							oclPredicate temp = (oclPredicate) templi.next();
							Utility.debugPrintln(
							">>>+ State predicate " + temp.toString());

						}
						// End debug lines	
						return true;
					} else {
						return false;	
					}
						
					
				} else {
					return false;
				}

				//				if (checkStaticsAndBuiltIns(workingDomain, stateList, mssgs)) {
				//					return unifiesSSwithAbstractSE(
				//						ss.getState(),
				//						toStrip(stateList),
				//						mssgs);
				//				} else {
				//					return false;
				//				}
			}
		}
		mssgs.add("NO state for this object");
		return false;
	}


	/* Weihong 18/2/02 */
	/**
	 * unifies Current State With Given State
	 * that match the transition's left hand side condition.
	 * @param stateList List of oclPredicate (no ne or static)
	 * @param mssgs the message list to add error messages
	 * @return boolean
	 */
	private boolean checkState(
		List curState,
		String objectName,
		List stateList,
		List mssgs) {
		Utility.debugPrintln("Looking for state of " + objectName);
		ListIterator li = curState.listIterator();
		while (li.hasNext()) {
			oclSS ss = (oclSS) li.next();
			if (objectName.equals(ss.getName())) {
				//check every predicate in this state
				Utility.debugPrintln("Matching State = " + ss.toString());
				if (checkStaticsAndBuiltIns(workingDomain, stateList, mssgs)) {
					return unifiesSSwithSE(
						ss.getState(),
						toStrip(stateList),
						mssgs);
				} else {
					return false;
				}
			}
		}
		mssgs.add("NO state for this object");
		return false;
	}

	/**
	 * Instantiate_state
	 * given an environment replaces variables with the binding recorded in
	 * the environment
	 * @param stateList - State List Partially instantiates se
	 * @param env - environment of bindings
	 * 
	 */
	public static void instantiate_state(List stateList, OEnviron env) {
		ListIterator li = stateList.listIterator();
		while (li.hasNext()) {
			oclPredicate cur = (oclPredicate) li.next();
			cur.instantiate(env);
		}
	}

	/**
	 * to check an operator against the given states
	 * if all unifies, then apply the post states
	 * @param givenCurStates the current states used in between the method check
	 * @param op oclOperator
	 * @return true if the operator conditions unify with the current states
	 */
	public boolean checkOperator(List givenCurStates, oclOperator op) {
		List mssgs = new ArrayList();
		ListIterator li;
		//check precoditons - oclSE
		oclSE se = null;
		oclSC sc = null;
		li = op.getPrevail().listIterator();
		while (li.hasNext()) {
			se = (oclSE) li.next();
			mssgs.add(
				"Finding state for preconditional object " + se.getName());
			if (!checkState(givenCurStates,
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
		li = op.getNecessary().listIterator();
		while (li.hasNext()) {
			sc = (oclSC) li.next();
			mssgs.add("Finding state for index transition for " + sc.getName());
			if (!checkState(givenCurStates, sc.getName(), sc.getPre(), mssgs)
				|| !checkStaticsAndBuiltIns(workingDomain, sc.getPost(), mssgs)) {
				Utility.debugPrintln(
					"OBJECT "
						+ sc.getName()
						+ " FAILED to unify state with index transition.");
				return false;
			}
		}

		opInstDialog(mssgs);

		//draw links from the curstate to this operator
		int swit = 0;

		//oclSE - Prevail

		//oclSC - Necessary
		li = op.getNecessary().listIterator();
		while (li.hasNext()) {
			sc = (oclSC) li.next();
			updateStates(givenCurStates, sc.getName(), sc.getPost());
		}
		//oclSC - Conditional
		li = op.getConditional().listIterator();
		while (li.hasNext()) {
			String objName = null;
			String before = null;
			Vector vector = new Vector();
			sc = (oclSC) li.next();
			String scName = sc.getName();
			if (Character.isUpperCase(scName.charAt(0))) {
				//if the object has not be instantiated
				//find sort of this varible
				List objectList = workingDomain.getObjectsOfSort(sc.getSort());
				ListIterator obLi = objectList.listIterator();
				while (obLi.hasNext()) {
					objName = obLi.next().toString();
					before = sc.getName();
					sc.replaceVariableName(before, objName);
					if (checkState(givenCurStates,
						sc.getName(),
						sc.getPre(),
						mssgs)) {
						vector.addElement(objName);
					}
				}
			} else {
				vector.addElement(scName);
			}

			for (int i = 0; i < vector.size(); i++) {
				objName = vector.elementAt(i).toString();
				before = sc.getName();
				sc.replaceVariableName(before, objName);
				updateStates(givenCurStates, sc.getName(), sc.getPost());
			}
		}
		return true;
	}

	/* Weihong 18/2/02 */
	/**
	 *
	 */
	private void updateStates(List oldStates, String objName, List newStates) {
		ListIterator oldli = oldStates.listIterator();
		while (oldli.hasNext()) {
			oclSS ss = (oclSS) oldli.next();
			if (objName.equals(ss.getName())) {
				ss.setState(newStates);
				return;
			}
		}
	}

	/**
	 * find ne & is_of_sort & statics and check
	 * @param workingDomain
	 * @param preds predicate list
	 * @param mssgs Error message list
	 * @return boolean true if all checks passed
	 */
	public static boolean checkStaticsAndBuiltIns(
		oclDomain workingDomain,
		List preds,
		List mssgs) {
		ListIterator li = preds.listIterator();
		boolean allOK = true;
		while (allOK && li.hasNext()) {
			oclPredicate cur = (oclPredicate) li.next();
			if ("ne".equals(cur.getName())) {
				if (cur
					.getNthElementName(0)
					.equals(cur.getNthElementName(1))) {
					allOK = false;
					mssgs.add(
						"The inequality " + cur.toString() + " is false.");
				}
			} else if ("is_of_sort".equals(cur.getName())) {
				String objSort =
					workingDomain.getSortOfObject(cur.getNthElementName(0));
				String reqSort = cur.getNthElementName(1);
				if (!objSort.equals(reqSort)
					&& !workingDomain.sortIsSubSortOf(objSort, reqSort)) {
					allOK = false;
					mssgs.add(
						"The sort restriction "
							+ cur.toString()
							+ " is not met.");
				}
			} else if (cur.isStatic()) {
				Utility.debugPrintln("Finding Match for " + cur.toString());
				ListIterator liAtomic =
					workingDomain.atomicInvars.listIterator();
				boolean found = false;
				while (!found && liAtomic.hasNext()) {
					oclPredicate atom = (oclPredicate) liAtomic.next();
					if (cur.equals(atom)) {
						found = true;
					}
				}
				if (!found) {
					allOK = false;
					mssgs.add(
						"The atomic invariant "
							+ cur.toString()
							+ " is not met.");
				}
			}
		}
		return allOK;

	}

	/**
	 * find ne & is_of_sort & statics and check
	 * Statics may not be fully instantiated
	 * @param workingDomain
	 * @param preds predicate list
	 * @param env
	 * @param mssgs Error message list
	 * @return boolean true if all checks passed
	 */
	public static boolean checkAbstractStaticsAndBuiltIns(
		oclDomain workingDomain,
		List preds,
		OEnviron env,
		List mssgs) {
		ListIterator li = preds.listIterator();
		OEnviron envTemp = new OEnviron();
		envTemp.copy(env);
		boolean allOK = true;
		while (allOK && li.hasNext()) {
			oclPredicate cur = (oclPredicate) li.next();
			if ("ne".equals(cur.getName())) {
				if (cur
					.getNthElementName(0)
					.equals(cur.getNthElementName(1))) {
					allOK = false;
					mssgs.add(
						"The inequality " + cur.toString() + " is false.");
				}
			} else if ("is_of_sort".equals(cur.getName())) {
				String objSort =
					workingDomain.getSortOfObject(cur.getNthElementName(0));
				String reqSort = cur.getNthElementName(1);
				if (!objSort.equals(reqSort)
					&& !workingDomain.sortIsSubSortOf(objSort, reqSort)) {
					allOK = false;
					mssgs.add(
						"The sort restriction "
							+ cur.toString()
							+ " is not met.");
				}
			} else if (cur.isStatic()) {
				Utility.debugPrintln("Finding Match for " + cur.toString());
				ListIterator liAtomic =
					workingDomain.atomicInvars.listIterator();
				boolean found = false;
				while (!found && liAtomic.hasNext()) {
					oclPredicate atom = (oclPredicate) liAtomic.next();
					OEnviron envCopy = null;
					try {
						envCopy = (OEnviron) envTemp.clone();
					} catch (CloneNotSupportedException e) {
						// Ignore
					}
					if (cur.unify(atom,envCopy)) {
						found = true;
						envTemp = envCopy;
					}
				}
				if (!found) {
					allOK = false;
					mssgs.add(
						"The atomic invariant "
							+ cur.toString()
							+ " is not met.");
				}
			}
		}
		env.copy(envTemp);
		return allOK;

	}

	/**
	 * check between two states to see if the 
	 * given states are all included in the current states.
	 * @param SSList List of oclPredicate (SS)
	 * @param SEList List of oclPredicate (SE)
	 * @param mssgs the error message list
	 * @return boolean
	 */
	public static boolean unifiesSSwithSE(
		List SSList,
		List SEList,
		List mssgs) {
		List temp = new ArrayList();
		boolean found = false;
		ListIterator liSE = SEList.listIterator();
		while (liSE.hasNext()) { //for every predicate in the SE
			oclPredicate oprd = (oclPredicate) liSE.next();
			ListIterator liSS = SSList.listIterator();
			if (SSList.size() == 0) {
				mssgs.add("HELP no predicates in the state for the object");
			}
			while (liSS.hasNext()) { //for every predicate in the SS
				oclPredicate basePred = (oclPredicate) liSS.next();
				if (oprd.equals(basePred)) {
					Utility.debugPrintln("Found " + oprd.toString());
					found = true;
					break;
				} else {
					temp.add(
						"    "
							+ oprd.toString()
							+ " << fails to match >>  "
							+ basePred.toString());
				}

			}
			if (!found) { //for every predicate in the SE there must be an exact match in SS
				mssgs.addAll(temp);
				return false;
			} else
				found = false; //reset found for next iteration
		}
		return true;
	}

	// Ron 8/4/03	
	/**
	 * check between two states to see if the 
	 * given states are all included in the current states.
	 * The SE list may only be partially instantiated
	 * @param SSList List of oclPredicate (SS)
	 * @param SEList List of oclPredicate (SE)
	 * @param env Environment of bindings
	 * @param mssgs the error message list
	 * @return boolean
	 */
	public static boolean unifiesSSwithAbstractSE(
		List SSList,
		List SEList,
		OEnviron env,
		List mssgs) {
		OEnviron envTemp = new OEnviron();
		List temp = new ArrayList();
		boolean found = false;
		ListIterator liSE = SEList.listIterator();
		while (liSE.hasNext()) { //for every predicate in the SE
			oclPredicate oprd = (oclPredicate) liSE.next();
			ListIterator liSS = SSList.listIterator();
			if (SSList.size() == 0) {
				mssgs.add("HELP no predicates in the state for the object");
			}
			while (liSS.hasNext()) { //for every predicate in the SS
				oclPredicate basePred = (oclPredicate) liSS.next();
				OEnviron envCopy = null;
				try {
					envCopy = (OEnviron) envTemp.clone();
				} catch (CloneNotSupportedException e) {
					// Ignore
				}
				if (oprd.unify(basePred, envCopy)) {
					Utility.debugPrintln("Found " + oprd.toString());
					found = true;
					envTemp = envCopy;
					break;
				} else {
					temp.add(
						"    "
							+ oprd.toString()
							+ " << fails to match >>  "
							+ basePred.toString());
				}

			}
			if (!found) { //for every predicate in the SE there must be an exact match in SS
				mssgs.addAll(temp);
				return false;
			} else
				found = false; //reset found for next iteration
		}
		env.copy(envTemp);
		return true;
	}

	/*Weihong 14/3/02 */
	/**
	 * return a list of current actions
	 * @return String
	 */
	public List getCurActionList() {
		List returnList = new ArrayList();
		for (int i = 1; i < vShapeList.size() + 1; i++) {
			vShape vs = (vShape) getShape(i);
			if (vs.getShapeID() == 0) //operator
				returnList.add(((oclOperator) vs.getObject()).opName);
			if (vs.getShapeID() == 5) //method
				returnList.add(((oclMethod) vs.getObject()).getName());
		}
		return returnList;
	}

	/**
	 * show operator's graphics on the screen
	 * @param op oclOperator
	 */
	// Ron 21/8/01 added new error display routines
	public void showAsAOperator(oclOperator op) {
		List mssgs = new ArrayList();
		if (vShapeList.size() == 0) {
			JOptionPane.showMessageDialog(
				parent,
				"No Initial States are Specified.",
				"Operator Instantiation Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}
		if (!findStates(op, mssgs)) {
			opInstDialog(mssgs);
			return;
		}
		vShape operatorShape;
		calculatePositionForOperator();
		d_Height *= 3; /* WZ 3/7/02 */
		d_Width *= 2; /* WZ 3/7/02 */
		tempShape = createVShape((int) (x - d_Width / 4), yOP);
		tempShape.removeTextField(); /* WZ 1/7/02 */
		d_Height /= 3; /* WZ 3/7/02 */
		d_Width /= 2; /* WZ 3/7/02 */
		tempShape.setShapeID(vShape.CIRCLE);
		tempShape.setLabel(op.opName.getName());
		operatorIndex++;
		tempShape.setObject(op);
		operatorShape = tempShape;

		//reset dynamic state to next column
		DynamicState old_dynamicSate = (DynamicState) dynamicState.clone();
		updateDynamicState(old_dynamicSate.getState());

		//draw links from the curstate to this operator
		int swit = 0;
		ListIterator li;

		//oclSE - Prevail
		li = op.getPrevail().listIterator();
		while (li.hasNext()) {
			oclSE se = (oclSE) li.next();
			vShape vp = old_dynamicSate.findMatchingShape(se.getName());
			if (vp != null) {
				setDrawingLinkID(0);
				createVLink(vp, operatorShape);
			} else {
				deleteShape(operatorShape);
				resumePositionForOperator();
				JOptionPane.showMessageDialog(
					parent,
					"Operator needs to be fully instantiated.",
					"Operator Instantiation Error",
					JOptionPane.ERROR_MESSAGE,
					null);
				return;
			}
		}
		//oclSC - Necessary
		li = op.getNecessary().listIterator();
		swit = NECESSARY;
		setDrawingLinkID(1);
		while (li.hasNext()) {
			oclSC sc = (oclSC) li.next();
			vShape vp = old_dynamicSate.findMatchingShape(sc.getName());
			if (vp != null) {
				createVLink(vp, operatorShape);
				showNewStates(operatorShape, sc, swit);
			} else {
				deleteShape(operatorShape);
				resumePositionForOperator();
				JOptionPane.showMessageDialog(
					parent,
					"Operator needs to be fully instantiated.",
					"Operator Instantiation Error",
					JOptionPane.ERROR_MESSAGE,
					null);
				return;
			}
		}
		//oclSC - Conditional
		li = op.getConditional().listIterator();
		swit = CONDITIONAL;
		setDrawingLinkID(2);
		while (li.hasNext()) {
			String objName = null;
			String before = null;
			Vector vector = new Vector();
			oclSC sc = (oclSC) li.next();
			String scName = sc.getName();
			if (Character.isUpperCase(scName.charAt(0))) {
				//if the object has not be instantiated
				//find sort of this varible
				List objectList = workingDomain.getObjectsOfSort(sc.getSort());
				ListIterator obLi = objectList.listIterator();
				while (obLi.hasNext()) {
					objName = obLi.next().toString();
					before = sc.getName();
					sc.replaceVariableName(before, objName);
					if (unifiesCurrentState(workingDomain,
						dynamicState,
						sc.getName(),
						sc.getPre(),
						mssgs)) {
						vector.addElement(objName);
					}
				}
			} else {
				vector.addElement(scName);
			}

			for (int i = 0; i < vector.size(); i++) {
				objName = vector.elementAt(i).toString();
				before = sc.getName();
				sc.replaceVariableName(before, objName);
				vShape vp = old_dynamicSate.findMatchingShape(objName);
				if (vp != null) {
					createVLink(vp, operatorShape);
					showNewStates(operatorShape, sc, swit);
				} else {
					deleteShape(operatorShape);
					resumePositionForOperator();
					JOptionPane.showMessageDialog(
						parent,
						"Operator needs to be fully instantiated.",
						"Operator Instantiation Error",
						JOptionPane.ERROR_MESSAGE,
						null);
					return;
				}
			}
		}

		recordState();
		repaint();
		checkGoalState();
	}

	/**
	 * to check if the goal state has been reached
	 * 
	 */
	private void checkGoalState() {
		List mssgs = new ArrayList();
		ListIterator li = goalStates.listIterator(); //oclSE
		while (li.hasNext()) {
			oclSE se = (oclSE) li.next();
			if (!unifiesCurrentState(workingDomain,
				dynamicState,
				se.getName(),
				se.getState(),
				mssgs))
				return;
		}

		JOptionPane.showMessageDialog(
			this,
			"Congratulations! The goal has been reached.",
			"OCL Confirm",
			JOptionPane.INFORMATION_MESSAGE,
			null);
	}

	/**
	 * get rid of ne or static predicates for this state list
	 * @param stateList state list
	 * @return a clean list of states
	 */
	// Ron 29/8/01 remove is_of_sort as well
	public static List toStrip(List stateList) {
		List list = new ArrayList();
		ListIterator li = stateList.listIterator();
		while (li.hasNext()) {
			oclPredicate oprd = (oclPredicate) li.next();
			if (!oprd.getName().equals("ne")
				&& !oprd.getName().equals("is_of_sort")) {
				if (!oprd.isStatic()) {
					list.add(oprd);
					Utility.debugPrintln(
						"Keeped "
							+ oprd.toString()
							+ " -- isStatic(): "
							+ oprd.isStatic());
				}
			}
		}
		return list;
	}

	/**
	 * show single state after transition
	 * @param operator the vshape which represents an oclOperator whose transition we are interested
	 * @param NesORCond distinguish from necessary or conditional change in an oclOperator
	 * @param oclsc oclSC
	 */
	private void showNewStates(vShape operator, oclSC oclsc, int NesORCond) {
		/* Weihong added on 3/9/01 */
		oclSC sc = null;
		try {
			sc = (oclSC) oclsc.clone();
		} catch (CloneNotSupportedException e) {
			Utility.debugPrintln(e);
		}
		/* Weihong added end */

		vShape tempShape = dynamicState.findMatchingShape(sc.getName());
		if (NesORCond == NECESSARY)
			tempShape.setShapeID(2);
		else if (NesORCond == CONDITIONAL)
			tempShape.setShapeID(3);
		else
			Utility.debugPrintln("Error: shape tyepID has not been set.");

		oclSS ss = new oclSS(sc.getSort(), sc.getName());
		ss.setState(sc.getPost());
		dynamicState.refresh(sc.getPre(), ss, tempShape);
		createVLink(operator, tempShape);
	}

	/**
	 * overwrite parent's to disable the function of reshape
	 * @param me MouseEvent
	 * 
	 */
	public synchronized void mouseDragged(MouseEvent me) {
		if (mouseAction == SELECT) {
			int i, k;
			//check all the shapes
			for (i = 1; i < vShapeList.size() + 1; i++) {
				vShape vs = (vShape) getShape(i);
				if (vs.getSelected()) {
					dragged = true;
					setCursor(Cursor.getPredefinedCursor(Cursor.MOVE_CURSOR));
					vs.setPosition(
						(double) (me.getX() - vs.offsetX),
						(double) (me.getY() - vs.offsetY));

					/* WZ 15/5/02 */
					//resize the canvas when required
					if (vs.px + vs.width > getBounds().width + 100) {
						setSize(
							(int) (vs.px + vs.width + 100),
							getBounds().height);
					}
					if (vs.py + vs.height > getBounds().height + 100) {
						setSize(
							getBounds().width,
							(int) (vs.py + vs.height + 100));
					}
					repaint();
				}
			}

			/* Weihong changed/added on 8/10/2001 */
			//check all the links
			for (i = 1; i < linkList.size() + 1; i++) { //for every vlink ...
				vLink vl = (vLink) linkList.get("linkKey" + i);
				if (vl.getSelected()) {
					for (LinkPoint lp = vl.getStartPoint().getNext();
						;
						lp = lp.getNext()) {
						//for every link point except the start and stop point ...
						if (lp
							.contains((double) me.getX(), (double) me.getY())) {
							lp.setPosition(
								(double) me.getX(),
								(double) me.getY());
							repaint();
							dragged = true;
							break;
						}
						if (lp.getNext() == vl.getStopPoint())
							break;
					}
				}
			}
			/* end Weihong changed/added on 8/10/2001 */

			if (!dragged) {
				dragSelect = true;
				mouseCurrentPoint = new Point(me.getPoint());
				repaint();
			}
		}
	}

	/**
	 * dobule click to show the property
	 * @param me MouseEvent
	 * 
	 */
	public void mouseClicked(MouseEvent me) {
		if (me.getClickCount() == 2) {
			vShape vs = null;
			for (int i = 1; i < vShapeList.size() + 1; i++) {
				vs = (vShape) getShape(i);
				if (vs.contains((double) me.getX(), (double) me.getY())) {
					if (vs.getShapeID() > 0) {
						showStatePropertyWindow(vs, me);
					}

					if (vs.getShapeID() == 0) {
						//check shapelist to see if there at least one shape was highlighted.
						OperatorProperty pw =
							new OperatorProperty(
								workingDomain,
								(oclOperator) vs.getObject(),
								parent,
								parent.getTheParent().strImageDir);
						pw.setLocation(me.getX(), me.getY());
						pw.show();
					}
				}
			}
		}
	}

	/**
	 * bring up the state property window
	 * @param me MouseEvent
	 * @param vshape vShape which has been double clicked on
	 * 
	 */
	public void showStatePropertyWindow(vShape vshape, MouseEvent me) {
		ObjectProperty statePropertyWindow =
			new ObjectProperty(
				parent.getTheParent(),
				(List) ((oclSS) vshape.getObject()).getState(),
				vshape.getLabel());
		statePropertyWindow.setLocation(me.getX(), me.getY());
		statePropertyWindow.show();
	}

	/**
	 * regain dynamic state after undo/redo
	 * 
	 */
	private void resetDynamicState() {
		dynamicState.reset();
		for (int i = vShapeList.size() - stateSize + 1;
			i < vShapeList.size() + 1;
			i++) {
			vShape vs = (vShape) getShape(i);
			dynamicState.addState((oclSS) vs.getObject(), vs);
		}
	}

	/**
	 * undo the last recorded action
	 * 
	 */
	public void undo() {
		super.undo();
		resetDynamicState();
		resetPosition();
	}

	/*
	 * redo the last undo action
	 * 
	 */
	public void redo() {
		super.redo();
		resetDynamicState();
		resetPosition();
	}

	/**
	 * reset the default position of the object after undo/redo
	 * 
	 */
	private void resetPosition() {
		int i = vShapeList.size() - stateSize + 1;
		vShape vs = (vShape) getShape(i);
		x = (int) vs.px;
		y = (int) vs.py;
	}

	/**
	 * used to display a scrolling list of error messages
	 * indicating where operator instantiation failed
	 * @param mssgs the list of error messages
	 */
	// Ron added 21/8/01 to improve failure to instantiate operator messages
	public static void opInstDialog(java.util.List mssgs) {
		JPanel jpanMssgs = new JPanel();
		jpanMssgs.setBorder(
			BorderFactory.createTitledBorder("Instantiation Messages"));
		jpanMssgs.setLayout(new BorderLayout());
		JScrollPane jScrollPane = new JScrollPane();
		JTextArea jtxtMessages = new JTextArea();
		jtxtMessages.setLineWrap(true);
		jtxtMessages.setEditable(false);
		jScrollPane.setViewportView(jtxtMessages);
		jpanMssgs.add(jScrollPane, java.awt.BorderLayout.CENTER);
		jpanMssgs.setPreferredSize(new Dimension(400, 200));
		java.util.ListIterator li = mssgs.listIterator();
		jtxtMessages.setText("");
		while (li.hasNext()) {
			jtxtMessages.append((String) li.next() + "\n");
		}
		JOptionPane.showMessageDialog(
			null,
			jpanMssgs,
			"GIPO Information",
			JOptionPane.ERROR_MESSAGE,
			null);

	}
}
