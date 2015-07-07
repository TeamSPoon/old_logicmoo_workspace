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
 * Created on 27-Jun-2003
 *
 * Author ron
 * 
 */
package jplan.tools.sketch;

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

import jplan.top.OclEd;
import jplan.graphics.gTool.Graphics.vShape;
import jplan.graphics.gTool.Graphics.vLink;
import jplan.graphics.gTool.Graphics.LinkPoint;
import jplan.ocl.*;
import jplan.graphics.*;
import jplan.general.Utility;
import jplan.general.GipoInternalFrame;
import jplan.general.OEnviron;
import jplan.tools.animator.OperatorProperty;

/**
 * @author ron
 *
 * This is the drawing canvas for the sketcher
 * We draw the plan on this
 */
public class SketchCanvas extends JGraphCanvas {
	/**
	 * The domain being used
	 */
	private oclDomain workingDomain = null;
	/**
	 * parent frame
	 */
	private GipoInternalFrame parent = null;
	/**
	 * The top level OclEd
	 */
	private OclEd top = null;
	/**
	 * The clock vShape
	 */
	private vShape clock;
	/**
	 * The list of actions currently in the plan
	 */
	private List lstActions;
	/**
	 * The list of processses currently in the plan
	 */
	private List lstProcs;

	/**
	 * The inter line gap for actions on the time line
	 */
	private static final int GAP = 2;
	/**
	 * The Start y Coordinate for processes
	 */
	private static final int PROCSTART = 100;
	
	/**
	 * The initial state for the problem
	 */
	private List initState;

	/**
	 * Creates a canvas allowing drawing objects' state (vShape) in batch,
	 * and support mouse click for viewing/editing, mouse drag for relayout.
	 * @param parent StepperWindow
	 */
	public SketchCanvas(GipoInternalFrame parent) {
		super(); // This adds the mouse listener
		this.parent = parent;
		lstActions = new ArrayList();
		lstProcs = new ArrayList();
		//current default width and height.
		d_Width = 80;
		d_Height = 20;
		mouseAction = SELECT;
		setDrawingShapeID(1);
		createClockVShape(2, 4);
	}

	/**
	 * Sets to editing frame 
	 * @param cur ocl domain 
	 * @return void
	 */
	public void setTop(OclEd top) {
		this.top = top;
	}

	/**
	 * Sets ocl domain 
	 * @param cur ocl domain 
	 * @return void
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
	 * Create the vShape at the given location. <br>
	 * Register with the vShapeList;
	 * Add it to the screen; 
	 * Mark the dirty flag.
	 * @param xPoint int value at X axis
	 * @param yPoint int value at Y axis
	 */
	private void createClockVShape(int xPoint, int yPoint) {
		Clock curTime = new Clock();
		shapeIndex++;
		shapeID++;
		int d_Width = 60;

		//create the vShape
		vShape clockShape =
			new vShape(
				shapeID,
				(double) xPoint,
				(double) yPoint,
				d_Width,
				d_Height);
		clockShape.setShapeID(shapeTypeID);
		//clockShape.removeTextField();
		clockShape.setLabel("0");
		clockShape.setObject(curTime);
		clockShape.setSelected(false);
		//register with the vShapeList
		vShapeList.put(
			"shapeKey" + shapeIndex,
			(Object) String.valueOf(shapeID));
		shape[shapeID] = clockShape;

		//add it to the screen
		add(clockShape);
		clockShape.setBounds(0, 0, getWidth(), getHeight());

		//mark the dirty flag
		isDirty = true; /* Weihong changed/added on 2/10/2001 */
		// Remember that this is the clock
		clock = clockShape;
	}

	/**
	 * getClockXCoordinate
	 * @return - the x cordinate of clock dead centre as a double
	 */
	public double getClockXCoordinate() {
		return clock.px + (clock.width / 2.0);
	}

	/**
	 * show graphics for the Action
	 * @param the action name
	 * @return the graphcial representation of this action
	 */
	public vShape createAction(String name) {
		int xPoint = new Double(getClockXCoordinate()).intValue();
		int yPoint = calcActionYCoord(xPoint);
		vShape tempShape = createVShape(xPoint, yPoint);
		tempShape.removeTextField();
		tempShape.setLabel(name);
		lstActions.add(tempShape);
		oclOperator op = new oclOperator();
		op.setName(new oclPredicate(name));
		tempShape.setObject(op);
		return tempShape;
	}

	/**
	 * show graphics for the process
	 * @param the process name
	 * @return the graphcial representation of this state
	 */
	public vShape createProcess(String name) {
		int xPoint = new Double(getClockXCoordinate()).intValue();
		int yPoint = calcProcessYCoord(xPoint);
		vShape tempShape = createVShape(xPoint, yPoint);
		tempShape.removeTextField();
		tempShape.setLabel(name);
		lstProcs.add(tempShape);
		oclOperator op = new oclOperator();
		op.setName(new oclPredicate(name));
		tempShape.setObject(op);
		return tempShape;
	}

	/**
	 * calcActionYCoord
	 * @param - the x coordinate of the action box
	 * @return - the y coordinate
	 */
	private int calcActionYCoord(int xCoord) {
		ListIterator li = null;
		int yCoord = (new Double(d_Height)).intValue() + 4 + GAP;
		// First possible position
		boolean foundFree = false;
		while (!foundFree) {
			li = lstActions.listIterator();
			boolean clash = false;
			while (!clash && li.hasNext()) {
				vShape v = (vShape) li.next();
				if (yCoord == v.py) {
					// On this row is it below the clock?
					if (xCoord >= v.px && xCoord <= v.px + v.width) {
						clash = true;
					}
				}
			}
			if (!clash) {
				foundFree = true;
			} else {
				// Try next row
				yCoord = yCoord + (new Double(d_Height)).intValue() + GAP;
			}
		}
		return yCoord; //Must be true after we have looked at all actions
	}

	/**
	 * calcProcessYCoord
	 * @param - the x coordinate of the action box
	 * @return - the y coordinate
	 */
	private int calcProcessYCoord(int xCoord) {
		ListIterator li = null;
		int yCoord = PROCSTART + GAP; // First possible position
		boolean foundFree = false;
		while (!foundFree) {
			li = lstProcs.listIterator();
			boolean clash = false;
			while (!clash && li.hasNext()) {
				vShape v = (vShape) li.next();
				if (yCoord == v.py) {
					// On this row is it below the clock?
					if (xCoord >= v.px && xCoord <= v.px + v.width) {
						clash = true;
					}
				}
			}
			if (!clash) {
				foundFree = true;
			} else {
				// Try next row
				yCoord = yCoord + (new Double(d_Height)).intValue() + GAP;
			}
		}
		return yCoord; //Must be true after we have looked at all processes
	}

	/**
	 * extendProcesses
	 * The time line has moved extend the process line for all active processes
	 */
	private void extendProcesses(double clockX) {
		double xPoint = clockX + clock.width / 2.0;
		ListIterator li = lstProcs.listIterator();
		while (li.hasNext()) {
			vShape curProc = (vShape) li.next();
			if (xPoint > curProc.px + d_Width) {
				// OK drag it out 
				curProc.width = xPoint - curProc.px;
			}
		}

	}

	/**
	  * dobule click to show the property.
	  * @param me MouseEvent
	  * @return void
	  */
	public void mouseClicked(MouseEvent me) {
		//double click left mouse
		if (me.getClickCount() == 2
			&& me.getModifiers() == MouseEvent.BUTTON1_MASK) {
			//check shapelist to see if there at least one shape was highlighted.
			Utility.debugPrintln(
				"No vShapes registered = " + vShapeList.size());
			for (int i = 1; i < vShapeList.size() + 1; i++) {
				vs = getShape(i);
				if (vs.contains((double) me.getX(), (double) me.getY())) {
					Object vsObj = vs.getObject();
					if (vsObj instanceof jplan.ocl.oclOperator) {
						ActionPropertyWindow propWnd =
							new ActionPropertyWindow(
								vs.getLabel().toString(),
								getTime(vs),
								workingDomain,
								top);
						propWnd.setCurState(calculateStateValue(getTime(vs)));
						top.desktop.add(propWnd);
						propWnd.toFront();
						try {
							propWnd.setSelected(true);
						} catch (Exception e){
							Utility.debugPrintln("Cannot select property window");
						}
						propWnd.show();
					} else {
						showRenameWindow(vs);
					}
					return;
				}
			}
		}
	}

	/**
	 * When mouse drags, actions could be one of the followings: 
	 * vShape reshape, move vShapes/vLinks, drag seletion.
	 * @param me MouseEvent
	 * @return void
	 */
	public void mouseDragged(MouseEvent me) {

		if (mouseAction == SELECT) {
			int i, k;
			//check all the shapes
			for (i = 1; i < vShapeList.size() + 1; i++) {
				vs = getShape(i);
				if (vs.getSelected()) {
					//k = vs.getShapeHighlighter().contains((double)me.getX(),(double)me.getY());
					k = 0; // Do not allow resizing for the moment
					switch (k) {
						case 0 : //dragging the shape
							if (!resize) { /* WZ 14/5/02 */
								dragged = true;
								setCursor(
									Cursor.getPredefinedCursor(
										Cursor.MOVE_CURSOR));
								if (vs
									.getObject()
									.getClass()
									.getName()
									.equals("jplan.tools.sketch.Clock")) {
									double dragPoint = me.getX() - vs.offsetX;
									if (dragPoint >= 0.0 + 2.0) {
										vs.setPosition(dragPoint, vs.py);
										int time =
											(new Double(dragPoint)).intValue()
												- 2;
										vs.setLabel(Integer.toString(time));
										extendProcesses(dragPoint);
									} // only drag the time within the positive scale
								} else {
									// Only allow horizontal dragging
									vs.setPosition(
										(double) (me.getX() - vs.offsetX),
										vs.py);
								}
								//vs.setLocation(me.getX()-4 - (int)vs.offsetX, me.getY()-4 - (int)vs.offsetY);
								repaint();
								break;
							}

							/*resize the shape*/
						case 2 : //west
							if (!dragged) { /* WZ 15/5/02 */
								resize = true; /* WZ 14/5/02 */
								setCursor(
									Cursor.getPredefinedCursor(
										Cursor.W_RESIZE_CURSOR));
								vs.setDoubleBounds(
									Math.min(
										vs.px + vs.width - 5,
										(double) me.getX()),
									vs.py,
									Math.max(
										5,
										vs.px - (double) me.getX() + vs.width),
									vs.height);
								repaint();
								break;
							}
						case 1 : //north
							if (!dragged) { /* WZ 15/5/02 */
								resize = true; /* WZ 14/5/02 */
								setCursor(
									Cursor.getPredefinedCursor(
										Cursor.N_RESIZE_CURSOR));
								vs.setDoubleBounds(
									vs.px,
									(double) me.getY(),
									vs.width,
									Math.max(
										5,
										vs.py
											- (double) me.getY()
											+ vs.height));
								repaint();
								break;
							}
						case 4 : //east
							if (!dragged) { /* WZ 15/5/02 */
								resize = true; /* WZ 14/5/02 */
								setCursor(
									Cursor.getPredefinedCursor(
										Cursor.E_RESIZE_CURSOR));
								vs.setDoubleBounds(
									vs.px,
									vs.py,
									Math.max((double) me.getX() - vs.px, 5),
									vs.height);
								repaint();
								break;
							}
						case 3 : //south
							if (!dragged) { /* WZ 15/5/02 */
								resize = true; /* WZ 14/5/02 */
								setCursor(
									Cursor.getPredefinedCursor(
										Cursor.S_RESIZE_CURSOR));
								vs.setDoubleBounds(
									vs.px,
									vs.py,
									vs.width,
									Math.max(5, (double) me.getY() - vs.py));
								repaint();
								break;
							}
					}

					/* WZ 14/5/02 */
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
				}
			}

			//check all the links
			for (i = 1; i < linkList.size() + 1; i++) { //for every vlink ...
				vl = (vLink) linkList.get("linkKey" + i);
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

			if (!dragged) {
				dragSelect = true;
				mouseCurrentPoint = new Point(me.getPoint());
				repaint();
			}
		}
	}
	
	/***********************************************************/
	/* The following methods are used to calculate and maintain
	 * the state of the plan.
	 */
	 
	/**
	 * calculateStateValue
	 * @param - the time for the state calculatio
	 * @return the state list
	 */
	
	public List calculateStateValue(int time) {
		if (initState == null) {
			initState = ((SketchWindow)parent).curTask.getInits();
		}
		ArrayList state = new ArrayList(initState.size());
		ListIterator li = initState.listIterator();
		while (li.hasNext()) {
			try {
				oclSS objState = (oclSS)((oclSS)li.next()).clone();
				state.add(objState);
			} catch (CloneNotSupportedException e){}
		}
		if (time > 0) {
			advanceState(state,time);
		} 
		return state;
	}

	/**
	 * advanceState 
	 * apply all actions and processes up to given time
	 * @param state
	 * @param time
	 */
	private void advanceState(List state, int time) {
		Utility.debugPrintln("advanceState NOT YET IMPLEMENTED");
		Utility.debugPrintln("advanceState TIME = " + time);
	}
	
	/**
	 * getTime 
	 * calculate the start time for an action vShape
	 * @param vs - the action vShape
	 * @return - time as an int
	 */
	private int getTime(vShape vs) {
		int time = (new Double(vs.px - (clock.width / 2.0)).intValue()) - 2 ;
		return time;
	}
}
