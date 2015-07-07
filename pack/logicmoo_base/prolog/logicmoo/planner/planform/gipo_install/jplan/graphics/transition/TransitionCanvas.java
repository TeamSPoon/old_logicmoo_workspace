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

package jplan.graphics.transition;

/* TransitionCanvas.java
 * 21/6/01
 * Weihong Zhao
*/

import javax.swing.JComponent;
import java.awt.event.MouseEvent;
import java.awt.Dimension;
import java.awt.Point;
import java.util.List;
import java.util.Vector;
import java.util.Hashtable;
import java.util.ListIterator;
import javax.swing.JComboBox;
import javax.swing.JOptionPane;
import javax.swing.plaf.basic.BasicComboPopup;
import java.util.ArrayList;

import jplan.graphics.JGraphCanvas;
import jplan.graphics.gTool.Graphics.vShape;
import jplan.graphics.gTool.Graphics.vLink;
import jplan.graphics.gTool.Windows.vShapePropertyWindow;
import jplan.ocl.*;
import jplan.general.*; /* Weihong changed on 30/08/01 */

/**
 * Extended from JGraphCanvas with all the drawing/editing functions inherited,
 * this TransitionCanvas has added a few more functions specifically for dealing with transitions.
 * Different part of the transitions is attached to different shapes shown on the canvas.
 * When operator's head graphics is created, it is allowed to edit the name/label by mouse
 * double clicking to bring up a property window; for other graphics like prevail condition,
 * mouse double clicking is disabled as they are not allowed to edit freely, instead, a right
 * click will allow the static precidates to be added from the popup list.<br>
 * When deleting states in both sides of the transition will be deleted in pairs.
 */
public class TransitionCanvas extends JGraphCanvas {

	//for dealing with vShapes
	/**
	 * Temporary sort name.
	 */
	public String tmpSort = null; //reprents oclSort
	/**
	 * Temporary sort ID.
	 */
	public String tmpSortID = null;
	/**
	 * The string value of the selected state. If no state is selected currently, buffLabel is null.
	 */
	public String buffLabel = null;
	/**
	 *  The current selected substate lists.
	 */
	public List predicateList = new ArrayList();
	/**
	 * The previous selected object ID.
	 */
	public String curObjectID = null;
	private String objectID = null; //to represent the newly selected object ID

	private oclDomain workingDomain;
	private BasicComboPopup popupMenu;
	private vShape popupShape;
	private ListIterator liPreds;
	private String str;
	private JComboBox combo;

	private vShape OPName = null;
	private vShape tempShape = null;

	private String checkLabel = null;
	private oclSC sc = null; //temp oclSC
	private oclSE se = null; //temp oclSE
	private ActionWindow parent = null;

	/**
	 * Create a canvas allowing drawing and editing different shapes
	 * to represent operator head/id, prevail condition, necessary condition, conditional change
	 * @param parent the parent ActionWindow
	 */
	public TransitionCanvas(ActionWindow parent) {
		super();
		this.parent = parent;
		d_Width = 140; /* WZ 14/6/02 */
		d_Height = 80; /* WZ 14/6/02 */
		removeMListener(); /* Weihong added/changed on 2/10/01 */
	}

	/**
	 * to pass the domain
	 * @param cur domain
	 * 
	 */
	public void setWorkingDomain(oclDomain cur) {
		workingDomain = cur;
	}

	/* Weihong added on 27/11/2001 */
	/**
	 * return the domain
	 * @return the ocl domain
	 */
	public oclDomain getWorkingDomain() {
		return workingDomain;
	}

	/**
	 * when mouse clicked; override its super
	 * @param me MouseEvent
	 * 
	 */
	public void mouseClicked(MouseEvent me) {
		if (me.getClickCount() == 2
			&& me.getModifiers() == MouseEvent.BUTTON1_MASK) {
			//check shapelist to see if there at least one shape was highlighted.
			for (int i = 1; i < vShapeList.size() + 1; i++) {
				tempShape = (vShape) getShape(i);
				if (tempShape.getShapeID() == 0
					&& tempShape.contains(
						(double) me.getX(),
						(double) me.getY())) {
					/* Weihong changed/added on 3/9/2001 */
					String name =
						GipoInputBox.showIdentifierInputBox(
							parent.top,
							"Operator Name",
							"Please input/rename the operator's name.",
							tempShape.getLabel());
					if (name != null) {
						tempShape.setLabel(name);
						tempShape.repaint();
					}
					/* Weihong changed/added end */
					return;
				}
			}
		}

		if (me.getModifiers() == MouseEvent.BUTTON3_MASK) {
			for (int i = 1; i < vShapeList.size() + 1; i++) {
				tempShape = (vShape) getShape(i);
				if (tempShape.getShapeID() != 0
					&& tempShape.contains(
						(double) me.getX(),
						(double) me.getY())) {
					initPopupMenu();
					popupMenu.show(this, me.getX(), me.getY());
				}
			}
			return;
		}
	}

	/* Weihong added on 16/08/2001 */
	/**
	 * deleteShape in pair
	 * 
	 */
	public void deleteShape() {
		//get all highlighted shapes
		vShape tempShape = null;
		List list = new ArrayList();
		for (int i = 1; i < vShapeList.size() + 1; i++) {
			tempShape = (vShape) getShape(i);
			if (tempShape.getSelected()) {
				/* Weihong added on 16/08/2001 */
				//check if this required deleting shape is the ActionID (head of the operator)
				if (tempShape.getShapeID() == 0)
					tempShape.setSelected(false);
				else
					/* end Weihong added on 16/08/2001 */
					list.add(tempShape);
			}
		}
		//find another pair
		ListIterator li = list.listIterator();
		while (li.hasNext()) {
			vShape vsp = findPairShape((vShape) li.next());
			if (vsp != null)
				vsp.setSelected(true);
		}

		//delete all shapes
		super.deleteShape();
	}

	/* Weihong added on 16/08/2001 */
	/**
	 * find another shape in pair
	 * @param vs the first vShape
	 */
	private vShape findPairShape(vShape vs) {
		//left hand side shapes were clicked
		if (vs.getInLinks().size() == 0) {
			vLink vpLink = (vLink) vs.getOutLinks().get("outlinks" + 1);
			//it has only 1 outlink
			vLink tmpLink = null;
			for (int i = 1; i < OPName.getOutLinks().size() + 1; i++) {
				tmpLink = (vLink) OPName.getOutLinks().get("outlinks" + i);
				if (vpLink.getType() == tmpLink.getType()) {
					return (vShape) tmpLink.getStopShape();
				}
			}
		}
		//right hand side shapes were clicked
		if (vs.getOutLinks().size() == 0) {
			vLink vpLink = (vLink) vs.getInLinks().get("inlinks" + 1);
			//it has only 1 outlink
			vLink tmpLink = null;
			for (int i = 1; i < OPName.getInLinks().size() + 1; i++) {
				tmpLink = (vLink) OPName.getInLinks().get("inlinks" + i);
				if (vpLink.getType() == tmpLink.getType()) {
					return (vShape) tmpLink.getStartShape();
				}
			}
		}

		return null;
	}

	/**
	 * when mouse prssed
	 * @param me MouseEvent
	 * 
	 */
	public void mousePressed(MouseEvent me) {
		mouseDownPoint = new Point(me.getPoint());
		for (int i = 1; i < vShapeList.size() + 1; i++) {
			tempShape = (vShape) getShape(i);
			tempShape.getOffset(mouseDownPoint); //calculate the offset value
		}

		dragSelect = false;

		//to create and draw a new vShape
		if (mouseAction == CREATE_SHAPE) {
			if (buffLabel != null) { /* WZ 2/5/02 */
				/* Weihong changed/added on 3/9/2001 */
				// Ron 11/10/02 added new verify loop
				String objID = null;
				boolean newID = false;
				while (!newID) {
					objID =
						GipoInputBox.showVarableInputBox(
							parent.top,
							"Object ID",
							"Please Input a new object ID.");
					if (objID != null) {
						// Ron 11/01/02 Need to check that this is a unique ID
						if (!checkIDUnique(objID)) {
							JOptionPane.showMessageDialog(
								this,
								"Object ID already used select another.",
								"GIPO Error",
								JOptionPane.ERROR_MESSAGE,
								null);
						} else {
							newID = true;
						}
					}
				}
				objectID = objID;
				createOPshapes(me);
			} else
			JOptionPane.showMessageDialog(
				this,
				"No value for the state.",
				"GIPO Warning",
				JOptionPane.WARNING_MESSAGE,
				null);
		}

		if (mouseAction == CREATE_LINK) {
			createVLink(me);
		}

		//to select a (or more) shape(s)
		if (mouseAction == SELECT) {
			//check shapelist to see if there at least one shape was highlighted.
			for (int i = 1; i < vShapeList.size() + 1; i++) {
				tempShape = (vShape) getShape(i);
				/* Weihong added on 18/11/2001 */
				if (tempShape.getSelected()
					&& me.getModifiers() == MouseEvent.BUTTON1_MASK) {
					//if there is at least one shape was preselected...
					if (tempShape
						.contains((double) me.getX(), (double) me.getY())) {
						//do not de-select the shapes since this is a temp to drag Shapes;
						return;
					}
				}
			}

			selectSingleShape((double) me.getX(), (double) me.getY());
			selectSingleLink((double) me.getX(), (double) me.getY());
		}
		repaint();
	}
	
	/**
	 * checkIDUnique
	 * check that the supplied transition ID is quique
	 * @param id - the suggested ID
	 * @return - true if unique
	 */
	// Ron 11/10/02
	private boolean checkIDUnique(String id) {
		boolean res = true;
		vShape vs = null;
		oclOperator op = null;
		vs = getVShape(0)[0];
		if (vs != null) {
			op = parent.getOpertor(vs);
			ListIterator li = op.getPrevail().listIterator();
			while (li.hasNext()) {
				oclSE se = (oclSE)li.next();
				if (id.equals(se.getName())) {
					return false;
				}
			}
			li = op.getNecessary().listIterator();
			while (li.hasNext()) {
				oclSC sc = (oclSC)li.next();
				if (id.equals(sc.getName())) {
					return false;
				}
			}
			li = op.getConditional().listIterator();
			while (li.hasNext()) {
				oclSC sc = (oclSC)li.next();
				if (id.equals(sc.getName())) {
					return false;
				}
			}
		}
		return res;
	}

	/**
	 * select a single vshape which contains the current point
	 * @param x double value at X axis - the position of mouse
	 * @param y double value at Y axis - the position of mouse
	 * @return vShape
	 */
	protected vShape selectSingleShape(double x, double y) {
		vShape vsp = null;
		for (int i = 1; i < vShapeList.size() + 1; i++) {
			tempShape = (vShape) getShape(i);
			if (tempShape.contains(x, y)) {
				tempShape.setSelected(true);

				if (tempShape.getShapeID() != 0)
					/* for the popup menu to perform to assign a predicate 
					   to the name shape of an operator */
					popupShape = tempShape;
				Utility.debugPrintln(
					"The popupShape has id " + popupShape.getShapeID());
				vsp = tempShape;
			} else
				tempShape.setSelected(false);
		}
		return vsp;
	}

	/***********************************
	  Basic shapes creation section
	************************************/
	/**
	 * create shapes to represent an operator
	 * @param me MouseEvent
	 * 
	 */
	public void createOPshapes(MouseEvent me) {
		oclPredicate opd;
		tempShape = createVShape(me.getX(), me.getY());
		//tempShape.textField.setHierFlag(parent.top.hierarchicalSwitch);
		tempShape.textField.setHierFlag(workingDomain.isHierarchical());
		/* WZ 27/8/02 */
		tempShape.textField.setObjectID(objectID);
		tempShape.textField.setSort(tmpSort); /* WZ 27/8/02 */
		tempShape.textField.setCurDomain(workingDomain);
		tempShape.setDrawLabel(false);
		// Ron 10/10/02 added clone of predicateList
		// All later references to predicateList changed to copyPredicateList
		ArrayList copyPredicateList = new ArrayList();
		ListIterator li = predicateList.listIterator();
		while (li.hasNext()) {
			oclPredicate copy = null;
			try {
				copy = (oclPredicate) ((oclPredicate) li.next()).clone();
			} catch (Exception e) {
				System.err.println(
					"Unexpected failure to clone predicate : TransitionCanvas");
			}
			copyPredicateList.add(copy);
		}
		// End ron add 10/10/02
		if (buffLabel != null) {
			tempShape.setLabel(buffLabel);
			Thread runner = new Thread() {
				public void run() {
					setMouseAction(0); //disable the click on the canvas
					while (buffLabel.equals(checkLabel)) {
						// 			if (tempShape == null) {//if it's inlink no longer exist
						if (vShapeList
							.contains("shapeKey" + tempShape.getID())) {
							parent.resetStateList();
							return;
						}
						tempShape.setSelected(!tempShape.getSelected());
						//flashing to get attention
						repaint();
						try {
							sleep(100);
						} catch (Exception e) {
						}
						//wait until the mouse clicked the states list
					}
					// Ron 10/10/02 added clone of predicateList
					ArrayList copyPredicateList = new ArrayList();
					ListIterator li = predicateList.listIterator();
					while (li.hasNext()) {
						oclPredicate copy = null;
						try {
							copy =
								(oclPredicate) ((oclPredicate) li.next())
									.clone();
						} catch (Exception e) {
							System.err.println(
								"Unexpected failure to clone predicate : TransitionCanvas");
						}
						copyPredicateList.add(copy);
					}
					// End ron add 10/10/02
					tempShape.setLabel(buffLabel);
					replaceVariableNameFromAList(
						copyPredicateList,
						curObjectID,
						objectID);
					tempShape.textField.setObjectID(objectID);
					/* WZ 27/8/02 */
					tempShape.textField.setHierFlag(
						workingDomain.isHierarchical());
					//	parent.top.hierarchicalSwitch);
					tempShape.textField.setSort(tmpSort);
					tempShape.textField.setCurDomain(workingDomain);
					/* WZ 27/8/02 */
					tempShape.textField.setTransElement(
						TransExpressionDocPane.RHS);
					/* Weihong changed on 30/08/01 */
					sc.setPost(copyPredicateList);
					tempShape.setObject(sc);
					assignText(tempShape, copyPredicateList);
					tempShape.setSelected(false);
					repaint();
					tempShape = null;
					resetMouseAction();
					//for undo
					recordState();
					parent.resetStateList();
					parent.enableSortTree(true); /* WZ 2/5/02 */

				}
			};

			switch (tempShape.getShapeID()) {
				case 0 : //operator's name - predicate
					createActionID();
					break;
				case 1 : //oclSE
					se = new jplan.ocl.oclSE(tmpSort, objectID);
					replaceVariableNameFromAList(
						copyPredicateList,
						curObjectID,
						objectID);
					tempShape.textField.setTransElement(
						TransExpressionDocPane.PREV);
					/* Weihong changed on 30/08/01 */
					se.setPredicateList(copyPredicateList);
					tempShape.setObject(se);
					assignText(tempShape, copyPredicateList);

					//set the relationship
					setMouseAction(CREATE_LINK);
					setDrawingLinkID(calculateLinkTypeID());
					createVLink(tempShape, OPName);
					resetMouseAction();
					//for undo
					recordState();
					tempShape = null;
					break;
				case 2 : //oclSC
					sc = new jplan.ocl.oclSC(tmpSort, objectID);
					replaceVariableNameFromAList(
						copyPredicateList,
						curObjectID,
						objectID);
					tempShape.textField.setTransElement(
						TransExpressionDocPane.LHS);
					/* Weihong changed on 30/08/01 */
					sc.setPre(copyPredicateList);
					tempShape.setObject(sc);
					assignText(tempShape, copyPredicateList);

					setMouseAction(CREATE_LINK);
					setDrawingLinkID(calculateLinkTypeID());
					createVLink(tempShape, OPName);
					setMouseAction(CREATE_SHAPE);
					tempShape = createVShape(me.getX() + 300, me.getY());
					tempShape.setDrawLabel(false);
					setMouseAction(CREATE_LINK);
					createVLink(OPName, tempShape);
					setMouseAction(CREATE_SHAPE);
					repaint();
					buffLabel = "";
					checkLabel = buffLabel;
					parent.resetStateList(); /* WZ 1/5/02 */
					parent.enableSortTree(false); /* WZ 2/5/02 */
					runner.start();
					break;
				case 3 : //oclSC
					sc = new jplan.ocl.oclSC(tmpSort, objectID);
					replaceVariableNameFromAList(
						copyPredicateList,
						curObjectID,
						objectID);
					tempShape.textField.setTransElement(
						TransExpressionDocPane.LHS);
					/* Weihong changed on 30/08/01 */
					sc.setPre(copyPredicateList);
					tempShape.setObject(sc);
					assignText(tempShape, copyPredicateList);

					setMouseAction(CREATE_LINK);
					setDrawingLinkID(calculateLinkTypeID());
					createVLink(tempShape, OPName);
					setMouseAction(CREATE_SHAPE);
					tempShape = createVShape(me.getX() + 300, me.getY());
					tempShape.setDrawLabel(false);
					setMouseAction(CREATE_LINK);
					createVLink(OPName, tempShape);
					setMouseAction(CREATE_SHAPE);
					repaint();
					buffLabel = "";
					checkLabel = buffLabel;
					parent.resetStateList(); /* WZ 1/5/02 */
					parent.enableSortTree(false); /* WZ 2/5/02 */
					runner.start();
					break;
			}
		} else
			JOptionPane.showMessageDialog(
				this,
				"No value for the state.",
				"GIPO Warning",
				JOptionPane.WARNING_MESSAGE,
				null);

	}

	//     /**
	//      * remove highlights inside the textRield - the transitionExpressionPane
	//      * 
	//      */
	//     public void removeHighlighter(){
	// 	vShape tempShape;
	// 	for (int i = 1; i < vShapeList.size()+1; i++) {
	// 	    tempShape = (vShape)getShape(i);
	// 	    if (tempShape.getShapeID() != 0) {
	// 		tempShape.textField.removeHighlights();
	// 	    }
	// 	}
	//     }

	/* Weihong added on 15/08/2001 */
	/**
	 * replaceVariableNameFromAList for all the predicate
	 * replace with a new variable value
	 * not in an element throw exception
	 * @param listPred the list of predicates to be operated
	 * @param before the varible
	 * @param after new element name
	 */
	private void replaceVariableNameFromAList(
		List listPred,
		String before,
		String after) {
		ListIterator li = listPred.listIterator();
		while (li.hasNext()) {
			oclPredicate oprd = (oclPredicate) li.next();
			try {
				oprd.replaceVariableNameByName(before, after);
			} catch (Exception e) {
				Utility.debugPrintln(e);
			}
		}
	}

	/**
	 * reset mouse mode to default: "selection"
	 * 
	 */
	private void resetMouseAction() {
		mouseAction = SELECT;
		parent.rbt_select.setSelected(true);
		parent.jMI_Select.setSelected(true);
	}

	/**
	 * put the current edited predicates to the vshape
	 * @param theShape the vShape which textField needs to be updated
	 * @param predList  the predicate list to assign // Ron 10/10/02 make sure that we have the edited copy
	 * 
	 */
	private void assignText(vShape theShape, List predList) {
		// used to be the global predicateList
		ListIterator li = predList.listIterator();
		while (li.hasNext()) {
			try {
				theShape.textField.addPredicate((oclPredicate) li.next());
			} catch (Exception e) {
				Utility.debugPrintln(
					"Failed to insert predicate " + e.toString());
			}
		}
	}

	/**
	 * loop within 6 different link types to pick a different link type
	 * @return int linktype
	 */
	public int calculateLinkTypeID() {
		if (linkTypeID == 5)
			linkTypeID = 0;
		else
			linkTypeID++;

		return linkTypeID;
	}

	/**
	 * set graphical representation of the operator's name/head
	 * @param theShape vShape which is set to the operator's head
	 * 
	 */
	public void setOPName(vShape theShape) {
		OPName = theShape; //set center shape
	}

	/**
	 * create graphical representation of the operator's name/head
	 * 
	 */
	public void createActionID() {
		vShape tempShape = createVShape(200, 200);
		tempShape.setObject(new oclPredicate(tempShape.getLabel()));
		OPName = tempShape; //set center shape
		tempShape.removeTextField();
		tempShape = null;
	}

	/**
	 * initialise the popup menu for appending the static predicates
	 * when right click on the vshapes other than the operators head (circle)
	 * 
	 */
	public void initPopupMenu() {
		Vector items = new Vector();
		liPreds = (workingDomain.predicates).listIterator();
		oclPredicate opde;
		while (liPreds.hasNext()) {
			opde = (oclPredicate) liPreds.next();
			if (opde.isStatic())
				items.addElement(opde);
		}
		combo = new JComboBox(items);
		combo.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				try {
					// Ron 29/8/01 added clone otherwise this is the 
					// predicate in the predicate list
					oclPredicate org =
						(oclPredicate) (popupMenu.getList().getSelectedValue());
					oclPredicate opt = (oclPredicate) org.clone();
					opt.toVar();
					if (popupShape == null)
						Utility.debugPrintln("PopupShape is null");
					popupShape.textField.addPredicate(opt);
					if (popupShape.getShapeID() == 1) //oclSE
						 ((oclSE) popupShape.getObject()).addPredicate(opt);
					else if (
						popupShape.getShapeID() == 2
							|| popupShape.getShapeID() == 3) { //oclSC
						if (popupShape.getInLinks().size() == 0)
							//left hand side
							 ((oclSC) popupShape.getObject()).addPre(opt);
						else if (popupShape.getOutLinks().size() == 0)
							//right hand side
							 ((oclSC) popupShape.getObject()).addPost(opt);
					}
				} catch (Exception e) {
					Utility.debugPrintln(
						"Failed to insert predicate " + e.toString());
				}
				repaint();
				popupShape = null;
				popupMenu.hide(); // Ron 9/10/02 Mmmmm
			}
		});
		popupMenu = new BasicComboPopup(combo);
	}

	/* WZ 27/8/02 */
	/**
	 * Get the value of parent.
	 * @return Value of parent.
	 */
	public ActionWindow getParentFrame() {
		return parent;
	}

}
