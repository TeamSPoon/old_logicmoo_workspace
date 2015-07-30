package jplan.graphics.transition;
/**
 * MethodHeadCanvas.java
 *
 *
 * Created: Tue Nov 13 10:51:11 2001
 *
 * @author W Zhao
 * @version
 */
/*
 * History
 * Ron 10/11/02 when ss dropped onto canvas ask for an ID
 */

import javax.swing.*;
import java.awt.event.*;
import java.awt.Graphics;
import java.util.ListIterator;
import java.util.List;
import java.util.Hashtable;

import jplan.ocl.*;
import jplan.graphics.gTool.Graphics.*;
import jplan.general.GipoTab; /* Weihong  26/11/2001 */
import java.awt.dnd.*; /* Weihong  29/11/2001 */
import java.awt.datatransfer.*; /* Weihong  30/11/2001 */
import jplan.general.VarEditWindow; /* WZ 28/3/02 */
import jplan.graphics.JGraphCanvas; /* WZ 4/4/02 */
import jplan.general.GipoInputBox; // Ron 10/11/02

/**
 * This is the graphics canvas to put the oclMethod Head vShape
 * in order to view/edit the decomposition of the oclMethod.
 */
public class MethodHeadCanvas
	extends jplan.graphics.JGraphCanvas
	implements DropTargetListener {
	private ButtonGroup bgroup = new ButtonGroup();
	private JCheckBoxMenuItem CMI;
	private JMenuItem MI;
	private JPopupMenu popupMenu; //for displaying the general functions
	private JPopupMenu shapeMenu; //to attach to vShapes
	private oclDomain curDomain;
	private GipoTab tabParent; /* Weihong  26/11/2001 */
	private vShape popupShape = null; /* Weihong  26/11/2001 */
	private vShape shapeParent = null; /* Weihong  26/11/2001 */
	private HighLevelTransitionWindow theFrame; /* Weihong  26/11/2001 */
	private DropTarget dropTarget; /* Weihong  30/11/2001 */
	private boolean secondLevel; /* Weihong  3/12/2001 */
	private VarEditWindow varEditWin; /* WZ 28/3/02 */

	/**
	 * Creates an dummy instance of MethodHeadCanvas
	 */
	public MethodHeadCanvas() {
		super();
	}

	/**
	 * Creates an dummy instance of MethodHeadCanvas
	 */
	public MethodHeadCanvas(oclDomain curDomain) {
		super();
		this.curDomain = curDomain;
	}

	/**
	 * Creates an instance of MethodHeadCanvas
	 */
	public MethodHeadCanvas(
		boolean secondLevel,
		vShape shapeParent,
		HighLevelTransitionWindow theFrame) {
		super();
		this.theFrame = theFrame;
		this.curDomain = theFrame.getWorkingDomain();
		this.tabParent = theFrame.getTabParent(); /* Weihong  26/11/2001 */
		this.shapeParent = shapeParent; /* Weihong  26/11/2001 */
		this.secondLevel = secondLevel; /* Weihong  3/12/2001 */
		d_Width = 70;
		d_Height = 40;
		setToolTipText("Right click mouse to see the functions for editing.");
		if (shapeParent != null) /* WZ 3/4/02 */
			setDefaultColor(java.awt.Color.lightGray); //set default color
		else
			setDefaultColor(java.awt.Color.white); //set default color to white

		setVisible(false); //set to transparent
		initPopupMenu(); //initiate the popup menu
		//if this is not the first level decomposition then disable drag and drop
		if (!secondLevel) {
			/* Weihong  29/11/2001 */
			dropTarget = new DropTarget(this, this);
			/* WZ 28/3/02 */
			if (shapeParent != null) { /* WZ 3/4/02 */
				varEditWin =
					new VarEditWindow(
						theFrame.top,
						vShapeList,
						shape,
						theFrame,
						true);
				jplan.general.Utility.debugPrintln("Embedded!!! ");
			} else
				varEditWin =
					new VarEditWindow(
						theFrame.top,
						vShapeList,
						shape,
						theFrame,
						false);
		}
	}

	/* WZ 16/4/02 */
	/**
	 * return shapeParent
	 */
	public vShape getParentShape() {
		return shapeParent;
	}

	/* WZ 16/4/02 */
	/**
	 * return HighLevelTransitionWindow
	 */
	public HighLevelTransitionWindow getParentFrame() {
		return theFrame;
	}

	/* WZ 28/3/02 */
	/**
	 * return VarEditWindow
	 */
	public VarEditWindow getVarEditWindow() {
		return varEditWin;
	}

	/* Weihong  on 3/12/2001 */
	/**
	 * check if this canvas/shape is the first level of the decomposition
	 * @return ture if this is the first level of the decomposition of an oclMethod
	 */
	public boolean isFirstLevel() {
		return !secondLevel;
	}

	/* Weihong  30/11/2001 */
	public void removeDropTarget() {
		dropTarget = null;
	}

	/* Weihong  29/11/2001 */
	public void dragEnter(DropTargetDragEvent event) {
		event.acceptDrag(DnDConstants.ACTION_COPY);
	}

	/* Weihong  29/11/2001 */
	/**
	 * is invoked when you are exit the DropSite without dropping
	 */
	public void dragExit(DropTargetEvent event) {
	}

	/* Weihong  29/11/2001 */
	/**
	 * is invoked when a drag operatiis going on
	 */
	public void dragOver(DropTargetDragEvent event) {
	}

	/* Weihong  29/11/2001 */
	/**
	 * a drop has occurred
	 */
	public void drop(DropTargetDropEvent event) {
		try {
			Transferable transferable = event.getTransferable();

			DataFlavor df =
				new DataFlavor(
					(new oclMethod()).getClass(),
					"TransferableObject");
			if (transferable.isDataFlavorSupported(df)) {
				event.acceptDrop(DnDConstants.ACTION_COPY);
				oclMethod om = (oclMethod) transferable.getTransferData(df);
				drawOclMethod(
					this,
					om.getName(),
					event.getLocation().x,
					event.getLocation().y);
				/* WZ 4/4/02 */
				repaint();
				event.getDropTargetContext().dropComplete(true);
			} else if (
				transferable.isDataFlavorSupported(
					new DataFlavor(
						(new oclStateList()).getClass(),
						"TransferableObject"))) {
				event.acceptDrop(DnDConstants.ACTION_COPY);
				oclStateList slist =
					(oclStateList) transferable.getTransferData(
						new DataFlavor(
							(new oclStateList()).getClass(),
							"TransferableObject"));
				// Ron 10/11/02 get object id from user and replace in state list XXX
				oclSS ss = null;
				String objID =
					GipoInputBox.showVarableInputBox(
						theFrame.top,
						"Object ID",
						"Please Input a new object ID.");
				if (objID != null) {
					ss = new oclSS(theFrame.getSort(), objID);
					ss.setState(slist.getPredicateList());
					ss.replaceVariableName(theFrame.getObject(), objID);
				} else {
					// else contains origonal text
					ss = new oclSS(theFrame.getSort(), theFrame.getObject());
					ss.setState(slist.getPredicateList());
				}
				// Ron End 10/11/02
				/* WZ 21/8/02 */
				jplan.general.Utility.debugPrintln("debugging...");
				ListIterator listate = slist.getPredicateList().listIterator();
				while (listate.hasNext()) {
					oclPredicate oprd = (oclPredicate) listate.next();
					jplan.general.Utility.debugPrintln(
						(oprd.getSort()).toString());
				}

				drawOclSS(
					this,
					curDomain,
					ss,
					event.getLocation().x,
					event.getLocation().y);
				/* WZ 4/4/02 */
				repaint();
				event.getDropTargetContext().dropComplete(true);
			} else if (
				transferable.isDataFlavorSupported(
					new DataFlavor(
						(new oclOperator()).getClass(),
						"TransferableObject"))) {
				event.acceptDrop(DnDConstants.ACTION_COPY);
				oclOperator op =
					(oclOperator) transferable.getTransferData(
						new DataFlavor(
							(new oclOperator()).getClass(),
							"TransferableObject"));
				drawOclOperator(
					this,
					op,
					event.getLocation().x,
					event.getLocation().y);
				/* WZ 4/4/02 */
				repaint();
				event.getDropTargetContext().dropComplete(true);
			} else {
				event.rejectDrop();
			}
		} catch (Exception exception) {
			exception.printStackTrace();
			System.err.println("Exception" + exception.getMessage());
			event.rejectDrop();
		}
	}

	public void dropActionChanged(DropTargetDragEvent event) {
	}

	/* WZ 4/4/02 */
	/**
	 * return the domain
	 * @return the ocl domain
	 */
	public oclDomain getWorkingDomain() {
		return curDomain;
	}

	/**
	 * initialise the popup menu to show all functions
	 * including create, select, delete, etc.
	 * 
	 */
	public void initPopupMenu() {
		popupMenu = new JPopupMenu("Functions ...");

		MI = new JMenuItem("Zoom in");
		MI.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				setScale(1.5);
			}
		});
		popupMenu.add(MI);

		MI = new JMenuItem("Zoom out");
		MI.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				setScale(0.67);
			}
		});
		popupMenu.add(MI);

		popupMenu.addSeparator();

		CMI = new JCheckBoxMenuItem("Temporal constraints");
		CMI.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				setMouseAction(CREATE_LINK);
				setDrawingLinkID(3); //
			}
		});
		popupMenu.add(CMI);
		bgroup.add(CMI);

		CMI = new JCheckBoxMenuItem("Select");
		CMI.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				setMouseAction(SELECT);
			}
		});
		mouseAction = SELECT;
		CMI.setState(true);
		popupMenu.add(CMI);
		bgroup.add(CMI);

		MI = new JMenuItem("Delete");
		MI.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				setMouseAction(DELETE);
				deleteShape();
				deleteLink();
				setMouseAction(SELECT);
				((JCheckBoxMenuItem) popupMenu.getComponent(4)).setState(true);
			}
		});
		popupMenu.add(MI);

		/* WZ 3/5/02 */
		if (shapeParent == null) { /* WZ 3/5/02 */
			popupMenu.addSeparator();

			MI = new JMenuItem("Edit variables ...");
			MI.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(ActionEvent e) {
					jplan.general.MethodVarEdit mvd =
						new jplan.general.MethodVarEdit(
							theFrame.top,
							MethodHeadCanvas.this,
							true);
				}
			});
			popupMenu.add(MI);
		}
		/* end 3/5/02 */
	}

	/**
	 * initialise the popup menu to show functions like 'rename', 'property'
	 * @param vs the vShape
	 * 
	 */
	public void initShapeMenu(vShape vs) {
		popupShape = vs;
		shapeMenu = new JPopupMenu();
		//if the shape stands for oclMethod/oclOperator
		switch (vs.getShapeID()) {
			case 5 :
				MI = new JMenuItem("Compound Operator Property");
				MI.addActionListener(new java.awt.event.ActionListener() {
					public void actionPerformed(ActionEvent e) {
						if (shapeParent != null) /* Weihong 12/3/02 */
							tabParent.addMethodTab(
								shapeParent.getLabel()
									+ "["
									+ popupShape.getLabel()
									+ "]",
								theFrame);
						HighLevelTransitionCanvas tmpCanvas =
							(HighLevelTransitionCanvas) tabParent
								.getSelectedCanvas();
						HighLevelTransitionWindow.showOclMethod(
							true,
							(oclMethod) popupShape.getObject(),
							tmpCanvas);
					}
				});
				shapeMenu.add(MI);
				break;
			case 0 :
				MI = new JMenuItem("Primitive Operator Property");
				MI.addActionListener(new java.awt.event.ActionListener() {
					public void actionPerformed(ActionEvent e) {
						if (shapeParent != null) /* Weihong 12/3/02 */
							tabParent.addOPTab(
								shapeParent.getLabel()
									+ "["
									+ popupShape.getLabel()
									+ "]",
								theFrame);
						TransitionCanvas tmpCanvas =
							(TransitionCanvas) tabParent.getSelectedCanvas();
						ActionWindow.showOperatorGraphics(
							(oclOperator) popupShape.getObject(),
							tmpCanvas);
					}
				});
				shapeMenu.add(MI);
				break;
			case 3 :
				MI = new JMenuItem("World State Property");
				MI.addActionListener(new java.awt.event.ActionListener() {
					public void actionPerformed(ActionEvent e) {
						if (shapeParent != null) /* Weihong 12/3/02 */
							tabParent.addAchieveTab(
								shapeParent.getLabel() + "[achieve]",
								theFrame);
						oclSS ss = (oclSS) popupShape.getObject();
						/* Weihong 26/4/02 */
						MethodHeadCanvas.showOclSS(
							(TransitionCanvas) tabParent.getSelectedCanvas(),
							ss);
					}
				});
				shapeMenu.add(MI);
				break;
		}
	}

	/* Weihong 26/4/02 */
	/**
	 * Show oclSS detail onto another JGraphCanvas.
	 * @param canvas the target JGraphCanvas
	 * @param ss which reprents the oclSS
	 * 
	 */
	public static void showOclSS(TransitionCanvas canvas, oclSS ss) {
		canvas.setDrawingShapeID(3);
		vShape vs = canvas.createVShape(10, 10);
		if (vs.getShapeID() != 0)
			canvas.add(vs);
		vs.textField.setCurDomain(canvas.getWorkingDomain());
		vs.textField.setObjectID(ss.getName());
		vs.textField.setTransElement(jplan.general.TransExpressionDocPane.PREV);
		vs.setDrawLabel(false);

		ListIterator lii = ss.getState().listIterator();
		while (lii.hasNext()) {
			try {
				oclPredicate opt = (oclPredicate) lii.next();
				vs.textField.addPredicate(opt);
			} catch (Exception e) {
				jplan.general.Utility.debugPrintln(
					"Failed to insert predicate " + e.toString());
			}
		}
		vs.setLabel(ss.toString());
		vs.setObject(ss);
		canvas.repaint();
	}

	/**
	 * to show the dialog window for rename of the shape
	 * 
	 */
	protected void showRenameWindow(vShape vs) {
		String name =
			jplan.general.GipoInputBox.showIdentifierInputBox(
				theFrame.top,
				"Operator Name",
				"Please rename the operator.",
				vs.getLabel());
		if (name != null) {
			vs.setLabel(name);
			repaint();
		}
	}

	/**
	 * dobule click to show the property.
	 * @param me MouseEvent
	 * 
	 */
	public void mouseClicked(MouseEvent me) {
		//disable mouseclick.
	}

	/**
	 * When mouse pressed/clicked. Right click to show all functions;
	 * left click to either show help text or create/select/delete a vShape/vLink.
	 * @param me MouseEvent
	 * 
	 */
	public void mousePressed(MouseEvent me) {
		if (me.getClickCount() == 1
			&& me.getModifiers() == MouseEvent.BUTTON1_MASK) {
			if (mouseAction == 0)
				JOptionPane.showMessageDialog(
					null,
					"Right click mouse the canvas to see the functions for editing.",
					"GIPO Information",
					JOptionPane.INFORMATION_MESSAGE,
					null);
			else
				super.mousePressed(me);
		}
		if (me.getClickCount() == 1
			&& me.getModifiers() == MouseEvent.BUTTON3_MASK) {
			// 	    if (shapeParent != null){/* WZ 3/4/02 */
			// 		for (int i = 1; i < vShapeList.size()+1; i++) {
			// 		    vShape vs = (vShape)getShape(i);
			// 		    if (vs.contains((double)me.getX(),(double)me.getY())) {
			// 			initShapeMenu(vs);
			// 			shapeMenu.show(this, me.getX(), me.getY());
			// 			return;
			// 		    }
			// 		}
			// 	    }
			//else show the general menu bar
			popupMenu.show(this, me.getX(), me.getY());
		}
	}

	/* WZ 7/5/02 */
	/**
	   * Get the value of Decomposition.
	   * Add every shapes to the decomposition, and check every shape, 
	   * find it is relationship with other shapes and save the relationship.
	   * @return Value of Decomposition.
	   */
	public oclMethod getDecomposition() {
		Hashtable shapeList = (Hashtable) vShapeList.clone();
		oclMethod returnOMD = new oclMethod();
		oclPredicate tmpOMName = new oclPredicate("");

		vShape tmpShape = null;
		vLink tmpLink = null;
		int index = shapeList.size();
		String spID;
		int spKey;

		//collect all shapes and translate into decompositions
		for (int i = 1; i < shapeList.size() + 1; i++) {
			tempShape = getVShape(String.valueOf(i));

			//check every shape, and save the relationship information into temporal
			//check its in link
			for (int j = 1; j < tempShape.getInLinks().size() + 1; j++) {
				tmpLink = (vLink) tempShape.getInLinks().get("inlinks" + j);
				tmpShape = tmpLink.getStartShape();
				spID = String.valueOf(tmpShape.getID());
				spKey = getShapeKey(spID);
				returnOMD.addTempClauseBefore(
					String.valueOf(spKey),
					String.valueOf(i));
			}
			//check its outlink
			for (int k = 1; k < tempShape.getOutLinks().size() + 1; k++) {
				tmpLink = (vLink) tempShape.getOutLinks().get("outlinks" + k);
				tmpShape = tmpLink.getStopShape();
				spID = String.valueOf(tmpShape.getID());
				spKey = getShapeKey(spID);
				returnOMD.addTempClauseBefore(
					String.valueOf(i),
					String.valueOf(spKey));
			}

			//collect all shapes and translate into decompositions
			switch (tempShape.getShapeID()) {
				case 3 : //oclSS
					oclSS ss = (oclSS) tempShape.getObject();
					returnOMD.addDecomps(ss);
					break;
				case 5 : //oclMethod
					oclPredicate opd = (oclPredicate) tempShape.getObject();
					returnOMD.addDecomps(opd);
					break;
				case 0 : //oclOperator
					opd =
						(oclPredicate)
							((oclOperator) tempShape.getObject()).opName;
					returnOMD.addDecomps(opd);
					break;
				case 6 : /* WZ 21/8/02 UFO */
					opd = (oclPredicate) tempShape.getObject();
					returnOMD.addDecomps(opd);
					break;
			}
		}

		return returnOMD;
	}

	//     /* Weihong  19/11/2001 */
	//     /**
	//        * Get the value of Decomposition.
	//        * @return Value of Decomposition.
	//        */
	//     public oclMethod getDecomposition() {
	// 	Hashtable shapeList = (Hashtable)vShapeList.clone();
	// 	oclMethod returnOMD = new oclMethod();
	// 	oclPredicate tmpOMName = new oclPredicate(""); /* WZ 19/3/02 */

	// 	vShape lastShape = null;
	// 	vLink tmpLink = null;
	// 	int index = shapeList.size();
	// 	Object tmpValue = null;
	// 	int counterLastShape = 0;
	// 	//find all vShapes which are registered, find the last shape in the chain.
	// 	for (int i = 1; i < shapeList.size()+1; i++) {
	// 	    tempShape = (vShape)getShape(i);
	// 	    //find the last shape in the chain
	// 	    if (tempShape.getOutLinks().size() == 0){
	// 		if (tempShape.getInLinks().size() != 0) {
	// 		    if (counterLastShape < 1){
	// 			lastShape = tempShape;
	// 			counterLastShape++;
	// 		    }
	// 		    else {
	// 			JOptionPane.showMessageDialog(this ,
	// 					      "Please relink shapes to have only one shape at the end of the chain.",
	// 					      "GIPO ERROR",
	// 					      JOptionPane.ERROR_MESSAGE,null); 
	// 			return null;
	// 		    }
	// 		}
	// 		else {
	// 		    JOptionPane.showMessageDialog(this ,
	// 						  "Found shape(s) without linking to any others.",
	// 						  "GIPO ERROR",
	// 						      JOptionPane.ERROR_MESSAGE,null); 
	// 		    return null;
	// 		}
	// 	    }
	// 	}

	// 	//check if found the last shape
	// 	if (lastShape == null){
	// 	    if (shapeList.size() == 0) //
	// 		return returnOMD;
	// 	    else {
	// 		JOptionPane.showMessageDialog(this ,
	// 					      "Please spcify the sequencial relationship between decomposed actions.",
	// 					      "GIPO Warning",
	// 					      JOptionPane.WARNING_MESSAGE,null); 
	// 		return returnOMD;
	// 	    }
	// 	}

	// 	shapeList.clear();
	// 	shapeList.put("shapeKey"+index, (Object)String.valueOf(lastShape.getID()));

	// 	//to check all inlink of the last shape
	// 	findPreviousShape(lastShape, index, returnOMD, shapeList);

	// 	//reset the decomposition
	// 	for (int i = 1; i < shapeList.size()+1; i++) {
	// 	    tempShape = (vShape)getShape(i);
	// 	    switch (tempShape.getShapeID()){
	// 	    case 3: //oclSS
	// 		oclSS ss = (oclSS)tempShape.getObject();
	// 		returnOMD.addDecomps(ss);
	// 		//get signature for decompostion
	// 		addArgList(tmpOMName, ss.getState());/* WZ 19/3/02 */
	// 		break;
	// 	    case 5: //oclMethod
	// 		oclPredicate opd = (oclPredicate)tempShape.getObject();
	// 		returnOMD.addDecomps(opd);
	// 		curDomain.addSignatureArgument(tmpOMName, opd);/* WZ 19/3/02 */
	// 		break;	
	// 	    case 0: //oclOperator
	// 		opd = (oclPredicate)((oclOperator)tempShape.getObject()).opName;
	// 		returnOMD.addDecomps(opd);
	// 		curDomain.addSignatureArgument(tmpOMName, opd);/* WZ 19/3/02 */
	// 		break;	
	// 	    }
	// 	}

	// 	//update the vShapeList
	// 	vShapeList = shapeList;

	// 	returnOMD.setName(tmpOMName); /* WZ 19/3/02 */
	// 	return returnOMD;
	//     }

	private void addArgList(oclPredicate operatorName, List givenOPDList) {
		ListIterator liOPD = givenOPDList.listIterator();
		while (liOPD.hasNext()) {
			oclPredicate opd = (oclPredicate) liOPD.next();
			curDomain.addSignatureArgument(operatorName, opd);
		}
	}

	private void findPreviousShape(
		vShape vs,
		int index,
		oclMethod returnOMD,
		Hashtable shapeList) {
		for (int i = 1; i < vs.getInLinks().size() + 1; i++) {
			vLink tmpLink = (vLink) vs.getInLinks().get("inlinks" + i);
			vShape tmpShape = tmpLink.getStartShape();
			Object tmpValue = (Object) String.valueOf(tmpShape.getID());
			index--;
			shapeList.put("shapeKey" + index, tmpValue);
			//register in oclMethod
			returnOMD.addTempClauseBefore(
				String.valueOf(index),
				String.valueOf(index + 1));
			//recursing
			findPreviousShape(tmpShape, index, returnOMD, shapeList);
		}
	}

	/* Weihong  19/11/2001 */

	/**
	 * Set the value of Decomposition.
	 * @param canvas
	 * @param curDomain
	 * @param omd
	 */
	public static void setDecomposition(
		MethodHeadCanvas canvas,
		oclDomain curDomain,
		oclMethod omd) {
		//draw shapes
		vShape vs;
		int x = 20, y = 20;
		int k = 0;
		ListIterator li = ((List) omd.getDecomps()).listIterator();
		while (li.hasNext()) {
			Object obj = (Object) li.next();
			if (obj.getClass().getName() == "jplan.ocl.oclSS") {
				oclSS ss = (oclSS) obj;
				drawOclSS(
					canvas,
					curDomain,
					ss,
					(int) (x + k * (canvas.getDefaultWidth() + 20)),
					(int) (y + k * (canvas.getDefaultHeight() + 20)));
			} else if (obj.getClass().getName() == "jplan.ocl.oclPredicate") {
				Object object = curDomain.checkObjectType((oclPredicate) obj);
				jplan.general.Utility.debugPrintln("obj - " + obj.toString());
				if (object == null) { /* WZ 21/8/02 */
					drawUnknownOM(
						canvas,
						(oclPredicate) obj,
						(int) (x + k * (canvas.getDefaultWidth() + 20)),
						(int) (y + k * (canvas.getDefaultHeight() + 20)));
				} else if (
					object.getClass().getName() == "jplan.ocl.oclOperator") {
					oclOperator op = (oclOperator) object;
					drawOclOperator(
						canvas,
						op,
						(int) (x + k * (canvas.getDefaultWidth() + 20)),
						(int) (y + k * (canvas.getDefaultHeight() + 20)));
				} else if (
					object.getClass().getName() == "jplan.ocl.oclMethod") {
					oclMethod om = (oclMethod) object;
					/* WZ 17/4/02 */
					//changed to attach only an oclPredicate to the shape
					drawOclMethod(
						canvas,
						om.getName(),
						(int) (x + k * (canvas.getDefaultWidth() + 20)),
						(int) (y + k * (canvas.getDefaultHeight() + 20)));
				}
			}
			k++;
		}
		//link shapes
		vShape fromShape = null, toShape = null;
		String fromShapeKey = null, toShapeKey = null;
		oclPredicate oprd = null;
		li = ((List) omd.getTemps()).listIterator();
		while (li.hasNext()) {
			oprd = (oclPredicate) li.next();
			fromShapeKey = oprd.getNthElementName(0);
			fromShape = canvas.getVShape(fromShapeKey);
			toShapeKey = oprd.getNthElementName(1);
			toShape = canvas.getVShape(toShapeKey);
			canvas.createVLink(fromShape, toShape);
		}
	}

	/* WZ 21/8/02 */
	/**
	 * draw the UFO's image on this MethodCanvas
	 * @param UFO the Unknown OM
	 * 
	 */
	private static void drawUnknownOM(
		MethodHeadCanvas canvas,
		oclPredicate UFO,
		int x,
		int y) {
		canvas.setDrawingShapeID(6);
		vShape vs = canvas.createVShape(x, y);
		vs.removeTextField();
		vs.setLabel(UFO.getName());
		vs.setObject(UFO);
	}

	/* Weihong  30/11/2001 */
	/**
	 * draw the oclOperator's image on this MethodCanvas
	 * @param op the given oclOperator
	 * 
	 */
	private static void drawOclOperator(
		MethodHeadCanvas canvas,
		oclOperator op,
		int x,
		int y) {
		canvas.setDrawingShapeID(0);
		vShape vs = canvas.createVShape(x, y);
		vs.removeTextField();
		vs.setLabel(op.opName.getName());
		vs.setObject(op);
	}

	/* Weihong  30/11/2001 */
	/**
	 * draw the oclSS's image on this MethodCanvas
	 * @param ss the given oclSS
	 * 
	 */
	private static void drawOclSS(
		MethodHeadCanvas canvas,
		oclDomain curDomain,
		oclSS ss,
		int x,
		int y) {
		canvas.setDrawingShapeID(3);
		vShape vs = canvas.createVShape(x, y);
		vs.textField.setCurDomain(curDomain);
		vs.textField.setObjectID(ss.getName());
		vs.textField.setTransElement(jplan.general.TransExpressionDocPane.PREV);
		ListIterator lii = ss.getState().listIterator();
		while (lii.hasNext()) {
			try {
				oclPredicate opt = (oclPredicate) lii.next();
				vs.textField.addPredicate(opt);
			} catch (Exception e) {
				jplan.general.Utility.debugPrintln(
					"Failed to insert predicate " + e.toString());
			}
		}
		vs.setLabel(ss.toString());
		vs.setObject(ss);
	}

	/* WZ 17/4/02 */
	/**
	 * draw the oclMethod's image on this MethodCanvas
	 * @param canvas
	 * @param omName the given oclMethod name - compound operator
	 * @param x
	 * @param y
	 */
	private static void drawOclMethod(
		MethodHeadCanvas canvas,
		oclPredicate omName,
		int x,
		int y) {
		canvas.setDrawingShapeID(5);
		vShape vs = canvas.createVShape(x, y);
		vs.removeTextField();
		vs.setLabel(omName.getName());
		vs.setObject(omName);
	}

	/* Weihong  19/11/2001 */
	/**
	 * Create the vShape at the given location. <br>
	 * Register with the vShapeList;
	 * Add it to the screen; 
	 * @param xPoint int value at X axis
	 * @param yPoint int value at Y axis
	 * @return the vShape created 
	 */
	public vShape createVShape(int xPoint, int yPoint) {
		shapeIndex++;
		shapeID++;

		//create the vShape
		vShape tempShape =
			new vShape(
				shapeID,
				(double) xPoint,
				(double) yPoint,
				d_Width,
				d_Height);

		tempShape.setShapeID(shapeTypeID);

		//register with the vShapeList
		vShapeList.put(
			"shapeKey" + shapeIndex,
			(Object) String.valueOf(shapeID));
		shape[shapeID] = tempShape;

		return tempShape;
	}

} // MethodHeadCanvas
