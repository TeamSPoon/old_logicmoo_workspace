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
 * VarEditWindow.java
 *
 *
 * Created: Mon Mar 11 10:34:28 2002
 *
 * @author W Zhao
 * @version
 */

import jplan.graphics.transition.*;
import jplan.ocl.*;
import jplan.graphics.gTool.Graphics.vShape;

public class VarEditWindow extends javax.swing.JDialog {
    private oclDomain curDomain;
    private VarEditCanvas drawingCanvas;
    private MethodHeadCanvas methodCanvas;
    private java.util.Hashtable shapeList;
    private vShape[] shapeArray;
    private HighLevelTransitionWindow theParentFrame;
    private javax.swing.JScrollPane centerPanel;  /* WZ 28/3/02 */
    private boolean embedded; /* WZ 3/4/02 */
    private oclPredicate restoredMethod;/* WZ 4/4/02 */


    public VarEditWindow(jplan.top.OclEd top, java.util.Hashtable shapeList, vShape[] shapeArray, HighLevelTransitionWindow theParentFrame, boolean embedded) {
// 	super ();
	this.curDomain = top.curDomain;
//	this.methodCanvas = methodCanvas;
	this.shapeList = shapeList;
	this.shapeArray = shapeArray;
	this.theParentFrame = theParentFrame;
	this.embedded = embedded;/* WZ 3/4/02 */
	/* WZ 4/4/02 */
	if (embedded){
	    oclPredicate tmpPrd = (oclPredicate)theParentFrame.getCanvas().getMethodHead().getObject();
	    try {
		restoredMethod = (oclPredicate)tmpPrd.clone();
	    } catch (Exception e) {
		javax.swing.JOptionPane.showMessageDialog(this ,
					      "Failed to clone Method Head.",
					      "GIPO Error",
					      javax.swing.JOptionPane.ERROR_MESSAGE,null); 
	    }
	}
	/* WZ 4/4/02 end */

	setTitle("Variable Editing Window");

	initComponents ();
 	pack ();
	setSize(650, 800);
	setLocation(getToolkit().getScreenSize().width/2 - getBounds().width/2,
		    getToolkit().getScreenSize().height/2 - getBounds().height/2);
	setVisible(false);
    }
   

    /**
     * Initialisation
     * 
     */
    protected void initComponents () {
	getContentPane ().setLayout (new java.awt.BorderLayout ());

	refreshCanvas();

	javax.swing.JToolBar bottomPanel = new javax.swing.JToolBar ();
	bottomPanel.setLayout (new java.awt.FlowLayout ());
	bottomPanel.setFloatable(false);

	javax.swing.JButton okButton = new javax.swing.JButton(" OK ");
	okButton.addActionListener (new java.awt.event.ActionListener () {
	    public void actionPerformed (java.awt.event.ActionEvent evt) {
		jbn_OKActionPerformed ();
	    }
	}  );
	bottomPanel.add (okButton);

	javax.swing.JButton cancelButton = new javax.swing.JButton ("  Cancel  ");
	cancelButton.addActionListener (new java.awt.event.ActionListener () {
	    public void actionPerformed (java.awt.event.ActionEvent evt) {
		jbn_CancelActionPerformed ();
	    }
	}  );
	bottomPanel.add (cancelButton);

	javax.swing.JButton bt = new javax.swing.JButton(" Zoom In ");
	bt.setAlignmentY(0.5f);
	bt.addActionListener(new java.awt.event.ActionListener() {
	    public void actionPerformed(java.awt.event.ActionEvent e) {
		jMI_ZoomInActionPerformed ();
	    }
	});
	bottomPanel.add(bt);
	
	bt = new javax.swing.JButton(" Zoom Out ");
	bt.setAlignmentY(0.5f);
	bt.addActionListener(new java.awt.event.ActionListener() {
	    public void actionPerformed(java.awt.event.ActionEvent e) {
		jMI_ZoomOutActionPerformed ();
	    }
	});
	bottomPanel.add(bt);

	getContentPane ().add (bottomPanel, "South");
    }

    /* WZ 3/4/02 */
    /**
     * return true is this window is called by a methodHeadCanvas,
     * which is embedded in a vShape
     * 
     */
    public boolean isEmbedded(){
	return embedded;
    }

    /* WZ 18/3/02 */
    /**
     * return parent frame
     * 
     */
    public HighLevelTransitionWindow getParentHT(){
	return theParentFrame;
    }

    /** 
     * draw all shapes in the drawingCavas
     *
     */
    private void drawShapes(){
	int y = 10;
	int lineSpace = 50;
	for (int i = 1; i < shapeList.size()+1; i++) {
	    Object tmpValue = shapeList.get("shapeKey"+i);
	    vShape tempShape = (vShape)shapeArray[Integer.parseInt(tmpValue.toString())];
	    
	    switch (tempShape.getShapeID()){
	    case 3: //oclSS
		try {
		    oclSS ss = (oclSS)((oclSS)tempShape.getObject()).clone();
// 		    oclSS ss = (oclSS)tempShape.getObject();
		    y = drawingCanvas.showOclSS(ss, y)+lineSpace;
		} catch (CloneNotSupportedException e){}
		break;
	    case 5: //oclMethod
		oclMethod md = (oclMethod)tempShape.getObject();
		y = drawingCanvas.showOclMethod(md, y)+lineSpace;
		break;	
	    case 0: //oclOperator
		oclOperator op = (oclOperator)tempShape.getObject();
		y = drawingCanvas.showOperatorGraphics(op, y)+lineSpace;
		break;	
	    }
	}
    }

     /* WZ 28/3/02 */
    /**
     * to update the canvas content.
     * first to remove existing canvas, then add a new one, then redraw shapes
     * 
     */
    public void refreshCanvas(){
	if (centerPanel != null)
	    getContentPane ().remove(centerPanel);
	drawingCanvas = new VarEditCanvas(this);
	drawShapes();
	centerPanel = new javax.swing.JScrollPane(drawingCanvas);
	getContentPane ().add (centerPanel, "Center");
    }

     /* WZ 27/3/02 */
    /**
     * return the parent's default canvas
     * @return the parent's default canvas
     */
    public HighLevelTransitionCanvas getParentCanvas(){
	return theParentFrame.getCanvas();
    }

    /** 
     * when ok button to be pressed, close this dialog window
     * and show the graphics at parent's canvas.
     * 
     */
    private void jbn_OKActionPerformed () {
	if (!isEmbedded()){
	    //take the changed variables and save them
	    Object objectArray [] = drawingCanvas.updateVar();
	    for (int i = 0; i < objectArray.length; i++) {
		vShape tempShape = (vShape)shapeArray[i+1];
		
		switch (tempShape.getShapeID()){
		case 3: //oclSS
		    tempShape.setObject((oclSS)objectArray[i]);
		    tempShape.setLabel(((oclSS)objectArray[i]).toString());/* WZ 3/4/02 */
		    break;
		case 5: //oclMethod
		    tempShape.setObject((oclMethod)objectArray[i]);
		    Utility.debugPrintln("jbn_OKActionPerformed ==> "+((oclMethod)objectArray[i]).toString());
		    break;	
		case 0: //oclOperator
		    tempShape.setObject((oclOperator)objectArray[i]);
		    break;	
		}
	    }
	    //to repaint methodheadcanvas
	    theParentFrame.updateUI();/* WZ 4/4/02 */
	}

	closeDialog();
    }
    
    /** 
     * when cancel button is pressed, close this dialog window
     * 
     */
    private void jbn_CancelActionPerformed () {
	closeDialog();
	resetHeadShape();
    }

    /* WZ 4/4/02 */
    /** 
     * when cancel button is pressed, close this dialog window
     * 
     */
    private void resetHeadShape() {
	if (isEmbedded()){
	    theParentFrame.getCanvas().getMethodHead().setObject(restoredMethod);
	    theParentFrame.refreshCanvas();
	}
    }

    /** 
     * close this dialog window
     * 
     */
    private void closeDialog() {
	setVisible (false);
// 	dispose ();
    }

    /**
     * to zoom out the view of the canvas
     * 
     */
    private void jMI_ZoomOutActionPerformed () {
	drawingCanvas.setScale(1/1.2);
    }

    /**
     * to zoom in the view of the canvas
     * 
     */
    private void jMI_ZoomInActionPerformed () {
	drawingCanvas.setScale(1.2);
    }
} // VarEditWindow
