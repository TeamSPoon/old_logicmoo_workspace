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
 * VarEditCanvas.java
 *
 *
 * Created: Mon Mar 11 09:58:57 2002
 *
 * @author W Zhao
 * @version
 */
import jplan.graphics.transition.*;
import jplan.ocl.*;
import jplan.graphics.gTool.Graphics.vShape;
import jplan.graphics.gTool.Graphics.vLink;
import java.util.ListIterator;
import java.util.Hashtable;/* WZ 18/3/02 */
import javax.swing.JOptionPane;/* WZ 18/3/02 */


public class VarEditCanvas extends HighLevelTransitionCanvas {
    private oclDomain curDomain;
    private VarEditWindow theParentFrame;
    private Hashtable specialList = new Hashtable(); /* WZ 18/3/02 */
    private int spIndex = 0;

    public VarEditCanvas(VarEditWindow theParentFrame) {
	super(theParentFrame.getParentHT().getWorkingDomain());
	this.theParentFrame = theParentFrame;
	this.curDomain = theParentFrame.getParentHT().getWorkingDomain();
	setEditMode(true);
    }

    /**
     * Returns the default size of this canvas.
     * @return Dimension
     */
    public java.awt.Dimension getPreferredSize() {
// 	return new Dimension(getToolkit().getScreenSize());
	return new java.awt.Dimension(getBounds().width, getBounds().height);
    }

    /**
     * Zoom in/out
     * @param ss the zoom rate: zoom in if more than 1; zoom out if less than 1.
     * 
     */  
    public void setScale(double ss) {
	vShape vs = null;
	int maxW = 0, maxH = 0;	/* Weihong 11/3/02 */
	for (int j =1; j < vShapeList.size()+1; j++) {
	    vs = (vShape)shape[Integer.parseInt(vShapeList.get("shapeKey"+j).toString())];
	    vs.scale(ss);
	    //vs.setBounds((int)vs.px-4, (int)vs.py-4, (int)vs.width+8, (int)vs.height+8);
	    vs.setBounds(0,0,getWidth(),getHeight());

	    /* Weihong 11/3/02 */
	    //record the maximum scale
	    maxW = java.lang.Math.max(maxW, (int)(vs.px+vs.width));
	    maxH = java.lang.Math.max(maxH, (int)(vs.py+d_Height));
	}
	repaint();
	
	//change the default drawing parameters for the next
	d_Width *= ss;
	d_Height *= ss;

	/* Weihong 11/3/02 */
	//resize the canvas when required
	setSize(maxW + 100, maxH + 100);

    }

    /* WZ 3/4/02 */
    /**
     * register given vshape's transExpresionDocPane to monitor.
     * @param vs vShape
     * 
     */ 
    private void monitor(vShape vs){
	if (theParentFrame.isEmbedded()){
	    theParentFrame.getParentCanvas().addToPaneGroup(vs.textField);    /* WZ 27/3/02 */
	    theParentFrame.getParentCanvas().addToMonitor(vs.textField); /* WZ 28/3/02 */
	}
    }

    /**
     * Show oclSS detail onto another JGraphCanvas.
     * @param ss
     * @param yPosition
     * @return the last shape's lowest position in Y axis
     */ 
    public int showOclSS(jplan.ocl.oclSS ss, int yPosition){
	setDrawingShapeID(3);
	vShape vs = createVShape(30, yPosition);
	monitor(vs);/* WZ 3/4/02 */
 	if (vs.getShapeID() != 0)
	    add (vs);
	vs.textField.setCurDomain(curDomain);
	vs.textField.setObjectID(ss.getName());
	vs.textField.setTransElement(jplan.general.TransExpressionDocPane.PREV); 
	vs.setDrawLabel(false);
	
	ListIterator lii = ss.getState().listIterator();
	while (lii.hasNext()) {
	    try {
		oclPredicate opt = (oclPredicate)lii.next();
		vs.textField.addPredicate(opt);
	    } catch (Exception e) {
		jplan.general.Utility.debugPrintln("Failed to insert predicate " + e.toString());
	    }
	}
	vs.setLabel(ss.toString());
	vs.setObject(ss);

	spIndex++;
	specialList.put("special"+spIndex, (Object)String.valueOf(vs.getID()));/* WZ 18/3/02 */

	return yPosition+(int)vs.height;
    }

    /**
     * to show the graphical representation of the given
     * oclOperator on the given canvas.
     * @param op the operator which has been double clicked
     * @param yPosition where to show the graphical representation
     * 
     */
    public int showOperatorGraphics(oclOperator op, int yPosition) {
	int i=0, x1=30, y1=yPosition, x2 = 430;
	vShape vsID, vs;
	oclPredicate opd;
	int lineSpace = 20;

// 	addMListener();

	//draw the Id
	setMouseAction(CREATE_SHAPE);
	setDrawingShapeID(0); 
	i = op.getPrevail().size() + op.getNecessary().size() + op.getConditional().size();
	vsID = createVShape(260, (i-1)*50+y1);
	vsID.removeTextField();
	setOPName(vsID);//set center shape
	vsID.setLabel(op.opName.getName()); //operator's name(predicate)'s name(string)
	vsID.setObject(op.opName);

	//draw prevail conditions
	ListIterator li = op.getPrevail().listIterator();
	while (li.hasNext()) {
	    oclSE se = (oclSE)li.next();
	    setDrawingShapeID(1); //set the mousemode
	    vs = createVShape(x1, y1);
	    monitor(vs);/* WZ 3/4/02 */
	    vs.textField.setCurDomain(curDomain);
	    vs.textField.setObjectID(se.getName()); 
	    vs.textField.setTransElement(TransExpressionDocPane.PREV);
	    vs.setDrawLabel(false);
	    ListIterator lili = se.getPredicateList().listIterator();
	    while (lili.hasNext()) {
		try {
		    oclPredicate opt = (oclPredicate)lili.next();
		    vs.textField.addPredicate(opt);
		} catch (Exception e) {
		    Utility.debugPrintln("Failed to insert predicate " + e.toString());
		}
	    }
	    vs.setLabel(se.toString());
	    vs.setObject(se);

	    setDrawingLinkID(0);
	    createVLink(vs, vsID);//set the relationship

	    y1 += (int)vs.height + lineSpace;
	}

	//draw necessary change
	li = op.getNecessary().listIterator();
	while (li.hasNext()) {
	    oclSC necessary = (oclSC)li.next();
	    setDrawingShapeID(2); //set the mousemode
	    vs = createVShape(x1, y1);
	    monitor(vs);/* WZ 3/4/02 */
	    vs.textField.setCurDomain(curDomain);
	    vs.textField.setObjectID(necessary.getName());
	    vs.textField.setTransElement(TransExpressionDocPane.LHS); 
	    vs.setDrawLabel(false);
	    String str = "sc(" + necessary.getSort() + "," + necessary.getName() + ",";
	    vs.setLabel(str + necessary.getPre().toString());
	    vs.setObject(necessary);
	    ListIterator lili = necessary.getPre().listIterator();
	    while (lili.hasNext()) {
		try {
		    oclPredicate opt = (oclPredicate)lili.next();
		    vs.textField.addPredicate(opt);
		} catch (Exception e) {
		    Utility.debugPrintln("Failed to insert predicate " + e.toString());
		}
	    }

// 	    setMouseAction( CREATE_LINK);
	    setDrawingLinkID(calculateLinkTypeID());
	    createVLink(vs, vsID);//set the relationship

// 	    setDrawingShapeID(2);; //set the mousemode
	    vs = createVShape(x2, y1);
	    monitor(vs);/* WZ 3/4/02 */
	    vs.textField.setCurDomain(curDomain);
	    vs.textField.setObjectID(necessary.getName()); 
	    vs.textField.setTransElement(TransExpressionDocPane.RHS); 
	    vs.setDrawLabel(false);
	    vs.setLabel(str+ necessary.getPost().toString());
	    vs.setObject(necessary);
	    lili = necessary.getPost().listIterator();
	    while (lili.hasNext()) {
		try {
		    oclPredicate opt = (oclPredicate)lili.next();
		    vs.textField.addPredicate(opt);
		} catch (Exception e) {
		    Utility.debugPrintln("Failed to insert predicate " + e.toString());
		}
	    }

// 	    setMouseAction( CREATE_LINK);
	    createVLink(vsID, vs);//set the relationship

	    y1 += (int)vs.height + lineSpace;
	}

	//draw conditional change
	li = op.getConditional().listIterator();
	while (li.hasNext()) {
	    oclSC conditional = (oclSC)li.next();
	    setDrawingShapeID(3);//set the mousemode
	    vs = createVShape(x1, y1);
	    monitor(vs);/* WZ 3/4/02 */
	    vs.textField.setCurDomain(curDomain);
	    vs.textField.setObjectID(conditional.getName());
	    vs.textField.setTransElement(TransExpressionDocPane.LHS);
	    vs.setDrawLabel(false);
	    String str = "sc(" + conditional.getSort() + "," + conditional.getName() + ",";
	    vs.setLabel(str + conditional.getPre().toString());
	    vs.setObject(conditional);
	    ListIterator lili = conditional.getPre().listIterator();
	    while (lili.hasNext()) {
		try {
		    oclPredicate opt = (oclPredicate)lili.next();
		    vs.textField.addPredicate(opt);
		} catch (Exception e) {
		    Utility.debugPrintln("Failed to insert predicate " + e.toString());
		}
	    }
// 	    setMouseAction( CREATE_LINK);
	    setDrawingLinkID(calculateLinkTypeID());
	    createVLink(vs, vsID);//set the relationship

// 	    setDrawingShapeID(3);//set the mousemode
	    vs = createVShape(x2, y1);
	    monitor(vs);/* WZ 3/4/02 */
	    vs.textField.setCurDomain(curDomain);
	    vs.textField.setObjectID(conditional.getName()); 
	    vs.textField.setTransElement(TransExpressionDocPane.RHS); 
	    vs.setDrawLabel(false);
	    vs.setObject(conditional);
	    lili = conditional.getPost().listIterator();
	    while (lili.hasNext()) {
		try {
		    oclPredicate opt = (oclPredicate)lili.next();
		    vs.textField.addPredicate(opt);
		} catch (Exception e) {
		    Utility.debugPrintln("Failed to insert predicate " + e.toString());
		}
	    }
// 	    setMouseAction( CREATE_LINK);
	    createVLink(vsID, vs);//set the relationship

	    y1 += (int)vs.height + lineSpace;
	}

	setMouseAction(SELECT);
// 	recordState(); //record state for undo

// 	repaint();

	spIndex++;
	specialList.put("special"+spIndex, (Object)String.valueOf(vsID.getID()));/* WZ 18/3/02 */

	return y1;
    }


    /* Weihong 11/3/02 */
    /**
     * to show the graphical representation of the given
     * oclMethod on the given canvas.
     * @param oMethod the compound operator
     * @param yPosition where to show the graphical representation
     * 
     */
    public int showOclMethod(oclMethod oMethod, int yPosition) {
	int i=0, x1=30, y1=yPosition, x2 = 430;
	vShape vsID, vs;
	oclPredicate opd;
	int lineSpace = 20;

// 	addMListener();

	//draw the Id
	setDrawingShapeID(5); 
	i = oMethod.getPrecondition().size() + oMethod.getIndex().size() + 1;
	vsID = createVShape(260, (i-1)*50+y1);
	vsID.removeTextField();
	/* Weihong added on 30/11/2001 */
	MethodHeadCanvas methodCanvas = new MethodHeadCanvas(true, vsID, theParentFrame.getParentHT());
	vsID.addDecompCanvas(methodCanvas); 
	setOPName(vsID);//set center shape
	vsID.setLabel(oMethod.getName().getName()); 
// 	vsID.setObject(oMethod.getName());
	vsID.setObject(oMethod);/* WZ 5/4/02 */
	initPopupMenuForMethodHead();   

	//draw preconditions
	ListIterator li = oMethod.getPrecondition().listIterator();
	while (li.hasNext()) {
	    oclSE se = (oclSE)li.next();
	    setDrawingShapeID(1);
	    vs = createVShape(x1, y1);
	    monitor(vs);/* WZ 3/4/02 */
	    monitorMethod(vs, vsID);/* WZ 5/4/02 */
	    vs.textField.setCurDomain(curDomain);
	    vs.textField.setObjectID(se.getName());
	    vs.textField.setTransElement(TransExpressionDocPane.PREV); 
	    vs.setDrawLabel(false);
	    ListIterator lili = se.getPredicateList().listIterator();
	    while (lili.hasNext()) {
		try {
		    oclPredicate opt = (oclPredicate)lili.next();
		    vs.textField.addPredicate(opt);
		} catch (Exception e) {
		    Utility.debugPrintln("Failed to insert predicate " + e.toString());
		}
	    }
	    vs.setLabel(se.toString());
	    vs.setObject(se);

	    setDrawingLinkID(0);
	    createVLink(vs, vsID);//set the relationship

	    y1 += (int)vs.height + lineSpace;
	}

	//draw Index
	li = oMethod.getIndex().listIterator();
	while (li.hasNext()) {
	    oclSC oclsc = (oclSC)li.next();
	    setDrawingShapeID(2);//set the mousemode
	    vs = createVShape(x1, y1);
	    monitor(vs);/* WZ 3/4/02 */
	    monitorMethod(vs, vsID);/* WZ 5/4/02 */
	    vs.textField.setCurDomain(curDomain);
	    vs.textField.setObjectID(oclsc.getName());
	    vs.textField.setTransElement(TransExpressionDocPane.LHS); 
	    vs.setDrawLabel(false);
	    String str = "sc(" + oclsc.getSort() + "," + oclsc.getName() + ",";
	    vs.setLabel(str + oclsc.getPre().toString());
	    vs.setObject(oclsc);
	    ListIterator lili = oclsc.getPre().listIterator();
	    while (lili.hasNext()) {
		try {
		    oclPredicate opt = (oclPredicate)lili.next();
		    vs.textField.addPredicate(opt);
		} catch (Exception e) {
		    Utility.debugPrintln("Failed to insert predicate " + e.toString());
		}
	    }
	    setMouseAction(CREATE_LINK);
	    setDrawingLinkID(calculateLinkTypeID());
	    createVLink(vs, vsID);//set the relationship

	    setDrawingShapeID(2); //set the mousemode
	    vs = createVShape(x2, y1);
	    monitor(vs);/* WZ 3/4/02 */
	    monitorMethod(vs, vsID);/* WZ 5/4/02 */
	    vs.textField.setCurDomain(curDomain);
	    vs.textField.setObjectID(oclsc.getName()); 
	    vs.textField.setTransElement(TransExpressionDocPane.RHS); 
	    vs.setDrawLabel(false);
	    vs.setLabel(str+ oclsc.getPost().toString());
	    vs.setObject(oclsc);
	    lili = oclsc.getPost().listIterator();
	    while (lili.hasNext()) {
		try {
		    oclPredicate opt = (oclPredicate)lili.next();
		    vs.textField.addPredicate(opt);
		} catch (Exception e) {
		    Utility.debugPrintln("Failed to insert predicate " + e.toString());
		}
	    }
	    setMouseAction( CREATE_LINK);
	    createVLink(vsID, vs);//set the relationship

	    y1 += (int)vs.height + lineSpace;
	}

 	//draw static conditions
	//attach a List to the object of the vShape
	li = ((java.util.List)oMethod.getStatics()).listIterator();
	if (li.hasNext()){
	    setDrawingShapeID(4); //set the mousemode
	    vs = createVShape(x1, y1);
	    monitor(vs);/* WZ 3/4/02 */
	    monitorMethod(vs, vsID);/* WZ 5/4/02 */
	    vs.setSize((double)200, (double)150);
	    vs.textField.setCurDomain(curDomain);
	    vs.textField.setTransElement(TransExpressionDocPane.PREV); //only left hand side
	    vs.setDrawLabel(false);
	    vs.setLabel(oMethod.getStatics().toString());
	    vs.setObject(oMethod.getStatics());
	    ListIterator lili = oMethod.getStatics().listIterator();
	    while (lili.hasNext()) {
		try {
		    oclPredicate opt = (oclPredicate)lili.next();
		    vs.textField.addPredicate(opt);
		} catch (Exception e) {
		    Utility.debugPrintln("Failed to insert predicate " + e.toString());
		}
	    }
	    setMouseAction(CREATE_LINK);
	    setDrawingLinkID(calculateLinkTypeID());
	    createVLink(vs, vsID);//set the relationship
	    
	    y1 += (int)vs.height + lineSpace;

	    setMouseAction(SELECT);
	}

	//do not draw decompositions
// 	MethodHeadCanvas.setDecomposition(methodCanvas, methodCanvas.getWorkingDomain(), oMethod);

	spIndex++;
	specialList.put("special"+spIndex, (Object)String.valueOf(vsID.getID()));/* WZ 18/3/02 */

	return y1;
    }

    /* WZ 5/4/02 */
    /**
     * Monitor the vShape which relates to oclMethod in this canvas.
     * @param vs vShape
     * 
     */ 
    private void monitorMethod(vShape vs, vShape methodHead){
	if (theParentFrame.isEmbedded())
	    return;

	oclMethod md = (oclMethod)methodHead.getObject();
	md.monitorMethod(vs);
    }
			
    /* WZ 27/3/02 */
    /**
     * Create the vShape at the given location. <br>
     * Register with the vShapeList;
     * Register with the textPane group;
     * Add it to the screen; 
     * Mark the dirty flag.
     * @param xPoint int value at X axis
     * @param yPoint int value at Y axis
     * @return the vShape created 
     */
    public vShape createVShape(int xPoint, int yPoint){
	if (!theParentFrame.isEmbedded()) /* WZ 3/4/02 */
	    return super.createVShape(xPoint, yPoint);

	shapeIndex ++;
	shapeID ++;
	
	/* Weihong 11/3/02 */
	//resize the canvas when required
	if (xPoint+d_Width > getBounds().width+100){
	    setSize((int)(xPoint + d_Width + 100), getBounds().height);
	}
	if (yPoint+d_Height > getBounds().height+100){
	    setSize(getBounds().width, (int)(yPoint+d_Height+100));
	}

	//create the vShape
	vShape tempShape = new vShape(shapeID,(double)xPoint,(double)yPoint, d_Width, d_Height);
	tempShape.setShapeID(shapeTypeID);	 

 	//register with the vShapeList
	vShapeList.put("shapeKey"+shapeIndex, (Object)String.valueOf(shapeID));
	shape[shapeID] = tempShape;
	
// 	//when used without being embedded in a vShape
// 	if (!isEmbedded()){ /* WZ 3/4/02 */
// 	    //register with the textPane group
// 	    if (tempShape.getShapeID() != 0 && tempShape.getShapeID() != 5)
// 		editPaneGroup.addPane(tempShape.textField);
// 	}

	//add it to the screen
	add (tempShape);
	tempShape.setBounds(0,0,getWidth(),getHeight());

	//mark the dirty flag
	isDirty = true; /* Weihong changed/added on 2/10/2001 */

	return tempShape;
    }

    /* WZ 18/3/02 */
    /** 
     * to save the updated variables to the object of the vShape
     * @return object array
     */
    public Object[] updateVar(){
	Object [] tmpOb = new Object[specialList.size()];
	Object returnOb = null;
	for (int i = 1; i < specialList.size()+1; i++) {
	    Object tmpValue = specialList.get("special"+i);
	    vShape tempShape = (vShape)shape[Integer.parseInt(tmpValue.toString())];
	    
	    switch (tempShape.getShapeID()){
	    case 3: //update oclSS
		returnOb = updateSS(tempShape);
		break;
	    case 5: //update oclMethod
		returnOb = updateMethod(tempShape);
		break;	
	    case 0: //update oclOperator
		returnOb = updateOperator(tempShape);
		break;	
	    }
	    tmpOb [i-1] = returnOb;
	}
	return tmpOb;
    }

    /* Weihong 18/3/02 */
    /**
     * return updated operator
     * @param vs vShape which represents an operator's head shape
     * @return oclOperator
     */
    public oclSS updateSS(vShape vs){
	oclSS tmpSS = null;
	if (isAncestorOf(vs)) {
	    //update objectID
	    ((oclSS)vs.getObject()).setName(vs.textField.getObjectID());
	    tmpSS = new oclSS(((oclSS)vs.getObject()).getSort(), ((oclSS)vs.getObject()).getName());
	    ListIterator l = vs.textField.getPurePredicateList().listIterator();
	    while (l.hasNext()) {
		oclPredicate oclpd = (oclPredicate)l.next();
		tmpSS.addPredicate(oclpd);
	    }
	}    
	return tmpSS;
    }

    /* Weihong 18/3/02 */
    /**
     * return updated operator
     * @param vs vShape which represents an operator's head shape
     * @return oclOperator
     */
    public oclOperator updateOperator(vShape vs){
	vLink tmpLink = null;
	vShape tmpShape = null;
	oclSC tmpSC = null;
	oclSE tmpSE = null;
	oclOperator tmpOP = new oclOperator();
	oclPredicate OPName = null, oprde = null;
	int k = 1;

	OPName = new oclPredicate(vs.getLabel());

	try {
	for (int i=1; i < vs.getInLinks().size()+1; i++) {
	    tmpLink =(vLink)vs.getInLinks().get("inlinks"+i);
	    tmpShape = tmpLink.getStartShape();
	    if (isAncestorOf(tmpShape)) {
		switch (tmpShape.getShapeID()) {
		case 1: //oclSE
		    //update objectID
		    ((oclSE)tmpShape.getObject()).setName(tmpShape.textField.getObjectID());
		    tmpSE = new oclSE(((oclSE)tmpShape.getObject()).getSort(), ((oclSE)tmpShape.getObject()).getName());
		    ListIterator l = tmpShape.textField.getPurePredicateList().listIterator();
		    while (l.hasNext()) {
			oclPredicate oclpd = (oclPredicate)l.next();
			tmpSE.addPredicate(oclpd);
			curDomain.addSignatureArgument(OPName, oclpd); //take the arguments for the operator
		    }
		    tmpOP.addPrevSE(tmpSE);
		    break;

		case 2: //oclSC
		    //update objectID
		    ((oclSC)tmpShape.getObject()).setName(tmpShape.textField.getObjectID());
		    /* to add preclause*/
		    tmpSC = new oclSC(((oclSC)tmpShape.getObject()).getSort(), ((oclSC)tmpShape.getObject()).getName());
		    l = tmpShape.textField.getPurePredicateList().listIterator();
		    while (l.hasNext()) {
			oclPredicate oclpd = (oclPredicate)l.next();
			tmpSC.addPre(oclpd);
			curDomain.addSignatureArgument(OPName, oclpd); //take the arguments for the operator
		    }
		    
		    /* to add PostClause*/
		    tmpLink =(vLink)vs.getOutLinks().get("outlinks"+k);
		    tmpShape = tmpLink.getStopShape();
		    l = tmpShape.textField.getPurePredicateList().listIterator();
		    while (l.hasNext()) {
			oclPredicate oclpd = (oclPredicate)l.next();
			tmpSC.addPost(oclpd);
			curDomain.addSignatureArgument(OPName, oclpd); //take the arguments for the operator
		    }	
		    tmpOP.addNecSC(tmpSC);
		    k++;
		    break;

		case 3: //oclSC
		    //update objectID
		    ((oclSC)tmpShape.getObject()).setName(tmpShape.textField.getObjectID());
		    /* to add preclause*/
		    tmpSC = new oclSC(((oclSC)tmpShape.getObject()).getSort(), ((oclSC)tmpShape.getObject()).getName());
		    l = tmpShape.textField.getPurePredicateList().listIterator();
		    while (l.hasNext()) {
			oclPredicate oclpd = (oclPredicate)l.next();
			tmpSC.addPre(oclpd);
		    }
		    
		    /* to add PostClause*/
		    tmpLink =(vLink)vs.getOutLinks().get("outlinks"+k);
		    tmpShape = tmpLink.getStopShape();
		    l = tmpShape.textField.getPurePredicateList().listIterator();
		    while (l.hasNext()) {
			oclPredicate oclpd = (oclPredicate)l.next();
			tmpSC.addPost(oclpd);
		    }
		    
		    tmpOP.addCondSC(tmpSC);
		    k++;
		    break;
		}
	    }
	    else if (tmpShape.getShapeID() > 1)
		k++;
	}
	}catch (Exception e){
	    JOptionPane.showMessageDialog(theParentFrame.getParentHT().top,"Creation of the current operator failed. Please check the pre-condition and post-condition exist in pair.",
					  "GIPO Error",JOptionPane.ERROR_MESSAGE,null);
	    return null;
	}
	    
	tmpOP.setName(OPName);

	return tmpOP;
    }

    /**
     * return updated method
     * @param vs vShape which represents an oclMethod's head shape
     * @return oclMethod
     */
    public oclMethod updateMethod(vShape vs){
	vLink tmpLink = null;
	vShape tmpShape = null;
	oclSC tmpSC = null;
	oclSE tmpSE = null;
	oclMethod tmpOP = new oclMethod();
	oclPredicate OPName = null, oprde = null;
	int k = 1;

	//save decomposition and temporal constraint
// 	oclMethod omdDEC = (oclMethod)((MethodHeadCanvas)vs.getDecompCanvas()).getDecomposition();
	oclMethod omdDEC = (oclMethod)vs.getObject();/* WZ 5/4/02 */
	if (omdDEC == null)
	    return null;

	tmpOP.setTemps(omdDEC.getTemps());
	tmpOP.setDecomps(omdDEC.getDecomps());

	//set the oclMethod name - oclPredicate
	OPName = new oclPredicate(vs.getLabel());

	try {
	    for (int i=1; i < vs.getInLinks().size()+1; i++) {
		tmpLink =(vLink)vs.getInLinks().get("inlinks"+i);
		tmpShape = tmpLink.getStartShape();
		if (isAncestorOf(tmpShape)) {
		    switch (tmpShape.getShapeID()) {
		    case 1: //save pre conditions - oclSE 
			//update objectID
			((oclSE)tmpShape.getObject()).setName(tmpShape.textField.getObjectID());
			tmpSE = new oclSE(((oclSE)tmpShape.getObject()).getSort(), ((oclSE)tmpShape.getObject()).getName());
			ListIterator l = tmpShape.textField.getPurePredicateList().listIterator();
			while (l.hasNext()) {
			    oclPredicate oclpd = (oclPredicate)l.next();
			    tmpSE.addPredicate(oclpd);
			    curDomain.addSignatureArgument(OPName, oclpd); //take the arguments for the operator
			}
			tmpOP.addPreSE(tmpSE);
			break;
			
		    case 2: //save index - oclSC
			//update objectID
			((oclSC)tmpShape.getObject()).setName(tmpShape.textField.getObjectID());
			/* to add preclause*/
			tmpSC = new oclSC(((oclSC)tmpShape.getObject()).getSort(), ((oclSC)tmpShape.getObject()).getName());
			l = tmpShape.textField.getPurePredicateList().listIterator();
			while (l.hasNext()) {
			    oclPredicate oclpd = (oclPredicate)l.next();
			    tmpSC.addPre(oclpd);
			    curDomain.addSignatureArgument(OPName, oclpd); //take the arguments for the operator
			}
			
			/* to add PostClause*/
			tmpLink =(vLink)vs.getOutLinks().get("outlinks"+k);
			tmpShape = tmpLink.getStopShape();
			l = tmpShape.textField.getPurePredicateList().listIterator();
			while (l.hasNext()) {
			    oclPredicate oclpd = (oclPredicate)l.next();
			    tmpSC.addPost(oclpd);
			    curDomain.addSignatureArgument(OPName, oclpd); //take the arguments for the operator
			}	
			tmpOP.addIndexSC(tmpSC);
			k++;
			break;
			
		    case 4: //save statics - oclPredicate
			l = tmpShape.textField.getPurePredicateList().listIterator();
			while (l.hasNext()) {
			    oclPredicate oclpd = (oclPredicate)l.next();
			    tmpOP.addStatic(oclpd);
			    curDomain.addSignatureArgument(OPName, oclpd); //take the arguments for the operator
			}
			break;
		    }
		}
		else if (tmpShape.getShapeID() > 1)
		    k++;
		
		//to hold the information of the decomposition and temporal constraints
	    }
	}catch (Exception e){
	    JOptionPane.showMessageDialog(theParentFrame.getParentHT().top,"Creation of the current compound operator failed.\n"
					  + "Please check the pre-condition and post-condition exist in pair.",
					  "GIPO Error",JOptionPane.ERROR_MESSAGE,null);
	    return null;
	}

	/* WZ 19/3/02 */
	// add variables to the opname
	curDomain.addSignatureArgument(OPName, (oclPredicate)omdDEC.getName());

	tmpOP.setName(OPName);

	return tmpOP;
    }

} // VarEditCanvas
