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

/**
 * HighLevelTransitionCanvas.java
 *
 *
 * Created: Tue Nov  6 10:29:08 2001
 *
 * @author W Zhao
 * @version
 */

import javax.swing.*;
import java.awt.*;
import java.util.List;
import java.util.Vector;
import java.util.Hashtable;
import java.util.ListIterator;
import javax.swing.plaf.basic.BasicComboPopup;
import java.util.ArrayList;
import java.awt.event.*;    /* Weihong added on 6/11/2001 */

import jplan.graphics.JGraphCanvas;
import jplan.graphics.gTool.Graphics.vShape;
import jplan.graphics.gTool.Graphics.vLink;
import jplan.graphics.gTool.Windows.vShapePropertyWindow;
import jplan.ocl.*;
import jplan.general.*;


/**
 * Extended from JGraphCanvas with all the drawing/editing functions inherited,
 * this TransitionCanvas has added a few more functions specifically for dealing with
 * high level (compound) transitions.<br>
 * Different part of the compound transitions is attached to different shapes
 * shown on the canvas.
 * When method's head graphics is created, it is allowed to edit the name/label by mouse
 * double clicking to bring up a property window; for other graphics like prevail
 * condition, mouse double clicking is disabled as they are not allowed to edit
 * freely, instead, a right click will allow the static precidates to be added
 * from the popup list.<br>
 * When deleting states in both sides of the transition will be deleted in pairs.
 */
public class HighLevelTransitionCanvas extends JGraphCanvas {
    private HighLevelTransitionWindow parent = null; 

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
    private String buffLabel = null;
    private String staticLabel = null;
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

    private boolean decomposing; //to indicate the state of decomposing oclmethod
    /* Weihong added on 12/11/2001 */
    /**
     *   popup menu for the method head
     */
    protected JPopupMenu menuMethodHead; 
    /* Weihong added on 12/11/2001 */
    /**
     *   popup menu item for the method head
     */
    private JMenuItem decomp;
    /* Weihong added on 12/11/2001 */
    /**
     *   to store the position of the method id before the decomposition
     */
    private Point position_methodID;
    /* Weihong added on 15/11/2001 */
    /**
     *   to store temporary static conditions selected from the static predicates list.
     */
    private List tmpStatic;

    private GipoTab tabParent;    /* Weihong added on 26/11/2001 */
    
    private String md_description = new String(""); /* WZ 21/8/02 */

    /**
     * Create a canvas allowing drawing and editing different shapes
     * to represent method head/id, prevail condition, necessary condition, conditional change
     * @param parent the parent HighLevelTransitionWindow
     */
    public HighLevelTransitionCanvas(HighLevelTransitionWindow parent) {
	super();
	this.parent = parent;
	this.tabParent = parent.getTabParent();   /* Weihong added on 26/11/2001 */
	removeMListener();
	d_Width = 120;
	d_Height = 90;
    }

    /* Weihong 11/3/02 */
    /**
     * Create a canvas allowing drawing and editing different shapes
     * to represent method head/id, prevail condition, necessary condition, conditional change
     * @param curDomain the parent HighLevelTransitionWindow
     */
    public HighLevelTransitionCanvas(oclDomain curDomain) {
	super();
	workingDomain = curDomain;
	removeMListener();
	d_Width = 150;
	d_Height = 90;
    }

    /* WZ 21/8/02 */
    /**
       * Get the value of md_description.
       * @return Value of md_description.
       */
    public String getMd_description() {return md_description;}

    /* WZ 21/8/02 */
    /**
       * Set the value of md_description.
       * @param v  Value to assign to md_description.
       */
    public void setMd_description(String  v) {this.md_description = v;}
    
    /* Weihong added on 4/12/2001 */
    /**
     * Get the value of OPName.
     * @return Value of OPName.
     */
    public vShape getMethodHead() {return OPName;}

    /* Weihong added on 15/11/2001 */
    /**
     * Get the value of tmpStatic.
     * @return Value of tmpStatic.
     */
    public List getTmpStatic() {return tmpStatic;}

    /* Weihong added on 15/11/2001 */
    /**
     * Set the value of tmpStatic.
     * @param v  Value to assign to tmpStatic.
     */
    public void setTmpStatic(List  v) {this.tmpStatic = v;}

    /* Weihong added on 15/11/2001 */    
    /**
     * Get the value of buffLabel.
     * @return Value of buffLabel.
     */
    public String getBuffLabel() {return buffLabel;}

    /* Weihong added on 15/11/2001 */
    /**
     * Set the value of buffLabel.
     * @param v  Value to assign to buffLabel.
     */
    public void setBuffLabel(String  v) {this.buffLabel = v;}
    
    /**
     * Get the value of staticLabel.
     * @return Value of staticLabel.
     */
    public String getStaticLabel() {return staticLabel;}
    
    /**
     * Set the value of staticLabel.
     * @param v  Value to assign to staticLabel.
     */
    public void setStaticLabel(String  v) {this.staticLabel = v;}
    
    /* Weihong added on 26/11/2001 */
    /**
     * Get the value of parent.
     * @return Value of parent.
     */
    public HighLevelTransitionWindow getParentFrame() {return parent;}    

    /**
     * Returns the default size
     * @return the default size
     */
    public Dimension getDefaultSize(){
	return (new Dimension((int)d_Width, (int)d_Height));
    }

    private void animatedZoom(vShape vshape, boolean zoomIN, double animatedRate, jplan.graphics.gTool.Graphics.Double_Dimension maxDimension){
	double cx, cy; //center point of canvas
	double px, py; //center point of vShape
	vShape cp = (vShape)vshape.clone(); //to store the original position and size of the shape
	double newcx, newcy; //dynamic center point of vShape
	cx = maxDimension.width/2;
	cy = maxDimension.height/2;
	px = vshape.px + vshape.width/2;
	py = vshape.py + vshape.height/2;

	if (zoomIN){
	    for (int i = 1; i<animatedRate+1; i++){
		//find new width, height
		vshape.setSize(cp.width + (maxDimension.width - cp.width)*i/animatedRate, cp.height + (maxDimension.height - cp.height)*i/animatedRate);
		//find new center point of vShape
		newcx = px + (cx-px)*i/animatedRate;
		newcy = py + (cy-py)*i/animatedRate;
		//set the position
		vshape.setPosition(newcx-vshape.width/2, newcy - vshape.height/2);
		
		paintIt(getGraphics());
		try{
		    java.lang.Thread.sleep(50);
		}catch (InterruptedException e){}
	    }
	}
	else {
	    for (int i = (int)animatedRate-1; i>=0; i--){
		//find new width, height
		vshape.setSize(d_Width + (maxDimension.width - d_Width)
			       *i/animatedRate, d_Height +
			       (maxDimension.height - d_Height)*i/animatedRate);
		//find new center point of vShape
		newcx = position_methodID.x+ (cx-position_methodID.x)*i/animatedRate;
		newcy = position_methodID.y+ (cy-position_methodID.y)*i/animatedRate;
		//set the position
		vshape.setPosition(newcx-vshape.width/2, newcy - vshape.height/2);
		
		paintIt(getGraphics());
		try{
		    java.lang.Thread.sleep(50);
		}catch (InterruptedException e){}
	    }
	}
    }

    private void animatedOpenWindow(vShape parent, JPanel jpanel,
				    boolean open, double animatedRate){
	Rectangle maxBound = jpanel.getBounds();
	double x;
	double w;
	if (open){
	    jpanel.setVisible(true);
	    for (int i = 0; i<animatedRate+1; i++){
		x = maxBound.x + 0.5*maxBound.width*(1-(double)(i/animatedRate));
		w = maxBound.width*i/animatedRate;
		jpanel.setBounds((int)x, maxBound.y, (int)w, maxBound.height);
		parent.update(getGraphics());
		try{
		    java.lang.Thread.sleep(50);
		}catch (InterruptedException e){}
	    }
	}
	else {
	    double h;
	    for (int i = (int)animatedRate; i>0; i--){
		h = maxBound.height*i/animatedRate;
		jpanel.setBounds(maxBound.x, maxBound.y, maxBound.width, (int)h);
		parent.update(getGraphics());
		try{
		    java.lang.Thread.sleep(50);
		}catch (InterruptedException e){}
	    }
	    jpanel.setVisible(false);
	}
    }

    /* Weihong changed on 12/11/2001 */
    /**
     * To enlarge the vShape (method head) to show/edit the decomposition
     * of the method if dcomp is true; reverse action to the above if dcomp is false.
     * @param windowSize the container's size which the vShape will be growing and fit to.
     * @param dcomp true if to decompose, false if back from decomposition
     */
    public void toDecompose(jplan.graphics.gTool.Graphics.Double_Dimension
			    windowSize, boolean dcomp){
	parent.resetViewport(); //reset the focus to the top left corner of the view port.
	boolean widthDone = false;
	boolean heightDone = false;
	decomposing = dcomp;
	vShape vs = null;
	vs = (vShape)getVShape(5)[0];
	if (dcomp){
	    position_methodID = new Point((int)vs.getPosition().getX(),
					  (int)vs.getPosition().getY());
	    removeMouseMotionListener(this);
	}
	else {
	    /* Weihong changed on 14/11/2001 */
	    animatedOpenWindow(vs, vs.getDecompCanvas(), false, 3); 
	}

	animatedZoom(vs, dcomp, 3, windowSize);
	if (!dcomp) {
	    vs.setPosition(position_methodID.x, position_methodID.y);
	    addMouseMotionListener(this);
	}
	else{
	    /* Weihong changed on 13/11/2001 */
	    animatedOpenWindow(vs, vs.getDecompCanvas(), true, 3);
	}
	
	repaint();
    }

    /**
     * This method is invoked by Swing to draw components. 
     * Applications should not invoke paint directly, but should instead use the repaint 
     * method to schedule the component for redrawing.<br>
     * This method actually delegates the work of painting to three protected methods: 
     * paintComponent, paintBorder, and paintChildren. They're called in the order 
     * listed to ensure that children appear on top of component itself.
     * Generally speaking, the component and its children should not paint
     * in the insets area allocated to the border.
     * Subclasses can just override this method, as always. A subclass that just
     * wants to specialize the UI (look and feel) delegate's paint method should
     * just override paintComponent.
     * @overrides paint JComponent 
     */
    public void paint(java.awt.Graphics g){
	if (decomposing) {
	    paintIt(g);
	    paintChildren(g);
	}
	else
	    super.paint(g);
    }

    /* Weihong added on 9/11/2001 */
    /**
     * Draw all the shapes which are already registered with the vShapeList. 
     * In the edit mode do not paint children. <br>
     * Draw all the links which are already registered with the linkList. <br>
     * To paint the drag selecting rectangle. <br>
     * @param g Graphics of the canvas
     */
    private synchronized void paintIt (java.awt.Graphics g) {
	g.setColor(java.awt.Color.black);
	g.fillRect(0,0, getWidth(), getHeight());
	vShape vs = null;
	vs = (vShape)getVShape(5)[0];	
	vs.setBounds(0,0, getWidth(), getHeight());
	vs.getDecompCanvas().setBounds((int)vs.width/6, (int)vs.height/6,
				       (int)vs.width*2/3, (int)vs.height*2/3);	
	vs.update(g); //do not paint children
    }

    /**
     * to pass the domain
     * @param cur domain
     * 
     */
    public void setWorkingDomain(oclDomain cur){
	workingDomain = cur;
    }

    /* Weihong added on 26/11/2001 */
    /**
     * return the domain
     * @return the ocl domain
     */
    public oclDomain getWorkingDomain(){
	return workingDomain;
    }

    /**
     * when mouse clicked; override its super
     * @param me MouseEvent
     * 
     */
    public void mouseClicked (MouseEvent me) {
	if (me.getClickCount() == 2 
	    && me.getModifiers() == MouseEvent.BUTTON1_MASK) {
	    tempShape = (vShape)getVShape(5)[0];
	    if (tempShape.contains((double)me.getX(),(double)me.getY())) {
		renameShape(tempShape);
	    }
	}
	
	if(me.getModifiers() == MouseEvent.BUTTON3_MASK) {
	    for (int i = 1; i < vShapeList.size()+1; i++) {
		tempShape = getShape(i);
		/* Weihong changed on 3/12/2001 */
		if (tempShape.contains((double)me.getX(),(double)me.getY())){
		    if (tempShape.getShapeID() == 4){
			initPopupMenu(); 
			popupMenu.show(this, me.getX(), me.getY());
		    }
		    if (tempShape.getShapeID() == 5){
			menuMethodHead.show(this, me.getX(), me.getY());
		    }
		    return;
		}
		/* end Weihong changed on 3/12/2001 */
	    }
	}
    }

    /* WZ 14/5/02 */
    /**
     * to rename the method head shape
     * 
     */
    private void renameShape(vShape tempShape){
	String name = GipoInputBox.showIdentifierInputBox(parent.top,
				   "Compound Operator Name",
				   "Please rename the Compound operator.",
				   tempShape.getLabel());
	if (name != null) {
	    /* Weihong added on 4/12/2001 */
	    if (((MethodHeadCanvas)tempShape.getDecompCanvas()).isFirstLevel()){
		parent.getTabParent().setTitleAt(parent.getTabParent().getSelectedIndex(), name);
		parent.getTabParent().refreshTitleParent(tempShape.getLabel(), name);
	    }
	    /* end Weihong added on 4/12/2001 */
	    tempShape.setLabel(name);
	    repaint();
	}
    }

    /* WZ 14/5/02 */
    /**
     * to rename the method head shape
     * 
     */
    private void showDescription(){
	vShape tempShape = (vShape)getVShape(5)[0];
	//other input box should be used.
	String desc = JOptionPane.showInputDialog(parent.top,
		    "Please type a description for the current editing method",
		    md_description);
	if (desc != null) {
	    md_description = desc;/* WZ 21/8/02 */
	}
    }

    /**
     * deleteShape in pair
     * 
     */
    public void deleteShape(){
	//get all highlighted shapes
	vShape tempShape = null;
	List list = new ArrayList();
	for (int i = 1; i < vShapeList.size()+1; i++) {
	    tempShape = (vShape)getShape(i);
	    if (tempShape.getSelected()) {
		//check if this required deleting shape is the ActionID (head of the operator)
		if (tempShape.getShapeID() == 5) 
		    tempShape.setSelected(false);
		else
		    list.add(tempShape);
	    }
	}
	//find another pair
	ListIterator li = list.listIterator();
	while (li.hasNext()){
	    vShape vsp = findPairShape((vShape)li.next());
	    if (vsp != null)
		vsp.setSelected(true);
	}

	
	//delete all shapes
	super.deleteShape();
    }

    /**
     * find another shape in pair
     * @param vs the first vShape
     */
    private vShape findPairShape(vShape vs){
	//left hand side shapes were clicked
	if (vs.getInLinks().size() == 0) {
	    vLink vpLink = (vLink)vs.getOutLinks().get("outlinks"+1); //it has only 1 outlink
	    vLink tmpLink = null;
	    for (int i=1; i < OPName.getOutLinks().size()+1; i++) {
		tmpLink =(vLink)OPName.getOutLinks().get("outlinks"+i);
		if (vpLink.getType() == tmpLink.getType()){
		    return (vShape)tmpLink.getStopShape();
		}
	    }
	}
	//right hand side shapes were clicked
	if (vs.getOutLinks().size() == 0) {
	    vLink vpLink = (vLink)vs.getInLinks().get("inlinks"+1); //it has only 1 outlink
	    vLink tmpLink = null;
	    for (int i=1; i < OPName.getInLinks().size()+1; i++) {
		tmpLink =(vLink)OPName.getInLinks().get("inlinks"+i);
		if (vpLink.getType() == tmpLink.getType()){
		    return (vShape)tmpLink.getStartShape();
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
	if (!decomposing){    /* Weihong added on 13/11/2001 */
	    mouseDownPoint = new Point(me.getPoint());
	    for (int i = 1; i < vShapeList.size()+1; i++) {
		tempShape = (vShape)getShape(i);
		tempShape.getOffset(mouseDownPoint); //calculate the offset value
	    }
	    
	    dragSelect = false;
	    
	    //to create and draw a new vShape
	    if (mouseAction == CREATE_SHAPE){
		if (buffLabel != null) { /* WZ 2/5/02 */
		    if (shapeTypeID == 1 || shapeTypeID == 2) { /* Weihong changed on 3/12/2001 */
			String objID = GipoInputBox.showVarableInputBox(parent.top,
									"Object ID",
									"Please Input a new object ID.");
			if (objID != null){
			    objectID = objID;
			    createOPshapes(me);
			}
		    }
		    else {
			createOPshapes(me);/* Weihong changed on 3/12/2001 */
		    }
		}
		else
		    JOptionPane.showMessageDialog(this ,
					      "No value for the state.",
					      "GIPO Error",
					      JOptionPane.WARNING_MESSAGE,null);   
	    }

	    if(mouseAction == CREATE_LINK) {
		createVLink(me);
	    }
	    
	    //to select a (or more) shape(s)
	    if (mouseAction == SELECT) {
		//check shapelist to see if there at least one shape was highlighted.
		for (int i = 1; i < vShapeList.size()+1; i++) {
		    tempShape = (vShape)getShape(i);
		    if (tempShape.getSelected() && me.getModifiers() == MouseEvent.BUTTON1_MASK) {//if there is at least one shape was preselected...
			if (tempShape.contains((double)me.getX(),(double)me.getY())) {
			    //do not de-select the shapes since this is a temp to drag Shapes;
			    return;
			}
		    }
		}
		
		selectSingleShape((double)me.getX(),(double)me.getY());
		selectSingleLink((double)me.getX(),(double)me.getY());
	    }
	    repaint();
	}
    }

    /**
     * select a single vshape which contains the current point
     * @param x double value at X axis - the position of mouse
     * @param y double value at Y axis - the position of mouse
     * @return vShape
     */
    protected vShape selectSingleShape(double x, double y) {
	vShape vsp = null;
	for (int i = 1; i < vShapeList.size()+1; i++) {
	    tempShape = (vShape)getShape(i);
	    if (tempShape.contains(x,y)) {
		tempShape.setSelected(true);

		if (tempShape.getShapeID() != 5)
		    /* for the popup menu to perform to assign a predicate 
		       to the name shape of an operator */
		    popupShape = tempShape; 
		vsp = tempShape;
	    }
	    else tempShape.setSelected(false);
	}
	return vsp;
    }

    /* WZ 27/3/02 */
    /**
     * add Pane Change Listener
     * @param tPane TransExpressionDocPane
     * 
     */
    public void addToMonitor(TransExpressionDocPane tPane){
	tPane.addChangeListener(new ExpressionPaneListener() {
	    public void getChange(ExpressionPaneEvent evt) {
		if (evt.getID() == ExpressionPaneEvent.RENAME) {
		    if (evt.getScope() == ExpressionPaneEvent.GLOBAL){
			String vName = evt.getVName();
			String oldVName = evt.getOldVName();
			//update representing method
// 			curOM.replaceVariableName(oldVName, vName);

			//update OPName
			oclPredicate curOpd = (oclPredicate)OPName.getObject();
			try {
			    curOpd.replaceVariableNameByName(oldVName, vName);
			}catch (Exception e){Utility.debugPrintln(e);}

			/* WZ 28/3/02 */
			updateMDHeadCanvas();
 
// 			//print it out to debug
// 			java.io.StringWriter opDetail = new java.io.StringWriter();
// 			curOM.oclPrint(new java.io.PrintWriter (opDetail) ,0,false);
// 			Utility.debugPrintln("******\n"+new String(opDetail.getBuffer())+"******\n");

			/* WZ 28/3/02 end */

		    }
		}
	    }
	});
    }

    /* WZ 4/4/02 */
    /**
     * to update methodHeadCanvas when the oclMethod has been changed elsewhere
     * 
     */
    private void updateMDHeadCanvas(){
	oclMethod curOM = (oclMethod)workingDomain.checkObjectType((oclPredicate)OPName.getObject());
	if (curOM == null){
	    Utility.debugPrintln("No oclMethod found matching the head shape ...");
	    return;
	}
	//update decompostions
	boolean isVisible = OPName.getDecompCanvas().isVisible();
	OPName.removeDecompCanvas();
	MethodHeadCanvas curMDcanvas = new MethodHeadCanvas(false, OPName, parent);
	OPName.addDecompCanvas(curMDcanvas); 
	/* WZ 4/4/02 */
	MethodHeadCanvas.setDecomposition(curMDcanvas, curMDcanvas.getWorkingDomain(), curOM);
	curMDcanvas.setVisible(isVisible);
	repaint();
	//remove all other opened tabbed panes
	parent.getTabParent().rmExtraTabs();
	//update the variable editing window
	curMDcanvas.getVarEditWindow().refreshCanvas();
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
	/* WZ 29/8/02 */	
	//tempShape.textField.setHierFlag(parent.top.hierarchicalSwitch);
	// Ron 5/5/03
	tempShape.textField.setHierFlag(workingDomain.isHierarchical());
	tempShape.textField.setObjectID(objectID);	
	tempShape.textField.setSort(tmpSort);
	/* end 29/8/02 */	   		
	tempShape.textField.setCurDomain(workingDomain);
	addToMonitor(tempShape.textField);    /* WZ 27/3/02 to monitor changes in the pane */
	tempShape.setDrawLabel(false);
	Thread runner = new Thread() {
	    public void run() {
		setMouseAction(0); //disable the click on the canvas
// 		while (buffLabel.equals(checkLabel) || !parent.getKeyedAction()) {
		while (buffLabel.equals(checkLabel)) {
		    if (vShapeList.contains("shapeKey"+tempShape.getID())){
// 			parent.resetStateList();
			return;
		    }
		    tempShape.setSelected(!tempShape.getSelected()); //flashing to get attention
		    repaint();
		    try {
			sleep(100);
		    } catch(Exception e) {	}
		    //wait until the mouse clicked the states list
		}
		tempShape.setLabel(buffLabel);
		replaceVariableNameFromAList(predicateList,curObjectID, objectID);
		tempShape.textField.setObjectID(objectID);
		/* WZ 29/8/02 */
		tempShape.textField.setSort(tmpSort);	 	
		//tempShape.textField.setHierFlag(parent.top.hierarchicalSwitch);
		tempShape.textField.setHierFlag(workingDomain.isHierarchical());
		tempShape.textField.setCurDomain(workingDomain);		
		/* end 29/8/02 */
		tempShape.textField.setTransElement(TransExpressionDocPane.RHS); 
		addToMonitor(tempShape.textField);    /* WZ 27/3/02 to monitor changes in the pane */
		sc.setPost(predicateList);
		tempShape.setObject(sc);
		assignText(tempShape);
		tempShape.setSelected(false);
		repaint();
		tempShape = null;
		resetMouseAction();
		//for undo
		recordState(); 
		parent.resetStateList();
		parent.enableSortTree(true);/* WZ 2/5/02 */
	    }
	};
	    
	switch (tempShape.getShapeID()) {
	case 5: //compound operator's name - predicate
	    if (buffLabel != null) {
		tempShape.setLabel(buffLabel);
		createActionID();
	    }
	    else 
		JOptionPane.showMessageDialog(this ,
					      "No value for the state.",
					      "GIPO Warning",
					      JOptionPane.WARNING_MESSAGE,null);   
	    break;
	case 1: //pre-conditions - oclSE
	    if (buffLabel != null) {
		tempShape.setLabel(buffLabel);
		se = new jplan.ocl.oclSE(tmpSort, objectID);
		replaceVariableNameFromAList(predicateList,curObjectID, objectID);
		//tempShape.textField.setObjectID(objectID);
		tempShape.textField.setTransElement(TransExpressionDocPane.PREV);
		se.setPredicateList(predicateList);
		tempShape.setObject(se);
		assignText(tempShape);
		
		//set the relationship
		setMouseAction(CREATE_LINK);
		setDrawingLinkID(calculateLinkTypeID());
		createVLink(tempShape, OPName);
		resetMouseAction();
		//for undo
		recordState(); 
		tempShape = null;
	    }
	    else 
		JOptionPane.showMessageDialog(this ,
					      "No value for the state.",
					      "GIPO Warning",
					      JOptionPane.WARNING_MESSAGE,null);   
	    break;
	case 2: //index - oclSC
	    if (buffLabel != null) {
		tempShape.setLabel(buffLabel);
		sc = new jplan.ocl.oclSC(tmpSort, objectID);
		replaceVariableNameFromAList(predicateList,curObjectID, objectID);
		//tempShape.textField.setObjectID(objectID);
		tempShape.textField.setTransElement(TransExpressionDocPane.LHS);
		sc.setPre(predicateList);
		tempShape.setObject(sc);
		assignText(tempShape);
		
		setMouseAction(CREATE_LINK);
		setDrawingLinkID(calculateLinkTypeID());
		createVLink(tempShape, OPName);
		setMouseAction(CREATE_SHAPE);
		tempShape = createVShape(me.getX()+300, me.getY());
		tempShape.setDrawLabel(false);
		setMouseAction(CREATE_LINK);
		createVLink(OPName, tempShape);
		setMouseAction(CREATE_SHAPE);
		repaint();
		buffLabel = "";
		checkLabel = buffLabel;
		parent.resetStateList();/* WZ 2/5/02 */
		parent.enableSortTree(false);/* WZ 2/5/02 */
		runner.start();
	    }
	    else 
		JOptionPane.showMessageDialog(this ,
					      "No value for the state.",
					      "GIPO Warning",
					      JOptionPane.WARNING_MESSAGE,null);   
	    break;
	case 4: //statics - oclPredciate list
	    if (staticLabel != null) {
		tempShape.setLabel(buffLabel);
// 		replaceVariableNameFromAList(predicateList,curObjectID, objectID);
// 		tempShape.textField.setObjectID(objectID);
		tempShape.textField.setTransElement(TransExpressionDocPane.PREV);
		tempShape.textField.setHierFlag(false);/* WZ 29/8/02 to disable the delete by level popupmenu */
		tempShape.setObject(predicateList);
		assignText(tempShape, tmpStatic);
		
		//set the relationship
		setMouseAction(CREATE_LINK);
		setDrawingLinkID(calculateLinkTypeID());
		createVLink(tempShape, OPName);
		resetMouseAction();
		//for undo
		recordState(); 
		tempShape = null;
	    }
	    else 
		JOptionPane.showMessageDialog(this ,
					  "No static predicates has been selected.",
					  "GIPO Warning",
					  JOptionPane.WARNING_MESSAGE,null);   
	}
    }

    /**
     * replaceVariableNameFromAList for all the predicate
     * replace with a new variable value
     * not in an element throw exception
     * @param listPred the list of predicates to be operated
     * @param before the varible
     * @param after new element name
     */
    private void replaceVariableNameFromAList(List listPred, String before, String after){
	ListIterator li = listPred.listIterator();
	while(li.hasNext()) {
	    oclPredicate oprd = (oclPredicate)li.next();
	    try {
		oprd.replaceVariableNameByName(before, after);
	    } catch (Exception e){Utility.debugPrintln(e);}
	}
    }

    /**
     * reset mouse mode to default: "selection"
     * 
     */
    private void resetMouseAction(){
	mouseAction = SELECT;
	parent.setSelectionButton(true);    /* Weihong added on 15/11/2001 */
    }

    /**
     * put the current edited predicates to the vshape
     * @param theShape the vShape which textField needs to be updated
     * 
     */
    private void assignText(vShape theShape) {
	ListIterator li = predicateList.listIterator();
	while (li.hasNext()) {
	    try {
		theShape.textField.addPredicate((oclPredicate)li.next());
	    } catch (Exception e) {
		Utility.debugPrintln("Failed to insert predicate " + e.toString());
	    }
	}
    }
  
    /**
     * put the current edited predicates to the vshape
     * @param theShape the vShape which textField needs to be updated
     * @param predList list of predicates
     * 
     */
    private void assignText(vShape theShape, List predList) {
	ListIterator li = predList.listIterator();
	while (li.hasNext()) {
	    try {
		theShape.textField.addPredicate((oclPredicate)li.next());
	    } catch (Exception e) {
		Utility.debugPrintln("Failed to insert predicate " + e.toString());
	    }
	}
    }

    /**
     * loop within 6 different link types to pick a different link type
     * @return int linktype
     */  
    public int calculateLinkTypeID(){
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
    public void setOPName(vShape theShape){
	OPName = theShape; //set center shape
    }
      
    /**
     * create graphical representation of the operator's name/head
     * 
     */  
    public void createActionID(){
	setDrawingShapeID(5);/* Weihong added on 15/11/2001 */
	vShape tempShape = createVShape(200, 200);
	tempShape.setObject(new oclPredicate(tempShape.getLabel()));
	OPName = tempShape; //set center shape
	tempShape.removeTextField();
	// to add a decompCanvas(JGraphCanvas) to the vShape
	tempShape.addDecompCanvas(new MethodHeadCanvas(false, tempShape, parent)); /* Weihong added on 26/11/2001 */
	tempShape = null;
	initPopupMenuForMethodHead();    /* Weihong added on 12/11/2001 */
    }
      
    /* Weihong added on 12/11/2001 */
    /**
     * initialise the popup menu for the method's head (circle)
     * 
     */ 
    public void initPopupMenuForMethodHead() {
	menuMethodHead = new JPopupMenu();
	decomp = new JMenuItem("Show decomposition");
	decomp.addActionListener(new java.awt.event.ActionListener() {
	    public void actionPerformed(ActionEvent e) {
		if (decomp.getText() == "Show decomposition"){
		    toDecompose(parent.getGraphicsWindowSize(), true);   
		    /* Weihong added on 28/11/2001 */
		    if (parent.getTabParent().getSelectedIndex() == 0) //if the decomposition happens at the main oclMethod
			parent.setEditModeButtonEnabled(false);
		    /* end Weihong added on 28/11/2001 */
		    if (((MethodHeadCanvas)OPName.getDecompCanvas()).isFirstLevel())
			parent.showOperatorsList(true); /* Weihong added on 30/11/2001 */
		    decomp.setText("Hide decomposition");
		}
		else if (decomp.getText() == "Hide decomposition"){
		    toDecompose(parent.getGraphicsWindowSize(), false);
		    /* Weihong added on 28/11/2001 */
		    if (parent.getTabParent().getSelectedIndex() == 0) //if the decomposition happens at the main oclMethod
			parent.setEditModeButtonEnabled(true);
		    /* end Weihong added on 28/11/2001 */
		    if (((MethodHeadCanvas)OPName.getDecompCanvas()).isFirstLevel())
			parent.showOperatorsList(false); /* Weihong added on 30/11/2001 */
		    decomp.setText("Show decomposition");
		}
	    }
	});
	menuMethodHead.add(decomp);

	// rename menu
	JMenuItem mRename = new JMenuItem("Rename");
	mRename.addActionListener(new java.awt.event.ActionListener() {
	    public void actionPerformed(ActionEvent e) {
		vShape tempShape = (vShape)getVShape(5)[0];
		renameShape(tempShape);
	    }
	});
	menuMethodHead.add(mRename);

	// rename menu
	JMenuItem mDescription = new JMenuItem("Description");
	mDescription.addActionListener(new java.awt.event.ActionListener() {
	    public void actionPerformed(ActionEvent e) {
		showDescription();
	    }
	});
	menuMethodHead.add(mDescription);
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
	    opde = (oclPredicate)liPreds.next();
	    if (opde.isStatic())
		items.addElement(opde);
	}
	combo = new JComboBox(items);
	combo.addActionListener (new java.awt.event.ActionListener () {
	    public void actionPerformed (java.awt.event.ActionEvent evt) {
		try {
		    // Ron 29/8/01 added clone otherwise this is the 
		    // predicate in the predicate list
		    oclPredicate org = (oclPredicate)(popupMenu.getList().getSelectedValue());
		    oclPredicate opt = (oclPredicate)org.clone();
		    opt.toVar();
		    popupShape.textField.addPredicate(opt);
		    if (popupShape.getShapeID() == 1) //oclSE
			((oclSE)popupShape.getObject()).addPredicate(opt);
		    else if (popupShape.getShapeID() == 2|| popupShape.getShapeID() == 3) {//oclSC
			if (popupShape.getInLinks().size() == 0) //left hand side
			    ((oclSC)popupShape.getObject()).addPre(opt);
			else if (popupShape.getOutLinks().size() == 0) //right hand side
			    ((oclSC)popupShape.getObject()).addPost(opt);
		    }
		    if (popupShape.getShapeID() == 4) //oclSE
			((List)popupShape.getObject()).add(opt);

		    popupMenu.hide(); /* WZ 21/8/02 */
		} catch (Exception e) {
		    Utility.debugPrintln("Failed to insert predicate " + e.toString());
		}
		repaint();
		popupShape = null;
	    }
	}); 
	popupMenu = new BasicComboPopup(combo); 
    }
    
   
} // HighLevelTransitionCanvas
