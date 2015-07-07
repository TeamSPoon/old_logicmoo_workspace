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

package jplan.graphics;

/** JGraphCanvas.java
 * 23/1/01 - Chinese New Year Eve
 * @author Weihong Zhao
*/


import java.awt.event.*;
import java.awt.*;
import java.awt.image.*;
import java.util.*;
import javax.swing.undo.*;
import javax.swing.event.*;
import javax.swing.*;
import java.awt.print.*;
import java.io.BufferedReader;

import jplan.graphics.gTool.Graphics.vShape;
import jplan.graphics.gTool.Graphics.vLink;
import jplan.graphics.gTool.Graphics.LinkPoint;
import jplan.graphics.gTool.Windows.vShapePropertyWindow;
import jplan.general.EditPaneGroup;
import jplan.general.GipoInputBox; /* Weihong changed/added on 3/9/2001 */
import jplan.general.Utility;


/**
 * Extended from JPanel, JGraphCanvas is a drawing canvas, where vShape and vLink can be drawn.<br>
 * In the normal mode,  basic graphical editing functions are supported here, such as mouse click to select vShape and vLink, mouse drag for mutiple selection, mouse drag to move vShape and vLink, and mouse drag to reshape the vShapes.<br>
 * vShape and vLink can be deleted as well as easy click creation.
 * other functions like undo/redo, zoom in/out, print/print preview, save graphics are also enabled.
 * In the edit mode, individual vShape will listen the mouse event for editing the text in its JTextPane - textField.
 */
public class JGraphCanvas extends JPanel implements MouseListener, MouseMotionListener, Printable {
    /**
     * Mouse mode
     */
    public static final int CREATE_SHAPE = 1;
    /**
     * Mouse mode
     */
    public static final int CREATE_LINK = 2;
    /**
     * Mouse mode
     */
    public static final int SELECT = 3;
    /**
     * Mouse mode
     */
    public static final int DELETE = 4;
    /**
     * Flag to show there are (not) some contents in the drawing/editing canvas.
     */
    public boolean isDirty; /* Weihong changed/added on 2/10/2001 */

    //for dealing with vShapes
    /**
     * A hashtable to store the registered vShapes which has been drawn on the canvas.
     */
    public Hashtable vShapeList = new Hashtable();
    /**
     * Flag of selected/deselected
     */
    private int selected = 0;
    /**
     * vShape ID - for the shape identification
     */
    protected int shapeID = 0;    /* Weihong added on 20/11/2001 */
    /**
     * Mouse mode
     */
    protected int mouseAction = 0; // doing nothing
    /**
     * vShape type. Check the static finals in the vShape.
     */
    protected int shapeTypeID = 0;     /* Weihong added on 20/11/2001 */
    /**
     * Cumulated integer to mark the vShapes registration using as "key" in the hashtable.
     */
    protected int shapeIndex = 0; //for the vShapeList registration
    /**
     * Temporary vShape;
     */
    protected vShape vs; 
    /**
     * Default vShape which is currently selected/edited.
     */
    protected vShape tempShape; 
    /**
     *  Detecting selection area
     */
    private Shape selectedArea; 
    /**
     * Mouse motion.
     */
    protected boolean dragged;
    /**
     * Mouse motion.
     */
    protected boolean dragSelect;
    /**
     * The orginal position when the mouse touch down to the canvas before dragging.
     */
    protected Point mouseDownPoint;
    /**
     * Current mouse position during dragging.
     */
    protected Point mouseCurrentPoint;
    /**
     * current default width and height.
     */
    protected double d_Width, d_Height; 

    //for dealing with vLinks
    /**
     * 
     */
    private vShape tempShapeToLink;
    /**
     * vLink counter.
     */
    private int linkIndex = 0; // ready for the first link to be created.
    /**
     * Default vLink currently selected/edited.
     */
    protected vLink vl; //temperary vLink
    /**
     * Cumulated integer to mark the vLinks registration using as "key" in the hashtable.
     */
    protected Hashtable linkList = new Hashtable(); /* Weihong changed/added on 8/10/2001 */
    /**
     * Link type index
     */
    protected int linkTypeID = 0;
    /**
     * ID of vLink
     */
    private int linkID = 0; //for the link identification

    //for undo/redo
    /**
     * Index of states
     */
    protected int index_Edit = -1;/* WZ 10/6/02 */
    /**
     * Maximum number of undoable states
     */
    private int maxSize_undoStates = 100;/* WZ 11/6/02 */
    /**
     * State of the recorded moment for undo/redo.
     * It contains the information of vShapes, vLinks.
     */
    private vState[] states = new vState[maxSize_undoStates];
    /**
     * Maximum number of vShapes allowed to draw on the canvas.
     */
    private int MaxNoShapes = 1000;
    /**
     * This is the only place to store shapes.
     */
    public vShape[] shape = new vShape[MaxNoShapes]; 

    //for print
    /**
     * The buffered image of the graphics on the drawing canvas using for printing.
     */
    protected BufferedImage bufferedImage;
    /**
     * Maximum number of page for printing.
     */
    private int maxPageNo = 1;
    /**
     * Edit mode to disable the all mouse listeners added to this canvas and enable the mouse listeners for the vShapes which have be registered.
     */
    private boolean editMode = false;
    /**
     * Group for the TransitionDocPane inside the vShapes to fire events to each other.
     */
    private EditPaneGroup editPaneGroup = new EditPaneGroup();
    /**
     * The default color of the canvas.
     */
    private Color normalColor = Color.white;    /* Weihong added on 13/11/2001 */

    protected boolean resize = false;/* WZ 14/5/02 */

    /**
     * To create a canvas allowing drawing and editing different shapes, mouse drag and drop.
     */
    public JGraphCanvas() {
	setDoubleBuffered(true);
	setBackground(Color.white);
	setLayout(null); //the absolute layout
	d_Width = 100;
	d_Height = 60; 
	addMListener();
	isDirty = false; /* Weihong changed/added on 2/10/2001 */
// 	recordState(); //record the current state as initial state
    }

    /* Weihong added on 28/11/2001 */
    /**
     * add mouse listener
     */
    public void addMListener(){
	addMouseListener(this);
	addMouseMotionListener(this);
    }

    /* Weihong added on 28/11/2001 */
    /**
     * remove mouse listener
     */
    public void removeMListener(){
	removeMouseListener(this);
	removeMouseMotionListener(this);
    }

    /* WZ 4/4/02 */
    public double getDefaultWidth(){
	return d_Width;
    }

    /* WZ 4/4/02 */
    public double getDefaultHeight(){
	return d_Height;
    }

    /* WZ 16/4/02 */
    /**
     * return the number of shapes in this canvas
     */
    public int getShapeCount(){
	return vShapeList.size();
    }

    /* WZ 4/4/02 */
    /**
     * return shape with given shapeKey
     * @param shapeKey String registered in vShapeList
     */
    public vShape getVShape(String shapeKey){
	return (vShape)shape[Integer.parseInt(vShapeList.get("shapeKey"+shapeKey).toString())];
    }

    /* WZ 14/4/02 */
    /**
     * return shape with given shapeKey
     * @param shapeKey int registered in vShapeList
     */
    public vShape getShape(int shapeKey){
	String spKey = String.valueOf(shapeKey);
	return (vShape)shape[Integer.parseInt(vShapeList.get("shapeKey"+spKey).toString())];
    }

    /* WZ 7/5/02 */
    /**
     * return shape key with given shapeID
     * shapeID integer registered in vShapeList followed "shapeKey"
     */
    public int getShapeKey(String shapeID){
	for (int i = 1; i < vShapeList.size()+1; i++) {
	    if (shapeID.equals(vShapeList.get("shapeKey"+i).toString()))
		return i;
	}
	return 0;
    }

    /* Weihong added on 28/11/2001 */
    /**
     * remove highlights inside the textRield - the transitionExpressionPane
     */
    public void removeHighlighter(){
	vShape tempShape;
	for (int i = 1; i < vShapeList.size()+1; i++) {
	    tempShape = getShape(i);
	    if (tempShape.getShapeID() != 0 && tempShape.getShapeID() != 5) {
		tempShape.textField.removeHighlights();
	    }
	}
    }

    /* Weihong added on 13/11/2001 */
    /**
     * Sets the default color of the canvas
     * @param color the color to be set
     */
    public void setDefaultColor(Color color) {
	normalColor = color;
    }

    /* Weihong changed/added on 8/11/2001 */
    /**
     * Returns the vShape which has the same shape type as the given vShapeType
     * @param vShapeType vShape Type
     * @return an array of vShapes
     */
    public vShape [] getVShape(int vShapeType) {
	vShape [] returnShape = new vShape[vShapeList.size()];
	vShape tempShape = null;
	int j = 0;
	for (int i = 1; i < vShapeList.size()+1; i++) {
	    tempShape = (vShape)shape[Integer.parseInt
				     (vShapeList.get("shapeKey"+i).toString())];
	    if (tempShape.getShapeID() == vShapeType) {
		returnShape[j] = tempShape;
		j++;
	    }
	}
	return returnShape;
    }

    /* WZ 14/6/02 */
    /**
       * Get the value of d_Width.
       * @return Value of d_Width.
       */
    public double getD_Width() {return d_Width;}

    /* WZ 14/6/02 */
    /**
       * Set the value of d_Width.
       * @param v  Value to assign to d_Width.
       */
    public void setD_Width(double  v) {this.d_Width = v;}

    /* WZ 14/6/02 */
    /**
       * Get the value of d_Height.
       * @return Value of d_Height.
       */
    public double getD_Height() {return d_Height;}

    /* WZ 14/6/02 */
    /**
       * Set the value of d_Height.
       * @param v  Value to assign to d_Height.
       */
    public void setD_Height(double  v) {this.d_Height = v;}
    
    /**
     * Switch on/off the edit mode.
     * @param bl true or false
     * 
     */
    public void setEditMode(boolean bl) {
	editMode = bl;
    }

    /**
     * dobule click to show the property.
     * @param me MouseEvent
     */ 
    public void mouseClicked (MouseEvent me) {
	//double click left mouse
	if (me.getClickCount() == 2 && me.getModifiers() == MouseEvent.BUTTON1_MASK) { 
	    //check shapelist to see if there at least one shape was highlighted.
	    for (int i = 1; i < vShapeList.size()+1; i++) {
		vs = getShape(i);
		if (vs.contains((double)me.getX(),(double)me.getY())) {
		    showRenameWindow(vs);
		    return;
		}
	    }
	}
    }

    /**
     * to show the dialog window for rename of the shape
     * @param vs
     */ 
    protected void showRenameWindow (vShape vs) {
	vShapePropertyWindow pw = new vShapePropertyWindow(this);
	pw.setShape(vs);
	pw.show();
    }

    /**
     * When mouse pressed either to create and draw a new vShape
     * or to select a (or more) shape(s).
     * @param me MouseEvent
     */ 
    public void mousePressed(MouseEvent me) {
	mouseDownPoint = new Point(me.getPoint());
	for (int i = 1; i < vShapeList.size()+1; i++) {
	    vs = getShape(i);
	    vs.getOffset(mouseDownPoint); //calculate the offset value
	}
	
	dragSelect = false;
	
	//to create and draw a new vShape
	if (mouseAction == CREATE_SHAPE) {
	    createVShape(me);	  
	}
	
	if(mouseAction == CREATE_LINK) {
	    createVLink(me);
	}
	
	//to select a (or more) shape(s)
	if (mouseAction == SELECT) {
	    //check shapelist to see if there at least one shape was highlighted.
	    for (int i = 1; i < vShapeList.size()+1; i++) {
		vs = getShape(i);
		if (vs.getSelected()) {//if there is at least one shape was preselected...
		    if (vs.contains((double)me.getX(),(double)me.getY())) {
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

    /**
     * To select the vShape when the given point is inside the vShape's bound.
     * @param x double value at X axis
     * @param y double value at Y axis
     * @return vShape
     */ 
    protected vShape selectSingleShape(double x, double y) {
	vShape vsp = null;
	for (int i = 1; i < vShapeList.size()+1; i++) {
	    vs = getShape(i);
	    if (vs.contains(x,y)) {
		vs.setSelected(true);
		vsp = vs;
	    }
	    else vs.setSelected(false);
	}
	return vsp;
    }
    
    /**
     * To select the vShape when the given point is inside the vShape's bound.
     * @param vshape
     */ 
    protected void selectSingleShape(vShape vshape) {
	vShape vsp = null;
	for (int i = 1; i < vShapeList.size()+1; i++) {
	    vs = getShape(i);
	    if (vs.getID() == vshape.getID()) {
		vs.setSelected(true);
	    }
	    else vs.setSelected(false);
	}
	repaint();
    }

    /**
     * To select the vLink when the given point is contained by this vLink.
     * @param x double value at X axis
     * @param y double value at Y axis
     * @return vLink
     */
    protected vLink selectSingleLink(double x, double y) {
	for (int i = 1; i < linkList.size()+1; i++) {
	    vl = (vLink)linkList.get("linkKey"+i);
	    if (vl.contains(x,y)) {
		vl.setSelected(true);
		return vl;
	    }
	    else vl.setSelected(false);
	}
	return null;
    }
    
    /**
     * When mouse drags, actions could be one of the followings: 
     * vShape reshape, move vShapes/vLinks, drag seletion.
     * @param me MouseEvent
     * 
     */
    public void mouseDragged(MouseEvent me) {
	
	if (mouseAction == SELECT) {
	    int i,k;
	    //check all the shapes
	    for (i = 1; i < vShapeList.size()+1; i++) {
		vs = getShape(i);
		if (vs.getSelected()) {
		    k = vs.getShapeHighlighter().contains((double)me.getX(),(double)me.getY());
		    switch (k) {
		    case 0: //dragging the shape
			if (!resize){ /* WZ 14/5/02 */
			    dragged = true;
			    setCursor(Cursor.getPredefinedCursor(Cursor.MOVE_CURSOR));
			    vs.setPosition((double)(me.getX() - vs.offsetX),(double)(me.getY() - vs.offsetY));
			//vs.setLocation(me.getX()-4 - (int)vs.offsetX, me.getY()-4 - (int)vs.offsetY);
			    repaint();
			    break;
			}
			
			/*resize the shape*/
		    case 2: //west
			if (!dragged){ /* WZ 15/5/02 */
			    resize = true;/* WZ 14/5/02 */
			    setCursor(Cursor.getPredefinedCursor(Cursor.W_RESIZE_CURSOR));
			    vs.setDoubleBounds(Math.min(vs.px+vs.width-5,(double)me.getX()),vs.py ,Math.max(5,vs.px-(double)me.getX()+vs.width),vs.height);
			    repaint();
			    break;
			}
		    case 1: //north
			if (!dragged){ /* WZ 15/5/02 */
			    resize = true;/* WZ 14/5/02 */
			    setCursor(Cursor.getPredefinedCursor(Cursor.N_RESIZE_CURSOR));
			    vs.setDoubleBounds(vs.px, (double)me.getY(),vs.width, Math.max(5, vs.py-(double)me.getY()+vs.height));
			    repaint();
			    break;
			}
		    case 4: //east
			if (!dragged){ /* WZ 15/5/02 */
			    resize = true;/* WZ 14/5/02 */
			    setCursor(Cursor.getPredefinedCursor(Cursor.E_RESIZE_CURSOR));
			    vs.setDoubleBounds(vs.px ,vs.py, Math.max((double)me.getX()-vs.px,5),vs.height);
			    repaint();
			    break;
			}
		    case 3: //south
			if (!dragged){ /* WZ 15/5/02 */
			    resize = true;/* WZ 14/5/02 */
			    setCursor(Cursor.getPredefinedCursor(Cursor.S_RESIZE_CURSOR));
			    vs.setDoubleBounds(vs.px,vs.py ,vs.width, Math.max(5, (double)me.getY()-vs.py));
			    repaint();
			    break;
			}
		    }

		    /* WZ 14/5/02 */
		    //resize the canvas when required
		    if (vs.px+vs.width > getBounds().width+100){
			setSize((int)(vs.px+vs.width + 100), getBounds().height);
		    }
		    if (vs.py+vs.height > getBounds().height+100){
			setSize(getBounds().width, (int)(vs.py+vs.height+100));
		    }
		}
	    }
	
	    //check all the links
	    for (i = 1; i < linkList.size()+1; i++) { //for every vlink ...
		vl = (vLink)linkList.get("linkKey"+ i);
		if (vl.getSelected()) {
		    for (LinkPoint lp = vl.getStartPoint().getNext(); ;lp = lp.getNext()) {//for every link point except the start and stop point ...
			if (lp.contains((double)me.getX(),(double) me.getY())) {
			    lp.setPosition((double)me.getX(),(double)me.getY());
			    repaint();
			    dragged = true;
			    break;
			}
			if (lp.getNext() == vl.getStopPoint())
			    break;
		    }
		}
	    }
	    
	    if (!dragged){
		dragSelect = true;
		mouseCurrentPoint = new Point(me.getPoint());
		repaint();
	    }
	}
    }

    /**
     * No action.
     * @param me MouseEvent
     * 
     */
    public void mouseEntered(MouseEvent me) {}

    /**
     * No action.
     * @param me MouseEvent
     * 
     */  
    public void mouseExited(MouseEvent me) {}
  
    /**
     * When mouse moved, return the dynamic position of the mouse.
     * @param me MouseEvent
     * 
     */    
    public void mouseMoved(MouseEvent me) {
	mouseCurrentPoint = new Point(me.getPoint());
    }
    
    /**
     * When mouse released, it is the end of the drag selection action.
     * @param me MouseEvent
     */ 
    public void mouseReleased(MouseEvent me) {
	if (dragSelect){
	    mouseCurrentPoint = new Point(me.getPoint());
	    for (int i = 1; i < vShapeList.size()+1; i++) {
		vs = getShape(i);
		if (getSelectedArea().contains(vs.getDoubleBounds()))
		    vs.setSelected(true);
	    }
	    
	    dragSelect = false;
	    selectedArea = null; //reset selected area
	    repaint();
	}
	
	dragged = false;
	resize = false;/* WZ 14/5/02 */
	setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
    }
    
    /**
     * Zoom in/out
     * @param ss the zoom rate: zoom in if more than 1; zoom out if less than 1.
     * 
     */  
    public void setScale(double ss) {
// 	int maxW = 0, maxH = 0;	/* Weihong 11/3/02 */
	for (int j =1; j < vShapeList.size()+1; j++) {
	    vs = getShape(j);
	    vs.scale(ss);
	    //vs.setBounds((int)vs.px-4, (int)vs.py-4, (int)vs.width+8, (int)vs.height+8);
	    vs.setBounds(0,0,getWidth(),getHeight());

// 	    /* Weihong 11/3/02 */
// 	    //record the maximum scale
// 	    maxW = java.lang.Math.max(maxW, (int)(vs.px+vs.width));
// 	    maxH = java.lang.Math.max(maxH, (int)(vs.py+d_Height));
	}
	repaint();
	
	//change the default drawing parameters for the next
	d_Width *= ss;
	d_Height *= ss;

// 	/* Weihong 11/3/02 */
// 	//resize the canvas when required
// 	resize(maxW + 100, maxH + 100);

	/* WZ 14/5/02 */
	//resize the canvas when required
	if (vs.px+vs.width > getBounds().width+100){
	    setSize((int)(vs.px+vs.width + 100), getBounds().height);
	}
	if (vs.py+vs.height > getBounds().height+100){
	    setSize(getBounds().width, (int)(vs.py+vs.height+100));
	}

    }
        
    /**
     * Returns the registered vShape list 
     * @return Hashtable
     */  
    public Hashtable getShapeList(){
	return vShapeList;
    }

    /**
     * Returns the registered vLink list.
     * @return Hashtable
     */  
    public Hashtable getLinkList(){
	return linkList;
    }


    /*********************************************
	      Print to a printer section
    **********************************************/
    /**
     * Gets the bufferedimage for printing.
     * 
     */ 
    public void freezeImageForPrinting(){
	double shrinkRate = 0.7;/* WZ 14/6/02 */
	bufferedImage = new BufferedImage(1+(int)(shrinkRate*getImageArea().width), 1+(int)(shrinkRate*getImageArea().height), BufferedImage.TYPE_INT_RGB);
	printIt(bufferedImage.createGraphics(), shrinkRate);
    }
    
    /**
     * For printing/print preview: draw all the shapes which are already registered with the vShapeList,
     * draw all the links which are already registered with the linkList.
     * @param g the graphics on the drawing canvas
     * 
     */ 
    private void printIt(Graphics g, double shrinkRate) {
	g.setColor(Color.white);
	g.fillRect(0,0, getWidth(), getHeight());
	setScale(shrinkRate);/* WZ 14/6/02 */
// 	Font oldFont = g.getFont();
	g.setFont(new Font("Arial", Font.PLAIN, 10));/* WZ 14/6/02 */
	//draw all the shapes which are already registered with the vShapeList.
	int i;
	for (i =1; i < vShapeList.size()+1; i++) {
	    vs = getShape(i);
	    vs.printIt(g);
	}
	
	//draw all the links which are already registered with the linkList.
	for (i =1; i < linkList.size()+1; i++) {
	    vl = (vLink)linkList.get("linkKey"+ i);
	    vl.paint(g);
	}
	setScale(1/shrinkRate);/* WZ 14/6/02 */
// 	g.setFont(oldFont);
    }

    /**
     * Prints the page at the specified index into the specified Graphics context in the specified format. 
     * A PrinterJob calls the Printable interface to request that a page be rendered 
     * into the context specified by graphics. The format of the page to be drawn is specified by pageFormat.
     * The zero based index of the requested page is specified by pageIndex.
     * If the requested page does not exist then this method returns NO_SUCH_PAGE;
     * otherwise PAGE_EXISTS is returned. The Graphics class or subclass 
     * implements the PrinterGraphics interface to provide additional information.
     * @param g the context into which the page is drawn
     * @param pf the size and orientation of the page being drawn
     * @param pageIndex the zero based index of the page to be drawn
     * @return PAGE_EXISTS if the page is rendered successfully
     * or NO_SUCH_PAGE if pageIndex specifies a non-existent page.
     */ 
    public int print(Graphics g, PageFormat pf, int pageIndex) {
	if (pageIndex >= maxPageNo)
	    return NO_SUCH_PAGE;
	
	g.translate((int)pf.getImageableX(), (int)pf.getImageableY());
	int wPage = (int)pf.getImageableWidth();
	int hPage = (int)pf.getImageableHeight();
	
	int w = bufferedImage.getWidth();
	int h = bufferedImage.getHeight();
	if (w == 0 || h == 0)
	    return NO_SUCH_PAGE;
	
	int nCol = Math.max((int)Math.ceil((double)w/wPage),1);
	int nRow = Math.max((int)Math.ceil((double)h/hPage),1);
	maxPageNo = nCol * nRow;

	int iCol = pageIndex % nCol;
	int iRow = pageIndex / nCol;
	int x = iCol * wPage;
	int y = iRow * hPage;
	
	int wImage = Math.min(wPage, w-x);
	int hImage = Math.min(hPage, h-y);
	
	g.drawImage(bufferedImage, 0, 0, wImage, hImage, x, y, x+wImage, y+hImage, this);
	System.gc();
	
	pageIndex++;
	return PAGE_EXISTS;
    }
    
    /**
     * Returns the image area on the canvas
     * @return Dimension 
     */ 
    public Dimension getImageArea() {
	int w=0, h=0;
	vShape theShape;
	for (int i=1; i < vShapeList.size()+1; i++) {
	    theShape = getShape(i);
	    w = Math.max((int)theShape.px +(int) theShape.width,w);
	    h = Math.max((int)theShape.py + (int)theShape.height, h);
	}
	return new Dimension(w,h);
    }
    
    
    
    /*********************************************
    Draw/redraw all images onto the screen section
    **********************************************/

//       public void paintChildren(Graphics g) {

//       }
    /**
     * This method is invoked by Swing to draw components. 
     * Applications should not invoke paint directly, but should instead use the repaint 
     * method to schedule the component for redrawing.<br>
     * This method actually delegates the work of painting to three protected methods: 
     * paintComponent, paintBorder, and paintChildren. They're called in the order 
     * listed to ensure that children appear on top of component itself. Generally speaking,
     * the component and its children should not paint in the insets area allocated to the border.
     * Subclasses can just override this method, as always. A subclass that just wants to
     * specialize the UI (look and feel) delegate's paint method should just override paintComponent.
     * @overrides paint JComponent 
     * 
     */
    public void paint(Graphics g){
	paintComponent(g);
	super.paintBorder(g);    /* Weihong added on 12/11/2001 */
	if (editMode)
	   paintChildren(g);
    }

    /**
     * If the UI delegate is non-null, calls its paint method. We pass the delegate a copy
     * of the Graphics object to protect the rest of the paint code from irrevocable changes
     * (for example, Graphics.translate()).
     * @overrides paintComponent JComponent 
     * 
     */
    public synchronized void paintComponent (Graphics g) {
	super.paintComponent(g);
	drawIt(g);
    }

    /**
     * Draw all the shapes which are already registered with the vShapeList. 
     * In the edit mode do not paint children. <br>
     * Draw all the links which are already registered with the linkList. <br>
     * To paint the drag selecting rectangle. <br>
     * @param g Graphics of the canvas
     * 
     */
    private void drawIt(Graphics g) {
	if (!editMode)
	    g.setColor(normalColor);
	else
	    g.setColor(new Color(39, 19, 55));

	g.fillRect(0,0, getWidth(), getHeight());
	
	//draw all the shapes which are already registered with the vShapeList.
	int i;
// 	jplan.general.Utility.debugPrintln("vShapeList size: " + vShapeList.size());
	for (i =1; i < vShapeList.size()+1; i++) {
	    vs = getShape(i);
	    if (!editMode) {
		vs.textField.removeMyMouseListener();
		vs.setBounds((int)(vs.px + vs.width), (int)(vs.py + vs.height), getWidth(), getHeight());  /* Weihong added on 28/11/2001 */
// 		jplan.general.Utility.debugPrintln("Not editmode.");
		vs.update(g); //do not show children
	    }
	    else {
		if (vs.getShapeID() != 0)
		    vs.textField.addMyMouseListener();
		vs.setBounds(0,0, getWidth(), getHeight());
		//to paint children only
	    }
	}
	
	//draw all the links which are already registered with the linkList.
	for (i =1; i < linkList.size()+1; i++) {
	    vl = (vLink)linkList.get("linkKey"+ i);
	    vl.paint(g);
	}
	
	//to paint the drag selecting rectangle
	if (dragSelect) {
	    g.setColor(Color.orange);
	    Graphics2D graphics = (Graphics2D) g;
	    graphics.setStroke(new BasicStroke(1));
	    graphics.draw(getSelectedArea());
	}
    }


   

    /*********************************************
        Drag select section
    **********************************************/
    /**
     * Returns the selected area when mouse draged.
     * @return rectangle
     */
    protected Shape getSelectedArea(){
	int a, b, c, d;
	if (mouseDownPoint.x<mouseCurrentPoint.x) {
	    a = mouseDownPoint.x;
	    c = mouseCurrentPoint.x-mouseDownPoint.x; //width
	}
	else {
	    a = mouseCurrentPoint.x;
	    c = mouseDownPoint.x - mouseCurrentPoint.x;
	}
	
	if (mouseDownPoint.y<mouseCurrentPoint.y){
	    b = mouseDownPoint.y;
	    d = mouseCurrentPoint.y-mouseDownPoint.y;
	}
	else {
	    b = mouseCurrentPoint.y;
	    d = mouseDownPoint.y - mouseCurrentPoint.y;
	}
	
	selectedArea = new Rectangle(a,b,c,d);
	return selectedArea;
    }



    /***********************************
	    Basic shapes creation
    ************************************/
    
    /**
	 * Create the vShape at the given location. <br>
	 * Register with the vShapeList; Register with the textPane group; Add it to
	 * the screen; Mark the dirty flag.
	 * 
	 * @param xPoint
	 *            int value at X axis
	 * @param yPoint
	 *            int value at Y axis
	 * @return the vShape created
	 */
	public vShape createVShape(int xPoint, int yPoint) {
		shapeIndex++;
		shapeID++;
		/* Weihong 11/3/02 */
		//resize the canvas when required
		if (xPoint + d_Width > getBounds().width + 100) {
			setSize((int) (xPoint + d_Width + 100), getBounds().height);
		}
		if (yPoint + d_Height > getBounds().height + 100) {
			setSize(getBounds().width, (int) (yPoint + d_Height + 100));
		}
		//create the vShape
		vShape tempShape = new vShape(shapeID, (double) xPoint,
				(double) yPoint, d_Width, d_Height);
		tempShape.setShapeID(shapeTypeID);
		//register with the vShapeList
		vShapeList.put("shapeKey" + shapeIndex, (Object) String
				.valueOf(shapeID));
		shape[shapeID] = tempShape;
		//register with the textPane group
		if (tempShape.getShapeID() != 0 && tempShape.getShapeID() != 5)
			editPaneGroup.addPane(tempShape.textField);
		//add it to the screen
		add(tempShape);
		tempShape.setBounds(0, 0, getWidth(), getHeight());
		//mark the dirty flag
		isDirty = true; /* Weihong changed/added on 2/10/2001 */
		return tempShape;
	}

    /* WZ 27/3/02 */
    /** 
     * to link to the panes outside this
     * 
     */
    public void addToPaneGroup(jplan.general.TransExpressionDocPane tPane){
	editPaneGroup.addPane(tPane);
    }

    /**
	 * Create the vShape from a mouse event. <br>
	 * Register with the vShapeList; Register with the textPane group; Add it to
	 * the screen; Mark the dirty flag; Record state for undo.
	 * 
	 * @param me
	 *            MouseEvent
	 */
	public void createVShape(MouseEvent me) {
		vShape tempShape = createVShape(me.getX(), me.getY());
		//for undo
		recordState();
	}

    /**
	 * Create the vLink from a mouse event. <br>
	 * Click on the first vShape to selected then click on the second vShape to
	 * create the vLink. <br>
	 * Record state for undo.
	 * 
	 * @param me
	 *            MouseEvent
	 *  
	 */
    public void createVLink(MouseEvent me) {
	vShape toThisShape;
	
	if (tempShapeToLink == null) { //if this is the first shape to select
	    tempShapeToLink = selectSingleShape((double)me.getX(),(double)me.getY());
	    if (tempShapeToLink != null) //make sure one shape was selected.
		Utility.debugPrintln("Linking from shape" + tempShapeToLink.getID());
	}
	else {
	    toThisShape = selectSingleShape((double)me.getX(),(double)me.getY());
	    if ((toThisShape != null) && (toThisShape != tempShapeToLink)) {
		createVLink(tempShapeToLink, toThisShape);

		//for undo
		recordState(); 
	    }
	    else {tempShapeToLink = null;}
	}
    }
    
    /**
     * Create the vLink from two given vShapes. <br>
     * Create the link;
     * Register with the linkList;
     * Record state for undo.
     * @param sp1
     * @param sp2
     * 
     */
    public void createVLink(vShape sp1, vShape sp2){
	vLink tempLink;
	linkID++; //for next link id
	linkIndex ++;//for next link in the registration list

	//create the link
	tempLink = new vLink(linkID, sp1, sp2);
	tempLink.setType(linkTypeID);
		
	//register with the linkList
	linkList.put("linkKey"+linkIndex, tempLink);

	//debugging
	Utility.debugPrintln("vLink"+linkID+" from vShape" + tempLink.getStartShape().getID() + " to vShape" + tempLink.getStopShape().getID() + " created and registered.");
		
	tempLink.getStartShape().setSelected(false);
	tempLink.getStopShape().setSelected(false);
		
	//no matter what happend, reset the tempShapeToLink to empty
	tempShapeToLink = null;
    }


    /* WZ 27/5/02 */
    /**
     * Create the vLink with straight line from two given vShapes. <br>
     * Create the link;
     * Register with the linkList;
     * Record state for undo.
     * @param sp1
     * @param sp2
     * routeType
     * 
     */
    public void createVLink(vShape sp1, vShape sp2, int routeType){
	vLink tempLink;
	linkID++; //for next link id
	linkIndex ++;//for next link in the registration list

	//create the link
	tempLink = new vLink(linkID, sp1, sp2, routeType);
	tempLink.setType(linkTypeID);
		
	//register with the linkList
	linkList.put("linkKey"+linkIndex, tempLink);

	//debugging
	Utility.debugPrintln("vLink"+linkID+" from vShape" + tempLink.getStartShape().getID() + " to vShape" + tempLink.getStopShape().getID() + " created and registered.");
		
	tempLink.getStartShape().setSelected(false);
	tempLink.getStopShape().setSelected(false);
		
	//no matter what happend, reset the tempShapeToLink to empty
	tempShapeToLink = null;
    }

    /**
     * clear editpane group. <br>
     * 
     */
    public void clearPaneGroup() {
	EditPaneGroup editPaneGroup = new EditPaneGroup();
    }

    /**
     * Sets the mouse mode. <br>
     * @param m mouse mode. Can be selected from CREATE_SHAPE, CREATE_LINK, SELECT, DELETE.
     * 
     */
    public void setMouseAction(int m) {
	this.mouseAction = m;
    }
    
    /**
     * Sets vShape type for the following vShape to be created. <br>
     * @param s vShape type.<br>
     * Can be selected from vShape.CIRCLE, vShape.RECTANGLE, vShape.ROUND_RECTANGLE.
     * 
     */  
    public void setDrawingShapeID(int s){
	shapeTypeID = s;
    }
    
    /**
     * Sets vLink type for the following vLinks to be created. <br>
     * @param s vLink type.<br> 
     * Can be selected from 0 to 5 standing for blue, magenta, green, cyan, orange, pink.
     * 
     */  
    public void setDrawingLinkID(int s){
	linkTypeID = s;
    }

    /* WZ 10/6/02 */
    /**
     * returns the first vShape which is highlighted/selected
     * @return vShape
     */
    public vShape getFirstSelectedShape() {
	int x = vShapeList.size();
	for (int i = 1; i < x+1; i++) {
	    vs = getShape(i);
	    if (vs.getSelected()) {
		return vs;
	    }
	}
	return null;
    }
    /**
     * Delete all selected vShapes. 
     * 
     */ 
    public void deleteShape(){
	if (mouseAction == DELETE) {
	    boolean changed = false;
	    //remove from the vShapeList
	    Utility.debugPrintln("shapelistSize: " + vShapeList.size());
	    int x = vShapeList.size();
	    for (int i = 1; i < x+1; i++) {
		vs = getShape(i);
		if (vs.getSelected()) {
		    vShapeList.remove("shapeKey"+ i);
		    remove(vs);
		    Utility.debugPrintln("shape " + vs.getID() + " removed.");
		    changed = true; //to confirm that this state should be recorded as an undoable state
		    /*set all its in and out links to be selected
		     * and ready for the subsequent elimination.
		     */
		    int j;
		    //inlinks
		    for (j=1;j < vs.getInLinks().size()+1;j++) {
			vl = (vLink)vs.getInLinks().get("inlinks"+j);
			vl.setSelected(true); //set selected
		    }
		    //outlinks
		    for (j=1;j<vs.getOutLinks().size()+1;j++) {
			vl = (vLink)vs.getOutLinks().get("outlinks"+j);
			vl.setSelected(true); //set selected
		    }
		}
	    }
	    refreshShapeList();
	    repaint();
	    if (changed) {
		recordState(); //for undo
		changed = false;
	    }
	}
	//  else "Please selecte an object to take operations."
    }

    /**
     * Refresh the vShape list.<br>
     * This is the method to call after the delete of vShapes.
     * 
     */
    public void refreshShapeList(){
	int i, j;
	
	for (i=1; i < vShapeList.size()+1; i++) {
	    if (!vShapeList.containsKey("shapeKey"+i)) {
		if (!(i == shapeIndex)) { // not the last shape in the list was removed.
		    //search from the last record until there is a non-empty record.
		    while (!vShapeList.containsKey("shapeKey"+shapeIndex) && shapeIndex > i) {
			shapeIndex --;}
 
		    //shapeIndex is the last shape created.
		    Object obj = vShapeList.get("shapeKey"+shapeIndex);
		    vShapeList.put("shapeKey"+i, obj); //move the last record to the current blank place.
		    vShapeList.remove("shapeKey" + shapeIndex);
		}
		else {} //if the last record was removed then reset the last shape index - shapeIndex to --.
		
		Utility.debugPrintln("Shape " + i + " were replaced by Shape " + shapeIndex);
		shapeIndex--;
	    }
	}
	shapeIndex = vShapeList.size(); //to make sure the last shape in the list.
	Utility.debugPrintln("Till now, the last shape index - vShapeList and vShapeList.size() -------  " + shapeIndex);
    }

    /**
     * Remove all selected vLinks from registration.
     * 
     */
    public void deleteLink(){
	if (mouseAction == DELETE) {
	    boolean changed = false;
	    //remove from the linkList
	    Utility.debugPrintln("linklistSize: " + linkList.size());
	    int x = linkList.size();
	    for (int i = 1; i < x+1; i++) {
		vl = (vLink)linkList.get("linkKey"+ i);
		if (vl.getSelected()) {
		    linkList.remove("linkKey"+ i);
		    changed = true;

		    //remove the registration in the associated shape.
		    Utility.debugPrintln("removing out link registration in vShape" + vl.getStartShape().getID());
		    vl.getStartShape().removeLinks(vShape.OUT, vl);
		    Utility.debugPrintln("removing in link registration in vShape" + vl.getStopShape().getID());
		    vl.getStopShape().removeLinks(vShape.IN, vl);
		}
	    }
	    refreshLinkList();
	    repaint();
	    if (changed) {
		//recordState(); //for undo
		changed = false;
	    }
	}
	//  else "Please selecte an object to take operations."
    }
    
    /**
     * Refresh the vLink list.<br>
     * This is the method to call after the delete of vLinks.
     * 
     */
    public void refreshLinkList(){
	int i, j;
	
	for (i=1; i < linkList.size()+1; i++) {
	    if (!linkList.containsKey("linkKey"+i)) {
		if (!(i == linkIndex)) { // not the last link in the list was removed.
		    //search from the last record until there is a non-empty record.
		    while (!linkList.containsKey("linkKey"+linkIndex) && linkIndex > i) {
			linkIndex --;}
		    vl = (vLink)linkList.get("linkKey"+linkIndex);//linkIndex is the last link created.
		    linkList.put("linkKey"+i, vl); //move the last record to the current blank place.
		    linkList.remove("linkKey" + linkIndex);
		}
		else {} //if the last record was removed then reset the last link index - linkIndex to --.
		
		Utility.debugPrintln("Link " + i + " were replaced by Link " + linkIndex);
		linkIndex--;
	    }
	}
	linkIndex = linkList.size(); //to make sure the last link in the list.
	Utility.debugPrintln("Till now, the last link index - linkList and linkList.size() -------  " + linkIndex);
    }
    
    /**
     * Returns the default size of this canvas.
     * @return Dimension
     */
    public Dimension getPreferredSize() {
// 	return new Dimension(getToolkit().getScreenSize());
	/* WZ 13/5/02 */
	return new Dimension(getBounds().width, getBounds().height);
    }
    
//     /**
//      * Returns the default size of this canvas.
//      * @return Dimension
//      */
//     public Dimension getMinimumSize() {
// 	return new Dimension(getBounds().x, getBounds().y);
//     }

//     /**
//      * Returns the default size of this canvas.
//      * @return Dimension
//      */
//     public Dimension getMaximumSize(){
// 	return new Dimension(getBounds().x, getBounds().y);
//     }


    /*********************************************
        for undo and redo - to store the state
    **********************************************/
    /**
     * For undo and redo - to store the state.<br>
     * Any actions considered as valuable for later undo/redo process
     * must be recorded by calling this method immediately after the action was processed.
     * 
     */
    public void recordState() {
	if (index_Edit == maxSize_undoStates-1) {
	    refreshStatesList();
	    index_Edit --;
	}
	
	index_Edit ++;
	states[index_Edit] = new vState(vShapeList.size(), linkList.size());
	showStates(index_Edit);
    }
    
    /**
     * When records reaches its allowed maximum number of undoable states,
     * all states will be shift forward. As a result, the first record was removed, and last record is empty
     * ready to store the next state.
     * 
     */ 
    private void refreshStatesList() {
	for (int i=1; i<maxSize_undoStates;i++ ) {
	    states[i-1] = states[i];
	}
    }
    
    /**
     * Undo the last action including creating vShape/vLink, deleting vShape/vLink, etc.
     * 
     */ 
    public void undo() {
	if (index_Edit >0)  {
	    index_Edit--;
	    states[index_Edit].restore();
	    showStates(index_Edit);
	    tempShape = null; //to reset the current working shape to null
	}
	else System.err.println("Can not undo.");
    }

    /**
     * Redo the last undo action.
     * 
     */ 
    public void redo() {
	if (states[index_Edit+1] != null){
	    index_Edit++;
	    states[index_Edit].restore();
	    showStates(index_Edit);
	}
	else System.err.println("Can not redo.");
    }
    
    /**
     * Show the states recorded in the concole.
     * This is only for debugging purpose.
     * @param editIndex the counter for the recorded undoable states.
     * 
     */   
    public void showStates(int editIndex) {
	Utility.debugPrintln("\n\n++++++ Current undoable states ++++++");
// 	for (int i=0; i<editIndex+1; i++) {
	    Utility.debugPrintln("STATE "+editIndex+"\n" + states[editIndex].toString());
// 	}
    }
    
    /**
     * Inner class to store the graphical information on the canvas.
     */ 
    class vState {
	private Hashtable shadowShapeList = new Hashtable();
	private vLink[] vlk;
	private int shapeSize, linkSize;

	/**
	 * Show the states recorded in the concole.
	 * @param size_shape the number of vShapes registered in that recording moment.
	 * @param size_link the number of vLinks registered in that recording moment.
	 */	
        public vState(int size_shape, int size_link) {
	    shapeSize = size_shape;
	    linkSize = size_link;
	    
	    vlk = new vLink[size_link];
	    shadowShapeList = (Hashtable)vShapeList.clone();
	    
	    for (int i=0; i<size_link; i++) {
		vlk[i] = (vLink)linkList.get("linkKey"+(i+1)); //links with start and stop shapes information
            }
        }

	/**
	 * Restore the saved graphical information to replace the current state, i.e. undo/redo.
	 * 
	 */
        public void restore() {
	    vShapeList.clear();
	    linkList.clear();
	    removeAll();
	    //dealing with the groups.
	    for (int i = 1; i < MaxNoShapes; i++){
		if (shape[i] == null)
		    break;
		vShape vshape = (vShape)shape[i];
		vshape.resetInLinks();
		vshape.resetOutLinks();
	    }

	    vShapeList = (Hashtable)shadowShapeList.clone();
	    if (shapeSize != 0) {
		for (int i=1; i < shapeSize+1; i++){
		    vShape vp = getShape(i);
		    add(vp);
		    vp.setBounds((int)vp.px-4,(int) vp.py-4,(int)vp.width+8, (int)vp.height+8);
		}
	    }
	    
	    if (linkSize != 0) {
		for (int i=0; i < linkSize; i++){
		    linkList.put("linkKey"+(i+1), vlk[i]);

		    int start = vlk[i].getStartShape().getID();
		    int stop = vlk[i].getStopShape().getID();
		    for (int k=1; k <vShapeList.size()+1; k++){
			vShape vp = getShape(k);
			if (vp.getID() == start){
			    vlk[i].setStartShape(vp);
			    vp.registerLinks(vShape.OUT, vlk[i]);
			}
			if (vp.getID() == stop) {
			    vlk[i].setStopShape(vp);
			    vp.registerLinks(vShape.IN, vlk[i]);
			}
		    }
		}
	    }
	    
	    shapeIndex = vShapeList.size();
	    linkIndex = linkList.size();

	    //debugging
	    for (int i=1; i <vShapeList.size()+1; i++){
		Utility.debugPrintln((getShape(i)).getLabel());
	    }
	    for (int i=1; i <linkList.size()+1; i++){
		Utility.debugPrintln("vLink"+((vLink)linkList.get("linkKey"+i)).getID());
	    }
	    repaint();
        }
	
	/**
	 * A string representation of the states stored.
	 * @return String
	 */
	public String toString() {
	    StringBuffer thisStr = new StringBuffer();
	    
	    for (int i = 1; i < vShapeList.size()+1; i++) {
		vShape v = getShape(i);
		thisStr.append("vShape("+v.getLabel()+")");
		if (v.getSelected())
		    thisStr.append("SELECTED!");
		if (i == vShapeList.size())
		    thisStr.append("\n");
		else
		    thisStr.append(", ");
	    }
	    for (int i=0; i < linkSize; i++){
		thisStr.append("vLink"+vlk[i].getID());
		if (i == linkSize-1)
		    thisStr.append("\n");
		else
		    thisStr.append(", ");
	    }
	    
	    return thisStr.toString();
	}
	
    }
    
    
    
    

  /*********************************************
        Save to a file
  **********************************************/
    /**
     * A string representation of the graphics on the canvas.
     * Used mainly for saving purpose.
     * @return String
     */
    public String to_String() {
	StringBuffer str = new StringBuffer();
jplan.general.Utility.debugPrintln("BEGIN JGRAPHCANVAS\n");
	str.append("BEGIN JGRAPHCANVAS\n");
	
	str.append( "d_Width:"+d_Width+"\n");
	str.append( "d_Height:"+d_Height+"\n");
	str.append( "shapeID:" +shapeID + "\n");
	str.append( "shapeIndex:" +shapeIndex + "\n");
	str.append( "linkIndex:" +linkIndex + "\n");
	str.append( "linkID:" +linkID + "\n");
	jplan.general.Utility.debugPrintln("(Shape Lists)\n");
	str.append( "(Shape Lists):\n");
	for (int i = 1; i<vShapeList.size()+1; i++) {
	    str.append( "\n");
	    vShape v = getShape(i);
	    str.append(v.to_String());
	    str.append( "\n");
	}
	jplan.general.Utility.debugPrintln("(Link Lists)\n");	
	str.append( "(Link Lists):\n");
	for (int i = 1; i<linkList.size()+1; i++) {
	    str.append( "\n");
	    str.append( ((vLink)linkList.get("linkKey"+i)).to_String());
	    str.append( "\n");
	}
	
	str.append( "END JGRAPHCANVAS\n");
// 	jplan.general.Utility.debugPrintln("str.toString()\n"+str.toString());
	return str.toString();
    }
    
    

  /*********************************************
               Reload from a file
  **********************************************/
    /**
     * To open a presaved .vm (Visual Modeller) file, load data from a bufferredReader.
     * 
     */
    public void loadFile (BufferedReader br) {
	String str= "";
	int mySwitch = 0, i = 0, k = 0, j = 0, m = 0, n = 0, re = 0;
	vShape tempSP = null;
	vLink tempLK = null;
	int tempID = 0; // for the shape and link id
	double tempX = 0, tempY = 0, tempWidth = 0, tempHeight = 0;
	int routeType = 0;// for the link line type/* WZ 17/6/02 */
	String tempLabel = null;/* WZ 17/6/02 */

	while (!str.equals("END JGRAPHCANVAS"))  {
	    try {
		str = br.readLine();
		if (str.equals("BEGIN JGRAPHCANVAS"))
		    mySwitch = 1; //for the JGraphCanvas varibles
		if (str.equals("BEGIN VSHAPE"))
		    mySwitch = 2; //for the shape
		if (str.equals("BEGIN VLINK"))
		    mySwitch = 3; //for the link
		if(str.startsWith("END")){/* WZ 20/6/02 */
		    mySwitch = 0;//reset
		}
		switch (mySwitch) {
		case 1:
		    switch (i) {
		    case 1:
			d_Width = parseDouble(str, "d_Width:");
			break;
		    case 2:
			d_Height = parseDouble(str, "d_Height:");
			break;
		    case 3:
			shapeID = parseInt(str, "shapeID:");
			break;
		    case 4:
			shapeIndex = parseInt(str, "shapeIndex:");
			break;
		    case 5:
			linkIndex = parseInt(str, "linkIndex:");
			break;
		    case 6:
			linkID = parseInt(str, "linkID:");
		    }
		    i++;
		    break;
		    
		case 2: //get vShapes
		    if (str.equals("BEGIN VSHAPE")) {
			j = 0; //counter to indicate which line the pointer is pointing
			k ++;
			shapeIndex = k-1;
		    }
		    switch (j) {
		    case 1:
			shapeID = parseInt(str, "index:") - 1;
			break;  
		    case 2:
			tempLabel = getString(str, "label:");
			break;
		    case 3:
			tempX = parseDouble(str, "px:");
			break;  
		    case 4:
			tempY = parseDouble(str, "py:");
			tempSP = createVShape((int)tempX, (int)tempY);
			tempSP.setLabel(tempLabel);
			break;
		    case 5:
			tempWidth = parseDouble(str, "width:");
			break;  
		    case 6:
			tempHeight = parseDouble(str, "height:");
			tempSP.setSize(tempWidth, tempHeight);
			break;
		    case 7:
			tempSP.setShapeID(parseInt(str, "shapeID:"));
			break;  
		    case 8:
			String selected = getString(str, "isSelected:");
			if (selected.equals("true"))
			    tempSP.setSelected(true);
			else
			    tempSP.setSelected(false);
			break;  
		    /* WZ 17/6/02 */
		    case 9:
			String drawLabel = getString(str, "drawLabel:");
			if (drawLabel.equals("true")){
			    tempSP.setDrawLabel(true);
			    tempSP.removeTextField();
			}
			else
			    tempSP.setDrawLabel(false);
		    /* end 17/6/02 */
			
			//reset tempShape
			tempSP = null;
			
			break;
		    }
		    j++;
		    break;
		    
		case 3: //get vLinks
		    if (str.equals("BEGIN VLINK")) {
			m = 0; //counter to indicate which line the pointer is pointing
			n ++;
		    }
		    switch (m) {
			
		    case 1: //get the linkID
			tempID = parseInt(str, "index:");
			break;
			
		    case 2: //get the linkID
			routeType = parseInt(str, "routeType:");
			break;

		    case 3://get the startShape
			boolean gotFirstShape = false;
			re = parseInt(str, "theStartShape Index:");
			for (int counter = 1; counter < vShapeList.size()+1; counter ++) {
			    if (Integer.parseInt(vShapeList.get("shapeKey"+counter).toString()) == re){
				tempSP = (vShape)shape[re];
				gotFirstShape = true;
				break;
			    }
			}
			
			if (!gotFirstShape)
			    System.err.println("Did not get the first shape for vLink" + tempID + ".");
			
			break;
			
		    case 4://get the stopShape
			vShape tempSP2 = null;
			boolean gotSecondShape = false;
			re = parseInt(str, "theStopShape Index:");
			for (int counter = 1; counter < vShapeList.size()+1; counter ++) {
			    tempSP2 = getShape(counter);
			    if (tempSP2.getID() == re) {
				gotSecondShape = true;
				break;
			    }
			}
			
			if (gotSecondShape) 
			    tempLK = new vLink(tempID, tempSP, tempSP2, routeType);/* WZ 17/6/02 */
			
			break;
			
		    case 5://get shapeTypeID and register it
			re = parseInt(str, "linkTypeID:");
			tempLK.setType(re);
			
			linkList.put("linkKey"+n, tempLK); //register with the linkList
			
			//debugging
			Utility.debugPrintln("vLink" + tempID + " from vShape" + tempLK.getStartShape().getID() + " to vShape" + tempLK.getStopShape().getID() + " created and registered.");
			
			tempLK = null;
		    }
		    
		    m++;
		}
    
	    } catch(java.io.IOException ex){};
	};
	
    }
    
    /* WZ 18/6/02 */
    /**
     * Return a string in the baseStr followed keyStr.
     * @param baseStr base string
     * @param keyStr key string
     * @return a string in the baseStr followed keyStr.
     */
    protected String getString (String baseStr, String keyStr) { 
	int length = keyStr.length();
	int indexStr = baseStr.indexOf(keyStr);
	if (indexStr == -1) /* WZ 23/7/02 */
	    return new String ("");
	else
	    return baseStr.substring(length+indexStr);
    }

    /* WZ 18/6/02 */
    /**
     * Get a string in the baseStr followed keyStr, then parse it to double.
     * @param baseStr base string
     * @param keyStr key string
     * @return a double value
     */
    protected double parseDouble (String baseStr, String keyStr) { 
	return Double.parseDouble(getString(baseStr, keyStr));
    }

    /* WZ 18/6/02 */
    /**
     * Get a string in the baseStr followed keyStr, then parse it to int.
     * @param baseStr base string
     * @param keyStr key string
     * @return a int value
     */
    protected int parseInt (String baseStr, String keyStr) { 
	return Integer.parseInt(getString(baseStr, keyStr));
    }
}
