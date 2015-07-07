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

package jplan.graphics.gTool.Graphics;

/*
 * vLink.java
 * 07/02/2001
 * Weihong Zhao
*/



import java.awt.*;
import java.lang.Math;
import java.util.Hashtable;
import javax.swing.*;
import java.awt.geom.*;

import jplan.graphics.gTool.Graphics.Double_Dimension;
import jplan.graphics.gTool.Graphics.HighlightPoint;
import jplan.graphics.gTool.Graphics.LinkPoint;
import jplan.graphics.gTool.Graphics.vShape;



/**
 * Visual Links (vLink) - basic link used for graphical display.
 * It is constructed with 4 or more lines and 1 arrow at the end pointing the direction.
 * vLinks can be distinguished with each other by having different colors. 
 * vLinks can be created alone as well as connecting vShapes 
 * to indicate the relationships between those vShapes. 
 * vLinks can be automatically reshaped when the connected vShapes move or reshape.
 * More joint points can be added to increase the flexibility of the vLink. 
 * vLinks support mouse drag for repositioning, mouse click for selection. 
 * vLinks can also be saved as .vm format
 */

public class vLink implements Cloneable   {
    /**
     * internal id - unique int number to distinguish between vLinks
     */
    protected int index;
    /**
     * internal representation of the vLink (concentrated at the start and stop point)
     */
    public Line2D.Double line2d;
    /**
     * start position of the Link
     */
    public double x1, y1; 
    /**
     * stop position of the Link
     */
    public double x2, y2;
    /**
     * offset of the start and stop point to the given mouse position
     */
    public double offsetX1, offsetY1, offsetX2, offsetY2; 
    /**
     * offset value from a point (normally a mouse location)
     */
    private int linkTypeID = 0; 
    /**
     * distinguish between different colours which indicates different links
     */
    private boolean isSelected;
    /**
     * two default start and stop points
     */
    private LinkPoint[] iniLinkPoint = new LinkPoint[2];
    /**
     * the middle point
     */
    private LinkPoint middlePoint;
    /**
     * the start and stop vShape attached in either side of the vLink
     */ 
    private vShape theStartShape, theStopShape;
    /**
     * vLink's joint point id (0 to 3) of start and stop shape
     */
    private int fromPointIDofShape = 4, toPointIDofShape = 4;
    /**
     * to indicate the begining of the vLink
     */
    public final static int FROM = 34590867;
    /**
     * to indicate the end of the vLink
     */
    public final static int TO = 34590868;
     /**
     * straight line
     */
    public final static int STRAIGHT = 1;   
    /* WZ 17/6/02 */
    /**
     * curve line
     */
    public final static int NON_STRAIGHT = 0;   
    private int routeType;//curve line or straight line
    /* end 17/6/02 */



    /**
     * Create the default vLink 
     * @param newIndex the index of the shape
     */ 
    public vLink(int newIndex){
	index = newIndex;
	routeType = NON_STRAIGHT;/* WZ 17/6/02 */
	x1 = 0.0;
	y1 = 0.0;
	x1 = 100.0;
	y1 = 100.0;
	line2d = new Line2D.Double(x1, y1, x2, y2);
	
	isSelected = false;
	iniLinkPoint[0] = new LinkPoint(1,x1,y1);
	iniLinkPoint[1] = new LinkPoint(100,x2,y2);
	iniLinkPoint[0].setNext(iniLinkPoint[1]); //link the two points
	insertPoint(new LinkPoint(25));
	insertPoint(new LinkPoint(75));
	autoResetMiddlePointPosition();
	
	theStartShape = null;
	theStopShape = null;
    }

    /**
     * Create the vLink for two given vShapes
     * @param newIndex the index of the shape
     * @param fromShape the start shape where the link comes from
     * @param toShape the stop shape where the link ends at
     */ 
    public vLink(int newIndex, vShape fromShape, vShape toShape) {
	index = newIndex;
	routeType = NON_STRAIGHT;/* WZ 17/6/02 */
	pickClosestHighlightingPoint(fromShape, toShape); 
	//return two points: startpoint and stoppoint.
	line2d = new Line2D.Double(x1, y1, x2, y2);
	
	isSelected = false;
	iniLinkPoint[0] = new LinkPoint(1,x1,y1);
	iniLinkPoint[1] = new LinkPoint(100,x2,y2);
	iniLinkPoint[0].setNext(iniLinkPoint[1]); //link the two points
	insertPoint(new LinkPoint(25));
	insertPoint(new LinkPoint(75));
	autoResetMiddlePointPosition();
	
	theStartShape = fromShape;
	theStopShape = toShape;
	
	fromShape.registerLinks(vShape.OUT, this);
	toShape.registerLinks(vShape.IN, this);
    }

    /* WZ 27/5/02 */
    /**
     * Create the vLink for two given vShapes
     * @param newIndex the index of the shape
     * @param fromShape the start shape where the link comes from
     * @param toShape the stop shape where the link ends at
     */ 
    public vLink(int newIndex, vShape fromShape, vShape toShape, int routeType) {
	index = newIndex;
	this.routeType = routeType;/* WZ 17/6/02 */
	pickClosestHighlightingPoint(fromShape, toShape); 
	//return two points: startpoint and stoppoint.
	line2d = new Line2D.Double(x1, y1, x2, y2);
	
	isSelected = false;
	iniLinkPoint[0] = new LinkPoint(1,x1,y1);
	iniLinkPoint[1] = new LinkPoint(100,x2,y2);
	iniLinkPoint[0].setNext(iniLinkPoint[1]); //link the two points
	if (routeType != STRAIGHT){
	    insertPoint(new LinkPoint(25));
	    insertPoint(new LinkPoint(75));
	    autoResetMiddlePointPosition(routeType);
	}

	theStartShape = fromShape;
	theStopShape = toShape;
	
	fromShape.registerLinks(vShape.OUT, this);
	toShape.registerLinks(vShape.IN, this);
    }
    
    /**
     * Create the vLink with given positions
     * @param newIndex the index of the shape
     * @param xx1 value at X axis of the start point
     * @param yy1 value at Y axis of the start point
     * @param xx2 value at X axis of the stop point
     * @param yy2 value at Y axis of the stop point
     */ 
    public vLink(int newIndex, double xx1, double yy1, double xx2, double yy2) {
	index = newIndex;
	routeType = NON_STRAIGHT;/* WZ 17/6/02 */
	x1 = xx1;
	y1 = yy1;
	x2 = xx2;
	y2 = yy2;
	line2d = new Line2D.Double(x1, y1, x2, y2);
	
	isSelected = false;
	iniLinkPoint[0] = new LinkPoint(1,x1,y1);
	iniLinkPoint[1] = new LinkPoint(100,x2,y2);
	iniLinkPoint[0].setNext(iniLinkPoint[1]); //link the two points
	insertPoint(new LinkPoint(25));
	insertPoint(new LinkPoint(75));
	autoResetMiddlePointPosition();
    }
    
    /**
     * To insert a new linkPoint
     * @param lp LinkPoint
     * 
     */
    public void insertPoint(LinkPoint lp) {
	for (LinkPoint tempLP = iniLinkPoint[0]; ;  tempLP = tempLP.getNext()) {
	    if (tempLP.getNext() != null) {
		if (lp.getSequenceID()<tempLP.getNext().getSequenceID()) {
		    //insert
		    lp.setNext(tempLP.getNext());
		    tempLP.setNext(lp);
		    break; //found the gas between two linkpoints
		    //priority value is the lower one.
		}
	    }
	}
    }
    
    /**
     * Retruns the vLink's id
     * @return int vLink id
     */
    public int getID() {
	return index;
    }
    
    /**
     * Gets the joining point of the vShape where a vLink comes in or goes out
     * @param mySwitch choosen between two static int parameters: FROM or TO
     * @return int 0 to 3
     */
    public int getJointID(int mySwitch) {
	switch (mySwitch)
	    {
	    case FROM:
		return fromPointIDofShape;
	    case TO:
		return toPointIDofShape;
	    }
	return 4; //null
    }
    
    /**
     * Returns the start point of the vLink
     * @return LinkPoint the start point of the vLink
     */
    public LinkPoint getStartPoint() {
	return iniLinkPoint[0];
    }
    
    /**
     * Returns the stop point of the vLink
     * @return LinkPoint the stop point of the vLink
     */
    public LinkPoint getStopPoint() {
	return iniLinkPoint[1];
    }
    
    /**
     * Sets the start position of this vLink
     * @param new_x X value of the start point
     * @param new_y Y value of the start point
     * 
     */
    public void setStartPosition(double new_x, double new_y) {
	x1 = new_x;
	y1 = new_y;
	line2d = new Line2D.Double(x1, y1, x2, y2);
	iniLinkPoint[0].setPosition(new_x,new_y);
	autoResetMiddlePointPosition();
    }
    
    /**
     * Returns the start position of this vLink
     * @return the start position of this vLink
     */
    public Point2D.Double getStartPosition() {
	Point2D.Double position = new Point2D.Double(x1, y1);
	return position;
    }
    
    /**
     * Sets the stop position of this link
     * @param new_x X value of the stop point
     * @param new_y Y value of the stop point
     * 
     */
    public void setStopPosition(double new_x, double new_y) {
	x2 = new_x;
	y2 = new_y;
	line2d = new Line2D.Double(x1, y1, x2, y2);
	iniLinkPoint[1].setPosition(new_x,new_y);
	autoResetMiddlePointPosition();
    }
    
    /**
     * Returns the stop position of this link
     * @return the stop position of this link
     */
    public Point2D.Double getStopPosition() {
	Point2D.Double position = new Point2D.Double(x2, y2);
	return position;
    }
    
    /**
     * Auto arrange the shapes to its preset shapes
     * 
     */
    public void autoResetMiddlePointPosition(){
	LinkPoint lp = iniLinkPoint[0].getNext();
	if (lp == iniLinkPoint[1])
	    return;
	else {
	    int i = getJointID(FROM);
	    switch (i) {
	    case 0:
		lp.setPosition(x1, y1-(y1-y2)/2); //go up
		lp.getNext().setPosition(x1+(x2-x1)/2, y1-(y1-y2)/2);
		break;
	    case 1:
		lp.setPosition(x1-(x1-x2)/2, y1); //go left
		lp.getNext().setPosition(x1-(x1-x2)/2, y1+(y2-y1)/2);		     
		break;
	    case 2:
		lp.setPosition(x1, y1+(y2-y1)/2); //go down
		lp.getNext().setPosition(x1+(x2-x1)/2, y1-(y1-y2)/2);
		break;
	    case 3:
		lp.setPosition(x1+(x2-x1)/2, y1); //go right
		lp.getNext().setPosition(x1-(x1-x2)/2, y1+(y2-y1)/2);	
		break;
	    }
	}
    }

    /* WZ 27/5/02 */
    /**
     * Auto arrange the shapes to its preset shapes
     * 
     */
    public void autoResetMiddlePointPosition(int routeType){
	LinkPoint lp = iniLinkPoint[0].getNext();
	if (lp == iniLinkPoint[1])
	    return;
	else {
	    int i = getJointID(FROM);
	    switch (i) {
	    case 0:
		lp.setPosition(x1, y1-(y1-y2)/2); //go up
		lp.getNext().setPosition(x1+(x2-x1)/2, y1-(y1-y2)/2);
		break;
	    case 1:
		lp.setPosition(x1-(x1-x2)/2, y1); //go left
		lp.getNext().setPosition(x1-(x1-x2)/2, y1+(y2-y1)/2);		     
		break;
	    case 2:
		lp.setPosition(x1, y1+(y2-y1)/2); //go down
		lp.getNext().setPosition(x1+(x2-x1)/2, y1-(y1-y2)/2);
		break;
	    case 3:
		lp.setPosition(x1+(x2-x1)/2, y1); //go right
		lp.getNext().setPosition(x1-(x1-x2)/2, y1+(y2-y1)/2);	
		break;
	    }
	}
    }

    /**
     * returns the start vShape where the vLink comes out 
     * @return the start vShape
     */
    public vShape getStartShape() {
	return theStartShape;
    }
    
    /**
     * returns the stop vShape where the vLink goes in 
     * @return the stop vShape
     */
    public vShape getStopShape() {
	return theStopShape;
    }
    
    /**
     * Sets the vLink's start vShape 
     * @param vsp the vShape to be set as start vShape
     * 
     */
    public void setStartShape(vShape vsp) {
	theStartShape = vsp;
    }
    
    /**
     * Sets the vLink's stop vShape  
     * @param vsp the vShape to be set as stop vShape
     * 
     */
    public void setStopShape(vShape vsp) {
	theStopShape = vsp;
    }
    
    /**
     * Sets the vLink type 
     * @param new_type the vLink type
     * 
     */
    public void setType(int new_type){ //to change link type by changing the colour of the link
	linkTypeID = new_type;
    }
    
    /**
     * Return the vLink type  
     * @return int the vLink type  
     */
    public int getType()  {
	return linkTypeID;
    }
    
    /*
      public void setLabel(String new_label) {
      label = new_label;
      }
      
      
      public String getLabel()  {
      return label;
      }
    */
    
    /**
     * Selecte/deselect the vLink 
     * @param selected true of false
     * 
     */
      public void setSelected(boolean selected) {
	  isSelected = selected;
	  for (LinkPoint lp = iniLinkPoint[0];;  lp = lp.getNext()) {
	      lp.setVisible(selected);
	      if (lp.getNext()==null)
		  break;
	  }
      }
    
    /**
     * Returns true if the vLink is selected 
     * @return boolean
     */
    public boolean getSelected() {
	return isSelected;
    }
    
    /**
     * Used for mouse drag.
     * Gets the offset of the start point to the current mouse point
     * @param p mouse position (Point)
     *  
     */
    public Double_Dimension getStartOffset(Point p) {
	offsetX1 = (double)p.x - x1;
	offsetY1 = (double)p.y - y1;
	return new Double_Dimension (offsetX1, offsetY1);
    }
    
    /**
     * Used for mouse drag.
     * Gets the offset of the stop point to the current mouse point
     * @param p mouse position (Point)
     *  
     */
    public Double_Dimension getStopOffset(Point p) {
	offsetX2 = (double)p.x - x2;
	offsetY2 = (double)p.y - y2;
	return new Double_Dimension (offsetX2, offsetY2);
    }
    
    /**
     * Retruns true if the given point is inside the vLink's bound 
     * @param x value in X axis
     * @param y value in Y axis
     * @return boolean 
     */
    public boolean contains(double x, double y) {
	double x11, y11, x22, y22;
	double a, b, c, d;
	double xx, yy;
	for (LinkPoint lp = iniLinkPoint[0];;  lp = lp.getNext()) {
	    x11 = lp.cx;
	    y11 = lp.cy;
	    x22=lp.getNext().cx;
	    y22=lp.getNext().cy;
	    if (y11==y22) { //A horizontal line
		if (x11>x22) //point1 is to the left of point 2
		    xx = x22;
		else xx = x11;
		
		Rectangle2D.Double rect = new Rectangle.Double(xx, y11-3, Math.abs(x22-x11), 6);
		if (rect.contains(x, y)) {
		    return true;
		}
	    }
	    else if (x11==x22)  { //A vertical line
		if (y11>y22) //point1 is to the left of point 2
		    yy = y22;
		else yy = y11;
		
		Rectangle2D.Double rect = new Rectangle.Double(x11-3, yy, 6, Math.abs(y22-y11));
		if (rect.contains(x, y))  {
		    return true;
		}
	    }
	    else { //A angled line
		a=Math.abs((y22-y11)*10/(x22-x11));
		b=Math.abs((y-y11)*10/(x-x11));
		c=Math.abs((y11-y22)*10/(x11-x22));
		d=Math.abs((y-y22)*10/(x-x22));
		if (((Math.abs(a-b)<3) ||(Math.abs(c-d)<3)) && ((x11<x && x<x22) || (x22<x && x<x11)) && ((y11<y && y<y22) || (y22<y && y<y11)))
		    return true;
	    }
	    
	    if (lp.getNext()==iniLinkPoint[1])
		break;	       
	}
	return false;
    }
    
    /**
     * Detects if a link is the same as another
     * @param vl given vLink to be comared
     * @return true if the vLink is the same as the given one 
     */
    public boolean equals(vLink vl) {
	if (index == vl.getID())
	    return true;
	return false;
    }
    /**
     * clone a copy of this vLink.
     * @return Object
     * @throws java.lang.CloneNotSupportedException
     */
    public Object clone() throws java.lang.CloneNotSupportedException {
	vLink copy;
	copy = (vLink)super.clone();
	return copy;
    }
    
    /**
     * Repaint the vLink and its contained components if any.
     * @param g vLink Graphics
     * 
     */ 
    public void paint(Graphics g){
	double xxx1;
	double yyy1;
	double xxx2;
	double yyy2;
	
	Graphics2D graphics = (Graphics2D) g;
	graphics.setStroke(new BasicStroke(1));
	
	if(isSelected) 
	    graphics.setColor(Color.red);
	else {
	    switch (linkTypeID) {
	    case 0: 
		graphics.setColor(Color.blue);		 
		break;
	    case 1: 
		graphics.setColor(Color.magenta);
		break;
	    case 2:
		graphics.setColor(Color.green);
		break;
	    case 3:
		graphics.setColor(Color.cyan);
		break;
	    case 4:
		graphics.setColor(Color.orange);
		break;
	    case 5:
		graphics.setColor(Color.pink);
		break;
	    }
	}
	
	//draw this line here.
	for (LinkPoint tempLP = iniLinkPoint[0];;  tempLP = tempLP.getNext()) {
	    if (tempLP != null) {
		Shape shp = new Line2D.Double(tempLP.cx, tempLP.cy, tempLP.getNext().cx, tempLP.getNext().cy);
		graphics.draw(shp);
		//Utility.debugPrintln("link part:" +shp);
		tempLP.paintComponent(g);
		/******paint the highlighting square
		 *if not being selected then the visible property 
		 *of the highlighter will be set to false
		 ******/
		
		if (tempLP.getNext()==iniLinkPoint[1]){
		    //for drawing the arrow
		    xxx1 = tempLP.cx;
		    yyy1 = tempLP.cy;
		    xxx2 = tempLP.getNext().cx;
		    yyy2 = tempLP.getNext().cy;
		    break;
		}
	    }
	}
	
	/* Draw arrow: formulas here */
	double h = 12;//length of the arrow
	double s = 4;//half width of the arrow
	double Xm, Ym, Xa, Ya, Xb, Yb;
	Shape sp;
	
	//get pen ready
	graphics.setColor(Color.black);

	//the middle point
	Xm=h*(xxx1-xxx2)/Math.sqrt(Math.pow(xxx1-xxx2,2)+Math.pow(yyy1-yyy2,2))+xxx2;
	Ym=h*(yyy1-yyy2)/Math.sqrt(Math.pow(xxx1-xxx2,2)+Math.pow(yyy1-yyy2,2))+yyy2;
	
	//left point
	Ya = Ym-s*s/Math.sqrt(s*s+h*h);
	Xa = Xm - s*h/Math.sqrt(s*s+h*h);
	sp = new Line2D.Double(xxx2, yyy2, Xa, Ya);
	graphics.draw(sp);
	
	//right point
	Yb = 2*Ym - Ya;
	Xb = Math.sqrt(s*s - (Yb-Ym)*(Yb-Ym))+Xm;
	sp = new Line2D.Double(xxx2, yyy2, Xb, Yb);
	graphics.draw(sp);
    }
    
    /**
     * Gets the closest highlight point (1 out of 4) from the vshapes: theStartShape and theStopShape
     * 
     */ 
    public void pickClosestHighlightingPoint() {
	pickClosestHighlightingPoint(theStartShape, theStopShape);
    }
    
    /**
     * Get the closest highlight point (1 out of 4) from the given two vshapes
     * @param startShape the vShape that this vLink comes out of
     * @param stopShape the vShape that this vLink ends at
     * 
     */ 
    public void pickClosestHighlightingPoint(vShape startShape, vShape stopShape) {
	int fromPoint, toPoint;
	double lineLength, returnLength = 1000000.0;
	double p1,q1,p2,q2;
	for (fromPoint = 0; fromPoint < 4; fromPoint++) {
	    for (toPoint = 0; toPoint<4; toPoint++) {
		p1=startShape.getShapeHighlighter().hp[fromPoint].cx;
		q1=startShape.getShapeHighlighter().hp[fromPoint].cy;
		p2=stopShape.getShapeHighlighter().hp[toPoint].cx;
		q2=stopShape.getShapeHighlighter().hp[toPoint].cy;
		lineLength = Point2D.distance(p1,q1,p2,q2);
		
		if (Math.abs(lineLength)<Math.abs(returnLength)) {
		    returnLength = Math.abs(lineLength);
		    fromPointIDofShape = fromPoint;
		    toPointIDofShape = toPoint;
		}
	    }
	}
	x1=startShape.getShapeHighlighter().hp[fromPointIDofShape].cx;
	y1=startShape.getShapeHighlighter().hp[fromPointIDofShape].cy;
	x2=stopShape.getShapeHighlighter().hp[toPointIDofShape].cx;
	y2=stopShape.getShapeHighlighter().hp[toPointIDofShape].cy;
    }
    
    /**
     * A string representation of the vLink, mainly used for saving purpose
     * 
     */ 
    public String to_String(){
	
	StringBuffer str = new StringBuffer();
	str.append( "BEGIN VLINK\n");
	
	str.append( "index:"+index+"\n");	
	str.append( "routeType:" +routeType + "\n");/* WZ 17/6/02 */
	str.append( "theStartShape Index:" +theStartShape.getID() + "\n");
	str.append( "theStopShape Index:" +theStopShape.getID() + "\n");
	str.append( "linkTypeID:" +linkTypeID + "\n");
	
	str.append( "x1:" +x1 + "\n");
	str.append( "y1:" +y1 + "\n");
	str.append( "x2:" +x2 + "\n");
	str.append( "y2:" +y2 + "\n");
	str.append( "isSelected:" +isSelected + "\n");

	if (routeType != STRAIGHT){/* WZ 17/6/02 */
	    int i=1;
	    for (LinkPoint thisPoint = iniLinkPoint[0].getNext(); ;thisPoint = thisPoint.getNext()) {
		str.append( "middle point" + i + ":\n");
		str.append( "x:" +thisPoint.cx + "\n");
		str.append( "y:" +thisPoint.cy + "\n");
		if (thisPoint.getNext() == iniLinkPoint[1])
		    break;
		i++;
	    }
	}
	
	str.append( "END VLINK\n");
	
	return str.toString();
    }
    
}



