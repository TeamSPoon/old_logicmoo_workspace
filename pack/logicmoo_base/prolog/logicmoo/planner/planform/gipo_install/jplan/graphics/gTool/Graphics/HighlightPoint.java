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
 * vShape.java
 * 24/1/2001 - Chinese New Year - Snake
 * Auther: Weihong Zhao
 */


import java.awt.*;
import javax.swing.*;
import java.awt.geom.*;

import jplan.graphics.gTool.Graphics.Double_Dimension;



/**
 * Basic Connection point.
 * A square shaped component with red outline and yellow filling color
 */

public class HighlightPoint extends JComponent implements Cloneable{
    /**
     * internal id - unique int number to distinguish between each other.
     */
    protected int index;
    /**
     * position (top left corner)
     */
    private double px, py;
    /**
     * Center point
     */
    public double cx, cy; 
    /**
     * size
     */
    private double width, height;
    /**
     * for display
     */
    private Color lineColor; 
    /**
     * for display
     */
    private Color fillColor;
    /**
     * visibility
     */
    protected boolean visible = false;
    
    
    /**
     * Create the default highligh point for a given center point
     */
    public HighlightPoint(double x, double y) {
	setDoubleBuffered(true);
	setOpaque(true);
	
	width = 6.0;
	height = width;
	cx = x;
	cy = y;
	px = cx - 0.5 * width;
	py = cy - 0.5 * height;
    }
    
    /**
     * gets the vShapeHighlightPoint ID
     * @return int
     */
    public int getID() {
	return index;
    }
    
    /**
     * sets the top left corner 
     * @param x double value at X axis
     * @param y double value at Y axis
     * @
     */
    public void setPosition(double x, double y){
	cx = x;
	cy = y;
	px = cx - 0.5 * width;
	py = cy - 0.5 * height;
    }
   
    /**
     * gets the top left corner of the vShape 
     * @return Point2D.Double value the top left corner
     */
    public Point2D.Double getPosition()
    {
        Point2D.Double position = new Point2D.Double(px, py);
	return position;
    }
    
    /**
     * resize this vShape and readjust the position of the highlightingArea 
     * and save status for the undo/redo
     * @param new_width double value of width
     * @param new_height double value of height
     * @
     */
    public void setSize(double new_width, double new_height){
	width = new_width;
	height = new_height;
    }
    
    /**
     * gets the size of the vShape 
     * @return Double_Dimension value of the size
     */
    public Double_Dimension getDoubleSize() {
	Double_Dimension box = new Double_Dimension(width, height);
	return box;
    }
    
    /**
     * setVisible - 
     * @parm  boolean sel
     * return -
     */
    public void setVisible(boolean sel){
	visible = sel;
    }
    
    /**
     * sets/disable the visibility
     */
    public boolean getVisible(){
	return visible;
    }
    
    /**
     * check if the given point is inside the bound of this HighlightPoint
     * @param x double value at X axis
     * @param y double value at Y axis
     * @return 0 for nothing traced; 1 to 4 for hp{1 to 4] contains the point;
     */
    public boolean contains(double x, double y) {
	//for detecting a larger sensitive area
	if (px-5<=x && (px+width+10)>=x && (py-5)<=y && (py+height+10)>=y)
	    return true;
	else return false;
    }
    
    /**
     * clone a copy of this vShape
     * @return Object
     */   
    public Object clone() throws java.lang.CloneNotSupportedException{
	HighlightPoint copy;
	copy = (HighlightPoint)super.clone();
	return copy;
    }
    
    /**
     * repaint the shape and its contained component textField
     * @param g vShape graphics
     * @
     */	
    public synchronized void paintComponent(Graphics g){
	super.paintComponent(g);
	if (visible) {      
	    Graphics2D graphics = (Graphics2D) g;
	    graphics.setStroke(new BasicStroke(1));
	    
	    Point2D.Double position = new Point2D.Double(px, py);
	    
	    double w = width;
	    double h = height;
	    
	    //draw shape
	    lineColor = Color.red;
	    fillColor = Color.yellow;
	    
	    Shape sh = new Rectangle((int)px,(int)py,(int)w,(int)h);
	    fillShape(graphics,sh);
	    drawOutLine(graphics,sh);
	}
    }
    
    /**
     * draw its bound line
     * @param g Graphics2D
     * @param s shape type
     * 
     */
    private void drawOutLine(Graphics2D g, Shape s) {
	g.setColor(lineColor);
	g.draw(s);
    }
    
    /**
     * fills the shape
     * @param g Graphics2D
     * @param s shape type
     *
     */	
    private void fillShape(Graphics2D g, Shape s) {
	g.setColor(fillColor);
	g.fill(s);
    }
    
}



