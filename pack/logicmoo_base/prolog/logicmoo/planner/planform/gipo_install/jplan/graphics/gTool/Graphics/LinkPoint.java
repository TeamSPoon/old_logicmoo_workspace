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
 * LinkPoint.java
 * 9/2/2001
 * Weihong Zhao
*/




import jplan.graphics.gTool.Graphics.HighlightPoint;
import java.awt.event.*;
import jplan.graphics.gTool.Graphics.Double_Dimension;


/**
 *	Basic link point used for vLinks.
**/

public class LinkPoint extends HighlightPoint {

    private LinkPoint nextPoint;
    private double sequenceID;


    /**
     * Create the default LinkPoint 
     * @param priority double value to indicate the order in a link compare with other linkPoints
     */
    public LinkPoint(double priority) {
	super(0,0);
	sequenceID = priority;
	nextPoint = null;
    }

    /**
     * Create the LinkPoint at a given location
     * @param priority double value to indicate the order in a link compare with other linkPoints
     * @param x double value at X axis
     * @param y double value at Y axis
     */
    public LinkPoint(double priority, double x, double y) {
	super(x,y);
	sequenceID = priority;
	nextPoint = null;
    }

    
    /**
     * gets next joint point
     * @return LinkPoint
     */
    public LinkPoint getNext() {
	return nextPoint;
    }
    
    /**
     * used for insert a new point
     * @return double value of getSequenceID
     */
    public double getSequenceID(){
	return sequenceID;
    }
    
    /**
     * sets its next point
     */
    public void setNext(LinkPoint lp){
	nextPoint = lp;
    }

}



