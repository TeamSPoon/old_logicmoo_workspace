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
import java.awt.geom.*;
import jplan.graphics.gTool.Graphics.*;


/**
 * Basic Connection point.
 * Consists of 4 HighlightPoints at 4 different direction: West, North, East, and South.
**/

public class vShapeHighlightPoint {
    /**
     * the parent vShape
     */
    public vShape thisShape;
    /**
     * 4 highlighting points, anticlockwise starts at west from 0 to 3
     */
       public HighlightPoint[] hp = new HighlightPoint[4];
    /**
     * internal id - unique int number to distinguish between shapes
     */
       protected int index;
    /**
     * indicates the visibility
     */
       private boolean visible = false;
    /**
     * 
     */
       public int theCursor;

    /**
     * Create the default vShapeHighlightPoint 
     * @param vs the parent vShape
     */ 
      public vShapeHighlightPoint(vShape vs) {
	  thisShape = vs;
	  hp[0] = new HighlightPoint(vs.px+vs.width/2, vs.py);
	  hp[1] = new HighlightPoint(vs.px, vs.py+vs.height/2);
	  hp[2] = new HighlightPoint(vs.px+vs.width/2, vs.py+vs.height);
	  hp[3] = new HighlightPoint(vs.px+vs.width, vs.py+vs.height/2);
	  index = vs.getID();
      }
   
    /**
     * gets the vShapeHighlightPoint ID
     * @return int
     */
      public int getID() {
         return index;
      }
 
    /**
     * resets position after vShape is moved/reshaped
     * 
     */
      public void resetPosition() {
	  Rectangle2D.Double box = thisShape.getDoubleBounds();
	  hp[0].setPosition(box.x+box.width/2, box.y);
	  hp[1].setPosition(box.x, box.y+box.height/2);
	  hp[2].setPosition(box.x+box.width/2, box.y+box.height);
	  hp[3].setPosition(box.x+box.width, box.y+box.height/2);
      }
   
    /**
     * sets/disable the visibility
     * @param sel true or false
     * 
     */
      public void setVisible(boolean sel) {
         visible = sel;
	 for (int i=0; i<4;i++)
	     hp[i].setVisible(sel);
      }
   
       /**
     * returns true if the vShape is visible
     * @return true or false
     */
      public boolean getVisible() {
         return visible;
      }

       /**
     * repaint this component
     * @param g Graphics
     * 
     */
       public void paint(Graphics g){
	   int i;
	   for (i=0; i<4; i++) {
	       hp[i].paintComponent(g);
	   }
       }

       /**
     * check if the given point is inside the bound of this vShapeHighlightPoint
     * @param x double value at X axis
     * @param y double value at Y axis
     * @return 0 for nothing traced; 1 to 4 for hp{1 to 4] contains the point;
     */
       public int contains(double x, double y) { // return 0 for nothing traced;
	   int j = 0;                            // return 1 to 4 for hp{1 to 4] contains the point;
	   for (int i=0; i<4; i++) {
	       if (hp[i].contains(x,y))
		   j = i+1;
	   }
	   return j;
       }
       
   }



