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
 * MethodPropertyCanvas.java
 *
 *
 * Created: Tue Dec  4 10:59:41 2001
 *
 * @author W Zhao
 * @version
 */
import java.awt.event.*;  
import jplan.graphics.gTool.Graphics.vShape;

public class MethodPropertyCanvas extends HighLevelTransitionCanvas {
    
    public MethodPropertyCanvas(HighLevelTransitionWindow parent) {
	super(parent);
    }

    /* WZ 25/4/02 */
    /**
     * HighLevelTransitionCanvas without parent but with domain
     */
    public MethodPropertyCanvas(jplan.ocl.oclDomain curDomain) {
	super(curDomain);
    }

    /**
     * when mouse clicked; override its super
     * @param me MouseEvent
     */
    public void mouseClicked (MouseEvent me) {
// 	if(me.getModifiers() == MouseEvent.BUTTON3_MASK) {
// 	    for (int i = 1; i < vShapeList.size()+1; i++) {
// 		vShape tempShape = (vShape)shape[Integer.parseInt(vShapeList.get("shapeKey"+i).toString())];
// 		if (tempShape.getShapeID() == 5 && tempShape.contains((double)me.getX(),(double)me.getY())){
// 		    menuMethodHead.show(this, me.getX(), me.getY());
// 		    return;
// 		}
// 	    }
// 	}
    }
} // MethodPropertyCanvas
