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
 * OperatorPropertyCanvas.java
 *
 *
 * Created: Tue Dec  4 11:09:49 2001
 *
 * @author W Zhao
 * @version
 */
import java.awt.event.*;  
import jplan.graphics.gTool.Graphics.vShape;

public class OperatorPropertyCanvas extends TransitionCanvas {
    
    public OperatorPropertyCanvas(ActionWindow parent) {
	super(parent);
    }
    
    /* WZ 24/4/02 */
    /**
     * transition canvas without parent but with domain
     */
    public OperatorPropertyCanvas(jplan.ocl.oclDomain curDomain) {
	super(null);
	setWorkingDomain(curDomain);
    }

    /**
     * when mouse clicked; override its super
     * @param me MouseEvent
     * 
     */
    public void mouseClicked (MouseEvent me) {	
	//doing nothing, just overide the parents.
    }

} // OperatorPropertyCanvas
