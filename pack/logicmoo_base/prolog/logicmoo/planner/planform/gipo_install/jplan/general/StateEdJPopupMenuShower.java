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

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.text.*;
import java.util.*;

public class StateEdJPopupMenuShower extends MouseAdapter {

    private JPopupMenu popup;
    private JTextPane edPane;
    private java.util.List allowedPoints;
    private Point popupTarget;

    public StateEdJPopupMenuShower(JPopupMenu popup,JTextPane edPane,
				   java.util.List allowedPoints,
				   Point popupTarget) {
	this.popup = popup;
	this.edPane = edPane;
	this.allowedPoints = allowedPoints;
	this.popupTarget = popupTarget;
    }

    private void showIfPopupTrigger(MouseEvent mouseEvent) {
	Point viewPoint = mouseEvent.getPoint();
	int clickPos = edPane.viewToModel(viewPoint);
	DefaultCaret caret = (DefaultCaret)edPane.getCaret();
	if(mouseEvent.isPopupTrigger()) {
	    //caret.positionCaret(mouseEvent);
	    int pos = caret.getDot();
	    Utility.debugPrintln("Dot = " + pos + " View = " + clickPos);
	    ListIterator li = allowedPoints.listIterator();
	    boolean found = false;
	    while (li.hasNext() && !found) {
		Point next = (Point)li.next();
		Utility.debugPrintln("X = " + next.x + " Y = " + next.y);
		if(next.x <= clickPos && next.y >= clickPos) {
		    popupTarget.x = next.x;
		    popupTarget.y = next.y;
		    found = true;
		}
	    }
	    if (found)
		popup.show(mouseEvent.getComponent(),mouseEvent.getX(),
			   mouseEvent.getY());
	}
    }

    public void mousePressed(MouseEvent mouseEvent) {
	showIfPopupTrigger(mouseEvent);
    }

    public void mouseReleased(MouseEvent mouseEvent) {
	showIfPopupTrigger(mouseEvent);
    }
}

