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
 * HierarchicalStateEXDocPane.java
 *
 *
 * Created: Thu Jan 31 09:33:44 2002
 *
 * @author W Zhao
 * @version
 */
import java.awt.event.*;
import javax.swing.*;
import javax.swing.text.*;
import java.util.List;
import java.util.*;
import java.util.NoSuchElementException;
import java.awt.Point;

import jplan.ocl.*;
import jplan.top.OclEd;

public class HierarchicalStateEXDocPane extends StateExpressionDocPane {
    
    public HierarchicalStateEXDocPane(OclEd top, oclDomain cur,boolean editable) {
	super (top, cur, editable);
    }
    
    /**
     * mouseWatch
     * watch for selections of variables and requests for popups
     * override parent method
     */
    public void mouseWatch(MouseEvent mouseEvent) {
	Point viewPoint = mouseEvent.getPoint();
	int clickPos = this.viewToModel(viewPoint);
	DefaultCaret caret = (DefaultCaret)this.getCaret();
	if(SwingUtilities.isLeftMouseButton(mouseEvent)) {
	    int pos = clickPos;
	    StateModel.PredDetail cur = null;
	    String arg = null;
	    try {
		cur = curStateModel.getPredDetailAt(pos);
	    } catch (Exception e) {
		// No predicate found
		return;
	    }
	    try {
		arg = cur.pred.elementAt(pos - cur.startOffset);
		
		int startOffset = cur.startOffset +
		    cur.pred.startElementAt(pos - cur.startOffset);
		int n = cur.pred.elementNoAt(pos - cur.startOffset);
		selectedType = cur.proto.getNthElementName(n);
		List unifiers = curDomain.getSortUnifiers(selectedType);
		selectedArg = arg;
		searcher.markSelected(startOffset,startOffset + arg.length());
		unifiers.add(selectedType);
		selectedUnifiers = unifiers;
		searcher.searchUnifiers(unifiers,startOffset,startOffset + arg.length());
		lastSelPred = cur;
		fireEvent(new ExpressionPaneEvent(this,
						  ExpressionPaneEvent.SELECTION,
						  arg));
		selectedArg = arg;
	    } catch (Exception e) {
		if (e instanceof NoSuchElementException) {
		    Utility.debugPrintln("Illegal sort name!!");
		    return;
		}
	    }
	}
    }

} // HierarchicalStateEXDocPane
