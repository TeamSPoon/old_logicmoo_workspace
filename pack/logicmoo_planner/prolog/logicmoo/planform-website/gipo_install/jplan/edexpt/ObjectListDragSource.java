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

package jplan.edexpt;

import java.awt.*;
import java.awt.event.*;
import java.awt.datatransfer.*;
import java.awt.dnd.*;
import java.io.*;
import java.util.*;
import javax.swing.*;
import javax.swing.tree.*;

import jplan.ocl.*;
import jplan.general.Utility;

/**
 * ObjectListDragSource - Allow dragging of objects for the Object list
 */
public class ObjectListDragSource implements 
    DragGestureListener,DragSourceListener {

    JList jlstObjects;

    public ObjectListDragSource(JList jlstObjects) {
	this.jlstObjects = jlstObjects;

	// Use the default DragSource
	DragSource dragSource = DragSource.getDefaultDragSource();

	// Create a DragGestureRecognizer and
	// register as the listener
	dragSource.createDefaultDragGestureRecognizer(
		   jlstObjects, DnDConstants.ACTION_COPY_OR_MOVE,
		   this);
    }

    // Implementation of DragGestureListener interface.
    public void dragGestureRecognized(DragGestureEvent dge) {
	String selObj = (String)jlstObjects.getSelectedValue();
	if (selObj == null) {
	    jlstObjects.getToolkit().beep();
	    return;
	}
	// InputEvent ie = dge.getTriggerEvent();
// 	if (!SwingUtilities.isRightMouseButton((MouseEvent)ie)) {
// 	    return;
// 	}
	try {
	    dge.startDrag(null, 
			  new ObjectTransferable(selObj), 
			  ObjectListDragSource.this);
	} catch (Exception e) {
	    Utility.debugPrintln("Drag Exception " +  e.toString());
	}
    }

    // Implementation of DragSourceListener interface
    public void dragEnter(DragSourceDragEvent dsde) {
    }

    public void dragOver(DragSourceDragEvent dsde) {
    }

    public void dragExit(DragSourceEvent dse) {
    }

    public void dropActionChanged(DragSourceDragEvent dsde) {
    }

    public void dragDropEnd(DragSourceDropEvent dsde) {
	if (!dsde.getDropSuccess()) {
	    jlstObjects.getToolkit().beep();
	    return; // Do nothing drop didn't work
	}	
    }
}

