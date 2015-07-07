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
 * ObjectDragSource - Allow dragging of objects and
 * attach their sorts
 * @author Ron Simpson
 * @version 0
 */
public class ObjectDragSource implements 
    DragGestureListener,DragSourceListener {

    /**
     * Constructor-
     * @param tree - The Tree
     * @param dndAction The DnDConstants representing the actions
     * supported ie Copy move or both
     */
    public ObjectDragSource(BaseTree tree,int dndAction) {
	this.tree = tree;

	// Use the default DragSource
	DragSource dragSource = DragSource.getDefaultDragSource();

	// Create a DragGestureRecognizer and
	// register as the listener
	dragSource.createDefaultDragGestureRecognizer(
		   tree, dndAction,
		   this);
    }
    /**
     * Constructor-
     * @param tree - The Tree
     * supports both copy and move
     */
    public ObjectDragSource(BaseTree tree) {
	this(tree,DnDConstants.ACTION_COPY);
    }

    // Implementation of DragGestureListener interface.
    public void dragGestureRecognized(DragGestureEvent dge) {
	// Get the mouse location and convert it to
	// a location within the tree.
	Point location = dge.getDragOrigin();
	TreePath dragPath = tree.getPathForLocation(location.x, location.y);
	if (dragPath != null && tree.isPathSelected(dragPath)) {
	    // Get the Object and its parent
	    Utility.debugPrintln("Drag path " + dragPath);
	    paths = tree.getSelectionPaths();
	    // if single selection mode set I do not really need this loop
	    if (paths != null && paths.length > 0) {
		SortNode dragNode = null;
		String pathName = null;
		dragSortsObjects = new String[paths.length];
		for (int i = 0; i < paths.length; i++) {
		    Utility.debugPrintln("Selected path " + paths[i]);
		    pathName = 
			paths[i].getLastPathComponent().toString();
		    dragNode =
			(SortNode) paths[i].getLastPathComponent();
		    if (BaseTree.isObject(dragNode))
			Utility.debugPrintln("OBJECT " + pathName);
		    else
			Utility.debugPrintln("CLASS " + pathName);
		    dragSortsObjects[i] = pathName;
		    Utility.debugPrintln("Drag Sort candidate " + pathName);
		}
		if (!BaseTree.isObject(dragNode)) {
		    tree.getToolkit().beep();
		} else {
		    Utility.debugPrintln("OBJECT " + pathName);
		    String par = ((NodeData)((SortNode)dragNode.getParent()).getUserObject()).name;
		    Utility.debugPrintln("Sort  " + par);
		    try {
			dge.startDrag(null, 
				  new ObjectSortTransferable(pathName,par), ObjectDragSource.this);
		    } catch (Exception e) {
			Utility.debugPrintln("Drag Exception " +  e.toString());
		    }
		}
	    }
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
	    tree.getToolkit().beep();
	    return; // Do nothing drop didn't work
	}
    }

    protected BaseTree tree;// The associated tree
    protected String[] dragSortsObjects;// Dragged Sorts/Objects
    protected TreePath[] paths;// Dragged paths
}

