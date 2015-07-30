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
 * PredTreeDragSource - Allow dragging of objects and
 * sorts. - but not the root node
 */
public class PredTreeDragSource implements 
    DragGestureListener,DragSourceListener {
    public PredTreeDragSource(PredTree tree) {
	this.tree = tree;

	// Use the default DragSource
	DragSource dragSource = DragSource.getDefaultDragSource();

	// Create a DragGestureRecognizer and
	// register as the listener
	dragSource.createDefaultDragGestureRecognizer(
		   tree, DnDConstants.ACTION_COPY_OR_MOVE,
		   this);
    }

    // Implementation of DragGestureListener interface.
    public void dragGestureRecognized(DragGestureEvent dge) {
	// Get the mouse location and convert it to
	// a location within the tree.
	Point location = dge.getDragOrigin();
	TreePath dragPath = tree.getPathForLocation(location.x, location.y);
	if (dragPath != null && tree.isPathSelected(dragPath)) {
	    // Get the list of selected files and create a Transferable
	    // The list of files and the is saved for use when 
	    // the drop completes.
	    Utility.debugPrintln("Drag path " + dragPath);
	    path = tree.getSelectionPath();
	    if (path != null) {
		DefaultMutableTreeNode dragNode = null;
		String pathName = null;
		Utility.debugPrintln("Selected path " + path);
		pathName = 
		    path.getLastPathComponent().toString();
		dragNode =
		    (DefaultMutableTreeNode) path.getLastPathComponent();
		dragSort = pathName;
		Utility.debugPrintln("Drag Sort candidate " + pathName);
		if (dragNode.isRoot()) {
		    tree.getToolkit().beep();
		} else {
		    Utility.debugPrintln("CLASS " + pathName);
		    try {
// 			ImageIcon icon = new ImageIcon("/home/ron/jplan/images/Object.gif");
// 			Image dImage = icon.getImage();
// 			dge.startDrag(null,dImage,new Point(5,5), 
// 				      new StringSelection(dragNode.toString()), this);			
			dge.startDrag(null,
				      new StringSelection(dragNode.toString()), this);
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
	
	// If the drop action was ACTION_MOVE, 
	// the tree might need to be updated.
	if (dsde.getDropAction() == DnDConstants.ACTION_MOVE) {
	    final String draggedSort = dragSort;
	    final TreePath draggedPath = path;
				// Remove this node
	    DefaultMutableTreeNode node =
		(DefaultMutableTreeNode)draggedPath.getLastPathComponent();
		((DefaultTreeModel)tree.getModel()).removeNodeFromParent(node);
	}
    }


    protected PredTree tree;// The associated tree
    protected String dragSort;// Dragged Sorts/Objects
    protected TreePath path;// Dragged path
}

