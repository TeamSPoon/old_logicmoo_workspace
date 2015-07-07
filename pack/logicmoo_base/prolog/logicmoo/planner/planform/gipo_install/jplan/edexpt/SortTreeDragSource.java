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
 * SortTreeDragSource - Allow dragging of objects and
 * sorts. - but not the root node
 */
public class SortTreeDragSource implements 
    DragGestureListener,DragSourceListener {
    private boolean objectOnlyDrag = false; // Ron 2/11/01

    /**
     * Constructor-
     * @param tree  The Tree
     * @param dndAction The DnDConstants representing the actions
     * @param objectsOnly - true if only objects dragged
     * supported ie Copy move or both
     */
    public SortTreeDragSource(BaseTree tree,int dndAction,boolean objectsOnly) {
	this(tree,dndAction); 
	objectOnlyDrag = objectsOnly;
    }

    /**
     * Constructor-
     * @param tree - The Tree
     * @param dndAction The DnDConstants representing the actions
     * supported ie Copy move or both
     */
    public SortTreeDragSource(BaseTree tree,int dndAction) {
	this.tree = tree;
	objectOnlyDrag = false; // Ron 2/11/01
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
    public SortTreeDragSource(BaseTree tree) {
	this(tree,DnDConstants.ACTION_COPY_OR_MOVE);
	objectOnlyDrag = false; // Ron 2/11/01
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
		if (dragNode.isRoot()) {
		    tree.getToolkit().beep();
		} else if (BaseTree.isObject(dragNode)) {
		    Utility.debugPrintln("OBJECT " + pathName);
		    try {
			dge.startDrag(null, 
				  new ObjectTransferable(pathName), SortTreeDragSource.this);
		    } catch (Exception e) {
			Utility.debugPrintln("Drag Exception " +  e.toString());
		    }
		} else if (!objectOnlyDrag && !BaseTree.isObject(dragNode)) {
		    Utility.debugPrintln("CLASS " + pathName);
		    try {
			dge.startDrag(null, 
				  new SortTransferable(dragNode), this);
		    } catch (Exception e) {
			Utility.debugPrintln("Drag Exception " +  e.toString());
		    }
		} else {
		    tree.getToolkit().beep();
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
	    final String[] draggedSorts = dragSortsObjects;
	    final TreePath[] draggedPaths = paths;
	    for (int i = 0; i < draggedSorts.length; i++) {
				// Remove this node
		SortNode node =
		    (SortNode)draggedPaths[i].
		    getLastPathComponent();
		((DefaultTreeModel)tree.getModel()).
		    removeNodeFromParent(node);
	    }
	}
    }

    /**
     * This is just a test routine
     */
    public static void main(String[] args) {
	JFrame f = new JFrame("Draggable SortTree Tree");
	oclDomain curDomain = new oclDomain(false);
	oclSort curSort = null;
	curSort = curDomain.addSort("thing");
	curSort.addSubType("cargo");
	oclObject curObj = curDomain.addObject("cargo");
	curObj.addObjID("c1");
	curObj.addObjID("c2");
	curObj.addObjID("c3");
	f.addWindowListener(new WindowAdapter() {
		public void windowClosing(WindowEvent evt) {
		    System.exit(0);
		}
	    });
	try {
	    SortTree tree = new SortTree(curDomain);
	    f.getContentPane().add(new JScrollPane(tree));

	    // Attach the drag source
	    SortTreeDragSource dragSource = new SortTreeDragSource(tree);
	    SortTreeDropTarget dropTarget = new SortTreeDropTarget(tree);
	    tree.setEditable(true);
	} catch (Exception e) {
	}
	f.pack();
	f.setVisible(true);
    }
    protected BaseTree tree;// The associated tree
    protected String[] dragSortsObjects;// Dragged Sorts/Objects
    protected TreePath[] paths;// Dragged paths
}

