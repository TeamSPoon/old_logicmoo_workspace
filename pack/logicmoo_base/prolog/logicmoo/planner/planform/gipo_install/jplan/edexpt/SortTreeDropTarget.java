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

import javax.swing.*;
import javax.swing.tree.*;
import java.util.NoSuchElementException;
import java.awt.*;
import java.awt.event.*;
import java.awt.datatransfer.*;
import java.awt.dnd.*;
import java.beans.*;
import java.io.*;
import java.net.*;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;

import jplan.ocl.*;
import jplan.general.Utility;

public class SortTreeDropTarget implements DropTargetListener, 
    PropertyChangeListener {
    public SortTreeDropTarget(SortTree tree) {
	this.tree = tree;
	isObject = false;	
	// Listen for changes in the enabled property
	tree.addPropertyChangeListener(this);
	// Create the DropTarget and register 
	// it with the FileTree.
	dropTarget = new DropTarget(tree,
				    DnDConstants.ACTION_MOVE, 
				    this, 
				    tree.isEnabled(), null);
    }

    // Implementation of the DropTargetListener interface
    public void dragEnter(DropTargetDragEvent dtde) {
	// Save the list of selected items
	saveTreeSelection();
	// Get the type of object being transferred and determine
	// whether it is appropriate.
	checkTransferType(dtde);
	// Accept or reject the drag.
	boolean acceptedDrag = acceptOrRejectDrag(dtde);
	// Do drag-under feedback
	dragUnderFeedback(dtde, acceptedDrag);
    }

    public void dragExit(DropTargetEvent dte) {	
	// Do drag-under feedback
	dragUnderFeedback(null, false);
	// Restore the original selections
	restoreTreeSelection();
    }

    public void dragOver(DropTargetDragEvent dtde) {
	// Accept or reject the drag
	boolean acceptedDrag = acceptOrRejectDrag(dtde);
	// Do drag-under feedback
	dragUnderFeedback(dtde, acceptedDrag);
    }

    public void dropActionChanged(DropTargetDragEvent dtde) {
	// Accept or reject the drag
	boolean acceptedDrag = acceptOrRejectDrag(dtde);
		
	// Do drag-under feedback
	dragUnderFeedback(dtde, acceptedDrag);
    }

    public void drop(DropTargetDropEvent dtde) {
	// Check the drop action
	if ((dtde.getDropAction() & DnDConstants.ACTION_COPY_OR_MOVE) != 0) {
	    // Accept the drop and get the transfer data
	    dtde.acceptDrop(dtde.getDropAction());
	    Transferable transferable = dtde.getTransferable();
	    boolean dropSucceeded = false;
			
	    try {
		tree.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));

				// Save the user's selections
		saveTreeSelection();
														  
		dropSucceeded = dropSort(dtde.getDropAction(),
					 transferable, dtde.getLocation());
				
		Utility.debugPrintln("Drop completed, success: " 
				      + dropSucceeded);
	    } catch (Exception e) {
		Utility.debugPrintln("Exception while handling drop " + e);
	    } finally {
		tree.setCursor(Cursor.getDefaultCursor());

				// Restore the user's selections
		restoreTreeSelection();
		dtde.dropComplete(dropSucceeded);
	    }
	} else {
	    Utility.debugPrintln("Drop target rejected drop");
	    dtde.dropComplete(false);
	}
    }
	
    // PropertyChangeListener interface
    public void propertyChange(PropertyChangeEvent evt) {
	String propertyName = evt.getPropertyName();
	if (propertyName.equals("enabled")) {
	    // Enable the drop target if the SortTree is enabled
	    // and vice versa.		
	    dropTarget.setActive(tree.isEnabled());
	}
    }

    // Internal methods start here

    protected boolean acceptOrRejectDrag(DropTargetDragEvent dtde) {
	int dropAction = dtde.getDropAction();
	int sourceActions = dtde.getSourceActions();
	boolean acceptedDrag = false;
		
	Point location = dtde.getLocation();
	boolean acceptableDropLocation = isAcceptableDropLocation(location);

	// Reject if the object being transferred 
	// or the operations available are not acceptable.
	if (!acceptableType || 
	    (sourceActions & DnDConstants.ACTION_COPY_OR_MOVE) == 0) {
	    Utility.debugPrintln("Drop target rejecting drag");
	    dtde.rejectDrag();
	} else if (!tree.isEditable()) {
	    // Can't drag to a read-only SortTree
	    dtde.rejectDrag();
	} else if (!acceptableDropLocation) {
	    // Can only drag to writable directory
	    dtde.rejectDrag();
	} else if ((dropAction & DnDConstants.ACTION_COPY_OR_MOVE) == 0) {
	    // Not offering copy or move - suggest a copy
	    Utility.debugPrintln("Drop target offering COPY");
	    dtde.acceptDrag(DnDConstants.ACTION_COPY);
	    acceptedDrag = true;
	} else {
	    // Offering an acceptable operation: accept
	    Utility.debugPrintln("Drop target accepting drag");
	    dtde.acceptDrag(dropAction);
	    acceptedDrag = true;
	}

	return acceptedDrag;
    }

    protected void dragUnderFeedback(DropTargetDragEvent dtde, 
				     boolean acceptedDrag) {
	if (dtde != null && acceptedDrag) {
	    Point location = dtde.getLocation();
	    if (isAcceptableDropLocation(location)) {
		tree.setSelectionRow(
				     tree.getRowForLocation(location.x, location.y));
	    } else {
		tree.clearSelection();
	    }
	} else {
	    tree.clearSelection();
	}
    }
		
    protected void checkTransferType(DropTargetDragEvent dtde) {
	// Accept a list of files
	acceptableType = false;
	Utility.debugPrintln("Flavors[0] =" + 
			   dtde.getCurrentDataFlavors()[0].getHumanPresentableName());
	if (dtde.isDataFlavorSupported(ObjectTransferable.objectFlavor)) {
	    acceptableType = true;
	    isObject = true;
	    Utility.debugPrintln("Object Flavor found");
	} else if (dtde.isDataFlavorSupported(SortTransferable.sortFlavor)) {
	    acceptableType = true;
	    isObject = false;
	    Utility.debugPrintln("Sort Flavor found");
	}
    }

    // This method handles a drop 
    protected boolean dropSort(int action,
			       Transferable transferable, Point loc) 
	throws IOException, UnsupportedFlavorException,
	MalformedURLException {
	if (!isAcceptableDropLocation(loc))
	    return false;
	Utility.debugPrintln("Fetching data ");
	if(isObject) {
	    String data = "none";
	    data = (String)transferable.getTransferData(ObjectTransferable.objectFlavor);
	    Utility.debugPrintln("Got object " + data);
	TreePath treePath = tree.getPathForLocation(loc.x, loc.y);
	if (treePath == null) {
	    return false;
	}
	SortNode node = 
	    (SortNode)treePath.getLastPathComponent();
	
	// Highlight the drop location while we perform the drop
	tree.setSelectionPath(treePath);
	transferObject(action, data, node);
	return true;
	} else {
	    SortNode data = null;
	    data = (SortNode)transferable.getTransferData(SortTransferable.sortFlavor);
	    Utility.debugPrintln("Got sort");
	    TreePath treePath = tree.getPathForLocation(loc.x, loc.y);
	    if (treePath == null) {
		return false;
	    }
	    SortNode node = 
		(SortNode)treePath.getLastPathComponent();
	    
	    // Highlight the drop location while we perform the drop
	    tree.setSelectionPath(treePath);
	    transferSort(action, data, node);
	    return true;
	}
    }

    
	
    protected boolean isAcceptableDropLocation(Point loc) {
	if (isObject) {
	    TreePath sel;
	    int row = tree.getRowForLocation(loc.x,loc.y);
	    if (row >= 0) {
		sel = tree.getPathForRow(row);
		SortNode endPt = 
		    (SortNode)sel.getLastPathComponent();	
		if (SortTree.isObject(endPt) || endPt.isRoot())
		    return false;
		else {
		    try {
			SortNode child;
			child = (SortNode)endPt.getFirstChild();
			if (! SortTree.isObject(child)) 
			    return false;
		    } catch (NoSuchElementException e) {
		    }
		}
	    } else {
		return false;
	    }
	} else { // This must be a sort
	    TreePath sel;
	    int row = tree.getRowForLocation(loc.x,loc.y);
	    if (row >= 0) {
		sel = tree.getPathForRow(row);
		SortNode endPt = 
		    (SortNode)sel.getLastPathComponent();	
		if (SortTree.isObject(endPt))
		    return false;
		else {
		    try {
			SortNode child;
			child = (SortNode)endPt.getFirstChild();
			if (SortTree.isObject(child)) 
			    return false;
		    } catch (NoSuchElementException e) {
		    }
		}
	    } else {
		return false;
	    }
	}
	return true;
    }
	
    protected void saveTreeSelection() {
	selections = tree.getSelectionPaths();
	leadSelection = tree.getLeadSelectionPath();
	tree.clearSelection();
    }

    protected void restoreTreeSelection() {
	tree.setSelectionPaths(selections);

	// Restore the lead selection
	if (leadSelection != null) {
	    tree.removeSelectionPath(leadSelection);
	    tree.addSelectionPath(leadSelection);
	}
    }

    

    // Copy or move object 
    protected void transferObject(int action,  String src,
				SortNode targetNode) {
	Utility.debugPrintln("Try to add to tree " + src);
	
	// Update the tree display
	if (targetNode != null) {
		tree.addNode(targetNode,src,true);
	}
    }

    // Copy or move sort node 
    protected void transferSort(int action,  SortNode src,
				SortNode targetNode) {
	Utility.debugPrintln("Try to add sort to tree ");
	
	// Update the tree display
	if (targetNode != null) {
		tree.addTreeNode(targetNode,src);
	}
    }



	
    public static void main(String[] args) {
	final JFrame f = new JFrame("SortTree Drop Target Example");
	oclDomain curDomain = new oclDomain(false);
	oclSort curSort = null;
	curSort = curDomain.addSort("thing");
	curSort.addSubType("transport");
	try {
	    final SortTree tree = new SortTree(curDomain);
			
	    // Add a drop target to the SortTree
	    SortTreeDropTarget target = new SortTreeDropTarget(tree);

	    tree.setEditable(true);

	    f.addWindowListener(new WindowAdapter() {
		public void windowClosing(WindowEvent evt) {
		    System.exit(0);
		}
	    });

	    JPanel panel = new JPanel();
	    final JCheckBox editable = new JCheckBox("Editable");
	    editable.setSelected(true);
	    panel.add(editable);
	    editable.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent evt) {
		    tree.setEditable(editable.isSelected());
		}
	    });

			
	    final JCheckBox enabled = new JCheckBox("Enabled");
	    enabled.setSelected(true);
	    panel.add(enabled);
	    enabled.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent evt) {
		    tree.setEnabled(enabled.isSelected());
		}
	    });
			
	    f.getContentPane().add(new JScrollPane(tree), BorderLayout.CENTER);
	    f.getContentPane().add(panel, BorderLayout.SOUTH);
	    f.setSize(500, 400);
	    f.setVisible(true);
	} catch (Exception e) {
	    Utility.debugPrintln("Failed to build GUI: " + e);
	}
    }

    protected boolean isObject;         // Is node to be dropped an object
    protected SortTree tree;
    protected DropTarget dropTarget;
    protected boolean acceptableType;	// Indicates whether data is acceptable
    TreePath[] selections;		// Initially selected rows
    TreePath leadSelection;		// Initial lead selection
    boolean copyOverExistingFiles;		
}


