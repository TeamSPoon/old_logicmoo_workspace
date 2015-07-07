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
import javax.swing.event.*;
import java.awt.*;
import java.awt.dnd.*;
import java.util.*;
import java.io.*;

import jplan.ocl.*;
import jplan.images.ImageLoader;
import jplan.general.Utility;

/**
 * PredTree implements an editable tree representation of a predicate
 * @author Ron Simpson
 */
public class PredTree extends JTree {
    private  DefaultMutableTreeNode rootNode = null;
    private oclPredicate curPred = null;
    protected java.util.List tempArgs = new ArrayList();
    private boolean dirty;          // Flag to indicate if tree edited
    private int lstIndex = -1; //the index position of the current predicate
    private PredChangeListener top;     // The PredicateView Window

    /**
     * constructor
     * @param curPred
     * @param top
     * @param lstIndex
     */
    public PredTree(oclPredicate curPred,PredChangeListener top, int lstIndex) {
	super((TreeModel)null);			// Create the JTree itself
	this.getSelectionModel().setSelectionMode(
                      DefaultTreeSelectionModel.SINGLE_TREE_SELECTION);
	// Store the oclElements
	this.curPred = curPred;
	this.top = top;
	this.lstIndex = lstIndex;
	// Use horizontal and vertical lines
	putClientProperty("JTree.lineStyle", "Angled");
	String strCodeBase = new String(System.getProperty("ocled.codebase"));
	String strImageDir = strCodeBase + File.separator +
	    "jplan" + File.separator + "images" + File.separator;
	ImageIcon icon = ImageLoader.getImageIcon(strImageDir, "Predicate.gif");
	ImageIcon iconleaf = ImageLoader.getImageIcon(strImageDir, "Sort3.gif");
	//ImageIcon icon = new ImageIcon(strImageDir + "Predicate.gif");
	//ImageIcon iconleaf = new ImageIcon(strImageDir + "Sort3.gif");
	UIManager.put("Tree.openIcon",icon);
       	UIManager.put("Tree.closedIcon",icon);
	UIManager.put("Tree.leafIcon",iconleaf);
	UIManager.put("Tree.hash",Color.red);
	SwingUtilities.updateComponentTreeUI(this);
       
	// Create the first node

	rootNode = 
	    new DefaultMutableTreeNode(curPred.getName(),true);

	// Populate the root node with its sub-types
	populateArgs(rootNode);
	setModel(new DefaultTreeModel(rootNode));
	dirty = false;         // Not yet edited
	this.getModel().addTreeModelListener(new predTreeListener());

    }


    /**
     * setTreePred - set The tree to show new predicate
     */
    public void setTreePred(oclPredicate curPred, int lstIndex) {
	this.curPred = curPred;
	this.lstIndex = lstIndex;
	rootNode = 
	    new DefaultMutableTreeNode(curPred.getName(),true);

	// Populate the root node with its sub-types
	populateArgs(rootNode);
	setModel(new DefaultTreeModel(rootNode));
	this.getModel().addTreeModelListener(new predTreeListener());
	dirty = false;
    }

    /**
     * restoreTree - set The tree back to st
     */
    public void restoreTree() {
	rootNode = 
	    new DefaultMutableTreeNode(curPred.getName(),true);

	// Populate the root node with its sub-types
	populateArgs(rootNode);
	setModel(new DefaultTreeModel(rootNode));
	this.getModel().addTreeModelListener(new predTreeListener());	
    }
    
    /**
     * addToRoot - add a new Sort at the root level
     * @param  name of the new sort
     */
    public void addToRoot(String name) {
	 DefaultMutableTreeNode node =
	    new DefaultMutableTreeNode(name);
	rootNode.add(node);
	node.setAllowsChildren(false);
	int index = rootNode.getChildCount() - 1;
	if (index != -1) {
	    ((DefaultTreeModel)getModel()).nodesWereInserted(
				       rootNode, new int[] { index });
	}
	dirty = true;
    }

    /**
     * addToRootAfter - add a new Sort at the root level
     * after the specified node
     * @param  name of the new sort
     * @param target
     */
    public void addToRootAfter(String name,DefaultMutableTreeNode target) {
	int insIndex = 0;
	DefaultMutableTreeNode node =
	    new DefaultMutableTreeNode(name);
	 if (target.isRoot()) {
	     // Add as first child
	     rootNode.insert(node,0);
	     insIndex = 0;
	 } else {
	     int selIndex = rootNode.getIndex(target);
	     rootNode.insert(node,selIndex+1);
	     insIndex = selIndex + 1;
	 }
	 node.setAllowsChildren(false);
	 int index = rootNode.getChildCount() - 1;
	 if (index != -1) {
	     ((DefaultTreeModel)getModel()).nodesWereInserted(
				   rootNode, new int[] { insIndex });
	     makeVisible(new TreePath(node.getPath()));
	}
	 
	dirty = true;
    }


    /**
     * delNode - deletes a selected node
     * @param selNode - the selected node
     * @return boolean to indicate success
     */
    public boolean delNode(DefaultMutableTreeNode selNode) {
	DefaultMutableTreeNode par = 
	    (DefaultMutableTreeNode)selNode.getParent();
	if (par == null) {
	    return false; // Must be root
	} else {
	    par.remove(selNode);
	    ((DefaultTreeModel)getModel()).nodeStructureChanged(par);
		dirty = true;
	}
	return true;
    }

	   
    /** 
     * Add the predicate arguments
     */
    protected void populateArgs(DefaultMutableTreeNode parent) {
	java.util.List args = curPred.getArguments();
	ListIterator li = args.listIterator();
	while (li.hasNext()) {
	    DefaultMutableTreeNode node = 
		new DefaultMutableTreeNode(li.next());
	    node.setAllowsChildren(false);
	    parent.add(node);
	}
    }

    /**
     * treeToPredicate - create an oclPredicate from the tree
     */

    public oclPredicate treeToPredicate () {
	TreeModel mTree = this.getModel();
	oclPredicate curPred = new oclPredicate(mTree.getRoot().toString());	for(int i = 0; i < mTree.getChildCount(mTree.getRoot()); i++ ) {
	    curPred.addConstArgument(mTree.getChild(mTree.getRoot(),i).toString());
	}
	return curPred;
    }


    private class predTreeListener implements TreeModelListener {
	public void treeNodesChanged(TreeModelEvent tme) {
	    oclPredicate curPred = PredTree.this.treeToPredicate();
	    PredTree.this.top.updatePredicateAt(curPred,PredTree.this.lstIndex);
	    Utility.debugPrintln("Nodes Changed"); 
	};
	public void treeNodesInserted(TreeModelEvent tme){
	    oclPredicate curPred = PredTree.this.treeToPredicate();
	    PredTree.this.top.updatePredicateAt(curPred,PredTree.this.lstIndex);
	    Utility.debugPrintln("Nodes Inserted");};
	public void treeNodesRemoved(TreeModelEvent tme){
	    oclPredicate curPred = PredTree.this.treeToPredicate();
	    PredTree.this.top.updatePredicateAt(curPred,PredTree.this.lstIndex);
	    Utility.debugPrintln("Nodes Removed");};
	public void treeStructureChanged(TreeModelEvent tme){
	    oclPredicate curPred = PredTree.this.treeToPredicate();
	    PredTree.this.top.updatePredicateAt(curPred,PredTree.this.lstIndex);
	    Utility.debugPrintln("New Predicate " + curPred.toString());
	}
    }
				      

    /**
     * isDirty - check to see if tree has unsaved changes;
     * @return boolean
     */
    public boolean isDirty() {
	return dirty;
    }
	
}
