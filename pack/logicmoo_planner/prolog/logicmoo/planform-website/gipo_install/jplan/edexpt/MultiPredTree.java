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
import jplan.general.Utility;

/**
 * MultiPredTree implements an editable tree representation of a 
 * set of related predicates
 * @author Ron Simpson
 */
public class MultiPredTree extends JTree {
    private  DefaultMutableTreeNode rootNode = null;
    private oclPredicate curPred = null;
    protected java.util.List tempArgs = new ArrayList();
    private java.util.List preds = new ArrayList();
    private String Id; // The variable/or Id for this state
    private String sort; // The sort for this state;
    private String expKind; //The kind of expression represented
    //either ss or se
    private boolean dirty;          // Flag to indicate if tree edited
    private int lstIndex = -1; 
    //the index position of the current predicate set
    private TaskView top;     // The TaskView Window

    /**
     * constructor
     * @param expKind
     * @param sort
     * @param Id
     * @param preds
     * @param top
     * @param lstIndex
     */
    public MultiPredTree(String expKind, String sort,String Id, 
			 java.util.List preds,
			 TaskView top, int lstIndex) {
	super((TreeModel)null);			// Create the JTree itself
	this.getSelectionModel().setSelectionMode(
                      DefaultTreeSelectionModel.SINGLE_TREE_SELECTION);
	// Store the oclElements
	this.Id = Id;
	this.sort = sort;
	this.expKind = expKind;
	this.preds = preds;
	this.top = top;
	this.lstIndex = lstIndex;
	// Use horizontal and vertical lines
	putClientProperty("JTree.lineStyle", "Angled");
	String strOCLPath = new String(System.getProperty("ocled.path"));
	ImageIcon icon = new ImageIcon(strOCLPath + "/images/Predicate.gif");
	ImageIcon iconleaf = new ImageIcon(strOCLPath + "/images/Sort3.gif");
	UIManager.put("Tree.openIcon",icon);
       	UIManager.put("Tree.closedIcon",icon);
	UIManager.put("Tree.leafIcon",iconleaf);
	UIManager.put("Tree.hash",Color.red);
	SwingUtilities.updateComponentTreeUI(this);
       
	// Create the first node

	rootNode = 
	    new DefaultMutableTreeNode(makeRootDesc(),true);

	// Populate the root node with its sub-types
	populateTree(rootNode);
	setModel(new DefaultTreeModel(rootNode));
	dirty = false;         // Not yet edited
	//this.getModel().addTreeModelListener(new predTreeListener());

    }

    /**
     * makeRootDesc
     * constructs the expression header info
     */
    private String makeRootDesc() {
	return new String(expKind + "(" + sort + "," + Id + "...)");
	
    }

     /**
     * rePopPredTree - construct the predicate tree again
     * @param expKind (either "ss" or "se"
     * @param sort - the sort for this SSC
     * @param Id - the Object Id (Var)
     * @param preds - the predicate List
     * @param lstIndex - the index position of the associated SSC
     */
    public void rePopPredTree(String expKind, String sort,String Id, 
			 java.util.List preds, int lstIndex) {
	// Store the oclElements
	this.Id = Id;
	this.sort = sort;
	this.expKind = expKind;
	this.preds = preds;
	this.lstIndex = lstIndex;
	rootNode.removeAllChildren();
	rootNode.setUserObject(makeRootDesc());
	populateTree(rootNode);
	((DefaultTreeModel)getModel()).nodeStructureChanged(rootNode);
	int rows = getRowCount() - 1;
	while (rows > 0) {
	    expandRow(rows--);
	}
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
    protected void populateTree(DefaultMutableTreeNode parent) {
	ListIterator li = preds.listIterator();
	while(li.hasNext()) {
	    oclPredicate curPred = (oclPredicate)li.next();
	    DefaultMutableTreeNode predNode = 
		    new DefaultMutableTreeNode(curPred.getName());
	    predNode.setAllowsChildren(true);
	    parent.add(predNode);
	    java.util.List args = curPred.getArguments();
	    ListIterator liArgs = args.listIterator();
	    while (liArgs.hasNext()) {
		String sort = ((oclPredicate.pArg)liArgs.next()).name;
		DefaultMutableTreeNode argNode = 
		    new DefaultMutableTreeNode(new mpArg(sort,sort));
		argNode.setAllowsChildren(false);
		predNode.add(argNode);
	    }
	}
    }

//     /**
//      * treeToPredicate - create an oclPredicate from the tree
//      */

//     public oclPredicate treeToPredicate () {
// 	TreeModel mTree = this.getModel();
// 	oclPredicate curPred = new oclPredicate(mTree.getRoot().toString());	for(int i = 0; i < mTree.getChildCount(mTree.getRoot()); i++ ) {
// 	    curPred.addConstArgument(mTree.getChild(mTree.getRoot(),i).toString());
// 	}
// 	return curPred;
//     }

    /**
     * replaceNode
     * accept a dropped object name to replace the current
     * variable name or object name if it has already been replaced
     * propigate the changes to all unifying variables/objects
     * @param src String - the object name 
     * @param sort String - the name of the sort
     * @param targetNode DefaultMutableTreeNode - the selected node
     * @return boolean - if replacement allowed i.e. object is of correct
     *                   sort and operation sucessful
     */
    public boolean replaceNode(String src,String sort,
			    DefaultMutableTreeNode targetNode){
	Utility.debugPrintln("replaceNode - with " + src);
	mpArg arg = (mpArg)targetNode.getUserObject();
	Utility.debugPrintln("replaceNode - old value " + arg.name
			   + " sort " + arg.sort);
	return false;
    }

//     private class predTreeListener implements TreeModelListener {
// 	public void treeNodesChanged(TreeModelEvent tme) {
// 	    oclPredicate curPred = PredTree.this.treeToPredicate();
// 	    PredTree.this.top.updatePredicateAt(curPred,PredTree.this.lstIndex);
// 	    Utility.debugPrintln("Nodes Changed"); 
// 	};
// 	public void treeNodesInserted(TreeModelEvent tme){
// 	    oclPredicate curPred = PredTree.this.treeToPredicate();
// 	    PredTree.this.top.updatePredicateAt(curPred,PredTree.this.lstIndex);
// 	    Utility.debugPrintln("Nodes Inserted");};
// 	public void treeNodesRemoved(TreeModelEvent tme){
// 	    oclPredicate curPred = PredTree.this.treeToPredicate();
// 	    PredTree.this.top.updatePredicateAt(curPred,PredTree.this.lstIndex);
// 	    Utility.debugPrintln("Nodes Removed");};
// 	public void treeStructureChanged(TreeModelEvent tme){
// 	    oclPredicate curPred = PredTree.this.treeToPredicate();
// 	    PredTree.this.top.updatePredicateAt(curPred,PredTree.this.lstIndex);
// 	    Utility.debugPrintln("New Predicate " + curPred.toString());
// 	}
//     }
				      

    /**
     * isDirty - check to see if tree has unsaved changes;
     * @return boolean
     */
    public boolean isDirty() {
	return dirty;
    }

    /** Representation of a predicate argument.
     * Stores the sort and text of an argument.
     * initially the same but may diverge
     */
    private class mpArg implements Cloneable {
	public String sort;
	public String name;

	public mpArg (String sort,String n) {
	    this.sort = sort;
	    name = new String(n);
	}
	
	public String toString() {
	    return name;
	}

	public Object clone() throws CloneNotSupportedException {
	    return new mpArg(sort,name);
	}
    }
	
}
