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

/**
 * StaticPredTree implements an editable tree representation of a predicate
 * modelled on a static predicate
 * @author Ron Simpson
 */
public class StaticPredTree extends JTree {
    private  DefaultMutableTreeNode rootNode = null;
    private oclPredicate curPred = null;
    private oclPredicate proto = null;
    protected java.util.List tempArgs = new ArrayList();
    private boolean dirty;          // Flag to indicate if tree edited
    private int lstIndex = -1; //the index position of the current predicate
    private AtomicInvarView top;     // The PredicateView Window
    private oclDomain curDomain = null;

    /**
     * constructor
     * @param curPred
     * @param top
     * @param lstIndex
     */
    public StaticPredTree(oclPredicate curPred,AtomicInvarView top, 
			  int lstIndex) {
	super((TreeModel)null);			// Create the JTree itself
	this.getSelectionModel().setSelectionMode(
                      DefaultTreeSelectionModel.SINGLE_TREE_SELECTION);
	// Store the oclElements
	this.curPred = curPred;
	try {
	    proto = (oclPredicate)curPred.clone();
	} catch (Exception e) {
	}
	this.top = top;
	this.lstIndex = lstIndex;
	// Use horizontal and vertical lines
	putClientProperty("JTree.lineStyle", "Angled");
       
	// Create the first node

	rootNode = 
	    new DefaultMutableTreeNode(curPred.getName(),true);

	// Populate the root node with its sub-types
	populateArgs(rootNode);
	setModel(new DefaultTreeModel(rootNode));
	dirty = false;         // Not yet edited
    }

    /**
     * setCurDomain
     * allow access to the current domain definition - needed for
     * hierarchical manipulation of sorts
     * @param cur - the current domain
     */
    public void setCurDomain(oclDomain cur) {
	curDomain = cur;
    }


    /**
     * setTreePred - set The tree to show new predicate
     * @param curPred the predicate/prototype to display
     * @param lstIndex the index position of the prototype in the
     * prototype list
     */
    public void setTreePred(oclPredicate curPred, int lstIndex) {
	this.curPred = curPred;
	try {
	    proto = (oclPredicate)curPred.clone();
	} catch (Exception e) {
	}
	this.lstIndex = lstIndex;
	rootNode = 
	    new DefaultMutableTreeNode(curPred.getName(),true);

	// Populate the root node with its sub-types
	populateArgs(rootNode);
	setModel(new DefaultTreeModel(rootNode));
	dirty = false;
    }

    /**
     * setTreeExistingPred - set The tree to show existing predicate
     * @param curPred the predicate to display
     * @param proto the prototype for the predicate
     * @param lstIndex the index position of the prototype in the
     * prototype list
     */
    public void setTreeExistingPred(oclPredicate curPred, 
			    oclPredicate proto, int lstIndex) {
	this.curPred = curPred;
	this.proto = proto;
	this.lstIndex = lstIndex;
	rootNode = 
	    new DefaultMutableTreeNode(curPred.getName(),true);

	// Populate the root node with its sub-types
	populateExistingArgs(rootNode);
	setModel(new DefaultTreeModel(rootNode));
	dirty = false;
    }

    /**
     * restoreTree - set The tree back to start
     */
    public void restoreTree() {
	rootNode = 
	    new DefaultMutableTreeNode(curPred.getName(),true);

	// Populate the root node with its sub-types
	populateArgs(rootNode);
	setModel(new DefaultTreeModel(rootNode));	
    }

    // Ron 18/1/02 added ability to add objects of a subsort
    /**
     * replaceNode - add an object to replace the current sort
     * cgeck that sort is compatable
     * @param name - the name of the new sort
     * @param sort 
     * @return boolean - success 
     */
    public boolean replaceNode(String name,String sort,
			    DefaultMutableTreeNode target) {
	int inx = rootNode.getIndex(target);
	String targSort = proto.getNthElementName(inx);
	if (sort.equals(targSort)) {
	    rootNode.remove(inx);
	    rootNode.insert(new DefaultMutableTreeNode(name),inx);
	    ((DefaultTreeModel)getModel()).nodeStructureChanged(rootNode);
	    dirty = true;
	    return true;
	} else if (curDomain != null && 
		   curDomain.sortIsSubSortOf(sort,targSort)){
	    rootNode.remove(inx);
	    rootNode.insert(new DefaultMutableTreeNode(name),inx);
	    ((DefaultTreeModel)getModel()).nodeStructureChanged(rootNode);
	    dirty = true;
	    return true;
	} else {
	    return false;
	}
    }


    /**
     * delNode - set the node back to the proto type sort
     * @param selNode - the selected node
     * @return boolean to indicate success
     */
    public boolean delNode(DefaultMutableTreeNode selNode) {
	int inx = rootNode.getIndex(selNode);
	String targSort = proto.getNthElementName(inx);
	DefaultMutableTreeNode par = 
	    (DefaultMutableTreeNode)selNode.getParent();
	if (par == null) {
	    return false; // Must be root
	} else {
	    rootNode.remove(inx);
	    rootNode.insert(new DefaultMutableTreeNode(targSort),inx);
	    ((DefaultTreeModel)getModel()).nodeStructureChanged(rootNode);
	    ((DefaultTreeModel)getModel()).nodeStructureChanged(par);
		dirty = true;
	}
	return true;
    }

	   
    /** 
     * Add the predicate prototype arguments
     */
    protected void populateArgs(DefaultMutableTreeNode parent) {
	java.util.List args = proto.getArguments();
	ListIterator li = args.listIterator();
	while (li.hasNext()) {
	    DefaultMutableTreeNode node = 
		new DefaultMutableTreeNode(li.next());
	    node.setAllowsChildren(false);
	    parent.add(node);
	}
    }

    /** 
     * Add the predicate existing arguments
     */
    protected void populateExistingArgs(DefaultMutableTreeNode parent) {
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

    /**
     * predicateComplete
     * Test to see that all sort names have been replaced by constants
     * @return boolean
     */
    public boolean predicateComplete() {
	oclPredicate cur = treeToPredicate();
	java.util.List predArgs = cur.getArguments();
	java.util.List protoArgs = proto.getArguments();
	ListIterator predLi = predArgs.listIterator();
	ListIterator protoLi = protoArgs.listIterator();
	boolean diff = true;
	while(diff && predLi.hasNext() && protoLi.hasNext()) {
	    oclPredicate.pArg predArg = (oclPredicate.pArg)predLi.next();
	    oclPredicate.pArg protoArg = (oclPredicate.pArg)protoLi.next();
	    if (predArg.name.equals(protoArg.name)) {
		diff = false;
	    }
	}
	return diff;
    }
							      

    /**
     * isDirty - check to see if tree has unsaved changes;
     * @return boolean
     */
    public boolean isDirty() {
	return dirty;
    }
	
}
