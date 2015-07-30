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
 
 /**
 * Created on May 11, 2003
 *
 */
package jplan.edexpt;

import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.*;

/**
 * @author ron
 * Facilitate editing of tree nodes
 */
public class SortTreeModel extends DefaultTreeModel {
	
	public SortTreeModel(DefaultMutableTreeNode node) {
		super(node);
	}
	
	
	/**
	  * This sets the user object of the TreeNode identified by path
	  * and posts a node changed.  If you use custom user objects in
	  * the TreeModel you're going to need to subclass this and
	  * set the user object of the changed node to something meaningful.
	  * Needs to use SortNodes
	  */
	public void valueForPathChanged(TreePath path, Object newValue) {
		DefaultMutableTreeNode   aNode = (DefaultMutableTreeNode)path.getLastPathComponent();
		if (newValue.getClass().getName().equals("jplan.edexpt.SortNode")) {
			aNode.setUserObject(newValue);
		} else {
			NodeData data = (NodeData)aNode.getUserObject();
			data.name = (String)newValue;
		}
		nodeChanged(aNode);
	}
}
