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

import javax.swing.tree.*;
import java.io.*;

/**
 * class to allow the rules for leafs to be changed 
 * for the DefaultMutableTreeNode.
 * @author Ron Simpson
 */
public class SortNode extends DefaultMutableTreeNode 
    implements Serializable{
    
    public SortNode (Object userObject) {
	super(userObject);
    }
    public SortNode (Object userObject,boolean allowsChildren) {
	super(userObject,allowsChildren);
    }
    
    /**
     * Override isLeaf to check if this is at the bottom of the sort tree
     */
    public boolean isLeaf() {
			NodeData nData = (NodeData)getUserObject();
			if (nData.type == NodeData.OBJECT)
	    		return true;
			else
	    		return false;
    	
    }
}
		
