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
 * SortTree implements an editable tree of OCL sorts
 * @author Ron Simpson
 */
public class SortOnlyTree extends BaseTree {


    /**
     * @constructor
     * @param curDom current ocl domain
     */
    public SortOnlyTree(oclDomain curDom) {
	super(curDom);
    }
	
    /**
     * This is the method to populate the tree initially
     */
     protected void populateSubTypes(SortNode parent) {
	 java.util.List subTypes = null;
	 String pname = ((NodeData)parent.getUserObject()).name;
	 Utility.debugPrint("Populate " + pname);
	 if (pname.equals(oclDomain.OCLSortImpliedRoot)) {
	     subTypes = curDomain.getSortRoots();
	 } else {
	     subTypes = curDomain.getSortSubTypes(pname);
	 }
	 // Process the subTypes
	 if (subTypes != null) {
	     ListIterator li = subTypes.listIterator();
	     while (li.hasNext()) {
		 String subname = (String)li.next();
		 NodeData nData = new NodeData(subname,NodeData.SORT);
		 SortNode node = 
		     new SortNode(nData);
		 node.setAllowsChildren(true);
		 parent.add(node);
		 populateSubTypes(node);
	     }
	 }
     }	

}
