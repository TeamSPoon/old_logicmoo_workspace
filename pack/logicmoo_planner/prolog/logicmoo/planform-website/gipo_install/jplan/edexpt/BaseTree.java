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
 * BaseTree implements an editable tree of OCL sorts
 * This is an abstract class an implementation of the
 * object populating class is required in the derived classes
 * @author Ron Simpson
 */

/*
 * @history -
 * 11/5/03 Replaced DefaultTreeModel with SortTreeModel This allows
 * the in-tree editing to work and update the appropriate SortNode
 */

public abstract class BaseTree extends JTree {

	protected oclDomain curDomain;
	protected java.util.List oclSorts;
	protected java.util.List oclObjects;
	private SortNode rootNode = null;
	protected java.util.List tempOCLSorts = null;
	protected java.util.List tempOCLObjs = null;
	protected oclSort tempPrimSorts;
	private boolean dirty; // Flag to indicate if tre edited

	/**
	 * @constructor
	 * @param curDom The current ocl domain
	 */
	public BaseTree(oclDomain curDom) {
		super((TreeModel) null); // Create the JTree itself
		this.getSelectionModel().setSelectionMode(
			DefaultTreeSelectionModel.SINGLE_TREE_SELECTION);
		// Store the oclElements
		curDomain = curDom;
		oclSorts = curDomain.sorts;
		oclObjects = curDomain.objects;

		// Use horizontal and vertical lines
		putClientProperty("JTree.lineStyle", "Angled");
		String strCodeBase = new String(System.getProperty("ocled.codebase"));
		String strImageDir =
			strCodeBase
				+ File.separator
				+ "jplan"
				+ File.separator
				+ "images"
				+ File.separator;
		ImageIcon icon = ImageLoader.getImageIcon(strImageDir, "Sort3.gif");
		ImageIcon iconleaf =
			ImageLoader.getImageIcon(strImageDir, "Object.gif");
		//ImageIcon icon = new ImageIcon(strImageDir + "Sort3.gif");
		//ImageIcon iconleaf = new ImageIcon(strImageDir + "Object.gif");
		UIManager.put("Tree.openIcon", icon);
		UIManager.put("Tree.closedIcon", icon);
		UIManager.put("Tree.leafIcon", iconleaf);
		UIManager.put("Tree.hash", Color.red);
		SwingUtilities.updateComponentTreeUI(this);

		// Create the first node
		NodeData rdata =
			new NodeData(oclDomain.OCLSortImpliedRoot, NodeData.ROOT);
		rootNode = new SortNode(rdata, true);

		// Populate the root node with its sub-types
		populateSubTypes(rootNode);
		setModel(new SortTreeModel(rootNode));

		dirty = false; // Not yet edited
	}

	/**
	 * restoreTree - set The tree back to state of ocl Domain
	 */
	public void restoreTree() {
		NodeData rdata =
			new NodeData(oclDomain.OCLSortImpliedRoot, NodeData.ROOT);
		rootNode = new SortNode(rdata, true);

		// Populate the root node with its sub-types
		populateSubTypes(rootNode);
		setModel(new SortTreeModel(rootNode));
	}

	/**
	 * updateDomain - Called to save the current state of the ocl Sort Tree
	 *                as a set of ocl Sort and ocl Object clauses and
	 *                update the current domain;
	 */
	public void updateDomain() {
		tempOCLSorts = new ArrayList();
		tempOCLObjs = new ArrayList();
		Enumeration children = rootNode.children();
		//	tempOCLSorts = new ArrayList();
		//	tempOCLObjs = new ArrayList();
		tempPrimSorts =
			new oclSort(curDomain.getDescriptionObject(), "primitive_sorts");
		tempOCLSorts.add(tempPrimSorts);
		if (children != null) {
			while (children.hasMoreElements()) {
				constructClause((SortNode) children.nextElement());
			}
		}
		curDomain.sorts = tempOCLSorts;
		curDomain.objects = tempOCLObjs;
		dirty = false; // have updated the tree
	}

	/**
	 * constructClause - Called to construct the sort and object clauses
	 * @param curNode corresponding to a sort or object
	 */
	private void constructClause(SortNode curNode) {
		boolean firstChild = true;
		oclSort curSort = null;

		NodeData curNodeData = (NodeData) curNode.getUserObject();
		Utility.debugPrintln("YYY Main Sort " + curNodeData.toString());
		Enumeration children = curNode.children();
		if (children != null) {
			if (!children.hasMoreElements()) {
				tempPrimSorts.addSubType(curNodeData.toString());
			}
			while (children.hasMoreElements()) {
				SortNode curSubNode = (SortNode) children.nextElement();
				NodeData curSubData = (NodeData) curSubNode.getUserObject();
				Utility.debugPrintln("YYY Sub Sort " + curSubData.toString());
				if (NodeData.SORT == curSubData.type) {
					if (firstChild) {
						curSort =
							new oclSort(
								curDomain.getDescriptionObject(),
								curNodeData.toString());
						tempOCLSorts.add(curSort);
						firstChild = false;
						// Now descend the tree
						constructClause(curSubNode);
					} else {
						// Now descend the tree
						constructClause(curSubNode);
					}
					Utility.debugPrintln(
						"YYY Parent + sub "
							+ curNodeData.toString()
							+ curSubData.toString());
					curSort.addSubType(curSubData.toString());
				} else {
					constructObjectClause(curNode);
					break;
				}
			}
		} else {
			// no children assume it is a primitive sort
		}
	}

	/**
	 * constructObjectClause - construct object clause for 
	 * primitive sort
	 * @param curNode - representing the primitive sort
	 */

	protected void constructObjectClause(SortNode curNode) {
		NodeData curData = (NodeData) curNode.getUserObject();
		oclObject curObj = new oclObject(curData.toString());
		tempOCLObjs.add(curObj);
		Enumeration children = curNode.children();
		while (children.hasMoreElements()) {
			SortNode curSubNode = (SortNode) children.nextElement();
			NodeData curSubData = (NodeData) curSubNode.getUserObject();
			curObj.addObjID(curSubData.toString());
		}
		tempPrimSorts.addSubType(curData.toString());
	}

	/**
	 * Adds a new node to the tree after construction.
	 * @param parent node (SortNode)
	 * @param name (String)
	 * @param isObject - truw if node represents an object
	 */
	public void addNode(SortNode parent, String name, boolean isObject) {
		NodeData nData = null;
		if (isObject)
			nData = new NodeData(name, NodeData.OBJECT);
		else
			nData = new NodeData(name, NodeData.SORT);
		SortNode node = new SortNode(nData);
		parent.add(node);
		node.setAllowsChildren(!isObject);
		int index = parent.getChildCount() - 1;
		if (index != -1) {
			((SortTreeModel) getModel()).nodesWereInserted(
				parent,
				new int[] { index });
		}
		dirty = true;
	}

	/**
	 * addToRoot - add a new Sort at the root level
	 * @param name of the new sort
	 */
	public void addToRoot(String name) {
		Utility.debugPrintln("Node to root " + name);
		addNode(rootNode, name, false); // must be sort
		dirty = true;
	}

	/**
	 * adds an arbitrary node into the tree
	 * @param parent node i.e. addition point
	 * @param node i.e. node to be added
	 */
	public void addTreeNode(SortNode parent, SortNode node) {
		parent.add(node);
		int index = parent.getChildCount() - 1;
		if (index != -1) {
			((SortTreeModel) getModel()).nodeStructureChanged(parent);
			dirty = true;
		}
	}

	/**
	 * delNode - deletes a selected node
	 * @param selNode - the selected node
	 * @return boolean to indicate success
	 */
	public boolean delNode(SortNode selNode) {
		TreePath[] paths = this.getSelectionPaths();
		if (paths != null && paths.length > 0) {
			selNode = (SortNode) paths[0].getLastPathComponent();
			SortNode par = (SortNode) selNode.getParent();
			if (par == null) {
				return false; // Must be root
			} else {
				par.remove(selNode);
				((SortTreeModel) getModel()).nodeStructureChanged(par);
				dirty = true;
			}
			return true;
		}
		return false;
	}

	/**
	 * isObject - test node to see if it represents an object
	 * @param node - the node
	 * @return boolean result
	 */
	public static boolean isObject(SortNode node) {
		NodeData nData = (NodeData) node.getUserObject();
		if (nData.type == NodeData.OBJECT)
			return true;
		else
			return false;
	}

	/** 
	 * If we are a higher level sort, scan our contents and populate 
	 * with children. In addition, populate those children
	 * if the "descend" flag is true. We only descend once,
	 * to avoid recursing the whole subtree. 
	 * *return  true if some nodes were added
	 */
	abstract protected void populateSubTypes(SortNode parent);

	//      abstract protected void populateSubTypes(SortNode parent) {
	// 	 java.util.List subTypes = null;
	// 	 String pname = ((NodeData)parent.getUserObject()).name;
	// 	 Utility.debugPrint("Populate " + pname);
	// 	 if (pname.equals(oclDomain.OCLSortImpliedRoot)) {
	// 	     subTypes = curDomain.getSortRoots();
	// 	 } else {
	// 	     subTypes = curDomain.getSortSubTypes(pname);
	// 	 }
	// 	 // Process the subTypes
	// 	 if (subTypes != null) {
	// 	     ListIterator li = subTypes.listIterator();
	// 	     while (li.hasNext()) {
	// 		 String subname = (String)li.next();
	// 		 NodeData nData = new NodeData(subname,NodeData.SORT);
	// 		 SortNode node = 
	// 		     new SortNode(nData);
	// 		 node.setAllowsChildren(true);
	// 		 parent.add(node);
	// 		 populateSubTypes(node);
	// 	     }
	// 	 } else { // No subtypes must be bottom level sort
	// 	     // look for objects 
	// 	     ListIterator liObjs = oclObjects.listIterator();
	// 	     java.util.List objNames = null;
	// 	     while (liObjs.hasNext()) {
	// 		 oclObject cur = (oclObject)liObjs.next();
	// 		 if (pname.equals(cur.getObjectSort())) {
	// 		     objNames = cur.getObjectNames();
	// 		     break;
	// 		 }
	// 	     }
	// 	     // Process the objects
	// 	     if (objNames != null) {
	// 		 ListIterator liObj = objNames.listIterator();
	// 		 while (liObj.hasNext()) {
	// 		     String objname = (String)liObj.next();
	// 		     Utility.debugPrintln("Try to add " + objname);
	// 		     NodeData nData = new NodeData(objname,NodeData.OBJECT);
	// 		     SortNode node = 
	// 			 new SortNode(nData);
	// 		     node.setAllowsChildren(false);
	// 		     parent.add(node);
	// 		 }
	// 	     }
	// 	 }
	//      }
	/**
	 * isDirty - check to see if tre has unsaved changes;
	 * @return boolean
	 */
	public boolean isDirty() {
		return dirty;
	}

	/**
	 * getSelectedNodeName - return the name of the selected node
	 * NOTE does not allow root selection
	 * @return String name of node
	 * throws NoSuchElementException
	 */
	public String getSelectedNodeName() throws SortSelectionException {
		TreePath[] paths = this.getSelectionPaths();
		if (paths != null && paths.length > 0) {
			SortNode selNode = (SortNode) paths[0].getLastPathComponent();
			SortNode par = (SortNode) selNode.getParent();
			if (par == null) {
				throw new SortSelectionException("Not allowed to select root.");
			}
			return selNode.toString();
		}
		throw new SortSelectionException("No selection made.");
	}

	/**
	 * isSelectedObject
	 * @return boolean - true if selected node represents an object
	 * throws NoSuchElementException
	 */
	public boolean isSelectedObject() throws SortSelectionException {
		TreePath[] paths = this.getSelectionPaths();
		if (paths != null && paths.length > 0) {
			SortNode selNode = (SortNode) paths[0].getLastPathComponent();
			SortNode par = (SortNode) selNode.getParent();
			if (par == null) {
				throw new SortSelectionException("Not allowed to select root.");
			}
			return isObject(selNode);
		}
		throw new SortSelectionException("No selection made.");
	}

	/**
	 * checkTree
	 * check that displayed tree does not contain duplicate names
	 * @return List - This list will be populated with the names of
	 *               duplicate nodes - zero size indicates no duplicates
	 */
	public java.util.List checkTree() {
		java.util.List names = new ArrayList();
		java.util.List dups = new ArrayList();
		Enumeration enum = rootNode.preorderEnumeration();
		while (enum.hasMoreElements()) {
			SortNode cur = (SortNode) enum.nextElement();
			String name = ((NodeData) cur.getUserObject()).toString();
			names.add(name);
		}
		curDomain.checkListUnique(names, dups, "");
		return dups;
	}

	/**
	 * findNode
	 * @param name  the node name
	 * @return  - the Node with the given name
	 */
	public DefaultMutableTreeNode findNode(String name)
		throws SortSelectionException {
		SortNode node = rootNode;
		Enumeration enum = node.depthFirstEnumeration();
		boolean found = false;
		while (!found && enum.hasMoreElements()) {
			SortNode cur = (SortNode) enum.nextElement();
			String curName = ((NodeData) cur.getUserObject()).name;
			if (curName.equals(name)) {
				found = true;
				return cur;
			}
		}
		throw new SortSelectionException("No such node");
	}

	public class SortSelectionException extends Exception {
		public SortSelectionException() {
			super();
		}
		public SortSelectionException(String s) {
			super(s);
		}
	}

}
