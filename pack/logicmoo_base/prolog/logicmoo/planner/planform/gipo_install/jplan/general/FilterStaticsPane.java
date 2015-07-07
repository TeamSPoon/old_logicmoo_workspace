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

package jplan.general;

import java.awt.event.*;
import java.awt.*;
import javax.swing.event.*;
import javax.swing.*;
import java.util.*;

import jplan.ocl.*;
import jplan.general.Utility;
import jplan.edexpt.PredListCellRenderer;

/**
 * FilterStaticsPane
 * A window with a popup menu
 * menu representing the object/sort tree
 * and a content with a list of static Predicates
 * selecting an item from the menu will result in the list being filtered 
 * by reference to the sort/object
 * NOTE : Currently not filtering by sort - Need to add prototypes and sort
 * the arguments to task initial state predicates
 * @author ron
 */

public class FilterStaticsPane extends JScrollPane {
	private oclDomain curDom = null;    
	private JPopupMenu popupMenu; // THE POPUP MENU
	private String selectedName = "All";
	private JList jlstStatics = null;
	private java.util.List curStatics = null;
	private DefaultListModel lmStatics = null;
	
	/**
	 * basic constructor
	 * @param dom - the current domain
	 * @param title - the title for the window to display on the top bar
	 */
	public FilterStaticsPane(oclDomain dom, String title) {
		curDom = dom;
		curStatics = curDom.atomicInvars;
		selectedName = "All";
		buildFilterMenu();
		jlstStatics = new JList();
		lmStatics = new DefaultListModel();
		jlstStatics.setModel(lmStatics);
		jlstStatics.setEnabled(false);
		setBorder(BorderFactory.createTitledBorder(title));
		add(jlstStatics);	
		setViewportView(jlstStatics);
		jlstStatics.addMouseListener(mouseListener);
		jlstStatics.setToolTipText("Right click to filter");
		filterList("All");
	}

    private MouseListener mouseListener = new MouseInputAdapter(){
		public void mouseClicked(MouseEvent me){
	    	mouseWatch(me);
		}
    };


    /**
     * mouseWatch
     * All we need look for is a button press
     * @param mouseEvent - the mouse event that has occures
     */
    public void mouseWatch(MouseEvent mouseEvent) {
		if(SwingUtilities.isRightMouseButton(mouseEvent)) {
			popupMenu.show(mouseEvent.getComponent(),mouseEvent.getX(),mouseEvent.getY());
		} 
    }
    
	/**
	 * buildFilterMenu
	 * build the menu to represent the sort tree
	 */
	private void buildFilterMenu() {
		popupMenu = new JPopupMenu();
		JMenuItem mItem = new JMenuItem("All");
		JMenu sortMenu = new JMenu("Sorts");
		JMenu objectMenu = new JMenu("Objects");
		popupMenu.add(mItem);
		popupMenu.add(sortMenu);
		popupMenu.add(objectMenu);
		mItem.addActionListener(new java.awt.event.ActionListener() {
					public void actionPerformed(
						java.awt.event.ActionEvent evt) {
						mItemActionPerformed(evt);
					}
				});
		populateSubTypes(curDom.OCLSortImpliedRoot, sortMenu,false);
// Not yet working
		populateSubTypes(curDom.OCLSortImpliedRoot, objectMenu,true);
	}

	/**
	 * populateSubTypes
	 * recursevely descend the sort hierarchy building the menu
	 * @param parent - the current root sort
	 * @param curMenu - the menu item to build on to
	 * @param objects - if true build the object menu otherwise
	 *                    build the sort menu
	 */
	private void populateSubTypes(String parent, JMenu curMenu,
									boolean objects) {
		java.util.List subTypes = null;
		String pname = parent;
		Utility.debugPrint("Populate " + pname);
		if (pname.equals(oclDomain.OCLSortImpliedRoot)) {
			subTypes = curDom.getSortRoots();
		} else {
			JMenu mData = new JMenu(parent);
			subTypes = curDom.getSortSubTypes(pname);
			if (subTypes != null || objects) {
				curMenu.add(mData);
				curMenu = mData;
			}
		}
		// Process the subTypes
		if (subTypes != null) {
			ListIterator li = subTypes.listIterator();
			while (li.hasNext()) {
				String subname = (String) li.next();
				if (!objects && curDom.getObjectsOfSort(subname) != null) {
					JMenuItem mItem = new JMenuItem(subname);
					mItem.addActionListener(new java.awt.event.ActionListener() {
						public void actionPerformed(
							java.awt.event.ActionEvent evt) {
							mItemActionPerformed(evt);
						}
					});
					curMenu.add(mItem);
				}
				populateSubTypes(subname, curMenu,objects);
			}
		} else if (objects) { // No subtypes must be bottom level sort
			// look for objects 
			ListIterator liObjs = curDom.objects.listIterator();
			java.util.List objNames = null;
			while (liObjs.hasNext()) {
				oclObject cur = (oclObject) liObjs.next();
				if (pname.equals(cur.getObjectSort())) {
					objNames = cur.getObjectNames();
					break;
				}
			}
			// Process the objects
			if (objNames != null) {
				ListIterator liObj = objNames.listIterator();
				while (liObj.hasNext()) {
					String objname = (String) liObj.next();
					Utility.debugPrintln("Try to add " + objname);
					JMenuItem mItem = new JMenuItem(objname);
					mItem.addActionListener(new java.awt.event.ActionListener() {
					public void actionPerformed(
						java.awt.event.ActionEvent evt) {
						mItemActionPerformed(evt);
					}
				});
					curMenu.add(mItem);
				}
			}
		}
	}


	/**
	 * mItemActionPerformed
	 * the action to perform when menu item selected
	 * @param evt - the action event
	 */	
	private void mItemActionPerformed(java.awt.event.ActionEvent evt) {
		String filterName = evt.getActionCommand();
		Utility.debugPrintln("ACTION COMMAND " + evt.getActionCommand());
		if (!selectedName.equals(filterName)) {
			selectedName = filterName;
			filterList(selectedName);
		}
			
	}
	
	
	/**
	 * filterList
	 * filter the current state list on the selected sort/object
	 * @param fName- the selected filter name
	 */
	private void filterList(String fName) {
		if (fName.equals("All")) {
			lmStatics.clear();
			ListIterator li = curStatics.listIterator();
			while (li.hasNext()) {
				oclPredicate pred = (oclPredicate) li.next();
				lmStatics.addElement(pred);
			}
		} else {
			if (curDom.isSort(fName)) {
				lmStatics.clear();
				ListIterator li = curStatics.listIterator();
				while (li.hasNext()) {
					oclPredicate pred = (oclPredicate)li.next();
					if (pred.refersToSort(fName))
						lmStatics.addElement(pred);
				}
			} else {
				// Must be an object to sort by
				lmStatics.clear();
				ListIterator li = curStatics.listIterator();
				while (li.hasNext()) {
					oclPredicate pred = (oclPredicate) li.next();
					if (pred.refersTo(fName))
						lmStatics.addElement(pred);
				}
			}
		}
		updateUI();
	}
}
