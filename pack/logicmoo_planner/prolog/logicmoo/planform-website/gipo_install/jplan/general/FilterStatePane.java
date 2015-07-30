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
 * FilterStatePane
 * A window with a popup menu
 * menu representing the object/sort tree
 * and a content with a list of object states
 * selecting an item from the menu will result in the list being filtered 
 * by reference to the sort/object
 * NOTE : Currently not filtering by sort - Need to add prototypes and sort
 * the arguments to task initial state predicates
 * @author ron
 */

public class FilterStatePane extends JScrollPane {
	private oclDomain curDom = null;    
	private JPopupMenu popupMenu; // THE POPUP MENU
	private String selectedName = "All";
	private JList jlstCurState = null;
	private ArrayList curState = new ArrayList();
	private DefaultListModel lmCurState = null;
	private int sessSwitch = 0;
	
	/**
	 * basic constructor
	 * @param dom - the current domain
	 * @param title - the title for the window to display on the top bar
	 */
	public FilterStatePane(oclDomain dom, String title) {
		curDom = dom;
		selectedName = "All";
		buildFilterMenu();
		jlstCurState = new JList();
		lmCurState = new DefaultListModel();
		jlstCurState.setModel(lmCurState);
		jlstCurState.setEnabled(false);
		setBorder(BorderFactory.createTitledBorder(title));
		jlstCurState.setCellRenderer(new PredListCellRenderer());
		add(jlstCurState);	
		setViewportView(jlstCurState);
		jlstCurState.addMouseListener(mouseListener);
		jlstCurState.setToolTipText("Right click to filter");
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
	 * @param evt  the action event
	 */	
	private void mItemActionPerformed(java.awt.event.ActionEvent evt) {
		String filterName = evt.getActionCommand();
		Utility.debugPrintln("ACTION COMMAND " + evt.getActionCommand());
		if (!selectedName.equals(filterName)) {
			selectedName = filterName;
			filterList(selectedName,sessSwitch);
		}
			
	}
	
	/**
	  * show states to a JList.
	  * @param stateList input
	  * @param mySwitch switch between oclSS and oclSE
	  */
	public void showStateProperty(java.util.List stateList,int mySwitch) {
		sessSwitch = mySwitch;
		curState.clear();
		ListIterator li = stateList.listIterator();
		while (li.hasNext()) {
			if (mySwitch == 1) {
				oclSS ss = (oclSS) li.next();
				curState.add(ss);
			} else if (mySwitch == 2) {
				oclSE se = (oclSE) li.next();
				curState.add(se);
			}
		}
		filterList(selectedName,mySwitch);
	}
	
	/**
	 * filterList
	 * filter the current state list on the selected sort/object
	 * @param fName - the selected filter name
	 * @param mySwitch
	 */
	private void filterList(String fName,int mySwitch) {
		if (fName.equals("All")) {
			lmCurState.clear();
			ListIterator li = curState.listIterator();
			while (li.hasNext()) {
				if (mySwitch == 1) {
					oclSS ss = (oclSS) li.next();
					lmCurState.addElement(ss);
				} else if (mySwitch == 2) {
					oclSE se = (oclSE) li.next();
					lmCurState.addElement(se);
				}
			}
		} else {
			if (curDom.isSort(fName)) {
				lmCurState.clear();
				ListIterator li = curState.listIterator();
				while (li.hasNext()) {
					if (mySwitch == 1) {
						oclSS ss = (oclSS) li.next();
						if (ss.refersToSort(fName))
							lmCurState.addElement(ss);
					} else if (mySwitch == 2) {
						oclSE se = (oclSE) li.next();
						if (se.refersToSort(fName))
							lmCurState.addElement(se);
					}
				}
			} else {
				// Must be an object to sort by
				lmCurState.clear();
				ListIterator li = curState.listIterator();
				while (li.hasNext()) {
					if (mySwitch == 1) {
						oclSS ss = (oclSS) li.next();
						if (ss.refersTo(fName))
							lmCurState.addElement(ss);
					} else if (mySwitch == 2) {
						oclSE se = (oclSE) li.next();
						if (se.refersTo(fName))
							lmCurState.addElement(se);
					}
				}
			}
		}
		updateUI();
	}
	

}
