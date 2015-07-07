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

import javax.swing.*;
import javax.swing.event.*;
import java.awt.event.*;
import java.util.*;

public class ListChooserPane extends JPanel {
    DefaultListModel listLeft;
    DefaultListModel listRight;
    JList jListLeft;
    JList jListRight;
    JButton jbnAddAll;
    JButton jbnAdd;
    JButton jbnDel;
    JButton jbnDelAll;
    boolean allowInheritance;


    public ListChooserPane() {
	this(true);
    }

    /**
     * @param allowInheritance - allowSubSets if false do not show left pane
     */
    public ListChooserPane(boolean allowInheritance) {
	this.allowInheritance = allowInheritance;
	listLeft = new DefaultListModel();
	listRight = new DefaultListModel();
	jListLeft = new JList(listLeft);
	jListRight = new JList(listRight);
	jListLeft.setFixedCellWidth(100);
	jListRight.setFixedCellWidth(100);
	jbnAddAll = new JButton("    Add All   >> ");
	jbnAdd =    new JButton("      Add     >> ");
	jbnDelAll = new JButton(" << Delete All   ");
	jbnDel =    new JButton(" <<   Delete     ");
	jbnAddAll.addActionListener (new java.awt.event.ActionListener () {
	    public void actionPerformed (java.awt.event.ActionEvent evt) {
		jbnAddAllActionPerformed ();
	    }
	}
				      );
	jbnAdd.addActionListener (new java.awt.event.ActionListener () {
	    public void actionPerformed (java.awt.event.ActionEvent evt) {
		jbnAddActionPerformed ();
	    }
	}
				      );
	jbnDelAll.addActionListener (new java.awt.event.ActionListener () {
	    public void actionPerformed (java.awt.event.ActionEvent evt) {
		jbnDelAllActionPerformed ();
	    }
	}
				      );
	jbnDel.addActionListener (new java.awt.event.ActionListener () {
	    public void actionPerformed (java.awt.event.ActionEvent evt) {
		jbnDelActionPerformed ();
	    }
	}
				      );
	initComponents();
    }

    private void initComponents() {
	setLayout(new BoxLayout(this,BoxLayout.X_AXIS));
	JPanel leftPane = new JPanel();
	JPanel buttonPane = new JPanel();
	JPanel rightPane = new JPanel();
	JScrollPane scrLeft = new JScrollPane(jListLeft);
	JScrollPane scrRight = new JScrollPane(jListRight);
	leftPane.add(scrLeft);
	rightPane.add(scrRight);
	buttonPane.setLayout(new BoxLayout(buttonPane,BoxLayout.Y_AXIS));
	if (allowInheritance) {
	    buttonPane.add(jbnAdd);
	    buttonPane.add(jbnAddAll);
	}
	buttonPane.add(jbnDel);
	buttonPane.add(jbnDelAll);
	if (allowInheritance) {
	    add(leftPane);
	}
	add(buttonPane);
	add(rightPane);
	
    }

    /**
     * initLeftList
     * initialise the contents of the left list
     * @param content - the contents
     */
    public void initLeftList(List content) {
	ListIterator li = content.listIterator();
	while (li.hasNext()) {
	    listLeft.addElement(li.next());
	}
    }

    /**
     * initRightList
     * initialise the contents of the left list
     * @param content - the contents
     */
    public void initRightList(List content) {
	ListIterator li = content.listIterator();
	while (li.hasNext()) {
	    listRight.addElement(li.next());
	}
    }

    /**
     * addItemRightList
     * add an object to the rightList
     * @param item - the item to be added
     */
    public void addItemRightList(Object item) {
	if (!listRight.contains(item)) {
	    listRight.addElement(item);
	}
    }

    /**
     * addItemLeftList
     * add an object to the listLeft
     * @param item - the item to be added
     */
    public void addItemLeftList(Object item) {
	if (!listLeft.contains(item)) {
	    listLeft.addElement(item);
	}
    }

    /**
     * delItemLeftList
     * delete an object from the listLeft
     * @param item - the item to be added
     */
    public void delItemLeftList(Object item) {
	if (listLeft.contains(item)) {
	    listLeft.removeElement(item);
	}
    }
    
    /**
     * clearLeftList
     * empty left list
     */
    public void clearLeftList(){
    	listLeft.clear();
    }

    /**
     * delItemRightList
     * delete an object from the listRight
     * @param item - the item to be added
     */
    public void delItemRightList(Object item) {
	if (listRight.contains(item)) {
	    listRight.removeElement(item);
	}
    }

    /**
     * getContentRight
     * @return - List of content
     */
    public List getContentRight() {
	List ret = new ArrayList();
	for (int i = 0;i < listRight.getSize();i++) {
	    ret.add(listRight.get(i));
	}
	return ret;
    }

    private void jbnAddAllActionPerformed () {
	for (int i = 0;i < listLeft.getSize();i++) {
	    Object obj = listLeft.get(i);
	    if (!listRight.contains(obj)) {
		listRight.addElement(obj);
	    }
	}
    }
	
    private void jbnAddActionPerformed () {
	for (int i = 0;i < listLeft.getSize();i++) {
	    Object obj = listLeft.get(i);
	    if (jListLeft.isSelectedIndex(i) && !listRight.contains(obj)) {
		listRight.addElement(obj);
	    }
	}
    }	
	
    private void jbnDelAllActionPerformed () {
	int top = listRight.getSize() - 1;
	for (int i = top; i >= 0 ;i--) {
	    listRight.remove(i);
	}
    }

    private void jbnDelActionPerformed () {
	int top = listRight.getSize() - 1;
	for (int i = top; i >= 0;i--) {
	    if (jListRight.isSelectedIndex(i)) {
		listRight.remove(i);
	    }
	}
    }
}
