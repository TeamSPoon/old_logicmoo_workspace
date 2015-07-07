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

public class SequenceList extends JPanel {
    DefaultListModel listRight;
    JList jListRight;
    JButton jbnUp;
    JButton jbnDown;
    JButton jbnDel;
    JButton jbnDelAll;


    public SequenceList() {
	listRight = new DefaultListModel();
	jListRight = new JList(listRight);
	jListRight.setFixedCellWidth(100);
	jbnUp =      new JButton("      Up       ");
	jbnDown =    new JButton("      Down     ");
	jbnDelAll = new JButton("  Delete All   ");
	jbnDel =    new JButton("    Delete     ");
	jbnUp.addActionListener (new java.awt.event.ActionListener () {
	    public void actionPerformed (java.awt.event.ActionEvent evt) {
		jbnUpActionPerformed ();
	    }
	}
				      );
	jbnDown.addActionListener (new java.awt.event.ActionListener () {
	    public void actionPerformed (java.awt.event.ActionEvent evt) {
		jbnDownActionPerformed ();
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
	JPanel buttonPane = new JPanel();
	JPanel rightPane = new JPanel();
	JScrollPane scrRight = new JScrollPane(jListRight);
	rightPane.add(scrRight);
	buttonPane.setLayout(new BoxLayout(buttonPane,BoxLayout.Y_AXIS));
	buttonPane.add(jbnDown);
	buttonPane.add(jbnUp);
	buttonPane.add(jbnDel);
	buttonPane.add(jbnDelAll);
	add(buttonPane);
	add(rightPane);
	
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

    private void jbnUpActionPerformed () {
	int top = listRight.getSize();
	for (int i = 1;i < top ;i++) {
	    Object obj = listRight.get(i);
	    if (jListRight.isSelectedIndex(i)) {
		listRight.remove(i);
		listRight.insertElementAt(obj,i-1);
		jListRight.setSelectedIndex(i-1);
		return;
	    }
	}
    }
	
    private void jbnDownActionPerformed () {
	int top = listRight.getSize() - 1;
	for (int i = top -1;i >= 0 ;i--) {
	    Object obj = listRight.get(i);
	    if (jListRight.isSelectedIndex(i)) {
		listRight.remove(i);
		listRight.insertElementAt(obj,i+1);
		jListRight.setSelectedIndex(i+1);
		return;
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
