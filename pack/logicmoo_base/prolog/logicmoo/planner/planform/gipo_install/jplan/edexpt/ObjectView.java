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

/**
 * ObjectView
 * This is the OCL expert Object editor
 * Part of the edexpt (Editor for Experts) Views
 * @author Ron Simpson
 * @version 0
 */

import javax.swing.*;
import javax.swing.event.*;
import java.util.List;
import java.util.ArrayList;
import java.util.ListIterator;
import java.util.Hashtable;

import jplan.top.OclEd;
import jplan.ocl.oclObject;
import jplan.general.Utility;

public class ObjectView extends JInternalFrame {
    private OclEd top;
    private List objects;
    // Interface components or thier Models
    private DefaultListModel lmObj;
    JList jlstObj;
    private Hashtable hashObjs; 
    JTextField jtxtSortName;
    JTextField jtxtName;
    //Used to associate object names with Sort names

    /**
     * Constructor
     * @param parent top level EdOcl reference
     * @param isModal boolean to determine if dialog is modal normally not
     */
    public ObjectView (OclEd parent,boolean isModal) {
	super("Object View (Expert)",true,true,true,true);
	objects = parent.curDomain.objects;
	initComponents();
	top = parent;
	try {
	    ObjectListDragSource dragSource = 
		new ObjectListDragSource(jlstObj);
	} catch (Exception e) {
	    Utility.debugPrintln("Object List exception " + e.toString());
	}
	pack();
	show();
    }
    
    /**
     * initComponents
     * Sets up the user interface
     */
    private void initComponents() {
	Box boxEd = Box.createHorizontalBox();
	Box boxObj = Box.createVerticalBox();
	lmObj = new DefaultListModel();
	// Fill the model;
	ListIterator liObjDecl = objects.listIterator();
        hashObjs = new Hashtable();
	while (liObjDecl.hasNext()) {
	    oclObject objDecl = (oclObject)liObjDecl.next();
	    String objSort = objDecl.getObjectSort();
	    List objList = objDecl.getObjectNames();
	    ListIterator liObjNames = objList.listIterator();
	    while (liObjNames.hasNext()) {
		String objID = (String)liObjNames.next();
		lmObj.addElement(objID);
		hashObjs.put(objID,objSort);
	    }
	}
	jlstObj = new JList(lmObj);
	jlstObj.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
	ListSelectionListener listSelListenerObjs = 
	    new ListSelectionListener() {
		    public void valueChanged(ListSelectionEvent lsEvent) {
			boolean adj = lsEvent.getValueIsAdjusting();
			if (!adj) {
			    String sel = jlstObj.getSelectedValue().toString();
			    jtxtSortName.setText((String)hashObjs.get(sel));
			}
		    }
		};
	jlstObj.addListSelectionListener(listSelListenerObjs);
	JScrollPane scrollPaneObjs = new JScrollPane(jlstObj);
	boxObj.add(scrollPaneObjs);
	// Now some details of the object
	Box boxObjDetails = Box.createHorizontalBox();
	JLabel jlblSortName = new JLabel("Sort Name");
	jtxtSortName = new JTextField(15);
	boxObjDetails.add(jlblSortName);
	boxObjDetails.add(jtxtSortName);
	boxObj.add(Box.createVerticalStrut(5));
	boxObj.add(boxObjDetails);
	Box boxAdd = Box.createHorizontalBox();
	JLabel jlblAdd = new JLabel("Object Name");
	jtxtName = new JTextField(15);
	JButton cmdAdd = new JButton("Add Object");
	cmdAdd.addActionListener (new java.awt.event.ActionListener () {
		public void actionPerformed (java.awt.event.ActionEvent evt) {
 		    addCommand(evt);
		}
	    }
				  );
	boxAdd.add(jlblAdd);
	boxAdd.add(jtxtName);
	boxAdd.add(cmdAdd);
	boxObj.add(Box.createVerticalStrut(5));
	boxObj.add(boxAdd);
	boxEd.add(boxObj);
	getContentPane().add(boxEd);
    }

    public void addCommand(java.awt.event.ActionEvent evt) {
	lmObj.addElement(jtxtName.getText());
	hashObjs.put(jtxtName.getText(),"none");
    }


		
}
