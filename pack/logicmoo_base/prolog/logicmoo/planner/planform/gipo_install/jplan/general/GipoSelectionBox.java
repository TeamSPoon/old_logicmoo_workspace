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

/**
 * GipoSelectionBox.java
 *
 *
 * Created: Mon Jun 10 09:56:18 2002
 *
 * @author W Zhao
 * @version
 */
import javax.swing.*;
import java.awt.*;
import java.util.List;
import java.util.*;
import java.awt.event.*;

public class GipoSelectionBox extends JDialog {
    private JLabel prompText;
    private List selectList;
    private Object returnObj;
    private JList inputList;

    /**
     * Gipo selection box
     * @param owner top level frame generating this dialog
     * @param title the title for the box
     * @param prompText prompt question for user
     * @param selectList document for textfield to enforce its own input rules
     */
    public GipoSelectionBox(Frame owner, String title, 
			String prompText, List selectList) { 
	super(owner);
	returnObj = null;
	setTitle(title);
	setModal(true);
	this.prompText = new JLabel(prompText);
	this.selectList = selectList;
	initComponents ();
	pack ();
	setPosition(owner);
    }
    

    /**
     * Initialisation
     */
    private void initComponents () {
	addWindowListener (new java.awt.event.WindowAdapter () {
	    public void windowClosing (java.awt.event.WindowEvent evt) {
		closeDialog ();
	    }
	}
			   );

	getContentPane ().setLayout (new java.awt.BorderLayout ());

	JToolBar northToolBar = new JToolBar ();
	northToolBar.setLayout (new FlowLayout ());
	northToolBar.setFloatable(false); 
	
	JButton jbn_OK = new JButton ();
	jbn_OK.setText ("   OK   ");
	jbn_OK.addActionListener (new java.awt.event.ActionListener () {
	    public void actionPerformed (java.awt.event.ActionEvent evt) {
		jbn_OKActionPerformed ();
	    }
	}
				  );
	northToolBar.add (jbn_OK);
	
	JButton jbn_Cancel = new JButton ();
	jbn_Cancel.setText ("  Cancel  ");
	jbn_Cancel.addActionListener (new java.awt.event.ActionListener () {
	    public void actionPerformed (java.awt.event.ActionEvent evt) {
		jbn_CancelActionPerformed ();
	    }
	}
				      );
	northToolBar.add (jbn_Cancel);

	JPanel topPane = new JPanel();
	topPane.setLayout(new FlowLayout());
	topPane.add(prompText);	

	prompText.setSize(200,80);

	JPanel centerPane = new JPanel();
	centerPane.setLayout(new GridLayout(1,1));
	
	inputList = new JList();
	DefaultListModel lmShapes = new DefaultListModel();
	inputList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
	ListIterator li = selectList.listIterator();
	while (li.hasNext()) {
	    Object obj = (Object)li.next();
	    lmShapes.addElement(obj);
	}
	inputList.setModel(lmShapes);
	inputList.setSelectedIndex(-1);
	inputList.addMouseListener(new MouseAdapter() {
	    public void mouseClicked(MouseEvent me) {
		returnObj = (Object)inputList.getSelectedValue();
	    }
	});
	centerPane.add(inputList);

	centerPane.setBorder (new javax.swing.border.BevelBorder(1));

	getContentPane ().add (northToolBar, "South");
	getContentPane ().add (topPane, "North");
	getContentPane ().add (centerPane, "Center");
    }

    /**
     * when the ok button is pressed, get the input text
     */
    private void jbn_OKActionPerformed (){
	closeDialog();
    }

    /**
     * cancel the box
     */
    private void jbn_CancelActionPerformed (){
	returnObj =  null;
	closeDialog();
    }

    /**
     * Returns the Selected Object
     * @return the Selected Object
     */
    public Object getSelectedObject(){
	return returnObj;
    }

    /**
     * Sets position of the input box
     * @param owner parent frame
     */
    private void setPosition(Frame owner){
	if (owner == null) {
	    return;
	}
	int X = (owner.getX() + (owner.getWidth() - getWidth())/2);
	int Y = (owner.getY() + (owner.getHeight() - getHeight())/2);
	if (owner.getWidth() < getWidth()) {
	    X = owner.getX();
	}
	if (owner.getHeight() < getHeight()) {
	    Y = owner.getY();
	}
	setLocation(X,Y);
    }

    /** 
     * close this dialog window
     */
    private void closeDialog() {
	setVisible (false);
	dispose ();
    }

    /**
     * factory method to create and display box;
     * provides default title
     * @param owner top level frame generating this dialog
     * @param prompText prompt question for user
     * @param selectionList
     * @return the selected Object
     */
    public static Object showSelectionBox(Frame owner,String prompText, List selectionList){
	GipoSelectionBox selectBox = 
	    new GipoSelectionBox(owner, "GIPO Selection",prompText, selectionList);
	selectBox.show();
	return selectBox.getSelectedObject();
    }

} // GipoSelectionBox
