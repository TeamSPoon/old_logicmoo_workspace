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

package jplan.graphics.gTool.Windows;

/**
 * Author: Weihong Zhao
 * 05/03/2001
 */


import javax.swing.JFrame;

import jplan.graphics.gTool.Graphics.vShape;
import jplan.graphics.gTool.Graphics.vLink;
import jplan.graphics.JGraphCanvas;

import java.awt.Graphics;



/**
 * to show vShapes property mainly label
 */
public class vShapePropertyWindow extends javax.swing.JDialog {

    /**
     * creates default vShapePropertyWindow with given parent
     * @param parentObj parent JGraphCanvas
     */
    public vShapePropertyWindow(JGraphCanvas parentObj) {
	super();
	initComponents ();
	pack ();
	parent = parentObj;
    }

    /**
     * initialisation
     * 
     */
    private void initComponents () {
	setBackground (new java.awt.Color (114, 159, 255));
	addWindowListener (new java.awt.event.WindowAdapter () {
	    public void windowClosing (java.awt.event.WindowEvent evt) {
		closeDialog ();
	    }
	}
			   );
	getContentPane ().setLayout (new java.awt.BorderLayout ());
	
	vShapeProperty = new javax.swing.JPanel ();
	vShapeProperty.setToolTipText ("vShape Property");
	vShapeProperty.setLayout (new java.awt.FlowLayout ());
	
	jLabel1 = new javax.swing.JLabel ();
	jLabel1.setText ("Name");
	vShapeProperty.add (jLabel1);
	
	shapeName = new javax.swing.JTextField ("", 20);
	vShapeProperty.add (shapeName);
	
	getContentPane ().add (vShapeProperty, "Center");
	
	jPanel1 = new javax.swing.JPanel ();
	jPanel1.setLayout (new java.awt.FlowLayout ());
	
	jbn_OK = new javax.swing.JButton ();
	jbn_OK.setText ("OK");
	jbn_OK.addActionListener (new java.awt.event.ActionListener () {
	    public void actionPerformed (java.awt.event.ActionEvent evt) {
		jbn_OKActionPerformed ();
	    }
	}
				  );
	jPanel1.add (jbn_OK);
	
	jbn_Cancel = new javax.swing.JButton ();
	jbn_Cancel.setText ("Cancel");
	jbn_Cancel.addActionListener (new java.awt.event.ActionListener () {
	    public void actionPerformed (java.awt.event.ActionEvent evt) {
		jbn_CancelActionPerformed ();
	    }
	}
				      );
	jPanel1.add (jbn_Cancel);
	
	getContentPane ().add (jPanel1, "South");
	
    }
    
    /**
     * pass the vshape
     * 
     */
    public void setShape(vShape parent) {
	vs = parent;
	shapeName.setText(vs.getLabel());
    }

    /**
     * change the label
     * 
     */
    private void jbn_OKActionPerformed () {
	jbn_ApplyActionPerformed();
	jbn_CancelActionPerformed();
    }
    
    /**
     * reset the label of the vShape
     * 
     */
    private void jbn_ApplyActionPerformed () {
	vs.setLabel(shapeName.getText());
	parent.repaint();
    }

    /**
     * close window
     * 
     */
    private void jbn_CancelActionPerformed () {
	setVisible (false);
	dispose ();
    }

    /**
     * close window
     * 
     */
    private void closeDialog() {
	setVisible (false);
	dispose ();
    }

    // Variables declaration
    private javax.swing.JPanel jPanel1;
    private javax.swing.JButton jbn_OK;
    private javax.swing.JButton jbn_Apply;
    private javax.swing.JButton jbn_Cancel;
    private javax.swing.JPanel vShapeProperty;
    private javax.swing.JTextField shapeName;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JPanel vLinkProperty;

    private vShape vs = null;
    private JGraphCanvas parent;

}
