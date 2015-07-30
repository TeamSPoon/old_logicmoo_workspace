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

package jplan.top;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import javax.swing.*;
import javax.swing.text.*;

import jplan.ocl.*;
/**
 * HTMLDomView Class used to display a domain model in HTML
 * This view is not editable
 */

public class HTMLDomView extends JFrame {
    private JEditorPane pane;
    private JTextField textField; 

    /**
     * Create the HTML Editor Dialog Box : Add a url Text window
     * to allow domains to be loaded directly
     */
    public HTMLDomView () {
	super ("OCL HTML View");
	pane = new JEditorPane();
	pane.setEditable(false);
	getContentPane().add(new JScrollPane(pane),"Center");
	
	JPanel panel = new JPanel();
	panel.setLayout(new BorderLayout(4,4));
	JLabel urlLabel = new JLabel("URL: ",JLabel.RIGHT);
	panel.add(urlLabel, "West");
	textField = new JTextField(32);
	panel.add(textField, "Center");

	getContentPane().add(panel,"South");

	textField.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent evt) {
		    String url = textField.getText();
		    try {
			pane.setPage(url);
		    } catch (IOException e) {
			JOptionPane.showMessageDialog(pane, new String[] {
			    "Unable to open file",
			    url
			}, 
		        "File Open Error",
			JOptionPane.ERROR_MESSAGE);
		    }
		}
	    });
	this.setSize(500,400);
	this.setVisible(true);
    }
    
    public void htmlDomAddText(String content) {
	pane.setContentType("text/html");
	pane.setText(content);
    }
    
}

    
