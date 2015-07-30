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
import javax.swing.event.*;

import jplan.images.ImageLoader; 
import jplan.general.Utility;

/**
 * Manual Class used to display the HTML OCL Manual
 */

public class Manual extends JFrame {
    private JEditorPane pane;
    private JTextField jtxtManURL; 
    private String oclEdPath;
    private String contentPage;

    /**
     * Create the HTML Editor Dialog Box : Add a url Text window
     * to allow domains to be loaded directly
     */
    public Manual (String strImageDir,String contentPage) {
	setTitle ("GIPO Help System");
	oclEdPath = System.getProperty("ocled.codebase");
	this.contentPage = contentPage;
	pane = new JEditorPane();
	pane.setEditable(false);
	getContentPane().add(new JScrollPane(pane),"Center");
	
	JPanel panel = new JPanel();
	panel.setLayout(new BorderLayout(4,4));
	JLabel urlLabel = new JLabel("URL: ",JLabel.RIGHT);
	panel.add(urlLabel, "West");
	jtxtManURL = new JTextField(32);
	jtxtManURL.setEditable(false); /* Weihong changed on 19/10/2001 */
	panel.add(jtxtManURL, "Center");
	ImageIcon ii = ImageLoader.getImageIcon(strImageDir, "Stop16.gif");
	JButton cmdClose = new JButton ("Close", ii);
	cmdClose.setMnemonic(KeyEvent.VK_L);
	cmdClose.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent evt) {	
		    setVisible (false);
		    dispose ();
		}
	}
		);
	panel.add(cmdClose,"East");

	getContentPane().add(panel,"South");

	jtxtManURL.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent evt) {
		    String url = jtxtManURL.getText();
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
	try {
	    pane.setPage("file:" + oclEdPath + File.separator +
			 "jplan" + File.separator + 
			 "docs" + File.separator +
			 contentPage);
	    jtxtManURL.setText("file:" + oclEdPath + File.separator + 
			       "jplan" + File.separator +
			       "docs" + File.separator + 
			       contentPage);
	} catch (IOException e) {
	    JOptionPane.showMessageDialog(pane,
					  "Unable to open Manual" +
					  "\n Not installed at "+
			      oclEdPath + File.separator +
					"jplan" + File.separator +
					"docs",
					  "GIPO Error",
					  JOptionPane.ERROR_MESSAGE);

	    this.dispose();
	    return;
	}
	pane.addHyperlinkListener(new ActivatedHyperlinkListener(this,pane));
	this.setSize(850,700);
	this.setVisible(true);
    }
    
    public void htmlDomAddText(String content) {
	pane.setContentType("text/html");
	pane.setText(content);
    }

}

    
