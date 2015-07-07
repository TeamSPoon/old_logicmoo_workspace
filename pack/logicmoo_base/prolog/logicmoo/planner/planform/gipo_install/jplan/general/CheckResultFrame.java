/*
 * GIPO COPYRIGHT NOTICE, LICENSE AND DISCLAIMER.
 *
 * Copyright 2001 - 2003 by R.M.Simpson W.Zhao T.L.McCLuskey D.Liu D. Kitchin
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

/**
 * CheckResultFrame
 * This is a frame for OCL check messages  
 * @author Donghong Liu
 * @date 25/10/02
 * @version 0
 */
package jplan.general;


import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import java.io.StringWriter;
import java.io.PrintWriter;
import java.util.*;

import jplan.ocl.*;
import jplan.top.OclEd;
import jplan.images.ImageLoader; 
import jplan.general.Utility;
import jplan.general.UnderlineHighlighter;
import javax.swing.text.BadLocationException;

public class CheckResultFrame extends GipoInternalFrame {
    private OclEd top;
    private oclDomain curDomain;
    private JTextArea text;
    private JScrollPane jscrText;

    public CheckResultFrame (OclEd top,String mssgs,String ttlName) {
	super(top);
    setTitle("GIPO Information");
    this.top = top;
	text = new JTextArea();
  	initComponents();
	printMss(mssgs,ttlName);
	text.select(0,0);
	text.setEditable( false );
    setPreferredSize(new Dimension(600, 600) );	
	pack ();
	setVisible(true);
    }
    
   private void initComponents() {
   	
    text = new JTextArea();
	JScrollPane jscrText = new JScrollPane( text );
	getContentPane ().add (jscrText, "Center");
	jscrText = new JScrollPane( text );
	getContentPane ().add (jscrText);
		JPanel panel = new JPanel();
	panel.setLayout(new BorderLayout(4,4));
	JLabel urlLabel = new JLabel("Domain File: ",JLabel.RIGHT);
	panel.add(urlLabel, "West");
	JTextField jtxtDomain = new JTextField(32);	
	if (top.curDomain != null) {
	    jtxtDomain.setText(top.curDomain.getName());
	} else {
	    jtxtDomain.setText("none");
	}
	jtxtDomain.setEditable(false);
	panel.add(jtxtDomain, "Center");
	ImageIcon ii = ImageLoader.getImageIcon(top.strImageDir, "Stop16.gif");
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

    }

	//donghong add
	public static void showMessages(OclEd top, String strMssgs, String ttlName) {

		CheckResultFrame cf = new CheckResultFrame(top, strMssgs,ttlName);
		top.desktop.add(cf);
		top.deskManager.activateFrame(cf);
	}
	
	public static void showMessages(OclEd top, java.util.List mssgs, String ttlName) {
		String strMssgs = "";
		ListIterator li = mssgs.listIterator();
		while(li.hasNext()) {
			strMssgs = strMssgs.concat((String)li.next() + "\n");
		}
		strMssgs = strMssgs.concat("\n");

		CheckResultFrame cf = new CheckResultFrame(top, strMssgs, ttlName);
		top.desktop.add(cf);
		top.deskManager.activateFrame(cf);
	}
    private void printMss(String mssgs,String ttlName) {
    	
    	if (top.curDomain == null ) {
	    JOptionPane query = 
		new JOptionPane("No GIPO Domain Currently Exists",
				JOptionPane.ERROR_MESSAGE,
				JOptionPane.DEFAULT_OPTION);
	    JDialog dia = query.createDialog(top,"GIPO Error");
	    dia.show();
	    } else {
 	    String txt = "";
 	    text.setText(mssgs);
        setTitle(ttlName);
    	}
    }
}
