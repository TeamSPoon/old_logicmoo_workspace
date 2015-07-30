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

import se.sics.jasper.*;
import jplan.general.Utility;

/**
 * HaltControl - this is a little popup to allow a background process
 * such as a Sicstus planner to be stopped by the user 
 * @author Ron Simpson
 */

public class HaltControl extends JInternalFrame { 
    String kill = null;
    JFrame parent = null;
    File killFile = null;
    String oclPath = null;
    PToolsControl control = null;

    /**
     * Create the window with a string to identify the process
     * and a halt button
     * @param parent - the parent frame
     * @param process description of the process to be controlled
     * @param kill - kill file name
     */
    public HaltControl (OclEd parent,String process,String kill) {
	super("GIPO Process Control",false,false,false,false);
	this.parent = parent;
	this.kill = kill;
	oclPath = parent.strOCLPath;
	try {
	    killFile = new File(oclPath + File.separator + 
				"tmp" + File.separator + 
				kill);
	    if (killFile.exists()) {
		killFile.delete();
	    }
	} catch (Exception e) { 
	    JOptionPane.showMessageDialog(parent,
		      "Cannot create kill signal." +
		      "\nConsult your local expert.\n" + e.toString(),
		      "GIPO Error",
		       JOptionPane.ERROR_MESSAGE,
		       null);
	} 
	JLabel ident = new JLabel(process);
	JButton cmdStop = new JButton("Stop");
	getContentPane().add(cmdStop, "South");

	getContentPane().add(ident,"North");

	cmdStop.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent evt) {
		    haltProcess();
		}
	    });
	this.setSize(300,100);
	//this.pack();
	this.show();

    }

    /**
     * Create the window with a string to identify the process
     * and a halt button - This version deals with external 
     * non prolog processes
     * @param parent - the parent frame
     * @param process -  description of the process to be controlled
     * @param cont - kill file name
     */
    public HaltControl (OclEd parent,String process,PToolsControl cont) {
	super("GIPO Process Control",false,false,false,false);
	this.parent = parent;
	this.control = cont;
	JLabel ident = new JLabel(process);
	JButton cmdStop = new JButton("Stop");
	getContentPane().add(cmdStop, "South");

	getContentPane().add(ident,"North");

	cmdStop.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent evt) {
		    haltExternalProcess();
		}
	    });
	this.setSize(300,100);
	this.show();

    }

    private void haltExternalProcess() {
	Process proc = control.getProc();
	proc.destroy();
	setVisible(false);
	dispose();
    }

    private void haltProcess() {
	try {
	    File killFile = (new File(oclPath +
				      File.separator + 
				      "tmp" + 
				      File.separator + 
				      kill));
	    killFile.createNewFile();
	    
	} catch (Exception e) {
	    JOptionPane.showMessageDialog(parent,
	             "Cannot create kill signal." +
		      "\nConsult your local expert.\n" + e.toString(),
		      "GIPO Error",
		       JOptionPane.ERROR_MESSAGE,
		       null);
	}
	setVisible(false);
	dispose();
    }
}

    
