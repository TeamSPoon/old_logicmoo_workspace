/*
 * GIPO COPYRIGHT NOTICE, LICENSE AND DISCLAIMER.
 *
 * Copyright 2001 - 2005 by R.M.Simpson W.Zhao T.L.McCLuskey D Liu D. Kitchin
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


import javax.swing.*;

import java.awt.*;
import java.io.*;


/**
 * Preferences.java
 * Used to store system preferences
 * At the Moment only the domains directory
 *
 * Created: Mon 11th July 2005
 *
 * @author Ron
 * @version
 */

public class Preferences extends JDialog{
	
	private JPanel mainPanel;
	private JLabel lblDirectory;
	private JTextField txtDirectory;
	
	OclEd parent;

	
	/**
	 * Constructor
	 */
	public Preferences(OclEd parent) {
		super(parent,"Preferences");
		this.parent = parent;
		setModal(true);
		addWindowListener(new java.awt.event.WindowAdapter() {
			public void windowClosing(java.awt.event.WindowEvent evt) {
				exitDiag();
			}
		});
		initComponents();
		setLocationRelativeTo(parent);
		pack();
		//setSize(450, 450);
	}


	/**
	 * initComponents
	 * set up the dialog
	 */
	private void initComponents() {
		mainPanel = new javax.swing.JPanel();

		lblDirectory = new JLabel("Domains Directory");
		txtDirectory = new JTextField(30);
		txtDirectory.setText(parent.strDomainsDir);

		addWindowListener(new java.awt.event.WindowAdapter() {
			public void windowClosing(java.awt.event.WindowEvent evt) {
				closeDialog();
			}
		});
		
		//mainPanel.setLayout(new BorderLayout());
		

		mainPanel.add(lblDirectory);
		mainPanel.add(txtDirectory);
		

		getContentPane().add(mainPanel, BorderLayout.CENTER);
		

		JPanel northToolBar = new javax.swing.JPanel();
		northToolBar.setLayout(new java.awt.FlowLayout());

		JButton jbn_OK = new javax.swing.JButton();
		jbn_OK.setText("   OK   ");
		jbn_OK.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jbn_OKActionPerformed();
			}
		});
		northToolBar.add(jbn_OK);
		JButton jbn_Cancel = new javax.swing.JButton();
		jbn_Cancel.setText("   Cancel   ");
		jbn_Cancel.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jbn_CancelActionPerformed();
			}
		});
		northToolBar.add(jbn_Cancel);

		getContentPane().add(northToolBar, "South");
	}

	/*
	 * jbn_CancelActionPerformed
	 * close Window only
	 */
	private void jbn_CancelActionPerformed() {
		closeDialog();
	}

	/*
	 * jbn_OKActionPerformed
	 * close Window
	 */
	private void jbn_OKActionPerformed() {
		if (txtDirectory.getText().length() == 0) {
			JOptionPane.showMessageDialog(
				this,
				"You must provide a valid directory name.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}
		File dir = new File(txtDirectory.getText());
		if (dir.exists() && dir.canRead() && dir.canWrite()) {
			if (!txtDirectory.getText().equals(parent.strDomainsDir)) {
				parent.strDomainsDir = txtDirectory.getText();
				parent.props.setProperty("ocled.domains",parent.strDomainsDir);
				try {
					parent.props.store(new BufferedOutputStream(new FileOutputStream( 
						  parent.strCodeBase +  
						  File.separator +
						  "jplan" + File.separator + 
						  "gipo.properties")),"Working directory for domain definitions");
				} catch (IOException e){
					JOptionPane.showMessageDialog(
							this,
							"Cannot save the preferences file",
							"GIPO Error",
							JOptionPane.ERROR_MESSAGE,
							null);
						return;
				}
			}
		} else {
			JOptionPane.showMessageDialog(
					this,
					"The directory given is not readable/writable.",
					"GIPO Error",
					JOptionPane.ERROR_MESSAGE,
					null);
				return;
		}
		closeDialog();
	}

	/**
	 * exitDiag
	 * called when the window default close down button is used
	 */
	private void exitDiag() {
		closeDialog();
	}

	/**
	 * closeDialog
	 * close Window
	 */
	private void closeDialog() {
		setVisible(false);
		dispose();
	}


}
