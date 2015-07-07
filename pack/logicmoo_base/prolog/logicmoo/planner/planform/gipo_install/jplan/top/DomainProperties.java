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
/**
 * DomainProperty.java
 *
 *
 * Created: Mon Oct 15 10:46:37 2001
 *
 * @author W Zhao
 * @version
 */
/* Revision History
 * Ron 8/11/02 - allow cancel of new domains without forcing
 *               user to provide a domain name
 *       Added new flag for new domains and new constructor
 */
import javax.swing.*;
import java.awt.*;
import jplan.ocl.oclDomain;

import jplan.general.Utility;

public class DomainProperties extends JDialog {
	private OclEd parent;
	private JPanel mainPanel;
	private JLabel jLabel_domainName,
		jLabel_author,
		jLabel_description,
		jLabel_institute,
		jLabel_created,
		jLabel_modified;
	private JTextField jTextField_domainName,
		jTextField_author,
		jTextField_institute,
		jTextField_created,
		jTextField_modified;
	private JEditorPane jTextArea_description;
	private JScrollPane jscrTextDesc;

	private oclDomain curDomain;
	// 8/11/02 
	private boolean newDomain = false;
	// 5/5/03 Added Hierarchical check box
	private JCheckBox checkHierarchical;
	//	 9/5/05 Added Oclplus check box
	private JCheckBox checkOclPlus;
	
	/*
	 * Constructor
	 * 	Use this version with an existing domain to edit properties
	 * @param parent - The OclEd top frame
	 * @param curDomain - the domain being edited
	 */
	public DomainProperties(OclEd parent, oclDomain curDomain) {
		this(parent, curDomain, false);
	}

	/*
	 * Constructor
	 * @param parent - The OclEd top frame
	 * @param curDomain - the domain being edited
	 * @param isNew - flag to indicate if this is a new domain
	 */
	public DomainProperties(OclEd parent, oclDomain curDomain, boolean isNew) {
		super(parent, "Domain Properties");
		this.parent = parent;
		this.curDomain = curDomain;
		setModal(true);
		addWindowListener(new java.awt.event.WindowAdapter() {
			public void windowClosing(java.awt.event.WindowEvent evt) {
				exitDiag();
			}
		});
		initComponents();
		pack();
		if (parent.oclplus) {
			setSize(450, 490);
		} else {
			setSize(450, 450);
		}
		newDomain = isNew;
	}


	/*
	 * initiate Components
	 *
	 */
	private void initComponents() {
		mainPanel = new javax.swing.JPanel();

		jLabel_domainName = new javax.swing.JLabel("Domain Name");
		jTextField_domainName = new javax.swing.JTextField();
		jTextField_domainName.setDocument(
			new jplan.general.OCLIdentifierDocument());
		/* WZ 8/5/02 */
		jTextField_domainName.setText(curDomain.getName());

		jLabel_author = new javax.swing.JLabel("Author");
		jTextField_author = new javax.swing.JTextField();
		if (curDomain.getAuthor() == null
			|| curDomain.getAuthor().length() == 0
			|| curDomain.getAuthor().equals("none")) {
			String loginName = System.getProperty("user.name");
			jTextField_author.setText(loginName);
		} else {
			jTextField_author.setText(curDomain.getAuthor());
		}
		jLabel_description = new javax.swing.JLabel("Description");
		jTextArea_description = new javax.swing.JEditorPane();
		jTextArea_description.setText(curDomain.getDomDesc());
		jscrTextDesc = new javax.swing.JScrollPane(jTextArea_description);

		jLabel_institute = new javax.swing.JLabel("Institute");
		jTextField_institute = new javax.swing.JTextField();
		if (curDomain.getInstitution() == null
			|| curDomain.getInstitution().length() == 0
			|| curDomain.getInstitution().equals("null")) {
			jTextField_institute.setText("University of Huddersfield");
		} else {
			jTextField_institute.setText(curDomain.getInstitution());
		}

		jLabel_created = new javax.swing.JLabel("Created");
		jTextField_created = new javax.swing.JTextField();
		jTextField_created.setEnabled(false);
		jTextField_created.setBackground(new Color(255, 255, 241));
		/* WZ 9/5/02 */
		jTextField_created.setText(curDomain.getDateCreated());

		jLabel_modified = new javax.swing.JLabel("Modified");
		jTextField_modified = new javax.swing.JTextField();
		jTextField_modified.setEnabled(false);
		jTextField_modified.setBackground(new Color(255, 255, 241));
		/* WZ 9/5/02 */
		jTextField_modified.setText(curDomain.getDateModified());

		Label label1 = new java.awt.Label();
		Label label2 = new java.awt.Label();
		Label label3 = new java.awt.Label();

		addWindowListener(new java.awt.event.WindowAdapter() {
			public void windowClosing(java.awt.event.WindowEvent evt) {
				closeDialog();
			}
		});

		checkHierarchical = new JCheckBox("Domain is Hierarchical",
					curDomain.isHierarchical());
		checkOclPlus = new JCheckBox("Domain has Durative actions",
				curDomain.isOclPlus());
		
		mainPanel.setLayout(new javax.swing.BoxLayout(mainPanel, 1));


		mainPanel.add(jLabel_domainName);
		mainPanel.add(jTextField_domainName);
		mainPanel.add(jLabel_author);
		mainPanel.add(jTextField_author);
		mainPanel.add(jLabel_institute);
		mainPanel.add(jTextField_institute);
		mainPanel.add(jLabel_description);
		jscrTextDesc.setPreferredSize(new java.awt.Dimension(137, 117));
		jscrTextDesc.setMinimumSize(new java.awt.Dimension(137, 117));

		mainPanel.add(jscrTextDesc);
		mainPanel.add(checkHierarchical);
		if (parent.oclplus) {
			mainPanel.add(checkOclPlus);
		}
		if (!curDomain.lifeHistoryFileName.equals("none")) {
			JLabel lhLabel = new JLabel("Life History Graphics file is : " + curDomain.lifeHistoryFileName);
			mainPanel.add(lhLabel);
		}
		mainPanel.add(jLabel_created);
		mainPanel.add(jTextField_created);
		mainPanel.add(jLabel_modified);
		mainPanel.add(jTextField_modified);

		getContentPane().add(mainPanel, java.awt.BorderLayout.CENTER);
		getContentPane().add(label1, java.awt.BorderLayout.WEST);
		getContentPane().add(label2, java.awt.BorderLayout.EAST);
		getContentPane().add(label3, java.awt.BorderLayout.NORTH);

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
		if (!newDomain && (jTextField_domainName.getText().length() == 0
			|| jTextField_domainName.getText().equals("none"))) {
			JOptionPane.showMessageDialog(
				this,
				"You must atleast provide a name for the domain.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}
		closeDialog();
	}

	/*
	 * jbn_OKActionPerformed
	 * close Window
	 */
	private void jbn_OKActionPerformed() {
		//save info first - write to the domain, mark the dirtyFlag
		if (jTextField_domainName.getText().length() == 0
			|| jTextField_domainName.getText().equals("none")) {
			JOptionPane.showMessageDialog(
				this,
				"You must provide a name for the domain.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}

		curDomain.setName(jTextField_domainName.getText());
		curDomain.setAuthor(jTextField_author.getText());

		String s, tmpStr;
		tmpStr = jTextArea_description.getText().trim();
		curDomain.clearDomDesc();
		while (true) {
			int position = tmpStr.indexOf("\n");
			if (position == -1) {
				s = tmpStr;
				tmpStr = "";
				//Utility.debugPrintln(s);
				curDomain.addDomDescLine(s);
				break;
			} else {
				s = tmpStr.substring(0, position);
				tmpStr = tmpStr.substring(position + 1);
				//Ron was 2 ? How about dos \r\n
				//Utility.debugPrintln(s);
				curDomain.addDomDescLine(s);
			}
		}
		// Ron 5/5/03
		curDomain.setHierarchical(checkHierarchical.isSelected());
		if (parent.oclplus) {
			curDomain.setOclPlus(checkOclPlus.isSelected());
		}
		if (checkHierarchical.isSelected()) {
			parent.updateMenuBar(OclEd.OCLHIER);
		} else if (checkOclPlus.isSelected()) {
			parent.updateMenuBar(OclEd.OCLPLUS);
		} else {
			parent.updateMenuBar(OclEd.OCLFLAT);
		}
		curDomain.setInstitution(jTextField_institute.getText());
		curDomain.setDateCreated(jTextField_created.getText());
		curDomain.setDateModified(jTextField_modified.getText());

		parent.flagDirty = true; /* Weihong added on 19/10/2001 */

		closeDialog();
	}

	/**
	 * exitDiag
	 * called when the window default close down button is used
	 */
	private void exitDiag() {
		if (!newDomain && (jTextField_domainName.getText().length() == 0
			|| jTextField_domainName.getText().equals("none"))) {
			JOptionPane.showMessageDialog(
				this,
				"You should atleast provide a name for the domain.",
				"GIPO Error",
				JOptionPane.ERROR_MESSAGE,
				null);
			return;
		}
		closeDialog();
	}

	/*
	 * closeDialog
	 * close Window
	 */
	private void closeDialog() {
		setVisible(false);
		dispose();
	}



} // DomainProperty
