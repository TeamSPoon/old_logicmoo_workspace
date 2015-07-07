/*
 * GIPO COPYRIGHT NOTICE, LICENSE AND DISCLAIMER.
 * 
 * Copyright 2001 by R.M.Simpson W.Zhao T.L.McCLuskey D Lui D. Kitchin
 * 
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both the
 * copyright notice and this permission notice and warranty disclaimer appear in
 * supporting documentation, and that the names of the authors or their
 * employers not be used in advertising or publicity pertaining to distribution
 * of the software without specific, written prior permission.
 * 
 * The authors and their employers disclaim all warranties with regard to this
 * software, including all implied warranties of merchantability and fitness. In
 * no event shall the authors or their employers be liable for any special,
 * indirect or consequential damages or any damages whatsoever resulting from
 * loss of use, data or profits, whether in an action of contract, negligence or
 * other tortious action, arising out of or in connection with the use or
 * performance of this software.
 */
 
 
package jplan.tools.lifeHist;

import java.awt.*;
import java.io.*;

import javax.swing.*;

import jplan.top.OclEd;
import jplan.general.*;

import org.jgraph.graph.*;

/**
 * @author ron
 * Created on 14-Jan-2005
 *
 * Gives a preview view of Library patterns and
 * Provides help text
 */
public class LibraryItemView extends GipoInternalFrame {
	OclEd top = null;
	PalletManager parent = null;
	String graphName = null;
	String oclEdPath = null; // Path to parent of domains directory
	
	
	public LibraryItemView(OclEd top,PalletManager parent,String itemName,
		DefaultGraphModel model,LHGraph tempGraph) {
		this.top = top;
		this.parent = parent;
		oclEdPath = top.strOCLPath;
		graphName= itemName;
		initComponents(model,tempGraph);
		pack();
		setPosition(top);
	}
	
	private void initComponents(DefaultGraphModel model,LHGraph tempGraph){
		setTitle("Library Item Viewer");
		getContentPane().setLayout(new java.awt.BorderLayout());
		// Add an OK/Close Button
		JToolBar southToolBar = new JToolBar();
		southToolBar.setLayout(new FlowLayout());
		JButton cmdOK = new JButton();
		cmdOK.setText(" OK ");
		cmdOK.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				setVisible(false);
				dispose();
			}
		});
		
		southToolBar.add(cmdOK);
		getContentPane().add(cmdOK,"South");
		LHGraph graph = new LHGraph(new DefaultGraphModel());
		parent.insertLibraryItem(graph,model,tempGraph,false);
		JEditorPane description = new JEditorPane();
		description.setContentType("text/html");
		description.setEditable(false);
		setDescriptionText(description,graphName);
		description.setPreferredSize(new Dimension(300, 200));
		JSplitPane contentArea = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,new JScrollPane(description),new JScrollPane(graph));
		getContentPane().add(contentArea,"Center");
	}
	/**
	 * Sets position of the input box
	 * 
	 * @param owner -
	 *            parent frame
	 */
	protected void setPosition(Frame owner) {
		if (owner == null) {
			return;
		}
		int X = (owner.getX() + ((owner.getWidth() - getWidth()) / 2));
		int Y = (owner.getY() + ((owner.getHeight() - getHeight()) / 2));
		if (owner.getWidth() < getWidth()) {
			X = owner.getX();
		}
		if (owner.getHeight() < getHeight()) {
			Y = owner.getY();
		}
		setLocation(X, Y);
	}
	
	/**
	 * setDescriptionText find the description text for the selected pattern
	 * 
	 * @param description - the editor pane to populate
	 * @param strPat - the library item name
	 */
	private void setDescriptionText(JEditorPane description, String strPat) {
		if (description == null)
			return;
		String contentPage = strPat + ".html";
		boolean notfound = false;
		Utility.debugPrintln("patterns", "URL = " + oclEdPath
				+ "/domains/library/" + contentPage);
		try {
			description.setPage("file:" + oclEdPath + File.separator + "domains"
					+ File.separator + "library"
					+ File.separator + contentPage);
		} catch (java.io.IOException e) {
			notfound = true;
			Utility.debugPrintln("patterns", "Cannot Find ");
		}
		if (notfound)
			try {
				description.setPage("file:" + oclEdPath + File.separator
						+ "domains" + File.separator + "library"
						+ File.separator + "Error.html");
			} catch (java.io.IOException e) {
				Utility.debugPrintln("patterns", "Cannot Find Error"
						+ e.toString());
			}
	}
}
