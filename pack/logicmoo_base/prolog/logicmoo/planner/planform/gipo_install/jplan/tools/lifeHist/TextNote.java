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
 *
 * Created on 09-Mar-2005
 *
 * @author Ron
 */
package jplan.tools.lifeHist;

import javax.swing.*;
import javax.swing.text.*;
import javax.swing.text.html.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;


import org.jgraph.graph.AttributeMap;
import org.jgraph.graph.DefaultGraphCell;
import org.jgraph.graph.GraphConstants;


import jplan.general.Utility;

/**
 *
 * TextNote
 * The TextNote class allows simple text notes not attached to any node 
 * to be used to annotate diagrams
 */
public class TextNote extends DefaultGraphCell {
	
	protected transient JDialog propertyDlg;
	protected transient JEditorPane description;
	
	public TextNote(){
		this(null);
	}
	public TextNote(Object userObject){
		super(userObject);
	}
	public void setText(String text){
		LHUserObject uObj = (LHUserObject)this.getUserObject();
		uObj.putProperty("label",text);
		super.setUserObject(uObj);
	}
	
	public String getText(){
		return (String)((LHUserObject)getUserObject()).getProperty("label");
	}
	
	protected void showTextDialog(final LHGraph graph) {
		Frame frame = (Frame) SwingUtilities.windowForComponent(graph);
		if (frame != null && propertyDlg == null) {
			propertyDlg = new JDialog(frame, "", false);
			Container fContentPane = propertyDlg.getContentPane();
			fContentPane.setLayout(new BorderLayout());
			JMenuBar menuBar = new JMenuBar();
			propertyDlg.setJMenuBar(menuBar);
		    JMenu menu = new JMenu("Edit");
		    menuBar.add(menu);
		    JMenuItem menuItem;
		    
		    
			description = new JEditorPane();
			description.setContentType("text/html");
			description.setEditorKit(new EnlargedHTMLEditorKit());
			description.setPreferredSize(new Dimension(300, 200));
			description.setEditable(true);
			JScrollPane scrollpane = new JScrollPane(description);
			fContentPane.add(BorderLayout.CENTER, scrollpane);
			Action headingAction = locate(description, "Heading");
		    menuItem = menu.add(headingAction);
		    Action breakAction = locate(description, "InsertPre");
		    menuItem = menu.add(breakAction);
		    menuItem.setText("Formatted text");
		    Action redAction = locate(description, "Property Description");
		    menuItem = menu.add(redAction);
			JButton okButton = new JButton("OK");
			JButton cancelButton = new JButton("Close");
			JButton applyButton =
				new JButton("Apply");
			JPanel buttonPanel = new JPanel();
			buttonPanel.add(okButton);
			buttonPanel.add(cancelButton);
			buttonPanel.add(applyButton);
			fContentPane.add(BorderLayout.SOUTH, buttonPanel);
			applyButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					apply(graph);
				}
			});
			okButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					apply(graph);
					propertyDlg.dispose();
				}
			});
			cancelButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					propertyDlg.dispose();
				}
			});
			propertyDlg.setSize(new Dimension(300, 300));
			propertyDlg.setLocationRelativeTo(frame);
		}
		LHUserObject uObj = (LHUserObject)getUserObject();
		description.setText(uObj.getLabel((DefaultGraphCell)this));
		propertyDlg.setTitle("Comment Text");
		propertyDlg.show();
	}
	protected void apply(LHGraph graph) {
		Map nested = new Hashtable();
		AttributeMap attr = new AttributeMap();
		LHUserObject uObj = (LHUserObject)getUserObject();
		Document doc = description.getDocument();
		EditorKit kit = description.getEditorKit();
		if (kit instanceof HTMLEditorKit){
			Utility.debugPrintln("patterns","OK HTML Editor");
		} else {
			Utility.debugPrintln("patterns","NOT HTML Editor " + kit.getContentType());
		}
		StringWriter strW = new StringWriter();
		try {
			description.getEditorKit().write(strW,doc,0,doc.getLength());
		} catch (Exception e){}
		Utility.debugPrintln("patterns","OUT >>" + strW.toString());
		uObj.putProperty("label",strW.toString());
		GraphConstants.setValue(attr, uObj);
		nested.put(this, attr);
		graph.getModel().edit(nested, null, null, null);
	}
	private static Action locate(JTextComponent component, String name) {
	    Action returnValue = null;
	    Action actions[] = component.getActions();
	    for (int i = 0, n=actions.length; i<n; i++) {
	      Action action = actions[i];
	      String actionName = (String)action.getValue(Action.NAME);
	      if (actionName.equals(name)) {
	        returnValue = action;
	        break;
	      }
	    }
	    return returnValue;
	  }
}
