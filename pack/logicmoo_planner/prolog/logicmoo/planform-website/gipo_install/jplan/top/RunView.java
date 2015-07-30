/*
 * GIPO COPYRIGHT NOTICE, LICENSE AND DISCLAIMER.
 * 
 * Copyright 2001 - 2003 by R.M.Simpson W.Zhao T.L.McCLuskey D Liu D. Kitchin
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
package jplan.top;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import javax.swing.*;
import javax.swing.text.*;
import jplan.ocl.*;
import jplan.general.Utility;
import jplan.general.HardcopyWriter; /* Weihong 14/3/02 */
import jplan.images.ImageLoader; 

/**
 * Run View - Run Tasks and View solutions to planning problems on the current
 * domain This view is not editable
 * 
 * @author Ron Simpson
 */
public class RunView extends JInternalFrame implements oclPrint {/*
																  * Weihong
																  * 14/3/02
																  */
	private JEditorPane jedResults;
	private JTextField textField;
	private OclEd parent;/* Weihong 14/3/02 */
	/**
	 * Create the HTML Editor Dialog Box : Add a url Text window to allow
	 * domains to be loaded directly
	 */
	public RunView(OclEd parent, String resPath) {
		super("", true, true, true, true);
		this.parent = parent;
		jedResults = new JEditorPane();
		jedResults.setEditable(false);
		getContentPane().add(new JScrollPane(jedResults), "Center");
		JToolBar panel = new JToolBar();
		panel.setFloatable(false);
		//	panel.setLayout(new BorderLayout(4,4));
		/* Weihong 14/3/02 */
		ImageIcon ii = ImageLoader.getImageIcon(parent.strImageDir, "Print16.gif");
		JButton bt = new JButton(" Print ",ii);
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				printActionPerformed();
			}
		});
		panel.add(bt);
		ii = ImageLoader.getImageIcon(parent.strImageDir, "Stop16.gif");
		JButton cmdClose = new JButton("Close",ii);
		panel.add(cmdClose);
		cmdClose.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				setVisible(false);
				dispose();
			}
		});
		getContentPane().add(panel, "South");
		jedResults.setContentType("text/plain");
		try {
			Utility.debugPrintln("Try to open " + resPath);
			jedResults.setPage("file:" + resPath);
		} catch (IOException e) {
			JOptionPane.showMessageDialog(parent,
					"Cannot read the results file.", "GIPO Error",
					JOptionPane.ERROR_MESSAGE, null);
			return;
		}
		this.setSize(500, 400);
		this.show();
	}
	/**
	 * Create the HTML Editor Dialog Box : Add plain text to display the content
	 */
	public RunView(OclEd parent, String content, String AlgName) {
		super(AlgName, true, true, true, true);
		this.parent = parent;
		jedResults = new JEditorPane();
		jedResults.setEditable(false);
		getContentPane().add(new JScrollPane(jedResults), "Center");
		JToolBar panel = new JToolBar();
		panel.setFloatable(false);
		// 	panel.setLayout(new BorderLayout(4,4));
		/* Weihong 14/3/02 */
		ImageIcon ii = ImageLoader.getImageIcon(parent.strImageDir, "Print16.gif");
		JButton bt = new JButton(" Print ",ii);
		bt.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(ActionEvent e) {
				printActionPerformed();
			}
		});
		panel.add(bt);
		ii = ImageLoader.getImageIcon(parent.strImageDir, "Stop16.gif");
		JButton cmdClose = new JButton(" Close ",ii);
		// 	panel.add(cmdClose, "West");
		panel.add(cmdClose);
		getContentPane().add(panel, "South");
		cmdClose.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				setVisible(false);
				dispose();
			}
		});
		jedResults.setContentType("text/plain");
		jedResults.setText(content);
		this.setSize(500, 400);
		this.show();
	}
	/* Weihong 14/3/02 */
	/**
	 * print text
	 */
	private void printActionPerformed() {
		String header = "Gipo - Domain [" + parent.curDomain.getName()
				+ "] Author [" + parent.curDomain.getAuthor() + "]";
		HardcopyWriter hw;
		try {
			hw = new HardcopyWriter(parent, header, 10, .75, .5, .75, .5);
		} catch (HardcopyWriter.PrintCanceledException e) {
			JOptionPane.showMessageDialog(this,
					"Error Printing Action Sequence.", "GIPO Error",
					JOptionPane.ERROR_MESSAGE, null);
			return;
		}
		// Send output to it through a PrintWriter stream
		PrintWriter out = new PrintWriter(hw);
		oclPrintComponent(out, 0, false);
		out.close();
	}
	/* Weihong 14/3/02 */
	/*
	 * Print the action sequence (Probably) to the printer
	 */
	public void oclPrintComponent(PrintWriter ps, int indent, boolean nline) {
		ps.println("Planning result");
		ps.println();
		String baseString = jedResults.getText();
		ps.println(baseString);
	}
}