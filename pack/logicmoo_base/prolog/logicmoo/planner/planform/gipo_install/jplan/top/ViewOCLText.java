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

/**
 * ViewOCLText
 * This class controls the plain text view of an ocl domain
 */

package jplan.top;
import jplan.general.GipoInternalFrame;/* Weihong added on 24/10/2001 */

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import java.io.StringWriter;
import java.io.PrintWriter;

import jplan.ocl.*;
import jplan.images.ImageLoader; 
import jplan.general.Utility;
import jplan.general.UnderlineHighlighter;/* WZ 22/8/02 */
import javax.swing.text.BadLocationException;/* WZ 22/8/02 */

/**
 * @Author ron
 * @version 0
 */
public class ViewOCLText extends GipoInternalFrame {/* Weihong added on 24/10/2001 */
    protected OclEd top;
    protected oclDomain curDomain;
    protected JTextArea text;
    protected JScrollPane jscrText;
    protected JTextField searchField;/* WZ 22/8/02 */


    public ViewOCLText(OclEd top) {
	super(top);	/* Weihong added on 24/10/2001 */
	setTitle("Domain Specification");
	this.top = top;
	if (top.curDomain == null ) {
	    /* Weihong added on 5/11/2001 */
	    JOptionPane.showMessageDialog(top,
				    "No GIPO Domain Currently Exists",
				    jplan.general.EdStrings.strErrorHeading,
				    JOptionPane.ERROR_MESSAGE,
				    null);	    
	    return;
	} else {
	    initComponents ();
	    curDomain = top.curDomain;
	    printDomain();
	    /* Weihong added on 18/10/2001 */
	    // to bring the scrollbar back to the top
	    text.select(0,0);
	    text.setEditable( false );
	    //Point topPt = top.getLocation();
	    //setLocation(topPt.x,topPt.y + top.getHeight());
	    setPreferredSize(new Dimension(600, 600) );	
	    pack ();
	    setVisible(true);
	}
    }

    protected void initComponents() {
	/* WZ 22/8/02 to add a search function */
	searchField = new JTextField(20);
	ImageIcon ii = ImageLoader.getImageIcon(top.strImageDir, "Find16.gif");
	JButton searchBTN = new JButton(" String Search ", ii);
        searchBTN.addActionListener (new java.awt.event.ActionListener () {
            public void actionPerformed (java.awt.event.ActionEvent evt) {
		searchActionPerformed ();
            }
	}
				     );
	ii = ImageLoader.getImageIcon(top.strImageDir, "FindAgain16.gif");
	JButton searchObjBTN = new JButton(" Object Search ", ii);
        searchObjBTN.addActionListener (new java.awt.event.ActionListener () {
            public void actionPerformed (java.awt.event.ActionEvent evt) {
		searchObjActionPerformed ();
            }
	}
				     );

	JPanel searchPanel = new JPanel();
	searchPanel.add(searchField);
	searchPanel.add(searchBTN);
	searchPanel.add(searchObjBTN);
	getContentPane ().add (searchPanel, "North");
	/* end 22/8/02 */

	text = new JTextArea();
// 	text.setEditable( false );
	JScrollPane jscrText = new JScrollPane( text );
	getContentPane ().add (jscrText, "Center");
	jscrText = new JScrollPane( text );
	getContentPane ().add (jscrText);
		JPanel panel = new JPanel();
	panel.setLayout(new BorderLayout(4,4));
	JLabel urlLabel = new JLabel("Domain File: ",JLabel.RIGHT);
	panel.add(urlLabel, "West");
	JTextField jtxtDomain = new JTextField(32);
	if (top.osDiags.getOclFile() != null) {
	    jtxtDomain.setText(top.osDiags.getOclFile().getName());
	} else {
	    jtxtDomain.setText("none");
	}
	jtxtDomain.setEditable(false);
	panel.add(jtxtDomain, "Center");
	ii = ImageLoader.getImageIcon(top.strImageDir, "Stop16.gif");
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

    private void printDomain() {
	StringWriter dom;

	if (top.curDomain == null ) {
	    JOptionPane query = 
		new JOptionPane("No GIPO Domain Currently Exists",
				JOptionPane.ERROR_MESSAGE,
				JOptionPane.DEFAULT_OPTION);
	    JDialog dia = query.createDialog(top,"GIPO Error");
	    dia.show();
	} else {
	    top.curDomain.oclPrintComponent
		(new PrintWriter (dom = new StringWriter())
		    ,0,false);
	    String domText = dom.getBuffer().toString();
	    text.setText(domText);
	    setTitle("GIPO Text View [ " + top.curDomain.getName() + " ]");
	}
    }

    /* WZ 22/8/02 */
    /**
     * to search the given string
     * and highlight all in once
     */
    private void searchObjActionPerformed (){
	String schTXT = searchField.getText();
	if (schTXT.length() == 0)
	    return;

	String doc = text.getText();
	if (doc.length() == 0)
	    return;

	int end = 0;
	int bgn = 0;
	text.getHighlighter().removeAllHighlights();
	while (schTXT.length() < doc.length()){
	    String tmpTxt = doc.substring(end);
	    bgn = tmpTxt.indexOf(schTXT);
	    if (bgn == -1)
		break;

	    bgn += end;
	    end = bgn + schTXT.length();

	    //check if the search string partially another string
	    String chkStr = doc.substring(end,end+1);
	    if (chkStr.equals(",") || chkStr.equals(")") || chkStr.equals("]") || chkStr.equals("(") || chkStr.equals("[")){
		try {
		    text.getHighlighter().addHighlight(bgn, end, new UnderlineHighlighter.UnderlineHighlightPainter (Color.green));
		} catch (BadLocationException e) {
		    Utility.debugPrintln("Unexpected search failure.");
		}
	    }
	}
    }

    /* WZ 22/8/02 */
    /**
     * to search the given string
     * and highlight all in once
     */
    private void searchActionPerformed (){
	String schTXT = searchField.getText();
	if (schTXT.length() == 0)
	    return;

	String doc = text.getText();
	if (doc.length() == 0)
	    return;

	int end = 0;
	int bgn = 0;
	text.getHighlighter().removeAllHighlights();
	while (schTXT.length() < doc.length()){
	    String tmpTxt = doc.substring(end);
	    bgn = tmpTxt.indexOf(schTXT);
	    if (bgn == -1)
		break;

	    bgn += end;
	    end = bgn + schTXT.length();

	    try {
		text.getHighlighter().addHighlight(bgn, end, new UnderlineHighlighter.UnderlineHighlightPainter (Color.red));
	    } catch (BadLocationException e) {
		Utility.debugPrintln("Unexpected search failure.");
	    }
	}
    }
}
 
