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

import java.awt.*;
import javax.swing.*;
import java.io.StringWriter;
import java.io.PrintWriter;
import java.util.*;

import jplan.ocl.*;

/**
 * @Author ron
 * Display the reachability analysis in a text window
 * @version 0
 */
public class ViewReachability extends JInternalFrame {
    private OclEd top;
    private oclDomain curDomain;
    private JTextArea text;
    private JScrollPane jscrText;

    public static final int REACH = 0;
    public static final int PREDUSE = 1;

    public ViewReachability(OclEd top,int analysis) {
	super ("Analysis",true,true,true,true);
	this.top = top;
	if (analysis == REACH) {
	    setTitle("Reachability Analysis");
	} else {
	    setTitle("Predicate Use Analysis");
	}
	if (top.curDomain == null ) {
	    JOptionPane query = 
		new JOptionPane("No OCL Domain Currently Exists",
				JOptionPane.ERROR_MESSAGE,
				JOptionPane.DEFAULT_OPTION);
	    JDialog dia = query.createDialog(top,"GIPO Error");
	    dia.show();
	    dispose();
	} else {
	    initComponents ();
	    curDomain = top.curDomain;
	    if (analysis == REACH) {
		displayReachabilityAnalysis();	    
	    } else {
		displayPredUseAnalysis();
	    }	    
	    //Point topPt = top.getLocation();
	    //setLocation(topPt.x,topPt.y + top.getHeight());
	    setPreferredSize(new Dimension(400, 400) );	
	    pack ();
	    setVisible(true);
	}
    }

    private void initComponents() {
	text = new JTextArea();
	text.setEditable( false );
	JScrollPane jscrText = new JScrollPane( text );
	getContentPane ().add (jscrText);
    }

    private void displayReachabilityAnalysis() {
	StringWriter dom;

	if (top.curDomain == null ) {
	    JOptionPane query = 
		new JOptionPane("No OCL Domain Currently Exists",
				JOptionPane.ERROR_MESSAGE,
				JOptionPane.DEFAULT_OPTION);
	    JDialog dia = query.createDialog(top,"GIPO Error");
	    dia.show();
	} else {
	    top.curDomain.checkStateUses();
	    top.curDomain.printStateUses(new PrintWriter 
					 (dom = new StringWriter()));
	    text.setText(new String(dom.getBuffer()));
	    setTitle("Reachability Analysis [ " + top.curDomain.getName() + " ]");
	}
    }

    private void displayPredUseAnalysis() {
	StringWriter dom;

	if (top.curDomain == null ) {
	    JOptionPane query = 
		new JOptionPane("No OCL Domain Currently Exists",
				JOptionPane.ERROR_MESSAGE,
				JOptionPane.DEFAULT_OPTION);
	    JDialog dia = query.createDialog(top,"GIPO Error");
	    dia.show();
	} else {
	    java.util.List mssgs = top.curDomain.checkAllPredicateUses();
	    if (mssgs.size() == 0) {
		text.setText("All dynamic predicates are used in exactly on substate definition.");
	    } else {
		String t = new String("");
		ListIterator li = mssgs.listIterator();
		while (li.hasNext()) {
		    t = t.concat((String)li.next() + "\n");
		}
		text.setText(t);
	    }
	    setTitle("Predicate Use Analysis [ " + top.curDomain.getName() + " ]");
	}
    }
}
 
