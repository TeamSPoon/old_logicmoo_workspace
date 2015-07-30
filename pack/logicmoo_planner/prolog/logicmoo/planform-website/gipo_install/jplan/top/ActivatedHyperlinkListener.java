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
import javax.swing.*;
import javax.swing.text.*;
import javax.swing.event.*;
import java.io.*;
import java.net.*;

import jplan.general.Utility;

public class ActivatedHyperlinkListener implements HyperlinkListener {

  JFrame frame;
  JEditorPane editorPane;

  public ActivatedHyperlinkListener(JFrame parent, JEditorPane editorPane) {
    this.frame = parent;
    this.editorPane = editorPane;
  }

    public void hyperlinkUpdate(HyperlinkEvent hyperlinkEvent) {
	HyperlinkEvent.EventType type = hyperlinkEvent.getEventType();
	final URL url = hyperlinkEvent.getURL();
//             if (type == HyperlinkEvent.EventType.ENTERED) {
//               Utility.debugPrintln("patterns","Entered " + url.toString());
//             } else if (type == HyperlinkEvent.EventType.EXITED) {
//               Utility.debugPrintln("patterns","Exited");
//             } 
	if (type == HyperlinkEvent.EventType.ACTIVATED) {
	    //Utility.debugPrintln("Activated");
	    Runnable runner = new Runnable() {
		    public void run() {
			// Retain reference to original
			Document doc = editorPane.getDocument();
			try {
			    editorPane.setPage(url);
			} catch (IOException ioException) {
			    JOptionPane.showMessageDialog(frame, 
			             "Error following link " + url.toString(), 
                                      "GIPO Error", 
                                       JOptionPane.ERROR_MESSAGE);
			    editorPane.setDocument(doc);
			}
		    }
		};
	    SwingUtilities.invokeLater(runner);
	}
    }
}
