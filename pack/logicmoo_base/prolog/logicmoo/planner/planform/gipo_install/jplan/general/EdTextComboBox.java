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

package jplan.general;

import java.awt.*;
import javax.swing.*;
import javax.swing.event.*;
import java.awt.event.*;
import javax.swing.text.*;
import java.util.*;


/**
 * An editable Combo boxes to enforce document editing rules
 * the default is to use the Identifier document rules
 * @author Ron Simpson
 * @version 0
 */

public class EdTextComboBox extends JComboBox {
    Document doc;

    public EdTextComboBox() {
	super();
	doc = new OCLIdentifierDocument();
	TextComboBoxEditor ed = new TextComboBoxEditor(doc);
	setEditor(ed);
	setEditable(true);
    }

    /**
     * setDocument
     * @param doc - Document to enforce editing rules
     */
    public void setDocument(Document doc) {
	TextComboBoxEditor ed = new TextComboBoxEditor(doc);
	setEditor(ed);
    }

    /**
     * addAllList
     * add the contents of a List to the combo box
     * param - the list of strings
     */
    public void addAllList(java.util.List lst) {
	ListIterator li = lst.listIterator();
	while (li.hasNext()) {
	    String str = (String)li.next();
	    addItem(str);
	}
    }
}
