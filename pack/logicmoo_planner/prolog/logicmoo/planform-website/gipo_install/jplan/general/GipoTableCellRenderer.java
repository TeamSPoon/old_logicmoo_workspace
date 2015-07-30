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

/**
 * GipoTableCellRenderer.java
 *
 *
 * Created: Wed Jun 12 16:10:17 2002
 *
 * @author W Zhao
 * @version
 */
import javax.swing.JTextArea;
import javax.swing.JTable;
import javax.swing.JList;
import javax.swing.DefaultListModel;
import javax.swing.table.*;
import java.awt.Component;
import jplan.ocl.oclSS;
import jplan.ocl.oclPredicate;
import java.util.List;
import java.util.*;
import jplan.edexpt.PredListCellRenderer;

public class GipoTableCellRenderer extends JList implements TableCellRenderer {
    JList list = new JList();

    public Component getTableCellRendererComponent(
                                JTable table, Object value, 
                                boolean isSelected, boolean hasFocus,
                                int row, int column) {

	DefaultListModel lmList = new DefaultListModel();
	int n_row = 1;

	/* for oclSS */
	if (value instanceof oclSS){    
	    ListIterator li = ((oclSS)value).getState().listIterator();
	    while(li.hasNext()) {
		oclPredicate cur = (oclPredicate)li.next();
		lmList.addElement(cur.toString());
	    }
	    n_row = ((oclSS)value).getState().size();
	}
	else {
	    lmList.addElement(value);
	}

	list.setModel(lmList);

	/* WZ 13/6/02 */
	if (table.getRowHeight(row)<table.getRowHeight()*(n_row+1))
	    table.setRowHeight(row, table.getRowHeight()*(n_row+1));

	if (!isSelected) {
	    list.setBackground(table.getBackground());
	} else {
	    list.setBackground(new java.awt.Color(255, 255, 241));
	}
	/* end 13/6/02 */
	return list;
    }
} // GipTableCellRenderer
