
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
 *
 *
 * Created on 01-Jul-2003
 *
 * Author ron
 * 
 */
package jplan.edexpt;

import java.awt.*;
import javax.swing.*;
import jplan.ocl.oclPredicate;

/**
 * @author ron
 *
 * Renderer for mixture of fluents and predicates
 * use to display instantiated fluents with their values
 */
public class FluentRenderer implements ListCellRenderer {

	protected DefaultListCellRenderer defaultRenderer =
	new DefaultListCellRenderer();

	public Component getListCellRendererComponent(JList list,
						  Object value,
						  int index,
						  boolean isSelected,
						  boolean cellhasFocus) {
	
	JLabel renderer = 
		(JLabel)defaultRenderer.getListCellRendererComponent(
			   list,value,index,isSelected,cellhasFocus);
	if (((oclPredicate)value).isFluent())
		renderer.setText(((oclPredicate)value).toString() 
			+ " is " +
			((oclPredicate)value).getFluentValue());
	else
		renderer.setText(value.toString());
	return renderer;
	}

}
