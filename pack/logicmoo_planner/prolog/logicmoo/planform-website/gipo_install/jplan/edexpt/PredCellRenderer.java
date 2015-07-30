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

package jplan.edexpt;

import java.awt.*;
import javax.swing.*;
import jplan.ocl.oclPredicate;

import jplan.images.ImageLoader;

/**
 * PredCellRenderer used to add simple icons to predicate
 * descriptions in the Predicate View List Box
 * @author Ron Simpson
 * @version 0
 */
public class PredCellRenderer implements ListCellRenderer {
	Icon staticIcon, fluentIcon, bothIcon;

	public PredCellRenderer(String staticIconPath, String staticIconName,
			String fluentIconName, String bothIconName) {
		staticIcon = ImageLoader.getImageIcon(staticIconPath, staticIconName);
		fluentIcon = ImageLoader.getImageIcon(staticIconPath, fluentIconName);
		bothIcon = ImageLoader.getImageIcon(staticIconPath, bothIconName);
		//staticIcon = new ImageIcon(staticIconPath);
	}

	public PredCellRenderer(String staticIconPath, String staticIconName) {
		staticIcon = ImageLoader.getImageIcon(staticIconPath, staticIconName);
		//staticIcon = new ImageIcon(staticIconPath);
	}

	protected DefaultListCellRenderer defaultRenderer = new DefaultListCellRenderer();

	public Component getListCellRendererComponent(JList list, Object value,
			int index, boolean isSelected, boolean cellhasFocus) {

		JLabel renderer = (JLabel) defaultRenderer
				.getListCellRendererComponent(list, value, index, isSelected,
						cellhasFocus);
		if (((oclPredicate) value).isStatic()
				&& ((oclPredicate) value).isFluent()) // Ron 25/06/03 Deal with functors in the predicate list
			renderer.setIcon(bothIcon);
		else if (((oclPredicate) value).isStatic())
			renderer.setIcon(staticIcon);
		else if (((oclPredicate) value).isFluent()) // Ron 25/06/03 Deal with functors in the predicate list
			renderer.setIcon(fluentIcon);
		else
			renderer.setIcon(null);
		renderer.setText(value.toString());
		return renderer;
	}
}
