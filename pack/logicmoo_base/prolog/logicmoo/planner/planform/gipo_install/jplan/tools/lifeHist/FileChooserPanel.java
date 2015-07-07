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
 * 
 * 
 * 
 * Created on 22-Jan-2005
 * 
 * Author Ron
 */
package jplan.tools.lifeHist;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;

import javax.swing.*;

import jplan.general.OCLIdentifierDocument;
import jplan.general.TextComboBoxEditor;

/**
 * This class defines the FileChooser Panel to Prompt user to select appropriate
 * icon and package name when a new Package is created.
 */
public class FileChooserPanel extends JPanel {

	JTextField jtxtPackageName = null;

	public FileChooserPanel(Icon curIcon) {
		setLayout(new BorderLayout());
		JPanel namePan = new JPanel();
		jtxtPackageName = new JTextField("package");
		namePan.add(new JLabel("Package Name"));
		namePan.add(jtxtPackageName);
		add(namePan, "South");
		if (curIcon != null) {
			JLabel lblIcon = new JLabel(curIcon);
			add(lblIcon, "Center");
		} else {
			JLabel lblIcon = new JLabel("none");
			add(lblIcon, "Center");
		}
	}
}
