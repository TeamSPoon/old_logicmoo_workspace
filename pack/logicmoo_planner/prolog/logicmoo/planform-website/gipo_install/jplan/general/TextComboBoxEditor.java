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

/**
 * An editor to use with JCombo boxes to enforce document editing rules
 * 
 * @author Ron Simpson
 * @version 0
 */

public class TextComboBoxEditor implements ComboBoxEditor {
	final protected JTextField editor;

	protected EventListenerList listenerList = new EventListenerList();

	public TextComboBoxEditor(Document doc) {
		editor = new JTextField(8);// was 13
		editor.setDocument(doc);
		DocumentListener dl = new DocumentListener() {
			public void changedUpdate(DocumentEvent de) {
				fireActionEvent(de);
			}

			public void insertUpdate(DocumentEvent de) {
				//fireActionEvent(de);
			}

			public void removeUpdate(DocumentEvent de) {
				//fireActionEvent(de);
			}
		};
		doc.addDocumentListener(dl);
	}

	public void addActionListener(ActionListener l) {
		listenerList.add(ActionListener.class, l);
	}

	public Component getEditorComponent() {
		return editor;
	}

	public Object getItem() {
		return editor.getText();
	}

	public void removeActionListener(ActionListener l) {
		listenerList.remove(ActionListener.class, l);
	}

	public void selectAll() {
		editor.selectAll();
	}

	public void setItem(Object newValue) {
		if (newValue instanceof String) {
			editor.setText((String) newValue);
			Utility.debugPrintln("patterns", "setText = " + newValue);
		}
		// else ignore
	}

	protected void fireActionEvent(DocumentEvent de) {
		if (editor.getText().equals(""))
			return;
		Object listeners[] = listenerList.getListenerList();
		for (int i = listeners.length - 2; i >= 0; i -= 2) {
			if (listeners[i] == ActionListener.class) {
				ActionEvent actionEvent = new ActionEvent(editor,
						ActionEvent.ACTION_PERFORMED, "doneEdit");
				((ActionListener) listeners[i + 1])
						.actionPerformed(actionEvent);
			}
		}
	}
}

