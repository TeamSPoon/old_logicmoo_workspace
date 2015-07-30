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
 * Created on 03-Jul-2003
 *
 * Author ron
 * 
 */
package jplan.general;

import javax.swing.text.*;
import java.awt.Toolkit;

/**
 * @author ron
 *
 * This document controls the format of plain decimals
 * Allows any digit string with up to one stop anywhere in sequence
 */
public class oclDecimalDocument extends PlainDocument {
	public void insertString(
		int offset,
		String string,
		AttributeSet attributes)
		throws BadLocationException {
		if (string == null) {
			return;
		} else {
			String newValue;
			int length = getLength();
			if (length == 0) {
				newValue = string;
			} else {
				String currentContent = getText(0, length);
				StringBuffer currentBuffer = new StringBuffer(currentContent);
				currentBuffer.insert(offset, string);
				newValue = currentBuffer.toString();
			}
			try {
				checkInput(newValue);
				super.insertString(offset, string, attributes);
			} catch (OCLFormatException fe) {
				Toolkit.getDefaultToolkit().beep();
			} catch (Exception e) {
				//Toolkit.getDefaultToolkit().beep();
			}
		}
	}

	public void remove(int offset, int length) throws BadLocationException {
		int currentLength = getLength();
		String currentContent = getText(0, currentLength);
		String before = currentContent.substring(0, offset);
		String after = currentContent.substring(length + offset, currentLength);
		String newValue = before + after;
		try {
			checkInput(newValue);
			super.remove(offset, length);
		} catch (OCLFormatException fe) {
			Toolkit.getDefaultToolkit().beep();
		} catch (Exception e) {
			//Toolkit.getDefaultToolkit().beep();
		}
	}

	public void checkInput(String proposedValue) throws OCLFormatException {
		int length = proposedValue.length();
		int stopCount = 0;
		if (length == 0)
			return;
		for (int i = 0;i < length;i++)
			if (Character.isDigit(proposedValue.charAt(i)))
				continue;
			else if(proposedValue.charAt(i) == '.' && stopCount == 0) {
				stopCount++;
				continue;
			} else
				throw new OCLFormatException("Illegal digit character");
	}
}
