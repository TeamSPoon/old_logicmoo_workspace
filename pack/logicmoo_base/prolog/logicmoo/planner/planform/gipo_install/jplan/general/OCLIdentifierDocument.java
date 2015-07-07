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

import javax.swing.text.*;
import java.awt.Toolkit;

/**
 * OCLIdentifierDocument
 * Provides a document class to enforce text field entries to conform to OCL
 * Identifiers i.e. first Character lowercase letter 
 * then any sequence of characters digits underscores
 * @author Ron Simpson
 * @version 0
 */

public class OCLIdentifierDocument extends PlainDocument{

    public void insertString(int offset, String string, 
			     AttributeSet attributes) 
	throws BadLocationException {
	if(string == null) {
	    return;
	} else {
	    String newValue;
	    int length = getLength();
	    if (length == 0) {
		newValue = string;
	    } else {
		String currentContent = getText(0,length);
		StringBuffer currentBuffer = new StringBuffer(currentContent);
		currentBuffer.insert(offset,string);
		newValue = currentBuffer.toString();
	    }
	    try {
		checkInput(newValue);
		
		super.insertString(offset,string,attributes);
	    } catch (OCLFormatException fe) {	
		Toolkit.getDefaultToolkit().beep();
	    } catch (Exception e) {
	    		e.printStackTrace();
		Utility.debugPrintln("patterns","OCLIdentifier::Exception " + e.toString());
	    }
	}
    }

    public void remove(int offset, int length) throws BadLocationException {
	int currentLength = getLength();
	String currentContent = getText(0,currentLength);
	String before = currentContent.substring(0,offset);
	String after = currentContent.substring(length+offset,currentLength);
	String newValue = before + after;
	try {
	    checkInput(newValue);
	    super.remove(offset,length);
	} catch (OCLFormatException fe) {	
		Toolkit.getDefaultToolkit().beep();
	} catch (Exception e) {
		e.printStackTrace();
				Utility.debugPrintln("patterns","OCLIdentifier Remove::Exception " + e.toString());
	    //Toolkit.getDefaultToolkit().beep();
	}
    }

    public void checkInput(String proposedValue) throws OCLFormatException {
	int length = proposedValue.length();
	if (length == 0)
	    return;
	if (! Character.isLowerCase(proposedValue.charAt(0))) {
	    throw new OCLFormatException("Illegal first identifier character");
	} else {
	    for(int i = 1; i < length; i++)
		if (Character.isLetterOrDigit(proposedValue.charAt(i)) ||
		    proposedValue.charAt(i) == '_')
		    continue;
		else
		    throw new OCLFormatException("Illegal identifier character");
	}
    }
}


	 	      
					      
		    
	    
