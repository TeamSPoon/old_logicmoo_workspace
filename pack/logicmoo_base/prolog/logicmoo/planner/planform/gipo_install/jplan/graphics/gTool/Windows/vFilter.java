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

package jplan.graphics.gTool.Windows;

/*
 * vFilter.java
 * 21/02/2001 - "pay day"
 * Author: Weihong Zhao
*/



import java.awt.*;
import javax.swing.filechooser.FileFilter;
import java.io.File;


/**
 * my own file filter
 */

public class vFilter extends FileFilter {
    
    private String fileExtension = null;
    private String fileDescription = null;

    /**
     * file filter with specified extension, anddescription
     * @param extension String extension
     * @param description String
     */
    public vFilter (String extension, String description) {
	fileExtension = extension;
	fileDescription = description;
    }

    /**
     * geta file Description
     * @return String
     */
    public String getDescription(){
	return fileDescription;
    }

    /**
     * returns true if filds the specified file
     * @parm f given file
     * @return true or false
     */
    public boolean accept(File f) {
	if (f == null)
	    return false;
	if (f.isDirectory())
	    return true;
	return f.getName().toLowerCase().endsWith(fileExtension);
    }
}
