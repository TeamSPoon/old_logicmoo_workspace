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

import javax.swing.filechooser.*;
import java.io.File;

public class ExtensionFileFilter extends FileFilter {
  String description;
  String extensions[];

  public ExtensionFileFilter(String description, String extension) {
    this(description, new String[] {extension});
  }

  public ExtensionFileFilter(String description, String extensions[]) {
    if (description == null) {
      // Since no description, use first extension and # of extensions as description
      this.description = extensions[0]+"{"+extensions.length+"}" ;
    } else {
      this.description = description;
    }
    this.extensions = (String[])extensions.clone();
    // Convert array to lowercase
    // Don't alter original entries
    toLower(this.extensions);
  }

  private void toLower(String array[]) {
    for (int i=0, n=array.length; i<n; i++) {
      array[i] = array[i].toLowerCase();
    }
  } 

  public String getDescription() {
    return description;
  }

  // ignore case, always accept directories
  // character before extension must be a period
  public boolean accept(File file) {
    if (file.isDirectory()) {
      return true;
    } else {
      String path = file.getAbsolutePath().toLowerCase();
      for (int i=0, n=extensions.length; i<n; i++) {
        String extension = extensions[i];
        if ((path.endsWith(extension)  &&
            (path.charAt(path.length()-extension.length()-1)) == '.')) {
          return true;
        }
      }
    }
    return false;
  }
}

