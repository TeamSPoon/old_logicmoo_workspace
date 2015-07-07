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

import java.awt.datatransfer.*;
import java.io.*;
import java.util.*;

import jplan.ocl.*;

public class ObjectTransferable implements Transferable {
    final static int OBJECT_FLAVOR = 0;

    String name = "empty";

    public ObjectTransferable (String node) {
	name = node;
    }

    public DataFlavor[] getTransferDataFlavors() {
	return flavors;
    }

     public boolean isDataFlavorSupported(DataFlavor fl) {
	for (int i = 0;i < flavors.length; i++) {
	    if (fl.equals(flavors[i]))
		return true;
	}
	return false;
    }

    public Object getTransferData(DataFlavor fl) {
	if (fl.equals(flavors[OBJECT_FLAVOR])) {
	    return name;
	} else {
	    return null;
	}
    }

    public static final DataFlavor objectFlavor =
	new DataFlavor(String.class,"OCL Object");
    
    static DataFlavor flavors[] = { objectFlavor };
}
	    
