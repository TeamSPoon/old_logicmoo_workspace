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

// oclDescriptions - Store descriptions of the ocl elements
package jplan.ocl;

import java.util.*;
import java.io.*;

/**
 * oclDescriptions - store the descriptions of ocl Elements
 * @author Ron Simpson
 * @version 0
 */
// Note this must eventually implement oclPrint
public class oclDescriptions implements Serializable{
    /**
     * hashOCLDescription - store descriptive text et describe
     * ocl element - the key is maintained by the ocl elements
     * keys are formed from the element name in an element
     * dependant manner
     */
    private Hashtable hashOCLDescriptions = null;

    /**
     * no argument constructor
     */
    public oclDescriptions() {
	hashOCLDescriptions = new Hashtable(50);
    }

    /**
     * addDescription 
     * add a text description on the ocl specification element
     * @param key - the key for this description
     * @param desc - the text description
     * 
     */
    public void addDescription(String key,String desc) {
	hashOCLDescriptions.put(key,desc);
    }

    /**
     * getDescription 
     * retrieve a text description on the ocl specification element
     * @param key the key for the element
     * @return String - the description
     */
    public String getDescription(String key) {
	return (String)hashOCLDescriptions.get(key);
    }

    /**
     * removeDescription 
     * remove a text description on the ocl specification element
     * @param key the key position of the description
     */
    public void removeDescription(String key) {
	Object ret = hashOCLDescriptions.remove(key);
    }
}
