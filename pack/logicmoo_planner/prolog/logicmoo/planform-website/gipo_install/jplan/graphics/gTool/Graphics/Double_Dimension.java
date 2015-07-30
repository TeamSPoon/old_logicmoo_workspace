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

package jplan.graphics.gTool.Graphics;

/*
 * Double_Dimension.java
 * Auther: W Zhao
 * 26/1/2001
*/


/**
 * dimension with double value
 */

public class Double_Dimension {
    /**
     * size
     */
    public double	width, height;
   
    /**
     * default double dimension
     * @param w width with double value 
     * @param h height with double value
     */
    public Double_Dimension(double w, double h) {
	width = w;
	height = h;
    }
   
    /**
     * returns true if this Double_Dimension is the same as the given Double_Dimension
     * @param d given Double_Dimension
     * @return true or false
     */
    public boolean equals(Double_Dimension d) {
	if(d.width == width && d.height == height)
            return true;
	else return false;
    }
   
}
