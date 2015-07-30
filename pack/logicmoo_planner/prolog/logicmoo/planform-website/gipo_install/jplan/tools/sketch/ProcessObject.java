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
 * Created on 12-Sep-2003
 *
 * Author ron
 * 
 */
package jplan.tools.sketch;

import javax.swing.JComponent;

/**
 * @author ron
 *
 * Wrapper for processes shown on sketch canvas
 */
public class ProcessObject implements DrawableObject {
	private String objName;
	private String objSort;
	private JComponent objIcon;
	
	public ProcessObject(String name,String sort){
		objName = name;
		objSort = sort;
	}
	/**
	 * ToolTip title
	 * @see jplan.tools.sketch.DrawableObject#getTitle()
	 */
	public String getTitle() {
		return objName;
	}

	/* (non-Javadoc)
	 * @see jplan.tools.sketch.DrawableObject#getIcon()
	 */
	public JComponent getIcon() {
		return objIcon;
	}

	/* (non-Javadoc)
	 * @see jplan.tools.sketch.DrawableObject#setIcon(javax.swing.JComponent)
	 */
	public void setIcon(JComponent newIcon) {
		objIcon = newIcon;

	}

	/* (non-Javadoc)
	 * @see jplan.tools.sketch.DrawableObject#setStartTime(int)
	 */
	public void setStartTime(int time) {
		// Do Nothing

	}

	/* (non-Javadoc)
	 * @see jplan.tools.sketch.DrawableObject#getStartTime()
	 */
	public int getStartTime() {
		// Do Nothing
		return 0;
	}

	/* (non-Javadoc)
	 * @see jplan.tools.sketch.DrawableObject#setEndTime(int)
	 */
	public void setEndTime(int time) {
		// Do Nothing

	}

	/* (non-Javadoc)
	 * @see jplan.tools.sketch.DrawableObject#getEndTime()
	 */
	public int getEndTime() {
		// Do Nothing
		return 0;
	}

}
