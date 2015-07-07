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

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

public class JPopupMenuShower extends MouseAdapter {

    private JPopupMenu popup;

    public JPopupMenuShower(JPopupMenu popup) {
	this.popup = popup;
    }

    private void showIfPopupTrigger(MouseEvent mouseEvent) {
	if(mouseEvent.isPopupTrigger()) {
	    popup.show(mouseEvent.getComponent(),mouseEvent.getX(),
		       mouseEvent.getY());
	}
    }

    public void mousePressed(MouseEvent mouseEvent) {
	showIfPopupTrigger(mouseEvent);
    }

    public void mouseReleased(MouseEvent mouseEvent) {
	showIfPopupTrigger(mouseEvent);
    }
}

