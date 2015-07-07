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
/**
 * GipoButton.java
 *
 *
 * Created: Wed Dec  5 11:17:11 2001
 *
 * @author W Zhao
 * @version
 */
import javax.swing.*;
import java.awt.event.*;

public class GipoButton extends JButton implements MouseListener {
    public GipoButton(String text, Icon icon) {
	super(text, icon);
	addMouseListener(this);
	setVerticalTextPosition(AbstractButton.BOTTOM);
	setHorizontalTextPosition(AbstractButton.CENTER);
	setBorderPainted(false);
	setToolTipText(text);
    }

    /**
     * empty
     */
    public void mouseClicked(MouseEvent e) {}
    /**
     * When mouse enters paint border
     */
    public void mouseEntered(MouseEvent e) {
	setBorderPainted(true);
	repaint();
    }
    /**
     * When mouse exits hide border
     */
    public void mouseExited(MouseEvent e) {
	setBorderPainted(false);
	repaint();
    }
    /**
     * empty
     */
    public void mousePressed(MouseEvent e) {}
    /**
     * empty
     */
    public void mouseReleased(MouseEvent e) {}
} // GipoButton
