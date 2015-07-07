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
/**
 * PicturePanel.java
 *
 *
 * Created: Thu Aug 30 12:15:48 2001
 *
 * @author W Zhao
 * @version
 */

import java.awt.Image;
import java.awt.*;

/**
 * A JPanel to display images.
 */
public class PicturePanel extends javax.swing.JPanel {
    private Image image;
    private Dimension preferredSize;

    /**
     * Creates an instance of the PicturePanel.
     * @param image the image to put in the panel.
     */
    public PicturePanel(Image image) {
	super();
	setOpaque(true);
	this.image = image;
// 	preferredSize = new Dimension(this.image.getWidth(this),this.image.getHeight(this));    /* Weihong changed on 19/10/2001 */
	preferredSize = new Dimension(300, 150);
    }
    
    /**
     * Get the value of image.
     * @return Value of image.
     */
    public Image getImage() {return image;}
    
    /**
     * Set the value of image.
     * @param v  Value to assign to image.
     */
    public void setImage(Image  v) {this.image = v;}

    
    /**
     * Get the value of preferredSize.
     * @return Value of preferredSize.
     */
    public Dimension getPreferredSize() {
// 	preferredSize = new Dimension(image.getWidth(this),image.getHeight(this)); 
	return preferredSize;
    }
    
    /* Weihong changed on 18/10/2001 */   
    /**
     * Set the value of preferredSize.
     * @param v  Value to assign to preferredSize.
     */
//     public void setPreferredSize(Dimension  v) {this.preferredSize = v;}
    
    /**
     * This method is invoked by Swing to draw components. 
     * Applications should not invoke paint directly, but should instead use the repaint 
     * method to schedule the component for redrawing.<br>
     * This method actually delegates the work of painting to three protected methods: 
     * paintComponent, paintBorder, and paintChildren. They're called in the order 
     * listed to ensure that children appear on top of component itself. Generally speaking,
     * the component and its children should not paint in the insets area allocated to the border.
     * Subclasses can just override this method, as always. A subclass that just wants to
     * specialize the UI (look and feel) delegate's paint method should just override paintComponent.
     * @overrides paint JComponent 
     */
    public void paintComponent(Graphics g){
	super.paintComponent(g); 
	g.drawImage(image, 0,0,getWidth(),getHeight(),this);
    }

} // PicturePanel
