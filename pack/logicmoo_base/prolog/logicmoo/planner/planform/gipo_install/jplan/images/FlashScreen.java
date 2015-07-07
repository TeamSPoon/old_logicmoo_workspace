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

package jplan.images;

/**
 * FlashScreen.java
 *
 *
 * Created: Wed Mar  6 14:27:03 2002
 *
 * @author W Zhao
 * @version
 */
import javax.swing.*;
import java.awt.Font;
import java.awt.Image;
import jplan.images.ImageLoader;
import java.awt.GridBagLayout;/* WZ 20/5/02 */
import java.awt.GridBagConstraints;/* WZ 20/5/02 */

public class FlashScreen extends JWindow {
    
    public FlashScreen() {
	super();
	pack();
	setLocation(getToolkit().getScreenSize().width/2 - getBounds().width/2, getToolkit().getScreenSize().height/2 - getBounds().height/2);
// 	setSize(400, 160);
 	setVisible( true );
    }

    protected void windowInit(){
	JPanel basePanel = new JPanel();
// 	basePanel.setLayout(new java.awt.BorderLayout());
	/* WZ 20/5/02 */
	GridBagLayout gridbag = new GridBagLayout();
	basePanel.setLayout(gridbag);
	GridBagConstraints c = new GridBagConstraints();
	c.fill = GridBagConstraints.BOTH;
	c.weightx = 1.0;
	c.weighty = 1.0;
	c.gridheight = GridBagConstraints.RELATIVE; 
	c.gridwidth = GridBagConstraints.REMAINDER; //end row
	/* end 20/5/02 */
	basePanel.setBorder(new javax.swing.border.LineBorder(java.awt.Color.red));

	Image image = ImageLoader.getImage("gipo_gear.gif", "gipo_gear.gif");
	if (image == null)
	    jplan.general.Utility.debugPrintln("no images loaded.");
	JPanel panel1 = new jplan.top.PicturePanel(image);
	panel1.setBorder(new javax.swing.border.BevelBorder(2));
	panel1.setBackground(java.awt.Color.white);
	gridbag.setConstraints(panel1, c);  /* WZ 20/5/02 */
	basePanel.add(panel1);

	JPanel panel2 = new JPanel();
	panel2.setLayout(new java.awt.GridLayout());
	JTextArea txt = new JTextArea();
	txt.setEditable(false);
	txt.setFont(new Font("Arial", Font.BOLD, 14));
	txt.append(" Graphical Interface for Planning with Objects (GIPO III) \n");
	panel2.add(txt);
	c.gridheight = 1;
	gridbag.setConstraints(panel2, c);  /* WZ 20/5/02 */
	basePanel.add(panel2);

	add(basePanel);
    }
} // FlashScreen
