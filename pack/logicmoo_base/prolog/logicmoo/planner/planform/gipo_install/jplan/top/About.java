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
 * About.java
 *
 *
 * Created: Thu Aug 30 10:12:44 2001
 *
 * @author W Zhao
 * @version 2 
 * @history - updated to GIPO Version 3 (III)
 */


import javax.swing.*;
import javax.swing.border.*;
import java.awt.*;
import java.awt.Image.*;
import java.util.Vector;
import jplan.images.ImageLoader; 	/* Weihong changed on 5/9/2001 */

/**
 * The dialog box of displaying the basic information for the GIPO software.
 */
public class About extends JDialog {
    
    private OclEd top;
    /* Weihong changed on 18/10/2001 */
    private javax.swing.JTextArea detailedArea =  new javax.swing.JTextArea("detailedArea");
    /* Weihong changed on 18/10/2001 */
    private javax.swing.JTextArea team =  new javax.swing.JTextArea("Research Team");
    JButton butCopyRight, buttonTeam;

    /**
     * Creates an instance of the About dialog box.
     * @param parent the parent frame
     */
    public About(OclEd parent) {
	super(parent);
	setModal(true);
	setTitle("About GIPO");
	this.top = parent;
	initComponents ();
	pack ();
    }

    /**
     * Creates an instance of the default About dialog box.
     */
    public About() {
	super();
	setTitle("About GIPO");
	initComponents ();
	pack ();
    }

    /**
     * Initialisation
     * 
     */
    private void initComponents(){
	setBackground (new java.awt.Color (114, 159, 255));
	addWindowListener (new java.awt.event.WindowAdapter () {
	    public void windowClosing (java.awt.event.WindowEvent evt) {
		closeDialog ();
	    }
	}
			   );
	getContentPane ().setLayout (new java.awt.BorderLayout ());

	//left - image
	/* Weihong changed on 4/9/2001 */
	Image image = ImageLoader.getImage(top.strImageDir + "gipo_gear.gif", "gipo_gear.gif");
	JPanel leftPanel = new PicturePanel(image);
	leftPanel.setBackground(Color.white);
	leftPanel.setPreferredSize(new Dimension(120, 100));

	//top - list
	JPanel topPanel = new JPanel();
	topPanel.setLayout(new GridLayout(0,1));
	JTextArea txt = new JTextArea();
	txt.setEditable(false);
	txt.setFont(new Font("Arial", Font.BOLD, 14));
	txt.append(" --- Graphical Interface for Planning with Objects (GIPO  - III)\n");
	topPanel.add(txt);

	JPanel centerPanel = new JPanel();
	centerPanel.setLayout(new GridLayout(0,1));
	JTextArea text = new JTextArea();
	text.setEditable(false);
	text.append("All rights reserved by\n");
	text.append("School of Computing and Engineering,\n    University of Huddersfield, UK\n\n");
	text.append("Technical support:\n    Ron Simpson <r.m.simpson@hud.ac.uk>");
	centerPanel.add(text);

	//center - text
	/* Weihong changed on 18/10/2001 */
	//Image img = ImageLoader.getImage(top.strImageDir + "planform_logo2.gif", "planform_logo2.gif");
	//JPanel eastPanel1 = new PicturePanel(img);
	//eastPanel1.setPreferredSize(new Dimension(100, 15));
	/* end Weihong changed on 18/10/2001 */

	//bottom - button
	JPanel bottomPanel = new JPanel();
	bottomPanel.setLayout(new java.awt.BorderLayout ());
	JPanel bottomPanel0 = new JPanel();
	JButton button = new JButton(" OK ");
	button.addActionListener (new java.awt.event.ActionListener () {
	    public void actionPerformed (java.awt.event.ActionEvent evt) {
		closeDialog ();
	    }
	});
	bottomPanel0.add(button);

	/* Weihong changed on 18/10/2001 */
	//bottomPanel0.add(eastPanel1); //add planform logo

	butCopyRight = new JButton("CopyRight");
	butCopyRight.addActionListener (new java.awt.event.ActionListener () {
	    public void actionPerformed (java.awt.event.ActionEvent evt) {
		detailedArea.setVisible(!detailedArea.isVisible());
		if (butCopyRight.getText() == "CopyRight")
		    butCopyRight.setText("Hide CopyRight");
		else
		    butCopyRight.setText("CopyRight");
		pack();
	    }
	});
	bottomPanel0.add(butCopyRight);

	bottomPanel0.setBorder (new javax.swing.border.BevelBorder(1));

	detailedArea.setLineWrap(true);
	detailedArea.setEditable(false);
	detailedArea.setBorder(BorderFactory.createBevelBorder(BevelBorder.LOWERED));
	detailedArea.setSize(500, 300);
	detailedArea.setFont(new java.awt.Font("Arial", java.awt.Font.PLAIN, 11));
	detailedArea.setVisible(false);

	StringBuffer sb = new StringBuffer();
	sb.append("GIPO COPYRIGHT NOTICE, LICENSE AND DISCLAIMER.\n\n");
	sb.append("Copyright 2001 - 2003 - 2005 by R.M.Simpson W.Zhao T.L.McCLuskey D.Liu D.Kitchin\n\n");
	sb.append("Permission to use, copy, modify, and distribute this software and its ");
	sb.append("documentation for any purpose and without fee is hereby granted, ");
	sb.append("provided that the above copyright notice appear in all copies and that ");
	sb.append("both the copyright notice and this permission notice and warranty ");
	sb.append("disclaimer appear in supporting documentation, and that the names of ");
	sb.append("the authors or their employers not be used in advertising or publicity ");
	sb.append("pertaining to distribution of the software without specific, written ");
	sb.append("prior permission.\n\n");

	sb.append("The authors and their employers disclaim all warranties with regard to ");
	sb.append("this software, including all implied warranties of merchantability and ");
	sb.append("fitness.  In no event shall the authors or their employers be liable ");
	sb.append("for any special, indirect or consequential damages or any damages ");
	sb.append("whatsoever resulting from loss of use, data or profits, whether in an ");
	sb.append("action of contract, negligence or other tortious action, arising out of ");
	sb.append("or in connection with the use or performance of this software.\n");
	detailedArea.setText(sb.toString());
	detailedArea.setBorder (new javax.swing.border.BevelBorder(1));

	bottomPanel.add(bottomPanel0, "North");
	bottomPanel.add(detailedArea, "Center");
	/* end Weihong changed on 18/10/2001 */

	/* Weihong changed on 22/10/2001 */
	buttonTeam = new JButton("Research Team");
	buttonTeam.addActionListener (new java.awt.event.ActionListener () {
	    public void actionPerformed (java.awt.event.ActionEvent evt) {
		team.setVisible(!team.isVisible());
		if (buttonTeam.getText() == "Research Team")
		    buttonTeam.setText("Hide Research Team");
		else
		    buttonTeam.setText("Research Team");
		pack();
	    }
	});
	bottomPanel0.add(buttonTeam);

	team.setLineWrap(true);
	team.setEditable(false);
	team.setBorder(BorderFactory.createBevelBorder(BevelBorder.LOWERED));
	team.setSize(500, 120);
	team.setVisible(false);	

	StringBuffer sbr = new StringBuffer();
	sbr.append("R.M.Simpson <r.m.simpson@hud.ac.uk>\n");
	sbr.append("W Zhao\n");
	sbr.append("T.L.McCLuskey <t.l.mccluskey@hud.ac.uk>\n");
	sbr.append("D Liu <d.liu@hud.ac.uk>\n");
	sbr.append("D. Kitchin <d.kitchin@hud.ac.uk>\n");
	team.setText(sbr.toString());

	bottomPanel.add(team, "South");
	/* end Weihong changed on 22/10/2001 */

	getContentPane ().add (topPanel, "North");
	getContentPane ().add (leftPanel, "West");
	getContentPane ().add (centerPanel, "Center");
	getContentPane ().add (bottomPanel, "South");
    }

    /**
     * Close window
     * 
     */
    private void closeDialog() {
	setVisible (false);
	dispose ();
    }

//     /**
//      * Main method to create an instance.
//      * 
//      */
//     public static void main (String args[]){
// 	 new About().show();
//     }

} // About
