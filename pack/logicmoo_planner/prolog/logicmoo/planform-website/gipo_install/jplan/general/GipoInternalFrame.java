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
 * GipoInternalFrame.java
 *
 *
 * Created: Tue Oct 23 16:21:26 2001
 *
 * @author W Zhao
 * @version
 */

import javax.swing.*;
import javax.swing.event.*;
import jplan.top.OclEd;

/**
 * This is a high level JInternalFrame with function of registration with its parent's menu.
 */
public class GipoInternalFrame extends JInternalFrame implements InternalFrameListener {
    JCheckBoxMenuItem tmpMenuItem = null;
    private OclEd parent;

    //Ron 28/12/01 added to support patterns
    /**
     * Creates an instance of GipoInternalFrame to work with OclEd JFrame.
     * No parameter constructor use with register method
     */
    public GipoInternalFrame() {
	super("",true,true,true,true);
	// Ron Removed the setting of a default size 5/4/02
	// setPreferredSize(new java.awt.Dimension(750, 500)); /* Weihong added on 5/2/02 */
    }

    /**
     * Creates an instance of GipoInternalFrame to work with OclEd JFrame.
     * @param parent parent frame
     */
    public GipoInternalFrame(OclEd parent) {
	super("",true,true,true,true);
	this.parent = parent;
	addInternalFrameListener(this);
	setPreferredSize(new java.awt.Dimension(750, 500)); /* Weihong added on 5/2/02 */
    }

    // Ron 5/4/02 Constructor for patterns
    // that do not want the default size
    /**
     * Creates an instance of GipoInternalFrame to work with OclEd JFrame.
     * @param parent parent frame
     * @param noSize - if true do not set preferred size
     */
    public GipoInternalFrame(OclEd parent,boolean noSize) {
	super("",true,true,true,true);
	this.parent = parent;
	addInternalFrameListener(this);
	if (! noSize)
	    setPreferredSize(new java.awt.Dimension(750, 500));
    }

    //Ron 28/12/01 added to support patterns
    /**
     * registerWindow
     * @param parent  the top level parent window
     */
    public void registerWindow(OclEd parent) {
	this.parent = parent;
	addInternalFrameListener(this);
    }

    public OclEd getTheParent(){ /* Weihong added on 1/11/01 */
	return parent;
    }

    /**
     * Invoked when an internal frame is activated.
     * @param e InternalFrameEvent
     */
    public void internalFrameActivated(InternalFrameEvent e) {
	parent.setActiveFrame(this, true);
    }
    
    /**
     * Invoked when an internal frame is closed and remove its registration from its parent's menu.
     * @param e InternalFrameEvent
     */
    public void internalFrameClosed(InternalFrameEvent e) {
	 parent.removeOnMenubar(this);
     }
    
    /**
     * Invoked when an internal frame is clsing.
     * @param e InternalFrameEvent
     */
    public void internalFrameClosing(InternalFrameEvent e) {

    }

    /**
     * Invoked when an internal frame is deactived.
     * @param e InternalFrameEvent
     */
    public void internalFrameDeactivated(InternalFrameEvent e) {
	parent.setActiveFrame(this, false);
    }

    /**
     * Invoked when an internal frame is deiconified.#
     * @param e InternalFrameEvent
     */
    public void internalFrameDeiconified(InternalFrameEvent e) {

    }

    /**
     * Invoked when an internal frame is iconified.
     * @param e InternalFrameEvent
     */
     public void internalFrameIconified(InternalFrameEvent e) {

    }

     /**
     * Invoked when an internal frame is opened to register the frame on a menu and set to active.
     * @param e InternalFrameEvent
     */
    public void internalFrameOpened(InternalFrameEvent e) {
	 tmpMenuItem = parent.registerOnMenubar(getTitle());
	 tmpMenuItem.addActionListener(new java.awt.event.ActionListener () {
	     public void actionPerformed (java.awt.event.ActionEvent evt) {
		 try {
		     setSelected(tmpMenuItem.getState());
		 }catch (java.beans.PropertyVetoException _e) {
		     
		 }
	     }
	 }
				       );
    }
} // GipoInternalFrame
