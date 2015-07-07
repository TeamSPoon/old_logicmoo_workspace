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
 * GipoTab.java
 *
 *
 * Created: Mon Nov 26 10:06:15 2001
 *
 * @author W Zhao
 * @version
 */
import javax.swing.*;
import jplan.graphics.transition.*;

/**
 * This is a tab structured panel which contains JGraphCanvas.
 */
public class GipoTab extends JTabbedPane {
    private HighLevelTransitionWindow parentFrame;

    /**
     * Constructor
     */
    public GipoTab(HighLevelTransitionWindow parent) {
	super(); 
	/* Weihong added on 28/11/2001 */
	parentFrame = parent;
	addChangeListener(new javax.swing.event.ChangeListener(){
	    public void stateChanged(javax.swing.event.ChangeEvent ce){
// 		jplan.graphics.JGraphCanvas jcanvas =  getSelectedCanvas();
		if (getSelectedIndex() == 0)
		    parentFrame.setBottomToolBarEnabled(true);
		else
		    parentFrame.setBottomToolBarEnabled(false);
	    }
	});
    }

    /**
     * To add a tab in this tabPane to show compound oprerator's detail
     * @param title the title appears on the tab button
     * @param parentFrame the ActionWindow to put on the tab window
     */
    public void addMethodTab(String title, HighLevelTransitionWindow parentFrame){
	HighLevelTransitionCanvas methodCanvas = new MethodPropertyCanvas(parentFrame);/* Weihong added on 4/12/2001 */
	methodCanvas.setWorkingDomain(parentFrame.getWorkingDomain());
	JScrollPane jsp = new JScrollPane(methodCanvas);
	methodCanvas.setDefaultColor(new java.awt.Color(255, 248, 236));
	addTab(title, jsp);
    }

    /**
     * To add a tab in this tabPane to show oprerator detail
     * @param title the title appears on the tab button
     * @param parentFrame the ActionWindow to put on the tab window
     */
    public void addOPTab(String title, ActionWindow parentFrame){
	TransitionCanvas OPCanvas = new OperatorPropertyCanvas(parentFrame);/* Weihong added on 4/12/2001 */
	OPCanvas.setWorkingDomain(parentFrame.getWorkingDomain());
	JScrollPane jsp = new JScrollPane(OPCanvas);
	OPCanvas.setDefaultColor(new java.awt.Color(216, 237, 255));
	addTab(title, jsp);
    }

    /**
     * To add a tab in this tabPane.
     * If the tab with save title exists, then active the existing one.
     * @param tabLabel the title appears on the tab button
     * @param component the component put on the tab window
     */
    public void addTab(String tabLabel, java.awt.Component component){
	for (int i = 0; i<getTabCount(); i++){
	    if (tabLabel.equals(getTitleAt(i))){
		setSelectedIndex(indexOfTab(tabLabel));
		return;
	    }
	}
	super.addTab(tabLabel, component);
	setSelectedComponent(component);
    }

    /**
     * To add a tab in this tabPane to show statics detail
     * @param title the title appears on the tab button
     * @param parentFrame the ActionWindow to put on the tab window
     */
    public void addAchieveTab(String title, ActionWindow parentFrame){
	TransitionCanvas achieveCanvas = new TransitionCanvas(parentFrame);
	achieveCanvas.setWorkingDomain(parentFrame.getWorkingDomain());
	JScrollPane jsp = new JScrollPane(achieveCanvas);
	achieveCanvas.setDefaultColor(new java.awt.Color(255, 194, 233));
	// 	achieveCanvas.setEditMode(true); / *WZ 27/3/02 */
	achieveCanvas.setScale(3);
	addTab(title, jsp);
    }

    /**
     * Returns the selected canvas
     * @return the selected canvas
     */ 
    public jplan.graphics.JGraphCanvas getSelectedCanvas(){
	JScrollPane comp = (JScrollPane)getSelectedComponent();
	return (jplan.graphics.JGraphCanvas)comp.getViewport().getView();
    }
 
    /* Weihong added on 4/12/2001 */
    /**
     * Returns true if the given title is already existing in this tabPane.
     * @param theTitle the given title
     * @return true if the given title is already existing in this tabPane.
     */ 
    public boolean hasTitle(String theTitle){
	for (int i = 0; i< getTabCount(); i++){
	    if (theTitle.equals(getTitleAt(i)))
		return true;
	}
	return false;
    }

    /* Weihong added on 4/12/2001 */
    /**
     * Returns true if the given title is already existing in this tabPane.
     * param oldTitleParent
     * @param theTitleParent the given title
     */ 
    public void refreshTitleParent(String oldTitleParent, String theTitleParent){
	for (int i = 1; i< getTabCount(); i++){
	    String str = getTitleAt(i);
	    if (str.startsWith(oldTitleParent)){
		StringBuffer sb = new StringBuffer();
		sb.append(theTitleParent);
		sb.append(str.substring(oldTitleParent.length()));
		setTitleAt(i, sb.toString());
	    }
	}
    }

    /* WZ 2/4/02 */
    /**
     * keep the basic tab pane (the first pane), remove others.
     */
    public void rmExtraTabs() {
	int j = getTabCount();
	for (int i = j-1; i>0; i--){
	    remove(i);
	}
    }

} // GipoTab
