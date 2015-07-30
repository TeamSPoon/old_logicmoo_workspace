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
 * DNDList.java
 *
 *
 * Created: Thu Nov 29 11:13:33 2001
 *
 * @author W Zhao
 * @version
 */
import java.awt.*;
import java.awt.dnd.*;
import java.awt.datatransfer.*;

import java.util.Hashtable;
import java.util.List;
import java.util.Iterator;

import java.io.*;
import java.io.IOException;

import javax.swing.JList;
import javax.swing.DefaultListModel;

/**
 * JList has been used as a droppable target
 * and a draggable source.
 */
public class DNDList extends JList implements DragSourceListener, DragGestureListener {
    
  /**
   * enables this component to be a Drag Source
   */
  private DragSource dragSource = null;


  /**
   * constructor - initializes the DropTarget and DragSource.
   */

  public DNDList() {
      dragSource = new DragSource();
      dragSource.createDefaultDragGestureRecognizer(this, DnDConstants.ACTION_COPY, this);
  }

  /**
   * a drag gesture has been initiated
   */
  public void dragGestureRecognized(DragGestureEvent event) {
      jplan.general.TransferableObject selected = (jplan.general.TransferableObject)getSelectedValue();
      if (selected != null ){
	  dragSource.startDrag (event, DragSource.DefaultCopyDrop, selected, this);
      } 
  }

  /**
   *  is invoked when the dragging has ended
   */
  public void dragDropEnd (DragSourceDropEvent event) { }

  /**
   *  is invoked when the dragging has entered the DropSite
   */
  public void dragEnter (DragSourceDragEvent event) {}

  /**
   * is invoked when the dragging has exited the DropSite
   */
  public void dragExit (DragSourceEvent event) {}

  /**
   * is invoked when the dragging is currently ocurring over the DropSite
   */
  public void dragOver (DragSourceDragEvent event) {}

  /**
   * is invoked when the user changes the dropAction
   */
  public void dropActionChanged ( DragSourceDragEvent event) {}

  
} // DNDList
