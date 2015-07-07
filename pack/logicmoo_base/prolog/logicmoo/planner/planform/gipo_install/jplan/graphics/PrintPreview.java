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

package jplan.graphics;

/**
 * @author Weihong Zhao
 * PrintPreview.java
 * 14/3/2001
 */


import java.util.*;
import java.awt.*;
import java.awt.image.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;
import java.awt.print.*;

import jplan.general.Utility;

/**
 * Extended JFrame to show the content for print preview.
 */
public class PrintPreview extends JFrame {
    /**
     * 
     */
    private int pageWidth;
    /**
     * 
     */
    private int pageHeight;
    /**
     * 
     */
    private Printable target;
    /**
     * 
     */
    private JComboBox viewScale;
    /**
     * 
     */
    private PreviewContainer previewPanel;
    
    
    /**
     * Create a printpreview window
     * @param target a printable interface
     */ 
    public PrintPreview(Printable target) {
	this (target, "Print Preview");
    }
    
    /**
     * Create a printpreview window
     * @param theTarget a printable interface
     * @param title title of the print preview window
     */ 
    public PrintPreview(Printable theTarget, String title) {
	super(title);
	setSize(800, 600);
	this.target = theTarget;
	
	JToolBar tb = new JToolBar();
	JButton bt = new JButton("Print");
	bt.addActionListener(new java.awt.event.ActionListener() {
	    public void actionPerformed(ActionEvent e) {
		try {
		    PrinterJob pj = PrinterJob.getPrinterJob();
		    pj.setPrintable(target);
		    pj.print();
		    dispose();
		} catch (PrinterException ex) {
		    ex.printStackTrace();
		    System.err.println("Printing error: " + ex.toString());
		}
	    }
	});
	bt.setAlignmentY(0.5f);
	bt.setMargin(new Insets(3,5,3,5));
	tb.add(bt);
	
	bt = new JButton("Close");
	bt.addActionListener(new ActionListener(){
	    public void actionPerformed (ActionEvent e){
		dispose();
	    }
	});
	bt.setAlignmentY(0.5f);
	bt.setMargin(new Insets(2,6,2,6));
	tb.add(bt);

	String[] scales = { "10%", "25%", "50%", "100%",  "150%", "200%"};
	viewScale = new JComboBox(scales);
	viewScale.setSelectedIndex(3);
	viewScale.addActionListener(new ActionListener() {
	    public void actionPerformed (ActionEvent evt) {
		viewScaleActionPerformed();
	    }
	});
	viewScale.setMaximumSize(viewScale.getPreferredSize());
	viewScale.setEditable(true);
	tb.addSeparator();
	tb.add(viewScale);
	getContentPane().add(tb, BorderLayout.NORTH);
		       
	previewPanel = new PreviewContainer();
		       
	PrinterJob pj = PrinterJob.getPrinterJob();
	PageFormat pageFormat = pj.defaultPage();
	if (pageFormat.getHeight() == 0 || pageFormat.getWidth() == 0) {
	    System.err.println("Unable to determin default page size");
	    return;
	}
	pageWidth = (int)(pageFormat.getWidth());
	pageHeight = (int)(pageFormat.getHeight());
	int scale = 100;
	int w = (int)(pageWidth*scale/100);
	int h = (int)(pageHeight*scale/100);
	int pageIndex = 0;
	try {
	    while (true) {
		BufferedImage img = new BufferedImage(pageWidth,pageHeight, BufferedImage.TYPE_INT_RGB);
		Graphics g = img.getGraphics();
		g.setColor(Color.white);
		g.fillRect(0,0,pageWidth, pageHeight);
		if (target.print(g, pageFormat, pageIndex) != Printable.PAGE_EXISTS)
		    break;
		PagePreview pp = new PagePreview(w, h, img);
		previewPanel.add(pp);
		Utility.debugPrintln("pageIndex:"+pageIndex);
		pageIndex++;
	    }
	}
	catch (PrinterException e) {
	    e.printStackTrace();
	    System.err.println("Printing error: " + e.toString());
	}

	JScrollPane ps = new JScrollPane(previewPanel);
	getContentPane().add(ps, BorderLayout.CENTER);

	setDefaultCloseOperation(DISPOSE_ON_CLOSE);
	setVisible(true);

    }

    /**
     * When viewing scale is changed.
     * 
     */ 
    private void viewScaleActionPerformed(){
	Thread runner = new Thread() {
	    public void run() {
		String str = viewScale.getSelectedItem().toString();
		if (str.endsWith("%"))
		    str = str.substring(0, str.length()-1);
		str = str.trim();
		int scale = 0;
		try {
		    scale = Integer.parseInt(str);
		} catch(NumberFormatException ex) {return;}
		int w = (int)(pageWidth*scale/100);
		int h = (int)(pageHeight*scale/100);
		
		Component[] comps = previewPanel.getComponents();
		for (int k=0; k<comps.length; k++) {
		    if (!(comps[k] instanceof PagePreview))
			continue;
		    PagePreview pp = (PagePreview)comps[k];
		    pp.setScaledSize(w,h);

		    Utility.debugPrintln("Components:" + k);
		}
		previewPanel.doLayout();
		previewPanel.getParent().getParent().validate();
	    }
	};
	runner.start();
    }
    

    /**
     * Inner class for displaying the viewing content.
     */
    class PreviewContainer extends JPanel {
	/**
	 * Horizontal margin between pages.
	 */	
	protected int H_GAP = 16;
	/**
	 * Vertical margin between pages.
	 */	
	protected int V_GAP = 10;
	
	/**
	 * Returns the default size of this canvas.
	 * @return Dimension
	 */
	public Dimension getPreferredSize() {
	    int n = getComponentCount();
	    Utility.debugPrintln("pages:" + n);
	    if (n == 0)
		return new Dimension(H_GAP, V_GAP);
	    Component comp = getComponent(0);
	    Dimension dc = comp.getPreferredSize();
	    int w = dc.width;
	    int h = dc.height;
	    
	    Dimension dp = getParent().getSize();
	    int nCol = Math.max((dp.width - H_GAP)/(w + H_GAP), 1);
	    int nRow = n/nCol;
	    if (nRow * nCol < n)
		nRow++;

	    int ww = nCol*(w+H_GAP) + H_GAP;
	    int hh = nRow*(h+V_GAP) + V_GAP;
	    Insets ins = getInsets();
	    return new Dimension(ww + ins.left + ins.right, hh+ins.top + ins.bottom);
	}

	/**
	 * Returns the default size of this canvas.
	 * @return Dimension
	 */
	public Dimension getMaximumSize() {
	    return getPreferredSize();
	}

	/**
	 * Returns the default size of this canvas.
	 * @return Dimension
	 */
	public Dimension getMinimumSize() {
	    return getPreferredSize();
	}

	/**
	 * Causes this container to lay out its components.
	 * Most programs should not call this method directly,
	 * but should invoke the validate method instead.
	 * 
	 */
	public void doLayout() {
	    Insets ins = getInsets();
	    int x = ins.left + H_GAP;
	    int y = ins.top + V_GAP;

	    int n = getComponentCount();
	    if (n ==0)
		return;
	    Component comp = getComponent(0);
	    Dimension dc = comp.getPreferredSize();
	    int w = dc.width;
	    int h = dc.height;

	    Dimension dp = getParent().getSize();
	    int nCol = Math.max((dp.width - H_GAP)/(w+H_GAP), 1);
	    int nRow = n/nCol;
	    if (nRow*nCol<n)
		nRow++;
	    
	    int index = 0;
	    for (int k=0; k<nRow; k++) {
		for (int m=0; m<nCol; m++) {
		    if (index >= n)
			return;
		    comp = getComponent(index++);
		    comp.setBounds(x,y,w,h);
		    x += w+H_GAP;
		}
		y += h+V_GAP;
		x = ins.left + H_GAP;
	    }
	}
    }  
    
   
    /**
     * Image previews in one page.
     */
    class PagePreview extends JPanel {
	protected int m_w;
	protected int m_h;
	protected Image m_source;
	protected Image m_img;
	/**
	 * Create a PagePreview with given size.
	 * @param w width
	 * @param h height
	 * @param source the image to be viewed
	 */
	public PagePreview(int w, int h, Image source) {
	    m_w = w;
	    m_h = h;
	    m_source = source;
	    m_img = m_source.getScaledInstance(m_w, m_h, Image.SCALE_SMOOTH);
	    m_img.flush();
	    setBackground(Color.white);
	    setBorder(new MatteBorder(1,1,2,2, Color.black));
	}

	/**
	 * Set size to the given dimension.
	 * @param w width
	 * @param h height
	 * 
	 */
	public void setScaledSize(int w, int h) {
	    m_w = w;
	    m_h = h;
	    m_img = m_source.getScaledInstance(m_w, m_h, Image.SCALE_SMOOTH);
	    repaint();
	}

	/**
	 * Returns the default size of this canvas.
	 * @return Dimension
	 */
	public Dimension getPreferredSize() {
	    Insets ins = getInsets();
	    return new Dimension(m_w + ins.left + ins.right, m_h + ins.top + ins.bottom);
	}

	/**
	 * Returns the default size of this canvas.
	 * @return Dimension
	 */
	public Dimension getMaximumSize() {
	    return getPreferredSize();
	}

	/**
	 * Returns the default size of this canvas.
	 * @return Dimension
	 */
	public Dimension getMnimumSize() {
	    return getPreferredSize();
	}

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
	 * 
	 */
	public void paint(Graphics g) {
	    g.setColor (getBackground());
	    g.fillRect(0,0,getWidth(), getHeight());
	    g.drawImage(m_img, 0, 0, this);
	    paintBorder(g);
	}
    }
}
