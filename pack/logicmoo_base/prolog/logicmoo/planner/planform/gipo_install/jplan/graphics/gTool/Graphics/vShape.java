/*
 * GIPO COPYRIGHT NOTICE, LICENSE AND DISCLAIMER.
 * 
 * Copyright 2001 - 2003 by R.M.Simpson W.Zhao T.L.McCLuskey D Liu D. Kitchin
 * 
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both the
 * copyright notice and this permission notice and warranty disclaimer appear in
 * supporting documentation, and that the names of the authors or their
 * employers not be used in advertising or publicity pertaining to distribution
 * of the software without specific, written prior permission.
 * 
 * The authors and their employers disclaim all warranties with regard to this
 * software, including all implied warranties of merchantability and fitness. In
 * no event shall the authors or their employers be liable for any special,
 * indirect or consequential damages or any damages whatsoever resulting from
 * loss of use, data or profits, whether in an action of contract, negligence or
 * other tortious action, arising out of or in connection with the use or
 * performance of this software.
 */
package jplan.graphics.gTool.Graphics;
/**
 * vShape.java 24/1/2001 - Chinese New Year - Snake
 * 
 * @author Weihong Zhao
 */
import java.awt.*;
import java.awt.image.ImageObserver;
import java.lang.Math;
import java.util.Hashtable;
import java.lang.System;
import javax.swing.*;
import java.awt.geom.*;
import java.awt.event.*;
import jplan.graphics.gTool.Graphics.Double_Dimension;
import jplan.graphics.gTool.Graphics.vShapeHighlightPoint;
import jplan.graphics.gTool.Graphics.vLink;
import jplan.general.TransExpressionDocPane;
import jplan.general.Utility;
import jplan.graphics.*;
/**
 * Visual Shape (vShape) - basic shape used for graphical display. vShapes can
 * be distinguished with each other by having different colors or different
 * shapes. vShapes can be created alone as well as attched an object to
 * represent the object's graphics. vShapes support mouse drag for
 * reposition/reshape, mouse click for selection. vShapes have a textArea to
 * show a group of text and a label to show simple text. vShapes can also be
 * saved as .vm format.
 */
public class vShape extends JComponent implements Cloneable {
	/**
	 * to show the object this graphics represents
	 */
	public Object ME = null;
	/**
	 * internal id - unique int number to distinguish between shapes.
	 */
	protected int index;
	/**
	 * position of the shape
	 */
	public double px, py;
	/**
	 * size of the shape
	 */
	public double width, height;
	/**
	 * offset value from a point (normally a mouse location)
	 */
	public double offsetX, offsetY;
	/**
	 * distinguish between: circle, square, triangle, etc.
	 */
	private int shapeID = CIRCLE;
	/**
	 * text appear on the shape
	 */
	private String label;
	/**
	 *  
	 */
	private boolean drawLabel = true;
	/**
	 *  
	 */
	private boolean isSelected;
	/**
	 * last position before change
	 */
	private Point2D.Double pre_position = new Point2D.Double(px, py);
	/**
	 * last dimension before change
	 */
	private Double_Dimension pre_dimension = new Double_Dimension(width, height);
	/**
	 * for showing and editing the label
	 */
	public TransExpressionDocPane textField = new TransExpressionDocPane();
	/**
	 * margin of the text from the drawing box
	 */
	private int margin;
	/**
	 *  
	 */
	private double zoom = 1.0;
	/**
	 * for display of the shapes
	 */
	private Color[] lineColor = new Color[7];/* WZ 21/8/02 */
	/**
	 *  
	 */
	private Color[] fillColor = new Color[7];/* WZ 21/8/02 */
	/**
	 *  
	 */
	private vShapeHighlightPoint shapeHighlighter;
	/**
	 *  
	 */
	private Hashtable inLinks = new Hashtable(), outLinks = new Hashtable();
	/**
	 *  
	 */
	private int inLinkID = 0, outLinkID = 0;
	/**
	 *  
	 */
	private Font font = new Font("Arial", Font.PLAIN, 11);
	/* Weihong added on 12/11/2001 */
	/**
	 * the JGraphCanvas to be added to the vShape
	 */
	private JGraphCanvas decompCanvas;
	/**
	 * vShape type
	 */
	public final static int CIRCLE = 0;
	/**
	 * vShape type
	 */
	public final static int RECTANGLE = 1;
	/**
	 * vShape type
	 */
	public final static int ROUND_RECTANGLE = 2;
	/**
	 * to indicate inLink related
	 */
	public final static int IN = 5634;
	/**
	 * to indicate outLink related
	 */
	public final static int OUT = 5678;
	/**
	 * Create the default vShape
	 * 
	 * @param newIndex
	 *            the index of the vShape - a unique integer to distinguish with
	 *            other vShapes
	 */
	public vShape(int newIndex) {
		index = newIndex;
		px = 0.0;
		py = 0.0;
		init();
	}
	/**
	 * Create a vShape with a given size at a given location
	 * 
	 * @param newIndex
	 *            the index of the vShape - a unique integer to distinguish with
	 *            other vShapes
	 * @param x
	 *            value at X axis for the top left corner
	 * @param y
	 *            value at Y axis for the top left corner
	 * @param w
	 *            width
	 * @param h
	 *            height
	 */
	public vShape(int newIndex, double x, double y, double w, double h) {
		index = newIndex;
		px = x;
		py = y;
		width = w;
		height = h;
		init();
	}
	/**
	 * Create a vShape with a given size at a given location to represent an
	 * object
	 * 
	 * @param object
	 *            the object this vShape stands for
	 * @param newIndex
	 *            the index of the vShape - a unique integer to distinguish with
	 *            other vShapes
	 * @param x
	 *            value at X axis for the top left corner
	 * @param y
	 *            value at Y axis for the top left corner
	 * @param w
	 *            width
	 * @param h
	 *            height
	 */
	public vShape(Object object, int newIndex, double x, double y, double w,
			double h) {
		index = newIndex;
		ME = object;
		px = x;
		py = y;
		width = w;
		height = h;
		init();
	}
	/**
	 * construction of the components
	 *  
	 */
	private void init() {
		setLayout(null);
		initTextField();
		label = new String("actionID");
		isSelected = false;
		margin = 10;
		shapeHighlighter = new vShapeHighlightPoint(this);
		saveStatus();
	}
	/**
	 * construction of the textField
	 *  
	 */
	private void initTextField() {
		textField.setFont(font);
		textField.setOpaque(false);
		add(textField);
		textField.setBounds((int) px + 10, (int) py + 10, (int) width - 20,
				(int) height - 20);
	}
	/**
	 * gets the vShape ID
	 * 
	 * @return int
	 */
	public int getID() {
		return index;
	}
	/**
	 * remove textfield
	 *  
	 */
	public void removeTextField() {
		remove(textField);
	}
	/* Weihong added on 12/11/2001 */
	/**
	 * remove decompCanvas
	 *  
	 */
	public void addDecompCanvas(JGraphCanvas decomp) {
		add(decomp);
		decompCanvas = decomp;
		decompCanvas.setBounds((int) width / 6, (int) height / 6,
				(int) width * 2 / 3, (int) height * 2 / 3);
		decompCanvas.setBorder(new javax.swing.border.BevelBorder(1));
	}
	/* Weihong added on 12/11/2001 */
	/**
	 * remove decompCanvas
	 *  
	 */
	public JGraphCanvas getDecompCanvas() {
		return decompCanvas;
	}
	/* Weihong added on 12/11/2001 */
	/**
	 * remove decompCanvas
	 *  
	 */
	public void removeDecompCanvas() {
		remove(decompCanvas);
	}
	/**
	 * attach an object to the vShape
	 * 
	 * @param obj
	 *            the object to attach to the vShape
	 *  
	 */
	public void setObject(Object obj) {
		ME = obj;
	}
	/**
	 * gets the object attached to the vShape
	 * 
	 * @return the object attached to the vShape
	 */
	public Object getObject() {
		return ME;
	}
	/**
	 * gets the in links
	 * 
	 * @return inLinks hashtable
	 */
	public Hashtable getInLinks() {
		return inLinks;
	}
	/**
	 * sets the inlinks
	 * 
	 * @param h
	 *            inlinks hashtable
	 *  
	 */
	public void setInLinks(Hashtable h) {
		inLinks = h;
	}
	/**
	 * clear all items in the inlinks hashtable
	 *  
	 */
	public void resetInLinks() {
		inLinks.clear();
		inLinkID = 0;
	}
	/**
	 * gets the outlinks
	 * 
	 * @return outlinks hashtable
	 */
	public Hashtable getOutLinks() {
		return outLinks;
	}
	/**
	 * sets the outlinks
	 * 
	 * @param h
	 *            outlinks hashtable
	 *  
	 */
	public void setOutLinks(Hashtable h) {
		outLinks = h;
	}
	/**
	 * clear all items in the outlinks hashtable
	 *  
	 */
	public void resetOutLinks() {
		outLinks.clear();
		outLinkID = 0;
	}
	/**
	 * sets the top left corner of the vShape
	 * 
	 * @param new_x
	 *            double value at X axis
	 * @param new_y
	 *            double value at Y axis
	 *  
	 */
	public void setPosition(double new_x, double new_y) {
		px = new_x;
		py = new_y;
		shapeHighlighter.resetPosition();//adjust the position of the
										 // highlightingArea
		setLinksPosition();
		saveStatus();
		textField.setBounds((int) px + 10, (int) py + 10, (int) width - 20,
				(int) height - 20);
	}
	/**
	 * gets the top left corner of the vShape
	 * 
	 * @return Point2D.Double value the top left corner
	 */
	public Point2D.Double getPosition() {
		Point2D.Double position = new Point2D.Double(px, py);
		return position;
	}
	/**
	 * clone a copy of this vShape
	 * 
	 * @return Object
	 */
	public Object clone() {
		vShape copy = new vShape(ME, index, px, py, width, height);
		copy.setLabel(label);
		// 	if (shapeID != 0) {
		// 	if (isAncestorOf(textField)){/* WZ 10/6/02 */
		if (!drawLabel) {/* WZ 11/6/02 */
			copy.remove(copy.textField);
			copy.textField = (TransExpressionDocPane) this.textField.clone();
			copy.initTextField();
		}
		copy.setInLinks((Hashtable) getInLinks().clone());
		copy.setOutLinks((Hashtable) getOutLinks().clone());
		copy.setShapeID(shapeID);
		copy.setInLinkID(inLinkID);
		copy.setOutLinkID(outLinkID);
		copy.drawLabel = drawLabel;
		copy.setSelected(isSelected); /* WZ 10/6/02 */
		return copy;
	}
	/**
	 * resize this vShape and readjust the position of the highlightingArea and
	 * save status for the undo/redo
	 * 
	 * @param new_width
	 *            double value of width
	 * @param new_height
	 *            double value of height
	 *  
	 */
	public void setSize(double new_width, double new_height) {
		width = new_width;
		height = new_height;
		shapeHighlighter.resetPosition(); //adjust the position of the
										  // highlightingArea
		setLinksPosition();
		saveStatus();
		textField.setBounds((int) px + 10, (int) py + 10, (int) width - 20,
				(int) height - 20);
	}
	/* Weihong added on 12/11/2001 */
	/**
	 * resize the width of this vShape and readjust the position of the
	 * highlightingArea and save status for the undo/redo
	 * 
	 * @param new_width
	 *            double value of width
	 *  
	 */
	public void setWidth(double new_width) {
		width = new_width;
		shapeHighlighter.resetPosition(); //adjust the position of the
										  // highlightingArea
		setLinksPosition();
		saveStatus();
		textField.setBounds((int) px + 10, (int) py + 10, (int) width - 20,
				(int) height - 20);
	}
	/* Weihong added on 12/11/2001 */
	/**
	 * resize the height of this vShape and readjust the position of the
	 * highlightingArea and save status for the undo/redo
	 * 
	 * @param new_height
	 *            double value of height
	 *  
	 */
	public void setHeight(double new_height) {
		height = new_height;
		shapeHighlighter.resetPosition(); //adjust the position of the
										  // highlightingArea
		setLinksPosition();
		saveStatus();
		textField.setBounds((int) px + 10, (int) py + 10, (int) width - 20,
				(int) height - 20);
	}
	/**
	 * gets the size of the vShape
	 * 
	 * @return Double_Dimension value of the size
	 */
	public Double_Dimension getDoubleSize() {
		Double_Dimension box = new Double_Dimension(width, height);
		return box;
	}
	/**
	 * Reset the size of the vShape to show all text in textField.
	 *  
	 */
	public void wrapText() {
		//needs a bit more thinking. Weihong 8/11/01
		setSize((double) textField.getSize().width + 20, (double) textField
				.getSize().height + 20);
	}
	/**
	 * sets the label which appears on the vLink
	 * 
	 * @param new_label
	 *            String value appears on the vLink
	 *  
	 */
	public void setLabel(String new_label) {
		label = new_label;
	}
	/**
	 * gets the label which appears on the vLink
	 * 
	 * @return label which appears on the vLink
	 */
	public String getLabel() {
		return label;
	}
	/**
	 * switch on/off the label's visibility
	 * 
	 * @param TorF
	 *            true or false
	 *  
	 */
	public void setDrawLabel(boolean TorF) {
		drawLabel = TorF;
	}
	/* WZ 17/6/02 */
	/**
	 * return true if label should be drawn
	 * 
	 * @return true if label should be drawn
	 */
	public boolean getDrawLabel() {
		return drawLabel;
	}
	/**
	 * get the type of the vShape
	 * 
	 * @return int value
	 */
	public int getShapeID() {
		return shapeID;
	}
	/**
	 * sets the shape type
	 * 
	 * @param s
	 *            choose between CIRCLE, RECTANGLE, and ROUND_RECTANGLE
	 *  
	 */
	public void setShapeID(int s) {
		shapeID = s;
	}
	/**
	 * gets the inlink id
	 * 
	 * @return int value between IN or OUT
	 */
	public int getInLinkID() {
		return inLinkID;
	}
	/**
	 * sets the inlink id
	 * 
	 * @param s
	 *            choose between IN and OUT
	 *  
	 */
	public void setInLinkID(int s) {
		inLinkID = s;
	}
	/**
	 * gets the outlink id
	 * 
	 * @return int value between IN or OUT
	 */
	public int getOutLinkID() {
		return outLinkID;
	}
	/**
	 * sets the outlink id
	 * 
	 * @param s
	 *            choose between IN and OUT
	 *  
	 */
	public void setOutLinkID(int s) {
		outLinkID = s;
	}
	/**
	 * selecte/deselecte vShape
	 * 
	 * @param selected
	 *            true or false
	 *  
	 */
	public void setSelected(boolean selected) {
		isSelected = selected;
		shapeHighlighter.setVisible(selected);
	}
	/**
	 * returns true if the vShape is highlighted/selected
	 * 
	 * @return boolean
	 */
	public boolean getSelected() {
		return isSelected;
	}
	/**
	 * register the vLink with the inlinks/outlinks
	 * 
	 * @param mySwitch
	 *            IN or OUT
	 * @param vl
	 *            the vLink to be registered
	 *  
	 */
	public void registerLinks(int mySwitch, vLink vl) {
		switch (mySwitch) {
			case IN :
				inLinkID++;
				inLinks.put("inlinks" + inLinkID, vl);
				break;
			case OUT :
				outLinkID++;
				outLinks.put("outlinks" + outLinkID, vl);
				break;
		}
	}
	/**
	 * remove the vLink from the inlinks/outlinks
	 * 
	 * @param mySwitch
	 *            IN or OUT
	 * @param vl
	 *            the vLink to be registered
	 *  
	 */
	public void removeLinks(int mySwitch, vLink vl) {
		switch (mySwitch) {
			case IN :
				for (int i = 1; i < inLinks.size() + 1; i++) {
					if (((vLink) inLinks.get("inlinks" + i)).equals(vl)) {
						vLink vlk = (vLink) inLinks.get("inlinks" + i);
						inLinks.remove("inlinks" + i);
						inLinkID = refreshHashtableList(inLinks, "inlinks",
								inLinkID);
						break;
					}
				}
				break;
			case OUT :
				for (int i = 1; i < outLinks.size() + 1; i++) {
					if (((vLink) outLinks.get("outlinks" + i)).equals(vl)) {
						vLink vlk = (vLink) outLinks.get("outlinks" + i);
						outLinks.remove("outlinks" + i);
						outLinkID = refreshHashtableList(outLinks, "outlinks",
								outLinkID);
						break;
					}
				}
				break;
		}
	}
	/**
	 * refresh hashtable by filling the blank place with the last item on the
	 * list
	 * 
	 * @param htb
	 *            the inlinks or outlinks
	 * @param linkKey
	 *            the object key
	 * @param linkIndex
	 *            index of the link
	 * @return int - the latest index of the link
	 */
	private int refreshHashtableList(Hashtable htb, String linkKey,
			int linkIndex) {
		int i, j;
		vLink vl;
		for (i = 1; i < htb.size() + 1; i++) {
			if (!htb.containsKey(linkKey + i)) {
				if (!(i == linkIndex)) { // not the last link in the list was
										 // removed.
					//search from the last record until there is a non-empty
					// record.
					while (!htb.containsKey(linkKey + linkIndex)
							&& linkIndex > i) {
						linkIndex--;
					}
					vl = (vLink) htb.get(linkKey + linkIndex);//linkIndex is
															  // the last link
															  // created.
					htb.put(linkKey + i, vl); //move the last record to the
											  // current blank place.
					htb.remove(linkKey + linkIndex);
				} else {
				} //if the last record was removed then reset the last link
				  // index - linkIndex to --.
				linkIndex--;
			}
		}
		linkIndex = htb.size(); //to make sure the last link in the list.
		return linkIndex;
	}
	/**
	 * auto reshape links after the vShape has been moved or reshaped
	 *  
	 */
	private void setLinksPosition() {
		double x, y;
		int i, hh;
		vLink vl;
		for (i = 1; i < inLinks.size() + 1; i++) {
			vl = (vLink) inLinks.get("inlinks" + i);
			vl.pickClosestHighlightingPoint();
			hh = vl.getJointID(vLink.TO);
			x = shapeHighlighter.hp[hh].cx;
			y = shapeHighlighter.hp[hh].cy;
			vl.setStopPosition(x, y);
			hh = vl.getJointID(vLink.FROM);
			x = vl.getStartShape().shapeHighlighter.hp[hh].cx;
			y = vl.getStartShape().shapeHighlighter.hp[hh].cy;
			vl.setStartPosition(x, y);
		}
		for (i = 1; i < outLinks.size() + 1; i++) {
			vl = (vLink) outLinks.get("outlinks" + i);
			vl.pickClosestHighlightingPoint();
			hh = vl.getJointID(vLink.FROM);
			x = shapeHighlighter.hp[hh].cx;
			y = shapeHighlighter.hp[hh].cy;
			vl.setStartPosition(x, y);
			hh = vl.getJointID(vLink.TO);
			x = vl.getStopShape().shapeHighlighter.hp[hh].cx;
			y = vl.getStopShape().shapeHighlighter.hp[hh].cy;
			vl.setStopPosition(x, y);
		}
	}
	/**
	 * gets the offset of the top left corner to the given point
	 * 
	 * @param p
	 *            the mouse point
	 * @return Double_Dimension
	 */
	public Double_Dimension getOffset(Point p) {
		offsetX = (double) p.x - px;
		offsetY = (double) p.y - py;
		return new Double_Dimension(offsetX, offsetY);
	}
	/**
	 * check if the given point is inside the bound of the vShape
	 * 
	 * @param x
	 *            double value at X axis
	 * @param y
	 *            double value at Y axis
	 * @return boolean
	 */
	public boolean contains(double x, double y) {
		if (px <= x && (px + width) >= x && (py) <= y && py + height >= y)
			return true;
		else
			return false;
	}
	/**
	 * gets the shapeHighlighter
	 * 
	 * @return vShapeHighlightPoint
	 */
	public vShapeHighlightPoint getShapeHighlighter() {
		return shapeHighlighter;
	}
	/**
	 * repaint the shape and its contained component textField
	 * 
	 * @param g
	 *            vShape graphics
	 *  
	 */
	public synchronized void paintComponent(Graphics g) {
		// 	jplan.general.Utility.debugPrintln("- paintComponent -");
		super.paintComponent(g);
		drawIt(g);
	}
	/**
	 * draws the boundary line
	 * 
	 * @param mySwitch
	 *            int value color
	 * @param g
	 *            Graphics2D
	 * @param s
	 *            shape type
	 *  
	 */
	private void drawOutLine(int mySwitch, Graphics2D g, Shape s) {
		g.setColor(lineColor[mySwitch]);
		g.draw(s);
	}
	/**
	 * fills the shape
	 * 
	 * @param mySwitch
	 *            in value color
	 * @param g
	 *            Graphics2D
	 * @param s
	 *            shape type
	 *  
	 */
	private void fillShape(int mySwitch, Graphics2D g, Shape s) {
		g.setColor(fillColor[mySwitch]);
		g.fill(s);
	}
	/**
	 * draws the shape, label, shapeHighlighter, but not the textField
	 * 
	 * @param g
	 *            Graphics2D
	 *  
	 */
	public void drawIt(Graphics g) {
		// 	jplan.general.Utility.debugPrintln("Painting vShape: " + getLabel());
		Graphics2D graphics = (Graphics2D) g;
		graphics.setStroke(new BasicStroke(1));
		Point2D.Double position = new Point2D.Double(px, py);
		double w = width;
		double h = height;
		//draw shape
		fillColor[0] = new Color(184, 255, 204); //circle
		fillColor[1] = new Color(224, 161, 141); //rectangle
		fillColor[2] = new Color(159, 165, 206); //round rectangle
		fillColor[3] = new Color(173, 234, 218); //
		fillColor[4] = new Color(232, 188, 218); //	  
		fillColor[5] = new Color(182, 179, 246); //
		fillColor[6] = new Color(255, 46, 0); /* WZ 21/8/02 */
		if (!isSelected) {
			for (int xx = 0; xx < 7; xx++) {/* WZ 21/8/02 */
				lineColor[xx] = Color.black;
			}
		} else {
			for (int l = 0; l < 7; l++)
				/* WZ 21/8/02 */
				lineColor[l] = Color.red;
		}
		if (shapeID == CIRCLE) {
			Shape sh = new Ellipse2D.Double(px, py, w, h);
			fillShape(shapeID, graphics, sh);
			drawOutLine(shapeID, graphics, sh);
		} else if (shapeID == RECTANGLE) {
			Shape sh = new Rectangle((int) px, (int) py, (int) w, (int) h);
			fillShape(shapeID, graphics, sh);
			drawOutLine(shapeID, graphics, sh);
		} else if (shapeID == ROUND_RECTANGLE) {
			Shape sh = new RoundRectangle2D.Double(px, py, w, h, 10, 10);
			fillShape(shapeID, graphics, sh);
			drawOutLine(shapeID, graphics, sh);
		} else if (shapeID == 3) {
			Shape sh = new Rectangle((int) px, (int) py, (int) w, (int) h);
			fillShape(shapeID, graphics, sh);
			drawOutLine(shapeID, graphics, sh);
		} else if (shapeID == 4) {
			Shape sh = new RoundRectangle2D.Double(px, py, w, h, 10, 10);
			fillShape(shapeID, graphics, sh);
			drawOutLine(shapeID, graphics, sh);
		} else if (shapeID == 5) {
			Shape sh = new Ellipse2D.Double(px, py, w, h);
			fillShape(shapeID, graphics, sh);
			drawOutLine(shapeID, graphics, sh);
		} else if (shapeID == 6) {/* WZ 21/8/02 */
			Shape sh = new Ellipse2D.Double(px, py, w, h);
			fillShape(shapeID, graphics, sh);
			drawOutLine(shapeID, graphics, sh);
		}
		//draw label
		/* WZ 13/6/02 change to multiline label */
		if (drawLabel) {
			double lx = 0.0, ly = 0.0;
			FontMetrics fm = graphics.getFontMetrics();
			double theWidth = fm.stringWidth(label);
			double theHeight = fm.getHeight();
			double margin = 2.0;
			double lineSpace = -5.0;
			double lengthPerLine = width - 2 * margin;
			int totalLine = (int) (theWidth / lengthPerLine) + 1;
			graphics.setColor(Color.black);
			String tmpLabel = new String();
			int totalLength = label.length();
			for (int i = 0; i < totalLine; i++) {
				tmpLabel = label.substring((int) (i * totalLength / totalLine),
						(int) ((i + 1) * totalLength / totalLine));
				lx = px + width / 2 - fm.stringWidth(tmpLabel) / 2;
				ly = (theHeight + lineSpace) * (i + 1) + py + height / 2
						- (theHeight + lineSpace) * totalLine / 2;
				graphics.drawString(tmpLabel, (int) lx, (int) ly);
			}
		}
		//paint the highlighting square, if not being selected then the visible
		// property of the highlighter will be set to false;
		shapeHighlighter.paint(g);
	}
	/**
	 * to print out to a file or a computer
	 * 
	 * @param graphics
	 *            Graphics
	 *  
	 */
	public void printIt(Graphics graphics) {
		/* WZ 13/6/02 change to multiline label */
		boolean changed = false;
		if (!drawLabel) {
			drawLabel = true;
			changed = true;
		}
		drawIt(graphics);
		if (changed)
			drawLabel = false;
	}
	/**
	 * save current position and size for undo/redo
	 *  
	 */
	public void saveStatus() {
		pre_position = new Point2D.Double(px, py);
		pre_dimension = new Double_Dimension(width, height);
	}
	/**
	 * zoom in/out
	 * 
	 * @param zoom_Rate
	 *            zoom in if more than 1; zoom out if less than 1
	 *  
	 */
	public void scale(double zoom_Rate) {
		if (pre_dimension == null)
			return;
		setSize(zoom_Rate * pre_dimension.width, zoom_Rate
				* pre_dimension.height);
		setPosition(px * zoom_Rate, py * zoom_Rate);
		zoom = zoom_Rate;
		saveStatus();
	}
	/**
	 * gets bounds with double data type
	 * 
	 * @return Rectangle2D.Double
	 */
	public Rectangle2D.Double getDoubleBounds() {
		return new Rectangle2D.Double(px, py, width, height);
	}
	/**
	 * sets bounds with double data type
	 * 
	 * @param x -
	 *            double value of top left corner on X axis
	 * @param y -
	 *            double value of top left corner on Y axis
	 * @param w -
	 *            width
	 * @param h -
	 *            height
	 *  
	 */
	public void setDoubleBounds(double x, double y, double w, double h) {
		setPosition(x, y);
		setSize(w, h);
	}
	/* WZ 10/6/02 */
	public String toString() {
		return label;
	}
	/**
	 * a string expression of the vShape, mainly used for saving purpose
	 * 
	 * @return String
	 */
	public String to_String() {
		StringBuffer str = new StringBuffer();
		str.append("BEGIN VSHAPE\n");
		jplan.general.Utility.debugPrintln("BEGIN VSHAPE\n");
		str.append("index:" + index + "\n");
		str.append("label:" + label + "\n");
		str.append("px:" + px + "\n");
		str.append("py:" + py + "\n");
		str.append("width:" + width + "\n");
		str.append("height:" + height + "\n");
		str.append("shapeID:" + shapeID + "\n");
		str.append("isSelected:" + isSelected + "\n");
		str.append("drawLabel:" + drawLabel + "\n");/* WZ 14/6/02 */
		str.append("inLinkID:" + inLinkID + "\n");
		str.append("outLinkID:" + outLinkID + "\n");
		jplan.general.Utility.debugPrintln("(In Links)\n");
		str.append("(In Links):\n");
		str.append("linkID(");
		for (int i = 1; i < inLinks.size() + 1; i++) {
			str.append(((vLink) inLinks.get("inlinks" + i)).getID());
			if (i != inLinks.size())
				str.append(",");
		}
		str.append(")\n");
		jplan.general.Utility.debugPrintln("(Out Links)\n");
		str.append("(Out Links):\n");
		str.append("linkID(");
		for (int i = 1; i < outLinks.size() + 1; i++) {
			str.append(((vLink) outLinks.get("outlinks" + i)).getID());
			if (i != outLinks.size())
				str.append(",");
		}
		str.append(")\n");
		str.append("END VSHAPE\n");
		return str.toString();
	}
	//     public String to_String() {
	// 	StringBuffer str = new StringBuffer();
	// 	str = str.append( "BEGIN VSHAPE\n");
	// 	jplan.general.Utility.debugPrintln("BEGIN VSHAPE\n");
	// 	str = str.append( "index:"+index+"\n");
	// 	str = str.append( "label:"+label+"\n");
	// 	str = str.append( "px:" +px + "\n");
	// 	str = str.append( "py:" +py + "\n");
	// 	str = str.append( "width:" +width + "\n");
	// 	str = str.append( "height:" +height + "\n");
	// 	str = str.append( "shapeID:" +shapeID + "\n");
	// 	str = str.append( "isSelected:" +isSelected + "\n");
	// 	str = str.append( "drawLabel:" +drawLabel + "\n");/* WZ 14/6/02 */
	// 	str = str.append( "object:" +ME.toString() + "\n");/* WZ 14/6/02 */
	// 	str = str.append( "inLinkID:" +inLinkID + "\n");
	// 	str = str.append( "outLinkID:" +outLinkID + "\n");
	// 		jplan.general.Utility.debugPrintln("(In Links)\n");
	// 	str = str.append( "(In Links):\n");
	// 	str = str.append("linkID(");
	// 	for (int i=1;i<inLinks.size()+1;i++) {
	// 	    str = str.append(((vLink)inLinks.get("inlinks"+i)).getID());
	// 	    if (i != inLinks.size())
	// 		str = str.append(",");
	// 	}
	// 	str = str.append(")\n");
	// 			jplan.general.Utility.debugPrintln("(Out Links)\n");
	// 	str = str.append( "(Out Links):\n");
	// 	str = str.append("linkID(");
	// 	for (int i=1;i<outLinks.size()+1;i++) {
	// 	    str = str.append(((vLink)outLinks.get("outlinks"+i)).getID());
	// 	    if (i != outLinks.size())
	// 		str = str.append(",");
	// 	}
	// 	str = str.append(")\n");
	// 	str = str.append( "END VSHAPE\n");
	// 	return str.toString();
	//     }
	/**
	 * gets the default size
	 * 
	 * @return Dimension
	 */
	public Dimension getPreferredSize() {
		return new Dimension((int) width, (int) height);
	}
}