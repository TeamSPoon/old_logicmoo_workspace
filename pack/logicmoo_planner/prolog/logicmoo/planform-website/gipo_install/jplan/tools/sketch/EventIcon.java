/**
 * DomainIcon representing an Event in the Domain
 * @author Graeme Elliott
 * 31/07/03
 */

package jplan.tools.sketch;


import java.awt.*;
import java.awt.geom.*;

public class EventIcon extends DomainIcon {
	
	/**
	 * Width of the rectangles making up the cross.
	 */
	private final double recWidth = 6.0;
	/**
	 * Start coordinates of the rectangles making up the cross.
	 */
	private final double xStart = ((double)WIDTH - recWidth)/2.0;
	private final double yStart = ((double)HEIGHT - recWidth)/2.0;
	
	/**
	 * Creates an instance of EventIcon.
	 * @param drawableObject the DrawableObject associated with the icon.
	 */
	public EventIcon(DrawableObject drawableObj, int colourIndex) {
		super(drawableObj, colourIndex);
	}
	
	/**
	 * Overrides paintComponent in class JComponent.
	 * Calls drawIt() to peform custom painting.
	 * @param g ActionIcon graphics.
	 */
	public synchronized void paintComponent(Graphics g) {
		super.paintComponent(g);
		drawIt(g);
	}
		
	/**
	 * Draws a cross with GradientPaint.
	 * The colour of the GradientPaint starts at darkBlue at the bottom-left
	 * of the icon and transitions to lightBlue at the top-right.
	 * @param g EventIcon graphics.
	 */
	private void drawIt(Graphics g) {
		Graphics2D g2 = (Graphics2D) g;
		setPaint(g2);
		/*
		 * create the vertical and horizontal components of the cross
		 * fill the cross
		 */
		Shape vertCross = new Rectangle2D.Double(xStart,0.0,recWidth,(double)getHeight());
		Shape horizCross = new Rectangle2D.Double(0.0,yStart,(double)getWidth(),recWidth);
		g2.fill(vertCross);
		g2.fill(horizCross);
	}
}

