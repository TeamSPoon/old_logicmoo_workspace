/**
 * DomainIcon representing an Action
 * @author Graeme Elliott
 * 31/07/03
 */


package jplan.tools.sketch;


import java.awt.*;
import java.awt.geom.*;

public class ActionIcon extends DomainIcon {
	
	/**
	 * Creates an instance of ActionIcon.
	 * @param drawableObject the DrawableObject associated with the icon.
	 */
	public ActionIcon(DrawableObject drawableObj, int colourIndex) {
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
	 * Draws a circle with GradientPaint.
	 * The colour of the GradientPaint starts at darkRed at the bottom-left
	 * of the icon and transitions to lightRed at the top-right.
	 * @param g ActionIcon graphics.
	 */
	private void drawIt(Graphics g) {
		Graphics2D g2 = (Graphics2D) g;
		setPaint(g2);
		/*
		 * create and fill a circle
		 */
		g2.fill(new Ellipse2D.Float(0,0,WIDTH,HEIGHT));
	}
}

