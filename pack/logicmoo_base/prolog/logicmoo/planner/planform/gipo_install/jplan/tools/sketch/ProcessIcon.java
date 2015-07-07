/**
 * DomainIcon representing a Process
 * @author Graeme Elliott
 * 31/07/03
 */

package jplan.tools.sketch;


import java.awt.*;
import java.awt.geom.*;

public class ProcessIcon extends DomainIcon {
	
	/**
	 * Creates an instance of ProcessIcon.
	 * @param drawableObject the DrawableObject associated with the icon.
	 */
	public ProcessIcon(DrawableObject drawableObj, int colourIndex) {
		super(drawableObj, colourIndex);
	}
	
	/**
	 * Overrides paintComponent in class JComponent.
	 * Calls drawIt() to peform custom painting.
	 * @param g ProcessIcon graphics.
	 */
	public synchronized void paintComponent(Graphics g) {
		super.paintComponent(g);
		drawIt(g);
	}
		
	/**
	 * Draws a rectangle with GradientPaint.
	 * The colour of the GradientPaint starts at darkGreen at the bottom-left
	 * of the icon and transitions to lightGreen at the top-right.
	 * @param g ActionIcon graphics.
	 */
	private void drawIt(Graphics g) {
		Graphics2D g2 = (Graphics2D) g;
		setPaint(g2);
		/*
		 * create a shape the size of the component and fill
		 */
		Shape rectangle = new Rectangle2D.Double(0.0,0.0,(double)getWidth(),(double)getHeight());
		g2.fill(rectangle);
	}
}

