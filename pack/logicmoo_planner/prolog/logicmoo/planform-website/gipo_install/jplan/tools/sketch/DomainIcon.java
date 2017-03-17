/**
 * An Icon associated with an object which implements DrawableObject.
 * DomainIcons can be one of three colours.
 * @author Graeme Elliott
 * 31/07/03
 */

package jplan.tools.sketch;

import java.awt.*;
import javax.swing.*;
import java.awt.geom.*;

public class DomainIcon extends JComponent {
	
	/**
	 * Width and height of the icon.
	 */
	public static final int WIDTH = 20, HEIGHT = 20;
	/**
	 * DrawableObject the icon is associated with.
	 */
	private DrawableObject drawableObj;
	/**
	 * Constants for the various colours a DomainIcon can be.
	 * There are two shades to each colour for use with GradientPaint.
	 */
	private final static Color darkRed = new Color(170,0,0);
	private final static Color lightRed = new Color(255,34,34);
	private final static Color darkBlue = new Color(0,64,102);
	private final static Color lightBlue = new Color(117,203,255);
	private final static Color darkGreen = new Color(32,156,1);
	private final static Color lightGreen = new Color(107,254,71);
	/**
	 * Indicates the base colour the Icon should be painted.
	 */
	private int colourIndex;
	/**
	 * Constants to represent the Base Colours a DomainIcon can be.
	 */
	public static final int RED = 0;
	public static final int BLUE = 1;
	public static final int GREEN = 2;
	public static final int COLOURCOUNT = 3;
	
	/**
	 * Creates an instance of DomainIcon associated with the supplied DrawableObject.
	 * @param drawableObject The DrawableObject associated with the icon.
	 */
	public DomainIcon(DrawableObject drawableObj, int colourIndex) {
		setOpaque(true);
		setBackground(Color.white);
		setPreferredSize(new Dimension(WIDTH, HEIGHT));
		this.drawableObj = drawableObj;
		this.colourIndex = colourIndex;
	}
	
	/**
	 * Returns the DrawableObject associated with this icon.
	 * @return The DrawableObject associated with this icon.
	 */
	public DrawableObject getDrawableObj() {
		return drawableObj;
	}
	
	/**
	 * Sets the Paint to be a Gradient paint comprising the lighter and darker shades of the 
	 * base colour of the DomainIcon.
	 * The darker shade is at the bottom right of the DomainIcon, the lighter shade at the top right.
	 * @param g2 the Graphics2D context.
	 */
	public Graphics2D setPaint(Graphics2D g2) {
		Color darkColour = null, lightColour = null;
		if (colourIndex == RED) {
			darkColour = darkRed;
			lightColour = lightRed;
		} else if (colourIndex == BLUE) {
			darkColour = darkBlue;
			lightColour = lightBlue;
		} else if (colourIndex == GREEN) {
			darkColour = darkGreen;
			lightColour = lightGreen;
		}
		/*
		 * get coordinates of the bottom left and top right of the component
		 * setup the gradient paint
		 */
		Point2D.Double bottomLeft = new Point2D.Double(0,getHeight());
		Point2D.Double topRight = new Point2D.Double(getWidth(),0);
		g2.setPaint(new GradientPaint(bottomLeft,darkColour,topRight,lightColour));
		return g2;
	}
}