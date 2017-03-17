/**
 * DomainIcon representing an Object
 * @author Graeme Elliott
 * 31/07/03
 */

package jplan.tools.sketch;

import javax.swing.*;

public class ObjectIcon extends JTextArea {
	
	/**
	 * DrawableObject the icon is associated with.
	 */
	private DrawableObject drawableObject;
	
	/**
	 * Creates an instance of ObjectIcon.
	 * @param drawableObject the DrawableObject associated with the icon.
	 */
	public ObjectIcon(DrawableObject drawableObject) {
		super(drawableObject.getTitle());
		this.drawableObject = drawableObject;
		setEditable(false);
	}
}


