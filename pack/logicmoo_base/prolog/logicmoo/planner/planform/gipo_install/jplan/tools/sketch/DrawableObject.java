/**
 * An Interface which, when implemented, models Actions, Events and Processes as having a Title,
 * Start Time, End Time and DomainIcon associated with them.
 * @author Graeme Elliott
 * 31/07/03
 */

package jplan.tools.sketch;

import javax.swing.*;

public interface DrawableObject {
	
	/**
	 * Returns the name of the DrawableObject.
	 * @return the name of the DrawableObject.
	 */
	public String getTitle();
	
	/**
	 * Returns the JComponent associated with the DrawableObject.
	 * @return the JComponent associated with the DrawableObject.
	 */
	public JComponent getIcon();	
	
	/**
	 * Sets the JComponent associated with the DrawableObject to the supplied JComponent.
	 * @param newIcon the JComponent to be associated with the DrawableObject.
	 */
	public void setIcon(JComponent newIcon);
	
	/**
	 * Sets the start time of the Process or the time an Action or Event occurred.
	 * @param time the start time of the Process or the time an Action or Event occurred.
	 */
	public void setStartTime(int time);
	
	/**
	 * Returns the start time of the Process or the time an Action or Event occurred.
	 * @return the start time of the Process or the time an Action or Event occurred.
	 */
	public int getStartTime();
	
	/**
	 * Sets the end time of the Process.
	 * This can be ignored if the DrawableObject represents an Action or Event.
	 * @param time the end time of the Process.
	 */
	public void setEndTime(int time);
	
	/**
	 * Gets the end time of the Process.
	 * This can be ignored if the DrawableObject represents an Action or Event.
	 */
	public int getEndTime();
}
