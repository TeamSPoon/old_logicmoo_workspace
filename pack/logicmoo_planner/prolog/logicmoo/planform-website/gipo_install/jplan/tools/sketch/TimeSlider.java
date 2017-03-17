/**
 * Slider which can adjust the current time in the Event and Process Canvases.
 * @author Graeme Elliott
 * 31/07/03
 */

package jplan.tools.sketch;

import javax.swing.*;

public class TimeSlider extends JSlider {
	
	/**
	 * Constants to represent the unit of the time the Slider is working in.
	 */
	public final static String SECONDS = "secs", MINUTES = "mins", HOURS = "hrs";
	/**
	 * Unit of time the Slider is working in.
	 */
	private String units;
	/**
	 * Event Canvas which the Slider can adjust the current time index of.
	 */
	private EventCanvas evtCanvas;
	/**
	 * Process Canvas which the Slider can adjust the current time index of.
	 */
	private ProcessCanvas procCanvas;
	/**
	 * Minimum and maximum values of the Slider.
	 */
	private int min,max;
	
	/**
	 * Creates an instance of TimeSlider associated with instances of EventCanvas and ProcessCanvas.
	 * @param evtCanvas the Event Canvas which the Slider can adjust the current time index of.
	 * @param procCanvas the Process Canvas which the Slider can adjust the current time index of.
	 */
	public TimeSlider(EventCanvas evtCanvas, ProcessCanvas procCanvas) {
		super();
		int min = 0;
		int max = 90;
		this.units = MINUTES;
		this.evtCanvas = evtCanvas;
		this.procCanvas = procCanvas;
		setSnapToTicks(true);
		setMinorTickSpacing((max-min)/60);
		setMajorTickSpacing((max-min)/6);
		setMinimum(min);
		setMaximum(max);
		setValue(min);
		setPaintLabels(true);
		setPaintTicks(true);
		evtCanvas.setIndexRange(min,max);
		evtCanvas.setCurrentIndex(min);
		procCanvas.setIndexRange(min,max);
		procCanvas.setCurrentIndex(min);
		addChangeListener(evtCanvas);
		addChangeListener(procCanvas);
	}
	
	/**
	 * Resets the slider to the minimum value.
	 */
	public void resetSlider() {
		setValue(min);
	}
	
	/**
	 * Returns the unit of time the Slider is working in.
	 * @return the unit of time the Slider is working in.
	 */
	public String getUnit() {
		return units;
	}
}
