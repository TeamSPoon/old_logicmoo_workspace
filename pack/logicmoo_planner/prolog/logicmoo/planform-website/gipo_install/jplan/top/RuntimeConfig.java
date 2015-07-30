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

package jplan.top;

/**
 * RuntimeConfig
 * This class stores the parameters to the currently selected planning
 * algorithm. The values are set from the PlannerDia Dialogue
 */

public class RuntimeConfig {
    // Constants added 15/9/01
    public static final int INTERNAL = 0;
    public static final int SHELL = 1;

    /**
     * The Planning algorithm name - must match prolog file name
     */
    public String algName;
    /**
     * Indicates whether or not the algorithm deals with conditional effects.
     */
    public boolean condEffects;
    /**
     * Indicates whether or not the algorithm deals with hierarchical methods.
     */
    public boolean hierMethods;
    /**
     * Has the user selected to generate a statistics file.
     */
    public boolean storeStats;
    /**
     * The path and name of the statistics file.
     */
    public String strStatsFile;
    /**
     * The maximum time in miliseconds to run the algorithm.
     */
    public long maxTime;
    /**
     * Has the configuration changed
     */
    public boolean dirty;
    //Ron Added 15/9/01
    /**
     * callMethod
     */
    public int callMethod;
    //Ron Added 11/10/01
    /**
     * command - this is the command line for an external planner
     */
    public String command;
    /**
     * hierSorts
     */
    public boolean hierSorts;

    /**
     * Constructor initialises all values to empty
     */
    public RuntimeConfig () {
	algName = new String("none");
	condEffects = false;
	hierMethods = false;
	storeStats = false;
	strStatsFile = new String("");
	maxTime = 0L;
	dirty = true;
	callMethod = INTERNAL;
	command = new String("");
	hierSorts = false;
    }
}
