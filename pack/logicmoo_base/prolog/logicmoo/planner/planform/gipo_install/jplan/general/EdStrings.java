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

package jplan.general;

/**
 * EdStrings - All static text strings
 * Should help internationialization
 * @author Ron Simpson
 * @version 0
 */

public final class EdStrings {
    // Message Dialogs Text
    public static final String strErrorHeading = "GIPO Error";
    public static final String strWarningHeading = "GIPO Warning";
    public static final String strInfoHeading = "GIPO Information";

    public static final String strNoDomain = 
	"No Domain currently being edited.";
    public static final String strTreeChanged = 
	"Tree Edited - Exit Anyway.";
    public static final String strSelectSort = 
	"Select Sort Node to add new element.";
    public static final String strDontMixSortsObjs = 
	"Cannot mix sorts and objects.";
    public static final String strSelectNode = 
	"Select a node to add to.";
    public static final String strSelectSortNode = 
	"Select a sort node to add new object.";
    public static final String strVerifyOK = 
	"All verification checks passed.";
    public static final String strDupEntries =
	"The Sort Tree contains duplicate entries\n" + 
	"Please remove duplicates for \n";
    public static final String strNoCommit =
	"There are unsaved changes. Commit Changes\n";

    // Dialog Box Titles
    public static final String strSortViewTitle =
	"Sort View (Expert)";

    // ToolTip Texts
    public static final String identifierToolTipText =
	new String("Requires legal OCL identifier. Start lower case");
}
