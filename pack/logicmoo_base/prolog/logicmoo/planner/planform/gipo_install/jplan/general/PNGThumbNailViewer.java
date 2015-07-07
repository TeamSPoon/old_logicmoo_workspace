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
 * 
 * 
 * Created on 11-Jan-2005
 *
 * Author Ron
 */
package jplan.general;

import java.io.File;
import java.awt.*;
import javax.swing.*;
import javax.swing.filechooser.*;
import javax.swing.plaf.metal.MetalIconFactory;

/**
 * @author ron
 * 
 * This class is used with Open/Save File Dialogs to present icon thumbnail
 * views of the contents of icon and graphics files
 */
public class PNGThumbNailViewer extends FileView {
	private Icon fileIcon = MetalIconFactory.getTreeLeafIcon();

	private Icon folderIcon = MetalIconFactory.getTreeFolderIcon();

	private Component observer;

	public PNGThumbNailViewer(Component c) {
		// We need a component around to create our icon’s image
		observer = c;
	}

	public String getDescription(File f) {
		// We won’t store individual descriptions, so just return the
		// type description.
		return getTypeDescription(f);
	}

	public Icon getIcon(File f) {
		// Is it a folder?
		if (f.isDirectory()) {
			return folderIcon;
		}

		// Ok, it’s a file, so return a custom icon if it’s an image file
		String name = f.getName().toLowerCase();
		if (name.endsWith(".jpg") || name.endsWith(".gif")
				|| name.endsWith(".png")) {
			return new Icon16(f.getAbsolutePath());
		}
		// Return the generic file icon if it’s not
		return fileIcon;
	}

	public String getName(File f) {
		String name = f.getName();
		return name.equals("") ? f.getPath() : name;
	}

	public String getTypeDescription(File f) {
		String name = f.getName().toLowerCase();
		if (f.isDirectory()) {
			return "Folder";
		}
		if (name.endsWith(".jpg")) {
			return "JPEG Image";
		}
		if (name.endsWith(".gif")) {
			return "GIF Image";
		}
		return "Generic File";
	}

	public Boolean isTraversable(File f) {
		// We’ll mark all directories as traversable
		return f.isDirectory() ? Boolean.TRUE : Boolean.FALSE;
	}

	public class Icon16 extends ImageIcon {
		public Icon16(String f) {
			super(f);
			Image i = observer.createImage(16, 16);
			i.getGraphics().drawImage(getImage(), 0, 0, 16, 16, observer);
			setImage(i);
		}

		public int getIconHeight() {
			return 16;
		}

		public int getIconWidth() {
			return 16;
		}

		public void paintIcon(Component c, Graphics g, int x, int y) {
			g.drawImage(getImage(), x, y, c);
		}
	}
}