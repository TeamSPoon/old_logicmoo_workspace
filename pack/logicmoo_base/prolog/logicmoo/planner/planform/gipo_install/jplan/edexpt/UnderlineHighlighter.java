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

package jplan.edexpt;

/**
 * This is the highlighter example from Toply
 */

import javax.swing.text.*;
import java.awt.*;

public class UnderlineHighlighter extends DefaultHighlighter {
    public UnderlineHighlighter(Color c) {
	painter = (c == null ? sharedPainter :
		   new UnderlineHighlightPainter(c));
    }

    // Convenience method to add a highlight with
    // the default painter.
    public Object addHighlight(int p0, int p1) 
	throws BadLocationException {
	return addHighlight(p0, p1, painter);
    }

    public void setDrawsLayeredHighlights(boolean newValue) {
	// Illegal if false - we only support layered highlights
	if (newValue == false) {
	    throw new IllegalArgumentException("UnderlineHighlighter only draws layered highlights");
	}
	super.setDrawsLayeredHighlights(true);
    }

    // Painter for underlined highlights
    public static class UnderlineHighlightPainter extends
	LayeredHighlighter.LayerPainter {
	public UnderlineHighlightPainter(Color c) {
	    color = c;
	}

	public void paint(Graphics g, int offs0, int offs1, 
			  Shape bounds, JTextComponent c) {
	    // Do nothing: this method will never be called
	}

	public Shape paintLayer(Graphics g, int offs0, int offs1,
				Shape bounds, JTextComponent c, View view) {
	    g.setColor(color == null ? c.getSelectionColor() : color);

	    Rectangle alloc = null;
	    if (offs0 == view.getStartOffset() &&
		offs1 == view.getEndOffset()) {
		if (bounds instanceof Rectangle) {
		    alloc = (Rectangle)bounds;
		} else {
		    alloc = bounds.getBounds();
		}
	    } else {
		try {
		    Shape shape = view.modelToView( 
						   offs0, Position.Bias.Forward,
						   offs1, Position.Bias.Backward,
						   bounds);
		    alloc = (shape instanceof Rectangle) ?
			(Rectangle)shape : shape.getBounds();					
		} catch (BadLocationException e) {
		    return null;
		}
	    }

	    FontMetrics fm = c.getFontMetrics(c.getFont());
	    int baseline = alloc.y + alloc.height - fm.getDescent() + 1;
	    g.drawLine(alloc.x, baseline, alloc.x + alloc.width, 
		       baseline);
	    g.drawLine(alloc.x, baseline + 1, alloc.x + alloc.width, 
		       baseline + 1);
			
	    return alloc;
	}	    

	protected Color color;	// The color for the underline
    }

    // Shared painter used for default highlighting
    protected static final Highlighter.HighlightPainter sharedPainter 
	= new UnderlineHighlightPainter(null);

    // Painter used for this highlighter
    protected Highlighter.HighlightPainter painter;
}
