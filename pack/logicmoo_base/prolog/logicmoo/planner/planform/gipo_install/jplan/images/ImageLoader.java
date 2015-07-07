package jplan.images;
/**
 * ImageLoader.java
 *
 *
 * Created: Tue Sep  4 10:49:14 2001
 * courtesy of John Zukowski
 * @author W Zhao
 * @version
 */

import java.awt.*;
import java.io.*;
import javax.swing.ImageIcon;

public final class ImageLoader  {
    
    private ImageLoader() {
	
    }

    public static ImageIcon getImageIcon(String fileDiretory, String imageName){
	String fileName = fileDiretory + imageName;
	Image image = ImageLoader.getImage(fileName, imageName);
	ImageIcon ii = null;
	if (image != null)
	    ii= new ImageIcon(image);

	return ii;
    }

    /*
     * getImage - get image from a jar file or a unjared file
     * @param Image
     * @param fileName - unjared absolute directory and name of this image
     * @param jarFilename - relative directory and name of this image in a jar file
     */
    public static Image getImage(String fileName, String jarFilename){
	Image returnValue = null;
	//for the jar situation
 	Class relativeClass = ImageLoader.class;
	InputStream is = relativeClass.getResourceAsStream(jarFilename);
	if (is != null) {
	    BufferedInputStream bis = new BufferedInputStream(is);
	    ByteArrayOutputStream baos = new ByteArrayOutputStream();
	    try {
		int ch;
		while ((ch = bis.read())!= -1){
		    baos.write(ch);
		}
		returnValue = Toolkit.getDefaultToolkit().createImage(baos.toByteArray());
	    }catch (IOException exception){
		System.err.println("Error loading: "+fileName);
	    }
	}

	//check for the unjar situation
	if (returnValue == null){
	    returnValue = Toolkit.getDefaultToolkit().createImage(fileName);
	}
	return returnValue;
    }
    
} // ImageLoader
