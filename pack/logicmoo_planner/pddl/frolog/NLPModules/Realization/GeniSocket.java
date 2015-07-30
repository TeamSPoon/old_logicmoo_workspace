package NLPModules.Realization;

import java.io.*;
import java.util.*;
import java.net.*;

/**
    Socket to GenI server.
    GenI is the surface realizer (generation) written by Eric Kow during his PhD (2004-2007).
    @author Alexandre Denis
*/
public class GeniSocket extends Socket
{
    private static final String resEncoding = "ISO-8859-1"; // encoding of the resources


//    public static void main(String[] args) throws Exception
//   	{
//		GeniSocket geniSocket = new GeniSocket();
//		System.out.println(geniSocket.generate("A:take(E) A:agent(E you) A:patient(E K) D:a(K) D:key(K)"));
//	} 
//
//    /**
//        Creates a new GeniSocket with given host and port.
//    */
//    public GeniSocket(String host, int port) throws UnknownHostException, IOException
//    {
//        super(host, port);
//    }


    /**
        Creates a new GeniSocket based on system properties.
        The host should be defined in <i>media.geni.host property</i>, and port in <i>media.geni.port</i> property.
    */
    public GeniSocket() throws UnknownHostException, IOException
    {
        super(System.getProperty("media.geni.host", "localhost"), Integer.getInteger("media.geni.port", 2035));
    }


    /**
        Generate a list of possible utterances for the given input semantics.
        @param sems a list of flat semantic feature
        @return the potential candidates for surface realization.
    */
    public List<String> generate(String sems)
    {
        if (!isConnected())
            System.out.println("Warning the GenI socket is not connected !");
        else System.out.println("The GenI socket is connected");

        String toGenI = "begin params\nend params\nbegin semantics\nsemantics:["+sems+"]\nend semantics\n";
        StringBuilder fromGenI = new StringBuilder();
        InputStream istr = null;
        OutputStream ostr = null;
        BufferedInputStream br = null;
        BufferedReader reader = null;

        System.out.println("Sending\n"+toGenI);
        try
        {
            ostr = getOutputStream();
            ostr.write(toGenI.getBytes(resEncoding));
            istr = getInputStream();
            br = new BufferedInputStream(istr);
            int c;
            while((c=br.read())!=-1)
                fromGenI.append((char)c);
            /*reader = new BufferedReader(new InputStreamReader(istr, resEncoding));
            int c;
            while((c=reader.read())!=-1)
                fromGenI.append((char)c);*/
        }
        catch(IOException e)
        {
            e.printStackTrace();
        }
        finally
        {
            try
            {
                ostr.close();
                istr.close();
                //reader.close();
                br.close();
            }
            catch(IOException ex)
            {
                ex.printStackTrace();
            }
        }
        
        // exclude the first and last element (begin responses and end responses tags)
        List<String> results = Arrays.asList(fromGenI.toString().split("\n"));
        return results.subList(1,results.size()-1);
    }
}
