import java.net.*;
import java.io.*;
import adc.parser.*;
import java.util.*;

public class WebCrawler implements Runnable {


   URL uin;
   int num; 
   int bytesread=0;

   static int numconnects = 0;
   static int numthreads = 0;
   static int urls = 0;
   static int numdups = 0;
   static int numwaiting = 0;
   static double bps = 0;
   static double waittime = 0;
   static double connecttime = 0;
   static Hashtable visited = new Hashtable(10);
   static Object indicator = new Object();

   WebCrawler(URL u) {
     uin = u;
     urls++;
     num = urls;
   }

  static public void main(String argv[]) {
     int intv = Integer.parseInt(argv[1]);
     new Thread(new Clock(intv)).start();
     try { new Thread(new WebCrawler(new URL(argv[0]))).start(); }
     catch (MalformedURLException e) { 
        System.out.println("Bad input URL"); 
     }
  }

  public void run() {
    try { 
      numthreads++;
      URL[] uout = go();
      for(int i = 0; i<uout.length; i++) {
        if (!(visited.get(uout[i]) == indicator)) {
          visited.put(uout[i], indicator); 
          new Thread(new WebCrawler(uout[i])).start();
        }
        else numdups++;
      }
    }
    finally {numthreads--;}
  }


   URL[] go() {
     Object contents = null;
     long time = System.currentTimeMillis();
     try { 
       numwaiting++;
       try {
          contents = uin.getContent();
       }
       catch (IOException e) { 
        //System.out.println("couldn't read this URL: " +
        //                   uin.toString()); 
       }
     }
     finally { numwaiting--; 
               waittime = (waittime*(double)((double)num/(double)(num + 1))) + 
			  ((double)(System.currentTimeMillis()-time)/(double)(num + 1)); 
     }
     time = System.currentTimeMillis();
     try { 
     numconnects++;
     if (!(contents instanceof InputStream)) return new URL[0];
     
     HtmlStreamTokenizer htmlstream = new HtmlStreamTokenizer((InputStream)contents);
      Vector v = new Vector();
      
      while (true) { 
	URL thing = getNextURL(htmlstream);
        if (thing==null) break;
        v.addElement(thing);
      }
      bytesread = htmlstream.numbytes;

      return vectorToArray(v);
      }
      finally { numconnects--; 
                long now = System.currentTimeMillis()-time;
                double b = (double)bytesread;
                double n = (double)now;
                double nsec = (n+1)/1000.0;
                double numa = (double)num;
                double fract = numa / (numa + 1);
                connecttime = (connecttime*fract) + (n/(numa+1)); 
                bps = (bps*fract) + ((b/nsec)/(numa+1));
      }
     
    }

    URL[] vectorToArray(Vector v) {
       URL[] output = new URL[v.size()];
       int i = 0;
       for(Enumeration e = v.elements(); e.hasMoreElements();) {
         output[i++] = (URL)e.nextElement();
       }
       return output;
    }
 
    URL getNextURL(HtmlStreamTokenizer h) {
       HtmlTag tag = new HtmlTag();
       try {
	 while (h.nextToken() != HtmlStreamTokenizer.TT_EOF) {
	   int ttype = h.getTokenType();
	   if (ttype == HtmlStreamTokenizer.TT_TAG) {
	      try {
		h.parseTag(h.getStringValue(), tag);
		if (tag.getTagType() == HtmlTag.T_A) {
                   String href = tag.getParam(HtmlTag.P_HREF);
		   if (href != null) {
		     URL output = null;
		     try { 
		       output = new URL(uin, href);
		     }
		     catch (MalformedURLException e) { System.out.println("new URL " + href + " didn't work"); }
		     return output;
		   }
		}
	      }
	      catch (HtmlException f) {
		System.out.println("html error");
	      }
	   }
	 }
	 return null;
       }
       catch (IOException g) {}
       return null; // dumbass compiler
    }
	    
}

class Clock implements Runnable {
 
   long currenttime = System.currentTimeMillis();
   long base = currenttime;
   long interval = 1000;

   Clock(long intv) {
      interval = intv;
   }

   public void run() {
     while (true) {
	long now = System.currentTimeMillis();
        if ((now - currenttime) > interval) {
          System.out.println("Time is " + (now-base) + 
           ". Avg Bytes/Second: " + WebCrawler.bps + ".");
          System.out.println("#Threads: " + WebCrawler.numthreads + 
	   ". #Active Connections: " + WebCrawler.numconnects +
           ". Avg Connect Time: " + (WebCrawler.connecttime/1000) +
           ". \n#Waiting to Connect: " + WebCrawler.numwaiting +
           ". Avg Wait to Connect Time: " + (WebCrawler.waittime/1000) +    
           ". \n#Duplicate URLs pruned: " + WebCrawler.numdups + ".");
          currenttime = now;
        }
        try { Thread.currentThread().sleep(1000); }
        catch(InterruptedException e) {}
     }
   }
}

