import java.util.*;
import java.io.*;
import javax.swing.*;
import javax.swing.event.*;

/**
*The driver class for Habplan
*The following things are done:
*Check the classpath to find the Habplan Home directory.
*Get any user specified properties.
*Create an instance of the main menu from FrameMenu.java
*@author Paul Van Deusen (NCASI)  12/97
*/
public class Habplan3 {
  static FrameMenu f;
  private static Properties props;
  private static String propFile="properties.hbp";
  private static String configFile="configuration.hbp";
  private static String text;  //used to get config properties 
  private static String fileSep=System.getProperty("file.separator");
    private static String frameTitle="NCASI Habplan 3.0  \u00A9 1999-2011";
  
public static void main (String args[]) {
    String cpath=System.getProperty("java.class.path"); 
    String path=null;
    //find where the Habplan files are
    
    int p1=0,p2=-1;
     while(p2<cpath.toLowerCase().lastIndexOf("habplan")){
      p1=p2+1;
      p2=cpath.indexOf(System.getProperty("path.separator"),p1);
       if (p2==-1)p2=cpath.toLowerCase().indexOf("habplan")+8;
       path=cpath.substring(p1,p2);
    }


   //if Habplan not in classpath, this should work 
   if (path==null)path=System.getProperty("user.dir");
   UserInput.HabplanDir=path + fileSep;
   UserInput.hbpDir=path + fileSep + "hbp" + fileSep;
   UserInput.projectDir=path + fileSep + "project" + fileSep;
   UserInput.SoundDir=path + fileSep + "sound" + fileSep;
   UserInput.HelpDir=path + fileSep + "help" + fileSep;
   UserInput.oldJobs=UserInput.hbpDir  + "oldJobs.hbp";
   UserInput.ExampleDir=path + fileSep +"example" + fileSep;
   UserInput.TempDir=path + fileSep +"temp" + fileSep;
   UserInput.LPDir=path + fileSep +"LP" + fileSep;

   //get project dir and project file from command line if present
   if(args.length==1){
       UserInput.projectFile=args[0];
       UserInput.isCommandLine=true;
   }
   if(args.length==2){
       UserInput.projectDir=args[0]+fileSep;
       UserInput.projectFile=args[1];
       UserInput.isCommandLine=true;
   }

     //load Habplan color properties 
   propFile=UserInput.hbpDir + propFile;
   props = new Properties(System.getProperties());
   try{
   props.load(new BufferedInputStream(new FileInputStream(propFile)));
   System.setProperties(props);
     }
   catch(Exception e){
    System.err.println("ERROR: Can't find the properties.hbp file in Habplan directory!");
    System.out.println("Diagnostic Information follows: \npropFile = " + propFile);
    System.out.println("\nClasspath: "+ System.getProperty("java.class.path") );
    System.exit(1);
    }
   //load Habplan configuration properties 
   configFile=UserInput.hbpDir + configFile;
   props = new Properties(System.getProperties());
   try{
   props.load(new BufferedInputStream(new FileInputStream(configFile)));
   System.setProperties(props);
     }
   catch(Exception e){
    System.err.println("ERROR: Can't find the configuration.hbp file in Habplan directory!");
    System.out.println("Diagnostic Information follows: \nconfigFile = " + configFile);
    System.out.println("\nClasspath: "+ System.getProperty("java.class.path") );
    System.exit(1);
    }
   configurator();  //implement user supplied configuration
    f= new FrameMenu(frameTitle);
    f.setBounds(100,400,200,200);
    f.validate();
    f.pack();
    f.setVisible(true);
    }/*end main*/

/**
*reads the oldjobs number from properties.hbp to
*set the number of oldjobs appearing in File menu
*<P>
*reads the configuration string from properties.hbp
*the string contains a number for nFlows followed
*by a string of nCofF's, then a string of nBKofF's
*then by a number for nBioI and a number for nBioII
*then sMods etc...
*/

protected static void configurator(){
 int maxOldJobs=5;
 try{
  maxOldJobs=Integer.parseInt(System.getProperty("oldjobs"));
 }
 catch(Exception e){ /*do nothing*/ }
 finally{UserInput.maxOldJobs=maxOldJobs;}
 
  //do the FrameMenu panel configuration
  int nFlow=1, nBioI=1, nBioII=1, nSMod=1;
  int nComponents=6; //total number of components in the configuration
  int [] nCofF,nBKofF;
  nCofF=new int[2];nCofF[1]=1;
  nBKofF=new int[2];nBKofF[1]=1;
  text=System.getProperty("habplan.config");
   //check for different separators and change to blanks
  if (text.indexOf(";")>0 ||text.indexOf(",")>0){
	 text=text.replace(';',' '); text=text.replace(',',' ');
	 }
  	
   try{	
	for (int i=1;i<=4;i++){  
	  switch (i){
	   case 1: nFlow=Integer.parseInt(getSub(text));
		   nComponents=nFlow;
		     //System.out.println("nFlow=" + nFlow);
	     nCofF=new int[nFlow+1];
	    for (int j=1;j<=nFlow;j++){
	    nCofF[j]=Integer.parseInt(getSub(text));
	    nComponents+=nCofF[j];
		      //System.out.println(" nCofF= " + nCofF[j]);
	    }
	     nBKofF=new int[nFlow+1];
	    for (int j=1;j<=nFlow;j++){
	    nBKofF[j]=Integer.parseInt(getSub(text));
	    nComponents+=nBKofF[j];	 
		      //System.out.println(" nBKofF= " + nBKofF[j]);     
	    }
	       break; 
	   case 2: nBioI=Integer.parseInt(getSub(text));
		   nComponents+=nBioI;
		      //System.out.println("nBioI = " + nBioI);
		   break;   
	   case 3: nBioII=Integer.parseInt(getSub(text));
		   nComponents+=nBioII;
		      //System.out.println("nBioII=" + nBioII);
		   break; 
	   case 4: nSMod=Integer.parseInt(getSub(text));
		   nComponents+=nSMod;
		      //System.out.println("nSmod=" + nSmod);
		   break; 	       		   
	   default: //do nothing as default;
	 }/*end switch*/ 
	}/*end for i*/
	      //System.out.println("nComponents=" + nComponents);
	   UserInput.nComponents=nComponents;
	   UserInput.nFlow=nFlow;
	   UserInput.nCofF=nCofF;
	   UserInput.nBKofF=nBKofF;
	   UserInput.nBioI=nBioI;
	   UserInput.nBioII=nBioII;
	   UserInput.nSMod=nSMod;
	  
	}/*end try*/
    catch(Exception e){
      System.out.println("Check your configuration specification in properties.hbp file.");
    }	
  	
}/*end configurator()*/

/**
*get the next section of a string of things separated by blanks
*and chop the returned section out of static text string
*this method is pretty specific to this job
*/
protected static String getSub(String s){
	String sub; //use to pull out numbers from the string
	  text=s;
	  int finish=0;
	  text=text.trim();
	  finish = text.indexOf(" "); //index of next  separator
	  finish=finish>0?finish:text.length();
	  sub=text.substring(0,finish);
	  text=text.substring(finish,text.length()).trim(); //throw out used part;
	  
	return sub;
}/*end getSub*/	  
	  



} /*end Habplan2 class*/
