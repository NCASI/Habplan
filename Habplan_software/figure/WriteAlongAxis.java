/**
*Write something along the axis
*This class is extended to write numbers along the x or y axis
*override drawThis(), xCoordinates(), and yCoordinates
*reset isScaleFree, isXaxis, and change tics
*/
package figure;
import java.awt.*;
import java.awt.event.*;
import java.awt.geom.*;
import javax.swing.*;
import java.awt.font.*;

abstract class WriteAlongAxis extends GraphComponent{
 AGraph ag;
 float tics=10; //interpretation depends on isScaleFree, has set method
 boolean isScaleFree=false; //use scale free ticMark spacing if true, has set method
 boolean isXAxis=true; //are we drawing along xaxis, has set method
 float Min,Max; 
 String string;
 
 public WriteAlongAxis(AGraph ag){
    super();        
    this.ag=ag;
 }/*end constructor*/
 
 /**
 *Draw the text
 */
 public void draw(Graphics2D g2){
  if (!getDrawState())return;    
  reScale(); 
  g2.setPaint(color);
  
  double skip;
  if (isScaleFree) skip=(double)((Max-Min)/tics);
  else skip=tics;
  
  for (double i=Min; i<=Max+.01; i+=skip){
      if(isXAxis & (i>Min & Math.floor(i/5)==(i-1)/5))i=i-1; //force tics at 5 or 10 years,
   if (Max>5.f)string=String.valueOf((int)i); //don't show decimals for big numbers.  Looks sloppy.   
   else string=String.valueOf(Math.round(i*1000.)/1000);
   g2.drawString(string,(int)xCoordinate((float)i),(int)yCoordinate((float)i));
  }/*end for i*/
 }/*end draw()*/
 

/**
*x coordinates
*/
abstract float xCoordinate(float i);

/**
*y coordinates
*/
abstract float yCoordinate(float i);


/**
*Used to get x and y mins and maxes from graph
*and decide where the tics go with current scale
*/
public void reScale(){
    float[] bounds=ag.getLineBounds(); 
  if (isXAxis){
   Min=bounds[0];
   Max=bounds[1];
  }else{ 
  float pow10=(float)Math.pow(10,ag.getYExponent());
  Min=bounds[2]/pow10;
  Max=bounds[3]/pow10;
  }/*end if*/
}/*end of reScale*/

/**
*set isScaleFree to t or f
*/
public void setIsScaleFree(boolean value){isScaleFree=value;}

/**
*set isXAxis to t or f
*/
public void setIsXAxis(boolean value){isXAxis=value;}

/**
*set tics to a value
*if isScaleFree=true then tics is the n of tics
*otherwise tics is the amount of space between tics in user coordinates
*/
public void setTics(float value){tics=value;}
}/*end  class*/
