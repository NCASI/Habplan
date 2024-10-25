/**
*Draw something along the axis
*This class is extended to get x or y tics or grids
*override drawThis(), xCoordinates(), and yCoordinates
*reset isScaleFree, isXaxis, and change tics
*/
package figure;
import java.awt.*;
import java.awt.event.*;
import java.awt.geom.*;
import javax.swing.*;

abstract class DrawAlongAxis extends GraphComponent{
 AGraph ag;
 float tics=3; //interpretation depends on isScaleFree, has set method
 boolean isScaleFree=true; //use scale free ticMark spacing if true, has set method
 boolean isXAxis=false; //are we drawing along xaxis, has set method
 float Min=Float.MAX_VALUE,Max=Float.MIN_VALUE;
 Shape shape;
 
 public DrawAlongAxis(AGraph ag){
    super();        
    this.ag=ag;
 }/*end constructor*/
 
 /**
 *Draw the ticmarks
 */
 public void draw(Graphics2D g2){ 
  if (!getDrawState())return;
  reScale(); 
  g2.setStroke(stroke);
  g2.setPaint(color);
  double skip;
  if (isScaleFree) skip=(double)((Max-Min)/tics);
  else skip=tics;
  for (double i=Min; i<Max; i+=skip){
      if(isXAxis & (i>Min & Math.floor(i/5)==(i-1)/5))i=i-1; //force tics at 5 or 10 years, not 6 or 11
   shape=drawThis(xCoordinate((float)i),yCoordinate((float)i));
   g2.draw(shape);
  }/*end for i*/
 }/*end draw()*/
 
/**
*What to draw. Set up the shape to draw, e.g. a tic mark
*at the supplied coordinates
*/
abstract Shape drawThis(double x,double y);


/**
*x coordinates.
@param i only used when x coordinates vary
*/
double xCoordinate(float i){
    return ag.gx;
}/*end xCoordinate()*/



/**
*y coordinates
*@param i only used when y coordinates vary
*/
double yCoordinate(float i){
    return ag.gy+ag.gheight-(double)((i-Min)/(Max-Min))*ag.gheight; 
}/*end yCoordinate()*/


/**
*Used to get x and y mins and maxes from graph
*and decide where the tics go with current scale
*/
public void reScale(){
    float[] bounds = ag.getLineBounds(); 
  if (isXAxis){
   Min=bounds[0];
   Max=bounds[1];
  }else{ 
   Min=bounds[2];
   Max=bounds[3];
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
}/*end DrawAlongAxis class*/
