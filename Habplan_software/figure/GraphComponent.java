/**
*This is the parent class for 2D graphical components
*Actual graphical components will extend this class
*Every component defines its shape, color and stroke
*/
package figure;
import java.awt.*;
import java.awt.event.*;
import java.awt.geom.*;
import javax.swing.*;

public class GraphComponent{
    final static BasicStroke thin = new BasicStroke(2.0f);
    final static BasicStroke wide = new BasicStroke(4.0f);
    final static BasicStroke dashed = new BasicStroke(2.0f, BasicStroke.CAP_ROUND, 
	    BasicStroke.JOIN_ROUND,10.0f, new float[] {5.0f}, 0.0f);   
    final static BasicStroke dotDash = new BasicStroke(2.0f, BasicStroke.CAP_ROUND, 
	    BasicStroke.JOIN_ROUND,10.0f, new float[] {4,4,10,4}, 0.0f);   	     
    final static BasicStroke dotted= new BasicStroke(1.f,BasicStroke.CAP_ROUND,
	  BasicStroke.JOIN_ROUND,8.f,new float[]{1.0f,3.0f},0.0f);	
	  //create a vector for picking line strokes automatically by line number
    final static BasicStroke[] lineStroke={thin,dashed,dotDash,dotDash,dotted,wide};	  
	  					      
    protected Color color;
    BasicStroke stroke;			      
    protected boolean drawState=true; //only draw this if its true
    
 public GraphComponent(){ 
    this.color=Color.white;
    this.stroke=thin;        
 }/*end constructor*/
 
 /**
 *draw the component.
 */
 public void draw(Graphics2D g2){    
 }/*end draw*/
 
 /**
 *Rescale the component to fit the graph
 */
 public void reScale(){}  
 
 /**
*Set the color of this component
*/  
public void setColor (Color color){
    this.color=color;
}/*end setColor()*/

 /**
*Get the color of this component
*/  
public Color getColor (){
    return color;
}/*end getColor()*/

 /**
*Set the stroke of this component
*/  
public void setStroke (BasicStroke stroke){    
    this.stroke=stroke;
}/*end setStroke*/

 /**
*Set the drawState of this component
*/  
public void setDrawState(boolean drawState){
    this.drawState=drawState;
}/*end set()*/

 /**
*Get the drawState of this component
*/  
public boolean getDrawState(){
    return drawState;
}/*end get()*/
 					       						       
}/*end GraphComponent class*/
