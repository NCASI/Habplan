import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import java.io.*;
import java.util.*;
/**
*Gives the version information
*/

public class Version extends JDialog{

JPanel panel;
JLabel lab1,lab2,lab3;
JButton cancel;
    String version="Build Date: October 18, 2012, NCASI \u00A9 1999-2013 ";

public Version(FrameMenu parent){
 super(parent,"Habplan 3 Version Information");
 setLocation(200,200);
 panel=new JPanel();
 Font font = new Font("Times", Font.BOLD, 16);
 lab1=new JLabel(version);
 lab1.setBackground(Color.white);
 lab1.setFont(font);
    cancel = new JButton("Close");
    cancel.addActionListener(new ButtonListener());
    cancel.setActionCommand("cancel");
    cancel.setFont(font);
    
 panel.add(lab1);
 panel.add(cancel); 
 panel.setBackground(Color.black);
 lab1.setForeground(Color.white); 
 getContentPane().add(panel);
 
 pack();

      addWindowListener(new WindowAdapter(){
       public void windowClosing (WindowEvent event)
    {
     dispose();
      
    }
      });

}/*end constructor*/

/**
* Handle button clicks
**/
   
public class ButtonListener implements ActionListener{

  public void actionPerformed(ActionEvent e) {
    String cmd = e.getActionCommand();
     
    if (cmd.equals("cancel")) {
      dispose();
    }  
   
  }/*end actionperformed()*/

} /*end inner Listener class*/
}
    
    
    
