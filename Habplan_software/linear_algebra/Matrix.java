
package linear_algebra;

import java.util.*;
import java.lang.*;
import java.io.*;

/**
*This class includes a number of methods useful for matrices
*e.g. covar, AxB, etc.
*</ol>
*
*@author Paul Van Deusen
*@version 0  10/97
* results from matlab to check the tests:
>> x=[1 3 4 8
      2 3 5 7
      3 4 5 9
      2 2 6 8
      1 3 3 7]

>> c=cov(x)
 
c =

    0.7000    0.2500    0.6500    0.4500
    0.2500    0.5000   -0.2500    0.2500
    0.6500   -0.2500    1.3000    0.4000
    0.4500    0.2500    0.4000    0.7000

det(c)
 
ans =

    0.0125

>> inv(c)
 
ans =

   16.0000  -14.0000  -11.0000    1.0000
  -14.0000   16.0000   11.0000   -3.0000
  -11.0000   11.0000    9.0000   -2.0000
    1.0000   -3.0000   -2.0000    3.0000

>> s=svd(cov(x))
 
s =

    1.9830
    0.9293
    0.2618
    0.0259


*/


public class Matrix {


   public static void main (String args[]) {

     System.out.println("Testing the Matrix class");

    double [][] x = {		{0, 0,0,0,0},
				{0, 1,3,4,8},
				{0, 2,3,5,7},
				{0, 3,4,5,9},
				{0, 2,2,6,8},
				{0, 1,3,3,7}, };
     int np=x[1].length;
     
    double cov[][] = new double [np][np];
    double mx[] = new double [np];
  mean(x,mx);
 covar(x,cov);
 double det=det(cov);
 double cond=cond(cov);
 inv(cov,cov);

    System.out.println("The determinant, condition number are " + det + " " +cond);
    System.out.println("The inverse covariance matrix");
   print(cov);  

 System.out.println("\n Heres the mean values:");
 for (int j=1; j<np; j++)    			
      System.out.print(mx[j] + " ");




 System.out.println(" \nEnd of test for Matrix class");

   } /*end main*/



/**
* Print out a double matrix
*/
  public static void print(double x[][]){
    int nr=x.length;
    int nc=x[nr-1].length;

  for (int i=1; i<nr; i++) {
      System.out.println(" ");
    for (int j=1; j<nc; j++)    			
      System.out.print(x[i][j] + " ");
   } /*end for i*/
    return;
} /*end print method*/


/**
* Print out a float matrix
*/
  public static void print(float x[][]){
    int nr=x.length;
    int nc=x[nr-1].length;

  for (int i=1; i<nr; i++) {
      System.out.println(" ");
    for (int j=1; j<nc; j++)    			
      System.out.print(x[i][j] + " ");
   } /*end for i*/
    return;
} /*end print method*/

/**
* Print out a int matrix
*/
  public static void print(int x[][]){
    int nr=x.length;
    int nc=x[nr-1].length;

  for (int i=1; i<nr; i++) {
      System.out.println(" ");
    for (int j=1; j<nc; j++)    			
      System.out.print(x[i][j] + " ");
   } /*end for i*/
    return;
} /*end print method*/


/**
*compute the covariance matrix of x which is rx by cx and return in cov
*@param x the input nxp matrix
*/
  public static void covar(double x[][], double cov[][]){

    int rx=x.length;
    int cx=x[1].length; //every row must be the same length
    double x2[][]=new double[rx][cx]; //temporary matrix
    double mean[]=new double[cx];
             /*compute column means*/
  for (int i=1;i<rx;i++){
   for (int j=1;j<cx;j++)
    mean[j]=mean[j]+x[i][j]/(rx-1);
  } /*end for i*/
             /*center the x-matrix*/
 for (int i=1;i<rx;i++){
   for (int j=1;j<cx;j++)
    x2[i][j]=x[i][j]-mean[j];
  } /*end for i*/
            /*compute covariance*/
   AtxB(x2, x2, cov);
  for (int i=1;i<cx;i++){
   for (int j=1;j<cx;j++)
    cov[i][j]=cov[i][j]/(rx-2);
  } /*end for i*/
    return;
} /*end covar method*/

/**
*compute the inverse(a) = v*diag(1/w)*u' where u, w, v came
*from SVDC_f77. The result is returned as double
matrix u. a is square.
*/

public static void inv(double a[][], double u[][])
{
int n=a.length;
double w[]=new double[n];
double e[]=new double[n];
double v[][]=new double[n][n];
double work[]=new double[n];

double a2[][]=new double[n][n]; /*DSVDC_f77 destroys the matrix it decomposes*/
 for(int k=1;k<n;k++){ 
     for(int j=1;j<n;j++)
       a2[j][k]=a[j][k];
    }/*end for k*/
   
try{
   SVDC_f77.dsvdc_f77(a2,n-1,n-1,w,e,u,v,work,12);
  }
catch (Exception exc){
  System.out.println("SVD error: " + exc);
}
 
  double denom,minw=10000000, maxw=0;
  double c2[][]=new double[n][n];
    for (int j=1;j<n;j++){  /*compute the condition number*/
      maxw=maxw>w[j]?maxw:w[j];
      minw=minw<w[j]?minw:w[j];
  } /*end for j*/
    maxw=maxw/minw;

    if (maxw>.8*Double.MAX_VALUE) 
      System.out.print("\nMatrix is ill conditioned\n");
 for(int k=1;k<n;k++){  /*compute the inverse*/
  for(int j=1;j<n;j++){
   for (int i=1;i<n;i++){
      if (w[i]<.000001) denom=0; else  denom=1/w[i];
     c2[j][k]= c2[j][k]+v[j][i]*u[k][i]*denom;
   }/*end for i*/    
  } /*end for j*/
 }/*end for k*/

    for(int k=1;k<n;k++){ /*put inverse in to u*/
     for(int j=1;j<n;j++)
       u[j][k]=c2[j][k];
    }/*end for k*/
  return;
} /*end inv*/

/**
*Return the double condition number of square matrix a
*/

public static double cond(double a[][])
{
int n=a.length;
double w[]=new double[n];
double e[]=new double[n];
double u[][]=new double[n][n];
double v[][]=new double[n][n];
double work[]=new double[n];

double a2[][]=new double[n][n]; /*DSVDC_f77 destroys the matrix it decomposes*/
 for(int k=1;k<n;k++){ 
     for(int j=1;j<n;j++)
       a2[j][k]=a[j][k];
    }/*end for k*/
   
try{
   SVDC_f77.dsvdc_f77(a2,n-1,n-1,w,e,u,v,work,0);
  }
catch (Exception exc){
  System.out.println("SVD error: " + exc);
}
 
  double denom,minw=10000000, maxw=0;
  double c2[][]=new double[n][n];
    for (int j=1;j<n;j++){  /*compute the condition number*/
      maxw=maxw>w[j]?maxw:w[j];
      minw=minw<w[j]?minw:w[j];
  } /*end for j*/
    maxw=maxw/minw;

  return(maxw);
} /*end cond*/


/**
*compute the determinant of a square matrix a
*and return the result as a double scalar.
*/

public static double det(double a[][])
{
int n=a.length;
double w[]=new double[n];
double e[]=new double[n];
double u[][]=new double[n][n];
double v[][]=new double[n][n];
double work[]=new double[n];
   
double a2[][]=new double[n][n]; /*DSVDC_f77 destroys the matrix it decomposes*/
 for(int k=1;k<n;k++){ 
     for(int j=1;j<n;j++)
       a2[j][k]=a[j][k];
    }/*end for k*/

try{
   SVDC_f77.dsvdc_f77(a2,n-1,n-1,w,e,u,v,work,0);
  }
catch (Exception exc){
  System.out.println("SVD error: " + exc);
}

double det=1.0; for(int j=1;j<n;j++) det*=w[j];
  return(det);
} /*end det*/


 /**
*multiply a transpose times b where a is ra by ca and b is ra by cb,
*the result is returned in c which is ca by cb,
*/

public static void AtxB(double a[][], double b[][], double c[][])
{

int ra = a.length;
int ca = a[1].length;
int cb = b[1].length;

   double c2[][]=new double[ra][cb];
  /*temporary holder for c in case user wants to use  a or b later*/
        
    for(int k=1;k<cb;k++){
     for(int j=1;j<ca;j++){
      for (int i=1; i<ra; i++)
       c2[j][k]=c2[j][k]+a[i][j]*b[i][k];
      } /*end for j*/
     }/*end for k*/
    for(int k=1;k<cb;k++){
     for(int j=1;j<ca;j++)
       c[j][k]=c2[j][k];
     }/*end for k*/
    return;
} /*end AtxB*/

void AxB(double a[][], double b[][], double c[][])
 /*multiply a times b where a is ra by ca and b is ca by cb,
    the result is returned in c which is ra by cb*/
{
   int ra=a.length;
   int ca=a[1].length;
   int cb=b[1].length;
   double c2[][]=new double[ra][cb]; //a holder so a isn't destroyed
      for(int k=1;k<cb;k++){
     for(int j=1;j<ra;j++){
      for (int i=1; i<ca; i++)
       c2[j][k]=c2[j][k]+a[j][i]*b[i][k];
      } /*end for j*/
     }/*end for k*/
    for(int k=1;k<cb;k++){
     for(int j=1;j<ra;j++)
       c[j][k]=c2[j][k];
     }/*end for k*/
 return;
}/*end AxB*/


/**
*multiply a vector v transposed times square a times v where
*A is n by n, and return the scaler result as a double
*/

public static double VtxAxV(double v[], double a[][])
{
  int n=a.length;   
    double ans=0; /*holds the answer*/
    /* printf("in VtxAxV routine\n");*/    
     for(int j=1;j<n;j++){
      for (int i=1; i<n; i++)
        ans=ans+v[j]*v[i]*a[i][j];
      } /*end for j*/
   return(ans);
} /*end VtxAxV*/


/**
*Compute the mean of the columns of a and return in mean
*/

public static void mean(double a[][], double mean [])
{

int ra = a.length;
int ca = a[1].length;
 for (int i=1;i<ra;i++){
   for (int j=1;j<ca;j++)
    mean[j]=mean[j]+a[i][j]/(ra-1);
  } /*end for i*/

    return;
} /*end of mean for a double*/

/**
*Compute the mean of the columns of a and return in mean
*/

public static void mean(float a[][], float mean [])
{

int ra = a.length;
int ca = a[1].length;
 for (int i=1;i<ra;i++){
   for (int j=1;j<ca;j++)
    mean[j]=mean[j]+a[i][j]/(ra-1);
  } /*end for i*/

    return;
} /*end of mean for a float*/




} /*end Matrix class*/

