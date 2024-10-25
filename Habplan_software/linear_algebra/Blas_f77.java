Blas_j.java                                                                                         0100644 0007456 0000012 00000073620 06347630337 0013464 0                                                                                                    ustar 00steve                           staff                           0000040 0000007                                                                                                                                                                        /*
    Blas_j.java copyright claim:

    This software is based on public domain LINPACK routines.
    It was translated from FORTRAN to Java by a US government employee 
    on official time.  Thus this software is also in the public domain.


    The translator's mail address is:

    Steve Verrill 
    USDA Forest Products Laboratory
    1 Gifford Pinchot Drive
    Madison, Wisconsin
    53705


    The translator's e-mail address is:

    steve@ws10.fpl.fs.fed.us


***********************************************************************

DISCLAIMER OF WARRANTIES:

THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND. 
THE TRANSLATOR DOES NOT WARRANT, GUARANTEE OR MAKE ANY REPRESENTATIONS 
REGARDING THE SOFTWARE OR DOCUMENTATION IN TERMS OF THEIR CORRECTNESS, 
RELIABILITY, CURRENTNESS, OR OTHERWISE. THE ENTIRE RISK AS TO 
THE RESULTS AND PERFORMANCE OF THE SOFTWARE IS ASSUMED BY YOU. 
IN NO CASE WILL ANY PARTY INVOLVED WITH THE CREATION OR DISTRIBUTION 
OF THE SOFTWARE BE LIABLE FOR ANY DAMAGE THAT MAY RESULT FROM THE USE 
OF THIS SOFTWARE.

Sorry about that.

***********************************************************************


History:

Date        Translator        Changes

2/25/97     Steve Verrill     Translated
6/3/97                        Java/C style indexing

*/


package linear_algebra;

import linear_algebra.*;


/**
*
*<p>
*This class contains Java versions of a number of the LINPACK 
*basic linear algebra subroutines (blas):
*<ol>
*<li> daxpy_j
*<li> ddot_j
*<li> dscal_j
*<li> dswap_j
*<li> dnrm2_j
*<li> dcopy_j
*<li> drotg_j
*</ol>
*It also contains utility routines that the translator found useful
*while translating the FORTRAN code to Java code.  "col" indicates that
*the routine operates on two columns of a matrix.  "colv" indicates that
*the routine operates on a column of a matrix and a vector.  The "p"
*at the end of dscalp, dnrm2p, and dcopyp indicates that these
*routines operate on a portion of a vector:
*
*<ol>
*<li> colaxpy_j
*<li> colvaxpy_j
*<li> colvraxpy_j
*<li> coldot_j
*<li> colvdot_j
*<li> colscal_j
*<li> dscalp_j
*<li> colswap_j
*<li> colnrm2_j
*<li> dnrm2p_j
*<li> dcopyp_j
*<li> colrot_j
*<li> sign_j
*</ol>
*
*<p>
*<b>IMPORTANT:</b>  The "_j" suffixes indicate that these routines use
*Java style indexing.  For example, you will see
*<pre>
*   for (i = 0; i < n; i++)
*</pre>
*rather than (FORTRAN style)
*<pre>
*   for (i = 1; i <= n; i++)
*</pre>
*To use the "_j" routines you will have to 
*fill elements 0 through n - 1 of vectors rather than elements 1 through n.
*Versions of these programs that use FORTRAN style indexing are
*also available.  They end with the suffix "_f77".
*
*<p>
*This class was translated by a statistician from FORTRAN versions of 
*the LINPACK blas.  It is NOT an official translation.  When 
*public domain Java numerical analysis routines become available 
*from the people who produce LAPACK, then <b>THE CODE PRODUCED
*BY THE NUMERICAL ANALYSTS SHOULD BE USED</b>.
*
*<p>
*Meanwhile, if you have suggestions for improving this
*code, please contact Steve Verrill at steve@ws10.fpl.fs.fed.us.
*
*@author Steve Verrill
*@version .5 --- June 3, 1997
* 
*/

public class Blas_j extends Object {

/**
*
*<p>
*This method multiplies a constant times a vector and adds the product
*to another vector --- dy[&#32] = da*dx[&#32] + dy[&#32].  
*It uses unrolled loops for increments equal to
*one.  It is a translation from FORTRAN to Java of the LINPACK subroutine
*DAXPY.  In the LINPACK listing DAXPY is attributed to Jack Dongarra
*with a date of 3/11/78.
*
*Translated by Steve Verrill, June 3, 1997.
*
*@param  n         The order of the vectors dy[&#32] and dx[&#32]
*@param  da        The constant
*@param  dx[&#32]  This vector will be multiplied by the constant da
*@param  incx      The subscript increment for dx[&#32]
*@param  dy[&#32]  This vector will be added to da*dx[&#32]
*@param  incy      The subscript increment for dy[&#32]
*
*/

   public static void daxpy_j (int n, double da, double dx[], int incx, double
                      dy[], int incy) {

      int i,ix,iy,m;

      if (n <= 0) return;
      if (da == 0.0) return;

      if ((incx == 1) && (incy == 1)) {

//   both increments equal to 1

         m = n%4;

         for (i = 0; i < m; i++) {

            dy[i] += da*dx[i];

         }

         for (i = m; i < n; i += 4) {

            dy[i]   += da*dx[i];
            dy[i+1] += da*dx[i+1];
            dy[i+2] += da*dx[i+2];
            dy[i+3] += da*dx[i+3];

         }

         return;

      } else {

//   at least one increment not equal to 1

         ix = 0;
         iy = 0;

         if (incx < 0) ix = (-n+1)*incx;
         if (incy < 0) iy = (-n+1)*incy;

         for (i = 0; i < n; i++) {

            dy[iy] += da*dx[ix];

            ix += incx;
            iy += incy;

         }

         return;

      }

   } 



/**
*
*<p>
*This method calculates the dot product of two vectors.
*It uses unrolled loops for increments equal to
*one.  It is a translation from FORTRAN to Java of the LINPACK function
*DDOT.  In the LINPACK listing DDOT is attributed to Jack Dongarra
*with a date of 3/11/78.
*
*Translated by Steve Verrill, June 3, 1997.
*
*@param  n         The order of the vectors dx[&#32] and dy[&#32]
*@param  dx[&#32]  vector
*@param  incx      The subscript increment for dx[&#32]
*@param  dy[&#32]  vector
*@param  incy      The subscript increment for dy[&#32]
*
*/

   public static double ddot_j (int n, double dx[], int incx, double
                      dy[], int incy) {

      double ddot;
      int i,ix,iy,m;

      ddot = 0.0;

      if (n <= 0) return ddot;

      if ((incx == 1) && (incy == 1)) {

//   both increments equal to 1

         m = n%5;

         for (i = 0; i < m; i++) {

            ddot += dx[i]*dy[i];

         }

         for (i = m; i < n; i += 5) {

            ddot += dx[i]*dy[i] + dx[i+1]*dy[i+1] + dx[i+2]*dy[i+2] +
                    dx[i+3]*dy[i+3] + dx[i+4]*dy[i+4];

         }

         return ddot;

      } else {

//   at least one increment not equal to 1

         ix = 0;
         iy = 0;

         if (incx < 0) ix = (-n+1)*incx;
         if (incy < 0) iy = (-n+1)*incy;

         for (i = 0; i < n; i++) {

            ddot += dx[ix]*dy[iy];

            ix += incx;
            iy += incy;

         }

         return ddot;

      }

   } 


/**
*
*<p>
*This method scales a vector by a constant.  
*It uses unrolled loops for an increment equal to
*one.  It is a translation from FORTRAN to Java of the LINPACK subroutine
*DSCAL.  In the LINPACK listing DSCAL is attributed to Jack Dongarra
*with a date of 3/11/78.
*
*Translated by Steve Verrill, June 3, 1997.
*
*@param  n         The order of the vector dx[&#32]
*@param  da        The constant
*@param  dx[&#32]  This vector will be multiplied by the constant da
*@param  incx      The subscript increment for dx[&#32]
*
*/

   public static void dscal_j (int n, double da, double dx[], int incx) {

      int i,m,nincx;

      if (n <= 0 || incx <= 0) return;

      if (incx == 1) {

//   increment equal to 1

         m = n%5;

         for (i = 0; i < m; i++) {

            dx[i] *= da;

         }

         for (i = m; i < n; i += 5) {

            dx[i]   *= da;
            dx[i+1] *= da;
            dx[i+2] *= da;
            dx[i+3] *= da;
            dx[i+4] *= da;

         }

         return;

      } else {

//   increment not equal to 1

         nincx = n*incx;

         for (i = 0; i < nincx; i += incx) {

            dx[i] *= da;

         }

         return;

      }

   } 


/**
*
*<p>
*This method interchanges two vectors.
*It uses unrolled loops for increments equal to
*one.  It is a translation from FORTRAN to Java of the LINPACK function
*DSWAP.  In the LINPACK listing DSWAP is attributed to Jack Dongarra
*with a date of 3/11/78.
*
*Translated by Steve Verrill, June 3, 1997.
*
*@param  n         The order of the vectors dx[&#32] and dy[&#32]
*@param  dx[&#32]  vector
*@param  incx      The subscript increment for dx[&#32]
*@param  dy[&#32]  vector
*@param  incy      The subscript increment for dy[&#32]
*
*/

   public static void dswap_j (int n, double dx[], int incx, double
                      dy[], int incy) {

      double dtemp;
      int i,ix,iy,m;

      if (n <= 0) return;

      if ((incx == 1) && (incy == 1)) {

//   both increments equal to 1

         m = n%3;

         for (i = 0; i < m; i++) {

            dtemp = dx[i];
            dx[i] = dy[i];
            dy[i] = dtemp;

         }

         for (i = m; i < n; i += 3) {

            dtemp = dx[i];
            dx[i] = dy[i];
            dy[i] = dtemp;

            dtemp = dx[i+1];
            dx[i+1] = dy[i+1];
            dy[i+1] = dtemp;

            dtemp = dx[i+2];
            dx[i+2] = dy[i+2];
            dy[i+2] = dtemp;

         }

         return;

      } else {

//   at least one increment not equal to 1

         ix = 0;
         iy = 0;

         if (incx < 0) ix = (-n+1)*incx;
         if (incy < 0) iy = (-n+1)*incy;

         for (i = 0; i < n; i++) {

            dtemp = dx[ix];
            dx[ix] = dy[iy];
            dy[iy] = dtemp;

            ix += incx;
            iy += incy;

         }

         return;

      }

   } 


/**
*
*<p>
*This method calculates the Euclidean norm of the vector
*stored in dx[&#32] with storage increment incx.
*It is a translation from FORTRAN to Java of the LINPACK function
*DNRM2.
*In the LINPACK listing DNRM2 is attributed to C.L. Lawson
*with a date of January 8, 1978.  The routine below is based
*on a more recent DNRM2 version that is attributed in LAPACK
*documentation to Sven Hammarling.
*
*Translated by Steve Verrill, June 3, 1997.
*
*@param  n        The order of the vector x[&#32]
*@param  x[&#32]  vector
*@param  incx     The subscript increment for x[&#32]
*
*/


   public static double dnrm2_j (int n, double x[], int incx) {

      double absxi,norm,scale,ssq,fac;
      int ix,limit;

      if (n < 1 || incx < 1) {

         norm = 0.0;

      } else if (n == 1) {

         norm = Math.abs(x[1]);

      } else {

         scale = 0.0;
         ssq = 1.0;

         limit = (n - 1)*incx;

         for (ix = 0; ix <= limit; ix += incx) {

            if (x[ix] != 0.0) {

               absxi = Math.abs(x[ix]);

               if (scale < absxi) {

                  fac = scale/absxi;
                  ssq = 1.0 + ssq*fac*fac;
                  scale = absxi;

               } else {

                  fac = absxi/scale;
                  ssq += fac*fac;

               }

            }

         }

         norm = scale*Math.sqrt(ssq);

      }

      return norm;

   } 


/**
*
*<p>
*This method copies the vector dx[&#32] to the vector dy[&#32].
*It uses unrolled loops for increments equal to
*one.  It is a translation from FORTRAN to Java of the LINPACK subroutine
*DCOPY.  In the LINPACK listing DCOPY is attributed to Jack Dongarra
*with a date of 3/11/78.
*
*Translated by Steve Verrill, March 1, 1997.
*
*@param  n         The order of dx[&#32] and dy[&#32]
*@param  dx[&#32]  vector
*@param  incx      The subscript increment for dx[&#32]
*@param  dy[&#32]  vector
*@param  incy      The subscript increment for dy[&#32]
*
*/

   public static void dcopy_j (int n, double dx[], int incx, double
                      dy[], int incy) {

      double dtemp;
      int i,ix,iy,m;

      if (n <= 0) return;

      if ((incx == 1) && (incy == 1)) {

//   both increments equal to 1

         m = n%7;

         for (i = 0; i < m; i++) {

            dy[i] = dx[i];

         }

         for (i = m; i < n; i += 7) {

            dy[i]   = dx[i];
            dy[i+1] = dx[i+1];
            dy[i+2] = dx[i+2];
            dy[i+3] = dx[i+3];
            dy[i+4] = dx[i+4];
            dy[i+5] = dx[i+5];
            dy[i+6] = dx[i+6];

         }

         return;

      } else {

//   at least one increment not equal to 1

         ix = 0;
         iy = 0;

         if (incx < 0) ix = (-n+1)*incx;
         if (incy < 0) iy = (-n+1)*incy;

         for (i = 0; i < n; i++) {

            dy[iy] = dx[ix];

            ix += incx;
            iy += incy;

         }

         return;

      }

   } 



/**
*
*<p>
*This method constructs a Givens plane rotation.
*It is a translation from FORTRAN to Java of the LINPACK subroutine
*DROTG.  In the LINPACK listing DROTG is attributed to Jack Dongarra
*with a date of 3/11/78.
*
*Translated by Steve Verrill, March 3, 1997.
*
*@param  rotvec[]   Contains the a,b,c,s values.  In Java they
*                   cannot be passed as primitive types (e.g., double
*                   or int or ...) if we want their return values to
*                   be altered.
*
*/

   public static void drotg_j (double rotvec[]) {

//   construct a Givens plane rotation

      double a,b,c,s,roe,scale,r,z,ra,rb;

      a = rotvec[0];
      b = rotvec[1];

      roe = b;

      if (Math.abs(a) > Math.abs(b)) roe = a;

      scale = Math.abs(a) + Math.abs(b);

      if (scale != 0.0) {

         ra = a/scale;
         rb = b/scale;
         r = scale*Math.sqrt(ra*ra + rb*rb);
         r = sign_j(1.0,roe)*r;
         c = a/r;
         s = b/r;
         z = 1.0;
         if (Math.abs(a) > Math.abs(b)) z = s;
         if ((Math.abs(b) >= Math.abs(a)) && (c != 0.0)) z = 1.0/c;

      } else {

         c = 1.0;
         s = 0.0;
         r = 0.0;
         z = 0.0;

      }

      a = r;
      b = z;

      rotvec[0] = a;
      rotvec[1] = b;
      rotvec[2] = c;
      rotvec[3] = s;

      return;

   } 



/**
*
*<p>
*This method multiplies a constant times a portion of a column
*of a matrix and adds the product to the corresponding portion
*of another column of the matrix --- a portion of col2 is 
replaced by the corresponding portion of a*col1 + col2.
*It uses unrolled loops.
*It is a modification of the LINPACK subroutine
*DAXPY.  In the LINPACK listing DAXPY is attributed to Jack Dongarra
*with a date of 3/11/78.
*
*Translated and modified by Steve Verrill, February 26, 1997.
*
*@param  nrow           The number of rows involved
*@param  a              The constant
*@param  x[&#32][&#32]  The matrix
*@param  begin          The starting row
*@param  j1             The id of col1
*@param  j2             The id of col2
*
*/


   public static void colaxpy_j (int nrow, double a, double x[][], int begin,
                        int j1, int j2) {

      int i,m,mpbegin,end;

      if (nrow <= 0) return;
      if (a == 0.0) return;

      m = nrow%4;
      mpbegin = m + begin;
      end = begin + nrow - 1;

      for (i = begin; i < mpbegin; i++) {

         x[i][j2] += a*x[i][j1];

      }

      for (i = mpbegin; i <= end; i += 4) {

         x[i][j2]   += a*x[i][j1];
         x[i+1][j2] += a*x[i+1][j1];
         x[i+2][j2] += a*x[i+2][j1];
         x[i+3][j2] += a*x[i+3][j1];

      }

      return;

   } 


/**
*
*<p>
*This method multiplies a constant times a portion of a column
*of a matrix x[&#32][&#32] and adds the product to the corresponding portion
*of a vector y[&#32] --- a portion of y[&#32] is replaced by the corresponding
*portion of ax[&#32][j] + y[&#32].
*It uses unrolled loops.
*It is a modification of the LINPACK subroutine
*DAXPY.  In the LINPACK listing DAXPY is attributed to Jack Dongarra
*with a date of 3/11/78.
*
*Translated and modified by Steve Verrill, March 1, 1997.
*
*@param  nrow           The number of rows involved
*@param  a              The constant
*@param  x[&#32][&#32]  The matrix
*@param  y[&#32]        The vector
*@param  begin          The starting row
*@param  j              The id of the column of the x matrix
*
*/

   public static void colvaxpy_j (int nrow, double a, double x[][], double y[],
                        int begin, int j) {

      int i,m,mpbegin,end;

      if (nrow <= 0) return;
      if (a == 0.0) return;

      m = nrow%4;
      mpbegin = m + begin;
      end = begin + nrow - 1;

      for (i = begin; i < mpbegin; i++) {

         y[i] += a*x[i][j];

      }

      for (i = mpbegin; i <= end; i += 4) {

         y[i]   += a*x[i][j];
         y[i+1] += a*x[i+1][j];
         y[i+2] += a*x[i+2][j];
         y[i+3] += a*x[i+3][j];

      }

      return;

   } 


/**
*
*<p>
*This method multiplies a constant times a portion of a vector y[&#32]
*and adds the product to the corresponding portion
*of a column of a matrix x[&#32][&#32] --- a portion of column j of x[&#32][&#32] 
*is replaced by the corresponding
*portion of ay[&#32] + x[&#32][j].
*It uses unrolled loops.
*It is a modification of the LINPACK subroutine
*DAXPY.  In the LINPACK listing DAXPY is attributed to Jack Dongarra
*with a date of 3/11/78.
*
*Translated and modified by Steve Verrill, March 3, 1997.
*
*@param  nrow           The number of rows involved
*@param  a              The constant
*@param  y[&#32]        The vector
*@param  x[&#32][&#32]  The matrix
*@param  begin          The starting row
*@param  j              The id of the column of the x matrix
*
*/

   public static void colvraxpy_j (int nrow, double a, double y[], double x[][],
                        int begin, int j) {

      int i,m,mpbegin,end;

      if (nrow <= 0) return;
      if (a == 0.0) return;

      m = nrow%4;
      mpbegin = m + begin;
      end = begin + nrow - 1;

      for (i = begin; i < mpbegin; i++) {

         x[i][j] += a*y[i];

      }

      for (i = mpbegin; i <= end; i += 4) {

         x[i][j]   += a*y[i];
         x[i+1][j] += a*y[i+1];
         x[i+2][j] += a*y[i+2];
         x[i+3][j] += a*y[i+3];

      }

      return;

   } 


/**
*
*<p>
*This method calculates the dot product of portions of two
*columns of a matrix.  It uses unrolled loops.
*It is a modification of the LINPACK function
*DDOT.  In the LINPACK listing DDOT is attributed to Jack Dongarra
*with a date of 3/11/78.
*
*Translated and modified by Steve Verrill, February 27, 1997.
*
*@param  nrow           The number of rows involved
*@param  x[&#32][&#32]  The matrix
*@param  begin          The starting row
*@param  j1             The id of the first column
*@param  j2             The id of the second column
*
*/

   public static double coldot_j (int nrow, double x[][], int begin,
                         int j1, int j2) {

      double coldot;
      int i,m,mpbegin,end;

      coldot = 0.0;

      if (nrow <= 0) return coldot;

      m = nrow%5;
      mpbegin = m + begin;
      end = begin + nrow - 1;

      for (i = begin; i < mpbegin; i++) {

         coldot += x[i][j1]*x[i][j2];

      }

      for (i = mpbegin; i <= end; i += 5) {

         coldot += x[i][j1]*x[i][j2] + 
                   x[i+1][j1]*x[i+1][j2] + 
                   x[i+2][j1]*x[i+2][j2] +
                   x[i+3][j1]*x[i+3][j2] + 
                   x[i+4][j1]*x[i+4][j2];

      }

      return coldot;

   } 


/**
*
*<p>
*This method calculates the dot product of a portion of a
*column of a matrix and the corresponding portion of a
*vector.  It uses unrolled loops.
*It is a modification of the LINPACK function
*DDOT.  In the LINPACK listing DDOT is attributed to Jack Dongarra
*with a date of 3/11/78.
*
*Translated and modified by Steve Verrill, March 1, 1997.
*
*@param  nrow           The number of rows involved
*@param  x[&#32][&#32]  The matrix
*@param  y[&#32]        The vector
*@param  begin          The starting row
*@param  j              The id of the column of the matrix
*
*/

   public static double colvdot_j (int nrow, double x[][], double y[],
                         int begin, int j) {

      double colvdot;
      int i,m,mpbegin,end;

      colvdot = 0.0;

      if (nrow <= 0) return colvdot;

      m = nrow%5;
      mpbegin = m + begin;
      end = begin + nrow - 1;

      for (i = begin; i < mpbegin; i++) {

         colvdot += x[i][j]*y[i];

      }

      for (i = mpbegin; i <= end; i += 5) {

         colvdot += x[i][j]*y[i] + 
                   x[i+1][j]*y[i+1] + 
                   x[i+2][j]*y[i+2] +
                   x[i+3][j]*y[i+3] + 
                   x[i+4][j]*y[i+4];

      }

      return colvdot;

   } 


/**
*
*<p>
*This method scales a portion of a column of a matrix by a constant.  
*It uses unrolled loops.
*It is a modification of the LINPACK subroutine
*DSCAL.  In the LINPACK listing DSCAL is attributed to Jack Dongarra
*with a date of 3/11/78.
*
*Translated and modified by Steve Verrill, February 27, 1997.
*
*@param  nrow           The number of rows involved
*@param  a              The constant
*@param  x[&#32][&#32]  The matrix
*@param  begin          The starting row
*@param  j              The id of the column
*
*/

   public static void colscal_j (int nrow, double a, double x[][], int begin, int j) {

      int i,m,mpbegin,end;

      if (nrow <= 0) return;

      m = nrow%5;
      mpbegin = m + begin;
      end = begin + nrow - 1;

      for (i = begin; i < mpbegin; i++) {

         x[i][j] *= a;

      }

      for (i = mpbegin; i <= end; i += 5) {

         x[i][j]   *= a;
         x[i+1][j] *= a;
         x[i+2][j] *= a;
         x[i+3][j] *= a;
         x[i+4][j] *= a;

      }

      return;

   }          


/**
*
*<p>
*This method scales a portion of a vector by a constant.  
*It uses unrolled loops.
*It is a modification of the LINPACK subroutine
*DSCAL.  In the LINPACK listing DSCAL is attributed to Jack Dongarra
*with a date of 3/11/78.
*
*Translated and modified by Steve Verrill, March 3, 1997.
*
*@param  nrow           The number of rows involved
*@param  a              The constant
*@param  x[&#32]        The vector
*@param  begin          The starting row
*
*/

   public static void dscalp_j (int nrow, double a, double x[], int begin) {

      int i,m,mpbegin,end;

      if (nrow <= 0) return;

      m = nrow%5;
      mpbegin = m + begin;
      end = begin + nrow - 1;

      for (i = begin; i < mpbegin; i++) {

         x[i] *= a;

      }

      for (i = mpbegin; i <= end; i += 5) {

         x[i]   *= a;
         x[i+1] *= a;
         x[i+2] *= a;
         x[i+3] *= a;
         x[i+4] *= a;

      }

      return;

   }          



/**
*
*<p>
*This method interchanges two columns of a matrix.
*It uses unrolled loops.
*It is a modification of the LINPACK function
*DSWAP.  In the LINPACK listing DSWAP is attributed to Jack Dongarra
*with a date of 3/11/78.
*
*Translated and modified by Steve Verrill, February 26, 1997.
*
*@param  n              The number of rows of the matrix
*@param  x[&#32][&#32]  The matrix
*@param  j1             The id of the first column
*@param  j2             The id of the second column
*
*/

   public static void colswap_j (int n, double x[][], int j1, int j2) {

      double temp;
      int i,m;

      if (n <= 0) return;

      m = n%3;

      for (i = 0; i < m; i++) {

         temp = x[i][j1];
         x[i][j1] = x[i][j2];
         x[i][j2] = temp;

      }

      for (i = m; i < n; i += 3) {

         temp = x[i][j1];
         x[i][j1] = x[i][j2];
         x[i][j2] = temp;

         temp = x[i+1][j1];
         x[i+1][j1] = x[i+1][j2];
         x[i+1][j2] = temp;

         temp = x[i+2][j1];
         x[i+2][j1] = x[i+2][j2];
         x[i+2][j2] = temp;

      }

      return;

   }



/**
*
*<p>
*This method calculates the Euclidean norm of a portion of a
*column of a matrix.
*It is a modification of the LINPACK function
*dnrm2.
*In the LINPACK listing dnrm2 is attributed to C.L. Lawson
*with a date of January 8, 1978.  The routine below is based
*on a more recent dnrm2 version that is attributed in LAPACK
*documentation to Sven Hammarling.
*
*Translated and modified by Steve Verrill, February 26, 1997.
*
*@param  nrow           The number of rows involved
*@param  x[&#32][&#32]  The matrix
*@param  begin          The starting row
*@param  j              The id of the column
*
*/


   public static double colnrm2_j (int nrow, double x[][], int begin, int j) {

      double absxij,norm,scale,ssq,fac;
      int i,end;

      if (nrow < 1) {

         norm = 0.0;

      } else if (nrow == 1) {

         norm = Math.abs(x[begin][j]);

      } else {

         scale = 0.0;
         ssq = 1.0;

         end = begin + nrow - 1;

         for (i = begin; i <= end; i++) {

            if (x[i][j] != 0.0) {

               absxij = Math.abs(x[i][j]);

               if (scale < absxij) {

                  fac = scale/absxij;
                  ssq = 1.0 + ssq*fac*fac;
                  scale = absxij;

               } else {

                  fac = absxij/scale;
                  ssq += fac*fac;

               }

            }

         }

         norm = scale*Math.sqrt(ssq);

      }

      return norm;

   } 


/**
*
*<p>
*This method calculates the Euclidean norm of a portion
*of a vector x[&#32].
*It is a modification of the LINPACK function
*dnrm2.
*In the LINPACK listing dnrm2 is attributed to C.L. Lawson
*with a date of January 8, 1978.  The routine below is based
*on a more recent dnrm2 version that is attributed in LAPACK
*documentation to Sven Hammarling.
*
*Translated by Steve Verrill, March 3, 1997.
*
*@param  nrow     The number of rows involved
*@param  x[&#32]  vector
*@param  begin    The starting row
*
*/


   public static double dnrm2p_j (int nrow, double x[], int begin) {

      double absxi,norm,scale,ssq,fac;
      int i,end;

      if (nrow < 1) {

         norm = 0.0;

      } else if (nrow == 1) {

         norm = Math.abs(x[begin]);

      } else {

         scale = 0.0;
         ssq = 1.0;

         end = begin + nrow - 1;

         for (i = begin; i <= end; i++) {

            if (x[i] != 0.0) {

               absxi = Math.abs(x[i]);

               if (scale < absxi) {

                  fac = scale/absxi;
                  ssq = 1.0 + ssq*fac*fac;
                  scale = absxi;

               } else {

                  fac = absxi/scale;
                  ssq += fac*fac;

               }

            }

         }

         norm = scale*Math.sqrt(ssq);

      }

      return norm;

   } 



/**
*
*<p>
*This method copies a portion of vector x[&#32] to the corresponding
portion of vector y[&#32].
*It uses unrolled loops.
*It is a modification of the LINPACK subroutine
*dcopy.  In the LINPACK listing dcopy is attributed to Jack Dongarra
*with a date of 3/11/78.
*
*Translated by Steve Verrill, March 1, 1997.
*
*@param  nrow     The number of rows involved
*@param  x[&#32]  vector
*@param  y[&#32]  vector
*@param  begin    The starting row
*
*/

   public static void dcopyp_j (int nrow, double x[], double y[], int begin) {

      double temp;
      int i,m,mpbegin,end;

      m = nrow%7;
      mpbegin = m + begin;
      end = begin + nrow - 1;

      for (i = begin; i < mpbegin; i++) {

            y[i] = x[i];

         }

      for (i = mpbegin; i <= end; i += 7) {

         y[i]   = x[i];
         y[i+1] = x[i+1];
         y[i+2] = x[i+2];
         y[i+3] = x[i+3];
         y[i+4] = x[i+4];
         y[i+5] = x[i+5];
         y[i+6] = x[i+6];

      }

       return;

   } 


/**
*
*<p>
*This method "applies a plane rotation."
*It is a modification of the LINPACK function
*DROT.  In the LINPACK listing DROT is attributed to Jack Dongarra
*with a date of 3/11/78.
*
*Translated and modified by Steve Verrill, March 4, 1997.
*
*@param  n              The order of x[&#32][&#32]
*@param  x[&#32][&#32]  The matrix
*@param  j1             The id of the first column
*@param  j2             The id of the second column
*@param  c              "cos"
*@param  s              "sin"
*
*/

   public static void colrot_j (int n, double x[][],
                       int j1, int j2, double c, double s) {

      double temp;
      int i;


      if (n <= 0) return;

      for (i = 0; i < n; i++) {

         temp = c*x[i][j1] + s*x[i][j2];
         x[i][j2] = c*x[i][j2] - s*x[i][j1];
         x[i][j1] = temp;

      } 

      return;

   } 



/**
*
*<p>
*This method implements the FORTRAN sign (not sin) function.
*See the code for details.
*
*Created by Steve Verrill, March 1997.
*
*@param  a   a
*@param  b   b
*
*/

   public static double sign_j (double a, double b) {

      if (b < 0.0) {

         return -Math.abs(a);

      } else {

         return Math.abs(a);      

      }

   }



/**
*
*<p>
*This method multiplies an n x p matrix by a p x r matrix.
*
*Created by Steve Verrill, March 1997.
*
*@param  a[&#32][&#32]   The left matrix
*@param  b[&#32][&#32]   The right matrix
*@param  c[&#32][&#32]   The product
*@param  n   n
*@param  p   p
*@param  r   r
*
*/

   public static void matmat_j (double a[][], double b[][], double c[][],
                                int n, int p, int r) {

      double vdot;
      int i,j,k,m;

      for (i = 0; i < n; i++) {

         for (j = 0; j < r; j++) {

            vdot = 0.0;

            m = p%5;

            for (k = 0; k < m; k++) {

               vdot += a[i][k]*b[k][j];

            }

            for (k = m; k < p; k += 5) {

               vdot += a[i][k]*b[k][j] + 
                       a[i][k+1]*b[k+1][j] + 
                       a[i][k+2]*b[k+2][j] +
                       a[i][k+3]*b[k+3][j] + 
                       a[i][k+4]*b[k+4][j];

            }

            c[i][j] = vdot;

         }

      }

   }



/**
*
*<p>
*This method obtains the transpose of an n x p matrix.
*
*Created by Steve Verrill, March 1997.
*
*@param  a[&#32][&#32]    matrix
*@param  at[&#32][&#32]   transpose of the matrix
*@param  n                n
*@param  p                p
*
*/

   public static void mattran_j (double a[][], double at[][],
                                int n, int p) {

      int i,j;

      for (i = 0; i < n; i++) {

         for (j = 0; j < p; j++) {

            at[j][i] = a[i][j];

         }

      }

   }



/**
*
*<p>
*This method multiplies an n x p matrix by a p x 1 vector.
*
*Created by Steve Verrill, March 1997.
*
*@param  a[&#32][&#32]   The matrix
*@param  b[&#32]         The vector
*@param  c[&#32]         The product
*@param  n               n
*@param  p               p
*
*/

   public static void matvec_j (double a[][], double b[], double c[],
                                int n, int p) {

      double vdot;
      int i,j,m;

      for (i = 0; i < n; i++) {

         vdot = 0.0;

         m = p%5;

         for (j = 0; j < m; j++) {

            vdot += a[i][j]*b[j];

         }

         for (j = m; j < p; j += 5) {

               vdot += a[i][j]*b[j] + 
                       a[i][j+1]*b[j+1] + 
                       a[i][j+2]*b[j+2] +
                       a[i][j+3]*b[j+3] + 
                       a[i][j+4]*b[j+4];

         }

         c[i] = vdot;

      }

   }




}




Verrill, February 27, 1997.
*
*@param  nrow           The number of rows involved
*@param  a              The coCholTest.java                                                                                       0100644 0007456 0000012 00000020623 06251367234 0014007 0                                                                                                    ustar 00steve                           staff                           0000040 0000007                                                                                                                                                                        
package linear_algebra;

import java.util.*;
import java.lang.*;
import java.io.*;
import linear_algebra.*;
import corejava.Console;

/**
*
*This class tests the Cholesky decomposition class
*and the Triangular solve/invert class.
*
*It
*<ol>
*<li> Randomly generates numbers between randlow and randhigh
*and fills a lower triangular matrix R with them.  Note that
*randlow and randhigh should be of the same sign.  Otherwise it
*would be possible for R not to be of full rank.
*<li> Obtains the positive definite matrix A = RR&#180.
*<li> Randomly generates a vector x.
*<li> Calculates the vector b = Ax.
*<li> Performs a Cholesky decomposition of A in an effort to
*recover R. (This tests Cholesky.factorPosDef.)
*<li> Solves the system Az = b in an effort to recover x. 
*(This tests Cholesky.solvePosDef, Triangular.solveLower,
* and Triangular.solveUpper.)
*<li> Obtains an estimate of A^{-1} and compares A^{-1}A with the
*identity matrix. (This tests Cholesky.invertPosDef and
*Triangular.invertLower.)
*<li> Obtains the inverse of an upper triangular matrix and compares
*the product of this inverse and the original upper triangular matrix
*with the identity matrix. (This tests Triangular.invertUpper.)
*</ol>
*
*@author Steve Verrill
*@version .5 --- November 30, 1996
*
*/


public class CholTest extends Object {


   public static void main (String args[]) {

      Cholesky cholesky = new Cholesky();
      Triangular triangular = new Triangular();

/*

Some of the variables of main:

1.)   randstart --- The integer starting value for the 
                    random number generator.
2.)           n --- The order of the vectors and matrices.
3.)       r[][] --- Holds the lower triangular Cholesky factor of
                    the positive definite matrix A.
4.)       a[][] --- Holds the lower triangle of A.
5.)      a2[][] --- A copy of a[][].
6.)  aupper[][] --- Holds the upper triangle of A.
7.) aupper2[][] --- A copy of aupper[][].
8.)   afull[][] --- Holds all of A.
9.)         x[] --- The x in Ax = b.
10.)        y[] --- Work array needed to help solve Ax = b.
11.)        b[] --- The b in Ax = b.

*/ 

      long randstart;

      int n;

      int i,j,k;

      double randlow,randhigh,randdiff;

      double r[][] = new double[100][100];
      double a[][] = new double[100][100];
      double a2[][] = new double[100][100];
      double aupper[][] = new double[100][100];
      double aupper2[][] = new double[100][100];
      double afull[][] = new double[100][100];

      double x[] = new double[100];
      double y[] = new double[100];
      double b[] = new double[100];

/*

   Console is a public domain class described in Cornell
   and Horstmann's Core Java (SunSoft Press/Prentice-Hall).

*/
   
      randstart = Console.readInt("\nWhat is the starting value (an " +
      "integer)\n" +
      "for the random number generator? ");

      n = Console.readInt("\nWhat is the n value? (100 or less) ");

      Random rand01 = new Random(randstart);

      randlow = Console.readDouble("\nWhat is randlow? ");

      randhigh = Console.readDouble("\nWhat is randhigh? ");

      randdiff = randhigh - randlow;

/*

   Generate a positive definite matrix.

*/

      for (i = 0; i < n; i++) {

         for (j = 0; j <= i; j++) {

            r[i][j] = randlow + rand01.nextDouble()*randdiff;

         }

      }
      System.out.print("\n\n");
      System.out.print("*********************************************\n");
      System.out.print("\nThe r matrix is \n\n");

      for (i = 0; i < n; i++) {

         for (j = 0; j <= i; j++) {

            System.out.print(r[i][j] + "  ");

         }

         System.out.print("\n");

      }

/*

   The positive definite matrix A = RR'.

*/

      for (j = 0; j < n; j++) {

         for (i = j; i < n; i++) {

            a[i][j] = 0.0;

            for (k = 0; k <= j; k++) {

               a[i][j] += r[i][k]*r[j][k];

            }

            a2[i][j] = a[i][j];
/*

   a[][] only has the lower triangle filled.
   a2[][] is a duplicate of a[][].
   aupper[][] has only the upper triangle filled.
   aupper2[][] is a duplicate of aupper[][].
   afull[][] has both the upper and lower triangles filled.

*/
            afull[i][j] = a[i][j];
            afull[j][i] = afull[i][j];

            aupper[j][i] = a[i][j];
            aupper2[j][i] = a[i][j];

         }

      }




/*

   Generate a vector x

*/

      for (i = 0; i < n; i++) {

         x[i] = randlow + rand01.nextDouble()*randdiff;

      }


/*

   Calculate Ax = b

*/

      for (i = 0; i < n; i++) {

         b[i] = 0.0;

         for (j = 0; j < n; j++) {

            b[i] += afull[i][j]*x[j];

         }

      }


/*   

   This should recover R.

*/

      try {

         cholesky.factorPosDef(a,n);

      } catch (NotPosDefException npd) {

         System.out.print("\nThe matrix was not positive definite" +
         " so it was not possible\n" +
         "to obtain a Cholesky decomposition.\n\n");  

         System.exit(0);

      }   


      System.out.print("\n\nThe recovered r matrix is \n\n");

      for (i = 0; i < n; i++) {

         for (j = 0; j <= i; j++) {

            System.out.print(a[i][j] + "  ");

         }

         System.out.print("\n");

      }

      System.out.print("\n");
      System.out.print("*********************************************\n");
      System.out.print("\n");

/*   

   This should obtain the solution to Ax = b.
   The solution x will be in the vector b.

*/

      try {

         cholesky.solvePosDef(a,b,y,n,true);

      } catch (NotPosDefException npd) {

         System.out.print("\nThe matrix was not positive definite" +
         " so it was not possible\n" +
         "to solve Ax = b.\n\n");  

      }   


      System.out.print("\nThe input x vector was\n\n");

      for (i = 0; i < n; i++) {

         System.out.print(x[i] + "\n");

      }

      System.out.print("\n\nThe recovered x vector was\n\n");

      for (i = 0; i < n; i++) {

         System.out.print(b[i] + "\n");

      }

      System.out.print("\n");
      System.out.print("\n");
      System.out.print("*********************************************\n");
      System.out.print("\n");

/*

   Obtain the inverse of A.

*/
      try {

         cholesky.invertPosDef(a2,n,false);

      } catch (NotPosDefException npd) {

         System.out.print("\nThe matrix was not positive definite" +
         " so it was not possible\n" +
         "to use a Cholesky decomposition to obtain an inverse.\n\n");

      }

/*

   Fill the upper triangle of a2 so a2[][] = A^{-1}.

*/
      
      for (i = 0; i < n - 1; i++) {

         for (j = i + 1; j < n; j++) {

            a2[i][j] = a2[j][i];

         }

      }

/*

   now a2*afull should equal I

*/

      for (i = 0; i < n; i++) {

         for (j = 0; j <= i; j++) {

            a[i][j] = 0.0;

            for (k = 0; k < n; k++) {

               a[i][j] += a2[i][k]*afull[k][j];

            }

         }

      }

      System.out.print("\nA test of Cholesky.invertPosDef\n\n" +
      "The lower triangle of the I matrix is:\n\n");

      for (i = 0; i < n; i++) {

         for (j = 0; j <= i; j++) {

            System.out.print(a[i][j] + "  ");

         }

         System.out.print("\n");

      }

      System.out.print("\n");
      System.out.print("\n");
      System.out.print("*********************************************\n");
      System.out.print("\n");

/*

    Test the inversion of upper triangular matrices.

*/

/*

   Obtain the inverse of aupper[][].

*/
      try {

         triangular.invertUpper(aupper,n);

      } catch (NotFullRankException nfr) {

         System.out.print("\nThe upper triangular matrix was not of" +
         " full rank\n" + " so it was not possible " +
         "to invert it.\n\n");

      }

/*

   now aupper*aupper2 should equal I

*/

      for (i = 0; i < n; i++) {

         for (j = i; j < n; j++) {

            a[i][j] = 0.0;

            for (k = 0; k < n; k++) {

               a[i][j] += aupper[i][k]*aupper2[k][j];

            }

         }

      }

      System.out.print("\nA test of Triangular.invertUpper\n\n" +
      "The transpose of the upper triangle of the I matrix is:\n\n");

      for (i = 0; i < n; i++) {

         for (j = 0; j <= i; j++) {

            System.out.print(a[j][i] + "  ");

         }

         System.out.print("\n");

      }

      System.out.print("\n");
      System.out.print("\n");
      System.out.print("*********************************************\n");
      System.out.print("\n");

   }

}



                                                                                                             CholTest_f77.java                                                                                   0100644 0007456 0000012 00000014452 06312344507 0014471 0                                                                                                    ustar 00steve                           staff                           0000040 0000007                                                                                                                                                                        
package linear_algebra;

import java.util.*;
import java.lang.*;
import java.io.*;
import linear_algebra.*;
import corejava.Console;

/**
*
*This class tests the Cholesky_f77 classes.
*
*It
*<ol>
*<li> Randomly generates numbers between randlow and randhigh
*and fills a lower triangular matrix R with them.  Note that
*randlow and randhigh should be of the same sign.  Otherwise it
*would be possible for R not to be of full rank.
*<li> Obtains the positive definite matrix A = RR&#180.
*<li> Randomly generates a vector x.
*<li> Calculates the vector b = Ax.
*<li> Performs a Cholesky decomposition of A in an effort to
*recover R. (This tests Cholesky_f77.dpofa_f77.)
*<li> Solves the system Az = b in an effort to recover x. 
*(This tests Cholesky_f77.dposl_f77.)
*<li> Obtains an estimate of A^{-1} and compares A^{-1}A with the
*identity matrix. (This tests Cholesky_f77.dpodi_f77.)
*</ol>
*
*@author Steve Verrill
*@version .5 --- March 12, 1997
*
*/


public class CholTest_f77 extends Object {


   public static void main (String args[]) {


/*

Some of the variables of main:

1.)   randstart --- The integer starting value for the 
                    random number generator.
2.)           n --- The order of the vectors and matrices.
3.)       r[][] --- Holds the lower triangular Cholesky factor of
                    the positive definite matrix A.
4.)       a[][] --- Holds the lower triangle of A.
5.)  aupper[][] --- Holds the upper triangle of A.
6.)   afull[][] --- Holds all of A.
7.)         x[] --- The x in Ax = b.
8.)         y[] --- Work array needed to help solve Ax = b.
9.)         b[] --- The b in Ax = b.

*/ 

      long randstart;

      int n;

      int i,j,k;

      double randlow,randhigh,randdiff;

      double r[][] = new double[101][101];
      double a[][] = new double[101][101];
      double aupper[][] = new double[101][101];
      double afull[][] = new double[101][101];

      double x[] = new double[101];
      double y[] = new double[101];
      double b[] = new double[101];

      double det[] = new double[2];

/*

   Console is a public domain class described in Cornell
   and Horstmann's Core Java (SunSoft Press, Prentice-Hall).

*/
   
      randstart = Console.readInt("\nWhat is the starting value (an " +
      "integer)\n" +
      "for the random number generator? ");

      n = Console.readInt("\nWhat is the n value? (100 or less) ");

      Random rand01 = new Random(randstart);

      randlow = Console.readDouble("\nWhat is randlow? ");

      randhigh = Console.readDouble("\nWhat is randhigh? ");

      randdiff = randhigh - randlow;

/*

   Generate a positive definite matrix.

*/

      for (i = 1; i <= n; i++) {

         for (j = 1; j <= i; j++) {

            r[i][j] = randlow + rand01.nextDouble()*randdiff;

         }

      }
      System.out.print("\n\n");
      System.out.print("*********************************************\n");
      System.out.print("\nThe r matrix is \n\n");

      for (i = 1; i <= n; i++) {

         for (j = 1; j <= i; j++) {

            System.out.print(r[i][j] + "  ");

         }

         System.out.print("\n");

      }

/*

   The positive definite matrix A = RR'.

*/

      for (j = 1; j <= n; j++) {

         for (i = j; i <= n; i++) {

            a[i][j] = 0.0;

            for (k = 1; k <= j; k++) {

               a[i][j] += r[i][k]*r[j][k];

            }

/*

   a[][] only has the lower triangle filled.
   aupper[][] has only the upper triangle filled.
   afull[][] has both the upper and lower triangles filled.

*/

            aupper[j][i] = a[i][j];

            afull[i][j] = a[i][j];
            afull[j][i] = afull[i][j];


         }

      }




/*

   Generate a vector x

*/

      for (i = 1; i <= n; i++) {

         x[i] = randlow + rand01.nextDouble()*randdiff;

      }


/*

   Calculate Ax = b

*/

      for (i = 1; i <= n; i++) {

         b[i] = 0.0;

         for (j = 1; j <= n; j++) {

            b[i] += afull[i][j]*x[j];

         }

      }


/*   

   This should recover R.

*/

      try {

         Cholesky_f77.dpofa_f77(aupper,n);

      } catch (NotPosDefException npd) {

         System.out.print("\nThe matrix was not positive definite" +
         " so it was not possible\n" +
         "to obtain a Cholesky decomposition.\n\n");  

         System.out.print("\nThe info value was " + npd.info + ".\n\n");  

         System.exit(0);

      }
   


      System.out.print("\n\nThe recovered r matrix is \n\n");

      for (i = 1; i <= n; i++) {

         for (j = 1; j <= i; j++) {

            System.out.print(aupper[j][i] + "  ");

         }

         System.out.print("\n");

      }

      System.out.print("\n");
      System.out.print("*********************************************\n");
      System.out.print("\n");

/*   

   This should obtain the solution to Ax = b.
   The solution x will be in the vector b.

*/

      Cholesky_f77.dposl_f77(aupper,n,b);


      System.out.print("\nThe input x vector was\n\n");

      for (i = 1; i <= n; i++) {

         System.out.print(x[i] + "\n");

      }

      System.out.print("\n\nThe recovered x vector was\n\n");

      for (i = 1; i <= n; i++) {

         System.out.print(b[i] + "\n");

      }

      System.out.print("\n");
      System.out.print("\n");
      System.out.print("*********************************************\n");
      System.out.print("\n");

/*

   Obtain the inverse of A.

*/

      Cholesky_f77.dpodi_f77(aupper,n,det,1);

/*

   Fill the  lower triangle of aupper so aupper[][] = A^{-1}.

*/
      
      for (i = 2; i <= n; i++) {

         for (j = 1; j < i; j++) {

            aupper[i][j] = aupper[j][i];

         }

      }

/*

   now aupper*afull should equal I

*/

      for (i = 1; i <= n; i++) {

         for (j = 1; j <= i; j++) {

            a[i][j] = 0.0;

            for (k = 1; k <= n; k++) {

               a[i][j] += aupper[i][k]*afull[k][j];

            }

         }

      }

      System.out.print("\nA test of Cholesky_f77.dpodi_f77\n\n" +
      "The lower triangle of the I matrix is:\n\n");

      for (i = 1; i <= n; i++) {

         for (j = 1; j <= i; j++) {

            System.out.print(a[i][j] + "  ");

         }

         System.out.print("\n");

      }

      System.out.print("\n");
      System.out.print("\n");
      System.out.print("*********************************************\n");
      System.out.print("\n");


   }

}




                                                                                                                                                                                                                      CholTest_j.java                                                                                     0100644 0007456 0000012 00000014372 06347360007 0014321 0                                                                                                    ustar 00steve                           staff                           0000040 0000007                                                                                                                                                                        
package linear_algebra;

import java.util.*;
import java.lang.*;
import java.io.*;
import linear_algebra.*;
import corejava.Console;

/**
*
*This class tests the Cholesky_j classes.
*
*It
*<ol>
*<li> Randomly generates numbers between randlow and randhigh
*and fills a lower triangular matrix R with them.  Note that
*randlow and randhigh should be of the same sign.  Otherwise it
*would be possible for R not to be of full rank.
*<li> Obtains the positive definite matrix A = RR&#180.
*<li> Randomly generates a vector x.
*<li> Calculates the vector b = Ax.
*<li> Performs a Cholesky decomposition of A in an effort to
*recover R. (This tests Cholesky_j.dpofa_j.)
*<li> Solves the system Az = b in an effort to recover x. 
*(This tests Cholesky_j.dposl_j.)
*<li> Obtains an estimate of A^{-1} and compares A^{-1}A with the
*identity matrix. (This tests Cholesky_j.dpodi_j.)
*</ol>
*
*@author Steve Verrill
*@version .5 --- June 5, 1997
*
*/


public class CholTest_j extends Object {


   public static void main (String args[]) {


/*

Some of the variables of main:

1.)   randstart --- The integer starting value for the 
                    random number generator.
2.)           n --- The order of the vectors and matrices.
3.)       r[][] --- Holds the lower triangular Cholesky factor of
                    the positive definite matrix A.
4.)       a[][] --- Holds the lower triangle of A.
5.)  aupper[][] --- Holds the upper triangle of A.
6.)   afull[][] --- Holds all of A.
7.)         x[] --- The x in Ax = b.
8.)         y[] --- Work array needed to help solve Ax = b.
9.)         b[] --- The b in Ax = b.

*/ 

      long randstart;

      int n;

      int i,j,k;

      double randlow,randhigh,randdiff;

      double r[][] = new double[100][100];
      double a[][] = new double[100][100];
      double aupper[][] = new double[100][100];
      double afull[][] = new double[100][100];

      double x[] = new double[100];
      double y[] = new double[100];
      double b[] = new double[100];

      double det[] = new double[2];

/*

   Console is a public domain class described in Cornell
   and Horstmann's Core Java (SunSoft Press, Prentice-Hall).

*/
   
      randstart = Console.readInt("\nWhat is the starting value (an " +
      "integer)\n" +
      "for the random number generator? ");

      n = Console.readInt("\nWhat is the n value? (100 or less) ");

      Random rand01 = new Random(randstart);

      randlow = Console.readDouble("\nWhat is randlow? ");

      randhigh = Console.readDouble("\nWhat is randhigh? ");

      randdiff = randhigh - randlow;

/*

   Generate a positive definite matrix.

*/

      for (i = 0; i < n; i++) {

         for (j = 0; j <= i; j++) {

            r[i][j] = randlow + rand01.nextDouble()*randdiff;

         }

      }
      System.out.print("\n\n");
      System.out.print("*********************************************\n");
      System.out.print("\nThe r matrix is \n\n");

      for (i = 0; i < n; i++) {

         for (j = 0; j <= i; j++) {

            System.out.print(r[i][j] + "  ");

         }

         System.out.print("\n");

      }

/*

   The positive definite matrix A = RR'.

*/

      for (j = 0; j < n; j++) {

         for (i = j; i < n; i++) {

            a[i][j] = 0.0;

            for (k = 0; k <= j; k++) {

               a[i][j] += r[i][k]*r[j][k];

            }

/*

   a[][] only has the lower triangle filled.
   aupper[][] has only the upper triangle filled.
   afull[][] has both the upper and lower triangles filled.

*/

            aupper[j][i] = a[i][j];

            afull[i][j] = a[i][j];
            afull[j][i] = afull[i][j];


         }

      }




/*

   Generate a vector x

*/

      for (i = 0; i < n; i++) {

         x[i] = randlow + rand01.nextDouble()*randdiff;

      }


/*

   Calculate Ax = b

*/

      for (i = 0; i < n; i++) {

         b[i] = 0.0;

         for (j = 0; j < n; j++) {

            b[i] += afull[i][j]*x[j];

         }

      }


/*   

   This should recover R.

*/

      try {

         Cholesky_j.dpofa_j(aupper,n);

      } catch (NotPosDefException npd) {

         System.out.print("\nThe matrix was not positive definite" +
         " so it was not possible\n" +
         "to obtain a Cholesky decomposition.\n\n");  

         System.out.print("\nThe info value was " + npd.info + ".\n\n");  

         System.exit(0);

      }
   


      System.out.print("\n\nThe recovered r matrix is \n\n");

      for (i = 0; i < n; i++) {

         for (j = 0; j <= i; j++) {

            System.out.print(aupper[j][i] + "  ");

         }

         System.out.print("\n");

      }

      System.out.print("\n");
      System.out.print("*********************************************\n");
      System.out.print("\n");

/*   

   This should obtain the solution to Ax = b.
   The solution x will be in the vector b.

*/

      Cholesky_j.dposl_j(aupper,n,b);


      System.out.print("\nThe input x vector was\n\n");

      for (i = 0; i < n; i++) {

         System.out.print(x[i] + "\n");

      }

      System.out.print("\n\nThe recovered x vector was\n\n");

      for (i = 0; i < n; i++) {

         System.out.print(b[i] + "\n");

      }

      System.out.print("\n");
      System.out.print("\n");
      System.out.print("*********************************************\n");
      System.out.print("\n");

/*

   Obtain the inverse of A.

*/

      Cholesky_j.dpodi_j(aupper,n,det,1);

/*

   Fill the  lower triangle of aupper so aupper[][] = A^{-1}.

*/
      
      for (i = 1; i < n; i++) {

         for (j = 0; j < i; j++) {

            aupper[i][j] = aupper[j][i];

         }

      }

/*

   now aupper*afull should equal I

*/

      for (i = 0; i < n; i++) {

         for (j = 0; j <= i; j++) {

            a[i][j] = 0.0;

            for (k = 0; k < n; k++) {

               a[i][j] += aupper[i][k]*afull[k][j];

            }

         }

      }

      System.out.print("\nA test of Cholesky_j.dpodi_j\n\n" +
      "The lower triangle of the I matrix is:\n\n");

      for (i = 0; i < n; i++) {

         for (j = 0; j <= i; j++) {

            System.out.print(a[i][j] + "  ");

         }

         System.out.print("\n");

      }

      System.out.print("\n");
      System.out.print("\n");
      System.out.print("*********************************************\n");
      System.out.print("\n");


   }

}




                                                                                                                                                                                                                                                                      Cholesky.java                                                                                       0100644 0007456 0000012 00000024542 06251367234 0014047 0                                                                                                    ustar 00steve                           staff                           0000040 0000007                                                                                                                                                                        /*
    Cholesky.java copyright claim:

    Copyright (C) 1996 by Steve Verrill.

    This class is free software; you can redistribute it and/or 
    modify it under the terms of version 2 of the GNU General 
    Public License as published by the Free Software Foundation.

    This class is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with these programs in the file COPYING; if not, write to 
    the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, 
    MA 02139, USA.

    The author's mail address is:

    Steve Verrill 
    USDA Forest Products Laboratory
    1 Gifford Pinchot Drive
    Madison, Wisconsin
    53705

    The author's e-mail address is:

    steve@ws10.fpl.fs.fed.us


***********************************************************************

DISCLAIMER OF WARRANTIES:

THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND. 
THE AUTHOR DOES NOT WARRANT, GUARANTEE OR MAKE ANY REPRESENTATIONS 
REGARDING THE SOFTWARE OR DOCUMENTATION IN TERMS OF THEIR CORRECTNESS, 
RELIABILITY, CURRENTNESS, OR OTHERWISE. THE ENTIRE RISK AS TO 
THE RESULTS AND PERFORMANCE OF THE SOFTWARE IS ASSUMED BY YOU. 
IN NO CASE WILL ANY PARTY INVOLVED WITH THE CREATION OR DISTRIBUTION 
OF THE SOFTWARE BE LIABLE FOR ANY DAMAGE THAT MAY RESULT FROM THE USE 
OF THIS SOFTWARE.

Sorry about that.

***********************************************************************


History:

Date        Author            Changes

11/30/96    Steve Verrill     Created

*/


package linear_algebra;

import linear_algebra.*;


/**
*
*<p>
*This class contains:
*<ol>
*<li> a method that obtains the Cholesky
*factorization RR&#180, where R is a lower triangular matrix, 
*of a symmetric positive definite matrix A.
*<li> a method to invert a symmetric positive
*definite matrix.
*<li> a method to solve Ax = b where A is a symmetric
*positive definite matrix.
*</ol>
*
*<p>
*This class was written by a statistician rather than
*a numerical analyst.  I have tried to check the code carefully,
*but it may still contain bugs.  Further, its stability and
*efficiency do not meet the standards of high quality
*numerical analysis software.  When public domain Java numerical 
*analysis routines become available from numerical analysts 
*(e.g., the people who produce LAPACK), then <b>THE CODE PRODUCED
*BY THE NUMERICAL ANALYSTS SHOULD BE USED</b>.
*
*<p>
*Meanwhile, if you have suggestions for improving this
*code, please contact Steve Verrill at <a
*href="mailto:steve@ws10.fpl.fs.fed.us">steve@ws10.fpl.fs.fed.us</a>.
*
*@author Steve Verrill
*@version .5 --- November 30, 1996
* 
*/

public class Cholesky extends Object {

/**
*
*<p>
*This method factors the n by n symmetric positive definite
*matrix A as RR&#180 where R is a lower triangular matrix.
*The method assumes that at least the lower triangle
*of A is filled on entry.  On exit, the lower triangle
*of A has been replaced by R.
*
*@param  a[&#32][&#32]  The positive definite matrix to factor.  The method assumes
*that at least the lower triangle of a[&#32][&#32] is filled on entry.  On exit
*the lower triangle of a[&#32][&#32] has been replaced by R such that RR&#180 = A.
*@param  n  The order of the matrix a[&#32][&#32].
*@throws NotPosDefException if the factorization cannot be completed.
*
*/

   public void factorPosDef (double a[][], int n) throws NotPosDefException {


  
      double sum,diff;

      int i,j,l;

      for (i = 0; i < n; i++) {

         sum = 0.0;

         for (j = 0; j < i; j++) {

            sum += a[i][j]*a[i][j];

         }

         diff = a[i][i] - sum;

         if (diff <= 0.0) {

            System.out.print("\nCholesky.factorPosDef error:" +
            "  A is not positive definite.\n" +
            "The i value at which a non-positive a[i][i] - sum value\n" + 
            "was encountered is " + i + ".\n\n");

            throw new NotPosDefException();

         }

         a[i][i] = Math.sqrt(diff);

         for (l = i+1; l < n; l++) {

            sum = 0.0;

            for (j = 0; j < i; j++) {

               sum += a[l][j]*a[i][j];

            }

            a[l][i] = (a[l][i] - sum)/a[i][i];

         }

      }   

      return;

   }             


/**
*
*<p>
*This method solves the equation
*<pre>
*
*      Ax = b
*
*</pre>
*where A is a known n by n symmetric positive definite matrix,
*and b is a known vector of length n.<br><br>
*
*<dl>
*
*   <dt>On entrance:
*
*      <dd>If factored == false, the lower triangle of a[&#32][&#32] should
*      contain the lower triangle of A.<br>
*
*      If factored == true, the lower triangle of a[&#32][&#32] should
*      contain a lower triangular matrix R such that RR&#180 = A.
*
*   <dt>On exit:
*
*      <dd>The elements of b have been replaced by the elements of x.
*
*</dl>
*
*<p>
*The method proceeds by first factoring A as RR&#180 where R is a lower
*triangular matrix.  Thus Ax = b is equivalent to R(R&#180x) = b.  Then the
*method performs two additional operations.  First it solves Ry = b for y.
*Then it solves R&#180x = y for x.  It stores x in b.
*
*@param  a[&#32][&#32]  On entrance, if the boolean variable factored is false,
*the lower triangle of a[&#32][&#32] should contain the lower triangle of A.
*If factored is true, the lower triangle of a[&#32][&#32] should
*contain a lower triangular matrix R such that RR&#180 = A.
*@param  b  On entrance b must contain the known b of Ax = b.  On exit
*it contains the solution x to Ax = b.
*@param  y  A work vector of order at least n.
*@param  n  The order of A and b.
*@param  factored  On entrance, factored should be set to true if A 
*already has been factored, false
*if A has not yet been factored.
*@throws NotPosDefException if A cannot be factored as RR&#180 for a
*full rank lower triangular matrix R.
*
*/

   public void solvePosDef (double a[][], double b[], double y[],
                     int n, boolean factored) 
                     throws NotPosDefException {

      Cholesky cholesky = new Cholesky();
      Triangular triangular = new Triangular();




      int i,j;


      if (factored == false) {

/*

   A has not yet been factored.  Factor it.
   The lower triangle of a[][] will contain R where 
   RR' = A.

*/

         try {

            cholesky.factorPosDef(a,n);

         } catch (NotPosDefException npd) {

            System.out.print("\nCholesky.solvePosDef error:" +
            "  The matrix was not positive definite\n" +
            "so it was not possible" +
            "to use the Cholesky decomposition\n" +
            " to solve Ax = b.\n\n");

            throw npd;

         }

      }

/*

   Solve Ry = b.

*/

      try {

         triangular.solveLower(a, y, b, n);

      } catch (NotFullRankException nfr) {

         System.out.print("\nTriangular.solveLower error:" +
         "  The lower triangular factor of A\n" +
         "was not of full rank.\n\n");

         throw new NotPosDefException();

      }

/*

   Fill the upper triangle of a[][] with R'
   where RR' = A.

*/

      for (i = 0; i < n; i++) {

         for (j = i; j < n; j++) {

            a[i][j] = a[j][i];

         }

      }


/*

   Solve R'x = y.  Put x in the b vector.

*/

      try {

         triangular.solveUpper(a, b, y, n);

      } catch (NotFullRankException nfr) {

         System.out.print("\nTriangular.solveUpper error:" +
         "  The upper triangular factor of A\n" +
         "was not of full rank.\n\n");

         throw new NotPosDefException();

      }

      return;

   }             


/**
*
*<p>
*This method obtains the inverse of an n by n
*symmetric positive definite matrix A.<br><br>
*<dl>
*   <dt>On entrance:
*
*      <dd>If factored == false, the lower triangle of a[&#32][&#32] should
*      contain the lower triangle of A.<br>
*
*      If factored == true, the lower triangle of a[&#32][&#32] should
*      contain a lower triangular matrix R such that RR&#180 = A.
*
*   <dt>On exit:
*
*      <dd>The lower triangle of a[&#32][&#32] has been replaced by the
*      lower triangle of the inverse of A.
*</dl>
*
*@param  a[&#32][&#32]  On entrance, if the boolean variable factored is false,
*the lower triangle of a[&#32][&#32] should contain the lower triangle of A.
*If factored is true, the lower triangle of a[&#32][&#32] should
*contain a lower triangular matrix R such that RR&#180 = A.
*@param  n  The order of A.
*@param  factored  On entrance, factored should be set to true if A 
*already has been factored, false
*if A has not yet been factored.
*@throws NotPosDefException if A cannot be factored as RR&#180 for a
*full rank lower triangular matrix R.
*
*/


   public void invertPosDef (double a[][], int n, boolean factored) 
                      throws NotPosDefException {

      Cholesky cholesky = new Cholesky();
      Triangular triangular = new Triangular();





      double sum;

      int i,j,k;



      if (factored == false) {

/*

   A has not yet been factored.  Factor it.
   The lower triangle of a[][] will contain R where 
   RR' = A.

*/

         try {

            cholesky.factorPosDef(a,n);

         } catch (NotPosDefException npd) {

            System.out.print("\nCholesky.invertPosDef error:" +
            "  The matrix was not positive definite\n" +
            "so it was not possible" +
            "to use the Cholesky decomposition\n" +
            " to obtain an inverse.\n\n");

            throw npd;

         }

      }

/*

   Invert the lower triangle of a[][].
   The lower triangle will contain 
   the inverse of R
   where RR' = A.

*/


      try {

         triangular.invertLower(a,n);

      } catch (NotFullRankException nfr) {

         System.out.print("\nCholesky.invertPosDef error:" +
         "  The matrix was not positive definite\n" +
         "so it was not possible" +
         "to use the Cholesky decomposition\n" +
         " to obtain an inverse.\n\n");

         throw new NotPosDefException();

      }

/*

   The inverse of A is (R^{-1})'(R^{-1}).
   Fill the lower triangle of a[][] with
   the lower triangle of (R^{-1})'(R^{-1}).

*/

      for (j = 0; j < n; j++) {

         for (i = j; i < n; i++) {

            sum = 0.0;

            for (k = i; k < n; k++) {

               sum += a[k][i]*a[k][j];

            }

            a[i][j] = sum;

         }

      }

      return;

   }

}
          


/**
*
*<p>
*This method solves the equation
*<pre>
*
*      Ax = b
*
*</pre>
*where A is a known n by n symmetric positive definite matrix,
*and Cholesky_f77.java                                                                                   0100644 0007456 0000012 00000033336 06347630337 0014536 0                                                                                                    ustar 00steve                           staff                           0000040 0000007                                                                                                                                                                        /*
    Cholesky_f77.java copyright claim:

    This software is based on public domain LINPACK routines.
    It was translated from FORTRAN to Java by a US government employee 
    on official time.  Thus this software is also in the public domain.


    The translator's mail address is:

    Steve Verrill 
    USDA Forest Products Laboratory
    1 Gifford Pinchot Drive
    Madison, Wisconsin
    53705


    The translator's e-mail address is:

    steve@ws10.fpl.fs.fed.us


***********************************************************************

DISCLAIMER OF WARRANTIES:

THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND. 
THE TRANSLATOR DOES NOT WARRANT, GUARANTEE OR MAKE ANY REPRESENTATIONS 
REGARDING THE SOFTWARE OR DOCUMENTATION IN TERMS OF THEIR CORRECTNESS, 
RELIABILITY, CURRENTNESS, OR OTHERWISE. THE ENTIRE RISK AS TO 
THE RESULTS AND PERFORMANCE OF THE SOFTWARE IS ASSUMED BY YOU. 
IN NO CASE WILL ANY PARTY INVOLVED WITH THE CREATION OR DISTRIBUTION 
OF THE SOFTWARE BE LIABLE FOR ANY DAMAGE THAT MAY RESULT FROM THE USE 
OF THIS SOFTWARE.

Sorry about that.

***********************************************************************


History:

Date        Translator        Changes

3/2/97      Steve Verrill     Translated

*/


package linear_algebra;

import linear_algebra.*;


/**
*
*<p>
*This class contains the LINPACK DPOFA (Cholesky decomposition),
*DPOSL (solve), and DPODI (determinant and inverse) routines.
*
*<p>
*<b>IMPORTANT:</b>  The "_f77" suffixes indicate that these routines use
*FORTRAN style indexing.  For example, you will see
*<pre>
*   for (i = 1; i <= n; i++)
*</pre>
*rather than
*<pre>
*   for (i = 0; i < n; i++)
*</pre>
*To use the "_f77" routines you will have to declare your vectors
*and matrices to be one element larger (e.g., v[101] rather than
*v[100], and a[101][101] rather than a[100][100]), and you will have
*to fill elements 1 through n rather than elements 0 through n - 1.
*Versions of these programs that use C/Java style indexing will
*soon be available.  They will end with the suffix "_j".
*
*<p>
*This class was translated by a statistician from FORTRAN versions of 
*the LINPACK routines.  It is NOT an official translation.  It wastes
*memory by failing to use the first elements of vectors.  When 
*public domain Java numerical analysis routines become available 
*from the people who produce LAPACK, then <b>THE CODE PRODUCED
*BY THE NUMERICAL ANALYSTS SHOULD BE USED</b>.
*
*<p>
*Meanwhile, if you have suggestions for improving this
*code, please contact Steve Verrill at steve@ws10.fpl.fs.fed.us.
*
*@author Steve Verrill
*@version .5 --- March 2, 1997
* 
*/

public class Cholesky_f77 extends Object {

/**
*
*<p>
*This method decomposes an p by p symmetric, positive definite 
*matrix X into a product, R&#180R, where R is an upper triangular matrix
*and R&#180 is the transpose of R.
*For details, see the comments in the code.
*This method is a translation from FORTRAN to Java of the LINPACK subroutine
*DPOFA.  In the LINPACK listing DPOFA is attributed to Cleve Moler
*with a date of 8/14/78.
*
*Translated by Steve Verrill, March 2, 1997.
*
*@param  a     The matrix to be decomposed
*@param  n     The order of a
*
*/



   public static void dpofa_f77 (double a[][], int n) throws
                                                  NotPosDefException {

/*

Here is a copy of the LINPACK documentation (from the SLATEC version):

c***begin prologue  dpofa
c***date written   780814   (yymmdd)
c***revision date  861211   (yymmdd)
c***category no.  d2b1b
c***keywords  library=slatec(linpack),
c             type=double precision(spofa-s dpofa-d cpofa-c),
c             linear algebra,matrix,matrix factorization,
c             positive definite
c***author  moler, c. b., (u. of new mexico)
c***purpose  factors a double precision symmetric positive definite
c            matrix.
c***description
c
c     dpofa factors a double precision symmetric positive definite
c     matrix.
c
c     dpofa is usually called by dpoco, but it can be called
c     directly with a saving in time if  rcond  is not needed.
c     (time for dpoco) = (1 + 18/n)*(time for dpofa) .
c
c     on entry
c
c        a       double precision(lda, n)
c                the symmetric matrix to be factored.  only the
c                diagonal and upper triangle are used.
c
c        lda     integer
c                the leading dimension of the array  a .
c
c        n       integer
c                the order of the matrix  a .
c
c     on return
c
c        a       an upper triangular matrix  r  so that  a = trans(r)*r
c                where  trans(r)  is the transpose.
c                the strict lower triangle is unaltered.
c                if  info .ne. 0 , the factorization is not complete.
c
c        info    integer
c                = 0  for normal return.
c                = k  signals an error condition.  the leading minor
c                     of order  k  is not positive definite.


   For the Java version, info 
   is returned as an argument to NotPosDefException
   if the matrix is not positive definite.


c
c     linpack.  this version dated 08/14/78 .
c     cleve moler, university of new mexico, argonne national lab.
c
c     subroutines and functions
c
c     blas ddot
c     fortran dsqrt
c***references  dongarra j.j., bunch j.r., moler c.b., stewart g.w.,
c                 *linpack users  guide*, siam, 1979.
c***routines called  ddot
c***end prologue  dpofa

*/

      double s,t;
      int j,jm1,k,info;
      
      for (j = 1; j <= n; j++) {

         info = j;
         s = 0.0;
         jm1 = j - 1;

         for (k = 1; k <= jm1; k++) {

            t = a[k][j] - Blas_f77.coldot_f77(k-1,a,1,k,j);
            t = t/a[k][k];
            a[k][j] = t;
            s = s + t*t;

         }

         s = a[j][j] - s;

         if (s <= 0.0) {

/*

            System.out.print("\nCholesky_f77.dpofa error:" +
            "  A is not positive definite.\n" +
            "The j value at which a non-positive a[j][j] - sum value\n" + 
            "was encountered is " + j + ".\n\n");

*/

            throw new NotPosDefException(info);

         }

         a[j][j] = Math.sqrt(s);

      }

      return;

   }


/**
*
*<p>
*This method uses the Cholesky decomposition provided by
*DPOFA to solve the equation Ax = b where A is symmetric,
*positive definite.
*For details, see the comments in the code.
*This method is a translation from FORTRAN to Java of the LINPACK subroutine
*DPOSL.  In the LINPACK listing DPOSL is attributed to Cleve Moler
*with a date of 8/14/78.
*
*Translated by Steve Verrill, March 2, 1997.
*
*@param  a     a[][]
*@param  n     The order of a
*@param  b     The vector b in Ax = b
*
*/

   public static void dposl_f77 (double a[][], int n, double b[]) {

/*

Here is a copy of the LINPACK documentation (from the SLATEC version):
      
c***begin prologue  dposl
c***date written   780814   (yymmdd)
c***revision date  861211   (yymmdd)
c***category no.  d2b1b
c***keywords  library=slatec(linpack),
c             type=double precision(sposl-s dposl-d cposl-c),
c             linear algebra,matrix,positive definite,solve
c***author  moler, c. b., (u. of new mexico)
c***purpose  solves the double precision symmetric positive definite
c            system a*x=b using the factors computed by dpoco or dpofa.
c***description
c
c     dposl solves the double precision symmetric positive definite
c     system a * x = b
c     using the factors computed by dpoco or dpofa.
c
c     on entry
c
c        a       double precision(lda, n)
c                the output from dpoco or dpofa.
c
c        lda     integer
c                the leading dimension of the array  a .
c
c        n       integer
c                the order of the matrix  a .
c
c        b       double precision(n)
c                the right hand side vector.
c
c     on return
c
c        b       the solution vector  x .
c
c     error condition
c
c        a division by zero will occur if the input factor contains
c        a zero on the diagonal.  technically this indicates
c        singularity, but it is usually caused by improper subroutine
c        arguments.  it will not occur if the subroutines are called
c        correctly and  info .eq. 0 .
c
c     to compute  inverse(a) * c  where  c  is a matrix
c     with  p  columns
c           call dpoco(a,lda,n,rcond,z,info)
c           if (rcond is too small .or. info .ne. 0) go to ...
c           do 10 j = 1, p
c              call dposl(a,lda,n,c(1,j))
c        10 continue
c
c     linpack.  this version dated 08/14/78 .
c     cleve moler, university of new mexico, argonne national lab.
c
c     subroutines and functions
c
c     blas daxpy,ddot
c***references  dongarra j.j., bunch j.r., moler c.b., stewart g.w.,
c                 *linpack users  guide*, siam, 1979.
c***routines called  daxpy,ddot
c***end prologue  dposl

*/

      double t;
      int k,kb;

//   solve transpose(r)y = b

      for (k = 1; k <= n; k++) {

         t = Blas_f77.colvdot_f77(k-1,a,b,1,k);
         b[k] = (b[k] - t)/a[k][k];

      }

//   solve rx = y

      for (kb = 1; kb <= n; kb++) {

         k = n + 1 - kb;
         b[k] = b[k]/a[k][k];
         t = -b[k];
         Blas_f77.colvaxpy_f77(k-1,t,a,b,1,k);

      }

      return;

   }



/**
*
*<p>
*This method uses the Cholesky decomposition provided by
*DPOFA to obtain the determinant and/or inverse of a symmetric,
*positive definite matrix.
*For details, see the comments in the code.
*This method is a translation from FORTRAN to Java of the LINPACK subroutine
*DPODI.  In the LINPACK listing DPODI is attributed to Cleve Moler
*with a date of 8/14/78.
*
*Translated by Steve Verrill, March 3, 1997.
*
*@param  a     a[][]
*@param  n     The order of a
*@param  det   det[]
*@param  job   Indicates whether a determinant, inverse,
*              or both is desired
*
*/


   public static void dpodi_f77 (double a[][], int n, double det[], int job) {


/*

Here is a copy of the LINPACK documentation (from the SLATEC version):


C***BEGIN PROLOGUE  DPODI
C***DATE WRITTEN   780814   (YYMMDD)
C***REVISION DATE  861211   (YYMMDD)
C***CATEGORY NO.  D2B1B,D3B1B
C***KEYWORDS  LIBRARY=SLATEC(LINPACK),
C             TYPE=DOUBLE PRECISION(SPODI-S DPODI-D CPODI-C),
C             DETERMINANT,INVERSE,LINEAR ALGEBRA,MATRIX,
C             MATRIX FACTORIZATION,POSITIVE DEFINITE
C***AUTHOR  MOLER, C. B., (U. OF NEW MEXICO)
C***PURPOSE  Computes the determinant and inverse of a certain double
C            precision SYMMETRIC POSITIVE DEFINITE matrix (see abstract)
C            using the factors computed by DPOCO, DPOFA or DQRDC.
C***DESCRIPTION
C
C     DPODI computes the determinant and inverse of a certain
C     double precision symmetric positive definite matrix (see below)
C     using the factors computed by DPOCO, DPOFA or DQRDC.
C
C     On Entry
C
C        A       DOUBLE PRECISION(LDA, N)
C                the output  A  from DPOCO or DPOFA
C                or the output  X  from DQRDC.
C
C        LDA     INTEGER
C                the leading dimension of the array  A .
C
C        N       INTEGER
C                the order of the matrix  A .
C
C        JOB     INTEGER
C                = 11   both determinant and inverse.
C                = 01   inverse only.
C                = 10   determinant only.
C
C     On Return
C
C        A       If DPOCO or DPOFA was used to factor  A , then
C                DPODI produces the upper half of INVERSE(A) .
C                If DQRDC was used to decompose  X , then
C                DPODI produces the upper half of inverse(TRANS(X)*X)
C                where TRANS(X) is the transpose.
C                Elements of  A  below the diagonal are unchanged.
C                If the units digit of JOB is zero,  A  is unchanged.
C
C        DET     DOUBLE PRECISION(2)
C                determinant of  A  or of  TRANS(X)*X  if requested.
C                Otherwise not referenced.
C                Determinant = DET(1) * 10.0**DET(2)
C                with  1.0 .LE. DET(1) .LT. 10.0
C                or  DET(1) .EQ. 0.0 .
C
C     Error Condition
C
C        A division by zero will occur if the input factor contains
C        a zero on the diagonal and the inverse is requested.
C        It will not occur if the subroutines are called correctly
C        and if DPOCO or DPOFA has set INFO .EQ. 0 .
C
C     LINPACK.  This version dated 08/14/78 .
C     Cleve Moler, University of New Mexico, Argonne National Lab.
C
C     Subroutines and Functions
C
C     BLAS DAXPY,DSCAL
C     Fortran MOD
C***REFERENCES  DONGARRA J.J., BUNCH J.R., MOLER C.B., STEWART G.W.,
C                 *LINPACK USERS  GUIDE*, SIAM, 1979.
C***ROUTINES CALLED  DAXPY,DSCAL
C***END PROLOGUE  DPODI

*/

      double s,t;
      int i,j,jm1,k,kp1;

//   compute determinant

      if (job/10 != 0) {

         det[1] = 1.0;
         det[2] = 0.0;
         s = 10.0;

         for (i = 1; i <= n; i++) {

            det[1] *= a[i][i]*a[i][i];
            
            if (det[1] == 0.0) break;

            while (det[1] < 1.0) {

               det[1] *= s;
               det[2]--;

            }

            while (det[1] >= s) {

               det[1] /= s;
               det[2]++;

            }

         }

      }

//   compute inverse(R)

      if (job%10 != 0) {

         for (k = 1; k <= n; k++) {

            a[k][k] = 1.0/a[k][k];
            t = -a[k][k];
            Blas_f77.colscal_f77(k-1,t,a,1,k);
            kp1 = k + 1;

            for (j = kp1; j <= n; j++) {

               t = a[k][j];
               a[k][j] = 0.0;
               Blas_f77.colaxpy_f77(k,t,a,1,k,j);

            }

         }

//   form inverse(R) * transpose(inverse(R))

         for (j = 1; j <= n; j++) {

            jm1 = j - 1;

            for (k = 1; k <= jm1; k++) {

               t = a[k][j];
               Blas_f77.colaxpy_f77(k,t,a,1,j,k);

            }

            t = a[j][j];
            Blas_f77.colscal_f77(j,t,a,1,j);

         }   

      }

      return;

   }   
                                    
}
                                                                                                                                                                                                                                                                                                  Cholesky_j.java                                                                                     0100644 0007456 0000012 00000033023 06347630337 0014355 0                                                                                                    ustar 00steve                           staff                           0000040 0000007                                                                                                                                                                        /*
    Cholesky_j.java copyright claim:

    This software is based on public domain LINPACK routines.
    It was translated from FORTRAN to Java by a US government employee 
    on official time.  Thus this software is also in the public domain.


    The translator's mail address is:

    Steve Verrill 
    USDA Forest Products Laboratory
    1 Gifford Pinchot Drive
    Madison, Wisconsin
    53705


    The translator's e-mail address is:

    steve@ws10.fpl.fs.fed.us


***********************************************************************

DISCLAIMER OF WARRANTIES:

THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND. 
THE TRANSLATOR DOES NOT WARRANT, GUARANTEE OR MAKE ANY REPRESENTATIONS 
REGARDING THE SOFTWARE OR DOCUMENTATION IN TERMS OF THEIR CORRECTNESS, 
RELIABILITY, CURRENTNESS, OR OTHERWISE. THE ENTIRE RISK AS TO 
THE RESULTS AND PERFORMANCE OF THE SOFTWARE IS ASSUMED BY YOU. 
IN NO CASE WILL ANY PARTY INVOLVED WITH THE CREATION OR DISTRIBUTION 
OF THE SOFTWARE BE LIABLE FOR ANY DAMAGE THAT MAY RESULT FROM THE USE 
OF THIS SOFTWARE.

Sorry about that.

***********************************************************************


History:

Date        Translator        Changes

3/2/97      Steve Verrill     Translated
6/4/97                        Java style indexing

*/


package linear_algebra;

import linear_algebra.*;


/**
*
*<p>
*This class contains the LINPACK DPOFA (Cholesky decomposition),
*DPOSL (solve), and DPODI (determinant and inverse) routines.
*
*<p>
*<b>IMPORTANT:</b>  The "_j" suffixes indicate that these routines use
*Java/C style indexing.  For example, you will see
*<pre>
*   for (i = 0; i < n; i++)
*</pre>
*rather than
*<pre>
*   for (i = 1; i <= n; i++)
*</pre>
*To use the "_j" routines you will have
*to fill elements 0 through n - 1 rather than elements 1 through n.
*Versions of these programs that use FORTRAN style indexing are
*also available.  They end with the suffix "_f77".
*
*<p>
*This class was translated by a statistician from FORTRAN versions of 
*the LINPACK routines.  It is NOT an official translation.  When 
*public domain Java numerical analysis routines become available 
*from the people who produce LAPACK, then <b>THE CODE PRODUCED
*BY THE NUMERICAL ANALYSTS SHOULD BE USED</b>.
*
*<p>
*Meanwhile, if you have suggestions for improving this
*code, please contact Steve Verrill at steve@ws10.fpl.fs.fed.us.
*
*@author Steve Verrill
*@version .5 --- June 4, 1997
* 
*/

public class Cholesky_j extends Object {

/**
*
*<p>
*This method decomposes an p by p symmetric, positive definite 
*matrix X into a product, R&#180R, where R is an upper triangular matrix
*and R&#180 is the transpose of R.
*For details, see the comments in the code.
*This method is a translation from FORTRAN to Java of the LINPACK subroutine
*DPOFA.  In the LINPACK listing DPOFA is attributed to Cleve Moler
*with a date of 8/14/78.
*
*Translated by Steve Verrill, March 2, 1997.
*
*@param  a     The matrix to be decomposed
*@param  n     The order of a
*
*/



   public static void dpofa_j (double a[][], int n) throws
                                                  NotPosDefException {

/*

Here is a copy of the LINPACK documentation (from the SLATEC version):

c***begin prologue  dpofa
c***date written   780814   (yymmdd)
c***revision date  861211   (yymmdd)
c***category no.  d2b1b
c***keywords  library=slatec(linpack),
c             type=double precision(spofa-s dpofa-d cpofa-c),
c             linear algebra,matrix,matrix factorization,
c             positive definite
c***author  moler, c. b., (u. of new mexico)
c***purpose  factors a double precision symmetric positive definite
c            matrix.
c***description
c
c     dpofa factors a double precision symmetric positive definite
c     matrix.
c
c     dpofa is usually called by dpoco, but it can be called
c     directly with a saving in time if  rcond  is not needed.
c     (time for dpoco) = (1 + 18/n)*(time for dpofa) .
c
c     on entry
c
c        a       double precision(lda, n)
c                the symmetric matrix to be factored.  only the
c                diagonal and upper triangle are used.
c
c        lda     integer
c                the leading dimension of the array  a .
c
c        n       integer
c                the order of the matrix  a .
c
c     on return
c
c        a       an upper triangular matrix  r  so that  a = trans(r)*r
c                where  trans(r)  is the transpose.
c                the strict lower triangle is unaltered.
c                if  info .ne. 0 , the factorization is not complete.
c
c        info    integer
c                = 0  for normal return.
c                = k  signals an error condition.  the leading minor
c                     of order  k  is not positive definite.


   For the Java version, info 
   is returned as an argument to NotPosDefException
   if the matrix is not positive definite.


c
c     linpack.  this version dated 08/14/78 .
c     cleve moler, university of new mexico, argonne national lab.
c
c     subroutines and functions
c
c     blas ddot
c     fortran dsqrt
c***references  dongarra j.j., bunch j.r., moler c.b., stewart g.w.,
c                 *linpack users  guide*, siam, 1979.
c***routines called  ddot
c***end prologue  dpofa

*/

      double s,t;
      int j,k,info;
      
      for (j = 0; j < n; j++) {

         info = j + 1;
         s = 0.0;

         for (k = 0; k < j; k++) {

            t = a[k][j] - Blas_j.coldot_j(k,a,0,k,j);
            t = t/a[k][k];
            a[k][j] = t;
            s = s + t*t;

         }

         s = a[j][j] - s;

         if (s <= 0.0) {

/*

            System.out.print("\nCholesky_j.dpofa error:" +
            "  A is not positive definite.\n" +
            "The j value at which a non-positive a[j-1][j-1] - sum value\n" + 
            "was encountered is " + j + ".\n\n");

*/

            throw new NotPosDefException(info);

         }

         a[j][j] = Math.sqrt(s);

      }

      return;

   }


/**
*
*<p>
*This method uses the Cholesky decomposition provided by
*DPOFA to solve the equation Ax = b where A is symmetric,
*positive definite.
*For details, see the comments in the code.
*This method is a translation from FORTRAN to Java of the LINPACK subroutine
*DPOSL.  In the LINPACK listing DPOSL is attributed to Cleve Moler
*with a date of 8/14/78.
*
*Translated by Steve Verrill, March 2, 1997.
*
*@param  a     a[][]
*@param  n     The order of a
*@param  b     The vector b in Ax = b
*
*/

   public static void dposl_j (double a[][], int n, double b[]) {

/*

Here is a copy of the LINPACK documentation (from the SLATEC version):
      
c***begin prologue  dposl
c***date written   780814   (yymmdd)
c***revision date  861211   (yymmdd)
c***category no.  d2b1b
c***keywords  library=slatec(linpack),
c             type=double precision(sposl-s dposl-d cposl-c),
c             linear algebra,matrix,positive definite,solve
c***author  moler, c. b., (u. of new mexico)
c***purpose  solves the double precision symmetric positive definite
c            system a*x=b using the factors computed by dpoco or dpofa.
c***description
c
c     dposl solves the double precision symmetric positive definite
c     system a * x = b
c     using the factors computed by dpoco or dpofa.
c
c     on entry
c
c        a       double precision(lda, n)
c                the output from dpoco or dpofa.
c
c        lda     integer
c                the leading dimension of the array  a .
c
c        n       integer
c                the order of the matrix  a .
c
c        b       double precision(n)
c                the right hand side vector.
c
c     on return
c
c        b       the solution vector  x .
c
c     error condition
c
c        a division by zero will occur if the input factor contains
c        a zero on the diagonal.  technically this indicates
c        singularity, but it is usually caused by improper subroutine
c        arguments.  it will not occur if the subroutines are called
c        correctly and  info .eq. 0 .
c
c     to compute  inverse(a) * c  where  c  is a matrix
c     with  p  columns
c           call dpoco(a,lda,n,rcond,z,info)
c           if (rcond is too small .or. info .ne. 0) go to ...
c           do 10 j = 1, p
c              call dposl(a,lda,n,c(1,j))
c        10 continue
c
c     linpack.  this version dated 08/14/78 .
c     cleve moler, university of new mexico, argonne national lab.
c
c     subroutines and functions
c
c     blas daxpy,ddot
c***references  dongarra j.j., bunch j.r., moler c.b., stewart g.w.,
c                 *linpack users  guide*, siam, 1979.
c***routines called  daxpy,ddot
c***end prologue  dposl

*/

      double t;
      int k,kb;

//   solve transpose(r)y = b

      for (k = 0; k < n; k++) {

         t = Blas_j.colvdot_j(k,a,b,0,k);
         b[k] = (b[k] - t)/a[k][k];

      }

//   solve rx = y

      for (kb = 1; kb <= n; kb++) {

         k = n - kb;
         b[k] = b[k]/a[k][k];
         t = -b[k];
         Blas_j.colvaxpy_j(k,t,a,b,0,k);

      }

      return;

   }



/**
*
*<p>
*This method uses the Cholesky decomposition provided by
*DPOFA to obtain the determinant and/or inverse of a symmetric,
*positive definite matrix.
*For details, see the comments in the code.
*This method is a translation from FORTRAN to Java of the LINPACK subroutine
*DPODI.  In the LINPACK listing DPODI is attributed to Cleve Moler
*with a date of 8/14/78.
*
*Translated by Steve Verrill, March 3, 1997.
*
*@param  a     a[][]
*@param  n     The order of a
*@param  det   det[]
*@param  job   Indicates whether a determinant, inverse,
*              or both is desired
*
*/


   public static void dpodi_j (double a[][], int n, double det[], int job) {


/*

Here is a copy of the LINPACK documentation (from the SLATEC version):


C***BEGIN PROLOGUE  DPODI
C***DATE WRITTEN   780814   (YYMMDD)
C***REVISION DATE  861211   (YYMMDD)
C***CATEGORY NO.  D2B1B,D3B1B
C***KEYWORDS  LIBRARY=SLATEC(LINPACK),
C             TYPE=DOUBLE PRECISION(SPODI-S DPODI-D CPODI-C),
C             DETERMINANT,INVERSE,LINEAR ALGEBRA,MATRIX,
C             MATRIX FACTORIZATION,POSITIVE DEFINITE
C***AUTHOR  MOLER, C. B., (U. OF NEW MEXICO)
C***PURPOSE  Computes the determinant and inverse of a certain double
C            precision SYMMETRIC POSITIVE DEFINITE matrix (see abstract)
C            using the factors computed by DPOCO, DPOFA or DQRDC.
C***DESCRIPTION
C
C     DPODI computes the determinant and inverse of a certain
C     double precision symmetric positive definite matrix (see below)
C     using the factors computed by DPOCO, DPOFA or DQRDC.
C
C     On Entry
C
C        A       DOUBLE PRECISION(LDA, N)
C                the output  A  from DPOCO or DPOFA
C                or the output  X  from DQRDC.
C
C        LDA     INTEGER
C                the leading dimension of the array  A .
C
C        N       INTEGER
C                the order of the matrix  A .
C
C        JOB     INTEGER
C                = 11   both determinant and inverse.
C                = 01   inverse only.
C                = 10   determinant only.
C
C     On Return
C
C        A       If DPOCO or DPOFA was used to factor  A , then
C                DPODI produces the upper half of INVERSE(A) .
C                If DQRDC was used to decompose  X , then
C                DPODI produces the upper half of inverse(TRANS(X)*X)
C                where TRANS(X) is the transpose.
C                Elements of  A  below the diagonal are unchanged.
C                If the units digit of JOB is zero,  A  is unchanged.
C
C        DET     DOUBLE PRECISION(2)
C                determinant of  A  or of  TRANS(X)*X  if requested.
C                Otherwise not referenced.
C                Determinant = DET(1) * 10.0**DET(2)
C                with  1.0 .LE. DET(1) .LT. 10.0
C                or  DET(1) .EQ. 0.0 .
C
C     Error Condition
C
C        A division by zero will occur if the input factor contains
C        a zero on the diagonal and the inverse is requested.
C        It will not occur if the subroutines are called correctly
C        and if DPOCO or DPOFA has set INFO .EQ. 0 .
C
C     LINPACK.  This version dated 08/14/78 .
C     Cleve Moler, University of New Mexico, Argonne National Lab.
C
C     Subroutines and Functions
C
C     BLAS DAXPY,DSCAL
C     Fortran MOD
C***REFERENCES  DONGARRA J.J., BUNCH J.R., MOLER C.B., STEWART G.W.,
C                 *LINPACK USERS  GUIDE*, SIAM, 1979.
C***ROUTINES CALLED  DAXPY,DSCAL
C***END PROLOGUE  DPODI

*/

      double s,t;
      int i,j,jm1,jp1,k,kp1;

//   compute determinant

      if (job/10 != 0) {

         det[0] = 1.0;
         det[1] = 0.0;
         s = 10.0;

         for (i = 0; i < n; i++) {

            det[0] *= a[i][i]*a[i][i];
            
            if (det[0] == 0.0) break;

            while (det[0] < 1.0) {

               det[0] *= s;
               det[1]--;

            }

            while (det[0] >= s) {

               det[0] /= s;
               det[1]++;

            }

         }

      }

//   compute inverse(R)

      if (job%10 != 0) {

         for (k = 0; k < n; k++) {

            a[k][k] = 1.0/a[k][k];
            t = -a[k][k];
            Blas_j.colscal_j(k,t,a,0,k);
            kp1 = k + 1;

            for (j = kp1; j < n; j++) {

               t = a[k][j];
               a[k][j] = 0.0;
               Blas_j.colaxpy_j(kp1,t,a,0,k,j);

            }

         }

//   form inverse(R) * transpose(inverse(R))

         for (j = 0; j < n; j++) {

            jm1 = j - 1;

            for (k = 0; k <= jm1; k++) {

               kp1 = k + 1;
               t = a[k][j];
               Blas_j.colaxpy_j(kp1,t,a,0,j,k);

            }

            jp1 = j + 1;

            t = a[j][j];
            Blas_j.colscal_j(jp1,t,a,0,j);

         }   

      }

      return;

   }   
                                    
}
purpose  factors a double precision symmetric positive definite
c            matrix.
c***description
c
c     dpofa factors a double precision symmetric positive definite
c     matrix.
c
c     dpofa is usually called by dpoco, but it can be called
c     directly with a saving in time if  rcond  is not needed.
c     (time for dpoco) = (1 + 18/n)*(time for dpofa) .
c
c     on entry
c
c        a       double precision(lda, n)
c                the symmetric matrix to be factored.  only the
c  Console.java                                                                                        0100644 0007456 0000012 00000007612 06251367234 0013667 0                                                                                                    ustar 00steve                           staff                           0000040 0000007                                                                                                                                                                        /*
 * Gary Cornell and Cay S. Horstmann, Core Java (Book/CD-ROM)
 * Published By SunSoft Press/Prentice-Hall
 * Copyright (C) 1996 Sun Microsystems Inc.
 * All Rights Reserved. ISBN 0-13-565755-5
 *
 * Permission to use, copy, modify, and distribute this 
 * software and its documentation for NON-COMMERCIAL purposes
 * and without fee is hereby granted provided that this 
 * copyright notice appears in all copies. 
 * 
 * THE AUTHORS AND PUBLISHER MAKE NO REPRESENTATIONS OR 
 * WARRANTIES ABOUT THE SUITABILITY OF THE SOFTWARE, EITHER 
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE 
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A 
 * PARTICULAR PURPOSE, OR NON-INFRINGEMENT. THE AUTHORS
 * AND PUBLISHER SHALL NOT BE LIABLE FOR ANY DAMAGES SUFFERED 
 * BY LICENSEE AS A RESULT OF USING, MODIFYING OR DISTRIBUTING 
 * THIS SOFTWARE OR ITS DERIVATIVES.
 */
 
 /**
 * An easy interface to read numbers and strings from 
 * standard input
 * @version 1.01 15 Feb 1996 
 * @author Cay Horstmann
 */

package corejava;

public class Console
{  /**
    * print a prompt on the console but don't print a newline
    * @param prompt the prompt string to display
    */

   public static void printPrompt(String prompt)
   {  System.out.print(prompt + " ");
      System.out.flush();
   }
   
   /**
    * read a string from the console. The string is 
    * terminated by a newline
    * @return the input string (without the newline)
    */
    
   public static String readString()
   {  int ch;
      String r = "";
      boolean done = false;
      while (!done)
      {  try
         {  ch = System.in.read();
            if (ch < 0 || (char)ch == '\n')
               done = true;
            else
               r = r + (char) ch;
         }
         catch(java.io.IOException e)
         {  done = true;
         }
      }
      return r;
   }

   /**
    * read a string from the console. The string is 
    * terminated by a newline
    * @param prompt the prompt string to display
    * @return the input string (without the newline)
    */
    
   public static String readString(String prompt)
   {  printPrompt(prompt);
      return readString();
   }

   /**
    * read a word from the console. The word is 
    * any set of characters terminated by whitespace
    * @return the 'word' entered
    */
    
   public static String readWord()
   {  int ch;
      String r = "";
      boolean done = false;
      while (!done)
      {  try
         {  ch = System.in.read();
            if (ch < 0 
               || java.lang.Character.isSpace((char)ch))
               done = true;
            else
               r = r + (char) ch;
         }
         catch(java.io.IOException e)
         {  done = true;
         }
      }
      return r;
   }

   /**
    * read an integer from the console. The input is 
    * terminated by a newline
    * @param prompt the prompt string to display
    * @return the input value as an int
    * @exception NumberFormatException if bad input
    */
    
   public static int readInt(String prompt)
   {  while(true)
      {  printPrompt(prompt);
         try
         {  return Integer.valueOf
               (readString().trim()).intValue();
         } catch(NumberFormatException e)
         {  System.out.println
               ("Not an integer. Please try again!");
         }
      }
   }

   /**
    * read a floating point number from the console. 
    * The input is terminated by a newline
    * @param prompt the prompt string to display
    * @return the input value as a double
    * @exception NumberFormatException if bad input
    */
    
   public static double readDouble(String prompt)
   {  while(true)
      {  printPrompt(prompt);
         try
         {  return Double.valueOf
               (readString().trim()).doubleValue();
         } catch(NumberFormatException e)
         {  System.out.println
         ("Not a floating point number. Please try again!");
         }
      }
   }
}
                                                                                                                      NotFullRankException.java                                                                           0100644 0007456 0000012 00000000645 06365735027 0016347 0                                                                                                    ustar 00steve                           staff                           0000040 0000007                                                                                                                                                                        
package linear_algebra;

/**
*
*This is the exception produced by a Triangular method
*if a matrix has at least one zero diagonal element.
*
*/

public class NotFullRankException extends Exception {

   public int info;


   public NotFullRankException(int info) {

      this.info = info;

   }

   public NotFullRankException(String problem) {

      super(problem);

   }

   public NotFullRankException() {

   }

}
                                                                                           NotPosDefException.java                                                                             0100644 0007456 0000012 00000000674 06365735055 0016014 0                                                                                                    ustar 00steve                           staff                           0000040 0000007                                                                                                                                                                        
package linear_algebra;

/**
*
*This is the exception produced by the Cholesky factorization
*if it determines that the matrix to be factored is not
*positive definite.
*
*/

public class NotPosDefException extends Exception {

   public int info;


   public NotPosDefException(int info) {

      this.info = info;

   }

   public NotPosDefException(String problem) {

      super(problem);

   }

   public NotPosDefException() {

   }

}

                                                                    QRTest_f77.java                                                                                     0100644 0007456 0000012 00000015401 06312344524 0014120 0                                                                                                    ustar 00steve                           staff                           0000040 0000007                                                                                                                                                                        
package linear_algebra;

import java.util.*;
import java.lang.*;
import java.io.*;
import linear_algebra.*;
import corejava.Console;

/**
*
*This class tests the (LINPACK) QR classes.
*
*It
*<ol>
*<li> Randomly fills an n by n matrix, X.
*<li> Randomly generates a vector b0.
*<li> Calculates the vector y = X(b0).
*<li> Performs a QR decomposition of X.
*<li> Solves the system Xb = y in an effort to recover b0. 
*<li> Handles the regression equation y = a0 + a1*x + a2*x^2 + e
*     where a0 = a1 = a2 = 1.  Its estimates of the parameters
*     should be close to 1.  Its estimate of the standard deviation
*     should be close to the input value.
*</ol>
*
*@author Steve Verrill
*@version .5 --- March 6, 1997
*
*/


public class QRTest_f77 extends Object {


   public static void main (String args[]) {


/*

Some of the variables of main:


*/ 

      long randstart;

      int n;

      int i,j,k;

      int info = 0;

      double randlow,randhigh,randdiff;

      double x[][] = new double[101][101];

      double y[] = new double[101];
      double b0[] = new double[101];
      double b[] = new double[101];

      double qraux[] = new double[101];
      double work[] = new double[101];
      double qy[] = new double[101];
      double qty[] = new double[101];
      double rsd[] = new double[101];
      double xb[] = new double[101];

      double sd,rss,diff;
      
      int jpvt[] = new int[101];
      int job;

      PrintStream pryx;


/*

   Console is a public domain class described in Cornell
   and Horstmann's Core Java (SunSoft Press, Prentice-Hall).

*/

      n = Console.readInt("\nWhat is the n value? (100 or less) ");

   
      randstart = Console.readInt("\nWhat is the starting value (an " +
      "integer)\n" +
      "for the random number generator? ");

      Random rand01 = new Random(randstart);

      randlow = Console.readDouble("\nWhat is randlow? ");

      randhigh = Console.readDouble("\nWhat is randhigh? ");

      randdiff = randhigh - randlow;

      sd = Console.readDouble("\nWhat is the standard deviation? ");

/*

   Generate an n by n X

*/

      for (i = 1; i <= n; i++) {

         for (j = 1; j <= n; j++) {

            x[i][j] = randlow + rand01.nextDouble()*randdiff;

         }

      }


/*
      System.out.print("\n\n");
      System.out.print("*********************************************\n");
      System.out.print("\nThe X matrix is \n\n");

      for (i = 1; i <= n; i++) {

         for (j = 1; j <= n; j++) {

            System.out.print(x[i][j] + "  ");

         }

         System.out.print("\n");

      }
*/



/*

   Generate a vector b

*/

      for (i = 1; i <= n; i++) {

         b0[i] = randlow + rand01.nextDouble()*randdiff;

      }


/*

   Calculate Xb = y

*/

      for (i = 1; i <= n; i++) {

         y[i] = 0.0;

         for (j = 1; j <= n; j++) {

            y[i] += x[i][j]*b0[j];

         }

      }


      job = 0;

      QR_f77.dqrdc_f77(x,n,n,qraux,jpvt,work,job);

/*   

   This should obtain the solution to Xb = y.

*/

      job = 100;
      info = QR_f77.dqrsl_f77(x,n,n,qraux,y,qy,qty,b,rsd,xb,job);

      System.out.print("\nThe info value returned by QR_f77.dqrsl_f77 was: " +
      info + "\n\n");


      System.out.print("\nThe input b vector was:\n\n");

      for (i = 1; i <= n; i++) {

         System.out.print(b0[i] + "\n");

      }

      System.out.print("\n\nThe recovered b vector was:\n\n");

      for (i = 1; i <= n; i++) {

         System.out.print(b[i] + "\n");

      }

      System.out.print("\n");
      System.out.print("\n");
      System.out.print("*********************************************\n");
      System.out.print("\n");


/*

   Generate a new X matrix and the y vector.

   Also, generate a data file that can be used with a different
   regression program to check whether the correct
   regression results are being obtained.

*/

//      try {

//         FileOutputStream fout = new FileOutputStream("yx.data");
//         pryx = new PrintStream(fout);

         for (i = 1; i <= n; i++) {

            x[i][1] = 1.0;
            x[i][2] = -1.0 + rand01.nextDouble()*2.0;
            x[i][3] = x[i][2]*x[i][2];

            y[i] = x[i][1] + x[i][2] + x[i][3] + sd*normi(rand01.nextDouble());

//            pryx.print(y[i] + "  " + x[i][1] + "  ");
//            pryx.print(x[i][2] + "  " + x[i][3] + "\n");

         }

//      }

//      catch (Exception e) {

//         System.out.println("\nError:  Problem with the yx.data file\n");
//         System.exit(0);

//      }

      job = 0;
      QR_f77.dqrdc_f77(x,n,3,qraux,jpvt,work,job);

/*   

   This should obtain the solution to Xb = y.

*/

      job = 111;
      info = QR_f77.dqrsl_f77(x,n,3,qraux,y,qy,qty,b,rsd,xb,job);

      System.out.print("\nThe info value returned by QR_f77.dqrsl_f77 was: " +
      info + "\n\n");

      System.out.print("\nThe input b vector was: \n\n1\n1\n1\n\n");

      System.out.print("\n\nThe recovered b vector was:\n\n");

      for (i = 1; i <= 3; i++) {

         System.out.print(b[i] + "\n");

      }

      rss = 0.0;

      for (i = 1; i <= n; i++) {

         rss += rsd[i]*rsd[i];

      }

      sd = Math.sqrt(rss/(n - 1.0));

      System.out.print("\nThe estimate of sigma from the rsd vector");
      System.out.print(" was " + sd + "\n");

      rss = 0.0;

      for (i = 1; i <= n; i++) {

         diff = y[i] - xb[i];
         rss += diff*diff;

      }

      sd = Math.sqrt(rss/(n - 1.0));

      System.out.print("\nThe estimate of sigma from the xb vector");
      System.out.print(" was " + sd + "\n");


      System.out.print("\n");
      System.out.print("\n");
      System.out.print("*********************************************\n");
      System.out.print("\n");



   }



/**
*
*<p>
*This is a normal cdf inverse routine.
*
*Created by Steve Verrill, March 1997.
*
*@param  u   The value (between 0 and 1) to invert.
*
*/

   public static double normi (double u) {

//   This is a temporary and potentially ugly way to generate
//   normal random numbers from uniform random numbers

      double c[] = new double[4];
      double d[] = new double[4];

      double normi,arg,t,t2,t3,xnum,xden;

      c[1] = 2.515517;
      c[2] = .802853;
      c[3] = .010328;

      d[1] = 1.432788;
      d[2] = .189269;
      d[3] = .001308;

      if (u <= .5) {

         arg = -2.0*Math.log(u);
         t = Math.sqrt(arg);
         t2 = t*t;
         t3 = t2*t;

         xnum = c[1] + c[2]*t + c[3]*t2;
         xden = 1.0 + d[1]*t + d[2]*t2 + d[3]*t3;

         normi = -(t - xnum/xden);

         return normi;

      } else {

         arg = -2.0*Math.log(1.0 - u);
  
         t = Math.sqrt(arg);
         t2 = t*t;
         t3 = t2*t;

         xnum = c[1] + c[2]*t + c[3]*t2;
         xden = 1.0 + d[1]*t + d[2]*t2 + d[3]*t3;

         normi = t - xnum/xden;

         return normi;

      }

   }

}



                                                                                                                                                                                                                                                               QRTest_j.java                                                                                       0100644 0007456 0000012 00000022042 06347360013 0013744 0                                                                                                    ustar 00steve                           staff                           0000040 0000007                                                                                                                                                                        
package linear_algebra;

import java.util.*;
import java.lang.*;
import java.io.*;
import linear_algebra.*;
import corejava.Console;

/**
*
*This class tests the (LINPACK) QR classes.
*
*It
*<ol>
*<li> Randomly fills an n by n matrix, X.
*<li> Randomly generates a vector b0.
*<li> Calculates the vector y = X(b0).
*<li> Performs a QR decomposition of X.
*<li> Solves the system Xb = y in an effort to recover b0. 
*<li> Handles the regression equation y = a0 + a1*x + a2*x^2 + e
*     where a0 = a1 = a2 = 1.  Its estimates of the parameters
*     should be close to 1.  Its estimate of the standard deviation
*     should be close to the input value.
*</ol>
*
*@author Steve Verrill
*@version .5 --- June 6, 1997
*
*/


public class QRTest_j extends Object {


   public static void main (String args[]) {


/*

Some of the variables of main:


*/ 

      long randstart;

      int n;

      int i,j,k;

      int info = 0;

      double randlow,randhigh,randdiff;

      double x[][] = new double[100][100];

      double y[] = new double[100];
      double b0[] = new double[100];
      double b[] = new double[100];

      double qraux[] = new double[100];
      double work[] = new double[100];
      double qy[] = new double[100];
      double qty[] = new double[100];
      double rsd[] = new double[100];
      double xb[] = new double[100];

      double sd,rss,diff;
      
      int jpvt[] = new int[100];
      int jpvts[] = new int[100];
      int origord[] = new int[100];

      int jobjpvt,job;

      PrintStream pryx;


/*

   Console is a public domain class described in Cornell
   and Horstmann's Core Java (SunSoft Press, Prentice-Hall).

*/

      n = Console.readInt("\nWhat is the n value? (100 or less) ");

   
      randstart = Console.readInt("\nWhat is the starting value (an " +
      "integer)\n" +
      "for the random number generator? ");

      Random rand01 = new Random(randstart);

      randlow = Console.readDouble("\nWhat is randlow? ");

      randhigh = Console.readDouble("\nWhat is randhigh? ");

      randdiff = randhigh - randlow;

      sd = Console.readDouble("\nWhat is the standard deviation? ");

      jobjpvt = Console.readInt("\nWhat is jobjpvt? (0 for no pivoting)");

/*

   Generate an n by n X

*/

      for (i = 0; i < n; i++) {

         for (j = 0; j < n; j++) {

            x[i][j] = randlow + rand01.nextDouble()*randdiff;

         }

      }


/*
      System.out.print("\n\n");
      System.out.print("*********************************************\n");
      System.out.print("\nThe X matrix is \n\n");

      for (i = 0; i < n; i++) {

         for (j = 0; j < n; j++) {

            System.out.print(x[i][j] + "  ");

         }

         System.out.print("\n");

      }
*/



/*

   Generate a vector b

*/

      for (i = 0; i < n; i++) {

         b0[i] = randlow + rand01.nextDouble()*randdiff;

      }


/*

   Calculate Xb = y

*/

      for (i = 0; i < n; i++) {

         y[i] = 0.0;

         for (j = 0; j < n; j++) {

            y[i] += x[i][j]*b0[j];

         }

      }


      job = jobjpvt;

      QR_j.dqrdc_j(x,n,n,qraux,jpvt,work,job);

/*   

   This should obtain the solution to Xb = y.

*/

      job = 100;
      info = QR_j.dqrsl_j(x,n,n,qraux,y,qy,qty,b,rsd,xb,job);

      System.out.print("\nThe info value returned by QR_j.dqrsl_j was: " +
      info + "\n\n");


      System.out.print("\nThe input b vector was:\n\n");

      for (i = 0; i < n; i++) {

         System.out.print(b0[i] + "\n");

      }

      System.out.print("\n\nThe recovered b vector was:\n\n");

      if (jobjpvt != 0) {

         isort(jpvt,jpvts,origord,n);

         for (i = 0; i < n; i++) {

            System.out.print(b[origord[i]] + "\n");

         }

      }

      else {

         for (i = 0; i < n; i++) {

            System.out.print(b[i] + "\n");

         }

      }

      System.out.print("\n");
      System.out.print("\n");
      System.out.print("*********************************************\n");
      System.out.print("\n");


/*

   Generate a new X matrix and the y vector.

   Also, generate a data file that can be used with a different
   regression program to check whether the correct
   regression results are being obtained.

*/

//      try {

//         FileOutputStream fout = new FileOutputStream("yx.data");
//         pryx = new PrintStream(fout);

         for (i = 0; i < n; i++) {

            x[i][0] = 1.0;
            x[i][1] = -1.0 + rand01.nextDouble()*2.0;
            x[i][2] = x[i][1]*x[i][1];

            y[i] = x[i][0] + x[i][1] + x[i][2] + sd*normi(rand01.nextDouble());

//            pryx.print(y[i] + "  " + x[i][1] + "  ");
//            pryx.print(x[i][2] + "  " + x[i][3] + "\n");

         }

//      }

//      catch (Exception e) {

//         System.out.println("\nError:  Problem with the yx.data file\n");
//         System.exit(0);

//      }

      job = jobjpvt;
      QR_j.dqrdc_j(x,n,3,qraux,jpvt,work,job);

/*   

   This should obtain the solution to Xb = y.

*/

      job = 111;
      info = QR_j.dqrsl_j(x,n,3,qraux,y,qy,qty,b,rsd,xb,job);

      System.out.print("\nThe info value returned by QR_j.dqrsl_j was: " +
      info + "\n\n");

      System.out.print("\nThe input b vector was: \n\n1\n1\n1\n\n");

      System.out.print("\n\nThe recovered b vector was:\n\n");

      
      if (jobjpvt != 0) {

         isort(jpvt,jpvts,origord,3);

         for (i = 0; i < 3; i++) {

            System.out.print(b[origord[i]] + "\n");

         }

      }

      else {

         for (i = 0; i < 3; i++) {

            System.out.print(b[i] + "\n");

         }

      }

      rss = 0.0;

      for (i = 0; i < n; i++) {

         rss += rsd[i]*rsd[i];

      }

      sd = Math.sqrt(rss/(n - 1.0));

      System.out.print("\nThe estimate of sigma from the rsd vector");
      System.out.print(" was " + sd + "\n");

      rss = 0.0;

      for (i = 0; i < n; i++) {

         diff = y[i] - xb[i];
         rss += diff*diff;

      }

      sd = Math.sqrt(rss/(n - 1.0));

      System.out.print("\nThe estimate of sigma from the xb vector");
      System.out.print(" was " + sd + "\n");


      System.out.print("\n");
      System.out.print("\n");
      System.out.print("*********************************************\n");
      System.out.print("\n");



   }



/**
*
*<p>
*This is a normal cdf inverse routine.
*
*Created by Steve Verrill, March 1997.
*
*@param  u   The value (between 0 and 1) to invert.
*
*/

   public static double normi (double u) {

//   This is a temporary and potentially ugly way to generate
//   normal random numbers from uniform random numbers

      double c[] = new double[4];
      double d[] = new double[4];

      double normi,arg,t,t2,t3,xnum,xden;

      c[1] = 2.515517;
      c[2] = .802853;
      c[3] = .010328;

      d[1] = 1.432788;
      d[2] = .189269;
      d[3] = .001308;

      if (u <= .5) {

         arg = -2.0*Math.log(u);
         t = Math.sqrt(arg);
         t2 = t*t;
         t3 = t2*t;

         xnum = c[1] + c[2]*t + c[3]*t2;
         xden = 1.0 + d[1]*t + d[2]*t2 + d[3]*t3;

         normi = -(t - xnum/xden);

         return normi;

      } else {

         arg = -2.0*Math.log(1.0 - u);
  
         t = Math.sqrt(arg);
         t2 = t*t;
         t3 = t2*t;

         xnum = c[1] + c[2]*t + c[3]*t2;
         xden = 1.0 + d[1]*t + d[2]*t2 + d[3]*t3;

         normi = t - xnum/xden;

         return normi;

      }

   }


/*
   This routine attempts to perform an n*log(n) sort
   rather than an n*n sort.
   It sorts doubles.
   It also keeps track of the original order of the x's.
  

   Steve Verrill, 5/2/89
   Translated to java from fortran on 6/7/96

   There are other, faster sorts that are available in java.
   See, for example, 

   http://www.cs.ubc.ca/spider/harrison/Java/sorting-demo.html

*/


   public static void isort(int x[], int xsort[], int origord[], int n) {

      int num = 1;
      int i,j,il,iu,numb,numbd2,itest,k,km1,ilp1;

      for (i = 0; i < n; i++) {

         origord[i] = i;

      }

      if (x[0] < x[1]) {

         xsort[0] = x[0];
         origord[0] = 0;
         xsort[1] = x[1];
         origord[1] = 1;

      }

      else {

         xsort[0] = x[1];
         origord[0] = 1;
         xsort[1] = x[0];
         origord[1] = 0;

      }
      
      for (j = 2; j < n; j++) {

//   insert the next value

         if (x[j] >= xsort[num]) {

            num++;
            xsort[num] = x[j];
            origord[num] = j;

//  The continue statement is described on page 90 of TYJ

            continue;

         }

         il = -1;

         if (x[j] > xsort[0]) {

            il = 0;
            iu = num;

            numb = iu - il;

            while (numb > 1) {

               numbd2 = numb/2;

               itest = il + numbd2;

               if (x[j] > xsort[itest]) il = itest;
               else iu = itest;

               numb = iu - il;

            }

         }

         num++;

         for (k = num; k > il + 1; k--) {

            km1 = k - 1;
            xsort[k] = xsort[km1];
            origord[k] = origord[km1];

         }

         ilp1 = il + 1;
         xsort[ilp1] = x[j];
         origord[ilp1] = j;

      } 

   }

}




                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              QR_f77.java                                                                                         0100644 0007456 0000012 00000053520 06347630337 0013274 0                                                                                                    ustar 00steve                           staff                           0000040 0000007                                                                                                                                                                        /*
    QR_f77.java copyright claim:

    This software is based on public domain LINPACK routines.
    It was translated from FORTRAN to Java by a US government employee 
    on official time.  Thus this software is also in the public domain.


    The translator's mail address is:

    Steve Verrill 
    USDA Forest Products Laboratory
    1 Gifford Pinchot Drive
    Madison, Wisconsin
    53705


    The translator's e-mail address is:

    steve@ws10.fpl.fs.fed.us


***********************************************************************

DISCLAIMER OF WARRANTIES:

THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND. 
THE TRANSLATOR DOES NOT WARRANT, GUARANTEE OR MAKE ANY REPRESENTATIONS 
REGARDING THE SOFTWARE OR DOCUMENTATION IN TERMS OF THEIR CORRECTNESS, 
RELIABILITY, CURRENTNESS, OR OTHERWISE. THE ENTIRE RISK AS TO 
THE RESULTS AND PERFORMANCE OF THE SOFTWARE IS ASSUMED BY YOU. 
IN NO CASE WILL ANY PARTY INVOLVED WITH THE CREATION OR DISTRIBUTION 
OF THE SOFTWARE BE LIABLE FOR ANY DAMAGE THAT MAY RESULT FROM THE USE 
OF THIS SOFTWARE.

Sorry about that.

***********************************************************************


History:

Date        Translator        Changes

2/25/97     Steve Verrill     Translated

*/


package linear_algebra;

import linear_algebra.*;


/**
*
*<p>
*This class contains the LINPACK DQRDC (QR decomposition)
*and DQRSL (QR solve) routines.
*
*<p>
*<b>IMPORTANT:</b>  The "_f77" suffixes indicate that these routines use
*FORTRAN style indexing.  For example, you will see
*<pre>
*   for (i = 1; i <= n; i++)
*</pre>
*rather than
*<pre>
*   for (i = 0; i < n; i++)
*</pre>
*To use the "_f77" routines you will have to declare your vectors
*and matrices to be one element larger (e.g., v[101] rather than
*v[100], and a[101][101] rather than a[100][100]), and you will have
*to fill elements 1 through n rather than elements 0 through n - 1.
*Versions of these programs that use C/Java style indexing will
*soon be available.  They will end with the suffix "_j".
*
*<p>
*This class was translated by a statistician from FORTRAN versions of 
*the LINPACK routines.  It is NOT an official translation.  It wastes
*memory by failing to use the first elements of vectors.  When 
*public domain Java numerical analysis routines become available 
*from the people who produce LAPACK, then <b>THE CODE PRODUCED
*BY THE NUMERICAL ANALYSTS SHOULD BE USED</b>.
*
*<p>
*Meanwhile, if you have suggestions for improving this
*code, please contact Steve Verrill at steve@ws10.fpl.fs.fed.us.
*
*@author Steve Verrill
*@version .5 --- February 25, 1997
* 
*/

public class QR_f77 extends Object {

/**
*
*<p>
*This method decomposes an n by p matrix X into a product, QR, of
*an orthogonal n by n matrix Q and an upper triangular n by p matrix R.
*For details, see the comments in the code.
*This method is a translation from FORTRAN to Java of the LINPACK subroutine
*DQRDC.  In the LINPACK listing DQRDC is attributed to G.W. Stewart
*with a date of 8/14/78.
*
*Translated by Steve Verrill, February 25, 1997.
*
*@param  X     The matrix to be decomposed
*@param  n     The number of rows of the matrix X
*@param  p     The number of columns of the matrix X
*@param  qraux This vector "contains further information required to
*              recover the orthogonal part of the decomposition."
*@param  jpvt  This vector contains pivoting information
*@param  work  This vector is used as temporary space
*@param  job   This value indicates whether column pivoting should be performed
*
*/

   public static void dqrdc_f77 (double x[][], int n, int p, double qraux[], int
                             jpvt[], double work[], int job) {

      int j,jj,jp,l,lp1,lup,maxj,pl,pu;

      double maxnrm,tt;
      double nrmxl,t;
      double fac;

      boolean negj,swapj;

/*

Here is a copy of the LINPACK documentation (from the SLATEC version):

C***BEGIN PROLOGUE  DQRDC
C***DATE WRITTEN   780814   (YYMMDD)
C***REVISION DATE  861211   (YYMMDD)
C***CATEGORY NO.  D5
C***KEYWORDS  LIBRARY=SLATEC(LINPACK),
C             TYPE=DOUBLE PRECISION(SQRDC-S DQRDC-D CQRDC-C),
C             LINEAR ALGEBRA,MATRIX,ORTHOGONAL TRIANGULAR,
C             QR DECOMPOSITION
C***AUTHOR  STEWART, G. W., (U. OF MARYLAND)
C***PURPOSE  Uses Householder transformations to compute the QR factori-
C            zation of N by P matrix X.  Column pivoting is optional.
C***DESCRIPTION
C
C     DQRDC uses Householder transformations to compute the QR
C     factorization of an N by P matrix X.  Column pivoting
C     based on the 2-norms of the reduced columns may be
C     performed at the user's option.
C
C     On Entry
C
C        X       DOUBLE PRECISION(LDX,P), where LDX .GE. N.
C                X contains the matrix whose decomposition is to be
C                computed.
C
C        LDX     INTEGER.
C                LDX is the leading dimension of the array X.
C
C        N       INTEGER.
C                N is the number of rows of the matrix X.
C
C        P       INTEGER.
C                P is the number of columns of the matrix X.
C
C        JPVT    INTEGER(P).
C                JPVT contains integers that control the selection
C                of the pivot columns.  The K-th column X(K) of X
C                is placed in one of three classes according to the
C                value of JPVT(K).
C
C                   If JPVT(K) .GT. 0, then X(K) is an initial
C                                      column.
C
C                   If JPVT(K) .EQ. 0, then X(K) is a free column.
C
C                   If JPVT(K) .LT. 0, then X(K) is a final column.
C
C                Before the decomposition is computed, initial columns
C                are moved to the beginning of the array X and final
C                columns to the end.  Both initial and final columns
C                are frozen in place during the computation and only
C                free columns are moved.  At the K-th stage of the
C                reduction, if X(K) is occupied by a free column
C                it is interchanged with the free column of largest
C                reduced norm.  JPVT is not referenced if
C                JOB .EQ. 0.
C
C        WORK    DOUBLE PRECISION(P).
C                WORK is a work array.  WORK is not referenced if
C                JOB .EQ. 0.
C
C        JOB     INTEGER.
C                JOB is an integer that initiates column pivoting.
C                If JOB .EQ. 0, no pivoting is done.
C                If JOB .NE. 0, pivoting is done.
C
C     On Return
C
C        X       X contains in its upper triangle the upper
C                triangular matrix R of the QR factorization.
C                Below its diagonal X contains information from
C                which the orthogonal part of the decomposition
C                can be recovered.  Note that if pivoting has
C                been requested, the decomposition is not that
C                of the original matrix X but that of X
C                with its columns permuted as described by JPVT.
C
C        QRAUX   DOUBLE PRECISION(P).
C                QRAUX contains further information required to recover
C                the orthogonal part of the decomposition.
C
C        JPVT    JPVT(K) contains the index of the column of the
C                original matrix that has been interchanged into
C                the K-th column, if pivoting was requested.
C
C     LINPACK.  This version dated 08/14/78 .
C     G. W. Stewart, University of Maryland, Argonne National Lab.
C
C     DQRDC uses the following functions and subprograms.
C
C     BLAS DAXPY,DDOT,DSCAL,DSWAP,DNRM2
C     Fortran DABS,DMAX1,MIN0,DSQRT
C***REFERENCES  DONGARRA J.J., BUNCH J.R., MOLER C.B., STEWART G.W.,
C                 *LINPACK USERS  GUIDE*, SIAM, 1979.
C***ROUTINES CALLED  DAXPY,DDOT,DNRM2,DSCAL,DSWAP
C***END PROLOGUE  DQRDC

*/

      pl = 1;
      pu = 0;

      if (job != 0) {

//   Pivoting has been requested.  Rearrange the columns according to jpvt.

         for (j = 1; j <= p; j++) {

            swapj = (jpvt[j] > 0);
            negj = (jpvt[j] < 0);

            jpvt[j] = j;
            if (negj) jpvt[j] = -j;

            if (swapj) {

               if (j != pl) Blas_f77.colswap_f77(n,x,pl,j);

               jpvt[j] = jpvt[pl];
               jpvt[pl] = j;
               pl++;

            }      

         }

         pu = p;

         for (jj = 1; jj <= p; jj++) {

            j = p - jj + 1;

            if (jpvt[j] < 0) {

               jpvt[j] = -jpvt[j];

               if (j != pu) {

                  Blas_f77.colswap_f77(n,x,pu,j);

                  jp = jpvt[pu];
                  jpvt[pu] = jpvt[j];
                  jpvt[j] = jp;

               }

               pu--;

            }

         }

      }

//   Compute the norms of the free columns.

//      if (pu >= pl) {    This test is not necessary under Java.
//                         The loop will be skipped if pu < pl.

         for (j = pl; j <= pu; j++) {

            qraux[j] = Blas_f77.colnrm2_f77(n,x,1,j);
            work[j] = qraux[j];

         }

//      }    This test is not necessary under Java.  See above.

//   Perform the Householder reduction of X.

      lup = Math.min(n,p);

      for (l = 1; l <= lup; l++) {

         if (l >= pl && l < pu) {

//   Locate the column of greatest norm and bring it into
//   the pivot position.

            maxnrm = 0.0;
            maxj = l;

            for (j = l; j <= pu; j++) {

               if (qraux[j] > maxnrm) {

                  maxnrm = qraux[j];
                  maxj = j;

               }

            }

            if (maxj != l) {

               Blas_f77.colswap_f77(n,x,l,maxj);
               qraux[maxj] = qraux[l];
               work[maxj] = work[l];
               jp = jpvt[maxj];
               jpvt[maxj] = jpvt[l];
               jpvt[l] = jp;

            }

         }

         qraux[l] = 0.0;

         if (l != n) {

//   Compute the Householder transformation for column l.

            nrmxl = Blas_f77.colnrm2_f77(n-l+1,x,l,l);

            if (nrmxl != 0.0) {

               if (x[l][l] != 0.0) nrmxl = Blas_f77.sign_f77(nrmxl,x[l][l]);

                Blas_f77.colscal_f77(n-l+1,1.0/nrmxl,x,l,l);

               x[l][l]++;

//   Apply the transformation to the remaining columns, 
//   updating the norms.

               lp1 = l + 1;

//               if (p >= lp1) {    This test is not necessary under Java.
//                                  The loop will be skipped if p < lp1.

                  for (j = lp1; j <= p; j++) {

                     t = -Blas_f77.coldot_f77(n-l+1,x,l,l,j)/x[l][l];

                     Blas_f77.colaxpy_f77(n-l+1,t,x,l,l,j);

                     if (j >= pl && j <= pu) {

                        if (qraux[j] != 0.0) {

                           fac = Math.abs(x[l][j])/qraux[j];
                           tt = 1.0 - fac*fac;
                           tt = Math.max(tt,0.0);
                           t = tt;
                           fac = qraux[j]/work[j];
                           tt = 1.0 + .05*tt*fac*fac;

                           if (tt != 1.0) {

                              qraux[j] *= Math.sqrt(t);

                           } else {

                              qraux[j] = Blas_f77.colnrm2_f77(n-l,x,l+1,j);
                              work[j] = qraux[j];

                           }

                        }

                     }

                  }

//               }    This test is not necessary under Java.  See above.

//   Save the transformation

               qraux[l] = x[l][l];
               x[l][l] = -nrmxl;

            }

         }

      }

      return;

   }
              


/**
*
*<p>
*This method "applies the output of DQRDC to compute coordinate
*transformations, projections, and least squares solutions."
*For details, see the comments in the code.
*This method is a translation from FORTRAN to Java of the LINPACK subroutine
*DQRSL.  In the LINPACK listing DQRSL is attributed to G.W. Stewart
*with a date of 8/14/78.
*
*Translated by Steve Verrill, February 27, 1997.
*
*@param  X     This n by p matrix contains most of the output from DQRDC
*@param  n     The number of rows of X
*@param  k     k <= min(n,p) where p is the number of columns of X
*@param  qraux This vector "contains further information required to
*              recover the orthogonal part of the decomposition"
*@param  y     This n by 1 vector will be manipulated by DQRSL
*@param  qy    On output, this vector contains Qy if it has been requested
*@param  qty   On output, this vector contains transpose(Q)y if it has been requested
*@param  b     Parameter estimates
*@param  rsd   Residuals
*@param  xb    Predicted values
*@param  job   Specifies what is to be computed (see the code for details)
*
*/

   public static int dqrsl_f77 (double x[][], int n, int k, double qraux[],
                                double y[], double qy[], double qty[],
                                double b[], double rsd[], double xb[],
                                int job) {

/*

Here is a copy of the LINPACK documentation (from the SLATEC version):

c***begin prologue  dqrsl
c***date written   780814   (yymmdd)
c***revision date  861211   (yymmdd)
c***category no.  d9,d2a1
c***keywords  library=slatec(linpack),
c             type=double precision(sqrsl-s dqrsl-d cqrsl-c),
c             linear algebra,matrix,orthogonal triangular,solve
c***author  stewart, g. w., (u. of maryland)
c***purpose  applies the output of dqrdc to compute coordinate
c            transformations, projections, and least squares solutions.
c***description
c
c     dqrsl applies the output of dqrdc to compute coordinate
c     transformations, projections, and least squares solutions.
c     for k .le. min(n,p), let xk be the matrix
c
c            xk = (x(jpvt(1)),x(jpvt(2)), ... ,x(jpvt(k)))
c
c     formed from columnns jpvt(1), ... ,jpvt(k) of the original
c     n x p matrix x that was input to dqrdc (if no pivoting was
c     done, xk consists of the first k columns of x in their
c     original order).  dqrdc produces a factored orthogonal matrix q
c     and an upper triangular matrix r such that
c
c              xk = q * (r)
c                       (0)
c
c     this information is contained in coded form in the arrays
c     x and qraux.
c
c     on entry
c
c        x      double precision(ldx,p).
c               x contains the output of dqrdc.
c
c        ldx    integer.
c               ldx is the leading dimension of the array x.
c
c        n      integer.
c               n is the number of rows of the matrix xk.  it must
c               have the same value as n in dqrdc.
c
c        k      integer.
c               k is the number of columns of the matrix xk.  k
c               must not be greater than min(n,p), where p is the
c               same as in the calling sequence to dqrdc.
c
c        qraux  double precision(p).
c               qraux contains the auxiliary output from dqrdc.
c
c        y      double precision(n)
c               y contains an n-vector that is to be manipulated
c               by dqrsl.
c
c        job    integer.
c               job specifies what is to be computed.  job has
c               the decimal expansion abcde, with the following
c               meaning.
c
c                    if a .ne. 0, compute qy.
c                    if b,c,d, or e .ne. 0, compute qty.
c                    if c .ne. 0, compute b.
c                    if d .ne. 0, compute rsd.
c                    if e .ne. 0, compute xb.
c
c               note that a request to compute b, rsd, or xb
c               automatically triggers the computation of qty, for
c               which an array must be provided in the calling
c               sequence.
c
c     on return
c
c        qy     double precision(n).
c               qy contains q*y, if its computation has been
c               requested.
c
c        qty    double precision(n).
c               qty contains trans(q)*y, if its computation has
c               been requested.  here trans(q) is the
c               transpose of the matrix q.
c
c        b      double precision(k)
c               b contains the solution of the least squares problem
c
c                    minimize norm2(y - xk*b),
c
c               if its computation has been requested.  (note that
c               if pivoting was requested in dqrdc, the j-th
c               component of b will be associated with column jpvt(j)
c               of the original matrix x that was input into dqrdc.)
c
c        rsd    double precision(n).
c               rsd contains the least squares residual y - xk*b,
c               if its computation has been requested.  rsd is
c               also the orthogonal projection of y onto the
c               orthogonal complement of the column space of xk.
c
c        xb     double precision(n).
c               xb contains the least squares approximation xk*b,
c               if its computation has been requested.  xb is also
c               the orthogonal projection of y onto the column space
c               of x.
c
c        info   integer.
c               info is zero unless the computation of b has
c               been requested and r is exactly singular.  in
c               this case, info is the index of the first zero
c               diagonal element of r and b is left unaltered.


   For the Java version, info 
   is the return value of the the dqrsl_f77 method.


c
c     the parameters qy, qty, b, rsd, and xb are not referenced
c     if their computation is not requested and in this case
c     can be replaced by dummy variables in the calling program.
c     to save storage, the user may in some cases use the same
c     array for different parameters in the calling sequence.  a
c     frequently occuring example is when one wishes to compute
c     any of b, rsd, or xb and does not need y or qty.  in this
c     case one may identify y, qty, and one of b, rsd, or xb, while
c     providing separate arrays for anything else that is to be
c     computed.  thus the calling sequence
c
c          call dqrsl(x,ldx,n,k,qraux,y,dum,y,b,y,dum,110,info)
c
c     will result in the computation of b and rsd, with rsd
c     overwriting y.  more generally, each item in the following
c     list contains groups of permissible identifications for
c     a single calling sequence.
c
c          1. (y,qty,b) (rsd) (xb) (qy)
c
c          2. (y,qty,rsd) (b) (xb) (qy)
c
c          3. (y,qty,xb) (b) (rsd) (qy)
c
c          4. (y,qy) (qty,b) (rsd) (xb)
c
c          5. (y,qy) (qty,rsd) (b) (xb)
c
c          6. (y,qy) (qty,xb) (b) (rsd)
c
c     in any group the value returned in the array allocated to
c     the group corresponds to the last member of the group.
c
c     linpack.  this version dated 08/14/78 .
c     g. w. stewart, university of maryland, argonne national lab.
c
c     dqrsl uses the following functions and subprograms.
c
c     blas daxpy,dcopy,ddot
c     fortran dabs,min0,mod
c***references  dongarra j.j., bunch j.r., moler c.b., stewart g.w.,
c                 *linpack users  guide*, siam, 1979.
c***routines called  daxpy,dcopy,ddot
c***end prologue  dqrsl

*/

      int i,j,jj,ju,kp1,info;
      double t,temp;
      boolean cb,cqy,cqty,cr,cxb;

//   set info flag

      info = 0;

//   determine what is to be computed

      cqy = (job/10000 != 0);
      cqty = ((job%10000) != 0);
      cb = ((job%1000)/100 != 0);
      cr = ((job%100)/10 != 0);
      cxb = ((job%10) != 0);

      ju = Math.min(k,n-1);

//   special action when n = 1

      if (ju == 0) {

         if (cqy) qy[1] = y[1];
         if (cqty) qty[1] = y[1];
         if (cxb) xb[1] = y[1];

         if (cb) {

            if (x[1][1] == 0.0) {

               info = 1;

            } else {

               b[1] = y[1]/x[1][1];

            }

         }

         if (cr) rsd[1] = 0.0;

         return info;

      }

//   set up to compute qy or transpose(q)y

      if (cqy) Blas_f77.dcopy_f77(n,y,1,qy,1);
      if (cqty) Blas_f77.dcopy_f77(n,y,1,qty,1);

      if (cqy) {

//   compute qy

         for (jj = 1; jj <= ju; jj++) {

            j = ju - jj + 1;

            if (qraux[j] != 0.0) {

               temp = x[j][j];
               x[j][j] = qraux[j];
               t = -Blas_f77.colvdot_f77(n-j+1,x,qy,j,j)/x[j][j];
               Blas_f77.colvaxpy_f77(n-j+1,t,x,qy,j,j);
               x[j][j] = temp;

            }

         }

      }

      if (cqty) {

//   compute transpose(q)y

         for (j = 1; j <= ju; j++) {

            if (qraux[j] != 0.0) {

               temp = x[j][j];
               x[j][j] = qraux[j];
               t = -Blas_f77.colvdot_f77(n-j+1,x,qty,j,j)/x[j][j];
               Blas_f77.colvaxpy_f77(n-j+1,t,x,qty,j,j);
               x[j][j] = temp;

            }

         }

      }

//   set up to compute b, rsd, or xb

      if (cb) Blas_f77.dcopy_f77(k,qty,1,b,1);

      kp1 = k + 1;

      if (cxb) Blas_f77.dcopy_f77(k,qty,1,xb,1);

      if (cr && (k < n)) Blas_f77.dcopyp_f77(n-k,qty,rsd,kp1);

      if (cxb && (kp1 <= n)) {

         for (i = kp1; i <= n; i++) {

            xb[i] = 0.0;

         }

      }

      if (cr) {

         for (i = 1; i <= k; i++) {

            rsd[i] = 0.0;

         }

      }

      if (cb) {

//   compute b

         for (jj = 1; jj <= k; jj++) {

            j = k - jj + 1;

            if (x[j][j] == 0.0) {

               info = j;
               break;

            }

            b[j] = b[j]/x[j][j];

            if (j != 1) {

               t = -b[j];
               Blas_f77.colvaxpy_f77(j-1,t,x,b,1,j);      

            }

         }

      }

      if (cr || cxb) {

//   compute rsd or xb as required

         for (jj = 1; jj <= ju; jj++) {

            j = ju - jj + 1;

            if (qraux[j] != 0.0) {

               temp = x[j][j];
               x[j][j] = qraux[j];
               
               if (cr) {

                  t = -Blas_f77.colvdot_f77(n-j+1,x,rsd,j,j)/x[j][j];
                  Blas_f77.colvaxpy_f77(n-j+1,t,x,rsd,j,j);

               }

               if (cxb) {

                  t = -Blas_f77.colvdot_f77(n-j+1,x,xb,j,j)/x[j][j];
                  Blas_f77.colvaxpy_f77(n-j+1,t,x,xb,j,j);

               }

               x[j][j] = temp;

            }

         }

      }

      return info;

   }
                                    
}
p matrix contains most of the output from DQRDC
*@param  n     The number of rows of X
*@param  k     k <= min(n,p) where p is the number of columns of X
*@param  qraux This veQR_j.java                                                                                           0100644 0007456 0000012 00000053664 06347630337 0013133 0                                                                                                    ustar 00steve                           staff                           0000040 0000007                                                                                                                                                                        /*
    QR_j.java copyright claim:

    This software is based on public domain LINPACK routines.
    It was translated from FORTRAN to Java by a US government employee 
    on official time.  Thus this software is also in the public domain.


    The translator's mail address is:

    Steve Verrill 
    USDA Forest Products Laboratory
    1 Gifford Pinchot Drive
    Madison, Wisconsin
    53705


    The translator's e-mail address is:

    steve@ws10.fpl.fs.fed.us


***********************************************************************

DISCLAIMER OF WARRANTIES:

THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND. 
THE TRANSLATOR DOES NOT WARRANT, GUARANTEE OR MAKE ANY REPRESENTATIONS 
REGARDING THE SOFTWARE OR DOCUMENTATION IN TERMS OF THEIR CORRECTNESS, 
RELIABILITY, CURRENTNESS, OR OTHERWISE. THE ENTIRE RISK AS TO 
THE RESULTS AND PERFORMANCE OF THE SOFTWARE IS ASSUMED BY YOU. 
IN NO CASE WILL ANY PARTY INVOLVED WITH THE CREATION OR DISTRIBUTION 
OF THE SOFTWARE BE LIABLE FOR ANY DAMAGE THAT MAY RESULT FROM THE USE 
OF THIS SOFTWARE.

Sorry about that.

***********************************************************************


History:

Date        Translator        Changes

2/25/97     Steve Verrill     Translated
6/5/97                        Java/C style indexing

*/


package linear_algebra;

import linear_algebra.*;


/**
*
*<p>
*This class contains the LINPACK DQRDC (QR decomposition)
*and DQRSL (QR solve) routines.
*
*<p>
*<b>IMPORTANT:</b>  The "_j" suffixes indicate that these routines use
*Java/C style indexing.  For example, you will see
*<pre>
*   for (i = 0; i < n; i++)
*</pre>
*rather than
*<pre>
*   for (i = 1; i <= n; i++)
*</pre>
*To use the "_j" routines you will have
*to fill elements 0 through n - 1 rather than elements 1 through n.
*Versions of these programs that use FORTYRAN style indexing are
*also available.  They end with the suffix "_f77".
*
*<p>
*This class was translated by a statistician from FORTRAN versions of 
*the LINPACK routines.  It is NOT an official translation.  When 
*public domain Java numerical analysis routines become available 
*from the people who produce LAPACK, then <b>THE CODE PRODUCED
*BY THE NUMERICAL ANALYSTS SHOULD BE USED</b>.
*
*<p>
*Meanwhile, if you have suggestions for improving this
*code, please contact Steve Verrill at steve@ws10.fpl.fs.fed.us.
*
*@author Steve Verrill
*@version .5 --- June 5, 1997
* 
*/

public class QR_j extends Object {

/**
*
*<p>
*This method decomposes an n by p matrix X into a product, QR, of
*an orthogonal n by n matrix Q and an upper triangular n by p matrix R.
*For details, see the comments in the code.
*This method is a translation from FORTRAN to Java of the LINPACK subroutine
*DQRDC.  In the LINPACK listing DQRDC is attributed to G.W. Stewart
*with a date of 8/14/78.
*
*Translated by Steve Verrill, February 25, 1997.
*
*@param  X     The matrix to be decomposed
*@param  n     The number of rows of the matrix X
*@param  p     The number of columns of the matrix X
*@param  qraux This vector "contains further information required to
*              recover the orthogonal part of the decomposition."
*@param  jpvt  This output vector contains pivoting information.
*@param  work  This vector is used as temporary space
*@param  job   This value indicates whether column pivoting should be performed
*
*/

   public static void dqrdc_j (double x[][], int n, int p, double qraux[], int
                               jpvt[], double work[], int job) {

      int j,jj,jp,l,lp1,lup,maxj,pl,pu;
      int jm1,plm1,pum1,lm1,maxjm1;

      double maxnrm,tt;
      double nrmxl,t;
      double fac;

      boolean initial,fin;

/*

Here is a copy of the LINPACK documentation (from the SLATEC version):

C***BEGIN PROLOGUE  DQRDC
C***DATE WRITTEN   780814   (YYMMDD)
C***REVISION DATE  861211   (YYMMDD)
C***CATEGORY NO.  D5
C***KEYWORDS  LIBRARY=SLATEC(LINPACK),
C             TYPE=DOUBLE PRECISION(SQRDC-S DQRDC-D CQRDC-C),
C             LINEAR ALGEBRA,MATRIX,ORTHOGONAL TRIANGULAR,
C             QR DECOMPOSITION
C***AUTHOR  STEWART, G. W., (U. OF MARYLAND)
C***PURPOSE  Uses Householder transformations to compute the QR factori-
C            zation of N by P matrix X.  Column pivoting is optional.
C***DESCRIPTION
C
C     DQRDC uses Householder transformations to compute the QR
C     factorization of an N by P matrix X.  Column pivoting
C     based on the 2-norms of the reduced columns may be
C     performed at the user's option.
C
C     On Entry
C
C        X       DOUBLE PRECISION(LDX,P), where LDX .GE. N.
C                X contains the matrix whose decomposition is to be
C                computed.
C
C        LDX     INTEGER.
C                LDX is the leading dimension of the array X.
C
C        N       INTEGER.
C                N is the number of rows of the matrix X.
C
C        P       INTEGER.
C                P is the number of columns of the matrix X.
C
C        JPVT    INTEGER(P).
C                JPVT contains integers that control the selection
C                of the pivot columns.  The K-th column X(K) of X
C                is placed in one of three classes according to the
C                value of JPVT(K).
C
C                   If JPVT(K) .GT. 0, then X(K) is an initial
C                                      column.
C
C                   If JPVT(K) .EQ. 0, then X(K) is a free column.
C
C                   If JPVT(K) .LT. 0, then X(K) is a final column.
C
C                Before the decomposition is computed, initial columns
C                are moved to the beginning of the array X and final
C                columns to the end.  Both initial and final columns
C                are frozen in place during the computation and only
C                free columns are moved.  At the K-th stage of the
C                reduction, if X(K) is occupied by a free column
C                it is interchanged with the free column of largest
C                reduced norm.  JPVT is not referenced if
C                JOB .EQ. 0.
C
C        WORK    DOUBLE PRECISION(P).
C                WORK is a work array.  WORK is not referenced if
C                JOB .EQ. 0.
C
C        JOB     INTEGER.
C                JOB is an integer that initiates column pivoting.
C                If JOB .EQ. 0, no pivoting is done.
C                If JOB .NE. 0, pivoting is done.
C
C     On Return
C
C        X       X contains in its upper triangle the upper
C                triangular matrix R of the QR factorization.
C                Below its diagonal X contains information from
C                which the orthogonal part of the decomposition
C                can be recovered.  Note that if pivoting has
C                been requested, the decomposition is not that
C                of the original matrix X but that of X
C                with its columns permuted as described by JPVT.
C
C        QRAUX   DOUBLE PRECISION(P).
C                QRAUX contains further information required to recover
C                the orthogonal part of the decomposition.
C
C        JPVT    JPVT(K) contains the index of the column of the
C                original matrix that has been interchanged into
C                the K-th column, if pivoting was requested.
C
C     LINPACK.  This version dated 08/14/78 .
C     G. W. Stewart, University of Maryland, Argonne National Lab.
C
C     DQRDC uses the following functions and subprograms.
C
C     BLAS DAXPY,DDOT,DSCAL,DSWAP,DNRM2
C     Fortran DABS,DMAX1,MIN0,DSQRT
C***REFERENCES  DONGARRA J.J., BUNCH J.R., MOLER C.B., STEWART G.W.,
C                 *LINPACK USERS  GUIDE*, SIAM, 1979.
C***ROUTINES CALLED  DAXPY,DDOT,DNRM2,DSCAL,DSWAP
C***END PROLOGUE  DQRDC

*/

      pl = 1;
      plm1 = pl - 1;
      pu = 0;

      if (job != 0) {

//   Pivoting has been requested.  Rearrange the columns according to pvt.

         for (j = 1; j <= p; j++) {

            jm1 = j - 1;

            initial = (jpvt[jm1] > 0);
            fin = (jpvt[jm1] < 0);

            jpvt[jm1] = j;
            if (fin) jpvt[jm1] = -j;

            if (initial) {

               if (j != pl) Blas_j.colswap_j(n,x,plm1,jm1);

               jpvt[jm1] = jpvt[plm1];
               jpvt[plm1] = j;
               plm1 = pl;
               pl++;
               

            }      

         }

         pu = p;
         pum1 = p - 1;

         for (jj = 1; jj <= p; jj++) {

            j = p - jj;

            if (jpvt[j] < 0) {

               jpvt[j] = -jpvt[j];

               if (j != pum1) {

                  Blas_j.colswap_j(n,x,pum1,j);

                  jp = jpvt[pum1];
                  jpvt[pum1] = jpvt[j];
                  jpvt[j] = jp;

               }

               pu--;
               pum1--;

            }

         }

      }

//   Compute the norms of the free columns.

//      if (pu >= pl) {    This test is not necessary under Java.
//                         The loop will be skipped if pu < pl.

         for (j = pl - 1; j < pu; j++) {

            qraux[j] = Blas_j.colnrm2_j(n,x,0,j);
            work[j] = qraux[j];

         }

//      }    This test is not necessary under Java.  See above.

//   Perform the Householder reduction of X.

      lup = Math.min(n,p);

      for (l = 1; l <= lup; l++) {

         lm1 = l - 1;

         if (l >= pl && l < pu) {

//   Locate the column of greatest norm and bring it into
//   the pivot position.

            maxnrm = 0.0;
            maxj = l;

            for (j = l; j <= pu; j++) {

               jm1 = j - 1;

               if (qraux[jm1] > maxnrm) {

                  maxnrm = qraux[jm1];
                  maxj = j;

               }

            }

            if (maxj != l) {

               maxjm1 = maxj - 1;

               Blas_j.colswap_j(n,x,lm1,maxjm1);
               qraux[maxjm1] = qraux[lm1];
               work[maxjm1] = work[lm1];
               jp = jpvt[maxjm1];
               jpvt[maxjm1] = jpvt[lm1];
               jpvt[lm1] = jp;

            }

         }

         qraux[lm1] = 0.0;

         if (l != n) {

//   Compute the Householder transformation for column l.

            nrmxl = Blas_j.colnrm2_j(n-l+1,x,lm1,lm1);

            if (nrmxl != 0.0) {

               if (x[lm1][lm1] != 0.0) nrmxl = Blas_j.sign_j(nrmxl,x[lm1][lm1]);

                Blas_j.colscal_j(n-l+1,1.0/nrmxl,x,lm1,lm1);

               x[lm1][lm1]++;

//   Apply the transformation to the remaining columns, 
//   updating the norms.

               lp1 = l + 1;

//               if (p >= lp1) {    This test is not necessary under Java.
//                                  The loop will be skipped if p < lp1.

                  for (j = lp1; j <= p; j++) {

                     jm1 = j - 1;

                     t = -Blas_j.coldot_j(n-l+1,x,lm1,lm1,jm1)/x[lm1][lm1];

                     Blas_j.colaxpy_j(n-l+1,t,x,lm1,lm1,jm1);

                     if (j >= pl && j <= pu) {

                        if (qraux[jm1] != 0.0) {

                           fac = Math.abs(x[lm1][jm1])/qraux[jm1];
                           tt = 1.0 - fac*fac;
                           tt = Math.max(tt,0.0);
                           t = tt;
                           fac = qraux[jm1]/work[jm1];
                           tt = 1.0 + .05*tt*fac*fac;

                           if (tt != 1.0) {

                              qraux[jm1] *= Math.sqrt(t);

                           } else {

                              qraux[jm1] = Blas_j.colnrm2_j(n-l,x,l,jm1);
                              work[jm1] = qraux[jm1];

                           }

                        }

                     }

                  }

//               }    This test is not necessary under Java.  See above.

//   Save the transformation

               qraux[lm1] = x[lm1][lm1];
               x[lm1][lm1] = -nrmxl;

            }

         }

      }

      return;

   }
              


/**
*
*<p>
*This method "applies the output of DQRDC to compute coordinate
*transformations, projections, and least squares solutions."
*For details, see the comments in the code.
*This method is a translation from FORTRAN to Java of the LINPACK subroutine
*DQRSL.  In the LINPACK listing DQRSL is attributed to G.W. Stewart
*with a date of 8/14/78.
*
*Translated by Steve Verrill, February 27, 1997.
*
*@param  X     This n by p matrix contains most of the output from DQRDC
*@param  n     The number of rows of X
*@param  k     k <= min(n,p) where p is the number of columns of X
*@param  qraux This vector "contains further information required to
*              recover the orthogonal part of the decomposition"
*@param  y     This n by 1 vector will be manipulated by DQRSL
*@param  qy    On output, this vector contains Qy if it has been requested
*@param  qty   On output, this vector contains transpose(Q)y if it has been requested
*@param  b     Parameter estimates
*@param  rsd   Residuals
*@param  xb    Predicted values
*@param  job   Specifies what is to be computed (see the code for details)
*
*/

   public static int dqrsl_j (double x[][], int n, int k, double qraux[],
                                double y[], double qy[], double qty[],
                                double b[], double rsd[], double xb[],
                                int job) {

/*

Here is a copy of the LINPACK documentation (from the SLATEC version):

c***begin prologue  dqrsl
c***date written   780814   (yymmdd)
c***revision date  861211   (yymmdd)
c***category no.  d9,d2a1
c***keywords  library=slatec(linpack),
c             type=double precision(sqrsl-s dqrsl-d cqrsl-c),
c             linear algebra,matrix,orthogonal triangular,solve
c***author  stewart, g. w., (u. of maryland)
c***purpose  applies the output of dqrdc to compute coordinate
c            transformations, projections, and least squares solutions.
c***description
c
c     dqrsl applies the output of dqrdc to compute coordinate
c     transformations, projections, and least squares solutions.
c     for k .le. min(n,p), let xk be the matrix
c
c            xk = (x(jpvt(1)),x(jpvt(2)), ... ,x(jpvt(k)))
c
c     formed from columnns jpvt(1), ... ,jpvt(k) of the original
c     n x p matrix x that was input to dqrdc (if no pivoting was
c     done, xk consists of the first k columns of x in their
c     original order).  dqrdc produces a factored orthogonal matrix q
c     and an upper triangular matrix r such that
c
c              xk = q * (r)
c                       (0)
c
c     this information is contained in coded form in the arrays
c     x and qraux.
c
c     on entry
c
c        x      double precision(ldx,p).
c               x contains the output of dqrdc.
c
c        ldx    integer.
c               ldx is the leading dimension of the array x.
c
c        n      integer.
c               n is the number of rows of the matrix xk.  it must
c               have the same value as n in dqrdc.
c
c        k      integer.
c               k is the number of columns of the matrix xk.  k
c               must not be greater than min(n,p), where p is the
c               same as in the calling sequence to dqrdc.
c
c        qraux  double precision(p).
c               qraux contains the auxiliary output from dqrdc.
c
c        y      double precision(n)
c               y contains an n-vector that is to be manipulated
c               by dqrsl.
c
c        job    integer.
c               job specifies what is to be computed.  job has
c               the decimal expansion abcde, with the following
c               meaning.
c
c                    if a .ne. 0, compute qy.
c                    if b,c,d, or e .ne. 0, compute qty.
c                    if c .ne. 0, compute b.
c                    if d .ne. 0, compute rsd.
c                    if e .ne. 0, compute xb.
c
c               note that a request to compute b, rsd, or xb
c               automatically triggers the computation of qty, for
c               which an array must be provided in the calling
c               sequence.
c
c     on return
c
c        qy     double precision(n).
c               qy contains q*y, if its computation has been
c               requested.
c
c        qty    double precision(n).
c               qty contains trans(q)*y, if its computation has
c               been requested.  here trans(q) is the
c               transpose of the matrix q.
c
c        b      double precision(k)
c               b contains the solution of the least squares problem
c
c                    minimize norm2(y - xk*b),
c
c               if its computation has been requested.  (note that
c               if pivoting was requested in dqrdc, the j-th
c               component of b will be associated with column jpvt(j)
c               of the original matrix x that was input into dqrdc.)
c
c        rsd    double precision(n).
c               rsd contains the least squares residual y - xk*b,
c               if its computation has been requested.  rsd is
c               also the orthogonal projection of y onto the
c               orthogonal complement of the column space of xk.
c
c        xb     double precision(n).
c               xb contains the least squares approximation xk*b,
c               if its computation has been requested.  xb is also
c               the orthogonal projection of y onto the column space
c               of x.
c
c        info   integer.
c               info is zero unless the computation of b has
c               been requested and r is exactly singular.  in
c               this case, info is the index of the first zero
c               diagonal element of r and b is left unaltered.


   For the Java version, info 
   is the return value of the the dqrsl_j method.


c
c     the parameters qy, qty, b, rsd, and xb are not referenced
c     if their computation is not requested and in this case
c     can be replaced by dummy variables in the calling program.
c     to save storage, the user may in some cases use the same
c     array for different parameters in the calling sequence.  a
c     frequently occuring example is when one wishes to compute
c     any of b, rsd, or xb and does not need y or qty.  in this
c     case one may identify y, qty, and one of b, rsd, or xb, while
c     providing separate arrays for anything else that is to be
c     computed.  thus the calling sequence
c
c          call dqrsl(x,ldx,n,k,qraux,y,dum,y,b,y,dum,110,info)
c
c     will result in the computation of b and rsd, with rsd
c     overwriting y.  more generally, each item in the following
c     list contains groups of permissible identifications for
c     a single calling sequence.
c
c          1. (y,qty,b) (rsd) (xb) (qy)
c
c          2. (y,qty,rsd) (b) (xb) (qy)
c
c          3. (y,qty,xb) (b) (rsd) (qy)
c
c          4. (y,qy) (qty,b) (rsd) (xb)
c
c          5. (y,qy) (qty,rsd) (b) (xb)
c
c          6. (y,qy) (qty,xb) (b) (rsd)
c
c     in any group the value returned in the array allocated to
c     the group corresponds to the last member of the group.
c
c     linpack.  this version dated 08/14/78 .
c     g. w. stewart, university of maryland, argonne national lab.
c
c     dqrsl uses the following functions and subprograms.
c
c     blas daxpy,dcopy,ddot
c     fortran dabs,min0,mod
c***references  dongarra j.j., bunch j.r., moler c.b., stewart g.w.,
c                 *linpack users  guide*, siam, 1979.
c***routines called  daxpy,dcopy,ddot
c***end prologue  dqrsl

*/

      int i,j,jj,ju,kp1,info;
      int jm1;
      double t,temp;
      boolean cb,cqy,cqty,cr,cxb;

//   set info flag

      info = 0;

//   determine what is to be computed

      cqy = (job/10000 != 0);
      cqty = ((job%10000) != 0);
      cb = ((job%1000)/100 != 0);
      cr = ((job%100)/10 != 0);
      cxb = ((job%10) != 0);

      ju = Math.min(k,n-1);

//   special action when n = 1

      if (ju == 0) {

         if (cqy) qy[0] = y[0];
         if (cqty) qty[0] = y[0];
         if (cxb) xb[0] = y[0];

         if (cb) {

            if (x[0][0] == 0.0) {

               info = 1;

            } else {

               b[0] = y[0]/x[0][0];

            }

         }

         if (cr) rsd[0] = 0.0;

         return info;

      }

//   set up to compute qy or transpose(q)y

      if (cqy) Blas_j.dcopy_j(n,y,1,qy,1);
      if (cqty) Blas_j.dcopy_j(n,y,1,qty,1);

      if (cqy) {

//   compute qy

         for (jj = 1; jj <= ju; jj++) {

            j = ju - jj;

            if (qraux[j] != 0.0) {

               temp = x[j][j];
               x[j][j] = qraux[j];
               t = -Blas_j.colvdot_j(n-j,x,qy,j,j)/x[j][j];
               Blas_j.colvaxpy_j(n-j,t,x,qy,j,j);
               x[j][j] = temp;

            }

         }

      }

      if (cqty) {

//   compute transpose(q)y

         for (j = 0; j < ju; j++) {

            if (qraux[j] != 0.0) {

               temp = x[j][j];
               x[j][j] = qraux[j];
               t = -Blas_j.colvdot_j(n-j,x,qty,j,j)/x[j][j];
               Blas_j.colvaxpy_j(n-j,t,x,qty,j,j);
               x[j][j] = temp;

            }

         }

      }

//   set up to compute b, rsd, or xb

      if (cb) Blas_j.dcopy_j(k,qty,1,b,1);

      if (cxb) Blas_j.dcopy_j(k,qty,1,xb,1);

      if (cr && (k < n)) Blas_j.dcopyp_j(n-k,qty,rsd,k);

      if (cxb) {

         for (i = k; i < n; i++) {

            xb[i] = 0.0;

         }

      }

      if (cr) {

         for (i = 0; i < k; i++) {

            rsd[i] = 0.0;

         }

      }

      if (cb) {

//   compute b

         for (jj = 1; jj <= k; jj++) {

            jm1 = k - jj;

            if (x[jm1][jm1] == 0.0) {

               info = jm1 + 1;
               break;

            }

            b[jm1] = b[jm1]/x[jm1][jm1];

            if (jm1 != 0) {

               t = -b[jm1];
               Blas_j.colvaxpy_j(jm1,t,x,b,0,jm1);      

            }

         }

      }

      if (cr || cxb) {

//   compute rsd or xb as required

         for (jj = 1; jj <= ju; jj++) {

            j = ju - jj;

            if (qraux[j] != 0.0) {

               temp = x[j][j];
               x[j][j] = qraux[j];
               
               if (cr) {

                  t = -Blas_j.colvdot_j(n-j,x,rsd,j,j)/x[j][j];
                  Blas_j.colvaxpy_j(n-j,t,x,rsd,j,j);

               }

               if (cxb) {

                  t = -Blas_j.colvdot_j(n-j,x,xb,j,j)/x[j][j];
                  Blas_j.colvaxpy_j(n-j,t,x,xb,j,j);

               }

               x[j][j] = temp;

            }

         }

      }

      return info;

   }
                                    
}

8/14/78.
*
*Translated by Steve Verrill, February 27, 1997.
*
*@param  X    SVDCException.java                                                                                  0100644 0007456 0000012 00000000623 06365735076 0014707 0                                                                                                    ustar 00steve                           staff                           0000040 0000007                                                                                                                                                                        
package linear_algebra;

/**
*
*This is the exception produced by the Singular Value Decomposition method.
*See the SVDC_f77.java code for details.
*
*/

public class SVDCException extends Exception {

   public int info;


   public SVDCException(int info) {

      this.info = info;

   }

   public SVDCException(String problem) {

      super(problem);

   }

   public SVDCException() {

   }

}

                                                                                                             SVDCTest_f77.java                                                                                   0100644 0007456 0000012 00000015515 06312344535 0014345 0                                                                                                    ustar 00steve                           staff                           0000040 0000007                                                                                                                                                                        
package linear_algebra;

import linear_algebra.*;
import corejava.Console;

import java.util.*;
import java.lang.*;
import java.io.*;

/**
*
*This class tests the (LINPACK) SVDC method.
*
*It
*<ol>
*<li> Randomly fills an n by n matrix, X.
*<li> Randomly generates a vector b0.
*<li> Calculates the vector y = X(b0).
*<li> Performs a UDV&#180 decomposition of X.
*<li> Solves the system Xb = y in an effort to recover b0. 
*<li> Handles the regression equation y = a0 + a1*x + a2*x^2 + e
*     where a0 = a1 = a2 = 1.  Its estimates of the parameters
*     should be close to 1.  Its estimate of the standard deviation
*     should be close to the input value.
*</ol>
*
*@author Steve Verrill
*@version .5 --- March 6, 1997
*
*/


public class SVDCTest_f77 extends Object {


   public static void main (String args[]) {


/*

Some of the variables of main:


*/ 

      long randstart;

      int n,p;

      int i,j,k;

      double randlow,randhigh,randdiff;

      double x[][] = new double[101][101];
      double copyx[][] = new double[101][101];

      double y[] = new double[101];
      double b0[] = new double[101];
      double b[] = new double[101];

      double s[] = new double[101];
      double e[] = new double[101];
      double u[][] = new double[101][101];
      double ut[][] = new double[101][101];
      double v[][] = new double[101][101];
      double work[] = new double[101];

      double pred[] = new double[101];

      double uty[] = new double[101];

      double sd,rss,diff;
      
      int job;


/*

   Console is a public domain class described in Cornell
   and Horstmann's Core Java (SunSoft Press, Prentice-Hall).

*/

      n = Console.readInt("\nWhat is the n value? (100 or less) ");
   
      randstart = Console.readInt("\nWhat is the starting value (an " +
      "integer)\n" +
      "for the random number generator? ");

      Random rand01 = new Random(randstart);

      randlow = Console.readDouble("\nWhat is randlow? ");

      randhigh = Console.readDouble("\nWhat is randhigh? ");

      randdiff = randhigh - randlow;

      sd = Console.readDouble("\nWhat is the standard deviation? ");


/*

   Generate an n by n X

*/


      for (i = 1; i <= n; i++) {

         for (j = 1; j <= n; j++) {

            x[i][j] = randlow + rand01.nextDouble()*randdiff;

         }

      }



/*
      System.out.print("\n\n");
      System.out.print("*********************************************\n");
      System.out.print("\nThe X matrix is \n\n");

      for (i = 1; i <= n; i++) {

         for (j = 1; j <= n; j++) {

            System.out.print(x[i][j] + "  ");

         }

         System.out.print("\n");

      }
*/



/*

   Generate a vector b

*/


      for (i = 1; i <= n; i++) {

         b0[i] = randlow + rand01.nextDouble()*randdiff;

      }


/*

   Calculate Xb = y

*/

      for (i = 1; i <= n; i++) {

         y[i] = 0.0;

         for (j = 1; j <= n; j++) {

            y[i] += x[i][j]*b0[j];

         }

      }


      job = 11;

      try {

         SVDC_f77.dsvdc_f77(x,n,n,s,e,u,v,work,job);

/*   

   This should obtain the solution to Xb = y.

*/

         Blas_f77.mattran_f77(u,ut,n,n);

         Blas_f77.matvec_f77(ut,y,uty,n,n);

         for (i = 1; i <= n; i++) {

            uty[i] = uty[i]/s[i];

         }

         Blas_f77.matvec_f77(v,uty,b,n,n);


         System.out.print("\nThe input b vector was:\n\n");

         for (i = 1; i <= n; i++) {

            System.out.print(b0[i] + "\n");

         }

         System.out.print("\n\nThe recovered b vector was:\n\n");

         for (i = 1; i <= n; i++) {

            System.out.print(b[i] + "\n");

         }

      } catch (SVDCException svdce) {

         System.out.print("\nThere was an SVDC_f77 exception on the" +
         " first test.\n\n" +
         "The info value from SVDC_f77 was " + svdce.info +
         ".\n\n");  

      }

      System.out.print("\n");
      System.out.print("\n");
      System.out.print("*********************************************\n");
      System.out.print("\n");


/*

   Generate a new X matrix and the y vector

*/

      for (i = 1; i <= n; i++) {

         x[i][1] = 1.0;
         copyx[i][1] = x[i][1];
         x[i][2] = -1.0 + rand01.nextDouble()*2.0;
         copyx[i][2] = x[i][2];
         x[i][3] = x[i][2]*x[i][2];
         copyx[i][3] = x[i][3];

         y[i] = x[i][1] + x[i][2] + x[i][3] + sd*normi(rand01.nextDouble());

      }

      job = 21;

      try {

         SVDC_f77.dsvdc_f77(x,n,3,s,e,u,v,work,job);

/*   

   This should obtain the solution to Xb = y.

*/

         Blas_f77.mattran_f77(u,ut,n,3);

         Blas_f77.matvec_f77(ut,y,uty,3,n);

         for (i = 1; i <= 3; i++) {

            uty[i] = uty[i]/s[i];

         }

         b[1] = 0.0;
         b[2] = 0.0;
         b[3] = 0.0;
         Blas_f77.matvec_f77(v,uty,b,3,3);

         System.out.print("\nThe input b vector was: \n\n1\n1\n1\n\n");

         System.out.print("\n\nThe recovered b vector was:\n\n");

         for (i = 1; i <= 3; i++) {

            System.out.print(b[i] + "\n");

         }

         Blas_f77.matvec_f77(copyx,b,pred,n,3);

         rss = 0.0;

         for (i = 1; i <= n; i++) {

            diff = y[i] - pred[i];
            rss += diff*diff;

         }

         sd = Math.sqrt(rss/(n - 1.0));

         System.out.print("\nThe estimate of sigma from the residuals");
         System.out.print(" was " + sd + "\n");

      } catch (SVDCException svdce) {

         System.out.print("\nThere was an SVDC_f77 exception on the" +
         " second test.\n\n" +
         "The info value from SVDC_f77 was " + svdce.info +
         ".\n\n");  

      }

      System.out.print("\n");
      System.out.print("\n");
      System.out.print("*********************************************\n");
      System.out.print("\n");


   }



/**
*
*<p>
*This is a normal cdf inverse routine.
*
*Created by Steve Verrill, March 1997.
*
*@param  u   The value (between 0 and 1) to invert.
*
*/

   public static double normi (double u) {

//   This is a temporary and potentially ugly way to generate
//   normal random numbers from uniform random numbers

      double c[] = new double[4];
      double d[] = new double[4];

      double normi,arg,t,t2,t3,xnum,xden;

      c[1] = 2.515517;
      c[2] = .802853;
      c[3] = .010328;

      d[1] = 1.432788;
      d[2] = .189269;
      d[3] = .001308;

      if (u <= .5) {

         arg = -2.0*Math.log(u);
         t = Math.sqrt(arg);
         t2 = t*t;
         t3 = t2*t;

         xnum = c[1] + c[2]*t + c[3]*t2;
         xden = 1.0 + d[1]*t + d[2]*t2 + d[3]*t3;

         normi = -(t - xnum/xden);

         return normi;

      } else {

         arg = -2.0*Math.log(1.0 - u);
  
         t = Math.sqrt(arg);
         t2 = t*t;
         t3 = t2*t;

         xnum = c[1] + c[2]*t + c[3]*t2;
         xden = 1.0 + d[1]*t + d[2]*t2 + d[3]*t3;

         normi = t - xnum/xden;

         return normi;

      }

   }

}



bles of main:


*/ 

      long randstart;

      int n,p;

      int i,j,k;

      double randlow,randhigh,randdiff;

      double x[][] = new double[101][101];
      double copySVDCTest_j.java                                                                                     0100644 0007456 0000012 00000015420 06347360015 0014165 0                                                                                                    ustar 00steve                           staff                           0000040 0000007                                                                                                                                                                        
package linear_algebra;

import linear_algebra.*;
import corejava.Console;

import java.util.*;
import java.lang.*;
import java.io.*;

/**
*
*This class tests the (LINPACK) SVDC method.
*
*It
*<ol>
*<li> Randomly fills an n by n matrix, X.
*<li> Randomly generates a vector b0.
*<li> Calculates the vector y = X(b0).
*<li> Performs a UDV&#180 decomposition of X.
*<li> Solves the system Xb = y in an effort to recover b0. 
*<li> Handles the regression equation y = a0 + a1*x + a2*x^2 + e
*     where a0 = a1 = a2 = 1.  Its estimates of the parameters
*     should be close to 1.  Its estimate of the standard deviation
*     should be close to the input value.
*</ol>
*
*@author Steve Verrill
*@version .5 --- June 9, 1997
*
*/


public class SVDCTest_j extends Object {


   public static void main (String args[]) {


/*

Some of the variables of main:


*/ 

      long randstart;

      int n,p;

      int i,j,k;

      double randlow,randhigh,randdiff;

      double x[][] = new double[100][100];
      double copyx[][] = new double[100][100];

      double y[] = new double[100];
      double b0[] = new double[100];
      double b[] = new double[100];

      double s[] = new double[100];
      double e[] = new double[100];
      double u[][] = new double[100][100];
      double ut[][] = new double[100][100];
      double v[][] = new double[100][100];
      double work[] = new double[100];

      double pred[] = new double[100];

      double uty[] = new double[100];

      double sd,rss,diff;
      
      int job;


/*

   Console is a public domain class described in Cornell
   and Horstmann's Core Java (SunSoft Press, Prentice-Hall).

*/

      n = Console.readInt("\nWhat is the n value? (100 or less) ");
   
      randstart = Console.readInt("\nWhat is the starting value (an " +
      "integer)\n" +
      "for the random number generator? ");

      Random rand01 = new Random(randstart);

      randlow = Console.readDouble("\nWhat is randlow? ");

      randhigh = Console.readDouble("\nWhat is randhigh? ");

      randdiff = randhigh - randlow;

      sd = Console.readDouble("\nWhat is the standard deviation? ");


/*

   Generate an n by n X

*/


      for (i = 0; i < n; i++) {

         for (j = 0; j < n; j++) {

            x[i][j] = randlow + rand01.nextDouble()*randdiff;

         }

      }



/*
      System.out.print("\n\n");
      System.out.print("*********************************************\n");
      System.out.print("\nThe X matrix is \n\n");

      for (i = 0; i < n; i++) {

         for (j = 0; j < n; j++) {

            System.out.print(x[i][j] + "  ");

         }

         System.out.print("\n");

      }
*/



/*

   Generate a vector b

*/


      for (i = 0; i < n; i++) {

         b0[i] = randlow + rand01.nextDouble()*randdiff;

      }


/*

   Calculate Xb = y

*/

      for (i = 0; i < n; i++) {

         y[i] = 0.0;

         for (j = 0; j < n; j++) {

            y[i] += x[i][j]*b0[j];

         }

      }


      job = 11;

      try {

         SVDC_j.dsvdc_j(x,n,n,s,e,u,v,work,job);

/*   

   This should obtain the solution to Xb = y.

*/

         Blas_j.mattran_j(u,ut,n,n);

         Blas_j.matvec_j(ut,y,uty,n,n);

         for (i = 0; i < n; i++) {

            uty[i] = uty[i]/s[i];

         }

         Blas_j.matvec_j(v,uty,b,n,n);


         System.out.print("\nThe input b vector was:\n\n");

         for (i = 0; i < n; i++) {

            System.out.print(b0[i] + "\n");

         }

         System.out.print("\n\nThe recovered b vector was:\n\n");

         for (i = 0; i < n; i++) {

            System.out.print(b[i] + "\n");

         }

      } catch (SVDCException svdce) {

         System.out.print("\nThere was an SVDC_j exception on the" +
         " first test.\n\n" +
         "The info value from SVDC_j was " + svdce.info +
         ".\n\n");  

      }

      System.out.print("\n");
      System.out.print("\n");
      System.out.print("*********************************************\n");
      System.out.print("\n");


/*

   Generate a new X matrix and the y vector

*/

      for (i = 0; i < n; i++) {

         x[i][0] = 1.0;
         copyx[i][0] = x[i][0];
         x[i][1] = -1.0 + rand01.nextDouble()*2.0;
         copyx[i][1] = x[i][1];
         x[i][2] = x[i][1]*x[i][1];
         copyx[i][2] = x[i][2];

         y[i] = x[i][0] + x[i][1] + x[i][2] + sd*normi(rand01.nextDouble());

      }

      job = 21;

      try {

         SVDC_j.dsvdc_j(x,n,3,s,e,u,v,work,job);

/*   

   This should obtain the solution to Xb = y.

*/

         Blas_j.mattran_j(u,ut,n,3);

         Blas_j.matvec_j(ut,y,uty,3,n);

         for (i = 0; i < 3; i++) {

            uty[i] = uty[i]/s[i];

         }

         b[0] = 0.0;
         b[1] = 0.0;
         b[2] = 0.0;
         Blas_j.matvec_j(v,uty,b,3,3);

         System.out.print("\nThe input b vector was: \n\n1\n1\n1\n\n");

         System.out.print("\n\nThe recovered b vector was:\n\n");

         for (i = 0; i < 3; i++) {

            System.out.print(b[i] + "\n");

         }

         Blas_j.matvec_j(copyx,b,pred,n,3);

         rss = 0.0;

         for (i = 0; i < n; i++) {

            diff = y[i] - pred[i];
            rss += diff*diff;

         }

         sd = Math.sqrt(rss/(n - 1.0));

         System.out.print("\nThe estimate of sigma from the residuals");
         System.out.print(" was " + sd + "\n");

      } catch (SVDCException svdce) {

         System.out.print("\nThere was an SVDC_j exception on the" +
         " second test.\n\n" +
         "The info value from SVDC_j was " + svdce.info +
         ".\n\n");  

      }

      System.out.print("\n");
      System.out.print("\n");
      System.out.print("*********************************************\n");
      System.out.print("\n");


   }



/**
*
*<p>
*This is a normal cdf inverse routine.
*
*Created by Steve Verrill, March 1997.
*
*@param  u   The value (between 0 and 1) to invert.
*
*/

   public static double normi (double u) {

//   This is a temporary and potentially ugly way to generate
//   normal random numbers from uniform random numbers

      double c[] = new double[4];
      double d[] = new double[4];

      double normi,arg,t,t2,t3,xnum,xden;

      c[1] = 2.515517;
      c[2] = .802853;
      c[3] = .010328;

      d[1] = 1.432788;
      d[2] = .189269;
      d[3] = .001308;

      if (u <= .5) {

         arg = -2.0*Math.log(u);
         t = Math.sqrt(arg);
         t2 = t*t;
         t3 = t2*t;

         xnum = c[1] + c[2]*t + c[3]*t2;
         xden = 1.0 + d[1]*t + d[2]*t2 + d[3]*t3;

         normi = -(t - xnum/xden);

         return normi;

      } else {

         arg = -2.0*Math.log(1.0 - u);
  
         t = Math.sqrt(arg);
         t2 = t*t;
         t3 = t2*t;

         xnum = c[1] + c[2]*t + c[3]*t2;
         xden = 1.0 + d[1]*t + d[2]*t2 + d[3]*t3;

         normi = t - xnum/xden;

         return normi;

      }

   }

}



                                                                                                                                                                                                                                                SVDC_f77.java                                                                                       0100644 0007456 0000012 00000055642 06347630337 0013520 0                                                                                                    ustar 00steve                           staff                           0000040 0000007                                                                                                                                                                        /*
    SVDC_f77.java copyright claim:

    This software is based on public domain LINPACK routines.
    It was translated from FORTRAN to Java by a US government employee 
    on official time.  Thus this software is also in the public domain.


    The translator's mail address is:

    Steve Verrill 
    USDA Forest Products Laboratory
    1 Gifford Pinchot Drive
    Madison, Wisconsin
    53705


    The translator's e-mail address is:

    steve@ws10.fpl.fs.fed.us


***********************************************************************

DISCLAIMER OF WARRANTIES:

THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND. 
THE TRANSLATOR DOES NOT WARRANT, GUARANTEE OR MAKE ANY REPRESENTATIONS 
REGARDING THE SOFTWARE OR DOCUMENTATION IN TERMS OF THEIR CORRECTNESS, 
RELIABILITY, CURRENTNESS, OR OTHERWISE. THE ENTIRE RISK AS TO 
THE RESULTS AND PERFORMANCE OF THE SOFTWARE IS ASSUMED BY YOU. 
IN NO CASE WILL ANY PARTY INVOLVED WITH THE CREATION OR DISTRIBUTION 
OF THE SOFTWARE BE LIABLE FOR ANY DAMAGE THAT MAY RESULT FROM THE USE 
OF THIS SOFTWARE.

Sorry about that.

***********************************************************************


History:

Date        Translator        Changes

3/3/97      Steve Verrill     Translated

*/


package linear_algebra;

import linear_algebra.*;


/**
*
*<p>
*This class contains the LINPACK DSVDC (singular value
*decomposition) routine.
*
*<p>
*<b>IMPORTANT:</b>  The "_f77" suffixes indicate that these routines use
*FORTRAN style indexing.  For example, you will see
*<pre>
*   for (i = 1; i <= n; i++)
*</pre>
*rather than
*<pre>
*   for (i = 0; i < n; i++)
*</pre>
*To use the "_f77" routines you will have to declare your vectors
*and matrices to be one element larger (e.g., v[101] rather than
*v[100], and a[101][101] rather than a[100][100]), and you will have
*to fill elements 1 through n rather than elements 0 through n - 1.
*Versions of these programs that use C/Java style indexing will
*soon be available.  They will end with the suffix "_j".
*
*<p>
*This class was translated by a statistician from FORTRAN versions of 
*the LINPACK routines.  It is NOT an official translation.  It wastes
*memory by failing to use the first elements of vectors.  When 
*public domain Java numerical analysis routines become available 
*from the people who produce LAPACK, then <b>THE CODE PRODUCED
*BY THE NUMERICAL ANALYSTS SHOULD BE USED</b>.
*
*<p>
*Meanwhile, if you have suggestions for improving this
*code, please contact Steve Verrill at steve@ws10.fpl.fs.fed.us.
*
*@author Steve Verrill
*@version .5 --- March 3, 1997
* 
*/

public class SVDC_f77 extends Object {

/**
*
*<p>
*This method decomposes a n by p 
*matrix X into a product UDV&#180 where ...
*For details, see the comments in the code.
*This method is a translation from FORTRAN to Java 
*of the LINPACK subroutine DSVDC.  
*In the LINPACK listing DSVDC is attributed to G.W. Stewart
*with a date of 3/19/79.
*
*Translated by Steve Verrill, March 3, 1997.
*
*@param  x     The matrix to be decomposed
*@param  n     The number of rows of X
*@param  p     The number of columns of X
*@param  s     The singular values of X
*@param  e     See the documentation in the code
*@param  u     The left singular vectors
*@param  v     The right singular vectors
*@param  work  A work array
*@param  job   See the documentation in the code
*@param  info  See the documentation in the code
*
*/



   public static void dsvdc_f77 (double x[][], int n, int p, double s[],
                                 double e[], double u[][], double v[][], 
                                 double work[], int job) 
                                 throws SVDCException {


/*

Here is a copy of the LINPACK documentation (from the SLATEC version):


C***BEGIN PROLOGUE  DSVDC
C***DATE WRITTEN   790319   (YYMMDD)
C***REVISION DATE  861211   (YYMMDD)
C***CATEGORY NO.  D6
C***KEYWORDS  LIBRARY=SLATEC(LINPACK),
C             TYPE=DOUBLE PRECISION(SSVDC-S DSVDC-D CSVDC-C),
C             LINEAR ALGEBRA,MATRIX,SINGULAR VALUE DECOMPOSITION
C***AUTHOR  STEWART, G. W., (U. OF MARYLAND)
C***PURPOSE  Perform the singular value decomposition of a d.p. NXP
C            matrix.
C***DESCRIPTION
C
C     DSVDC is a subroutine to reduce a double precision NxP matrix X
C     by orthogonal transformations U and V to diagonal form.  The
C     diagonal elements S(I) are the singular values of X.  The
C     columns of U are the corresponding left singular vectors,
C     and the columns of V the right singular vectors.
C
C     On Entry
C
C         X         DOUBLE PRECISION(LDX,P), where LDX .GE. N.
C                   X contains the matrix whose singular value
C                   decomposition is to be computed.  X is
C                   destroyed by DSVDC.
C
C         LDX       INTEGER.
C                   LDX is the leading dimension of the array X.
C
C         N         INTEGER.
C                   N is the number of rows of the matrix X.
C
C         P         INTEGER.
C                   P is the number of columns of the matrix X.
C
C         LDU       INTEGER.
C                   LDU is the leading dimension of the array U.
C                   (See below).
C
C         LDV       INTEGER.
C                   LDV is the leading dimension of the array V.
C                   (See below).
C
C         WORK      DOUBLE PRECISION(N).
C                   WORK is a scratch array.
C
C         JOB       INTEGER.
C                   JOB controls the computation of the singular
C                   vectors.  It has the decimal expansion AB
C                   with the following meaning
C
C                        A .EQ. 0    do not compute the left singular
C                                  vectors.
C                        A .EQ. 1    return the N left singular vectors
C                                  in U.
C                        A .GE. 2    return the first MIN(N,P) singular
C                                  vectors in U.
C                        B .EQ. 0    do not compute the right singular
C                                  vectors.
C                        B .EQ. 1    return the right singular vectors
C                                  in V.
C
C     On Return
C
C         S         DOUBLE PRECISION(MM), where MM=MIN(N+1,P).
C                   The first MIN(N,P) entries of S contain the
C                   singular values of X arranged in descending
C                   order of magnitude.
C
C         E         DOUBLE PRECISION(P).
C                   E ordinarily contains zeros.  However see the
C                   discussion of INFO for exceptions.
C
C         U         DOUBLE PRECISION(LDU,K), where LDU .GE. N.
C                   If JOBA .EQ. 1, then K .EQ. N.
C                   If JOBA .GE. 2, then K .EQ. MIN(N,P).
C                   U contains the matrix of right singular vectors.
C                   U is not referenced if JOBA .EQ. 0.  If N .LE. P
C                   or if JOBA .EQ. 2, then U may be identified with X
C                   in the subroutine call.
C
C         V         DOUBLE PRECISION(LDV,P), where LDV .GE. P.
C                   V contains the matrix of right singular vectors.
C                   V is not referenced if JOB .EQ. 0.  If P .LE. N,
C                   then V may be identified with X in the
C                   subroutine call.
C
C         INFO      INTEGER.
C                   The singular values (and their corresponding
C                   singular vectors) S(INFO+1),S(INFO+2),...,S(M)
C                   are correct (here M=MIN(N,P)).  Thus if
C                   INFO .EQ. 0, all the singular values and their
C                   vectors are correct.  In any event, the matrix
C                   B = TRANS(U)*X*V is the bidiagonal matrix
C                   with the elements of S on its diagonal and the
C                   elements of E on its super-diagonal (TRANS(U)
C                   is the transpose of U).  Thus the singular
C                   values of X and B are the same.


   For the Java version, info
   is returned as an argument to SVDCException
   if the decomposition fails.


C
C     LINPACK.  This version dated 03/19/79 .
C     G. W. Stewart, University of Maryland, Argonne National Lab.
C
C     DSVDC uses the following functions and subprograms.
C
C     External DROT
C     BLAS DAXPY,DDOT,DSCAL,DSWAP,DNRM2,DROTG
C     Fortran DABS,DMAX1,MAX0,MIN0,MOD,DSQRT
C***REFERENCES  DONGARRA J.J., BUNCH J.R., MOLER C.B., STEWART G.W.,
C                 *LINPACK USERS  GUIDE*, SIAM, 1979.
C***ROUTINES CALLED  DAXPY,DDOT,DNRM2,DROT,DROTG,DSCAL,DSWAP
C***END PROLOGUE  DSVDC

*/

//   There is a bug in the 1.0.2 Java compiler that
//   forces me to skimp on the number of local
//   variables.  This is the cause of the following changes:

//      double t,b,c,cs,el,emm1,f,g,scale1,scale2,scale,shift,sl,sm,
//             sn,smm1,t1,test,ztest,fac;

//      int i,iter,j,jobu,k,kase,kk,l,ll,lls,lm1,lp1,ls,lu,
//          m,maxit,mm,mm1,mp1,nct,nctp1,ncu,nrt,nrtp1;

      double t,b,c,cs,f,g,scale,shift,
             sn,t1,test,ztest;

      double rotvec[] = new double[5];

      int i,iter,j,jobu,k,kase,kk,l,ll,lls,lm1,lp1,ls,lu,
          m,maxit,mm,mp1,nct,ncu,nrt;

      boolean wantu,wantv;

//   Java requires that all local variables be initialized before they
//   are used.  This is the reason for the initialization
//   line below:

      ls = 0;

//   set the maximum number of iterations

      maxit = 30;

//   determine what is to be computed

      wantu = false;
      wantv = false;

      jobu = (job%100)/10;

      ncu = n;

      if (jobu > 1) ncu = Math.min(n,p);
      if (jobu != 0) wantu = true;
      if ((job%10) != 0) wantv = true;

//   reduce x to bidiagonal form, storing the diagonal elements
//   in s and the super-diagonal elements in e

      nct = Math.min(n-1,p);
      nrt = Math.max(0,Math.min(p-2,n));
      lu = Math.max(nct,nrt);

//      if (lu >= 1) {       This test is not necessary under Java.
//                           The loop will be skipped if lu < 1 = the
//                           starting value of l.

         for (l = 1; l <= lu; l++) {

            lp1 = l + 1;

            if (l <= nct) {

//   compute the transformation for the l-th column and
//   place the l-th diagonal in s[l]

               s[l] = Blas_f77.colnrm2_f77(n-l+1,x,l,l);

               if (s[l] != 0.0) {

                  if (x[l][l] != 0.0) s[l] = Blas_f77.sign_f77(s[l],x[l][l]);
                  Blas_f77.colscal_f77(n-l+1,1.0/s[l],x,l,l);
                  x[l][l]++;

               }

               s[l] = -s[l];

            }

//            if (p >= lp1) {   This test is not necessary under Java.
//                              The loop will be skipped if p < lp1.

               for (j = lp1; j <= p; j++) {

                  if ((l <= nct) && (s[l] != 0.0)) {

//   apply the transformation

                     t = -Blas_f77.coldot_f77(n-l+1,x,l,l,j)/x[l][l];
                     Blas_f77.colaxpy_f77(n-l+1,t,x,l,l,j);

                  }

//   place the l-th row of x into e for the
//   subsequent calculation of the row transformation

                  e[j] = x[l][j];

               }

//            }   This test is not necessary under Java.  See above.

            if (wantu && (l <= nct)) {

//   place the transformation in u for subsequent 
//   back multiplication

               for (i = l; i <= n; i++) {

                  u[i][l] = x[i][l];

               }

            }

            if (l <= nrt) {

//   compute the l-th row transformation and place the
//   l-th super-diagonal in e[l]

               e[l] = Blas_f77.dnrm2p_f77(p-l,e,lp1);

               if (e[l] != 0.0) {

                  if (e[lp1] != 0.0) e[l] = Blas_f77.sign_f77(e[l],e[lp1]);
                  Blas_f77.dscalp_f77(p-l,1.0/e[l],e,lp1);
                  e[lp1]++;

               }

               e[l] = -e[l];

               if ((lp1 <= n) && (e[l] != 0.0)) {

//   apply the transformation

                  for (i = lp1; i <= n; i++) {

                     work[i] = 0.0;

                  }

                  for (j = lp1; j <= p; j++) {

                     Blas_f77.colvaxpy_f77(n-l,e[j],x,work,lp1,j);

                  }

                  for (j = lp1; j <= p; j++) {

                     Blas_f77.colvraxpy_f77(n-l,-e[j]/e[lp1],work,x,lp1,j);

                  }

               }

               if (wantv) {

//   place the transformation in v for subsequent
//   back multiplication

                  for (i = lp1; i <= p; i++) {

                     v[i][l] = e[i];

                  }

               }

            }

         }

//      }         This test (see above) is not necessary under Java

//   set up the final bidiagonal matrix of order m

      m = Math.min(p,n+1);

//   There is a bug in the 1.0.2 Java compiler that
//   forces me to skimp on the number of local
//   variables.  This is the cause of the following changes:

//      nctp1 = nct + 1;
//      nrtp1 = nrt + 1;

      if (nct < p) s[nct+1] = x[nct+1][nct+1];
      if (n < m) s[m] = 0.0;
      if (nrt+1 < m) e[nrt+1] = x[nrt+1][m];

      e[m] = 0.0;

//   if required, generate u

      if (wantu) {

//         if (ncu >= nct+1) {        This test is not necessary under Java.
//                                    The loop will be skipped if ncu < nct+1.

            for (j = nct+1; j <= ncu; j++) {

               for (i = 1; i <= n; i++) {

                  u[i][j] = 0.0;

               }

               u[j][j] = 1.0;

            }

//         }           This test is not necessary under Java.  See above.

//         if (nct >= 1) {            This test is not necessary under Java.
//                                    The loop will be skipped if nct < 1.

            for (ll = 1; ll <= nct; ll++) {

               l = nct - ll + 1;

               if (s[l] != 0.0) {

                  lp1 = l + 1;

//                  if (ncu >= lp1) {   Not necessary under Java.
//                                      Loop will be skipped if ncu < lp1.

                     for (j = lp1; j <= ncu; j++) {

                        t = -Blas_f77.coldot_f77(n-l+1,u,l,l,j)/u[l][l];
                        Blas_f77.colaxpy_f77(n-l+1,t,u,l,l,j);

                     }

//                  }   Not necessary under Java.  See above.

                  Blas_f77.colscal_f77(n-l+1,-1.0,u,l,l);
                  u[l][l]++;
                  lm1 = l - 1;

//                  if (lm1 >= 1) {   Not necessary under Java.
//                                    Loop will be skipped if lm1 < 1.

                     for (i = 1; i <= lm1; i++) {

                        u[i][l] = 0.0;

                     }

//                  }   Not necessary under Java.  See above.

               } else {

                  for (i = 1; i <= n; i++) {

                     u[i][l] = 0.0;

                  }

                  u[l][l] = 1.0;

               }

            }

//         }      This test is not necessary under Java.  See above.

      }

//   if it is required, generate v

      if (wantv) {

         for (ll = 1; ll <= p; ll++) {

            l = p - ll + 1;
            lp1 = l + 1;

            if ((l <= nrt) && (e[l] != 0.0)) {

               for (j = lp1; j <= p; j++) {

                  t = -Blas_f77.coldot_f77(p-l,v,lp1,l,j)/v[lp1][l];
                  Blas_f77.colaxpy_f77(p-l,t,v,lp1,l,j);

               }

            }

            for (i = 1; i <= p; i++) {

               v[i][l] = 0.0;

            }

            v[l][l] = 1.0;

         }

      }

//   main iteration loop for the singular values

      mm = m;
      iter = 0;

      while (true) {

//   quit if all of the singular values have been found

         if (m == 0) return;

//   if too many iterations have been performed, 
//   set flag and return     

         if (iter >= maxit) {

            throw new SVDCException(m);

         }

/*

   This section of the program inspects for
   negligible elements in the s and e arrays.
   On completion the variables kase and l are 
   set as follows:

      kase = 1     If s[m] and e[l-1] are negligible and l < m
      kase = 2     If s[l] is negligible and l < m
      kase = 3     If e[l-1] is negligible, l < m, and
                   s[l], ..., s[m] are not negligible (QR step)
      kase = 4     If e[m-1] is negligible (convergence)

*/


         for (ll = 1; ll <= m; ll++) {

            l = m - ll;

            if (l == 0) break;

            test = Math.abs(s[l]) + Math.abs(s[l+1]);
            ztest = test + Math.abs(e[l]);

            if (ztest == test) {

               e[l] = 0.0;
               break;

            }

         }

         
         if (l == m - 1) {

            kase = 4;

         } else {

            lp1 = l + 1;
            mp1 = m + 1;

            for (lls = lp1; lls <= mp1; lls++) {

               ls = m - lls + lp1;
               if (ls == l) break;

               test = 0.0;
               if (ls != m)  test += Math.abs(e[ls]);
               if (ls != l+1) test += Math.abs(e[ls-1]);

               ztest = test + Math.abs(s[ls]);

               if (ztest == test) {

                  s[ls] = 0.0;
                  break;

               }

            }

            if (ls == l) {

               kase = 3;

            } else if (ls == m) {

               kase = 1;

            } else {

               kase = 2;
               l = ls;

            }

         }

         l++;

//   perform the task indicated by kase

         switch (kase) {

            case 1:

//   deflate negligible s[m]

//   There is a bug in the 1.0.2 Java compiler that
//   forces me to skimp on the number of local
//   variables.  This is the cause of the following changes:

//               mm1 = m - 1;
               f = e[m-1];
               e[m-1] = 0.0;

               for (kk = l; kk <= m-1; kk++) {

                  k = (m-1) - kk + l;
                  t1 = s[k];

//   Although objects are passed by reference, primitive types
//   (e.g., doubles, ints, ...) are passed by value.  Thus
//   rotvec is needed.

                  rotvec[1] = t1;
                  rotvec[2] = f;
                  Blas_f77.drotg_f77(rotvec);
                  t1 = rotvec[1];
                  f  = rotvec[2];
                  cs = rotvec[3];
                  sn = rotvec[4];

                  s[k] = t1;

                  if (k != l) {

                     f = -sn*e[k-1];
                     e[k-1] *= cs;

                  }

                  if (wantv) Blas_f77.colrot_f77(p,v,k,m,cs,sn);

               }

               break;

            case 2:

//   split at negligible s[l]               

               f = e[l-1];
               e[l-1] = 0.0;

               for (k = l; k <= m; k++) {

                  t1 = s[k];

//   Although objects are passed by reference, primitive types
//   (e.g., doubles, ints, ...) are passed by value.  Thus
//   rotvec is needed.

                  rotvec[1] = t1;
                  rotvec[2] = f;
                  Blas_f77.drotg_f77(rotvec);
                  t1 = rotvec[1];
                  f  = rotvec[2];
                  cs = rotvec[3];
                  sn = rotvec[4];

                  s[k] = t1;

                  f = -sn*e[k];

                  e[k] *= cs;

                  if (wantu) Blas_f77.colrot_f77(n,u,k,l-1,cs,sn);

               }

               break;

            case 3:

//   perform one QR step

//   calculate the shift

//   There is a bug in the 1.0.2 Java compiler that
//   forces me to skimp on the number of local
//   variables.  Otherwise the following would have
//   been shorter:

               scale = Math.max(Math.abs(s[m]),Math.abs(s[m-1]));
               scale = Math.max(Math.abs(e[m-1]),scale);
               scale = Math.max(Math.abs(s[l]),scale);
               scale = Math.max(Math.abs(e[l]),scale);


//   There is a bug in the 1.0.2 Java compiler that
//   forces me to skimp on the number of local
//   variables.  This is the cause of the following changes:

//               sm = s[m]/scale;

//               smm1 = s[m-1]/scale;
//               emm1 = e[m-1]/scale;
//               sl = s[l]/scale;
//               el = e[l]/scale;

//               b = ((smm1 + sm)*(smm1 - sm) + emm1*emm1)/2.0;
//               c = (sm*emm1)*(sm*emm1);


               b = ((s[m-1] + s[m])*(s[m-1] - s[m]) + 
                     e[m-1]*e[m-1])/(2.0*scale*scale);
               c = (s[m]*e[m-1])*(s[m]*e[m-1])/(scale*scale*scale*scale);

               shift = 0.0;

               if ((b != 0.0) || (c != 0.0)) {

                  shift = Math.sqrt(b*b + c);
                  if (b < 0.0) shift = -shift;
                  shift = c/(b + shift);

               }

//               f = (sl + sm)*(sl - sm) - shift;
//               g = sl*el;

               f = (s[l] + s[m])*(s[l] - s[m])/(scale*scale) - shift;
               g = s[l]*e[l]/(scale*scale);

//   chase zeros


//   There is a bug in the 1.0.2 Java compiler that
//   forces me to skimp on the number of local
//   variables.  This is the cause of the following changes:

//               mm1 = m - 1;

//               for (k = l; k <= mm1; k++) {

               for (k = l; k <= m - 1; k++) {

//   Although objects are passed by reference, primitive types
//   (e.g., doubles, ints, ...) are passed by value.  Thus
//   rotvec is needed.

                  rotvec[1] = f;
                  rotvec[2] = g;
                  Blas_f77.drotg_f77(rotvec);
                  f = rotvec[1];
                  g  = rotvec[2];
                  cs = rotvec[3];
                  sn = rotvec[4];

                  if (k != l) e[k-1] = f;
                  f = cs*s[k] + sn*e[k];
                  e[k] = cs*e[k] - sn*s[k];
                  g = sn*s[k+1];
                  s[k+1] *= cs;
                  
                  if (wantv) Blas_f77.colrot_f77(p,v,k,k+1,cs,sn);

//   Although objects are passed by reference, primitive types
//   (e.g., doubles, ints, ...) are passed by value.  Thus
//   rotvec is needed.

                  rotvec[1] = f;
                  rotvec[2] = g;
                  Blas_f77.drotg_f77(rotvec);
                  f = rotvec[1];
                  g  = rotvec[2];
                  cs = rotvec[3];
                  sn = rotvec[4];

                  s[k] = f;
                  f = cs*e[k] + sn*s[k+1];
                  s[k+1] = -sn*e[k] + cs*s[k+1];
                  g = sn*e[k+1];
                  e[k+1] *= cs;

                  if (wantu && (k < n)) Blas_f77.colrot_f77(n,u,k,k+1,cs,sn);

               }

               e[m-1] = f;
               iter++;

               break;

            case 4:

//   convergence

//   make the singular value positive

               if (s[l] < 0.0) {

                  s[l] = -s[l];
                  if (wantv) Blas_f77.colscal_f77(p,-1.0,v,1,l);

               }

//   order the singular value

               while (l != mm) {

                  if (s[l] >= s[l+1]) break;

                  t = s[l];
                  s[l] = s[l+1];
                  s[l+1] = t;

                  if (wantv && (l < p)) Blas_f77.colswap_f77(p,v,l,l+1);

                  if (wantu && (l < n)) Blas_f77.colswap_f77(n,u,l,l+1);

                  l++;

               }

               iter = 0;
               m--;

               break;

         }

      }

   }
                                    
}
(ncu >= nct+1) {        This test is not necessary under Java.
//                             SVDC_j.java                                                                                         0100644 0007456 0000012 00000054020 06347630337 0013333 0                                                                                                    ustar 00steve                           staff                           0000040 0000007                                                                                                                                                                        /*
    SVDC_j.java copyright claim:

    This software is based on public domain LINPACK routines.
    It was translated from FORTRAN to Java by a US government employee 
    on official time.  Thus this software is also in the public domain.


    The translator's mail address is:

    Steve Verrill 
    USDA Forest Products Laboratory
    1 Gifford Pinchot Drive
    Madison, Wisconsin
    53705


    The translator's e-mail address is:

    steve@ws10.fpl.fs.fed.us


***********************************************************************

DISCLAIMER OF WARRANTIES:

THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND. 
THE TRANSLATOR DOES NOT WARRANT, GUARANTEE OR MAKE ANY REPRESENTATIONS 
REGARDING THE SOFTWARE OR DOCUMENTATION IN TERMS OF THEIR CORRECTNESS, 
RELIABILITY, CURRENTNESS, OR OTHERWISE. THE ENTIRE RISK AS TO 
THE RESULTS AND PERFORMANCE OF THE SOFTWARE IS ASSUMED BY YOU. 
IN NO CASE WILL ANY PARTY INVOLVED WITH THE CREATION OR DISTRIBUTION 
OF THE SOFTWARE BE LIABLE FOR ANY DAMAGE THAT MAY RESULT FROM THE USE 
OF THIS SOFTWARE.

Sorry about that.

***********************************************************************


History:

Date        Translator        Changes

3/3/97      Steve Verrill     Translated
6/6/97                        Java/C style indexing

*/


package linear_algebra;

import linear_algebra.*;


/**
*
*<p>
*This class contains the LINPACK DSVDC (singular value
*decomposition) routine.
*
*<p>
*<b>IMPORTANT:</b>  The "_j" suffixes indicate that these routines use
*Java/C style indexing.  For example, you will see
*<pre>
*   for (i = 0; i < n; i++)
*</pre>
*rather than
*<pre>
*   for (i = 1; i <= n; i++)
*</pre>
*To use the "_j" routines you will have
*to fill elements 0 through n - 1 rather than elements 1 through n.
*Versions of these programs that use FORTRAN style indexing 
*are also available.  They end with the suffix "_f77".
*
*<p>
*This class was translated by a statistician from FORTRAN versions of 
*the LINPACK routines.  It is NOT an official translation.  When 
*public domain Java numerical analysis routines become available 
*from the people who produce LAPACK, then <b>THE CODE PRODUCED
*BY THE NUMERICAL ANALYSTS SHOULD BE USED</b>.
*
*<p>
*Meanwhile, if you have suggestions for improving this
*code, please contact Steve Verrill at steve@ws10.fpl.fs.fed.us.
*
*@author Steve Verrill
*@version .5 --- June 6, 1997
* 
*/

public class SVDC_j extends Object {

/**
*
*<p>
*This method decomposes a n by p 
*matrix X into a product UDV&#180 where ...
*For details, see the comments in the code.
*This method is a translation from FORTRAN to Java 
*of the LINPACK subroutine DSVDC.  
*In the LINPACK listing DSVDC is attributed to G.W. Stewart
*with a date of 3/19/79.
*
*Translated by Steve Verrill, March 3, 1997.
*
*@param  x     The matrix to be decomposed
*@param  n     The number of rows of X
*@param  p     The number of columns of X
*@param  s     The singular values of X
*@param  e     See the documentation in the code
*@param  u     The left singular vectors
*@param  v     The right singular vectors
*@param  work  A work array
*@param  job   See the documentation in the code
*@param  info  See the documentation in the code
*
*/



   public static void dsvdc_j (double x[][], int n, int p, double s[],
                                 double e[], double u[][], double v[][], 
                                 double work[], int job) 
                                 throws SVDCException {


/*

Here is a copy of the LINPACK documentation (from the SLATEC version):


C***BEGIN PROLOGUE  DSVDC
C***DATE WRITTEN   790319   (YYMMDD)
C***REVISION DATE  861211   (YYMMDD)
C***CATEGORY NO.  D6
C***KEYWORDS  LIBRARY=SLATEC(LINPACK),
C             TYPE=DOUBLE PRECISION(SSVDC-S DSVDC-D CSVDC-C),
C             LINEAR ALGEBRA,MATRIX,SINGULAR VALUE DECOMPOSITION
C***AUTHOR  STEWART, G. W., (U. OF MARYLAND)
C***PURPOSE  Perform the singular value decomposition of a d.p. NXP
C            matrix.
C***DESCRIPTION
C
C     DSVDC is a subroutine to reduce a double precision NxP matrix X
C     by orthogonal transformations U and V to diagonal form.  The
C     diagonal elements S(I) are the singular values of X.  The
C     columns of U are the corresponding left singular vectors,
C     and the columns of V the right singular vectors.
C
C     On Entry
C
C         X         DOUBLE PRECISION(LDX,P), where LDX .GE. N.
C                   X contains the matrix whose singular value
C                   decomposition is to be computed.  X is
C                   destroyed by DSVDC.
C
C         LDX       INTEGER.
C                   LDX is the leading dimension of the array X.
C
C         N         INTEGER.
C                   N is the number of rows of the matrix X.
C
C         P         INTEGER.
C                   P is the number of columns of the matrix X.
C
C         LDU       INTEGER.
C                   LDU is the leading dimension of the array U.
C                   (See below).
C
C         LDV       INTEGER.
C                   LDV is the leading dimension of the array V.
C                   (See below).
C
C         WORK      DOUBLE PRECISION(N).
C                   WORK is a scratch array.
C
C         JOB       INTEGER.
C                   JOB controls the computation of the singular
C                   vectors.  It has the decimal expansion AB
C                   with the following meaning
C
C                        A .EQ. 0    do not compute the left singular
C                                  vectors.
C                        A .EQ. 1    return the N left singular vectors
C                                  in U.
C                        A .GE. 2    return the first MIN(N,P) singular
C                                  vectors in U.
C                        B .EQ. 0    do not compute the right singular
C                                  vectors.
C                        B .EQ. 1    return the right singular vectors
C                                  in V.
C
C     On Return
C
C         S         DOUBLE PRECISION(MM), where MM=MIN(N+1,P).
C                   The first MIN(N,P) entries of S contain the
C                   singular values of X arranged in descending
C                   order of magnitude.
C
C         E         DOUBLE PRECISION(P).
C                   E ordinarily contains zeros.  However see the
C                   discussion of INFO for exceptions.
C
C         U         DOUBLE PRECISION(LDU,K), where LDU .GE. N.
C                   If JOBA .EQ. 1, then K .EQ. N.
C                   If JOBA .GE. 2, then K .EQ. MIN(N,P).
C                   U contains the matrix of right singular vectors.
C                   U is not referenced if JOBA .EQ. 0.  If N .LE. P
C                   or if JOBA .EQ. 2, then U may be identified with X
C                   in the subroutine call.
C
C         V         DOUBLE PRECISION(LDV,P), where LDV .GE. P.
C                   V contains the matrix of right singular vectors.
C                   V is not referenced if JOB .EQ. 0.  If P .LE. N,
C                   then V may be identified with X in the
C                   subroutine call.
C
C         INFO      INTEGER.
C                   The singular values (and their corresponding
C                   singular vectors) S(INFO+1),S(INFO+2),...,S(M)
C                   are correct (here M=MIN(N,P)).  Thus if
C                   INFO .EQ. 0, all the singular values and their
C                   vectors are correct.  In any event, the matrix
C                   B = TRANS(U)*X*V is the bidiagonal matrix
C                   with the elements of S on its diagonal and the
C                   elements of E on its super-diagonal (TRANS(U)
C                   is the transpose of U).  Thus the singular
C                   values of X and B are the same.


   For the Java version, info
   is returned as an argument to SVDCException
   if the decomposition fails.


C
C     LINPACK.  This version dated 03/19/79 .
C     G. W. Stewart, University of Maryland, Argonne National Lab.
C
C     DSVDC uses the following functions and subprograms.
C
C     External DROT
C     BLAS DAXPY,DDOT,DSCAL,DSWAP,DNRM2,DROTG
C     Fortran DABS,DMAX1,MAX0,MIN0,MOD,DSQRT
C***REFERENCES  DONGARRA J.J., BUNCH J.R., MOLER C.B., STEWART G.W.,
C                 *LINPACK USERS  GUIDE*, SIAM, 1979.
C***ROUTINES CALLED  DAXPY,DDOT,DNRM2,DROT,DROTG,DSCAL,DSWAP
C***END PROLOGUE  DSVDC

*/

//   There is a bug in the 1.0.2 Java compiler that
//   forces me to skimp on the number of local
//   variables.  This is the cause of the following changes:

//      double t,b,c,cs,el,emm1,f,g,scale1,scale2,scale,shift,sl,sm,
//             sn,smm1,t1,test,ztest,fac;

//      int i,iter,j,jobu,k,kase,kk,l,ll,lls,lm1,lp1,ls,lu,
//          m,maxit,mm,mm1,mp1,nct,nctp1,ncu,nrt,nrtp1;

      double t,b,c,cs,f,g,scale,shift,
             sn,t1,test,ztest;

      double rotvec[] = new double[4];

      int i,iter,j,jobu,k,kase,kk,l,ll,lls,lm1,lp1,ls,lu,
          m,maxit,mm,mp1,nct,ncu,nrt;

      int jm1,lsm1,km1,mm1;

      boolean wantu,wantv;

//   Java requires that all local variables be initialized before they
//   are used.  This is the reason for the initialization
//   line below:

      ls = 0;

//   set the maximum number of iterations

      maxit = 30;

//   determine what is to be computed

      wantu = false;
      wantv = false;

      jobu = (job%100)/10;

      ncu = n;

      if (jobu > 1) ncu = Math.min(n,p);
      if (jobu != 0) wantu = true;
      if ((job%10) != 0) wantv = true;

//   reduce x to bidiagonal form, storing the diagonal elements
//   in s and the super-diagonal elements in e

      nct = Math.min(n-1,p);
      nrt = Math.max(0,Math.min(p-2,n));
      lu = Math.max(nct,nrt);

//      if (lu >= 1) {       This test is not necessary under Java.
//                           The loop will be skipped if lu < 1 = the
//                           starting value of l.

         for (l = 1; l <= lu; l++) {

            lm1 = l - 1;

            lp1 = l + 1;

            if (l <= nct) {

//   compute the transformation for the l-th column and
//   place the l-th diagonal in s[lm1]

               s[lm1] = Blas_j.colnrm2_j(n-l+1,x,lm1,lm1);

               if (s[lm1] != 0.0) {

                  if (x[lm1][lm1] != 0.0) s[lm1] = Blas_j.sign_j(s[lm1],x[lm1][lm1]);
                  Blas_j.colscal_j(n-l+1,1.0/s[lm1],x,lm1,lm1);
                  x[lm1][lm1]++;

               }

               s[lm1] = -s[lm1];

            }

            for (j = l; j < p; j++) {

               if ((l <= nct) && (s[lm1] != 0.0)) {

//   apply the transformation

                  t = -Blas_j.coldot_j(n-l+1,x,lm1,lm1,j)/x[lm1][lm1];
                  Blas_j.colaxpy_j(n-l+1,t,x,lm1,lm1,j);

               }

//   place the l-th row of x into e for the
//   subsequent calculation of the row transformation

               e[j] = x[lm1][j];

            }


            if (wantu && (l <= nct)) {

//   place the transformation in u for subsequent 
//   back multiplication

               for (i = lm1; i < n; i++) {

                  u[i][lm1] = x[i][lm1];

               }

            }

            if (l <= nrt) {

//   compute the l-th row transformation and place the
//   l-th super-diagonal in e[lm1]

               e[lm1] = Blas_j.dnrm2p_j(p-l,e,l);

               if (e[lm1] != 0.0) {

                  if (e[l] != 0.0) e[lm1] = Blas_j.sign_j(e[lm1],e[l]);
                  Blas_j.dscalp_j(p-l,1.0/e[lm1],e,l);
                  e[l]++;

               }

               e[lm1] = -e[lm1];

               if ((lp1 <= n) && (e[lm1] != 0.0)) {

//   apply the transformation

                  for (i = l; i < n; i++) {

                     work[i] = 0.0;

                  }

                  for (j = l; j < p; j++) {

                     Blas_j.colvaxpy_j(n-l,e[j],x,work,l,j);

                  }

                  for (j = l; j < p; j++) {

                     Blas_j.colvraxpy_j(n-l,-e[j]/e[l],work,x,l,j);

                  }

               }

               if (wantv) {

//   place the transformation in v for subsequent
//   back multiplication

                  for (i = l; i < p; i++) {

                     v[i][lm1] = e[i];

                  }

               }

            }

         }

//      }         This test (see above) is not necessary under Java

//   set up the final bidiagonal matrix of order m

      m = Math.min(p,n+1);

      if (nct < p) s[nct] = x[nct][nct];
      if (n < m) s[m-1] = 0.0;
      if (nrt+1 < m) e[nrt] = x[nrt][m-1];

      e[m-1] = 0.0;

//   if required, generate u

      if (wantu) {


         for (j = nct; j < ncu; j++) {

            for (i = 0; i < n; i++) {

               u[i][j] = 0.0;

            }

            u[j][j] = 1.0;

         }


//         if (nct >= 1) {            This test is not necessary under Java.
//                                    The loop will be skipped if nct < 1.


            for (ll = 1; ll <= nct; ll++) {

               l = nct - ll + 1;
               lm1 = l - 1;

               if (s[lm1] != 0.0) {

//                  lp1 = l + 1;


                  for (j = l; j < ncu; j++) {

                     t = -Blas_j.coldot_j(n-l+1,u,lm1,lm1,j)/u[lm1][lm1];
                     Blas_j.colaxpy_j(n-l+1,t,u,lm1,lm1,j);

                  }


                  Blas_j.colscal_j(n-l+1,-1.0,u,lm1,lm1);
                  u[lm1][lm1]++;


                  for (i = 0; i < lm1; i++) {

                     u[i][lm1] = 0.0;

                  }


               } else {

                  for (i = 0; i < n; i++) {

                     u[i][lm1] = 0.0;

                  }

                  u[lm1][lm1] = 1.0;

               }

            }

//         }      This test is not necessary under Java.  See above.

      }

//   if it is required, generate v

      if (wantv) {

         for (ll = 1; ll <= p; ll++) {

            l = p - ll + 1;
            lm1 = l - 1;

            if ((l <= nrt) && (e[lm1] != 0.0)) {

               for (j = l; j < p; j++) {

                  t = -Blas_j.coldot_j(p-l,v,l,lm1,j)/v[l][lm1];
                  Blas_j.colaxpy_j(p-l,t,v,l,lm1,j);

               }

            }

            for (i = 0; i < p; i++) {

               v[i][lm1] = 0.0;

            }

            v[lm1][lm1] = 1.0;

         }

      }

//   main iteration loop for the singular values

      mm = m;
      iter = 0;

      while (true) {

//   quit if all of the singular values have been found

         if (m == 0) return;

//   if too many iterations have been performed, 
//   set flag and return     

         if (iter >= maxit) {

            throw new SVDCException(m);

         }

/*

   This section of the program inspects for
   negligible elements in the s and e arrays.
   On completion the variables kase and l are 
   set as follows:

      kase = 1     If s[m] and e[l-1] are negligible and l < m
      kase = 2     If s[l] is negligible and l < m
      kase = 3     If e[l-1] is negligible, l < m, and
                   s[l], ..., s[m] are not negligible (QR step)
      kase = 4     If e[m-1] is negligible (convergence)

    The material above is for FORTRAN style indexing.  Subtract
    indices by 1 for Java style indexing.

*/


         for (ll = 1; ll <= m; ll++) {

            l = m - ll;
            lm1 = l - 1;

            if (l == 0) break;

            test = Math.abs(s[lm1]) + Math.abs(s[l]);
            ztest = test + Math.abs(e[lm1]);

            if (ztest == test) {

               e[lm1] = 0.0;
               break;

            }

         }

         
         if (l == m - 1) {

            kase = 4;

         } else {

            lp1 = l + 1;
            mp1 = m + 1;

            for (lls = lp1; lls <= mp1; lls++) {

               ls = m - lls + lp1;
               lsm1 = ls - 1;
               if (ls == l) break;

               test = 0.0;
               if (ls != m)  test += Math.abs(e[lsm1]);
               if (ls != l+1) test += Math.abs(e[lsm1-1]);

               ztest = test + Math.abs(s[lsm1]);

               if (ztest == test) {

                  s[lsm1] = 0.0;
                  break;

               }

            }

            if (ls == l) {

               kase = 3;

            } else if (ls == m) {

               kase = 1;

            } else {

               kase = 2;
               l = ls;

            }

         }

         l++;

         lm1 = l - 1;

         mm1 = m - 1;

//   perform the task indicated by kase

         switch (kase) {

            case 1:

//   deflate negligible s[m]

//   There is a bug in the 1.0.2 Java compiler that
//   forces me to skimp on the number of local
//   variables.  This is the cause of the following changes:

//               mm1 = m - 1;

               f = e[m-2];
               e[m-2] = 0.0;

               for (kk = l; kk <= mm1; kk++) {

                  k = (mm1) - kk + l;
                  km1 = k - 1;

                  t1 = s[km1];

//   Although objects are passed by reference, primitive types
//   (e.g., doubles, ints, ...) are passed by value.  Thus
//   rotvec is needed.

                  rotvec[0] = t1;
                  rotvec[1] = f;
                  Blas_j.drotg_j(rotvec);
                  t1 = rotvec[0];
                  f  = rotvec[1];
                  cs = rotvec[2];
                  sn = rotvec[3];

                  s[km1] = t1;

                  if (k != l) {

                     f = -sn*e[k-2];
                     e[k-2] *= cs;

                  }

                  if (wantv) Blas_j.colrot_j(p,v,km1,mm1,cs,sn);

               }

               break;

            case 2:

//   split at negligible s[lm1]               

               f = e[l-2];
               e[l-2] = 0.0;

               for (k = l; k <= m; k++) {

                  km1 = k - 1;

                  t1 = s[km1];

//   Although objects are passed by reference, primitive types
//   (e.g., doubles, ints, ...) are passed by value.  Thus
//   rotvec is needed.

                  rotvec[0] = t1;
                  rotvec[1] = f;
                  Blas_j.drotg_j(rotvec);
                  t1 = rotvec[0];
                  f  = rotvec[1];
                  cs = rotvec[2];
                  sn = rotvec[3];

                  s[km1] = t1;

                  f = -sn*e[km1];

                  e[km1] *= cs;

                  if (wantu) Blas_j.colrot_j(n,u,km1,l-2,cs,sn);

               }

               break;

            case 3:

//   perform one QR step

//   calculate the shift

//   There is a bug in the 1.0.2 Java compiler that
//   forces me to skimp on the number of local
//   variables.  Otherwise the following would have
//   been shorter:

               scale = Math.max(Math.abs(s[mm1]),Math.abs(s[m-2]));
               scale = Math.max(Math.abs(e[m-2]),scale);
               scale = Math.max(Math.abs(s[lm1]),scale);
               scale = Math.max(Math.abs(e[lm1]),scale);


//   There is a bug in the 1.0.2 Java compiler that
//   forces me to skimp on the number of local
//   variables.  This is the cause of the following changes:

//               sm = s[m]/scale;

//               smm1 = s[mm1]/scale;
//               emm1 = e[mm1]/scale;
//               sl = s[l]/scale;
//               el = e[l]/scale;

//               b = ((smm1 + sm)*(smm1 - sm) + emm1*emm1)/2.0;
//               c = (sm*emm1)*(sm*emm1);


               b = ((s[m-2] + s[mm1])*(s[m-2] - s[mm1]) + 
                     e[m-2]*e[m-2])/(2.0*scale*scale);
               c = (s[mm1]*e[m-2])*(s[mm1]*e[m-2])/(scale*scale*scale*scale);

               shift = 0.0;

               if ((b != 0.0) || (c != 0.0)) {

                  shift = Math.sqrt(b*b + c);
                  if (b < 0.0) shift = -shift;
                  shift = c/(b + shift);

               }

//               f = (sl + sm)*(sl - sm) - shift;
//               g = sl*el;

               f = (s[lm1] + s[mm1])*(s[lm1] - s[mm1])/(scale*scale) - shift;
               g = s[lm1]*e[lm1]/(scale*scale);

//   chase zeros


//   There is a bug in the 1.0.2 Java compiler that
//   forces me to skimp on the number of local
//   variables.  This is the cause of the following changes:

//               mm1 = m - 1;

//               for (k = l; k <= mm1; k++) {

               for (k = l; k <= mm1; k++) {

                  km1 = k - 1;

//   Although objects are passed by reference, primitive types
//   (e.g., doubles, ints, ...) are passed by value.  Thus
//   rotvec is needed.

                  rotvec[0] = f;
                  rotvec[1] = g;
                  Blas_j.drotg_j(rotvec);
                  f = rotvec[0];
                  g  = rotvec[1];
                  cs = rotvec[2];
                  sn = rotvec[3];

                  if (k != l) e[k-2] = f;
                  f = cs*s[km1] + sn*e[km1];
                  e[km1] = cs*e[km1] - sn*s[km1];
                  g = sn*s[k];
                  s[k] *= cs;
                  
                  if (wantv) Blas_j.colrot_j(p,v,km1,k,cs,sn);

//   Although objects are passed by reference, primitive types
//   (e.g., doubles, ints, ...) are passed by value.  Thus
//   rotvec is needed.

                  rotvec[0] = f;
                  rotvec[1] = g;
                  Blas_j.drotg_j(rotvec);
                  f = rotvec[0];
                  g  = rotvec[1];
                  cs = rotvec[2];
                  sn = rotvec[3];

                  s[km1] = f;
                  f = cs*e[km1] + sn*s[k];
                  s[k] = -sn*e[km1] + cs*s[k];
                  g = sn*e[k];
                  e[k] *= cs;

                  if (wantu && (k < n)) Blas_j.colrot_j(n,u,km1,k,cs,sn);

               }

               e[m-2] = f;
               iter++;

               break;

            case 4:

//   convergence

//   make the singular value positive

               if (s[lm1] < 0.0) {

                  s[lm1] = -s[lm1];
                  if (wantv) Blas_j.colscal_j(p,-1.0,v,0,lm1);

               }

//   order the singular value

               while (l != mm) {

                  if (s[lm1] >= s[l]) break;

                  t = s[lm1];
                  s[lm1] = s[l];
                  s[l] = t;

                  if (wantv && (l < p)) Blas_j.colswap_j(p,v,lm1,l);

                  if (wantu && (l < n)) Blas_j.colswap_j(n,u,lm1,l);

                  l++;

               }

               iter = 0;
               m--;

               break;

         }

      }

   }
                                    
}
     This test (see above) is not necessary under Java

//   set up the final bidiagonal matrix of order m

      m = Math.min(p,n+1);

      if (nct < p) s[nct] = x[nct][nct];
      if (n < m) s[m-1] = 0.0;
      if (nrt+1 < m) e[nrt] = x[nrt][m-1];

      e[m-1] = 0.0;

//   if required, generate u

      if (wantu) {


         for (j = nct; j < ncu; j++) {

            for (i = 0; i < n; i++) {

               u[i][j] = 0.0;

            }

            u[j][j] = 1.0;

         }


//    Triangular.java                                                                                     0100644 0007456 0000012 00000016230 06251367234 0014371 0                                                                                                    ustar 00steve                           staff                           0000040 0000007                                                                                                                                                                        /*
    Triangular.java copyright claim:

    Copyright (C) 1996 by Steve Verrill.

    This class is free software; you can redistribute it and/or 
    modify it under the terms of version 2 of the GNU General 
    Public License as published by the Free Software Foundation.

    This class are distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with these programs in the file COPYING; if not, write to 
    the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, 
    MA 02139, USA.

    The author's mail address is:

    Steve Verrill 
    USDA Forest Products Laboratory
    1 Gifford Pinchot Drive
    Madison, Wisconsin
    53705

    The author's e-mail address is:

    steve@ws10.fpl.fs.fed.us


***********************************************************************

DISCLAIMER OF WARRANTIES:

THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND. 
THE AUTHOR DOES NOT WARRANT, GUARANTEE OR MAKE ANY REPRESENTATIONS 
REGARDING THE SOFTWARE OR DOCUMENTATION IN TERMS OF THEIR CORRECTNESS, 
RELIABILITY, CURRENTNESS, OR OTHERWISE. THE ENTIRE RISK AS TO 
THE RESULTS AND PERFORMANCE OF THE SOFTWARE IS ASSUMED BY YOU. 
IN NO CASE WILL ANY PARTY INVOLVED WITH THE CREATION OR DISTRIBUTION 
OF THE SOFTWARE BE LIABLE FOR ANY DAMAGE THAT MAY RESULT FROM THE USE 
OF THIS SOFTWARE.

Sorry about that.

***********************************************************************

History:

Date        Author            Changes

11/30/96    Steve Verrill     Created

*/


package linear_algebra;


/**
*
*<p>
*This class contains:
*<ol>
*<li> methods to solve Ly = b and Ux = y
*where L is a full rank lower triangular matrix and
*U is a full rank upper triangular matrix.
*<li> methods to invert upper and lower
*triangular full rank matrices.
*</ol>
*
*<p>
*This class was written by a statistician rather than
*a numerical analyst.  When public domain Java 
*numerical analysis routines become available from
*numerical analysts (e.g., the people who produce
*LAPACK), then the code produced by the numerical
*analysts should be used.
*
*<p>   
*Meanwhile, if you have suggestions for improving this
*code, please contact Steve Verrill at <a
*href="mailto:steve@ws10.fpl.fs.fed.us">steve@ws10.fpl.fs.fed.us</a>.
*
*@author Steve Verrill
*@version .5 --- November 30, 1996
*
*/


public class Triangular extends Object {

/**
*
*<p>
*This method obtains the solution, y, of the equation
*Ly = b where L is a known full rank lower triangular 
*n by n matrix,
*and b is a known vector of length n.
*
*@param  l[&#32][&#32]  The lower triangular matrix.
*@param  y  The solution vector.
*@param  b  The right hand side of the equation.
*@param  n  The order of l, y, and b.
*@throws NotFullRankException if one or more of the diagonal
*elements of l[&#32][&#32] is zero.
*
*/

   public void solveLower (double l[][], double y[], double b[], int n) 
                    throws NotFullRankException {



      double sum;

      int i,j;
      int err;

      err = 0;

      for (i = 0; i < n; i++) {

         if (l[i][i] == 0.0) {

            System.out.println("\nTriangular.solveLower error:" +
            "  Diagonal element " + i +
            " of the L matrix is zero.\n");
            err = 1;

         }

      }

      if (err == 1) {

            throw new NotFullRankException();

      }

      for (i = 0; i < n; i++) {

         sum = 0.0;

         for (j = 0; j < i; j++) {
      
            sum += l[i][j]*y[j];

         }

         y[i] = (b[i] - sum)/l[i][i];

      }

      return;

   }

/**
*
*This method obtains the solution, x, of the equation
*Ux = y where U is a known full rank upper triangular 
*n by n matrix,
*and y is a known vector of length n.
*
*@param  u[&#32][&#32]  The upper triangular matrix.
*@param  x  The solution vector.
*@param  y  The right hand side of the equation.
*@param  n  The order of u, x, and y.
*@throws NotFullRankException if one or more of the diagonal
*elements of u[&#32][&#32] is zero.
*
*/

   public void solveUpper (double u[][], double x[], double y[], int n) 
                   throws NotFullRankException {



      double sum;

      int i,j;
      int err;

      err = 0;

      for (i = 0; i < n; i++) {

         if (u[i][i] == 0.0) {

            System.out.println("\nTriangular.solveUpper error:" +
            "  Diagonal element " + i +
            " of the U matrix is zero.\n");
            err = 1;

         }

      }

      if (err == 1) {

            throw new NotFullRankException();

      }

      for (i = n - 1; i > -1; i--) {

         sum = 0.0;

         for (j = i + 1; j < n; j++) {
      
            sum += u[i][j]*x[j];

         }

         x[i] = (y[i] - sum)/u[i][i];

      }

      return;

   }

/**
*
*This method obtains the inverse of a lower
*triangular n by n matrix L.  L must have non-zero
*diagonal elements.  On exit L is replaced by its inverse.
*
*@param  l[&#32][&#32]  The lower triangular matrix.
*@param  n  The order of l.
*@throws NotFullRankException if one or more of the diagonal
*elements of l[&#32][&#32] is zero.
*
*/

   public void invertLower (double l[][], int n)
                    throws NotFullRankException {



      double sum;

      int i,j,k;
      int err;

      err = 0;

      for (i = 0; i < n; i++) {

         if (l[i][i] == 0.0) {

            System.out.println("\nTriangular.invertLower error:" +
            "  Diagonal element " + i +
            " of the L matrix is zero.\n");
            err = 1;

         }

      }

      if (err == 1) {

            throw new NotFullRankException();

      }

      for (j = 0; j < n; j++) {

         l[j][j] = 1.0/l[j][j];

         for (i = j + 1; i < n; i++) {

            sum = 0.0;

            for (k = j; k < i; k++) {
      
               sum -= l[i][k]*l[k][j];

            }

            l[i][j] = sum/l[i][i];

         }

      }

      return;

   }

/**
*
*This method obtains the inverse of an upper
*triangular n by n matrix U.  U must have non-zero
*diagonal elements.  On exit U is replaced by its inverse.
*
*@param  u[&#32][&#32]  The upper triangular matrix.
*@param  n  The order of u.
*@throws NotFullRankException if one or more of the diagonal
*elements of u[&#32][&#32] is zero.
*
*/

   public void invertUpper (double u[][], int n)
                    throws NotFullRankException {

      double sum;

      int i,j,k;
      int err;

      err = 0;

      for (i = 0; i < n; i++) {

         if (u[i][i] == 0.0) {

            System.out.println("\nTriangular.invertUpper error:" +
            "  Diagonal element " + i +
            " of the U matrix is zero.\n");
            err = 1;

         }

      }

      if (err == 1) {

            throw new NotFullRankException();

      }


      for (j = n - 1; j > -1; j--) {

         u[j][j] = 1.0/u[j][j];

         for (i = j - 1; i > -1; i--) {

            sum = 0.0;

            for (k = j; k > i; k--) {
      
               sum -= u[i][k]*u[k][j];

            }

            u[i][j] = sum/u[i][i];

         }

      }

      return;

   }

}
angular 
*n by n matrix,
*and b is a known vector of length n.
*
*@param  l[&#32][&#32]  The lower triangular matrix.
*@param  y  The solution vector.
*@param  b  The right hand side of the equation.
*@param  n  The order of l, y, and b.
*@throws NotFullRankException if one or more of the diagonal
*elements of l[&#32][&#32] is zero.
*
*/

   public void solv                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                