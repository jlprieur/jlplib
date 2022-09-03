// from http://www.taygeta.com/random/gaussian.html
// Algorithm by Dr. Everett (Skip) Carter, Jr.
// This function generates Gaussian random numbers in pairs (e.g., y1, y2). 
// A function that returns a single random number would only calculate y1 or y2. 
// ranf() is the random number routine, uniformly distributed in [0,1).
#include <math.h>
int random_gauss(float *y1, float *y2)
{
float x1, x2, w;
 
   do {
       x1 = 2.0 * ranf() - 1.0;
       x2 = 2.0 * ranf() - 1.0;
       w = x1 * x1 + x2 * x2;
   } while ( w >= 1.0 );

   w = sqrt( (-2.0 * ln( w ) ) / w );
   *y1 = x1 * w;
   *y2 = x2 * w;
return(0);
}
