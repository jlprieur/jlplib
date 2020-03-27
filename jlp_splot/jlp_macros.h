/**************************************************************************
* Basic macro definitions
* JLP
* Version 03/05/2013
**************************************************************************/
#ifndef _jlp_macros_   // BOF entry
#define _jlp_macros_
#include <math.h>

/* See also M_PI in math.h ... */
#ifndef PI 
#define PI 3.14159265358979323846 
#endif

typedef unsigned char BYTE;

#ifndef MAXI
#define MAXI(x,y) ((x) > (y) ? (x) : (y))
#endif

#ifndef MINI
#define MINI(x,y) ((x) < (y) ? (x) : (y))
#endif

#ifndef ABS 
#define ABS(x) ((x) > 0. ? (x) : (-(x)))
#endif

#ifndef SQUARE 
#define SQUARE(x) ((x) * (x))
#endif

#ifndef NINT 
#define NINT(x) (int)((x) + 0.5)
#endif

#define hypot(x,y) sqrt(x*x + y*y)

#endif   // EOF sentry
