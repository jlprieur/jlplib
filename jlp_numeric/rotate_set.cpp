/*******************************************************************
* Rotation of an image
* 
* JLP
* Version 06Â/03/2020
*******************************************************************/
#include"stdio.h"
#include"math.h"

/* ----------------------------------------------------------------------
* Rotation of "angle" in radians
*---------------------------------------------------------------------- */
int rotimage(double *ima, double *imarot, int nx, int ny, double angle)
{
int	i, j;
double	ii, jj;
int	x1, x2, y1, y2, dim;
double	za, zb, zc, zd, z1, z2, z;
double	x, y, back_value, *tmp_ima;

// Copy to tmp_ima (to allow in-place rotation with ima=imrot):
tmp_ima = new double[nx * ny];
for (i = 0; i < nx * nx; i++) tmp_ima[i] = ima[i];

dim = nx;

// Compute the mean value of the input image (for the outer parts)
back_value = 0.;
ii = 0.;
 for (j = ny/30; j < ny/10; j++)
  for (i = ny/30; i < nx/10; i++)
   {
   ii++;
   back_value += tmp_ima[i + j*dim];
   }
back_value /= ii;
	
	for (j=0; j<ny; j++)		
// i,j are the array coordinates, after rotation:
	for (i=0; i<nx; i++)
	{
		imarot[i + j*dim] = 0.;
		ii=i-nx/2; jj=j-ny/2;
		x=nx/2 + ii*cos(angle) + jj*sin(angle);
/* line x(th): */
		y=ny/2 - ii*sin(angle) + jj*cos(angle);	
		x1=(int)x; x2=x1+1;
		y1=(int)y; y2=y1+1;
		if ((x1 >= 0) && (y1 >= 0) && (x2 < nx) && (y2 < ny))
		{
// Linear interpolation for the z value:
			za = tmp_ima[x1 + y1*dim];
			zb = tmp_ima[x1 + y2*dim];
			zc = tmp_ima[x2 + y1*dim];
			zd = tmp_ima[x2 + y2*dim];
			z1 = za + (y-y1)*(zb-za);
			z2 = zc + (y-y1)*(zd-zc);
			z = z1 + (x-x1)*(z2-z1);
		}
		else {
                z=back_value;
                }
		
		imarot[i + j * dim] = z;
	}

/* Fill last column */
  i = nx - 1;
  for (j = 0; j < ny; j++) imarot[i + j * dim] = imarot[(i-1) + j * dim];	

/* Fill last line */
  j = ny -1;
  for (i = 0; i < nx; i++) imarot[i + j * dim] = imarot[i + (j-1) * dim];	

delete[] tmp_ima;
return(0);
}
