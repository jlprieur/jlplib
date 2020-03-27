/**************************************************************************
* "jlp_gdev_images.cpp"
*
* Definition of the members of the JLP_GDev class
* to load and plot images
*
* JLP
* Version 27/07/2017
**************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "jlp_gdev.h"
#include "jlp_fitsio.h"  // JLP_RDFITS_2D()

/* Contained here
*/

/*************************************************************
* Load plot the settings that will be used by PlotDisplay() 
* (generally called by external routines)
*
* INPUT:
*   iplane: number of the image plane of 3D data (from 1 to nz1)
*************************************************************/
int JLP_GDev::Load2DFitsImage(char *filename0, const int iplane0)
{
char comments0[80], errmess0[200];
int status, nx0, ny0, nz0;

// Load a new image or image plane in FITS format
status = JLP_RDFITS_2D_dble(&dble_image_1, &nx0, &ny0, &nz0, iplane0, 
                            filename0, comments0, errmess0);

// Private variables for Images:
// dble_image_1: 2D array with the data contained in the FITS file
// nx_1, ny_1, nz_1: size of the full data cube
// iplane_1: index of image plane to be loaded from 1 to nz (in case of 3D data)
// fits_filename1[128]

if(status == 0) {
  strcpy(fits_filename_1, filename0);
  iplane_1 = iplane0;
  nx_1 = nx0;
  ny_1 = ny0;
  nz_1 = nz0;
  } else {
  fprintf(stderr, "Images_Load2DFitsImage/JLP_RDFITS_2D_dble/Error reading %s (with iplane=%d)\n",
          filename0, iplane0);
  strcpy(fits_filename_1, "");
  iplane_1 = -1;
  nx_1 = 0;
  ny_1 = 0;
  nz_1 = 0;
  }

return(0);
}
/*************************************************************
* Load double image to private array dble_image1
*
*************************************************************/
int JLP_GDev::LoadDbleImage1(double *dble_image0, int nx0, int ny0)
{
int i;
bool change_size0;

change_size0 = (nx0 != nx_1 || ny0 != ny_1);

if(change_size0) {
  if(dble_image_1 != NULL) delete[] dble_image_1;
  nx_1 = nx0;
  ny_1 = ny0;
  dble_image_1 = new double[nx_1 * ny_1];
 }

for(i = 0; i < nx_1 * ny_1; i++) dble_image_1[i] = dble_image0[i];

return(0);
}
