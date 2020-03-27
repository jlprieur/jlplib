/***************************************************************************
* JLP_GsegData class
*
* JLP
* Version 27/03/2017
***************************************************************************/
#include "jlp_gseg_data.h"
#include "jlp_gseg_plot_data1.h"    // jlp_gseg_init_plot_data()

/***************************************************************************
* Constructor with parameter file:
***************************************************************************/
JLP_GsegData::JLP_GsegData(JLP_Gseg *jlp_gseg0, JLP_Gsegraf *jlp_gsegraf0)
{

// Save to private variables:
jlp_gseg1 = jlp_gseg0;
jlp_gsegraf1 = jlp_gsegraf0;

// Initialize private variable:
gsegdata_from_paramfile = 1;

InitDataParameters();

InitializeDataVariables();

return;
}
/***************************************************************************
* Constructor with the GSEG_PLOT_DATA structure
* containing all the required parameters
***************************************************************************/
JLP_GsegData::JLP_GsegData(JLP_Gseg *jlp_gseg0, JLP_Gsegraf *jlp_gsegraf0,
                           GSEG_PLOT_DATA *gseg_pltdata0, const int nplots0)
{
int status;

// Save to private variables:
jlp_gseg1 = jlp_gseg0;
jlp_gsegraf1 = jlp_gsegraf0;

// Initialize private variable:
gsegdata_from_paramfile = 0;

InitDataParameters();

InitializeDataVariables();

// Load plot parameters contained in GSEG_PLOT_DATA structures
 status = SetPlotDataParamsFromGsegPlotData(gseg_pltdata0, nplots0);
 if(status) {
  fprintf(stderr, "GsegData/SetPlotDataParamsFromGsegPlotDataError: status = %d\n",
          status);
  }

return;
}
/***************************************************************************
* Create pointers and structures for JLP_GsegData
*
***************************************************************************/
void JLP_GsegData::InitDataParameters()
{
int iplot;
static data_min_max_type data_min_max0, data_min_max1;
int dummy_graphic_dev = 4; // 4 = gsegraf_2d_curves

// Create pointers of structures
 p_data_min_max = &data_min_max0;
 fromfile_data_min_max = &data_min_max1;

// Set pointers to NULL:
 for(iplot = 1; iplot < NPLOTS_MAXI; iplot++) {
   jlp_gseg_init_plot_data(dummy_graphic_dev, &(gseg_plotdata1[iplot]));
   }

// Used in string_get():
 string_get            = NULL;

/* symbol-specification characters for style options */
 strcpy(symbol_string, "ld.cCtTsSiIpPhH+xra");
/* symbol-specification characters for style options */
 strcpy(symbol_string1, "cCtTsSiIpPhH");
/* symbol-specification characters for style options */
 strcpy(symbol_string2, "+xra");
/* color-specification characters for color options */
 strcpy(color_string, "kaswrylqbfmogtnpx");

/* Tabulate color array for colors specified by color characters */
 color_rgba1[0]  = 0x000000FF;   /* k black   (black)        */
 color_rgba1[1]  = 0x808080FF;   /* a gray    (gray50)       */
 color_rgba1[2]  = 0xC0C0C0FF;   /* s silver  (gray75)       */
 color_rgba1[3]  = 0xFFFFFFFF;   /* w white   (white)        */
 color_rgba1[4]  = 0xFF0000FF;   /* r red     (red)          */
 color_rgba1[5]  = 0xFFFF00FF;   /* y yellow  (yellow)       */
 color_rgba1[6]  = 0x00FF00FF;   /* l lime    (green)        */
 color_rgba1[7]  = 0x00FFFFFF;   /* q aqua    (cyan)         */
 color_rgba1[8]  = 0x0000FFFF;   /* b blue    (blue)         */
 color_rgba1[9]  = 0xFF00FFFF;   /* f fuchsia (magenta)      */
 color_rgba1[10] = 0x800000FF;   /* m maroon  (dark red)     */
 color_rgba1[11] = 0x808000FF;   /* o olive   (dark yellow)  */
 color_rgba1[12] = 0x008000FF;   /* g green   (dark green)   */
 color_rgba1[13] = 0x008080FF;   /* t teal    (dark cyan)    */
 color_rgba1[14] = 0x000080FF;   /* n navy    (dark blue)    */
 color_rgba1[15] = 0x800080FF;   /* p purple  (dark magenta) */
 color_rgba1[16] = 0xFFFFFF00;   /* x transparent            */

return;
}
/***************************************************************************
* Initialize variables for JLP_GsegData
***************************************************************************/
int JLP_GsegData::InitDataArraysAndOptions(const int nplots0)
{
int i;

   nplots1 = nplots0;

// Extra lines, ellipses, rectangles (shape data):
   n_extra_lines1 = 0;
   n_extra_ellipses1 = 0;
   n_extra_rectangles1 = 0;
   n_extra_symbols1 = 0;

// Extra text items:
   n_text_items1 = 0;
   for(i = 0; i < MAX_NSHAPES; i++) {
     text_items1[i].text_str = NULL;
     text_items1[i].nlines = 0;
     }

// Extra image:
   extra_image_filename1[0] = '\0';

// Legend:
   n_legend_lines1 = 0;
   legend_str1 = NULL;

return(0);
}
/***************************************************************************
* Initialize variables for JLP_GsegData
***************************************************************************/
int JLP_GsegData::InitOptionsFromFile(char *param_filename)
{
int iplot, ncontours0;
unsigned int i1_str, i2_str, size;
char *string, line1[256], string_error[256];
double bin_width0;
FILE *fptr;

/* Open plot-parameter file */
 if ( (fptr = fopen(param_filename, "r")) == NULL )
    {
    sprintf(string_error, "%s%s", "Cannot open plot-parameter file:\n",
            param_filename);
    JLP_ErrorDialog(string_error);
    return(-1);
    }

// Read parameter file:

 iplot = 0;

   while ( fgets(line1, 256, fptr) != NULL )
      {
       if ( strncmp(line1, "file_name", 9) == 0 )
        {
         iplot++;
        }
      else if ( strncmp(line1, "bin_width", 9) == 0 )
         {
            if ( sscanf(&line1[9], "%lf", &bin_width0) != 1 )
               {
               strcpy(string_error, "Invalid histogram bin width.");
               JLP_ErrorDialog(string_error);
               return(-1);
               } else {
               gseg_plotdata1[iplot].bin_width = bin_width0;
               }
         }

      else if ( strncmp(line1, "bin_value", 9) == 0 )
         {
            size = strlen(&line1[9]);
            if ( (string = gseg_get_string(string_get, &line1[9],
                  &i1_str, &i2_str, &size, 0)) != NULL )
               {
               strncpy(gseg_plotdata1[iplot].bin_value, string, 8);
               }
         }

      else if ( strncmp(line1, "bin_ref", 7) == 0 )
         {
            size = strlen(&line1[7]);
            if ( (string = gseg_get_string(string_get, &line1[7],
                    &i1_str, &i2_str, &size, 0)) != NULL )
               {
               strncpy(gseg_plotdata1[iplot].bin_ref, string, 8);
               }
         }
      else if ( strncmp(line1, "contours", 8) == 0 )
         {
         if ( sscanf(&line1[8], "%d", &ncontours0) == 1 )
            {
            gseg_plotdata1[iplot].ncontours = ncontours0;
            } else {
            strcpy(string_error, "Invalid contours value.");
            JLP_ErrorDialog(string_error);
            return(-1);
            }
         }
     }
   fclose(fptr);

return(0);
}
/***************************************************************************
* Initialize variables for JLP_GsegData
***************************************************************************/
void JLP_GsegData::InitializeDataVariables()
{

/* Initialize min-max data */
   p_data_min_max->xmin = DBLE_MAX_VALUE;
   p_data_min_max->xmax = -DBLE_MAX_VALUE;
   p_data_min_max->ymin = DBLE_MAX_VALUE;
   p_data_min_max->ymax = -DBLE_MAX_VALUE;
   p_data_min_max->zmin = DBLE_MAX_VALUE;
   p_data_min_max->zmax = -DBLE_MAX_VALUE;

   fromfile_data_min_max->xmin = DBLE_MAX_VALUE;
   fromfile_data_min_max->xmax = -DBLE_MAX_VALUE;
   fromfile_data_min_max->ymin = DBLE_MAX_VALUE;
   fromfile_data_min_max->ymax = -DBLE_MAX_VALUE;
   fromfile_data_min_max->zmin = DBLE_MAX_VALUE;
   fromfile_data_min_max->zmax = -DBLE_MAX_VALUE;

return;
}
/*****************************************************************************
* Get number of contour or color plots
*****************************************************************************/
void JLP_GsegData::Get_nplots(int *nplots0)
{
*nplots0 = nplots1;
return;
}
/*****************************************************************************
* Get number of contour plots
*****************************************************************************/
int JLP_GsegData::Get_ncontour_plots(int *ncontour_plots0)
{
int icount, iplot, status = 0;

/* gseg_plot_type:
* 1="points"
* 2="histogram"
* 3="contour"
* 4="color"
* 5="mesh"
*************************/

/* Get number of contour plots */
 icount = 0;
 for ( iplot=1; iplot <= nplots1; iplot++ ){
// 3="contour"
   if(gseg_plotdata1[iplot].gseg_plot_type == 3) icount++;
   }

 *ncontour_plots0 = icount;
 if(*ncontour_plots0 == 0) status = -1;

return(status);
}
/*****************************************************************************
* Get number of color plots
*****************************************************************************/
void JLP_GsegData::Get_ncolor_plots(int *ncolor_plots0)
{
int icount, iplot;

/* Get number of color plots */
 icount = 0;
 for ( iplot=1; iplot <= nplots1; iplot++ ) {
// 4="color"
   if(gseg_plotdata1[iplot].gseg_plot_type == 4) icount++;
   }

 *ncolor_plots0 = icount;

return;
}
/*****************************************************************************
* Get zcolor array for color plots
*****************************************************************************/
int JLP_GsegData::GetImageArray(double **array0, int *nx0, int *ny0)
{
int status, i, j, nx, ny, iplot, icolor = 1;

 *nx0 = 0;
 *ny0 = 0;
 icolor = 0;

 for ( iplot=1; iplot<= nplots1; iplot++ ) {
// 4="color"
   if(gseg_plotdata1[iplot].gseg_plot_type == 4)
    {
    icolor++;
    nx = gseg_plotdata1[iplot].nxcolor;
    ny = gseg_plotdata1[iplot].nycolor;
// Warning: swap x/y here:
    *nx0 = ny;
    *ny0 = nx;
    *array0 = new double[nx * ny];
      for(i = 1; i <= nx; i++) {
        for(j = 1; j <= ny; j++) {
          (*array0)[(j-1) + (i-1) * ny] = gseg_plotdata1[iplot].zcolor[ny * (i-1) + (j-1)];
        }
      }
      status = 0;
      }
 }

 if(status != 0) {
  fprintf(stderr, "JLP_GsegData::GetImageArray/Error, no color plot found \n");
 }

return(status);
}

