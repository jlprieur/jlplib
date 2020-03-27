/***************************************************************************
* JLP_GsegData class
*
* JLP
* Version 27/03/2017
***************************************************************************/
#include "jlp_gseg_data.h"

/***************************************************************************
* Constructor
***************************************************************************/
JLP_GsegData::JLP_GsegData(JLP_Gseg *jlp_gseg0, JLP_Gsegraf *jlp_gsegraf0)
{

// Save to private variables:
jlp_gseg1 = jlp_gseg0;
jlp_gsegraf1 = jlp_gsegraf0;

InitDataParameters();

InitializeDataVariables();

return;
}
/***************************************************************************
* Create pointers and structures for JLP_GsegData
* 
***************************************************************************/
void JLP_GsegData::InitDataParameters()
{
static data_min_max_type data_min_max0, data_min_max1;

// Create pointers of structures 
  p_data_min_max = &data_min_max0;
  fromfile_data_min_max = &data_min_max1;

// Set pointers to NULL:
   nlinebreak            = NULL;
   ndata                 = NULL;
   xdata                 = NULL;
   ydata                 = NULL;
   zdata                 = NULL;
   nxmesh                = NULL;
   nymesh                = NULL;
   xmesh                 = NULL;
   ymesh                 = NULL;
   zmesh                 = NULL;
   nxcontour             = NULL;
   nycontour             = NULL;
   xcontour              = NULL;
   ycontour              = NULL;
   zcontour              = NULL;
   nxcolor               = NULL;
   nycolor               = NULL;
   xcolor                = NULL;
   ycolor                = NULL;
   zcolor                = NULL;

// Filenames:
   nfilenames            = NULL;
   filenames             = NULL;
   nformats              = NULL;
   formats               = NULL;
   formats_mod           = NULL;

// Options:
   plot_types            = NULL;
   stylesizes            = NULL;
   styleflags            = NULL;
   stylechar1            = NULL;
   stylechar2            = NULL;
   stylecolor1           = NULL;
   stylecolor2           = NULL;

// Used in string_get():
   string_get            = NULL;

// Histogram:
   bin_widths            = NULL;
   bin_values            = NULL;
   bin_refs              = NULL;
return;
}
/***************************************************************************
* Initialize variables for JLP_GsegData 
***************************************************************************/
int JLP_GsegData::InitDataArraysAndOptions(char *param_filename,
                                            const int nplots0)
{
int i, index, ibinwidth, ibinvalue, ibinref;
int index_bin_values, index_bin_refs;
unsigned int i1_str, i2_str, size;
char *string, line1[256], string_error[256];
FILE *fptr;

   nplots1 = nplots0;
   ndata               = new int[nplots1];
   bin_widths          = new double[nplots1];
   bin_values          = new char[9 * nplots1];
   bin_refs            = new char[9 * nplots1];
   memset(bin_values,  0, 9 * nplots1 * sizeof(char));
   memset(bin_refs,    0, 9 * nplots1 * sizeof(char));

/* Set default histogram bin_widths and bin_values */
   for ( i=1; i<=nplots1; i++ )
      {
      bin_widths[i-1] = -1.0;
      strcpy(&bin_values[(i-1) * 9], "percent");
      strcpy(&bin_refs[(i-1) * 9], "mean");
      }

/* Set default number of contours */
/* (2d and 3d contour plots) */
   ncontours1 = 0;

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

// Image:
   image_filename1[0] = '\0';

// Legend:
   n_legend_lines1 = 0;
   legend_str1 = NULL;

/* Open plot-parameter file */
 if ( (fptr = fopen(param_filename, "r")) == NULL )
    {
    sprintf(string_error, "%s%s", "Cannot open plot-parameter file:\n",
            param_filename);
    JLP_ErrorDialog(string_error);
    return(-1);
    }

// Read parameter file:

 ibinwidth = 0;
 ibinvalue = 0;
 ibinref = 0;
 index_bin_values = 0;
 index_bin_refs = 0;

   while ( fgets(line1, 256, fptr) != NULL )
      {

      if ( strncmp(line1, "bin_width", 9) == 0 )
         {
         ibinwidth++;
         if ( ibinwidth <= nplots1 )
            if ( sscanf(&line1[9], "%lf", &bin_widths[ibinwidth-1]) != 1 )
               {
               strcpy(string_error, "Invalid histogram bin width.");
               JLP_ErrorDialog(string_error);
               return(-1);
               }
         }

      else if ( strncmp(line1, "bin_value", 9) == 0 )
         {
         ibinvalue++;
         if ( ibinvalue <= nplots1 )
            {
            size = strlen(&line1[9]);
            if ( (string = gseg_get_string(string_get, &line1[9], 
                  &i1_str, &i2_str, &size, 0)) != NULL )
               {
               strncpy(&bin_values[index_bin_values], string, 8);
               index_bin_values = index_bin_values + 9;
               }
            }
         }

      else if ( strncmp(line1, "bin_ref", 7) == 0 )
         {
         ibinref++;
         if ( ibinref <= nplots1 )
            {
            size = strlen(&line1[7]);
            if ( (string = gseg_get_string(string_get, &line1[7], 
                    &i1_str, &i2_str, &size, 0)) != NULL )
               {
               strncpy(&bin_refs[index_bin_refs], string, 8);
               index_bin_refs = index_bin_refs + 9;
               }
            }
         }
      else if ( strncmp(line1, "contours", 8) == 0 )
         {
         if ( sscanf(&line1[8], "%d", &ncontours1) != 1 )
            {
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

// Dummy values

   p_data_min_max->xmin = 0.0;
   p_data_min_max->xmax = 1.0;
   p_data_min_max->ymin = 0.0;
   p_data_min_max->ymax = 1.0;
   p_data_min_max->zmin = 0.0;
   p_data_min_max->zmax = 1.0;

   fromfile_data_min_max->xmin = 0.0;
   fromfile_data_min_max->xmax = 1.0;
   fromfile_data_min_max->ymin = 0.0;
   fromfile_data_min_max->ymax = 1.0;
   fromfile_data_min_max->zmin = 0.0;
   fromfile_data_min_max->zmax = 1.0;

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
* Get number of contour or color plots
*****************************************************************************/
void JLP_GsegData::Get_ncontours(int *ncontours0)
{
 *ncontours0 = ncontours1;
return;
}
/*****************************************************************************
* Get style flag of plot #iplot
*****************************************************************************/
int JLP_GsegData::Get_styleflag(int *styleflag0, const int iplot0)
{
int status = -1;

*styleflag0 = 0;
// plot index varies from 1 to nplots1
if(iplot0 >= 1 && iplot0 <= nplots1) {
  *styleflag0 = styleflags[iplot0 - 1];
  status = 0;
 }

return(status);
}
/*****************************************************************************
* Get plot type of plot #iplot
*****************************************************************************/
int JLP_GsegData::Get_plot_type(char *plot_type0, const int iplot0)
{
int status = -1;

strcpy(plot_type0, "");
// plot index varies from 1 to nplots1
if(iplot0 >= 1 && iplot0 <= nplots1) {
  strcpy(plot_type0, &plot_types[(iplot0 - 1) * 10]);
  status = 0;
 }

return(status);
}
/*****************************************************************************
* Get number of contour plots
*****************************************************************************/
int JLP_GsegData::Get_ncontour_plots(int *ncontour_plots0)
{
int icount, iplot, status = 0;

/* Get number of contour plots */
 icount = 0;
 for ( iplot=1; iplot <= nplots1; iplot++ ){
   if(strcmp(&plot_types[(iplot-1)*10], "contour") == 0)
    icount++;
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

/* Get number of contour plots */
 icount = 0;
 for ( iplot=1; iplot <= nplots1; iplot++ )
     if ( strcmp(&plot_types[(iplot-1)*10], "color") == 0 )
        icount++;

 *ncolor_plots0 = icount;

return;
}
