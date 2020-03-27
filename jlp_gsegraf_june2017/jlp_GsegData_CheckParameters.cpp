/***************************************************************************
* JLP_GsegData class
*
* JLP
* Version 27/03/2017
***************************************************************************/
#include <stdio.h> 
#include <ctype.h>    // isdigit() 
#include "jlp_gseg_data.h"

/***************************************************************************
* Check data filenames 
***************************************************************************/
int JLP_GsegData::CheckDataFilenames()
{
int iplot, index_filenames, size;
char string[256];
FILE *fptr;

   index_filenames = 0;
   for ( iplot=1; iplot<=nplots1; iplot++ )
      {
      if ( (fptr = fopen(&filenames[index_filenames], "r")) == NULL )
         {
         sprintf(string, "CheckDataFilenames/Error %s%s",
                 "Cannot open data file:\n", &filenames[index_filenames]);
         JLP_ErrorDialog(string);
         exit(1);
         }
      else
         {
         fclose(fptr);
// Warning: some plots may have nfilenames[iplot -1] = 0 !
         index_filenames = index_filenames + nfilenames[iplot-1];
         }
      }

return(0);
}
/***************************************************************************
* 
***************************************************************************/
int JLP_GsegData::CheckHistogramOptions()
{
int i, index_bin_values, index_bin_refs;
const char *error_str[] = 
   {    "CheckHistogramOptions/Invalid bin_ref parameter.", 
        "CheckHistogramOptions/View-direction elevation angle out of range." }; 


   /* Check bin_value parameter for histograms */
   index_bin_values = 0;
   for ( i=1; i<=nplots1; i++ )
      {
      if ( strcmp(&bin_values[index_bin_values], "number")   != 0 &&
           strcmp(&bin_values[index_bin_values], "fraction") != 0 &&
           strcmp(&bin_values[index_bin_values], "percent")  != 0 )
         {
         JLP_ErrorDialog(error_str[0]);
         exit(1);
         }

      index_bin_values = index_bin_values + 9;
      }


   /* Check bin_refs parameter for histograms */
   index_bin_refs = 0;
   for ( i=1; i<=nplots1; i++ )
      {
      if ( strcmp(&bin_refs[index_bin_refs], "mean")     != 0 &&
           strcmp(&bin_refs[index_bin_refs], "zero")     != 0 &&
           strcmp(&bin_refs[index_bin_refs], "integers") != 0 )
         {
         JLP_ErrorDialog(error_str[1]);
         exit(1);
         }

      index_bin_refs = index_bin_refs + 9;
      }


return(0);
}
/******************************************************************************
*
*******************************************************************************/
int JLP_GsegData::CheckPlotTypes(const char *axis_type0)
{
/* Declare variables */
int iplot, iformat, index_filenames, index_formats, index_plot_types,
    index_bin_values, index_bin_refs, icount, i, j, k, ifunc, status;
unsigned int size;
UINT32 stylecolor1_0, stylecolor2_0;
char *pchar, string[256], buffer[32];
const char *error_str[] =
   { "Invalid or missing plot_type parameter.",  
     "Only points, histogram, contour, and color plot_type's\ncompatible with linear axis_type.",
     "Only points plot_type compatible with semilogx axis_type.",
     "Only points plot_type compatible with semilogy axis_type.",
     "Only points plot_type compatible with loglog axis_type.",
     "Only points plot_type compatible with polar axis_type.",
     "Only points, mesh, contour, and color plot_type's\ncompatible with 3d axis_type.",
     "Invalid or missing plot_style parameter.",
     "Invalid or missing data format;\nformat must specify double data type;\none data column read for histogram plot type.",
     "Invalid or missing data format;\nformat must specify double data type;\ntwo data columns read for 2d points plot type.",
     "Invalid or missing data format;\nformat must specify double data type;\nthree data columns read for 3d points plot type."};
FILE *fptr;

/* Check number of plots */
 if(nplots1 == 0) { 
   fprintf(stderr, "CheckParamData/Error: nplots1 = 0 !\n");
   return(-1);
   }

   index_plot_types = 0;
   for ( iplot=1; iplot<=nplots1; iplot++ )
      {
      if ( strcmp(&plot_types[index_plot_types], "points")    != 0 &&
           strcmp(&plot_types[index_plot_types], "histogram") != 0 &&
           strcmp(&plot_types[index_plot_types], "mesh")      != 0 &&
           strcmp(&plot_types[index_plot_types], "contour")   != 0 &&
           strcmp(&plot_types[index_plot_types], "color")     != 0 )
         {
         JLP_ErrorDialog(error_str[0]);
         exit(1);
         }

      /* Increment index */
      index_plot_types = index_plot_types + 10;
      }


   /* Check compatibility of axis_type and plot_type parameters */
   index_plot_types = 0;
   for ( iplot=1; iplot<=nplots1; iplot++ )
      {
      if ( strcmp(axis_type0, "linear") == 0 &&
           strcmp(&plot_types[index_plot_types], "points")    != 0 &&
           strcmp(&plot_types[index_plot_types], "histogram") != 0 &&
           strcmp(&plot_types[index_plot_types], "contour")   != 0 &&
           strcmp(&plot_types[index_plot_types], "color")     != 0 )
         {
         JLP_ErrorDialog(error_str[1]);
         exit(1);
         }

      else if ( strcmp(axis_type0, "semilogx") == 0 &&
                strcmp(&plot_types[index_plot_types], "points") != 0 )
         {
         JLP_ErrorDialog(error_str[2]);
         exit(1);
         }

      else if ( strcmp(axis_type0, "semilogy") == 0 &&
                strcmp(&plot_types[index_plot_types], "points") != 0 )
         {
         JLP_ErrorDialog(error_str[3]);
         exit(1);
         }

      else if ( strcmp(axis_type0, "loglog") == 0 &&
                strcmp(&plot_types[index_plot_types], "points") != 0 )
         {
         JLP_ErrorDialog(error_str[4]);
         exit(1);
         }

      else if ( strcmp(axis_type0, "polar") == 0 &&
                strcmp(&plot_types[index_plot_types], "points") != 0 )
         {
         JLP_ErrorDialog(error_str[5]);
         exit(1);
         }

      else if ( strcmp(axis_type0, "3d") == 0 &&
                strcmp(&plot_types[index_plot_types], "points")  != 0 &&
                strcmp(&plot_types[index_plot_types], "mesh")    != 0 &&
                strcmp(&plot_types[index_plot_types], "contour") != 0 &&
                strcmp(&plot_types[index_plot_types], "color")   != 0 )
         {
         JLP_ErrorDialog(error_str[6]);
         exit(1);
         }

      /* Increment index */
      index_plot_types = index_plot_types + 10;
      }


   /* Check compatibility of plot_type and plot_style parameters */
   index_plot_types = 0;
   for ( iplot=1; iplot<=nplots1; iplot++ )
      {
#ifdef DEBUG
printf("Debug/ %s iplot=%d styleflags = %d \n", 
        &plot_types[index_plot_types], iplot, styleflags[iplot -1 ]);
#endif
      if ( strcmp(&plot_types[index_plot_types], "points") == 0 )
         {
         if ( styleflags[iplot-1] != 2 &&
              styleflags[iplot-1] != 4 )
            {
            JLP_ErrorDialog(error_str[7]);
            exit(1);
            }
         }

      else if ( strcmp(&plot_types[index_plot_types], "histogram") == 0 )
         {
         if ( styleflags[iplot-1] != 2 &&
              styleflags[iplot-1] != 4 )
            {
            JLP_ErrorDialog(error_str[7]);
            exit(1);
            }
         }

      else if ( strcmp(&plot_types[index_plot_types], "contour") == 0 &&
                strcmp(axis_type0, "linear") == 0 )
         {
         if ( styleflags[iplot-1] != 1 &&
              styleflags[iplot-1] != 3 &&
              styleflags[iplot-1] != 7 )
            {
            JLP_ErrorDialog(error_str[7]);
            exit(1);
            }
         }

      else if ( strcmp(&plot_types[index_plot_types], "color") == 0 &&
                strcmp(axis_type0, "linear") == 0 )
         {
         if ( styleflags[iplot-1] != 8 &&
              styleflags[iplot-1] != 9 )
            {
            JLP_ErrorDialog(error_str[7]);
            exit(1);
            }
         }

      else if ( strcmp(&plot_types[index_plot_types], "mesh") == 0 )
         {
         if ( styleflags[iplot-1] != 2 &&
              styleflags[iplot-1] != 4 &&
              styleflags[iplot-1] != 5 &&
              styleflags[iplot-1] != 6 &&
              styleflags[iplot-1] != 7 )
            {
            JLP_ErrorDialog(error_str[7]);
            exit(1);
            }
         }

      else if ( strcmp(&plot_types[index_plot_types], "contour") == 0 &&
                strcmp(axis_type0, "3d") == 0 )
         {
         if ( styleflags[iplot-1] != 2 &&
              styleflags[iplot-1] != 4 &&
              styleflags[iplot-1] != 5 &&
              styleflags[iplot-1] != 6 )
            {
            JLP_ErrorDialog(error_str[7]);
            exit(1);
            }
         }

      else if ( strcmp(&plot_types[index_plot_types], "color") == 0 &&
                strcmp(axis_type0, "3d") == 0 )
         {
         if ( styleflags[iplot-1] != 7 )
            {
            JLP_ErrorDialog(error_str[7]);
            exit(1);
            }
         }

      /* Increment index */
      index_plot_types = index_plot_types + 10;
      }


/* Check stylechar1 data */
index_filenames = 0;
index_plot_types = 0;
for ( iplot=1; iplot<=nplots1; iplot++ )
   {
   if ( strcmp(&plot_types[index_plot_types], "points") == 0 )
      {
      status = jlp_gsegraf1->SymbolFromStyleChar1(jlp_gsegraf1->SymbolString(),
                                                  iplot-1, &ifunc);
      if ( status != 0 )
         {
         sprintf(string, "CheckPlotTypes/Error %s%s (ifinc=%d)", 
                 "Invalid or missing plot point symbol for file:\n", 
                 &filenames[index_filenames], ifunc);
         JLP_ErrorDialog(string);
         exit(1);
         }
      }

   else if ( strcmp(&plot_types[index_plot_types], "histogram") == 0 )
      {
      strcpy(buffer, "lbB");
      status = jlp_gsegraf1->SymbolFromStyleChar1(buffer, iplot-1, &ifunc);
      if ( status != 0 )
         {
         sprintf(string, "CheckPlotTypes/Error %s%s", 
                 "Invalid or missing plot histo symbol for file:\n", 
                 &filenames[index_filenames]);
         JLP_ErrorDialog(string);
         exit(1);
         }
      }

   else if ( strcmp(&plot_types[index_plot_types], "mesh") == 0 )
      {
      if ( styleflags[iplot-1] ==  2 ||
           styleflags[iplot-1] ==  4 )
         {
         if (jlp_gsegraf1->SetStyleColorFromStyleChar(&stylecolor1_0,
                                             stylechar1[iplot-1]) != 0 )
            {
            sprintf(string, "CheckPlotTypes/Error Invalid or missing plot color for file:\n%s", 
                    &filenames[index_filenames]);
            JLP_ErrorDialog(string);
            exit(1);
            }
         }
      }

   else if ( strcmp(axis_type0, "3d") == 0 &&
             strcmp(&plot_types[index_plot_types], "contour") == 0 )
      {
      if ( styleflags[iplot-1] ==  2 ||
           styleflags[iplot-1] ==  4 )
         {
         if (jlp_gsegraf1->SetStyleColorFromStyleChar(&stylecolor1_0,
                                             stylechar1[iplot-1]) != 0 )
            {
            sprintf(string, "CheckPlotTypes/Error %s%s", 
                    "Invalid or missing plot color for file:\n", 
                    &filenames[index_filenames]);
            JLP_ErrorDialog(string);
            exit(1);
            }
         }
      }

/* Increment indices */
   index_filenames = index_filenames + nfilenames[iplot-1];
   index_plot_types = index_plot_types + 10;
   }


/* Check stylechar2 data */
index_filenames = 0;
index_plot_types = 0;
for ( iplot=1; iplot<=nplots1; iplot++ )
   {
   if ( strcmp(&plot_types[index_plot_types], "points")    == 0 ||
        strcmp(&plot_types[index_plot_types], "histogram") == 0 ||
        strcmp(&plot_types[index_plot_types], "mesh")      == 0 ||
        strcmp(&plot_types[index_plot_types], "contour")   == 0 )
      {
      if ( styleflags[iplot-1] ==  2 ||
           styleflags[iplot-1] ==  5 )
         if (jlp_gsegraf1->SetStyleColorFromStyleChar(&stylecolor2_0,
                                             stylechar2[iplot-1]) != 0 )
            {
            sprintf(string, "CheckPlotTypes/Error %s%s", 
                    "Invalid or missing plot color for file:\n", 
                    &filenames[index_filenames]);
            JLP_ErrorDialog(string);
            exit(1);
            }
      }

   index_filenames = index_filenames + nfilenames[iplot-1];
   index_plot_types = index_plot_types + 10;
   }


/* Check bin_value parameter for histograms */
CheckHistogramOptions();


/* Check data formats for points and histogram plot types */
index_plot_types = 0;
index_formats = 0;
iformat = 0;
for ( iplot=1; iplot<=nplots1; iplot++ )
   {
   if ( strcmp(&plot_types[index_plot_types], "points")    == 0 ||
        strcmp(&plot_types[index_plot_types], "histogram") == 0 )
      {
      iformat++;

/* Check that format is for double data type */
      icount = 0;
      i = 0;
      while ( (pchar = strchr(&formats[index_formats+i], '%')) != NULL )
         {
         if ( *(pchar+1) != '*' )
            {
            j = 0;
            while ( isdigit(*(pchar+j+1)) != 0 )
               j++;
            if ( *(pchar+j+1) == 'l' &&
                 (*(pchar+j+2) == 'e' || *(pchar+j+2) == 'f' 
                  || *(pchar+j+2) == 'g') )
               icount++;
            }
         i = i + pchar - &formats[index_formats+i] + j + 3;
         }

      if ( strcmp(&plot_types[index_plot_types], "histogram") == 0 )
         {
         if ( icount != 1 )
            {
            JLP_ErrorDialog(error_str[8]);
            exit(1);
            }
         }
      else if ( strcmp(&plot_types[index_plot_types], "points") == 0 )
         {
         if ( strcmp(axis_type0, "linear")    == 0 ||
              strcmp(axis_type0, "semilogx")  == 0 ||
              strcmp(axis_type0, "semilogy")  == 0 ||
              strcmp(axis_type0, "loglog")    == 0 ||
              strcmp(axis_type0, "polar")     == 0 )
            {
            if ( icount != 2 )
               {
               JLP_ErrorDialog(error_str[9]);
               exit(1);
               }
            }
         else if ( strcmp(axis_type0, "3d") == 0 )
            {
            if ( icount != 3 )
               {
               JLP_ErrorDialog(error_str[10]);
               exit(1);
               }
            }
         }

/* Increment indices */
      index_formats = index_formats + nformats[iformat-1];
      index_plot_types = index_plot_types + 10;
      }
   }


/* Create modified data formats to read asterisks 
* instead of numbers for points plot types */
index_plot_types = 0;
index_formats = 0;
iformat = 0;
for ( iplot=1; iplot<=nplots1; iplot++ )
   {
   if ( strcmp(&plot_types[index_plot_types], "points")    == 0 ||
        strcmp(&plot_types[index_plot_types], "histogram") == 0 )
      {
      iformat++;

      strcpy(&formats_mod[index_formats], &formats[index_formats]);

      i = 0;
      while ( (pchar = strchr(&formats[index_formats+i], '%')) != NULL )
         {
         if ( *(pchar+1) != '*' )
            {
            j = pchar - &formats[index_formats+i];
            k = 0;
            while ( isdigit(*(pchar+k+1)) != 0 )
               {
               formats_mod[index_formats+i+j+k+1] = ' ';
               k++;
               }
            formats_mod[index_formats+i+j+k+1] = ' ';
            formats_mod[index_formats+i+j+k+2] = ' ';
            formats_mod[index_formats+i+j+1] = 'c';
            }
         i = i + j + k + 3;
         }

      index_formats = index_formats + nformats[iformat-1];
      index_plot_types = index_plot_types + 10;
      }
   }

return(0);
}
