/***************************************************************************
* JLP_GsegData class
*
* JLP
* Version 27/06/2017
***************************************************************************/
#include <stdio.h> 
#include <ctype.h>    // isdigit() 
#include "jlp_gseg_data.h"

/***************************************************************************
* Check data filenames 
***************************************************************************/
int JLP_GsegData::CheckDataFilenames()
{
int iplot;
char fname0[128], string[256];
FILE *fptr;

// Always OK if no parameter file:
 if(gsegdata_from_paramfile == 0) return(0);

 for ( iplot=1; iplot<=nplots1; iplot++ )
   {
    strcpy(fname0, gseg_plotdata1[iplot].filename);
      if ( (fptr = fopen(fname0, "r")) == NULL ) {
        sprintf(string, "CheckDataFilenames/Error %s%s",
                "CheckDataFilenames/Cannot open data file:\n", fname0);
        JLP_ErrorDialog(string);
        return(1);
      } else {
        fclose(fptr);
      }
  }

return(0);
}
/***************************************************************************
* 
***************************************************************************/
int JLP_GsegData::CheckHistogramOptions()
{
int iplot;
const char *error_str[] = 
   { "CheckHistogramOptions/Invalid bin_ref parameter.", 
     "CheckHistogramOptions/View-direction elevation angle out of range." }; 

/* Check bin_value parameter for histograms */
  for ( iplot=1; iplot<=nplots1; iplot++ ) {
    if ( strcmp(gseg_plotdata1[iplot].bin_value, "number")   != 0 &&
         strcmp(gseg_plotdata1[iplot].bin_value, "fraction") != 0 &&
         strcmp(gseg_plotdata1[iplot].bin_value, "percent")  != 0 )
       {
       JLP_ErrorDialog(error_str[0]);
       return(1);
       }
    }


/* Check bin_refs parameter for histograms */
  for ( iplot=1; iplot<=nplots1; iplot++ ) {
    if ( strcmp(gseg_plotdata1[iplot].bin_ref, "mean")     != 0 &&
         strcmp(gseg_plotdata1[iplot].bin_ref, "zero")     != 0 &&
         strcmp(gseg_plotdata1[iplot].bin_ref, "integers") != 0 )
       {
       JLP_ErrorDialog(error_str[1]);
       return(1);
       }
    }


return(0);
}
/******************************************************************************
*
* Check compatibility of axis_type and plot_type parameters
*******************************************************************************/
int JLP_GsegData::CheckPlotType(const char *axis_type0)
{
int iplot, gseg_plot_type0;
char error_string[128];
const char *error_str[] =
   { "Invalid or missing plot_type parameter.",  
     "Only points, histogram, contour, and color plot_type's\ncompatible with linear axis_type.",
     "Only points plot_type compatible with semilogx axis_type.",
     "Only points plot_type compatible with semilogy axis_type.",
     "Only points plot_type compatible with loglog axis_type.",
     "Only points plot_type compatible with polar axis_type."};

/* Check number of plots */
 if(nplots1 == 0) { 
   fprintf(stderr, "CheckParamData/Error: nplots1 = 0 !\n");
   return(-1);
   }

 for ( iplot=1; iplot<=nplots1; iplot++ )
    {
/* gseg_plot_type:
* 1="points"
* 2="histogram"
* 3="contour"
* 4="color"
* 5="mesh"
*************************/
    gseg_plot_type0 = gseg_plotdata1[iplot].gseg_plot_type;

// 1="points", 2="histogram", 3="contour", 4="color", 5="mesh"
    if(gseg_plot_type0 < 1 || gseg_plot_type0 > 5)
       {
       sprintf(error_string, "CheckPlotType/Invalid or missing plot_type parameter: iplot=%d plot_type=%d\n",
               iplot, gseg_plot_type0);
       JLP_ErrorDialog(error_string);
       return(1);
       }

// axis_type0="linear" should be associated 
// with gseg_plot_type0="points", "histogram", "contour" or "color"
    if ( strcmp(axis_type0, "linear") == 0 &&
         (gseg_plot_type0 < 1 || gseg_plot_type0 > 4))
       {
       JLP_ErrorDialog(error_str[1]);
       return(1);
       }

// axis_type0="semilogx" should be associated 
// with gseg_plot_type0="points"
    else if ( strcmp(axis_type0, "semilogx") == 0 && gseg_plot_type0 != 1 )
       {
       JLP_ErrorDialog(error_str[2]);
       return(1);
       }

// axis_type0="semilogy" should be associated 
// with gseg_plot_type0="points"
    else if ( strcmp(axis_type0, "semilogy") == 0 && gseg_plot_type0 != 1 )
       {
       JLP_ErrorDialog(error_str[3]);
       return(1);
       }

// axis_type0="loglog" should be associated 
// with gseg_plot_type0="points"
    else if ( strcmp(axis_type0, "loglog") == 0 && gseg_plot_type0 != 1 )
       {
       JLP_ErrorDialog(error_str[4]);
       return(1);
       }

// axis_type0="polar" should be associated 
// with gseg_plot_type0="points"
    else if ( strcmp(axis_type0, "polar") == 0 && gseg_plot_type0 != 1 )
       {
       JLP_ErrorDialog(error_str[5]);
       return(1);
       }

// axis_type0="3d" should be associated 
// with gseg_plot_type0="points", "contour", "color" or "mesh"
/* gseg_plot_type :
* 1="points"
* 2="histogram"
* 3="contour"
* 4="color"
* 5="mesh"
*****************/
    else if ( strcmp(axis_type0, "3d") == 0 &&
              (gseg_plot_type0 < 1 || gseg_plot_type0 == 2
               || gseg_plot_type0 > 5 ))
       {
       JLP_ErrorDialog(error_str[6]);
       return(1);
       }
    }

return(0);
}
/*****************************************************************************
* Check style_flag
*
******************************************************************************/
int JLP_GsegData::CheckStyleFlag(const char *axis_type0)
{
int iplot, gseg_plot_type0, style_flag0;
char error_str[128];

/************************ style_flag ***********************************/
// Check style_flag0
 for ( iplot=1; iplot<=nplots1; iplot++ )
    {
/* gseg_plot_type:
* 1="points"
* 2="histogram"
* 3="contour"
* 4="color"
* 5="mesh"
*************************/
    gseg_plot_type0 = gseg_plotdata1[iplot].gseg_plot_type;
    style_flag0 = gseg_plotdata1[iplot].style_flag;

// gseg_plot_type0="points"
// should be associated with styleflag = 2 or 4
    if( (gseg_plot_type0 == 1) &&  
        (style_flag0 != 2 && style_flag0 != 4))
    {
      sprintf(error_str, "CheckStyleFlag/Invalid or missing plot_style parameter: plot_type=%d style_flag=%d",
              gseg_plot_type0, style_flag0);
      JLP_ErrorDialog(error_str);
      return(1);
     }

// gseg_plot_type0="histogram"
// should be associated with styleflag = 2 or 4
    else if( (gseg_plot_type0 == 2 ) &&
           (style_flag0 != 2 && style_flag0 != 4))
      {
      sprintf(error_str, "CheckStyleFlag/Invalid or missing plot_style parameter: plot_type=%d style_flag=%d",
              gseg_plot_type0, style_flag0);
      JLP_ErrorDialog(error_str);
      return(1);
      }

// gseg_plot_type0="contour" and axis_type0="linear"
// should be associated with styleflag = 1, 3 or 7
    else if ( (gseg_plot_type0 == 3) &&
              (strcmp(axis_type0, "linear") == 0) )
       {
       if ( style_flag0 != 1 && style_flag0 != 3 && style_flag0 != 7)
          {
          sprintf(error_str, "CheckStyleFlag/Invalid or missing plot_style parameter: plot_type=%d axis_type=%s style_flag=%d",
              gseg_plot_type0, axis_type0, style_flag0);
          JLP_ErrorDialog(error_str);
          return(1);
          }
       }

// gseg_plot_type0="color" and axis_type0="linear"
// should be associated with styleflag = 8 or 9
    else if ( (gseg_plot_type0 == 4) &&
              (strcmp(axis_type0, "linear") == 0) )
       {
       if ( style_flag0 != 8 && style_flag0 != 9)
          {
          sprintf(error_str, "CheckStyleFlag/Invalid or missing plot_style parameter: plot_type=%d axis_type=%s style_flag=%d",
              gseg_plot_type0, axis_type0, style_flag0);
          JLP_ErrorDialog(error_str);
          return(1);
          }
       }

// gseg_plot_type0="mesh"
// should be associated with styleflag = 2, 4, 5, 6 or 7
    else if ( gseg_plot_type0 == 5 )
       {
       if ( style_flag0 != 2 && style_flag0 != 4 && style_flag0 != 5
            && style_flag0 != 6 && style_flag0 != 7)
          {
          sprintf(error_str, "CheckStyleFlag/Invalid or missing plot_style parameter: plot_type=%d style_flag=%d",
              gseg_plot_type0, style_flag0);
          JLP_ErrorDialog(error_str);
          return(1);
          }
       }

// gseg_plot_type0="contour" and axis_type0="3d"
// should be associated with styleflag = 2, 4, 5 or 6
    else if ( (gseg_plot_type0 == 3) &&
              (strcmp(axis_type0, "3d") == 0) )
       {
       if ( style_flag0 != 2 && style_flag0 != 4 && style_flag0 != 5
            && style_flag0 != 6)
          {
          sprintf(error_str, "CheckStyleFlag/Invalid or missing plot_style parameter: plot_type=%d axis_type=%s style_flag=%d",
              gseg_plot_type0, axis_type0, style_flag0);
          JLP_ErrorDialog(error_str);
          return(1);
          }
       }

// gseg_plot_type0="color" and axis_type0="3d"
// should be associated with styleflag = 7 
    else if ( (gseg_plot_type0 == 4) &&
              (strcmp(axis_type0, "3d") == 0) )
       {
       if ( style_flag0 != 7 )
          {
          sprintf(error_str, "CheckStyleFlag/Invalid or missing plot_style parameter: plot_type=%d axis_type=%s style_flag=%d (should be 7)",
              gseg_plot_type0, axis_type0, style_flag0);
          JLP_ErrorDialog(error_str);
          return(1);
          }
       }

    }

return(0);
}
/******************************************************************************
* Check style_char1 and style_char2 data
*
*******************************************************************************/
int JLP_GsegData::CheckStyleChar(const char *axis_type0)
{
int iplot, gseg_plot_type0, ifunc, status, style_flag0, is_upper_case0;
char string[256];
char style_char10, style_char20;
UINT32 style_color10, style_color20;

for ( iplot=1; iplot<=nplots1; iplot++ )
   {
/* gseg_plot_type:
* 1="points"
* 2="histogram"
* 3="contour"
* 4="color"
* 5="mesh"
*************************/
   gseg_plot_type0 = gseg_plotdata1[iplot].gseg_plot_type;
   style_flag0 = gseg_plotdata1[iplot].style_flag;
   style_char10 = gseg_plotdata1[iplot].style_char1;

// 1="points"
   if ( gseg_plot_type0 == 1 )
      {
// Try decoding style_char10 with "symbol_string"
      status = DrawSymbolOptionFromChar(style_char10, 4, &ifunc, 
                                        &is_upper_case0);
      if ( status != 0 )
         {
         sprintf(string, "CheckStyleChar/Error %s%s (iplot=%d style_char10=%c ifunc=%d)", 
                 "Invalid or missing plot point symbol for file:\n", 
                 gseg_plotdata1[iplot].filename, iplot, style_char10, ifunc);
         JLP_ErrorDialog(string);
         return(1);
         }
      }

// 2="histogram"
   else if ( gseg_plot_type0 == 2 )
      {
// strcpy(buffer, "lbB");
// Try decoding style_char10 with "lbB" 
      status = DrawSymbolOptionFromChar(style_char10, 3, &ifunc, 
                                        &is_upper_case0);
      if ( status != 0 )
         {
         sprintf(string, "CheckStyleChar/Error %s%s", 
                 "Invalid or missing plot histo symbol for file:\n", 
                 gseg_plotdata1[iplot].filename);
         JLP_ErrorDialog(string);
         return(1);
         }
      }

// 5="mesh"
   else if ( gseg_plot_type0 == 5 )
      {
      if ( style_flag0 ==  2 || style_flag0 ==  4 )
         {
         if (GetStyleColorFromStyleChar(style_char10, &style_color10) != 0 )
            {
            sprintf(string, "CheckStyleChar/Error Invalid or missing plot color for file:\n%s", 
                    gseg_plotdata1[iplot].filename);
            JLP_ErrorDialog(string);
            return(1);
            } else {
            gseg_plotdata1[iplot].style_color1 = style_color10;
            }
         }
      }

// 3="contour"
   else if ( strcmp(axis_type0, "3d") == 0 && gseg_plot_type0 == 3 )
      {
      if ( style_flag0 ==  2 || style_flag0 ==  4 )
         {
         if (GetStyleColorFromStyleChar(style_char10, &style_color10) != 0 )
            {
            sprintf(string, "CheckStyleChar/Error %s%s", 
                    "Invalid or missing plot color for file:\n", 
                    gseg_plotdata1[iplot].filename);
            JLP_ErrorDialog(string);
            return(1);
            } else {
            gseg_plotdata1[iplot].style_color1 = style_color10;
            }
         }
      }

   }


/* Check stylechar2 data */
for ( iplot=1; iplot<=nplots1; iplot++ )
   {
/* gseg_plot_type:
* 1="points"
* 2="histogram"
* 3="contour"
* 4="color"
* 5="mesh"
*************************/
   gseg_plot_type0 = gseg_plotdata1[iplot].gseg_plot_type;
   style_flag0 = gseg_plotdata1[iplot].style_flag;
   style_char20 = gseg_plotdata1[iplot].style_char2;

// 1="points", 2="histogram", 3="contour" or 5="mesh"
   if ( gseg_plot_type0 == 1 || gseg_plot_type0 == 2 
        || gseg_plot_type0 == 3 || gseg_plot_type0 == 5)
      {
         if (GetStyleColorFromStyleChar(style_char20, &style_color20) != 0 )
            {
            sprintf(string, "CheckStyleChar/Error %s%s", 
                    "Invalid or missing plot color for file:\n", 
                    gseg_plotdata1[iplot].filename);
            JLP_ErrorDialog(string);
            return(1);
            } else {
            gseg_plotdata1[iplot].style_color2 = style_color20;
            }
      }
   }

return(0);
}
/***************************************************************************
*
****************************************************************************/
int JLP_GsegData::CheckHistogramData(const char *axis_type0)
{
int i, j, k, iplot, icount, gseg_plot_type0;
char *pchar;
const char *error_str[] =
   { "Invalid or missing data format;\nformat must specify double data type;\none data column read for histogram plot type.",
     "Invalid or missing data format;\nformat must specify double data type;\ntwo data columns read for 2d points plot type.",
     "Invalid or missing data format;\nformat must specify double data type;\nthree data columns read for 3d points plot type."};

/* Check bin_value parameter for histograms */
CheckHistogramOptions();

/* Check data formats for points and histogram plot types */
for ( iplot=1; iplot<=nplots1; iplot++ )
   {
/* gseg_plot_type:
* 1="points"
* 2="histogram"
* 3="contour"
* 4="color"
* 5="mesh"
*************************/
   gseg_plot_type0 = gseg_plotdata1[iplot].gseg_plot_type;

// 1="points" or 2="histogram"
   if ( gseg_plot_type0 == 1 || gseg_plot_type0 == 2) 
      {

/* Check that format is for double data type */
      icount = 0;
      i = 0;
// char *strchr(const char *str, int c) searches for the first occurrence 
// of the character c (an unsigned char) in the string pointed to by 
// the argument str.
      while ( (pchar = strchr(&((gseg_plotdata1[iplot]).prnt_formats[i]), '%')) 
             != NULL )
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
         i = i + pchar - &((gseg_plotdata1[iplot]).prnt_formats[i]) + j + 3;
         }

// 2="histogram"
      if ( gseg_plot_type0 == 2) 
         {
         if ( icount != 1 )
            {
            JLP_ErrorDialog(error_str[0]);
            return(1);
            }
         }
// 1="points"
      else if ( gseg_plot_type0 == 1) 
         {
         if ( strcmp(axis_type0, "linear")    == 0 ||
              strcmp(axis_type0, "semilogx")  == 0 ||
              strcmp(axis_type0, "semilogy")  == 0 ||
              strcmp(axis_type0, "loglog")    == 0 ||
              strcmp(axis_type0, "polar")     == 0 )
            {
            if ( icount != 2 )
               {
               JLP_ErrorDialog(error_str[1]);
               return(1);
               }
            }
         else if ( strcmp(axis_type0, "3d") == 0 )
            {
            if ( icount != 3 )
               {
               JLP_ErrorDialog(error_str[2]);
               return(1);
               }
            }
         } // EOF while()
      } // 1="points" or 2="histogram"
   } // EOF loop on iplot 

/* Create modified data formats to read asterisks 
* instead of numbers for points plot types */
for ( iplot=1; iplot<=nplots1; iplot++ )
   {
/* gseg_plot_type:
* 1="points"
* 2="histogram"
* 3="contour"
* 4="color"
* 5="mesh"
*************************/
   gseg_plot_type0 = gseg_plotdata1[iplot].gseg_plot_type;
// 1="points" or 2="histogram"
   if ( gseg_plot_type0 == 1 || gseg_plot_type0 == 2) 
      {
      strcpy(gseg_plotdata1[iplot].prnt_mod_formats, 
              gseg_plotdata1[iplot].prnt_formats);

      i = 0;
      while ( (pchar = strchr(&((gseg_plotdata1[iplot]).prnt_formats[i]), '%')) 
             != NULL )
         {
         if ( *(pchar+1) != '*' )
            {
            j = pchar - &((gseg_plotdata1[iplot]).prnt_formats[i]) ;
            k = 0;
            while ( isdigit(*(pchar+k+1)) != 0 )
               {
               (gseg_plotdata1[iplot]).prnt_mod_formats[i+j+k+1] = ' ';
               k++;
               }
            (gseg_plotdata1[iplot]).prnt_mod_formats[i+j+k+1] = ' ';
            (gseg_plotdata1[iplot]).prnt_mod_formats[i+j+k+2] = ' ';
            (gseg_plotdata1[iplot]).prnt_mod_formats[i+j+1] = 'c';
            }
         i = i + j + k + 3;
         }

      }
   }

return(0);
}
