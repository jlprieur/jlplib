/***************************************************************************
* JLP_GsegData_plotStyles.cpp
*
* JLP_GsegData class
*
* JLP
* Version 27/03/2017
***************************************************************************/
#include <stdio.h> 
#include <ctype.h>    // isdigit() 
#include "jlp_gseg_data.h"

/*
#define DEBUG
*/

/***************************************************************************
* Get the gseg_plot_type corresponding to iplot0:
*
* gseg_plot_type:
* 1="points"
* 2="histogram"
* 3="contour"
* 4="color"
* 5="mesh"
*
* INPUT:
*  iplot0 : plot index (from 1 to nplots1)
***************************************************************************/
int JLP_GsegData::GetGsegPlotType(const int iplot0, int *gseg_plot_type0) 
{
int status = -1;
 *gseg_plot_type0 = 0;
 if(iplot0 > 0 && iplot0 <= nplots1) { 
   *gseg_plot_type0 = gseg_plotdata1[iplot0].gseg_plot_type; 
   status = 0;
   }
return(status);
}
/***************************************************************************
* style_flag
* 1 or 3 = "Contour plot with constant-color contour lines"
* 7 = "Contour plot with variable-color contour lines" (2d)
*     or "Find polygon minimum and maximum z coordinates" (3d)
* 8 = "bilinear interpolation"
* 9 = "nearest-neighbor interpolation"
*
* INPUT:
*  iplot0 : plot index (from 1 to nplots1)
***************************************************************************/
int JLP_GsegData::GetStyleFlag(const int iplot0, int *styleflag0) 
{
int status = -1;
 *styleflag0 = 0;
 if(iplot0 > 0 && iplot0 <= nplots1) { 
   *styleflag0 = gseg_plotdata1[iplot0].style_flag; 
   status = 0;
   }
return(status);
}
/***************************************************************************
* Types and styles:
* INPUT:
*  iplot0 : plot index (from 1 to nplots1)
***************************************************************************/
int JLP_GsegData::GetStyleSize(const int iplot0, unsigned int *stylesize0) 
{
int status = -1;
 *stylesize0 = 0;
 if(iplot0 > 0 && iplot0 <= nplots1) { 
      *stylesize0 = gseg_plotdata1[iplot0].style_size; 
      status = 0;
      }
return(status);
}
/***************************************************************************
* Types and styles:
* INPUT:
*  iplot0 : plot index (from 1 to nplots1)
***************************************************************************/
int JLP_GsegData::GetStyleChar1(const int iplot0, char *stylechar10) 
{
int status = -1;
 *stylechar10 = '\0';
 if(iplot0 > 0 && iplot0 <= nplots1) { 
    *stylechar10 = gseg_plotdata1[iplot0].style_char1; 
    status = 0;
    }
return(status);
}
/***************************************************************************
* Types and styles:
* INPUT:
*  iplot0 : plot index (from 1 to nplots1)
***************************************************************************/
int JLP_GsegData::GetStyleChar2(const int iplot0, char *stylechar20) 
{
int status = -1;
 *stylechar20 = '\0';
 if(iplot0 > 0 && iplot0 <= nplots1) { 
    *stylechar20 = gseg_plotdata1[iplot0].style_char2; 
    status = 0;
    }
return(status);
}
/***************************************************************************
* Types and styles:
* INPUT:
*  iplot0 : plot index (from 1 to nplots1)
***************************************************************************/
int JLP_GsegData::GetStyleColor1(const int iplot0, UINT32 *stylecolor10) 
{
int status = -1;
 *stylecolor10 = 0;
 if(iplot0 > 0 && iplot0 <= nplots1) { 
   *stylecolor10 = gseg_plotdata1[iplot0].style_color1; 
   status = 0;
   }
return(status);
}
/***************************************************************************
* Types and styles:
* INPUT:
*  iplot0 : plot index (from 1 to nplots1)
***************************************************************************/
int JLP_GsegData::GetStyleColor2(const int iplot0, UINT32 *stylecolor20) 
{
int status = -1;
 *stylecolor20 = 0;
 if(iplot0 > 0 && iplot0 <= nplots1) { 
    *stylecolor20 = gseg_plotdata1[iplot0].style_color2; 
    status = 0;
    }
return(status);
}
/***************************************************************************
* Types and styles:
* INPUT:
*  iplot0 : plot index (from 1 to nplots1)
***************************************************************************/
int JLP_GsegData::GetOutlineColor(const int iplot0, UINT32 *outline_color0) 
{
int status = -1;
 *outline_color0 = 0;
 if(iplot0 > 0 && iplot0 <= nplots1) { 
   *outline_color0 = gseg_plotdata1[iplot0].outline_color_rgba; 
   status = 0;
   }
return(status);
}
/***************************************************************************
* Types and styles:
* INPUT:
*  iplot0 : plot index (from 1 to nplots1)
***************************************************************************/
int JLP_GsegData::GetFillColor(const int iplot0, UINT32 *fill_color0) 
{
int status = -1;
 *fill_color0 = 0;
 if(iplot0 > 0 && iplot0 <= nplots1) { 
   *fill_color0 = gseg_plotdata1[iplot0].fill_color_rgba;
   status = 0;
   }
return(status);
}
/****************************************************************************
* Set outline_colors_rgba and fill_colors_rgba
* (called by jlp_GsegData_ReadDataParamFile.cpp)
*
* INPUT:
*  iplot0 : plot index (from 1 to nplots1)
*****************************************************************************/
int JLP_GsegData::SetOutlineAndFillColorFromStyleColor(const int iplot0,
                                                       UINT32 stylecolor2_0,
                                                       char stylechar1_0)
{
char *pchar, buffer[32];
UINT32 canvas_bg_color0;

 if((iplot0 < 1) || (iplot0 > nplots1)) return(-1);

 jlp_gsegraf1->Get_canvas_bg_color(&canvas_bg_color0);

// Set specified outline color 
  gseg_plotdata1[iplot0].outline_color_rgba = stylecolor2_0;
// If stylechar1 is among "ctsiphb", set fill color to background canvas color
  strcpy(buffer, "ctsiphb");
  if ( (pchar = strchr(buffer, stylechar1_0)) != NULL )
    gseg_plotdata1[iplot0].fill_color_rgba = canvas_bg_color0;
/* Else set specified fill color to stylecolor2__0 */
  else
    gseg_plotdata1[iplot0].fill_color_rgba = stylecolor2_0;

return(0);
}
/****************************************************************************
* Set stylecolor2, outline_colors_rgba and fill_colors_rgba
* (called by jlp_GsegData_ReadDataParamFile.cpp)
*
* INPUT:
*  iplot0 : plot index (from 1 to nplots1)
*****************************************************************************/
int JLP_GsegData::SetOutlineAndFillColorFromStyleChars(const int iplot0,
                                                       UINT32 *stylecolor2_0,
                                                       char stylechar1_0, 
                                                       char stylechar2_0)
{
int status = -1, index;
char *pchar, buffer[32];
UINT32 canvas_bg_color0, canvas_fg_color0;

 jlp_gsegraf1->Get_canvas_bg_color(&canvas_bg_color0);
 jlp_gsegraf1->Get_canvas_fg_color(&canvas_fg_color0);

// Initialization by default to foreground color:
 *stylecolor2_0 = canvas_fg_color0;

 if((iplot0 < 1) || (iplot0 > nplots1)) return(-1);

 if((pchar = strchr(color_string, stylechar2_0)) != NULL) {
/* Get index to color character */
    index = pchar - &color_string[0];
/* Get specified color */
    *stylecolor2_0 = color_rgba1[index];
/* Set specified outline color */
    gseg_plotdata1[iplot0].outline_color_rgba = *stylecolor2_0;
// If stylechar1 is among "ctsiphb", set fill color to background canvas color
    strcpy(buffer, "ctsiphb");
    if ( (pchar = strchr(buffer, stylechar1_0)) != NULL )
     gseg_plotdata1[iplot0].fill_color_rgba = canvas_bg_color0;
/* Else set specified fill color to stylecolor2_0 */
    else
     gseg_plotdata1[iplot0].fill_color_rgba = *stylecolor2_0;
   status = 0;
   }

/* DEBUG
printf("SetOutlineAndFillColorFromStyleChars/stylechar1=%c stylechar2=%c stylecolor2=%d\n", 
        stylechar1_0, stylechar2_0, *stylecolor2_0);
*/

return(status);
}
/****************************************************************************
* zblack and zwhite are two decimal numbers
*
* INPUT:
*  iplot0 : plot index (from 1 to nplots1)
*****************************************************************************/
int JLP_GsegData::GetZBlackWhite(const int iplot0, double *zblack0,
                                 double *zwhite0)
{

 *zblack0 = -DBLE_MAX_VALUE;
 *zwhite0 = DBLE_MAX_VALUE;
 if((iplot0 < 1) || (iplot0 > nplots1)) return(-1);

 *zblack0 = gseg_plotdata1[iplot0].zblack;
 *zwhite0 = gseg_plotdata1[iplot0].zwhite;

return(0);
}
/****************************************************************************
* 2d color plot type
* when input string of parameter file is:
* "nearest zblack zwhite" or "bilinear zblack zwhite"
* where zblack and zwhite are two decimal numbers
*
* INPUT:
*  iplot0 : plot index (from 1 to nplots1)
*****************************************************************************/
int JLP_GsegData::SetZBlackWhite(const int iplot0, const double zblack_0,
                                 const double zwhite_0)
{

 if((iplot0 < 1) || (iplot0 > nplots1)) return(-1);

 gseg_plotdata1[iplot0].zblack = zblack_0;
 gseg_plotdata1[iplot0].zwhite = zwhite_0;

return(0);
}
/****************************************************************************
*
* INPUT:
*  iplot0 : plot index (from 1 to nplots1)
*****************************************************************************/
int JLP_GsegData::GetAlphaColor(const int iplot0, UINT32 *alpha_color0) 
{
 *alpha_color0 = 0;
 if((iplot0 < 1) || (iplot0 > nplots1)) return(-1);
 *alpha_color0 = gseg_plotdata1[iplot0].alpha_color;
return(0);
}
/****************************************************************************
*
* INPUT:
*  iplot0 : plot index (from 1 to nplots1)
*****************************************************************************/
int JLP_GsegData::SetAlphaColor(const int iplot0, const UINT32 alphacolor_0)
{

 if((iplot0 < 1) || (iplot0 > nplots1)) return(-1);

/* Check alpha value */
   if(alphacolor_0 > 0xFFFFFFFF ) 
     gseg_plotdata1[iplot0].alpha_color = 0xFFFFFFFF;  // white
   else 
     gseg_plotdata1[iplot0].alpha_color = alphacolor_0;

return(0);
}
/****************************************************************************
*
* INPUT:
*  iplot0 : plot index (from 1 to nplots1)
*****************************************************************************/
int JLP_GsegData::GetContour3dColor(const int iplot0, UINT32 *contour3d_color0)
{
 *contour3d_color0 = 0;
 if((iplot0 < 1) || (iplot0 > nplots1)) return(-1);
   *contour3d_color0 = gseg_plotdata1[iplot0].contour3d_color;
return(0);
}
/****************************************************************************
*
* INPUT:
*  iplot0 : plot index (from 1 to nplots1)
*****************************************************************************/
int JLP_GsegData::GetMeshColor(const int iplot0, UINT32 *mesh_color0) 
{
 *mesh_color0 = 0;
 if((iplot0 < 1) || (iplot0 > nplots1)) return(-1);
   *mesh_color0 = gseg_plotdata1[iplot0].mesh_color;
return(0);
}
/*****************************************************************************
* Determine the DrawSymbol option
* by decoding stylechar1 with symbol_string ("ld.cCtTsSiIpPhH+xra")
*
* INPUT:
*  iplot : plot index (from 1 to nplots1)
* OUTPUT:
*  ifunc0 : DrawSymbol1/DrawSymbol2 option
*****************************************************************************/
int JLP_GsegData::DrawSymbolOptionFromStyleChar1(const int iplot0, 
                                                 int *ifunc0)
{
int status = -1, is_upper_case0;
char stylechar1_0;

 *ifunc0 = 0;
 if((iplot0 < 1) || (iplot0 > nplots1)) {
   fprintf(stderr, "DrawSymbolOptionFromStyleChar1/Error iplot0=%d\n", iplot0);
   return(-1);
   }

// Retrieve stylechar1 of plot
 status = GetStyleChar1(iplot0, &stylechar1_0);
 if(status) {
  fprintf(stderr, "DrawSymbolOptionFromStyleChar1/GetStyleChar1/Error: stylechar1_0=%c\n",
          stylechar1_0);
    return(-1);
  }

// Decode it using symbol_string ("ld.cCtTsSiIpPhH+xra")
 status = DrawSymbolOptionFromChar(stylechar1_0, 4, ifunc0, &is_upper_case0);

return(status);
}
/*****************************************************************************
* Determine the DrawSymbol1 option
* by decoding stylechar1 with symbol_string1, ("cCtTsSiIpPhH");
* (NB: ""+xra" decoded in DrawSymbolr21OptionFromStyleChar1) 
*
* INPUT:
*  iplot : plot index (from 1 to nplots1)
* OUTPUT:
*  ifunc0 : DrawSymbol1 option
*****************************************************************************/
int JLP_GsegData::DrawSymbol1OptionFromStyleChar1(const int iplot0, int *ifunc0)
{
int status = -1, is_upper_case0;
char stylechar1_0;

 *ifunc0 = 0;
 if((iplot0 < 1) || (iplot0 > nplots1)) {
   fprintf(stderr, "SymbolFromStyleChar1/Error iplot0=%d\n", iplot0);
   return(-1);
   }

// Retrieve stylechar1 of plot
 status = GetStyleChar1(iplot0, &stylechar1_0);
 if(status) return(-1);

// Decode it using symbol_string1 ("cCtTsSiIpPhH")
 status = DrawSymbolOptionFromChar(stylechar1_0, 1, ifunc0, &is_upper_case0);
#ifdef DEBUG
 if(status == 0) {
   printf("DrawSymbol1OptionFromStyleChar1/Debug: stylechar1=%c in cCtTsSiIpPhH gives ifunc0=%d\n",
        stylechar1_0, *ifunc0); 
   }
#endif

return(status);
}
/*****************************************************************************
* Determine the DrawSymbol1/DrawSymbol2 option
* by decoding stylechar0 with symbol_str
*
* INPUT:
*  isymb : 0 for symbol_string, 1 for symbol_string1, 2 for symbol_string2,
*          3 for "lbB" (for histograms)
*
* OUTPUT:
*  ifunc0 : DrawSymbol1/DrawSymbol2 option
*****************************************************************************/
int JLP_GsegData::DrawSymbolOptionFromChar(const char stylechar0, 
                                           const int isymb,
                                           int *ifunc0, int *is_upper_case0)
{
int status = -1;
char *pchar, symbol_str[64];

 *ifunc0 = 0;
 *is_upper_case0 = 0;

 switch (isymb) {
// symbol_string ("ld.cCtTsSiIpPhH")
   case 0:
    strcpy(symbol_str, "ld.cCtTsSiIpPhH");
    break;
// symbol_string1 ("cCtTsSiIpPhH")
   case 1:
    strcpy(symbol_str, "cCtTsSiIpPhH");
    break;
// symbol_string2 ("+xra")
   case 2: 
    strcpy(symbol_str, "+xra");
    break;
// for histograms ("lbB") 
   case 3:
    strcpy(symbol_str, "lbB");
    break;
// symbol_string ("ld.cCtTsSiIpPhH+xra")
   default:
   case 4:
    strcpy(symbol_str, "ld.cCtTsSiIpPhH+xra");
    break;
   case 5:
// symbol_string ("cCtTsSiIpPhH+xra")
    strcpy(symbol_str, "cCtTsSiIpPhH+xra");
    break;
  }

// Decode stylechar0 in symbol_str
// Return the character string starting with stylechar0 from symbol_str string:
 pchar = strchr(symbol_str, stylechar0);
 if(pchar != NULL) {
   *ifunc0 = pchar - symbol_str;
   if( isupper(*pchar) == 0 ) *is_upper_case0 = 1; 
   status = 0;
/* It is not an error in many cases, since this routine may be used 
*  to check if style_char1 is encoding a symbol...
   } else {
   fprintf(stderr, "DrawSymbolOptionFromChar/Error: stylechar0= >%c< not found in >%s<\n",
           stylechar0, symbol_str);
*/
   }

return(status);
}
/*****************************************************************************
* Determine the DrawSymbol2 option
* by decoding stylechar1 in the symbol_string2 ("+xra")
*
* INPUT:
*  iplot0 : plot index (from 1 to nplots1)
* OUTPUT:
*  ifunc0 : DrawSymbol2 option
*****************************************************************************/
int JLP_GsegData::DrawSymbol2OptionFromStyleChar1(const int iplot0, int *ifunc0)
{
int status = -1, is_upper_case0;
char stylechar1_0;
 
 *ifunc0 = 0;
 if((iplot0 < 1) || (iplot0 > nplots1)) {
   fprintf(stderr, "DrawSymbol2FromStyleChar1/Error iplot0=%d\n", iplot0);
   return(-1);
   }

// Retrieve stylechar of plot 
 status = GetStyleChar1(iplot0, &stylechar1_0);
 if(status) {
   fprintf(stderr, "DrawSymbol2FromStyleChar1/Error in GetStyleChar1=%d\n", iplot0);
  return(-1);
  }

// Decode it using symbol_string2 ("+xra")
 status = DrawSymbolOptionFromChar(stylechar1_0, 2, ifunc0, &is_upper_case0);
#ifdef DEBUG
 if(status == 0) {
   printf("DrawSymbol2OptionFromStyleChar1/Debug: stylechar1=%c in +xra gives ifunc0=%d\n",
        stylechar1_0, *ifunc0); 
   }
#endif

return(status);
}
/****************************************************************************
* Get stylecolor1/2 from stylechar1/2
* (called by jlp_GsegData_ReadDataParamFile.cpp)
* stylechar compared to  color_string = "kaswrylqbfmogtnpx"
*****************************************************************************/
int JLP_GsegData::GetStyleColorFromStyleChar(char style_char0,
                                             UINT32 *style_color0)
{
int status = -1, index;
char *pchar;
UINT32 canvas_fg_color0;

 jlp_gsegraf1->Get_canvas_fg_color(&canvas_fg_color0);

*style_color0 = canvas_fg_color0;

// color_string (= "kaswrylqbfmogtnpx")
 if((pchar = strchr(color_string, style_char0)) != NULL) {
/* Get index to color character */
    index = pchar - &color_string[0];
/* Get specified color */
    *style_color0 = color_rgba1[index];
   status = 0;
   }

/* DEBUG:
printf("GetStyleColorFromStyleChar/style_char0=%c style_color0=%d\n", 
        style_char0, *style_color0);
*/

return(status);
}
/*****************************************************************************
* Determine the style color 
* by decoding stylechar2 with color_string = "kaswrylqbfmogtnpx"
*
* INPUT:
*  iplot : plot index (from 1 to nplots1)
* OUTPUT:
*  style_color0 : style color option
*****************************************************************************/
int JLP_GsegData::GetStyleColorFromStyleChar2(const int iplot0, 
                                               UINT32 *style_color0)
{
int status = -1;
char style_char10;

 *style_color0 = 0;

 if((iplot0 < 1) || (iplot0 > nplots1)) {
   fprintf(stderr, "GetColorFromStyleChar1/Error iplot0=%d\n", iplot0);
   return(-1);
   }

// Retrieve style_char1 of plot
 status = GetStyleChar1(iplot0, &style_char10);
 if(status) return(-1);

// Decode it using color_string = "kaswrylqbfmogtnpx" 
 status = GetStyleColorFromStyleChar(style_char10, style_color0);

return(status);
}
