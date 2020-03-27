/*******************************************************************************
*
* DrawSymbols1.c
*
* Contains functions:
*    DrawSymbol1(isymb1,...)
*    0,1=jlp_gseg1->GSEG_DrawCircle
*    2,3=GSEG_DrawTriangle
*    4,5=jlp_gseg1->GSEG_DrawSquare
*    6,7=DrawDiamond
*    8,9=DrawPentagon
*    10,11=DrawHexagon
*
*    DrawSymbol2(isymb2,...)
*    0=DrawPlus
*    1=DrawX
*    2=DrawStar
*    3=DrawAsterisk
*
*******************************************************************************/
#include <math.h>
#include "jlp_gsegraf.h"

/************************************************************************
*    DrawSymbol1(isymb1,...)
*    0,1=jlp_gseg1->GSEG_DrawCircle
*    2,3=GSEG_DrawTriangle
*    4,5=jlp_gseg1->GSEG_DrawSquare
*    6,7=DrawDiamond
*    8,9=DrawPentagon
*    10,11=DrawHexagon
* 
* isymb1 was obtained by decoding stylechar with symbol_string1:("cCtTsSiIpPhH")
*
************************************************************************/
void JLP_Gsegraf::DrawSymbol1(const int isymb1, double x0, double y0, 
                 UINT32 fill_color_rgba0, UINT32 outline_color_rgba0, 
                 unsigned int size0)
{
int linewidth0 = 1;
UINT32 fill_color_rgba2, fill_with_transparent_rgba = 0xFFFFFF00;

fill_color_rgba2 = fill_color_rgba0;

 switch(isymb1) {
   default:
// Empty circle:
   case 0:
     fill_color_rgba2 = fill_with_transparent_rgba;
// Filled circle:
   case 1:
     jlp_gseg1->GSEG_DrawCircle(x0, y0, size0, fill_color_rgba2,
                                outline_color_rgba0, linewidth0);
     break;
// Empty triangle:
   case 2:
     fill_color_rgba2 = fill_with_transparent_rgba;
// Filled triangle:
   case 3:
     GSEG_DrawTriangle(x0, y0, fill_color_rgba2, outline_color_rgba0, size0);
     break;
// Empty square:
   case 4:
     fill_color_rgba2 = fill_with_transparent_rgba;
// Filled square:
   case 5:
     jlp_gseg1->GSEG_DrawSquare(x0, y0, size0, fill_color_rgba2, 
                                outline_color_rgba0, linewidth0);
     break;
// Empty diamond:
   case 6:
     fill_color_rgba2 = fill_with_transparent_rgba;
// Filled diamond:
   case 7:
     DrawDiamond(x0, y0, fill_color_rgba2, outline_color_rgba0, size0);
     break;
// Empty pentagon:
   case 8:
     fill_color_rgba2 = fill_with_transparent_rgba;
// Filled pentagon:
   case 9:
     DrawPentagon(x0, y0, fill_color_rgba2, outline_color_rgba0, size0);
     break;
// Empty hexagon:
   case 10:
     fill_color_rgba2 = fill_with_transparent_rgba;
// Filled hexagon:
   case 11:
     DrawHexagon(x0, y0, fill_color_rgba2, outline_color_rgba0, size0);
     break;
 }
return;
}
/************************************************************************
*    DrawSymbol2(isymb2,...)
*    0=DrawPlus
*    1=DrawX
*    2=DrawStar
*    3=DrawAsterisk
*
* isymb2 was obtained by decoding stylechar with symbol_string2: ("+xra")
*
************************************************************************/
void JLP_Gsegraf::DrawSymbol2(const int isymb2, double x0, double y0, 
                 UINT32 outline_color_rgba0, unsigned int size0)
{
 switch(isymb2) {
   default:
   case 0:
     DrawPlus(x0, y0, outline_color_rgba0, size0);
     break;
   case 1:
     DrawX(x0, y0, outline_color_rgba0, size0);
     break;
   case 2:
     DrawStar(x0, y0, outline_color_rgba0, size0);
     break;
   case 3:
     DrawAsterisk(x0, y0, outline_color_rgba0, size0);
     break;
 }
return;
}
/************************************************************************
*
************************************************************************/
void JLP_Gsegraf::GSEG_DrawTriangle ( double x, double y, UINT32 fill_color_rgba,
                    UINT32 outline_color_rgba, unsigned int size )
   {
   /* Declare variables */
   double r, dx, dy;
   JLP_CanvasPoints *points;


   /* Draw triangle */
   if ( size == 0 )
      return;
   else
      {
      r = 0.666666667*size;
      dx = 0.577350269*size;
      dy = 0.333333333*size;

      points = jlp_canvas_points_new(3);
      points->coords[0] = x;
      points->coords[1] = y - r;
      points->coords[2] = x - dx;
      points->coords[3] = y + dy;
      points->coords[4] = x + dx;
      points->coords[5] = y + dy;

// Draw the triangle
      jlp_gseg1->GSEG_DrawPolygon(points, fill_color_rgba, outline_color_rgba, 1);

      jlp_canvas_points_free(points);
      }
   }
/************************************************************************
*
************************************************************************/
void JLP_Gsegraf::DrawDiamond ( double x, double y, UINT32 fill_color_rgba,
                   UINT32 outline_color_rgba, unsigned int size )
   {
   /* Declare variables */
   double r;
   JLP_CanvasPoints *points;


   /* Draw diamond */
   if ( size == 0 )
      return;
   else
      {
      r = 0.707106781*size;

      points = jlp_canvas_points_new(4);
      points->coords[0] = x;
      points->coords[1] = y - r;
      points->coords[2] = x - r;
      points->coords[3] = y;
      points->coords[4] = x;
      points->coords[5] = y + r;
      points->coords[6] = x + r;
      points->coords[7] = y;

// Draw the diamond 
      jlp_gseg1->GSEG_DrawPolygon(points, fill_color_rgba, outline_color_rgba, 1);

      jlp_canvas_points_free(points);
      }
   }

/************************************************************************
*
************************************************************************/
void JLP_Gsegraf::DrawPentagon ( double x, double y, UINT32 fill_color_rgba,
                    UINT32 outline_color_rgba, unsigned int size )
   {
   /* Declare variables */
   double r, dx1, dx2, dy1, dy2;
   JLP_CanvasPoints *points;


   /* Draw pentagon */
   if ( size == 0 )
      return;
   else
      {
      r = 0.618033988*size;
      dx1 = 0.587785252*size;
      dy1 = 0.190983005*size;
      dx2 = 0.363271264*size;
      dy2 = 0.5*size;

      points = jlp_canvas_points_new(5);
      points->coords[0] = x;
      points->coords[1] = y - r;
      points->coords[2] = x - dx1;
      points->coords[3] = y - dy1;
      points->coords[4] = x - dx2;
      points->coords[5] = y + dy2;
      points->coords[6] = x + dx2;
      points->coords[7] = y + dy2;
      points->coords[8] = x + dx1;
      points->coords[9] = y - dy1;

// Draw the pentagon 
      jlp_gseg1->GSEG_DrawPolygon(points, fill_color_rgba, outline_color_rgba, 1);

      jlp_canvas_points_free(points);
      }
   }

/************************************************************************
*
************************************************************************/
void JLP_Gsegraf::DrawHexagon ( double x, double y, UINT32 fill_color_rgba,
                   UINT32 outline_color_rgba, unsigned int size )
   {
   /* Declare variables */
   double r, dx, dy;
   JLP_CanvasPoints *points;


   /* Draw hexagon */
   if ( size == 0 )
      return;
   else
      {
      r = 0.577350269*size;
      dx = 0.288675134*size;
      dy = 0.5*size;

      points = jlp_canvas_points_new(6);
      points->coords[0]  = x - dx;
      points->coords[1]  = y - dy;
      points->coords[2]  = x - r;
      points->coords[3]  = y;
      points->coords[4]  = x - dx;
      points->coords[5]  = y + dy;
      points->coords[6]  = x + dx;
      points->coords[7]  = y + dy;
      points->coords[8]  = x + r;
      points->coords[9]  = y;
      points->coords[10] = x + dx;
      points->coords[11] = y - dy;

// Draw the hexagon 
      jlp_gseg1->GSEG_DrawPolygon(points, fill_color_rgba, outline_color_rgba, 1);

      jlp_canvas_points_free(points);
      }
   }

/**************************************************************************
*
**************************************************************************/
void JLP_Gsegraf::DrawPlus(double x, double y, UINT32 outline_color_rgba, 
                           unsigned int size)
{
   /* Declare variables */
   double r;
   JLP_CanvasPoints *points;
   int line_width_one = 1;


   /* Draw plus sign */
   if ( size == 0 )
      return;
   else
      {
      r = 0.707106781*size;

      points = jlp_canvas_points_new(2);
      points->coords[0] = x;
      points->coords[1] = y - r;
      points->coords[2] = x;
      points->coords[3] = y + r;

      jlp_gseg1->GSEG_DrawLine(points, outline_color_rgba, line_width_one);

      points->coords[0] = x - r;
      points->coords[1] = y;
      points->coords[2] = x + r;
      points->coords[3] = y;

      jlp_gseg1->GSEG_DrawLine(points, outline_color_rgba, line_width_one);

      jlp_canvas_points_free(points);
      }

return;
}

/************************************************************************
*
************************************************************************/
void JLP_Gsegraf::DrawX ( double x, double y, UINT32 outline_color_rgba, 
                          unsigned int size )
{
   /* Declare variables */
   double dx, dy;
   JLP_CanvasPoints *points;
   int line_width_one = 1;

   /* Draw x */
   if ( size == 0 )
      return;
   else
      {
      dx = 0.5*size;
      dy = 0.5*size;

      points = jlp_canvas_points_new(2);
      points->coords[0] = x - dx;
      points->coords[1] = y - dy;
      points->coords[2] = x + dx;
      points->coords[3] = y + dy;

      jlp_gseg1->GSEG_DrawLine(points, outline_color_rgba, line_width_one);

      points->coords[0] = x + dx;
      points->coords[1] = y - dy;
      points->coords[2] = x - dx;
      points->coords[3] = y + dy;

      jlp_gseg1->GSEG_DrawLine(points, outline_color_rgba, line_width_one);

      jlp_canvas_points_free(points);
      }

return;
}

/************************************************************************
*
************************************************************************/
void JLP_Gsegraf::DrawStar(double x, double y, UINT32 outline_color_rgba, 
                           unsigned int size)
{
   /* Declare variables */
   double r, dx1, dx2, dy1, dy2;
   JLP_CanvasPoints *points;
   int line_width_one = 1;

   /* Draw star */
   if ( size == 0 )
      return;
   else
      {
      r = 0.618033988*size;
      dx1 = 0.587785252*size;
      dy1 = 0.190983005*size;
      dx2 = 0.363271264*size;
      dy2 = 0.5*size;

      points = jlp_canvas_points_new(3);
      points->coords[0] = x;
      points->coords[1] = y - r;
      points->coords[2] = x;
      points->coords[3] = y;
      points->coords[4] = x - dx2;
      points->coords[5] = y + dy2;

      jlp_gseg1->GSEG_DrawLine(points, outline_color_rgba, line_width_one);

      points->coords[0] = x;
      points->coords[1] = y - r;
      points->coords[2] = x;
      points->coords[3] = y;
      points->coords[4] = x + dx2;
      points->coords[5] = y + dy2;

      jlp_gseg1->GSEG_DrawLine(points, outline_color_rgba, line_width_one);

      points->coords[0] = x - dx1;
      points->coords[1] = y - dy1;
      points->coords[2] = x;
      points->coords[3] = y;
      points->coords[4] = x + dx1;
      points->coords[5] = y - dy1;

      jlp_gseg1->GSEG_DrawLine(points, outline_color_rgba, line_width_one);

      jlp_canvas_points_free(points);
      }

return;
}

/************************************************************************
*
************************************************************************/
void JLP_Gsegraf::DrawAsterisk(double x, double y, UINT32 outline_color_rgba, 
                  unsigned int size)
{
   /* Declare variables */
   double r, dx, dy;
   JLP_CanvasPoints *points;
   int line_width_one = 1;


   /* Draw asterisk */
   if ( size == 0 )
      return;
   else
      {
      r = 0.577350269*size;
      dx = 0.5*size;
      dy = 0.288675134*size;

      points = jlp_canvas_points_new(2);
      points->coords[0] = x;
      points->coords[1] = y - r;
      points->coords[2] = x;
      points->coords[3] = y + r;

      jlp_gseg1->GSEG_DrawLine(points, outline_color_rgba, line_width_one);

      points->coords[0] = x + dx;
      points->coords[1] = y + dy;
      points->coords[2] = x - dx;
      points->coords[3] = y - dy;

      jlp_gseg1->GSEG_DrawLine(points, outline_color_rgba, line_width_one);

      points->coords[0] = x - dx;
      points->coords[1] = y + dy;
      points->coords[2] = x + dx;
      points->coords[3] = y - dy;

      jlp_gseg1->GSEG_DrawLine(points, outline_color_rgba, line_width_one);

      jlp_canvas_points_free(points);
      }

return;
}
