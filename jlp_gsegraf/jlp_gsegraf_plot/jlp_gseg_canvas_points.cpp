/*******************************************************************************
* jlp_canvas_points.cpp
*
* Declares JLP equivalent definitions of some drawing GNOME functions 
*
* JLP_CanvasPoints *jlp_canvas_points_new(const int npts0);
* void JLP_Gseg::jlp_canvas_points_free(JLP_CanvasPoints *jlp_canvas_points0);
* void JLP_Gseg::JLP_ErrorDialog(const char *error_str);
*
* JLP
* Version 29/10/2016
*******************************************************************************/
#include <stdio.h>

#include "jlp_gsegraf_defs.h"

/************************************************************************ 
* Re-definition of GnomeCanvasPoints structure
* The coords field contains an array of points, 
* (alternating X and Y coordinates). 
* You fill the coords array directly, after creating a GnomeCanvasPoints 
* with gnome_canvas_points_new(); 
* the structure should be destroyed with gnome_canvas_points_unref().
* NB: the GnomeCanvasPoints structure is actually managed by a reference count.
* so it won't be freed until this count reaches 0. 
* To increment its reference count call gnome_canvas_points_ref() 
* and to decrement it call gnome_canvas_points_unref(). 
*************************************************************************/

/***********************************************************************
* Re-definition of gnome_canvas_points_new(const int npts0) function
*
************************************************************************/
JLP_CanvasPoints *jlp_canvas_points_new(const int npts0)
{
JLP_CanvasPoints *jlp_canvas_points0;

jlp_canvas_points0 = new JLP_CanvasPoints;

jlp_canvas_points0->num_points = npts0;
jlp_canvas_points0->coords = new double[npts0 * 2];

return(jlp_canvas_points0);
}

/***********************************************************************
* Re-definition of gnome_canvas_points_free() function
*
************************************************************************/
void jlp_canvas_points_free(JLP_CanvasPoints *jlp_canvas_points0)
{

delete[] jlp_canvas_points0->coords;
jlp_canvas_points0->num_points = 0;
jlp_canvas_points0->coords = NULL;

return;
}
