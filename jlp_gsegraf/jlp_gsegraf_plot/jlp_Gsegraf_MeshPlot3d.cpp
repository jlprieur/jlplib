/*******************************************************************************
*
* jlp_Gsegraf_MeshPlot3d.cpp
*
* Plots a two-dimensional projection of three-dimensional mesh data.
*
* Copyright © 2008, 2009, 2010, 2011 Spencer A. Buckner
* http://savannah.gnu.org/projects/gsegrafix
*
* This file is part of GSEGrafix, a scientific and engineering plotting program.
*
* This program is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with this program.  If not, see <http://www.gnu.org/licenses/>.
*
* JLP
* Version 17/04/2017
*******************************************************************************/
#include "jlp_gsegraf.h"
#include "jlp_gseg_axes.h"     // JLP_GsegAxes class
#include "jlp_gseg_data.h"     // JLP_GsegData class

/*****************************************************************************
*
*****************************************************************************/
void JLP_Gsegraf::MeshPlot3d(int iplot)
{
/* Declare variables */
 int i, j, quadrant0, nx, ny, style_flag0;
 UINT32 fill_color[2];
 double xmin, xmax, ymin, ymax, zmin, zmax, xscale, yscale, zscale, 
        origin[3], xpoints[4], ypoints[4], zpoints[4];
 double *xmesh0, *ymesh0, *zmesh0;
 double Ry[9], Rz[9], Ryz[9];
 char style_char10, style_char20;
 UINT32 style_color10, style_color20, color_rgba0;

// Style parameters:
 jlp_gseg_data1->GetStyleFlag(iplot, &style_flag0);
// jlp_gseg_data1->GetStyleColor1(iplot, &style_color10);
 jlp_gseg_data1->GetMeshColor(iplot, &style_color10);
 jlp_gseg_data1->GetStyleColor2(iplot, &style_color20);
 jlp_gseg_data1->GetStyleChar1(iplot, &style_char10);
 jlp_gseg_data1->GetStyleChar2(iplot, &style_char20);

// Get plot settings
 jlp_gseg_axes1->GetBoxSettingsFor3d(origin, Ry, Rz, Ryz, &xmin, &xmax, &ymin,
                                     &ymax, &zmin, &zmax, &xscale, &yscale,
                                     &zscale);

 fill_color[0] = style_color10;
 fill_color[1] = style_color20;

/* Get fill colors */
// style_flag = 2 : two colors encoded with chars
 if ( style_flag0 == 2 )
    {
// color corresponding to style_char10:
      if ( jlp_gseg_data1->GetStyleColorFromStyleChar(style_char10,
                                                         &color_rgba0) == 0 )
        fill_color[0] = color_rgba0;
// color corresponding to style_char20:
      if ( jlp_gseg_data1->GetStyleColorFromStyleChar(style_char20,
                                                         &color_rgba0) == 0 )
        fill_color[1] = color_rgba0;
    }
// style_flag = 2 : color#0 encoded with char
 else if ( style_flag0 == 4 )
    {
// color corresponding to style_char10:
      if ( jlp_gseg_data1->GetStyleColorFromStyleChar(style_char10,
                                                         &color_rgba0) == 0 )
        fill_color[0] = color_rgba0;
        fill_color[1] = style_color20;
      }
// style_flag = 2 : color#1 encoded with char
   else if ( style_flag0 == 5 )
      {
      fill_color[0] = style_color10;
// color corresponding to style_char20:
      if ( jlp_gseg_data1->GetStyleColorFromStyleChar(style_char20,
                                                         &color_rgba0) == 0 )
        fill_color[1] = color_rgba0;
      }
// style_flag = 2 : two colors read directly from style_color1, style_color2
   else if ( style_flag0 == 6 )
      {
      fill_color[0] = style_color10;
      fill_color[1] = style_color20;
      }

 xmesh0 = jlp_gseg_data1->XMeshPtr(iplot);
 ymesh0 = jlp_gseg_data1->YMeshPtr(iplot);
 zmesh0 = jlp_gseg_data1->ZMeshPtr(iplot);
 nx = jlp_gseg_data1->NXMesh(iplot);
 ny = jlp_gseg_data1->NYMesh(iplot);

/* Plot data */
 jlp_gseg_axes1->Get3dQuadrant(&quadrant0);
/***************** Quadrant==1 *******************************************/
 if (quadrant0 == 1 )
    {
    for ( i=1; i<nx; i++ )
       {
       xpoints[0] = xmesh0[i-1];
       xpoints[1] = xmesh0[i];
       xpoints[2] = xmesh0[i];
       xpoints[3] = xmesh0[i-1];

/* All x coordinates within range */
       if ( (xmin <= xpoints[0] && xpoints[0] <= xmax) &&
            (xmin <= xpoints[1] && xpoints[1] <= xmax) )
          {
          for ( j=1; j<ny; j++ )
             {
             ypoints[0] = ymesh0[j-1];
             ypoints[1] = ymesh0[j-1];
             ypoints[2] = ymesh0[j];
             ypoints[3] = ymesh0[j];

/* All y coordinates within range */
             if ( (ymin <= ypoints[1] && ypoints[1] <= ymax) &&
                  (ymin <= ypoints[2] && ypoints[2] <= ymax) )
                {
                zpoints[0] = zmesh0[ny*(i-1)+j-1];
                zpoints[1] = zmesh0[ny*i+j-1];
                zpoints[2] = zmesh0[ny*i+j];
                zpoints[3] = zmesh0[ny*(i-1)+j];

// All z coordinates within range: plot "normal" polygon 
                if ( (zmin <= zpoints[0] && zpoints[0] <= zmax) &&
                     (zmin <= zpoints[1] && zpoints[1] <= zmax) &&
                     (zmin <= zpoints[2] && zpoints[2] <= zmax) &&
                     (zmin <= zpoints[3] && zpoints[3] <= zmax) )
                   PlotNormal3d(iplot, xmin, ymin, zmin, zmax, xscale, 
                                yscale, zscale,
                                &origin[0], &Ryz[0], &fill_color[0],
                                &xpoints[0], &ypoints[0], &zpoints[0]);

/* Not all z coordinates within range:
 Plot truncated polygon with the interpolated points 
  (when data points are on the edges, outside the frame)
*/
                else
                   PlotInterp3d(iplot, xmin, ymin, zmin, zmax, xscale, 
                                yscale, zscale,
                                &origin[0], &Ryz[0], &fill_color[0],
                                &xpoints[0], &ypoints[0], &zpoints[0]);
                }
             }
          }
       }
    }

/***************** Quadrant==2 *******************************************/
 else if ( quadrant0 == 2 )
    {
    for ( i=nx-1; i>0; i-- )
       {
       xpoints[0] = xmesh0[i-1];
       xpoints[1] = xmesh0[i];
       xpoints[2] = xmesh0[i];
       xpoints[3] = xmesh0[i-1];

/* All x coordinates within range */
       if ( (xmin <= xpoints[0] && xpoints[0] <= xmax) &&
            (xmin <= xpoints[1] && xpoints[1] <= xmax) )
          {
          for ( j=1; j<ny; j++ )
             {
             ypoints[0] = ymesh0[j-1];
             ypoints[1] = ymesh0[j-1];
             ypoints[2] = ymesh0[j];
             ypoints[3] = ymesh0[j];

/* All y coordinates within range */
             if ( (ymin <= ypoints[1] && ypoints[1] <= ymax) &&
                  (ymin <= ypoints[2] && ypoints[2] <= ymax) )
                {
                zpoints[0] = zmesh0[ny*(i-1)+j-1];
                zpoints[1] = zmesh0[ny*i+j-1];
                zpoints[2] = zmesh0[ny*i+j];
                zpoints[3] = zmesh0[ny*(i-1)+j];

// All z coordinates within range: plot "normal" polygon 
                if ( (zmin <= zpoints[0] && zpoints[0] <= zmax) &&
                     (zmin <= zpoints[1] && zpoints[1] <= zmax) &&
                     (zmin <= zpoints[2] && zpoints[2] <= zmax) &&
                     (zmin <= zpoints[3] && zpoints[3] <= zmax) )
                   PlotNormal3d(iplot, xmin, ymin, zmin, zmax, xscale, 
                                yscale, zscale,
                                &origin[0], &Ryz[0], &fill_color[0],
                                &xpoints[0], &ypoints[0], &zpoints[0]);

/* Not all z coordinates within range:
 Plot truncated polygon with the interpolated points 
  (when data points are on the edges, outside the frame)
*/
                else
                   PlotInterp3d(iplot, xmin, ymin, zmin, zmax, xscale, 
                                yscale, zscale,
                                &origin[0], &Ryz[0], &fill_color[0],
                                &xpoints[0], &ypoints[0], &zpoints[0]);
                }
             }
          }
       }
    }

/***************** Quadrant==3 *******************************************/
 else if ( quadrant0 == 3 )
    {
    for ( i=nx-1; i>0; i-- )
       {
       xpoints[0] = xmesh0[i-1];
       xpoints[1] = xmesh0[i];
       xpoints[2] = xmesh0[i];
       xpoints[3] = xmesh0[i-1];

/* All x coordinates within range */
       if ( (xmin <= xpoints[0] && xpoints[0] <= xmax) &&
            (xmin <= xpoints[1] && xpoints[1] <= xmax) )
          {
          for ( j=ny-1; j>0; j-- )
             {
             ypoints[0] = ymesh0[j-1];
             ypoints[1] = ymesh0[j-1];
             ypoints[2] = ymesh0[j];
             ypoints[3] = ymesh0[j];

/* All y coordinates within range */
             if ( (ymin <= ypoints[1] && ypoints[1] <= ymax) &&
                  (ymin <= ypoints[2] && ypoints[2] <= ymax) )
                {
                zpoints[0] = zmesh0[ny*(i-1)+j-1];
                zpoints[1] = zmesh0[ny*i+j-1];
                zpoints[2] = zmesh0[ny*i+j];
                zpoints[3] = zmesh0[ny*(i-1)+j];

// All z coordinates within range: plot "normal" polygon 
                if ( (zmin <= zpoints[0] && zpoints[0] <= zmax) &&
                     (zmin <= zpoints[1] && zpoints[1] <= zmax) &&
                     (zmin <= zpoints[2] && zpoints[2] <= zmax) &&
                     (zmin <= zpoints[3] && zpoints[3] <= zmax) )
                   PlotNormal3d(iplot, xmin, ymin, zmin, zmax, xscale, 
                                yscale, zscale,
                                &origin[0], &Ryz[0], &fill_color[0],
                                &xpoints[0], &ypoints[0], &zpoints[0]);

/* Not all z coordinates within range:
 Plot truncated polygon with the interpolated points 
  (when data points are on the edges, outside the frame)
*/
                else
                   PlotInterp3d(iplot, xmin, ymin, zmin, zmax, xscale, 
                                yscale, zscale,
                                &origin[0], &Ryz[0], &fill_color[0],
                                &xpoints[0], &ypoints[0], &zpoints[0]);
                }
             }
         }
      }
    }
/***************** Quadrant==4 *******************************************/
 else if ( quadrant0 == 4 )
    {
    for ( i=1; i<nx; i++ )
       {
       xpoints[0] = xmesh0[i-1];
       xpoints[1] = xmesh0[i];
       xpoints[2] = xmesh0[i];
       xpoints[3] = xmesh0[i-1];

/* All x coordinates within range */
       if ( (xmin <= xpoints[0] && xpoints[0] <= xmax) &&
            (xmin <= xpoints[1] && xpoints[1] <= xmax) )
          {
          for ( j=ny-1; j>0; j-- )
             {
             ypoints[0] = ymesh0[j-1];
             ypoints[1] = ymesh0[j-1];
             ypoints[2] = ymesh0[j];
             ypoints[3] = ymesh0[j];

/* All y coordinates within range */
             if ( (ymin <= ypoints[1] && ypoints[1] <= ymax) &&
                  (ymin <= ypoints[2] && ypoints[2] <= ymax) )
                {
                zpoints[0] = zmesh0[ny*(i-1)+j-1];
                zpoints[1] = zmesh0[ny*i+j-1];
                zpoints[2] = zmesh0[ny*i+j];
                zpoints[3] = zmesh0[ny*(i-1)+j];

// All z coordinates within range: plot "normal" polygon 
                if ( (zmin <= zpoints[0] && zpoints[0] <= zmax) &&
                     (zmin <= zpoints[1] && zpoints[1] <= zmax) &&
                     (zmin <= zpoints[2] && zpoints[2] <= zmax) &&
                     (zmin <= zpoints[3] && zpoints[3] <= zmax) )
                   PlotNormal3d(iplot, xmin, ymin, zmin, zmax, xscale, 
                                yscale, zscale,
                                &origin[0], &Ryz[0], &fill_color[0],
                                &xpoints[0], &ypoints[0], &zpoints[0]);

/* Not all z coordinates within range:
 Plot truncated polygon with the interpolated points 
  (when data points are on the edges, outside the frame)
*/
                else
                   PlotInterp3d(iplot, xmin, ymin, zmin, zmax, xscale, 
                                yscale, zscale,
                                &origin[0], &Ryz[0], &fill_color[0],
                                &xpoints[0], &ypoints[0], &zpoints[0]);
                }
             }
          }
       }
    }

return;
}
