/***********************************************************************
* jlp_wxgdev_shapes.cpp
* JLP_wxGDevShapes class defined for adding labels to an image.
* Used by jlp_gdev_wxwid routines
*
* JLP
* Version 10/02/2017
***********************************************************************/
#include "jlp_wxgdev_shapes.h"   // JLP_wxGDevShapes class
#include "jlp_gdev_wxwid.h"      // JLP_GDev_wxWID class
#include "jlp_macros.h"          // SQUARE, etc 
 
static void CopyShape(JLP_SHAPE shape_1, JLP_SHAPE *shape_2);

/**************************************************************************
* Constructor
**************************************************************************/
JLP_wxGDevShapes::JLP_wxGDevShapes()
{
  nshapes1 = 0;
}
/**************************************************************************
* Adding a shape to the current set of shapes 
*
* INPUT:
* x1, y1, x2, y2: (user coord) position parameters of the shape 
* shape_type0 : type of shape (1=line 2=rectangle 3=circle 4=ellipse 5=ring
**************************************************************************/
int JLP_wxGDevShapes::AddShape(double x1, double y1, double x2, double y2,
                                int shape_type0) 
{
  if(nshapes1 == MAX_SHAPES) {
    wxMessageBox(_T("WARNING: maximum number of shapes has been reached!"),
                 _T("ImageLabels::AddShape"), wxOK | wxICON_ERROR);
    fprintf(stderr, "AddShape/Error: nshapes1=%d has reached upper limit\n",
            nshapes1);
    return(-1);
    }
  shape1[nshapes1].type = shape_type0;
  shape1[nshapes1].x1 = x1;
  shape1[nshapes1].x2 = x2;
  shape1[nshapes1].y1 = y1;
  shape1[nshapes1].y2 = y2;
  nshapes1++;
  return(0);
}
/**************************************************************************
* Move a shape
*
* INPUT:
* x1, y1, x2, y2: (user coord) position parameters of the rotation
**************************************************************************/
int JLP_wxGDevShapes::MoveShape(double x1, double y1, double x2, double y2)
{
int imin;
double dx, dy;
JLP_SHAPE shape0;

// Look for the closest shape: 
 GetClosestShape(x1, y1, &imin, &shape0);

// Translation: 
 dx = x2 - x1;
 shape1[imin].x1 += dx;
 shape1[imin].x2 += dx;

 dy = y2 - y1;
 shape1[imin].y1 += dy;
 shape1[imin].y2 += dy;

return(0);
}
/**************************************************************************
* Rotate a shape
*
* INPUT:
* x1, y1, x2, y2: (user coord) position parameters of the rotation 
**************************************************************************/
int JLP_wxGDevShapes::RotateShape(double x1, double y1, double x2, double y2)
{
int imin;
double angle1, angle2, rot_angle;
double xc, yc;
JLP_SHAPE shape0;

// Look for the closest shape: 
 GetClosestShape(x1, y1, &imin, &shape0);

// Compute its center:
 CenterOfShape(shape1[imin], &xc, &yc); 

// First angle:
 if(x1 != xc) angle1 = atan2((y1 - yc), (x1 - xc));
 else angle1 = 0.;

// Second angle:
 if(x2 != xc) angle2 = atan2((y2 - yc), (x2 - xc));
 else angle2 = 0.;

// Rotation: 
 rot_angle = angle2 - angle1; 

 shape1[imin].angle += rot_angle;

return(0);
}
/**************************************************************************
* Magnify a shape
*
* INPUT:
* x1, y1, x2, y2: (user coord) position parameters of the magnification 
**************************************************************************/
int JLP_wxGDevShapes::MagnifyShape(double x1, double y1, double x2, double y2)
{
int imin;
// Windows has trouble with rad1, rad2...
double radius1, radius2, magni_factor;
double xc, yc, x1_old, x2_old, y1_old, y2_old;
double radius0, width0, height0;
JLP_SHAPE shape0;

// Look for the closest shape:
 GetClosestShape(x1, y1, &imin, &shape0);

// Compute its center:
 CenterOfShape(shape1[imin], &xc, &yc);

// First radius:
 radius1 = sqrt(SQUARE(x1 - xc) + SQUARE(y1 - yc));

// Second radius:
 radius2 = sqrt(SQUARE(x2 - xc) + SQUARE(y2 - yc));

// Magnification:
 magni_factor = radius2 / (radius1 + 0.01);

x1_old = shape1[imin].x1;
y1_old = shape1[imin].y1;
x2_old = shape1[imin].x2;
y2_old = shape1[imin].y2;
width0 = x2_old - x1_old;
height0 = y2_old - y1_old;
radius0 = sqrt(SQUARE(width0) + SQUARE(height0));

switch(shape1[imin].type) {
// 3=Circle
    case 3:
      shape1[imin].x2 = xc + ABS(magni_factor) * radius0;
      shape1[imin].y2 = yc;
      break;
// 5=Ring: two circles (rad1=width, rad2=height)
    case 5:
      shape1[imin].x1 = x1_old - (magni_factor - 1.) * width0 / 2.;
      shape1[imin].x2 = x2_old + (magni_factor - 1.) * width0 / 2.;
      shape1[imin].y1 = y1_old - (magni_factor - 1.) * height0 / 2.;
      shape1[imin].y2 = y2_old + (magni_factor - 1.) * height0 / 2.;
// 1=Line
    case 1:
// 2=Rectangle
    case 2:
// 4=Ellipse
    case 4:
// Change width:
      shape1[imin].x1 = x1_old - (magni_factor - 1.) * width0 / 2.;
      shape1[imin].x2 = x2_old + (magni_factor - 1.) * width0 / 2.;
// Change height:
      shape1[imin].y1 = y1_old - (magni_factor - 1.) * height0 / 2.;
      shape1[imin].y2 = y2_old + (magni_factor - 1.) * height0 / 2.;
      break;
  }

return(0);
}
/**************************************************************************
* Compute the center of the shape 
*
* OUTPUT:
* xc, yc: (user coord) center of the shape
**************************************************************************/
int JLP_wxGDevShapes::CenterOfShape(JLP_SHAPE shape0, double *xc, double *yc)
{
double x1, y1, x2, y2;

x1 = shape0.x1;
y1 = shape0.y1;
x2 = shape0.x2;
y2 = shape0.y2;

// Center: 
switch(shape0.type) {
// 3=Circle
    case 3:
// 5=Ring: two circles (rad1=width, rad2=height)
    case 5:
      *xc = x1;
      *yc = y1;
      break;
// 1=Line
    case 1: 
// 2=Rectangle
    case 2:
// 4=Ellipse
    case 4:
      *xc = (x1 + x2) / 2;
      *yc = (y1 + y2) / 2;
      break;
  }

return(0);
}
/**********************************************************************
* Removing a shape from the current set of shapes 
* by looking at the closest shape to the input point
**********************************************************************/
int JLP_wxGDevShapes::RemoveShape(double x0, double y0)
{
int i, imin;
JLP_SHAPE shape0;

// Look for the closest shape:
 GetClosestShape(x0, y0, &imin, &shape0);

// Remove the item #imin in the list:
for(i = imin; i < nshapes1 - 1; i++) {
  CopyShape(shape1[i+1], &shape1[i]);
  }

nshapes1--;

return(0);
}
/**********************************************************************
* Get the closest shape 
*
* INPUT:
* x0, y0:(user coord)  position entered by the user
*
* OUTPUT:
* imin: index of closest shape
* shape0: shape parameters
**************************************************************************/
int JLP_wxGDevShapes::GetClosestShape(double x0, double y0, int* imin,
                                       JLP_SHAPE *shape0)
{
int i, status = -1; 
double d, dmin;

// Look for the closest shape:
dmin = 10000;
*imin = 0; 
for(i = 0; i < nshapes1; i++) {
  if((shape1[i].type == 3) || (shape1[i].type == 5)) { 
   d = SQUARE(shape1[i].x1 - x0) + SQUARE(shape1[i].y1 - y0);
   } else {
   d = SQUARE((shape1[i].x1 + shape1[i].x2)/2. - x0) 
       + SQUARE((shape1[i].y1 + shape1[i].y2)/2. - y0);
   }
  if(d < dmin){ dmin = d; *imin = i; status = 0;}
  }

CopyShape(shape1[*imin], shape0);

return(status);
}
/**********************************************************************
* Get a shape 
**********************************************************************/
int JLP_wxGDevShapes::GetShape(const int ishape, JLP_SHAPE *shape0) 
{
int status = -1;

  if((ishape >= 0) && (ishape < nshapes1)) {
    CopyShape(shape1[ishape], shape0);
    status = 0;
  }

return(status);
}
/*************************************************************
*
*************************************************************/
static void CopyShape(JLP_SHAPE shape_1, JLP_SHAPE *shape_2)
{

 shape_2->x1 = shape_1.x1;
 shape_2->y1 = shape_1.y1;
 shape_2->x2 = shape_1.x2;
 shape_2->y2 = shape_1.y2;
 shape_2->angle = shape_1.angle;
 shape_2->type = shape_1.type;
}
/*************************************************************
* To draw the shapes and labels to the graphic context  
*************************************************************/
void JLP_wxGDevShapes::DrawShapesToBackupDC(JLP_GDev_wxWID *jlp_wx_gdev1,
                                             wxMemoryDC *backup_dc,
                                             wxColour c_PenColour,
                                             int scroll_x, int scroll_y)
{
double wx, wy; 
int i, in_frame, ix1, iy1, ix2, iy2, ixc = 0, iyc = 0;
int pen_width = 2, PointSize, width0, height0, radius0; 
bool filled = false;
wxFont def_Font = wxSystemSettings::GetFont(wxSYS_DEFAULT_GUI_FONT);
#ifdef NEW_FONTS
wxFont *my_Font;
#endif
wxString label_txt;

// Setting fonts:
PointSize = def_Font.GetPointSize() * 3 / 2;
#ifdef NEW_FONTS
// wxSWISS_FONT allows rotation with DrawRotatedText
my_Font = new wxFont(PointSize, wxFONTFAMILY_SWISS,
                     wxFONTSTYLE_NORMAL, wxFONTWEIGHT_BOLD,
                     false, wxEmptyString, wxFONTENCODING_DEFAULT);
backup_dc->SetFont(*my_Font);
#endif

// Set the color and width:
 backup_dc->SetPen(wxPen(c_PenColour, pen_width));
 backup_dc->SetTextForeground(c_PenColour);

//
if(!filled) backup_dc->SetBrush(*wxTRANSPARENT_BRUSH);

// Draw all the shapes:
for(i = 0; i < nshapes1; i++) {
  jlp_wx_gdev1->ConvUserToDev(shape1[i].x1, shape1[i].y1, &wx, &wy, &in_frame);
  ix1 = (int)wx;
  iy1 = (int)wy;
  jlp_wx_gdev1->ConvUserToDev(shape1[i].x2, shape1[i].y2, &wx, &wy, &in_frame);
  ix2 = (int)wx;
  iy2 = (int)wy;
// Drawing the shapes and computing the position of the labels:
// shape_type: 1=line 2=rectangle 3=circle 4=ellipse 5=ring
  width0 = (int)(ix2 - ix1);
  height0 = (int)(iy2 - iy1);
  switch(shape1[i].type) {
    default:
// 1=Line
    case 1: 
      backup_dc->DrawLine(ix1 + scroll_x, iy1 + scroll_y, 
                          ix2 + scroll_x, iy2 + scroll_y);
      ixc = (ix1 + ix2) / 2 + 10;
      iyc = (iy1 + iy2) / 2 - 10;
      break;
// 2=Rectangle
    case 2: 
      backup_dc->DrawRectangle(ix1 + scroll_x, iy1 + scroll_y,
                               width0, height0);
// Center of the rectangle:
      ixc = MINI(ix1, ix2) + ABS(ix2 - ix1) / 2;
      iyc = MAXI(iy1, iy2) - ABS(iy2 - iy1) / 2;
      break;
// 3=Circle
    case 3: 
      radius0 = (int)sqrt((double)SQUARE(width0) 
                           + (double)SQUARE(height0));
      backup_dc->DrawCircle(ix1 + scroll_x, iy1 + scroll_y, radius0);
// Center of the circle:
      ixc = ix1;
      iyc = iy1;
      break;
// 4=Ellipse
    case 4: 
      backup_dc->DrawEllipse(ix1 + scroll_x, iy1 + scroll_y, width0, height0);
// Center of the ellipse:
      ixc = ix1 + width0 / 2;
      iyc = iy1 + height0 / 2;
      break;
// 5=Ring: two circles (rad1=width, rad2=height)
    case 5: 
      radius0 = (int)ABS(width0); 
      backup_dc->DrawCircle(ix1 + scroll_x, iy1 + scroll_y, radius0);
      radius0 = (int)ABS(height0); 
      backup_dc->DrawCircle(ix1 + scroll_x, iy1 + scroll_y, radius0);
// Center of the circles:
      ixc = ix1;
      iyc = iy1;
      break;
    }

// Label:
  label_txt.Printf(wxT("%d"), i);
  backup_dc->DrawText(label_txt, ixc + scroll_x, iyc + scroll_y);

 }
// Go back to default fonts:
#ifdef NEW_FONTS
backup_dc->SetFont(*def_Font);
delete my_Font;
#endif
return;
}
/*********************************************************************
*
*********************************************************************/
/*
static void DrawEllipse()
{
double a, b;
MyPoint c;
// a = half of the lenght of the major axis
// b = half of the lenght opf the minor axis
// c = center point
int maxPoints = 20;
double step = M_PI * 2 / maxPoints;
MyPoint *ellipse = new MyPoint[maxPoints];
MyPoint p;
int index;
double angle;

index = 0;
for (angle = 0; angle < M_PI * 2; angle += step) {
    p.x = c.x + a * cos(angle);
    p.y = c.y + b * cos(angle);
    ellipse[index++] = p;
   }
return;
}
*/
