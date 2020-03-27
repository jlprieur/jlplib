/**
* \file jlp_gseg_gnome.cpp 
* \class JLP_Gseg_Gnome (Gnu Graphic Device) 
* \author JLP
* \date 13/12/2016
* \brief Definition of the gnome graphic drivers for JLP_Gseg 
*
* Class JLP_Gseg_Gnome ("Gnu Graphic Device")
* Definition of the graphic drivers 
* used in the JLP/GNU plotting package
*
* JLP
* Version 14/12/2016
**************************************************************************/
#include <math.h>
#include <time.h>
#include "jlp_gseg_gnome.h"       // JLP_Gseg_Gnome class 

/************************************************************************
* Constructor
************************************************************************/
JLP_Gseg_Gnome::JLP_Gseg_Gnome(window_data_type *p_window_data_0)
{
/* Save to private variables: */
 p_parent_window_data.x = p_window_data_0->x;
 p_parent_window_data.y = p_window_data_0->y;
 p_parent_window_data.width = p_window_data_0->width;
 p_parent_window_data.height = p_window_data_0->height;
 p_parent_window_data.window = NULL; 
 p_parent_window_data.canvas = NULL; 

// Initialize width dummy values to avoid problems when not fully initialized:
parent_group = NULL;
parent_height_menu_bar = 0;

/* Initialize font variables */
 GSEG_gnome_InitializeFonts();
}

/******************************************************************************
*
******************************************************************************/
void JLP_Gseg_Gnome::GSEG_gnome_InitializePlotWindow(GnomeApp *window0,
                                                     GtkWidget *canvas0)
{
p_parent_window_data.window = window0;
p_parent_window_data.canvas = canvas0;
}
/******************************************************************************
*
******************************************************************************/
void JLP_Gseg_Gnome::GSEG_gnome_InitializePlotCanvas(GnomeCanvasGroup *group0, 
                                                     int height_menu_bar0)
{
parent_group = group0;
parent_height_menu_bar = height_menu_bar0;
}

/*****************************************************************************
*
*****************************************************************************/
void JLP_Gseg_Gnome::GSEG_gnome_SetPointersFromPlot(PangoFontDescription **font_text_0)
{

*font_text_0 = font_text1;

return;
}
/*****************************************************************************
*
*****************************************************************************/
void JLP_Gseg_Gnome::GSEG_gnome_InitializeFonts( void )
{
unsigned int size;

/* Set pointers to NULL */
   font_date_time1        = NULL;
   font_legend1           = NULL;
   font_text1             = NULL;
   font_tick_labels1      = NULL;
   font_axis_labels1      = NULL;
   font_title1            = NULL;

/* Specify default font name */
   strcpy(font_name1, "Sans");

/* Specify font style and weight */
   font_date_time1 = pango_font_description_new();
   pango_font_description_set_style(font_date_time1, PANGO_STYLE_NORMAL);
   pango_font_description_set_weight(font_date_time1, PANGO_WEIGHT_NORMAL);

   font_legend1 = pango_font_description_new();
   pango_font_description_set_style(font_legend1, PANGO_STYLE_NORMAL);
   pango_font_description_set_weight(font_legend1, PANGO_WEIGHT_NORMAL);

   font_text1 = pango_font_description_new();
   pango_font_description_set_style(font_text1, PANGO_STYLE_NORMAL);
   pango_font_description_set_weight(font_text1, PANGO_WEIGHT_NORMAL);

   font_tick_labels1 = pango_font_description_new();
   pango_font_description_set_style(font_tick_labels1, PANGO_STYLE_NORMAL);
   pango_font_description_set_weight(font_tick_labels1, PANGO_WEIGHT_NORMAL);

   font_axis_labels1 = pango_font_description_new();
   pango_font_description_set_style(font_axis_labels1, PANGO_STYLE_NORMAL);
   pango_font_description_set_weight(font_axis_labels1, PANGO_WEIGHT_NORMAL);

   font_title1 = pango_font_description_new();
   pango_font_description_set_style(font_title1, PANGO_STYLE_NORMAL);
   pango_font_description_set_weight(font_title1, PANGO_WEIGHT_NORMAL);

/* Specify default font point sizes */
   font_size_date_time1   = 12;
   font_size_legend1      = 14;
   font_size_text1        = 14;
   font_size_tick_labels1 = 14;
   font_size_axis_labels1 = 16;
   font_size_title1       = 18;

return;
}
